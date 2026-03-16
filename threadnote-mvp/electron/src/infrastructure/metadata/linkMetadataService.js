import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";
import {
  classifyLocatorKind,
  hostnameFromLocator,
  isKnownVideoHost,
  isLocalLocator,
  locatorExtension,
  resolveEntrySourceDescriptor,
  titleFromLocator
} from "../../domain/resources/richSourceDescriptor.js";

export class LinkMetadataService {
  constructor({
    fetchImpl = globalThis.fetch?.bind(globalThis),
    workspacePathResolver = () => null,
    maxCacheEntries = 256,
    maxConcurrent = 3
  } = {}) {
    this.fetchImpl = fetchImpl;
    this.workspacePathResolver = workspacePathResolver;
    this.maxCacheEntries = maxCacheEntries;
    this.maxConcurrent = maxConcurrent;
    this.cache = new Map();
    this.activeCount = 0;
    this.pendingTasks = [];
  }

  async getEntryRichPreview(entry) {
    const descriptor = resolveEntrySourceDescriptor(entry);
    if (!descriptor) {
      return null;
    }
    return this.getRichPreviewForDescriptor(descriptor);
  }

  async getRichPreviewForDescriptor(descriptor) {
    const resolvedPath = descriptor.isLocal ? resolveLocalPath(descriptor.locator, this.workspacePathResolver()) : null;
    const cacheKey = descriptor.isLocal
      ? `local:${resolvedPath ?? descriptor.locator}`
      : `remote:${descriptor.locator}`;
    if (this.cache.has(cacheKey)) {
      return this.cache.get(cacheKey);
    }

    const task = this.#enqueue(async () => {
      const preview = descriptor.isLocal
        ? buildLocalPreview(descriptor, resolvedPath)
        : await buildRemotePreview(descriptor, this.fetchImpl);
      return preview;
    });
    this.#remember(cacheKey, task);
    return task;
  }

  #enqueue(task) {
    return new Promise((resolve, reject) => {
      const run = async () => {
        this.activeCount += 1;
        try {
          resolve(await task());
        } catch (error) {
          reject(error);
        } finally {
          this.activeCount -= 1;
          const next = this.pendingTasks.shift();
          if (next) {
            void next();
          }
        }
      };

      if (this.activeCount < this.maxConcurrent) {
        void run();
      } else {
        this.pendingTasks.push(run);
      }
    });
  }

  #remember(key, value) {
    this.cache.delete(key);
    this.cache.set(key, value);
    while (this.cache.size > this.maxCacheEntries) {
      const oldestKey = this.cache.keys().next().value;
      this.cache.delete(oldestKey);
    }
  }
}

function buildLocalPreview(descriptor, resolvedPath) {
  const sourceKind = classifyLocatorKind(descriptor);
  const exists = Boolean(resolvedPath) && fs.existsSync(resolvedPath);
  const previewURL = exists ? pathToFileURL(resolvedPath).href : null;
  const title = descriptor.title || titleFromLocator(descriptor.locator);
  const description = exists ? "Workspace-local attachment" : "File missing from current workspace";

  return {
    locator: descriptor.locator,
    openLocator: resolvedPath ?? descriptor.locator,
    sourceKind,
    previewMode: exists ? previewModeFor(sourceKind, descriptor.locator) : "missing-file",
    title,
    description,
    citation: descriptor.citation || descriptor.summary,
    siteName: "Local file",
    hostname: null,
    previewURL: shouldUseMediaURL(sourceKind) ? previewURL : null,
    previewImageURL: sourceKind === "image" ? previewURL : null,
    isLocal: true,
    isMissingLocalFile: !exists,
    fileName: path.basename(resolvedPath ?? descriptor.locator),
    extension: descriptor.extension || locatorExtension(descriptor.locator)
  };
}

async function buildRemotePreview(descriptor, fetchImpl) {
  const sourceKind = classifyLocatorKind(descriptor);
  const hostname = hostnameFromLocator(descriptor.locator);
  const base = {
    locator: descriptor.locator,
    openLocator: descriptor.locator,
    sourceKind,
    previewMode: previewModeFor(sourceKind, descriptor.locator),
    title: descriptor.title || titleFromLocator(descriptor.locator),
    description: descriptor.citation || descriptor.summary || "",
    citation: descriptor.citation || descriptor.summary || "",
    siteName: hostname,
    hostname,
    previewURL: shouldUseMediaURL(sourceKind) && !isKnownVideoHost(descriptor.locator) ? descriptor.locator : null,
    previewImageURL: null,
    isLocal: false,
    isMissingLocalFile: false,
    fileName: titleFromLocator(descriptor.locator),
    extension: descriptor.extension || locatorExtension(descriptor.locator)
  };

  if (!fetchImpl || sourceKind === "image" || sourceKind === "audio" || sourceKind === "document" || isKnownVideoHost(descriptor.locator)) {
    return base;
  }

  if (sourceKind === "video" && base.previewMode === "video-player") {
    return base;
  }

  try {
    const metadata = await fetchRemoteMetadata(descriptor.locator, fetchImpl);
    return {
      ...base,
      title: metadata.title || base.title,
      description: metadata.description || base.description,
      citation: metadata.description || base.citation,
      siteName: metadata.siteName || base.siteName,
      previewImageURL: metadata.image || null
    };
  } catch {
    return base;
  }
}

async function fetchRemoteMetadata(locator, fetchImpl) {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 3000);
  try {
    const response = await fetchImpl(locator, {
      signal: controller.signal,
      headers: {
        accept: "text/html,application/xhtml+xml"
      }
    });
    if (!response?.ok) {
      throw new Error(`Metadata fetch failed: ${response?.status ?? "unknown"}`);
    }
    const contentType = response.headers?.get?.("content-type") ?? "";
    if (contentType && !contentType.includes("text/html")) {
      return {};
    }
    const html = await response.text();
    return parseHTMLMetadata(html, locator);
  } finally {
    clearTimeout(timeout);
  }
}

function parseHTMLMetadata(html, locator) {
  const title = firstMatch(html, [
    /<meta[^>]+property=["']og:title["'][^>]+content=["']([^"']+)["']/i,
    /<meta[^>]+content=["']([^"']+)["'][^>]+property=["']og:title["']/i,
    /<title>([^<]+)<\/title>/i
  ]);
  const description = firstMatch(html, [
    /<meta[^>]+property=["']og:description["'][^>]+content=["']([^"']+)["']/i,
    /<meta[^>]+name=["']description["'][^>]+content=["']([^"']+)["']/i,
    /<meta[^>]+content=["']([^"']+)["'][^>]+name=["']description["']/i
  ]);
  const siteName = firstMatch(html, [
    /<meta[^>]+property=["']og:site_name["'][^>]+content=["']([^"']+)["']/i,
    /<meta[^>]+content=["']([^"']+)["'][^>]+property=["']og:site_name["']/i
  ]);
  const image = resolveRelativeURL(
    locator,
    firstMatch(html, [
      /<meta[^>]+property=["']og:image["'][^>]+content=["']([^"']+)["']/i,
      /<meta[^>]+content=["']([^"']+)["'][^>]+property=["']og:image["']/i
    ])
  );

  return {
    title: decodeHTML(title),
    description: decodeHTML(description),
    siteName: decodeHTML(siteName),
    image
  };
}

function firstMatch(html, patterns) {
  for (const pattern of patterns) {
    const match = String(html ?? "").match(pattern);
    const value = match?.[1]?.trim();
    if (value) {
      return value;
    }
  }
  return "";
}

function resolveRelativeURL(baseURL, value) {
  if (!value) {
    return null;
  }
  try {
    return new URL(value, baseURL).href;
  } catch {
    return value;
  }
}

function resolveLocalPath(locator, workspacePath) {
  const value = String(locator ?? "").trim();
  if (!value) {
    return null;
  }
  if (value.startsWith("file://")) {
    try {
      return new URL(value).pathname;
    } catch {
      return null;
    }
  }
  if (value.startsWith("/")) {
    return value;
  }
  if (value.startsWith("attachments/") && workspacePath) {
    return path.join(workspacePath, value);
  }
  return isLocalLocator(value) ? value : null;
}

function previewModeFor(sourceKind, locator) {
  if (sourceKind === "image") {
    return "image";
  }
  if (sourceKind === "audio") {
    return "audio-player";
  }
  if (sourceKind === "video") {
    return isKnownVideoHost(locator) ? "video-card" : "video-player";
  }
  if (sourceKind === "document") {
    return "document-card";
  }
  return "link-card";
}

function shouldUseMediaURL(sourceKind) {
  return sourceKind === "image" || sourceKind === "audio" || sourceKind === "video";
}

function decodeHTML(value) {
  return String(value ?? "")
    .replaceAll("&amp;", "&")
    .replaceAll("&lt;", "<")
    .replaceAll("&gt;", ">")
    .replaceAll("&quot;", '"')
    .replaceAll("&#39;", "'");
}
