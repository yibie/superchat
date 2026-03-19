const REMOTE_URL_PATTERN = /https?:\/\/[^\s<>)"'`]+/i;
const LOCAL_PATH_PATTERN = /(?:^|[\s(])((?:attachments\/|file:\/\/|\/)[^\s<>)"'`]+)/i;
const LOCATOR_PATTERN = /(https?:\/\/[^\s<>)"'`]+)|(?:^|[\s(])((?:attachments\/|file:\/\/|\/)[^\s<>)"'`]+)/gi;

const IMAGE_EXTENSIONS = new Set(["png", "jpg", "jpeg", "gif", "webp", "bmp", "heic", "svg"]);
const VIDEO_EXTENSIONS = new Set(["mp4", "mov", "m4v", "webm", "avi", "mkv"]);
const AUDIO_EXTENSIONS = new Set(["mp3", "wav", "ogg", "m4a", "flac", "aac"]);
const DOCUMENT_EXTENSIONS = new Set(["pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "epub", "txt", "md"]);
const VIDEO_HOSTS = [
  "youtube.com",
  "youtu.be",
  "vimeo.com",
  "bilibili.com",
  "loom.com"
];

export function resolveEntrySourceDescriptor(entry) {
  const locator = normalizeLocator(
    entry?.sourceMetadata?.locator ??
      entry?.body?.url ??
      extractLocatorCandidate(entry?.summaryText ?? "")
  );
  if (!locator) {
    return null;
  }

  const explicitKind = normalizeKind(entry?.body?.kind);
  const contentType = normalizeKind(entry?.body?.linkMeta?.contentType);
  const sourceKind = classifyLocatorKind({ locator, explicitKind, contentType });

  return {
    locator,
    sourceKind,
    explicitKind,
    contentType,
    isLocal: isLocalLocator(locator),
    extension: locatorExtension(locator),
    title:
      firstNonEmpty(
        entry?.sourceMetadata?.title,
        entry?.body?.title,
        titleFromLocator(locator)
      ) ?? "Resource",
    summary: String(entry?.summaryText ?? "").trim(),
    citation:
      firstNonEmpty(
        entry?.sourceMetadata?.citation,
        entry?.body?.details
      ) ?? ""
  };
}

export function extractLocatorCandidate(text) {
  return extractLocatorCandidates(text)[0] ?? null;
}

export function extractLocatorCandidates(text) {
  const value = String(text ?? "");
  if (!value.trim()) {
    return [];
  }

  const results = [];
  const seen = new Set();
  for (const match of value.matchAll(LOCATOR_PATTERN)) {
    const locator = normalizeLocator(match[1] ?? match[2] ?? "");
    if (!locator || seen.has(locator)) {
      continue;
    }
    seen.add(locator);
    results.push(locator);
  }
  return results;
}

export function classifyLocatorKind({ locator, explicitKind = null, contentType = null } = {}) {
  const kind = normalizeKind(explicitKind);
  if (kind && kind !== "mixed") {
    return kind;
  }
  const content = normalizeKind(contentType);
  if (content && content !== "mixed") {
    return content;
  }

  const extension = locatorExtension(locator);
  if (IMAGE_EXTENSIONS.has(extension)) {
    return "image";
  }
  if (VIDEO_EXTENSIONS.has(extension)) {
    return "video";
  }
  if (AUDIO_EXTENSIONS.has(extension)) {
    return "audio";
  }
  if (DOCUMENT_EXTENSIONS.has(extension)) {
    return "document";
  }
  if (isKnownVideoHost(locator)) {
    return "video";
  }
  return "url";
}

export function isLocalLocator(locator) {
  const value = String(locator ?? "").trim();
  return value.startsWith("attachments/") || value.startsWith("/") || value.startsWith("file://");
}

export function locatorExtension(locator) {
  const value = String(locator ?? "").trim();
  if (!value) {
    return "";
  }
  const candidate = stripQueryAndHash(value);
  const dotIndex = candidate.lastIndexOf(".");
  if (dotIndex < 0) {
    return "";
  }
  return candidate.slice(dotIndex + 1).toLowerCase();
}

export function titleFromLocator(locator) {
  const value = String(locator ?? "").trim();
  if (!value) {
    return "";
  }
  const stripped = stripQueryAndHash(value).replace(/\/$/, "");
  const segments = stripped.split("/").filter(Boolean);
  const lastSegment = segments.at(-1);
  if (lastSegment && !/^https?:$/i.test(lastSegment)) {
    return decodeURIComponentSafe(lastSegment);
  }
  try {
    const parsed = new URL(value);
    return parsed.hostname.replace(/^www\./, "");
  } catch {
    return value;
  }
}

export function hostnameFromLocator(locator) {
  try {
    return new URL(String(locator ?? "")).hostname.replace(/^www\./, "");
  } catch {
    return null;
  }
}

export function isKnownVideoHost(locator) {
  const hostname = hostnameFromLocator(locator);
  return hostname ? VIDEO_HOSTS.some((host) => hostname === host || hostname.endsWith(`.${host}`)) : false;
}

function normalizeLocator(value) {
  const locator = String(value ?? "").trim();
  return locator || null;
}

function stripQueryAndHash(value) {
  return String(value ?? "").split(/[?#]/, 1)[0];
}

function normalizeKind(value) {
  const normalized = String(value ?? "").trim().toLowerCase();
  return normalized || null;
}

function firstNonEmpty(...values) {
  for (const value of values) {
    const trimmed = String(value ?? "").trim();
    if (trimmed) {
      return trimmed;
    }
  }
  return null;
}

function decodeURIComponentSafe(value) {
  try {
    return decodeURIComponent(value);
  } catch {
    return value;
  }
}
