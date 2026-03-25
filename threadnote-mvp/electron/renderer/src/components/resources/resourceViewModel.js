import { buildResourcePresentation } from "../../../../src/domain/resources/resourcePresentation.js";

function extractDomain(url) {
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return "";
  }
}

function inferFileKind(resource) {
  const sourceKind = String(resource?.sourceKind ?? "").trim().toLowerCase();
  if (sourceKind === "image" || sourceKind === "video") {
    return sourceKind;
  }
  if (sourceKind === "document") {
    return "document";
  }
  if (sourceKind === "audio") {
    return "file";
  }
  if (resource?.kind === "mention") {
    return "mention";
  }
  if (resource?.kind === "link") {
    return "link";
  }
  if (resource?.attachment) {
    return "file";
  }
  return resource?.kind === "media" ? "file" : "link";
}

function toAbsolutePath(locator, workspace) {
  const value = String(locator ?? "").trim();
  if (!value) {
    return null;
  }
  if (/^https?:\/\//i.test(value)) {
    return value;
  }
  if (value.startsWith("file://")) {
    try {
      return decodeURIComponent(new URL(value).pathname);
    } catch {
      return value.replace(/^file:\/\//i, "");
    }
  }
  if (value.startsWith("/")) {
    return value;
  }
  if (value.startsWith("attachments/") && workspace?.workspacePath) {
    return `${workspace.workspacePath}/${value}`;
  }
  return null;
}

function toFileURL(absolutePath) {
  const value = String(absolutePath ?? "").trim();
  if (!value) {
    return null;
  }
  if (value.startsWith("file://")) {
    return value;
  }
  return encodeURI(`file://${value}`);
}

function formatFileSize(bytes) {
  if (bytes == null || bytes === "") {
    return "";
  }
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1048576) return `${Math.round(bytes / 1024)} KB`;
  return `${(bytes / 1048576).toFixed(1)} MB`;
}

function fileTypeLabel(resource) {
  const mimeType = String(resource?.attachment?.mimeType ?? "").trim();
  if (mimeType) {
    if (mimeType === "application/pdf") {
      return "PDF Document";
    }
    if (mimeType.startsWith("image/")) {
      return "Image";
    }
    if (mimeType.startsWith("video/")) {
      return "Video";
    }
    if (mimeType.startsWith("audio/")) {
      return "Audio";
    }
  }
  const sourceKind = String(resource?.sourceKind ?? "").trim();
  if (sourceKind === "document") {
    return "Document";
  }
  if (sourceKind === "audio") {
    return "Audio";
  }
  if (sourceKind === "video") {
    return "Video";
  }
  if (sourceKind === "image") {
    return "Image";
  }
  return "File";
}

export function resolveResourceLocator(resource) {
  return (
    resource?.locator ??
    resource?.attachment?.relativePath ??
    resource?.entry?.body?.url ??
    resource?.previewText ??
    null
  );
}

export function resolveResourceTitle(resource) {
  const locator = resolveResourceLocator(resource) ?? "";
  const sourceKind = String(resource?.sourceKind ?? "").trim().toLowerCase();
  const presentation = buildResourcePresentation({
    title: resource?.entry?.sourceMetadata?.title ?? resource?.entry?.body?.title ?? resource?.title ?? "",
    displayName: resource?.attachment?.displayName ?? "",
    fileName: resource?.attachment?.fileName ?? "",
    locator,
    sourceKind,
    isLocal: Boolean(resource?.attachment) || locator.startsWith("attachments/")
  });
  return presentation.title || fileTypeLabel(resource);
}

export function resolveResourceRenderableURL(resource, workspace = null) {
  const locator = resolveResourceLocator(resource);
  if (!locator) {
    return null;
  }
  if (/^https?:\/\//i.test(locator)) {
    return locator;
  }
  const absolutePath = toAbsolutePath(locator, workspace);
  return absolutePath ? toFileURL(absolutePath) : null;
}

export function resolveResourceOpenTarget(resource, workspace = null) {
  const locator = resolveResourceLocator(resource);
  if (!locator) {
    return null;
  }
  if (/^https?:\/\//i.test(locator)) {
    return locator;
  }
  return toAbsolutePath(locator, workspace) ?? locator;
}

export function resourcePreviewKind(resource) {
  return inferFileKind(resource);
}

export function resourceSourceLabel(resource) {
  const threadTitle = String(resource?.threadTitle ?? "").trim();
  if (threadTitle) {
    return threadTitle;
  }
  if (resource?.threadID == null) {
    return "Inbox";
  }
  return "Thread";
}

export function resourceSummaryLabel(resource) {
  const summary = String(resource?.entry?.summaryText ?? "").trim();
  return summary || "";
}

export function resourceMetaLabel(resource) {
  const previewKind = resourcePreviewKind(resource);
  if (previewKind === "document" || previewKind === "file") {
    const parts = [fileTypeLabel(resource), formatFileSize(resource?.attachment?.size)].filter(Boolean);
    return parts.join(" ");
  }
  if (previewKind === "link") {
    return extractDomain(resolveResourceLocator(resource)) || "";
  }
  return "";
}

export function groupDisplayResources(resources = []) {
  const grouped = { media: [], links: [], documents: [], files: [], mentions: [] };
  for (const resource of resources) {
    const previewKind = resourcePreviewKind(resource);
    if (previewKind === "image" || previewKind === "video") {
      grouped.media.push(resource);
    } else if (previewKind === "link") {
      grouped.links.push(resource);
    } else if (previewKind === "document") {
      grouped.documents.push(resource);
    } else if (previewKind === "file") {
      grouped.files.push(resource);
    } else if (previewKind === "mention") {
      grouped.mentions.push(resource);
    }
  }
  return grouped;
}

function normalizeMentionName(value) {
  return String(value ?? "").trim().replace(/^@+/, "");
}

export function groupMentionResources(resources = []) {
  const mentions = new Map();

  for (const resource of resources) {
    if (resourcePreviewKind(resource) !== "mention") {
      continue;
    }

    for (const label of resource?.mentionLabels ?? []) {
      const name = normalizeMentionName(label);
      if (!name) {
        continue;
      }

      const key = name.toLowerCase();
      const current = mentions.get(key) ?? {
        id: `mention:${key}`,
        name,
        label: `@${name}`,
        count: 0,
        entryIDs: new Set()
      };

      const entryID = resource?.entryID ?? resource?.entry?.id ?? null;
      if (!entryID || current.entryIDs.has(entryID)) {
        mentions.set(key, current);
        continue;
      }

      current.entryIDs.add(entryID);
      current.count += 1;
      mentions.set(key, current);
    }
  }

  return [...mentions.values()]
    .map((item) => ({
      id: item.id,
      name: item.name,
      label: item.label,
      count: item.count
    }))
    .sort((lhs, rhs) => {
      if (rhs.count !== lhs.count) {
        return rhs.count - lhs.count;
      }
      return lhs.name.localeCompare(rhs.name, undefined, { sensitivity: "base" });
    });
}

export function entriesForMention(resources = [], mentionName) {
  const target = normalizeMentionName(mentionName).toLowerCase();
  if (!target) {
    return [];
  }

  const entries = new Map();
  for (const resource of resources) {
    if (resourcePreviewKind(resource) !== "mention") {
      continue;
    }

    const matches = (resource?.mentionLabels ?? []).some(
      (label) => normalizeMentionName(label).toLowerCase() === target
    );
    if (!matches) {
      continue;
    }

    const entry = resource?.entry;
    if (!entry?.id || entries.has(entry.id)) {
      continue;
    }
    entries.set(entry.id, entry);
  }

  return [...entries.values()].sort(
    (lhs, rhs) => new Date(rhs.createdAt ?? 0).getTime() - new Date(lhs.createdAt ?? 0).getTime()
  );
}

export function createResourceActions(resource, { workspace = null, openLocator, focusEntry }) {
  const openTarget = resolveResourceOpenTarget(resource, workspace);
  const entryID = resource?.entryID ?? resource?.entry?.id ?? null;
  const threadID = resource?.threadID ?? null;

  return {
    canOpen: Boolean(openTarget),
    canGoToNote: Boolean(entryID),
    openResource() {
      if (openTarget) {
        openLocator?.(openTarget);
      }
    },
    goToNote() {
      if (entryID) {
        focusEntry?.(entryID, { threadID });
      }
    }
  };
}
