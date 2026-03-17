import { classifyLocatorKind, resolveEntrySourceDescriptor } from "./richSourceDescriptor.js";

export const ResourceKind = Object.freeze({
  LINK: "link",
  MEDIA: "media",
  MENTION: "mention"
});

export function deriveResources(entries) {
  return (entries ?? [])
    .flatMap((entry) => deriveEntryResources(entry))
    .filter(Boolean)
    .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime());
}

export function resourceCounts(resources) {
  const counts = {
    linkCount: 0,
    mediaCount: 0,
    mentionCount: 0,
    totalCount: 0
  };
  for (const resource of resources ?? []) {
    if (resource.kind === ResourceKind.LINK) {
      counts.linkCount += 1;
    } else if (resource.kind === ResourceKind.MEDIA) {
      counts.mediaCount += 1;
    } else if (resource.kind === ResourceKind.MENTION) {
      counts.mentionCount += 1;
    }
  }
  counts.totalCount = counts.linkCount + counts.mediaCount + counts.mentionCount;
  return counts;
}

export function classifyResource(entry) {
  if (isMedia(entry)) {
    return ResourceKind.MEDIA;
  }
  if (isLink(entry)) {
    return ResourceKind.LINK;
  }
  if (isMention(entry)) {
    return ResourceKind.MENTION;
  }
  return null;
}

export function mentionLabels(entry) {
  return Array.from(new Set((entry?.objectMentions ?? []).map((item) => item.name)))
    .filter(Boolean)
    .sort((lhs, rhs) => lhs.localeCompare(rhs, undefined, { sensitivity: "base" }));
}

function deriveEntryResources(entry) {
  const resources = [];
  const attachments = attachmentResources(entry);
  if (attachments.length > 0) {
    resources.push(...attachments);
  }

  if (isMedia(entry)) {
    resources.push(createResource(entry, ResourceKind.MEDIA));
  } else if (isLink(entry)) {
    resources.push(createResource(entry, ResourceKind.LINK));
  }

  const mentions = mentionLabels(entry);
  for (const mention of mentions) {
    resources.push(createResource(entry, ResourceKind.MENTION, { mention }));
  }

  return resources;
}

function createResource(entry, kind, { mention = null, locator = null, sourceKind = null, attachment = null, title = null, previewText = null, idSuffix = kind } = {}) {
  const descriptor = resolveEntrySourceDescriptor(entry);
  const resolvedLocator = locator ?? descriptor?.locator ?? null;
  const resolvedSourceKind = sourceKind ?? descriptor?.sourceKind ?? null;
  return {
    id: mention ? `${entry.id}::mention::${mention.toLowerCase()}` : `${entry.id}::${idSuffix}`,
    entry,
    entryID: entry?.id ?? null,
    threadID: entry.threadID,
    kind,
    locator: resolvedLocator,
    sourceKind: resolvedSourceKind,
    attachment,
    mentionLabels: mention ? [mention] : mentionLabels(entry),
    createdAt: entry.createdAt,
    title: title ?? resourceItemTitle(entry, kind, mention),
    previewText: previewText ?? resourceItemPreview(entry, kind, mention)
  };
}

function resourceItemTitle(entry, kind, mention = null) {
  const source = resolveEntrySourceDescriptor(entry);
  if (kind === ResourceKind.MENTION) {
    return mention ? `@${mention}` : entry.summaryText ?? "";
  }
  return (
    source?.title ??
    source?.locator ??
    entry?.summaryText ??
    ""
  );
}

function resourceItemPreview(entry, kind, mention = null) {
  const title = resourceItemTitle(entry, kind, mention);
  const locator = resolveEntrySourceDescriptor(entry)?.locator ?? "";
  if (kind === ResourceKind.MENTION) {
    return entry?.summaryText ?? `@${mention ?? ""}`;
  }
  if (locator && locator !== title) {
    return locator;
  }
  return entry?.summaryText ?? "";
}

function isMedia(entry) {
  const kind = resolveEntrySourceDescriptor(entry)?.sourceKind;
  if (kind === "image" || kind === "video" || kind === "audio" || kind === "document") {
    return true;
  }
  return false;
}

function isLink(entry) {
  if (!entry) {
    return false;
  }
  if (entry.kind === "source") {
    return true;
  }
  return Boolean(resolveEntrySourceDescriptor(entry)?.locator);
}

function isMention(entry) {
  return (entry?.objectMentions ?? []).length > 0;
}

function attachmentResources(entry) {
  const attachments = Array.isArray(entry?.body?.attachments) ? entry.body.attachments : [];
  return attachments
    .map((attachment, index) => createAttachmentResource(entry, attachment, index))
    .filter(Boolean);
}

function createAttachmentResource(entry, attachment, index) {
  const locator = normalizeAttachmentLocator(attachment);
  if (!locator) {
    return null;
  }

  const title = String(attachment?.fileName ?? "").trim() || resourceItemTitle(entry, ResourceKind.MEDIA);
  const previewText =
    String(attachment?.mimeType ?? "").trim() ||
    String(attachment?.relativePath ?? "").trim() ||
    String(attachment?.fileName ?? "").trim() ||
    resourceItemPreview(entry, ResourceKind.MEDIA);

  return createResource(entry, ResourceKind.MEDIA, {
    locator,
    sourceKind: classifyAttachmentSourceKind(attachment),
    attachment,
    title,
    previewText,
    idSuffix: `attachment::${index}`
  });
}

function normalizeAttachmentLocator(attachment) {
  const relativePath = String(attachment?.relativePath ?? "").trim();
  if (relativePath) {
    return relativePath;
  }
  const fileName = String(attachment?.fileName ?? "").trim();
  return fileName || null;
}

function classifyAttachmentSourceKind(attachment) {
  const locator = normalizeAttachmentLocator(attachment);
  if (!locator) {
    return null;
  }
  const mimeType = String(attachment?.mimeType ?? "").trim().toLowerCase();
  const explicitKind = attachmentKindFromMimeType(mimeType);
  return classifyLocatorKind({ locator, explicitKind });
}

function attachmentKindFromMimeType(mimeType) {
  if (!mimeType) {
    return null;
  }
  if (mimeType.startsWith("image/")) {
    return "image";
  }
  if (mimeType.startsWith("video/")) {
    return "video";
  }
  if (mimeType.startsWith("audio/")) {
    return "audio";
  }
  if (
    mimeType === "application/pdf" ||
    mimeType.startsWith("text/") ||
    mimeType.includes("word") ||
    mimeType.includes("excel") ||
    mimeType.includes("powerpoint") ||
    mimeType.includes("officedocument") ||
    mimeType.includes("epub")
  ) {
    return "document";
  }
  return null;
}
