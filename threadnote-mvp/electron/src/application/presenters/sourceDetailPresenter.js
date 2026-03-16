import { resolveEntrySourceDescriptor } from "../../domain/resources/richSourceDescriptor.js";

export function presentSourceDetail({ resource, threads = [] } = {}) {
  if (!resource) {
    return null;
  }

  const thread = resource.threadID ? (threads ?? []).find((item) => item.id === resource.threadID) ?? null : null;
  const source = resolveEntrySourceDescriptor(resource.entry);
  const locator = source?.locator ?? null;

  return {
    entryID: resource.entry?.id ?? null,
    title: resource.title || resource.kind || "Resource",
    kindLabel: formatKindLabel(resource.kind),
    locator,
    summary: resource.entry?.summaryText || resource.previewText || "No summary available.",
    citation: source?.citation || (resource.previewText && resource.previewText !== locator ? resource.previewText : ""),
    mentionLabels: resource.mentionLabels ?? [],
    threadID: resource.threadID ?? null,
    threadTitle: thread?.title ?? null
  };
}

function formatKindLabel(value) {
  return String(value ?? "resource")
    .replace(/^./, (match) => match.toUpperCase());
}
