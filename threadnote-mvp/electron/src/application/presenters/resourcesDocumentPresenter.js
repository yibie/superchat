import { ResourceKind } from "../../domain/resources/resourceDerivation.js";
import { resolveEntrySourceDescriptor } from "../../domain/resources/richSourceDescriptor.js";

export function presentResourcesDocument({ resources = [], counts, threads = [] } = {}) {
  const threadMap = new Map((threads ?? []).map((thread) => [thread.id, thread]));
  const sections = [
    buildSection(ResourceKind.LINK, "Links", resources, threadMap),
    buildSection(ResourceKind.MEDIA, "Media", resources, threadMap),
    buildSection(ResourceKind.MENTION, "Mentions", resources, threadMap)
  ];

  return {
    overview: [
      { id: "links", label: "Links", count: counts?.linkCount ?? 0 },
      { id: "media", label: "Media", count: counts?.mediaCount ?? 0 },
      { id: "mentions", label: "Mentions", count: counts?.mentionCount ?? 0 }
    ],
    sections
  };
}

function buildSection(kind, title, resources, threadMap) {
  const items = (resources ?? [])
    .filter((resource) => resource.kind === kind)
    .map((resource, index) => {
      const source = resolveEntrySourceDescriptor(resource.entry);
      return {
        id: resource.id ?? `${kind}-${index}`,
        resource,
        entryID: resource.entryID ?? resource.entry?.id ?? null,
        title: resource.title || formatKindLabel(kind),
        previewText: resource.previewText || resource.kind,
        kindLabel: formatKindLabel(kind),
        locator: resource.locator ?? source?.locator ?? null,
        threadID: resource.threadID ?? null,
        threadTitle: resource.threadID ? threadMap.get(resource.threadID)?.title ?? null : null
      };
    });

  return {
    id: kind,
    title,
    count: items.length,
    items
  };
}

function formatKindLabel(value) {
  return String(value ?? "resource")
    .replace(/^./, (match) => match.toUpperCase());
}
