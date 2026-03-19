import { resolveEntrySourceDescriptor } from "../../domain/resources/richSourceDescriptor.js";
import { normalizeEntryMode } from "../../domain/models/threadnoteModels.js";

const KIND_COLORS = {
  note:          "#8e8e93",
  question:      "#6e3aba",
  source:        "#0055c4",
};

function toDate(value) {
  const parsed = value instanceof Date ? value : new Date(value);
  return Number.isNaN(parsed.getTime()) ? new Date(0) : parsed;
}

export function presentTimelineEntries({
  entries = [],
  allEntries = [],
  threads = [],
  discourseRelations = [],
  mode = "stream"
} = {}) {
  const threadMap = new Map((threads ?? []).map((thread) => [thread.id, thread]));
  return (entries ?? []).map((entry) =>
    presentEntryCard({
      entry,
      threadMap,
      discourseRelations,
      mode
    })
  );
}

function presentEntryCard({ entry, threadMap, discourseRelations, mode }) {
  const entryMode = normalizeEntryMode(entry.kind);
  const relatedKinds = Array.from(
    new Set(
      (discourseRelations ?? [])
        .filter((relation) => relation.sourceEntryID === entry.id || relation.targetEntryID === entry.id)
        .map((relation) => formatKindLabel(relation.kind))
    )
  );
  const source = resolveEntrySourceDescriptor(entry);
  const sourceLocator = source?.locator ?? null;

  return {
    id: entry.id,
    summaryText: (entry.summaryText ?? "").trim() || "(empty)",
    kind: entryMode,
    kindLabel: formatKindLabel(entryMode),
    aiActivity: entry.aiActivity ?? null,
    routeState: mode === "thread" ? "routed" : presentRouteState(entry),
    kindColor: KIND_COLORS[entryMode] ?? "#8e8e93",
    threadID: entry.threadID ?? null,
    threadColor: entry.threadID ? (threadMap.get(entry.threadID)?.color ?? null) : null,
    timeLabel: toDate(entry.createdAt).toLocaleTimeString([], { hour: "numeric", minute: "2-digit" }),
    objectNames: (entry.objectMentions ?? []).map((item) => item?.name).filter(Boolean),
    referenceBadges: (entry.references ?? []).map((reference) => reference.label).filter(Boolean),
    relationBadges: relatedKinds,
    sourceLocator,
    sourceLabel: sourceLocator ? sourceHost(sourceLocator) : null,
    sourceTitle: source?.title ?? null,
    threadTitle: entry.threadID ? threadMap.get(entry.threadID)?.title ?? null : null,
    canRoute: !entry.threadID,
    canReply: true,
    canViewSource: Boolean(sourceLocator),
    canEdit: true,
    canDelete: true
  };
}

function sourceHost(locator) {
  try {
    return new URL(locator).hostname;
  } catch {
    return String(locator);
  }
}

function presentRouteState(entry) {
  if (entry.threadID) {
    return "routed";
  }
  if (entry.inboxState === "routing") {
    return "routing";
  }
  return "inbox";
}

function formatKindLabel(value) {
  return String(value ?? "note")
    .replace(/([A-Z])/g, " $1")
    .replace(/^./, (match) => match.toUpperCase());
}
