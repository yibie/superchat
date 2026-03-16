import { resolveEntrySourceDescriptor } from "../../domain/resources/richSourceDescriptor.js";

const KIND_COLORS = {
  note:          "#8e8e93",
  idea:          "#8e8e93",
  question:      "#6e3aba",
  claim:         "#c2620a",
  evidence:      "#1a8a3c",
  source:        "#0055c4",
  comparison:    "#1a8a3c",
  pattern:       "#1a8a3c",
  plan:          "#8e8e93",
  decided:       "#c2620a",
  solved:        "#c2620a",
  verified:      "#c2620a",
  dropped:       "#c2620a",
  handoff:       "#8e8e93",
  anchorWritten: "#8e8e93"
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
  const repliesByParentID = new Map();
  for (const entry of allEntries ?? []) {
    if (!entry?.parentEntryID) {
      continue;
    }
    const list = repliesByParentID.get(entry.parentEntryID) ?? [];
    list.push(entry);
    repliesByParentID.set(entry.parentEntryID, list);
  }

  return (entries ?? []).map((entry) =>
    presentEntryCard({
      entry,
      threadMap,
      repliesByParentID,
      discourseRelations,
      mode
    })
  );
}

function presentEntryCard({ entry, threadMap, repliesByParentID, discourseRelations, mode }) {
  const replies = (repliesByParentID.get(entry.id) ?? [])
    .slice()
    .sort((lhs, rhs) => toDate(lhs.createdAt).getTime() - toDate(rhs.createdAt).getTime())
    .map((reply) => presentReplyCard(reply));
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
    kind: entry.kind ?? "note",
    kindLabel: formatKindLabel(entry.kind),
    routeState: mode === "thread" ? "routed" : presentRouteState(entry),
    kindColor: KIND_COLORS[entry.kind ?? "note"] ?? "#8e8e93",
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
    canRoute: !entry.threadID && !entry.parentEntryID,
    canReply: true,
    canViewSource: Boolean(sourceLocator),
    canEdit: true,
    canDelete: true,
    replies
  };
}

function sourceHost(locator) {
  try {
    return new URL(locator).hostname;
  } catch {
    return String(locator);
  }
}

function presentReplyCard(entry) {
  return {
    id: entry.id,
    summaryText: (entry.summaryText ?? "").trim() || "(empty)",
    kind: entry.kind ?? "note",
    kindLabel: formatKindLabel(entry.kind),
    timeLabel: toDate(entry.createdAt).toLocaleTimeString([], { hour: "numeric", minute: "2-digit" }),
    objectNames: (entry.objectMentions ?? []).map((item) => item?.name).filter(Boolean),
    referenceBadges: (entry.references ?? []).map((reference) => reference.label).filter(Boolean)
  };
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
