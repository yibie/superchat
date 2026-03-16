import { presentTimelineEntries } from "./timelineEntryPresenter.js";

function toDate(value) {
  const parsed = value instanceof Date ? value : new Date(value);
  return Number.isNaN(parsed.getTime()) ? new Date(0) : parsed;
}

export function presentThreadDocument({ threadView, preparedView = null } = {}) {
  if (!threadView?.thread) {
    return null;
  }

  const topLevelEntries = (threadView.entries ?? []).filter((entry) => !entry.parentEntryID);
  const sortedDesc = topLevelEntries
    .slice()
    .sort((lhs, rhs) => toDate(rhs.createdAt).getTime() - toDate(lhs.createdAt).getTime());

  const grouped = new Map();
  const orderedKeys = [];
  for (const entry of sortedDesc) {
    const key = entry.sessionID ?? toDayKey(entry.createdAt);
    if (!grouped.has(key)) {
      grouped.set(key, []);
      orderedKeys.push(key);
    }
    grouped.get(key).push(entry);
  }

  const sections = orderedKeys.map((key) => {
    const items = grouped.get(key) ?? [];
    const ascending = items.slice().sort((lhs, rhs) => toDate(lhs.createdAt).getTime() - toDate(rhs.createdAt).getTime());
    const startedAt = ascending[0]?.createdAt ?? new Date();
    return {
      id: key,
      title: formatSectionTitle(startedAt),
      itemCount: ascending.length,
      items: presentTimelineEntries({
        entries: ascending,
        allEntries: threadView.entries ?? [],
        threads: [threadView.thread],
        discourseRelations: threadView.discourseRelations ?? [],
        mode: "thread"
      })
    };
  });

  return {
    header: {
      title: threadView.thread.title,
      goalStatement: threadView.thread.goalLayer?.goalStatement ?? "",
      stageLabel: formatStageLabel(threadView.thread.goalLayer?.currentStage),
      color: threadView.thread.color ?? "sky",
      claimCount: threadView.claims?.length ?? 0,
      entryCount: topLevelEntries.length,
      memoryCount: threadView.memory?.length ?? 0,
      resourceCount: threadView.resources?.length ?? 0
    },
    preparedView: presentPreparedView(preparedView),
    workingStreamSections: sections
  };
}

function presentPreparedView(preparedView) {
  if (!preparedView) {
    return null;
  }

  return {
    typeLabel: formatKindLabel(preparedView.type ?? "writing"),
    title: preparedView.title ?? "",
    openLoops: preparedView.openLoops ?? [],
    recommendedNextSteps: preparedView.recommendedNextSteps ?? [],
    status: preparedView.contentState?.status ?? "ready",
    message: preparedView.contentState?.message ?? ""
  };
}

function formatSectionTitle(value) {
  return toDate(value).toLocaleDateString([], {
    month: "long",
    day: "numeric",
    weekday: "short"
  });
}

function formatStageLabel(value) {
  if (!value) {
    return "Working";
  }
  return formatKindLabel(value);
}

function formatKindLabel(kind) {
  if (!kind) {
    return "Note";
  }
  return String(kind)
    .replace(/([A-Z])/g, " $1")
    .replace(/^./, (match) => match.toUpperCase());
}

function toDayKey(value) {
  const date = toDate(value);
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}
