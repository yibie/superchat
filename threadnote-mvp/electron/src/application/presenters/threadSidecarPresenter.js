function toDate(value) {
  const parsed = value instanceof Date ? value : new Date(value);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
}

export function presentThreadSidecar({ threadView, selectedTab = "restart" } = {}) {
  if (!threadView?.thread) {
    return null;
  }

  return {
    selectedTab,
    restart: presentRestartTab(threadView),
    resources: presentResourcesTab(threadView)
  };
}

function presentRestartTab(threadView) {
  const snapshot = threadView.aiSnapshot;
  if (!snapshot) {
    return {
      status: "unavailable",
      headline: "Restart Note unavailable",
      statusLabel: "Unavailable",
      message: "AI synthesis has not been generated for this thread yet.",
      blocks: [],
      debug: {
        currentJudgment: threadView.anchors.at(-1)?.stateSummary ?? "",
        openLoops: threadView.anchors.at(-1)?.openLoops ?? []
      },
      savedAtLabel: null
    };
  }

  return {
    status: "ready",
    headline: snapshot.headline || snapshot.restartNote || "Restart Note",
    statusLabel: "Ready",
    message: snapshot.currentJudgment || snapshot.restartNote || "AI synthesis is available.",
    blocks: (snapshot.blocks ?? []).map((block, index) => ({
      id: `${index}-${block.kind ?? "block"}`,
      kindLabel: formatKindLabel(block.kind ?? "block"),
      title: block.title ?? "",
      summary: block.summary ?? "",
      items: block.items ?? [],
      tone: block.tone ?? "neutral"
    })),
    debug: {
      currentJudgment: snapshot.currentJudgment ?? "",
      openLoops: snapshot.openLoops ?? []
    },
    savedAtLabel: formatSavedAt(snapshot.synthesizedAt)
  };
}

function presentResourcesTab(threadView) {
  return {
    totalCount: threadView.resourceCounts?.totalCount ?? threadView.resources?.length ?? 0,
    byKind: threadView.resourceCounts ?? { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 },
    resources: (threadView.resources ?? []).map((resource, index) => ({
      id: resource.id ?? `${resource.kind}-${index}`,
      title: resource.title || resource.kind,
      previewText: resource.previewText || resource.kind,
      kindLabel: formatKindLabel(resource.kind)
    }))
  };
}

function formatSavedAt(value) {
  const date = toDate(value);
  if (!date) {
    return null;
  }
  return date.toLocaleString([], {
    month: "short",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit"
  });
}

function formatKindLabel(kind) {
  return String(kind ?? "item")
    .replace(/([A-Z])/g, " $1")
    .replace(/^./, (match) => match.toUpperCase());
}
