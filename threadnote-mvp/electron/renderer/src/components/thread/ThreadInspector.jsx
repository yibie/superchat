import { useCallback, useMemo, useState } from "react";
import { cn } from "../../lib/cn.js";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { PreparedView } from "./PreparedView.jsx";
import { ThreadResources } from "./ThreadResources.jsx";

const TABS = [
  { key: "restart", label: "Restart Note" },
  { key: "prepare", label: "Prepare View" },
  { key: "memory", label: "Memory" },
  { key: "resources", label: "Resources" },
];

export function ThreadInspector({ threadID }) {
  const workbench = useWorkbenchContext();
  const { threadInspectorTab, setThreadInspectorTab } = useNavigationContext();
  const [preparing, setPreparing] = useState(false);
  const [preparedView, setPreparedView] = useState(null);
  const threadDetail = workbench.thread;
  const snapshot = threadDetail?.aiSnapshot ?? null;
  const activeTab = TABS.some((tab) => tab.key === threadInspectorTab) ? threadInspectorTab : "restart";

  const restartBlocks = useMemo(() => {
    return (snapshot?.blocks ?? []).map((block, index) => ({
      id: `${index}-${block.kind ?? "block"}`,
      kind: block.kind ?? "block",
      title: block.title ?? "",
      summary: block.summary ?? "",
      items: Array.isArray(block.items) ? block.items : [],
      tone: block.tone ?? "neutral"
    }));
  }, [snapshot]);

  const handlePrepare = useCallback(async () => {
    if (!threadID || preparing) {
      return;
    }
    setPreparing(true);
    try {
      const result = await workbench.prepareThread({ threadID });
      setPreparedView(result ?? null);
      setThreadInspectorTab("prepare");
      await workbench.openThread(threadID);
    } finally {
      setPreparing(false);
    }
  }, [preparing, setThreadInspectorTab, threadID, workbench]);

  return (
    <div className="flex h-full min-h-0 flex-col">
      <div className="shrink-0 border-b border-border px-3 py-2">
        <div className="flex flex-wrap items-center gap-1">
          {TABS.map((tab) => (
            <button
              key={tab.key}
              type="button"
              onClick={() => setThreadInspectorTab(tab.key)}
              className={cn(
                "rounded-md px-2.5 py-1 text-xs font-medium transition-colors",
                activeTab === tab.key
                  ? "bg-elevated text-text"
                  : "text-text-tertiary hover:text-text hover:bg-elevated/50"
              )}
            >
              {tab.label}
            </button>
          ))}
        </div>
      </div>

      <div className="flex-1 overflow-y-auto p-3">
        {activeTab === "restart" && (
          <RestartTab
            snapshot={snapshot}
            anchors={threadDetail?.anchors ?? []}
            restartBlocks={restartBlocks}
          />
        )}

        {activeTab === "prepare" && (
          <PrepareTab
            preparedView={preparedView}
            preparing={preparing}
            onPrepare={handlePrepare}
          />
        )}

        {activeTab === "memory" && (
          <MemoryTab memory={threadDetail?.memory ?? []} />
        )}

        {activeTab === "resources" && (
          <ResourcesTab entries={threadDetail?.entries ?? []} />
        )}
      </div>
    </div>
  );
}

function RestartTab({ snapshot, anchors, restartBlocks }) {
  const currentJudgment = snapshot?.currentJudgment ?? anchors.at(-1)?.stateSummary ?? "";
  const openLoops = snapshot?.openLoops ?? anchors.at(-1)?.openLoops ?? [];
  const savedAtLabel = formatTimestamp(snapshot?.synthesizedAt);

  return (
    <div className="space-y-4">
      <section className="space-y-1">
        <h3 className="text-sm font-semibold text-text">Restart Note</h3>
        <p className="text-xs text-text-tertiary">
          {savedAtLabel ? `Last saved ${savedAtLabel}` : "No AI synthesis saved yet."}
        </p>
      </section>

      {snapshot?.headline ? (
        <p className="text-sm font-medium text-text">{snapshot.headline}</p>
      ) : null}

      <p className="text-sm leading-6 text-text-secondary">
        {snapshot?.restartNote || "Restart note will appear here after synthesis is available."}
      </p>

      {restartBlocks.length > 0 ? (
        <section className="space-y-3">
          {restartBlocks.map((block) => (
            <div key={block.id} className="rounded-md bg-elevated px-3 py-2">
              <div className="mb-1 flex items-center justify-between gap-2">
                <span className="text-[11px] uppercase tracking-wide text-text-tertiary">
                  {formatLabel(block.kind)}
                </span>
                <span className="text-[11px] text-text-tertiary">{block.tone}</span>
              </div>
              {block.title ? <p className="text-sm font-medium text-text">{block.title}</p> : null}
              {block.summary ? <p className="mt-1 text-sm text-text-secondary">{block.summary}</p> : null}
              {block.items.length > 0 ? (
                <ul className="mt-2 space-y-1">
                  {block.items.map((item, index) => (
                    <li key={`${block.id}-${index}`} className="text-sm text-text-secondary">{item}</li>
                  ))}
                </ul>
              ) : null}
            </div>
          ))}
        </section>
      ) : null}

      <section className="space-y-2">
        <h3 className="text-sm font-semibold text-text">AI Debug</h3>
        <div>
          <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Current Judgment</p>
          <p className="mt-1 text-sm leading-6 text-text-secondary">
            {currentJudgment || "No current judgment available."}
          </p>
        </div>
        <div>
          <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Open Loops</p>
          {openLoops.length > 0 ? (
            <ul className="mt-1 space-y-1">
              {openLoops.map((loop, index) => (
                <li key={`loop-${index}`} className="text-sm text-text-secondary">{loop}</li>
              ))}
            </ul>
          ) : (
            <p className="mt-1 text-sm text-text-secondary">No open loops recorded.</p>
          )}
        </div>
      </section>
    </div>
  );
}

function PrepareTab({ preparedView, preparing, onPrepare }) {
  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between gap-3">
        <div>
          <h3 className="text-sm font-semibold text-text">Prepare View</h3>
          <p className="text-sm text-text-secondary">Draft a focused next-step view from the current thread state.</p>
        </div>
        <button
          type="button"
          onClick={onPrepare}
          disabled={preparing}
          className="rounded-md bg-elevated px-3 py-2 text-sm text-text transition-colors hover:bg-bg disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {preparing ? "Preparing..." : preparedView ? "Refresh" : "Prepare"}
        </button>
      </div>
      {preparedView ? (
        <PreparedView aiSnapshot={normalizePreparedView(preparedView)} />
      ) : (
        <p className="text-sm text-text-secondary">No prepared draft yet.</p>
      )}
      {preparedView?.contentState?.message ? (
        <p className="text-xs text-text-tertiary">{preparedView.contentState.message}</p>
      ) : null}
    </div>
  );
}

function MemoryTab({ memory }) {
  return (
    <div className="space-y-3">
      <section>
        <h3 className="text-sm font-semibold text-text">Thread Memory</h3>
        <p className="mt-1 text-sm text-text-secondary">{memory.length} memory records for this thread.</p>
      </section>

      {memory.length === 0 ? (
        <p className="text-sm text-text-secondary">No memory captured for this thread yet.</p>
      ) : (
        <div className="space-y-2">
          {memory.map((item, index) => (
            <div key={item.id ?? `${item.scope ?? "memory"}-${index}`} className="rounded-md bg-elevated px-3 py-2">
              <div className="flex items-center justify-between gap-2">
                <span className="text-[11px] uppercase tracking-wide text-text-tertiary">
                  {formatLabel(item.scope ?? item.kind ?? "memory")}
                </span>
                <span className="text-[11px] text-text-tertiary">{formatTimestamp(item.updatedAt ?? item.createdAt) ?? ""}</span>
              </div>
              <p className="mt-1 text-sm text-text-secondary">
                {item.summary ?? item.content ?? item.value ?? "Memory item"}
              </p>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

function ResourcesTab({ entries }) {
  return (
    <div className="space-y-3">
      <section>
        <h3 className="text-sm font-semibold text-text">Resources</h3>
        <p className="mt-1 text-sm text-text-secondary">Thread-linked source material and URL entries.</p>
      </section>
      <ThreadResources entries={entries} />
    </div>
  );
}

function normalizePreparedView(preparedView) {
  return {
    headline: preparedView.title ?? "Prepared View",
    currentJudgment: preparedView.contentState?.message ?? "",
    blocks: [
      preparedView.openLoops?.length > 0
        ? { kind: "LIST", content: preparedView.openLoops }
        : null,
      preparedView.recommendedNextSteps?.length > 0
        ? { kind: "LIST", content: preparedView.recommendedNextSteps }
        : null
    ].filter(Boolean)
  };
}

function formatLabel(value) {
  return String(value ?? "item")
    .replace(/([A-Z])/g, " $1")
    .replace(/[_-]+/g, " ")
    .replace(/^./, (match) => match.toUpperCase());
}

function formatTimestamp(value) {
  if (!value) {
    return null;
  }
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return null;
  }
  return date.toLocaleString([], {
    month: "short",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit"
  });
}
