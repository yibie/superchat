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

const STATUS_GROUPS = [
  { key: "decided", label: "Decisions" },
  { key: "solved", label: "Solved" },
  { key: "verified", label: "Verified" },
  { key: "dropped", label: "Dropped" }
];

const STATUS_OPTIONS = [
  { value: "open", label: "Open" },
  { value: "decided", label: "Decided" },
  { value: "solved", label: "Solved" },
  { value: "verified", label: "Verified" },
  { value: "dropped", label: "Dropped" }
];

export function ThreadInspector({ threadID }) {
  const workbench = useWorkbenchContext();
  const { threadInspectorTab, setThreadInspectorTab, focusEntry } = useNavigationContext();
  const [preparing, setPreparing] = useState(false);
  const [updatingStatusEntryID, setUpdatingStatusEntryID] = useState(null);
  const threadDetail = workbench.getThreadDetail(threadID);
  const snapshot = threadDetail?.aiSnapshot ?? null;
  const aiStatus = threadDetail?.aiStatus ?? null;
  const preparedView = threadDetail?.preparedView ?? null;
  const aiDebug = threadDetail?.aiDebug ?? null;
  const statusSummary = threadDetail?.statusSummary ?? {
    decided: [],
    solved: [],
    verified: [],
    dropped: []
  };
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
      await workbench.prepareThread({ threadID });
      setThreadInspectorTab("prepare");
      await workbench.openThread(threadID);
    } finally {
      setPreparing(false);
    }
  }, [preparing, setThreadInspectorTab, threadID, workbench]);

  const handleFocusStatusEntry = useCallback((entryID) => {
    if (!entryID || !threadID) {
      return;
    }
    focusEntry(entryID, { threadID });
  }, [focusEntry, threadID]);

  const handleUpdateStatus = useCallback(async (entryID, nextStatus) => {
    if (!entryID || !nextStatus || updatingStatusEntryID) {
      return;
    }
    setUpdatingStatusEntryID(entryID);
    try {
      await workbench.updateEntryStatus?.({ entryID, status: nextStatus });
      await workbench.openThread?.(threadID);
    } finally {
      setUpdatingStatusEntryID(null);
    }
  }, [threadID, updatingStatusEntryID, workbench]);

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
            thread={threadDetail?.thread ?? null}
            snapshot={snapshot}
            anchors={threadDetail?.anchors ?? []}
            restartBlocks={restartBlocks}
            status={aiStatus?.resume ?? null}
            aiDebug={aiDebug}
            threadID={threadID}
            statusSummary={statusSummary}
            onFocusEntry={handleFocusStatusEntry}
            onUpdateStatus={handleUpdateStatus}
            updatingStatusEntryID={updatingStatusEntryID}
          />
        )}

        {activeTab === "prepare" && (
          <PrepareTab
            preparedView={preparedView}
            preparing={preparing || aiStatus?.prepare?.status === "loading"}
            status={aiStatus?.prepare ?? null}
            onPrepare={handlePrepare}
          />
        )}

        {activeTab === "memory" && (
          <MemoryTab
            memory={threadDetail?.memory ?? []}
            memoryCount={threadDetail?.memoryCount ?? threadDetail?.memory?.length ?? 0}
          />
        )}

        {activeTab === "resources" && (
          <ResourcesTab resources={threadDetail?.resources ?? []} />
        )}
      </div>
    </div>
  );
}

function RestartTab({
  thread,
  snapshot,
  anchors,
  restartBlocks,
  status,
  aiDebug,
  threadID,
  statusSummary,
  onFocusEntry,
  onUpdateStatus,
  updatingStatusEntryID
}) {
  const currentJudgment = snapshot?.currentJudgment ?? anchors.at(-1)?.stateSummary ?? "";
  const openLoops = snapshot?.openLoops ?? anchors.at(-1)?.openLoops ?? [];
  const savedAtLabel = formatTimestamp(snapshot?.synthesizedAt);
  const stage = describeThreadStage(thread?.goalLayer?.currentStage);

  return (
    <div className="space-y-4">
      <section className="space-y-1">
        <h3 className="text-sm font-semibold text-text">Restart Note</h3>
        <p className="text-xs text-text-tertiary">
          {savedAtLabel ? `Last saved ${savedAtLabel}` : "No AI synthesis saved yet."}
        </p>
        {status?.message ? <p className="text-xs text-text-tertiary">{status.message}</p> : null}
      </section>

      {snapshot?.headline ? (
        <p className="text-sm font-medium text-text">{snapshot.headline}</p>
      ) : null}

      <p className="text-sm leading-6 text-text-secondary">
        {snapshot?.restartNote || "Restart note will appear here after synthesis is available."}
      </p>

      {stage ? (
        <section className="rounded-md bg-elevated px-3 py-2">
          <div className="mb-1 flex items-center gap-2">
            <span className="text-[11px] uppercase tracking-wide text-text-tertiary">Current Stage</span>
            <span className="text-sm font-medium text-text">{stage.label}</span>
          </div>
          <p className="text-sm leading-6 text-text-secondary">{stage.description}</p>
        </section>
      ) : null}

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

      <StatusTab
        threadID={threadID}
        statusSummary={statusSummary}
        onFocusEntry={onFocusEntry}
        onUpdateStatus={onUpdateStatus}
        updatingStatusEntryID={updatingStatusEntryID}
      />

      <section className="space-y-2">
        <h3 className="text-sm font-semibold text-text">AI Debug</h3>
        <StatusRow label="Resume Status" value={status?.status ?? "idle"} />
        {status?.errorKind ? <StatusRow label="Error" value={status.errorKind} /> : null}
        {status?.responseModelID ? <StatusRow label="Model" value={status.responseModelID} /> : null}
        {status?.finishReason ? <StatusRow label="Finish" value={status.finishReason} /> : null}
        <OperationTelemetry status={status} />
        <QueueTelemetry aiDebug={aiDebug} />
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

function describeThreadStage(value) {
  const normalized = String(value ?? "").trim().toLowerCase();
  if (!normalized) {
    return null;
  }

  const descriptions = {
    framing: "This thread is still defining the question, scope, and success condition before collecting more material.",
    gathering: "This thread is collecting notes, evidence, and source material to reduce uncertainty.",
    synthesizing: "This thread is connecting signals, comparing options, and turning raw notes into a coherent judgment.",
    concluding: "This thread is settling the current judgment, decisions, and next actions so the work can move forward.",
    working: "This thread is actively in progress, but the current stage has not been categorized into the standard thinking flow."
  };

  return {
    label: formatLabel(normalized),
    description:
      descriptions[normalized] ??
      "This thread has a custom stage label. Treat it as lightweight context for how the work should resume."
  };
}

function PrepareTab({ preparedView, preparing, status, onPrepare }) {
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
      {status?.message ? <p className="text-xs text-text-tertiary">{status.message}</p> : null}
      <div className="space-y-1 rounded-md bg-elevated px-3 py-2">
        <StatusRow label="Prepare Status" value={status?.status ?? "idle"} />
        {status?.errorKind ? <StatusRow label="Error" value={status.errorKind} /> : null}
        {status?.responseModelID ? <StatusRow label="Model" value={status.responseModelID} /> : null}
        {status?.finishReason ? <StatusRow label="Finish" value={status.finishReason} /> : null}
        <OperationTelemetry status={status} />
      </div>
    </div>
  );
}

function StatusTab({ threadID, statusSummary, onFocusEntry, onUpdateStatus, updatingStatusEntryID }) {
  const hasAnyItems = STATUS_GROUPS.some((group) => (statusSummary?.[group.key] ?? []).length > 0);

  return (
    <div className="space-y-4">
      <section>
        <h3 className="text-sm font-semibold text-text">Thread Outcomes</h3>
        <p className="mt-1 text-sm text-text-secondary">Settled decisions and outcomes gathered from entry-level status signals.</p>
      </section>

      {!hasAnyItems ? (
        <p className="text-sm text-text-secondary">No thread outcomes yet.</p>
      ) : (
        <div className="space-y-4">
          {STATUS_GROUPS.map((group) => {
            const items = statusSummary?.[group.key] ?? [];
            if (items.length === 0) {
              return null;
            }
            return (
              <section key={group.key} className="space-y-2">
                <div className="flex items-center justify-between gap-2">
                  <h4 className="text-xs font-semibold uppercase tracking-wide text-text-tertiary">{group.label}</h4>
                  <span className="text-[11px] text-text-tertiary">{items.length}</span>
                </div>
                <div className="space-y-2">
                  {items.map((item) => (
                    <div key={item.id} className="rounded-md border border-border/70 bg-elevated px-3 py-2">
                      <button
                        type="button"
                        onClick={() => onFocusEntry(item.id)}
                        className="w-full text-left"
                      >
                        <p className="text-sm font-medium text-text">
                          {item.summaryText || "Untitled entry"}
                        </p>
                        <p className="mt-1 text-xs text-text-tertiary">
                          {formatLabel(item.kind)} · {formatLabel(item.source)} · {formatTimestamp(item.updatedAt) ?? "Unknown time"}
                        </p>
                      </button>
                      <div className="mt-2 flex items-center justify-between gap-3">
                        <span className="text-[11px] uppercase tracking-wide text-text-tertiary">
                          Thread {threadID ? "status" : "entry status"}
                        </span>
                        <label className="flex items-center gap-2 text-xs text-text-secondary">
                          <span className="sr-only">Update entry status</span>
                          <select
                            aria-label={`Update status for ${item.summaryText || item.id}`}
                            value={item.status}
                            disabled={updatingStatusEntryID === item.id}
                            onChange={(event) => void onUpdateStatus(item.id, event.target.value)}
                            className="rounded-md border border-border/70 bg-bg px-2 py-1 text-xs text-text outline-none transition-colors focus:border-text-tertiary disabled:cursor-not-allowed disabled:opacity-60"
                          >
                            {STATUS_OPTIONS.map((option) => (
                              <option key={option.value} value={option.value}>
                                {option.label}
                              </option>
                            ))}
                          </select>
                        </label>
                      </div>
                    </div>
                  ))}
                </div>
              </section>
            );
          })}
        </div>
      )}
    </div>
  );
}

function StatusRow({ label, value }) {
  return (
    <div className="flex items-center justify-between gap-3 text-xs">
      <span className="uppercase tracking-wide text-text-tertiary">{label}</span>
      <span className="text-text-secondary">{value || "n/a"}</span>
    </div>
  );
}

function OperationTelemetry({ status }) {
  const rows = [
    { label: "Prompt Stats", value: status?.promptStats ?? "" },
    { label: "Started", value: formatTimestamp(status?.startedAt) ?? "" },
    { label: "Updated", value: formatTimestamp(status?.updatedAt) ?? "" },
    { label: "Elapsed", value: formatElapsed(status?.elapsedMS) ?? "" },
    { label: "Raw Error", value: status?.rawErrorMessage ?? "" }
  ].filter((row) => row.value);

  if (rows.length === 0) {
    return null;
  }

  return (
    <div className="space-y-1 rounded-md border border-border/60 px-2 py-2">
      <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Operation Telemetry</p>
      {rows.map((row) => (
        <StatusRow key={row.label} label={row.label} value={row.value} />
      ))}
    </div>
  );
}

function QueueTelemetry({ aiDebug }) {
  const queue = aiDebug?.queue ?? null;
  const operations = aiDebug?.activeOperations ?? [];
  if (!queue) {
    return null;
  }

  return (
    <div className="space-y-1 rounded-md border border-border/60 px-2 py-2">
      <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Queue Snapshot</p>
      <StatusRow label="Active" value={String(queue.activeCount ?? 0)} />
      <StatusRow label="Queued" value={String(queue.queueDepth ?? 0)} />
      <StatusRow label="Concurrency" value={String(queue.maxConcurrent ?? 0)} />
      {operations.length > 0 ? (
        <div className="pt-1">
          <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Current Ops</p>
          <div className="mt-1 space-y-1">
            {operations.map((operation) => (
              <p key={operation.label} className="text-xs text-text-secondary">
                {operation.label}
                {operation.elapsedMS != null ? ` · ${formatElapsed(operation.elapsedMS)}` : ""}
              </p>
            ))}
          </div>
        </div>
      ) : null}
      {queue.pendingOperations?.length > 0 ? (
        <div className="pt-1">
          <p className="text-[11px] uppercase tracking-wide text-text-tertiary">Pending Ops</p>
          <div className="mt-1 space-y-1">
            {queue.pendingOperations.map((operation) => (
              <p key={operation.label} className="text-xs text-text-secondary">
                {operation.label}
                {operation.waitMS != null ? ` · waiting ${formatElapsed(operation.waitMS)}` : ""}
              </p>
            ))}
          </div>
        </div>
      ) : null}
    </div>
  );
}

function MemoryTab({ memory, memoryCount = 0 }) {
  const previewCount = memory.length;
  return (
    <div className="space-y-3">
      <section>
        <h3 className="text-sm font-semibold text-text">Thread Memory</h3>
        <p className="mt-1 text-sm text-text-secondary">
          {memoryCount} memory records for this thread.
          {memoryCount > previewCount ? ` Showing latest ${previewCount}.` : ""}
        </p>
      </section>

      {previewCount === 0 ? (
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

function ResourcesTab({ resources }) {
  return (
    <div className="space-y-3">
      <section>
        <h3 className="text-sm font-semibold text-text">Resources</h3>
        <p className="mt-1 text-sm text-text-secondary">Thread-linked links, attachments, and mention anchors.</p>
      </section>
      <ThreadResources resources={resources} />
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

function formatElapsed(value) {
  const number = Number(value);
  if (!Number.isFinite(number) || number < 0) {
    return null;
  }
  if (number < 1000) {
    return `${Math.round(number)}ms`;
  }
  return `${(number / 1000).toFixed(1)}s`;
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
