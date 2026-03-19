import { useMemo } from "react";
import { normalizeEntryMode } from "../../../../src/domain/models/threadnoteModels.js";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { KIND_LABELS } from "../../lib/constants.js";

export function StreamInspector() {
  const { home } = useWorkbenchContext();
  const entries = home?.streamPage?.items ?? home?.inboxEntries ?? [];
  const threads = home?.threads ?? [];
  const routeDebugByEntryID = home?.aiState?.routeDebugByEntryID ?? {};
  const queue = home?.aiState?.queue ?? null;
  const activeOperations = home?.aiState?.activeOperations ?? [];
  const rc = home?.resourceCounts ?? { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 };

  const kindCounts = useMemo(() => {
    const counts = {};
    for (const e of entries) {
      const k = normalizeEntryMode(e.kind ?? "note");
      counts[k] = (counts[k] ?? 0) + 1;
    }
    return Object.entries(counts).sort((a, b) => b[1] - a[1]);
  }, [entries]);

  const routeRows = useMemo(() => {
    return entries
      .map((entry) => ({
        entry,
        debug: routeDebugByEntryID[entry.id] ?? null
      }))
      .filter((item) => item.debug)
      .sort((lhs, rhs) => new Date(rhs.debug.updatedAt ?? 0).getTime() - new Date(lhs.debug.updatedAt ?? 0).getTime())
      .slice(0, 6);
  }, [entries, routeDebugByEntryID]);

  return (
    <div className="p-3 space-y-5 text-sm">
      {/* Entries */}
      <Section title="Entries" count={entries.length}>
        {kindCounts.map(([kind, count]) => (
          <Row key={kind} label={KIND_LABELS[kind] ?? kind} value={count} />
        ))}
      </Section>

      {/* Active Threads */}
      <Section title="Active Threads" count={threads.length}>
        {threads.map((t) => (
          <Row key={t.id} label={t.title || "Untitled"} value={t.entryCount ?? 0} />
        ))}
      </Section>

      {/* Resources */}
      <Section title="Resources" count={rc.totalCount}>
        {rc.linkCount > 0 && <Row label="Links" value={rc.linkCount} />}
        {rc.mediaCount > 0 && <Row label="Media" value={rc.mediaCount} />}
        {rc.mentionCount > 0 && <Row label="Mentions" value={rc.mentionCount} />}
      </Section>

      <Section title="Route Debug" count={routeRows.length}>
        {routeRows.length > 0 ? routeRows.map(({ entry, debug }) => (
          <div key={entry.id} className="rounded-md bg-elevated px-2 py-1.5">
            <div className="flex items-center justify-between gap-2">
              <span className="truncate text-text-secondary">{entry.summaryText || "Untitled entry"}</span>
              <span className="text-xs uppercase tracking-wide text-text-tertiary">{debug.status}</span>
            </div>
            <p className="mt-1 text-xs text-text-tertiary">{debug.decisionReason || debug.message || "No route detail."}</p>
            {(debug.responseModelID || debug.finishReason) ? (
              <p className="mt-1 text-[11px] text-text-tertiary">
                {[debug.responseModelID, debug.finishReason].filter(Boolean).join(" · ")}
              </p>
            ) : null}
          </div>
        )) : (
          <p className="px-1 text-text-secondary">No route planner activity yet.</p>
        )}
      </Section>

      {queue ? (
        <Section title="AI Queue" count={queue.activeCount + queue.queueDepth}>
          <Row label="Active" value={queue.activeCount} />
          <Row label="Queued" value={queue.queueDepth} />
          <Row label="Concurrency" value={queue.maxConcurrent} />
          {activeOperations.map((operation) => (
            <div key={operation.label} className="px-1 py-0.5 text-xs text-text-secondary">
              {operation.label}
              {operation.elapsedMS != null ? ` · ${Math.round(operation.elapsedMS)}ms` : ""}
            </div>
          ))}
          {(queue.pendingLabels ?? []).map((label) => (
            <div key={`pending-${label}`} className="px-1 py-0.5 text-xs text-text-tertiary">{label}</div>
          ))}
        </Section>
      ) : null}
    </div>
  );
}

function Section({ title, count, children }) {
  return (
    <div>
      <div className="flex items-center justify-between mb-1.5">
        <span className="text-xs font-medium text-text-tertiary uppercase tracking-wider">{title}</span>
        <span className="text-xs font-semibold text-text-secondary">{count}</span>
      </div>
      <div className="space-y-0.5">{children}</div>
    </div>
  );
}

function Row({ label, value }) {
  return (
    <div className="flex items-center justify-between px-1 py-0.5 rounded hover:bg-surface-hover">
      <span className="text-text-secondary truncate">{label}</span>
      <span className="text-text-tertiary tabular-nums ml-2">{value}</span>
    </div>
  );
}
