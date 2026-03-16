import { useMemo } from "react";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { KIND_LABELS } from "../../lib/constants.js";

export function StreamInspector() {
  const { home } = useWorkbenchContext();
  const entries = home?.inboxEntries ?? [];
  const threads = home?.threads ?? [];
  const rc = home?.resourceCounts ?? { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 };

  const kindCounts = useMemo(() => {
    const counts = {};
    for (const e of entries) {
      const k = e.kind ?? "note";
      counts[k] = (counts[k] ?? 0) + 1;
    }
    return Object.entries(counts).sort((a, b) => b[1] - a[1]);
  }, [entries]);

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
