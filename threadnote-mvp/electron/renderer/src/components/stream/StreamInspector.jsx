import { useEffect, useMemo, useRef, useState } from "react";
import { normalizeEntryMode } from "../../../../src/domain/models/threadnoteModels.js";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { cn } from "../../lib/cn.js";
import { KIND_LABELS } from "../../lib/constants.js";

const TABS = [
  { key: "overview", label: "Overview" },
  { key: "search", label: "Search" }
];

const SEARCH_DEBOUNCE_MS = 200;

export function StreamInspector() {
  const workbench = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const { home } = workbench;
  const entries = home?.streamPage?.items ?? home?.inboxEntries ?? [];
  const threads = home?.threads ?? [];
  const routeDebugByEntryID = home?.aiState?.routeDebugByEntryID ?? {};
  const queue = home?.aiState?.queue ?? null;
  const activeOperations = home?.aiState?.activeOperations ?? [];
  const rc = home?.resourceCounts ?? { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 };
  const search = workbench.search ?? {
    query: "",
    results: [],
    loading: false,
    error: null,
    lastCompletedQuery: ""
  };
  const [activeTab, setActiveTab] = useState("overview");
  const [query, setQuery] = useState(search.query ?? "");

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
    <div className="flex h-full min-h-0 flex-col">
      <div className="shrink-0 border-b border-border px-3 py-2">
        <div className="flex flex-wrap items-center gap-1">
          {TABS.map((tab) => (
            <button
              key={tab.key}
              type="button"
              onClick={() => setActiveTab(tab.key)}
              className={cn(
                "rounded-md px-2.5 py-1 text-xs font-medium transition-colors",
                activeTab === tab.key
                  ? "bg-elevated text-text"
                  : "text-text-tertiary hover:bg-elevated/50 hover:text-text"
              )}
            >
              {tab.label}
            </button>
          ))}
        </div>
      </div>

      <div className="flex-1 overflow-y-auto p-3">
        {activeTab === "overview" ? (
          <div className="space-y-5 text-sm">
            <Section title="Entries" count={entries.length}>
              {kindCounts.map(([kind, count]) => (
                <Row key={kind} label={KIND_LABELS[kind] ?? kind} value={count} />
              ))}
            </Section>

            <Section title="Active Threads" count={threads.length}>
              {threads.map((t) => (
                <Row key={t.id} label={t.title || "Untitled"} value={t.entryCount ?? 0} />
              ))}
            </Section>

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
        ) : (
          <SearchTab
            query={query}
            setQuery={setQuery}
            search={search}
            onSearch={workbench.searchEntries}
            onOpenResult={(result) => focusEntry(result.entryID, { threadID: result.threadID ?? null })}
          />
        )}
      </div>
    </div>
  );
}

function SearchTab({ query, setQuery, search, onSearch, onOpenResult }) {
  const inputRef = useRef(null);

  useEffect(() => {
    inputRef.current?.focus();
    inputRef.current?.select();
  }, []);

  useEffect(() => {
    const handle = window.setTimeout(() => {
      void onSearch(query);
    }, SEARCH_DEBOUNCE_MS);
    return () => window.clearTimeout(handle);
  }, [onSearch, query]);

  const resultLabel = useMemo(() => {
    if (!search.lastCompletedQuery) {
      return "Search all entries";
    }
    return `${search.results.length} result${search.results.length === 1 ? "" : "s"} for “${search.lastCompletedQuery}”`;
  }, [search.lastCompletedQuery, search.results.length]);

  return (
    <div className="space-y-3">
      <div>
        <p className="text-sm font-semibold text-text">Search</p>
        <p className="mt-1 text-xs leading-5 text-text-secondary">
          Find any entry and jump back to its thread or inbox context.
        </p>
      </div>

      <input
        ref={inputRef}
        type="search"
        value={query}
        onChange={(event) => setQuery(event.target.value)}
        onKeyDown={(event) => {
          if (event.key === "Escape" && query) {
            event.preventDefault();
            setQuery("");
          }
        }}
        placeholder="Search all entries..."
        className="soft-focus-field w-full rounded-xl border border-border/70 bg-elevated px-3 py-2.5 text-sm text-text"
        aria-label="Search entries"
      />

      <p className="text-xs text-text-tertiary">{resultLabel}</p>

      {!query.trim() ? (
        <SearchEmptyState
          title="Search all entries"
          body="Use keywords to find notes, questions, and sources across every thread."
        />
      ) : search.loading ? (
        <SearchEmptyState
          title="Searching…"
          body={`Looking for matches for “${query.trim()}”.`}
        />
      ) : search.error ? (
        <SearchEmptyState
          title="Search failed"
          body={search.error}
        />
      ) : search.results.length === 0 ? (
        <SearchEmptyState
          title="No matches"
          body={`No entries matched “${search.lastCompletedQuery || query.trim()}”.`}
        />
      ) : (
        <div className="space-y-2">
          {search.results.map((result) => (
            <button
              key={result.entryID}
              type="button"
              onClick={() => onOpenResult(result)}
              className="w-full rounded-2xl border border-border/70 bg-surface px-3 py-3 text-left transition-colors hover:bg-elevated/70"
            >
              <div className="flex items-center justify-between gap-3">
                <span className="text-xs font-medium uppercase tracking-wide text-text-tertiary">
                  {KIND_LABELS[result.entryKind] ?? result.entryKind ?? "Entry"}
                </span>
                <time className="text-xs text-text-tertiary" dateTime={result.createdAt}>
                  {formatSearchTime(result.createdAt)}
                </time>
              </div>
              <p className="mt-2 text-sm leading-6 text-text">
                {result.bodySnippet || "Untitled entry"}
              </p>
              <div className="mt-3 flex items-center gap-3 text-xs text-text-secondary">
                <span className="truncate">{result.threadTitle || "Inbox"}</span>
              </div>
            </button>
          ))}
        </div>
      )}
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

function SearchEmptyState({ title, body }) {
  return (
    <div className="flex min-h-[220px] flex-col items-center justify-center rounded-3xl border border-dashed border-border/70 bg-surface/40 px-4 text-center">
      <p className="text-sm font-semibold text-text">{title}</p>
      <p className="mt-2 max-w-md text-sm leading-6 text-text-secondary">{body}</p>
    </div>
  );
}

function formatSearchTime(value) {
  if (!value) {
    return "";
  }
  try {
    return new Date(value).toLocaleString(undefined, {
      month: "short",
      day: "numeric",
      hour: "2-digit",
      minute: "2-digit"
    });
  } catch {
    return String(value);
  }
}
