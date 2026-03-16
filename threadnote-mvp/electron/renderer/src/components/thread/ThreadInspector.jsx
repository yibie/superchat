import { useState, useCallback } from "react";
import { cn } from "../../lib/cn.js";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { PreparedView } from "./PreparedView.jsx";
import { RestartNote } from "./RestartNote.jsx";
import { ThreadResources } from "./ThreadResources.jsx";

const TABS = [
  { key: "overview", label: "Overview" },
  { key: "ai", label: "AI" },
  { key: "resources", label: "Resources" },
];

function OverviewTab({ threadDetail }) {
  const claims = threadDetail?.claims ?? [];
  const relations = threadDetail?.discourseRelations ?? [];
  const snapshot = threadDetail?.aiSnapshot;

  return (
    <div className="space-y-4">
      {/* Restart note — shown prominently when available */}
      {snapshot?.restartNote && (
        <RestartNote
          restartNote={snapshot.restartNote}
          recoveryLines={snapshot.recoveryLines}
          openLoops={snapshot.openLoops}
          nextAction={snapshot.nextAction}
        />
      )}

      {/* Claims */}
      <section>
        <h3 className="text-[11px] font-medium text-text-tertiary uppercase tracking-wider mb-2">
          Claims ({claims.length})
        </h3>
        {claims.length === 0 ? (
          <p className="text-sm text-text-tertiary">No claims recorded.</p>
        ) : (
          <ul className="space-y-1.5">
            {claims.map((claim, i) => (
              <li
                key={claim.id ?? i}
                className="text-sm text-text-secondary rounded-md bg-elevated px-2.5 py-1.5"
              >
                <span className="text-xs text-text-tertiary uppercase mr-1.5">{claim.status}</span>
                {claim.statement}
              </li>
            ))}
          </ul>
        )}
      </section>

      {/* Discourse relations */}
      <section>
        <h3 className="text-[11px] font-medium text-text-tertiary uppercase tracking-wider mb-2">
          Relations ({relations.length})
        </h3>
        {relations.length === 0 ? (
          <p className="text-sm text-text-tertiary">No discourse relations.</p>
        ) : (
          <ul className="space-y-1.5">
            {relations.map((rel, i) => (
              <li
                key={rel.id ?? i}
                className="text-sm text-text-secondary rounded-md bg-elevated px-2.5 py-1.5"
              >
                <span className="font-medium text-text">{rel.kind}</span>
              </li>
            ))}
          </ul>
        )}
      </section>
    </div>
  );
}

function AITab({ threadDetail, onPrepare, preparing }) {
  const snapshot = threadDetail?.aiSnapshot;

  return (
    <div className="space-y-4">
      <PreparedView aiSnapshot={snapshot} />

      {snapshot && (
        <RestartNote
          restartNote={snapshot.restartNote}
          recoveryLines={snapshot.recoveryLines}
          openLoops={snapshot.openLoops}
          nextAction={snapshot.nextAction}
        />
      )}

      <button
        onClick={onPrepare}
        disabled={preparing}
        className={cn(
          "w-full rounded-md px-3 py-2 text-sm font-medium transition-colors",
          "bg-accent text-white hover:bg-accent-hover",
          "disabled:opacity-50 disabled:cursor-not-allowed"
        )}
      >
        {preparing ? "Preparing..." : snapshot ? "Re-prepare" : "Prepare Thread"}
      </button>
    </div>
  );
}

export function ThreadInspector({ threadID }) {
  const workbench = useWorkbenchContext();
  const [activeTab, setActiveTab] = useState("overview");
  const [preparing, setPreparing] = useState(false);

  const threadDetail = workbench.thread;

  const handlePrepare = useCallback(async () => {
    if (!threadID || preparing) return;
    setPreparing(true);
    try {
      await workbench.prepareThread({ threadID });
      await workbench.openThread(threadID);
    } finally {
      setPreparing(false);
    }
  }, [threadID, preparing, workbench]);

  return (
    <div className="flex flex-col h-full">
      {/* Tab bar */}
      <div className="flex items-center gap-0.5 px-3 py-1.5 shrink-0">
        {TABS.map((tab) => (
          <button
            key={tab.key}
            onClick={() => setActiveTab(tab.key)}
            className={cn(
              "px-2.5 py-1 rounded-md text-xs font-medium transition-colors",
              activeTab === tab.key
                ? "bg-elevated text-text"
                : "text-text-tertiary hover:text-text hover:bg-elevated/50"
            )}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* Tab content */}
      <div className="flex-1 overflow-y-auto p-3">
        {activeTab === "overview" && (
          <OverviewTab threadDetail={threadDetail} />
        )}

        {activeTab === "ai" && (
          <AITab
            threadDetail={threadDetail}
            onPrepare={handlePrepare}
            preparing={preparing}
          />
        )}

        {activeTab === "resources" && (
          <ThreadResources entries={threadDetail?.entries ?? []} />
        )}
      </div>
    </div>
  );
}
