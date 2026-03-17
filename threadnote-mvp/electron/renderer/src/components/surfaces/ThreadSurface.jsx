import { useEffect, useCallback, useRef, useState } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { EntryList } from "../entries/EntryList.jsx";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { THREAD_COLORS } from "../../lib/constants.js";

export function ThreadSurface() {
  const {
    selectedThreadID,
    goBack,
    goToStream,
    focusedEntryTarget,
    clearFocusedEntry,
    showThreadInspectorTab,
    setInspectorOpen
  } = useNavigationContext();
  const workbench = useWorkbenchContext();
  const actions = useEntryActions();
  const listRef = useRef(null);
  const [highlightedEntryID, setHighlightedEntryID] = useState(null);
  const [archiving, setArchiving] = useState(false);

  const threadDetail = workbench.getThreadDetail(selectedThreadID);
  const threadLoading = workbench.isThreadLoading(selectedThreadID);
  const thread = threadDetail?.thread ?? null;
  const entries = threadDetail?.entries ?? [];
  const threads = workbench.home?.threads ?? [];

  useEffect(() => {
    if (selectedThreadID) {
      workbench.openThread(selectedThreadID);
    }
  }, [selectedThreadID]); // eslint-disable-line react-hooks/exhaustive-deps

  const sortedEntries = [...entries].sort(
    (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
  );

  useEffect(() => {
    if (!focusedEntryTarget?.entryID || focusedEntryTarget.threadID !== selectedThreadID) {
      return undefined;
    }
    const frame = requestAnimationFrame(() => {
      const target = listRef.current?.querySelector(`[data-entry-id="${focusedEntryTarget.entryID}"]`);
      if (!target) {
        return;
      }
      target.scrollIntoView({ block: "center", behavior: "smooth" });
      setHighlightedEntryID(focusedEntryTarget.entryID);
      clearFocusedEntry();
    });
    return () => cancelAnimationFrame(frame);
  }, [clearFocusedEntry, focusedEntryTarget, selectedThreadID, sortedEntries.length]);

  useEffect(() => {
    if (!highlightedEntryID) {
      return undefined;
    }
    const timer = window.setTimeout(() => setHighlightedEntryID(null), 1800);
    return () => window.clearTimeout(timer);
  }, [highlightedEntryID]);

  const handleSubmit = async (text, attachments, references) => {
    await workbench.submitCapture({ text, threadID: selectedThreadID, attachments, references });
  };

  const getEditorState = useCallback(() => ({
    threads,
    allEntries: entries,
    objects: [],
  }), [threads, entries]);

  const handleArchive = useCallback(async () => {
    if (!selectedThreadID || archiving) {
      return;
    }
    setArchiving(true);
    try {
      await workbench.archiveThread(selectedThreadID);
      goToStream();
    } finally {
      setArchiving(false);
    }
  }, [archiving, goToStream, selectedThreadID, workbench]);

  if (!selectedThreadID) {
    return (
      <div className="flex flex-col items-center justify-center h-full text-text-tertiary">
        <p className="text-sm">No thread selected.</p>
      </div>
    );
  }

  const color = THREAD_COLORS[thread?.color] ?? THREAD_COLORS.sky;
  const stageLabel = formatStage(thread?.goalLayer?.currentStage);
  const showLoadingState = !threadDetail && threadLoading;

  return (
    <div className="flex flex-col h-full">
      <div className="h-1 w-full shrink-0" style={{ backgroundColor: color }} />

      <div className="shrink-0">
        <div className="max-w-2xl mx-auto px-6 py-3 flex items-center gap-3">
          <IconButton
            label="Back"
            icon={"\u2190"}
            onClick={goBack}
            className="shrink-0"
          />
          <div className="min-w-0 flex-1 flex items-center gap-2">
            <h1 className="text-lg font-semibold text-text truncate">{thread?.title ?? "Thread"}</h1>
            <button
              type="button"
              onClick={() => {
                showThreadInspectorTab("restart");
                setInspectorOpen(true);
              }}
              className="inline-flex items-center rounded-full bg-elevated px-2 py-0.5 text-[11px] font-medium text-text-secondary hover:text-text transition-colors"
            >
              {stageLabel}
            </button>
          </div>
          <button
            type="button"
            onClick={handleArchive}
            disabled={archiving}
            className="inline-flex items-center rounded-md px-2 py-1 text-xs text-text-tertiary hover:text-danger hover:bg-danger-muted transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {archiving ? "Archiving..." : "Archive Thread"}
          </button>
        </div>
      </div>

      <div className="flex-1 overflow-y-auto" ref={listRef}>
        <div className="max-w-2xl mx-auto px-6 py-4 space-y-5">
          <section>
            <div className="mb-2 flex items-center justify-between gap-3">
              <div>
                <h2 className="text-sm font-semibold text-text">Continue</h2>
              </div>
            </div>
            {showLoadingState ? (
              <div className="rounded-lg border border-border/60 bg-elevated/40 px-4 py-6 text-sm text-text-tertiary">
                Loading thread…
              </div>
            ) : (
              <CaptureEditor
                onSubmit={handleSubmit}
                placeholder="#role @object [[reference]] or [[supports|reference]]"
                getEditorState={getEditorState}
              />
            )}
          </section>

          <section>
            <div className="mb-2 flex items-center justify-between gap-3">
              <div>
                <h2 className="text-sm font-semibold text-text">Working Stream</h2>
                <p className="text-xs text-text-tertiary">
                  {sortedEntries.length === 0 ? "No entries yet." : `${sortedEntries.length} entries`}
                </p>
              </div>
            </div>

            {showLoadingState ? (
              <div className="flex flex-col items-center justify-center py-16 text-text-tertiary">
                <p className="text-sm">Loading thread…</p>
              </div>
            ) : sortedEntries.length === 0 ? (
              <div className="flex flex-col items-center justify-center py-16 text-text-tertiary">
                <p className="text-sm">No entries yet.</p>
                <p className="text-xs mt-1">Start capturing above.</p>
              </div>
            ) : (
              <EntryList
                entries={sortedEntries}
                allEntries={entries}
                threads={threads}
                actions={actions}
                showThread={false}
                highlightedEntryID={highlightedEntryID}
              />
            )}
          </section>
        </div>
      </div>
    </div>
  );
}

function formatStage(value) {
  return String(value ?? "working")
    .replace(/([A-Z])/g, " $1")
    .replace(/[_-]+/g, " ")
    .replace(/^./, (match) => match.toUpperCase());
}
