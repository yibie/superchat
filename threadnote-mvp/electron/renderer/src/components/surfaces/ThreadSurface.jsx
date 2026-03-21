import { useEffect, useCallback, useRef, useState } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { EntryList } from "../entries/EntryList.jsx";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { showToast } from "../shared/FeedbackToast.jsx";
import { THREAD_COLORS } from "../../lib/constants.js";

export function ThreadSurface() {
  const {
    selectedThreadID,
    goToStream,
    focusedEntryTarget,
    clearFocusedEntry,
    showThreadInspectorTab,
    setInspectorOpen
  } = useNavigationContext();
  const workbench = useWorkbenchContext();
  const actions = useEntryActions();
  const listRef = useRef(null);
  const titleInputRef = useRef(null);
  const renamingRef = useRef(false);
  const [highlightedEntryID, setHighlightedEntryID] = useState(null);
  const [archiving, setArchiving] = useState(false);
  const [isEditingTitle, setIsEditingTitle] = useState(false);
  const [draftTitle, setDraftTitle] = useState("");

  const threadDetail = workbench.getThreadDetail(selectedThreadID);
  const threadLoading = workbench.isThreadLoading(selectedThreadID);
  const threadPageLoading = workbench.isThreadPageLoading?.(selectedThreadID) ?? false;
  const thread = threadDetail?.thread ?? null;
  const entriesPage = threadDetail?.entriesPage ?? {
    items: threadDetail?.entries ?? [],
    replies: [],
    hasMore: false,
    totalCount: (threadDetail?.entries ?? []).length
  };
  const allEntries = entriesPage.items ?? [];
  const threads = workbench.home?.threads ?? [];

  useEffect(() => {
    if (selectedThreadID) {
      workbench.openThread(selectedThreadID);
    }
  }, [selectedThreadID]); // eslint-disable-line react-hooks/exhaustive-deps

  const sortedEntries = [...(entriesPage.items ?? [])].sort(
    (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
  );

  useEffect(() => {
    if (!focusedEntryTarget?.entryID || focusedEntryTarget.threadID !== selectedThreadID) {
      return undefined;
    }
    const frame = requestAnimationFrame(() => {
      const primaryTarget = listRef.current?.querySelector(`[data-entry-id="${focusedEntryTarget.entryID}"]`);
      const replyTarget = listRef.current?.querySelector(`[data-reply-entry-id="${focusedEntryTarget.entryID}"]`);
      const target = primaryTarget ?? replyTarget?.closest?.("[data-entry-id]") ?? replyTarget ?? null;
      if (!target) {
        clearFocusedEntry();
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

  useEffect(() => {
    if (!isEditingTitle) {
      setDraftTitle(thread?.title ?? "");
    }
  }, [isEditingTitle, thread?.title]);

  useEffect(() => {
    if (!isEditingTitle) {
      return;
    }
    const frame = requestAnimationFrame(() => {
      titleInputRef.current?.focus();
      titleInputRef.current?.select();
    });
    return () => cancelAnimationFrame(frame);
  }, [isEditingTitle]);

  const handleSubmit = async (text, attachments, references) => {
    await workbench.submitCapture({ text, threadID: selectedThreadID, attachments, references });
  };

  const getEditorState = useCallback(() => ({
    threads,
    allEntries,
    objects: [],
  }), [allEntries, threads]);

  const handleScroll = useCallback(() => {
    const node = listRef.current;
    if (!node || threadPageLoading || !entriesPage.hasMore || !selectedThreadID) {
      return;
    }
    const remaining = node.scrollHeight - node.scrollTop - node.clientHeight;
    if (remaining < 240) {
      void workbench.loadMoreThread(selectedThreadID);
    }
  }, [entriesPage.hasMore, selectedThreadID, threadPageLoading, workbench]);

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

  const submitTitle = useCallback(async (value = draftTitle) => {
    if (renamingRef.current) {
      return;
    }

    const nextTitle = String(value ?? "").trim();
    const currentTitle = String(thread?.title ?? "").trim();
    if (!nextTitle) {
      setDraftTitle(currentTitle || draftTitle);
      return;
    }
    if (!selectedThreadID || nextTitle === currentTitle) {
      setIsEditingTitle(false);
      return;
    }

    renamingRef.current = true;
    try {
      await workbench.updateThreadTitle({ threadID: selectedThreadID, title: nextTitle });
      setDraftTitle(nextTitle);
      setIsEditingTitle(false);
      showToast("Thread renamed", "success");
    } catch (error) {
      setDraftTitle(nextTitle);
      showToast("Failed to rename thread", "error");
    } finally {
      renamingRef.current = false;
    }
  }, [draftTitle, selectedThreadID, thread?.title, workbench]);

  if (!selectedThreadID) {
    return (
      <div className="flex flex-col items-center justify-center h-full text-text-tertiary">
        <p className="text-sm">No thread selected.</p>
      </div>
    );
  }

  const color = THREAD_COLORS[thread?.color] ?? THREAD_COLORS.sky;
  const showLoadingState = !threadDetail && threadLoading;

  return (
    <div className="flex flex-col h-full">
      <div className="h-1 w-full shrink-0" style={{ backgroundColor: color }} />

      <div className="shrink-0">
        <div className="max-w-2xl mx-auto px-6 py-3 flex items-center gap-3">
          <div className="min-w-0 flex-1 flex items-center gap-2">
            {isEditingTitle ? (
              <input
                ref={titleInputRef}
                type="text"
                value={draftTitle}
                onChange={(event) => setDraftTitle(event.target.value)}
                onBlur={(event) => { void submitTitle(event.target.value); }}
                onKeyDown={(event) => {
                  if (event.key === "Enter") {
                    event.preventDefault();
                    event.stopPropagation();
                    void submitTitle(event.currentTarget.value);
                  }
                  if (event.key === "Escape") {
                    event.preventDefault();
                    event.stopPropagation();
                    setDraftTitle(thread?.title ?? "");
                    setIsEditingTitle(false);
                  }
                }}
                className="soft-focus-field min-w-0 flex-1 rounded-md px-2 py-1 text-lg font-semibold text-text"
                aria-label="Thread title"
              />
            ) : (
              <button
                type="button"
                onClick={() => setIsEditingTitle(true)}
                className="min-w-0 truncate text-left text-lg font-semibold text-text"
                title="Rename thread"
              >
                {thread?.title ?? "Thread"}
              </button>
            )}
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
                restoreFocusOnSubmit
              />
            )}
          </section>

          <section>
            <div className="mb-2 flex items-center justify-between gap-3">
              <div>
                <h2 className="text-sm font-semibold text-text">Working Stream</h2>
                <p className="text-xs text-text-tertiary">
                  {sortedEntries.length === 0 ? "No entries yet." : `${sortedEntries.length} entries`}
                  {entriesPage.totalCount > sortedEntries.length ? ` · ${entriesPage.totalCount} total` : ""}
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
                allEntries={allEntries}
                threads={threads}
                actions={actions}
                showThread={false}
                highlightedEntryID={highlightedEntryID}
                scrollContainerRef={listRef}
                onScrollFrame={handleScroll}
                footer={entriesPage.hasMore || threadPageLoading ? (
                  <div className="px-4 py-4 text-center text-xs text-text-tertiary">
                    {threadPageLoading ? "Loading more…" : "Scroll for older entries"}
                  </div>
                ) : null}
              />
            )}
          </section>
        </div>
      </div>
    </div>
  );
}
