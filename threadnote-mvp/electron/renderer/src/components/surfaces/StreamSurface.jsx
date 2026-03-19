import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { EntryList } from "../entries/EntryList.jsx";

function emptyStreamPage() {
  return { items: [], replies: [], hasMore: false, nextCursor: null, totalCount: 0 };
}

function deriveLegacyStreamPage(home) {
  const allEntries = Array.isArray(home?.allEntries) ? home.allEntries : [];
  const inboxEntries = Array.isArray(home?.inboxEntries) ? home.inboxEntries : [];
  const source = allEntries.length > 0 ? allEntries : inboxEntries;
  return {
    ...emptyStreamPage(),
    items: source.filter(Boolean),
    replies: [],
    totalCount: source.filter(Boolean).length
  };
}

/**
 * Stream timeline surface.
 * Shows ALL entries (entries routed to threads still appear here).
 */
export function StreamSurface() {
  const { focusedEntryTarget, clearFocusedEntry } = useNavigationContext();
  const workbench = useWorkbenchContext();
  const actions = useEntryActions();
  const home = workbench.home;
  const listRef = useRef(null);
  const [highlightedEntryID, setHighlightedEntryID] = useState(null);
  const streamPage = home?.streamPage ?? deriveLegacyStreamPage(home);

  const entries = useMemo(() => {
    const raw = streamPage.items ?? [];
    return [...raw].sort(
      (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
    );
  }, [streamPage.items]);

  const threads = home?.threads ?? [];

  const handleSubmitCapture = useCallback(
    (text, attachments, references) => workbench.submitCapture({ text, attachments, references }),
    [workbench]
  );

  const getEditorState = useCallback(() => ({
    threads: home?.threads ?? [],
    allEntries: streamPage.items ?? [],
    objects: [],
  }), [home?.threads, streamPage.items]);

  const handleScroll = useCallback(() => {
    const node = listRef.current;
    if (!node || workbench.streamLoadingMore || !streamPage.hasMore) {
      return;
    }
    const remaining = node.scrollHeight - node.scrollTop - node.clientHeight;
    if (remaining < 240) {
      void workbench.loadMoreStream();
    }
  }, [streamPage.hasMore, workbench]);

  useEffect(() => {
    if (!focusedEntryTarget?.entryID || focusedEntryTarget.threadID) {
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
  }, [clearFocusedEntry, focusedEntryTarget, entries.length]);

  useEffect(() => {
    if (!highlightedEntryID) {
      return undefined;
    }
    const timer = window.setTimeout(() => setHighlightedEntryID(null), 1800);
    return () => window.clearTimeout(timer);
  }, [highlightedEntryID]);

  return (
    <div className="flex flex-col h-full">
      <div>
        <div className="max-w-2xl mx-auto px-4 pt-4 pb-1">
          <CaptureEditor
            onSubmit={handleSubmitCapture}
            placeholder="#role @object [[reference]] or [[supports|reference]]"
            submitLabel="Send"
            minHeight={120}
            getEditorState={getEditorState}
          />
        </div>
      </div>

      <div className="flex-1 overflow-y-auto" ref={listRef}>
        <div className="max-w-2xl mx-auto px-4">
          <EntryList
            entries={entries}
            allEntries={streamPage.items ?? []}
            threads={threads}
            actions={actions}
            showThread
            highlightedEntryID={highlightedEntryID}
            scrollContainerRef={listRef}
            onScrollFrame={handleScroll}
            footer={streamPage.hasMore || workbench.streamLoadingMore ? (
              <div className="px-4 py-4 text-center text-xs text-text-tertiary">
                {workbench.streamLoadingMore ? "Loading more…" : "Scroll for older entries"}
              </div>
            ) : null}
          />
        </div>
      </div>
    </div>
  );
}
