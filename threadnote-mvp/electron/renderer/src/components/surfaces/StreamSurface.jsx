import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { EntryList } from "../entries/EntryList.jsx";

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

  const entries = useMemo(() => {
    const raw = home?.inboxEntries ?? [];
    return [...raw].sort(
      (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
    );
  }, [home?.inboxEntries]);

  const threads = home?.threads ?? [];

  const handleSubmitCapture = useCallback(
    (text, attachments, references) => workbench.submitCapture({ text, attachments, references }),
    [workbench]
  );

  const getEditorState = useCallback(() => ({
    threads: home?.threads ?? [],
    allEntries: home?.allEntries ?? home?.inboxEntries ?? [],
    objects: [],
  }), [home?.threads, home?.allEntries, home?.inboxEntries]);

  useEffect(() => {
    if (!focusedEntryTarget?.entryID || focusedEntryTarget.threadID) {
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
        <div className="max-w-2xl mx-auto px-6 pt-4 pb-1">
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
        <div className="max-w-2xl mx-auto px-2">
          <EntryList
            entries={entries}
            allEntries={home?.allEntries ?? []}
            threads={threads}
            actions={actions}
            showThread
            highlightedEntryID={highlightedEntryID}
          />
        </div>
      </div>
    </div>
  );
}
