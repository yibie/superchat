import { useCallback, useMemo } from "react";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { EntryList } from "../entries/EntryList.jsx";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";

/**
 * Stream timeline surface.
 * Shows ALL entries (entries routed to threads still appear here).
 */
export function StreamSurface() {
  const workbench = useWorkbenchContext();
  const actions = useEntryActions();
  const home = workbench.home;

  const entries = useMemo(() => {
    const raw = home?.inboxEntries ?? [];
    return [...raw].sort(
      (a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
    );
  }, [home?.inboxEntries]);

  const threads = home?.threads ?? [];
  const entryCount = entries.length;

  const handleSubmitCapture = useCallback(
    (text) => workbench.submitCapture({ text }),
    [workbench]
  );

  const getEditorState = useCallback(() => ({
    threads: home?.threads ?? [],
    entries: home?.inboxEntries ?? [],
    objects: [],
  }), [home?.threads, home?.inboxEntries]);

  return (
    <div className="flex flex-col h-full">
      {/* Title bar */}
      <SurfaceHeader title="Stream" count={entryCount} className="max-w-2xl mx-auto" />

      {/* Capture editor */}
      <div className="border-b border-border">
        <div className="max-w-2xl mx-auto px-6 pt-3 pb-1">
          <CaptureEditor
            onSubmit={handleSubmitCapture}
            placeholder="#role @object [[reference]]"
            submitLabel="Capture"
            minHeight={80}
            getEditorState={getEditorState}
          />
        </div>
      </div>

      {/* Entry list */}
      <div className="flex-1 overflow-y-auto">
        <div className="max-w-2xl mx-auto px-2">
          <EntryList
            entries={entries}
            threads={threads}
            actions={actions}
            showThread
          />
        </div>
      </div>
    </div>
  );
}
