import { useEffect } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { EntryList } from "../entries/EntryList.jsx";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { THREAD_COLORS } from "../../lib/constants.js";

export function ThreadSurface() {
  const { selectedThreadID, goBack } = useNavigationContext();
  const workbench = useWorkbenchContext();
  const actions = useEntryActions();

  const threadDetail = workbench.thread;
  const thread = threadDetail?.thread ?? threadDetail;
  const entries = threadDetail?.entries ?? [];
  const threads = workbench.home?.threads ?? [];

  // Load thread detail when selectedThreadID changes
  useEffect(() => {
    if (selectedThreadID) {
      workbench.openThread(selectedThreadID);
    }
  }, [selectedThreadID]); // eslint-disable-line react-hooks/exhaustive-deps

  const sortedEntries = [...entries].sort(
    (a, b) => new Date(a.createdAt) - new Date(b.createdAt)
  );

  const handleSubmit = async (text) => {
    await workbench.submitCapture({ text, threadID: selectedThreadID });
  };

  if (!selectedThreadID) {
    return (
      <div className="flex flex-col items-center justify-center h-full text-text-tertiary">
        <p className="text-sm">No thread selected.</p>
      </div>
    );
  }

  const color = THREAD_COLORS[thread?.color] ?? THREAD_COLORS.sky;

  return (
    <div className="flex flex-col h-full">
      <div className="h-1 w-full shrink-0" style={{ backgroundColor: color }} />
      <SurfaceHeader title={thread?.title ?? "Thread"} className="max-w-2xl mx-auto">
        <IconButton
          label="Back"
          icon={"\u2190"}
          onClick={goBack}
          className="ml-auto shrink-0"
        />
      </SurfaceHeader>

      {/* Entry list - scrollable area */}
      <div className="flex-1 overflow-y-auto">
        <div className="max-w-2xl mx-auto px-2 py-3">
          {sortedEntries.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-16 text-text-tertiary">
              <p className="text-sm">No entries yet.</p>
              <p className="text-xs mt-1">Start capturing below.</p>
            </div>
          ) : (
            <EntryList entries={sortedEntries} threads={threads} actions={actions} showThread={false} />
          )}
        </div>
      </div>

      {/* Capture editor pinned to bottom */}
      <div className="shrink-0 border-t border-border">
        <div className="max-w-2xl mx-auto px-6 py-3">
          <CaptureEditor onSubmit={handleSubmit} threadID={selectedThreadID} />
        </div>
      </div>
    </div>
  );
}
