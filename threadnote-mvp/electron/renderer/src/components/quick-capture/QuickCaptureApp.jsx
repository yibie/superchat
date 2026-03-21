import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { ipc } from "../../lib/ipc.js";
import { cn } from "../../lib/cn.js";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";

export function QuickCaptureApp() {
  const workbench = useWorkbenchContext();
  const runtimeRef = useRef(null);
  const [editorState, setEditorState] = useState({ text: "", attachments: [], canSubmit: false });
  const [incomingDraft, setIncomingDraft] = useState(null);
  const [draftSource, setDraftSource] = useState("quickCaptureHotkey");
  const [draftSourceContext, setDraftSourceContext] = useState({});
  const [feedback, setFeedback] = useState("");
  const [submitting, setSubmitting] = useState(false);

  const referenceState = useMemo(() => ({
    threads: workbench.home?.threads ?? [],
    allEntries: workbench.home?.allEntries ?? workbench.home?.inboxEntries ?? [],
    objects: []
  }), [workbench.home?.allEntries, workbench.home?.inboxEntries, workbench.home?.threads]);

  const handleCancel = useCallback(() => {
    setFeedback("");
    ipc.closeQuickCapture();
  }, []);

  useEffect(() => {
    const unsubscribeHydrate = ipc.onQuickCaptureHydrate((payload) => {
      setIncomingDraft({
        text: payload?.text ?? "",
        attachments: payload?.attachments ?? []
      });
      setDraftSource(payload?.source ?? "quickCaptureHotkey");
      setDraftSourceContext(payload?.sourceContext ?? {});
      queueMicrotask(() => runtimeRef.current?.focus());
    });
    return () => {
      unsubscribeHydrate?.();
    };
  }, [handleCancel]);

  useEffect(() => {
    function handleWindowKeydown(event) {
      if (event.defaultPrevented || submitting) {
        return;
      }
      if (event.key !== "Escape") {
        return;
      }
      event.preventDefault();
      handleCancel();
    }

    window.addEventListener("keydown", handleWindowKeydown);
    return () => window.removeEventListener("keydown", handleWindowKeydown);
  }, [handleCancel, submitting]);

  const handleSubmit = useCallback(async (text, attachments) => {
    setSubmitting(true);
    setFeedback("");
    try {
      await ipc.submitQuickCapture({
        text,
        attachments,
        references: [],
        source: draftSource,
        sourceContext: {
          ...(draftSourceContext ?? {}),
          attachmentCount: attachments?.length ?? 0
        }
      });
      runtimeRef.current?.clear?.();
      setIncomingDraft(null);
      setDraftSource("quickCaptureHotkey");
      setDraftSourceContext({});
      handleCancel();
    } catch (error) {
      setFeedback(error.message);
    } finally {
      setSubmitting(false);
    }
  }, [draftSource, draftSourceContext, handleCancel]);

  const attachmentCount = editorState.attachments?.length ?? 0;
  const lineCount = String(editorState.text ?? "").split(/\r?\n/).filter(Boolean).length;
  const isExpanded = attachmentCount > 1 || lineCount > 3 || String(editorState.text ?? "").trim().length > 180;

  useEffect(() => {
    const width = isExpanded ? 820 : 720;
    const height = feedback ? (isExpanded ? 296 : 132) : (isExpanded ? 260 : 92);
    void ipc.resizeQuickCapture({ width, height });
  }, [feedback, isExpanded]);

  if (!workbench.workspace) {
    return (
      <div className="quick-capture-window min-h-screen text-text flex items-center justify-center px-6">
        <div className="quick-capture-bar max-w-sm px-6 py-7 text-center">
          <h1 className="text-xl font-semibold">Quick Capture needs a workspace</h1>
          <p className="mt-2 text-sm text-text-secondary">
            Open the main Threadnote window and create or select a workspace before capturing from outside the app.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="quick-capture-window min-h-screen text-text">
      <div className="quick-capture-shell">
        <div
          className={cn(
            "quick-capture-bar drag-region w-full h-full",
            isExpanded ? "quick-capture-card-expanded" : "quick-capture-card-compact"
          )}
        >
          <div className="quick-capture-bar-body no-drag">
            <CaptureEditor
              onSubmit={handleSubmit}
              placeholder="Capture anything"
              submitLabel="Send to Inbox"
              minHeight={isExpanded ? 220 : 68}
              getEditorState={() => referenceState}
              variant="panel"
              submitPlacement="hidden"
              incomingDraft={incomingDraft}
              incomingDraftMode="append"
              onStateChange={setEditorState}
              onReady={(runtime) => {
                runtimeRef.current = runtime;
                if (runtime) {
                  queueMicrotask(() => runtime.focus());
                }
              }}
              onAttachmentAccepted={({ source }) => {
                if (source === "finderDrop") {
                  setDraftSource("finderDrop");
                }
              }}
            />
          </div>

          {feedback ? (
            <p className="quick-capture-feedback border border-warning/30 bg-warning/10 px-3 py-2 text-sm text-warning">
              {feedback}
            </p>
          ) : null}
        </div>
      </div>
    </div>
  );
}
