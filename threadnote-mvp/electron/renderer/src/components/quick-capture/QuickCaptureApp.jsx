import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";
import { ipc } from "../../lib/ipc.js";
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

  useEffect(() => {
    ipc.onQuickCaptureHydrate((payload) => {
      setIncomingDraft({
        text: payload?.text ?? "",
        attachments: payload?.attachments ?? []
      });
      setDraftSource(payload?.source ?? "quickCaptureHotkey");
      setDraftSourceContext(payload?.sourceContext ?? {});
      queueMicrotask(() => runtimeRef.current?.focus());
    });
    ipc.onQuickCaptureSubmitted(() => {
      setFeedback("");
    });
  }, []);

  const handleClipboardImport = useCallback(async () => {
    try {
      setFeedback("");
      const imported = await ipc.importFromClipboard();
      if (!imported || (!imported.text?.trim() && (imported.attachments?.length ?? 0) === 0)) {
        setFeedback("Clipboard does not contain supported text or image data.");
        return;
      }
      setIncomingDraft({
        text: imported.text ?? "",
        attachments: imported.attachments ?? []
      });
      setDraftSource(imported.source ?? "clipboardImport");
      setDraftSourceContext(imported.sourceContext ?? {});
    } catch (error) {
      setFeedback(error.message);
    }
  }, []);

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
      setDraftSource("quickCaptureHotkey");
      setDraftSourceContext({});
    } catch (error) {
      setFeedback(error.message);
    } finally {
      setSubmitting(false);
    }
  }, []);

  const handleCancel = useCallback(() => {
    setFeedback("");
    ipc.closeQuickCapture();
  }, []);

  if (!workbench.workspace) {
    return (
      <div className="min-h-screen bg-bg text-text flex items-center justify-center px-6">
        <div className="max-w-sm rounded-[28px] border border-border bg-surface px-6 py-7 shadow-lg text-center">
          <h1 className="text-xl font-semibold tracking-tight">Quick Capture needs a workspace</h1>
          <p className="mt-2 text-sm text-text-secondary">
            Open the main Threadnote window and create or select a workspace before capturing from outside the app.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-[radial-gradient(circle_at_top,_rgba(193,160,90,0.22),_transparent_45%),linear-gradient(180deg,_#f6f0e6_0%,_#efe7db_100%)] text-text">
      <div className="mx-auto flex min-h-screen max-w-2xl items-center px-4 py-5">
        <div className="w-full rounded-[28px] border border-border/80 bg-surface/95 p-4 shadow-[0_18px_50px_rgba(74,60,37,0.14)] backdrop-blur">
          <div className="mb-3 flex items-start justify-between gap-4">
            <div>
              <p className="text-[11px] uppercase tracking-[0.22em] text-text-tertiary">External Capture</p>
              <h1 className="mt-1 text-xl font-semibold tracking-tight">Send to Inbox</h1>
              <p className="mt-1 text-sm text-text-secondary">
                Paste text, import clipboard content, or drop Finder files here.
              </p>
            </div>
            <button
              type="button"
              onClick={handleCancel}
              className="rounded-full border border-border px-3 py-1.5 text-sm text-text-secondary hover:bg-surface-hover"
            >
              Cancel
            </button>
          </div>

          <CaptureEditor
            onSubmit={handleSubmit}
            placeholder="#role @object [[reference]] or paste a link, thought, or file"
            submitLabel="Send to Inbox"
            minHeight={180}
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

          <div className="mt-3 flex items-center justify-between gap-3">
            <button
              type="button"
              onClick={handleClipboardImport}
              disabled={submitting}
              className="rounded-full border border-border px-3.5 py-2 text-sm font-medium text-text hover:bg-surface-hover disabled:opacity-50"
            >
              Import Clipboard
            </button>
            <div className="flex items-center gap-2">
              <span className="text-xs text-text-tertiary">Cmd+Enter to send</span>
              <button
                type="button"
                onClick={() => runtimeRef.current?.submit()}
                disabled={!editorState.canSubmit || submitting}
                className="rounded-full bg-accent px-4 py-2 text-sm font-medium text-white shadow-sm hover:bg-accent-hover disabled:opacity-50"
              >
                {submitting ? "Sending..." : "Send to Inbox"}
              </button>
            </div>
          </div>

          {feedback ? (
            <p className="mt-3 rounded-2xl border border-amber-300/50 bg-amber-50 px-3 py-2 text-sm text-amber-900">
              {feedback}
            </p>
          ) : null}
        </div>
      </div>
    </div>
  );
}
