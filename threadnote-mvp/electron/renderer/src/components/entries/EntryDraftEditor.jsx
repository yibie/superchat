import { useRef } from "react";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";

export function EntryDraftEditor({
  entryID,
  initialText = "",
  initialAttachments = [],
  onSubmit,
  onCancel,
  getEditorState,
  placeholder,
  submitLabel,
  minHeight = 96,
  className = ""
}) {
  const runtimeRef = useRef(null);
  const initialDraftRef = useRef({
    text: initialText,
    attachments: Array.isArray(initialAttachments) ? initialAttachments : []
  });

  return (
    <div
      className={className}
      onKeyDown={(event) => {
        if (event.key === "Escape") {
          event.preventDefault();
          onCancel();
        }
      }}
    >
      <CaptureEditor
        onSubmit={(text, attachments, references) => (
          onSubmit(entryID, text.trim(), attachments ?? [], references ?? [])
        )}
        placeholder={placeholder}
        submitLabel={submitLabel}
        minHeight={minHeight}
        variant="panel"
        incomingDraft={initialDraftRef.current}
        getEditorState={getEditorState}
        onReady={(runtime) => {
          runtimeRef.current = runtime;
          runtime?.focus();
        }}
      />
      <div className="mt-2 flex items-center gap-2">
        <button
          type="button"
          onClick={() => runtimeRef.current?.submit()}
          className="px-3 py-1 text-xs font-medium rounded-md bg-accent text-white hover:bg-accent/90 disabled:opacity-40 transition-colors"
        >
          {submitLabel}
        </button>
        <button
          type="button"
          onClick={onCancel}
          className="px-3 py-1 text-xs font-medium rounded-md text-text-secondary hover:text-text hover:bg-elevated transition-colors"
        >
          Cancel
        </button>
      </div>
    </div>
  );
}
