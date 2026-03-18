import { useRef } from "react";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";

/**
 * Inline reply input backed by the shared capture editor runtime.
 */
export function ReplyComposer({ entryID, onSubmit, onCancel, getEditorState }) {
  const runtimeRef = useRef(null);

  return (
    <div className="mt-2 ml-6 space-y-2" onKeyDown={(event) => {
      if (event.key === "Escape") {
        event.preventDefault();
        onCancel();
      }
    }}>
      <CaptureEditor
        onSubmit={(text, attachments, references) => onSubmit(entryID, text.trim(), attachments ?? [], references ?? [])}
        placeholder="Write a reply with #tags, @objects, [[references]], or attachments…"
        submitLabel="Reply"
        minHeight={88}
        variant="panel"
        getEditorState={getEditorState}
        onReady={(runtime) => {
          runtimeRef.current = runtime;
          runtime?.focus();
        }}
      />
      <div className="flex items-center gap-2">
        <button
          type="button"
          onClick={() => runtimeRef.current?.submit()}
          className="px-3 py-1 text-xs font-medium rounded-md bg-accent text-white hover:bg-accent/90 disabled:opacity-40 transition-colors"
        >
          Reply
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
