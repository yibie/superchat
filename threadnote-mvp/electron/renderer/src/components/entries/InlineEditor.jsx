import { useMemo, useRef } from "react";
import { CaptureEditor } from "../editor/CaptureEditor.jsx";

/**
 * Inline edit mode for an entry using the shared capture editor runtime.
 */
export function InlineEditor({ entry, onSave, onCancel, getEditorState }) {
  const runtimeRef = useRef(null);
  const incomingDraft = useMemo(() => ({
    text: entry?.body?.text || entry?.summaryText || "",
    attachments: Array.isArray(entry?.body?.attachments) ? entry.body.attachments : []
  }), [entry]);

  return (
    <div className="space-y-2" onKeyDown={(event) => {
      if (event.key === "Escape") {
        event.preventDefault();
        onCancel();
      }
    }}>
      <CaptureEditor
        onSubmit={(text, attachments, references) => onSave(entry.id, text.trim(), attachments ?? [], references ?? [])}
        placeholder="#role @object [[reference]] or [[supports|reference]]"
        submitLabel="Save"
        minHeight={96}
        variant="panel"
        incomingDraft={incomingDraft}
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
          Save
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
