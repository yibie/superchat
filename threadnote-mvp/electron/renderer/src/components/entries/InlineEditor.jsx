import { useState, useRef, useEffect } from "react";

/**
 * Inline edit mode for an entry.
 * Pre-fills textarea with the entry body text. Save + Cancel buttons.
 */
export function InlineEditor({ entry, onSave, onCancel }) {
  const [text, setText] = useState(entry?.body?.text ?? "");
  const textareaRef = useRef(null);

  useEffect(() => {
    const el = textareaRef.current;
    if (el) {
      el.focus();
      el.setSelectionRange(el.value.length, el.value.length);
    }
  }, []);

  const handleSave = () => {
    const trimmed = text.trim();
    if (!trimmed) return;
    onSave(entry.id, trimmed);
  };

  const handleKeyDown = (e) => {
    if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
      e.preventDefault();
      handleSave();
    }
    if (e.key === "Escape") {
      e.preventDefault();
      onCancel();
    }
  };

  return (
    <div className="space-y-2">
      <textarea
        ref={textareaRef}
        value={text}
        onChange={(e) => setText(e.target.value)}
        onKeyDown={handleKeyDown}
        rows={3}
        className="w-full rounded-md border border-border bg-surface px-3 py-2 text-sm text-text focus:outline-accent resize-y"
      />
      <div className="flex items-center gap-2">
        <button
          type="button"
          onClick={handleSave}
          disabled={!text.trim()}
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
