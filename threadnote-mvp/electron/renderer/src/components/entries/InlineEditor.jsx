import { useState, useRef, useEffect, useCallback } from "react";

/**
 * Inline edit mode for an entry.
 * Pre-fills textarea with the entry body text. Save + Cancel buttons.
 */
export function InlineEditor({ entry, onSave, onCancel }) {
  const [text, setText] = useState(entry?.body?.text || entry?.summaryText || "");
  const textareaRef = useRef(null);

  const autoResize = useCallback(() => {
    const el = textareaRef.current;
    if (!el) return;
    el.style.height = "auto";
    el.style.height = el.scrollHeight + "px";
  }, []);

  useEffect(() => {
    const el = textareaRef.current;
    if (el) {
      autoResize();
      el.focus();
      el.setSelectionRange(el.value.length, el.value.length);
    }
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

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
        onChange={(e) => { setText(e.target.value); autoResize(); }}
        onKeyDown={handleKeyDown}
        rows={2}
        className="w-full rounded-md border border-border bg-surface px-3 py-2 text-sm text-text focus:outline-accent resize-y overflow-hidden"
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
