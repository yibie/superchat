import { useEffect, useRef, useState } from "react";
import { KIND_COLORS, KIND_LABELS, KIND_OPTIONS } from "../../lib/constants.js";

/**
 * Compact badge: colored dot + bold label.
 */
export function KindBadge({ kind, interactive = false, onSelect = null }) {
  const color = KIND_COLORS[kind] ?? KIND_COLORS.note;
  const label = KIND_LABELS[kind] ?? kind;
  const isNote = kind === "note";
  const [open, setOpen] = useState(false);
  const rootRef = useRef(null);

  useEffect(() => {
    if (!open) {
      return undefined;
    }
    const handlePointerDown = (event) => {
      if (!rootRef.current?.contains(event.target)) {
        setOpen(false);
      }
    };
    window.addEventListener("pointerdown", handlePointerDown);
    return () => window.removeEventListener("pointerdown", handlePointerDown);
  }, [open]);

  const badge = (
    <span
      className="inline-flex items-center gap-1 shrink-0 font-semibold px-1.5 py-0.5 rounded"
      style={{
        color,
        backgroundColor: `color-mix(in srgb, ${color} ${isNote ? 9 : 15}%, transparent)`,
        fontSize: 11,
        opacity: isNote ? 0.82 : 1
      }}
    >
      {label}
    </span>
  );

  if (!interactive || typeof onSelect !== "function") {
    return badge;
  }

  return (
    <div ref={rootRef} className="relative inline-flex">
      <button
        type="button"
        aria-haspopup="menu"
        aria-expanded={open ? "true" : "false"}
        onClick={() => setOpen((value) => !value)}
        className="cursor-pointer"
      >
        {badge}
      </button>
      {open && (
        <div
          role="menu"
          className="absolute left-0 top-full z-20 mt-1 min-w-36 rounded-lg border border-border bg-surface p-1 shadow-lg"
        >
          {KIND_OPTIONS.map((option) => (
            <button
              key={option}
              type="button"
              role="menuitemradio"
              aria-checked={option === kind ? "true" : "false"}
              onClick={() => {
                onSelect(option);
                setOpen(false);
              }}
              className="flex w-full items-center justify-between rounded-md px-2 py-1.5 text-left text-xs text-text hover:bg-elevated"
            >
              <span>{KIND_LABELS[option] ?? option}</span>
              {option === kind ? <span className="text-text-tertiary">Current</span> : null}
            </button>
          ))}
        </div>
      )}
    </div>
  );
}
