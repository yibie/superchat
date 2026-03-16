import { useEffect, useRef } from "react";
import { cn } from "../../lib/cn.js";
import { IconButton } from "../shared/IconButton.jsx";

export function ModalOverlay({ open, onClose, title, children, width = "max-w-md" }) {
  const panelRef = useRef(null);

  useEffect(() => {
    if (!open) return;
    const onKey = (e) => {
      if (e.key === "Escape") onClose();
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open, onClose]);

  useEffect(() => {
    if (!open) return;
    const el = panelRef.current;
    if (!el) return;
    const focusable = el.querySelector(
      'input, select, textarea, button, [tabindex]:not([tabindex="-1"])'
    );
    focusable?.focus();
  }, [open]);

  if (!open) return null;

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center"
      onClick={onClose}
    >
      <div className="absolute inset-0 bg-black/50" />
      <div
        ref={panelRef}
        role="dialog"
        aria-modal="true"
        aria-label={title}
        onClick={(e) => e.stopPropagation()}
        className={cn(
          "relative z-10 w-full mx-4 rounded-xl",
          "bg-surface border border-border shadow-xl",
          width
        )}
      >
        <div className="flex items-center justify-between px-4 py-3 border-b border-border">
          <h3 className="text-sm font-semibold text-text">{title}</h3>
          <IconButton
            label="Close"
            icon={<span>&#x2715;</span>}
            onClick={onClose}
          />
        </div>
        <div className="p-4">{children}</div>
      </div>
    </div>
  );
}
