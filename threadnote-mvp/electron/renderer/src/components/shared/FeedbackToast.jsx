import { useState, useEffect, useCallback } from "react";
import { cn } from "../../lib/cn.js";

let toastSetter = null;

export function showToast(message, variant = "default") {
  toastSetter?.({ message, variant, key: Date.now() });
}

export function FeedbackToast() {
  const [toast, setToast] = useState(null);
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    toastSetter = setToast;
    return () => { toastSetter = null; };
  }, []);

  useEffect(() => {
    if (!toast) return;
    setVisible(true);
    const timer = setTimeout(() => setVisible(false), 2400);
    return () => clearTimeout(timer);
  }, [toast?.key]);

  if (!toast || !visible) return null;

  return (
    <div
      role="status"
      aria-live="polite"
      className={cn(
        "fixed bottom-5 left-1/2 -translate-x-1/2 z-50",
        "px-4 py-2 rounded-lg text-sm font-medium shadow-lg",
        "animate-[fadeIn_0.15s_ease-out]",
        toast.variant === "error" && "bg-danger text-white",
        toast.variant === "success" && "bg-success text-white",
        toast.variant === "default" && "bg-elevated text-text border border-border"
      )}
    >
      {toast.message}
    </div>
  );
}
