import { cn } from "../../lib/cn.js";

export function Field({ label, hint, error, children, className }) {
  return (
    <label className={cn("flex flex-col gap-1", className)}>
      {label && <span className="text-sm font-medium text-text">{label}</span>}
      {children}
      {hint && !error && <span className="text-xs text-text-tertiary">{hint}</span>}
      {error && <span className="text-xs text-danger">{error}</span>}
    </label>
  );
}

export function Input({ className, ...props }) {
  return (
    <input
      className={cn(
        "w-full px-3 py-2 text-sm bg-surface border border-border rounded-md",
        "text-text placeholder:text-text-tertiary",
        "focus:outline-none focus:border-accent transition-colors",
        className
      )}
      {...props}
    />
  );
}

export function Select({ className, children, ...props }) {
  return (
    <select
      className={cn(
        "w-full px-3 py-2 text-sm bg-surface border border-border rounded-md",
        "text-text focus:outline-none focus:border-accent transition-colors",
        "appearance-none cursor-pointer",
        className
      )}
      {...props}
    >
      {children}
    </select>
  );
}

export function TextArea({ className, ...props }) {
  return (
    <textarea
      className={cn(
        "w-full px-3 py-2 text-sm bg-surface border border-border rounded-md",
        "text-text placeholder:text-text-tertiary resize-none",
        "focus:outline-none focus:border-accent transition-colors",
        className
      )}
      {...props}
    />
  );
}
