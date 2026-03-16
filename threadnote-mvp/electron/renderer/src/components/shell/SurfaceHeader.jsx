import { cn } from "../../lib/cn.js";

export function SurfaceHeader({ title, count, children, className }) {
  return (
    <div className="shrink-0 h-12 border-b border-border">
      <div className={cn("h-full flex items-center px-6 gap-2", className)}>
        <h1 className="text-base font-semibold text-text truncate">{title}</h1>
        {count != null && (
          <span className="text-xs text-text-tertiary tabular-nums">({count})</span>
        )}
        {children}
      </div>
    </div>
  );
}
