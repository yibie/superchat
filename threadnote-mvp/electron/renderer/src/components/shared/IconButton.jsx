import { cn } from "../../lib/cn.js";

export function IconButton({ label, icon, onClick, className, size = "sm", variant = "ghost", ...props }) {
  return (
    <button
      onClick={onClick}
      aria-label={label}
      className={cn(
        "inline-flex items-center justify-center rounded-md transition-colors cursor-pointer",
        "focus-visible:outline-accent",
        size === "sm" && "w-7 h-7 text-xs",
        size === "md" && "w-8 h-8 text-sm",
        variant === "ghost" && "text-text-tertiary hover:text-text hover:bg-elevated",
        variant === "danger" && "text-text-tertiary hover:text-danger hover:bg-danger-muted",
        className
      )}
      {...props}
    >
      {icon}
    </button>
  );
}
