import { cn } from "../../lib/cn.js";

export function NavButton({ label, icon, active, onClick, shortcut }) {
  return (
    <button
      onClick={onClick}
      aria-label={label}
      aria-current={active ? "page" : undefined}
      className={cn(
        "flex items-center gap-2 w-full px-2.5 py-1.5 rounded-md text-sm transition-colors",
        "hover:bg-elevated focus-visible:outline-accent",
        active ? "bg-elevated text-text font-medium" : "text-text-secondary"
      )}
    >
      {icon && <span className="w-4 h-4 flex items-center justify-center text-xs shrink-0">{icon}</span>}
      <span className="truncate">{label}</span>
      {shortcut && (
        <kbd className="ml-auto text-2xs text-text-tertiary font-mono">{shortcut}</kbd>
      )}
    </button>
  );
}
