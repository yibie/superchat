import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { THREAD_COLORS } from "../../lib/constants.js";

/**
 * Clickable thread association chip with color dot + title.
 */
export function ThreadBadge({ thread }) {
  const { openThread } = useNavigationContext();

  if (!thread) return null;

  const dotColor = THREAD_COLORS[thread.color] ?? "var(--color-text-tertiary)";

  return (
    <button
      type="button"
      onClick={(e) => {
        e.stopPropagation();
        openThread(thread.id);
      }}
      className="inline-flex items-center gap-1.5 text-xs text-text-secondary hover:text-text px-2 py-0.5 rounded-full transition-colors hover:bg-black/5 dark:hover:bg-white/10 cursor-pointer"
    >
      <span
        className="w-2 h-2 rounded-full shrink-0"
        style={{ backgroundColor: dotColor }}
      />
      <span className="truncate max-w-[10rem]">{thread.title}</span>
    </button>
  );
}
