import { cn } from "../../lib/cn.js";
import { THREAD_COLORS } from "../../lib/constants.js";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { IconButton } from "../shared/IconButton.jsx";

export function ThreadHeader({ thread }) {
  const { goBack } = useNavigationContext();

  if (!thread) return null;

  const color = THREAD_COLORS[thread.color] ?? THREAD_COLORS.sky;
  const goal = thread.goalLayer;

  return (
    <header className="shrink-0">
      {/* color stripe */}
      <div className="h-1 w-full" style={{ backgroundColor: color }} />

      <div className="flex items-start gap-3 px-4 py-3">
        <IconButton
          label="Back"
          icon={"\u2190"}
          onClick={goBack}
          className="mt-0.5 shrink-0"
        />

        <div className="min-w-0 flex-1">
          <h1 className="text-lg font-semibold text-text truncate">
            {thread.title ?? "Untitled thread"}
          </h1>

          {goal?.goalStatement && (
            <p className="mt-0.5 text-sm text-text-secondary line-clamp-2">
              {goal.goalStatement}
            </p>
          )}

          <div className="mt-2 flex flex-wrap items-center gap-1.5">
            {goal?.currentStage && (
              <span
                className="inline-flex items-center rounded-full px-2 py-0.5 text-[11px] font-medium bg-elevated text-text-secondary"
              >
                {goal.currentStage}
              </span>
            )}
            {goal?.goalType && (
              <span
                className="inline-flex items-center rounded-full px-2 py-0.5 text-[11px] font-medium border border-border text-text-tertiary"
              >
                {goal.goalType}
              </span>
            )}
          </div>
        </div>
      </div>
    </header>
  );
}
