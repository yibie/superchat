import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useThemeContext } from "../../contexts/ThemeContext.jsx";
import { SURFACES, THREAD_COLORS } from "../../lib/constants.js";
import { NavButton } from "../shared/NavButton.jsx";
import { cn } from "../../lib/cn.js";

export function Sidebar({ onNewThread }) {
  const { surface, goToStream, goToResources, goToSettings, openThread } = useNavigationContext();
  const { home } = useWorkbenchContext();
  const { preference, cycle } = useThemeContext();

  const threads = home?.threads ?? [];

  const themeIcon = preference === "dark" ? "\u263E" : preference === "light" ? "\u2600" : "\u25D1";

  return (
    <aside className="flex flex-col h-full bg-surface border-r border-border" aria-label="Sidebar">
      {/* Drag region */}
      <div className="drag-region h-10 shrink-0" />

      {/* Nav */}
      <nav className="flex flex-col gap-0.5 px-2 py-1" aria-label="Main navigation">
        <NavButton
          label="Stream"
          icon={"\u2302"}
          active={surface === SURFACES.STREAM}
          onClick={goToStream}
          shortcut={"\u2318 1"}
        />
        <NavButton
          label="Resources"
          icon={"\u2197"}
          active={surface === SURFACES.RESOURCES}
          onClick={goToResources}
          shortcut={"\u2318 2"}
        />
      </nav>

      {/* Divider */}
      <div className="h-px bg-border mx-3 my-2" />

      {/* Threads */}
      <div className="flex items-center justify-between px-3 mb-1">
        <span className="text-xs font-medium text-text-tertiary uppercase tracking-wider">Threads</span>
        {onNewThread && (
          <button
            onClick={onNewThread}
            aria-label="New thread"
            className="w-5 h-5 flex items-center justify-center rounded text-text-tertiary hover:text-text hover:bg-elevated transition-colors text-sm"
          >
            +
          </button>
        )}
      </div>

      <div className="flex-1 overflow-y-auto px-2 pb-2">
        {threads.length === 0 ? (
          <p className="px-2.5 py-4 text-xs text-text-tertiary text-center">No threads yet</p>
        ) : (
          <div className="flex flex-col gap-0.5">
            {threads.map((t) => (
              <button
                key={t.id}
                onClick={() => openThread(t.id)}
                className={cn(
                  "flex items-center gap-2 w-full px-2.5 py-1.5 rounded-md text-sm font-semibold text-left transition-colors",
                  "hover:bg-elevated",
                  surface === SURFACES.THREAD ? "bg-elevated text-text" : "text-text-secondary"
                )}
              >
                <span
                  className="w-2 h-2 rounded-full shrink-0"
                  style={{ background: THREAD_COLORS[t.color] ?? THREAD_COLORS.sky }}
                />
                <span className="truncate">{t.title}</span>
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Footer */}
      <div className="shrink-0 px-2 py-2 border-t border-border flex items-center justify-between">
        <button
          onClick={cycle}
          aria-label={`Theme: ${preference}`}
          className="w-8 h-8 flex items-center justify-center rounded-md text-xl text-text-tertiary hover:text-text hover:bg-elevated transition-colors"
        >
          {themeIcon}
        </button>
        <button
          onClick={goToSettings}
          aria-label="Settings"
          className={cn(
            "w-8 h-8 flex items-center justify-center rounded-md text-xl text-text-tertiary hover:text-text hover:bg-elevated transition-colors",
            surface === SURFACES.SETTINGS && "text-text bg-elevated"
          )}
        >
          {"\u2699"}
        </button>
      </div>
    </aside>
  );
}
