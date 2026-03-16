import { cn } from "../../lib/cn.js";

export function RestartNote({ restartNote, recoveryLines, openLoops, nextAction }) {
  const hasContent = restartNote || recoveryLines?.length || openLoops?.length || nextAction;
  if (!hasContent) return null;

  return (
    <div className="space-y-3">
      {/* Restart note card */}
      {restartNote && (
        <div className="border-l-3 border-accent bg-surface rounded-r-md px-3 py-2.5 shadow-sm">
          <span className="text-[11px] font-medium text-text-tertiary uppercase tracking-wider">
            Restart Note
          </span>
          <p className="mt-1 text-sm text-text leading-relaxed">{restartNote}</p>
        </div>
      )}

      {/* Recovery lines */}
      {recoveryLines?.length > 0 && (
        <div>
          <span className="text-[11px] font-medium text-text-tertiary uppercase tracking-wider">
            Recovery Lines
          </span>
          <ul className="mt-1.5 list-disc list-inside space-y-0.5">
            {recoveryLines.map((line, i) => (
              <li key={i} className="text-sm text-text-secondary">{line}</li>
            ))}
          </ul>
        </div>
      )}

      {/* Open loops */}
      {openLoops?.length > 0 && (
        <div>
          <span className="text-[11px] font-medium text-text-tertiary uppercase tracking-wider">
            Open Loops
          </span>
          <ul className="mt-1.5 space-y-1">
            {openLoops.map((loop, i) => (
              <li key={i} className="flex items-start gap-2 text-sm text-text-secondary">
                <span className="mt-0.5 h-4 w-4 shrink-0 rounded border border-border inline-flex items-center justify-center text-[10px] text-text-tertiary">
                  {""}
                </span>
                <span>{loop}</span>
              </li>
            ))}
          </ul>
        </div>
      )}

      {/* Next action */}
      {nextAction && (
        <div className="rounded-md bg-accent/10 border border-accent/20 px-3 py-2.5">
          <span className="text-[11px] font-medium text-accent uppercase tracking-wider">
            Next Action
          </span>
          <p className="mt-1 text-sm font-medium text-text">{nextAction}</p>
        </div>
      )}
    </div>
  );
}
