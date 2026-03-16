import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";

export function WorkspaceGate() {
  const { createWorkspace, openWorkspace, loading } = useWorkbenchContext();

  return (
    <div className="flex items-center justify-center h-full bg-bg">
      <div className="flex flex-col items-center gap-6 max-w-sm text-center">
        <div className="flex flex-col gap-1">
          <h1 className="text-2xl font-semibold text-text tracking-tight">Threadnote</h1>
          <p className="text-sm text-text-secondary">Structured thinking workspace</p>
        </div>

        <div className="flex flex-col gap-2.5 w-64">
          <button
            onClick={createWorkspace}
            disabled={loading}
            className="w-full py-2.5 px-4 text-sm font-medium bg-accent text-white rounded-lg hover:bg-accent-hover transition-colors disabled:opacity-50"
          >
            Create workspace
          </button>
          <button
            onClick={openWorkspace}
            disabled={loading}
            className="w-full py-2.5 px-4 text-sm font-medium bg-surface border border-border text-text rounded-lg hover:bg-elevated transition-colors disabled:opacity-50"
          >
            Open existing
          </button>
        </div>

        <p className="text-xs text-text-tertiary">
          A workspace is a folder that stores your threads, entries, and AI config.
        </p>
      </div>
    </div>
  );
}
