import { useRef } from "react";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useEntryActions } from "../../hooks/useEntryActions.js";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";
import { ResourceCollection } from "../resources/ResourceCollection.jsx";
import { entriesForMention } from "../resources/resourceViewModel.js";
import { EntryList } from "../entries/EntryList.jsx";

export function ResourcesSurface() {
  const { home } = useWorkbenchContext();
  const { selectedMentionName, goToResources } = useNavigationContext();
  const actions = useEntryActions();
  const listRef = useRef(null);
  const resources = home?.resources ?? [];
  const threads = home?.threads ?? [];

  if (selectedMentionName) {
    const entries = entriesForMention(resources, selectedMentionName);

    return (
      <div className="flex h-full flex-col bg-bg">
        <SurfaceHeader title={`@${selectedMentionName}`} count={entries.length} className="mx-auto max-w-4xl">
          <button
            type="button"
            onClick={() => goToResources()}
            className="ml-auto inline-flex items-center rounded-md px-2 py-1 text-xs text-text-secondary transition-colors hover:bg-elevated hover:text-text"
          >
            Back
          </button>
        </SurfaceHeader>

        {entries.length === 0 ? (
          <div className="flex flex-1 items-center justify-center">
            <p className="text-sm text-text-tertiary">No notes mention @{selectedMentionName}</p>
          </div>
        ) : (
          <div className="flex-1 overflow-y-auto" ref={listRef}>
            <div className="mx-auto max-w-2xl px-4 py-6">
              <EntryList
                entries={entries}
                allEntries={entries}
                threads={threads}
                actions={actions}
                showThread
                scrollContainerRef={listRef}
              />
            </div>
          </div>
        )}
      </div>
    );
  }

  return (
    <div className="flex h-full flex-col bg-bg">
      <SurfaceHeader title="Resources" count={resources.length} className="mx-auto max-w-4xl" />

      {resources.length === 0 ? (
        <div className="flex flex-1 items-center justify-center">
          <p className="text-sm text-text-tertiary">No resources yet</p>
        </div>
      ) : (
        <div className="flex-1 overflow-y-auto">
          <div className="mx-auto max-w-4xl px-6 py-6">
            <ResourceCollection resources={resources} />
          </div>
        </div>
      )}
    </div>
  );
}
