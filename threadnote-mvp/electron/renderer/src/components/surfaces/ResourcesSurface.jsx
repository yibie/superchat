import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";
import { ResourceCollection } from "../resources/ResourceCollection.jsx";

export function ResourcesSurface() {
  const { home } = useWorkbenchContext();
  const resources = home?.resources ?? [];

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
