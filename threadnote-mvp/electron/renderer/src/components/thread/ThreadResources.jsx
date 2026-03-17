import { ResourceCollection } from "../resources/ResourceCollection.jsx";

export function ThreadResources({ resources }) {
  if (!resources?.length) {
    return (
      <div className="flex flex-col items-center justify-center py-10 text-text-tertiary">
        <p className="text-sm">No resources yet.</p>
        <p className="mt-1 text-xs">Add links, attachments, or mentions inside this thread.</p>
      </div>
    );
  }

  return <ResourceCollection resources={resources} compact />;
}
