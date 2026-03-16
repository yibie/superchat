import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { ThreadInspector } from "../thread/ThreadInspector.jsx";
import { StreamInspector } from "../stream/StreamInspector.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { SURFACES } from "../../lib/constants.js";

export function InspectorPanel() {
  const { inspectorOpen, setInspectorOpen, selectedThreadID, surface } = useNavigationContext();

  if (!inspectorOpen) return null;

  let content = null;
  if (surface === SURFACES.THREAD && selectedThreadID) {
    content = <ThreadInspector threadID={selectedThreadID} />;
  } else if (surface === SURFACES.STREAM) {
    content = <StreamInspector />;
  } else {
    return null;
  }

  return (
    <aside
      className="flex flex-col h-full bg-surface border-l border-border overflow-hidden"
      aria-label="Inspector"
    >
      <div className="flex items-center justify-end px-3 h-10 shrink-0">
        <IconButton
          label="Close inspector"
          icon={"\u2715"}
          onClick={() => setInspectorOpen(false)}
        />
      </div>
      <div className="flex-1 overflow-y-auto">{content}</div>
    </aside>
  );
}
