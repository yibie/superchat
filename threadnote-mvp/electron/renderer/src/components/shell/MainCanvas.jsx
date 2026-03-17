import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { SURFACES } from "../../lib/constants.js";
import { StreamSurface } from "../surfaces/StreamSurface.jsx";
import { ThreadSurface } from "../surfaces/ThreadSurface.jsx";
import { ResourcesSurface } from "../surfaces/ResourcesSurface.jsx";

const SURFACE_MAP = {
  [SURFACES.STREAM]: StreamSurface,
  [SURFACES.THREAD]: ThreadSurface,
  [SURFACES.RESOURCES]: ResourcesSurface,
};

export function MainCanvas() {
  const { surface } = useNavigationContext();
  const Surface = SURFACE_MAP[surface] ?? StreamSurface;

  return (
    <main className="flex flex-col h-full overflow-hidden bg-bg" role="main">
      <Surface />
    </main>
  );
}
