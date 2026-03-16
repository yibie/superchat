import { useEffect, useState, useCallback } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { Sidebar } from "./Sidebar.jsx";
import { MainCanvas } from "./MainCanvas.jsx";
import { InspectorPanel } from "./InspectorPanel.jsx";
import { FeedbackToast } from "../shared/FeedbackToast.jsx";
import { NewThreadModal } from "../modals/NewThreadModal.jsx";
import { initKeyboardShortcuts, registerShortcut, unregisterShortcut } from "../../lib/keyboard.js";
import { ipc } from "../../lib/ipc.js";

export function AppShell() {
  const { inspectorOpen, selectedThreadID, goToStream, goToResources, goToSettings, goBack, toggleInspector } = useNavigationContext();
  const showInspector = inspectorOpen && selectedThreadID;
  const [newThreadOpen, setNewThreadOpen] = useState(false);

  // Wire app menu "Settings" to navigate
  useEffect(() => {
    ipc.onOpenSettings(goToSettings);
  }, [goToSettings]);

  useEffect(() => {
    const cleanup = initKeyboardShortcuts();
    registerShortcut("mod+1", goToStream);
    registerShortcut("mod+2", goToResources);
    registerShortcut("mod+,", goToSettings);
    registerShortcut("mod+n", () => setNewThreadOpen(true));
    registerShortcut("mod+\\", toggleInspector);
    registerShortcut("mod+[", goBack);
    registerShortcut("escape", () => setNewThreadOpen(false));

    return () => {
      unregisterShortcut("mod+1");
      unregisterShortcut("mod+2");
      unregisterShortcut("mod+,");
      unregisterShortcut("mod+n");
      unregisterShortcut("mod+\\");
      unregisterShortcut("mod+[");
      unregisterShortcut("escape");
      cleanup();
    };
  }, [goToStream, goToResources, goToSettings, goBack, toggleInspector]);

  return (
    <div
      className="grid h-full overflow-hidden"
      style={{
        gridTemplateColumns: showInspector
          ? "220px 1fr 320px"
          : "220px 1fr",
      }}
    >
      <Sidebar onNewThread={() => setNewThreadOpen(true)} />
      <MainCanvas />
      {showInspector && <InspectorPanel />}
      <FeedbackToast />
      <NewThreadModal open={newThreadOpen} onClose={() => setNewThreadOpen(false)} />
    </div>
  );
}
