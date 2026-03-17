import { useEffect, useState, useCallback } from "react";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { Sidebar } from "./Sidebar.jsx";
import { MainCanvas } from "./MainCanvas.jsx";
import { InspectorPanel } from "./InspectorPanel.jsx";
import { FeedbackToast } from "../shared/FeedbackToast.jsx";
import { NewThreadModal } from "../modals/NewThreadModal.jsx";
import { initKeyboardShortcuts, registerShortcut, unregisterShortcut } from "../../lib/keyboard.js";
import { ipc } from "../../lib/ipc.js";
import { useShortcutSettings } from "../../hooks/useShortcutSettings.js";
import { ShortcutActionID, electronAcceleratorToAppCombo } from "../../lib/shortcutActions.js";
import { SURFACES } from "../../lib/constants.js";

export function AppShell() {
  const { inspectorOpen, surface, goToStream, goToResources, goBack, toggleInspector } = useNavigationContext();
  const showInspector = inspectorOpen && (surface === SURFACES.STREAM || surface === SURFACES.THREAD);
  const [newThreadOpen, setNewThreadOpen] = useState(false);
  const { shortcuts } = useShortcutSettings();

  // Wire app menu "Settings" to open independent settings window
  useEffect(() => {
    const unsubscribe = ipc.onOpenSettings(() => {
      void ipc.openSettingsWindow();
    });
    return () => unsubscribe?.();
  }, []);

  useEffect(() => {
    const cleanup = initKeyboardShortcuts();
    registerShortcut("escape", () => setNewThreadOpen(false));

    const bindings = [
      [ShortcutActionID.GO_TO_STREAM, goToStream],
      [ShortcutActionID.GO_TO_RESOURCES, goToResources],
      [ShortcutActionID.OPEN_SETTINGS, () => ipc.openSettingsWindow()],
      [ShortcutActionID.NEW_THREAD, () => setNewThreadOpen(true)],
      [ShortcutActionID.TOGGLE_INSPECTOR, toggleInspector],
      [ShortcutActionID.GO_BACK, goBack]
    ];
    const registeredCombos = [];
    for (const [actionId, handler] of bindings) {
      const shortcut = shortcuts.find((item) => item.actionId === actionId);
      if (!shortcut || !shortcut.enabled || shortcut.registrationState !== "registered") {
        continue;
      }
      const combo = electronAcceleratorToAppCombo(shortcut.accelerator);
      if (!combo) {
        continue;
      }
      registerShortcut(combo, handler);
      registeredCombos.push(combo);
    }

    return () => {
      for (const combo of registeredCombos) {
        unregisterShortcut(combo);
      }
      unregisterShortcut("escape");
      cleanup();
    };
  }, [goBack, goToResources, goToStream, shortcuts, toggleInspector]);

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
