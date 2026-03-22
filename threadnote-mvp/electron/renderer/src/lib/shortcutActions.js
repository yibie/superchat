export const ShortcutActionID = Object.freeze({
  QUICK_CAPTURE: "quickCapture",
  QUICK_CAPTURE_CLIPBOARD: "quickCaptureClipboard",
  GO_TO_STREAM: "goToStream",
  GO_TO_RESOURCES: "goToResources",
  OPEN_SETTINGS: "openSettings",
  NEW_THREAD: "newThread",
  TOGGLE_INSPECTOR: "toggleInspector",
  GO_BACK: "goBack"
});

export const SHORTCUT_ACTIONS = Object.freeze([
  { id: ShortcutActionID.QUICK_CAPTURE, label: "Quick Capture", scope: "global" },
  { id: ShortcutActionID.QUICK_CAPTURE_CLIPBOARD, label: "Quick Capture from Clipboard", scope: "global" },
  { id: ShortcutActionID.GO_TO_STREAM, label: "Go to Stream", scope: "app" },
  { id: ShortcutActionID.GO_TO_RESOURCES, label: "Go to Resources", scope: "app" },
  { id: ShortcutActionID.OPEN_SETTINGS, label: "Open Settings", scope: "app" },
  { id: ShortcutActionID.NEW_THREAD, label: "New Thread", scope: "app" },
  { id: ShortcutActionID.TOGGLE_INSPECTOR, label: "Toggle Inspector", scope: "app" },
  { id: ShortcutActionID.GO_BACK, label: "Go Back", scope: "app" }
]);

const ACTIONS_BY_ID = new Map(SHORTCUT_ACTIONS.map((action) => [action.id, action]));

export function getShortcutAction(actionId) {
  return ACTIONS_BY_ID.get(actionId) ?? null;
}

export function formatShortcutLabel(accelerator) {
  if (!accelerator) {
    return "Not set";
  }
  return accelerator
    .split("+")
    .map((part) => {
      switch (part) {
        case "CommandOrControl":
        case "CmdOrCtrl":
          return "Cmd";
        case "Alt":
        case "Option":
          return "Option";
        default:
          return part.length === 1 ? part.toUpperCase() : part;
      }
    })
    .join(" + ");
}

export function electronAcceleratorToAppCombo(accelerator) {
  if (!accelerator) {
    return null;
  }
  const parts = accelerator.split("+");
  const combo = [];
  if (parts.includes("CommandOrControl") || parts.includes("Command") || parts.includes("Control") || parts.includes("CmdOrCtrl")) {
    combo.push("mod");
  }
  if (parts.includes("Shift")) {
    combo.push("shift");
  }
  if (parts.includes("Alt") || parts.includes("Option")) {
    combo.push("alt");
  }
  const key = parts[parts.length - 1];
  combo.push(String(key ?? "").toLowerCase());
  return combo.join("+");
}
