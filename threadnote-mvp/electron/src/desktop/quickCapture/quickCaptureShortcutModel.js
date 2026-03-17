export const ShortcutRegistrationState = Object.freeze({
  REGISTERED: "registered",
  CONFLICT: "conflict",
  DISABLED: "disabled",
  UNSET: "unset"
});

export function defaultQuickCaptureShortcutConfig() {
  return {
    accelerator: null,
    enabled: true
  };
}

export function normalizeQuickCaptureShortcutConfig(config) {
  return {
    accelerator: normalizeAccelerator(config?.accelerator),
    enabled: config?.enabled !== false
  };
}

export function macOSSuggestedAccelerators() {
  return [
    "CommandOrControl+Shift+1",
    "CommandOrControl+Shift+2",
    "CommandOrControl+Alt+;"
  ];
}

export function normalizeAccelerator(value) {
  const trimmed = String(value ?? "").trim();
  return trimmed || null;
}

export function formatShortcutLabel(accelerator) {
  const normalized = normalizeAccelerator(accelerator);
  if (!normalized) {
    return "Not set";
  }

  return normalized
    .split("+")
    .map((part) => formatShortcutPart(part))
    .join(" + ");
}

function formatShortcutPart(part) {
  switch (String(part)) {
    case "CommandOrControl":
    case "CmdOrCtrl":
      return process.platform === "darwin" ? "Cmd" : "Ctrl";
    case "Command":
      return "Cmd";
    case "Control":
      return "Ctrl";
    case "Alt":
    case "Option":
      return process.platform === "darwin" ? "Option" : "Alt";
    case "Shift":
      return "Shift";
    case "Super":
      return "Super";
    case "Plus":
      return "+";
    case "Space":
      return "Space";
    default:
      return String(part).length === 1 ? String(part).toUpperCase() : String(part);
  }
}
