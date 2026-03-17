export const ShortcutScope = Object.freeze({
  GLOBAL: "global",
  APP: "app"
});

export const ShortcutRegistrationState = Object.freeze({
  REGISTERED: "registered",
  CONFLICT: "conflict",
  DISABLED: "disabled",
  UNSET: "unset"
});

export const ShortcutActionID = Object.freeze({
  QUICK_CAPTURE: "quickCapture",
  GO_TO_STREAM: "goToStream",
  GO_TO_RESOURCES: "goToResources",
  OPEN_SETTINGS: "openSettings",
  NEW_THREAD: "newThread",
  TOGGLE_INSPECTOR: "toggleInspector",
  GO_BACK: "goBack"
});

export const SHORTCUT_ACTIONS = Object.freeze([
  {
    id: ShortcutActionID.QUICK_CAPTURE,
    label: "Quick Capture",
    scope: ShortcutScope.GLOBAL,
    defaultAccelerator: null,
    suggestionCandidates: [
      "CommandOrControl+Shift+1",
      "CommandOrControl+Shift+2",
      "CommandOrControl+Alt+;"
    ]
  },
  {
    id: ShortcutActionID.GO_TO_STREAM,
    label: "Go to Stream",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+1"
  },
  {
    id: ShortcutActionID.GO_TO_RESOURCES,
    label: "Go to Resources",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+2"
  },
  {
    id: ShortcutActionID.OPEN_SETTINGS,
    label: "Open Settings",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+,"
  },
  {
    id: ShortcutActionID.NEW_THREAD,
    label: "New Thread",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+N"
  },
  {
    id: ShortcutActionID.TOGGLE_INSPECTOR,
    label: "Toggle Inspector",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+\\"
  },
  {
    id: ShortcutActionID.GO_BACK,
    label: "Go Back",
    scope: ShortcutScope.APP,
    defaultAccelerator: "CommandOrControl+["
  }
]);

const ACTIONS_BY_ID = new Map(SHORTCUT_ACTIONS.map((action) => [action.id, action]));

export function getShortcutAction(actionID) {
  return ACTIONS_BY_ID.get(actionID) ?? null;
}

export function defaultShortcutSettings() {
  return Object.fromEntries(
    SHORTCUT_ACTIONS.map((action) => [
      action.id,
      normalizeShortcutRecord({
        actionId: action.id,
        accelerator: action.defaultAccelerator,
        enabled: action.defaultAccelerator != null
      })
    ])
  );
}

export function normalizeShortcutSettings(value) {
  const defaults = defaultShortcutSettings();
  const source = value && typeof value === "object" ? value : {};
  for (const action of SHORTCUT_ACTIONS) {
    defaults[action.id] = normalizeShortcutRecord({
      ...defaults[action.id],
      ...(source[action.id] ?? {}),
      actionId: action.id
    });
  }
  return defaults;
}

export function normalizeShortcutRecord(record) {
  const action = getShortcutAction(record?.actionId);
  return {
    actionId: action?.id ?? record?.actionId ?? null,
    accelerator: normalizeAccelerator(record?.accelerator),
    enabled: record?.enabled !== false && normalizeAccelerator(record?.accelerator) != null,
    scope: action?.scope ?? record?.scope ?? ShortcutScope.APP
  };
}

export function normalizeAccelerator(value) {
  const trimmed = String(value ?? "").trim();
  return trimmed || null;
}

export function buildShortcutState(record, registrationState, extra = {}) {
  const action = getShortcutAction(record?.actionId);
  return {
    actionId: record?.actionId ?? null,
    label: action?.label ?? record?.actionId ?? "",
    accelerator: normalizeAccelerator(record?.accelerator),
    enabled: record?.enabled !== false,
    scope: record?.scope ?? action?.scope ?? ShortcutScope.APP,
    registrationState,
    ...extra
  };
}

export function electronAcceleratorToAppCombo(accelerator) {
  const normalized = normalizeAccelerator(accelerator);
  if (!normalized) {
    return null;
  }

  const parts = normalized.split("+");
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
  combo.push(normalizeComboKey(key));
  return combo.join("+");
}

function normalizeComboKey(key) {
  if (key === "Space") {
    return "space";
  }
  return String(key ?? "").toLowerCase();
}
