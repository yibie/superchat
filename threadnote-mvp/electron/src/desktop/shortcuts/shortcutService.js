import electron from "electron";
import {
  buildShortcutState,
  defaultShortcutSettings,
  getShortcutAction,
  normalizeAccelerator,
  normalizeShortcutRecord,
  normalizeShortcutSettings,
  ShortcutActionID,
  ShortcutRegistrationState,
  ShortcutScope
} from "./shortcutModel.js";

const { globalShortcut } = electron;

export class ShortcutService {
  constructor({
    store,
    onOpenQuickCapture = () => {},
    onOpenSettingsWindow = () => {},
    onStateChanged = () => {},
    globalShortcutImpl = globalShortcut
  }) {
    this.store = store;
    this.onOpenQuickCapture = onOpenQuickCapture;
    this.onOpenSettingsWindow = onOpenSettingsWindow;
    this.onStateChanged = onStateChanged;
    this.globalShortcut = globalShortcutImpl;
    this.settings = defaultShortcutSettings();
    this.states = [];
    this.currentGlobalAccelerator = null;
  }

  initialize() {
    this.settings = normalizeShortcutSettings(this.store.load());
    if (!this.settings[ShortcutActionID.QUICK_CAPTURE]?.accelerator) {
      const action = getShortcutAction(ShortcutActionID.QUICK_CAPTURE);
      const suggested = (action?.suggestionCandidates ?? []).find((candidate) => this.#canRegisterGlobalAccelerator(candidate)) ?? null;
      if (suggested) {
        this.settings[ShortcutActionID.QUICK_CAPTURE] = normalizeShortcutRecord({
          actionId: ShortcutActionID.QUICK_CAPTURE,
          accelerator: suggested,
          enabled: true
        });
        this.store.save(this.settings);
      }
    }
    this.#recomputeStates();
    return this.getShortcutSettings();
  }

  getShortcutSettings() {
    return this.states.map((state) => ({ ...state }));
  }

  getShortcutState(actionId) {
    return this.states.find((state) => state.actionId === actionId) ?? null;
  }

  getMenuAccelerator(actionId) {
    const state = this.getShortcutState(actionId);
    return state?.registrationState === ShortcutRegistrationState.REGISTERED ? state.accelerator : null;
  }

  updateShortcutSetting(actionId, accelerator) {
    const action = getShortcutAction(actionId);
    if (!action) {
      throw new Error(`Unknown shortcut action: ${actionId}`);
    }
    const normalized = normalizeAccelerator(accelerator);
    if (!normalized) {
      return this.clearShortcutSetting(actionId);
    }

    if (action.scope === ShortcutScope.GLOBAL) {
      if (!this.#canRegisterGlobalAccelerator(normalized)) {
        return {
          ...this.getShortcutState(actionId),
          attemptedAccelerator: normalized,
          attemptedRegistrationState: ShortcutRegistrationState.CONFLICT
        };
      }
    } else {
      const conflicting = this.states.find((state) =>
        state.scope === ShortcutScope.APP &&
        state.actionId !== actionId &&
        state.enabled &&
        state.registrationState === ShortcutRegistrationState.REGISTERED &&
        state.accelerator === normalized
      );
      if (conflicting) {
        return {
          ...this.getShortcutState(actionId),
          attemptedAccelerator: normalized,
          attemptedRegistrationState: ShortcutRegistrationState.CONFLICT,
          conflictingActionId: conflicting.actionId,
          conflictingActionLabel: conflicting.label
        };
      }
    }

    this.settings[actionId] = normalizeShortcutRecord({
      actionId,
      accelerator: normalized,
      enabled: true
    });
    this.store.save(this.settings);
    this.#recomputeStates();
    return this.getShortcutState(actionId);
  }

  clearShortcutSetting(actionId) {
    const action = getShortcutAction(actionId);
    if (!action) {
      throw new Error(`Unknown shortcut action: ${actionId}`);
    }
    this.settings[actionId] = normalizeShortcutRecord({
      actionId,
      accelerator: null,
      enabled: false
    });
    this.store.save(this.settings);
    this.#recomputeStates();
    return this.getShortcutState(actionId);
  }

  unregisterAll() {
    this.globalShortcut.unregisterAll();
    this.currentGlobalAccelerator = null;
  }

  #recomputeStates() {
    this.#unregisterCurrentGlobalAccelerator();
    const usedAppAccelerators = new Map();
    const nextStates = [];

    for (const action of Object.values(ShortcutActionID)) {
      const record = this.settings[action] ?? normalizeShortcutRecord({ actionId: action, accelerator: null, enabled: false });
      if (!record.enabled) {
        nextStates.push(buildShortcutState(record, ShortcutRegistrationState.DISABLED));
        continue;
      }
      if (!record.accelerator) {
        nextStates.push(buildShortcutState(record, ShortcutRegistrationState.UNSET));
        continue;
      }

      if (record.scope === ShortcutScope.GLOBAL) {
        const registered = this.globalShortcut.register(record.accelerator, () => {
          this.#dispatchGlobalAction(record.actionId);
        });
        if (registered) {
          this.currentGlobalAccelerator = record.accelerator;
          nextStates.push(buildShortcutState(record, ShortcutRegistrationState.REGISTERED));
        } else {
          nextStates.push(buildShortcutState(record, ShortcutRegistrationState.CONFLICT));
        }
        continue;
      }

      const conflicting = usedAppAccelerators.get(record.accelerator);
      if (conflicting) {
        nextStates.push(buildShortcutState(record, ShortcutRegistrationState.CONFLICT, {
          conflictingActionId: conflicting.actionId,
          conflictingActionLabel: conflicting.label
        }));
        continue;
      }
      const state = buildShortcutState(record, ShortcutRegistrationState.REGISTERED);
      usedAppAccelerators.set(record.accelerator, state);
      nextStates.push(state);
    }

    this.states = nextStates;
    this.onStateChanged(this.getShortcutSettings());
  }

  #dispatchGlobalAction(actionId) {
    if (actionId === ShortcutActionID.QUICK_CAPTURE) {
      this.onOpenQuickCapture();
    } else if (actionId === ShortcutActionID.OPEN_SETTINGS) {
      this.onOpenSettingsWindow();
    }
  }

  #canRegisterGlobalAccelerator(accelerator) {
    const candidate = normalizeAccelerator(accelerator);
    if (!candidate) {
      return false;
    }
    if (candidate === this.currentGlobalAccelerator) {
      return true;
    }

    const previous = this.currentGlobalAccelerator;
    if (previous) {
      this.globalShortcut.unregister(previous);
      this.currentGlobalAccelerator = null;
    }
    this.globalShortcut.unregister(candidate);
    const registered = this.globalShortcut.register(candidate, () => {});
    if (registered) {
      this.globalShortcut.unregister(candidate);
    }
    if (previous) {
      const restored = this.globalShortcut.register(previous, () => {
        this.#dispatchGlobalAction(ShortcutActionID.QUICK_CAPTURE);
      });
      if (restored) {
        this.currentGlobalAccelerator = previous;
      }
    }
    return registered;
  }

  #unregisterCurrentGlobalAccelerator() {
    if (!this.currentGlobalAccelerator) {
      return;
    }
    this.globalShortcut.unregister(this.currentGlobalAccelerator);
    this.currentGlobalAccelerator = null;
  }
}
