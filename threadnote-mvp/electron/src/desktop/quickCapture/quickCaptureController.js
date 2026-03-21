import electron from "electron";
import {
  defaultQuickCaptureShortcutConfig,
  macOSSuggestedAccelerators,
  normalizeAccelerator,
  normalizeQuickCaptureShortcutConfig,
  ShortcutRegistrationState
} from "./quickCaptureShortcutModel.js";

const { app: electronApp, clipboard, globalShortcut } = electron;

export class QuickCaptureController {
  constructor({
    BrowserWindow,
    appService,
    shellState,
    preloadPath,
    createWindow,
    shortcutStore,
    onShortcutStateChanged = () => {},
    onCaptureBackgroundTask = () => {},
    onWindowClosed = () => {},
    appImpl = electronApp,
    globalShortcutImpl = globalShortcut,
    clipboardImpl = clipboard
  }) {
    this.BrowserWindow = BrowserWindow;
    this.appService = appService;
    this.shellState = shellState;
    this.preloadPath = preloadPath;
    this.createWindow = createWindow;
    this.shortcutStore = shortcutStore;
    this.onShortcutStateChanged = onShortcutStateChanged;
    this.onCaptureBackgroundTask = onCaptureBackgroundTask;
    this.onWindowClosed = onWindowClosed;
    this.app = appImpl;
    this.globalShortcut = globalShortcutImpl;
    this.clipboard = clipboardImpl;
    this.window = null;
    this.forceClose = false;
    this.returnToBackgroundOnClose = false;
    this.shortcutConfig = defaultQuickCaptureShortcutConfig();
    this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.UNSET);
    this.currentRegisteredAccelerator = null;
  }

  initializeShortcut() {
    const saved = this.shortcutStore?.load?.();
    if (saved) {
      this.shortcutConfig = normalizeQuickCaptureShortcutConfig(saved);
      return this.#applyShortcutConfig(this.shortcutConfig);
    }

    const suggested = this.#suggestInitialShortcut();
    if (suggested) {
      this.shortcutConfig = this.shortcutStore.save({
        accelerator: suggested,
        enabled: true
      });
      return this.#applyShortcutConfig(this.shortcutConfig);
    }

    this.shortcutConfig = this.shortcutStore.save({
      accelerator: null,
      enabled: true
    });
    this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.UNSET);
    this.onShortcutStateChanged(this.shortcutState);
    return this.shortcutState;
  }

  getShortcutState() {
    return { ...this.shortcutState };
  }

  setShortcut(accelerator) {
    const normalized = normalizeAccelerator(accelerator);
    if (!normalized) {
      return this.clearShortcut();
    }

    if (!this.#canRegisterAccelerator(normalized)) {
      return {
        ...this.getShortcutState(),
        attemptedAccelerator: normalized,
        attemptedRegistrationState: ShortcutRegistrationState.CONFLICT
      };
    }

    this.shortcutConfig = this.shortcutStore.save({
      accelerator: normalized,
      enabled: true
    });
    return this.#applyShortcutConfig(this.shortcutConfig);
  }

  clearShortcut() {
    this.shortcutConfig = this.shortcutStore.save({
      accelerator: null,
      enabled: false
    });
    return this.#applyShortcutConfig(this.shortcutConfig);
  }

  unregisterAll() {
    this.globalShortcut.unregisterAll();
    this.currentRegisteredAccelerator = null;
  }

  prepareForQuit() {
    this.forceClose = true;
    if (this.window && !this.window.isDestroyed()) {
      this.window.destroy();
    }
  }

  async openQuickCapture(payload = {}) {
    const window = this.#ensureWindow();
    const requestedDraft = normalizeDraft(payload);
    const clipboardDraft = isExternalQuickCaptureTrigger(requestedDraft)
      ? await this.importFromClipboard()
      : null;
    const draft = mergeDrafts(requestedDraft, clipboardDraft);
    this.returnToBackgroundOnClose = isExternalQuickCaptureTrigger(draft);
    if (window.isMinimized()) {
      window.restore();
    }
    window.setVisibleOnAllWorkspaces(true, { visibleOnFullScreen: true });
    window.moveTop();
    window.center();
    window.show();
    window.focus();
    if (window.webContents.isLoadingMainFrame()) {
      window.webContents.once("did-finish-load", () => {
        window.webContents.send("quick-capture:hydrate", draft);
      });
    } else {
      window.webContents.send("quick-capture:hydrate", draft);
    }
    return true;
  }

  closeQuickCapture() {
    if (!this.window || this.window.isDestroyed()) {
      return true;
    }
    this.window.setVisibleOnAllWorkspaces(false);
    this.window.setAlwaysOnTop(false);
    if (this.returnToBackgroundOnClose && process.platform === "darwin") {
      this.app?.hide?.();
      this.window.hide();
      this.returnToBackgroundOnClose = false;
      return true;
    }
    this.window.hide();
    this.returnToBackgroundOnClose = false;
    return true;
  }

  resizeQuickCapture({ width, height } = {}) {
    if (!this.window || this.window.isDestroyed()) {
      return false;
    }
    const nextWidth = Number.isFinite(width) ? Math.round(width) : this.window.getBounds().width;
    const nextHeight = Number.isFinite(height) ? Math.round(height) : this.window.getBounds().height;
    this.window.setContentSize(nextWidth, nextHeight);
    this.window.center();
    return true;
  }

  async importFromClipboard() {
    const clipboardTypes = this.clipboard.availableFormats();
    const text = this.clipboard.readText();
    const image = this.clipboard.readImage();
    const attachments = [];

    if (!image.isEmpty()) {
      const pngBuffer = image.toPNG();
      if (pngBuffer.length > 0) {
        const saved = this.appService.writeAttachmentBuffer({
          bytes: Array.from(pngBuffer),
          fileName: `clipboard-${Date.now()}.png`,
          mimeType: "image/png"
        });
        attachments.push({
          ...saved,
          fileName: "clipboard.png",
          mimeType: "image/png",
          size: pngBuffer.length
        });
      }
    }

    return normalizeDraft({
      text,
      attachments,
      source: "clipboardImport",
      sourceContext: { clipboardTypes }
    });
  }

  async submitQuickCapture(draft = {}) {
    const normalized = normalizeDraft(draft);
    const result = await this.appService.submitExternalCapture(normalized);
    this.onCaptureBackgroundTask(result.backgroundTask);
    return result;
  }

  #ensureWindow() {
    if (this.window && !this.window.isDestroyed()) {
      return this.window;
    }

    this.window = this.createWindow({
      BrowserWindow: this.BrowserWindow,
      shellState: this.shellState,
      preloadPath: this.preloadPath,
      onCloseRequest: (event) => {
        if (this.forceClose) {
          return;
        }
        event.preventDefault();
        this.window?.hide();
      },
      onClosed: () => {
        this.window = null;
        this.forceClose = false;
        this.onWindowClosed();
      }
    });
    return this.window;
  }

  #applyShortcutConfig(config) {
    this.#unregisterCurrentAccelerator();
    const resolved = normalizeQuickCaptureShortcutConfig(config);
    this.shortcutConfig = resolved;

    if (!resolved.enabled) {
      this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.DISABLED);
      this.onShortcutStateChanged(this.shortcutState);
      return this.shortcutState;
    }

    if (!resolved.accelerator) {
      this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.UNSET);
      this.onShortcutStateChanged(this.shortcutState);
      return this.shortcutState;
    }

    const registered = this.globalShortcut.register(resolved.accelerator, () => {
      void this.openQuickCapture({
        source: "quickCaptureHotkey",
        sourceContext: {
          trigger: "shortcut",
          accelerator: resolved.accelerator
        }
      });
    });

    if (!registered) {
      console.error("[quickCapture] failed to register configured shortcut", resolved.accelerator);
      this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.CONFLICT);
      this.onShortcutStateChanged(this.shortcutState);
      return this.shortcutState;
    }

    this.currentRegisteredAccelerator = resolved.accelerator;
    this.shortcutState = this.#buildShortcutState(ShortcutRegistrationState.REGISTERED);
    this.onShortcutStateChanged(this.shortcutState);
    return this.shortcutState;
  }

  #suggestInitialShortcut() {
    const candidates = process.platform === "darwin" ? macOSSuggestedAccelerators() : [];
    return candidates.find((candidate) => this.#canRegisterAccelerator(candidate)) ?? null;
  }

  #canRegisterAccelerator(accelerator) {
    const candidate = normalizeAccelerator(accelerator);
    if (!candidate) {
      return false;
    }
    if (this.currentRegisteredAccelerator === candidate) {
      return true;
    }

    const previous = this.currentRegisteredAccelerator;
    if (previous) {
      this.globalShortcut.unregister(previous);
      this.currentRegisteredAccelerator = null;
    }

    this.globalShortcut.unregister(candidate);
    const registered = this.globalShortcut.register(candidate, () => {});
    if (registered) {
      this.globalShortcut.unregister(candidate);
    }

    if (previous) {
      const restored = this.globalShortcut.register(previous, () => {
        void this.openQuickCapture({
          source: "quickCaptureHotkey",
          sourceContext: {
            trigger: "shortcut",
            accelerator: previous
          }
        });
      });
      if (restored) {
        this.currentRegisteredAccelerator = previous;
      }
    }
    return registered;
  }

  #unregisterCurrentAccelerator() {
    if (!this.currentRegisteredAccelerator) {
      return;
    }
    this.globalShortcut.unregister(this.currentRegisteredAccelerator);
    this.currentRegisteredAccelerator = null;
  }

  #buildShortcutState(registrationState, overrides = null) {
    const config = overrides
      ? normalizeQuickCaptureShortcutConfig(overrides)
      : this.shortcutConfig;
    return {
      accelerator: config.accelerator,
      enabled: config.enabled,
      registrationState
    };
  }
}

function normalizeDraft(payload = {}) {
  return {
    text: String(payload.text ?? ""),
    attachments: Array.isArray(payload.attachments) ? payload.attachments.filter(Boolean) : [],
    references: Array.isArray(payload.references) ? payload.references : [],
    source: payload.source ?? "quickCaptureHotkey",
    sourceContext: { ...(payload.sourceContext ?? {}) }
  };
}

function mergeDrafts(requestedDraft = {}, clipboardDraft = null) {
  const baseDraft = normalizeDraft(requestedDraft);
  const importedDraft = clipboardDraft ? normalizeDraft(clipboardDraft) : null;
  if (!importedDraft || (!importedDraft.text.trim() && importedDraft.attachments.length === 0)) {
    return baseDraft;
  }

  return {
    ...baseDraft,
    text: baseDraft.text.trim() ? baseDraft.text : importedDraft.text,
    attachments: [...baseDraft.attachments, ...importedDraft.attachments],
    source: importedDraft.source ?? baseDraft.source,
    sourceContext: {
      ...importedDraft.sourceContext,
      ...baseDraft.sourceContext
    }
  };
}

function isExternalQuickCaptureTrigger(draft = {}) {
  if (draft.source !== "quickCaptureHotkey") {
    return false;
  }
  const trigger = draft.sourceContext?.trigger ?? null;
  return trigger === "shortcut";
}
