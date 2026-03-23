import path from "node:path";
import { fileURLToPath } from "node:url";
import { app, BrowserWindow, Menu, dialog, ipcMain, shell } from "electron";
import { bootstrapDesktopApp } from "./app/bootstrap.js";
import { WorkspaceManager } from "../infrastructure/persistence/workspace/workspaceManager.js";
import { ThreadnoteApplicationService } from "../application/services/threadnoteApplicationService.js";
import { AIProviderConfigStore } from "../infrastructure/ai/runtime/aiProviderConfigStore.js";
import { AIProviderRuntime } from "../infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIRequestQueue } from "../infrastructure/ai/queue/aiRequestQueue.js";
import { ThreadnoteAIService } from "../infrastructure/ai/runtime/services/threadnoteAIService.js";
import { createMainWindow, createQuickCaptureWindow, createSettingsWindow } from "./windows/windowFactory.js";
import { buildAppMenuTemplate } from "./menus/appMenu.js";
import { QuickCaptureController } from "./quickCapture/quickCaptureController.js";
import { ShortcutStore } from "./shortcuts/shortcutStore.js";
import { ShortcutService } from "./shortcuts/shortcutService.js";
import { ShortcutActionID } from "./shortcuts/shortcutModel.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const preloadPath = path.join(__dirname, "bridge", "preload.js");

let shellState = null;
let workspaceManager = null;
let appService = null;
let mainWindow = null;
let settingsWindow = null;
let quickCaptureController = null;
let shortcutService = null;
const threadRefreshTokens = new Map();
const captureFinalizationTokens = new Map();


installProcessDiagnostics();
app.commandLine.appendSwitch("allow-file-access-from-files");
installAppLifecycleDiagnostics();
console.error("[desktop] before app.whenReady");
app.whenReady().then(() => {
  console.error("[desktop] after app.whenReady");
  bootstrapApplication();
}).catch((error) => {
  console.error("[desktop] bootstrap failed", error);
  app.quit();
});

app.on("activate", () => {
  if (BrowserWindow.getAllWindows().length === 0) {
    openMainWindow();
  }
});

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") {
    app.quit();
  }
});

app.on("before-quit", () => {
  quickCaptureController?.prepareForQuit();
  appService?.aiProviderRuntime?.stopLocalKeepWarm?.();
});

app.on("will-quit", () => {
  shortcutService?.unregisterAll();
});

function openMainWindow() {
  if (mainWindow && !mainWindow.isDestroyed()) {
    console.error("[desktop] reuse main window");
    mainWindow.focus();
    return mainWindow;
  }
  console.error("[desktop] creating main window");
  mainWindow = createMainWindow({
    BrowserWindow,
    shellState,
    preloadPath,
    onClosed: () => {
      mainWindow = null;
    }
  });
  attachWindowDiagnostics(mainWindow, "main");
  console.error("[desktop] main window created");
  return mainWindow;
}

function bootstrapApplication() {
  workspaceManager = new WorkspaceManager({
    stateFilePath: path.join(app.getPath("userData"), "workspace-state.json")
  });
  console.error("[desktop] workspace manager ready", workspaceManager.describe?.() ?? null);

  const aiProviderRuntime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({
      configPath: path.join(app.getPath("userData"), "ai-provider.json")
    })
  });
  const aiService = new ThreadnoteAIService({
    providerRuntime: aiProviderRuntime,
    requestQueue: new AIRequestQueue({
      maxConcurrent: aiProviderRuntime.preferredMaxConcurrentRequests || 2
    })
  });

  appService = new ThreadnoteApplicationService({
    workspaceManager,
    aiProviderRuntime,
    aiService,
    onAsyncStateChanged: ({ threadID } = {}) => {
      const thread = threadID ? appService.openThreadSurface(threadID) : null;
      if (threadID && thread) {
        broadcastThreadUpdated({ threadID, thread });
      }
      broadcastWorkbenchUpdated({
        workbench: buildWorkbenchState(),
        threadID: threadID ?? null,
        thread
      });
    }
  });
  console.error("[desktop] application service ready");
  appService.restoreWorkspace();
  aiProviderRuntime.startLocalKeepWarm();
  void aiProviderRuntime.prewarmIfLocal({ reason: "desktop-bootstrap" }).catch(() => {});
  shellState = bootstrapDesktopApp({ workspaceManager });
  console.error("[desktop] shell state ready", {
    workspace: shellState.workspace?.workspacePath ?? null,
    mainHTML: shellState.windows.main.htmlPath
  });

  installIPC();
  console.error("[desktop] ipc installed");
  quickCaptureController = new QuickCaptureController({
    BrowserWindow,
    appService,
    shellState,
    preloadPath,
    onCaptureBackgroundTask: (task) => {
      scheduleCaptureFinalization(task);
    },
    createWindow: ({ BrowserWindow: WindowCtor, shellState: state, preloadPath: preload, onClosed, onCloseRequest }) =>
      createQuickCaptureWindow({
        BrowserWindow: WindowCtor,
        shellState: state,
        preloadPath: preload,
        onClosed,
        onCloseRequest
      })
  });
  shortcutService = new ShortcutService({
    store: new ShortcutStore({
      configPath: path.join(app.getPath("userData"), "shortcuts.json"),
      legacyQuickCapturePath: path.join(app.getPath("userData"), "quick-capture-shortcut.json")
    }),
    onOpenQuickCapture: () => {
      void quickCaptureController?.openQuickCapture({
        source: "quickCaptureHotkey",
        sourceContext: { trigger: "shortcut" }
      });
    },
    onOpenQuickCaptureClipboard: async () => {
      const imported = await quickCaptureController?.importFromClipboard();
      await quickCaptureController?.openQuickCapture({
        ...(imported ?? {}),
        source: imported?.source ?? "clipboardImport",
        sourceContext: {
          ...(imported?.sourceContext ?? {}),
          trigger: "shortcut"
        }
      });
    },
    onOpenSettingsWindow: () => {
      openSettingsWindow();
    },
    onStateChanged: () => {
      installMenu();
      broadcastShortcutSettings();
    }
  });
  shortcutService.initialize();
  installMenu();
  console.error("[desktop] menu installed");
  openMainWindow();
  console.error("[desktop] openMainWindow invoked");
}

function installMenu() {
  const template = buildAppMenuTemplate({
    onOpenSettings: () => openSettingsWindow(),
    onOpenQuickCapture: () => quickCaptureController?.openQuickCapture({
      source: "quickCaptureHotkey",
      sourceContext: { trigger: "menu" }
    }),
    quickCaptureAccelerator: shortcutService?.getMenuAccelerator(ShortcutActionID.QUICK_CAPTURE) ?? null,
    settingsAccelerator: shortcutService?.getMenuAccelerator(ShortcutActionID.OPEN_SETTINGS) ?? null
  });
  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

function installIPC() {
  ipcMain.handle("desktop:get-shell-state", () => ({
    ...shellState,
    workspace: workspaceManager.describe()
  }));
  ipcMain.handle("desktop:open-settings", async () => {
    openSettingsWindow();
    return true;
  });
  ipcMain.handle("desktop:close-window", (event) => {
    BrowserWindow.fromWebContents(event.sender)?.close();
    return true;
  });
  ipcMain.handle("app:get-workbench-state", () => buildWorkbenchState());
  ipcMain.handle("app:repair-workspace-relations", () => {
    const result = appService.repairWorkspaceRelations();
    return {
      result,
      workbench: buildWorkbenchState()
    };
  });
  ipcMain.handle("app:get-stream-page", (_event, payload) => appService.streamPage(payload ?? {}));
  ipcMain.handle("app:get-thread-page", (_event, payload) => appService.openThreadSurface(payload?.threadID, payload ?? {}));
  ipcMain.handle("app:get-shortcut-settings", () => shortcutService?.getShortcutSettings() ?? []);
  ipcMain.handle("app:update-shortcut-setting", (_event, payload) => {
    return shortcutService?.updateShortcutSetting(payload?.actionId, payload?.accelerator ?? null) ?? null;
  });
  ipcMain.handle("app:clear-shortcut-setting", (_event, actionId) => {
    return shortcutService?.clearShortcutSetting(actionId) ?? null;
  });
  ipcMain.handle("app:open-settings-window", () => {
    openSettingsWindow();
    return true;
  });
  ipcMain.handle("app:create-workspace", async () => {
    const result = await dialog.showSaveDialog({
      title: "Create Threadnote Workspace",
      defaultPath: path.join(app.getPath("documents"), "Threadnote.threadnote"),
      buttonLabel: "Create Workspace"
    });
    if (!result.canceled && result.filePath) {
      appService.createWorkspace(result.filePath);
      shellState = bootstrapDesktopApp({ workspaceManager });
    }
    return buildWorkbenchState();
  });
  ipcMain.handle("app:open-workspace", async () => {
    const result = await dialog.showOpenDialog({
      title: "Open Threadnote Workspace",
      properties: ["openDirectory"]
    });
    if (!result.canceled && result.filePaths[0]) {
      appService.openWorkspace(result.filePaths[0]);
      shellState = bootstrapDesktopApp({ workspaceManager });
    }
    return buildWorkbenchState();
  });
  ipcMain.handle("app:create-thread", async (_event, payload) => {
    const thread = await appService.createThread(payload ?? {});
    return {
      workbench: buildWorkbenchState(),
      thread
    };
  });
  ipcMain.handle("app:create-thread-from-entry", async (_event, payload) => {
    const thread = await appService.createThreadFromEntry(payload ?? {});
    return {
      workbench: buildWorkbenchState(),
      thread
    };
  });
  ipcMain.handle("app:archive-thread", async (_event, threadID) => {
    await appService.archiveThread(threadID);
    return {
      workbench: buildWorkbenchState(),
      thread: null
    };
  });
  ipcMain.handle("app:update-thread-title", async (_event, payload) => {
    const thread = await appService.updateThreadTitle(payload ?? {});
    return {
      workbench: buildWorkbenchState(),
      thread
    };
  });
  ipcMain.handle("app:submit-capture", async (_event, payload) => {
    const result = await appService.submitCapture(payload ?? {});
    scheduleCaptureFinalization(result.backgroundTask);
    const routedThreadID = result.routingDecision?.threadID ?? null;
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: routedThreadID ? appService.openThreadSurface(routedThreadID) : null
    };
  });
  ipcMain.handle("app:open-quick-capture", async (_event, payload) => {
    return quickCaptureController?.openQuickCapture(payload ?? {});
  });
  ipcMain.handle("app:close-quick-capture", () => {
    return quickCaptureController?.closeQuickCapture() ?? true;
  });
  ipcMain.handle("app:resize-quick-capture", (_event, payload) => {
    return quickCaptureController?.resizeQuickCapture(payload ?? {}) ?? false;
  });
  ipcMain.handle("app:import-from-clipboard", async () => {
    return quickCaptureController?.importFromClipboard();
  });
  ipcMain.handle("app:submit-quick-capture", async (_event, payload) => {
    return quickCaptureController?.submitQuickCapture(payload ?? {});
  });
  ipcMain.handle("app:copy-attachment", (_event, filePath) => {
    return appService.copyAttachment(filePath);
  });
  ipcMain.handle("app:write-attachment-buffer", (_event, payload) => {
    return appService.writeAttachmentBuffer(payload ?? {});
  });
  ipcMain.handle("app:open-thread", async (_event, threadID) => ({
    workbench: buildWorkbenchState(),
    thread: openThreadDeterministic(threadID)
  }));
  ipcMain.handle("app:append-reply", async (_event, payload) => {
    const result = await appService.appendReply(payload ?? {});
    console.error("[desktop] append-reply", {
      targetEntryID: payload?.entryID ?? null,
      entryID: result?.entry?.id ?? null,
      parentEntryID: result?.entry?.parentEntryID ?? null,
      references: result?.entry?.references ?? []
    });
    const threadID = result?.entry?.threadID ?? null;
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThreadSurface(threadID) : null
    };
  });
  ipcMain.handle("app:update-entry-text", async (_event, payload) => {
    const result = await appService.updateEntryText(payload ?? {});
    const threadID = payload?.entryID ? findEntryThreadID(payload.entryID) : null;
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThreadSurface(threadID) : null
    };
  });
  ipcMain.handle("app:update-entry-kind", async (_event, payload) => {
    const result = await appService.updateEntryKind(payload ?? {});
    const threadID = payload?.entryID ? findEntryThreadID(payload.entryID) : null;
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThreadSurface(threadID) : null
    };
  });
  ipcMain.handle("app:update-entry-status", async (_event, payload) => {
    const result = await appService.updateEntryStatus(payload ?? {});
    const threadID = payload?.entryID ? findEntryThreadID(payload.entryID) : null;
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThreadSurface(threadID) : null
    };
  });
  ipcMain.handle("app:delete-entry", async (_event, entryID) => {
    const threadID = findEntryThreadID(entryID);
    await appService.deleteEntry(entryID);
    return {
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThreadSurface(threadID) : null
    };
  });
  ipcMain.handle("app:route-entry-to-thread", async (_event, payload) => {
    const thread = await appService.routeEntryToThread(payload ?? {});
    return {
      workbench: buildWorkbenchState(),
      thread: thread ?? null,
      result: thread?.routeResult ?? null
    };
  });
  ipcMain.handle("app:get-entry-rich-preview", async (_event, entryID) => {
    return appService.getEntryRichPreview(entryID);
  });
  ipcMain.handle("app:get-locator-rich-preview", async (_event, locator) => {
    return appService.getLocatorRichPreview(locator);
  });
  ipcMain.handle("app:prepare-thread", async (_event, payload) => {
    return appService.prepareThread(payload ?? {});
  });
  ipcMain.handle("app:get-ai-provider-config", () => appService.getAIProviderConfig());
  ipcMain.handle("app:save-ai-provider-config", async (_event, payload) => {
    const result = await appService.configureAIProvider(payload ?? {});
    broadcastWorkbenchUpdated({
      workbench: buildWorkbenchState(),
      threadID: null,
      thread: null
    });
    return result;
  });
  ipcMain.handle("app:test-ai-provider", async (_event, payload) => {
    return appService.testAIProvider(payload ?? {});
  });
  ipcMain.handle("app:open-locator", async (_event, locator) => {
    if (!locator) {
      return false;
    }
    if (/^https?:\/\//i.test(locator)) {
      await shell.openExternal(locator);
      return true;
    }
    await shell.openPath(locator);
    return true;
  });
  ipcMain.handle("app:ping-ai-provider", async () => {
    try {
      return await appService.pingAIProvider();
    } catch (error) {
      return {
        ok: false,
        text: error.message,
        backendLabel: aiProviderRuntime.backendLabel,
        latencyMS: null,
        modelID: aiProviderRuntime.config?.model ?? null
      };
    }
  });
}

function openSettingsWindow() {
  if (settingsWindow && !settingsWindow.isDestroyed()) {
    settingsWindow.focus();
    return settingsWindow;
  }
  settingsWindow = createSettingsWindow({
    BrowserWindow,
    shellState,
    preloadPath,
    onClosed: () => {
      settingsWindow = null;
    }
  });
  return settingsWindow;
}

function broadcastShortcutSettings() {
  const payload = shortcutService?.getShortcutSettings() ?? [];
  for (const window of [mainWindow, settingsWindow]) {
    if (window && !window.isDestroyed()) {
      window.webContents.send("app:shortcut-settings-updated", payload);
    }
  }
}

function broadcastThreadUpdated(payload) {
  if (!payload?.threadID || !payload?.thread?.thread?.id) {
    return;
  }
  for (const window of [mainWindow]) {
    if (window && !window.isDestroyed()) {
      window.webContents.send("app:thread-updated", payload);
    }
  }
}

function broadcastWorkbenchUpdated(payload) {
  if (!payload?.workbench) {
    return;
  }
  for (const window of [mainWindow]) {
    if (window && !window.isDestroyed()) {
      window.webContents.send("app:workbench-updated", payload);
    }
  }
}

function buildWorkbenchState() {
  const workspace = workspaceManager.describe();
  if (!workspace) {
    return {
      workspace: null,
      home: null,
      selectedSurface: "workspace-setup"
    };
  }
  const loaded = appService.loadWorkspace();
  return {
    workspace: loaded.workspace,
    home: appService.homeSurfaceView(),
    selectedSurface: "stream"
  };
}

function installProcessDiagnostics() {
  process.on("uncaughtException", (error) => {
    console.error("[desktop] uncaughtException", error);
  });
  process.on("unhandledRejection", (reason) => {
    console.error("[desktop] unhandledRejection", reason);
  });
  app.on("render-process-gone", (_event, webContents, details) => {
    console.error("[desktop] render-process-gone", {
      url: webContents?.getURL?.() ?? null,
      ...details
    });
  });
  app.on("child-process-gone", (_event, details) => {
    console.error("[desktop] child-process-gone", details);
  });
}

function installAppLifecycleDiagnostics() {
  console.error("[desktop] app lifecycle diagnostics installed", {
    isReady: app.isReady(),
    isPackaged: app.isPackaged
  });
  app.on("will-finish-launching", () => {
    console.error("[desktop] will-finish-launching", { isReady: app.isReady() });
  });
  app.on("ready", () => {
    console.error("[desktop] ready event", { isReady: app.isReady() });
  });
  app.on("activate", () => {
    console.error("[desktop] activate event", { windowCount: BrowserWindow.getAllWindows().length });
  });
  app.on("browser-window-created", (_event, window) => {
    console.error("[desktop] browser-window-created", {
      title: window.getTitle(),
      visible: window.isVisible()
    });
  });
  app.on("web-contents-created", (_event, contents) => {
    console.error("[desktop] web-contents-created", {
      type: contents.getType?.() ?? "unknown"
    });
  });
  setTimeout(() => {
    console.error("[desktop] whenReady timeout probe", {
      isReady: app.isReady(),
      argv: process.argv,
      execPath: process.execPath
    });
  }, 5000);
}

function attachWindowDiagnostics(window, label) {
  window.webContents.on("did-start-loading", () => {
    console.error(`[window:${label}] did-start-loading`, window.webContents.getURL());
  });
  window.webContents.on("did-finish-load", () => {
    console.error(`[window:${label}] did-finish-load`, window.webContents.getURL());
  });
  window.webContents.on("did-fail-load", (_event, errorCode, errorDescription, validatedURL, isMainFrame) => {
    console.error(`[window:${label}] did-fail-load`, {
      errorCode,
      errorDescription,
      validatedURL,
      isMainFrame
    });
  });
  window.webContents.on("render-process-gone", (_event, details) => {
    console.error(`[window:${label}] render-process-gone`, details);
  });
  window.webContents.on("unresponsive", () => {
    console.error(`[window:${label}] unresponsive`);
  });
  window.webContents.on("responsive", () => {
    console.error(`[window:${label}] responsive`);
  });
  window.webContents.on("console-message", (event) => {
    const level = event.level ?? event.messageLevel ?? "log";
    console.error(`[window:${label}] console-message`, {
      level,
      message: event.message,
      lineNumber: event.lineNumber,
      sourceId: event.sourceId
    });
  });
}

function openThreadDeterministic(threadID) {
  if (!threadID) {
    return null;
  }
  const thread = appService.openThreadSurface(threadID);
  scheduleThreadRefresh(threadID);
  return thread;
}

function scheduleThreadRefresh(threadID) {
  if (!threadID || !appService?.aiService) {
    return;
  }
  const token = `${Date.now()}-${Math.random()}`;
  threadRefreshTokens.set(threadID, token);
  try {
    appService.invalidateAIOutputState(`openThread:${threadID}`, { threadIDs: [threadID] });
    appService.scheduleSweep({ delay: 100, reason: `openThread:${threadID}` });
  } catch (error) {
    console.error("[ai-refresh] failed", { threadID, error: error?.message ?? String(error) });
  } finally {
    if (threadRefreshTokens.get(threadID) === token) {
      threadRefreshTokens.delete(threadID);
    }
  }
}

function scheduleCaptureFinalization(task) {
  if (!task?.entryID) {
    return;
  }

  const token = `${Date.now()}-${Math.random()}`;
  captureFinalizationTokens.set(task.entryID, token);

  void appService.finalizeCaptureAsync(task)
    .then((result) => {
      if (captureFinalizationTokens.get(task.entryID) !== token) {
        return;
      }
      console.error("[ai-finalize] completed", {
        entryID: task.entryID,
        threadID: result?.threadID ?? null
      });
      const payload = {
        workbench: buildWorkbenchState(),
        threadID: result?.threadID ?? null,
        thread: result?.threadID ? appService.openThreadSurface(result.threadID) : null
      };
      if (payload.threadID && payload.thread) {
        broadcastThreadUpdated({
          threadID: payload.threadID,
          thread: payload.thread
        });
      }
      broadcastWorkbenchUpdated(payload);
    })
    .catch((error) => {
      console.error("[ai-finalize] failed", {
        entryID: task.entryID,
        error: error?.message ?? String(error)
      });
      if (captureFinalizationTokens.get(task.entryID) !== token) {
        return;
      }
      const threadID = task.refreshThreadID ?? null;
      const thread = threadID ? appService.openThreadSurface(threadID) : null;
      if (threadID && thread) {
        broadcastThreadUpdated({ threadID, thread });
      }
      broadcastWorkbenchUpdated({
        workbench: buildWorkbenchState(),
        threadID,
        thread
      });
    })
    .finally(() => {
      if (captureFinalizationTokens.get(task.entryID) === token) {
        captureFinalizationTokens.delete(task.entryID);
      }
    });
}

function findEntryThreadID(entryID) {
  const loaded = appService.loadWorkspace();
  const entry = loaded.home.inboxEntries.find((item) => item.id === entryID)
    ?? appService.repository?.store?.fetchEntry?.(entryID)
    ?? null;
  return entry?.threadID ?? null;
}
