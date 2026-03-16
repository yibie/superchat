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
import { createMainWindow } from "./windows/windowFactory.js";
import { buildAppMenuTemplate } from "./menus/appMenu.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const preloadPath = path.join(__dirname, "bridge", "preload.js");

let shellState = null;
let workspaceManager = null;
let appService = null;
let mainWindow = null;


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
    aiService
  });
  console.error("[desktop] application service ready");
  appService.restoreWorkspace();
  shellState = bootstrapDesktopApp({ workspaceManager });
  console.error("[desktop] shell state ready", {
    workspace: shellState.workspace?.workspacePath ?? null,
    mainHTML: shellState.windows.main.htmlPath
  });

  installIPC();
  console.error("[desktop] ipc installed");
  installMenu();
  console.error("[desktop] menu installed");
  openMainWindow();
  console.error("[desktop] openMainWindow invoked");
}

function installMenu() {
  const template = buildAppMenuTemplate({
    onOpenSettings: () => showSettingsPlaceholder()
  });
  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

function installIPC() {
  ipcMain.handle("desktop:get-shell-state", () => ({
    ...shellState,
    workspace: workspaceManager.describe()
  }));
  ipcMain.handle("desktop:open-settings", async () => {
    showSettingsPlaceholder();
    return true;
  });
  ipcMain.handle("desktop:close-window", (event) => {
    BrowserWindow.fromWebContents(event.sender)?.close();
    return true;
  });
  ipcMain.handle("app:get-workbench-state", () => buildWorkbenchState());
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
    await appService.createThread(payload ?? {});
    return buildWorkbenchState();
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
  ipcMain.handle("app:submit-capture", async (_event, payload) => {
    const result = await appService.submitCapture(payload ?? {});
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: result.routingDecision?.threadID ? appService.openThread(result.routingDecision.threadID) : null
    };
  });
  ipcMain.handle("app:copy-attachment", (_event, filePath) => {
    return appService.copyAttachment(filePath);
  });
  ipcMain.handle("app:write-attachment-buffer", (_event, payload) => {
    return appService.writeAttachmentBuffer(payload ?? {});
  });
  ipcMain.handle("app:open-thread", (_event, threadID) => ({
    workbench: buildWorkbenchState(),
    thread: appService.openThread(threadID)
  }));
  ipcMain.handle("app:append-reply", async (_event, payload) => {
    const result = await appService.appendReply(payload ?? {});
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: payload?.entryID ? reopenThreadForEntry(payload.entryID) : null
    };
  });
  ipcMain.handle("app:update-entry-text", async (_event, payload) => {
    const result = await appService.updateEntryText(payload ?? {});
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: payload?.entryID ? reopenThreadForEntry(payload.entryID) : null
    };
  });
  ipcMain.handle("app:delete-entry", async (_event, entryID) => {
    const threadID = findEntryThreadID(entryID);
    await appService.deleteEntry(entryID);
    return {
      workbench: buildWorkbenchState(),
      thread: threadID ? appService.openThread(threadID) : null
    };
  });
  ipcMain.handle("app:route-entry-to-thread", async (_event, payload) => {
    await appService.routeEntryToThread(payload ?? {});
    return {
      workbench: buildWorkbenchState()
    };
  });
  ipcMain.handle("app:get-entry-rich-preview", async (_event, entryID) => {
    return appService.getEntryRichPreview(entryID);
  });
  ipcMain.handle("app:prepare-thread", async (_event, payload) => {
    return appService.prepareThread(payload ?? {});
  });
  ipcMain.handle("app:get-ai-provider-config", () => appService.getAIProviderConfig());
  ipcMain.handle("app:save-ai-provider-config", async (_event, payload) => {
    return appService.configureAIProvider(payload ?? {});
  });
  ipcMain.handle("app:save-and-ping-ai-provider", async (_event, payload) => {
    return appService.saveAndPingAIProvider(payload ?? {});
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

function showSettingsPlaceholder() {
  mainWindow?.webContents.send("desktop:open-settings");
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
    home: loaded.home,
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

function reopenThreadForEntry(entryID) {
  const threadID = findEntryThreadID(entryID);
  return threadID ? appService.openThread(threadID) : null;
}

function findEntryThreadID(entryID) {
  const loaded = appService.loadWorkspace();
  const entry = loaded.home.inboxEntries.find((item) => item.id === entryID)
    ?? appService.snapshot.entries.find((item) => item.id === entryID)
    ?? null;
  return entry?.threadID ?? null;
}
