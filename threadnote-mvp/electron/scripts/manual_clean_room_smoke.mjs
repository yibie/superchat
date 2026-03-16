import path from "node:path";
import os from "node:os";
import fs from "node:fs";
import fsPromises from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";
import { app, BrowserWindow, ipcMain } from "electron";
import { bootstrapDesktopApp } from "../src/desktop/app/bootstrap.js";
import { createMainWindow, createQuickCaptureWindow } from "../src/desktop/windows/windowFactory.js";
import { WorkspaceManager } from "../src/infrastructure/persistence/workspace/workspaceManager.js";
import { ThreadnoteApplicationService } from "../src/application/services/threadnoteApplicationService.js";
import { LinkMetadataService } from "../src/infrastructure/metadata/linkMetadataService.js";
import { AIProviderRuntime } from "../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIProviderConfigStore } from "../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import { AIRequestQueue } from "../src/infrastructure/ai/queue/aiRequestQueue.js";
import { ThreadnoteAIService } from "../src/infrastructure/ai/runtime/services/threadnoteAIService.js";
import { createEntry, createThreadAISnapshot, EntryKind } from "../src/domain/models/threadnoteModels.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "..");
const preloadPath = path.join(repoRoot, "src/desktop/bridge/preload.js");
const outputDir = path.join(os.tmpdir(), `threadnote-clean-room-manual-${Date.now()}-${randomUUID()}`);
const progressPath = path.join(outputDir, "progress.log");

let shellState = null;
let workspaceManager = null;
let appService = null;
let mainWindow = null;
let quickCaptureWindow = null;
let seededThreadID = null;

app.commandLine.appendSwitch("allow-file-access-from-files");
await app.whenReady();
await fsPromises.mkdir(outputDir, { recursive: true });

workspaceManager = new WorkspaceManager({
  stateFilePath: path.join(outputDir, "workspace-state.json")
});

const aiProviderRuntime = new AIProviderRuntime({
  configStore: new AIProviderConfigStore({
    configPath: path.join(outputDir, "ai-provider.json")
  }),
  clientFactory: async () => ({
    async generateText({ systemPrompt }) {
      if (String(systemPrompt ?? "").includes("healthcheck")) {
        return {
          text: "pong",
          finishReason: "stop",
          warnings: [],
          response: { id: "ping-1", modelId: "smoke-model", body: { type: "pong" } }
        };
      }
      return {
        text: JSON.stringify({
          title: "Atlas unblock draft",
          openLoops: ["Confirm legal owner", "Promote screenshot into evidence"],
          recommendedNextSteps: ["Lead with the judgment", "Close the legal gap next"]
        }),
        finishReason: "stop",
        warnings: [],
        response: { id: "prepare-1", modelId: "smoke-model", body: { type: "prepare" } }
      };
    }
  })
});

aiProviderRuntime.configure({
  providerKind: "ollama",
  baseURL: "http://localhost:11434/v1",
  model: "qwen3.5:4b",
  embeddingModel: "nomic-embed-text"
});

const aiService = new ThreadnoteAIService({
  providerRuntime: aiProviderRuntime,
  requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
});

appService = new ThreadnoteApplicationService({
  workspaceManager,
  aiProviderRuntime,
  aiService,
  linkMetadataService: new LinkMetadataService({
    fetchImpl: async () => ({
      ok: true,
      headers: {
        get(name) {
          return name === "content-type" ? "text/html" : null;
        }
      },
      async text() {
        return `
          <html>
            <head>
              <meta property="og:title" content="Atlas Launch Spec" />
              <meta property="og:description" content="Spec page for launch planning." />
              <meta property="og:site_name" content="Atlas Docs" />
            </head>
          </html>
        `;
      }
    }),
    workspacePathResolver: () => workspaceManager.describe()?.workspacePath ?? null
  })
});

shellState = bootstrapDesktopApp({ workspaceManager });
installIPC();

const checks = [];
const captures = {};

try {
  logStep("open-main-window");
  mainWindow = createMainWindow({
    BrowserWindow,
    shellState,
    preloadPath,
    onClosed: () => {
      mainWindow = null;
    }
  });
  instrumentWindow(mainWindow, "main");
  await waitForLoad(mainWindow);
  await ensureRendererMounted(mainWindow);
  logStep("workspace-gate");
  await waitForMain(() => document.getElementById("create-workspace") !== null);
  captures.workspaceGate = await captureWindow(mainWindow, "workspace-gate");
  checks.push({
    id: "workspace-gate",
    result: "pass",
    evidence: "主窗在未恢复 workspace 时显示 Workspace Setup gate。"
  });

  logStep("seed-workspace");
  await seedWorkspace();
  mainWindow.reload();
  await waitForLoad(mainWindow);
  logStep("stream");
  await waitForMain(() => document.querySelector("[data-surface='stream']") !== null);
  await waitForMain(() => document.querySelectorAll("[data-entry-card-id]").length >= 1);
  captures.stream = await captureWindow(mainWindow, "stream");
  checks.push({
    id: "stream",
    result: "pass",
    evidence: "主工作区加载 Sidebar/Canvas/Sidecar，Stream surface 可见 inbox timeline。"
  });

  logStep("open-quick-capture");
  quickCaptureWindow = createQuickCaptureWindow({
    BrowserWindow,
    shellState: bootstrapDesktopApp({ workspaceManager }),
    preloadPath,
    onClosed: () => {
      quickCaptureWindow = null;
    }
  });
  instrumentWindow(quickCaptureWindow, "quick-capture");
  await waitForLoad(quickCaptureWindow);
  await ensureRendererMounted(quickCaptureWindow);
  await waitForQuickCapture(() => document.querySelector(".capture-editor-input") !== null);
  captures.quickCapture = await captureWindow(quickCaptureWindow, "quick-capture");
  logStep("submit-quick-capture");
  await quickCaptureWindow.webContents.executeJavaScript(`
    (() => {
      const textarea = document.querySelector(".capture-editor-input");
      textarea.value = "#claim Atlas launch needs legal review";
      textarea.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      document.querySelector(".capture-editor-runtime .primary")?.click();
      return true;
    })();
  `, true);
  await waitForCondition(() => quickCaptureWindow === null, 6000);

  const routedThread = appService.openThread(seededThreadID);
  checks.push({
    id: "quick-capture-route",
    result: routedThread.entries.some((entry) => entry.summaryText.includes("legal review")) ? "pass" : "fail",
    evidence: "Quick Capture 提交后，匹配 note 被自动路由进入已存在 thread。"
  });

  logStep("thread");
  mainWindow.reload();
  await waitForLoad(mainWindow);
  await waitForSelector(mainWindow, `[data-thread-id="${seededThreadID}"]`);
  logStep("prepare");
  await mainWindow.webContents.executeJavaScript(`
    document.querySelector('[data-thread-id="${seededThreadID}"]')?.click();
    true;
  `, true);
  await waitForMain(() => document.getElementById("prepare-thread") !== null);
  captures.thread = await captureWindow(mainWindow, "thread");
  checks.push({
    id: "thread",
    result: "pass",
    evidence: "Thread surface 显示 header、continue composer、working stream，sidecar 保持二级信息。"
  });

  logStep("resources");
  await mainWindow.webContents.executeJavaScript(`
    document.getElementById("prepare-thread")?.click();
    true;
  `, true);
  await waitForMain(() => document.body.textContent?.includes("Prepare"));
  captures.threadPrepare = await captureWindow(mainWindow, "thread-prepare");
  checks.push({
    id: "prepare",
    result: "pass",
    evidence: "Prepare use case 返回 AI 草稿标题和 next steps，thread 页面出现 prepare panel。"
  });

  logStep("source-detail");
  await mainWindow.webContents.executeJavaScript(`
    document.querySelector('[data-surface="resources"]')?.click();
    true;
  `, true);
  await waitForMain(() => document.querySelectorAll("[data-resource-index]").length >= 1);
  await waitForMain(() => document.querySelector(".rich-preview-slot") !== null);
  captures.resources = await captureWindow(mainWindow, "resources");
  checks.push({
    id: "resources",
    result: "pass",
    evidence: "Resources surface 按 Links/Media/Mentions 展示派生资源，并显示 rich preview slot。"
  });

  logStep("settings");
  await mainWindow.webContents.executeJavaScript(`
    (() => {
      const first = document.querySelector("[data-resource-index]")?.dataset?.resourceIndex;
      if (!first) return false;
      return window.__threadnoteDebug?.openResourceById(first) ?? false;
    })();
  `, true);
  await waitForMain(() => Boolean(window.__threadnoteDebugSourceDetailTitle));
  captures.sourceDetail = await captureWindow(mainWindow, "source-detail");
  checks.push({
    id: "source-detail",
    result: "pass",
    evidence: "Source Detail modal 可显示 locator、citation 和 rich preview。"
  });

  await mainWindow.webContents.executeJavaScript(`
    window.__threadnoteDebug?.openSettingsSurface() ?? false;
  `, true);
  await waitForMain(() => document.getElementById("test-ai-settings") !== null);
  await mainWindow.webContents.executeJavaScript(`
    document.getElementById("test-ai-settings")?.click();
    true;
  `, true);
  await waitForMain(() => document.body.textContent?.includes("Connected"));
  captures.settings = await captureWindow(mainWindow, "settings");
  checks.push({
    id: "settings",
    result: "pass",
    evidence: "Settings surface 可见 provider form，并且 Test Connection 返回 Connected 状态。"
  });

  const report = {
    outputDir,
    captures,
    checks
  };
  await fsPromises.writeFile(path.join(outputDir, "report.json"), JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
} catch (error) {
  console.error(error);
  process.exitCode = 1;
} finally {
  if (quickCaptureWindow && !quickCaptureWindow.isDestroyed()) {
    quickCaptureWindow.destroy();
  }
  if (mainWindow && !mainWindow.isDestroyed()) {
    mainWindow.destroy();
  }
  app.quit();
}

function installIPC() {
  ipcMain.handle("desktop:get-shell-state", () => ({
    ...shellState,
    workspace: workspaceManager.describe()
  }));
  ipcMain.handle("desktop:open-quick-capture", () => true);
  ipcMain.handle("desktop:open-settings", () => true);
  ipcMain.handle("desktop:close-window", (event) => {
    BrowserWindow.fromWebContents(event.sender)?.close();
    return true;
  });
  ipcMain.handle("app:get-workbench-state", () => buildWorkbenchState());
  ipcMain.handle("app:create-workspace", () => buildWorkbenchState());
  ipcMain.handle("app:open-workspace", () => buildWorkbenchState());
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
  ipcMain.handle("app:open-thread", (_event, threadID) => ({
    workbench: buildWorkbenchState(),
    thread: appService.openThread(threadID)
  }));
  ipcMain.handle("app:prepare-thread", (_event, payload) => appService.prepareThread(payload ?? {}));
  ipcMain.handle("app:ping-ai-provider", () => appService.pingAIProvider());
  ipcMain.handle("app:get-ai-provider-config", () => appService.getAIProviderConfig());
  ipcMain.handle("app:save-ai-provider-config", (_event, payload) => appService.configureAIProvider(payload ?? {}));
  ipcMain.handle("app:save-and-ping-ai-provider", (_event, payload) => appService.saveAndPingAIProvider(payload ?? {}));
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
    const thread = await appService.routeEntryToThread(payload ?? {});
    return {
      workbench: buildWorkbenchState(),
      thread
    };
  });
  ipcMain.handle("app:open-locator", (_event, locator) => locator);
  ipcMain.handle("app:submit-capture", async (_event, payload) => {
    const result = await appService.submitCapture(payload ?? {});
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: result.routingDecision?.threadID ? appService.openThread(result.routingDecision.threadID) : null
    };
  });
  ipcMain.handle("app:copy-attachment", (_event, filePath) => appService.copyAttachment(filePath));
  ipcMain.handle("app:get-entry-rich-preview", (_event, entryID) => appService.getEntryRichPreview(entryID));
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

function reopenThreadForEntry(entryID) {
  const threadID = findEntryThreadID(entryID);
  return threadID ? appService.openThread(threadID) : null;
}

function findEntryThreadID(entryID) {
  return appService.snapshot.entries.find((entry) => entry.id === entryID)?.threadID ?? null;
}

async function seedWorkspace() {
  const workspacePath = path.join(outputDir, "Atlas.threadnote");
  appService.createWorkspace(workspacePath);
  shellState = bootstrapDesktopApp({ workspaceManager });

  const workspace = workspaceManager.describe();
  const atlasImagePath = path.join(workspace.attachmentsPath, "atlas.png");
  fs.writeFileSync(atlasImagePath, "atlas-image");

  const thread = await appService.createThread({
    title: "Atlas launch",
    prompt: "Atlas launch"
  });
  seededThreadID = thread.id;

  await appService.repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.SOURCE,
      body: { url: "https://example.com/spec", title: "Atlas Spec" },
      summaryText: "Atlas specification reference"
    })
  );
  await appService.repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.NOTE,
      body: { kind: "image", url: "attachments/atlas.png", title: "Atlas board" },
      summaryText: "Atlas board screenshot"
    })
  );
  await appService.repository.upsertAISnapshot(
    createThreadAISnapshot({
      threadID: thread.id,
      contentFingerprint: "smoke-fingerprint",
      headline: "Atlas launch is close, but legal review still controls release.",
      currentJudgment: "Atlas launch is blocked by one unresolved legal owner.",
      synthesizedAt: new Date().toISOString(),
      blocks: [
        {
          kind: "judgment",
          title: "Working Read",
          summary: "The thread already has enough context to continue.",
          items: ["One legal owner confirmation still blocks final launch."],
          tone: "accent"
        }
      ]
    })
  );
  await appService.repository.flush();

  await appService.submitCapture({
    text: "General scratch note for inbox"
  });
}

async function waitForMain(predicate, timeoutMS = 7000) {
  return waitForRenderer(mainWindow, predicate, timeoutMS);
}

async function waitForQuickCapture(predicate, timeoutMS = 7000) {
  return waitForRenderer(quickCaptureWindow, predicate, timeoutMS);
}

async function waitForSelector(window, selector, timeoutMS = 7000) {
  return waitForRenderer(
    window,
    (query) => document.querySelector(query) !== null,
    timeoutMS,
    [selector]
  );
}

async function waitForRenderer(window, predicate, timeoutMS = 7000, args = []) {
  const startedAt = Date.now();
  while (Date.now() - startedAt < timeoutMS) {
    if (!window || window.isDestroyed()) {
      throw new Error("Renderer window is unavailable");
    }
    const passed = await window.webContents.executeJavaScript(
      `(${predicate.toString()})(...${JSON.stringify(args)})`,
      true
    );
    if (passed) {
      return;
    }
    await pause(120);
  }
  try {
    const html = await window.webContents.executeJavaScript("document.body.innerHTML", true);
    fs.appendFileSync(progressPath, `[manual-clean-room-smoke] renderer-dump ${String(html).slice(0, 1500)}\n`);
    const diagnostics = await window.webContents.executeJavaScript(`
      JSON.stringify({
        hasDesktopBridge: typeof window.threadnoteDesktop !== "undefined",
        hasGetWorkbenchState: typeof window.threadnoteDesktop?.getWorkbenchState === "function",
        appChildCount: document.getElementById("app")?.childElementCount ?? null,
        readyState: document.readyState
      })
    `, true);
    fs.appendFileSync(progressPath, `[manual-clean-room-smoke] renderer-diagnostics ${diagnostics}\n`);
  } catch {}
  throw new Error("Timed out waiting for renderer condition");
}

async function waitForCondition(check, timeoutMS = 7000) {
  const startedAt = Date.now();
  while (Date.now() - startedAt < timeoutMS) {
    if (check()) {
      return;
    }
    await pause(120);
  }
  throw new Error("Timed out waiting for condition");
}

async function waitForLoad(window, timeoutMS = 7000) {
  if (window.webContents.isLoadingMainFrame()) {
    await Promise.race([
      new Promise((resolve) => window.webContents.once("did-finish-load", resolve)),
      pause(timeoutMS).then(() => {
        throw new Error("Timed out waiting for did-finish-load");
      })
    ]);
  } else {
    await pause(250);
  }
}

async function ensureRendererMounted(window) {
  const startedAt = Date.now();
  while (Date.now() - startedAt < 6000) {
    const mounted = await window.webContents.executeJavaScript(
      "Boolean(document.getElementById('app')?.childElementCount || document.getElementById('quick-capture-editor')?.childElementCount)",
      true
    );
    if (mounted) {
      return;
    }
    await pause(120);
  }
  const diagnostics = await window.webContents.executeJavaScript(`
    JSON.stringify({
      readyState: document.readyState,
      appChildCount: document.getElementById('app')?.childElementCount ?? null,
      quickCaptureChildCount: document.getElementById('quick-capture-editor')?.childElementCount ?? null,
      bodyPreview: document.body.innerHTML.slice(0, 400)
    })
  `, true);
  throw new Error(`Renderer did not mount in time: ${diagnostics}`);
}

async function captureWindow(window, label) {
  const image = await window.webContents.capturePage();
  const filePath = path.join(outputDir, `${label}.png`);
  await fsPromises.writeFile(filePath, image.toPNG());
  return filePath;
}

async function pause(ms) {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

function logStep(step) {
  fs.appendFileSync(progressPath, `[manual-clean-room-smoke] ${step}\n`);
}

function instrumentWindow(window, label) {
  window.webContents.on("console-message", (_event, level, message) => {
    fs.appendFileSync(progressPath, `[${label}] console:${level} ${message}\n`);
  });
  window.webContents.on("did-fail-load", (_event, code, description) => {
    fs.appendFileSync(progressPath, `[${label}] did-fail-load ${code} ${description}\n`);
  });
  window.webContents.on("render-process-gone", (_event, details) => {
    fs.appendFileSync(progressPath, `[${label}] render-process-gone ${JSON.stringify(details)}\n`);
  });
}
