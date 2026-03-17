import path from "node:path";
import os from "node:os";
import fs from "node:fs/promises";
import { randomUUID } from "node:crypto";
import { fileURLToPath } from "node:url";
import { app, BrowserWindow, ipcMain } from "electron";
import { bootstrapDesktopApp } from "../src/desktop/app/bootstrap.js";
import { createMainWindow } from "../src/desktop/windows/windowFactory.js";
import { WorkspaceManager } from "../src/infrastructure/persistence/workspace/workspaceManager.js";
import { ThreadnoteApplicationService } from "../src/application/services/threadnoteApplicationService.js";
import { createEntry, EntryKind } from "../src/domain/models/threadnoteModels.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "..");
const preloadPath = path.join(repoRoot, "src/desktop/bridge/preload.js");
const outputDir = path.join(os.tmpdir(), `threadnote-thread-navigation-${Date.now()}-${randomUUID()}`);

let shellState = null;
let workspaceManager = null;
let appService = null;
let mainWindow = null;

let atlasThreadID = null;
let beaconThreadID = null;
let cometThreadID = null;
let atlasEntryID = null;
let beaconEntryID = null;
let cometEntryID = null;
let referenceEntryID = null;
let atlasBacklinkEntryID = null;

app.commandLine.appendSwitch("allow-file-access-from-files");
await app.whenReady();
await fs.mkdir(outputDir, { recursive: true });
console.error("[thread-navigation-smoke] outputDir", outputDir);

workspaceManager = new WorkspaceManager({
  stateFilePath: path.join(outputDir, "workspace-state.json")
});
appService = new ThreadnoteApplicationService({ workspaceManager });
shellState = bootstrapDesktopApp({ workspaceManager });

installIPC();

const checks = [];

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

  await waitForLoad(mainWindow);
  await ensureRendererMounted(mainWindow);
  logStep("workspace-gate");
  await waitForMain(() => Array.from(document.querySelectorAll("button")).some((button) => button.textContent?.includes("Create workspace")));

  logStep("seed-workspace");
  await seedWorkspace();
  mainWindow.reload();
  await waitForLoad(mainWindow);
  await ensureRendererMounted(mainWindow);
  logStep("stream-ready");
  await waitForMain(() => Array.from(document.querySelectorAll("aside[aria-label='Sidebar'] button")).some((button) => button.textContent?.includes("Atlas launch")));
  await waitForMain(() => Array.from(document.querySelectorAll("aside[aria-label='Sidebar'] button")).some((button) => button.textContent?.includes("Beacon rollout")));

  logStep("sidebar-beacon");
  await clickSidebarThread("Beacon rollout");
  await waitForThreadVisible("Beacon rollout", "Beacon architecture plan");
  checks.push({
    id: "sidebar-opens-correct-thread",
    result: "pass",
    evidence: "Sidebar 点击 Beacon rollout 后，thread header 与 working stream 都切到 Beacon。"
  });

  logStep("reference-path");
  await clickNavButton("Stream");
  await waitForMain(() => document.body.textContent?.includes("Cross-thread checkpoint"));
  await clickReferenceButton("Beacon architecture plan");
  await waitForThreadVisible("Beacon rollout", "Beacon architecture plan");
  checks.push({
    id: "reference-opens-target-thread",
    result: "pass",
    evidence: "点击 inline reference 后，focusEntry 进入目标 Beacon thread。"
  });

  logStep("backlink-path");
  await waitForMain((label) => Array.from(document.querySelectorAll(".entry-backlink")).some((item) => item.textContent?.includes(label)), 7000, ["Atlas depends on Beacon architecture"]);
  await clickBacklinkButton("Atlas depends on Beacon architecture");
  await waitForThreadVisible("Atlas launch", "Atlas depends on Beacon architecture");
  checks.push({
    id: "backlink-opens-source-thread",
    result: "pass",
    evidence: "点击 Beacon thread 内的 backlink 后，主视图跳回来源 Atlas thread。"
  });

  logStep("thread-badge-path");
  await clickNavButton("Stream");
  await waitForMain(() => document.body.textContent?.includes("Atlas routed note"));
  await clickThreadBadgeForEntry(atlasEntryID, "Atlas launch");
  await waitForThreadVisible("Atlas launch", "Atlas routed note");
  checks.push({
    id: "thread-badge-opens-owning-thread",
    result: "pass",
    evidence: "点击 routed entry 的 ThreadBadge 后，主视图进入 Atlas thread。"
  });

  logStep("rapid-sidebar-switch");
  await rapidSwitchThreads(["Atlas launch", "Beacon rollout", "Comet metrics"]);
  await waitForThreadVisible("Comet metrics", "Comet metrics checklist");
  checks.push({
    id: "rapid-switch-keeps-last-thread",
    result: "pass",
    evidence: "快速连续点击 Atlas -> Beacon -> Comet 后，最终 thread header 与正文都稳定停在最后一个 Comet。"
  });

  const report = { outputDir, checks };
  await fs.writeFile(path.join(outputDir, "report.json"), JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
} catch (error) {
  console.error(error);
  process.exitCode = 1;
} finally {
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
  ipcMain.handle("app:get-shortcut-settings", () => []);
  ipcMain.handle("app:update-shortcut-setting", (_event, payload) => ({
    actionId: payload?.actionId ?? null,
    attemptedRegistrationState: "unset"
  }));
  ipcMain.handle("app:clear-shortcut-setting", (event, actionId) => ({
    actionId,
    attemptedRegistrationState: "disabled"
  }));
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
  ipcMain.handle("app:submit-capture", async (_event, payload) => {
    const result = await appService.submitCapture(payload ?? {});
    return {
      result,
      workbench: buildWorkbenchState(),
      thread: result.routingDecision?.threadID ? appService.openThread(result.routingDecision.threadID) : null
    };
  });
  ipcMain.handle("app:archive-thread", async (_event, threadID) => {
    await appService.archiveThread(threadID);
    return {
      workbench: buildWorkbenchState()
    };
  });
  ipcMain.handle("app:prepare-thread", async (_event, payload) => appService.prepareThread(payload ?? {}));
  ipcMain.handle("app:get-entry-rich-preview", async (_event, entryID) => appService.getEntryRichPreview(entryID));
  ipcMain.handle("app:get-ai-provider-config", () => appService.getAIProviderConfig());
  ipcMain.handle("app:save-ai-provider-config", async (_event, payload) => appService.configureAIProvider(payload ?? {}));
  ipcMain.handle("app:test-ai-provider", async (_event, payload) => appService.testAIProvider(payload ?? {}));
  ipcMain.handle("app:ping-ai-provider", async () => appService.pingAIProvider());
  ipcMain.handle("app:open-locator", (_event, locator) => locator);
}

function logStep(step) {
  console.error("[thread-navigation-smoke]", step);
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

  const atlasThread = await appService.createThread({
    title: "Atlas launch",
    prompt: "Atlas launch"
  });
  const beaconThread = await appService.createThread({
    title: "Beacon rollout",
    prompt: "Beacon rollout"
  });
  const cometThread = await appService.createThread({
    title: "Comet metrics",
    prompt: "Comet metrics"
  });

  atlasThreadID = atlasThread.id;
  beaconThreadID = beaconThread.id;
  cometThreadID = cometThread.id;

  const beaconEntry = createEntry({
    threadID: beaconThreadID,
    kind: EntryKind.NOTE,
    body: { text: "Beacon architecture plan" },
    summaryText: "Beacon architecture plan"
  });
  await appService.repository.saveEntry(beaconEntry);
  await appService.repository.flush();

  const atlasEntry = await appService.submitCapture({
    text: "Atlas routed note",
    threadID: atlasThreadID
  });
  const cometEntry = await appService.submitCapture({
    text: "Comet metrics checklist",
    threadID: cometThreadID
  });
  const referenceEntry = await appService.submitCapture({
    text: "Cross-thread checkpoint [[Beacon architecture plan]]",
    references: [
      {
        label: "Beacon architecture plan",
        relationKind: "informs",
        targetID: beaconEntry.id
      }
    ]
  });
  const atlasBacklinkEntry = await appService.submitCapture({
    text: "Atlas depends on Beacon architecture [[supports|Beacon architecture plan]]",
    threadID: atlasThreadID,
    references: [
      {
        label: "Beacon architecture plan",
        relationKind: "supports",
        targetID: beaconEntry.id
      }
    ]
  });

  atlasEntryID = atlasEntry.entry.id;
  beaconEntryID = beaconEntry.id;
  cometEntryID = cometEntry.entry.id;
  referenceEntryID = referenceEntry.entry.id;
  atlasBacklinkEntryID = atlasBacklinkEntry.entry.id;

  appService.loadWorkspace();
}

async function clickSidebarThread(title) {
  await mainWindow.webContents.executeJavaScript(`
    (() => {
      const button = Array.from(document.querySelectorAll('aside[aria-label="Sidebar"] button'))
        .find((item) => item.textContent?.trim() === ${JSON.stringify(title)});
      if (!button) {
        throw new Error('Sidebar thread button not found: ' + ${JSON.stringify(title)});
      }
      button.click();
      return true;
    })();
  `, true);
}

async function clickNavButton(label) {
  await mainWindow.webContents.executeJavaScript(`
    (() => {
      const button = Array.from(document.querySelectorAll('nav[aria-label="Main navigation"] button'))
        .find((item) => item.textContent?.trim().includes(${JSON.stringify(label)}));
      if (!button) {
        throw new Error('Nav button not found: ' + ${JSON.stringify(label)});
      }
      button.click();
      return true;
    })();
  `, true);
}

async function clickReferenceButton(label) {
  await mainWindow.webContents.executeJavaScript(`
    (() => {
      const button = Array.from(document.querySelectorAll('button'))
        .find((item) => item.textContent?.trim() === ${JSON.stringify(label)});
      if (!button) {
        throw new Error('Reference button not found: ' + ${JSON.stringify(label)});
      }
      button.click();
      return true;
    })();
  `, true);
}

async function clickThreadBadgeForEntry(entryID, label) {
  await mainWindow.webContents.executeJavaScript(`
    ((targetEntryID, targetLabel) => {
      const card = document.querySelector('[data-entry-id="' + targetEntryID + '"]');
      if (!card) {
        throw new Error('Entry card not found: ' + targetEntryID);
      }
      const button = Array.from(card.querySelectorAll('button'))
        .find((item) => item.textContent?.trim() === targetLabel);
      if (!button) {
        throw new Error('Thread badge not found for entry: ' + targetEntryID);
      }
      button.click();
      return true;
    })(${JSON.stringify(entryID)}, ${JSON.stringify(label)});
  `, true);
}

async function clickBacklinkButton(label) {
  await mainWindow.webContents.executeJavaScript(`
    (() => {
      const button = Array.from(document.querySelectorAll('.entry-backlink'))
        .find((item) => item.textContent?.includes(${JSON.stringify(label)}));
      if (!button) {
        throw new Error('Backlink button not found: ' + ${JSON.stringify(label)});
      }
      button.click();
      return true;
    })();
  `, true);
}

async function rapidSwitchThreads(labels) {
  await mainWindow.webContents.executeJavaScript(`
    ((threadLabels) => {
      const buttons = Array.from(document.querySelectorAll('aside[aria-label="Sidebar"] button'));
      for (const label of threadLabels) {
        const button = buttons.find((item) => item.textContent?.trim() === label);
        if (!button) {
          throw new Error('Sidebar thread button not found: ' + label);
        }
        button.click();
      }
      return true;
    })(${JSON.stringify(labels)});
  `, true);
}

async function waitForThreadVisible(title, entryText, timeoutMS = 7000) {
  await waitForMain((expectedTitle, expectedEntryText) => {
    const heading = Array.from(document.querySelectorAll("h1")).find((item) => item.textContent?.trim() === expectedTitle);
    if (!heading) {
      return false;
    }
    return document.body.textContent?.includes(expectedEntryText) ?? false;
  }, timeoutMS, [title, entryText]);
}

async function waitForMain(predicate, timeoutMS = 7000, args = []) {
  return waitForRenderer(mainWindow, predicate, timeoutMS, args);
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
  throw new Error("Timed out waiting for renderer condition");
}

async function waitForLoad(window, timeoutMS = 7000) {
  const startedAt = Date.now();
  while (Date.now() - startedAt < timeoutMS) {
    if (!window || window.isDestroyed()) {
      throw new Error("Window destroyed before load");
    }
    const url = window.webContents.getURL();
    if (url) {
      return;
    }
    await pause(100);
  }
  throw new Error("Timed out waiting for window load");
}

async function ensureRendererMounted(window, timeoutMS = 7000) {
  return waitForRenderer(
    window,
    () => Boolean(document.getElementById("app")?.childElementCount),
    timeoutMS
  );
}

function pause(durationMS) {
  return new Promise((resolve) => setTimeout(resolve, durationMS));
}
