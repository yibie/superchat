import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { bootstrapDesktopApp, buildRendererURL } from "../../src/desktop/app/bootstrap.js";
import { buildAppMenuTemplate } from "../../src/desktop/menus/appMenu.js";
import { ThreadnoteApplicationService } from "../../src/application/services/threadnoteApplicationService.js";
import { WorkspaceManager } from "../../src/infrastructure/persistence/workspace/workspaceManager.js";
import { ShortcutStore } from "../../src/desktop/shortcuts/shortcutStore.js";
import { ShortcutService } from "../../src/desktop/shortcuts/shortcutService.js";
import { ShortcutActionID } from "../../src/desktop/shortcuts/shortcutModel.js";

test("clean-room desktop bootstrap builds main shell", () => {
  const shellState = bootstrapDesktopApp({
    workspaceManager: {
      describe() {
        return { workspacePath: "/tmp/Atlas.threadnote" };
      }
    }
  });

  assert.equal(shellState.status, "ready");
  assert.equal(shellState.windows.main.width, 1280);
  assert.equal(shellState.windows.settings.width, 760);
  assert.equal(shellState.workspace.workspacePath, "/tmp/Atlas.threadnote");
});

test("clean-room desktop bootstrap appends query params to renderer url", () => {
  const url = buildRendererURL("/tmp/index.html", { surface: "main", workspace: "ready" });
  assert.match(url, /surface=main/);
  assert.match(url, /workspace=ready/);
});

test("clean-room desktop menu includes settings shortcut", () => {
  const template = buildAppMenuTemplate({
    onOpenSettings() {},
    quickCaptureAccelerator: "CommandOrControl+Shift+1",
    settingsAccelerator: "CommandOrControl+,"
  });

  const appMenu = template[0].submenu.filter((item) => item?.label);
  const quickCaptureItem = appMenu.find((item) => item.label === "Quick Capture");
  assert.equal(quickCaptureItem?.accelerator, "CommandOrControl+Shift+1");
  const settingsItem = appMenu.find((item) => item.label === "Settings...");
  assert.equal(settingsItem?.label, "Settings...");
  assert.equal(settingsItem?.accelerator, "CommandOrControl+,");
  const editMenu = template.find((item) => item.label === "Edit");
  assert.equal(editMenu?.submenu[4].role, "copy");
  assert.equal(editMenu?.submenu[5].role, "paste");
  const viewMenu = template.find((item) => item.label === "View");
  assert.equal(viewMenu?.submenu[0].role, "reload");
  assert.equal(viewMenu?.submenu[1].role, "forceReload");
  assert.equal(viewMenu?.submenu[2].role, "toggleDevTools");
});

test("clean-room application shell enters stream after workspace creation", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-workbench-"));
  const service = new ThreadnoteApplicationService({
    workspaceManager: new WorkspaceManager({
      stateFilePath: path.join(root, "workspace-state.json")
    })
  });

  assert.equal(service.restoreWorkspace(), null);
  const loaded = service.createWorkspace(path.join(root, "Atlas"));
  assert.equal(Boolean(loaded.workspace.workspacePath), true);
  assert.equal(Array.isArray(loaded.home.threads), true);
});

test("clean-room shortcut service suggests first available quick capture shortcut on initial setup", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-shortcut-service-"));
  const store = new ShortcutStore({
    configPath: path.join(root, "shortcuts.json")
  });
  const registered = new Set();
  const unavailable = new Set(["CommandOrControl+Shift+1"]);
  const service = new ShortcutService({
    store,
    onOpenQuickCapture() {},
    onOpenSettingsWindow() {},
    globalShortcutImpl: {
      register(accelerator) {
        if (unavailable.has(accelerator)) {
          return false;
        }
        registered.add(accelerator);
        return true;
      },
      unregister(accelerator) {
        registered.delete(accelerator);
      },
      unregisterAll() {
        registered.clear();
      }
    },
  });

  const settings = service.initialize();
  const state = settings.find((item) => item.actionId === ShortcutActionID.QUICK_CAPTURE);
  assert.equal(state.registrationState, "registered");
  assert.equal(state.accelerator, "CommandOrControl+Shift+2");
  assert.equal(store.load().quickCapture.accelerator, "CommandOrControl+Shift+2");
});

test("clean-room shortcut service reports conflict without overwriting existing quick capture shortcut", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-shortcut-service-"));
  const store = new ShortcutStore({
    configPath: path.join(root, "shortcuts.json")
  });
  const persisted = store.load();
  persisted.quickCapture.accelerator = "CommandOrControl+Shift+2";
  persisted.quickCapture.enabled = true;
  store.save(persisted);
  const registered = new Set();
  const unavailable = new Set(["CommandOrControl+Shift+9"]);
  const service = new ShortcutService({
    store,
    onOpenQuickCapture() {},
    onOpenSettingsWindow() {},
    globalShortcutImpl: {
      register(accelerator) {
        if (unavailable.has(accelerator)) {
          return false;
        }
        registered.add(accelerator);
        return true;
      },
      unregister(accelerator) {
        registered.delete(accelerator);
      },
      unregisterAll() {
        registered.clear();
      }
    },
  });

  service.initialize();
  const result = service.updateShortcutSetting(ShortcutActionID.QUICK_CAPTURE, "CommandOrControl+Shift+9");
  assert.equal(result.attemptedRegistrationState, "conflict");
  assert.equal(store.load().quickCapture.accelerator, "CommandOrControl+Shift+2");
  assert.equal(service.getShortcutState(ShortcutActionID.QUICK_CAPTURE).registrationState, "registered");
  assert.equal(service.getShortcutState(ShortcutActionID.QUICK_CAPTURE).accelerator, "CommandOrControl+Shift+2");
});

test("clean-room shortcut service rejects duplicate app shortcut assignments", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-shortcut-service-"));
  const service = new ShortcutService({
    store: new ShortcutStore({ configPath: path.join(root, "shortcuts.json") }),
    onOpenQuickCapture() {},
    onOpenSettingsWindow() {},
    globalShortcutImpl: {
      register() { return true; },
      unregister() {},
      unregisterAll() {}
    }
  });

  service.initialize();
  const result = service.updateShortcutSetting(ShortcutActionID.GO_TO_RESOURCES, "CommandOrControl+1");
  assert.equal(result.attemptedRegistrationState, "conflict");
  assert.equal(result.conflictingActionId, ShortcutActionID.GO_TO_STREAM);
});
