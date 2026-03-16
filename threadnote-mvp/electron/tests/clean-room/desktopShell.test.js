import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { bootstrapDesktopApp, buildRendererURL } from "../../src/desktop/app/bootstrap.js";
import { buildAppMenuTemplate } from "../../src/desktop/menus/appMenu.js";
import { ThreadnoteApplicationService } from "../../src/application/services/threadnoteApplicationService.js";
import { WorkspaceManager } from "../../src/infrastructure/persistence/workspace/workspaceManager.js";

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
  assert.equal(shellState.workspace.workspacePath, "/tmp/Atlas.threadnote");
});

test("clean-room desktop bootstrap appends query params to renderer url", () => {
  const url = buildRendererURL("/tmp/index.html", { surface: "main", workspace: "ready" });
  assert.match(url, /surface=main/);
  assert.match(url, /workspace=ready/);
});

test("clean-room desktop menu includes settings shortcut", () => {
  const template = buildAppMenuTemplate({
    onOpenSettings() {}
  });

  assert.equal(template[0].submenu[0].label, "Settings...");
  assert.equal(template[0].submenu[0].accelerator, "CmdOrCtrl+,");
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
