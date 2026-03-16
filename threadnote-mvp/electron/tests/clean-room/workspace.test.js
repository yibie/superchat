import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { WorkspaceManager } from "../../src/infrastructure/persistence/workspace/workspaceManager.js";
import { AttachmentManager } from "../../src/infrastructure/persistence/workspace/attachmentManager.js";

function makeTempDir() {
  return fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-workspace-"));
}

test("clean-room workspace manager creates workspace structure and persists state", () => {
  const root = makeTempDir();
  const stateFilePath = path.join(root, "state.json");
  const manager = new WorkspaceManager({ stateFilePath });

  const workspace = manager.createWorkspace(path.join(root, "Atlas"));
  assert.equal(workspace.workspacePath.endsWith("Atlas.threadnote"), true);
  assert.equal(fs.existsSync(workspace.workspacePath), true);
  assert.equal(fs.existsSync(workspace.attachmentsPath), true);
  assert.equal(manager.databasePath.endsWith(path.join("Atlas.threadnote", "db.sqlite")), true);
  assert.equal(fs.existsSync(stateFilePath), true);
});

test("clean-room workspace manager restores only when db exists and clears stale state", () => {
  const root = makeTempDir();
  const stateFilePath = path.join(root, "state.json");
  const workspacePath = path.join(root, "Atlas.threadnote");
  fs.mkdirSync(path.join(workspacePath, "attachments"), { recursive: true });
  fs.writeFileSync(
    stateFilePath,
    JSON.stringify({
      workspacePath
    }),
    "utf8"
  );

  const coldStart = new WorkspaceManager({ stateFilePath });
  assert.equal(coldStart.isConfigured, false);
  assert.equal(fs.existsSync(stateFilePath), false);

  fs.mkdirSync(path.join(workspacePath, "attachments"), { recursive: true });
  fs.writeFileSync(path.join(workspacePath, "db.sqlite"), "", "utf8");
  fs.writeFileSync(
    stateFilePath,
    JSON.stringify({
      workspacePath
    }),
    "utf8"
  );

  const restored = new WorkspaceManager({ stateFilePath });
  assert.equal(restored.isConfigured, true);
  assert.equal(restored.workspacePath, workspacePath);
});

test("clean-room attachment manager copies by sha256 and deduplicates identical payloads", () => {
  const root = makeTempDir();
  const attachmentsPath = path.join(root, "attachments");
  const sourceA = path.join(root, "alpha.txt");
  const sourceB = path.join(root, "beta.txt");
  fs.writeFileSync(sourceA, "same payload", "utf8");
  fs.writeFileSync(sourceB, "same payload", "utf8");

  const first = AttachmentManager.copyFile(sourceA, attachmentsPath);
  const second = AttachmentManager.copyFile(sourceB, attachmentsPath);

  assert.equal(first.relativePath.startsWith("attachments/"), true);
  assert.equal(first.sha256, second.sha256);
  assert.equal(first.absolutePath, second.absolutePath);
  assert.equal(fs.readdirSync(attachmentsPath).length, 1);
});
