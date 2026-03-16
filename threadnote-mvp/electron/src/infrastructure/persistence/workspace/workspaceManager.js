import fs from "node:fs";
import path from "node:path";

const DEFAULT_STATE_FILE = path.join(process.cwd(), ".threadnote-workspace.json");

export class WorkspaceManager {
  constructor({ stateFilePath = DEFAULT_STATE_FILE } = {}) {
    this.stateFilePath = path.resolve(stateFilePath);
    this.workspacePath = null;
    this.restoreWorkspace();
  }

  get isConfigured() {
    return Boolean(this.workspacePath);
  }

  get databasePath() {
    return this.workspacePath ? path.join(this.workspacePath, "db.sqlite") : null;
  }

  get attachmentsPath() {
    return this.workspacePath ? path.join(this.workspacePath, "attachments") : null;
  }

  createWorkspace(workspacePath) {
    const resolved = normalizeWorkspacePath(workspacePath);
    ensureWorkspaceStructure(resolved);
    this.#saveState(resolved);
    this.workspacePath = resolved;
    return this.describe();
  }

  openWorkspace(workspacePath) {
    const resolved = normalizeWorkspacePath(workspacePath);
    if (!fs.existsSync(resolved) || !fs.statSync(resolved).isDirectory()) {
      throw new Error(`Workspace directory does not exist: ${resolved}`);
    }
    ensureWorkspaceStructure(resolved);
    this.#saveState(resolved);
    this.workspacePath = resolved;
    return this.describe();
  }

  restoreWorkspace() {
    const state = this.#readState();
    if (!state?.workspacePath) {
      this.workspacePath = null;
      return null;
    }

    const resolved = path.resolve(state.workspacePath);
    if (!isWorkspaceRestorable(resolved)) {
      this.clearWorkspaceState();
      return null;
    }

    this.workspacePath = resolved;
    return this.describe();
  }

  clearWorkspaceState() {
    this.workspacePath = null;
    if (fs.existsSync(this.stateFilePath)) {
      fs.rmSync(this.stateFilePath, { force: true });
    }
  }

  describe() {
    if (!this.workspacePath) {
      return null;
    }
    return {
      workspacePath: this.workspacePath,
      databasePath: this.databasePath,
      attachmentsPath: this.attachmentsPath
    };
  }

  #saveState(workspacePath) {
    fs.mkdirSync(path.dirname(this.stateFilePath), { recursive: true });
    fs.writeFileSync(
      this.stateFilePath,
      JSON.stringify(
        {
          workspacePath
        },
        null,
        2
      ),
      "utf8"
    );
  }

  #readState() {
    if (!fs.existsSync(this.stateFilePath)) {
      return null;
    }
    try {
      return JSON.parse(fs.readFileSync(this.stateFilePath, "utf8"));
    } catch {
      return null;
    }
  }
}

function normalizeWorkspacePath(workspacePath) {
  const trimmed = String(workspacePath ?? "").trim();
  if (!trimmed) {
    throw new Error("Workspace path is required");
  }
  return path.resolve(trimmed.endsWith(".threadnote") ? trimmed : `${trimmed}.threadnote`);
}

function ensureWorkspaceStructure(workspacePath) {
  fs.mkdirSync(workspacePath, { recursive: true });
  fs.mkdirSync(path.join(workspacePath, "attachments"), { recursive: true });
}

function isWorkspaceRestorable(workspacePath) {
  if (!fs.existsSync(workspacePath)) {
    return false;
  }
  if (!fs.statSync(workspacePath).isDirectory()) {
    return false;
  }
  const databasePath = path.join(workspacePath, "db.sqlite");
  return fs.existsSync(databasePath);
}
