import fs from "node:fs";
import path from "node:path";
import { defaultShortcutSettings, normalizeShortcutSettings } from "./shortcutModel.js";

export class ShortcutStore {
  constructor({ configPath, legacyQuickCapturePath = null }) {
    this.configPath = path.resolve(configPath);
    this.legacyQuickCapturePath = legacyQuickCapturePath ? path.resolve(legacyQuickCapturePath) : null;
  }

  load() {
    if (fs.existsSync(this.configPath)) {
      try {
        return normalizeShortcutSettings(JSON.parse(fs.readFileSync(this.configPath, "utf8")));
      } catch {
        return defaultShortcutSettings();
      }
    }

    const defaults = defaultShortcutSettings();
    const migrated = this.#loadLegacyQuickCapture();
    if (migrated) {
      defaults.quickCapture = {
        ...defaults.quickCapture,
        accelerator: migrated.accelerator,
        enabled: migrated.enabled !== false && migrated.accelerator != null,
        scope: "global",
        actionId: "quickCapture"
      };
    }
    return defaults;
  }

  save(settings) {
    const resolved = normalizeShortcutSettings(settings);
    fs.mkdirSync(path.dirname(this.configPath), { recursive: true });
    fs.writeFileSync(this.configPath, JSON.stringify(resolved, null, 2), "utf8");
    return resolved;
  }

  #loadLegacyQuickCapture() {
    if (!this.legacyQuickCapturePath || !fs.existsSync(this.legacyQuickCapturePath)) {
      return null;
    }
    try {
      return JSON.parse(fs.readFileSync(this.legacyQuickCapturePath, "utf8"));
    } catch {
      return null;
    }
  }
}
