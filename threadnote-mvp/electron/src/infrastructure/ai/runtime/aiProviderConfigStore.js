import fs from "node:fs";
import path from "node:path";
import { resolveProviderConfig } from "../providers/providerRegistry.js";

const DEFAULT_CONFIG_PATH = path.join(process.cwd(), ".threadnote-ai-provider.json");

export class AIProviderConfigStore {
  constructor({ configPath = DEFAULT_CONFIG_PATH } = {}) {
    this.configPath = path.resolve(configPath);
  }

  load() {
    if (!fs.existsSync(this.configPath)) {
      return null;
    }
    try {
      return resolveProviderConfig(JSON.parse(fs.readFileSync(this.configPath, "utf8")));
    } catch {
      return null;
    }
  }

  save(config) {
    const resolved = resolveProviderConfig(config);
    fs.mkdirSync(path.dirname(this.configPath), { recursive: true });
    fs.writeFileSync(this.configPath, JSON.stringify(resolved, null, 2), "utf8");
    return resolved;
  }
}
