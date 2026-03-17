import {
  isLocalProvider,
  providerTitle,
  validateProviderConfig
} from "../providers/providerRegistry.js";
import { createVercelAIClient } from "../adapters/vercelAiClientFactory.js";

export const DEFAULT_TEST_MESSAGE = "Say Yes.";

export class AIProviderRuntime {
  constructor({
    configStore,
    clientFactory = createVercelAIClient,
    logger = () => {}
  } = {}) {
    this.configStore = configStore;
    this.clientFactory = clientFactory;
    this.logger = logger;
    this._config = this.configStore?.load?.() ?? null;
    this._clientPromise = null;
  }

  get config() {
    return this._config;
  }

  get isConfigured() {
    return Boolean(this._config?.model && this._config?.baseURL);
  }

  get isLocal() {
    return this.isConfigured ? isLocalProvider(this._config.providerKind) : false;
  }

  get preferredMaxConcurrentRequests() {
    return this.isLocal ? 1 : 2;
  }

  get backendLabel() {
    if (!this.isConfigured) {
      return "Unconfigured provider";
    }
    return `${providerTitle(this._config.providerKind)} · ${this._config.model}`;
  }

  configure(config) {
    const resolved = validateProviderConfig(config);
    this._config = this.configStore?.save?.(resolved) ?? resolved;
    this._clientPromise = null;
    this.logger(`[AIProviderRuntime] configured ${this.backendLabel}`);
    return this._config;
  }

  async ping() {
    this.#assertConfigured();
    const client = await this.#ensureClient();
    return this.#runProbe(client, this._config);
  }

  async createTextClient() {
    this.#assertConfigured();
    return this.#ensureClient();
  }

  async pingWithConfig(config) {
    const resolved = validateProviderConfig(config);
    const client = await this.clientFactory(resolved);
    return this.#runProbe(client, resolved);
  }

  #assertConfigured() {
    if (!this.isConfigured) {
      throw new Error("AI provider runtime is not configured");
    }
  }

  async #ensureClient() {
    if (!this._clientPromise) {
      this._clientPromise = this.clientFactory(this._config);
    }
    return this._clientPromise;
  }

  async #runProbe(client, config) {
    const startedAt = Date.now();
    const result = await client.generateText({
      systemPrompt: "Answer the user's message directly and briefly.",
      userPrompt: DEFAULT_TEST_MESSAGE,
      temperature: 0
    });
    const text = String(result.text ?? "").trim();
    return {
      ok: Boolean(text),
      text,
      backendLabel: `${providerTitle(config.providerKind)} · ${config.model}`,
      latencyMS: Date.now() - startedAt,
      modelID: result.response?.modelId ?? config.model,
      probeMode: "message",
      probeMessage: DEFAULT_TEST_MESSAGE
    };
  }
}
