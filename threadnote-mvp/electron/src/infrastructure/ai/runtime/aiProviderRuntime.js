import {
  isLocalProvider,
  providerTitle,
  validateProviderConfig
} from "../providers/providerRegistry.js";
import { performance } from "node:perf_hooks";
import { createVercelAIClient } from "../adapters/vercelAiClientFactory.js";

export const DEFAULT_TEST_MESSAGE = "Say Yes.";
export const DEFAULT_LOCAL_PREWARM_INTERVAL_MS = 8 * 60 * 1000;

export class AIProviderRuntime {
  constructor({
    configStore,
    clientFactory = createVercelAIClient,
    logger = () => {},
    setIntervalFn = globalThis.setInterval,
    clearIntervalFn = globalThis.clearInterval
  } = {}) {
    this.configStore = configStore;
    this.clientFactory = clientFactory;
    this.logger = logger;
    this.setIntervalFn = setIntervalFn;
    this.clearIntervalFn = clearIntervalFn;
    this._config = this.configStore?.load?.() ?? null;
    this._clientPromise = null;
    this._prewarmPromise = null;
    this._keepWarmTimer = null;
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
    this._prewarmPromise = null;
    if (!this.isLocal) {
      this.stopLocalKeepWarm();
    }
    this.logger(`[AIProviderRuntime] configured ${this.backendLabel}`);
    return this._config;
  }

  async ping({ signal = null } = {}) {
    this.#assertConfigured();
    const probe = await this.pingWithTelemetry({ signal });
    return probe.result;
  }

  async createTextClient() {
    this.#assertConfigured();
    return this.#ensureClient();
  }

  async createTextClientWithTelemetry() {
    this.#assertConfigured();
    const startedAt = performance.now();
    const coldStart = !this._clientPromise;
    const client = await this.#ensureClient();
    return {
      client,
      telemetry: {
        clientCreateMS: performance.now() - startedAt,
        coldStart
      }
    };
  }

  async pingWithConfig(config, { signal = null } = {}) {
    const resolved = validateProviderConfig(config);
    const client = await this.clientFactory(resolved);
    return this.#runProbe(client, resolved, { signal });
  }

  async pingWithTelemetry({ signal = null } = {}) {
    this.#assertConfigured();
    const totalStartedAt = performance.now();
    const systemPrompt = "Answer the user's message directly and briefly.";
    const promptBytes = Buffer.byteLength(`${systemPrompt}\n${DEFAULT_TEST_MESSAGE}`, "utf8");
    const { client, telemetry } = await this.createTextClientWithTelemetry();
    const providerStartedAt = performance.now();
    const result = await this.#runProbe(client, this._config, { signal });
    const providerCallMS = performance.now() - providerStartedAt;
    const text = String(result.text ?? "");
    return {
      result: {
        ...result,
        queueWaitMS: 0,
        clientCreateMS: telemetry.clientCreateMS,
        providerCallMS,
        totalMS: performance.now() - totalStartedAt,
        promptBytes,
        responseBytes: Buffer.byteLength(text, "utf8"),
        responseLength: text.length,
        coldStartClient: telemetry.coldStart
      }
    };
  }

  prewarmIfLocal({ reason = "local-prewarm" } = {}) {
    if (!this.isConfigured || !this.isLocal) {
      return null;
    }
    if (!this._prewarmPromise) {
      this.logger(`[AIProviderRuntime] prewarm start ${this.backendLabel} (${reason})`);
      this._prewarmPromise = this.pingWithTelemetry()
        .then((result) => {
          this.logger(
            `[AIProviderRuntime] prewarm ok ${this.backendLabel} totalMS=${Math.round(result.result.totalMS)} (${reason})`
          );
          return result;
        })
        .catch((error) => {
          this.logger(
            `[AIProviderRuntime] prewarm failed ${this.backendLabel} reason=${reason} error=${error?.message ?? error}`
          );
          throw error;
        })
        .finally(() => {
          this._prewarmPromise = null;
        });
    }
    return this._prewarmPromise;
  }

  startLocalKeepWarm({ intervalMS = DEFAULT_LOCAL_PREWARM_INTERVAL_MS } = {}) {
    if (!this.isConfigured || !this.isLocal || this._keepWarmTimer || typeof this.setIntervalFn !== "function") {
      return false;
    }
    const normalizedInterval = Math.max(30_000, Number(intervalMS) || DEFAULT_LOCAL_PREWARM_INTERVAL_MS);
    this._keepWarmTimer = this.setIntervalFn(() => {
      void this.prewarmIfLocal({ reason: "keep-warm-interval" }).catch(() => {});
    }, normalizedInterval);
    this._keepWarmTimer?.unref?.();
    this.logger(`[AIProviderRuntime] keep-warm started ${this.backendLabel} intervalMS=${normalizedInterval}`);
    return true;
  }

  stopLocalKeepWarm() {
    if (!this._keepWarmTimer) {
      return false;
    }
    if (typeof this.clearIntervalFn === "function") {
      this.clearIntervalFn(this._keepWarmTimer);
    }
    this._keepWarmTimer = null;
    this.logger(`[AIProviderRuntime] keep-warm stopped ${this.backendLabel}`);
    return true;
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

  async #runProbe(client, config, { signal = null } = {}) {
    const startedAt = Date.now();
    const result = await client.generateText({
      systemPrompt: "Answer the user's message directly and briefly.",
      userPrompt: DEFAULT_TEST_MESSAGE,
      temperature: 0,
      signal
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
