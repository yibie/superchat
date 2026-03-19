import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  AIProviderRuntime,
  DEFAULT_TEST_MESSAGE
} from "../../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIProviderConfigStore } from "../../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import {
  buildOllamaGenerateURL,
  createOllamaGenerateBody,
  createOllamaNativeClient,
  createModelHandle,
  resolveOllamaKeepAlive,
  resolveOllamaThinkSetting,
  resolveOpenAICompatibleAPIKey
} from "../../src/infrastructure/ai/adapters/vercelAiClientFactory.js";

function makeTempPath(filename) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-ai-runtime-"));
  return path.join(directory, filename);
}

test("clean-room ai provider runtime allows local config without api key and persists it", () => {
  const configPath = makeTempPath("provider.json");
  const store = new AIProviderConfigStore({ configPath });
  const runtime = new AIProviderRuntime({
    configStore: store,
    clientFactory: async () => ({
      async generateText() {
        return { text: "pong", response: { modelId: "mock-local" } };
      }
    })
  });

  runtime.configure({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:4b"
  });

  assert.equal(runtime.isConfigured, true);
  assert.equal(runtime.isLocal, true);
  assert.equal(runtime.preferredMaxConcurrentRequests, 1);
  assert.equal(fs.existsSync(configPath), true);
  assert.equal(store.load().providerKind, "ollama");
});

test("clean-room ai provider runtime rejects cloud config without api key", () => {
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => ({
      async generateText() {
        return { text: "pong", response: { modelId: "mock-cloud" } };
      }
    })
  });

  assert.throws(
    () =>
      runtime.configure({
        providerKind: "openAI",
        baseURL: "https://api.openai.com/v1",
        model: "gpt-4.1-mini"
      }),
    /requires apiKey/
  );
});

test("clean-room ai provider runtime pings configured backend and caches client", async () => {
  let createCalls = 0;
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => {
      createCalls += 1;
      return {
        async generateText() {
          return {
            text: "pong",
            response: { modelId: "mock-model" }
          };
        }
      };
    }
  });

  runtime.configure({
    providerKind: "openAICompat",
    baseURL: "http://localhost:8000/v1",
    model: "mock"
  });

  const first = await runtime.ping();
  const second = await runtime.createTextClient();

  assert.equal(first.ok, true);
  assert.equal(first.modelID, "mock-model");
  assert.equal(runtime.backendLabel, "OpenAI-Compatible · mock");
  assert.equal(Boolean(second), true);
  assert.equal(createCalls, 1);
});

test("clean-room ai provider runtime exposes ping telemetry with cold-start info", async () => {
  let createCalls = 0;
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => {
      createCalls += 1;
      return {
        async generateText() {
          return {
            text: "pong",
            response: { modelId: "mock-model" }
          };
        }
      };
    }
  });

  runtime.configure({
    providerKind: "openAICompat",
    baseURL: "http://localhost:8000/v1",
    model: "mock"
  });

  const first = await runtime.pingWithTelemetry();
  const second = await runtime.pingWithTelemetry();

  assert.equal(first.result.ok, true);
  assert.equal(first.result.clientCreateMS >= 0, true);
  assert.equal(first.result.providerCallMS >= 0, true);
  assert.equal(first.result.totalMS >= first.result.providerCallMS, true);
  assert.equal(first.result.responseBytes > 0, true);
  assert.equal(first.result.coldStartClient, true);
  assert.equal(second.result.coldStartClient, false);
  assert.equal(createCalls, 1);
});

test("clean-room ai provider runtime prewarm deduplicates concurrent local warmups", async () => {
  let calls = 0;
  let release = null;
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => ({
      async generateText() {
        calls += 1;
        await new Promise((resolve) => {
          release = resolve;
        });
        return {
          text: "pong",
          response: { modelId: "mock-local" }
        };
      }
    })
  });

  runtime.configure({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:9b"
  });

  const first = runtime.prewarmIfLocal({ reason: "test-1" });
  const second = runtime.prewarmIfLocal({ reason: "test-2" });
  assert.equal(first, second);
  await new Promise((resolve) => setImmediate(resolve));
  release();
  await first;
  assert.equal(calls, 1);
});

test("clean-room ai provider runtime starts and stops local keep-warm timer only for local providers", () => {
  const intervals = [];
  const cleared = [];
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => ({
      async generateText() {
        return { text: "pong", response: { modelId: "mock-local" } };
      }
    }),
    setIntervalFn(handler, intervalMS) {
      const timer = {
        handler,
        intervalMS,
        unrefCalled: false,
        unref() {
          this.unrefCalled = true;
        }
      };
      intervals.push(timer);
      return timer;
    },
    clearIntervalFn(timer) {
      cleared.push(timer);
    }
  });

  runtime.configure({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:9b"
  });
  assert.equal(runtime.startLocalKeepWarm({ intervalMS: 60000 }), true);
  assert.equal(runtime.startLocalKeepWarm({ intervalMS: 60000 }), false);
  assert.equal(intervals.length, 1);
  assert.equal(intervals[0].intervalMS, 60000);
  assert.equal(intervals[0].unrefCalled, true);
  assert.equal(runtime.stopLocalKeepWarm(), true);
  assert.equal(cleared.length, 1);

  runtime.configure({
    providerKind: "openAI",
    baseURL: "https://api.openai.com/v1",
    model: "mock-cloud",
    apiKey: "sk-test"
  });
  assert.equal(runtime.startLocalKeepWarm(), false);
});

test("clean-room ai provider runtime tests ad hoc config without mutating saved runtime config", async () => {
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async (config) => ({
      async generateText() {
        return {
          text: "pong",
          response: { modelId: `${config.model}-response` }
        };
      }
    })
  });

  runtime.configure({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "saved-model"
  });

  const tested = await runtime.pingWithConfig({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "test-model"
  });

  assert.equal(tested.ok, true);
  assert.equal(tested.backendLabel, "Ollama (Local) · test-model");
  assert.equal(tested.modelID, "test-model-response");
  assert.equal(runtime.config.model, "saved-model");
});

test("clean-room ai provider runtime sends custom probe message when provided", async () => {
  const prompts = [];
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => ({
      async generateText(input) {
        prompts.push(input);
        return {
          text: "hello from model",
          response: { modelId: "mock-model" }
        };
      }
    })
  });

  const tested = await runtime.pingWithConfig({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/api",
    model: "test-model"
  });

  assert.equal(prompts[0].userPrompt, DEFAULT_TEST_MESSAGE);
  assert.equal(tested.ok, true);
  assert.equal(tested.probeMode, "message");
  assert.equal(tested.probeMessage, DEFAULT_TEST_MESSAGE);
  assert.equal(tested.text, "hello from model");
});

test("clean-room ai provider runtime requires config before ping", async () => {
  const runtime = new AIProviderRuntime({
    configStore: new AIProviderConfigStore({ configPath: makeTempPath("provider.json") }),
    clientFactory: async () => ({
      async generateText() {
        return {
          text: "pong",
          response: { modelId: "mock-model" }
        };
      }
    })
  });

  await assert.rejects(() => runtime.ping(), /not configured/);
});

test("clean-room vercel ai model factory rejects unsupported provider kind", async () => {
  await assert.rejects(
    () =>
      createModelHandle({
        providerKind: "unknown",
        model: "mock",
        baseURL: "http://localhost:8000/v1",
        apiKey: null,
        headers: {}
      }),
    /Unsupported provider kind|requires apiKey/
  );
});

test("clean-room vercel ai client uses placeholder api key for local openai-compatible providers", () => {
  assert.equal(
    resolveOpenAICompatibleAPIKey({
      providerKind: "ollama",
      apiKey: null
    }),
    "threadnote-local"
  );
  assert.equal(
    resolveOpenAICompatibleAPIKey({
      providerKind: "openAI",
      apiKey: "sk-live"
    }),
    "sk-live"
  );
});

test("clean-room ai package imports without missing transitive dependencies", async () => {
  const module = await import("ai");
  assert.equal(typeof module.generateText, "function");
});

test("clean-room ollama native client strips /v1 and calls /api/generate", async () => {
  const calls = [];
  const client = createOllamaNativeClient(
    {
      providerKind: "ollama",
      model: "qwen3.5:4b",
      baseURL: "http://localhost:11434/v1",
      headers: { "x-test": "1" }
    },
    {
      fetchImpl: async (url, options) => {
        calls.push({ url, options });
        return {
          ok: true,
          async json() {
            return {
              model: "qwen3.5:4b",
              response: "pong",
              done: true,
              eval_count: 4,
              prompt_eval_count: 8
            };
          }
        };
      }
    }
  );

  const result = await client.generateText({
    systemPrompt: "system",
    userPrompt: "Respond with pong",
    temperature: 0
  });

  assert.equal(calls.length, 1);
  assert.equal(calls[0].url, "http://localhost:11434/api/generate");
  assert.match(String(calls[0].options.body), /"model":"qwen3\.5:4b"/);
  assert.match(String(calls[0].options.body), /"think":false/);
  assert.match(String(calls[0].options.body), /"keep_alive":"15m"/);
  assert.equal(result.text, "pong");
  assert.equal(result.response.modelId, "qwen3.5:4b");
  assert.deepEqual(result.usage, {
    completionTokens: 4,
    promptTokens: 8,
    totalTokens: 12
  });
});

test("clean-room ollama request body disables default thinking for all ollama models", () => {
  const qwenBody = createOllamaGenerateBody(
    { model: "qwen3.5:9b" },
    {
      systemPrompt: "Return JSON only.",
      userPrompt: "Say hi",
      temperature: 0.2,
      maxOutputTokens: 120
    }
  );
  const llamaBody = createOllamaGenerateBody(
    { model: "llama3.2:3b" },
    {
      systemPrompt: "Return JSON only.",
      userPrompt: "Say hi",
      temperature: 0.2,
      maxOutputTokens: 120
    }
  );

  assert.equal(qwenBody.think, false);
  assert.equal(llamaBody.think, false);
  assert.equal(qwenBody.keep_alive, "15m");
  assert.equal(llamaBody.keep_alive, "15m");
  assert.equal(resolveOllamaThinkSetting("qwen3:8b"), false);
  assert.equal(resolveOllamaThinkSetting("llama3.2:3b"), false);
  assert.equal(resolveOllamaKeepAlive(), "15m");
});

test("clean-room buildOllamaGenerateURL handles base urls with and without /v1", () => {
  assert.equal(buildOllamaGenerateURL("http://localhost:11434/v1"), "http://localhost:11434/api/generate");
  assert.equal(buildOllamaGenerateURL("http://localhost:11434"), "http://localhost:11434/api/generate");
  assert.equal(buildOllamaGenerateURL("http://localhost:11434/api"), "http://localhost:11434/api/generate");
  assert.equal(buildOllamaGenerateURL("http://localhost:11434/api/generate"), "http://localhost:11434/api/generate");
});
