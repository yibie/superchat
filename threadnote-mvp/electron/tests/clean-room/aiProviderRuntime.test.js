import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { AIProviderRuntime } from "../../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIProviderConfigStore } from "../../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import {
  createModelHandle,
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
