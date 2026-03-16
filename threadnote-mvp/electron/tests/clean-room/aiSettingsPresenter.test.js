import test from "node:test";
import assert from "node:assert/strict";
import { presentAISettings } from "../../src/application/presenters/aiSettingsPresenter.js";

test("clean-room ai settings presenter exposes conditional fields for local and cloud providers", () => {
  const local = presentAISettings({
    providerKind: "ollama",
    model: "qwen3.5:4b",
    baseURL: "http://localhost:11434/v1",
    embeddingModel: "nomic-embed-text"
  });
  const cloud = presentAISettings({
    providerKind: "openAI",
    model: "gpt-4.1-mini",
    baseURL: "https://api.openai.com/v1",
    apiKey: "sk-test"
  });

  assert.equal(local.conditionalFields.showsAPIKey, false);
  assert.equal(local.conditionalFields.showsEmbeddingModel, true);
  assert.equal(cloud.conditionalFields.showsAPIKey, true);
  assert.equal(cloud.conditionalFields.showsEmbeddingModel, false);
});
