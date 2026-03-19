import test from "node:test";
import assert from "node:assert/strict";
import {
  parseCLIArgs,
  runOllamaGenerateCompare,
  summarizeDiagnosticReports
} from "../../scripts/diagnose-ollama-generate.mjs";

test("clean-room ollama probe cli parser normalizes models and cases", () => {
  const parsed = parseCLIArgs([
    "--models", "qwen3.5:4b,llama3.2:3b",
    "--scenario", "resume",
    "--timeout-ms", "12000",
    "--cases", "default,think-false"
  ]);

  assert.equal(parsed.model, null);
  assert.deepEqual(parsed.models, ["qwen3.5:4b", "llama3.2:3b"]);
  assert.equal(parsed.scenario, "resume");
  assert.equal(parsed.timeoutMS, 12000);
  assert.deepEqual(parsed.cases, ["default", "think-false"]);
});

test("clean-room ollama probe cli parser promotes single model to compare-compatible models array", () => {
  const parsed = parseCLIArgs([
    "--model", "qwen3.5:9b",
    "--cases", "think-false"
  ]);

  assert.equal(parsed.model, "qwen3.5:9b");
  assert.deepEqual(parsed.models, ["qwen3.5:9b"]);
  assert.deepEqual(parsed.cases, ["think-false"]);
});

test("clean-room ollama probe summarizeDiagnosticReports builds unified compare rows", () => {
  const summary = summarizeDiagnosticReports([
    {
      environment: { model: "qwen3.5:9b" },
      results: [
        {
          label: "default",
          ok: true,
          elapsedMS: 3200,
          response: {
            doneReason: "length",
            responseChars: 0,
            thinkingChars: 420,
            promptEvalCount: 90,
            evalCount: 120
          },
          error: null
        }
      ]
    }
  ]);

  assert.equal(summary.length, 1);
  assert.equal(summary[0].model, "qwen3.5:9b");
  assert.deepEqual(summary[0].rows[0], {
    case: "default",
    ok: true,
    elapsedMS: 3200,
    doneReason: "length",
    responseChars: 0,
    thinkingChars: 420,
    promptEvalCount: 90,
    evalCount: 120,
    error: null
  });
});

test("clean-room ollama probe compare runs selected cases for each model", async () => {
  const seen = [];
  const report = await runOllamaGenerateCompare({
    models: ["qwen3.5:4b", "llama3.2:3b"],
    scenario: "prepare",
    timeoutMS: 1000,
    cases: ["think-false"],
    diagnosticRunner: async ({ model, scenario, timeoutMS, cases }) => ({
      benchmark: "ollama-generate-diagnostic-v2",
      generatedAt: "2026-03-19T00:00:00.000Z",
      environment: {
        endpoint: "http://localhost:11434/api/generate",
        model,
        scenario,
        timeoutMS
      },
      results: cases.map((label) => ({ label, ok: true, elapsedMS: 123, response: null, error: null }))
    }),
    modelLister: async () => {
      throw new Error("should not list models");
    }
  });

  assert.equal(report.benchmark, "ollama-generate-compare-v1");
  assert.deepEqual(report.environment.cases, ["think-false"]);
  assert.deepEqual(report.models.map((item) => item.model), ["qwen3.5:4b", "llama3.2:3b"]);
  assert.equal(report.reports.length, 2);
  for (const item of report.reports) {
    assert.equal(item.results.length, 1);
    seen.push(`${item.environment.model}:${item.results[0].label}`);
  }
  assert.deepEqual(seen, [
    "qwen3.5:4b:think-false",
    "llama3.2:3b:think-false"
  ]);
});
