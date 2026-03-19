import test from "node:test";
import assert from "node:assert/strict";
import { parseCLIArgs, summarizeReports } from "../../scripts/benchmark-ai-compare.mjs";
import { createBenchmarkRuntime } from "../../scripts/benchmark-ai-real.mjs";

test("clean-room ai compare benchmark cli parser normalizes models and operations", () => {
  const parsed = parseCLIArgs([
    "--samples", "2",
    "--timeout-ms", "1500",
    "--ping-timeout-ms", "8000",
    "--prepare-timeout-ms", "6000",
    "--ops", "ping,prepare",
    "--models", "qwen3.5:0.8b,qwen3.5:4b",
    "--write", "docs/benchmarks/x.json"
  ]);

  assert.equal(parsed.samples, 2);
  assert.equal(parsed.timeoutMS, 1500);
  assert.deepEqual(parsed.timeoutByOperation, {
    ping: 8000,
    prepare: 6000
  });
  assert.deepEqual(parsed.operations, ["ping", "prepare"]);
  assert.deepEqual(parsed.models, ["qwen3.5:0.8b", "qwen3.5:4b"]);
  assert.equal(parsed.outputPath, "docs/benchmarks/x.json");
});

test("clean-room ai compare benchmark summarizes per-model outcomes", () => {
  const summary = summarizeReports([
    {
      model: "qwen3.5:0.8b",
      report: {
        environment: { model: "qwen3.5:0.8b" },
        operations: {
          ping: {
            successfulSamples: 1,
            failedSamples: 0,
            aggregate: { totalMS: { avg: 120 }, providerCallMS: { avg: 118 } },
            samples: []
          }
        }
      }
    },
    {
      model: "qwen3.5:35b",
      report: {
        environment: { model: "qwen3.5:35b" },
        operations: {
          ping: {
            successfulSamples: 0,
            failedSamples: 1,
            aggregate: { totalMS: null, providerCallMS: null },
            samples: [{ status: "error", error: { message: "timed out" } }]
          }
        }
      }
    }
  ]);

  assert.equal(summary[0].operations.ping.totalMSAvg, 120);
  assert.equal(summary[1].operations.ping.failedSamples, 1);
  assert.deepEqual(summary[1].operations.ping.errors, ["timed out"]);
});

test("clean-room ai real benchmark model override does not require mutating saved config", async () => {
  const runtime = createBenchmarkRuntime({
    configStore: {
      load() {
        return {
          providerKind: "openAICompat",
          baseURL: "http://localhost:8000/v1",
          model: "saved-model",
          apiKey: null,
          embeddingModel: "",
          headers: {}
        };
      }
    },
    configOverride: { model: "override-model" }
  });

  assert.equal(runtime.config.model, "override-model");
  assert.equal(runtime.config.baseURL, "http://localhost:8000/v1");
});
