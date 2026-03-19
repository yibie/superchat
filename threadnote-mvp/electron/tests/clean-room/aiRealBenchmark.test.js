import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  aggregateOperationSamples,
  isolateOllamaModel,
  parseCLIArgs,
  runAIRealBenchmark
} from "../../scripts/benchmark-ai-real.mjs";
import { AIProviderRuntime } from "../../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIProviderConfigStore } from "../../src/infrastructure/ai/runtime/aiProviderConfigStore.js";

function makeTempPath(filename) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-ai-benchmark-"));
  return path.join(directory, filename);
}

function createRuntime({ handler, model = "mock-model" }) {
  const store = new AIProviderConfigStore({ configPath: makeTempPath("provider.json") });
  store.save({
    providerKind: "openAICompat",
    baseURL: "http://localhost:8000/v1",
    model
  });
  const runtime = new AIProviderRuntime({
    configStore: store,
    clientFactory: async () => ({
      async generateText(input) {
        return handler(input);
      }
    })
  });
  return { runtime, store };
}

test("clean-room ai real benchmark fails clearly when provider is not configured", async () => {
  const store = new AIProviderConfigStore({ configPath: makeTempPath("provider.json") });
  await assert.rejects(
    () => runAIRealBenchmark({ configStore: store, samples: 1, operations: ["ping"] }),
    /not configured/
  );
});

test("clean-room ai real benchmark reports segmented timings and aggregates", async () => {
  const { runtime } = createRuntime({
    handler: async (input) => {
      if (input.userPrompt === "Say Yes.") {
        return {
          text: "Yes.",
          response: { modelId: "mock-model", id: "ping-1", body: { ok: true } },
          finishReason: "stop",
          warnings: []
        };
      }
      if (/Return JSON only\./.test(input.systemPrompt) && /Prepare a short actionable draft plan/.test(input.systemPrompt)) {
        return {
          text: JSON.stringify({
            title: "Benchmark draft",
            openLoops: ["Record latency"],
            recommendedNextSteps: ["Compare samples"]
          }),
          response: { modelId: "mock-model", id: "prepare-1", body: { ok: true } },
          finishReason: "stop",
          warnings: []
        };
      }
      return {
        text: JSON.stringify({
          currentJudgment: "Instrumentation is ready.",
          openLoops: ["Run more samples"],
          nextAction: "Compare p50 and p95",
          restartNote: "Use the report before tuning.",
          recommendedNextSteps: ["Record another baseline"],
          presentation: { headline: "Benchmark ready", blocks: [] }
        }),
        response: { modelId: "mock-model", id: "resume-1", body: { ok: true } },
        finishReason: "stop",
        warnings: []
      };
    }
  });

  const report = await runAIRealBenchmark({
    runtime,
    samples: 2,
    operations: ["ping", "prepare", "resume"]
  });

  assert.equal(report.benchmark, "ai-real-first-response-v1");
  assert.equal(report.samples, 2);
  assert.equal(report.environment.model, "mock-model");
  assert.equal(report.environment.backendLabel, "OpenAI-Compatible · mock-model");
  assert.equal(report.operations.ping.samples.length, 2);
  assert.equal(report.operations.prepare.samples.length, 2);
  assert.equal(report.operations.resume.samples.length, 2);
  assert.equal(report.operations.prepare.aggregate.totalMS.avg >= 0, true);
  assert.equal(report.operations.prepare.samples[0].promptBytes > 0, true);
  assert.equal(report.operations.resume.samples[0].responseLength > 0, true);
  assert.deepEqual(report.errors, []);
});

test("clean-room ai real benchmark keeps failed samples in report", async () => {
  let prepareCalls = 0;
  const { runtime } = createRuntime({
    handler: async (input) => {
      if (input.userPrompt === "Say Yes.") {
        return {
          text: "Yes.",
          response: { modelId: "mock-model", id: "ping-ok", body: {} },
          finishReason: "stop",
          warnings: []
        };
      }
      if (/Prepare a short actionable draft plan/.test(input.systemPrompt)) {
        prepareCalls += 1;
        if (prepareCalls === 2) {
          throw new Error("prepare exploded");
        }
        return {
          text: JSON.stringify({
            title: "Draft",
            openLoops: [],
            recommendedNextSteps: []
          }),
          response: { modelId: "mock-model", id: `prepare-${prepareCalls}`, body: {} },
          finishReason: "stop",
          warnings: []
        };
      }
      return {
        text: JSON.stringify({
          currentJudgment: "Ready",
          openLoops: [],
          nextAction: null,
          restartNote: "Ready",
          recommendedNextSteps: [],
          presentation: { headline: "Ready", blocks: [] }
        }),
        response: { modelId: "mock-model", id: "resume-ok", body: {} },
        finishReason: "stop",
        warnings: []
      };
    }
  });

  const report = await runAIRealBenchmark({
    runtime,
    samples: 3,
    operations: ["prepare"]
  });

  assert.equal(report.operations.prepare.samples.length, 3);
  assert.equal(report.operations.prepare.failedSamples, 1);
  assert.equal(report.operations.prepare.samples[1].status, "error");
  assert.match(report.operations.prepare.samples[1].error.message, /prepare exploded/);
  assert.equal(report.errors.length, 1);
});

test("clean-room ai real benchmark records timed out samples as errors", async () => {
  const { runtime } = createRuntime({
    handler: async (input) => {
      await new Promise((resolve, reject) => {
        const timeout = setTimeout(resolve, 100);
        input.signal?.addEventListener("abort", () => {
          clearTimeout(timeout);
          reject(input.signal.reason ?? new Error("aborted"));
        }, { once: true });
      });
      return {
        text: JSON.stringify({
          title: "late draft",
          openLoops: [],
          recommendedNextSteps: []
        }),
        response: { modelId: "mock-model", id: "late", body: {} },
        finishReason: "stop",
        warnings: []
      };
    }
  });

  const report = await runAIRealBenchmark({
    runtime,
    samples: 1,
    operations: ["prepare"],
    timeoutMS: 10
  });

  assert.equal(report.operations.prepare.failedSamples, 1);
  assert.equal(report.operations.prepare.samples[0].status, "error");
  assert.match(report.operations.prepare.samples[0].error.message, /timed out/i);
  assert.equal(report.operations.prepare.samples[0].promptBytes > 0, true);
  assert.equal(report.operations.prepare.samples[0].totalMS >= 0, true);
  assert.equal(typeof report.operations.prepare.samples[0].rawResponsePreview, "string");
  assert.equal(report.operations.prepare.samples[0].rawResponseBodySummary, null);
});

test("clean-room ai real benchmark aggregate computes percentile summaries", () => {
  const aggregate = aggregateOperationSamples([
    { status: "ok", queueWaitMS: 0, clientCreateMS: 1, providerCallMS: 8, totalMS: 10, promptBytes: 100, responseBytes: 20, responseLength: 5 },
    { status: "ok", queueWaitMS: 2, clientCreateMS: 3, providerCallMS: 12, totalMS: 20, promptBytes: 110, responseBytes: 30, responseLength: 6 },
    { status: "error", error: { message: "boom" } }
  ]);

  assert.deepEqual(aggregate.totalMS, {
    min: 10,
    p50: 10,
    p95: 20,
    max: 20,
    avg: 15
  });
  assert.equal(aggregate.failedSamples, 1);
  assert.equal(aggregate.promptBytes.avg, 105);
});

test("clean-room ai real benchmark cli parser normalizes samples and ops", () => {
  const parsed = parseCLIArgs([
    "--samples", "3",
    "--ops", "resume,ping",
    "--timeout-ms", "1500",
    "--ping-timeout-ms", "2000",
    "--resume-timeout-ms", "4000",
    "--isolate-local-model",
    "--write", "docs/x.json"
  ]);
  assert.equal(parsed.samples, 3);
  assert.deepEqual(parsed.operations, ["resume", "ping"]);
  assert.equal(parsed.timeoutMS, 1500);
  assert.deepEqual(parsed.timeoutByOperation, {
    ping: 2000,
    resume: 4000
  });
  assert.equal(parsed.isolateLocalModel, true);
  assert.equal(parsed.outputPath, "docs/x.json");
});

test("clean-room ai real benchmark isolates other ollama models when requested", async () => {
  const commands = [];
  const { runtime } = createRuntime({
    model: "qwen3.5:9b",
    handler: async () => ({
      text: "Yes.",
      response: { modelId: "qwen3.5:9b", id: "ping-1", body: {} },
      finishReason: "stop",
      warnings: []
    })
  });
  runtime.configure({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:9b"
  });

  const report = await runAIRealBenchmark({
    runtime,
    samples: 1,
    operations: ["ping"],
    isolateLocalModel: true,
    processRunner: async (command, args) => {
      commands.push([command, ...args]);
      if (args[0] === "ps") {
        return {
          stdout: [
            "NAME          ID              SIZE     PROCESSOR    CONTEXT    UNTIL",
            "qwen3.5:9b    6488c96fa5fa    20 GB    100% GPU     262144     4 minutes from now",
            "qwen3.5:35b   3460ffeede54    34 GB    100% GPU     262144     3 minutes from now",
            "qwen3.5:4b    2a654d98e6fb    17 GB    100% GPU     262144     2 minutes from now",
            ""
          ].join("\n")
        };
      }
      return { stdout: "" };
    }
  });

  assert.deepEqual(commands, [
    ["ollama", "ps"],
    ["ollama", "stop", "qwen3.5:35b"],
    ["ollama", "stop", "qwen3.5:4b"]
  ]);
  assert.deepEqual(report.isolation, {
    enabled: true,
    targetModel: "qwen3.5:9b",
    stoppedModels: ["qwen3.5:35b", "qwen3.5:4b"]
  });
});

test("clean-room isolateOllamaModel leaves target model running", async () => {
  const commands = [];
  const isolation = await isolateOllamaModel("qwen3.5:9b", {
    processRunner: async (command, args) => {
      commands.push([command, ...args]);
      if (args[0] === "ps") {
        return {
          stdout: [
            "NAME          ID              SIZE     PROCESSOR    CONTEXT    UNTIL",
            "qwen3.5:9b    6488c96fa5fa    20 GB    100% GPU     262144     4 minutes from now",
            ""
          ].join("\n")
        };
      }
      return { stdout: "" };
    }
  });

  assert.deepEqual(commands, [["ollama", "ps"]]);
  assert.deepEqual(isolation, {
    enabled: true,
    targetModel: "qwen3.5:9b",
    stoppedModels: []
  });
});
