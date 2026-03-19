import fs from "node:fs";
import path from "node:path";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";
import { AIProviderConfigStore } from "../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import { AIProviderRuntime } from "../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIRequestQueue } from "../src/infrastructure/ai/queue/aiRequestQueue.js";
import { ThreadnoteAIService } from "../src/infrastructure/ai/runtime/services/threadnoteAIService.js";
import { isLocalProvider } from "../src/infrastructure/ai/providers/providerRegistry.js";
import { resolveProviderConfig } from "../src/infrastructure/ai/providers/providerRegistry.js";

const execFileAsync = promisify(execFile);
const DEFAULT_OUTPUT_PATH = "docs/benchmarks/ai-real-latest.json";
const DEFAULT_SAMPLE_COUNT = 5;
const DEFAULT_OPERATIONS = Object.freeze(["ping", "prepare", "resume"]);
const FIRST_RESPONSE_DEFINITION = "time_to_first_complete_usable_result";
const DEFAULT_TIMEOUT_MS = 30000;
const DEFAULT_TIMEOUTS_BY_OPERATION = Object.freeze({
  ping: DEFAULT_TIMEOUT_MS,
  prepare: DEFAULT_TIMEOUT_MS,
  resume: DEFAULT_TIMEOUT_MS
});

export async function runAIRealBenchmark({
  samples = DEFAULT_SAMPLE_COUNT,
  operations = DEFAULT_OPERATIONS,
  timeoutMS = DEFAULT_TIMEOUT_MS,
  timeoutByOperation = null,
  configStore = new AIProviderConfigStore(),
  configOverride = null,
  runtime = null,
  requestQueue = null,
  isolateLocalModel = false,
  processRunner = execFileAsync
} = {}) {
  const resolvedOperations = normalizeOperations(operations);
  const sampleCount = normalizeSampleCount(samples);
  const resolvedTimeouts = normalizeTimeoutConfig(timeoutMS, timeoutByOperation);
  const providerRuntime = runtime ?? createBenchmarkRuntime({ configStore, configOverride });
  if (!providerRuntime.isConfigured) {
    throw new Error("AI provider is not configured. Configure Threadnote first or create .threadnote-ai-provider.json.");
  }

  const isolation = shouldIsolateLocalModel(providerRuntime, isolateLocalModel)
    ? await isolateOllamaModel(providerRuntime.config.model, { processRunner })
    : null;

  const queue = requestQueue ?? new AIRequestQueue({
    maxConcurrent: providerRuntime.preferredMaxConcurrentRequests || 2
  });
  const measuredOperations = [];
  const aiService = new ThreadnoteAIService({
    providerRuntime,
    requestQueue: queue,
    onOperationMeasured: (measurement) => {
      measuredOperations.push(measurement);
    }
  });

  const operationsReport = {};
  const errors = [];
  for (const operation of resolvedOperations) {
    const samplesForOperation = [];
    for (let index = 0; index < sampleCount; index += 1) {
      const sampleID = index + 1;
      try {
        const sample = await runBenchmarkOperation({
          operation,
          sampleID,
          aiService,
          providerRuntime,
          measuredOperations,
          timeoutMS: resolvedTimeouts[operation]
        });
        samplesForOperation.push(sample);
      } catch (error) {
        const failedSample = createFailedSample({
          operation,
          sampleID,
          error,
          timing: findLatestMeasurement(measuredOperations, operation)
        });
        samplesForOperation.push(failedSample);
        errors.push({
          operation,
          sampleID,
          error: failedSample.error
        });
      }
    }
    operationsReport[operation] = {
      sampleCount,
      successfulSamples: samplesForOperation.filter((sample) => sample.status === "ok").length,
      failedSamples: samplesForOperation.filter((sample) => sample.status === "error").length,
      aggregate: aggregateOperationSamples(samplesForOperation),
      samples: samplesForOperation
    };
  }

  return {
    benchmark: "ai-real-first-response-v1",
    generatedAt: new Date().toISOString(),
    firstResponseDefinition: FIRST_RESPONSE_DEFINITION,
    environment: createEnvironmentSnapshot(providerRuntime),
    isolation,
    samples: sampleCount,
    timeoutMS: resolvedTimeouts,
    aggregate: Object.fromEntries(
      Object.entries(operationsReport).map(([operation, report]) => [operation, report.aggregate])
    ),
    operations: operationsReport,
    errors
  };
}

export async function recordAIRealBenchmark(outputPath = DEFAULT_OUTPUT_PATH, options = {}) {
  const report = await runAIRealBenchmark(options);
  const resolvedOutputPath = path.resolve(outputPath);
  fs.mkdirSync(path.dirname(resolvedOutputPath), { recursive: true });
  fs.writeFileSync(resolvedOutputPath, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  return {
    outputPath: resolvedOutputPath,
    report
  };
}

export function aggregateOperationSamples(samples = []) {
  const successful = samples.filter((sample) => sample.status === "ok");
  return {
    sampleCount: samples.length,
    successfulSamples: successful.length,
    failedSamples: samples.length - successful.length,
    queueWaitMS: aggregateNumericField(successful, "queueWaitMS"),
    clientCreateMS: aggregateNumericField(successful, "clientCreateMS"),
    providerCallMS: aggregateNumericField(successful, "providerCallMS"),
    totalMS: aggregateNumericField(successful, "totalMS"),
    promptBytes: aggregateNumericField(successful, "promptBytes"),
    responseBytes: aggregateNumericField(successful, "responseBytes"),
    responseLength: aggregateNumericField(successful, "responseLength")
  };
}

export function createEnvironmentSnapshot(runtime) {
  const config = runtime.config ?? {};
  return {
    providerKind: config.providerKind ?? null,
    model: config.model ?? null,
    backendLabel: runtime.backendLabel ?? null,
    baseURL: config.baseURL ?? null,
    isLocalProvider: Boolean(isLocalProvider(config.providerKind)),
    nodeVersion: process.version
  };
}

export function parseCLIArgs(argv = process.argv.slice(2)) {
  let samples = DEFAULT_SAMPLE_COUNT;
  let operations = DEFAULT_OPERATIONS;
  let outputPath = null;
  let timeoutMS = DEFAULT_TIMEOUT_MS;
  const timeoutByOperation = {};
  let model = null;
  let isolateLocalModel = false;

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--samples") {
      samples = argv[index + 1];
      index += 1;
      continue;
    }
    if (arg === "--ops") {
      operations = String(argv[index + 1] ?? "")
        .split(",")
        .map((item) => item.trim())
        .filter(Boolean);
      index += 1;
      continue;
    }
    if (arg === "--write") {
      outputPath = argv[index + 1] ?? null;
      index += 1;
      continue;
    }
    if (arg === "--timeout-ms") {
      timeoutMS = argv[index + 1];
      index += 1;
      continue;
    }
    if (arg === "--ping-timeout-ms") {
      timeoutByOperation.ping = argv[index + 1];
      index += 1;
      continue;
    }
    if (arg === "--prepare-timeout-ms") {
      timeoutByOperation.prepare = argv[index + 1];
      index += 1;
      continue;
    }
    if (arg === "--resume-timeout-ms") {
      timeoutByOperation.resume = argv[index + 1];
      index += 1;
      continue;
    }
    if (arg === "--model") {
      model = argv[index + 1] ?? null;
      index += 1;
      continue;
    }
    if (arg === "--isolate-local-model") {
      isolateLocalModel = true;
    }
  }

  return {
    samples: normalizeSampleCount(samples),
    operations: normalizeOperations(operations),
    timeoutMS: normalizeTimeoutMS(timeoutMS),
    timeoutByOperation: Object.keys(timeoutByOperation).length > 0
      ? Object.fromEntries(
          Object.entries(timeoutByOperation).map(([operation, value]) => [operation, normalizeTimeoutMS(value)])
        )
      : null,
    configOverride: model ? { model } : null,
    isolateLocalModel,
    outputPath
  };
}

async function runBenchmarkOperation({
  operation,
  sampleID,
  aiService,
  providerRuntime,
  measuredOperations,
  timeoutMS
}) {
  const startedAt = measuredOperations.length;
  const controller = new AbortController();
  const timeout = setTimeout(() => {
    controller.abort(createTimeoutError(operation, timeoutMS));
  }, timeoutMS);
  try {
  if (operation === "ping") {
    const telemetry = await providerRuntime.pingWithTelemetry({ signal: controller.signal });
    return createSuccessfulSample({
      operation,
      sampleID,
      timing: telemetry.result,
      promptSummary: {
        promptBytes: telemetry.result.promptBytes ?? 0,
        responseLength: telemetry.result.responseLength ?? 0
      }
    });
  }

  if (operation === "prepare") {
    const result = await aiService.prepareDraft(createPrepareInput(sampleID), { signal: controller.signal });
    const measurement = findMeasurement(measuredOperations, startedAt, "prepare");
    return createSuccessfulSample({
      operation,
      sampleID,
      timing: measurement,
      promptSummary: {
        promptBytes: measurement?.promptBytes ?? 0,
        responseLength: measurement?.responseLength ?? 0,
        inputSummaryLength: createPrepareInput(sampleID).coreQuestion.length
      },
      resultSummary: {
        title: result.title
      }
    });
  }

  if (operation === "resume") {
    const result = await aiService.synthesizeResume(createResumeInput(sampleID), { signal: controller.signal });
    const measurement = findMeasurement(measuredOperations, startedAt, "resume");
    return createSuccessfulSample({
      operation,
      sampleID,
      timing: measurement,
      promptSummary: {
        promptBytes: measurement?.promptBytes ?? 0,
        responseLength: measurement?.responseLength ?? 0,
        inputSummaryLength: createResumeInput(sampleID).currentJudgment.length
      },
      resultSummary: {
        restartNote: result.restartNote
      }
    });
  }

  throw new Error(`Unsupported benchmark operation: ${operation}`);
  } finally {
    clearTimeout(timeout);
  }
}

function createSuccessfulSample({
  operation,
  sampleID,
  timing = {},
  promptSummary = {},
  resultSummary = null
}) {
  return {
    operation,
    sampleID,
    status: "ok",
    queueWaitMS: roundMetric(timing.queueWaitMS),
    clientCreateMS: roundMetric(timing.clientCreateMS),
    providerCallMS: roundMetric(timing.providerCallMS),
    totalMS: roundMetric(timing.totalMS),
    promptBytes: Math.max(0, Number(promptSummary.promptBytes ?? timing.promptBytes ?? 0)),
    responseBytes: Math.max(0, Number(timing.responseBytes ?? 0)),
    responseLength: Math.max(0, Number(promptSummary.responseLength ?? timing.responseLength ?? 0)),
    inputSummaryLength: Math.max(0, Number(promptSummary.inputSummaryLength ?? 0)),
    modelID: timing.modelID ?? null,
    backendLabel: timing.backendLabel ?? null,
    coldStartClient: Boolean(timing.coldStartClient),
    resultSummary
  };
}

function createFailedSample({ operation, sampleID, error, timing = null }) {
  const sample = {
    operation,
    sampleID,
    status: "error",
    error: {
      name: error.name ?? "Error",
      message: error.message ?? String(error)
    }
  };
  if (timing) {
    sample.queueWaitMS = roundMetric(timing.queueWaitMS);
    sample.clientCreateMS = roundMetric(timing.clientCreateMS);
    sample.providerCallMS = roundMetric(timing.providerCallMS);
    sample.totalMS = roundMetric(timing.totalMS);
    sample.promptBytes = Math.max(0, Number(timing.promptBytes ?? 0));
    sample.responseBytes = Math.max(0, Number(timing.responseBytes ?? 0));
    sample.responseLength = Math.max(0, Number(timing.responseLength ?? 0));
    sample.modelID = timing.modelID ?? null;
    sample.backendLabel = timing.backendLabel ?? null;
    sample.rawResponsePreview = String(timing.rawResponsePreview ?? "").slice(0, 400);
    sample.rawResponseBodySummary = timing.rawResponseBodySummary ?? null;
  }
  return sample;
}

function findMeasurement(measuredOperations, startIndex, operation) {
  return measuredOperations.slice(startIndex).find((item) => item.operation === operation) ?? null;
}

function findLatestMeasurement(measuredOperations, operation) {
  for (let index = measuredOperations.length - 1; index >= 0; index -= 1) {
    if (measuredOperations[index]?.operation === operation) {
      return measuredOperations[index];
    }
  }
  return null;
}

function createPrepareInput(sampleID) {
  return {
    threadID: `benchmark-prepare-${sampleID}`,
    type: "writing",
    coreQuestion: `How should benchmark sample ${sampleID} reduce AI resume latency?`,
    activeClaims: ["Need a measurable baseline before tuning."],
    openLoops: ["Record first complete usable result timing."],
    keyEvidence: [
      { id: `evidence-${sampleID}`, text: "Current benchmark only covers data assembly." }
    ],
    recentNotes: [
      { id: `note-${sampleID}`, text: "Real provider runs are noisier but closer to actual user experience." }
    ]
  };
}

function createResumeInput(sampleID) {
  return {
    threadID: `benchmark-resume-${sampleID}`,
    coreQuestion: `What should the user do next for benchmark sample ${sampleID}?`,
    goalLayer: { goalType: "build", currentStage: "validation" },
    activeClaims: ["A stable first-response benchmark should precede optimization."],
    currentJudgment: "The benchmark needs segmented timings before any tuning.",
    judgmentBasis: "Existing coverage only times local thread assembly and mocked AI calls.",
    openLoops: ["Measure queue wait and client cold start separately."],
    nextAction: "Run the real provider benchmark and compare samples.",
    recoveryLines: [],
    resolvedSoFar: ["Large-thread state assembly is already covered."],
    statusSummary: {
      decided: [{ id: `d-${sampleID}`, text: "Measure real provider latency first.", kind: "claim", source: "benchmark" }],
      solved: [],
      verified: [{ id: `v-${sampleID}`, text: "Large-thread perf harness already exists.", kind: "evidence", source: "benchmark" }],
      dropped: []
    },
    recentNotes: [
      { id: `resume-note-${sampleID}`, text: "Use serial samples so queue noise does not dominate the baseline." }
    ],
    evidenceCount: 1,
    sourceCount: 1
  };
}

function aggregateNumericField(samples, fieldName) {
  const values = samples
    .map((sample) => Number(sample[fieldName]))
    .filter((value) => Number.isFinite(value))
    .sort((lhs, rhs) => lhs - rhs);
  if (values.length === 0) {
    return null;
  }
  return {
    min: roundMetric(values[0]),
    p50: roundMetric(percentile(values, 0.5)),
    p95: roundMetric(percentile(values, 0.95)),
    max: roundMetric(values[values.length - 1]),
    avg: roundMetric(values.reduce((sum, value) => sum + value, 0) / values.length)
  };
}

function percentile(sortedValues, ratio) {
  if (sortedValues.length === 0) {
    return 0;
  }
  const index = Math.min(sortedValues.length - 1, Math.max(0, Math.ceil(sortedValues.length * ratio) - 1));
  return sortedValues[index];
}

function normalizeSampleCount(value) {
  const numeric = Number.parseInt(String(value ?? DEFAULT_SAMPLE_COUNT), 10);
  if (!Number.isFinite(numeric) || numeric <= 0) {
    throw new Error("Benchmark sample count must be a positive integer");
  }
  return numeric;
}

function normalizeTimeoutMS(value) {
  const numeric = Number.parseInt(String(value ?? DEFAULT_TIMEOUT_MS), 10);
  if (!Number.isFinite(numeric) || numeric <= 0) {
    throw new Error("Benchmark timeout must be a positive integer");
  }
  return numeric;
}

function normalizeTimeoutConfig(timeoutMS, timeoutByOperation) {
  const fallback = normalizeTimeoutMS(timeoutMS);
  return {
    ping: normalizeTimeoutMS(timeoutByOperation?.ping ?? fallback),
    prepare: normalizeTimeoutMS(timeoutByOperation?.prepare ?? fallback),
    resume: normalizeTimeoutMS(timeoutByOperation?.resume ?? fallback)
  };
}

function normalizeOperations(operations) {
  const resolved = Array.isArray(operations) ? operations : String(operations ?? "").split(",");
  const normalized = [...new Set(
    resolved
      .map((item) => String(item ?? "").trim().toLowerCase())
      .filter(Boolean)
  )];
  if (normalized.length === 0) {
    return [...DEFAULT_OPERATIONS];
  }
  for (const operation of normalized) {
    if (!DEFAULT_OPERATIONS.includes(operation)) {
      throw new Error(`Unsupported operation "${operation}". Supported operations: ${DEFAULT_OPERATIONS.join(", ")}`);
    }
  }
  return normalized;
}

function roundMetric(value) {
  return Math.round((Number(value ?? 0) || 0) * 100) / 100;
}

async function main() {
  const { samples, operations, timeoutMS, timeoutByOperation, configOverride, isolateLocalModel, outputPath } = parseCLIArgs();
  if (outputPath) {
    const recorded = await recordAIRealBenchmark(outputPath, {
      samples,
      operations,
      timeoutMS,
      timeoutByOperation,
      configOverride,
      isolateLocalModel
    });
    console.log(JSON.stringify({ ...recorded.report, outputPath: recorded.outputPath }, null, 2));
    return;
  }
  console.log(JSON.stringify(await runAIRealBenchmark({
    samples,
    operations,
    timeoutMS,
    timeoutByOperation,
    configOverride,
    isolateLocalModel
  }), null, 2));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
}

function createTimeoutError(operation, timeoutMS) {
  const error = new Error(`${operation} benchmark timed out after ${timeoutMS}ms`);
  error.name = "AbortError";
  return error;
}

export function createBenchmarkRuntime({ configStore = new AIProviderConfigStore(), configOverride = null } = {}) {
  if (!configOverride) {
    return new AIProviderRuntime({ configStore });
  }
  const baseConfig = configStore.load();
  const mergedConfig = resolveProviderConfig({
    ...(baseConfig ?? {}),
    ...configOverride
  });
  return new AIProviderRuntime({
    configStore: {
      load() {
        return mergedConfig;
      },
      save(config) {
        return config;
      }
    }
  });
}

function shouldIsolateLocalModel(runtime, isolateLocalModel) {
  return Boolean(isolateLocalModel) && runtime?.config?.providerKind === "ollama" && runtime?.config?.model;
}

export async function isolateOllamaModel(targetModel, { processRunner = execFileAsync } = {}) {
  const runningModels = await listRunningOllamaModels({ processRunner });
  const stoppedModels = [];
  for (const model of runningModels) {
    if (model === targetModel) {
      continue;
    }
    await processRunner("ollama", ["stop", model], { encoding: "utf8" }).catch(() => {});
    stoppedModels.push(model);
  }
  return {
    enabled: true,
    targetModel,
    stoppedModels
  };
}

async function listRunningOllamaModels({ processRunner = execFileAsync } = {}) {
  const { stdout } = await processRunner("ollama", ["ps"], { encoding: "utf8" });
  return stdout
    .split("\n")
    .slice(1)
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line) => line.split(/\s{2,}/)[0])
    .filter(Boolean);
}
