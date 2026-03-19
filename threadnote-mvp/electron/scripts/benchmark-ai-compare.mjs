import fs from "node:fs";
import path from "node:path";
import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";
import { AIProviderConfigStore } from "../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import { runAIRealBenchmark } from "./benchmark-ai-real.mjs";

const execFileAsync = promisify(execFile);
const DEFAULT_OUTPUT_PATH = "docs/benchmarks/ai-real-compare-latest.json";
const DEFAULT_TIMEOUT_MS = 5000;
const DEFAULT_OPERATIONS = ["ping", "prepare", "resume"];

export async function runAICompareBenchmark({
  samples = 1,
  timeoutMS = DEFAULT_TIMEOUT_MS,
  timeoutByOperation = null,
  operations = DEFAULT_OPERATIONS,
  models = null,
  configStore = new AIProviderConfigStore()
} = {}) {
  const baselineConfig = configStore.load();
  if (!baselineConfig) {
    throw new Error("AI provider is not configured. Configure Threadnote first or create .threadnote-ai-provider.json.");
  }
  const resolvedModels = models?.length ? models : await listOllamaModels();
  const reports = [];

  for (const model of resolvedModels) {
    reports.push({
      model,
      report: await runAIRealBenchmark({
        samples,
        timeoutMS,
        timeoutByOperation,
        operations,
        configStore,
        configOverride: { model }
      })
    });
  }

  return {
    benchmark: "ai-real-compare-v1",
    generatedAt: new Date().toISOString(),
    baselineProvider: {
      providerKind: baselineConfig.providerKind,
      baseURL: baselineConfig.baseURL
    },
    samples,
    timeoutMS,
    timeoutByOperation,
    operations,
    models: summarizeReports(reports),
    reports
  };
}

export async function recordAICompareBenchmark(outputPath = DEFAULT_OUTPUT_PATH, options = {}) {
  const report = await runAICompareBenchmark(options);
  const resolvedOutputPath = path.resolve(outputPath);
  fs.mkdirSync(path.dirname(resolvedOutputPath), { recursive: true });
  fs.writeFileSync(resolvedOutputPath, `${JSON.stringify(report, null, 2)}\n`, "utf8");
  return {
    outputPath: resolvedOutputPath,
    report
  };
}

export async function listOllamaModels() {
  const { stdout } = await execFileAsync("ollama", ["list"], { encoding: "utf8" });
  return stdout
    .split("\n")
    .slice(1)
    .map((line) => line.trim())
    .filter(Boolean)
    .map((line) => line.split(/\s{2,}/)[0])
    .filter((name) => !name.includes("embedding"));
}

export function summarizeReports(reports = []) {
  return reports.map(({ model, report }) => ({
    model,
    environment: report.environment,
    operations: Object.fromEntries(
      Object.entries(report.operations).map(([operation, payload]) => [
        operation,
        {
          successfulSamples: payload.successfulSamples,
          failedSamples: payload.failedSamples,
          totalMSAvg: payload.aggregate.totalMS?.avg ?? null,
          providerCallMSAvg: payload.aggregate.providerCallMS?.avg ?? null,
          errors: payload.samples.filter((item) => item.status === "error").map((item) => item.error.message)
        }
      ])
    )
  }));
}

export function parseCLIArgs(argv = process.argv.slice(2)) {
  let samples = 1;
  let timeoutMS = DEFAULT_TIMEOUT_MS;
  const timeoutByOperation = {};
  let outputPath = null;
  let operations = DEFAULT_OPERATIONS;
  let models = null;

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--samples") {
      samples = Number.parseInt(argv[index + 1] ?? "1", 10);
      index += 1;
      continue;
    }
    if (arg === "--timeout-ms") {
      timeoutMS = Number.parseInt(argv[index + 1] ?? String(DEFAULT_TIMEOUT_MS), 10);
      index += 1;
      continue;
    }
    if (arg === "--ping-timeout-ms") {
      timeoutByOperation.ping = Number.parseInt(argv[index + 1] ?? String(DEFAULT_TIMEOUT_MS), 10);
      index += 1;
      continue;
    }
    if (arg === "--prepare-timeout-ms") {
      timeoutByOperation.prepare = Number.parseInt(argv[index + 1] ?? String(DEFAULT_TIMEOUT_MS), 10);
      index += 1;
      continue;
    }
    if (arg === "--resume-timeout-ms") {
      timeoutByOperation.resume = Number.parseInt(argv[index + 1] ?? String(DEFAULT_TIMEOUT_MS), 10);
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
    if (arg === "--models") {
      models = String(argv[index + 1] ?? "")
        .split(",")
        .map((item) => item.trim())
        .filter(Boolean);
      index += 1;
      continue;
    }
    if (arg === "--write") {
      outputPath = argv[index + 1] ?? null;
      index += 1;
    }
  }

  return {
    samples,
    timeoutMS,
    timeoutByOperation: Object.keys(timeoutByOperation).length > 0 ? timeoutByOperation : null,
    operations,
    models,
    outputPath
  };
}

async function main() {
  const args = parseCLIArgs();
  if (args.outputPath) {
    const recorded = await recordAICompareBenchmark(args.outputPath, args);
    console.log(JSON.stringify({ ...recorded.report, outputPath: recorded.outputPath }, null, 2));
    return;
  }
  console.log(JSON.stringify(await runAICompareBenchmark(args), null, 2));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
}
