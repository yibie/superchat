import { execFile } from "node:child_process";
import path from "node:path";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";
import { AIProviderConfigStore } from "../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import { buildOllamaGenerateURL } from "../src/infrastructure/ai/adapters/vercelAiClientFactory.js";

const execFileAsync = promisify(execFile);
const DEFAULT_TIMEOUT_MS = 30000;
const DEFAULT_MODEL = "qwen3.5:9b";
const DEFAULT_CASES = Object.freeze(["default", "think-false", "think-true"]);

export async function runOllamaGenerateDiagnostic({
  model = DEFAULT_MODEL,
  scenario = "prepare",
  baseURL = null,
  timeoutMS = DEFAULT_TIMEOUT_MS,
  cases = DEFAULT_CASES
} = {}) {
  const endpoint = resolveEndpoint(baseURL);
  const prompt = createScenarioPrompt(scenario);
  const diagnosticCases = createDiagnosticCases(model, prompt)
    .filter((item) => normalizeCaseSelection(cases).includes(item.label));
  const results = [];

  for (const item of diagnosticCases) {
    results.push(await runCase(endpoint, item, timeoutMS));
  }

  return {
    benchmark: "ollama-generate-diagnostic-v2",
    generatedAt: new Date().toISOString(),
    environment: {
      endpoint,
      model,
      scenario,
      timeoutMS
    },
    results
  };
}

export async function runOllamaGenerateCompare({
  models = null,
  scenario = "prepare",
  baseURL = null,
  timeoutMS = DEFAULT_TIMEOUT_MS,
  cases = DEFAULT_CASES,
  modelLister = listOllamaModels,
  diagnosticRunner = runOllamaGenerateDiagnostic
} = {}) {
  const endpoint = resolveEndpoint(baseURL);
  const resolvedModels = normalizeModels(models?.length ? models : await modelLister());
  const reports = [];

  for (const model of resolvedModels) {
    reports.push(await diagnosticRunner({
      model,
      scenario,
      baseURL,
      timeoutMS,
      cases
    }));
  }

  return {
    benchmark: "ollama-generate-compare-v1",
    generatedAt: new Date().toISOString(),
    environment: {
      endpoint,
      scenario,
      timeoutMS,
      cases: normalizeCaseSelection(cases)
    },
    models: summarizeDiagnosticReports(reports),
    reports
  };
}

export function summarizeDiagnosticReports(reports = []) {
  return reports.map((report) => ({
    model: report.environment?.model ?? null,
    rows: (report.results ?? []).map((item) => ({
      case: item.label,
      ok: item.ok,
      elapsedMS: item.elapsedMS,
      doneReason: item.response?.doneReason ?? null,
      responseChars: item.response?.responseChars ?? 0,
      thinkingChars: item.response?.thinkingChars ?? 0,
      promptEvalCount: item.response?.promptEvalCount ?? null,
      evalCount: item.response?.evalCount ?? null,
      error: item.error ?? null
    }))
  }));
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

async function runCase(endpoint, item, timeoutMS) {
  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), timeoutMS);
  const startedAt = Date.now();
  try {
    const response = await fetch(endpoint, {
      method: "POST",
      signal: controller.signal,
      headers: { "content-type": "application/json" },
      body: JSON.stringify(item.payload)
    });
    const body = await response.json().catch(() => null);
    return {
      label: item.label,
      ok: response.ok,
      elapsedMS: Date.now() - startedAt,
      request: {
        think: Object.hasOwn(item.payload, "think") ? item.payload.think : "omitted",
        numPredict: item.payload.options?.num_predict ?? null,
        systemLength: String(item.payload.system ?? "").length,
        promptLength: String(item.payload.prompt ?? "").length
      },
      response: summarizeBody(body),
      error: response.ok ? null : body?.error ?? `HTTP ${response.status}`
    };
  } catch (error) {
    return {
      label: item.label,
      ok: false,
      elapsedMS: Date.now() - startedAt,
      request: {
        think: Object.hasOwn(item.payload, "think") ? item.payload.think : "omitted",
        numPredict: item.payload.options?.num_predict ?? null,
        systemLength: String(item.payload.system ?? "").length,
        promptLength: String(item.payload.prompt ?? "").length
      },
      response: null,
      error: error?.message ?? String(error)
    };
  } finally {
    clearTimeout(timeout);
  }
}

function summarizeBody(body) {
  if (!body || typeof body !== "object") {
    return null;
  }
  const responseText = typeof body.response === "string" ? body.response : "";
  const thinkingText = typeof body.thinking === "string" ? body.thinking : "";
  return {
    done: body.done ?? null,
    doneReason: body.done_reason ?? null,
    promptEvalCount: body.prompt_eval_count ?? null,
    evalCount: body.eval_count ?? null,
    responseChars: responseText.length,
    responsePreview: responseText.slice(0, 400),
    thinkingChars: thinkingText.length,
    thinkingPreview: thinkingText.slice(0, 400)
  };
}

function createDiagnosticCases(model, prompt) {
  return [
    {
      label: "default",
      payload: {
        model,
        system: prompt.system,
        prompt: prompt.user,
        stream: false,
        options: { temperature: 0.2, num_predict: prompt.numPredict }
      }
    },
    {
      label: "think-false",
      payload: {
        model,
        system: prompt.system,
        prompt: prompt.user,
        stream: false,
        think: false,
        options: { temperature: 0.2, num_predict: prompt.numPredict }
      }
    },
    {
      label: "think-true",
      payload: {
        model,
        system: prompt.system,
        prompt: prompt.user,
        stream: false,
        think: true,
        options: { temperature: 0.2, num_predict: prompt.numPredict }
      }
    }
  ];
}

function createScenarioPrompt(scenario) {
  if (scenario === "resume") {
    return {
      numPredict: 220,
      system: "Return JSON only. Summarize thread state for resume. Keep it short and concrete.",
      user: [
        "Question: What should the user do next for benchmark sample 1?",
        "Judgment: The benchmark needs segmented timings before any tuning.",
        "Basis: Existing coverage only times local thread assembly and mocked AI calls.",
        "Open loops: Measure queue wait and client cold start separately.",
        "Next: Run the real provider benchmark and compare samples.",
        "Claims: A stable first-response benchmark should precede optimization.",
        "",
        "Outcomes: Decided=direction. Solved=resolved. Verified=checked. Dropped=abandoned.",
        "Decided: Measure real provider latency first. (claim/benchmark)",
        "Verified: Large-thread perf harness already exists. (evidence/benchmark)",
        "",
        "Recent: Use serial samples so queue noise does not dominate the baseline.",
        "Keep output short: max 2 openLoops, max 3 recommendedNextSteps, max 1 sentence per field.",
        "For local models, set presentation to null unless one short headline is essential.",
        'Return JSON: {"currentJudgment":"...","openLoops":["..."],"nextAction":"...|null","restartNote":"...","recommendedNextSteps":["..."],"presentation":null}'
      ].join("\n")
    };
  }

  return {
    numPredict: 120,
    system: "Return JSON only. Prepare a short actionable draft plan from thread state.",
    user: [
      "Type: writing",
      "Question: How should benchmark sample 1 reduce AI resume latency?",
      "Claims: Need a measurable baseline before tuning.",
      "Open loops: Record first complete usable result timing.",
      "Evidence: Current benchmark only covers data assembly.",
      'Return JSON: {"title":"...","openLoops":["..."],"recommendedNextSteps":["..."]}'
    ].join("\n")
  };
}

export function parseCLIArgs(argv = process.argv.slice(2)) {
  let model = null;
  let models = null;
  let scenario = "prepare";
  let baseURL = null;
  let timeoutMS = DEFAULT_TIMEOUT_MS;
  let cases = [...DEFAULT_CASES];

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === "--model") {
      model = argv[index + 1] ?? DEFAULT_MODEL;
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
    if (arg === "--scenario") {
      scenario = argv[index + 1] ?? scenario;
      index += 1;
      continue;
    }
    if (arg === "--base-url") {
      baseURL = argv[index + 1] ?? null;
      index += 1;
      continue;
    }
    if (arg === "--timeout-ms") {
      timeoutMS = Number.parseInt(argv[index + 1] ?? String(DEFAULT_TIMEOUT_MS), 10);
      index += 1;
      continue;
    }
    if (arg === "--cases") {
      cases = String(argv[index + 1] ?? "")
        .split(",")
        .map((item) => item.trim())
        .filter(Boolean);
      index += 1;
    }
  }

  return {
    model,
    models: model ? [model] : (models?.length ? normalizeModels(models) : null),
    scenario,
    baseURL,
    timeoutMS,
    cases: normalizeCaseSelection(cases)
  };
}

function normalizeModels(models = []) {
  const normalized = [...new Set((models ?? []).map((item) => String(item ?? "").trim()).filter(Boolean))];
  if (normalized.length === 0) {
    throw new Error("At least one Ollama model is required");
  }
  return normalized;
}

function normalizeCaseSelection(cases = DEFAULT_CASES) {
  const normalized = [...new Set((cases ?? []).map((item) => String(item ?? "").trim().toLowerCase()).filter(Boolean))];
  if (normalized.length === 0) {
    return [...DEFAULT_CASES];
  }
  for (const item of normalized) {
    if (!DEFAULT_CASES.includes(item)) {
      throw new Error(`Unsupported case "${item}". Supported cases: ${DEFAULT_CASES.join(", ")}`);
    }
  }
  return normalized;
}

function resolveEndpoint(baseURL) {
  const store = new AIProviderConfigStore();
  const config = store.load() ?? {};
  const resolvedBaseURL = baseURL ?? config.baseURL ?? "http://localhost:11434/v1";
  return buildOllamaGenerateURL(resolvedBaseURL);
}

async function main() {
  const args = parseCLIArgs();
  if (args.models?.length === 1) {
    console.log(JSON.stringify(await runOllamaGenerateDiagnostic({
      model: args.models[0],
      scenario: args.scenario,
      baseURL: args.baseURL,
      timeoutMS: args.timeoutMS,
      cases: args.cases
    }), null, 2));
    return;
  }

  console.log(JSON.stringify(await runOllamaGenerateCompare({
    models: args.models,
    scenario: args.scenario,
    baseURL: args.baseURL,
    timeoutMS: args.timeoutMS,
    cases: args.cases
  }), null, 2));
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
}
