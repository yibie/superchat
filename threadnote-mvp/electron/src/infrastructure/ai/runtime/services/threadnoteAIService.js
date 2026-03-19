import {
  createAIDebugPayload,
  createDiscourseAnalysisRequest,
  createDiscourseAnalysisResult,
  createDiscourseInferenceRequest,
  createDiscourseInferenceResult,
  createDraftPreparationRequest,
  createDraftPreparationResult,
  createEntryKindClassificationRequest,
  createEntryKindClassificationResult,
  createEntryStatusClassificationRequest,
  createEntryStatusClassificationResult,
  createResumeSynthesisRequest,
  createResumeSynthesisResult,
  createRoutePlanningRequest,
  createRoutePlanningResult
} from "../../../../domain/ai/aiContracts.js";
import { AIRequestPriority } from "../../queue/aiRequestQueue.js";
import { performance } from "node:perf_hooks";

export class ThreadnoteAIService {
  constructor({ providerRuntime, requestQueue, onOperationMeasured = null }) {
    this.providerRuntime = providerRuntime;
    this.requestQueue = requestQueue;
    this.onOperationMeasured = typeof onOperationMeasured === "function" ? onOperationMeasured : null;
  }

  async planRoute(input, { signal = null } = {}) {
    const request = createRoutePlanningRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.ROUTING,
      label: `route:${request.entryID ?? "unknown"}`,
      signal,
      systemPrompt:
        "Route a note to a thread or keep in inbox. Be conservative: only route when clearly matching. Return JSON only.",
      userPrompt: buildRoutePrompt(request)
    });

    return createRoutePlanningResult(
      {
        shouldRoute: String(response.parsed.decision ?? "inbox").trim().toLowerCase() === "route",
        selectedThreadID: response.parsed.selectedThreadID ?? null,
        decisionReason: response.parsed.decisionReason ?? "",
        suggestions: Array.isArray(response.parsed.suggestions) ? response.parsed.suggestions : [],
        debugPayload: this.#debugPayload(response.meta, response.parsed, "route")
      },
      request
    );
  }

  async classifyEntryKind(input, { signal = null } = {}) {
    const request = createEntryKindClassificationRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.CLASSIFY,
      label: `classify:${request.entryID ?? "unknown"}`,
      signal,
      systemPrompt:
        "Classify a Threadnote entry into the supported entry modes only: note, question, source. Be conservative. Return JSON only.",
      userPrompt: buildEntryKindClassificationPrompt(request)
    });

    return createEntryKindClassificationResult({
      kind: response.parsed.kind,
      reason: response.parsed.reason ?? response.parsed.decisionReason ?? "",
      confidence: response.parsed.confidence,
      debugPayload: this.#debugPayload(response.meta, response.parsed, "classifyEntryKind")
    });
  }

  async classifyEntryStatus(input, { signal = null } = {}) {
    const request = createEntryStatusClassificationRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.CLASSIFY,
      label: `status:${request.entryID ?? "unknown"}`,
      signal,
      systemPrompt:
        "Classify the work status of a Threadnote entry inside its thread context. Be conservative. Return JSON only.",
      userPrompt: buildEntryStatusClassificationPrompt(request)
    });

    return createEntryStatusClassificationResult({
      status: response.parsed.status,
      reason: response.parsed.reason ?? response.parsed.decisionReason ?? "",
      confidence: response.parsed.confidence,
      debugPayload: this.#debugPayload(response.meta, response.parsed, "classifyEntryStatus")
    });
  }

  async synthesizeResume(input, { signal = null } = {}) {
    const request = createResumeSynthesisRequest(input);
    const localCompactMode = Boolean(this.providerRuntime?.isLocal);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.SYNTHESIS,
      label: `resume:${request.threadID ?? "unknown"}`,
      signal,
      maxOutputTokens: 220,
      systemPrompt:
        localCompactMode
          ? "Return JSON only. Summarize thread state for resume. Keep it short and concrete."
          : "Return JSON only. Summarize thread state for resume. Keep it short, concrete, and use only supported block kinds.",
      userPrompt: buildResumePrompt(request, { compactLocalMode: localCompactMode })
    });

    return createResumeSynthesisResult({
      currentJudgment: response.parsed.currentJudgment ?? request.currentJudgment,
      openLoops: response.parsed.openLoops ?? request.openLoops,
      nextAction: response.parsed.nextAction ?? request.nextAction,
      restartNote: response.parsed.restartNote ?? request.currentJudgment,
      recoveryLines: request.recoveryLines,
      resolvedSoFar: request.resolvedSoFar,
      recommendedNextSteps: response.parsed.recommendedNextSteps ?? [],
      presentationPlan: response.parsed.presentation ?? null,
      debugPayload: this.#debugPayload(response.meta, response.parsed, "resume")
    });
  }

  async prepareDraft(input, { signal = null } = {}) {
    const request = createDraftPreparationRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.PREPARE,
      label: `prepare:${request.threadID ?? "unknown"}`,
      signal,
      maxOutputTokens: 120,
      systemPrompt:
        "Return JSON only. Prepare a short actionable draft plan from thread state.",
      userPrompt: buildDraftPrompt(request)
    });

    return createDraftPreparationResult({
      title: response.parsed.title,
      openLoops: response.parsed.openLoops,
      recommendedNextSteps: response.parsed.recommendedNextSteps,
      debugPayload: this.#debugPayload(response.meta, response.parsed, "prepare")
    });
  }

  async analyzeDiscourse(input, { signal = null } = {}) {
    const request = createDiscourseAnalysisRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.DISCOURSE,
      label: `discourse-analysis:${request.threadID ?? "unknown"}`,
      signal,
      systemPrompt:
        "Infer discourse relations between snippets. Return JSON only. Use relation kinds: supports/opposes/informs/answers.",
      userPrompt: buildDiscourseAnalysisPrompt(request)
    });

    return createDiscourseAnalysisResult({
      relationPairs: response.parsed.relationPairs ?? response.parsed.relations ?? [],
      rationale: response.parsed.rationale ?? "No rationale provided."
    });
  }

  async inferDiscourseRelations(input, { signal = null } = {}) {
    const request = createDiscourseInferenceRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.DISCOURSE,
      label: `discourse-inference:${request.threadID ?? "unknown"}`,
      signal,
      systemPrompt:
        "Given pair candidates, classify relation kind for each valid pair. Return JSON only with supports/opposes/informs/answers.",
      userPrompt: buildDiscourseInferencePrompt(request)
    });

    return createDiscourseInferenceResult(
      {
        relations: response.parsed.relations ?? []
      },
      request
    );
  }

  async #runJSONTask({ priority, label, systemPrompt, userPrompt, signal = null, maxOutputTokens = null }) {
    const totalStartedAt = performance.now();
    const promptBytes = Buffer.byteLength(`${systemPrompt ?? ""}\n${userPrompt ?? ""}`, "utf8");
    return this.requestQueue.run(async ({ signal: queueSignal, lease }) => {
      let result = null;
      let parsed = null;
      let clientCreateMS = 0;
      let providerCallMS = 0;
      let coldStartClient = false;
      try {
        const clientStartedAt = performance.now();
        let client = null;
        if (typeof this.providerRuntime.createTextClientWithTelemetry === "function") {
          const clientResult = await this.providerRuntime.createTextClientWithTelemetry();
          client = clientResult.client;
          clientCreateMS = Number(clientResult.telemetry?.clientCreateMS ?? 0);
          coldStartClient = Boolean(clientResult.telemetry?.coldStart);
        } else {
          client = await this.providerRuntime.createTextClient();
          clientCreateMS = performance.now() - clientStartedAt;
        }

        const providerStartedAt = performance.now();
        result = await client.generateText({
          systemPrompt,
          userPrompt,
          temperature: 0.2,
          signal: queueSignal ?? signal,
          maxOutputTokens
        });
        providerCallMS = performance.now() - providerStartedAt;
        parsed = tryParseJSON(result.text);
        if (parsed == null) {
          throw new Error("AI response is not valid JSON");
        }
        return {
          parsed,
          meta: {
            ...result,
            telemetry: this.#createTelemetry({
              label,
              operation: label.split(":")[0],
              lease,
              totalStartedAt,
              clientCreateMS,
              providerCallMS,
              coldStartClient,
              promptBytes,
              responseText: result.text,
              responseModelID: result.response?.modelId ?? null,
              responseBody: result.response?.body ?? null
            })
          }
        };
      } catch (error) {
        this.#emitOperationMeasured(this.#createTelemetry({
          label,
          operation: label.split(":")[0],
          lease,
          totalStartedAt,
          clientCreateMS,
          providerCallMS,
          coldStartClient,
          promptBytes,
          responseText: result?.text ?? "",
          responseModelID: result?.response?.modelId ?? null,
          responseBody: result?.response?.body ?? null,
          error
        }));
        throw error;
      }
    }, { priority, label, signal });
  }

  #debugPayload(meta, parsedResponse, operation) {
    const telemetry = meta.telemetry ?? null;
    return createAIDebugPayload({
      backendLabel: this.providerRuntime.backendLabel,
      configuredModelID: this.providerRuntime.config?.model ?? "",
      responseModelID: meta.response?.modelId ?? null,
      responseID: meta.response?.id ?? null,
      finishReason: meta.finishReason ?? "stop",
      warnings: Array.isArray(meta.warnings) ? meta.warnings : [],
      promptStats: this.#buildPromptStats(operation, telemetry),
      parsedResponse,
      rawResponseBody: meta.response?.body ?? null
    });
  }

  #createTelemetry({
    label,
    operation,
    lease,
    totalStartedAt,
    clientCreateMS = 0,
    providerCallMS = 0,
    coldStartClient = false,
    promptBytes = 0,
    responseText = "",
    responseModelID = null,
    responseBody = null,
    error = null
  }) {
    const normalizedResponseText = String(responseText ?? "");
    const telemetry = {
      label,
      operation,
      queueWaitMS: roundMetric(lease?.queueWaitMS ?? 0),
      clientCreateMS: roundMetric(clientCreateMS),
      providerCallMS: roundMetric(providerCallMS),
      totalMS: roundMetric(performance.now() - totalStartedAt),
      responseBytes: Buffer.byteLength(normalizedResponseText, "utf8"),
      responseLength: normalizedResponseText.length,
      rawResponsePreview: normalizedResponseText ? normalizedResponseText.slice(0, 400) : "",
      rawResponseBodySummary: summarizeResponseBody(responseBody),
      promptBytes: Math.max(0, Number(promptBytes ?? 0)),
      modelID: responseModelID ?? this.providerRuntime.config?.model ?? null,
      backendLabel: this.providerRuntime.backendLabel,
      coldStartClient
    };
    if (error) {
      telemetry.error = {
        name: error.name ?? "Error",
        message: error.message ?? String(error)
      };
    }
    this.#emitOperationMeasured(telemetry);
    return telemetry;
  }

  #emitOperationMeasured(telemetry) {
    this.onOperationMeasured?.(Object.freeze({ ...telemetry }));
  }

  #buildPromptStats(operation, telemetry) {
    const parts = [`op=${operation}`];
    if (!telemetry) {
      return parts.join(" ");
    }
    parts.push(`queueWaitMS=${telemetry.queueWaitMS}`);
    parts.push(`clientCreateMS=${telemetry.clientCreateMS}`);
    parts.push(`providerCallMS=${telemetry.providerCallMS}`);
    parts.push(`totalMS=${telemetry.totalMS}`);
    parts.push(`promptBytes=${telemetry.promptBytes}`);
    parts.push(`responseBytes=${telemetry.responseBytes}`);
    return parts.join(" ");
  }
}

function roundMetric(value) {
  return Math.round(Math.max(0, Number(value) || 0) * 100) / 100;
}

function buildRoutePrompt(request) {
  return [
    `Note: ${compact(request.normalizedText, 220)}`,
    `Type: ${request.detectedItemType}`,
    `Objects: ${request.detectedObjects.join(", ") || "None"}`,
    "",
    "Threads:",
    ...(request.candidates ?? []).map(
      (candidate, index) =>
        `${index + 1}. ${candidate.threadID} "${compact(candidate.threadTitle, 60)}" objects=[${candidate.coreObjects.slice(0, 3).join(", ")}] score=${candidate.totalScore}`
    ),
    "",
    'Return JSON: {"decision":"route"|"inbox","selectedThreadID":"UUID|null","decisionReason":"...","suggestions":[{"threadID":"UUID","reason":"..."}]}'
  ].join("\n");
}

function buildResumePrompt(request, { compactLocalMode = false } = {}) {
  const outcomeLines = [
    formatOutcomeGroup("Decided", request.statusSummary?.decided),
    formatOutcomeGroup("Solved", request.statusSummary?.solved),
    formatOutcomeGroup("Verified", request.statusSummary?.verified),
    formatOutcomeGroup("Dropped", request.statusSummary?.dropped)
  ].filter(Boolean);

  if (compactLocalMode) {
    return [
      `Question: ${compact(request.coreQuestion, 100)}`,
      `Judgment: ${compact(request.currentJudgment, 120)}`,
      `Basis: ${compact(request.judgmentBasis, 120)}`,
      `Open loops: ${request.openLoops.slice(0, 2).join(" | ") || "None"}`,
      `Next: ${compact(request.nextAction ?? "None", 80)}`,
      `Claims: ${request.activeClaims.slice(0, 2).map((item) => compact(item, 60)).join(" | ") || "None"}`,
      "",
      "Outcomes: Decided=direction. Solved=resolved. Verified=checked. Dropped=abandoned.",
      ...(outcomeLines.length > 0 ? outcomeLines : ["No thread outcomes recorded."]),
      "",
      `Recent: ${request.recentNotes.slice(0, 2).map((item) => compact(item.text, 70)).join(" | ") || "None"}`,
      "Keep output short: max 2 openLoops, max 3 recommendedNextSteps, max 1 sentence per field.",
      "For local models, set presentation to null unless one short headline is essential.",
      'Return JSON: {"currentJudgment":"...","openLoops":["..."],"nextAction":"...|null","restartNote":"...","recommendedNextSteps":["..."],"presentation":null}'
    ].join("\n");
  }

  return [
    `Question: ${compact(request.coreQuestion, 100)}`,
    `Judgment: ${compact(request.currentJudgment, 140)}`,
    `Basis: ${compact(request.judgmentBasis, 140)}`,
    `Open loops: ${request.openLoops.slice(0, 3).join(" | ") || "None"}`,
    `Next: ${compact(request.nextAction ?? "None", 100)}`,
    `Claims: ${request.activeClaims.slice(0, 3).map((item) => compact(item, 70)).join(" | ") || "None"}`,
    "",
    "Outcomes: Decided=direction. Solved=resolved. Verified=checked. Dropped=abandoned.",
    ...(outcomeLines.length > 0 ? outcomeLines : ["No thread outcomes recorded."]),
    "",
    `Recent: ${request.recentNotes.slice(0, 3).map((item) => compact(item.text, 80)).join(" | ") || "None"}`,
    `Counts: evidence=${request.evidenceCount ?? 0} sources=${request.sourceCount ?? 0}`,
    "",
    "Blocks: judgment,basis,gap,nextMove,evidence,sources,resolved,questions,principles,risks,contrast,checklist",
    'Return JSON: {"currentJudgment":"...","openLoops":["..."],"nextAction":"...|null","restartNote":"...","recommendedNextSteps":["..."],"presentation":{"headline":"...","primaryAction":"...|null","blocks":[{"kind":"judgment|basis|gap|nextMove|evidence|sources|resolved|questions|principles|risks|contrast|checklist","title":"...|null","summary":"...|null","items":["..."],"tone":"accent|warning|success|subdued|neutral|null"}]}}'
  ].join("\n");
}

function formatOutcomeGroup(label, items = []) {
  if (!Array.isArray(items) || items.length === 0) {
    return null;
  }
  return `${label}: ${items
    .slice(0, 3)
    .map((item) => `${compact(item.text, 70)} (${item.kind}/${item.source})`)
    .join(" | ")}`;
}

function buildEntryKindClassificationPrompt(request) {
  return [
    `Entry: ${compact(request.normalizedText, 280)}`,
    `Current mode: ${request.detectedItemType}`,
    `Objects: ${request.detectedObjects.map((item) => item.name).join(", ") || "None"}`,
    `Candidate claims: ${request.candidateClaims.map((item) => compact(item.text, 100)).join(" | ") || "None"}`,
    "",
    "Mode guide:",
    "note = default captured content, including ideas, claims, plans, comparisons, and summaries.",
    "question = an explicit open problem, request for explanation, or issue to resolve.",
    "source = a link, attachment, citation, excerpt, or external material/evidence carrier.",
    "",
    "Allowed modes: note, question, source",
    'Return JSON: {"kind":"...","reason":"...","confidence":0.0}'
  ].join("\n");
}

function buildDraftPrompt(request) {
  return [
    `Type: ${request.type}`,
    `Question: ${compact(request.coreQuestion, 100)}`,
    `Claims: ${request.activeClaims.slice(0, 3).map((item) => compact(item, 70)).join(" | ") || "None"}`,
    `Open loops: ${request.openLoops.slice(0, 3).map((item) => compact(item, 70)).join(" | ") || "None"}`,
    `Evidence: ${request.keyEvidence.slice(0, 3).map((item) => compact(item.text, 70)).join(" | ") || "None"}`,
    'Return JSON: {"title":"...","openLoops":["..."],"recommendedNextSteps":["..."]}'
  ].join("\n");
}

function buildEntryStatusClassificationPrompt(request) {
  return [
    `Entry: ${compact(request.normalizedText, 240)}`,
    `Current mode: ${request.currentKind}`,
    `Current status: ${request.currentStatus}`,
    `Thread title: ${compact(request.threadTitle, 100) || "Unknown"}`,
    `Thread goal: ${compact(request.threadGoal, 160) || "None"}`,
    `Recent thread context: ${request.recentThreadEntries.map((item) => `${item.kind}/${item.status}: ${compact(item.text, 90)}`).join(" | ") || "None"}`,
    "",
    "Status guide:",
    "open = still active, unresolved, or not clearly settled.",
    "decided = a choice or direction has been settled for now.",
    "solved = a question or problem is resolved.",
    "verified = a claim or result has been checked and confirmed.",
    "dropped = an idea, path, or issue is intentionally abandoned.",
    "Use thread context. Do not infer a closed status unless the text is explicit or strongly implied by the surrounding thread.",
    "",
    "Allowed status values: open, decided, solved, verified, dropped",
    'Return JSON: {"status":"open|decided|solved|verified|dropped","reason":"...","confidence":0.0}'
  ].join("\n");
}

function buildDiscourseAnalysisPrompt(request) {
  return [
    `ThreadID: ${request.threadID ?? "unknown"}`,
    "Snippets:",
    ...request.snippets.map(
      (snippet, index) => `${index + 1}. id=${snippet.id} kind=${snippet.kind} text=${compact(snippet.text, 180)}`
    ),
    "",
    'Return JSON: {"relationPairs":[{"sourceEntryID":"...","targetEntryID":"...","kind":"supports|opposes|informs|answers"}],"rationale":"..."}'
  ].join("\n");
}

function buildDiscourseInferencePrompt(request) {
  return [
    `ThreadID: ${request.threadID ?? "unknown"}`,
    "Pairs:",
    ...request.pairs.map(
      (pair) =>
        `pair=${pair.pairIndex} source=${pair.sourceID}(${pair.sourceKind}) target=${pair.targetID}(${pair.targetKind}) src=${compact(pair.sourceSnippet, 120)} tgt=${compact(pair.targetSnippet, 120)}`
    ),
    "",
    'Return JSON: {"relations":[{"sourceEntryID":"...","targetEntryID":"...","kind":"supports|opposes|informs|answers"}]}'
  ].join("\n");
}

function tryParseJSON(text) {
  const raw = String(text ?? "").trim();
  if (!raw) {
    return null;
  }
  for (const candidate of extractJSONCandidates(raw)) {
    const parsed = parseJSONCandidate(candidate);
    if (parsed != null) {
      return parsed;
    }
  }
  return null;
}

function compact(text, maxLength) {
  const value = String(text ?? "").replace(/\s+/g, " ").trim();
  if (value.length <= maxLength) {
    return value;
  }
  return `${value.slice(0, Math.max(0, maxLength - 1))}…`;
}

function summarizeResponseBody(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) {
    return null;
  }
  const responseText = typeof body.response === "string" ? body.response : "";
  const thinkingText = typeof body.thinking === "string" ? body.thinking : "";
  return {
    done: body.done ?? null,
    doneReason: body.done_reason ?? null,
    totalDuration: body.total_duration ?? null,
    loadDuration: body.load_duration ?? null,
    promptEvalCount: body.prompt_eval_count ?? null,
    evalCount: body.eval_count ?? null,
    responseChars: responseText.length,
    responsePreview: responseText.slice(0, 200),
    thinkingChars: thinkingText.length,
    thinkingPreview: thinkingText.slice(0, 200)
  };
}

function extractJSONCandidates(raw) {
  const candidates = [raw];
  const fenced = raw.match(/```(?:json)?\s*([\s\S]*?)```/i);
  if (fenced?.[1]) {
    candidates.push(fenced[1].trim());
  }

  const balancedObject = extractBalancedJSONBlock(raw, "{", "}");
  if (balancedObject) {
    candidates.push(balancedObject);
  }

  const balancedArray = extractBalancedJSONBlock(raw, "[", "]");
  if (balancedArray) {
    candidates.push(balancedArray);
  }

  return [...new Set(candidates.map((item) => String(item ?? "").trim()).filter(Boolean))];
}

function parseJSONCandidate(candidate) {
  const direct = tryJSONParse(candidate);
  if (direct != null) {
    return direct;
  }

  const repaired = repairLikelyJSON(candidate);
  if (!repaired || repaired === candidate) {
    return null;
  }
  return tryJSONParse(repaired);
}

function tryJSONParse(value) {
  try {
    return JSON.parse(value);
  } catch {
    return null;
  }
}

function extractBalancedJSONBlock(raw, openChar, closeChar) {
  const start = raw.indexOf(openChar);
  if (start < 0) {
    return null;
  }
  let depth = 0;
  let inString = false;
  let escaped = false;
  for (let index = start; index < raw.length; index += 1) {
    const char = raw[index];
    if (inString) {
      if (escaped) {
        escaped = false;
      } else if (char === "\\") {
        escaped = true;
      } else if (char === "\"") {
        inString = false;
      }
      continue;
    }
    if (char === "\"") {
      inString = true;
      continue;
    }
    if (char === openChar) {
      depth += 1;
    } else if (char === closeChar) {
      depth -= 1;
      if (depth === 0) {
        return raw.slice(start, index + 1);
      }
    }
  }
  return null;
}

function repairLikelyJSON(candidate) {
  return String(candidate ?? "")
    .replace(/^[^{\[]*([{\[])/s, "$1")
    .replace(/([}\]])[^}\]]*$/s, "$1")
    .replace(/[\u201C\u201D]/g, "\"")
    .replace(/[\u2018\u2019]/g, "'")
    .replace(/([{,]\s*)([A-Za-z_][A-Za-z0-9_]*)(\s*:)/g, '$1"$2"$3')
    .replace(/'([^'\\]*(?:\\.[^'\\]*)*)'/g, (_, value) => `"${String(value).replace(/"/g, '\\"')}"`)
    .replace(/,\s*([}\]])/g, "$1")
    .trim();
}
