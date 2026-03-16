import {
  createAIDebugPayload,
  createDiscourseAnalysisRequest,
  createDiscourseAnalysisResult,
  createDiscourseInferenceRequest,
  createDiscourseInferenceResult,
  createDraftPreparationRequest,
  createDraftPreparationResult,
  createResumeSynthesisRequest,
  createResumeSynthesisResult,
  createRoutePlanningRequest,
  createRoutePlanningResult
} from "../../../../domain/ai/aiContracts.js";
import { AIRequestPriority } from "../../queue/aiRequestQueue.js";

export class ThreadnoteAIService {
  constructor({ providerRuntime, requestQueue }) {
    this.providerRuntime = providerRuntime;
    this.requestQueue = requestQueue;
  }

  async planRoute(input) {
    const request = createRoutePlanningRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.ROUTING,
      label: `route:${request.entryID ?? "unknown"}`,
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

  async synthesizeResume(input) {
    const request = createResumeSynthesisRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.SYNTHESIS,
      label: `resume:${request.threadID ?? "unknown"}`,
      systemPrompt:
        "You are a thinking assistant for Threadnote. Preserve deterministic thread state, but improve how it is presented to help the user resume work. Return a short restart note plus a constrained UI plan made only of supported block kinds. Be concrete and specific. Return JSON only.",
      userPrompt: buildResumePrompt(request)
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

  async prepareDraft(input) {
    const request = createDraftPreparationRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.PREPARE,
      label: `prepare:${request.threadID ?? "unknown"}`,
      systemPrompt:
        "Prepare an actionable draft plan from thread state. Keep concrete and short. Return JSON only.",
      userPrompt: buildDraftPrompt(request)
    });

    return createDraftPreparationResult({
      title: response.parsed.title,
      openLoops: response.parsed.openLoops,
      recommendedNextSteps: response.parsed.recommendedNextSteps,
      debugPayload: this.#debugPayload(response.meta, response.parsed, "prepare")
    });
  }

  async analyzeDiscourse(input) {
    const request = createDiscourseAnalysisRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.DISCOURSE,
      label: `discourse-analysis:${request.threadID ?? "unknown"}`,
      systemPrompt:
        "Infer discourse relations between snippets. Return JSON only. Use relation kinds: supports/opposes/informs/answers.",
      userPrompt: buildDiscourseAnalysisPrompt(request)
    });

    return createDiscourseAnalysisResult({
      relationPairs: response.parsed.relationPairs ?? response.parsed.relations ?? [],
      rationale: response.parsed.rationale ?? "No rationale provided."
    });
  }

  async inferDiscourseRelations(input) {
    const request = createDiscourseInferenceRequest(input);
    const response = await this.#runJSONTask({
      priority: AIRequestPriority.DISCOURSE,
      label: `discourse-inference:${request.threadID ?? "unknown"}`,
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

  async #runJSONTask({ priority, label, systemPrompt, userPrompt }) {
    return this.requestQueue.run(async () => {
      const client = await this.providerRuntime.createTextClient();
      const result = await client.generateText({
        systemPrompt,
        userPrompt,
        temperature: 0.2
      });
      const parsed = tryParseJSON(result.text);
      if (parsed == null) {
        throw new Error("AI response is not valid JSON");
      }
      return {
        parsed,
        meta: result
      };
    }, { priority, label });
  }

  #debugPayload(meta, parsedResponse, operation) {
    return createAIDebugPayload({
      backendLabel: this.providerRuntime.backendLabel,
      configuredModelID: this.providerRuntime.config?.model ?? "",
      responseModelID: meta.response?.modelId ?? null,
      responseID: meta.response?.id ?? null,
      finishReason: meta.finishReason ?? "stop",
      warnings: Array.isArray(meta.warnings) ? meta.warnings : [],
      promptStats: `op=${operation}`,
      parsedResponse,
      rawResponseBody: meta.response?.body ?? null
    });
  }
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

function buildResumePrompt(request) {
  return [
    `Core question: ${compact(request.coreQuestion, 140)}`,
    `Current judgment: ${compact(request.currentJudgment, 220)}`,
    `Judgment basis: ${compact(request.judgmentBasis, 240)}`,
    `Open loops: ${request.openLoops.join(" | ") || "None"}`,
    `Next action: ${request.nextAction ?? "None"}`,
    `Claims: ${request.activeClaims.slice(0, 6).join(" | ") || "None"}`,
    `Recent notes: ${request.recentNotes.map((item) => compact(item.text, 120)).join(" | ") || "None"}`,
    `Evidence count: ${request.evidenceCount ?? 0}`,
    `Source count: ${request.sourceCount ?? 0}`,
    "",
    "Supported block kinds: judgment, basis, gap, nextMove, evidence, sources, resolved, questions, principles, risks, contrast, checklist",
    'Return JSON: {"currentJudgment":"...","openLoops":["..."],"nextAction":"...|null","restartNote":"...","recommendedNextSteps":["..."],"presentation":{"headline":"...","primaryAction":"...|null","blocks":[{"kind":"judgment|basis|gap|nextMove|evidence|sources|resolved|questions|principles|risks|contrast|checklist","title":"...|null","summary":"...|null","items":["..."],"tone":"accent|warning|success|subdued|neutral|null"}]}}'
  ].join("\n");
}

function buildDraftPrompt(request) {
  return [
    `Type: ${request.type}`,
    `Core question: ${compact(request.coreQuestion, 140)}`,
    `Claims: ${request.activeClaims.slice(0, 6).join(" | ") || "None"}`,
    `Open loops: ${request.openLoops.slice(0, 6).join(" | ") || "None"}`,
    `Evidence: ${request.keyEvidence.map((item) => compact(item.text, 100)).join(" | ") || "None"}`,
    'Return JSON: {"title":"...","openLoops":["..."],"recommendedNextSteps":["..."]}'
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
  try {
    return JSON.parse(raw);
  } catch {
    const fenced = raw.match(/```json\s*([\s\S]*?)```/i);
    if (fenced?.[1]) {
      try {
        return JSON.parse(fenced[1]);
      } catch {
        return null;
      }
    }
    const match = raw.match(/\{[\s\S]*\}/);
    if (!match) {
      return null;
    }
    try {
      return JSON.parse(match[0]);
    } catch {
      return null;
    }
  }
}

function compact(text, maxLength) {
  const value = String(text ?? "").replace(/\s+/g, " ").trim();
  if (value.length <= maxLength) {
    return value;
  }
  return `${value.slice(0, Math.max(0, maxLength - 1))}…`;
}
