import { DiscourseRelationKind, EntryKind, EntryStatus } from "../models/threadnoteModels.js";

export const AIProviderKind = Object.freeze({
  OPENAI: "openAI",
  ANTHROPIC: "anthropic",
  GOOGLE: "google",
  GROQ: "groq",
  DEEPSEEK: "deepSeek",
  XAI: "xai",
  OLLAMA: "ollama",
  LM_STUDIO: "lmStudio",
  OPENAI_COMPAT: "openAICompat"
});

export const PresentationBlockKind = Object.freeze({
  JUDGMENT: "judgment",
  BASIS: "basis",
  GAP: "gap",
  NEXT_MOVE: "nextMove",
  EVIDENCE: "evidence",
  SOURCES: "sources",
  RESOLVED: "resolved",
  QUESTIONS: "questions",
  PRINCIPLES: "principles",
  RISKS: "risks",
  CONTRAST: "contrast",
  CHECKLIST: "checklist"
});

export const PresentationTone = Object.freeze({
  ACCENT: "accent",
  WARNING: "warning",
  SUCCESS: "success",
  SUBDUED: "subdued",
  NEUTRAL: "neutral"
});

const PRESENTATION_BLOCK_KINDS = new Set(Object.values(PresentationBlockKind));
const PRESENTATION_TONES = new Set(Object.values(PresentationTone));
const RELATION_KINDS = new Set(Object.values(DiscourseRelationKind));
const ENTRY_KINDS = new Set(Object.values(EntryKind));
const ENTRY_STATUSES = new Set(Object.values(EntryStatus));

export function createAISnippet({ id, text, kind = "note" }) {
  return {
    id: stringOrNull(id),
    text: stringOrEmpty(text),
    kind: stringOrEmpty(kind) || "note"
  };
}

export function createAIRelationPair({ sourceEntryID, targetEntryID, kind = DiscourseRelationKind.INFORMS }) {
  return {
    sourceEntryID: stringOrNull(sourceEntryID),
    targetEntryID: stringOrNull(targetEntryID),
    kind: normalizeRelationKind(kind)
  };
}

export function createDiscourseAnalysisRequest({ threadID = null, snippets = [] }) {
  return {
    threadID: stringOrNull(threadID),
    snippets: snippets.map(createAISnippet).filter((item) => item.id && item.text)
  };
}

export function createDiscourseAnalysisResult({ relationPairs = [], rationale = "" }) {
  return {
    relationPairs: relationPairs
      .map(createAIRelationPair)
      .filter((item) => item.sourceEntryID && item.targetEntryID),
    rationale: stringOrEmpty(rationale)
  };
}

export function createDiscourseInferencePair({
  pairIndex,
  sourceID,
  targetID,
  sourceKind = "note",
  targetKind = "note",
  sourceSnippet = "",
  targetSnippet = ""
}) {
  return {
    pairIndex: Number(pairIndex ?? 0),
    sourceID: stringOrNull(sourceID),
    targetID: stringOrNull(targetID),
    sourceKind: stringOrEmpty(sourceKind) || "note",
    targetKind: stringOrEmpty(targetKind) || "note",
    sourceSnippet: stringOrEmpty(sourceSnippet),
    targetSnippet: stringOrEmpty(targetSnippet)
  };
}

export function createDiscourseInferenceRequest({ threadID = null, pairs = [] }) {
  return {
    threadID: stringOrNull(threadID),
    pairs: pairs
      .map(createDiscourseInferencePair)
      .filter((item) => item.sourceID && item.targetID && item.sourceSnippet && item.targetSnippet)
  };
}

export function createDiscourseInferenceResult({ relations = [] }, request = null) {
  const allowed = new Set(
    (request?.pairs ?? []).map((pair) => `${pair.sourceID}::${pair.targetID}`)
  );
  return {
    relations: relations
      .map(createAIRelationPair)
      .filter((item) => item.sourceEntryID && item.targetEntryID)
      .filter((item) => allowed.size === 0 || allowed.has(`${item.sourceEntryID}::${item.targetEntryID}`))
  };
}

export function createRoutePlanningSuggestion({ threadID, reason = "" }) {
  return {
    threadID: stringOrNull(threadID),
    reason: stringOrEmpty(reason)
  };
}

export function createEntryKindClassificationRequest({
  entryID = null,
  normalizedText = "",
  detectedItemType = "note",
  detectedObjects = [],
  candidateClaims = []
}) {
  return {
    entryID: stringOrNull(entryID),
    normalizedText: stringOrEmpty(normalizedText),
    detectedItemType: normalizeEntryKind(detectedItemType),
    detectedObjects: (detectedObjects ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        name: stringOrEmpty(item?.name),
        kind: stringOrEmpty(item?.kind)
      }))
      .filter((item) => item.name),
    candidateClaims: (candidateClaims ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        text: stringOrEmpty(item?.text),
        confidenceScore: normalizeConfidence(item?.confidenceScore, null)
      }))
      .filter((item) => item.text)
  };
}

export function createEntryKindClassificationResult({
  kind = "note",
  reason = "",
  confidence = null,
  debugPayload = null
} = {}) {
  return {
    kind: normalizeEntryKind(kind),
    reason: stringOrEmpty(reason),
    confidence: normalizeConfidence(confidence, null),
    debugPayload: debugPayload ? createAIDebugPayload(debugPayload) : null
  };
}

export function createEntryStatusClassificationRequest({
  entryID = null,
  threadID = null,
  normalizedText = "",
  currentKind = "note",
  currentStatus = "open",
  threadTitle = "",
  threadGoal = "",
  recentThreadEntries = []
}) {
  return {
    entryID: stringOrNull(entryID),
    threadID: stringOrNull(threadID),
    normalizedText: stringOrEmpty(normalizedText),
    currentKind: normalizeEntryKind(currentKind),
    currentStatus: normalizeEntryStatus(currentStatus),
    threadTitle: stringOrEmpty(threadTitle),
    threadGoal: stringOrEmpty(threadGoal),
    recentThreadEntries: (recentThreadEntries ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        text: stringOrEmpty(item?.text),
        kind: normalizeEntryKind(item?.kind),
        status: normalizeEntryStatus(item?.status)
      }))
      .filter((item) => item.text)
  };
}

export function createEntryStatusClassificationResult({
  status = "open",
  reason = "",
  confidence = null,
  debugPayload = null
} = {}) {
  return {
    status: normalizeEntryStatus(status),
    reason: stringOrEmpty(reason),
    confidence: normalizeConfidence(confidence, null),
    debugPayload: debugPayload ? createAIDebugPayload(debugPayload) : null
  };
}

export function createRoutePlanningRequest({
  entryID = null,
  normalizedText = "",
  detectedItemType = "note",
  detectedObjects = [],
  candidateClaims = [],
  routingQueries = [],
  candidates = []
}) {
  return {
    entryID: stringOrNull(entryID),
    normalizedText: stringOrEmpty(normalizedText),
    detectedItemType: stringOrEmpty(detectedItemType) || "note",
    detectedObjects: stringArray(detectedObjects),
    candidateClaims: stringArray(candidateClaims),
    routingQueries: stringArray(routingQueries),
    candidates: (candidates ?? [])
      .map((candidate) => ({
        threadID: stringOrNull(candidate?.threadID),
        threadTitle: stringOrEmpty(candidate?.threadTitle),
        coreObjects: stringArray(candidate?.coreObjects),
        totalScore: Number(candidate?.totalScore ?? 0),
        semanticScore: Number(candidate?.semanticScore ?? 0),
        retrievalScore: Number(candidate?.retrievalScore ?? 0),
        reason: stringOrEmpty(candidate?.reason)
      }))
      .filter((candidate) => candidate.threadID)
  };
}

export function createRoutePlanningResult(
  {
    shouldRoute = false,
    selectedThreadID = null,
    decisionReason = "",
    suggestions = [],
    debugPayload = null
  },
  request = null
) {
  const validThreadIDs = new Set((request?.candidates ?? []).map((candidate) => candidate.threadID));
  const selected = stringOrNull(selectedThreadID);
  return {
    shouldRoute: Boolean(shouldRoute) && (!request || (selected && validThreadIDs.has(selected))),
    selectedThreadID: !request ? selected : validThreadIDs.has(selected) ? selected : null,
    decisionReason: stringOrEmpty(decisionReason) || "No route reason provided.",
    suggestions: suggestions
      .map(createRoutePlanningSuggestion)
      .filter((item) => item.threadID)
      .filter((item) => !request || validThreadIDs.has(item.threadID)),
    debugPayload: debugPayload ? createAIDebugPayload(debugPayload) : null
  };
}

export function createResumeSynthesisRequest({
  threadID = null,
  coreQuestion = "",
  goalLayer = {},
  activeClaims = [],
  currentJudgment = "",
  judgmentBasis = "",
  openLoops = [],
  nextAction = null,
  recoveryLines = [],
  resolvedSoFar = [],
  statusSummary = {},
  recentNotes = [],
  evidenceCount = 0,
  sourceCount = 0
}) {
  return {
    threadID: stringOrNull(threadID),
    coreQuestion: stringOrEmpty(coreQuestion),
    goalLayer: {
      goalType: stringOrEmpty(goalLayer?.goalType) || "build",
      currentStage: stringOrEmpty(goalLayer?.currentStage) || "framing"
    },
    activeClaims: stringArray(activeClaims),
    currentJudgment: stringOrEmpty(currentJudgment),
    judgmentBasis: stringOrEmpty(judgmentBasis),
    openLoops: stringArray(openLoops),
    nextAction: stringOrNull(nextAction),
    recoveryLines: stringArray(recoveryLines),
    resolvedSoFar: stringArray(resolvedSoFar),
    statusSummary: createResumeStatusSummary(statusSummary),
    recentNotes: (recentNotes ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        text: stringOrEmpty(item?.text),
        kind: stringOrEmpty(item?.kind) || "note"
      }))
      .filter((item) => item.text),
    evidenceCount: Number(evidenceCount ?? 0),
    sourceCount: Number(sourceCount ?? 0)
  };
}

export function createResumeSynthesisResult({
  currentJudgment = "",
  openLoops = [],
  nextAction = null,
  restartNote = "",
  recoveryLines = [],
  resolvedSoFar = [],
  recommendedNextSteps = [],
  presentationPlan = null,
  debugPayload = null
}) {
  return {
    currentJudgment: stringOrEmpty(currentJudgment),
    openLoops: stringArray(openLoops).slice(0, 6),
    nextAction: stringOrNull(nextAction),
    restartNote: stringOrEmpty(restartNote),
    recoveryLines: stringArray(recoveryLines),
    resolvedSoFar: stringArray(resolvedSoFar),
    recommendedNextSteps: stringArray(recommendedNextSteps).slice(0, 6),
    presentationPlan: validatePresentationPlan(presentationPlan),
    debugPayload: debugPayload ? createAIDebugPayload(debugPayload) : null
  };
}

export function createDraftPreparationRequest({
  threadID = null,
  type = "writing",
  coreQuestion = "",
  activeClaims = [],
  openLoops = [],
  keyEvidence = [],
  recentNotes = []
}) {
  return {
    threadID: stringOrNull(threadID),
    type: stringOrEmpty(type) || "writing",
    coreQuestion: stringOrEmpty(coreQuestion),
    activeClaims: stringArray(activeClaims),
    openLoops: stringArray(openLoops),
    keyEvidence: (keyEvidence ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        text: stringOrEmpty(item?.text)
      }))
      .filter((item) => item.text),
    recentNotes: (recentNotes ?? [])
      .map((item) => ({
        id: stringOrNull(item?.id),
        text: stringOrEmpty(item?.text)
      }))
      .filter((item) => item.text)
  };
}

export function createDraftPreparationResult({
  title = "Draft",
  openLoops = [],
  recommendedNextSteps = [],
  debugPayload = null
}) {
  return {
    title: stringOrEmpty(title) || "Draft",
    openLoops: stringArray(openLoops).slice(0, 6),
    recommendedNextSteps: stringArray(recommendedNextSteps).slice(0, 6),
    debugPayload: debugPayload ? createAIDebugPayload(debugPayload) : null
  };
}

export function createAIDebugPayload({
  backendLabel = "",
  configuredModelID = "",
  responseModelID = null,
  responseID = null,
  finishReason = "stop",
  warnings = [],
  promptStats = "",
  parsedResponse = null,
  rawResponseBody = null,
  updatedAt = new Date().toISOString()
}) {
  return {
    backendLabel: stringOrEmpty(backendLabel),
    configuredModelID: stringOrEmpty(configuredModelID),
    responseModelID: stringOrNull(responseModelID),
    responseID: stringOrNull(responseID),
    finishReason: stringOrEmpty(finishReason) || "stop",
    warnings: stringArray(warnings),
    promptStats: stringOrEmpty(promptStats),
    parsedResponse: nullableJSON(parsedResponse),
    rawResponseBody: nullableJSON(rawResponseBody),
    updatedAt: stringOrEmpty(updatedAt) || new Date().toISOString()
  };
}

export function normalizeRelationKind(value) {
  const normalized = stringOrEmpty(value).toLowerCase();
  return RELATION_KINDS.has(normalized) ? normalized : DiscourseRelationKind.INFORMS;
}

export function validatePresentationPlan(value) {
  if (!value || typeof value !== "object" || Array.isArray(value)) {
    return null;
  }
  const blocks = Array.isArray(value.blocks)
    ? value.blocks
        .map((block) => normalizePresentationBlock(block))
        .filter(Boolean)
    : [];

  return {
    headline: stringOrNull(value.headline),
    primaryAction: stringOrNull(value.primaryAction),
    blocks
  };
}

function normalizePresentationBlock(block) {
  if (!block || typeof block !== "object" || Array.isArray(block)) {
    return null;
  }
  const kind = stringOrEmpty(block.kind);
  if (!PRESENTATION_BLOCK_KINDS.has(kind)) {
    return null;
  }
  return {
    kind,
    title: stringOrNull(block.title),
    summary: stringOrNull(block.summary),
    items: stringArray(block.items),
    tone: normalizePresentationTone(block.tone)
  };
}

function createResumeStatusSummary(value = {}) {
  return {
    decided: createResumeOutcomeItems(value?.decided),
    solved: createResumeOutcomeItems(value?.solved),
    verified: createResumeOutcomeItems(value?.verified),
    dropped: createResumeOutcomeItems(value?.dropped)
  };
}

function createResumeOutcomeItems(items = []) {
  return (items ?? [])
    .map((item) => ({
      id: stringOrNull(item?.id),
      text: stringOrEmpty(item?.text ?? item?.summaryText),
      kind: normalizeEntryKind(item?.kind),
      updatedAt: stringOrNull(item?.updatedAt),
      source: stringOrEmpty(item?.source) || "heuristic"
    }))
    .filter((item) => item.text);
}

function normalizePresentationTone(value) {
  const normalized = stringOrEmpty(value);
  return PRESENTATION_TONES.has(normalized) ? normalized : PresentationTone.NEUTRAL;
}

function stringArray(value) {
  return Array.isArray(value) ? value.map((item) => stringOrEmpty(item)).filter(Boolean) : [];
}

function normalizeEntryKind(value) {
  const normalized = stringOrEmpty(value);
  return ENTRY_KINDS.has(normalized) ? normalized : EntryKind.NOTE;
}

function normalizeEntryStatus(value) {
  const normalized = stringOrEmpty(value);
  return ENTRY_STATUSES.has(normalized) ? normalized : EntryStatus.OPEN;
}

function normalizeConfidence(value, fallback = 0) {
  const numeric = Number(value);
  if (!Number.isFinite(numeric)) {
    return fallback;
  }
  return Math.max(0, Math.min(1, numeric));
}

function stringOrNull(value) {
  const text = stringOrEmpty(value);
  return text || null;
}

function stringOrEmpty(value) {
  return String(value ?? "").trim();
}

function nullableJSON(value) {
  if (value == null) {
    return null;
  }
  if (typeof value === "string") {
    return value;
  }
  try {
    return JSON.stringify(value, null, 2);
  } catch {
    return null;
  }
}
