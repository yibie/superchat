import { randomID } from "../support/randomID.js";

export const ThreadColor = Object.freeze({
  ROSE: "rose",
  AMBER: "amber",
  LIME: "lime",
  TEAL: "teal",
  SKY: "sky",
  VIOLET: "violet",
  FUCHSIA: "fuchsia",
  ORANGE: "orange",
  EMERALD: "emerald",
  INDIGO: "indigo"
});

export const ThreadColorValues = Object.freeze(Object.values(ThreadColor));

export const EntryKind = Object.freeze({
  NOTE: "note",
  IDEA: "idea",
  QUESTION: "question",
  CLAIM: "claim",
  EVIDENCE: "evidence",
  SOURCE: "source",
  COMPARISON: "comparison",
  PATTERN: "pattern",
  PLAN: "plan",
  DECIDED: "decided",
  SOLVED: "solved",
  VERIFIED: "verified",
  DROPPED: "dropped",
  HANDOFF: "handoff",
  ANCHOR_WRITTEN: "anchorWritten"
});

export const EntryMode = Object.freeze({
  NOTE: EntryKind.NOTE,
  QUESTION: EntryKind.QUESTION,
  SOURCE: EntryKind.SOURCE
});

export const EntryModeValues = Object.freeze(Object.values(EntryMode));

export const EntryStatus = Object.freeze({
  OPEN: "open",
  DECIDED: "decided",
  SOLVED: "solved",
  VERIFIED: "verified",
  DROPPED: "dropped"
});

export const ThreadStatus = Object.freeze({
  ACTIVE: "active",
  ARCHIVED: "archived"
});

export const ThreadGoalType = Object.freeze({
  BUILD: "build",
  STUDY: "study",
  RESEARCH: "research"
});

export const ThreadGoalStage = Object.freeze({
  FRAMING: "framing",
  GATHERING: "gathering",
  SYNTHESIZING: "synthesizing",
  CONCLUDING: "concluding"
});

export const ClaimStatus = Object.freeze({
  CANDIDATE: "candidate",
  WORKING: "working",
  STABLE: "stable",
  SUPERSEDED: "superseded"
});

export const ObjectKind = Object.freeze({
  PERSON: "person",
  CONTACT: "contact",
  COMPANY: "company",
  PRODUCT: "product",
  BOOK: "book",
  FILM: "film",
  PAPER: "paper",
  EVENT: "event",
  PLACE: "place",
  GENERIC: "generic"
});

export const DiscourseRelationKind = Object.freeze({
  SUPPORTS: "supports",
  OPPOSES: "opposes",
  INFORMS: "informs",
  ANSWERS: "answers",
  RESPONDS_TO: "responds-to"
});

export const MemoryScope = Object.freeze({
  WORKING: "working",
  EPISODIC: "episodic",
  SEMANTIC: "semantic",
  SOURCE: "source"
});

export const CaptureTag = Object.freeze({
  NOTE: "note",
  QUESTION: "question",
  SOURCE: "source"
});

export const CaptureTagValues = Object.freeze(Object.values(CaptureTag));

export const CaptureTagToEntryKind = Object.freeze({
  [CaptureTag.NOTE]: EntryKind.NOTE,
  [CaptureTag.QUESTION]: EntryKind.QUESTION,
  [CaptureTag.SOURCE]: EntryKind.SOURCE
});

const LEGACY_ENTRY_KIND_TO_MODE = Object.freeze({
  [EntryKind.NOTE]: EntryKind.NOTE,
  [EntryKind.IDEA]: EntryKind.NOTE,
  [EntryKind.QUESTION]: EntryKind.QUESTION,
  [EntryKind.CLAIM]: EntryKind.NOTE,
  [EntryKind.EVIDENCE]: EntryKind.SOURCE,
  [EntryKind.SOURCE]: EntryKind.SOURCE,
  [EntryKind.COMPARISON]: EntryKind.NOTE,
  [EntryKind.PATTERN]: EntryKind.NOTE,
  [EntryKind.PLAN]: EntryKind.NOTE,
  [EntryKind.DECIDED]: EntryKind.NOTE,
  [EntryKind.SOLVED]: EntryKind.NOTE,
  [EntryKind.VERIFIED]: EntryKind.NOTE,
  [EntryKind.DROPPED]: EntryKind.NOTE,
  [EntryKind.HANDOFF]: EntryKind.NOTE,
  [EntryKind.ANCHOR_WRITTEN]: EntryKind.NOTE
});

export function normalizeEntryMode(kind) {
  const normalized = String(kind ?? "").trim();
  return LEGACY_ENTRY_KIND_TO_MODE[normalized] ?? EntryKind.NOTE;
}

export function isEntryMode(kind) {
  return EntryModeValues.includes(String(kind ?? "").trim());
}

export function toDate(value) {
  if (value instanceof Date) {
    return value;
  }
  if (value == null) {
    return new Date();
  }
  const parsed = new Date(value);
  return Number.isNaN(parsed.getTime()) ? new Date() : parsed;
}

export function normalizeThreadStatus(value) {
  if (value === "paused" || value === "resolved" || value === ThreadStatus.ARCHIVED) {
    return ThreadStatus.ARCHIVED;
  }
  return ThreadStatus.ACTIVE;
}

export function suggestGoalType(text) {
  const lowered = String(text ?? "").toLowerCase();
  if (
    lowered.includes("research") ||
    lowered.includes("survey") ||
    lowered.includes("compare") ||
    lowered.includes("competitor") ||
    lowered.includes("market") ||
    lowered.includes("product") ||
    lowered.includes("openclaw")
  ) {
    return ThreadGoalType.RESEARCH;
  }
  if (
    lowered.includes("watch") ||
    lowered.includes("read") ||
    lowered.includes("film") ||
    lowered.includes("movie") ||
    lowered.includes("author") ||
    lowered.includes("summary") ||
    lowered.includes("style") ||
    lowered.includes("study")
  ) {
    return ThreadGoalType.STUDY;
  }
  return ThreadGoalType.BUILD;
}

export function createThreadRecord({
  id = randomID(),
  title,
  prompt = title,
  goalLayer,
  status = ThreadStatus.ACTIVE,
  createdAt = new Date(),
  updatedAt = createdAt,
  lastActiveAt = updatedAt,
  color = ThreadColor.SKY
}) {
  const resolvedGoalLayer =
    goalLayer ??
    {
      goalStatement: prompt?.trim() ? prompt : title,
      goalType: suggestGoalType(`${title} ${prompt ?? ""}`),
      successCondition: "Reach a clearer conclusion for this thread.",
      currentStage: ThreadGoalStage.FRAMING
    };

  return {
    id,
    title,
    prompt,
    goalLayer: {
      goalStatement: resolvedGoalLayer.goalStatement,
      goalType: resolvedGoalLayer.goalType ?? ThreadGoalType.BUILD,
      successCondition:
        resolvedGoalLayer.successCondition ?? "Reach a clearer conclusion for this thread.",
      currentStage: resolvedGoalLayer.currentStage ?? ThreadGoalStage.FRAMING
    },
    status: normalizeThreadStatus(status),
    createdAt: toDate(createdAt),
    updatedAt: toDate(updatedAt),
    lastActiveAt: toDate(lastActiveAt),
    color: ThreadColorValues.includes(color) ? color : ThreadColor.SKY
  };
}

export function createEntry({
  id = randomID(),
  threadID = null,
  kind = EntryKind.NOTE,
  status = null,
  body = {},
  summaryText = "",
  sourceMetadata = null,
  statusMetadata = null,
  objectMentions = [],
  references = [],
  createdAt = new Date(),
  sessionID = null,
  authorType = "user",
  parentEntryID = null,
  supersedesEntryID = null,
  importanceScore = null,
  confidenceScore = null,
  inboxState = "unresolved"
}) {
  return {
    id,
    threadID,
    kind,
    status: normalizeEntryStatus(status ?? inferLegacyEntryStatus(kind)),
    body,
    summaryText,
    sourceMetadata,
    statusMetadata,
    objectMentions,
    references,
    createdAt: toDate(createdAt),
    sessionID,
    authorType,
    parentEntryID,
    supersedesEntryID,
    importanceScore,
    confidenceScore,
    inboxState
  };
}

export function normalizeEntryStatus(value) {
  const normalized = String(value ?? "").trim();
  if (Object.values(EntryStatus).includes(normalized)) {
    return normalized;
  }
  return EntryStatus.OPEN;
}

function inferLegacyEntryStatus(kind) {
  switch (kind) {
    case EntryKind.DECIDED:
      return EntryStatus.DECIDED;
    case EntryKind.SOLVED:
      return EntryStatus.SOLVED;
    case EntryKind.VERIFIED:
      return EntryStatus.VERIFIED;
    case EntryKind.DROPPED:
      return EntryStatus.DROPPED;
    default:
      return EntryStatus.OPEN;
  }
}

export function createClaim({
  id = randomID(),
  threadID,
  originEntryID = randomID(),
  statement,
  status = ClaimStatus.CANDIDATE,
  confidenceScore = 0.5,
  createdAt = new Date(),
  updatedAt = createdAt
}) {
  return {
    id,
    threadID,
    originEntryID,
    statement,
    status,
    confidenceScore,
    createdAt: toDate(createdAt),
    updatedAt: toDate(updatedAt)
  };
}

export function createAnchor({
  id = randomID(),
  threadID,
  basedOnEntryID = null,
  title = "Checkpoint",
  coreQuestion,
  stateSummary,
  openLoops = [],
  nextSteps = [],
  claimIDs = [],
  evidenceEntryIDs = [],
  phase = "working",
  createdAt = new Date()
}) {
  return {
    id,
    threadID,
    basedOnEntryID,
    title,
    coreQuestion,
    stateSummary,
    openLoops,
    nextSteps,
    claimIDs,
    evidenceEntryIDs,
    phase,
    createdAt: toDate(createdAt)
  };
}

export function createObjectMention({
  id = randomID(),
  name,
  kind = ObjectKind.GENERIC
}) {
  return { id, name, kind };
}

export function createDiscourseRelation({
  id = randomID(),
  sourceEntryID,
  targetEntryID,
  kind = DiscourseRelationKind.INFORMS,
  confidence = 1
}) {
  return {
    id,
    sourceEntryID,
    targetEntryID,
    kind,
    confidence
  };
}

export function createThreadTask({
  id = randomID(),
  threadID,
  originEntryID = null,
  title,
  status = "open",
  createdAt = new Date(),
  updatedAt = createdAt
}) {
  return {
    id,
    threadID,
    originEntryID,
    title,
    status,
    createdAt: toDate(createdAt),
    updatedAt: toDate(updatedAt)
  };
}

export function createMemoryRecord({
  id = randomID(),
  threadID,
  scope = MemoryScope.WORKING,
  text,
  provenance,
  createdAt = new Date()
}) {
  return {
    id,
    threadID,
    scope,
    text,
    provenance,
    createdAt: toDate(createdAt)
  };
}

export function createThreadAISnapshot({
  threadID,
  contentFingerprint,
  headline = "",
  blocks = [],
  restartNote = "",
  currentJudgment = "",
  openLoops = [],
  nextAction = null,
  recoveryLines = [],
  synthesizedAt = Date.now(),
  modelID = ""
}) {
  return {
    threadID,
    contentFingerprint,
    headline,
    blocks,
    restartNote,
    currentJudgment,
    openLoops,
    nextAction,
    recoveryLines,
    synthesizedAt,
    modelID
  };
}
