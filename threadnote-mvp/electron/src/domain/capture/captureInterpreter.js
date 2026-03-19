import {
  CaptureTagToEntryKind,
  CaptureTagValues,
  EntryKind,
  ObjectKind,
  normalizeEntryMode,
  createObjectMention
} from "../models/threadnoteModels.js";
import { extractLocatorCandidate } from "../resources/richSourceDescriptor.js";
import { randomID } from "../support/randomID.js";
import { tokenizeForSearch } from "./tokenizeForSearch.js";

const TAG_PATTERN = new RegExp(`(?<!\\S)#(${CaptureTagValues.join("|")})\\b`, "i");
const AT_MENTION_REGEX = /(?<!\S)@([\p{L}\p{N}][\p{L}\p{N}._-]*)/gu;

export class CaptureInterpreter {
  interpretText(rawText) {
    const trimmed = String(rawText ?? "").trim();
    const explicitTag = parseTag(trimmed);
    const normalizedText = stripExplicitTag(trimmed, explicitTag);
    const detection = detectEntryKind(normalizedText, explicitTag);
    const detectedItemType = detection.kind;
    const detectedObjects = parseObjectMentions(normalizedText);
    const candidateClaims = extractCandidateClaims(normalizedText, detectedItemType);
    const routingSignals = makeRoutingSignals(normalizedText, detectedObjects, candidateClaims);

    return {
      normalizedText,
      explicitTag,
      detectedItemType,
      detectedItemSource: detection.source,
      detectedObjects,
      candidateClaims,
      routingSignals,
      confidenceScore: Math.min(
        1,
        Math.max(
          detection.confidence,
          ...candidateClaims.map((item) => item.confidenceScore),
          detectedObjects.length > 0 ? 0.55 : 0.45
        )
      )
    };
  }

  interpretEntry(entry) {
    const normalizedText = String(entry?.summaryText ?? "").trim();
    const detectedObjects =
      Array.isArray(entry?.objectMentions) && entry.objectMentions.length > 0
        ? entry.objectMentions
        : parseObjectMentions(normalizedText);
    const candidateClaims = extractCandidateClaims(normalizedText, normalizeEntryMode(entry?.kind ?? EntryKind.NOTE));
    return {
      normalizedText,
      explicitTag: null,
      detectedItemType: normalizeEntryMode(entry?.kind ?? EntryKind.NOTE),
      detectedItemSource: String(entry?.sourceMetadata?.kindAttribution?.source ?? "heuristic"),
      detectedObjects,
      candidateClaims,
      routingSignals: makeRoutingSignals(normalizedText, detectedObjects, candidateClaims),
      confidenceScore: Math.max(0.35, ...candidateClaims.map((item) => item.confidenceScore), 0.35)
    };
  }

  extractObjects(texts) {
    return mergeMentions((texts ?? []).flatMap((text) => parseObjectMentions(text)));
  }
}

export function mergeMentions(mentions) {
  const sorted = [...mentions].sort((lhs, rhs) => String(rhs.name).length - String(lhs.name).length);
  const kept = [];
  for (const candidate of sorted) {
    const lowered = String(candidate.name).toLowerCase();
    const isContained = kept.some((existing) => String(existing.name).toLowerCase().includes(lowered));
    if (!isContained) {
      kept.push(candidate);
    }
  }
  return kept.sort((lhs, rhs) => String(lhs.name).localeCompare(String(rhs.name)));
}

function parseTag(text) {
  const match = String(text ?? "").match(TAG_PATTERN);
  return match?.[1]?.toLowerCase() ?? null;
}

function stripExplicitTag(text, explicitTag) {
  if (!explicitTag) {
    return text;
  }
  return String(text).replace(TAG_PATTERN, "").replace(/\s{2,}/g, " ").trim();
}

function parseObjectMentions(text) {
  const mentions = [];
  for (const match of String(text ?? "").matchAll(AT_MENTION_REGEX)) {
    const name = match[1];
    if (!name) {
      continue;
    }
    mentions.push(createObjectMention({ id: randomID(), name, kind: ObjectKind.GENERIC }));
  }
  return mergeMentions(mentions);
}

function detectEntryKind(text, explicitTag) {
  if (explicitTag) {
    return {
      kind: normalizeEntryMode(CaptureTagToEntryKind[explicitTag] ?? EntryKind.NOTE),
      source: "explicitTag",
      confidence: 1
    };
  }

  const normalizedText = normalizeSentence(text);
  if (!normalizedText) {
    return {
      kind: EntryKind.NOTE,
      source: "heuristic",
      confidence: 0.45
    };
  }

  if (looksLikeQuestion(normalizedText)) {
    return {
      kind: EntryKind.QUESTION,
      source: "heuristic",
      confidence: 0.86
    };
  }

  if (looksLikeSource(normalizedText)) {
    return {
      kind: EntryKind.SOURCE,
      source: "heuristic",
      confidence: 0.82
    };
  }

  return {
    kind: EntryKind.NOTE,
    source: "heuristic",
    confidence: 0.45
  };
}

function extractCandidateClaims(text, detectedItemType) {
  const baseConfidence = claimBaseConfidence(detectedItemType);
  if (baseConfidence <= 0) {
    return [];
  }

  const seen = new Set();
  const results = [];
  for (const sentence of splitSentences(text)) {
    const cleaned = normalizeSentence(sentence);
    if (!cleaned) {
      continue;
    }
    const lowered = cleaned.toLowerCase();
    if (isQuestion(lowered, cleaned)) {
      continue;
    }
    if (!looksLikeClaim(lowered, detectedItemType)) {
      continue;
    }
    if (seen.has(lowered)) {
      continue;
    }
    seen.add(lowered);
    results.push({
      id: randomID(),
      text: cleaned,
      confidenceScore: baseConfidence
    });
    if (results.length >= 2) {
      break;
    }
  }
  return results;
}

function claimBaseConfidence(kind) {
  switch (kind) {
    case EntryKind.NOTE:
    case EntryKind.QUESTION:
      return 0.58;
    case EntryKind.SOURCE:
      return 0.48;
    default:
      return 0;
  }
}

function splitSentences(text) {
  return String(text ?? "")
    .split(/[.!?\n。！？]+/g)
    .map((item) => item.trim())
    .filter(Boolean);
}

function normalizeSentence(sentence) {
  return String(sentence ?? "").replace(/\s+/g, " ").trim();
}

function isQuestion(lowered, original) {
  if (original.endsWith("?") || original.endsWith("？")) {
    return true;
  }
  return [
    "why ",
    "how ",
    "what ",
    "who ",
    "where ",
    "when ",
    "should ",
    "can ",
    "is ",
    "are ",
    "do ",
    "does ",
    "did ",
    "which ",
    "could ",
    "would "
  ].some((prefix) => lowered.startsWith(prefix));
}

function looksLikeQuestion(text) {
  const normalized = normalizeSentence(text);
  const lowered = normalized.toLowerCase();
  if (isQuestion(lowered, normalized)) {
    return true;
  }
  return [
    "什么是",
    "为什么",
    "如何",
    "怎么",
    "是否",
    "能否",
    "有没有",
    "which ",
    "what ",
    "why ",
    "how "
  ].some((cue) => lowered.startsWith(cue) || lowered.includes(cue));
}

function looksLikeSource(text) {
  return Boolean(extractLocatorCandidate(text));
}

function looksLikeClaim(lowered, kind) {
  if (kind === EntryKind.QUESTION) {
    return false;
  }
  if (kind === EntryKind.SOURCE) {
    return lowered.includes(" says ") || lowered.includes(" according to ") || lowered.includes(" shows ");
  }
  if (kind === EntryKind.NOTE) {
    return true;
  }
  return [
    " is ",
    " are ",
    " will ",
    " should ",
    " suggests ",
    " means ",
    " likely ",
    " because ",
    " blocks ",
    " depends on ",
    " confirms "
  ].some((cue) => lowered.includes(cue));
}

function makeRoutingSignals(normalizedText, detectedObjects, candidateClaims) {
  const keywords = dedupeStrings([
    ...tokenizeForSearch(normalizedText),
    ...detectedObjects.map((item) => item.name),
    ...candidateClaims.flatMap((item) => tokenizeForSearch(item.text))
  ]);
  const objectNames = dedupeStrings(detectedObjects.map((item) => item.name));
  const candidateClaimTexts = dedupeStrings(candidateClaims.map((item) => item.text));
  const queries = dedupeStrings([...candidateClaimTexts, ...objectNames, ...keywords]);

  return {
    keywords,
    objectNames,
    candidateClaimTexts,
    queries
  };
}

function dedupeStrings(values) {
  const seen = new Set();
  const results = [];
  for (const value of values) {
    const trimmed = String(value ?? "").trim();
    if (trimmed.length <= 1) {
      continue;
    }
    const key = trimmed.toLowerCase();
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    results.push(trimmed);
  }
  return results;
}
