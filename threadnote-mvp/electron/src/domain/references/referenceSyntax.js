export const DEFAULT_REFERENCE_RELATION = "informs";

export const REFERENCE_RELATION_LABELS = Object.freeze({
  informs: "informs",
  supports: "supports",
  opposes: "opposes",
  answers: "answers"
});

export const EXPLICIT_REFERENCE_RELATIONS = Object.freeze([
  "supports",
  "opposes",
  "answers"
]);

const ALL_REFERENCE_RELATIONS = new Set(Object.keys(REFERENCE_RELATION_LABELS));
const REFERENCE_PATTERN = /\[\[([^\]]+)\]\]/g;

export function tokenizeReferenceText(text) {
  const source = String(text ?? "");
  const segments = [];
  let cursor = 0;
  let referenceIndex = 0;

  for (const match of source.matchAll(REFERENCE_PATTERN)) {
    const start = match.index ?? 0;
    const fullMatch = match[0] ?? "";
    const raw = match[1] ?? "";
    if (start > cursor) {
      segments.push({ type: "text", value: source.slice(cursor, start) });
    }
    segments.push({
      type: "reference",
      raw: fullMatch,
      index: referenceIndex,
      start,
      end: start + fullMatch.length,
      ...parseReferenceToken(raw)
    });
    referenceIndex += 1;
    cursor = start + fullMatch.length;
  }

  if (cursor < source.length) {
    segments.push({ type: "text", value: source.slice(cursor) });
  }

  return segments;
}

export function parseReferencesFromText(text) {
  return tokenizeReferenceText(text)
    .filter((segment) => segment.type === "reference" && segment.label)
    .map((segment) => ({
      id: `${segment.label}:${segment.index}`,
      label: segment.label,
      relationKind: segment.relationKind,
      targetKind: "unresolved",
      targetID: null
    }));
}

export function stripReferenceMarkup(text) {
  return tokenizeReferenceText(text)
    .map((segment) => (segment.type === "text" ? segment.value : " "))
    .join("")
    .replace(/[ \t]{2,}/g, " ")
    .replace(/\n[ \t]+/g, "\n")
    .replace(/\n{3,}/g, "\n\n")
    .trim();
}

export function normalizeReferenceRelation(kind) {
  const normalized = String(kind ?? "").trim().toLowerCase();
  return ALL_REFERENCE_RELATIONS.has(normalized) ? normalized : DEFAULT_REFERENCE_RELATION;
}

export function formatReferenceRelation(kind) {
  return REFERENCE_RELATION_LABELS[normalizeReferenceRelation(kind)] ?? DEFAULT_REFERENCE_RELATION;
}

export function deriveReferenceTargetLabel(entry) {
  const raw = String(entry?.body?.text || entry?.summaryText || "").trim();
  if (!raw) {
    return "";
  }
  const stripped = stripReferenceMarkup(raw);
  return firstDisplayLine(stripped || raw);
}

export function splitReferenceTriggerQuery(query) {
  const raw = String(query ?? "");
  const pipeIndex = raw.indexOf("|");
  if (pipeIndex <= 0) {
    return {
      query: raw.toLowerCase(),
      relationKind: null
    };
  }

  const relationKind = raw.slice(0, pipeIndex).trim().toLowerCase();
  if (!ALL_REFERENCE_RELATIONS.has(relationKind)) {
    return {
      query: raw.toLowerCase(),
      relationKind: null
    };
  }

  return {
    query: raw.slice(pipeIndex + 1).toLowerCase(),
    relationKind
  };
}

function parseReferenceToken(rawText) {
  const raw = String(rawText ?? "").trim();
  if (!raw) {
    return {
      label: "",
      relationKind: DEFAULT_REFERENCE_RELATION
    };
  }

  const parts = raw.split("|").map((item) => item.trim()).filter(Boolean);
  if (parts.length === 1) {
    return {
      label: raw,
      relationKind: DEFAULT_REFERENCE_RELATION
    };
  }

  if (parts.length === 2 && ALL_REFERENCE_RELATIONS.has(parts[0].toLowerCase())) {
    return {
      label: parts[1],
      relationKind: parts[0].toLowerCase()
    };
  }

  return {
    label: raw,
    relationKind: DEFAULT_REFERENCE_RELATION
  };
}

function firstDisplayLine(text) {
  return String(text ?? "")
    .split("\n")
    .map((line) => line.trim())
    .find(Boolean) ?? "";
}
