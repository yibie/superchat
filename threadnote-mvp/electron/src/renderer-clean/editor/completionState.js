import {
  deriveReferenceTargetLabel,
  EXPLICIT_REFERENCE_RELATIONS,
  splitReferenceTriggerQuery
} from "../../domain/references/referenceSyntax.js";

export const CompletionTriggerKind = Object.freeze({
  TAG: "tag",
  OBJECT: "object",
  REFERENCE: "reference"
});

const TAGS = ["note", "idea", "question", "claim", "evidence", "source", "comparison", "pattern", "plan", "decided", "solved", "verified", "dropped"];
const RELATIONS = EXPLICIT_REFERENCE_RELATIONS.map((value) => ({
  value,
  icon: value === "supports" ? "↑" : value === "opposes" ? "✕" : "✓",
  label: value[0].toUpperCase() + value.slice(1)
}));

export function detectCompletionTrigger(text, cursorIndex) {
  const safeText = String(text ?? "");
  const end = Math.max(0, Math.min(cursorIndex ?? safeText.length, safeText.length));
  let start = end;
  while (start > 0) {
    const char = safeText[start - 1];
    if (/\s/.test(char)) {
      break;
    }
    start -= 1;
  }
  if (start >= end) {
    // Still check for [[ trigger — cursor may be right after a space inside [[...
    const refStart = findReferenceBracket(safeText, end);
    if (refStart !== -1) {
      const refQuery = splitReferenceTriggerQuery(safeText.slice(refStart + 2, end));
      return {
        kind: CompletionTriggerKind.REFERENCE,
        query: refQuery.query,
        selectedRelation: refQuery.relationKind,
        tokenStart: refStart,
        tokenEnd: end
      };
    }
    return null;
  }
  const token = safeText.slice(start, end);
  if (token.startsWith("#")) {
    return { kind: CompletionTriggerKind.TAG, query: token.slice(1).toLowerCase(), tokenStart: start, tokenEnd: end };
  }
  if (token.startsWith("@")) {
    return { kind: CompletionTriggerKind.OBJECT, query: token.slice(1).toLowerCase(), tokenStart: start, tokenEnd: end };
  }
  if (token.startsWith("[[")) {
    const refQuery = splitReferenceTriggerQuery(token.slice(2));
    return {
      kind: CompletionTriggerKind.REFERENCE,
      query: refQuery.query,
      selectedRelation: refQuery.relationKind,
      tokenStart: start,
      tokenEnd: end
    };
  }
  // Token doesn't start with a trigger prefix — scan back further for [[
  const refStart = findReferenceBracket(safeText, end);
  if (refStart !== -1) {
    const refQuery = splitReferenceTriggerQuery(safeText.slice(refStart + 2, end));
    return {
      kind: CompletionTriggerKind.REFERENCE,
      query: refQuery.query,
      selectedRelation: refQuery.relationKind,
      tokenStart: refStart,
      tokenEnd: end
    };
  }
  return null;
}

export function completionsForTrigger(editorState, trigger) {
  if (!trigger) {
    return [];
  }
  if (trigger.kind === CompletionTriggerKind.TAG) {
    return TAGS.map((tag) => completionItem(`tag-${tag}`, tag, CompletionTriggerKind.TAG, "#", tag, scorePrefix(tag, trigger.query)))
      .filter((item) => item.score > 0)
      .sort(sortByScore)
      .slice(0, 6);
  }
  if (trigger.kind === CompletionTriggerKind.OBJECT) {
    return uniqueObjectNames(editorState)
      .map((name) => completionItem(`obj-${name}`, name, CompletionTriggerKind.OBJECT, "@", name, scorePrefix(name, trigger.query)))
      .filter((item) => item.score > 0)
      .sort(sortByScore)
      .slice(0, 6);
  }
  return referenceCandidates(editorState)
    .map((candidate) => completionItem(
      `ref-${candidate.id}`,
      candidate.title,
      CompletionTriggerKind.REFERENCE,
      "↗",
      candidate.title,
      scorePrefix(candidate.title, trigger.query),
      candidate.id
    ))
    .filter((item) => item.score > 0)
    .sort(sortByScore)
    .slice(0, 6);
}

export function applyCompletion(text, trigger, item) {
  const selectedRelation = item.selectedRelation ?? trigger.selectedRelation ?? null;
  const replacement = item.kind === CompletionTriggerKind.TAG
    ? `#${item.insertionText} `
    : item.kind === CompletionTriggerKind.OBJECT
      ? `@${item.insertionText} `
      : selectedRelation ? `[[${selectedRelation}|${item.insertionText}]] ` : `[[${item.insertionText}]] `;
  const next = text.slice(0, trigger.tokenStart) + replacement + text.slice(trigger.tokenEnd);
  return {
    text: next,
    cursor: trigger.tokenStart + replacement.length
  };
}

export function shouldSyncFromInput({ isComposing, inputType }) {
  if (isComposing) {
    return false;
  }
  return inputType !== "insertCompositionText";
}

export function relationOptions() {
  return RELATIONS;
}

/**
 * Scan backwards from `end` to find an unmatched `[[` on the same line.
 * Returns the index of the first `[` or -1 if not found.
 */
function findReferenceBracket(text, end) {
  let pos = end - 1;
  while (pos >= 1) {
    if (text[pos] === "\n") return -1; // don't cross line boundaries
    if (text[pos] === "]" && text[pos - 1] === "]") return -1; // already closed
    if (text[pos] === "[" && text[pos - 1] === "[") return pos - 1;
    pos -= 1;
  }
  return -1;
}

function completionItem(id, title, kind, icon, insertionText, score, targetID = null) {
  return { id, title, kind, icon, insertionText, score, targetID };
}

function scorePrefix(value, query) {
  if (!query) {
    return 1;
  }
  const lowered = value.toLowerCase();
  if (lowered === query) return 100;
  if (lowered.startsWith(query)) return 80;
  if (lowered.includes(query)) return 40;
  return 0;
}

function sortByScore(lhs, rhs) {
  return rhs.score - lhs.score;
}

function uniqueObjectNames(state) {
  const names = [];
  const seen = new Set();
  for (const entry of state?.allEntries ?? []) {
    for (const mention of entry.objectMentions ?? []) {
      const value = String(mention.name ?? "").trim();
      if (!value) continue;
      const key = value.toLowerCase();
      if (seen.has(key)) continue;
      seen.add(key);
      names.push(value);
    }
  }
  return names;
}

function referenceCandidates(state) {
  return (state?.allEntries ?? [])
    .filter((entry) => !entry.parentEntryID)
    .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime())
    .slice(0, 24)
    .map((entry) => ({
      id: entry.id,
      title: deriveReferenceTargetLabel(entry)
    }))
    .filter((item) => item.title);
}
