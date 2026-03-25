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

// Safety ceiling for very large workspaces. Visible rows are controlled by popup CSS.
const MAX_COMPLETION_CANDIDATES = 200;

// Capture tag completions should only expose entry modes.
// Thread work-state values like decided/solved/verified/dropped are edited elsewhere.
const TAGS = ["note", "question", "source"];
const RELATIONS = EXPLICIT_REFERENCE_RELATIONS.map((value) => ({
  value,
  icon: value === "supports" ? "↑" : value === "opposes" ? "✕" : "✓",
  label: value[0].toUpperCase() + value.slice(1)
}));
const recentCompletionSelections = new Map();

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
    return TAGS.map((tag) => completionItem(`tag-${tag}`, tag, CompletionTriggerKind.TAG, "#", tag, scoreCandidate(tag, trigger.query)))
      .filter((item) => item.score > 0)
      .sort(sortByScore)
      .slice(0, MAX_COMPLETION_CANDIDATES);
  }
  if (trigger.kind === CompletionTriggerKind.OBJECT) {
    return objectCandidates(editorState)
      .map((candidate) => completionItem(
        `obj-${candidate.name}`,
        candidate.name,
        CompletionTriggerKind.OBJECT,
        "@",
        candidate.name,
        scoreCandidate(candidate.name, trigger.query, {
          currentThreadMatch: candidate.threadIDs.includes(editorState?.currentThreadID ?? null),
          createdAt: candidate.lastSeenAt,
          length: candidate.name.length,
          usageCount: candidate.count,
          recentSelection: recentCompletionSelections.get(completionSelectionKey(trigger, { insertionText: candidate.name }))
        })
      ))
      .filter((item) => item.score > 0)
      .sort(sortByScore)
      .slice(0, MAX_COMPLETION_CANDIDATES);
  }
  return referenceCandidates(editorState)
    .map((candidate) => completionItem(
      `ref-${candidate.id}`,
      candidate.title,
      CompletionTriggerKind.REFERENCE,
      "↗",
      candidate.title,
      scoreCandidate(candidate.title, trigger.query, {
        currentThreadMatch: candidate.threadID != null && candidate.threadID === (editorState?.currentThreadID ?? null),
        createdAt: candidate.createdAt,
        length: candidate.title.length,
        recentSelection: recentCompletionSelections.get(completionSelectionKey(trigger, { targetID: candidate.id, insertionText: candidate.title }))
      }),
      candidate.id
    ))
    .filter((item) => item.score > 0)
    .sort(sortByScore)
    .slice(0, MAX_COMPLETION_CANDIDATES);
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

export function recordCompletionSelection(trigger, item, editorState = {}) {
  const key = completionSelectionKey(trigger, item);
  if (!key) {
    return;
  }
  const current = recentCompletionSelections.get(key) ?? {
    count: 0,
    updatedAt: 0,
    threadIDs: new Set()
  };
  current.count += 1;
  current.updatedAt = Date.now();
  if (editorState?.currentThreadID) {
    current.threadIDs.add(editorState.currentThreadID);
  }
  recentCompletionSelections.set(key, current);
}

export function resetCompletionSelectionHistory() {
  recentCompletionSelections.clear();
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

function scoreCandidate(value, query, { currentThreadMatch = false, createdAt = null, length = 0, usageCount = 0, recentSelection = null } = {}) {
  if (!query) {
    return (
      (currentThreadMatch ? 120 : 0) +
      usageCountScore(usageCount) +
      recentSelectionScore(recentSelection, { currentThreadMatch }) +
      recencyScore(createdAt) +
      lengthScore(length || value.length) +
      1
    );
  }
  const lowered = value.toLowerCase();
  const normalizedQuery = query.toLowerCase();
  let score = 0;
  if (lowered === normalizedQuery) {
    score += 1000;
  } else if (lowered.startsWith(normalizedQuery)) {
    score += 700;
  } else if (lowered.includes(normalizedQuery)) {
    score += 350;
  } else {
    return 0;
  }
  if (currentThreadMatch) {
    score += 120;
  }
  score += usageCountScore(usageCount);
  score += recentSelectionScore(recentSelection, { currentThreadMatch });
  score += recencyScore(createdAt);
  score += lengthScore(length || value.length);
  return score;
}

function sortByScore(lhs, rhs) {
  return rhs.score - lhs.score || lhs.title.localeCompare(rhs.title, undefined, { sensitivity: "base" });
}

function objectCandidates(state) {
  const explicitObjects = (state?.objects ?? [])
    .map((item) => {
      if (typeof item === "string") {
        return {
          name: item,
          count: 0,
          lastSeenAt: null,
          threadIDs: []
        };
      }
      return {
        name: item?.name ?? item?.label ?? "",
        count: item?.count ?? 0,
        lastSeenAt: item?.lastSeenAt ?? null,
        threadIDs: Array.isArray(item?.threadIDs) ? item.threadIDs : []
      };
    })
    .map((item) => ({
      ...item,
      name: String(item?.name ?? "").trim().replace(/^@+/, "")
    }))
    .filter((item) => item.name);
  if (explicitObjects.length > 0) {
    return dedupeObjectCandidates(explicitObjects);
  }

  const names = [];
  const seen = new Set();
  for (const entry of state?.allEntries ?? []) {
    const candidateNames = [
      ...(entry.objectMentions ?? []).map((mention) => mention?.name),
      ...extractMentionNamesFromEntry(entry)
    ];
    for (const name of candidateNames) {
      const value = String(name ?? "").trim();
      if (!value) continue;
      const key = value.toLowerCase();
      if (seen.has(key)) continue;
      seen.add(key);
      names.push({
        name: value,
        count: 1,
        lastSeenAt: entry?.createdAt ?? null,
        threadIDs: entry?.threadID ? [entry.threadID] : []
      });
    }
  }
  return names;
}

function referenceCandidates(state) {
  return (state?.allEntries ?? [])
    .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime())
    .map((entry) => ({
      id: entry.id,
      title: deriveReferenceTargetLabel(entry),
      threadID: entry.threadID ?? null,
      createdAt: entry.createdAt ?? null
    }))
    .filter((item) => item.title);
}

function extractMentionNamesFromEntry(entry) {
  const source = String(entry?.body?.text || entry?.summaryText || "");
  const matches = source.match(/(^|[\s(])@([\p{L}\p{N}][\p{L}\p{N}._-]*)/gu) ?? [];
  return matches
    .map((item) => item.trim())
    .map((item) => item.replace(/^@/, ""))
    .filter(Boolean);
}

function dedupeObjectCandidates(values = []) {
  const result = [];
  const seen = new Set();
  for (const item of values) {
    const value = String(item?.name ?? "").trim();
    if (!value) {
      continue;
    }
    const key = value.toLowerCase();
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    result.push(item);
  }
  return result;
}

function usageCountScore(count) {
  return Math.min(60, Math.max(0, Number(count) || 0) * 10);
}

function recencyScore(createdAt) {
  const age = Date.now() - new Date(createdAt ?? 0).getTime();
  if (!Number.isFinite(age)) {
    return 0;
  }
  if (age <= 1000 * 60 * 60 * 24) return 40;
  if (age <= 1000 * 60 * 60 * 24 * 7) return 24;
  if (age <= 1000 * 60 * 60 * 24 * 30) return 12;
  return 0;
}

function lengthScore(length) {
  const normalized = Math.max(0, Number(length) || 0);
  return Math.max(0, 24 - Math.min(normalized, 24));
}

function recentSelectionScore(selection, { currentThreadMatch = false } = {}) {
  if (!selection) {
    return 0;
  }
  const age = Date.now() - Number(selection.updatedAt ?? 0);
  let score = 0;
  if (age <= 1000 * 60 * 10) {
    score += 220;
  } else if (age <= 1000 * 60 * 60) {
    score += 140;
  } else if (age <= 1000 * 60 * 60 * 24) {
    score += 80;
  } else {
    score += 30;
  }
  score += Math.min(80, (selection.count ?? 0) * 20);
  if (currentThreadMatch) {
    score += 30;
  }
  return score;
}

function completionSelectionKey(trigger, item) {
  if (trigger?.kind === CompletionTriggerKind.OBJECT) {
    const name = String(item?.insertionText ?? item?.title ?? "").trim().replace(/^@+/, "").toLowerCase();
    return name ? `${CompletionTriggerKind.OBJECT}:${name}` : null;
  }
  if (trigger?.kind === CompletionTriggerKind.REFERENCE) {
    const targetID = String(item?.targetID ?? "").trim().toLowerCase();
    if (targetID) {
      return `${CompletionTriggerKind.REFERENCE}:${targetID}`;
    }
    const title = String(item?.insertionText ?? item?.title ?? "").trim().toLowerCase();
    return title ? `${CompletionTriggerKind.REFERENCE}:label:${title}` : null;
  }
  if (trigger?.kind === CompletionTriggerKind.TAG) {
    const tag = String(item?.insertionText ?? item?.title ?? "").trim().toLowerCase();
    return tag ? `${CompletionTriggerKind.TAG}:${tag}` : null;
  }
  return null;
}
