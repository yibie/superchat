export const CompletionTriggerKind = Object.freeze({
  TAG: "tag",
  OBJECT: "object",
  REFERENCE: "reference"
});

const TAGS = ["note", "idea", "question", "claim", "evidence", "source", "comparison", "pattern", "plan", "decided", "solved", "verified", "dropped"];
const RELATIONS = [
  { value: null, icon: "↗", label: "None" },
  { value: "supports", icon: "↑", label: "Supports" },
  { value: "opposes", icon: "✕", label: "Opposes" },
  { value: "informs", icon: "ℹ", label: "Informs" },
  { value: "answers", icon: "✓", label: "Answers" }
];

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
    return { kind: CompletionTriggerKind.REFERENCE, query: token.slice(2).toLowerCase(), tokenStart: start, tokenEnd: end };
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
    .map((title) => completionItem(`ref-${title}`, title, CompletionTriggerKind.REFERENCE, "↗", title, scorePrefix(title, trigger.query)))
    .filter((item) => item.score > 0)
    .sort(sortByScore)
    .slice(0, 6);
}

export function applyCompletion(text, trigger, item) {
  const replacement = item.kind === CompletionTriggerKind.TAG
    ? `#${item.insertionText} `
    : item.kind === CompletionTriggerKind.OBJECT
      ? `@${item.insertionText} `
      : item.selectedRelation ? `[[${item.insertionText}::${item.selectedRelation}]] ` : `[[${item.insertionText}]] `;
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

function completionItem(id, title, kind, icon, insertionText, score) {
  return { id, title, kind, icon, insertionText, score };
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
    .map((entry) => entry.summaryText || entry.body?.text || "")
    .filter(Boolean);
}
