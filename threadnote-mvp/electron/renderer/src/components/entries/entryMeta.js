import {
  formatReferenceRelation,
  stripReferenceMarkup,
  tokenizeReferenceText
} from "../../../../src/domain/references/referenceSyntax.js";
import {
  extractLocatorCandidates,
  isLocalLocator
} from "../../../../src/domain/resources/richSourceDescriptor.js";

export function getEntryDisplayText(entry) {
  const rawText = entry?.body?.text || entry?.summaryText || "";
  return stripReferenceMarkup(rawText);
}

export function tokenizeEntryBody(entry, { hiddenLocators = [] } = {}) {
  const rawText = filterStandaloneLocatorLines(
    entry?.body?.text || entry?.summaryText || "",
    hiddenLocators
  );
  const references = entry?.references ?? [];
  let referenceCursor = 0;

  return tokenizeReferenceText(rawText).flatMap((segment) => {
    if (segment.type !== "reference") {
      return splitMentionSegments(segment.value);
    }
    const reference = references[referenceCursor] ?? {
      id: `${segment.label}:${referenceCursor}`,
      label: segment.label,
      relationKind: segment.relationKind,
      targetID: segment.targetID ?? null,
      targetThreadID: null,
      targetSummaryText: null,
      isResolved: false
    };
    referenceCursor += 1;
    return {
      type: "reference",
      reference: presentReference(reference, referenceCursor - 1)
    };
  });
}

export function presentBacklinks(entry) {
  return mergeBacklinks(entry, []).map((backlink, index) => ({
    id: backlink.id ?? `${entry.id}:incoming:${index}`,
    sourceEntryID: backlink.sourceEntryID,
    sourceThreadID: backlink.sourceThreadID ?? null,
    sourceSummaryText: String(backlink.sourceSummaryText ?? "").trim() || "note",
    relationKind: formatReferenceRelation(backlink.relationKind),
    title: `<- ${formatReferenceRelation(backlink.relationKind)} from ${String(backlink.sourceSummaryText ?? "").trim() || "note"}`
  }));
}

export function presentBacklinksFromEntries(entry, allEntries = []) {
  return mergeBacklinks(entry, allEntries).map((backlink, index) => ({
    id: backlink.id ?? `${entry.id}:incoming:${index}`,
    sourceEntryID: backlink.sourceEntryID,
    sourceThreadID: backlink.sourceThreadID ?? null,
    sourceSummaryText: String(backlink.sourceSummaryText ?? "").trim() || "note",
    relationKind: formatReferenceRelation(backlink.relationKind),
    title: `<- ${formatReferenceRelation(backlink.relationKind)} from ${String(backlink.sourceSummaryText ?? "").trim() || "note"}`
  }));
}

export function stripEntryReferenceMarkup(text) {
  return stripReferenceMarkup(text);
}

export function collectEntryRenderableLocators(entry) {
  const results = [];
  const seen = new Set();
  const bodyText = String(entry?.body?.text || entry?.summaryText || "");
  const explicitURL = String(entry?.body?.url ?? "").trim();
  const attachments = Array.isArray(entry?.body?.attachments) ? entry.body.attachments : [];

  if (explicitURL) {
    pushLocator(results, seen, {
      locator: explicitURL,
      previewEntryID: entry?.id ?? null
    });
  }

  for (const locator of extractLocatorCandidates(bodyText)) {
    pushLocator(results, seen, { locator });
  }

  for (const attachment of attachments) {
    pushLocator(results, seen, { locator: attachment?.relativePath ?? "" });
  }

  return results;
}

export function filterStandaloneLocatorLines(text, hiddenLocators = []) {
  const hidden = new Set(
    (hiddenLocators ?? [])
      .map((value) => String(value ?? "").trim())
      .filter(Boolean)
  );

  if (!hidden.size) {
    return String(text ?? "");
  }

  const lines = String(text ?? "").split("\n");
  const filtered = [];

  for (const line of lines) {
    const trimmed = line.trim();
    if (hidden.has(trimmed)) {
      continue;
    }
    filtered.push(line);
  }

  return filtered.join("\n").replace(/^\n+/, "");
}

function presentReference(reference, index) {
  const label = String(reference?.targetSummaryText ?? reference?.label ?? "").trim();
  const relationKind = formatReferenceRelation(reference?.relationKind);
  return {
    id: reference?.id ?? `${label}:${index}`,
    label,
    relationKind,
    relationTone: `reference-token-${relationKind}`,
    targetID: reference?.targetID ?? null,
    targetThreadID: reference?.targetThreadID ?? null,
    targetSummaryText: reference?.targetSummaryText ?? null,
    isResolved: Boolean(reference?.isResolved && reference?.targetID),
    title: `${relationKind} ${label}`.trim()
  };
}

function mergeBacklinks(entry, allEntries = []) {
  const merged = [];
  const seen = new Set();

  for (const backlink of entry?.incomingBacklinks ?? []) {
    pushBacklink(merged, seen, backlink);
  }

  for (const candidate of allEntries ?? []) {
    if (!candidate?.id || candidate.id === entry?.id) {
      continue;
    }
    for (const reference of candidate.references ?? []) {
      if ((reference?.targetID ?? null) !== (entry?.id ?? null)) {
        continue;
      }
      pushBacklink(merged, seen, {
        id: reference.id ? `${candidate.id}:${entry.id}:${reference.id}` : `${candidate.id}:${entry.id}:${reference.relationKind ?? "informs"}`,
        sourceEntryID: candidate.id,
        sourceThreadID: candidate.threadID ?? null,
        sourceSummaryText: getEntryDisplayText(candidate) || "note",
        relationKind: reference.relationKind
      });
    }
  }

  return merged;
}

function pushBacklink(target, seen, backlink) {
  if ((backlink?.relationKind ?? null) === "responds-to") {
    return;
  }
  const key = `${backlink?.sourceEntryID ?? ""}::${backlink?.relationKind ?? ""}`;
  if (!backlink?.sourceEntryID || seen.has(key)) {
    return;
  }
  seen.add(key);
  target.push(backlink);
}

function pushLocator(results, seen, { locator, previewEntryID = null } = {}) {
  const normalized = String(locator ?? "").trim();
  if (!normalized || seen.has(normalized)) {
    return;
  }
  seen.add(normalized);
  results.push({
    locator: normalized,
    previewEntryID: previewEntryID && !isLocalLocator(normalized) ? previewEntryID : null
  });
}

function splitMentionSegments(text) {
  const source = String(text ?? "");
  if (!source) {
    return [];
  }

  const pattern = /(^|[\s(])(@[\p{L}\p{N}][\p{L}\p{N}._-]*)/gu;
  const segments = [];
  let lastIndex = 0;

  for (const match of source.matchAll(pattern)) {
    const prefix = match[1] ?? "";
    const mention = match[2] ?? "";
    const matchIndex = match.index ?? 0;
    const mentionIndex = matchIndex + prefix.length;

    if (matchIndex > lastIndex) {
      segments.push({ type: "text", value: source.slice(lastIndex, matchIndex) });
    }
    if (prefix) {
      segments.push({ type: "text", value: prefix });
    }
    segments.push({ type: "mention", mention });
    lastIndex = mentionIndex + mention.length;
  }

  if (lastIndex < source.length) {
    segments.push({ type: "text", value: source.slice(lastIndex) });
  }

  return segments.length > 0 ? segments : [{ type: "text", value: source }];
}
