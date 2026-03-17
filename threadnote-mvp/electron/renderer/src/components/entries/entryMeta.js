import {
  formatReferenceRelation,
  stripReferenceMarkup,
  tokenizeReferenceText
} from "../../../../src/domain/references/referenceSyntax.js";

export function getEntryDisplayText(entry) {
  const rawText = entry?.body?.text || entry?.summaryText || "";
  return stripReferenceMarkup(rawText);
}

export function tokenizeEntryBody(entry) {
  const rawText = entry?.body?.text || entry?.summaryText || "";
  const references = entry?.references ?? [];
  let referenceCursor = 0;

  return tokenizeReferenceText(rawText).map((segment) => {
    if (segment.type !== "reference") {
      return segment;
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
  return (entry?.incomingBacklinks ?? []).map((backlink, index) => ({
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
