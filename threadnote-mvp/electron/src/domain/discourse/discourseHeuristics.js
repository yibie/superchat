import {
  DiscourseRelationKind,
  EntryKind,
  createDiscourseRelation
} from "../models/threadnoteModels.js";
import { tokenizeForSearch } from "../capture/tokenizeForSearch.js";

const KIND_AFFINITY = new Map([
  ["claim::question", 6],
  ["decided::question", 6],
  ["verified::question", 6],
  ["comparison::question", 6],
  ["pattern::question", 6],
  ["evidence::claim", 6],
  ["source::claim", 4],
  ["verified::claim", 4],
  ["comparison::claim", 4],
  ["pattern::claim", 4],
  ["handoff::claim", 3],
  ["handoff::question", 3]
]);

const OPPOSING_CUES = ["not", "instead", "however", "fails", "doesn't", "does not", "but"];

export function primaryRelation(entryID, relations) {
  return (relations ?? []).find((relation) => relation.sourceEntryID === entryID) ??
    (relations ?? []).find((relation) => relation.targetEntryID === entryID) ??
    null;
}

export function relationTarget(entry, previousEntries) {
  const candidates = [...(previousEntries ?? [])].reverse();
  switch (entry?.kind) {
    case EntryKind.EVIDENCE:
      return firstMatching(candidates, [EntryKind.CLAIM, EntryKind.SOURCE, EntryKind.EVIDENCE]);
    case EntryKind.SOURCE:
    case EntryKind.QUESTION:
      return null;
    case EntryKind.CLAIM:
    case EntryKind.COMPARISON:
    case EntryKind.PATTERN:
    case EntryKind.DECIDED:
    case EntryKind.VERIFIED:
      return firstMatching(candidates, [EntryKind.QUESTION, EntryKind.CLAIM, EntryKind.EVIDENCE]);
    default:
      return null;
  }
}

export function relationKind(entry, target) {
  switch (entry?.kind) {
    case EntryKind.EVIDENCE:
      if (target?.kind === EntryKind.SOURCE) {
        return DiscourseRelationKind.INFORMS;
      }
      return isOpposingNarrative(entry?.summaryText) ? DiscourseRelationKind.OPPOSES : DiscourseRelationKind.SUPPORTS;
    case EntryKind.SOURCE:
      return DiscourseRelationKind.INFORMS;
    case EntryKind.QUESTION:
      return DiscourseRelationKind.ANSWERS;
    case EntryKind.CLAIM:
    case EntryKind.COMPARISON:
    case EntryKind.PATTERN:
    case EntryKind.DECIDED:
    case EntryKind.VERIFIED:
      if (target?.kind === EntryKind.QUESTION) {
        return DiscourseRelationKind.ANSWERS;
      }
      return isOpposingNarrative(entry?.summaryText) ? DiscourseRelationKind.OPPOSES : DiscourseRelationKind.SUPPORTS;
    default:
      return DiscourseRelationKind.SUPPORTS;
  }
}

export function isOpposingNarrative(text) {
  const lowered = String(text ?? "").toLowerCase();
  return OPPOSING_CUES.some((cue) => lowered.includes(cue));
}

export function inferHeuristicRelations(entries) {
  const sorted = [...(entries ?? [])].sort(
    (lhs, rhs) => new Date(lhs.createdAt).getTime() - new Date(rhs.createdAt).getTime()
  );
  const relations = [];
  for (let index = 0; index < sorted.length; index += 1) {
    const entry = sorted[index];
    const target = relationTarget(entry, sorted.slice(0, index));
    if (!target) {
      continue;
    }
    relations.push(
      createDiscourseRelation({
        sourceEntryID: entry.id,
        targetEntryID: target.id,
        kind: relationKind(entry, target),
        confidence: 0.72
      })
    );
  }
  return relations;
}

export class DiscourseInferenceEngine {
  findCandidatePairs(entries, threshold = 10) {
    const sorted = [...(entries ?? [])].sort(
      (lhs, rhs) => new Date(lhs.createdAt).getTime() - new Date(rhs.createdAt).getTime()
    );
    if (sorted.length < 2) {
      return [];
    }

    const result = [];
    for (let i = 1; i < sorted.length; i += 1) {
      const source = sorted[i];
      const windowStart = Math.max(0, i - 15);
      const topTwo = [];

      for (let j = windowStart; j < i; j += 1) {
        const target = sorted[j];
        const score = pairScore(source, target);
        if (score < threshold) {
          continue;
        }
        topTwo.push({
          target,
          score,
          kind: relationKind(source, target)
        });
        topTwo.sort((lhs, rhs) => rhs.score - lhs.score);
        if (topTwo.length > 2) {
          topTwo.pop();
        }
      }

      for (const pair of topTwo) {
        result.push({
          source,
          target: pair.target,
          similarityScore: pair.score,
          relationKind: pair.kind
        });
      }
    }

    return result;
  }
}

function pairScore(source, target) {
  return tokenOverlapScore(source, target) + objectOverlapScore(source, target) + kindAffinityScore(source.kind, target.kind);
}

function tokenOverlapScore(source, target) {
  const sourceTokens = new Set(tokenizeForSearch(source?.body?.text ?? source?.summaryText ?? ""));
  const targetTokens = new Set(tokenizeForSearch(target?.body?.text ?? target?.summaryText ?? ""));
  if (sourceTokens.size === 0 || targetTokens.size === 0) {
    return 0;
  }
  const intersection = [...sourceTokens].filter((token) => targetTokens.has(token)).length;
  const union = new Set([...sourceTokens, ...targetTokens]).size;
  return union === 0 ? 0 : Math.round((intersection / union) * 20);
}

function objectOverlapScore(source, target) {
  const sourceNames = new Set((source?.objectMentions ?? []).map((item) => String(item?.name ?? "").toLowerCase()));
  const targetNames = new Set((target?.objectMentions ?? []).map((item) => String(item?.name ?? "").toLowerCase()));
  let overlap = 0;
  for (const name of sourceNames) {
    if (targetNames.has(name)) {
      overlap += 1;
    }
  }
  return Math.min(overlap * 4, 12);
}

function kindAffinityScore(sourceKind, targetKind) {
  return KIND_AFFINITY.get(`${sourceKind}::${targetKind}`) ?? 0;
}

function firstMatching(entries, kinds) {
  return entries.find((entry) => kinds.includes(entry?.kind)) ?? null;
}
