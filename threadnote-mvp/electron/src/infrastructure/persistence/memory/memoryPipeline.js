import {
  ClaimStatus,
  EntryKind,
  MemoryScope,
  createMemoryRecord
} from "../../../domain/models/threadnoteModels.js";

const SUBSTANTIVE_ENTRY_KINDS = new Set([
  EntryKind.NOTE,
  EntryKind.QUESTION,
  EntryKind.SOURCE
]);

export class MemoryPipeline {
  constructor({ store }) {
    this.store = store;
  }

  buildThreadMemory({ entries = [], claims = [], anchors = [] }) {
    const records = [];

    for (const entry of entries) {
      const working = this.recordWorking(entry);
      if (working) {
        records.push(working);
      }

      const source = this.recordSource(entry);
      if (source) {
        records.push(source);
      }
    }

    for (const claim of claims) {
      const semantic = this.recordSemantic(claim);
      if (semantic) {
        records.push(semantic);
      }
    }

    for (const anchor of anchors) {
      const episodic = this.recordEpisodic(anchor);
      if (episodic) {
        records.push(episodic);
      }
    }

    return records;
  }

  recordWorking(entry) {
    if (!entry?.threadID || !SUBSTANTIVE_ENTRY_KINDS.has(entry.kind)) {
      return null;
    }
    const text = String(entry.summaryText ?? entry.body?.text ?? "").trim();
    if (!text) {
      return null;
    }
    return createMemoryRecord({
      threadID: entry.threadID,
      scope: MemoryScope.WORKING,
      text,
      provenance: `entry:${entry.id}`,
      createdAt: entry.createdAt
    });
  }

  recordEpisodic(anchor) {
    const text = [anchor?.stateSummary, anchor?.coreQuestion].filter(Boolean).join(" -- ");
    if (!anchor?.threadID || !text) {
      return null;
    }
    return createMemoryRecord({
      threadID: anchor.threadID,
      scope: MemoryScope.EPISODIC,
      text,
      provenance: `anchor:${anchor.id}`,
      createdAt: anchor.createdAt
    });
  }

  recordSemantic(claim) {
    if (!claim?.threadID || claim.status !== ClaimStatus.STABLE || !claim.statement) {
      return null;
    }
    return createMemoryRecord({
      threadID: claim.threadID,
      scope: MemoryScope.SEMANTIC,
      text: claim.statement,
      provenance: `claim:${claim.id}`,
      createdAt: claim.updatedAt
    });
  }

  recordSource(entry) {
    if (!entry?.threadID || entry.kind !== EntryKind.SOURCE) {
      return null;
    }
    const title = entry.body?.title ?? entry.summaryText ?? "";
    const url = entry.body?.url ?? entry.sourceMetadata?.locator ?? "";
    const text = [title, url].filter(Boolean).join(" ");
    if (!text) {
      return null;
    }
    return createMemoryRecord({
      threadID: entry.threadID,
      scope: MemoryScope.SOURCE,
      text,
      provenance: `entry:${entry.id}`,
      createdAt: entry.createdAt
    });
  }

  fetchThreadMemory(threadID, scope = null) {
    return this.store.fetchMemoryRecords(threadID, scope);
  }
}
