import {
  ClaimStatus,
  EntryKind,
  MemoryScope,
  createMemoryRecord
} from "../../../domain/models/threadnoteModels.js";

const SUBSTANTIVE_ENTRY_KINDS = new Set([
  EntryKind.NOTE,
  EntryKind.CLAIM,
  EntryKind.QUESTION,
  EntryKind.EVIDENCE,
  EntryKind.SOURCE,
  EntryKind.IDEA,
  EntryKind.PLAN,
  EntryKind.DECIDED,
  EntryKind.SOLVED,
  EntryKind.VERIFIED,
  EntryKind.DROPPED
]);

export class MemoryPipeline {
  constructor({ store }) {
    this.store = store;
  }

  recordWorking(entry) {
    if (!entry?.threadID || !SUBSTANTIVE_ENTRY_KINDS.has(entry.kind)) {
      return null;
    }
    const text = String(entry.summaryText ?? entry.body?.text ?? "").trim();
    if (!text) {
      return null;
    }
    return this.#persist(
      createMemoryRecord({
        threadID: entry.threadID,
        scope: MemoryScope.WORKING,
        text,
        provenance: `entry:${entry.id}`,
        createdAt: entry.createdAt
      })
    );
  }

  recordEpisodic(anchor) {
    const text = [anchor?.stateSummary, anchor?.coreQuestion].filter(Boolean).join(" -- ");
    if (!anchor?.threadID || !text) {
      return null;
    }
    return this.#persist(
      createMemoryRecord({
        threadID: anchor.threadID,
        scope: MemoryScope.EPISODIC,
        text,
        provenance: `anchor:${anchor.id}`,
        createdAt: anchor.createdAt
      })
    );
  }

  recordSemantic(claim) {
    if (!claim?.threadID || claim.status !== ClaimStatus.STABLE || !claim.statement) {
      return null;
    }
    return this.#persist(
      createMemoryRecord({
        threadID: claim.threadID,
        scope: MemoryScope.SEMANTIC,
        text: claim.statement,
        provenance: `claim:${claim.id}`,
        createdAt: claim.updatedAt
      })
    );
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
    return this.#persist(
      createMemoryRecord({
        threadID: entry.threadID,
        scope: MemoryScope.SOURCE,
        text,
        provenance: `entry:${entry.id}`,
        createdAt: entry.createdAt
      })
    );
  }

  fetchThreadMemory(threadID, scope = null) {
    return this.store.fetchMemoryRecords(threadID, scope);
  }

  #persist(record) {
    this.store.insertMemoryRecord(record);
    return record;
  }
}
