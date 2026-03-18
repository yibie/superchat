import { MemoryPipeline } from "../memory/memoryPipeline.js";
import { RetrievalEngine } from "../retrieval/retrievalEngine.js";
import { buildThreadRetrievalDocuments } from "../retrieval/retrievalDocumentBuilder.js";

export class ThreadnoteRepository {
  constructor({ store }) {
    this.store = store;
    this.retrievalEngine = new RetrievalEngine({ store });
    this.memoryPipeline = new MemoryPipeline({ store });
    this.embedding = null;
    this.#writeTail = Promise.resolve();
  }

  loadSnapshot() {
    return this.store.fetchWorkspaceSnapshot();
  }

  configureEmbedding(embedding) {
    this.embedding = embedding ?? null;
  }

  saveThread(thread) {
    return this.#enqueue(() => {
      this.store.upsertThread(thread);
    });
  }

  saveEntry(entry) {
    return this.#enqueue(() => {
      const previous = this.store.fetchEntry(entry.id);
      this.store.upsertEntry(entry);
      this.#syncEntryKnowledge(previous, entry);
      this.#syncEntryAggregate(previous, entry);
    });
  }

  deleteEntries(entryIDs) {
    return this.#enqueue(() => {
      const entries = this.store.fetchEntriesByIDs(entryIDs);
      const claims = this.store.fetchClaimsByOriginEntryIDs(entryIDs);
      this.store.deleteEntries(entryIDs);
      for (const entry of entries) {
        this.#deleteEntryKnowledge(entry);
      }
      for (const claim of claims) {
        this.#deleteClaimKnowledge(claim);
      }
      for (const entry of entries) {
        this.#syncEntryAggregate(entry, null);
      }
      for (const claim of claims) {
        this.#syncClaimAggregate(claim, null);
      }
    });
  }

  saveClaim(claim) {
    return this.#enqueue(() => {
      const previous = this.store.fetchClaim(claim.id);
      this.store.upsertClaim(claim);
      this.#syncClaimKnowledge(previous, claim);
      this.#syncClaimAggregate(previous, claim);
    });
  }

  saveAnchor(anchor) {
    return this.#enqueue(() => {
      const previous = this.store.fetchAnchor(anchor.id);
      this.store.upsertAnchor(anchor);
      this.#syncAnchorKnowledge(previous, anchor);
      this.#syncAnchorAggregate(previous, anchor);
    });
  }

  saveTask(task) {
    return this.#enqueue(() => this.store.upsertTask(task));
  }

  saveDiscourseRelation(relation) {
    return this.#enqueue(() => this.store.upsertDiscourseRelation(relation));
  }

  replaceDiscourseRelations(removingEntryIDs, relations) {
    return this.#enqueue(() => this.store.replaceDiscourseRelations(removingEntryIDs, relations));
  }

  upsertAISnapshot(snapshot) {
    return this.#enqueue(() => this.store.upsertAISnapshot(snapshot));
  }

  syncThreadRetrieval({ thread, entries = [], claims = [], anchors = [] }) {
    return this.#enqueue(() => {
      const memoryRecords = this.memoryPipeline.buildThreadMemory({
        entries,
        claims,
        anchors
      });
      this.store.replaceMemoryRecords(thread.id, memoryRecords);
      const documents = buildThreadRetrievalDocuments({
        thread,
        entries,
        claims,
        anchors,
        memoryRecords
      });
      this.store.replaceRetrievalDocuments(thread.id, documents);
      this.store.touchThreadMaterializedVersion(thread.id, {
        memory: true,
        retrieval: true
      });
      this.store.rebuildThreadAggregate(thread.id);
    });
  }

  fetchMemory(threadID, scope = null) {
    return this.memoryPipeline.fetchThreadMemory(threadID, scope);
  }

  fetchMemoryPreview(threadID, { scope = null, limit = 200 } = {}) {
    return this.store.fetchMemoryRecordPreview(threadID, { scope, limit });
  }

  countMemory(threadID, scope = null) {
    return this.store.countMemoryRecords(threadID, scope);
  }

  rebuildKnowledgeIndex() {
    return this.#enqueue(() => {
      this.rebuildKnowledgeIndexSync();
    });
  }

  rebuildKnowledgeIndexSync() {
    for (const thread of this.store.fetchThreads()) {
      this.#syncThreadKnowledge(thread.id);
    }
  }

  rebuildThreadAggregatesSync() {
    for (const thread of this.store.fetchThreads()) {
      this.store.rebuildThreadAggregate(thread.id);
    }
  }

  flush() {
    return this.#writeTail;
  }

  #writeTail;

  #enqueue(operation) {
    const run = this.#writeTail.then(() => operation());
    this.#writeTail = run.catch(() => {});
    return run;
  }

  #syncThreadKnowledge(threadID) {
    if (!threadID) {
      return;
    }
    const thread = this.store.fetchThread(threadID);
    if (!thread) {
      return;
    }
    const entries = this.store.fetchEntries(threadID);
    const claims = this.store.fetchClaims(threadID);
    const anchors = this.store.fetchAnchors(threadID);
    const memoryRecords = this.memoryPipeline.buildThreadMemory({
      entries,
      claims,
      anchors
    });
    this.store.replaceMemoryRecords(threadID, memoryRecords);
    const documents = buildThreadRetrievalDocuments({
      thread,
      entries,
      claims,
      anchors,
      memoryRecords
    });
    this.store.replaceRetrievalDocuments(threadID, documents);
    this.store.touchThreadMaterializedVersion(threadID, {
      memory: true,
      retrieval: true
    });
    this.store.rebuildThreadAggregate(threadID);
  }

  #syncEntryAggregate(previousEntry, nextEntry) {
    const previousThreadID = previousEntry?.threadID ?? null;
    const nextThreadID = nextEntry?.threadID ?? null;
    if (!previousThreadID && !nextThreadID) {
      return;
    }
    if (previousThreadID && nextThreadID && previousThreadID === nextThreadID) {
      this.store.patchThreadAggregate(nextThreadID, diffEntryAggregate(previousEntry, nextEntry));
      return;
    }
    if (previousThreadID) {
      this.store.patchThreadAggregate(previousThreadID, diffEntryAggregate(previousEntry, null));
    }
    if (nextThreadID) {
      this.store.patchThreadAggregate(nextThreadID, diffEntryAggregate(null, nextEntry));
    }
  }

  #syncClaimAggregate(previousClaim, nextClaim) {
    const previousThreadID = previousClaim?.threadID ?? null;
    const nextThreadID = nextClaim?.threadID ?? null;
    if (!previousThreadID && !nextThreadID) {
      return;
    }
    if (previousThreadID && nextThreadID && previousThreadID === nextThreadID) {
      this.#applyAggregateStrategy(nextThreadID, diffClaimAggregate(previousClaim, nextClaim));
      return;
    }
    if (previousThreadID) {
      this.#applyAggregateStrategy(previousThreadID, diffClaimAggregate(previousClaim, null));
    }
    if (nextThreadID) {
      this.#applyAggregateStrategy(nextThreadID, diffClaimAggregate(null, nextClaim));
    }
  }

  #syncAnchorAggregate(previousAnchor, nextAnchor) {
    const previousThreadID = previousAnchor?.threadID ?? null;
    const nextThreadID = nextAnchor?.threadID ?? null;
    if (!previousThreadID && !nextThreadID) {
      return;
    }
    if (previousThreadID && nextThreadID && previousThreadID === nextThreadID) {
      this.#applyAggregateStrategy(nextThreadID, diffAnchorAggregate(previousAnchor, nextAnchor));
      return;
    }
    if (previousThreadID) {
      this.#applyAggregateStrategy(previousThreadID, diffAnchorAggregate(previousAnchor, null));
    }
    if (nextThreadID) {
      this.#applyAggregateStrategy(nextThreadID, diffAnchorAggregate(null, nextAnchor));
    }
  }

  #applyAggregateStrategy(threadID, strategy) {
    if (!threadID || !strategy) {
      return;
    }
    if (strategy.type === "patch") {
      this.store.patchThreadAggregate(threadID, strategy.patch);
      return;
    }
    this.store.rebuildThreadAggregate(threadID);
  }

  #syncEntryKnowledge(previousEntry, nextEntry) {
    if (previousEntry && (!nextEntry || previousEntry.threadID !== nextEntry.threadID)) {
      this.#deleteEntryKnowledge(previousEntry);
    }
    if (!nextEntry?.threadID) {
      return;
    }
    const thread = this.store.fetchThread(nextEntry.threadID);
    if (!thread) {
      return;
    }
    const memoryRecords = this.#replaceEntityMemory({
      provenance: `entry:${nextEntry.id}`,
      records: [
        this.memoryPipeline.recordWorking(nextEntry),
        this.memoryPipeline.recordSource(nextEntry)
      ].filter(Boolean)
    });
    this.store.upsertRetrievalDocuments([
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [nextEntry],
        claims: [],
        anchors: [],
        memoryRecords: []
      }),
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [],
        claims: [],
        anchors: [],
        memoryRecords
      })
    ]);
    this.store.touchThreadMaterializedVersion(nextEntry.threadID, {
      memory: true,
      retrieval: true
    });
  }

  #syncClaimKnowledge(previousClaim, nextClaim) {
    if (previousClaim && (!nextClaim || previousClaim.threadID !== nextClaim.threadID)) {
      this.#deleteClaimKnowledge(previousClaim);
    }
    if (!nextClaim?.threadID) {
      return;
    }
    const thread = this.store.fetchThread(nextClaim.threadID);
    if (!thread) {
      return;
    }
    const memoryRecords = this.#replaceEntityMemory({
      provenance: `claim:${nextClaim.id}`,
      records: [this.memoryPipeline.recordSemantic(nextClaim)].filter(Boolean)
    });
    this.store.upsertRetrievalDocuments([
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [],
        claims: [nextClaim],
        anchors: [],
        memoryRecords: []
      }),
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [],
        claims: [],
        anchors: [],
        memoryRecords
      })
    ]);
    this.store.touchThreadMaterializedVersion(nextClaim.threadID, {
      memory: true,
      retrieval: true
    });
  }

  #syncAnchorKnowledge(previousAnchor, nextAnchor) {
    if (previousAnchor && (!nextAnchor || previousAnchor.threadID !== nextAnchor.threadID)) {
      this.#deleteAnchorKnowledge(previousAnchor);
    }
    if (!nextAnchor?.threadID) {
      return;
    }
    const thread = this.store.fetchThread(nextAnchor.threadID);
    if (!thread) {
      return;
    }
    const memoryRecords = this.#replaceEntityMemory({
      provenance: `anchor:${nextAnchor.id}`,
      records: [this.memoryPipeline.recordEpisodic(nextAnchor)].filter(Boolean)
    });
    this.store.upsertRetrievalDocuments([
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [],
        claims: [],
        anchors: [nextAnchor],
        memoryRecords: []
      }),
      ...buildThreadRetrievalDocuments({
        thread,
        entries: [],
        claims: [],
        anchors: [],
        memoryRecords
      })
    ]);
    this.store.touchThreadMaterializedVersion(nextAnchor.threadID, {
      memory: true,
      retrieval: true
    });
  }

  #replaceEntityMemory({ provenance, records }) {
    this.store.replaceMemoryRecordsByProvenance(provenance, records);
    return this.store.fetchMemoryRecordsByProvenance(provenance);
  }

  #deleteEntryKnowledge(entry) {
    if (!entry?.id) {
      return;
    }
    this.store.replaceMemoryRecordsByProvenance(`entry:${entry.id}`, []);
    this.store.deleteRetrievalDocumentsByOwners([{ ownerType: "entry", ownerID: entry.id }]);
    if (entry.threadID) {
      this.store.touchThreadMaterializedVersion(entry.threadID, {
        memory: true,
        retrieval: true
      });
    }
  }

  #deleteClaimKnowledge(claim) {
    if (!claim?.id) {
      return;
    }
    this.store.replaceMemoryRecordsByProvenance(`claim:${claim.id}`, []);
    this.store.deleteRetrievalDocumentsByOwners([{ ownerType: "claim", ownerID: claim.id }]);
    if (claim.threadID) {
      this.store.touchThreadMaterializedVersion(claim.threadID, {
        memory: true,
        retrieval: true
      });
    }
  }

  #deleteAnchorKnowledge(anchor) {
    if (!anchor?.id) {
      return;
    }
    this.store.replaceMemoryRecordsByProvenance(`anchor:${anchor.id}`, []);
    this.store.deleteRetrievalDocumentsByOwners([{ ownerType: "anchor", ownerID: anchor.id }]);
    if (anchor.threadID) {
      this.store.touchThreadMaterializedVersion(anchor.threadID, {
        memory: true,
        retrieval: true
      });
    }
  }
}

function diffEntryAggregate(previousEntry, nextEntry) {
  return {
    entryCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry ? 1 : 0)),
    evidenceCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.kind === "evidence" ? 1 : 0)),
    sourceCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.kind === "source" ? 1 : 0)),
    decidedCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.status === "decided" ? 1 : 0)),
    solvedCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.status === "solved" ? 1 : 0)),
    verifiedCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.status === "verified" ? 1 : 0)),
    droppedCountDelta: entryDelta(previousEntry, nextEntry, (entry) => (entry?.status === "dropped" ? 1 : 0))
  };
}

function diffClaimAggregate(previousClaim, nextClaim) {
  const previousStable = previousClaim?.status === "stable";
  const nextStable = nextClaim?.status === "stable";

  if (previousStable && !nextStable) {
    return { type: "rebuild" };
  }
  if (previousStable && nextStable) {
    const previousUpdatedAt = toTimestamp(previousClaim?.updatedAt);
    const nextUpdatedAt = toTimestamp(nextClaim?.updatedAt);
    if (nextUpdatedAt < previousUpdatedAt) {
      return { type: "rebuild" };
    }
    return {
      type: "patch",
      patch: {
        stableClaimCountDelta: 0,
        stableClaimUpdatedAtMax: nextClaim?.updatedAt ?? null
      }
    };
  }
  if (!previousStable && nextStable) {
    return {
      type: "patch",
      patch: {
        stableClaimCountDelta: 1,
        stableClaimUpdatedAtMax: nextClaim?.updatedAt ?? null
      }
    };
  }
  return {
    type: "patch",
    patch: {
      stableClaimCountDelta: 0
    }
  };
}

function diffAnchorAggregate(previousAnchor, nextAnchor) {
  if (previousAnchor && !nextAnchor) {
    return { type: "rebuild" };
  }
  if (!previousAnchor && nextAnchor) {
    return {
      type: "patch",
      patch: {
        anchorCountDelta: 1,
        latestAnchorCreatedAtMax: nextAnchor?.createdAt ?? null
      }
    };
  }
  if (previousAnchor && nextAnchor) {
    const previousCreatedAt = toTimestamp(previousAnchor?.createdAt);
    const nextCreatedAt = toTimestamp(nextAnchor?.createdAt);
    if (nextCreatedAt < previousCreatedAt) {
      return { type: "rebuild" };
    }
    return {
      type: "patch",
      patch: {
        anchorCountDelta: 0,
        latestAnchorCreatedAtMax: nextAnchor?.createdAt ?? null
      }
    };
  }
  return {
    type: "patch",
    patch: {
      anchorCountDelta: 0
    }
  };
}

function entryDelta(previousEntry, nextEntry, selector) {
  return Number(selector(nextEntry) ?? 0) - Number(selector(previousEntry) ?? 0);
}

function toTimestamp(value) {
  if (!value) {
    return Number.NEGATIVE_INFINITY;
  }
  return new Date(value).getTime();
}
