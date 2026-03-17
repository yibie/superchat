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
    return this.store.fetchSnapshot();
  }

  configureEmbedding(embedding) {
    this.embedding = embedding ?? null;
  }

  saveThread(thread) {
    return this.#enqueue(() => {
      this.store.upsertThread(thread);
      this.#syncThreadKnowledge(thread.id);
    });
  }

  saveEntry(entry) {
    return this.#enqueue(() => {
      const previous = this.store.fetchEntries().find((item) => item.id === entry.id) ?? null;
      this.store.upsertEntry(entry);
      this.#syncThreadKnowledge(previous?.threadID);
      this.#syncThreadKnowledge(entry.threadID);
    });
  }

  deleteEntries(entryIDs) {
    return this.#enqueue(() => {
      const affectedThreadIDs = new Set(
        this.store
          .fetchEntries()
          .filter((entry) => entryIDs.includes(entry.id))
          .map((entry) => entry.threadID)
          .filter(Boolean)
      );
      this.store.deleteEntries(entryIDs);
      for (const threadID of affectedThreadIDs) {
        this.#syncThreadKnowledge(threadID);
      }
    });
  }

  saveClaim(claim) {
    return this.#enqueue(() => {
      const previous = this.store.fetchClaims().find((item) => item.id === claim.id) ?? null;
      this.store.upsertClaim(claim);
      this.#syncThreadKnowledge(previous?.threadID);
      this.#syncThreadKnowledge(claim.threadID);
    });
  }

  saveAnchor(anchor) {
    return this.#enqueue(() => {
      const previous = this.store.fetchAnchors().find((item) => item.id === anchor.id) ?? null;
      this.store.upsertAnchor(anchor);
      this.#syncThreadKnowledge(previous?.threadID);
      this.#syncThreadKnowledge(anchor.threadID);
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
    });
  }

  fetchMemory(threadID, scope = null) {
    return this.memoryPipeline.fetchThreadMemory(threadID, scope);
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
  }
}
