import { MemoryPipeline } from "../memory/memoryPipeline.js";
import { RetrievalEngine } from "../retrieval/retrievalEngine.js";

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
    return this.#enqueue(() => this.store.upsertThread(thread));
  }

  saveEntry(entry) {
    return this.#enqueue(() => {
      this.store.upsertEntry(entry);
      this.memoryPipeline.recordWorking(entry);
      if (entry.kind === "source") {
        this.memoryPipeline.recordSource(entry);
      }
    });
  }

  deleteEntries(entryIDs) {
    return this.#enqueue(() => {
      this.store.deleteEntries(entryIDs);
    });
  }

  saveClaim(claim) {
    return this.#enqueue(() => {
      this.store.upsertClaim(claim);
      this.memoryPipeline.recordSemantic(claim);
    });
  }

  saveAnchor(anchor) {
    return this.#enqueue(() => {
      this.store.upsertAnchor(anchor);
      this.memoryPipeline.recordEpisodic(anchor);
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
      const memoryRecords = this.memoryPipeline.fetchThreadMemory(thread.id);
      const documents = this.retrievalEngine.buildThreadDocuments({
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

  flush() {
    return this.#writeTail;
  }

  #writeTail;

  #enqueue(operation) {
    const run = this.#writeTail.then(() => operation());
    this.#writeTail = run.catch(() => {});
    return run;
  }
}
