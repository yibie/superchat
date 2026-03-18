import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  ClaimStatus,
  EntryKind,
  EntryStatus,
  MemoryScope,
  ThreadGoalStage,
  ThreadGoalType,
  createAnchor,
  createClaim,
  createEntry,
  createObjectMention,
  createThreadAISnapshot,
  createThreadRecord,
  createThreadTask
} from "../../src/domain/models/threadnoteModels.js";
import {
  DiscourseInferenceEngine,
  inferHeuristicRelations,
  isOpposingNarrative,
  primaryRelation
} from "../../src/domain/discourse/discourseHeuristics.js";
import { deriveResources, resourceCounts } from "../../src/domain/resources/resourceDerivation.js";
import { SQLitePersistenceStore } from "../../src/infrastructure/persistence/stores/sqlitePersistenceStore.js";
import { ThreadnoteRepository } from "../../src/infrastructure/persistence/repositories/threadnoteRepository.js";

function makeTempDatabasePath() {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-clean-room-"));
  return path.join(directory, "threadnote.sqlite");
}

function makeThread({ title, lastActiveAt = new Date() }) {
  return createThreadRecord({
    title,
    prompt: title,
    goalLayer: {
      goalStatement: title,
      goalType: ThreadGoalType.RESEARCH,
      successCondition: "Done",
      currentStage: ThreadGoalStage.FRAMING
    },
    createdAt: lastActiveAt,
    updatedAt: lastActiveAt,
    lastActiveAt
  });
}

test("clean-room discourse heuristics infer relation kind and candidate pairs", () => {
  const question = createEntry({
    threadID: "t1",
    kind: EntryKind.QUESTION,
    summaryText: "Why is Atlas launch blocked?",
    createdAt: "2026-03-15T10:00:00.000Z"
  });
  const claim = createEntry({
    threadID: "t1",
    kind: EntryKind.CLAIM,
    summaryText: "Atlas launch is blocked by legal review",
    objectMentions: [createObjectMention({ name: "Atlas" })],
    createdAt: "2026-03-15T10:01:00.000Z"
  });
  const evidence = createEntry({
    threadID: "t1",
    kind: EntryKind.EVIDENCE,
    summaryText: "However legal review has not cleared export terms",
    objectMentions: [createObjectMention({ name: "Atlas" })],
    createdAt: "2026-03-15T10:02:00.000Z"
  });

  const relations = inferHeuristicRelations([question, claim, evidence]);
  assert.equal(relations.length, 2);
  assert.equal(relations[0].kind, "answers");
  assert.equal(relations[1].kind, "opposes");
  assert.equal(primaryRelation(evidence.id, relations).targetEntryID, claim.id);
  assert.equal(isOpposingNarrative(evidence.summaryText), true);

  const pairs = new DiscourseInferenceEngine().findCandidatePairs([question, claim, evidence], 5);
  assert.equal(pairs.some((pair) => pair.source.id === evidence.id && pair.target.id === claim.id), true);
});

test("clean-room discourse heuristics remain stable for chinese token overlap", () => {
  const source = createEntry({
    threadID: "t1",
    kind: EntryKind.CLAIM,
    summaryText: "一个思维盒子应该容纳所有想法",
    createdAt: "2026-03-15T10:00:00.000Z"
  });
  const target = createEntry({
    threadID: "t1",
    kind: EntryKind.QUESTION,
    summaryText: "如何设计出一个思维盒子？",
    createdAt: "2026-03-15T10:01:00.000Z"
  });

  const pairs = new DiscourseInferenceEngine().findCandidatePairs([source, target], 1);
  assert.equal(pairs.some((pair) => pair.source.id === target.id && pair.target.id === source.id), true);
});

test("clean-room resource derivation classifies link media mention and counts", () => {
  const threadID = "thread-resource";
  const entries = [
    createEntry({
      threadID,
      kind: EntryKind.SOURCE,
      body: { url: "https://example.com/atlas" },
      summaryText: "Atlas memo"
    }),
    createEntry({
      threadID,
      kind: EntryKind.NOTE,
      body: { kind: "image", url: "file:///tmp/atlas.png" },
      summaryText: "Screenshot"
    }),
    createEntry({
      threadID,
      kind: EntryKind.NOTE,
      body: { url: "https://example.com/owner" },
      summaryText: "Talk to @Atlas owner",
      objectMentions: [createObjectMention({ name: "Atlas" })]
    }),
    createEntry({
      threadID,
      kind: EntryKind.NOTE,
      body: {
        attachments: [
          { relativePath: "attachments/atlas.png", fileName: "atlas.png", mimeType: "image/png" },
          { relativePath: "attachments/brief.pdf", fileName: "brief.pdf", mimeType: "application/pdf" }
        ]
      },
      summaryText: "Fresh uploads"
    })
  ];

  const resources = deriveResources(entries);
  const counts = resourceCounts(resources);
  assert.equal(resources.length, 6);
  assert.equal(counts.linkCount, 2);
  assert.equal(counts.mediaCount, 3);
  assert.equal(counts.mentionCount, 1);
  assert.equal(resources.some((resource) => resource.kind === "link" && resource.entry.id === entries[2].id), true);
  assert.equal(resources.some((resource) => resource.kind === "mention" && resource.title === "@Atlas"), true);
  assert.equal(resources.some((resource) => resource.attachment?.fileName === "atlas.png" && resource.locator === "attachments/atlas.png"), true);
  assert.equal(resources.some((resource) => resource.attachment?.fileName === "brief.pdf" && resource.sourceKind === "document"), true);
});

test("clean-room resource derivation includes inbox link attachment and mention resources", () => {
  const entries = [
    createEntry({
      threadID: null,
      kind: EntryKind.NOTE,
      summaryText: "https://example.com/inbox"
    }),
    createEntry({
      threadID: null,
      kind: EntryKind.NOTE,
      body: {
        attachments: [
          { relativePath: "attachments/inbox.png", fileName: "inbox.png", mimeType: "image/png" }
        ]
      },
      summaryText: "Fresh upload"
    }),
    createEntry({
      threadID: null,
      kind: EntryKind.NOTE,
      summaryText: "Talk to @Atlas",
      objectMentions: [createObjectMention({ name: "Atlas" })]
    })
  ];

  const resources = deriveResources(entries);
  const counts = resourceCounts(resources);
  assert.equal(resources.length, 3);
  assert.equal(counts.linkCount, 1);
  assert.equal(counts.mediaCount, 1);
  assert.equal(counts.mentionCount, 1);
  assert.equal(resources.some((resource) => resource.kind === "link" && resource.threadID == null), true);
  assert.equal(resources.some((resource) => resource.attachment?.fileName === "inbox.png" && resource.threadID == null), true);
  assert.equal(resources.some((resource) => resource.kind === "mention" && resource.threadID == null), true);
});

test("clean-room schema and persistence store persist core entities and ai snapshots", () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const thread = makeThread({ title: "Atlas relaunch" });
  const entry = createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    summaryText: "Atlas relaunch needs new launch checklist",
    createdAt: "2026-03-15T12:00:00.000Z"
  });
  const claim = createClaim({
    threadID: thread.id,
    originEntryID: entry.id,
    statement: "Atlas needs launch checklist",
    status: ClaimStatus.STABLE
  });
  const anchor = createAnchor({
    threadID: thread.id,
    coreQuestion: "What blocks Atlas relaunch?",
    stateSummary: "Checklist and legal review still open"
  });
  const task = createThreadTask({
    threadID: thread.id,
    title: "Draft launch checklist"
  });
  const snapshot = createThreadAISnapshot({
    threadID: thread.id,
    contentFingerprint: "fp-1",
    headline: "Atlas relaunch still blocked"
  });

  store.upsertThread(thread);
  store.upsertEntry(entry);
  store.upsertClaim(claim);
  store.upsertAnchor(anchor);
  store.upsertTask(task);
  store.upsertAISnapshot(snapshot);

  const reloaded = store.fetchSnapshot();
  assert.equal(reloaded.threads.length, 1);
  assert.equal(reloaded.entries.length, 1);
  assert.equal(reloaded.claims.length, 1);
  assert.equal(reloaded.anchors.length, 1);
  assert.equal(reloaded.tasks.length, 1);
  assert.equal(reloaded.aiSnapshots[0].headline, "Atlas relaunch still blocked");
});

test("clean-room store rebuilds thread aggregate and fingerprint basis from raw facts", () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const thread = makeThread({ title: "Aggregate rebuild" });
  store.upsertThread(thread);

  const note = createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    status: EntryStatus.SOLVED,
    summaryText: "Solved note",
    createdAt: "2026-03-15T09:00:00.000Z"
  });
  const evidence = createEntry({
    threadID: thread.id,
    kind: EntryKind.EVIDENCE,
    status: EntryStatus.VERIFIED,
    summaryText: "Verified evidence",
    createdAt: "2026-03-15T09:01:00.000Z"
  });
  const source = createEntry({
    threadID: thread.id,
    kind: EntryKind.SOURCE,
    status: EntryStatus.DROPPED,
    summaryText: "Dropped source",
    createdAt: "2026-03-15T09:02:00.000Z"
  });
  const claim = createClaim({
    threadID: thread.id,
    originEntryID: note.id,
    statement: "This is stable",
    status: ClaimStatus.STABLE,
    updatedAt: "2026-03-15T09:03:00.000Z"
  });
  const anchor = createAnchor({
    threadID: thread.id,
    coreQuestion: "What changed?",
    stateSummary: "Anchor is current",
    createdAt: "2026-03-15T09:04:00.000Z"
  });

  store.upsertEntry(note);
  store.upsertEntry(evidence);
  store.upsertEntry(source);
  store.upsertClaim(claim);
  store.upsertAnchor(anchor);
  store.upsertThreadAggregate(thread.id, {
    entryCount: 999,
    evidenceCount: 999,
    sourceCount: 999,
    stableClaimCount: 999,
    anchorCount: 999,
    decidedCount: 999,
    solvedCount: 999,
    verifiedCount: 999,
    droppedCount: 999,
    updatedAt: "2026-03-15T08:00:00.000Z"
  });

  const aggregate = store.rebuildThreadAggregate(thread.id);
  assert.deepEqual(
    {
      entryCount: aggregate.entryCount,
      evidenceCount: aggregate.evidenceCount,
      sourceCount: aggregate.sourceCount,
      stableClaimCount: aggregate.stableClaimCount,
      anchorCount: aggregate.anchorCount,
      solvedCount: aggregate.solvedCount,
      verifiedCount: aggregate.verifiedCount,
      droppedCount: aggregate.droppedCount
    },
    {
      entryCount: 3,
      evidenceCount: 1,
      sourceCount: 1,
      stableClaimCount: 1,
      anchorCount: 1,
      solvedCount: 1,
      verifiedCount: 1,
      droppedCount: 1
    }
  );
  assert.equal(aggregate.stableClaimUpdatedAt, "2026-03-15T09:03:00.000Z");
  assert.equal(aggregate.latestAnchorCreatedAt, "2026-03-15T09:04:00.000Z");

  const counts = store.fetchThreadCounts(thread.id);
  assert.equal(counts.entryCount, 3);
  assert.equal(counts.stableClaimCount, 1);
  assert.equal(counts.anchorCount, 1);
  assert.equal(counts.solvedCount, 1);
  assert.equal(counts.verifiedCount, 1);
  assert.equal(counts.droppedCount, 1);

  const fingerprintBasis = store.fetchThreadFingerprintBasis(thread.id);
  assert.equal(fingerprintBasis.evidenceCount, 1);
  assert.equal(fingerprintBasis.sourceCount, 1);
  assert.equal(fingerprintBasis.stableClaimCount, 1);
  assert.equal(fingerprintBasis.anchorCount, 1);
  assert.equal(fingerprintBasis.solvedCount, 1);
  assert.equal(fingerprintBasis.verifiedCount, 1);
  assert.equal(fingerprintBasis.droppedCount, 1);
});

test("clean-room repository serializes writes, syncs retrieval, and exposes recall", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const atlas = makeThread({ title: "Atlas launch", lastActiveAt: new Date("2026-03-15T09:00:00.000Z") });
  const hiring = makeThread({ title: "Hiring plan", lastActiveAt: new Date("2026-03-15T08:00:00.000Z") });

  await repository.saveThread(atlas);
  await repository.saveThread(hiring);

  const atlasEntry = createEntry({
    threadID: atlas.id,
    kind: EntryKind.NOTE,
    status: EntryStatus.DECIDED,
    statusMetadata: { source: "ai", updatedAt: "2026-03-15T09:02:00.000Z" },
    summaryText: "Atlas launch checklist and legal review",
    createdAt: "2026-03-15T09:01:00.000Z"
  });
  const atlasClaim = createClaim({
    threadID: atlas.id,
    originEntryID: atlasEntry.id,
    statement: "Atlas launch needs legal review",
    status: ClaimStatus.STABLE
  });
  const hiringEntry = createEntry({
    threadID: hiring.id,
    kind: EntryKind.NOTE,
    summaryText: "Hiring interview rubric and scorecard",
    createdAt: "2026-03-15T08:01:00.000Z"
  });

  await Promise.all([
    repository.saveEntry(atlasEntry),
    repository.saveClaim(atlasClaim),
    repository.saveEntry(hiringEntry)
  ]);
  await repository.flush();

  const recalled = repository.retrievalEngine.recall("Atlas legal review", { limit: 5 });
  assert.equal(recalled.length >= 1, true);
  assert.equal(recalled[0].threadID, atlas.id);
  const atlasEntryDoc = recalled.find((item) => item.ownerID === atlasEntry.id);
  assert.equal(Boolean(atlasEntryDoc?.metadataJSON?.includes("\"status\":\"decided\"")), true);
  assert.equal(Boolean(atlasEntryDoc?.metadataJSON?.includes("\"statusSource\":\"ai\"")), true);

  const ranked = repository.retrievalEngine.rankThreads("Atlas review", [atlas, hiring]);
  assert.equal(ranked[0].thread.id, atlas.id);
});

test("clean-room repository syncThreadRetrieval also rebuilds aggregate for bootstrap paths", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const thread = makeThread({ title: "Bootstrap aggregate" });
  await repository.saveThread(thread);

  const note = createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    summaryText: "Bootstrapped note",
    createdAt: "2026-03-15T09:00:00.000Z"
  });
  const evidence = createEntry({
    threadID: thread.id,
    kind: EntryKind.EVIDENCE,
    summaryText: "Bootstrapped evidence",
    createdAt: "2026-03-15T09:01:00.000Z"
  });
  const claim = createClaim({
    threadID: thread.id,
    originEntryID: note.id,
    statement: "Bootstrapped stable claim",
    status: ClaimStatus.STABLE,
    updatedAt: "2026-03-15T09:02:00.000Z"
  });
  const anchor = createAnchor({
    threadID: thread.id,
    coreQuestion: "What was imported?",
    stateSummary: "Imported aggregate state",
    createdAt: "2026-03-15T09:03:00.000Z"
  });

  store.upsertEntry(note);
  store.upsertEntry(evidence);
  store.upsertClaim(claim);
  store.upsertAnchor(anchor);

  await repository.syncThreadRetrieval({
    thread,
    entries: store.fetchEntries(thread.id),
    claims: store.fetchClaims(thread.id),
    anchors: store.fetchAnchors(thread.id)
  });
  await repository.flush();

  const counts = store.fetchThreadCounts(thread.id);
  assert.equal(counts.entryCount, 2);
  assert.equal(counts.evidenceCount, 1);
  assert.equal(counts.stableClaimCount, 1);
  assert.equal(counts.anchorCount, 1);
});

test("clean-room repository retrieval scoring applies owner type and recency boosts in one engine", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const atlas = makeThread({ title: "Atlas launch", lastActiveAt: new Date("2026-03-15T09:00:00.000Z") });

  await repository.saveThread(atlas);
  await repository.saveEntry(createEntry({
    threadID: atlas.id,
    kind: EntryKind.NOTE,
    summaryText: "Atlas legal review backlog",
    createdAt: "2026-02-01T09:01:00.000Z"
  }));
  await repository.saveClaim(createClaim({
    threadID: atlas.id,
    statement: "Atlas legal review",
    status: ClaimStatus.STABLE,
    createdAt: "2026-03-15T09:01:00.000Z"
  }));
  await repository.flush();

  const recalled = repository.retrievalEngine.recall("Atlas legal review", { limit: 5 });
  assert.equal(recalled[0].ownerType, "claim");
  assert.equal(recalled[0].score >= recalled[1].score, true);
});

test("clean-room repository retrieval exposes chinese recall diagnostics gap deterministically", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const thread = makeThread({ title: "思维盒子" });

  await repository.saveThread(thread);
  await repository.saveEntry(createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    summaryText: "一个思维盒子，应该可以容纳人关于某一方面的所有想法",
    createdAt: "2026-03-15T09:01:00.000Z"
  }));
  await repository.saveClaim(createClaim({
    threadID: thread.id,
    statement: "思维盒子应该容纳某一方面的所有想法",
    status: ClaimStatus.STABLE
  }));
  await repository.flush();

  const recalled = repository.retrievalEngine.recall("如何设计出一个思维盒子？", { limit: 5 });
  assert.equal(Array.isArray(recalled), true);
  assert.equal(recalled.every((item) => item.threadID === thread.id), true);
});

test("clean-room repository rewrites memory and retrieval when entries move or change", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const atlas = makeThread({ title: "Atlas launch", lastActiveAt: new Date("2026-03-15T09:00:00.000Z") });
  const hiring = makeThread({ title: "Hiring plan", lastActiveAt: new Date("2026-03-15T08:00:00.000Z") });

  await repository.saveThread(atlas);
  await repository.saveThread(hiring);

  const entry = createEntry({
    threadID: atlas.id,
    kind: EntryKind.NOTE,
    summaryText: "Vendor invoice mismatch blocks payout",
    createdAt: "2026-03-15T09:01:00.000Z"
  });

  await repository.saveEntry(entry);
  await repository.flush();

  assert.equal(repository.fetchMemory(atlas.id, MemoryScope.WORKING).length, 1);
  assert.equal(repository.retrievalEngine.recall("invoice mismatch", { threadID: atlas.id }).length >= 1, true);

  await repository.saveEntry({
    ...entry,
    threadID: hiring.id,
    summaryText: "Candidate scorecard needs calibration"
  });
  await repository.flush();

  assert.equal(repository.fetchMemory(atlas.id, MemoryScope.WORKING).length, 0);
  assert.equal(repository.fetchMemory(hiring.id, MemoryScope.WORKING).length, 1);
  assert.equal(repository.retrievalEngine.recall("invoice mismatch", { threadID: atlas.id }).length, 0);
  assert.equal(repository.retrievalEngine.recall("scorecard calibration", { threadID: hiring.id }).length >= 1, true);
  assert.equal(store.fetchThreadCounts(atlas.id).entryCount, 0);
  assert.equal(store.fetchThreadCounts(hiring.id).entryCount, 1);
});

test("clean-room memory pipeline writes working episodic semantic and source scopes", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const thread = makeThread({ title: "Atlas knowledge base" });

  await repository.saveThread(thread);

  const sourceEntry = createEntry({
    threadID: thread.id,
    kind: EntryKind.SOURCE,
    body: { title: "Atlas spec", url: "https://example.com/spec" },
    summaryText: "Atlas specification"
  });
  const workingEntry = createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    summaryText: "Need to reconcile launch checklist with spec"
  });
  const stableClaim = createClaim({
    threadID: thread.id,
    originEntryID: workingEntry.id,
    statement: "Launch checklist depends on export review",
    status: ClaimStatus.STABLE
  });
  const anchor = createAnchor({
    threadID: thread.id,
    coreQuestion: "What should restart explain?",
    stateSummary: "Checklist and spec remain primary blockers"
  });

  await repository.saveEntry(sourceEntry);
  await repository.saveEntry(workingEntry);
  await repository.saveClaim(stableClaim);
  await repository.saveAnchor(anchor);
  await repository.flush();

  const memory = repository.fetchMemory(thread.id);
  assert.equal(memory.length, 5);
  assert.equal(repository.fetchMemory(thread.id, MemoryScope.WORKING).length, 2);
  assert.equal(repository.fetchMemory(thread.id, MemoryScope.SOURCE).length, 1);
  assert.equal(repository.fetchMemory(thread.id, MemoryScope.SEMANTIC).length, 1);
  assert.equal(repository.fetchMemory(thread.id, MemoryScope.EPISODIC).length, 1);
});

test("clean-room repository incremental knowledge sync avoids thread-wide replacement on normal writes", async () => {
  class SpyStore extends SQLitePersistenceStore {
    constructor(databasePath) {
      super(databasePath);
      this.fullReplaceCounts = { memory: 0, retrieval: 0 };
      this.aggregateOps = { patch: 0, rebuild: 0 };
    }

    replaceMemoryRecords(threadID, records) {
      this.fullReplaceCounts.memory += 1;
      return super.replaceMemoryRecords(threadID, records);
    }

    replaceRetrievalDocuments(threadID, documents) {
      this.fullReplaceCounts.retrieval += 1;
      return super.replaceRetrievalDocuments(threadID, documents);
    }

    patchThreadAggregate(threadID, patch) {
      this.aggregateOps.patch += 1;
      return super.patchThreadAggregate(threadID, patch);
    }

    rebuildThreadAggregate(threadID) {
      this.aggregateOps.rebuild += 1;
      return super.rebuildThreadAggregate(threadID);
    }
  }

  const store = new SpyStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const thread = makeThread({ title: "Incremental sync" });

  await repository.saveThread(thread);
  await repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.NOTE,
      summaryText: "A plain note should not trigger full-thread rebuild",
      createdAt: "2026-03-15T09:00:00.000Z"
    })
  );
  await repository.saveClaim(
    createClaim({
      threadID: thread.id,
      statement: "A stable claim should only update semantic scope",
      status: ClaimStatus.STABLE,
      createdAt: "2026-03-15T09:01:00.000Z",
      updatedAt: "2026-03-15T09:01:30.000Z"
    })
  );
  await repository.saveAnchor(
    createAnchor({
      threadID: thread.id,
      coreQuestion: "What matters now?",
      stateSummary: "Anchor updates should stay episodic",
      createdAt: "2026-03-15T09:02:00.000Z"
    })
  );
  await repository.flush();

  assert.deepEqual(store.fullReplaceCounts, { memory: 0, retrieval: 0 });
  assert.equal(store.aggregateOps.patch >= 3, true);
  assert.equal(store.aggregateOps.rebuild, 0);
  assert.equal(store.fetchMemoryRecords(thread.id).length > 0, true);
  assert.equal(store.fetchRetrievalDocumentsRecency({ threadID: thread.id, limit: 20 }).length > 0, true);
});

test("clean-room repository maintains thread aggregate on entry claim anchor updates and deletes", async () => {
  const store = new SQLitePersistenceStore(makeTempDatabasePath());
  const repository = new ThreadnoteRepository({ store });
  const thread = makeThread({ title: "Aggregate maintained" });

  await repository.saveThread(thread);
  const note = createEntry({
    threadID: thread.id,
    kind: EntryKind.NOTE,
    status: EntryStatus.OPEN,
    summaryText: "Plain note",
    createdAt: "2026-03-15T09:00:00.000Z"
  });
  await repository.saveEntry(note);
  await repository.flush();

  assert.equal(store.fetchThreadCounts(thread.id).entryCount, 1);
  assert.equal(store.fetchThreadCounts(thread.id).solvedCount, 0);

  await repository.saveEntry({
    ...note,
    status: EntryStatus.SOLVED,
    statusMetadata: { source: "ai", updatedAt: "2026-03-15T09:05:00.000Z" }
  });
  await repository.saveClaim(createClaim({
    threadID: thread.id,
    originEntryID: note.id,
    statement: "Stable enough",
    status: ClaimStatus.STABLE,
    updatedAt: "2026-03-15T09:06:00.000Z"
  }));
  await repository.saveAnchor(createAnchor({
    threadID: thread.id,
    coreQuestion: "Where are we now?",
    stateSummary: "Anchored",
    createdAt: "2026-03-15T09:07:00.000Z"
  }));
  await repository.flush();

  let counts = store.fetchThreadCounts(thread.id);
  assert.equal(counts.entryCount, 1);
  assert.equal(counts.solvedCount, 1);
  assert.equal(counts.stableClaimCount, 1);
  assert.equal(counts.anchorCount, 1);
  assert.equal(store.fetchThreadAggregate(thread.id).stableClaimUpdatedAt, "2026-03-15T09:06:00.000Z");
  assert.equal(store.fetchThreadAggregate(thread.id).latestAnchorCreatedAt, "2026-03-15T09:07:00.000Z");

  await repository.deleteEntries([note.id]);
  await repository.flush();

  counts = store.fetchThreadCounts(thread.id);
  assert.equal(counts.entryCount, 0);
  assert.equal(counts.solvedCount, 0);
  assert.equal(counts.stableClaimCount, 0);
  assert.equal(counts.anchorCount, 1);
});
