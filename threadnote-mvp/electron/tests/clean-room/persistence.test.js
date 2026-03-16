import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  ClaimStatus,
  EntryKind,
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
    })
  ];

  const resources = deriveResources(entries);
  const counts = resourceCounts(resources);
  assert.equal(resources.length, 4);
  assert.equal(counts.linkCount, 2);
  assert.equal(counts.mediaCount, 1);
  assert.equal(counts.mentionCount, 1);
  assert.equal(resources.some((resource) => resource.kind === "link" && resource.entry.id === entries[2].id), true);
  assert.equal(resources.some((resource) => resource.kind === "mention" && resource.title === "@Atlas"), true);
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
  await repository.syncThreadRetrieval({
    thread: atlas,
    entries: [atlasEntry],
    claims: [atlasClaim]
  });
  await repository.syncThreadRetrieval({
    thread: hiring,
    entries: [hiringEntry]
  });
  await repository.flush();

  const recalled = repository.retrievalEngine.recall("Atlas legal review", { limit: 5 });
  assert.equal(recalled.length >= 1, true);
  assert.equal(recalled[0].threadID, atlas.id);

  const ranked = repository.retrievalEngine.rankThreads("Atlas review", [atlas, hiring]);
  assert.equal(ranked[0].thread.id, atlas.id);
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
