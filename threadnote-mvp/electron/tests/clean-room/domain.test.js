import test from "node:test";
import assert from "node:assert/strict";
import {
  ClaimStatus,
  ThreadGoalStage,
  ThreadGoalType,
  createAnchor,
  createClaim,
  createEntry,
  createThreadRecord
} from "../../src/domain/models/threadnoteModels.js";
import { CaptureInterpreter, mergeMentions } from "../../src/domain/capture/captureInterpreter.js";
import { ThreadRoutingEngine } from "../../src/domain/routing/threadRoutingEngine.js";
import { extractLocatorCandidate, resolveEntrySourceDescriptor } from "../../src/domain/resources/richSourceDescriptor.js";

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

function makeEntry({ threadID = null, kind = "note", text, createdAt = new Date() }) {
  return createEntry({
    threadID,
    kind,
    summaryText: text,
    createdAt
  });
}

function makeClaim({ threadID, statement, status = ClaimStatus.STABLE, createdAt = new Date() }) {
  return createClaim({
    threadID,
    statement,
    status,
    createdAt,
    updatedAt: createdAt
  });
}

test("clean-room models: createThreadRecord builds legacy goal layer defaults", () => {
  const thread = createThreadRecord({ title: "Atlas launch", prompt: "" });
  assert.equal(thread.goalLayer.goalStatement, "Atlas launch");
  assert.equal(thread.goalLayer.currentStage, ThreadGoalStage.FRAMING);
  assert.equal(thread.status, "active");
});

test("clean-room capture: explicit tag overrides type and strips tag", () => {
  const interpreter = new CaptureInterpreter();
  const result = interpreter.interpretText("#claim Atlas launch depends on legal review");
  assert.equal(result.detectedItemType, "claim");
  assert.equal(result.normalizedText, "Atlas launch depends on legal review");
  assert.equal(result.candidateClaims.length, 1);
});

test("clean-room capture: object mentions and routing queries are extracted", () => {
  const interpreter = new CaptureInterpreter();
  const result = interpreter.interpretText("Need to align with @OpenAI and @Atlas launch owners");
  assert.deepEqual(
    result.detectedObjects.map((item) => item.name),
    ["Atlas", "OpenAI"]
  );
  assert.equal(result.routingSignals.queries.includes("OpenAI"), true);
});

test("clean-room capture: mergeMentions keeps longer containing mention", () => {
  const merged = mergeMentions([
    { id: "1", name: "J.K.罗琳", kind: "person" },
    { id: "2", name: "罗琳", kind: "person" }
  ]);
  assert.deepEqual(
    merged.map((item) => item.name),
    ["J.K.罗琳"]
  );
});

test("clean-room routing: decide routes strong exact claim overlap", () => {
  const atlas = makeThread({ title: "Atlas release" });
  const hiring = makeThread({ title: "Hiring plan", lastActiveAt: new Date(Date.now() - 60_000) });

  const threadEntries = new Map([
    [atlas.id, [makeEntry({ threadID: atlas.id, text: "Atlas release blockers" })]],
    [hiring.id, [makeEntry({ threadID: hiring.id, text: "Hiring debrief notes" })]]
  ]);
  const threadClaims = new Map([
    [atlas.id, [makeClaim({ threadID: atlas.id, statement: "Atlas release playbook" })]],
    [hiring.id, [makeClaim({ threadID: hiring.id, statement: "Hiring interview rubric" })]]
  ]);
  const threadAnchors = new Map([
    [
      atlas.id,
      createAnchor({
        threadID: atlas.id,
        coreQuestion: atlas.goalLayer.goalStatement,
        stateSummary: "Atlas release blockers and next steps"
      })
    ]
  ]);

  const engine = new ThreadRoutingEngine({
    threadsProvider: () => [atlas, hiring],
    entriesProvider: (threadID) => threadEntries.get(threadID) ?? [],
    claimsProvider: (threadID) => threadClaims.get(threadID) ?? [],
    latestAnchorProvider: (threadID) => threadAnchors.get(threadID) ?? null
  });

  const decision = engine.decide("Atlas release playbook");
  assert.equal(decision.type, "route");
  assert.equal(decision.threadID, atlas.id);
});

test("clean-room routing: ambiguous result stays in inbox", () => {
  const alpha = createThreadRecord({
    title: "Atlas alpha",
    prompt: "Atlas weekly plan",
    goalLayer: {
      goalStatement: "Atlas weekly plan",
      goalType: ThreadGoalType.RESEARCH,
      successCondition: "Done",
      currentStage: ThreadGoalStage.FRAMING
    }
  });
  const beta = createThreadRecord({
    title: "Atlas beta",
    prompt: "Atlas weekly plan",
    goalLayer: {
      goalStatement: "Atlas weekly plan",
      goalType: ThreadGoalType.RESEARCH,
      successCondition: "Done",
      currentStage: ThreadGoalStage.FRAMING
    },
    lastActiveAt: new Date(Date.now() - 60_000)
  });

  const claims = new Map([
    [alpha.id, [makeClaim({ threadID: alpha.id, statement: "Atlas weekly plan" })]],
    [beta.id, [makeClaim({ threadID: beta.id, statement: "Atlas weekly plan" })]]
  ]);

  const engine = new ThreadRoutingEngine({
    threadsProvider: () => [alpha, beta],
    entriesProvider: () => [],
    claimsProvider: (threadID) => claims.get(threadID) ?? [],
    latestAnchorProvider: () => null
  });

  const decision = engine.decide("Atlas weekly plan");
  assert.equal(decision.type, "noMatch");
  assert.equal(Boolean(decision.reason), true);
});

test("clean-room rich source descriptor extracts locator from plain text attachment and classifies media kind", () => {
  assert.equal(extractLocatorCandidate("Review attachments/atlas-demo.mp4"), "attachments/atlas-demo.mp4");

  const descriptor = resolveEntrySourceDescriptor(
    createEntry({
      summaryText: "attachments/atlas-demo.mp4"
    })
  );

  assert.equal(descriptor.locator, "attachments/atlas-demo.mp4");
  assert.equal(descriptor.sourceKind, "video");
  assert.equal(descriptor.isLocal, true);
});
