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
import { tokenizeForSearch } from "../../src/domain/capture/tokenizeForSearch.js";
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
  assert.equal(result.detectedItemSource, "explicitTag");
  assert.equal(result.normalizedText, "Atlas launch depends on legal review");
  assert.equal(result.candidateClaims.length, 1);
});

test("clean-room capture: heuristic fast classification detects question claim and plan", () => {
  const interpreter = new CaptureInterpreter();

  const question = interpreter.interpretText("什么是新时代 AI 人机交互？");
  const claim = interpreter.interpretText("AI 时代的人机交互，最大的挑战是聊天成为默认界面。");
  const plan = interpreter.interpretText("下一步先梳理问题，再给出交互方案。");

  assert.equal(question.detectedItemType, "question");
  assert.equal(claim.detectedItemType, "claim");
  assert.equal(plan.detectedItemType, "plan");
  assert.equal(question.detectedItemSource, "heuristic");
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

test("clean-room tokenizer: segments CJK and mixed-language search terms consistently", () => {
  assert.deepEqual(tokenizeForSearch("思维盒子"), ["思维", "盒子"]);
  assert.deepEqual(tokenizeForSearch("OpenAI Atlas 定价"), ["openai", "atlas", "定价"]);
  assert.deepEqual(tokenizeForSearch("如何设计出一个思维盒子？"), ["如何", "设计", "出", "一个", "思维", "盒子"]);
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

test("clean-room routing: support snapshot exposes replay diagnostics for chinese recall cases", () => {
  const thread = makeThread({ title: "思维盒子" });
  const entries = new Map([
    [thread.id, [makeEntry({ threadID: thread.id, text: "一个思维盒子，应该可以容纳人关于某一方面的所有想法" })]]
  ]);
  const claims = new Map([
    [thread.id, [makeClaim({ threadID: thread.id, statement: "思维盒子应该容纳某一方面的所有想法" })]]
  ]);

  const engine = new ThreadRoutingEngine({
    threadsProvider: () => [thread],
    entriesProvider: (threadID) => entries.get(threadID) ?? [],
    claimsProvider: (threadID) => claims.get(threadID) ?? [],
    latestAnchorProvider: () => null,
    retrievalEngine: {
      recall: () => [],
      rankThreads: () => [{ thread, score: 0, reason: "No matching content" }]
    }
  });

  const debug = engine.debugState("如何设计出一个思维盒子？");
  assert.equal(debug.tokenizedQueryTerms.includes("思维"), true);
  assert.equal(debug.tokenizedQueryTerms.includes("盒子"), true);
  assert.equal(Array.isArray(debug.retrievalDiagnostics), true);
  assert.equal(debug.retrievalDiagnostics.length >= 1, true);
  assert.equal(Array.isArray(debug.retrievalDiagnostics[0].tokenizedTerms), true);
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
