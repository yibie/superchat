import test from "node:test";
import assert from "node:assert/strict";
import {
  PresentationTone,
  createAIDebugPayload,
  createDiscourseAnalysisRequest,
  createDiscourseAnalysisResult,
  createDiscourseInferenceRequest,
  createDiscourseInferenceResult,
  createDraftPreparationRequest,
  createDraftPreparationResult,
  createResumeSynthesisResult,
  createRoutePlanningRequest,
  createRoutePlanningResult,
  normalizeRelationKind
} from "../../src/domain/ai/aiContracts.js";

test("clean-room ai contracts normalize route planning result against request candidates", () => {
  const request = createRoutePlanningRequest({
    entryID: "entry-1",
    normalizedText: "Atlas launch needs legal review",
    detectedItemType: "claim",
    detectedObjects: ["Atlas"],
    candidateClaims: ["Atlas launch needs legal review"],
    routingQueries: ["Atlas", "legal review"],
    candidates: [
      { threadID: "thread-atlas", threadTitle: "Atlas launch", totalScore: 31 },
      { threadID: "thread-hiring", threadTitle: "Hiring plan", totalScore: 8 }
    ]
  });

  const result = createRoutePlanningResult(
    {
      shouldRoute: true,
      selectedThreadID: "thread-unknown",
      decisionReason: "",
      suggestions: [
        { threadID: "thread-atlas", reason: "Claim and thread overlap" },
        { threadID: "thread-missing", reason: "Should be dropped" }
      ]
    },
    request
  );

  assert.equal(result.shouldRoute, false);
  assert.equal(result.selectedThreadID, null);
  assert.equal(result.decisionReason, "No route reason provided.");
  assert.deepEqual(result.suggestions, [
    { threadID: "thread-atlas", reason: "Claim and thread overlap" }
  ]);
});

test("clean-room ai contracts normalize resume result presentation plan", () => {
  const result = createResumeSynthesisResult({
    currentJudgment: "Atlas is blocked by approvals",
    openLoops: ["Confirm legal review", "Confirm legal review", "Assign owner"],
    nextAction: "Get final legal sign-off",
    restartNote: "You have one dominant blocker.",
    recommendedNextSteps: ["Get legal sign-off"],
    presentationPlan: {
      headline: "Clear the legal bottleneck.",
      primaryAction: "Get final legal sign-off",
      blocks: [
        {
          kind: "unsupported-kind",
          title: "Ignore",
          summary: "bad",
          items: ["bad"],
          tone: "chaotic"
        },
        {
          kind: "gap",
          title: "Main Gap",
          summary: "Legal review remains open.",
          items: ["Confirm ownership"],
          tone: "chaotic"
        }
      ]
    }
  });

  assert.equal(result.presentationPlan.blocks.length, 1);
  assert.equal(result.presentationPlan.blocks[0].kind, "gap");
  assert.equal(result.presentationPlan.blocks[0].tone, PresentationTone.NEUTRAL);
});

test("clean-room ai contracts normalize discourse requests and relation kinds", () => {
  const request = createDiscourseAnalysisRequest({
    threadID: "thread-1",
    snippets: [
      { id: "e1", text: "Atlas claim", kind: "claim" },
      { id: "e2", text: "Approval evidence", kind: "evidence" },
      { id: null, text: "broken", kind: "note" }
    ]
  });
  const result = createDiscourseAnalysisResult({
    relationPairs: [
      { sourceEntryID: "e1", targetEntryID: "e2", kind: "supports" },
      { sourceEntryID: "e2", targetEntryID: "e1", kind: "nonsense" }
    ],
    rationale: "Explicit support pair."
  });

  assert.equal(request.snippets.length, 2);
  assert.equal(result.relationPairs[1].kind, "informs");
  assert.equal(normalizeRelationKind("answers"), "answers");
});

test("clean-room ai contracts filter discourse inference results to declared pairs", () => {
  const request = createDiscourseInferenceRequest({
    threadID: "thread-1",
    pairs: [
      {
        pairIndex: 1,
        sourceID: "e1",
        targetID: "e2",
        sourceKind: "claim",
        targetKind: "question",
        sourceSnippet: "Atlas needs approval",
        targetSnippet: "What blocks Atlas?"
      }
    ]
  });
  const result = createDiscourseInferenceResult(
    {
      relations: [
        { sourceEntryID: "e1", targetEntryID: "e2", kind: "answers" },
        { sourceEntryID: "e9", targetEntryID: "e8", kind: "supports" }
      ]
    },
    request
  );

  assert.deepEqual(result.relations, [
    { sourceEntryID: "e1", targetEntryID: "e2", kind: "answers" }
  ]);
});

test("clean-room ai contracts normalize draft request/result and debug payload", () => {
  const request = createDraftPreparationRequest({
    threadID: "thread-1",
    type: "meeting",
    coreQuestion: "How do we unblock Atlas?",
    activeClaims: ["Atlas is blocked by approvals"],
    openLoops: ["Confirm legal review"],
    keyEvidence: [{ id: "e1", text: "Legal sign-off is missing" }],
    recentNotes: [{ id: "e2", text: "Need owner and deadline" }]
  });
  const debugPayload = createAIDebugPayload({
    backendLabel: "OpenAI · mock",
    configuredModelID: "gpt-mock",
    parsedResponse: { title: "Atlas unblock plan" }
  });
  const result = createDraftPreparationResult({
    title: "Atlas unblock plan",
    openLoops: ["Confirm legal review"],
    recommendedNextSteps: ["Get legal sign-off"],
    debugPayload
  });

  assert.equal(request.keyEvidence.length, 1);
  assert.equal(request.recentNotes.length, 1);
  assert.equal(result.title, "Atlas unblock plan");
  assert.equal(typeof result.debugPayload.parsedResponse, "string");
});
