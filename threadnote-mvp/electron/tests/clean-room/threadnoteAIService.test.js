import test from "node:test";
import assert from "node:assert/strict";
import { ThreadnoteAIService } from "../../src/infrastructure/ai/runtime/services/threadnoteAIService.js";
import { AIRequestQueue } from "../../src/infrastructure/ai/queue/aiRequestQueue.js";

function makeRuntimeWithJSON(payload, overrides = {}) {
  return {
    backendLabel: overrides.backendLabel ?? "OpenAI-Compatible · mock",
    config: { model: "mock-model" },
    async createTextClient() {
      return {
        async generateText() {
          return {
            text: typeof payload === "string" ? payload : JSON.stringify(payload),
            finishReason: "stop",
            warnings: [],
            response: {
              id: "resp-1",
              modelId: "mock-model",
              body: payload
            }
          };
        }
      };
    }
  };
}

test("clean-room ai service plans route through queue and normalizes response", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      decision: "route",
      selectedThreadID: "thread-1",
      decisionReason: "Strong overlap",
      suggestions: [
        { threadID: "thread-1", reason: "Best match" },
        { threadID: "thread-x", reason: "Drop me" }
      ]
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.planRoute({
    entryID: "entry-1",
    normalizedText: "Atlas launch legal review",
    detectedItemType: "claim",
    candidates: [{ threadID: "thread-1", threadTitle: "Atlas launch", totalScore: 30, coreObjects: [] }]
  });

  assert.equal(result.shouldRoute, true);
  assert.equal(result.selectedThreadID, "thread-1");
  assert.equal(result.suggestions.length, 1);
  assert.equal(result.debugPayload.responseID, "resp-1");
});

test("clean-room ai service synthesizes resume and filters presentation blocks via contracts", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      currentJudgment: "Atlas is blocked by approvals",
      openLoops: ["Confirm legal review"],
      nextAction: "Get sign-off",
      restartNote: "One blocker dominates the thread.",
      recommendedNextSteps: ["Get sign-off"],
      presentation: {
        headline: "Clear the legal bottleneck.",
        blocks: [
          { kind: "unsupported", title: "Bad", items: ["x"] },
          { kind: "gap", title: "Main Gap", items: ["Confirm owner"], tone: "warning" }
        ]
      }
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.synthesizeResume({
    threadID: "thread-1",
    coreQuestion: "How do we ship Atlas?",
    goalLayer: { goalType: "research", currentStage: "framing" },
    currentJudgment: "Atlas is blocked by approvals",
    judgmentBasis: "Legal sign-off missing",
    openLoops: ["Confirm legal review"],
    activeClaims: ["Atlas is blocked by approvals"],
    recentNotes: []
  });

  assert.equal(result.presentationPlan.blocks.length, 1);
  assert.equal(result.presentationPlan.blocks[0].kind, "gap");
});

test("clean-room ai service analyzes discourse and infers only declared pairs", async () => {
  const analysisService = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      relationPairs: [
        { sourceEntryID: "e1", targetEntryID: "e2", kind: "supports" },
        { sourceEntryID: "e2", targetEntryID: "e1", kind: "bogus" }
      ],
      rationale: "Clear support"
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const analysis = await analysisService.analyzeDiscourse({
    threadID: "t1",
    snippets: [
      { id: "e1", text: "Claim", kind: "claim" },
      { id: "e2", text: "Evidence", kind: "evidence" }
    ]
  });
  assert.equal(analysis.relationPairs[1].kind, "informs");

  const inferenceService = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      relations: [
        { sourceEntryID: "e1", targetEntryID: "e2", kind: "answers" },
        { sourceEntryID: "x", targetEntryID: "y", kind: "supports" }
      ]
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });
  const inference = await inferenceService.inferDiscourseRelations({
    threadID: "t1",
    pairs: [
      {
        pairIndex: 1,
        sourceID: "e1",
        targetID: "e2",
        sourceKind: "claim",
        targetKind: "question",
        sourceSnippet: "Claim",
        targetSnippet: "Question"
      }
    ]
  });
  assert.deepEqual(inference.relations, [
    { sourceEntryID: "e1", targetEntryID: "e2", kind: "answers" }
  ]);
});

test("clean-room ai service prepares draft and surfaces invalid JSON", async () => {
  const okService = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      title: "Atlas unblock draft",
      openLoops: ["Confirm legal review"],
      recommendedNextSteps: ["Get sign-off"]
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });
  const draft = await okService.prepareDraft({
    threadID: "thread-1",
    type: "writing",
    coreQuestion: "How do we unblock Atlas?"
  });
  assert.equal(draft.title, "Atlas unblock draft");

  const badService = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON("not json"),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });
  await assert.rejects(
    () => badService.prepareDraft({ threadID: "thread-1", type: "writing", coreQuestion: "x" }),
    /valid JSON/
  );
});

test("clean-room ai service forwards AbortSignal to the provider client", async () => {
  let receivedSignal = null;
  const service = new ThreadnoteAIService({
    providerRuntime: {
      backendLabel: "OpenAI-Compatible · mock",
      config: { model: "mock-model" },
      async createTextClient() {
        return {
          async generateText(input) {
            receivedSignal = input.signal;
            return {
              text: JSON.stringify({
                decision: "inbox",
                selectedThreadID: null,
                decisionReason: "keep in inbox",
                suggestions: []
              }),
              finishReason: "stop",
              warnings: [],
              response: {
                id: "resp-signal",
                modelId: "mock-model",
                body: {}
              }
            };
          }
        };
      }
    },
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });
  const controller = new AbortController();

  await service.planRoute({
    entryID: "entry-signal",
    normalizedText: "Atlas note",
    detectedItemType: "note",
    candidates: [{ threadID: "thread-1", threadTitle: "Atlas", totalScore: 10, coreObjects: [] }]
  }, { signal: controller.signal });

  assert.equal(receivedSignal, controller.signal);
});
