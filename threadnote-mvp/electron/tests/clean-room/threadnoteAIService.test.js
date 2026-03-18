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

test("clean-room ai service classifies entry kind and preserves debug payload", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      kind: "question",
      reason: "The note is phrased as an open problem.",
      confidence: 0.88
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.classifyEntryKind({
    entryID: "entry-classify",
    normalizedText: "How should we design the Atlas box?",
    detectedItemType: "note",
    candidateClaims: []
  });

  assert.equal(result.kind, "question");
  assert.equal(result.confidence, 0.88);
  assert.equal(result.debugPayload.responseID, "resp-1");
});

test("clean-room ai service includes entry kind semantics in the classification prompt", async () => {
  let capturedPrompt = "";
  const service = new ThreadnoteAIService({
    providerRuntime: {
      backendLabel: "OpenAI-Compatible · mock",
      config: { model: "mock-model" },
      async createTextClient() {
        return {
          async generateText(input) {
            capturedPrompt = input.userPrompt;
            return {
              text: JSON.stringify({
                kind: "question",
                reason: "Prompt captured",
                confidence: 0.9
              }),
              finishReason: "stop",
              warnings: [],
              response: {
                id: "resp-prompt",
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

  await service.classifyEntryKind({
    entryID: "entry-classify",
    normalizedText: "什么是新时代 AI 人机交互？",
    detectedItemType: "note",
    candidateClaims: []
  });

  assert.match(capturedPrompt, /question = an explicit open problem/i);
  assert.match(capturedPrompt, /claim = an assertion, judgment, thesis/i);
  assert.match(capturedPrompt, /anchorWritten = a rare internal state/i);
});

test("clean-room ai service falls back to note for unsupported classification kinds", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      kind: "bogus-kind",
      reason: "Bad kind",
      confidence: 0.91
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.classifyEntryKind({
    entryID: "entry-classify",
    normalizedText: "Atlas note",
    detectedItemType: "note"
  });

  assert.equal(result.kind, "note");
});

test("clean-room ai service classifies entry status with thread context", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      status: "solved",
      reason: "The thread already contains the final answer.",
      confidence: 0.86
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.classifyEntryStatus({
    entryID: "entry-status",
    threadID: "thread-1",
    normalizedText: "这个问题已经解决了，方案已经落地。",
    currentKind: "question",
    currentStatus: "open",
    threadTitle: "AI 时代的人机交互",
    threadGoal: "Clarify the next interaction model",
    recentThreadEntries: [
      { id: "entry-older", text: "我们已经确定使用工作区视图。", kind: "claim", status: "decided" }
    ]
  });

  assert.equal(result.status, "solved");
  assert.equal(result.confidence, 0.86);
  assert.equal(result.debugPayload.responseID, "resp-1");
});

test("clean-room ai service includes status semantics and thread context in the prompt", async () => {
  let capturedPrompt = "";
  const service = new ThreadnoteAIService({
    providerRuntime: {
      backendLabel: "OpenAI-Compatible · mock",
      config: { model: "mock-model" },
      async createTextClient() {
        return {
          async generateText(input) {
            capturedPrompt = input.userPrompt;
            return {
              text: JSON.stringify({
                status: "verified",
                reason: "Prompt captured",
                confidence: 0.9
              }),
              finishReason: "stop",
              warnings: [],
              response: {
                id: "resp-status-prompt",
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

  await service.classifyEntryStatus({
    entryID: "entry-status",
    threadID: "thread-1",
    normalizedText: "这个结论已经验证通过。",
    currentKind: "claim",
    currentStatus: "open",
    threadTitle: "Atlas",
    threadGoal: "Ship the launch",
    recentThreadEntries: [
      { id: "entry-older", text: "之前已经完成实现。", kind: "claim", status: "decided" }
    ]
  });

  assert.match(capturedPrompt, /decided = a choice or direction has been settled for now/i);
  assert.match(capturedPrompt, /solved = a question or problem is resolved/i);
  assert.match(capturedPrompt, /thread title/i);
  assert.match(capturedPrompt, /recent thread context/i);
});

test("clean-room ai service falls back to open for unsupported status classification", async () => {
  const service = new ThreadnoteAIService({
    providerRuntime: makeRuntimeWithJSON({
      status: "bogus-status",
      reason: "Bad status",
      confidence: 0.92
    }),
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
  });

  const result = await service.classifyEntryStatus({
    entryID: "entry-status",
    threadID: "thread-1",
    normalizedText: "Atlas note",
    currentKind: "note",
    currentStatus: "open"
  });

  assert.equal(result.status, "open");
});
