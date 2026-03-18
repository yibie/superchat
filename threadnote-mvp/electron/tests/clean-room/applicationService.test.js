import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { ThreadnoteApplicationService } from "../../src/application/services/threadnoteApplicationService.js";
import { WorkspaceManager } from "../../src/infrastructure/persistence/workspace/workspaceManager.js";
import { AIProviderConfigStore } from "../../src/infrastructure/ai/runtime/aiProviderConfigStore.js";
import { AIProviderRuntime } from "../../src/infrastructure/ai/runtime/aiProviderRuntime.js";
import { AIRequestQueue } from "../../src/infrastructure/ai/queue/aiRequestQueue.js";
import { ThreadnoteAIService } from "../../src/infrastructure/ai/runtime/services/threadnoteAIService.js";
import { LinkMetadataService } from "../../src/infrastructure/metadata/linkMetadataService.js";
import {
  createAnchor,
  createClaim,
  ClaimStatus,
  createDiscourseRelation,
  EntryKind,
  createEntry,
  createThreadAISnapshot
} from "../../src/domain/models/threadnoteModels.js";

function makeTempDir() {
  return fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-app-service-"));
}

function deferred() {
  let resolve;
  let reject;
  const promise = new Promise((res, rej) => {
    resolve = res;
    reject = rej;
  });
  return { promise, resolve, reject };
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function makeAppService({
  runtimePayload = null,
  aiProviderRuntimeOverride = null,
  aiServiceOverride = null,
  onAsyncStateChanged = null
} = {}) {
  const root = makeTempDir();
  const workspaceManager = new WorkspaceManager({
    stateFilePath: path.join(root, "workspace-state.json")
  });
  let aiProviderRuntime = aiProviderRuntimeOverride;
  let aiService = aiServiceOverride;

  if (!aiProviderRuntime && runtimePayload) {
    aiProviderRuntime = new AIProviderRuntime({
      configStore: new AIProviderConfigStore({ configPath: path.join(root, "provider.json") }),
      clientFactory: async () => ({
        async generateText() {
          return {
            text: JSON.stringify(runtimePayload),
            finishReason: "stop",
            warnings: [],
            response: { id: "resp-1", modelId: "mock-model", body: runtimePayload }
          };
        }
      })
    });
    aiProviderRuntime.configure({
      providerKind: "ollama",
      baseURL: "http://localhost:11434/v1",
      model: "qwen3.5:4b"
    });
  }

  if (!aiService && aiProviderRuntime) {
    aiService = new ThreadnoteAIService({
      providerRuntime: aiProviderRuntime,
      requestQueue: new AIRequestQueue({ maxConcurrent: 1 })
    });
  }

  return {
    root,
    service: new ThreadnoteApplicationService({
      workspaceManager,
      aiProviderRuntime,
      aiService,
      onAsyncStateChanged,
      linkMetadataService: new LinkMetadataService({
        fetchImpl: async () => ({
          ok: true,
          headers: {
            get(name) {
              return name === "content-type" ? "text/html" : null;
            }
          },
          async text() {
            return "<html><head><title>Atlas Spec</title><meta name=\"description\" content=\"Launch document\"></head></html>";
          }
        }),
        workspacePathResolver: () => workspaceManager.describe()?.workspacePath ?? null
      })
    })
  };
}

test("clean-room application service creates workspace and exposes home view", () => {
  const { root, service } = makeAppService();
  const loaded = service.createWorkspace(path.join(root, "Atlas"));

  assert.equal(Boolean(loaded.workspace.workspacePath), true);
  assert.equal(Array.isArray(loaded.home.threads), true);
  assert.equal(loaded.home.inboxEntries.length, 0);
});

test("clean-room application service creates thread and routes matching capture into it", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "#claim Atlas launch needs legal review"
  });

  assert.equal(result.routingDecision.type, "route");
  assert.equal(result.routingDecision.threadID, thread.id);

  const threadView = service.openThread(thread.id);
  assert.equal(threadView.entries.length, 1);
  assert.equal(threadView.entries[0].kind, EntryKind.CLAIM);
});

test("clean-room application service returns refreshed thread detail after manual route and preserves it through openThreadWithAI", async () => {
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      return { shouldRoute: false, selectedThreadID: null, decisionReason: "inbox", suggestions: [] };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "嫌累", prompt: "嫌累" });
  const capture = await service.submitCapture({ text: "今天真的嫌累，先记下来" });

  assert.equal(capture.entry.threadID, null);

  const routedThread = await service.routeEntryToThread({ entryID: capture.entry.id, threadID: thread.id });
  assert.equal(routedThread.thread.id, thread.id);
  assert.equal(routedThread.entries.some((entry) => entry.id === capture.entry.id), true);

  const reopenedThread = await service.openThreadWithAI(thread.id);
  assert.equal(reopenedThread.thread.id, thread.id);
  assert.equal(reopenedThread.entries.some((entry) => entry.id === capture.entry.id), true);
});

test("clean-room application service reconnects retrieval-backed routing for existing workspace data", async () => {
  const { root, service } = makeAppService();
  const workspacePath = path.join(root, "Atlas");
  service.createWorkspace(workspacePath);
  const thread = await service.createThread({ title: "Backlog", prompt: "Backlog" });

  await service.repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.NOTE,
      summaryText: "Vendor invoice mismatch blocks payout approval",
      createdAt: "2026-03-15T09:01:00.000Z"
    })
  );
  await service.repository.flush();

  service.openWorkspace(workspacePath);
  const result = await service.submitCapture({
    text: "Need to resolve vendor invoice mismatch before payout approval"
  });

  assert.equal(result.routingDecision.type, "route");
  assert.equal(result.routingDecision.threadID, thread.id);
});

test("clean-room application service keeps ambiguous capture in inbox and exposes its resources globally", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  await service.submitCapture({
    text: "Check https://example.com and talk to @Atlas"
  });

  const home = service.homeView();
  assert.equal(home.inboxEntries.length, 1);
  assert.equal(home.resources.length, 2);
  assert.equal(home.resources.some((resource) => resource.kind === "link" && resource.threadID == null), true);
  assert.equal(home.resources.some((resource) => resource.kind === "mention" && resource.threadID == null), true);
});

test("clean-room application service stores external capture metadata while reusing inbox submit path", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const result = await service.submitExternalCapture({
    text: "https://example.com/spec",
    source: "clipboardImport",
    sourceContext: {
      clipboardTypes: ["text/plain", "text/html"]
    }
  });

  assert.equal(result.routingDecision.type, "noMatch");
  assert.equal(result.entry.threadID, null);
  assert.equal(result.entry.sourceMetadata.externalCapture.source, "clipboardImport");
  assert.deepEqual(result.entry.sourceMetadata.externalCapture.sourceContext.clipboardTypes, ["text/plain", "text/html"]);
});

test("clean-room application service submitCapture returns immediately while ai routing is deferred to background finalize", async () => {
  const planningStarted = deferred();
  const releasePlanning = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      planningStarted.resolve();
      await releasePlanning.promise;
      return {
        shouldRoute: true,
        selectedThreadID: thread.id,
        decisionReason: "route to Atlas launch",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "#claim Atlas launch needs legal review"
  });

  assert.equal(result.entry.threadID, null);
  assert.equal(result.routingDecision.reason, "Pending AI route decision.");
  assert.equal(Boolean(result.backgroundTask?.entryID), true);
  assert.equal(service.homeView().inboxEntries.length, 1);

  const finalizePromise = service.finalizeCaptureAsync(result.backgroundTask);
  await planningStarted.promise;
  releasePlanning.resolve();
  const finalized = await finalizePromise;

  assert.equal(finalized.routingDecision.threadID, thread.id);
  assert.equal(service.openThread(thread.id).entries.some((entry) => entry.id === result.entry.id), true);
});

test("clean-room application service exposes route-planning ai activity on inbox entries while routing is in flight", async () => {
  const planningStarted = deferred();
  const releasePlanning = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      planningStarted.resolve();
      await releasePlanning.promise;
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "#claim Atlas launch needs legal review"
  });
  const finalizePromise = service.finalizeCaptureAsync(result.backgroundTask);

  await planningStarted.promise;
  const pendingEntry = service.homeView().inboxEntries.at(0);
  assert.equal(pendingEntry.aiActivity.kind, "routePlanning");
  assert.equal(pendingEntry.aiActivity.label, "AI 正在判断归档位置");
  const queue = service.homeView().aiState.queue;
  assert.equal(typeof queue.activeCount, "number");
  assert.equal(Array.isArray(queue.activeLabels), true);
  assert.equal(Array.isArray(service.homeView().aiState.activeOperations), true);
  assert.equal(Boolean(service.homeView().aiState.routeDebugByEntryID[result.entry.id]?.startedAt), true);

  releasePlanning.resolve();
  await finalizePromise;

  const settledEntry = service.homeView().inboxEntries.at(0);
  assert.equal(settledEntry.aiActivity, undefined);
});

test("clean-room application service exposes thread-refresh ai activity on thread entries while synthesis is in flight", async () => {
  const synthesisStarted = deferred();
  const releaseSynthesis = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      synthesisStarted.resolve();
      await releaseSynthesis.promise;
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const result = await service.submitCapture({
    text: "Atlas launch needs legal review",
    threadID: thread.id
  });
  const synthPromise = service.finalizeCaptureAsync(result.backgroundTask);
  await synthesisStarted.promise;
  await sleep(0);

  const pendingEntry = service.openThread(thread.id).entries.at(0);
  assert.equal(pendingEntry.aiActivity.kind, "threadRefreshing");
  assert.equal(pendingEntry.aiActivity.label, "AI 正在整理线程");

  releaseSynthesis.resolve();
  await synthPromise;

  const settledEntry = service.openThread(thread.id).entries.at(0);
  assert.equal(settledEntry.aiActivity, undefined);
});

test("clean-room application service route planning only sends top three candidates to ai and ignores stale result", async () => {
  const first = deferred();
  const second = deferred();
  const requests = [];
  let callCount = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 2
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 2 }),
    async planRoute(request) {
      requests.push(request);
      callCount += 1;
      return callCount === 1 ? first.promise : second.promise;
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  await service.createThread({ title: "Atlas pricing", prompt: "Atlas pricing" });
  await service.createThread({ title: "Atlas legal", prompt: "Atlas legal" });
  const extra = await service.createThread({ title: "Hiring plan", prompt: "Hiring plan" });

  const capture = createEntry({
    threadID: null,
    kind: EntryKind.CLAIM,
    summaryText: "Atlas launch pricing legal blocker"
  });
  await service.repository.saveEntry(capture);
  await service.repository.flush();
  service.loadWorkspace();

  const routeA = service.planRouteForEntry({ entryID: capture.id, autoRoute: true });
  const routeB = service.planRouteForEntry({ entryID: capture.id, autoRoute: true });
  second.resolve({
    shouldRoute: true,
    selectedThreadID: "not-a-candidate",
    decisionReason: "Invalid candidate should be ignored.",
    suggestions: []
  });
  first.resolve({
    shouldRoute: true,
    selectedThreadID: "stale-thread",
    decisionReason: "Stale result.",
    suggestions: []
  });

  const [, latest] = await Promise.all([routeA, routeB]);

  assert.equal(requests[0].candidates.length, 3);
  assert.equal(requests[1].candidates.length, 3);
  assert.equal(latest.type, "noMatch");
  assert.equal(service.snapshot.entries.find((entry) => entry.id === capture.id)?.threadID ?? null, null);
});

test("clean-room application service cancels superseded route work and clears processing state", async () => {
  const firstStarted = deferred();
  const secondStarted = deferred();
  let callCount = 0;
  let firstSignal = null;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute(_request, { signal } = {}) {
      callCount += 1;
      if (callCount === 1) {
        firstSignal = signal;
        firstStarted.resolve();
        await new Promise((resolve, reject) => {
          signal.addEventListener("abort", () => reject(signal.reason ?? new Error("aborted")), { once: true });
        });
      }
      secondStarted.resolve();
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const capture = createEntry({
    threadID: null,
    kind: EntryKind.CLAIM,
    summaryText: "Atlas launch pricing legal blocker"
  });
  await service.repository.saveEntry(capture);
  await service.repository.flush();
  service.loadWorkspace();

  const first = service.planRouteForEntry({ entryID: capture.id, autoRoute: true }).catch((error) => error);
  await firstStarted.promise;
  const second = service.planRouteForEntry({ entryID: capture.id, autoRoute: true });
  await secondStarted.promise;

  const firstResult = await first;
  const secondResult = await second;

  assert.equal(firstSignal.aborted, true);
  assert.match(firstResult.message, /superseded route|aborted/i);
  assert.equal(secondResult.type, "noMatch");
  assert.equal(service.homeView().inboxEntries.at(0).aiActivity, undefined);
});

test("clean-room application service clears processing state when ai route fails", async () => {
  const planningStarted = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      planningStarted.resolve();
      throw new Error("context length exceeded");
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "#claim Atlas launch needs legal review"
  });
  const finalizePromise = service.finalizeCaptureAsync(result.backgroundTask);
  await planningStarted.promise;
  await assert.rejects(() => finalizePromise, /context length exceeded/);

  const settledEntry = service.homeView().inboxEntries.at(0);
  assert.equal(settledEntry.aiActivity, undefined);
});

test("clean-room application service exposes thread detail and memory/resources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  await service.repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.SOURCE,
      body: { url: "https://example.com/spec", title: "Spec" },
      summaryText: "Atlas specification"
    })
  );
  await service.repository.saveClaim(
    createClaim({
      threadID: thread.id,
      statement: "Atlas launch needs legal review",
      status: ClaimStatus.STABLE
    })
  );
  await service.repository.saveAnchor(
    createAnchor({
      threadID: thread.id,
      coreQuestion: "What blocks Atlas launch?",
      stateSummary: "Legal review still open",
      openLoops: ["Confirm owner"]
    })
  );
  await service.repository.upsertAISnapshot(
    createThreadAISnapshot({
      threadID: thread.id,
      contentFingerprint: "fingerprint-1",
      headline: "Clear the legal bottleneck",
      currentJudgment: "Legal review still open"
    })
  );
  await service.repository.flush();
  service.loadWorkspace();

  const threadView = service.openThread(thread.id);
  assert.equal(threadView.claims.length, 1);
  assert.equal(threadView.memory.length >= 2, true);
  assert.equal(threadView.resources.length, 1);
  assert.equal(threadView.aiSnapshot.headline, "Clear the legal bottleneck");
});

test("clean-room application service configures and pings ai provider and prepares draft", async () => {
  const { root, service } = makeAppService({
    runtimePayload: {
      title: "Atlas unblock draft",
      openLoops: ["Confirm legal review"],
      recommendedNextSteps: ["Get sign-off"]
    }
  });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  await service.submitCapture({ text: "#claim Atlas launch needs legal review", threadID: thread.id });

  const ping = await service.pingAIProvider();
  assert.equal(ping.ok, true);

  const prepared = await service.prepareThread({ threadID: thread.id, type: "writing" });
  assert.equal(prepared.title, "Atlas unblock draft");
  assert.equal(prepared.contentState.status, "ready");
});

test("clean-room application service updates queue concurrency when provider changes", async () => {
  const { root, service } = makeAppService({
    runtimePayload: {
      title: "draft",
      openLoops: [],
      recommendedNextSteps: []
    }
  });
  service.createWorkspace(path.join(root, "Atlas"));

  assert.equal(service.aiService.requestQueue.maxConcurrent, 1);

  await service.configureAIProvider({
    providerKind: "openAI",
    baseURL: "https://api.openai.com/v1",
    model: "gpt-4.1-mini",
    apiKey: "sk-test"
  });
  assert.equal(service.aiService.requestQueue.maxConcurrent, 2);

  await service.configureAIProvider({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/api",
    model: "qwen3.5:4b"
  });
  assert.equal(service.aiService.requestQueue.maxConcurrent, 1);
});

test("clean-room application service cancels in-flight ai work when provider changes", async () => {
  const planningStarted = deferred();
  let routeSignal = null;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    get preferredMaxConcurrentRequests() {
      return this.config?.providerKind === "openAI" ? 2 : 1;
    },
    configure(config) {
      this.config = config;
      return config;
    }
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute(_request, { signal } = {}) {
      routeSignal = signal;
      planningStarted.resolve();
      await new Promise((resolve, reject) => {
        signal.addEventListener("abort", () => reject(signal.reason ?? new Error("aborted")), { once: true });
      });
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const capture = createEntry({
    threadID: null,
    kind: EntryKind.CLAIM,
    summaryText: "Atlas launch pricing legal blocker"
  });
  await service.repository.saveEntry(capture);
  await service.repository.flush();
  service.loadWorkspace();

  const routePromise = service.planRouteForEntry({ entryID: capture.id, autoRoute: true }).catch((error) => error);
  await planningStarted.promise;

  await service.configureAIProvider({
    providerKind: "openAI",
    baseURL: "https://api.openai.com/v1",
    model: "gpt-4.1-mini",
    apiKey: "sk-test"
  });

  const routeResult = await routePromise;
  assert.equal(routeSignal.aborted, true);
  assert.match(routeResult.message, /provider reconfigured|aborted/i);
  assert.equal(service.homeView().inboxEntries.at(0).aiActivity, undefined);
  assert.equal(service.aiService.requestQueue.maxConcurrent, 2);
});

test("clean-room application service exposes not-configured prepare state without ai runtime", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const prepared = await service.prepareThread({ threadID: thread.id, type: "writing" });

  assert.equal(prepared.contentState.status, "notConfigured");
});

test("clean-room application service exposes and saves ai provider config", async () => {
  const { root, service } = makeAppService({
    runtimePayload: {
      title: "draft",
      openLoops: [],
      recommendedNextSteps: []
    }
  });
  service.createWorkspace(path.join(root, "Atlas"));

  assert.equal(service.getAIProviderConfig().providerKind, "ollama");

  const saved = await service.configureAIProvider({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:4b",
    embeddingModel: "nomic-embed-text"
  });

  assert.equal(saved.embeddingModel, "nomic-embed-text");
  assert.equal(service.getAIProviderConfig().providerKind, "ollama");
  assert.equal(service.getAIProviderConfig().model, "qwen3.5:4b");
});

test("clean-room application service tests ai provider config without persisting it", async () => {
  const { root, service } = makeAppService({
    runtimePayload: {
      title: "draft",
      openLoops: [],
      recommendedNextSteps: []
    }
  });
  service.createWorkspace(path.join(root, "Atlas"));

  await service.configureAIProvider({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "saved-model",
    embeddingModel: "nomic-embed-text"
  });

  const tested = await service.testAIProvider({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "unsaved-model",
    embeddingModel: "nomic-embed-text"
  });

  assert.equal(tested.ok, true);
  assert.equal(tested.modelID, "mock-model");
  assert.equal(tested.backendLabel, "Ollama (Local) · unsaved-model");
  assert.equal(service.getAIProviderConfig().model, "saved-model");
});

test("clean-room application service resume synthesis only applies latest token", async () => {
  const first = deferred();
  const second = deferred();
  let calls = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 2
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 2 }),
    async planRoute() {
      return { shouldRoute: false, selectedThreadID: null, decisionReason: "inbox", suggestions: [] };
    },
    async synthesizeResume() {
      calls += 1;
      return calls === 1 ? first.promise : second.promise;
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  await service.repository.saveEntry(
    createEntry({
      threadID: thread.id,
      kind: EntryKind.CLAIM,
      summaryText: "Atlas launch needs legal review"
    })
  );
  await service.repository.flush();
  service.loadWorkspace();

  const synthA = service.synthesizeThreadState({ threadID: thread.id });
  const synthB = service.synthesizeThreadState({ threadID: thread.id });
  second.resolve({
    currentJudgment: "Latest judgment",
    openLoops: ["Confirm owner"],
    nextAction: "Get sign-off",
    restartNote: "Use the latest result.",
    recoveryLines: [],
    resolvedSoFar: [],
    recommendedNextSteps: ["Get sign-off"],
    presentationPlan: { headline: "Latest headline", blocks: [] }
  });
  first.resolve({
    currentJudgment: "Stale judgment",
    openLoops: [],
    nextAction: null,
    restartNote: "Ignore me.",
    recoveryLines: [],
    resolvedSoFar: [],
    recommendedNextSteps: [],
    presentationPlan: { headline: "Stale headline", blocks: [] }
  });

  await Promise.all([synthA, synthB]);
  const snapshot = service.openThread(thread.id).aiSnapshot;
  assert.equal(snapshot.headline, "Latest headline");
  assert.equal(snapshot.currentJudgment, "Latest judgment");
});

test("clean-room application service archives thread and removes it from home active list", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  let home = service.homeView();
  assert.equal(home.threads.some((item) => item.id === thread.id), true);

  await service.archiveThread(thread.id);

  home = service.homeView();
  assert.equal(home.threads.some((item) => item.id === thread.id), false);
  assert.equal(service.openThread(thread.id)?.thread.status, "archived");
});

test("clean-room application service updates thread title", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Old title", prompt: "Old title" });

  const updated = await service.updateThreadTitle({
    threadID: thread.id,
    title: "New title"
  });

  assert.equal(updated.thread.title, "New title");
  assert.equal(updated.thread.prompt, "New title");
  assert.equal(service.homeView().threads.find((item) => item.id === thread.id)?.title, "New title");
});

test("clean-room application service appends replies edits entries routes inbox items and deletes trees", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const inboxCapture = await service.submitCapture({ text: "Inbox item for @Atlas" });

  const routed = await service.routeEntryToThread({
    entryID: inboxCapture.entry.id,
    threadID: thread.id
  });
  assert.equal(routed.thread.id, thread.id);

  const reply = await service.appendReply({
    entryID: inboxCapture.entry.id,
    text: "Reply note"
  });
  assert.equal(reply.entry.parentEntryID, inboxCapture.entry.id);

  await service.updateEntryText({
    entryID: inboxCapture.entry.id,
    text: "Edited entry for [[supports|Reply note]]",
    references: [{ label: "Reply note", relationKind: "supports", targetID: reply.entry.id }]
  });
  let threadView = service.openThread(thread.id);
  assert.equal(threadView.entries[0].references[0].label, "Reply note");
  assert.equal(threadView.entries[0].references[0].relationKind, "supports");
  assert.equal(threadView.entries[0].references[0].targetID, reply.entry.id);

  await service.deleteEntry(inboxCapture.entry.id);
  threadView = service.openThread(thread.id);
  assert.equal(threadView.entries.length, 0);
});

test("clean-room application service anchors replies-to-replies at the top-level conversation entry", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const parent = await service.submitCapture({ text: "Parent entry" });
  const firstReply = await service.appendReply({
    entryID: parent.entry.id,
    text: "First reply"
  });
  const secondReply = await service.appendReply({
    entryID: firstReply.entry.id,
    text: "Second reply"
  });

  assert.equal(firstReply.entry.parentEntryID, parent.entry.id);
  assert.equal(secondReply.entry.parentEntryID, parent.entry.id);

  const home = service.homeView();
  const renderedReplies = home.allEntries
    .filter((entry) => entry.parentEntryID === parent.entry.id)
    .map((entry) => entry.summaryText);

  assert.deepEqual(renderedReplies, ["First reply", "Second reply"]);
});

test("clean-room application service discourse inference replaces low-confidence thread relations only", async () => {
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 2
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 2 }),
    async planRoute() {
      return { shouldRoute: false, selectedThreadID: null, decisionReason: "inbox", suggestions: [] };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "",
        openLoops: [],
        nextAction: null,
        restartNote: "",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return {
        relations: [
          { sourceEntryID: "entry-source", targetEntryID: "entry-target", kind: "answers" }
        ]
      };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const target = createEntry({
    id: "entry-target",
    threadID: thread.id,
    kind: EntryKind.QUESTION,
    summaryText: "What blocks Atlas launch?"
  });
  const source = createEntry({
    id: "entry-source",
    threadID: thread.id,
    kind: EntryKind.CLAIM,
    summaryText: "Atlas launch is blocked by legal review"
  });
  await service.repository.saveEntry(target);
  await service.repository.saveEntry(source);
  await service.repository.saveDiscourseRelation(
    createDiscourseRelation({
      sourceEntryID: target.id,
      targetEntryID: source.id,
      kind: "supports",
      confidence: 0.95
    })
  );
  await service.repository.saveDiscourseRelation(
    createDiscourseRelation({
      sourceEntryID: source.id,
      targetEntryID: target.id,
      kind: "supports",
      confidence: 0.4
    })
  );
  await service.repository.flush();
  service.loadWorkspace();

  const relations = await service.inferDiscourseRelations({ threadID: thread.id });
  assert.equal(relations.some((relation) => relation.kind === "answers" && relation.confidence === 0.75), true);
  assert.equal(relations.some((relation) => relation.kind === "supports" && relation.confidence === 0.95), true);
  assert.equal(relations.some((relation) => relation.kind === "supports" && relation.confidence === 0.4), false);
});

test("clean-room application service background sweep routes invalidated inbox entries without refreshing resume before threshold", async () => {
  let routeCalls = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute(request) {
      routeCalls += 1;
      return {
        shouldRoute: true,
        selectedThreadID: thread.id,
        decisionReason: "Atlas launch is the clear match.",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const capture = await service.submitCapture({ text: "#claim Atlas launch needs legal review" });
  assert.equal(capture.entry.threadID, null);

  for (let index = 0; index < 20; index += 1) {
    const inboxEntry = service.snapshot.entries.find((entry) => entry.id === capture.entry.id);
    if (inboxEntry?.threadID === thread.id) {
      break;
    }
    await sleep(100);
  }

  const inboxEntry = service.snapshot.entries.find((entry) => entry.id === capture.entry.id);
  assert.equal(inboxEntry?.threadID, thread.id);
  assert.equal(routeCalls, 1);
  await sleep(1400);
  assert.notEqual(service.openThread(thread.id)?.aiStatus?.resume?.status, "ready");
});

test("clean-room application service marks invalid resume plans and clears cached ai state on provider reset", async () => {
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1,
    configure(config) {
      this.config = config;
      return config;
    }
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Still blocked",
        openLoops: ["Confirm owner"],
        nextAction: "Follow up",
        restartNote: "Use deterministic fallback.",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: {
          headline: "Broken plan",
          blocks: [{ kind: "nonsense", items: "bad-shape" }]
        }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  await service.submitCapture({ text: "Atlas launch still blocked", threadID: thread.id });

  await service.synthesizeThreadState({ threadID: thread.id });
  await service.prepareThread({ threadID: thread.id, type: "writing" });
  const inbox = await service.submitCapture({ text: "Need route debug" });

  const threadBeforeReset = service.openThread(thread.id);
  assert.equal(threadBeforeReset.aiStatus.resume.status, "invalidPlan");
  assert.equal(Boolean(threadBeforeReset.preparedView), true);
  assert.equal(Boolean(service.homeView().aiState.routeDebugByEntryID[inbox.entry.id]), true);

  await service.configureAIProvider({
    providerKind: "openAI",
    baseURL: "https://api.openai.com/v1",
    model: "gpt-4.1-mini",
    apiKey: "sk-test"
  });

  const threadAfterReset = service.openThread(thread.id);
  assert.equal(threadAfterReset.preparedView, null);
  assert.notEqual(threadAfterReset.aiStatus.prepare.status, "ready");
  assert.deepEqual(service.homeView().aiState.routeDebugByEntryID, {});
});

test("clean-room application service resolves reference targets and backlinks", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const target = await service.submitCapture({
    text: "Atlas spec"
  });
  await service.routeEntryToThread({ entryID: target.entry.id, threadID: thread.id });
  await service.submitCapture({
    text: "Need [[supports|Atlas spec]]",
    threadID: thread.id,
    references: [{ label: "Atlas spec", relationKind: "supports", targetID: target.entry.id }]
  });

  const threadView = service.openThread(thread.id);
  const sourceEntry = threadView.entries.find((entry) => entry.summaryText === "Need [[supports|Atlas spec]]");
  const targetEntry = threadView.entries.find((entry) => entry.id === target.entry.id);

  assert.equal(sourceEntry.references[0].targetID, target.entry.id);
  assert.equal(sourceEntry.references[0].isResolved, true);
  assert.equal(sourceEntry.references[0].targetSummaryText, "Atlas spec");
  assert.equal(targetEntry.incomingBacklinks[0].sourceEntryID, sourceEntry.id);
  assert.equal(targetEntry.incomingBacklinks[0].relationKind, "supports");
});

test("clean-room application service keeps references and backlinks stable when target text changes", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const target = await service.submitCapture({ text: "Atlas spec\nSecond line", threadID: thread.id });
  const source = await service.submitCapture({
    text: "Need [[Atlas spec]]",
    threadID: thread.id,
    references: [{ label: "Atlas spec", relationKind: "informs", targetID: target.entry.id }]
  });

  await service.updateEntryText({
    entryID: target.entry.id,
    text: "Atlas spec revised\nAnother detail"
  });

  const threadView = service.openThread(thread.id);
  const updatedSource = threadView.entries.find((entry) => entry.id === source.entry.id);
  const updatedTarget = threadView.entries.find((entry) => entry.id === target.entry.id);

  assert.equal(updatedSource.references[0].targetID, target.entry.id);
  assert.equal(updatedSource.references[0].targetSummaryText, "Atlas spec revised");
  assert.equal(updatedTarget.incomingBacklinks[0].sourceEntryID, source.entry.id);
  assert.equal(updatedTarget.incomingBacklinks[0].sourceSummaryText, "Need");
});

test("clean-room application service migrates legacy visible target ids on workspace load", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const target = createEntry({
    threadID: thread.id,
    summaryText: "Atlas spec",
    references: []
  });
  const source = createEntry({
    threadID: thread.id,
    summaryText: `Need [[${target.id}|Atlas spec]]`,
    references: []
  });
  await service.repository.saveEntry(target);
  await service.repository.saveEntry(source);
  await service.repository.flush();

  service.loadWorkspace();
  const threadView = service.openThread(thread.id);
  const migratedSource = threadView.entries.find((entry) => entry.id === source.id);
  const migratedTarget = threadView.entries.find((entry) => entry.id === target.id);

  assert.equal(migratedSource.summaryText, "Need [[Atlas spec]]");
  assert.equal(migratedSource.references[0].targetID, target.id);
  assert.equal(migratedTarget.incomingBacklinks[0].sourceEntryID, source.id);
});

test("clean-room application service migrates legacy explicit relation tokens without exposing target ids", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const target = createEntry({
    threadID: thread.id,
    summaryText: "Atlas spec",
    references: []
  });
  const source = createEntry({
    threadID: thread.id,
    summaryText: `Need [[supports|${target.id}|Atlas spec]]`,
    references: []
  });
  await service.repository.saveEntry(target);
  await service.repository.saveEntry(source);
  await service.repository.flush();

  service.loadWorkspace();
  const threadView = service.openThread(thread.id);
  const migratedSource = threadView.entries.find((entry) => entry.id === source.id);

  assert.equal(migratedSource.summaryText, "Need [[supports|Atlas spec]]");
  assert.equal(migratedSource.references[0].relationKind, "supports");
  assert.equal(migratedSource.references[0].targetID, target.id);
});

test("clean-room application service does not mis-migrate pipe syntax that is not a real entry id", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const source = createEntry({
    threadID: thread.id,
    summaryText: "Need [[foo|Atlas spec]]",
    references: []
  });
  await service.repository.saveEntry(source);
  await service.repository.flush();

  service.loadWorkspace();
  const threadView = service.openThread(thread.id);
  const untouchedSource = threadView.entries.find((entry) => entry.id === source.id);

  assert.equal(untouchedSource.summaryText, "Need [[foo|Atlas spec]]");
  assert.equal(untouchedSource.references[0].targetID, null);
  assert.equal(untouchedSource.references[0].label, "foo|Atlas spec");
});

test("clean-room application service exposes rich preview contract for entry sources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const entry = createEntry({
    threadID: thread.id,
    kind: EntryKind.SOURCE,
    body: { url: "https://example.com/spec", title: "Spec" },
    summaryText: "Spec reference"
  });
  await service.repository.saveEntry(entry);
  await service.repository.flush();
  service.loadWorkspace();

  const preview = await service.getEntryRichPreview(entry.id);
  assert.equal(preview.sourceKind, "url");
  assert.equal(preview.title, "Atlas Spec");
  assert.equal(preview.citation, "Launch document");
});

test("clean-room text-only capture preserves default body (no body.text set)", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const result = await service.submitCapture({ text: "hello" });

  assert.deepEqual(result.entry.body, {});
});

test("clean-room attachment capture sets body.text and body.attachments", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const attachments = [
    { relativePath: "attachments/abc.docx", fileName: "abc.docx", mimeType: "application/vnd.openxmlformats-officedocument.wordprocessingml.document", size: 5000 }
  ];
  const result = await service.submitCapture({ text: "see this", attachments });

  assert.equal(result.entry.body.text, "see this");
  assert.equal(result.entry.body.attachments.length, 1);
  assert.equal(result.entry.body.attachments[0].fileName, "abc.docx");
});

test("clean-room attachment-only capture (empty text) sets body.attachments", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const attachments = [
    { relativePath: "attachments/img.png", fileName: "img.png", mimeType: "image/png", size: 12000 }
  ];
  const result = await service.submitCapture({ text: "", attachments });

  assert.ok(result.entry, "entry should be created");
  assert.equal(result.entry.body.attachments.length, 1);
  assert.equal(result.entry.body.text, "");
});

test("clean-room attachment capture in thread appears in home and thread resources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  await service.submitCapture({
    text: "",
    threadID: thread.id,
    attachments: [
      { relativePath: "attachments/img.png", fileName: "img.png", mimeType: "image/png", size: 12000 }
    ]
  });

  const home = service.homeView();
  const threadView = service.openThread(thread.id);

  assert.equal(home.resources.some((resource) => resource.attachment?.fileName === "img.png"), true);
  assert.equal(threadView.resources.some((resource) => resource.attachment?.fileName === "img.png"), true);
  assert.equal(threadView.resourceCounts.mediaCount, 1);
});

test("clean-room inbox attachment capture appears in global resources but not thread resources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  await service.submitCapture({
    text: "",
    attachments: [
      { relativePath: "attachments/inbox.pdf", fileName: "inbox.pdf", mimeType: "application/pdf", size: 5000 }
    ]
  });

  const home = service.homeView();
  const threadView = service.openThread(thread.id);
  assert.equal(home.resources.some((resource) => resource.attachment?.fileName === "inbox.pdf" && resource.threadID == null), true);
  assert.equal(threadView.resources.some((resource) => resource.attachment?.fileName === "inbox.pdf"), false);
});

test("clean-room inbox link appears in global resources but not thread resources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  await service.submitCapture({
    text: "https://example.com/inbox"
  });

  const home = service.homeView();
  const threadView = service.openThread(thread.id);
  assert.equal(home.resources.some((resource) => resource.kind === "link" && resource.locator === "https://example.com/inbox" && resource.threadID == null), true);
  assert.equal(threadView.resources.some((resource) => resource.locator === "https://example.com/inbox"), false);
});

test("clean-room application service writes clipboard attachments into workspace", () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const saved = service.writeAttachmentBuffer({
    bytes: new Uint8Array([0x89, 0x50, 0x4e, 0x47]),
    fileName: "clipboard-image.png",
    mimeType: "image/png"
  });

  assert.equal(saved.relativePath.endsWith(".png"), true);
  assert.equal(fs.existsSync(saved.absolutePath), true);
});

test("clean-room application service fast-classifies obvious questions on submit", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  const result = await service.submitCapture({
    text: "什么是新时代 AI 人机交互？"
  });

  assert.equal(result.entry.kind, EntryKind.QUESTION);
  assert.equal(result.entry.sourceMetadata.kindAttribution.source, "heuristic");
});

test("clean-room application service lets manual kind updates lock AI overrides until reset to note", async () => {
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  let classifyCalls = 0;
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      classifyCalls += 1;
      return {
        kind: EntryKind.CLAIM,
        reason: "Looks like a claim",
        confidence: 0.92,
        debugPayload: null
      };
    },
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));

  const capture = await service.submitCapture({
    text: "随手记一条普通笔记"
  });

  await service.updateEntryKind({ entryID: capture.entry.id, kind: EntryKind.QUESTION });
  await sleep(100);

  let entry = service.homeView().inboxEntries.find((item) => item.id === capture.entry.id);
  assert.equal(entry.kind, EntryKind.QUESTION);
  assert.equal(entry.sourceMetadata.kindOverride.source, "manual");
  assert.equal(classifyCalls, 0);

  await service.updateEntryKind({ entryID: capture.entry.id, kind: EntryKind.NOTE });
  await sleep(100);

  entry = service.homeView().inboxEntries.find((item) => item.id === capture.entry.id);
  assert.equal(entry.kind, EntryKind.CLAIM);
  assert.equal(entry.sourceMetadata.kindOverride, undefined);
  assert.equal(entry.sourceMetadata.kindAttribution.source, "ai");
  assert.equal(classifyCalls > 0, true);
});

test("clean-room application service schedules entry classification for note entries and updates kind", async () => {
  const classificationStarted = deferred();
  const releaseClassification = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      classificationStarted.resolve();
      await releaseClassification.promise;
      return {
        kind: EntryKind.QUESTION,
        reason: "This entry is asking for a design decision.",
        confidence: 0.91,
        debugPayload: null
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    },
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "How should we design the atlas box?",
    threadID: thread.id
  });

  await sleep(90);
  await classificationStarted.promise;
  const pendingEntry = service.openThread(thread.id).entries.find((entry) => entry.id === result.entry.id);
  assert.equal(pendingEntry.aiActivity.kind, "entryClassifying");
  assert.equal(pendingEntry.aiActivity.label, "AI 正在判断笔记类型");

  releaseClassification.resolve();
  await sleep(0);
  await sleep(0);

  const settledEntry = service.openThread(thread.id).entries.find((entry) => entry.id === result.entry.id);
  assert.equal(settledEntry.kind, EntryKind.QUESTION);
  assert.equal(settledEntry.aiActivity, undefined);
  assert.equal(
    service.homeView().aiState.entryClassificationDebugByEntryID[result.entry.id]?.suggestedKind,
    EntryKind.QUESTION
  );
});

test("clean-room application service skips entry classification when capture has explicit tag", async () => {
  let classifyCalls = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      classifyCalls += 1;
      return {
        kind: EntryKind.QUESTION,
        reason: "Should not apply",
        confidence: 0.95,
        debugPayload: null
      };
    },
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));

  const result = await service.submitCapture({
    text: "#claim Atlas launch needs legal review"
  });

  await sleep(100);

  const entry = service.homeView().inboxEntries.find((item) => item.id === result.entry.id);
  assert.equal(classifyCalls, 0);
  assert.equal(entry.kind, EntryKind.CLAIM);
  assert.equal(entry.sourceMetadata.captureInterpretation.explicitTag, "claim");
});

test("clean-room application service clears entry classification activity when task is cancelled", async () => {
  const classificationStarted = deferred();
  let capturedSignal = null;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind(_request, { signal } = {}) {
      capturedSignal = signal;
      classificationStarted.resolve();
      await new Promise((resolve, reject) => {
        signal.addEventListener("abort", () => reject(signal.reason ?? new Error("aborted")), { once: true });
      });
    },
    async planRoute() {
      return {
        shouldRoute: false,
        selectedThreadID: null,
        decisionReason: "keep in inbox",
        suggestions: []
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  const result = await service.submitCapture({
    text: "How should we design the atlas box?",
    threadID: thread.id
  });

  await sleep(90);
  await classificationStarted.promise;
  assert.equal(service.openThread(thread.id).entries.find((entry) => entry.id === result.entry.id)?.aiActivity.kind, "entryClassifying");

  service.invalidateAIOutputState("test cancel", { entryIDs: [result.entry.id] });
  await sleep(0);

  assert.equal(Boolean(capturedSignal?.aborted), true);
  assert.equal(service.openThread(thread.id).entries.find((entry) => entry.id === result.entry.id)?.aiActivity, undefined);
  assert.equal(service.homeView().aiState.entryClassificationDebugByEntryID[result.entry.id], undefined);
});

test("clean-room application service emits thread-aware async updates for thread entry classification", async () => {
  const asyncSnapshots = [];
  const classificationStarted = deferred();
  const releaseClassification = deferred();
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      classificationStarted.resolve();
      await releaseClassification.promise;
      return {
        kind: EntryKind.QUESTION,
        reason: "Question detected",
        confidence: 0.9,
        debugPayload: null
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({
    aiProviderRuntimeOverride: aiProviderRuntime,
    aiServiceOverride: aiService,
    onAsyncStateChanged: (payload) => {
      asyncSnapshots.push({
        threadID: payload?.threadID ?? null,
        aiActivity: service.openThread(thread.id)?.entries.find((entry) => entry.summaryText.includes("atlas box"))?.aiActivity ?? null
      });
    }
  });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  await service.submitCapture({
    text: "How should we design the atlas box?",
    threadID: thread.id
  });

  await sleep(90);
  await classificationStarted.promise;
  releaseClassification.resolve();
  await sleep(0);
  await sleep(0);

  assert.equal(asyncSnapshots.some((payload) => payload?.threadID === thread.id), true);
  assert.equal(asyncSnapshots.some((payload) => payload?.threadID === thread.id && payload?.aiActivity == null), true);
});

test("clean-room application service waits for a quiet period before refreshing thread ai state", async () => {
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      return {
        kind: EntryKind.NOTE,
        reason: "Keep note",
        confidence: 0.3,
        debugPayload: null
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  for (const text of ["First note", "Second note", "Third note", "Fourth note"]) {
    await service.submitCapture({ text, threadID: thread.id });
    await sleep(120);
  }

  await sleep(1400);

  assert.notEqual(service.openThread(thread.id)?.aiStatus?.resume?.status, "ready");

  await service.submitCapture({
    text: "Fifth note",
    threadID: thread.id
  });

  for (let index = 0; index < 20; index += 1) {
    if (service.openThread(thread.id)?.aiStatus?.resume?.status === "ready") {
      break;
    }
    await sleep(100);
  }

  assert.equal(service.openThread(thread.id)?.aiStatus?.resume?.status, "ready");
});

test("clean-room application service refreshes resume without discourse for plain thread replies", async () => {
  let discourseCalls = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      return {
        kind: EntryKind.NOTE,
        reason: "Keep note",
        confidence: 0.3,
        debugPayload: null
      };
    },
    async synthesizeResume() {
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      discourseCalls += 1;
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const parent = await service.submitCapture({
    text: "Parent note",
    threadID: thread.id
  });

  for (const text of ["Follow-up note 1", "Follow-up note 2", "Follow-up note 3", "Follow-up note 4"]) {
    await service.appendReply({
      entryID: parent.entry.id,
      text
    });
  }

  for (let index = 0; index < 20; index += 1) {
    if (service.openThread(thread.id)?.aiStatus?.resume?.status === "ready") {
      break;
    }
    await sleep(100);
  }

  assert.equal(service.openThread(thread.id)?.aiStatus?.resume?.status, "ready");
  assert.equal(discourseCalls, 0);
});

test("clean-room application service retains resume pending count after resume failure", async () => {
  let resumeCalls = 0;
  const aiProviderRuntime = {
    config: { model: "mock-model" },
    backendLabel: "Mock LLM · mock-model",
    preferredMaxConcurrentRequests: 1
  };
  const aiService = {
    requestQueue: new AIRequestQueue({ maxConcurrent: 1 }),
    async classifyEntryKind() {
      return {
        kind: EntryKind.NOTE,
        reason: "Keep note",
        confidence: 0.3,
        debugPayload: null
      };
    },
    async synthesizeResume() {
      resumeCalls += 1;
      if (resumeCalls === 1) {
        throw new Error("temporary resume failure");
      }
      return {
        currentJudgment: "Ready",
        openLoops: [],
        nextAction: null,
        restartNote: "Ready",
        recoveryLines: [],
        resolvedSoFar: [],
        recommendedNextSteps: [],
        presentationPlan: { headline: "Ready", blocks: [] }
      };
    },
    async inferDiscourseRelations() {
      return { relations: [] };
    },
    async prepareDraft() {
      return { title: "Draft", openLoops: [], recommendedNextSteps: [] };
    }
  };
  const { root, service } = makeAppService({ aiProviderRuntimeOverride: aiProviderRuntime, aiServiceOverride: aiService });
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });

  for (const text of ["One", "Two", "Three", "Four", "Five"]) {
    await service.submitCapture({ text, threadID: thread.id });
  }

  for (let index = 0; index < 20; index += 1) {
    const status = service.openThread(thread.id)?.aiStatus?.resume?.status;
    if (status === "failed") {
      break;
    }
    await sleep(100);
  }

  assert.equal(service.openThread(thread.id)?.aiStatus?.resume?.status, "failed");
  assert.equal(resumeCalls, 1);

  await service.submitCapture({ text: "Six", threadID: thread.id });

  for (let index = 0; index < 20; index += 1) {
    const status = service.openThread(thread.id)?.aiStatus?.resume?.status;
    if (status === "ready") {
      break;
    }
    await sleep(100);
  }

  assert.equal(service.openThread(thread.id)?.aiStatus?.resume?.status, "ready");
  assert.equal(resumeCalls, 2);
});
