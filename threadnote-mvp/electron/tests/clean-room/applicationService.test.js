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
  EntryKind,
  createEntry,
  createThreadAISnapshot
} from "../../src/domain/models/threadnoteModels.js";

function makeTempDir() {
  return fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-app-service-"));
}

function makeAppService({ runtimePayload = null } = {}) {
  const root = makeTempDir();
  const workspaceManager = new WorkspaceManager({
    stateFilePath: path.join(root, "workspace-state.json")
  });
  let aiProviderRuntime = null;
  let aiService = null;

  if (runtimePayload) {
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

test("clean-room application service keeps ambiguous capture in inbox without forcing global resources", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));

  await service.submitCapture({
    text: "Check https://example.com and talk to @Atlas"
  });

  const home = service.homeView();
  assert.equal(home.inboxEntries.length, 1);
  assert.equal(home.resources.length, 0);
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
  assert.equal(ping.ok, false);

  const prepared = await service.prepareThread({ threadID: thread.id, type: "writing" });
  assert.equal(prepared.title, "Atlas unblock draft");
  assert.equal(prepared.contentState.status, "ready");
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
  const tested = await service.saveAndPingAIProvider({
    providerKind: "ollama",
    baseURL: "http://localhost:11434/v1",
    model: "qwen3.5:4b",
    embeddingModel: "nomic-embed-text"
  });

  assert.equal(saved.embeddingModel, "nomic-embed-text");
  assert.equal(service.getAIProviderConfig().providerKind, "ollama");
  assert.equal(tested.config.model, "qwen3.5:4b");
});

test("clean-room application service appends replies edits entries routes inbox items and deletes trees", async () => {
  const { root, service } = makeAppService();
  service.createWorkspace(path.join(root, "Atlas"));
  const thread = await service.createThread({ title: "Atlas launch", prompt: "Atlas launch" });
  const inboxCapture = await service.submitCapture({ text: "Inbox item for [[Plan]] @Atlas" });

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
    text: "Edited entry for [[Spec]]"
  });
  let threadView = service.openThread(thread.id);
  assert.equal(threadView.entries[0].references[0].label, "Spec");

  await service.deleteEntry(inboxCapture.entry.id);
  threadView = service.openThread(thread.id);
  assert.equal(threadView.entries.length, 0);
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
