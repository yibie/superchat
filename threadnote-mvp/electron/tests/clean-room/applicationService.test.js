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
