import path from "node:path";
import { CaptureInterpreter } from "../../domain/capture/captureInterpreter.js";
import { ThreadRoutingEngine } from "../../domain/routing/threadRoutingEngine.js";
import { deriveResources, resourceCounts } from "../../domain/resources/resourceDerivation.js";
import {
  createDiscourseRelation,
  createEntry,
  createThreadAISnapshot,
  createThreadRecord,
  ThreadColorValues
} from "../../domain/models/threadnoteModels.js";
import {
  DEFAULT_REFERENCE_RELATION,
  EXPLICIT_REFERENCE_RELATIONS,
  deriveReferenceTargetLabel,
  parseReferencesFromText
} from "../../domain/references/referenceSyntax.js";
import { ThreadStatus } from "../../domain/models/threadnoteModels.js";
import { DiscourseInferenceEngine } from "../../domain/discourse/discourseHeuristics.js";
import { AttachmentManager } from "../../infrastructure/persistence/workspace/attachmentManager.js";
import { SQLitePersistenceStore } from "../../infrastructure/persistence/stores/sqlitePersistenceStore.js";
import { ThreadnoteRepository } from "../../infrastructure/persistence/repositories/threadnoteRepository.js";
import { LinkMetadataService } from "../../infrastructure/metadata/linkMetadataService.js";

export class ThreadnoteApplicationService {
  constructor({
    workspaceManager,
    aiProviderRuntime = null,
    aiService = null,
    repositoryFactory = (databasePath) => new ThreadnoteRepository({ store: new SQLitePersistenceStore(databasePath) }),
    linkMetadataService = null
  }) {
    this.workspaceManager = workspaceManager;
    this.aiProviderRuntime = aiProviderRuntime;
    this.aiService = aiService;
    this.repositoryFactory = repositoryFactory;
    this.linkMetadataService =
      linkMetadataService ??
      new LinkMetadataService({
        workspacePathResolver: () => this.workspaceManager?.describe()?.workspacePath ?? null
    });
    this.captureInterpreter = new CaptureInterpreter();
    this.discourseInferenceEngine = new DiscourseInferenceEngine();
    this.repository = null;
    this.snapshot = emptySnapshot();
    this.routePlanningTasks = new Map();
    this.routePlanningTokens = new Map();
    this.routePlanningProcessingEntryIDs = new Set();
    this.resumeSynthesisTasks = new Map();
    this.resumeSynthesisTokens = new Map();
    this.resumeSynthesisProcessingThreadIDs = new Set();
    this.draftPreparationTasks = new Map();
    this.draftPreparationTokens = new Map();
    this.draftPreparationProcessingThreadIDs = new Set();
    this.discourseInferenceTasks = new Map();
    this.discourseInferenceTokens = new Map();
    this.discourseInferenceProcessingThreadIDs = new Set();
  }

  createWorkspace(workspacePath) {
    const workspace = this.workspaceManager.createWorkspace(workspacePath);
    this.#configureRepository(workspace.databasePath);
    return this.loadWorkspace();
  }

  openWorkspace(workspacePath) {
    const workspace = this.workspaceManager.openWorkspace(workspacePath);
    this.#configureRepository(workspace.databasePath);
    return this.loadWorkspace();
  }

  restoreWorkspace() {
    const workspace = this.workspaceManager.restoreWorkspace();
    if (!workspace) {
      this.repository = null;
      this.snapshot = emptySnapshot();
      return null;
    }
    this.#configureRepository(workspace.databasePath);
    return this.loadWorkspace();
  }

  loadWorkspace() {
    this.#assertRepository();
    this.snapshot = this.repository.loadSnapshot();
    if (migrateLegacyReferenceEntries(this.snapshot.entries, this.repository)) {
      this.snapshot = this.repository.loadSnapshot();
    }
    this.repository.rebuildKnowledgeIndexSync();
    return {
      workspace: this.workspaceManager.describe(),
      home: this.homeView()
    };
  }

  homeView() {
    const resolvedEntries = withEntryAIActivity(
      resolveReferenceGraph(this.snapshot.entries),
      this.#entryAIActivityState()
    );
    const threadEntryCounts = new Map();
    for (const entry of resolvedEntries) {
      if (entry.threadID) {
        threadEntryCounts.set(entry.threadID, (threadEntryCounts.get(entry.threadID) ?? 0) + 1);
      }
    }
    return {
      threads: this.snapshot.threads
        .filter((thread) => thread.status !== ThreadStatus.ARCHIVED)
        .slice()
        .sort((lhs, rhs) => new Date(rhs.lastActiveAt).getTime() - new Date(lhs.lastActiveAt).getTime())
        .map((thread) => ({ ...thread, entryCount: threadEntryCounts.get(thread.id) ?? 0 })),
      inboxEntries: resolvedEntries
        .filter((entry) => !entry.parentEntryID)
        .slice()
        .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime()),
      allEntries: resolvedEntries,
      resources: deriveResources(resolvedEntries),
      resourceCounts: resourceCounts(deriveResources(resolvedEntries))
    };
  }

  async createThread({ title, prompt = title, goalLayer = null }) {
    this.#assertRepository();
    const existingCount = this.snapshot?.threads?.length ?? 0;
    const color = ThreadColorValues[existingCount % ThreadColorValues.length];
    const thread = createThreadRecord({ title, prompt, goalLayer, color });
    await this.repository.saveThread(thread);
    await this.repository.flush();
    return this.loadWorkspace().home.threads.find((item) => item.id === thread.id) ?? thread;
  }

  async submitCapture({ text, threadID = null, attachments = [], references = [] }) {
    this.#assertRepository();
    const interpretation = this.captureInterpreter.interpretText(text);
    const useAIRouting = !threadID && Boolean(this.aiService);
    const routing = threadID
      ? { type: "route", threadID, reason: "Manually assigned" }
      : useAIRouting
        ? { type: "noMatch", reason: "Pending AI route decision." }
        : this.#routingEngine().decideFromInterpretation(interpretation);
    const entry = createEntry({
      threadID: threadID || (useAIRouting ? null : routing.type === "route" ? routing.threadID : null),
      kind: interpretation.detectedItemType,
      summaryText: interpretation.normalizedText,
      objectMentions: interpretation.detectedObjects,
      references: mergeReferenceBindings({
        parsedReferences: parseReferencesFromText(interpretation.normalizedText),
        explicitReferences: references
      }),
      confidenceScore: interpretation.confidenceScore,
      ...(attachments?.length > 0
        ? { body: { text: interpretation.normalizedText, attachments } }
        : {})
    });
    await this.repository.saveEntry(entry);
    await this.repository.flush();
    this.loadWorkspace();

    const backgroundTask = createCaptureBackgroundTask({
      entryID: entry.id,
      threadID,
      useAIRouting: useAIRouting && Boolean(this.aiService),
      refreshThreadID: threadID && this.aiService ? threadID : null
    });

    return {
      entry,
      routingDecision: routing,
      backgroundTask
    };
  }

  async submitExternalCapture({
    text = "",
    attachments = [],
    source = "quickCaptureHotkey",
    sourceContext = {},
    references = []
  }) {
    this.#assertRepository();
    const normalizedText = String(text ?? "");
    if (!normalizedText.trim() && (!Array.isArray(attachments) || attachments.length === 0)) {
      throw new Error("External capture must include text or attachments");
    }

    const result = await this.submitCapture({
      text: normalizedText,
      threadID: null,
      attachments,
      references
    });

    const savedEntry = this.snapshot.entries.find((entry) => entry.id === result.entry.id) ?? result.entry;
    const updatedEntry = {
      ...savedEntry,
      sourceMetadata: {
        ...(savedEntry.sourceMetadata ?? {}),
        externalCapture: {
          source,
          sourceContext: { ...(sourceContext ?? {}) }
        }
      }
    };
    await this.repository.saveEntry(updatedEntry);
    await this.repository.flush();
    this.loadWorkspace();

    return {
      ...result,
      entry: updatedEntry
    };
  }

  async finalizeCaptureAsync({ entryID, useAIRouting = false, refreshThreadID = null } = {}) {
    this.#assertRepository();
    if (!entryID) {
      return {
        routingDecision: null,
        threadID: refreshThreadID ?? null,
        thread: refreshThreadID ? this.openThread(refreshThreadID) : null
      };
    }

    let routingDecision = null;
    let threadID = refreshThreadID ?? null;

    if (useAIRouting) {
      routingDecision = await this.planRouteForEntry({ entryID, autoRoute: true });
      threadID = routingDecision?.threadID ?? null;
    }

    if (threadID) {
      await this.#refreshThreadAI(threadID);
    }

    this.loadWorkspace();

    return {
      routingDecision,
      threadID,
      thread: threadID ? this.openThread(threadID) : null
    };
  }

  copyAttachment(sourcePath) {
    const workspace = this.workspaceManager.describe();
    if (!workspace?.attachmentsPath) {
      throw new Error("Workspace is not configured");
    }
    return AttachmentManager.copyFile(sourcePath, workspace.attachmentsPath);
  }

  writeAttachmentBuffer({ bytes, fileName = "", mimeType = "" }) {
    const workspace = this.workspaceManager.describe();
    if (!workspace?.attachmentsPath) {
      throw new Error("Workspace is not configured");
    }
    return AttachmentManager.writeBuffer({
      buffer: Buffer.from(bytes ?? []),
      attachmentsPath: workspace.attachmentsPath,
      fileName,
      mimeType
    });
  }

  openThread(threadID) {
    this.#assertRepository();
    const thread = this.snapshot.threads.find((item) => item.id === threadID) ?? null;
    if (!thread) {
      return null;
    }
    const resolvedEntries = withEntryAIActivity(
      resolveReferenceGraph(this.snapshot.entries),
      this.#entryAIActivityState()
    );
    const entries = resolvedEntries
      .filter((entry) => entry.threadID === threadID)
      .slice()
      .sort((lhs, rhs) => new Date(lhs.createdAt).getTime() - new Date(rhs.createdAt).getTime());
    const claims = this.snapshot.claims.filter((claim) => claim.threadID === threadID);
    const anchors = this.snapshot.anchors.filter((anchor) => anchor.threadID === threadID);
    const tasks = this.snapshot.tasks.filter((task) => task.threadID === threadID);
    const discourseRelations = this.snapshot.discourseRelations.filter((relation) => {
      const source = this.snapshot.entries.find((entry) => entry.id === relation.sourceEntryID);
      return source?.threadID === threadID;
    });
    const memory = this.repository.fetchMemory(threadID);
    const resources = deriveResources(entries);
    const aiSnapshot = this.snapshot.aiSnapshots.find((snapshot) => snapshot.threadID === threadID) ?? null;
    return {
      thread,
      entries,
      claims,
      anchors,
      tasks,
      discourseRelations,
      memory,
      aiSnapshot,
      resources,
      resourceCounts: resourceCounts(resources)
    };
  }

  async appendReply({ entryID, text, references = [] }) {
    this.#assertRepository();
    const parent = this.snapshot.entries.find((entry) => entry.id === entryID) ?? null;
    if (!parent) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const normalizedText = String(text ?? "").trim();
    if (!normalizedText) {
      throw new Error("Reply cannot be empty");
    }
    const interpretation = this.captureInterpreter.interpretText(normalizedText);
    const reply = createEntry({
      threadID: parent.threadID,
      parentEntryID: parent.id,
      kind: interpretation.detectedItemType,
      summaryText: interpretation.normalizedText,
      objectMentions: interpretation.detectedObjects,
      references: mergeReferenceBindings({
        parsedReferences: parseReferencesFromText(interpretation.normalizedText),
        explicitReferences: references
      }),
      confidenceScore: interpretation.confidenceScore,
      inboxState: parent.threadID ? "resolved" : "unresolved"
    });
    await this.repository.saveEntry(reply);
    await this.repository.flush();
    this.loadWorkspace();
    if (parent.threadID) {
      await this.#refreshThreadAI(parent.threadID);
    }
    return {
      entry: reply
    };
  }

  async updateEntryText({ entryID, text, references = [] }) {
    this.#assertRepository();
    const existing = this.snapshot.entries.find((entry) => entry.id === entryID) ?? null;
    if (!existing) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const normalizedText = String(text ?? "").trim();
    if (!normalizedText) {
      throw new Error("Entry text cannot be empty");
    }
    const interpretation = this.captureInterpreter.interpretText(normalizedText);
    const updated = {
      ...existing,
      summaryText: interpretation.normalizedText,
      objectMentions: interpretation.detectedObjects,
      references: mergeReferenceBindings({
        parsedReferences: parseReferencesFromText(interpretation.normalizedText),
        explicitReferences: references,
        existingReferences: existing.references ?? []
      }),
      confidenceScore: interpretation.confidenceScore
    };
    await this.repository.saveEntry(updated);
    await this.repository.flush();
    this.loadWorkspace();
    if (updated.threadID) {
      await this.#refreshThreadAI(updated.threadID);
    } else if (this.aiService) {
      await this.planRouteForEntry({ entryID, autoRoute: true });
    }
    return {
      entry: updated
    };
  }

  async deleteEntry(entryID) {
    this.#assertRepository();
    const ids = collectEntryTree(this.snapshot.entries, entryID);
    if (ids.length === 0) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const threadID = this.snapshot.entries.find((entry) => ids.includes(entry.id))?.threadID ?? null;
    await this.repository.deleteEntries(ids);
    await this.repository.flush();
    this.loadWorkspace();
    if (threadID) {
      await this.#refreshThreadAI(threadID);
    }
    return true;
  }

  async routeEntryToThread({ entryID, threadID }) {
    this.#assertRepository();
    const thread = this.snapshot.threads.find((item) => item.id === threadID) ?? null;
    if (!thread) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    const ids = collectEntryTree(this.snapshot.entries, entryID);
    if (ids.length === 0) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    for (const id of ids) {
      const existing = this.snapshot.entries.find((entry) => entry.id === id);
      if (!existing) {
        continue;
      }
      await this.repository.saveEntry({
        ...existing,
        threadID,
        inboxState: "resolved"
      });
    }
    await this.repository.flush();
    this.loadWorkspace();
    await this.#refreshThreadAI(threadID);
    return this.openThread(threadID);
  }

  async createThreadFromEntry({ entryID, title }) {
    const cleanTitle = String(title ?? "").trim();
    if (!cleanTitle) {
      throw new Error("Thread title cannot be empty");
    }
    const thread = await this.createThread({ title: cleanTitle, prompt: cleanTitle });
    await this.routeEntryToThread({ entryID, threadID: thread.id });
    return this.openThread(thread.id);
  }

  async archiveThread(threadID) {
    this.#assertRepository();
    const existing = this.snapshot.threads.find((thread) => thread.id === threadID) ?? null;
    if (!existing) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    await this.repository.saveThread({
      ...existing,
      status: ThreadStatus.ARCHIVED,
      updatedAt: new Date()
    });
    await this.repository.flush();
    return this.loadWorkspace();
  }

  resourcesView({ threadID = null } = {}) {
    const entries = threadID
      ? this.snapshot.entries.filter((entry) => entry.threadID === threadID)
      : this.snapshot.entries;
    const resources = deriveResources(entries);
    return {
      threadID,
      resources,
      counts: resourceCounts(resources)
    };
  }

  async getEntryRichPreview(entryID) {
    this.#assertRepository();
    const entry = this.snapshot.entries.find((item) => item.id === entryID) ?? null;
    if (!entry) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const preview = await this.linkMetadataService?.getEntryRichPreview(entry);
    if (!preview) {
      return null;
    }
    return {
      entryID,
      ...preview
    };
  }

  async configureAIProvider(config) {
    if (!this.aiProviderRuntime) {
      throw new Error("AI provider runtime is unavailable");
    }
    this.#cancelAllAITasks("provider reconfigured");
    const saved = this.aiProviderRuntime.configure(config);
    this.aiService?.requestQueue?.setMaxConcurrent?.(this.aiProviderRuntime.preferredMaxConcurrentRequests || 2);
    this.loadWorkspace();
    return saved;
  }

  getAIProviderConfig() {
    return this.aiProviderRuntime?.config ?? null;
  }

  async pingAIProvider() {
    if (!this.aiProviderRuntime) {
      throw new Error("AI provider runtime is unavailable");
    }
    return this.aiProviderRuntime.ping();
  }

  async testAIProvider(config) {
    if (!this.aiProviderRuntime) {
      throw new Error("AI provider runtime is unavailable");
    }
    return this.aiProviderRuntime.pingWithConfig(config ?? {});
  }

  async planRouteForEntry({ entryID, autoRoute = true, limit = 3 } = {}) {
    this.#assertRepository();
    const entry = this.snapshot.entries.find((item) => item.id === entryID) ?? null;
    if (!entry) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const interpretation = this.captureInterpreter.interpretText(entry.summaryText ?? entry.body?.text ?? "");
    const support = this.#routingEngine().supportSnapshot(interpretation, new Set(), Math.max(limit, 5));
    const candidates = support.rankedCandidates.slice(0, limit);
    if (!this.aiService) {
      return this.#normalizeRoutingDecision(this.#routingEngine().decideFromInterpretation(interpretation));
    }
    if (candidates.length === 0) {
      return {
        type: "noMatch",
        reason: "No AI routing candidates available.",
        source: "ai"
      };
    }

    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.routePlanningTasks.get(entryID), `route:${entryID}`, "superseded route");
    this.routePlanningTasks.delete(entryID);
    this.routePlanningTokens.set(entryID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `route:${entryID}`,
      execute: async ({ signal }) => {
      this.routePlanningProcessingEntryIDs.add(entryID);
      this.#logAITask("started", {
        label: `route:${entryID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const result = await this.aiService.planRoute({
          entryID,
          normalizedText: support.normalizedText,
          detectedItemType: support.detectedItemType,
          detectedObjects: support.detectedObjects,
          candidateClaims: support.candidateClaims,
          routingQueries: support.routingQueries,
          candidates
        }, { signal });
        const activeToken = this.routePlanningTokens.get(entryID);
        if (activeToken !== token) {
          return { type: "stale", reason: "Superseded route result.", source: "ai" };
        }

        const candidateIDs = new Set(candidates.map((candidate) => candidate.threadID));
        const canRoute = Boolean(result.shouldRoute && result.selectedThreadID && candidateIDs.has(result.selectedThreadID));
        const routingDecision = canRoute
          ? {
              type: "route",
              threadID: result.selectedThreadID,
              reason: result.decisionReason || "AI selected a thread.",
              source: "ai"
            }
          : {
              type: "noMatch",
              reason: result.decisionReason || "AI kept the entry in inbox.",
              source: "ai"
            };

        if (canRoute && autoRoute) {
          const latest = this.snapshot.entries.find((item) => item.id === entryID) ?? null;
          if (latest) {
            await this.repository.saveEntry({
              ...latest,
              threadID: result.selectedThreadID,
              inboxState: "resolved"
            });
            await this.repository.flush();
            this.loadWorkspace();
          }
        }

        this.#logAITask("completed", {
          label: `route:${entryID}`,
          decision: routingDecision.type,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return routingDecision;
      } catch (error) {
        this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
          label: `route:${entryID}`,
          error: error?.message ?? String(error),
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        throw error;
      } finally {
        if (this.routePlanningTokens.get(entryID) === token) {
          this.routePlanningProcessingEntryIDs.delete(entryID);
          this.routePlanningTasks.delete(entryID);
          this.routePlanningTokens.delete(entryID);
        }
      }
      }
    });

    this.routePlanningTasks.set(entryID, handle);
    return handle.promise;
  }

  async synthesizeThreadState({ threadID } = {}) {
    this.#assertRepository();
    const threadView = this.openThread(threadID);
    if (!threadView) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    if (!this.aiService) {
      return threadView.aiSnapshot ?? null;
    }

    const fingerprint = this.#threadContentFingerprint(threadView);
    if (threadView.aiSnapshot?.contentFingerprint === fingerprint) {
      return threadView.aiSnapshot;
    }

    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.resumeSynthesisTasks.get(threadID), `resume:${threadID}`, "superseded resume");
    this.resumeSynthesisTasks.delete(threadID);
    this.resumeSynthesisTokens.set(threadID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `resume:${threadID}`,
      execute: async ({ signal }) => {
      this.resumeSynthesisProcessingThreadIDs.add(threadID);
      this.#logAITask("started", {
        label: `resume:${threadID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const latest = this.openThread(threadID);
        if (!latest) {
          return null;
        }
        const latestAnchor = latest.anchors.at(-1) ?? null;
        const result = await this.aiService.synthesizeResume({
          threadID,
          coreQuestion: latest.thread.goalLayer?.goalStatement ?? latest.thread.prompt ?? latest.thread.title,
          goalLayer: latest.thread.goalLayer ?? {},
          activeClaims: latest.claims.map((claim) => claim.statement),
          currentJudgment: latest.aiSnapshot?.currentJudgment ?? latestAnchor?.stateSummary ?? "",
          judgmentBasis: latest.entries
            .filter((entry) => entry.kind === "evidence" || entry.kind === "source")
            .slice(-2)
            .map((entry) => entry.summaryText)
            .join(" | "),
          openLoops: latestAnchor?.openLoops ?? latest.aiSnapshot?.openLoops ?? [],
          nextAction: latest.aiSnapshot?.nextAction ?? null,
          recoveryLines: latest.aiSnapshot?.recoveryLines ?? [],
          resolvedSoFar: latest.claims
            .filter((claim) => claim.status === "stable")
            .slice(0, 4)
            .map((claim) => claim.statement),
          recentNotes: latest.entries.slice(-6).map((entry) => ({
            id: entry.id,
            text: entry.summaryText,
            kind: entry.kind
          })),
          evidenceCount: latest.entries.filter((entry) => entry.kind === "evidence").length,
          sourceCount: latest.entries.filter((entry) => entry.kind === "source").length
        }, { signal });

        if (this.resumeSynthesisTokens.get(threadID) !== token) {
          return null;
        }

        const snapshot = createThreadAISnapshot({
          threadID,
          contentFingerprint: this.#threadContentFingerprint(latest),
          headline: result.presentationPlan?.headline ?? result.restartNote ?? latest.thread.title,
          blocks: result.presentationPlan?.blocks ?? [],
          restartNote: result.restartNote,
          currentJudgment: result.currentJudgment,
          openLoops: result.openLoops,
          nextAction: result.nextAction,
          recoveryLines: result.recoveryLines,
          synthesizedAt: Date.now(),
          modelID: this.aiProviderRuntime?.config?.model ?? ""
        });
        await this.repository.upsertAISnapshot(snapshot);
        await this.repository.flush();
        this.loadWorkspace();
        this.#logAITask("completed", {
          label: `resume:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return snapshot;
      } catch (error) {
        this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
          label: `resume:${threadID}`,
          error: error?.message ?? String(error),
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        throw error;
      } finally {
        if (this.resumeSynthesisTokens.get(threadID) === token) {
          this.resumeSynthesisProcessingThreadIDs.delete(threadID);
          this.resumeSynthesisTasks.delete(threadID);
          this.resumeSynthesisTokens.delete(threadID);
        }
      }
      }
    });

    this.resumeSynthesisTasks.set(threadID, handle);
    return handle.promise;
  }

  async inferDiscourseRelations({ threadID } = {}) {
    this.#assertRepository();
    const threadView = this.openThread(threadID);
    if (!threadView) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    if (!this.aiService) {
      return threadView.discourseRelations ?? [];
    }

    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.discourseInferenceTasks.get(threadID), `discourse:${threadID}`, "superseded discourse");
    this.discourseInferenceTasks.delete(threadID);
    this.discourseInferenceTokens.set(threadID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `discourse:${threadID}`,
      execute: async ({ signal }) => {
      this.discourseInferenceProcessingThreadIDs.add(threadID);
      this.#logAITask("started", {
        label: `discourse:${threadID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const latest = this.openThread(threadID);
        if (!latest) {
          return [];
        }
        const pairs = this.discourseInferenceEngine.findCandidatePairs(latest.entries);
        if (pairs.length === 0) {
          return latest.discourseRelations ?? [];
        }

        const result = await this.aiService.inferDiscourseRelations({
          threadID,
          pairs: pairs.slice(0, 10).map((pair, index) => ({
            pairIndex: index + 1,
            sourceID: pair.source.id,
            targetID: pair.target.id,
            sourceKind: pair.source.kind,
            targetKind: pair.target.kind,
            sourceSnippet: String(pair.source.summaryText ?? "").slice(0, 60),
            targetSnippet: String(pair.target.summaryText ?? "").slice(0, 60)
          }))
        }, { signal });

        if (this.discourseInferenceTokens.get(threadID) !== token) {
          return [];
        }

        const threadEntryIDs = new Set(latest.entries.map((entry) => entry.id));
        const retained = (latest.discourseRelations ?? []).filter((relation) => {
          const touchesThread = threadEntryIDs.has(relation.sourceEntryID) || threadEntryIDs.has(relation.targetEntryID);
          return !touchesThread || Number(relation.confidence ?? 0) >= 0.9;
        });
        const inferred = (result.relations ?? []).map((relation) =>
          createDiscourseRelation({
            sourceEntryID: relation.sourceEntryID,
            targetEntryID: relation.targetEntryID,
            kind: relation.kind,
            confidence: 0.75
          })
        );

        await this.repository.replaceDiscourseRelations(Array.from(threadEntryIDs), [...retained, ...inferred]);
        await this.repository.flush();
        this.loadWorkspace();
        this.#logAITask("completed", {
          label: `discourse:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return this.openThread(threadID)?.discourseRelations ?? [];
      } catch (error) {
        this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
          label: `discourse:${threadID}`,
          error: error?.message ?? String(error),
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        throw error;
      } finally {
        if (this.discourseInferenceTokens.get(threadID) === token) {
          this.discourseInferenceProcessingThreadIDs.delete(threadID);
          this.discourseInferenceTasks.delete(threadID);
          this.discourseInferenceTokens.delete(threadID);
        }
      }
      }
    });

    this.discourseInferenceTasks.set(threadID, handle);
    return handle.promise;
  }

  async prepareThread({ threadID, type = "writing" }) {
    const threadView = this.openThread(threadID);
    if (!threadView) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    if (!this.aiService) {
      return {
        threadID,
        type,
        title: `${capitalize(type)} unavailable`,
        openLoops: [],
        recommendedNextSteps: [],
        contentState: {
          status: "notConfigured",
          message: "AI backend is not configured."
        }
      };
    }
    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.draftPreparationTasks.get(threadID), `prepare:${threadID}`, "superseded prepare");
    this.draftPreparationTasks.delete(threadID);
    this.draftPreparationTokens.set(threadID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `prepare:${threadID}`,
      execute: async ({ signal }) => {
      this.draftPreparationProcessingThreadIDs.add(threadID);
      this.#logAITask("started", {
        label: `prepare:${threadID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const latest = this.openThread(threadID);
        if (!latest) {
          throw new Error(`Unknown thread: ${threadID}`);
        }
        const prepared = await this.aiService.prepareDraft({
          threadID,
          type,
          coreQuestion: latest.thread.goalLayer.goalStatement,
          activeClaims: latest.claims.map((claim) => claim.statement),
          openLoops: latest.anchors.at(-1)?.openLoops ?? [],
          keyEvidence: latest.entries
            .filter((entry) => entry.kind === "evidence")
            .slice(-5)
            .map((entry) => ({ id: entry.id, text: entry.summaryText })),
          recentNotes: latest.entries.slice(-6).map((entry) => ({ id: entry.id, text: entry.summaryText }))
        }, { signal });

        if (this.draftPreparationTokens.get(threadID) !== token) {
          return {
            threadID,
            type,
            title: `${capitalize(type)} stale`,
            openLoops: [],
            recommendedNextSteps: [],
            contentState: {
              status: "stale",
              message: "Superseded prepare result was ignored."
            }
          };
        }

        this.#logAITask("completed", {
          label: `prepare:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return {
          ...prepared,
          threadID,
          type,
          contentState: {
            status: "ready",
            message: `Prepared by ${this.aiProviderRuntime?.backendLabel ?? "AI backend"}.`
          }
        };
      } catch (error) {
        this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
          label: `prepare:${threadID}`,
          error: error?.message ?? String(error),
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return {
          threadID,
          type,
          title: `${capitalize(type)} unavailable`,
          openLoops: threadView.anchors.at(-1)?.openLoops ?? [],
          recommendedNextSteps: [],
          contentState: {
            status: "error",
            message: error.message
          }
        };
      } finally {
        if (this.draftPreparationTokens.get(threadID) === token) {
          this.draftPreparationProcessingThreadIDs.delete(threadID);
          this.draftPreparationTasks.delete(threadID);
          this.draftPreparationTokens.delete(threadID);
        }
      }
      }
    });

    this.draftPreparationTasks.set(threadID, handle);
    return handle.promise;
  }

  async openThreadWithAI(threadID) {
    const thread = this.openThread(threadID);
    if (!thread) {
      return null;
    }
    await this.inferDiscourseRelations({ threadID }).catch(() => null);
    await this.synthesizeThreadState({ threadID }).catch(() => null);
    return this.openThread(threadID);
  }

  #configureRepository(databasePath) {
    this.repository = this.repositoryFactory(path.resolve(databasePath));
    this.repository.rebuildKnowledgeIndexSync();
    this.snapshot = emptySnapshot();
  }

  #routingEngine() {
    return new ThreadRoutingEngine({
      threadsProvider: () => this.snapshot.threads,
      entriesProvider: (threadID) => this.snapshot.entries.filter((entry) => entry.threadID === threadID),
      claimsProvider: (threadID) => this.snapshot.claims.filter((claim) => claim.threadID === threadID),
      latestAnchorProvider: (threadID) =>
        this.snapshot.anchors
          .filter((anchor) => anchor.threadID === threadID)
          .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime())[0] ?? null,
      retrievalEngine: this.repository?.retrievalEngine ?? null
    });
  }

  #assertRepository() {
    if (!this.repository) {
      throw new Error("Workspace is not configured");
    }
  }

  async #refreshThreadAI(threadID) {
    if (!threadID || !this.aiService) {
      return;
    }
    await this.inferDiscourseRelations({ threadID }).catch(() => null);
    await this.synthesizeThreadState({ threadID }).catch(() => null);
  }

  #normalizeRoutingDecision(decision) {
    return decision?.type === "route"
      ? { type: "route", threadID: decision.threadID, reason: decision.reason ?? "", source: "heuristic" }
      : { type: "noMatch", reason: decision?.reason ?? "No confident thread match.", source: "heuristic" };
  }

  #threadContentFingerprint(threadView) {
    return JSON.stringify({
      threadID: threadView.thread.id,
      entries: (threadView.entries ?? []).map((entry) => ({
        id: entry.id,
        threadID: entry.threadID,
        kind: entry.kind,
        summaryText: entry.summaryText,
        updatedAt: entry.updatedAt ?? entry.createdAt
      })),
      claims: (threadView.claims ?? []).map((claim) => ({
        id: claim.id,
        statement: claim.statement,
        status: claim.status,
        updatedAt: claim.updatedAt
      })),
      anchors: (threadView.anchors ?? []).map((anchor) => ({
        id: anchor.id,
        stateSummary: anchor.stateSummary,
        openLoops: anchor.openLoops,
        createdAt: anchor.createdAt
      }))
    });
  }

  #entryAIActivityState() {
    return {
      routePlanningEntryIDs: this.routePlanningProcessingEntryIDs,
      threadRefreshingThreadIDs: new Set([
        ...this.resumeSynthesisProcessingThreadIDs,
        ...this.discourseInferenceProcessingThreadIDs
      ])
    };
  }

  #createTaskHandle({ token, label, execute }) {
    const controller = new AbortController();
    const handle = {
      token,
      label,
      startedAt: Date.now(),
      cancel: (reason = `${label} cancelled`) => {
        if (!controller.signal.aborted) {
          controller.abort(new Error(String(reason)));
        }
      },
      promise: null
    };
    handle.promise = Promise.resolve().then(() => execute({ signal: controller.signal }));
    return handle;
  }

  #cancelTaskHandle(handle, label, reason) {
    if (!handle?.cancel) {
      return;
    }
    this.#logAITask("cancelling", { label, reason });
    handle.cancel(reason);
  }

  #cancelAllAITasks(reason = "service reset") {
    for (const [entryID, handle] of this.routePlanningTasks) {
      this.#cancelTaskHandle(handle, `route:${entryID}`, reason);
    }
    for (const [threadID, handle] of this.resumeSynthesisTasks) {
      this.#cancelTaskHandle(handle, `resume:${threadID}`, reason);
    }
    for (const [threadID, handle] of this.draftPreparationTasks) {
      this.#cancelTaskHandle(handle, `prepare:${threadID}`, reason);
    }
    for (const [threadID, handle] of this.discourseInferenceTasks) {
      this.#cancelTaskHandle(handle, `discourse:${threadID}`, reason);
    }
    this.routePlanningTasks.clear();
    this.routePlanningTokens.clear();
    this.routePlanningProcessingEntryIDs.clear();
    this.resumeSynthesisTasks.clear();
    this.resumeSynthesisTokens.clear();
    this.resumeSynthesisProcessingThreadIDs.clear();
    this.draftPreparationTasks.clear();
    this.draftPreparationTokens.clear();
    this.draftPreparationProcessingThreadIDs.clear();
    this.discourseInferenceTasks.clear();
    this.discourseInferenceTokens.clear();
    this.discourseInferenceProcessingThreadIDs.clear();
  }

  #logAITask(event, details = {}) {
    console.error("[ai-task]", event, details);
  }
}

function capitalize(value) {
  const text = String(value ?? "");
  return text ? `${text[0].toUpperCase()}${text.slice(1)}` : "Prepare";
}

function isAbortError(error) {
  return error?.name === "AbortError" || error?.message === "This operation was aborted";
}

function collectEntryTree(entries, rootID) {
  const queue = [rootID];
  const found = new Set();
  while (queue.length > 0) {
    const current = queue.pop();
    if (!current || found.has(current)) {
      continue;
    }
    const match = entries.find((entry) => entry.id === current);
    if (!match) {
      continue;
    }
    found.add(current);
    for (const entry of entries) {
      if (entry.parentEntryID === current) {
        queue.push(entry.id);
      }
    }
  }
  return Array.from(found);
}

function emptySnapshot() {
  return {
    threads: [],
    entries: [],
    claims: [],
    anchors: [],
    discourseRelations: [],
    tasks: [],
    memoryRecords: [],
    aiSnapshots: []
  };
}

function resolveReferenceGraph(entries) {
  const entryByID = new Map();
  const resolvedEntries = (entries ?? []).map((entry) => {
    const clone = {
      ...entry,
      references: (entry.references ?? []).map((reference) => ({
        ...reference,
        relationKind: reference.relationKind ?? DEFAULT_REFERENCE_RELATION,
        targetKind: "unresolved",
        targetID: reference.targetID ?? null,
        targetThreadID: null,
        targetSummaryText: null,
        isResolved: false
      })),
      incomingBacklinks: []
    };
    entryByID.set(clone.id, clone);
    return clone;
  });

  for (const entry of resolvedEntries) {
    const sourceSummaryText = deriveReferenceTargetLabel(entry) || entry.kind || "note";
    entry.references = (entry.references ?? []).map((reference) => {
      const target = entryByID.get(reference.targetID) ?? null;
      if (!target) {
        return {
          ...reference,
          relationKind: reference.relationKind ?? DEFAULT_REFERENCE_RELATION
        };
      }

      target.incomingBacklinks.push({
        id: `${entry.id}:${target.id}:${reference.id}`,
        sourceEntryID: entry.id,
        sourceThreadID: entry.threadID ?? null,
        sourceSummaryText,
        relationKind: reference.relationKind ?? DEFAULT_REFERENCE_RELATION
      });

      return {
        ...reference,
        relationKind: reference.relationKind ?? DEFAULT_REFERENCE_RELATION,
        targetKind: target.kind ?? "note",
        targetID: target.id,
        targetThreadID: target.threadID ?? null,
        targetSummaryText: deriveReferenceTargetLabel(target),
        isResolved: true
      };
    });
  }

  for (const entry of resolvedEntries) {
    entry.incomingBacklinks.sort((lhs, rhs) => {
      const lhsEntry = entryByID.get(lhs.sourceEntryID);
      const rhsEntry = entryByID.get(rhs.sourceEntryID);
      return new Date(rhsEntry?.createdAt ?? 0).getTime() - new Date(lhsEntry?.createdAt ?? 0).getTime();
    });
  }

  return resolvedEntries;
}

function withEntryAIActivity(entries, {
  routePlanningEntryIDs = new Set(),
  threadRefreshingThreadIDs = new Set()
} = {}) {
  return (entries ?? []).map((entry) => {
    const aiActivity = deriveEntryAIActivity(entry, {
      routePlanningEntryIDs,
      threadRefreshingThreadIDs
    });
    return aiActivity ? { ...entry, aiActivity } : entry;
  });
}

function deriveEntryAIActivity(entry, { routePlanningEntryIDs, threadRefreshingThreadIDs }) {
  if (!entry) {
    return null;
  }
  if (!entry.threadID && routePlanningEntryIDs.has(entry.id)) {
    return {
      visible: true,
      kind: "routePlanning",
      label: "AI 正在判断归档位置"
    };
  }
  if (entry.threadID && threadRefreshingThreadIDs.has(entry.threadID)) {
    return {
      visible: true,
      kind: "threadRefreshing",
      label: "AI 正在整理线程"
    };
  }
  return null;
}

function createCaptureBackgroundTask({ entryID, threadID = null, useAIRouting = false, refreshThreadID = null } = {}) {
  if (!entryID) {
    return null;
  }
  if (!useAIRouting && !refreshThreadID) {
    return null;
  }
  return {
    entryID,
    useAIRouting,
    refreshThreadID: refreshThreadID ?? threadID ?? null
  };
}

function migrateLegacyReferenceEntries(entries, repository) {
  const entryList = entries ?? [];
  const knownEntryIDs = new Set(entryList.map((entry) => entry.id).filter(Boolean));
  let changed = false;

  for (const entry of entryList) {
    const migrated = migrateLegacyReferenceEntry(entry, knownEntryIDs);
    if (!migrated) {
      continue;
    }
    repository.store.upsertEntry(migrated);
    changed = true;
  }

  return changed;
}

function migrateLegacyReferenceEntry(entry, knownEntryIDs) {
  const bodyText = typeof entry?.body?.text === "string" ? entry.body.text : null;
  const summaryText = typeof entry?.summaryText === "string" ? entry.summaryText : "";
  const migratedBody = bodyText == null ? null : migrateLegacyReferenceText(bodyText, knownEntryIDs);
  const migratedSummary = migrateLegacyReferenceText(summaryText, knownEntryIDs);
  const explicitReferences = [
    ...(migratedBody?.bindings ?? []),
    ...migratedSummary.bindings
  ];
  const nextBodyText = migratedBody?.text ?? bodyText;
  const nextSummaryText = migratedSummary.text;
  const nextReferences = mergeReferenceBindings({
    parsedReferences: parseReferencesFromText(nextBodyText ?? nextSummaryText),
    explicitReferences,
    existingReferences: entry?.references ?? []
  });

  const bodyChanged = migratedBody?.changed ?? false;
  const summaryChanged = migratedSummary.changed;
  const referencesChanged = !referenceListsEqual(entry?.references ?? [], nextReferences);

  if (!bodyChanged && !summaryChanged && !referencesChanged) {
    return null;
  }

  return {
    ...entry,
    body:
      bodyText == null
        ? (entry.body ?? {})
        : {
            ...(entry.body ?? {}),
            text: nextBodyText
          },
    summaryText: nextSummaryText,
    references: nextReferences
  };
}

function migrateLegacyReferenceText(text, knownEntryIDs) {
  const source = String(text ?? "");
  const bindings = [];
  let changed = false;
  const next = source.replace(/\[\[([^\]]+)\]\]/g, (fullMatch, inner) => {
    const raw = String(inner ?? "").trim();
    const parts = raw.split("|").map((item) => item.trim());

    if (parts.length === 2 && knownEntryIDs.has(parts[0])) {
      changed = true;
      bindings.push({
        label: parts[1],
        relationKind: DEFAULT_REFERENCE_RELATION,
        targetID: parts[0]
      });
      return `[[${parts[1]}]]`;
    }

    if (
      parts.length === 3 &&
      EXPLICIT_REFERENCE_RELATIONS.includes(parts[0].toLowerCase()) &&
      knownEntryIDs.has(parts[1])
    ) {
      changed = true;
      bindings.push({
        label: parts[2],
        relationKind: parts[0].toLowerCase(),
        targetID: parts[1]
      });
      return `[[${parts[0].toLowerCase()}|${parts[2]}]]`;
    }

    return fullMatch;
  });

  return {
    text: next,
    bindings,
    changed
  };
}

function mergeReferenceBindings({ parsedReferences = [], explicitReferences = [], existingReferences = [] }) {
  const pendingExplicit = [...explicitReferences];
  const pendingExisting = [...existingReferences];

  return parsedReferences.map((reference) => {
    const explicitIndex = pendingExplicit.findIndex((candidate) => referenceSignature(candidate) === referenceSignature(reference));
    if (explicitIndex >= 0) {
      const [matched] = pendingExplicit.splice(explicitIndex, 1);
      return {
        ...reference,
        targetID: matched.targetID ?? null
      };
    }

    const existingIndex = pendingExisting.findIndex((candidate) => referenceSignature(candidate) === referenceSignature(reference));
    if (existingIndex >= 0) {
      const [matched] = pendingExisting.splice(existingIndex, 1);
      return {
        ...reference,
        targetID: matched.targetID ?? null
      };
    }

    return reference;
  });
}

function referenceSignature(reference) {
  return `${String(reference?.label ?? "").trim().toLowerCase()}::${String(reference?.relationKind ?? DEFAULT_REFERENCE_RELATION).trim().toLowerCase()}`;
}

function referenceListsEqual(left, right) {
  if (left.length !== right.length) {
    return false;
  }
  return left.every((reference, index) => {
    const candidate = right[index];
    return (
      reference?.id === candidate?.id &&
      reference?.label === candidate?.label &&
      reference?.relationKind === candidate?.relationKind &&
      (reference?.targetID ?? null) === (candidate?.targetID ?? null) &&
      (reference?.targetKind ?? "unresolved") === (candidate?.targetKind ?? "unresolved")
    );
  });
}
