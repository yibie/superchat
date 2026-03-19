import path from "node:path";
import { CaptureInterpreter } from "../../domain/capture/captureInterpreter.js";
import { ThreadRoutingEngine } from "../../domain/routing/threadRoutingEngine.js";
import { deriveResources, resourceCounts } from "../../domain/resources/resourceDerivation.js";
import {
  EntryKind,
  EntryStatus,
  normalizeEntryMode,
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
import { resolveEntrySourceDescriptor } from "../../domain/resources/richSourceDescriptor.js";

const THREAD_REFRESH_QUIET_MS = 1200;
const THREAD_RESUME_PENDING_THRESHOLD = 5;
const DEFAULT_PAGE_LIMIT = 50;
const DEFAULT_MEMORY_PREVIEW_LIMIT = 200;
const SETTLED_AI_STATUSES = new Set(["decided", "solved", "verified", "dropped", "stable"]);
const AI_CONTEXT_LIMITS = Object.freeze({
  maxQueries: 4,
  recallPerQuery: 8,
  semanticDocs: 6,
  episodicDocs: 4,
  sourceDocs: 4,
  rawDocs: 6,
  recentNotes: 6,
  keyEvidence: 5,
  judgmentBasis: 2
});

export class ThreadnoteApplicationService {
  constructor({
    workspaceManager,
    aiProviderRuntime = null,
    aiService = null,
    repositoryFactory = (databasePath) => new ThreadnoteRepository({ store: new SQLitePersistenceStore(databasePath) }),
    linkMetadataService = null,
    onAsyncStateChanged = null
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
    this.onAsyncStateChanged = onAsyncStateChanged;
    this.captureInterpreter = new CaptureInterpreter();
    this.discourseInferenceEngine = new DiscourseInferenceEngine();
    this.repository = null;
    this.snapshot = emptySnapshot();
    this.routeDebugByEntryID = new Map();
    this.threadAIStatusByThreadID = new Map();
    this.preparedViewByThreadID = new Map();
    this.routePlanningTasks = new Map();
    this.routePlanningTokens = new Map();
    this.routePlanningProcessingEntryIDs = new Set();
    this.entryClassificationTasks = new Map();
    this.entryClassificationTokens = new Map();
    this.entryClassificationProcessingEntryIDs = new Set();
    this.entryClassificationDebugByEntryID = new Map();
    this.entryClassificationTimers = new Map();
    this.entryStatusClassificationTasks = new Map();
    this.entryStatusClassificationTokens = new Map();
    this.entryStatusClassificationProcessingEntryIDs = new Set();
    this.entryStatusClassificationDebugByEntryID = new Map();
    this.entryStatusClassificationTimers = new Map();
    this.resumeSynthesisTasks = new Map();
    this.resumeSynthesisTokens = new Map();
    this.resumeSynthesisProcessingThreadIDs = new Set();
    this.draftPreparationTasks = new Map();
    this.draftPreparationTokens = new Map();
    this.draftPreparationProcessingThreadIDs = new Set();
    this.discourseInferenceTasks = new Map();
    this.discourseInferenceTokens = new Map();
    this.discourseInferenceProcessingThreadIDs = new Set();
    this.invalidatedEntryIDs = new Set();
    this.invalidatedThreadIDs = new Set();
    this.invalidatedThreadUpdatedAt = new Map();
    this.invalidatedThreadNeedsDiscourse = new Map();
    this.threadResumePendingCount = new Map();
    this.backgroundSweepTimer = null;
    this.backgroundSweepRunning = false;
  }

  createWorkspace(workspacePath) {
    const workspace = this.workspaceManager.createWorkspace(workspacePath);
    this.#configureRepository(workspace.databasePath);
    this.repairWorkspaceRelations();
    this.repository.rebuildKnowledgeIndexSync();
    return this.loadWorkspace();
  }

  openWorkspace(workspacePath) {
    const workspace = this.workspaceManager.openWorkspace(workspacePath);
    this.#configureRepository(workspace.databasePath);
    this.repairWorkspaceRelations();
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
    this.repairWorkspaceRelations();
    return this.loadWorkspace();
  }

  loadWorkspace() {
    this.#assertRepository();
    if (this.repository.store.hasLegacyRepairCandidates()) {
      this.repairWorkspaceRelations();
    }
    return this.#refreshWorkspaceState();
  }

  #refreshWorkspaceState() {
    this.snapshot = this.repository.loadSnapshot();
    return {
      workspace: this.workspaceManager.describe(),
      home: this.homeView()
    };
  }

  repairWorkspaceRelations() {
    this.#assertRepository();
    const entries = this.repository.store.fetchLegacyRepairCandidates();
    const knownEntryIDs = new Set(this.repository.store.fetchEntryIDs());
    const referenceEntriesMigrated = migrateLegacyReferenceEntries(entries, this.repository, knownEntryIDs);
    const replyRepair = migrateLegacyReplyEntries(entries, this.repository);
    const changed = Boolean(
      referenceEntriesMigrated ||
      replyRepair.migratedReplyCount > 0 ||
      replyRepair.clearedParentCount > 0
    );

    this.snapshot = this.repository.loadSnapshot();

    return {
      changed,
      referenceEntriesMigrated,
      migratedReplyCount: replyRepair.migratedReplyCount,
      clearedParentCount: replyRepair.clearedParentCount
    };
  }

  homeView() {
    const resolvedEntries = this.#resolvedEntries(this.snapshot.entries ?? []);
    const threadEntryCounts = this.repository?.store?.fetchThreadEntryCounts?.() ?? new Map();
    return {
      threads: this.#sortedActiveThreads(threadEntryCounts),
      inboxEntries: resolvedEntries
        .slice()
        .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime()),
      allEntries: resolvedEntries,
      aiState: {
        routeDebugByEntryID: Object.fromEntries(this.routeDebugByEntryID),
        entryClassificationDebugByEntryID: Object.fromEntries(this.entryClassificationDebugByEntryID),
        entryStatusClassificationDebugByEntryID: Object.fromEntries(this.entryStatusClassificationDebugByEntryID),
        queue: this.#queueDebugState(),
        activeOperations: this.#activeOperationLabels()
      },
      resources: deriveResources(resolvedEntries),
      resourceCounts: resourceCounts(deriveResources(resolvedEntries))
    };
  }

  homeSurfaceView({ limit = DEFAULT_PAGE_LIMIT } = {}) {
    this.#assertRepository();
    const resolvedEntries = this.#resolvedEntries(this.snapshot.entries ?? []);
    const threadEntryCounts = this.repository?.store?.fetchThreadEntryCounts?.() ?? new Map();
    const resources = deriveResources(resolvedEntries);
    return {
      threads: this.#sortedActiveThreads(threadEntryCounts),
      streamPage: this.streamPage({ limit }),
      aiState: {
        routeDebugByEntryID: Object.fromEntries(this.routeDebugByEntryID),
        entryClassificationDebugByEntryID: Object.fromEntries(this.entryClassificationDebugByEntryID),
        entryStatusClassificationDebugByEntryID: Object.fromEntries(this.entryStatusClassificationDebugByEntryID),
        queue: this.#queueDebugState(),
        activeOperations: this.#activeOperationLabels()
      },
      resources,
      resourceCounts: resourceCounts(resources)
    };
  }

  streamPage({ cursor = null, limit = DEFAULT_PAGE_LIMIT } = {}) {
    this.#assertRepository();
    return this.#buildEntryPage({
      cursor,
      limit,
      threadID: null
    });
  }

  openThreadSurface(threadID, { cursor = null, limit = DEFAULT_PAGE_LIMIT } = {}) {
    this.#assertRepository();
    const thread = this.#openThreadAggregate(threadID, { entryLimit: 0 });
    if (!thread) {
      return null;
    }

    const entriesPage = this.#buildEntryPage({
      cursor,
      limit,
      threadID
    });
    const loadedEntries = entriesPage.items;

    return {
      ...thread,
      entries: loadedEntries,
      entriesPage
    };
  }

  async createThread({ title, prompt = title, goalLayer = null }) {
    this.#assertRepository();
    const existingCount = this.snapshot?.threads?.length ?? 0;
    const color = ThreadColorValues[existingCount % ThreadColorValues.length];
    const thread = createThreadRecord({ title, prompt, goalLayer, color });
    await this.repository.saveThread(thread);
    await this.repository.flush();
    return this.#refreshWorkspaceState().home.threads.find((item) => item.id === thread.id) ?? thread;
  }

  async updateThreadTitle({ threadID, title }) {
    this.#assertRepository();
    const existing = this.snapshot.threads.find((thread) => thread.id === threadID) ?? null;
    if (!existing) {
      throw new Error(`Unknown thread: ${threadID}`);
    }

    const normalizedTitle = String(title ?? "").trim();
    if (!normalizedTitle) {
      throw new Error("Thread title cannot be empty");
    }

    const next = {
      ...existing,
      title: normalizedTitle,
      prompt: existing.prompt === existing.title ? normalizedTitle : existing.prompt,
      updatedAt: new Date(),
      lastActiveAt: existing.lastActiveAt ?? new Date()
    };

    await this.repository.saveThread(next);
    await this.repository.flush();
    this.#refreshWorkspaceState();
    return this.openThread(threadID);
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
      status: EntryStatus.OPEN,
      summaryText: interpretation.normalizedText,
      sourceMetadata: mergeSourceMetadata(null, interpretation, {
        source: interpretation.detectedItemSource,
        confidence: interpretation.confidenceScore
      }),
      statusMetadata: createStatusMetadata({ source: "heuristic", status: EntryStatus.OPEN, confidence: 1 }),
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
    this.#refreshWorkspaceState();
    if (useAIRouting) {
      this.#setRouteDebug(entry.id, createRouteDebugState({
        entryID: entry.id,
        status: "processing",
        message: "AI 正在判断归档位置",
        source: "ai",
        startedAt: new Date().toISOString()
      }));
      this.invalidatedEntryIDs.add(entry.id);
      this.scheduleSweep({ delay: 150, reason: `submitCapture:${entry.id}` });
    }
    this.#scheduleEntryClassificationIfEligible(entry.id, { reason: "submitCapture", delay: 80 });
    this.#scheduleEntryStatusClassificationIfEligible(entry.id, { reason: "submitCapture", delay: 120 });
    if (threadID && this.aiService) {
      this.#incrementThreadResumePendingCount(threadID);
      this.invalidateAIOutputState(`thread capture:${threadID}`, { threadIDs: [threadID] });
      this.scheduleSweep({ delay: 150, reason: `submitCapture-thread:${threadID}` });
    }

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

    const savedEntry = this.#fetchEntry(result.entry.id) ?? result.entry;
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
    this.#refreshWorkspaceState();

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

    this.#refreshWorkspaceState();

    return {
      routingDecision,
      threadID,
      thread: threadID ? this.openThread(threadID) : null
    };
  }

  invalidateAIOutputState(reason = "invalidated", { entryIDs = [], threadIDs = [], discourseThreadIDs = [], clearAll = false } = {}) {
    if (clearAll) {
      this.#cancelAllAITasks(reason);
      this.routeDebugByEntryID.clear();
      this.entryClassificationDebugByEntryID.clear();
      this.entryStatusClassificationDebugByEntryID.clear();
      this.threadAIStatusByThreadID.clear();
      this.preparedViewByThreadID.clear();
      this.invalidatedEntryIDs.clear();
      this.invalidatedThreadIDs.clear();
      this.invalidatedThreadUpdatedAt.clear();
      this.invalidatedThreadNeedsDiscourse.clear();
      this.threadResumePendingCount.clear();
      this.#notifyAsyncStateChanged();
      return;
    }

    const discourseThreadIDSet = new Set(discourseThreadIDs.filter(Boolean));

    for (const entryID of entryIDs) {
      if (!entryID) {
        continue;
      }
      this.#cancelTaskHandle(this.routePlanningTasks.get(entryID), `route:${entryID}`, reason);
      this.routePlanningTasks.delete(entryID);
      this.routePlanningTokens.delete(entryID);
      this.routePlanningProcessingEntryIDs.delete(entryID);
      this.routeDebugByEntryID.delete(entryID);
      this.#clearEntryClassificationState(entryID, reason);
      this.#clearEntryStatusClassificationState(entryID, reason);
      this.invalidatedEntryIDs.add(entryID);
    }

    for (const threadID of threadIDs) {
      if (!threadID) {
        continue;
      }
      this.#cancelTaskHandle(this.resumeSynthesisTasks.get(threadID), `resume:${threadID}`, reason);
      this.#cancelTaskHandle(this.draftPreparationTasks.get(threadID), `prepare:${threadID}`, reason);
      this.#cancelTaskHandle(this.discourseInferenceTasks.get(threadID), `discourse:${threadID}`, reason);
      this.resumeSynthesisTasks.delete(threadID);
      this.resumeSynthesisTokens.delete(threadID);
      this.resumeSynthesisProcessingThreadIDs.delete(threadID);
      this.draftPreparationTasks.delete(threadID);
      this.draftPreparationTokens.delete(threadID);
      this.draftPreparationProcessingThreadIDs.delete(threadID);
      this.discourseInferenceTasks.delete(threadID);
      this.discourseInferenceTokens.delete(threadID);
      this.discourseInferenceProcessingThreadIDs.delete(threadID);
      this.threadAIStatusByThreadID.delete(threadID);
      this.preparedViewByThreadID.delete(threadID);
      this.invalidatedThreadIDs.add(threadID);
      this.invalidatedThreadUpdatedAt.set(threadID, Date.now());
      this.invalidatedThreadNeedsDiscourse.set(
        threadID,
        Boolean(this.invalidatedThreadNeedsDiscourse.get(threadID)) || discourseThreadIDSet.has(threadID)
      );
    }

    this.#notifyAsyncStateChanged(threadIDs[0] ?? null);
  }

  #incrementThreadResumePendingCount(threadID, delta = 1) {
    if (!threadID || delta <= 0) {
      return;
    }
    this.threadResumePendingCount.set(threadID, (this.threadResumePendingCount.get(threadID) ?? 0) + delta);
  }

  #clearThreadRefreshState(threadID) {
    if (!threadID) {
      return;
    }
    this.invalidatedThreadIDs.delete(threadID);
    this.invalidatedThreadUpdatedAt.delete(threadID);
    this.invalidatedThreadNeedsDiscourse.delete(threadID);
  }

  scheduleSweep({ delay = 250, reason = "scheduled" } = {}) {
    if (!this.aiService || !this.repository) {
      return;
    }
    if (this.backgroundSweepTimer) {
      clearTimeout(this.backgroundSweepTimer);
    }
    this.backgroundSweepTimer = setTimeout(() => {
      this.backgroundSweepTimer = null;
      void this.#runBackgroundSweep(reason);
    }, Math.max(0, Number(delay) || 0));
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
    return this.#openThreadAggregate(threadID);
  }

  #openThreadAggregate(threadID, { entryLimit = DEFAULT_PAGE_LIMIT, includeMemory = true } = {}) {
    const thread = this.snapshot.threads.find((item) => item.id === threadID) ?? null;
    if (!thread) {
      return null;
    }
    const claims = this.repository.store.fetchClaims(threadID);
    const anchors = this.repository.store.fetchAnchors(threadID);
    const tasks = this.repository.store.fetchTasks(threadID);
    const discourseRelations = this.repository.store.fetchDiscourseRelations(threadID);
    const memory = includeMemory
      ? this.repository.fetchMemoryPreview(threadID, { limit: DEFAULT_MEMORY_PREVIEW_LIMIT })
      : [];
    const memoryCount = includeMemory ? this.repository.countMemory(threadID) : 0;
    const aiSnapshot = this.snapshot.aiSnapshots.find((snapshot) => snapshot.threadID === threadID) ?? null;
    const aiStatus = this.#getThreadAIStatus(threadID, { hasSnapshot: Boolean(aiSnapshot) });
    const preparedView = this.preparedViewByThreadID.get(threadID) ?? null;
    const statusSummary = this.repository.store.fetchThreadStatusSummary(threadID);
    const counts = this.repository.store.fetchThreadCounts(threadID);
    const entries = entryLimit > 0 ? this.#fetchResolvedThreadEntries(threadID, { limit: entryLimit }) : [];
    const resources = deriveResources(entries);
    return {
      thread,
      entries,
      claims,
      anchors,
      tasks,
      discourseRelations,
      memory,
      memoryCount,
      aiSnapshot,
      aiStatus,
      statusSummary,
      preparedView,
      aiDebug: {
        queue: this.#queueDebugState(),
        activeOperations: this.#activeOperationLabels({ threadID })
      },
      resources,
      resourceCounts: resourceCounts(resources),
      counts
    };
  }

  #openThreadAIAggregate(threadID) {
    return this.#openThreadAggregate(threadID, {
      entryLimit: 0,
      includeMemory: false
    });
  }

  #resolvedEntries(entries = this.#allEntries()) {
    return withEntryAIActivity(
      resolveReferenceGraph(entries),
      this.#entryAIActivityState()
    );
  }

  #allEntries() {
    return this.repository?.store?.fetchEntries?.() ?? [];
  }

  #fetchEntry(entryID) {
    return entryID ? (this.repository?.store?.fetchEntry?.(entryID) ?? null) : null;
  }

  #fetchEntriesByIDs(entryIDs = []) {
    return this.repository?.store?.fetchEntriesByIDs?.(entryIDs) ?? [];
  }

  #fetchResolvedThreadEntries(threadID, { limit = DEFAULT_PAGE_LIMIT } = {}) {
    const descending = this.repository.store.fetchEntryPage({
      threadID,
      limit: Math.max(1, Number(limit) || 1),
      topLevelOnly: false
    });
    const entries = descending.slice().reverse();
    return this.#resolvedEntries(entries);
  }

  #sortedActiveThreads(threadEntryCounts = new Map()) {
    return this.snapshot.threads
      .filter((thread) => thread.status !== ThreadStatus.ARCHIVED)
      .slice()
      .sort((lhs, rhs) => new Date(rhs.lastActiveAt).getTime() - new Date(lhs.lastActiveAt).getTime())
      .map((thread) => ({ ...thread, entryCount: threadEntryCounts.get(thread.id) ?? 0 }));
  }

  #buildEntryPage({ threadID = null, cursor = null, limit = DEFAULT_PAGE_LIMIT } = {}) {
    const pageSize = Math.max(1, Number(limit) || 1);
    const pagedItems = this.repository.store.fetchEntryPage({
      threadID,
      cursor,
      limit: pageSize + 1,
      topLevelOnly: false
    });
    const hasMore = pagedItems.length > pageSize;
    const pageItems = hasMore ? pagedItems.slice(0, pageSize) : pagedItems;
    const resolvedEntries = this.#resolvedEntries(this.#fetchEntriesByIDs(pageItems.map((entry) => entry.id)));
    const resolvedByID = new Map(resolvedEntries.map((entry) => [entry.id, entry]));
    const items = pageItems.map((entry) => resolvedByID.get(entry.id) ?? entry);
    const totalCount = this.repository.store.countEntries({ threadID, topLevelOnly: false });
    const lastItem = pageItems.at(-1) ?? null;

    return {
      items,
      replies: [],
      totalCount,
      hasMore,
      nextCursor: lastItem ? { createdAt: lastItem.createdAt, id: lastItem.id } : null
    };
  }

  async appendReply({ entryID, text, attachments = [], references = [] }) {
    this.#assertRepository();
    const parent = this.#fetchEntry(entryID);
    if (!parent) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const normalizedText = String(text ?? "").trim();
    if (!normalizedText) {
      throw new Error("Reply cannot be empty");
    }
    const interpretation = this.captureInterpreter.interpretText(normalizedText);
    const nextBody = buildEntryBody({
      existingBody: null,
      text: interpretation.normalizedText,
      attachments
    });
    const explicitReferences = Array.isArray(references) ? references : [];
    const replyLabel = deriveReferenceTargetLabel(parent);
    const reply = createEntry({
      threadID: parent.threadID ?? null,
      kind: interpretation.detectedItemType,
      status: EntryStatus.OPEN,
      summaryText: interpretation.normalizedText,
      sourceMetadata: mergeSourceMetadata(null, interpretation, {
        source: interpretation.detectedItemSource,
        confidence: interpretation.confidenceScore
      }),
      statusMetadata: createStatusMetadata({ source: "heuristic", status: EntryStatus.OPEN, confidence: 1 }),
      objectMentions: interpretation.detectedObjects,
      references: mergeReferenceBindings({
        parsedReferences: parseReferencesFromText(interpretation.normalizedText),
        explicitReferences: [
          createDefaultReplyReference(parent, { label: replyLabel }),
          ...explicitReferences
        ]
      }),
      confidenceScore: interpretation.confidenceScore,
      inboxState: parent.threadID ? "resolved" : "unresolved",
      ...(nextBody ? { body: nextBody } : {})
    });
    await this.repository.saveEntry(reply);
    await this.repository.flush();
    this.#refreshWorkspaceState();
    if (parent.threadID) {
      this.#incrementThreadResumePendingCount(parent.threadID);
      this.invalidateAIOutputState(`appendReply:${parent.threadID}`, { threadIDs: [parent.threadID] });
      this.scheduleSweep({ delay: 150, reason: `appendReply:${parent.threadID}` });
    }
    this.#scheduleEntryClassificationIfEligible(reply.id, { reason: "appendReply", delay: 80 });
    this.#scheduleEntryStatusClassificationIfEligible(reply.id, { reason: "appendReply", delay: 120 });
    return {
      entry: reply
    };
  }

  async updateEntryText({ entryID, text, attachments, references = [] }) {
    this.#assertRepository();
    const existing = this.#fetchEntry(entryID);
    if (!existing) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const normalizedText = String(text ?? "").trim();
    if (!normalizedText) {
      throw new Error("Entry text cannot be empty");
    }
    const interpretation = this.captureInterpreter.interpretText(normalizedText);
    const preservedKind = shouldPreserveUserKind(existing)
      ? existing.kind
      : interpretation.detectedItemType;
    const preservedStatus = shouldPreserveUserStatus(existing)
      ? existing.status
      : EntryStatus.OPEN;
    const nextReferences = mergeReferenceBindings({
      parsedReferences: parseReferencesFromText(interpretation.normalizedText),
      explicitReferences: references,
      existingReferences: existing.references ?? []
    });
    const nextBody = buildEntryBody({
      existingBody: existing.body ?? null,
      text: interpretation.normalizedText,
      attachments
    });
    const updated = {
      ...existing,
      kind: preservedKind,
      status: preservedStatus,
      ...(nextBody ? { body: nextBody } : {}),
      summaryText: interpretation.normalizedText,
      sourceMetadata: mergeSourceMetadata(existing.sourceMetadata ?? null, interpretation, {
        source: shouldPreserveUserKind(existing) ? existing.sourceMetadata?.kindAttribution?.source ?? "manual" : interpretation.detectedItemSource,
        confidence: shouldPreserveUserKind(existing) ? existing.sourceMetadata?.kindAttribution?.confidence ?? null : interpretation.confidenceScore
      }),
      statusMetadata: shouldPreserveUserStatus(existing)
        ? existing.statusMetadata ?? createStatusMetadata({ source: "manual", status: existing.status ?? EntryStatus.OPEN, confidence: 1 })
        : createStatusMetadata({ source: "heuristic", status: EntryStatus.OPEN, confidence: 1 }),
      objectMentions: interpretation.detectedObjects,
      references: nextReferences,
      confidenceScore: interpretation.confidenceScore
    };
    await this.repository.saveEntry(updated);
    await this.repository.flush();
    this.#refreshWorkspaceState();
    if (updated.threadID) {
      const needsDiscourse = !referenceListsEqual(existing.references ?? [], nextReferences);
      this.invalidateAIOutputState(`updateEntry:${updated.threadID}`, {
        threadIDs: [updated.threadID],
        discourseThreadIDs: needsDiscourse ? [updated.threadID] : []
      });
      this.scheduleSweep({ delay: 150, reason: `updateEntry:${updated.threadID}` });
    } else if (this.aiService) {
      this.invalidateAIOutputState(`updateInboxEntry:${updated.id}`, { entryIDs: [updated.id] });
      this.#setRouteDebug(updated.id, createRouteDebugState({
        entryID: updated.id,
        status: "processing",
        message: "AI 正在判断归档位置",
        source: "ai",
        startedAt: new Date().toISOString()
      }));
      this.scheduleSweep({ delay: 150, reason: `updateInboxEntry:${updated.id}` });
    }
    this.#scheduleEntryClassificationIfEligible(updated.id, { reason: "updateEntryText", delay: 80 });
    this.#scheduleEntryStatusClassificationIfEligible(updated.id, { reason: "updateEntryText", delay: 120 });
    return {
      entry: updated
    };
  }

  async updateEntryKind({ entryID, kind, source = "manual" } = {}) {
    this.#assertRepository();
    const existing = this.#fetchEntry(entryID);
    if (!existing) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const nextKind = normalizeEntryKind(kind);
    const updatedAt = new Date().toISOString();
    const updated = {
      ...existing,
      kind: nextKind,
      sourceMetadata: mergeManualKindMetadata(existing.sourceMetadata ?? null, {
        kind: nextKind,
        source,
        updatedAt
      })
    };
    await this.repository.saveEntry(updated);
    await this.repository.flush();
    this.#refreshWorkspaceState();

    if (updated.threadID) {
      this.invalidateAIOutputState(`updateEntryKind:${updated.threadID}`, {
        threadIDs: [updated.threadID]
      });
      this.scheduleSweep({ delay: 150, reason: `updateEntryKind:${updated.threadID}` });
    } else if (this.aiService) {
      this.invalidateAIOutputState(`updateInboxEntryKind:${updated.id}`, { entryIDs: [updated.id] });
      this.#setRouteDebug(updated.id, createRouteDebugState({
        entryID: updated.id,
        status: "processing",
        message: "AI 正在判断归档位置",
        source: source === "manual" ? "manual" : "ai",
        startedAt: updatedAt
      }));
      this.scheduleSweep({ delay: 150, reason: `updateInboxEntryKind:${updated.id}` });
    }

    if (nextKind === EntryKind.NOTE) {
      this.#scheduleEntryClassificationIfEligible(updated.id, { reason: "updateEntryKind", delay: 80 });
    }
    this.#scheduleEntryStatusClassificationIfEligible(updated.id, { reason: "updateEntryKind", delay: 120 });

    return {
      entry: updated
    };
  }

  async updateEntryStatus({ entryID, status, source = "manual" } = {}) {
    this.#assertRepository();
    const existing = this.#fetchEntry(entryID);
    if (!existing) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const nextStatus = normalizeEntryStatus(status);
    const updatedAt = new Date().toISOString();
    const updated = {
      ...existing,
      status: nextStatus,
      statusMetadata: mergeManualStatusMetadata(existing.statusMetadata ?? null, {
        status: nextStatus,
        source,
        updatedAt
      })
    };
    await this.repository.saveEntry(updated);
    await this.repository.flush();
    this.#refreshWorkspaceState();

    if (updated.threadID) {
      this.invalidateAIOutputState(`updateEntryStatus:${updated.threadID}`, {
        threadIDs: [updated.threadID]
      });
      this.scheduleSweep({ delay: 150, reason: `updateEntryStatus:${updated.threadID}` });
    }

    if (nextStatus === EntryStatus.OPEN) {
      this.#scheduleEntryStatusClassificationIfEligible(updated.id, { reason: "updateEntryStatus", delay: 120 });
    }

    return {
      entry: updated
    };
  }

  async deleteEntry(entryID) {
    this.#assertRepository();
    const allEntries = this.#allEntries();
    const ids = collectEntryTree(allEntries, entryID);
    if (ids.length === 0) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const threadID = allEntries.find((entry) => ids.includes(entry.id))?.threadID ?? null;
    for (const id of ids) {
      this.#clearEntryClassificationState(id, "deleteEntry");
      this.#clearEntryStatusClassificationState(id, "deleteEntry");
    }
    await this.repository.deleteEntries(ids);
    await this.repository.flush();
    this.#refreshWorkspaceState();
    if (threadID) {
      this.invalidateAIOutputState(`deleteEntry:${threadID}`, {
        threadIDs: [threadID],
        discourseThreadIDs: [threadID]
      });
      this.scheduleSweep({ delay: 150, reason: `deleteEntry:${threadID}` });
    }
    return true;
  }

  async routeEntryToThread({ entryID, threadID }) {
    this.#assertRepository();
    const thread = this.snapshot.threads.find((item) => item.id === threadID) ?? null;
    if (!thread) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    const allEntries = this.#allEntries();
    const ids = collectEntryTree(allEntries, entryID);
    if (ids.length === 0) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const sourceThreadID = allEntries.find((entry) => entry.id === entryID)?.threadID ?? null;
    for (const id of ids) {
      const existing = allEntries.find((entry) => entry.id === id);
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
    this.#refreshWorkspaceState();
    this.invalidateAIOutputState(`routeEntryToThread:${threadID}`, {
      threadIDs: [threadID],
      discourseThreadIDs: [threadID],
      entryIDs: ids
    });
    for (const id of ids) {
      this.#setRouteDebug(id, createRouteDebugState({
        entryID: id,
        status: "routed",
        message: "Entry routed to thread.",
        selectedThreadID: threadID,
        source: "manual",
        updatedAt: new Date().toISOString()
      }));
      this.#scheduleEntryStatusClassificationIfEligible(id, { reason: "routeEntryToThread", delay: 120 });
    }
    this.scheduleSweep({ delay: 150, reason: `routeEntryToThread:${threadID}` });
    return {
      ...this.openThread(threadID),
      routeResult: {
        movedEntryIDs: ids,
        sourceThreadID,
        targetThreadID: threadID,
        entry: this.#fetchEntry(entryID)
      }
    };
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
    this.#clearThreadRefreshState(threadID);
    this.threadResumePendingCount.delete(threadID);
    return this.#refreshWorkspaceState();
  }

  resourcesView({ threadID = null } = {}) {
    const entries = threadID ? this.repository.store.fetchEntries(threadID) : this.#allEntries();
    const resources = deriveResources(entries);
    return {
      threadID,
      resources,
      counts: resourceCounts(resources)
    };
  }

  async getEntryRichPreview(entryID) {
    this.#assertRepository();
    const entry = this.#fetchEntry(entryID);
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

  async getLocatorRichPreview(locator) {
    const descriptor = resolveEntrySourceDescriptor({ summaryText: locator });
    if (!descriptor) {
      return null;
    }
    const preview = await this.linkMetadataService?.getRichPreviewForDescriptor(descriptor);
    if (!preview) {
      return null;
    }
    return {
      locator,
      ...preview
    };
  }

  async configureAIProvider(config) {
    if (!this.aiProviderRuntime) {
      throw new Error("AI provider runtime is unavailable");
    }
    this.invalidateAIOutputState("provider reconfigured", { clearAll: true });
    const saved = this.aiProviderRuntime.configure(config);
    this.aiService?.requestQueue?.setMaxConcurrent?.(this.aiProviderRuntime.preferredMaxConcurrentRequests || 2);
    this.#refreshWorkspaceState();
    this.scheduleSweep({ delay: 250, reason: "provider reconfigured" });
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
    const entry = this.#fetchEntry(entryID);
    if (!entry) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const interpretation = this.captureInterpreter.interpretEntry(entry);
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
      this.#setRouteDebug(entryID, createRouteDebugState({
        entryID,
        status: "processing",
        message: "AI 正在判断归档位置",
        source: "ai",
        startedAt: new Date().toISOString()
      }));
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
          this.#setRouteDebug(entryID, createRouteDebugState({
            entryID,
            status: "stale",
            message: "Superseded route result was ignored.",
            source: "ai",
            updatedAt: new Date().toISOString()
          }));
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
          const latest = this.#fetchEntry(entryID);
          if (latest) {
            await this.repository.saveEntry({
              ...latest,
              threadID: result.selectedThreadID,
              inboxState: "resolved"
            });
            await this.repository.flush();
            this.#refreshWorkspaceState();
            this.#scheduleEntryStatusClassificationIfEligible(entryID, {
              reason: "planRouteForEntry",
              delay: 120
            });
          }
        }

        this.#logAITask("completed", {
          label: `route:${entryID}`,
          decision: routingDecision.type,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        this.#setRouteDebug(entryID, createRouteDebugState({
          entryID,
          status: routingDecision.type === "route" ? "routed" : "inbox",
          message: routingDecision.reason,
          decisionReason: routingDecision.reason,
          selectedThreadID: routingDecision.threadID ?? null,
          source: routingDecision.source ?? "ai",
          debugPayload: result.debugPayload ?? null,
          updatedAt: new Date().toISOString()
        }));
        return routingDecision;
      } catch (error) {
        const errorKind = classifyAIError(error);
        this.#setRouteDebug(entryID, createRouteDebugState({
          entryID,
          status: errorKind === "abort" ? "cancelled" : "failed",
          message: error?.message ?? "Route planning failed.",
          errorKind,
          rawErrorMessage: error?.message ?? String(error),
          source: "ai",
          startedAt: this.routeDebugByEntryID.get(entryID)?.startedAt ?? new Date().toISOString(),
          updatedAt: new Date().toISOString()
        }));
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

  async classifyEntryKind({ entryID } = {}) {
    this.#assertRepository();
    const entry = this.#fetchEntry(entryID);
    if (!entry) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const eligibility = this.#entryClassificationEligibility(entry);
    if (!eligibility.allowed) {
      this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
        entryID,
        status: eligibility.status,
        message: eligibility.reason,
        currentKind: entry.kind,
        suggestedKind: entry.kind,
        updatedAt: new Date().toISOString()
      }));
      return {
        kind: entry.kind,
        skipped: true,
        reason: eligibility.reason
      };
    }
    if (!this.aiService || typeof this.aiService.classifyEntryKind !== "function") {
      this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
        entryID,
        status: "notConfigured",
        message: "AI backend is not configured.",
        currentKind: entry.kind,
        suggestedKind: entry.kind,
        updatedAt: new Date().toISOString()
      }));
      return {
        kind: entry.kind,
        skipped: true,
        reason: "AI backend is not configured."
      };
    }

    const interpretation = this.captureInterpreter.interpretText(entry.summaryText ?? entry.body?.text ?? "");
    const token = `${Date.now()}-${Math.random()}`;
    this.#clearEntryClassificationState(entryID, "superseded classification");
    this.entryClassificationTokens.set(entryID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `classify:${entryID}`,
      execute: async ({ signal }) => {
        const startedAt = new Date().toISOString();
        this.entryClassificationProcessingEntryIDs.add(entryID);
        this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
          entryID,
          status: "processing",
        message: "AI 正在判断笔记模式",
          currentKind: entry.kind,
          startedAt,
          updatedAt: startedAt
        }));
        this.#logAITask("started", {
          label: `classify:${entryID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        try {
          const result = await this.aiService.classifyEntryKind({
            entryID,
            normalizedText: interpretation.normalizedText,
            detectedItemType: interpretation.detectedItemType,
            detectedObjects: interpretation.detectedObjects,
            candidateClaims: interpretation.candidateClaims
          }, { signal });

          if (this.entryClassificationTokens.get(entryID) !== token) {
            this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
              entryID,
              status: "stale",
              message: "Superseded classification result was ignored.",
              currentKind: entry.kind,
              suggestedKind: result.kind,
              confidence: result.confidence,
              updatedAt: new Date().toISOString()
            }));
            return { kind: entry.kind, skipped: true, reason: "Superseded classification result." };
          }

          const latest = this.#fetchEntry(entryID);
          const latestEligibility = this.#entryClassificationEligibility(latest);
          if (!latest || !latestEligibility.allowed || !this.#shouldApplyEntryClassification(result, latest)) {
            this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
              entryID,
              status: latest ? latestEligibility.status : "stale",
              message: latest
                ? latestEligibility.allowed
                  ? result.reason || "Classification result did not meet overwrite rules."
                  : latestEligibility.reason
                : "Entry disappeared before classification could be applied.",
              currentKind: latest?.kind ?? entry.kind,
              suggestedKind: result.kind,
              confidence: result.confidence,
              debugPayload: result.debugPayload ?? null,
              startedAt,
              updatedAt: new Date().toISOString()
            }));
            return {
              kind: latest?.kind ?? entry.kind,
              skipped: true,
              reason: latestEligibility.reason || result.reason
            };
          }

          if (result.kind !== latest.kind) {
            await this.repository.saveEntry({
              ...latest,
              kind: result.kind,
              sourceMetadata: mergeKindAttributionMetadata(latest.sourceMetadata ?? null, {
                source: "ai",
                confidence: result.confidence,
                updatedAt: new Date().toISOString()
              })
            });
            await this.repository.flush();
            this.#refreshWorkspaceState();
            if (latest.threadID) {
              this.invalidateAIOutputState(`classifyEntry:${latest.threadID}`, {
                threadIDs: [latest.threadID]
              });
              this.scheduleSweep({ delay: 150, reason: `classifyEntry-thread:${latest.threadID}` });
            } else {
              this.invalidateAIOutputState(`classifyEntry:${entryID}`, { entryIDs: [entryID] });
              this.scheduleSweep({ delay: 150, reason: `classifyEntry:${entryID}` });
            }
          }

          this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
            entryID,
            status: result.kind === latest.kind ? "idle" : "ready",
            message: result.reason || (result.kind === latest.kind ? "Mode remains unchanged." : "Entry mode updated."),
            currentKind: latest.kind,
            suggestedKind: result.kind,
            confidence: result.confidence,
            debugPayload: result.debugPayload ?? null,
            startedAt,
            updatedAt: new Date().toISOString()
          }));
          this.#logAITask("completed", {
            label: `classify:${entryID}`,
            kind: result.kind,
            activeCount: this.aiService?.requestQueue?.activeCount ?? null,
            queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
          });
          return result;
        } catch (error) {
          if (this.entryClassificationTokens.get(entryID) !== token) {
            throw error;
          }
          const errorKind = classifyAIError(error);
          this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
            entryID,
            status: errorKind === "abort" ? "cancelled" : "failed",
            message: error?.message ?? "Entry classification failed.",
            errorKind,
            rawErrorMessage: error?.message ?? String(error),
            currentKind: entry.kind,
            startedAt,
            updatedAt: new Date().toISOString()
          }));
          this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
            label: `classify:${entryID}`,
            error: error?.message ?? String(error),
            activeCount: this.aiService?.requestQueue?.activeCount ?? null,
            queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
          });
          throw error;
        } finally {
          if (this.entryClassificationTokens.get(entryID) === token) {
            const threadID = this.#entryThreadID(entryID);
            this.entryClassificationProcessingEntryIDs.delete(entryID);
            this.entryClassificationTasks.delete(entryID);
            this.entryClassificationTokens.delete(entryID);
            this.#notifyAsyncStateChanged(threadID);
          }
        }
      }
    });

    this.entryClassificationTasks.set(entryID, handle);
    return handle.promise;
  }

  async classifyEntryStatus({ entryID } = {}) {
    this.#assertRepository();
    const entry = this.#fetchEntry(entryID);
    if (!entry) {
      throw new Error(`Unknown entry: ${entryID}`);
    }
    const eligibility = this.#entryStatusClassificationEligibility(entry);
    if (!eligibility.allowed) {
      this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
        entryID,
        status: eligibility.status,
        message: eligibility.reason,
        currentStatus: entry.status,
        suggestedStatus: entry.status,
        updatedAt: new Date().toISOString()
      }));
      return {
        status: entry.status,
        skipped: true,
        reason: eligibility.reason
      };
    }
    if (!this.aiService || typeof this.aiService.classifyEntryStatus !== "function") {
      this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
        entryID,
        status: "notConfigured",
        message: "AI backend is not configured.",
        currentStatus: entry.status,
        suggestedStatus: entry.status,
        updatedAt: new Date().toISOString()
      }));
      return {
        status: entry.status,
        skipped: true,
        reason: "AI backend is not configured."
      };
    }

    const token = `${Date.now()}-${Math.random()}`;
    this.#clearEntryStatusClassificationState(entryID, "superseded status classification");
    this.entryStatusClassificationTokens.set(entryID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `status:${entryID}`,
      execute: async ({ signal }) => {
        const startedAt = new Date().toISOString();
        this.entryStatusClassificationProcessingEntryIDs.add(entryID);
        this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
          entryID,
          status: "processing",
          message: "AI 正在判断工作状态",
          currentStatus: entry.status,
          startedAt,
          updatedAt: startedAt
        }));
        this.#logAITask("started", {
          label: `status:${entryID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        try {
          const latestThread = entry.threadID ? this.openThread(entry.threadID) : null;
          const result = await this.aiService.classifyEntryStatus({
            entryID,
            threadID: entry.threadID,
            normalizedText: entry.summaryText ?? entry.body?.text ?? "",
            currentKind: entry.kind,
            currentStatus: entry.status,
            threadTitle: latestThread?.thread?.title ?? "",
            threadGoal: latestThread?.thread?.goalLayer?.goalStatement ?? "",
            recentThreadEntries: (latestThread?.entries ?? [])
              .filter((item) => item.id !== entryID)
              .slice(-6)
              .map((item) => ({
                id: item.id,
                text: item.summaryText,
                kind: item.kind,
                status: item.status
              }))
          }, { signal });

          if (this.entryStatusClassificationTokens.get(entryID) !== token) {
            this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
              entryID,
              status: "stale",
              message: "Superseded status result was ignored.",
              currentStatus: entry.status,
              suggestedStatus: result.status,
              confidence: result.confidence,
              updatedAt: new Date().toISOString()
            }));
            return { status: entry.status, skipped: true, reason: "Superseded status result." };
          }

          const latest = this.#fetchEntry(entryID);
          const latestEligibility = this.#entryStatusClassificationEligibility(latest);
          if (!latest || !latestEligibility.allowed || !this.#shouldApplyEntryStatusClassification(result, latest)) {
            this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
              entryID,
              status: latest ? latestEligibility.status : "stale",
              message: latest
                ? latestEligibility.allowed
                  ? result.reason || "Status result did not meet overwrite rules."
                  : latestEligibility.reason
                : "Entry disappeared before status classification could be applied.",
              currentStatus: latest?.status ?? entry.status,
              suggestedStatus: result.status,
              confidence: result.confidence,
              debugPayload: result.debugPayload ?? null,
              startedAt,
              updatedAt: new Date().toISOString()
            }));
            return {
              status: latest?.status ?? entry.status,
              skipped: true,
              reason: latestEligibility.reason || result.reason
            };
          }

          if (result.status !== latest.status) {
            await this.repository.saveEntry({
              ...latest,
              status: result.status,
              statusMetadata: mergeAIStatusMetadata(latest.statusMetadata ?? null, {
                status: result.status,
                confidence: result.confidence,
                reason: result.reason,
                updatedAt: new Date().toISOString()
              })
            });
            await this.repository.flush();
            this.#refreshWorkspaceState();
            if (latest.threadID) {
              this.invalidateAIOutputState(`classifyStatus:${latest.threadID}`, {
                threadIDs: [latest.threadID]
              });
            }
          }

          this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
            entryID,
            status: result.status === entry.status ? "idle" : "ready",
            message: result.reason || (result.status === entry.status ? "Status remains unchanged." : "Entry status updated."),
            currentStatus: (this.#fetchEntry(entryID) ?? latest ?? entry).status,
            suggestedStatus: result.status,
            confidence: result.confidence,
            debugPayload: result.debugPayload ?? null,
            startedAt,
            updatedAt: new Date().toISOString()
          }));
          this.#logAITask("completed", {
            label: `status:${entryID}`,
            status: result.status,
            activeCount: this.aiService?.requestQueue?.activeCount ?? null,
            queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
          });
          return result;
        } catch (error) {
          if (this.entryStatusClassificationTokens.get(entryID) !== token) {
            throw error;
          }
          const errorKind = classifyAIError(error);
          this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
            entryID,
            status: errorKind === "abort" ? "cancelled" : "failed",
            message: error?.message ?? "Entry status classification failed.",
            errorKind,
            rawErrorMessage: error?.message ?? String(error),
            currentStatus: entry.status,
            startedAt,
            updatedAt: new Date().toISOString()
          }));
          this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
            label: `status:${entryID}`,
            error: error?.message ?? String(error),
            activeCount: this.aiService?.requestQueue?.activeCount ?? null,
            queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
          });
          throw error;
        } finally {
          if (this.entryStatusClassificationTokens.get(entryID) === token) {
            const threadID = this.#entryThreadID(entryID);
            this.entryStatusClassificationProcessingEntryIDs.delete(entryID);
            this.entryStatusClassificationTasks.delete(entryID);
            this.entryStatusClassificationTokens.delete(entryID);
            this.#notifyAsyncStateChanged(threadID);
          }
        }
      }
    });

    this.entryStatusClassificationTasks.set(entryID, handle);
    return handle.promise;
  }

  async synthesizeThreadState({ threadID } = {}) {
    this.#assertRepository();
    const initialThreadView = this.#openThreadAIAggregate(threadID);
    if (!initialThreadView) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    if (!this.aiService) {
      return initialThreadView.aiSnapshot ?? null;
    }

    const fingerprint = this.#threadContentFingerprint(initialThreadView);
    if (initialThreadView.aiSnapshot?.contentFingerprint === fingerprint) {
      return initialThreadView.aiSnapshot;
    }

    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.resumeSynthesisTasks.get(threadID), `resume:${threadID}`, "superseded resume");
    this.resumeSynthesisTasks.delete(threadID);
    this.resumeSynthesisTokens.set(threadID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `resume:${threadID}`,
      execute: async ({ signal }) => {
      const startedAt = new Date().toISOString();
      this.resumeSynthesisProcessingThreadIDs.add(threadID);
      this.#setThreadAIStatus(threadID, {
        resume: createThreadOperationState({
          status: "loading",
          message: "AI 正在整理线程",
          startedAt,
          updatedAt: new Date().toISOString()
        })
      });
      this.#logAITask("started", {
        label: `resume:${threadID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const latest = initialThreadView;
        if (!latest) {
          return null;
        }
        const latestFingerprint = this.#threadContentFingerprint(latest);
        const latestAnchor = latest.anchors.at(-1) ?? null;
        const statusSummary = latest.statusSummary ?? emptyStatusSummary();
        const evidencePackage = this.#buildThreadAIEvidencePackage({
          threadView: latest,
          type: "resume"
        });
        const result = await this.aiService.synthesizeResume({
          threadID,
          coreQuestion: latest.thread.goalLayer?.goalStatement ?? latest.thread.prompt ?? latest.thread.title,
          goalLayer: latest.thread.goalLayer ?? {},
          activeClaims: evidencePackage.activeClaims,
          currentJudgment: latest.aiSnapshot?.currentJudgment ?? latestAnchor?.stateSummary ?? "",
          judgmentBasis: evidencePackage.judgmentBasis,
          openLoops: latestAnchor?.openLoops ?? latest.aiSnapshot?.openLoops ?? [],
          nextAction: latest.aiSnapshot?.nextAction ?? null,
          recoveryLines: latest.aiSnapshot?.recoveryLines ?? [],
          resolvedSoFar: latest.claims
            .filter((claim) => claim.status === "stable")
            .slice(0, 4)
            .map((claim) => claim.statement),
          statusSummary: mapResumeStatusSummary(statusSummary),
          recentNotes: evidencePackage.recentNotes,
          evidenceCount: latest.counts?.evidenceCount ?? 0,
          sourceCount: latest.counts?.sourceCount ?? 0
        }, { signal });

        if (this.resumeSynthesisTokens.get(threadID) !== token) {
          this.#setThreadAIStatus(threadID, {
            resume: createThreadOperationState({
              status: "cancelled",
              message: "Superseded resume result was ignored.",
              updatedAt: new Date().toISOString()
            })
          });
          return null;
        }

        const resumeStatus = detectInvalidResumePlan(result)
          ? {
              status: "invalidPlan",
              message: "AI planner output was rejected. Falling back to deterministic restart note.",
              debugPayload: result.debugPayload ?? null,
              startedAt,
              updatedAt: new Date().toISOString()
            }
          : {
              status: "ready",
              message: "Restart note is ready.",
              debugPayload: result.debugPayload ?? null,
              startedAt,
              updatedAt: new Date().toISOString()
            };

        const snapshot = createThreadAISnapshot({
          threadID,
          contentFingerprint: latestFingerprint,
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
        this.#refreshWorkspaceState();
        this.threadResumePendingCount.set(threadID, 0);
        this.#setThreadAIStatus(threadID, {
          resume: createThreadOperationState(resumeStatus)
        });
        this.#logAITask("completed", {
          label: `resume:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return snapshot;
      } catch (error) {
        const previous = this.threadAIStatusByThreadID.get(threadID) ?? createThreadAIStatusState();
        this.#setThreadAIStatus(threadID, {
          resume: createThreadOperationState({
            status: classifyAIError(error) === "abort" ? "cancelled" : "failed",
            message: error?.message ?? "Resume synthesis failed.",
            errorKind: classifyAIError(error),
            rawErrorMessage: error?.message ?? String(error),
            startedAt: previous.resume?.startedAt ?? startedAt,
            updatedAt: new Date().toISOString()
          })
        });
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
      const startedAt = new Date().toISOString();
      this.discourseInferenceProcessingThreadIDs.add(threadID);
      this.#setThreadAIStatus(threadID, {
        discourse: createThreadOperationState({
          status: "loading",
          message: "Refreshing discourse relations.",
          startedAt,
          updatedAt: new Date().toISOString()
        })
      });
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
        const fullEntries = this.#resolvedEntries(this.repository.store.fetchEntries(threadID));
        const pairs = this.discourseInferenceEngine.findCandidatePairs(fullEntries);
        if (pairs.length === 0) {
          this.#setThreadAIStatus(threadID, {
            discourse: createThreadOperationState({
              status: "ready",
              message: "No discourse candidates required refresh.",
              startedAt,
              updatedAt: new Date().toISOString()
            })
          });
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
          this.#setThreadAIStatus(threadID, {
            discourse: createThreadOperationState({
              status: "cancelled",
              message: "Superseded discourse result was ignored.",
              updatedAt: new Date().toISOString()
            })
          });
          return [];
        }

        const threadEntryIDs = new Set(fullEntries.map((entry) => entry.id));
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
        this.#refreshWorkspaceState();
        this.#setThreadAIStatus(threadID, {
          discourse: createThreadOperationState({
            status: "ready",
            message: "Discourse relations refreshed.",
            startedAt,
            updatedAt: new Date().toISOString()
          })
        });
        this.#logAITask("completed", {
          label: `discourse:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        return this.openThread(threadID)?.discourseRelations ?? [];
      } catch (error) {
        const previous = this.threadAIStatusByThreadID.get(threadID) ?? createThreadAIStatusState();
        this.#setThreadAIStatus(threadID, {
          discourse: createThreadOperationState({
            status: classifyAIError(error) === "abort" ? "cancelled" : "failed",
            message: error?.message ?? "Discourse inference failed.",
            errorKind: classifyAIError(error),
            rawErrorMessage: error?.message ?? String(error),
            startedAt: previous.discourse?.startedAt ?? startedAt,
            updatedAt: new Date().toISOString()
          })
        });
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
    const initialThreadView = this.#openThreadAIAggregate(threadID);
    if (!initialThreadView) {
      throw new Error(`Unknown thread: ${threadID}`);
    }
    if (!this.aiService) {
      const notConfigured = {
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
      this.#setPreparedView(threadID, notConfigured);
      this.#setThreadAIStatus(threadID, {
        prepare: createThreadOperationState({
          status: "notConfigured",
          message: "AI backend is not configured.",
          updatedAt: new Date().toISOString()
        })
      });
      return notConfigured;
    }
    const token = `${Date.now()}-${Math.random()}`;
    this.#cancelTaskHandle(this.draftPreparationTasks.get(threadID), `prepare:${threadID}`, "superseded prepare");
    this.draftPreparationTasks.delete(threadID);
    this.draftPreparationTokens.set(threadID, token);

    const handle = this.#createTaskHandle({
      token,
      label: `prepare:${threadID}`,
      execute: async ({ signal }) => {
      const startedAt = new Date().toISOString();
      this.draftPreparationProcessingThreadIDs.add(threadID);
      this.#setThreadAIStatus(threadID, {
        prepare: createThreadOperationState({
          status: "loading",
          message: "Preparing draft view.",
          startedAt,
          updatedAt: new Date().toISOString()
        })
      });
      this.#logAITask("started", {
        label: `prepare:${threadID}`,
        activeCount: this.aiService?.requestQueue?.activeCount ?? null,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      try {
        const latest = initialThreadView;
        if (!latest) {
          throw new Error(`Unknown thread: ${threadID}`);
        }
        const evidencePackage = this.#buildThreadAIEvidencePackage({
          threadView: latest,
          type
        });
        const prepared = await this.aiService.prepareDraft({
          threadID,
          type,
          coreQuestion: latest.thread.goalLayer.goalStatement,
          activeClaims: evidencePackage.activeClaims,
          openLoops: latest.anchors.at(-1)?.openLoops ?? [],
          keyEvidence: evidencePackage.keyEvidence,
          recentNotes: evidencePackage.recentNotes.map((item) => ({ id: item.id, text: item.text }))
        }, { signal });

        if (this.draftPreparationTokens.get(threadID) !== token) {
          const staleResult = {
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
          this.#setPreparedView(threadID, staleResult);
          this.#setThreadAIStatus(threadID, {
            prepare: createThreadOperationState({
              status: "cancelled",
              message: staleResult.contentState.message,
              startedAt,
              updatedAt: new Date().toISOString()
            })
          });
          return staleResult;
        }

        this.#logAITask("completed", {
          label: `prepare:${threadID}`,
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        const readyResult = {
          ...prepared,
          threadID,
          type,
          contentState: {
            status: "ready",
            message: `Prepared by ${this.aiProviderRuntime?.backendLabel ?? "AI backend"}.`
          }
        };
        this.#setPreparedView(threadID, readyResult);
        this.#setThreadAIStatus(threadID, {
          prepare: createThreadOperationState({
            status: "ready",
            message: readyResult.contentState.message,
            debugPayload: prepared.debugPayload ?? null,
            startedAt,
            updatedAt: new Date().toISOString()
          })
        });
        return readyResult;
      } catch (error) {
        const previous = this.threadAIStatusByThreadID.get(threadID) ?? createThreadAIStatusState();
        this.#logAITask(isAbortError(error) ? "cancelled" : "failed", {
          label: `prepare:${threadID}`,
          error: error?.message ?? String(error),
          activeCount: this.aiService?.requestQueue?.activeCount ?? null,
          queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
        });
        const failedResult = {
          threadID,
          type,
          title: `${capitalize(type)} unavailable`,
          openLoops: initialThreadView.anchors.at(-1)?.openLoops ?? [],
          recommendedNextSteps: [],
          contentState: {
            status: "error",
            message: error.message
          }
        };
        this.#setPreparedView(threadID, failedResult);
        this.#setThreadAIStatus(threadID, {
          prepare: createThreadOperationState({
            status: classifyAIError(error) === "abort" ? "cancelled" : "failed",
            message: failedResult.contentState.message,
            errorKind: classifyAIError(error),
            rawErrorMessage: error?.message ?? String(error),
            startedAt: previous.prepare?.startedAt ?? startedAt,
            updatedAt: new Date().toISOString()
          })
        });
        return failedResult;
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
    const thread = this.#openThreadAggregate(threadID);
    if (!thread) {
      return null;
    }
    await this.inferDiscourseRelations({ threadID }).catch(() => null);
    await this.synthesizeThreadState({ threadID }).catch(() => null);
    return this.openThread(threadID);
  }

  #configureRepository(databasePath) {
    this.repository = this.repositoryFactory(path.resolve(databasePath));
    this.snapshot = emptySnapshot();
  }

  #buildThreadAIEvidencePackage({ threadView, type = "resume" } = {}) {
    const threadID = threadView?.thread?.id ?? null;
    if (!threadID || !this.repository?.store) {
      return createEmptyAIEvidencePackage();
    }

    const queryList = buildThreadAIQueries({ threadView, type }).slice(0, AI_CONTEXT_LIMITS.maxQueries);
    const recentRawDocuments = collectRecentRawDocuments({ store: this.repository.store, threadID });
    const rankedDocs = collectRankedThreadDocuments({
      retrievalEngine: this.repository?.retrievalEngine ?? null,
      store: this.repository.store,
      threadID,
      queries: queryList,
      recentRawDocuments
    });
    const rankedEvidenceDocs = collectRankedThreadDocuments({
      retrievalEngine: this.repository?.retrievalEngine ?? null,
      store: this.repository.store,
      threadID,
      queries: queryList,
      ownerTypes: ["entry", "memory", "anchor"],
      recentRawDocuments
    }).filter((document) => isEvidenceLikeDocument(document));
    const byLayer = splitRankedDocumentsByLayer(rankedDocs);
    const semanticDocs = takeDocuments(byLayer.semantic, AI_CONTEXT_LIMITS.semanticDocs);
    const episodicDocs = takeDocuments(byLayer.episodic, AI_CONTEXT_LIMITS.episodicDocs);
    const sourceDocs = takeDocuments(byLayer.source, AI_CONTEXT_LIMITS.sourceDocs);
    const rawDocs = takeDocuments(
      [...byLayer.raw, ...recentRawDocuments],
      AI_CONTEXT_LIMITS.rawDocs
    );

    const activeClaims = dedupeStrings([
      ...semanticDocs.map(documentToClaimText),
      ...(threadView.claims ?? []).map((claim) => claim.statement)
    ]).slice(0, AI_CONTEXT_LIMITS.semanticDocs);

    const evidenceDocs = takeDocuments(
      [...rankedEvidenceDocs, ...sourceDocs, ...rawDocs.filter((doc) => isEvidenceLikeDocument(doc))],
      AI_CONTEXT_LIMITS.keyEvidence
    );
    const recentNotes = takeDocuments(
      [...episodicDocs, ...rawDocs],
      AI_CONTEXT_LIMITS.recentNotes
    ).map(documentToRecentNote);

    return {
      queryList,
      semanticDocs,
      episodicDocs,
      sourceDocs,
      rawDocs,
      activeClaims,
      judgmentBasis: evidenceDocs
        .slice(0, AI_CONTEXT_LIMITS.judgmentBasis)
        .map(documentToPromptText)
        .filter(Boolean)
        .join(" | "),
      keyEvidence: evidenceDocs.map((doc) => ({
        id: doc.ownerID ?? doc.id,
        text: documentToPromptText(doc)
      })),
      recentNotes
    };
  }

  #routingEngine() {
    return new ThreadRoutingEngine({
      threadsProvider: () => this.snapshot.threads,
      entriesProvider: (threadID) => this.repository?.store?.fetchEntries(threadID) ?? [],
      claimsProvider: (threadID) => this.repository?.store?.fetchClaims(threadID) ?? [],
      latestAnchorProvider: (threadID) =>
        this.repository?.store?.fetchLatestAnchor(threadID) ?? null,
      retrievalEngine: this.repository?.retrievalEngine ?? null
    });
  }

  #assertRepository() {
    if (!this.repository) {
      throw new Error("Workspace is not configured");
    }
  }

  async #refreshThreadAI(threadID, { includeDiscourse = true } = {}) {
    if (!threadID || !this.aiService) {
      return;
    }
    if (includeDiscourse) {
      await this.inferDiscourseRelations({ threadID }).catch(() => null);
    }
    await this.synthesizeThreadState({ threadID }).catch(() => null);
  }

  #normalizeRoutingDecision(decision) {
    return decision?.type === "route"
      ? { type: "route", threadID: decision.threadID, reason: decision.reason ?? "", source: "heuristic" }
      : { type: "noMatch", reason: decision?.reason ?? "No confident thread match.", source: "heuristic" };
  }

  #threadContentFingerprint(threadView) {
    const basis = this.repository?.store?.fetchThreadFingerprintBasis?.(threadView?.thread?.id ?? null);
    return JSON.stringify({
      threadID: threadView.thread.id,
      threadUpdatedAt: basis?.threadUpdatedAt ?? threadView.thread.updatedAt ?? null,
      threadLastActiveAt: basis?.threadLastActiveAt ?? threadView.thread.lastActiveAt ?? null,
      latestAnchorCreatedAt: basis?.latestAnchorCreatedAt ?? threadView.anchors?.at?.(-1)?.createdAt ?? null,
      anchorCount: basis?.anchorCount ?? threadView.anchors?.length ?? 0,
      stableClaimCount: basis?.stableClaimCount ?? (threadView.claims ?? []).filter((claim) => claim.status === "stable").length,
      stableClaimUpdatedAt: basis?.stableClaimUpdatedAt ?? null,
      evidenceCount: basis?.evidenceCount ?? threadView.counts?.evidenceCount ?? 0,
      sourceCount: basis?.sourceCount ?? threadView.counts?.sourceCount ?? 0,
      decidedCount: basis?.decidedCount ?? threadView.counts?.decidedCount ?? 0,
      solvedCount: basis?.solvedCount ?? threadView.counts?.solvedCount ?? 0,
      verifiedCount: basis?.verifiedCount ?? threadView.counts?.verifiedCount ?? 0,
      droppedCount: basis?.droppedCount ?? threadView.counts?.droppedCount ?? 0,
      aggregateUpdatedAt: basis?.aggregateUpdatedAt ?? null,
      retrievalCount: basis?.retrievalCount ?? 0,
      retrievalUpdatedAt: basis?.retrievalUpdatedAt ?? null,
      retrievalVersion: basis?.retrievalVersion ?? 0,
      memoryCount: basis?.memoryCount ?? 0,
      memoryUpdatedAt: basis?.memoryUpdatedAt ?? null,
      memoryVersion: basis?.memoryVersion ?? 0,
      materializedUpdatedAt: basis?.materializedUpdatedAt ?? null
    });
  }

  #scheduleEntryClassificationIfEligible(entryID, { reason = "scheduled", delay = 80 } = {}) {
    if (!this.aiService || typeof this.aiService.classifyEntryKind !== "function" || !entryID) {
      return;
    }
    const entry = this.#fetchEntry(entryID);
    const eligibility = this.#entryClassificationEligibility(entry);
    if (!eligibility.allowed) {
      if (entry) {
        this.#setEntryClassificationDebug(entryID, createEntryClassificationDebugState({
          entryID,
          status: eligibility.status,
          message: eligibility.reason,
          currentKind: entry.kind,
          suggestedKind: entry.kind,
          updatedAt: new Date().toISOString()
        }));
      }
      return;
    }
    const existingTimer = this.entryClassificationTimers.get(entryID);
    if (existingTimer) {
      clearTimeout(existingTimer);
    }
    const timer = setTimeout(() => {
      if (this.entryClassificationTimers.get(entryID) === timer) {
        this.entryClassificationTimers.delete(entryID);
      }
      void this.classifyEntryKind({ entryID }).catch(() => null);
    }, Math.max(0, Number(delay) || 0));
    this.entryClassificationTimers.set(entryID, timer);
    this.#logAITask("scheduled", { label: `classify:${entryID}`, reason });
  }

  #scheduleEntryStatusClassificationIfEligible(entryID, { reason = "scheduled", delay = 120 } = {}) {
    if (!this.aiService || typeof this.aiService.classifyEntryStatus !== "function" || !entryID) {
      return;
    }
    const entry = this.#fetchEntry(entryID);
    const eligibility = this.#entryStatusClassificationEligibility(entry);
    if (!eligibility.allowed) {
      if (entry) {
        this.#setEntryStatusClassificationDebug(entryID, createEntryStatusClassificationDebugState({
          entryID,
          status: eligibility.status,
          message: eligibility.reason,
          currentStatus: entry.status,
          suggestedStatus: entry.status,
          updatedAt: new Date().toISOString()
        }));
      }
      return;
    }
    const existingTimer = this.entryStatusClassificationTimers.get(entryID);
    if (existingTimer) {
      clearTimeout(existingTimer);
    }
    const timer = setTimeout(() => {
      if (this.entryStatusClassificationTimers.get(entryID) === timer) {
        this.entryStatusClassificationTimers.delete(entryID);
      }
      void this.classifyEntryStatus({ entryID }).catch(() => null);
    }, Math.max(0, Number(delay) || 0));
    this.entryStatusClassificationTimers.set(entryID, timer);
    this.#logAITask("scheduled", { label: `status:${entryID}`, reason });
  }

  #entryClassificationEligibility(entry) {
    if (!entry) {
      return { allowed: false, status: "stale", reason: "Entry no longer exists." };
    }
    const explicitTag = String(entry.sourceMetadata?.captureInterpretation?.explicitTag ?? "").trim();
    if (explicitTag) {
      return { allowed: false, status: "idle", reason: "Explicit capture tag locks entry mode." };
    }
    if (entry.sourceMetadata?.kindOverride?.source === "manual") {
      return { allowed: false, status: "idle", reason: "Manual mode override locks entry mode." };
    }
    const attributionSource = String(entry.sourceMetadata?.kindAttribution?.source ?? "heuristic");
    if (!["heuristic", "ai"].includes(attributionSource)) {
      return { allowed: false, status: "idle", reason: "Only system-derived entry modes are eligible for AI reclassification." };
    }
    return { allowed: true, status: "processing", reason: "Eligible for AI kind classification." };
  }

  #entryStatusClassificationEligibility(entry) {
    if (!entry) {
      return { allowed: false, status: "stale", reason: "Entry no longer exists." };
    }
    if (!entry.threadID) {
      return { allowed: false, status: "idle", reason: "Status classification only runs for entries inside a thread." };
    }
    if (entry.statusMetadata?.source === "manual" && normalizeEntryStatus(entry.status) !== EntryStatus.OPEN) {
      return { allowed: false, status: "idle", reason: "Manual status override locks entry status." };
    }
    return { allowed: true, status: "processing", reason: "Eligible for AI status classification." };
  }

  #shouldApplyEntryClassification(result, entry) {
    if (!entry) {
      return false;
    }
    if (String(entry.sourceMetadata?.captureInterpretation?.explicitTag ?? "").trim()) {
      return false;
    }
    if (entry.sourceMetadata?.kindOverride?.source === "manual") {
      return false;
    }
    if (!result?.kind) {
      return false;
    }
    const confidence = Number(result.confidence ?? 0);
    if (!Number.isFinite(confidence) || confidence < 0.65) {
      return false;
    }
    return true;
  }

  #shouldApplyEntryStatusClassification(result, entry) {
    if (!entry) {
      return false;
    }
    if (entry.statusMetadata?.source === "manual" && normalizeEntryStatus(entry.status) !== EntryStatus.OPEN) {
      return false;
    }
    const confidence = Number(result.confidence ?? 0);
    if (!Number.isFinite(confidence) || confidence < 0.72) {
      return false;
    }
    return true;
  }

  #clearEntryClassificationState(entryID, reason = "cleared") {
    if (!entryID) {
      return;
    }
    const threadID = this.#entryThreadID(entryID);
    const timer = this.entryClassificationTimers.get(entryID);
    if (timer) {
      clearTimeout(timer);
      this.entryClassificationTimers.delete(entryID);
    }
    this.#cancelTaskHandle(this.entryClassificationTasks.get(entryID), `classify:${entryID}`, reason);
    this.entryClassificationTasks.delete(entryID);
    this.entryClassificationTokens.delete(entryID);
    this.entryClassificationProcessingEntryIDs.delete(entryID);
    this.entryClassificationDebugByEntryID.delete(entryID);
    this.#notifyAsyncStateChanged(threadID);
  }

  #clearEntryStatusClassificationState(entryID, reason = "cleared") {
    if (!entryID) {
      return;
    }
    const threadID = this.#entryThreadID(entryID);
    const timer = this.entryStatusClassificationTimers.get(entryID);
    if (timer) {
      clearTimeout(timer);
      this.entryStatusClassificationTimers.delete(entryID);
    }
    this.#cancelTaskHandle(this.entryStatusClassificationTasks.get(entryID), `status:${entryID}`, reason);
    this.entryStatusClassificationTasks.delete(entryID);
    this.entryStatusClassificationTokens.delete(entryID);
    this.entryStatusClassificationProcessingEntryIDs.delete(entryID);
    this.entryStatusClassificationDebugByEntryID.delete(entryID);
    this.#notifyAsyncStateChanged(threadID);
  }

  #entryThreadID(entryID) {
    return this.#fetchEntry(entryID)?.threadID ?? null;
  }

  #entryAIActivityState() {
    const routePlanningEntryIDs = new Set();
    for (const [entryID, state] of this.routeDebugByEntryID.entries()) {
      if (state?.status === "processing") {
        routePlanningEntryIDs.add(entryID);
      }
    }
    const threadRefreshingThreadIDs = new Set();
    for (const [threadID, state] of this.threadAIStatusByThreadID.entries()) {
      if (state?.resume?.status === "loading" || state?.discourse?.status === "loading" || state?.prepare?.status === "loading") {
        threadRefreshingThreadIDs.add(threadID);
      }
    }
    return {
      entryClassificationEntryIDs: new Set(this.entryClassificationProcessingEntryIDs),
      entryStatusClassificationEntryIDs: new Set(this.entryStatusClassificationProcessingEntryIDs),
      routePlanningEntryIDs,
      threadRefreshingThreadIDs
    };
  }

  #queueDebugState() {
    return this.aiService?.requestQueue?.debugSnapshot?.() ?? {
      maxConcurrent: 0,
      activeCount: 0,
      queueDepth: 0,
      activeLabels: [],
      pendingLabels: []
    };
  }

  #activeOperationLabels({ threadID = null } = {}) {
    const operations = [
      ...Array.from(this.routePlanningTasks.values()),
      ...Array.from(this.entryClassificationTasks.values()),
      ...Array.from(this.entryStatusClassificationTasks.values()),
      ...Array.from(this.resumeSynthesisTasks.values()),
      ...Array.from(this.draftPreparationTasks.values()),
      ...Array.from(this.discourseInferenceTasks.values())
    ]
      .filter((handle) => handle?.label)
      .map((handle) => ({
        label: handle.label,
        startedAt: handle.startedAt ? new Date(handle.startedAt).toISOString() : null,
        elapsedMS: handle.startedAt ? Math.max(0, Date.now() - handle.startedAt) : null
      }));
    if (!threadID) {
      return operations.sort((lhs, rhs) => lhs.label.localeCompare(rhs.label));
    }
    return operations.filter((operation) => operation.label.includes(threadID))
      .sort((lhs, rhs) => lhs.label.localeCompare(rhs.label));
  }

  #setRouteDebug(entryID, nextState) {
    if (!entryID || !nextState) {
      return;
    }
    this.routeDebugByEntryID.set(entryID, createRouteDebugState({
      ...(this.routeDebugByEntryID.get(entryID) ?? {}),
      ...nextState,
      elapsedMS: computeElapsedMS(nextState.startedAt ?? this.routeDebugByEntryID.get(entryID)?.startedAt, nextState.updatedAt),
      entryID
    }));
    this.#notifyAsyncStateChanged();
  }

  #setEntryClassificationDebug(entryID, nextState) {
    if (!entryID || !nextState) {
      return;
    }
    const threadID = this.#entryThreadID(entryID);
    this.entryClassificationDebugByEntryID.set(entryID, createEntryClassificationDebugState({
      ...(this.entryClassificationDebugByEntryID.get(entryID) ?? {}),
      ...nextState,
      elapsedMS: computeElapsedMS(
        nextState.startedAt ?? this.entryClassificationDebugByEntryID.get(entryID)?.startedAt,
        nextState.updatedAt
      ),
      entryID
    }));
    this.#notifyAsyncStateChanged(threadID);
  }

  #setEntryStatusClassificationDebug(entryID, nextState) {
    if (!entryID || !nextState) {
      return;
    }
    const threadID = this.#entryThreadID(entryID);
    this.entryStatusClassificationDebugByEntryID.set(entryID, createEntryStatusClassificationDebugState({
      ...(this.entryStatusClassificationDebugByEntryID.get(entryID) ?? {}),
      ...nextState,
      elapsedMS: computeElapsedMS(
        nextState.startedAt ?? this.entryStatusClassificationDebugByEntryID.get(entryID)?.startedAt,
        nextState.updatedAt
      ),
      entryID
    }));
    this.#notifyAsyncStateChanged(threadID);
  }

  #setThreadAIStatus(threadID, partialState = {}) {
    if (!threadID) {
      return;
    }
    const current = this.threadAIStatusByThreadID.get(threadID) ?? createThreadAIStatusState();
    const next = {
      resume: partialState.resume ? createThreadOperationState({
        ...current.resume,
        ...partialState.resume
      }) : current.resume,
      prepare: partialState.prepare ? createThreadOperationState({
        ...current.prepare,
        ...partialState.prepare
      }) : current.prepare,
      discourse: partialState.discourse ? createThreadOperationState({
        ...current.discourse,
        ...partialState.discourse
      }) : current.discourse,
      updatedAt: new Date().toISOString()
    };
    this.threadAIStatusByThreadID.set(threadID, next);
    this.#notifyAsyncStateChanged(threadID);
  }

  #setPreparedView(threadID, preparedView) {
    if (!threadID) {
      return;
    }
    if (preparedView == null) {
      this.preparedViewByThreadID.delete(threadID);
    } else {
      this.preparedViewByThreadID.set(threadID, {
        ...preparedView,
        threadID
      });
    }
    this.#notifyAsyncStateChanged(threadID);
  }

  #getThreadAIStatus(threadID, { hasSnapshot = false } = {}) {
    const status = this.threadAIStatusByThreadID.get(threadID) ?? createThreadAIStatusState();
    if (!this.aiService) {
      return {
        ...status,
        resume: createThreadOperationState({
          ...status.resume,
          status: hasSnapshot ? "ready" : "notConfigured",
          message: hasSnapshot ? "Restart note is ready." : "AI backend is not configured."
        }),
        prepare: createThreadOperationState({
          ...status.prepare,
          status: "notConfigured",
          message: "AI backend is not configured."
        }),
        discourse: createThreadOperationState({
          ...status.discourse,
          status: "notConfigured",
          message: "AI backend is not configured."
        })
      };
    }
    if (hasSnapshot && status.resume.status === "idle") {
      return {
        ...status,
        resume: createThreadOperationState({
          ...status.resume,
          status: "ready",
          message: "Restart note is ready."
        })
      };
    }
    return status;
  }

  #notifyAsyncStateChanged(threadID = null) {
    if (!this.onAsyncStateChanged) {
      return;
    }
    Promise.resolve().then(() => this.onAsyncStateChanged({
      threadID: threadID ?? null
    })).catch(() => null);
  }

  async #runBackgroundSweep(reason = "scheduled") {
    if (this.backgroundSweepRunning || !this.aiService || !this.repository) {
      return;
    }
    const queueDepth = this.aiService?.requestQueue?.queueDepth ?? 0;
    if (queueDepth > 10) {
      this.#logAITask("sweep-skipped", { reason, queueDepth });
      this.scheduleSweep({ delay: 1000, reason: `${reason}:retry` });
      return;
    }

    this.backgroundSweepRunning = true;
    const entryIDs = Array.from(this.invalidatedEntryIDs);
    const threadIDs = Array.from(this.invalidatedThreadIDs);
    this.invalidatedEntryIDs.clear();
    this.invalidatedThreadIDs.clear();
    this.#logAITask("sweep-started", { reason, entryCount: entryIDs.length, threadCount: threadIDs.length, queueDepth });

    try {
      for (const entryID of entryIDs) {
        const entry = this.#fetchEntry(entryID);
        if (!entry || entry.threadID) {
          continue;
        }
        const decision = await this.planRouteForEntry({ entryID, autoRoute: true }).catch(() => null);
        if (decision?.type === "route" && decision.threadID) {
          this.#incrementThreadResumePendingCount(decision.threadID);
          this.invalidatedThreadIDs.add(decision.threadID);
          this.invalidatedThreadUpdatedAt.set(decision.threadID, Date.now());
          this.invalidatedThreadNeedsDiscourse.set(
            decision.threadID,
            Boolean(this.invalidatedThreadNeedsDiscourse.get(decision.threadID))
          );
        }
      }

      for (const threadID of threadIDs) {
        if (!this.snapshot.threads.some((thread) => thread.id === threadID)) {
          this.#clearThreadRefreshState(threadID);
          this.threadResumePendingCount.delete(threadID);
          continue;
        }
        const updatedAt = this.invalidatedThreadUpdatedAt.get(threadID) ?? 0;
        const remainingQuietMS = THREAD_REFRESH_QUIET_MS - (Date.now() - updatedAt);
        if (remainingQuietMS > 0) {
          this.invalidatedThreadIDs.add(threadID);
          continue;
        }
        const includeDiscourse = Boolean(this.invalidatedThreadNeedsDiscourse.get(threadID));
        const pendingResumeCount = this.threadResumePendingCount.get(threadID) ?? 0;
        if (!includeDiscourse && pendingResumeCount < THREAD_RESUME_PENDING_THRESHOLD) {
          this.#clearThreadRefreshState(threadID);
          continue;
        }
        this.#clearThreadRefreshState(threadID);
        await this.#refreshThreadAI(threadID, { includeDiscourse }).catch(() => null);
      }
    } finally {
      this.backgroundSweepRunning = false;
      this.#logAITask("sweep-finished", {
        reason,
        pendingEntries: this.invalidatedEntryIDs.size,
        pendingThreads: this.invalidatedThreadIDs.size,
        queueDepth: this.aiService?.requestQueue?.queueDepth ?? null
      });
      this.#notifyAsyncStateChanged();
      if (this.invalidatedEntryIDs.size > 0 || this.invalidatedThreadIDs.size > 0) {
        const followupDelay = this.invalidatedEntryIDs.size > 0
          ? 250
          : Math.max(50, this.#nextThreadRefreshDelay());
        this.scheduleSweep({ delay: followupDelay, reason: `${reason}:followup` });
      }
    }
  }

  #nextThreadRefreshDelay() {
    let delay = null;
    const now = Date.now();
    for (const threadID of this.invalidatedThreadIDs) {
      const updatedAt = this.invalidatedThreadUpdatedAt.get(threadID) ?? now;
      const remainingQuietMS = Math.max(0, THREAD_REFRESH_QUIET_MS - (now - updatedAt));
      delay = delay == null ? remainingQuietMS : Math.min(delay, remainingQuietMS);
    }
    return delay ?? 250;
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
    for (const [entryID, handle] of this.entryClassificationTasks) {
      this.#cancelTaskHandle(handle, `classify:${entryID}`, reason);
    }
    for (const [entryID, handle] of this.entryStatusClassificationTasks) {
      this.#cancelTaskHandle(handle, `status:${entryID}`, reason);
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
    this.entryClassificationTasks.clear();
    this.entryClassificationTokens.clear();
    this.entryClassificationProcessingEntryIDs.clear();
    for (const timer of this.entryClassificationTimers.values()) {
      clearTimeout(timer);
    }
    this.entryClassificationTimers.clear();
    this.entryStatusClassificationTasks.clear();
    this.entryStatusClassificationTokens.clear();
    this.entryStatusClassificationProcessingEntryIDs.clear();
    for (const timer of this.entryStatusClassificationTimers.values()) {
      clearTimeout(timer);
    }
    this.entryStatusClassificationTimers.clear();
    this.resumeSynthesisTasks.clear();
    this.resumeSynthesisTokens.clear();
    this.resumeSynthesisProcessingThreadIDs.clear();
    this.draftPreparationTasks.clear();
    this.draftPreparationTokens.clear();
    this.draftPreparationProcessingThreadIDs.clear();
    this.discourseInferenceTasks.clear();
    this.discourseInferenceTokens.clear();
    this.discourseInferenceProcessingThreadIDs.clear();
    if (this.backgroundSweepTimer) {
      clearTimeout(this.backgroundSweepTimer);
      this.backgroundSweepTimer = null;
    }
    this.backgroundSweepRunning = false;
  }

  #logAITask(event, details = {}) {
    console.error("[ai-task]", event, details);
  }
}

function capitalize(value) {
  const text = String(value ?? "");
  return text ? `${text[0].toUpperCase()}${text.slice(1)}` : "Prepare";
}

function createRouteDebugState({
  entryID = null,
  status = "idle",
  message = "",
  decisionReason = "",
  selectedThreadID = null,
  source = "ai",
  errorKind = null,
  rawErrorMessage = "",
  debugPayload = null,
  startedAt = null,
  updatedAt = null,
  elapsedMS = null
} = {}) {
  const payload = debugPayload ?? null;
  return {
    entryID,
    status,
    message: String(message ?? ""),
    decisionReason: String(decisionReason ?? message ?? ""),
    selectedThreadID,
    source,
    errorKind,
    rawErrorMessage: String(rawErrorMessage ?? ""),
    debugPayload: payload,
    promptStats: payload?.promptStats ?? "",
    responseModelID: payload?.responseModelID ?? null,
    finishReason: payload?.finishReason ?? null,
    updatedAt: updatedAt ?? new Date().toISOString(),
    startedAt: startedAt ?? null,
    elapsedMS
  };
}

function createEntryClassificationDebugState({
  entryID = null,
  status = "idle",
  message = "",
  currentKind = EntryKind.NOTE,
  suggestedKind = null,
  confidence = null,
  source = "heuristic",
  errorKind = null,
  rawErrorMessage = "",
  debugPayload = null,
  startedAt = null,
  updatedAt = null,
  elapsedMS = null
} = {}) {
  const payload = debugPayload ?? null;
  return {
    entryID,
    status,
    message: String(message ?? ""),
    currentKind: String(currentKind ?? EntryKind.NOTE),
    suggestedKind: suggestedKind ? String(suggestedKind) : null,
    confidence: Number.isFinite(Number(confidence)) ? Number(confidence) : null,
    source: String(source ?? "heuristic"),
    errorKind,
    rawErrorMessage: String(rawErrorMessage ?? ""),
    debugPayload: payload,
    promptStats: payload?.promptStats ?? "",
    responseModelID: payload?.responseModelID ?? null,
    finishReason: payload?.finishReason ?? null,
    updatedAt: updatedAt ?? new Date().toISOString(),
    startedAt: startedAt ?? null,
    elapsedMS
  };
}

function createEntryStatusClassificationDebugState({
  entryID = null,
  status = "idle",
  message = "",
  currentStatus = EntryStatus.OPEN,
  suggestedStatus = null,
  confidence = null,
  source = "heuristic",
  errorKind = null,
  rawErrorMessage = "",
  debugPayload = null,
  startedAt = null,
  updatedAt = null,
  elapsedMS = null
} = {}) {
  const payload = debugPayload ?? null;
  return {
    entryID,
    status,
    message: String(message ?? ""),
    currentStatus: String(currentStatus ?? EntryStatus.OPEN),
    suggestedStatus: suggestedStatus ? String(suggestedStatus) : null,
    confidence: Number.isFinite(Number(confidence)) ? Number(confidence) : null,
    source: String(source ?? "heuristic"),
    errorKind,
    rawErrorMessage: String(rawErrorMessage ?? ""),
    debugPayload: payload,
    promptStats: payload?.promptStats ?? "",
    responseModelID: payload?.responseModelID ?? null,
    finishReason: payload?.finishReason ?? null,
    updatedAt: updatedAt ?? new Date().toISOString(),
    startedAt: startedAt ?? null,
    elapsedMS
  };
}

function createThreadAIStatusState() {
  return {
    resume: createThreadOperationState(),
    prepare: createThreadOperationState(),
    discourse: createThreadOperationState(),
    updatedAt: null
  };
}

function createThreadOperationState({
  status = "idle",
  message = "",
  errorKind = null,
  rawErrorMessage = "",
  debugPayload = null,
  startedAt = null,
  updatedAt = null
} = {}) {
  const payload = debugPayload ?? null;
  return {
    status,
    message: String(message ?? ""),
    errorKind,
    rawErrorMessage: String(rawErrorMessage ?? ""),
    debugPayload: payload,
    promptStats: payload?.promptStats ?? "",
    responseModelID: payload?.responseModelID ?? null,
    finishReason: payload?.finishReason ?? null,
    startedAt: startedAt ?? null,
    updatedAt: updatedAt ?? new Date().toISOString(),
    elapsedMS: computeElapsedMS(startedAt, updatedAt)
  };
}

function classifyAIError(error) {
  const message = String(error?.message ?? "");
  if (isAbortError(error)) {
    return "abort";
  }
  if (/timeout/i.test(message)) {
    return "timeout";
  }
  if (/not valid json|json/i.test(message)) {
    return "invalidJSON";
  }
  if (/invalid plan|planner output was rejected/i.test(message)) {
    return "invalidPlan";
  }
  if (/not configured|unavailable/i.test(message)) {
    return "notConfigured";
  }
  return "backend";
}

function detectInvalidResumePlan(result) {
  const plan = result?.presentationPlan ?? null;
  if (!plan) {
    return false;
  }
  const allowedKinds = new Set([
    "judgment",
    "basis",
    "gap",
    "nextMove",
    "evidence",
    "sources",
    "resolved",
    "questions",
    "principles",
    "risks",
    "contrast",
    "checklist"
  ]);
  const blocks = Array.isArray(plan.blocks) ? plan.blocks : [];
  return blocks.some((block) => {
    const kind = String(block?.kind ?? "");
    return !allowedKinds.has(kind) || (block?.items != null && !Array.isArray(block.items));
  });
}

function computeElapsedMS(startedAt, updatedAt = null) {
  if (!startedAt) {
    return null;
  }
  const start = new Date(startedAt).getTime();
  const end = new Date(updatedAt ?? Date.now()).getTime();
  if (Number.isNaN(start) || Number.isNaN(end)) {
    return null;
  }
  return Math.max(0, end - start);
}

function isAbortError(error) {
  return error?.name === "AbortError" || error?.message === "This operation was aborted";
}

function collectEntryTree(entries, rootID) {
  return entries.some((entry) => entry.id === rootID) ? [rootID] : [];
}

function mergeSourceMetadata(existing = null, interpretation = {}, attribution = {}) {
  return {
    ...(existing ?? {}),
    captureInterpretation: {
      ...(existing?.captureInterpretation ?? {}),
      explicitTag: interpretation?.explicitTag ?? null
    },
    kindAttribution: {
      ...(existing?.kindAttribution ?? {}),
      source: attribution?.source ?? existing?.kindAttribution?.source ?? "heuristic",
      confidence: attribution?.confidence ?? existing?.kindAttribution?.confidence ?? null,
      updatedAt: attribution?.updatedAt ?? new Date().toISOString()
    }
  };
}

function mergeKindAttributionMetadata(existing = null, attribution = {}) {
  return {
    ...(existing ?? {}),
    kindAttribution: {
      ...(existing?.kindAttribution ?? {}),
      source: attribution?.source ?? existing?.kindAttribution?.source ?? "heuristic",
      confidence: attribution?.confidence ?? existing?.kindAttribution?.confidence ?? null,
      updatedAt: attribution?.updatedAt ?? new Date().toISOString()
    }
  };
}

function createStatusMetadata({
  source = "heuristic",
  status = EntryStatus.OPEN,
  confidence = null,
  reason = "",
  updatedAt = new Date().toISOString()
} = {}) {
  return {
    source,
    status: normalizeEntryStatus(status),
    confidence: Number.isFinite(Number(confidence)) ? Number(confidence) : null,
    reason: String(reason ?? ""),
    updatedAt
  };
}

function mergeAIStatusMetadata(existing = null, attribution = {}) {
  return {
    ...(existing ?? {}),
    ...createStatusMetadata({
      source: "ai",
      status: attribution?.status ?? existing?.status ?? EntryStatus.OPEN,
      confidence: attribution?.confidence ?? existing?.confidence ?? null,
      reason: attribution?.reason ?? existing?.reason ?? "",
      updatedAt: attribution?.updatedAt ?? new Date().toISOString()
    })
  };
}

function mergeManualKindMetadata(existing = null, { kind, source = "manual", updatedAt = new Date().toISOString() } = {}) {
  const next = {
    ...(existing ?? {}),
    captureInterpretation: {
      ...(existing?.captureInterpretation ?? {}),
      explicitTag: null
    },
    kindAttribution: {
      ...(existing?.kindAttribution ?? {}),
      source,
      confidence: 1,
      updatedAt
    }
  };
  next.kindOverride = {
    source,
    updatedAt
  };
  return next;
}

function mergeManualStatusMetadata(existing = null, { status, source = "manual", updatedAt = new Date().toISOString() } = {}) {
  const nextStatus = normalizeEntryStatus(status);
  if (nextStatus === EntryStatus.OPEN) {
    return createStatusMetadata({
      source: "heuristic",
      status: EntryStatus.OPEN,
      confidence: 0.45,
      updatedAt
    });
  }
  return createStatusMetadata({
    ...(existing ?? {}),
    source,
    status: nextStatus,
    confidence: 1,
    updatedAt
  });
}

function shouldPreserveUserKind(entry) {
  return entry?.sourceMetadata?.kindOverride?.source === "manual";
}

function shouldPreserveUserStatus(entry) {
  return entry?.statusMetadata?.source === "manual" && normalizeEntryStatus(entry?.status) !== EntryStatus.OPEN;
}

function normalizeEntryKind(kind) {
  return normalizeEntryMode(kind);
}

function normalizeEntryStatus(status) {
  return Object.values(EntryStatus).includes(status) ? status : EntryStatus.OPEN;
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
  entryClassificationEntryIDs = new Set(),
  entryStatusClassificationEntryIDs = new Set(),
  routePlanningEntryIDs = new Set(),
  threadRefreshingThreadIDs = new Set()
} = {}) {
  return (entries ?? []).map((entry) => {
    const aiActivity = deriveEntryAIActivity(entry, {
      entryClassificationEntryIDs,
      entryStatusClassificationEntryIDs,
      routePlanningEntryIDs,
      threadRefreshingThreadIDs
    });
    return aiActivity ? { ...entry, aiActivity } : entry;
  });
}

function deriveEntryAIActivity(entry, { entryClassificationEntryIDs, entryStatusClassificationEntryIDs, routePlanningEntryIDs, threadRefreshingThreadIDs }) {
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
  if (entryClassificationEntryIDs.has(entry.id)) {
    return {
      visible: true,
      kind: "entryClassifying",
      label: "AI 正在判断笔记模式"
    };
  }
  if (entryStatusClassificationEntryIDs.has(entry.id)) {
    return {
      visible: true,
      kind: "entryStatusClassifying",
      label: "AI 正在判断工作状态"
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

function createEmptyAIEvidencePackage() {
  return {
    queryList: [],
    semanticDocs: [],
    episodicDocs: [],
    sourceDocs: [],
    rawDocs: [],
    activeClaims: [],
    judgmentBasis: "",
    keyEvidence: [],
    recentNotes: []
  };
}

function emptyStatusSummary() {
  return {
    decided: [],
    solved: [],
    verified: [],
    dropped: []
  };
}

function buildThreadAIQueries({ threadView, type = "resume" } = {}) {
  const latestAnchor = threadView?.anchors?.at?.(-1) ?? null;
  return dedupeStrings([
    threadView?.thread?.goalLayer?.goalStatement,
    threadView?.thread?.title,
    latestAnchor?.stateSummary,
    ...(latestAnchor?.openLoops ?? []).slice(0, 2),
    threadView?.aiSnapshot?.currentJudgment,
    type ? `${type} ${threadView?.thread?.title ?? ""}` : null
  ]).filter(Boolean);
}

function collectRankedThreadDocuments({
  retrievalEngine,
  store,
  threadID,
  queries = [],
  ownerTypes = [],
  recentRawDocuments = null
} = {}) {
  const ranked = new Map();
  for (const query of queries) {
    const results = retrievalEngine?.recall?.(query, {
      threadID,
      ownerTypes,
      limit: AI_CONTEXT_LIMITS.recallPerQuery
    }) ?? [];
    for (const result of results) {
      const existing = ranked.get(result.id);
      if (!existing || Number(result.score ?? 0) > Number(existing.score ?? 0)) {
        ranked.set(result.id, result);
      }
    }
  }
  const fallbackDocuments = Array.isArray(recentRawDocuments)
    ? recentRawDocuments.filter((item) => ownerTypes.length === 0 || ownerTypes.includes(item.ownerType))
    : collectRecentRawDocuments({ store, threadID, ownerTypes });
  for (const result of fallbackDocuments) {
    if (!ranked.has(result.id)) {
      ranked.set(result.id, result);
    }
  }
  return [...ranked.values()].sort((lhs, rhs) => {
    const scoreDiff = Number(rhs.score ?? 0) - Number(lhs.score ?? 0);
    if (scoreDiff !== 0) {
      return scoreDiff;
    }
    return new Date(rhs.createdAt ?? 0).getTime() - new Date(lhs.createdAt ?? 0).getTime();
  });
}

function collectRecentRawDocuments({ store, threadID, ownerTypes = [] } = {}) {
  if (!store || !threadID) {
    return [];
  }
  return store.fetchRetrievalDocumentsRecency({
    threadID,
    ownerTypes: ownerTypes.length > 0 ? ownerTypes : ["entry", "claim", "anchor"],
    limit: AI_CONTEXT_LIMITS.rawDocs * 2
  }).map((item) => ({
    ...item,
    score: Number(item.score ?? 1)
  }));
}

function splitRankedDocumentsByLayer(documents = []) {
  const grouped = {
    semantic: [],
    episodic: [],
    source: [],
    raw: []
  };
  for (const document of documents) {
    grouped[classifyRetrievalLayer(document)].push(document);
  }
  return grouped;
}

function classifyRetrievalLayer(document) {
  const meta = parseRetrievalMetadata(document?.metadataJSON);
  const scope = String(meta.scope ?? "").toLowerCase();
  const kind = String(meta.kind ?? "").toLowerCase();
  const status = String(meta.status ?? "").toLowerCase();
  if (scope === "semantic" || document?.ownerType === "claim" || isSettledStatus(status) || isSettledStatus(kind)) {
    return "semantic";
  }
  if (scope === "episodic" || document?.ownerType === "anchor") {
    return "episodic";
  }
  if (scope === "source" || kind === "source") {
    return "source";
  }
  return "raw";
}

function parseRetrievalMetadata(metadataJSON) {
  if (!metadataJSON) {
    return {};
  }
  try {
    return JSON.parse(metadataJSON) ?? {};
  } catch {
    return {};
  }
}

function isSettledStatus(value) {
  return SETTLED_AI_STATUSES.has(String(value ?? "").toLowerCase());
}

function takeDocuments(documents = [], limit = 0) {
  const seen = new Set();
  const result = [];
  for (const document of documents) {
    const key = `${document?.ownerType ?? "unknown"}:${document?.ownerID ?? document?.id ?? ""}`;
    if (!document?.id || seen.has(key)) {
      continue;
    }
    seen.add(key);
    result.push(document);
    if (result.length >= limit) {
      break;
    }
  }
  return result;
}

function documentToClaimText(document) {
  return documentToPromptText(document);
}

function documentToPromptText(document) {
  const body = String(document?.body ?? "").trim();
  if (body) {
    return body;
  }
  return String(document?.title ?? "").trim();
}

function documentToRecentNote(document) {
  const meta = parseRetrievalMetadata(document?.metadataJSON);
  return {
    id: document?.ownerID ?? document?.id ?? null,
    text: documentToPromptText(document),
    kind: meta.kind ?? document?.ownerType ?? EntryKind.NOTE
  };
}

function isEvidenceLikeDocument(document) {
  const meta = parseRetrievalMetadata(document?.metadataJSON);
  const kind = String(meta.kind ?? "").toLowerCase();
  return kind === "evidence" || kind === "source" || document?.ownerType === "anchor";
}

function dedupeStrings(values = []) {
  const seen = new Set();
  const result = [];
  for (const value of values) {
    const normalized = String(value ?? "").trim();
    if (!normalized || seen.has(normalized)) {
      continue;
    }
    seen.add(normalized);
    result.push(normalized);
  }
  return result;
}

function summarizeThreadEntryStatus(entries = []) {
  const grouped = emptyStatusSummary();
  for (const entry of entries) {
    const status = normalizeEntryStatus(entry?.status);
    if (status === EntryStatus.OPEN || !(status in grouped)) {
      continue;
    }
    grouped[status].push({
      id: entry.id,
      threadID: entry.threadID ?? null,
      kind: entry.kind ?? EntryKind.NOTE,
      status,
      summaryText: entry.summaryText ?? "",
      updatedAt: entry.statusMetadata?.updatedAt ?? entry.createdAt,
      source: entry.statusMetadata?.source ?? "heuristic"
    });
  }
  for (const key of Object.keys(grouped)) {
    grouped[key].sort((lhs, rhs) => new Date(rhs.updatedAt).getTime() - new Date(lhs.updatedAt).getTime());
  }
  return grouped;
}

function mapResumeStatusSummary(statusSummary = {}) {
  return {
    decided: mapResumeOutcomeItems(statusSummary.decided),
    solved: mapResumeOutcomeItems(statusSummary.solved),
    verified: mapResumeOutcomeItems(statusSummary.verified),
    dropped: mapResumeOutcomeItems(statusSummary.dropped)
  };
}

function mapResumeOutcomeItems(items = []) {
  return (items ?? []).map((item) => ({
    id: item.id,
    text: item.summaryText ?? "",
    kind: item.kind ?? EntryKind.NOTE,
    updatedAt: item.updatedAt ?? null,
    source: item.source ?? "heuristic"
  }));
}

function migrateLegacyReferenceEntries(entries, repository, knownEntryIDs = null) {
  const entryList = entries ?? [];
  const resolvedKnownEntryIDs = knownEntryIDs instanceof Set
    ? knownEntryIDs
    : new Set(entryList.map((entry) => entry.id).filter(Boolean));
  let changed = false;

  for (const entry of entryList) {
    const migrated = migrateLegacyReferenceEntry(entry, resolvedKnownEntryIDs);
    if (!migrated) {
      continue;
    }
    repository.store.upsertEntry(migrated);
    changed = true;
  }

  return changed;
}

function migrateLegacyReplyEntries(entries, repository) {
  const legacyReplies = (entries ?? []).filter((entry) => entry?.parentEntryID);
  if (legacyReplies.length === 0) {
    return {
      migratedReplyCount: 0,
      clearedParentCount: 0
    };
  }

  let migratedReplyCount = 0;
  let clearedParentCount = 0;

  for (const reply of legacyReplies) {
    const parent = (entries ?? []).find((entry) => entry.id === reply.parentEntryID) ?? null;
    const nextReferences = parent
      ? mergeReferenceBindings({
          existingReferences: reply.references ?? [],
          explicitReferences: [
            createDefaultReplyReference(parent, {
              label: deriveReferenceTargetLabel(parent)
            })
          ]
        })
      : (reply.references ?? []);
    const needsReferenceMigration = parent
      && !nextReferences.some((reference) =>
        (reference?.relationKind ?? null) === "responds-to" && (reference?.targetID ?? null) === parent.id
      );
    const needsParentClear = reply.parentEntryID != null;

    if (!needsParentClear && !needsReferenceMigration) {
      continue;
    }

    const updated = {
      ...reply,
      parentEntryID: null,
      references: nextReferences
    };
    repository.store.upsertEntry(updated);
    if (parent) {
      migratedReplyCount += 1;
    }
    if (needsParentClear) {
      clearedParentCount += 1;
    }
  }
  return {
    migratedReplyCount,
    clearedParentCount
  };
}

function createDefaultReplyReference(targetEntry, { label = "" } = {}) {
  return {
    id: `reply:${targetEntry.id}`,
    label: String(label ?? "").trim() || deriveReferenceTargetLabel(targetEntry),
    relationKind: "responds-to",
    targetKind: targetEntry?.kind ?? "unresolved",
    targetID: targetEntry?.id ?? null
  };
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

function buildEntryBody({ existingBody = null, text = "", attachments } = {}) {
  const normalizedText = String(text ?? "");
  const hasExplicitAttachments = Array.isArray(attachments);
  const nextAttachments = hasExplicitAttachments
    ? attachments.filter(Boolean)
    : Array.isArray(existingBody?.attachments)
      ? existingBody.attachments.filter(Boolean)
      : null;

  if (!existingBody && (!nextAttachments || nextAttachments.length === 0)) {
    return null;
  }

  const nextBody = {
    ...(existingBody ?? {}),
    text: normalizedText
  };

  if (nextAttachments && nextAttachments.length > 0) {
    nextBody.attachments = nextAttachments;
  } else {
    delete nextBody.attachments;
  }

  return nextBody;
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
  const merged = parsedReferences.map((reference) => {
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

  return dedupeReferenceBindings([...merged, ...pendingExplicit, ...pendingExisting]);
}

function referenceSignature(reference) {
  return `${String(reference?.label ?? "").trim().toLowerCase()}::${String(reference?.relationKind ?? DEFAULT_REFERENCE_RELATION).trim().toLowerCase()}`;
}

function dedupeReferenceBindings(references = []) {
  const seen = new Set();
  const result = [];

  for (const reference of references) {
    const key = [
      referenceSignature(reference),
      String(reference?.targetID ?? "").trim().toLowerCase()
    ].join("::");
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    result.push(reference);
  }

  return result;
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
