import path from "node:path";
import { CaptureInterpreter } from "../../domain/capture/captureInterpreter.js";
import { ThreadRoutingEngine } from "../../domain/routing/threadRoutingEngine.js";
import { deriveResources, resourceCounts } from "../../domain/resources/resourceDerivation.js";
import { createEntry, createThreadRecord, ThreadColorValues } from "../../domain/models/threadnoteModels.js";
import {
  DEFAULT_REFERENCE_RELATION,
  EXPLICIT_REFERENCE_RELATIONS,
  deriveReferenceTargetLabel,
  parseReferencesFromText
} from "../../domain/references/referenceSyntax.js";
import { ThreadStatus } from "../../domain/models/threadnoteModels.js";
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
    this.repository = null;
    this.snapshot = emptySnapshot();
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
    return {
      workspace: this.workspaceManager.describe(),
      home: this.homeView()
    };
  }

  homeView() {
    const resolvedEntries = resolveReferenceGraph(this.snapshot.entries);
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
    const routing = threadID ? { type: "route", threadID, reason: "Manually assigned" } : this.#routingEngine().decideFromInterpretation(interpretation);
    const entry = createEntry({
      threadID: routing.type === "route" ? routing.threadID : null,
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
    return {
      entry,
      routingDecision: routing
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
    const resolvedEntries = resolveReferenceGraph(this.snapshot.entries);
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
    await this.repository.deleteEntries(ids);
    await this.repository.flush();
    this.loadWorkspace();
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
    return this.aiProviderRuntime.configure(config);
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

  async saveAndPingAIProvider(config) {
    const saved = await this.configureAIProvider(config);
    try {
      const ping = await this.pingAIProvider();
      return { config: saved, ping };
    } catch (error) {
      return {
        config: saved,
        ping: {
          ok: false,
          text: error.message,
          backendLabel: this.aiProviderRuntime?.backendLabel ?? "Unconfigured provider",
          latencyMS: null,
          modelID: saved.model
        }
      };
    }
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
    try {
      const prepared = await this.aiService.prepareDraft({
        threadID,
        type,
        coreQuestion: threadView.thread.goalLayer.goalStatement,
        activeClaims: threadView.claims.map((claim) => claim.statement),
        openLoops: threadView.anchors.at(-1)?.openLoops ?? [],
        keyEvidence: threadView.entries
          .filter((entry) => entry.kind === "evidence")
          .slice(0, 5)
          .map((entry) => ({ id: entry.id, text: entry.summaryText })),
        recentNotes: threadView.entries.slice(-6).map((entry) => ({ id: entry.id, text: entry.summaryText }))
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
    }
  }

  #configureRepository(databasePath) {
    this.repository = this.repositoryFactory(path.resolve(databasePath));
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
          .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime())[0] ?? null
    });
  }

  #assertRepository() {
    if (!this.repository) {
      throw new Error("Workspace is not configured");
    }
  }
}

function capitalize(value) {
  const text = String(value ?? "");
  return text ? `${text[0].toUpperCase()}${text.slice(1)}` : "Prepare";
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
