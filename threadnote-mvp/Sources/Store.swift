import CryptoKit
import Foundation
import NaturalLanguage
import Observation

@MainActor
@Observable
final class ThreadnoteStore {
    private enum AIPromptBudget {
        static let defaultConcurrentRequests = 2
        static let routeCandidateLimit = 3
        static let resumeTokenBudget = 1200
        static let resumeMaxSnippetCount = 8
        static let resumeMaxSnippetCharacters = 360
        static let draftTokenBudget = 600
        static let draftMaxSnippetCount = 6
        static let draftMaxSnippetCharacters = 280
    }

    private struct RouteConnectivitySnapshot {
        let status: String
        let message: String
        let checkedAt: Date
    }

    // Runtime dependencies and derived caches are mutated during reads;
    // keep them out of Observation to avoid self-triggered redraws.
    @ObservationIgnored
    private let currentSampleDataVersion = 9
    @ObservationIgnored
    private let deterministicAI = DeterministicAIHelper()
    @ObservationIgnored
    private let captureInterpreter = CaptureInterpreter()
    @ObservationIgnored
    private let aiQueue = AITaskQueue(maxConcurrent: AIPromptBudget.defaultConcurrentRequests)

    var threads: [ThreadRecord] = []
    var entries: [Entry] = []
    var claims: [Claim] = []
    var anchors: [Anchor] = []
    var tasks: [ThreadTask] = []
    var discourseRelations: [DiscourseRelation] = []

    var selectedHomeSurface: HomeSurface = .inbox
    var selectedThreadID: UUID?
    var selectedSourceEntryID: UUID?
    var threadSidebar = ThreadSidebarState()
    var preparedView: PreparedView?
    var preparedViewType: PreparedViewType = .writing
    var threadCreationContext: ThreadCreationContext?
    var quickCaptureDraft = QuickCaptureDraft()
    var inlineNoteDraft = ""
    var replyDrafts: [UUID: String] = [:]
    var expandedReplyEntryIDs: Set<UUID> = []

    @ObservationIgnored
    private var repository: ThreadnoteRepository?
    @ObservationIgnored
    private(set) var retrievalEngine: RetrievalEngine?
    @ObservationIgnored
    private(set) var threadRoutingEngine: ThreadRoutingEngine?
    @ObservationIgnored
    private var memoryPipeline: MemoryPipeline?
    @ObservationIgnored
    private var activeThreadSessionID = UUID()
    @ObservationIgnored
    private var activeThreadSessionThreadID: UUID?
    @ObservationIgnored
    private var threadStateCache: [UUID: ThreadState] = [:]
    @ObservationIgnored
    private var threadStateRevision = 0
    @ObservationIgnored
    private var routeDebugCache: [UUID: RouteDebugState] = [:]
    @ObservationIgnored
    private var routeDebugRevision = 0
    @ObservationIgnored
    private var entriesByThreadCache: [UUID: [Entry]] = [:]
    @ObservationIgnored
    private var visibleEntriesCache: [UUID: [Entry]] = [:]
    @ObservationIgnored
    private var topLevelEntriesCache: [UUID: [Entry]] = [:]
    @ObservationIgnored
    private var repliesCache: [UUID: [Entry]] = [:]
    @ObservationIgnored
    private var resourceItemsCache: [UUID: [ResourceItem]] = [:]
    @ObservationIgnored
    private var allResourcesCache: [ResourceItem]?
    @ObservationIgnored
    private var pendingMetadataEntryIDs: Set<UUID> = []

    @ObservationIgnored
    let linkMetadataService = LinkMetadataService()

    var resumeSynthesisProcessingThreadID: UUID?
    var draftPreparationProcessingThreadID: UUID?
    var routePlanningProcessingEntryIDs: Set<UUID> = []
    @ObservationIgnored
    private(set) var llmProvider: (any AIBackendClient)?
    @ObservationIgnored
    private var resumeSynthesisTasks: [UUID: Task<Void, Never>] = [:]
    @ObservationIgnored
    private var resumeSynthesisTaskTokens: [UUID: UUID] = [:]
    @ObservationIgnored
    private var draftPreparationTasks: [UUID: Task<Void, Never>] = [:]
    @ObservationIgnored
    private var draftPreparationTaskTokens: [UUID: UUID] = [:]
    @ObservationIgnored
    private var routePlanningTasks: [UUID: Task<Void, Never>] = [:]
    @ObservationIgnored
    private var routePlanningTaskTokens: [UUID: UUID] = [:]
    @ObservationIgnored
    private var backgroundSweepTask: Task<Void, Never>?
    @ObservationIgnored
    private var sweepDebounceTask: Task<Void, Never>?
    @ObservationIgnored
    private var idleSweepTask: Task<Void, Never>?
    @ObservationIgnored
    private var isRunningBackgroundSweep = false

    init(enableLLM: Bool = true) {
        startIdleSweepTimer()
        if enableLLM {
            configureLLM()
            NotificationCenter.default.addObserver(
                forName: .aiSettingsChanged,
                object: nil,
                queue: .main
            ) { [weak self] _ in
                Task { @MainActor in
                    self?.configureLLM()
                }
            }
        }
    }

    deinit {
        sweepDebounceTask?.cancel()
        backgroundSweepTask?.cancel()
        idleSweepTask?.cancel()
    }

    private func logRoute(_ message: String) {
        print("[RouteAI] \(message)")
    }

    /// Called once workspace is ready. Creates repository, loads data.
    func configure(with databaseURL: URL) {
        do {
            let repository = try ThreadnoteRepository(databaseURL: databaseURL)
            self.repository = repository
            retrievalEngine = repository.retrievalEngine
            threadRoutingEngine = ThreadRoutingEngine(
                retrievalEngine: repository.retrievalEngine,
                threadsProvider: { [weak self] in
                    self?.homeThreads ?? []
                },
                entriesProvider: { [weak self] threadID in
                    self?.visibleEntries(for: threadID) ?? []
                },
                claimsProvider: { [weak self] threadID in
                    self?.claims(for: threadID) ?? []
                },
                latestAnchorProvider: { [weak self] threadID in
                    self?.latestAnchor(for: threadID)
                },
                captureInterpreter: captureInterpreter
            )
            memoryPipeline = MemoryPipeline(repository: repository)
            load()
            if threads.isEmpty {
                seed()
            } else {
                queueMetadataEnrichmentForLoadedEntries()
            }
        } catch {
            print("[Store] Failed to configure repository: \(error)")
        }
    }

    private func configureLLM() {
        let provider = LLMProvider()
        do {
            try provider.configure()
            llmProvider = provider.isConfigured ? provider : nil
            if let llmProvider {
                logRoute("Configured backend: \(llmProvider.backendLabel)")
            } else {
                logRoute("No backend configured for routing.")
            }
        } catch {
            llmProvider = nil
            logRoute("Failed to configure backend: \(error.localizedDescription)")
        }
        updateAIQueueConcurrency()
        invalidateAIOutputState()
        invalidateRouteAnalysisForInboxEntries()
        scheduleSweep(delay: .seconds(1))
    }

    func setAIBackendForTesting(_ backend: (any AIBackendClient)?) {
        llmProvider = backend
        updateAIQueueConcurrency()
        invalidateAIOutputState()
        invalidateRouteAnalysisForInboxEntries()
        scheduleSweep(delay: .seconds(1))
    }

    private func updateAIQueueConcurrency() {
        let concurrency = llmProvider?.preferredMaxConcurrentRequests ?? AIPromptBudget.defaultConcurrentRequests
        Task {
            await aiQueue.setMaxConcurrent(concurrency)
        }
    }

    private func invalidateAIOutputState() {
        for task in resumeSynthesisTasks.values {
            task.cancel()
        }
        resumeSynthesisTasks.removeAll()
        resumeSynthesisTaskTokens.removeAll()
        resumeSynthesisProcessingThreadID = nil

        for task in draftPreparationTasks.values {
            task.cancel()
        }
        draftPreparationTasks.removeAll()
        draftPreparationTaskTokens.removeAll()
        draftPreparationProcessingThreadID = nil

        threadStateCache.removeAll()
        bumpThreadStateRevision()
        preparedView = nil
    }

    private func invalidateDerivedState(
        for threadIDs: Set<UUID> = [],
        replyParents: Set<UUID> = []
    ) {
        if threadIDs.isEmpty {
            entriesByThreadCache.removeAll()
            visibleEntriesCache.removeAll()
            topLevelEntriesCache.removeAll()
            resourceItemsCache.removeAll()
            threadStateCache.removeAll()
            bumpThreadStateRevision()
            resetRouteAnalysis()
            allResourcesCache = nil
        } else {
            for threadID in threadIDs {
                entriesByThreadCache.removeValue(forKey: threadID)
                visibleEntriesCache.removeValue(forKey: threadID)
                topLevelEntriesCache.removeValue(forKey: threadID)
                resourceItemsCache.removeValue(forKey: threadID)
                threadStateCache.removeValue(forKey: threadID)
                if preparedView?.threadID == threadID {
                    preparedView = nil
                }
            }
            if !threadIDs.isEmpty {
                bumpThreadStateRevision()
            }
            invalidateRouteAnalysisForInboxEntries()
            allResourcesCache = nil
        }

        for parentID in replyParents {
            repliesCache.removeValue(forKey: parentID)
        }
    }

    private func clearRouteAnalysis(for entryIDs: Set<UUID>) {
        guard !entryIDs.isEmpty else { return }
        for entryID in entryIDs {
            routePlanningTasks.removeValue(forKey: entryID)?.cancel()
            routePlanningTaskTokens.removeValue(forKey: entryID)
            routeDebugCache.removeValue(forKey: entryID)
            routePlanningProcessingEntryIDs.remove(entryID)
        }
        routeDebugRevision &+= 1
    }

    private func invalidateRouteAnalysisForInboxEntries() {
        clearRouteAnalysis(for: Set(entries.lazy.filter { $0.threadID == nil }.map(\.id)))
    }

    private func resetRouteAnalysis() {
        for task in routePlanningTasks.values {
            task.cancel()
        }
        routePlanningTasks.removeAll()
        routePlanningTaskTokens.removeAll()
        routePlanningProcessingEntryIDs.removeAll()
        routeDebugCache.removeAll()
        routeDebugRevision &+= 1
    }

    private func bumpThreadStateRevision() {
        threadStateRevision &+= 1
    }

    func scheduleSweep(delay: Duration = .seconds(2)) {
        sweepDebounceTask?.cancel()
        sweepDebounceTask = Task { [weak self] in
            try? await Task.sleep(for: delay)
            guard !Task.isCancelled else { return }
            await self?.runBackgroundSweep()
        }
    }

    private func startIdleSweepTimer() {
        idleSweepTask?.cancel()
        idleSweepTask = Task { [weak self] in
            while !Task.isCancelled {
                try? await Task.sleep(for: .seconds(300))
                guard !Task.isCancelled else { return }
                self?.scheduleSweep(delay: .zero)
            }
        }
    }

    private func runBackgroundSweep() async {
        guard !isRunningBackgroundSweep else { return }
        guard await aiQueue.queueDepth <= 10 else { return }

        isRunningBackgroundSweep = true
        defer { isRunningBackgroundSweep = false }

        let unroutedEntries = entries.filter {
            $0.threadID == nil && !routePlanningProcessingEntryIDs.contains($0.id)
        }
        for entry in unroutedEntries {
            ensureRoutePlanning(for: entry, autoRoute: true)
        }

        guard repository != nil, llmProvider != nil else { return }

        for thread in homeThreads {
            guard resumeSynthesisTaskTokens[thread.id] == nil else { continue }
            let fingerprint = contentFingerprint(for: thread.id)
            let snapshot = repository?.fetchAISnapshot(for: thread.id)
            guard snapshot?.contentFingerprint != fingerprint else { continue }

            threadStateCache.removeValue(forKey: thread.id)
            bumpThreadStateRevision()
            _ = threadState(for: thread.id)
        }
    }

    private func persistThread(_ threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        repository?.upsertThread(thread)
    }

    private func persistEntry(_ entry: Entry) {
        repository?.upsertEntry(entry)
    }

    private func persistClaim(_ claim: Claim) {
        repository?.upsertClaim(claim)
    }

    private func persistAnchor(_ anchor: Anchor) {
        repository?.upsertAnchor(anchor)
    }

    private func persistDiscourseRelations(for threadID: UUID) {
        let threadEntryIDs = Set(visibleEntries(for: threadID).map(\.id))
        let threadRelations = discourseRelations
            .filter { threadEntryIDs.contains($0.sourceEntryID) && threadEntryIDs.contains($0.targetEntryID) }
        repository?.replaceDiscourseRelations(
            removingRelationsTouching: threadEntryIDs,
            with: threadRelations
        )
    }

    private func cancelAIWork(for threadID: UUID) {
        resumeSynthesisTasks.removeValue(forKey: threadID)?.cancel()
        resumeSynthesisTaskTokens.removeValue(forKey: threadID)
        draftPreparationTasks.removeValue(forKey: threadID)?.cancel()
        draftPreparationTaskTokens.removeValue(forKey: threadID)
        if resumeSynthesisProcessingThreadID == threadID {
            resumeSynthesisProcessingThreadID = nil
        }
        if draftPreparationProcessingThreadID == threadID {
            draftPreparationProcessingThreadID = nil
        }
    }

    private func queueMetadataEnrichmentIfNeeded(for entry: Entry) {
        guard let urlString = entry.body.url,
              !urlString.isEmpty,
              URL(string: urlString) != nil,
              entry.body.linkMeta == nil,
              pendingMetadataEntryIDs.insert(entry.id).inserted else { return }

        linkMetadataService.fetchIfNeeded(urlString) { [weak self] meta in
            guard let self else { return }
            self.pendingMetadataEntryIDs.remove(entry.id)
            guard let index = self.entries.firstIndex(where: { $0.id == entry.id }) else { return }
            guard self.entries[index].body.linkMeta == nil else { return }

            self.entries[index].body.linkMeta = meta
            let updatedEntry = self.entries[index]
            if let threadID = updatedEntry.threadID {
                self.invalidateDerivedState(for: [threadID])
            }
            self.persistEntry(updatedEntry)
        }
    }

    private func queueMetadataEnrichmentForLoadedEntries() {
        for entry in entries where entry.body.url != nil && entry.body.linkMeta == nil {
            queueMetadataEnrichmentIfNeeded(for: entry)
        }
    }

    // MARK: - Navigation

    var isInWorkbench: Bool { selectedThreadID != nil }

    var selectedThread: ThreadRecord? {
        guard let selectedThreadID else { return nil }
        return threads.first(where: { $0.id == selectedThreadID })
    }

    var selectedSourceEntry: Entry? {
        guard let selectedSourceEntryID else { return nil }
        return sourceEntry(id: selectedSourceEntryID)
    }

    func goToStream() {
        if let tid = selectedThreadID {
            maybeWriteAnchorIfNeeded(for: tid)
        }
        selectedHomeSurface = .inbox
        selectedThreadID = nil
        selectedSourceEntryID = nil
        resetThreadSidebar()
        preparedView = nil
        activeThreadSessionThreadID = nil
    }

    func openThread(_ threadID: UUID) {
        let isSwitchingThread = selectedThreadID != threadID
        if let current = selectedThreadID, current != threadID {
            maybeWriteAnchorIfNeeded(for: current)
            cancelAIWork(for: current)
            threadStateCache.removeValue(forKey: current)
            bumpThreadStateRevision()
        }
        selectedHomeSurface = .inbox
        selectedThreadID = threadID
        selectedSourceEntryID = nil
        if isSwitchingThread {
            resetThreadSidebar()
        }
        beginThreadSession(for: threadID)
        if preparedView?.threadID != threadID {
            preparedView = nil
        }
    }

    func setThreadStatus(_ threadID: UUID, to status: ThreadStatus) {
        guard let index = threads.firstIndex(where: { $0.id == threadID }) else { return }
        threads[index].status = status
        threads[index].updatedAt = .now

        if status == .archived {
            if selectedThreadID == threadID {
                goToStream()
            }
            if preparedView?.threadID == threadID {
                preparedView = nil
            }
        }

        invalidateDerivedState(for: [threadID])
        persistThread(threadID)
    }

    func archiveThread(_ threadID: UUID) {
        setThreadStatus(threadID, to: .archived)
    }

    func restoreThread(_ threadID: UUID) {
        setThreadStatus(threadID, to: .active)
    }

    func openResources() {
        if let tid = selectedThreadID {
            maybeWriteAnchorIfNeeded(for: tid)
        }
        selectedHomeSurface = .resources
        selectedThreadID = nil
        selectedSourceEntryID = nil
        resetThreadSidebar()
        preparedView = nil
        activeThreadSessionThreadID = nil
    }

    // MARK: - Stream computed properties

    var streamEntries: [Entry] {
        entries
            .filter { isUserVisible($0) && $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
    }

    var inboxEntries: [Entry] {
        entries
            .filter { $0.threadID == nil && isUserVisible($0) && $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
    }

    var homeThreads: [ThreadRecord] {
        threads
            .filter { $0.status == .active }
            .sorted { $0.lastActiveAt > $1.lastActiveAt }
    }

    func thread(for entry: Entry) -> ThreadRecord? {
        guard let threadID = entry.threadID else { return nil }
        return threads.first(where: { $0.id == threadID })
    }

    // MARK: - Completion Provider Data

    var uniqueObjectNames: [String] {
        var seen = Set<String>()
        var result: [String] = []
        for entry in entries {
            for mention in entry.objectMentions {
                let key = mention.name.lowercased()
                if seen.insert(key).inserted {
                    result.append(mention.name)
                }
            }
        }
        return result
    }

    var referenceCompletionCandidates: [(String, String, String)] {
        entries
            .filter { isUserVisible($0) && $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
            .prefix(20)
            .map { ($0.summaryText, $0.kind.title, "note.text") }
    }

    func sourceEntry(id: UUID) -> Entry? {
        entries.first(where: { $0.id == id && $0.isSourceResource })
    }

    func openSource(_ entryID: UUID) {
        guard sourceEntry(id: entryID) != nil else { return }
        selectedSourceEntryID = entryID
    }

    func closeSource() {
        selectedSourceEntryID = nil
    }

    func openThreadSidebar(_ role: ThreadSidebarTabRole, for threadID: UUID? = nil) {
        guard let targetThreadID = threadID ?? selectedThreadID,
              threads.contains(where: { $0.id == targetThreadID }) else { return }

        if selectedThreadID != targetThreadID {
            openThread(targetThreadID)
        }

        threadSidebar.open(role: role)
    }

    func toggleThreadSidebar(_ tab: ThreadSidebarTab, for threadID: UUID? = nil) {
        guard let targetThreadID = threadID ?? selectedThreadID,
              threads.contains(where: { $0.id == targetThreadID }) else { return }

        if selectedThreadID != targetThreadID {
            openThread(targetThreadID)
        }

        if threadSidebar.isPresented, threadSidebar.selectedTabID == tab.id {
            threadSidebar.close()
        } else {
            threadSidebar.select(tabID: tab.id)
        }
    }

    func selectThreadSidebarTab(_ tabID: String) {
        threadSidebar.select(tabID: tabID)
    }

    func closeThreadSidebar() {
        threadSidebar.close()
    }

    private func resetThreadSidebar() {
        threadSidebar.reset()
    }

    func resourceItems(for threadID: UUID) -> [ResourceItem] {
        if let cached = resourceItemsCache[threadID] {
            return cached
        }
        let derived = ResourceDerivation.derive(from: topLevelEntries(for: threadID))
        resourceItemsCache[threadID] = derived
        return derived
    }

    func resourceItems(for threadID: UUID, kind: ResourceKind) -> [ResourceItem] {
        resourceItems(for: threadID).filter { $0.kind == kind }
    }

    func resourceCounts(for threadID: UUID) -> ResourceCounts {
        ResourceDerivation.counts(from: resourceItems(for: threadID))
    }

    var resourceThreads: [ThreadRecord] {
        threads
            .filter { !resourceItems(for: $0.id).isEmpty }
            .sorted { $0.lastActiveAt > $1.lastActiveAt }
    }

    var allResources: [ResourceItem] {
        if let cached = allResourcesCache {
            return cached
        }
        let derived = resourceThreads
            .flatMap { resourceItems(for: $0.id) }
            .sorted { $0.createdAt > $1.createdAt }
        allResourcesCache = derived
        return derived
    }

    var allResourceCounts: ResourceCounts {
        ResourceDerivation.counts(from: allResources)
    }

    // MARK: - Thread queries

    func entries(for threadID: UUID) -> [Entry] {
        if let cached = entriesByThreadCache[threadID] {
            return cached
        }
        let derived = entries
            .filter { $0.threadID == threadID }
            .sorted { $0.createdAt > $1.createdAt }
        entriesByThreadCache[threadID] = derived
        return derived
    }

    func visibleEntries(for threadID: UUID) -> [Entry] {
        if let cached = visibleEntriesCache[threadID] {
            return cached
        }
        let derived = entries(for: threadID).filter(isUserVisible)
        visibleEntriesCache[threadID] = derived
        return derived
    }

    func topLevelEntries(for threadID: UUID) -> [Entry] {
        if let cached = topLevelEntriesCache[threadID] {
            return cached
        }
        let derived = visibleEntries(for: threadID)
            .filter { $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
        topLevelEntriesCache[threadID] = derived
        return derived
    }

    func replies(for entryID: UUID) -> [Entry] {
        if let cached = repliesCache[entryID] {
            return cached
        }
        let derived = entries
            .filter { $0.parentEntryID == entryID && isUserVisible($0) }
            .sorted { $0.createdAt < $1.createdAt }
        repliesCache[entryID] = derived
        return derived
    }

    func discourseRelations(for threadID: UUID) -> [DiscourseRelation] {
        let ids = Set(visibleEntries(for: threadID).map(\.id))
        return discourseRelations
            .filter { ids.contains($0.sourceEntryID) && ids.contains($0.targetEntryID) }
            .sorted { $0.confidence > $1.confidence }
    }

    func relatedEntries(for entryID: UUID, in threadID: UUID) -> [Entry] {
        let ids = discourseRelations(for: threadID)
            .filter { $0.sourceEntryID == entryID || $0.targetEntryID == entryID }
            .flatMap { relation in
                [relation.sourceEntryID, relation.targetEntryID].filter { $0 != entryID }
            }
        let unique = Array(NSOrderedSet(array: ids)) as? [UUID] ?? []
        return unique.compactMap { id in entries.first(where: { $0.id == id }) }
    }

    func referencedEntries(for entry: Entry) -> [Entry] {
        entry.references
            .filter { $0.targetKind == .note }
            .compactMap { reference in
                guard let targetID = reference.targetID else { return nil }
                return entries.first(where: { $0.id == targetID && isUserVisible($0) })
            }
    }

    func referencedThreads(for entry: Entry) -> [ThreadRecord] {
        entry.references
            .filter { $0.targetKind == .thread }
            .compactMap { reference in
                guard let targetID = reference.targetID else { return nil }
                return threads.first(where: { $0.id == targetID })
            }
    }

    func unresolvedReferences(for entry: Entry) -> [EntryReference] {
        entry.references.filter { $0.targetKind == .unresolved || $0.targetID == nil }
    }

    func claims(for threadID: UUID) -> [Claim] {
        claims
            .filter { $0.threadID == threadID && $0.status != .superseded }
            .sorted { $0.updatedAt > $1.updatedAt }
    }

    func setClaimStatus(_ claimID: UUID, to status: ClaimStatus) {
        guard let index = claims.firstIndex(where: { $0.id == claimID }) else { return }
        claims[index].status = status
        claims[index].updatedAt = .now
        let updated = claims[index]
        invalidateDerivedState(for: [updated.threadID])
        persistClaim(updated)
        memoryPipeline?.recordSemantic(claim: updated)
    }

    func tasks(for threadID: UUID) -> [ThreadTask] {
        tasks
            .filter { $0.threadID == threadID && $0.status != "done" }
            .sorted { $0.updatedAt > $1.updatedAt }
    }

    func needsThread(_ entry: Entry) -> Bool {
        entry.threadID == nil
    }

    func memoryRecords(for threadID: UUID, scope: MemoryScope? = nil) -> [MemoryRecord] {
        repository?.fetchMemoryRecords(for: threadID, scope: scope) ?? []
    }

    // MARK: - Anchor

    func latestAnchor(for threadID: UUID) -> Anchor? {
        anchors
            .filter { $0.threadID == threadID }
            .max(by: { $0.createdAt < $1.createdAt })
    }

    func deltaEntries(for threadID: UUID) -> [Entry] {
        let visible = topLevelEntries(for: threadID)
        guard let anchor = latestAnchor(for: threadID) else {
            return visible
        }
        return visible.filter { $0.createdAt > anchor.createdAt }
    }

    func newEntryCount(for threadID: UUID) -> Int {
        deltaEntries(for: threadID).count
    }

    // MARK: - Thread state

    func threadState(for threadID: UUID) -> ThreadState? {
        _ = threadStateRevision
        if let cached = threadStateCache[threadID] {
            return cached
        }
        guard let thread = threads.first(where: { $0.id == threadID }) else { return nil }
        let anchor = latestAnchor(for: threadID)
        let threadClaims = claims(for: threadID)
        let visible = visibleEntries(for: threadID)
        let topLevel = topLevelEntries(for: threadID)
        let relations = discourseRelations(for: threadID)
        let input = buildThreadStateInput(
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            visibleEntries: visible,
            topLevelEntries: topLevel,
            claims: threadClaims,
            anchor: anchor,
            relations: relations
        )
        let deterministicSnapshot = deterministicAI.synthesizeThreadState(input: input)
        let fingerprint = contentFingerprint(for: threadID)

        if let saved = repository?.fetchAISnapshot(for: threadID),
           saved.contentFingerprint == fingerprint,
           let cachedState = buildInitialThreadState(
               for: threadID,
               thread: thread,
               anchor: anchor,
               threadClaims: threadClaims,
               topLevel: topLevel,
               relations: relations,
               deterministicSnapshot: deterministicSnapshot,
               contentState: AIContentState(status: .ready, message: "Loaded from cache"),
               aiDebug: cachedResumeAIDebugState(modelID: saved.modelID, synthesizedAt: saved.synthesizedAt),
               presentation: deserializePresentation(from: saved),
               restartNote: saved.restartNote,
               currentJudgment: saved.currentJudgment,
               openLoops: decodeStableJSON(saved.openLoopsJSON, as: [String].self) ?? deterministicSnapshot.openLoops,
               nextAction: saved.nextAction,
               recoveryLines: decodeStableJSON(saved.recoveryLinesJSON, as: [ResumeRecoveryLine].self) ?? deterministicSnapshot.recoveryLines
           ) {
            threadStateCache[threadID] = cachedState
            bumpThreadStateRevision()
            return cachedState
        }

        let request = deterministicAI.resumeRequest(from: deterministicSnapshot, input: input)
        let contentState = llmProvider.map {
            loadingAIContentState(
                feature: "Restart Note",
                backendLabel: $0.backendLabel
            )
        } ?? notConfiguredAIContentState(feature: "Restart Note")
        guard let state = buildInitialThreadState(
            for: threadID,
            thread: thread,
            anchor: anchor,
            threadClaims: threadClaims,
            topLevel: topLevel,
            relations: relations,
            deterministicSnapshot: deterministicSnapshot,
            contentState: contentState,
            aiDebug: initialResumeAIDebugState(),
            presentation: .empty,
            restartNote: "",
            currentJudgment: "",
            openLoops: [],
            nextAction: nil,
            recoveryLines: []
        ) else {
            return nil
        }
        threadStateCache[threadID] = state
        synthesizeThreadState(
            threadID: threadID,
            input: input,
            snapshot: deterministicSnapshot,
            request: request
        )
        return state
    }

    private func buildInitialThreadState(
        for threadID: UUID,
        thread: ThreadRecord,
        anchor: Anchor?,
        threadClaims: [Claim],
        topLevel: [Entry],
        relations: [DiscourseRelation],
        deterministicSnapshot: ThreadStateSnapshot,
        contentState: AIContentState,
        aiDebug: ThreadAIDebugState,
        presentation: ThreadPresentation,
        restartNote: String,
        currentJudgment: String,
        openLoops: [String],
        nextAction: String?,
        recoveryLines: [ResumeRecoveryLine]
    ) -> ThreadState? {
        let sections = streamSections(for: threadID, topLevelEntries: topLevel, relations: relations)
        let questions = topLevel.filter { $0.kind == .question }
        let evidence = topLevel.filter { $0.kind == .evidence }
        let sources = topLevel.filter { $0.kind == .source }

        return ThreadState(
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            contentState: contentState,
            presentation: presentation,
            aiDebug: aiDebug,
            restartNote: restartNote,
            currentJudgment: currentJudgment,
            judgmentBasis: deterministicSnapshot.judgmentBasis,
            openLoops: openLoops,
            nextAction: nextAction,
            recoveryLines: recoveryLines,
            resolvedSoFar: deterministicSnapshot.resolvedSoFar,
            recentChanges: recentChanges(for: threadID),
            keyAnchors: keyAnchors(for: threadClaims, evidence: evidence, sources: sources),
            claimCount: threadClaims.count,
            evidenceCount: evidence.count,
            sourceCount: sources.count,
            keyQuestions: Array(questions.prefix(3)),
            keyClaims: Array(threadClaims.prefix(3)),
            supportingEvidence: Array(evidence.prefix(3)),
            recentSources: Array(sources.prefix(3)),
            streamSections: sections,
            lastAnchorID: anchor?.id,
            lastAnchorAt: anchor?.createdAt
        )
    }

    private func deserializePresentation(from snapshot: ThreadAISnapshotRow) -> ThreadPresentation {
        ThreadPresentation(
            headline: snapshot.headline,
            blocks: decodeStableJSON(snapshot.blocksJSON, as: [ThreadBlock].self) ?? [],
            primaryAction: snapshot.nextAction
        )
    }

    private func cachedResumeAIDebugState(
        modelID: String,
        synthesizedAt: Date
    ) -> ThreadAIDebugState {
        ThreadAIDebugState(
            status: .applied,
            backendLabel: modelID.isEmpty ? "Cached AI snapshot" : "Cached AI snapshot",
            configuredModelID: modelID.isEmpty ? nil : modelID,
            responseModelID: modelID.isEmpty ? nil : modelID,
            responseID: nil,
            finishReason: "cache_hit",
            warnings: [],
            message: "Loaded cached AI cockpit synthesized at \(synthesizedAt.formatted(date: .abbreviated, time: .shortened)).",
            promptStats: nil,
            parsedResponse: nil,
            rawResponseBody: nil,
            updatedAt: synthesizedAt
        )
    }

    // MARK: - Capture

    func beginCapture(prefill: String = "") {
        quickCaptureDraft = QuickCaptureDraft(text: prefill)
    }

    func submitCapture() {
        let text = quickCaptureDraft.text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let parsed = parseCaptureInput(text)
        let entry = makeEntry(from: parsed, threadID: nil)
        append(entry)
        quickCaptureDraft = QuickCaptureDraft()
    }

    func appendInlineNote(to threadID: UUID) {
        let text = inlineNoteDraft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let parsed = parseCaptureInput(text)
        let entry = makeEntry(from: parsed, threadID: threadID)
        append(entry)
        inlineNoteDraft = ""
    }

    func appendReply(to entryID: UUID) {
        guard let parent = entries.first(where: { $0.id == entryID }) else { return }
        let text = replyDrafts[entryID, default: ""].trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let parsed = parseCaptureInput(text)
        let reply = makeEntry(from: parsed, threadID: parent.threadID, parentEntryID: entryID)
        append(reply)
        replyDrafts[entryID] = ""
        expandedReplyEntryIDs.insert(entryID)
    }

    func toggleReplies(for entryID: UUID) {
        if expandedReplyEntryIDs.contains(entryID) {
            expandedReplyEntryIDs.remove(entryID)
        } else {
            expandedReplyEntryIDs.insert(entryID)
        }
    }

    func beginThreadCreation(seedText: String = "", sourceEntryID: UUID? = nil) {
        let suggestedTitle = suggestThreadTitle(for: seedText)
        let suggestedType = suggestGoalType(for: seedText.isEmpty ? suggestedTitle : seedText)
        threadCreationContext = ThreadCreationContext(
            editingThreadID: nil,
            sourceEntryID: sourceEntryID,
            suggestedTitle: suggestedTitle,
            seedGoalStatement: seedText,
            suggestedGoalType: suggestedType,
            successCondition: "",
            currentStage: .framing
        )
    }

    func beginGoalEditing(for threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        threadCreationContext = ThreadCreationContext(
            editingThreadID: threadID,
            sourceEntryID: nil,
            suggestedTitle: thread.title,
            seedGoalStatement: thread.goalLayer.goalStatement,
            suggestedGoalType: thread.goalLayer.goalType,
            successCondition: thread.goalLayer.successCondition,
            currentStage: thread.goalLayer.currentStage
        )
    }

    func dismissThreadCreation() {
        threadCreationContext = nil
    }

    func completeThreadCreation(
        title: String,
        goalType: ThreadGoalType
    ) {
        guard let context = threadCreationContext else { return }
        let cleanTitle = title.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !cleanTitle.isEmpty else { return }
        let resolvedTitle = cleanTitle
        let resolvedSuccess = context.successCondition.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
            ? "Reach a clear enough conclusion to continue from this thread later."
            : context.successCondition.trimmingCharacters(in: .whitespacesAndNewlines)
        let resolvedStage = context.editingThreadID == nil ? .framing : context.currentStage
        let goalLayer = ThreadGoalLayer(
            goalStatement: resolvedTitle,
            goalType: goalType,
            successCondition: resolvedSuccess,
            currentStage: resolvedStage
        )

        if let editingThreadID = context.editingThreadID {
            guard let index = threads.firstIndex(where: { $0.id == editingThreadID }) else { return }
            threads[index].title = resolvedTitle
            threads[index].prompt = resolvedTitle
            threads[index].goalLayer = goalLayer
            threads[index].updatedAt = .now
            threads[index].lastActiveAt = .now
        } else {
            let thread = ThreadRecord(
                id: UUID(),
                title: resolvedTitle,
                prompt: resolvedTitle,
                goalLayer: goalLayer,
                status: .active,
                createdAt: .now,
                updatedAt: .now,
                lastActiveAt: .now,
                color: ThreadColor.allCases[threads.count % ThreadColor.allCases.count]
            )
            threads.insert(thread, at: 0)
            if let sourceEntryID = context.sourceEntryID {
                setEntryThread(sourceEntryID, to: thread.id)
            }
            openThread(thread.id)
        }

        threadCreationContext = nil
        if let editingThreadID = context.editingThreadID {
            invalidateDerivedState(for: [editingThreadID])
            persistThread(editingThreadID)
        } else if let createdThreadID = threads.first?.id {
            invalidateDerivedState(for: [createdThreadID])
            persistThread(createdThreadID)
        }
    }

    func createThread(title: String? = nil, seedText: String = "") {
        let sourceText = seedText.isEmpty ? (title ?? "A new problem worth returning to.") : seedText
        let newTitle = title ?? suggestThreadTitle(for: sourceText)
        let thread = ThreadRecord(
            id: UUID(),
            title: newTitle,
            prompt: newTitle,
            goalLayer: ThreadGoalLayer(
                goalStatement: newTitle,
                goalType: suggestGoalType(for: sourceText),
                successCondition: "Reach a clearer conclusion for this thread.",
                currentStage: .framing
            ),
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now,
            color: ThreadColor.allCases[threads.count % ThreadColor.allCases.count]
        )
        threads.insert(thread, at: 0)
        invalidateDerivedState(for: [thread.id])
        persistThread(thread.id)
    }

    func setContextThread(_ id: UUID?) {
        threadSidebar.contextThreadID = id
    }

    func createThreadFromEntry(_ entryID: UUID) {
        guard let entry = entries.first(where: { $0.id == entryID }) else { return }
        beginThreadCreation(seedText: entry.summaryText, sourceEntryID: entryID)
    }

    func deleteEntry(_ entryID: UUID) {
        let removedEntry = entries.first(where: { $0.id == entryID })
        let removedClaims = claims.filter { $0.originEntryID == entryID }
        claims.removeAll { $0.originEntryID == entryID }
        entries.removeAll { $0.id == entryID }
        discourseRelations.removeAll { $0.sourceEntryID == entryID || $0.targetEntryID == entryID }
        pendingMetadataEntryIDs.remove(entryID)

        let affectedThreadIDs = Set([removedEntry?.threadID].compactMap { $0 })
        let replyParents = Set([removedEntry?.parentEntryID].compactMap { $0 })
        clearRouteAnalysis(for: [entryID])
        invalidateDerivedState(for: affectedThreadIDs, replyParents: replyParents)
        for threadID in affectedThreadIDs {
            cancelAIWork(for: threadID)
            persistThread(threadID)
            persistDiscourseRelations(for: threadID)
        }
        repository?.deleteEntry(id: entryID)
        for claim in removedClaims {
            repository?.deleteClaim(id: claim.id)
        }
    }

    func updateEntryText(_ entryID: UUID, newText: String) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        let trimmed = newText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return }
        let parsed = parseCaptureInput(trimmed)
        let threadID = entries[index].threadID
        let parentEntryID = entries[index].parentEntryID
        entries[index].summaryText = trimmed
        entries[index].kind = parsed.semanticKind
        entries[index].body = parsed.body
        entries[index].objectMentions = parsed.objectMentions
        entries[index].references = parsed.references
        let updatedEntry = entries[index]
        clearRouteAnalysis(for: [entryID])
        invalidateDerivedState(
            for: Set([threadID].compactMap { $0 }),
            replyParents: Set([parentEntryID].compactMap { $0 })
        )
        if let threadID {
            cancelAIWork(for: threadID)
            refreshDiscourseRelations(for: threadID)
            persistDiscourseRelations(for: threadID)
        }
        queueMetadataEnrichmentIfNeeded(for: updatedEntry)
        persistEntry(updatedEntry)
    }

    func setEntryThread(_ entryID: UUID, to threadID: UUID) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        let oldThreadID = entries[index].threadID
        entries[index].threadID = threadID
        entries[index].inboxState = "resolved"
        touchThread(threadID)
        let updatedEntry = entries[index]
        let promotedClaim = maybePromoteClaim(from: updatedEntry)
        if let existingClaimIndex = claims.firstIndex(where: { $0.originEntryID == entryID }) {
            claims[existingClaimIndex].threadID = threadID
            claims[existingClaimIndex].updatedAt = .now
            persistClaim(claims[existingClaimIndex])
        }
        discourseRelations.removeAll { $0.sourceEntryID == entryID || $0.targetEntryID == entryID }
        if let oldThreadID {
            refreshDiscourseRelations(for: oldThreadID)
        }
        refreshDiscourseRelations(for: threadID)
        let affectedThreadIDs = Set([oldThreadID, threadID].compactMap { $0 })
        invalidateDerivedState(for: affectedThreadIDs)
        for affectedThreadID in affectedThreadIDs {
            cancelAIWork(for: affectedThreadID)
            persistThread(affectedThreadID)
            persistDiscourseRelations(for: affectedThreadID)
        }
        persistEntry(updatedEntry)
        if oldThreadID == nil {
            memoryPipeline?.recordWorking(entry: updatedEntry)
            memoryPipeline?.recordSource(entry: updatedEntry)
        }
        if let promotedClaim {
            persistClaim(promotedClaim)
        }
    }

    func resolveInboxEntry(_ entry: Entry, to threadID: UUID) {
        setEntryThread(entry.id, to: threadID)
    }

    // MARK: - Anchor writing

    func writeAnchor(for threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        let threadEntries = visibleEntries(for: threadID)
        let threadClaims = claims(for: threadID)
        let summary = buildSummary(for: thread, entries: threadEntries, claims: threadClaims)
        let openLoops = buildOpenLoops(entries: threadEntries, claims: threadClaims)
        let nextSteps = buildNextSteps(entries: threadEntries, claims: threadClaims)
        let anchor = Anchor(
            id: UUID(),
            threadID: threadID,
            createdAt: .now,
            basedOnEntryID: threadEntries.first?.id,
            title: "Checkpoint",
            coreQuestion: thread.goalLayer.goalStatement,
            stateSummary: summary,
            openLoops: openLoops,
            nextSteps: nextSteps,
            claimIDs: threadClaims.map(\.id),
            evidenceEntryIDs: Array(threadEntries.filter { $0.kind == .evidence || $0.kind == .source }.prefix(5).map(\.id)),
            phase: detectPhase(entries: threadEntries, claims: threadClaims)
        )
        anchors.append(anchor)
        memoryPipeline?.recordEpisodic(anchor: anchor)

        let anchorEntry = Entry(
            id: UUID(),
            threadID: threadID,
            kind: .anchorWritten,
            body: EntryBody(kind: .text, text: "Checkpoint saved: \(summary)", url: nil, title: nil, details: nil),
            summaryText: "Checkpoint saved: \(summary)",
            sourceMetadata: nil,
            createdAt: .now,
            sessionID: activeThreadSessionThreadID == threadID ? activeThreadSessionID : nil,
            authorType: "system",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 1,
            confidenceScore: 1,
            inboxState: "resolved"
        )
        entries.append(anchorEntry)
        touchThread(threadID)
        invalidateDerivedState(for: [threadID])
        cancelAIWork(for: threadID)
        persistAnchor(anchor)
        persistEntry(anchorEntry)
        persistThread(threadID)
    }

    func maybeWriteAnchorIfNeeded(for threadID: UUID?) {
        guard let threadID else { return }
        guard shouldWriteAnchor(for: threadID) else { return }
        writeAnchor(for: threadID)
    }

    // MARK: - Prepared views

    func prepareView(type: PreparedViewType, for threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        preparedViewType = type
        let threadAnchor = latestAnchor(for: threadID)
        let threadClaims = claims(for: threadID)
        let recentEntries = Array(visibleEntries(for: threadID).prefix(6))
        let evidenceIDs = threadAnchor?.evidenceEntryIDs ?? Array(recentEntries.filter { $0.kind == .evidence || $0.kind == .source }.prefix(3).map(\.id))
        let evidence = entries.filter { evidenceIDs.contains($0.id) && isUserVisible($0) }
        prepareDraft(
            type: type,
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            claims: threadClaims,
            evidence: evidence.sorted { $0.createdAt > $1.createdAt },
            anchor: threadAnchor,
            recentEntries: recentEntries
        )
    }

    // MARK: - Suggestions

    func suggestedThreads(for entry: Entry, limit: Int = 3) -> [ThreadSuggestion] {
        ensureRoutePlanning(for: entry)
        guard let debug = routeDebugCache[entry.id] else { return [] }
        return Array(debug.plannedSuggestions.prefix(limit)).compactMap { suggestion in
            guard let thread = homeThreads.first(where: { $0.id == suggestion.threadID }) else { return nil }
            return ThreadSuggestion(
                thread: thread,
                score: max(1, 100 - suggestion.rank),
                reason: suggestion.reason
            )
        }
    }

    func routeDebug(for entry: Entry, limit: Int = 3) -> RouteDebugState? {
        _ = routeDebugRevision
        if let cached = routeDebugCache[entry.id] {
            return cached
        }
        ensureRoutePlanning(for: entry, limit: max(limit, 5))
        return routeDebugCache[entry.id]
    }

    // MARK: - Private helpers

    private func append(_ entry: Entry) {
        entries.append(entry)
        persistEntry(entry)

        if let threadID = entry.threadID {
            touchThread(threadID)
            let promotedClaim = maybePromoteClaim(from: entry)
            refreshDiscourseRelations(for: threadID)
            invalidateDerivedState(
                for: [threadID],
                replyParents: Set([entry.parentEntryID].compactMap { $0 })
            )
            cancelAIWork(for: threadID)
            persistThread(threadID)
            persistDiscourseRelations(for: threadID)
            if let promotedClaim {
                persistClaim(promotedClaim)
            }
        } else {
            invalidateDerivedState(
                replyParents: Set([entry.parentEntryID].compactMap { $0 })
            )
            autoRoute(entry)
        }
        let currentEntry = entries.first(where: { $0.id == entry.id }) ?? entry
        if entry.threadID != nil {
            memoryPipeline?.recordWorking(entry: currentEntry)
            memoryPipeline?.recordSource(entry: currentEntry)
        }
        queueMetadataEnrichmentIfNeeded(for: currentEntry)
        scheduleSweep(delay: .seconds(3))
    }

    private func autoRoute(_ entry: Entry) {
        ensureRoutePlanning(for: entry, autoRoute: true)
    }

    private func ensureRoutePlanning(
        for entry: Entry,
        limit: Int = 5,
        autoRoute: Bool = false
    ) {
        guard entry.threadID == nil else { return }
        guard let threadRoutingEngine else { return }

        routePlanningTasks.removeValue(forKey: entry.id)?.cancel()
        routePlanningTaskTokens.removeValue(forKey: entry.id)
        routePlanningProcessingEntryIDs.remove(entry.id)

        let interpretation = captureInterpreter.interpret(entry: entry)
        let support = threadRoutingEngine.supportSnapshot(interpretation: interpretation, limit: max(limit, 5))
        routeDebugCache[entry.id] = initialRouteDebugState(for: support)
        routeDebugRevision &+= 1
        logRoute("Prepared routing support for entry \(entry.id.uuidString) with \(support.rankedCandidates.count) candidates.")
        guard !support.rankedCandidates.isEmpty else {
            routeDebugCache[entry.id] = RouteDebugState(
                plannerLabel: "LLM route planner",
                supportEngineLabel: "Deterministic routing support",
                status: .stayedInInbox,
                message: "No active thread candidates are available for routing.",
                connectivityStatus: llmProvider == nil ? "not_configured" : nil,
                connectivityMessage: llmProvider == nil ? "No AI backend configured." : nil,
                connectivityCheckedAt: llmProvider == nil ? .now : nil,
                normalizedText: support.normalizedText,
                detectedItemType: support.detectedItemType,
                detectedObjects: support.detectedObjects,
                candidateClaims: support.candidateClaims,
                routingQueries: support.routingQueries,
                topCandidates: [],
                decisionReason: "No active thread candidates are available for routing.",
                plannedSuggestions: [],
                selectedThreadID: nil,
                selectedThreadTitle: nil,
                topScore: support.topScore,
                secondScore: support.secondScore,
                autoRouteThreshold: support.autoRouteThreshold,
                autoRouteGapThreshold: support.autoRouteGapThreshold,
                backendLabel: llmProvider?.backendLabel,
                configuredModelID: llmProvider?.activeModelID,
                responseModelID: nil,
                responseID: nil,
                finishReason: nil,
                warnings: [],
                promptStats: nil,
                parsedResponse: nil,
                rawResponseBody: nil,
                updatedAt: .now
            )
            routeDebugRevision &+= 1
            logRoute("No routing candidates available for entry \(entry.id.uuidString).")
            return
        }

        guard let llm = llmProvider else {
            logRoute("Routing blocked for entry \(entry.id.uuidString): no AI backend configured.")
            return
        }

        let request = RoutePlanningRequest(
            entryID: entry.id,
            normalizedText: support.normalizedText,
            detectedItemType: support.detectedItemType,
            detectedObjects: support.detectedObjects,
            candidateClaims: support.candidateClaims,
            routingQueries: support.routingQueries,
            candidates: Array(support.rankedCandidates.prefix(AIPromptBudget.routeCandidateLimit))
        )
        let requestStats = routeRequestStats(for: request, support: support)

        routePlanningProcessingEntryIDs.insert(entry.id)
        let requestToken = UUID()
        routePlanningTaskTokens[entry.id] = requestToken
        let task = Task {
            defer {
                if routePlanningTaskTokens[entry.id] == requestToken {
                    routePlanningProcessingEntryIDs.remove(entry.id)
                    routePlanningTasks.removeValue(forKey: entry.id)
                    routePlanningTaskTokens.removeValue(forKey: entry.id)
                    routeDebugRevision &+= 1
                }
            }

            do {
                let lease = try await aiQueue.acquire(
                    priority: .routing,
                    label: "route:\(entry.id.uuidString)"
                )
                defer { releaseQueueLease(lease) }

                if var pendingDebug = routeDebugCache[entry.id] {
                    pendingDebug.message = "Requesting route decision from \(llm.backendLabel)."
                    pendingDebug.promptStats = requestStats
                    routeDebugCache[entry.id] = pendingDebug
                    routeDebugRevision &+= 1
                }
                logRoute(
                    """
                    Requesting route plan for entry \(entry.id.uuidString) from \(llm.backendLabel). \
                    Candidates: \(request.candidates.count). Text: \(request.normalizedText). Stats: \(requestStats)
                    """
                )
                let result = try await llm.planRoute(request: request)
                let connectivity = RouteConnectivitySnapshot(
                    status: "reachable",
                    message: "Route request succeeded.",
                    checkedAt: .now
                )
                let debug = resolveRouteDebugState(
                    entryID: entry.id,
                    support: support,
                    result: result,
                    llm: llm,
                    connectivity: connectivity,
                    promptStats: requestStats
                )
                guard routePlanningTaskTokens[entry.id] == requestToken else { return }
                routeDebugCache[entry.id] = debug
                logRoute(
                    """
                    Route decision for entry \(entry.id.uuidString): \(debug.status.rawValue) \
                    \(debug.selectedThreadTitle ?? "inbox"). Reason: \(debug.decisionReason)
                    """
                )
                if debug.status == .routed,
                   autoRoute,
                   let threadID = debug.selectedThreadID,
                   entries.contains(where: { $0.id == entry.id && $0.threadID == nil }) {
                    setEntryThread(entry.id, to: threadID)
                }
            } catch {
                guard routePlanningTaskTokens[entry.id] == requestToken else { return }
                guard !(error is CancellationError) else { return }
                routeDebugCache[entry.id] = failedRouteDebugState(
                    support: support,
                    llm: llm,
                    error: error,
                    promptStats: requestStats,
                    connectivityStatus: "failed",
                    connectivityMessage: error.localizedDescription,
                    connectivityCheckedAt: .now
                )
                logRoute("Route request failed for entry \(entry.id.uuidString): \(error.localizedDescription)")
            }
        }
        routePlanningTasks[entry.id] = task
    }

    func enrichEntryMetadata(_ entry: Entry) {
        queueMetadataEnrichmentIfNeeded(for: entry)
    }

    private func makeEntry(from parsed: CaptureParseResult, threadID: UUID?, parentEntryID: UUID? = nil) -> Entry {
        return Entry(
            id: UUID(),
            threadID: threadID,
            kind: parsed.semanticKind,
            body: parsed.body,
            summaryText: parsed.strippedText,
            sourceMetadata: parsed.sourceMetadata,
            objectMentions: parsed.objectMentions,
            references: parsed.references,
            createdAt: .now,
            sessionID: sessionIDForNewEntry(in: threadID),
            authorType: "user",
            parentEntryID: parentEntryID,
            supersedesEntryID: nil,
            importanceScore: parentEntryID == nil ? 0.8 : 0.55,
            confidenceScore: threadID == nil ? parsed.interpretation.confidenceScore : max(0.92, parsed.interpretation.confidenceScore),
            inboxState: threadID == nil ? "unresolved" : "resolved"
        )
    }

    private func maybePromoteClaim(from entry: Entry) -> Claim? {
        guard entry.kind == .claim, let threadID = entry.threadID else { return nil }
        guard !claims.contains(where: { $0.originEntryID == entry.id }) else { return nil }
        let claim = Claim(
            id: UUID(),
            threadID: threadID,
            originEntryID: entry.id,
            statement: entry.summaryText,
            status: .working,
            createdAt: .now,
            updatedAt: .now,
            confidenceScore: entry.confidenceScore ?? 0.7
        )
        claims.append(claim)
        return claim
    }

    private func beginThreadSession(for threadID: UUID) {
        guard activeThreadSessionThreadID != threadID else { return }
        activeThreadSessionThreadID = threadID
        activeThreadSessionID = UUID()
    }

    private func sessionIDForNewEntry(in threadID: UUID?) -> UUID {
        guard let threadID else { return UUID() }
        if isInWorkbench, selectedThreadID == threadID {
            if activeThreadSessionThreadID != threadID {
                beginThreadSession(for: threadID)
            }
            return activeThreadSessionID
        }
        return UUID()
    }

    private func parseCaptureInput(_ text: String) -> CaptureParseResult {
        let interpretation = captureInterpreter.interpret(text: text)
        let fallbackText = interpretation.normalizedText
        let bodyAndMetadata = buildEntryBody(from: fallbackText)
        let references = parseReferences(in: fallbackText)

        return CaptureParseResult(
            interpretation: interpretation,
            body: bodyAndMetadata.body,
            sourceMetadata: bodyAndMetadata.sourceMetadata,
            references: references
        )
    }


private func parseReferences(in text: String) -> [EntryReference] {
    guard let regex = try? NSRegularExpression(pattern: #"\[\[([^\]]+)\]\]"#, options: []) else {
        return []
    }
    let range = NSRange(text.startIndex..<text.endIndex, in: text)
    return regex.matches(in: text, options: [], range: range)
        .compactMap { match -> EntryReference? in
            guard let labelRange = Range(match.range(at: 1), in: text) else { return nil }
            let label = text[labelRange].trimmingCharacters(in: .whitespacesAndNewlines)
            guard !label.isEmpty else { return nil }
            return resolveReference(label: label)
        }
}

private func resolveReference(label: String) -> EntryReference {
    let normalized = label.folding(options: [.diacriticInsensitive, .caseInsensitive], locale: .current)
    let matchingThreads = threads.filter {
        $0.title.folding(options: [.diacriticInsensitive, .caseInsensitive], locale: .current) == normalized
    }
    let matchingEntries = entries.filter {
        isUserVisible($0)
            && $0.summaryText.folding(options: [.diacriticInsensitive, .caseInsensitive], locale: .current) == normalized
    }

    if matchingThreads.count == 1 && matchingEntries.isEmpty {
        return EntryReference(id: UUID(), label: label, targetKind: .thread, targetID: matchingThreads[0].id)
    }
    if matchingEntries.count == 1 && matchingThreads.isEmpty {
        return EntryReference(id: UUID(), label: label, targetKind: .note, targetID: matchingEntries[0].id)
    }
    return EntryReference(id: UUID(), label: label, targetKind: .unresolved, targetID: nil)
}

    private func buildEntryBody(from text: String) -> (body: EntryBody, sourceMetadata: SourceMetadata?) {
        if let attachmentBody = URLBodyMigration.attachmentBodyIfNeeded(from: text) {
            return (
                attachmentBody,
                URLBodyMigration.sourceMetadata(for: attachmentBody, sourceMetadata: nil)
            )
        }

        if let migrated = URLBodyMigration.bodyAndMetadataIfNeeded(from: text, sourceMetadata: nil) {
            return migrated
        }

        let summary = URLBodyMigration.normalizedSummaryText(from: text)
        return (
            EntryBody(kind: .text, text: summary, url: nil, title: nil, details: nil, linkMeta: nil),
            nil
        )
    }

    private func isUserVisible(_ entry: Entry) -> Bool {
        switch entry.kind {
        case .anchorWritten, .handoff:
            return false
        default:
            return entry.authorType != "system"
        }
    }

    func suggestGoalType(for goalStatement: String) -> ThreadGoalType {
        deterministicAI.suggestGoalType(for: goalStatement)
    }

    // MARK: - M4: Retrieval-backed context building

    /// Build ranked AISnippets for a thread using recall order:
    /// semantic memory → episodic memory → source memory → recent raw entries.
    /// Total text is capped at `tokenBudget` chars (≈ tokens * 4).
    private func retrievalSnippets(
        for threadID: UUID,
        rawEntries: [Entry],
        tokenBudget: Int = AIPromptBudget.resumeTokenBudget,
        maxSnippetCount: Int = AIPromptBudget.resumeMaxSnippetCount,
        maxSnippetCharacters: Int = AIPromptBudget.resumeMaxSnippetCharacters
    ) -> [AISnippet] {
        var snippets: [AISnippet] = []
        var charBudget = tokenBudget * 4   // rough chars-per-token conversion

        // 1. Semantic memory (settled claims)
        let semantic = repository?.fetchMemoryRecords(for: threadID, scope: .semantic) ?? []
        for rec in semantic.prefix(3) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(min(charBudget, maxSnippetCharacters)))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .claim))
            charBudget -= text.count
            if snippets.count >= maxSnippetCount { break }
        }

        // 2. Episodic memory (anchors / checkpoints)
        let episodic = repository?.fetchMemoryRecords(for: threadID, scope: .episodic) ?? []
        for rec in episodic.prefix(1) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(min(charBudget, maxSnippetCharacters)))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .anchorWritten))
            charBudget -= text.count
            if snippets.count >= maxSnippetCount { break }
        }

        // 3. Source memory (provenance)
        let source = repository?.fetchMemoryRecords(for: threadID, scope: .source) ?? []
        for rec in source.prefix(2) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(min(charBudget, maxSnippetCharacters)))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .source))
            charBudget -= text.count
            if snippets.count >= maxSnippetCount { break }
        }

        // 4. Recent raw entries (fallback / freshness)
        let recent = rawEntries
            .filter { $0.authorType == "user" }
            .sorted { $0.createdAt > $1.createdAt }
        for entry in recent {
            guard charBudget > 0 else { break }
            let text = String(entry.summaryText.prefix(min(charBudget, maxSnippetCharacters)))
            guard !text.isEmpty else { continue }
            // Avoid duplicating entries already covered by memory records
            if !snippets.contains(where: { $0.id == entry.id }) {
                snippets.append(AISnippet(id: entry.id, text: text, kind: entry.kind))
                charBudget -= text.count
            }
            if snippets.count >= maxSnippetCount { break }
        }

        return snippets
    }

    /// Retrieval-ranked evidence+source snippets scoped to one thread.
    private func retrievalEvidenceSnippets(
        for threadID: UUID,
        evidence: [Entry],
        sources: [Entry],
        tokenBudget: Int = AIPromptBudget.draftTokenBudget,
        maxSnippetCount: Int = AIPromptBudget.draftMaxSnippetCount,
        maxSnippetCharacters: Int = AIPromptBudget.draftMaxSnippetCharacters
    ) -> [AISnippet] {
        var snippets: [AISnippet] = []
        var charBudget = tokenBudget * 4

        // Use retrieval engine ranking if available, otherwise fall back to recency
        let candidates: [Entry]
        if let engine = retrievalEngine,
           let results = try? engine.recall(query: "", threadID: threadID,
                                            ownerTypes: ["entry"], limit: 10) {
            let ranked = results.compactMap { r -> Entry? in
                guard let uid = r.ownerUUID else { return nil }
                return (evidence + sources).first(where: { $0.id == uid })
            }
            candidates = ranked.isEmpty ? (evidence + sources) : ranked
        } else {
            candidates = (evidence + sources).sorted { $0.createdAt > $1.createdAt }
        }

        for entry in candidates {
            guard charBudget > 0 else { break }
            let text = String(entry.summaryText.prefix(min(charBudget, maxSnippetCharacters)))
            guard !text.isEmpty else { continue }
            snippets.append(AISnippet(id: entry.id, text: text, kind: entry.kind))
            charBudget -= text.count
            if snippets.count >= maxSnippetCount { break }
        }
        return snippets
    }

    private func contentFingerprint(for threadID: UUID) -> String {
        let goalToken = threads
            .first(where: { $0.id == threadID })
            .map { "\($0.id.uuidString):\(stableJSONString($0.goalLayer)):\($0.status.rawValue)" }
            ?? threadID.uuidString
        let anchorToken = latestAnchor(for: threadID).map {
            "\($0.id.uuidString):\($0.createdAt.timeIntervalSince1970):\($0.stateSummary):\($0.openLoops.joined(separator: "|"))"
        } ?? "no-anchor"
        let entryTokens = visibleEntries(for: threadID)
            .map {
                "\($0.id.uuidString):\($0.kind.rawValue):\($0.summaryText):\(stableJSONString($0.body)):\($0.parentEntryID?.uuidString ?? "root")"
            }
            .sorted()
        let claimTokens = claims(for: threadID)
            .map {
                "\($0.id.uuidString):\($0.status.rawValue):\($0.statement):\($0.updatedAt.timeIntervalSince1970)"
            }
            .sorted()
        let raw = ([goalToken, anchorToken] + entryTokens + claimTokens).joined(separator: "|")
        let digest = SHA256.hash(data: Data(raw.utf8))
        return digest.prefix(8).map { String(format: "%02x", $0) }.joined()
    }

    private func stableJSONString<T: Encodable>(_ value: T) -> String {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.sortedKeys]
        guard let data = try? encoder.encode(value),
              let string = String(data: data, encoding: .utf8) else {
            return ""
        }
        return string
    }

    private func decodeStableJSON<T: Decodable>(_ value: String, as type: T.Type) -> T? {
        guard let data = value.data(using: .utf8) else { return nil }
        return try? JSONDecoder().decode(type, from: data)
    }

    private func buildThreadStateInput(
        threadID: UUID,
        coreQuestion: String,
        goalLayer: ThreadGoalLayer,
        visibleEntries: [Entry],
        topLevelEntries: [Entry],
        claims: [Claim],
        anchor: Anchor?,
        relations: [DiscourseRelation]
    ) -> ThreadStateInput {
        let rankedSnippets = retrievalSnippets(for: threadID, rawEntries: visibleEntries)
        let questions = topLevelEntries.filter { $0.kind == .question }
        let evidence = topLevelEntries.filter { $0.kind == .evidence }
        let sources = topLevelEntries.filter { $0.kind == .source }
        let signature = threadRoutingEngine?.signature(for: threadID) ?? fallbackThreadSignature(
            threadID: threadID,
            coreQuestion: coreQuestion,
            goalLayer: goalLayer,
            claims: claims,
            anchor: anchor,
            visibleEntries: visibleEntries
        )

        return ThreadStateInput(
            threadID: threadID,
            coreQuestion: coreQuestion,
            goalLayer: goalLayer,
            signature: signature,
            activeClaims: claims.filter { $0.status != .superseded },
            questions: questions,
            evidenceEntries: evidence,
            sourceEntries: sources,
            recentEntries: topLevelEntries.sorted { $0.createdAt > $1.createdAt },
            recentNotes: rankedSnippets,
            anchor: anchor,
            entriesSinceAnchor: deltaEntries(for: threadID),
            discourseRelations: relations
        )
    }

    private func synthesizeThreadState(
        threadID: UUID,
        input: ThreadStateInput,
        snapshot: ThreadStateSnapshot,
        request: ResumeSynthesisRequest
    ) {
        guard let llm = llmProvider else { return }

        resumeSynthesisTasks.removeValue(forKey: threadID)?.cancel()
        resumeSynthesisTaskTokens.removeValue(forKey: threadID)

        let requestToken = UUID()
        resumeSynthesisTaskTokens[threadID] = requestToken
        let task = Task {
            resumeSynthesisProcessingThreadID = threadID
            defer {
                if resumeSynthesisTaskTokens[threadID] == requestToken {
                    resumeSynthesisProcessingThreadID = nil
                    resumeSynthesisTasks.removeValue(forKey: threadID)
                    resumeSynthesisTaskTokens.removeValue(forKey: threadID)
                }
            }

            do {
                let lease = try await aiQueue.acquire(
                    priority: .synthesis,
                    label: "resume:\(threadID.uuidString)"
                )
                defer { releaseQueueLease(lease) }

                let llmResult = try await llm.synthesizeResume(request: request)
                guard resumeSynthesisTaskTokens[threadID] == requestToken else { return }
                guard var state = threadStateCache[threadID] else { return }

                let presentationUpdate = deterministicAI.presentationUpdate(
                    from: llmResult,
                    input: input
                )

                switch presentationUpdate.status {
                case .applied:
                    guard let presentation = presentationUpdate.presentation else { return }
                    state.contentState = readyAIContentState()
                    state.presentation = presentation
                    state.aiDebug = appliedResumeAIDebugState(
                        from: llmResult.debugPayload,
                        fallbackPromptStats: resumeRequestStats(for: request),
                        presentation: presentation
                    )
                    state.restartNote = llmResult.restartNote
                    state.currentJudgment = llmResult.currentJudgment
                    state.judgmentBasis = snapshot.judgmentBasis
                    state.openLoops = llmResult.openLoops
                    state.nextAction = llmResult.nextAction
                    state.recoveryLines = llmResult.recoveryLines
                    state.resolvedSoFar = llmResult.resolvedSoFar
                    repository?.upsertAISnapshot(makeAISnapshot(
                        threadID: threadID,
                        state: state,
                        modelID: llmResult.debugPayload?.responseModelID ?? llm.activeModelID ?? ""
                    ))
                case let .invalid(reason):
                    state.contentState = failedAIContentState(
                        feature: "Restart Note",
                        message: "AI replied, but the restart note plan was invalid: \(reason)"
                    )
                    state.aiDebug = invalidResumeAIDebugState(
                        from: llmResult.debugPayload,
                        fallbackPromptStats: resumeRequestStats(for: request),
                        reason: reason
                    )
                }

                threadStateCache[threadID] = state
                bumpThreadStateRevision()
            } catch {
                guard resumeSynthesisTaskTokens[threadID] == requestToken else { return }
                guard !(error is CancellationError) else { return }
                if var state = threadStateCache[threadID] {
                    state.contentState = failedAIContentState(
                        feature: "Restart Note",
                        error: error
                    )
                    state.aiDebug = failedResumeAIDebugState(
                        llm: llm,
                        promptStats: resumeRequestStats(for: request),
                        error: error
                    )
                    threadStateCache[threadID] = state
                    bumpThreadStateRevision()
                }
            }
        }
        resumeSynthesisTasks[threadID] = task
    }

    private func releaseQueueLease(_ lease: AITaskQueue.Lease) {
        Task {
            await aiQueue.release(lease)
        }
    }

    private func makeAISnapshot(
        threadID: UUID,
        state: ThreadState,
        modelID: String
    ) -> ThreadAISnapshotRow {
        ThreadAISnapshotRow(
            threadID: threadID,
            contentFingerprint: contentFingerprint(for: threadID),
            headline: state.presentation.headline,
            blocksJSON: stableJSONString(state.presentation.blocks),
            restartNote: state.restartNote,
            currentJudgment: state.currentJudgment,
            openLoopsJSON: stableJSONString(state.openLoops),
            nextAction: state.nextAction,
            recoveryLinesJSON: stableJSONString(state.recoveryLines),
            synthesizedAt: .now,
            modelID: modelID
        )
    }

    private func notConfiguredAIContentState(feature: String) -> AIContentState {
        AIContentState(
            status: .notConfigured,
            message: "\(feature) requires an AI backend. Open Settings and configure one first."
        )
    }

    private func loadingAIContentState(feature: String, backendLabel: String) -> AIContentState {
        AIContentState(
            status: .loading,
            message: "Loading \(feature) from \(backendLabel)..."
        )
    }

    private func readyAIContentState() -> AIContentState {
        AIContentState(
            status: .ready,
            message: ""
        )
    }

    private func failedAIContentState(feature: String, error: Error) -> AIContentState {
        failedAIContentState(
            feature: feature,
            message: "\(feature) failed: \(error.localizedDescription)"
        )
    }

    private func failedAIContentState(feature: String, message: String) -> AIContentState {
        AIContentState(
            status: .error,
            message: message
        )
    }

    private func initialResumeAIDebugState() -> ThreadAIDebugState {
        guard let llm = llmProvider else {
            return ThreadAIDebugState(
                status: .notConfigured,
                backendLabel: "No backend configured",
                configuredModelID: nil,
                responseModelID: nil,
                responseID: nil,
                finishReason: nil,
                warnings: [],
                message: "AI backend is not configured. Open Settings and test the connection first.",
                promptStats: nil,
                parsedResponse: nil,
                rawResponseBody: nil,
                updatedAt: .now
            )
        }

        return ThreadAIDebugState(
            status: .pending,
            backendLabel: llm.backendLabel,
            configuredModelID: llm.activeModelID,
            responseModelID: nil,
            responseID: nil,
            finishReason: nil,
            warnings: [],
            message: "Waiting for \(llm.backendLabel) to return a cockpit plan.",
            promptStats: nil,
            parsedResponse: nil,
            rawResponseBody: nil,
            updatedAt: .now
        )
    }

    private func appliedResumeAIDebugState(
        from payload: LLMResumeDebugPayload?,
        fallbackPromptStats: String?,
        presentation: ThreadPresentation
    ) -> ThreadAIDebugState {
        ThreadAIDebugState(
            status: .applied,
            backendLabel: payload?.backendLabel ?? llmProvider?.backendLabel ?? "AI backend",
            configuredModelID: payload?.configuredModelID,
            responseModelID: payload?.responseModelID,
            responseID: payload?.responseID,
            finishReason: payload?.finishReason,
            warnings: payload?.warnings ?? [],
            message: "Applied AI cockpit plan with \(presentation.blocks.count) blocks.",
            promptStats: payload?.promptStats ?? fallbackPromptStats,
            parsedResponse: payload?.parsedResponse,
            rawResponseBody: payload?.rawResponseBody,
            updatedAt: payload?.updatedAt ?? .now
        )
    }

    private func invalidResumeAIDebugState(
        from payload: LLMResumeDebugPayload?,
        fallbackPromptStats: String?,
        reason: String
    ) -> ThreadAIDebugState {
        ThreadAIDebugState(
            status: .invalidPlan,
            backendLabel: payload?.backendLabel ?? llmProvider?.backendLabel ?? "AI backend",
            configuredModelID: payload?.configuredModelID,
            responseModelID: payload?.responseModelID,
            responseID: payload?.responseID,
            finishReason: payload?.finishReason,
            warnings: payload?.warnings ?? [],
            message: "AI replied, but its cockpit plan was rejected: \(reason)",
            promptStats: payload?.promptStats ?? fallbackPromptStats,
            parsedResponse: payload?.parsedResponse,
            rawResponseBody: payload?.rawResponseBody,
            updatedAt: payload?.updatedAt ?? .now
        )
    }

    private func failedResumeAIDebugState(
        llm: any AIBackendClient,
        promptStats: String?,
        error: Error
    ) -> ThreadAIDebugState {
        ThreadAIDebugState(
            status: .failed,
            backendLabel: llm.backendLabel,
            configuredModelID: llm.activeModelID,
            responseModelID: nil,
            responseID: nil,
            finishReason: nil,
            warnings: [],
            message: "AI request failed: \(error.localizedDescription)",
            promptStats: promptStats,
            parsedResponse: nil,
            rawResponseBody: nil,
            updatedAt: .now
        )
    }

    private func initialRouteDebugState(for support: RouteSupportSnapshot) -> RouteDebugState {
        let backendLabel = llmProvider?.backendLabel
        let configuredModelID = llmProvider?.activeModelID
        let hasBackend = llmProvider != nil
        return RouteDebugState(
            plannerLabel: "LLM route planner",
            supportEngineLabel: "Deterministic routing support",
            status: hasBackend ? .pending : .notConfigured,
            message: hasBackend
                ? "Waiting for \(backendLabel ?? "AI backend") to return a route decision."
                : "No AI backend configured. Route will stay in inbox until LLM routing is available.",
            connectivityStatus: hasBackend ? "pending_request" : "not_configured",
            connectivityMessage: hasBackend
                ? "No route response yet."
                : "No AI backend configured. Open Settings and test the connection first.",
            connectivityCheckedAt: hasBackend ? nil : .now,
            normalizedText: support.normalizedText,
            detectedItemType: support.detectedItemType,
            detectedObjects: support.detectedObjects,
            candidateClaims: support.candidateClaims,
            routingQueries: support.routingQueries,
            topCandidates: support.rankedCandidates,
            decisionReason: hasBackend
                ? "Pending LLM route decision."
                : "LLM routing is required and no backend is configured.",
            plannedSuggestions: [],
            selectedThreadID: nil,
            selectedThreadTitle: nil,
            topScore: support.topScore,
            secondScore: support.secondScore,
            autoRouteThreshold: support.autoRouteThreshold,
            autoRouteGapThreshold: support.autoRouteGapThreshold,
            backendLabel: backendLabel,
            configuredModelID: configuredModelID,
            responseModelID: nil,
            responseID: nil,
            finishReason: nil,
            warnings: [],
            promptStats: nil,
            parsedResponse: nil,
            rawResponseBody: nil,
            updatedAt: .now
        )
    }

    private func resolveRouteDebugState(
        entryID: UUID,
        support: RouteSupportSnapshot,
        result: RoutePlanningResult,
        llm: any AIBackendClient,
        connectivity: RouteConnectivitySnapshot,
        promptStats: String?
    ) -> RouteDebugState {
        let threadMap = Dictionary(uniqueKeysWithValues: homeThreads.map { ($0.id, $0) })
        let suggestions = result.suggestions.enumerated().compactMap { index, suggestion -> RoutePlannedSuggestion? in
            guard let thread = threadMap[suggestion.threadID] else { return nil }
            return RoutePlannedSuggestion(
                threadID: suggestion.threadID,
                threadTitle: thread.title,
                reason: suggestion.reason,
                rank: index
            )
        }

        let payload = result.debugPayload
        let selectedThread = result.selectedThreadID.flatMap { threadMap[$0] }

        if result.shouldRoute {
            guard let selectedThreadID = result.selectedThreadID,
                  let thread = selectedThread else {
                return RouteDebugState(
                    plannerLabel: "LLM route planner",
                    supportEngineLabel: "Deterministic routing support",
                    status: .invalidDecision,
                    message: "LLM tried to route this note, but did not return a valid candidate thread ID.",
                    connectivityStatus: connectivity.status,
                    connectivityMessage: connectivity.message,
                    connectivityCheckedAt: connectivity.checkedAt,
                    normalizedText: support.normalizedText,
                    detectedItemType: support.detectedItemType,
                    detectedObjects: support.detectedObjects,
                    candidateClaims: support.candidateClaims,
                    routingQueries: support.routingQueries,
                    topCandidates: support.rankedCandidates,
                    decisionReason: "Invalid LLM route decision.",
                    plannedSuggestions: suggestions,
                    selectedThreadID: nil,
                    selectedThreadTitle: nil,
                    topScore: support.topScore,
                    secondScore: support.secondScore,
                    autoRouteThreshold: support.autoRouteThreshold,
                    autoRouteGapThreshold: support.autoRouteGapThreshold,
                    backendLabel: payload?.backendLabel ?? llm.backendLabel,
                    configuredModelID: payload?.configuredModelID ?? llm.activeModelID,
                    responseModelID: payload?.responseModelID,
                    responseID: payload?.responseID,
                    finishReason: payload?.finishReason,
                    warnings: payload?.warnings ?? [],
                    promptStats: payload?.promptStats ?? promptStats,
                    parsedResponse: payload?.parsedResponse,
                    rawResponseBody: payload?.rawResponseBody,
                    updatedAt: payload?.updatedAt ?? .now
                )
            }

            return RouteDebugState(
                plannerLabel: "LLM route planner",
                supportEngineLabel: "Deterministic routing support",
                status: .routed,
                message: "LLM routed this note to \(thread.title).",
                connectivityStatus: connectivity.status,
                connectivityMessage: connectivity.message,
                connectivityCheckedAt: connectivity.checkedAt,
                normalizedText: support.normalizedText,
                detectedItemType: support.detectedItemType,
                detectedObjects: support.detectedObjects,
                candidateClaims: support.candidateClaims,
                routingQueries: support.routingQueries,
                topCandidates: support.rankedCandidates,
                decisionReason: result.decisionReason,
                plannedSuggestions: suggestions,
                selectedThreadID: selectedThreadID,
                selectedThreadTitle: thread.title,
                topScore: support.topScore,
                secondScore: support.secondScore,
                autoRouteThreshold: support.autoRouteThreshold,
                autoRouteGapThreshold: support.autoRouteGapThreshold,
                backendLabel: payload?.backendLabel ?? llm.backendLabel,
                configuredModelID: payload?.configuredModelID ?? llm.activeModelID,
                responseModelID: payload?.responseModelID,
                responseID: payload?.responseID,
                finishReason: payload?.finishReason,
                warnings: payload?.warnings ?? [],
                promptStats: payload?.promptStats ?? promptStats,
                parsedResponse: payload?.parsedResponse,
                rawResponseBody: payload?.rawResponseBody,
                updatedAt: payload?.updatedAt ?? .now
            )
        }

        return RouteDebugState(
            plannerLabel: "LLM route planner",
            supportEngineLabel: "Deterministic routing support",
            status: .stayedInInbox,
            message: "LLM kept this note in inbox.",
            connectivityStatus: connectivity.status,
            connectivityMessage: connectivity.message,
            connectivityCheckedAt: connectivity.checkedAt,
            normalizedText: support.normalizedText,
            detectedItemType: support.detectedItemType,
            detectedObjects: support.detectedObjects,
            candidateClaims: support.candidateClaims,
            routingQueries: support.routingQueries,
            topCandidates: support.rankedCandidates,
            decisionReason: result.decisionReason,
            plannedSuggestions: suggestions,
            selectedThreadID: nil,
            selectedThreadTitle: nil,
            topScore: support.topScore,
            secondScore: support.secondScore,
            autoRouteThreshold: support.autoRouteThreshold,
            autoRouteGapThreshold: support.autoRouteGapThreshold,
            backendLabel: payload?.backendLabel ?? llm.backendLabel,
            configuredModelID: payload?.configuredModelID ?? llm.activeModelID,
            responseModelID: payload?.responseModelID,
            responseID: payload?.responseID,
            finishReason: payload?.finishReason,
            warnings: payload?.warnings ?? [],
            promptStats: payload?.promptStats ?? promptStats,
            parsedResponse: payload?.parsedResponse,
            rawResponseBody: payload?.rawResponseBody,
            updatedAt: payload?.updatedAt ?? .now
        )
    }

    private func failedRouteDebugState(
        support: RouteSupportSnapshot,
        llm: any AIBackendClient,
        error: Error,
        promptStats: String?,
        connectivityStatus: String?,
        connectivityMessage: String?,
        connectivityCheckedAt: Date?
    ) -> RouteDebugState {
        RouteDebugState(
            plannerLabel: "LLM route planner",
            supportEngineLabel: "Deterministic routing support",
            status: .failed,
            message: "LLM route request failed: \(error.localizedDescription)",
            connectivityStatus: connectivityStatus,
            connectivityMessage: connectivityMessage,
            connectivityCheckedAt: connectivityCheckedAt,
            normalizedText: support.normalizedText,
            detectedItemType: support.detectedItemType,
            detectedObjects: support.detectedObjects,
            candidateClaims: support.candidateClaims,
            routingQueries: support.routingQueries,
            topCandidates: support.rankedCandidates,
            decisionReason: "LLM route request failed before a decision was returned.",
            plannedSuggestions: [],
            selectedThreadID: nil,
            selectedThreadTitle: nil,
            topScore: support.topScore,
            secondScore: support.secondScore,
            autoRouteThreshold: support.autoRouteThreshold,
            autoRouteGapThreshold: support.autoRouteGapThreshold,
            backendLabel: llm.backendLabel,
            configuredModelID: llm.activeModelID,
            responseModelID: nil,
            responseID: nil,
            finishReason: nil,
            warnings: [],
            promptStats: promptStats,
            parsedResponse: nil,
            rawResponseBody: nil,
            updatedAt: .now
        )
    }

    private func routeRequestStats(
        for request: RoutePlanningRequest,
        support: RouteSupportSnapshot
    ) -> String {
        let candidateChars = request.candidates.reduce(into: 0) { total, candidate in
            total += candidate.threadTitle.count
            total += candidate.goalStatement.count
            total += candidate.reason.count
            total += candidate.coreObjects.joined(separator: ",").count
            total += candidate.activeClaims.joined(separator: "|").count
            total += candidate.openLoops.joined(separator: "|").count
            total += candidate.latestAnchorSummary?.count ?? 0
        }
        let inputChars = request.normalizedText.count
            + request.candidateClaims.joined(separator: "\n").count
            + request.routingQueries.joined(separator: "\n").count
        return "route_candidates=\(request.candidates.count)/\(support.rankedCandidates.count), input_chars=\(inputChars), candidate_chars=\(candidateChars)"
    }

    private func resumeRequestStats(for request: ResumeSynthesisRequest) -> String {
        let recentNoteChars = request.recentNotes.reduce(0) { $0 + $1.text.count }
        return "recent_notes=\(request.recentNotes.count), recent_note_chars=\(recentNoteChars), active_claims=\(request.activeClaims.count), open_loops=\(request.openLoops.count), recovery_lines=\(request.recoveryLines.count), budget_tokens=\(AIPromptBudget.resumeTokenBudget)"
    }

    private func fallbackThreadSignature(
        threadID: UUID,
        coreQuestion: String,
        goalLayer: ThreadGoalLayer,
        claims: [Claim],
        anchor: Anchor?,
        visibleEntries: [Entry]
    ) -> ThreadSignature {
        let thread = threads.first(where: { $0.id == threadID }) ?? ThreadRecord(
            id: threadID,
            title: coreQuestion,
            prompt: coreQuestion,
            goalLayer: goalLayer,
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now,
            color: .sky
        )
        let entryObjects = visibleEntries.flatMap { entry in
            entry.objectMentions.isEmpty ? captureInterpreter.interpret(entry: entry).detectedObjects : entry.objectMentions
        }
        let derivedObjects = captureInterpreter.extractObjects(
            from: [thread.title, coreQuestion]
                + claims.prefix(4).map(\.statement)
                + [anchor?.stateSummary].compactMap { $0 }
                + visibleEntries.prefix(4).map(\.summaryText)
        )

        return ThreadSignature(
            thread: thread,
            goalStatement: coreQuestion,
            coreObjects: dedupeObjects(derivedObjects + entryObjects),
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            latestAnchorSummary: anchor?.stateSummary,
            openLoops: anchor?.openLoops ?? [],
            lastActiveAt: thread.lastActiveAt
        )
    }

    private func dedupeObjects(_ objects: [ObjectMention]) -> [ObjectMention] {
        var seen = Set<String>()
        var deduped: [ObjectMention] = []

        for object in objects {
            let key = object.name.lowercased()
            guard !seen.contains(key) else { continue }
            seen.insert(key)
            deduped.append(object)
        }

        return Array(deduped.prefix(5))
    }

    private func recentChanges(for threadID: UUID) -> [ThreadChangeItem] {
        deltaEntries(for: threadID)
            .sorted { $0.createdAt > $1.createdAt }
            .prefix(3)
            .map { ThreadChangeItem(text: $0.summaryText, changedAt: $0.createdAt) }
    }

    private func keyAnchors(for claims: [Claim], evidence: [Entry], sources: [Entry]) -> [ThreadAnchorHighlight] {
        var items: [ThreadAnchorHighlight] = []
        if let claim = claims.first {
            items.append(ThreadAnchorHighlight(title: "Claim", body: claim.statement))
        }
        if let evidence = evidence.first {
            items.append(ThreadAnchorHighlight(title: "Evidence", body: evidence.summaryText))
        }
        if items.count < 2, let source = sources.first {
            items.append(ThreadAnchorHighlight(title: "Source", body: source.sourceDisplayTitle))
        }
        return Array(items.prefix(2))
    }

    private func prepareDraft(
        type: PreparedViewType,
        threadID: UUID,
        coreQuestion: String,
        claims: [Claim],
        evidence: [Entry],
        anchor: Anchor?,
        recentEntries: [Entry]
    ) {
        let rankedEvidence = retrievalEvidenceSnippets(
            for: threadID,
            evidence: evidence,
            sources: recentEntries.filter { $0.kind == .source }
        )
        let rankedNotes = retrievalSnippets(for: threadID, rawEntries: recentEntries, tokenBudget: 1500)
        let request = DraftPreparationRequest(
            threadID: threadID,
            type: type,
            coreQuestion: coreQuestion,
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            keyEvidence: rankedEvidence,
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: recentEntries, claims: claims),
            recentNotes: rankedNotes
        )
        let title = "\(type.title) Draft"
        let featureName = title
        let baseView = PreparedView(
            threadID: threadID,
            type: type,
            title: title,
            contentState: llmProvider == nil
                ? notConfiguredAIContentState(feature: featureName)
                : loadingAIContentState(
                    feature: featureName,
                    backendLabel: llmProvider?.backendLabel ?? "AI backend"
                ),
            coreQuestion: coreQuestion,
            activeClaims: Array(claims.prefix(4)),
            keyEvidence: evidence,
            openLoops: [],
            recommendedNextSteps: [],
            recentEntries: recentEntries
        )
        preparedView = baseView

        guard let llm = llmProvider else { return }

        draftPreparationTasks.removeValue(forKey: threadID)?.cancel()
        draftPreparationTaskTokens.removeValue(forKey: threadID)
        let requestToken = UUID()
        draftPreparationTaskTokens[threadID] = requestToken
        let task = Task {
            draftPreparationProcessingThreadID = threadID
            defer {
                if draftPreparationTaskTokens[threadID] == requestToken {
                    draftPreparationProcessingThreadID = nil
                    draftPreparationTasks.removeValue(forKey: threadID)
                    draftPreparationTaskTokens.removeValue(forKey: threadID)
                }
            }

            do {
                let llmResult = try await llm.prepareDraft(request: request)
                guard draftPreparationTaskTokens[threadID] == requestToken else { return }
                guard var view = preparedView, view.threadID == threadID, view.type == type else { return }
                view.title = llmResult.title
                view.contentState = readyAIContentState()
                view.openLoops = llmResult.openLoops
                view.recommendedNextSteps = llmResult.recommendedNextSteps
                preparedView = view
            } catch {
                guard draftPreparationTaskTokens[threadID] == requestToken else { return }
                guard !(error is CancellationError) else { return }
                guard var view = preparedView, view.threadID == threadID, view.type == type else { return }
                view.contentState = failedAIContentState(
                    feature: featureName,
                    error: error
                )
                preparedView = view
            }
        }
        draftPreparationTasks[threadID] = task
    }

    private func touchThread(_ threadID: UUID) {
        guard let index = threads.firstIndex(where: { $0.id == threadID }) else { return }
        threads[index].lastActiveAt = .now
        threads[index].updatedAt = .now
    }

    func suggestThreadTitle(for text: String) -> String {
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return "Untitled Thread" }
        return String(trimmed.prefix(48))
    }

    private func streamSections(for threadID: UUID, topLevelEntries: [Entry], relations: [DiscourseRelation]) -> [ThreadStreamSection] {
        let sortedEntries = topLevelEntries.sorted { $0.createdAt > $1.createdAt }
        var orderedSessionIDs: [UUID] = []
        var groupedEntries: [UUID: [Entry]] = [:]

        for entry in sortedEntries {
            let sessionID = entry.sessionID ?? entry.id
            if groupedEntries[sessionID] == nil {
                orderedSessionIDs.append(sessionID)
            }
            groupedEntries[sessionID, default: []].append(entry)
        }

        return orderedSessionIDs.prefix(3).compactMap { sessionID in
            guard let grouped = groupedEntries[sessionID], !grouped.isEmpty else { return nil }
            let ascending = grouped.sorted { $0.createdAt < $1.createdAt }
            let items = ascending.map { entry in
                EntryStreamItem(
                    entry: entry,
                    primaryRelation: primaryRelation(for: entry.id, relations: relations),
                    relatedEntries: relatedEntries(for: entry.id, in: threadID),
                    replies: replies(for: entry.id)
                )
            }
            return ThreadStreamSection(
                id: sessionID,
                startedAt: ascending.first?.createdAt ?? .now,
                endedAt: ascending.last?.createdAt ?? .now,
                items: items
            )
        }
    }

    private func refreshDiscourseRelations(for threadID: UUID) {
        let threadEntryIDs = Set(visibleEntries(for: threadID).map(\.id))
        discourseRelations.removeAll { threadEntryIDs.contains($0.sourceEntryID) || threadEntryIDs.contains($0.targetEntryID) }

        let sorted = topLevelEntries(for: threadID).sorted { $0.createdAt < $1.createdAt }
        var newRelations: [DiscourseRelation] = []

        for (index, entry) in sorted.enumerated() where index > 0 {
            let previous = Array(Array(sorted[..<index]).reversed())
            guard let target = relationTarget(for: entry, previousEntries: previous) else { continue }
            newRelations.append(
                DiscourseRelation(
                    id: UUID(),
                    sourceEntryID: entry.id,
                    targetEntryID: target.id,
                    kind: relationKind(for: entry, target: target),
                    confidence: 0.7
                )
            )
        }

        discourseRelations.append(contentsOf: newRelations)
    }

    private func buildSummary(for thread: ThreadRecord, entries: [Entry], claims: [Claim]) -> String {
        if let leadingClaim = claims.first?.statement {
            return deterministicAI.compactWorkingRead(leadingClaim, maxLength: 88)
        }
        if let strongEvidence = entries.first(where: { $0.kind == .evidence })?.summaryText {
            return deterministicAI.compactWorkingRead(strongEvidence, maxLength: 88)
        }
        if let latest = entries.first?.summaryText {
            return deterministicAI.compactWorkingRead(latest, maxLength: 88)
        }
        return deterministicAI.compactWorkingRead(thread.goalLayer.goalStatement, maxLength: 88)
    }

    private func buildOpenLoops(entries: [Entry], claims: [Claim]) -> [String] {
        let questions = entries
            .filter { $0.kind == .question }
            .map(\.summaryText)
        if !questions.isEmpty {
            return Array(questions.prefix(3))
        }
        if claims.isEmpty {
            return ["No clear claim yet. Pull the strongest note into a claim."]
        }
        return ["What evidence or source would most change the current claim?"]
    }

    private func buildNextSteps(entries: [Entry], claims: [Claim]) -> [String] {
        if claims.isEmpty, let latest = entries.first?.summaryText {
            let compact = deterministicAI.compactWorkingRead(latest, maxLength: 72)
            return ["Turn this into a clearer claim: \(compact)"]
        }
        if !entries.contains(where: { $0.kind == .evidence }) {
            return ["Add evidence that strengthens or weakens the current claim."]
        }
        if !entries.contains(where: { $0.kind == .source }) {
            return ["Link a source or reference that grounds the latest evidence."]
        }
        return [
            "Reply on the strongest note to tighten the argument before writing.",
            "Open Prepare only when you need to turn this thread into output."
        ]
    }

    private func detectPhase(entries: [Entry], claims: [Claim]) -> String {
        if claims.count >= 2 && entries.contains(where: { $0.kind == .evidence }) {
            return "synthesizing"
        }
        if entries.contains(where: { $0.kind == .source }) {
            return "researching"
        }
        return "collecting"
    }

    private func shouldWriteAnchor(for threadID: UUID) -> Bool {
        let nonSystemEntries = visibleEntries(for: threadID)
        guard let anchor = latestAnchor(for: threadID) else {
            return !nonSystemEntries.isEmpty
        }
        let entriesSinceAnchor = nonSystemEntries.filter { $0.createdAt > anchor.createdAt && $0.parentEntryID == nil }
        return entriesSinceAnchor.count >= 3
    }

    // MARK: - Retrieval

    /// Deterministic thread suggestion using RetrievalEngine (no LLM required).
    /// Falls back to recency order when retrieval engine is unavailable.
    func suggestThreadsLocally(for text: String, excluding threadID: UUID? = nil) -> [ThreadSuggestion] {
        let candidates = homeThreads.filter { $0.id != threadID }
        guard let engine = retrievalEngine, !candidates.isEmpty else {
            return candidates.prefix(5).enumerated().map { idx, t in
                ThreadSuggestion(thread: t, score: candidates.count - idx, reason: "Recently active")
            }
        }
        do {
            return try engine.rankThreads(for: text, candidates: candidates)
        } catch {
            print("[Store] RetrievalEngine.rankThreads failed: \(error)")
            return candidates.prefix(5).enumerated().map { idx, t in
                ThreadSuggestion(thread: t, score: candidates.count - idx, reason: "Recently active")
            }
        }
    }

    // MARK: - Seed data

    private func seed() {
        let snapshot = makeSeedSnapshot()
        threads = snapshot.threads
        entries = snapshot.entries
        claims = snapshot.claims
        anchors = snapshot.anchors
        tasks = snapshot.tasks
        discourseRelations = snapshot.discourseRelations
        for thread in threads {
            refreshDiscourseRelations(for: thread.id)
        }
        invalidateDerivedState()
        repository?.saveSnapshot(AppSnapshot(
            sampleDataVersion: currentSampleDataVersion,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: tasks,
            discourseRelations: discourseRelations
        ))
        queueMetadataEnrichmentForLoadedEntries()
    }

    // MARK: - Persistence

    private func load() {
        guard let repository else { return }
        do {
            let snapshot = try repository.loadSnapshot()
            if shouldReseedLegacyDemoSnapshot(snapshot) {
                seed()
                return
            }
            threads = snapshot.threads
            entries = snapshot.entries
            claims = snapshot.claims
            anchors = snapshot.anchors
            tasks = snapshot.tasks
            discourseRelations = snapshot.discourseRelations

            if try repository.metadataValue(for: URLBodyMigration.metadataKey) != "1" {
                let modified = URLBodyMigration.runIfNeeded(entries: &entries)
                if !modified.isEmpty {
                    try repository.upsertEntriesImmediately(modified)
                }
                try repository.setMetadata("1", for: URLBodyMigration.metadataKey)
            }

            invalidateDerivedState()
            queueMetadataEnrichmentForLoadedEntries()
            scheduleSweep(delay: .seconds(1))
        } catch {
            print("[Store] Load failed: \(error)")
        }
    }

    private func shouldReseedLegacyDemoSnapshot(_ snapshot: AppSnapshot) -> Bool {
        guard (snapshot.sampleDataVersion ?? 0) < currentSampleDataVersion else {
            return false
        }
        guard !snapshot.threads.isEmpty else { return false }
        let knownDemoTitles = [
            "AI notes should restore context, not just store text",
            "Threadnote should restore a problem's last useful state",
            "Resume should restart work in 10 seconds",
            "Hitchcock suspense pattern study",
            "OpenClaw product landscape"
        ]
        return snapshot.threads.allSatisfy { knownDemoTitles.contains($0.title) }
    }
}
