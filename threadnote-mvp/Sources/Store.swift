import Foundation
import Observation

@MainActor
@Observable
final class ThreadnoteStore {
    private let currentSampleDataVersion = 8
    let aiConfiguration = ThreadnoteAIConfiguration.default

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

    private var persistenceStore: PersistenceStore?
    private(set) var retrievalEngine: RetrievalEngine?
    private var memoryPipeline: MemoryPipeline?
    private var activeThreadSessionID = UUID()
    private var activeThreadSessionThreadID: UUID?
    private var threadStateCache: [UUID: ThreadState] = [:]

    let linkMetadataService = LinkMetadataService()

    var isAIProcessing = false
    private(set) var llmProvider: LLMProvider?

    private var aiRuntime: ThreadnoteAIRuntime {
        ThreadnoteAIRuntime(configuration: aiConfiguration)
    }

    init() {
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

    /// Called once workspace is ready. Creates PersistenceStore, loads data.
    func configure(with databaseURL: URL) {
        do {
            let store = try PersistenceStore(databaseURL: databaseURL)
            persistenceStore = store
            retrievalEngine = RetrievalEngine(pool: store.databasePool)
            memoryPipeline = MemoryPipeline(persistence: store)
            load()
            if threads.isEmpty {
                seed()
            }
        } catch {
            print("[Store] Failed to configure PersistenceStore: \(error)")
        }
    }

    private func configureLLM() {
        let provider = LLMProvider()
        do {
            try provider.configure()
            llmProvider = provider.isConfigured ? provider : nil
        } catch {
            llmProvider = nil
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

        persist()
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
        let threadItems = homeThreads.map { ($0.title, "Thread", "rectangle.stack") }
        let entryItems = entries
            .filter { isUserVisible($0) && $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
            .prefix(20)
            .map { ($0.summaryText, $0.kind.title, "note.text") }
        return threadItems + entryItems
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
        ResourceDerivation.derive(from: topLevelEntries(for: threadID))
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
        resourceThreads
            .flatMap { resourceItems(for: $0.id) }
            .sorted { $0.createdAt > $1.createdAt }
    }

    var allResourceCounts: ResourceCounts {
        ResourceDerivation.counts(from: allResources)
    }

    // MARK: - Thread queries

    func entries(for threadID: UUID) -> [Entry] {
        entries
            .filter { $0.threadID == threadID }
            .sorted { $0.createdAt > $1.createdAt }
    }

    func visibleEntries(for threadID: UUID) -> [Entry] {
        entries(for: threadID).filter(isUserVisible)
    }

    func topLevelEntries(for threadID: UUID) -> [Entry] {
        visibleEntries(for: threadID)
            .filter { $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }
    }

    func replies(for entryID: UUID) -> [Entry] {
        entries
            .filter { $0.parentEntryID == entryID && isUserVisible($0) }
            .sorted { $0.createdAt < $1.createdAt }
    }

    func discourseRelations(for threadID: UUID) -> [DiscourseRelation] {
        let ids = Set(visibleEntries(for: threadID).map(\.id))
        return discourseRelations
            .filter { ids.contains($0.sourceEntryID) && ids.contains($0.targetEntryID) }
            .sorted { $0.confidence > $1.confidence }
    }

    func refreshDiscourseRelationsViaAI(for threadID: UUID) {
        guard let llm = llmProvider else { return }
        let snippets = visibleEntries(for: threadID).prefix(20).map {
            AISnippet(id: $0.id, text: $0.summaryText, kind: $0.kind)
        }
        guard snippets.count >= 2 else { return }

        let request = DiscourseAnalysisRequest(threadID: threadID, snippets: snippets)
        Task {
            isAIProcessing = true
            defer { isAIProcessing = false }
            guard let result = try? await llm.analyzeDiscourse(request: request) else { return }
            let entryIDs = Set(snippets.map(\.id))
            for pair in result.relationPairs {
                guard entryIDs.contains(pair.sourceEntryID),
                      entryIDs.contains(pair.targetEntryID) else { continue }
                let exists = discourseRelations.contains {
                    $0.sourceEntryID == pair.sourceEntryID && $0.targetEntryID == pair.targetEntryID
                }
                guard !exists else { continue }
                discourseRelations.append(DiscourseRelation(
                    id: UUID(),
                    sourceEntryID: pair.sourceEntryID,
                    targetEntryID: pair.targetEntryID,
                    kind: .informs,
                    confidence: 0.7
                ))
            }
            threadStateCache.removeValue(forKey: threadID)
            persist()
        }
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
        if let ps = persistenceStore {
            try? ps.upsertClaim(updated)
        }
        memoryPipeline?.recordSemantic(claim: updated)
        persist()
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
        guard let ps = persistenceStore else { return [] }
        return (try? ps.fetchMemoryRecords(for: threadID, scope: scope)) ?? []
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
        if let cached = threadStateCache[threadID] {
            return cached
        }
        guard let thread = threads.first(where: { $0.id == threadID }) else { return nil }
        let anchor = latestAnchor(for: threadID)
        let threadClaims = claims(for: threadID)
        let visible = visibleEntries(for: threadID)
        let topLevel = topLevelEntries(for: threadID)
        let relations = discourseRelations(for: threadID)
        let sections = streamSections(for: threadID, topLevelEntries: topLevel, relations: relations)
        let questions = topLevel.filter { $0.kind == .question }
        let evidence = topLevel.filter { $0.kind == .evidence }
        let sources = topLevel.filter { $0.kind == .source }
        let aiResume = synthesizeResume(
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            visibleEntries: visible,
            claims: threadClaims,
            anchor: anchor,
            evidenceCount: evidence.count,
            sourceCount: sources.count
        )
        let nextSteps = [aiResume.nextAction].compactMap { $0 }

        let state = ThreadState(
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            goalLayer: thread.goalLayer,
            restartNote: aiResume.restartNote,
            currentJudgment: aiResume.currentJudgment,
            openLoops: aiResume.openLoops,
            nextAction: nextSteps.first,
            recoveryLines: aiResume.recoveryLines,
            resolvedSoFar: resolvedItems(from: topLevel),
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
        threadStateCache[threadID] = state
        return state
    }

    // MARK: - Capture

    func beginCapture(prefill: String = "") {
        quickCaptureDraft = QuickCaptureDraft(text: prefill)
    }

    func submitCapture() {
        let text = quickCaptureDraft.text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let parsed = parseCaptureInput(text)
        let inferredThreadID = inferThreadID(for: parsed.strippedText)
        let entry = makeEntry(from: parsed, threadID: inferredThreadID)
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
        persist()
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
        persist()
    }

    func setContextThread(_ id: UUID?) {
        threadSidebar.contextThreadID = id
    }

    func createThreadFromEntry(_ entryID: UUID) {
        guard let entry = entries.first(where: { $0.id == entryID }) else { return }
        beginThreadCreation(seedText: entry.summaryText, sourceEntryID: entryID)
    }

    func updateEntryText(_ entryID: UUID, newText: String) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        let trimmed = newText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return }
        let parsed = parseCaptureInput(trimmed)
        entries[index].summaryText = trimmed
        entries[index].kind = parsed.semanticKind
        entries[index].body = parsed.body
        entries[index].objectMentions = parsed.objectMentions
        entries[index].references = parsed.references
        threadStateCache.removeAll()
        persist()
    }

    func setEntryThread(_ entryID: UUID, to threadID: UUID) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        entries[index].threadID = threadID
        entries[index].inboxState = "resolved"
        touchThread(threadID)
        maybePromoteClaim(from: entries[index])
        refreshDiscourseRelations(for: threadID)
        persist()
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
        persist()
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
        let draft = prepareDraft(
            type: type,
            threadID: threadID,
            coreQuestion: thread.goalLayer.goalStatement,
            claims: threadClaims,
            evidence: evidence.sorted { $0.createdAt > $1.createdAt },
            anchor: threadAnchor,
            recentEntries: recentEntries
        )
        preparedView = PreparedView(
            threadID: threadID,
            type: type,
            title: draft.title,
            coreQuestion: thread.goalLayer.goalStatement,
            activeClaims: Array(threadClaims.prefix(4)),
            keyEvidence: evidence.sorted { $0.createdAt > $1.createdAt },
            openLoops: draft.openLoops,
            recommendedNextSteps: draft.recommendedNextSteps,
            recentEntries: recentEntries
        )
    }

    // MARK: - Suggestions

    func suggestedThreads(for entry: Entry, limit: Int = 3) -> [ThreadSuggestion] {
        let excluded = entry.threadID.map { Set([$0]) } ?? []
        return suggestedThreadsViaAI(forText: entry.summaryText, excluding: excluded, limit: limit)
    }

    // MARK: - Private helpers

    private func append(_ entry: Entry) {
        entries.append(entry)
        if let threadID = entry.threadID {
            touchThread(threadID)
            maybePromoteClaim(from: entry)
            refreshDiscourseRelations(for: threadID)
        }
        memoryPipeline?.recordWorking(entry: entry)
        memoryPipeline?.recordSource(entry: entry)
        persist()
    }

    func enrichEntryMetadata(_ entry: Entry) {
        guard let urlString = entry.body.url, !urlString.isEmpty,
              entry.body.linkMeta == nil else { return }
        linkMetadataService.fetchIfNeeded(urlString) { [weak self] meta in
            guard let self,
                  let idx = self.entries.firstIndex(where: { $0.id == entry.id }) else { return }
            self.entries[idx].body.linkMeta = meta
            self.persist()
        }
    }

    private func makeEntry(from parsed: CaptureParseResult, threadID: UUID?, parentEntryID: UUID? = nil) -> Entry {
        return Entry(
            id: UUID(),
            threadID: threadID,
            kind: parsed.semanticKind,
            body: parsed.body,
            summaryText: parsed.strippedText,
            sourceMetadata: parsed.sourceMetadata,
            createdAt: .now,
            sessionID: sessionIDForNewEntry(in: threadID),
            authorType: "user",
            parentEntryID: parentEntryID,
            supersedesEntryID: nil,
            importanceScore: parentEntryID == nil ? 0.8 : 0.55,
            confidenceScore: threadID == nil ? 0.25 : 0.92,
            inboxState: threadID == nil ? "unresolved" : "resolved"
        )
    }

    private func maybePromoteClaim(from entry: Entry) {
        guard entry.kind == .claim, let threadID = entry.threadID else { return }
        guard !claims.contains(where: { $0.originEntryID == entry.id }) else { return }
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
    }

    private func inferThreadID(for text: String) -> UUID? {
        suggestedThreadsViaAI(forText: text, excluding: [], limit: 1).first.flatMap { $0.score > 1 ? $0.thread.id : nil }
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
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        let matchedTag = parseExplicitTag(in: trimmed)
        let strippedText = stripExplicitTag(from: trimmed, matchedTag: matchedTag)
        let fallbackText = strippedText.isEmpty ? trimmed : strippedText
        let bodyAndMetadata = buildEntryBody(from: fallbackText)
        let semanticKind = resolveSemanticKind(tag: matchedTag, fallbackText: fallbackText)
        let objectMentions = parseObjectMentions(in: fallbackText)
        let references = parseReferences(in: fallbackText)

        return CaptureParseResult(
            semanticKind: semanticKind,
            strippedText: fallbackText,
            matchedTag: matchedTag,
            body: bodyAndMetadata.body,
            sourceMetadata: bodyAndMetadata.sourceMetadata,
            objectMentions: objectMentions,
            references: references
        )
    }

    private func parseExplicitTag(in text: String) -> CaptureTag? {
        CaptureTag.parseTag(in: text)
    }

    private func stripExplicitTag(from text: String, matchedTag: CaptureTag?) -> String {
        guard let matchedTag else { return text }
        guard let pattern = try? NSRegularExpression(
            pattern: "(?<!\\\\S)#\(matchedTag.rawValue)\\\\b",
            options: [.caseInsensitive]
        ) else {
            return text
        }
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        let stripped = pattern.stringByReplacingMatches(in: text, options: [], range: range, withTemplate: "")
        return stripped
            .replacingOccurrences(of: "  ", with: " ")
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }

    private func resolveSemanticKind(tag: CaptureTag?, fallbackText: String) -> EntryKind {
        if let tag {
            return tag.entryKind
        }
        return inferSemanticKind(from: fallbackText)
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
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        let detector = try? NSDataDetector(types: NSTextCheckingResult.CheckingType.link.rawValue)
        let range = NSRange(trimmed.startIndex..<trimmed.endIndex, in: trimmed)
        let matches = detector?.matches(in: trimmed, options: [], range: range) ?? []
        let urls = matches.compactMap { $0.url?.absoluteString }
        let summary = trimmed.replacingOccurrences(of: "\n", with: " ")

        let body: EntryBody
        let sourceMetadata: SourceMetadata?
        if let firstURL = urls.first {
            let isOnlyURL = summary == firstURL
            body = EntryBody(
                kind: isOnlyURL ? .url : .mixed,
                text: isOnlyURL ? nil : summary,
                url: firstURL,
                title: nil,
                details: nil
            )
            sourceMetadata = SourceMetadata(title: nil, locator: firstURL, citation: nil)
        } else {
            body = EntryBody(kind: .text, text: summary, url: nil, title: nil, details: nil)
            sourceMetadata = nil
        }

        return (body, sourceMetadata)
    }

    private func inferSemanticKind(from text: String) -> EntryKind {
        if text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            return .note
        }
        return .note
    }

    private func isUserVisible(_ entry: Entry) -> Bool {
        switch entry.kind {
        case .anchorWritten, .handoff:
            return false
        default:
            return entry.authorType != "system"
        }
    }

    private func suggestedThreads(forText text: String, excluding excluded: Set<UUID>, limit: Int) -> [ThreadSuggestion] {
        let lowered = text.lowercased()
        let tokens = lowered
            .split(separator: " ")
            .map(String.init)
            .filter { $0.count > 2 }

        guard !tokens.isEmpty else { return [] }

        return threads
            .filter { !excluded.contains($0.id) }
            .map { thread in
                let haystack = "\(thread.title) \(thread.prompt)".lowercased()
                let matchedTokens = tokens.filter { haystack.contains($0) }
                let score = matchedTokens.count
                let reason: String
                if score >= 2 {
                    reason = "Matches \(score) terms from this note."
                } else if haystack.contains(tokens.first ?? "") {
                    reason = "Matches the main phrase and was active recently."
                } else {
                    reason = "Recently active related thread."
                }
                return ThreadSuggestion(thread: thread, score: score, reason: reason)
            }
            .filter { $0.score > 0 }
            .sorted { lhs, rhs in
                if lhs.score == rhs.score {
                    return lhs.thread.lastActiveAt > rhs.thread.lastActiveAt
                }
                return lhs.score > rhs.score
            }
            .prefix(limit)
            .map { $0 }
    }

    private func suggestedThreadsViaAI(forText text: String, excluding excluded: Set<UUID>, limit: Int) -> [ThreadSuggestion] {
        let request = ThreadSuggestionRequest(
            noteSummary: text,
            excludedThreadIDs: Array(excluded),
            candidateThreads: threads.map {
                AIThreadCandidate(id: $0.id, title: $0.title, prompt: $0.prompt, lastActiveAt: $0.lastActiveAt)
            }
        )

        // Try heuristic runtime first (synchronous)
        if case let .threadSuggestion(result)? = try? aiRuntime.run(.threadSuggestion(request)) {
            let heuristicResults = result.suggestions
                .prefix(limit)
                .compactMap { suggestion -> ThreadSuggestion? in
                    guard let thread = threads.first(where: { $0.id == suggestion.threadID }) else { return nil }
                    return ThreadSuggestion(thread: thread, score: suggestion.score, reason: suggestion.rationale)
                }

            // Fire LLM in background to refine results
            if let llm = llmProvider {
                Task {
                    isAIProcessing = true
                    defer { isAIProcessing = false }
                    if (try? await llm.suggestThreads(request: request)) != nil {
                        // Invalidate thread state cache so UI picks up new suggestions
                        threadStateCache.removeAll()
                    }
                }
            }

            return heuristicResults
        }

        return suggestedThreads(forText: text, excluding: excluded, limit: limit)
    }

    func suggestGoalType(for goalStatement: String) -> ThreadGoalType {
        if case let .goalTypeSuggestion(result)? = try? aiRuntime.run(.goalTypeSuggestion(GoalTypeSuggestionRequest(goalStatement: goalStatement))) {
            return result.goalType
        }
        return ThreadGoalType.suggested(for: goalStatement)
    }

    // MARK: - M4: Retrieval-backed context building

    /// Build ranked AISnippets for a thread using recall order:
    /// semantic memory → episodic memory → source memory → recent raw entries.
    /// Total text is capped at `tokenBudget` chars (≈ tokens * 4).
    private func retrievalSnippets(
        for threadID: UUID,
        rawEntries: [Entry],
        tokenBudget: Int = 3000
    ) -> [AISnippet] {
        var snippets: [AISnippet] = []
        var charBudget = tokenBudget * 4   // rough chars-per-token conversion

        // 1. Semantic memory (settled claims)
        let semantic = (try? persistenceStore?.fetchMemoryRecords(for: threadID, scope: .semantic)) ?? []
        for rec in semantic.prefix(4) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(charBudget))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .claim))
            charBudget -= text.count
        }

        // 2. Episodic memory (anchors / checkpoints)
        let episodic = (try? persistenceStore?.fetchMemoryRecords(for: threadID, scope: .episodic)) ?? []
        for rec in episodic.prefix(2) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(charBudget))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .anchorWritten))
            charBudget -= text.count
        }

        // 3. Source memory (provenance)
        let source = (try? persistenceStore?.fetchMemoryRecords(for: threadID, scope: .source)) ?? []
        for rec in source.prefix(3) {
            guard charBudget > 0 else { break }
            let text = String(rec.text.prefix(charBudget))
            snippets.append(AISnippet(id: rec.id, text: text, kind: .source))
            charBudget -= text.count
        }

        // 4. Recent raw entries (fallback / freshness)
        let recent = rawEntries
            .filter { $0.authorType == "user" }
            .sorted { $0.createdAt > $1.createdAt }
        for entry in recent {
            guard charBudget > 0 else { break }
            let text = String(entry.summaryText.prefix(charBudget))
            guard !text.isEmpty else { continue }
            // Avoid duplicating entries already covered by memory records
            if !snippets.contains(where: { $0.id == entry.id }) {
                snippets.append(AISnippet(id: entry.id, text: text, kind: entry.kind))
                charBudget -= text.count
            }
            if snippets.count >= 12 { break }
        }

        return snippets
    }

    /// Retrieval-ranked evidence+source snippets scoped to one thread.
    private func retrievalEvidenceSnippets(
        for threadID: UUID,
        evidence: [Entry],
        sources: [Entry],
        tokenBudget: Int = 1500
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
            let text = String(entry.summaryText.prefix(charBudget))
            guard !text.isEmpty else { continue }
            snippets.append(AISnippet(id: entry.id, text: text, kind: entry.kind))
            charBudget -= text.count
            if snippets.count >= 6 { break }
        }
        return snippets
    }

    private func synthesizeResume(
        threadID: UUID,
        coreQuestion: String,
        goalLayer: ThreadGoalLayer,
        visibleEntries: [Entry],
        claims: [Claim],
        anchor: Anchor?,
        evidenceCount: Int,
        sourceCount: Int
    ) -> ResumeSynthesisResult {
        let rankedSnippets = retrievalSnippets(for: threadID, rawEntries: visibleEntries)
        let request = ResumeSynthesisRequest(
            threadID: threadID,
            coreQuestion: coreQuestion,
            goalLayer: goalLayer,
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: visibleEntries, claims: claims),
            recentNotes: rankedSnippets,
            evidenceCount: evidenceCount,
            sourceCount: sourceCount
        )

        // Try heuristic runtime first (synchronous)
        if case let .resumeSynthesis(result)? = try? aiRuntime.run(.resumeSynthesis(request)) {
            // Fire LLM in background to provide richer synthesis
            if let llm = llmProvider {
                Task {
                    isAIProcessing = true
                    defer { isAIProcessing = false }
                    if let llmResult = try? await llm.synthesizeResume(request: request) {
                        // Update cached thread state with LLM result
                        if var state = threadStateCache[threadID] {
                            state.restartNote = llmResult.restartNote
                            state.currentJudgment = llmResult.currentJudgment
                            state.openLoops = llmResult.openLoops
                            state.nextAction = llmResult.nextAction
                            state.recoveryLines = llmResult.recoveryLines
                            state.resolvedSoFar = llmResult.resolvedSoFar
                            threadStateCache[threadID] = state
                        }
                    }
                }
            }
            return result
        }

        let fallbackJudgment = anchor?.stateSummary
            ?? buildSummary(for: threads.first(where: { $0.id == threadID })!, entries: visibleEntries, claims: claims)
        let fallbackLoops = anchor?.openLoops ?? buildOpenLoops(entries: visibleEntries, claims: claims)
        let fallbackNext = (anchor?.nextSteps ?? buildNextSteps(entries: visibleEntries, claims: claims)).first
        let fallbackSnippets = retrievalSnippets(for: threadID, rawEntries: visibleEntries, tokenBudget: 1500)
        let fallbackRecovery = HeuristicAIProvider().runFallbackRecovery(
            goalLayer: goalLayer,
            currentJudgment: fallbackJudgment,
            openLoops: fallbackLoops,
            nextAction: fallbackNext,
            claims: Array(claims.prefix(4).map(\.statement)),
            recentNotes: fallbackSnippets,
            evidenceCount: evidenceCount,
            sourceCount: sourceCount
        )
        return ResumeSynthesisResult(
            currentJudgment: fallbackJudgment,
            openLoops: fallbackLoops,
            nextAction: fallbackNext,
            restartNote: fallbackRecovery.restartNote,
            recoveryLines: fallbackRecovery.lines,
            resolvedSoFar: fallbackRecovery.resolved
        )
    }

    private func resolvedItems(from topLevelEntries: [Entry]) -> [ResolvedItem] {
        topLevelEntries
            .filter { entry in
                switch entry.kind {
                case .decided, .solved, .verified, .dropped:
                    true
                default:
                    false
                }
            }
            .sorted { $0.createdAt > $1.createdAt }
            .map { entry in
                let label: String
                switch entry.kind {
                case .decided:
                    label = "Decision Made"
                case .solved:
                    label = "Problem Solved"
                case .verified:
                    label = "Verified"
                case .dropped:
                    label = "Dropped"
                default:
                    label = "Resolved"
                }
                return ResolvedItem(text: entry.summaryText, statusLabel: label, resolvedAt: entry.createdAt)
            }
            .prefix(5)
            .map { $0 }
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
    ) -> DraftPreparationResult {
        let evidenceSources = evidence + recentEntries.filter { $0.kind == .source }
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

        if case let .draftPreparation(result)? = try? aiRuntime.run(.draftPreparation(request)) {
            return result
        }

        return DraftPreparationResult(
            title: "\(type.title) Draft",
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: recentEntries, claims: claims),
            recommendedNextSteps: anchor?.nextSteps ?? buildNextSteps(entries: recentEntries, claims: claims)
        )
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
            return leadingClaim
        }
        if let strongEvidence = entries.first(where: { $0.kind == .evidence })?.summaryText {
            return strongEvidence
        }
        if let latest = entries.first?.summaryText {
            return latest
        }
        return thread.goalLayer.goalStatement
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
            return ["Turn this into a clearer claim: \(latest)"]
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
        persist()
    }

    // MARK: - Persistence

    private func load() {
        guard let db = persistenceStore else { return }
        do {
            let snapshot = try db.loadSnapshot()
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
            threadStateCache.removeAll()
        } catch {
            print("[Store] Load failed: \(error)")
        }
    }

    private func persist() {
        threadStateCache.removeAll()
        guard let db = persistenceStore else { return }
        let snapshot = AppSnapshot(
            sampleDataVersion: currentSampleDataVersion,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: tasks,
            discourseRelations: discourseRelations
        )
        do {
            try db.saveSnapshot(snapshot)
        } catch {
            print("[Store] Persist failed: \(error)")
        }
    }

    private func shouldReseedLegacyDemoSnapshot(_ snapshot: AppSnapshot) -> Bool {
        guard (snapshot.sampleDataVersion ?? 0) < currentSampleDataVersion else {
            return false
        }
        guard snapshot.threads.count == 1 else { return false }
        let title = snapshot.threads.first?.title ?? ""
        let knownDemoTitles = [
            "AI notes should restore context, not just store text",
            "Threadnote should restore a problem's last useful state"
        ]
        return knownDemoTitles.contains(title)
    }
}
