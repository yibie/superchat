import Foundation
import Observation

@MainActor
@Observable
final class ThreadnoteStore {
    private let currentSampleDataVersion = 7
    let aiConfiguration = ThreadnoteAIConfiguration.default

    var threads: [ThreadRecord] = []
    var entries: [Entry] = []
    var claims: [Claim] = []
    var anchors: [Anchor] = []
    var tasks: [ThreadTask] = []
    var discourseRelations: [DiscourseRelation] = []
    var lists: [ListRecord] = []
    var listItems: [ListItem] = []

    var selectedThreadID: UUID?
    var selectedListID: UUID?
    var selectedSourceEntryID: UUID?
    var selectedResourcesThreadID: UUID?
    var preparedView: PreparedView?
    var preparedViewType: PreparedViewType = .writing
    var threadCreationContext: ThreadCreationContext?
    var quickCaptureDraft = QuickCaptureDraft()
    var inlineNoteDraft = ""
    var replyDrafts: [UUID: String] = [:]
    var expandedReplyEntryIDs: Set<UUID> = []
    var showingTimeline = false

    private let saveURL: URL
    private var activeThreadSessionID = UUID()
    private var activeThreadSessionThreadID: UUID?

    private var aiRuntime: ThreadnoteAIRuntime {
        ThreadnoteAIRuntime(configuration: aiConfiguration)
    }

    init() {
        let appSupport = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first!
        let directory = appSupport.appendingPathComponent("ThreadnoteMVP", isDirectory: true)
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        saveURL = directory.appendingPathComponent("snapshot.json")
        load()
        if threads.isEmpty {
            seed()
        }
    }

    // MARK: - Navigation

    var isInWorkbench: Bool { selectedThreadID != nil }

    var selectedThread: ThreadRecord? {
        guard let selectedThreadID else { return nil }
        return threads.first(where: { $0.id == selectedThreadID })
    }

    var selectedList: ListRecord? {
        guard let selectedListID else { return nil }
        return lists.first(where: { $0.id == selectedListID })
    }

    var selectedSourceEntry: Entry? {
        guard let selectedSourceEntryID else { return nil }
        return sourceEntry(id: selectedSourceEntryID)
    }

    var selectedResourcesThread: ThreadRecord? {
        guard let selectedResourcesThreadID else { return nil }
        return threads.first(where: { $0.id == selectedResourcesThreadID })
    }

    func goToStream() {
        if let tid = selectedThreadID {
            maybeWriteAnchorIfNeeded(for: tid)
        }
        selectedThreadID = nil
        selectedListID = nil
        selectedSourceEntryID = nil
        selectedResourcesThreadID = nil
        preparedView = nil
        activeThreadSessionThreadID = nil
        showingTimeline = false
    }

    func openThread(_ threadID: UUID) {
        if let current = selectedThreadID, current != threadID {
            maybeWriteAnchorIfNeeded(for: current)
        }
        selectedThreadID = threadID
        selectedListID = nil
        selectedSourceEntryID = nil
        selectedResourcesThreadID = nil
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
            if selectedResourcesThreadID == threadID {
                selectedResourcesThreadID = nil
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

    func selectList(_ listID: UUID) {
        if let tid = selectedThreadID {
            maybeWriteAnchorIfNeeded(for: tid)
        }
        selectedThreadID = nil
        selectedListID = listID
        selectedSourceEntryID = nil
        selectedResourcesThreadID = nil
        preparedView = nil
        activeThreadSessionThreadID = nil
        showingTimeline = false
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

    func openThreadResources(_ threadID: UUID) {
        guard threads.contains(where: { $0.id == threadID }) else { return }
        selectedResourcesThreadID = threadID
    }

    func closeThreadResources() {
        selectedResourcesThreadID = nil
    }

    func items(for listID: UUID) -> [ListItem] {
        listItems
            .filter { $0.listID == listID }
            .sorted { lhs, rhs in
                if lhs.position == rhs.position {
                    return lhs.addedAt < rhs.addedAt
                }
                return lhs.position < rhs.position
            }
    }

    @discardableResult
    func createList(title: String, description: String = "", kind: ListKind) -> UUID {
        let now = Date.now
        let id = UUID()
        lists.insert(
            ListRecord(
                id: id,
                title: title,
                description: description,
                kind: kind,
                createdAt: now,
                updatedAt: now
            ),
            at: 0
        )
        persist()
        return id
    }

    func addToList(itemType: ListItemType, itemID: UUID, to listID: UUID, note: String? = nil) {
        guard lists.contains(where: { $0.id == listID }) else { return }
        appendToListIfNeeded(itemType: itemType, itemID: itemID, to: listID, note: note)
        touchList(listID)
        persist()
    }

    func collectThreadResources(_ threadID: UUID, into listID: UUID) {
        guard threads.contains(where: { $0.id == threadID }) else { return }
        guard lists.contains(where: { $0.id == listID }) else { return }

        appendToListIfNeeded(itemType: .thread, itemID: threadID, to: listID)

        for entry in topLevelEntries(for: threadID) {
            switch entry.kind {
            case .evidence:
                appendToListIfNeeded(itemType: .entry, itemID: entry.id, to: listID)
            case .source:
                appendToListIfNeeded(itemType: .source, itemID: entry.id, to: listID)
            default:
                continue
            }
        }

        touchList(listID)
        persist()
    }

    func removeFromList(_ listItemID: UUID) {
        guard let index = listItems.firstIndex(where: { $0.id == listItemID }) else { return }
        let listID = listItems[index].listID
        listItems.remove(at: index)
        touchList(listID)
        persist()
    }

    func togglePinned(_ listItemID: UUID) {
        guard let index = listItems.firstIndex(where: { $0.id == listItemID }) else { return }
        let listID = listItems[index].listID
        listItems[index].isPinned.toggle()
        touchList(listID)
        persist()
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

    func tasks(for threadID: UUID) -> [ThreadTask] {
        tasks
            .filter { $0.threadID == threadID && $0.status != "done" }
            .sorted { $0.updatedAt > $1.updatedAt }
    }

    func needsThread(_ entry: Entry) -> Bool {
        entry.threadID == nil
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

        return ThreadState(
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
                lastActiveAt: .now
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
            lastActiveAt: .now
        )
        threads.insert(thread, at: 0)
        persist()
    }

    func createThreadFromEntry(_ entryID: UUID) {
        guard let entry = entries.first(where: { $0.id == entryID }) else { return }
        beginThreadCreation(seedText: entry.summaryText, sourceEntryID: entryID)
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
        persist()
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
        let supportedTags = CaptureTag.allCases.map(\.rawValue).joined(separator: "|")
        guard let regex = try? NSRegularExpression(pattern: #"(?<!\S)#("# + supportedTags + #")\b"#, options: [.caseInsensitive]) else {
            return nil
        }
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        guard let match = regex.firstMatch(in: text, options: [], range: range),
              let tagRange = Range(match.range(at: 1), in: text) else {
            return nil
        }
        return CaptureTag(rawValue: text[tagRange].lowercased())
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

private func parseObjectMentions(in text: String) -> [ObjectMention] {
    guard let regex = try? NSRegularExpression(pattern: #"(?<!\S)@([\p{L}\p{N}][\p{L}\p{N}._-]*)"#, options: []) else {
        return []
    }
    let range = NSRange(text.startIndex..<text.endIndex, in: text)
    let names = regex.matches(in: text, options: [], range: range)
        .compactMap { match -> String? in
            guard let nameRange = Range(match.range(at: 1), in: text) else { return nil }
            return String(text[nameRange])
        }

    var seen = Set<String>()
    return names.compactMap { name in
        let key = name.lowercased()
        guard seen.insert(key).inserted else { return nil }
        return ObjectMention(id: UUID(), name: name, kind: inferObjectKind(for: name))
    }
}

private func inferObjectKind(for name: String) -> ObjectKind {
    let lowered = name.lowercased()
    if lowered.hasSuffix("inc") || lowered.hasSuffix("labs") || lowered.hasSuffix("corp") || lowered.contains("openclaw") {
        return .company
    }
    return .generic
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

        if case let .threadSuggestion(result)? = try? aiRuntime.run(.threadSuggestion(request)) {
            return result.suggestions
                .prefix(limit)
                .compactMap { suggestion in
                    guard let thread = threads.first(where: { $0.id == suggestion.threadID }) else { return nil }
                    return ThreadSuggestion(thread: thread, score: suggestion.score, reason: suggestion.rationale)
                }
        }

        return suggestedThreads(forText: text, excluding: excluded, limit: limit)
    }

    func suggestGoalType(for goalStatement: String) -> ThreadGoalType {
        if case let .goalTypeSuggestion(result)? = try? aiRuntime.run(.goalTypeSuggestion(GoalTypeSuggestionRequest(goalStatement: goalStatement))) {
            return result.goalType
        }
        return ThreadGoalType.suggested(for: goalStatement)
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
        let request = ResumeSynthesisRequest(
            threadID: threadID,
            coreQuestion: coreQuestion,
            goalLayer: goalLayer,
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: visibleEntries, claims: claims),
            recentNotes: Array(visibleEntries.prefix(6)).map {
                AISnippet(id: $0.id, text: $0.summaryText, kind: $0.kind)
            },
            evidenceCount: evidenceCount,
            sourceCount: sourceCount
        )

        if case let .resumeSynthesis(result)? = try? aiRuntime.run(.resumeSynthesis(request)) {
            return result
        }

        let fallbackJudgment = anchor?.stateSummary
            ?? buildSummary(for: threads.first(where: { $0.id == threadID })!, entries: visibleEntries, claims: claims)
        let fallbackLoops = anchor?.openLoops ?? buildOpenLoops(entries: visibleEntries, claims: claims)
        let fallbackNext = (anchor?.nextSteps ?? buildNextSteps(entries: visibleEntries, claims: claims)).first
        let fallbackSnippets = Array(visibleEntries.prefix(6)).map {
            AISnippet(id: $0.id, text: $0.summaryText, kind: $0.kind)
        }
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
        let request = DraftPreparationRequest(
            threadID: threadID,
            type: type,
            coreQuestion: coreQuestion,
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            keyEvidence: Array(evidence.prefix(3)).map {
                AISnippet(id: $0.id, text: $0.summaryText, kind: $0.kind)
            },
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: recentEntries, claims: claims),
            recentNotes: recentEntries.map {
                AISnippet(id: $0.id, text: $0.summaryText, kind: $0.kind)
            }
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

    private func touchList(_ listID: UUID) {
        guard let index = lists.firstIndex(where: { $0.id == listID }) else { return }
        lists[index].updatedAt = .now
    }

    private func appendToListIfNeeded(itemType: ListItemType, itemID: UUID, to listID: UUID, note: String? = nil) {
        guard !listItems.contains(where: { $0.listID == listID && $0.itemType == itemType && $0.itemID == itemID }) else { return }
        let nextPosition = (items(for: listID).map(\.position).max() ?? -1) + 1
        listItems.append(
            ListItem(
                id: UUID(),
                listID: listID,
                itemType: itemType,
                itemID: itemID,
                addedAt: .now,
                note: note,
                position: nextPosition,
                isPinned: false
            )
        )
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

    private func primaryRelation(for entryID: UUID, relations: [DiscourseRelation]) -> DiscourseRelation? {
        relations.first(where: { $0.sourceEntryID == entryID })
            ?? relations.first(where: { $0.targetEntryID == entryID })
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

    private func relationTarget(for entry: Entry, previousEntries: [Entry]) -> Entry? {
        switch entry.kind {
        case .evidence:
            return previousEntries.first(where: { $0.kind == .claim })
                ?? previousEntries.first(where: { $0.kind == .source })
                ?? previousEntries.first(where: { $0.kind == .evidence })
        case .source:
            return nil
        case .question:
            return nil
        case .claim, .comparison, .pattern, .decided, .verified:
            return previousEntries.first(where: { $0.kind == .question })
                ?? previousEntries.first(where: { $0.kind == .claim || $0.kind == .evidence })
        case .note, .idea, .plan, .solved, .dropped, .handoff, .anchorWritten:
            return nil
        }
    }

    private func relationKind(for entry: Entry, target: Entry) -> DiscourseRelationKind {
        switch entry.kind {
        case .evidence:
            if target.kind == .source {
                return .informs
            }
            return isOpposingNarrative(entry.summaryText) ? .opposes : .supports
        case .source:
            return .informs
        case .question:
            return .answers
        case .claim, .comparison, .pattern, .decided, .verified:
            if target.kind == .question {
                return .answers
            }
            return isOpposingNarrative(entry.summaryText) ? .opposes : .supports
        case .note, .idea, .plan, .solved, .dropped, .handoff, .anchorWritten:
            return .supports
        }
    }

    private func isOpposingNarrative(_ text: String) -> Bool {
        let lowered = text.lowercased()
        return lowered.contains("not")
            || lowered.contains("instead")
            || lowered.contains("however")
            || lowered.contains("fails")
            || lowered.contains("doesn't")
            || lowered.contains("does not")
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

    // MARK: - Seed data

    private func seed() {
        let firstSessionID = UUID()
        let secondSessionID = UUID()
        let thirdSessionID = UUID()
        let buildThread = ThreadRecord(
            id: UUID(),
            title: "Resume should restart work in 10 seconds",
            prompt: "How should Threadnote reopen a thread without forcing the user to reread a wall of context?",
            goalLayer: ThreadGoalLayer(
                goalStatement: "Define a Resume model that lets someone reopen a thread and continue work in 10-30 seconds.",
                goalType: .build,
                successCondition: "The Resume design is clear enough that the thread can restart work with only a tiny visible surface.",
                currentStage: .synthesizing
            ),
            status: .active,
            createdAt: .now.addingTimeInterval(-86_400 * 4),
            updatedAt: .now.addingTimeInterval(-3_600),
            lastActiveAt: .now.addingTimeInterval(-3_600)
        )
        let studyThread = ThreadRecord(
            id: UUID(),
            title: "Hitchcock suspense pattern study",
            prompt: "What recurring suspense techniques appear across Hitchcock's films?",
            goalLayer: ThreadGoalLayer(
                goalStatement: "Watch Hitchcock films and extract a reusable note on how suspense is staged across multiple works.",
                goalType: .study,
                successCondition: "The thread has a stable pattern read with enough examples and one or two useful exceptions.",
                currentStage: .gathering
            ),
            status: .active,
            createdAt: .now.addingTimeInterval(-86_400 * 6),
            updatedAt: .now.addingTimeInterval(-7_200),
            lastActiveAt: .now.addingTimeInterval(-7_200)
        )
        let researchThread = ThreadRecord(
            id: UUID(),
            title: "OpenClaw product landscape",
            prompt: "What products are adjacent to OpenClaw, and how should they be grouped?",
            goalLayer: ThreadGoalLayer(
                goalStatement: "Map products adjacent to OpenClaw and identify the most useful comparison axes.",
                goalType: .research,
                successCondition: "The landscape is grouped clearly enough that the next comparison can focus on one missing category.",
                currentStage: .gathering
            ),
            status: .active,
            createdAt: .now.addingTimeInterval(-86_400 * 5),
            updatedAt: .now.addingTimeInterval(-10_800),
            lastActiveAt: .now.addingTimeInterval(-10_800)
        )
        threads = [buildThread, studyThread, researchThread]

        let e1 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .claim,
            body: EntryBody(kind: .text, text: "Resume should reopen a thread with a tiny restart surface, not a component wall.", url: nil, title: nil, details: nil),
            summaryText: "Resume should reopen a thread with a tiny restart surface, not a component wall.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400 * 3),
            sessionID: firstSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.95,
            confidenceScore: 0.9,
            inboxState: "resolved"
        )
        let e2 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .question,
            body: EntryBody(kind: .text, text: "What information belongs behind Resolved So Far instead of the default Resume?", url: nil, title: nil, details: nil),
            summaryText: "What information belongs behind Resolved So Far instead of the default Resume?",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400 * 3 + 1_200),
            sessionID: firstSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 1,
            confidenceScore: 0.82,
            inboxState: "resolved"
        )
        let e3 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .evidence,
            body: EntryBody(kind: .text, text: "Users stall when the Resume opens with too many structured blocks before they know what to do next.", url: nil, title: nil, details: nil),
            summaryText: "Users stall when the Resume opens with too many structured blocks before they know what to do next.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400 * 2),
            sessionID: secondSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.92,
            confidenceScore: 0.88,
            inboxState: "resolved"
        )
        let e4 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .source,
            body: EntryBody(kind: .url, text: nil, url: "https://tape.systems", title: "Tape Systems", details: nil),
            summaryText: "https://tape.systems",
            sourceMetadata: SourceMetadata(title: "Tape Systems", locator: "https://tape.systems", citation: "Reference for append-only context architecture."),
            createdAt: .now.addingTimeInterval(-86_400 + 600),
            sessionID: thirdSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.84,
            confidenceScore: 0.93,
            inboxState: "resolved"
        )
        let e5 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .claim,
            body: EntryBody(kind: .text, text: "The default Resume should show only three recovery lines: state, gap, and next move.", url: nil, title: nil, details: nil),
            summaryText: "The default Resume should show only three recovery lines: state, gap, and next move.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400),
            sessionID: thirdSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 1,
            confidenceScore: 0.93,
            inboxState: "resolved"
        )
        let e6 = Entry(
            id: UUID(),
            threadID: buildThread.id,
            kind: .evidence,
            body: EntryBody(kind: .text, text: "Users can restart work faster when the first screen answers state, blocker, and next move within one viewport.", url: nil, title: nil, details: nil),
            summaryText: "Users can restart work faster when the first screen answers state, blocker, and next move within one viewport.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-3_400),
            sessionID: thirdSessionID,
            authorType: "user",
            parentEntryID: e5.id,
            supersedesEntryID: nil,
            importanceScore: 0.76,
            confidenceScore: 0.86,
            inboxState: "resolved"
        )
        let unresolved = Entry(
            id: UUID(),
            threadID: nil,
            kind: .claim,
            body: EntryBody(kind: .text, text: "Watching Hitchcock should produce reusable suspense patterns, not just a watch log.", url: nil, title: nil, details: nil),
            summaryText: "Watching Hitchcock should produce reusable suspense patterns, not just a watch log.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-43_200),
            sessionID: UUID(),
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.76,
            confidenceScore: 0.25,
            inboxState: "unresolved"
        )
        let studySessionID = UUID()
        let s1 = Entry(
            id: UUID(),
            threadID: studyThread.id,
            kind: .claim,
            body: EntryBody(kind: .text, text: "Hitchcock often delays release by making the viewer know more than the character on screen.", url: nil, title: nil, details: nil),
            summaryText: "Hitchcock often delays release by making the viewer know more than the character on screen.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400 * 2 + 1_800),
            sessionID: studySessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.82,
            confidenceScore: 0.76,
            inboxState: "resolved"
        )
        let s2 = Entry(
            id: UUID(),
            threadID: studyThread.id,
            kind: .question,
            body: EntryBody(kind: .text, text: "Does this pattern still hold when suspense comes from mistaken identity rather than physical threat?", url: nil, title: nil, details: nil),
            summaryText: "Does this pattern still hold when suspense comes from mistaken identity rather than physical threat?",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-50_000),
            sessionID: studySessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.78,
            confidenceScore: 0.72,
            inboxState: "resolved"
        )
        let s3 = Entry(
            id: UUID(),
            threadID: studyThread.id,
            kind: .evidence,
            body: EntryBody(kind: .text, text: "Rear Window sustains suspense by trapping the viewer inside one observational position.", url: nil, title: nil, details: nil),
            summaryText: "Rear Window sustains suspense by trapping the viewer inside one observational position.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-20_000),
            sessionID: studySessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.85,
            confidenceScore: 0.8,
            inboxState: "resolved"
        )
        let researchSessionID = UUID()
        let r1 = Entry(
            id: UUID(),
            threadID: researchThread.id,
            kind: .claim,
            body: EntryBody(kind: .text, text: "The OpenClaw-adjacent space seems split between capture tools, visual organizers, and workflow shells.", url: nil, title: nil, details: nil),
            summaryText: "The OpenClaw-adjacent space seems split between capture tools, visual organizers, and workflow shells.",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-86_400 * 2 + 3_600),
            sessionID: researchSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.8,
            confidenceScore: 0.74,
            inboxState: "resolved"
        )
        let r2 = Entry(
            id: UUID(),
            threadID: researchThread.id,
            kind: .question,
            body: EntryBody(kind: .text, text: "Which comparison axis matters more here: workflow depth or speed of capture?", url: nil, title: nil, details: nil),
            summaryText: "Which comparison axis matters more here: workflow depth or speed of capture?",
            sourceMetadata: nil,
            createdAt: .now.addingTimeInterval(-30_000),
            sessionID: researchSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.77,
            confidenceScore: 0.71,
            inboxState: "resolved"
        )
        let r3 = Entry(
            id: UUID(),
            threadID: researchThread.id,
            kind: .source,
            body: EntryBody(kind: .url, text: nil, url: "https://example.com/openclaw-landscape", title: "OpenClaw Landscape", details: nil),
            summaryText: "https://example.com/openclaw-landscape",
            sourceMetadata: SourceMetadata(title: "OpenClaw Landscape", locator: "https://example.com/openclaw-landscape", citation: "A rough external scan of adjacent products and categories."),
            createdAt: .now.addingTimeInterval(-18_000),
            sessionID: researchSessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.74,
            confidenceScore: 0.79,
            inboxState: "resolved"
        )
        entries = [e1, e2, e3, e4, e5, e6, s1, s2, s3, r1, r2, r3, unresolved]

        claims = [
            Claim(
                id: UUID(),
                threadID: buildThread.id,
                originEntryID: e5.id,
                statement: e5.summaryText,
                status: .stable,
                createdAt: .now.addingTimeInterval(-50_000),
                updatedAt: .now.addingTimeInterval(-4_000),
                confidenceScore: 0.87
            ),
            Claim(
                id: UUID(),
                threadID: studyThread.id,
                originEntryID: s1.id,
                statement: s1.summaryText,
                status: .working,
                createdAt: .now.addingTimeInterval(-48_000),
                updatedAt: .now.addingTimeInterval(-18_000),
                confidenceScore: 0.76
            ),
            Claim(
                id: UUID(),
                threadID: researchThread.id,
                originEntryID: r1.id,
                statement: r1.summaryText,
                status: .working,
                createdAt: .now.addingTimeInterval(-40_000),
                updatedAt: .now.addingTimeInterval(-12_000),
                confidenceScore: 0.73
            )
        ]

        anchors = [
            Anchor(
                id: UUID(),
                threadID: buildThread.id,
                createdAt: .now.addingTimeInterval(-3_600),
                basedOnEntryID: e5.id,
                title: "Checkpoint",
                coreQuestion: buildThread.goalLayer.goalStatement,
                stateSummary: "The direction is to make Resume a tiny restart point instead of a structured reading surface.",
                openLoops: [
                    "What belongs behind Resolved So Far instead of the default Resume?",
                    "How should the second line describe the real blocker instead of repeating a raw question?"
                ],
                nextSteps: [
                    "Rewrite the default Resume around three recovery lines: current direction, blocker, and next decision.",
                    "Keep all other structure behind one controlled expansion path."
                ],
                claimIDs: claims.filter { $0.threadID == buildThread.id }.map(\.id),
                evidenceEntryIDs: [e3.id, e4.id, e6.id],
                phase: "synthesizing"
            ),
            Anchor(
                id: UUID(),
                threadID: studyThread.id,
                createdAt: .now.addingTimeInterval(-7_200),
                basedOnEntryID: s3.id,
                title: "Checkpoint",
                coreQuestion: studyThread.goalLayer.goalStatement,
                stateSummary: "The current read is that Hitchcock sustains suspense by controlling what the viewer sees and when release is delayed.",
                openLoops: [
                    "Does mistaken-identity suspense break the current pattern?",
                    "Which film should serve as the strongest counterexample?"
                ],
                nextSteps: [
                    "Watch one film that stresses mistaken identity and write the next observation immediately after."
                ],
                claimIDs: claims.filter { $0.threadID == studyThread.id }.map(\.id),
                evidenceEntryIDs: [s3.id],
                phase: "gathering"
            ),
            Anchor(
                id: UUID(),
                threadID: researchThread.id,
                createdAt: .now.addingTimeInterval(-10_800),
                basedOnEntryID: r1.id,
                title: "Checkpoint",
                coreQuestion: researchThread.goalLayer.goalStatement,
                stateSummary: "The map is starting to separate capture tools, visual organizers, and workflow shells, but the comparison axes are still weak.",
                openLoops: [
                    "Which missing category matters most for the next comparison pass?",
                    "Should workflow depth outrank capture speed as the main axis?"
                ],
                nextSteps: [
                    "Probe one missing category before refining the comparison axes."
                ],
                claimIDs: claims.filter { $0.threadID == researchThread.id }.map(\.id),
                evidenceEntryIDs: [r3.id],
                phase: "gathering"
            )
        ]

        tasks = [
            ThreadTask(
                id: UUID(),
                threadID: buildThread.id,
                originEntryID: nil,
                title: "Refine the three-line recovery surface",
                status: "open",
                createdAt: .now.addingTimeInterval(-1_800),
                updatedAt: .now.addingTimeInterval(-1_800)
            )
        ]

        let collectionList = ListRecord(
            id: UUID(),
            title: "Resume-first references",
            description: "A read-only shelf for the strongest artifacts behind the workbench model.",
            kind: .collection,
            createdAt: .now.addingTimeInterval(-18_000),
            updatedAt: .now.addingTimeInterval(-2_400)
        )
        let queueList = ListRecord(
            id: UUID(),
            title: "Open capture follow-ups",
            description: "Notes to revisit later without turning the list into another active thread.",
            kind: .queue,
            createdAt: .now.addingTimeInterval(-14_000),
            updatedAt: .now.addingTimeInterval(-1_200)
        )
        lists = [queueList, collectionList]
        listItems = [
            ListItem(
                id: UUID(),
                listID: collectionList.id,
                itemType: .thread,
                itemID: buildThread.id,
                addedAt: .now.addingTimeInterval(-12_000),
                note: "Core demo thread for the current product direction.",
                position: 0,
                isPinned: true
            ),
            ListItem(
                id: UUID(),
                listID: collectionList.id,
                itemType: .source,
                itemID: e4.id,
                addedAt: .now.addingTimeInterval(-11_400),
                note: "External reference that anchors the append-only framing.",
                position: 1,
                isPinned: true
            ),
            ListItem(
                id: UUID(),
                listID: collectionList.id,
                itemType: .thread,
                itemID: studyThread.id,
                addedAt: .now.addingTimeInterval(-9_000),
                note: "Study-type sample thread for pattern extraction work.",
                position: 2,
                isPinned: false
            ),
            ListItem(
                id: UUID(),
                listID: collectionList.id,
                itemType: .thread,
                itemID: researchThread.id,
                addedAt: .now.addingTimeInterval(-8_400),
                note: "Research-type sample thread for landscape mapping.",
                position: 3,
                isPinned: false
            ),
            ListItem(
                id: UUID(),
                listID: queueList.id,
                itemType: .entry,
                itemID: e2.id,
                addedAt: .now.addingTimeInterval(-3_800),
                note: "Still a useful framing question.",
                position: 0,
                isPinned: false
            ),
            ListItem(
                id: UUID(),
                listID: queueList.id,
                itemType: .entry,
                itemID: unresolved.id,
                addedAt: .now.addingTimeInterval(-3_200),
                note: "Candidate idea that belongs in a list before it earns a thread.",
                position: 1,
                isPinned: false
            )
        ]

        refreshDiscourseRelations(for: buildThread.id)
        refreshDiscourseRelations(for: studyThread.id)
        refreshDiscourseRelations(for: researchThread.id)
        persist()
    }

    // MARK: - Persistence

    private func load() {
        guard let data = try? Data(contentsOf: saveURL) else { return }
        guard let snapshot = try? JSONDecoder().decode(AppSnapshot.self, from: data) else { return }
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
        lists = snapshot.lists
        listItems = snapshot.listItems
    }

    private func persist() {
        let snapshot = AppSnapshot(
            sampleDataVersion: currentSampleDataVersion,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: tasks,
            discourseRelations: discourseRelations,
            lists: lists,
            listItems: listItems
        )
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        encoder.dateEncodingStrategy = .iso8601
        guard let data = try? encoder.encode(snapshot) else { return }
        try? data.write(to: saveURL)
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
