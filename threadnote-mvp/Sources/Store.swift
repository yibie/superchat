import Foundation
import Observation

@MainActor
@Observable
final class ThreadnoteStore {
    private let currentSampleDataVersion = 3
    var route: AppRoute = .home
    var threads: [ThreadRecord] = []
    var entries: [Entry] = []
    var claims: [Claim] = []
    var anchors: [Anchor] = []
    var tasks: [ThreadTask] = []
    var lists: [ListRecord] = []
    var listItems: [ListItem] = []
    var selectedThreadID: UUID?
    var preparedView: PreparedView?
    var presentedListID: UUID?
    var preparedViewType: PreparedViewType = .writing
    var quickCaptureDraft = QuickCaptureDraft()
    var inboxEntries: [Entry] = []
    var selectedEntryID: UUID?
    var inlineNoteDraft = ""
    var noteStreamDraft = ""

    private let saveURL: URL
    private var activeThreadSessionID = UUID()
    private var activeThreadSessionThreadID: UUID?

    init() {
        let appSupport = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first!
        let directory = appSupport.appendingPathComponent("ThreadnoteMVP", isDirectory: true)
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        saveURL = directory.appendingPathComponent("snapshot.json")
        load()
        if threads.isEmpty {
            seed()
        }
        if selectedThreadID == nil {
            selectedThreadID = threads.first?.id
        }
    }

    var selectedThread: ThreadRecord? {
        guard let selectedThreadID else { return nil }
        return threads.first(where: { $0.id == selectedThreadID })
    }

    var selectedEntry: Entry? {
        guard let selectedEntryID else { return nil }
        return entries.first(where: { $0.id == selectedEntryID })
    }

    var recentVisibleEntries: [Entry] {
        entries
            .filter(isUserVisible)
            .sorted { $0.createdAt > $1.createdAt }
    }

    var unresolvedEntryCount: Int {
        inboxEntries.count
    }

    var homeThreads: [ThreadRecord] {
        threads.sorted { $0.lastActiveAt > $1.lastActiveAt }
    }

    var homeLists: [ListRecord] {
        lists.sorted { $0.updatedAt > $1.updatedAt }
    }

    var needsSortingEntries: [Entry] {
        inboxEntries.sorted { $0.createdAt > $1.createdAt }
    }

    var homeRecentEntries: [Entry] {
        recentVisibleEntries.filter { !needsThread($0) }
    }

    var presentedList: ListRecord? {
        guard let presentedListID else { return nil }
        return lists.first(where: { $0.id == presentedListID })
    }

    func entries(for threadID: UUID) -> [Entry] {
        entries
            .filter { $0.threadLinks.contains(where: { $0.threadID == threadID }) }
            .sorted { $0.createdAt > $1.createdAt }
    }

    func visibleEntries(for threadID: UUID) -> [Entry] {
        entries(for: threadID).filter(isUserVisible)
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
        entry.threadLinks.isEmpty || entry.inboxState != "resolved"
    }

    func items(for listID: UUID) -> [ListItem] {
        listItems
            .filter { $0.listID == listID }
            .sorted { $0.position < $1.position }
    }

    func entryCount(for listID: UUID) -> Int {
        items(for: listID).filter { $0.entityType == .entry }.count
    }

    func threadCount(for listID: UUID) -> Int {
        items(for: listID).filter { $0.entityType == .thread }.count
    }

    func openList(_ listID: UUID) {
        presentedListID = listID
    }

    func suggestedThreads(for entry: Entry, limit: Int = 3) -> [ThreadSuggestion] {
        suggestedThreads(forText: entry.content, excluding: Set(entry.threadIDs), limit: limit)
    }

    func quickCaptureSuggestions(limit: Int = 3) -> [ThreadSuggestion] {
        suggestedThreads(forText: quickCaptureDraft.text, excluding: [], limit: limit)
    }

    func threadState(for threadID: UUID) -> ThreadState? {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return nil }
        let anchor = anchor(for: thread)
        let claims = claims(for: threadID)
        let visibleEntries = visibleEntries(for: threadID)
        let deltaEntries: [Entry]

        if let anchor {
            deltaEntries = visibleEntries
                .filter { $0.createdAt > anchor.createdAt }
        } else {
            deltaEntries = visibleEntries
        }
        let recentSessions = sessionGroups(from: deltaEntries)
        let relatedEntries = Array(visibleEntries.prefix(6))

        let nextSteps = anchor?.nextSteps ?? buildNextSteps(entries: visibleEntries, claims: claims)

        return ThreadState(
            threadID: threadID,
            coreQuestion: anchor?.coreQuestion ?? thread.prompt,
            currentJudgment: anchor?.stateSummary ?? buildSummary(for: thread, entries: visibleEntries, claims: claims),
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: visibleEntries, claims: claims),
            nextAction: nextSteps.first,
            keyClaims: Array(claims.prefix(3)),
            recentSessions: recentSessions,
            relatedEntries: relatedEntries,
            lastAnchorID: anchor?.id,
            lastAnchorAt: anchor?.createdAt
        )
    }

    func anchor(for thread: ThreadRecord) -> Anchor? {
        guard let id = thread.currentAnchorID else { return nil }
        return anchors.first(where: { $0.id == id })
    }

    func primaryThread(for entry: Entry) -> ThreadRecord? {
        guard let threadID = entry.primaryThreadID else { return nil }
        return threads.first(where: { $0.id == threadID })
    }

    func isEntry(_ entryID: UUID, inThread threadID: UUID) -> Bool {
        entries.contains { $0.id == entryID && $0.threadLinks.contains(where: { $0.threadID == threadID }) }
    }

    func secondaryThreadCount(for entry: Entry) -> Int {
        max(0, entry.threadLinks.count - 1)
    }

    func beginCapture(prefill: String = "", threadID: UUID? = nil) {
        quickCaptureDraft = QuickCaptureDraft(text: prefill, selectedThreadID: threadID ?? selectedThreadID)
    }

    func setSelectedEntry(_ entryID: UUID?) {
        selectedEntryID = entryID
    }

    func selectThread(_ threadID: UUID) {
        if let currentThreadID = selectedThreadID, currentThreadID != threadID {
            maybeWriteAnchorIfNeeded(for: currentThreadID)
        }
        selectedThreadID = threadID
    }

    func openThread(_ threadID: UUID) {
        selectThread(threadID)
        beginThreadSession(for: threadID)
        route = .thread
        if preparedView?.threadID != threadID {
            preparedView = nil
        }
    }

    func openThreadFromNote(threadID: UUID, entryID: UUID?) {
        if let entryID {
            selectedEntryID = entryID
        }
        openThread(threadID)
    }

    func goHome() {
        route = .home
        preparedView = nil
        activeThreadSessionThreadID = nil
    }

    func submitCapture() {
        let text = quickCaptureDraft.text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }

        let inferredThreadID = quickCaptureDraft.selectedThreadID ?? inferThreadID(for: text)
        let sessionID = sessionIDForNewEntry(in: inferredThreadID)
        let entry = Entry(
            id: UUID(),
            threadLinks: inferredThreadID.map { [EntryThreadLink(threadID: $0, role: .core)] } ?? [],
            kind: classifyKind(for: text),
            content: text,
            createdAt: .now,
            sessionID: sessionID,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: nil,
            confidenceScore: inferredThreadID == nil ? 0.25 : 0.9,
            inboxState: inferredThreadID == nil ? "unresolved" : "resolved"
        )

        entries.append(entry)
        selectedEntryID = entry.id
        if let inferredThreadID {
            touchThread(inferredThreadID, fallbackSummary: text)
            maybePromoteClaim(from: entry)
            selectedThreadID = inferredThreadID
        } else {
            inboxEntries.append(entry)
        }

        quickCaptureDraft = QuickCaptureDraft()
        persist()
    }

    func submitNoteStreamCapture() {
        let text = noteStreamDraft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        quickCaptureDraft = QuickCaptureDraft(text: text, selectedThreadID: nil)
        submitCapture()
        noteStreamDraft = ""
    }

    func createThread(from draft: QuickCaptureDraft? = nil, title: String? = nil) {
        let seedText = draft?.text.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        let newTitle = title ?? suggestThreadTitle(for: seedText)
        let thread = ThreadRecord(
            id: UUID(),
            title: newTitle,
            prompt: seedText.isEmpty ? "A new problem worth returning to." : seedText,
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now,
            currentAnchorID: nil,
            summary: "Fresh thread. Capture a few signals before you distill it.",
            nextStep: "Add the first concrete observation."
        )
        threads.insert(thread, at: 0)
        selectedThreadID = thread.id
        route = .thread

        if draft != nil, !seedText.isEmpty {
            quickCaptureDraft.selectedThreadID = thread.id
            submitCapture()
        } else {
            persist()
        }
    }

    func createThreadLinkingExistingEntry(_ entryID: UUID) {
        guard let entry = entries.first(where: { $0.id == entryID }) else { return }
        let newThread = ThreadRecord(
            id: UUID(),
            title: suggestThreadTitle(for: entry.content),
            prompt: entry.content,
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now,
            currentAnchorID: nil,
            summary: "Fresh thread created from an existing note.",
            nextStep: "Add the next concrete observation."
        )
        threads.insert(newThread, at: 0)
        attachEntry(entryID, to: newThread.id, role: entry.threadLinks.isEmpty ? .core : .supporting)
        openThreadFromNote(threadID: newThread.id, entryID: entryID)
        persist()
    }

    func resolveInboxEntry(_ entry: Entry, to threadID: UUID) {
        attachEntry(entry.id, to: threadID, role: entry.threadLinks.isEmpty ? .core : .supporting)
        guard let index = entries.firstIndex(where: { $0.id == entry.id }) else { return }
        entries[index].inboxState = "resolved"
        inboxEntries.removeAll { $0.id == entry.id }
        touchThread(threadID, fallbackSummary: entries[index].content)
        maybePromoteClaim(from: entries[index])
        selectedEntryID = entry.id
        selectedThreadID = threadID
        persist()
    }

    func appendInlineNote(to threadID: UUID) {
        let text = inlineNoteDraft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let entry = Entry(
            id: UUID(),
            threadLinks: [EntryThreadLink(threadID: threadID, role: .core)],
            kind: classifyKind(for: text),
            content: text,
            createdAt: .now,
            sessionID: sessionIDForNewEntry(in: threadID),
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: 0.7,
            confidenceScore: 0.95,
            inboxState: "resolved"
        )
        entries.append(entry)
        selectedEntryID = entry.id
        touchThread(threadID, fallbackSummary: text)
        maybePromoteClaim(from: entry)
        inlineNoteDraft = ""
        persist()
    }

    func writeAnchor(for threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        let threadEntries = entries(for: threadID)
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
            coreQuestion: thread.prompt,
            stateSummary: summary,
            openLoops: openLoops,
            nextSteps: nextSteps,
            claimIDs: threadClaims.map(\.id),
            evidenceEntryIDs: Array(threadEntries.prefix(5).map(\.id)),
            phase: detectPhase(entries: threadEntries, claims: threadClaims)
        )
        anchors.append(anchor)

        let anchorEntry = Entry(
            id: UUID(),
            threadLinks: [EntryThreadLink(threadID: threadID, role: .reference)],
            kind: .anchorWritten,
            content: "Checkpoint saved: \(summary)",
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

        if let index = threads.firstIndex(where: { $0.id == threadID }) {
            threads[index].currentAnchorID = anchor.id
            threads[index].summary = summary
            threads[index].nextStep = nextSteps.first ?? threads[index].nextStep
            threads[index].updatedAt = .now
            threads[index].lastActiveAt = .now
        }

        persist()
    }

    func maybeWriteAnchorIfNeeded(for threadID: UUID?) {
        guard let threadID else { return }
        guard shouldWriteAnchor(for: threadID) else { return }
        writeAnchor(for: threadID)
    }

    func prepareView(type: PreparedViewType, for threadID: UUID) {
        guard let thread = threads.first(where: { $0.id == threadID }) else { return }
        preparedViewType = type
        let threadAnchor = anchor(for: thread)
        let threadClaims = claims(for: threadID)
        let recentEntries = Array(visibleEntries(for: threadID).prefix(6))
        let evidenceIDs = threadAnchor?.evidenceEntryIDs ?? Array(recentEntries.prefix(3).map(\.id))
        let evidence = entries.filter { evidenceIDs.contains($0.id) && isUserVisible($0) }
        preparedView = PreparedView(
            threadID: threadID,
            type: type,
            title: "\(type.title) Draft",
            coreQuestion: threadAnchor?.coreQuestion ?? thread.prompt,
            activeClaims: Array(threadClaims.prefix(4)),
            keyEvidence: evidence.sorted { $0.createdAt > $1.createdAt },
            openLoops: threadAnchor?.openLoops ?? buildOpenLoops(entries: recentEntries, claims: threadClaims),
            recommendedNextSteps: threadAnchor?.nextSteps ?? buildNextSteps(entries: recentEntries, claims: threadClaims),
            recentEntries: recentEntries
        )
    }

    func refreshPreparedView(as type: PreparedViewType) {
        preparedViewType = type
        guard let threadID = preparedView?.threadID ?? selectedThreadID else { return }
        prepareView(type: type, for: threadID)
    }

    func listEntry(_ item: ListItem) -> Entry? {
        guard item.entityType == .entry else { return nil }
        return entries.first(where: { $0.id == item.entityID })
    }

    func listThread(_ item: ListItem) -> ThreadRecord? {
        guard item.entityType == .thread else { return nil }
        return threads.first(where: { $0.id == item.entityID })
    }

    private func maybePromoteClaim(from entry: Entry) {
        let normalized = entry.content.lowercased()
        guard let threadID = entry.threadLinks.first(where: { $0.role == .core })?.threadID else { return }
        guard normalized.contains("should") || normalized.contains("must") || normalized.contains("problem") || normalized.contains("core") else {
            return
        }

        let claim = Claim(
            id: UUID(),
            threadID: threadID,
            originEntryID: entry.id,
            statement: entry.content,
            status: .working,
            createdAt: .now,
            updatedAt: .now,
            confidenceScore: 0.7
        )
        claims.append(claim)
    }

    private func inferThreadID(for text: String) -> UUID? {
        suggestedThreads(forText: text, excluding: [], limit: 1).first.flatMap { $0.score > 1 ? $0.thread.id : nil }
    }

    private func beginThreadSession(for threadID: UUID) {
        guard activeThreadSessionThreadID != threadID else { return }
        activeThreadSessionThreadID = threadID
        activeThreadSessionID = UUID()
    }

    private func sessionIDForNewEntry(in threadID: UUID?) -> UUID {
        guard let threadID else {
            return UUID()
        }

        if route == .thread, selectedThreadID == threadID {
            if activeThreadSessionThreadID != threadID {
                beginThreadSession(for: threadID)
            }
            return activeThreadSessionID
        }

        return UUID()
    }

    private func classifyKind(for text: String) -> EntryKind {
        let lowered = text.lowercased()
        if lowered.hasSuffix("?") || lowered.hasPrefix("why ") {
            return .question
        }
        if lowered.contains("decide") || lowered.contains("we will") {
            return .decision
        }
        return .capture
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
                let haystack = "\(thread.title) \(thread.prompt) \(thread.summary)".lowercased()
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

    private func touchThread(_ threadID: UUID, fallbackSummary: String) {
        guard let index = threads.firstIndex(where: { $0.id == threadID }) else { return }
        threads[index].lastActiveAt = .now
        threads[index].updatedAt = .now
        if threads[index].summary.isEmpty || threads[index].summary.hasPrefix("Fresh thread") {
            threads[index].summary = fallbackSummary
        }
        if threads[index].nextStep.isEmpty {
            threads[index].nextStep = "Distill what changed."
        }
    }

    func linkRole(for entryID: UUID, in threadID: UUID) -> ThreadLinkRole? {
        guard let entry = entries.first(where: { $0.id == entryID }) else { return nil }
        return entry.threadLinks.first(where: { $0.threadID == threadID })?.role
    }

    func setLinkRole(for entryID: UUID, in threadID: UUID, role: ThreadLinkRole) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        guard let linkIndex = entries[index].threadLinks.firstIndex(where: { $0.threadID == threadID }) else { return }

        if role == .core {
            for currentIndex in entries[index].threadLinks.indices {
                if entries[index].threadLinks[currentIndex].threadID == threadID {
                    entries[index].threadLinks[currentIndex].role = .core
                } else if entries[index].threadLinks[currentIndex].role == .core {
                    entries[index].threadLinks[currentIndex].role = .supporting
                }
            }
            let coreLink = entries[index].threadLinks.remove(at: linkIndex)
            entries[index].threadLinks.insert(coreLink, at: 0)
        } else {
            entries[index].threadLinks[linkIndex].role = role
            if !entries[index].threadLinks.contains(where: { $0.role == .core }),
               let firstIndex = entries[index].threadLinks.indices.first {
                entries[index].threadLinks[firstIndex].role = .core
            }
        }

        persist()
    }

    private func attachEntry(_ entryID: UUID, to threadID: UUID, role: ThreadLinkRole) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        if let existing = entries[index].threadLinks.firstIndex(where: { $0.threadID == threadID }) {
            entries[index].threadLinks[existing].role = role
        } else {
            entries[index].threadLinks.append(EntryThreadLink(threadID: threadID, role: role))
        }
    }

    private func suggestThreadTitle(for text: String) -> String {
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return "Untitled Thread" }
        return String(trimmed.prefix(48))
    }

    private func sessionGroups(from entries: [Entry]) -> [ThreadSession] {
        let sortedEntries = entries.sorted { $0.createdAt > $1.createdAt }
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
            guard let entries = groupedEntries[sessionID], !entries.isEmpty else { return nil }
            let ascendingEntries = entries.sorted { $0.createdAt < $1.createdAt }
            return ThreadSession(
                id: sessionID,
                startedAt: ascendingEntries.first?.createdAt ?? .now,
                endedAt: ascendingEntries.last?.createdAt ?? .now,
                entries: ascendingEntries
            )
        }
    }

    private func buildSummary(for thread: ThreadRecord, entries: [Entry], claims: [Claim]) -> String {
        if let leadingClaim = claims.first?.statement {
            return leadingClaim
        }
        if let latest = entries.first?.content {
            return latest
        }
        return thread.summary
    }

    private func buildOpenLoops(entries: [Entry], claims: [Claim]) -> [String] {
        let questions = entries
            .filter { $0.kind == .question }
            .map(\.content)
        if !questions.isEmpty {
            return Array(questions.prefix(3))
        }
        if claims.isEmpty {
            return ["No stable claim yet. Distill the last few captures into a stronger point."]
        }
        return ["What evidence would change the current conclusion?"]
    }

    private func buildNextSteps(entries: [Entry], claims: [Claim]) -> [String] {
        if claims.isEmpty, let latest = entries.first?.content {
            return ["Turn this into a concrete claim: \(latest)"]
        }
        return [
            "Check whether the strongest claim is backed by enough evidence.",
            "Prepare a writing view if you need to turn this into output."
        ]
    }

    private func detectPhase(entries: [Entry], claims: [Claim]) -> String {
        if claims.count >= 3 {
            return "synthesizing"
        }
        if entries.count >= 8 {
            return "exploring"
        }
        return "collecting"
    }

    private func shouldWriteAnchor(for threadID: UUID) -> Bool {
        let nonSystemEntries = entries(for: threadID).filter { $0.kind != .anchorWritten }
        guard let latestEntry = nonSystemEntries.max(by: { $0.createdAt < $1.createdAt }) else {
            return false
        }

        guard let thread = threads.first(where: { $0.id == threadID }) else {
            return false
        }

        guard let anchorID = thread.currentAnchorID,
              let anchor = anchors.first(where: { $0.id == anchorID }) else {
            return !nonSystemEntries.isEmpty
        }

        return latestEntry.createdAt > anchor.createdAt
    }

    private func seed() {
        let firstSessionID = UUID()
        let secondSessionID = UUID()
        let thirdSessionID = UUID()
        let thread = ThreadRecord(
            id: UUID(),
            title: "Threadnote should restore a problem's last useful state",
            prompt: "How should Threadnote help someone return to a problem without rereading everything?",
            status: .active,
            createdAt: .now.addingTimeInterval(-86_400 * 4),
            updatedAt: .now.addingTimeInterval(-3_600),
            lastActiveAt: .now.addingTimeInterval(-3_600),
            currentAnchorID: nil,
            summary: "Resuming should feel like returning to a workbench, not reopening an archive.",
            nextStep: "Tighten the resume strip so it reads like a handoff from my past self."
        )
        threads = [thread]

        let seededEntries = [
            Entry(id: UUID(), threadLinks: [EntryThreadLink(threadID: thread.id, role: .core)], kind: .capture, content: "Resuming should feel like returning to a workbench, not opening a note archive.", createdAt: .now.addingTimeInterval(-86_400 * 3), sessionID: firstSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.95, confidenceScore: 0.9, inboxState: "resolved"),
            Entry(id: UUID(), threadLinks: [EntryThreadLink(threadID: thread.id, role: .core)], kind: .question, content: "What is the smallest amount of context someone needs to continue a thread?", createdAt: .now.addingTimeInterval(-86_400 * 3 + 1_200), sessionID: firstSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 1, confidenceScore: 0.82, inboxState: "resolved"),
            Entry(id: UUID(), threadLinks: [EntryThreadLink(threadID: thread.id, role: .core)], kind: .capture, content: "Home should foreground recent captures so the user can immediately see where each note went.", createdAt: .now.addingTimeInterval(-86_400 * 2), sessionID: secondSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.92, confidenceScore: 0.88, inboxState: "resolved"),
            Entry(id: UUID(), threadLinks: [EntryThreadLink(threadID: thread.id, role: .core)], kind: .capture, content: "The thread page should start with resume, then show the working stream, then let me continue writing.", createdAt: .now.addingTimeInterval(-86_400), sessionID: thirdSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 1, confidenceScore: 0.93, inboxState: "resolved"),
            Entry(id: UUID(), threadLinks: [], kind: .capture, content: "Lists should collect notes and threads without becoming a second kind of problem space.", createdAt: .now.addingTimeInterval(-43_200), sessionID: UUID(), authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.76, confidenceScore: 0.25, inboxState: "unresolved")
        ]
        entries = seededEntries
        selectedEntryID = seededEntries[3].id
        inboxEntries = seededEntries.filter { $0.threadLinks.isEmpty || $0.inboxState != "resolved" }

        let seededClaim = Claim(
            id: UUID(),
            threadID: thread.id,
            originEntryID: seededEntries[3].id,
            statement: "Threadnote should make continuity visible: recent note, thread, resume strip, then continue.",
            status: .stable,
            createdAt: .now.addingTimeInterval(-50_000),
            updatedAt: .now.addingTimeInterval(-4_000),
            confidenceScore: 0.87
        )
        claims = [seededClaim]

        let anchor = Anchor(
            id: UUID(),
            threadID: thread.id,
            createdAt: .now.addingTimeInterval(-3_600),
            basedOnEntryID: seededEntries[3].id,
            title: "Checkpoint",
            coreQuestion: thread.prompt,
            stateSummary: "The product should restore a problem's last useful state and let the user continue immediately.",
            openLoops: [
                "How should Home show secondary thread links without adding concept tax?",
                "How strong should the resume strip be before it starts feeling like a dashboard?"
            ],
            nextSteps: [
                "Tighten the note card so one primary thread is obvious at a glance.",
                "Refine the resume strip copy so it reads like a handoff from my past self."
            ],
            claimIDs: [seededClaim.id],
            evidenceEntryIDs: Array(seededEntries.prefix(4).map(\.id)),
            phase: "synthesizing"
        )
        anchors = [anchor]
        threads[0].currentAnchorID = anchor.id
        tasks = [
            ThreadTask(
                id: UUID(),
                threadID: thread.id,
                originEntryID: nil,
                title: "Refine the resume-first thread page",
                status: "open",
                createdAt: .now.addingTimeInterval(-1_800),
                updatedAt: .now.addingTimeInterval(-1_800)
            )
        ]
        let list = ListRecord(
            id: UUID(),
            title: "Continuity Pack",
            kind: .pack,
            note: "A lightweight pack collecting the main continuity thread and the strongest supporting notes.",
            createdAt: .now.addingTimeInterval(-2_400),
            updatedAt: .now.addingTimeInterval(-1_200)
        )
        lists = [list]
        listItems = [
            ListItem(
                id: UUID(),
                listID: list.id,
                entityType: .thread,
                entityID: thread.id,
                position: 0,
                note: "Main problem space"
            ),
            ListItem(
                id: UUID(),
                listID: list.id,
                entityType: .entry,
                entityID: seededEntries[3].id,
                position: 1,
                note: "Best articulation of the redesigned thread flow"
            )
        ]
        persist()
    }

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
        lists = snapshot.lists
        listItems = snapshot.listItems
        inboxEntries = entries.filter { $0.threadLinks.isEmpty || $0.inboxState != "resolved" }
        selectedEntryID = entries.sorted { $0.createdAt > $1.createdAt }.first?.id
    }

    private func persist() {
        let snapshot = AppSnapshot(
            sampleDataVersion: currentSampleDataVersion,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: tasks,
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

        let legacyThreadTitle = snapshot.threads.first?.title
        let legacyListTitle = snapshot.lists.first?.title

        return snapshot.threads.count == 1
            && legacyThreadTitle == "AI notes should restore context, not just store text"
            && legacyListTitle == "Launch Pack"
    }
}
