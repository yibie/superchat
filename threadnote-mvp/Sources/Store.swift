import Foundation
import Observation

@MainActor
@Observable
final class ThreadnoteStore {
    private let currentSampleDataVersion = 3
    var threads: [ThreadRecord] = []
    var entries: [Entry] = []
    var claims: [Claim] = []
    var anchors: [Anchor] = []
    var tasks: [ThreadTask] = []
    var selectedThreadID: UUID?
    var preparedView: PreparedView?
    var preparedViewType: PreparedViewType = .writing
    var quickCaptureDraft = QuickCaptureDraft()
    var inlineNoteDraft = ""

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
    }

    // MARK: - Navigation

    var isInWorkbench: Bool { selectedThreadID != nil }

    var selectedThread: ThreadRecord? {
        guard let selectedThreadID else { return nil }
        return threads.first(where: { $0.id == selectedThreadID })
    }

    func goToStream() {
        if let tid = selectedThreadID {
            maybeWriteAnchorIfNeeded(for: tid)
        }
        selectedThreadID = nil
        preparedView = nil
        activeThreadSessionThreadID = nil
    }

    func openThread(_ threadID: UUID) {
        if let current = selectedThreadID, current != threadID {
            maybeWriteAnchorIfNeeded(for: current)
        }
        selectedThreadID = threadID
        beginThreadSession(for: threadID)
        if preparedView?.threadID != threadID {
            preparedView = nil
        }
    }

    // MARK: - Stream computed properties

    var streamEntries: [Entry] {
        entries
            .filter(isUserVisible)
            .sorted { $0.createdAt > $1.createdAt }
    }

    var inboxEntries: [Entry] {
        entries.filter { $0.threadID == nil && isUserVisible($0) }
    }

    var homeThreads: [ThreadRecord] {
        threads.sorted { $0.lastActiveAt > $1.lastActiveAt }
    }

    func thread(for entry: Entry) -> ThreadRecord? {
        guard let threadID = entry.threadID else { return nil }
        return threads.first(where: { $0.id == threadID })
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

    func isEntry(_ entryID: UUID, inThread threadID: UUID) -> Bool {
        entries.contains { $0.id == entryID && $0.threadID == threadID }
    }

    // MARK: - Anchor

    func latestAnchor(for threadID: UUID) -> Anchor? {
        anchors
            .filter { $0.threadID == threadID }
            .max(by: { $0.createdAt < $1.createdAt })
    }

    func deltaEntries(for threadID: UUID) -> [Entry] {
        let visible = visibleEntries(for: threadID)
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
        let delta = deltaEntries(for: threadID)
        let recentSessions = sessionGroups(from: delta)
        let relatedEntries = Array(visible.prefix(6))
        let nextSteps = anchor?.nextSteps ?? buildNextSteps(entries: visible, claims: threadClaims)

        return ThreadState(
            threadID: threadID,
            coreQuestion: anchor?.coreQuestion ?? thread.prompt,
            currentJudgment: anchor?.stateSummary ?? buildSummary(for: thread, entries: visible, claims: threadClaims),
            openLoops: anchor?.openLoops ?? buildOpenLoops(entries: visible, claims: threadClaims),
            nextAction: nextSteps.first,
            keyClaims: Array(threadClaims.prefix(3)),
            recentSessions: recentSessions,
            relatedEntries: relatedEntries,
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

        let inferredThreadID = inferThreadID(for: text)
        let sessionID = sessionIDForNewEntry(in: inferredThreadID)
        let entry = Entry(
            id: UUID(),
            threadID: inferredThreadID,
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
        if let inferredThreadID {
            touchThread(inferredThreadID)
            maybePromoteClaim(from: entry)
        }

        quickCaptureDraft = QuickCaptureDraft()
        persist()
    }

    func appendInlineNote(to threadID: UUID) {
        let text = inlineNoteDraft.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }
        let entry = Entry(
            id: UUID(),
            threadID: threadID,
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
        touchThread(threadID)
        maybePromoteClaim(from: entry)
        inlineNoteDraft = ""
        persist()
    }

    func createThread(title: String? = nil, seedText: String = "") {
        let newTitle = title ?? suggestThreadTitle(for: seedText)
        let thread = ThreadRecord(
            id: UUID(),
            title: newTitle,
            prompt: seedText.isEmpty ? "A new problem worth returning to." : seedText,
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
        let thread = ThreadRecord(
            id: UUID(),
            title: suggestThreadTitle(for: entry.content),
            prompt: entry.content,
            status: .active,
            createdAt: .now,
            updatedAt: .now,
            lastActiveAt: .now
        )
        threads.insert(thread, at: 0)
        setEntryThread(entryID, to: thread.id)
        openThread(thread.id)
    }

    func setEntryThread(_ entryID: UUID, to threadID: UUID) {
        guard let index = entries.firstIndex(where: { $0.id == entryID }) else { return }
        entries[index].threadID = threadID
        entries[index].inboxState = "resolved"
        touchThread(threadID)
        maybePromoteClaim(from: entries[index])
        persist()
    }

    func resolveInboxEntry(_ entry: Entry, to threadID: UUID) {
        setEntryThread(entry.id, to: threadID)
    }

    // MARK: - Anchor writing

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
            threadID: threadID,
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

    // MARK: - Suggestions

    func suggestedThreads(for entry: Entry, limit: Int = 3) -> [ThreadSuggestion] {
        let excluded = entry.threadID.map { Set([$0]) } ?? []
        return suggestedThreads(forText: entry.content, excluding: excluded, limit: limit)
    }

    // MARK: - Private helpers

    private func maybePromoteClaim(from entry: Entry) {
        let normalized = entry.content.lowercased()
        guard let threadID = entry.threadID else { return }
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
        guard let threadID else { return UUID() }
        if isInWorkbench, selectedThreadID == threadID {
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

    private func touchThread(_ threadID: UUID) {
        guard let index = threads.firstIndex(where: { $0.id == threadID }) else { return }
        threads[index].lastActiveAt = .now
        threads[index].updatedAt = .now
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
        return thread.prompt
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
        guard let anchor = latestAnchor(for: threadID) else {
            return !nonSystemEntries.isEmpty
        }
        let entriesSinceAnchor = nonSystemEntries.filter { $0.createdAt > anchor.createdAt }
        return entriesSinceAnchor.count >= 3
    }

    // MARK: - Seed data

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
            lastActiveAt: .now.addingTimeInterval(-3_600)
        )
        threads = [thread]

        let seededEntries = [
            Entry(id: UUID(), threadID: thread.id, kind: .capture, content: "Resuming should feel like returning to a workbench, not opening a note archive.", createdAt: .now.addingTimeInterval(-86_400 * 3), sessionID: firstSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.95, confidenceScore: 0.9, inboxState: "resolved"),
            Entry(id: UUID(), threadID: thread.id, kind: .question, content: "What is the smallest amount of context someone needs to continue a thread?", createdAt: .now.addingTimeInterval(-86_400 * 3 + 1_200), sessionID: firstSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 1, confidenceScore: 0.82, inboxState: "resolved"),
            Entry(id: UUID(), threadID: thread.id, kind: .capture, content: "Home should foreground recent captures so the user can immediately see where each note went.", createdAt: .now.addingTimeInterval(-86_400 * 2), sessionID: secondSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.92, confidenceScore: 0.88, inboxState: "resolved"),
            Entry(id: UUID(), threadID: thread.id, kind: .capture, content: "The thread page should start with resume, then show the working stream, then let me continue writing.", createdAt: .now.addingTimeInterval(-86_400), sessionID: thirdSessionID, authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 1, confidenceScore: 0.93, inboxState: "resolved"),
            Entry(id: UUID(), threadID: nil, kind: .capture, content: "Lists should collect notes and threads without becoming a second kind of problem space.", createdAt: .now.addingTimeInterval(-43_200), sessionID: UUID(), authorType: "user", parentEntryID: nil, supersedesEntryID: nil, importanceScore: 0.76, confidenceScore: 0.25, inboxState: "unresolved")
        ]
        entries = seededEntries

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
    }

    private func persist() {
        let snapshot = AppSnapshot(
            sampleDataVersion: currentSampleDataVersion,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: tasks
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
        return title == "AI notes should restore context, not just store text"
            || title == "Threadnote should restore a problem's last useful state"
    }
}
