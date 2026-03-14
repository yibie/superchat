import Foundation
@testable import ThreadnoteMVP

@MainActor
final class MockAIBackend: AIBackendClient {
    var isConfigured: Bool = true
    var activeModelID: String? = "mock-model"
    var backendLabel: String = "Mock LLM · mock-model"

    var routeHandler: @MainActor (RoutePlanningRequest) async throws -> RoutePlanningResult = { request in
        let first = request.candidates.first
        return RoutePlanningResult(
            shouldRoute: false,
            selectedThreadID: nil,
            decisionReason: "No mock route decision configured.",
            suggestions: first.map { [RoutePlanningSuggestion(threadID: $0.threadID, reason: "Only mock suggestion")] } ?? [],
            debugPayload: nil
        )
    }

    var resumeHandler: @MainActor (ResumeSynthesisRequest) async throws -> ResumeSynthesisResult = { request in
        let blocks = [
            ThreadBlockPlan(
                kind: .judgment,
                title: "Current Judgment",
                summary: request.currentJudgment,
                items: [request.judgmentBasis].filter { !$0.isEmpty },
                tone: .accent
            )
        ] + request.openLoops.prefix(1).map { openLoop in
            ThreadBlockPlan(
                kind: .gap,
                title: "Main Gap",
                summary: openLoop,
                items: Array(request.openLoops.dropFirst().prefix(2)),
                tone: .warning
            )
        } + [ThreadBlockPlan(
            kind: .nextMove,
            title: "Next Move",
            summary: request.nextAction ?? "Keep moving the thread forward.",
            items: [],
            tone: .success
        )]

        return ResumeSynthesisResult(
            currentJudgment: request.currentJudgment,
            openLoops: request.openLoops,
            nextAction: request.nextAction,
            restartNote: "AI restart note: \(request.currentJudgment)",
            recoveryLines: request.recoveryLines,
            resolvedSoFar: request.resolvedSoFar,
            presentationPlan: ThreadPresentationPlan(
                headline: "AI restart note for \(request.coreQuestion)",
                blocks: blocks,
                primaryAction: request.nextAction
            ),
            debugPayload: nil
        )
    }

    func planRoute(request: RoutePlanningRequest) async throws -> RoutePlanningResult {
        try await routeHandler(request)
    }

    func synthesizeResume(request: ResumeSynthesisRequest) async throws -> ResumeSynthesisResult {
        try await resumeHandler(request)
    }

    func analyzeDiscourse(request: DiscourseAnalysisRequest) async throws -> DiscourseAnalysisResult {
        throw LLMError.notConfigured
    }

    var draftHandler: @MainActor (DraftPreparationRequest) async throws -> DraftPreparationResult = { request in
        let nextSteps: [String]
        if let claim = request.activeClaims.first {
            nextSteps = ["Lead with the strongest claim: \(claim)"]
        } else if let evidence = request.keyEvidence.first?.text {
            nextSteps = ["Turn the strongest evidence into a claim: \(evidence)"]
        } else {
            nextSteps = ["Clarify the decision before writing."]
        }

        return DraftPreparationResult(
            title: "\(request.type.title) Draft",
            openLoops: request.openLoops,
            recommendedNextSteps: nextSteps
        )
    }

    func prepareDraft(request: DraftPreparationRequest) async throws -> DraftPreparationResult {
        try await draftHandler(request)
    }
}

enum TestFixtures {
    static func makeDatabaseURL() -> URL {
        FileManager.default.temporaryDirectory
            .appendingPathComponent(UUID().uuidString)
            .appendingPathExtension("sqlite")
    }

    static func seedDatabase(with snapshot: AppSnapshot) throws -> URL {
        let url = makeDatabaseURL()
        let store = try PersistenceStore(databaseURL: url)
        try store.saveSnapshot(snapshot)
        return url
    }

    @MainActor
    static func makeThreadnoteStore(snapshot: AppSnapshot) throws -> ThreadnoteStore {
        let url = try seedDatabase(with: snapshot)
        let store = ThreadnoteStore(enableLLM: false)
        store.configure(with: url)
        return store
    }

    static func makePersistenceStore(snapshot: AppSnapshot) throws -> PersistenceStore {
        let url = makeDatabaseURL()
        let store = try PersistenceStore(databaseURL: url)
        try store.saveSnapshot(snapshot)
        return store
    }

    static func makeThread(
        title: String,
        lastActiveAt: Date = .now
    ) -> ThreadRecord {
        ThreadRecord(
            id: UUID(),
            title: title,
            prompt: title,
            goalLayer: ThreadGoalLayer(
                goalStatement: title,
                goalType: .research,
                successCondition: "Done",
                currentStage: .framing
            ),
            status: .active,
            createdAt: lastActiveAt,
            updatedAt: lastActiveAt,
            lastActiveAt: lastActiveAt,
            color: .sky
        )
    }

    static func makeEntry(
        threadID: UUID?,
        kind: EntryKind = .note,
        text: String,
        createdAt: Date = .now,
        inboxState: String? = nil
    ) -> Entry {
        Entry(
            id: UUID(),
            threadID: threadID,
            kind: kind,
            body: EntryBody(kind: .text, text: text, url: nil, title: nil, details: nil),
            summaryText: text,
            sourceMetadata: nil,
            createdAt: createdAt,
            sessionID: nil,
            authorType: "user",
            parentEntryID: nil,
            supersedesEntryID: nil,
            importanceScore: nil,
            confidenceScore: nil,
            inboxState: inboxState ?? (threadID == nil ? "unresolved" : "resolved")
        )
    }

    static func makeClaim(
        threadID: UUID,
        statement: String,
        status: ClaimStatus = .stable,
        createdAt: Date = .now
    ) -> Claim {
        Claim(
            id: UUID(),
            threadID: threadID,
            originEntryID: UUID(),
            statement: statement,
            status: status,
            createdAt: createdAt,
            updatedAt: createdAt,
            confidenceScore: 0.9
        )
    }

    static func makeAnchor(
        threadID: UUID,
        coreQuestion: String,
        stateSummary: String,
        createdAt: Date = .now
    ) -> Anchor {
        Anchor(
            id: UUID(),
            threadID: threadID,
            createdAt: createdAt,
            basedOnEntryID: nil,
            title: "Checkpoint",
            coreQuestion: coreQuestion,
            stateSummary: stateSummary,
            openLoops: ["Validate the next Atlas assumption"],
            nextSteps: ["Continue from the current checkpoint"],
            claimIDs: [],
            evidenceEntryIDs: [],
            phase: "researching"
        )
    }

    static func makeSnapshot(
        threads: [ThreadRecord],
        entries: [Entry] = [],
        claims: [Claim] = [],
        anchors: [Anchor] = []
    ) -> AppSnapshot {
        AppSnapshot(
            sampleDataVersion: 9,
            threads: threads,
            entries: entries,
            claims: claims,
            anchors: anchors,
            tasks: [],
            discourseRelations: []
        )
    }
}
