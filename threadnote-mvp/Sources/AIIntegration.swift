import Foundation

enum AIRole: String, CaseIterable, Codable, Identifiable, Sendable {
    case inputAssistant
    case structureAssistant
    case draftAssistant
    case librarian

    var id: Self { self }

    var title: String {
        switch self {
        case .inputAssistant:
            "Input Assistant"
        case .structureAssistant:
            "Structure Assistant"
        case .draftAssistant:
            "Draft Assistant"
        case .librarian:
            "Librarian"
        }
    }
}

enum AIWorkflow: String, CaseIterable, Codable, Identifiable, Sendable {
    case captureClassification
    case threadSuggestion
    case goalTypeSuggestion
    case discourseAnalysis
    case resumeSynthesis
    case draftPreparation
    case listCuration

    var id: Self { self }
}

enum AIProviderKind: String, CaseIterable, Codable, Identifiable, Sendable {
    case heuristics
    case appleFoundationModels
    case openAI
    case anthropic
    case google
    case groq
    case deepSeek
    case xai

    var id: Self { self }

    var title: String {
        switch self {
        case .heuristics:           "Heuristics"
        case .appleFoundationModels:"Apple Foundation Models"
        case .openAI:               "OpenAI"
        case .anthropic:            "Anthropic"
        case .google:               "Google (Gemini)"
        case .groq:                 "Groq"
        case .deepSeek:             "DeepSeek"
        case .xai:                  "xAI (Grok)"
        }
    }

    static var cloudProviders: [AIProviderKind] {
        [.anthropic, .openAI, .google, .groq, .deepSeek, .xai]
    }
}

enum AIProviderChannel: String, Codable, Sendable {
    case local
    case cloud
}

struct AIProviderDescriptor: Hashable, Codable, Identifiable, Sendable {
    let kind: AIProviderKind
    let channel: AIProviderChannel
    let displayName: String
    let supportsStreaming: Bool

    var id: AIProviderKind { kind }

    static let heuristic = AIProviderDescriptor(
        kind: .heuristics,
        channel: .local,
        displayName: "Local Heuristics",
        supportsStreaming: false
    )

    static let appleFoundationModels = AIProviderDescriptor(
        kind: .appleFoundationModels,
        channel: .local,
        displayName: "Apple Foundation Models",
        supportsStreaming: true
    )

    static let openAI = AIProviderDescriptor(
        kind: .openAI,
        channel: .cloud,
        displayName: "OpenAI",
        supportsStreaming: true
    )

    static let anthropic = AIProviderDescriptor(
        kind: .anthropic,
        channel: .cloud,
        displayName: "Anthropic",
        supportsStreaming: true
    )

    static let google = AIProviderDescriptor(
        kind: .google,
        channel: .cloud,
        displayName: "Google (Gemini)",
        supportsStreaming: true
    )

    static let groq = AIProviderDescriptor(
        kind: .groq,
        channel: .cloud,
        displayName: "Groq",
        supportsStreaming: true
    )

    static let deepSeek = AIProviderDescriptor(
        kind: .deepSeek,
        channel: .cloud,
        displayName: "DeepSeek",
        supportsStreaming: true
    )

    static let xai = AIProviderDescriptor(
        kind: .xai,
        channel: .cloud,
        displayName: "xAI (Grok)",
        supportsStreaming: true
    )
}

struct AIWorkflowBinding: Hashable, Codable, Sendable {
    let workflow: AIWorkflow
    let role: AIRole
    let primaryProvider: AIProviderKind
    let fallbackProviders: [AIProviderKind]
}

struct AIProductPolicy: Hashable, Codable, Sendable {
    let workflowBindings: [AIWorkflowBinding]
    let disallowedWorkflows: [AIWorkflow]
    let writingReplacementAllowed: Bool

    func binding(for workflow: AIWorkflow) -> AIWorkflowBinding? {
        workflowBindings.first(where: { $0.workflow == workflow })
    }

    static let `default` = AIProductPolicy(
        workflowBindings: [
            AIWorkflowBinding(
                workflow: .captureClassification,
                role: .inputAssistant,
                primaryProvider: .heuristics,
                fallbackProviders: [.appleFoundationModels, .openAI]
            ),
            AIWorkflowBinding(
                workflow: .threadSuggestion,
                role: .inputAssistant,
                primaryProvider: .heuristics,
                fallbackProviders: [.appleFoundationModels, .openAI]
            ),
            AIWorkflowBinding(
                workflow: .goalTypeSuggestion,
                role: .inputAssistant,
                primaryProvider: .heuristics,
                fallbackProviders: [.appleFoundationModels, .openAI]
            ),
            AIWorkflowBinding(
                workflow: .discourseAnalysis,
                role: .structureAssistant,
                primaryProvider: .appleFoundationModels,
                fallbackProviders: [.openAI, .anthropic, .heuristics]
            ),
            AIWorkflowBinding(
                workflow: .resumeSynthesis,
                role: .structureAssistant,
                primaryProvider: .appleFoundationModels,
                fallbackProviders: [.openAI, .anthropic, .heuristics]
            ),
            AIWorkflowBinding(
                workflow: .draftPreparation,
                role: .draftAssistant,
                primaryProvider: .appleFoundationModels,
                fallbackProviders: [.openAI, .anthropic, .heuristics]
            ),
            AIWorkflowBinding(
                workflow: .listCuration,
                role: .librarian,
                primaryProvider: .heuristics,
                fallbackProviders: [.appleFoundationModels, .openAI]
            )
        ],
        disallowedWorkflows: [],
        writingReplacementAllowed: false
    )
}

struct ThreadnoteAIConfiguration: Hashable, Codable, Sendable {
    let availableProviders: [AIProviderDescriptor]
    let policy: AIProductPolicy

    func primaryProvider(for workflow: AIWorkflow) -> AIProviderKind? {
        policy.binding(for: workflow)?.primaryProvider
    }

    static let `default` = ThreadnoteAIConfiguration(
        availableProviders: [
            .heuristic,
            .appleFoundationModels,
            .openAI,
            .anthropic,
            .google,
            .groq,
            .deepSeek,
            .xai
        ],
        policy: .default
    )
}

struct AIThreadCandidate: Hashable, Codable, Identifiable, Sendable {
    let id: UUID
    let title: String
    let prompt: String
    let lastActiveAt: Date
}

struct AISnippet: Hashable, Codable, Sendable {
    let id: UUID
    let text: String
    let kind: EntryKind?
}

struct CaptureClassificationRequest: Hashable, Codable, Sendable {
    let text: String
    let explicitTag: CaptureTag?
}

struct CaptureClassificationResult: Hashable, Codable, Sendable {
    let semanticKind: EntryKind
    let preservedExplicitTag: CaptureTag?
    let rationale: String
}

struct ThreadSuggestionRequest: Hashable, Codable, Sendable {
    let noteSummary: String
    let excludedThreadIDs: [UUID]
    let candidateThreads: [AIThreadCandidate]
}

struct ThreadSuggestionResult: Hashable, Codable, Sendable {
    let suggestions: [AIThreadSuggestion]
}

struct GoalTypeSuggestionRequest: Hashable, Codable, Sendable {
    let goalStatement: String
}

struct GoalTypeSuggestionResult: Hashable, Codable, Sendable {
    let goalType: ThreadGoalType
    let rationale: String
}

struct AIThreadSuggestion: Hashable, Codable, Sendable {
    let threadID: UUID
    let score: Int
    let rationale: String
}

struct DiscourseAnalysisRequest: Hashable, Codable, Sendable {
    let threadID: UUID
    let snippets: [AISnippet]
}

struct AIRelationPair: Hashable, Codable, Sendable {
    let sourceEntryID: UUID
    let targetEntryID: UUID
}

struct DiscourseAnalysisResult: Hashable, Codable, Sendable {
    let relationPairs: [AIRelationPair]
    let rationale: String
}

struct ResumeSynthesisRequest: Hashable, Codable, Sendable {
    let threadID: UUID
    let coreQuestion: String
    let goalLayer: ThreadGoalLayer
    let activeClaims: [String]
    let openLoops: [String]
    let recentNotes: [AISnippet]
    let evidenceCount: Int
    let sourceCount: Int
}

struct ResumeSynthesisResult: Hashable, Codable, Sendable {
    let currentJudgment: String
    let openLoops: [String]
    let nextAction: String?
    let restartNote: String
    let recoveryLines: [ResumeRecoveryLine]
    let resolvedSoFar: [ResolvedItem]
}

struct DraftPreparationRequest: Hashable, Codable, Sendable {
    let threadID: UUID
    let type: PreparedViewType
    let coreQuestion: String
    let activeClaims: [String]
    let keyEvidence: [AISnippet]
    let openLoops: [String]
    let recentNotes: [AISnippet]
}

struct DraftPreparationResult: Hashable, Codable, Sendable {
    let title: String
    let openLoops: [String]
    let recommendedNextSteps: [String]
}

struct ListCurationRequest: Hashable, Codable, Sendable {
    let listID: UUID
    let itemSummaries: [AISnippet]
}

struct ListCurationResult: Hashable, Codable, Sendable {
    let promotedItemIDs: [UUID]
    let rationale: String
}

enum AIRequest: Hashable, Codable, Sendable {
    case captureClassification(CaptureClassificationRequest)
    case threadSuggestion(ThreadSuggestionRequest)
    case goalTypeSuggestion(GoalTypeSuggestionRequest)
    case discourseAnalysis(DiscourseAnalysisRequest)
    case resumeSynthesis(ResumeSynthesisRequest)
    case draftPreparation(DraftPreparationRequest)
    case listCuration(ListCurationRequest)

    var workflow: AIWorkflow {
        switch self {
        case .captureClassification:
            .captureClassification
        case .threadSuggestion:
            .threadSuggestion
        case .goalTypeSuggestion:
            .goalTypeSuggestion
        case .discourseAnalysis:
            .discourseAnalysis
        case .resumeSynthesis:
            .resumeSynthesis
        case .draftPreparation:
            .draftPreparation
        case .listCuration:
            .listCuration
        }
    }
}

enum AIResponse: Hashable, Codable, Sendable {
    case captureClassification(CaptureClassificationResult)
    case threadSuggestion(ThreadSuggestionResult)
    case goalTypeSuggestion(GoalTypeSuggestionResult)
    case discourseAnalysis(DiscourseAnalysisResult)
    case resumeSynthesis(ResumeSynthesisResult)
    case draftPreparation(DraftPreparationResult)
    case listCuration(ListCurationResult)
}

enum AIProviderError: Error, LocalizedError, Sendable {
    case workflowNotSupported(provider: AIProviderKind, workflow: AIWorkflow)
    case providerNotConfigured(provider: AIProviderKind, workflow: AIWorkflow)

    var errorDescription: String? {
        switch self {
        case let .workflowNotSupported(provider, workflow):
            return "\(provider.title) does not support \(workflow.rawValue)."
        case let .providerNotConfigured(provider, workflow):
            return "\(provider.title) has no implementation for \(workflow.rawValue) yet."
        }
    }
}

protocol ThreadnoteAIProvider: Sendable {
    var descriptor: AIProviderDescriptor { get }
    var supportedWorkflows: Set<AIWorkflow> { get }

    func run(_ request: AIRequest) throws -> AIResponse
}

struct HeuristicAIProvider: ThreadnoteAIProvider {
    let descriptor = AIProviderDescriptor.heuristic
    let supportedWorkflows: Set<AIWorkflow> = [
        .captureClassification,
        .threadSuggestion,
        .goalTypeSuggestion,
        .resumeSynthesis,
        .draftPreparation,
        .listCuration
    ]

    func run(_ request: AIRequest) throws -> AIResponse {
        guard supportedWorkflows.contains(request.workflow) else {
            throw AIProviderError.workflowNotSupported(provider: descriptor.kind, workflow: request.workflow)
        }

        switch request {
        case let .captureClassification(request):
            return .captureClassification(classifyCapture(request))
        case let .threadSuggestion(request):
            return .threadSuggestion(suggestThreads(request))
        case let .goalTypeSuggestion(request):
            return .goalTypeSuggestion(suggestGoalType(request))
        case let .resumeSynthesis(request):
            return .resumeSynthesis(synthesizeResume(request))
        case let .draftPreparation(request):
            return .draftPreparation(prepareDraft(request))
        case let .listCuration(request):
            return .listCuration(curateList(request))
        case .discourseAnalysis:
            throw AIProviderError.workflowNotSupported(provider: descriptor.kind, workflow: .discourseAnalysis)
        }
    }

    private func classifyCapture(_ request: CaptureClassificationRequest) -> CaptureClassificationResult {
        if let explicitTag = request.explicitTag {
            return CaptureClassificationResult(
                semanticKind: explicitTag.entryKind,
                preservedExplicitTag: explicitTag,
                rationale: "Explicit tag overrides inference."
            )
        }

        return CaptureClassificationResult(
            semanticKind: .note,
            preservedExplicitTag: nil,
            rationale: "No explicit role was set, so the capture defaults to note."
        )
    }

    private func suggestThreads(_ request: ThreadSuggestionRequest) -> ThreadSuggestionResult {
        let tokens = request.noteSummary.lowercased()
            .split(separator: " ")
            .map(String.init)
            .filter { $0.count > 2 }

        guard !tokens.isEmpty else {
            return ThreadSuggestionResult(suggestions: [])
        }

        let suggestions = request.candidateThreads
            .filter { !request.excludedThreadIDs.contains($0.id) }
            .map { thread in
                let haystack = "\(thread.title) \(thread.prompt)".lowercased()
                let matchedTokens = tokens.filter { haystack.contains($0) }
                let score = matchedTokens.count
                let rationale: String
                if score >= 2 {
                    rationale = "Matches \(score) terms from this note."
                } else if let first = tokens.first, haystack.contains(first) {
                    rationale = "Matches the main phrase and was active recently."
                } else {
                    rationale = "Recently active related thread."
                }
                return AIThreadSuggestion(threadID: thread.id, score: score, rationale: rationale)
            }
            .filter { $0.score > 0 }
            .sorted { lhs, rhs in
                if lhs.score == rhs.score {
                    let lhsThread = request.candidateThreads.first(where: { $0.id == lhs.threadID })
                    let rhsThread = request.candidateThreads.first(where: { $0.id == rhs.threadID })
                    return (lhsThread?.lastActiveAt ?? .distantPast) > (rhsThread?.lastActiveAt ?? .distantPast)
                }
                return lhs.score > rhs.score
            }

        return ThreadSuggestionResult(suggestions: suggestions)
    }

    private func suggestGoalType(_ request: GoalTypeSuggestionRequest) -> GoalTypeSuggestionResult {
        let type = ThreadGoalType.suggested(for: request.goalStatement)
        let rationale: String
        switch type {
        case .build:
            rationale = "The goal reads like creating or shipping something."
        case .study:
            rationale = "The goal reads like understanding a body of work or sample set."
        case .research:
            rationale = "The goal reads like surveying external products, markets, or evidence."
        }
        return GoalTypeSuggestionResult(goalType: type, rationale: rationale)
    }

    private func synthesizeResume(_ request: ResumeSynthesisRequest) -> ResumeSynthesisResult {
        let currentJudgment: String
        if let leadingClaim = request.activeClaims.first {
            currentJudgment = leadingClaim
        } else if let strongEvidence = request.recentNotes.first(where: { $0.kind == .evidence })?.text {
            currentJudgment = strongEvidence
        } else if let latest = request.recentNotes.first?.text {
            currentJudgment = latest
        } else {
            currentJudgment = request.coreQuestion
        }

        let openLoops: [String]
        if !request.openLoops.isEmpty {
            openLoops = request.openLoops
        } else {
            let questionNotes = request.recentNotes
                .filter { $0.kind == .question }
                .map(\.text)
            openLoops = questionNotes.isEmpty
                ? ["What evidence or source would most change the current claim?"]
                : Array(questionNotes.prefix(3))
        }

        let nextAction: String?
        if request.activeClaims.isEmpty, let latest = request.recentNotes.first?.text {
            nextAction = "Turn this into a clearer claim: \(latest)"
        } else if !request.recentNotes.contains(where: { $0.kind == .evidence }) {
            nextAction = "Add evidence that strengthens or weakens the current claim."
        } else if !request.recentNotes.contains(where: { $0.kind == .source }) {
            nextAction = "Link a source or reference that grounds the latest evidence."
        } else {
            nextAction = "Reply on the strongest note to tighten the argument before writing."
        }

        let recoveryLines = recoveryLines(
            goalLayer: request.goalLayer,
            currentJudgment: currentJudgment,
            openLoops: openLoops,
            nextAction: nextAction,
            claims: request.activeClaims,
            recentNotes: request.recentNotes,
            evidenceCount: request.evidenceCount,
            sourceCount: request.sourceCount
        )
        let resolved = resolvedSoFar(
            goalLayer: request.goalLayer,
            claims: request.activeClaims,
            recentNotes: request.recentNotes
        )
        let restartNote = restartNote(
            goalLayer: request.goalLayer,
            recoveryLines: recoveryLines
        )

        return ResumeSynthesisResult(
            currentJudgment: currentJudgment,
            openLoops: openLoops,
            nextAction: nextAction,
            restartNote: restartNote,
            recoveryLines: recoveryLines,
            resolvedSoFar: resolved
        )
    }

    private func prepareDraft(_ request: DraftPreparationRequest) -> DraftPreparationResult {
        let recommendedNextSteps: [String]
        if !request.activeClaims.isEmpty {
            recommendedNextSteps = [
                "Lead with the strongest claim before expanding supporting detail.",
                "Use only evidence that changes the reader's confidence."
            ]
        } else if let firstEvidence = request.keyEvidence.first?.text {
            recommendedNextSteps = [
                "Turn the strongest evidence into a claim: \(firstEvidence)",
                "Only open a full draft after the thread has a stable judgment."
            ]
        } else {
            recommendedNextSteps = [
                "Clarify the claim before drafting.",
                "Keep the draft scoped to the current thread, not the whole archive."
            ]
        }

        return DraftPreparationResult(
            title: "\(request.type.title) Draft",
            openLoops: request.openLoops,
            recommendedNextSteps: recommendedNextSteps
        )
    }

    private func curateList(_ request: ListCurationRequest) -> ListCurationResult {
        let promoted = request.itemSummaries.prefix(2).map(\.id)
        return ListCurationResult(
            promotedItemIDs: promoted,
            rationale: "Local heuristics promoted the shortest path back into the resource view."
        )
    }

    private func recoveryLines(
        goalLayer: ThreadGoalLayer,
        currentJudgment: String,
        openLoops: [String],
        nextAction: String?,
        claims: [String],
        recentNotes: [AISnippet],
        evidenceCount: Int,
        sourceCount: Int
    ) -> [ResumeRecoveryLine] {
        let gap = strongestGap(goalLayer: goalLayer, openLoops: openLoops, recentNotes: recentNotes)
        let move = bestMove(goalLayer: goalLayer, nextAction: nextAction, recentNotes: recentNotes)

        switch goalLayer.goalType {
        case .build:
            return [
                ResumeRecoveryLine(title: "Current Direction", body: currentJudgment),
                ResumeRecoveryLine(title: "Blocked By", body: gap),
                ResumeRecoveryLine(title: "Next Decision", body: move)
            ]
        case .study:
            return [
                ResumeRecoveryLine(title: "Current Read", body: currentJudgment),
                ResumeRecoveryLine(title: "Still Unclear", body: gap),
                ResumeRecoveryLine(title: "Next Observation", body: move)
            ]
        case .research:
            return [
                ResumeRecoveryLine(title: "Current Map", body: currentJudgment),
                ResumeRecoveryLine(title: "Missing Coverage", body: gap),
                ResumeRecoveryLine(title: "Next Probe", body: move)
            ]
        }
    }

    private func strongestGap(goalLayer: ThreadGoalLayer, openLoops: [String], recentNotes: [AISnippet]) -> String {
        if let first = openLoops.first {
            return first
        }
        switch goalLayer.currentStage {
        case .framing:
            return "The goal still needs a sharper frame before the thread can move quickly."
        case .gathering:
            return "The thread still needs stronger material before the current read is reliable."
        case .synthesizing:
            return "The thread needs one tighter comparison or synthesis step to stabilize its direction."
        case .concluding:
            return "No major blocker is visible right now."
        }
    }

    private func bestMove(goalLayer: ThreadGoalLayer, nextAction: String?, recentNotes: [AISnippet]) -> String {
        if let nextAction {
            return nextAction
        }
        switch goalLayer.goalType {
        case .build:
            return "Make the next concrete product decision that reduces ambiguity."
        case .study:
            return "Record the next observation that changes how you read the material."
        case .research:
            return "Probe one missing part of the landscape before widening the search."
        }
    }

    private func resolvedSoFar(goalLayer: ThreadGoalLayer, claims: [String], recentNotes: [AISnippet]) -> [ResolvedItem] {
        var items: [ResolvedItem] = []
        let now = Date()
        for claim in claims.prefix(2) {
            items.append(ResolvedItem(text: claim, statusLabel: "Decided", resolvedAt: now))
        }
        for note in recentNotes where items.count < 3 {
            guard note.kind == .evidence || note.kind == .source else { continue }
            items.append(ResolvedItem(text: note.text, statusLabel: "Confirmed", resolvedAt: now))
        }
        if items.isEmpty {
            items.append(ResolvedItem(text: "The goal is defined and the thread can resume from a stable starting point.", statusLabel: "Confirmed", resolvedAt: now))
        }
        return Array(items.prefix(3))
    }

    private func restartNote(goalLayer: ThreadGoalLayer, recoveryLines: [ResumeRecoveryLine]) -> String {
        guard !recoveryLines.isEmpty else {
            return "- Return to the strongest thread state available.\n- Continue from the next concrete move."
        }
        return recoveryLines
            .map { "- \($0.title): \($0.body)" }
            .joined(separator: "\n")
    }

    func runFallbackRecovery(
        goalLayer: ThreadGoalLayer,
        currentJudgment: String,
        openLoops: [String],
        nextAction: String?,
        claims: [String],
        recentNotes: [AISnippet],
        evidenceCount: Int,
        sourceCount: Int
    ) -> (restartNote: String, lines: [ResumeRecoveryLine], resolved: [ResolvedItem]) {
        let lines = recoveryLines(
            goalLayer: goalLayer,
            currentJudgment: currentJudgment,
            openLoops: openLoops,
            nextAction: nextAction,
            claims: claims,
            recentNotes: recentNotes,
            evidenceCount: evidenceCount,
            sourceCount: sourceCount
        )
        return (
            restartNote(goalLayer: goalLayer, recoveryLines: lines),
            lines,
            resolvedSoFar(
                goalLayer: goalLayer,
                claims: claims,
                recentNotes: recentNotes
            )
        )
    }
}

struct ThreadnoteAIRuntime: Sendable {
    let configuration: ThreadnoteAIConfiguration
    let heuristicProvider: HeuristicAIProvider

    init(configuration: ThreadnoteAIConfiguration) {
        self.configuration = configuration
        self.heuristicProvider = HeuristicAIProvider()
    }

    func run(_ request: AIRequest) throws -> AIResponse {
        let binding = configuration.policy.binding(for: request.workflow)
        let providerOrder = ([binding?.primaryProvider].compactMap { $0 } + (binding?.fallbackProviders ?? []))
        for provider in providerOrder {
            if provider == heuristicProvider.descriptor.kind, heuristicProvider.supportedWorkflows.contains(request.workflow) {
                return try heuristicProvider.run(request)
            }
        }
        throw AIProviderError.providerNotConfigured(
            provider: binding?.primaryProvider ?? .heuristics,
            workflow: request.workflow
        )
    }
}
