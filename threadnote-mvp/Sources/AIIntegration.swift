import Foundation

enum AIProviderKind: String, CaseIterable, Codable, Identifiable, Sendable {
    case openAI
    case anthropic
    case google
    case groq
    case deepSeek
    case xai
    case ollama         // local: Ollama (http://localhost:11434/v1)
    case lmStudio       // local: LM Studio (http://localhost:1234/v1)
    case openAICompat   // custom: any OpenAI-compatible endpoint

    var id: Self { self }

    var title: String {
        switch self {
        case .openAI:       "OpenAI"
        case .anthropic:    "Anthropic"
        case .google:       "Google (Gemini)"
        case .groq:         "Groq"
        case .deepSeek:     "DeepSeek"
        case .xai:          "xAI (Grok)"
        case .ollama:       "Ollama (Local)"
        case .lmStudio:     "LM Studio (Local)"
        case .openAICompat: "OpenAI-Compatible"
        }
    }

    static var cloudProviders: [AIProviderKind] {
        [.anthropic, .openAI, .google, .groq, .deepSeek, .xai]
    }

    static var localProviders: [AIProviderKind] {
        [.ollama, .lmStudio, .openAICompat]
    }

    static var supportedBackends: [AIProviderKind] {
        cloudProviders + localProviders
    }
}

struct AISnippet: Hashable, Codable, Sendable {
    let id: UUID
    let text: String
    let kind: EntryKind?
}

struct DiscourseAnalysisRequest: Hashable, Codable, Sendable {
    let threadID: UUID
    let snippets: [AISnippet]
}

struct AIRelationPair: Hashable, Codable, Sendable {
    let sourceEntryID: UUID
    let targetEntryID: UUID
    let kind: DiscourseRelationKind
}

struct DiscourseAnalysisResult: Hashable, Codable, Sendable {
    let relationPairs: [AIRelationPair]
    let rationale: String
}

struct ThreadStateInput: Hashable {
    let threadID: UUID
    let coreQuestion: String
    let goalLayer: ThreadGoalLayer
    let signature: ThreadSignature
    let activeClaims: [Claim]
    let questions: [Entry]
    let evidenceEntries: [Entry]
    let sourceEntries: [Entry]
    let recentEntries: [Entry]
    let recentNotes: [AISnippet]
    let anchor: Anchor?
    let entriesSinceAnchor: [Entry]
    let discourseRelations: [DiscourseRelation]
}

struct JudgmentCandidate: Hashable {
    let text: String
    let basis: String
    let supportSummary: String
    let statusLabel: String
    let confidence: Double
}

enum OpenLoopKind: String, Hashable {
    case claimMissing
    case evidenceMissing
    case sourceMissing
    case contradiction
    case staleCheckpoint
    case unansweredQuestion
    case anchorLoop
    case synthesis
}

struct OpenLoopCandidate: Hashable {
    let text: String
    let kind: OpenLoopKind
    let priority: Int
}

struct ThreadBlockPlan: Hashable, Codable, Sendable {
    let kind: ThreadBlockKind
    let title: String?
    let summary: String?
    let items: [String]
    let tone: ThreadBlockTone?
}

struct ThreadPresentationPlan: Hashable, Codable, Sendable {
    let headline: String
    let blocks: [ThreadBlockPlan]
    let primaryAction: String?
}

struct LLMResumeDebugPayload: Hashable, Codable, Sendable {
    let backendLabel: String
    let configuredModelID: String?
    let responseModelID: String?
    let responseID: String?
    let finishReason: String?
    let warnings: [String]
    let parsedResponse: String?
    let rawResponseBody: String?
    let updatedAt: Date
}

struct RoutePlanningRequest: Hashable, Codable, Sendable {
    let entryID: UUID
    let normalizedText: String
    let detectedItemType: String
    let detectedObjects: [String]
    let candidateClaims: [String]
    let routingQueries: [String]
    let candidates: [RouteCandidateDebug]
}

struct RoutePlanningSuggestion: Hashable, Codable, Sendable {
    let threadID: UUID
    let reason: String
}

struct LLMRouteDebugPayload: Hashable, Codable, Sendable {
    let backendLabel: String
    let configuredModelID: String?
    let responseModelID: String?
    let responseID: String?
    let finishReason: String?
    let warnings: [String]
    let parsedResponse: String?
    let rawResponseBody: String?
    let updatedAt: Date
}

struct RoutePlanningResult: Hashable, Codable, Sendable {
    let shouldRoute: Bool
    let selectedThreadID: UUID?
    let decisionReason: String
    let suggestions: [RoutePlanningSuggestion]
    let debugPayload: LLMRouteDebugPayload?
}

enum PresentationPlanApplicationStatus: Hashable, Sendable {
    case applied
    case invalid(String)
}

enum PresentationPlanValidationError: Error, Hashable, Sendable {
    case emptyHeadline
    case noValidBlocks

    var message: String {
        switch self {
        case .emptyHeadline:
            return "AI plan headline was empty."
        case .noValidBlocks:
            return "AI plan did not produce any valid blocks after validation."
        }
    }
}

struct PresentationApplicationResult: Hashable, Sendable {
    let presentation: ThreadPresentation?
    let status: PresentationPlanApplicationStatus
}

struct ThreadStateSnapshot: Hashable {
    let currentJudgment: String
    let judgmentBasis: String
    let openLoops: [String]
    let nextAction: String?
    let presentation: ThreadPresentation
    let restartNote: String
    let recoveryLines: [ResumeRecoveryLine]
    let resolvedSoFar: [ResolvedItem]
}

struct ResumeSynthesisRequest: Hashable, Codable, Sendable {
    let threadID: UUID
    let coreQuestion: String
    let goalLayer: ThreadGoalLayer
    let activeClaims: [String]
    let currentJudgment: String
    let judgmentBasis: String
    let openLoops: [String]
    let nextAction: String?
    let recoveryLines: [ResumeRecoveryLine]
    let resolvedSoFar: [ResolvedItem]
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
    let presentationPlan: ThreadPresentationPlan?
    let debugPayload: LLMResumeDebugPayload?

    init(
        currentJudgment: String,
        openLoops: [String],
        nextAction: String?,
        restartNote: String,
        recoveryLines: [ResumeRecoveryLine],
        resolvedSoFar: [ResolvedItem],
        presentationPlan: ThreadPresentationPlan? = nil,
        debugPayload: LLMResumeDebugPayload? = nil
    ) {
        self.currentJudgment = currentJudgment
        self.openLoops = openLoops
        self.nextAction = nextAction
        self.restartNote = restartNote
        self.recoveryLines = recoveryLines
        self.resolvedSoFar = resolvedSoFar
        self.presentationPlan = presentationPlan
        self.debugPayload = debugPayload
    }
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

struct DeterministicAIHelper {
    func suggestGoalType(for goalStatement: String) -> ThreadGoalType {
        ThreadGoalType.suggested(for: goalStatement)
    }

    func compactWorkingRead(_ text: String, maxLength: Int = 96) -> String {
        let normalized = normalizedText(text)
        guard !normalized.isEmpty else { return "" }

        let lines = text
            .split(whereSeparator: \.isNewline)
            .map { normalizedText(String($0)) }
            .filter { !$0.isEmpty }

        if let heading = lines.first(where: isUsefulHeading) {
            return trimmedExcerpt(heading, limit: maxLength)
        }

        if let sentence = firstSentence(in: normalized) {
            return trimmedExcerpt(sentence, limit: maxLength)
        }

        return trimmedExcerpt(normalized, limit: maxLength)
    }

    func synthesizeThreadState(input: ThreadStateInput) -> ThreadStateSnapshot {
        let judgment = judgmentCandidate(from: input)
        let loopCandidates = openLoopCandidates(from: input, judgment: judgment)
        let openLoops = loopCandidates.map(\.text)
        let nextAction = nextAction(from: loopCandidates.first, judgment: judgment, input: input)
        let recoveryLines = [
            ResumeRecoveryLine(title: "Current Judgment", body: judgment.text),
            ResumeRecoveryLine(title: "Why This Holds", body: judgment.basis),
            ResumeRecoveryLine(title: "Main Gap", body: loopCandidates.first?.text ?? defaultGap(for: input.goalLayer)),
            ResumeRecoveryLine(title: "Next Move", body: nextAction ?? defaultNextMove(for: input.goalLayer))
        ]
        let resolved = resolvedSoFar(input: input, judgment: judgment)
        let restartNote = restartNote(
            coreQuestion: input.coreQuestion,
            judgment: judgment,
            mainGap: loopCandidates.first?.text,
            nextAction: nextAction
        )
        let presentation = makePresentation(
            input: input,
            currentJudgment: judgment.text,
            judgmentBasis: judgment.basis,
            openLoops: openLoops,
            nextAction: nextAction,
            resolved: resolved
        )

        return ThreadStateSnapshot(
            currentJudgment: judgment.text,
            judgmentBasis: judgment.basis,
            openLoops: openLoops,
            nextAction: nextAction,
            presentation: presentation,
            restartNote: restartNote,
            recoveryLines: recoveryLines,
            resolvedSoFar: resolved
        )
    }

    func resumeRequest(
        from snapshot: ThreadStateSnapshot,
        input: ThreadStateInput
    ) -> ResumeSynthesisRequest {
        ResumeSynthesisRequest(
            threadID: input.threadID,
            coreQuestion: input.coreQuestion,
            goalLayer: input.goalLayer,
            activeClaims: input.activeClaims.prefix(4).map(\.statement),
            currentJudgment: snapshot.currentJudgment,
            judgmentBasis: snapshot.judgmentBasis,
            openLoops: snapshot.openLoops,
            nextAction: snapshot.nextAction,
            recoveryLines: snapshot.recoveryLines,
            resolvedSoFar: snapshot.resolvedSoFar,
            recentNotes: input.recentNotes,
            evidenceCount: input.evidenceEntries.count,
            sourceCount: input.sourceEntries.count
        )
    }

    func enrichedResume(
        snapshot: ThreadStateSnapshot,
        llmResult: ResumeSynthesisResult
    ) -> ResumeSynthesisResult {
        ResumeSynthesisResult(
            currentJudgment: snapshot.currentJudgment,
            openLoops: snapshot.openLoops,
            nextAction: snapshot.nextAction,
            restartNote: normalizedRestartNote(llmResult.restartNote, fallback: snapshot.restartNote),
            recoveryLines: mergeRecoveryLines(base: snapshot.recoveryLines, candidate: llmResult.recoveryLines),
            resolvedSoFar: snapshot.resolvedSoFar,
            presentationPlan: llmResult.presentationPlan,
            debugPayload: llmResult.debugPayload
        )
    }

    func presentationUpdate(
        from result: ResumeSynthesisResult,
        input: ThreadStateInput
    ) -> PresentationApplicationResult {
        guard let plan = result.presentationPlan else {
            return PresentationApplicationResult(
                presentation: nil,
                status: .invalid("LLM reply did not include a presentation plan.")
            )
        }

        switch normalizedPresentation(from: plan, input: input) {
        case let .success(presentation):
            return PresentationApplicationResult(
                presentation: presentation,
                status: .applied
            )
        case let .failure(error):
            return PresentationApplicationResult(
                presentation: nil,
                status: .invalid(error.message)
            )
        }
    }

    private func judgmentCandidate(from input: ThreadStateInput) -> JudgmentCandidate {
        let sortedClaims = input.activeClaims.sorted { lhs, rhs in
            claimScore(lhs, in: input) > claimScore(rhs, in: input)
        }

        if let claim = sortedClaims.first {
            let supportCount = supportingRelationCount(for: claim, in: input)
            let opposingCount = opposingRelationCount(for: claim, in: input)
            let sourceCount = min(input.sourceEntries.count, 2)
            let supportSummary = supportSummary(
                supportCount: supportCount,
                opposingCount: opposingCount,
                sourceCount: sourceCount,
                anchor: input.anchor
            )
            return JudgmentCandidate(
                text: claim.statement,
                basis: supportSummary,
                supportSummary: supportSummary,
                statusLabel: claim.status == .stable ? "Stable claim" : "Working claim",
                confidence: confidence(for: claim)
            )
        }

        if let anchor = input.anchor, !anchor.stateSummary.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            let basis = "The latest checkpoint captures the thread's working read."
            return JudgmentCandidate(
                text: compactWorkingRead(anchor.stateSummary, maxLength: 88),
                basis: basis,
                supportSummary: basis,
                statusLabel: "Checkpoint",
                confidence: 0.62
            )
        }

        if let evidence = input.evidenceEntries.sorted(by: recencyDescending).first {
            let basis = input.sourceEntries.isEmpty
                ? "Recent evidence is the clearest concrete progress in the thread."
                : "Recent evidence already has at least one source-linked note behind it."
            return JudgmentCandidate(
                text: compactWorkingRead(evidence.summaryText, maxLength: 88),
                basis: basis,
                supportSummary: basis,
                statusLabel: "Evidence",
                confidence: input.sourceEntries.isEmpty ? 0.48 : 0.58
            )
        }

        if let latest = input.recentEntries.sorted(by: recencyDescending).first {
            let basis = "The latest user note is still the best available read of this thread."
            return JudgmentCandidate(
                text: compactWorkingRead(latest.summaryText, maxLength: 88),
                basis: basis,
                supportSummary: basis,
                statusLabel: "Latest note",
                confidence: 0.35
            )
        }

        let basis = "The goal is framed, but the working judgment is still thin."
        return JudgmentCandidate(
            text: input.coreQuestion,
            basis: basis,
            supportSummary: basis,
            statusLabel: "Goal frame",
            confidence: 0.2
        )
    }

    private func openLoopCandidates(
        from input: ThreadStateInput,
        judgment: JudgmentCandidate
    ) -> [OpenLoopCandidate] {
        var candidates: [OpenLoopCandidate] = []

        if input.activeClaims.isEmpty {
            candidates.append(OpenLoopCandidate(
                text: "No clear claim yet. Promote the strongest note into a claim.",
                kind: .claimMissing,
                priority: 100
            ))
        }

        if !input.activeClaims.isEmpty && input.evidenceEntries.isEmpty {
            candidates.append(OpenLoopCandidate(
                text: "The current judgment still needs evidence that could change confidence in it.",
                kind: .evidenceMissing,
                priority: 90
            ))
        }

        if !input.evidenceEntries.isEmpty && input.sourceEntries.isEmpty {
            candidates.append(OpenLoopCandidate(
                text: "The strongest evidence is still ungrounded by a source or reference.",
                kind: .sourceMissing,
                priority: 80
            ))
        }

        if hasContradiction(in: input) {
            candidates.append(OpenLoopCandidate(
                text: "The thread contains contradictory notes that still need reconciliation.",
                kind: .contradiction,
                priority: 78
            ))
        }

        if input.anchor == nil || input.entriesSinceAnchor.count >= 3 {
            candidates.append(OpenLoopCandidate(
                text: checkpointGap(input: input),
                kind: .staleCheckpoint,
                priority: 72
            ))
        }

        for question in input.questions.sorted(by: recencyDescending).prefix(2) {
            candidates.append(OpenLoopCandidate(
                text: question.summaryText,
                kind: .unansweredQuestion,
                priority: 68
            ))
        }

        for loop in input.anchor?.openLoops.prefix(2) ?? [] {
            candidates.append(OpenLoopCandidate(
                text: loop,
                kind: .anchorLoop,
                priority: 64
            ))
        }

        if candidates.isEmpty {
            candidates.append(OpenLoopCandidate(
                text: synthesisGap(for: input.goalLayer, judgment: judgment),
                kind: .synthesis,
                priority: 40
            ))
        }

        return dedupeLoopCandidates(candidates)
    }

    private func nextAction(
        from leadLoop: OpenLoopCandidate?,
        judgment: JudgmentCandidate,
        input: ThreadStateInput
    ) -> String? {
        guard let leadLoop else {
            return defaultNextMove(for: input.goalLayer)
        }

        switch leadLoop.kind {
        case .claimMissing:
            if let note = input.recentEntries.sorted(by: recencyDescending).first?.summaryText {
                return "Turn this into a clearer claim: \(compactWorkingRead(note, maxLength: 72))"
            }
            return "Turn the strongest note into a concrete claim before broadening the thread."
        case .evidenceMissing:
            return "Add one piece of evidence that strengthens or weakens: \(trimmedExcerpt(judgment.text))"
        case .sourceMissing:
            if let evidence = input.evidenceEntries.sorted(by: recencyDescending).first?.summaryText {
                return "Link a source that grounds this evidence: \(compactWorkingRead(evidence, maxLength: 72))"
            }
            return "Link a source or reference that grounds the strongest evidence."
        case .contradiction:
            return "Reconcile the contradictory notes before widening the argument."
        case .staleCheckpoint:
            let count = max(input.entriesSinceAnchor.count, 1)
            return "Write a fresh checkpoint that incorporates the latest \(count) note\(count == 1 ? "" : "s")."
        case .unansweredQuestion:
            return "Answer the sharpest open question: \(trimmedExcerpt(leadLoop.text))"
        case .anchorLoop:
            return "Work the highest-value checkpoint gap next: \(trimmedExcerpt(leadLoop.text))"
        case .synthesis:
            return defaultNextMove(for: input.goalLayer)
        }
    }

    private func resolvedSoFar(
        input: ThreadStateInput,
        judgment: JudgmentCandidate
    ) -> [ResolvedItem] {
        var items: [ResolvedItem] = []
        var seen = Set<String>()

        func appendItem(text: String, statusLabel: String, resolvedAt: Date) {
            let normalized = text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
            guard !normalized.isEmpty, !seen.contains(normalized) else { return }
            seen.insert(normalized)
            items.append(ResolvedItem(text: text, statusLabel: statusLabel, resolvedAt: resolvedAt))
        }

        appendItem(text: compactWorkingRead(judgment.text, maxLength: 88), statusLabel: judgment.statusLabel, resolvedAt: .now)

        if let anchor = input.anchor {
            appendItem(
                text: compactWorkingRead(anchor.stateSummary, maxLength: 88),
                statusLabel: "Checkpoint",
                resolvedAt: anchor.createdAt
            )
        }

        for source in input.sourceEntries.sorted(by: recencyDescending) where items.count < 3 {
            appendItem(
                text: compactWorkingRead(source.summaryText, maxLength: 88),
                statusLabel: "Source linked",
                resolvedAt: source.createdAt
            )
        }

        for evidence in input.evidenceEntries.sorted(by: recencyDescending) where items.count < 3 {
            appendItem(
                text: compactWorkingRead(evidence.summaryText, maxLength: 88),
                statusLabel: "Evidence added",
                resolvedAt: evidence.createdAt
            )
        }

        if items.isEmpty {
            items.append(ResolvedItem(
                text: "The goal is defined and the thread can resume from a stable starting point.",
                statusLabel: "Goal framed",
                resolvedAt: .now
            ))
        }
        return Array(items.prefix(3))
    }

    private func mergeRecoveryLines(
        base: [ResumeRecoveryLine],
        candidate: [ResumeRecoveryLine]
    ) -> [ResumeRecoveryLine] {
        guard base.count == candidate.count else {
            return base
        }
        return zip(base, candidate).map { original, rewritten in
            let body = rewritten.body.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !body.isEmpty else { return original }
            return ResumeRecoveryLine(title: original.title, body: body)
        }
    }

    private func normalizedRestartNote(_ restartNote: String, fallback: String) -> String {
        let trimmed = restartNote.trimmingCharacters(in: .whitespacesAndNewlines)
        return trimmed.isEmpty ? fallback : trimmed
    }

    private func claimScore(_ claim: Claim, in input: ThreadStateInput) -> Int {
        let statusWeight: Int
        switch claim.status {
        case .stable:
            statusWeight = 30
        case .working:
            statusWeight = 24
        case .candidate:
            statusWeight = 16
        case .superseded:
            statusWeight = 4
        }
        let support = supportingRelationCount(for: claim, in: input) * 5
        let opposition = opposingRelationCount(for: claim, in: input) * 3
        let anchorMention = input.anchor?.stateSummary.localizedCaseInsensitiveContains(claim.statement) == true ? 4 : 0
        return statusWeight + support - opposition + anchorMention
    }

    private func supportingRelationCount(for claim: Claim, in input: ThreadStateInput) -> Int {
        input.discourseRelations.reduce(into: 0) { count, relation in
            guard relation.targetEntryID == claim.originEntryID else { return }
            if relation.kind == .supports || relation.kind == .answers || relation.kind == .informs {
                count += 1
            }
        }
    }

    private func opposingRelationCount(for claim: Claim, in input: ThreadStateInput) -> Int {
        input.discourseRelations.reduce(into: 0) { count, relation in
            guard relation.targetEntryID == claim.originEntryID else { return }
            if relation.kind == .opposes {
                count += 1
            }
        }
    }

    private func supportSummary(
        supportCount: Int,
        opposingCount: Int,
        sourceCount: Int,
        anchor: Anchor?
    ) -> String {
        var parts: [String] = []
        if supportCount > 0 {
            parts.append("\(supportCount) note\(supportCount == 1 ? "" : "s") in the thread support it")
        }
        if sourceCount > 0 {
            parts.append("\(sourceCount) source-linked item\(sourceCount == 1 ? "" : "s") already ground the thread")
        }
        if let anchor, !anchor.stateSummary.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            parts.append("the latest checkpoint keeps the same direction")
        }
        if opposingCount > 0 {
            parts.append("but \(opposingCount) contradictory note\(opposingCount == 1 ? "" : "s") still need reconciliation")
        }
        if parts.isEmpty {
            return "It is still the clearest active claim in this thread, but the support base is thin."
        }
        return sentence(parts.joined(separator: ", "))
    }

    private func confidence(for claim: Claim) -> Double {
        switch claim.status {
        case .stable:
            return 0.82
        case .working:
            return 0.68
        case .candidate:
            return 0.5
        case .superseded:
            return 0.18
        }
    }

    private func hasContradiction(in input: ThreadStateInput) -> Bool {
        input.discourseRelations.contains { $0.kind == .opposes }
    }

    private func checkpointGap(input: ThreadStateInput) -> String {
        if input.anchor == nil {
            return "No checkpoint yet. Capture the current working state before the thread spreads further."
        }
        let count = input.entriesSinceAnchor.count
        return "The latest checkpoint is stale relative to \(count) newer top-level note\(count == 1 ? "" : "s")."
    }

    private func synthesisGap(for goalLayer: ThreadGoalLayer, judgment: JudgmentCandidate) -> String {
        switch goalLayer.currentStage {
        case .framing:
            return "Sharpen the frame so the thread can test \(trimmedExcerpt(judgment.text)) against a clearer goal."
        case .gathering:
            return "Tighten the evidence around \(trimmedExcerpt(judgment.text)) before expanding the search."
        case .synthesizing:
            return "Compress the current material into one tighter comparison or conclusion."
        case .concluding:
            return "Package the current judgment into output without widening the thread again."
        }
    }

    private func dedupeLoopCandidates(_ candidates: [OpenLoopCandidate]) -> [OpenLoopCandidate] {
        var bestByText: [String: OpenLoopCandidate] = [:]

        for candidate in candidates {
            let key = candidate.text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
            guard !key.isEmpty else { continue }
            if let existing = bestByText[key], existing.priority >= candidate.priority {
                continue
            }
            bestByText[key] = candidate
        }

        return bestByText.values.sorted { lhs, rhs in
            if lhs.priority == rhs.priority {
                return lhs.text < rhs.text
            }
            return lhs.priority > rhs.priority
        }
    }

    private func defaultGap(for goalLayer: ThreadGoalLayer) -> String {
        switch goalLayer.currentStage {
        case .framing:
            return "The thread still needs a sharper frame before it can move quickly."
        case .gathering:
            return "The thread still needs stronger material before the current read is reliable."
        case .synthesizing:
            return "The thread needs one tighter synthesis step to stabilize its direction."
        case .concluding:
            return "No major blocker is visible right now."
        }
    }

    private func defaultNextMove(for goalLayer: ThreadGoalLayer) -> String {
        switch goalLayer.goalType {
        case .build:
            return "Make the next concrete product decision that reduces ambiguity."
        case .study:
            return "Record the next observation that changes how you read the material."
        case .research:
            return "Probe one missing part of the landscape before widening the search."
        }
    }

    private func restartNote(
        coreQuestion: String,
        judgment: JudgmentCandidate,
        mainGap: String?,
        nextAction: String?
    ) -> String {
        var parts = [
            sentence("Thread focus: \(compactWorkingRead(coreQuestion, maxLength: 72))"),
            sentence("Current judgment: \(compactWorkingRead(judgment.text, maxLength: 88))"),
            sentence("Why this holds: \(judgment.basis)")
        ]
        if let mainGap, !mainGap.isEmpty {
            parts.append(sentence("Main gap: \(trimmedExcerpt(mainGap, limit: 110))"))
        }
        if let nextAction, !nextAction.isEmpty {
            parts.append(sentence("Next move: \(trimmedExcerpt(nextAction, limit: 110))"))
        }
        return parts.joined(separator: " ")
    }

    private func makePresentation(
        input: ThreadStateInput,
        currentJudgment: String,
        judgmentBasis: String,
        openLoops: [String],
        nextAction: String?,
        resolved: [ResolvedItem],
        headlineOverride: String? = nil
    ) -> ThreadPresentation {
        var blocks: [ThreadBlock] = [
            ThreadBlock(
                kind: .judgment,
                title: defaultTitle(for: .judgment),
                summary: currentJudgment,
                items: [judgmentBasis].filter { !$0.isEmpty },
                tone: defaultTone(for: .judgment),
                provenanceEntryIDs: provenanceEntryIDs(for: .judgment, input: input)
            )
        ]

        if let mainGap = openLoops.first {
            blocks.append(
                ThreadBlock(
                    kind: .gap,
                    title: defaultTitle(for: .gap),
                    summary: mainGap,
                    items: Array(openLoops.dropFirst().prefix(2)),
                    tone: defaultTone(for: .gap),
                    provenanceEntryIDs: provenanceEntryIDs(for: .gap, input: input)
                )
            )
        }

        if let nextAction, !nextAction.isEmpty {
            blocks.append(
                ThreadBlock(
                    kind: .nextMove,
                    title: defaultTitle(for: .nextMove),
                    summary: nextAction,
                    items: [],
                    tone: defaultTone(for: .nextMove),
                    provenanceEntryIDs: provenanceEntryIDs(for: .nextMove, input: input)
                )
            )
        }

        let evidenceItems = input.evidenceEntries
            .sorted(by: recencyDescending)
            .prefix(3)
            .map { compactWorkingRead($0.summaryText, maxLength: 82) }
        if !evidenceItems.isEmpty {
            blocks.append(
                ThreadBlock(
                    kind: .evidence,
                    title: defaultTitle(for: .evidence),
                    summary: nil,
                    items: evidenceItems,
                    tone: defaultTone(for: .evidence),
                    provenanceEntryIDs: provenanceEntryIDs(for: .evidence, input: input)
                )
            )
        }

        let sourceItems = input.sourceEntries
            .sorted(by: recencyDescending)
            .prefix(3)
            .map { compactWorkingRead($0.sourceDisplayTitle, maxLength: 82) }
        if !sourceItems.isEmpty {
            blocks.append(
                ThreadBlock(
                    kind: .sources,
                    title: defaultTitle(for: .sources),
                    summary: nil,
                    items: sourceItems,
                    tone: defaultTone(for: .sources),
                    provenanceEntryIDs: provenanceEntryIDs(for: .sources, input: input)
                )
            )
        }

        let resolvedItems = resolved.prefix(3).map { "[\($0.statusLabel)] \($0.text)" }
        if !resolvedItems.isEmpty {
            blocks.append(
                ThreadBlock(
                    kind: .resolved,
                    title: defaultTitle(for: .resolved),
                    summary: nil,
                    items: resolvedItems,
                    tone: defaultTone(for: .resolved),
                    provenanceEntryIDs: provenanceEntryIDs(for: .resolved, input: input)
                )
            )
        }

        let questionItems = input.questions
            .sorted(by: recencyDescending)
            .prefix(2)
            .map { compactWorkingRead($0.summaryText, maxLength: 82) }
        if !questionItems.isEmpty {
            blocks.append(
                ThreadBlock(
                    kind: .questions,
                    title: defaultTitle(for: .questions),
                    summary: nil,
                    items: questionItems,
                    tone: defaultTone(for: .questions),
                    provenanceEntryIDs: provenanceEntryIDs(for: .questions, input: input)
                )
            )
        }

        return ThreadPresentation(
            headline: headlineOverride ?? presentationHeadline(
                input: input,
                currentJudgment: currentJudgment,
                openLoops: openLoops,
                nextAction: nextAction
            ),
            blocks: blocks,
            primaryAction: nextAction
        )
    }

    private func normalizedPresentation(
        from plan: ThreadPresentationPlan,
        input: ThreadStateInput
    ) -> Result<ThreadPresentation, PresentationPlanValidationError> {
        let headline = compactWorkingRead(plan.headline, maxLength: 132)
        guard !headline.isEmpty else {
            return .failure(.emptyHeadline)
        }

        var seenKinds = Set<ThreadBlockKind>()
        let blocks = plan.blocks.prefix(5).compactMap { block -> ThreadBlock? in
            guard seenKinds.insert(block.kind).inserted else { return nil }

            let summary = block.summary.map { compactWorkingRead($0, maxLength: 132) }
            let items = block.items
                .map { compactWorkingRead($0, maxLength: 88) }
                .filter { !$0.isEmpty }
                .prefix(3)
                .map { $0 }

            guard !(summary?.isEmpty ?? true) || !items.isEmpty else { return nil }

            let title = compactWorkingRead(block.title ?? defaultTitle(for: block.kind), maxLength: 36)
            let tone = block.tone ?? defaultTone(for: block.kind)
            return ThreadBlock(
                kind: block.kind,
                title: title.isEmpty ? defaultTitle(for: block.kind) : title,
                summary: summary,
                items: items,
                tone: tone,
                provenanceEntryIDs: provenanceEntryIDs(for: block.kind, input: input)
            )
        }

        guard !blocks.isEmpty else {
            return .failure(.noValidBlocks)
        }
        let primaryAction = compactWorkingRead(plan.primaryAction ?? "", maxLength: 110)

        return .success(ThreadPresentation(
            headline: headline,
            blocks: blocks,
            primaryAction: primaryAction.isEmpty ? nil : primaryAction
        ))
    }

    private func presentationHeadline(
        input: ThreadStateInput,
        currentJudgment: String,
        openLoops: [String],
        nextAction: String?
    ) -> String {
        if input.activeClaims.isEmpty {
            return "This thread is still taking shape. Promote the strongest note into a claim before you widen it."
        }
        if input.evidenceEntries.isEmpty {
            return "You have a working read, but it still needs evidence before you can trust it."
        }
        if input.sourceEntries.isEmpty {
            return "The thread has evidence on hand, but it is still missing grounding sources."
        }
        if let openLoop = openLoops.first {
            return "The thread has a working read. Clear the main gap next: \(trimmedExcerpt(openLoop, limit: 84))"
        }
        if let nextAction {
            return "The thread is stable enough to move. Next: \(trimmedExcerpt(nextAction, limit: 84))"
        }
        return "The thread has a working read: \(trimmedExcerpt(currentJudgment, limit: 88))"
    }

    private func defaultTitle(for kind: ThreadBlockKind) -> String {
        switch kind {
        case .judgment:
            return "Working Read"
        case .basis:
            return "Why This Holds"
        case .gap:
            return "Main Gap"
        case .nextMove:
            return "Next Move"
        case .evidence:
            return "Evidence On Hand"
        case .sources:
            return "Grounding Sources"
        case .resolved:
            return "Locked In"
        case .questions:
            return "Open Questions"
        case .principles:
            return "Design Principles"
        case .risks:
            return "Risks"
        case .contrast:
            return "Key Contrast"
        case .checklist:
            return "Checklist"
        }
    }

    private func defaultTone(for kind: ThreadBlockKind) -> ThreadBlockTone {
        switch kind {
        case .judgment, .nextMove, .principles:
            return .accent
        case .gap, .risks, .contrast:
            return .warning
        case .resolved:
            return .success
        case .sources, .questions, .checklist:
            return .subdued
        case .basis, .evidence:
            return .neutral
        }
    }

    private func provenanceEntryIDs(for kind: ThreadBlockKind, input: ThreadStateInput) -> [UUID] {
        switch kind {
        case .judgment:
            return Array(input.activeClaims.prefix(2).map(\.originEntryID))
        case .basis, .evidence:
            return Array(input.evidenceEntries.prefix(3).map(\.id))
        case .sources:
            return Array(input.sourceEntries.prefix(3).map(\.id))
        case .gap, .questions:
            return Array(input.questions.prefix(2).map(\.id))
        case .nextMove, .resolved, .principles, .risks, .contrast, .checklist:
            return Array(input.recentEntries.prefix(3).map(\.id))
        }
    }

    private func trimmedExcerpt(_ text: String, limit: Int = 96) -> String {
        let trimmed = normalizedText(text)
        guard trimmed.count > limit else { return trimmed }
        let prefix = trimmed.prefix(limit).trimmingCharacters(in: .whitespacesAndNewlines)
        return "\(prefix)…"
    }

    private func sentence(_ text: String) -> String {
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard let last = trimmed.last else { return "" }
        if ".!?。！？；;".contains(last) {
            return trimmed
        }
        return "\(trimmed)."
    }

    private func normalizedText(_ text: String) -> String {
        text
            .replacingOccurrences(of: "\r\n", with: "\n")
            .replacingOccurrences(of: "\r", with: "\n")
            .components(separatedBy: .newlines)
            .map { $0.replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression) }
            .joined(separator: " ")
            .replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression)
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }

    private func firstSentence(in text: String) -> String? {
        let punctuation = CharacterSet(charactersIn: ".!?。！？")
        if let range = text.rangeOfCharacter(from: punctuation) {
            let sentence = String(text[..<range.upperBound]).trimmingCharacters(in: .whitespacesAndNewlines)
            return sentence.isEmpty ? nil : sentence
        }
        return nil
    }

    private func isUsefulHeading(_ text: String) -> Bool {
        guard text.count <= 72 else { return false }
        let letters = text.unicodeScalars.filter(CharacterSet.alphanumerics.contains).count
        return letters >= 4
    }

    private func recencyDescending(lhs: Entry, rhs: Entry) -> Bool {
        lhs.createdAt > rhs.createdAt
    }
}
