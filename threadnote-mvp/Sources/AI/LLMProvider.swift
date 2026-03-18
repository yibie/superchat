import Foundation
import SwiftAISDK
import AnthropicProvider
import OpenAIProvider
import GoogleProvider
import GroqProvider
import DeepSeekProvider
import XAIProvider
import OpenAICompatibleProvider

@MainActor
protocol AIBackendClient: AnyObject {
    var isConfigured: Bool { get }
    var activeModelID: String? { get }
    var backendLabel: String { get }
    var preferredMaxConcurrentRequests: Int { get }

    func planRoute(request: RoutePlanningRequest) async throws -> RoutePlanningResult
    func synthesizeResume(request: ResumeSynthesisRequest) async throws -> ResumeSynthesisResult
    func analyzeDiscourse(request: DiscourseAnalysisRequest) async throws -> DiscourseAnalysisResult
    func prepareDraft(request: DraftPreparationRequest) async throws -> DraftPreparationResult
}

@MainActor
final class LLMProvider: AIBackendClient {
    private enum RequestTimeout {
        static func ping(for providerKind: AIProviderKind) -> Duration {
            providerKind.isLocalProvider ? .seconds(45) : .seconds(15)
        }

        static func route(for providerKind: AIProviderKind) -> Duration {
            providerKind.isLocalProvider ? .seconds(90) : .seconds(30)
        }

        static func resume(for providerKind: AIProviderKind) -> Duration {
            providerKind.isLocalProvider ? .seconds(120) : .seconds(45)
        }

        static func discourse(for providerKind: AIProviderKind) -> Duration {
            providerKind.isLocalProvider ? .seconds(90) : .seconds(30)
        }

        static func draft(for providerKind: AIProviderKind) -> Duration {
            providerKind.isLocalProvider ? .seconds(120) : .seconds(45)
        }
    }

    private enum TimedResult<T: Sendable>: Sendable {
        case value(T)
        case timedOut
    }

    private var model: LanguageModel?
    private var providerKind: AIProviderKind = .anthropic
    private var configuredModelID: String?

    private func log(_ message: String) {
        print("[LLMProvider] \(message)")
    }

    var isConfigured: Bool { model != nil }
    var activeModelID: String? { configuredModelID }
    var preferredMaxConcurrentRequests: Int {
        providerKind.isLocalProvider ? 1 : 2
    }
    var backendLabel: String {
        let modelLabel = configuredModelID ?? "default"
        return "\(providerKind.title) · \(modelLabel)"
    }

    nonisolated static func relationKind(from rawValue: String) -> DiscourseRelationKind {
        let normalized = rawValue.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return DiscourseRelationKind(rawValue: normalized) ?? .informs
    }

    nonisolated static func blockKind(from rawValue: String) -> ThreadBlockKind? {
        let normalized = rawValue.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return ThreadBlockKind.allCases.first { $0.rawValue.lowercased() == normalized }
    }

    nonisolated static func blockTone(from rawValue: String?) -> ThreadBlockTone? {
        guard let rawValue else { return nil }
        let normalized = rawValue.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        return ThreadBlockTone(rawValue: normalized)
    }

    func configure() throws {
        configuredModelID = nil
        guard let selectedRaw = KeychainHelper.read(key: "selectedProvider"),
              let kind = AIProviderKind(rawValue: selectedRaw) else {
            model = nil
            log("No provider selected.")
            return
        }
        let supportedProviders = Set(AIProviderKind.supportedBackends)
        guard supportedProviders.contains(kind) else {
            model = nil
            log("Unsupported provider selected: \(kind.rawValue)")
            return
        }

        let prefix = kind.rawValue
        let localProviders: Set<AIProviderKind> = [.ollama, .lmStudio, .openAICompat]
        let apiKey = KeychainHelper.read(key: "\(prefix).apiKey") ?? ""
        if !localProviders.contains(kind) && apiKey.isEmpty {
            model = nil
            log("Provider \(kind.rawValue) is missing an API key.")
            return
        }

        let modelName = KeychainHelper.read(key: "\(prefix).model") ?? ""
        providerKind = kind
        configuredModelID = modelName.isEmpty ? nil : modelName

        switch kind {
        case .anthropic:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createAnthropicProvider(
                settings: AnthropicProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        case .openAI:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createOpenAIProvider(
                settings: OpenAIProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        case .google:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createGoogleGenerativeAI(
                settings: GoogleProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider.languageModel(modelId: name))
        case .groq:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createGroq(
                settings: GroqProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider.languageModel(modelId: name))
        case .deepSeek:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createDeepSeek(
                settings: DeepSeekProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        case .xai:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let provider = createXai(
                settings: XAIProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        case .ollama:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let baseURL = KeychainHelper.read(key: "\(prefix).baseURL") ?? kind.defaultBaseURL
            let ollamaModel = OllamaChatModel(
                modelId: name,
                baseURL: baseURL,
                requestTimeout: 150,
                keepAlive: "30m"
            )
            model = .v3(ollamaModel)
        case .lmStudio:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let baseURL = KeychainHelper.read(key: "\(prefix).baseURL") ?? kind.defaultBaseURL
            let provider = createOpenAICompatibleProvider(
                settings: OpenAICompatibleProviderSettings(
                    baseURL: baseURL,
                    name: "lmstudio",
                    supportsStructuredOutputs: false
                )
            )
            model = .v3(try provider.languageModel(modelId: name))
        case .openAICompat:
            let name = modelName.isEmpty ? kind.defaultModel : modelName
            configuredModelID = name
            let baseURL = KeychainHelper.read(key: "\(prefix).baseURL") ?? kind.defaultBaseURL
            let provider = createOpenAICompatibleProvider(
                settings: OpenAICompatibleProviderSettings(
                    baseURL: baseURL,
                    name: "openai-compat",
                    apiKey: apiKey.isEmpty ? nil : apiKey,
                    supportsStructuredOutputs: false
                )
            )
            model = .v3(try provider.languageModel(modelId: name))
        }

        log("Configured backend \(backendLabel)")
    }

    func ping() async throws -> String {
        guard let model else { throw LLMError.notConfigured }
        log("Pinging \(backendLabel)")
        let result = try await runWithTimeout(
            RequestTimeout.ping(for: providerKind),
            operation: "Ping"
        ) {
            try await generateText(
                model: model,
                prompt: "Respond with the single word: pong",
                experimentalOutput: nil as Output.Specification<Never, JSONValue>?
            )
        }
        log("Ping succeeded for \(backendLabel) with response: \(result.text.trimmingCharacters(in: .whitespacesAndNewlines))")
        return result.text
    }

    // MARK: - Route Planning

    struct RoutePlanningResponse: Codable, Sendable {
        let decision: String
        let selectedThreadID: String?
        let decisionReason: String
        let suggestions: [RoutePlanningSuggestionResponse]
    }

    struct RoutePlanningSuggestionResponse: Codable, Sendable {
        let threadID: String
        let reason: String
    }

    func planRoute(request: RoutePlanningRequest) async throws -> RoutePlanningResult {
        guard let model else { throw LLMError.notConfigured }
        log("Planning route with \(backendLabel) for entry \(request.entryID.uuidString) across \(request.candidates.count) candidates.")

        let routeSystemPrompt = "You are the routing planner for Threadnote. Your job is to decide which thread a new note belongs to. Be conservative: if the note does not clearly belong to one thread, keep it in inbox. Use deterministic candidate scores only as supporting evidence. Return only thread IDs from the provided candidate list."
        let objectsText = request.detectedObjects.isEmpty ? "None" : request.detectedObjects.joined(separator: ", ")
        let claimsText = request.candidateClaims.isEmpty ? "None" : request.candidateClaims.joined(separator: "\n")
        let queriesText = request.routingQueries.isEmpty ? "None" : request.routingQueries.joined(separator: "\n")
        let candidateText = request.candidates.enumerated().map { index, candidate in
            let objectText = candidate.coreObjects.prefix(3).joined(separator: ", ")
            let claimText = candidate.activeClaims.prefix(2).map { compact($0, maxLength: 90) }.joined(separator: " | ")
            let loopText = candidate.openLoops.prefix(2).map { compact($0, maxLength: 80) }.joined(separator: " | ")
            return """
            \(index + 1). Thread ID: \(candidate.threadID.uuidString)
               Title: \(compact(candidate.threadTitle, maxLength: 80))
               Goal: \(compact(candidate.goalStatement, maxLength: 120))
               Core objects: \(objectText.isEmpty ? "None" : objectText)
               Active claims: \(claimText.isEmpty ? "None" : claimText)
               Latest anchor: \(compact(candidate.latestAnchorSummary ?? "None", maxLength: 120))
               Open loops: \(loopText.isEmpty ? "None" : loopText)
               Deterministic support: total \(candidate.totalScore) = semantic \(candidate.semanticScore) + retrieval \(candidate.retrievalScore)
               Deterministic reason: \(compact(candidate.reason, maxLength: 140))
            """
        }.joined(separator: "\n")

        let userPrompt = """
        New note:
        \(request.normalizedText)

        Detected item type: \(request.detectedItemType)
        Detected objects: \(objectsText)
        Candidate claims:
        \(claimsText)
        Routing queries:
        \(queriesText)

        Candidate threads:
        \(candidateText)

        Decide whether this note should be routed into one of the candidate threads, or should stay in inbox.
        Rules:
        - You may only select a thread ID from the candidate list.
        - Use the deterministic scores as supporting evidence, not as the final decision.
        - If the note is ambiguous, underspecified, or does not clearly belong to one thread, keep it in inbox.
        - Return up to 3 suggestions ranked best-first.

        You MUST respond with exactly this JSON structure (no extra fields):
        {
          "decision": "route" or "inbox",
          "selectedThreadID": "UUID string from candidate list, or null if inbox",
          "decisionReason": "one sentence explaining why",
          "suggestions": [{"threadID": "UUID string", "reason": "one sentence"}]
        }
        """
        let promptStats = makePromptStats(
            operation: "route",
            systemPrompt: routeSystemPrompt,
            userPrompt: userPrompt,
            extra: [
                "candidates=\(request.candidates.count)",
                "candidate_claims=\(request.candidateClaims.count)",
                "routing_queries=\(request.routingQueries.count)",
                "objects=\(request.detectedObjects.count)"
            ]
        )
        log("Route prompt stats: \(promptStats)")

        let result = try await runWithTimeout(
            RequestTimeout.route(for: providerKind),
            operation: "Route planning"
        ) {
            try await generateObject(
                model: model,
                schema: RoutePlanningResponse.self,
                system: routeSystemPrompt,
                prompt: userPrompt,
                schemaName: "route_planning"
            )
        }

        let now = Date()
        let validThreadIDs = Set(request.candidates.map(\.threadID))
        let suggestions = result.object.suggestions.compactMap { suggestion -> RoutePlanningSuggestion? in
            guard let threadID = UUID(uuidString: suggestion.threadID),
                  validThreadIDs.contains(threadID) else {
                return nil
            }
            return RoutePlanningSuggestion(threadID: threadID, reason: suggestion.reason)
        }
        let selectedThreadID = result.object.selectedThreadID.flatMap(UUID.init(uuidString:))
        let debugPayload = LLMRouteDebugPayload(
            backendLabel: backendLabel,
            configuredModelID: configuredModelID,
            responseModelID: result.response.modelId,
            responseID: result.response.id,
            finishReason: result.finishReason.rawValue,
            warnings: (result.warnings ?? []).map { String(describing: $0) },
            promptStats: promptStats,
            parsedResponse: Self.prettyJSONString(from: result.object),
            rawResponseBody: Self.prettyJSONString(from: result.response.body),
            updatedAt: now
        )

        log(
            """
            Route response for entry \(request.entryID.uuidString): decision=\(result.object.decision), \
            selectedThreadID=\(result.object.selectedThreadID ?? "nil"), finishReason=\(result.finishReason.rawValue)
            """
        )

        return RoutePlanningResult(
            shouldRoute: result.object.decision.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() == "route",
            selectedThreadID: selectedThreadID,
            decisionReason: result.object.decisionReason,
            suggestions: suggestions,
            debugPayload: debugPayload
        )
    }

    // MARK: - Resume Synthesis

    struct ResumeSynthesisResponse: Codable, Sendable {
        let currentJudgment: String
        let openLoops: [String]
        let nextAction: String?
        let restartNote: String
        let recoveryLines: [RecoveryLineResponse]
        let resolvedSoFar: [ResolvedItemResponse]
        let presentation: PresentationResponse?
    }

    struct RecoveryLineResponse: Codable, Sendable {
        let title: String
        let body: String
    }

    struct ResolvedItemResponse: Codable, Sendable {
        let text: String
        let statusLabel: String
    }

    struct PresentationResponse: Codable, Sendable {
        let headline: String
        let primaryAction: String?
        let blocks: [PresentationBlockResponse]
    }

    struct PresentationBlockResponse: Codable, Sendable {
        let kind: String
        let title: String?
        let summary: String?
        let items: [String]
        let tone: String?
    }

    func synthesizeResume(request: ResumeSynthesisRequest) async throws -> ResumeSynthesisResult {
        guard let model else { throw LLMError.notConfigured }
        let resumeSystemPrompt = "You are a thinking assistant for a research tool called Threadnote. Preserve the deterministic thread state, but improve how it is presented to help the user resume work. Return a short restart note plus a constrained UI plan made of supported block kinds. Reorganize the cockpit around what matters most; do not mirror raw note types mechanically. Be concrete and specific — reference actual content from the notes, not generic advice. Write in second person (\"you\")."

        let claimsText = request.activeClaims.isEmpty
            ? "No active claims yet."
            : request.activeClaims.enumerated().map { "  \($0.offset + 1). \($0.element)" }.joined(separator: "\n")

        let loopsText = request.openLoops.isEmpty
            ? "No open loops."
            : request.openLoops.enumerated().map { "  \($0.offset + 1). \($0.element)" }.joined(separator: "\n")

        let recoveryText = request.recoveryLines.map { line in
            "  - \(line.title): \(line.body)"
        }.joined(separator: "\n")

        let resolvedText = request.resolvedSoFar.isEmpty
            ? "No stable resolved items yet."
            : request.resolvedSoFar.map { "  - [\($0.statusLabel)] \($0.text)" }.joined(separator: "\n")

        let notesText = request.recentNotes.map { note in
            "  - [\(note.kind?.rawValue ?? "note")] \(note.text)"
        }.joined(separator: "\n")

        let supportedBlocks = [
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
        ].joined(separator: ", ")

        let userPrompt = """
        Thread goal: \(request.coreQuestion)
        Goal type: \(request.goalLayer.goalType.rawValue)
        Stage: \(request.goalLayer.currentStage.rawValue)

        Active claims:
        \(claimsText)

        Deterministic state snapshot:
        Current judgment: \(request.currentJudgment)
        Judgment basis: \(request.judgmentBasis)
        Next action: \(request.nextAction ?? "None yet")

        Open loops:
        \(loopsText)

        Structured restart lines:
        \(recoveryText)

        Resolved so far:
        \(resolvedText)

        Recent notes:
        \(notesText)

        Evidence count: \(request.evidenceCount), Source count: \(request.sourceCount)

        Plan a better thread cockpit for resuming work.
        You may reorganize the UI into up to 5 blocks chosen only from these supported kinds:
        \(supportedBlocks)

        Preserve the same core state as the deterministic snapshot; improve structure, prioritization, and specificity.
        Do not simply mirror every note kind into one block. Group, compress, or omit low-value material.
        The restartNote should be a short supporting summary, not the whole UI plan.

        You MUST respond with exactly this JSON structure (no extra fields):
        {
          "currentJudgment": "string",
          "openLoops": ["string"],
          "nextAction": "string or null",
          "restartNote": "string",
          "recoveryLines": [{"title": "string", "body": "string"}],
          "resolvedSoFar": [{"text": "string", "statusLabel": "string"}],
          "presentation": {
            "headline": "string",
            "primaryAction": "string or null",
            "blocks": [{"kind": "one of: judgment|basis|gap|nextMove|evidence|sources|resolved|questions|principles|risks|contrast|checklist", "title": "string or null", "summary": "string or null", "items": ["string"], "tone": "accent|warning|success|subdued|neutral or null"}]
          }
        }
        """
        let promptStats = makePromptStats(
            operation: "resume",
            systemPrompt: resumeSystemPrompt,
            userPrompt: userPrompt,
            extra: [
                "recent_notes=\(request.recentNotes.count)",
                "recent_note_chars=\(request.recentNotes.reduce(0) { $0 + $1.text.count })",
                "active_claims=\(request.activeClaims.count)",
                "open_loops=\(request.openLoops.count)",
                "recovery_lines=\(request.recoveryLines.count)"
            ]
        )
        log("Resume prompt stats: \(promptStats)")

        let result = try await runWithTimeout(
            RequestTimeout.resume(for: providerKind),
            operation: "Resume synthesis"
        ) {
            try await generateObject(
                model: model,
                schema: ResumeSynthesisResponse.self,
                system: resumeSystemPrompt,
                prompt: userPrompt,
                schemaName: "resume_synthesis"
            )
        }

        let obj = result.object
        let now = Date()
        let presentationPlan = obj.presentation.flatMap { presentation -> ThreadPresentationPlan? in
            let blocks = presentation.blocks.compactMap { block -> ThreadBlockPlan? in
                guard let kind = Self.blockKind(from: block.kind) else { return nil }
                return ThreadBlockPlan(
                    kind: kind,
                    title: block.title,
                    summary: block.summary,
                    items: block.items,
                    tone: Self.blockTone(from: block.tone)
                )
            }
            guard !blocks.isEmpty else { return nil }
            return ThreadPresentationPlan(
                headline: presentation.headline,
                blocks: blocks,
                primaryAction: presentation.primaryAction
            )
        }
        let debugPayload = LLMResumeDebugPayload(
            backendLabel: backendLabel,
            configuredModelID: configuredModelID,
            responseModelID: result.response.modelId,
            responseID: result.response.id,
            finishReason: result.finishReason.rawValue,
            warnings: (result.warnings ?? []).map { String(describing: $0) },
            promptStats: promptStats,
            parsedResponse: Self.prettyJSONString(from: obj),
            rawResponseBody: Self.prettyJSONString(from: result.response.body),
            updatedAt: now
        )
        return ResumeSynthesisResult(
            currentJudgment: obj.currentJudgment,
            openLoops: obj.openLoops,
            nextAction: obj.nextAction,
            restartNote: obj.restartNote,
            recoveryLines: obj.recoveryLines.map {
                ResumeRecoveryLine(title: $0.title, body: $0.body)
            },
            resolvedSoFar: obj.resolvedSoFar.map {
                ResolvedItem(text: $0.text, statusLabel: $0.statusLabel, resolvedAt: now)
            },
            presentationPlan: presentationPlan,
            debugPayload: debugPayload
        )
    }

    // MARK: - Discourse Analysis

    struct DiscourseRelationResponse: Codable, Sendable {
        let sourceID: String
        let targetID: String
        let kind: String
    }

    struct DiscourseAnalysisResponse: Codable, Sendable {
        let relations: [DiscourseRelationResponse]
    }

    func analyzeDiscourse(request: DiscourseAnalysisRequest) async throws -> DiscourseAnalysisResult {
        guard let model else { throw LLMError.notConfigured }

        let entriesText = request.snippets.map { snippet in
            "- ID: \(snippet.id.uuidString), Kind: \(snippet.kind?.rawValue ?? "note"), Text: \(snippet.text)"
        }.joined(separator: "\n")

        let userPrompt = """
        Entries in this thread:
        \(entriesText)

        Identify discourse relationships between these entries. Valid relation kinds: supports, opposes, informs, answers.
        Only include relationships where there is a clear semantic connection.

        You MUST respond with exactly this JSON structure (no extra fields):
        {
          "relations": [{"sourceID": "UUID string", "targetID": "UUID string", "kind": "supports|opposes|informs|answers"}]
        }
        """

        let result = try await runWithTimeout(
            RequestTimeout.discourse(for: providerKind),
            operation: "Discourse analysis"
        ) {
            try await generateObject(
                model: model,
                schema: DiscourseAnalysisResponse.self,
                system: "You are analyzing relationships between notes in a thinking thread. Identify how entries relate to each other: supports (evidence backing a claim), opposes (contradicting evidence), informs (context or background), answers (resolving a question). Be conservative — only flag strong, clear relationships.",
                prompt: userPrompt,
                schemaName: "discourse_analysis"
            )
        }

        let pairs = result.object.relations.compactMap { rel -> AIRelationPair? in
            guard let source = UUID(uuidString: rel.sourceID),
                  let target = UUID(uuidString: rel.targetID) else { return nil }
            let kind = Self.relationKind(from: rel.kind)
            return AIRelationPair(sourceEntryID: source, targetEntryID: target, kind: kind)
        }
        return DiscourseAnalysisResult(
            relationPairs: pairs,
            rationale: "LLM-analyzed discourse relationships"
        )
    }

    // MARK: - Draft Preparation

    struct DraftPreparationResponse: Codable, Sendable {
        let title: String
        let openLoops: [String]
        let recommendedNextSteps: [String]
    }

    func prepareDraft(request: DraftPreparationRequest) async throws -> DraftPreparationResult {
        guard let model else { throw LLMError.notConfigured }

        let claimsText = request.activeClaims.isEmpty
            ? "No active claims."
            : request.activeClaims.enumerated().map { "  \($0.offset + 1). \($0.element)" }.joined(separator: "\n")

        let evidenceText = request.keyEvidence.map { "  - [\($0.kind?.rawValue ?? "note")] \($0.text)" }.joined(separator: "\n")

        let loopsText = request.openLoops.isEmpty
            ? "No open loops."
            : request.openLoops.map { "  - \($0)" }.joined(separator: "\n")

        let notesText = request.recentNotes.map { "  - [\($0.kind?.rawValue ?? "note")] \($0.text)" }.joined(separator: "\n")

        let userPrompt = """
        Thread goal: \(request.coreQuestion)
        Preparing: \(request.type.rawValue)

        Active claims:
        \(claimsText)

        Key evidence:
        \(evidenceText)

        Open loops:
        \(loopsText)

        Recent notes:
        \(notesText)

        Generate a \(request.type.rawValue) draft outline with a title, the remaining open loops, and recommended next steps.

        You MUST respond with exactly this JSON structure (no extra fields):
        {
          "title": "string",
          "openLoops": ["string"],
          "recommendedNextSteps": ["string"]
        }
        """

        let result = try await runWithTimeout(
            RequestTimeout.draft(for: providerKind),
            operation: "Draft preparation"
        ) {
            try await generateObject(
                model: model,
                schema: DraftPreparationResponse.self,
                system: "You are a writing assistant for a research tool called Threadnote. Given thread content, produce a concise draft outline. Be specific — reference actual content. Keep next steps actionable and concrete.",
                prompt: userPrompt,
                schemaName: "draft_preparation"
            )
        }

        return DraftPreparationResult(
            title: result.object.title,
            openLoops: result.object.openLoops,
            recommendedNextSteps: result.object.recommendedNextSteps
        )
    }

    private nonisolated static func prettyJSONString<T: Encodable>(from value: T?) -> String? {
        guard let value else { return nil }
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
        guard let data = try? encoder.encode(value),
              let string = String(data: data, encoding: .utf8) else {
            return nil
        }
        return string
    }

    private func makePromptStats(
        operation: String,
        systemPrompt: String,
        userPrompt: String,
        extra: [String]
    ) -> String {
        let total = systemPrompt.count + userPrompt.count
        return ([
            "op=\(operation)",
            "system_chars=\(systemPrompt.count)",
            "user_chars=\(userPrompt.count)",
            "total_chars=\(total)"
        ] + extra).joined(separator: ", ")
    }

    private func compact(_ text: String, maxLength: Int) -> String {
        let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard trimmed.count > maxLength else { return trimmed }
        let prefix = trimmed.prefix(max(0, maxLength - 1))
        return "\(prefix)…"
    }

    private func runWithTimeout<T: Sendable>(
        _ timeout: Duration,
        operation: String,
        execute: @escaping @MainActor () async throws -> T
    ) async throws -> T {
        let operationTask = Task { @MainActor in
            try await execute()
        }
        let timeoutTask = Task {
            try await Task.sleep(for: timeout)
        }

        defer {
            operationTask.cancel()
            timeoutTask.cancel()
        }

        return try await withThrowingTaskGroup(of: TimedResult<T>.self) { group in
            group.addTask { .value(try await operationTask.value) }
            group.addTask {
                try await timeoutTask.value
                return .timedOut
            }
            let result = try await group.next()!
            group.cancelAll()
            switch result {
            case let .value(value):
                return value
            case .timedOut:
                throw LLMError.timeout(operation: operation, duration: timeout)
            }
        }
    }
}

enum LLMError: Error, LocalizedError {
    case notConfigured
    case timeout(operation: String, duration: Duration)

    var errorDescription: String? {
        switch self {
        case .notConfigured:
            return "No AI provider configured. Open Settings to add an API key."
        case let .timeout(operation, duration):
            let seconds = Int(duration.components.seconds)
            return "\(operation) timed out after \(seconds)s."
        }
    }
}
