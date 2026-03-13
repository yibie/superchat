import Foundation
import SwiftAISDK
import AnthropicProvider
import OpenAIProvider

@MainActor
final class LLMProvider {
    private var model: LanguageModel?
    private var providerKind: AIProviderKind = .heuristics

    var isConfigured: Bool { model != nil }

    func configure() throws {
        let selectedRaw = KeychainHelper.read(key: "selectedProvider") ?? AIProviderKind.anthropic.rawValue
        guard let kind = AIProviderKind(rawValue: selectedRaw),
              kind == .anthropic || kind == .openAI else {
            model = nil
            return
        }

        let prefix = kind.rawValue
        guard let apiKey = KeychainHelper.read(key: "\(prefix).apiKey"), !apiKey.isEmpty else {
            model = nil
            return
        }

        let modelName = KeychainHelper.read(key: "\(prefix).model") ?? ""
        providerKind = kind

        switch kind {
        case .anthropic:
            let name = modelName.isEmpty ? "claude-sonnet-4-5-20250514" : modelName
            let provider = createAnthropicProvider(
                settings: AnthropicProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        case .openAI:
            let name = modelName.isEmpty ? "gpt-4.1-mini" : modelName
            let provider = createOpenAIProvider(
                settings: OpenAIProviderSettings(apiKey: apiKey)
            )
            model = .v3(try provider(name))
        default:
            model = nil
        }
    }

    // MARK: - Ping

    func ping() async throws -> String {
        guard let model else { throw LLMError.notConfigured }
        let result = try await generateText(
            model: model,
            prompt: "Respond with the single word: pong",
            experimentalOutput: nil as Output.Specification<Never, JSONValue>?
        )
        return result.text
    }

    // MARK: - Thread Suggestions

    struct ThreadSuggestionItem: Codable, Sendable {
        let threadID: String
        let score: Int
        let rationale: String
    }

    struct ThreadSuggestionResponse: Codable, Sendable {
        let suggestions: [ThreadSuggestionItem]
    }

    func suggestThreads(request: ThreadSuggestionRequest) async throws -> ThreadSuggestionResult {
        guard let model else { throw LLMError.notConfigured }

        let candidateList = request.candidateThreads
            .filter { !request.excludedThreadIDs.contains($0.id) }
            .map { "- ID: \($0.id.uuidString), Title: \($0.title), Goal: \($0.prompt)" }
            .joined(separator: "\n")

        let userPrompt = """
        Note text: "\(request.noteSummary)"

        Candidate threads:
        \(candidateList)

        Return the top threads this note belongs to, scored 1-10. Only include threads with score >= 3.
        """

        let result = try await generateObject(
            model: model,
            schema: ThreadSuggestionResponse.self,
            system: "You are a note routing assistant for a research tool called Threadnote. Given a note and candidate threads, suggest which threads the note belongs to. Be selective — only suggest threads with genuine semantic relevance, not just keyword overlap.",
            prompt: userPrompt,
            schemaName: "thread_suggestions"
        )

        let suggestions = result.object.suggestions.compactMap { item -> AIThreadSuggestion? in
            guard let uuid = UUID(uuidString: item.threadID) else { return nil }
            return AIThreadSuggestion(threadID: uuid, score: item.score, rationale: item.rationale)
        }
        return ThreadSuggestionResult(suggestions: suggestions)
    }

    // MARK: - Resume Synthesis

    struct ResumeSynthesisResponse: Codable, Sendable {
        let currentJudgment: String
        let openLoops: [String]
        let nextAction: String?
        let restartNote: String
        let recoveryLines: [RecoveryLineResponse]
        let resolvedSoFar: [ResolvedItemResponse]
    }

    struct RecoveryLineResponse: Codable, Sendable {
        let title: String
        let body: String
    }

    struct ResolvedItemResponse: Codable, Sendable {
        let text: String
        let statusLabel: String
    }

    func synthesizeResume(request: ResumeSynthesisRequest) async throws -> ResumeSynthesisResult {
        guard let model else { throw LLMError.notConfigured }

        let claimsText = request.activeClaims.isEmpty
            ? "No active claims yet."
            : request.activeClaims.enumerated().map { "  \($0.offset + 1). \($0.element)" }.joined(separator: "\n")

        let loopsText = request.openLoops.isEmpty
            ? "No open loops."
            : request.openLoops.enumerated().map { "  \($0.offset + 1). \($0.element)" }.joined(separator: "\n")

        let notesText = request.recentNotes.map { note in
            "  - [\(note.kind?.rawValue ?? "note")] \(note.text)"
        }.joined(separator: "\n")

        let userPrompt = """
        Thread goal: \(request.coreQuestion)
        Goal type: \(request.goalLayer.goalType.rawValue)
        Stage: \(request.goalLayer.currentStage.rawValue)

        Active claims:
        \(claimsText)

        Open loops:
        \(loopsText)

        Recent notes:
        \(notesText)

        Evidence count: \(request.evidenceCount), Source count: \(request.sourceCount)

        Synthesize where this thread stands so the user can resume work effectively.
        """

        let result = try await generateObject(
            model: model,
            schema: ResumeSynthesisResponse.self,
            system: "You are a thinking assistant for a research tool called Threadnote. Summarize where a research thread stands to help the user resume work. Be concrete and specific — reference actual content from the notes, not generic advice. Write in second person (\"you\").",
            prompt: userPrompt,
            schemaName: "resume_synthesis"
        )

        let obj = result.object
        let now = Date()
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
            }
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
        """

        let result = try await generateObject(
            model: model,
            schema: DiscourseAnalysisResponse.self,
            system: "You are analyzing relationships between notes in a thinking thread. Identify how entries relate to each other: supports (evidence backing a claim), opposes (contradicting evidence), informs (context or background), answers (resolving a question). Be conservative — only flag strong, clear relationships.",
            prompt: userPrompt,
            schemaName: "discourse_analysis"
        )

        let pairs = result.object.relations.compactMap { rel -> AIRelationPair? in
            guard let source = UUID(uuidString: rel.sourceID),
                  let target = UUID(uuidString: rel.targetID) else { return nil }
            return AIRelationPair(sourceEntryID: source, targetEntryID: target)
        }
        return DiscourseAnalysisResult(
            relationPairs: pairs,
            rationale: "LLM-analyzed discourse relationships"
        )
    }
}

enum LLMError: Error, LocalizedError {
    case notConfigured

    var errorDescription: String? {
        switch self {
        case .notConfigured:
            "No AI provider configured. Open Settings to add an API key."
        }
    }
}
