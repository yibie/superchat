import Foundation

enum RoutingDecision: Equatable {
    case route(threadID: UUID, score: Int, reason: String)
    case noMatch(reason: String)
}

struct RouteSupportSnapshot {
    var normalizedText: String
    var detectedItemType: String
    var detectedObjects: [String]
    var candidateClaims: [String]
    var routingQueries: [String]
    var rankedCandidates: [RouteCandidateDebug]
    var topScore: Int?
    var secondScore: Int?
    var autoRouteThreshold: Int
    var autoRouteGapThreshold: Int
}

final class ThreadSignatureEngine {
    private let threadsProvider: () -> [ThreadRecord]
    private let entriesProvider: (UUID) -> [Entry]
    private let claimsProvider: (UUID) -> [Claim]
    private let latestAnchorProvider: (UUID) -> Anchor?
    private let captureInterpreter: CaptureInterpreter

    init(
        threadsProvider: @escaping () -> [ThreadRecord],
        entriesProvider: @escaping (UUID) -> [Entry],
        claimsProvider: @escaping (UUID) -> [Claim],
        latestAnchorProvider: @escaping (UUID) -> Anchor?,
        captureInterpreter: CaptureInterpreter
    ) {
        self.threadsProvider = threadsProvider
        self.entriesProvider = entriesProvider
        self.claimsProvider = claimsProvider
        self.latestAnchorProvider = latestAnchorProvider
        self.captureInterpreter = captureInterpreter
    }

    func signatures(excluding excludedThreadIDs: Set<UUID> = []) -> [ThreadSignature] {
        threadsProvider()
            .filter { !excludedThreadIDs.contains($0.id) }
            .map(signature(for:))
            .sorted { $0.lastActiveAt > $1.lastActiveAt }
    }

    func signature(for threadID: UUID) -> ThreadSignature? {
        guard let thread = threadsProvider().first(where: { $0.id == threadID }) else {
            return nil
        }
        return signature(for: thread)
    }

    private func signature(for thread: ThreadRecord) -> ThreadSignature {
        let claims = claimsProvider(thread.id)
            .filter { $0.status != .superseded }
            .sorted { $0.updatedAt > $1.updatedAt }
        let anchor = latestAnchorProvider(thread.id)
        let recentEntries = entriesProvider(thread.id)
            .filter { $0.parentEntryID == nil }
            .sorted { $0.createdAt > $1.createdAt }

        let derivedObjects = captureInterpreter.extractObjects(
            from: [thread.title, thread.goalLayer.goalStatement]
                + claims.prefix(4).map(\.statement)
                + [anchor?.stateSummary].compactMap { $0 }
                + recentEntries.prefix(4).map(\.summaryText)
        )
        let entryObjects = recentEntries.flatMap { entry in
            entry.objectMentions.isEmpty ? captureInterpreter.interpret(entry: entry).detectedObjects : entry.objectMentions
        }

        return ThreadSignature(
            thread: thread,
            goalStatement: thread.goalLayer.goalStatement,
            coreObjects: dedupeObjects(derivedObjects + entryObjects),
            activeClaims: Array(claims.prefix(4).map(\.statement)),
            latestAnchorSummary: anchor?.stateSummary,
            openLoops: anchor?.openLoops ?? [],
            lastActiveAt: thread.lastActiveAt
        )
    }

    private func dedupeObjects(_ objects: [ObjectMention]) -> [ObjectMention] {
        var counts: [String: Int] = [:]
        var mentions: [String: ObjectMention] = [:]

        for object in objects {
            let key = object.name.lowercased()
            counts[key, default: 0] += 1
            if mentions[key] == nil {
                mentions[key] = object
            }
        }

        return mentions
            .values
            .sorted {
                let lhs = counts[$0.name.lowercased(), default: 0]
                let rhs = counts[$1.name.lowercased(), default: 0]
                if lhs == rhs {
                    return $0.name < $1.name
                }
                return lhs > rhs
            }
            .prefix(5)
            .map { $0 }
    }
}

final class ThreadRoutingEngine {
    private struct ScoreBreakdown {
        var score: Int
        var reason: String?
    }

    private struct RetrievalSupport {
        var score: Int
        var reason: String?
    }

    private struct RankedThread {
        var thread: ThreadRecord
        var semantic: ScoreBreakdown
        var retrieval: RetrievalSupport

        var totalScore: Int { semantic.score + retrieval.score }
        var reason: String { semantic.reason ?? retrieval.reason ?? "No matching content" }
    }

    private struct RouteEvaluation {
        var interpretation: CaptureInterpretation
        var ranked: [RankedThread]
        var decision: RoutingDecision
    }

    private let retrievalEngine: RetrievalEngine
    private let threadsProvider: () -> [ThreadRecord]
    private let captureInterpreter: CaptureInterpreter
    private let threadSignatureEngine: ThreadSignatureEngine

    private let autoRouteThreshold = 20
    private let autoRouteGapThreshold = 8

    init(
        retrievalEngine: RetrievalEngine,
        threadsProvider: @escaping () -> [ThreadRecord],
        entriesProvider: @escaping (UUID) -> [Entry] = { _ in [] },
        claimsProvider: @escaping (UUID) -> [Claim] = { _ in [] },
        latestAnchorProvider: @escaping (UUID) -> Anchor? = { _ in nil },
        captureInterpreter: CaptureInterpreter = CaptureInterpreter()
    ) {
        self.retrievalEngine = retrievalEngine
        self.threadsProvider = threadsProvider
        self.captureInterpreter = captureInterpreter
        self.threadSignatureEngine = ThreadSignatureEngine(
            threadsProvider: threadsProvider,
            entriesProvider: entriesProvider,
            claimsProvider: claimsProvider,
            latestAnchorProvider: latestAnchorProvider,
            captureInterpreter: captureInterpreter
        )
    }

    func suggest(
        noteText: String,
        excluding excludedThreadIDs: Set<UUID> = [],
        limit: Int = 3
    ) -> [ThreadSuggestion] {
        suggest(
            interpretation: captureInterpreter.interpret(text: noteText),
            excluding: excludedThreadIDs,
            limit: limit
        )
    }

    func suggest(
        interpretation: CaptureInterpretation,
        excluding excludedThreadIDs: Set<UUID> = [],
        limit: Int = 3
    ) -> [ThreadSuggestion] {
        let signatures = threadSignatureEngine.signatures(excluding: excludedThreadIDs)
        guard !signatures.isEmpty else { return [] }

        let trimmed = interpretation.normalizedText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else {
            return recencyFallback(from: signatures.map(\.thread), limit: limit)
        }

        let ranked = evaluate(interpretation: interpretation, excluding: excludedThreadIDs).ranked.map {
            ThreadSuggestion(thread: $0.thread, score: $0.totalScore, reason: $0.reason)
        }
        let positiveMatches = ranked.filter { $0.score > 0 }
        if !positiveMatches.isEmpty {
            return Array(positiveMatches.prefix(limit))
        }
        return recencyFallback(from: signatures.map(\.thread), limit: limit)
    }

    func decide(
        noteText: String,
        excluding excludedThreadIDs: Set<UUID> = []
    ) -> RoutingDecision {
        decide(
            interpretation: captureInterpreter.interpret(text: noteText),
            excluding: excludedThreadIDs
        )
    }

    func decide(
        interpretation: CaptureInterpretation,
        excluding excludedThreadIDs: Set<UUID> = []
    ) -> RoutingDecision {
        evaluate(interpretation: interpretation, excluding: excludedThreadIDs).decision
    }

    func debugState(
        noteText: String,
        excluding excludedThreadIDs: Set<UUID> = [],
        limit: Int = 3
    ) -> RouteDebugState {
        debugState(
            interpretation: captureInterpreter.interpret(text: noteText),
            excluding: excludedThreadIDs,
            limit: limit
        )
    }

    func debugState(
        interpretation: CaptureInterpretation,
        excluding excludedThreadIDs: Set<UUID> = [],
        limit: Int = 3
    ) -> RouteDebugState {
        let support = supportSnapshot(
            interpretation: interpretation,
            excluding: excludedThreadIDs,
            limit: limit
        )
        let evaluation = evaluate(interpretation: interpretation, excluding: excludedThreadIDs)

        switch evaluation.decision {
        case let .route(threadID, _, reason):
            let selectedThread = evaluation.ranked.first(where: { $0.thread.id == threadID })?.thread
            return RouteDebugState(
                plannerLabel: "Deterministic router",
                supportEngineLabel: "Deterministic routing engine",
                status: .routed,
                message: reason,
                connectivityStatus: nil,
                connectivityMessage: nil,
                connectivityCheckedAt: nil,
                normalizedText: support.normalizedText,
                detectedItemType: support.detectedItemType,
                detectedObjects: support.detectedObjects,
                candidateClaims: support.candidateClaims,
                routingQueries: support.routingQueries,
                topCandidates: support.rankedCandidates,
                decisionReason: reason,
                plannedSuggestions: [],
                selectedThreadID: threadID,
                selectedThreadTitle: selectedThread?.title,
                topScore: support.topScore,
                secondScore: support.secondScore,
                autoRouteThreshold: support.autoRouteThreshold,
                autoRouteGapThreshold: support.autoRouteGapThreshold,
                backendLabel: nil,
                configuredModelID: nil,
                responseModelID: nil,
                responseID: nil,
                finishReason: nil,
                warnings: [],
                promptStats: nil,
                parsedResponse: nil,
                rawResponseBody: nil,
                updatedAt: .now
            )
        case let .noMatch(reason):
            return RouteDebugState(
                plannerLabel: "Deterministic router",
                supportEngineLabel: "Deterministic routing engine",
                status: .stayedInInbox,
                message: reason,
                connectivityStatus: nil,
                connectivityMessage: nil,
                connectivityCheckedAt: nil,
                normalizedText: support.normalizedText,
                detectedItemType: support.detectedItemType,
                detectedObjects: support.detectedObjects,
                candidateClaims: support.candidateClaims,
                routingQueries: support.routingQueries,
                topCandidates: support.rankedCandidates,
                decisionReason: reason,
                plannedSuggestions: [],
                selectedThreadID: nil,
                selectedThreadTitle: nil,
                topScore: support.topScore,
                secondScore: support.secondScore,
                autoRouteThreshold: support.autoRouteThreshold,
                autoRouteGapThreshold: support.autoRouteGapThreshold,
                backendLabel: nil,
                configuredModelID: nil,
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
    }

    func supportSnapshot(
        interpretation: CaptureInterpretation,
        excluding excludedThreadIDs: Set<UUID> = [],
        limit: Int = 5
    ) -> RouteSupportSnapshot {
        let signatures = threadSignatureEngine.signatures(excluding: excludedThreadIDs)
        let ranked = rankThreads(using: interpretation, signatures: signatures)
        let candidates = Array(ranked.prefix(limit)).map { ranked in
            let signature = signatures.first(where: { $0.thread.id == ranked.thread.id })
            return RouteCandidateDebug(
                threadID: ranked.thread.id,
                threadTitle: ranked.thread.title,
                goalStatement: signature?.goalStatement ?? ranked.thread.goalLayer.goalStatement,
                coreObjects: signature?.coreObjects.map(\.name) ?? [],
                activeClaims: signature?.activeClaims ?? [],
                latestAnchorSummary: signature?.latestAnchorSummary,
                openLoops: signature?.openLoops ?? [],
                totalScore: ranked.totalScore,
                semanticScore: ranked.semantic.score,
                retrievalScore: ranked.retrieval.score,
                reason: ranked.reason
            )
        }
        return RouteSupportSnapshot(
            normalizedText: interpretation.normalizedText,
            detectedItemType: interpretation.detectedItemType.rawValue,
            detectedObjects: interpretation.detectedObjects.map(\.name),
            candidateClaims: interpretation.candidateClaims.map(\.text),
            routingQueries: routingQueries(for: interpretation),
            rankedCandidates: candidates,
            topScore: ranked.first?.totalScore,
            secondScore: ranked.dropFirst().first?.totalScore,
            autoRouteThreshold: autoRouteThreshold,
            autoRouteGapThreshold: autoRouteGapThreshold
        )
    }

    func signature(for threadID: UUID) -> ThreadSignature? {
        threadSignatureEngine.signature(for: threadID)
    }

    private func evaluate(
        interpretation: CaptureInterpretation,
        excluding excludedThreadIDs: Set<UUID>
    ) -> RouteEvaluation {
        let signatures = threadSignatureEngine.signatures(excluding: excludedThreadIDs)
        guard !signatures.isEmpty else {
            return RouteEvaluation(
                interpretation: interpretation,
                ranked: [],
                decision: .noMatch(reason: "No active threads available.")
            )
        }

        let trimmed = interpretation.normalizedText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else {
            return RouteEvaluation(
                interpretation: interpretation,
                ranked: [],
                decision: .noMatch(reason: "Empty capture text.")
            )
        }

        let ranked = rankThreads(using: interpretation, signatures: signatures)
        guard let top = ranked.first, top.totalScore > 0 else {
            return RouteEvaluation(
                interpretation: interpretation,
                ranked: ranked,
                decision: .noMatch(reason: "No confident thread match.")
            )
        }

        let secondScore = ranked.dropFirst().first?.totalScore ?? 0
        guard top.totalScore >= autoRouteThreshold else {
            return RouteEvaluation(
                interpretation: interpretation,
                ranked: ranked,
                decision: .noMatch(reason: "Top thread score is below the auto-route threshold.")
            )
        }
        guard top.totalScore - secondScore >= autoRouteGapThreshold else {
            return RouteEvaluation(
                interpretation: interpretation,
                ranked: ranked,
                decision: .noMatch(reason: "Top thread is not separated enough from the next candidate.")
            )
        }

        return RouteEvaluation(
            interpretation: interpretation,
            ranked: ranked,
            decision: .route(threadID: top.thread.id, score: top.totalScore, reason: top.reason)
        )
    }

    private func rankThreads(
        using interpretation: CaptureInterpretation,
        signatures: [ThreadSignature]
    ) -> [RankedThread] {
        let retrievalSupport = retrievalSupportScores(
            for: interpretation,
            candidates: signatures.map(\.thread)
        )

        return signatures
            .map { signature in
                let semantic = semanticScore(for: interpretation, signature: signature)
                let support = retrievalSupport[signature.thread.id] ?? RetrievalSupport(score: 0, reason: nil)
                return RankedThread(
                    thread: signature.thread,
                    semantic: semantic,
                    retrieval: support
                )
            }
            .sorted { lhs, rhs in
                if lhs.totalScore == rhs.totalScore { return lhs.thread.lastActiveAt > rhs.thread.lastActiveAt }
                return lhs.totalScore > rhs.totalScore
            }
    }

    private func semanticScore(
        for interpretation: CaptureInterpretation,
        signature: ThreadSignature
    ) -> ScoreBreakdown {
        var total = 0
        var bestReason: (score: Int, text: String)?

        func collect(_ score: Int, _ reason: String?) {
            guard score > 0 else { return }
            total += score
            if let reason {
                if bestReason == nil || score > bestReason?.score ?? 0 {
                    bestReason = (score, reason)
                }
            }
        }

        func collect(_ breakdown: ScoreBreakdown) {
            collect(breakdown.score, breakdown.reason)
        }

        let normalized = interpretation.normalizedText.lowercased()
        let queryTokens = Set(interpretation.routingSignals.keywords.map { $0.lowercased() })

        let titleGoalTexts = [signature.thread.title, signature.goalStatement]
        collect(bestTextOverlapScore(query: normalized, queryTokens: queryTokens, texts: titleGoalTexts, weight: 3, cap: 14))
        collect(bestTextOverlapScore(query: normalized, queryTokens: queryTokens, texts: signature.activeClaims, weight: 4, cap: 18))

        if let anchorSummary = signature.latestAnchorSummary {
            collect(bestTextOverlapScore(query: normalized, queryTokens: queryTokens, texts: [anchorSummary], weight: 3, cap: 12))
        }

        collect(bestTextOverlapScore(query: normalized, queryTokens: queryTokens, texts: signature.openLoops, weight: 2, cap: 8))

        if !interpretation.detectedObjects.isEmpty && !signature.coreObjects.isEmpty {
            let detected = Set(interpretation.detectedObjects.map { $0.name.lowercased() })
            let matches = signature.coreObjects.filter { detected.contains($0.name.lowercased()) }
            if let first = matches.first {
                collect(min(16, matches.count * 8), "Matches object \(first.name)")
            }
        }

        if !interpretation.candidateClaims.isEmpty && !signature.activeClaims.isEmpty {
            let claimQueries = interpretation.candidateClaims.map(\.text)
            collect(bestPhraseContainmentScore(queries: claimQueries, texts: signature.activeClaims, score: 12))
        }

        switch interpretation.detectedItemType {
        case .question where !signature.openLoops.isEmpty:
            collect(2, signature.openLoops.first)
        case .claim where !signature.activeClaims.isEmpty:
            collect(2, signature.activeClaims.first)
        case .plan where signature.latestAnchorSummary != nil:
            collect(2, signature.latestAnchorSummary)
        default:
            break
        }

        return ScoreBreakdown(score: total, reason: bestReason?.text)
    }

    private func bestTextOverlapScore(
        query: String,
        queryTokens: Set<String>,
        texts: [String],
        weight: Int,
        cap: Int
    ) -> ScoreBreakdown {
        var best = ScoreBreakdown(score: 0, reason: nil)

        for text in texts where !text.isEmpty {
            let lowered = text.lowercased()
            var score = 0
            if query.count >= 4 && (lowered.contains(query) || query.contains(lowered)) {
                score = max(score, cap)
            }
            let overlap = queryTokens.intersection(Set(tokenizeForSearch(text))).count
            score = max(score, min(cap, overlap * weight))
            if score > best.score {
                best = ScoreBreakdown(score: score, reason: text)
            }
        }

        return best
    }

    private func bestPhraseContainmentScore(
        queries: [String],
        texts: [String],
        score: Int
    ) -> ScoreBreakdown {
        for query in queries where query.count >= 4 {
            let loweredQuery = query.lowercased()
            for text in texts where !text.isEmpty {
                let loweredText = text.lowercased()
                if loweredText.contains(loweredQuery) || loweredQuery.contains(loweredText) {
                    return ScoreBreakdown(score: score, reason: text)
                }
            }
        }
        return ScoreBreakdown(score: 0, reason: nil)
    }

    private func retrievalSupportScores(
        for interpretation: CaptureInterpretation,
        candidates: [ThreadRecord]
    ) -> [UUID: RetrievalSupport] {
        guard !candidates.isEmpty else { return [:] }

        let queries = routingQueries(for: interpretation)
        guard !queries.isEmpty else { return [:] }

        var supportByThread: [UUID: RetrievalSupport] = [:]
        for query in queries {
            let ranked = (try? retrievalEngine.rankThreads(for: query, candidates: candidates)) ?? []
            for suggestion in ranked where suggestion.score > 0 {
                let boosted = min(12, max(1, suggestion.score / 8))
                supportByThread[suggestion.thread.id, default: RetrievalSupport(score: 0, reason: nil)].score += boosted
                if supportByThread[suggestion.thread.id]?.reason == nil,
                   suggestion.reason != "No matching content" {
                    supportByThread[suggestion.thread.id]?.reason = suggestion.reason
                }
            }
        }
        return supportByThread
    }

    private func routingQueries(for interpretation: CaptureInterpretation) -> [String] {
        var seen = Set<String>()
        let rawQueries = [interpretation.normalizedText]
            + interpretation.candidateClaims.map(\.text)
            + interpretation.detectedObjects.map(\.name)

        return rawQueries.compactMap { query in
            let trimmed = query.trimmingCharacters(in: .whitespacesAndNewlines)
            guard trimmed.count >= 2 else { return nil }
            let key = trimmed.lowercased()
            guard seen.insert(key).inserted else { return nil }
            return trimmed
        }
        .prefix(3)
        .map { $0 }
    }

    private func recencyFallback(from candidates: [ThreadRecord], limit: Int) -> [ThreadSuggestion] {
        Array(candidates.prefix(limit)).enumerated().map { index, thread in
            ThreadSuggestion(
                thread: thread,
                score: max(1, limit - index),
                reason: "Recently active"
            )
        }
    }
}
