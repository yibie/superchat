import Foundation
import NaturalLanguage

struct CaptureInterpreter {
    func interpret(text rawText: String) -> CaptureInterpretation {
        let trimmed = rawText.trimmingCharacters(in: .whitespacesAndNewlines)
        let explicitTag = CaptureTag.parseTag(in: trimmed)
        let normalizedText = stripExplicitTag(from: trimmed, explicitTag: explicitTag)
        let detectedType = inferItemType(from: normalizedText, explicitTag: explicitTag)
        let detectedObjects = parseObjectMentions(in: normalizedText)
        let candidateClaims = extractCandidateClaims(
            from: normalizedText,
            detectedItemType: detectedType.kind
        )

        let routingSignals = RoutingSignals(
            keywords: routingKeywords(
                text: normalizedText,
                objects: detectedObjects,
                candidateClaims: candidateClaims
            ),
            objectNames: detectedObjects.map(\.name),
            candidateClaimTexts: candidateClaims.map(\.text)
        )

        let confidence = max(
            detectedType.confidence,
            candidateClaims.map(\.confidenceScore).max() ?? 0,
            detectedObjects.isEmpty ? 0.25 : 0.55
        )

        return CaptureInterpretation(
            normalizedText: normalizedText,
            explicitTag: explicitTag,
            detectedItemType: detectedType.kind,
            detectedObjects: detectedObjects,
            candidateClaims: candidateClaims,
            routingSignals: routingSignals,
            confidenceScore: min(1.0, confidence)
        )
    }

    func interpret(entry: Entry) -> CaptureInterpretation {
        let normalizedText = entry.summaryText.trimmingCharacters(in: .whitespacesAndNewlines)
        let detectedObjects = entry.objectMentions.isEmpty ? parseObjectMentions(in: normalizedText) : entry.objectMentions
        let candidateClaims = extractCandidateClaims(
            from: normalizedText,
            detectedItemType: entry.kind
        )

        return CaptureInterpretation(
            normalizedText: normalizedText,
            explicitTag: nil,
            detectedItemType: entry.kind,
            detectedObjects: detectedObjects,
            candidateClaims: candidateClaims,
            routingSignals: RoutingSignals(
                keywords: routingKeywords(
                    text: normalizedText,
                    objects: detectedObjects,
                    candidateClaims: candidateClaims
                ),
                objectNames: detectedObjects.map(\.name),
                candidateClaimTexts: candidateClaims.map(\.text)
            ),
            confidenceScore: entry.confidenceScore ?? max(0.35, candidateClaims.map(\.confidenceScore).max() ?? 0.35)
        )
    }

    func extractObjects(from texts: [String]) -> [ObjectMention] {
        mergeMentions(texts.flatMap(parseObjectMentions(in:)))
    }

    private func stripExplicitTag(from text: String, explicitTag: CaptureTag?) -> String {
        guard explicitTag != nil else { return text }
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        guard let match = CaptureTag.tagRegex.firstMatch(in: text, options: [], range: range) else {
            return text
        }
        let stripped = (text as NSString).replacingCharacters(in: match.range, with: "")
        return stripped
            .replacingOccurrences(of: #"\s{2,}"#, with: " ", options: .regularExpression)
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }

    private func inferItemType(from text: String, explicitTag: CaptureTag?) -> (kind: EntryKind, confidence: Double) {
        guard let explicitTag else {
            let trimmed = text.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !trimmed.isEmpty else {
                return (.note, 0.2)
            }
            let lowered = trimmed.lowercased()

            if isURLHeavy(trimmed) {
                return (.source, 0.95)
            }
            if isQuestion(lowered, original: trimmed) {
                return (.question, 0.9)
            }
            if containsAny(lowered, in: ["decision:", "decided", "we decided", "final decision"]) {
                return (.decided, 0.88)
            }
            if containsAny(lowered, in: ["solved", "fixed", "resolved", "issue closed"]) {
                return (.solved, 0.86)
            }
            if containsAny(lowered, in: ["verified", "confirmed", "validated"]) {
                return (.verified, 0.84)
            }
            if containsAny(lowered, in: ["dropped", "not pursuing", "ruled out", "won't pursue", "will not pursue"]) {
                return (.dropped, 0.84)
            }
            if containsAny(lowered, in: [" vs ", "versus", "compare", "comparison"]) {
                return (.comparison, 0.82)
            }
            if containsAny(lowered, in: ["pattern", "recurring", "keeps happening", "shows up again", "same shape"]) {
                return (.pattern, 0.78)
            }
            if hasPrefixAny(lowered, in: ["todo", "next step", "follow up", "follow-up", "action:", "plan:", "next:"]) {
                return (.plan, 0.76)
            }
            if containsEvidenceCue(lowered) {
                return (.evidence, 0.74)
            }
            if looksLikeClaim(lowered) {
                return (.claim, 0.7)
            }
            return (.note, 0.35)
        }

        return (explicitTag.entryKind, 1)
    }

    private func extractCandidateClaims(from text: String, detectedItemType: EntryKind) -> [CandidateClaim] {
        let sentences = splitSentences(in: text)
        guard !sentences.isEmpty else { return [] }

        let baseConfidence: Double
        switch detectedItemType {
        case .claim, .decided, .solved, .verified, .dropped:
            baseConfidence = 0.92
        case .evidence, .comparison, .pattern:
            baseConfidence = 0.66
        case .note, .idea, .plan:
            baseConfidence = 0.58
        default:
            baseConfidence = 0.0
        }

        guard baseConfidence > 0 else { return [] }

        var seen = Set<String>()
        return sentences.compactMap { sentence in
            let cleaned = normalizeClaimSentence(sentence)
            let lowered = cleaned.lowercased()
            guard !cleaned.isEmpty,
                  !isQuestion(lowered, original: cleaned),
                  looksLikeClaim(lowered) || detectedItemType == .claim || detectedItemType == .decided || detectedItemType == .solved || detectedItemType == .verified || detectedItemType == .dropped else {
                return nil
            }
            let key = cleaned.lowercased()
            guard seen.insert(key).inserted else { return nil }
            return CandidateClaim(text: cleaned, confidenceScore: baseConfidence)
        }
        .prefix(2)
        .map { $0 }
    }

    private func routingKeywords(
        text: String,
        objects: [ObjectMention],
        candidateClaims: [CandidateClaim]
    ) -> [String] {
        var keywords = tokenizeForSearch(text)
        keywords.append(contentsOf: objects.map(\.name))
        keywords.append(contentsOf: candidateClaims.flatMap { tokenizeForSearch($0.text) })

        var seen = Set<String>()
        return keywords.compactMap { keyword in
            let trimmed = keyword.trimmingCharacters(in: .whitespacesAndNewlines)
            guard trimmed.count > 1 else { return nil }
            let key = trimmed.lowercased()
            guard seen.insert(key).inserted else { return nil }
            return trimmed
        }
    }

    private func containsAny(_ text: String, in phrases: [String]) -> Bool {
        phrases.contains { text.contains($0) }
    }

    private func hasPrefixAny(_ text: String, in prefixes: [String]) -> Bool {
        prefixes.contains { text.hasPrefix($0) }
    }

    private func isQuestion(_ lowered: String, original: String) -> Bool {
        if original.hasSuffix("?") || original.hasSuffix("？") {
            return true
        }
        let prefixes = [
            "why ", "how ", "what ", "who ", "where ", "when ",
            "should ", "can ", "is ", "are ", "do ", "does ", "did ",
            "which ", "could ", "would "
        ]
        return prefixes.contains { lowered.hasPrefix($0) }
    }

    private func containsEvidenceCue(_ lowered: String) -> Bool {
        if containsAny(lowered, in: ["because", "evidence", "data", "metric", "metrics", "observed", "according to", "results show", "numbers show"]) {
            return true
        }
        return lowered.range(of: #"\b\d+([.,]\d+)?%?\b"#, options: .regularExpression) != nil
    }

    private func looksLikeClaim(_ lowered: String) -> Bool {
        let claimCues = [
            " is ", " are ", " will ", " should ", " suggests ", " means ",
            " likely ", " unlikely ", " better ", " worse ", " need to ",
            " seems ", " appears ", " indicates ", " blocks ", " matters "
        ]
        if containsAny(" \(lowered) ", in: claimCues) {
            return true
        }
        let tokenCount = tokenizeForSearch(lowered).count
        return tokenCount >= 4 && !containsEvidenceCue(lowered)
    }

    private func isURLHeavy(_ text: String) -> Bool {
        guard let detector = try? NSDataDetector(types: NSTextCheckingResult.CheckingType.link.rawValue) else {
            return false
        }
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        let urls = detector.matches(in: text, options: [], range: range).compactMap(\.url)
        guard !urls.isEmpty else { return false }
        return urls.count == 1 && text.replacingOccurrences(of: urls[0].absoluteString, with: "").trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
    }

    private func splitSentences(in text: String) -> [String] {
        text
            .split(whereSeparator: { [".", "!", "?", "。", "！", "？", "\n"].contains($0) })
            .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) }
            .filter { !$0.isEmpty }
    }

    private func normalizeClaimSentence(_ sentence: String) -> String {
        sentence
            .replacingOccurrences(of: #"^(i think|we think|it seems that|i suspect|we suspect)\s+"#, with: "", options: [.regularExpression, .caseInsensitive])
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }
}

func parseObjectMentions(in text: String) -> [ObjectMention] {
    mergeMentions(
        explicitAtMentions(in: text)
        + namedEntityMentions(in: text)
        + productMentions(in: text)
    )
}

func inferObjectKind(for name: String) -> ObjectKind {
    let lowered = name.lowercased()
    if lowered.hasSuffix("inc")
        || lowered.hasSuffix("labs")
        || lowered.hasSuffix("corp")
        || lowered.hasSuffix("llc")
        || lowered.contains("openai")
        || lowered.contains("anthropic")
        || lowered.contains("google")
        || lowered.contains("threadnote") {
        return .company
    }
    if name.range(of: #"[A-Z].*[A-Z]"#, options: .regularExpression) != nil
        || name.range(of: #"\d"#, options: .regularExpression) != nil {
        return .product
    }
    return .generic
}

private func explicitAtMentions(in text: String) -> [ObjectMention] {
    guard let regex = try? NSRegularExpression(pattern: #"(?<!\S)@([\p{L}\p{N}][\p{L}\p{N}._-]*)"#, options: []) else {
        return []
    }
    let range = NSRange(text.startIndex..<text.endIndex, in: text)
    return regex.matches(in: text, options: [], range: range)
        .compactMap { match -> ObjectMention? in
            guard let nameRange = Range(match.range(at: 1), in: text) else { return nil }
            let name = String(text[nameRange])
            return ObjectMention(id: UUID(), name: name, kind: inferObjectKind(for: name))
        }
}

private func namedEntityMentions(in text: String) -> [ObjectMention] {
    let tagger = NLTagger(tagSchemes: [.nameTypeOrLexicalClass])
    tagger.string = text

    var mentions: [ObjectMention] = []
    let options: NLTagger.Options = [.omitWhitespace, .omitPunctuation, .joinNames]
    tagger.enumerateTags(in: text.startIndex..<text.endIndex, unit: .word, scheme: .nameTypeOrLexicalClass, options: options) { tag, range in
        guard let tag else { return true }
        let name = String(text[range]).trimmingCharacters(in: .whitespacesAndNewlines)
        guard name.count > 1 else { return true }

        let kind: ObjectKind?
        switch tag {
        case .personalName:
            kind = .person
        case .placeName:
            kind = .place
        case .organizationName:
            kind = .company
        default:
            kind = nil
        }

        if let kind {
            mentions.append(ObjectMention(id: UUID(), name: name, kind: kind))
        }
        return true
    }

    return mentions
}

private func productMentions(in text: String) -> [ObjectMention] {
    let patterns = [
        #"(?:(?:ship|launch|build|use|adopt|deploy|compare|review|pricing for|roadmap for)\s+)([A-Z][\p{L}\p{N}.-]+(?:\s+[A-Z][\p{L}\p{N}.-]+){0,2})"#,
        #"\b([A-Z][a-z]+(?:[A-Z][\p{L}\p{N}]+)+)\b"#,
        #"\b([A-Z]{2,}[A-Za-z0-9.-]*)\b"#,
    ]

    var mentions: [ObjectMention] = []
    let stopwords: Set<String> = [
        "The", "This", "That", "These", "Those", "Today", "Tomorrow", "Yesterday"
    ]

    for pattern in patterns {
        guard let regex = try? NSRegularExpression(pattern: pattern, options: []) else { continue }
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        for match in regex.matches(in: text, options: [], range: range) {
            guard let captureRange = Range(match.range(at: 1), in: text) else { continue }
            let name = String(text[captureRange]).trimmingCharacters(in: .whitespacesAndNewlines)
            guard name.count > 1, !stopwords.contains(name) else { continue }
            mentions.append(ObjectMention(id: UUID(), name: name, kind: inferObjectKind(for: name)))
        }
    }

    return mentions
}

private func mergeMentions(_ mentions: [ObjectMention]) -> [ObjectMention] {
    var seen = Set<String>()
    return mentions.compactMap { mention in
        let key = mention.name.lowercased()
        guard seen.insert(key).inserted else { return nil }
        return mention
    }
}
