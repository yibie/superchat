import Foundation

enum ThreadColor: String, Codable, CaseIterable {
    case rose, amber, lime, teal, sky, violet, fuchsia, orange, emerald, indigo
}

enum EntryKind: String, Codable, CaseIterable, Identifiable {
    case note
    case idea
    case question
    case claim
    case evidence
    case source
    case comparison
    case pattern
    case plan
    case decided
    case solved
    case verified
    case dropped
    case handoff
    case anchorWritten

    var id: Self { self }

    var title: String {
        switch self {
        case .note:
            "Note"
        case .idea:
            "Idea"
        case .question:
            "Question"
        case .claim:
            "Claim"
        case .evidence:
            "Evidence"
        case .source:
            "Source"
        case .comparison:
            "Comparison"
        case .pattern:
            "Pattern"
        case .plan:
            "Plan"
        case .decided:
            "Decided"
        case .solved:
            "Solved"
        case .verified:
            "Verified"
        case .dropped:
            "Dropped"
        case .handoff:
            "Handoff"
        case .anchorWritten:
            "Checkpoint"
        }
    }
}

enum EntryBodyKind: String, Codable, CaseIterable, Identifiable {
    case text
    case url
    case image
    case document
    case mixed

    var id: Self { self }
}

enum LinkContentType: String, Codable, Hashable {
    case webpage, video, image, audio, document
}

struct LinkMetadata: Codable, Hashable {
    var metaTitle: String?
    var metaDescription: String?
    var imageURL: String?
    var siteName: String?
    var videoID: String?
    var contentType: LinkContentType?
}

struct SourceMetadata: Codable, Hashable {
    var title: String?
    var locator: String?
    var citation: String?
}

struct EntryBody: Codable, Hashable {
    var kind: EntryBodyKind
    var text: String?
    var url: String?
    var title: String?
    var details: String?
    var linkMeta: LinkMetadata?
}

enum ThreadStatus: String, Codable, CaseIterable, Identifiable {
    case active
    case archived

    var id: Self { self }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let rawValue = try container.decode(String.self)
        switch rawValue {
        case "active":
            self = .active
        case "paused", "resolved", "archived":
            self = .archived
        default:
            self = .active
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(rawValue)
    }
}

enum ThreadGoalType: String, Codable, CaseIterable, Identifiable, Sendable {
    case build
    case study
    case research

    var id: Self { self }

    var title: String {
        switch self {
        case .build:
            "Build"
        case .study:
            "Study"
        case .research:
            "Research"
        }
    }

    var systemImage: String {
        switch self {
        case .build:
            "hammer"
        case .study:
            "book"
        case .research:
            "magnifyingglass"
        }
    }

    static func suggested(for text: String) -> ThreadGoalType {
        let lowered = text.lowercased()
        if lowered.contains("research")
            || lowered.contains("survey")
            || lowered.contains("compare")
            || lowered.contains("competitor")
            || lowered.contains("market")
            || lowered.contains("product")
            || lowered.contains("openclaw")
        {
            return .research
        }
        if lowered.contains("watch")
            || lowered.contains("read")
            || lowered.contains("film")
            || lowered.contains("movie")
            || lowered.contains("author")
            || lowered.contains("summary")
            || lowered.contains("style")
            || lowered.contains("study")
        {
            return .study
        }
        return .build
    }
}

enum ThreadGoalStage: String, Codable, CaseIterable, Identifiable, Sendable {
    case framing
    case gathering
    case synthesizing
    case concluding

    var id: Self { self }

    var title: String {
        rawValue.capitalized
    }
}

struct ThreadGoalLayer: Hashable, Codable, Sendable {
    var goalStatement: String
    var goalType: ThreadGoalType
    var successCondition: String
    var currentStage: ThreadGoalStage

    static func legacyDefault(title: String, prompt: String) -> ThreadGoalLayer {
        let statement = prompt.isEmpty ? title : prompt
        return ThreadGoalLayer(
            goalStatement: statement,
            goalType: ThreadGoalType.suggested(for: "\(title) \(statement)"),
            successCondition: "Reach a clearer conclusion for this thread.",
            currentStage: .framing
        )
    }
}

enum ClaimStatus: String, Codable, CaseIterable, Identifiable {
    case candidate
    case working
    case stable
    case superseded

    var id: Self { self }
}

enum PreparedViewType: String, Codable, CaseIterable, Identifiable {
    case writing
    case meeting
    case decision

    var id: Self { self }

    var title: String {
        switch self {
        case .writing:
            "Writing"
        case .meeting:
            "Meeting"
        case .decision:
            "Decision"
        }
    }
}

enum CaptureTag: String, CaseIterable, Identifiable, Hashable, Codable, Sendable {
    case note
    case idea
    case question
    case claim
    case evidence
    case source
    case comparison
    case pattern
    case plan
    case decided
    case solved
    case verified
    case dropped

    var id: Self { self }

    var insertionText: String {
        "#\(rawValue)"
    }

    var displayText: String {
        insertionText
    }

    var suggestionTitle: String {
        switch self {
        case .note:
            "General note"
        case .idea:
            "Idea"
        case .question:
            "Open question"
        case .claim:
            "Current take"
        case .evidence:
            "Observed evidence"
        case .source:
            "External source"
        case .comparison:
            "Comparison"
        case .pattern:
            "Pattern"
        case .plan:
            "Next step"
        case .decided:
            "Decision made"
        case .solved:
            "Problem solved"
        case .verified:
            "Verified result"
        case .dropped:
            "Dropped path"
        }
    }

    var suggestionHint: String {
        switch self {
        case .note:
            "Use when you are just recording quickly."
        case .idea:
            "Early direction not ready as a claim."
        case .question:
            "Something you still need to answer."
        case .claim:
            "Your current judgment or stance."
        case .evidence:
            "Fact or observation that supports or opposes."
        case .source:
            "Reference link, material, or citation."
        case .comparison:
            "Comparing options, cases, or samples."
        case .pattern:
            "Recurring signal across samples."
        case .plan:
            "The next move to push the thread."
        case .decided:
            "A stable decision to keep in memory."
        case .solved:
            "An issue that is now resolved."
        case .verified:
            "A result that has been confirmed."
        case .dropped:
            "A path intentionally abandoned."
        }
    }

    var suggestionRank: Int {
        switch self {
        case .question: 10
        case .claim: 20
        case .evidence: 30
        case .source: 40
        case .note: 50
        case .idea: 60
        case .plan: 70
        case .decided: 80
        case .solved: 90
        case .verified: 100
        case .dropped: 110
        case .comparison: 120
        case .pattern: 130
        }
    }

    private static let searchTokensMap: [CaptureTag: [String]] = [
        .note: ["note", "record", "capture", "memo", "记录", "笔记"],
        .idea: ["idea", "direction", "thought", "想法", "方向"],
        .question: ["question", "ask", "open", "problem", "问题", "疑问"],
        .claim: ["claim", "take", "stance", "judgment", "观点", "判断"],
        .evidence: ["evidence", "fact", "proof", "observation", "依据", "证据"],
        .source: ["source", "reference", "citation", "link", "来源", "参考"],
        .comparison: ["comparison", "compare", "versus", "对比", "比较"],
        .pattern: ["pattern", "motif", "recurring", "模式", "规律"],
        .plan: ["plan", "next", "todo", "action", "计划", "下一步"],
        .decided: ["decided", "decision", "settled", "决定", "定案"],
        .solved: ["solved", "resolved", "fixed", "解决", "已解决"],
        .verified: ["verified", "confirmed", "check", "验证", "确认"],
        .dropped: ["dropped", "discarded", "ruledout", "放弃", "排除"],
    ]

    var suggestionSearchTokens: [String] {
        Self.searchTokensMap[self] ?? []
    }

    static let tagRegex: NSRegularExpression = {
        let tags = CaptureTag.allCases.map(\.rawValue).joined(separator: "|")
        return try! NSRegularExpression(pattern: #"(?<!\S)#("# + tags + #")\b"#, options: [.caseInsensitive])
    }()

    static func parseTag(in text: String) -> CaptureTag? {
        let range = NSRange(text.startIndex..<text.endIndex, in: text)
        guard let match = tagRegex.firstMatch(in: text, options: [], range: range),
              let tagRange = Range(match.range(at: 1), in: text) else {
            return nil
        }
        return CaptureTag(rawValue: text[tagRange].lowercased())
    }

    var entryKind: EntryKind {
        switch self {
        case .note:
            .note
        case .idea:
            .idea
        case .question:
            .question
        case .claim:
            .claim
        case .evidence:
            .evidence
        case .source:
            .source
        case .comparison:
            .comparison
        case .pattern:
            .pattern
        case .plan:
            .plan
        case .decided:
            .decided
        case .solved:
            .solved
        case .verified:
            .verified
        case .dropped:
            .dropped
        }
    }
}

struct CaptureSuggestion: Identifiable, Hashable {
    let tag: CaptureTag
    let score: Int

    var id: CaptureTag { tag }
}

struct CaptureComposerState: Hashable {
    var selectedTag: CaptureTag?
    var activeQuery: String = ""
    var suggestions: [CaptureSuggestion] = []
    var highlightedSuggestionIndex: Int = 0
    var insertionRange: NSRange?

    var isShowingSuggestions: Bool { insertionRange != nil && !suggestions.isEmpty }
}

struct CaptureParseResult: Hashable {
    var interpretation: CaptureInterpretation
    var body: EntryBody
    var sourceMetadata: SourceMetadata?
    var references: [EntryReference]

    var semanticKind: EntryKind { interpretation.detectedItemType }
    var strippedText: String { interpretation.normalizedText }
    var matchedTag: CaptureTag? { interpretation.explicitTag }
    var objectMentions: [ObjectMention] { interpretation.detectedObjects }
}

struct CandidateClaim: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var text: String
    var confidenceScore: Double
}

struct RoutingSignals: Hashable, Codable, Sendable {
    var keywords: [String]
    var objectNames: [String]
    var candidateClaimTexts: [String]

    var queries: [String] {
        var seen = Set<String>()
        var results: [String] = []
        for value in candidateClaimTexts + objectNames + keywords {
            let trimmed = value.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !trimmed.isEmpty else { continue }
            let key = trimmed.lowercased()
            guard seen.insert(key).inserted else { continue }
            results.append(trimmed)
        }
        return results
    }
}

struct CaptureInterpretation: Hashable, Codable {
    var normalizedText: String
    var explicitTag: CaptureTag?
    var detectedItemType: EntryKind
    var detectedObjects: [ObjectMention]
    var candidateClaims: [CandidateClaim]
    var routingSignals: RoutingSignals
    var confidenceScore: Double
}

enum DiscourseRelationKind: String, Codable, CaseIterable, Identifiable {
    case supports
    case opposes
    case informs
    case answers

    var id: Self { self }

    var title: String {
        switch self {
        case .supports:
            "Supports"
        case .opposes:
            "Opposes"
        case .informs:
            "Informs"
        case .answers:
            "Answers"
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let rawValue = try container.decode(String.self)
        switch rawValue {
        case "supports":
            self = .supports
        case "opposes", "challenges":
            self = .opposes
        case "informs", "cites":
            self = .informs
        case "answers", "questions", "extends":
            self = .answers
        default:
            self = .supports
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(rawValue)
    }
}

enum ObjectKind: String, Codable, CaseIterable, Identifiable, Sendable {
    case person
    case contact
    case company
    case product
    case book
    case film
    case paper
    case event
    case place
    case generic

    var id: Self { self }
}

struct ObjectMention: Identifiable, Codable, Hashable, Sendable {
    var id: UUID
    var name: String
    var kind: ObjectKind
}

enum ReferenceTargetKind: String, Codable, CaseIterable, Identifiable, Sendable {
    case note
    case thread
    case unresolved

    var id: Self { self }
}

struct EntryReference: Identifiable, Codable, Hashable {
    var id: UUID
    var label: String
    var targetKind: ReferenceTargetKind
    var targetID: UUID?
}

struct Entry: Identifiable, Hashable {
    var id: UUID
    var threadID: UUID?
    var kind: EntryKind
    var body: EntryBody
    var summaryText: String
    var sourceMetadata: SourceMetadata?
    var objectMentions: [ObjectMention] = []
    var references: [EntryReference] = []
    var createdAt: Date
    var sessionID: UUID?
    var authorType: String
    var parentEntryID: UUID?
    var supersedesEntryID: UUID?
    var importanceScore: Double?
    var confidenceScore: Double?
    var inboxState: String

    var content: String { summaryText }

    var isSourceResource: Bool { kind == .source }
    var isAttachmentResource: Bool {
        if kind == .source {
            return true
        }
        if let url = body.url, !url.isEmpty {
            return true
        }
        if let locator = sourceMetadata?.locator, !locator.isEmpty {
            return true
        }
        switch body.kind {
        case .url, .image, .document, .mixed:
            return true
        case .text:
            return false
        }
    }
    var isObjectResourceNote: Bool { !objectMentions.isEmpty }
    var isResourceEntry: Bool { isAttachmentResource || isObjectResourceNote }

    var sourceDisplayTitle: String {
        if let title = sourceMetadata?.title, !title.isEmpty {
            return title
        }
        if let bodyTitle = body.title, !bodyTitle.isEmpty {
            return bodyTitle
        }
        if let url = body.url, !url.isEmpty {
            return url
        }
        if let locator = sourceMetadata?.locator, !locator.isEmpty {
            return locator
        }
        return summaryText
    }

    var sourceLocator: String? {
        if let locator = sourceMetadata?.locator, !locator.isEmpty {
            return locator
        }
        if let url = body.url, !url.isEmpty {
            return url
        }
        return nil
    }
}

extension Entry: Codable {
    private enum CodingKeys: String, CodingKey {
        case id
        case threadID
        case threadLinks
        case kind
        case content
        case body
        case summaryText
        case sourceMetadata
        case objectMentions
        case references
        case createdAt
        case sessionID
        case authorType
        case parentEntryID
        case supersedesEntryID
        case importanceScore
        case confidenceScore
        case inboxState
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(UUID.self, forKey: .id)

        if let tid = try container.decodeIfPresent(UUID.self, forKey: .threadID) {
            threadID = tid
        } else if let links = try container.decodeIfPresent([[String: String]].self, forKey: .threadLinks),
                  let first = links.first?["threadID"],
                  let uuid = UUID(uuidString: first) {
            threadID = uuid
        } else {
            threadID = nil
        }

        if let decodedKind = try container.decodeIfPresent(EntryKind.self, forKey: .kind) {
            kind = decodedKind
        } else {
            let legacyKind = try container.decode(String.self, forKey: .kind)
            switch legacyKind {
            case "note":
                kind = .note
            case "idea":
                kind = .idea
            case "question":
                kind = .question
            case "claimProposed":
                kind = .claim
            case "decision":
                kind = .claim
            case "taskAdded":
                kind = .evidence
            case "comparison":
                kind = .comparison
            case "pattern":
                kind = .pattern
            case "plan":
                kind = .plan
            case "decided":
                kind = .decided
            case "solved":
                kind = .solved
            case "verified":
                kind = .verified
            case "dropped":
                kind = .dropped
            case "handoff":
                kind = .handoff
            case "anchorWritten":
                kind = .anchorWritten
            case "source":
                kind = .source
            default:
                kind = .evidence
            }
        }

        let legacyContent = try container.decodeIfPresent(String.self, forKey: .content) ?? ""
        body = try container.decodeIfPresent(EntryBody.self, forKey: .body)
            ?? EntryBody(kind: .text, text: legacyContent, url: nil, title: nil, details: nil)
        summaryText = try container.decodeIfPresent(String.self, forKey: .summaryText) ?? legacyContent
        sourceMetadata = try container.decodeIfPresent(SourceMetadata.self, forKey: .sourceMetadata)
        objectMentions = try container.decodeIfPresent([ObjectMention].self, forKey: .objectMentions) ?? []
        references = try container.decodeIfPresent([EntryReference].self, forKey: .references) ?? []
        createdAt = try container.decode(Date.self, forKey: .createdAt)
        sessionID = try container.decodeIfPresent(UUID.self, forKey: .sessionID)
        authorType = try container.decode(String.self, forKey: .authorType)
        parentEntryID = try container.decodeIfPresent(UUID.self, forKey: .parentEntryID)
        supersedesEntryID = try container.decodeIfPresent(UUID.self, forKey: .supersedesEntryID)
        importanceScore = try container.decodeIfPresent(Double.self, forKey: .importanceScore)
        confidenceScore = try container.decodeIfPresent(Double.self, forKey: .confidenceScore)
        inboxState = try container.decode(String.self, forKey: .inboxState)
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(id, forKey: .id)
        try container.encodeIfPresent(threadID, forKey: .threadID)
        try container.encode(kind, forKey: .kind)
        try container.encode(body, forKey: .body)
        try container.encode(summaryText, forKey: .summaryText)
        try container.encode(summaryText, forKey: .content)
        try container.encodeIfPresent(sourceMetadata, forKey: .sourceMetadata)
        try container.encode(objectMentions, forKey: .objectMentions)
        try container.encode(references, forKey: .references)
        try container.encode(createdAt, forKey: .createdAt)
        try container.encodeIfPresent(sessionID, forKey: .sessionID)
        try container.encode(authorType, forKey: .authorType)
        try container.encodeIfPresent(parentEntryID, forKey: .parentEntryID)
        try container.encodeIfPresent(supersedesEntryID, forKey: .supersedesEntryID)
        try container.encodeIfPresent(importanceScore, forKey: .importanceScore)
        try container.encodeIfPresent(confidenceScore, forKey: .confidenceScore)
        try container.encode(inboxState, forKey: .inboxState)
    }
}

struct Claim: Identifiable, Codable, Hashable {
    var id: UUID
    var threadID: UUID
    var originEntryID: UUID
    var statement: String
    var status: ClaimStatus
    var createdAt: Date
    var updatedAt: Date
    var confidenceScore: Double
}

struct Anchor: Identifiable, Hashable {
    var id: UUID
    var threadID: UUID
    var createdAt: Date
    var basedOnEntryID: UUID?
    var title: String
    var coreQuestion: String
    var stateSummary: String
    var openLoops: [String]
    var nextSteps: [String]
    var claimIDs: [UUID]
    var evidenceEntryIDs: [UUID]
    var phase: String
}

extension Anchor: Codable {
    private enum CodingKeys: String, CodingKey {
        case id, threadID, createdAt, basedOnEntryID, title
        case coreQuestion, currentQuestion
        case stateSummary, openLoops, nextSteps
        case claimIDs, evidenceEntryIDs, phase
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(UUID.self, forKey: .id)
        threadID = try container.decode(UUID.self, forKey: .threadID)
        createdAt = try container.decode(Date.self, forKey: .createdAt)
        basedOnEntryID = try container.decodeIfPresent(UUID.self, forKey: .basedOnEntryID)
        title = try container.decode(String.self, forKey: .title)
        if let q = try container.decodeIfPresent(String.self, forKey: .coreQuestion) {
            coreQuestion = q
        } else {
            coreQuestion = try container.decode(String.self, forKey: .currentQuestion)
        }
        stateSummary = try container.decode(String.self, forKey: .stateSummary)
        openLoops = try container.decode([String].self, forKey: .openLoops)
        nextSteps = try container.decode([String].self, forKey: .nextSteps)
        claimIDs = try container.decode([UUID].self, forKey: .claimIDs)
        evidenceEntryIDs = try container.decode([UUID].self, forKey: .evidenceEntryIDs)
        phase = try container.decode(String.self, forKey: .phase)
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(id, forKey: .id)
        try container.encode(threadID, forKey: .threadID)
        try container.encode(createdAt, forKey: .createdAt)
        try container.encodeIfPresent(basedOnEntryID, forKey: .basedOnEntryID)
        try container.encode(title, forKey: .title)
        try container.encode(coreQuestion, forKey: .coreQuestion)
        try container.encode(stateSummary, forKey: .stateSummary)
        try container.encode(openLoops, forKey: .openLoops)
        try container.encode(nextSteps, forKey: .nextSteps)
        try container.encode(claimIDs, forKey: .claimIDs)
        try container.encode(evidenceEntryIDs, forKey: .evidenceEntryIDs)
        try container.encode(phase, forKey: .phase)
    }
}

struct DiscourseRelation: Identifiable, Codable, Hashable {
    var id: UUID
    var sourceEntryID: UUID
    var targetEntryID: UUID
    var kind: DiscourseRelationKind
    var confidence: Double
}

struct ThreadTask: Identifiable, Codable, Hashable {
    var id: UUID
    var threadID: UUID
    var originEntryID: UUID?
    var title: String
    var status: String
    var createdAt: Date
    var updatedAt: Date
}

struct ThreadRecord: Identifiable, Hashable {
    var id: UUID
    var title: String
    var prompt: String
    var goalLayer: ThreadGoalLayer
    var status: ThreadStatus
    var createdAt: Date
    var updatedAt: Date
    var lastActiveAt: Date
    var color: ThreadColor
}

extension ThreadRecord: Codable {
    private enum CodingKeys: String, CodingKey {
        case id, title, prompt, goalLayer, status, createdAt, updatedAt, lastActiveAt, color
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(UUID.self, forKey: .id)
        title = try container.decode(String.self, forKey: .title)
        prompt = try container.decode(String.self, forKey: .prompt)
        goalLayer = try container.decodeIfPresent(ThreadGoalLayer.self, forKey: .goalLayer)
            ?? ThreadGoalLayer.legacyDefault(title: title, prompt: prompt)
        status = try container.decode(ThreadStatus.self, forKey: .status)
        createdAt = try container.decode(Date.self, forKey: .createdAt)
        updatedAt = try container.decode(Date.self, forKey: .updatedAt)
        lastActiveAt = try container.decode(Date.self, forKey: .lastActiveAt)
        color = try container.decodeIfPresent(ThreadColor.self, forKey: .color) ?? .sky
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(id, forKey: .id)
        try container.encode(title, forKey: .title)
        try container.encode(prompt, forKey: .prompt)
        try container.encode(goalLayer, forKey: .goalLayer)
        try container.encode(status, forKey: .status)
        try container.encode(createdAt, forKey: .createdAt)
        try container.encode(updatedAt, forKey: .updatedAt)
        try container.encode(lastActiveAt, forKey: .lastActiveAt)
        try container.encode(color, forKey: .color)
    }
}

enum ResumeComponentKind: String, Codable, CaseIterable, Identifiable, Sendable {
    case goalFocus
    case currentState
    case nextBestMove
    case keyContext
    case openGaps
    case coverageProgress
    case emergingPatterns
    case currentMap
    case comparisonAxes
    case decisionPressure

    var id: Self { self }

    var title: String {
        switch self {
        case .goalFocus:
            "Goal Focus"
        case .currentState:
            "Current State"
        case .nextBestMove:
            "Next Best Move"
        case .keyContext:
            "Key Context"
        case .openGaps:
            "Open Gaps"
        case .coverageProgress:
            "Coverage / Progress"
        case .emergingPatterns:
            "Emerging Patterns"
        case .currentMap:
            "Current Map"
        case .comparisonAxes:
            "Comparison Axes"
        case .decisionPressure:
            "Decision Pressure"
        }
    }

    var systemImage: String {
        switch self {
        case .goalFocus:
            "scope"
        case .currentState:
            "point.topleft.down.curvedto.point.bottomright.up"
        case .nextBestMove:
            "arrow.right.circle"
        case .keyContext:
            "square.text.square"
        case .openGaps:
            "questionmark.circle"
        case .coverageProgress:
            "chart.bar"
        case .emergingPatterns:
            "sparkles"
        case .currentMap:
            "map"
        case .comparisonAxes:
            "slider.horizontal.3"
        case .decisionPressure:
            "exclamationmark.triangle"
        }
    }
}

struct ResumeComponent: Identifiable, Hashable, Codable, Sendable {
    var kind: ResumeComponentKind
    var title: String
    var body: String?
    var items: [String]

    var id: ResumeComponentKind { kind }
}

struct ResumeRecoveryLine: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var title: String
    var body: String
}

struct ResolvedItem: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var text: String
    var statusLabel: String
    var resolvedAt: Date
}

struct ThreadChangeItem: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var text: String
    var changedAt: Date
}

struct ThreadAnchorHighlight: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var title: String
    var body: String
}

struct PreparedView: Identifiable, Hashable {
    var id = UUID()
    var threadID: UUID
    var type: PreparedViewType
    var title: String
    var contentState: AIContentState
    var coreQuestion: String
    var activeClaims: [Claim]
    var keyEvidence: [Entry]
    var openLoops: [String]
    var recommendedNextSteps: [String]
    var recentEntries: [Entry]
}

struct EntryStreamItem: Identifiable, Hashable {
    var id: UUID { entry.id }
    var entry: Entry
    var primaryRelation: DiscourseRelation?
    var relatedEntries: [Entry]
    var replies: [Entry]
}

struct ThreadStreamSection: Identifiable, Hashable {
    var id: UUID
    var startedAt: Date
    var endedAt: Date
    var items: [EntryStreamItem]
}

enum ThreadBlockKind: String, CaseIterable, Hashable, Codable, Sendable {
    case judgment
    case basis
    case gap
    case nextMove
    case evidence
    case sources
    case resolved
    case questions
    case principles
    case risks
    case contrast
    case checklist
}

enum ThreadBlockTone: String, Hashable, Codable, Sendable {
    case neutral
    case accent
    case warning
    case success
    case subdued
}

struct ThreadBlock: Identifiable, Hashable, Codable, Sendable {
    var id = UUID()
    var kind: ThreadBlockKind
    var title: String
    var summary: String?
    var items: [String]
    var tone: ThreadBlockTone
    var provenanceEntryIDs: [UUID]
}

struct ThreadPresentation: Hashable, Codable, Sendable {
    var headline: String
    var blocks: [ThreadBlock]
    var primaryAction: String?

    static let empty = ThreadPresentation(
        headline: "",
        blocks: [],
        primaryAction: nil
    )
}

enum AIContentStatus: String, Hashable, Codable, Sendable {
    case notConfigured
    case loading
    case ready
    case error
}

struct AIContentState: Hashable, Codable, Sendable {
    var status: AIContentStatus
    var message: String
}

enum ThreadAIStatus: String, Hashable, Codable, Sendable {
    case notConfigured
    case pending
    case applied
    case invalidPlan
    case failed
}

struct ThreadAIDebugState: Hashable, Codable, Sendable {
    var status: ThreadAIStatus
    var backendLabel: String
    var configuredModelID: String?
    var responseModelID: String?
    var responseID: String?
    var finishReason: String?
    var warnings: [String]
    var message: String
    var parsedResponse: String?
    var rawResponseBody: String?
    var updatedAt: Date
}

struct ThreadState: Hashable {
    var threadID: UUID
    var coreQuestion: String
    var goalLayer: ThreadGoalLayer
    var contentState: AIContentState
    var presentation: ThreadPresentation
    var aiDebug: ThreadAIDebugState
    var restartNote: String
    var currentJudgment: String
    var judgmentBasis: String
    var openLoops: [String]
    var nextAction: String?
    var recoveryLines: [ResumeRecoveryLine]
    var resolvedSoFar: [ResolvedItem]
    var recentChanges: [ThreadChangeItem]
    var keyAnchors: [ThreadAnchorHighlight]
    var claimCount: Int
    var evidenceCount: Int
    var sourceCount: Int
    var keyQuestions: [Entry]
    var keyClaims: [Claim]
    var supportingEvidence: [Entry]
    var recentSources: [Entry]
    var streamSections: [ThreadStreamSection]
    var lastAnchorID: UUID?
    var lastAnchorAt: Date?
}

struct ThreadCreationContext: Identifiable, Hashable {
    var id = UUID()
    var editingThreadID: UUID?
    var sourceEntryID: UUID?
    var suggestedTitle: String
    var seedGoalStatement: String
    var suggestedGoalType: ThreadGoalType
    var successCondition: String
    var currentStage: ThreadGoalStage
}

struct ThreadSuggestion: Identifiable, Hashable {
    var thread: ThreadRecord
    var score: Int
    var reason: String

    var id: UUID { thread.id }
}

enum RouteDebugStatus: String, Hashable, Codable, Sendable {
    case notConfigured
    case pending
    case routed
    case stayedInInbox
    case failed
    case invalidDecision
}

struct RouteCandidateDebug: Identifiable, Hashable, Codable, Sendable {
    var threadID: UUID
    var threadTitle: String
    var goalStatement: String
    var coreObjects: [String]
    var activeClaims: [String]
    var latestAnchorSummary: String?
    var openLoops: [String]
    var totalScore: Int
    var semanticScore: Int
    var retrievalScore: Int
    var reason: String

    var id: UUID { threadID }
}

struct RoutePlannedSuggestion: Identifiable, Hashable, Codable, Sendable {
    var threadID: UUID
    var threadTitle: String
    var reason: String
    var rank: Int

    var id: UUID { threadID }
}

struct RouteDebugState: Hashable, Codable, Sendable {
    var plannerLabel: String
    var supportEngineLabel: String
    var status: RouteDebugStatus
    var message: String
    var connectivityStatus: String?
    var connectivityMessage: String?
    var connectivityCheckedAt: Date?
    var normalizedText: String
    var detectedItemType: String
    var detectedObjects: [String]
    var candidateClaims: [String]
    var routingQueries: [String]
    var topCandidates: [RouteCandidateDebug]
    var decisionReason: String
    var plannedSuggestions: [RoutePlannedSuggestion]
    var selectedThreadID: UUID?
    var selectedThreadTitle: String?
    var topScore: Int?
    var secondScore: Int?
    var autoRouteThreshold: Int
    var autoRouteGapThreshold: Int
    var backendLabel: String?
    var configuredModelID: String?
    var responseModelID: String?
    var responseID: String?
    var finishReason: String?
    var warnings: [String]
    var parsedResponse: String?
    var rawResponseBody: String?
    var updatedAt: Date
}

struct ThreadSignature: Identifiable, Hashable {
    var thread: ThreadRecord
    var goalStatement: String
    var coreObjects: [ObjectMention]
    var activeClaims: [String]
    var latestAnchorSummary: String?
    var openLoops: [String]
    var lastActiveAt: Date

    var id: UUID { thread.id }
}

struct QuickCaptureDraft: Identifiable {
    let id = UUID()
    var text = ""
}

enum HomeSurface {
    case inbox
    case resources
}

// MARK: - Memory

enum MemoryScope: String, Codable, CaseIterable {
    case working   // ephemeral: entries within a session
    case episodic  // checkpoints: written anchors
    case semantic  // stable: settled claims
    case source    // provenance: source-kind entries

    var label: String {
        switch self {
        case .working:  "Working"
        case .episodic: "Checkpoint"
        case .semantic: "Stable"
        case .source:   "Source"
        }
    }
}

struct MemoryRecord: Identifiable {
    var id: UUID
    var threadID: UUID
    var scope: MemoryScope
    var text: String
    var provenance: String   // "entry:<uuid>" | "anchor:<uuid>" | "claim:<uuid>"
    var createdAt: Date
}

struct AppSnapshot {
    var sampleDataVersion: Int?
    var threads: [ThreadRecord]
    var entries: [Entry]
    var claims: [Claim]
    var anchors: [Anchor]
    var tasks: [ThreadTask]
    var discourseRelations: [DiscourseRelation]
}

extension AppSnapshot: Codable {
    private enum CodingKeys: String, CodingKey {
        case sampleDataVersion, threads, entries, claims, anchors, tasks, discourseRelations
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        sampleDataVersion = try container.decodeIfPresent(Int.self, forKey: .sampleDataVersion)
        threads = try container.decode([ThreadRecord].self, forKey: .threads)
        entries = try container.decode([Entry].self, forKey: .entries)
        claims = try container.decode([Claim].self, forKey: .claims)
        anchors = try container.decode([Anchor].self, forKey: .anchors)
        tasks = try container.decode([ThreadTask].self, forKey: .tasks)
        discourseRelations = try container.decodeIfPresent([DiscourseRelation].self, forKey: .discourseRelations) ?? []
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encodeIfPresent(sampleDataVersion, forKey: .sampleDataVersion)
        try container.encode(threads, forKey: .threads)
        try container.encode(entries, forKey: .entries)
        try container.encode(claims, forKey: .claims)
        try container.encode(anchors, forKey: .anchors)
        try container.encode(tasks, forKey: .tasks)
        try container.encode(discourseRelations, forKey: .discourseRelations)
    }
}
