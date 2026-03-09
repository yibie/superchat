import Foundation

enum EntryKind: String, Codable, CaseIterable, Identifiable {
    case capture
    case note
    case question
    case claimProposed
    case decision
    case taskAdded
    case handoff
    case anchorWritten

    var id: Self { self }
}

enum ThreadStatus: String, Codable, CaseIterable, Identifiable {
    case active
    case paused
    case resolved

    var id: Self { self }
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

enum AppRoute: String, Identifiable {
    case home
    case thread

    var id: Self { self }

    var title: String {
        switch self {
        case .home:
            "Home"
        case .thread:
            "Thread"
        }
    }
}

enum ListKind: String, Codable, CaseIterable, Identifiable {
    case pack
    case review
    case queue

    var id: Self { self }

    var title: String {
        switch self {
        case .pack:
            "Pack"
        case .review:
            "Review"
        case .queue:
            "Queue"
        }
    }
}

enum ThreadLinkRole: String, Codable, CaseIterable, Identifiable {
    case core
    case supporting
    case reference

    var id: Self { self }

    var title: String {
        switch self {
        case .core:
            "Core"
        case .supporting:
            "Supporting"
        case .reference:
            "Reference"
        }
    }
}

struct EntryThreadLink: Codable, Hashable, Identifiable {
    var threadID: UUID
    var role: ThreadLinkRole

    var id: UUID { threadID }
}

struct Entry: Identifiable, Hashable {
    var id: UUID
    var threadID: UUID?
    var threadLinks: [EntryThreadLink]
    var kind: EntryKind
    var content: String
    var createdAt: Date
    var sessionID: UUID?
    var authorType: String
    var parentEntryID: UUID?
    var supersedesEntryID: UUID?
    var importanceScore: Double?
    var confidenceScore: Double?
    var inboxState: String

    var primaryThreadID: UUID? {
        threadID ?? threadLinks.first?.threadID
    }

    var threadIDs: [UUID] {
        if let threadID { return [threadID] }
        return threadLinks.map(\.threadID)
    }
}

extension Entry: Codable {
    private enum CodingKeys: String, CodingKey {
        case id
        case threadID
        case threadLinks
        case threadIDs
        case kind
        case content
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

        // Decode legacy threadLinks
        if let links = try container.decodeIfPresent([EntryThreadLink].self, forKey: .threadLinks) {
            threadLinks = links
        } else if let ids = try container.decodeIfPresent([UUID].self, forKey: .threadIDs) {
            threadLinks = ids.enumerated().map { index, tid in
                EntryThreadLink(threadID: tid, role: index == 0 ? .core : .supporting)
            }
        } else {
            threadLinks = []
        }

        // Migrate threadID: prefer explicit field, else derive from threadLinks
        if let tid = try container.decodeIfPresent(UUID.self, forKey: .threadID) {
            threadID = tid
        } else if let coreLink = threadLinks.first(where: { $0.role == .core }) {
            threadID = coreLink.threadID
        } else {
            threadID = threadLinks.first?.threadID
        }

        kind = try container.decode(EntryKind.self, forKey: .kind)
        content = try container.decode(String.self, forKey: .content)
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
        try container.encode(threadLinks, forKey: .threadLinks)
        try container.encode(kind, forKey: .kind)
        try container.encode(content, forKey: .content)
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

struct ThreadTask: Identifiable, Codable, Hashable {
    var id: UUID
    var threadID: UUID
    var originEntryID: UUID?
    var title: String
    var status: String
    var createdAt: Date
    var updatedAt: Date
}

struct ListRecord: Identifiable, Codable, Hashable {
    var id: UUID
    var title: String
    var kind: ListKind
    var note: String
    var createdAt: Date
    var updatedAt: Date
}

enum ListItemEntityType: String, Codable, CaseIterable, Identifiable {
    case entry
    case thread

    var id: Self { self }
}

struct ListItem: Identifiable, Codable, Hashable {
    var id: UUID
    var listID: UUID
    var entityType: ListItemEntityType
    var entityID: UUID
    var position: Int
    var note: String?
}

struct ThreadRecord: Identifiable, Hashable {
    var id: UUID
    var title: String
    var prompt: String
    var status: ThreadStatus
    var createdAt: Date
    var updatedAt: Date
    var lastActiveAt: Date
    var currentAnchorID: UUID?
    var summary: String
    var nextStep: String
}

extension ThreadRecord: Codable {
    private enum CodingKeys: String, CodingKey {
        case id, title, prompt, status, createdAt, updatedAt, lastActiveAt
        case currentAnchorID, summary, nextStep
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        id = try container.decode(UUID.self, forKey: .id)
        title = try container.decode(String.self, forKey: .title)
        prompt = try container.decode(String.self, forKey: .prompt)
        status = try container.decode(ThreadStatus.self, forKey: .status)
        createdAt = try container.decode(Date.self, forKey: .createdAt)
        updatedAt = try container.decode(Date.self, forKey: .updatedAt)
        lastActiveAt = try container.decode(Date.self, forKey: .lastActiveAt)
        currentAnchorID = try container.decodeIfPresent(UUID.self, forKey: .currentAnchorID)
        summary = try container.decodeIfPresent(String.self, forKey: .summary) ?? ""
        nextStep = try container.decodeIfPresent(String.self, forKey: .nextStep) ?? ""
    }
}

struct PreparedView: Identifiable, Hashable {
    var id = UUID()
    var threadID: UUID
    var type: PreparedViewType
    var title: String
    var coreQuestion: String
    var activeClaims: [Claim]
    var keyEvidence: [Entry]
    var openLoops: [String]
    var recommendedNextSteps: [String]
    var recentEntries: [Entry]
}

struct ThreadState: Hashable {
    var threadID: UUID
    var coreQuestion: String
    var currentJudgment: String
    var openLoops: [String]
    var nextAction: String?
    var keyClaims: [Claim]
    var recentSessions: [ThreadSession]
    var relatedEntries: [Entry]
    var lastAnchorID: UUID?
    var lastAnchorAt: Date?
}

struct ThreadSession: Identifiable, Hashable {
    var id: UUID
    var startedAt: Date
    var endedAt: Date
    var entries: [Entry]
}

struct ThreadSuggestion: Identifiable, Hashable {
    var thread: ThreadRecord
    var score: Int
    var reason: String

    var id: UUID { thread.id }
}

struct QuickCaptureDraft: Identifiable {
    let id = UUID()
    var text = ""
    var selectedThreadID: UUID?
}

struct AppSnapshot {
    var sampleDataVersion: Int?
    var threads: [ThreadRecord]
    var entries: [Entry]
    var claims: [Claim]
    var anchors: [Anchor]
    var tasks: [ThreadTask]
    var lists: [ListRecord]
    var listItems: [ListItem]
}

extension AppSnapshot: Codable {
    private enum CodingKeys: String, CodingKey {
        case sampleDataVersion
        case threads
        case entries
        case claims
        case anchors
        case tasks
        case lists
        case listItems
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        sampleDataVersion = try container.decodeIfPresent(Int.self, forKey: .sampleDataVersion)
        threads = try container.decode([ThreadRecord].self, forKey: .threads)
        entries = try container.decode([Entry].self, forKey: .entries)
        claims = try container.decode([Claim].self, forKey: .claims)
        anchors = try container.decode([Anchor].self, forKey: .anchors)
        tasks = try container.decode([ThreadTask].self, forKey: .tasks)
        lists = try container.decodeIfPresent([ListRecord].self, forKey: .lists) ?? []
        listItems = try container.decodeIfPresent([ListItem].self, forKey: .listItems) ?? []
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encodeIfPresent(sampleDataVersion, forKey: .sampleDataVersion)
        try container.encode(threads, forKey: .threads)
        try container.encode(entries, forKey: .entries)
        try container.encode(claims, forKey: .claims)
        try container.encode(anchors, forKey: .anchors)
        try container.encode(tasks, forKey: .tasks)
        try container.encode(lists, forKey: .lists)
        try container.encode(listItems, forKey: .listItems)
    }
}
