/// GRDB record types — one per table, mapping to/from app model types.
/// Models.swift stays free of GRDB imports.

import Foundation
import GRDB

// MARK: - Shared helpers

private func encodeDate(_ d: Date) -> String {
    let f = ISO8601DateFormatter()
    f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
    return f.string(from: d)
}

private func decodeDate(_ s: String) -> Date {
    let f = ISO8601DateFormatter()
    f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
    return f.date(from: s) ?? Date()
}

private func encodeJSON<T: Encodable>(_ value: T) -> String {
    let data = (try? JSONEncoder().encode(value)) ?? Data()
    return String(data: data, encoding: .utf8) ?? "{}"
}

private func decodeJSON<T: Decodable>(_ s: String, as type: T.Type) -> T? {
    guard let data = s.data(using: .utf8) else { return nil }
    return try? JSONDecoder().decode(type, from: data)
}

// MARK: - ThreadRow

struct ThreadRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "threads"

    var id: String
    var title: String
    var prompt: String
    var goalLayerJSON: String
    var status: String
    var color: String
    var createdAt: String
    var updatedAt: String
    var lastActiveAt: String

    init(row: Row) {
        id            = row["id"]
        title         = row["title"]
        prompt        = row["prompt"]
        goalLayerJSON = row["goal_layer_json"]
        status        = row["status"]
        color         = row["color"]
        createdAt     = row["created_at"]
        updatedAt     = row["updated_at"]
        lastActiveAt  = row["last_active_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]              = id
        container["title"]           = title
        container["prompt"]          = prompt
        container["goal_layer_json"] = goalLayerJSON
        container["status"]          = status
        container["color"]           = color
        container["created_at"]      = createdAt
        container["updated_at"]      = updatedAt
        container["last_active_at"]  = lastActiveAt
    }

    init(record: ThreadRecord) {
        id            = record.id.uuidString
        title         = record.title
        prompt        = record.prompt
        goalLayerJSON = encodeJSON(record.goalLayer)
        status        = record.status.rawValue
        color         = record.color.rawValue
        createdAt     = encodeDate(record.createdAt)
        updatedAt     = encodeDate(record.updatedAt)
        lastActiveAt  = encodeDate(record.lastActiveAt)
    }

    func toRecord() -> ThreadRecord {
        ThreadRecord(
            id:           UUID(uuidString: id) ?? UUID(),
            title:        title,
            prompt:       prompt,
            goalLayer:    decodeJSON(goalLayerJSON, as: ThreadGoalLayer.self)
                          ?? ThreadGoalLayer.legacyDefault(title: title, prompt: prompt),
            status:       ThreadStatus(rawValue: status) ?? .active,
            createdAt:    decodeDate(createdAt),
            updatedAt:    decodeDate(updatedAt),
            lastActiveAt: decodeDate(lastActiveAt),
            color:        ThreadColor(rawValue: color) ?? .sky
        )
    }
}

// MARK: - EntryRow

struct EntryRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "entries"

    var id: String
    var threadID: String?
    var parentEntryID: String?
    var supersedesEntryID: String?
    var kind: String
    var bodyJSON: String
    var summaryText: String
    var sourceMetadataJSON: String?
    var authorType: String
    var inboxState: String
    var importanceScore: Double?
    var confidenceScore: Double?
    var sessionID: String?
    var createdAt: String

    init(row: Row) {
        id                  = row["id"]
        threadID            = row["thread_id"]
        parentEntryID       = row["parent_entry_id"]
        supersedesEntryID   = row["supersedes_entry_id"]
        kind                = row["kind"]
        bodyJSON            = row["body_json"]
        summaryText         = row["summary_text"]
        sourceMetadataJSON  = row["source_metadata_json"]
        authorType          = row["author_type"]
        inboxState          = row["inbox_state"]
        importanceScore     = row["importance_score"]
        confidenceScore     = row["confidence_score"]
        sessionID           = row["session_id"]
        createdAt           = row["created_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]                   = id
        container["thread_id"]            = threadID
        container["parent_entry_id"]      = parentEntryID
        container["supersedes_entry_id"]  = supersedesEntryID
        container["kind"]                 = kind
        container["body_json"]            = bodyJSON
        container["summary_text"]         = summaryText
        container["source_metadata_json"] = sourceMetadataJSON
        container["author_type"]          = authorType
        container["inbox_state"]          = inboxState
        container["importance_score"]     = importanceScore
        container["confidence_score"]     = confidenceScore
        container["session_id"]           = sessionID
        container["created_at"]           = createdAt
    }

    init(entry: Entry) {
        id                  = entry.id.uuidString
        threadID            = entry.threadID?.uuidString
        parentEntryID       = entry.parentEntryID?.uuidString
        supersedesEntryID   = entry.supersedesEntryID?.uuidString
        kind                = entry.kind.rawValue
        bodyJSON            = encodeJSON(entry.body)
        summaryText         = entry.summaryText
        sourceMetadataJSON  = entry.sourceMetadata.map { encodeJSON($0) }
        authorType          = entry.authorType
        inboxState          = entry.inboxState
        importanceScore     = entry.importanceScore
        confidenceScore     = entry.confidenceScore
        sessionID           = entry.sessionID?.uuidString
        createdAt           = encodeDate(entry.createdAt)
    }

    func toEntry(objectMentions: [ObjectMention] = [], references: [EntryReference] = []) -> Entry {
        Entry(
            id:                 UUID(uuidString: id) ?? UUID(),
            threadID:           threadID.flatMap(UUID.init(uuidString:)),
            kind:               EntryKind(rawValue: kind) ?? .note,
            body:               decodeJSON(bodyJSON, as: EntryBody.self)
                                ?? EntryBody(kind: .text, text: summaryText, url: nil, title: nil, details: nil),
            summaryText:        summaryText,
            sourceMetadata:     sourceMetadataJSON.flatMap { decodeJSON($0, as: SourceMetadata.self) },
            objectMentions:     objectMentions,
            references:         references,
            createdAt:          decodeDate(createdAt),
            sessionID:          sessionID.flatMap(UUID.init(uuidString:)),
            authorType:         authorType,
            parentEntryID:      parentEntryID.flatMap(UUID.init(uuidString:)),
            supersedesEntryID:  supersedesEntryID.flatMap(UUID.init(uuidString:)),
            importanceScore:    importanceScore,
            confidenceScore:    confidenceScore,
            inboxState:         inboxState
        )
    }
}

// MARK: - ObjectMentionRow

struct ObjectMentionRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "object_mentions"

    var id: String
    var entryID: String
    var label: String
    var kind: String

    init(row: Row) {
        id      = row["id"]
        entryID = row["entry_id"]
        label   = row["label"]
        kind    = row["kind"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]       = id
        container["entry_id"] = entryID
        container["label"]    = label
        container["kind"]     = kind
    }

    init(mention: ObjectMention, entryID: UUID) {
        id          = mention.id.uuidString
        self.entryID = entryID.uuidString
        label       = mention.name
        kind        = mention.kind.rawValue
    }

    func toMention() -> ObjectMention {
        ObjectMention(
            id:   UUID(uuidString: id) ?? UUID(),
            name: label,
            kind: ObjectKind(rawValue: kind) ?? .generic
        )
    }
}

// MARK: - EntryReferenceRow

struct EntryReferenceRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "entry_references"

    var id: String
    var entryID: String
    var label: String
    var targetKind: String
    var targetID: String?

    init(row: Row) {
        id         = row["id"]
        entryID    = row["entry_id"]
        label      = row["label"]
        targetKind = row["target_kind"]
        targetID   = row["target_id"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]          = id
        container["entry_id"]    = entryID
        container["label"]       = label
        container["target_kind"] = targetKind
        container["target_id"]   = targetID
    }

    init(reference: EntryReference, entryID: UUID) {
        id          = reference.id.uuidString
        self.entryID = entryID.uuidString
        label       = reference.label
        targetKind  = reference.targetKind.rawValue
        targetID    = reference.targetID?.uuidString
    }

    func toReference() -> EntryReference {
        EntryReference(
            id:         UUID(uuidString: id) ?? UUID(),
            label:      label,
            targetKind: ReferenceTargetKind(rawValue: targetKind) ?? .unresolved,
            targetID:   targetID.flatMap(UUID.init(uuidString:))
        )
    }
}

// MARK: - ClaimRow

struct ClaimRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "claims"

    var id: String
    var threadID: String
    var originEntryID: String
    var statement: String
    var status: String
    var confidenceScore: Double
    var createdAt: String
    var updatedAt: String

    init(row: Row) {
        id              = row["id"]
        threadID        = row["thread_id"]
        originEntryID   = row["origin_entry_id"]
        statement       = row["statement"]
        status          = row["status"]
        confidenceScore = row["confidence_score"]
        createdAt       = row["created_at"]
        updatedAt       = row["updated_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]               = id
        container["thread_id"]        = threadID
        container["origin_entry_id"]  = originEntryID
        container["statement"]        = statement
        container["status"]           = status
        container["confidence_score"] = confidenceScore
        container["created_at"]       = createdAt
        container["updated_at"]       = updatedAt
    }

    init(claim: Claim) {
        id              = claim.id.uuidString
        threadID        = claim.threadID.uuidString
        originEntryID   = claim.originEntryID.uuidString
        statement       = claim.statement
        status          = claim.status.rawValue
        confidenceScore = claim.confidenceScore
        createdAt       = encodeDate(claim.createdAt)
        updatedAt       = encodeDate(claim.updatedAt)
    }

    func toClaim() -> Claim {
        Claim(
            id:              UUID(uuidString: id) ?? UUID(),
            threadID:        UUID(uuidString: threadID) ?? UUID(),
            originEntryID:   UUID(uuidString: originEntryID) ?? UUID(),
            statement:       statement,
            status:          ClaimStatus(rawValue: status) ?? .candidate,
            createdAt:       decodeDate(createdAt),
            updatedAt:       decodeDate(updatedAt),
            confidenceScore: confidenceScore
        )
    }
}

// MARK: - AnchorRow

struct AnchorRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "anchors"

    var id: String
    var threadID: String
    var basedOnEntryID: String?
    var title: String
    var coreQuestion: String
    var stateSummary: String
    var openLoopsJSON: String
    var nextStepsJSON: String
    var claimIDsJSON: String
    var evidenceEntryIDsJSON: String
    var phase: String
    var createdAt: String

    init(row: Row) {
        id                   = row["id"]
        threadID             = row["thread_id"]
        basedOnEntryID       = row["based_on_entry_id"]
        title                = row["title"]
        coreQuestion         = row["core_question"]
        stateSummary         = row["state_summary"]
        openLoopsJSON        = row["open_loops_json"]
        nextStepsJSON        = row["next_steps_json"]
        claimIDsJSON         = row["claim_ids_json"]
        evidenceEntryIDsJSON = row["evidence_entry_ids_json"]
        phase                = row["phase"]
        createdAt            = row["created_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]                     = id
        container["thread_id"]              = threadID
        container["based_on_entry_id"]      = basedOnEntryID
        container["title"]                  = title
        container["core_question"]          = coreQuestion
        container["state_summary"]          = stateSummary
        container["open_loops_json"]        = openLoopsJSON
        container["next_steps_json"]        = nextStepsJSON
        container["claim_ids_json"]         = claimIDsJSON
        container["evidence_entry_ids_json"] = evidenceEntryIDsJSON
        container["phase"]                  = phase
        container["created_at"]             = createdAt
    }

    init(anchor: Anchor) {
        id                   = anchor.id.uuidString
        threadID             = anchor.threadID.uuidString
        basedOnEntryID       = anchor.basedOnEntryID?.uuidString
        title                = anchor.title
        coreQuestion         = anchor.coreQuestion
        stateSummary         = anchor.stateSummary
        openLoopsJSON        = encodeJSON(anchor.openLoops)
        nextStepsJSON        = encodeJSON(anchor.nextSteps)
        claimIDsJSON         = encodeJSON(anchor.claimIDs.map(\.uuidString))
        evidenceEntryIDsJSON = encodeJSON(anchor.evidenceEntryIDs.map(\.uuidString))
        phase                = anchor.phase
        createdAt            = encodeDate(anchor.createdAt)
    }

    func toAnchor() -> Anchor {
        let claimUUIDs = (decodeJSON(claimIDsJSON, as: [String].self) ?? [])
            .compactMap(UUID.init(uuidString:))
        let evidenceUUIDs = (decodeJSON(evidenceEntryIDsJSON, as: [String].self) ?? [])
            .compactMap(UUID.init(uuidString:))
        return Anchor(
            id:                 UUID(uuidString: id) ?? UUID(),
            threadID:           UUID(uuidString: threadID) ?? UUID(),
            createdAt:          decodeDate(createdAt),
            basedOnEntryID:     basedOnEntryID.flatMap(UUID.init(uuidString:)),
            title:              title,
            coreQuestion:       coreQuestion,
            stateSummary:       stateSummary,
            openLoops:          decodeJSON(openLoopsJSON, as: [String].self) ?? [],
            nextSteps:          decodeJSON(nextStepsJSON, as: [String].self) ?? [],
            claimIDs:           claimUUIDs,
            evidenceEntryIDs:   evidenceUUIDs,
            phase:              phase
        )
    }
}

// MARK: - DiscourseRelationRow

struct DiscourseRelationRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "discourse_relations"

    var id: String
    var sourceEntryID: String
    var targetEntryID: String
    var kind: String
    var confidence: Double

    init(row: Row) {
        id            = row["id"]
        sourceEntryID = row["source_entry_id"]
        targetEntryID = row["target_entry_id"]
        kind          = row["kind"]
        confidence    = row["confidence"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]              = id
        container["source_entry_id"] = sourceEntryID
        container["target_entry_id"] = targetEntryID
        container["kind"]            = kind
        container["confidence"]      = confidence
    }

    init(relation: DiscourseRelation) {
        id            = relation.id.uuidString
        sourceEntryID = relation.sourceEntryID.uuidString
        targetEntryID = relation.targetEntryID.uuidString
        kind          = relation.kind.rawValue
        confidence    = relation.confidence
    }

    func toRelation() -> DiscourseRelation {
        DiscourseRelation(
            id:            UUID(uuidString: id) ?? UUID(),
            sourceEntryID: UUID(uuidString: sourceEntryID) ?? UUID(),
            targetEntryID: UUID(uuidString: targetEntryID) ?? UUID(),
            kind:          DiscourseRelationKind(rawValue: kind) ?? .informs,
            confidence:    confidence
        )
    }
}

// MARK: - ThreadTaskRow

struct ThreadTaskRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "tasks"

    var id: String
    var threadID: String
    var originEntryID: String?
    var title: String
    var status: String
    var createdAt: String
    var updatedAt: String

    init(row: Row) {
        id            = row["id"]
        threadID      = row["thread_id"]
        originEntryID = row["origin_entry_id"]
        title         = row["title"]
        status        = row["status"]
        createdAt     = row["created_at"]
        updatedAt     = row["updated_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]              = id
        container["thread_id"]       = threadID
        container["origin_entry_id"] = originEntryID
        container["title"]           = title
        container["status"]          = status
        container["created_at"]      = createdAt
        container["updated_at"]      = updatedAt
    }

    init(task: ThreadTask) {
        id            = task.id.uuidString
        threadID      = task.threadID.uuidString
        originEntryID = task.originEntryID?.uuidString
        title         = task.title
        status        = task.status
        createdAt     = encodeDate(task.createdAt)
        updatedAt     = encodeDate(task.updatedAt)
    }

    func toTask() -> ThreadTask {
        ThreadTask(
            id:            UUID(uuidString: id) ?? UUID(),
            threadID:      UUID(uuidString: threadID) ?? UUID(),
            originEntryID: originEntryID.flatMap(UUID.init(uuidString:)),
            title:         title,
            status:        status,
            createdAt:     decodeDate(createdAt),
            updatedAt:     decodeDate(updatedAt)
        )
    }
}

// MARK: - ThreadAISnapshotRow

struct ThreadAISnapshotRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "thread_ai_snapshots"

    var threadID: UUID
    var contentFingerprint: String
    var headline: String
    var blocksJSON: String
    var restartNote: String
    var currentJudgment: String
    var openLoopsJSON: String
    var nextAction: String?
    var recoveryLinesJSON: String
    var synthesizedAt: Date
    var modelID: String

    init(
        threadID: UUID,
        contentFingerprint: String,
        headline: String,
        blocksJSON: String,
        restartNote: String,
        currentJudgment: String,
        openLoopsJSON: String,
        nextAction: String?,
        recoveryLinesJSON: String,
        synthesizedAt: Date,
        modelID: String
    ) {
        self.threadID = threadID
        self.contentFingerprint = contentFingerprint
        self.headline = headline
        self.blocksJSON = blocksJSON
        self.restartNote = restartNote
        self.currentJudgment = currentJudgment
        self.openLoopsJSON = openLoopsJSON
        self.nextAction = nextAction
        self.recoveryLinesJSON = recoveryLinesJSON
        self.synthesizedAt = synthesizedAt
        self.modelID = modelID
    }

    init(row: Row) {
        threadID = UUID(uuidString: row["thread_id"]) ?? UUID()
        contentFingerprint = row["content_fingerprint"]
        headline = row["headline"]
        blocksJSON = row["blocks_json"]
        restartNote = row["restart_note"]
        currentJudgment = row["current_judgment"]
        openLoopsJSON = row["open_loops_json"]
        nextAction = row["next_action"]
        recoveryLinesJSON = row["recovery_lines_json"]
        synthesizedAt = Date(timeIntervalSince1970: row["synthesized_at"])
        modelID = row["model_id"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["thread_id"] = threadID.uuidString
        container["content_fingerprint"] = contentFingerprint
        container["headline"] = headline
        container["blocks_json"] = blocksJSON
        container["restart_note"] = restartNote
        container["current_judgment"] = currentJudgment
        container["open_loops_json"] = openLoopsJSON
        container["next_action"] = nextAction
        container["recovery_lines_json"] = recoveryLinesJSON
        container["synthesized_at"] = synthesizedAt.timeIntervalSince1970
        container["model_id"] = modelID
    }
}
