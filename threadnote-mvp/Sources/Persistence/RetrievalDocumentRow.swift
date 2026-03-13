/// GRDB record for retrieval_documents + helpers to build rows from app entities.

import Foundation
import GRDB

struct RetrievalDocumentRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "retrieval_documents"

    var id: String
    var ownerType: String
    var ownerID: String
    var threadID: String?
    var title: String
    var body: String
    var metadataJSON: String
    var createdAt: String
    var updatedAt: String

    init(row: Row) {
        id           = row["id"]
        ownerType    = row["owner_type"]
        ownerID      = row["owner_id"]
        threadID     = row["thread_id"]
        title        = row["title"]
        body         = row["body"]
        metadataJSON = row["metadata_json"]
        createdAt    = row["created_at"]
        updatedAt    = row["updated_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]            = id
        container["owner_type"]    = ownerType
        container["owner_id"]      = ownerID
        container["thread_id"]     = threadID
        container["title"]         = title
        container["body"]          = body
        container["metadata_json"] = metadataJSON
        container["created_at"]    = createdAt
        container["updated_at"]    = updatedAt
    }

    // MARK: - Factory helpers

    static func from(entry: Entry) -> RetrievalDocumentRow {
        let now = isoNow()
        let bodyText = entry.body.text ?? ""
        let meta: [String: String] = [
            "kind": entry.kind.rawValue,
            "inbox_state": entry.inboxState
        ]
        return RetrievalDocumentRow(
            id:           "entry:\(entry.id.uuidString)",
            ownerType:    "entry",
            ownerID:      entry.id.uuidString,
            threadID:     entry.threadID?.uuidString,
            title:        entry.summaryText,
            body:         bodyText,
            metadataJSON: encodeMetadata(meta),
            createdAt:    isoDate(entry.createdAt),
            updatedAt:    now
        )
    }

    static func from(claim: Claim) -> RetrievalDocumentRow {
        let now = isoNow()
        let meta: [String: String] = [
            "status": claim.status.rawValue
        ]
        return RetrievalDocumentRow(
            id:           "claim:\(claim.id.uuidString)",
            ownerType:    "claim",
            ownerID:      claim.id.uuidString,
            threadID:     claim.threadID.uuidString,
            title:        claim.statement,
            body:         "",
            metadataJSON: encodeMetadata(meta),
            createdAt:    isoDate(claim.createdAt),
            updatedAt:    now
        )
    }

    static func from(anchor: Anchor) -> RetrievalDocumentRow {
        let now = isoNow()
        let bodyParts = ([anchor.coreQuestion, anchor.stateSummary] + anchor.openLoops)
            .filter { !$0.isEmpty }
            .joined(separator: " ")
        let meta: [String: String] = ["phase": anchor.phase]
        return RetrievalDocumentRow(
            id:           "anchor:\(anchor.id.uuidString)",
            ownerType:    "anchor",
            ownerID:      anchor.id.uuidString,
            threadID:     anchor.threadID.uuidString,
            title:        anchor.title,
            body:         bodyParts,
            metadataJSON: encodeMetadata(meta),
            createdAt:    isoDate(anchor.createdAt),
            updatedAt:    now
        )
    }

    // MARK: - Private

    private static func encodeMetadata(_ dict: [String: String]) -> String {
        let data = (try? JSONEncoder().encode(dict)) ?? Data()
        return String(data: data, encoding: .utf8) ?? "{}"
    }

    private static func isoNow() -> String { isoDate(Date()) }

    private static func isoDate(_ d: Date) -> String {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f.string(from: d)
    }

    // MARK: - Explicit init for internal construction

    init(
        id: String, ownerType: String, ownerID: String,
        threadID: String?, title: String, body: String,
        metadataJSON: String, createdAt: String, updatedAt: String
    ) {
        self.id           = id
        self.ownerType    = ownerType
        self.ownerID      = ownerID
        self.threadID     = threadID
        self.title        = title
        self.body         = body
        self.metadataJSON = metadataJSON
        self.createdAt    = createdAt
        self.updatedAt    = updatedAt
    }
}
