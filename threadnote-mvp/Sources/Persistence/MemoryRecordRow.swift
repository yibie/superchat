/// GRDB record for memory_records table.

import Foundation
import GRDB

struct MemoryRecordRow: FetchableRecord, PersistableRecord {
    static let databaseTableName = "memory_records"

    var id: String
    var threadID: String
    var scope: String
    var text: String
    var provenance: String
    var createdAt: String

    init(row: Row) {
        id         = row["id"]
        threadID   = row["thread_id"]
        scope      = row["scope"]
        text       = row["text"]
        provenance = row["provenance"]
        createdAt  = row["created_at"]
    }

    func encode(to container: inout PersistenceContainer) {
        container["id"]         = id
        container["thread_id"]  = threadID
        container["scope"]      = scope
        container["text"]       = text
        container["provenance"] = provenance
        container["created_at"] = createdAt
    }

    init(record: MemoryRecord) {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        id         = record.id.uuidString
        threadID   = record.threadID.uuidString
        scope      = record.scope.rawValue
        text       = record.text
        provenance = record.provenance
        createdAt  = f.string(from: record.createdAt)
    }

    func toMemoryRecord() -> MemoryRecord? {
        guard
            let uuid = UUID(uuidString: id),
            let tid = UUID(uuidString: threadID),
            let sc = MemoryScope(rawValue: scope)
        else { return nil }
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        let date = f.date(from: createdAt) ?? Date()
        return MemoryRecord(
            id: uuid,
            threadID: tid,
            scope: sc,
            text: text,
            provenance: provenance,
            createdAt: date
        )
    }
}
