/// task004 — PersistenceStore.
/// DatabasePool-backed store; single writer, concurrent readers (WAL mode).
/// This is the only layer that talks to SQLite.

import Foundation
import GRDB

final class PersistenceStore: @unchecked Sendable {

    private let pool: DatabasePool

    init(databaseURL: URL) throws {
        pool = try AppDatabase.makeDatabasePool(at: databaseURL)
        try JSONMigration.runIfNeeded(into: self)
    }

    // MARK: - Threads

    func fetchAllThreads() throws -> [ThreadRecord] {
        try pool.read { db in
            try ThreadRow.fetchAll(db).map { $0.toRecord() }
        }
    }

    func upsertThread(_ thread: ThreadRecord) throws {
        try pool.write { db in
            try ThreadRow(record: thread).save(db)
        }
    }

    func deleteThread(id: UUID) throws {
        try pool.write { db in
            _ = try ThreadRow.deleteOne(db, key: id.uuidString)
        }
    }

    // MARK: - Entries

    func fetchAllEntries() throws -> [Entry] {
        try pool.read { db in
            let rows = try EntryRow.fetchAll(db)
            let mentions = try ObjectMentionRow.fetchAll(db)
            let refs = try EntryReferenceRow.fetchAll(db)

            let mentionsByEntry = Dictionary(grouping: mentions, by: \.entryID)
            let refsByEntry = Dictionary(grouping: refs, by: \.entryID)

            return rows.map { row in
                let ms = (mentionsByEntry[row.id] ?? []).map { $0.toMention() }
                let rs = (refsByEntry[row.id] ?? []).map { $0.toReference() }
                return row.toEntry(objectMentions: ms, references: rs)
            }
        }
    }

    func fetchEntries(for threadID: UUID) throws -> [Entry] {
        try pool.read { db in
            let rows = try EntryRow
                .filter(Column("thread_id") == threadID.uuidString)
                .fetchAll(db)
            let entryIDs = rows.map(\.id)
            let mentions = try ObjectMentionRow
                .filter(entryIDs.contains(Column("entry_id")))
                .fetchAll(db)
            let refs = try EntryReferenceRow
                .filter(entryIDs.contains(Column("entry_id")))
                .fetchAll(db)

            let mentionsByEntry = Dictionary(grouping: mentions, by: \.entryID)
            let refsByEntry = Dictionary(grouping: refs, by: \.entryID)

            return rows.map { row in
                let ms = (mentionsByEntry[row.id] ?? []).map { $0.toMention() }
                let rs = (refsByEntry[row.id] ?? []).map { $0.toReference() }
                return row.toEntry(objectMentions: ms, references: rs)
            }
        }
    }

    func upsertEntry(_ entry: Entry) throws {
        try pool.write { db in
            try EntryRow(entry: entry).save(db)
            // Replace associated child rows
            try ObjectMentionRow
                .filter(Column("entry_id") == entry.id.uuidString)
                .deleteAll(db)
            for mention in entry.objectMentions {
                try ObjectMentionRow(mention: mention, entryID: entry.id).insert(db)
            }
            try EntryReferenceRow
                .filter(Column("entry_id") == entry.id.uuidString)
                .deleteAll(db)
            for ref in entry.references {
                try EntryReferenceRow(reference: ref, entryID: entry.id).insert(db)
            }
            try RetrievalDocumentRow.from(entry: entry).save(db)
        }
    }

    func deleteEntry(id: UUID) throws {
        try pool.write { db in
            _ = try EntryRow.deleteOne(db, key: id.uuidString)
        }
    }

    // MARK: - Claims

    func fetchAllClaims() throws -> [Claim] {
        try pool.read { db in
            try ClaimRow.fetchAll(db).map { $0.toClaim() }
        }
    }

    func upsertClaim(_ claim: Claim) throws {
        try pool.write { db in
            try ClaimRow(claim: claim).save(db)
            try RetrievalDocumentRow.from(claim: claim).save(db)
        }
    }

    func deleteClaim(id: UUID) throws {
        try pool.write { db in
            _ = try ClaimRow.deleteOne(db, key: id.uuidString)
        }
    }

    // MARK: - Anchors

    func fetchAllAnchors() throws -> [Anchor] {
        try pool.read { db in
            try AnchorRow.fetchAll(db).map { $0.toAnchor() }
        }
    }

    func upsertAnchor(_ anchor: Anchor) throws {
        try pool.write { db in
            try AnchorRow(anchor: anchor).save(db)
            try RetrievalDocumentRow.from(anchor: anchor).save(db)
        }
    }

    // MARK: - Discourse Relations

    func fetchAllDiscourseRelations() throws -> [DiscourseRelation] {
        try pool.read { db in
            try DiscourseRelationRow.fetchAll(db).map { $0.toRelation() }
        }
    }

    func upsertDiscourseRelation(_ relation: DiscourseRelation) throws {
        try pool.write { db in
            try DiscourseRelationRow(relation: relation).save(db)
        }
    }

    func replaceDiscourseRelations(
        removingRelationsTouching entryIDs: Set<UUID>,
        with relations: [DiscourseRelation]
    ) throws {
        try pool.write { db in
            if !entryIDs.isEmpty {
                let ids = entryIDs.map(\.uuidString)
                try DiscourseRelationRow
                    .filter(ids.contains(Column("source_entry_id")) || ids.contains(Column("target_entry_id")))
                    .deleteAll(db)
            }

            for relation in relations {
                try DiscourseRelationRow(relation: relation).save(db)
            }
        }
    }

    // MARK: - Tasks

    func fetchAllTasks() throws -> [ThreadTask] {
        try pool.read { db in
            try ThreadTaskRow.fetchAll(db).map { $0.toTask() }
        }
    }

    func upsertTask(_ task: ThreadTask) throws {
        try pool.write { db in
            try ThreadTaskRow(task: task).save(db)
        }
    }

    // MARK: - Bulk load (used by migration + initial load)

    func loadSnapshot() throws -> AppSnapshot {
        AppSnapshot(
            sampleDataVersion: nil,
            threads:            try fetchAllThreads(),
            entries:            try fetchAllEntries(),
            claims:             try fetchAllClaims(),
            anchors:            try fetchAllAnchors(),
            tasks:              try fetchAllTasks(),
            discourseRelations: try fetchAllDiscourseRelations()
        )
    }

    func saveSnapshot(_ snapshot: AppSnapshot) throws {
        try pool.write { db in
            try RetrievalDocumentRow.deleteAll(db)
            try DiscourseRelationRow.deleteAll(db)
            try ThreadTaskRow.deleteAll(db)
            try AnchorRow.deleteAll(db)
            try ClaimRow.deleteAll(db)
            try EntryReferenceRow.deleteAll(db)
            try ObjectMentionRow.deleteAll(db)
            try EntryRow.deleteAll(db)
            try ThreadRow.deleteAll(db)

            // Threads
            for thread in snapshot.threads {
                try ThreadRow(record: thread).save(db)
            }
            // Entries + children
            for entry in snapshot.entries {
                try EntryRow(entry: entry).save(db)
                try ObjectMentionRow.filter(Column("entry_id") == entry.id.uuidString).deleteAll(db)
                for mention in entry.objectMentions {
                    try ObjectMentionRow(mention: mention, entryID: entry.id).insert(db)
                }
                try EntryReferenceRow.filter(Column("entry_id") == entry.id.uuidString).deleteAll(db)
                for ref in entry.references {
                    try EntryReferenceRow(reference: ref, entryID: entry.id).insert(db)
                }
            }
            // Claims
            for claim in snapshot.claims {
                try ClaimRow(claim: claim).save(db)
            }
            // Anchors
            for anchor in snapshot.anchors {
                try AnchorRow(anchor: anchor).save(db)
            }
            // Tasks
            for task in snapshot.tasks {
                try ThreadTaskRow(task: task).save(db)
            }
            // Discourse relations
            for relation in snapshot.discourseRelations {
                try DiscourseRelationRow(relation: relation).save(db)
            }
            // Retrieval documents — rebuild from all entities
            for entry in snapshot.entries {
                try RetrievalDocumentRow.from(entry: entry).save(db)
            }
            for claim in snapshot.claims {
                try RetrievalDocumentRow.from(claim: claim).save(db)
            }
            for anchor in snapshot.anchors {
                try RetrievalDocumentRow.from(anchor: anchor).save(db)
            }
        }
    }

    // MARK: - Retrieval document sync (fine-grained)

    func syncRetrievalDocument(for entry: Entry) throws {
        try pool.write { db in
            try RetrievalDocumentRow.from(entry: entry).save(db)
        }
    }

    func syncRetrievalDocument(for claim: Claim) throws {
        try pool.write { db in
            try RetrievalDocumentRow.from(claim: claim).save(db)
        }
    }

    func syncRetrievalDocument(for anchor: Anchor) throws {
        try pool.write { db in
            try RetrievalDocumentRow.from(anchor: anchor).save(db)
        }
    }

    /// Exposed for RetrievalEngine.
    var databasePool: DatabasePool { pool }

    // MARK: - Memory Records

    func fetchMemoryRecords(for threadID: UUID, scope: MemoryScope? = nil) throws -> [MemoryRecord] {
        try pool.read { db in
            var query = MemoryRecordRow
                .filter(Column("thread_id") == threadID.uuidString)
            if let scope {
                query = query.filter(Column("scope") == scope.rawValue)
            }
            return try query
                .order(Column("created_at").desc)
                .fetchAll(db)
                .compactMap { $0.toMemoryRecord() }
        }
    }

    func insertMemoryRecord(_ record: MemoryRecord) throws {
        try pool.write { db in
            try MemoryRecordRow(record: record).insert(db)
        }
    }

    func deleteMemoryRecords(for threadID: UUID, scope: MemoryScope) throws {
        _ = try pool.write { db in
            try MemoryRecordRow
                .filter(Column("thread_id") == threadID.uuidString)
                .filter(Column("scope") == scope.rawValue)
                .deleteAll(db)
        }
    }

    // MARK: - Metadata

    func metadataValue(for key: String) throws -> String? {
        try pool.read { db in
            try String.fetchOne(db, sql: "SELECT value FROM app_metadata WHERE key = ?", arguments: [key])
        }
    }

    func setMetadata(_ value: String, for key: String) throws {
        try pool.write { db in
            try db.execute(
                sql: "INSERT OR REPLACE INTO app_metadata (key, value) VALUES (?, ?)",
                arguments: [key, value]
            )
        }
    }
}
