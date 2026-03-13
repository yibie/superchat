/// task004 — PersistenceStore.
/// DatabasePool-backed store; single writer, concurrent readers (WAL mode).
/// This is the only layer that talks to SQLite.

import Foundation
import GRDB

@MainActor
final class PersistenceStore {

    private let pool: DatabasePool

    init(databaseURL: URL) throws {
        pool = try AppDatabase.makeDatabasePool(at: databaseURL)
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
