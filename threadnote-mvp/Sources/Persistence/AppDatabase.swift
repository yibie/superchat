/// task003 — DatabaseMigrator v1 schema.
/// Creates all tables for the M1 persistence layer.

import Foundation
import GRDB

enum AppDatabase {

    static func makeDatabasePool(at url: URL) throws -> DatabasePool {
        let pool = try DatabasePool(path: url.path)
        try migrate(pool)
        return pool
    }

    // MARK: - Migrations

    static func migrate(_ writer: any DatabaseWriter) throws {
        var migrator = DatabaseMigrator()

        // Fail fast in development; remove for release builds.
        #if DEBUG
        migrator.eraseDatabaseOnSchemaChange = false
        #endif

        migrator.registerMigration("v1_initial_schema") { db in
            try db.execute(sql: """
                PRAGMA foreign_keys = ON;
                PRAGMA journal_mode = WAL;
            """)

            // threads
            try db.create(table: "threads", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("title", .text).notNull()
                t.column("prompt", .text).notNull().defaults(to: "")
                t.column("goal_layer_json", .text).notNull().defaults(to: "{}")
                t.column("status", .text).notNull().defaults(to: "active")
                t.column("color", .text).notNull().defaults(to: "sky")
                t.column("created_at", .text).notNull()
                t.column("updated_at", .text).notNull()
                t.column("last_active_at", .text).notNull()
            }

            // entries
            try db.create(table: "entries", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("thread_id", .text).references("threads", onDelete: .setNull)
                t.column("parent_entry_id", .text).references("entries", onDelete: .setNull)
                t.column("supersedes_entry_id", .text)
                t.column("kind", .text).notNull()
                t.column("body_json", .text).notNull().defaults(to: "{}")
                t.column("summary_text", .text).notNull().defaults(to: "")
                t.column("source_metadata_json", .text)
                t.column("author_type", .text).notNull().defaults(to: "user")
                t.column("inbox_state", .text).notNull().defaults(to: "unresolved")
                t.column("importance_score", .double)
                t.column("confidence_score", .double)
                t.column("session_id", .text)
                t.column("created_at", .text).notNull()
            }

            // object_mentions
            try db.create(table: "object_mentions", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("entry_id", .text).notNull().references("entries", onDelete: .cascade)
                t.column("label", .text).notNull()
                t.column("kind", .text).notNull()
            }

            // entry_references
            try db.create(table: "entry_references", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("entry_id", .text).notNull().references("entries", onDelete: .cascade)
                t.column("label", .text).notNull()
                t.column("target_kind", .text).notNull()
                t.column("target_id", .text)
            }

            // claims
            try db.create(table: "claims", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("thread_id", .text).notNull().references("threads", onDelete: .cascade)
                t.column("origin_entry_id", .text).notNull()
                t.column("statement", .text).notNull()
                t.column("status", .text).notNull().defaults(to: "candidate")
                t.column("confidence_score", .double).notNull().defaults(to: 0.5)
                t.column("created_at", .text).notNull()
                t.column("updated_at", .text).notNull()
            }

            // anchors
            try db.create(table: "anchors", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("thread_id", .text).notNull().references("threads", onDelete: .cascade)
                t.column("based_on_entry_id", .text)
                t.column("title", .text).notNull()
                t.column("core_question", .text).notNull()
                t.column("state_summary", .text).notNull()
                t.column("open_loops_json", .text).notNull().defaults(to: "[]")
                t.column("next_steps_json", .text).notNull().defaults(to: "[]")
                t.column("claim_ids_json", .text).notNull().defaults(to: "[]")
                t.column("evidence_entry_ids_json", .text).notNull().defaults(to: "[]")
                t.column("phase", .text).notNull()
                t.column("created_at", .text).notNull()
            }

            // discourse_relations
            try db.create(table: "discourse_relations", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("source_entry_id", .text).notNull().references("entries", onDelete: .cascade)
                t.column("target_entry_id", .text).notNull().references("entries", onDelete: .cascade)
                t.column("kind", .text).notNull()
                t.column("confidence", .double).notNull().defaults(to: 1.0)
            }

            // tasks
            try db.create(table: "tasks", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("thread_id", .text).notNull().references("threads", onDelete: .cascade)
                t.column("origin_entry_id", .text)
                t.column("title", .text).notNull()
                t.column("status", .text).notNull().defaults(to: "open")
                t.column("created_at", .text).notNull()
                t.column("updated_at", .text).notNull()
            }

            // app_metadata — migration version tracking
            try db.create(table: "app_metadata", ifNotExists: true) { t in
                t.primaryKey("key", .text)
                t.column("value", .text).notNull()
            }
        }

        migrator.registerMigration("v2_retrieval_layer") { db in
            // retrieval_documents — normalized recall units
            try db.create(table: "retrieval_documents", ifNotExists: true) { t in
                t.primaryKey("id", .text)
                t.column("owner_type", .text).notNull()   // entry/claim/anchor/resource
                t.column("owner_id", .text).notNull()
                t.column("thread_id", .text)
                t.column("title", .text).notNull().defaults(to: "")
                t.column("body", .text).notNull().defaults(to: "")
                t.column("metadata_json", .text).notNull().defaults(to: "{}")
                t.column("created_at", .text).notNull()
                t.column("updated_at", .text).notNull()
            }
            try db.create(indexOn: "retrieval_documents", columns: ["owner_id"])
            try db.create(indexOn: "retrieval_documents", columns: ["thread_id"])

            // retrieval_fts — FTS5 virtual table (content table mode)
            try db.execute(sql: """
                CREATE VIRTUAL TABLE IF NOT EXISTS retrieval_fts USING fts5(
                    title,
                    body,
                    content='retrieval_documents',
                    content_rowid='rowid',
                    tokenize='unicode61'
                )
            """)

            // Triggers to keep FTS index in sync with retrieval_documents
            try db.execute(sql: """
                CREATE TRIGGER IF NOT EXISTS retrieval_fts_ai
                AFTER INSERT ON retrieval_documents BEGIN
                    INSERT INTO retrieval_fts(rowid, title, body)
                    VALUES (new.rowid, new.title, new.body);
                END
            """)
            try db.execute(sql: """
                CREATE TRIGGER IF NOT EXISTS retrieval_fts_au
                AFTER UPDATE ON retrieval_documents BEGIN
                    INSERT INTO retrieval_fts(retrieval_fts, rowid, title, body)
                    VALUES ('delete', old.rowid, old.title, old.body);
                    INSERT INTO retrieval_fts(rowid, title, body)
                    VALUES (new.rowid, new.title, new.body);
                END
            """)
            try db.execute(sql: """
                CREATE TRIGGER IF NOT EXISTS retrieval_fts_ad
                AFTER DELETE ON retrieval_documents BEGIN
                    INSERT INTO retrieval_fts(retrieval_fts, rowid, title, body)
                    VALUES ('delete', old.rowid, old.title, old.body);
                END
            """)
        }

        try migrator.migrate(writer)
    }
}
