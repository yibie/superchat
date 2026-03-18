export const APP_DATABASE_MIGRATIONS = Object.freeze([
  {
    name: "v1_initial_schema",
    statements: [
      `CREATE TABLE IF NOT EXISTS threads (
        id TEXT PRIMARY KEY,
        title TEXT NOT NULL,
        prompt TEXT NOT NULL DEFAULT '',
        goal_layer_json TEXT NOT NULL DEFAULT '{}',
        status TEXT NOT NULL DEFAULT 'active',
        color TEXT NOT NULL DEFAULT 'sky',
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        last_active_at TEXT NOT NULL
      )`,
      `CREATE TABLE IF NOT EXISTS entries (
        id TEXT PRIMARY KEY,
        thread_id TEXT,
        parent_entry_id TEXT,
        supersedes_entry_id TEXT,
        kind TEXT NOT NULL,
        body_json TEXT NOT NULL DEFAULT '{}',
        summary_text TEXT NOT NULL DEFAULT '',
        source_metadata_json TEXT,
        object_mentions_json TEXT NOT NULL DEFAULT '[]',
        references_json TEXT NOT NULL DEFAULT '[]',
        author_type TEXT NOT NULL DEFAULT 'user',
        inbox_state TEXT NOT NULL DEFAULT 'unresolved',
        importance_score REAL,
        confidence_score REAL,
        session_id TEXT,
        created_at TEXT NOT NULL
      )`,
      `CREATE TABLE IF NOT EXISTS claims (
        id TEXT PRIMARY KEY,
        thread_id TEXT NOT NULL,
        origin_entry_id TEXT NOT NULL,
        statement TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'candidate',
        confidence_score REAL NOT NULL DEFAULT 0.5,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )`,
      `CREATE TABLE IF NOT EXISTS anchors (
        id TEXT PRIMARY KEY,
        thread_id TEXT NOT NULL,
        based_on_entry_id TEXT,
        title TEXT NOT NULL,
        core_question TEXT NOT NULL,
        state_summary TEXT NOT NULL,
        open_loops_json TEXT NOT NULL DEFAULT '[]',
        next_steps_json TEXT NOT NULL DEFAULT '[]',
        claim_ids_json TEXT NOT NULL DEFAULT '[]',
        evidence_entry_ids_json TEXT NOT NULL DEFAULT '[]',
        phase TEXT NOT NULL DEFAULT 'working',
        created_at TEXT NOT NULL
      )`,
      `CREATE TABLE IF NOT EXISTS discourse_relations (
        id TEXT PRIMARY KEY,
        source_entry_id TEXT NOT NULL,
        target_entry_id TEXT NOT NULL,
        kind TEXT NOT NULL,
        confidence REAL NOT NULL DEFAULT 1
      )`,
      `CREATE TABLE IF NOT EXISTS tasks (
        id TEXT PRIMARY KEY,
        thread_id TEXT NOT NULL,
        origin_entry_id TEXT,
        title TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'open',
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )`,
      `CREATE TABLE IF NOT EXISTS app_metadata (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      )`
    ]
  },
  {
    name: "v2_retrieval_layer",
    statements: [
      `CREATE TABLE IF NOT EXISTS retrieval_documents (
        id TEXT PRIMARY KEY,
        owner_type TEXT NOT NULL,
        owner_id TEXT NOT NULL,
        thread_id TEXT,
        title TEXT NOT NULL DEFAULT '',
        body TEXT NOT NULL DEFAULT '',
        metadata_json TEXT NOT NULL DEFAULT '{}',
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )`,
      "CREATE INDEX IF NOT EXISTS idx_retrieval_documents_owner_id ON retrieval_documents(owner_id)",
      "CREATE INDEX IF NOT EXISTS idx_retrieval_documents_thread_id ON retrieval_documents(thread_id)"
    ]
  },
  {
    name: "v3_memory_layer",
    statements: [
      `CREATE TABLE IF NOT EXISTS memory_records (
        id TEXT PRIMARY KEY,
        thread_id TEXT NOT NULL,
        scope TEXT NOT NULL,
        text TEXT NOT NULL,
        provenance TEXT NOT NULL,
        created_at TEXT NOT NULL
      )`,
      "CREATE INDEX IF NOT EXISTS idx_memory_records_thread_scope ON memory_records(thread_id, scope)"
    ]
  },
  {
    name: "v4_thread_ai_snapshots",
    statements: [
      `CREATE TABLE IF NOT EXISTS thread_ai_snapshots (
        thread_id TEXT PRIMARY KEY,
        content_fingerprint TEXT NOT NULL,
        headline TEXT NOT NULL DEFAULT '',
        blocks_json TEXT NOT NULL DEFAULT '[]',
        restart_note TEXT NOT NULL DEFAULT '',
        current_judgment TEXT NOT NULL DEFAULT '',
        open_loops_json TEXT NOT NULL DEFAULT '[]',
        next_action TEXT,
        recovery_lines_json TEXT NOT NULL DEFAULT '[]',
        synthesized_at REAL NOT NULL,
        model_id TEXT NOT NULL DEFAULT ''
      )`
    ]
  },
  {
    name: "v5_entry_status",
    statements: [
      "ALTER TABLE entries ADD COLUMN status TEXT NOT NULL DEFAULT 'open'",
      "ALTER TABLE entries ADD COLUMN status_metadata_json TEXT"
    ]
  },
  {
    name: "v6_entry_paging_indexes",
    statements: [
      "CREATE INDEX IF NOT EXISTS idx_entries_created_id ON entries(created_at DESC, id DESC)",
      "CREATE INDEX IF NOT EXISTS idx_entries_thread_created_id ON entries(thread_id, created_at DESC, id DESC)",
      "CREATE INDEX IF NOT EXISTS idx_entries_parent_created_id ON entries(parent_entry_id, created_at DESC, id DESC)"
    ]
  },
  {
    name: "v7_thread_materialized_versions",
    statements: [
      `CREATE TABLE IF NOT EXISTS thread_materialized_versions (
        thread_id TEXT PRIMARY KEY,
        memory_version INTEGER NOT NULL DEFAULT 0,
        retrieval_version INTEGER NOT NULL DEFAULT 0,
        updated_at TEXT NOT NULL
      )`,
      "CREATE INDEX IF NOT EXISTS idx_thread_materialized_versions_updated_at ON thread_materialized_versions(updated_at DESC)"
    ]
  },
  {
    name: "v8_thread_aggregates",
    statements: [
      `CREATE TABLE IF NOT EXISTS thread_aggregates (
        thread_id TEXT PRIMARY KEY,
        entry_count INTEGER NOT NULL DEFAULT 0,
        evidence_count INTEGER NOT NULL DEFAULT 0,
        source_count INTEGER NOT NULL DEFAULT 0,
        stable_claim_count INTEGER NOT NULL DEFAULT 0,
        stable_claim_updated_at TEXT,
        anchor_count INTEGER NOT NULL DEFAULT 0,
        latest_anchor_created_at TEXT,
        decided_count INTEGER NOT NULL DEFAULT 0,
        solved_count INTEGER NOT NULL DEFAULT 0,
        verified_count INTEGER NOT NULL DEFAULT 0,
        dropped_count INTEGER NOT NULL DEFAULT 0,
        updated_at TEXT NOT NULL
      )`,
      "CREATE INDEX IF NOT EXISTS idx_thread_aggregates_updated_at ON thread_aggregates(updated_at DESC)"
    ]
  }
]);

export function applyMigrations(database) {
  database.exec("PRAGMA journal_mode = WAL;");
  database.exec("PRAGMA foreign_keys = OFF;");
  database.exec("CREATE TABLE IF NOT EXISTS schema_migrations (name TEXT PRIMARY KEY, applied_at TEXT NOT NULL)");
  const existing = new Set(
    database.prepare("SELECT name FROM schema_migrations").all().map((row) => row.name)
  );

  for (const migration of APP_DATABASE_MIGRATIONS) {
    if (existing.has(migration.name)) {
      continue;
    }
    database.exec("BEGIN");
    try {
      for (const statement of migration.statements) {
        try {
          database.exec(statement);
        } catch (error) {
          const message = String(error?.message ?? "");
          if (/duplicate column name/i.test(message)) {
            continue;
          }
          throw error;
        }
      }
      database
        .prepare("INSERT INTO schema_migrations (name, applied_at) VALUES (?, ?)")
        .run(migration.name, new Date().toISOString());
      database.exec("COMMIT");
    } catch (error) {
      database.exec("ROLLBACK");
      throw error;
    }
  }
}
