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
        database.exec(statement);
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
