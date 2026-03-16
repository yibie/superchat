import fs from "node:fs";
import path from "node:path";
import { DatabaseSync } from "node:sqlite";
import {
  createAnchor,
  createClaim,
  createDiscourseRelation,
  createEntry,
  createMemoryRecord,
  createThreadAISnapshot,
  createThreadRecord,
  createThreadTask
} from "../../../domain/models/threadnoteModels.js";
import { applyMigrations } from "../schema/appDatabase.js";

export class SQLitePersistenceStore {
  constructor(databasePath) {
    const resolvedPath = path.resolve(databasePath);
    fs.mkdirSync(path.dirname(resolvedPath), { recursive: true });
    this.db = new DatabaseSync(resolvedPath);
    applyMigrations(this.db);
    this.ftsAvailable = this.#setupFTS();
  }

  fetchSnapshot() {
    return {
      threads: this.fetchThreads(),
      entries: this.fetchEntries(),
      claims: this.fetchClaims(),
      anchors: this.fetchAnchors(),
      discourseRelations: this.fetchDiscourseRelations(),
      tasks: this.fetchTasks(),
      memoryRecords: this.fetchMemoryRecords(),
      aiSnapshots: this.fetchAISnapshots()
    };
  }

  fetchThreads() {
    return this.db.prepare("SELECT * FROM threads").all().map(mapThreadRow);
  }

  fetchEntries(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM entries WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM entries ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapEntryRow);
  }

  fetchClaims(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM claims WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM claims ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapClaimRow);
  }

  fetchAnchors(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM anchors WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM anchors ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapAnchorRow);
  }

  fetchDiscourseRelations(threadID = null) {
    const rows = threadID
      ? this.db
          .prepare(
            `SELECT relation.*
             FROM discourse_relations relation
             JOIN entries source ON source.id = relation.source_entry_id
             WHERE source.thread_id = ?
             ORDER BY relation.rowid ASC`
          )
          .all(threadID)
      : this.db.prepare("SELECT * FROM discourse_relations ORDER BY rowid ASC").all();
    return rows.map(mapDiscourseRelationRow);
  }

  fetchTasks(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM tasks WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM tasks ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapTaskRow);
  }

  fetchMemoryRecords(threadID = null, scope = null) {
    const conditions = [];
    const params = [];
    if (threadID) {
      conditions.push("thread_id = ?");
      params.push(threadID);
    }
    if (scope) {
      conditions.push("scope = ?");
      params.push(scope);
    }
    const where = conditions.length > 0 ? ` WHERE ${conditions.join(" AND ")}` : "";
    return this.db
      .prepare(`SELECT * FROM memory_records${where} ORDER BY created_at ASC`)
      .all(...params)
      .map(mapMemoryRecordRow);
  }

  fetchAISnapshots() {
    return this.db.prepare("SELECT * FROM thread_ai_snapshots").all().map(mapAISnapshotRow);
  }

  fetchAISnapshot(threadID) {
    const row = this.db.prepare("SELECT * FROM thread_ai_snapshots WHERE thread_id = ?").get(threadID);
    return row ? mapAISnapshotRow(row) : null;
  }

  upsertThread(thread) {
    this.db
      .prepare(
        `INSERT INTO threads (id, title, prompt, goal_layer_json, status, color, created_at, updated_at, last_active_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           title = excluded.title,
           prompt = excluded.prompt,
           goal_layer_json = excluded.goal_layer_json,
           status = excluded.status,
           color = excluded.color,
           created_at = excluded.created_at,
           updated_at = excluded.updated_at,
           last_active_at = excluded.last_active_at`
      )
      .run(
        thread.id,
        thread.title,
        thread.prompt,
        encodeJSON(thread.goalLayer),
        thread.status,
        thread.color,
        toISO(thread.createdAt),
        toISO(thread.updatedAt),
        toISO(thread.lastActiveAt)
      );
  }

  upsertEntry(entry) {
    this.db
      .prepare(
        `INSERT INTO entries (
           id, thread_id, parent_entry_id, supersedes_entry_id, kind, body_json, summary_text,
           source_metadata_json, object_mentions_json, references_json, author_type, inbox_state,
           importance_score, confidence_score, session_id, created_at
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           thread_id = excluded.thread_id,
           parent_entry_id = excluded.parent_entry_id,
           supersedes_entry_id = excluded.supersedes_entry_id,
           kind = excluded.kind,
           body_json = excluded.body_json,
           summary_text = excluded.summary_text,
           source_metadata_json = excluded.source_metadata_json,
           object_mentions_json = excluded.object_mentions_json,
           references_json = excluded.references_json,
           author_type = excluded.author_type,
           inbox_state = excluded.inbox_state,
           importance_score = excluded.importance_score,
           confidence_score = excluded.confidence_score,
           session_id = excluded.session_id,
           created_at = excluded.created_at`
      )
      .run(
        entry.id,
        entry.threadID,
        entry.parentEntryID ?? null,
        entry.supersedesEntryID ?? null,
        entry.kind,
        encodeJSON(entry.body ?? {}),
        entry.summaryText ?? "",
        encodeJSON(entry.sourceMetadata ?? null),
        encodeJSON(entry.objectMentions ?? []),
        encodeJSON(entry.references ?? []),
        entry.authorType ?? "user",
        entry.inboxState ?? "unresolved",
        entry.importanceScore ?? null,
        entry.confidenceScore ?? null,
        entry.sessionID ?? null,
        toISO(entry.createdAt)
      );
  }

  deleteEntries(entryIDs) {
    const ids = Array.from(new Set((entryIDs ?? []).filter(Boolean)));
    if (ids.length === 0) {
      return;
    }
    const placeholders = ids.map(() => "?").join(", ");
    this.db.prepare(`DELETE FROM discourse_relations WHERE source_entry_id IN (${placeholders}) OR target_entry_id IN (${placeholders})`).run(...ids, ...ids);
    this.db.prepare(`DELETE FROM claims WHERE origin_entry_id IN (${placeholders})`).run(...ids);
    const provenanceIDs = ids.map((id) => `entry:${id}`);
    const provenancePlaceholders = provenanceIDs.map(() => "?").join(", ");
    this.db.prepare(`DELETE FROM memory_records WHERE provenance IN (${provenancePlaceholders})`).run(...provenanceIDs);
    this.db.prepare(`DELETE FROM entries WHERE id IN (${placeholders})`).run(...ids);
  }

  upsertClaim(claim) {
    this.db
      .prepare(
        `INSERT INTO claims (id, thread_id, origin_entry_id, statement, status, confidence_score, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           thread_id = excluded.thread_id,
           origin_entry_id = excluded.origin_entry_id,
           statement = excluded.statement,
           status = excluded.status,
           confidence_score = excluded.confidence_score,
           created_at = excluded.created_at,
           updated_at = excluded.updated_at`
      )
      .run(
        claim.id,
        claim.threadID,
        claim.originEntryID,
        claim.statement,
        claim.status,
        claim.confidenceScore,
        toISO(claim.createdAt),
        toISO(claim.updatedAt)
      );
  }

  upsertAnchor(anchor) {
    this.db
      .prepare(
        `INSERT INTO anchors (
           id, thread_id, based_on_entry_id, title, core_question, state_summary,
           open_loops_json, next_steps_json, claim_ids_json, evidence_entry_ids_json, phase, created_at
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           thread_id = excluded.thread_id,
           based_on_entry_id = excluded.based_on_entry_id,
           title = excluded.title,
           core_question = excluded.core_question,
           state_summary = excluded.state_summary,
           open_loops_json = excluded.open_loops_json,
           next_steps_json = excluded.next_steps_json,
           claim_ids_json = excluded.claim_ids_json,
           evidence_entry_ids_json = excluded.evidence_entry_ids_json,
           phase = excluded.phase,
           created_at = excluded.created_at`
      )
      .run(
        anchor.id,
        anchor.threadID,
        anchor.basedOnEntryID ?? null,
        anchor.title,
        anchor.coreQuestion,
        anchor.stateSummary,
        encodeJSON(anchor.openLoops ?? []),
        encodeJSON(anchor.nextSteps ?? []),
        encodeJSON(anchor.claimIDs ?? []),
        encodeJSON(anchor.evidenceEntryIDs ?? []),
        anchor.phase ?? "working",
        toISO(anchor.createdAt)
      );
  }

  upsertDiscourseRelation(relation) {
    this.db
      .prepare(
        `INSERT INTO discourse_relations (id, source_entry_id, target_entry_id, kind, confidence)
         VALUES (?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           source_entry_id = excluded.source_entry_id,
           target_entry_id = excluded.target_entry_id,
           kind = excluded.kind,
           confidence = excluded.confidence`
      )
      .run(relation.id, relation.sourceEntryID, relation.targetEntryID, relation.kind, relation.confidence ?? 1);
  }

  replaceDiscourseRelations(removingEntryIDs, relations) {
    const ids = [...(removingEntryIDs ?? [])];
    this.#transaction(() => {
      if (ids.length > 0) {
        const placeholders = ids.map(() => "?").join(", ");
        this.db
          .prepare(
            `DELETE FROM discourse_relations
             WHERE source_entry_id IN (${placeholders}) OR target_entry_id IN (${placeholders})`
          )
          .run(...ids, ...ids);
      }
      for (const relation of relations ?? []) {
        this.upsertDiscourseRelation(relation);
      }
    });
  }

  upsertTask(task) {
    this.db
      .prepare(
        `INSERT INTO tasks (id, thread_id, origin_entry_id, title, status, created_at, updated_at)
         VALUES (?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           thread_id = excluded.thread_id,
           origin_entry_id = excluded.origin_entry_id,
           title = excluded.title,
           status = excluded.status,
           created_at = excluded.created_at,
           updated_at = excluded.updated_at`
      )
      .run(task.id, task.threadID, task.originEntryID ?? null, task.title, task.status, toISO(task.createdAt), toISO(task.updatedAt));
  }

  insertMemoryRecord(record) {
    const existing = this.db
      .prepare("SELECT id FROM memory_records WHERE provenance = ? AND scope = ? LIMIT 1")
      .get(record.provenance, record.scope);
    if (existing) {
      return;
    }
    this.db
      .prepare("INSERT INTO memory_records (id, thread_id, scope, text, provenance, created_at) VALUES (?, ?, ?, ?, ?, ?)")
      .run(record.id, record.threadID, record.scope, record.text, record.provenance, toISO(record.createdAt));
  }

  upsertAISnapshot(snapshot) {
    this.db
      .prepare(
        `INSERT INTO thread_ai_snapshots (
           thread_id, content_fingerprint, headline, blocks_json, restart_note, current_judgment,
           open_loops_json, next_action, recovery_lines_json, synthesized_at, model_id
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(thread_id) DO UPDATE SET
           content_fingerprint = excluded.content_fingerprint,
           headline = excluded.headline,
           blocks_json = excluded.blocks_json,
           restart_note = excluded.restart_note,
           current_judgment = excluded.current_judgment,
           open_loops_json = excluded.open_loops_json,
           next_action = excluded.next_action,
           recovery_lines_json = excluded.recovery_lines_json,
           synthesized_at = excluded.synthesized_at,
           model_id = excluded.model_id`
      )
      .run(
        snapshot.threadID,
        snapshot.contentFingerprint,
        snapshot.headline ?? "",
        encodeJSON(snapshot.blocks ?? []),
        snapshot.restartNote ?? "",
        snapshot.currentJudgment ?? "",
        encodeJSON(snapshot.openLoops ?? []),
        snapshot.nextAction ?? null,
        encodeJSON(snapshot.recoveryLines ?? []),
        snapshot.synthesizedAt,
        snapshot.modelID ?? ""
      );
  }

  replaceRetrievalDocuments(threadID, documents) {
    this.#transaction(() => {
      this.db.prepare("DELETE FROM retrieval_documents WHERE thread_id = ?").run(threadID);
      if (this.ftsAvailable) {
        this.db.prepare("DELETE FROM retrieval_fts WHERE thread_id = ?").run(threadID);
      }
      for (const document of documents ?? []) {
        this.db
          .prepare(
            `INSERT INTO retrieval_documents (
               id, owner_type, owner_id, thread_id, title, body, metadata_json, created_at, updated_at
             ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)`
          )
          .run(
            document.id,
            document.ownerType,
            document.ownerID,
            document.threadID ?? null,
            document.title ?? "",
            document.body ?? "",
            encodeJSON(document.metadata ?? {}),
            document.createdAt,
            document.updatedAt
          );
        if (this.ftsAvailable) {
          this.db
            .prepare(
              "INSERT INTO retrieval_fts (id, owner_type, thread_id, title, body) VALUES (?, ?, ?, ?, ?)"
            )
            .run(document.id, document.ownerType, document.threadID ?? null, document.title ?? "", document.body ?? "");
        }
      }
    });
  }

  recallRetrievalDocuments(query, { threadID = null, ownerTypes = [], limit = 30 } = {}) {
    if (!String(query ?? "").trim()) {
      return this.db
        .prepare(buildRecencyQuery({ threadID, ownerTypes, limit }))
        .all(...buildRecencyParams({ threadID, ownerTypes }))
        .map(mapRetrievalRow);
    }
    if (this.ftsAvailable) {
      const params = [];
      let sql = `
        SELECT rd.*, bm25(retrieval_fts) AS score
        FROM retrieval_fts
        JOIN retrieval_documents rd ON rd.id = retrieval_fts.id
        WHERE retrieval_fts MATCH ?`;
      params.push(toFTSQuery(query));
      if (threadID) {
        sql += " AND rd.thread_id = ?";
        params.push(threadID);
      }
      if (ownerTypes.length > 0) {
        sql += ` AND rd.owner_type IN (${ownerTypes.map(() => "?").join(", ")})`;
        params.push(...ownerTypes);
      }
      sql += " ORDER BY score ASC LIMIT ?";
      params.push(limit);
      return this.db.prepare(sql).all(...params).map((row) => mapRetrievalRow(row, true));
    }
    return this.db
      .prepare(buildLikeQuery({ threadID, ownerTypes, limit }))
      .all(...buildLikeParams(query, { threadID, ownerTypes, limit }))
      .map(mapRetrievalRow);
  }

  #setupFTS() {
    try {
      this.db.exec(
        `CREATE VIRTUAL TABLE IF NOT EXISTS retrieval_fts USING fts5(
          id UNINDEXED,
          owner_type UNINDEXED,
          thread_id UNINDEXED,
          title,
          body,
          tokenize='unicode61'
        )`
      );
      return true;
    } catch {
      return false;
    }
  }

  #transaction(callback) {
    this.db.exec("BEGIN");
    try {
      const result = callback();
      this.db.exec("COMMIT");
      return result;
    } catch (error) {
      this.db.exec("ROLLBACK");
      throw error;
    }
  }
}

function mapThreadRow(row) {
  return createThreadRecord({
    id: row.id,
    title: row.title,
    prompt: row.prompt,
    goalLayer: decodeJSON(row.goal_layer_json, {}),
    status: row.status,
    color: row.color,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    lastActiveAt: row.last_active_at
  });
}

function mapEntryRow(row) {
  return createEntry({
    id: row.id,
    threadID: row.thread_id,
    kind: row.kind,
    body: decodeJSON(row.body_json, {}),
    summaryText: row.summary_text,
    sourceMetadata: decodeJSON(row.source_metadata_json, null),
    objectMentions: decodeJSON(row.object_mentions_json, []),
    references: decodeJSON(row.references_json, []),
    createdAt: row.created_at,
    sessionID: row.session_id,
    authorType: row.author_type,
    parentEntryID: row.parent_entry_id,
    supersedesEntryID: row.supersedes_entry_id,
    importanceScore: row.importance_score,
    confidenceScore: row.confidence_score,
    inboxState: row.inbox_state
  });
}

function mapClaimRow(row) {
  return createClaim({
    id: row.id,
    threadID: row.thread_id,
    originEntryID: row.origin_entry_id,
    statement: row.statement,
    status: row.status,
    confidenceScore: row.confidence_score,
    createdAt: row.created_at,
    updatedAt: row.updated_at
  });
}

function mapAnchorRow(row) {
  return createAnchor({
    id: row.id,
    threadID: row.thread_id,
    basedOnEntryID: row.based_on_entry_id,
    title: row.title,
    coreQuestion: row.core_question,
    stateSummary: row.state_summary,
    openLoops: decodeJSON(row.open_loops_json, []),
    nextSteps: decodeJSON(row.next_steps_json, []),
    claimIDs: decodeJSON(row.claim_ids_json, []),
    evidenceEntryIDs: decodeJSON(row.evidence_entry_ids_json, []),
    phase: row.phase,
    createdAt: row.created_at
  });
}

function mapDiscourseRelationRow(row) {
  return createDiscourseRelation({
    id: row.id,
    sourceEntryID: row.source_entry_id,
    targetEntryID: row.target_entry_id,
    kind: row.kind,
    confidence: row.confidence
  });
}

function mapTaskRow(row) {
  return createThreadTask({
    id: row.id,
    threadID: row.thread_id,
    originEntryID: row.origin_entry_id,
    title: row.title,
    status: row.status,
    createdAt: row.created_at,
    updatedAt: row.updated_at
  });
}

function mapMemoryRecordRow(row) {
  return createMemoryRecord({
    id: row.id,
    threadID: row.thread_id,
    scope: row.scope,
    text: row.text,
    provenance: row.provenance,
    createdAt: row.created_at
  });
}

function mapAISnapshotRow(row) {
  return createThreadAISnapshot({
    threadID: row.thread_id,
    contentFingerprint: row.content_fingerprint,
    headline: row.headline,
    blocks: decodeJSON(row.blocks_json, []),
    restartNote: row.restart_note,
    currentJudgment: row.current_judgment,
    openLoops: decodeJSON(row.open_loops_json, []),
    nextAction: row.next_action,
    recoveryLines: decodeJSON(row.recovery_lines_json, []),
    synthesizedAt: row.synthesized_at,
    modelID: row.model_id
  });
}

function mapRetrievalRow(row, fromFTS = false) {
  return {
    id: row.id,
    ownerType: row.owner_type,
    ownerID: row.owner_id,
    threadID: row.thread_id,
    title: row.title,
    body: row.body,
    metadataJSON: row.metadata_json,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    score: fromFTS ? Math.max(1, Math.round(100 - Math.abs(row.score ?? 0) * 10)) : 1
  };
}

function encodeJSON(value) {
  return JSON.stringify(value ?? null);
}

function decodeJSON(value, fallback) {
  if (value == null) {
    return fallback;
  }
  try {
    return JSON.parse(value);
  } catch {
    return fallback;
  }
}

function toISO(value) {
  return value instanceof Date ? value.toISOString() : new Date(value).toISOString();
}

function toFTSQuery(query) {
  return String(query ?? "")
    .trim()
    .split(/\s+/)
    .filter(Boolean)
    .map((token) => `${token.replace(/"/g, "")}*`)
    .join(" ");
}

function buildRecencyQuery({ threadID, ownerTypes, limit }) {
  const conditions = [];
  if (threadID) {
    conditions.push("thread_id = ?");
  }
  if (ownerTypes.length > 0) {
    conditions.push(`owner_type IN (${ownerTypes.map(() => "?").join(", ")})`);
  }
  const where = conditions.length > 0 ? ` WHERE ${conditions.join(" AND ")}` : "";
  return `SELECT * FROM retrieval_documents${where} ORDER BY created_at DESC LIMIT ${limit}`;
}

function buildRecencyParams({ threadID, ownerTypes }) {
  return [...(threadID ? [threadID] : []), ...ownerTypes];
}

function buildLikeQuery({ threadID, ownerTypes, limit }) {
  const conditions = ["(title LIKE ? OR body LIKE ?)"];
  if (threadID) {
    conditions.push("thread_id = ?");
  }
  if (ownerTypes.length > 0) {
    conditions.push(`owner_type IN (${ownerTypes.map(() => "?").join(", ")})`);
  }
  return `SELECT * FROM retrieval_documents WHERE ${conditions.join(" AND ")} ORDER BY created_at DESC LIMIT ${limit}`;
}

function buildLikeParams(query, { threadID, ownerTypes }) {
  const like = `%${String(query ?? "").trim()}%`;
  return [like, like, ...(threadID ? [threadID] : []), ...ownerTypes];
}
