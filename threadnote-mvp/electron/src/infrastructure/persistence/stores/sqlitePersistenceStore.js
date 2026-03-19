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

  fetchWorkspaceSnapshot() {
    return {
      threads: this.fetchThreads(),
      entries: this.fetchRecentEntries({ limit: 200 }),
      claims: [],
      anchors: [],
      discourseRelations: [],
      tasks: [],
      memoryRecords: [],
      aiSnapshots: this.fetchAISnapshots()
    };
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

  hasLegacyRepairCandidates() {
    const row = this.db
      .prepare(
        `SELECT EXISTS(
          SELECT 1
          FROM entries
          WHERE parent_entry_id IS NOT NULL
             OR (
               instr(summary_text, '[[') > 0
               AND instr(summary_text, '|') > 0
               AND instr(summary_text, ']]') > 0
             )
          LIMIT 1
        ) AS present`
      )
      .get();
    return Boolean(row?.present);
  }

  fetchLegacyRepairCandidates() {
    return this.db
      .prepare(
        `WITH legacy_candidates AS (
           SELECT id, parent_entry_id
           FROM entries
           WHERE parent_entry_id IS NOT NULL
              OR (
                instr(summary_text, '[[') > 0
                AND instr(summary_text, '|') > 0
                AND instr(summary_text, ']]') > 0
              )
         )
         SELECT DISTINCT entry.*
         FROM entries entry
         LEFT JOIN legacy_candidates candidate ON candidate.id = entry.id
         WHERE candidate.id IS NOT NULL
            OR entry.id IN (
              SELECT parent_entry_id
              FROM legacy_candidates
              WHERE parent_entry_id IS NOT NULL
            )
         ORDER BY entry.created_at ASC, entry.rowid ASC`
      )
      .all()
      .map(mapEntryRow);
  }

  fetchInboxEntries() {
    return this.db
      .prepare("SELECT * FROM entries WHERE thread_id IS NULL ORDER BY created_at ASC, rowid ASC")
      .all()
      .map(mapEntryRow);
  }

  fetchThreads() {
    return this.db.prepare("SELECT * FROM threads").all().map(mapThreadRow);
  }

  fetchThread(threadID) {
    const row = this.db.prepare("SELECT * FROM threads WHERE id = ?").get(threadID);
    return row ? mapThreadRow(row) : null;
  }

  fetchEntry(entryID) {
    const row = this.db.prepare("SELECT * FROM entries WHERE id = ?").get(entryID);
    return row ? mapEntryRow(row) : null;
  }

  fetchEntryIDs() {
    return this.db.prepare("SELECT id FROM entries ORDER BY rowid ASC").all().map((row) => row.id).filter(Boolean);
  }

  fetchEntries(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM entries WHERE thread_id = ? ORDER BY created_at ASC, rowid ASC")
      : this.db.prepare("SELECT * FROM entries ORDER BY created_at ASC, rowid ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapEntryRow);
  }

  fetchEntriesByIDs(entryIDs = []) {
    const ids = Array.from(new Set((entryIDs ?? []).filter(Boolean)));
    if (ids.length === 0) {
      return [];
    }
    const placeholders = ids.map(() => "?").join(", ");
    return this.db
      .prepare(`SELECT * FROM entries WHERE id IN (${placeholders}) ORDER BY created_at ASC, rowid ASC`)
      .all(...ids)
      .map(mapEntryRow);
  }

  fetchEntryPage({ threadID = null, cursor = null, limit = 50, topLevelOnly = true } = {}) {
    const { sql, params } = buildEntryPageQuery({ threadID, cursor, limit, topLevelOnly });
    const rows = this.db.prepare(sql).all(...params);
    return rows.map(mapEntryRow);
  }

  fetchRecentEntries({ threadID = null, limit = 200 } = {}) {
    return this.fetchEntryPage({
      threadID,
      limit,
      topLevelOnly: false
    }).slice().reverse();
  }

  countEntries({ threadID = null, topLevelOnly = false } = {}) {
    const conditions = [];
    const params = [];
    if (threadID) {
      conditions.push("thread_id = ?");
      params.push(threadID);
    }
    const where = conditions.length > 0 ? ` WHERE ${conditions.join(" AND ")}` : "";
    const row = this.db.prepare(`SELECT COUNT(*) AS count FROM entries${where}`).get(...params);
    return Number(row?.count ?? 0);
  }

  fetchThreadEntryCounts() {
    const rows = this.db
      .prepare(
        `SELECT thread_id, COUNT(*) AS count
         FROM entries
         WHERE thread_id IS NOT NULL
         GROUP BY thread_id`
      )
      .all();
    return new Map(rows.map((row) => [row.thread_id, Number(row.count ?? 0)]));
  }

  fetchClaims(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM claims WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM claims ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapClaimRow);
  }

  fetchClaim(claimID) {
    const row = this.db.prepare("SELECT * FROM claims WHERE id = ?").get(claimID);
    return row ? mapClaimRow(row) : null;
  }

  fetchClaimsByOriginEntryIDs(entryIDs = []) {
    const ids = Array.from(new Set((entryIDs ?? []).filter(Boolean)));
    if (ids.length === 0) {
      return [];
    }
    const placeholders = ids.map(() => "?").join(", ");
    return this.db
      .prepare(`SELECT * FROM claims WHERE origin_entry_id IN (${placeholders}) ORDER BY created_at ASC`)
      .all(...ids)
      .map(mapClaimRow);
  }

  fetchAnchors(threadID = null) {
    const statement = threadID
      ? this.db.prepare("SELECT * FROM anchors WHERE thread_id = ? ORDER BY created_at ASC")
      : this.db.prepare("SELECT * FROM anchors ORDER BY created_at ASC");
    return statement.all(...(threadID ? [threadID] : [])).map(mapAnchorRow);
  }

  fetchAnchor(anchorID) {
    const row = this.db.prepare("SELECT * FROM anchors WHERE id = ?").get(anchorID);
    return row ? mapAnchorRow(row) : null;
  }

  fetchLatestAnchor(threadID) {
    const row = this.db
      .prepare(
        `SELECT * FROM anchors
         WHERE thread_id = ?
         ORDER BY created_at DESC, rowid DESC
         LIMIT 1`
      )
      .get(threadID);
    return row ? mapAnchorRow(row) : null;
  }

  fetchSettledClaims(threadID, limit = null) {
    const params = [threadID];
    let sql = `
      SELECT * FROM claims
      WHERE thread_id = ?
        AND status = 'stable'
      ORDER BY updated_at DESC, created_at DESC`;
    if (Number.isFinite(Number(limit)) && Number(limit) > 0) {
      sql += " LIMIT ?";
      params.push(Number(limit));
    }
    return this.db.prepare(sql).all(...params).map(mapClaimRow);
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

  fetchThreadAggregate(threadID) {
    if (!threadID) {
      return createEmptyThreadAggregate(threadID);
    }
    const row = this.db.prepare("SELECT * FROM thread_aggregates WHERE thread_id = ?").get(threadID);
    return row ? mapThreadAggregateRow(row) : createEmptyThreadAggregate(threadID);
  }

  fetchThreadCounts(threadID) {
    const aggregate = this.fetchThreadAggregate(threadID);
    const claimCounts = this.db.prepare("SELECT COUNT(*) AS claim_count FROM claims WHERE thread_id = ?").get(threadID);
    const taskCounts = this.db.prepare("SELECT COUNT(*) AS task_count FROM tasks WHERE thread_id = ?").get(threadID);
    const discourseCounts = this.db
      .prepare(
        `SELECT COUNT(*) AS discourse_count
         FROM discourse_relations relation
         JOIN entries source ON source.id = relation.source_entry_id
         WHERE source.thread_id = ?`
      )
      .get(threadID);
    return {
      entryCount: aggregate.entryCount,
      evidenceCount: aggregate.evidenceCount,
      sourceCount: aggregate.sourceCount,
      claimCount: Number(claimCounts?.claim_count ?? 0),
      stableClaimCount: aggregate.stableClaimCount,
      anchorCount: aggregate.anchorCount,
      taskCount: Number(taskCounts?.task_count ?? 0),
      discourseCount: Number(discourseCounts?.discourse_count ?? 0),
      decidedCount: aggregate.decidedCount,
      solvedCount: aggregate.solvedCount,
      verifiedCount: aggregate.verifiedCount,
      droppedCount: aggregate.droppedCount
    };
  }

  fetchThreadStatusSummary(threadID) {
    const rows = this.db
      .prepare(
        `SELECT
           id,
           thread_id,
           kind,
           status,
           summary_text,
           source_metadata_json,
           status_metadata_json,
           created_at
         FROM entries
         WHERE thread_id = ?
           AND status IN ('decided', 'solved', 'verified', 'dropped')
         ORDER BY COALESCE(json_extract(status_metadata_json, '$.updatedAt'), created_at) DESC, rowid DESC`
      )
      .all(threadID);
    return summarizeStatusSummaryRows(rows);
  }

  fetchThreadFingerprintBasis(threadID) {
    const row = this.db
      .prepare(
        `SELECT
           t.id AS thread_id,
           t.updated_at AS thread_updated_at,
           t.last_active_at AS thread_last_active_at,
           ta.stable_claim_count AS stable_claim_count,
           ta.stable_claim_updated_at AS stable_claim_updated_at,
           ta.anchor_count AS anchor_count,
           ta.latest_anchor_created_at AS latest_anchor_created_at,
           ta.evidence_count AS evidence_count,
           ta.source_count AS source_count,
           ta.decided_count AS decided_count,
           ta.solved_count AS solved_count,
           ta.verified_count AS verified_count,
           ta.dropped_count AS dropped_count,
           ta.updated_at AS aggregate_updated_at,
           tmv.memory_version AS memory_version,
           tmv.retrieval_version AS retrieval_version,
           tmv.updated_at AS materialized_updated_at
         FROM threads t
         LEFT JOIN thread_aggregates ta ON ta.thread_id = t.id
         LEFT JOIN thread_materialized_versions tmv ON tmv.thread_id = t.id
         WHERE t.id = ?`
      )
      .get(threadID);
    if (!row) {
      return null;
    }
    return {
      threadID: row.thread_id,
      threadUpdatedAt: row.thread_updated_at,
      threadLastActiveAt: row.thread_last_active_at,
      stableClaimCount: Number(row.stable_claim_count ?? 0),
      stableClaimUpdatedAt: row.stable_claim_updated_at ?? null,
      anchorCount: Number(row.anchor_count ?? 0),
      latestAnchorCreatedAt: row.latest_anchor_created_at ?? null,
      evidenceCount: Number(row.evidence_count ?? 0),
      sourceCount: Number(row.source_count ?? 0),
      decidedCount: Number(row.decided_count ?? 0),
      solvedCount: Number(row.solved_count ?? 0),
      verifiedCount: Number(row.verified_count ?? 0),
      droppedCount: Number(row.dropped_count ?? 0),
      aggregateUpdatedAt: row.aggregate_updated_at ?? null,
      retrievalCount: 0,
      retrievalUpdatedAt: null,
      memoryCount: 0,
      memoryUpdatedAt: null,
      memoryVersion: Number(row.memory_version ?? 0),
      retrievalVersion: Number(row.retrieval_version ?? 0),
      materializedUpdatedAt: row.materialized_updated_at ?? null
    };
  }

  upsertThreadAggregate(threadID, aggregate = {}) {
    const next = {
      ...createEmptyThreadAggregate(threadID),
      ...aggregate,
      threadID,
      updatedAt: aggregate.updatedAt ?? new Date().toISOString()
    };
    this.db
      .prepare(
        `INSERT INTO thread_aggregates (
           thread_id, entry_count, evidence_count, source_count,
           stable_claim_count, stable_claim_updated_at, anchor_count, latest_anchor_created_at,
           decided_count, solved_count, verified_count, dropped_count, updated_at
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(thread_id) DO UPDATE SET
           entry_count = excluded.entry_count,
           evidence_count = excluded.evidence_count,
           source_count = excluded.source_count,
           stable_claim_count = excluded.stable_claim_count,
           stable_claim_updated_at = excluded.stable_claim_updated_at,
           anchor_count = excluded.anchor_count,
           latest_anchor_created_at = excluded.latest_anchor_created_at,
           decided_count = excluded.decided_count,
           solved_count = excluded.solved_count,
           verified_count = excluded.verified_count,
           dropped_count = excluded.dropped_count,
           updated_at = excluded.updated_at`
      )
      .run(
        next.threadID,
        next.entryCount,
        next.evidenceCount,
        next.sourceCount,
        next.stableClaimCount,
        next.stableClaimUpdatedAt ? toISO(next.stableClaimUpdatedAt) : null,
        next.anchorCount,
        next.latestAnchorCreatedAt ? toISO(next.latestAnchorCreatedAt) : null,
        next.decidedCount,
        next.solvedCount,
        next.verifiedCount,
        next.droppedCount,
        toISO(next.updatedAt)
      );
    return next;
  }

  patchThreadAggregate(threadID, patch = {}) {
    if (!threadID) {
      return createEmptyThreadAggregate(threadID);
    }
    const current = this.fetchThreadAggregate(threadID);
    const next = {
      ...current,
      threadID,
      entryCount: clampCount(current.entryCount + Number(patch.entryCountDelta ?? 0)),
      evidenceCount: clampCount(current.evidenceCount + Number(patch.evidenceCountDelta ?? 0)),
      sourceCount: clampCount(current.sourceCount + Number(patch.sourceCountDelta ?? 0)),
      stableClaimCount: clampCount(current.stableClaimCount + Number(patch.stableClaimCountDelta ?? 0)),
      anchorCount: clampCount(current.anchorCount + Number(patch.anchorCountDelta ?? 0)),
      decidedCount: clampCount(current.decidedCount + Number(patch.decidedCountDelta ?? 0)),
      solvedCount: clampCount(current.solvedCount + Number(patch.solvedCountDelta ?? 0)),
      verifiedCount: clampCount(current.verifiedCount + Number(patch.verifiedCountDelta ?? 0)),
      droppedCount: clampCount(current.droppedCount + Number(patch.droppedCountDelta ?? 0)),
      stableClaimUpdatedAt: maxTimestamp(
        current.stableClaimUpdatedAt,
        patch.stableClaimUpdatedAtMax ?? null
      ),
      latestAnchorCreatedAt: maxTimestamp(
        current.latestAnchorCreatedAt,
        patch.latestAnchorCreatedAtMax ?? null
      ),
      updatedAt: new Date().toISOString()
    };
    return this.upsertThreadAggregate(threadID, next);
  }

  rebuildThreadAggregate(threadID) {
    if (!threadID) {
      return createEmptyThreadAggregate(threadID);
    }
    const thread = this.fetchThread(threadID);
    if (!thread) {
      this.db.prepare("DELETE FROM thread_aggregates WHERE thread_id = ?").run(threadID);
      return createEmptyThreadAggregate(threadID);
    }

    const entryCounts = this.db
      .prepare(
        `SELECT
           COUNT(*) AS entry_count,
           SUM(CASE WHEN kind = 'evidence' THEN 1 ELSE 0 END) AS evidence_count,
           SUM(CASE WHEN kind IN ('source', 'evidence') THEN 1 ELSE 0 END) AS source_count,
           SUM(CASE WHEN status = 'decided' THEN 1 ELSE 0 END) AS decided_count,
           SUM(CASE WHEN status = 'solved' THEN 1 ELSE 0 END) AS solved_count,
           SUM(CASE WHEN status = 'verified' THEN 1 ELSE 0 END) AS verified_count,
           SUM(CASE WHEN status = 'dropped' THEN 1 ELSE 0 END) AS dropped_count
         FROM entries
         WHERE thread_id = ?`
      )
      .get(threadID);
    const stableClaimCounts = this.db
      .prepare(
        `SELECT
           COUNT(*) AS stable_claim_count,
           MAX(updated_at) AS stable_claim_updated_at
         FROM claims
         WHERE thread_id = ?
           AND status = 'stable'`
      )
      .get(threadID);
    const anchorCounts = this.db
      .prepare(
        `SELECT
           COUNT(*) AS anchor_count,
           MAX(created_at) AS latest_anchor_created_at
         FROM anchors
         WHERE thread_id = ?`
      )
      .get(threadID);

    return this.upsertThreadAggregate(threadID, {
      entryCount: Number(entryCounts?.entry_count ?? 0),
      evidenceCount: Number(entryCounts?.evidence_count ?? 0),
      sourceCount: Number(entryCounts?.source_count ?? 0),
      stableClaimCount: Number(stableClaimCounts?.stable_claim_count ?? 0),
      stableClaimUpdatedAt: stableClaimCounts?.stable_claim_updated_at ?? null,
      anchorCount: Number(anchorCounts?.anchor_count ?? 0),
      latestAnchorCreatedAt: anchorCounts?.latest_anchor_created_at ?? null,
      decidedCount: Number(entryCounts?.decided_count ?? 0),
      solvedCount: Number(entryCounts?.solved_count ?? 0),
      verifiedCount: Number(entryCounts?.verified_count ?? 0),
      droppedCount: Number(entryCounts?.dropped_count ?? 0)
    });
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

  countMemoryRecords(threadID = null, scope = null) {
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
    const row = this.db.prepare(`SELECT COUNT(*) AS count FROM memory_records${where}`).get(...params);
    return Number(row?.count ?? 0);
  }

  fetchMemoryRecordPreview(threadID, { scope = null, limit = 200 } = {}) {
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
    const rows = this.db
      .prepare(
        `SELECT * FROM memory_records${where}
         ORDER BY created_at DESC
         LIMIT ?`
      )
      .all(...params, Math.max(1, Number(limit) || 1));
    return rows.reverse().map(mapMemoryRecordRow);
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
           source_metadata_json, status, status_metadata_json, object_mentions_json, references_json,
           author_type, inbox_state, importance_score, confidence_score, session_id, created_at
         ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(id) DO UPDATE SET
           thread_id = excluded.thread_id,
           parent_entry_id = excluded.parent_entry_id,
           supersedes_entry_id = excluded.supersedes_entry_id,
           kind = excluded.kind,
           body_json = excluded.body_json,
           summary_text = excluded.summary_text,
           source_metadata_json = excluded.source_metadata_json,
           status = excluded.status,
           status_metadata_json = excluded.status_metadata_json,
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
        entry.status ?? "open",
        encodeJSON(entry.statusMetadata ?? null),
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

  replaceMemoryRecords(threadID, records) {
    this.#transaction(() => {
      this.db.prepare("DELETE FROM memory_records WHERE thread_id = ?").run(threadID);
      for (const record of records ?? []) {
        this.db
          .prepare("INSERT INTO memory_records (id, thread_id, scope, text, provenance, created_at) VALUES (?, ?, ?, ?, ?, ?)")
          .run(record.id, record.threadID, record.scope, record.text, record.provenance, toISO(record.createdAt));
      }
    });
  }

  fetchMemoryRecordsByProvenance(provenance) {
    return this.db
      .prepare("SELECT * FROM memory_records WHERE provenance = ? ORDER BY created_at ASC")
      .all(provenance)
      .map(mapMemoryRecordRow);
  }

  fetchThreadMaterializedVersion(threadID) {
    const row = this.db
      .prepare("SELECT * FROM thread_materialized_versions WHERE thread_id = ?")
      .get(threadID);
    return row
      ? {
          threadID: row.thread_id,
          memoryVersion: Number(row.memory_version ?? 0),
          retrievalVersion: Number(row.retrieval_version ?? 0),
          updatedAt: row.updated_at
        }
      : {
          threadID,
          memoryVersion: 0,
          retrievalVersion: 0,
          updatedAt: null
        };
  }

  touchThreadMaterializedVersion(threadID, { memory = false, retrieval = false } = {}) {
    if (!threadID || (!memory && !retrieval)) {
      return this.fetchThreadMaterializedVersion(threadID);
    }
    const previous = this.fetchThreadMaterializedVersion(threadID);
    const updatedAt = new Date().toISOString();
    const next = {
      memoryVersion: previous.memoryVersion + (memory ? 1 : 0),
      retrievalVersion: previous.retrievalVersion + (retrieval ? 1 : 0),
      updatedAt
    };
    this.db
      .prepare(
        `INSERT INTO thread_materialized_versions (thread_id, memory_version, retrieval_version, updated_at)
         VALUES (?, ?, ?, ?)
         ON CONFLICT(thread_id) DO UPDATE SET
           memory_version = excluded.memory_version,
           retrieval_version = excluded.retrieval_version,
           updated_at = excluded.updated_at`
      )
      .run(threadID, next.memoryVersion, next.retrievalVersion, updatedAt);
    return {
      threadID,
      ...next
    };
  }

  replaceMemoryRecordsByProvenance(provenance, records = []) {
    this.#transaction(() => {
      const existing = this.fetchMemoryRecordsByProvenance(provenance);
      if (existing.length > 0) {
        const ids = existing.map((record) => record.id);
        const placeholders = ids.map(() => "?").join(", ");
        this.db.prepare(`DELETE FROM memory_records WHERE id IN (${placeholders})`).run(...ids);
        this.db
          .prepare(`DELETE FROM retrieval_documents WHERE owner_type = 'memory' AND owner_id IN (${placeholders})`)
          .run(...ids);
        if (this.ftsAvailable) {
          this.db
            .prepare(`DELETE FROM retrieval_fts WHERE owner_type = 'memory' AND id IN (${ids.map(() => "?").join(", ")})`)
            .run(...ids);
        }
      }
      for (const record of records ?? []) {
        this.db
          .prepare("INSERT INTO memory_records (id, thread_id, scope, text, provenance, created_at) VALUES (?, ?, ?, ?, ?, ?)")
          .run(record.id, record.threadID, record.scope, record.text, record.provenance, toISO(record.createdAt));
      }
    });
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

  upsertRetrievalDocuments(documents = []) {
    this.#transaction(() => {
      for (const document of documents ?? []) {
        this.db
          .prepare(
            `INSERT INTO retrieval_documents (
               id, owner_type, owner_id, thread_id, title, body, metadata_json, created_at, updated_at
             ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
             ON CONFLICT(id) DO UPDATE SET
               owner_type = excluded.owner_type,
               owner_id = excluded.owner_id,
               thread_id = excluded.thread_id,
               title = excluded.title,
               body = excluded.body,
               metadata_json = excluded.metadata_json,
               created_at = excluded.created_at,
               updated_at = excluded.updated_at`
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
          this.db.prepare("DELETE FROM retrieval_fts WHERE id = ?").run(document.id);
          this.db
            .prepare(
              "INSERT INTO retrieval_fts (id, owner_type, thread_id, title, body) VALUES (?, ?, ?, ?, ?)"
            )
            .run(document.id, document.ownerType, document.threadID ?? null, document.title ?? "", document.body ?? "");
        }
      }
    });
  }

  deleteRetrievalDocumentsByOwners(owners = []) {
    const normalizedOwners = (owners ?? []).filter((owner) => owner?.ownerType && owner?.ownerID);
    if (normalizedOwners.length === 0) {
      return;
    }
    this.#transaction(() => {
      for (const owner of normalizedOwners) {
        const documentIDs = this.db
          .prepare("SELECT id FROM retrieval_documents WHERE owner_type = ? AND owner_id = ?")
          .all(owner.ownerType, owner.ownerID)
          .map((row) => row.id);
        this.db
          .prepare("DELETE FROM retrieval_documents WHERE owner_type = ? AND owner_id = ?")
          .run(owner.ownerType, owner.ownerID);
        if (this.ftsAvailable && documentIDs.length > 0) {
          const placeholders = documentIDs.map(() => "?").join(", ");
          this.db.prepare(`DELETE FROM retrieval_fts WHERE id IN (${placeholders})`).run(...documentIDs);
        }
      }
    });
  }

  fetchRetrievalDocumentsRecency({ threadID = null, ownerTypes = [], limit = 30 } = {}) {
    return this.db
      .prepare(buildRecencyQuery({ threadID, ownerTypes, limit }))
      .all(...buildRecencyParams({ threadID, ownerTypes }))
      .map(mapRetrievalRow);
  }

  searchRetrievalDocuments(query, { matchExpr = null, threadID = null, ownerTypes = [], limit = 30 } = {}) {
    if (this.ftsAvailable) {
      const params = [];
      let sql = `
        SELECT rd.*, bm25(retrieval_fts) AS fts_rank
        FROM retrieval_fts
        JOIN retrieval_documents rd ON rd.id = retrieval_fts.id
        WHERE retrieval_fts MATCH ?`;
      params.push(matchExpr ?? String(query ?? "").trim());
      if (threadID) {
        sql += " AND rd.thread_id = ?";
        params.push(threadID);
      }
      if (ownerTypes.length > 0) {
        sql += ` AND rd.owner_type IN (${ownerTypes.map(() => "?").join(", ")})`;
        params.push(...ownerTypes);
      }
      sql += " ORDER BY fts_rank ASC LIMIT ?";
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

function mapThreadAggregateRow(row) {
  return {
    threadID: row.thread_id,
    entryCount: Number(row.entry_count ?? 0),
    evidenceCount: Number(row.evidence_count ?? 0),
    sourceCount: Number(row.source_count ?? 0),
    stableClaimCount: Number(row.stable_claim_count ?? 0),
    stableClaimUpdatedAt: row.stable_claim_updated_at ?? null,
    anchorCount: Number(row.anchor_count ?? 0),
    latestAnchorCreatedAt: row.latest_anchor_created_at ?? null,
    decidedCount: Number(row.decided_count ?? 0),
    solvedCount: Number(row.solved_count ?? 0),
    verifiedCount: Number(row.verified_count ?? 0),
    droppedCount: Number(row.dropped_count ?? 0),
    updatedAt: row.updated_at ?? null
  };
}

function createEmptyThreadAggregate(threadID) {
  return {
    threadID: threadID ?? null,
    entryCount: 0,
    evidenceCount: 0,
    sourceCount: 0,
    stableClaimCount: 0,
    stableClaimUpdatedAt: null,
    anchorCount: 0,
    latestAnchorCreatedAt: null,
    decidedCount: 0,
    solvedCount: 0,
    verifiedCount: 0,
    droppedCount: 0,
    updatedAt: null
  };
}

function clampCount(value) {
  return Math.max(0, Number(value ?? 0));
}

function maxTimestamp(current, candidate) {
  if (!candidate) {
    return current ?? null;
  }
  if (!current) {
    return candidate;
  }
  return new Date(candidate).getTime() >= new Date(current).getTime() ? candidate : current;
}

function mapEntryRow(row) {
  return createEntry({
    id: row.id,
    threadID: row.thread_id,
    kind: row.kind,
    status: row.status,
    body: decodeJSON(row.body_json, {}),
    summaryText: row.summary_text,
    sourceMetadata: decodeJSON(row.source_metadata_json, null),
    statusMetadata: decodeJSON(row.status_metadata_json, null),
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

function buildEntryPageQuery({ threadID = null, cursor = null, limit = 50, topLevelOnly = true } = {}) {
  const conditions = [];
  const params = [];

  if (threadID) {
    conditions.push("thread_id = ?");
    params.push(threadID);
  }
  if (cursor?.createdAt && cursor?.id) {
    conditions.push("(created_at < ? OR (created_at = ? AND id < ?))");
    params.push(cursor.createdAt, cursor.createdAt, cursor.id);
  }

  const where = conditions.length > 0 ? ` WHERE ${conditions.join(" AND ")}` : "";
  return {
    sql: `SELECT * FROM entries${where} ORDER BY created_at DESC, id DESC LIMIT ?`,
    params: [...params, Math.max(1, Number(limit) || 1)]
  };
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
    ftsRank: fromFTS ? row.fts_rank ?? 0 : null
  };
}

function summarizeStatusSummaryRows(rows = []) {
  const grouped = {
    decided: [],
    solved: [],
    verified: [],
    dropped: []
  };
  for (const row of rows ?? []) {
    if (!(row.status in grouped)) {
      continue;
    }
    const statusMetadata = decodeJSON(row.status_metadata_json, null);
    grouped[row.status].push({
      id: row.id,
      threadID: row.thread_id ?? null,
      kind: row.kind ?? "note",
      status: row.status,
      summaryText: row.summary_text ?? "",
      updatedAt: statusMetadata?.updatedAt ?? row.created_at,
      source: statusMetadata?.source ?? "heuristic"
    });
  }
  return grouped;
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
