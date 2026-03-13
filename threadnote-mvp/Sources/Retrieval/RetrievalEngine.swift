/// task011 — Deterministic retrieval engine.
/// Pipeline: scope filter → FTS5 recall → structural boosts → sorted results.
/// No LLM dependency. Embeddings are an optional future layer.

import Foundation
import GRDB

// MARK: - Result type

struct RetrievalResult: Identifiable {
    var id: String { ownerID }
    var ownerType: String    // entry / claim / anchor / resource
    var ownerID: String
    var ownerUUID: UUID? { UUID(uuidString: ownerID) }
    var threadID: UUID?
    var title: String
    var body: String
    var score: Int
    var metadataJSON: String
}

// MARK: - Engine

final class RetrievalEngine {

    private let pool: DatabasePool

    init(pool: DatabasePool) {
        self.pool = pool
    }

    // MARK: - Primary API

    /// Recall documents matching `query`, ranked by structural score.
    /// - Parameters:
    ///   - query: Free-text search string. If empty, falls back to recency ranking.
    ///   - threadID: When set, boosts documents from this thread.
    ///   - ownerTypes: Restrict to specific owner types. Empty = all types.
    ///   - limit: Maximum results to return.
    func recall(
        query: String,
        threadID: UUID? = nil,
        ownerTypes: [String] = [],
        limit: Int = 30
    ) throws -> [RetrievalResult] {
        let trimmed = query.trimmingCharacters(in: .whitespaces)
        if trimmed.isEmpty {
            return try recencyFallback(threadID: threadID, ownerTypes: ownerTypes, limit: limit)
        }
        return try ftsRecall(query: trimmed, threadID: threadID, ownerTypes: ownerTypes, limit: limit)
    }

    /// Rank candidate threads by their retrieval documents matching `query`.
    /// Returns thread IDs sorted best-first with an aggregate score.
    func rankThreads(for query: String, candidates: [ThreadRecord]) throws -> [ThreadSuggestion] {
        guard !candidates.isEmpty else { return [] }
        let trimmed = query.trimmingCharacters(in: .whitespaces)
        guard !trimmed.isEmpty else {
            // Fall back to recency: sort by lastActiveAt
            return candidates
                .sorted { $0.lastActiveAt > $1.lastActiveAt }
                .enumerated()
                .map { idx, t in
                    ThreadSuggestion(thread: t, score: candidates.count - idx, reason: "Recently active")
                }
        }

        // Aggregate scores per thread from FTS results
        var threadScores: [UUID: Int] = [:]
        var threadReasons: [UUID: String] = [:]

        let results = try ftsRecall(query: trimmed, threadID: nil, ownerTypes: [], limit: 60)
        for result in results {
            guard let tid = result.threadID else { continue }
            // Only accumulate for candidate threads
            guard candidates.contains(where: { $0.id == tid }) else { continue }
            threadScores[tid, default: 0] += result.score
            if threadReasons[tid] == nil {
                threadReasons[tid] = result.title
            }
        }

        return candidates
            .map { thread in
                let score = threadScores[thread.id] ?? 0
                let reason = threadReasons[thread.id] ?? "No matching content"
                return ThreadSuggestion(thread: thread, score: score, reason: reason)
            }
            .sorted { $0.score > $1.score }
    }

    // MARK: - FTS recall

    private func ftsRecall(
        query: String,
        threadID: UUID?,
        ownerTypes: [String],
        limit: Int
    ) throws -> [RetrievalResult] {
        try pool.read { db in
            // Build FTS match expression — escape for FTS5
            let matchExpr = ftsPattern(for: query)

            // Base FTS query joining with retrieval_documents for metadata
            var sql = """
                SELECT
                    rd.id, rd.owner_type, rd.owner_id, rd.thread_id,
                    rd.title, rd.body, rd.metadata_json, rd.created_at,
                    fts.rank AS fts_rank
                FROM retrieval_fts fts
                JOIN retrieval_documents rd ON rd.rowid = fts.rowid
                WHERE retrieval_fts MATCH ?
            """
            var args: [DatabaseValueConvertible] = [matchExpr]

            if !ownerTypes.isEmpty {
                let placeholders = ownerTypes.map { _ in "?" }.joined(separator: ",")
                sql += " AND rd.owner_type IN (\(placeholders))"
                args += ownerTypes.map { $0 as DatabaseValueConvertible }
            }

            sql += " ORDER BY fts.rank LIMIT \(min(limit * 3, 90))"

            let rows = try Row.fetchAll(db, sql: sql, arguments: StatementArguments(args))
            return rows.compactMap { row -> RetrievalResult? in
                guard
                    let id: String = row["id"],
                    let ownerType: String = row["owner_type"],
                    let ownerID: String = row["owner_id"]
                else { return nil }

                let rawThreadID: String? = row["thread_id"]
                let tid = rawThreadID.flatMap(UUID.init(uuidString:))
                let ftsRank: Double = row["fts_rank"] ?? 0   // negative; lower = more relevant
                let createdAt: String = row["created_at"] ?? ""
                let meta: String = row["metadata_json"] ?? "{}"

                let score = computeScore(
                    ftsRank: ftsRank,
                    ownerType: ownerType,
                    metadataJSON: meta,
                    createdAt: createdAt,
                    threadID: tid,
                    filterThreadID: threadID
                )

                return RetrievalResult(
                    ownerType: ownerType,
                    ownerID: ownerID,
                    threadID: tid,
                    title: row["title"] ?? "",
                    body: row["body"] ?? "",
                    score: score,
                    metadataJSON: meta
                )
            }
            .sorted { $0.score > $1.score }
            .prefix(limit)
            .map { $0 }
        }
    }

    // MARK: - Recency fallback

    private func recencyFallback(
        threadID: UUID?,
        ownerTypes: [String],
        limit: Int
    ) throws -> [RetrievalResult] {
        try pool.read { db in
            var sql = "SELECT * FROM retrieval_documents"
            var args: [DatabaseValueConvertible] = []
            var conditions: [String] = []

            if let tid = threadID {
                conditions.append("thread_id = ?")
                args.append(tid.uuidString)
            }
            if !ownerTypes.isEmpty {
                let placeholders = ownerTypes.map { _ in "?" }.joined(separator: ",")
                conditions.append("owner_type IN (\(placeholders))")
                args += ownerTypes.map { $0 as DatabaseValueConvertible }
            }
            if !conditions.isEmpty {
                sql += " WHERE " + conditions.joined(separator: " AND ")
            }
            sql += " ORDER BY created_at DESC LIMIT \(limit)"

            return try Row.fetchAll(db, sql: sql, arguments: StatementArguments(args))
                .compactMap { row -> RetrievalResult? in
                    guard
                        let ownerType: String = row["owner_type"],
                        let ownerID: String = row["owner_id"]
                    else { return nil }
                    let rawThreadID: String? = row["thread_id"]
                    return RetrievalResult(
                        ownerType: ownerType,
                        ownerID: ownerID,
                        threadID: rawThreadID.flatMap(UUID.init(uuidString:)),
                        title: row["title"] ?? "",
                        body: row["body"] ?? "",
                        score: 1,
                        metadataJSON: row["metadata_json"] ?? "{}"
                    )
                }
        }
    }

    // MARK: - Scoring

    private func computeScore(
        ftsRank: Double,
        ownerType: String,
        metadataJSON: String,
        createdAt: String,
        threadID: UUID?,
        filterThreadID: UUID?
    ) -> Int {
        // FTS rank is negative (lower = more relevant); invert and scale to 0–50
        var score = Int(min(50.0, (-ftsRank) * 20.0))

        // Thread match boost
        if let ft = filterThreadID, threadID == ft {
            score += 20
        }

        // Owner-type boost
        switch ownerType {
        case "anchor": score += 10
        case "claim":  score += 8
        case "entry":  score += 4
        default:       break
        }

        // Settled status boost (decided/solved/verified/dropped)
        if let meta = parseMetadata(metadataJSON) {
            let settledKinds: Set<String> = ["decided", "solved", "verified", "dropped"]
            let kind = meta["kind"] ?? ""
            let status = meta["status"] ?? ""
            if settledKinds.contains(kind) || settledKinds.contains(status) {
                score += 12
            }
        }

        // Recency boost: decay over 30 days
        if let date = isoDate(createdAt) {
            let daysSince = Date().timeIntervalSince(date) / 86_400
            let recency = max(0.0, 1.0 - daysSince / 30.0)
            score += Int(recency * 8)
        }

        return max(0, score)
    }

    // MARK: - Helpers

    private func ftsPattern(for query: String) -> String {
        // Wrap each whitespace-separated token for FTS5 prefix matching
        let tokens = query.split(separator: " ")
            .map { "\($0)*" }
            .joined(separator: " ")
        return tokens.isEmpty ? query : tokens
    }

    private func parseMetadata(_ json: String) -> [String: String]? {
        guard let data = json.data(using: .utf8) else { return nil }
        return try? JSONDecoder().decode([String: String].self, from: data)
    }

    private func isoDate(_ s: String) -> Date? {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f.date(from: s)
    }
}
