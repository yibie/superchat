import { tokenizeForSearch } from "../../../domain/capture/tokenizeForSearch.js";

export class RetrievalEngine {
  constructor({ store }) {
    this.store = store;
  }

  recall(query, { threadID = null, ownerTypes = [], limit = 30 } = {}) {
    const trimmed = String(query ?? "").trim();
    if (!trimmed) {
      return this.recencyFallback({ threadID, ownerTypes, limit });
    }
    return this.ftsRecall(trimmed, { threadID, ownerTypes, limit });
  }

  rankThreads(query, candidates = []) {
    if (!Array.isArray(candidates) || candidates.length === 0) {
      return [];
    }

    const trimmed = String(query ?? "").trim();
    if (!trimmed) {
      return [...candidates]
        .sort((lhs, rhs) => new Date(rhs.lastActiveAt).getTime() - new Date(lhs.lastActiveAt).getTime())
        .map((thread, index) => ({
          thread,
          score: candidates.length - index,
          reason: "Recently active"
        }));
    }

    const candidateIDs = new Set(candidates.map((thread) => thread.id));
    const recalled = this.ftsRecall(trimmed, { limit: 60 });
    const byThread = new Map();
    for (const item of recalled) {
      if (!item.threadID || !candidateIDs.has(item.threadID)) {
        continue;
      }
      const current = byThread.get(item.threadID) ?? { score: 0, reason: null };
      current.score += item.score;
      if (!current.reason) {
        current.reason = item.title || item.body || "No matching content";
      }
      byThread.set(item.threadID, current);
    }

    return [...candidates]
      .map((thread) => {
        const aggregated = byThread.get(thread.id) ?? { score: 0, reason: "No matching content" };
        return {
          thread,
          score: aggregated.score,
          reason: aggregated.reason ?? "No matching content"
        };
      })
      .sort((lhs, rhs) => {
        if (lhs.score === rhs.score) {
          return new Date(rhs.thread.lastActiveAt).getTime() - new Date(lhs.thread.lastActiveAt).getTime();
        }
        return rhs.score - lhs.score;
      });
  }

  recencyFallback({ threadID = null, ownerTypes = [], limit = 30 } = {}) {
    return this.store.fetchRetrievalDocumentsRecency({ threadID, ownerTypes, limit }).map((row) =>
      this.#toRetrievalResult(row, 1)
    );
  }

  ftsRecall(query, { threadID = null, ownerTypes = [], limit = 30 } = {}) {
    const rows = this.store.searchRetrievalDocuments(query, {
      matchExpr: this.ftsPattern(query),
      threadID,
      ownerTypes,
      limit: Math.min(limit * 3, 90)
    });

    return rows
      .map((row) => this.#toRetrievalResult(
        row,
        this.computeScore({
          ftsRank: row.ftsRank ?? 0,
          ownerType: row.ownerType,
          metadataJSON: row.metadataJSON,
          createdAt: row.createdAt,
          threadID: row.threadID,
          filterThreadID: threadID
        })
      ))
      .sort((lhs, rhs) => rhs.score - lhs.score)
      .slice(0, limit);
  }

  ftsPattern(query) {
    const tokens = tokenizeForSearch(query);
    if (tokens.length === 0) {
      return String(query ?? "").trim();
    }
    return tokens.map((token) => `${escapeFTSToken(token)}*`).join(" ");
  }

  computeScore({ ftsRank, ownerType, metadataJSON, createdAt, threadID, filterThreadID }) {
    const magnitude = Math.abs(Number(ftsRank ?? 0));
    let score = magnitude > 0
      ? Math.min(50, Math.max(10, Math.round(-Math.log10(magnitude) * 4)))
      : 10;

    if (filterThreadID && threadID === filterThreadID) {
      score += 20;
    }

    switch (ownerType) {
      case "anchor":
        score += 10;
        break;
      case "claim":
        score += 8;
        break;
      case "entry":
        score += 4;
        break;
      default:
        break;
    }

    const meta = parseMetadata(metadataJSON);
    const settledKinds = new Set(["decided", "solved", "verified", "dropped"]);
    if (settledKinds.has(meta.kind) || settledKinds.has(meta.status)) {
      score += 12;
    }

    const date = parseISODate(createdAt);
    if (date) {
      const daysSince = (Date.now() - date.getTime()) / 86_400_000;
      const recency = Math.max(0, 1 - daysSince / 30);
      score += Math.round(recency * 8);
    }

    return Math.max(0, score);
  }

  #toRetrievalResult(row, score) {
    return {
      id: row.id,
      ownerType: row.ownerType,
      ownerID: row.ownerID,
      threadID: row.threadID,
      title: row.title,
      body: row.body,
      metadataJSON: row.metadataJSON,
      createdAt: row.createdAt,
      updatedAt: row.updatedAt,
      score
    };
  }
}

function parseMetadata(metadataJSON) {
  if (!metadataJSON) {
    return {};
  }
  try {
    return JSON.parse(metadataJSON) ?? {};
  } catch {
    return {};
  }
}

function parseISODate(value) {
  if (!value) {
    return null;
  }
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? null : date;
}

function escapeFTSToken(token) {
  return String(token ?? "").replace(/[^\p{L}\p{N}]+/gu, "");
}
