import { CaptureInterpreter } from "../capture/captureInterpreter.js";
import { tokenizeForSearch } from "../capture/tokenizeForSearch.js";

export class ThreadSignatureEngine {
  constructor({
    threadsProvider,
    entriesProvider,
    claimsProvider,
    latestAnchorProvider,
    captureInterpreter = new CaptureInterpreter()
  }) {
    this.threadsProvider = threadsProvider;
    this.entriesProvider = entriesProvider;
    this.claimsProvider = claimsProvider;
    this.latestAnchorProvider = latestAnchorProvider;
    this.captureInterpreter = captureInterpreter;
  }

  signatures(excluding = new Set()) {
    return this.threadsProvider()
      .filter((thread) => !excluding.has(thread.id))
      .map((thread) => this.#signatureForThread(thread))
      .sort((lhs, rhs) => new Date(rhs.lastActiveAt).getTime() - new Date(lhs.lastActiveAt).getTime());
  }

  signature(threadID) {
    const thread = this.threadsProvider().find((item) => item.id === threadID);
    return thread ? this.#signatureForThread(thread) : null;
  }

  #signatureForThread(thread) {
    const claims = this.claimsProvider(thread.id)
      .filter((claim) => claim.status !== "superseded")
      .sort((lhs, rhs) => new Date(rhs.updatedAt).getTime() - new Date(lhs.updatedAt).getTime());
    const anchor = this.latestAnchorProvider(thread.id);
    const recentEntries = this.entriesProvider(thread.id)
      .filter((entry) => !entry.parentEntryID)
      .sort((lhs, rhs) => new Date(rhs.createdAt).getTime() - new Date(lhs.createdAt).getTime());

    const derivedObjects = this.captureInterpreter.extractObjects([
      thread.title,
      thread.goalLayer.goalStatement,
      ...claims.slice(0, 4).map((claim) => claim.statement),
      anchor?.stateSummary ?? "",
      ...recentEntries.slice(0, 4).map((entry) => entry.summaryText)
    ]);

    return {
      thread,
      goalStatement: thread.goalLayer.goalStatement,
      coreObjects: dedupeObjects([
        ...derivedObjects,
        ...recentEntries.flatMap((entry) =>
          entry.objectMentions?.length > 0
            ? entry.objectMentions
            : this.captureInterpreter.interpretEntry(entry).detectedObjects
        )
      ]),
      activeClaims: claims.slice(0, 4).map((claim) => claim.statement),
      latestAnchorSummary: anchor?.stateSummary ?? null,
      openLoops: anchor?.openLoops ?? [],
      lastActiveAt: thread.lastActiveAt
    };
  }
}

export class ThreadRoutingEngine {
  constructor({
    threadsProvider,
    entriesProvider = () => [],
    claimsProvider = () => [],
    latestAnchorProvider = () => null,
    retrievalEngine = null,
    captureInterpreter = new CaptureInterpreter()
  }) {
    this.retrievalEngine = retrievalEngine;
    this.captureInterpreter = captureInterpreter;
    this.autoRouteThreshold = 20;
    this.autoRouteGapThreshold = 8;
    this.threadSignatureEngine = new ThreadSignatureEngine({
      threadsProvider,
      entriesProvider,
      claimsProvider,
      latestAnchorProvider,
      captureInterpreter
    });
  }

  suggest(noteText, excluding = new Set(), limit = 3) {
    return this.suggestFromInterpretation(this.captureInterpreter.interpretText(noteText), excluding, limit);
  }

  suggestFromInterpretation(interpretation, excluding = new Set(), limit = 3) {
    const signatures = this.threadSignatureEngine.signatures(excluding);
    if (signatures.length === 0) {
      return [];
    }

    if (!String(interpretation.normalizedText ?? "").trim()) {
      return recencyFallback(signatures.map((item) => item.thread), limit);
    }

    const retrievalDiagnostics = this.#retrievalSupport(interpretation, signatures.map((item) => item.thread));
    const ranked = this.#rankThreads(interpretation, signatures, retrievalDiagnostics.byThread).map((item) => ({
      thread: item.thread,
      score: item.totalScore,
      reason: item.reason
    }));
    const positive = ranked.filter((item) => item.score > 0);
    return (positive.length > 0 ? positive : recencyFallback(signatures.map((item) => item.thread), limit)).slice(0, limit);
  }

  decide(noteText, excluding = new Set()) {
    return this.decideFromInterpretation(this.captureInterpreter.interpretText(noteText), excluding);
  }

  decideFromInterpretation(interpretation, excluding = new Set()) {
    const signatures = this.threadSignatureEngine.signatures(excluding);
    if (signatures.length === 0) {
      return { type: "noMatch", reason: "No active threads available." };
    }

    if (!String(interpretation.normalizedText ?? "").trim()) {
      return { type: "noMatch", reason: "Empty capture text." };
    }

    const retrievalDiagnostics = this.#retrievalSupport(interpretation, signatures.map((item) => item.thread));
    const ranked = this.#rankThreads(interpretation, signatures, retrievalDiagnostics.byThread);
    const top = ranked[0];
    const secondScore = ranked[1]?.totalScore ?? 0;

    if (!top || top.totalScore <= 0) {
      return { type: "noMatch", reason: "No confident thread match." };
    }
    if (top.totalScore < this.autoRouteThreshold) {
      return { type: "noMatch", reason: "Top thread score is below the auto-route threshold." };
    }
    if (top.totalScore - secondScore < this.autoRouteGapThreshold) {
      return { type: "noMatch", reason: "Top thread is not separated enough from the next candidate." };
    }

    return {
      type: "route",
      threadID: top.thread.id,
      score: top.totalScore,
      reason: top.reason
    };
  }

  debugState(noteText, excluding = new Set(), limit = 3) {
    const interpretation = this.captureInterpreter.interpretText(noteText);
    const support = this.supportSnapshot(interpretation, excluding, Math.max(limit, 5));
    const decision = this.decideFromInterpretation(interpretation, excluding);
    return {
      supportEngineLabel: "Deterministic routing engine",
      status: decision.type === "route" ? "routed" : "stayedInInbox",
      decisionReason: decision.reason,
      normalizedText: support.normalizedText,
      detectedItemType: support.detectedItemType,
      detectedObjects: support.detectedObjects,
      candidateClaims: support.candidateClaims,
      routingQueries: support.routingQueries,
      tokenizedQueryTerms: support.tokenizedQueryTerms,
      retrievalDiagnostics: support.retrievalDiagnostics,
      topCandidates: support.rankedCandidates.slice(0, limit),
      topScore: support.topScore,
      secondScore: support.secondScore,
      autoRouteThreshold: support.autoRouteThreshold,
      autoRouteGapThreshold: support.autoRouteGapThreshold
    };
  }

  supportSnapshot(interpretation, excluding = new Set(), limit = 5) {
    const signatures = this.threadSignatureEngine.signatures(excluding);
    const retrievalDiagnostics = this.#retrievalSupport(interpretation, signatures.map((item) => item.thread));
    const ranked = this.#rankThreads(interpretation, signatures, retrievalDiagnostics.byThread);
    return {
      normalizedText: interpretation.normalizedText,
      detectedItemType: interpretation.detectedItemType,
      detectedObjects: interpretation.detectedObjects.map((item) => item.name),
      candidateClaims: interpretation.candidateClaims.map((item) => item.text),
      routingQueries: interpretation.routingSignals.queries,
      tokenizedQueryTerms: dedupeTerms(
        (interpretation.routingSignals.queries ?? []).flatMap((query) => tokenizeForSearch(query))
      ),
      retrievalDiagnostics: retrievalDiagnostics.queries,
      rankedCandidates: ranked.slice(0, limit).map((item) => ({
        threadID: item.thread.id,
        threadTitle: item.thread.title,
        coreObjects: item.signature.coreObjects.map((object) => object.name),
        totalScore: item.totalScore,
        semanticScore: item.semanticScore,
        retrievalScore: item.retrievalScore,
        reason: item.reason
      })),
      topScore: ranked[0]?.totalScore ?? null,
      secondScore: ranked[1]?.totalScore ?? null,
      autoRouteThreshold: this.autoRouteThreshold,
      autoRouteGapThreshold: this.autoRouteGapThreshold
    };
  }

  #rankThreads(interpretation, signatures, retrievalScores = new Map()) {
    return signatures
      .map((signature) => {
        const semanticPoints = semanticScore(interpretation, signature);
        const retrieval = retrievalScores.get(signature.thread.id) ?? { score: 0, reason: null };
        return {
          thread: signature.thread,
          signature,
          semanticScore: semanticPoints,
          retrievalScore: retrieval.score,
          totalScore: semanticPoints + retrieval.score,
          reason: semanticReason(interpretation, signature) ?? retrieval.reason ?? "No matching content"
        };
      })
      .sort((lhs, rhs) => {
        if (lhs.totalScore === rhs.totalScore) {
          return new Date(rhs.thread.lastActiveAt).getTime() - new Date(lhs.thread.lastActiveAt).getTime();
        }
        return rhs.totalScore - lhs.totalScore;
      });
  }

  #retrievalSupport(interpretation, candidateThreads) {
    const scores = new Map();
    const queries = [];
    if (!this.retrievalEngine?.rankThreads) {
      return { byThread: scores, queries };
    }
    for (const query of interpretation.routingSignals.queries ?? []) {
      const tokenizedTerms = tokenizeForSearch(query);
      const recalled = this.retrievalEngine.recall?.(query, {
        limit: Math.max(30, candidateThreads.length * 6)
      }) ?? [];
      const ranked = this.retrievalEngine.rankThreads(query, candidateThreads);
      queries.push({
        query,
        tokenizedTerms,
        recalledDocuments: recalled.slice(0, 10).map((item) => ({
          ownerType: item.ownerType,
          ownerID: item.ownerID,
          threadID: item.threadID,
          title: item.title,
          score: item.score
        })),
        rankedThreads: (ranked ?? []).slice(0, 5).map((item) => ({
          threadID: item.thread.id,
          threadTitle: item.thread.title,
          score: item.score,
          reason: item.reason
        }))
      });
      for (const item of ranked ?? []) {
        const current = scores.get(item.thread.id) ?? { score: 0, reason: null };
        if (item.score > current.score) {
          scores.set(item.thread.id, { score: item.score, reason: item.reason });
        }
      }
    }
    return { byThread: scores, queries };
  }
}

function semanticScore(interpretation, signature) {
  let score = 0;
  const normalized = String(interpretation.normalizedText ?? "").toLowerCase();
  const tokens = new Set(tokenizeForSearch(normalized));
  const objectNames = new Set((interpretation.detectedObjects ?? []).map((item) => String(item.name).toLowerCase()));
  const claimTexts = (interpretation.candidateClaims ?? []).map((item) => String(item.text).toLowerCase());

  if (normalized.includes(String(signature.thread.title).toLowerCase())) {
    score += 12;
  }
  if (normalized.includes(String(signature.goalStatement).toLowerCase())) {
    score += 10;
  }

  for (const object of signature.coreObjects) {
    if (objectNames.has(String(object.name).toLowerCase())) {
      score += 6;
    }
  }

  for (const claim of signature.activeClaims) {
    const lowered = String(claim).toLowerCase();
    if (claimTexts.some((text) => text === lowered)) {
      score += 12;
      continue;
    }
    const overlap = overlapCount(tokens, new Set(tokenizeForSearch(lowered)));
    score += Math.min(overlap * 2, 8);
  }

  if (signature.latestAnchorSummary) {
    score += Math.min(overlapCount(tokens, new Set(tokenizeForSearch(signature.latestAnchorSummary))) * 2, 6);
  }

  return score;
}

function semanticReason(interpretation, signature) {
  const claimTexts = (interpretation.candidateClaims ?? []).map((item) => String(item.text).toLowerCase());
  const exactClaim = signature.activeClaims.find((claim) => claimTexts.includes(String(claim).toLowerCase()));
  if (exactClaim) {
    return `Claim overlaps with thread claim: ${exactClaim}`;
  }
  const objectNames = new Set((interpretation.detectedObjects ?? []).map((item) => String(item.name).toLowerCase()));
  const matchingObject = signature.coreObjects.find((object) => objectNames.has(String(object.name).toLowerCase()));
  if (matchingObject) {
    return `Object overlap with thread: ${matchingObject.name}`;
  }
  if (
    String(interpretation.normalizedText ?? "")
      .toLowerCase()
      .includes(String(signature.thread.title).toLowerCase())
  ) {
    return `Title overlap with thread: ${signature.thread.title}`;
  }
  return null;
}

function dedupeObjects(objects) {
  const mentions = new Map();
  const counts = new Map();
  for (const object of objects) {
    const key = String(object.name).toLowerCase();
    counts.set(key, (counts.get(key) ?? 0) + 1);
    if (!mentions.has(key)) {
      mentions.set(key, object);
    }
  }
  return [...mentions.values()]
    .sort((lhs, rhs) => {
      const left = counts.get(String(lhs.name).toLowerCase()) ?? 0;
      const right = counts.get(String(rhs.name).toLowerCase()) ?? 0;
      if (left === right) {
        return String(lhs.name).localeCompare(String(rhs.name));
      }
      return right - left;
    })
    .slice(0, 5);
}

function overlapCount(lhs, rhs) {
  let count = 0;
  for (const item of lhs) {
    if (rhs.has(item)) {
      count += 1;
    }
  }
  return count;
}

function dedupeTerms(values) {
  return [...new Set((values ?? []).filter(Boolean))];
}

function recencyFallback(threads, limit) {
  return [...threads]
    .sort((lhs, rhs) => new Date(rhs.lastActiveAt).getTime() - new Date(lhs.lastActiveAt).getTime())
    .slice(0, limit)
    .map((thread) => ({
      thread,
      score: 1,
      reason: "Recently active"
    }));
}
