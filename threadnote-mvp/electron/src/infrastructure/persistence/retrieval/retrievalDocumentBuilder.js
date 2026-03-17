function toISO(value) {
  return value instanceof Date ? value.toISOString() : new Date(value).toISOString();
}

export function buildThreadRetrievalDocuments({ thread, entries = [], claims = [], anchors = [], memoryRecords = [] }) {
  const documents = [];

  for (const entry of entries) {
    const body = String(entry.summaryText ?? entry.body?.text ?? "").trim();
    if (!body) {
      continue;
    }
    documents.push({
      id: `entry:${entry.id}`,
      ownerType: "entry",
      ownerID: entry.id,
      threadID: thread.id,
      title: body.slice(0, 120),
      body,
      metadata: { kind: entry.kind },
      createdAt: toISO(entry.createdAt),
      updatedAt: toISO(entry.createdAt)
    });
  }

  for (const claim of claims) {
    if (!claim.statement) {
      continue;
    }
    documents.push({
      id: `claim:${claim.id}`,
      ownerType: "claim",
      ownerID: claim.id,
      threadID: thread.id,
      title: claim.statement.slice(0, 120),
      body: claim.statement,
      metadata: { status: claim.status },
      createdAt: toISO(claim.createdAt),
      updatedAt: toISO(claim.updatedAt)
    });
  }

  for (const anchor of anchors) {
    const body = [anchor.coreQuestion, anchor.stateSummary, ...(anchor.openLoops ?? [])]
      .filter(Boolean)
      .join("\n");
    if (!body) {
      continue;
    }
    documents.push({
      id: `anchor:${anchor.id}`,
      ownerType: "anchor",
      ownerID: anchor.id,
      threadID: thread.id,
      title: anchor.title ?? "Checkpoint",
      body,
      metadata: { phase: anchor.phase ?? "working" },
      createdAt: toISO(anchor.createdAt),
      updatedAt: toISO(anchor.createdAt)
    });
  }

  for (const record of memoryRecords) {
    if (!record.text) {
      continue;
    }
    documents.push({
      id: `memory:${record.id}`,
      ownerType: "memory",
      ownerID: record.id,
      threadID: thread.id,
      title: `${record.scope} memory`,
      body: record.text,
      metadata: { scope: record.scope, provenance: record.provenance },
      createdAt: toISO(record.createdAt),
      updatedAt: toISO(record.createdAt)
    });
  }

  return documents;
}
