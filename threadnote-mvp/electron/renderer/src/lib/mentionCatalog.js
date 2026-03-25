export function buildMentionCatalog(entries = []) {
  const mentions = new Map();

  for (const entry of entries ?? []) {
    const candidateNames = [
      ...(entry?.objectMentions ?? []).map((mention) => mention?.name),
      ...extractMentionNamesFromEntry(entry)
    ];

    for (const rawName of candidateNames) {
      const name = String(rawName ?? "").trim().replace(/^@+/, "");
      if (!name) {
        continue;
      }
      const key = name.toLowerCase();
      const current = mentions.get(key) ?? {
        id: `mention:${key}`,
        name,
        label: `@${name}`,
        count: 0,
        lastSeenAt: null,
        threadIDs: new Set()
      };
      current.count += 1;
      if (entry?.threadID) {
        current.threadIDs.add(entry.threadID);
      }
      if (isMoreRecent(entry?.createdAt, current.lastSeenAt)) {
        current.lastSeenAt = entry?.createdAt ?? null;
      }
      mentions.set(key, current);
    }
  }

  return [...mentions.values()]
    .map((mention) => ({
      ...mention,
      threadIDs: [...mention.threadIDs]
    }))
    .sort((lhs, rhs) => lhs.name.localeCompare(rhs.name, undefined, { sensitivity: "base" }));
}

function extractMentionNamesFromEntry(entry) {
  const source = String(entry?.body?.text || entry?.summaryText || "");
  const matches = source.match(/(^|[\s(])@([\p{L}\p{N}][\p{L}\p{N}._-]*)/gu) ?? [];
  return matches
    .map((item) => item.trim())
    .map((item) => item.replace(/^@/, ""))
    .filter(Boolean);
}

function isMoreRecent(lhs, rhs) {
  return new Date(lhs ?? 0).getTime() > new Date(rhs ?? 0).getTime();
}
