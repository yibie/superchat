export function buildMentionCatalog(entries = []) {
  const names = [];
  const seen = new Set();

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
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      names.push(name);
    }
  }

  return names.sort((lhs, rhs) => lhs.localeCompare(rhs, undefined, { sensitivity: "base" }));
}

function extractMentionNamesFromEntry(entry) {
  const source = String(entry?.body?.text || entry?.summaryText || "");
  const matches = source.match(/(^|[\s(])@([\p{L}\p{N}][\p{L}\p{N}._-]*)/gu) ?? [];
  return matches
    .map((item) => item.trim())
    .map((item) => item.replace(/^@/, ""))
    .filter(Boolean);
}
