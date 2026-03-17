export function tokenizeForSearch(text) {
  const lowered = String(text ?? "").toLowerCase().trim();
  if (!lowered) {
    return [];
  }

  const roughTokens = segmentWords(lowered);

  const results = [];
  for (const token of roughTokens) {
    if (containsCJK(token)) {
      results.push(...emitCJKBigrams(token));
      continue;
    }
    if (token.length > 2) {
      results.push(token);
    }
  }
  return dedupe(results);
}

function segmentWords(text) {
  if (typeof Intl?.Segmenter === "function") {
    const segmenter = new Intl.Segmenter(undefined, { granularity: "word" });
    return [...segmenter.segment(text)]
      .filter((item) => item.isWordLike)
      .map((item) => item.segment.trim())
      .filter(Boolean);
  }
  return text
    .split(/[\s,.;:!?()[\]{}"'/\\|<>`~]+/g)
    .map((item) => item.trim())
    .filter(Boolean);
}

function emitCJKBigrams(token) {
  const chars = [...token];
  if (chars.length <= 2) {
    return chars.length === 0 ? [] : [token];
  }
  const results = [];
  for (let index = 0; index < chars.length - 1; index += 1) {
    results.push(chars.slice(index, index + 2).join(""));
  }
  return results;
}

function dedupe(values) {
  const seen = new Set();
  const results = [];
  for (const value of values) {
    if (!value || seen.has(value)) {
      continue;
    }
    seen.add(value);
    results.push(value);
  }
  return results;
}

function containsCJK(text) {
  return [...text].some((char) => {
    const value = char.codePointAt(0);
    return (
      (value >= 0x4e00 && value <= 0x9fff) ||
      (value >= 0x3400 && value <= 0x4dbf) ||
      (value >= 0xf900 && value <= 0xfaff) ||
      (value >= 0x20000 && value <= 0x2fa1f) ||
      (value >= 0xac00 && value <= 0xd7af) ||
      (value >= 0x3040 && value <= 0x30ff)
    );
  });
}
