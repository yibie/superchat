export function tokenizeForSearch(text) {
  const lowered = String(text ?? "").toLowerCase().trim();
  if (!lowered) {
    return [];
  }

  const roughTokens = lowered
    .split(/[\s,.;:!?()[\]{}"'/\\|<>`~]+/g)
    .map((item) => item.trim())
    .filter(Boolean);

  const results = [];
  for (const token of roughTokens) {
    if (containsCJK(token)) {
      if (token.length > 2) {
        for (let index = 0; index < token.length - 1; index += 1) {
          results.push(token.slice(index, index + 2));
        }
      } else {
        results.push(token);
      }
      continue;
    }
    if (token.length > 2) {
      results.push(token);
    }
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
