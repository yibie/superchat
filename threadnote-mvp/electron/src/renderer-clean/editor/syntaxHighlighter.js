function escapeHTML(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;");
}

export function highlightToHTML(text) {
  const escaped = escapeHTML(text);
  return escaped
    .replace(/(^|\s)(#[a-zA-Z][\w-]*)/gm, (_match, prefix, tag) => `${prefix}<span class="syntax-tag">${tag}</span>`)
    .replace(/(^|\s)(@[\p{L}\p{N}][\p{L}\p{N}._-]*)/gu, (_match, prefix, mention) => `${prefix}<span class="syntax-object">${mention}</span>`)
    .replace(/(\[\[[^\]]+\]\])/g, '<span class="syntax-ref">$1</span>');
}
