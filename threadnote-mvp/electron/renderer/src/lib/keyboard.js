const registry = new Map();

export function registerShortcut(combo, handler) {
  registry.set(combo, handler);
}

export function unregisterShortcut(combo) {
  registry.delete(combo);
}

export function initKeyboardShortcuts() {
  function onKeyDown(e) {
    const parts = [];
    if (e.metaKey) parts.push("mod");
    if (e.shiftKey) parts.push("shift");
    if (e.altKey) parts.push("alt");
    parts.push(e.key.toLowerCase());
    const combo = parts.join("+");

    const handler = registry.get(combo);
    if (handler) {
      e.preventDefault();
      handler(e);
    }
  }

  window.addEventListener("keydown", onKeyDown);
  return () => window.removeEventListener("keydown", onKeyDown);
}
