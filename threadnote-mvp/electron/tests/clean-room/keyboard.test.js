import test from "node:test";
import assert from "node:assert/strict";

// Minimal window shim — capture the keydown listener so we can fire events
let keydownListener = null;
globalThis.window = {
  addEventListener(type, fn) {
    if (type === "keydown") keydownListener = fn;
  },
  removeEventListener(type, fn) {
    if (type === "keydown" && keydownListener === fn) keydownListener = null;
  },
};

const { initKeyboardShortcuts, registerShortcut, unregisterShortcut } =
  await import("../../renderer/src/lib/keyboard.js");

function makeEvent({ key, metaKey = false, ctrlKey = false, shiftKey = false, altKey = false }) {
  let defaultPrevented = false;
  return {
    key,
    metaKey,
    ctrlKey,
    shiftKey,
    altKey,
    preventDefault() { defaultPrevented = true; },
    get defaultPrevented() { return defaultPrevented; },
  };
}

// Initialize once — installs the keydown listener
const cleanup = initKeyboardShortcuts();

// -- Ctrl+key combos must NOT trigger "mod+key" shortcuts -----------------

test("Ctrl+N does not fire mod+n handler", () => {
  let fired = false;
  registerShortcut("mod+n", () => { fired = true; });
  const e = makeEvent({ key: "n", ctrlKey: true });
  keydownListener(e);
  assert.equal(fired, false, "mod+n handler should not fire on Ctrl+N");
  assert.equal(e.defaultPrevented, false);
  unregisterShortcut("mod+n");
});

for (const key of ["p", "b", "f", "a", "e"]) {
  test(`Ctrl+${key.toUpperCase()} does not fire mod+${key} handler`, () => {
    let fired = false;
    registerShortcut(`mod+${key}`, () => { fired = true; });
    const e = makeEvent({ key, ctrlKey: true });
    keydownListener(e);
    assert.equal(fired, false);
    assert.equal(e.defaultPrevented, false);
    unregisterShortcut(`mod+${key}`);
  });
}

// -- Cmd+key combos MUST trigger "mod+key" shortcuts ----------------------

test("Cmd+N fires mod+n handler and prevents default", () => {
  let fired = false;
  registerShortcut("mod+n", () => { fired = true; });
  const e = makeEvent({ key: "n", metaKey: true });
  keydownListener(e);
  assert.equal(fired, true, "mod+n handler should fire on Cmd+N");
  assert.equal(e.defaultPrevented, true);
  unregisterShortcut("mod+n");
});

for (const [combo, key] of [
  ["mod+1", "1"],
  ["mod+2", "2"],
  ["mod+,", ","],
  ["mod+\\", "\\"],
  ["mod+[", "["],
]) {
  test(`Cmd+${key} fires ${combo} handler`, () => {
    let fired = false;
    registerShortcut(combo, () => { fired = true; });
    const e = makeEvent({ key, metaKey: true });
    keydownListener(e);
    assert.equal(fired, true);
    assert.equal(e.defaultPrevented, true);
    unregisterShortcut(combo);
  });
}

// -- Unmatched combos must not preventDefault ------------------------------

test("plain key with no registered shortcut does not preventDefault", () => {
  const e = makeEvent({ key: "n" });
  keydownListener(e);
  assert.equal(e.defaultPrevented, false);
});

// Cleanup
test("cleanup removes keydown listener", () => {
  cleanup();
  assert.equal(keydownListener, null);
});
