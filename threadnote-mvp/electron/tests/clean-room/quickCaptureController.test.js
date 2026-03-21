import test from "node:test";
import assert from "node:assert/strict";
import { QuickCaptureController } from "../../src/desktop/quickCapture/quickCaptureController.js";

test("quick capture close hides app for shortcut-triggered external capture", async () => {
  const events = [];
  const window = createWindowDouble(events);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    clipboardImpl: {
      availableFormats: () => [],
      readText: () => "",
      readImage: () => ({
        isEmpty: () => true
      })
    },
    appImpl: {
      hide() {
        events.push("app.hide");
      }
    },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "shortcut" }
  });
  controller.closeQuickCapture();

  assert.deepEqual(events.filter((event) => event === "app.hide"), ["app.hide"]);
  assert.equal(events.includes("window.hide"), true);
  assert.equal(events.includes("window.setVisibleOnAllWorkspaces:false"), true);
  assert.equal(events.includes("window.setAlwaysOnTop:false"), true);
});

test("quick capture close hides only the quick capture window for menu-triggered opens", async () => {
  const events = [];
  const window = createWindowDouble(events);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    appImpl: {
      hide() {
        events.push("app.hide");
      }
    },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "menu" }
  });
  controller.closeQuickCapture();

  assert.equal(events.includes("app.hide"), false);
  assert.equal(events.includes("window.hide"), true);
  assert.equal(events.includes("window.setVisibleOnAllWorkspaces:false"), true);
  assert.equal(events.includes("window.setAlwaysOnTop:false"), true);
});

test("quick capture shortcut open hydrates clipboard text into the draft", async () => {
  const events = [];
  const sentPayloads = [];
  const window = createWindowDouble(events, sentPayloads);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    clipboardImpl: {
      availableFormats: () => ["text/plain"],
      readText: () => "Copied from clipboard",
      readImage: () => ({
        isEmpty: () => true
      })
    },
    appImpl: { hide() {} },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "shortcut" }
  });

  assert.equal(sentPayloads.length, 1);
  assert.equal(sentPayloads[0].channel, "quick-capture:hydrate");
  assert.equal(sentPayloads[0].payload.text, "Copied from clipboard");
  assert.equal(sentPayloads[0].payload.source, "clipboardImport");
  assert.deepEqual(sentPayloads[0].payload.sourceContext, {
    clipboardTypes: ["text/plain"],
    trigger: "shortcut"
  });
});

test("quick capture shortcut open hydrates clipboard image attachments", async () => {
  const events = [];
  const sentPayloads = [];
  const window = createWindowDouble(events, sentPayloads);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null }),
      writeAttachmentBuffer: () => ({
        relativePath: "attachments/clipboard-image.png",
        absolutePath: "/tmp/attachments/clipboard-image.png"
      })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    clipboardImpl: {
      availableFormats: () => ["image/png"],
      readText: () => "",
      readImage: () => ({
        isEmpty: () => false,
        toPNG: () => Buffer.from([0x89, 0x50, 0x4e, 0x47])
      })
    },
    appImpl: { hide() {} },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "shortcut" }
  });

  assert.equal(sentPayloads[0].payload.text, "");
  assert.equal(sentPayloads[0].payload.attachments.length, 1);
  assert.equal(sentPayloads[0].payload.attachments[0].fileName, "clipboard.png");
  assert.equal(sentPayloads[0].payload.source, "clipboardImport");
  assert.deepEqual(sentPayloads[0].payload.sourceContext, {
    clipboardTypes: ["image/png"],
    trigger: "shortcut"
  });
});

test("quick capture shortcut open falls back cleanly when clipboard is empty", async () => {
  const events = [];
  const sentPayloads = [];
  const window = createWindowDouble(events, sentPayloads);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    clipboardImpl: {
      availableFormats: () => [],
      readText: () => "",
      readImage: () => ({
        isEmpty: () => true
      })
    },
    appImpl: { hide() {} },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "shortcut" }
  });

  assert.deepEqual(sentPayloads[0].payload, {
    text: "",
    attachments: [],
    references: [],
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "shortcut" }
  });
});

test("quick capture menu open does not import clipboard content", async () => {
  const events = [];
  const sentPayloads = [];
  let clipboardReads = 0;
  const window = createWindowDouble(events, sentPayloads);
  const controller = new QuickCaptureController({
    BrowserWindow: {
      getAllWindows: () => [window]
    },
    appService: {
      submitExternalCapture: async () => ({ entry: { id: "entry-1" }, backgroundTask: null })
    },
    shellState: {},
    preloadPath: "/tmp/preload.js",
    createWindow: () => window,
    shortcutStore: { save: (value) => value },
    clipboardImpl: {
      availableFormats: () => {
        clipboardReads += 1;
        return ["text/plain"];
      },
      readText: () => "Should not be used",
      readImage: () => ({
        isEmpty: () => true
      })
    },
    appImpl: { hide() {} },
    globalShortcutImpl: { register: () => true, unregister() {}, unregisterAll() {} }
  });

  await controller.openQuickCapture({
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "menu" }
  });

  assert.equal(clipboardReads, 0);
  assert.deepEqual(sentPayloads[0].payload, {
    text: "",
    attachments: [],
    references: [],
    source: "quickCaptureHotkey",
    sourceContext: { trigger: "menu" }
  });
});

function createWindowDouble(events, sentPayloads = []) {
  const webContents = {
    isLoadingMainFrame: () => false,
    once() {},
    send(channel, payload) {
      sentPayloads.push({ channel, payload });
    }
  };

  return {
    webContents,
    isDestroyed: () => false,
    isMinimized: () => false,
    restore() {
      events.push("window.restore");
    },
    setVisibleOnAllWorkspaces(flag) {
      events.push(`window.setVisibleOnAllWorkspaces:${String(flag)}`);
    },
    setAlwaysOnTop(flag) {
      events.push(`window.setAlwaysOnTop:${String(flag)}`);
    },
    moveTop() {
      events.push("window.moveTop");
    },
    center() {
      events.push("window.center");
    },
    show() {
      events.push("window.show");
    },
    focus() {
      events.push("window.focus");
    },
    hide() {
      events.push("window.hide");
    },
    setContentSize(width, height) {
      events.push(`window.setContentSize:${width}x${height}`);
    },
    getBounds() {
      return { width: 720, height: 92 };
    }
  };
}
