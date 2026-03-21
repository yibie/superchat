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
  assert.equal(events.includes("window.hide"), false);
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
});

function createWindowDouble(events) {
  const webContents = {
    isLoadingMainFrame: () => false,
    once() {},
    send() {}
  };

  return {
    webContents,
    isDestroyed: () => false,
    isMinimized: () => false,
    restore() {
      events.push("window.restore");
    },
    setVisibleOnAllWorkspaces() {
      events.push("window.setVisibleOnAllWorkspaces");
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
