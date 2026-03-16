import test from "node:test";
import assert from "node:assert/strict";
import {
  applyCompletion,
  completionsForTrigger,
  CompletionTriggerKind,
  detectCompletionTrigger,
  shouldSyncFromInput
} from "../../src/renderer-clean/editor/completionState.js";
import {
  pastedFilesFromClipboard,
  pastedTextFromClipboard
} from "../../src/renderer-clean/editor/captureEditorRuntime.js";
import { highlightToHTML } from "../../src/renderer-clean/editor/syntaxHighlighter.js";

test("clean-room editor detects tag/object/reference triggers", () => {
  assert.deepEqual(detectCompletionTrigger("#cl", 3), {
    kind: CompletionTriggerKind.TAG,
    query: "cl",
    tokenStart: 0,
    tokenEnd: 3
  });
  assert.equal(detectCompletionTrigger("talk to @atl", 12).kind, CompletionTriggerKind.OBJECT);
  assert.equal(detectCompletionTrigger("see [[note", 10).kind, CompletionTriggerKind.REFERENCE);
});

test("clean-room editor builds completion lists from entry state", () => {
  const state = {
    allEntries: [
      {
        summaryText: "Atlas launch blockers",
        createdAt: "2026-03-15T00:00:00Z",
        objectMentions: [{ name: "Atlas" }]
      }
    ]
  };
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.TAG, query: "cl" })[0].title, "claim");
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.OBJECT, query: "at" })[0].title, "Atlas");
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.REFERENCE, query: "at" })[0].title, "Atlas launch blockers");
});

test("clean-room editor applies relation-aware completion insertion", () => {
  const result = applyCompletion("Need [[Atl", {
    kind: CompletionTriggerKind.REFERENCE,
    tokenStart: 5,
    tokenEnd: 10
  }, {
    kind: CompletionTriggerKind.REFERENCE,
    insertionText: "Atlas launch blockers",
    selectedRelation: "supports"
  });
  assert.equal(result.text, "Need [[Atlas launch blockers::supports]] ");
});

test("clean-room editor syntax highlighter marks tags mentions and refs", () => {
  const html = highlightToHTML("#claim talk to @Atlas [[Launch Plan]]");
  assert.match(html, /syntax-tag/);
  assert.match(html, /syntax-object/);
  assert.match(html, /syntax-ref/);
});

test("clean-room editor skips sync during IME composition", () => {
  assert.equal(shouldSyncFromInput({ isComposing: true, inputType: "insertText" }), false);
  assert.equal(shouldSyncFromInput({ isComposing: false, inputType: "insertText" }), true);
});

test("clean-room editor paste helpers prefer uri-list and expose clipboard files", () => {
  const fakeFile = { name: "pasted.png" };
  const clipboard = {
    files: [fakeFile],
    getData(type) {
      if (type === "text/uri-list") {
        return "https://example.com/spec";
      }
      if (type === "text/plain") {
        return "fallback";
      }
      return "";
    }
  };

  assert.equal(pastedTextFromClipboard(clipboard), "https://example.com/spec");
  assert.deepEqual(pastedFilesFromClipboard(clipboard), [fakeFile]);
});
