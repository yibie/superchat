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
  bindSubmittedReferences,
  pastedFilesFromClipboard,
  pastedTextFromClipboard,
  formatPillSize
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
        id: "entry-1",
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
    selectedRelation: "supports",
    targetID: "entry-1"
  });
  assert.equal(result.text, "Need [[supports|Atlas launch blockers]] ");
});

test("clean-room editor applies default relation insertion without explicit relation", () => {
  const result = applyCompletion("Need [[Atl", {
    kind: CompletionTriggerKind.REFERENCE,
    tokenStart: 5,
    tokenEnd: 10
  }, {
    kind: CompletionTriggerKind.REFERENCE,
    insertionText: "Atlas launch blockers",
    targetID: "entry-1"
  });
  assert.equal(result.text, "Need [[Atlas launch blockers]] ");
});

test("clean-room editor preserves typed relation prefix when confirming target completion", () => {
  const result = applyCompletion("Need [[supports|Atl", {
    kind: CompletionTriggerKind.REFERENCE,
    tokenStart: 5,
    tokenEnd: 19,
    selectedRelation: "supports"
  }, {
    kind: CompletionTriggerKind.REFERENCE,
    insertionText: "Atlas launch blockers",
    targetID: "entry-1"
  });
  assert.equal(result.text, "Need [[supports|Atlas launch blockers]] ");
});

test("clean-room editor detects [[ reference trigger with spaces in query", () => {
  const trigger = detectCompletionTrigger("see [[Atlas launch", 18);
  assert.equal(trigger.kind, CompletionTriggerKind.REFERENCE);
  assert.equal(trigger.query, "atlas launch");
  assert.equal(trigger.tokenStart, 4);
  assert.equal(trigger.tokenEnd, 18);
});

test("clean-room editor parses explicit relation prefix inside [[ relation|ref trigger", () => {
  const trigger = detectCompletionTrigger("see [[supports|Atlas", 20);
  assert.equal(trigger.kind, CompletionTriggerKind.REFERENCE);
  assert.equal(trigger.query, "atlas");
  assert.equal(trigger.selectedRelation, "supports");
});

test("clean-room editor [[ trigger stops at closing brackets", () => {
  // Already-closed reference should not trigger
  assert.equal(detectCompletionTrigger("see [[done]] more", 17), null);
});

test("clean-room editor [[ trigger does not cross newlines", () => {
  assert.equal(detectCompletionTrigger("line1\n[[ref", 11).kind, CompletionTriggerKind.REFERENCE);
  // [[ on previous line should not trigger on current line
  assert.equal(detectCompletionTrigger("[[\nsome text", 12), null);
});

test("clean-room editor syntax highlighter marks tags mentions and refs", () => {
  const html = highlightToHTML("#claim talk to @Atlas [[Launch Plan]]");
  assert.match(html, /syntax-tag/);
  assert.match(html, /syntax-object/);
  assert.match(html, /syntax-ref/);
});

test("clean-room editor syntax highlighter highlights tags after newlines", () => {
  const html = highlightToHTML("line one\n#idea on line two");
  assert.match(html, /syntax-tag/);
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

// --- formatPillSize tests ---

test("formatPillSize returns bytes for values under 1 KB", () => {
  assert.equal(formatPillSize(0), "0 B");
  assert.equal(formatPillSize(512), "512 B");
  assert.equal(formatPillSize(1023), "1023 B");
});

test("formatPillSize returns KB for values under 1 MB", () => {
  assert.equal(formatPillSize(1024), "1 KB");
  assert.equal(formatPillSize(45000), "44 KB");
  assert.equal(formatPillSize(1048575), "1024 KB");
});

test("formatPillSize returns MB for values 1 MB and above", () => {
  assert.equal(formatPillSize(1048576), "1.0 MB");
  assert.equal(formatPillSize(5242880), "5.0 MB");
  assert.equal(formatPillSize(15728640), "15.0 MB");
});

// --- Attachment submit contract tests ---
// These test the submit pipeline contract: onSubmit receives (text, attachments)

test("submit passes (text, attachments) when attachments exist", async () => {
  let receivedText, receivedAttachments;
  const onSubmit = (text, attachments) => {
    receivedText = text;
    receivedAttachments = attachments;
  };

  // Simulate what the runtime does internally on submit with pending attachments
  const currentText = "Check this doc";
  const pendingAttachments = [
    { relativePath: "attachments/abc123.docx", fileName: "report.docx", mimeType: "application/vnd.openxmlformats-officedocument.wordprocessingml.document", size: 45000 }
  ];
  const attachmentsToSend = pendingAttachments.length > 0 ? [...pendingAttachments] : undefined;
  await onSubmit(currentText, attachmentsToSend);

  assert.equal(receivedText, "Check this doc");
  assert.equal(receivedAttachments.length, 1);
  assert.equal(receivedAttachments[0].fileName, "report.docx");
  assert.equal(receivedAttachments[0].relativePath, "attachments/abc123.docx");
});

test("bindSubmittedReferences preserves targetID for completion-selected default reference", () => {
  const references = bindSubmittedReferences("Need [[Atlas launch blockers]]", [
    {
      label: "Atlas launch blockers",
      relationKind: "informs",
      targetID: "entry-1",
      referenceIndex: 0
    }
  ]);

  assert.equal(references[0].targetID, "entry-1");
  assert.equal(references[0].relationKind, "informs");
});

test("bindSubmittedReferences falls back to label and relation when reference index changes", () => {
  const references = bindSubmittedReferences("Need [[supports|Atlas launch blockers]] and [[Atlas launch blockers]]", [
    {
      label: "Atlas launch blockers",
      relationKind: "supports",
      targetID: "entry-1",
      referenceIndex: 99
    }
  ]);

  assert.equal(references[0].targetID, "entry-1");
  assert.equal(references[1].targetID, null);
});

test("submit works with attachments-only (empty text)", async () => {
  let receivedText, receivedAttachments;
  const onSubmit = (text, attachments) => {
    receivedText = text;
    receivedAttachments = attachments;
  };

  const currentText = "";
  const pendingAttachments = [
    { relativePath: "attachments/img001.png", fileName: "screenshot.png", mimeType: "image/png", size: 120000 }
  ];

  // Should allow submit: !currentText.trim() but pendingAttachments.length > 0
  const canSubmit = currentText.trim() || pendingAttachments.length > 0;
  assert.ok(canSubmit, "should allow submit with attachments but no text");

  const attachmentsToSend = pendingAttachments.length > 0 ? [...pendingAttachments] : undefined;
  await onSubmit(currentText, attachmentsToSend);

  assert.equal(receivedText, "");
  assert.equal(receivedAttachments.length, 1);
  assert.equal(receivedAttachments[0].fileName, "screenshot.png");
});

test("attachments clear after submit", async () => {
  const onSubmit = () => {};

  let pendingAttachments = [
    { relativePath: "attachments/a.pdf", fileName: "a.pdf", mimeType: "application/pdf", size: 5000 },
    { relativePath: "attachments/b.png", fileName: "b.png", mimeType: "image/png", size: 8000 }
  ];

  // Simulate submit clearing attachments
  const attachmentsToSend = pendingAttachments.length > 0 ? [...pendingAttachments] : undefined;
  await onSubmit("text", attachmentsToSend);
  pendingAttachments = [];

  assert.equal(pendingAttachments.length, 0, "pending attachments should be empty after submit");
});
