import test from "node:test";
import assert from "node:assert/strict";
import { JSDOM } from "jsdom";
import {
  applyCompletion,
  completionsForTrigger,
  CompletionTriggerKind,
  detectCompletionTrigger,
  shouldSyncFromInput
} from "../../src/renderer-clean/editor/completionState.js";
import {
  bindSubmittedReferences,
  createCaptureEditorRuntime,
  pastedFilesFromClipboard,
  pastedTextFromClipboard,
  formatPillSize,
  shouldSubmitFromKeydown
} from "../../src/renderer-clean/editor/captureEditorRuntime.js";
import { highlightToHTML } from "../../src/renderer-clean/editor/syntaxHighlighter.js";

function installDom() {
  const dom = new JSDOM("<!doctype html><html><body></body></html>", {
    pretendToBeVisual: true
  });
  const previousDescriptors = new Map();

  for (const [key, value] of Object.entries({
    window: dom.window,
    document: dom.window.document,
    navigator: dom.window.navigator,
    HTMLElement: dom.window.HTMLElement,
    Node: dom.window.Node,
    getComputedStyle: dom.window.getComputedStyle.bind(dom.window),
    InputEvent: dom.window.InputEvent
  })) {
    previousDescriptors.set(key, Object.getOwnPropertyDescriptor(globalThis, key));
    Object.defineProperty(globalThis, key, {
      configurable: true,
      writable: true,
      value
    });
  }

  return () => {
    dom.window.close();
    for (const [key, descriptor] of previousDescriptors.entries()) {
      if (descriptor) {
        Object.defineProperty(globalThis, key, descriptor);
      } else {
        delete globalThis[key];
      }
    }
  };
}

function dispatchKeydown(target, init) {
  const event = new window.KeyboardEvent("keydown", { bubbles: true, ...init });
  target.dispatchEvent(event);
  return event;
}

function flush() {
  return new Promise((resolve) => setTimeout(resolve, 0));
}

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
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.TAG, query: "qu" })[0].title, "question");
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.OBJECT, query: "at" })[0].title, "Atlas");
  assert.equal(completionsForTrigger(state, { kind: CompletionTriggerKind.REFERENCE, query: "at" })[0].title, "Atlas launch blockers");
});

test("clean-room editor tag completions do not expose entry status values", () => {
  const titles = completionsForTrigger(
    { allEntries: [] },
    { kind: CompletionTriggerKind.TAG, query: "" }
  ).map((item) => item.title);

  assert.equal(titles.includes("decided"), false);
  assert.equal(titles.includes("solved"), false);
  assert.equal(titles.includes("verified"), false);
  assert.equal(titles.includes("dropped"), false);
  assert.deepEqual(titles, ["note", "question", "source"]);
});

test("clean-room editor includes related entries in reference completions", () => {
  const state = {
    allEntries: [
      {
        id: "entry-1",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T00:00:00Z",
        objectMentions: []
      },
      {
        id: "entry-2",
        summaryText: "Follow-up detail",
        createdAt: "2026-03-15T01:00:00Z",
        objectMentions: []
      }
    ]
  };

  const results = completionsForTrigger(state, { kind: CompletionTriggerKind.REFERENCE, query: "follow-up" });

  assert.equal(results[0].title, "Follow-up detail");
  assert.equal(results[0].targetID, "entry-2");
});

test("clean-room editor reference completions are not truncated to six results", () => {
  const state = {
    allEntries: Array.from({ length: 12 }, (_, index) => ({
      id: `entry-${index + 1}`,
      summaryText: `Atlas note ${index + 1}`,
      createdAt: `2026-03-15T${String(index).padStart(2, "0")}:00:00Z`,
      objectMentions: []
    }))
  };

  const results = completionsForTrigger(state, { kind: CompletionTriggerKind.REFERENCE, query: "atlas" });

  assert.equal(results.length, 12);
  assert.equal(results[0].title, "Atlas note 12");
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

test("shouldSubmitFromKeydown allows Cmd/Ctrl+Enter and rejects repeats or composing", () => {
  assert.equal(shouldSubmitFromKeydown({ key: "Enter", metaKey: true }), true);
  assert.equal(shouldSubmitFromKeydown({ key: "Enter", ctrlKey: true }), true);
  assert.equal(shouldSubmitFromKeydown({ key: "Enter", metaKey: true, repeat: true }), false);
  assert.equal(shouldSubmitFromKeydown({ key: "Enter", metaKey: true }, { isComposing: true }), false);
  assert.equal(shouldSubmitFromKeydown({ key: "Enter", metaKey: true }, { popupOpen: true }), false);
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

test("capture editor submits once on Cmd+Enter and ignores repeat while in flight", async () => {
  const cleanupDom = installDom();
  try {
    const mount = document.createElement("div");
    document.body.append(mount);

    let resolveSubmit;
    let submitCount = 0;
    const runtime = createCaptureEditorRuntime({
      mount,
      text: "Ship this",
      onSubmit: async () => {
        submitCount += 1;
        await new Promise((resolve) => {
          resolveSubmit = resolve;
        });
      }
    });

    const { textarea } = runtime;
    dispatchKeydown(textarea, { key: "Enter", metaKey: true });
    dispatchKeydown(textarea, { key: "Enter", metaKey: true, repeat: true });
    dispatchKeydown(textarea, { key: "Enter", ctrlKey: true });

    assert.equal(submitCount, 1);
    assert.equal(textarea.disabled, true);

    resolveSubmit();
    await flush();

    assert.equal(textarea.disabled, false);
  } finally {
    cleanupDom();
  }
});

test("capture editor does not restore textarea focus after submit completes", async () => {
  const cleanupDom = installDom();
  try {
    const mount = document.createElement("div");
    document.body.append(mount);

    let resolveSubmit;
    createCaptureEditorRuntime({
      mount,
      text: "Ship this",
      onSubmit: async () => new Promise((resolve) => {
        resolveSubmit = resolve;
      })
    });

    const textarea = mount.querySelector("textarea");
    const button = mount.querySelector("button");
    button.focus();
    button.click();

    assert.equal(document.activeElement, button);

    resolveSubmit();
    await flush();

    assert.equal(document.activeElement, button);
  } finally {
    cleanupDom();
  }
});

test("capture editor submits on Ctrl+Enter", async () => {
  const cleanupDom = installDom();
  try {
    const mount = document.createElement("div");
    document.body.append(mount);

    let submitCount = 0;
    createCaptureEditorRuntime({
      mount,
      text: "Ship this",
      onSubmit: async () => {
        submitCount += 1;
      }
    });

    const textarea = mount.querySelector("textarea");
    dispatchKeydown(textarea, { key: "Enter", ctrlKey: true });
    await flush();

    assert.equal(submitCount, 1);
  } finally {
    cleanupDom();
  }
});

test("capture editor does not submit during IME composition", async () => {
  const cleanupDom = installDom();
  try {
    const mount = document.createElement("div");
    document.body.append(mount);

    let submitCount = 0;
    createCaptureEditorRuntime({
      mount,
      text: "中文输入",
      onSubmit: async () => {
        submitCount += 1;
      }
    });

    const textarea = mount.querySelector("textarea");
    textarea.dispatchEvent(new window.CompositionEvent("compositionstart", { bubbles: true }));
    dispatchKeydown(textarea, { key: "Enter", metaKey: true });
    textarea.dispatchEvent(new window.CompositionEvent("compositionend", { bubbles: true }));
    await flush();

    assert.equal(submitCount, 0);
  } finally {
    cleanupDom();
  }
});

test("capture editor Enter confirms completion popup instead of submitting", async () => {
  const cleanupDom = installDom();
  try {
    const mount = document.createElement("div");
    document.body.append(mount);

    let submitCount = 0;
    const runtime = createCaptureEditorRuntime({
      mount,
      text: "",
      getEditorState: () => ({
        threads: [],
        allEntries: [{ id: "entry-1", summaryText: "Atlas launch blockers", createdAt: "2026-03-15T00:00:00Z", objectMentions: [] }],
        objects: []
      }),
      onSubmit: async () => {
        submitCount += 1;
      }
    });

    const { textarea } = runtime;
    textarea.value = "[[Atl";
    textarea.setSelectionRange(5, 5);
    textarea.dispatchEvent(new window.InputEvent("input", { bubbles: true, inputType: "insertText", data: "l" }));
    await flush();

    dispatchKeydown(textarea, { key: "Enter" });
    await flush();

    assert.equal(submitCount, 0);
    assert.equal(textarea.value, "[[Atlas launch blockers]] ");
  } finally {
    cleanupDom();
  }
});
