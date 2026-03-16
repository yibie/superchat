import { applyCompletion, completionsForTrigger, CompletionTriggerKind, detectCompletionTrigger, shouldSyncFromInput } from "./completionState.js";
import { CompletionPopup } from "./completionPopup.js";
import { highlightToHTML } from "./syntaxHighlighter.js";
import { createElement } from "./dom.js";

export function createCaptureEditorRuntime({
  mount,
  text = "",
  placeholder = "#role @object [[reference]]",
  helperText = "#role @object [[reference]]",
  submitLabel = "Save",
  minHeight = 120,
  getEditorState,
  onSubmit,
  onAttachmentDrop = null
}) {
  let currentText = text;
  let isComposing = false;
  let activeTrigger = null;
  const popup = new CompletionPopup();

  const root = createElement("div", { className: "capture-editor-runtime" });
  const editor = createElement("div", { className: "capture-editor-shell" });
  const overlay = createElement("pre", { className: "capture-editor-overlay" });
  const textarea = createElement("textarea", {
    className: "capture-editor-input",
    attrs: { placeholder, spellcheck: "false" }
  });
  textarea.style.minHeight = `${minHeight}px`;
  textarea.value = currentText;
  overlay.innerHTML = `${highlightToHTML(currentText)}<br />`;
  editor.append(overlay, textarea);

  const footer = createElement("div", { className: "capture-editor-footer" });
  footer.append(
    createElement("span", { className: "capture-editor-helper", text: helperText }),
    createElement("span", { className: "capture-editor-helper", text: "Cmd/Ctrl+Enter to submit" })
  );
  const submitButton = createElement("button", { className: "primary", text: submitLabel });
  submitButton.disabled = !currentText.trim();
  footer.append(submitButton);

  root.append(editor, footer);
  mount.replaceChildren(root);

  textarea.addEventListener("compositionstart", () => {
    isComposing = true;
  });
  textarea.addEventListener("compositionend", () => {
    isComposing = false;
    syncFromTextarea();
    refreshCompletion();
  });
  textarea.addEventListener("input", (event) => {
    if (!shouldSyncFromInput({ isComposing, inputType: event.inputType })) {
      return;
    }
    syncFromTextarea();
    refreshCompletion();
  });
  textarea.addEventListener("scroll", () => {
    overlay.scrollTop = textarea.scrollTop;
    overlay.scrollLeft = textarea.scrollLeft;
  });
  textarea.addEventListener("click", () => refreshCompletion());
  textarea.addEventListener("keyup", (event) => {
    if (isNavigationKey(event)) return;
    refreshCompletion();
  });
  textarea.addEventListener("keydown", (event) => {
    popup.suppressMouse();
    if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
      event.preventDefault();
      void submit();
      popup.hide();
      return;
    }
    if (popup.items?.length > 0) {
      if (event.key === "ArrowDown") { event.preventDefault(); popup.move(1); return; }
      if (event.key === "ArrowUp") { event.preventDefault(); popup.move(-1); return; }
      if (event.key === "ArrowRight" && activeTrigger?.kind === CompletionTriggerKind.REFERENCE) { event.preventDefault(); popup.moveRight(); return; }
      if (event.key === "ArrowLeft" && activeTrigger?.kind === CompletionTriggerKind.REFERENCE) { event.preventDefault(); popup.moveLeft(); return; }
      if (event.key === "Enter" || event.key === "Tab") { event.preventDefault(); popup.confirm(); return; }
      if (event.key === "Escape") { event.preventDefault(); popup.hide(); activeTrigger = null; return; }
    }
  });
  textarea.addEventListener("dragover", (event) => {
    if (event.dataTransfer?.types?.includes("Files")) {
      event.preventDefault();
    }
  });
  textarea.addEventListener("drop", async (event) => {
    const files = Array.from(event.dataTransfer?.files ?? []);
    if (files.length === 0 || !onAttachmentDrop) {
      return;
    }
    event.preventDefault();
    const relativePaths = [];
    for (const file of files) {
      const copied = await onAttachmentDrop(file);
      if (copied?.relativePath) {
        relativePaths.push(copied.relativePath);
      }
    }
    if (relativePaths.length === 0) {
      return;
    }
    insertText(relativePaths.join("\n"));
  });
  textarea.addEventListener("paste", async (event) => {
    const clipboard = event.clipboardData;
    if (!clipboard) {
      return;
    }

    const files = pastedFilesFromClipboard(clipboard);
    if (files.length > 0 && onAttachmentDrop) {
      event.preventDefault();
      const relativePaths = [];
      for (const file of files) {
        const copied = await onAttachmentDrop(file);
        if (copied?.relativePath) {
          relativePaths.push(copied.relativePath);
        }
      }
      if (relativePaths.length > 0) {
        insertText(relativePaths.join("\n"));
      }
      return;
    }

    const pastedText = pastedTextFromClipboard(clipboard);
    if (pastedText) {
      event.preventDefault();
      insertText(pastedText);
    }
  });
  submitButton.addEventListener("click", () => {
    void submit();
  });

  function syncFromTextarea() {
    currentText = textarea.value;
    overlay.innerHTML = `${highlightToHTML(currentText)}<br />`;
    submitButton.disabled = !currentText.trim();
  }

  function refreshCompletion() {
    activeTrigger = detectCompletionTrigger(textarea.value, textarea.selectionStart);
    if (!activeTrigger) {
      popup.hide();
      return;
    }
    const items = completionsForTrigger(getEditorState?.() ?? {}, activeTrigger);
    if (items.length === 0) {
      popup.hide();
      return;
    }
    popup.show({
      items,
      anchorRect: getCaretRect(textarea, activeTrigger.tokenStart),
      isReference: activeTrigger.kind === CompletionTriggerKind.REFERENCE,
      onChoose(item) {
        const applied = applyCompletion(textarea.value, activeTrigger, item);
        textarea.value = applied.text;
        textarea.setSelectionRange(applied.cursor, applied.cursor);
        popup.hide();
        activeTrigger = null;
        syncFromTextarea();
      }
    });
  }

  async function submit() {
    if (!currentText.trim()) return;
    submitButton.disabled = true;
    try {
      await onSubmit(currentText);
      currentText = "";
      textarea.value = "";
      syncFromTextarea();
      popup.hide();
    } finally {
      submitButton.disabled = !currentText.trim();
    }
  }

  function insertText(insertedText) {
    const pos = textarea.selectionStart;
    const next = textarea.value.slice(0, pos) + insertedText + textarea.value.slice(pos);
    textarea.value = next;
    textarea.setSelectionRange(pos + insertedText.length, pos + insertedText.length);
    syncFromTextarea();
    refreshCompletion();
  }

  return {
    root,
    textarea,
    focus() {
      textarea.focus();
    },
    setText(next) {
      currentText = String(next ?? "");
      textarea.value = currentText;
      syncFromTextarea();
    }
  };
}

export function pastedFilesFromClipboard(clipboard) {
  return Array.from(clipboard?.files ?? []);
}

export function pastedTextFromClipboard(clipboard) {
  const uriList = clipboard?.getData?.("text/uri-list")?.trim?.() ?? "";
  const plainText = clipboard?.getData?.("text/plain") ?? "";
  return uriList || plainText;
}

function isNavigationKey(event) {
  return ["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Enter", "Tab", "Escape"].includes(event.key);
}

function getCaretRect(textarea, pos = textarea.selectionStart) {
  const taRect = textarea.getBoundingClientRect();
  const cs = window.getComputedStyle(textarea);
  const mirror = document.createElement("div");
  [
    "fontFamily", "fontSize", "fontWeight", "fontStyle", "letterSpacing", "textTransform", "lineHeight",
    "paddingTop", "paddingRight", "paddingBottom", "paddingLeft", "borderTopWidth", "borderRightWidth",
    "borderBottomWidth", "borderLeftWidth", "boxSizing"
  ].forEach((property) => {
    mirror.style[property] = cs[property];
  });
  mirror.style.position = "fixed";
  mirror.style.top = `${taRect.top}px`;
  mirror.style.left = `${taRect.left}px`;
  mirror.style.width = `${taRect.width}px`;
  mirror.style.visibility = "hidden";
  mirror.style.whiteSpace = "pre-wrap";
  mirror.style.wordBreak = "break-word";
  const text = textarea.value;
  mirror.append(document.createTextNode(text.slice(0, pos)));
  const caret = document.createElement("span");
  caret.textContent = "\u200b";
  mirror.append(caret);
  mirror.append(document.createTextNode(text.slice(pos) || " "));
  document.body.append(mirror);
  const rect = caret.getBoundingClientRect();
  mirror.remove();
  return {
    top: rect.top - textarea.scrollTop,
    left: rect.left,
    bottom: rect.bottom - textarea.scrollTop
  };
}
