import { applyCompletion, completionsForTrigger, CompletionTriggerKind, detectCompletionTrigger, shouldSyncFromInput } from "./completionState.js";
import { CompletionPopup } from "./completionPopup.js";
import { highlightToHTML } from "./syntaxHighlighter.js";
import { createElement } from "./dom.js";
import { normalizeReferenceRelation, parseReferencesFromText, tokenizeReferenceText } from "../../domain/references/referenceSyntax.js";

export function createCaptureEditorRuntime({
  mount,
  text = "",
  attachments = [],
  placeholder = "#role @object [[reference]] or [[supports|reference]]",
  submitLabel = "Save",
  submitButtonText = "\u2191",
  minHeight = 120,
  variant = "panel",
  submitPlacement = "footer",
  restoreFocusOnSubmit = false,
  getEditorState,
  onSubmit,
  onAttachmentDrop = null,
  onStateChange = null
}) {
  let currentText = text;
  let pendingAttachments = [...attachments];
  let pendingReferenceBindings = [];
  let isComposing = false;
  let isSubmitting = false;
  let dragDepth = 0;
  let isFileDragActive = false;
  let activeTrigger = null;
  const popup = new CompletionPopup();

  const root = createElement("div", {
    className: `capture-editor-runtime capture-editor-runtime-${variant} capture-editor-submit-${submitPlacement}`
  });
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

  const submitButton = createElement("button", {
    className: "capture-editor-submit",
    text: submitButtonText,
    attrs: {
      "aria-label": submitLabel,
      title: submitLabel
    }
  });
  submitButton.disabled = !currentText.trim() && pendingAttachments.length === 0;

  const attachmentBar = createElement("div", { className: "capture-editor-attachments" });
  if (submitPlacement !== "hidden") {
    editor.append(submitButton);
  }
  root.append(editor, attachmentBar);
  mount.replaceChildren(root);
  renderAttachmentBar();
  syncFromTextarea();

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
    if (shouldSubmitFromKeydown(event, { isComposing, isSubmitting, popupOpen: popup.isOpen() })) {
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
      if (event.key === "Escape") { event.preventDefault(); event.stopPropagation(); popup.hide(); activeTrigger = null; return; }
    }
  });
  textarea.addEventListener("dragover", (event) => {
    if (event.dataTransfer?.types?.includes("Files")) {
      event.preventDefault();
      setFileDragActive(true);
    }
  });
  textarea.addEventListener("dragenter", (event) => {
    if (event.dataTransfer?.types?.includes("Files")) {
      event.preventDefault();
      dragDepth += 1;
      setFileDragActive(true);
    }
  });
  textarea.addEventListener("dragleave", () => {
    if (dragDepth > 0) {
      dragDepth -= 1;
    }
    if (dragDepth === 0) {
      setFileDragActive(false);
    }
  });
  textarea.addEventListener("drop", async (event) => {
    const files = Array.from(event.dataTransfer?.files ?? []);
    dragDepth = 0;
    setFileDragActive(false);
    if (files.length === 0 || !onAttachmentDrop) {
      return;
    }
    event.preventDefault();
    for (const file of files) {
      const copied = await onAttachmentDrop(file, { source: "finderDrop" });
      if (!copied?.relativePath) {
        showErrorPill(file.name);
        continue;
      }
      pendingAttachments.push({
        relativePath: copied.relativePath,
        fileName: file.name,
        mimeType: file.type,
        size: file.size
      });
      renderAttachmentBar();
    }
    updateSubmitState();
  });
  textarea.addEventListener("paste", async (event) => {
    const clipboard = event.clipboardData;
    if (!clipboard) {
      return;
    }

    const files = pastedFilesFromClipboard(clipboard);
    if (files.length > 0 && onAttachmentDrop) {
      event.preventDefault();
      for (const file of files) {
        const copied = await onAttachmentDrop(file, { source: "paste" });
        if (!copied?.relativePath) {
          showErrorPill(file.name);
          continue;
        }
        pendingAttachments.push({
          relativePath: copied.relativePath,
          fileName: file.name,
          mimeType: file.type,
          size: file.size
        });
        renderAttachmentBar();
      }
      updateSubmitState();
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
    // Auto-convert Chinese brackets to trigger reference completion
    const replaced = textarea.value.replace(/【【/g, "[[").replace(/】】/g, "]]");
    if (replaced !== textarea.value) {
      const pos = textarea.selectionStart - (textarea.value.length - replaced.length);
      textarea.value = replaced;
      textarea.setSelectionRange(pos, pos);
    }

    currentText = textarea.value;
    overlay.innerHTML = `${highlightToHTML(currentText)}<br />`;
    updateSubmitState();

    // Auto-resize textarea to fit content
    textarea.style.height = "auto";
    textarea.style.height = textarea.scrollHeight + "px";
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
        if (item.kind === CompletionTriggerKind.REFERENCE && item.targetID) {
          const insertedReference = tokenizeReferenceText(applied.text).find((segment) =>
            segment.type === "reference" && segment.start === activeTrigger.tokenStart
          );
          pendingReferenceBindings.push({
            label: item.insertionText,
            relationKind: normalizeReferenceRelation(item.selectedRelation ?? activeTrigger?.selectedRelation ?? null),
            targetID: item.targetID,
            referenceIndex: insertedReference?.index ?? null
          });
        }
        popup.hide();
        activeTrigger = null;
        syncFromTextarea();
      }
    });
  }

  function renderAttachmentBar() {
    attachmentBar.replaceChildren();
    if (pendingAttachments.length === 0 && !isFileDragActive) {
      attachmentBar.style.display = "none";
      return;
    }
    attachmentBar.style.display = "flex";
    if (pendingAttachments.length === 0 && isFileDragActive) {
      attachmentBar.append(createElement("span", {
        className: "capture-editor-attachment-drop-hint",
        text: "Drop files to attach"
      }));
      return;
    }
    attachmentBar.append(buildAttachmentPills(pendingAttachments, {
      onRemove(index) {
        pendingAttachments.splice(index, 1);
        renderAttachmentBar();
        updateSubmitState();
      }
    }));
  }

  function showErrorPill(fileName) {
    attachmentBar.style.display = "flex";
    const pill = createElement("span", {
      className: "capture-editor-attachment-pill capture-editor-attachment-error",
      text: `${fileName} failed`
    });
    attachmentBar.append(pill);
    setTimeout(() => { pill.remove(); renderAttachmentBar(); }, 3000);
  }

  function updateSubmitState() {
    const canSubmit = currentText.trim().length > 0 || pendingAttachments.length > 0;
    submitButton.disabled = !canSubmit || isSubmitting;
    submitButton.classList.toggle("is-active", canSubmit && !isSubmitting);
    submitButton.setAttribute("aria-busy", String(isSubmitting));
    textarea.disabled = isSubmitting;
    textarea.setAttribute("aria-busy", String(isSubmitting));
    submitButton.textContent = isSubmitting ? "…" : submitButtonText;
    onStateChange?.({
      text: currentText,
      attachments: [...pendingAttachments],
      canSubmit
    });
  }

  function setFileDragActive(next) {
    if (isFileDragActive === next) {
      return;
    }
    isFileDragActive = next;
    root.classList.toggle("is-drag-over", next);
    renderAttachmentBar();
  }

  async function submit() {
    if (isSubmitting) return;
    if (!currentText.trim() && pendingAttachments.length === 0) return;
    isSubmitting = true;
    updateSubmitState();
    try {
      const attachmentsToSend = [...pendingAttachments];
      const referencesToSend = bindSubmittedReferences(currentText, pendingReferenceBindings);
      await onSubmit(currentText, attachmentsToSend, referencesToSend);
      currentText = "";
      pendingAttachments = [];
      pendingReferenceBindings = [];
      textarea.value = "";
      syncFromTextarea();
      renderAttachmentBar();
      popup.hide();
    } finally {
      isSubmitting = false;
      updateSubmitState();
      if (restoreFocusOnSubmit) {
        queueMicrotask(() => textarea.focus());
      }
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
    },
    setDraft(next = {}, mode = "replace") {
      const incomingText = String(next.text ?? "");
      const incomingAttachments = Array.isArray(next.attachments) ? next.attachments.filter(Boolean) : [];
      if (mode === "replace") {
        currentText = incomingText;
        pendingAttachments = dedupeAttachments(incomingAttachments);
      } else {
        currentText = mergeDraftText(currentText, incomingText);
        pendingAttachments = dedupeAttachments([...pendingAttachments, ...incomingAttachments]);
      }
      textarea.value = currentText;
      syncFromTextarea();
      renderAttachmentBar();
    },
    submit() {
      return submit();
    },
    clear() {
      currentText = "";
      pendingAttachments = [];
      pendingReferenceBindings = [];
      textarea.value = "";
      syncFromTextarea();
      renderAttachmentBar();
    }
  };
}

function mergeDraftText(currentText, incomingText) {
  const current = String(currentText ?? "").trim();
  const incoming = String(incomingText ?? "").trim();
  if (!incoming) {
    return currentText;
  }
  if (!current) {
    return incomingText;
  }
  if (current.includes(incoming)) {
    return currentText;
  }
  return `${currentText.trimEnd()}\n${incoming}`;
}

function dedupeAttachments(attachments) {
  const seen = new Set();
  const results = [];
  for (const attachment of attachments) {
    const key = attachment?.relativePath ?? `${attachment?.fileName ?? ""}:${attachment?.size ?? ""}`;
    if (!key || seen.has(key)) {
      continue;
    }
    seen.add(key);
    results.push(attachment);
  }
  return results;
}

export function bindSubmittedReferences(text, bindings) {
  const parsed = parseReferencesFromText(text);
  const remaining = [...(bindings ?? [])];
  return parsed.map((reference) => {
    const exactIndex = remaining.findIndex((binding) => binding.referenceIndex === reference.index);
    if (exactIndex !== -1) {
      const [binding] = remaining.splice(exactIndex, 1);
      return {
        ...reference,
        targetID: binding.targetID
      };
    }

    const matchIndex = remaining.findIndex((binding) =>
      binding.label === reference.label &&
      normalizeReferenceRelation(binding.relationKind) === normalizeReferenceRelation(reference.relationKind)
    );
    if (matchIndex === -1) {
      return reference;
    }
    const [binding] = remaining.splice(matchIndex, 1);
    return {
      ...reference,
      targetID: binding.targetID
    };
  });
}

export function buildAttachmentPills(attachments, { onRemove }) {
  const fragment = document.createDocumentFragment();
  attachments.forEach((att, index) => {
    const pill = createElement("span", {
      className: "capture-editor-attachment-pill"
    });
    const content = createElement("span", {
      className: "capture-editor-attachment-copy"
    });
    const name = createElement("span", {
      className: "capture-editor-attachment-name",
      text: att.fileName || "attachment"
    });
    const meta = createElement("span", {
      className: "capture-editor-attachment-meta",
      text: formatAttachmentMeta(att)
    });
    const removeBtn = createElement("button", {
      className: "capture-editor-attachment-remove",
      text: "×"
    });
    removeBtn.addEventListener("click", () => onRemove(index));
    content.append(name, meta);
    pill.append(content, removeBtn);
    fragment.append(pill);
  });
  return fragment;
}

function formatAttachmentMeta(attachment) {
  const parts = [];
  const extension = attachment?.fileName
    ? attachment.fileName.split(".").pop()?.trim()?.toUpperCase()
    : "";
  if (extension && extension !== attachment.fileName?.trim()?.toUpperCase()) {
    parts.push(extension);
  }
  if (attachment?.size != null) {
    parts.push(formatPillSize(attachment.size));
  }
  return parts.join(" · ");
}

export function formatPillSize(bytes) {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1048576) return `${Math.round(bytes / 1024)} KB`;
  return `${(bytes / 1048576).toFixed(1)} MB`;
}

export function pastedFilesFromClipboard(clipboard) {
  return Array.from(clipboard?.files ?? []);
}

export function pastedTextFromClipboard(clipboard) {
  const uriList = clipboard?.getData?.("text/uri-list")?.trim?.() ?? "";
  const plainText = clipboard?.getData?.("text/plain") ?? "";
  return uriList || plainText;
}

export function shouldSubmitFromKeydown(event, { isComposing = false, isSubmitting = false, popupOpen = false } = {}) {
  if (!event) return false;
  if (isComposing || isSubmitting || popupOpen) return false;
  if (event.repeat) return false;
  if (event.key !== "Enter") return false;
  return Boolean(event.metaKey || event.ctrlKey);
}

function isNavigationKey(event) {
  if (event.ctrlKey) return true; // macOS emacs cursor movement (C-n/p/f/b/a/e)
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
