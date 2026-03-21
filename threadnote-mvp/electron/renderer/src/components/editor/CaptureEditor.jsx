import { useRef, useEffect, useCallback } from "react";
import { createCaptureEditorRuntime } from "../../../../src/renderer-clean/editor/captureEditorRuntime.js";
import { ipc } from "../../lib/ipc.js";

/**
 * React wrapper for the vanilla captureEditorRuntime.
 * Manages lifecycle via refs to avoid re-creating the editor on each render.
 */
export function CaptureEditor({
  onSubmit,
  placeholder,
  submitLabel,
  submitButtonText,
  minHeight = 120,
  getEditorState,
  variant = "panel",
  submitPlacement = "footer",
  restoreFocusOnSubmit = false,
  incomingDraft = null,
  incomingDraftMode = "replace",
  onStateChange,
  onReady,
  onAttachmentAccepted,
}) {
  const mountRef = useRef(null);
  const runtimeRef = useRef(null);
  const onSubmitRef = useRef(onSubmit);
  const getEditorStateRef = useRef(getEditorState);
  const onStateChangeRef = useRef(onStateChange);

  // Keep callback refs up to date without re-creating runtime
  useEffect(() => { onSubmitRef.current = onSubmit; }, [onSubmit]);
  useEffect(() => { getEditorStateRef.current = getEditorState; }, [getEditorState]);
  useEffect(() => { onStateChangeRef.current = onStateChange; }, [onStateChange]);

  const handleAttachmentDrop = useCallback(async (file, meta = {}) => {
    try {
      const filePath = ipc.getFilePath(file);
      let copied = null;
      if (filePath) {
        copied = await ipc.copyAttachment(filePath);
      } else {
        const buffer = await file.arrayBuffer();
        copied = await ipc.writeAttachmentBuffer({
          bytes: Array.from(new Uint8Array(buffer)),
          fileName: file.name,
          mimeType: file.type,
        });
      }
      if (copied?.relativePath) {
        onAttachmentAccepted?.({
          attachment: copied,
          file,
          source: meta.source ?? (filePath ? "finderDrop" : "paste")
        });
      }
      return copied;
    } catch (err) {
      console.warn("Attachment drop failed:", err);
      return null;
    }
  }, [onAttachmentAccepted]);

  useEffect(() => {
    const mount = mountRef.current;
    if (!mount || runtimeRef.current) return;

    const runtime = createCaptureEditorRuntime({
      mount,
      placeholder,
      submitLabel,
      submitButtonText,
      minHeight,
      variant,
      submitPlacement,
      restoreFocusOnSubmit,
      getEditorState: () => getEditorStateRef.current?.() ?? {},
      onSubmit: (text, attachments, references) => onSubmitRef.current?.(text, attachments, references),
      onAttachmentDrop: handleAttachmentDrop,
      onStateChange: (state) => onStateChangeRef.current?.(state),
    });

    runtimeRef.current = runtime;
    onReady?.(runtime);

    return () => {
      onReady?.(null);
      runtimeRef.current = null;
      mount.replaceChildren();
    };
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  useEffect(() => {
    if (!incomingDraft || !runtimeRef.current) {
      return;
    }
    runtimeRef.current.setDraft(incomingDraft, incomingDraftMode);
  }, [incomingDraft, incomingDraftMode]);

  return <div ref={mountRef} className="capture-editor-mount" />;
}
