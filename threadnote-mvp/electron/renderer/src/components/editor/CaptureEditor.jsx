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
  minHeight = 120,
  getEditorState,
}) {
  const mountRef = useRef(null);
  const runtimeRef = useRef(null);
  const onSubmitRef = useRef(onSubmit);
  const getEditorStateRef = useRef(getEditorState);

  // Keep callback refs up to date without re-creating runtime
  useEffect(() => { onSubmitRef.current = onSubmit; }, [onSubmit]);
  useEffect(() => { getEditorStateRef.current = getEditorState; }, [getEditorState]);

  const handleAttachmentDrop = useCallback(async (file) => {
    try {
      const filePath = ipc.getFilePath(file);
      if (filePath) {
        return await ipc.copyAttachment(filePath);
      }
      const buffer = await file.arrayBuffer();
      return await ipc.writeAttachmentBuffer({
        bytes: Array.from(new Uint8Array(buffer)),
        fileName: file.name,
        mimeType: file.type,
      });
    } catch (err) {
      console.warn("Attachment drop failed:", err);
      return null;
    }
  }, []);

  useEffect(() => {
    const mount = mountRef.current;
    if (!mount || runtimeRef.current) return;

    const runtime = createCaptureEditorRuntime({
      mount,
      placeholder,
      submitLabel,
      minHeight,
      getEditorState: () => getEditorStateRef.current?.() ?? {},
      onSubmit: (payload) => onSubmitRef.current?.(payload),
      onAttachmentDrop: handleAttachmentDrop,
    });

    runtimeRef.current = runtime;

    return () => {
      runtimeRef.current = null;
      mount.replaceChildren();
    };
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  return <div ref={mountRef} className="capture-editor-mount" />;
}
