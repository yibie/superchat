import React from "react";
import { beforeEach, expect, test, vi } from "vitest";
import { act, fireEvent, render, screen } from "@testing-library/react";

let workbenchState;
let captureEditorState;
let hydrateListener;
let runtime;
let lastCaptureEditorProps;
const closeQuickCapture = vi.fn();
const resizeQuickCapture = vi.fn();
const importFromClipboard = vi.fn();
const submitQuickCapture = vi.fn();

vi.mock("../../renderer/src/contexts/WorkbenchContext.jsx", () => ({
  useWorkbenchContext: () => workbenchState
}));

vi.mock("../../renderer/src/lib/ipc.js", () => ({
  ipc: {
    closeQuickCapture,
    resizeQuickCapture,
    importFromClipboard,
    submitQuickCapture,
    onQuickCaptureHydrate: (cb) => {
      hydrateListener = cb;
      return () => {
        if (hydrateListener === cb) hydrateListener = null;
      };
    }
  }
}));

vi.mock("../../renderer/src/components/editor/CaptureEditor.jsx", () => ({
  CaptureEditor: (props) => {
    lastCaptureEditorProps = props;
    React.useEffect(() => {
      props.onReady?.(runtime);
      props.onStateChange?.(captureEditorState);
    }, [props]);
    return (
      <button
        type="button"
        data-testid="capture-editor"
        onClick={() => props.onSubmit?.(captureEditorState.text, captureEditorState.attachments, [])}
      >
        Capture Editor
      </button>
    );
  }
}));

const { QuickCaptureApp } = await import("../../renderer/src/components/quick-capture/QuickCaptureApp.jsx");

beforeEach(() => {
  workbenchState = {
    workspace: { workspacePath: "/tmp/threadnote-workspace" },
    home: { threads: [], allEntries: [] }
  };
  captureEditorState = {
    text: "",
    attachments: [],
    canSubmit: false
  };
  hydrateListener = null;
  lastCaptureEditorProps = null;
  runtime = {
    focus: vi.fn(),
    submit: vi.fn(),
    clear: vi.fn()
  };
  closeQuickCapture.mockReset();
  resizeQuickCapture.mockReset();
  importFromClipboard.mockReset();
  submitQuickCapture.mockReset();
});

test("quick capture stays compact for short drafts and expands for longer ones", () => {
  const view = render(<QuickCaptureApp />);

  expect(document.querySelector(".quick-capture-card-compact")).not.toBeNull();
  expect(resizeQuickCapture).toHaveBeenCalledWith({ width: 720, height: 92 });

  captureEditorState = {
    text: "Line 1\nLine 2\nLine 3\nLine 4",
    attachments: [],
    canSubmit: true
  };
  view.rerender(<QuickCaptureApp />);

  expect(document.querySelector(".quick-capture-card-expanded")).not.toBeNull();
  expect(resizeQuickCapture).toHaveBeenLastCalledWith({ width: 820, height: 260 });
});

test("quick capture expands for a single source pill", async () => {
  const view = render(<QuickCaptureApp />);

  await act(async () => {
    hydrateListener?.({
      attachments: [
        {
          kind: "link",
          locator: "https://example.com/spec",
          fileName: "example.com",
          mimeType: "text/uri-list"
        }
      ]
    });
  });

  captureEditorState = {
    text: "",
    attachments: [
      {
        kind: "link",
        locator: "https://example.com/spec",
        fileName: "example.com",
        mimeType: "text/uri-list"
      }
    ],
    canSubmit: true
  };

  view.rerender(<QuickCaptureApp />);

  expect(document.querySelector(".quick-capture-card-expanded")).not.toBeNull();
  expect(resizeQuickCapture).toHaveBeenLastCalledWith({ width: 760, height: 126 });
});

test("quick capture closes on Escape", () => {
  render(<QuickCaptureApp />);

  fireEvent.keyDown(window, { key: "Escape" });

  expect(closeQuickCapture).toHaveBeenCalledTimes(1);
  expect(runtime.clear).toHaveBeenCalledTimes(1);
});

test("quick capture relies on the native window close control", () => {
  render(<QuickCaptureApp />);

  expect(screen.queryByRole("button", { name: "Close quick capture" })).toBeNull();
  expect(screen.queryByRole("button", { name: "Use Clipboard" })).toBeNull();
});

test("quick capture enables the shared send button with explicit label", () => {
  render(<QuickCaptureApp />);

  expect(lastCaptureEditorProps.submitPlacement).toBe("footer");
  expect(lastCaptureEditorProps.submitButtonText).toBe("Send");
  expect(lastCaptureEditorProps.submitLabel).toBe("Send to Inbox");
  expect(lastCaptureEditorProps.incomingDraftMode).toBe("replace");
});

test("quick capture does not show file source labels after finder hydration", () => {
  render(<QuickCaptureApp />);

  hydrateListener?.({
    source: "finderDrop",
    attachments: [{ relativePath: "attachments/demo.pdf", fileName: "demo.pdf", size: 1024 }]
  });

  expect(screen.queryByText("Finder drop")).toBeNull();
  expect(document.querySelector(".quick-capture-bar")).not.toBeNull();
});

test("quick capture submits hydrated clipboard source metadata unchanged", async () => {
  submitQuickCapture.mockResolvedValue({
    entry: { id: "entry-1" },
    backgroundTask: null
  });

  render(<QuickCaptureApp />);

  await act(async () => {
    hydrateListener?.({
      text: "Copied from clipboard",
      source: "clipboardImport",
      sourceContext: {
        trigger: "shortcut",
        clipboardTypes: ["text/plain"]
      }
    });
  });

  captureEditorState = {
    text: "Copied from clipboard",
    attachments: [],
    canSubmit: true
  };

  await act(async () => {
    fireEvent.click(screen.getByTestId("capture-editor"));
  });

  expect(submitQuickCapture).toHaveBeenCalledWith(expect.objectContaining({
    text: "Copied from clipboard",
    source: "clipboardImport",
    sourceContext: expect.objectContaining({
      trigger: "shortcut",
      clipboardTypes: ["text/plain"],
      attachmentCount: 0
    })
  }));
});

test("quick capture converts link pills into locator text on submit", async () => {
  submitQuickCapture.mockResolvedValue({
    entry: { id: "entry-1" },
    backgroundTask: null
  });

  render(<QuickCaptureApp />);

  captureEditorState = {
    text: "Need to review this spec",
    attachments: [
      {
        kind: "link",
        locator: "https://example.com/spec",
        fileName: "example.com",
        mimeType: "text/uri-list"
      }
    ],
    canSubmit: true
  };

  await act(async () => {
    fireEvent.click(screen.getByTestId("capture-editor"));
  });

  expect(submitQuickCapture).toHaveBeenCalledWith(expect.objectContaining({
    text: "Need to review this spec\nhttps://example.com/spec",
    attachments: [],
    sourceContext: expect.objectContaining({
      attachmentCount: 0
    })
  }));
});

test("quick capture closes immediately after successful submit", async () => {
  submitQuickCapture.mockResolvedValue({
    entry: { id: "entry-1" },
    backgroundTask: { useAIRouting: true, refreshThreadID: "thread-1" }
  });

  render(<QuickCaptureApp />);

  captureEditorState = {
    text: "Ship this",
    attachments: [],
    canSubmit: true
  };

  await act(async () => {
    fireEvent.click(screen.getByTestId("capture-editor"));
  });

  expect(submitQuickCapture).toHaveBeenCalledTimes(1);
  expect(closeQuickCapture).toHaveBeenCalledTimes(1);
});
