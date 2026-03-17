import React from "react";
import { beforeEach, expect, test, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

let navigationState;
let workbenchState;
let entryActionsState;
let themeState;
let shortcutSettingsState;

vi.mock("../../renderer/src/contexts/NavigationContext.jsx", () => ({
  useNavigationContext: () => navigationState
}));

vi.mock("../../renderer/src/contexts/WorkbenchContext.jsx", () => ({
  useWorkbenchContext: () => workbenchState
}));

vi.mock("../../renderer/src/contexts/ThemeContext.jsx", () => ({
  useThemeContext: () => themeState
}));

vi.mock("../../renderer/src/hooks/useEntryActions.js", () => ({
  useEntryActions: () => entryActionsState
}));

vi.mock("../../renderer/src/hooks/useShortcutSettings.js", () => ({
  useShortcutSettings: () => shortcutSettingsState
}));

vi.mock("../../renderer/src/lib/ipc.js", () => ({
  ipc: {
    openSettingsWindow: vi.fn()
  }
}));

vi.mock("../../renderer/src/components/editor/CaptureEditor.jsx", () => ({
  CaptureEditor: ({ onSubmit }) => (
    <button type="button" data-testid="capture-editor" onClick={() => onSubmit?.("note", [], [])}>
      Capture
    </button>
  )
}));

const { ThreadBadge } = await import("../../renderer/src/components/entries/ThreadBadge.jsx");
const { EntryInlineBody } = await import("../../renderer/src/components/entries/EntryInlineBody.jsx");
const { EntryBacklinks } = await import("../../renderer/src/components/entries/EntryBacklinks.jsx");
const { ThreadSurface } = await import("../../renderer/src/components/surfaces/ThreadSurface.jsx");
const { Sidebar } = await import("../../renderer/src/components/shell/Sidebar.jsx");

function makeThreadDetail(id, title) {
  return {
    thread: {
      id,
      title,
      color: "sky",
      goalLayer: { currentStage: "working" }
    },
    entries: [],
    memory: [],
    anchors: [],
    resources: [],
    aiSnapshot: null
  };
}

beforeEach(() => {
  navigationState = {
    openThread: vi.fn(),
    focusEntry: vi.fn(),
    selectedThreadID: "thread-a",
    goBack: vi.fn(),
    goToStream: vi.fn(),
    focusedEntryTarget: null,
    clearFocusedEntry: vi.fn(),
    showThreadInspectorTab: vi.fn(),
    setInspectorOpen: vi.fn()
  };
  workbenchState = {
    home: { threads: [] },
    getThreadDetail: vi.fn(() => null),
    isThreadLoading: vi.fn(() => false),
    openThread: vi.fn(async () => null),
    submitCapture: vi.fn(async () => null),
    archiveThread: vi.fn(async () => null)
  };
  entryActionsState = {
    editingEntryID: null,
    replyingToEntryID: null,
    startEdit: vi.fn(),
    cancelEdit: vi.fn(),
    saveEdit: vi.fn(),
    startReply: vi.fn(),
    cancelReply: vi.fn(),
    submitReply: vi.fn(),
    deleteEntry: vi.fn(),
    routeToThread: vi.fn()
  };
  themeState = {
    preference: "light",
    cycle: vi.fn()
  };
  shortcutSettingsState = {
    shortcuts: []
  };
});

test("renderer thread badge opens the correct thread", () => {
  render(<ThreadBadge thread={{ id: "thread-b", title: "Beta", color: "sky" }} />);

  fireEvent.click(screen.getByRole("button", { name: /beta/i }));

  expect(navigationState.openThread).toHaveBeenCalledWith("thread-b");
});

test("renderer sidebar thread row opens the correct thread", () => {
  navigationState.surface = "thread";
  navigationState.selectedThreadID = "thread-a";
  workbenchState.home = {
    threads: [
      { id: "thread-a", title: "Alpha", color: "sky" },
      { id: "thread-b", title: "Beta", color: "amber" }
    ]
  };

  render(<Sidebar />);

  fireEvent.click(screen.getByRole("button", { name: "Beta" }));

  expect(navigationState.openThread).toHaveBeenCalledWith("thread-b");
});

test("renderer inline reference focuses its target entry and thread", () => {
  render(
    <EntryInlineBody
      entry={{
        summaryText: "Need [[Atlas Spec]] now",
        references: [
          {
            id: "ref-1",
            label: "Atlas Spec",
            relationKind: "informs",
            targetID: "entry-2",
            targetThreadID: "thread-b",
            targetSummaryText: "Atlas Spec",
            isResolved: true
          }
        ]
      }}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: /atlas spec/i }));

  expect(navigationState.focusEntry).toHaveBeenCalledWith("entry-2", { threadID: "thread-b" });
});

test("renderer backlink focuses the source entry and thread", () => {
  render(
    <EntryBacklinks
      entry={{
        id: "entry-2",
        incomingBacklinks: [
          {
            id: "backlink-1",
            sourceEntryID: "entry-1",
            sourceThreadID: "thread-a",
            sourceSummaryText: "Launch plan",
            relationKind: "supports"
          }
        ]
      }}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: /launch plan/i }));

  expect(navigationState.focusEntry).toHaveBeenCalledWith("entry-1", { threadID: "thread-a" });
});

test("renderer thread surface does not keep showing stale thread content while the next thread loads", () => {
  const threadMap = {
    "thread-a": makeThreadDetail("thread-a", "Alpha"),
    "thread-b": null
  };
  const loadingMap = {
    "thread-a": false,
    "thread-b": true
  };
  workbenchState.home = {
    threads: [
      { id: "thread-a", title: "Alpha", color: "sky" },
      { id: "thread-b", title: "Beta", color: "amber" }
    ]
  };
  workbenchState.getThreadDetail = vi.fn((threadID) => threadMap[threadID] ?? null);
  workbenchState.isThreadLoading = vi.fn((threadID) => Boolean(loadingMap[threadID]));

  const view = render(<ThreadSurface />);
  expect(screen.getByText("Alpha")).toBeTruthy();

  navigationState.selectedThreadID = "thread-b";
  view.rerender(<ThreadSurface />);

  expect(screen.getAllByText("Loading thread…").length).toBeGreaterThan(0);
  expect(screen.queryByText("Alpha")).toBeNull();

  threadMap["thread-b"] = makeThreadDetail("thread-b", "Beta");
  loadingMap["thread-b"] = false;
  view.rerender(<ThreadSurface />);

  expect(screen.getByText("Beta")).toBeTruthy();
  expect(screen.getByTestId("capture-editor")).toBeTruthy();
});
