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
const { EntryCard } = await import("../../renderer/src/components/entries/EntryCard.jsx");
const { EntryInlineBody } = await import("../../renderer/src/components/entries/EntryInlineBody.jsx");
const { EntryBacklinks } = await import("../../renderer/src/components/entries/EntryBacklinks.jsx");
const { ThreadSurface } = await import("../../renderer/src/components/surfaces/ThreadSurface.jsx");
const { Sidebar } = await import("../../renderer/src/components/shell/Sidebar.jsx");
const { ThreadInspector } = await import("../../renderer/src/components/thread/ThreadInspector.jsx");
const { StreamInspector } = await import("../../renderer/src/components/stream/StreamInspector.jsx");

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
    setInspectorOpen: vi.fn(),
    threadInspectorTab: "restart",
    setThreadInspectorTab: vi.fn((value) => {
      navigationState.threadInspectorTab = value;
    })
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
    updateKind: vi.fn(),
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

test("renderer entry card renders ai activity indicator when entry is processing", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Inbox note",
        createdAt: "2026-03-15T10:00:00.000Z",
        aiActivity: {
          visible: true,
          kind: "routePlanning",
          label: "AI 正在判断归档位置"
        }
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText("AI 正在判断归档位置")).toBeTruthy();
  expect(screen.getByTestId("entry-ai-activity-dot")).toBeTruthy();
});

test("renderer entry card omits ai activity indicator when entry is idle", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Inbox note",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.queryByTestId("entry-ai-activity-dot")).toBeNull();
  expect(screen.queryByText("AI 正在判断归档位置")).toBeNull();
});

test("renderer entry card lets user change entry kind from the badge menu", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Inbox note",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: /note/i }));
  fireEvent.click(screen.getByRole("menuitemradio", { name: "Question" }));

  expect(entryActionsState.updateKind).toHaveBeenCalledWith("entry-1", "question");
});

test("renderer entry card shows replies inline in chronological order", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[
        {
          id: "reply-older",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "Older reply",
          createdAt: "2026-03-15T10:01:00.000Z"
        },
        {
          id: "reply-newer",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "Newer reply",
          createdAt: "2026-03-15T10:02:00.000Z"
        }
      ]}
      allEntries={[
        {
          id: "entry-1",
          kind: "note",
          summaryText: "Parent entry",
          createdAt: "2026-03-15T10:00:00.000Z"
        },
        {
          id: "reply-older",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "Older reply",
          createdAt: "2026-03-15T10:01:00.000Z"
        },
        {
          id: "reply-newer",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "Newer reply",
          createdAt: "2026-03-15T10:02:00.000Z"
        }
      ]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  const olderReply = screen.getByText("Older reply");
  const newerReply = screen.getByText("Newer reply");

  expect(olderReply.compareDocumentPosition(newerReply) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
});

test("renderer entry card lets user continue a reply thread inline", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[
        {
          id: "reply-1",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "First reply",
          createdAt: "2026-03-15T10:01:00.000Z"
        }
      ]}
      allEntries={[
        {
          id: "entry-1",
          kind: "note",
          summaryText: "Parent entry",
          createdAt: "2026-03-15T10:00:00.000Z"
        },
        {
          id: "reply-1",
          parentEntryID: "entry-1",
          kind: "note",
          summaryText: "First reply",
          createdAt: "2026-03-15T10:01:00.000Z"
        }
      ]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: /reply to reply/i }));

  expect(entryActionsState.startReply).toHaveBeenCalledWith("reply-1");
});

test("renderer entry card keeps rendering plain text entries through ai background transitions", () => {
  const entry = {
    id: "entry-1",
    kind: "note",
    summaryText: "Launch [[Atlas]] next",
    createdAt: "2026-03-15T10:00:00.000Z",
    references: [
      {
        id: "ref-1",
        label: "Atlas",
        relationKind: "informs",
        targetID: "entry-2",
        targetThreadID: "thread-b",
        targetSummaryText: "Atlas",
        isResolved: true
      }
    ]
  };

  const view = render(
    <EntryCard
      entry={entry}
      entries={[entry]}
      allEntries={[entry]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByRole("button", { name: /atlas/i })).toBeTruthy();
  expect(screen.queryByTestId("entry-ai-activity-dot")).toBeNull();

  view.rerender(
    <EntryCard
      entry={{
        ...entry,
        aiActivity: {
          visible: true,
          kind: "routePlanning",
          label: "AI 正在判断归档位置"
        }
      }}
      entries={[entry]}
      allEntries={[entry]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByTestId("entry-ai-activity-dot")).toBeTruthy();
  expect(screen.getByText("AI 正在判断归档位置")).toBeTruthy();

  const routedEntry = {
    ...entry,
    threadID: "thread-b",
    incomingBacklinks: [
      {
        id: "backlink-1",
        sourceEntryID: "entry-9",
        sourceThreadID: "thread-z",
        sourceSummaryText: "Linked follow-up",
        relationKind: "supports"
      }
    ]
  };

  view.rerender(
    <EntryCard
      entry={{
        ...routedEntry,
        aiActivity: {
          visible: true,
          kind: "threadRefreshing",
          label: "AI 正在整理线程"
        }
      }}
      entries={[routedEntry]}
      allEntries={[routedEntry]}
      threads={[{ id: "thread-b", title: "Beta", color: "sky" }]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText("AI 正在整理线程")).toBeTruthy();
  expect(screen.getByRole("button", { name: /beta/i })).toBeTruthy();
  expect(screen.getByRole("button", { name: /linked follow-up/i })).toBeTruthy();
});

test("renderer thread surface shows newer entries above older entries", () => {
  workbenchState.getThreadDetail = vi.fn(() => ({
    thread: {
      id: "thread-a",
      title: "Alpha",
      color: "sky",
      goalLayer: { currentStage: "working" }
    },
    entries: [
      {
        id: "entry-older",
        kind: "note",
        summaryText: "Older entry",
        createdAt: "2026-03-17T11:00:00.000Z"
      },
      {
        id: "entry-newer",
        kind: "note",
        summaryText: "Newer entry",
        createdAt: "2026-03-17T12:00:00.000Z"
      }
    ],
    memory: [],
    anchors: [],
    resources: [],
    aiSnapshot: null
  }));

  render(<ThreadSurface />);

  const newerEntry = screen.getByText("Newer entry");
  const olderEntry = screen.getByText("Older entry");

  expect(newerEntry.compareDocumentPosition(olderEntry) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
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

test("renderer thread inspector renders unified ai status for restart and prepare views", () => {
  navigationState.threadInspectorTab = "restart";
  workbenchState.getThreadDetail = vi.fn(() => ({
    thread: {
      id: "thread-a",
      title: "Alpha",
      color: "sky",
      goalLayer: { currentStage: "working" }
    },
    entries: [],
    memory: [],
    anchors: [],
    resources: [],
    aiSnapshot: {
      headline: "Recover context",
      restartNote: "Use the saved note.",
      currentJudgment: "Judgment",
      openLoops: []
    },
    aiStatus: {
      resume: {
        status: "invalidPlan",
        message: "AI planner output was rejected.",
        errorKind: "invalidPlan",
        rawErrorMessage: "planner schema mismatch: missing blocks",
        responseModelID: "mock-model",
        finishReason: "stop",
        promptStats: "op=resume prompt=compact",
        startedAt: "2026-03-17T11:59:45.000Z",
        elapsedMS: 15000,
        updatedAt: "2026-03-17T12:00:00.000Z"
      },
      prepare: {
        status: "failed",
        message: "Draft unavailable.",
        errorKind: "backend",
        rawErrorMessage: "upstream 502 from provider",
        responseModelID: null,
        finishReason: null,
        promptStats: "op=prepare timeout=30s",
        startedAt: "2026-03-17T12:01:00.000Z",
        elapsedMS: 62000,
        updatedAt: "2026-03-17T12:02:00.000Z"
      }
    },
    aiDebug: {
      queue: {
        activeCount: 1,
        queueDepth: 2,
        maxConcurrent: 2,
        activeLabels: ["resume:thread-a"],
        pendingLabels: ["prepare:thread-a", "route:entry-1"],
        activeOperations: [
          { label: "resume:thread-a", startedAt: 1710676800000, elapsedMS: 15000 }
        ],
        pendingOperations: [
          { label: "prepare:thread-a", enqueuedAt: 1710676810000, waitMS: 8200 },
          { label: "route:entry-1", enqueuedAt: 1710676815000, waitMS: 5100 }
        ]
      },
      activeOperations: [
        { label: "resume:thread-a", startedAt: "2026-03-17T11:59:45.000Z", elapsedMS: 15000 },
        { label: "prepare:thread-a", startedAt: "2026-03-17T12:01:00.000Z", elapsedMS: 62000 }
      ]
    },
    preparedView: {
      title: "Draft view",
      openLoops: ["Loop A"],
      recommendedNextSteps: ["Step A"],
      contentState: {
        status: "ready",
        message: "Prepared by Mock LLM."
      }
    }
  }));

  const view = render(<ThreadInspector threadID="thread-a" />);
  expect(screen.getByText("Resume Status")).toBeTruthy();
  expect(screen.getAllByText("invalidPlan").length).toBeGreaterThan(0);
  expect(screen.getByText("AI planner output was rejected.")).toBeTruthy();
  expect(screen.getByText("mock-model")).toBeTruthy();
  expect(screen.getByText("Operation Telemetry")).toBeTruthy();
  expect(screen.getByText("op=resume prompt=compact")).toBeTruthy();
  expect(screen.getByText("planner schema mismatch: missing blocks")).toBeTruthy();
  expect(screen.getByText("15.0s")).toBeTruthy();
  expect(screen.getByText("Queue Snapshot")).toBeTruthy();
  expect(screen.getByText("resume:thread-a · 15.0s")).toBeTruthy();
  expect(screen.getByText("prepare:thread-a · 62.0s")).toBeTruthy();
  expect(screen.getByText("prepare:thread-a · waiting 8.2s")).toBeTruthy();
  expect(screen.getByText("route:entry-1 · waiting 5.1s")).toBeTruthy();

  navigationState.threadInspectorTab = "prepare";
  view.rerender(<ThreadInspector threadID="thread-a" />);

  expect(screen.getByText("Prepare Status")).toBeTruthy();
  expect(screen.getAllByText("failed").length).toBeGreaterThan(0);
  expect(screen.getByText("Draft unavailable.")).toBeTruthy();
  expect(screen.getByText("Loop A")).toBeTruthy();
  expect(screen.getByText("op=prepare timeout=30s")).toBeTruthy();
  expect(screen.getByText("upstream 502 from provider")).toBeTruthy();
  expect(screen.getByText("62.0s")).toBeTruthy();
});

test("renderer stream inspector renders route debug rows", () => {
  workbenchState.home = {
    inboxEntries: [
      {
        id: "entry-1",
        kind: "note",
        summaryText: "Atlas note"
      }
    ],
    threads: [],
    resourceCounts: { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 },
    aiState: {
      queue: {
        activeCount: 1,
        queueDepth: 1,
        maxConcurrent: 2,
        activeLabels: ["route:entry-1"],
        pendingLabels: ["resume:thread-a"],
        activeOperations: [
          { label: "route:entry-1", startedAt: 1710676820000, elapsedMS: 900 }
        ],
        pendingOperations: [
          { label: "resume:thread-a", enqueuedAt: 1710676820500, waitMS: 3000 }
        ]
      },
      activeOperations: [
        { label: "route:entry-1", startedAt: "2026-03-17T12:00:20.000Z", elapsedMS: 900 }
      ],
      routeDebugByEntryID: {
        "entry-1": {
          status: "failed",
          decisionReason: "AI response is not valid JSON",
          rawErrorMessage: "Unexpected token < in JSON at position 0",
          responseModelID: "mock-model",
          finishReason: "stop",
          elapsedMS: 900,
          updatedAt: "2026-03-17T10:00:00.000Z"
        }
      }
    }
  };

  render(<StreamInspector />);

  expect(screen.getByText("Route Debug")).toBeTruthy();
  expect(screen.getByText("Atlas note")).toBeTruthy();
  expect(screen.getByText("AI response is not valid JSON")).toBeTruthy();
  expect(screen.getByText("mock-model · stop")).toBeTruthy();
  expect(screen.getByText("AI Queue")).toBeTruthy();
  expect(screen.getByText("route:entry-1 · 900ms")).toBeTruthy();
  expect(screen.getByText("resume:thread-a")).toBeTruthy();
});
