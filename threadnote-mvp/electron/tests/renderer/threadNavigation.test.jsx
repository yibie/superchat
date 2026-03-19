import React from "react";
import { beforeEach, expect, test, vi } from "vitest";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";

let navigationState;
let workbenchState;
let entryActionsState;
let themeState;
let shortcutSettingsState;
let captureEditorRenderProps;

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
    openSettingsWindow: vi.fn(),
    getEntryRichPreview: vi.fn(async () => ({
      title: "Preview Title",
      image: null,
      url: "https://agent-trace.dev/",
      locator: "https://agent-trace.dev/"
    })),
    getLocatorRichPreview: vi.fn(async (locator) => ({
      title: locator.includes("docs.polymarket.com")
        ? "Docs Preview"
        : locator.includes("attachments/")
          ? "Attachment Preview"
          : "Preview Title",
      image: null,
      previewImageURL: locator.includes("attachments/") ? "file:///tmp/preview.jpg" : null,
      fileName: locator.includes("attachments/") ? "f101135c.jpg" : null,
      url: locator,
      locator
    })),
    openLocator: vi.fn()
  }
}));

vi.mock("../../renderer/src/components/editor/CaptureEditor.jsx", () => ({
  CaptureEditor: (props) => {
    captureEditorRenderProps.push(props);
    return (
      <button type="button" data-testid="capture-editor" onClick={() => props.onSubmit?.("note", [], [])}>
      Capture
      </button>
    );
  }
}));

const { ThreadBadge } = await import("../../renderer/src/components/entries/ThreadBadge.jsx");
const { EntryCard } = await import("../../renderer/src/components/entries/EntryCard.jsx");
const { EntryInlineBody } = await import("../../renderer/src/components/entries/EntryInlineBody.jsx");
const { EntryBacklinks } = await import("../../renderer/src/components/entries/EntryBacklinks.jsx");
const { deriveDisplayRows } = await import("../../renderer/src/components/entries/EntryList.jsx");
const { ThreadSurface } = await import("../../renderer/src/components/surfaces/ThreadSurface.jsx");
const { StreamSurface } = await import("../../renderer/src/components/surfaces/StreamSurface.jsx");
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
  captureEditorRenderProps = [];
  navigationState = {
    openThread: vi.fn(),
    focusEntry: vi.fn(),
    selectedThreadID: "thread-a",
    goBack: vi.fn(),
    goForward: vi.fn(),
    canGoBack: true,
    canGoForward: false,
    goToStream: vi.fn(),
    goToResources: vi.fn(),
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
    createThread: vi.fn(async () => ({})),
    createThreadFromEntry: vi.fn(async () => ({})),
    submitCapture: vi.fn(async () => null),
    archiveThread: vi.fn(async () => null),
    updateThreadTitle: vi.fn(async () => null),
    updateEntryStatus: vi.fn(async () => null)
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

test("renderer sidebar navigation buttons dispatch back and forward actions", () => {
  navigationState.surface = "thread";
  navigationState.canGoBack = true;
  navigationState.canGoForward = true;
  workbenchState.home = { threads: [] };

  render(<Sidebar />);

  fireEvent.click(screen.getByRole("button", { name: "Back" }));
  fireEvent.click(screen.getByRole("button", { name: "Forward" }));

  expect(navigationState.goBack).toHaveBeenCalled();
  expect(navigationState.goForward).toHaveBeenCalled();
});

test("renderer thread surface lets user rename the current thread inline", async () => {
  workbenchState.getThreadDetail = vi.fn(() => makeThreadDetail("thread-a", "Alpha"));

  render(<ThreadSurface />);

  fireEvent.click(screen.getByRole("button", { name: /alpha/i }));

  const input = screen.getByRole("textbox", { name: "Thread title" });
  fireEvent.change(input, { target: { value: "Renamed Alpha" } });
  fireEvent.keyDown(input, { key: "Enter" });

  await waitFor(() => {
    expect(workbenchState.updateThreadTitle).toHaveBeenCalledWith({
      threadID: "thread-a",
      title: "Renamed Alpha"
    });
  });
});

test("renderer thread surface does not show stage badge next to thread title", () => {
  workbenchState.getThreadDetail = vi.fn(() => ({
    ...makeThreadDetail("thread-a", "Alpha"),
    thread: {
      ...makeThreadDetail("thread-a", "Alpha").thread,
      goalLayer: { currentStage: "framing" }
    }
  }));

  render(<ThreadSurface />);

  expect(screen.getByRole("button", { name: "Alpha" })).toBeTruthy();
  expect(screen.queryByRole("button", { name: "Framing" })).toBeNull();
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

test("renderer entry route picker includes a + New Thread action", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        createdAt: "2026-03-19T10:00:00.000Z",
        summaryText: "Inbox note",
        body: { text: "Inbox note" },
        references: [],
        objectMentions: [],
        threadID: null
      }}
      entries={[]}
      allEntries={[]}
      threads={[
        { id: "thread-a", title: "Alpha", color: "sky" }
      ]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: "Move to thread" }));

  expect(screen.getByRole("button", { name: "+ New Thread" })).toBeTruthy();
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

test("renderer entry card keeps continue as a flat-entry action", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByRole("button", { name: /continue/i }));
  expect(entryActionsState.startReply).toHaveBeenCalledWith("entry-1");
});

test("renderer related entry renders as a normal flat card", () => {
  render(
    <EntryCard
      entry={{
        id: "related-entry-1",
        kind: "note",
        summaryText: "Follow-up note",
        createdAt: "2026-03-15T10:01:00.000Z",
        references: [
          {
            id: "ref-1",
            relationKind: "responds-to",
            label: "Parent entry",
            targetID: "entry-1",
            isResolved: true,
            targetSummaryText: "Parent entry"
          }
        ]
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText("Follow-up note")).toBeTruthy();
  expect(screen.getByRole("button", { name: /continue/i })).toBeTruthy();
});

test("renderer entry list derives inline replies without duplicating them as flat rows", () => {
  const rows = deriveDisplayRows([
    {
      id: "entry-a",
      kind: "note",
      summaryText: "Parent entry",
      createdAt: "2026-03-15T10:00:00.000Z",
      references: []
    },
    {
      id: "entry-b",
      kind: "note",
      summaryText: "Reply entry",
      createdAt: "2026-03-15T10:01:00.000Z",
      references: [{ id: "ref-b", relationKind: "responds-to", targetID: "entry-a" }]
    }
  ]);

  expect(rows).toHaveLength(1);
  expect(rows[0].entry.id).toBe("entry-a");
  expect(rows[0].replies.map((entry) => entry.id)).toEqual(["entry-b"]);
});

test("renderer entry list flattens nested replies into the parent reply lane", () => {
  const rows = deriveDisplayRows([
    {
      id: "entry-a",
      kind: "note",
      summaryText: "Parent entry",
      createdAt: "2026-03-15T10:00:00.000Z",
      references: []
    },
    {
      id: "entry-b",
      kind: "note",
      summaryText: "Reply entry",
      createdAt: "2026-03-15T10:01:00.000Z",
      references: [{ id: "ref-b", relationKind: "responds-to", targetID: "entry-a" }]
    },
    {
      id: "entry-c",
      kind: "note",
      summaryText: "Reply to reply",
      createdAt: "2026-03-15T10:02:00.000Z",
      references: [{ id: "ref-c", relationKind: "responds-to", targetID: "entry-b" }]
    }
  ]);

  expect(rows).toHaveLength(1);
  expect(rows[0].entry.id).toBe("entry-a");
  expect(rows[0].replies.map((entry) => entry.id)).toEqual(["entry-b", "entry-c"]);
});

test("renderer entry list falls back to flat rows when reply target is not loaded", () => {
  const rows = deriveDisplayRows([
    {
      id: "entry-b",
      kind: "note",
      summaryText: "Reply entry",
      createdAt: "2026-03-15T10:01:00.000Z",
      references: [{ id: "ref-b", relationKind: "responds-to", targetID: "missing-parent" }]
    }
  ]);

  expect(rows).toHaveLength(1);
  expect(rows[0].entry.id).toBe("entry-b");
  expect(rows[0].replies).toEqual([]);
});

test("renderer standalone reply entry keeps discussion styling when its target is not loaded", () => {
  const { container } = render(
    <EntryCard
      entry={{
        id: "entry-reply",
        kind: "claim",
        summaryText: "Standalone reply",
        createdAt: "2026-03-15T10:01:00.000Z",
        references: [{ id: "ref-b", relationKind: "responds-to", targetID: "missing-parent" }]
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  const card = container.querySelector('[data-entry-id="entry-reply"]');
  expect(card?.className).toContain("discussion-cluster");
  expect(card?.className).toContain("discussion-cluster-standalone-reply");
});

test("renderer entry card renders replies inline under the parent entry", () => {
  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      replies={[
        {
          id: "entry-2",
          kind: "note",
          summaryText: "Reply entry",
          createdAt: "2026-03-15T10:01:00.000Z",
          references: [{ id: "ref-1", relationKind: "responds-to", targetID: "entry-1" }]
        }
      ]}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText("Parent entry")).toBeTruthy();
  expect(screen.getByText("Reply entry")).toBeTruthy();
});

test("renderer entry card reuses capture editor for inline edit submits", () => {
  entryActionsState.editingEntryID = "entry-1";

  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByTestId("capture-editor"));

  expect(entryActionsState.saveEdit).toHaveBeenCalledWith("entry-1", "note", [], []);
});

test("renderer entry card keeps edit draft stable across parent rerenders", () => {
  entryActionsState.editingEntryID = "entry-1";

  const entry = {
    id: "entry-1",
    kind: "note",
    summaryText: "Parent entry",
    createdAt: "2026-03-15T10:00:00.000Z"
  };

  const { rerender } = render(
    <EntryCard
      entry={entry}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  const firstDraft = captureEditorRenderProps.at(-1)?.incomingDraft;

  rerender(
    <EntryCard
      entry={{ ...entry, createdAt: "2026-03-15T10:01:00.000Z" }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
      highlighted
    />
  );

  const secondDraft = captureEditorRenderProps.at(-1)?.incomingDraft;
  expect(secondDraft).toBe(firstDraft);
});

test("renderer reply composer reuses capture editor for replies", () => {
  entryActionsState.replyingToEntryID = "entry-1";

  render(
    <EntryCard
      entry={{
        id: "entry-1",
        kind: "note",
        summaryText: "Parent entry",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  fireEvent.click(screen.getByTestId("capture-editor"));

  expect(entryActionsState.submitReply).toHaveBeenCalledWith("entry-1", "note", [], []);
});

test("renderer entry card renders rich preview when url is mixed with text", async () => {
  render(
    <EntryCard
      entry={{
        id: "entry-mixed-url",
        kind: "source",
        summaryText: "https://agent-trace.dev/\n\nAgent Trace is useful.",
        createdAt: "2026-03-15T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText(/agent trace is useful/i)).toBeTruthy();
  expect(screen.queryByText("https://agent-trace.dev/")).toBeNull();
  await waitFor(() => {
    expect(screen.getByText("Preview Title")).toBeTruthy();
  });
});

test("renderer related entry card renders rich preview when url is mixed with text", async () => {
  render(
    <EntryCard
      entry={{
        id: "related-preview",
        kind: "source",
        summaryText: "https://agent-trace.dev/\n\nAgent Trace is useful in follow-ups.",
        createdAt: "2026-03-15T10:01:00.000Z",
        references: [
          {
            id: "ref-1",
            relationKind: "responds-to",
            label: "Parent entry",
            targetID: "entry-with-preview",
            isResolved: true,
            targetSummaryText: "Parent entry"
          }
        ]
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText(/agent trace is useful in follow-ups/i)).toBeTruthy();
  expect(screen.queryAllByText("https://agent-trace.dev/").length).toBe(0);
  await waitFor(() => {
    expect(screen.getByText("Preview Title")).toBeTruthy();
  });
});

test("renderer entry card renders only one link preview when multiple urls are present", async () => {
  render(
    <EntryCard
      entry={{
        id: "entry-multi-url",
        kind: "source",
        summaryText: "@Polymarket\nhttps://x.com/Polymarket\nhttps://docs.polymarket.com/api-reference/introduction",
        createdAt: "2026-03-18T07:52:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  await waitFor(() => {
    expect(screen.getByText("Preview Title")).toBeTruthy();
    expect(screen.getByText("Docs Preview")).toBeTruthy();
  });
  expect(screen.queryByText("https://x.com/Polymarket")).toBeNull();
  expect(screen.queryByText("https://docs.polymarket.com/api-reference/introduction")).toBeNull();
});

test("renderer rich preview falls back to a plain link card when metadata is unavailable", async () => {
  const { ipc } = await import("../../renderer/src/lib/ipc.js");
  ipc.getEntryRichPreview.mockImplementationOnce(async () => null);

  render(
    <EntryCard
      entry={{
        id: "entry-null-preview",
        kind: "source",
        summaryText: "https://example.org/no-meta",
        createdAt: "2026-03-18T08:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  await waitFor(() => {
    expect(screen.getAllByText("example.org").length).toBeGreaterThan(0);
  });
});

test("renderer entry card renders attachment preview when attachment locator is mixed with text", async () => {
  render(
    <EntryCard
      entry={{
        id: "entry-mixed-attachment",
        kind: "source",
        summaryText: "参考：Distill 的界面设计\nattachments/f101135c480c6d4ce49c726af01ec2a06c230f7225a74ef88512b7fb6215b598.jpg",
        createdAt: "2026-03-18T10:00:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  expect(screen.getByText(/参考：Distill 的界面设计/i)).toBeTruthy();
  expect(screen.queryByText(/attachments\/f101135c/i)).toBeNull();
  await waitFor(() => {
    expect(screen.getByText("Attachment Preview")).toBeTruthy();
  });
});

test("renderer rich preview hides broken preview images and keeps text card content", async () => {
  const { ipc } = await import("../../renderer/src/lib/ipc.js");
  const brokenPreview = {
    title: "Broken Image Preview",
    image: "file:///tmp/broken-preview.jpg",
    url: "https://example.com/broken-image",
    locator: "https://example.com/broken-image"
  };
  ipc.getEntryRichPreview.mockImplementationOnce(async () => brokenPreview);
  ipc.getLocatorRichPreview.mockImplementationOnce(async () => brokenPreview);

  const { container } = render(
    <EntryCard
      entry={{
        id: "entry-broken-image-preview",
        kind: "source",
        summaryText: "https://example.com/broken-image",
        createdAt: "2026-03-18T10:05:00.000Z"
      }}
      entries={[]}
      allEntries={[]}
      threads={[]}
      actions={entryActionsState}
    />
  );

  await waitFor(() => {
    expect(screen.getByText("Broken Image Preview")).toBeTruthy();
  });

  const image = container.querySelector('img[src="file:///tmp/broken-preview.jpg"]');
  expect(image).toBeTruthy();

  fireEvent.error(image);

  expect(container.querySelector('img[src="file:///tmp/broken-preview.jpg"]')).toBeNull();
  expect(screen.getByText("Broken Image Preview")).toBeTruthy();
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

test("renderer stream surface scrolls to the parent card when focusing a nested reply", async () => {
  navigationState.focusedEntryTarget = { entryID: "entry-reply", threadID: null };
  workbenchState.home = {
    threads: [],
    streamPage: {
      items: [
        {
          id: "entry-parent",
          kind: "note",
          summaryText: "Parent entry",
          createdAt: "2026-03-17T12:00:00.000Z",
          references: []
        },
        {
          id: "entry-reply",
          kind: "note",
          summaryText: "Reply entry",
          createdAt: "2026-03-17T12:01:00.000Z",
          references: [{ id: "ref-1", relationKind: "responds-to", targetID: "entry-parent" }]
        }
      ],
      hasMore: false,
      nextCursor: null,
      totalCount: 2
    }
  };

  const scrollIntoView = vi.fn();
  window.HTMLElement.prototype.scrollIntoView = scrollIntoView;

  render(<StreamSurface />);

  await waitFor(() => {
    expect(scrollIntoView).toHaveBeenCalled();
  });
  expect(navigationState.clearFocusedEntry).toHaveBeenCalled();
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
    statusSummary: {
      decided: [
        {
          id: "entry-1",
          threadID: "thread-a",
          kind: "question",
          status: "decided",
          summaryText: "We should move from chat to workspace.",
          updatedAt: "2026-03-18T08:10:00.000Z",
          source: "ai"
        }
      ],
      solved: [],
      verified: [],
      dropped: []
    },
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
  expect(screen.getByText("Current Stage")).toBeTruthy();
  expect(screen.getByText("Working")).toBeTruthy();
  expect(
    screen.getByText(/actively in progress, but the current stage has not been categorized/i)
  ).toBeTruthy();
  expect(screen.queryByRole("button", { name: "Status" })).toBeNull();
  expect(screen.getByRole("heading", { name: "Thread Outcomes" })).toBeTruthy();
  expect(screen.getByRole("heading", { name: "Decisions" })).toBeTruthy();
  expect(screen.getByText("We should move from chat to workspace.")).toBeTruthy();
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

test("renderer thread inspector renders status summary and allows manual status updates", async () => {
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
      headline: "Restart",
      restartNote: "Resume from the latest outcomes."
    },
    statusSummary: {
      decided: [
        {
          id: "entry-1",
          threadID: "thread-a",
          kind: "question",
          status: "decided",
          summaryText: "We should move from chat to workspace.",
          updatedAt: "2026-03-18T08:10:00.000Z",
          source: "ai"
        }
      ],
      solved: [],
      verified: [
        {
          id: "entry-2",
          threadID: "thread-a",
          kind: "claim",
          status: "verified",
          summaryText: "The prototype now validates the interaction model.",
          updatedAt: "2026-03-18T08:12:00.000Z",
          source: "manual"
        }
      ],
      dropped: []
    }
  }));

  render(<ThreadInspector threadID="thread-a" />);

  expect(screen.queryByRole("button", { name: "Status" })).toBeNull();
  expect(screen.getByRole("heading", { name: "Thread Outcomes" })).toBeTruthy();
  expect(screen.getByRole("heading", { name: "Decisions" })).toBeTruthy();
  expect(screen.getByRole("heading", { name: "Verified" })).toBeTruthy();
  expect(screen.getByText("We should move from chat to workspace.")).toBeTruthy();

  fireEvent.click(screen.getByRole("button", { name: /we should move from chat to workspace/i }));
  expect(navigationState.focusEntry).toHaveBeenCalledWith("entry-1", { threadID: "thread-a" });

  fireEvent.change(screen.getByLabelText(/update status for we should move/i), {
    target: { value: "open" }
  });

  await waitFor(() => {
    expect(workbenchState.updateEntryStatus).toHaveBeenCalledWith({
      entryID: "entry-1",
      status: "open"
    });
  });
  expect(workbenchState.openThread).toHaveBeenCalledWith("thread-a");
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
