import { renderHook, act, waitFor } from "@testing-library/react";
import { beforeEach, expect, test, vi } from "vitest";

const mocks = vi.hoisted(() => {
  let threadUpdatedListener = null;
  let workbenchUpdatedListener = null;
  return {
    ipcMock: {
      getWorkbenchState: vi.fn(async () => ({ workspace: null, home: null })),
      getStreamPage: vi.fn(async () => ({ items: [], replies: [], hasMore: false, nextCursor: null, totalCount: 0 })),
      getThreadPage: vi.fn(async () => ({ thread: { id: "thread-b" }, entriesPage: { items: [], replies: [], hasMore: false, nextCursor: null, totalCount: 0 } })),
      createWorkspace: vi.fn(async () => ({})),
      openWorkspace: vi.fn(async () => ({})),
      submitCapture: vi.fn(async () => ({})),
      appendReply: vi.fn(async () => ({})),
      updateEntryText: vi.fn(async () => ({})),
      updateEntryKind: vi.fn(async () => ({})),
      updateEntryStatus: vi.fn(async () => ({})),
      deleteEntry: vi.fn(async () => ({})),
      routeEntryToThread: vi.fn(async () => ({})),
      createThread: vi.fn(async () => ({})),
      createThreadFromEntry: vi.fn(async () => ({})),
      archiveThread: vi.fn(async () => ({})),
      updateThreadTitle: vi.fn(async () => ({})),
      openThread: vi.fn(),
      prepareThread: vi.fn(async () => ({})),
      onThreadUpdated: vi.fn((callback) => {
        threadUpdatedListener = callback;
        return () => {
          threadUpdatedListener = null;
        };
      }),
      onWorkbenchUpdated: vi.fn((callback) => {
        workbenchUpdatedListener = callback;
        return () => {
          workbenchUpdatedListener = null;
        };
      })
    },
    getThreadUpdatedListener: () => threadUpdatedListener,
    getWorkbenchUpdatedListener: () => workbenchUpdatedListener,
    clearThreadUpdatedListener: () => {
      threadUpdatedListener = null;
    },
    clearWorkbenchUpdatedListener: () => {
      workbenchUpdatedListener = null;
    }
  };
});

vi.mock("../../renderer/src/lib/ipc.js", () => ({
  ipc: mocks.ipcMock
}));

const { useWorkbench } = await import("../../renderer/src/hooks/useWorkbench.js");

function deferred() {
  let resolve;
  let reject;
  const promise = new Promise((res, rej) => {
    resolve = res;
    reject = rej;
  });
  return { promise, resolve, reject };
}

beforeEach(() => {
  mocks.clearThreadUpdatedListener();
  mocks.clearWorkbenchUpdatedListener();
  mocks.ipcMock.getWorkbenchState.mockClear();
  mocks.ipcMock.createWorkspace.mockClear();
  mocks.ipcMock.openWorkspace.mockClear();
  mocks.ipcMock.getStreamPage.mockClear();
  mocks.ipcMock.getThreadPage.mockClear();
  mocks.ipcMock.submitCapture.mockClear();
  mocks.ipcMock.appendReply.mockClear();
  mocks.ipcMock.updateEntryText.mockClear();
  mocks.ipcMock.updateEntryKind.mockClear();
  mocks.ipcMock.updateEntryStatus.mockClear();
  mocks.ipcMock.deleteEntry.mockClear();
  mocks.ipcMock.routeEntryToThread.mockClear();
  mocks.ipcMock.createThread.mockClear();
  mocks.ipcMock.createThreadFromEntry.mockClear();
  mocks.ipcMock.archiveThread.mockClear();
  mocks.ipcMock.updateThreadTitle.mockClear();
  mocks.ipcMock.prepareThread.mockClear();
  mocks.ipcMock.onThreadUpdated.mockClear();
  mocks.ipcMock.onWorkbenchUpdated.mockClear();
  mocks.ipcMock.openThread.mockReset();
  mocks.ipcMock.getWorkbenchState.mockResolvedValue({ workspace: null, home: null });
});

test("renderer useWorkbench keeps only the latest response for repeated openThread calls on the same thread", async () => {
  const first = deferred();
  const second = deferred();
  mocks.ipcMock.openThread
    .mockImplementationOnce(() => first.promise)
    .mockImplementationOnce(() => second.promise);

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  let openA;
  let openB;
  act(() => {
    openA = result.current.openThread("thread-b");
    openB = result.current.openThread("thread-b");
  });

  await act(async () => {
    second.resolve({
      thread: {
        thread: { id: "thread-b", title: "Latest" },
        entries: []
      }
    });
    await openB;
  });

  expect(result.current.getThreadDetail("thread-b").thread.title).toBe("Latest");

  await act(async () => {
    first.resolve({
      thread: {
        thread: { id: "thread-b", title: "Stale" },
        entries: []
      }
    });
    await openA;
  });

  expect(result.current.getThreadDetail("thread-b").thread.title).toBe("Latest");
});

test("renderer useWorkbench drops mismatched openThread payloads", async () => {
  mocks.ipcMock.openThread.mockResolvedValue({
    thread: {
      thread: { id: "thread-a", title: "Wrong thread" },
      entries: []
    }
  });

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    await result.current.openThread("thread-b");
  });

  expect(result.current.getThreadDetail("thread-b")).toBeNull();
});

test("renderer useWorkbench merges background thread-updated payloads by thread id", async () => {
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    mocks.getThreadUpdatedListener()?.({
      threadID: "thread-b",
      thread: {
        thread: { id: "thread-b", title: "Beta" },
        entries: []
      }
    });
  });

  expect(result.current.getThreadDetail("thread-b").thread.title).toBe("Beta");
});

test("renderer useWorkbench ignores background thread-updated payloads whose thread id mismatches the key", async () => {
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    mocks.getThreadUpdatedListener()?.({
      threadID: "thread-b",
      thread: {
        thread: { id: "thread-a", title: "Wrong thread" },
        entries: []
      }
    });
  });

  expect(result.current.getThreadDetail("thread-b")).toBeNull();
  expect(result.current.getThreadDetail("thread-a")).toBeNull();
});

test("renderer useWorkbench applies background workbench updates without requiring navigation", async () => {
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    mocks.getWorkbenchUpdatedListener()?.({
      workbench: {
        workspace: { workspacePath: "/tmp/Atlas" },
        home: {
          streamPage: {
            items: [{ id: "entry-1", summaryText: "Fresh capture" }],
            replies: [],
            hasMore: false,
            nextCursor: null,
            totalCount: 1
          },
          threads: []
        }
      },
      threadID: "thread-b",
      thread: {
        thread: { id: "thread-b", title: "Beta" },
        entries: []
      }
    });
  });

  expect(result.current.home.streamPage.items[0].id).toBe("entry-1");
  expect(result.current.getThreadDetail("thread-b").thread.title).toBe("Beta");
});

test("renderer useWorkbench derives streamPage from legacy home payloads", async () => {
  mocks.ipcMock.getWorkbenchState.mockResolvedValueOnce({
    workspace: { workspacePath: "/tmp/Atlas" },
    home: {
      inboxEntries: [
        { id: "entry-1", summaryText: "Legacy entry", createdAt: "2026-03-18T00:00:00Z", parentEntryID: null }
      ],
      allEntries: [
        { id: "entry-1", summaryText: "Legacy entry", createdAt: "2026-03-18T00:00:00Z", parentEntryID: null },
        { id: "reply-1", summaryText: "Legacy reply", createdAt: "2026-03-18T00:01:00Z", parentEntryID: "entry-1" }
      ],
      threads: []
    }
  });

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  expect(result.current.home.streamPage.items.map((entry) => entry.id)).toEqual(["reply-1", "entry-1"]);
  expect(result.current.home.streamPage.replies).toBeUndefined();
  expect(result.current.home.streamPage.totalCount).toBe(2);
});

test("renderer useWorkbench surfaces updateEntryKind transport failures", async () => {
  mocks.ipcMock.updateEntryKind.mockResolvedValueOnce(null);
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await expect(
    act(async () => {
      await result.current.updateEntryKind({ entryID: "entry-1", kind: "question" });
    })
  ).rejects.toThrow(/returned no entry payload/i);
});

test("renderer useWorkbench surfaces updateEntryStatus transport failures", async () => {
  mocks.ipcMock.updateEntryStatus.mockResolvedValueOnce(null);
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await expect(
    act(async () => {
      await result.current.updateEntryStatus({ entryID: "entry-1", status: "solved" });
    })
  ).rejects.toThrow(/returned no entry payload/i);
});

test("renderer useWorkbench applies updateThreadTitle payloads", async () => {
  mocks.ipcMock.updateThreadTitle.mockResolvedValueOnce({
    workbench: {
      workspace: null,
      home: {
        streamPage: {
          items: [],
          replies: [],
          hasMore: false,
          nextCursor: null,
          totalCount: 0
        },
        threads: [{ id: "thread-a", title: "Renamed" }]
      }
    },
    thread: {
      thread: { id: "thread-a", title: "Renamed" },
      entries: []
    }
  });

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    await result.current.updateThreadTitle({ threadID: "thread-a", title: "Renamed" });
  });

  expect(mocks.ipcMock.updateThreadTitle).toHaveBeenCalledWith({ threadID: "thread-a", title: "Renamed" });
  expect(result.current.getThreadDetail("thread-a").thread.title).toBe("Renamed");
});

test("renderer useWorkbench removes rerouted entries from the old thread and prepends them to the cached target thread", async () => {
  mocks.ipcMock.openThread
    .mockResolvedValueOnce({
        thread: {
          thread: { id: "thread-a", title: "Alpha" },
          entriesPage: {
          items: [{ id: "entry-1", threadID: "thread-a", summaryText: "Moved entry" }],
          hasMore: false,
          nextCursor: null,
          totalCount: 1
        }
      }
    })
    .mockResolvedValueOnce({
      thread: {
        thread: { id: "thread-b", title: "Beta" },
        entriesPage: {
          items: [{ id: "entry-2", threadID: "thread-b", summaryText: "Existing target entry" }],
          replies: [],
          hasMore: false,
          nextCursor: null,
          totalCount: 1
        }
      }
    });
  mocks.ipcMock.routeEntryToThread.mockResolvedValueOnce({
    workbench: {
      workspace: null,
      home: {
        streamPage: {
          items: [{ id: "entry-1", threadID: "thread-b", summaryText: "Moved entry" }],
          replies: [],
          hasMore: false,
          nextCursor: null,
          totalCount: 1
        },
        threads: []
      }
    },
    result: {
      movedEntryIDs: ["entry-1"],
      sourceThreadID: "thread-a",
      targetThreadID: "thread-b",
      entry: { id: "entry-1", threadID: "thread-b", summaryText: "Moved entry" }
    },
    thread: {
      thread: { id: "thread-b", title: "Beta" },
      entriesPage: {
        items: [{ id: "entry-1", threadID: "thread-b", summaryText: "Moved entry" }],
        hasMore: false,
        nextCursor: null,
        totalCount: 1
      }
    }
  });

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    mocks.getWorkbenchUpdatedListener()?.({
      workbench: {
        workspace: null,
        home: {
          streamPage: {
            items: [{ id: "entry-1", threadID: "thread-a", summaryText: "Moved entry" }],
            replies: [],
            hasMore: false,
            nextCursor: null,
            totalCount: 1
          },
          threads: []
        }
      }
    });
  });

  await act(async () => {
    await result.current.openThread("thread-a");
    await result.current.openThread("thread-b");
  });

  await act(async () => {
    await result.current.routeEntryToThread({ entryID: "entry-1", threadID: "thread-b" });
  });

  expect(result.current.getThreadDetail("thread-a").entriesPage.items.map((entry) => entry.id)).toEqual([]);
  expect(result.current.getThreadDetail("thread-b").entriesPage.items.map((entry) => entry.id)).toEqual(["entry-1", "entry-2"]);
  expect(result.current.home.streamPage.items.map((entry) => entry.id)).toEqual(["entry-1"]);
  expect(result.current.home.streamPage.items[0].threadID).toBe("thread-b");
});

test("renderer useWorkbench surfaces updateThreadTitle transport failures", async () => {
  mocks.ipcMock.updateThreadTitle.mockResolvedValueOnce(null);
  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await expect(
    act(async () => {
      await result.current.updateThreadTitle({ threadID: "thread-a", title: "Renamed" });
    })
  ).rejects.toThrow(/returned no thread payload/i);
});

test("renderer useWorkbench adds a backlink to the replied entry when a linked note is created", async () => {
  mocks.ipcMock.getWorkbenchState.mockResolvedValueOnce({
    workspace: { workspacePath: "/tmp/Atlas" },
    home: {
      streamPage: {
        items: [
          {
            id: "entry-1",
            summaryText: "Parent entry",
            createdAt: "2026-03-18T00:00:00Z",
            incomingBacklinks: []
          }
        ],
        hasMore: false,
        nextCursor: null,
        totalCount: 1
      },
      threads: []
    }
  });
  mocks.ipcMock.appendReply.mockResolvedValueOnce({
    result: {
      entry: {
        id: "entry-2",
        summaryText: "Follow-up note",
        createdAt: "2026-03-18T00:01:00Z",
        threadID: null,
        references: [
          {
            id: "reply:entry-1",
            relationKind: "responds-to",
            targetID: "entry-1",
            label: "Parent entry"
          }
        ]
      }
    },
    workbench: {
      workspace: { workspacePath: "/tmp/Atlas" },
      home: {
        streamPage: {
          items: [
            {
              id: "entry-2",
              summaryText: "Follow-up note",
              createdAt: "2026-03-18T00:01:00Z",
              threadID: null
            }
          ],
          hasMore: false,
          nextCursor: null,
          totalCount: 2
        },
        threads: []
      }
    },
    thread: null
  });

  const { result } = renderHook(() => useWorkbench());
  await waitFor(() => expect(result.current.loading).toBe(false));

  await act(async () => {
    await result.current.appendReply({ entryID: "entry-1", text: "Follow-up note" });
  });

  const parentEntry = result.current.home.streamPage.items.find((entry) => entry.id === "entry-1");
  const linkedEntry = result.current.home.streamPage.items.find((entry) => entry.id === "entry-2");

  expect(linkedEntry).toBeTruthy();
  expect(parentEntry.incomingBacklinks).toEqual([
    expect.objectContaining({
      sourceEntryID: "entry-2",
      relationKind: "responds-to",
      sourceSummaryText: "Follow-up note"
    })
  ]);
});
