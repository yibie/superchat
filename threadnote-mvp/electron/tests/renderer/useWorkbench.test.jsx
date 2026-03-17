import { renderHook, act, waitFor } from "@testing-library/react";
import { beforeEach, expect, test, vi } from "vitest";

const mocks = vi.hoisted(() => {
  let threadUpdatedListener = null;
  let workbenchUpdatedListener = null;
  return {
    ipcMock: {
      getWorkbenchState: vi.fn(async () => ({ workspace: null, home: null })),
      createWorkspace: vi.fn(async () => ({})),
      openWorkspace: vi.fn(async () => ({})),
      submitCapture: vi.fn(async () => ({})),
      appendReply: vi.fn(async () => ({})),
      updateEntryText: vi.fn(async () => ({})),
      updateEntryKind: vi.fn(async () => ({})),
      deleteEntry: vi.fn(async () => ({})),
      routeEntryToThread: vi.fn(async () => ({})),
      createThread: vi.fn(async () => ({})),
      createThreadFromEntry: vi.fn(async () => ({})),
      archiveThread: vi.fn(async () => ({})),
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
  mocks.ipcMock.submitCapture.mockClear();
  mocks.ipcMock.appendReply.mockClear();
  mocks.ipcMock.updateEntryText.mockClear();
  mocks.ipcMock.updateEntryKind.mockClear();
  mocks.ipcMock.deleteEntry.mockClear();
  mocks.ipcMock.routeEntryToThread.mockClear();
  mocks.ipcMock.createThread.mockClear();
  mocks.ipcMock.createThreadFromEntry.mockClear();
  mocks.ipcMock.archiveThread.mockClear();
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
          inboxEntries: [{ id: "entry-1", summaryText: "Fresh capture" }],
          allEntries: [{ id: "entry-1", summaryText: "Fresh capture" }],
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

  expect(result.current.home.inboxEntries[0].id).toBe("entry-1");
  expect(result.current.getThreadDetail("thread-b").thread.title).toBe("Beta");
});
