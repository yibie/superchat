import { useState, useEffect, useCallback, useRef } from "react";
import { unstable_batchedUpdates } from "react-dom";
import { ipc } from "../lib/ipc.js";
import {
  mergeEntriesPage,
  normalizeEntriesPage,
  upsertThreadDetail
} from "./threadDetailState.js";

function emptyPage() {
  return {
    items: [],
    nextCursor: null,
    hasMore: false,
    totalCount: 0
  };
}

function deriveLegacyStreamPage(home) {
  const allEntries = Array.isArray(home?.allEntries) ? home.allEntries : [];
  const inboxEntries = Array.isArray(home?.inboxEntries) ? home.inboxEntries : [];
  const source = allEntries.length > 0 ? allEntries : inboxEntries;
  const items = source
    .filter(Boolean)
    .slice()
    .sort((lhs, rhs) => new Date(rhs.createdAt ?? 0).getTime() - new Date(lhs.createdAt ?? 0).getTime());
  return {
    ...emptyPage(),
    items,
    totalCount: items.length
  };
}

function mergePage(currentPage, incomingPage, { replaceHead = false } = {}) {
  return mergeEntriesPage(currentPage ?? emptyPage(), incomingPage ?? emptyPage(), { replaceHead });
}

function mergeHomeState(currentHome, incomingHome) {
  if (!incomingHome) {
    return currentHome;
  }
  const incomingStreamPage = incomingHome.streamPage ?? deriveLegacyStreamPage(incomingHome);
  const nextStreamPage = mergePage(currentHome?.streamPage, incomingStreamPage, { replaceHead: true });

  return {
    ...(currentHome ?? {}),
    ...incomingHome,
    streamPage: nextStreamPage
  };
}

function patchEntry(existing, nextEntry) {
  if (!nextEntry?.id) {
    return existing;
  }
  let found = false;
  const items = existing.items.map((entry) => {
    if (entry.id !== nextEntry.id) {
      return entry;
    }
    found = true;
    return nextEntry;
  });
  if (found) {
    return { ...existing, items };
  }
  return {
    ...existing,
    items: [nextEntry, ...items.filter((entry) => entry.id !== nextEntry.id)],
    totalCount: found ? (existing.totalCount ?? 0) : (existing.totalCount ?? 0) + 1
  };
}

function appendIncomingBacklink(existing, targetEntryID, sourceEntry) {
  if (!targetEntryID || !sourceEntry?.id) {
    return existing;
  }

  let changed = false;
  const items = existing.items.map((entry) => {
    if (entry.id !== targetEntryID) {
      return entry;
    }

    const nextBacklink = {
      id: `${sourceEntry.id}:${targetEntryID}:responds-to`,
      sourceEntryID: sourceEntry.id,
      sourceThreadID: sourceEntry.threadID ?? null,
      sourceSummaryText: sourceEntry.summaryText ?? sourceEntry.body?.text ?? "note",
      relationKind: "responds-to"
    };
    const currentBacklinks = Array.isArray(entry.incomingBacklinks) ? entry.incomingBacklinks : [];
    const duplicate = currentBacklinks.some((backlink) =>
      backlink?.sourceEntryID === nextBacklink.sourceEntryID &&
      (backlink?.relationKind ?? null) === nextBacklink.relationKind
    );

    if (duplicate) {
      return entry;
    }

    changed = true;
    return {
      ...entry,
      incomingBacklinks: [nextBacklink, ...currentBacklinks]
    };
  });

  return changed ? { ...existing, items } : existing;
}

function removeEntry(existing, entryID) {
  if (!entryID) {
    return existing;
  }
  const nextItems = existing.items.filter((entry) => entry.id !== entryID);
  const removedTopLevel = existing.items.length !== nextItems.length;
  return {
    ...existing,
    items: nextItems,
    totalCount: removedTopLevel ? Math.max(0, (existing.totalCount ?? 0) - 1) : existing.totalCount
  };
}

function removeEntryTree(existing, entryIDs = []) {
  const ids = new Set((entryIDs ?? []).filter(Boolean));
  if (ids.size === 0) {
    return existing;
  }
  const nextItems = existing.items.filter((entry) => !ids.has(entry.id));
  const removedTopLevel = existing.items.length - nextItems.length;
  return {
    ...existing,
    items: nextItems,
    totalCount: Math.max(0, (existing.totalCount ?? 0) - removedTopLevel)
  };
}

function extractTree(existing, entryIDs = []) {
  const ids = new Set((entryIDs ?? []).filter(Boolean));
  if (ids.size === 0) {
    return { items: [] };
  }
  return {
    items: existing.items.filter((entry) => ids.has(entry.id))
  };
}

function prependSubtree(existing, subtree, targetThreadID) {
  const items = (subtree?.items ?? []).map((entry) => ({ ...entry, threadID: targetThreadID }));
  const itemIDs = new Set(items.map((entry) => entry.id));
  const addedTopLevel = items.filter((entry) => !existing.items.some((current) => current.id === entry.id)).length;
  return {
    ...existing,
    items: [...items, ...existing.items.filter((entry) => !itemIDs.has(entry.id))],
    totalCount: (existing.totalCount ?? existing.items.length) + addedTopLevel
  };
}

export function useWorkbench() {
  const [workspace, setWorkspace] = useState(null);
  const [home, setHome] = useState(null);
  const [threadDetailsByID, setThreadDetailsByID] = useState({});
  const [threadLoadingByID, setThreadLoadingByID] = useState({});
  const [streamLoadingMore, setStreamLoadingMore] = useState(false);
  const [threadPageLoadingByID, setThreadPageLoadingByID] = useState({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const mountedRef = useRef(true);
  const openThreadTokensRef = useRef(new Map());
  const threadDetailsRef = useRef({});
  const streamLoadTokenRef = useRef(null);
  const threadPageTokensRef = useRef(new Map());

  useEffect(() => {
    mountedRef.current = true;
    return () => { mountedRef.current = false; };
  }, []);

  useEffect(() => {
    threadDetailsRef.current = threadDetailsByID;
  }, [threadDetailsByID]);

  const applyWorkbench = useCallback((wb) => {
    if (!mountedRef.current || !wb) return;
    if (wb.workspace !== undefined) setWorkspace(wb.workspace);
    if (wb.home !== undefined) {
      setHome((current) => mergeHomeState(current, wb.home));
    }
  }, []);

  const mergeThreadDetail = useCallback((detail) => {
    if (!mountedRef.current || !detail?.thread?.id) {
      return;
    }
    setThreadDetailsByID((current) => upsertThreadDetail(current, detail));
    setThreadLoadingByID((current) => ({
      ...current,
      [detail.thread.id]: false
    }));
    setThreadPageLoadingByID((current) => ({
      ...current,
      [detail.thread.id]: false
    }));
  }, []);

  const applyWorkbenchPayload = useCallback((payload) => {
    if (!mountedRef.current || !payload) {
      return;
    }
    const workbench = payload?.workbench ?? payload;
    const thread = payload?.thread;

    unstable_batchedUpdates(() => {
      applyWorkbench(workbench);
      mergeThreadDetail(thread);
    });
  }, [applyWorkbench, mergeThreadDetail]);

  const refresh = useCallback(async () => {
    try {
      const wb = await ipc.getWorkbenchState();
      applyWorkbench(wb);
    } catch (err) {
      if (mountedRef.current) setError(err.message);
    }
  }, [applyWorkbench]);

  useEffect(() => {
    (async () => {
      try {
        const wb = await ipc.getWorkbenchState();
        applyWorkbench(wb);
      } catch (err) {
        if (mountedRef.current) setError(err.message);
      } finally {
        if (mountedRef.current) setLoading(false);
      }
    })();
  }, [applyWorkbench]);

  useEffect(() => {
    const unsubscribe = ipc.onThreadUpdated?.((payload) => {
      if (!mountedRef.current || !payload?.threadID || !payload?.thread?.thread?.id) {
        return;
      }
      if (payload.thread.thread.id !== payload.threadID) {
        return;
      }
      mergeThreadDetail(payload.thread);
    });
    return () => unsubscribe?.();
  }, [mergeThreadDetail]);

  useEffect(() => {
    const unsubscribe = ipc.onWorkbenchUpdated?.((payload) => {
      if (!mountedRef.current || !payload?.workbench) {
        return;
      }
      applyWorkbenchPayload(payload);
    });
    return () => unsubscribe?.();
  }, [applyWorkbenchPayload]);

  const createWorkspace = useCallback(async () => {
    const wb = await ipc.createWorkspace();
    applyWorkbench(wb);
    return wb;
  }, [applyWorkbench]);

  const openWorkspace = useCallback(async () => {
    const wb = await ipc.openWorkspace();
    applyWorkbench(wb);
    return wb;
  }, [applyWorkbench]);

  const loadMoreStream = useCallback(async () => {
    const currentPage = home?.streamPage ?? emptyPage();
    if (!currentPage.hasMore || streamLoadingMore) {
      return null;
    }
    const token = `${Date.now()}-${Math.random()}`;
    streamLoadTokenRef.current = token;
    setStreamLoadingMore(true);
    try {
      const nextPage = await ipc.getStreamPage({
        cursor: currentPage.nextCursor,
        limit: 50
      });
      if (streamLoadTokenRef.current !== token || !mountedRef.current) {
        return nextPage;
      }
      setHome((current) => ({
        ...(current ?? {}),
        streamPage: mergePage(current?.streamPage, nextPage)
      }));
      return nextPage;
    } finally {
      if (streamLoadTokenRef.current === token && mountedRef.current) {
        setStreamLoadingMore(false);
      }
    }
  }, [home?.streamPage, streamLoadingMore]);

  const loadMoreThread = useCallback(async (threadID) => {
    if (!threadID) {
      return null;
    }
    const detail = threadDetailsRef.current[threadID];
    const currentPage = normalizeEntriesPage(detail);
    if (!currentPage.hasMore || threadPageLoadingByID[threadID]) {
      return null;
    }

    const token = `${Date.now()}-${Math.random()}`;
    threadPageTokensRef.current.set(threadID, token);
    setThreadPageLoadingByID((current) => ({ ...current, [threadID]: true }));
    try {
      const nextDetail = await ipc.getThreadPage({
        threadID,
        cursor: currentPage.nextCursor,
        limit: 50
      });
      if (threadPageTokensRef.current.get(threadID) !== token || !mountedRef.current) {
        return nextDetail;
      }
      setThreadDetailsByID((current) => {
        const existing = current[threadID] ?? null;
        const incomingPage = normalizeEntriesPage(nextDetail);
        const mergedPage = mergePage(existing?.entriesPage, incomingPage);
        return {
          ...current,
          [threadID]: {
            ...(existing ?? {}),
            ...nextDetail,
            entriesPage: mergedPage,
            entries: mergedPage.items
          }
        };
      });
      return nextDetail;
    } finally {
      if (threadPageTokensRef.current.get(threadID) === token && mountedRef.current) {
        setThreadPageLoadingByID((current) => ({ ...current, [threadID]: false }));
      }
    }
  }, [threadPageLoadingByID]);

  const submitCapture = useCallback(async (payload) => {
    const res = await ipc.submitCapture(payload);
    applyWorkbenchPayload(res);
    if (res?.result?.entry) {
      setHome((current) => current ? ({
        ...current,
        streamPage: patchEntry(current.streamPage ?? emptyPage(), res.result.entry)
      }) : current);
    }
    return res;
  }, [applyWorkbenchPayload]);

  const appendReply = useCallback(async (payload) => {
    const res = await ipc.appendReply(payload);
    applyWorkbenchPayload(res);
    const targetEntryID = payload?.entryID ?? null;
    const linkedEntry = res?.result?.entry ?? null;

    if (targetEntryID && linkedEntry) {
      setHome((current) => current ? ({
        ...current,
        streamPage: appendIncomingBacklink(
          patchEntry(current.streamPage ?? emptyPage(), linkedEntry),
          targetEntryID,
          linkedEntry
        )
      }) : current);
    }

    if (res?.result?.entry?.threadID) {
      setThreadDetailsByID((current) => {
        const threadID = res.result.entry.threadID;
        const existing = current[threadID];
        if (!existing) {
          return current;
        }
        const entriesPage = appendIncomingBacklink(
          patchEntry(existing.entriesPage ?? emptyPage(), res.result.entry),
          targetEntryID,
          res.result.entry
        );
        return {
          ...current,
          [threadID]: {
            ...existing,
            entriesPage,
            entries: entriesPage.items
          }
        };
      });
    }
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryText = useCallback(async (payload) => {
    const res = await ipc.updateEntryText(payload);
    applyWorkbenchPayload(res);
    if (res?.result?.entry) {
      setHome((current) => current ? ({
        ...current,
        streamPage: patchEntry(current.streamPage ?? emptyPage(), res.result.entry)
      }) : current);
    }
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryKind = useCallback(async (payload) => {
    const res = await ipc.updateEntryKind(payload);
    if (!res?.result?.entry?.id) {
      throw new Error("Entry kind update returned no entry payload");
    }
    applyWorkbenchPayload(res);
    setHome((current) => current ? ({
      ...current,
      streamPage: patchEntry(current.streamPage ?? emptyPage(), res.result.entry)
    }) : current);
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryStatus = useCallback(async (payload) => {
    const res = await ipc.updateEntryStatus(payload);
    if (!res?.result?.entry?.id) {
      throw new Error("Entry status update returned no entry payload");
    }
    applyWorkbenchPayload(res);
    setHome((current) => current ? ({
      ...current,
      streamPage: patchEntry(current.streamPage ?? emptyPage(), res.result.entry)
    }) : current);
    return res;
  }, [applyWorkbenchPayload]);

  const deleteEntry = useCallback(async (entryID) => {
    const res = await ipc.deleteEntry(entryID);
    applyWorkbenchPayload(res);
    setHome((current) => current ? ({
      ...current,
      streamPage: removeEntry(current.streamPage ?? emptyPage(), entryID)
    }) : current);
    setThreadDetailsByID((current) => {
      const next = {};
      for (const [threadID, detail] of Object.entries(current)) {
        const entriesPage = removeEntry(detail.entriesPage ?? emptyPage(), entryID);
        next[threadID] = {
          ...detail,
          entriesPage,
          entries: entriesPage.items
        };
      }
      return next;
    });
    return res;
  }, [applyWorkbenchPayload]);

  const routeEntryToThread = useCallback(async (payload) => {
    const res = await ipc.routeEntryToThread(payload);
    applyWorkbenchPayload(res);
    if (payload?.entryID) {
      if (res?.result?.entry) {
        setHome((current) => current ? ({
          ...current,
          streamPage: patchEntry(current.streamPage ?? emptyPage(), res.result.entry)
        }) : current);
      }
      setThreadDetailsByID((current) => {
        const movedEntryIDs = res?.result?.movedEntryIDs ?? [payload.entryID];
        const targetThreadID = res?.result?.targetThreadID ?? payload.threadID ?? null;
        const next = { ...current };
        let subtree = { items: [] };

        for (const [cachedThreadID, detail] of Object.entries(current)) {
          const existingPage = detail.entriesPage ?? emptyPage();
          const extracted = extractTree(existingPage, movedEntryIDs);
          if (extracted.items.length > 0) {
            subtree = extracted;
          }
          const entriesPage = removeEntryTree(existingPage, movedEntryIDs);
          next[cachedThreadID] = {
            ...detail,
            entriesPage,
            entries: entriesPage.items
          };
        }

        if (targetThreadID && next[targetThreadID] && subtree.items.length > 0) {
          const targetDetail = next[targetThreadID];
          const entriesPage = prependSubtree(targetDetail.entriesPage ?? emptyPage(), subtree, targetThreadID);
          next[targetThreadID] = {
            ...targetDetail,
            entriesPage,
            entries: entriesPage.items
          };
        }

        return next;
      });
    }
    return res;
  }, [applyWorkbenchPayload]);

  const createThread = useCallback(async (payload) => {
    const res = await ipc.createThread(payload);
    applyWorkbench(res);
    return res;
  }, [applyWorkbench]);

  const createThreadFromEntry = useCallback(async (payload) => {
    const res = await ipc.createThreadFromEntry(payload);
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const archiveThread = useCallback(async (threadID) => {
    const res = await ipc.archiveThread(threadID);
    applyWorkbench(res?.workbench ?? res);
    setThreadDetailsByID((current) => {
      if (!threadID || !(threadID in current)) {
        return current;
      }
      const next = { ...current };
      delete next[threadID];
      return next;
    });
    return res;
  }, [applyWorkbench]);

  const updateThreadTitle = useCallback(async (payload) => {
    const res = await ipc.updateThreadTitle(payload);
    if (!res?.thread?.thread?.id) {
      throw new Error("Thread title update returned no thread payload");
    }
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const openThread = useCallback(async (threadID) => {
    if (!threadID) {
      return null;
    }
    const token = `${Date.now()}-${Math.random()}`;
    openThreadTokensRef.current.set(threadID, token);
    setThreadLoadingByID((current) => ({
      ...current,
      [threadID]: true
    }));
    try {
      const res = await ipc.openThread(threadID);
      applyWorkbench(res?.workbench ?? res);
      if (!mountedRef.current) {
        return res;
      }
      const currentToken = openThreadTokensRef.current.get(threadID);
      const nextThread = res?.thread ?? null;
      const accepted = Boolean(
        currentToken === token &&
        nextThread?.thread?.id &&
        nextThread.thread.id === threadID
      );
      if (accepted) {
        setThreadDetailsByID((current) => upsertThreadDetail(current, nextThread));
      }
      setThreadLoadingByID((current) => ({
        ...current,
        [threadID]: false
      }));
      return res;
    } catch (err) {
      setThreadLoadingByID((current) => ({
        ...current,
        [threadID]: false
      }));
      throw err;
    }
  }, [applyWorkbench]);

  const prepareThread = useCallback(async (payload) => {
    const res = await ipc.prepareThread(payload);
    return res;
  }, []);

  const getThreadDetail = useCallback((threadID) => {
    if (!threadID) {
      return null;
    }
    return threadDetailsByID[threadID] ?? null;
  }, [threadDetailsByID]);

  const isThreadLoading = useCallback((threadID) => {
    if (!threadID) {
      return false;
    }
    return Boolean(threadLoadingByID[threadID]);
  }, [threadLoadingByID]);

  const isThreadPageLoading = useCallback((threadID) => {
    if (!threadID) {
      return false;
    }
    return Boolean(threadPageLoadingByID[threadID]);
  }, [threadPageLoadingByID]);

  return {
    workspace,
    home,
    thread: null,
    threadDetailsByID,
    loading,
    error,
    streamLoadingMore,
    refresh, createWorkspace, openWorkspace,
    submitCapture, appendReply, updateEntryText, deleteEntry,
    updateEntryKind, updateEntryStatus,
    routeEntryToThread, createThread, createThreadFromEntry, archiveThread, updateThreadTitle,
    openThread, prepareThread, loadMoreStream, loadMoreThread,
    getThreadDetail,
    isThreadLoading,
    isThreadPageLoading
  };
}
