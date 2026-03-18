import { useState, useEffect, useCallback, useRef } from "react";
import { unstable_batchedUpdates } from "react-dom";
import { ipc } from "../lib/ipc.js";
import { resolveOpenThreadResult, upsertThreadDetail } from "./threadDetailState.js";

export function useWorkbench() {
  const [workspace, setWorkspace] = useState(null);
  const [home, setHome] = useState(null);
  const [threadDetailsByID, setThreadDetailsByID] = useState({});
  const [threadLoadingByID, setThreadLoadingByID] = useState({});
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const mountedRef = useRef(true);
  const openThreadTokensRef = useRef(new Map());
  const threadDetailsRef = useRef({});
  const threadLoadingRef = useRef({});

  useEffect(() => {
    mountedRef.current = true;
    return () => { mountedRef.current = false; };
  }, []);

  useEffect(() => {
    threadDetailsRef.current = threadDetailsByID;
  }, [threadDetailsByID]);

  useEffect(() => {
    threadLoadingRef.current = threadLoadingByID;
  }, [threadLoadingByID]);

  const applyWorkbench = useCallback((wb) => {
    if (!mountedRef.current || !wb) return;
    if (wb.workspace) setWorkspace(wb.workspace);
    if (wb.home) setHome(wb.home);
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

  const submitCapture = useCallback(async (payload) => {
    const res = await ipc.submitCapture(payload);
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const appendReply = useCallback(async (payload) => {
    const res = await ipc.appendReply(payload);
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryText = useCallback(async (payload) => {
    const res = await ipc.updateEntryText(payload);
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryKind = useCallback(async (payload) => {
    const res = await ipc.updateEntryKind(payload);
    if (!res?.result?.entry?.id) {
      throw new Error("Entry kind update returned no entry payload");
    }
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const updateEntryStatus = useCallback(async (payload) => {
    const res = await ipc.updateEntryStatus(payload);
    if (!res?.result?.entry?.id) {
      throw new Error("Entry status update returned no entry payload");
    }
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const deleteEntry = useCallback(async (entryID) => {
    const res = await ipc.deleteEntry(entryID);
    applyWorkbenchPayload(res);
    return res;
  }, [applyWorkbenchPayload]);

  const routeEntryToThread = useCallback(async (payload) => {
    const res = await ipc.routeEntryToThread(payload);
    applyWorkbenchPayload(res);
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
      applyWorkbenchPayload(res);
      if (!mountedRef.current) {
        return res;
      }
      const resolved = resolveOpenThreadResult({
        threadDetailsByID: threadDetailsRef.current,
        threadLoadingByID: threadLoadingRef.current,
        requestedThreadID: threadID,
        expectedToken: token,
        activeTokens: openThreadTokensRef.current,
        thread: res?.thread ?? null
      });
      setThreadDetailsByID(resolved.threadDetailsByID);
      setThreadLoadingByID(resolved.threadLoadingByID);
      return res;
    } catch (err) {
      setThreadLoadingByID((current) => ({
        ...current,
        [threadID]: false
      }));
      throw err;
    }
  }, [applyWorkbenchPayload]);

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

  return {
    workspace,
    home,
    thread: null,
    threadDetailsByID,
    loading,
    error,
    refresh, createWorkspace, openWorkspace,
    submitCapture, appendReply, updateEntryText, deleteEntry,
    updateEntryKind, updateEntryStatus,
    routeEntryToThread, createThread, createThreadFromEntry, archiveThread, updateThreadTitle,
    openThread, prepareThread,
    getThreadDetail,
    isThreadLoading
  };
}
