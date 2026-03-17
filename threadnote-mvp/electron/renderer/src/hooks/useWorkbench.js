import { useState, useEffect, useCallback, useRef } from "react";
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
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

  const appendReply = useCallback(async (payload) => {
    const res = await ipc.appendReply(payload);
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

  const updateEntryText = useCallback(async (payload) => {
    const res = await ipc.updateEntryText(payload);
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

  const deleteEntry = useCallback(async (entryID) => {
    const res = await ipc.deleteEntry(entryID);
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

  const routeEntryToThread = useCallback(async (payload) => {
    const res = await ipc.routeEntryToThread(payload);
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

  const createThread = useCallback(async (payload) => {
    const res = await ipc.createThread(payload);
    applyWorkbench(res);
    return res;
  }, [applyWorkbench]);

  const createThreadFromEntry = useCallback(async (payload) => {
    const res = await ipc.createThreadFromEntry(payload);
    applyWorkbench(res?.workbench ?? res);
    mergeThreadDetail(res?.thread);
    return res;
  }, [applyWorkbench, mergeThreadDetail]);

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

  return {
    workspace,
    home,
    thread: null,
    threadDetailsByID,
    loading,
    error,
    refresh, createWorkspace, openWorkspace,
    submitCapture, appendReply, updateEntryText, deleteEntry,
    routeEntryToThread, createThread, createThreadFromEntry, archiveThread,
    openThread, prepareThread,
    getThreadDetail,
    isThreadLoading
  };
}
