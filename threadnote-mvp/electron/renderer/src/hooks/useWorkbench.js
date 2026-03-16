import { useState, useEffect, useCallback, useRef } from "react";
import { ipc } from "../lib/ipc.js";

export function useWorkbench() {
  const [workspace, setWorkspace] = useState(null);
  const [home, setHome] = useState(null);
  const [thread, setThread] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const mountedRef = useRef(true);

  useEffect(() => {
    mountedRef.current = true;
    return () => { mountedRef.current = false; };
  }, []);

  const applyWorkbench = useCallback((wb) => {
    if (!mountedRef.current || !wb) return;
    if (wb.workspace) setWorkspace(wb.workspace);
    if (wb.home) setHome(wb.home);
    if (wb.thread !== undefined) setThread(wb.thread);
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
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const appendReply = useCallback(async (payload) => {
    const res = await ipc.appendReply(payload);
    applyWorkbench(res?.workbench ?? res);
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const updateEntryText = useCallback(async (payload) => {
    const res = await ipc.updateEntryText(payload);
    applyWorkbench(res?.workbench ?? res);
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const deleteEntry = useCallback(async (entryID) => {
    const res = await ipc.deleteEntry(entryID);
    applyWorkbench(res?.workbench ?? res);
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const routeEntryToThread = useCallback(async (payload) => {
    const res = await ipc.routeEntryToThread(payload);
    applyWorkbench(res?.workbench ?? res);
    return res;
  }, [applyWorkbench]);

  const createThread = useCallback(async (payload) => {
    const res = await ipc.createThread(payload);
    applyWorkbench(res);
    return res;
  }, [applyWorkbench]);

  const createThreadFromEntry = useCallback(async (payload) => {
    const res = await ipc.createThreadFromEntry(payload);
    applyWorkbench(res?.workbench ?? res);
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const archiveThread = useCallback(async (threadID) => {
    const res = await ipc.archiveThread(threadID);
    applyWorkbench(res?.workbench ?? res);
    setThread(null);
    return res;
  }, [applyWorkbench]);

  const openThread = useCallback(async (threadID) => {
    const res = await ipc.openThread(threadID);
    applyWorkbench(res?.workbench ?? res);
    if (res?.thread) setThread(res.thread);
    return res;
  }, [applyWorkbench]);

  const prepareThread = useCallback(async (payload) => {
    const res = await ipc.prepareThread(payload);
    return res;
  }, []);

  return {
    workspace, home, thread, loading, error,
    refresh, createWorkspace, openWorkspace,
    submitCapture, appendReply, updateEntryText, deleteEntry,
    routeEntryToThread, createThread, createThreadFromEntry, archiveThread,
    openThread, prepareThread,
  };
}
