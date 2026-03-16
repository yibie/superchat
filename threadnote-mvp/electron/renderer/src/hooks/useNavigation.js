import { useState, useCallback, useRef } from "react";
import { SURFACES } from "../lib/constants.js";

function supportsInspector(surface) {
  return surface === SURFACES.STREAM || surface === SURFACES.THREAD;
}

function defaultInspectorOpen(surface) {
  return supportsInspector(surface);
}

export function useNavigation() {
  const [surface, setSurface] = useState(SURFACES.STREAM);
  const [selectedThreadID, setSelectedThreadID] = useState(null);
  const [inspectorOpen, setInspectorOpen] = useState(true);
  const [threadInspectorTab, setThreadInspectorTab] = useState("restart");
  const [focusedEntryTarget, setFocusedEntryTarget] = useState(null);
  const backStack = useRef([]);

  const navigate = useCallback((nextSurface, opts = {}) => {
    const nextInspectorOpen = opts.inspector ?? defaultInspectorOpen(nextSurface);
    setSurface((prev) => {
      backStack.current.push({ surface: prev, threadID: selectedThreadID });
      if (backStack.current.length > 20) backStack.current.shift();
      return nextSurface;
    });
    if (opts.threadID !== undefined) setSelectedThreadID(opts.threadID);
    setInspectorOpen(nextInspectorOpen);
  }, [selectedThreadID]);

  const goBack = useCallback(() => {
    const prev = backStack.current.pop();
    if (prev) {
      setSurface(prev.surface);
      setSelectedThreadID(prev.threadID);
      setInspectorOpen(defaultInspectorOpen(prev.surface));
    }
  }, []);

  const openThread = useCallback((threadID, opts = {}) => {
    navigate(SURFACES.THREAD, { threadID, inspector: true });
    setThreadInspectorTab(opts.inspectorTab ?? "restart");
  }, [navigate]);

  const goToStream = useCallback(() => {
    navigate(SURFACES.STREAM, { inspector: true });
  }, [navigate]);

  const goToSettings = useCallback(() => {
    navigate(SURFACES.SETTINGS, { inspector: false });
  }, [navigate]);

  const goToResources = useCallback(() => {
    navigate(SURFACES.RESOURCES, { inspector: false });
  }, [navigate]);

  const focusEntry = useCallback((entryID, { threadID = null } = {}) => {
    setFocusedEntryTarget({ entryID, threadID, nonce: Date.now() });
    if (threadID) {
      if (!(surface === SURFACES.THREAD && selectedThreadID === threadID)) {
        navigate(SURFACES.THREAD, { threadID, inspector: true });
      }
      return;
    }
    if (surface !== SURFACES.STREAM) {
      navigate(SURFACES.STREAM, { inspector: true });
    }
  }, [navigate, selectedThreadID, surface]);

  const clearFocusedEntry = useCallback(() => {
    setFocusedEntryTarget(null);
  }, []);

  const toggleInspector = useCallback(() => {
    if (!supportsInspector(surface)) {
      setInspectorOpen(false);
      return;
    }
    setInspectorOpen((v) => !v);
  }, [surface]);

  const showThreadInspectorTab = useCallback((tab) => {
    setThreadInspectorTab(tab);
    if (surface === SURFACES.THREAD) {
      setInspectorOpen(true);
    }
  }, [surface]);

  return {
    surface, selectedThreadID, inspectorOpen, threadInspectorTab, focusedEntryTarget,
    navigate, goBack, openThread,
    goToStream, goToSettings, goToResources,
    toggleInspector, setInspectorOpen,
    focusEntry, clearFocusedEntry,
    setThreadInspectorTab, showThreadInspectorTab
  };
}
