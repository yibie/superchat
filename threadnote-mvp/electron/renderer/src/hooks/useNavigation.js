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
  const forwardStack = useRef([]);
  const currentStateRef = useRef(null);

  currentStateRef.current = {
    surface,
    threadID: selectedThreadID,
    inspectorOpen,
    threadInspectorTab
  };

  const restoreSnapshot = useCallback((snapshot) => {
    if (!snapshot) return;
    setSurface(snapshot.surface);
    setSelectedThreadID(snapshot.threadID ?? null);
    setInspectorOpen(snapshot.inspectorOpen ?? defaultInspectorOpen(snapshot.surface));
    setThreadInspectorTab(snapshot.threadInspectorTab ?? "restart");
  }, []);

  const navigate = useCallback((nextSurface, opts = {}) => {
    const current = currentStateRef.current;
    const nextState = {
      surface: nextSurface,
      threadID: opts.threadID !== undefined ? opts.threadID : current.threadID,
      inspectorOpen: opts.inspector ?? defaultInspectorOpen(nextSurface),
      threadInspectorTab: opts.threadInspectorTab ?? current.threadInspectorTab
    };
    const isSameState =
      current.surface === nextState.surface &&
      current.threadID === nextState.threadID &&
      current.inspectorOpen === nextState.inspectorOpen &&
      current.threadInspectorTab === nextState.threadInspectorTab;

    if (isSameState) {
      return;
    }

    backStack.current.push(current);
    if (backStack.current.length > 20) backStack.current.shift();
    forwardStack.current = [];

    setSurface(nextState.surface);
    setSelectedThreadID(nextState.threadID);
    setInspectorOpen(nextState.inspectorOpen);
    setThreadInspectorTab(nextState.threadInspectorTab);
  }, []);

  const goBack = useCallback(() => {
    const prev = backStack.current.pop();
    if (prev) {
      forwardStack.current.push(currentStateRef.current);
      restoreSnapshot(prev);
    }
  }, [restoreSnapshot]);

  const goForward = useCallback(() => {
    const next = forwardStack.current.pop();
    if (next) {
      backStack.current.push(currentStateRef.current);
      restoreSnapshot(next);
    }
  }, [restoreSnapshot]);

  const openThread = useCallback((threadID, opts = {}) => {
    navigate(SURFACES.THREAD, {
      threadID,
      inspector: true,
      threadInspectorTab: opts.inspectorTab ?? "restart"
    });
  }, [navigate]);

  const goToStream = useCallback(() => {
    navigate(SURFACES.STREAM, { inspector: true });
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
    navigate, goBack, goForward, openThread,
    canGoBack: backStack.current.length > 0,
    canGoForward: forwardStack.current.length > 0,
    goToStream, goToResources,
    toggleInspector, setInspectorOpen,
    focusEntry, clearFocusedEntry,
    setThreadInspectorTab, showThreadInspectorTab
  };
}
