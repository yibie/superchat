import { useState, useCallback, useRef } from "react";
import { SURFACES } from "../lib/constants.js";

export function useNavigation() {
  const [surface, setSurface] = useState(SURFACES.STREAM);
  const [selectedThreadID, setSelectedThreadID] = useState(null);
  const [inspectorOpen, setInspectorOpen] = useState(false);
  const backStack = useRef([]);

  const navigate = useCallback((nextSurface, opts = {}) => {
    setSurface((prev) => {
      backStack.current.push({ surface: prev, threadID: selectedThreadID });
      if (backStack.current.length > 20) backStack.current.shift();
      return nextSurface;
    });
    if (opts.threadID !== undefined) setSelectedThreadID(opts.threadID);
    if (opts.inspector !== undefined) setInspectorOpen(opts.inspector);
  }, [selectedThreadID]);

  const goBack = useCallback(() => {
    const prev = backStack.current.pop();
    if (prev) {
      setSurface(prev.surface);
      setSelectedThreadID(prev.threadID);
    }
  }, []);

  const openThread = useCallback((threadID) => {
    navigate(SURFACES.THREAD, { threadID, inspector: true });
  }, [navigate]);

  const goToStream = useCallback(() => {
    navigate(SURFACES.STREAM);
  }, [navigate]);

  const goToSettings = useCallback(() => {
    navigate(SURFACES.SETTINGS);
    setInspectorOpen(false);
  }, [navigate]);

  const goToResources = useCallback(() => {
    navigate(SURFACES.RESOURCES);
    setInspectorOpen(false);
  }, [navigate]);

  const toggleInspector = useCallback(() => {
    setInspectorOpen((v) => !v);
  }, []);

  return {
    surface, selectedThreadID, inspectorOpen,
    navigate, goBack, openThread,
    goToStream, goToSettings, goToResources,
    toggleInspector, setInspectorOpen,
  };
}
