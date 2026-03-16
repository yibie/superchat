import { useState, useCallback, useRef } from "react";
import { ipc } from "../lib/ipc.js";

/**
 * Lazy-loads rich preview HTML for entries.
 * Maintains an in-memory cache keyed by entryID.
 */
export function useRichPreviews() {
  const cache = useRef(new Map());
  const inflight = useRef(new Set());
  const [revision, setRevision] = useState(0);

  const getPreview = useCallback((entryID) => {
    if (cache.current.has(entryID)) {
      return cache.current.get(entryID);
    }

    // Already fetching — return loading sentinel
    if (inflight.current.has(entryID)) {
      return { loading: true };
    }

    // Kick off fetch
    inflight.current.add(entryID);
    ipc.getEntryRichPreview(entryID)
      .then((preview) => {
        cache.current.set(entryID, preview ? { ...preview, loading: false } : null);
        inflight.current.delete(entryID);
        setRevision((r) => r + 1);
      })
      .catch(() => {
        cache.current.set(entryID, null);
        inflight.current.delete(entryID);
        setRevision((r) => r + 1);
      });

    return { loading: true };
  }, []);

  const invalidate = useCallback((entryID) => {
    cache.current.delete(entryID);
    inflight.current.delete(entryID);
    setRevision((r) => r + 1);
  }, []);

  return { getPreview, invalidate, _revision: revision };
}
