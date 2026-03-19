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

  const getPreview = useCallback((key, loader) => {
    if (!key || typeof loader !== "function") {
      return null;
    }

    if (cache.current.has(key)) {
      return cache.current.get(key);
    }

    if (inflight.current.has(key)) {
      return { loading: true };
    }

    inflight.current.add(key);
    loader()
      .then((preview) => {
        cache.current.set(key, preview ? { ...preview, loading: false } : null);
        inflight.current.delete(key);
        setRevision((r) => r + 1);
      })
      .catch(() => {
        cache.current.set(key, null);
        inflight.current.delete(key);
        setRevision((r) => r + 1);
      });

    return { loading: true };
  }, []);

  const invalidate = useCallback((key) => {
    cache.current.delete(key);
    inflight.current.delete(key);
    setRevision((r) => r + 1);
  }, []);

  return { getPreview, invalidate, _revision: revision };
}
