import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { ipc } from "../lib/ipc.js";
import { SHORTCUT_ACTIONS } from "../lib/shortcutActions.js";

export function useShortcutSettings() {
  const [shortcuts, setShortcuts] = useState([]);
  const [loading, setLoading] = useState(true);
  const [savingActionId, setSavingActionId] = useState(null);
  const [errors, setErrors] = useState({});
  const mountedRef = useRef(true);

  const applyShortcuts = useCallback((items) => {
    if (!mountedRef.current || !Array.isArray(items)) {
      return;
    }
    setShortcuts(items);
  }, []);

  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  const refresh = useCallback(async () => {
    const result = await ipc.getShortcutSettings();
    applyShortcuts(result);
    return result;
  }, [applyShortcuts]);

  useEffect(() => {
    (async () => {
      try {
        await refresh();
      } finally {
        if (mountedRef.current) {
          setLoading(false);
        }
      }
    })();
  }, [refresh]);

  useEffect(() => {
    const unsubscribe = ipc.onShortcutSettingsUpdated((items) => {
      applyShortcuts(items);
    });
    return () => unsubscribe?.();
  }, [applyShortcuts]);

  const updateShortcut = useCallback(async (actionId, accelerator) => {
    setSavingActionId(actionId);
    setErrors((prev) => ({ ...prev, [actionId]: "" }));
    try {
      const result = await ipc.updateShortcutSetting({ actionId, accelerator });
      if (result?.attemptedRegistrationState === "conflict") {
        setErrors((prev) => ({
          ...prev,
          [actionId]:
            result.conflictingActionLabel
              ? `That shortcut is already used by ${result.conflictingActionLabel}.`
              : "That shortcut is currently unavailable. Your previous shortcut is still active."
        }));
      }
      return result;
    } finally {
      if (mountedRef.current) {
        setSavingActionId(null);
      }
    }
  }, []);

  const clearShortcut = useCallback(async (actionId) => {
    setSavingActionId(actionId);
    setErrors((prev) => ({ ...prev, [actionId]: "" }));
    try {
      return await ipc.clearShortcutSetting(actionId);
    } finally {
      if (mountedRef.current) {
        setSavingActionId(null);
      }
    }
  }, []);

  const shortcutMap = useMemo(() => {
    const incoming = new Map(shortcuts.map((item) => [item.actionId, item]));
    return SHORTCUT_ACTIONS.map((action) => ({
      ...action,
      ...(incoming.get(action.id) ?? {
        actionId: action.id,
        label: action.label,
        scope: action.scope,
        accelerator: null,
        enabled: false,
        registrationState: "unset"
      }),
      error: errors[action.id] ?? ""
    }));
  }, [errors, shortcuts]);

  return {
    shortcuts: shortcutMap,
    loading,
    savingActionId,
    updateShortcut,
    clearShortcut,
    refresh
  };
}
