import { useState, useEffect, useCallback, useRef } from "react";
import { ipc } from "../lib/ipc.js";

const EMPTY_CONFIG = {
  providerKind: "openai",
  apiKey: "",
  baseURL: "",
  modelID: "",
};

export function useAISettings() {
  const [config, setConfig] = useState(EMPTY_CONFIG);
  const [pingResult, setPingResult] = useState(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const mountedRef = useRef(true);

  useEffect(() => {
    mountedRef.current = true;
    return () => { mountedRef.current = false; };
  }, []);

  useEffect(() => {
    (async () => {
      try {
        const cfg = await ipc.getAIProviderConfig();
        if (mountedRef.current && cfg) setConfig(cfg);
      } finally {
        if (mountedRef.current) setLoading(false);
      }
    })();
  }, []);

  const updateField = useCallback((key, value) => {
    setConfig((prev) => ({ ...prev, [key]: value }));
  }, []);

  const save = useCallback(async () => {
    setSaving(true);
    try {
      await ipc.saveAIProviderConfig(config);
    } finally {
      if (mountedRef.current) setSaving(false);
    }
  }, [config]);

  const saveAndPing = useCallback(async () => {
    setSaving(true);
    setPingResult(null);
    try {
      const result = await ipc.saveAndPingAIProvider(config);
      if (mountedRef.current) setPingResult(result);
    } finally {
      if (mountedRef.current) setSaving(false);
    }
  }, [config]);

  const ping = useCallback(async () => {
    setPingResult(null);
    try {
      const result = await ipc.pingAIProvider();
      if (mountedRef.current) setPingResult(result);
    } catch {
      if (mountedRef.current) setPingResult({ ok: false, text: "Ping failed" });
    }
  }, []);

  return { config, pingResult, loading, saving, updateField, save, saveAndPing, ping };
}
