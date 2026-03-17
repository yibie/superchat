import { useState, useEffect, useCallback, useRef } from "react";
import { ipc } from "../lib/ipc.js";

const PROVIDER_DEFAULTS = {
  openAI: { model: "gpt-4.1-mini", baseURL: "https://api.openai.com/v1" },
  anthropic: { model: "claude-sonnet-4-5-20250514", baseURL: "https://api.anthropic.com/v1" },
  google: { model: "gemini-2.0-flash", baseURL: "https://generativelanguage.googleapis.com/v1beta/openai" },
  groq: { model: "llama-3.3-70b-versatile", baseURL: "https://api.groq.com/openai/v1" },
  deepSeek: { model: "deepseek-chat", baseURL: "https://api.deepseek.com/v1" },
  xai: { model: "grok-3-mini", baseURL: "https://api.x.ai/v1" },
  ollama: { model: "qwen3.5:4b", baseURL: "http://localhost:11434/api", embeddingModel: "nomic-embed-text" }
};

const EMPTY_CONFIG = {
  providerKind: "openAI",
  apiKey: "",
  baseURL: PROVIDER_DEFAULTS.openAI.baseURL,
  model: PROVIDER_DEFAULTS.openAI.model,
  embeddingModel: "",
};

export function useAISettings() {
  const [config, setConfig] = useState(EMPTY_CONFIG);
  const [pingResult, setPingResult] = useState(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [testing, setTesting] = useState(false);
  const mountedRef = useRef(true);

  useEffect(() => {
    mountedRef.current = true;
    return () => { mountedRef.current = false; };
  }, []);

  useEffect(() => {
    (async () => {
      try {
        const cfg = await ipc.getAIProviderConfig();
        if (mountedRef.current && cfg) {
          setConfig((prev) => ({
            ...prev,
            ...cfg,
            model: cfg.model ?? cfg.modelID ?? prev.model
          }));
        }
      } finally {
        if (mountedRef.current) setLoading(false);
      }
    })();
  }, []);

  const updateField = useCallback((key, value) => {
    setConfig((prev) => {
      if (key !== "providerKind") {
        return { ...prev, [key]: value };
      }

      const nextDefaults = PROVIDER_DEFAULTS[value] ?? {};
      const prevDefaults = PROVIDER_DEFAULTS[prev.providerKind] ?? {};
      const shouldReplaceBaseURL = !prev.baseURL || prev.baseURL === prevDefaults.baseURL;
      const shouldReplaceModel = !prev.model || prev.model === prevDefaults.model;
      const shouldReplaceEmbeddingModel =
        !prev.embeddingModel || prev.embeddingModel === (prevDefaults.embeddingModel ?? "");

      return {
        ...prev,
        providerKind: value,
        baseURL: shouldReplaceBaseURL ? (nextDefaults.baseURL ?? prev.baseURL) : prev.baseURL,
        model: shouldReplaceModel ? (nextDefaults.model ?? prev.model) : prev.model,
        embeddingModel: shouldReplaceEmbeddingModel
          ? (nextDefaults.embeddingModel ?? "")
          : prev.embeddingModel
      };
    });
  }, []);

  const save = useCallback(async () => {
    setSaving(true);
    try {
      await ipc.saveAIProviderConfig(config);
    } finally {
      if (mountedRef.current) setSaving(false);
    }
  }, [config]);

  const test = useCallback(async () => {
    setTesting(true);
    setPingResult(null);
    try {
      const result = await ipc.testAIProvider(config);
      if (mountedRef.current) setPingResult(result);
    } finally {
      if (mountedRef.current) setTesting(false);
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

  return { config, pingResult, loading, saving, testing, updateField, save, test, ping };
}
