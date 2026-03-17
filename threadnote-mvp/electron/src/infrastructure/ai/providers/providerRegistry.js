import { AIProviderKind } from "../../../domain/ai/aiContracts.js";

export const ProviderTitles = Object.freeze({
  [AIProviderKind.OPENAI]: "OpenAI",
  [AIProviderKind.ANTHROPIC]: "Anthropic",
  [AIProviderKind.GOOGLE]: "Google (Gemini)",
  [AIProviderKind.GROQ]: "Groq",
  [AIProviderKind.DEEPSEEK]: "DeepSeek",
  [AIProviderKind.XAI]: "xAI (Grok)",
  [AIProviderKind.OLLAMA]: "Ollama (Local)",
  [AIProviderKind.LM_STUDIO]: "LM Studio (Local)",
  [AIProviderKind.OPENAI_COMPAT]: "OpenAI-Compatible"
});

export const ProviderDefaults = Object.freeze({
  [AIProviderKind.OPENAI]: { model: "gpt-4.1-mini", baseURL: "https://api.openai.com/v1" },
  [AIProviderKind.ANTHROPIC]: { model: "claude-sonnet-4-5-20250514", baseURL: "https://api.anthropic.com/v1" },
  [AIProviderKind.GOOGLE]: { model: "gemini-2.0-flash", baseURL: "https://generativelanguage.googleapis.com/v1beta/openai" },
  [AIProviderKind.GROQ]: { model: "llama-3.3-70b-versatile", baseURL: "https://api.groq.com/openai/v1" },
  [AIProviderKind.DEEPSEEK]: { model: "deepseek-chat", baseURL: "https://api.deepseek.com/v1" },
  [AIProviderKind.XAI]: { model: "grok-3-mini", baseURL: "https://api.x.ai/v1" },
  [AIProviderKind.OLLAMA]: { model: "qwen3.5:4b", baseURL: "http://localhost:11434/api" },
  [AIProviderKind.LM_STUDIO]: { model: "local-model", baseURL: "http://localhost:1234/v1" },
  [AIProviderKind.OPENAI_COMPAT]: { model: "default", baseURL: "http://localhost:8000/v1" }
});

export const OpenAICompatibleKinds = new Set([
  AIProviderKind.OPENAI,
  AIProviderKind.GROQ,
  AIProviderKind.DEEPSEEK,
  AIProviderKind.XAI,
  AIProviderKind.OLLAMA,
  AIProviderKind.LM_STUDIO,
  AIProviderKind.OPENAI_COMPAT
]);

export const LocalProviderKinds = new Set([
  AIProviderKind.OLLAMA,
  AIProviderKind.LM_STUDIO,
  AIProviderKind.OPENAI_COMPAT
]);

export function resolveProviderConfig(config = {}) {
  const providerKind = config.providerKind ?? AIProviderKind.OPENAI_COMPAT;
  const defaults = ProviderDefaults[providerKind] ?? ProviderDefaults[AIProviderKind.OPENAI_COMPAT];
  return {
    providerKind,
    model: String(config.model || defaults.model).trim(),
    baseURL: sanitizeBaseURL(config.baseURL || defaults.baseURL),
    apiKey: stringOrNull(config.apiKey),
    embeddingModel: String(config.embeddingModel || defaultEmbeddingModel(providerKind)).trim(),
    headers: isPlainObject(config.headers) ? config.headers : {}
  };
}

export function validateProviderConfig(config = {}) {
  const resolved = resolveProviderConfig(config);
  if (!resolved.model) {
    throw new Error("Provider model is required");
  }
  if (!resolved.baseURL) {
    throw new Error("Provider baseURL is required");
  }
  if (!LocalProviderKinds.has(resolved.providerKind) && !resolved.apiKey) {
    throw new Error(`Provider ${resolved.providerKind} requires apiKey`);
  }
  return resolved;
}

export function isLocalProvider(providerKind) {
  return LocalProviderKinds.has(providerKind);
}

export function providerTitle(providerKind) {
  return ProviderTitles[providerKind] ?? String(providerKind ?? "");
}

function sanitizeBaseURL(raw) {
  return String(raw ?? "").trim().replace(/\/+$/, "");
}

function stringOrNull(value) {
  const text = String(value ?? "").trim();
  return text || null;
}

function isPlainObject(value) {
  return Boolean(value) && typeof value === "object" && !Array.isArray(value);
}

function defaultEmbeddingModel(providerKind) {
  return providerKind === AIProviderKind.OLLAMA ? "nomic-embed-text" : "";
}
