import {
  AIProviderKind
} from "../../../domain/ai/aiContracts.js";
import {
  LocalProviderKinds,
  OpenAICompatibleKinds,
  validateProviderConfig
} from "../providers/providerRegistry.js";

async function loadModule(moduleName) {
  try {
    return await import(moduleName);
  } catch {
    throw new Error(
      `Missing dependency "${moduleName}". Install Vercel AI SDK packages in electron/: npm install ai @ai-sdk/openai @ai-sdk/anthropic @ai-sdk/google`
    );
  }
}

export async function createVercelAIClient(config) {
  const resolved = validateProviderConfig(config);

  if (resolved.providerKind === AIProviderKind.OLLAMA) {
    return createOllamaNativeClient(resolved);
  }

  const { generateText } = await loadModule("ai");
  const model = await createModelHandle(resolved);

  return {
    model,
    async generateText({ systemPrompt, userPrompt, temperature = 0.2, signal = null, maxOutputTokens = null }) {
      const result = await generateText({
        model,
        system: systemPrompt,
        prompt: userPrompt,
        temperature,
        abortSignal: signal ?? undefined,
        maxOutputTokens: maxOutputTokens ?? undefined,
        experimental_telemetry: undefined,
        include: {
          requestBody: true,
          responseBody: true
        }
      });

      return {
        text: result.text ?? "",
        finishReason: result.finishReason ?? "stop",
        warnings: result.warnings ?? [],
        request: result.request ?? null,
        response: result.response ?? {},
        usage: result.usage ?? null
      };
    }
  };
}

export async function createModelHandle(config) {
  const resolved = validateProviderConfig(config);

  if (resolved.providerKind === AIProviderKind.OLLAMA) {
    throw new Error("Ollama uses the native adapter and does not expose an OpenAI-compatible model handle");
  }

  if (resolved.providerKind === AIProviderKind.ANTHROPIC) {
    const { createAnthropic } = await loadModule("@ai-sdk/anthropic");
    const anthropic = createAnthropic({
      apiKey: resolved.apiKey,
      baseURL: resolved.baseURL || undefined,
      headers: resolved.headers
    });
    return anthropic(resolved.model);
  }

  if (resolved.providerKind === AIProviderKind.GOOGLE) {
    const { createGoogleGenerativeAI } = await loadModule("@ai-sdk/google");
    const google = createGoogleGenerativeAI({
      apiKey: resolved.apiKey,
      baseURL: resolved.baseURL || undefined,
      headers: resolved.headers
    });
    return google(resolved.model);
  }

  if (OpenAICompatibleKinds.has(resolved.providerKind)) {
    const { createOpenAI } = await loadModule("@ai-sdk/openai");
    const openai = createOpenAI({
      apiKey: resolveOpenAICompatibleAPIKey(resolved),
      baseURL: resolved.baseURL || undefined,
      headers: resolved.headers
    });
    return openai(resolved.model);
  }

  throw new Error(`Unsupported provider kind for Vercel AI SDK: ${resolved.providerKind}`);
}

export function resolveOpenAICompatibleAPIKey(config) {
  return config.apiKey || (LocalProviderKinds.has(config.providerKind) ? "threadnote-local" : undefined);
}

export function createOllamaNativeClient(config, { fetchImpl = globalThis.fetch } = {}) {
  if (typeof fetchImpl !== "function") {
    throw new Error("Fetch API is unavailable for Ollama native client");
  }

  const endpoint = buildOllamaGenerateURL(config.baseURL);

  return {
    model: {
      provider: "ollama-native",
      modelId: config.model
    },
    async generateText({ systemPrompt, userPrompt, temperature = 0.2, signal = null, maxOutputTokens = null }) {
      const requestBody = createOllamaGenerateBody(config, {
        systemPrompt,
        userPrompt,
        temperature,
        maxOutputTokens
      });
      const response = await fetchImpl(endpoint, {
        method: "POST",
        signal: signal ?? undefined,
        headers: {
          "content-type": "application/json",
          ...(config.headers ?? {})
        },
        body: JSON.stringify(requestBody)
      });

      const body = await response.json().catch(() => null);
      if (!response.ok) {
        throw new Error(body?.error || `Ollama request failed with status ${response.status}`);
      }

      return {
        text: String(body?.response ?? ""),
        finishReason: body?.done ? "stop" : "unknown",
        warnings: [],
        request: null,
        response: {
          id: body?.created_at ?? null,
          modelId: body?.model ?? config.model,
          body
        },
        usage: body?.eval_count != null || body?.prompt_eval_count != null
          ? {
              completionTokens: Number(body?.eval_count ?? 0),
              promptTokens: Number(body?.prompt_eval_count ?? 0),
              totalTokens: Number(body?.eval_count ?? 0) + Number(body?.prompt_eval_count ?? 0)
            }
          : null
      };
    }
  };
}

export function createOllamaGenerateBody(
  config,
  { systemPrompt, userPrompt, temperature = 0.2, maxOutputTokens = null } = {}
) {
  const options = {
    temperature
  };
  if (maxOutputTokens != null) {
    options.num_predict = maxOutputTokens;
  }

  const body = {
    model: config.model,
    system: systemPrompt || undefined,
    prompt: userPrompt,
    stream: false,
    options,
    think: resolveOllamaThinkSetting(),
    keep_alive: resolveOllamaKeepAlive()
  };

  return body;
}

export function resolveOllamaThinkSetting() {
  return false;
}

export function resolveOllamaKeepAlive() {
  return "15m";
}

export function buildOllamaGenerateURL(baseURL) {
  const url = new URL(String(baseURL ?? "").trim());
  const pathname = url.pathname.replace(/\/+$/, "");
  if (!pathname || pathname === "/") {
    url.pathname = "/api/generate";
    return url.toString();
  }
  if (pathname.endsWith("/v1")) {
    url.pathname = `${pathname.slice(0, -3)}/api/generate`;
    return url.toString();
  }
  if (pathname.endsWith("/api")) {
    url.pathname = `${pathname}/generate`;
    return url.toString();
  }
  if (pathname.endsWith("/api/generate")) {
    url.pathname = pathname;
    return url.toString();
  }
  url.pathname = `${pathname}/api/generate`;
  return url.toString();
}
