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
  const { generateText } = await loadModule("ai");
  const model = await createModelHandle(resolved);

  return {
    model,
    async generateText({ systemPrompt, userPrompt, temperature = 0.2 }) {
      const result = await generateText({
        model,
        system: systemPrompt,
        prompt: userPrompt,
        temperature,
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
