import {
  LocalProviderKinds,
  ProviderDefaults,
  ProviderTitles
} from "../../infrastructure/ai/providers/providerRegistry.js";

export function presentAISettings(config = null, pingState = null) {
  const providerKind = config?.providerKind ?? "ollama";
  const defaults = ProviderDefaults[providerKind] ?? ProviderDefaults.ollama;
  const resolved = {
    providerKind,
    model: config?.model ?? defaults.model,
    baseURL: config?.baseURL ?? defaults.baseURL,
    apiKey: config?.apiKey ?? "",
    embeddingModel: config?.embeddingModel ?? defaultEmbeddingModel(providerKind)
  };

  return {
    providerOptions: Object.entries(ProviderTitles).map(([value, label]) => ({
      value,
      label
    })),
    form: resolved,
    conditionalFields: {
      showsAPIKey: !LocalProviderKinds.has(providerKind),
      showsEmbeddingModel: providerKind === "ollama"
    },
    status: pingState ?? {
      tone: "neutral",
      title: "Not tested",
      detail: "Save settings and test connection to verify the backend."
    }
  };
}

function defaultEmbeddingModel(providerKind) {
  return providerKind === "ollama" ? "nomic-embed-text" : "";
}
