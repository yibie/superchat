import { useAISettings } from "../../hooks/useAISettings.js";
import { cn } from "../../lib/cn.js";
import { Field, Input, Select } from "../shared/Field.jsx";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";

const PROVIDERS = [
  { value: "openai", label: "OpenAI" },
  { value: "anthropic", label: "Anthropic" },
  { value: "google", label: "Google" },
  { value: "groq", label: "Groq" },
  { value: "deepseek", label: "DeepSeek" },
  { value: "xai", label: "xAI" },
  { value: "ollama", label: "Ollama" },
];

function PingResultBanner({ result }) {
  if (!result) return null;

  return (
    <div
      className={cn(
        "flex flex-col gap-1 p-3 rounded-lg text-sm border",
        result.ok
          ? "bg-success/10 border-success/30 text-success"
          : "bg-danger/10 border-danger/30 text-danger"
      )}
    >
      <span className="font-medium">{result.ok ? "Connected" : "Connection failed"}</span>
      {result.text && <span className="text-xs opacity-80">{result.text}</span>}
      {result.ok && (
        <div className="flex flex-wrap gap-x-4 gap-y-1 text-xs opacity-80">
          {result.modelID && <span>Model: {result.modelID}</span>}
          {result.backendLabel && <span>Backend: {result.backendLabel}</span>}
          {result.latencyMS != null && <span>Latency: {result.latencyMS}ms</span>}
        </div>
      )}
    </div>
  );
}

export function SettingsSurface() {
  const { config, pingResult, loading, saving, updateField, save, saveAndPing } = useAISettings();
  const isOllama = config.providerKind === "ollama";

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <span className="text-sm text-text-tertiary">Loading settings...</span>
      </div>
    );
  }

  return (
    <div className="flex flex-col h-full">
      <SurfaceHeader title="AI Provider" className="max-w-2xl mx-auto" />

      <div className="flex-1 overflow-y-auto">
        <div className="max-w-2xl mx-auto px-6 py-4">
          <div className="flex flex-col gap-4 max-w-md">
        <Field label="Provider">
          <Select
            value={config.providerKind}
            onChange={(e) => updateField("providerKind", e.target.value)}
          >
            {PROVIDERS.map((p) => (
              <option key={p.value} value={p.value}>{p.label}</option>
            ))}
          </Select>
        </Field>

        {!isOllama && (
          <Field label="API Key">
            <Input
              type="password"
              placeholder="sk-..."
              value={config.apiKey}
              onChange={(e) => updateField("apiKey", e.target.value)}
            />
          </Field>
        )}

        <Field label="Base URL" hint={isOllama ? "Required for Ollama" : "Optional override"}>
          <Input
            placeholder={isOllama ? "http://localhost:11434" : "https://api.example.com/v1"}
            value={config.baseURL}
            onChange={(e) => updateField("baseURL", e.target.value)}
          />
        </Field>

        <Field label="Model ID">
          <Input
            placeholder="gpt-4o, claude-3-opus, etc."
            value={config.modelID}
            onChange={(e) => updateField("modelID", e.target.value)}
          />
        </Field>

        <div className="flex gap-2 pt-2">
          <button
            onClick={save}
            disabled={saving}
            className={cn(
              "px-4 py-2 text-sm font-medium rounded-md transition-colors",
              "bg-elevated text-text border border-border",
              "hover:bg-surface-hover disabled:opacity-50"
            )}
          >
            {saving ? "Saving..." : "Save"}
          </button>
          <button
            onClick={saveAndPing}
            disabled={saving}
            className={cn(
              "px-4 py-2 text-sm font-medium rounded-md transition-colors",
              "bg-accent text-white",
              "hover:bg-accent/90 disabled:opacity-50"
            )}
          >
            {saving ? "Testing..." : "Save & Ping"}
          </button>
        </div>

        <PingResultBanner result={pingResult} />
          </div>
        </div>
      </div>
    </div>
  );
}
