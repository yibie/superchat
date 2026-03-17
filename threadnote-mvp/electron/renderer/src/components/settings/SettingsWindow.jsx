import { useEffect, useMemo, useState } from "react";
import { useAISettings } from "../../hooks/useAISettings.js";
import { useShortcutSettings } from "../../hooks/useShortcutSettings.js";
import { cn } from "../../lib/cn.js";
import { ipc } from "../../lib/ipc.js";
import { formatShortcutLabel } from "../../lib/shortcutActions.js";
import { Field, Input, Select } from "../shared/Field.jsx";

const PROVIDERS = [
  { value: "openAI", label: "OpenAI" },
  { value: "anthropic", label: "Anthropic" },
  { value: "google", label: "Google" },
  { value: "groq", label: "Groq" },
  { value: "deepSeek", label: "DeepSeek" },
  { value: "xai", label: "xAI" },
  { value: "ollama", label: "Ollama" }
];

export function SettingsWindow() {
  const quickSettings = useShortcutSettings();
  const { config, pingResult, loading, saving, testing, updateField, save, test } = useAISettings();
  const isOllama = config.providerKind === "ollama";

  if (loading) {
    return (
      <div className="flex h-screen items-center justify-center bg-bg text-text">
        <span className="text-sm text-text-tertiary">Loading settings...</span>
      </div>
    );
  }

  return (
    <div className="flex h-screen flex-col overflow-hidden bg-[radial-gradient(circle_at_top,_rgba(193,160,90,0.18),_transparent_38%),linear-gradient(180deg,_#f6f0e6_0%,_#efe7db_100%)] text-text">
      <header className="drag-region shrink-0 border-b border-border/70 bg-surface/85 backdrop-blur">
        <div className="mx-auto flex max-w-4xl items-center justify-between gap-4 px-6 py-4">
          <div className="min-w-0">
            <p className="text-[11px] uppercase tracking-[0.22em] text-text-tertiary">Preferences</p>
            <h1 className="mt-1 text-2xl font-semibold tracking-tight">Settings</h1>
            <p className="mt-1 text-sm text-text-secondary">Manage keyboard shortcuts and AI provider settings.</p>
          </div>
          <button
            type="button"
            onClick={() => void ipc.closeWindow()}
            className="no-drag rounded-full border border-border bg-elevated px-3 py-1.5 text-sm font-medium text-text-secondary hover:bg-surface-hover"
          >
            Close
          </button>
        </div>
      </header>

      <div className="min-h-0 flex-1 overflow-y-auto">
        <div className="mx-auto flex max-w-4xl flex-col gap-6 px-6 py-6">
          <ShortcutSettingsSection settings={quickSettings} />

          <section className="rounded-[28px] border border-border/80 bg-surface/96 p-5 shadow-[0_18px_50px_rgba(74,60,37,0.12)]">
            <div className="mb-4">
              <h2 className="text-base font-semibold text-text">AI Provider</h2>
              <p className="mt-1 text-sm text-text-secondary">Configure the shared AI backend used by Threadnote.</p>
            </div>

            <div className="flex max-w-md flex-col gap-4">
              <Field label="Provider">
                <Select
                  value={config.providerKind}
                  onChange={(e) => updateField("providerKind", e.target.value)}
                >
                  {PROVIDERS.map((provider) => (
                    <option key={provider.value} value={provider.value}>{provider.label}</option>
                  ))}
                </Select>
              </Field>

              {!isOllama ? (
                <Field label="API Key">
                  <Input
                    type="password"
                    placeholder="sk-..."
                    value={config.apiKey}
                    onChange={(e) => updateField("apiKey", e.target.value)}
                  />
                </Field>
              ) : null}

              <Field label="Base URL">
                <Input
                  placeholder={isOllama ? "http://localhost:11434/api" : "https://api.example.com/v1"}
                  value={config.baseURL}
                  onChange={(e) => updateField("baseURL", e.target.value)}
                />
              </Field>

              <Field label="Model ID">
                <Input
                  placeholder="gpt-4o, claude-sonnet, etc."
                  value={config.model}
                  onChange={(e) => updateField("model", e.target.value)}
                />
              </Field>

              <div className="flex gap-2 pt-2">
                <button
                  onClick={save}
                  disabled={saving || testing}
                  className="rounded-md border border-border bg-elevated px-4 py-2 text-sm font-medium text-text hover:bg-surface-hover disabled:opacity-50"
                >
                  {saving ? "Saving..." : "Save"}
                </button>
                <button
                  onClick={test}
                  disabled={saving || testing}
                  className="rounded-md bg-accent px-4 py-2 text-sm font-medium text-white hover:bg-accent/90 disabled:opacity-50"
                >
                  {testing ? "Testing..." : "Test"}
                </button>
              </div>

              <PingResultBanner result={pingResult} />
            </div>
          </section>
        </div>
      </div>
    </div>
  );
}

function ShortcutSettingsSection({ settings }) {
  const { shortcuts, loading, savingActionId, updateShortcut, clearShortcut } = settings;
  return (
    <section className="rounded-[28px] border border-border/80 bg-surface/96 p-5 shadow-[0_18px_50px_rgba(74,60,37,0.12)]">
      <div className="mb-4">
        <h2 className="text-base font-semibold text-text">Keyboard Shortcuts</h2>
        <p className="mt-1 text-sm text-text-secondary">
          Configure the primary shortcuts used across Threadnote. Failed updates keep the previous shortcut active.
        </p>
      </div>

      <div className="flex flex-col gap-4">
        {shortcuts.map((shortcut) => (
          <ShortcutRow
            key={shortcut.actionId}
            shortcut={shortcut}
            loading={loading || savingActionId === shortcut.actionId}
            onSave={updateShortcut}
            onClear={clearShortcut}
          />
        ))}
      </div>
    </section>
  );
}

function ShortcutRow({ shortcut, loading, onSave, onClear }) {
  const [recording, setRecording] = useState(false);
  const [draftAccelerator, setDraftAccelerator] = useState(null);
  const previewLabel = draftAccelerator ? formatShortcutLabel(draftAccelerator) : formatShortcutLabel(shortcut.accelerator);
  const statusText = useMemo(() => {
    switch (shortcut.registrationState) {
      case "registered":
        return shortcut.scope === "global"
          ? "Registered and available outside the app."
          : "Enabled inside the app.";
      case "conflict":
        return shortcut.scope === "global"
          ? "Current saved shortcut is unavailable right now."
          : shortcut.conflictingActionLabel
            ? `Conflicts with ${shortcut.conflictingActionLabel}.`
            : "Conflicts with another shortcut.";
      case "disabled":
        return "Disabled.";
      case "unset":
      default:
        return "Not set.";
    }
  }, [shortcut.conflictingActionLabel, shortcut.registrationState, shortcut.scope]);

  useEffect(() => {
    if (!recording) {
      return undefined;
    }

    function onKeyDown(event) {
      event.preventDefault();
      event.stopPropagation();
      if (event.key === "Escape") {
        setRecording(false);
        setDraftAccelerator(null);
        return;
      }
      const accelerator = acceleratorFromKeyboardEvent(event);
      if (!accelerator) {
        return;
      }
      setDraftAccelerator(accelerator);
      setRecording(false);
    }

    window.addEventListener("keydown", onKeyDown, true);
    return () => window.removeEventListener("keydown", onKeyDown, true);
  }, [recording]);

  return (
    <div className="rounded-2xl border border-border/70 bg-bg/65 p-4">
      <div className="flex items-start justify-between gap-4">
        <div className="min-w-0">
          <h3 className="text-sm font-medium text-text">{shortcut.label}</h3>
          <p className="mt-1 text-xs text-text-tertiary">{statusText}</p>
          {shortcut.error ? <p className="mt-1 text-xs text-danger">{shortcut.error}</p> : null}
        </div>
        <span className="rounded-full border border-border px-2 py-0.5 text-[11px] uppercase tracking-wide text-text-tertiary">
          {shortcut.scope}
        </span>
      </div>

      <div className="mt-3 flex flex-wrap items-center gap-2">
        <Input readOnly value={previewLabel} className="min-w-[220px] flex-1" />
        <button
          type="button"
          onClick={() => {
            setRecording(true);
            setDraftAccelerator(null);
          }}
          disabled={loading}
          className="rounded-md border border-border bg-elevated px-3 py-2 text-sm font-medium text-text hover:bg-surface-hover disabled:opacity-50"
        >
          {recording ? "Press keys..." : "Record"}
        </button>
        <button
          type="button"
          onClick={() => void onSave(shortcut.actionId, draftAccelerator ?? shortcut.accelerator)}
          disabled={loading || !draftAccelerator}
          className="rounded-md bg-accent px-3 py-2 text-sm font-medium text-white hover:bg-accent/90 disabled:opacity-50"
        >
          {loading ? "Saving..." : "Save"}
        </button>
        <button
          type="button"
          onClick={() => {
            setDraftAccelerator(null);
            void onClear(shortcut.actionId);
          }}
          disabled={loading}
          className="rounded-md border border-border bg-elevated px-3 py-2 text-sm font-medium text-text hover:bg-surface-hover disabled:opacity-50"
        >
          Disable
        </button>
      </div>
    </div>
  );
}

function PingResultBanner({ result }) {
  if (!result) return null;
  return (
    <div
      className={cn(
        "flex flex-col gap-1 rounded-lg border p-3 text-sm",
        result.ok
          ? "border-success/30 bg-success/10 text-success"
          : "border-danger/30 bg-danger/10 text-danger"
      )}
    >
      <span className="font-medium">{result.ok ? "Connected" : "Connection failed"}</span>
      {result.text ? <span className="text-xs opacity-80">{result.text}</span> : null}
    </div>
  );
}

function acceleratorFromKeyboardEvent(event) {
  const key = String(event.key ?? "");
  if (!key || ["Meta", "Control", "Shift", "Alt"].includes(key)) {
    return null;
  }

  const modifiers = [];
  if (event.metaKey || event.ctrlKey) modifiers.push("CommandOrControl");
  if (event.altKey) modifiers.push("Alt");
  if (event.shiftKey) modifiers.push("Shift");
  if (modifiers.length === 0) return null;

  const normalizedKey = normalizeShortcutKey(key);
  if (!normalizedKey) return null;
  return [...modifiers, normalizedKey].join("+");
}

function normalizeShortcutKey(key) {
  if (key === " ") return "Space";
  if (key === "ArrowUp") return "Up";
  if (key === "ArrowDown") return "Down";
  if (key === "ArrowLeft") return "Left";
  if (key === "ArrowRight") return "Right";
  return key.length === 1 ? key.toUpperCase() : key;
}
