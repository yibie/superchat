import { ipc } from "../../lib/ipc.js";
import { cn } from "../../lib/cn.js";
import { KIND_LABELS, KIND_COLORS } from "../../lib/constants.js";
import { ModalOverlay } from "./ModalOverlay.jsx";

function MetadataRow({ label, children }) {
  if (!children) return null;
  return (
    <div className="flex gap-2 text-xs">
      <span className="text-text-tertiary shrink-0">{label}:</span>
      <span className="text-text">{children}</span>
    </div>
  );
}

function TagList({ label, items }) {
  if (!items?.length) return null;
  return (
    <div className="flex flex-col gap-1">
      <span className="text-xs font-medium text-text-tertiary">{label}</span>
      <div className="flex flex-wrap gap-1.5">
        {items.map((item, i) => (
          <span
            key={i}
            className="px-2 py-0.5 text-xs rounded bg-elevated text-text border border-border"
          >
            {typeof item === "string" ? item : item.text ?? item.id ?? JSON.stringify(item)}
          </span>
        ))}
      </div>
    </div>
  );
}

export function SourceDetailModal({ open, onClose, entry }) {
  if (!entry) return null;

  const body = entry.body ?? {};
  const kind = entry.kind ?? "note";
  const text = body.text ?? "";
  const url = body.url;
  const created = entry.created ? new Date(entry.created).toLocaleDateString() : null;
  const mentions = body.objectMentions ?? entry.objectMentions ?? [];
  const references = body.references ?? entry.references ?? [];

  return (
    <ModalOverlay open={open} onClose={onClose} title="Source Detail" width="max-w-lg">
      <div className="flex flex-col gap-4">
        {text && (
          <p className="text-sm text-text whitespace-pre-wrap leading-relaxed">{text}</p>
        )}

        {url && (
          <button
            onClick={() => ipc.openLocator(url)}
            className={cn(
              "self-start text-xs text-accent underline underline-offset-2",
              "hover:text-accent/80 transition-colors truncate max-w-full"
            )}
          >
            {url}
          </button>
        )}

        <div className="flex flex-col gap-2 pt-2 border-t border-border">
          <MetadataRow label="Kind">
            <span
              className="px-1.5 py-0.5 rounded text-[10px] font-medium"
              style={{ color: KIND_COLORS[kind], borderColor: KIND_COLORS[kind], border: "1px solid" }}
            >
              {KIND_LABELS[kind] ?? kind}
            </span>
          </MetadataRow>
          <MetadataRow label="Created">{created}</MetadataRow>
          <MetadataRow label="Thread">{entry.threadID ?? entry.thread?.title}</MetadataRow>
        </div>

        <TagList label="Object Mentions" items={mentions} />
        <TagList label="References" items={references} />
      </div>
    </ModalOverlay>
  );
}
