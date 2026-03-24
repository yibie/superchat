import { ipc } from "../../lib/ipc.js";
import { cn } from "../../lib/cn.js";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { KIND_LABELS, KIND_COLORS } from "../../lib/constants.js";
import { normalizeEntryMode } from "../../../../src/domain/models/threadnoteModels.js";
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

function tagLabel(item) {
  if (typeof item === "string") {
    return item;
  }
  return item?.name ?? item?.text ?? item?.id ?? JSON.stringify(item);
}

function TagList({ label, items, onItemClick = null }) {
  if (!items?.length) return null;
  return (
    <div className="flex flex-col gap-1">
      <span className="text-xs font-medium text-text-tertiary">{label}</span>
      <div className="flex flex-wrap gap-1.5">
        {items.map((item, i) => (
          <button
            key={i}
            type="button"
            onClick={() => onItemClick?.(item)}
            className={cn(
              "px-2 py-0.5 text-xs rounded bg-elevated text-text border border-border",
              onItemClick && "transition-colors hover:border-accent/40 hover:text-accent"
            )}
            disabled={!onItemClick}
          >
            {tagLabel(item)}
          </button>
        ))}
      </div>
    </div>
  );
}

export function SourceDetailModal({ open, onClose, entry }) {
  const { openMention } = useNavigationContext();
  if (!entry) return null;

  const body = entry.body ?? {};
  const kind = normalizeEntryMode(entry.kind ?? "note");
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
          <MetadataRow label="Mode">
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

        <TagList
          label="Mentions"
          items={mentions}
          onItemClick={(item) => {
            const value = typeof item === "string" ? item : item?.name ?? item?.text ?? "";
            if (value) {
              openMention(value);
              onClose?.();
            }
          }}
        />
        <TagList label="References" items={references} />
      </div>
    </ModalOverlay>
  );
}
