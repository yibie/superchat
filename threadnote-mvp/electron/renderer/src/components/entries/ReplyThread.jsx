import { useState } from "react";
import { KindBadge } from "./KindBadge.jsx";
import { InlineEditor } from "./InlineEditor.jsx";
import { ReplyComposer } from "./ReplyComposer.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { EntryInlineBody } from "./EntryInlineBody.jsx";

/**
 * Expandable reply thread for an entry.
 * Shows a toggle button with reply count; when expanded, renders each reply
 * as a compact indented card (no thread badge).
 */
export function ReplyThread({ replies, threads, actions }) {
  const [expanded, setExpanded] = useState(false);

  if (!replies || replies.length === 0) return null;

  return (
    <div className="mt-1 ml-6">
      <button
        type="button"
        onClick={() => setExpanded((v) => !v)}
        className="text-xs text-text-tertiary hover:text-text-secondary transition-colors"
      >
        {expanded ? "Hide" : "Show"} {replies.length} {replies.length === 1 ? "reply" : "replies"}
      </button>

      {expanded && (
        <div className="mt-1 space-y-0 border-l border-border-subtle pl-3">
          {replies.map((reply) => (
            <ReplyCard key={reply.id} entry={reply} actions={actions} />
          ))}
        </div>
      )}
    </div>
  );
}

function ReplyCard({ entry, actions }) {
  const isEditing = actions.editingEntryID === entry.id;
  const isReplying = actions.replyingToEntryID === entry.id;

  return (
    <div className="group relative rounded px-2 py-1.5 hover:bg-elevated/60 transition-colors">
      <div className="flex items-start gap-1.5">
        <KindBadge kind={entry.kind} />
        <div className="flex-1 min-w-0">
          {isEditing ? (
            <InlineEditor
              entry={entry}
              onSave={actions.saveEdit}
              onCancel={actions.cancelEdit}
            />
          ) : (
            <>
              <EntryInlineBody entry={entry} className="text-xs text-text-secondary" />
            </>
          )}
        </div>
      </div>

      {/* Hover actions */}
      {!isEditing && (
        <div className="absolute top-1 right-1 hidden group-hover:flex items-center gap-0.5">
          <IconButton
            label="Edit reply"
            icon={<PencilIcon />}
            onClick={() => actions.startEdit(entry.id)}
          />
          <IconButton
            label="Delete reply"
            icon={<TrashIcon />}
            variant="danger"
            onClick={() => actions.deleteEntry(entry.id)}
          />
        </div>
      )}

      {isReplying && (
        <ReplyComposer
          entryID={entry.id}
          onSubmit={actions.submitReply}
          onCancel={actions.cancelReply}
        />
      )}
    </div>
  );
}

function PencilIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M11.5 1.5l3 3L5 14H2v-3L11.5 1.5z" />
    </svg>
  );
}

function TrashIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M2 4h12M5 4V2.5A.5.5 0 015.5 2h5a.5.5 0 01.5.5V4m1.5 0l-.8 9.1a1 1 0 01-1 .9H5.3a1 1 0 01-1-.9L3.5 4" />
    </svg>
  );
}
