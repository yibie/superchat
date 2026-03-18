import { useMemo } from "react";
import { KindBadge } from "./KindBadge.jsx";
import { InlineEditor } from "./InlineEditor.jsx";
import { ReplyComposer } from "./ReplyComposer.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { EntryInlineBody } from "./EntryInlineBody.jsx";
import { EntryBacklinks } from "./EntryBacklinks.jsx";
import { RichPreview } from "./RichPreview.jsx";
import { collectEntryRenderableLocators } from "./entryMeta.js";

/**
 * Inline reply lane for an entry.
 * Replies stay visible by default and read as one continuous conversation block.
 */
export function ReplyThread({ replies, actions, allEntries = [], threads = [] }) {
  if (!replies || replies.length === 0) return null;

  return (
    <div className="reply-thread-lane mt-3 ml-1 pl-3">
      {replies.map((reply) => (
        <ReplyCard key={reply.id} entry={reply} actions={actions} allEntries={allEntries} threads={threads} />
      ))}
    </div>
  );
}

function ReplyCard({ entry, actions, allEntries, threads }) {
  const isEditing = actions.editingEntryID === entry.id;
  const isReplying = actions.replyingToEntryID === entry.id;
  const bodyText = entry.body?.text || entry.summaryText || "";
  const editorState = useMemo(() => ({
    threads,
    allEntries,
    objects: [],
  }), [allEntries, threads]);
  const previewLocators = collectEntryRenderableLocators(entry);
  const hiddenLocators = previewLocators.map((item) => item.locator);
  const isPureLocatorEntry = previewLocators.length === 1 && bodyText.trim() === previewLocators[0].locator;

  return (
    <div
      className="reply-thread-item group relative rounded-lg px-4 py-3 hover:bg-elevated/50 transition-colors pb-3 last:pb-0"
      data-reply-entry-id={entry.id}
    >
      <div className="flex items-center justify-between mb-2 gap-2">
        <div className="flex items-center gap-2 min-w-0">
          <KindBadge
            kind={entry.kind}
            interactive={typeof actions.updateKind === "function"}
            onSelect={(kind) => actions.updateKind?.(entry.id, kind)}
          />
        </div>
        <time className="text-2xs text-text-tertiary whitespace-nowrap" dateTime={entry.createdAt}>
          {formatRelative(entry.createdAt)}
        </time>
      </div>

      <div className="flex items-start gap-1.5">
        <div className="flex-1 min-w-0">
          {isEditing ? (
            <InlineEditor
              entry={entry}
              onSave={actions.saveEdit}
              onCancel={actions.cancelEdit}
              getEditorState={() => editorState}
            />
          ) : (
            <>
              {!isPureLocatorEntry && (
                <EntryInlineBody entry={entry} hiddenLocators={hiddenLocators} />
              )}
              {previewLocators.map((item) => (
                <RichPreview
                  key={`${entry.id}:preview:${item.locator}`}
                  entryID={item.previewEntryID}
                  url={item.locator}
                />
              ))}
              <EntryBacklinks entry={entry} />
            </>
          )}
        </div>
      </div>

      {!isEditing && (
        <div className="flex items-center justify-end mt-2">
          <div className="relative flex items-center gap-0.5 ml-auto">
            <IconButton
              label="Reply to reply"
              icon={<ReplyIcon />}
              onClick={() => actions.startReply(entry.id)}
            />
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
        </div>
      )}

      {isReplying && (
        <ReplyComposer
          entryID={entry.id}
          onSubmit={actions.submitReply}
          onCancel={actions.cancelReply}
          getEditorState={() => editorState}
        />
      )}
    </div>
  );
}

function formatRelative(value) {
  const parsed = new Date(value);
  if (Number.isNaN(parsed.getTime())) {
    return "";
  }
  return parsed.toLocaleTimeString([], { hour: "numeric", minute: "2-digit" });
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

function ReplyIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M6 5L2 8l4 3" />
      <path d="M2 8h7a4 4 0 014 4v0" />
    </svg>
  );
}
