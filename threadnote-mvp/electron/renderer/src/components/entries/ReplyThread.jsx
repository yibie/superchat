import { useMemo, useState } from "react";
import { cn } from "../../lib/cn.js";
import { THREAD_COLORS } from "../../lib/constants.js";
import { KindBadge } from "./KindBadge.jsx";
import { EntryDraftEditor } from "./EntryDraftEditor.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { EntryInlineBody } from "./EntryInlineBody.jsx";
import { EntryBacklinks } from "./EntryBacklinks.jsx";
import { RichPreview } from "./RichPreview.jsx";
import { collectEntryRenderableLocators } from "./entryMeta.js";
import { NewThreadModal } from "../modals/NewThreadModal.jsx";

const DISCUSSION_NODE_PALETTE = [
  "#e85d75",
  "#f59e0b",
  "#14b8a6",
  "#0ea5e9",
  "#22c55e",
  "#f97316"
];

export function getDiscussionNodeStyle(seed = "") {
  const key = String(seed ?? "");
  let hash = 0;
  for (let index = 0; index < key.length; index += 1) {
    hash = ((hash << 5) - hash + key.charCodeAt(index)) | 0;
  }
  const color = DISCUSSION_NODE_PALETTE[Math.abs(hash) % DISCUSSION_NODE_PALETTE.length];
  return {
    "--discussion-node-color": color,
    "--discussion-rail-color": `color-mix(in srgb, ${color} 52%, var(--color-border) 48%)`
  };
}

/**
 * Inline reply lane for an entry.
 * Replies stay visible by default and read as one continuous conversation block.
 */
export function ReplyThread({ replies, actions, allEntries = [], threads = [] }) {
  if (!replies || replies.length === 0) return null;

  return (
    <div className="reply-thread-lane">
      {replies.map((reply, index) => (
        <ReplyCard
          key={reply.id}
          entry={reply}
          actions={actions}
          allEntries={allEntries}
          threads={threads}
          highlighted={reply.highlighted === true}
          connectTop
          connectBottom={index < replies.length - 1}
        />
      ))}
    </div>
  );
}

function ReplyCard({
  entry,
  actions,
  allEntries,
  threads,
  highlighted = false,
  connectTop = false,
  connectBottom = false
}) {
  const isEditing = actions.editingEntryID === entry.id;
  const isReplying = actions.replyingToEntryID === entry.id;
  const [showRoutePicker, setShowRoutePicker] = useState(false);
  const [showNewThreadModal, setShowNewThreadModal] = useState(false);
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
      className="discussion-lane-item reply-thread-item"
      data-reply-entry-id={entry.id}
    >
      <div
        className={cn(
          "group relative rounded-lg px-4 py-3 hover:bg-elevated/50 transition-colors",
          highlighted && "entry-card-highlight"
        )}
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
              <EntryDraftEditor
                entryID={entry.id}
                initialText={entry?.body?.text || entry?.summaryText || ""}
                initialAttachments={entry?.body?.attachments ?? []}
                onSubmit={actions.saveEdit}
                onCancel={actions.cancelEdit}
                getEditorState={() => editorState}
                placeholder="#role @object [[reference]] or [[supports|reference]]"
                submitLabel="Save"
                minHeight={96}
                className="space-y-2"
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
                <EntryBacklinks entry={entry} allEntries={allEntries} />
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
                label="Move to thread"
                icon={<FolderIcon />}
                onClick={() => setShowRoutePicker((value) => !value)}
              />
              <IconButton
                label="Delete reply"
                icon={<TrashIcon />}
                variant="danger"
                onClick={() => actions.deleteEntry(entry.id)}
              />
              {showRoutePicker ? (
                <ThreadPicker
                  threads={threads}
                  currentThreadID={entry.threadID}
                  onSelect={(threadID) => {
                    actions.routeToThread(entry.id, threadID);
                    setShowRoutePicker(false);
                  }}
                  onCreateThread={() => {
                    setShowRoutePicker(false);
                    setShowNewThreadModal(true);
                  }}
                  onClose={() => setShowRoutePicker(false)}
                />
              ) : null}
            </div>
          </div>
        )}

        {isReplying ? (
          <div className="mt-3">
            <EntryDraftEditor
              entryID={entry.id}
              onSubmit={actions.submitReply}
              onCancel={actions.cancelReply}
              getEditorState={() => editorState}
              placeholder="Continue this note with #tags, @objects, [[references]], or attachments..."
              submitLabel="Continue"
              minHeight={88}
              className="space-y-2"
            />
          </div>
        ) : null}
      </div>
      {showNewThreadModal ? (
        <NewThreadModal
          open={showNewThreadModal}
          entryID={entry.id}
          onClose={() => setShowNewThreadModal(false)}
        />
      ) : null}
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

function FolderIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M2 4v8a1 1 0 001 1h10a1 1 0 001-1V6a1 1 0 00-1-1H8L6.5 3.5A1 1 0 005.8 3H3a1 1 0 00-1 1z" />
    </svg>
  );
}

function ThreadPicker({ threads = [], currentThreadID, onSelect, onCreateThread, onClose }) {
  return (
    <>
      <div className="fixed inset-0 z-40" onClick={onClose} />
      <div className="absolute top-full right-0 z-50 w-64 bg-elevated border border-border rounded-lg shadow-lg py-1 max-h-60 overflow-y-auto">
        {threads.map((thread) => (
          <button
            key={thread.id}
            type="button"
            onClick={() => onSelect(thread.id)}
            disabled={thread.id === currentThreadID}
            className={cn(
              "flex items-center gap-2 w-full px-2.5 py-1.5 text-xs text-left transition-colors",
              thread.id === currentThreadID
                ? "text-text-tertiary cursor-default"
                : "text-text-secondary hover:bg-surface hover:text-text"
            )}
          >
            <span
              className="w-2 h-2 rounded-full shrink-0"
              style={{ background: THREAD_COLORS[thread.color] ?? THREAD_COLORS.sky }}
            />
            <span className="break-words">{thread.title}</span>
            {thread.id === currentThreadID ? <span className="ml-auto text-2xs">current</span> : null}
          </button>
        ))}
        <div className="my-1 border-t border-border/80" />
        <button
          type="button"
          onClick={onCreateThread}
          className="flex w-full items-center px-2.5 py-1.5 text-xs text-left text-text-secondary transition-colors hover:bg-surface hover:text-text"
        >
          + New Thread
        </button>
      </div>
    </>
  );
}
