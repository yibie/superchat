import { useMemo, useState } from "react";
import { cn } from "../../lib/cn.js";
import { THREAD_COLORS } from "../../lib/constants.js";
import { KindBadge } from "./KindBadge.jsx";
import { ThreadBadge } from "./ThreadBadge.jsx";
import { RichPreview } from "./RichPreview.jsx";
import { EntryDraftEditor } from "./EntryDraftEditor.jsx";
import { IconButton } from "../shared/IconButton.jsx";
import { EntryInlineBody } from "./EntryInlineBody.jsx";
import { EntryBacklinks } from "./EntryBacklinks.jsx";
import { ReplyThread, getDiscussionNodeStyle } from "./ReplyThread.jsx";
import { collectEntryRenderableLocators } from "./entryMeta.js";
import { NewThreadModal } from "../modals/NewThreadModal.jsx";

export function EntryCard({
  entry,
  entries,
  allEntries,
  threads,
  actions,
  showThread = true,
  highlighted = false,
  replies = []
}) {
  const isEditing = actions.editingEntryID === entry.id;
  const isReplying = actions.replyingToEntryID === entry.id;
  const [showRoutePicker, setShowRoutePicker] = useState(false);
  const [showNewThreadModal, setShowNewThreadModal] = useState(false);

  const thread = useMemo(() => {
    if (!entry.threadID || !threads) return null;
    return threads.find((t) => t.id === entry.threadID) ?? null;
  }, [entry.threadID, threads]);

  const bodyText = entry.body?.text || entry.summaryText || "";
  const editorState = useMemo(() => ({
    threads: threads ?? [],
    allEntries: allEntries ?? entries ?? [],
    objects: [],
  }), [allEntries, entries, threads]);
  const previewLocators = collectEntryRenderableLocators(entry);
  const hiddenLocators = previewLocators.map((item) => item.locator);
  const isPureLocatorEntry = previewLocators.length === 1 && bodyText.trim() === previewLocators[0].locator;
  const isReplyEntry = (entry?.references ?? []).some(
    (reference) => (reference?.relationKind ?? null) === "responds-to" && reference?.targetID
  );
  const hasDiscussionRail = !isEditing && (replies.length > 0 || isReplyEntry);
  const discussionNodeStyle = getDiscussionNodeStyle(entry.id);

  return (
    <div
      data-entry-id={entry.id}
      className={cn(
        "discussion-lane-item",
        hasDiscussionRail && "discussion-cluster",
        isReplyEntry && replies.length === 0 && "discussion-cluster-standalone-reply"
      )}
      style={hasDiscussionRail ? discussionNodeStyle : undefined}
    >
      <div
        className={cn(
          "group relative rounded-lg px-4 py-3 hover:bg-elevated/50 transition-colors",
          highlighted && "entry-card-highlight"
        )}
      >
        <div className="flex items-center justify-between mb-2">
          <div className="flex items-center gap-2 min-w-0">
            <EntryAIActivity aiActivity={entry.aiActivity} />
            <KindBadge
              kind={entry.kind}
              interactive={typeof actions.updateKind === "function"}
              onSelect={(kind) => actions.updateKind?.(entry.id, kind)}
            />
          </div>
          <time className="text-2xs text-text-tertiary" dateTime={entry.createdAt}>
            {formatRelative(entry.createdAt)}
          </time>
        </div>

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
            <EntryBacklinks entry={entry} allEntries={allEntries ?? entries ?? []} />
          </>
        )}

        {(showThread && thread || !isEditing) && (
          <div className="flex items-center justify-between mt-2">
            {showThread && thread ? <ThreadBadge thread={thread} /> : <span />}
            {!isEditing && (
              <div className="relative flex items-center gap-0.5 ml-auto">
                <IconButton label="Edit" icon={<PencilIcon />} onClick={() => actions.startEdit(entry.id)} />
                <IconButton label="Continue" icon={<ReplyIcon />} onClick={() => actions.startReply(entry.id)} />
                <IconButton
                  label="Move to thread"
                  icon={<FolderIcon />}
                  onClick={() => setShowRoutePicker((v) => !v)}
                />
                <IconButton label="Delete" icon={<TrashIcon />} variant="danger" onClick={() => actions.deleteEntry(entry.id)} />
                {showRoutePicker && (
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
                )}
              </div>
            )}
          </div>
        )}

        {isReplying && (
          <EntryDraftEditor
            entryID={entry.id}
            onSubmit={actions.submitReply}
            onCancel={actions.cancelReply}
            getEditorState={() => editorState}
            placeholder="Continue this note with #tags, @objects, [[references]], or attachments..."
            submitLabel="Continue"
            minHeight={88}
            className="mt-2 ml-6 space-y-2"
          />
        )}
      </div>
      {showNewThreadModal ? (
        <NewThreadModal
          open={showNewThreadModal}
          entryID={entry.id}
          onClose={() => setShowNewThreadModal(false)}
        />
      ) : null}

      {!isEditing ? (
        <ReplyThread
          replies={replies}
          actions={actions}
          allEntries={allEntries ?? entries ?? []}
          threads={threads ?? []}
        />
      ) : null}
    </div>
  );
}

function EntryAIActivity({ aiActivity }) {
  if (!aiActivity?.visible) {
    return null;
  }

  return (
    <div className="entry-ai-activity" title={aiActivity.label}>
      <span
        aria-hidden="true"
        data-testid="entry-ai-activity-dot"
        className="entry-ai-activity-dot"
      />
      <span className="entry-ai-activity-label">{aiActivity.label}</span>
    </div>
  );
}

function ThreadPicker({ threads = [], currentThreadID, onSelect, onCreateThread, onClose }) {
  return (
    <>
      <div className="fixed inset-0 z-40" onClick={onClose} />
      <div className="absolute top-full right-0 z-50 w-64 bg-elevated border border-border rounded-lg shadow-lg py-1 max-h-60 overflow-y-auto">
        {threads.map((t) => (
          <button
            key={t.id}
            onClick={() => onSelect(t.id)}
            disabled={t.id === currentThreadID}
            className={cn(
              "flex items-center gap-2 w-full px-2.5 py-1.5 text-xs text-left transition-colors",
              t.id === currentThreadID
                ? "text-text-tertiary cursor-default"
                : "text-text-secondary hover:bg-surface hover:text-text"
            )}
          >
            <span
              className="w-2 h-2 rounded-full shrink-0"
              style={{ background: THREAD_COLORS[t.color] ?? THREAD_COLORS.sky }}
            />
            <span className="break-words">{t.title}</span>
            {t.id === currentThreadID && <span className="ml-auto text-2xs">current</span>}
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

function formatRelative(dateStr) {
  if (!dateStr) return "";
  const now = Date.now();
  const then = new Date(dateStr).getTime();
  const diffMs = now - then;
  const diffMin = Math.floor(diffMs / 60_000);
  if (diffMin < 1) return "just now";
  if (diffMin < 60) return `${diffMin}m ago`;
  const diffHr = Math.floor(diffMin / 60);
  if (diffHr < 24) return `${diffHr}h ago`;
  const d = new Date(dateStr);
  return d.toLocaleDateString("en-US", { month: "short", day: "numeric" });
}

function PencilIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M11.5 1.5l3 3L5 14H2v-3L11.5 1.5z" />
    </svg>
  );
}

function ReplyIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M6 3L2 7l4 4" /><path d="M2 7h8a4 4 0 014 4v1" />
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

function TrashIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M2 4h12M5 4V2.5A.5.5 0 015.5 2h5a.5.5 0 01.5.5V4m1.5 0l-.8 9.1a1 1 0 01-1 .9H5.3a1 1 0 01-1-.9L3.5 4" />
    </svg>
  );
}
