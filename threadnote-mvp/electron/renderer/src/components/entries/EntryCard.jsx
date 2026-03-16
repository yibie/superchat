import { useMemo, useState } from "react";
import { cn } from "../../lib/cn.js";
import { THREAD_COLORS } from "../../lib/constants.js";
import { ipc } from "../../lib/ipc.js";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { KindBadge } from "./KindBadge.jsx";
import { ThreadBadge } from "./ThreadBadge.jsx";
import { RichPreview } from "./RichPreview.jsx";
import { InlineEditor } from "./InlineEditor.jsx";
import { ReplyComposer } from "./ReplyComposer.jsx";
import { ReplyThread } from "./ReplyThread.jsx";
import { IconButton } from "../shared/IconButton.jsx";

export function EntryCard({ entry, entries, allEntries, threads, actions, showThread = true }) {
  const isEditing = actions.editingEntryID === entry.id;
  const isReplying = actions.replyingToEntryID === entry.id;
  const [showRoutePicker, setShowRoutePicker] = useState(false);

  const thread = useMemo(() => {
    if (!entry.threadID || !threads) return null;
    return threads.find((t) => t.id === entry.threadID) ?? null;
  }, [entry.threadID, threads]);

  const replies = useMemo(() => {
    const source = allEntries ?? entries;
    if (!source) return [];
    return source.filter((e) => e.parentEntryID === entry.id);
  }, [allEntries, entries, entry.id]);

  const bodyText = entry.body?.text || entry.summaryText || "";
  const isAttachment = bodyText.startsWith("attachments/");
  const isUrl = /^https?:\/\/\S+$/.test(bodyText.trim());

  return (
    <div className="group relative rounded-lg px-4 py-3 hover:bg-elevated/50 transition-colors">
      {/* Header: badge left, timestamp right */}
      <div className="flex items-center justify-between mb-2">
        <KindBadge kind={entry.kind} />
        <time className="text-2xs text-text-tertiary" dateTime={entry.createdAt}>
          {formatRelative(entry.createdAt)}
        </time>
      </div>

      {/* Body */}
      {isEditing ? (
        <InlineEditor
          entry={entry}
          onSave={actions.saveEdit}
          onCancel={actions.cancelEdit}
        />
      ) : (
        <>
          {!isUrl && !isAttachment && (
            <p className="text-sm text-text whitespace-pre-wrap break-words">
              {bodyText}
            </p>
          )}
          {isAttachment && <AttachmentPreview path={bodyText} />}
          {isUrl && <RichPreview entryID={entry.id} url={bodyText.trim()} />}
        </>
      )}

      {/* Bottom row: ThreadBadge left, action buttons right */}
      {(showThread && thread || !isEditing) && (
        <div className="flex items-center justify-between mt-2">
          {showThread && thread ? <ThreadBadge thread={thread} /> : <span />}
          {!isEditing && (
            <div className="relative flex items-center gap-0.5 ml-auto">
              <IconButton label="Edit" icon={<PencilIcon />} onClick={() => actions.startEdit(entry.id)} />
              <IconButton label="Reply" icon={<ReplyIcon />} onClick={() => actions.startReply(entry.id)} />
              <IconButton
                label="Move to thread"
                icon={<FolderIcon />}
                onClick={() => setShowRoutePicker((v) => !v)}
              />
              <IconButton label="Delete" icon={<TrashIcon />} variant="danger" onClick={() => actions.deleteEntry(entry.id)} />
              {/* Thread picker dropdown */}
              {showRoutePicker && threads?.length > 0 && (
                <ThreadPicker
                  threads={threads}
                  currentThreadID={entry.threadID}
                  onSelect={(threadID) => {
                    actions.routeToThread(entry.id, threadID);
                    setShowRoutePicker(false);
                  }}
                  onClose={() => setShowRoutePicker(false)}
                />
              )}
            </div>
          )}
        </div>
      )}

      {isReplying && (
        <ReplyComposer entryID={entry.id} onSubmit={actions.submitReply} onCancel={actions.cancelReply} />
      )}

      {replies.length > 0 && (
        <ReplyThread replies={replies} threads={threads} actions={actions} />
      )}
    </div>
  );
}

const IMG_EXT = /\.(png|jpe?g|gif|webp|bmp|svg|ico)$/i;
const VIDEO_EXT = /\.(mp4|mov|webm|mkv|avi)$/i;

function AttachmentPreview({ path }) {
  const { workspace } = useWorkbenchContext();
  const filename = path.split("/").pop();
  const absolutePath = workspace?.workspacePath
    ? `${workspace.workspacePath}/${path}`
    : path;
  const fileUrl = workspace?.workspacePath
    ? `file://${absolutePath}`
    : path;
  const open = () => ipc.openLocator(absolutePath);

  if (IMG_EXT.test(path)) {
    return (
      <img
        src={fileUrl}
        alt={filename}
        onClick={open}
        className="mt-1 max-w-full max-h-60 rounded-lg object-cover cursor-pointer hover:opacity-80 transition-opacity"
      />
    );
  }

  if (VIDEO_EXT.test(path)) {
    return (
      <video
        src={fileUrl}
        onClick={open}
        className="mt-1 max-w-full max-h-60 rounded-lg cursor-pointer hover:opacity-80 transition-opacity"
        muted
        preload="metadata"
      />
    );
  }

  return (
    <button
      onClick={open}
      className="mt-1 inline-flex items-center gap-1.5 text-sm text-accent hover:underline cursor-pointer"
    >
      <FileIcon />
      {filename}
    </button>
  );
}

function FileIcon() {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M9 1H4a1 1 0 00-1 1v12a1 1 0 001 1h8a1 1 0 001-1V5L9 1z" />
      <path d="M9 1v4h4" />
    </svg>
  );
}

function ThreadPicker({ threads, currentThreadID, onSelect, onClose }) {
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
