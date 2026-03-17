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
import { EntryInlineBody } from "./EntryInlineBody.jsx";
import { EntryBacklinks } from "./EntryBacklinks.jsx";

export function EntryCard({ entry, entries, allEntries, threads, actions, showThread = true, highlighted = false }) {
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
  const attachments = entry.body?.attachments ?? [];
  const isLegacyAttachment = bodyText.startsWith("attachments/") && attachments.length === 0;
  const isUrl = /^https?:\/\/\S+$/.test(bodyText.trim());

  return (
    <div
      data-entry-id={entry.id}
      className={cn(
        "group relative rounded-lg px-4 py-3 hover:bg-elevated/50 transition-colors",
        highlighted && "entry-card-highlight"
      )}
    >
      {/* Header: badge left, timestamp right */}
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

      {/* Body */}
      {isEditing ? (
        <InlineEditor
          entry={entry}
          onSave={actions.saveEdit}
          onCancel={actions.cancelEdit}
        />
      ) : (
        <>
          {!isLegacyAttachment && !isUrl && <EntryInlineBody entry={entry} />}
          {isLegacyAttachment && <AttachmentPreview path={bodyText} />}
          {isUrl && <RichPreview entryID={entry.id} url={bodyText.trim()} />}
          {attachments.length > 0 && <AttachmentGrid attachments={attachments} />}
          <EntryBacklinks entry={entry} />
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

const IMG_EXT = /\.(png|jpe?g|gif|webp|bmp|svg|ico)$/i;
const VIDEO_EXT = /\.(mp4|mov|webm|mkv|avi)$/i;
const DOC_EXT = /\.(md|docx?|xlsx?|pptx?|pdf|csv|txt)$/i;

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

function AttachmentGrid({ attachments }) {
  const { workspace } = useWorkbenchContext();
  const resolvePath = (relativePath) =>
    workspace?.workspacePath ? `${workspace.workspacePath}/${relativePath}` : relativePath;
  const resolveUrl = (relativePath) =>
    workspace?.workspacePath ? `file://${resolvePath(relativePath)}` : relativePath;

  const images = attachments.filter((a) => IMG_EXT.test(a.relativePath));
  const videos = attachments.filter((a) => VIDEO_EXT.test(a.relativePath));
  const docs = attachments.filter((a) => DOC_EXT.test(a.relativePath));
  const others = attachments.filter(
    (a) => !IMG_EXT.test(a.relativePath) && !VIDEO_EXT.test(a.relativePath) && !DOC_EXT.test(a.relativePath)
  );

  return (
    <div className="mt-2 space-y-2">
      {images.length === 1 && (
        <img
          src={resolveUrl(images[0].relativePath)}
          alt={images[0].fileName}
          onClick={() => ipc.openLocator(resolvePath(images[0].relativePath))}
          className="max-w-full max-h-60 rounded-xl object-cover cursor-pointer hover:opacity-80 transition-opacity"
        />
      )}
      {images.length > 1 && (
        <div className="flex gap-2">
          {images.map((a, i) => (
            <img
              key={i}
              src={resolveUrl(a.relativePath)}
              alt={a.fileName}
              onClick={() => ipc.openLocator(resolvePath(a.relativePath))}
              className="h-32 rounded-lg object-cover cursor-pointer hover:opacity-80 transition-opacity"
            />
          ))}
        </div>
      )}
      {videos.map((a, i) => (
        <video
          key={i}
          src={resolveUrl(a.relativePath)}
          onClick={() => ipc.openLocator(resolvePath(a.relativePath))}
          className="max-w-full max-h-60 rounded-lg cursor-pointer hover:opacity-80 transition-opacity"
          muted
          preload="metadata"
        />
      ))}
      {docs.map((a, i) => (
        <button
          key={i}
          onClick={() => ipc.openLocator(resolvePath(a.relativePath))}
          className="rounded-lg bg-elevated/60 border border-border px-3 py-2 flex items-center gap-2 cursor-pointer hover:bg-elevated transition-colors"
        >
          <FileIcon />
          <span className="text-sm text-text truncate">{a.fileName}</span>
          {a.size != null && (
            <span className="text-2xs text-text-tertiary ml-auto shrink-0">{formatFileSize(a.size)}</span>
          )}
        </button>
      ))}
      {others.map((a, i) => (
        <button
          key={i}
          onClick={() => ipc.openLocator(resolvePath(a.relativePath))}
          className="inline-flex items-center gap-1.5 text-sm text-accent hover:underline cursor-pointer"
        >
          <FileIcon />
          {a.fileName}
        </button>
      ))}
    </div>
  );
}

function formatFileSize(bytes) {
  if (bytes == null) return "";
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1048576) return `${Math.round(bytes / 1024)} KB`;
  return `${(bytes / 1048576).toFixed(1)} MB`;
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
