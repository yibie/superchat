import { cn } from "../../lib/cn.js";
import { ipc } from "../../lib/ipc.js";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useRichPreviews } from "../../hooks/useRichPreviews.js";
import {
  createResourceActions,
  groupDisplayResources,
  resourceMetaLabel,
  resourcePreviewKind,
  resourceSourceLabel,
  resourceSummaryLabel,
  resolveResourceOpenTarget,
  resolveResourceRenderableURL,
  resolveResourceLocator,
  resolveResourceTitle
} from "./resourceViewModel.js";

function PreviewAction({ resource, className }) {
  const { workspace } = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const actions = createResourceActions(resource, {
    workspace,
    openLocator: (target) => ipc.openLocator(target),
    focusEntry
  });

  if (!actions.canGoToNote) {
    return null;
  }

  return (
    <button
      type="button"
      onClick={(event) => {
        event.stopPropagation();
        actions.goToNote();
      }}
      className={cn(
        "absolute right-2 top-2 z-10 inline-flex h-7 w-7 items-center justify-center rounded-full",
        "bg-bg/78 text-text-secondary shadow-sm ring-1 ring-border/70 backdrop-blur-sm",
        "opacity-0 transition-opacity group-hover:opacity-100 group-focus-within:opacity-100",
        "hover:text-text hover:bg-elevated focus-visible:opacity-100",
        className
      )}
      aria-label="Back to note"
      title="Back to note"
    >
      {"\u2197"}
    </button>
  );
}

function EmptyThumbnail({ label, accent = false }) {
  return (
    <div className={cn(
      "flex h-full w-full items-center justify-center rounded-[18px] bg-elevated text-center",
      accent && "bg-accent/8"
    )}>
      <span className="px-4 text-[11px] font-medium text-text-secondary">{label}</span>
    </div>
  );
}

function mediaCardLabel(resource) {
  const summary = resourceSummaryLabel(resource);
  const locator = resolveResourceLocator(resource);
  const title = resolveResourceTitle(resource);
  const summaryValue = String(summary ?? "").trim();
  const locatorValue = String(locator ?? "").trim();
  const titleValue = String(title ?? "").trim();

  if (
    summaryValue &&
    summaryValue !== locatorValue &&
    !summaryValue.startsWith("attachments/") &&
    !/^https?:\/\//i.test(summaryValue)
  ) {
    return summaryValue;
  }

  if (
    titleValue &&
    titleValue !== locatorValue &&
    !titleValue.startsWith("attachments/") &&
    !/^https?:\/\//i.test(titleValue)
  ) {
    return titleValue;
  }

  return "";
}

function MediaResourceCard({ resource, compact = false }) {
  const { workspace } = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const previewKind = resourcePreviewKind(resource);
  const title = resolveResourceTitle(resource);
  const label = mediaCardLabel(resource);
  const source = resourceSourceLabel(resource);
  const renderableURL = resolveResourceRenderableURL(resource, workspace);
  const actions = createResourceActions(resource, {
    workspace,
    openLocator: (target) => ipc.openLocator(target),
    focusEntry
  });

  return (
    <button
      type="button"
      onClick={actions.openResource}
      disabled={!actions.canOpen}
      className={cn(
        "group relative overflow-hidden rounded-[20px] bg-surface/92 text-left shadow-sm ring-1 ring-border/55",
        "transition-all hover:-translate-y-px hover:shadow-md hover:ring-border/85",
        compact ? "min-h-[156px]" : "min-h-[188px]",
        !actions.canOpen && "cursor-default"
      )}
    >
      <PreviewAction resource={resource} />
      <div className={cn("overflow-hidden bg-elevated", compact ? "aspect-square" : "aspect-[0.98]")}>
        {previewKind === "image" && renderableURL ? (
          <img src={renderableURL} alt={title} className="h-full w-full object-cover" loading="lazy" />
        ) : previewKind === "video" && renderableURL ? (
          <video src={renderableURL} className="h-full w-full object-cover" muted preload="metadata" />
        ) : (
          <EmptyThumbnail label={previewKind === "video" ? "Video preview unavailable" : "Preview unavailable"} />
        )}
      </div>
      <div className="space-y-0.5 px-3 pb-3 pt-2">
        {label ? <p className="truncate text-[13px] font-medium text-text">{label}</p> : null}
        <p className="truncate text-[11px] text-text-secondary">{source}</p>
      </div>
    </button>
  );
}

function LinkThumbnail({ resource, preview }) {
  if (preview?.loading) {
    return <div className="h-full w-full animate-pulse rounded-[14px] bg-elevated" />;
  }
  if (preview?.previewImageURL) {
    return <img src={preview.previewImageURL} alt="" className="h-full w-full object-cover" loading="lazy" />;
  }
  const title = resolveResourceTitle(resource);
  const glyph = title.replace(/^@/, "").charAt(0).toUpperCase() || "L";
  return (
    <div className="flex h-full w-full items-center justify-center rounded-[14px] bg-elevated text-lg font-semibold text-text-secondary">
      {glyph}
    </div>
  );
}

function LinkResourceCard({ resource, compact = false }) {
  const { workspace } = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const { getPreview } = useRichPreviews();
  const actions = createResourceActions(resource, {
    workspace,
    openLocator: (target) => ipc.openLocator(target),
    focusEntry
  });
  const title = resolveResourceTitle(resource);
  const source = resourceSourceLabel(resource);
  const meta = resourceMetaLabel(resource);
  const preview = resource?.entryID ? getPreview(resource.entryID) : null;

  return (
    <button
      type="button"
      onClick={actions.openResource}
      disabled={!actions.canOpen}
      className={cn(
        "group relative flex w-full items-center gap-3 rounded-[20px] bg-surface/92 px-3.5 py-3.5 text-left shadow-sm ring-1 ring-border/55",
        "transition-all hover:-translate-y-px hover:shadow-md hover:ring-border/85",
        compact && "rounded-2xl px-3 py-3"
      )}
    >
      <PreviewAction resource={resource} className="top-1.5 right-1.5" />
      <div className={cn("shrink-0 overflow-hidden rounded-[14px] bg-elevated", compact ? "h-12 w-12" : "h-14 w-14")}>
        <LinkThumbnail resource={resource} preview={preview} />
      </div>
      <div className="min-w-0 flex-1 pr-6">
        <p className="truncate text-sm font-medium text-text">{preview?.title || title}</p>
        {meta ? <p className="truncate text-xs text-text-secondary">{meta}</p> : null}
        <p className="mt-0.5 truncate text-[11px] text-text-tertiary">{source}</p>
      </div>
    </button>
  );
}

function FileResourceRow({ resource, compact = false, emptyLabel = "File" }) {
  const { workspace } = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const previewURL = resolveResourceRenderableURL(resource, workspace);
  const actions = createResourceActions(resource, {
    workspace,
    openLocator: (target) => ipc.openLocator(target),
    focusEntry
  });
  const title = resolveResourceTitle(resource);
  const source = resourceSourceLabel(resource);
  const meta = resourceMetaLabel(resource);

  return (
    <button
      type="button"
      onClick={actions.openResource}
      disabled={!actions.canOpen}
      className={cn(
        "group relative flex w-full items-center gap-3 rounded-[22px] bg-surface/88 px-3.5 py-3.5 text-left shadow-sm ring-1 ring-border/45",
        "transition-all hover:-translate-y-px hover:shadow-md hover:ring-border/75",
        compact && "rounded-2xl px-3 py-3"
      )}
    >
      <PreviewAction resource={resource} className="top-1.5 right-1.5" />
      <div className={cn("shrink-0 overflow-hidden rounded-2xl bg-elevated", compact ? "h-12 w-12" : "h-14 w-14")}>
        {previewURL && resourcePreviewKind(resource) === "file" && String(resource?.sourceKind ?? "") === "image" ? (
          <img src={previewURL} alt="" className="h-full w-full object-cover" loading="lazy" />
        ) : (
          <EmptyThumbnail label={emptyLabel} accent />
        )}
      </div>
      <div className="min-w-0 flex-1 pr-6">
        <p className="truncate text-sm font-medium text-text">{title}</p>
        {meta ? <p className="truncate text-xs text-text-secondary">{meta}</p> : null}
        <p className="mt-0.5 truncate text-[11px] text-text-tertiary">{source}</p>
      </div>
    </button>
  );
}

function MentionResourceRow({ resource, compact = false }) {
  const { workspace } = useWorkbenchContext();
  const { focusEntry } = useNavigationContext();
  const actions = createResourceActions(resource, {
    workspace,
    openLocator: (target) => ipc.openLocator(target),
    focusEntry
  });
  const title = resolveResourceTitle(resource);
  const source = resourceSourceLabel(resource);

  return (
    <div className={cn(
      "group relative flex items-center gap-3 rounded-[18px] bg-surface/88 px-3.5 py-3 shadow-sm ring-1 ring-border/40",
      compact && "rounded-2xl px-3 py-2.5"
    )}>
      <PreviewAction resource={resource} className="top-1.5 right-1.5" />
      <span className="inline-flex h-9 w-9 shrink-0 items-center justify-center rounded-full bg-accent/10 text-sm font-semibold text-accent">
        {title.replace(/^@/, "").charAt(0).toUpperCase()}
      </span>
      <div className="min-w-0 pr-6">
        <p className="truncate text-sm font-medium text-text">{title}</p>
        <p className="truncate text-xs text-text-tertiary">{source}</p>
      </div>
      {actions.canOpen ? (
        <button
          type="button"
          onClick={actions.openResource}
          className="sr-only"
        >
          Open
        </button>
      ) : null}
    </div>
  );
}

function Section({ title, count, children }) {
  if (count === 0) {
    return null;
  }
  return (
    <section className="space-y-3">
      <div className="flex items-baseline justify-between">
        <h3 className="text-[11px] font-semibold uppercase tracking-[0.16em] text-text-tertiary">{title}</h3>
        <span className="text-[11px] text-text-tertiary/80 tabular-nums">{count}</span>
      </div>
      {children}
    </section>
  );
}

export function ResourceCollection({ resources, compact = false }) {
  const { media, links, documents, files, mentions } = groupDisplayResources(resources ?? []);

  if (media.length + links.length + documents.length + files.length + mentions.length === 0) {
    return null;
  }

  return (
    <div className={cn("space-y-7", compact && "space-y-5")}>
      <Section title="Media" count={media.length}>
        <div className={cn("grid gap-2.5", compact ? "grid-cols-3" : "grid-cols-3 lg:grid-cols-4")}>
          {media.map((resource) => (
            <MediaResourceCard key={resource.id} resource={resource} compact={compact} />
          ))}
        </div>
      </Section>

      <Section title="Links" count={links.length}>
        <div className="space-y-2.5">
          {links.map((resource) => (
            <LinkResourceCard key={resource.id} resource={resource} compact={compact} />
          ))}
        </div>
      </Section>

      <Section title="Documents" count={documents.length}>
        <div className="space-y-2.5">
          {documents.map((resource) => (
            <FileResourceRow key={resource.id} resource={resource} compact={compact} emptyLabel="Doc" />
          ))}
        </div>
      </Section>

      <Section title="Files" count={files.length}>
        <div className="space-y-2.5">
          {files.map((resource) => (
            <FileResourceRow key={resource.id} resource={resource} compact={compact} emptyLabel="File" />
          ))}
        </div>
      </Section>

      <Section title="Mentions" count={mentions.length}>
        <div className="space-y-2">
          {mentions.map((resource) => (
            <MentionResourceRow key={resource.id} resource={resource} compact={compact} />
          ))}
        </div>
      </Section>
    </div>
  );
}
