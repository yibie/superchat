import { useMemo } from "react";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { ipc } from "../../lib/ipc.js";
import { cn } from "../../lib/cn.js";
import { SurfaceHeader } from "../shell/SurfaceHeader.jsx";

function extractDomain(url) {
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return url;
  }
}

function extractFilename(path) {
  if (!path) return null;
  const segments = path.split("/");
  return segments[segments.length - 1] || path;
}

/* ── Media: thumbnail grid ── */

function MediaCard({ resource }) {
  const locator = resource.previewText || resource.entry?.body?.url;
  const filename = extractFilename(locator);

  return (
    <button
      onClick={() => locator && ipc.openLocator(locator)}
      className={cn(
        "aspect-square rounded-lg border border-border bg-surface-alt overflow-hidden",
        "flex items-center justify-center transition-colors hover:bg-elevated",
        locator ? "cursor-pointer" : "cursor-default"
      )}
    >
      {locator ? (
        <img
          src={locator}
          alt={filename ?? "media"}
          className="w-full h-full object-cover"
          onError={(e) => {
            e.target.style.display = "none";
            e.target.nextSibling?.classList.remove("hidden");
          }}
        />
      ) : null}
      <div className={cn("flex flex-col items-center gap-1 p-2 text-center", locator && "hidden")}>
        <span className="text-2xl text-text-tertiary">
          {resource.entry?.body?.source?.sourceKind === "video" ? "\u25B6" : "\u{1F4CE}"}
        </span>
        <span className="text-[11px] text-text-tertiary truncate max-w-full">
          {filename ?? "Media"}
        </span>
      </div>
    </button>
  );
}

/* ── Links: rich list rows ── */

function LinkRow({ resource }) {
  const url = resource.entry?.body?.url || resource.previewText;
  const domain = url ? extractDomain(url) : null;
  const title = resource.title || domain || "Untitled link";

  return (
    <button
      onClick={() => url && ipc.openLocator(url)}
      className={cn(
        "flex items-start gap-3 w-full p-3 rounded-lg border border-border bg-surface",
        "text-left transition-colors hover:bg-elevated cursor-pointer"
      )}
    >
      <span className="shrink-0 mt-0.5 text-base text-text-tertiary">{"\u{1F517}"}</span>
      <div className="flex flex-col gap-0.5 min-w-0">
        <span className="text-sm font-medium text-text truncate">{title}</span>
        {domain && (
          <span className="text-xs text-text-tertiary truncate">{domain}</span>
        )}
      </div>
    </button>
  );
}

/* ── Mentions: compact list rows ── */

function MentionRow({ resource }) {
  const label = resource.title || "Unknown";

  return (
    <div
      className={cn(
        "flex items-center gap-3 w-full p-3 rounded-lg border border-border bg-surface",
        "transition-colors hover:bg-elevated"
      )}
    >
      <span className="shrink-0 w-6 h-6 rounded-full bg-accent/10 text-accent flex items-center justify-center text-xs font-semibold">
        {label.replace(/^@/, "").charAt(0).toUpperCase()}
      </span>
      <span className="text-sm text-text truncate">{label}</span>
    </div>
  );
}

/* ── Section wrapper ── */

function Section({ title, count, children }) {
  if (count === 0) return null;
  return (
    <div className="flex flex-col gap-2">
      <h3 className="text-xs font-semibold text-text-tertiary uppercase tracking-wider">
        {title}
      </h3>
      {children}
    </div>
  );
}

/* ── Main surface ── */

export function ResourcesSurface() {
  const { home } = useWorkbenchContext();
  const resources = home?.resources ?? [];

  const { media, links, mentions } = useMemo(() => {
    const m = [], l = [], n = [];
    for (const r of resources) {
      if (r.kind === "media") m.push(r);
      else if (r.kind === "link") l.push(r);
      else if (r.kind === "mention") n.push(r);
    }
    return { media: m, links: l, mentions: n };
  }, [resources]);

  return (
    <div className="flex flex-col h-full">
      <SurfaceHeader title="Resources" count={resources.length} className="max-w-2xl mx-auto" />

      {resources.length === 0 ? (
        <div className="flex-1 flex items-center justify-center">
          <p className="text-sm text-text-tertiary">No resources yet</p>
        </div>
      ) : (
        <div className="flex-1 overflow-y-auto">
          <div className="max-w-2xl mx-auto px-6 py-4">
            <div className="flex flex-col gap-6">
          <Section title="Media" count={media.length}>
            <div className="grid grid-cols-3 gap-2">
              {media.map((r) => (
                <MediaCard key={r.id} resource={r} />
              ))}
            </div>
          </Section>

          <Section title="Links" count={links.length}>
            <div className="flex flex-col gap-2">
              {links.map((r) => (
                <LinkRow key={r.id} resource={r} />
              ))}
            </div>
          </Section>

          <Section title="Mentions" count={mentions.length}>
            <div className="flex flex-col gap-2">
              {mentions.map((r) => (
                <MentionRow key={r.id} resource={r} />
              ))}
            </div>
          </Section>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
