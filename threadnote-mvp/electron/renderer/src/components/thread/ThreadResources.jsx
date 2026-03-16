import { cn } from "../../lib/cn.js";

function extractDomain(url) {
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return url;
  }
}

export function ThreadResources({ entries }) {
  const resources = (entries ?? []).filter(
    (e) => e.body?.url || e.kind === "source"
  );

  if (resources.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center py-10 text-text-tertiary">
        <p className="text-sm">No resources yet.</p>
        <p className="text-xs mt-1">Add source entries or entries with URLs.</p>
      </div>
    );
  }

  return (
    <div className="grid grid-cols-1 gap-2">
      {resources.map((entry) => {
        const url = entry.body?.url;
        const title = entry.summaryText || entry.body?.text || url || "Untitled";
        const domain = url ? extractDomain(url) : null;

        return (
          <a
            key={entry.id}
            href={url ?? "#"}
            target="_blank"
            rel="noopener noreferrer"
            className={cn(
              "block rounded-md border border-border bg-surface px-3 py-2.5",
              "hover:bg-elevated transition-colors",
              !url && "pointer-events-none"
            )}
          >
            <p className="text-sm font-medium text-text truncate">{title}</p>
            {domain && (
              <p className="mt-0.5 text-xs text-text-tertiary truncate">{domain}</p>
            )}
            {url && (
              <p className="mt-0.5 text-[11px] text-text-tertiary truncate opacity-60">
                {url}
              </p>
            )}
          </a>
        );
      })}
    </div>
  );
}
