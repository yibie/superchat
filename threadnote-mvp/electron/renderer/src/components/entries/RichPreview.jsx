import { useRichPreviews } from "../../hooks/useRichPreviews.js";
import { Skeleton } from "../shared/Skeleton.jsx";
import { ipc } from "../../lib/ipc.js";

function extractDomain(url) {
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return url;
  }
}

/**
 * iMessage-style preview card for URL entries.
 * Large image on top, title + domain below.
 * Falls back to a clickable domain card when no preview data is available.
 */
export function RichPreview({ entryID, url }) {
  const { getPreview } = useRichPreviews();
  const preview = getPreview(entryID);

  if (!preview || preview.loading) {
    return (
      <div className="mt-2 rounded-xl border border-border p-3 space-y-2">
        <Skeleton className="h-40 w-full rounded-lg" />
        <Skeleton className="h-4 w-3/4" />
        <Skeleton className="h-3 w-1/3" />
      </div>
    );
  }

  const resolvedUrl = preview.url || url;
  const domain = extractDomain(resolvedUrl || "");
  const title = preview.title;

  const handleClick = () => {
    if (resolvedUrl) ipc.openLocator(resolvedUrl);
  };

  return (
    <div
      role="link"
      onClick={handleClick}
      className="mt-2 rounded-xl overflow-hidden bg-surface border border-border cursor-pointer hover:border-text-tertiary transition-colors"
    >
      {preview.image && (
        <img
          src={preview.image}
          alt=""
          className="w-full h-40 object-cover"
          loading="lazy"
        />
      )}
      <div className="px-3 py-2">
        {title && (
          <p className="text-sm font-medium text-text truncate">{title}</p>
        )}
        <p className="text-2xs text-text-tertiary truncate">{domain}</p>
      </div>
    </div>
  );
}
