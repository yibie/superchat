import { useEffect, useState } from "react";
import { useRichPreviews } from "../../hooks/useRichPreviews.js";
import { Skeleton } from "../shared/Skeleton.jsx";
import { ipc } from "../../lib/ipc.js";

function extractDomain(url) {
  try {
    return new URL(url).hostname.replace(/^www\./, "");
  } catch {
    return String(url ?? "");
  }
}

/**
 * Compact horizontal preview card.
 * Optional image thumbnail on the left, title + domain on the right.
 */
export function RichPreview({ entryID, url }) {
  const { getPreview } = useRichPreviews();
  const preview = getPreview(
    entryID ? `entry:${entryID}` : url ? `locator:${url}` : null,
    () => entryID ? ipc.getEntryRichPreview(entryID) : ipc.getLocatorRichPreview(url)
  );
  const previewImage = preview?.image || preview?.previewImageURL || null;
  const [imageVisible, setImageVisible] = useState(Boolean(previewImage));

  useEffect(() => {
    setImageVisible(Boolean(previewImage));
  }, [previewImage]);

  if (preview?.loading) {
    return (
      <div className="mt-2 flex items-center gap-3 rounded-xl border border-border p-3">
        <Skeleton className="h-14 w-14 shrink-0 rounded-lg" />
        <div className="min-w-0 flex-1 space-y-2">
          <Skeleton className="h-4 w-3/4" />
          <Skeleton className="h-3 w-1/3" />
        </div>
      </div>
    );
  }

  const resolvedUrl = preview?.url || preview?.openLocator || url;
  const title = preview?.title || extractDomain(resolvedUrl || "");
  const subtitle = preview?.siteName
    || preview?.hostname
    || preview?.fileName
    || extractDomain(resolvedUrl || "");
  const isAttachmentPreview = Boolean(preview?.isLocal);
  const previewMode = preview?.previewMode || "link-card";

  const handleClick = () => {
    if (resolvedUrl) ipc.openLocator(resolvedUrl);
  };

  if (isAttachmentPreview) {
    return (
      <div
        role="link"
        onClick={handleClick}
        className="mt-2 overflow-hidden rounded-xl border border-border bg-surface cursor-pointer hover:border-text-tertiary transition-colors"
      >
        {(previewMode === "image" || previewMode === "video-player") && previewImage && imageVisible ? (
          <div className="bg-elevated/50">
            {previewMode === "video-player" ? (
              <video
                src={preview?.previewURL || resolvedUrl}
                className="max-h-64 w-full object-cover"
                muted
                preload="metadata"
                onError={() => setImageVisible(false)}
              />
            ) : (
              <img
                src={previewImage}
                alt=""
                className="max-h-64 w-full object-cover"
                loading="lazy"
                onError={() => setImageVisible(false)}
              />
            )}
          </div>
        ) : (
          <div className="flex items-center gap-3 px-4 py-3">
            <div className="flex h-11 w-11 shrink-0 items-center justify-center rounded-lg bg-elevated text-text-secondary">
              <FileGlyph sourceKind={preview?.sourceKind} />
            </div>
            <div className="min-w-0 flex-1">
              {title && <p className="text-sm font-medium text-text truncate">{title}</p>}
              <p className="text-2xs text-text-tertiary truncate">{subtitle}</p>
            </div>
          </div>
        )}
        {(previewMode === "image" || previewMode === "video-player") && (
          <div className="px-4 py-3">
            {title && <p className="text-sm font-medium text-text truncate">{title}</p>}
            <p className="text-2xs text-text-tertiary truncate">{subtitle}</p>
          </div>
        )}
      </div>
    );
  }

  return (
    <div
      role="link"
      onClick={handleClick}
      className="mt-2 flex items-center gap-3 rounded-xl bg-surface border border-border p-3 cursor-pointer hover:border-text-tertiary transition-colors"
    >
      {previewImage && imageVisible && (
        <img
          src={previewImage}
          alt=""
          className="h-14 w-14 shrink-0 rounded-lg object-cover"
          loading="lazy"
          onError={() => setImageVisible(false)}
        />
      )}
      <div className="min-w-0 flex-1">
        {title && (
          <p className="text-sm font-medium text-text truncate">{title}</p>
        )}
        <p className="text-2xs text-text-tertiary truncate">{subtitle}</p>
      </div>
    </div>
  );
}

function FileGlyph({ sourceKind }) {
  if (sourceKind === "image") {
    return (
      <svg width="18" height="18" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
        <rect x="2.5" y="3" width="11" height="10" rx="1.5" />
        <circle cx="6" cy="6.5" r="1" />
        <path d="M3.5 11l3-3 2.2 2.2 1.8-1.8 2 2.6" />
      </svg>
    );
  }

  if (sourceKind === "document") {
    return (
      <svg width="18" height="18" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
        <path d="M5 1.5h4l3 3V13a1.5 1.5 0 01-1.5 1.5h-5A1.5 1.5 0 014 13V3A1.5 1.5 0 015.5 1.5z" />
        <path d="M9 1.5v3h3" />
        <path d="M6 8h4M6 10.5h4" />
      </svg>
    );
  }

  return (
    <svg width="18" height="18" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M5 1.5h4l3 3V13a1.5 1.5 0 01-1.5 1.5h-5A1.5 1.5 0 014 13V3A1.5 1.5 0 015.5 1.5z" />
      <path d="M9 1.5v3h3" />
    </svg>
  );
}
