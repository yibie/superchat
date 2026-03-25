import { buildResourcePresentation } from "../../domain/resources/resourcePresentation.js";

function escapeHTML(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

export function renderRichPreviewHTML(preview, { compact = false } = {}) {
  if (!preview) {
    return "";
  }

  const presentation = buildResourcePresentation({
    title: preview.title,
    displayName: preview.displayName,
    fileName: preview.fileName,
    locator: preview.locator,
    siteName: preview.siteName,
    hostname: preview.hostname,
    sourceKind: preview.sourceKind,
    isLocal: Boolean(preview.isLocal),
    includeKindTitle: preview.previewMode !== "image"
  });
  const displayTitle = presentation.title || (preview.previewMode === "image" ? "" : (preview.locator || "Resource"));
  const displayAttachmentTitle = presentation.title || presentation.kindLabel;

  if (preview.previewMode === "missing-file") {
    return `
      <div class="rich-preview-card rich-preview-missing">
        <div class="nav-label">Missing Local File</div>
        <h4>${escapeHTML(displayAttachmentTitle)}</h4>
        <p>${escapeHTML(preview.openLocator || preview.locator || "Unavailable")}</p>
      </div>
    `;
  }

  if (preview.previewMode === "image") {
    return `
      <div class="rich-preview-card rich-preview-media">
        <img class="rich-preview-image" src="${escapeHTML(preview.previewURL || preview.previewImageURL || preview.locator)}" alt="${escapeHTML(preview.title || "Image")}" />
        ${renderPreviewFooter(preview, compact)}
      </div>
    `;
  }

  if (preview.previewMode === "video-player") {
    return `
      <div class="rich-preview-card rich-preview-media">
        <video class="rich-preview-video" controls preload="metadata" src="${escapeHTML(preview.previewURL || preview.locator)}"></video>
        ${renderPreviewFooter(preview, compact)}
      </div>
    `;
  }

  if (preview.previewMode === "audio-player") {
    return `
      <div class="rich-preview-card rich-preview-audio">
        ${compact ? "" : `<div class="nav-label">Audio</div>`}
        <audio class="rich-preview-audio-player" controls preload="metadata" src="${escapeHTML(preview.previewURL || preview.locator)}"></audio>
        ${renderPreviewFooter(preview, compact)}
      </div>
    `;
  }

  return `
    <div class="rich-preview-card ${preview.previewMode === "video-card" ? "rich-preview-video-card" : ""}">
      ${preview.previewImageURL && !compact ? `<img class="rich-preview-thumb" src="${escapeHTML(preview.previewImageURL)}" alt="${escapeHTML(preview.title || "")}" />` : ""}
      <div class="rich-preview-body">
        ${compact ? "" : `<div class="nav-label">${escapeHTML(formatKind(preview.sourceKind))}</div>`}
        <h4>${escapeHTML(displayTitle)}</h4>
        ${preview.siteName ? `<p class="rich-preview-meta">${escapeHTML(preview.siteName)}</p>` : ""}
        ${(preview.description || (!compact ? preview.locator : "")) ? `<p>${escapeHTML(preview.description || preview.locator || "")}</p>` : ""}
      </div>
    </div>
  `;
}

function renderPreviewFooter(preview, compact) {
  const presentation = buildResourcePresentation({
    title: preview.title,
    displayName: preview.displayName,
    fileName: preview.fileName,
    locator: preview.locator,
    siteName: preview.siteName,
    hostname: preview.hostname,
    sourceKind: preview.sourceKind,
    isLocal: Boolean(preview.isLocal),
    includeKindTitle: preview.previewMode !== "image"
  });
  const hideImageChrome = preview.previewMode === "image" && !presentation.title;
  const displayTitle = presentation.title || (hideImageChrome ? "" : (preview.locator || "Resource"));
  return `
    <div class="rich-preview-body">
      ${(compact || hideImageChrome) ? "" : `<div class="nav-label">${escapeHTML(formatKind(preview.sourceKind))}</div>`}
      ${displayTitle ? `<h4>${escapeHTML(displayTitle)}</h4>` : ""}
      ${(!compact && preview.siteName) ? `<p class="rich-preview-meta">${escapeHTML(preview.siteName)}</p>` : ""}
      ${(preview.description && !compact) ? `<p>${escapeHTML(preview.description)}</p>` : ""}
    </div>
  `;
}

function formatKind(value) {
  return String(value ?? "resource").replace(/^./, (match) => match.toUpperCase());
}
