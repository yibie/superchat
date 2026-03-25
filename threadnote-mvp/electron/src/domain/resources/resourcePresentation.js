import { resolveDisplayFileName } from "./richSourceDescriptor.js";

function extractDomain(locator) {
  try {
    return new URL(String(locator ?? "")).hostname.replace(/^www\./, "");
  } catch {
    return "";
  }
}

function normalizePresentationTitle(value) {
  const title = String(value ?? "").trim();
  if (!title || title === "Resource") {
    return "";
  }
  return title;
}

export function fallbackResourceTitle({ sourceKind, isLocal }) {
  if (sourceKind === "image") {
    return "Image";
  }
  if (sourceKind === "video") {
    return "Video";
  }
  if (sourceKind === "audio") {
    return "Audio";
  }
  if (sourceKind === "document") {
    return "Document";
  }
  return isLocal ? "Attachment" : "Link";
}

export function buildResourcePresentation({
  title = "",
  displayName = "",
  fileName = "",
  locator = "",
  siteName = "",
  hostname = "",
  sourceKind = "",
  isLocal = false,
  includeKindTitle = true
} = {}) {
  const normalizedTitle = normalizePresentationTitle(title);
  const normalizedDisplayName = resolveDisplayFileName(displayName);
  const displayFileName = resolveDisplayFileName(fileName);
  const fallbackDomain = isLocal ? "" : extractDomain(locator);
  const kindLabel = fallbackResourceTitle({ sourceKind, isLocal });
  const resolvedTitle = normalizedTitle
    || normalizedDisplayName
    || displayFileName
    || fallbackDomain
    || (includeKindTitle ? kindLabel : "");

  return {
    title: resolvedTitle,
    subtitle: String(siteName ?? "").trim()
      || String(hostname ?? "").trim()
      || normalizedDisplayName
      || displayFileName
      || fallbackDomain
      || "",
    kindLabel
  };
}
