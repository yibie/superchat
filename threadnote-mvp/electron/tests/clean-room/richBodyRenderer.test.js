import test from "node:test";
import assert from "node:assert/strict";
import { renderRichPreviewHTML } from "../../src/renderer-clean/components/richBodyRenderer.js";

test("clean-room rich body renderer outputs missing local file fallback", () => {
  const html = renderRichPreviewHTML({
    previewMode: "missing-file",
    title: "atlas.pdf",
    openLocator: "/tmp/atlas.pdf"
  });

  assert.match(html, /Missing Local File/);
  assert.match(html, /atlas\.pdf/);
});

test("clean-room rich body renderer outputs image preview with escaped attributes", () => {
  const html = renderRichPreviewHTML({
    previewMode: "image",
    previewURL: "file:///tmp/atlas.png",
    title: "<Atlas>",
    sourceKind: "image",
    siteName: "Local file"
  });

  assert.match(html, /rich-preview-image/);
  assert.match(html, /&lt;Atlas&gt;/);
  assert.doesNotMatch(html, /<Atlas>/);
});

test("clean-room rich body renderer outputs link card metadata for remote sources", () => {
  const html = renderRichPreviewHTML({
    previewMode: "link-card",
    sourceKind: "url",
    title: "Atlas Spec",
    siteName: "example.com",
    description: "Launch reference"
  });

  assert.match(html, /Atlas Spec/);
  assert.match(html, /example\.com/);
  assert.match(html, /Launch reference/);
});
