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

test("clean-room rich body renderer hides generated file names in preview footer", () => {
  const html = renderRichPreviewHTML({
    previewMode: "image",
    previewURL: "file:///tmp/generated.png",
    fileName: "78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    title: "Image",
    sourceKind: "image",
    siteName: "Local file"
  });

  assert.match(html, />Image</);
  assert.doesNotMatch(html, /78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44/);
});

test("clean-room rich body renderer does not show placeholder image title in image cards", () => {
  const html = renderRichPreviewHTML({
    previewMode: "image",
    previewURL: "file:///tmp/generated.png",
    fileName: "78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    sourceKind: "image",
    siteName: "Local file",
    isLocal: true
  });

  assert.doesNotMatch(html, />Image</);
  assert.doesNotMatch(html, />Resource</);
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
