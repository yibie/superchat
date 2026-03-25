import test from "node:test";
import assert from "node:assert/strict";
import { buildResourcePresentation } from "../../src/domain/resources/resourcePresentation.js";

test("clean-room resource presentation prefers meaningful file names", () => {
  const presentation = buildResourcePresentation({
    displayName: "Quarterly brief.pdf",
    fileName: "brief.pdf",
    locator: "attachments/brief.pdf",
    sourceKind: "document",
    isLocal: true
  });

  assert.equal(presentation.title, "Quarterly brief.pdf");
  assert.equal(presentation.subtitle, "Quarterly brief.pdf");
  assert.equal(presentation.kindLabel, "Document");
});

test("clean-room resource presentation hides generated file names", () => {
  const presentation = buildResourcePresentation({
    fileName: "78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    locator: "attachments/78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    sourceKind: "image",
    isLocal: true
  });

  assert.equal(presentation.title, "Image");
  assert.equal(presentation.subtitle, "");
  assert.equal(presentation.kindLabel, "Image");
});

test("clean-room resource presentation can suppress placeholder image titles", () => {
  const presentation = buildResourcePresentation({
    fileName: "78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    locator: "attachments/78bbc1e2f66668e67d459a22c681d77cad9dd68f2133ce8e32c418dd5d6e4c44.png",
    sourceKind: "image",
    isLocal: true,
    includeKindTitle: false
  });

  assert.equal(presentation.title, "");
  assert.equal(presentation.subtitle, "");
  assert.equal(presentation.kindLabel, "Image");
});

test("clean-room resource presentation falls back to remote domain", () => {
  const presentation = buildResourcePresentation({
    locator: "https://example.com/spec",
    sourceKind: "url",
    isLocal: false
  });

  assert.equal(presentation.title, "example.com");
  assert.equal(presentation.subtitle, "example.com");
  assert.equal(presentation.kindLabel, "Link");
});
