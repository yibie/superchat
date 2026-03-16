import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { createEntry } from "../../src/domain/models/threadnoteModels.js";
import { LinkMetadataService } from "../../src/infrastructure/metadata/linkMetadataService.js";

test("clean-room link metadata service enriches html pages and caches result", async () => {
  let fetchCount = 0;
  const service = new LinkMetadataService({
    fetchImpl: async () => {
      fetchCount += 1;
      return {
        ok: true,
        headers: {
          get(name) {
            return name === "content-type" ? "text/html" : null;
          }
        },
        async text() {
          return `
            <html>
              <head>
                <meta property="og:title" content="Atlas Launch Plan" />
                <meta property="og:description" content="Everything needed for launch." />
                <meta property="og:site_name" content="Atlas Docs" />
              </head>
            </html>
          `;
        }
      };
    }
  });
  const entry = createEntry({
    summaryText: "https://example.com/atlas"
  });

  const first = await service.getEntryRichPreview(entry);
  const second = await service.getEntryRichPreview(entry);

  assert.equal(first.title, "Atlas Launch Plan");
  assert.equal(first.siteName, "Atlas Docs");
  assert.equal(second.description, "Everything needed for launch.");
  assert.equal(fetchCount, 1);
});

test("clean-room link metadata service resolves local attachments and reports missing files", async () => {
  const workspace = fs.mkdtempSync(path.join(os.tmpdir(), "threadnote-rich-preview-"));
  const attachments = path.join(workspace, "attachments");
  fs.mkdirSync(attachments);
  const filePath = path.join(attachments, "atlas.png");
  fs.writeFileSync(filePath, "image-bytes");

  const service = new LinkMetadataService({
    workspacePathResolver: () => workspace
  });

  const existing = await service.getEntryRichPreview(createEntry({ summaryText: "attachments/atlas.png" }));
  const missing = await service.getEntryRichPreview(createEntry({ summaryText: "attachments/missing.pdf" }));

  assert.equal(existing.previewMode, "image");
  assert.equal(existing.isMissingLocalFile, false);
  assert.equal(missing.previewMode, "missing-file");
  assert.equal(missing.isMissingLocalFile, true);
});
