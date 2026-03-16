import test from "node:test";
import assert from "node:assert/strict";
import { presentResourcesDocument } from "../../src/application/presenters/resourcesDocumentPresenter.js";
import { presentSourceDetail } from "../../src/application/presenters/sourceDetailPresenter.js";

test("clean-room resources presenter groups derived resources by kind and carries thread labels", () => {
  const document = presentResourcesDocument({
    threads: [{ id: "thread-1", title: "Atlas launch" }],
    counts: { linkCount: 1, mediaCount: 0, mentionCount: 1, totalCount: 2 },
    resources: [
      {
        id: "resource-1",
        kind: "link",
        title: "Spec",
        previewText: "https://example.com/spec",
        threadID: "thread-1",
        entry: { id: "entry-1", body: { url: "https://example.com/spec" } }
      },
      {
        id: "resource-2",
        kind: "mention",
        title: "Atlas owner",
        previewText: "@Atlas",
        threadID: "thread-1",
        entry: {}
      }
    ]
  });

  assert.equal(document.overview[0].count, 1);
  assert.equal(document.sections[0].items[0].threadTitle, "Atlas launch");
  assert.equal(document.sections[0].items[0].entryID, "entry-1");
  assert.equal(document.sections[2].items[0].kindLabel, "Mention");
});

test("clean-room source detail presenter exposes locator citation and thread affordance", () => {
  const detail = presentSourceDetail({
    threads: [{ id: "thread-1", title: "Atlas launch" }],
    resource: {
      kind: "link",
      title: "Spec",
      previewText: "Launch spec",
      threadID: "thread-1",
      mentionLabels: ["Atlas"],
      entry: {
        id: "entry-1",
        body: { url: "https://example.com/spec" },
        summaryText: "Spec summary"
      }
    }
  });

  assert.equal(detail.title, "Spec");
  assert.equal(detail.locator, "https://example.com/spec");
  assert.equal(detail.threadTitle, "Atlas launch");
  assert.equal(detail.citation, "Launch spec");
  assert.equal(detail.entryID, "entry-1");
});
