import test from "node:test";
import assert from "node:assert/strict";
import { buildMentionCatalog } from "../../renderer/src/lib/mentionCatalog.js";

test("clean-room mention catalog merges structured and inline mentions across entries", () => {
  const mentions = buildMentionCatalog([
    {
      id: "entry-1",
      summaryText: "Talk to @Polymarket",
      objectMentions: []
    },
    {
      id: "entry-2",
      summaryText: "Atlas note",
      objectMentions: [{ name: "Atlas" }]
    },
    {
      id: "entry-3",
      summaryText: "Duplicate @atlas mention",
      objectMentions: []
    }
  ]);

  assert.deepEqual(mentions, ["Atlas", "Polymarket"]);
});
