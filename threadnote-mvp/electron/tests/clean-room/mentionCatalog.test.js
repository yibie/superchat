import test from "node:test";
import assert from "node:assert/strict";
import { buildMentionCatalog } from "../../renderer/src/lib/mentionCatalog.js";

test("clean-room mention catalog merges structured and inline mentions across entries", () => {
  const mentions = buildMentionCatalog([
    {
      id: "entry-1",
      threadID: "thread-b",
      createdAt: "2026-03-18T00:00:00Z",
      summaryText: "Talk to @Polymarket",
      objectMentions: []
    },
    {
      id: "entry-2",
      threadID: "thread-a",
      createdAt: "2026-03-15T00:00:00Z",
      summaryText: "Atlas note",
      objectMentions: [{ name: "Atlas" }]
    },
    {
      id: "entry-3",
      threadID: "thread-a",
      createdAt: "2026-03-16T00:00:00Z",
      summaryText: "Duplicate @atlas mention",
      objectMentions: []
    }
  ]);

  assert.deepEqual(mentions, [
    {
      id: "mention:atlas",
      name: "Atlas",
      label: "@Atlas",
      count: 2,
      lastSeenAt: "2026-03-16T00:00:00Z",
      threadIDs: ["thread-a"]
    },
    {
      id: "mention:polymarket",
      name: "Polymarket",
      label: "@Polymarket",
      count: 1,
      lastSeenAt: "2026-03-18T00:00:00Z",
      threadIDs: ["thread-b"]
    }
  ]);
});
