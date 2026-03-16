import test from "node:test";
import assert from "node:assert/strict";
import { presentTimelineEntries } from "../../src/application/presenters/timelineEntryPresenter.js";

test("clean-room timeline presenter builds reply cards and route actions for inbox entries", () => {
  const cards = presentTimelineEntries({
    entries: [
      {
        id: "entry-1",
        kind: "note",
        summaryText: "Inbox note",
        createdAt: "2026-03-15T10:00:00.000Z",
        objectMentions: [{ name: "Atlas" }],
        references: [{ label: "Plan" }]
      }
    ],
    allEntries: [
      {
        id: "entry-1",
        kind: "note",
        summaryText: "Inbox note",
        createdAt: "2026-03-15T10:00:00.000Z",
        objectMentions: [{ name: "Atlas" }],
        references: [{ label: "Plan" }]
      },
      {
        id: "reply-1",
        kind: "note",
        parentEntryID: "entry-1",
        summaryText: "Reply",
        createdAt: "2026-03-15T10:05:00.000Z"
      }
    ],
    threads: [{ id: "thread-1", title: "Atlas launch" }],
    mode: "stream"
  });

  assert.equal(cards[0].canRoute, true);
  assert.equal(cards[0].replies.length, 1);
  assert.deepEqual(cards[0].objectNames, ["Atlas"]);
  assert.deepEqual(cards[0].referenceBadges, ["Plan"]);
});

test("clean-room timeline presenter exposes relation badges for thread entries", () => {
  const cards = presentTimelineEntries({
    entries: [
      {
        id: "entry-1",
        kind: "claim",
        threadID: "thread-1",
        summaryText: "Claim",
        createdAt: "2026-03-15T10:00:00.000Z"
      }
    ],
    allEntries: [],
    discourseRelations: [{ sourceEntryID: "entry-1", targetEntryID: "entry-2", kind: "supports" }],
    mode: "thread"
  });

  assert.equal(cards[0].routeState, "routed");
  assert.deepEqual(cards[0].relationBadges, ["Supports"]);
});
