import test from "node:test";
import assert from "node:assert/strict";
import { presentStreamTimeline } from "../../src/application/presenters/streamTimelinePresenter.js";

test("clean-room stream presenter groups root inbox entries by day and sorts descending", () => {
  const sections = presentStreamTimeline({
    entries: [
      {
        id: "reply-1",
        kind: "note",
        summaryText: "Ignore reply rows here",
        createdAt: "2026-03-15T09:45:00.000Z",
        parentEntryID: "entry-1"
      },
      {
        id: "entry-1",
        kind: "claim",
        summaryText: "Atlas legal review is blocking launch",
        createdAt: "2026-03-15T10:30:00.000Z",
        objectMentions: [{ name: "Atlas" }]
      },
      {
        id: "entry-2",
        kind: "source",
        summaryText: "Spec link",
        createdAt: "2026-03-14T08:00:00.000Z",
        body: { url: "https://example.com/spec" }
      }
    ]
  });

  assert.equal(sections.length, 2);
  assert.equal(sections[0].entries.length, 1);
  assert.equal(sections[0].entries[0].id, "entry-1");
  assert.equal(sections[0].entries[0].routeState, "inbox");
  assert.deepEqual(sections[0].entries[0].objectNames, ["Atlas"]);
  assert.equal(sections[1].entries[0].sourceLabel, "example.com");
});

test("clean-room stream presenter marks routed and routing states", () => {
  const sections = presentStreamTimeline({
    entries: [
      {
        id: "entry-1",
        kind: "note",
        summaryText: "Already in thread",
        createdAt: "2026-03-15T10:30:00.000Z",
        threadID: "thread-1"
      },
      {
        id: "entry-2",
        kind: "note",
        summaryText: "Routing now",
        createdAt: "2026-03-15T09:30:00.000Z",
        inboxState: "routing"
      }
    ]
  });

  assert.equal(sections[0].entries[0].routeState, "routed");
  assert.equal(sections[0].entries[1].routeState, "routing");
});
