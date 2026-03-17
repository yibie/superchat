import test from "node:test";
import assert from "node:assert/strict";
import { getEntryDisplayText, presentBacklinks, stripEntryReferenceMarkup, tokenizeEntryBody } from "../../renderer/src/components/entries/entryMeta.js";

test("clean-room entry meta strips reference markup from displayed body text", () => {
  assert.equal(stripEntryReferenceMarkup("Need [[supports|Atlas Spec]] review"), "Need review");
  assert.equal(getEntryDisplayText({ summaryText: "[[supports|Atlas Spec]]" }), "");
});

test("clean-room entry meta tokenizes inline references with default informs relation", () => {
  const segments = tokenizeEntryBody({
    summaryText: "Need [[Atlas Spec]] now",
    references: [
      { id: "ref-1", label: "Atlas Spec", relationKind: "informs", targetID: "entry-1", isResolved: false }
    ]
  });

  assert.equal(segments[1].reference.label, "Atlas Spec");
  assert.equal(segments[1].reference.relationKind, "informs");
  assert.equal(segments[1].reference.relationTone, "reference-token-informs");
});

test("clean-room entry meta exposes tone for explicit relation without visible prefix", () => {
  const segments = tokenizeEntryBody({
    summaryText: "Need [[supports|Atlas Spec]] now",
    references: [
      { id: "ref-1", label: "Atlas Spec", relationKind: "supports", targetID: "entry-1", isResolved: true, targetSummaryText: "Atlas Spec" }
    ]
  });

  assert.equal(segments[1].reference.relationTone, "reference-token-supports");
  assert.equal(segments[1].reference.label, "Atlas Spec");
});

test("clean-room entry meta presents backlinks for rendering", () => {
  const backlinks = presentBacklinks({
    id: "entry-2",
    incomingBacklinks: [
      { id: "b1", sourceEntryID: "entry-1", sourceThreadID: "thread-1", sourceSummaryText: "Launch plan", relationKind: "supports" }
    ]
  });

  assert.deepEqual(backlinks, [
    {
      id: "b1",
      sourceEntryID: "entry-1",
      sourceThreadID: "thread-1",
      sourceSummaryText: "Launch plan",
      relationKind: "supports",
      title: "<- supports from Launch plan"
    }
  ]);
});
