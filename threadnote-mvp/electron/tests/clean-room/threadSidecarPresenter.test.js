import test from "node:test";
import assert from "node:assert/strict";
import { presentThreadSidecar } from "../../src/application/presenters/threadSidecarPresenter.js";

test("clean-room sidecar presenter exposes unavailable restart state without ai snapshot", () => {
  const sidecar = presentThreadSidecar({
    threadView: {
      thread: { id: "thread-1" },
      anchors: [{ stateSummary: "Legal review open", openLoops: ["Assign owner"] }],
      resources: [],
      resourceCounts: { linkCount: 0, mediaCount: 0, mentionCount: 0, totalCount: 0 }
    }
  });

  assert.equal(sidecar.restart.status, "unavailable");
  assert.equal(sidecar.restart.debug.currentJudgment, "Legal review open");
  assert.deepEqual(sidecar.restart.debug.openLoops, ["Assign owner"]);
});

test("clean-room sidecar presenter exposes restart blocks and thread resources", () => {
  const sidecar = presentThreadSidecar({
    threadView: {
      thread: { id: "thread-1" },
      anchors: [],
      aiSnapshot: {
        threadID: "thread-1",
        headline: "Clear the legal bottleneck",
        currentJudgment: "Atlas is blocked by legal review",
        openLoops: ["Confirm reviewer"],
        synthesizedAt: "2026-03-15T12:00:00.000Z",
        blocks: [
          {
            kind: "gap",
            title: "Main gap",
            summary: "Legal owner missing",
            items: ["Pick one owner"],
            tone: "warning"
          }
        ]
      },
      resources: [
        { id: "resource-1", kind: "link", title: "Spec", previewText: "Launch spec" }
      ],
      resourceCounts: { linkCount: 1, mediaCount: 0, mentionCount: 0, totalCount: 1 }
    }
  });

  assert.equal(sidecar.restart.status, "ready");
  assert.equal(sidecar.restart.blocks.length, 1);
  assert.equal(sidecar.resources.totalCount, 1);
  assert.equal(sidecar.resources.resources[0].title, "Spec");
});
