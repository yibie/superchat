import test from "node:test";
import assert from "node:assert/strict";
import { presentThreadDocument } from "../../src/application/presenters/threadDocumentPresenter.js";

test("clean-room thread presenter builds header prepare view and working stream sections", () => {
  const document = presentThreadDocument({
    threadView: {
      thread: {
        id: "thread-1",
        title: "Atlas launch",
        color: "sky",
        goalLayer: {
          goalStatement: "Get Atlas ready for launch",
          currentStage: "gathering"
        }
      },
      claims: [{ id: "claim-1" }],
      memory: [{ id: "mem-1" }, { id: "mem-2" }],
      resources: [{ id: "resource-1" }],
      entries: [
        {
          id: "entry-1",
          kind: "claim",
          summaryText: "Legal review still blocks launch",
          createdAt: "2026-03-15T08:00:00.000Z",
          sessionID: "session-a",
          objectMentions: [{ name: "Atlas" }]
        },
        {
          id: "entry-2",
          kind: "source",
          summaryText: "Spec link",
          createdAt: "2026-03-15T08:20:00.000Z",
          sessionID: "session-a",
          body: { url: "https://example.com/spec" }
        },
        {
          id: "entry-3",
          kind: "note",
          summaryText: "Plan owner sync",
          createdAt: "2026-03-14T10:00:00.000Z"
        }
      ]
    },
    preparedView: {
      type: "writing",
      title: "Atlas unblock draft",
      openLoops: ["Confirm legal owner"],
      recommendedNextSteps: ["Book review"],
      contentState: { status: "ready", message: "Prepared by Ollama." }
    }
  });

  assert.equal(document.header.title, "Atlas launch");
  assert.equal(document.header.stageLabel, "Gathering");
  assert.equal(document.header.entryCount, 3);
  assert.equal(document.preparedView.title, "Atlas unblock draft");
  assert.equal(document.workingStreamSections.length, 2);
  assert.equal(document.workingStreamSections[0].items.length, 2);
  assert.deepEqual(document.workingStreamSections[0].items[0].objectNames, ["Atlas"]);
  assert.equal(document.workingStreamSections[0].items[1].sourceLabel, "example.com");
});
