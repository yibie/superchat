import test from "node:test";
import assert from "node:assert/strict";
import {
  createResourceActions,
  entriesForMention,
  groupDisplayResources,
  groupMentionResources,
  resourceMetaLabel,
  resourcePreviewKind,
  resourceSourceLabel,
  resourceSummaryLabel,
  resolveResourceLocator,
  resolveResourceOpenTarget,
  resolveResourceRenderableURL,
  resolveResourceTitle
} from "../../renderer/src/components/resources/resourceViewModel.js";
import {
  isGeneratedFileName,
  resolveDisplayFileName
} from "../../src/domain/resources/richSourceDescriptor.js";

test("clean-room resource view model resolves attachment locator title and file URLs", () => {
  const resource = {
    kind: "media",
    locator: "attachments/atlas.png",
    sourceKind: "image",
    attachment: { relativePath: "attachments/atlas.png", fileName: "atlas.png", displayName: "Atlas screenshot.png" },
    entry: { summaryText: "Atlas screenshot" }
  };
  const workspace = { workspacePath: "/Users/test/Workspace.threadnote" };

  assert.equal(resolveResourceLocator(resource), "attachments/atlas.png");
  assert.equal(resolveResourceTitle(resource), "Atlas screenshot.png");
  assert.equal(resolveResourceRenderableURL(resource, workspace), "file:///Users/test/Workspace.threadnote/attachments/atlas.png");
  assert.equal(resolveResourceOpenTarget(resource, workspace), "/Users/test/Workspace.threadnote/attachments/atlas.png");
});

test("clean-room resource view model hides generated attachment names", () => {
  const resource = {
    kind: "media",
    locator: "attachments/f101135c480c6d4ce49c726af01ec2a06c230f7225a74ef88512b7fb6215b598.jpg",
    sourceKind: "image",
    attachment: { fileName: "f101135c480c6d4ce49c726af01ec2a06c230f7225a74ef88512b7fb6215b598.jpg" }
  };

  assert.equal(isGeneratedFileName("f101135c.jpg"), true);
  assert.equal(resolveDisplayFileName("f101135c.jpg"), "");
  assert.equal(resolveResourceTitle(resource), "Image");
});

test("clean-room resource view model keeps meaningful attachment names", () => {
  assert.equal(isGeneratedFileName("brief.pdf"), false);
  assert.equal(resolveDisplayFileName("brief.pdf"), "brief.pdf");
});

test("clean-room resource view model falls back to fileName for legacy attachments", () => {
  const resource = {
    kind: "media",
    locator: "attachments/brief.pdf",
    sourceKind: "document",
    attachment: { fileName: "brief.pdf" }
  };

  assert.equal(resolveResourceTitle(resource), "brief.pdf");
});

test("clean-room resource view model keeps remote links unchanged", () => {
  const resource = {
    kind: "link",
    locator: "https://example.com/spec",
    sourceKind: "url",
    entry: { summaryText: "https://example.com/spec" }
  };

  assert.equal(resolveResourceRenderableURL(resource, null), "https://example.com/spec");
  assert.equal(resolveResourceOpenTarget(resource, null), "https://example.com/spec");
  assert.equal(resourcePreviewKind(resource), "link");
});

test("clean-room resource view model groups documents into files and media into media", () => {
  const grouped = groupDisplayResources([
    { id: "m1", kind: "media", sourceKind: "image", locator: "attachments/a.png" },
    { id: "v1", kind: "media", sourceKind: "video", locator: "attachments/a.mp4" },
    { id: "f1", kind: "media", sourceKind: "document", locator: "attachments/a.pdf", attachment: { fileName: "a.pdf" } },
    { id: "f2", kind: "media", sourceKind: "", locator: "attachments/archive.zip", attachment: { fileName: "archive.zip" } },
    { id: "l1", kind: "link", sourceKind: "url", locator: "https://example.com" },
    { id: "n1", kind: "mention" }
  ]);

  assert.equal(grouped.media.length, 2);
  assert.equal(grouped.documents.length, 1);
  assert.equal(grouped.files.length, 1);
  assert.equal(grouped.links.length, 1);
  assert.equal(grouped.mentions.length, 1);
});

test("clean-room resource view model groups mention resources by name", () => {
  const grouped = groupMentionResources([
    { id: "m1", kind: "mention", mentionLabels: ["OpenAI"], entryID: "entry-1" },
    { id: "m2", kind: "mention", mentionLabels: ["OpenAI"], entryID: "entry-2" },
    { id: "m3", kind: "mention", mentionLabels: ["Figma"], entryID: "entry-3" },
    { id: "m4", kind: "mention", mentionLabels: ["OpenAI"], entryID: "entry-2" }
  ]);

  assert.deepEqual(grouped, [
    { id: "mention:openai", name: "OpenAI", label: "@OpenAI", count: 2 },
    { id: "mention:figma", name: "Figma", label: "@Figma", count: 1 }
  ]);
});

test("clean-room resource view model derives mention entry list without duplicates", () => {
  const resources = [
    {
      id: "m1",
      kind: "mention",
      mentionLabels: ["OpenAI"],
      entryID: "entry-1",
      entry: { id: "entry-1", createdAt: "2026-03-10T00:00:00Z", summaryText: "Older note" }
    },
    {
      id: "m2",
      kind: "mention",
      mentionLabels: ["OpenAI", "Figma"],
      entryID: "entry-2",
      entry: { id: "entry-2", createdAt: "2026-03-12T00:00:00Z", summaryText: "Newer note" }
    },
    {
      id: "m3",
      kind: "mention",
      mentionLabels: ["OpenAI"],
      entryID: "entry-2",
      entry: { id: "entry-2", createdAt: "2026-03-12T00:00:00Z", summaryText: "Newer note" }
    }
  ];

  assert.deepEqual(entriesForMention(resources, "@OpenAI").map((entry) => entry.id), ["entry-2", "entry-1"]);
});

test("clean-room resource view model exposes inbox source label and file meta", () => {
  const resource = {
    entryID: "entry-1",
    threadID: null,
    sourceKind: "document",
    locator: "attachments/brief.pdf",
    attachment: { fileName: "brief.pdf", mimeType: "application/pdf", size: 2097152 },
    entry: { summaryText: "Quarterly brief" }
  };

  assert.equal(resourceSourceLabel(resource), "Inbox");
  assert.equal(resourceSummaryLabel(resource), "Quarterly brief");
  assert.equal(resourceMetaLabel(resource), "PDF Document 2.0 MB");
});

test("clean-room resource view model keeps link meta minimal", () => {
  const resource = {
    kind: "link",
    locator: "https://example.com/spec",
    sourceKind: "url",
    entry: { summaryText: "Long summary should not show as link meta" }
  };

  assert.equal(resourceMetaLabel(resource), "example.com");
});

test("clean-room resource view model builds open and go-to-note actions", () => {
  const calls = [];
  const resource = {
    entryID: "entry-1",
    threadID: "thread-1",
    locator: "attachments/atlas.png",
    sourceKind: "image"
  };
  const workspace = { workspacePath: "/Users/test/Workspace.threadnote" };
  const actions = createResourceActions(resource, {
    workspace,
    openLocator(locator) {
      calls.push(["open", locator]);
    },
    focusEntry(entryID, payload) {
      calls.push(["focus", entryID, payload.threadID]);
    }
  });

  actions.openResource();
  actions.goToNote();

  assert.deepEqual(calls, [
    ["open", "/Users/test/Workspace.threadnote/attachments/atlas.png"],
    ["focus", "entry-1", "thread-1"]
  ]);
});
