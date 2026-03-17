import test from "node:test";
import assert from "node:assert/strict";
import { resolveOpenThreadResult, upsertThreadDetail } from "../../renderer/src/hooks/threadDetailState.js";

test("clean-room thread detail state upserts thread detail by thread id", () => {
  const next = upsertThreadDetail({}, {
    thread: { id: "thread-b", title: "Beta" },
    entries: []
  });

  assert.equal(next["thread-b"].thread.title, "Beta");
});

test("clean-room thread detail state ignores stale open-thread response tokens", () => {
  const activeTokens = new Map([["thread-b", "new-token"]]);
  const currentCache = {
    "thread-a": { thread: { id: "thread-a", title: "Alpha" }, entries: [] }
  };
  const currentLoading = { "thread-b": true };

  const resolved = resolveOpenThreadResult({
    threadDetailsByID: currentCache,
    threadLoadingByID: currentLoading,
    requestedThreadID: "thread-b",
    expectedToken: "old-token",
    activeTokens,
    thread: { thread: { id: "thread-b", title: "Beta" }, entries: [] }
  });

  assert.equal(resolved.accepted, false);
  assert.deepEqual(resolved.threadDetailsByID, currentCache);
  assert.equal(resolved.threadLoadingByID["thread-b"], false);
});

test("clean-room thread detail state ignores mismatched thread payloads and accepts matching ones", () => {
  const activeTokens = new Map([["thread-b", "token-b"]]);

  const mismatched = resolveOpenThreadResult({
    threadDetailsByID: {},
    threadLoadingByID: { "thread-b": true },
    requestedThreadID: "thread-b",
    expectedToken: "token-b",
    activeTokens,
    thread: { thread: { id: "thread-a", title: "Alpha" }, entries: [] }
  });
  assert.equal(mismatched.accepted, false);
  assert.deepEqual(mismatched.threadDetailsByID, {});

  const accepted = resolveOpenThreadResult({
    threadDetailsByID: {},
    threadLoadingByID: { "thread-b": true },
    requestedThreadID: "thread-b",
    expectedToken: "token-b",
    activeTokens,
    thread: { thread: { id: "thread-b", title: "Beta" }, entries: [] }
  });
  assert.equal(accepted.accepted, true);
  assert.equal(accepted.threadDetailsByID["thread-b"].thread.title, "Beta");
  assert.equal(accepted.threadLoadingByID["thread-b"], false);
});
