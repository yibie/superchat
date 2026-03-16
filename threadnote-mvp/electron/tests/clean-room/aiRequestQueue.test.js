import test from "node:test";
import assert from "node:assert/strict";
import { AIRequestPriority, AIRequestQueue } from "../../src/infrastructure/ai/queue/aiRequestQueue.js";

test("clean-room ai request queue grants higher priority waiter first", async () => {
  const queue = new AIRequestQueue({ maxConcurrent: 1 });
  const first = await queue.acquire({ priority: AIRequestPriority.ROUTING, label: "first" });

  const order = [];
  const pendingLow = queue.acquire({ priority: AIRequestPriority.DISCOURSE, label: "low" }).then(() => {
    order.push("low");
  });
  const pendingHigh = queue.acquire({ priority: AIRequestPriority.SYNTHESIS, label: "high" }).then(() => {
    order.push("high");
  });

  queue.release(first);
  await pendingHigh;
  assert.deepEqual(order, ["high"]);

  queue.release({ id: "high" });
  await pendingLow;
  assert.deepEqual(order, ["high", "low"]);
});

test("clean-room ai request queue tracks queueDepth and activeCount", async () => {
  const queue = new AIRequestQueue({ maxConcurrent: 1 });
  const lease = await queue.acquire({ priority: AIRequestPriority.ROUTING, label: "running" });
  const pending = queue.acquire({ priority: AIRequestPriority.PREPARE, label: "pending" });

  assert.equal(queue.activeCount, 1);
  assert.equal(queue.queueDepth, 1);

  queue.release(lease);
  await pending;

  assert.equal(queue.queueDepth, 0);
  assert.equal(queue.activeCount, 1);
});

test("clean-room ai request queue cancels pending waiter via AbortSignal", async () => {
  const queue = new AIRequestQueue({ maxConcurrent: 1 });
  const running = await queue.acquire({ priority: AIRequestPriority.ROUTING, label: "running" });
  const controller = new AbortController();

  const pending = queue.acquire({
    priority: AIRequestPriority.SYNTHESIS,
    label: "cancel-me",
    signal: controller.signal
  });

  assert.equal(queue.queueDepth, 1);
  controller.abort();

  await assert.rejects(pending, /aborted/);
  assert.equal(queue.queueDepth, 0);

  queue.release(running);
});

test("clean-room ai request queue run wraps acquire and release", async () => {
  const queue = new AIRequestQueue({ maxConcurrent: 1 });
  const result = await queue.run(
    async () => {
      assert.equal(queue.activeCount, 1);
      return "done";
    },
    { priority: AIRequestPriority.ROUTING, label: "wrapped" }
  );

  assert.equal(result, "done");
  assert.equal(queue.activeCount, 0);
});

test("clean-room ai request queue drains pending when max concurrency increases", async () => {
  const queue = new AIRequestQueue({ maxConcurrent: 1 });
  const running = await queue.acquire({ priority: AIRequestPriority.ROUTING, label: "running" });

  const started = [];
  const waiterA = queue.acquire({ priority: AIRequestPriority.PREPARE, label: "a" }).then(() => {
    started.push("a");
  });
  const waiterB = queue.acquire({ priority: AIRequestPriority.PREPARE, label: "b" }).then(() => {
    started.push("b");
  });

  queue.setMaxConcurrent(2);
  await waiterA;
  assert.deepEqual(started, ["a"]);

  queue.release(running);
  await waiterB;
  assert.deepEqual(started, ["a", "b"]);
});
