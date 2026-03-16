import { randomUUID } from "node:crypto";

export const AIRequestPriority = Object.freeze({
  ROUTING: 10,
  SYNTHESIS: 5,
  DISCOURSE: 2,
  PREPARE: 4
});

export class AIRequestQueue {
  constructor({ maxConcurrent = 2 } = {}) {
    this.maxConcurrent = Math.max(1, Number(maxConcurrent) || 1);
    this.runningCount = 0;
    this.pending = [];
  }

  async acquire({ priority = 0, label = "", signal = null } = {}) {
    if (signal?.aborted) {
      throw createAbortError();
    }

    if (this.runningCount < this.maxConcurrent) {
      this.runningCount += 1;
      return {
        id: randomUUID(),
        priority: Number(priority) || 0,
        label: String(label ?? "")
      };
    }

    return new Promise((resolve, reject) => {
      const waiter = {
        id: randomUUID(),
        priority: Number(priority) || 0,
        label: String(label ?? ""),
        resolve,
        reject,
        signal,
        onAbort: null
      };

      if (signal) {
        waiter.onAbort = () => {
          this.#cancelPending(waiter.id, createAbortError());
        };
        signal.addEventListener("abort", waiter.onAbort, { once: true });
      }

      this.pending.push(waiter);
      this.pending.sort(compareWaiters);
    });
  }

  release(_lease) {
    const next = this.pending.shift();
    if (next) {
      this.#resolveWaiter(next);
      return;
    }
    this.runningCount = Math.max(0, this.runningCount - 1);
  }

  async run(task, options = {}) {
    const lease = await this.acquire(options);
    try {
      return await task();
    } finally {
      this.release(lease);
    }
  }

  setMaxConcurrent(value) {
    this.maxConcurrent = Math.max(1, Number(value) || 1);
    this.#drainPendingIfCapacityAllows();
  }

  get queueDepth() {
    return this.pending.length;
  }

  get activeCount() {
    return this.runningCount;
  }

  #drainPendingIfCapacityAllows() {
    while (this.runningCount < this.maxConcurrent && this.pending.length > 0) {
      this.runningCount += 1;
      this.#resolveWaiter(this.pending.shift());
    }
  }

  #resolveWaiter(waiter) {
    if (!waiter) {
      return;
    }
    if (waiter.signal && waiter.onAbort) {
      waiter.signal.removeEventListener("abort", waiter.onAbort);
    }
    waiter.resolve({
      id: waiter.id,
      priority: waiter.priority,
      label: waiter.label
    });
  }

  #cancelPending(waiterID, error) {
    const index = this.pending.findIndex((item) => item.id === waiterID);
    if (index < 0) {
      return false;
    }
    const [waiter] = this.pending.splice(index, 1);
    if (waiter.signal && waiter.onAbort) {
      waiter.signal.removeEventListener("abort", waiter.onAbort);
    }
    waiter.reject(error);
    return true;
  }
}

function compareWaiters(lhs, rhs) {
  if (lhs.priority === rhs.priority) {
    return lhs.label.localeCompare(rhs.label);
  }
  return rhs.priority - lhs.priority;
}

function createAbortError() {
  const error = new Error("AIRequestQueue acquire aborted");
  error.name = "AbortError";
  return error;
}
