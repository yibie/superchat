# Decision: Retry a failed request where it failed

Status: implemented

## Problem

When a model request failed, the run ended with status `error`, and the
person had to say "go on" to start another. Many such failures pass on
their own: a rate limit, an overloaded server, a dropped connection.

`superchat-llm.el` passed `:max-retries 2` to the bridge, so the bridge
tried a failed request again out of sight: no entry said that it waited or
why. A retry in L3 on top of it would have multiplied the two counts.
ON-ERROR got only a string, so a policy above L2 could only have decided
what to retry by reading error text.

A run can be taken up again at the failed request without losing anything:
the `assistant` entry is appended only on ON-DONE, a tool runs only after
its response is whole, and each round builds its messages from the Tape
(L3, "A run", step 1). Text streamed before the failure is shown but never
appended.

## Decision

- **L2.** `superchat-llm-request` makes one attempt: it passes
  `:max-retries 0` to the bridge. ON-ERROR gets
  `(:message S :kind KIND :status N)`, `:status` only when the message
  starts with one. KIND is `rate-limit`, `server`, `timeout`, `network`,
  `empty` or `other`. `superchat-llm.el` takes these facts from the
  bridge's error message, with the bridge's own classifier of passing
  failures (`superchat-bridge-retryable-assistant-error-p`), so a quota or
  billing limit sent as a 429 is `other`. A response with neither text nor
  calls is a failure of kind `empty`. Nothing above L2 reads error text.
- **L3.** A failure of kind `rate-limit`, `server`, `timeout`, `network` or
  `empty` appends an `event` of type `retry`, with `:data`
  `(:type "retry" :kind :status :wait :attempt :of)` (the kind a string,
  since the Tape holds no symbol) and a line such as
  `Request failed: 503 overloaded. Trying again in 2 s (1/5).`; gives it to
  ON-ENTRY; waits; and goes back to step 1. The waits are
  `superchat-agent-retry-waits`, `(2 4 8 16 32)`, one per retry; the count
  starts again when a request answers. With no wait left, or for any other
  kind, the run ends with status `error` as before. Ending a run stops the
  wait, so `superchat-cancel` during it ends the run `cancelled`. Tool
  calls and handoff requests are never retried.
- **L4.** On a `retry` event the chat buffer removes the text streamed for
  the failed request, shows the event among the turn's other entries, and
  its waiting line reads `⟳ trying again in Ns (I/N)`.
- **L0.** The event types of `SPEC.md` include `retry`.

## Alternatives considered

- Send the last prompt again when a turn ends in a known error, up to five
  times after 5, 10, 20, 40 and 80 seconds, as the agent-shell extension
  in the Zhihu answer the person shared does. Not taken: it adds a second
  `user` entry to the Tape, and a run here rebuilds its messages from the
  Tape, so nothing needs to be sent again.
- Keep the bridge's two hidden retries and add L3's on top. Not taken: the
  counts multiply and the bridge's attempts leave no entry. The DeepSeek
  Harness note "Bounded recovery for transient LLM request failures" rejects
  the same mix for the same reason.
- Let L3 decide from the error text. Not taken: the text is the bridge's
  and the provider's, and it would be read in a file that must not know the
  bridge.
- Wait as long as the provider's `Retry-After` header says. Not taken: the
  bridge reads that header for its own retries and gives the caller only
  the error message; carrying it out would change the bridge, which is a
  port of pi's package kept close to its source.
- User options for the count and the waits. Not taken: rule 2 allows no
  `defcustom` the specification does not list; a `defvar` serves the tests.
- Switch to another model or provider when a request keeps failing. Not
  taken: nothing here needs it, and the DeepSeek Harness note keeps it out
  of request recovery as another problem.

## Consequences

- A request that keeps failing is made six times in all, about a minute
  apart at the end, and each may be billed even when no text came back.
- The kind and the status come from the bridge's error message. A change of
  that message in the bridge changes the mapping; `test/test-llm.el` pins
  it for 429, 503, a quota sent as 429, 401, a timeout and a lost
  connection, through the real bridge where it can.
- A response with nothing in it, which used to end a run `done` with an
  empty answer, is now retried; a model that answers nothing on purpose is
  asked six times and then the run ends `error`.
- A run that waits is still running: the timeline stays busy and the chat
  buffer's waiting line counts down.
- Text streamed for a failed request disappears from the chat buffer when
  the retry event comes.
