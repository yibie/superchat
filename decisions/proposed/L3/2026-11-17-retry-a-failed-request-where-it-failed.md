# Decision: Retry a failed request where it failed

Status: proposed

## Problem

When a model request fails, the run ends with status `error`, and the
person has to say "go on" to start another. Many such failures pass on
their own: a rate limit, an overloaded server, a dropped connection.

`superchat-llm.el` passes `:max-retries 2` to the bridge, so the bridge
already tries a failed request again, but out of sight. No entry says that
it waited or why, and the waits are the bridge's, not ours. A retry added
in L3 on top of it would multiply the two counts.

ON-ERROR gets only a string, so a policy above L2 could only decide what to
retry by reading error text.

A run can be taken up again at the failed request without losing anything:
the `assistant` entry is appended only on ON-DONE, a tool runs only after
its response is whole, and each round builds its messages from the Tape
(L3, "A run", step 1). Text streamed before the failure is shown but never
appended.

## Proposal

- **L2.** `superchat-llm-request` makes one attempt: it passes
  `:max-retries 0` to the bridge. ON-ERROR is called with a plist
  `(:message S :kind KIND :status N :retry-after SECONDS)`; `:status` and
  `:retry-after` are present only when known. KIND is one of `rate-limit`
  (status 429), `server` (status 500 and above), `timeout`, `network`,
  `empty` (the response ended with neither text nor calls) and `other`.
  `superchat-llm.el`, the only file that knows the bridge, takes these
  facts from what the bridge gives; nothing above it reads error text.
- **L3.** When a request of a run fails with KIND `rate-limit`, `server`,
  `timeout`, `network` or `empty`, the run does not end. It appends an
  `event` of type `retry` with `:data` `(:kind :status :wait :attempt :of)`
  and a line a person can read, such as
  `Request failed (429); trying again in 8 s (2/5).`, waits, and goes back
  to step 1. The waits are 2, 4, 8, 16 and 32 seconds, from a `defvar` a
  test can bind; a `:retry-after` is used instead, up to 60 seconds. After
  the fifth retry the run ends with status `error` as it does now. The count
  starts again for each request of the run. `superchat-cancel` during a wait
  stops the timer and ends the run with status `cancelled`.
  `superchat-run-active` shows the run as `running` while it waits. Only
  requests are retried, never a tool call. Handoff requests are left as
  they are.
- **L4.** On a `retry` event the chat buffer drops the text streamed for
  the failed request and its waiting line counts down to the next try.
- **L0.** The table of entry kinds names the event type `retry`.

Each layer's part is its own commit, L2 first.

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
- User options for the count and the waits. Not taken now: rule 2 allows
  no `defcustom` the specification does not list; constants come first.
- Switch to another model or provider when a request keeps failing. Not
  taken: nothing here needs it, and the DeepSeek Harness note keeps it out
  of request recovery as another problem.

## Acceptance criteria

- With a scripted model that fails with KIND `server` twice and then
  answers, a run ends `done`, its Tape has two `retry` events and one
  `assistant` entry, and no tool ran twice.
- A failure of KIND `other` ends the run at once with status `error`, as
  now.
- Five retryable failures and a sixth end the run with status `error`
  after five `retry` events.
- A `:retry-after` of 3 seconds makes the wait 3 seconds; one of 600 makes
  it 60.
- `superchat-cancel` during a wait ends the run `cancelled`, and no request
  is made after it.
- `superchat-llm-request` passes `:max-retries 0`, and a test pins how the
  bridge's error message becomes `:kind` and `:status` for 429, 500, 503
  and a timeout.
- The chat buffer shows no text of a failed request after its `retry`
  event.
- Tests wait no longer than 0.1 s: they bind the waits.

## Risks

- A retried request may be billed again, even when no text came back. Five
  retries bound it.
- The status and the kind are taken from the bridge's error message. A
  change of that message in the bridge breaks the mapping; the L2 test
  pins it.
- `empty` turns a response that ends with nothing, now a `done` run with
  an empty answer, into a retry. A model that answers nothing on purpose
  would be asked six times.
- `superchat-chat.el` is at 1042 of 1100 lines; dropping streamed text and
  counting down must fit in what is left.
