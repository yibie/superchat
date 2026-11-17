# Decision: A question to the person ends the run and waits for the answer

Status: proposed

## Problem

At level `ask`, `superchat-tool-call` asks the person through
`superchat-permission-ask-function`, whose default reads one key with
`read-char-choice`. That holds Emacs until the person answers. It is right
when the person is at the chat buffer; it is wrong for a run nobody watches
(one started by a timer, an inbox event or another package): the first
`write` or `exec` call would freeze Emacs until someone comes back.

Every other primitive of an agent that works in the background (an inbox,
timers, a way to send results) needs runs that can ask without blocking.
Anthropic's article "Building always-on agents" calls the answer a deferred
tool call: the run ends, the session is saved mid-call, the request goes to
the person, and the answer comes back as an event that resumes it.

## Proposal

- **L2.** `superchat-permission-ask-function` may return `later` besides
  `allow`, `deny` and `always`. `superchat-tool-call` then runs nothing,
  appends an `event` of type `question` with `:data`
  `(:type "question" :call-id ID :tool NAME :summary S)` and the call's
  question as its text, and calls ON-RESULT with that event instead of a
  `tool` entry.
- **L3.** A run whose call came back as a `question` stops calling: the
  calls after it in the same response are not run. It ends with the new
  status `waiting`, frees the timeline and calls ON-DONE. A run that ends
  `waiting` appends no `Cancelled.` results: its unanswered calls stay
  open. `superchat-run` gets the key `:unattended`; when it is non-nil the
  run binds `superchat-permission-ask-function` to a function that returns
  `later`.
- **L3.** `(superchat-answer TAPE ANSWER)`, ANSWER `allow`, `deny` or
  `always`, answers the timeline's open `question`: it appends an `event` of
  type `answer`, then starts a run with no new `user` entry that first runs
  the open call with that decision (a denied call gets its `Permission
  denied` result, as now), then the remaining calls of that response in
  order, then goes on as any run. Answering a timeline with no open
  question, or one with a run, is an error.
- **L3.** `superchat-run` on a timeline with an open question is an error
  that says to answer it first. The cut-off check of
  `2026-11-17-a-run-interrupted-by-emacs-exiting` treats a `question` event
  as an ending: a waiting timeline is not cut off.
- **L4.** The chat buffer shows an open question as a line
  `⏸ waiting for your answer: NAME SUMMARY — y/n/a` above the separator,
  and `y`, `n` and `a` on it call `superchat-answer`. A run started from the
  chat buffer is attended and asks as now.
- **L0.** Event types `question` and `answer`; run status `waiting`.

## Alternatives considered

- Keep asking in the minibuffer for every run. Not taken: an unattended run
  freezes Emacs at its first `write` or `exec` call.
- Deny every call that needs asking when nobody watches. Not taken: the
  work stops for good where it should only pause, and the person never
  learns what was wanted.
- Keep the run alive while it waits, holding the timeline. Not taken: the
  article "Building always-on agents" ends the run so the worker can take
  other events meanwhile; a waiting run here would also keep its timeline
  busy for hours and be lost when Emacs exits.
- A proxy outside the agent that injects credentials, which the same article
  recommends. Not taken: `eval_elisp` and `shell` run in the person's own
  Emacs and see what the person sees; there is no outside to put a proxy in.
- A classifier that lets low-risk calls through without asking, the
  article's auto mode. Not taken now: the three levels and the classes
  `read`, `write` and `exec` already decide, and a classifier is another
  model request per call.

## Acceptance criteria

- With `:unattended t` at level `ask`, a scripted response with calls
  `echo` (read), `poke` (write) and `echo` runs the first, ends `waiting`
  at the second with one `question` event, does not run the third, and
  Emacs never reads a key.
- `superchat-answer` with `allow` runs `poke`, then the last `echo`, then
  makes the next request; with `deny` the result of `poke` says it was
  denied and the run goes on.
- `superchat-run` on a timeline with an open question is an error; after
  the answer it is not.
- Emacs exiting while a timeline waits, then starting again: the question
  is still open and `superchat-answer` works; no `interrupted` event is
  appended.
- In the chat buffer, `y` on the question line answers it.

## Risks

- A model response with several calls is half done while it waits: the
  calls before the question ran, the rest did not. Resuming must run the
  rest in order, never again the ones that ran.
- A question may wait for days. When the person answers, the world the call
  was meant for may have changed; the run goes on from the Tape as it is.
- `superchat-chat.el` is at 1054 of 1100 lines; the question line and its
  keys may not fit, and L4 may need a module of its own.
