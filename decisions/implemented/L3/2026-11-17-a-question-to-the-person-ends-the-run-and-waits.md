# Decision: A question to the person ends the run and waits for the answer

Status: implemented

## Problem

At level `ask`, `superchat-tool-call` asked the person through
`superchat-permission-ask-function`, whose default reads one key with
`read-char-choice`. That holds Emacs until the person answers. It is right
when the person is at the chat buffer; it is wrong for a run nobody watches
(one started by a timer, an inbox event or another package): the first
`write` or `exec` call would freeze Emacs until someone came back.

Every other primitive of an agent that works in the background (an inbox,
timers, a way to send results) needs runs that can ask without blocking.
Anthropic's article "Building always-on agents" calls the answer a deferred
tool call: the run ends, the session is saved mid-call, the request goes to
the person, and the answer comes back and resumes it.

## Decision

- **L2.** `superchat-permission-ask-function` may return `later`. Then
  `superchat-tool-call` runs nothing, appends no `permission` event, and
  gives ON-RESULT an `event` `(:type "question" :call-id ID :tool NAME
  :summary S)` instead of a `tool` entry. The answer is on the Tape too:
  `superchat-permission-answer` appends an `event`
  `(:type "answer" :call-id ID :answer NAME)`, and an `ask` decision for a
  call whose id has an answer (`superchat-permission-answered`) takes it
  without asking. These two live in `superchat-permission.el`, which owns
  asking.
- **L3.** `superchat-run` takes `:unattended`; such a run binds the ask
  function to one that returns `later` around each call. A call that comes
  back as a `question` ends the run with status `waiting`: the calls after
  it in the same response do not run and get no `Cancelled.` result, and
  the timeline is free. `superchat-question` returns the question that
  still has no answer and no result; `superchat-run` on its timeline is an
  error. `superchat-answer` appends the answer and starts a run, with no new
  `user` entry, that runs that call and the rest of its response, then goes
  on as any run.
- **L4.** While a question waits, the chat buffer's line above the
  separator reads `⏸ waiting for your answer: NAME SUMMARY — send y, n or
  a`, and sending `y`, `n` or `a` answers it; other text is refused and
  stays in the input. A run started from the chat buffer is attended and
  asks in the minibuffer, as before.
- **L0.** Event types `question` and `answer`.

Two things differ from the proposal. The answer is given by sending `y`,
`n` or `a` from the input, not by keys on the line, because that line is an
overlay where point cannot go. The check for a run cut off by Emacs exiting
needed no change: `superchat-run` refuses a timeline with a waiting
question before it looks, and once the question is answered a cut-off is
read as any other.

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
  article's auto mode. Not taken: the three levels and the classes `read`,
  `write` and `exec` already decide, and a classifier is another model
  request per call.

## Consequences

- An unattended run never reads a key; a question survives Emacs exiting,
  and `superchat-answer` works after Emacs starts again.
- A response with several calls is half done while it waits: the calls
  before the question ran, the rest run in order after the answer, and none
  runs twice.
- A question may wait for days; the run then goes on from the Tape as it is.
- Nothing starts unattended runs yet; the inbox and the timers will.
- `superchat-agent.el` is at 698 of 700 lines and `superchat-chat.el` at
  1072 of 1100; the next decisions must find room, likely in a module of
  their own. `spec/L2-capabilities.md` may hold 3800 words, up from 3750.
