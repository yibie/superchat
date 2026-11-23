# Decision: Completion sends the code around point and stays off the Tape

Status: implemented

## Problem

A person asked whether Superchat completes code as ECA does. It did not:
the only completion at point was of `/` commands in the chat input. A
completion is asked for at every pause in typing, so what each request
sends, and whether it is kept, decide its cost and the Tape's size.

## Decision

`superchat-complete.el`, a new L5 module, adds `superchat-complete-mode`,
a buffer-local minor mode the person turns on and off, and the command
`superchat-complete`. After a pause of 0.3 s at a point worth asking at, one
`superchat-llm-request` with reasoning off sends the text around point with
a `<CURSOR>` marker: in a Lisp mode the top-level form at point and the two
before it, to the line that starts the next one; elsewhere 60 lines before
and 20 after. The answer shows as ghost text. `TAB` inserts it, typing its
next character shortens it without a request, and an answer for a buffer
or point that changed is dropped. Nothing goes on the Tape.

## Alternatives considered

- **The whole buffer with a marker, as ECA sends.** Tried in a prototype:
  2.4 to 4.2 times the characters of the window, and slightly better
  answers in three holes, one request each. Rejected for its cost on every
  pause.
- **The whole buffer with line numbers and a set of JSON edits back, as
  avante.nvim asks.** It can change lines away from point, which this
  cannot, but sends more than ECA does. Rejected for the same cost.
- **A fixed window of lines in every mode.** Kept for modes that are not
  Lisp. In Lisp the forms around point sent half the characters of the
  window, and their answers were no worse in the prototype.
- **A fill-in-the-middle endpoint.** DeepSeek has one (`/beta/completions`,
  `prompt` and `suffix`), and it needs no system prompt. The bridge speaks
  only Chat Completions and Anthropic Messages, and FIM was no faster in the
  prototype, so it waits.
- **An `event` on the Tape for each accepted completion.** Not taken: an
  accepted completion is already in the buffer it was typed into, and the
  person and the model recall what was decided, not each line typed.

## Consequences

- Completion costs one request per pause at a point worth asking at; the
  person turns the mode on only where they want it.
- The answer's quality and latency are the configured model's: the module
  only chooses what is sent. A model that reasons answers slower, so
  reasoning is off for these requests.
- Completion goes through `superchat-llm-request`, not `superchat-run`, so
  it has no tools, no permission and no history.
