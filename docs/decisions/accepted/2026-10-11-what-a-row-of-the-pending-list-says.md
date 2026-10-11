# What a row of the pending list says, and three small corrections

Date: 2026-10-11
Status: accepted, implemented
Source: `reports/guidance-2026-10-11/GUIDANCE.md` (slices 1 and 4), written
at the user's request ("让 fable agent 指导你") after
`reports/fixes-review-2026-10-11/REPORT.md`.

## What changed

`M-x superchat-pending` (it still writes nothing):

- **A task or record that came due and did not start says how its retries
  stand.** "not started" / "not taken up" alone when its start never failed
  in this Emacs; "tried once, tried again at HH:MM" while a retry is to
  come; "given up in this Emacs after N tries, tried again after a restart"
  when the tries are used up. The words come from two functions that only
  read: `superchat-timer-retry-state` and `superchat-work-wake-retry-state`.
- **"Asks you now" says what is asked.** A question the model put with
  `ask_user` is quoted from the Runtime entry. An approval names the tool:
  the last `approval.requested` of that execution on its conversation's
  Tape with no `approval.decided` for the same tool call.
- **`RET` reads the list again**, so a row that was looked at leaves it
  without pressing `g`.
- **FR-09.** An unsaved buffer two conversations edited leads to the one
  that edited it last (the marks are kept newest first; the row took the
  oldest).

Elsewhere:

- **FR-08.** `superchat-scope-shared-effect` counts the first holder only
  after its set-up returned. A set-up that signals holds nothing, and the
  next installation sets the resource up.
- **FR-10.** A later message is told "remarks are hung on BUFFER" only
  while that buffer exists and still carries remarks. Clearing them, or
  killing the buffer, ends it. The rule is on the reading side (the
  collector); `superchat-annotate-clear` does not reach into the
  dispatcher's table.

## Evidence

Script tests only; no real model, nobody at the keyboard.

Each new test on the code before the change, with the assertion that failed:

| Test | Assertion that was red | Value then |
|---|---|---|
| `shared-resource/an-acquire-that-signals-holds-nothing` | `(should-not (gethash 'shres-fail …))` | 1 |
| `pending/an-unsaved-buffer-two-conversations-edited-names-the-latest` | row's `:session` equals the later editor | the first editor's |
| `annotate-chat/remarks-cleared-are-no-longer-said-to-be-hung` | `(should-not (annc--earlier-p …))` | still said |
| `annotate-chat/a-killed-buffer-is-no-longer-said-to-carry-remarks` | same | still said |
| `pending/a-task-not-started-says-whether-it-is-tried-again` | detail matches "tried once, tried again at" | "…, not started" |
| `pending/a-record-not-taken-up-says-whether-it-is-tried-again` | same, "not taken up: …" | "…, not taken up" |
| `pending/an-asking-row-says-what-is-asked` | detail matches "asks to run eval_elisp" | "execution … waits for your answer" |
| `pending/going-to-a-row-reads-the-list-again` | the looked-at row is gone from the buffer | still shown |

`pending/what-is-asked-is-the-undecided-request-of-that-execution` was
added after the change and was not run on the old code; it uses
hand-written approval facts.

The reviewer's probes (they assert the gap; their files are unchanged) on
the changed code: `fr-scope/an-acquire-that-signals-leaves-a-holder-nobody-owns`,
`fr-pending/an-unsaved-buffer-two-conversations-edited-names-the-first`,
`fr-pending/a-failed-task-reads-the-same-whether-a-retry-is-armed-or-not`,
`fr-annotate/clearing-the-remarks-does-not-clear-what-the-model-is-told`
and `fr-annotate/killing-the-source-does-not-clear-what-the-model-is-told`
are red; their guards pass.

Mutations, on a copy: 15 made, 13 found at first. One of the two misses was
a redundant check, removed (`get-buffer` returns only live buffers). The
other was a test whose facts happened to be in an order that hid the
mutant; reordered, it is found. 14 of 14 after that.

## Not done, and limits

- A record whose execution the user stopped still records its outcome as
  `failed`. Telling "stopped by you" apart needs a new outcome word, which
  is the user's decision (guidance, question 1).
- What an approval asks is read by replaying the conversation of each
  asking execution when the list is drawn. Asking executions are few; the
  cost on a long conversation was not measured.
- The approval row names the tool, not its arguments.
- "tried again after a restart" is what the code does (the counts live in
  this Emacs only); nothing on the Tape says a start was given up
  (guidance, slice 3).
