# Decision: Follow-ups and schedules that survive Emacs exiting

Status: proposed

## Problem

The agent can act only when someone asks. It cannot say "look again in an
hour whether the build passed", nor do something every morning. Emacs
timers do not survive Emacs exiting. Anthropic's article "Building
always-on agents" separates follow-ups (a deadline the agent sets itself)
from schedules (recurring work), keeps both apart from the inbox, and has a
scheduler that polls, updates the timer and enqueues an event: "A timer
doesn't initiate a run on its own."

This depends on `2026-11-17-an-inbox-on-the-tape-and-one-worker`.

## Proposal

- A timer is an entry of the new kind `timer` on the timeline it is for:
  `:text` what to do then, `:data` `(:at TIME)` for a follow-up, or
  `(:every SECONDS :from TIME)` or `(:daily "HH:MM")` for a schedule. A
  timer whose id a later `timer` entry has in `:refs` is cancelled.
- Two tools in `superchat-inbox.el`, class `read` because they only append:
  `follow_up` (`in` seconds or `at`, `text`) and `schedule` (`every` or
  `daily`, `text`), and `cancel_timer` (`id`). The person sees and cancels
  them with `/timers` in the chat buffer.
- The worker looks once a minute while Emacs runs and at start: a timer is
  due when its time has passed and no `inbox` item has it in `:refs` since
  that time. A due timer becomes one `inbox` item, unattended, its text the
  timer's, refs the timer. A follow-up fires once; a schedule that missed
  several times while Emacs was closed fires once, not once for each.
- **L0.** Entry kind `timer`; the tools in the table of built-in tools.

## Alternatives considered

- Emacs's `run-at-time` alone. Not taken: it is lost when Emacs exits.
- A durable execution engine, the article's other choice. Not taken: it is
  a dependency outside Emacs, and the dependencies are only what Emacs
  ships, `curl` and `bridge/`.
- A status on the timer that the scheduler updates, as the article
  describes. Not taken: whether a timer fired is read from the `inbox` item
  that refs it, so the Tape stays append-only.

## Acceptance criteria

- A follow-up in 0.05 s puts one unattended `inbox` item when due and none
  after.
- A daily schedule that missed three days while Emacs was closed puts one
  item at start.
- A cancelled timer never fires.
- Tests bind the minute of the poll to under 0.1 s.

## Risks

- Nothing fires while Emacs is closed: "always on" here means "while Emacs
  runs". The fire at start keeps work from being lost, not from being late.
- A schedule that fires a run every few minutes costs a model request each
  time; the tools' descriptions must ask for restraint.
