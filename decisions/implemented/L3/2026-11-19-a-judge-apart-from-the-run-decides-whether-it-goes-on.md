# Decision: A judge apart from the run decides whether it goes on

Status: implemented

## Problem

At the round limit the model that did the work wrote its own look-back and
its own `Status:` line, and that line decided whether the run went on. A
model that is going round is the worst placed to see it: its context is the
very rounds that went wrong, and a cheap model's look-back was one line
("f3: value 3."). The look-back was also an `assistant` entry, so every
later request carried it, a self-assessment piling up in the context it
judged. dim keeps its Goal plugin's check out of the model's context and
moves "is it finished" to a separate evaluator with a clean context that
answers DONE, CONTINUE or BLOCKED (`docs/design/plugins/Pursuit.md`);
Microsoft's TokenEconomizer has the same judge as an optional third step
(`tokeneconomizer/loopguard`).

## Decision

- When a run could go on (the `auto` level, continues left, no input
  pending), `superchat-limit-judge` asks the
  run's model in a request of its own: no tool, a short system prompt, one
  message with the task (the run's `user` entries), the facts
  `superchat-request-digest` counts, and the look-back. Its first word is
  the verdict; one that names none, or a failed request, is `continue`,
  which the count of going on still bounds.
- `done` ends the run `done`, the look-back its answer; `blocked` pauses
  it; `continue` goes on. A `verdict` event records the verdict and why.
- Going on, the look-back becomes a `look-back` event, which no request
  carries as a message; the first request after going on has it once, in
  its system prompt, after the facts.
- The worker's `Status:` line is an appeal, `blocked` too: in a live run
  DeepSeek wrote `blocked` because the limit was reached, and the judge
  rightly sent it on. Where the run cannot go on, `finished` still ends it
  `done`.
- The judge's prompt says the limit, not the agent, stopped it: without
  that line DeepSeek judged a run cut off at 3 of 8 files `blocked`.

## Alternatives considered

- Trust the worker's status line, as before. Not taken: the case above,
  and dim's reason for Pursuit: a worker's "completed" is a claim.
- Judge before every request, or on a schedule. Not taken: at most one
  judge per continue, so at most `superchat-agent-continues` (2) extra
  requests a run; TokenEconomizer judges only after its tier-0 gate opens.
- A different, stronger model for the judge. Not taken now: the person
  has one model set; a later option.
- Keep the look-back as an `assistant` entry. Not taken: dim's rule that a
  check's product does not enter the model's context.

## Consequences

- Live, with DeepSeek, a limit of 3 and eight files to read one per
  response: the judge said CONTINUE twice, each look-back named what was
  left, and the run answered the right sum.

- At most two more requests per run at the `auto` level.
- A run whose look-back says `finished` while its tests still fail can be
  sent on; one that says `continue` when the facts show it done can end.
- The judge's quality depends on the model; the verdict event shows each
  call so it can be checked on the Tape.
