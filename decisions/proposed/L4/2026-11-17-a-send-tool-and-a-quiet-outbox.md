# Decision: A send tool, and runs nobody watches stay quiet

Status: proposed

## Problem

A run started by the inbox or a timer has nobody at its chat buffer. Its
answer is on the Tape, but the person learns of it only by opening that
timeline. Anthropic's article "Building always-on agents" says that
background work does not stream: the person gets the outcome and the key
details, and "most always-on agents can get by with a 'send' tool that
delivers the message directly to the user"; and that "it's better for the
agent to start quiet and then earn more of the user's attention over time."

This depends on the inbox and on unattended runs.

## Proposal

- A tool `send` (`text`), class `read`, that appends an `event` of type
  `sent` with the text, for the person.
- The person sees `sent` events in one buffer, `*superchat: messages*`,
  newest last, each with the timeline it came from, and `RET` on one opens
  that timeline's chat buffer. A new one shows its first line with
  `message`.
- An unattended run tells the person nothing unless it calls `send` or asks
  a question (`2026-11-17-a-question-to-the-person-ends-the-run-and-waits`).
  The system prompt of an unattended run says so: send only what the person
  needs to act on or asked for.

## Alternatives considered

- Show the final answer of every unattended run. Not taken: the article
  says to start quiet; a schedule that runs every hour would fill the echo
  area.
- A formal outbox, a table and a dispatcher that paces messages and retries
  them. Not taken now: the article keeps it for pacing and recovery, and a
  `sent` event in Emacs is delivered when it is appended.

## Acceptance criteria

- An unattended run that calls `send` puts one line in
  `*superchat: messages*` and in the echo area; one that does not, nothing.
- `RET` on the line opens the chat buffer of its timeline.

## Risks

- The model may send too much or too little; the article tunes this with
  evals, and this repository has none yet.
