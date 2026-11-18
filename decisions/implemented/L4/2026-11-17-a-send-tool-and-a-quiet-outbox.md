# Decision: A send tool, and runs nobody watches stay quiet

Status: implemented

## Problem

A run started by the inbox or a timer has nobody at its chat buffer. Its
answer is on the Tape, but the person learned of it only by opening that
timeline. Anthropic's article "Building always-on agents" says that
background work does not stream: the person gets the outcome and the key
details, and "most always-on agents can get by with a 'send' tool that
delivers the message directly to the user"; and that "it's better for the
agent to start quiet and then earn more of the user's attention over time."

## Decision

- **L3.** A tool `send` (`text`), class `read`, in `superchat-inbox.el`: it
  appends an `event` of type `sent` with the text, shows its first line
  with `message`, and answers `Sent.`.
- **L3.** The inbox worker gives an unattended run, as SYSTEM, a few lines
  saying that nobody watches, that the person sees only what is sent and
  the questions, and to send only what they need to act on or asked for.
  A run started any other way is told nothing new.
- **L4.** A new module, `superchat-messages.el`, cap 100 lines, with the
  arrow `messages → chat`: the command `superchat-messages` shows every
  `sent` event of the Tape in `*superchat: messages*`, one line each with
  its time and timeline, oldest first and point on the newest; `RET` opens
  the chat buffer of that timeline and `g` draws it again.
- **L0.** Event type `sent`; the module's row and arrow.

The buffer is drawn from the Tape when it is opened, not kept up to date
as messages come: the echo area tells of each one as it is sent, and `g`
shows what came since. The proposal did not say which; drawing from the
Tape needs no state and no hook into the inbox.

## Alternatives considered

- Show the final answer of every unattended run. Not taken: the article
  says to start quiet; a schedule that runs every hour would fill the echo
  area.
- A formal outbox, a table and a dispatcher that paces messages and retries
  them. Not taken: the article keeps it for pacing and recovery, and a
  `sent` event in Emacs is delivered when it is appended.

## Consequences

- An unattended run that sends nothing tells the person nothing; its
  answer is still on the Tape.
- The model may send too much or too little; the article tunes this with
  evals, and this repository has none yet.
- `spec/L3-agent.md` and `spec/L4-interface.md` may hold more words: the
  tool, the prompt and the module are new and have no other home.
