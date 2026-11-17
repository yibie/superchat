# Decision: Dreaming, a scheduled review of the notes

Status: proposed

## Problem

Notes pile up. A note that a later one corrects is still shown by the
`notes` view and still reaches every system prompt, inside its 2000
characters. Anthropic's article "Building always-on agents" names an
optional "dreaming" step that reviews past sessions to curate memory,
replacing stale entries and finding patterns across runs.

This depends on `2026-11-17-follow-ups-and-schedules-that-survive-emacs`.

## Proposal

- **L1.** The `notes` view leaves out a note that a later note has in its
  `:refs`: by `SPEC.md`, a new entry whose `:refs` names an entry corrects
  it. The `search` view still finds both.
- **L3.** A schedule the person turns on with `/dream` (off by default):
  daily, an unattended run per scope that has new notes, which reads the
  notes and the timelines since the last such run with `recall`, and writes
  with `note` a note that replaces two that say the same or one that is
  wrong, citing them. It deletes nothing; it sends nothing unless a note
  contradicts what the person said.

## Alternatives considered

- Compaction of long sessions alone. Not taken as the answer: the article
  says compaction and summaries both "lose detail", and the facts worth
  keeping belong in memory.
- Let the person tidy the notes by hand. Not taken as the only way: it is
  the work the person gives an agent; `/note` stays.

## Acceptance criteria

- A note citing an earlier note hides the earlier one from the `notes` view
  and from the system prompt; `search` finds both.
- With dreaming on, a scripted run that writes a replacing note leaves the
  `notes` view one note shorter.

## Risks

- A note that cites a note as a source, not to correct it, now hides it.
  The `note` tool's description must say that citing a note replaces it.
- A wrong replacing note hides a right one; the old one is still on the
  Tape and in `search`, and the person can write a note that corrects it.
