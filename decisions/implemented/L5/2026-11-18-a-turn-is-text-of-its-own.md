# Decision: A turn is text of its own

Status: implemented

## Problem

`superchat-chat.el` had reached 1099 of its 1100 lines. Seven issues were
open in `NOTES.md`, five of them about the display, and none of them had
room to be fixed. Most of the file was the text of a turn: the conversion
of Markdown to Org, the call headings, the strip, the progress and answer
headings. None of it touches the state of a buffer.

## Decision

The text of one turn is `superchat-turn.el`, a module of L5 with a cap of
400 lines. Its public names are `superchat-turn-text`, `superchat-turn-seal`,
`superchat-turn-dim`, `superchat-turn-cut`, `superchat-turn-strip`,
`superchat-turn-cells` and `superchat-turn-answers`. `superchat-chat.el`
requires it and keeps the buffer, its state and the commands; its cap is
900 lines. The arrow is `chat → agent, plugin, recall, inbox, turn`. The
rules of a turn's text move to their own section of `spec/L5-interface.md`.
Behaviour does not change.

## Alternatives considered

- Raise the cap of `superchat-chat.el`. Not taken: the cap is a design
  signal, and the file would keep growing.
- Split by commands and display rather than by the text of a turn. Not
  taken: the conversion is the part that is pure, text in and text out,
  so it is the part that leaves cleanly.
- Put the conversion in L2 or L1. Not taken: it is display, and only L5
  uses it.

## Consequences

- `superchat-chat.el` is 762 lines, with room for the display issues in
  `NOTES.md`.
- The conversion and the headings are tested on their own, in
  `test/test-turn.el`, without a chat buffer.
- The text properties a turn puts on its text are named `superchat-turn-`.
  The chat buffer reads them by those names.
