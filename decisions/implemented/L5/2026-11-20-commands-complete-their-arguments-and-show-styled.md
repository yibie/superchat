# Decision: Commands complete their arguments and show styled

Status: implemented

## Problem

Completion stopped at the command's name: `/level`, `/model`, `/resume`
and `/timers cancel` still needed their argument typed from memory, and
the model list gave no hint which model is large, cheap or takes images.
The input also looked the same whether `/levle` named a command or not.

## Decision

- `superchat-command-define` takes an optional ARGS function: given the
  words typed after the command, it returns the next word's choices,
  plain or with a note. `/level`, `/model`, `/resume`, `/timers` and
  `/dream` give choices; `/timers cancel` notes each id with its task.
- Model choices are noted with window, input cost and `image`, from the
  new L2 `superchat-llm-model-info`.
- The command or `@MODEL` that starts the input is drawn in its own face:
  a known command, an unknown one, a model.
- `superchat-chat.el`'s cap goes from 900 to 1000, by the person's
  choice over a separate input module.

## Alternatives considered

- A module of its own for the input area (completion, styles, the
  `@MODEL` parse). Offered, not taken: the person chose to raise the cap.
- Highlight `/` and `@` anywhere in the text. Not taken here: only the
  start of the input is acted on; inline references are a later proposal.

## Consequences

- Commands defined by plugins can offer their arguments the same way.
- Styling runs with each refontification of the input and asks for the
  model ids; with no provider set, an `@` is left plain.
- The `@MODEL` completion and face are gone since
  [2026-11-20-a-model-is-chosen-with-slash-model.md](2026-11-20-a-model-is-chosen-with-slash-model.md).
