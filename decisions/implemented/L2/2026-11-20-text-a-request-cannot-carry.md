# Decision: Text a request cannot carry

Status: implemented

## Problem

The model read a telega buffer with `eval_elisp`, calling
`buffer-substring`. Two things went wrong. The value printed every string's
text properties, telega's message plists with base64 thumbnails, so the
result was megabytes of noise. And the buffer held raw bytes (an emoji
stored as broken UTF-8), which went onto the Tape in the tool result; every
later request of that conversation then failed with `Wrong type argument:
json-value-p`, because `json-serialize` takes no raw byte.

## Decision

- `eval_elisp` prints each value with the text properties of its strings
  left out, lists and vectors walked to a nesting of 50.
- `superchat-llm` sends every message text and the system prompt with each
  raw byte and lone surrogate replaced by U+FFFD, when it builds the
  request.

## Alternatives considered

- Clean the text when a tool result is written to the Tape. Not taken: a
  Tape that already holds such text would stay unsendable; cleaning when
  the request is built mends those conversations too.
- Print values with `print-length` limits instead of stripping properties.
  Not taken: it would cut the values the model asked for, and keep the
  noise.

## Consequences

- A conversation stuck on this error goes on at its next request.
- The Tape keeps the text as it came; only what is sent is replaced.
