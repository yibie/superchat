# Decision: recall and note are offered in every request

Status: implemented

## Problem

[Tools and skills are found as needed](../L0/2026-11-20-tools-and-skills-are-found-as-needed.md)
deferred the memory tools. On the person's Tape (31 conversations of this
project, 1479 requests), 14 conversations used `recall` or `note`, at a
median of their first request. Each of those spent one round on
`load_tools` before it could remember, the round waste earlier work set out
to remove, and memory is the third use of Superchat. The tool definitions
sit in the cached prefix of a request, so what deferring them saved cost
little.

## Decision

- `recall` and `note` are defined without `:deferred`: every request
  offers them, with the seven core tools and `load_tools`.
- `tape_query` stays deferred: it is used rarely and its SQL description is
  long.
- The base instructions no longer say to load them.

## Alternatives considered

- Keep them deferred, as the earlier decision did. Not taken: half the
  conversations of the Tape paid a round for it, at their start.
- Make `tape_query` core too. Not taken: no conversation of the Tape that
  used memory needed it first.

## Consequences

- A first request offers ten tools; its tool definitions are larger by the
  two descriptions, and the catalog of deferred tools shorter by two lines.
- The timer tools, `tape_query` and plugin tools are still loaded on first
  use.
