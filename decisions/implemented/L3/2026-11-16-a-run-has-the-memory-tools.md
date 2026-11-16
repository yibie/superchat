# Decision: A run has the memory tools without asking for them

Status: implemented

## Problem

`superchat-run` is the one function through which any package asks a
model. The tools `recall`, `tape_query` and `note` are defined in
`superchat-recall.el`, which only `superchat-chat.el` required. A package
that required `superchat-agent` alone got a run with no way to keep a note
or read an earlier conversation. In a run against DeepSeek, a model asked to
save a note found no `note` tool, spent eight `eval_elisp` calls probing for
one (each asking the person's permission) and saved nothing.

## Decision

`superchat-agent.el` requires `superchat-recall`, and `SPEC.md` has the
arrow `agent → recall`. A package that requires `superchat-agent` and
nothing else gets every built-in tool. `recall` is in L2, below the agent,
so the arrow keeps the layer order.

## Alternatives considered

- Say in the specification that a package which wants memory requires
  `superchat-recall` itself. Not taken: it breaks the promise that one
  function is enough, and a run without the tools makes the model look for
  them with `eval_elisp`, as the run against DeepSeek showed.

## Consequences

Loading `superchat-agent` loads `superchat-recall`, a file of about a
hundred lines. Every run offers the three memory tools unless its TOOLS
leave them out. A test starts a fresh Emacs that requires only
`superchat-agent` and checks that the three tools are defined; in the test
Emacs itself another test file may have loaded them already.
