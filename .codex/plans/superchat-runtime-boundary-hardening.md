# Runtime Boundary Hardening

## Scope

Close the two remaining executable boundary gaps without adding new Runtime
concepts:

1. Local cancellation must win if `llm-cancel-request` synchronously invokes a
   late provider callback.
2. Architecture tests must cover future root-level product entry files instead
   of relying on a hand-maintained Surface list.

The existing synchronous transport failure, audit-incomplete, release,
terminal-hook, and late-callback tests already protect the other reviewed
terminal paths.  Production code already contains no `agent-mode`, so there is
no compatibility migration to implement.

## Changes

1. Add a cancellation regression where the provider calls Runtime's terminal
   observer from inside `llm-cancel-request`.
2. Make Runtime commit the local cancelled terminal state before asking the
   provider to stop, while retaining the captured provider request handle.
3. Generalize the architecture regression to scan every root production
   `.el` file, with only Runtime/model-transport owners exempted.
4. Assert that no production source reintroduces `agent-mode` or calls
   Runtime-private completion functions outside `superchat-runtime.el`.

## Acceptance

- The cancel race ends once as `cancelled`; no `response.completed` or
  `run.completed` is admitted.
- Existing cancellation still targets the newest provider request.
- All loaded ERT tests pass.
- Repository checks show direct model calls only in Runtime/transport owners,
  no `agent-mode`, and no external completion owner.

