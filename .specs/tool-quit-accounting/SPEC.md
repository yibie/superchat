# Tool quit accounting

status: confirmed
revision: 1
context_refs: `CONTEXT.md` Authority, Effect, Run, Closure, and Experience vocabulary; `KERNEL.md` K20 tool execution, Runtime terminal, Closure, and authority-map contracts; `docs/protocols/session-editor-workspace-close.md` Runtime cancellation ownership; `docs/architecture-review-20260905/02-problems.md` P3

## Problem and goal

A user `quit` can escape a tool pre-hook, implementation, result filter, or
outcome-notification hook after different facts have already occurred.  The
current path can leave the Effect open or route a generic `tool.failed` into
Runtime/Experience recovery without preserving which phase was interrupted.

Record the facts already known at interruption, finish the minimum durable
accounting, cancel exactly the owning Run through Runtime, and then re-signal
the original `quit`.  An interruption is auditable but is not automatically a
tool-failure/recovery-learning sample.

## Unchanged contracts

- Authority remains the sole tool seam: permission, approval, rewrite,
  implementation invocation, outcome audit, and result filtering stay there.
- Runtime alone owns Run lifecycle and `superchat-runtime-cancel`; Authority
  gains no terminal state machine.  Cancellation identifies one exact Run and
  cannot cancel another Run.
- Tape remains append-only and its event schema is unchanged.  This change
  reuses `tool.failed`; it adds no event kind, registry, dependency-injection
  framework, or second permission/recovery authority.
- Closure continues to judge durable Tape facts.  A failed tool invocation
  discharges its Effect as failed; an audit write failure follows the existing
  `audit-incomplete` path and is not reported as a successful cancellation.
- Ordinary hook `error` isolation, permission/approval/rewrite policy,
  headless Authority behavior when Agent Loop is absent, and exact-evidence
  validation remain unchanged.
- Tool implementation return is not proof that an external effect succeeded,
  and tool-effect completion is distinct from filtered-result persistence and
  model delivery.

## Decision and boundaries

1. Treat a user `quit` as the primary condition.  After mandatory accounting
   and exact-Run interruption are attempted, re-signal that original condition.
   Do not continue every remaining hook, and do not allow a cleanup callback's
   `error` or `quit` to replace it.
2. Reuse `tool.failed` to mean that the tool-call pipeline did not complete
   normally.  Record a phase that distinguishes: `pre` (implementation not
   invoked), `implementation` (invoked but did not return normally; effects may
   be partial or unknown), and `result-processing` (implementation returned,
   then filtering or notification/accounting was interrupted).  A returned
   value is only an implementation receipt at that phase; it must not use the
   delivered-result fingerprint fields.
3. A user-interruption `tool.failed` remains visible in Tape and usable by
   Closure, but Runtime must not automatically open its recovery intervention
   and Experience must not select it as a failed/recovery learning sample.
4. Result filters are the only compatibility extension allowed to transform
   the successful result.  Both ordinary and exact-evidence success paths
   filter, detach/freeze the result, durably append `tool.completed`, notify
   post hooks with a detached copy, then deliver the same frozen result.
   Post-hook return values are ignored.
5. Failure hooks likewise observe an already durable `tool.failed` using
   detached notification data; their return values do not alter the outcome.
   Existing isolation of ordinary `error` conditions is retained.
6. Runtime supplies a request-local interruption callback that closes over the
   exact Run ID and delegates to existing cancellation.  It must be safe for
   synchronous, delayed asynchronous, late, repeated, and reentrant callbacks;
   terminal/late calls are no-ops with respect to other Runs.
7. If durable outcome accounting fails, invoke the existing audit-failure seam
   so Runtime seals `audit-incomplete`.  Do not invent `run.cancelled` or a
   successful `tool.completed` fact to cover the missing audit.

## Model delta

- **Tool Failure Fact refinement:** `tool.failed` also records user-interrupted
  pipeline completion, including phase and effect uncertainty.  It remains a
  failed Effect fact for Closure, but interruption is not itself an Experience
  recovery-learning classification.
- **Runtime–Authority relation:** a Request may give Authority one private,
  request-local exact-Run interruption callback.  This is a narrow callback
  Interface, not a public registry or transfer of cancellation authority.
- **Hook contract:** post-success and post-failure hooks are outcome
  notifications.  They do not transform or determine the durable outcome.

These are user-approved semantic revisions to K20.  The Kernel and any
affected Protocol are updated only after implementation and independent
verification during `learn`.

## Action Contracts

### Account for a user-interrupted tool call

precondition: Authority has accepted a tool call and a `quit` escapes a
pre-hook, implementation, result filter, or outcome-notification hook.

input: the original `quit`, tool execution identity and request hash, current
phase, any implementation receipt already returned, exact-evidence context,
and the Runtime-owned request-local interruption callback when present.

permitted effect: append one phase-labelled `tool.failed` for an outcome not
already durably frozen; notify failure observers after persistence; interrupt
only the exact owning Run; or, if audit persistence fails, invoke the existing
audit-failure path.  Finally re-signal the original `quit`.

invariant: pre interruption never invokes the implementation; implementation
interruption does not claim zero effects; a returned receipt does not become a
delivered-result fingerprint; no automatic Experience/Runtime recovery sample
is created; no secondary condition replaces the original `quit`; no other Run
is cancelled; no success event is fabricated.

### Freeze, record, notify, and deliver a successful tool result

precondition: the tool implementation returned normally and required exact
evidence checks still admit the result.

input: the raw implementation result, tool arguments and execution identity,
the compatibility result-filter chain, and post-success notification hooks.

permitted effect: filter the result, detach/freeze it, append one
`tool.completed` whose fingerprint describes that frozen result, notify post
hooks with a separate detached copy, then return or asynchronously deliver the
same frozen result.

invariant: post-hook mutation or return cannot change the Tape fingerprint or
delivered object; ordinary and exact-evidence paths use the same ordering;
hook `error` remains isolated; a `quit` starts the interruption contract and
does not continue later hooks.

## Seams and verification

- `superchat-authority.el` owns phase tracking, durable tool outcome ordering,
  detached notification data, and preservation of the original `quit`.
- `superchat-runtime.el` owns the request-local exact-Run interruption callback,
  terminal cancellation, `audit-incomplete`, and suppression of automatic
  recovery for interruption-class failures.
- `superchat-experience.el` owns excluding interruption-class `tool.failed`
  events from recovery-learning selection without weakening ordinary typed
  recovery matching.
- `superchat-agent-loop.el` remains the optional compatibility hook/filter
  implementation unless verified behavior proves a necessary change.
- `superchat-closure.el` is expected to require no production change; real
  replay tests must prove the existing failed-Effect and terminal judgments.

Verification uses isolated Emacs processes, temporary SQLite/Tape databases,
and local fake tools only.  It covers ordinary and exact-evidence calls across
pre, implementation, filter, post-success, and failure-notification quits;
sync and delayed async delivery; late/repeated/reentrant callbacks; two live
Runs; audit-write failure; detached mutation; unchanged ordinary errors;
headless Agent-Loop absence; Experience exclusion; and Closure replay.  A
signalled `quit` probe is evidence for Lisp control flow, not proof of a real
interactive keyboard `C-g` path.

## Compatibility and migration

This is not a zero-semantics or fully backward-compatible change.  Existing
ordinary post-success hooks currently run before filtering and may mutate a
shared result; after this revision they run after durable outcome persistence
on a detached copy, and their returns and mutations cannot affect delivery.
The user explicitly accepts that behavior change.

Tape storage and event kinds require no migration.  Existing `tool.failed`
consumers must distinguish the new interruption classification where recovery
or learning would otherwise be inferred.  Unknown external consumers of
private hooks or payload details remain unknown and are not claimed migrated.

## Out of scope

- Swallowing `quit`, running all remaining hooks after it, or converting every
  callback condition to an ordinary error.
- Treating an implementation return as external-effect verification or as
  proof of provider/result delivery.
- New Tape events or schema, Run states, Experience categories, recovery
  registry, generic DI framework, or second Authority/cancellation owner.
- Changes to permission, approval, rewrite, Session/EditorWorkspace ownership,
  provider transport, rendering, or publication/release policy.
- Automatically turning a user interruption into an Experience failure,
  recovery lesson, or Runtime recovery intervention.

## Issue map

- `issues/01-tool-quit-accounting.md`: one confirmed end-to-end Slice spanning
  Authority accounting, exact Runtime interruption, Experience exclusion, and
  replay verification.

## Revision notes

- Revision 1 (2026-09-05): captured the user-approved interruption accounting,
  exact-Run cancellation, recovery exclusion, and notification-order contracts.
