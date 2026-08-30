# LLM tool-wrapper admission

status: confirmed
revision: 1
context_refs: `CONTEXT.md` Tool, Authority, Request, Turn, and Run vocabulary; `KERNEL.md` K24 Tool provenance/wrapper and Kernel-layering contracts; `docs/architecture-review-20260905/02-problems.md` P3

## Problem and goal

An ordinary Request with a non-empty, non-prepared tool list can reach the
provider when no callable request-local or Agent Loop wrapper is available.
Those raw tools then lack the shared Authority execution seam promised by the
current product surface.

Reject that Request before any provider effect.  Preserve tool-free Requests,
correctly prepared Subagent Requests, trusted request-local callable wrappers,
and the stricter existing exact-evidence path.

## Unchanged contracts

- Authority remains the sole model-invoked tool execution seam.  Runtime keeps
  Turn/Run lifecycle and terminal ownership; LLM keeps transport ownership.
- Kernel does not require Agent Loop or another middle-tier module.  This
  admission check adds no registry, dependency-injection system, wrapper
  authority, or parallel permission policy.
- A Request with no tools remains valid without a wrapper.
- A correctly prepared Subagent Request keeps its existing prepared tool
  objects and is not wrapped a second time.
- A non-prepared Request may supply its existing trusted request-local callable
  wrapper.  Ordinary distribution Requests may continue to obtain the existing
  Agent Loop wrapper when that optional producer is loaded.
- Exact-evidence Requests retain their current tool validation, wrapper
  requirement, provenance, and failure condition.  This revision does not
  claim to validate arbitrary or malicious Lisp hidden inside a callable
  wrapper.
- Request, Runtime, Authority, provider, Tape, and tool-result schemas do not
  change.

## Decision and boundaries

For every non-empty tool list, distinguish prepared tools from ordinary raw
tools before provider dispatch:

1. A prepared tool list is admitted through its existing prepared-tool
   contract; it is not rewrapped.
2. A non-prepared tool list is admitted only when wrapper selection yields a
   callable function, whether supplied request-locally or produced by the
   already-loaded Agent Loop seam.
3. Otherwise LLM signals a stable admission failure before constructing or
   invoking the provider request.

Marking raw external tools as prepared is not a supported migration.  The
existing prepared-tool input remains a trusted internal contract: this change
does not authenticate arbitrary Lisp callers or detect a caller falsely
asserting that raw tools are prepared.  The implementation stays inside the LLM
request boundary and does not make Runtime or Authority depend on Agent Loop.

## Model delta

**Tool-bearing LLM Request admission:** non-empty, non-prepared tools require a
callable trusted wrapper before provider effects.  This is a user-approved
breaking tightening of the ordinary LLM boundary.  Kernel promotion, if the
verified implementation changes the enacted model, belongs to `learn`.

## Action Contracts

### Admit a tool-bearing LLM Request

precondition: LLM has resolved the Request's effective tool list and whether
the list is already prepared, but no provider effect has occurred.

input: the resolved tool list, prepared-tools presence, request-local wrapper,
and the optional already-loaded Agent Loop wrapper producer.

permitted effect: admit a tool-free Request; admit an already-prepared list
without wrapping it again; wrap a non-prepared list exactly once with a
callable trusted wrapper; or reject before provider invocation.

invariant: a non-empty non-prepared list never reaches the provider raw;
`prepared` is not a supported migration for raw external tools; wrapper admission does
not reinterpret Authority policy or prove arbitrary wrapper internals safe;
Runtime/Authority ownership and Kernel layering do not move.

## Seams and verification

- Production owner: `superchat-llm.el`, within the existing wrapper selection
  and pre-provider admission path.
- Direct test owner: `test/test-llm-backend.el`.
- `test/test-runtime.el` is needed only if a real Runtime terminal assertion
  cannot be demonstrated through the existing LLM tests; no Runtime production
  change is authorized.

Tests-first verification must cover:

- non-empty ordinary raw tools with neither wrapper source: deterministic
  rejection, provider call count zero;
- the same path when Agent Loop is absent versus loaded but unable to produce a
  callable wrapper;
- a trusted request-local callable wrapper: admitted and used exactly once;
- ordinary distribution wrapper: admitted and every invoked tool still crosses
  the real Authority seam;
- prepared Subagent tools: object identity preserved and no double wrapping;
- no-tools sync/async Requests: unchanged provider and terminal behavior;
- exact-evidence Requests: their existing stricter failure and success paths
  remain intact;
- rejection through the real Runtime path, if in scope: one failed terminal,
  quiescence/delivery preserved, and no provider or tool effect.

Focused LLM tests precede the canonical runner, check-parens, Kernel strict
read-only validation, and the authority/layering form gate.  No real provider
is used.

## Compatibility and migration

This is breaking for external callers that directly pass non-empty ordinary
tools without arranging the existing trusted wrapper.  They must migrate to a
standard Superchat Request producer or pass the existing callable wrapper
contract.  They must not set the prepared flag on raw tools merely to pass
admission.

Unknown external consumers remain unknown.  The release number, publication,
and provider rollout are outside this work; the SPEC does not claim that any
external ecosystem has already migrated.

## Out of scope

- Changes to Runtime, Authority, Agent Loop, Subagent, provider adapters, Tape,
  Session/EditorWorkspace, or public tool schemas.
- A new wrapper registry, facade, generic DI mechanism, Authority classifier,
  or Kernel-to-middle-tier dependency.
- Validating arbitrary callable Lisp as trustworthy.
- Redesigning prepared tools, exact evidence, tool execution, or result
  delivery.
- Release selection, publication, or external-consumer certification.

## Issue map

- `issues/01-admit-tool-wrapper.md` — one ready LLM admission Slice; Runtime
  production remains read-only and Runtime tests are conditional on needing
  their existing fixture for the required real terminal assertion.

## Revision notes

- Revision 1 (2026-09-05): captured the user-approved breaking admission rule
  and migration boundary from the P3 architecture investigation.
