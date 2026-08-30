---
status: accepted
scope: ordinary non-prepared LLM Requests with a non-empty tool surface
applies_when: LLM has resolved effective tools and wrapper sources before any provider effect
source Evidence ID: E-20260905-011
verification: independent P3 check and recheck; focused 5/5; canonical 1264/1246/0/18 RUNNER_GATE_OK
---

# Fail closed when ordinary LLM tools lack a trusted wrapper

## Context

An external or headless caller could previously pass ordinary raw tools to LLM
while neither a request-local wrapper nor the optional Agent Loop wrapper
producer was callable.  The provider could then receive tools outside the
shared Authority execution seam.

## Decision

- A non-empty, non-prepared tool surface is admitted only when wrapper
  selection yields a callable trusted wrapper.
- Otherwise LLM signals `superchat-llm-tool-wrapper-required` before prompt or
  provider-request construction, provider invocation, or tool effects.
- Exact-evidence validation remains prior and stricter.  Tool-free Requests,
  standard Agent Loop wrapping, trusted request-local wrapping, and prepared
  internal Subagent tools retain their existing contracts.
- Runtime lifecycle and Authority ownership do not move.  Kernel does not
  require Agent Loop, and no registry or second wrapper authority is added.

## Compatibility and migration

This is a user-approved breaking tightening for external direct callers that
attach ordinary tools without the existing trusted wrapper.  Such callers must
use a standard Superchat Request producer or supply the existing callable
wrapper contract.  Marking raw tools as prepared is not a supported migration;
prepared remains a trusted internal contract, and callable Lisp does not prove
arbitrary or malicious code safe.  Unknown external callers remain unknown.
Release selection and publication are outside this decision.

## Rejected alternatives

- Passing raw tools through for headless compatibility, because that bypasses
  the Authority invariant.
- Requiring Agent Loop from Kernel, because it reverses the enforced layering.
- Adding wrapper authentication, a registry, or another policy owner, because
  the approved change only closes the existing admission boundary.
