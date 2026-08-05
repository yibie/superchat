# Agent Profile Contract Stabilization

## Goal

Stabilize the existing Profile/Actor contract without introducing another
Agent object, registry, lifecycle, or store.

## Invariants

1. `superchat-preset-profile-id` is the only semantic source of Profile
   identity.
2. An Agent Profile's identity is independent from its explicit Capability.
3. Missing Capability remains a warned compatibility fallback at Session and
   Subagent adapters; Runtime never infers it from Profile identity.
4. Agent Profiles cannot grant effective Authority.
5. `actor-id` identifies a Runtime participant, not a permanent user-like
   identity; `run-id` identifies one execution.

## Vertical slices

1. Lock Profile identity and compatibility-warning behavior with contract
   tests.
2. Route Runtime and Subagent Profile identity through the Preset helper.
3. Lock effective Authority at the Runtime request seam.
4. Tighten the glossary and architecture documentation.

## Verification

- Focused Preset, Runtime, Skills, and Subagent ERT tests.
- Full `test/run-tests.el`.
- `check-parens` on modified Emacs Lisp files.
- `git diff --check` on this phase's files.
