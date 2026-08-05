# Agent Profile Contract & Actor Projection

## Goal

Make Profile identity, Capability, Actor identity, and Run identity explicit
without adding an Agent object, registry, DSL, or store.

## Invariants

1. `profile-id` identifies a static Agent Profile.
2. `capability` identifies work purpose and is never inferred from
   `profile-id` inside Runtime.
3. Runtime resolves Capability as:
   explicit Request → explicit Profile default → `chat`.
4. `actor-id` identifies the executing participant; `run-id` identifies one
   execution.
5. Team/Subagent state and new events use `actor-id`, `profile-id`, and
   `run-id`; `agent-id` remains readable only in legacy Tape payloads.
6. Agent Profiles cannot grant effective Authority.

## Compatibility

- Existing `type=agent` Presets without `capability` still load.
- Session and Subagent adapters may temporarily fall back to the Profile name
  with a warning so old Skills continue to run.
- Runtime itself never performs that fallback.
- Legacy Team events containing `agent_id` remain replayable.

## Vertical slices

1. Add `capability` to Preset parsing/export and prove a Profile ID can differ
   from its Capability.
2. Make Runtime use the explicit Profile Capability when the Request does not
   select one, and prove it does not infer from Profile name.
3. Rename live Subagent context from `agent-id` to `actor-id`, carry
   `profile-id`, and normalize legacy Team events in the projection.
4. Update the domain glossary and architecture documentation.

## Verification

- Focused Preset, Runtime, and Subagent ERT tests.
- Architecture scan forbidding production `agent-id`.
- Full `test/run-tests.el`.
- `check-parens` on modified Emacs Lisp files.
