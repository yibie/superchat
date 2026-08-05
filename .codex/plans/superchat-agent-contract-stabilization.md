# Agent Contract Stabilization

## Goal

Make Agent Profile history and delegated Actor lineage stable without adding an
Agent object, registry, lifecycle, or persistence layer.

## Invariants

1. An explicit `profile_id` is stable across mutable Profile names.
2. Legacy Agent Profiles without `profile_id` remain readable by falling back
   to their name.
3. Agent Profiles declare Capability explicitly; missing Capability remains a
   warned compatibility path outside Runtime.
4. A Profile Toolset exposes actions to the model but never grants Authority.
5. A delegated participant is identified by `actor-id`, `profile-id`,
   `run-id`, and `parent-run-id`; new production state never uses `agent-id`.

## Vertical slices

1. Add explicit Profile identity to Preset parsing, native/standard loading,
   and export; prove identity survives a display-name change.
2. Carry `parent-run-id` from the delegating tool context through the child
   Runtime request, status, and Tape events.
3. Add an Authority contract test showing a Profile Toolset cannot override an
   observer policy.
4. Update the glossary and architecture documentation.

## Compatibility

- Existing `name` remains the fallback Profile identity for legacy Presets.
- Existing Skills without `profile_id` continue to load.
- Existing Tape `agent_id` payloads remain projection-only compatibility data.

## Verification

- Focused Preset/round-trip, Runtime, Authority, and Subagent ERT tests.
- Full `test/run-tests.el`.
- `check-parens`, architecture scans, and `git diff --check`.
