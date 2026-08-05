# Agent Participant Finalization

## Decisions

1. Keep `audit-incomplete` as the explicit no-audit recovery status.  It cannot
   promise a terminal Tape event after Tape persistence failed, but it must use
   the same Turn completion seam for hooks and in-memory closure.
2. Reuse an `agent`-typed `superchat-preset` as the current Agent Profile
   representation.  Do not add an Agent DSL, registry, or module.
3. Use Runtime `actor-id` as Agent Instance identity.  Do not introduce a
   duplicate `agent-id` at the Runtime boundary.
4. Add `profile-id` only when the submitted Preset is an Agent Profile.
   Non-agent Actors such as users, workflow steps, and compactors remain valid
   without a Profile.

## Changes

1. Let `superchat-runtime--complete-turn` mark an unaudited completion without
   appending a false `turn.completed` event.
2. Route `superchat-runtime--audit-incomplete` through that completion seam.
3. Derive `profile-id` from an agent Preset during Runtime submission and expose
   it in Run status and `run.started`.
4. Verify ordinary agent Runs and existing Subagent submissions gain the
   Profile link without changing their current callers.

## Acceptance

- Audit failure ends as `audit-incomplete`, runs Turn hooks once, marks the Turn
  complete in memory, releases resources, and does not invent a terminal event.
- Agent Runs expose `actor-id` plus `profile-id`.
- Prompt, workflow, and other non-agent Presets do not become Agent Profiles.
- Existing Capability, Topology, Authority, and Subagent behavior is unchanged.
- The complete ERT suite has no unexpected result.

