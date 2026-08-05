# Experience Feedback Contract v0

> **Status:** design-only; no Runtime implementation is authorized.
>
> **Question:** how can Superchat preserve a conclusion, let humans or Agents
> correct it, and derive a current interpretation without erasing history?

## Boundary

Tape, Workspace, and Outcome Records preserve **Facts**.  Projection derives
a **Current View**.  Runtime and Authority execute any later action.  Memory
may consume an explicitly accepted result, but does not own this loop.

It introduces only two domain concepts: **Claim** and **Feedback**.  It does
not introduce an Evidence object, generic relation graph, confidence score,
learning engine, controller process, or automatic promotion path.

## Minimal records

The durable envelope already supplies identity, Actor, and time:

```text
Claim    = {statement, scope, basis[], supersedes?}
Feedback = {target, kind, basis[], note, replacement?}
kind     = support | challenge | correct
```

An empty `basis` means unsupported, not invalid.  Relationships are these
references; v0 has no independent Relation record.

## Invariants

1. **Append, never rewrite.** A correction adds Feedback and a replacement
   Claim.  The original Claim remains addressable.
2. **Provenance before truth.** Missing basis stays visible.  Neither a model
   nor a human assertion becomes a Fact merely because it was recorded.
3. **Conflict remains conflict.** Without explicit supersession, competing
   Claims remain contested; recency alone does not choose a winner.
4. **Interpretation has no Authority.** A Current View cannot execute Effects,
   relax policy, modify Closure, or promote itself into Memory or Capability.

The Current View may derive `active`, `superseded`, `contested`, and
`unsupported` for presentation.  These are Projection results, not stored
truth values.

## Control loop

Control theory is a boundary map, not another subsystem:

```text
Facts -> Current View --+
                        +-> compare -> Feedback
goal or rubric ---------+               |
                                        v
new Facts <- Runtime <- Authority-gated Request
```

- Runtime is the plant;
- Tape and Workspace are sensors;
- Current View is the state estimate;
- comparison with an explicit goal produces the error signal;
- Feedback records that signal;
- an existing Authority-gated Request is the actuator.

If no explicit goal or rubric exists, the loop may record Feedback but cannot
infer a correction action.  Feedback never expands Authority.

## Real correction case: Cohort v6 recovery

An initial reading of one Discovery Run could overstate a recovery rule:

```json
[
  {
    "record": "claim",
    "id": "claim-v6-recovery-001",
    "statement": "After agent_reach_read fails, switching to agent_reach_search is a stable recovery strategy.",
    "scope": "experience-cohort-deepseek-v6/discovery",
    "basis": ["tape:1577", "tape:1578", "tape:1580"]
  },
  {
    "record": "feedback",
    "id": "feedback-v6-recovery-001",
    "target": "claim-v6-recovery-001",
    "kind": "correct",
    "basis": ["tape:961", "tape:962", "tape:964", "tape:1577", "tape:1578", "tape:1580"],
    "note": "The two eligible failures have different follow-ups; one repeats agent_reach_read and one switches to search.",
    "replacement": "claim-v6-recovery-002"
  },
  {
    "record": "claim",
    "id": "claim-v6-recovery-002",
    "statement": "Cohort v6 contains two non-matching recovery episodes; no stable recovery strategy is established.",
    "scope": "experience-cohort-deepseek-v6/discovery",
    "basis": ["tape:961", "tape:962", "tape:964", "tape:1577", "tape:1578", "tape:1580"],
    "supersedes": "claim-v6-recovery-001"
  }
]
```

The frozen Tape contains all six references.  Events `961 -> 962 -> 964`
show a failed read followed by a successful read; events
`1577 -> 1578 -> 1580` show a failed read followed by a successful search.
The Current View selects the replacement Claim while retaining the original
error and its correction history.  It still cannot infer that either action
caused recovery or should become policy.

## v0 completion gate

This contract is sufficient when replay can answer what was claimed, which
Facts it referenced, who corrected it, what replaced it without deletion, and
what the Current View says now.

Implementation begins only when a real consumer needs that replay.  Until
then, this document and the frozen correction case are the complete v0.  The
executable canary is
[`experience-feedback/human-correction-keeps-history`](../../test/test-projection.el).
