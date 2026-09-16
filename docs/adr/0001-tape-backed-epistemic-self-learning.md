---
status: accepted
---

# Use a Tape-backed epistemic loop for self-learning

The [M3a decision](../decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md) supersedes the implementation consequences concerning Closure, the Claims module and Experience's context/review entry points. The consequences below record the 2026-08 implementation and are kept as written; where they name Closure or the Claims module, the current mechanics are in the [request lifecycle](../protocols/request-lifecycle.md). The epistemic-time, provenance and promotion constraints in this ADR remain in force, and they do not authorize restoring the removed interfaces.

Superchat will pursue self-learning without changing model weights by changing
versioned View and Action policies only after auditable experience and held-out
evaluation.  We choose `tape.systems` as the problem model—not a prescribed
solution—because it keeps durable history, reconstruction, current context, and
execution separate while allowing domain-specific Entry and Anchor schemas.

## Decision

The learning loop is:

```text
Decision Snapshot
  → Execution
  → Outcome
  → Attribution Claim
  → Pattern Claim
  → boundary-targeted Exercise
  → held-out Evaluation
  → Promotion or Rejection
  → changed future View / Action policy
```

The following invariants apply:

1. **Epistemic time is preserved.** A model-visible View contains only what was
   available before its Action.  Later Outcomes, expected targets, sealed
   oracles, and evaluator judgments cannot be backfilled into that View.
2. **Success is evidence, not prior knowledge.** A successful Run does not make
   every earlier step correct, prove causal attribution, or establish a
   transferable Pattern.
3. **Tape may carry graph structure.** Custom node and edge Entries may record
   proposed, supported, challenged, corrected, or retired relations.  Their
   presence records that the relation was asserted; it does not make the
   relation true.
4. **Graph state is interpreted, not authoritative.** An Applicability Graph may
   be rebuilt as a Projection from Tape and exposed as a task-specific subgraph
   through a View.  Topology, similarity, connectedness, or centrality alone do
   not prove applicability, causation, or priority.
5. **Anchor policy is customizable.** An Anchor may be a deterministic index,
   graph checkpoint, semantic interpretation, curriculum marker, or Handoff.
   It retains provenance and never replaces the Entries it represents.
6. **Exercises reduce a named uncertainty.** A useful Exercise distinguishes
   competing Pattern Claims, probes an applicability boundary, supplies a
   counterexample, or checks regression.  Its oracle remains outside the
   Actor's View until Execution is terminal.
7. **Entropy is advisory.** Uncertainty may identify an Observation Opportunity
   or trigger retrieval, evidence gathering, reflection, or additional
   sampling.  It cannot create an Outcome, establish a Pattern, or authorize
   Promotion.
8. **Promotion is evidence-gated and reversible.** A Candidate may shape future
   behavior only after preregistered held-out, counterexample, sham,
   contradictory, safety, cost, and regression gates.  Promotion, rejection,
   correction, retirement, and rollback are appended; prior Entries remain.

## Consequences

- Tape remains the append-only evidence substrate; Graph, Anchor, Projection,
  View, Exercise generation, and promotion are replaceable policies built on
  that substrate.
- Runtime continues to own execution, Authority continues to own Effects, and
  Closure continues to judge Runtime evidence only.  None of them becomes a
  semantic truth or learning authority.
- The first implementation must be one narrow vertical experiment, not a
  general learning platform: repair the current semantic-use false-positive
  boundary, preserve exact Decision Snapshot/Outcome separation, and test one
  conditional tool-action Pattern end to end.
- The initial implementation reuses Tape, SQLite, Projection, Claims/Feedback,
  Workspace revisions, and existing evaluation seams.  A graph database,
  vector database, training service, automatic Skill mutation, and model-weight
  updates are not prerequisites.

## Rejected shortcuts

- Promoting a successful trajectory directly into Memory or Skills.
- Treating model self-evaluation, low entropy, or semantic consistency as an
  Outcome or truth signal.
- Labelling every step in a successful or failed trajectory from its terminal
  result.
- Building a graph database or general ontology before a minimal vertical
  experiment demonstrates incremental value.

## Research basis

The research notes this decision drew on (a study of `tape.systems`, Bub and
Observations; a frozen-weight self-learning literature map; entropy measures
for frozen-weight Agent learning) were removed from `docs/research/` on
2026-09-12 as historical material. They remain in version-control history
before that date.
