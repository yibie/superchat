# Superchat Agent Runtime

Superchat coordinates durable, tool-using work inside Emacs.  This language
keeps execution identity separate from work purpose, permissions, and
organization.

## Language

**Actor**:
An identity to which Runtime actions and events are attributed.  Users,
workflow steps, and Agent Instances can all be Actors; not every Actor is an
Agent.
_Avoid_: Agent, when the participant may be non-agent

**Agent Profile**:
A reusable, static role definition that explicitly references its default
Capability.  Its immutable identity is independent of its mutable name;
changing that identity creates a distinct Profile, and a Profile cannot grant
effective Authority or own Runtime state.
_Avoid_: Agent type, Agent product

**Agent Instance**:
An Actor participating in Runtime according to one Agent Profile.  Its
identity is scoped to that participation, not a permanent user-like identity;
continuity across Runs must be explicit.
_Avoid_: Coding Agent, Research Agent, when referring to product categories

**Capability**:
The purpose and default behavior for a kind of work, selected independently
from Topology and effective Authority.  An Agent Profile fixes its Run
Capability; a Request may omit it or repeat it, but cannot reinterpret it.
_Avoid_: Agent, mode

**Topology**:
The organization of execution participants, such as single, subagent, team, or
workflow.  Topology does not define their Capability.
_Avoid_: Team Agent

**Authority**:
The effective policy limiting Effects on Resources and Scopes.  It is composed
at execution time and can only be tightened by delegation.
_Avoid_: Permission level

**Session**:
A durable logical context spanning multiple Turns and Runs.
_Avoid_: Buffer, Run

**Turn**:
The lifecycle of one immutable Request after Runtime accepts it.
_Avoid_: Session, model call

**Run**:
One concrete execution by an Actor within a Turn.  Child Runs represent
delegated execution, explicitly reference their direct parent Run, and do not
create a separate Runtime.
_Avoid_: Agent, Session

**Preset**:
A portable configuration source that can describe prompts, workflows,
Capabilities, or Agent Profiles.  A Preset is not inherently an Agent.
_Avoid_: Agent

## Tape and Context

**Tape**:
The append-only, durable history of Runtime Facts.  New summaries,
corrections, and judgments become new Entries; they never overwrite an older
Entry.
_Avoid_: mutable conversation state, latest state

**Entry**:
One immutable, ordered Fact on Tape with its own identity and provenance.
_Avoid_: message slot that can be replaced

**Anchor**:
An Entry that marks, indexes, or distills earlier Entries while retaining
enough source identity to return to them.  It may be a deterministic index or
an interpretation, but is always a reconstruction marker rather than a
deletion point.
_Avoid_: replacement, compressed Tape

**Tape View**:
A purpose-specific, rebuildable selection and rendering of Tape Entries.  It
may omit raw Entries or show a derived item in their place for one context,
but it never mutates Tape.
_Avoid_: stored history, rewritten Tape

**Represented Range**:
A View-level claim that one identified derived item stands in for an
identified range of raw Entries in that View.  The raw Entries remain on Tape,
and the correspondence must be auditable.
_Avoid_: replaced range, deleted history

**Projection**:
A rebuildable fold of Tape Facts into a current read model.  Materialized
Projection state may change or reset, but Tape Facts do not.
_Avoid_: source of truth, Tape mutation

**Handoff**:
A context or execution transition built from durable history, normally using
an Anchor as the reconstruction point.  It changes the active View, not the
past.
_Avoid_: history reset, new Runtime

## Experience Feedback

**Experience Thread**:
An auditable lineage of immutable Entries about one subject.  Later Claims,
Feedback, and Outcomes may extend or revise the interpretation of earlier
Facts without changing any earlier Entry.
_Avoid_: mutable memory record, latest truth, in-place learning

**Fact**:
An immutable, addressable observation already preserved by a Runtime surface,
such as a Tape event, Artifact revision, or Outcome Record.
_Avoid_: Claim, truth

**Claim**:
A contestable statement about Facts within an explicit scope.  A Claim may be
wrong and may be superseded, but is never overwritten.
_Avoid_: Fact, Memory, learned capability

**Feedback**:
An append-only response that supports, challenges, or corrects one Claim and
identifies its Actor and factual basis.
_Avoid_: mutation, Authority decision

**Current View**:
A Projection-derived interpretation of Claims and Feedback for a scope.  It
is rebuildable and must expose unresolved conflict instead of storing truth.
_Avoid_: canonical state, latest row wins

## Epistemic Learning

**Decision Snapshot**:
An immutable record of what a Run could see, predict, and choose immediately
before an Action.  It excludes later Outcomes and oracle material.
_Avoid_: reconstructed context, hindsight View

**Outcome**:
An immutable post-Execution observation from an environment, verifier, or
user about what occurred.  It does not label every earlier Action correct or
establish why the result occurred.
_Avoid_: success Pattern, step label, causal explanation

**Attribution Claim**:
A contestable post-Outcome Claim that identified conditions or Actions
contributed to an Outcome.  It never changes what was knowable in the earlier
Decision Snapshot.
_Avoid_: causal Fact, hindsight truth

**Applicability Graph**:
An auditable relation model connecting contexts, Pattern Claims, Actions,
Outcomes, and Exercises.  Its relations express conditional support,
conflict, or missing evidence rather than universal truth.
_Avoid_: similarity index, source of truth, universal knowledge graph

**Applicability Relation**:
An append-only reviewed judgment that a Pattern Claim or Candidate applies or
fails under one Context Claim.  A failed Action or Outcome is evidence for
review, not an automatic negative relation.
_Avoid_: failure log, automatic rejection, mutable applicability flag

**Review Need**:
A rebuildable indication that recorded recovery evidence awaits an explicit
judgment.  It neither changes Promotion nor establishes an Applicability
Relation by itself.
_Avoid_: automatic verdict, negative relation, stored truth

**Experience View**:
A purpose-specific View that presents at most one active promoted Candidate
applicable to the current Context.  When enabled, ordinary recovery work uses
it automatically; missing, conflicting, or ambiguous applicability yields no
Candidate.
_Avoid_: manual context attachment, automatic Learning, mutable Memory

**Exercise**:
A preregistered task designed to distinguish competing Pattern Claims or test
an applicability boundary.  Actor-visible material excludes its sealed oracle
and evaluation target.
_Avoid_: similar task, answer-bearing prompt, training example

**Promotion**:
An append-only decision that makes one versioned Candidate eligible to shape
a future View or Action policy after Evaluation.  It is not an assertion that
the Candidate is universally true.
_Avoid_: overwrite, Candidate creation, automatic truth

**First Recovery Action**:
The first user-visible instruction or effect-seeking Action intended to recover
from an observed failure.  It should use the unique reviewed Candidate that
fits the context without requiring the user to restate it, or ask rather than
guess when no Candidate applies uniquely.
_Avoid_: first model output, first tool call

**Recovery Class**:
A category of failures sharing the same operation boundary, stable error
identity, and recovery contract across Runs.  Membership is determined from
structured failure Facts; a model judgment or textual similarity cannot
establish it.
_Avoid_: similar failure, error cluster, exception message

**Recovery Intervention**:
One minimal insertion of an applicable Experience View after a known failure
and before the First Recovery Action.  It supplies reviewed context but does
not choose the Action, force a tool, retry, or otherwise orchestrate the model.
_Avoid_: recovery controller, forced workflow, policy override

**Acceptance Reference Cell**:
A sealed combination of product source, Provider/Model, Agent Profile,
model-visible context, tools, inference settings, and execution limits used by
one default-enablement gate.  Passing it authorizes only the declared product
behavior, not a Provider/Model support matrix.
_Avoid_: model name, runtime allowlist, support claim

## Experience Observation

**Designer Prior**:
Every instruction, affordance, topology, policy, and model-facing description
that could have shaped an observed behavior before it occurred.
_Avoid_: prompt text alone, experiment notes

**Observation Opportunity**:
A preregistered exposure in which a behavior could occur and its meaningful
alternatives and non-occurrence are observable.
_Avoid_: Run, task, occurrence

**Artifact Access**:
The successful delivery of one explicitly requested Artifact to a model Run.
It proves neither choice among alternatives nor use of the content.
_Avoid_: Artifact reuse, semantic uptake

**Artifact Selection**:
An explicit choice among candidates proved visible and equally available in
one Observation Opportunity.  A default or implicit active Artifact is not a
Selection.
_Avoid_: read, access, active Artifact fallback

**Semantic Use**:
A task-relevant, correct application of Artifact content supported by a
counterfactual Content Dependence result and blinded evaluation.
_Avoid_: read completion, copying, reuse

**Content Dependence**:
A counterfactual relationship in which withholding or changing Artifact
content changes a later result in a preregistered direction.
_Avoid_: access, string overlap, understanding

**Task Effect**:
A causal difference in a frozen quality, cost, latency, or safety Outcome
attributable to an Artifact condition.  It is separate from Semantic Use.
_Avoid_: association, Semantic Use, Outcome label

**Reusable Improvement**:
A beneficial Task Effect that survives preregistered held-out tasks,
counterexamples, and safety and cost gates.
_Avoid_: single-task win, benchmark score, Learning

**Learning**:
A durable change in how Runtime constructs a View or chooses and executes an
action, justified by an auditable Experience Thread and evaluated by Outcomes
appended back to Tape.  It requires a repeated, held-out Task Effect against
an otherwise matched no-Experience baseline, plus safety, cost, persistence,
and revocation evidence.  For an Agent to learn from experience, the Candidate
must be generated by a post-Run model reflection over its own closed
trajectory; a runner-written rule or a source prompt that states the answer is
supervised replay, not Learning.  Human Promotion may remain the safety gate,
but it must be recorded separately from model induction.  A retrieved View or
one successful recovery is only retrieval/use, not Learning.  Learning never
overwrites prior Entries or changes model weights.
_Avoid_: context injection, one successful intervention, Memory write, Skill
overwrite, model training, fine-tuning

**Experience Observer**:
A read-only role that compares frozen Facts with Designer Prior and proposes
contestable Pattern Claims.  It owns no Runtime state or Authority.
_Avoid_: Observer module, Pattern Engine, Learning Layer

**Pattern Claim**:
A Claim that one behavior repeats across eligible Observation Opportunities,
with its Prior classification, counterexamples, and missing evidence exposed.
_Avoid_: Pattern Fact, learned Capability

**Candidate**:
A reviewed Pattern Claim eligible for a separately preregistered intervention.
It is not evidence that the Pattern is useful or has been learned.
_Avoid_: Capability, promotion, validated improvement
