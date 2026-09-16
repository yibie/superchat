# Superchat Agent Runtime

Superchat coordinates durable, tool-using work inside Emacs.  This language
keeps execution identity separate from work purpose, permissions, and
organization.

## Language

**Actor**:
An identity to which Runtime actions and events are attributed.  Users,
workflow steps, and agent Runs can all be Actors; not every Actor is an
Agent.
_Avoid_: Agent, when the participant may be non-agent

**Capability**:
The purpose and default behavior for a kind of work, selected independently
from Topology and effective Authority.  An agent Preset fixes its Run
Capability; a Request may omit it or repeat it, but cannot reinterpret it.
_Avoid_: Agent, mode

**Topology**:
The organization of execution participants, such as single, subagent, team, or
workflow.  Topology does not define their Capability.
_Avoid_: Team Agent

**Authority**:
The effective policy limiting Effects on Resources.  It is composed
at execution time and can only be tightened by delegation.
_Avoid_: Permission level

**Session**:
A durable logical context spanning multiple Turns and Runs.
_Avoid_: Buffer, Run

**Session EditorWorkspace**:
The live set of Emacs buffers one Session owns.  Ownership comes from exact
markers on the buffer, never from its name, tab, project or directory.  It is
distinct from the durable Artifact Workspace; Runtime borrows a buffer from
it and never closes one.
_Avoid_: Session buffer, Artifact Workspace, HostLease

**Owned Editor Buffer**:
A buffer created and marked by one Session EditorWorkspace.  Only the owning
EditorWorkspace may close it.
_Avoid_: chat buffer, project buffer, borrowed buffer

**EditorWorkspace Acquisition**:
Obtaining a Session's buffer for one role, learning in the same step whether
it was newly created.  The result is not a lease or a token; it only lets a
producer roll back a fresh buffer before Runtime accepts the Request.
_Avoid_: HostLease, ownership token, buffer handle

**Operation-scoped Child EditorWorkspace**:
A child Session's EditorWorkspace created by a producer for one bounded
operation and closed by that producer afterwards, not by Runtime's terminal
completion.
_Avoid_: per-Run cleanup, parent-owned host, direct buffer kill

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
Capabilities, and Plugin composition for a kind of work.  A Preset of type
agent is a reusable role definition that fixes its default Capability; a
Preset is not inherently an Agent and cannot grant Authority or own Runtime
state.  A Preset can define scoped Plugin composition; this registration
context is distinct from Session identity and Run execution.  A Session is
not a Scope layer.
_Avoid_: Agent, Agent Profile, profile

## Composition

**Plugin**:
A unit of code that is mounted into a Scope and contributes Services, Event
listeners, tools, commands, Surfaces and policies through that Scope.
Scope-accounted contributions are reversible: unmounting the Plugin withdraws
them.  This does not make arbitrary Elisp mutation or an external Effect
reversible.  Built-in and third-party Plugins use the same composition model;
a product feature is not necessarily one Plugin.
_Avoid_: Extension (pi's term for a flat event-subscribing module), module,
package

**Scope**:
The registration context a Plugin receives.  Everything registered through a
Scope is visible within it and is withdrawn when it is disposed.  Scopes may
nest; a child sees its ancestors' contributions, never the reverse.
_Avoid_: context (overloaded with prompt context), namespace

**Service**:
A named capability with a declared interface, provided by exactly one mounted
Plugin at a time and consumed by others by name.  A Plugin declares the
Services it requires and is mounted only once they exist.  Consumers never
reach a provider's code directly.
_Avoid_: library, helper, `require`

**Event**:
A named extension point on the execution main line.  An Event declares one
combination rule for its listeners (first result wins, collect all, fold,
observe only, or an ordered-await waterfall), which is part of its contract.
An observe listener may be declared required when it is registered; its
failure is returned to the caller of that emission, which decides what it means.
_Avoid_: hook, signal

**Kernel**:
The part of Superchat that is not a Plugin: the Plugin loader, the Scope,
Service, Event and Invariant registries, the default mount list, and the
shared vocabulary every Plugin speaks (Envelope, Sink).  Kernel names
composition infrastructure, not the product responsibilities of the driver,
model loop, Authority, Tape or chat window.
_Avoid_: core, runtime

**Invariant**:
A rule that holds regardless of which Plugins are mounted, checked at a seam
the Kernel owns rather than inside any one Plugin.  Examples: a source buffer
is unchanged until its Change Set is accepted; every terminal Run state is
recorded on the Tape; every tool execution passes Authority.
_Avoid_: assertion, contract

**Envelope**:
The immutable description of one Request as a producer hands it in: content,
Surface, media, Sink, and owner-prefixed context.  Every Surface produces the
same shape.
_Avoid_: request plist, legacy request

**Sink**:
The exact delivery target of a Run: the callbacks that receive its stream,
status, terminal result or failure.  A Sink carries no buffer routing; the
producer decides what to do with what it is handed.
_Avoid_: delivery function, stream function, view

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

Claim, Feedback and Current View name the interpretation of retained `claim.recorded` and `feedback.recorded` Entries. These terms do not imply a built-in capture, correction or review command; see the [M3a decision](docs/decisions/accepted/2026-09-15-m3a-remove-closure-and-claims.md). Removing an entry point does not erase an Entry or turn a Claim into a Fact.

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
A contestable statement about Facts within an explicitly stated subject.  A
Claim may be wrong and may be superseded, but is never overwritten.
_Avoid_: Fact, Memory, learned capability

**Feedback**:
An append-only response that supports, challenges, or corrects one Claim and
identifies its Actor and factual basis.
_Avoid_: mutation, Authority decision

**Current View**:
A Projection-derived interpretation of Claims and Feedback for one subject.  It
is rebuildable and must expose unresolved conflict instead of storing truth.
_Avoid_: canonical state, latest row wins

## Epistemic Learning

**Outcome**:
An immutable post-Execution observation from an environment, verifier, or
user about what occurred.  It does not label every earlier Action correct or
establish why the result occurred.
_Avoid_: success Pattern, step label, causal explanation

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
A purpose-specific View that presents at most one active promoted Candidate applicable to the current Context. When enabled, recovery work uses it through the recovery Service; missing, conflicting, or ambiguous applicability yields no Candidate. This recovery View is distinct from a standalone prompt-context contribution.
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

**Candidate**:
A reviewed Claim eligible for a separately preregistered intervention.  It is
not evidence that the pattern is useful or has been learned.
_Avoid_: Capability, promotion, validated improvement

**Learning**:
A durable change in how Runtime constructs a View or chooses an action,
justified by an auditable Experience Thread and evaluated by Outcomes
appended back to Tape.  The Candidate must come from a post-Run model
reflection over its own closed trajectory; a hand-written rule is supervised
replay, not Learning.  Human Promotion may remain the safety gate but is
recorded separately.  Learning never overwrites prior Entries or changes
model weights.
_Avoid_: context injection, one successful intervention, Memory write, Skill
overwrite, model training, fine-tuning
