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
from Topology and the Session's Permission level.  An agent Preset fixes its Run
Capability; a Request may omit it or repeat it, but cannot reinterpret it.
_Avoid_: Agent, mode

**Topology**:
The organization of execution participants, such as single, subagent, team, or
workflow.  Topology does not define their Capability.
_Avoid_: Team Agent

**Permission level**:
A Session's standing answer to what a tool call may do: a sandbox mode
(`read-only`, `edit-buffers`, `danger-full-access`; `workspace-write` is the
default's former name and still accepted) plus an approval policy (`ask`,
`never`).  A level decides how the tools that exist may execute, never which
tools exist.  Built-in tools edit ordinary buffers freely except under
`read-only`, and never Superchat's own buffers; full-Emacs-control and
third-party tools are asked about, refused or free by level.  A delegated child inherits its parent's
mode with `never`.  It replaced Authority (effect policies) in M6; what was left of that module,
the tool execution pipeline, is `superchat-tool-execution.el` (named
`superchat-authority.el` until 2026-09-20).
_Avoid_: Authority, authority policy, attendance

**Session**:
A durable logical context spanning multiple Turns and Runs.  The chat View
shows one Session at a time, either beside the file being edited (editor mode)
or in the Session's own tab (agent mode); both modes show the same Session.
Entry points invoked from an ordinary buffer — inline ask, inline act, region
rewrite — join the Session the chat View is showing, not one chosen by project.
_Avoid_: Buffer, Run, project session

**Session Buffer Set**:
The live set of Emacs buffers one Session owns.  Ownership comes from exact
markers on the buffer, never from its name, tab, project or directory.  Where a
buffer is displayed (the Session's tab, or a side window of another tab) is
not ownership.  It is distinct from the durable Artifact Board; Runtime
borrows a buffer from it and never closes one.
_Avoid_: Session EditorWorkspace and EditorWorkspace (the name code identifiers
and older documents still carry), Session buffer, HostLease

**Owned Editor Buffer**:
A buffer created and marked by one Session Buffer Set.  Only the owning
Buffer Set may close it.
_Avoid_: chat buffer, project buffer, borrowed buffer

**Buffer Set Acquisition**:
Obtaining a Session's buffer for one role, learning in the same step whether
it was newly created.  The result is not a lease or a token; it only lets a
producer roll back a fresh buffer before Runtime accepts the Request.
_Avoid_: EditorWorkspace Acquisition, HostLease, ownership token, buffer handle

**Operation-scoped Child Buffer Set**:
A child Session's Buffer Set created by a producer for one bounded
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

**Skill**:
A named piece of task-specific instructions in a file (a folder with
`SKILL.md`, or a single file with a description) that the model finds in the
skill catalog and reads when a task needs it, or that the person invokes with
`>name`.
_Avoid_: preset (a Skill of type agent or plan is also a Preset), plugin

**Skill catalog**:
The list of Skills — name, one-line description, file location — carried in
the system text so the model knows what it can read.
_Avoid_: skill index, registry

**Region rewrite**:
One instruction applied to exactly the Rewrite target; its answer is the
replacement text alone, and it changes the buffer only when the user accepts it.
_Avoid_: inline edit, refactor

**Rewrite target**:
The text the user selected for a Region rewrite, and the only text its answer
may replace.
_Avoid_: region (when the surrounding text is meant), context

**Rewrite reference**:
Text around the Rewrite target sent so the model understands its setting; the
model may read it but never changes or returns it.
_Avoid_: context (the Context View is something else)

**Preset**:
A portable configuration source that can describe prompts, workflows,
Capabilities, and Plugin composition for a kind of work.  A Preset of type
agent is a reusable role definition that fixes its default Capability; a
Preset is not inherently an Agent and cannot widen a Permission level or own Runtime
state.  A Preset can define scoped Plugin composition; this registration
context is distinct from Session identity and Run execution.  A Session is
not a Scope layer.
_Avoid_: Agent, Agent Profile, profile

## Workspace

The behaviour behind Workspace, Temporary Workspace, Personal Workspace and
Work Record is the
[accepted contract](docs/decisions/accepted/2026-10-01-workspace-work-record-session-rules.md);
which of it is built is stated per slice in the roadmap's M11.

**Workspace**:
The place work happens in: one directory that Emacs treats as a project,
together with what Superchat keeps for it — its rules (the `AGENTS.md` at its
root), its conversations and its Work Records.
A buffer belongs to the Workspace of its file; on screen a Workspace is its tab
group.
_Avoid_: project (Emacs's own object, which says where a Workspace is), working
copy (what jj calls a workspace), global memory

**Temporary Workspace**:
The Workspace of work done outside any project.  It has no directory and so no
rules, holds one conversation, and nothing in it is carried into another
conversation.
_Avoid_: default workspace, global workspace

**Personal Workspace**:
The Workspace the user designates as the home of interfaces that belong to no
directory, such as the agenda, feeds and chats.  Until one is designated, work
started from them is in a Temporary Workspace.
_Avoid_: global workspace, default workspace, home

**Work Record**:
The durable record of a piece of work that has to outlive the Run that started
it: what is to be delivered, how far it has got, why it cannot go on yet, and
where to pick it up.  Its id is what the conversations, Runs and agents working
on that piece share; it is not a log of every Run.
_Avoid_: work item, todo, task, reminder, ticket, work log

**Artifact Board**:
The durable registry of Artifacts — published pieces of work product, each with
a revision — through which a lead agent and its sub-agents pass results.
_Avoid_: Artifact Workspace, Workspace (for this), shared memory

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
model loop, tool pipeline, Tape or chat window.
_Avoid_: core, runtime

**Invariant**:
A rule that holds regardless of which Plugins are mounted, checked at a seam
the Kernel owns rather than inside any one Plugin.  Examples: a source buffer
is unchanged until its Change Set is accepted; every terminal Run state is
recorded on the Tape; every tool execution passes the tool pipeline.
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

Claim, Feedback and Current View name the interpretation of retained `claim.recorded` and `feedback.recorded` Entries. These terms do not imply a built-in capture, correction or review command; see the [M3a decision](docs/decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md). Removing an entry point does not erase an Entry or turn a Claim into a Fact.

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
_Avoid_: mutation, permission decision

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
