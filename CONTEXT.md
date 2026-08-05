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

## Experience Feedback

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
