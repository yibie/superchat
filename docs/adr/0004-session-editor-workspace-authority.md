---
status: accepted
scope: live Emacs resources created for a Superchat Session and native Session-tab closure
applies_when: Superchat allocates or closes process-local editor buffers associated with a stable Session ID
source Evidence ID: E-20260903-003
verification: slices 01–02 focused tests and canonical 1184-test suite; independent adversarial review found no remaining High or Medium issue
clarifies: ADR 0002 uses “Workspace” for optional buffer-derived or durable work scope; it does not describe this Session-scoped live ownership authority
---

# Put live editor-resource ownership at the Session boundary

## Context

Runtime's legacy HostLease/Cleanup model makes a Run responsible for an
ephemeral editor buffer.  In interactive Emacs use, the useful lifetime is the
Session: several Runs use the same chat surface, and users expect the Session
tab and its private buffers to close together.  Inferring ownership from a
buffer name, project root, selected window, or tab layout cannot prove that
Superchat created the buffer.

ADR 0002 deliberately rejected making a filesystem Workspace the mandatory
parent of every Session or Run.  This decision does not reverse that result.
It introduces a different object: a process-local Session EditorWorkspace for
live resources, distinct from the durable Artifact Workspace and optional
project scope discussed there.

## Decision

- One stable Session ID names one logical EditorWorkspace.  Its live state is
  lazy and may be unmaterialized.
- Buffer-local Session and owned markers on the live buffer object grant close
  authority.  Membership is recalculated from `buffer-list`; there is no
  parallel buffer ledger.
- Existing file, Artifact, and other user buffers are borrowed.  Association,
  location under a root, or display in a tab/project/ibuffer view does not make
  them owned.
- A Session tab is a native presentation and close adapter.  Its stable Session
  parameter routes closure but does not prove buffer ownership.
- Native tagged-tab closure is exclusive once it reaches Superchat's hook:
  earlier prevent hooks may veto first, but EditorWorkspace then performs the
  whole close transaction and consumes the event.  This avoids a later hook
  leaving a tagged tab after irreversible Run cancellation and buffer closure.
- Runtime keeps authority over Run cancellation and its still-enacted legacy
  HostLease/Cleanup compatibility path until the remaining migration slices
  retire that path.

## Consequences

- Rejected close has zero effects and gives the user a manual retry command.
- Buffer kill refusal produces a visible `retained` workspace; retry rescans
  current markers instead of consulting stale inventory.
- Project and ibuffer integration can remain read-only projections over the
  same membership authority.
- The hook is intentionally not cooperative with later prevent-close hooks for
  tagged Session tabs.  Ordinary pre-close hooks still execute during the
  internal native deletion.
- Closing the live scope has no authority to delete durable Session, Artifact,
  Anchor, or Tape data.

## Rejected alternatives

- Per-Run editor ownership: it couples optional UI resource failure to the Run
  terminal lifecycle and requires a second cleanup-recovery contract.
- Name-, directory-, project-, or tab-derived ownership: each can include
  buffers that Superchat did not create.
- Allowing later prevent hooks after Session effects: a later veto can split
  the visible tab state from already performed cancellations and kills.
