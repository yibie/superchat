# Session EditorWorkspace

- Status: confirmed
- Revision: 5
- Decision class: breaking migration
- Captured: 2026-09-04
- Next action: `arrange`

## Problem and goal

Superchat currently treats an Emacs buffer used by a Run as a per-Run
`HostLease`.  Runtime acquires that lease, records Cleanup state, and attempts
to release the buffer after terminal delivery.  This makes editor-resource
lifetime part of the Run state machine and creates secondary problems:

- Cleanup needs public discovery, recovery, retry, abandonment, and late
  failure notification semantics.
- Workflow, Runtime, UI, and tests must coordinate around a resource that
  Emacs already knows how to organize as buffers, tabs, projects, and ibuffer
  groups.
- A Run is made to appear as the owner of an editor workspace even though all
  Runs carrying the same exact Session ID share one editor-resource scope.
- A large amount of contract surface is required to explain exceptional
  cleanup states instead of making ownership visible and closing it at the
  correct lifecycle boundary.

The goal is to move live editor-resource ownership to one
`Session EditorWorkspace` per Session.  A Run uses an EditorWorkspace; it does
not own or close that workspace.  Emacs-native mechanisms provide the visible
organization:

- an optional tab for interactive presentation and closure;
- an optional transient `project.el` view at the Session's authoritative
  durable Workspace root;
- an ibuffer group for inspection and manual recovery;
- live buffer-local ownership metadata as the exact authority for which
  buffers Superchat may close.

The resulting module must be small and deep: one authority answers membership,
status, and close behavior, while tab-bar, project, and ibuffer remain derived
adapters.  It must not introduce another per-buffer ledger or another Run
cleanup state machine.

## Unchanged contracts

The following contracts remain in force:

- `superchat-runtime.el` remains the unique authority for Request → Turn/Run
  lifecycle, terminal state, cancellation, delivery, and public Run status.
- Runtime still releases execution handles it owns, including transport,
  timers, callbacks, and pauses.  This SPEC changes editor-buffer ownership,
  not execution cleanup.
- `superchat-workspace.el` remains the authority for durable Workspace and
  Artifact identity, revisions, Projection, and editing tools.
- Tape remains append-only.  Closing an EditorWorkspace does not delete or
  rewrite Session history, Runs, Artifacts, Anchors, or Tape entries.
- Reopening a Session reconstructs its View from durable Tape and existing
  projections.
- All model-invoked tools continue to cross the shared Authority seam.
- Existing kernel layering remains intact.  No kernel file may require the
  EditorWorkspace module or any tab/project/ibuffer UI adapter.
- Buffer identity is exact Emacs buffer identity.  A display name is not an
  ownership capability.
- Existing public Session opening, conversation, Tape, and Artifact behavior
  is preserved unless explicitly changed below.

## Decision and boundaries

### One Session, one EditorWorkspace identity

Every Session has exactly one logical EditorWorkspace identity, keyed by the
stable Session ID.  Its live projection may be materialized lazily.  A Session
that has not opened a tab and has not allocated editor resources need not have
an eagerly created record, directory, or buffer.

The EditorWorkspace is distinct from the durable Artifact Workspace:

- **Session EditorWorkspace**: live Emacs resource scope, keyed by Session ID.
- **Artifact Workspace**: durable Artifact registry and revisions, retaining
  its existing Workspace ID and optional Session binding.

If the Session has an authoritative Artifact Workspace root, the
EditorWorkspace may expose it through a transient `project.el` object.  If no
such root exists, no fake project or temporary directory is manufactured.

Session identity is not inferred from a live View buffer.  Workflow must retain
the originating Session ID even if its View dies; correlation ID is not a
Session fallback.  A Subagent or compactor that intentionally has a distinct
child Session ID has a distinct EditorWorkspace identity, even when it shares
the parent's Team or durable Artifact Workspace.

### One middle-tier authority

A new middle-tier module, provisionally `superchat-editor-workspace.el`, owns:

- live EditorWorkspace state;
- allocation and tagging of Superchat-owned buffers;
- enumeration of live owned buffers;
- close planning and close execution;
- the retained state and retry path;
- the public manual close command;
- internal tab-bar, transient-project, and ibuffer adapters.

The exact filename may change during `arrange` only if the same single
authority and tier are preserved.  Keeping the built-in Emacs adapters behind
one deep middle-tier interface is preferred over creating several shallow
modules or registering a new optional integration.

`superchat-runtime.el` must not require or call this module.  Request-producing
middle-tier/plugin code obtains an exact buffer from the EditorWorkspace and
passes it to Runtime.  Runtime treats that editor buffer as borrowed for the
duration of the Run and never kills it.

### Emacs mechanisms are adapters, not ownership authorities

- A tab stores window/layout state and a stable Session ID parameter.  Tab
  name, selected window, current buffer, and `window-configuration` are not
  used to infer ownership.
- `project.el` supplies root-aware discovery and commands.  It does not decide
  which buffers Superchat may kill.
- ibuffer supplies a visible group and manual inspection surface.  A filter
  group is not a resource ledger.
- The live `buffer-list`, filtered by exact buffer-local ownership metadata,
  is the membership truth.  No parallel list of buffer objects is maintained.

### Owned and borrowed resources

An **owned buffer** is created by the Session EditorWorkspace specifically for
that Session and bears a buffer-local stable Session ID plus an owned marker.
One live buffer can be owned by at most one Session EditorWorkspace.

A **borrowed buffer** already exists independently of the EditorWorkspace, or
is a user/file/Artifact source buffer.  It is never auto-killed by Workspace
closure.  Borrowed relationships remain in the existing context/Artifact
model; the first delivery does not add a misleading single-session ownership
tag to arbitrary shared file buffers.

Private Superchat-owned buffers may clear their own
`kill-buffer-query-functions` at creation when their contents are disposable
and recoverable from durable state.  Superchat does not modify this hook in
borrowed buffers and does not bypass unrelated kill queries at close time.

Marker representation, validity, and owner-Session interpretation remain
private to the EditorWorkspace module.  Other modules may ask the public
EditorWorkspace interface for the valid owning Session of one exact buffer;
they must not read or partially reinterpret the private buffer-local markers.
Ownership exists only when both the owned marker and Session marker are
buffer-local and valid; default values, dynamic bindings, or partial metadata
never grant close authority.

### Producer identity survives extension composition

A Request producer fixes its Session ID and exact buffer before extension
callbacks contribute context, knowledge, tools, or policy.  Extension
composition may enrich the Request but may not retarget those two identity
fields.  The submission seam captures and validates the producer pair before
composition, rejects any identity change before Extension activation is
recorded, validates the final pair again, and only then permits activation
Tape and Runtime effects.  A rejected composition produces no activation
Tape, Run, or provider effect.  This keeps Extension a contribution mechanism
rather than a second Session router.

### Operation-scoped protocol child Sessions

Compaction keeps its distinct child Session and EditorWorkspace for isolation;
it never borrows the parent Session's owned View.  Each compaction child is
operation-scoped.  After terminal processing, or when Runtime submission
fails after materialization, Compact explicitly requests closure through the
public EditorWorkspace close authority.  Compact never kills the buffer
directly, and child closure does not delete its durable Session or Tape facts.

This is not Runtime terminal cleanup: Runtime still never closes an
EditorWorkspace merely because a Run completed, failed, or was cancelled.
The Compact producer that created the operation-scoped child explicitly ends
that child Session's live editor scope after it has handled the terminal
result.  Closure failure remains visible through the existing retained state
and recovery interface.

A synchronous nonlocal exit from Runtime submission, including `quit`, must
first seal any already-published Run through Runtime's existing terminal and
delivery path, then propagate the original condition.  Compact clears the
parent View's in-flight flag on every failed submission exit.  It does not
simulate Runtime terminal ownership or leave an accepted child Run active.

### Skills match and Subagent child lifetimes

Asynchronous Skills semantic matching is a bounded protocol operation, not a
user-trackable Session surface.  It opens no tab, returns no Session ID, and
its callback continues to return only the match result.  Its distinct child
Session and `skills-match-protocol` host are therefore operation-scoped.  The
Skills producer must request public EditorWorkspace closure exactly once after
terminal delivery, including completed, cancelled, provider-error, callback
`error`, and callback `quit` paths, and after a synchronous Runtime submission
rejection.  A synchronous provider condition propagated after Runtime has
already accepted, sealed, and delivered the Run is still the same terminal
close attempt; it must not trigger a second close.

Skills never kills the host directly.  If ordinary public close refuses or
fails, the exact child Session ID and retained state must be reported through
an actionable diagnostic, the live marked buffer remains discoverable through
the EditorWorkspace authority, and a later public close is the retry.  Retry
does not repeat semantic matching, Runtime terminal delivery, or the first
close attempt.  Durable child Session and Tape facts, when any were published,
remain append-only and are never deleted by close.

A Subagent is different: its child Session is a trackable execution identity,
not an operation-scoped editor lifetime.  Once Runtime accepts the child Run,
success, cancellation, provider error, callback `error`, and callback `quit`
all preserve the Session-owned `subagent-protocol` host.  That host remains
under Session lifetime and the ordinary public close authority so the child
Session can be inspected and revisited.

Before Runtime acceptance, however, a synchronous submission rejection has no
durable Run/Tape discovery edge.  The Subagent producer must atomically roll
back only a `subagent-protocol` host newly acquired by that invocation.  If the
role lookup reused a pre-existing host, rejection must not close it.  Normal
return of an accepted Run handle or synchronous terminal delivery proves
Runtime acceptance; because Runtime seals and delivers a published Run before
propagating a synchronous provider `error` or `quit`, those paths preserve the
host rather than taking pre-acceptance rollback.

The existing public allocation and close interfaces are compatible with this
decision but not sufficient to implement that distinction safely:
`superchat-editor-workspace-buffer` returns only the buffer, so a producer
cannot atomically distinguish a fresh acquisition from role reuse, while
`superchat-session-workspace-close` closes a whole Session rather than one
acquisition.  The minimal additive boundary is a separate public acquisition
API that atomically returns a plain value containing the exact buffer and an
authority-computed `newly-acquired` boolean, plus one public idempotent
rollback operation that accepts those exact fields.  The existing
`superchat-editor-workspace-buffer` API continues to return only a buffer and
is unchanged.  Rollback is a no-op for reuse; for a fresh acquisition it
validates the exact live owner and role, attempts normal close of only that
buffer, and publishes `retained` if a query or hook prevents release.  The
plain acquisition value is not an attestation token, is not stored in a
registry, and grants no general kill authority.

This correction authorizes the minimum corresponding change to
`superchat-editor-workspace.el`, `superchat-skills.el`, and
`superchat-subagent.el`, plus their focused authority/producer tests.  It does
not authorize any Runtime or transport dependency on EditorWorkspace, a new
global registry, a second ownership classifier, producer-side marker reads, or
name-based cleanup.

### Blocking adapter identity

Org Babel, Magit, and Rewrite continue to borrow the ordinary exact invocation
or source buffer required by their synchronous host contracts.  An unowned
buffer receives one adapter-call Session ID without being tagged or mutated.
An exactly owned buffer reuses its public owner Session.  Malformed local
metadata is rejected; inherited/default metadata is not ownership and
therefore degrades to the ordinary borrowed-buffer rule; a Session/owner
mismatch is rejected by public validation.  None of these adapters may repair,
clear, or install ownership markers.  Rewrite's later ediff preview buffers
remain lexical preview resources and can never replace the captured source
buffer as the Runtime host.

### Cooperative Emacs boundary

This is an in-process Emacs Lisp ownership contract.  It prevents accidental
cross-Session cleanup by Superchat code and makes survivors observable.  It
does not claim to defend against arbitrary third-party Lisp deliberately
removing buffer-local markers, killing buffers directly, or mutating tab
parameters.

## Model delta

### Removed model

For newly submitted Runs, remove editor buffers from the per-Run HostLease and
Cleanup model:

- no per-Run editor-buffer acquisition/release authority;
- no `unresolved` editor ownership state;
- no public Run Cleanup retry/abandonment contract for editor buffers;
- no independent post-terminal notification contract for editor-buffer
  Cleanup failure.

The word “cleanup” may still describe Runtime's private release of execution
handles, but it is not a public editor-resource lifecycle.

Runtime quiescence is not an EditorWorkspace state.  Every terminal Run must
unconditionally cancel or sever the execution handles it owns before any
optional editor-resource result can affect control flow.  Failure to close an
owned buffer must never skip or defer Run quiescence.

### Added model

An EditorWorkspace has these live states:

```text
unmaterialized -> open -> closing -> closed
                          |   ^
                          v   |
                       retained
```

- `unmaterialized`: logical identity exists; no live record is required.
- `open`: resource allocation and registration are allowed.
- `closing`: new owned-buffer allocation is refused; active Runs are being
  cancelled if authorized, and owned buffers are being closed.
- `retained`: at least one owned buffer refused or failed closure.  The tab and
  surviving buffers remain visible; allocation stays frozen.  Retry returns to
  `closing`.
- `closed`: no live owned buffers remain and the Session tab is closed.  A
  later Session open creates a new `open` live projection for the same Session
  identity.

There is no `unresolved` state:

- a live buffer with the exact owned marker belongs to that Session;
- a dead buffer is already released;
- a buffer without the marker is not auto-owned;
- a borrowed buffer is never auto-closed.

Tab presence is a presentation fact, not the EditorWorkspace state.  Headless
use has the same EditorWorkspace identity and close interface without a tab.

## Legacy implementation audit and retirement ledger

This is a focused, read-only simplification survey of the current JJ working
copy.  It establishes what must be removed or preserved during the breaking
migration.  It does not authorize product-code deletion during `capture`.

### Coverage map

The current tree does **not** yet contain a second Session EditorWorkspace
implementation.  The duplication is inside and around the existing Runtime
model: one physical buffer fact is represented several times, and mandatory
Run quiescence is coupled to optional editor-buffer release.

| Domain | Inspected surface | Classification |
| --- | --- | --- |
| Runtime ownership | `superchat-runtime.el` HostLease/Cleanup structs, acquisition, publication, status, release, retry, abandon | Current enacted authority; replacement target |
| Terminal lifecycle | finish, cancel, deadline, audit-incomplete, timer owner, callbacks, pause clearing | Must survive; not editor cleanup |
| Built-in producers | Dispatcher/Agent Loop, Workflow, Subagent, Compact, Skills | Every Request shape must be classified, not only ephemeral callers |
| Sync integrations | `ob-superchat.el`, `superchat-magit.el`, `superchat-rewrite.el` | Rely on Runtime's implicit host behavior; require explicit migration |
| Bundled plugin producers | Telega legacy/session request builders | Shipped compatibility surface; included in production zero-caller proof |
| External API | `docs/plugin-guide.md` and public `superchat-runtime-submit` | `:ephemeral-buffer` is documented for third-party plugins |
| Tests | Runtime, LLM backend, Workflow, Subagent, native UI and plugin tests | Some protect invariants; some duplicate the old implementation |
| Persistence | Tape/DB and replay paths | HostLease/Cleanup are process-local; no stored-data migration exists |
| Historical evidence | KERNEL K10/K11; `E-20260903-001` and `E-20260903-002` | Enacted baseline/evidence until `learn`; not a second implementation |
| Experiments | two learning experiments plus archived evidence manifests | Non-production samples; report separately from shipped callers |

Known blind spots are repository-external Lisp callers of the documented
Runtime fields and package hot reload while an old Run is still live.  The
approved breaking migration handles the former with an explicit compatibility
window and documentation change, and the latter with the process/restart
boundary defined below.

### Candidate A — retire the combined HostLease/Cleanup state machine

**Burden:** `superchat-runtime--release-current` currently combines two
different owners and guarantees:

- mandatory Run quiescence: deadline/transport timer cancellation, provider
  request severing, delivery/stream closure release, and exact pause clearing;
- optional editor release: killing a Runtime-owned ephemeral buffer and
  publishing `released`, `retained`, `unresolved`, or `abandoned`.

Consequently, even an ordinary borrowed View creates a hostless Cleanup and
walks `pending -> releasing -> released`.  Worse, the foreign-owner branch can
enter `unresolved` without calling the combined release body, so defensive
editor ownership logic can skip mandatory execution-handle quiescence.

**Reachability:** both terminal paths (`finish` and `audit-incomplete`) call
the combined release; public retry/abandon have no in-tree production or UI
caller, only tests and KERNEL declarations.

**Cut:** first extract one private, unconditional, idempotent Run-quiescence
seam.  Then remove the HostLease/Cleanup structs and their editor lifecycle
from Runtime after producers migrate.  Workspace `retained`/retry remains in
the EditorWorkspace authority and never re-runs terminal delivery or provider
cancellation.

**Consequence:** the documented Runtime cleanup retry/abandon API is retired;
manual recovery moves to `M-x superchat-close-session-workspace`.  This is part
of the approved breaking change, not a compatible refactor.

**Confidence / risk / proof:** high-confidence burden, medium migration risk.
Tests must prove that every terminal outcome clears execution handles even
when Workspace close is retained, and that retrying Workspace close cannot
repeat Run hooks, delivery, or provider cancellation.

### Candidate B — collapse duplicate buffer ownership representations

**Burden:** a modern ephemeral Run currently carries the same resource through
Request `:buffer-name`/`:ephemeral-buffer`/`:host-authority-mode`, Run entry
`:buffer`/`:buffer-name`/`:ephemeral-buffer`, and Cleanup's HostLease buffer.
`superchat-runtime--entry-cleanup` additionally reconstructs another lease
from legacy plist fields.  HostLease's textual `id` has zero production reader;
only tests copy it to construct substitution cases.

**Cut:** after migration, ownership exists only in the live buffer-local
EditorWorkspace marker.  A Run may hold one exact borrowed `:buffer` reference
for rendering/context and a copied name for diagnostics, but the name never
locates, recreates, or authorizes killing a buffer.  Remove HostLease ID,
legacy lease reconstruction, `:ephemeral-buffer`, `:host-authority-mode`,
`:cleanup-failure`, and name-based fallback after the compatibility boundary.

**Consequence:** hot-loaded pre-migration process-local entries cannot be
reconstructed after final retirement.  No Tape or DB representation is lost.

**Confidence / risk / proof:** high confidence.  Prove that same-name buffers
cannot be substituted, a marker grants ownership to at most one Session, an
unmarked buffer is never killed, and a live old HostLease buffer is never also
marked EditorWorkspace-owned.

### Candidate C — remove producer-side ephemeral policy duplication

**Burden and reachability:** old host policy has escaped Runtime into:

- Workflow's dead-View branch, which generates a protocol name, invents a
  correlation-ID Session fallback, copies Authority mode, and emits the
  ephemeral flag;
- Subagent request construction, which derives ephemeral behavior from the
  presence of a delivery callback;
- three Telega legacy requests plus an explicit nil in its Session request;
- public plugin documentation and its copyable example;
- two non-production learning experiments.

Other built-in producers do not emit the flag but depend on Runtime's implicit
`get-buffer-create` fallback: Dispatcher/Agent Loop, Compact, Skills, Org
Babel, Magit, and Rewrite.  They must be explicitly classified during
migration rather than omitted because a flag search did not find them.

**Cut:** producer code supplies stable Session identity and one exact buffer
obtained from the appropriate EditorWorkspace.  A headless Workspace may own a
protocol host without owning a tab.  Producer code never creates a private
ownership ledger, copies an editor Authority mode as host-acquisition policy,
or performs release.  Update the plugin guide in the same retirement slice.

**Consequence:** third-party plugins using name-plus-ephemeral fields must
migrate to the new exact-buffer/EditorWorkspace interface.  The compatibility
window must warn or adapt before final rejection; repository zero-callers
alone do not prove external absence.

**Confidence / risk / proof:** production fan-out is fully mapped in-tree;
external consumers remain uncertain.  Proof separately reports zero shipped
callers, remaining experiment samples, updated documentation, and behavior of
the temporary compatibility adapter.

### Candidate D — replace tests that reimplement ownership

**Burden:** `workflow-test--mock-runtime-submit` reads
`:ephemeral-buffer`, finds a buffer by name, and kills it.  This duplicates the
old production cleanup rule inside a mock and can make a wrong producer appear
correct.  At least 24 directly named ERT tests currently lock HostLease,
ephemeral acquisition/publication, Cleanup, retry, abandon, or host-authority
behavior; additional workflow/native tests assert legacy Request fields.

**Cut:** mocks capture Requests and trigger delivery only.  EditorWorkspace
lifecycle is tested through the real authority or a narrow interface fake.
Retire tests whose only contract is the removed representation; translate
legitimate invariants into exact-buffer, borrowed-survival, ownership-marker,
freeze, retained, and quiescence tests.

**Consequence:** no product behavior is surrendered.  Hostile lease-object
substitution tests disappear with the lease; legitimate synchronous
`kill-buffer-hook` reentrancy remains covered at the Workspace close seam.

**Confidence / risk / proof:** high confidence.  A structural test must reject
new production or test code that infers kill authority from `:buffer-name`,
project membership, tab layout, or ibuffer grouping.

### Explicitly preserved, not duplicate

The following look similar to cleanup but protect distinct failure windows and
must not be removed by this simplification:

- Runtime deadline timer (whole-Run bound);
- transport wait/hard timer owner (one provider attempt);
- provider request cancellation and late-callback rejection;
- delivery and stream closure severing;
- exact Run pause/continuation clearing;
- exact buffer object identity and dead-View no-retarget behavior;
- atomic Run terminal publication and at-most-once delivery;
- Artifact Workspace revision checks and buffers;
- temporary preview/rewrite/transport buffers owned and closed wholly inside
  their independent lexical operations.

`superchat--active-run-id` may remain as the current View's UI selection.  It
must not become another active-Run registry: Session close discovers the full
set only from Runtime's public status projection.  The general
`superchat-runtime--publish-entry` helper is not deleted merely because it was
introduced beside HostLease; its independent Run-registry guarantee must be
preserved or separately disproved.

## Action Contracts

### 1. Materialize or look up a Session EditorWorkspace

**Input:** a valid stable Session ID.

**Preconditions:** the caller has already established or loaded the Session;
the module does not invent a new durable Session.

**Effect:** return the single live EditorWorkspace projection for that Session,
creating an `open` projection lazily when editor resources are first needed.

**Postconditions:** repeated lookup for the same Session returns the same live
scope; lookup alone creates no directory, tab, project-list entry, or buffer.

**Failure:** invalid or absent Session identity is rejected without mutation.

### 2. Obtain an owned host buffer

The module exposes one allocation interface conceptually equivalent to:

```elisp
(superchat-editor-workspace-buffer SESSION-ID ROLE &optional CREATE)
```

`ROLE` is a small stable role such as a protocol host, allowing repeated Runs
in a Session to reuse an appropriate private buffer instead of allocating one
buffer per Run.

**Preconditions:** EditorWorkspace state is `open`; the Session ID and role are
valid.

**Effect:** return an existing live owned buffer for `(Session ID, role)`, or
create and tag one when `CREATE` is non-nil.

When the caller needs pre-acceptance rollback, it uses a separate public
interface conceptually equivalent to:

```elisp
(superchat-editor-workspace-acquire-buffer SESSION-ID ROLE)
;; => (:buffer BUFFER :newly-acquired BOOLEAN)
```

This interface performs the same authoritative lookup/create transaction and
returns a plain acquisition value.  `:newly-acquired` is true only when this
exact invocation created `:buffer`; role reuse returns false.  The existing
`superchat-editor-workspace-buffer` contract and its buffer-only return value
do not change.

**Postconditions:** the returned buffer has exact buffer-local ownership
metadata; its `default-directory` uses an already authoritative root or a
captured valid directory, never an invented durable Workspace.

**Failure:** allocation during `closing` or `retained` is rejected.  A stale
dead buffer is treated as absent, not unresolved.

### 2a. Query one exact buffer's owning Session

The EditorWorkspace exposes a narrow read interface conceptually equivalent
to:

```elisp
(superchat-editor-workspace-owner-session BUFFER)
```

For a live, valid owned buffer it returns the stable owner Session ID; for an
ordinary unowned buffer it returns nil.  A dead buffer or malformed ownership
metadata is rejected.  Callers do not inspect the private marker variables.
The query creates no state and does not widen close authority.

### 2b. Roll back an unaccepted host acquisition

The EditorWorkspace exposes one narrow public rollback operation conceptually
equivalent to:

```elisp
(superchat-editor-workspace-rollback-acquisition
 SESSION-ID ROLE BUFFER NEWLY-ACQUIRED)
```

Its arguments are the exact fields returned by the acquisition call in Action
Contract 2; there is no opaque lease or attestation object.

**Preconditions:** allocation and rollback belong to the same producer
invocation; Runtime has not accepted a Run using the buffer.  The producer must
not infer `newly-acquired` from a lookup race or buffer name.

**Effect:** when `newly-acquired` is false, return without closing anything.
When true, revalidate the exact live buffer's owner Session and role, then use
ordinary buffer close semantics on that buffer only.  Do not cancel Runs,
close other Session buffers, close tabs, or delete durable state.

**Postconditions:** a released or already-dead fresh buffer ends the rollback
successfully.  A reused buffer remains live.  Repeated rollback is harmless.

**Failure:** an identity mismatch is rejected without touching the candidate.
If a normal kill query, hook `error`, or hook `quit` leaves the fresh buffer
live, publish the Session EditorWorkspace as `retained`, keep the exact buffer
discoverable, and preserve the ordinary public Session-close retry path.
Rollback failure must not authorize direct or name-derived killing.

### 3. Close a Session EditorWorkspace

There is one programmatic close authority, conceptually:

```elisp
(superchat-session-workspace-close SESSION-ID)
```

and one public interactive entry point:

```text
M-x superchat-close-session-workspace
```

Tab-close interception and the manual command must invoke the same close
authority.  They may adapt confirmation and user messages, but may not
implement separate closure rules.

**Preconditions:** a stable Session ID is supplied.  Closing an already closed
or unmaterialized workspace is an idempotent success.

**Active-Run discovery:** use Runtime's public status projection to find
nonterminal Runs whose Session ID exactly matches the target.  UI code must not
inspect Runtime's private registry.

**No active Runs:** freeze new allocation, close every currently live owned
buffer, and close the associated tab only after no owned survivor remains.
Borrowed buffers are untouched.

**Active Runs:** present one confirmation explaining that all active Runs in
this Session will be cancelled before its owned buffers are closed.

- If confirmed: freeze allocation, call Runtime cancellation for each active
  Run, then close owned buffers and finally close the tab.
- If rejected: cancel nothing, change no buffer, preserve the tab, veto the
  initiating tab close, and show an actionable message naming
  `M-x superchat-close-session-workspace` as the manual path.

The manual command follows the same rule and therefore re-prompts while active
Runs remain.

**Closure failure:** if any owned buffer remains live after a normal close
attempt, set state to `retained`, keep the tab open, expose the survivors in
the ibuffer group, and report retry/manual inspection actions.  Do not force
kill a buffer by bypassing its queries.  Retrying the command re-enumerates the
live buffer list; it does not trust a cached failure list.

**Concurrency:** once closure begins, no new owned buffer may be allocated for
the Session.  Cancellation callbacks and workflow handoff must observe that
freeze.  A new Session View cannot open over a `closing` or `retained`
projection; retained resources must first be resolved.

### 4. Tab adapter

An interactive Session may have one tab whose parameters contain the stable
Session ID.  The adapter uses `tab-bar-tab-pre-close-functions` and/or
`tab-bar-tab-prevent-close-functions` to translate a native tab-close gesture
into Action Contract 3.

Closing the tab means “close this Session's live EditorWorkspace,” not “delete
the Session.”  A rejected confirmation vetoes the native close.  A retained
workspace keeps its tab present.  Headless Sessions do not require a tab.

### 5. Transient project adapter

When an authoritative root exists, expose an in-memory project object of the
shape supported by built-in `project.el`, such as `(transient . ROOT)`, and use
explicit project APIs.

The adapter must not call interactive discovery that prompts for or remembers
a project, must not call `project-remember-project`, and must not change
`project-list-file`.  `project-buffers` may aid discovery/presentation but may
not expand the owned-buffer close set.

### 6. ibuffer adapter

Provide a Session-labelled ibuffer group for live buffers carrying
EditorWorkspace metadata.  The group is recalculated from the live buffer
list.  It must visibly retain failed-close survivors.

The first delivery groups definitely associated Superchat buffers.  It does
not claim ownership of every file buffer under the transient project root,
because such buffers may be shared by Sessions or opened independently.

### 7. Runtime submission

Request producers obtain an exact Session-owned host buffer and pass that
buffer to Runtime.  Runtime verifies liveness and uses the exact object; it
does not recreate it from a buffer name and does not kill it at terminal
delivery.

For Chat submission, the producer-approved Session ID and exact View object
are immutable across Extension composition.  The shared submission seam
validates the pair before composition, rejects a callback that changes either
identity before activation audit, validates the final pair, records accepted
activation, and then submits it.  Extension callbacks may contribute Request
content but are not Session/buffer routing authority.  Rejection has zero
Extension Tape, Runtime, or provider effects.

Every Run with the same exact Session ID uses that Session's EditorWorkspace.
A Subagent or compactor with a distinct child Session ID uses its own
EditorWorkspace; shared Team or Artifact Workspace identity does not merge the
editor scopes.  A buffer owned by Session A must not be used as the execution
host of a Run in Session B, because closing A could otherwise invalidate B.

Run completion, cancellation, rejection, or delivery failure does not close
the EditorWorkspace merely as a Runtime terminal effect.  Session
EditorWorkspace closure remains the only owner-driven close boundary.  An
operation-scoped producer such as Compact may explicitly invoke that boundary
after handling its child Session's terminal result or a failed submission.

Skills semantic matching is also such an operation-scoped producer and closes
its child EditorWorkspace exactly once through that boundary across every
terminal or synchronous rejection path.  Subagent is not: after Runtime
acceptance its child host survives every terminal and callback outcome for the
child Session lifetime.  Only a pre-acceptance synchronous rejection may use
Action Contract 2b, and only for the host newly acquired by that invocation.

The three blocking adapters borrow their exact invocation/source buffer.  They
reuse a valid public owner Session when one exists, otherwise use an explicit
adapter-call Session without changing the buffer's markers.  Public owner
query and validation decide malformed, inherited, and mismatch cases; adapter
code does not interpret ownership metadata.

## Seams and verification

Verification must be performed through the real module interfaces and built-in
Emacs mechanisms.  Do not create a second production ownership implementation
solely for tests.

### Core state and ownership tests

- Same Session ID resolves to one live EditorWorkspace; different Session IDs
  do not share owned buffers.
- Owned-buffer enumeration scans live buffers and exact local metadata rather
  than a cached ledger.
- Dead owned buffers disappear naturally; unmarked and borrowed buffers are
  never selected for closure.
- Role lookup reuses a live protocol host and replaces a dead one.
- Allocation is rejected in `closing` and `retained`.
- Repeated close of `closed`/unmaterialized state is successful and harmless.

### Close behavior tests

- No-active-Run close kills only owned buffers and closes the tab last.
- Active-Run confirmation accepts: every matching active Run is cancelled;
  other Sessions' Runs are untouched; then owned buffers and the tab close.
- Active-Run confirmation rejects: no Run, buffer, or tab changes; tab closure
  is vetoed; the message names the manual command.
- A buffer kill query that refuses closure yields `retained`; the buffer and
  tab remain; ibuffer discovery includes the survivor; retry succeeds after
  the query permits closure.
- Cancellation callbacks cannot allocate a new owned buffer after closure has
  frozen the scope.
- The manual command and tab-close hook exercise the same close authority.

### Native adapter tests

- A real batch Emacs tab carries the Session ID parameter; closing it follows
  the confirmation contract.  Tests must not assume native tab close kills
  displayed buffers.
- The transient project exposes the authoritative root without modifying a
  temporary sentinel `project-list-file`.
- `project-buffers` and ibuffer grouping do not widen the owned close set.
- A headless Session can allocate, enumerate, and close owned buffers without
  a tab or selected window.

### Runtime and structural tests

- New Requests pass an exact live buffer and Runtime never kills it on success,
  cancellation, rejection, or error.
- Terminal Runtime quiescence clears or cancels the deadline timer, transport
  timer owner, provider request, delivery/stream closures, and pause
  continuation independently of Workspace close success.
- Workflow steps in one Session reuse the designated Session host role where
  appropriate rather than producing one ephemeral buffer per Run.
- No shipped producer remains for legacy `:ephemeral-buffer`,
  `:host-authority-mode`, or editor HostLease Cleanup before final retirement;
  plugin, experiment, and external-documentation results are reported
  separately.
- No mock or adapter infers kill authority from a buffer name, project root,
  tab layout, or ibuffer group.
- A Run whose Session differs from an owned buffer's Session is rejected or
  given its own correct Session host; Compact and Subagent paths cover this.
- Extension callbacks cannot change a Chat producer's captured Session ID or
  exact View, even by replacing both with another otherwise valid owned pair.
- Marker representation is referenced only inside EditorWorkspace; callers
  use its public owner query or validation interfaces.
- Every materialized compaction child EditorWorkspace is closed through the
  public close authority after terminal processing or failed submission;
  repeated compactions leave no accumulating hidden protocol hosts.
- Every asynchronous Skills match makes exactly one public close attempt after
  terminal delivery or synchronous submission rejection.  Repeated matches do
  not accumulate hidden live hosts; callback `error`/`quit`, provider
  error/cancel, and a synchronously propagated post-acceptance condition cannot
  skip or duplicate that close.  A refusal remains `retained`, identifies the
  exact child Session in an actionable diagnostic, and succeeds through a
  later public retry without repeating matching or Runtime terminal work.
- Subagent success, cancellation, provider error, and callback `error`/`quit`
  preserve its accepted child Session host.  Pre-acceptance rejection rolls
  back a host only when the same allocation transaction reports it fresh;
  rejection after role reuse preserves the exact pre-existing host.  A
  rollback refusal remains retained and retryable rather than being force
  killed or forgotten.
- Org Babel, Magit, and Rewrite cover ordinary borrowed, exactly owned,
  owner-mismatch, malformed-local, and inherited/default metadata cases.
  Tests prove marker state is unchanged and Rewrite submits the source buffer,
  never its later preview buffer.
- A non-buffer-local owned marker never enters owner queries, enumeration,
  role lookup, ibuffer grouping, or close planning; partial metadata is
  malformed rather than ownership.
- Synchronous Runtime `error` or `quit` after Run publication seals and
  delivers the Run before propagating the original condition; Compact clears
  its parent in-flight state on every failed submission exit.
- Runtime still releases transport/timer/callback execution handles.
- Kernel-require and eager-integration gates continue to pass; the new module
  is middle tier and is never required by a kernel file.
- The canonical ERT runner remains the acceptance command:

  ```sh
  emacs -Q --batch -L . -l test/run-tests.el
  ```

At capture time the most recently reported canonical baseline is 1151 total,
1134 passed, 0 unexpected, 17 expected.  `check` must report the then-current
numbers rather than copying this baseline as a result.

## Compatibility and migration

This is a deliberate breaking model migration, implemented in ordered slices
so existing production paths do not lose cleanup behavior halfway through.

1. Add the Session EditorWorkspace authority, metadata, close lifecycle, and
   native adapters without changing existing Runtime producers.
2. Add tab-close interception and the public manual close command.  Existing
   Runs continue to use their legacy HostLease until their producer migrates.
3. Extract unconditional private Run quiescence from the combined Cleanup
   state machine.  Legacy HostLease release may remain temporarily, but no
   editor ownership error may bypass execution-handle release.
4. Migrate every shipped Request producer: Dispatcher/Agent Loop, Workflow,
   Subagent, Compact, Skills, Org Babel, Magit, Rewrite, and bundled Telega.
   Each must explicitly use the correct Session buffer contract; a producer is
   not considered migrated merely because it never emitted the old flag.
   During this phase legacy Request keys and Cleanup entry state remain
   accepted for old callers, and plugin documentation describes the boundary.
   Request identity is captured before Extension composition and cannot be
   retargeted by a contribution callback.  Operation-scoped compaction child
   EditorWorkspaces are explicitly closed by Compact through the public close
   authority after terminal processing or failed submission.  Operation-
   scoped Skills matches do the same exactly once.  Accepted Subagents retain
   their child Session host; only a fresh acquisition rejected before Runtime
   acceptance is rolled back through the exact public acquisition boundary.
5. After shipped callers, tests, examples, and documentation are classified,
   retire HostLease/Cleanup's editor surface, legacy `:ephemeral-buffer`,
   `:host-authority-mode`, editor Cleanup retry/abandon, lazy legacy lease
   reconstruction, and name-based identity fallback.  Retain Runtime's
   quiescence seam and independently justified atomic Run publication.

Do not transfer ownership of a live legacy HostLease buffer into an
EditorWorkspace mid-Run.  A legacy Run finishes under the legacy cleanup path;
new workspace-aware Requests use the new path.  Because the live Runtime
registry is process-local, an Emacs restart is a valid final boundary for
removing hot-loaded legacy entries.  There is no Tape or SQLite schema
migration.

During compatibility, every accepted Request takes exactly one ownership path:
a legacy ephemeral host has a HostLease and no EditorWorkspace owned marker; a
new Request has an EditorWorkspace-owned buffer and no HostLease.  Mixed or
double-owned Requests are rejected before provider effects.

Existing durable Session IDs, Tape rows, Artifact Workspace bindings, and
Workspace roots are reused.  Closing a Session EditorWorkspace performs no
durable deletion.

`docs/plugin-guide.md` currently documents `:buffer-name /
:ephemeral-buffer` as a public plugin contract.  Final retirement must update
the example and field table and state the compatibility/release boundary.
Internal repository zero-callers are insufficient evidence about third-party
callers.

### Revision 5 compatibility, risks, and handoff

Revision 5 is a **compatible refinement** inside the already approved breaking
migration.  Existing public EditorWorkspace allocation, owner query,
validation, close, state, and enumeration behavior remains valid.  The
existing buffer-only allocation result remains unchanged; callers use the new
plain acquisition-value API only when they own a pre-acceptance rollback
window.  The exact rollback operation is additive and cannot replace ordinary
Session close after Runtime acceptance.

Primary risks for implementation and check are:

- treating any synchronous Runtime signal as pre-acceptance, which would close
  a host after a Run was already published and terminally delivered;
- setting the close/rollback guard after invoking callbacks, allowing
  synchronous reentry or callback `quit` to duplicate or skip release;
- inferring freshness with a producer-side lookup instead of receiving the
  fact atomically from EditorWorkspace, which can misclassify role reuse;
- using whole-Session close for Subagent acquisition rollback, which can close
  resources not acquired by that invocation;
- allowing a close or rollback condition to overwrite the primary provider or
  callback condition, or silently losing a retained survivor;
- tests that retain the hidden Session ID from a mocked Request and therefore
  appear green without proving production discoverability or non-accumulation.

Revision 5 specifically does **not** introduce a durable Session registry,
automatic deletion of Session/Tape facts, an attestation or lease token, a
second ownership classifier, a Runtime Request validator callback, a
producer-local marker reader, buffer-name cleanup, automatic close of accepted
Subagent Sessions, or any change to Runtime/transport lifecycle semantics.

Capture has revised the existing Slice07 contract and owned file list to this
confirmed Revision 5 boundary.  The next action is `arrange`, which must turn
the now-`stale` Slice07 into a corrected ready handoff before any further
`do`.  No code or test correction is claimed by this capture revision.

Historical `docs/adr/0002-lazy-buffer-derived-workspaces.md` is `proposed` and
excluded from enacted Kernel semantics.  Its useful constraints remain:
buffer identity is a real resource, direct chats may materialize lazily,
targets are explicit, and authority is separate from presentation.  Its
rejection of a one-Session/one-Workspace relationship is superseded only for
the newly distinguished **Session EditorWorkspace**, not for the durable
Artifact Workspace.  After implementation is verified, `learn` must record
this distinction by superseding/rejecting ADR 0002 or by adding a replacement
ADR; capture does not edit ADRs.

## Out of scope

- Changing durable Artifact Workspace identity, revision, or projection
  semantics.
- Deleting Session, Tape, Anchor, Run, Memory, Claim, or Artifact data when an
  EditorWorkspace closes.
- Auto-killing borrowed, user-created, file, or project buffers.
- Treating every buffer below a project root as Session-owned.
- Supporting one owned buffer with multiple Session owners.
- Persisting a duplicate owned-buffer inventory in SQLite or Tape.
- Automatically remembering transient projects in the user's project list.
- Manufacturing a temporary project root for a direct chat with no
  authoritative root.
- Force-killing buffers past their normal kill queries.
- Defending against deliberately hostile arbitrary Emacs Lisp mutation.
- Moving Session editor-resource ownership into kernel Runtime.
- Changing `superchat-llm.el`, model transport, tool Authority, or the active
  MCP Authority integration specification.
- General file, subprocess, network, or external-resource lifecycle management.

## Issue map

`arrange` should turn these into independently checkable vertical slices.  It
may refine file lists but must preserve the authority and dependency order.

### S1 — Session EditorWorkspace authority

- Scope: new middle-tier authority and focused ERT tests.
- Delivers: lazy identity, owned-buffer metadata, role-based buffer lookup,
  live enumeration, `open/closing/retained/closed` state, programmatic close.
- Does not yet migrate Runtime callers.
- Acceptance: ownership and state tests pass; no kernel require edge is added.

### S2 — Native close and recovery UX

- Depends on: S1.
- Scope: tab parameter/hook, confirmation behavior, public manual command,
  retained messaging and retry.
- Delivers: confirmed cancel-and-close; rejected close veto with manual action;
  retained tab/survivor behavior.
- Acceptance: real tab tests and all close-behavior tests pass.

### S3 — Project and ibuffer projections

- Depends on: S1; may proceed independently of most S2 implementation.
- Scope: in-memory transient project adapter and Session ibuffer grouping.
- Delivers: root-aware discovery without project-list persistence and visible
  retained survivors.
- Acceptance: project-list sentinel remains unchanged; adapter output never
  widens the owned close set.

### S4 — Separate Run quiescence from editor release

- Depends on: S1 so focused tests can express the destination boundary; no UI
  adapter dependency.
- Scope: Runtime terminal finalization and focused lifecycle tests.
- Delivers: one unconditional, idempotent private quiescence seam; legacy
  editor release cannot gate it.
- Acceptance: every terminal, error, and foreign-ownership path severs
  execution handles exactly once while borrowed buffers remain live.

### S5 — Migrate all Request producers and public guidance

- Depends on: S1 and S4.
- Scope: Dispatcher/Agent Loop, Workflow, Subagent, Compact, Skills, the three
  synchronous adapters, bundled Telega, EditorWorkspace's minimal atomic
  acquisition/rollback boundary, producer and authority tests, and plugin
  guide.  The Slice07 correction specifically authorizes
  `superchat-editor-workspace.el`, `superchat-skills.el`,
  `superchat-subagent.el`, `ob-superchat.el`, `superchat-magit.el`, and
  `superchat-rewrite.el`, with their directly corresponding tests; it does not
  authorize `superchat-runtime.el` or `superchat-llm.el` changes.
- Delivers: exact correct-Session buffers, including Session-owned protocol
  hosts for headless use; no shipped producer creates or requests per-Run
  editor ownership; Chat identity survives Extension composition; marker
  interpretation stays inside EditorWorkspace; operation-scoped compaction
  child hosts are closed through the shared authority.
- Acceptance: shipped, experimental, and external-compatibility search results
  are reported separately; Workflow keeps Session identity after View death;
  child Sessions never execute in a parent-owned host; repeated compactions do
  not accumulate hidden live buffers; repeated Skills matches do not
  accumulate hidden hosts; accepted Subagent hosts follow Session lifetime,
  while pre-acceptance rejection rolls back only a fresh acquisition; all
  Skills/Subagent terminal and callback branches and every blocking-adapter
  ownership/preview branch in the verification matrix pass.

### S6 — Retire legacy HostLease/Cleanup editor contracts

- Depends on: S2, S5, and a passing compatibility-boundary check.
- Scope: Runtime legacy structs, fields, functions and adapters;
  representation-only tests; structural gates; stale KERNEL references for
  later `learn`.
- Delivers: removal of HostLease, editor Cleanup state, retry/abandon APIs,
  name-based fallback, and old Request fields; Runtime retains quiescence and
  Run publication.
- Acceptance: repository search finds no shipped use of retired keys or
  functions, legacy dual ownership is impossible, replacement behavioral tests
  and canonical suite pass, and documentation names the breaking boundary.

## Revision notes

### Revision 5 — confirmed Slice07 child-lifetime correction, 2026-09-04

- Classified asynchronous Skills semantic matching as an operation-scoped
  child Session with no tab or returned Session ID, and assigned exactly-once
  public close responsibility to the Skills producer across terminal,
  callback, provider, cancel, and synchronous rejection paths.
- Kept accepted Subagents as trackable child Sessions whose owned host follows
  Session lifetime, while requiring pre-acceptance rejection to roll back only
  the exact host freshly acquired by that invocation and never a reused host.
- Found the existing public allocation/close pair compatible but insufficient
  for exact acquisition rollback; authorized one separate additive acquisition
  value API and one exact idempotent rollback operation in EditorWorkspace,
  leaving the old buffer-only API unchanged and adding no token, new registry,
  second classifier, name authority, or Runtime dependency.
- Defined the blocking adapters' ordinary exact invocation/source-buffer
  contract: owned buffers reuse their owner Session; mismatch and malformed
  metadata fail through public authority; inherited metadata degrades to
  ordinary borrowing; markers remain unchanged; Rewrite preview identity stays
  separate.
- Expanded verification to the full Skills/Subagent lifecycle matrix,
  repeated-operation accumulation, rollback reuse/refusal, and blocking-
  adapter owned/mismatch/malformed/inherited/preview cases.  This revision is a
  compatible refinement inside the already approved breaking migration and
  records decisions only; it does not claim the Slice07 correction delivered.

### Revision 4 — confirmed Slice06 effect-order correction, 2026-09-04

- Required both ownership and Session markers to be exact buffer-local
  metadata before they can grant close authority.
- Moved Chat identity rejection ahead of Extension activation Tape, making
  every retarget rejection zero-effect across Extension, Runtime, and provider.
- Brought Runtime's synchronous `quit` path under the existing atomic
  publication/terminal contract and required Compact to clear parent
  in-flight state on every failed submission exit.
- Expanded Slice06 correction Scope to Runtime, Extension, and their focused
  tests; no new lifecycle or authority was introduced.

### Revision 3 — confirmed Slice06 correction, 2026-09-04

- Froze Chat producer Session/buffer identity across Extension composition;
  callbacks may enrich Requests but cannot become a second routing authority.
- Added one narrow EditorWorkspace owner-Session query so marker
  representation and malformed-marker handling remain inside the owning
  module.
- Defined compaction child Sessions as operation-scoped live editor scopes:
  Compact explicitly requests shared-authority closure after terminal
  processing or failed submission, while durable child Session/Tape facts
  remain intact.
- Preserved Runtime's rule that terminal completion itself never closes an
  EditorWorkspace and rejected direct buffer killing or a parent/child
  workspace registry.
- Marked Slice06 stale for corrected Scope, implementation, and verification.

### Revision 2 — confirmed clarification, 2026-09-03

- Added a full-tree simplification audit instead of assuming HostLease removal
  was localized to Runtime and Workflow.
- Identified the core structural defect: mandatory Run quiescence and optional
  editor-buffer release share one Cleanup gate, allowing ownership anomalies
  to bypass execution-handle release.
- Classified duplicate buffer representations, unused HostLease ID, legacy
  reconstruction, producer-side ephemeral policy, test mocks, public plugin
  documentation, experiments, and hot-reload uncertainty.
- Expanded migration coverage to all shipped Runtime producers and separated
  quiescence extraction, producer migration, and legacy retirement.
- Clarified that distinct Subagent or compaction Session IDs imply distinct
  EditorWorkspaces; Team or Artifact Workspace sharing does not merge them.
- Recorded the invariants that old and new ownership never overlap and that
  Runtime status, tab, project, ibuffer, and tests cannot become shadow
  ownership authorities.

### Revision 1 — confirmed, 2026-09-03

- Replaced the per-Run editor HostLease/Cleanup direction with a Session-scoped
  EditorWorkspace model.
- Confirmed one logical EditorWorkspace per Session, with optional tab and
  lazy live materialization.
- Confirmed that active Runs require one user confirmation before cancellation
  and close.
- Confirmed that rejecting confirmation vetoes tab closure and exposes
  `M-x superchat-close-session-workspace` as the manual path.
- Defined `retained` as the observable recovery state for buffers that refuse
  normal closure; removed `unresolved` from the editor ownership model.
- Preserved Runtime, Artifact Workspace, Tape, and kernel authority boundaries.
- Recorded the breaking migration and the historical ADR 0002 conflict for
  resolution during `learn` after verified implementation.

## Context references

- `AGENTS.md`: module tiers, authority boundaries, canonical tests, and dirty
  worktree constraints.
- `KERNEL.md`: current Session, Run, Workspace/Artifact, HostLease, Cleanup,
  Submit Request, and architecture-map contracts.  These remain current until
  implementation is checked and learned.
- `EVIDENCE.md`: `E-20260903-001` and `E-20260903-002`, recording why the
  current HostLease/Cleanup and atomic acquisition/publication machinery was
  introduced and which counterexamples it protects.
- `CONTEXT.md`: enacted domain vocabulary for Session, Run, and Workspace.
- `docs/plugin-guide.md`: published `:buffer-name / :ephemeral-buffer` Request
  guidance that requires an explicit breaking migration.
- `docs/spec-agents/single-authority.md`: one authoritative owner per lifecycle
  decision.
- `docs/adr/0002-lazy-buffer-derived-workspaces.md`: proposed historical design
  whose useful lazy/resource constraints are retained and whose relationship
  decision requires later supersession.
- `.specs/mcp-authority-integration/SPEC.md`: active confirmed specification;
  this SPEC deliberately excludes its `superchat-llm.el`, MCP, tool Authority,
  and synthesis scope.
