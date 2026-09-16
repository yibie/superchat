# Closure Projection Contract v0

> **Status: historical.** This is the Closure v0 contract for the 2026-08 implementation. Closure, its terminal guard, shadow observation interfaces and Service are removed by the [M3a decision](../decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md). The rules and examples below are an archival design, not current prerequisites or instructions to restore a removed module.
>
> **Current contract:** [request lifecycle](../protocols/request-lifecycle.md). Runtime records open work; downstream consumers read the recorded Run terminal. The original v0 semantics and safety errata remain below as historical evidence.

This contract defines how Superchat derives auditable Runtime judgments from
immutable Tape history.  It deliberately does not define a general Agent world
model, knowledge graph, belief system, progress score, or planning theory.

The central rule is:

> Superchat does not store Agent state as mutable truth.  It interprets
> immutable execution history under a versioned semantic environment and
> derives Runtime judgments with evidence.

The first implementation MUST consume existing events.  It MUST NOT require
new event types, a Claim system, an entropy score, or an ontology expansion.

## 1. Normative language

The words **MUST**, **MUST NOT**, **SHOULD**, **SHOULD NOT**, and **MAY** are
normative.

This document distinguishes:

- **target semantics** — the contract a conforming implementation must obey;
- **current evidence** — facts that the existing Tape can already prove;
- **known gaps** — facts the current event vocabulary cannot prove without
  inventing meaning.

A known gap MUST yield `open` or `unknown`; it MUST NOT be guessed as `proved`.

## 2. Scope and non-goals

### 2.1 In scope

Contract v0 covers:

- Run and Turn lifecycle;
- intent and response observations;
- Tool request, Approval, execution, completion, and failure;
- Effect accounting at Tool-execution granularity;
- `completed`, `failed`, `timed-out`, and `cancelled` terminal kinds;
- explicit durable recovery of the process-local `audit-incomplete` status;
- Workspace Artifact identity and revision facts already present in Tape;
- deterministic replay, checkpoint continuation, provenance, soundness, and
  unknown safety;
- the separation between Closure judgment and Authority decision.

### 2.2 Out of scope

Contract v0 does not define:

- Claim or Evidence semantics;
- hallucination detection or Claim promotion;
- Personal, Capability, or Query Memory;
- a generic Plugin transition language;
- Artifact acceptance;
- automatic obligation transfer or waiver;
- probabilistic diagnostics, convergence, entropy, health, or confidence
  scores;
- Team- or Workflow-specific closure beyond identities already carried by
  events;
- deletion or compaction of canonical Tape history.

These may later be separate projections or contract extensions.  They MUST NOT
be smuggled into v0 through unversioned payload conventions.

## 3. Semantic model

### 3.1 Runtime world

The logical Runtime world is:

\[
W=(T,\Gamma)
\]

where:

- \(T=e_1e_2\ldots e_n\) is an ordered Tape trace;
- \(\Gamma\) is the versioned semantic environment used to interpret it.

The same Tape under two semantic environments may yield different judgments:

\[
(T,\Gamma_1)\neq(T,\Gamma_2)
\]

Therefore every durable Projection checkpoint MUST identify the semantics
under which it was produced.

### 3.2 Declarative judgment

The normative form of a Runtime question is:

\[
\Gamma;T\vdash_Q J
\]

where:

- \(Q\) identifies the question being asked;
- \(J\) is a judgment and its witness.

Examples include:

```text
trace T is well formed
effect execution x is accounted
run r is ready for terminal kind completed
run r has a durable terminal seal
turn t has a durable terminal seal
```

### 3.3 Executable interpretation

A conforming executable implementation may compute a shared Operational
Boundary Model first:

\[
\llbracket T\rrbracket_\Gamma=B
\]

and then answer questions from it:

\[
Judge_{Q,\Gamma}(B)=(J,w)
\]

where \(w\) is a machine-checkable witness.

The Boundary Model is an implementation of the semantics, not a new fact
store.  It MAY be cached or checkpointed, but it MUST be reconstructible from
\((T,\Gamma)\).

### 3.4 Behavioral equivalence

For the v0 question set \(Q_0\), two traces are behaviorally equivalent when
they permit the same v0 observations and matching legal continuations:

\[
T_1\sim_{Q_0,\Gamma}T_2
\]

requires at least:

\[
Obs_{Q_0,\Gamma}(T_1)=Obs_{Q_0,\Gamma}(T_2)
\]

and, for every event \(e\) legal after both traces:

\[
T_1e\sim_{Q_0,\Gamma}T_2e
\]

The Boundary Model is sufficient only if equal Boundary Models imply this
behavioral equivalence.  Field similarity is not sufficient.

## 4. Semantic layers and ownership

The Runtime semantic loop has three distinct responsibilities:

```text
Tape T
  │
  │ interpret under Γ
  ▼
Operational Boundary B
  │
  │ evaluate Q
  ▼
Hard Runtime Judgment J
  │
  │ apply Authority policy ΓA
  ▼
Authority Decision D
  │
  │ append decision / effect / receipt event
  ▼
Tape T'
```

### 4.1 Tape owns asserted facts

Tape events are immutable, typed assertions with identity, provenance, and
causal metadata.  An interpreter or decision MUST NOT rewrite an earlier Tape
event.

Events are not semantically empty, but they MUST contain local observations,
not derived closure consequences.  For example:

```elisp
(:type "tool.completed"
 :payload (:execution-id "exec-1"
           :request-hash "..."
           :status "succeeded"))
```

is a local Tool observation.  It MUST NOT also claim arbitrary downstream
operations such as `:closes (...)`.

### 4.2 Closure semantics owns derived meaning

Closure semantics interprets event type, payload, causal position, and the
current Boundary under \(\Gamma\).  It determines which obligations or
Effect executions remain unaccounted.

An LLM, Surface, Tool, or Plugin MUST NOT directly mark a Hard Closure node as
accounted.

### 4.3 Authority owns normative permission

Authority decides whether an otherwise meaningful action is permitted under
the current Effect, Resource, Scope, attendance, and policy contract.

The effective action gate is:

\[
Admissible(a)=SchemaValid(a)\land ClosureReady(a)\land AuthorityAllows(a)
\]

`AuthorityAllows` MUST NOT repair an `open` or `unknown` Closure judgment.
Closure MUST NOT manufacture Authority.  Soft diagnostics MAY request more
review or tighten execution, but MUST NOT loosen either gate.

An Authority decision becomes historical fact only by appending a new Tape
event.

## 5. Semantic environment \(\Gamma_0\)

The v0 semantic environment contains logical identities for:

```text
event schema set
Closure contract version
Projection implementation version
Authority policy contract
Topology policy
trusted event-producer contract
```

The logical environment is immutable for one interpretation.  A caller MAY
choose a different environment and rebuild, but MUST NOT silently combine a
checkpoint produced under one environment with suffix events interpreted
under another.

### 5.1 Current representation

Existing Tape envelopes already contain `schema`, and Tool/Approval payloads
contain `policy-version`.  Contract v0 does not require adding a complete
semantic environment snapshot to every event.

The reference interpreter identifies its static environment in every witness:

```elisp
(:closure-contract "closure-v0"
 :event-schema 1
 :authority-policy-contract "authority-v1")
```

`authority-v1` names the supported Authority contract family; named current
policies use versions such as `authority-v1/default`.  Missing versions,
other contract families, and a policy change within one Effect/Approval chain
are not silently interpreted as equivalent.

An implementation SHOULD identify \(\Gamma_0\) once per replay or checkpoint.
A future content-addressed `contract-id` may replace several version fields,
but that storage change is not part of v0.

### 5.2 Version mismatch

If an event schema or required policy contract is unknown, the affected Hard
judgment MUST be `unknown`.  The interpreter MUST NOT:

- ignore the event as harmless without a registered rule;
- reinterpret it using the nearest known version;
- treat a missing payload as an empty payload;
- upgrade `unknown` to `open` or `proved` based on event name similarity.

## 6. Tape trace model

### 6.1 Required envelope facts

Every event interpreted by v0 requires:

```text
event type
event ID
positive schema version
session ID
recorded order / storage cursor
```

Run-scoped events additionally require the identities used by their rule:

```text
turn ID
run ID
actor ID when attribution is relevant
causation ID when the producer supplies one
parent Run ID when delegated
workspace ID when scoped to a Workspace
```

Tool lifecycle events additionally require:

```text
execution ID
request hash
tool name
status or decision where applicable
resolved Effect envelope on tool.requested
policy version where applicable
```

### 6.2 Ordering and causation

The storage cursor gives a deterministic replay order.  `causation-id` gives
logical ancestry.

A causation reference MUST resolve to:

- an earlier event in the selected trace; or
- an event summarized by the supplied checkpoint boundary.

Missing causal ancestors make the affected chain `unknown`; they MUST NOT be
invented from adjacency.

Events with distinct identities and no causal or shared-node dependency MAY
commute.  Events touching the same Run, execution, Approval, or Artifact MUST
obey their transition rules.

### 6.3 Event understanding and scope

Event classification has two independent axes.

The **understanding axis** is:

- **relevant** — a registered rule changes or validates the Boundary;
- **known irrelevant** — \(\Gamma_0\) explicitly declares that the event has
  no effect on \(Q_0\);
- **unknown** — no rule establishes either conclusion.

The **scope axis** is:

- **in scope** — the event belongs to the Session, Run, Turn, execution,
  Approval, or Workspace selected by \(Q\);
- **out of scope** — understood envelope identity proves that the event belongs
  to a different selected scope.

An out-of-scope event has no effect on the current judgment even when its
event semantics are unknown.  The interpreter MUST establish this from
recognized envelope identity; absence of scope metadata is not proof that an
event is out of scope.

An unknown, in-scope event makes dependent Hard judgments `unknown`.  A known
irrelevant event remains harmless only for the question set and semantic
contract that classified it.

Under Γ0, the following current event families are known irrelevant to Q0
Run/Turn closure: Session configuration (`capability.activated`,
`topology.changed`, `authority.policy.selected`, `workspace.bound`,
`workspace.unbound`), `attachment.added`, `anchor.created`,
`extension.activated`, and the Memory events `memory.captured`,
`memory.corrected`, `memory.raw.added`, `memory.rejected`, and
`memory.reviewed`, plus `claim.recorded` and `feedback.recorded`.  Their facts
remain available to their own Projection families; Closure does not erase or
reinterpret them.
Workspace Artifact events are interpreted separately but do not change
`Ready_k` in v0.

## 7. Operational Boundary Model \(B_0\)

The Boundary Model is a typed graph:

\[
B=(V,R)
\]

It represents only facts from the past that still constrain v0 Runtime
questions.  It is not a general representation of the external world.

### 7.1 Node types

Contract v0 uses five logical node types.

#### RunBoundary

One node per `run-id`.  It records:

```text
session / turn / run identity
actor and parent Run identity when present
introducing intent and run.started events
observed response outcome
terminal seal, if any
unaccounted owned nodes
```

#### Obligation

A derived, typed requirement owned by a Run or Turn.  v0 requires only
lifecycle obligations needed to explain:

```text
Run outcome
Turn terminal seal
Tool execution accounting
Approval resolution
```

An Obligation is not an arbitrary model-generated task.

#### Effect

One logical Effect-execution node per `execution-id`.  The node carries the
resolved Effect envelope recorded by `tool.requested` and the request hash.
Multiple effect descriptors in one request remain one v0 accounting unit;
v0 does not invent per-effect IDs absent from Tape.

#### Approval

One node per Approval request, keyed by the request hash and execution ID.  It
records the request and the matching allow/deny decision.

#### Artifact

One node per `(workspace-id, artifact-id)`, with URI, role, and revision facts
from existing Workspace events.

Existing Workspace events are Session/Workspace scoped and do not carry a
uniform Run binding or acceptance event.  Therefore Artifact nodes are
auditable in v0 but MUST NOT block or prove Run terminal by themselves.

### 7.2 Relation types

The v0 relation vocabulary is deliberately small:

```text
owns       Run or Turn owns a Boundary node
requires   one node cannot be accounted before another judgment
blocks     an unresolved node prevents a named judgment
produces   an event or execution produced an observation or Artifact fact
```

`transfers` and semantic `supports`/`invalidates` relations are future
extensions.  No v0 reducer rule may invent them.

### 7.3 Required node metadata

Every future-relevant node MUST have:

```text
stable logical identity
type
scope
owner
introduced-by event ID
current disposition
witness event IDs for every disposition change
```

If required metadata is missing, the node is malformed.  Malformed structure
is not ordinary debt; it makes dependent judgments `unknown`.

### 7.4 Accounted predicate

`Accounted(node)` is the primary v0 predicate.  A node is accounted only when
its terminal disposition is supported by a rule and witness under
\(\Gamma_0\).

Possible semantic dispositions include:

```text
discharged
blocked
failed
cancelled
transferred
waived
```

Existing v0 events can prove only the dispositions named by their transition
rules.  Generic transfer and waiver are unsupported; encountering an alleged
transfer or waiver without a registered event rule yields `unknown`.

Completion does not require the global Boundary to be empty.  It requires
every node owned by the relevant Run boundary to be accounted:

\[
\forall n\in Boundary(r),\quad Accounted(n)
\]

## 8. Transition semantics \(\delta_{\Gamma_0}\)

The executable transition function is partial:

\[
\delta_{\Gamma_0}(B,e)\rightharpoonup B'
\]

An event with unsatisfied preconditions MUST NOT be accepted as an ordinary
state update.  It produces an invalid-trace witness or `unknown`, as specified
below.

### 8.1 Run and Turn events

| Event | Required precondition | Boundary consequence |
|---|---|---|
| `intent.received` | Valid Session, Turn, and Run identity | Introduce the RunBoundary in `accepted` form and its lifecycle obligations. |
| `run.started` | Matching prior intent; matching causation when present; no prior start or terminal seal | Mark execution started and record actor/capability facts. |
| `response.completed` | Matching started, nonterminal Run; recognized status | Record a response observation and its status. It is not by itself a Run terminal seal. |
| `run.completed` | `Ready_completed` proved for the prefix before this event | Account the Run outcome as `completed` and add the durable terminal seal. |
| `run.failed` | `Ready_failed` or `Ready_timed_out` proved for an ordinary outcome; or payload is exactly `status=audit-incomplete, audit-incomplete=true` with no prior terminal | Account the matching ordinary outcome, or add the exceptional audit terminal while preserving every residual Effect and Approval. |
| `run.cancelled` | `Ready_cancelled` proved for the prefix before this event | Account cancellation and add the durable terminal seal. |
| `turn.completed` | Matching owning Run has a valid terminal seal; status agrees | Account the Turn terminal obligation. |

Contract v0 assumes the current Runtime rule of one owning Run per accepted
Turn.  `parent-run-id` is preserved as identity but does not introduce Team or
Workflow join semantics.

### 8.2 Tool, Effect, and Approval events

| Event | Required precondition | Boundary consequence |
|---|---|---|
| `tool.requested` | Active Run; unique execution ID; valid request hash and Effect envelope; supported Authority policy contract | Introduce the Effect node in `requested` disposition and attach it to the Run. |
| `approval.requested` | Matching requested Effect whose recorded decision requires approval; matching Authority policy version | Introduce an Approval node; mark it as blocking the Effect execution. |
| `approval.decided` | Matching open Approval, request hash, and Authority policy version | Account the Approval as allow or deny. Allow removes the Approval block; deny does not authorize execution. |
| `tool.execution.started` | Matching Effect; Authority path resolved to allow; no previous start or terminal disposition | Mark the Effect `executing`. |
| `tool.completed` | Matching executing Effect and request hash | Account the Effect as `discharged`; record result fingerprint as an observation, not semantic truth. |
| `tool.failed` | Matching Effect and request hash; disposition compatible with the recognized status | Account it as `blocked`, `failed`, `cancelled`, or denied. `blocked`/`denied` are pre-execution refusals, `failed` requires `executing`, and approval cancellation requires a denied Approval before execution. |

An Approval `allow` does not itself execute or account an Effect.  A Tool
completion does not prove that the broader user goal succeeded.

### 8.3 Workspace Artifact events

| Event | Boundary consequence |
|---|---|
| `workspace.artifact.added` | Introduce or replace the identified Workspace Artifact fact. |
| `workspace.artifact.changed` | Update the matching Artifact revision, preserving provenance. |
| `workspace.artifact.removed` | Account removal of the Workspace Artifact fact. |

Because current Workspace events lack uniform `run-id` and acceptance
semantics, these rules do not change `Ready_k` for a Run.

## 9. Judgment set \(Q_0\)

The external semantic seam is one operation:

\[
Judge(\Gamma,T,Q)\rightarrow J
\]

Callers MUST NOT mutate the Boundary Graph.  The graph and reducer are
implementation details behind this judgment interface.

### 9.1 Judgment result

A closure question returns one of:

```text
proved    The requested property follows under Γ, with a witness.
open      The trace is understood, but named obligations remain unaccounted.
unknown   Required schema, provenance, contract, or semantic information is unavailable.
```

Trace well-formedness separately returns:

```text
valid
invalid
unknown
```

`open` is not the same as `unknown`.  An implementation MUST preserve the
distinction.

If trace validation returns `invalid`, Closure questions return `unknown`
with the validation failure in their witness.  Only `TraceWellFormed` exposes
`invalid` as a verdict; an invalid history cannot answer a lifecycle question.

### 9.2 Required v0 questions

#### TraceWellFormed

Determines whether the selected trace obeys required identity, causation,
ordering, and event-specific preconditions.

#### PendingEffects

Returns Effect nodes owned by the selected Run that are not accounted.

#### PendingApprovals

Returns Approval nodes without a valid matching decision.

#### ReadyTerminal

Answers `Ready_k(W,r)` for one requested terminal kind.

#### DurableRunTerminal

Determines whether a valid terminal event seals the Run under the matching
pre-terminal proof.

#### DurableTurnTerminal

Determines whether `turn.completed` validly seals the Turn after its owning
Run terminal event.

#### ResidualBoundary

Returns all owned nodes that are open, malformed, unknown, or accounted only
by a residual disposition.

## 10. Terminal semantics

Terminal is a proved result, not an instruction to set a mutable status.

### 10.1 Pre-terminal readiness

For a Run \(r\), all `Ready_k` predicates require:

```text
the relevant trace is valid
run.started is present
all owned Approvals are accounted
all owned Effects are accounted
no relevant event or required payload is unknown
no earlier terminal seal exists
```

Additional requirements are:

| Kind | Additional pre-terminal evidence |
|---|---|
| `completed` | Matching `response.completed` with status `completed`. |
| `failed` | Matching `response.completed` with status `failed`. |
| `timed-out` | Matching `response.completed` with status `timed-out`. |
| `cancelled` | A response event is optional and does not block cancellation; cancellation MUST NOT leave an unaccounted Effect or Approval. |

### 10.2 Terminal seal

The terminal event MUST be interpreted against the prefix before itself:

\[
\Gamma;T^-\vdash Ready_k(r)
\]

Only then may the matching terminal event establish:

\[
\Gamma;T^-\cdot terminal_k(r)\vdash Terminal_k(r)
\]

This prevents `run.completed` from proving itself merely by existing.

The current mapping is:

| Kind | Terminal event |
|---|---|
| `completed` | `run.completed` |
| `failed` | `run.failed` with status `failed` |
| `timed-out` | `run.failed` with status `timed-out` |
| `cancelled` | `run.cancelled` |
| `audit-incomplete` | `run.failed` with `status=audit-incomplete` and `audit-incomplete=true` |

### 10.3 `audit-incomplete`

The current Runtime may expose process-local `audit-incomplete` when it cannot
safely establish an ordinary durable terminal.  It may queue its durable pair
only when readable Tape establishes that **no Run terminal of any kind** exists:
for example, a required Run write fails before INSERT, or the terminal Guard
cannot prove the requested readiness judgment.  Runtime may then queue exactly
two recovery events in Tape's protected per-Run sidecar:

```text
run.failed       status=audit-incomplete, audit-incomplete=true
turn.completed   status=audit-incomplete, audit-incomplete=true
```

`superchat-tape-replay-events` copies missing recovery events into SQLite by
stable event ID before returning the trace.  Closure accepts only this
explicitly flagged pair as an exceptional durable terminal.  The exception
does not require ordinary `Ready_failed`, because its meaning is precisely
that ordinary audit completion could not be proved.  It also does not account
or synthesize any missing Tool, Effect, Approval, or response event; those
nodes remain visible in `ResidualBoundary` and `PendingEffects`.

**Safety correction — post-seal write failures.**  If readable Tape confirms
one ordinary Run terminal (including an INSERT whose receipt was lost), Runtime
MUST retain that fact and MUST NOT append this exceptional pair.  It may queue
only one matching ordinary `turn.completed` suffix after readable Tape also
proves the matching Turn seal absent; the suffix has the same Session/Run/Turn
and status and is caused by that confirmed Run seal.  Reconciliation rechecks
those facts before append.  A failed Run or Turn lookup is `unknown`, never
proof of absence: Runtime MUST queue neither the audit pair nor a guessed Turn
suffix.  A readably confirmed ordinary Run seal with its matching Turn seal
absent is a valid trace whose durable-Run judgment is `proved` and durable-Turn
judgment is `open`; only a failed lookup is `unknown`.  Runtime may still expose
local `audit-incomplete` on that lookup failure, but MUST queue no durable pair
or guessed suffix.

Before reconciliation, or if both the primary Tape write and fallback write
fail, the interpreter may observe only a partial trace such as:

```text
intent.received
run.started
response.completed      ; perhaps present
                         ; durable terminal seal absent
```

It MUST report that prefix as `open` (or `unknown` if the trace itself cannot
be interpreted).  It MUST NOT reconstruct `audit-incomplete` from absence,
because a crash or still-running provider can produce the same prefix.

### 10.4 Runtime Guard

When `superchat-closure-terminal-guard-enabled` is non-nil, Runtime MUST ask
`ReadyTerminal` against the persisted prefix immediately before appending the
matching Run terminal seal.  The requested terminal kind and the proved kind
MUST match.

The v0 distribution enables this Guard by default.  Users may opt out without
rewriting Tape; doing so affects only later Runtime transitions.

An `open` or `unknown` judgment, invalid trace, or interpreter/replay failure
MUST prevent the ordinary seal.  The live Run becomes `audit-incomplete`; it
MUST NOT fall back to an ordinary unproved `run.failed` event.  Runtime MAY
queue only the explicitly flagged recovery pair from section 10.3.  The Guard
does not make Authority decisions and does not change Tool policy.

### 10.5 Rollout observations

Shadow comparisons and Guard decisions produce copied diagnostic reports with
Session, Turn, Run, requested terminal kind, and their complete judgments or
errors.  Runtime retains at most the latest 100 reports in process memory.

These observations are Soft Diagnostics.  They are not Tape facts, do not
survive Emacs restart, and MUST NOT participate in Closure or Authority
decisions.  `superchat-runtime-closure-observations` returns caller-safe copies
and can restrict results to disagreements, rejections, and unknowns.

A minimal canary session is:

```elisp
(superchat-runtime-clear-closure-observations)
(setq superchat-closure-shadow-enabled t)
;; Exercise normal Runs, then inspect only anomalies:
(superchat-runtime-closure-observations t)
```

Default Guard activation was an explicit release decision made only after the
observed semantic classes were explained by the contract.  The observation
count itself was not an activation threshold.

## 11. Witness model

Every `proved` Hard judgment MUST include enough information to reconstruct
why it holds.  The logical witness contains:

```text
contract / semantics identity
judgment kind and subject
source event IDs
Boundary node IDs
rules applied
terminal or accounting dispositions
residual nodes, if the question permits them
```

One possible representation is:

```elisp
(:verdict proved
 :judgment (:kind durable-run-terminal
            :run-id "run-42"
            :terminal-kind completed)
 :contract "closure-v0"
 :event-ids ("evt_intent" "evt_start" "evt_response" "evt_terminal")
 :node-ids ("run:run-42" "obligation:run-42:outcome")
 :rules (run-started response-completed ready-completed terminal-seal)
 :residual nil)
```

This plist is illustrative, not a required public Elisp representation.

A human-readable explanation MAY be rendered from the witness.  Generated
prose is not itself a proof.

## 12. Checkpoints as proof boundaries

A checkpoint is a semantic interpretation boundary, not a memory summary or
permission to delete Tape.

For prefix \(P\) and suffix \(R\):

\[
T=P\cdot R
\]

checkpoint continuation is correct only if:

\[
\llbracket P\cdot R\rrbracket_\Gamma
=
fold(\delta_\Gamma,\llbracket P\rrbracket_\Gamma,R)
\]

A durable checkpoint logically identifies:

```text
last covered storage cursor and event ID
semantic contract / Projection version
Boundary Graph or its materialized equivalent
hash of the checkpointed interpretation
open Boundary node witnesses
```

An implementation MAY initially replay from Tape instead of materializing the
entire Boundary.  It MUST satisfy the same equivalence before using a
checkpoint as an optimization.

Canonical Tape history remains available for audit, rebuild, and future
Projection versions.

## 13. Authority contract seam

Closure Contract v0 does not define Authority policy.  It supplies only Hard
facts and readiness judgments used by that separate contract.

For action \(a\):

\[
\Gamma_A;B;a\vdash Decision(d,w_A)
\]

where the decision is one of:

```text
allow
deny
ask
defer
unknown
```

An Authority `allow` is insufficient when Closure is `open` or `unknown`.
A Closure `proved` is insufficient when Authority denies or asks.

Plugin or Tool descriptors MAY declare a possible Effect envelope and result
schema.  They MUST NOT certify that an Effect occurred, completed, or is
accounted; actual Runtime events and Authority receipts provide those facts.

## 14. Required safety properties

A conforming v0 implementation must demonstrate the following properties over
the supported event language.

### 14.1 Deterministic replay

The same ordered \((T,\Gamma)\) produces the same Boundary and judgments.

### 14.2 Prefix composition

For every valid prefix and event:

\[
\llbracket T\cdot e\rrbracket_\Gamma
=
\delta_\Gamma(\llbracket T\rrbracket_\Gamma,e)
\]

### 14.3 Checkpoint equivalence

Any checkpoint implementation MUST produce the same result as full replay
when continued with the same suffix.  Phase 1 exposes no checkpoint adapter;
full replay remains canonical, so this property becomes executable only when
checkpoint support is introduced.

### 14.4 Soundness

If the executable Projection returns `proved`, the declarative trace rules can
derive the same judgment:

\[
Judge_{Q,\Gamma}(\llbracket T\rrbracket_\Gamma)=proved(J,w)
\Rightarrow
\Gamma;T\vdash_QJ
\]

### 14.5 Completeness on the supported language

If the declarative v0 rules derive a judgment from a valid, fully supported
trace, the executable Projection can return it:

\[
\Gamma;T\vdash_QJ
\Rightarrow
Judge_{Q,\Gamma}(\llbracket T\rrbracket_\Gamma)=proved(J,w)
\]

Completeness is not claimed for unknown event schemas or future extensions.

### 14.6 Unknown safety

Unknown schema, missing required provenance, or contract mismatch MUST NOT
produce `proved` Hard Closure.

### 14.7 Provenance completeness

Every accounted node and every `proved` judgment has source event IDs and rule
identities sufficient for independent checking.

### 14.8 Invalid sequence rejection

Examples that MUST NOT be accepted as ordinary closure include:

```text
tool.completed without tool.requested
tool.completed after a pre-execution denial
approval.decided without approval.requested
run.completed without matching pre-terminal readiness
turn.completed before a valid Run terminal seal
relevant Run events after a terminal seal
```

### 14.9 Independent-event commutativity

If two events are causally independent and touch disjoint Boundary identities,
their application order does not change the result:

\[
\delta(\delta(B,e_1),e_2)=\delta(\delta(B,e_2),e_1)
\]

No commutativity is claimed for events sharing a Run, execution, Approval, or
Artifact identity.

## 15. Normative trace examples

### 15.1 Successful Run

```text
intent.received
run.started
response.completed      status=completed
run.completed           status=completed
turn.completed          status=completed
```

Expected judgments:

```text
TraceWellFormed          valid
ReadyTerminal completed proved over prefix before run.completed
DurableRunTerminal      proved completed
DurableTurnTerminal     proved completed
PendingEffects          empty
```

### 15.2 Approved Tool execution

```text
tool.requested           decision=ask
approval.requested
approval.decided         decision=allow
tool.execution.started
tool.completed           status=succeeded
```

Expected judgments after the last event:

```text
Approval                 accounted allow
Effect                   accounted discharged
PendingApprovals         empty
PendingEffects           empty
```

### 15.3 Denied Tool execution

```text
tool.requested           decision=deny
tool.failed              status=denied
```

Expected judgment:

```text
Effect request           accounted denied
Effect execution         never started
```

### 15.4 Timed-out Run

```text
intent.received
run.started
response.completed      status=timed-out
run.failed              status=timed-out
turn.completed          status=timed-out
```

The terminal kind is `timed-out`, not generic `failed`, even though the
terminal event type is `run.failed`.

### 15.5 Cancellation with a pending Effect

```text
tool.requested
tool.execution.started
run.cancelled
```

`Ready_cancelled` is not proved because the Effect remains unaccounted.  The
trace exposes a closure violation instead of allowing the terminal event to
erase the Effect.

### 15.6 Audit failure

```text
intent.received
run.started
response.completed      ; optional, depending on the failed write
run.failed               ; status=audit-incomplete, audit-incomplete=true
turn.completed           ; status=audit-incomplete, audit-incomplete=true
```

After recovery reconciliation, the Run and Turn terminal judgments are
`proved` with terminal kind `audit-incomplete`.  A missing `tool.completed`
remains missing, so any corresponding Effect stays `open`.  Without the two
explicitly flagged recovery events, replay cannot reconstruct the status and
the prefix remains `open`.

### 15.7 Unknown event schema

```text
tool.completed          schema=3, unsupported by Γ0
```

The affected Effect and Run judgments are `unknown`, never `proved` by event
name fallback.

### 15.8 Unknown or inconsistent Authority contract

An Effect chain with a missing or unsupported `policy-version` is `unknown`.
Changing the policy version between its request and Approval events is an
invalid trace; dependent lifecycle questions are `unknown`, never `proved`.

### 15.9 Impossible failure transition

```text
tool.requested           decision=allow
tool.execution.started
tool.failed              status=denied
```

This trace is invalid.  `denied` describes a pre-execution Authority refusal;
an Effect that started execution may finish only through an execution outcome
recognized for that disposition.

## 16. Implementation seam and rollout

The intended module is deep: one judgment interface hides trace validation,
Boundary construction, transition rules, witness assembly, and checkpoint
handling.

### 16.1 Contract-to-code mapping

| Contract concern | Phase 1 code |
|---|---|
| Γ identity and event classification | private constants and classification in `superchat-closure.el` |
| Trace validation | `superchat-closure--interpret` |
| Operational Boundary and δ transitions | private state plus `superchat-closure--apply-event` |
| Judgment and Witness | public `superchat-closure-judge` plus private query helpers |
| Persisted trace adapter | public `superchat-tape-replay-events` |
| Runtime shadow comparison | private `superchat-runtime--run-closure-shadow` |
| Shadow observation | `superchat-closure-shadow-functions` reports `agreed`, `disagreed`, or `unknown` |
| Runtime terminal Guard | private `superchat-runtime--guard-terminal`, opt-in through `superchat-closure-terminal-guard-enabled` |
| Rollout observations | bounded process-local ledger exposed by `superchat-runtime-closure-observations` |
| Normative replay fixtures | `test/test-closure.el` |

These concerns intentionally share one deep module.  Boundary data and reducer
functions are not public APIs; callers receive only Judgment plus Witness.
Phase 1 folds the caller-supplied trace directly: the current Projection
registry has static initial state and cannot carry a query-specific scope
without exposing Boundary.  Shadow-mode materialization may reuse that registry
later if it gains a scope-safe seam.

The first implementation SHOULD reuse the existing `superchat-projection`
replay seam.  It SHOULD NOT add setters for Run, Effect, Approval, or Boundary
state.

Recommended rollout:

1. implement the pure v0 interpreter over existing events;
2. test it against recorded successful, failed, timed-out, cancelled,
   approval, and audit-gap traces;
3. run it in shadow mode beside the current process-local Runtime registry;
4. report disagreements with witnesses;
5. only after soundness evidence, use `Ready_k` to guard terminal transitions.
6. retain bounded observations from real Shadow and Guard runs, reviewing
   anomalies by witness rather than by an error-count threshold;
7. change default activation only after observed semantic classes are either
   explained by the contract or corrected.

Steps 1–7 are implemented.  Shadow remains diagnostic and is disabled by
default through `superchat-closure-shadow-enabled`.  Terminal guarding is
enabled by default through `superchat-closure-terminal-guard-enabled`; it
permits only an exactly matching `proved` readiness judgment.  Guard rejection
cannot append the requested ordinary terminal seal; Runtime instead queues the
explicit `audit-incomplete` recovery pair and closes the process-local
lifecycle.  Step 7 was an explicit semantic-class review, not an automatic
counter-based transition.

## 17. Acceptance criteria for Contract v0

The contract is implementable when one pure interpreter can answer \(Q_0\)
for the existing event vocabulary and the following checks pass:

- successful replay produces proved Run and Turn terminals;
- Tool Approval and Effect accounting are identity-safe;
- failed, timed-out, and cancelled terminals remain distinct;
- a pending Effect prevents a valid cancellation closure;
- late callbacks cannot introduce valid events after terminal;
- an audit gap never becomes durable success;
- unsupported schema produces `unknown`;
- the Guard permits proved readiness and rejects open or unknown readiness
  without appending the requested ordinary seal; any recovery seal is
  explicitly flagged `audit-incomplete`;
- when a checkpoint adapter exists, checkpoint plus suffix equals full replay;
- every proved result identifies its events, Boundary nodes, and rules.

Only after these checks hold should v1 consider Claim/Evidence semantics,
generic obligation transfer, Artifact acceptance, or additional Projection
families.

## 18. Freeze boundary

Contract v0 is complete when it safely guards the current Runtime lifecycle:

- false terminal seals are rejected;
- dangling Effects and Approvals block terminal readiness;
- rejection and residual witnesses explain why;
- replay distinguishes `open`, `unknown`, and durable terminal evidence;
- Runtime recovery can durably seal explicit `audit-incomplete` Run/Turn
  terminals without inventing a missing Effect outcome.

The v0 implementation is now frozen at that boundary.  New Boundary node
types, a materialized graph, checkpoint storage, behavioral-equivalence
machinery, Claim/Evidence semantics, and Projection-lattice work are not v0
follow-ups.  Reopen this contract only for a demonstrated safety defect in the
supported trace language or for an event-schema/Authority-contract migration.
The 2026-08-07 recovery-seal change is such a safety erratum and remains within
the existing v0 event schema.

Product development proceeds in higher Runtime layers: task re-entry through
Memory Projection, durable Workspace lifecycle, Workflow/Trigger continuity,
and Team collaboration.  Closure remains their safety kernel rather than the
product destination.

## 19. Consumer adoption boundary

Contract v0 is stable.  A new consumer may depend on Closure only when all
three conditions hold:

1. the judgment must remain valid across an Emacs restart;
2. it controls a durable or irreversible state transition;
3. the same judgment can be rebuilt from Tape alone.

UI progress, streaming offsets, and in-memory queue positions therefore stay
outside Closure.  The first consumer is Workflow step handoff: Workflow owns
which step comes next, while Closure proves whether the previous Run has a
durable successful terminal before the step graph advances.  This proves
Runtime execution correctness only; it does not certify the quality or truth
of the model's result.
