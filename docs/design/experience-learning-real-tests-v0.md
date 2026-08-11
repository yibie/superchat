# Experience learning: thin real-environment tests v0

## What the first experiment can and cannot say

The smallest useful claim is:

> A later Run that receives one reviewed Experience View chooses the reviewed
> recovery action more often than an otherwise identical Run that receives no
> View.

This is a **retrieval/use** claim.  It is not the stronger claim that the model
induced a new memory from its own previous trajectory.  The current product
has the durable Tape, Evaluation, Candidate, Promotion, and View seams, but
does not yet have an automatic induction module.  A fixture-created Candidate
must therefore be named honestly as a reviewed seed.

## What the Google experience loop actually contributes

The relevant Google work is ReasoningBank, not a larger transcript store.  Its
time axis is a closed loop:

```text
before task: retrieve a strategy
during task: act and keep the trajectory
after task: judge the outcome, distill success or failure, append a strategy
next task: retrieve the updated strategy
```

The paper and the Google Research implementation make three boundaries useful
for Superchat:

1. A raw trajectory is input to extraction, not itself a learned rule.  The
   extracted item is short, structured, and intended to transfer beyond the
   original query; failed trajectories provide preventative, counterfactual
   lessons as well as successful trajectories providing procedures.
2. “It was retrieved” is not “it was learned.”  Their evidence is a change in
   held-out task success and interaction steps against a memory-free baseline,
   including cross-task and cross-site transfer.  Our current trace has a
   treatment-only wording transfer, not a held-out no-View control, so it does
   not meet that bar.
3. Their released code deliberately keeps consolidation simple (append) and
   does not provide our product's human promotion, revocation, Authority, or
   Tape immutability guarantees.  We borrow the temporal separation and the
   evaluation discipline, not their embedding cache or LLM-as-judge as a
   replacement for product evidence.

Sources: [Google Research's explanation](https://research.google/blog/reasoningbank-enabling-agents-to-learn-from-experience/),
[the paper](https://arxiv.org/abs/2509.25140), and the
[released implementation](https://github.com/google-research/reasoning-bank).

The local executable verdict is intentionally stricter than a View audit:

```sh
SUPERCHAT_EXPERIENCE_VERDICT_TRACE=experiments/experience-learning-minimal/five-run-20260812-revocation/trace.el \
  emacs -Q --batch -L . -l experiments/experience-learning-minimal/verdict.el
```

It reports `retrieval-use: observed`, `durable-retrieval: observed`, and
`learning: not-identifiable`.  The missing evidence is explicit: no held-out
no-View control, no repeated paired outcomes, and no completed task outcome
(the intentional two-round stop leaves the Runs `failed` after the action).
Therefore this trace is useful diagnostic evidence, but it cannot enable the
default Experience View.

The verifier only upgrades `learning` when the trace also names at least two
completed held-out control/treatment pairs in `:held-out-pairs`; the positive
ERT fixture in `verdict.el` exercises that gate without pretending to be a
provider run.

## Architecture decision

The subtraction belongs in the experiment and induction path, not in the
durable safety path:

- Keep Tape as the immutable history, Projection as the rebuildable View,
  Experience as reviewed selection, Runtime as lifecycle owner, and Authority
  as the effect gate. Each has a distinct owner and deleting one would move a
  real safety responsibility into another module.
- Do not add an embedding service, automatic LLM judge, consolidation worker,
  or “learned memory” module to the Runtime hot path. They are optional
  offline policies, not prerequisites for a safe View.
- Make the next experiment two-stage: (1) one real treatment/control pair to
  observe retrieval/use, then (2) a separate held-out stream with its own
  no-View control, repeated outcomes, and revocation check to test Learning.
  A failed gate produces `not-identifiable`; it does not trigger another
  contract revision or an automatic rerun.

The first paired run used three opaque tools and kept the action out of the
task prompt.  It retained the exact provider request bodies, decoded `llm`
events, and Tape events.

| paired run | no View | reviewed View | result |
|---|---|---|---|
| `trace-20260812-run2.el` | did not complete the probe/action protocol | `read_probe → minimal_refresh` | treatment-only success |
| `run-20260812/trace.el` | `read_probe → minimal_refresh` | called `minimal_refresh` before the probe | control-only success |

Both pairs used four local provider requests and no in-run retry.  The two
opposite outcomes are the important result: a single model call is not enough
to identify learning.  The current verdict is **not identifiable**; the
default remains off.

The first five-method trace exposed a runner bug: its arm names did not match
the condition that injected the View text.  It is retained as a harness-debug
artifact, not as behaviour evidence.  The corrected run is
`experiments/experience-learning-minimal/five-run-20260812-corrected/trace.el`
and used 12 local provider requests:

The completed follow-up trace is
`experiments/experience-learning-minimal/five-run-20260812-revocation/trace.el`;
it used 14 local provider requests with no retries.  It reopens the DB before
the first persistence Run, appends a user revocation, rebuilds the View, and
executes one post-revocation Run.

| method | View status | first observed action | interpretation |
|---|---|---|---|
| matched control | none | `read_probe → minimal_uncertain` | no reviewed action without the View |
| matched treatment | satisfied | `read_probe → minimal_refresh` | one treatment-only signal; needs replication |
| View-content ablation | satisfied, empty prompt | no tool action | no Candidate text; model completed without the action |
| wording transfer | satisfied | `read_probe → minimal_refresh` | same action appeared; one run is not transfer proof |
| negative transfer | `blocked` | `minimal_uncertain` after `read_probe` | Candidate text absent; the live model did not exercise the explicit denial branch |
| persistence after reopen | satisfied | `read_probe → minimal_refresh` | durable View selection survived DB reopen |
| persistence after revocation | `empty` | `read_probe → minimal_uncertain` | revocation removed the Candidate from the rebuilt View |

In this latest trace, the ablation Run completed without a tool action and the
other six Runs hit the intentional two-round stop after their follow-up action
and ended `failed`.  These terminal statuses are Runtime facts, not evidence
that the model learned or did not learn.  The positive observations are one
treatment sequence and durable View selection; the ablation and transfer
results prevent a learning or default-enablement claim.  The negative case
proves disclosure safety.  The live model selected `minimal_uncertain`, so it
did not exercise the explicit Authority denial of `minimal_refresh`; that
denial is covered by the shared Authority and Runtime tests.  Causal
behavioural uptake remains `not-identifiable`, and the default stays off.

### Minimal temporal loop: source failure before admission

The first implementation of the temporal loop exposed all three opaque tools
to the source Run.  The model skipped the prescribed failing probe and called
`minimal_refresh` immediately.  The adapter incorrectly fell back to the
source `response.completed` event and admitted a Candidate anyway.  That trace
is retained as a harness finding, not as induction evidence.

The strict follow-up keeps the source Run to `read_probe` for one tool round and
rejects admission unless the source Tape contains `tool.failed`, a terminal Run
event, and the Decision Snapshot.  Its trace is
`experiments/experience-learning-minimal/closed-loop-20260812-strict/trace.el`:

| stage | observed result |
|---|---|
| source Run | `read_probe` failed; Run ended `failed`; Candidate was admitted only afterwards |
| held-out pair 1 | no-View chose `minimal_uncertain`; View chose `minimal_refresh` |
| held-out pair 2 | no-View failed to complete the protocol; View chose `minimal_uncertain` |
| reopen | View was rebuilt as `satisfied` and the action was `minimal_refresh` |
| revoke | View rebuilt as `empty`; `minimal_refresh` was not selected |

There were 19 local provider requests and no retry.  The thin verifier reports
`learning: not-identifiable`: one pair shows a treatment-only action, but the
second pair does not, and the completed-outcome gate is not met.  The result is
useful because it catches both architectural mistakes—admitting without a
source failure and treating one successful pair as learning—while keeping the
default Experience View off.

### Self-learning loop: model induction without answer leakage

The next run changed the target from “does a reviewed View get used?” to
“does the Agent generate the Candidate from its own closed trajectory?”  The
source task no longer names either recovery action.  It only requires a probe,
an observed failure, and a recovery choice.  After the source Run closes, a
separate model Run receives the bounded Tape trajectory and must emit the
closed JSON shape `lesson/action/avoid`; the runner rejects malformed output and
does not supply a Candidate sentence.

The no-answer-leakage trace is
`experiments/experience-learning-minimal/self-learning-20260812-final-safe/trace.el`.
The model generated the rule:

> When a `read_probe` execution fails with an opaque error, apply the serial recovery tool `minimal_refresh`.

That generated Candidate was then used in two independent held-out pairs. Both
no-View controls failed on `minimal_uncertain`; both View treatments completed
with `minimal_refresh`. The same View survived a DB reopen and disappeared
after a user revocation. The run used 22 local requests, with equal three-call
budgets inside each control/treatment pair and no retry.

The thin self-learning verifier reports:

```text
status: supported-with-human-gate
model-induced-candidate: observed
held-out-task-effect: observed
learning: not-identifiable (fresh persistence provenance missing)
default-remains-off: t
```

This is the first evidence for the requested behavior: the model itself
produced the reusable rule, and the rule caused a repeated held-out effect.
It is not permission to silently enable the product.  Promotion was still an
explicit user gate, and the default Experience View remains off.  The earlier
`self-learning-20260812-v5` trace is intentionally rejected by the verifier
because its source prompt told the model the exact wrong-then-right action
sequence; that was supervised replay, not self-learning.

### Generalization attempt: new task, failed source closure

The first transfer attempt changed the opaque code to `Q-K42`, changed the
probe and recovery tools to `inspect_token`, `refresh_marker`, and
`defer_unknown`, and kept the same thin loop.  Its complete trace is
`experiments/experience-learning-minimal/self-learning-generalization-20260812/trace.el`.

This run is **not** learning evidence.  The source Run selected
`defer_unknown`, which failed; the model reflection then guessed a
`refresh_marker` rule, but the source Run never completed the corrective
action.  Held-out treatment 1 succeeded and treatment 2 selected the wrong
action.  The verifier therefore returns `not-identifiable`, and the default
remains off.  The source-success requirement is now enforced in both the
Candidate admission seam and the thin verifier, so a guessed rule cannot be
promoted merely because it sounds plausible.

### First-action rerun: remove the measurement confound

The previous trace mixed two questions: which recovery action was chosen
first, and whether the whole Run reached a terminal `completed` status.  A
Run could choose the correct action on its third tool round and still be
marked `failed` by the three-round ceiling.  The verifier therefore now uses
`first-recovery-action` as the primary metric; final status and request count
are retained as auxiliary diagnostics.

The fixed-parameter rerun is
`experiments/experience-learning-first-action-20260812-final3/trace.el`.
It uses the same `Q-K42` task, `temperature=0`, no provider seed (the current
evidence contract does not expose one), and a four-round ceiling.  It made 26
requests.  The source Run completed after the model eventually corrected
itself; both no-View controls chose `defer_unknown` first, while both View
treatments chose `refresh_marker` first and completed it.  The treatment effect
is therefore observed, but it is only a same-class first-action effect.

The original trace's reopen arm reused the in-memory View created before the
database was closed, so it does not prove View reconstruction.  Its revoke arm
reassembled in the same process, so it proves immediate disclosure removal but
not cross-restart revocation.  The runner and verifier now require explicit
View assembly provenance after each reopen; the historical trace must be
rerun before durable persistence can be claimed.  Under that stricter gate,
the trace's learning result is provisional rather than a persistence-verified
promotion signal.  The current interpretation is:

```text
primary-metric: first-recovery-action
model-induced-candidate: observed
held-out-task-effect: observed
learning: not-identifiable (fresh persistence provenance missing)
durable-reopen: not established in this trace
cross-restart-revocation: not established in this trace
auxiliary cost-parity: failed (wrong first actions take one extra round)
default-remains-off: t
```

This corrects the earlier interpretation: the original treatment-2 did not
necessarily ignore the View; it first chose the wrong action, then chose the
right action on a later round, and the old ceiling hid that distinction.

### Corrected persistence rerun

The repaired runner was executed once as
`experiments/experience-learning-first-action-20260812-final4/trace.el`.
It made 26 requests with the same `temperature=0`, no provider seed, and no
retry policy.  The two control arms again chose `defer_unknown` first; both
treatment arms chose `refresh_marker` first.  This time the trace records a
fresh `assemble-view` after DB reopen and another fresh assembly after revoke
and a second DB reopen.  The verifier therefore reports:

```text
status: supported-with-human-gate
primary-metric: first-recovery-action
model-induced-candidate: observed
held-out-task-effect: observed
persistence-and-revocation: passed
learning: supported
default-remains-off: t
```

The trace also records hashes for `closed-loop.el`, `paired-use.el`,
`generalization.el`, and `verdict.el`; all four match the files used for this
review.  This repairs the persistence evidence boundary, but does not turn
the same-class Q-K42 result into broad transfer evidence or prove that
temperature zero eliminates nondeterminism.

### Action-safety follow-up

The missing responsibility is now explicit.  A `context-envelope` may carry
`(:experience-action-policy (:dependent-tools ("tool-name" ...)))`.  Runtime
forwards that policy to Authority; when the View status is `blocked`, Authority
denies only those named tools and records the reason in `tool.requested` and
`tool.failed`.  An omitted policy keeps the weaker, disclosure-only behavior;
unrelated tools are never blanket-blocked.

The product path was then rerun in the complete 14-request trace named above.
The negative arm again chose `minimal_uncertain` after `read_probe`, so the
model did not exercise the denial branch.  The denial branch itself is covered
at the shared Authority seam and the Runtime envelope-refresh test.  This
fixes the responsibility gap; it does not change the learning verdict or
enable the default.

## Five real-environment methods

Each method runs the real Ollama provider through Runtime and Authority.  Each
Run keeps its full trace and is executed once per pre-registered assignment;
an infrastructure error is not silently retried.  A method is only scored
when its View status, action origin, and terminal Tape join are all present.

### 1. Matched control/treatment

Run the same task and tool surface with the View omitted and with the selected
View injected.  Randomize arm order across independent pairs.  Repeat the pair
enough times to estimate an action-rate difference; do not call one lucky pair
proof.

Pass signal: the treatment action rate is higher and the first action is
caused by the treatment Decision Snapshot.  If rates overlap, report
`not-identifiable`.

### 2. View-content ablation

Keep the candidate, promotion, context, and receipt in the envelope, but run
one arm with the candidate statement removed (or with only the receipt and no
instruction).  This separates “the model saw a View” from “the model used the
instruction in the View”.

Pass signal: the full View changes behaviour while the receipt-only ablation
does not.  The exact prompt and `view-envelope` must be stored so a missing
statement cannot be confused with a failed selection.

### 3. Wording/instance transfer

Use a new task wording and a new Context Claim that are semantically equivalent
to the seed, without copying the seed's sentence or identifiers into the user
prompt.  Assemble a separate `applies-under` relation for that context.

Pass signal: the agent performs the same safe abstract action on the new
instance.  Literal copying of an artifact ID or answer string is a failure of
generalisation, not a pass.

### 4. Negative transfer and safety

Present a conflicting, wrong-scope, multiple, or revoked Candidate.  The
Experience assembler must return `empty` or `blocked`; the model must not see
the blocked Candidate text.  If the product also promises that a blocked View
forbids a particular action, that prohibition must be an explicit Authority
policy carried into the tool execution seam; it cannot be inferred from the
absence of prompt text.

Pass signal for disclosure: fail-closed View selection and no candidate text
in the prepared prompt.  Pass signal for action safety: the Authority policy
denies the named action and records the denial.  A Runtime failure after a
safe refusal is not the same as a successful task, so record both separately.

### 5. Persistence and revocation

Select the same promoted Candidate after closing and reopening the DB (a new
Emacs process is preferable).  Run the real task once.  Append a user revocation
event, assemble a new View, and run the matched task again.

Pass signal: the pre-revocation Run receives the Candidate and the
post-revocation Run does not; no stale prompt survives the Projection rebuild.
The trace shows `satisfied` before revocation and `empty` after it.  This tests
durable selection and revocation, not model memory in RAM.

## Common trace minimum

Every assignment records:

- method/arm, task and Context reference;
- selected View status, prompt, receipt, candidate/promotion/relation refs;
- every Decision Snapshot and its prepared envelope;
- exact provider request body, decoded `llm` log, and request count;
- `tool.requested`, `tool.failed`/`tool.completed`, first action, and terminal
  Run event with causation/execution joins;
- model/dependency roles and the final interpretation (`passed`, `failed`, or
  `not-identifiable`).

The Tape remains an append-only evidence ledger.  The Experience View is the
small derived read model injected into a Run.  Embeddings, prompt caches, and
induction summaries are optional later modules; they are not prerequisites for
  these five tests.

## Recovery-domain closed loop (LM Studio, Qwen3-30B)

The workspace revision-recovery domain reopened the self-learning question with
a stronger model.  The transport moved from Ollama to LM Studio
(`make-llm-openai-compatible`, `http://localhost:1234/v1/`); the drivers read
`SUPERCHAT_EXPERIMENT_MODEL` / `SUPERCHAT_EXPERIMENT_URL` and default to the
old Ollama path.  A durable mutable Artifact adapter (`durable-file`,
`superchat-workspace-publish-mutable`) was added to the product so the stale
cell can be seeded with the revision mutation inside the producer Run's
durable proof.

The driver is `recovery-self-learning.el`; its loop is:

1. a candidate-free setup (stale durable Artifact + Context Claim);
2. a source Run with no Experience and a clean task that names neither
   recovery answer — the model must discover `workspace_info` and the
   corrected read from the tool surface alone;
3. reflection over the bounded source trajectory (consecutive identical
   outcomes collapse into one entry carrying `:repeats`, so repeated failed
   retries do not bury the successful tail);
4. admission through the user Promotion gate;
5. two held-out control/treatment pairs judged on `first-recovery-action`.

### Model comparison

| model | paired first-action separation | source self-discovery | induced rule |
|---|---|---|---|
| qwen3.5:9b (Ollama) | 4/4 vs 0/4 | never | not reached |
| LFM2.5-1.2B | 4/4 vs 0/4 | not tested | — |
| Qwen3-30B-A3B (LM Studio) | 4/4 vs 0/4 | 3/4 of fixed-harness runs | correct 3/3 when admitted |

The minimal recovery chain (Runtime policy state machine plus the Authority
gate) reproduced the full separation on every model, including a 1.2B model —
the behaviour difference is architectural, not scale-dependent.  Scale
mattered only for the self-induction step: qwen3.5:9b never discovered the
two-step recovery, while Qwen3-30B did in three of four runs and always
induced the correct rule (`workspace_info` first) once the source Run was
admissible.  The only remaining variance is the source Run itself: in the
failed run the model retried the read four times and stopped, which the
admission gate correctly rejected.

`verdict.el` gained `superchat-recovery-self-verdict` for this trace format.
Its gate mirrors the opaque-domain discipline: model-induced Candidate,
held-out first-action effect, human Promotion gate, and the source task must
not name either recovery answer.  The default remains off regardless of the
verdict.
