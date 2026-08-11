# Experience View Default Enablement — Decision Map

Goal: decide whether Superchat should replace today's explicit Experience
selection with one thin, default Recovery Intervention that improves the First
Recovery Action after a known repeated failure.  The map ends at an executable
ship/keep-off gate and an exact product change boundary; experiments exist only
when a named decision cannot be resolved from product evidence.

Existing evidence, not the goal:
[`experiments/artifact-semantic-use-v0/DECISION-MAP.md`](../../experiments/artifact-semantic-use-v0/DECISION-MAP.md).

## user-outcome: Improve The First Recovery Action

Blocked by: none
Status: resolved
Type: Grilling

### Question

What must the user experience differently?

### Answer

After a known repeated failure, the first user-visible instruction or
effect-seeking Action should use the unique reviewed Candidate applicable to
that context without making the user restate it.  A direct instruction and a
tool Action both count.  When no Candidate applies uniquely, asking rather
than guessing is correct.

## safety-boundary: Record Misuse Without Inventing Judgment

Blocked by: user-outcome
Status: resolved
Type: Grilling

### Question

What happens when an Experience is absent, ambiguous, misused or unsuccessful?

### Answer

Misapplication is a hard keep-off condition.  Missing, conflicting, multiple,
revoked or negatively related Candidates produce no injection.  A failed
First Recovery Action and its Outcome are recorded as Facts and surface a
Review Need; they do not automatically create `fails-under`.  An explicit
review may append `fails-under` when the Candidate does not apply to that
Context, or preserve applicability when execution failed for another reason.
Earlier relations remain on append-only Tape.

## default-behavior: Enable Automatically After Internal Acceptance

Blocked by: safety-boundary
Status: resolved
Type: Grilling

### Question

Does “enable” mean another limited rollout or ordinary default behavior?

### Answer

The current explicit `:experience-selection` path is already limited
enablement.  After internal acceptance passes, ordinary recovery work should
select Experience automatically.  This map does not add another opt-in,
preview period or rollout tier.

## thin-intervention: Add Context, Not A Controller

Blocked by: default-behavior
Status: resolved
Type: Grilling

### Question

How much should the framework intervene?

### Answer

Only after Authority records a known failure, inject at most one applicable
Experience View before the same Run's next model request.  Do not prefill the
initial request, choose the recovery Action, force a tool, retry, add a hidden
model call, relax Authority or introduce a recovery controller.  Runtime
records Facts already produced by execution; semantic judgment remains an
explicit user or evaluator act.

## acceptance-principle: Test The User Outcome

Blocked by: thin-intervention
Status: resolved
Type: Grilling

### Question

What evidence may authorize default enablement?

### Answer

Deterministic product tests for selection, receipts, replay and Authority are
necessary but insufficient.  A frozen internal end-to-end acceptance must
observe the real First Recovery Action.  Every declared supported scenario
must receive an acceptable first Action; every no-candidate, `fails-under`,
conflict, multiple-candidate and revoked scenario must avoid Candidate misuse.
Any misuse keeps the default path off.

## first-recovery-class: Start With One Real Read-Only Slice

Blocked by: acceptance-principle
Status: resolved
Type: Grilling

### Question

Which real recovery workflow should establish the first supported boundary?

### Answer

Use a cross-Run Workspace Artifact read whose expected revision is missing or
stale.  Its Recovery Class is determined from structured product Facts, not
error-text similarity or a model classifier.  Its reviewed recovery is to
obtain the current Artifact revision and then read that exact revision.  This
is the first vertical slice, not a request for a global error taxonomy.

## recovery-class-signal: Give The Failure A Stable Identity

Blocked by: first-recovery-class
Status: resolved
Type: Grilling

### Question

What is the smallest product-owned, structured identity for the selected
Workspace read failures, and where should it cross Authority/Tape without
parsing error strings, classifying with a model or creating a global error
framework?

### Answer

Use one read-specific Emacs condition,
`superchat-workspace-artifact-revision-error`.  Its Recovery Class identity is
the pair `workspace_read` plus that condition name.  Emit it only inside the
cross-Run Artifact delivery check when `expected_revision` is absent or stale,
including a stale final revision check.  Same-Run reads, `workspace_write`, an
unknown Artifact and every other delivery rejection remain outside this
class.

Authority catches the condition at its existing execution seam and appends
only its normalized name as `:error-type` to the existing `tool.failed` Fact;
the model-readable failure text remains delivery material, not identity.
Ordinary Tape does not gain the error message or condition data, and exact
evidence mode keeps its existing receipt behavior.  Recovery selection uses
the structured `:tool-name` and `:error-type` fields, never error text.  This
adds no event type, table, registry, model judgment or global error taxonomy.

## model-support-boundary: Decide What “Internally Tested” Covers

Blocked by: first-recovery-class
Status: resolved
Type: Grilling

### Question

Which model/Profile boundary must pass the real First Recovery Action gate
before a provider-agnostic package may enable Experience View by default, and
what behavior is promised outside that tested boundary?

### Answer

The real behavior gate covers one exact reference cell: the built-in `code`
Agent Profile and one Provider/Model combination frozen later by
`acceptance-contract`.  That contract must capture the Provider implementation
and version, Model identity, effective Profile body and tools, other effective
system-prompt contributors, inference options and tool-loop limits.  A Profile
name or Model name alone is not a reproducible boundary.  Existing ERT and the
old Ollama experiment do not satisfy this gate.

Passing that cell authorizes the provider-agnostic default insertion; it does
not create a Provider/Model/Profile support matrix or a runtime allowlist.
Outside the tested cell, Superchat promises only deterministic framework
behavior: inject the uniquely applicable Candidate, inject nothing for absent,
negative, conflicting, multiple or revoked Candidates, preserve Authority,
and add no forced Action, retry or hidden model request.  It does not promise
that an untested Model/Profile will use the Candidate correctly, produce an
acceptable First Recovery Action or recover successfully.  The resulting
Action and Outcome remain Facts subject to explicit review.

## review-path: Surface Review Without A Background Judge

Blocked by: first-recovery-class
Status: resolved
Type: Grilling

### Question

What is the smallest user/evaluator action and rebuildable View that turns
recorded recovery evidence into a Review Need, then permits an explicit
`fails-under`, retained applicability or revocation without an automatic
semantic verdict?

### Answer

Keep Review Need as a pure, rebuildable Experience-domain View, not a stored
status.  It opens only when Tape replay can uniquely join the structured
Recovery Class failure, the selected Candidate/Promotion/Context receipt, the
First Recovery Action after that intervention, and an explicit unsuccessful
Outcome.  A matching `tool.failed` may be the Outcome for a tool Action; a
direct instruction needs a later user or explicitly invoked evaluator Outcome
Fact.  Missing or ambiguous joins remain missing/blocked evidence and create
neither an actionable Need nor a semantic verdict.

One host-only review action, exposed to the user as `/review`, shows the exact
evidence references for one open Need and requires one of three judgments plus
a reason.  Retained applicability appends a new `applies-under` relation whose
basis names that recovery evidence; inapplicability appends `fails-under` with
the same basis; revocation appends the existing user-only `revoke` disposition
against the prior Promotion.  An explicitly invoked evaluator may record a
relation through the host seam, but only the user Actor may revoke.  None of
these writers is a model tool.

The View derives a Need as handled only from a later matching relation or
revocation whose basis cites its recovery evidence; no `review.need` or review
verdict event is added.  Append-only history remains decisive: a new positive
relation does not erase an earlier `fails-under`, opposite relations stay a
conflict and keep Experience selection off, and revocation only deactivates
the named Promotion.  This adds no background judge, automatic relation,
Memory status, materialized table, panel or recovery controller.  The later
`intervention-seam` must make the Experience receipt and Action/Outcome join
durable before this View can be proved on the ordinary path.

## intervention-seam: Prove The Thin Same-Run Product Flow

Blocked by: recovery-class-signal, review-path
Status: resolved
Type: Prototype

### Question

Can the real Workspace → Authority → Tape → Experience View → Runtime path
classify the selected failure and insert exactly one eligible Candidate before
the same Run's next provider request, while every absent, negative, ambiguous
or revoked case injects nothing and no extra orchestration appears?

### Answer

Yes, behind a default-off internal gate.  Cross-Run `workspace_read` now emits
`superchat-workspace-artifact-revision-error` only for missing or stale
expected revisions.  Authority persists the condition name as the existing
`tool.failed` Fact's `:error-type`, while ordinary Tape still omits the error
message and condition data.

Before an existing same-Run `tool-followup` or `final-answer` request, Runtime
replays the latest typed failure and asks Experience for the exact
`workspace_read` plus condition-name Recovery Class.  Experience reuses the
existing fail-closed View: it requires one Context and one active, uniquely
applicable promoted Candidate.  Empty, negative, conflicting, multiple and
revoked cases therefore return no prompt.  A satisfied View is appended to
the already-mutated llm.el prompt; no provider request, retry, controller,
tool choice or Authority exception is added.  Runtime permits one attempt per
Run and records `experience.intervention.recorded`, caused by the triggering
failure, with the exact Candidate/Promotion/Relation/Context receipt.

The durable receipt plus the next `tool.requested`/`response.completed` and
its explicit failed Outcome now rebuild an Experience-domain Review Need.
`/review` shows the evidence references and requires `retain`, `fails-under`
or `revoke` plus a reason, then calls the existing relation or user-only
disposition writer.  A later matching judgment whose basis cites the recovery
episode closes the derived Need; no Review Need table or verdict event exists.

Product ERT proves the typed Workspace/Authority boundary, exact selection and
negative fail-closed cases, one same-Run insertion, durable replay receipt,
all three review judgments, unchanged Closure semantics and exactly two
provider requests for a one-tool-round interaction.  The canonical suite
passes 712 tests with 0 unexpected results (19 optional-dependency skips).
This prototype does not prove a real Model's First Recovery Action; the gate
`superchat-runtime-recovery-intervention-enabled` remains `nil` until
`acceptance-contract` and `internal-acceptance` resolve.

## acceptance-contract: Freeze The Ship/Keep-Off Cases

Blocked by: recovery-class-signal, model-support-boundary, review-path
Status: resolved
Type: Grilling

### Question

What exact fixed internal scenarios, observable Actions, Outcomes, model
boundary and failure rules constitute the enablement gate before any live
acceptance run or default-on product change?

### Answer

Freeze
[`experience-view-internal-acceptance-contract-v0.md`](../design/experience-view-internal-acceptance-contract-v0.md)
(SHA-256
`85179efbb728fbda148cf8b95dc1b12d9263055fa9888a8c42d23bccfddfe2ab`).
It fixes one Acceptance Reference Cell: the sealed Superchat source, built-in
`code` Profile, local `llm-ollama` 0.31.1 / Ollama 0.32.6 /
`qwen3.5:9b` Provider/Model, exact prompt and tool boundary, Authority and
tool-loop settings, and a zero-cost local endpoint.

The cohort is seven isolated, once-only cells: missing and stale positive
cases plus absent, `fails-under`, conflict, multiple and revoked negative
cases.  Each cell permits at most two real provider requests; the cohort cap
is fourteen, with no retry or replacement.  Positives must uniquely prove
typed failure → intervention receipt → attempt-2 snapshot → first
no-argument `workspace_info` Action → matching `tool.completed`.  Negatives
must record no intervention, expose no blocked Experience bytes and avoid
Candidate misuse.  Any missing, ambiguous, infrastructure or provider result
fails and consumes this contract revision.

Only 7/7 plus zero-unexpected canonical ERT authorizes the one-line default
change.  Otherwise the gate remains `nil` and evidence names the next repair.
This ticket made no provider call and did not change the default.

## internal-acceptance: Decide Whether Default Enablement Is Earned

Blocked by: intervention-seam, acceptance-contract
Status: resolved
Type: Prototype

### Question

Does one frozen internal product acceptance pass every declared positive and
negative case without retry, replacement or Candidate misuse, so the exact
default-on product change is authorized; or does evidence require keep-off or
a named product repair?

### Answer

Keep off.  The frozen launch preflight is recorded in
[`launch-preflight-v0.md`](../../experiments/experience-view-internal-acceptance-v0/launch-preflight-v0.md).
All 15 sealed product-source digests, the contract digest, Emacs 31.0.91 and
Ollama 0.32.6 matched the declared boundary.  However, both ordinary batch
Emacs and `emacs -Q --batch` resolved neither `llm` nor `llm-ollama`; the
preflight could therefore neither load `llm-ollama` 0.31.1 nor record its
required commit, capabilities, or adapter SHA-256.

That is a contract-defined infrastructure mismatch before the first provider
request.  No `POST /api/chat`, cohort cell, retry, replacement, Candidate
selection, or canonical ERT run occurred.  It proves neither a successful
First Recovery Action nor Candidate misuse; it does prove that this frozen
contract cannot authorize default enablement.  The gate remains `nil`.

Per the frozen once-only rule, this rejected contract must not be retried.
After the required adapter is installed and pinned, a new contract revision
must freeze the resulting source/adapter/model boundary before a newly invoked
acceptance ticket can execute its seven cells.

## acceptance-contract-v1: Refreeze The Executable Launch Boundary

Blocked by: internal-acceptance
Status: resolved
Type: Task

### Question

Can a new acceptance contract preserve every v0 model, product and cohort
constraint while replacing only its incomplete `emacs -Q` package bootstrap
with the verified explicit activation and pinned-source load procedure?

### Answer

Freeze
[`experience-view-internal-acceptance-contract-v1.md`](../design/experience-view-internal-acceptance-contract-v1.md)
(SHA-256
`4a3d69f38bfc7773a79c1e9b62cd6f10df7b09008cbbcd58278fa612a6e90a39`).
It inherits every v0 product and acceptance constraint, but replaces the
incomplete launch assumption with the verified private package activation and
pinned-source load.  The exact llm commit and `llm-ollama.el` digest are now
part of the required manifest.

The bootstrap loads both libraries and constructs the frozen local provider
under `emacs -Q --batch` without a chat request.  It does not execute a cohort
cell or alter the default.  `internal-acceptance-v1` is now the one unblocked,
once-only ticket permitted to run the seven-cell cohort.

## internal-acceptance-v1: Execute The Refrozen Cohort Once

Blocked by: acceptance-contract-v1
Status: resolved
Type: Prototype

### Question

Does the v1 bootstrap seal successfully and then let the frozen seven-cell
cohort prove every required positive First Recovery Action and every required
negative no-misuse result, within its once-only request ceiling?

### Answer

Keep off. The retained v1 launch manifest has all four dependency versions as
`nil` and omits the required redacted role for the private llm source. The run
also omits other required launch fields, including `jj` working-copy identity,
tool-schema JSON hashes, and complete Profile/request settings. It therefore
did not satisfy its own bootstrap contract: an unrecordable dependency was
supposed to reject preflight before any cell started. It also made 0/14 provider
requests: all seven incorrectly admitted cells stopped before their first
provider request. The retained cell evidence is in
[`RESULT.md`](../../experiments/experience-view-internal-acceptance-v1/RESULT.md).

The built-in `code` Profile exposes `workspace_write.append` as a boolean.
The frozen `epistemic-learning-v0` tool validator accepts only string, integer
and number arguments, so it rejected the Profile before Runtime could persist
the required decision snapshot. Runtime correctly prevented the provider
call. This establishes no model First Recovery Action and does not show a
Candidate misuse. It proves two basic blockers: the runner did not enforce
complete dependency metadata, and the reference cell cannot start its exact
audit trail. The gate remains `nil`.

First, a new runner must record every launch field and reject missing package
versions before cells begin. Then decide and test boolean argument handling in
exact evidence. Since the second repair changes sealed product source, both
repairs require a new frozen acceptance-contract revision and a new once-only
cohort. This v1 cohort must not be rerun.

## acceptance-contract-v2: Repair The Evidence Boundary Before Re-running

Blocked by: internal-acceptance-v1
Status: resolved
Type: Task

### Question

Can the failed v1 boundary be repaired without changing the declared
seven-cell behavior, and can startup prove the exact runtime boundary before
any chat request?

### Answer

The frozen v2.2 contract is recorded in
[`experience-view-internal-acceptance-contract-v2.md`](../design/experience-view-internal-acceptance-contract-v2.md).
It keeps the v0 tool order and seven-cell verdicts, but fixes the two proven
startup blockers. Exact evidence now preserves three distinct cases for the
optional `workspace_write.append` boolean: omitted, JSON `true`, and JSON
`false`; explicit JSON `null` remains rejected. The provider-facing false
value is retained as `:json-false` while the Emacs function still receives
`nil`.

The no-chat preflight is executable at
[`preflight.sh`](../../experiments/experience-view-internal-acceptance-v2/preflight.sh).
It provisions a private pinned llm checkout and private ELPA (including a
private `compat`, rather than Emacs 31's built-in copy), then starts a fresh
`emacs -Q`, verifies dependency versions/source ownership, jj identity, the
ordered ten-tool schemas, Profile fields, Ollama/model hashes, and the
materialized request. It constructs the provider under a transport guard and
recorded **0** `/api/chat` requests. Missing private dependencies fail closed.

The v2.2 contract is frozen with canonical digest
`49857c23d35255d1c5edef602805b86a434d4729706be6d7fc50481e552645a3`, final
runner
`c088470a11e85438cfffc90762aa9507c39d9700f1748f27c9310e1743a0faf6`, exact
dependency/schema/Profile/prompt locks, and an immutable once-only assignment
ID derived from that digest. The runner performs the complete no-chat audit,
seals all seven setup manifests, then allows at most fourteen real local
provider requests. The recovery-intervention default remains `nil` until the
new cohort verdict is admissible.

## internal-acceptance-v2: Execute One Repaired Cohort

Blocked by: acceptance-contract-v2
Status: resolved (failed; consumed)
Type: Prototype

### Question

After the v2 contract is frozen, does one newly claimed seven-cell cohort
prove every positive First Recovery Action and every negative no-misuse case
within the same once-only request ceiling?

### Answer

The v2 assignment was claimed once and failed while writing the no-chat
preflight manifest after provider construction, with zero model requests. Its
retained failure is
`experiments/experience-view-internal-acceptance-v2/evidence-d80f2d2e4117110fd3a52e9bb123ce5d288e83927157a18845f3b16bb8d5e9db/failure.el`:
the runner's two-argument evidence writer replaced preflight's one-argument
writer. The claim is consumed; it is not retried, and the default gate remains
`nil`.

## internal-acceptance-v2.1: Correct The Runner Boundary

Blocked by: internal-acceptance-v2
Status: resolved (failed; consumed)
Type: Task

### Answer

v2.1 renamed the runner evidence writer so preflight and cohort persistence no
longer shared a function name. Its new assignment was claimed once, then
failed before any model request because the `ReplaceBuffer` schema lock had a
transposed hex character. Its evidence is
`experiments/experience-view-internal-acceptance-v2/evidence-cc10942d8221b3d2abca7670aa1307061e404f911df82cdaf0f1f930497c415e/failure.el`.
The claim is consumed; it is not retried, and the default gate remains `nil`.

## internal-acceptance-v2.2: Correct The Schema Lock

Blocked by: internal-acceptance-v2.1
Status: in-progress
Type: Task

### Answer

v2.2 corrects the recorded `ReplaceBuffer` digest from
`650459be...` to the observed `650459ea...`. It is frozen but has not been
executed; any future cohort would be a new assignment.

## Notes

- Domain language is in [`CONTEXT.md`](../../CONTEXT.md), especially First
  Recovery Action, Recovery Class, Recovery Intervention, Review Need,
  Applicability Relation and Experience View.
- Current code assembles Experience only from explicit `:experience-selection`;
  ordinary chat does not query it automatically.
- Keep the framework thin: one necessary context insertion, no controller,
  forced Action, retry policy, automatic relation writer or hidden model pass.
- Product decisions and changes precede evidence runs.  Add an experiment only
  when a named ticket states which ship/keep-off decision its result controls.
- The old semantic-use map and its consumed assignments are evidence only and
  must not be rerun or treated as authorization.
- A live model/provider call requires its own invoked ticket with a frozen
  model, request ceiling, retry rule, evidence path and cost boundary.
