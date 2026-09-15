# Roadmap

The one remaining plan for superchat. It says what is built, what the user has decided, what is still a proposal, and in which order the spine is simplified without giving up the safety contract already delivered. The current map of the code is [architecture.md](architecture.md); this page does not restate it.

## Status labels

Every sentence about the system in this page and in `docs/decisions/` carries one of four labels.

| Label | Meaning | Where it lives |
|---|---|---|
| `current` | In the tree and verified against the code | [architecture.md](architecture.md), component specs |
| `accepted` | The user has decided it. Whether it is unbuilt, partly built or built is stated per item below | `docs/decisions/accepted/` |
| `proposed` | A recommendation awaiting the user | `docs/decisions/proposed/` |
| `historical` | Superseded by an explicit later record; kept for reference | superseded decision records, `docs/design/turn-pipeline-contract.md`, `.wayfinder/` |

The ADRs under `docs/adr/` (0001, 0004, 0005, 0006) are accepted contracts in force; an ADR becomes `historical` only when a later record names it as superseded.

A milestone is `done` when its removal list is gone from the tree and its own acceptance section is satisfied. Code milestones name real-model (LM Studio) and scripted-transport evidence; a documentation milestone names link, example and review evidence instead.

The user decisions recorded under `docs/decisions/accepted/` were checked against the user's own words during the 2026-09-12 collaboration between the two editing agents. None of them approves trimming the default plugin set.

## Baseline (`current`, 2026-09-12)

Delivered through batch 4C-2: caller-borrowed execution Scope through Stage, Event and Runtime; scoped tool, command and Authority registries; per-Chat-View Preset Scope; Runtime retires a Scope's Runs before that Scope is disposed; a Run borrows its Scope and never owns it; closing a View or leaving the mode deactivates its Preset; a Run whose external Effect is unresolved at cancellation terminates as `audit-incomplete` while the Effect itself stays open, and if a genuine late result arrives its one completion fact is appended without resuming the Run. The prompt's `history` fragment is assembled from the Tape Context View (`superchat-view-assemble`); the chat window and compaction still read a separate in-buffer history list.

Where the current contracts live: the cross-module request lifecycle in [protocols/request-lifecycle.md](protocols/request-lifecycle.md); the composition contract in [design/plugin-architecture.md](design/plugin-architecture.md); the plugin author's guide in [plugin-guide.md](plugin-guide.md); commands, dependencies and environment quirks in [agents/development.md](agents/development.md). The former [turn-pipeline-contract.md](design/turn-pipeline-contract.md) is `historical` and kept for its section links. Past batch acceptance records stay in `.wayfinder/` and are not copied into tutorials.

Batch 4C-3 (an automatic Run Scope) is not started and is not scheduled on its own; Run lifetime is designed inside M3 so that no empty Scope layer is added.

## Decisions already taken (`accepted`)

| Decision | Implementation state |
|---|---|
| DeepSeek Harness is the blueprint; superchat's goal is a complete, opinionated AI layer for Emacs ([record](decisions/accepted/2026-09-12-deepseek-harness-blueprint.md)) | A direction; no code change is implied by itself |
| Experience, Claims and Closure leave the spine and ship as official plugins; they stay in the default set ([record](decisions/accepted/2026-09-12-experience-claims-closure-as-official-plugins.md)) | Not started; the driver plugin still injects `closure`; delivered as M3a right after M3 |
| Documentation follows one spec per component with a fixed structure, decision records with mandatory alternatives, present tense, one home per fact ([record](decisions/accepted/2026-09-12-documentation-regime.md)) | Scaffold in place (`docs/README.md`, `components/`, `decisions/`, `postmortems/`); component specs follow as modules land; M0 finishes the alignment |

Not decided by the user: the model-facing tool surface (dropping path-file tools for Emacs-object tools). Until decided, no existing tool, plugin or feature (workflow, MCP, memory, plans, teams, workspace, GenUI, emacsclient) is removed from the default set.

## Milestones

The order M1 to M4 and each milestone's sub-goal are the two editing agents' joint recommendation under the accepted blueprint and the Authority and Tape contracts in force; the user has not approved them item by item. Each milestone's mechanism is `proposed` until its own record is accepted.

### M0 — Documentation alignment (`done`)

Removes: the "Destination reached" claim in the working map, stage and Kernel-file counts that disagree with the tree, and the stale sentences in the earlier proposal (a Preset without a Scope, a prompt built without the Tape). `.scratch/` stays the local issue tracker.

Acceptance, limited to the documents changed in this round: each either describes `current` behaviour or carries one of the other three labels; `docs/decisions/accepted/` holds the four decisions above; every relative link in the changed documents resolves; every runnable example in an active guide written or changed in this round has been run; for a historical document only its status header and its changed links are checked, and the old examples in its body are not claimed as re-run; both editing agents have read each other's changed files in full and resolved every conflict; no `.el` or `scripts/` file changed during M0 (hashes compared before and after). Documents not touched in this round are not claimed as re-verified.

### M1 — One model history (`done`)

Goal: one authority for model-visible history. The Tape is the source from which it is derived; the chat View may keep a cache only if that cache is regenerated from the Tape and never read as an authority.

Removes: the in-buffer history list as an authority for compaction and rendering; any path that composes prompt history from a buffer instead of the Tape.

Mechanism: a `derive-messages` function over the Tape Context View; compaction and the chat window read it; before the buffer list loses its authority, the two histories are rebuilt for the same recorded session and compared. The evidence this acceptance needs is not on the Tape today; [the request-header decision](decisions/implemented/2026-09-13-reconstructable-provider-request.md) is `implemented` and fixes what to record and how to keep it from restating every request.

Delivered (`current`): `superchat-view-derive-messages` is the one derivation, honoring the compaction digest, the expanded-Anchor reading and the in-buffer cap; `request.header` and `request.message-list` are recorded before each provider attempt under the four write reasons the decision fixes; `superchat-tape-request-evidence` rebuilds a request from those rows and reports `unknown` when a reference does not resolve; `prompt-reconstructable` performs that reconstruction; the in-buffer list is regenerated from the derivation and is no longer edited by compaction or Anchor expansion.

Verification: the scripted fixture reproduces the provider-visible system text, message history and tool schemas, reports `unknown` when a reference does not resolve, and the buffer list and the derivation agree before compaction, after compaction and after Anchor expansion; compaction keeps the Anchor's identity, summary and sources. Against a real provider, one chat turn and one inline turn both complete their model exchange with every request rebuilt from the Tape and no invariant violations. The two transports assert different facts and the smoke script records which: the exact event sequence, the settled overlay and the untouched host buffer are `scripted-' keys, which the accepted criteria waive for a real model's free text, while `reconstruct-status' and `invariant-violations' are asserted in both modes.

Acceptance: for a recorded fixture (fixed input and fixed model result, scripted transport), an independent reconstruction from the Tape reproduces the provider-visible system text, messages and tool schemas of every request, including each rewrite and retry, with the source relation of every message stated; where redaction or missing evidence prevents reconstruction the check reports `unknown`, never a pass; the buffer-list history and the Tape-derived history agree on that same fixture; compaction on the fixture produces the same anchors as before. A recorded `prompt.assembled` receipt alone is not acceptance. Separately, one chat turn and one inline turn against a real provider (LM Studio) complete with their history derived from the Tape; the model's free text is not compared byte for byte with an older output.

### M2 — One tool pipeline (`done`)

Goal: every tool execution goes through one pipeline with Authority as the single decision point and approval as a seam. The mechanism is [the one-tool-pipeline decision](decisions/implemented/2026-09-13-one-tool-pipeline.md).

Candidates for removal, re-confirmed against the source before M2 started (they come from the 2026-09-12 full-read audit and were not re-checked after 4C-2). All four were still present, and two are narrower than the audit described; a candidate that the source no longer shows is dropped from the list, not removed from the code:

| Candidate | Re-checked finding | Outcome |
|---|---|---|
| a duplicated default tool list | `superchat--select-tools-baseline-names` and the default value of `superchat-llm-tool-names` hold the same five names in different order | removed: the baseline reads the one home (`superchat-llm-default-tool-names`), with a declared fallback literal for the load order |
| a second tool-selection pass on chat input | The decision is not recomputed -- the pipeline's `:tool-decision` reaches the LLM layer and is honored. The redundancy is a second registry query in `superchat--collect-llm-tools` | removed: a `baseline` decision's own `:materialized` structs are reused, so the chat path no longer queries the registry a second time |
| `fboundp` probes between Authority and the agent-loop wrappers | Seven probes in `superchat-authority.el`; four point at the agent loop and three at Runtime | removed for the four: each is a declared row in `superchat-authority--contributors` whose absent-case answer is written down. The three Runtime-side probes stay, out of this record's scope |
| the `on-demand` selection alias | Inert by its own documentation; kept for saved custom values and identical to `always` | retained, as the record decides |


Mechanism (`accepted`, implemented): pipeline order pre-policy → approval → monotonic guards (deny-only) → around → body → post → finalize, with the seven steps named at their boundaries in `superchat-tool-execute`; the approval prompt is a declared provider whose absence is a stated deny that the refusal reports as "no approver available" instead of "cancelled by user"; the named-policy monotonicity the record asked for already held and is a regression gate rather than new work.

Delivered (`current`): the pipeline's order is marked in the source and each step keeps the behaviour it replaced; approval resolves through one declared provider over the three existing sources in their existing precedence; the four agent-loop probes are gone, replaced by declared contributions whose missing case is stated in the table rather than implied at the call site; the default tool surface is unchanged, name for name.

Acceptance evidence: `.scratch/m1-verification/evidence-m2-acceptance.txt` records all three, and `scripts/smoke-expected/*` is byte-identical to the baseline M2 started from (`44153daa7ca8c4bc8064f5bb39f2a169f54e072feed59879655c490be57153be`). (a) An approved `read_buffer` on a real model (LM Studio, `qwen/qwen3.8-27b`) produces the Tape sequence smoke case B asserts, element for element, with one `request.header` and `superchat-tape-request-evidence` reporting `reproduced`. (b) A denied write and a denied read each reach the pipeline, run no tool body, record their own sequence (`tool.requested` then `tool.failed`, with no `tool.execution.started`) and deliver the refusal into the model's tool-result slot. (c) With a registered policy layer answering `allow` for everything, an `observer`-policy write is still refused with the policy's own text and its body still does not run, while a read in the same session still executes -- so the layer was consulted and could not loosen the inherited deny.

Acceptance (a) failed on its first run and found a real defect in M1's evidence recorder: from the second attempt on, the recorder read the system text off a prompt whose `:context` llm.el had already folded into a system interaction and cleared, so it recorded an empty system and appended a second `request.header` whose only change was that erasure. The provider always received the system text; the record did not. The recorder now takes the sending layer's own provider-facing system text instead, so the recorded sequence stays one header per series.

### M3 — One execution main line (`done`)

Goal: replace the fixed Stage registry with a single driver loop.

Removes: the Stage registry as an extension point, and the dispersed ownership of its eight names (`parse` is invoked only by the chat dispatcher over a preparation Turn, the six preparation Stages by the driver, `save-state` by Runtime); the dispersed ownership of pending input, so that follow-ups, steering and injection have one Runtime owner and steering and injection one ordered claim, over the three stores the [closing rulings](decisions/implemented/2026-09-15-m3-closing-rulings.md) keep. The count difference between declared and invoked names is not itself a defect; the dispersed ownership is what M3 ends.

Mechanism: the [execution main line decision](decisions/implemented/2026-09-14-m3-execution-main-line-resolution.md), taken by the leader under user delegation, fixes a Runtime-owned handle and target-aware inbox, pre-step and request waterfalls, a closing serial notification, and explicit `step.started` / `step.ended` Tape facts. Runtime retains sole lifecycle ownership. Existing queue, cancellation, Authority and retire-before-dispose contracts remain binding; no graceful-stop API or restartable pending queue is added.

Runtime owns the driver handle, Step boundaries, both waterfalls and the closing recheck; steer and inject have one ordered claim and audited pending-injection disposal. Earlier evidence is in `.scratch/m3-verification/evidence-m3-worker-round.txt`.

Stage retirement slice 2 is delivered (2026-09-15): `save-state`, `load-state`, `resolve-session`, `assemble-prompt` and `select-tools` are absent from the Stage vocabulary. Core assembles the prompt and builds default tool candidates; request middleware replaces tool selection within the original explicit ceiling. First and subsequent native attempts validate the replacement, preserve trusted wrappers, and record actual provider evidence. Pure tool-guidance renderers refresh dependent system blocks without repeating collectors. The default plugins remain mounted. Evidence and the narrowly authorized structural assertion replacement in smoke V are in `.scratch/m3-verification/LEADER-SLICE2.md`.

Stage retirement slice 3 (context, admission and parsing) is delivered (2026-09-15): `collect-context` is an Event whose listeners return fragment lists, and the driver's own direct call replaces the last Stage call site. `parse` is the chat surface's call to `superchat-core-prepare-turn`; submission admission and queueing live in Runtime (`superchat-runtime-admit-envelope`), with Step gating still in the pre-step waterfall. The Stage registry API (`superchat-stage-add`, `superchat-stage-call`, `superchat-stage-remove`, `superchat-stage-names`) no longer exists in the tree; the Event substrate, the two ordered-await waterfalls, the `collect-context` Event and the fault ring live in `superchat-event.el`. A required listener's execution or validation failure is reported as `context-error` by the pipeline's own collection error path and then re-signalled, so preparation still terminates. Fault records use `:site`; their recorded values are unchanged. Evidence is in `.scratch/m3-verification/LEADER-SLICE3.md`.

The M3 regression set replaced the ad-hoc probes (2026-09-15): `scripts/m3-regression.el` is one file of ERT tests grouped by contract area, run by one command, with each test in its own isolated fixture; only the leader agent edits it. It asserts contracts a fixed event sequence cannot express (who may claim what, what a refusal leaves behind, which Step a fact belongs to, what a failure is recorded as), and it caught a real product defect: a derived source's `:inputs` is a list of property lists, which the Tape's JSON round trip rendered as one object with colliding `:id` keys, so a merge or split lost every input but the last. The Tape now owns a value codec for its META column that tags that shape, so the durable record carries both inputs' identities and original sources. That closes the "source relationships" gate. The set also found that a command or skill refused by the queue while a Run is busy lost the user's draft; the dispatcher gives it back, meeting the same nine criteria as plain input. Details, the ten-way mutation check and the commands are in `.scratch/m3-verification/REGRESSION-SET.md`.

The [closing rulings](decisions/implemented/2026-09-15-m3-closing-rulings.md) are implemented (2026-09-15). The `chat-turn-recorder` plugin registers its two `run-sealing` observers with `:required t`, and a required recording failure seals an otherwise completed Run `audit-incomplete` through `run.failed`, leaving the Step ending and `response.completed` as recorded. `stage-error`, `:stage` and `superchat-stage.el` are `context-error`, `:site` and `superchat-event.el`. Injection and follow-up drops keep one event identity, so a drop whose append committed but lost its receipt is not recorded again, and an item whose drop is unconfirmed is never admitted or dispatched as input. Later Turns' Context View includes admitted material once, from the Tape; within a Run a later Step continues its provider prompt. A tool-protocol retry, and a provider failure on a later attempt, close that Step `failed`.

The user accepted the slice 2 smoke V baseline replacement on 2026-09-15 as a bounded exception after the Stage exit and the move of parsing and assembly: `chat-parser-direct: t` and `assembler-works-without-its-plugin: t` replace lines that read the retired Stage registry. `chat-parser-direct` checks only that `superchat-core-prepare-turn` exists, so chat parsing behaviour is proven by the M3 regression set's `m3/parse/` tests (`@model`, `/command`, `>skill`, parsing before queueing, unknown commands, non-chat surfaces, `#file`) and smoke cases S, T and W. The acceptance authorizes no other baseline change and relaxes no Tape event order, Step disposition, queueing, cancellation or Authority constraint.

The user accepted M3 on 2026-09-15, with the middleware interface narrowed by [closing ruling 5](decisions/implemented/2026-09-15-m3-closing-rulings.md): no cancellation context is passed, and after cancellation Runtime's execution boundaries keep a middleware result from opening a Step or entering the model transport, without stopping the middleware's own work. Follow-up, not part of M3: when request middleware cancels the current Run, the sender receives "Runtime Run is no longer active; model transport not entered"; the submission should end through the existing cancellation path with the single `cancelled` terminal, no new failure terminal and no resend, while genuine transport-unavailable errors keep being reported. M3a is next.
Acceptance: the complete canonical smoke set (every case in `scripts/smoke.el`, including R1 to R9) passes against the baselines in force when M3 starts with only the accepted insertion of `step.started` and `step.ended` facts and assertions for their identity, placement and disposition. Excluding only those two new types reproduces each old event sequence; the complete new sequence is also checked. Existing events retain their order and semantics (M1's `request.header` and `request.message-list` events remain part of the baseline); sending during a Run still queues; cancel still seals `run.cancelled`, and a Run with an entered external Effect still terminates `audit-incomplete` with the Effect open; a static check that a contract "passed" does not replace these behavioural assertions. Live evidence is kept as well: one normal turn and one cancelled turn against a real provider complete with the expected terminal facts, without demanding a fixed number of model events from a non-deterministic model.

### M3a — Experience, Claims and Closure as official plugins (`accepted` direction, bounded sub-item immediately after M3)

Goal: the spine loads without Experience, Claims or Closure mounted, while every consumer that needs a Closure proof fails closed when Closure is absent; all three stay in the default set.

Removes: the driver's hard `:inject` of `closure`; any Experience or Claims call reached from the spine other than through the optional `recovery` service.

Delivery criterion: with Closure unmounted, a Run seals its terminal facts and the workflow resume, subagent handoff and artifact delivery consumers report "no proof" and withhold, never "proved"; Closure absence is never recorded or treated as a proof. With the default set mounted, the complete canonical smoke set is unchanged. Nothing is removed from the default set.

### M4 — Surfaces as plugins, composition by profile (`proposed`)

Goal: the chat window, inline ask and rewrite, Org Babel, Magit and emacsclient are plugins that a profile row list assembles.

Removes: nothing from the default set. `superchat-plugins` stays until a profile mechanism exists and the default profile reproduces it exactly.

Acceptance: removing any row of the default profile leaves the rest loading; a `headless` profile answers one prompt with no UI plugin mounted.

### Later (`proposed`, gated on the undecided tool surface)

Emacs capability seams (buffer, command, eval, process, file, org, magit, lsp), each a Definition, a Provider and a Consumer, and the Emacs-object tool surface. Neither starts before the user decides the tool surface.

Independent and unscheduled (`proposed`): a standing Preset mount that several Views join by re-parenting. It does not depend on the tool surface. It must keep the delivered per-View isolation: two Views choosing the same Preset name are not, by that fact, joined to one mount.

## What this roadmap does not do

No line-count targets. No documentation gates or verification scripts. No new test suite; the existing smoke script and real-model runs are the evidence. No milestone deletes a feature from the default set without a recorded user decision.
