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
| The fixed Stage pipeline is retired in favour of one execution main line ([record](decisions/accepted/2026-09-12-retire-fixed-stages.md)) | Not started; the Stage registry is `current` until M3 |
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

### M2 — One tool pipeline (`proposed`; recommended sub-goal under the Authority contract in force)

Goal: every tool execution goes through one pipeline with Authority as the single decision point and approval as a seam.

Candidates for removal, each to be confirmed against the source before M2 starts (they come from the 2026-09-12 full-read audit and were not re-checked after 4C-2): a duplicated default tool list, a second tool-selection pass on chat input, `fboundp` probes between Authority and the agent-loop wrappers, the `on-demand` selection alias. A candidate that the source no longer shows is dropped from the list, not removed from the code.

Mechanism (`proposed`): pipeline order pre-policy → approval → monotonic guards (deny-only) → around → body → post → finalize; named Authority policies become guards; the approval prompt is a provider that fails closed when absent.

Acceptance: an approved `read_buffer` call on a real model produces the same Tape sequence as the current smoke case B; a denied write is verified separately: the refusal reaches the consumer and no tool body runs, with its own expected sequence; a policy that returns `allow` cannot relax an inherited `deny`.

### M3 — One execution main line (`accepted` retirement of the Stages, `proposed` mechanism)

Goal: replace the fixed Stage registry with a single driver loop.

Removes: the Stage registry as an extension point, and the dispersed ownership of its eight names (`parse` is invoked only by the chat dispatcher over a preparation Turn, the six preparation Stages by the driver, `save-state` by Runtime); the per-session FIFO and the steer queue as mechanisms separate from the inbox. The count difference between declared and invoked names is not itself a defect; the dispersed ownership is what M3 ends.

Mechanism (`proposed`, API not yet designed): an agent handle with an inbox (`followup` next turn, `steer` next step, `inject` no wake), turn and step bracketing on the Tape, a pre-step waterfall and a request waterfall as the two interception points, a serial turn-stopping hook. The FIFO's semantics (queued-while-running, drop on dead Sink, pause when transport is withdrawn, `scope-retired` drops) migrate into the inbox; they are not deleted. Run lifetime and the retire-before-dispose contract are designed here.

Acceptance: the complete canonical smoke set (every case in `scripts/smoke.el`, including R1 to R9) passes with Tape sequences unchanged from the baselines in force when M3 starts (M1's `request.header` and `request.message-list` events are part of those baselines); sending during a Run still queues; cancel still seals `run.cancelled`, and a Run with an entered external Effect still terminates `audit-incomplete` with the Effect open; a static check that a contract "passed" does not replace these behavioural assertions. Live evidence is kept as well: one normal turn and one cancelled turn against a real provider complete with the expected terminal facts, without demanding a fixed number of model events from a non-deterministic model.

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
