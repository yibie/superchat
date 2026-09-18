# Historical fixed turn-pipeline specification (revision 2)

> **Status: historical.** This is the S01–S27 planning record, not the current implementation contract or an executable work queue. Stage counts, file counts, line numbers, test gates, proposed interfaces and “current” evidence below refer to that older design snapshot. Closed slices do not establish that every statement here describes this checkout.
>
> Read the [current request lifecycle](../protocols/request-lifecycle.md), [composition contract](plugin-architecture.md) and [architecture map](../architecture.md) for implemented behavior. The [remaining plan](../roadmap.md) is the only active migration route. The fixed Stage registry is retired under [M3](../decisions/implemented/2026-09-14-m3-execution-main-line-resolution.md); the [M3a decision](../decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md) governs the removal of Closure and Claims. The Stage/Closure interfaces below belong to the historical snapshot.
>
> **Superseded since this snapshot (M6, 2026-09-18):** section 6 "Authority" and everything here that mentions effect resolvers, policies, `:attendance`, `:authority-policy`, `:approval-function`, `superchat-authority-mode` or the `tool-permission` / `tool-args-rewrite` / `tool-result-filter` Events describes machinery that has been deleted. Who may run what is now defined by [Permissions the DeepSeek Harness way](../decisions/implemented/2026-09-17-permissions-the-dsh-way.md); the execution pipeline's reference is [superchat-authority](../components/superchat-authority.md). The historical text below is left as it was.
>
> Historical sections are retained for existing references. Do not restore removed tests or follow obsolete file-count rules from this record. Relevant live behavior is defined by the source and the current contracts above.

## 1. Scope and boundary

| Item | Contract |
|---|---|
| Target scenarios | (1) persistent chat; (2) direct buffer work: inline ask and inline rewrite. |
| Buffer iron rule | A source buffer is byte-for-byte unchanged until the user explicitly accepts a Change Set (§11).  Overlay text, previews, Sink callbacks and Tape rows are not source edits. |
| Core | Envelope validation, fixed driver, stage registry, Run registry/lifecycle, Sink protocol, one model/tool loop, Authority, Tape/Closure, in-process follow-up queue. |
| Kernel file rule | Approved decision 4 (a fixed ten-stage pipeline) adds core files.  The Kernel grows from nine files to the nine plus `superchat-sink.el` (S01), `superchat-stage.el` and `superchat-pipeline.el` (S03a), `superchat-prompt.el` (S06a), `superchat-change-set.el` (S16).  A slice that creates one of these must, in the same edit, add it to the Kernel table and the "exactly N files" prose in `AGENTS.md` (`AGENTS.md:41-53`, `:98-105`) and to the kernel list of `runtime/kernel-requires-stay-inside-kernel` (`test/test-runtime.el:936-953`), with one line of rationale in the report.  Kernel require edges still stay inside the Kernel. |
| Plugin | Anything registered at a stage: surface, context collector, tool selector, renderer, recovery policy, domain feature. |
| Durable truth | Tape is append-only; Sinks, rendering, state and Views project it. |
| Execution model | Single-threaded Emacs.  There are no locks.  Re-entrancy is handled by transition flags and timer-0 deferral (§7.1). |
| Non-goals | No DI container, universal Session context, Request-struct migration, or "every reader is a Projection". |

Core never discovers a UI, infers a target from a buffer name, or writes a
buffer.  Only Sink implementations write buffers or apply Change Sets.

| Current evidence (verified 2026-09-11) | Consequence |
|---|---|
| Runtime derives default delivery from the request buffer plus `fboundp` render probes: `superchat-runtime.el:1028-1042` (final), `2111-2118` (status), `2124-2138` (stream); `superchat-runtime--view-session-current-p` treats any non-View buffer as writable (`1044-1059`). The chat dispatcher sets neither `:stream-function` nor `:delivery-function` (`superchat-dispatcher.el:373-395`), so the chat depends on this default. | S01 replaces the default with a Sink; a legacy adapter builds the chat Sink through a render-registered function, non-View buffers get a discarding Sink (D11, B-inv-1/2). |
| Prompt building runs plain hook lists in `superchat-core.el:77-89,134-146`; registration by `add-hook` at `superchat-prompt-hooks.el:385-418` prepends, so the actual order is the reverse of the comment at `:408` (D3). | S06: collect fragments, assemble once (§4.3). |
| llm.el emits observer `drain-steers` (`superchat-llm.el:2538-2544`); Runtime's observer `pcase` (`superchat-runtime.el:1943-2064`) has no such branch; `superchat-runtime--drain-steers` (`:700-709`) has no caller (D1). | S04: steering is a Run method; a nonempty Inbox forces another attempt (§5). |
| Streaming with tools requires `llm-capabilities` to advertise tool-use (`superchat-llm.el:1927-1936`), so unknown local model names never stream (N2). | S08: declared/adapter capabilities (§5.1). |
| Reads ask: `superchat-authority--decision-for-effect` maps attended `local-read` to `ask` (`superchat-authority.el:402-412`) and `--effective-decision` has no read branch (`:568-644`) (N1). | S07: read baseline and sensitive list (§6.1). |
| `superchat-runtime-cancel` (`superchat-runtime.el:1402-1432`) never consults `:parent-run-id`; no child index exists (D5). | S11: parent index and cascade (§7.4). |
| Typed-during-Run input is only "Draft retained" (`superchat-dispatcher.el:635-649`); `superchat--queued-follow-up` has no non-nil writer (D7). | S05: admit stage and in-process queue (§7.5). |

## 2. Envelope, surfaces, compatibility

### 2.1 Surfaces and producers

`superchat-register-surface SYMBOL` is a load-time side stage.  Built-in set:
`chat`, `inline-ask`, `inline-rewrite`, `org-babel`, `magit`, `workflow`,
`subagent`, `compaction`, `skills`, `external`.  A plugin registers its own
symbol before submitting (Telega registers `telega`).

| Producer | Attribution / Sink | Session | Tool baseline (§4.2) |
|---|---|---|---|
| Chat | chat View Sink (built by `superchat-runtime-view-sink-function`, owner render) | chat Session | `chat` set |
| Inline ask | `:context (:inline-buffer B :inline-range (BEG . END))`, overlay Sink | `inline:<project-root>` (one per project) | `superchat-authority-unprompted-read-tools` (§4.2) |
| Inline rewrite | `:context (:rewrite-buffer B :rewrite-region (BEG-MARKER . END-MARKER) :rewrite-original STRING)`, preview Sink | source project Session | `none` |
| Org-babel / Magit | string Sink (blocking adapter, §3.4) | ephemeral | `none` |
| Workflow / subagent / compaction | callback Sink; subagent sets `:parent-run-id` | producer-resolved | plugin-declared |
| Skills matching | `skills`, callback Sink | ephemeral | `none` |
| Telega | `telega`, plugin Sink | Telega Session | plugin-declared |
| External | registered or `external`, caller Sink | explicit or ephemeral | `none` |

`superchat-emacsclient.el` is a consumer only (`superchat-emacsclient.el:105-204`
lists/statuses/cancels).  S14 adds a `"run_status"` field with the new public
status and keeps `"status"` mapped: `waiting|typing|calling-tool → running`,
`ready → completed`, `error → failed` (or `audit-incomplete`),
`stopped|orphaned → cancelled`.

### 2.2 Envelope

A Request envelope is an ordinary plist copied by `superchat-pipeline-submit`.

| Field | Type | Required | Meaning |
|---|---|---:|---|
| `:content` | nonempty string | yes | Input before context assembly. |
| `:surface` | registered symbol | yes | One of §2.1. |
| `:sink` | `superchat-sink` struct | yes | Sole destination of visible emissions. |
| `:session-id` | nonempty string or nil | no (default nil) | Explicit durable Session or a request to resolve one. |
| `:attendance` | `attended` / `unattended` | no (default `attended`) | Human availability; never widens Authority. |
| `:parent-run-id` | nonempty string or nil | no | Direct delegation parent; never inferred. |
| `:context` | owner-prefixed plist | no | Surface facts (`:chat-*`, `:inline-*`, `:rewrite-*` …). |
| `:media` | list of file paths | no | Explicit media; never read from a buffer. |
| `:tools` | nil / `none` / list of tool names | no | Producer restriction consumed by `select-tools` (§4.2). |
| `:origin`, `:target-model`, `:capability`, `:topology`, `:authority-policy`, `:deadline`, `:system-prompt` | as today | no | Passed through to the Run. |
| `:request-id` | string | core fills | Identity before a Run exists. |

Validation (`superchat-pipeline-validate-envelope`) signals
`superchat-envelope-invalid` for: missing/empty `:content`; unregistered
`:surface`; `:sink` not a `superchat-sink`; dead Sink (§3.2); a `:context` key
without an owner prefix; presence of `:buffer`, `:buffer-name`,
`:stream-function`, `:delivery-function`, `:approval-function`,
`:prompt`, `:input`.  An explicit `:session-id` is never retargeted; a nil one
is resolved by stage 1.

**Host buffer.** The driver hosts the Run in the first live buffer among the
`:context` keys `:chat-buffer`, `:inline-buffer`, `:rewrite-buffer` and
`:host-buffer` — the last is the generic key for producers without a
user-facing surface buffer (subagent, skills, workflow, compaction) — validated
through `superchat-editor-workspace-validate-buffer` against the Run's Session;
with none of them, a hidden Session-unowned host buffer (S10, S13).

**Prepared prompt (transitional, S10/S13; S25 retires it).** A producer whose
legacy path has already prepared the complete user message passes
`:context (:prepared-prompt TEXT :prepared-system TEXT)` (the chat surface
included).  The driver then
skips the content and system-prompt fragments and the `collect-context` call,
and assembles exactly these two hard fragments.  `:content` stays the raw
input: it is what `intent.received` records, never the prepared prompt.

### 2.3 Compatibility bridges (kept through this project)

`superchat-runtime-submit` keeps accepting the documented legacy Request
(`docs/plugin-guide.md:166-188`).  Each bridge warns once per Emacs session
and seam through `display-warning` **only when
`superchat-runtime-warn-legacy-requests` is non-nil**; S14 sets it to `t` after
the built-in producers have moved to envelopes, so users are not warned about
superchat's own chat path in the meantime.

| Legacy seam | Bridge |
|---|---|
| Legacy Request (`:prompt`, `:buffer`, `:stream-function`, `:delivery-function`) | `superchat-runtime--legacy-sink` (S01): `:stream-function` → `:stream` (raw chunk, as today); `:delivery-function` → `:final` as `(funcall f TEXT 'completed TAPE-ID)` and `:error` as `(funcall f TEXT STATUS TAPE-ID)` with STATUS one of `failed timed-out audit-incomplete cancelled`; **no callback and a live `superchat-mode` View buffer** → the View Sink from `superchat-runtime-view-sink-function`; **no callback and any other buffer** → discarding Sink.  `:buffer-name` is never a routing key. |
| `:approval-function` / `superchat-authority-approval-function` | Unchanged in this project: the request-local `:approval-function` (a legacy Request key, or the `:runtime-extras` passthrough for envelopes) wins; otherwise the global custom `superchat-authority-approval-function` (default `superchat-authority-confirm`, minibuffer) is used for every submission.  Absence/error/quit → the same deny transition (§6.2).  Sink-based presenters are deferred (§13). |
| `superchat-extension-compose-request` | Stays a producer-side, pre-admission step; cannot inject delivery or bypass Authority. |
| `superchat-system-prompt-functions` / `superchat-build-prompt-functions` | A fixed `legacy-prompt-hooks` collector (owner `core`, S06) runs last in `collect-context`: it runs both chains on a scratch Turn in current list order and returns their net additions as fragments `:slot system :id "legacy-system"` and `:slot template :id "legacy-prompt"`. |
| `superchat-post-turn-functions`, `superchat-run-completed-functions`, `superchat-turn-completed-functions` | Unchanged timing (`superchat-runtime.el:1061-1092`, `887-899`): post-turn before the terminal seal; completion hooks after seal and after the terminal Sink callbacks settle. |
| `superchat-register-tool` | Unchanged public spec (`superchat-tools.el:1776-1826`); its registered effect resolver is what makes a tool name "Authority-backed" (§4.2). |
| `superchat-authority-register-policy` | Unchanged, tighten-only (`superchat-authority.el:94-114`). |

S14 updates `docs/plugin-guide.md`, `docs/architecture.md` and the Telega plugin.

## 3. Sink

### 3.1 Representation (`superchat-sink.el`, new in S01)

```elisp
(cl-defstruct (superchat-sink (:constructor superchat-sink-create) (:copier nil))
  id        ; string; `superchat-sink-create' fills "sink-<8 hex>" when nil
  live-p    ; nil (always live) or (lambda () BOOL)
  stream    ; nil or (lambda (RUN CHUNK))     CHUNK = (:kind text|reasoning :text STRING)
  status    ; nil or (lambda (RUN STATUS))    STATUS = waiting|typing|calling-tool|ready|error|stopped|orphaned
  final     ; nil or (lambda (RUN RESULT))    RESULT  = (:text STRING :tape-id ID-or-nil)
  error     ; nil or (lambda (RUN FAILURE))   FAILURE = (:status failed|timed-out|audit-incomplete|cancelled :text STRING :reason STRING-or-nil :tape-id ID-or-nil)
  orphaned) ; nil or (lambda (RUN REASON))    REASON = sink-lost
```

`:tape-id` in RESULT and FAILURE is the storage ID today's
`superchat-runtime--deliver` receives, unchanged: the `response.completed`
storage ID when that row was appended, else for `cancelled` the
`run.cancelled` storage ID (`superchat-runtime.el:1299-1303`), else nil.

`RUN` is the callback projection built by `superchat-runtime-sink-run RUN-ID`:
exactly the keys `:run-id :turn-id :session-id :surface :status :parent-run-id
:started-at :completed-at :error :pending-steer-count :sink-id`, taken from
`superchat-runtime--public-status` (`superchat-runtime.el:240-271`) and passed
through `copy-tree` (pure symbols, strings, numbers, lists only).  It contains
no buffer, marker, function, timer or process; the only opaque live objects a
Sink ever holds are the ones its own constructor captured (its buffer, overlay
or lambdas).  Payload plists are built fresh per call.  A Sink never receives
the registry entry.

### 3.2 Liveness and guarded emission

`superchat-sink-live-p SINK`: no predicate → t; predicate → its value;
predicate signals `error` → nil and one diagnostic is recorded.  Core checks
liveness at admission (dead → `superchat-envelope-invalid` / legacy: Runtime
error before any Tape row), before every active emission, and once immediately
before sealing.

`superchat-sink-emit SINK SLOT RUN PAYLOAD` → `(:sink-id ID :callback SLOT :settled-p BOOL :condition ERR-or-nil)`.
Missing callback → settled discard.  `error` is caught and recorded; `quit`
propagates from active emissions (Emacs interrupt semantics; the existing gate
`runtime/runtime-submit-does-not-swallow-render-stream-quit`,
`test/test-runtime.el:1125-1150`, stays valid).  Terminal emissions use the
capture-and-re-signal rule of §3.3; `superchat-sink-emit` takes a keyword
`:capture-quit t` for that call site and then returns the quit condition in
`:condition` instead of propagating.

### 3.3 Order and terminal rules

| Point | Rule |
|---|---|
| Active emissions | `:status waiting` after `run.started`; `:stream` zero or more; `:status typing` on first text; `:status calling-tool` per tool round.  Only while the Run is `running-model`; late chunks are dropped. |
| Pre-seal target loss | Liveness false before sealing wins: cancel provider work, seal as cancelled with `:reason sink-lost`, emit `:status orphaned` then `:orphaned` exactly once; never `:status stopped`.  **Applies only to a Sink that declares `:live-p`** (envelope Sinks such as the inline overlay and rewrite preview).  Legacy-bridge Sinks (the `:delivery-function`/`:stream-function` wrapper and the chat View Sink) declare no `:live-p`: as today, a Run whose buffer or View died keeps running and records its durable result (`superchat-runtime.el:1044-1047`: "Runtime keeps the durable result even when a View was cleared while the Run was in flight"); their callbacks check the buffer/View themselves and do nothing when it is gone (leader ruling R3-L7, after S01). |
| Terminal sequence | seal (§7.2) → terminal `:status` (advisory, guarded separately) → exactly one payload callback (`:final`, `:error` or `:orphaned`) attempted even if the status callback failed → completion hooks → quiesce → queue dispatch-next (§7.5). |
| Terminal `quit` | A `quit` from the terminal status callback is captured (not propagated immediately); the payload callback is then attempted inside `unwind-protect`; the aggregate receipt is appended if Tape allows; finally the **original** quit is re-signalled.  A `quit` from the payload callback is handled the same way (receipt, then re-signal).  Active (non-terminal) emissions keep plain propagation. |
| Terminal delivery failure | One aggregate Tape annotation `delivery.failed` (payload `:sink-id :conditions LIST` where each condition is `(:callback SLOT :condition PRINTED-STRING)`), appended after `turn.completed`; no retry, no reroute, no second transition.  A blocking adapter receives the first condition. |

### 3.4 Built-in Sinks

| Sink | Owner file | Slice |
|---|---|---:|
| Chat View: streaming insert, status line, stopped marker, final render, audit-incomplete notice (§7.2) | `superchat-render.el` (`superchat-render-view-sink BUFFER SESSION-ID OWNER`) | S02 |
| Discarding: every slot nil | `superchat-sink.el` (`superchat-sink-discard`) | S01 |
| String (blocking): collects `:final`/`:error`/`:orphaned` into a cell read by `superchat-runtime-submit-blocking` | `superchat-runtime.el` | S01 (adapter unchanged: `superchat-runtime.el:2192-2226` keeps polling its own cell) |
| Callback: wraps producer lambdas | `superchat-sink.el` (`superchat-sink-callback &key final error stream`) | S01 |
| Inline overlay | `superchat-inline.el` | S18 |
| Rewrite preview | `superchat-rewrite.el` | S17 |

## 4. Fixed driver and stages

`superchat-pipeline-submit ENVELOPE` (`superchat-pipeline.el`, S03) runs stages
1–6, then hands the prepared prompt to Runtime (stage 7); Runtime runs stages
8–10 inside its terminal path.  Stage order is a constant; plugins compete only
inside a stage.

| # | Stage / kind | Inputs → result | Absent / error | Journeys |
|---:|---|---|---|---|
| 1 | `resolve-session` / first-result | envelope → session-id string | absent: explicit `:session-id`, else `(format "ephemeral-%s" request-id)`; error: recorded, ephemeral | C1 C15 B1–B3 |
| 2 | `admit` / first-result | session-id, envelope, snapshot `(:active-run-id :pending-count)` → `(:action process\|queue\|steer\|drop :reason STRING :target-run-id STRING-or-nil)` | absent: `process`; error: `drop`, recorded, no Run | C12 D7 |
| 3 | `load-state` / collect | envelope, session-id → list of `(:owner SYM :delta OWNER-PREFIXED-PLIST)` merged oldest-first; a delta with a foreign prefix is dropped and recorded | absent: `(:runtime-authority-mode :runtime-authority-policy …)` only | C5 C16 |
| 4 | `select-tools` / first-result | envelope, state → `none` or list of tool names | absent: surface baseline (§4.2); error: `none`, recorded | C2 C8 D14 |
| 5 | `collect-context` / collect | envelope, state, tools → fragments (§4.3) | absent: content fragment only; a collector error omits its fragments and records; a `:required t` collector error is terminal | C5 C6 C14 B1–B4 |
| 6 | `assemble-prompt` / core-only | fragments, budget → `(:system STRING :messages ((:role user :content STRING)) :receipt PLIST)` | over-budget hard set → terminal `prompt-over-budget`, no provider call | C5 D3 |
| 7 | `run-model` / core-only | prepared prompt, tools, Run → outcome | `superchat--llm-generate-answer` stays the loop (§5) | all |
| 8 | `save-state` / collect then core seal | outcome, state → contributor receipts `(:owner SYM :receipts LIST)`; then the non-replaceable seal appends terminal facts and judges Closure | contributor error → seal `audit-incomplete` | C13 C15 |
| 9 | `render` / collect | sealed result, state → list of `(:renderer-id SYM :payload PLIST)` | absent: empty; error isolated | C11 |
| 10 | `dispatch` / core-only | sealed result, render deltas, exact Sink → emission records (§3.2) | delivery failure = `delivery.failed` | C2 C3 D11 |

Side stages: `on-error` (observer `(stage condition envelope)`, own errors swallowed),
`register-command`, `register-surface` (load-time; duplicate names rejected).

**Which stages are live after which slice** (ruling 4): after S03 all thirteen
stages are defined and `superchat-pipeline-submit` works for envelope callers,
with core defaults for 1–6 and the legacy Runtime path for 7–10; no built-in
producer uses it yet.  S05 registers the chat `admit` implementation and the
dispatcher calls `superchat-stage-call 'admit` directly.  S06 registers the
collectors and the dispatcher calls `superchat-prompt-collect` /
`superchat-prompt-assemble` directly.  S09 registers `select-tools` and
`superchat--llm-generate-answer` calls `superchat-select-tools` directly.  S10
switches the chat dispatcher to `superchat-pipeline-submit`; S12–S14 move the
other producers.  Until then `superchat-stage-call` may be invoked by the
phase-local caller named in the slice, not only by the driver.

### 4.1 Registry (`superchat-stage.el`, S03)

| API | Contract |
|---|---|
| `superchat-stage--define NAME &key kind validator core-only` | Internal; the thirteen stages are defined at load; unknown names cannot be defined by plugins. |
| `superchat-stage-add NAME ID FUNCTION &key owner required` | ID unique per stage; `owner` required symbol; `required` (boolean, `collect` stages only) marks a contributor whose `error` is terminal for the Turn; rejects unknown stage, core-only stage (unless owner `core`), duplicate ID, non-function, `required` on a non-collect stage.  Later registration runs first.  The registration record is `(:id ID :owner OWNER :function FN :required BOOL)`. |
| `superchat-stage-remove NAME ID` | Removes one implementation. |
| `superchat-stage-call NAME &rest ARGS` | first-result: first non-nil valid result; collect: every valid non-nil result in invocation order; fold: accumulator threading, nil = identity, `(:finish V)` short-circuits; core-only: the single core implementation.  An implementation `error` is recorded in `superchat-stage-faults` (bounded to 100) and skipped; validator failure counts as nil and is recorded. |
| `superchat-stage-report` | List of `(:stage NAME :kind KIND :implementations ((:id :owner :required) …))` plus the fault list. |

### 4.2 Tool selection

A **tool descriptor** is a tool name string with a registered Authority effect
resolver (`superchat-authority-effect-resolvers`, `superchat-authority.el:138`).
`superchat-select-tools ENVELOPE STATE` (S09):

1. `:tools 'none` → `none`.  `:tools LIST` → LIST filtered to descriptors (dropped names recorded).
2. Otherwise `superchat-stage-call 'select-tools envelope state`; a raw `llm-tool`
   value or unregistered name in the result is invalid (dropped, recorded).
3. Otherwise the surface baseline (revision 4, after rulings R3-L4/L5):
   `chat`, `workflow`, `subagent`, `external` and plugin surfaces → the small
   set `("list_buffers" "read_buffer" "apropos" "edit_buffer" "eval_elisp")`
   (or the user's `superchat-llm-tool-names` list when it is a list), plus
   every tool a plugin registered through `superchat-register-tool` (loading
   the plugin is the user's opt-in — user decision 2026-09-11; Authority still
   gates every call), plus
   `"ask_user"` when the envelope carries an input operation
   (`:runtime-input-operation` in state) — this is today's
   `superchat-get-llm-tools` default, so non-chat producers that pass no
   `:tools` keep the tools they get today; `inline-ask` → the current value
   of `superchat-authority-unprompted-read-tools` filtered to registered
   descriptors (so inline ask never prompts by default and follows the user's
   own list); `inline-rewrite`, `org-babel`, `magit`, `skills`, `compaction`
   → `none`.
4. `superchat-llm-tools-enabled` nil → `none` for every baseline (today's
   meaning); any non-nil value (`on-demand`, `always`, `t`) keeps the baseline
   (approved decision 3).  The keyword heuristic
   `superchat--should-attach-tools-p` (`superchat-llm.el` ~1365) is deleted
   (D14); `on-demand` is accepted for compatibility and documented as
   equivalent to `always`.

`superchat--collect-llm-tools` becomes the materializer (names → wrapped
`llm-tool` structs) and makes no decision.  Every attempt records
`:runtime-selected-tools` in state and the reason (`explicit`, `stage:<id>`,
`baseline`).

### 4.3 Fragments and assembly (S06)

Fragment: `(:owner SYM :id STRING :slot SYM :text STRING :priority INT :budget-class hard|soft :required BOOL :refit FN-or-nil :source LIST)`.

| Slot (rank) | Goes to | Today's producer |
|---|---|---|
| `system` (0), `instruction` (1) | `:system`, joined by `"\n\n"` | `superchat-system-prompt-functions` chain, `:system-prompt` |
| `attachment` (2) | user message | `#file` inline (`superchat-prompt-hooks.el:164-265`), dired/embark text files (N5), inline-ask reference block |
| `context` (3) | user message | region/defun header block (`superchat.el:3370-3406`) |
| `memory` (4) | user message | `--memory-context` (`:296-303`) |
| `history` (5) | user message | `--conversation-history` (`:338-379`) |
| `experience` (6), `plan` (7) | user message | `--experience-context`, `--active-plan` |
| `template` (8) | user message | `--template-substitution` (`:267-294`), user content |
| `continuation` (9) | user message | steer text, final-answer instruction (appended by the loop, not the assembler) |

Total order: slot rank ascending, `:priority` descending, then `:owner`,
`:id` lexicographic.  Duplicate global `:id` or unknown slot → terminal
`prompt-invalid`.  Rendering grammar: the user message is the chosen fragment
texts in total order joined by `"\n\n"`; no header is added by the assembler
(collectors keep their own headers).  Budget unit = characters of that user
message; budget source = `superchat--llm-context-budget-chars`
(`superchat-llm.el:624-674`; default 40000).  System text, tool schemas and
media are not counted (they are handled by llm.el's preflight,
`superchat-llm.el:2604-2631`).

Algorithm: reserve every hard fragment; if their total exceeds the budget,
return terminal `prompt-over-budget` without truncating.  Then fill soft
fragments in total order: a soft fragment with `:refit` is re-rendered once by
`(funcall refit REMAINING-CHARS)` (history uses this to call
`superchat-view-assemble` with the real remaining budget); a soft fragment
without `:refit` that does not fit is truncated to a prefix plus
`"…[truncated]"` when at least 80 characters remain, otherwise omitted; later
soft fragments are then omitted.  Receipt:
`(:budget N :hard N :used N :chosen IDS :omitted IDS :truncated IDS :refit IDS)`.
`:required t` requires `:budget-class hard`.  A required collector is declared
with `superchat-stage-add … :required t` (§4.1); if it signals, assembly is
skipped and the Run seals `failed` with reason `context-required-failed`.

**Legacy bridge algorithm** (`legacy-prompt-hooks` collector, owner `core`).
Scratch Turn = `(superchat-turn-new CONTENT SESSION-ID)` with `clean-input` set
to CONTENT-WITHOUT-#REF, `context-files` set to the envelope media list,
`system-prompt` = `""`, `prompt` = `""`, `state` = `(:legacy-bridge t)`; no
history, memory or template is pre-filled.  Run the two chains with
`superchat-core--run-hook-chain` in their current list order.  Extraction: the
`system-prompt` string after the chains, if non-empty, becomes fragment
`:slot system :id "legacy-system" :budget-class soft :priority -100`; the
`prompt` string after the chains, if non-empty, becomes fragment
`:slot template :id "legacy-prompt" :budget-class soft :priority -100`.
Because the scratch Turn starts empty and the built-in hooks are unregistered
(S06b), everything a third-party hook prepends or appends is by construction
"net addition"; a third-party hook that re-renders history would duplicate it
— that is accepted and documented as the reason to migrate to a collector.

## 5. `run-model`

`superchat--llm-generate-answer` (`superchat-llm.el:1791-3014`) stays the loop.
No split into attempt/loop is required by any journey; it is deferred (§13).

| Operation | Contract |
|---|---|
| drain steers | Immediately before **every** provider attempt, `(superchat-runtime-drain-steers RUN-ID)` returns and clears the FIFO snapshot; each text is appended as a user message.  The observer event `drain-steers` is removed. |
| drain port | `superchat-llm` stays loadable and runnable without Runtime.  Runtime binds the request-local port `superchat--llm-drain-steers-function` (a `defvar`, nil by default, bound in the same `let` as `superchat--llm-runtime-observer`, `superchat-runtime.el:1831-2101`) to `(lambda () (superchat-runtime-drain-steers RUN-ID))`; the loop calls it only when non-nil, otherwise the steer list is empty.  `superchat-runtime-pending-steer-count` is reached through a second port `superchat--llm-pending-steer-count-function` (nil → 0). |
| completion boundary | When a provider result is final text (no tool round) and the pending-count port returns > 0, the loop does not finalize: it increments `tool-rounds` (the same counter the tool-round branch increments, bounded by `superchat-llm-max-tool-rounds`), drains, and sends another attempt with reason `tool-followup`, subject to cancellation and deadline.  When the round limit is reached the loop finalizes; Runtime records the remaining steers by `run.steer-undelivered` (`superchat-runtime.el:711-733`, unchanged). |
| provider attempt | Publishes `:status waiting`; records `(:model :streaming :streaming-tool-use :capability-source)` in the attempt details already sent with `attempt-started` (`superchat-llm.el:2636`). |
| tool round | Publishes `:status calling-tool`; every call enters Authority (§6). |
| context overflow | `superchat-llm-context-budget-exceeded-functions` (`superchat-llm.el:253`) is the recovery seam; S19 makes compaction answer it by default (C14).  Recovery compacts only the overflowing Run's own Session (never another View's), at most once per Run.  An `error` anywhere in compaction → Anchor → recovery-View rebuild → retry handoff finalizes the original Run `failed` with a visible message; it is never left open.  A `quit` there finalizes the original Run the same way and is then re-signalled (the terminal-`quit` rule of §3.3: finish the durable work, then re-signal the original quit). |

### 5.1 Streaming capabilities (S08)

```elisp
(superchat-llm-set-model-capabilities MODEL '(:streaming t :streaming-tool-use t))
```

stores into `superchat-llm-model-capability-overrides` (alist keyed by model
string).  `superchat-llm--resolve-streaming BACKEND MODEL TOOLS-P` →
`(:streaming BOOL :streaming-tool-use BOOL :source SYMBOL)` with precedence:

1. `override` — exact per-model user declaration;
2. `provider` — `llm-capabilities` contains `streaming` / `streaming-tool-use`;
3. `adapter` — provider-type table: `llm-openai-compatible` (exact type) →
   both `t` for any model, because llm 0.31.1's chat-completions SSE decoder
   collects tool calls (the reason recorded at `superchat-llm.el:1922-1926`);
   every other type → no entry;
4. `fallback` — both `nil`.

The precedence is evaluated per boolean: an `override` entry decides both; a
`provider` list that contains a capability decides that capability; a missing
capability falls to `adapter`, then `fallback`; `:source` names the level that
decided `:streaming-tool-use` when tools are attached, else the level that
decided `:streaming`.  A tools-and-streaming attempt requires both booleans; a
tool-free attempt requires `:streaming`.  The master switch
`superchat-llm-streaming` (`superchat-llm.el:183`) is applied **after**
resolution and is recorded separately as `:streaming-enabled BOOL` in the
attempt details; it is not a capability source.  No extra provider request is
ever made.  The result and source are recorded per attempt.

## 6. Authority

Every model tool call runs `validate → effects → policy → approve → audit → run → after-tool-call`
inside `superchat-tool-execute` (`superchat-authority.el:1427-1710`), unchanged
in shape.

### 6.1 Baseline, modes, reads (S07)

`superchat-authority--effective-decision` (`superchat-authority.el:568-644`)
gains two branches, inserted after the unattended/policy/legacy deny-and-ask
checks (`:605-613`) and **before** every widening branch (`explore` `:616`,
legacy `allow` `:619`, `execute` `:622`, session allowlist `:631`), in this
order:

| # | Condition | Decision |
|---|---|---|
| 1 | every effect is `local-read` and (any effect is sensitive, or `superchat-authority-prompt-for-all-reads` is non-nil) | `ask` — no later branch (legacy allow, session allowlist, execute mode) can relax it |
| 2 | every effect is `local-read` **and the tool name is in `superchat-authority-unprompted-read-tools`** (and branch 1 did not match) | `allow` (still audited: `tool.requested`, `tool.execution.started`, `tool.completed`) |

**Unprompted reads are a user-owned allow-list of tool names, not a class
(revision 3, leader rulings R3-L4 and R3-L5 after the S07 reviews).**
`superchat-authority-unprompted-read-tools` (defcustom, list of tool-name
strings) defaults to exactly the literal list of user decision 2 ("read
buffer, list buffers, apropos/docs"): `("read_buffer" "list_buffers" "apropos"
"describe-function")`.  Branch 2 applies only when the tool name is in that
list **and** every effect the resolver returned is `local-read`; the decision
is Authority's, from the tool name it already receives — nothing a resolver,
`superchat-register-tool` caller, plugin or MCP declaration puts into an effect
plist can grant it.  Every other `local-read` keeps exactly its pre-S07
decision (it falls through to the later branches).  Deliberately **not** in the
default list (they keep asking in `ask` mode): `eval_elisp` even when its
classifier proves the expression read-only (it can call `symbol-value` /
`buffer-substring-no-properties`), `describe-variable` (returns the variable's
value), `sql` (whole database, every Session), history/memory tools, Workspace
and team reads, LSP reads, and every MCP or plugin tool.  Widening the list is
the user's decision, one tool name at a time, through this defcustom.

Sensitivity is judged at decision time from the buffer's current name and file
truename.  Pre-tool hooks (`superchat-authority.el:1451-1506`) are trusted
in-process code: a hook that renames or kills buffers between the decision and
the native call can change what is read, exactly as it could after an approved
prompt.  The sensitive-buffer list guards against the *model* choosing a
sensitive buffer; it is not a boundary against installed Elisp, which already
has full access.

Scope cases for a `local-read` effect on resource `buffer`: **absent** `:scope`
(`list_buffers`) → sensitive iff any live buffer matches; **string** `:scope`
naming a live buffer → match its name and file truename; **string** naming no
live buffer, or a **non-string** scope → treated as sensitive (→ `ask`).

| Mode (`superchat-authority-mode`, `superchat.el:963-968`) | Non-sensitive reads | Writes / execution |
|---|---|---|
| `explore` | allow | deny |
| `ask` (default) | allow | ask |
| `execute` | allow | allow only when all effects ∈ `local-write process-exec runtime-exec` (existing `:622-628`) |

`superchat-authority-sensitive-buffers` (defcustom, list of regexps, nil) and
`superchat-authority-prompt-for-all-reads` (defcustom, nil).  Matching uses
`(buffer-name . file-truename-or-nil)`; `list_buffers`'s approval preview never
prints a sensitive name.

### 6.1a Tool execution requires a Run (leader rulings R3-L6 / R3-L6b)

Every tool execution belongs to exactly one Run.  This is enforced by S03b's
append-time identity check, not by a separate front-door test:
`superchat-tool-execute` appends `tool.requested` (`superchat-authority.el`
~1693) before any approval, block/deny handling or native call, through
`superchat-authority--append-event`, which does not catch errors; a context
without non-empty `:run-id` and `:turn-id` therefore signals
`superchat-tape-invalid-event` (missing Run/Turn identity) at that append, and
nothing else happens — no approval prompt, no native call, no Tape row.
Rationale: Authority's events are `run-scoped` (§8); an unowned tool event is
the D2 class of trace poisoning.  In production the only path that builds tool
wrappers is the model call, whose only caller is Runtime
(`superchat-runtime.el:2123`, guarded by the single-caller test), so this
withdraws only the "nil / session-only legacy context" branch of
`superchat--agent-wrap-function` (`superchat-agent-loop.el:397-474`); the
tests `output-context/capture-once-and-nil-legacy-compatibility` and the
`session-only` case of `output-context/claimed-failures-preserve-effects-and-b-view`
assert the refusal.  It is not a §2.3 compatibility bridge.

R3-L6b (after the first implementation attempt): an explicit front-door check
was tried and withdrawn — it duplicated the append-time rule and broke ~68
existing tests that exercise Authority decision logic with the Tape append
stubbed (`authority-test--capture-events` and the agent-loop fixtures).  Those
tests model contexts without a Run and never reach the real append; they stay
as they are (decision-logic tests), and new tests follow the seam rule.

### 6.2 Approval settlement

Approval stays synchronous in this project (the presenter returns a truth
value; C9/C10/C17 pass this way; no journey needs approval in an overlay).
States, per request record:

```text
pending → decided-allow | decided-deny      (presenter result, quit, timeout, absence)
        → audited | audit-failed           (approval.decided append)
```

Native code runs only on `decided-allow + audited`.  `audit-failed` returns
the model-readable refusal `"[Tool call blocked because audit failed: …]"`
once and appends nothing further (existing handler `:1693-1710`).  Quit is a
recorded denial (00c, `:1647-1665`).  The presenter is the approval function
chosen by §2.3 (request-local, else the global custom).

## 7. Run lifecycle

### 7.1 Execution primitive (replaces "locks")

| Primitive | Rule |
|---|---|
| `:terminalizing` flag | The first action of `superchat-runtime--finish-body` (`superchat-runtime.el:1233-1321`) sets it; every public operation that would create new work on the Run (`steer`, child attach, second `cancel`) checks `(or :terminalizing terminal-p)` first and is rejected without waiting. |
| No callbacks inside a decision | A decision (enqueue, drain, attach, snapshot children) is plain sequential code that does not call Sinks, providers, hooks or `accept-process-output`. |
| Timer-0 deferral | Work that must happen after the current callback chain unwinds (queue `dispatch-next`, cascade cancel of children) is scheduled with `(run-at-time 0 nil …)`.  Timers fire in order of scheduling. |
| Tape append failure | Inside a decision, if the required append signals, the in-memory change is rolled back and the operation returns `(:accepted nil :reason tape-failed)`; no compensating fact is invented. |

### 7.2 Terminal mapping (exhaustive)

| Internal outcome | Public `:status` | Tape (in order) | Sink |
|---|---|---|---|
| `completed` | `ready` | `response.completed`, `run.completed`, `turn.completed` | `:status ready`, `:final` |
| `failed` / `timed-out` | `error` | `response.completed` (with the partial or error text, today's rule), `run.failed :status failed\|timed-out`, `turn.completed` | `:status error`, `:error` |
| `audit-incomplete` | `error`, `:failure-code audit-incomplete` | `response.completed` retained if the model outcome was already appended; recovery pair `run.failed :status audit-incomplete`, `turn.completed` (`superchat-runtime.el:1138-1149`); when the response row exists, the separate `system` material `[Audit incomplete: …]` (topic `audit-incomplete`, `:1178-1187`, unchanged) | `:status error`, `:error (:status audit-incomplete :tape-id ID-or-nil …)`; the Chat View renders the result and then, only when `:tape-id` is non-nil, the persistent audit notice — today's Runtime-side render at `:1191-1201` moves into the View Sink |
| `cancelled` | `stopped` | `run.cancelled`, `turn.completed`; no `response.completed` (`:1247-1251`) | `:status stopped`, `:error (:status cancelled :text PARTIAL :reason "user")` |
| orphaned before seal | `orphaned` | `run.cancelled :reason sink-lost`, `turn.completed` | `:status orphaned`, `:orphaned` |
| terminal delivery failure | unchanged | `delivery.failed` after `turn.completed` | — |

`response.completed` rule used by the seal: appended for every outcome except
`cancelled` and orphaned (`superchat-runtime.el:1247-1251`, unchanged); for
`audit-incomplete` it is present only when the model outcome had already been
appended before the audit failure.  Sealing obtains the Closure
judgment (`--guard-terminal`, `:901-943`) before any terminal status.  Quiesce
(`:2530-2564`) runs once after hooks.  `superchat-runtime-submit-blocking`
returns after the payload callback settles.

### 7.3 Steering (S04)

| API | Contract |
|---|---|
| `superchat-runtime-steer RUN-ID TEXT` (exists, `:676-698`) | Return shape is unchanged from today: the `run.steered` Tape record `(:event-id … :storage-id …)` on acceptance, **nil** on rejection (`superchat-resume` `superchat.el:939-943` and `control/…` tests already rely on nil = not active).  Rejected when the Run is unknown, `:terminalizing`, or terminal: appends `run.steer-undelivered` (`:count 1`) when an entry exists, then returns nil.  Otherwise: push the text onto `:steer-queue`, append `run.steered`; if the append signals, pop the text again and return nil (§7.1). |
| `superchat-runtime-drain-steers RUN-ID` (public rename of `--drain-steers`) | Returns and clears the FIFO snapshot. |
| `superchat-runtime-pending-steer-count RUN-ID` | Size without consuming. |

Gates: a steer after a tool round reaches the next call; a steer at a plain
completion boundary causes another call; a steer racing terminalization yields
exactly one `run.steer-undelivered`.

### 7.4 Parent index and cascade (S11)

| Rule | Contract |
|---|---|
| Attach | With non-nil `:parent-run-id`: parent must exist and be neither terminal nor `:terminalizing`; else submission is refused before any Tape row with `(error "Parent Run unavailable: %s")`.  On success the child id is pushed onto the parent entry's `:children`. |
| Terminalize | `--finish-body` sets `:terminalizing`, snapshots `:children`, seals, dispatches, then schedules `(run-at-time 0 …)` to cancel each still-live child with response `"[Cancelled with parent Run]"`; each child's own terminalization schedules its children the same way.  Guarantee: **eventual recursive cancellation of every descendant, with no guard held during any callback**; no ordering among descendants is promised. |
| Projection | `:parent-run-id` stays in public status; `:children` is exposed as `:child-run-ids`. |

### 7.5 In-process follow-up queue (S05)

Core owns `superchat-runtime--session-queues` (hash: session-id → FIFO of
`(:queue-id STRING :request PLIST-WITH-LIVE-SINK-OR-BUFFER :enqueued-at TIME)`).
Nothing is persisted; nothing revives after restart.

| API / event | Contract |
|---|---|
| `superchat-runtime-enqueue-follow-up SESSION-ID REQUEST` | Appends `queue.queued` (payload `:queue-id :sequence`); returns the queue-id string on success.  If the `queue.queued` append fails, nothing is enqueued, the sequence is not advanced, and it returns `(:accepted nil :reason tape-failed)` — a caller must treat only a string as acceptance (S05a-fix-1; S05b keeps the draft on rejection). |
| `superchat-runtime-pending-follow-ups SESSION-ID` | Count. |
| `superchat-runtime-clear-follow-ups SESSION-ID REASON` | Appends `queue.dropped :reason` per item. |
| `dispatch-next` | Scheduled by quiesce with timer-0 for the Run's session.  If the session still has an active Run, return.  Pop the head; if its target (Sink or legacy buffer) is dead → `queue.dropped :reason sink-dead`, continue; else submit it; success appends `queue.dispatched (:queue-id :run-id)`; a submit error appends `queue.dropped :reason submit-error` and continues. |
| Emacs exit / View kill | `kill-emacs-hook` and the chat View's kill hook call `clear-follow-ups` with `emacs-exit` / `view-closed`. |

Admit action table: `process` allocates a Run; `steer` appends `run.steered`
on the target and allocates none; `queue` calls `enqueue-follow-up` and returns
`(:status queued :queue-id ID)`; `drop` appends `admission.dropped :reason`
and returns `(:status dropped :reason R)`.  No Sink callback for the last three.

## 8. Tape event types (S03)

`superchat-tape-event-type-specs` (defconst in `superchat-tape.el`) is the one
static list: rows `(:type STRING :relevance run-scoped|session-annotation|configuration)`.
`superchat-closure.el` derives `superchat-closure--run-event-types` (relevance
`run-scoped`), `--known-irrelevant-event-types` (the rest) and
`--artifact-event-types` (the three `workspace.artifact.*`) from it; no second
inventory.  Ownership by phase: 00c's Closure lists (`superchat-closure.el:23-88`)
are the inventory until S03; from S03 the Tape list is authoritative and
Closure reads it; later slices add rows in the same edit as their first writer.

| Group | Types | Append-time validator |
|---|---|---|
| Run lifecycle (run-scoped) | `intent.received run.started response.completed run.completed run.failed run.cancelled turn.completed` | `:run-id` and `:turn-id` nonempty strings |
| Authority (run-scoped) | `tool.requested approval.requested approval.decided tool.execution.started tool.completed tool.failed` | same |
| Run annotations (run-scoped) | `decision.snapshot.recorded output.first.recorded run.steered run.steer-undelivered delivery.failed` | same; `delivery.failed` may follow `turn.completed` without changing Closure's terminal judgment |
| Configuration | `capability.activated topology.changed authority.policy.selected workspace.bound workspace.unbound extension.activated` | payload rule below |
| Session annotations | every type in `superchat-closure--known-irrelevant-event-types` (`superchat-closure.el:40-81`, including the dynamic family `subagent.started subagent.completed subagent.failed` derived from `superchat-closure--subagent-event-suffixes` `:32-37`, written by `superchat-subagent.el:510-525`) plus `queue.queued queue.dispatched queue.dropped admission.dropped edit.applied turn.superseded` | payload rule below |
| Artifacts | `workspace.artifact.added workspace.artifact.changed workspace.artifact.removed` | payload rule below |

Payload rule (all groups): the payload is nil, a plist, or an alist — exactly
what `superchat-tape--redact` (`superchat-tape.el:98-136`) already accepts;
current producers write both shapes (subagent/team writers use alists,
`superchat-subagent.el:453-491, 510-525`).  No producer migration is required.

`superchat-tape-append-event` (`superchat-tape.el:415-461`) signals
`superchat-tape-unknown-event-type` for an unlisted type and
`superchat-tape-invalid-event` for a failed group validator.
`superchat-tape-import-event` is the only path for unknown types: it stores
the row with `:historical-opaque t` in its meta; replay returns it as an
opaque annotation.  Closure rule for an opaque row: it is **skipped** only when
it carries an explicit `:run-id` different from the queried Run, or an explicit
`:turn-id` different from the Run's Turn once that Turn is known (from the query
or from an already interpreted event of the Run); every other opaque row —
absent identity, only a matching Turn, matching Run — makes the claim
`unknown` (`historical-opaque-in-scope`).  The static scan
`runtime/closure-knows-literal-production-tape-event-types`
(`test/test-runtime.el:811-878`) stays as a supplementary gate and additionally
asserts that the dynamic family is present in `superchat-tape-event-type-specs`.

## 9. State ownership

`load-state` output is Turn-local.  Keys are `:runtime-*` for core or
`:<owner>-*` for the registered owner.  Core reads only `:runtime-*`.  Session
mode and named policy are snapshotted at admission into
`:runtime-authority-mode` (from the chat View's `superchat-authority-mode`) and
`:runtime-authority-policy` (from `superchat--active-authority-policy`,
`superchat.el:1451-1452`); the chat surface sets both when it builds the
envelope (S10).  `superchat-stage-report` lists implementations and faults; it
does not track state-key writers at runtime (static tests do).

## 10. Gates (tests)

1. Exactly thirteen stages are defined; no production hook list is used as an ordered stage after S10.
2. Every observer event emitted by `superchat-llm.el` has a `pcase` branch in Runtime (static list comparison; `drain-steers` gone).
3. `superchat-tape-append-event` rejects an unknown local type; import accepts opaque unknowns.
4. Renderer output cannot name a Sink; terminal callback failure produces one `delivery.failed` and no second transition.
5. Steering gates of §7.3; parent attach vs cascade interleaving of §7.4.
6. Prompt gates: total order, duplicate-ID rejection, hard over-budget, soft truncation, refit, C5 `#path` cases.
7. Queue gates: per-session FIFO, dispatch after every terminal outcome, dead-target drop, clear on View kill.
8. `superchat-select-tools` is the sole decision; every selected name has an effect resolver.
9. Core files contain no `superchat-buffer-name` read, no direct buffer insertion, and no `fboundp`/`boundp` probe of a non-core symbol except a machine-readable allow-list `(symbol file reason removal-slice)`.

## 11. Change Set (S16)

`superchat-change-set-apply BUFFER EDITS &key session-id run-id source`
(`superchat-change-set.el`): EDITS = list of `(BEG END TEXT)` with positions or
markers; overlapping edits → error before any change; applied in one change
group (`prepare-change-group` / `undo-amalgamate-change-group`) so one `undo`
restores the buffer; on error the group is cancelled.  Appends `edit.applied`
(payload `:buffer :file :edits N :removed N :added N :source`) when
`session-id` is given.  Returns `(:applied N)`.  Inline rewrite accept, and the
chat `C-c C-a` apply, use it; the ediff path (D13) is deleted.

## 12. Migration map

| Current seam | Destination | Slice |
|---|---|---:|
| Runtime default delivery (`superchat-runtime.el:1028-1059`, `2102-2143`) | Sink, legacy adapter, view-sink function | S01 S02 |
| Steer/drain (`:676-733`) and observer `drain-steers` (`superchat-llm.el:2538-2544`) | Run methods, completion-boundary rule | S04 |
| "Draft retained" (`superchat-dispatcher.el:635-649`), `superchat--queued-follow-up` | admit + queue | S05 |
| Prompt hooks (`superchat-core.el:134-146`, `superchat-prompt-hooks.el:385-418`) | collectors + assembler + legacy bridge | S06 |
| Read policy (`superchat-authority.el:402-412`, `568-644`) | baseline + sensitive list | S07 |
| Streaming predicate (`superchat-llm.el:1927-1936`) | capability resolution | S08 |
| Tool gate (`superchat-llm.el:1328-1398`, `superchat.el:1248-1286`) | `superchat-select-tools` | S09 |
| Chat dispatcher submit (`superchat-dispatcher.el:459-551`) | envelope producer | S10 |
| Cancel (`superchat-runtime.el:1402-1432`) | parent index + cascade | S11 |
| Extension composition for all producers (D8) | `load-state`/`collect-context` contributions | S12 |
| Subagent, workflow, compaction, skills producers | envelopes | S13 |
| Telega, emacsclient, plugin docs | envelope adapter, `run_status`, docs | S14 |
| Rewrite (`superchat-rewrite.el`) | preview Sink + Change Set | S16 S17 |
| Inline ask (new) | `superchat-inline.el` | S18 |
| Compaction default (`superchat-compact.el:269-310`, `superchat.el:63-65`) | recovery on by default | S19 |
| Chat experience (C4 C11 C15 C16 C1) | chat plugin work | S20–S24 |
| Dead code and optional package | removal | S25–S27 |

## 13. Deferred (not needed by any journey or defect)

| Mechanism | Why deferred | If ever needed |
|---|---|---|
| Restart-durable queue (SQLite table, claims, rehydration) | Rejected by re-review §"Simplicity"; ruling 1 | Separate at-least-once design with idempotency and retention |
| Async approval continuation and any Sink-based approval presenter | Every journey passes with synchronous approval; B12 is optional; the Sink struct has no approval slot until this is approved | Make every provider wrapper async, add a presenter slot, route `DECIDE` through §6.2 states |
| Splitting `superchat--llm-generate-answer` into attempt + loop | Testability only; high risk for a 1224-line closure set | Do it after S10 with the journey suite green |
| Composite / fan-out Sinks | No producer needs two targets | Producer-created composite |
| Persistent "always allow" rules | Approved scope keeps per-call and per-session posture | Own Tape contract |
| Runtime state-key writer tracking in `superchat-stage-report` | Static tests suffice | — |
| `:media` beyond explicit file paths | Only images from `#file`/dired are used | — |
