# Decision: M3a — remove Closure and Claims, park Experience, following DeepSeek Harness

Status: accepted — user decisions on 2026-09-15 after the leader's DeepSeek Harness investigation; supersedes in part [the official-plugins decision](2026-09-12-experience-claims-closure-as-official-plugins.md); implementation state is in [the roadmap](../../roadmap.md#milestones)

## Problem

The [official-plugins decision](2026-09-12-experience-claims-closure-as-official-plugins.md) kept Experience, Claims and Closure in the default set as official plugins and left undesigned how a Run seal is proved without Closure. When a design built on it reached the user, they changed the yardstick: the goal is migrating the [DeepSeek Harness blueprint](2026-09-12-deepseek-harness-blueprint.md) (dsh, `~/Documents/reference-code/deepseek-harness`, commit `c389f96`), so a superchat subsystem stays only where dsh has a corresponding structure.

Measured on superchat commit `3a9cb97f`:

- **Closure** (`superchat-closure.el`) is a pure judge over a Session's Tape, answering `trace-well-formed`, `pending-effects`, `pending-approvals`, `ready-terminal`, `durable-run-terminal` and `durable-turn-terminal` with verdicts `proved`, `open`, `invalid` or `unknown`.
  - Runtime calls it directly in two places, besides a shadow report that is off by default. The terminal guard `superchat-runtime--guard-terminal` runs before every Run terminal, cancelled or not, and signals `superchat-closure-terminal-not-ready` unless readiness is proved; the Run then seals `audit-incomplete` (`run.failed` and `turn.completed` with `:status audit-incomplete`). `superchat-runtime-reconcile-session` writes the `abandoned` pair for a Run without a terminal only when Closure answers `open`.
  - Nothing outside Closure tracks which tool Effects are entered and unsettled, so the guard is what makes a Run cancelled during a tool seal `audit-incomplete` with the Effect open.
  - Three consumers ask the `closure` Service first: subagent handoff (`superchat-subagent-handoff-decision`: `deliver`, `wait`, `manual-review`, `reject`), workflow resume (`superchat-workflow--resume-decision`: `advance`, `wait`, `manual-review`, `stop`) and cross-Run artifact delivery (`superchat-workspace-artifact-delivery-decision`: `deliver`, `wait`, `manual-review`, `reject`). Subagent and workflow answer `manual-review` only when the trace judgment is neither `valid` nor `invalid`. Experience evaluation, both when it is recorded (`superchat-experience-record-evaluation`) and when an evaluation is read back as evidence (`superchat-experience--evaluation-evidence`), also requires a proved, not audit-incomplete, Turn terminal.
  - The `driver` plugin (`superchat-pipeline.el`) injects `closure` without using it.
- **Claims** (`superchat-claims.el`) is two interactive commands, `superchat-claim-capture` and `superchat-claim-correct`, producing `claim.recorded` and `feedback.recorded`; it has no plugin or spine caller. Experience's relations and candidates must reference `claim.recorded` events.
- **Experience** (`superchat-experience.el`) is a learning loop over the Tape: evaluations, applicability relations, candidate dispositions and rankings, an applicability-graph projection, a review of open recovery needs reached as the chat command `/review` and as `M-x superchat-review`, a read-only `M-x superchat-experience-monitor`, a context collector in `superchat-prompt-hooks.el` (never activated, because nothing sets `:experience-selection`), a recovery view behind the `recovery` Service, and a `run-completed` hook.
- **Reconcile's flow** is reopening or re-rendering a saved Session whose Run was cut off when Emacs exited. The caller `superchat--reconcile-session-on-reopen` (`superchat.el`) skips it while the Session has a live process-local Run. Conversation history does not depend on it; only Closure's consumers read the `abandoned` seal.
- **History rows.** Tool rows never reach the model: `superchat-view--derive-message` builds history from `user` and `assistant` rows, the compaction Anchor and admitted injection material only.

Read in dsh and verified at the cited places:

- **No Closure counterpart.** `packages/core/agent-loop/src/agent.ts` ends every turn in `finally` blocks with no readiness check; on cancellation `packages/core/agent-loop/src/tool-calls.ts` lets started tool calls settle and records their results before the turn ends. On resume, `AgentLoop.resumeWith` (`packages/core/agent-loop/src/index.ts`) appends `interruptedTurnClosers` (`packages/core/session/src/repair.ts`): unknown-outcome tool results, `step/end` and `turn/end {kind: 'interrupted'}`, without judging the log. A subagent's outcome is folded from the child's own log (`foldConsumedWork`). Log invariant checkers are mounted only in the `sdk-minimal` bundle and in tests.
- **No Claims counterpart.** Only log-only, never model-visible `/feedback` text and per-message ratings exist (`packages/feedback/*`).
- **No Experience counterpart.** No evaluation of past runs, relations, rankings, failure-time experience injection, learning context contributor or review command exists; dsh's slash commands are `compact`, `export`, `feedback`, `goal`, `permission` and `plan`.

## Decision

The user decided on 2026-09-15:

1. **Claims is deleted.** "对应 deepseek harness 有无这个结构，无就删除" — dsh has none.
2. **Experience's two entry points are removed; the rest is kept.** On the entry points: "看看 deepseek harnees 里有无对应的东西，无就去掉". Asked about the remainder, the user chose "只去掉两个入口": the context collector and the review of recovery needs go, and Experience stays an official plugin in the default set with its recovery Service, projection and `run-completed` hook. Once Claims produces no `claim.recorded` events, Experience can record no relation or candidate.
3. **Closure is deleted and replaced by dsh's approach.** "删除，换成 DSH 做法": no judge; reconcile repairs on reopen without a judgment; consumers read the recorded terminal status; the `driver` plugin stops injecting `closure`.
4. **A Run cancelled while a tool Effect is still executing keeps superchat's rule.** "保留现在的立刻结束": the Run seals `audit-incomplete` at once and leaves the Effect open, and a genuine late result is still appended, as M3's acceptance and smoke cases R1 and R9 state. This differs from dsh, which waits for started tools to settle; Runtime therefore tracks open Effects itself.

This supersedes the official-plugins decision for Claims and Closure, which leave the default set and the code, and for Experience's entry points. It also supersedes the implementation consequences of [ADR 0001](../../adr/0001-tape-backed-epistemic-self-learning.md) that name Closure, the Claims module and Experience's context and review entry points; that ADR's epistemic-time, provenance and promotion constraints stay in force. Its rulings that Experience stays an official plugin in the default set and that the spine loads without it remain.

## Mechanism

These follow from the decisions; they are the leader's design within them.

### Sealing without a judge

- **Runtime tracks open Effects and Approvals per Run, in process.** Authority reports to Runtime, by execution identity, where it appends `tool.execution.started`, the Effect's settlement (`tool.completed` or `tool.failed`), `approval.requested` and `approval.decided` (`superchat-authority.el`).
- **At seal time,** a Run whose requested status is `completed`, `failed`, `timed-out` or `cancelled` seals `audit-incomplete` through the existing audit path when an Effect is entered and unsettled or an Approval is requested and undecided; otherwise it seals with its requested status. A required recording failure still turns `completed` into `audit-incomplete`, as the [M3 closing rulings](../implemented/2026-09-15-m3-closing-rulings.md) state. A late settlement after the seal appends its one matching fact without reopening the Run.
- **No other pre-seal check remains.** Closure's refusal of seals over `invalid` or `unknown` traces goes with it, as in dsh.
- `superchat-closure-terminal-guard-enabled` and `superchat-closure-shadow-enabled` (defcustoms in `superchat.el`, defvars in `superchat-runtime.el`), the `superchat-closure-terminal-not-ready` error, the closure observation ring with `superchat-runtime-closure-observations`, and the `closure-shadow` Event are removed.

### Repair on reopen

When a chat View opens or re-renders a saved Session that has no live process-local Run, every Run with `run.started` and no Run terminal receives the existing `abandoned` pair (`run.failed` and `turn.completed` with `:status abandoned`), without a judgment. The existing completion of a half-written `abandoned` pair, which adds only the missing `turn.completed`, stays. The live-Run check in `superchat--reconcile-session-on-reopen` is the remaining guard, as dsh's single-writer lease is. Two dsh closers are deliberately not copied:

- **No synthetic tool results.** dsh appends unknown-outcome tool results because its provider messages must pair tool calls. superchat's history does not carry tool rows, so a synthetic tool fact would record an outcome no process observed.
- **No `step.ended` for an open Step.** The [M3 execution main line decision](../implemented/2026-09-14-m3-execution-main-line-resolution.md) states that recovery never invents an end merely to produce a pair; an interrupted Step keeps its open bracket.

### Consumers read the recorded terminal

Subagent handoff, workflow resume and cross-Run artifact delivery read the Run terminal fact recorded for the Run in question from the Tape:

| Recorded Run terminal | Subagent handoff | Workflow resume | Artifact delivery |
|---|---|---|---|
| none yet | `wait` | `wait` | `wait` |
| `run.completed` with status `completed` | `deliver` | `advance` | artifact checks, then `deliver` |
| any other terminal, including `audit-incomplete` and `abandoned` | `reject` | `stop` | `reject` |

Subagent handoff and workflow resume no longer answer `manual-review`, because the trace judgment that produced it is gone. Artifact delivery keeps its own checks unchanged, including `manual-review` for an artifact whose provenance is unknown; "the artifact precedes the terminal" is read from Tape order. Experience evaluation keeps its identity, order and single-terminal checks, and requires the Run terminal to be `run.completed` with status `completed` and a `turn.completed` for the same Turn that is not `audit-incomplete`.

### Removals

- `superchat-closure.el`, the `closure` row in the default plugin set (`superchat-plugin.el`) and the `closure` Service definition (`superchat-service.el`); `driver` injects `(tape model-transport)`; `superchat-subagent.el` reads `superchat-tape-subagent-event-suffixes` directly instead of Closure's alias.
- Tape vocabulary only Closure reads: the `:closure` role tags with `superchat-tape-event-closure-role`, and the `:historical-opaque` marking on imported rows. Event type specifications stay, so existing Sessions containing any event type remain readable.
- `superchat-claims.el` and its autoloads. `claim.recorded` and `feedback.recorded` stay readable event types for existing Sessions.
- The `experience` context collector in `superchat-prompt-hooks.el`, the `experience` prompt slot only it used, and the `history` collector's copy of the Turn's `:experience-view` into the Context envelope. The request-time `:experience-view` that the recovery path sets in `superchat-llm.el`, and Authority's gate over it (`superchat-authority--experience-block-reason`), stay with the recovery view.
- The review of recovery needs in both forms, the chat command `/review` with its help entry and `M-x superchat-review`, since both run the same review. `M-x superchat-experience-monitor` is a read-only report the user did not name and stays.
- Current documents describe the result: the architecture map, domain vocabulary, request lifecycle protocol, plugin guide and root `AGENTS.md`. Implemented records that mention Closure are corrected as facts in the same change.

## Alternatives considered

**Keep Closure as an optional plugin under a Runtime-owned fallback.** The earlier design; not chosen, because dsh has no counterpart and the user chose dsh's approach.

**Wait for started tools to settle before ending a cancelled Run, as dsh does.** Not chosen by the user: it changes M3's accepted behaviour and can hang a Run when an Emacs-side external process never returns.

**Delete Experience entirely.** Not chosen by the user, who kept the remainder after removing its entry points.

**Keep Claims as a command module.** Rejected by the user's rule: dsh has no counterpart.

**Copy dsh's closers exactly on reopen.** Rejected for the two reasons under Repair on reopen: superchat's history has no tool rows to pair, and the M3 decision forbids inventing a Step end.

**Keep reconcile Closure-free but still gated on an open-trace judgment.** Rejected because the judgment is what is removed.

## Consequences

The roadmap's M3a entry is rewritten to these decisions and owns implementation state.

Smoke-architecture cases R1, R5, R7 and R9 call `superchat-closure-judge` directly, and `scripts/smoke.el` binds `superchat-closure-terminal-guard-enabled` for every case; the implementation rewrites those assertions against recorded facts, for example "the execution id has `tool.execution.started` and no settlement", and drops the binding. Any change to a canonical baseline file needs the user's explicit acceptance.

The M3 regression set gains leader-written tests for Runtime's Effect accounting, repair on reopen, the consumer table and the removals before implementation starts.

A Session reopened after this change receives `abandoned` seals for interrupted Runs that Closure left open when its judgment was `unknown`. A subagent or workflow step that previously waited for `manual-review` is rejected or stopped instead.
