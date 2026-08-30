# Status

updated: 2026-09-07

## Active SPECs

`session-editor-workspace` — `.specs/session-editor-workspace/`, `status:
confirmed`. All nine Slices are implemented, independently checked, `done`,
and evidence-linked: 01–02 `E-20260903-003`, 03 `E-20260904-001`, 04
`E-20260904-002`, 05 `E-20260904-003`, 06 `E-20260904-004`, 07
`E-20260904-005`, 08 `E-20260904-006`, and 09 `E-20260905-001`. Runtime's
legacy HostLease/editor Cleanup and legacy Request fields are retired;
EditorWorkspace is the sole editor ownership/close authority, while Runtime
retains exact borrowed-host admission, Run lifecycle, quiescence, atomic
publication, and at-most-once delivery. Repository-external consumers remain
unknown; the breaking release number is unspecified and publication is not
authorized.

The implementation is no longer `doing`, but the SPEC is not declared
formally closed: Blocker 1 leaves no doctrine-authorized terminal SPEC
transition. Preserve the confirmed SPEC state until that upstream workflow gap
is resolved through its own `plan`.

`mcp-authority-integration` — `.specs/mcp-authority-integration/`, `status: confirmed`.
Slices 01–04 are `done` and verified (`E-20260827-003`); the SPEC holds no other slice. An
earlier note here said a fifth slice `05-context-overflow-signal` was filed under it — that was
wrong: the finding lived in an untracked `docs/finding-context-overflow-signal.md`, which has since
been folded into `E-20260827-012` and removed.

Blocked from closing: see Blockers 2.

`tool-quit-accounting` — `.specs/tool-quit-accounting/`, `status: confirmed`.
Its single end-to-end Slice is implemented, independently checked, `done`, and
evidence-linked as `E-20260905-006`. K21 records phase-labelled interruption
accounting, frozen outcome publication before detached notifications, exact-Run
cancellation, and Runtime/Experience exclusion from automatic recovery-learning.
The interruption remains a Tape/Closure failure fact; Review Needs may still
report `missing-outcome` as a blocked completeness diagnostic rather than a
learning sample. Formal SPEC closure remains subject to Blocker 1.

## Blockers

1. **Nothing is authorised to close a SPEC.** `spec-agents` v4.1.0 requires `status: verified`
   once every slice is `done` (`bin/spec-agents:161`), but `verified` appears in **none** of the six
   `skills/*/SKILL.md`; its only occurrences in `WORKFLOW.md` (`:139`, `:155`) belong to the
   **KnowledgeItem** lifecycle, a different entity. `WORKFLOW.md:184-186` assigns SPEC writes to
   `capture`; `learn` set `verified` and then reverted it as an unauthorised write. Worse, the
   workflow model itself gives SPEC no terminal transition (`WORKFLOW.md:123`: `Change --plan-->
   Plan --capture--> SPEC --arrange--> Slice* --do--> Code`, inbound and outbound edges only), while
   every other entity has a lifecycle line. `spec-agents check-state` therefore reports 5 violations,
   4 of them the `spec_ref` format issue and 1 this. Detail: `E-20260827-007`. Needs `plan`; the fix
   is upstream, and a peer session working in the upstream repo has the evidence.

   Also unresolved upstream: the doctrine's mandatory reading directs a managed project to run
   `spec-agents` subcommands, but `bin/` is not installed — this machine only resolves the command
   through a symlink into the upstream checkout.

## Completed architecture-review goal

The 2026-09-05 P1–P8 architecture-review goal is complete for repository scope
under `E-20260905-013`; that Evidence contains the final per-item map and
verification limits.  No do/check action remains for this goal.  That final architecture batch left K26 unchanged; subsequent framework learn is
recorded below at K30.  The
separate upstream SPEC-terminal authority gap in Blocker 1 continues to limit
formal administrative closure without reopening completed implementations.

The three defects confirmed in `plan` on 2026-08-27 are **fixed and verified**
(2026-08-31, batches `fix-batch1`/`fix-batch1b`; reports and adversarial reviews
under `.scratch/arch-test-20260830/`):

| # | Defect (was) | Fix landed | Regression tests |
| --- | --- | --- | --- |
| 1 | wrapped tool call minted a Session id into a buffer it created | the two `--ensure-session-id` helpers only return an already-bound id; refusal paths deliver readable errors through the callback and render no half-record | `test-agent-wrapper-does-not-mint-session-id`, `test-compact-session-no-op-on-non-chat-buffer` (real SQLite/Authority) |
| 2 | `/command` parsed alongside `>skill` and was silently dropped | `/command` step guarded by `unless skill`; skill precedence explicit | `pipeline/skill-precedes-command`, `pipeline/plain-command-still-parses-without-skill` |
| 3 | widening the allowlist could put two same-named tools on one surface | request assembly dedups in authority order registry -> active-plan tool -> synthesis | `test-collect-llm-tools-deduplicates-registry-before-synthesis`, `test-collect-llm-tools-plan-wins-before-synthesis` |

A second approved batch (`fix-batch2`/`fix-batch2b`, plan round 2026-08-31) also landed:
`>>name` autoloads the workflow engine on first use (silent degradation to chat removed;
`Invalid workflow name:`/`No such workflow:` negative paths tested), command completion and
`/commands` now derive from the live dispatch sources, `(:reasoning ...)` stream payloads are
accepted quietly, the five orphaned test files joined `test/run-tests.el` (canonical suite is
now 1023 tests, 0 unexpected), and four display defects were fixed (`%%` escaping, invalid
title face, dead `superchat-label-face` removed, TTFT annotation survives finalize). A docs
batch synced both READMEs (first-run path incl. local OpenAI-compatible backend,
`superchat.db`/`memory` table names, echo-area feedback, v1.3.2).

Open items routed to the **next `plan` round** (none started):
1. F3 — mixed `>skill /command` input: the command-looking rest silently enters the skill
   prompt, and the dispatcher passes the raw `>`-prefixed input to `superchat-skills-invoke`.
2. Workflow name grammar — `[a-zA-Z0-9_-]+` rejects non-ASCII names; decide whether Chinese
   names become legal or the ASCII contract gets documented.
3. `superchat-compact--request` accepts a nil session and mints `nil/compaction-...`
   (private builder, no production caller reaches it with nil today).
4. P0 concurrency regression findings (2026-08-31, `.scratch/arch-test-20260830/p0-concurrency-report.md`):
   typing into the workflow's own View during a run drops text into the response area instead
   of forming a request; cancelling a Run with an approval still open terminates as
   `audit-incomplete` and prints the raw Closure guard error into the chat buffer; a
   post-cancel follow-up in the same Session still carries the cancelled request's prompt
   context (continuity design, needs an explicit decision). Core isolation all passed:
   no phantom Sessions, no cross-Run Tape pollution, late terminal callbacks rejected,
   tool dedup stable under consecutive real requests.
5. P0 compaction regression findings (2026-08-31, `.scratch/arch-test-20260830/p0-compaction-report.md`):
   the full real-model compact success path works (two typed anchors, append-only sources,
   logs, in-process `/expand`, restart rebuild, live no-candidate echo), and the Context View
   three-way selection rule plus the `%%` header fix were confirmed live. Open defects:
   the compact success message is inserted and then immediately deleted by
   `superchat--refresh-prompt` (chat buffer shows no trace); the `/expand` high-water mark is
   buffer-local only, so after a restart `/expand` reports "No anchor found to expand" and the
   raw-View switch is unreachable; `/compact-log N` ignores N and always opens the latest log;
   the compaction completion marker is brittle against real providers (no-marker / empty /
   timeout all end with no anchor and messages-only feedback).
6. **Closed (2026-08-31, `E-20260831-001`)**: the `audit-incomplete` root cause — a
   compaction Anchor misclassified as a Runtime envelope during replay poisoned every
   later Run's audit in the Session — is fixed (`fix-batch3`), and the follow-on
   adversarial rounds (batch3b–3f, luna implements / sol refutes) landed the
   response-region ownership redesign: Render is the single transcript authority with
   explicit `:current-run`/`:independent-turn` owners, bounded/unbounded regions,
   persistent notices and one reset shared by Session rebuild and `/clear`. Canonical
   is now 1039 tests / 0 unexpected; `KERNEL.md` is at **K6** (`E-20260831-002`,
   77 entries, strict refs 854). Approval-open cancellation ending `audit-incomplete`
   was adjudicated Kernel-conformant; the audit warning is deliberately
   prompt-visible (user ruling 2026-08-31).
7. **Campaign closed (2026-09-01).** The 2026-08-30 "make superchat as usable and
   good-looking as pi" campaign is complete: test round (26 findings, adversarially
   verified) → fix batches 1–5 with per-batch cross-model review (implementer luna,
   then sol from batch5c per user instruction; reviewer sol throughout). Evidence:
   `E-20260831-001/-002/-003/-004`, `E-20260901-001/-002`. Canonical grew 991 → 1067
   tests / 0 unexpected; `KERNEL.md` is at **K8**. Per-gap verdicts are tabulated in
   `.scratch/arch-test-20260830/batch5-review.md` §7 ("战役总评").

   **Open residuals** (next `plan` items, none started): status-band richness beyond
   current data sources (cost/token/jobs); large-skill timeout root cause and
   progress display (M10); Kernel→distribution seam extraction (M3, gate-controlled);
   low-severity UX leftovers (in-place retry/copy actions, separators/spinner, Tape
   browser polish, `/remember` echo placement, dead-endpoint address in errors,
   Emacs 31 obsolete-macro warnings); display-matrix test face (GUI/terminal themes,
   narrow windows, multi-frame); K9 candidates (Kernel #8 narrowing; back-edge seam).

## Kernel

`KERNEL.md` is at **`K35`** (2026-09-07). `E-20260907-001` records
the independently checked selected active-run cancel redirect edit and 72 citation
reanchors. Cancellation precedes the prefilled edit; accepted text replaces the
same visible draft without automatic sending. This does not extend the UI cancel
selection to every Runtime Run: crd's old/new probes found direct submit(origin)
and agent-run untracked; its report also identifies the act/request-plan boundary.
`E-20260906-009` retains the
ordinary visible-draft lifecycle; its Slice remains done/evidence-linked without
an invented SPEC terminal transition. Other command dispatch and legacy queued-value
compatibility readers remain unchanged. These bounded observations do not reopen
the prior completed goal.
Architecture-map retains its historical K33 source snapshot; no refresh is part
of this input-lifecycle learn. `E-20260906-008` records
the checked O1 exact timer acquisition repair, S1 user-accepted bounded no-change,
and architecture documentation verification with its fresh/historical boundaries.
The completed framework01–05 and independent refusal/deadline Slice chains are
retained in E001–E008 rather than active work here. Their confirmed SPEC files
are not assigned an invented administrative terminal transition; Blocker 1 remains.
E007/K32 public refusal once/original-condition semantics and successful async
swallowing remain unchanged. Early fixture curl spawn remains UNKNOWN; crm's
later guarded runs do not retroactively establish early transport isolation.
Architecture metadata follows the reviewed K33 bytes through its separate do
writer after knowledge publication; diagram and visual receipts are unchanged.
`E-20260906-005` records Slice04's
approved finite seven Closure observations and18 mechanical citation reanchors.
That K30 revision retained existing since fields and other semantics, including
delegated identity, from K29; its then-fresh strict1077/0/0 and authority form passed.
E-20260906-004 retains Slice03's sole call-llm citation relocation81→84;
E-20260906-003 retains Slice02 and its86 mechanical citation relocations.
Architecture-map source hashes identify local snapshot bytes, not public commit attestation.
`E-20260906-001` promotes the approved
exact View rollover, explicit owner/prompt/final/context contracts;
`E-20260906-002` records the separate body-only reasoning correction. The old
terminal-first inactive-call audit contract is replaced by05 non-admission;
already admitted cancellation accounting remains. Mechanical citations are
re-anchored to actual constructs; learn runs strict and Authority-map gates.
Prior revisions remain evidence-linked: K18's retirement and K20's Workflow
editing contracts remain unchanged; `E-20260905-006` records the verified Tool
Quit accounting, exact-Run interruption, and recovery-learning exclusion
contract. `E-20260905-007` records the evidence-backed no-change conclusion for
P1 asynchronous exact-View callback targeting; `E-20260905-011` records P3's
fail-closed ordinary tool admission and breaking external migration boundary;
and `E-20260905-009` records the
bounded P4 no-change conclusion; `E-20260905-010` records P5's distinct reader
contracts without changing their semantics; `E-20260905-012` records P6's
future explicit-remember relation and bounded explicit-recall selection. The upstream terminal-authority
blocker above remains administrative and does not reopen the delivered
implementations.
