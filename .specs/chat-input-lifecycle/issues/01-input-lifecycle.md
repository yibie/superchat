# 01 Ordinary chat visible input lifecycle

status: done
blocked_by: []
writer: learn
authority: superchat-render.el — View prompt identity/placement; superchat-dispatcher.el — input consumption/routing; superchat.el — UI entry/follow-up; Runtime/EW lifecycle authority read-only
spec_ref: .specs/chat-input-lifecycle/SPEC.md
context_ref: KERNEL.md K33; CONTEXT.md
evidence_ref: E-20260906-009

## Goal

One editable draft across ordinary generation, explicit manual send after terminal,
and confirm-before-consume clear through the real interactive entry.

## Scope

superchat.el initialization/send/follow-up UI; superchat-render.el existing prompt
seam; superchat-dispatcher.el necessary routing. Direct tests only
test/test-native-ui.el and test/test-cmd-dispatch.el. Rev2 adds only the form
control/queued-follow-up-appends-and-is-consumed-without-steering in
test/test-control-channel.el (accurate rename permitted); all other forms frozen
and reader-equal, whole control suite required. This Slice status/summary.
All other files read-only during do, including Runtime/Authority/LLM/EW,
Workflow tests, runner/helpers, SPEC and knowledge/old issues/architecture.

## Acceptance

SPEC AC1–5. Three historical red symptoms enter direct tests; new repeated-send,
C-q coordination, terminal draft persistence and clear confirmation cases must
use actual C-c C-c. Real Run counts/Tape/no-draft-leak, stream/final/error/cancel,
sync completion/reentry, Org fontification and existing owner/Workflow guards.

## Verification

Guard self-check before tests; native async and streaming mocked, no provider.
Tests-first focused red/green and discriminating negative controls; fresh
ordinary/real direct and related tests, canonical, full real llm excluding socket;
parens/whitespace and read-only strict/authority with drift honestly reported.
Temporary SQLite/data before product load; preserve unrelated dirty bytes.

## Evidence

Prior plan red3/3, real controls5/5, ordinary4/5 plus1 dependency skip are
historical only. Implementation/check/learn pending; evidence_ref remains empty.

Do implementation complete; status remains doing pending independent check/learn.
Five real-entry baseline reds retained. Nine new native entry tests now cover
temporary draft/final/error/actual keyboard cancel, clear yes/no and late output,
C-q/reentry, unrelated same-Session Run, sync completion and actual native sync/
async tool scheduler with Authority/Tape/Closure. Four mutants discriminate the
new prompt/initialization/admission/exact View-origin seams. The one approved
control form migrated;22 other top-level forms reader-equal, all prior cmd bytes
retained. Actual production delta only superchat.el and superchat-dispatcher.el;
Render and lifecycle production unchanged. Fresh real focused17/17, related
601/599/0/2; ordinary601/573/0/28; canonical1376/1323/0/53; real LLM201/201
socket excluded. Strict1097/73/0 records citation drift (UI41/cmdtests30/
Dispatcher2), Authority form passes. Knowledge remains frozen; no learned claim.
Full commands/logs/limits: /private/tmp/human-prompt-lifecycle-do/WORKER_REPORT.md.

Learn handoff (published E-20260906-009; K34 SHA256 64150942a53ac02327da23c128396f93d8254e8e6907973bee23f42175489c43):
hpc independently PASS with zero Required. Proposed E-20260906-009 preserves
the historical do results above and records K34 input contracts/73 reanchors.
Other command dispatch, nonempty cancel redirect concatenation and legacy queue
readers remain bounded S1/S2/S3 observations, not repaired or generalized here.
SPEC remains confirmed; no goal or administrative terminal claim.
