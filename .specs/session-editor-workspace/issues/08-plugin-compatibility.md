# 08 迁移 Telega 与公开插件 Request 契约

status: done
blocked_by: 05
writer: do
authority: EditorWorkspace membership/role/close — `superchat-editor-workspace.el` | owned；Telega Request、审批/工具/Session/callback — `plugins/superchat-cowork-telega/superchat-cowork-telega-{legacy,session}.el` | owned；公开插件合同 — `docs/plugin-guide.md` | owned
spec_ref: `.specs/session-editor-workspace/SPEC.md` Revision 5 — Owned and borrowed resources；Compatibility and migration；Issue map S5
context_ref: `KERNEL.md` K17 — Session EditorWorkspace、Submit Request、authority boundary；`CONTEXT.md` — Session EditorWorkspace / Owned Editor Buffer
evidence_ref: E-20260904-006

## Goal

将 shipped bundled Telega 的四种 Runtime Request 全部迁移为稳定 Session ID 加公开
EditorWorkspace API 取得/验证的 exact live buffer，并把相同边界写成可执行的第三方
插件合同；迁移两个仍有运行入口的 active experiments。报告必须区分 shipped、插件、
测试、实验、文档、历史和未知外部调用者。

`blocked_by: 05` 已由 `E-20260904-003` 满足；Slice07/K17 也由
`E-20260904-005` 完成。现有公开 allocation/owner/validation/close API 足够，本 Slice
可由一个 fresh `do` worker 完成，不拆分、不回 plan。

## Executable classification model

### Four shipped Telega Request shapes

四条路径都异步调用 `superchat-runtime-submit`。Extension 可丰富 Request，但不得按
buffer 名、foreign callback 或 legacy 字段改变 producer 固定的 Session/buffer pair。
role 只是公开 EditorWorkspace lookup key；ownership 仍只由 authority 的 exact
buffer-local metadata 判定。

| Shape | Session source | Exact buffer / role | Terminal host lifetime |
| --- | --- | --- | --- |
| reply/draft (`--request`) | numeric chat ID 确定地产生 `telega-chat-<id>` | compose 前以 `superchat-editor-workspace-buffer` 取得/复用 `telega-reply-protocol`，再以 `superchat-editor-workspace-validate-buffer` 验证同一 object | 普通 Session-owned；success/cancel/provider error/callback error/quit 后均存活，public Session close 或用户明确关闭负责 |
| digest (`--digest-request`) | 现有稳定 `telega-digest` | 同 Session 取得/复用并验证 `telega-digest-protocol` exact host | 普通 Session-owned；所有终态存活，public close 负责 |
| triage (`--triage-request`) | 现有稳定 `telega-triage` | 同 Session 取得/复用并验证 `telega-triage-protocol` exact host | 普通 Session-owned；所有终态存活，public close 负责 |
| contextual Session (`--session-request`) | session struct 的 stable `id`，由 numeric chat ID 产生 `telega-cowork-<id>` | `session-open` 用公开 API 取得/复用 visible `telega-session-view`，每次 submit 验证 struct 保存的同一 live object | 可见且可回访；所有终态存活；直接用户 kill 保持现有 forget-session callback，Superchat 关闭走 public close |

每个 Request 必须写 exact `:buffer`，不得写 `:ephemeral-buffer`、
`:host-authority-mode` 或依赖 `:buffer-name` 选 host。same Session/role 必须复用同一
object；mismatch/malformed 在 provider/Tape/Run effect 前拒绝；inherited/default
metadata 不授予 ownership。不得把 stable reply/digest/triage Session 猜成 operation-
scoped child 或新增 terminal auto-close。

保持 Telega autonomy、scope、tools、tool-validator、Authority policy、approval preview/
fallback、attendance、origin、history/running flag、draft insertion、digest display、triage
sampling/rules 和 callback 语义。工具、审批或 foreign callback 不得成为 close authority。

### Public plugin guide

`docs/plugin-guide.md` 是公开第三方合同，必须：

- 明确公开依赖 `(require 'superchat-editor-workspace)` 以及示例实际使用的 Runtime/
  Authority/Tools 模块；不得引用 `superchat--` 私有符号。
- 给出可 batch-load/结构检查的 exact-buffer + Session 示例：先确定非空稳定 Session，
  以 plugin-specific role 取得 buffer，经 public validation 后把同一 object 作为
  `:buffer` 异步 submit，并保留 Extension/callback 合同。
- 区分 ordinary borrowed 与 owned host：producer 已知 Session 时可验证并借用已有
  user/source buffer，所有终态 Runtime 不 kill；owned host 由 EditorWorkspace 创建、
  标记、按 role 复用，仅 public Session close/用户明确关闭结束。名称、当前 buffer、
  callback 或目录都不证明 ownership。
- 字段表以 `:buffer` 为当前合同；将 `:buffer-name`/`:ephemeral-buffer` 和
  `:host-authority-mode` 明确列为 legacy compatibility inputs，并说明它们会在本次已
  宣布 breaking migration 的最终移除阶段退出旧 host-selection/ownership 路径。
  按 SPEC 的 Compatibility and migration 写清：producer 迁移阶段仍接受旧调用，
  完成迁移与分类后由 Slice09 删除旧路径；包含该删除的发行版构成 breaking boundary。
  数字版本号尚未指定，应如实标注，不为本地实现设置版本号前置条件。不得擅自承诺
  `2.0.0`、整个 `1.x` 的兼容期、发布日期或外部调用者已迁移；发布不属于本 Slice。
- 明说仓库内零 callers 不能证明 repository-external consumers 已迁移；外部消费者
  必须在 removal release 前迁移。

### Experiments

两文件均是 active runnable samples，不是 historical：`paired-use.el` 被
`closed-loop.el` 加载并有 `SUPERCHAT_EXPERIENCE_MINIMAL_EXECUTE=1` 入口；
`recovery-self-learning.el` 被 `run-self-learning.sh` 以
`SUPERCHAT_RECOVERY_SELF_EXECUTE=1` 执行。因此都要迁移所有 submit site：每个
distinct experiment Session 通过 public API 取得/复用 experiment-specific role、
validate exact object、Request 写 `:buffer` 且无 legacy keys。普通 Session host 在
terminal 后存活；实验 teardown 只能在全部 work 终结后 public close，不能按名 kill。
保持 provider、trace、Tape、tool、timeout、outcome 与 opt-in env 行为。

若实施证据证明某文件已无调用入口且只用于历史复现，立即回 plan；不得删除或偷偷
标 historical 来消除命中。真正 historical 内容必须显式注记并单列，不计入 shipped
zero-callers。

### Full-tree taxonomy and Slice09 gate

red/green 搜索将每个 legacy-key 命中归入互斥类别：

1. `shipped/core`：Runtime compatibility implementation 可保留；普通已迁移 producer
   应为零。
2. `shipped/bundled-plugin`：两个 Telega production 文件，green 必须零 legacy
   producer keys。
3. `test`：明确 legacy fixtures 可保留并单列。
4. `experiment-active`：本 Slice 两文件，green 必须零 legacy producer keys。
5. `docs-current`：当前示例/字段零 legacy use；兼容/removal 文字单列。
6. `archive/historical`：SPEC/Slices/KERNEL/EVIDENCE/archive/trace 仅报告，不修改删除。
7. `external-unknown`：仓库搜索不可见，固定报告 unknown，绝不声称已迁移。

Slice09 只有在 ordinary shipped producers 与 bundled Telega producers 均零 legacy
keys，且 active experiments/current example 分类通过后才解除 08 依赖。Runtime
implementation、legacy tests、兼容说明和历史文字命中不阻塞 producer gate，但都要报告。

## Scope and exclusive worker ownership

一个 fresh `do` worker 只能修改：

- `plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el`
- `plugins/superchat-cowork-telega/superchat-cowork-telega-session.el`
- `plugins/superchat-cowork-telega/test-cowork-telega.el`
- `docs/plugin-guide.md`
- `experiments/experience-learning-minimal/paired-use.el`
- `experiments/experience-learning-minimal/recovery-self-learning.el`

`test/run-tests.el` 只是验证输入：plugin-local test 当前不属于 canonical runner，必须
继续用其 standalone 命令；canonical 仍是强制回归门。experiment runners 与
`closed-loop.el` 仅为 invocation evidence；若验收必须修改它们则 hard-stop，不扩 scope。

Telega integration 可 require middle-tier `superchat-editor-workspace`；不得新增 Kernel →
EditorWorkspace/plugin 或 middle-tier → plugin top-level require。不得把 EditorWorkspace
注册为 plugin。

## Red-before-green tests-first matrix

### Phase 1 — four exact Requests

先在 plugin test 写 table-driven red：逐一覆盖 reply/digest/triage/contextual Session 的
exact object、稳定 Session 来源、role、异步 submit、无 legacy keys；再覆盖 fresh、
same Session/role reuse、wrong-Session owned mismatch、malformed local、inherited/default。
断言 mismatch/malformed 在 Runtime/provider effect 前失败，inherited 不授权也不 retarget；
Telega capability/topology/policy/approval/tools/origin/callback 字段除 host 外不变。观察 red
后才用公开 API 最小实现，禁止 marker 读取、name lookup 或第二 registry/classifier。

### Phase 2 — lifetime and regression

先补 success、cancel、provider error、callback `error`、callback `quit` red matrix；证明四
host 均存活，Runtime/producer/foreign callback 不 close；public Session close 才释放，
contextual sidecar 用户 kill 仍 forget session。保持 approval/tool/scope/history、draft、
digest 与 triage tests green。

### Phase 3 — guide contract

先在 owned plugin test 加 batch-evaluable extraction 或结构 red：公开 require 可解析、示例
含同一 exact buffer+Session+validation、async submit、无 legacy/private/blocking use，prose
覆盖 borrowed/owned、breaking/removal、external-unknown。再最小改 guide，不猜版本。

### Phase 4 — active experiments

先在 execution env unset 时 load 两文件，并记录 legacy-shape red search。用 owned plugin
test 或直接 batch form 做最小 smoke/structure gate，证明所有 experiment submit form 使用
public API exact buffer 且零 legacy keys，再迁移。例行验证不运行真实 provider；若未运行
需明确报告。

### Phase 5 — classified gates

重跑七类搜索、plugin tests、experiment smoke、architecture gates 和 canonical suite。
只有全部 acceptance 通过才标 done，并在本 Slice 记录 red、green exact counts、分类、
warnings、focused diff 和 remaining risk。

## Acceptance

1. 四种 Telega shape 使用上表的 Session/role/exact validated object，且无三个 legacy/
   name-based host fields。
2. fresh/reuse/mismatch/malformed/inherited 矩阵证明 exact-object/Session authority，无
   producer marker reads 或 ownership inference。
3. success/cancel/provider error/callback error/quit 后普通 host 均存活；public close/用户
   sidecar kill 的责任清晰且已测。
4. Telega approval、tools、scope、history、policy、Session、async 与 callback 语义不回归。
5. guide 当前示例/字段、依赖、borrowed/owned、breaking/removal/external unknown 合同与
   executable/structural gate 通过。
6. 两个 active experiments 在不改变实验语义下迁移，load/smoke 与 zero-legacy 通过；不
   删除 historical 材料掩盖命中。
7. 最终报告七类结果且不声称未知 external consumers 已迁移。
8. layering gates 通过；Runtime/EditorWorkspace API 不变。
9. standalone plugin test、canonical runner 与 search gate 通过；Slice09 只按明确 producer
   gate 解锁。

## Verification

```sh
emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
  -l plugins/superchat-cowork-telega/test-cowork-telega.el \
  -f ert-run-tests-batch-and-exit

env -u SUPERCHAT_EXPERIENCE_MINIMAL_EXECUTE \
  emacs -Q --batch -L . -l experiments/experience-learning-minimal/paired-use.el
env -u SUPERCHAT_RECOVERY_SELF_EXECUTE \
  emacs -Q --batch -L . -l experiments/experience-learning-minimal/recovery-self-learning.el

emacs -Q --batch -L . -l test/run-tests.el

rg -n ':ephemeral-buffer|:host-authority-mode' --hidden --glob '!.git/**' --glob '!*.elc'
rg -n ':buffer-name|superchat-runtime-submit(-blocking)?|superchat-editor-workspace-' \
  plugins/superchat-cowork-telega docs/plugin-guide.md \
  experiments/experience-learning-minimal/{paired-use,recovery-self-learning}.el
rg -n 'superchat--editor-workspace-(owned-p|session-id|role)' \
  plugins/superchat-cowork-telega docs/plugin-guide.md \
  experiments/experience-learning-minimal/{paired-use,recovery-self-learning}.el
rg -n "require 'superchat-cowork-telega|require 'superchat-editor-workspace" --glob '*.el'
rg -n 'test-cowork-telega|paired-use|recovery-self-learning' test/run-tests.el \
  plugins/superchat-cowork-telega experiments/experience-learning-minimal
```

Experiment bare `-Q` 若缺 optional deps，按 AGENTS 的临时 package initialization 加既有
依赖，不得据此误判样例不可用。另做 changed-file `check-parens`、repo 外 scratch byte
compile、六 owned files focused diff/check、只读证明 Runtime/EditorWorkspace/canonical
runner/core tests/其他脏文件未被编辑，以及 canonical kernel-require/eager-integration/
blocking-location/process-pump gates。Kernel citation 只报告，不编辑。

## Forbidden changes and hard stop

禁止修改 Runtime、EditorWorkspace 及其 API、非插件 production、`test/run-tests.el`、core
tests、plugin metadata/DESIGN/skill、experiment runners/traces/db/其他实验、SPEC、KERNEL、
EVIDENCE、CONTEXT、STATUS、plugin-guide 之外 docs、架构记录、doctrine、其他 Slice。禁止
Git/JJ 写；保留共享脏工作区。

若 Telega Session 来源不稳定，或正确行为需要新 durable identity、automatic child-close、
Runtime/transport/API/marker 改动、第二 registry/classifier、producer-side ownership read，
或验收必须越过 owned files，立即将 Slice 标 `blocked` 并记录最小 reproduction，回 plan。
若实现要求改变 SPEC 已批准的兼容/移除顺序，保持 blocked 并回 plan。
数字 release version 的未指定本身不阻塞本 Slice；不得猜版本或执行发布。

## Worker handoff

复用现有 `slice08_do` agent，以当前文件为基线。它必须重读 AGENTS/SPEC-AGENTS/doctrine、SPEC Revision 5、
K17、CONTEXT/STATUS、Slices05–08、完整 Telega/plugin test/guide/两个 active experiments
及 invocation files、canonical runner ownership 和 fresh 全树搜索；严格按 Phase 1–5。
完成或阻塞后主动向 `superchat_lead` 报告 owned files、四 shape、red/green counts、七类
搜索、hard risk、next action、Git/JJ writes=`none`。

## Evidence

Independent `slice08_final_check` passed with every Required finding closed.
`E-20260904-006` records the verified Slice08 increment and the explicit
decision to keep K17 because this migration applies existing contracts rather
than introducing a new semantic or Action Contract rule.

## Historical blocked verification summary (do, superseded)

The following report records the earlier stop under the then-written Slice
condition.  It is historical, not the current handoff or permission boundary.

- Hard stop: repository sources confirm only an unspecified “removal release”.
  No numeric removal version is confirmed.  The public boundary is therefore
  **next breaking release / removal version TBD**.
- Reproduction: the only removal-boundary matches are the generic wording in
  `.specs/session-editor-workspace/SPEC.md` and this Slice; current package
  metadata names the package version but does not designate a removal version.
- Per the Public plugin guide and Forbidden changes clauses, choosing that
  version requires a user/`plan` decision.  `do` did not guess it and did not
  enter Phases 1–5.
- Six owned production/test/doc/experiment files: unchanged.  Red/green ERT,
  experiment smoke, canonical ERT, compile, and check-parens: not run because
  the hard stop precedes implementation.  Provider effects: none.
- Fresh legacy search was run before the stop.  It confirms repository hits
  across shipped Runtime compatibility, bundled Telega producers, tests,
  active experiments, current docs, and archive/historical material; the raw
  tree also contains large trace/result payloads, so exact migration counts
  remain intentionally unclaimed until execution resumes.  External consumers
  remain `unknown`.
- Kernel/authority: read-only; no Runtime, EditorWorkspace, canonical runner,
  core test, Kernel, SPEC, CONTEXT, STATUS, Evidence, metadata, runner, trace,
  database, or unrelated file was edited.  Git/JJ writes: none.
- Next action: `plan`/user confirms the numeric removal version (or explicitly
  confirms that “next breaking release / removal version TBD” is the final
  publishable boundary), then `arrange` returns this Slice to `ready` for a
  fresh tests-first `do` run.

## Arrangement correction — release numbering is not an implementation dependency

The confirmed SPEC's Compatibility and migration section already defines
compatibility during producer migration and removal after caller/example/doc
classification.  Neither that section nor the enacted K17 boundary requires
a numeric release version before implementing Slice08.  The earlier hard stop
was an additional condition introduced during arrangement, not a user-approved
release decision.  This correction restores the existing SPEC boundary without
choosing a version, changing compatibility duration, or authorizing a release.

Slice08 is ready for the existing `slice08_do` worker to execute Phases 1–5.
The six-file ownership, verification gates, other hard stops, and empty
`evidence_ref` remain unchanged.  No implementation or test result is claimed
by this arrangement correction.

## Verification summary (do)

- Tests-first red: standalone plugin ERT ran 42 tests with 35 expected,
  6 unexpected, and 1 skipped.  The red failures covered missing exact hosts
  for the four Request shapes, missing pre-activation identity guard, stale
  guide contract, and unmigrated active experiments.  One red fixture path was
  corrected inside the owned test; the existing real-Runtime test also needed
  its intended test-only Closure guard binding made explicit.
- Green: standalone plugin ERT is 45 total, 44 expected, 0 unexpected,
  1 skipped (optional llm tool surface).  Table and lifecycle tests prove
  reply `telega-chat-123`/`telega-reply-protocol`, digest
  `telega-digest`/`telega-digest-protocol`, triage
  `telega-triage`/`telega-triage-protocol`, and contextual
  `telega-cowork-123`/`telega-session-view` exact host identity and reuse.
  Real Runtime terminal tests replace only transport/Tape boundaries and cover
  completed, failed/provider-error-shaped, and cancelled outcomes for all four
  hosts; callback `error` and `quit` retain the host.  Public Session close is
  the release boundary, and the existing contextual user-kill forget callback
  remains green.
- Extension composition now receives a producer identity acceptance function.
  Session or exact-buffer retarget/removal is rejected before
  `extension.activated` Tape publication.  All four Requests remain async and
  preserve their existing capability, topology, policy, attendance, tools,
  validator, approval, origin, history/running, draft, digest, triage, and
  callback behavior.
- Guide: the executable example requires Runtime, Authority, Tools, and public
  EditorWorkspace, allocates a plugin role, validates and submits the same
  exact object, distinguishes ordinary borrowed from owned host lifetime, and
  publishes `:buffer` as current.  Legacy inputs remain compatibility-only
  until Slice09; the removal release is the breaking boundary and its numeric
  version is explicitly unspecified.  External consumers remain unknown.
- Active experiments: both execution-env-unset load smokes exit 0 and perform
  no provider call.  Their submit sites use experiment-specific stable
  Sessions/roles, public allocation/validation, exact `:buffer`, and no legacy
  host keys.  Provider, Tape, trace, tools, timeout, outcome, and opt-in env
  semantics are unchanged; no runner, trace, or database was edited.
- Canonical ERT: 1240 total, 1223 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.  This includes the kernel-require, eager-integration,
  blocking-location, and process-pump architecture gates.
- Seven exclusive classes: `shipped/core` has 21 compatibility-key matches,
  all in `superchat-runtime.el`; ordinary shipped producers have zero.
  `shipped/bundled-plugin` production has zero.  `test` has 35 core-test
  fixture matches plus 6 plugin structural/fixture matches.
  `experiment-active` has zero.  `docs-current` has 2 compatibility/removal
  text matches and zero current-example use.  `archive/historical` has 8
  source/history files with matches (large trace/result payloads were excluded
  from printed output).  `external-unknown` is unknown.
- Changed Lisp files pass `check-parens`; scratch byte compilation wrote only
  under `/tmp/slice08-compile` and completed with pre-existing optional/free
  function and unused-binding warnings in the experiment samples.  Focused
  `git diff --check` passes.  No repository `.elc` was created.
- Kernel and Authority remained read-only; authority-map form is `ok`.
  Runtime, EditorWorkspace/API, canonical runner, core tests, SPEC, KERNEL,
  EVIDENCE, CONTEXT, STATUS, metadata, DESIGN, skills, and unrelated files were
  not edited.  Git/JJ writes: none.  Hard remaining risk is repository-external
  callers, which remain unknown and must migrate before the Slice09 removal.
  `evidence_ref` intentionally remains empty for independent `check`/`learn`.

## Required-fixes correction summary (do)

- Added the independent check's two missing executable matrices in the owned
  plugin test only; Telega production required no change.  The four real
  builders now cover fresh/reuse plus wrong-Session owned mismatch, malformed
  local metadata, inherited/default metadata as ordinary unowned borrowing,
  and a same-name foreign object.  Public EditorWorkspace validation is not
  replaced.  Mismatch/malformed stop before activation Tape, Runtime, and
  provider effects; borrowed/owned exact-object cases traverse real compose
  and Runtime submit.  Four Extension mutations (in-place buffer, copied
  buffer, removed buffer, alternate valid Session/buffer pair) are rejected
  before activation publication.
- Callback `error` and `quit` now traverse real Runtime submit, transport
  terminal observation, and actual delivery for reply, digest, triage, and
  contextual Requests (8 cases).  Each callback fires once; every Run is
  publicly terminal and absent from active Runs, a repeated late terminal does
  not redeliver, the exact host remains live, contextual `running-p` clears,
  and public Session close finally releases the hosts and forgets the
  contextual Session.  The test-only Closure guard is disabled only because
  the Tape boundary is stubbed and supplies no durable proof; this is not a
  Closure test.
- First correction run: 46 total, 44 expected, 1 unexpected, 1 skipped; the
  unexpected result was a new-test lexical-scope fixture error.  A focused
  rerun then exposed that public Runtime status intentionally omits the private
  quiescence bit, so the assertion was corrected to public terminal/active-run
  and at-most-once evidence.  The new production behavior itself required no
  green implementation change.  Final standalone plugin ERT: 46 total,
  45 expected, 0 unexpected, 1 skipped.  Both env-unset experiment smokes exit
  0.  Canonical ERT remains 1240 total, 1223 expected, 0 unexpected, 17
  skipped, with `RUNNER_GATE_OK`.
- Controlled all-three-key line search reports: `shipped/core` compatibility
  28 lines in Runtime and zero ordinary producer lines;
  `shipped/bundled-plugin` production zero; `test` 80 core-test lines and 4
  plugin-test fixture/structural lines; `experiment-active` zero;
  `docs-current` one compatibility/removal line (3 key tokens) and zero current
  example producers; `archive/historical` 8 selected source/history files;
  `external-unknown` remains unknown.  Large trace/result payloads were listed
  by path only and are not counted as remaining producers.
- Changed test Lisp passes `check-parens`; focused `git diff --check` passes.
  Kernel strict is 1062 references, 0 errors, 0 warnings; authority-map form is
  `ok`.  Kernel, Authority, Runtime, EditorWorkspace/API, production, guide,
  experiments, runners, traces, databases, and other shared changes remained
  untouched in this correction.  No provider or Git/JJ write ran, no repository
  byte compilation ran, and `evidence_ref` remains empty for independent
  recheck.

Missing-key fixture correction: the Extension table now keeps the explicitly
named nil-value case and separately removes the complete `:buffer` key/value
pair with `cl-remf`.  A `plist-member` absence assertion runs before the broad
rejection assertion, so fixture failure cannot create a false green.  The real
compose/acceptance path rejects all five named mutations with zero activation
Tape, Runtime, or provider effects while the original host remains live.  The
new missing-key target ERT passed 1/1 directly; standalone plugin ERT remains
46 total, 45 expected, 0 unexpected, 1 skipped, with `check-parens` and focused
diff-check passing.  Production was unchanged.  This test-only delta reuses the
independent recheck's canonical 1240/1223/0/17 `RUNNER_GATE_OK`, Kernel strict
1062/0/0, authority-map `ok`, and two env-unset smoke exit-0 results;
`evidence_ref` remains empty.
