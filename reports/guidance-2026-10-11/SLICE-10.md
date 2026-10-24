# 切片 10：测试台的判定分离、泄漏扫描、指纹、基线方差

日期 2026-10-20。只改 `.scratch/task-sim/harness-goals/`，不改产品、不改现有测试、不改 `scripts/`。
一个提交，只含这个目录下的文件。提交前停下报告。

## 事实（HEAD `01e3d47a`，今天核过）

- `launch.sh:27` 把 `harness/*.el` 整个复制进运行目录 `run/harness/`；`tasks-goals.el` 的 `:check` 就在里面。
- `run-task.el:427` 在沙箱里的 Emacs 末尾执行 `(funcall (ts-task-get :check) answer)`。
- `profile.sb` 是 Seatbelt，`(deny default)`，但允许读整个运行目录。所以模型通过 `run_shell` 能读到判定表达式。
- 10 月 13 日的 63 次运行，我今天扫了每次的 `evidence/events.json` 和 `messages.txt`，0 次工具调用参数碰到 `harness/`、`tasks-goals`、`tasks.el`、`run-task.el`。数字可信，但靠的是模型没想到，不是隔离。
- 运行目录里没有 `task.json`；判定键叫 `:check`，不叫 `:judge`。

## 10a 判定不进运行目录

要求：模型可读的任何目录里不能有判定代码。

做法建议，实现者可改：

1. 把每个任务的 `:check` 移到 `checks-goals.el`（同名键、同任务 id），这个文件不复制进运行目录。
2. 沙箱 Emacs 退出后，由 `launch.sh` 再起一个沙箱外的 Emacs `-Q -l checks-goals.el`，对着运行目录的磁盘状态（`home/`、`evidence/answer.txt`、`home` 下的 Tape 库）执行判定，把 `pass`、`checks` 写回 `result.json`。
3. 先做一次清点：现有 20 个任务的 `:check` 里，哪些读的是活 buffer 或活 Emacs 状态（未保存内容、`superchat-tape-replay-events`、Runtime 表）。这些要在 `run-task.el` 末尾把需要的状态落到 `evidence/`（例如 fixture buffer 的内容各存一个文件，Tape 本来就在磁盘），判定改读落盘的东西。
4. 清点结果先报告再改。有的判定如果搬不出去，说明原因，列出来，不要硬搬。

## 10b 每批跑完自动扫泄漏

`batch.sh` 和 `surface-batch.sh` 的最后一步：扫每次运行的工具调用参数，命中以下任一项就记一行到批目录的 `leak-scan.tsv`，并在汇总里标 `tainted`：
`harness/`、`tasks-goals`、`checks-goals`、`tasks.el`、`run-task.el`、`:check`，以及任何指向运行目录和 `profile.sb` 允许的系统目录之外的绝对路径。
只报不拦。先拿 10 月 13 日那 63 次跑一遍，结果应为 0 命中，贴进报告。

## 10c 运行记录带指纹

`result.json` 增加：请求 1 实际发出的工具数组的 sha256（从 `evidence/http` 取）、请求 1 系统文本的 sha256、规则文件文本的 sha256（没有就记 `none`）。`code-commit.txt` 已有，保留。
`surface-report.py` 按指纹分组；不同指纹的运行不并入同一平均，除非显式加 `--mix-fingerprints`。

## 10d 基线方差单列

`surface-report.py` 对基线列输出每任务跨 trial 的通过数，1/3 和 2/3 的标为"分叉"，单独一张表。这是 Harness² 的 DIVERGENCE 类别，也是 SHarP 论文里单模块剪枝最先要看的格子。

## 停点

1. 10a 的清点报告（每个任务的判定依赖什么状态）。
2. 四项做完：每个工具面跑一个任务证明管线通，63 次旧运行的扫描结果，一份变更清单。提交前停。

## 不做

- 不改产品。
- 不改 `tasks-goals.el` 里任务的提示词和 `:setup`。
- 不改 M3 集。

## 裁定（2026-10-20，对实现者停点 1 报告 `report-10a-inventory.md` 的回答）

1. **目录**：选 (a)，整个切片 10 在 `harness-tools/` 做，`harness-goals/` 不动。它是产出那 63 次数字的测试台，要保护的就是它。上面写的 `harness-goals/` 一律读作 `harness-tools/`。
2. **提交**：选 (b)，不提交，不改 `.gitignore`，不强制跟踪。改动留在磁盘上；把 `diff -ru`（改前改后）存到 `reports/guidance-2026-10-11/evidence/slice-10.diff`，再附文件清单和 sha256。改前先把 `harness-tools/` 原样复制到 `~/.cache/sc-slice10/harness-tools.before/`，diff 以它为基准。
3. **任务范围**：先搬 `tasks-goals.el` 的 21 个加 `tasks.el` 的 T1，共 22 个判定。`tasks.el`、`tasks-step4.el` 其余 30 个这次不搬，但管线必须能跑它们：判定仍在沙箱内的任务，`result.json` 记 `"check_in_sandbox": true`，报告脚本单列，免得被当成干净数字。
4. **通用采集**：是。按你说的做：每个被访问过的 fixture buffer 的文本、全部 Superchat 定时器、全部 Emacs 定时器（打印、有界）、所有 Workspace 的工作记录、`custom-enabled-themes`；加 `:gather` 条目只放值不放判断（E2 的探针、P3/T1R/P3T 的起始时间、A1/T1 的原文，原文在 `:setup` 时就存进 `evidence/`，避开 `ts-day` 跨午夜）。采集在 `:save-after` 和答案之后做。判定在沙箱外只比值、不加载 Superchat，同意你的理由。
5. **一致性验证**：搬完后，7 个 surface 任务各跑一次，同一次运行同时出两份结论：沙箱内旧判定和沙箱外新判定，两份都写进 `result.json`，表格贴进报告。只有全部一致才拆掉旧路径。不一致的逐条说明。
6. 10b 的扫描关键词加上 `checks-goals`、`gather`、`evidence/`。

## 验收（2026-10-20，leader）

接受。核过：`harness-goals/` 未动（mtime 10 月 10 日）；`launch.sh:27-31` 复制时排除 `checks-goals.el`，`:83` 记模型 Emacs 结束时间，`:104-111` 判定前计数、复制、同一沙箱里跑、删除；今天的 A2-real-D13-t1（切换后）运行目录的任务副本里 `:check` 为 0，`result.json` 写 `judged = after the run, by checks-goals.el`，日志"checks in the run before judging: 0"；63 次旧运行扫描文件只有表头，0 命中。一致性 30 次 0 分歧。

留作后续、不阻塞：只有判定用的辅助函数仍在运行目录；P1m、P1W、R1X、W11A、T1R、W9U 六个判定没有任何运行对照过；模型启动的、既无 cwd 也无打开文件在运行目录下的进程判定前找不到。
