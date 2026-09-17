# Superchat M5 开发交接（2026-09-17）

本轮确定性的产品缺陷已修复、提交并完成回归；**M5 的完整配置任务验收仍未通过，不能标 done**。最新代码提交是 `3f4f4846158a8d019c3b9b30c958ccd599233459`。本轮真实模型运行和专项调查已收尾，没有待继续的 sweep。

项目位置：`/Users/chenyibin/Documents/emacs/package/superchat`。

## 已交付的代码

| 提交 | 内容 |
|---|---|
| `d3fadb71` | 每次 provider 请求独立计算沉默时间；显式 reasoning 映射；如实记录工具预算收尾 |
| `4c067806` | 明确唯一替换和无变化结果；求值前检查完整表达式；识别真实 eval 错误与 reader 前授权边界 |
| `24d8c9a9` | 限制错误诊断并取消重复正文；act 和检查器复用已准备的上下文，发送最终审阅内容 |
| `5291b745` | 保存完整用户/assistant 历史；输出可准确读回的 buffer 名；重试成功后明确继续原任务 |
| `3f4f4846` | provider 没有返回回答时走现有 failed，避免以产品空答复占位冒充 completed |

最新修复只处理“没有收到回答”：nil、只有 token/usage 信息、只有 reasoning，都不能成为最终答复。当前请求已经收到的可见流式正文仍可补足 nil final；上一工具轮的前言不行。工具回合、待处理 steer、取消和预算证据保留原有路径。显式空字符串仍沿用旧完成政策与 Empty 显示，本轮没有扩大这项产品政策。

真实 B/chat 的缺失回调是已观察到的事实；原始 HTTP 响应体没有保存，所以不能判断上游为什么缺失。没有回写旧试验的 completed 记录，也没有将新补丁混入旧试验的源码身份。

历史修复保持原来同一条 Tape 记录，不创建第二份历史或新 metadata。预算仍完整选入或整条省略；旧版本裁掉的正文无法恢复。真实 undo 请求已分别收到聊天前轮完整的 2191 字符和 act 前轮完整的 1585 字符，均与 Tape 正文及散列一致。

## 最终代码验证

| 检查 | 结果 |
|---|---|
| M3 | 139 selected；137 pass；2 个 opt-in 实际 provider 用例跳过 |
| M5 | 11/11 |
| 执行可靠性 | 62/62 |
| 工具反馈 | 34/34 |
| 请求上下文 | 11/11 |
| 历史正文 | 8/8 |
| 最终响应 | 11/11 |
| 合计 | **276 selected，274 pass，2 skip，0 unexpected** |
| Canonical smoke / Kernel | 0 mismatch / 0 fail |
| 编译 | 修改的生产模块通过；8 条归一化 warning 与冻结 5291 相同 |

缺失响应的有效修复前结果是 5 红、6 个兼容守卫绿，修复后 11/11；独立 reviewer 重跑并在临时 Emacs 内存中恢复旧 finalizer，nil 用例重新失败。总回归发现一条旧预期需要同步更新：它在新请求返回 nil 后期待旧 Empty 占位；现在验证 failed 和 Missing，同时完整保留“旧请求的 OLD-PREFACE 不得泄漏”的断言。

受保护的 `scripts/m3-regression.el`、`scripts/m5-regression.el` 和旧破坏脚本未改。代码提交后工作区只剩原有 `.claude/settings.local.json`；没有推送远端。随后交接/路线图只涉及文档。

完整命令、原始测试输出和散列：`.scratch/m5-empty-response/FINAL-CHECKS.md`、`.scratch/m5-empty-response/checks/`。

## 最终真实任务：行为与答复分开看

以下三格全部加载冻结的 **5291b745**，不含后来的缺失响应修复。默认仍为 12/ask；每个 Run 最多模拟一次接受原生的 12 轮扩展，第二次询问拒绝。每 Run 实验 deadline 600 秒，原产品时限不变；批准和预算选择等待按隔离实验规则均为零。

| 条件 | 修改阶段 | 撤销阶段 | 完整验收 |
|---|---|---|---|
| B / chat | 298.507 秒、24 工具轮；没有建立目标绑定 | 204.406 秒、13 工具轮；原状态保持，但最终是产品 Empty 占位 | 未通过 |
| B / act | 196.701 秒、16 工具轮；即时和重启都生效，却误改并保存来源 work.el | 499.606 秒、15 工具轮；原绑定、原配置和来源字节恢复 | 未通过 |
| C / chat：原短说明 | 288.703 秒、24 工具轮；全部自动状态检查通过 | 212.921 秒、20 工具轮；全部自动状态检查通过 | **答复准确性未通过；按预先门槛不启动 C / act** |

六个 Run 均正常结束，没有触发产品或实验超时。C 的原始自动结果 `complete=true` 如实保留，它表示编辑状态通过；完整验收还要求审阅最终答复，不能把这个自动字段当作已过全部门槛。

C 使用的是早已冻结的 1024 字节说明，SHA-256 为 `bb78a2023c7e235a523cab208e3ab192fff8401f75d9ac883e4c97b3c9d1234b`，没有根据新结果改写。全部生产、harness、依赖和 Emacs 映射与 B 相同，唯一新增输入是这份说明。两入口的新增脚本校准先通过；全部 46 份实际 C HTTP 中，说明各完整出现一次。它仍不是按需读取参考资料的实验。

C 显示了值得保留的进步：模型找到了实际配置文件，完成保存、加载、范围保持和撤销。但答复把 `(buffer-name (get-buffer ...))` 在找不到 buffer 时返回当前聊天名，误说成 get-buffer 返回聊天 buffer；它还把尚未落盘的空 custom.el buffer 说成空文件。undo 的按键说明也须区分查询返回的整数与有效绑定。不能因为最终状态正确就撤掉已预先要求的答复准确性。

## 剩余问题及下一步判断

**继续加轮数或放宽时限，已经不是充分的下一步。** 本次真实任务都跑完了；错误包括 API 参数、误认 buffer、违反明确的来源保护要求和错误解释。B/act 的 edit_buffer 与 save-buffer 参数明确指向 work.el，工具没有重定向。没有证据支持把这些错误再归因于来源收集或历史被裁掉。

**短操作知识有实际作用的迹象，但没有通过本轮产品准入。** 一次 C/chat 的编辑状态改善不能证明稳定收益；答复门槛没过，也没有跑 C/act 或验证实际产品中的放置形式。因此没有把说明加入默认配置，第三、四片——参考资料载体/read_reference 与完整操作指南——仍是 proposed。不能说“知识没用”，也不能说“读资料接口已被验证”。

**如果要承诺模型即使选错目标也不能改受保护来源，需要单独定义可执行的写入范围。** 现有 Authority 对工具 Effect 和用户批准作决定，不把自然语言禁止自动编译成约束；本实验又自动批准工具。只拦 edit_buffer 的名字、或在 eval 文本中搜索路径，都挡不住任意 Lisp。这个保证涉及现有 eval 能力边界，不能包装成一行修复。下一份工具面契约应先明确这一点，再决定结构化配置/按键操作怎样减少模型自行拼 Lisp 的机会。

**另有一条确定的非默认链路问题，已登记而未修。** 动态工具说明刷新会误改 base 中相同文字；已删除的说明也会在“空 → 非空”刷新后复活。根因是有身份的贡献被压成 `(TEXT . RENDERER)`。最小设计是保留有序贡献和删除状态；公开中间件整串替换 `:system` 时是否视为调用方接管内容，需要明确兼容规则。issue 已补两个可运行反例、设计候选和七项验收，状态 `needs-contract-decision`；默认这三格不走该动态路径。

## 证据索引（相对于项目根目录）

- 总回归与最新修复：`.scratch/m5-empty-response/REPORT.md`、`FINAL-CHECKS.md`、`RED-GREEN.md`。
- 最新实现契约：`docs/decisions/implemented/2026-09-17-missing-provider-response.md`。
- 最终 B 计划与结果：`.scratch/m5-config-acceptance/FINAL-INTERACTIVE-PLAN.md`、`FINAL-INTERACTIVE-RESULTS.md`。
- B/chat：`.scratch/m5-config-acceptance/runs/real-final-interactive-chat-01/REVIEW.md`。
- B/act：`.scratch/m5-config-acceptance/runs/real-final-interactive-act-01/REVIEW.md`。
- 有界 C 计划：`.scratch/m5-config-acceptance/FINAL-NOTE-CHECK-PLAN.md`。
- C/chat：`.scratch/m5-config-acceptance/runs/real-final-note-chat-01/REVIEW.md`。
- 动态说明契约问题：`.scratch/m5-request-context/issues/02-guidance-equal-text-rewrite.md`。
- 配置任务未完成记录：`.scratch/m5-execution/issues/04-task-acceptance-not-met.md`。

快照只观察有限的已有/新建 buffer 和 fundamental-mode、text-mode 对照，不代表持续监测或所有模式。独立检查器的重启不等于模型亲自做过重启。隔离 HOME 也不是文件系统沙箱；更早 C2 的 /tmp/init-before.txt 写入仍保留在旧报告，未因本轮结果而抹去。原试验目录和判定保持原样。
