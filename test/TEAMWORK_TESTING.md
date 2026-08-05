# Superchat Teamwork 手动测试

本文用于验证 Superchat 的共享任务、mailbox、subagent delegation 和生命周期行为。
每个场景都要求观察真实工具调用；助手文字中声称“已经调用”不算通过。

## 测试前准备

最稳妥的方式是重启 Emacs。若只想重新加载当前工作区中的实现，依次执行：

```elisp
(load "/Users/chenyibin/Documents/emacs/package/superchat/superchat-subagent.el")
(load "/Users/chenyibin/Documents/emacs/package/superchat/superchat-tools.el")
(load "/Users/chenyibin/Documents/emacs/package/superchat/superchat-llm.el")
```

在空白 Superchat 会话中输入下面的命令。它只启用请求级 Teamwork 工具面，不修改
全局工具配置，也不会混入 `define_tool` 或已经持久化的合成工具：

```text
/team /Users/chenyibin/Documents/emacs/package/superchat
```

省略目录时使用当前工具目录：

```text
/team
```

完成测试后用 `/reset` 回到普通聊天模式。

测试时另开一个空白 Superchat 会话，避免旧会话的上下文影响模型选择。

## 场景 1A：自然语言能否路由到 subagent

目的：验证模型只收到业务意图时，能否自行选择 delegation，而不是自己造工具。

输入：

```text
调查 Superchat 的 subagent 实现
```

通过条件：

- 实际调用 `delegate_to_subagent` 或 `delegate_to_subagent_parallel`。
- 调查路径位于当前 Superchat 项目，不读取用户主目录下无关的 `package.json`。
- 不调用 `define_tool`，也不要求创建 `grep-subagent` 等临时工具。
- 最终报告引用 subagent 的真实返回结果。
- `/agents` 和 `team_task` 的任务状态大致一致。

如果模型只用内置的 `search-text`/`read-file` 自己完成调查，这不属于工具故障，
但本场景仍判定为“自治 delegation 未触发”。

## 场景 1B：强制验证共享任务工具

目的：把工具选择的不确定性排除，只验证 `team_task` 的调用链和返回值。

输入：

```text
不要口头模拟。请实际调用 team_task：

1. 创建任务“调查 Superchat 的 subagent 实现”
2. 创建任务“验证调查结果”
3. 调用 list 列出任务

最后只总结真实的 tool result。
```

通过条件：

- 产生两次真实的 `team_task` `create` 调用和一次 `list` 调用。
- 返回两个不同的真实任务 ID，例如 `T123` 和 `T124`。
- 两项任务初始状态均为 `pending`。
- 助手没有编造工具结果或任务 ID。

## 场景 2：依赖阻塞与解除

输入：

```text
请严格使用 team_task 完成下面的测试，不要编造任务 ID：

1. create 任务 A：“调查失败原因”
2. create 任务 B：“验证调查结论”，depends_on 必须引用任务 A 的真实 ID
3. 先尝试 claim 任务 B，并记录真实错误
4. claim 并 complete 任务 A
5. 再 claim 并 complete 任务 B
6. 最后 list 所有任务

不要跳过第 3 步。
```

通过条件：

- B 的 `depends_on` 使用 A 的真实任务 ID。
- A 未完成时，claim B 返回依赖未满足错误。
- A 完成后，B 可以被 claim 和 complete。
- 最终 A、B 都是 `completed`。

## 场景 3：并行 delegation 自动登记任务

输入：

```text
请实际调用 delegate_to_subagent_parallel，并行执行两个任务：

1. researcher：找出 superchat-subagent.el 中负责异步启动 subagent 的函数，并报告函数名
2. introspector：检查变量 superchat-subagent-parallel-max 的当前值和文档

两个 subagent 全部完成以后，调用 team_task 的 list 动作，展示它们自动创建的任务状态。

不要自己执行这两个调查，也不要口头模拟工具调用。
```

执行过程中运行一次 `/agents`。

通过条件：

- 两个 subagent 曾同时处于活动状态，受并行上限约束。
- delegation 自动创建并 claim 两项共享任务。
- 两位 subagent 返回的结论与源码或 Emacs introspection 一致。
- 正常结束后，两项任务自动变为 `completed`。

## 场景 4：mailbox 跨 subagent 传递

输入：

```text
请按顺序执行，不要并行，也不要自己代替 subagent：

第一步，调用 delegate_to_subagent 启动 researcher：
- 检查 superchat-subagent.el 中 superchat--subagent-make-context 的用途
- 得出结论后，必须调用 team_message，向 all 发送一句简短结论
- 发送成功后再返回报告

第一位完成后，再调用 delegate_to_subagent 启动 introspector：
- 开始调查前，必须先调用 team_message 的 list 动作
- 阅读 researcher 发出的广播
- 检查广播描述是否符合该函数的文档
- 在最终报告中写出收到的消息 ID 和验证结论

最后调用 team_task list。
```

通过条件：

- researcher 的 `team_message send` 返回真实消息 ID，例如 `M456`。
- introspector 的 mailbox 能读到同一消息 ID 和内容。
- 第二位 subagent 的验证引用收到的消息，而不是重新编造一条。
- 两项自动任务最终都是 `completed`。

当前 mailbox 是 pull-based：接收者必须调用 `team_message list`，不会被实时唤醒。

## 场景 5：取消后释放任务

输入：

```text
/subagent researcher 请逐个梳理 superchat-subagent.el 中的异步调用、超时、取消、并行窗口和 Tape 记录路径，给出详细报告
```

在任务仍运行时：

1. 执行 `/agents` 并复制 subagent ID。
2. 执行 `/cancel-agent <真实 subagent ID>`。
3. 再输入：

```text
请调用 team_task 的 list 动作，显示当前任务状态，不要口头模拟。
```

通过条件：

- subagent 被取消且不再显示为运行中。
- 自动登记的任务从 `in_progress` 回到 `pending`。
- 任务 owner 被清空，可以被其他 actor 再次 claim。

## 总体验收清单

- [ ] 所有 ID 都来自真实工具结果。
- [ ] 依赖未满足时不能 claim。
- [ ] delegation 自动登记、claim 并在成功后 complete。
- [ ] 失败或取消会释放任务，而不是错误标记为完成。
- [ ] `all` 广播可被另一个 subagent 读取。
- [ ] `/agents` 与 `team_task list` 的活动状态大致一致。
- [ ] 模型没有在已有内置工具时请求创建同类临时工具。
- [ ] 工具的默认目录指向被调查项目。

尚未覆盖：实时 mailbox 唤醒、图形化 Team View，以及多个 Emacs 进程同时操作同一
团队的竞争条件。

## 场景 1A 实测记录（2026-07-23）

输入：

```text
调查 Superchat 的 subagent 实现
```

结果：**失败**。

观察：

- 助手重复了多次“让我查找/搜索”的计划性旁白。
- 没有调用 `team_task`、`team_message` 或任何 delegation 工具。
- 请求读取了错误路径 `/Users/chenyibin/package.json`。
- 随后请求定义并持久化 `grep-subagent`，流程停在人工确认处。
- TTFT 为 8.82 秒。
- Tape 只记录了用户输入和最后一句助手旁白，没有对应的 `tool_call` 或
  `tool_result`；因此没有证据表明 Teamwork 工具执行后失败。

现场配置：

```elisp
(:tools-enabled always
 :tool-names ("read-file" "render_ui" "define_tool")
 :tools-default-directory nil
 :chat-default-directory "~/"
 :preset nil)
```

结论：

1. 本次请求中 Teamwork 工具没有暴露给模型，所以不是 Teamwork 实现执行失败。
2. `define_tool` 被暴露而 `search-text` 和 delegation 工具未暴露，直接促成了
   `grep-subagent` 自扩展请求。
3. 工具默认目录为空且聊天 buffer 位于 `~/`，解释了错误项目路径。
4. 这次结果可作为“自然语言自治路由 + 工作区定位”的失败基线；修正配置后应从
   场景 1A 重测，再用场景 1B 区分模型路由问题和底层工具问题。
