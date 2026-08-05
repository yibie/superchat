# llm-buddy：环境式 Agent 反馈回路观察

> 记录日期：2026-07-28
>
> 来源：[ahyatt/llm-buddy](https://github.com/ahyatt/llm-buddy) 的 README、源码与提交历史。
>
> 状态：**已记录，暂缓进入路线图。**

## 核心观察

`llm-buddy` 不是聊天客户端。它把 LLM 放进编辑现场的反馈回路：

1. 监听用户最近的 buffer 修改；
2. 把连续编辑压缩成净 diff，并附带 project、buffer 与 cursor 状态；
3. 空闲时让模型审查最小变化集，缺上下文时再调用 `read_buffer`；
4. 通过 `add_note`、`update_note`、`remove_note` 和 `end` 等受限动作返回结果；
5. 将结果显示在原文附近，而不是另开一个通用聊天窗口；
6. 用户继续编辑后重新验证 note，而不是把旧结论永久保留。

其重要产品判断是：工作现场本身可以表达意图；上下文可以由运行环境投影；
“没有值得打扰用户的内容”也是正式结果；AI 结论需要随现场变化而更新。

## 对 Superchat 有价值的启发

- **环境即输入**：当前 buffer、光标、选区、project 和最近变化都可以成为
  Request 的结构化 origin，而不必由用户重复描述。
- **最小上下文优先**：先给变化集，再按需调用工具扩大上下文，避免把整个现场
  一次性塞进 prompt。
- **结构化结果优先**：模型负责判断，宿主负责执行和呈现；结果不必只有自由文本。
- **结论有生命周期**：active、updated、resolved、dismissed、stale 比一次性的
  assistant message 更适合持续协作。
- **低打扰是能力**：主动型 Agent 的价值取决于 precision；无事发生时保持安静。
- **质量需要行为评估**：除了单元测试，还应衡量 false positive、false negative、
  dismiss rate 和实际行动转化。

这些思想未来可用于 Cowork Telegram 的 findings、消息锚点和持续重评估，也可用于
其他 Emacs Surface。但它们应建立在统一 Runtime 之上，而不是各 Surface 独立实现
自己的状态机。

## 不应照搬的部分

- `llm-buddy` 可选的后台自动修复不适用于 Telegram 发送、规则修改等外部副作用；
  Superchat 仍应在 Authority 边界显式审批。
- `read_buffer` 的读取范围主要依赖 prompt 约束；Superchat 必须继续在工具执行层
  强制 Resource 与 Scope。
- 项目仍处于早期 `0.1.0` 阶段，状态主要保存在内存中。应学习其产品思想，而非复制
  当前实现结构。

## 当前优先级边界

当前最优事项仍然是让下面这句话在代码契约上成立：

> **Superchat is an Emacs-native Agent Runtime.**

近期工作优先强化：

- immutable Request 与异步 Run；
- 明确、唯一的生命周期和 terminal-state closure；
- Runtime、Authority、Tape 的统一执行脊柱；
- Surface-independent 的提交、观测、取消与结果投递；
- Agent、Workflow、Subagent、Team、Cowork 共用同一 Runtime 语义。

Cowork Telegram 当前的意义是验证这些 Runtime 契约能否承载真实外部 Surface。
`llm-buddy` 所启发的环境式 findings 机制，在 Runtime 基础稳定且出现真实产品需求时
再重新评估。
