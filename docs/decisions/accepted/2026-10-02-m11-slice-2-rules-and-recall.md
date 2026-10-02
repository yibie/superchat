# Decision: M11 第 2 片——规矩是 `AGENTS.md`，回忆不出 workspace

Status: accepted — 用户，2026-10-02："你可以继续走向下一片"。条目来自 [workspace 契约](2026-10-01-workspace-work-record-session-rules.md)的 R1–R5；"leader 的选择"一节是 leader 在这句话之下定的机制，随本片向用户报告。

## Problem

说过的偏好重启后不在（目标批次 P1，0/3）：说"记住……"不写任何东西；superchat 不读目录里的 `AGENTS.md`；自动回忆按对话划分，别的对话里只有 `/remember` 的记录能被带上，而且查询把中文里两个标点之间的整段当成一个词，几乎匹配不上（`.scratch/issues/2026-10-01-recall-never-matches-a-chinese-sentence.md`）。

## Decision

| 小片 | 内容 | 契约条目 |
|---|---|---|
| 2.1 | 回忆的查询能匹配中文：连续的中日韩文字切成相互重叠的三字片段 | R5 |
| 2.2 | 绑定了 workspace 的对话，每次请求的系统文字里带上该 workspace 根目录下 `AGENTS.md` 的内容（没有时读 `CLAUDE.md`） | R2、R3 |
| 2.3 | 告诉模型：用户明确说"记住"或"以后都这样"时，改 `AGENTS.md` 的 buffer 并说明写了什么；临时对话里说明这里不保存 | R4 |
| 2.4 | 自动回忆和 `memory_search` 只在本 workspace 的对话里找；临时对话只找它自己 | R1、R5 |
| 2.5 | 子代理的提示里带上同一份规矩 | R3 |

验收：各小片自己的回归；目标批次 P1 改在一个 workspace 里做（说"记住"、保存、重启、再写日记）；一个"在 A 记的规矩不出现在 B 的请求里"的真实任务。

## leader 的选择

1. **规矩读磁盘上的文件，不读未保存的 buffer。** 契约说用户保存才生效。
2. **上限 6000 个字符。** 超出的部分不带，末尾写明被截断和文件路径，模型可以自己去读。这个仓库的 `AGENTS.md` 是 5501 个字符。
3. **`/remember` 命令本片不改。** 它仍写 Tape 上的记录，这些记录按 2.4 只在本 workspace 内被带上。把它并到"改 `AGENTS.md`"留待以后。
4. **临时对话里说"记住"**：模型回答这里是临时的、不保存，并告诉用户执行 `/workspace <目录>` 把目录设为 workspace。契约 W5 说的"当场问是否设为 workspace"由用户执行这条命令来完成，superchat 不替用户建目录标记。
5. **本片之前的、不属于任何 workspace 的旧记录**不再自动带进任何别的对话。

## Alternatives considered

**规矩读 buffer 里未保存的文字。** 输在契约 R4：保存是用户的同意。

**不设上限，或按 token 预算截。** 前者让一份很长的 `AGENTS.md` 挤掉别的内容；后者要在每次请求里估算，字符上限够用且可预测。

**让模型自己建 `.superchat/` 把目录设为 workspace。** 输在往用户目录里写标记应当是用户自己的动作。

## Consequences

- 绑定了 workspace 的请求多带至多 6000 个字符；写给编程 agent 的 `AGENTS.md` 也会进 superchat 的请求。
- 临时对话的系统文字多一行说明，canonical smoke 的基线 A 随之变化。
- 别的对话里的 `/remember` 记录，跨 workspace 不再被带上。
