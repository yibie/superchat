# Skills 快速开始

## 创建并使用本地技能

Superchat 的日常技能发现使用一个目录：`superchat-skills-directory`，
默认是 `<superchat-data-directory>/skills/`。在该目录直接保存
`explain-region.md`，而不是建立 `explain-region/SKILL.md` 子目录：

```markdown
---
name: explain-region
description: 解释代码的行为、结构与边界
version: "1.0"
type: prompt
triggers: ["explain this", "解释代码"]
---

向不熟悉这个代码库的工程师解释代码。关注实际行为、设计原因、边界条件，
并区分代码证据和推测。
```

在聊天输入区使用文件名（不含扩展名）：

```text
>explain-region #path/to/file.el
```

`name` 是显示元数据；显式查找使用文件的 basename。文件名与 `name`
保持一致最容易理解。无 frontmatter 的普通文本也可以作为技能正文。
支持的扩展名由 `superchat-skills-file-extensions` 控制。

初始化时，空的用户技能目录会从包内 `skills/` 播种默认文件。
包目录与 `examples/standard-skills/` 不是另一套自动覆盖用户目录的搜索层。

## 技能与 Workflow 不同

- `type: prompt`：为当前请求提供提示内容。
- `type: agent`：Agent Profile；用稳定的 `profile_id` 与明确的 `capability`
  描述身份和默认工作目的。工具、推理及其他字段见 [README](../README.md#core-concepts)。
- `type: plan`：规划模式。

新建多步骤配方请使用独立 `.workflow` 文件，不采用旧 `type: workflow`
教程。运行 `M-x superchat-workflow-edit` 创建或编辑，普通保存后通过
`/workflow <name>` 或 `>>name` 执行。`/workflow ` 后可以继续补全名称。

## 标准目录格式的导入与导出

其他工具常用 `<name>/SKILL.md` 布局。Superchat 的交换适配器是
`superchat-skills-standard.el`，与本地平铺文件发现分开：

```elisp
(require 'superchat-skills-standard)
```

- `M-x superchat-skills-standard-import`：选择包含多个技能子目录的父目录。
  每个子目录有 `SKILL.md`；导入在 `superchat-skills-directory` 写出
  `<name>.md`。先确保目标目录存在。当前导入只提取正文，不保留源文件的
  YAML 元数据或复制整个 `references/`、`scripts/` 目录；同名目标可能被覆盖，
  导入前备份已有文件，需要 Profile 元数据时在本地文件中补充。
- `M-x superchat-skills-standard-export`：选择一个本地技能和目标父目录，
  输出 `<name>/SKILL.md`，包含导出的元数据与正文。同名目标也可能被覆盖。
- `M-x superchat-skills-standard-list`：查看配置的标准目录中发现的技能。
  这个列表不表示这些技能都已经导入到本地 `>name` 发现目录。

标准目录位置由 `superchat-skills-standard-directories` 配置。
交换示例见 [examples/standard-skills](../examples/standard-skills/README.md)。

## 配置与诊断

```elisp
(setq superchat-skills-directory
      (expand-file-name "skills/" superchat-data-directory))
(setq superchat-skills-implicit-match-p t)
(setq superchat-skills-match-confidence-threshold 0.7)
```

隐式匹配依赖当前技能索引和匹配配置，不保证某一句话必定选中指定技能。
需要确定选择时使用 `>name`。

查不到技能时，核对文件是否直接位于 `superchat-skills-directory`、扩展名是否
受支持，以及输入的是否是 basename，而不是只存在于 frontmatter 的显示名称。
可使用 `M-x superchat-skills-diagnose` 和 `M-x superchat-skills-test-match`
查看诊断与匹配情况。

实现参考：[Skills](../superchat-skills.el)、[Preset](../superchat-preset.el)、
[交换适配器](../superchat-skills-standard.el)。
