# Skills 快速开始

## 创建并使用本地技能

本地发现读取 `superchat-skills-directory` 中的平铺文件，默认目录是 `<superchat-data-directory>/skills/`。把下面内容保存为该目录下的 `explain-region.md`，不是 `explain-region/SKILL.md`：

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

在聊天中通过文件 basename 调用：

```text
>explain-region #path/to/file.el
```

`name` 是显示元数据，显式查找用文件名；二者保持一致最容易理解。无 frontmatter 的普通文本也可用作技能正文。支持的扩展名由 `superchat-skills-file-extensions` 控制。

空的用户技能目录会在初始化时从包内 `skills/` 播种默认文件。包目录和 `examples/standard-skills/` 不是另外两层自动搜索路径。

## 技能与 Workflow 不同

- `type: prompt` 为当前请求提供提示内容。
- `type: agent` 是固定默认 Capability 的 Preset；`profile_id` 表示稳定角色身份，不放宽权限档位。
- `type: plan` 选择规划模式。

多步骤配方使用独立 `.workflow` 文件。通过 `M-x superchat-workflow-edit` 编辑，保存后用 `/workflow <name>` 或 `>>name` 执行；不要根据旧的 `type: workflow` 教程创建技能文件。

## 使用目录式 SKILL.md 材料

`<name>/SKILL.md` 可以作为整理材料的格式，但它不是本地技能发现布局。当前 checkout 没有 `superchat-skills-standard.el`，也没有该前缀的通用本地 import/export/list 命令。

要使用这类材料，请先阅读内容，再把需要的提示词保存为 `superchat-skills-directory/<name>.md`。按上面的平铺示例整理受支持的 frontmatter；`references/` 和 `scripts/` 不会因复制 SKILL.md 自动载入或执行。不要覆盖已有同名技能。

仓库的 [standard-skills 示例](../examples/standard-skills/README.md)仅展示材料布局。`superchat-skills.el` 中另有 GitHub 安装路径，它能识别目录中的 SKILL.md 并写出平铺文件；这不等于通用本地导入导出功能。

## 配置与诊断

```elisp
(setq superchat-skills-directory
      (expand-file-name "skills/" superchat-data-directory))
```

需要确定选择时使用 `>name`。查不到技能，先核对文件是否直接位于用户技能目录、扩展名是否受支持、输入是否为 basename。

加载 Superchat 后，可以从同一个发现与解析接口检查：

```elisp
(superchat-skills-exists-p "explain-region") ; 文件存在时为非 nil
(superchat-skills-load "explain-region")     ; 返回 Preset，找不到时为 nil
```

`M-x superchat-skills-inspect` 展示文件位置和解析结果。这些检查不需要请求模型；配置变量的存在也不意味着任意一句普通聊天输入会自动选中技能。

实现参考：[Skills](../superchat-skills.el)、[Preset](../superchat-preset.el)。插件组合与 Preset 生命周期见[组合合同](design/plugin-architecture.md#preset-composition-and-lifetime)。
