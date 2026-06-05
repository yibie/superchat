# Agentic Skills 快速开始

## 技能格式

superchat 使用 SKILL.md 格式。每个技能是一个目录，包含一个带 YAML frontmatter 的 `SKILL.md` 文件：

```markdown
---
name: my-skill           # 必填：技能名称
description: 技能描述     # 必填：一句话描述
version: "1.0"           # 可选，默认 "1.0"
type: prompt              # 可选，默认 "prompt"；有效值："prompt"、"workflow"
triggers:                # 可选，默认 []；用于隐式匹配的关键词
  - 关键词1
  - 关键词2
---

# Markdown 正文

正文内容——对于 type: prompt，这是系统提示词；对于 type: workflow，每一行非空非注释内容是一个步骤。
```

## type: prompt 示例

最常用的类型。正文作为系统提示词注入：

```markdown
---
name: explain-region
description: 向资深工程师解释一段代码
version: "1.0"
type: prompt
triggers: ["explain this", "这段代码什么意思"]
---

向一位经验丰富但不熟悉这个代码库的工程师解释代码。关注：实际行为、结构原因、边界情况、隐藏依赖。
```

调用方式：`>explain-region #path/to/file.el`

## type: workflow 示例

多步骤工作流。每行一个步骤，`#` 开头是注释，空行被跳过：

```markdown
---
name: git-commit-message
description: 从 git diff 生成 conventional-commit 格式的提交信息
version: "1.0"
type: workflow
---

# 步骤 1：搜索格式规范
/web-search conventional commit format specification

# 步骤 2：生成提交信息
@claude-3-5-sonnet-20241022 根据上面的 diff 和格式规范，写一条 conventional commit 提交信息。
```

每一步都会解析 `@model`、`>skill`、`/command`、`#file`，和用户手动输入的行为一致。步骤按顺序执行，不支持分支。

## 字段规则

| 字段 | 必填 | 默认值 | 说明 |
|------|------|--------|------|
| `name` | 是 | — | 技能名称，用 `>name` 调用 |
| `description` | 是 | — | 一句话描述 |
| `version` | 否 | `"1.0"` | 版本号 |
| `type` | 否 | `"prompt"` | `"prompt"` 或 `"workflow"` |
| `triggers` | 否 | `[]` | 隐式匹配关键词列表 |

## 使用方式

### 显式调用

输入 `>` 前缀 + skill 名称：

```
>code-review #file.el 帮我检查这段代码
>planning 我要做个新功能
>explain-region 这段代码什么意思
```

按 `TAB` 键可以补全 skill 名称。

### 隐式匹配

直接描述需求，系统根据 `triggers` 字段自动匹配合适的 skill：

| 你说 | 自动使用 |
|-----|---------|
| "帮我 review 这段代码" | code-review |
| "explain this function" | explain-region |

### 普通对话

如果没有匹配到合适的 skill，作为普通对话处理。

## 安装技能

使用 `/skill-install` 从 URL 或 GitHub 仓库安装：

```
/skill-install https://example.com/my-skill.tar.gz
/skill-install user/repo@branch
```

## 查找技能

技能存放在以下位置（按优先级）：

1. `<superchat-data-directory>/skills/` — 用户安装的技能
2. `skills/`（仓库根目录）— 内置技能
3. `examples/standard-skills/` — 示例技能（供参考）

内置技能：`code-review`、`planning`、`refactor`
示例技能：`explain-region` (`type: prompt`)、`git-commit-message` (`type: workflow`)、`weekly-tech-digest` (`type: workflow`)

## 配置

```elisp
;; 启用/禁用隐式匹配 (默认: t)
(setq superchat-skills-implicit-match-p t)

;; 设置匹配置信度阈值 (默认: 0.7)
(setq superchat-skills-match-confidence-threshold 0.7)

;; 自定义 skills 目录
(setq superchat-skills-directory "~/.emacs.d/superchat/skills/")
```

## 调试

### 查看匹配结果

```elisp
;; 查看最后一次匹配
superchat-skills--last-match-result

;; 诊断系统状态
M-x superchat-skills-diagnose

;; 测试特定输入
M-x superchat-skills-test-match
```

### 常见问题

**Q: 隐式匹配不工作**
- 检查 `superchat-skills-implicit-match-p` 是否为 `t`
- 降低置信度阈值 `(setq superchat-skills-match-confidence-threshold 0.5)`

**Q: 误匹配太多**
- 提高置信度阈值 `(setq superchat-skills-match-confidence-threshold 0.8)`
- 使用显式调用 `>skill-name`

**Q: Skill 文件找不到**
- 检查 `superchat-skills-directory` 设置
- 确保目录内有 `SKILL.md` 文件

## 更多信息

- README 核心概念：[README.md#core-concepts](../README.md) 中的 "Skills (SKILL.md)" 和 "Workflows (type: workflow)" 章节
- 架构文档：[docs/architecture.md](architecture.md) 中的 "Adding a new skill" 章节
- 完整测试指南：[test/SKILL_TESTING.md](../test/SKILL_TESTING.md)
