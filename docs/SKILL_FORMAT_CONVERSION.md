# Skill 格式转换指南

## 格式对比

### Superchat 格式（当前）

```
skills/
├── code-review.md          # 简单文本文件
├── planning.md
└── refactor.md

AGENTS.md                   # 自定义索引
```

**特点：**
- 简单，无结构要求
- 通过 AGENTS.md 描述触发条件
- 直接作为 prompt 注入

### 标准格式（OpenAI/Anthropic）

```
skills/
├── code-review/
│   ├── SKILL.md            # 必须，标准命名
│   ├── scripts/            # 可选，可执行脚本
│   │   └── analyze.sh
│   └── references/         # 可选，参考文档
│       └── checklist.md
└── planning/
    ├── SKILL.md
    └── references/
        └── template.md

agents/
└── openai.yaml             # 可选，UI元数据
```

**特点：**
- 结构化目录
- SKILL.md 支持 YAML frontmatter
- 渐进式披露（metadata → content）
- 支持脚本和参考资料

---

## 转换方案

### 方案 1：导入外部标准 Skills（使用 `superchat-skills-standard.el`）

```elisp
(require 'superchat-skills-standard)

;; 配置标准 skills 搜索路径
(setq superchat-skills-standard-directories
      '("~/.agents/skills/"
        "~/projects/my-project/.agents/skills/"))

;; 启用标准格式支持
(setq superchat-skills-support-standard-formats t)
(superchat-skills-standard-initialize)
```

自动合并标准 skills：
- 优先级：Native skills > Standard skills
- 同名 skill 优先使用 superchat 格式

### 方案 2：导出为其他平台可用格式

```elisp
;; 导出单个 skill
(superchat-skills-standard-export "code-review" "~/exported-skills/")

;; 结果
;; ~/exported-skills/code-review/SKILL.md
```

### 方案 3：批量转换脚本

```elisp
;; 转换所有 skills 到标准格式
(dolist (skill (superchat-skills-get-available))
  (superchat-skills-standard-export 
   skill 
   "~/my-skills-export/"))
```

---

## 格式转换对照表

### SKILL.md 结构

**Superchat 格式：**
```markdown
# Code Review Skill

你是专业的代码审查助手。

## 检查清单
- [ ] 代码是否符合惯例
```

**标准格式（转换后）：**
```markdown
---
name: code-review
description: Review code quality, check for issues
triggers: ["review", "check", "这段代码怎么样"]
---

# Code Review Skill

你是专业的代码审查助手。

## 检查清单
- [ ] 代码是否符合惯例
```

### 触发条件定义

**Superchat（AGENTS.md）：**
```markdown
### code-review
- **name**: code-review
- **context**: User asks to review code
- **triggers**: ["review", "check", "这段代码怎么样"]
- **file**: skills/code-review.md
```

**标准格式（SKILL.md frontmatter）：**
```yaml
---
name: code-review
description: User asks to review code
triggers:
  - review
  - check
  - 这段代码怎么样
---
```

---

## 渐进式披露实现

标准格式支持"渐进式披露"：Agent 先看到 metadata，需要时才加载完整内容。

```elisp
;; 在 superchat 中模拟渐进式披露
(defun superchat-skills-load-progressive (skill-name)
  "Load skill with progressive disclosure."
  (let* ((skill (superchat-skills--find-skill skill-name))
         (metadata (plist-get skill :metadata)))
    ;; Step 1: 只返回 metadata
    (if superchat-skills-use-progressive-disclosure
        metadata
      ;; Step 2: 或返回完整内容
      (superchat-skills-load skill-name))))
```

---

## 与 OpenAI Codex 的互操作

### 使用 Codex Skills

```bash
# 1. 复制 Codex skill 到 superchat
mkdir -p ~/.emacs.d/superchat/skills/my-codex-skill
cp ~/codex-skills/some-skill/SKILL.md \
   ~/.emacs.d/superchat/skills/my-codex-skill/

# 2. 创建 superchat 兼容版本
# SKILL.md 内容不变，superchat 会自动适配
```

### 导出到 Codex

```elisp
;; 导出 skill 到 Codex 格式
(superchat-skills-standard-export "code-review" "~/.agents/skills/")

;; Codex 可直接使用
# $ cd ~/.agents/skills/code-review
# $ ls
# SKILL.md
```

---

## 完整转换示例

### 转换前（Superchat）

```
skills/
└── code-review.md
```

**code-review.md:**
```markdown
# Code Review

审查代码时注意：
1. 正确性
2. 可读性
3. 性能
```

**AGENTS.md:**
```markdown
### code-review
- **name**: code-review
- **context**: Review code quality
- **triggers**: ["review", "check"]
- **file**: skills/code-review.md
```

### 转换后（标准格式）

```
skills/
└── code-review/
    ├── SKILL.md
    └── references/
        └── checklist.md
```

**SKILL.md:**
```markdown
---
name: code-review
description: Review code quality, check for issues
triggers: ["review", "check", "这段代码怎么样"]
---

# Code Review

审查代码时注意：
1. 正确性
2. 可读性
3. 性能
```

**references/checklist.md:**
```markdown
# Code Review Checklist

- [ ] Code follows conventions
- [ ] No obvious bugs
- [ ] Performance is acceptable
```

---

## 平台特定扩展

### OpenAI Codex (agents/openai.yaml)

```yaml
# skills/code-review/agents/openai.yaml
interface:
  display_name: "Code Review"
  short_description: "Review code for quality issues"
  icon_small: "./assets/icon.svg"
  
dependencies:
  tools:
    - type: "mcp"
      value: "lspServer"
```

### Claude (claude.json)

```json
{
  "name": "code-review",
  "description": "Review code quality",
  "allowed_tools": ["file_reader", "code_analyzer"]
}
```

---

## 推荐工作流

### 方案 A：Superchat 为主，兼容标准

1. 在 superchat 中开发 skills
2. 需要导出时，使用 `superchat-skills-standard-export`
3. 保持简单格式

### 方案 B：标准为主，多平台使用

1. 使用标准格式编写 skills
2. 通过 `superchat-skills-standard-initialize` 导入
3. Skills 可在多个平台使用

### 方案 C：混合策略

```
skills/
├── native/                 # Superchat 原生格式
│   ├── superchat-specific.md
│   └── emacs-specific.md
└── standard/               # 标准格式（跨平台）
    ├── code-review/
    └── planning/
```

```elisp
;; 同时支持两种格式
(setq superchat-skills-directory "~/.emacs.d/superchat/skills/native/")
(setq superchat-skills-standard-directories
      '("~/.emacs.d/superchat/skills/standard/"
        "~/.agents/skills/"))
```

---

## 迁移检查清单

从 Superchat 迁移到标准格式：

- [ ] 创建标准目录结构
- [ ] 添加 YAML frontmatter 到 SKILL.md
- [ ] 将触发条件从 AGENTS.md 移到 frontmatter
- [ ] 分离参考文档到 references/
- [ ] 添加可选脚本到 scripts/
- [ ] 测试转换后的 skill
- [ ] 更新文档

从标准格式迁移到 Superchat：

- [ ] 复制 SKILL.md 到 skills/
- [ ] 重命名为 skill-name.md
- [ ] 可选：在 AGENTS.md 中注册触发条件
- [ ] 测试 skill 加载
