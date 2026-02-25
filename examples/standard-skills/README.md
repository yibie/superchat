# 标准格式 Skills 示例

这是 OpenAI/Anthropic 风格的 standard skill 格式示例。

## 目录结构

```
code-review/
├── SKILL.md              # 主要 skill 定义
└── references/           # 参考资料
    └── checklist.md     # 检查清单
```

## 关键特性

### 1. YAML Frontmatter

```yaml
---
name: code-review
description: Review code quality
triggers:
  - review
  - check
version: "1.0"
---
```

### 2. 渐进式披露

Agent 先看到 frontmatter，需要时才加载完整内容：

```elisp
;; Step 1: 只读取元数据
(metadata :name "code-review" 
          :description "Review code..."
          :triggers ["review" ...])

;; Step 2: 需要时才读取完整内容
(load-skill-content "code-review")
```

### 3. 参考资料分离

大型参考资料放在 `references/` 目录，保持 SKILL.md 简洁。

## 在 Superchat 中使用

### 方法 1：直接导入

```elisp
(require 'superchat-skills-standard)

(superchat-skills-standard-import 
  "~/superchat/examples/standard-skills/")

;; 导入后会自动转换为 superchat 格式
```

### 方法 2：作为标准格式支持

```elisp
(setq superchat-skills-standard-directories
      '("~/superchat/examples/standard-skills/"))

(superchat-skills-standard-initialize)

;; 标准 skills 会自动合并到可用技能列表
```

## 转换对比

### Superchat 原生格式

```markdown
<!-- skills/code-review.md -->
# Code Review

审查代码时注意：...
```

```markdown
<!-- AGENTS.md -->
### code-review
- **triggers**: ["review", "check"]
```

### 标准格式（本示例）

```markdown
<!-- code-review/SKILL.md -->
---
name: code-review
triggers: ["review", "check"]
---

# Code Review

审查代码时注意：...
```

## 跨平台使用

这个 skill 格式可以被以下平台使用：

- **OpenAI Codex**: 直接兼容
- **Claude**: 通过适配器
- **Superchat**: 通过 `superchat-skills-standard.el`
- **其他 MCP 客户端**: 通过标准接口

## 扩展阅读

- [格式转换完整指南](../../docs/SKILL_FORMAT_CONVERSION.md)
- [SKILL_FORMAT_CONVERSION.md](../../docs/SKILL_FORMAT_CONVERSION.md)
