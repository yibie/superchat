# Superchat Agentic Skills 开发阶段

## 当前阶段

**阶段**: 从 Workflow 迁移到 Agentic Skills  
**目标**: 简化 workflow 系统，采用更自然的 agentic skills 模式  
**开始日期**: 2026-02-07

## 设计理念

参考 OpenAI Codex Skills 设计和 Vercel 的 AGENTS.md 研究，我们认识到：

1. **渐进式披露**: Agent 只需要知道 skill 的情境描述，而非完整列表
2. **隐式触发**: 通过 AGENTS.md 索引让 Agent 理解何时使用什么 skill
3. **简化结构**: 不需要复杂的 YAML 或结构化格式，纯文本描述即可

## 架构变化

### 旧架构 (Workflow)
```
>workflow-name args
↓
解析 .workflow 文件
↓
逐行执行 (@model, /command, #file)
↓
多步骤流式输出
```

### 新架构 (Agentic Skills)
```
>skill-name args
↓
读取 skills/skill-name.md
↓
将 skill 内容注入 prompt
↓
单次 LLM 调用（skill 作为 system context）
```

## 文件结构

```
superchat/
├── AGENTS.md              # 本文件：开发阶段登记和技能索引
├── skills/                # Agentic Skills 目录
│   ├── code-review.md     # 代码审查技能
│   ├── refactor.md        # 代码重构技能
│   ├── planning.md        # 开发规划技能
│   └── ...
├── superchat-executor.el  # 核心执行引擎（变量替换、上下文、LLM调用）
├── superchat-skills.el    # Agentic Skills 实现
├── superchat-workflow.el  # 兼容层（deprecated，将移除）
└── superchat.el           # 主入口
```

## 迁移状态

### 已完成 ✅

- [x] 创建 `superchat-executor.el` - 核心执行引擎
- [x] 重构函数命名（workflow → executor）
- [x] 创建 `superchat-skills.el` - Agentic Skills 实现
- [x] 简化 `superchat-workflow.el` 为兼容层
- [x] 更新 `superchat.el` 引用新模块
- [x] 创建示例 skills (code-review, planning, refactor)

### 核心函数映射

| 旧名称 (workflow) | 新名称 (executor/skills) |
|------------------|------------------------|
| `superchat-workflow-initialize` | `superchat-executor-initialize` |
| `superchat-workflow-replace-variables` | `superchat-executor-replace-variables` |
| `superchat-workflow-context` | `superchat-executor-context` |
| `superchat-step-result` | `superchat-executor-result` |
| `superchat-workflow-parse-workflow-input` | `superchat-skills-parse-input` |
| `superchat-workflow-handle-workflow-command` | `superchat-skills-invoke` |
| `superchat-workflow-load-workflow` | `superchat-skills-load` |
| `superchat-workflow-completion-workflows` | `superchat-skills-completion-list` |

## Skill Registry (for Implicit Matching)

This section defines skills for automatic matching based on user intent.
Each skill has:
- **name**: Unique identifier (matches filename in skills/)
- **context**: When to trigger this skill
- **triggers**: Specific keywords or patterns
- **file**: Path to skill definition

### code-review
- **name**: code-review
- **context**: User asks to review code quality, check for issues, or asks "how is this code?"
- **triggers**: ["review", "check code", "这段代码怎么样", "代码有问题吗", "帮我看看"]
- **file**: skills/code-review.md

### refactor
- **name**: refactor
- **context**: User mentions refactoring, optimizing structure, or improving design
- **triggers**: ["refactor", "重构", "优化", "改进结构", "简化"]
- **file**: skills/refactor.md
- **note**: May need planning skill first

### planning
- **name**: planning
- **context**: Starting a new feature or task, user says "help me do X"
- **triggers**: ["plan", "规划", "帮我做", "怎么实现", "如何做"]
- **file**: skills/planning.md
- **note**: Must confirm user intent first

## 技能索引 (中文参考)

### code-review

**情境**: 用户要求审查代码质量、检查潜在问题、询问"这段代码怎么样"  
**触发**: 代码审查请求  
**文件**: `skills/code-review.md`

### refactor

**情境**: 用户提到重构、优化结构、改进设计  
**触发**: 代码重构请求  
**文件**: `skills/refactor.md`  
**依赖**: 可能需要先执行 planning

### planning

**情境**: 开始新功能、新任务、用户说"帮我做XX"  
**触发**: 任何开发任务开始前  
**文件**: `skills/planning.md`  
**要求**: 必须确认用户真实意图

## 开发顺序

1. ✅ **Phase 1**: 清理 workflow 相关代码
2. ✅ **Phase 2**: 创建 superchat-skills.el 基础框架
3. ✅ **Phase 3**: 实现 >skill-name 解析和调用
4. ✅ **Phase 4**: 创建示例 skill 文件
5. ✅ **Phase 5**: 实现隐式 skill 匹配
6. ✅ **Phase 6**: 测试和文档更新

## 测试

### 运行自动化测试

```bash
# 运行所有测试
cd /path/to/superchat
emacs -batch -L . -l test/test-skills.el -l test/test-skills-integration.el -f ert-run-tests-batch-and-exit

# 或使用测试脚本
emacs -batch -L . -l test/run-tests.el
```

### 手动测试

参见 `test/SKILL_TESTING.md` 完整测试指南。

### 调试工具

```elisp
;; 诊断系统状态
M-x superchat-skills-diagnose

;; 测试特定输入的匹配
M-x superchat-skills-test-match

;; 查看 skill 详情
M-x superchat-skills-inspect
```

## 隐式匹配 (Implicit Skill Matching)

Superchat 支持自动检测用户意图并调用合适的 skill，无需显式输入 `>skill-name`。

### 工作原理

1. 用户输入: `帮我看看这段代码有没有问题 #file.el`
2. 系统读取 AGENTS.md 中的 skill registry
3. 通过关键词匹配检测意图（code-review）
4. 自动加载 `skills/code-review.md` 作为上下文
5. 执行增强后的 prompt

### 配置选项

```elisp
;; 启用/禁用隐式匹配 (默认: t)
(setq superchat-skills-implicit-match-p t)

;; 设置匹配置信度阈值 (默认: 0.7)
(setq superchat-skills-match-confidence-threshold 0.7)
```

### 禁用特定会话的隐式匹配

使用 `>` 前缀强制显式选择 skill，或输入命令 `/` 开头的内容。

## 标准格式支持

Superchat 支持标准 skill 格式（OpenAI Codex/Anthropic 风格）：

```elisp
;; 导入标准格式 skills
(require 'superchat-skills-standard)
(superchat-skills-standard-import "~/.agents/skills/")

;; 或启用自动发现
(superchat-skills-standard-initialize)
```

支持双向转换：
- Superchat → 标准格式：`superchat-skills-standard-export`
- 标准格式 → Superchat：`superchat-skills-standard-import`

详见 `docs/SKILL_FORMAT_CONVERSION.md`

## 注意事项

- 保持向后兼容：暂时保留 `>` 语法，内部实现改为 skills
- 简化用户认知：用户不需要知道 workflow 和 skills 的区别
- 渐进式迁移：现有 workflow 文件可以逐步转换为 skills
- 隐式匹配保守策略：不确定时不匹配，避免干扰正常对话
- 跨平台兼容：Skills 可导出到其他 AI 平台使用
