# Agentic Skills 快速开始

## 安装

确保文件结构如下：
```
superchat/
├── superchat-executor.el    ; 核心执行引擎
├── superchat-skills.el      ; Agentic Skills 实现
├── superchat-workflow.el    ; 兼容层（可选）
├── AGENTS.md                ; Skill 索引
├── skills/                  ; Skill 文件目录
│   ├── code-review.md
│   ├── planning.md
│   └── refactor.md
└── ...
```

## 使用方式

### 1. 显式调用

输入 `>` 前缀 + skill 名称：

```
>code-review #file.el 帮我检查这段代码
>planning 我要做个新功能
>refactor 优化这个函数
```

按 `TAB` 键可以补全 skill 名称。

### 2. 隐式匹配（智能）

直接描述你的需求，系统自动匹配合适的 skill：

| 你说 | 系统自动使用 |
|-----|------------|
| "帮我 review 这段代码" | code-review |
| "重构这个函数" | refactor |
| "帮我做个新功能" | planning |
| "这段代码怎么样" | code-review |

### 3. 普通对话

如果系统没有匹配到合适的 skill，会作为普通对话处理。

## 配置

```elisp
;; 启用/禁用隐式匹配 (默认: t)
(setq superchat-skills-implicit-match-p t)

;; 设置匹配置信度阈值 (默认: 0.7)
;; 值越高，匹配越严格
(setq superchat-skills-match-confidence-threshold 0.7)

;; 自定义 skills 目录
(setq superchat-skills-directory "~/.emacs.d/superchat/skills/")
```

## 创建新 Skill

1. 在 `skills/` 目录创建 `.md` 文件：
   ```bash
   touch skills/my-skill.md
   ```

2. 编写 skill 内容：
   ```markdown
   # My Skill
   
   你是 [角色描述]。
   
   ## 任务
   
   当用户请求时，你应该：
   1. [步骤 1]
   2. [步骤 2]
   3. [步骤 3]
   
   ## 输出格式
   
   [描述期望的输出格式]
   ```

3. 在 `AGENTS.md` 注册：
   ```markdown
   ### my-skill
   - **name**: my-skill
   - **context**: [描述何时使用这个 skill]
   - **triggers**: ["关键词1", "关键词2"]
   - **file**: skills/my-skill.md
   ```

4. 测试：
   ```
   >my-skill 测试请求
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
;; 输入: 帮我 review 代码
```

### 常见问题

**Q: 隐式匹配不工作**
- 检查 `superchat-skills-implicit-match-p` 是否为 `t`
- 检查 AGENTS.md 是否存在且格式正确
- 降低置信度阈值试试 `(setq superchat-skills-match-confidence-threshold 0.5)`

**Q: 误匹配太多**
- 提高置信度阈值 `(setq superchat-skills-match-confidence-threshold 0.8)`
- 使用显式调用 `>skill-name` 强制使用特定 skill

**Q: Skill 文件找不到**
- 检查 `superchat-skills-directory` 设置
- 确保文件扩展名是 `.md`、`.org`、`.txt` 或 `.el`

## 测试

```bash
# 运行所有测试
emacs -batch -L . -l test/test-skills.el -l test/test-skills-integration.el -f ert-run-tests-batch-and-exit
```

完整测试指南见 `test/SKILL_TESTING.md`。
