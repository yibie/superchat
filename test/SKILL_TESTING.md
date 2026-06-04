# Agentic Skills 测试指南

## 快速开始

### 1. 启动测试环境

```bash
cd /path/to/superchat
emacs -Q -L . -l superchat.el
```

### 2. 运行自动化测试

```elisp
;; 运行单元测试
(ert-run-tests-batch-and-exit "^test-skills")

;; 运行集成测试
(load-file "test/test-skills-integration.el")
(ert-run-tests-batch-and-exit "^test-implicit")
```

## 手动测试清单

### 阶段 1: 基础功能测试

#### 1.1 显式 Skill 调用

| 测试项 | 输入 | 预期结果 |
|-------|------|---------|
| 基本调用 | `>code-review #file.el` | 加载 code-review skill |
| 带参数 | `>planning 我要做新功能` | 加载 planning skill |
| 不存在 skill | `>nonexistent` | 显示 "Skill not found" |
| 补全 | 输入 `>` 后按 TAB | 显示可用 skills 列表 |

**测试步骤:**
1. 启动 `M-x superchat`
2. 输入 `>code-review` 按 `C-c C-c`
3. 观察是否显示 "🎯 Using skill: code-review"

#### 1.2 Skill 文件加载

```elisp
;; 测试 skill 加载
(superchat-skills-load "code-review")
;; 应该返回 skill 文件的内容

(superchat-skills-exists-p "code-review")
;; 应该返回 t

(superchat-skills-get-available)
;; 应该返回 ("code-review" "planning" "refactor")
```

### 阶段 2: 隐式匹配测试

#### 2.1 代码审查场景

| 输入 | 预期匹配 | 验证点 |
|-----|---------|--------|
| `帮我 review 这段代码` | code-review | 显示 "🔍 Implicit skill match: code-review" |
| `check this code` | code-review | 同上 |
| `这段代码怎么样` | code-review | 同上 |
| `帮我看看这段代码` | code-review | 同上 |
| `代码有问题吗` | code-review | 同上 |

**测试步骤:**
1. 输入 `帮我 review 这段代码`
2. 按 `C-c C-c`
3. 检查消息栏是否显示匹配信息

#### 2.2 重构场景

| 输入 | 预期匹配 | 验证点 |
|-----|---------|--------|
| `重构这段代码` | refactor | 显示匹配信息 |
| `优化一下这个函数` | refactor | 同上 |
| `改进代码结构` | refactor | 同上 |
| `refactor this` | refactor | 同上 |

#### 2.3 规划场景

| 输入 | 预期匹配 | 验证点 |
|-----|---------|--------|
| `帮我做个新功能` | planning | 显示匹配信息 |
| `怎么实现这个功能` | planning | 同上 |
| `规划一下这个项目` | planning | 同上 |
| `如何做这个任务` | planning | 同上 |

#### 2.4 不触发场景 (负向测试)

| 输入 | 预期结果 | 验证点 |
|-----|---------|--------|
| `今天天气怎么样` | 无匹配 | 作为普通对话处理 |
| `你好` | 无匹配 | 同上 |
| `谢谢` | 无匹配 | 同上 |
| `推荐一本书` | 无匹配 | 同上 |

**重要:** 这些输入不应该触发任何 skill，应该直接作为普通对话。

### 阶段 3: 边界情况测试

#### 3.1 显式优先于隐式

**测试:** 输入 `>refactor 帮我 review 这段代码`

**预期:** 
- 应该使用 `refactor` skill（显式）
- 而不是 `code-review` skill（隐式匹配）

**验证:**
```elisp
(let ((result (superchat-skills-parse-input ">refactor 帮我 review 这段代码")))
  (message "Parsed skill: %s" (car result))
  ;; 应该输出 "refactor"
  )
```

#### 3.2 置信度阈值

```elisp
;; 测试不同阈值
(setq superchat-skills-match-confidence-threshold 0.3)  ; 宽松
(setq superchat-skills-match-confidence-threshold 0.7)  ; 中等 (默认)
(setq superchat-skills-match-confidence-threshold 0.9)  ; 严格
```

**测试:**
- 宽松: 更多匹配
- 严格: 更少匹配
- 默认: 平衡

#### 3.3 禁用隐式匹配

```elisp
(setq superchat-skills-implicit-match-p nil)
```

**测试:** 输入 `帮我 review 代码`
**预期:** 应该作为普通对话，不触发 code-review skill

### 阶段 4: Prompt 构建测试

#### 4.1 验证 Skill 注入

**测试步骤:**
1. 启用调试: `(setq superchat-debug t)`
2. 输入 `>code-review #file.el test`
3. 检查 *Messages* 缓冲区

**预期输出:**
```
[Skill Context]
=======================
# Code Review Skill

你是专业的代码审查助手...
=======================

[User Request]
test
```

#### 4.2 变量替换

**测试:**
```elisp
(let ((ctx (superchat-executor-context-create nil "用户输入内容")))
  (superchat-executor-replace-variables 
   "Skill says: $input"
   nil
   nil
   ctx))
;; 应该返回 "Skill says: 用户输入内容"
```

## 调试技巧

### 查看最后一次匹配结果

```elisp
superchat-skills--last-match-result
;; 返回类似:
;; (:skill "code-review" :confidence 0.8 :reasoning "User mentioned reviewing code")
```

### 查看注册的 Skills

```elisp
(superchat-skills--get-registry)
;; 显示从 AGENTS.md 解析的所有 skill 定义
```

### 手动测试匹配

```elisp
;; 测试关键词匹配
(superchat-skills--match-by-keywords 
  "帮我 review 代码"
  (superchat-skills--get-registry))
;; 返回: ("code-review" . 0.8)
```

### 跟踪执行流程

```elisp
;; 启用详细日志
(setq message-log-max t)

;; 测试时观察 *Messages* 缓冲区
```

## 常见问题排查

### 问题 1: 隐式匹配不工作

**检查清单:**
- [ ] `superchat-skills-implicit-match-p` 是否为 `t`
- [ ] AGENTS.md 是否存在且格式正确
- [ ] 置信度阈值是否过高

**调试:**
```elisp
;; 检查 AGENTS.md 路径
superchat-skills-agents-md-file

;; 检查是否能加载
(superchat-skills--load-agents-md)

;; 检查解析结果
(superchat-skills--get-registry)
```

### 问题 2: Skill 文件找不到

**检查:**
```elisp
;; 检查 skills 目录
superchat-skills-directory

;; 检查文件是否存在
(file-exists-p (expand-file-name "code-review.md" superchat-skills-directory))

;; 列出可用 skills
(superchat-skills-get-available)
```

### 问题 3: 匹配不准确

**调整:**
```elisp
;; 调整阈值
(setq superchat-skills-match-confidence-threshold 0.5)

;; 或修改 AGENTS.md 中的 triggers
```

## 性能测试

### 基准测试

```elisp
(benchmark-run 100
  (superchat-skills-try-implicit "帮我 review 代码"))
;; 应该在 0.1 秒内完成 100 次
```

### 内存检查

```elisp
;; 测试前
(garbage-collect)

;; 运行多次匹配
(dotimes (_ 1000)
  (superchat-skills-try-implicit "测试输入"))

;; 检查内存增长
(garbage-collect)
```

## 回归测试

在每次修改后运行:

1. **单元测试:** `ert-run-tests-batch-and-exit "^test-skills"`
2. **集成测试:** `ert-run-tests-batch-and-exit "^test-implicit"`
3. **手动测试:** 按上面的清单测试关键场景

## 报告问题

如果发现问题，请提供:

1. 输入文本
2. 预期行为
3. 实际行为
4. 以下调试信息:
   ```elisp
   (list
     :input "你的输入"
     :parsed (superchat-skills-parse-input "你的输入")
     :registry (superchat-skills--get-registry)
     :last-match superchat-skills--last-match-result)
   ```
