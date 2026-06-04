# Memory-Soul 分离架构设计思路

> 保存日期：2026-03-18  
> 来源：与 Claude 的讨论

---

## 核心洞察

**记忆 ≠ 知识管理**

正如 OpenClaw 里的 Soul.md 和 Memory.md 是分开的一样：
- **Memory** = 原始的、情境化的、可矛盾的、带有时间性和情绪的真实体验
- **Soul/知识** = 提炼的、结构化的、一致的、稳定的抽象模型

---

## 当前 superchat-memory.el 的问题

过度"知识化"记忆：

1. **Tier 2 LLM 摘要丢失原始情境** - "今天真是糟糕的一天" → "用户对工作不满意"（丢失了情绪和具体事件）
2. **合并功能消除矛盾** - 但真实的记忆本来就是矛盾的
3. **时间衰减让旧记忆"消失"** - 但有时"当时的感觉"很重要

---

## 改进方向：双轨架构

```
┌─────────────────────┐    ┌─────────────────────┐
│   Memory.org        │    │     Soul.org        │
│   (原始记忆库)       │    │   (派生人格模型)     │
├─────────────────────┤    ├─────────────────────┤
│ • 原始对话片段       │    │ • Insights (审阅后)  │
│ • 时间戳精确         │    │ • 稳定的用户画像     │
│ • 情绪标签 :MOOD:   │    │ • 长期偏好           │
│ • 可矛盾、可过期     │    │ • 价值观             │
│ • 像日记一样保留     │    │ • 像人物小传         │
└─────────────────────┘    └─────────────────────┘
```

---

## 具体改进建议

### 1. 保留原始记忆

```elisp
(superchat-memory-add-raw
  :content "今天真是糟糕的一天，我被老板骂了"
  :mood "frustrated"
  :context "工作场景"
  :verbatim t)  ;; 不摘要
```

### 2. 分离 Soul 文件

- `memory.org`：保留所有原始互动（像日记）
- `soul.org`：仅存储审阅后的稳定洞察（像人物小传）

```elisp
(defun superchat-memory-synthesize-soul ()
  "从 memory.org 提取洞察写入 soul.org
   但不删除原始记忆！")
```

### 3. 拥抱矛盾

```org
* 用户的编程语言偏好 :CONTRADICTION:
:PROPERTIES:
:VALIDITY: expired  ;; 标记过期，但不删除
:REPLACED_BY: yyy
:END:
"Python 是最好的语言" [2023-06-01]

* 用户的编程语言偏好 :CONTRADICTION:
:PROPERTIES:
:VALIDITY: current
:REPLACES: xxx
:END:
"我现在更喜欢 Rust" [2024-03-15]
```

### 4. 检索时情境重建

不仅返回记忆内容，还返回：
- 前后文对话（±3 条消息）
- 当时的情绪状态
- 时间背景

### 5. 减少自动化，增加人工审阅

```elisp
(defcustom superchat-soul-synthesis-mode 'manual
  "Soul 提炼模式：manual / weekly / never")

(defun superchat-memory-review-for-soul ()
  "交互式审阅界面，人工决定何时从 Memory 提炼到 Soul")
```

---

## 核心哲学转变

```
从：记忆 = 需要压缩和清理的数据
到：记忆 = 需要保留和情境化的体验

从：让系统更"智能"地自动管理
到：让系统更"忠实"地保存，把理解留给用户
```

---

## 下一步行动（待决定）

- [ ] 在 superchat-memory.el 中实现 `add-raw` 函数
- [ ] 创建 soul.org 文件结构
- [ ] 设计情绪标签体系 (:MOOD:)
- [ ] 实现矛盾记忆共存机制
- [ ] 设计人工审阅界面
