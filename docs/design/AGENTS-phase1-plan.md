# Superchat → Agent Framework 计划

> 目标：把 superchat 从"聊天客户端"升级为"Bub 风格的 hook-first agent runtime"。
>
> 分两个 Phase。Phase 1 只动主路径、引入 turn struct + 3 core hooks，行为完全不变。
> Phase 2 全面模块化、加 hook registry，等 Phase 1 稳定后再评估。

---

## Phase 1 — Turn struct + 核心 hook（目标 2-3 天）

### 设计目标

- 外部行为 100% 不变，所有现有测试照过
- 内部把硬编码的 `superchat-send-input` → `execute-llm-query` → `llm-generate-answer` 主路径换成 turn struct + hook chain
- memory / skill / tool 各自成为独立 hook plugin

### Step 1.1：定义 `superchat-turn` struct

```elisp
(cl-defstruct superchat-turn
  "One conversation turn. Hooks read and modify fields through this struct."
  (id nil :read-only t)           ; uuid, set once at creation
  (inbound "")                     ; raw user input
  (clean-input "")                 ; after parsing (model switch, skill prefix removed)
  (target-model nil)               ; @model override, or nil
  (skill nil)                      ; parsed skill name, or nil
  (command nil)                    ; parsed command name, or nil
  (command-args "")                ; args after command
  (context-files nil)              ; #file references
  (system-prompt "")               ; built by system-prompt hooks
  (prompt "")                      ; final prompt sent to LLM
  (tools nil)                      ; collected tools for this turn
  (retrieved-memories nil)         ; from auto-recall or /recall
  (streaming-parts nil)            ; accumulated stream chunks
  (llm-result "")                  ; final LLM response
  (tape-records nil)               ; list of (kind . content) to be appended
  (state (make-hash-table :test 'equal))  ; arbitrary key-value store for hooks
  (error nil))                     ; error plist if turn failed
```

放在新文件 `superchat-core.el` 里。这个文件是新的枢纽——不包含 UI 逻辑，只定义 turn struct + hook 协议 + pipeline runner。

### Step 1.2：定义三个核心 hook

```elisp
;; Hook 1: 构建 system prompt（系统指令层）
;; Function: (turn) → modified-turn or nil
;; 多个 hook 都返回时，按 add-hook 顺序累加 system-prompt
(defvar superchat-system-prompt-functions nil
  "Abnormal hook run to build the system prompt for a turn.
Each function receives a `superchat-turn' struct and may return
a modified copy.  If it returns nil, the turn is unchanged.")

;; Hook 2: 构建最终 prompt（消息上下文层）
;; Function: (turn) → modified-turn or nil
(defvar superchat-build-prompt-functions nil
  "Abnormal hook run to build the final prompt for a turn.
Each function receives a `superchat-turn' struct and may return
a modified copy.  Append-only: hooks should concat to turn-prompt.")

;; Hook 3: 结果后处理（tape 记录、memory 抽取等）
;; Function: (turn) → nil (side-effect only)
(defvar superchat-post-turn-functions nil
  "Normal hook run after the LLM response is received.
Each function receives the final `superchat-turn' struct (read-only).")
```

### Step 1.3：Pipeline runner

在 `superchat-core.el` 中实现：

```elisp
(defun superchat-core-run-turn (turn)
  "Execute one full turn through the hook pipeline.  Returns the final turn."
  ;; Step order (Bub-style):
  ;;   resolve → parse → load-state → build-system → build-prompt
  ;;   → collect-tools → llm-call → stream → render → save-state → record
  (let ((t (superchat-turn-copy turn)))
    ;; 1. Parse raw input
    (superchat-core--parse-input t)
    ;; 2. Run system prompt hooks
    (dolist (fn superchat-system-prompt-functions)
      (when-let ((modified (funcall fn (superchat-turn-copy t))))
        (setq t modified)))
    ;; 3. Run build-prompt hooks (including memory injection, file injection)
    (dolist (fn superchat-build-prompt-functions)
      (when-let ((modified (funcall fn (superchat-turn-copy t))))
        (setq t modified)))
    ;; 4. Collect tools
    (superchat-core--collect-tools t)
    ;; 5. LLM call (delegated to llm-driver)
    (superchat-core--call-llm t)
    ;; 6. Run post-turn hooks
    (run-hook-with-args 'superchat-post-turn-functions t)
    t))
```

### Step 1.4：把现有功能迁移为 hook 实现

每个迁移都是"从 `superchat.el` 里抽出来，变成一个 hook 函数"。

**迁移 A：memory auto-recall → build-prompt hook**

```elisp
;; superchat-memory.el
(defun superchat-memory--auto-recall-hook (turn)
  "Auto-recall hook: inject retrieved memories into prompt."
  (when (superchat--input-meets-memory-threshold-p
         (superchat-turn-clean-input turn))
    (let ((memories (superchat-db-memory-search-simple
                     (superchat-turn-clean-input turn) 5)))
      (when memories
        (let ((t2 (superchat-turn-copy turn)))
          (setf (superchat-turn-retrieved-memories t2) memories)
          t2)))))
(add-hook 'superchat-build-prompt-functions #'superchat-memory--auto-recall-hook)
```

**迁移 B：文件引用注入 → build-prompt hook**

```elisp
;; superchat.el
(defun superchat--file-context-hook (turn)
  "Inject inline file content into prompt."
  (when-let ((files (superchat-turn-context-files turn)))
    (let ((t2 (superchat-turn-copy turn))
          (parts (mapcar #'superchat--make-inline-context files)))
      (setf (superchat-turn-prompt t2)
            (concat (string-join parts "\n\n")
                    "\n\n"
                    (superchat-turn-prompt t2)))
      t2)))
(add-hook 'superchat-build-prompt-functions #'superchat--file-context-hook)
```

**迁移 C：conversation history → build-prompt hook**

```elisp
(defun superchat--history-context-hook (turn)
  "Inject conversation history into prompt."
  (let ((history (superchat--conversation-context-string 10)))
    (unless (string-empty-p history)
      (let ((t2 (superchat-turn-copy turn)))
        (setf (superchat-turn-prompt t2)
              (concat history "\n\n" (superchat-turn-prompt t2)))
        t2))))
```

**迁移 D：tape recording → post-turn hook**

```elisp
(defun superchat--tape-record-hook (turn)
  "Record turn to SQLite tape."
  (when (fboundp 'superchat-db-tape-append)
    (ignore-errors
      (superchat-db-tape-append (superchat-turn-id turn) "user"
                                 (superchat-turn-inbound turn))
      (superchat-db-tape-append (superchat-turn-id turn) "assistant"
                                 (superchat-turn-llm-result turn)))))
(add-hook 'superchat-post-turn-functions #'superchat--tape-record-hook)
```

### Step 1.5：重写 `superchat-send-input`

当前这个函数 ~140 行，包含解析、dispatch、auto-recall、LLM 调用、渲染。重写后变成 ~30 行：

```elisp
(defun superchat-send-input ()
  "Parse user input, run through hook pipeline, render result."
  (interactive)
  (let* ((raw (superchat--current-input))
         (turn (make-superchat-turn
                :id (format-time-string "%Y%m%d-%H%M%S")
                :inbound (string-trim raw))))
    (when (> (length (superchat-turn-inbound turn)) 0)
      ;; Finalize input line in buffer
      (superchat--finalize-input-line)
      (superchat--prepare-for-response)
      ;; Run the pipeline
      (let ((result (superchat-core-run-turn turn)))
        ;; Render the result into the buffer
        (superchat--render-turn result)))))
```

### Step 1.6：处理 command dispatch

当前 `superchat--handle-command` 是个大 pcase。在 Phase 1 中**保持原样**——command 解析结果写入 `turn.command` 和 `turn.command-args`，然后由一个独立的 `superchat-core--dispatch-command` 调用原 handler。Phase 2 再拆成 hook chain。

这样 Phase 1 的风险最小：command 部分完全不动。

### 文件变更清单

| 文件 | 操作 | 说明 |
|------|------|------|
| `superchat-core.el` | **新建** | turn struct + hook 定义 + pipeline runner |
| `superchat.el` | **修改** | `send-input` 重写为 ~30 行；迁移 4 个 hook 实现 |
| `superchat-memory.el` | **修改** | auto-recall 迁移为 build-prompt hook |
| `superchat-db.el` | 不变 | |

---

## Phase 2 — 全面模块化（未来评估）

### 前提条件

- Phase 1 稳定运行，无回归
- 有外部插件需求（第三方想写 superchat 扩展）

### 目标

- 拆 `superchat-core` / `superchat-ui` / `superchat-llm-driver` 独立模块
- 实现 hook registry（优先级、启用/禁用、ID 管理）
- command dispatch 拆成 hook chain
- skill lifecycle hook
- tape replay 作为 context builder hook

### Phase 2 不做的事

- 不引入 async runtime（Emacs 单线程够用）
- 不引入 multi-channel（保持 org buffer 唯一 UI）
- 不做 multi-agent 编排（和 Bub 一样，定位是单 agent runtime）
