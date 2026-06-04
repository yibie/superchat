# Phase 2 — 全面模块化

> 目标：把 Phase 1 留下的硬编码全部拆成 hook，command dispatch、skill lifecycle 都变成可替换的 plugin。

## Step 2.1：Hook registry（优先级 + ID 管理）

当前用 `add-hook` 裸注册，顺序靠添加顺序决定。需要一层薄封装：

```elisp
;; superchat-core.el 新增

(defvar superchat-hook-registry (make-hash-table :test 'equal)
  "Hook name → sorted alist of (priority . (id . function)).")

(defun superchat-register-hook (hook-name fn &optional id priority)
  "Register FN on HOOK-NAME with optional PRIORITY (default 50, lower = earlier)."
  (let* ((id (or id (symbol-name (gensym "sch-"))))
         (prio (or priority 50))
         (entry (cons prio (cons id fn)))
         (entries (gethash hook-name superchat-hook-registry)))
    (puthash hook-name
             (sort (cons entry (cl-remove id entries
                                          :key #'cadr :test #'equal))
                   (lambda (a b) (< (car a) (car b))))
             superchat-hook-registry)))

(defun superchat-unregister-hook (hook-name id)
  (puthash hook-name
           (cl-remove id (gethash hook-name superchat-hook-registry)
                      :key #'cadr :test #'equal)
           superchat-hook-registry))

(defun superchat-core--run-hook-chain (turn hook-name _field)
  "Run registered hooks for HOOK-NAME, accumulating changes."
  (let ((current turn))
    (dolist (entry (gethash hook-name superchat-hook-registry))
      (let ((fn (cddr entry)))
        (when (functionp fn)
          (condition-case err
              (when-let ((modified (funcall fn (superchat-turn-copy current))))
                (when (superchat-turn-p modified)
                  (setq current modified)))
            (error
             (message "superchat-core: hook %s failed: %s"
                      hook-name (error-message-string err)))))))
    current))
```

Hook 注册从 `(add-hook 'superchat-build-prompt-functions #'fn)` 变成：

```elisp
(superchat-register-hook 'superchat-build-prompt-functions
  #'superchat--hook-file-context "file-context" 100)
(superchat-register-hook 'superchat-build-prompt-functions
  #'superchat--hook-memory-inject "memory-inject" 200)
```

优先级：100（文件）→ 200（memory）→ 300（history）→ 500（默认 skill）。

## Step 2.2：Command dispatch → hook chain

当前 `superchat--handle-command` 是个大 pcase，~100 行。Phase 2 拆成：

```elisp
(defvar superchat-command-hooks nil
  "Each function: (command args input lang target-model) → result-plist or nil.
Run with `run-hook-with-args-until-success'.")

(defun superchat--handle-command (command args input lang target-model)
  (or (run-hook-with-args-until-success
       'superchat-command-hooks command args input lang target-model)
      ;; Fallback: unknown command
      `(:type :echo :content ,(format "Unknown command: `/%s'." command))))
```

然后把每个命令的实现从 pcase 分支拆成独立的 hook 函数：

```elisp
;; superchat-memory.el
(superchat-register-hook 'superchat-command-hooks
  (lambda (command args input lang target-model)
    (when (equal command "recall")
      ...))
  "cmd-recall" 100)

(superchat-register-hook 'superchat-command-hooks
  (lambda (command args input lang target-model)
    (when (equal command "remember")
      ...))
  "cmd-remember" 100)
```

迁移清单：
- `recall` → superchat-memory.el
- `remember` → superchat-memory.el
- `reset` → superchat.el (builtin)
- `clear-context` → superchat.el
- `clear` → superchat.el
- `define` → superchat.el
- `commands` → superchat.el
- `skill-install` → superchat-skills.el
- `backend` / `models` / `mcp` / `mcp-start` → superchat.el (builtins)

## Step 2.3：Skill lifecycle hooks

当前 `superchat-skills.el` 有自己的初始化流程。Phase 2 把它接到 hook system：

```elisp
;; superchat-core.el
(defvar superchat-skill-load-functions nil
  "Abnormal hook: (skill-name) → skill-content-string or nil.
Called when a skill is first referenced.")

(defvar superchat-skill-invoke-functions nil
  "Abnormal hook: (skill-name args turn) → result-plist or nil.
Called to execute a skill with arguments.")
```

superchat-skills.el 使用 hook registry 注册自己为默认实现。

## Step 2.4：Tape replay → context builder hook

当前 `superchat--hook-conversation-history` 从内存中的 `superchat--conversation-history` 读历史。Phase 2 改为从 SQLite tape 重放：

```elisp
(defvar superchat-context-builder-functions nil
  "Abnormal hook: (turn) → modified-turn or nil.
Each function appends context to turn.prompt.
Default implementations: tape replay, file injection, memory injection.")

;; Default: replay recent tape entries as conversation context
(superchat-register-hook 'superchat-context-builder-functions
  #'superchat--hook-tape-replay "tape-replay" 300)

(defun superchat--hook-tape-replay (turn)
  "Replay recent tape entries for this session into the prompt."
  (when (fboundp 'superchat-db-tape-replay)
    (let* ((sid (superchat-turn-session-id turn))
           (anchor (superchat-db-tape-last-anchor sid))
           (entries (superchat-db-tape-replay sid (if anchor (car anchor) 0) 20))
           (context (superchat--format-tape-entries entries)))
      (when (> (length context) 0)
        (let ((new-turn (superchat-turn-copy turn)))
          (setf (superchat-turn-prompt new-turn)
                (concat context "\n\n" (superchat-turn-prompt new-turn)))
          new-turn)))))
```

## Step 2.5：清理

Phase 2 之后的文件结构：

```
superchat-core.el        ← turn struct + hook registry + pipeline runner（不变）
superchat.el             ← UI 层：buffer 管理、渲染、默认 hook、builtin commands
superchat-memory.el      ← memory plugin：recall/remember commands + auto-recall hook
superchat-skills.el      ← skill plugin：>skill-name lifecycle + skill-install command
superchat-tools.el       ← tool plugin：llm.el tool 注册
superchat-db.el          ← 存储层：SQLite tape + memory（不变）
superchat-executor.el    ← 执行引擎：变量替换 + context 管理（Phase 2 精简）
superchat-parser.el      ← 解析器（不变）
```

## 不做的事（和 Phase 1 一样）

- 不引入 async runtime
- 不引入 multi-channel（保持 org buffer 唯一 UI）
- 不做 multi-agent 编排
- 不拆分 llm-driver（llm.el 已经是独立包）
