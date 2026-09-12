# Superchat 插件开发指南

Superchat v2 里，**一切都是插件**：驱动器、模型循环、Authority、Tape、聊天窗口和你写的东西都用同一套机制挂进内核。内核只认识五样东西——插件/Scope、Service、Stage、Event、Invariant——和两种数据形状：Envelope 与 Sink。设计文档是 `docs/design/plugin-architecture.md`；参考实现是 `plugins/superchat-cowork-telega/`。

## 分层与你的位置

```text
Kernel（内核，不含任何产品逻辑）
├── superchat-plugin.el    —— superchat-define-plugin、Scope、挂载/卸载/重配置、superchat-plugins
├── superchat-service.el   —— Service 定义（名字 → 方法表）、superchat-service-call / -get
├── superchat-stage.el     —— 七个固定 Stage + Event 注册表（observe / first / fold / collect）
├── superchat-sink.el      —— Sink 结构与 superchat-sink-emit
├── superchat-invariant.el —— 六条 Invariant 与 seam 检查（违反写 invariant.violated 到 Tape）
└── superchat-pipeline.el  —— Envelope 校验、Surface 列表、驱动器主线

默认插件（随包发布、默认挂载、可卸载可替换）
└── transport-llm, driver, tape, session-view, authority, tools, tools-baseline,
    chat-view, chat-commands, chat-turn-recorder, chat-run-notices, closure,
    editor-workspace, change-set, memory, experience, plan, agents-panel, compact

第三方插件（你写的）
└── 同一个 superchat-define-plugin；require 内核文件，其余能力一律经 Service 拿
```

规则只有一条：**插件只通过它收到的 Scope 贡献东西**。卸载插件（或销毁 Scope）时，它注册的工具、命令、策略、Stage 实现、Event 监听全部撤回。Elisp 无法阻止你在 `apply` 里 `defun` 全局函数——那是 review 时检查的约定，不是内核能强制的。

## 插件四件套

```elisp
(superchat-define-plugin my-plugin
  :inject  (driver tools)                      ; 需要的 Service；齐了才 apply，少了就挂起
  :config  ((:limit natnump 5 "Max items"))    ; 每次激活都校验；非法配置 = failed，什么都不注册
  :doc     "One line for `superchat-plugin-report'."
  :apply   (lambda (scope config)              ; 一切注册都经 SCOPE
             …))
```

- `:inject` 列出的 Service 有一个没有提供者，插件就处于 **pending**；提供者出现（或换了一个）时自动重新 `apply`，消失时自动撤回。
- `:config` 的形状是 `((KEY PREDICATE DEFAULT [DOC]) …)`；`superchat-plugin-reconfigure` = 校验、撤回、重新 apply。
- 挂载：把名字加进 `superchat-plugins`（`(add-to-list 'superchat-plugins 'my-plugin t)` 后 `(superchat-plugin-mount-defaults)`），或运行时 `(superchat-plugin-mount 'my-plugin CONFIG)`；`superchat-plugin-unmount` 撤回；`superchat-plugin-report` 列出状态。

## 在 apply 里能做什么

| 你想做什么 | 怎么做 | 说明 |
|---|---|---|
| 给模型添加工具 | `(superchat-register-tool NAME :function … :description … :args … :effects … [:surfaces (…)])` | `:effects` 必填；`:surfaces` 限定哪些 Surface 的默认工具面会提供它（否则只能被显式 `:tools` 点名） |
| 添加 `/xxx` 命令 | `(superchat-register-command "xxx" #'handler)` | handler 收 `(cmd args input lang model)`，返回结果 plist |
| 添加命名 Authority 策略 | `(superchat-authority-register-policy 'name #'decide)` | 只能收紧：返回 `deny` / `ask` / `defer` |
| 为工具声明 effects / 审批预览 | `superchat-authority-register-effect-resolver` / `-register-approval-preview` | |
| 声明一个 Surface | `(superchat-register-surface 'my-surface)` | Envelope 的 `:surface` 必须已声明 |
| 往提示词里加内容 | `(superchat-stage-add 'collect-context 'my:id FN :owner 'my-plugin)` | FN `(env state tools)` 返回片段列表（见下） |
| 替换工具选择的默认策略 | `(superchat-stage-add 'select-tools 'my:tools FN :owner 'my-plugin)` | 后注册的先运行，返回非 nil 即胜出 |
| 排队 / 拒绝一条请求 | `(superchat-stage-add 'admit 'my:gate FN :owner 'my-plugin)` | 返回 `(:action queue\|drop\|process …)` |
| 监听生命周期 | `(superchat-on scope 'run-completed 'my:id FN)` | Event 列表见下 |
| 提供或替换一个 Service | `(superchat-service-provide scope 'view TABLE)` | TABLE 用 `superchat-service-table` 构造；先卸载原提供者（同名只能有一个） |

以上每一个调用在插件的 `apply` 里执行时都会自动把撤销动作记在 Scope 上（`superchat-plugin-record-undo`）。

### 提示词片段

```elisp
(superchat-stage-add
 'collect-context 'my:git-blame
 (lambda (env _state _tools)
   (when (eq (plist-get env :surface) 'chat)
     (when-let* ((file (plist-get (plist-get env :context) :chat-file-ref)))
       (list (superchat-prompt-fragment
              :owner 'my-plugin :id "my:git-blame" :slot 'context
              :text (shell-command-to-string (format "git blame -L 1,20 -- %s" file))
              :priority 0 :budget-class 'soft)))))
 :owner 'my-plugin)
```

片段带 owner、slot（`system` `instruction` `attachment` `context` `memory` `history` `experience` `plan` `template`）、优先级和预算类别；组装器按确定顺序排列，预算不够时先丢/缩 soft 片段，hard 片段永不丢。**收集器要自己按 `:surface` 把关**——所有 Surface 都走同一条主线。

### Service 目录

| Service | 方法 | 默认提供者 |
|---|---|---|
| `driver` | submit, submit-blocking, cancel, steer, status, active-runs, pending-input, answer-input, enqueue-follow-up, pending-follow-ups, clear-follow-ups, terminal-p | driver |
| `model-transport` | generate, capabilities | transport-llm（验证用 transport-mock） |
| `tape` | append-event, append-event-record, append-material, append-anchor, replay, register-projection, replay-projection, catch-up-projection, storage-reference-p, exact-json, select | tape |
| `session` | entries, search, assemble-view, superseded-ids, history, configuration, session-ids, anchor-latest, anchor-source-ids, expand-anchor, item-line | session-view |
| `authority` | decide, resolve-effects, register-policy, register-effect-resolver, register-approval-preview, transport-timer-owner | authority |
| `tools` | register, descriptors, materialize, execute | tools |
| `view` | sink-for, markdown-to-org | chat-view |
| `commands` | register, lookup, list | chat-commands |
| `closure` | judge | closure |
| `editor-workspace` | validate-buffer, buffer, owner-session, acquire-buffer, rollback-acquisition, close | editor-workspace |
| `change-set` | apply | change-set |
| `memory` | insert, search, search-prioritized, get, list-review, set-review, count, reset-projection | memory |
| `recovery` | assemble-view | experience（可选：driver 用 `superchat-service-get` 探测） |

调用：`(superchat-service-call nil 'driver 'cancel run-id)`；可选依赖用 `(superchat-service-get nil 'recovery)` 探测，不要用 `fboundp`。Service 只能在全局 Scope 提供（Preset Scope 不能遮蔽 Service）。

### Stage 主线（顺序固定）

`resolve-session` → `admit` → `load-state` → `select-tools` → `collect-context` → `assemble-prompt` → *（调用 `model-transport`）* → `save-state`。每个 Stage 都有默认插件实现；插件在 Stage 内竞争，不能改顺序。

### Event 目录

| Event | 规则 | 参数 |
|---|---|---|
| `stage-error` | observe | stage condition envelope |
| `run-started` | observe | run envelope |
| `run-sealing` | observe | turn（封存前） |
| `run-completed` | observe | status（仅成功） |
| `turn-completed` | observe | status（所有终态） |
| `tool-before` / `tool-after` / `tool-failed` | observe | tool args [result\|error] |
| `tool-permission` | collect（deny 优先） | tool args |
| `tool-args-rewrite` | fold | args tool → args 或 `(:block REASON)` |
| `tool-result-filter` | fold | result tool args → 替换字符串或 nil |
| `before-provider-request` | fold | prompt-spec |
| `context-budget-exceeded` | first | details |
| `closure-shadow` | observe | report |
| `session-started` / `session-closed` | observe | session-id |

实参以 `superchat-stage.el` 里各 Event 的声明为准：Run 事件第一个参数是 Run 投影，`tool-before`/`tool-after` 最后一个参数是 run-id，`before-provider-request` 折叠 `(:prompt … :system …)`。

## 五条铁律

1. **一切执行经 driver Service。** 需要模型时构造 Envelope，`(superchat-service-call nil 'driver 'submit ENVELOPE)`；结果只经 Sink 到达。禁止直接调用 `llm-chat-*`。
2. **禁止阻塞。** `submit-blocking` 只属于同步边界适配器（Org Babel、Magit）。插件一律异步。
3. **工具必须声明 effects，且绝不自行确认。** 审批、审计、Tape 记录由 `tools.execute` 统一负责——工具体内出现 `y-or-n-p` 即是错误。
4. **命名策略只能收紧。** `allow` 会被降级为 `defer`。
5. **命名空间。** 插件符号用 `superchat-<插件名>-` 前缀；双横线符号是内部实现，不在兼容承诺内。

## 完整示例：superchat-fortune.el

```elisp
;;; superchat-fortune.el --- Fortune plugin for Superchat -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "28.1") (superchat "1.4"))

(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-sink)

(defun superchat-fortune--cookie ()
  "Run the fortune binary and return its output."
  (string-trim (shell-command-to-string "fortune")))

(defun superchat-fortune--cmd (_cmd _args _input _lang _model)
  "Show a fortune in the chat buffer."
  (list :type :echo :content (superchat-fortune--cookie)))

;;;###autoload
(defun superchat-fortune-explain ()
  "Ask the model to explain today's fortune, asynchronously."
  (interactive)
  (let* ((fortune (superchat-fortune--cookie))
         (session-id "fortune")
         (buffer (superchat-service-call nil 'editor-workspace 'buffer
                                         session-id 'fortune-explanation t)))
    (superchat-service-call
     nil 'driver 'submit
     (list :surface 'fortune
           :content (format "Explain this aphorism briefly:\n\n%s" fortune)
           :session-id session-id
           :attendance 'attended
           :tools 'none
           :context (list :host-buffer buffer)
           :sink (superchat-sink-callback
                  :final (lambda (_run result)
                           (message "Fortune: %s" (plist-get result :text)))
                  :error (lambda (_run failure)
                           (message "Fortune failed (%s): %s"
                                    (plist-get failure :status)
                                    (plist-get failure :text))))))
    (message "Consulting the oracle asynchronously...")))

(superchat-define-plugin fortune
  :inject '(driver tools commands editor-workspace)
  :doc "A fortune cookie: one tool, one command, one Surface."
  :apply
  (lambda (_scope _config)
    (superchat-register-surface 'fortune)
    (superchat-register-tool
     "fortune_cookie"
     :function #'superchat-fortune--cookie
     :description "Return one random fortune-cookie aphorism."
     :args nil
     :effects '((:effect process-exec :resource process :scope "fortune")))
    (superchat-register-command "fortune" #'superchat-fortune--cmd)))

;;;###autoload
(with-eval-after-load 'superchat
  (add-to-list 'superchat-plugins 'fortune t)
  (superchat-plugin-mount-defaults))

(provide 'superchat-fortune)
;;; superchat-fortune.el ends here
```

`(superchat-plugin-unmount 'fortune)` 之后：`/fortune` 消失、`fortune_cookie` 离开工具面、`fortune` Surface 不再被接受。`scripts/smoke.el` 的 U 用例对 Telega 做的就是这个检查。

## Request envelope 字段速查

`driver.submit` 接受的 envelope plist（必填 `:content`、`:surface`、`:sink`）：

| 字段 | 含义 |
|---|---|
| :content | 原始输入文本，非空；`intent.received` 记录的就是它 |
| :surface | 已声明的 surface 符号（`chat` `inline-ask` `inline-rewrite` `workflow` `subagent` `compaction` `skills` `external` …） |
| :sink | `superchat-sink-callback :final :error :stream` 构造的 Sink；`:final` 收 `(:text :tape-id)`，`:error` 收 `(:status :text :tape-id)`，`:stream` 收 `(:kind text\|reasoning :text …)` |
| :session-id | 持久会话 ID；缺省时由 `resolve-session` 解析，否则临时 ID |
| :attendance | `attended`（可审批，默认）或 `unattended`（副作用一律 deny） |
| :parent-run-id | 直接委托的父 Run |
| :context | 带 owner 前缀的事实 plist：`:host-buffer`（Runtime 宿主 buffer）、`:prepared-prompt` / `:prepared-system`（你已经准备好的完整用户消息，`prepared` 收集器把它变成 content 片段）、`:chat-*` `:inline-*` `:telega-*` … |
| :media | 显式媒体文件路径列表 |
| :tools | `none`、工具名列表（只收窄不扩展）或 nil（按 `select-tools` 决定） |
| :origin / :target-model / :capability / :topology / :authority-policy / :deadline / :system-prompt | 原样传给 Run |
| :runtime-extras | 尚未建模的键，原样复制到 Runtime Request（`:approval-function` `:tool-validator` `:preset` `:actor-id` `:record-conversation` …） |

旧式 Runtime Request（`:prompt` + `:stream-function` / `:delivery-function`）已不再被 `superchat-runtime-submit` 接受；`superchat-pipeline-envelope-from-request` 可以把一个旧 plist 翻译成 envelope。
