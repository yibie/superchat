# Superchat 插件开发指南

Superchat 插件就是一个普通的 Emacs 包：`require` Superchat 的公开接缝，在加载时注册自己。没有插件框架、没有 manifest、没有专用打包格式——Emacs 的 `autoload` + `require` + 下面这组注册 API 就是全部机制。参考实现是 `plugins/superchat-cowork-telega/` bundle。

## 分层与你的位置

```text
Kernel（内核，headless 可载）
├── superchat-runtime   —— 执行生命周期（submit / cancel / status）
├── superchat-authority —— 工具执行唯一接缝（effects / 审批 / 审计）
├── superchat-tape      —— 事件历史（event-id / 因果链）
├── superchat-projection—— 可重放投影
└── superchat-core / preset / db / llm

插件（你写的）
└── 只 require 需要的模块，加载时注册，绝不被内核 require
```

分层由 `test/test-runtime.el` 中的架构测试强制：内核不得 require 插件；急加载发行层不得 require 集成包；阻塞提交只允许出现在声明过的同步适配器里。

## 声明式扩展（推荐入口）

多数扩展应该用一个声明说清自己"何时相关、贡献什么"，而不是散装注册。
核心是**激活作用域**：不活跃的扩展零成本——工具不进模型面、知识不进
prompt；只有 Request 命中激活谓词时，贡献才被组合进这一次请求。

```elisp
(superchat-define-extension 'telega-cowork
  :description "Attended coworker for Telega chats."
  ;; 激活谓词：按 Request 判定（surface / capability / 启发式）
  :active-when (lambda (request)
                 (eq (plist-get (plist-get request :origin) :surface)
                     'telega))
  ;; 激活时才进入模型工具面；被声明的工具同时退出全局面
  :tools '("telega_send_message" "memory_search")
  ;; 确定性上下文注入：插件代码判断相关性，不烧模型轮次
  :context #'my-extension--inject-agenda-if-scheduling
  ;; 激活时给模型的简报：何时做、怎么做（保持简短）
  :knowledge "Consult memory_search first; send exactly once; user approves."
  ;; 无条件贡献（注册即生效）
  :commands '(("my-cmd" . my-extension--cmd)))
```

组合规则（由 `test/test-extension.el` 契约测试守护）：

- 显式 `:tools`（含 `none`）是收窄决定，组合**永不放宽**它；
- 请求未给 `:tools` 时，活跃扩展的工具与核心动词面合并；
- 每次组合写一条 `extension.activated` 事件进 Tape（可审计）;
- `/extensions` 列出全部声明及其贡献。

能力分三层，声明的三个槽位与之对应：**确定要的信息放 `:context`**
（插件代码判断相关性）；**推理中才需要的动作放 `:tools`**（少而精）；
**流程与惯例放 `:knowledge`**（渐进披露）。一次性长尾交给
`eval-elisp`/`eshell-command`，不要为它们造工具。

## 底层接缝总表

`superchat-define-extension` 在内部组合这些原语；需要精细控制时可直接使用。

| 你想做什么                  | 用哪个接缝                                                                   | 定义于                  |
|-----------------------------|------------------------------------------------------------------------------|-------------------------|
| 声明激活作用域的扩展        | superchat-define-extension                                                   | superchat-extension.el  |
| 撤销扩展声明及其命令 / 策略 | superchat-extension-unregister                                               | superchat-extension.el  |
| 添加 /xxx 斜杠命令          | superchat-register-command                                                   | superchat.el            |
| 删除 /xxx 斜杠命令          | superchat-unregister-command                                                 | superchat.el            |
| 给模型添加可调用工具        | superchat-register-tool                                                      | superchat-tools.el      |
| 删除插件工具                | superchat-unregister-tool                                                    | superchat-tools.el      |
| 添加命名 Authority 策略     | superchat-authority-register-policy                                          | superchat-authority.el  |
| 删除命名 Authority 策略     | superchat-authority-unregister-policy                                        | superchat-authority.el  |
| 为既有工具声明 effects      | superchat-authority-register-effect-resolver                                 | superchat-authority.el  |
| 移除既有工具的 effects 声明  | superchat-authority-unregister-effect-resolver                               | superchat-authority.el  |
| 审批时展示自定义预览        | superchat-authority-register-approval-preview                                | superchat-authority.el  |
| 移除审批时的自定义预览      | superchat-authority-unregister-approval-preview                              | superchat-authority.el  |
| 从 Tape 构建可重放视图      | superchat-projection-register                                                | superchat-projection.el |
| 注入 system prompt / 上下文 | superchat-system-prompt-functions / superchat-build-prompt-functions hooks   | superchat-core.el       |
| 响应 Run/Turn 完成          | superchat-run-completed-functions / superchat-turn-completed-functions hooks | superchat-runtime.el    |
| 从新入口发起执行（Surface） | superchat-runtime-submit                                                     | superchat-runtime.el    |

插件工具必须使用独立的名称；`superchat-register-tool` 会拒绝与内置工具同名
的注册，避免注销插件时误删内置 Authority resolver 或审批预览。

## 五条铁律

违反前四条的代码无法通过仓库的架构回归测试；它们不是风格建议，是合同。

1. **一切执行经 Runtime。** 需要模型时构造 Request 交给 `superchat-runtime-submit`，它立即返回 Run 句柄，结果经 `:delivery-function` 异步到达。禁止直接调用 `llm-chat-*` 或 `superchat--llm-generate-answer`。
2. **禁止阻塞。** `superchat-runtime-submit-blocking` 只属于同步边界适配器白名单（Org Babel、Magit、Rewrite）。插件一律异步。
3. **工具必须声明 effects，且绝不自行确认。** Authority 不认识工具名，只认识 effects（`local-read` / `local-write` / `process-exec` / `runtime-exec` / `external-read` / `external-write`）。审批、审计、Tape 记录都由 `superchat-tool-execute` 统一负责——工具体内出现 `y-or-n-p` 即是错误。
4. **命名策略只能收紧。** 注册的 Authority 策略返回 `deny` / `ask` / `defer`；返回 `allow` 会被降级为 `defer`。
5. **命名空间与稳定性。** 插件符号用 `superchat-<插件名>-` 前缀。Superchat 中双横线（`superchat--`）符号是内部实现，不在兼容承诺内；本指南列出的 API 由 `test/test-plugin-api.el` 契约测试守护。

## 完整示例：superchat-fortune.el

一个最小但完整的插件：一条命令、一个带 effects 的工具、一个经 Runtime 的 Surface 入口。

```elisp
;;; superchat-fortune.el --- Fortune plugin for Superchat -*- lexical-binding: t; -*-
;; Package-Requires: ((emacs "28.1") (superchat "0.9"))

(require 'superchat-authority)
(require 'superchat-editor-workspace)
(require 'superchat-runtime)
(require 'superchat-tools)

;; 1. 工具：声明 effects，实现里没有任何确认逻辑。
(defun superchat-fortune--cookie ()
  "Run the fortune binary and return its output."
  (string-trim (shell-command-to-string "fortune")))

(superchat-register-tool
 "fortune_cookie"
 :function #'superchat-fortune--cookie
 :description "Return one random fortune-cookie aphorism."
 :args nil
 ;; process-exec：attended 会话中 Authority 会先征求用户同意。
 :effects '((:effect process-exec :resource process :scope "fortune")))

;; 2. 斜杠命令：handler 返回结果 plist。
;;;###autoload
(defun superchat-fortune--cmd (_cmd _args _input _lang _model)
  "Show a fortune in the chat buffer."
  (list :type :echo :content (superchat-fortune--cookie)))

(superchat-register-command "fortune" #'superchat-fortune--cmd)

;; 3. Surface 入口：从任意 buffer 发起一次经统一流水线的请求。
;;;###autoload
(defun superchat-fortune-explain ()
  "Ask the model to explain today's fortune, asynchronously."
  (interactive)
  (let* ((fortune (superchat-fortune--cookie))
         (session-id "fortune")
         (buffer (superchat-editor-workspace-buffer
                  session-id 'fortune-explanation t)))
    (superchat-pipeline-submit
     (list :surface 'external
           :content (format "Explain this aphorism briefly:\n\n%s" fortune)
           :session-id session-id
           :attendance 'attended
           :tools 'none
           :origin '(:surface fortune)
           :context (list :host-buffer buffer)
           :sink (superchat-sink-callback
                  :final (lambda (_run result)
                           (message "Fortune: %s" (plist-get result :text)))
                  :error (lambda (_run failure)
                           (message "Fortune failed (%s): %s"
                                    (plist-get failure :status)
                                    (plist-get failure :text))))))
    (message "Consulting the oracle asynchronously...")))

(provide 'superchat-fortune)
;;; superchat-fortune.el ends here
```

用户体验：安装即注册（加载插件就是 opt-in），`/fortune` 在聊天里可用，`fortune_cookie` 工具出现在模型工具面上并在执行前弹出 Authority 审批，`M-x superchat-fortune-explain` 从任何地方发起异步请求。

上例是一个 **owned host**：EditorWorkspace 创建并按 role 复用它。Runtime 的 success、cancel、provider error 和 callback error/quit 都不会关闭它；只有 `superchat-session-workspace-close` 或用户明确关闭才结束它。对于 **ordinary borrowed** buffer，producer 已知稳定 Session 时，也可以先用 `superchat-editor-workspace-validate-buffer` 验证同一对象再提交；所有 Runtime 终态同样不 kill。buffer 名、`current-buffer`、callback 或目录都不是 ownership 证据。

Extension 组合对每个 envelope 自动生效（S12）：`:active-when` 谓词收到 envelope，可读 `:surface` `:session-id` `:content` `:context`；上下文提供者的文本作为 `context` 片段、简报作为 `system` 片段进入提示词。

## Request envelope 字段速查

`superchat-pipeline-submit` 接受的 envelope plist（合同 §2.2；必填 `:content`、`:surface`、`:sink`）：

| 字段 | 含义 |
|---|---|
| :content | 进入上下文组装前的输入文本，非空；`intent.received` 记录的就是它 |
| :surface | 已注册的 surface 符号（`chat` `inline-ask` `inline-rewrite` `workflow` `subagent` `compaction` `skills` `external` …；插件用 `superchat-register-surface` 注册自己的） |
| :sink | `superchat-sink-callback :final :error :stream` 构造的 Sink，所有可见输出的唯一去处；`:final` 收到 `(:text :tape-id)`，`:error` 收到 `(:status :text :tape-id)`，`:stream` 收到 `(:kind text\|reasoning :text …)` |
| :session-id | 持久会话 ID；缺省时由 `resolve-session` 阶段解析，否则临时 ID |
| :attendance | `attended`（可审批，默认）或 `unattended`（副作用一律 deny） |
| :parent-run-id | 直接委托的父 Run；从不推断 |
| :context | 带 owner 前缀的事实 plist：`:host-buffer`（Runtime 宿主 buffer，你已获取的协议 buffer）、`:prepared-prompt` / `:prepared-system`（已经准备好的完整用户消息与系统提示，原样发送，跳过上下文收集）、`:chat-*` `:inline-*` `:rewrite-*` `:telega-*` … |
| :media | 显式媒体文件路径列表；绝不从 buffer 读取 |
| :tools | `none`（禁用）、工具名列表（只会被收窄，不会被扩展）或 nil（按 surface 基线） |
| :origin / :target-model / :capability / :topology / :authority-policy / :deadline / :system-prompt | 原样传给 Run |
| :runtime-extras | envelope 尚未建模的键，原样复制到 Runtime Request：`:approval-function`（请求级审批函数，优先于全局）、`:tool-validator`、`:preset`、`:actor-id`、`:record-conversation` 等；`:buffer :buffer-name :stream-function :delivery-function :prompt :input` 在这里一样会被拒绝 |

只有 Sink 能把输出送到用户面前；Extension 组合由流水线在 admission 之前自动完成（`superchat-extension-compose-envelope`），插件无需再调用 `superchat-extension-compose-request`。已有的 legacy Request 可以用 `superchat-pipeline-envelope-from-request` 一行翻译成 envelope。

## Legacy Request（deprecated，每个 seam 只警告一次）

`superchat-runtime-submit` 仍接受旧的 Runtime Request plist（必填 `:prompt` 与 exact live `:buffer`），但从 S14 起 `superchat-runtime-warn-legacy-requests` 默认为 `t`：使用 `:stream-function` / `:delivery-function` 桥接会各警告一次。新插件请直接提交 envelope。

| 字段                                        | 含义                                                                                |
|---------------------------------------------|-------------------------------------------------------------------------------------|
| :prompt / :input                            | 发给模型的 prompt / 原始用户意图（进 Tape）                                         |
| :session-id                                 | 持久会话 ID；缺省时 Runtime 生成临时 ID                                             |
| :actor-id / :preset                         | 参与者身份 / Agent Profile（type=agent 的 preset，需显式 profile-id 与 capability） |
| :capability / :topology / :authority-policy | 正交维度；capability 与 Profile 冲突会被拒绝                                        |
| :attendance                                 | attended（可审批）或 unattended（副作用一律 deny）                                  |
| :tools                                      | 工具名列表、none（禁用）、或 nil（按全局策略收集）                                  |
| :tool-validator                             | 语义预检函数，只能拒绝规范化后的工具参数                                            |
| :approval-function                          | 请求级审批函数（如"范围同意"：只自动放行已授权 scope 的读，其余回落默认审批）       |
| :delivery-function                          | (response status tape-id)，所有终态都会调用                                         |
| :stream-function                            | 流式增量回调；#'ignore 表示丢弃                                                     |
| :buffer                                      | exact live buffer object；Runtime 借用且所有终态不关闭                    |
| :buffer-name                                 | 可选诊断标签；Runtime 从 exact `:buffer` 派生实际名称，绝不按名称定位、创建或 retarget host |
| :ephemeral-buffer / :host-authority-mode     | 已删除的 legacy 输入；即使值为 nil 也会在 Run、Tape、provider effect 前被拒绝              |
| :origin                                     | 来源描述 plist（:surface :uri 等，进 Tape）                                         |
| :workspace-id / :workspace-root             | Workspace 绑定                                                                      |

返回 `(:turn-id .. :run-id .. :session-id .. :status accepted)`；用 `superchat-runtime-status` 查询、`superchat-runtime-cancel` 取消。

旧 host 路径已经从本地 Runtime 合同删除；包含该删除的发行版构成 **breaking boundary**。数字版本号尚未指定。这里不承诺 `2.0.0`、整个 `1.x` 兼容期、发布日期或发布动作。仓库内 caller 归零也不能证明 repository-external consumers 已迁移；external consumers 始终是 unknown，必须在 removal release 前迁移。

## 发布检查清单

- [ ] 包名 `superchat-<名字>`，符号带前缀，未引用任何 `superchat--` 内部符号
- [ ] 用 `superchat-define-extension` 声明，`:active-when` 尽量收窄
- [ ] `:tools` 少而精（两三个）；确定性信息走 `:context`，流程走 `:knowledge`，一次性长尾走 `eshell-command`/`eval-elisp`
- [ ] 入口命令带 `;;;###autoload`
- [ ] 每个工具都声明了 `:effects`，实现里没有确认/审计逻辑
- [ ] 没有 `superchat-runtime-submit-blocking`、没有 `llm-chat-*` 直调
- [ ] Surface 入口在 submit 前调 `superchat-extension-compose-request`
- [ ] 异常路径经 `:delivery-function` 的 status 分支处理
- [ ] 对照 `plugins/superchat-cowork-telega` 检查过整体形状（参考实现）
