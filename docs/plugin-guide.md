# Superchat 插件开发指南

这份教程使用当前接口，带你挂载、验证和卸载一个小插件。[架构地图](architecture.md)解释模块位置，[组合合同](design/plugin-architecture.md)解释 Scope、Service 与生命周期；未来主干改造只在[剩余计划](roadmap.md)中描述。

## 1. 写一个有两个贡献的插件

下面的插件提供 `/day-note` 本地命令，并为聊天请求添加一个 soft 提示词片段。不调用模型、不启动进程，也不需要第三方程序。保存为 `superchat-day-note.el`：

```elisp
;;; superchat-day-note.el --- A scoped note -*- lexical-binding: t; -*-

(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-event)

(superchat-define-plugin day-note
  :inject '(commands)
  :config '((:note stringp "Keep the answer concise." "Chat note."))
  :doc "One local command and one chat context contributor."
  :apply
  (lambda (scope config)
    (let ((note (plist-get config :note)))
      (superchat-service-call
       scope 'commands 'register "day-note"
       (lambda (_name _args _input _lang _model)
         (list :type :echo :content note)))
      (superchat-context-register
       'day-note:context
       (lambda (envelope _state _tools)
         (when (eq (plist-get envelope :surface) 'chat)
           (list (list :owner 'day-note :id "day-note:context"
                       :slot 'context :text note :priority 0
                       :budget-class 'soft))))
       :owner 'day-note :scope scope))))

(provide 'superchat-day-note)
;;; superchat-day-note.el ends here
```

插件定义只是数据；挂载才运行 `:apply`。这里的 `:inject` 和 `:config` 是表达式，所以字面量列表需要 quote。`commands` 缺席时插件处于 pending，而不是绕过 Service 找内部函数。两个贡献由同一次安装拥有，卸载会一起撤回。

## 2. 挂载并检查

在加载插件定义之前加载 Superchat；把下面的文件路径换成刚保存的路径：

```elisp
(require 'superchat)
(load "/path/to/superchat-day-note.el")
(superchat-plugin-mount 'day-note '(:note "Explain assumptions explicitly."))
(superchat-plugin-state 'day-note) ; => active
```

打开 `M-x superchat`，输入 `/day-note` 应显示配置文本，不发模型请求。也可以不打开窗口，直接检查同一个命令接口：

```elisp
(let ((handler (superchat-service-call nil 'commands 'lookup "day-note")))
  (funcall handler "day-note" "" "" "en" nil))
;; => (:type :echo :content "Explain assumptions explicitly.")

(apply #'append
       (superchat-context-collect '(:surface chat) nil nil))
;; 返回值中含 :id "day-note:context" 的片段；其他默认收集器也可能贡献片段。
```

`collect-context` 是一个组合规则为 `collect` 的 Event：每个监听器返回自己的片段列表，driver 再把这些列表合并。片段列表之外的返回值（或一个片段 plist 本身）会被判为非法贡献。上面的检查用的是同一个收集入口。片段的 `:id` 是字符串，Event 监听器 ID 是符号；`:owner` 是必填的所有者符号。soft 片段可因预算被缩减或省略；hard 片段必须完整放入，否则组装失败。`:required t` 的监听器出错会终止本次准备（无论它返回什么），可选监听器出错只记入 fault ring。该插件只对 `chat` 贡献，不会意外改变其他 Surface。

## 3. 重配置和卸载

重配置会先验证，再撤回并重新应用；同层重复注册不是隐式替换。

```elisp
(superchat-plugin-reconfigure 'day-note '(:note "Answer in two paragraphs."))
(superchat-plugin-unmount 'day-note)
(superchat-service-call nil 'commands 'lookup "day-note") ; => nil
```

卸载后再调用 `collect-context`，不应含 `"day-note:context"`。如果要在启动时默认挂载，把已加载定义的插件名或 `(NAME . CONFIG-PLIST)` 加入 `superchat-plugins`，然后调用 `superchat-plugin-mount-defaults`。

## 4. 用 Preset 选择组合

Preset 的原生 `:plugins` 支持名称和配置，例如 `(day-note)` 或 `((day-note :note "Check assumptions."))`。Markdown frontmatter 目前只接受名称的行内列表：

```yaml
plugins: [day-note]
```

每个 Chat View 的激活拥有独立 Scope；同名 Preset 不共享安装生命周期。具体的切换、候选失败、恢复与 View 关闭规则见[Preset 生命周期](design/plugin-architecture.md#preset-composition-and-lifetime)。不要在插件中按 Session 名称取消一批 Run，也不要把 Scope 清理当作关闭编辑 buffer 的授权。

## 5. 扩展到工具或模型请求

- 工具经 `tools` Service 注册：`(superchat-register-tool NAME :function F :description D :args ARGS [:async t] [:surfaces LIST] [:scope SCOPE])`。注册不声明任何 Effect——`:effects` 和 `:approval-preview` 已删除，传了会报错。每次执行都跨过唯一的执行管线 `superchat-tool-execute`，由 Session 的权限档位决定：插件工具属于"外部工具"，`edit-buffers`（默认档，旧名 `workspace-write`）下每次询问，`read-only` 下拒绝，`danger-full-access` 下直接执行。工具体不自行批准权限。
- 想让自己的工具更严，注册 `tools/pre-execute` 监听：`(lambda (call next) …)`，对要管的调用返回 `(plist-put (plist-put call :kind 'deny) :reason "…")`（或 `'ask`），其余 `(funcall next call)`；监听只能收紧，固定的档位阶段始终会再判一次。想自己回答批准提问（或先渲染预览），注册 `approval/request` 回答者：`(lambda (request next) …)`，返回 `allowed-once`／`rejected`／`cancelled`／`unavailable`，或 `(funcall next request)` 把问题交给后面的回答者直到人——从 `next` 拿回的就是人的最终决定。只对某一个 Request 生效的监听，挂在为该 Request 新建的子 Scope 上并在 Run 结束时销毁（见 `plugins/superchat-cowork-telega`）。包装一次调用用 `tools/execute`，改写或拦下结果用 `tools/post-execute`。
- 模型请求经 `driver.submit` 提交 Envelope，结果到精确 Sink；不要建立另一条直接 `llm-chat-*` 路径。只有 Org Babel 和 Magit 使用同步适配器。
- Service 的提供者是全局的。调用中的 nil Scope 继承执行 Scope；显式 global 才是回到全局。声明和方法以 `superchat-service.el` 为准。
- 生命周期监听用 `superchat-on`，参数和组合规则以 `superchat-event.el` 的 Event 声明为准。观察型监听器可在注册时声明 `:required t`；事件仍调用所有监听器，返回本次调用中的必需监听器失败，由调用者处理。普通观察者失败只记入故障记录。不要从过时的回调表猜测参数数量。
- 注册能撤销，不表示外部副作用能撤销；取消和迟到工具事实遵守[请求生命周期](protocols/request-lifecycle.md)。

更完整的产品例子是仓库里的 Telega 插件，`scripts/smoke.el` 的 U 用例验证其撤销。执行验证前阅读[开发指南](agents/development.md)；本教程已按 Stage 退役后的接口书写：`collect-context` 是 Event，上下文贡献一律走 `superchat-context-register`。
