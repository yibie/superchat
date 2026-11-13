# Superchat

Superchat 是住在 Emacs 里的 AI 助手。你可以在正在编辑的文件旁边打开一个聊天窗口和它对话，也可以直接在任何 buffer 里叫它：它读你的 buffer 和文件，替你改 buffer，保存留给你自己。

它解决浏览器里 AI 聊天的三个麻烦。不用把代码复制来复制去，因为它直接在你打开的 buffer 上工作。它干活时你可以接着干自己的，因为回答出现在你的文字旁边或一个浮层（overlay）里。它也不会忘：每段对话、它替你记住的东西、你交给它的任务都存在数据库里；没人看着的时候它也能继续做事，比如去读一次失败的编译、一篇新的订阅文章，只在有话要说时告诉你。

<!-- screenshot: the chat side window beside a source file, with an answer that edited the file and the review screen below -->

## 和 gptel、ellama、chatgpt-shell 比较

| 功能 | gptel | ellama | chatgpt-shell | Superchat |
|---|:-:|:-:|:-:|:-:|
| 通过 `llm.el` 支持多家模型服务 | 是 | 是 | （自有） | 是 |
| 流式输出 | 是 | 是 | 是 | 是 |
| MCP 工具 | （第三方） | 否 | 否 | 是 |
| 单条消息的 `@model` 换模型 | 否 | 否 | 否 | **是** |
| `#file` 文件引用语法 | （选区） | （选区） | 否 | **是** |
| SKILL.md 技能格式（frontmatter + 正文） | 否 | 否 | 否 | **是** |
| 异步多步骤工作流 | 否 | 否 | 否 | **是** |
| SQLite + FTS5 持久记忆 | 否 | 否 | 否 | **是** |
| 插件模型（Service、Event） | 否 | 否 | 否 | **是** |

如果你只要单模型、一次性的对话，不需要记忆、工具和工作流，选 gptel 或 ellama 就够了。

## 安装

需要 Emacs 30.1 或更高版本，以及 GNU ELPA 上的 [llm.el](https://github.com/ahyatt/llm) 0.31.1 或更高版本。安装 llm.el（会一起装上 `plz`、`plz-event-source` 和 `plz-media-type`）：

```sh
emacs --batch --eval "(require 'package)" \
  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
  --eval "(package-initialize)" --eval "(package-refresh-contents)" \
  --eval "(package-install 'llm)"
```

Superchat 还没有上 MELPA；从源码目录加载，再选一个后端：

```elisp
(require 'package)
(package-initialize)
(add-to-list 'load-path "/path/to/superchat")
(require 'llm-openai)
(require 'superchat)

(setq superchat-llm-backend
      (make-llm-openai :key (getenv "OPENAI_API_KEY") :chat-model "gpt-4o-mini"))
```

`llm.el` 支持的后端都能用：Anthropic、Ollama、LM Studio 等的写法见[配置](docs/guide/configuration.md#backends)。可选：装 [mcp.el](https://github.com/lizqwerscott/mcp.el) 以接入 MCP 服务器。

## 用例子看怎么用

下面的文本截取都来自真实运行：在批处理 Emacs 里用脚本化的模型跑一遍，把 buffer 的文字导出来，所以显示的是英文。

### 聊天、换模型、带上文件

`M-x superchat` 在你的文件旁边打开聊天。在 `* User:` 后面输入，按 `C-c C-c` 发送。

```text
…
* User: explain what a zettelkasten is in one sentence

** Assistant:
A Zettelkasten is a note-taking method: one idea per note, each note linked to related notes.
…
```

<!-- screenshot: the chat side window with this exchange, beside a source file -->

几个前缀能改变一条消息的作用：

```
@gpt-4o 解释 SQLite FTS5 怎么工作              ← 只这一条换个模型
#~/src/main.py 这个函数做什么？                ← 文件内容随问题一起发出
>code-review #~/src/main.py                    ← 套用一个技能
/permissions                                   ← 执行命令（输入 / 可补全）
```

`C-c C-w` 把对话移到它自己的标签页，再按一次移回来。详见：[docs/guide/chat.md](docs/guide/chat.md)。

### 在你自己的 buffer 里干活

不用打开聊天窗口。`M-x superchat-inline-ask` 就光标处的代码回答问题，答案出现在这一行下面的浮层里，文件不会被改：

```text
def total(prices, tax):superchat inline · Ready · M-RET menu
It returns the sum of the prices with the tax added; tax is a rate such as 0.08.

    s = 0
…
```

<!-- screenshot: an inline-ask overlay under a line of Python code -->

`M-x superchat-rewrite-region` 把选中的文字改写成一个预览；`C-c C-a` 接受，`C-c C-k` 拒绝，在那之前文件不变：

```text
* Monday
REWRITE  Ready  C-c C-a accept  C-c C-k reject  C-c C-d diff  RET menu
The meeting has moved to Thursday at 3 pm. Please bring the draft budget.
```

<!-- screenshot: a rewrite preview over a region in an Org file -->

`M-x superchat-inline-act` 就地完成一件事，可以改 buffer；`M-x superchat-annotate` 把批注挂在它说的那一行上；`M-x superchat-note` 让你在文字上写自己的笔记，随下一条消息发出。详见：[docs/guide/in-your-buffers.md](docs/guide/in-your-buffers.md)。

### 技能

技能是一个带简短文件头的 markdown 文件。放进技能目录（`superchat-data-directory` 下的 `skills/`），用 `>explain-region` 调用；任务和某个技能的 `description` 对得上时，模型也会自己找到它。

```markdown
---
name: explain-region
description: Explain a code region to a senior engineer new to this codebase
type: prompt
---
You are a senior engineer explaining code to a peer. Say what it does, why it is built this way, and any edge case.
```

自带的技能：`code-review`、`planning`、`refactor`、`render-ui`。连着发的几个提示词写进一个 `.workflow` 文件，用 `>>名字` 运行。详见：[docs/guide/skills-and-workflows.md](docs/guide/skills-and-workflows.md)。

### 记忆

```
/remember LLM temperature controls randomness; 0 is deterministic, 1 is creative.
/recall temperature
```

`/remember` 记下一条事实；`/recall` 把它找出来，附到你的下一条消息上。你输入较长的消息时，相关记忆也会自动找出来。全部存在一个 SQLite 文件里，你自己也能打开看。详见：[docs/guide/memory.md](docs/guide/memory.md)。

### 权限：它改，你存

模型的编辑工具改的是 buffer，不是文件：改动留在 buffer 里，模式行上照常有 `*`，直到你保存。它留下未保存的 buffer 时，会出现一个审阅界面，逐个显示差异（`s` 保存，`n` 下一个，`q` 停）。凡是等于完全控制 Emacs 的操作（执行 Lisp、运行 shell 命令），每次都会问你，除非你用 `/permissions` 调高档位。

```text
Review 1/1 · main.py (/…/main.py)
Unsaved.  The diff below is what saving would write to disk.
s: save   n: leave unsaved, next   q: stop (the rest stay unsaved)   g: refresh   SPC/DEL: scroll   M-n/M-p: next/previous change

--- a/main.py
+++ b/main.py
@@ -1,5 +1,3 @@
 def total(prices, tax):
-    s = 0
-    for p in prices:
-        s = s + p
+    s = sum(prices)
     return s * (1 + tax)
```

<!-- screenshot: the review screen with a colored diff and the s / n / q line -->

详见：[docs/guide/permissions.md](docs/guide/permissions.md)。

### 自己运行的工作

说出你想让它盯着什么，Superchat 问你一次之后就设好：

```
每天早上 star 数变了就告诉我
这里编译失败时查一下原因，只有是我的代码出错才告诉我
elfeed 里有新文章时，告诉我和 Emacs 有关的那些
/work 把所有测试辅助函数移到 tests/helpers.el，并让测试全部通过
```

结果是一行字，出现在 `M-x superchat-butler`、你正在用的对话里，以及模式行上：

```text
11-11 23:32  Command "tell me when the star count changes" first run: 41
11-11 23:32  Command "tell me when the star count changes" changed: 42 (was 41)
```

<!-- screenshot: M-x superchat-butler with two command lines, and the same line with its [bring in] button in the chat -->

`M-x superchat-pending` 列出所有在跑、在等、还没看过的事；`RET` 跳过去，`k` 停止或取消，`t` 把结果带进你正在写的消息：

```text
 Ended, not looked at   Command "tell me when the star count change… changed: 42 (was 41)                                 butler
 Scheduled              tell me when the star count changes          ran 2 times, last: changed (2027-01-16 00:00), 1 ch… 20261111-233228-b10f
 Scheduled              renew the domain                             reminder at 2027-01-16 02:00                         20261111-233228-b10f
 Scheduled              when a compile fails here, tell me only whe… not looked yet; when a compilation fails in /privat… 20261111-233228-b10f
```

`M-x superchat-schedule-task` 列出你设好让它自己跑的东西。在 macOS 上，编译规则还能在项目的一份沙箱副本里试着修，把修法以差异的形式交给你。详见：[docs/guide/unattended-work.md](docs/guide/unattended-work.md)。

<!-- screenshot: M-x superchat-pending with a command row, a reminder and a compile rule -->

### Home 和工作区

项目里的文件打开该项目的对话，带上项目的规则（项目根目录的 `AGENTS.md`，没有就用 `CLAUDE.md`）。其它东西（一封邮件、一篇订阅文章、一个聊天、一个草稿 buffer）在 Home 里打开它自己的对话，一个来源一段：同一篇文章或同一个文件，重启后接着原来的对话，Home 里的各段对话互相看不到。Home 有自己的规则文件，在数据目录的 `home/AGENTS.md`。详见：[docs/guide/chat.md](docs/guide/chat.md#which-conversation-opens)。

### MCP

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem"
                                                "/Users/you/Documents")))))
```

用 `/mcp-start` 启动服务器；它们的工具以 `mcp:<服务器>:<工具>` 出现，在默认权限档位下每次调用都先问你，直到你把这个服务器加进 `superchat-mcp-trusted-servers`。详见：[docs/guide/integrations.md](docs/guide/integrations.md#mcp)。

### Telega

在 [telega.el](https://github.com/zevlg/telega.el) 的聊天里运行 `M-x superchat-cowork-telega`，会打开一段关于这个聊天的对话。它能读这个聊天，但从不发送；`C-c C-d` 把它最新的回答放进 Telega 的输入框，由你修改后发送。详见：[docs/guide/integrations.md](docs/guide/integrations.md#telega)。

<!-- screenshot: a Cowork Telegram conversation beside a Telega chat, with an example group -->

### 插件

Superchat 自己的各部分都是从 `superchat-plugins` 挂载的插件。你自己的包可以用 `superchat-define-plugin` 加一个 prompt 收集器、一个工具、一个命令或一个 Event 监听器，不用 fork，卸载即撤回。详见：[docs/plugin-guide.md](docs/plugin-guide.md) 和 [docs/guide/advanced.md](docs/guide/advanced.md)。

## 文档

- [用户指南](docs/guide/README.md)（英文）：每个功能的细节、配置、故障排除。
- [技能快速开始](docs/SKILLS_QUICKSTART.md)和[插件指南](docs/plugin-guide.md)：教程。
- [架构](docs/architecture.md)、[路线图](docs/roadmap.md)、[决策记录](docs/decisions/README.md)和[文档地图](docs/README.md)：它怎么构成、为什么这样。

## 许可证

GPL-3.0-or-later。

## 背景

Superchat 最初是 [org-supertag](https://github.com/yibie/org-supertag) 的聊天视图模块，自 v0.1 起独立，不依赖 org-supertag。当前包版本是 1.4.0；[路线图](docs/roadmap.md)区分了已实现和计划中的内容。
