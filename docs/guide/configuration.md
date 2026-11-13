# Configuration

Options you are most likely to set. `M-x customize-group RET superchat RET` lists all of them.

## Backends

Superchat talks to any backend [llm.el](https://github.com/ahyatt/llm) supports. Set `superchat-llm-backend` to a provider struct before sending the first message. If it is nil, `M-x superchat` still opens the chat, but the first message is refused and reports the configuration error.

```elisp
;; OpenAI
(require 'llm-openai)
(setq superchat-llm-backend
      (make-llm-openai :key (getenv "OPENAI_API_KEY") :chat-model "gpt-4o-mini"))

;; Anthropic
(require 'llm-claude)
(setq superchat-llm-backend
      (make-llm-claude :key (getenv "ANTHROPIC_API_KEY") :chat-model "claude-opus-5-5"))

;; Ollama's native backend
(require 'llm-ollama)
(setq superchat-llm-backend (make-llm-ollama :chat-model "llama3.1"))

;; LM Studio, or Ollama with its OpenAI-compatible API enabled
(require 'llm-openai)
(setq superchat-llm-backend
      (make-llm-openai-compatible :key "lm-studio"
                                  :chat-model "qwen/qwen3.8-27b"
                                  :url "http://localhost:1234/v1/"))
```

For Ollama through its OpenAI-compatible endpoint, use the same `make-llm-openai-compatible` form with the URL and model that endpoint exposes (often `http://localhost:11434/v1/`). `/backend` shows the active backend, provider and model.

`superchat-llm-set-model-capabilities` declares a local model's streaming abilities, for example for LM Studio: `(superchat-llm-set-model-capabilities "qwen2.5-coder" '(:streaming t :streaming-tool-use nil))`. Unknown models fall back to safe defaults.

`superchat-llm-warning-message` set to nil stops llm's "… API is not free software" warning (and its `*Warnings*` window) for Superchat's requests; the default `t` leaves it to llm (`llm-warn-on-nonfree`).

## Cheatsheet

```elisp
;; Minimum working config
(setq superchat-llm-backend
      (make-llm-openai :key (getenv "OPENAI_API_KEY") :chat-model "gpt-4o-mini"))

;; Data directory (default: "superchat/" under user-emacs-directory)
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; Language for $lang in commands and workflows (default "English")
(setq superchat-lang "English")

;; History size
(setq superchat-context-message-count 10)        ; default nil
(setq superchat-conversation-history-limit 50)   ; default 50
```

## Options

- `superchat-llm-backend`: the llm.el provider struct. Must be set before the first message.
- `superchat-llm-model`: optional `:chat-model` override, used by the `@model` switch.
- `superchat-manual-models`: extra model ids for `@model` and `/models`.
- `superchat-llm-streaming`: `t` (default) streams, `nil` blocks.
- `superchat-llm-tool-names`: the built-in tools offered to the model. The default is `superchat-llm-default-tool-names`: `read_buffer`, `read_file`, `edit_buffer`, `eval_elisp`, `work_record`, `timer_create`. `'all` offers every built-in tool except `sql`; nil offers none of the tools this list governs. `work_record` is offered while its row `work-records` is mounted even when this list leaves it out. Tools a plugin registers are offered whatever this list says. Other built-in tools (`open_file`, `list_buffers`, `read_reference`, `timer_list`, `timer_cancel` and more) are offered where a Preset or this list names them; `read_reference` fetches one of Superchat's bundled how-to pages by id, and a short index of them is in the system text whenever it is offered. What a call may do is decided by the conversation's [permission level](permissions.md#permission-levels).
- `superchat-sandbox-default-mode` / `superchat-approval-default-policy`: the level new conversations start at (default `edit-buffers` and `ask`). `superchat-mcp-trusted-servers`: MCP servers whose tools run without asking.
- `superchat-rewrite-confirmation`: `overlay` (preview, accept with `C-c C-a`; the default) or `auto`.
- `superchat-memory-auto-recall-cross-session`: see [memory](memory.md#remember-and-recall).
- `superchat-personal-workspace-directory`, `superchat-source-key-functions`: see [the chat](chat.md#which-conversation-opens).
- `superchat-handoff-auto-enabled`: automatic compaction on context overflow (default `t`).
- `superchat-data-directory`: where the database, configuration, skills, workflows and commands live (default `superchat/` under `user-emacs-directory`).
- `superchat-lang`: the language for `$lang` (default `"English"`).
- `superchat-response-timeout`: the silence window for each provider attempt, in seconds (default 120, multiplied by `superchat-tool-timeout-multiplier`, 1.5, in tool mode).
- `superchat-response-timeout-hard-limit`: an optional limit across all attempts, in seconds. Default nil: no such limit.
- `superchat-llm-max-tool-rounds` / `superchat-llm-round-limit-action`: the tool round budget and what happens at it (default nil, no limit; `ask`, used only when a limit is set).
- `superchat-llm-reasoning`: requested reasoning effort, or nil for provider defaults; see [reasoning](#reasoning).
- `superchat-emacs-operating-note`: a short note about working in Emacs (keys, buffers versus files, what to report) sent in the system text of requests that offer `eval_elisp` or `edit_buffer`. Default `t`; nil turns it off; a string sends your own text. About 250 tokens per such request. Its effect on real models has not been verified.
- `superchat-context-message-count`: how many recent messages go into the prompt (default nil).
- `superchat-conversation-history-limit`: how many messages a chat buffer keeps in memory (default 50).
- `superchat-display-style`, `superchat-display-single-window`, `superchat-side-window-width`: see [the chat](chat.md#opening-and-closing).

## What edit_buffer and eval_elisp report

The built-in edit and eval tools report operations separately from task completion. `edit_buffer` changes one unique match in a live buffer; it does not save or evaluate the file. It can also take `old_string` as a regexp (`regexp`), only count the matches and show the first three (`count_only`), or replace every match (`all`), the last only after a count of the same pattern in the same execution. An edit that would leave a Lisp buffer with unbalanced parentheses or an unterminated string is not applied: the buffer stays as it was and the model is told where the mismatch is, so it can submit the edit again.

`eval_elisp` takes one expression. Several top-level forms run in order, as if wrapped in `progn`, when every one of them is a list; if one is not a list (a symbol, a number, a string), none runs and the call fails, naming the first form that is not a list ([decision](../decisions/accepted/2026-11-09-several-forms-in-one-eval-run-in-order.md)). It reports the return value or a bounded tool error; a call that errors is recorded as failed (`tool.failed`), and the model is given the same text. The record line under the answer counts every time Emacs Lisp ran and says how many calls failed.

Code run through `eval_elisp` is kept from asking you: if it would open a prompt (a question, a minibuffer, `read-char` or `read-event`), it stops there and the model is told to look the answer up itself, or to ask you through `ask_user` when only you know. This is narrower than "never": `read-key-sequence`, `read-key` and `describe-key` can still wait for a key, and so can code the expression leaves to a timer. Configuration work still needs a check of the effective setting. Making it last is a division of labour: the model edits the buffer of the configuration file Emacs actually loads, and you save it. See [edit and evaluation results](../components/superchat-tools.md#built-in-edit-and-evaluation-results).

## Reasoning

`superchat-llm-reasoning` defaults to nil: no thinking field is sent and the provider or server chooses its mode. To turn thinking off on a native provider such as Ollama, set it to `none` explicitly. An explicit Preset effort overrides the global value; `reasoning: inherit` in the Preset follows the global setting. Native providers use llm.el's model support.

For the `llm-openai-compatible` provider, an explicit effort needs a mapping for the complete chat endpoint and model, or a deliberately configured raw reasoning parameter. For an endpoint you have checked accepts `reasoning_effort="none"`, this declaration lets the global or Preset setting select it:

```elisp
(setq superchat-llm-reasoning-parameter-overrides
      '(((llm-openai-compatible
          "http://localhost:1234/v1/chat/completions"
          "qwen/qwen3.8-27b")
         reasoning_effort
         (none . "none"))))
(setq superchat-llm-reasoning 'none)
```

Declare other efforts only with values that endpoint supports. Model names and port numbers do not establish its protocol. A missing mapping fails before sending, with instructions for selecting server defaults or declaring the mapping. Explicit raw reasoning parameters already in the prompt or provider take precedence, so remove a forced raw value when Presets should choose the effort. Saved custom values are preserved; an explicit `none` is not converted to nil. Request serialization establishes what was requested, not whether a server honoured it.
