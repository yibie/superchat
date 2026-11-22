# Superchat reference

Everything you can type, call and set. For what Superchat is and how to start, see the [README](../README.md).

## The chat buffer


| key | does |
|---|---|
| `RET` or `C-c C-c` in the input | send; while a run goes, the text reaches it before its next step |
| `C-u C-c C-c` | cancel what is running and send |
| `C-c C-k` | cancel what is running |
| `C-g` | interrupt a tool that keeps Emacs busy |

Type `/` at the start of the input for the commands, completed with their arguments:

| command | does |
|---|---|
| `/new`, `/resume`, `/fork` | a new conversation; open another; branch off at point |
| `/handoff [name]`, `/topic name` | summarise and go on from the summary; show what was said under it |
| `/recall query`, `/notes`, `/note text` | search what was kept; show the notes; keep one |
| `/timers` | the timers that may still fire (`c` cancels, `RET` opens) |
| `/dream [off]` | the nightly review of this project's notes |
| `/learn prefix`, `/skills review` | draft a skill from code that ran; review the skills |
| `/level [read-only\|ask\|auto]` | show or set the permission level |
| `/model [name]` | show or set the model of this conversation |
| `/help` | list the commands |

A conversation started from a file or Dired buffer works in that directory; one started from a buffer with no directory, such as `*scratch*`, works on Emacs itself and needs absolute paths.

## In any buffer

- `M-x superchat-rewrite`: change the region; `C-c C-c` accepts the replacement, `C-c C-k` discards it.
- `M-x superchat-ask`: ask about the region in the chat beside it.
- `M-x superchat-messages`, `M-x superchat-timers`: what was sent and what waits for you; the timers.

## Tools

| tool | class |
|---|---|
| `read`, `grep`, `find`, `describe`, `recall`, `note`, `load_tools` | read |
| `edit`, `write` (each saves the file) | write |
| `eval_elisp`, `shell` | exec |

`tape_query`, the timer tools, plugin tools and skill tools are listed by name and loaded with `load_tools` when needed; once loaded they stay for the conversation. Skills are found the same way, in `skills/` of `superchat-directory` and a project's `.superchat/skills/`. A project's `AGENTS.md` is given to the model in that project's conversations.

## Options

| option | default | meaning |
|---|---|---|
| `superchat-directory` | `superchat/` in your Emacs directory | where the Tape, rules and skills live |
| `superchat-llm-provider`, `superchat-llm-model` | nil | provider and model; required |
| `superchat-llm-reasoning` | nil | `minimal`, `low`, `medium`, `high`, `xhigh` or `max` |
| `superchat-default-level` | `ask` | permission level of a new conversation |
| `superchat-context-chars` | nil | how much one request may carry; nil follows the model's window |
| `superchat-max-rounds` | 80 | model responses in one run before it looks back; nil for no limit |
| `superchat-tool-max-chars` | 20000 | longest tool result kept |
| `superchat-shell-seconds` | 60 | longest a `shell` command may run |
| `superchat-plugins` | nil | plugins to enable |

## Providers

These names work for `superchat-llm-provider`: `deepseek`, `anthropic` (Claude; key in `ANTHROPIC_API_KEY`), `ant-ling`, `baseten`, `cerebras`, `groq`, `huggingface`, `moonshotai`, `moonshotai-cn`, `nvidia`, `qwen-token-plan`, `qwen-token-plan-cn`, `qwen-token-plan-individual`, `together`, `xiaomi`, `xiaomi-token-plan-ams`, `xiaomi-token-plan-cn`, `xiaomi-token-plan-sgp`, `zai`, `zai-coding-cn`. Only `deepseek` has been used against the real service; `anthropic` is tested against a stubbed server.

A local model (Ollama, LM Studio) or any server that speaks the OpenAI Chat Completions API needs one entry and no key:

```elisp
(with-eval-after-load 'superchat-llm
  (push (cons "ollama"
              (lambda ()
                (superchat-bridge-openai-compatible-provider
                 :id "ollama" :base-url "http://localhost:11434/v1"
                 :models '("qwen3.5:9b"))))
        superchat-llm-providers))
(setq superchat-llm-provider "ollama" superchat-llm-model "qwen3.5:9b")
```

## Installing by hand

```elisp
(add-to-list 'load-path "/path/to/superchat/")
(add-to-list 'load-path "/path/to/superchat/bridge/lisp/")
(autoload 'superchat "superchat-chat" nil t)
(autoload 'superchat-ask "superchat-inline" nil t)
(autoload 'superchat-rewrite "superchat-inline" nil t)
(autoload 'superchat-timer-start "superchat-timer" nil t)
```

A package manager generates these from the package itself.
