# Superchat

Superchat puts a language model inside Emacs. It is three things:

- **The AI layer of Emacs.** Any command or package can ask a model to do
  something with one function, `superchat-run`.
- **A coding assistant.** The model reads and edits your buffers and files,
  under a permission level you set.
- **A personal agent.** Everything that happens is kept on one append-only
  record, the Tape, and the model can look things up in it later.

This is a clean rewrite. `SPEC.md` is the whole contract; this page is the
short way in.

## Requirements

- Emacs 30.1 or later, built with SQLite.
- `curl`.
- Nothing else. The model client is the `bridge/` directory of this
  repository.

## Set up

```elisp
(add-to-list 'load-path "/path/to/superchat-next/")
(add-to-list 'load-path "/path/to/superchat-next/bridge/lisp/")
(autoload 'superchat "superchat-chat" nil t)
(autoload 'superchat-note "superchat-chat" nil t)
(autoload 'superchat-recall "superchat-chat" nil t)
(autoload 'superchat-rewrite "superchat-inline" nil t)
(autoload 'superchat-ask "superchat-inline" nil t)

(setq superchat-llm-provider "deepseek"
      superchat-llm-model "deepseek-flash")
```

The API key is read from the environment, for DeepSeek from
`DEEPSEEK_API_KEY`. When it is missing, the error names the variable to set.

Then `M-x superchat`.

### Other providers

These names work for `superchat-llm-provider` out of the box: `deepseek`,
`ant-ling`, `baseten`, `cerebras`, `groq`, `huggingface`, `moonshotai`,
`moonshotai-cn`, `nvidia`, `qwen-token-plan`, `qwen-token-plan-cn`,
`qwen-token-plan-individual`, `together`, `xiaomi`, `xiaomi-token-plan-ams`,
`xiaomi-token-plan-cn`, `xiaomi-token-plan-sgp`, `zai`, `zai-coding-cn`.
Only `deepseek` has been used against the real service.

A model on your own machine (Ollama, LM Studio), or any other server that
speaks the OpenAI Chat Completions API, needs one entry and no key:

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

Anthropic, OpenAI and Gemini use other wire formats and are not supported
yet.

## Using it

### The chat buffer

`M-x superchat` opens the most recent conversation of the current project,
or a new one. Outside a project the conversation belongs to "home". With a
prefix argument it always starts a new one. A path the model uses is
relative to that root, or to your home directory outside a project, and the
prompt tells it which.

| key | does |
|---|---|
| `RET` or `C-c C-c` in the input area | send |
| `C-c C-k` | cancel what is running |
| `C-g` | interrupt a tool that keeps Emacs busy |

| command | does |
|---|---|
| `/new` | a new conversation in the same project |
| `/resume` | open another conversation of this project |
| `/fork` | branch off at the entry at point |
| `/handoff [name]` | summarise so far and go on from the summary |
| `/topic name` | show what was said under a handoff of that name |
| `/recall query` | search everything kept |
| `/notes` | show the notes |
| `/note text` | keep a note |
| `/timers` | what may still fire; `/timers cancel ID` drops one |
| `/dream` | review this project's notes each night; `/dream off` stops it |
| `/level [read-only\|ask\|auto]` | show or set the permission level |
| `/model [name]` | show or set the model of this buffer |
| `/help` | list the commands |

### In any buffer

- `M-x superchat-rewrite`: select a region, say what to change, see the
  replacement over it. `C-c C-c` accepts, `C-c C-k` discards.
- `M-x superchat-ask`: ask about the region; the question goes to the chat
  buffer beside the window.
- `M-x superchat-note`: keep a note.
- `M-x superchat-recall`: search what was kept; with an empty query, show
  the notes.

### What the model can do

| tool | class |
|---|---|
| `read` (a file, a buffer or a directory), `grep`, `find` | read |
| `recall`, `tape_query`, `note` | read |
| `edit`, `write` (each saves the file) | write |
| `eval_elisp`, `shell` | exec |

The permission level decides what happens when the model calls a tool:

| level | read | write | exec |
|---|---|---|---|
| `read-only` | allowed | refused | refused |
| `ask` (default) | allowed | asks you | asks you |
| `auto` | allowed | allowed | allowed |

When it asks, `y` allows once, `n` refuses, `a` allows that tool for the rest
of the conversation in this Emacs session.

`shell` and `grep` run in the background, so Emacs stays usable.
`eval_elisp` runs Lisp inside Emacs and can hold it; `C-g` stops it.

### Memory

There is one store, the Tape: an SQLite file that is only ever appended to.
A conversation is a view of it. So is memory: the model searches earlier
conversations with `recall`, queries the Tape with read-only SQL through
`tape_query`, and writes down conclusions with `note`. You do the same with
`/recall`, `/notes` and `/note`. The model's reasoning is never stored.

A long conversation is summarised before a new request when it has grown
past the budget; within one request, the oldest entries are left out when
they do not fit, and the model is told how many. The budget is half of the
model's context window unless you set `superchat-context-chars`. A request
that cannot be made to fit is not sent: the run ends with an error that
says how large it was.

### Rules and skills

- `AGENTS.md` at a project root is given to the model for conversations of
  that project; `AGENTS.md` in `superchat-directory` for "home".
- A skill is `skills/NAME/SKILL.md` in `superchat-directory`. The model sees
  each skill's name and `description:` line and reads the file when it needs
  it.

### While you are away

A conversation can be given work that nobody is typing into. It waits in an
inbox until the conversation is free, survives Emacs being closed, and is
answered by a run of its own; the items of one batch are answered together.

| what | does |
|---|---|
| `follow_up` (seconds), `schedule` (every, or daily `"HH:MM"`) | the model asks to be woken with a task later, or again and again; `cancel_timer` drops one |
| `send` | a run you are not watching shows you a line now |
| `M-x superchat-inbox-start` | starts the clock when Emacs starts |
| `superchat-inbox-notify-functions` | each `send` and each waiting question, for a notification hook of your own |

An unattended run asks nobody: a call that needs permission comes back as a
question, the run waits, and a message says so. Answer it with `y`, `n` or
`a` on its line in `M-x superchat-messages`.

`/dream` gives this project's notes a night review: at 03:00 the model
reads them and replaces, with `note`, those that repeat or are wrong. It
sends nothing. It is off until you ask for it.

## Options

| option | default | meaning |
|---|---|---|
| `superchat-directory` | `superchat/` in your Emacs directory | where the Tape, rules and skills live |
| `superchat-tape-file` | nil | the Tape file; nil means `tape.sqlite` in that directory |
| `superchat-llm-provider` | nil | provider name; required |
| `superchat-llm-model` | nil | model id of that provider; required |
| `superchat-llm-reasoning` | nil | nil for none, or `minimal`, `low`, `medium`, `high`, `xhigh`, `max` |
| `superchat-default-level` | `ask` | permission level of a new conversation |
| `superchat-context-chars` | nil | how much conversation one request may carry; nil follows the model's context window, or give a number of characters |
| `superchat-max-rounds` | nil | model responses in one run before it stops; nil, as in Pi, sets no limit |
| `superchat-tool-max-chars` | 20000 | longest tool result kept |
| `superchat-shell-seconds` | 60 | longest a `shell` command may run |
| `superchat-plugins` | nil | plugins to enable |

## For other packages

Ask one question and get the text back, without a chat buffer:

```elisp
(superchat-run (superchat-tape-new-name) "Summarise this: ..."
               :tools 'none
               :ephemeral t
               :on-done (lambda (run)
                          (pcase (plist-get run :status)
                            ('done (message "%s" (plist-get run :text)))
                            ('error (message "%s" (plist-get run :error))))))
```

`:ephemeral t` keeps the question out of the list of conversations. Leave
`:tools` out to give the model every tool.

A plugin adds tools, views and chat commands:

```elisp
(superchat-plugin-define 'weather
  :doc "Tell the model the weather."
  :enable (lambda ()
            (superchat-tool-define
             "weather" :class 'read :description "Weather of a city."
             :args '((:name "city" :type string :required t))
             :function (lambda (args) (my-weather (plist-get args :city)))))
  :disable (lambda () (superchat-tool-remove "weather")))
(setq superchat-plugins '(weather))
```

A tool that waits for a process is defined with `:async t`; see `SPEC.md`.
Plugins live in their own repository; this one ships none.

## How well it codes

Measured on 2026-11-18 with Aider's polyglot benchmark: 225 Exercism
exercises in C++, Go, Java, JavaScript, Python and Rust. Both harnesses
ran the same model, `deepseek-flash` on DeepSeek's API with reasoning
`high`, under the protocol of Aider's own benchmark: the instructions and
the stub files go in, the tests stay hidden, and a failure gets a second
attempt with the first 50 lines of the test output.

| Language | Superchat | Aider 0.86.2 |
| --- | --- | --- |
| C++ | 23/26 | 23/26 |
| Go | 35/39 | 32/39 |
| Java | 33/47 | 28/47 |
| JavaScript | 43/49 | 40/49 |
| Python | 31/34 | 28/34 |
| Rust | 28/30 | 29/30 |
| **All** | **193/225 (85.8%)** | **180/225 (80.0%)** |

On the first attempt the two are even (119 and 117 passed); Superchat gains
on the second, because it runs and inspects code with its own tools, which
Aider's benchmark mode does not.

Superchat's score is strict. With a shell, the model went looking for the
hidden tests: in 15 exercises it tried to fetch them from Exercism's GitHub
or found a copy of the benchmark on the disk. The shell ran without network,
but some attempts got through. Each of those 15 counts as failed, so 11
passes were taken away; without that rule Superchat scored 204/225 (90.7%).
Keep that in mind when Superchat works where answers it must not see are
within reach.

One run of each; with 225 exercises a single exercise moves the score by
half a point. The runner is not part of this repository.

## Development

```sh
scripts/check.sh          # Superchat: line caps, compile, tests
scripts/check-bridge.sh   # the bridge
```

Both run in a temporary copy and print `CHECK OK`. Read `SPEC.md` and
`AGENTS.md` before changing anything: product code has a line cap per file
and 6,000 lines in total.

`bridge/` is an Emacs Lisp port of part of `@earendil-works/pi-ai` 1.4.0:
the OpenAI Chat Completions API over `curl`. `bridge/docs/PORTING.md` says
which upstream files are ported.

## What it does not do

No sub-agents, no MCP, no workflows, no permission that survives Emacs
restarting. Anthropic, OpenAI and Gemini are not among the wire formats yet.
