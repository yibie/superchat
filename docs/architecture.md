# Architecture

This page explains how superchat works internally, so you can write your own hooks, commands, and skills. It assumes you've read `README.md#core-concepts`.

## The pipeline

Every user turn flows through `superchat-core-run-turn` (`superchat-core.el:118`):

```
┌─────────────────────────────────────────────┐
│  superchat-core--parse-input               │
│  Extracts @model, >skill, /command, #file  │
│  from turn.inbound → turn.{clean-input,    │
│  target-model, skill, command, files}       │
├─────────────────────────────────────────────┤
│  superchat-system-prompt-functions          │
│  Hook chain: (turn) → modified-turn        │
│  Builds turn.system-prompt                  │
├─────────────────────────────────────────────┤
│  superchat-build-prompt-functions           │
│  Hook chain: (turn) → modified-turn        │
│  Builds turn.prompt                         │
├─────────────────────────────────────────────┤
│  superchat-post-turn-functions              │
│  Hook chain: (turn) → nil                  │
│  Side effects: tape, memory capture, etc.  │
└─────────────────────────────────────────────┘
```

Each step is a `superchat-core--run-hook-chain` call, which runs every function in the hook variable in registration order, threading the turn through. If a hook function returns `nil`, the turn passes through unchanged. If it signals an error, the error is logged and execution continues.

The result of `superchat-core-run-turn` is a fully-populated turn struct. The dispatcher then decides what to do with it: send an LLM query, echo a message, update the buffer, etc.

## The turn struct

Defined in `superchat-core.el:22` as a `cl-defstruct`:

| Slot | Type | Writer | When set |
|------|------|--------|----------|
| `id` | string (read-only) | constructor | Created once per turn; format: `"<unix-ts>-<hex>"` |
| `session-id` | string (read-only) | constructor | Identifies the chat session |
| `inbound` | string | constructor | Raw text from the buffer between the prompt marker and `(point-max)` |
| `clean-input` | string | parse-input hook | `inbound` with @model, >skill, /command, #file tokens stripped |
| `target-model` | string or nil | parse-input hook | Extracted from `@model` prefix |
| `skill` | string or nil | parse-input hook | Extracted from `>skill` prefix |
| `command` | string or nil | parse-input hook | Extracted from `/command` prefix |
| `command-args` | string | parse-input hook | Arguments after the command name |
| `context-files` | list or nil | parse-input hook | List of file paths from `#file` refs |
| `system-prompt` | string | system-prompt hooks | Built incrementally by system-prompt hooks |
| `prompt` | string | build-prompt hooks | Built incrementally by build-prompt hooks; this is what gets sent to the LLM |
| `tools` | list or nil | dispatcher | Tool structs from `superchat-get-llm-tools` + `superchat-mcp-get-tools` |
| `retrieved-memories` | list or nil | dispatcher | Memories from `/recall` or auto-retrieval |
| `streaming-parts` | list | renderer | Tokens received during streaming |
| `llm-result` | string | renderer | Final accumulated response |
| `error` | string or nil | renderer | Error message if the LLM call failed |
| `state` | symbol or nil | various | Lifecycle marker (e.g., `:sent`, `:streaming`, `:done`, `:error`) |

A turn is created by `superchat-turn-new` in `superchat-core.el:42` and consumed by the dispatcher's `superchat--dispatch-result` in `superchat-dispatcher.el:322`.

## Hook function signatures

Hook functions have the signature:

```elisp
(turn) → modified-turn or nil
```

- **`turn`**: a `superchat-turn` struct. You can read any slot with the accessor `(superchat-turn-PROPERTY turn)`.
- **Return value**: a `superchat-turn` struct (modified copy), or `nil` (skip — turn passes through unchanged).
- Use `(setf (superchat-turn-PROPERTY turn) value)` to modify a slot. Always return the modified turn.

**Important performance note**: If you must decide whether to modify the turn or return nil, always return the turn (even unchanged) — this lets the core runner know you processed it. However, if you are certain your hook should do nothing (e.g., guard clauses that return nil immediately), that's also valid.

### Worked example: memory-context

Take `superchat-prompt-hook--memory-context` from `superchat-prompt-hooks.el:132`:

```elisp
(defun superchat-prompt-hook--memory-context (turn)
  "Prepend formatted memories from `turn.retrieved-memories' to `turn.prompt'."
  (when-let* ((mems (superchat-turn-retrieved-memories turn))
              (formatted (superchat--format-retrieved-memories mems))
              ((not (string-empty-p formatted))))
    (setf (superchat-turn-prompt turn)
          (concat formatted "\n\n" (superchat-turn-prompt turn))))
  turn)
```

Walkthrough:
1. `when-let*` short-circuits: if there are no memories, or formatting produces an empty string, the body is skipped.
2. If memories exist, `setf` prepends the formatted string to `turn.prompt`.
3. The function always returns `turn` — either modified (with memories) or unchanged (without).

### Example: injecting git-blame context

Here's a realistic hook you could add to your `init.el`:

```elisp
(defun my-git-blame-context-hook (turn)
  "Prepend git-blame context from the current buffer to the prompt."
  (when-let* ((buf (window-buffer (selected-window)))
              (file (buffer-file-name buf))
              ((file-exists-p file))
              ;; Only for files tracked by git
              ((eq 0 (call-process "git" nil nil nil
                                   "ls-files" "--error-unmatch" file)))
              (blame (with-temp-buffer
                       (call-process "git" nil t nil
                                     "blame" "-L" "1,30" "--" file)
                       (buffer-string))))
    (setf (superchat-turn-prompt turn)
          (concat "Git blame context (first 30 lines):\n```\n"
                  blame "```\n\n"
                  (superchat-turn-prompt turn))))
  turn)

(add-hook 'superchat-build-prompt-functions #'my-git-blame-context-hook)
```

This hook only activates when the current buffer is a git-tracked file. It runs `git blame -L 1,30` and prepends the output to the prompt. The model can then answer questions like "who wrote this function?" without you manually providing that context.

## Registering a hook

Hooks are Emacs `defvar` hook lists. Use standard `add-hook` / `remove-hook`:

| Hook variable | Phase | When to use |
|---|---|---|
| `superchat-system-prompt-functions` | Before prompt building | Inject language directives, role context |
| `superchat-build-prompt-functions` | Prompt assembly | Inject file content, memory, history, custom context |
| `superchat-post-turn-functions` | After LLM response | Logging, telemetry, notifications |
| `superchat-command-hooks` | Command dispatch | Add new slash commands |

Example: adding a hook that injects the current buffer's file path into every prompt:

```elisp
(defun my-current-buffer-hook (turn)
  "Tell the model which file the user is viewing."
  (when-let ((file (buffer-file-name (window-buffer (selected-window)))))
    (setf (superchat-turn-prompt turn)
          (concat (format "The user is currently viewing: %s\n\n" file)
                  (superchat-turn-prompt turn))))
  turn)

(add-hook 'superchat-build-prompt-functions #'my-current-buffer-hook)
```

Place this in your `init.el` after `(require 'superchat)`. It is picked up automatically — no restart needed.

## The five built-in build-prompt hooks

Registered in `superchat-prompt-hooks.el`, in order:

1. **`superchat-prompt-hook--language-instruction`** (`superchat-prompt-hooks.el:42`)
   System-prompt hook. When `superchat-lang` is not `"English"` and the template doesn't already reference `$lang`, sets a language directive like "Your response must be in 中文."

2. **`superchat-prompt-hook--file-inline`** (`superchat-prompt-hooks.el:62`)
   Build-prompt hook. Parses `#path` from `turn.clean-input`, reads the file, inlines its content into `turn.prompt`, and registers the file in the session context. Strips the `#path` reference from `turn.clean-input` so downstream hooks see a clean query.

3. **`superchat-prompt-hook--template-substitution`** (`superchat-prompt-hooks.el:112`)
   Build-prompt hook. Replaces `$input` and `$lang` in `superchat-general-answer-prompt` and appends the result to `turn.prompt`.

4. **`superchat-prompt-hook--memory-context`** (`superchat-prompt-hooks.el:132`)
   Build-prompt hook. If `turn.retrieved-memories` is non-nil, formats them and prepends to `turn.prompt`.

5. **`superchat-prompt-hook--conversation-history`** (`superchat-prompt-hooks.el:140`)
   Build-prompt hook. Prepends recent conversation messages to `turn.prompt` using `superchat-context-message-count`.

Registration order matters: hooks registered later see the modifications made by earlier hooks. For example, `conversation-history` runs last so it can prepend the session history *after* all other prompt builders have written their content.

## Post-turn hooks

`superchat-post-turn-functions` run after the LLM response is rendered. Unlike system-prompt and build-prompt hooks, these are side-effect-only — the turn is already consumed and the return value is ignored.

Registered post-turn hooks (both in `superchat-prompt-hooks.el`):

1. **Tape recording** — automatically records each exchange (user message + assistant response) into the tape subsystem (`superchat-db.el`).
2. **Memory capture** — when `/remember` is invoked with no arguments, captures the previous exchange into the SQLite memory store.

Post-turn hooks are the right place for:
- Logging or telemetry (`(write-region ... turn.llm-result ...)`)
- Desktop notifications (e.g., via `notifications-notify`)
- Triggering an external script or webhook
- Auto-save to a file

Example: a post-turn hook that notifies on desktop when the LLM finishes:

```elisp
(defun my-notify-on-done (turn)
  "Send a desktop notification when the LLM finishes responding."
  (require 'notifications)
  (let ((inhibit-message t))
    (notifications-notify
     :title "superchat"
     :body (format "Response from %s received"
                    (superchat-turn-target-model turn)))))

(add-hook 'superchat-post-turn-functions #'my-notify-on-done)
```

## A complete turn lifecycle

Here is the full lifecycle from user input to rendered response, showing which file is responsible at each stage:

```
User presses C-c C-c

1. superchat-dispatcher.el:220  superchat-send-input
   Reads raw text from the Org buffer between the prompt marker and point.

2. superchat-core.el:42         superchat-turn-new
   Creates a turn struct: inbound=raw text, id=<ts>-<hex>.

3. superchat-core.el:118        superchat-core-run-turn
   a. Parses @model → turn.target-model
      Parses >skill  → turn.skill
      Parses /command → turn.command, turn.command-args
      Parses #file   → turn.context-files
      Strips tokens  → turn.clean-input
   b. Runs superchat-system-prompt-functions on turn
   c. Runs superchat-build-prompt-functions on turn
   Returns turn with turn.prompt fully built.

4. superchat-dispatcher.el      Command dispatch
   If turn.command is set, resolves to a result type (echo/buffer/noop).
   If not, continues to LLM query.

5. superchat-llm.el             LLM call
   Collects tools from superchat-llm-tools-list + MCP tools.
   Calls llm-chat-streaming, rendering tokens into buffer as they arrive.
   Stores final string in turn.llm-result.

6. superchat-core.el            Post-turn hooks
   Runs superchat-post-turn-functions (tape, memory capture).

7. superchat-render.el          Buffer update
   Inserts the rendered response, repositions point, updates markers.
```

Each stage has exactly one owner file. If you want to change how parsing works, edit `superchat-core.el`. If you want to change what happens after a response, write a post-turn hook.

## The dispatcher

`superchat-send-input` (`superchat-dispatcher.el:220`) is the entry point triggered by `C-c C-c`. It:

1. Reads raw text from the buffer.
2. Creates a turn via `superchat-turn-new`.
3. Runs `superchat-core-run-turn` to populate the turn through the pipeline.
4. Checks and dispatches `/command` results.
5. If the result is `:llm-query`, attaches tools and makes the LLM call.
6. Passes the result to `superchat--dispatch-result` (`superchat-dispatcher.el:322`), which handles each result type:

| Result type | Action |
|---|---|
| `:llm-query` | Calls `llm-chat-streaming` or `llm-chat`, renders response |
| `:llm-query-and-mode-switch` | Same as above, but also sets command mode |
| `:buffer` | Inserts content into the chat buffer |
| `:echo` | Shows a temporary message |
| `:noop` | No action (e.g., after `/clear`) |

Command dispatch checks three sources in order:
1. `superchat--command-alist` — third-party commands registered via `add-to-list`.
2. `superchat-command-hooks` — hook chain (first non-nil return wins).
3. `superchat--builtin-commands` — builtins like `/backend`, `/mcp`.
4. `superchat--user-commands` — user-defined commands from prompt files.

## Adding a new command

**Via the command alist (simple):**

```elisp
(defun my-greet-cmd (_cmd args _input _lang _target-model)
  `(:type :echo :content ,(format "Hello, %s!" (or args "world"))))

(add-to-list 'superchat--command-alist '("greet" . my-greet-cmd))
```

The handler receives five arguments: command name, args string, raw input, language, and target model. It must return a plist with `:type` (one of `:echo`, `:buffer`, `:llm-query`, `:noop`) and appropriate content.

**Via the hook chain (for more complex dispatching):**

```elisp
(defun my-namespace-commands (_cmd args _input _lang _target-model)
  (cond
   ((string= _cmd "hello") `(:type :echo :content "Hi!"))
   (t nil)))  ; return nil to let other handlers try

(add-hook 'superchat-command-hooks #'my-namespace-commands)
```

**Via prompt files (user-friendly, no code):**

Create a file at `<data-dir>/command/my-command.prompt` containing the prompt template. The filename (before `.prompt`) becomes the command name. Use `$input` for the user's query and `$lang` for the language variable.

## Adding a new skill

See [docs/SKILLS_QUICKSTART.md](SKILLS_QUICKSTART.md) for the full guide. Quick summary:

1. Create a markdown file in `skills/` with YAML frontmatter.
2. Set `type: prompt` for system-prompt injection, or `type: workflow` for multi-step execution.
3. Invoke with `>skill-name`.

The skill resolver (`superchat-skills.el`) looks for skills in three locations, searched in order:

1. `<superchat-data-directory>/skills/` — user-installed skills (by `/skill-install`)
2. `skills/` in the repo root — built-in skills (code-review, planning, refactor)
3. `examples/standard-skills/` — reference examples

When you type `>code-review`, the resolver:

1. Reads `SKILL.md` from the first matching directory.
2. Calls `superchat-skills-standard--parse-frontmatter` to extract YAML frontmatter.
3. Validates required fields (`name`, `description`) and defaults others (`type: prompt`, `version: 1.0`).
4. For `type: prompt`: sets the body as `turn.system-prompt` so the LLM receives it as its system prompt for this one turn.
5. For `type: workflow`: passes each non-empty, non-comment line through `superchat-core-run-turn` sequentially. Each step gets its own `@model`, `>skill`, `/command`, and `#file` parsing. The user sees each step's response rendered in the buffer as if it were a separate turn.

If a `type: workflow` step fails (LLM error, timeout, lost connection), the remaining steps are skipped and the error is surfaced. There is no retry or branching.

See [docs/SKILLS_QUICKSTART.md](SKILLS_QUICKSTART.md) for the full guide on writing skills.

## What's NOT pluggable yet

- **Tool registration**: While you can `append` to `superchat-llm-tools-list`, there's no `add-hook` equivalent for tools. Tool registration is manual code manipulation.
- **Multi-turn agent loops**: Each turn is one user message → one LLM response. There's no built-in loop that feeds a tool result back for another LLM turn. This is Phase 2 / out of scope for v1.x.
- **Custom rendering**: The Org-mode buffer rendering in `superchat-render.el` is not hookable. You can't inject custom rendering logic without editing the source.
- **MCP server lifecycle hooks**: Starting and stopping MCP servers doesn't fire hooks you can hook into.
- **File reference parsing**: The `#file` parsing is hard-coded in the pipeline parse step, not exposed as a separate hook.
