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
| `tools` | list, `none`, or nil | dispatcher | Request-local tool-name profile, resolved to `llm-tool` structs by `superchat-get-llm-tools`; a connected MCP server has already registered its tools there with `superchat-register-tool`, so they need no separate collection |
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
- Triggering an external script or webhook
- Auto-save to a file

## A complete turn lifecycle

Here is the full lifecycle from user input to rendered response, showing which file is responsible at each stage:

```
User presses C-c C-c

1. superchat-dispatcher.el
   Collects Surface input and asks Core to prepare the request.

2. superchat-core.el
   Parses @model, >skill, /command, and #file, then runs the pure
   system-prompt and build-prompt hook chains.

3. superchat-dispatcher.el
   Converts the prepared value into an immutable Runtime request and calls
   superchat-runtime-submit.  Local UI commands do not complete a Runtime
   Turn; executable workflow steps submit their own Runs.

4. superchat-runtime.el
   Creates the private Turn and root Run, appends intent.received and
   run.started, and invokes the model transport.

5. superchat-llm.el / superchat-authority.el
   The model port streams provider output.  Every tool request passes through
   the shared Tool Executor and Authority chain.  A Surface may provide a
   request-local validator that can only reject normalized tool arguments; the
   attended Telega command uses this to bind its Runtime-produced draft to the
   chat that created the Request before handing the send to the shared Tool
   Executor.

6. superchat-runtime.el
   On a terminal provider result, Runtime appends response.completed, runs
   the compatibility post-turn hooks once, appends the terminal Run and Turn
   events, then delivers the closed result to its consumer.  Failed and
   cancelled Runs also complete their owning Turn.  Delivery failure cannot
   reopen an already sealed Run.  If Tape persistence itself fails, Runtime
   closes locally as audit-incomplete and queues a narrowly validated
   `run.failed` + `turn.completed` recovery pair in Tape's protected sidecar.
   Replay copies that pair into SQLite by stable event ID before returning the
   trace.  This also applies when an external tool effect succeeded but its
   `tool.completed` event could not be saved: Runtime cancels the provider
   instead of allowing an ordinary successful response, and recovery does not
   invent the missing Effect outcome.  Closure proves the audit-incomplete
   Run/Turn seal while leaving the unaudited Effect open.

7. superchat-render.el
   Acts as the default delivery adapter for the current Emacs Surface.
```

Core owns preparation; Runtime alone owns execution and completion. If you
want to change how parsing works, edit `superchat-core.el`. If you want to
react after a response, register a post-turn or Runtime completion hook;
callers must not finalize a Turn themselves.

### Asynchronous by default; blocking only at declared sync adapters

`superchat-runtime-submit` returns a Run handle immediately; results arrive
through delivery/stream callbacks, Tape events, and completion hooks.  That is
the only submission mode available to the Agent family (subagent, cowork,
workflow, team), to Surfaces, and to background maintenance such as session
compaction — none of them may hold the Emacs main loop hostage on a Run.

`superchat-runtime-submit-blocking` exists solely for synchronous boundary
adapters whose host contract requires a string in place: Org Babel results
(`ob-superchat.el`), commit-message insertion (`superchat-magit.el`), and
region replacement (`superchat-rewrite.el`).  The allowlist is enforced by
`runtime/blocking-submit-is-limited-to-sync-adapters`; adding a new blocking
caller means deliberately extending that test, not working around it.  The
same file also pins busy-wait pumps (`accept-process-output` loops) to the
blocking adapter seam and the documented `superchat-subagent-map` primitive.

### Workflow Resume Contract v0

Workflow owns the linear step graph; Closure owns whether the preceding Run is
durably consumable.  Before a completed step can advance, Workflow replays its
Session Tape and asks for both `trace-well-formed` and
`durable-run-terminal`.  The consumer mapping is fixed:

| Closure result | Workflow action |
|---|---|
| proved completed terminal | advance |
| open | wait; do not advance |
| unknown | require manual review |
| invalid trace or a non-success terminal | stop |

The pure `workflow-instance` Projection derives the latest Run for each
`workflow-step-N` actor within one Runtime `correlation-id`:

```text
step → run → Closure judgment → resume decision
```

`superchat-workflow-instance-view` returns that replayable view.  Closure
judgments and resume decisions are derived on read; they are not copied into
Tape or stored as a second Workflow truth.

The gate is deliberately narrower than a persistent Workflow engine.  It
protects live step handoff and exposes a restart-stable decision from Tape;
automatic process-restart continuation waits until Workflow inputs and prior
step results have a real durable representation.  The bundled Research
Workflow is the first end-to-end contract test: all three steps advance only
after their Runtime Runs have durable Closure proofs.

### Subagent Handoff Contract v0

The Parent may consume a successful Child report only through the Subagent
finish seam.  That seam replays the Child Session Tape and maps Closure to a
handoff decision:

| Closure result | Handoff decision |
|---|---|
| proved completed terminal | deliver the original report |
| open | wait; withhold the report |
| unknown | require manual review; withhold the report |
| invalid trace or a non-success terminal | reject the report |

The current async runner converts every non-`deliver` decision into an
explicit error result so callers and parallel aggregators finish without
consuming the unproved report.  Team task completion happens after this gate.
Cancellation and admission failures remain failure notifications, not
successful handoffs.

This contract proves only that the Child Run is durably closed.  It does not
certify the report's truth or Artifact quality, and it adds no Subagent event
store or lifecycle state outside Runtime Tape.

### Artifact Delivery Contract v0

Artifact delivery composes the existing Child Run proof with the existing
Workspace Artifact Projection; it does not add an Artifact state machine:

```text
Child Run Closure → current Artifact reference → delivery decision → consumer
```

A cross-Run `workspace_read` must name the revision reported by
`workspace_info`. Delivery succeeds only when the producer Run is durably
completed, the projected Artifact still has that revision and producer
identity, and its latest lifecycle event precedes the Run terminal seal.

| Evidence | Delivery decision |
|---|---|
| completed Run + current attributed revision inside the Run proof | deliver |
| producer Run still open | wait |
| unknown trace or legacy Artifact without provenance | manual review |
| invalid/non-success Run, missing/stale Artifact, producer mismatch, or post-terminal mutation | reject |

`deliver` admits the versioned Artifact reference, not the truth of its
contents. Its Adapter still owns URI resolution, and Authority still controls
effects. Existing `workspace.artifact.*` events and the Workspace Projection
remain the only facts; v0 adds no event type, table, or parallel status store.

## The dispatcher

`superchat-send-input` (`superchat-dispatcher.el:220`) is the entry point triggered by `C-c C-c`. It:

1. Reads raw text from the buffer.
2. Creates a turn via `superchat-turn-new`.
3. Runs `superchat-core-run-turn` to populate the turn through the pipeline.
4. Checks and dispatches `/command` results.
5. If the result is `:llm-query`, prepares the Runtime request with tools.
6. Passes the result to `superchat--dispatch-result` (`superchat-dispatcher.el:322`), which handles each result type:

| Result type | Action |
|---|---|
| `:llm-query` | Submits through `superchat-runtime-submit`; Runtime owns LLM transport and response delivery |
| `:llm-query-and-mode-switch` | Same Runtime submission, but also sets command mode |
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

The following surfaces remain limited:

- **Custom rendering**: The Org-mode buffer rendering in `superchat-render.el` is not hookable. You can't inject custom rendering logic without editing the source.
- **MCP server lifecycle hooks**: Starting and stopping MCP servers in `superchat-mcp.el` does not run lifecycle hooks you can subscribe to.
- **File reference parsing**: Basic `#file` extraction remains hard-coded in the parse step in `superchat-core.el`. Inline handling is also registered as a build-prompt hook in `superchat-prompt-hooks.el`, but there is no dedicated file-reference hook.

These former limitations are now covered:

- **Tool registration**: Plugins can call `superchat-register-tool` from `superchat-tools.el`; the public seam is also documented in `docs/plugin-guide.md`.
- **Multi-turn agent loops**: Runtime and the agent layer support tool rounds that feed results back into the model, as described in `superchat-runtime.el` and `superchat-agent-loop.el`.
