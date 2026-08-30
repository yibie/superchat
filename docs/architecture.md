# Architecture

This page explains how superchat works internally, so you can write your own hooks, commands, and skills. It assumes you've read `README.md#core-concepts`.

The 2026-09-05 architecture audit is closed against Kernel K26. Its sole
P1–P8 status table is in
[architecture review: problems and evidence](architecture-review-20260905/02-problems.md#1-结论与证据等级);
the other review chapters retain historical reproductions, design alternatives,
and evidence limits rather than separate progress ledgers.

## The pipeline

Core prepares a Turn through `superchat-core-prepare-turn`.
`superchat-core-run-turn` is the compatibility name for the same preparation
operation; neither function executes a model call or completion side effect:

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
└─────────────────────────────────────────────┘
```

Parsing is a direct Core operation. The two preparation hook lists run through
`superchat-core--run-hook-chain`, which gives each hook a copy of the current
Turn and threads a returned Turn into the next hook. A `nil` or non-Turn return
leaves the current value unchanged; an error is reported and preparation
continues.

The result is a prepared Turn. The dispatcher decides whether the input is a
local command or becomes a Runtime Request. Runtime alone owns model execution,
terminal state, durable completion, delivery, and completion notification.

## The turn struct

Defined by `superchat-turn` in `superchat-core.el` as a `cl-defstruct`. The same struct type is
used for preparation values and Runtime's private execution Turn, but their
writers and lifecycles are not identical:

| Slot | Type | Writer | When set |
|------|------|--------|----------|
| `id` | string (read-only) | constructor | Created once per turn; format: `"<unix-ts>-<hex>"` |
| `session-id` | string (read-only) | constructor | Identifies the chat session |
| `origin` | plist or nil | preparation caller / Runtime request mapping | Surface origin copied onto the preparation or execution Turn |
| `inbound` | string | constructor | Preparation input, or Runtime's request input fallback |
| `clean-input` | string | Core parser / Runtime request mapping | Parsed preparation input; execution Turn uses `:clean-input`, `:user-message`, or request input |
| `target-model` | string or nil | Core parser / Runtime request mapping | Parsed `@model` on preparation; copied from the admitted Request for execution |
| `skill` | string or nil | Core parser | Parsed `>skill` or reserved Workflow token on a preparation Turn |
| `preset` | `superchat-preset` or nil | dispatcher / Runtime request mapping | Applied request profile, copied onto the execution Turn |
| `command` | string or nil | Core parser | Parsed `/command` on a preparation Turn |
| `command-args` | string | Core parser | Arguments after the command name on a preparation Turn |
| `context-files` | list or nil | Core parser / Runtime request mapping | Parsed `#file` reference, or admitted Request context files |
| `system-prompt` | string | preparation hooks / Runtime request mapping | Built during preparation; copied from the admitted Request for execution |
| `prompt` | string | preparation hooks / Runtime request mapping | Built during preparation; copied from the admitted Request for execution |
| `tools` | list, `none`, or nil | dispatcher/preset / Runtime request mapping | Request-local tool profile copied onto the execution Turn |
| `retrieved-memories` | list or nil | dispatcher | Memories from `/recall` or auto-retrieval |
| `streaming-parts` | list | compatibility consumers | Retained Turn data; streaming lifecycle belongs to Runtime/transport |
| `llm-result` | string | Runtime | Final response copied onto the private execution Turn at terminal handling |
| `error` | string or nil | Runtime | Failure text copied onto the private execution Turn |
| `state` | plist or nil | prompt preparation / Runtime | Request context metadata such as the Context envelope; not the public Run lifecycle |

A preparation Turn is created by `superchat-turn-new`. Runtime creates its own
private execution Turn from the admitted Request in
`superchat-runtime--turn-from-request`; public Run state comes from Runtime,
not the Turn's `state` slot.

## Preparation hook signatures

Only functions in `superchat-system-prompt-functions` and
`superchat-build-prompt-functions` use this preparation protocol:

```elisp
(turn) → modified-turn or nil
```

- **`turn`**: a copy of the current `superchat-turn`. You can read any slot with the accessor `(superchat-turn-PROPERTY turn)`.
- **Return value**: a `superchat-turn` struct to thread onward, or `nil`/another value to leave the current Turn unchanged.
- Use `(setf (superchat-turn-PROPERTY turn) value)` to modify a slot. Always return the modified turn.

Return the modified Turn when you change it. Returning `nil` is also valid and
means that the current Turn continues unchanged; it is not a performance signal.

### Worked example: memory-context

Take `superchat-prompt-hook--memory-context` from `superchat-prompt-hooks.el`:

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

These extension points are Emacs `defvar` hook lists, but they do not share one
signature. Use standard `add-hook` / `remove-hook`:

| Hook variable | Phase | When to use |
|---|---|---|
| `superchat-system-prompt-functions` | Before prompt building | Inject language directives, role context |
| `superchat-build-prompt-functions` | Prompt assembly | Inject file content, memory, history, custom context |
| `superchat-post-turn-functions` | After LLM response | Logging, telemetry, notifications |
| `superchat-command-hooks` | Command dispatch | Add new slash commands |

Command hooks receive `(command args input lang target-model)` and dispatch
stops at the first non-`nil` result. Runtime Turn/post-turn hooks receive a Turn
and ignore return values; Runtime completion hooks receive a public status
plist. Runtime isolates both `error` and `quit` from each terminal notification.

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

## Built-in prompt hooks

The registration block in `superchat-prompt-hooks.el` is the source of truth.
It currently registers four system-prompt hooks and these five build-prompt
hooks, in order:

1. **`superchat-prompt-hook--file-inline`**
   Build-prompt hook. Parses `#path` from `turn.clean-input`, reads the file, inlines its content into `turn.prompt`, and registers the file in the session context. Strips the `#path` reference from `turn.clean-input` so downstream hooks see a clean query.

2. **`superchat-prompt-hook--template-substitution`**
   Build-prompt hook. Replaces `$input` and `$lang` in `superchat-general-answer-prompt` and appends the result to `turn.prompt`.

3. **`superchat-prompt-hook--memory-context`**
   Build-prompt hook. If `turn.retrieved-memories` is non-nil, formats them and prepends to `turn.prompt`.

4. **`superchat-prompt-hook--conversation-history`**
   Build-prompt hook. Prepends a bounded Tape-backed Context View and records
   its envelope in `turn.state`.

5. **`superchat-prompt-hook--experience-context`**
   Build-prompt hook. Prepends a satisfied, explicitly selected Experience View
   and records that View in `turn.state`.

The system-prompt hooks conditionally add language, tool-use, self-extension,
and generative-UI guidance. Registration order matters: later hooks see earlier
changes, so consult the registration block rather than fixed source line numbers.

## Post-turn hooks

`superchat-post-turn-functions` are compatibility Turn notifications run by
Runtime once during terminal handling, before the Turn completion seal. Their
return values are ignored. Tape persistence is part of Runtime/Tape lifecycle;
it is not implemented by a prompt-hook registration.

For public lifecycle observation, `superchat-run-completed-functions` receives
a public Run status plist on successful completion, while
`superchat-turn-completed-functions` observes every completed Turn. Runtime
isolates `error` and `quit` from these notification hooks so they cannot reopen
or invalidate a closed Run. This differs from the preparation chain, which
threads returned Turn values.

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
   Parses @model, >skill, /command, and #file, then runs the extensible
   system-prompt and build-prompt preparation chains.

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

`superchat-send-input` is the entry point triggered by `C-c C-c`. It:

1. Reads raw text from the buffer.
2. Creates a turn via `superchat-turn-new`.
3. Calls `superchat-core-prepare-turn` to populate the preparation Turn.
4. Checks and dispatches `/command` results.
5. If the result is `:llm-query`, prepares the Runtime request with tools.
6. Passes the result to `superchat--dispatch-result`, which handles each result type:

| Result type | Action |
|---|---|
| `:llm-query` | Submits through `superchat-runtime-submit`; Runtime owns LLM transport and response delivery |
| `:llm-query-and-mode-switch` | Same Runtime submission, but also sets command mode |
| `:buffer` | Inserts content into the chat buffer |
| `:echo` | Shows a temporary message |
| `:noop` | No action (e.g., after `/clear`) |

Command dispatch checks four sources in order:
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

See [docs/SKILLS_QUICKSTART.md](SKILLS_QUICKSTART.md) for the full guide. For a
direct Superchat skill:

1. Create a flat file such as `code-review.md` in `superchat-skills-directory`.
2. Add optional YAML frontmatter and prompt instructions in the body.
3. Invoke with `>skill-name`.

`superchat-skills-get-available` and `superchat-skills--find-file` discover
supported flat files directly in that configured directory.
`superchat-skills-load` reads the selected file and parses its frontmatter into
a preset; `superchat-skills-invoke` then returns a command-compatible request
result for Dispatcher to submit. It does not execute a Workflow merely because
a skill file contains a `type` value.

On first initialization, `superchat-skills--ensure-directory` creates the
configured directory and, when it is empty, copies bundled flat skill files
there. The package's `skills/` directory is therefore an installation source,
not a second live lookup tier. `examples/standard-skills/` is not in the direct
runtime search path.

Standard directory-based `SKILL.md` packages are a separate interchange seam in
`superchat-skills-standard.el`: use `superchat-skills-standard-import` to import
one into the direct Superchat skill directory, or the matching export command
to produce standard layout. Direct `>skill` invocation does not call a separate
standard-frontmatter parser.

## Running a Workflow recipe

Workflow recipes are independent `.workflow` files, not ordinary skill-file
execution. Type `>>name [args]` (or the retained `>workflow name [args]` form).
Core reserves the `workflow` token; Dispatcher resolves the recipe and calls
`superchat-workflow-execute-async`. The Workflow engine parses the recipe and
submits each executable step through Runtime, so every step keeps Runtime's Run,
Authority, terminal, and delivery contracts. Dispatcher does not run recipe
lines synchronously through `superchat-core-run-turn`.

See [docs/SKILLS_QUICKSTART.md](SKILLS_QUICKSTART.md) for the full guide on writing skills.

## What's NOT pluggable yet

The following surfaces remain limited:

- **Custom rendering**: The Org-mode buffer rendering in `superchat-render.el` is not hookable. You can't inject custom rendering logic without editing the source.
- **MCP server lifecycle hooks**: Starting and stopping MCP servers in `superchat-mcp.el` does not run lifecycle hooks you can subscribe to.
- **File reference parsing**: Basic `#file` extraction remains hard-coded in the parse step in `superchat-core.el`. Inline handling is also registered as a build-prompt hook in `superchat-prompt-hooks.el`, but there is no dedicated file-reference hook.

These former limitations are now covered:

- **Tool registration**: Plugins can call `superchat-register-tool` from `superchat-tools.el`; the public seam is also documented in `docs/plugin-guide.md`.
- **Multi-turn agent loops**: Runtime and the agent layer support tool rounds that feed results back into the model, as described in `superchat-runtime.el` and `superchat-agent-loop.el`.
