# Development and verification

Use the source, [architecture map](../architecture.md) and [current contracts](../protocols/request-lifecycle.md) to locate a change. Keep the worktree's unrelated edits; this package is often intentionally dirty. In a colocated jj repository, use jj for version-control work and do not create commits, bookmarks or workspaces merely to perform a check.

## Canonical checks

The package-wide ERT suite and `experiments/` were removed on 2026-09-11 and are not rebuilt. Three checks remain: the Kernel check, the canonical smoke set, and the M3 regression set in `scripts/m3-regression.el`, which asserts the execution main line's contracts (admission, inbox, pre-step and request waterfalls, Step facts, turn-stopping).

The Kernel check needs only the repository on the load path:

```sh
emacs -Q --batch -L . -l scripts/kernel-check.el
```

`emacs -Q` does not activate installed packages. Initialize a prepared package directory containing llm's dependencies, then put the pinned llm source checkout on the load path. Replace the two paths below with directories that already exist:

```sh
ELPA=/path/to/prepared/elpa
LLM=/path/to/pinned/llm

emacs -Q --batch \
  --eval "(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
  -L "$LLM" -L . -L plugins/superchat-cowork-telega \
  -l scripts/smoke.el

emacs -Q --batch \
  --eval "(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
  -L "$LLM" -L . \
  --eval "(require 'superchat)"
```

The M3 regression set uses the same load path and the smoke fixtures. It exits non-zero on any unexpected result; a test marked `:expected-result :failed` documents a known defect and turns into a failure once that defect is fixed, so remove the marker then. Its two live-provider tests are skipped unless `SMOKE_REAL=1`. The set is owned by the leader agent the user designated: other agents run it and report proposed test changes with evidence, but do not edit it.

```sh
emacs -Q --batch \
  --eval "(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
  -L "$LLM" -L . -L plugins/superchat-cowork-telega \
  -l scripts/m3-regression.el -f ert-run-tests-batch-and-exit

# one contract area
  ... -l scripts/m3-regression.el --eval '(ert-run-tests-batch-and-exit "^m3/pre-step/")'
```

These batch checks disable native subr trampolines: the mock cases temporarily rebind primitives, which can otherwise trigger a native compiler failure on this Emacs build. The setting affects only the batch process, not the user's configuration.

The focused execution reliability set in `scripts/m5-execution-regression.el` uses the same dependency setup and runs with `--eval '(ert-run-tests-batch-and-exit "^m5-execution/")'`. It covers provider silence clocks, reasoning request serialization and tool-budget finalization without a real model. It is independent of the leader-owned M3/M5 sets and needs no scratch files; its header documents the full command.

The independent tool-feedback set in `scripts/m5-tool-feedback-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m5-tool-feedback/")'`. It checks exact edits, no-change results, complete expression parsing and recognized eval failures through the real tool loop. It needs no scratch files or model connection.

The request-context set in `scripts/m5-request-context-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m5-request-context/")'`. It checks the actual provider body and system text for chat, act and edited inspection, including context reuse and budget refusal. It is independent of the protected M3/M5 sets.

The history-material set in `scripts/m5-history-material-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m5-history-material/")'`. It checks full user and assistant material in the actual SQLite row and next Run's provider input, database reopen, whole-item budget omission and inspector deletion. It needs neither scratch files nor a real model.

The final-response set in `scripts/m5-response-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m5-response/")'`. It checks missing results through the real llm adapter and Runtime, preserving explicit empty strings, visible stream fallback, tool continuation, pending steering, budget metadata and cancellation. It needs neither scratch files nor a real model.

The permission set in `scripts/m6-permissions-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m6-permissions/")'`. Permission levels are simply how the product works now — there is no switch to turn on. It checks the approval seam against a live scripted Run (claiming and delegating answerers, `never`, the paired audit, the open-approval mark, quit, and the `?` details through the real `read-multiple-choice`, which the test loads from source so the input primitive underneath can be stood in for), the permission levels (events written only on change, `custom`, reopen across a closed database, `/permissions`), the buffer-editing tools over a real directory tree with a symlink and an indirect buffer (Superchat's own state is refused, `read-only` refuses edits, where a file lives no longer matters, `save_buffer` conflict rules), scripted tool calls through the real executor (what each level asks and denies, the approved arguments, a cancel while the question is open, a `tools/pre-execute` listener against the fixed level stage, a delegated child Session, `workspace_write`, the policy fragment in the provider request), the Telega plugin's per-request listeners, and that the deleted Authority machinery is really gone. `scripts/read-file-regression.el` (`"^read-file/"`) covers `read_file` the same way, `scripts/operating-note-regression.el` (`"^operating-note/"`) covers when the Emacs operating note is in a request's system text (it captures the prompt's context; the smoke fixture's `:prompt-text` holds the interactions only). `scripts/no-cumulative-limit-regression.el` (`"^no-cumulative/"`) covers the cumulative transport limit being off by default, on the M5 execution regression's virtual clock. `scripts/approval-input-regression.el` (`"^approval-input/"`) covers the rule that an approval is answered only by input that came after it; it starts an interactive child Emacs under a pseudo-terminal for each scenario (`scripts/approval-input-child.el`), because batch Emacs has no input queue, and takes about forty seconds. `scripts/edit-check-regression.el` (`"^edit-check/"`) covers the rule that a structured edit which would unbalance a Lisp buffer is sent back by the tool with the buffer untouched, `scripts/approval-wait-regression.el` (`"^approval-wait/"`) covers the rule that an open approval question stops the attempt's inactivity clock (it runs a real one-second watchdog, so it takes about fifteen seconds), and `scripts/m7-buffer-first-regression.el` (`"^m7/"`) covers buffer-first mutation: `open_file`, the unsaved-changes list (the edited buffer is recognized by its own mark through renames, reused names, reopened buffers, clones, separate Runs and delegated Runs), the retired `write_file` and escalation, `save_buffer` against the approval policy, and the `edit-buffers` level with its former name. Neither needs scratch files or a real model. `scripts/review-screen-regression.el` (`"^review/"`) covers the review screen for unsaved changes: it appears by itself without taking focus, one buffer at a time in `diff-mode` colors, `s`/`n`/`q`, `s` on a buffer that is gone, `g` redrawing from the current text, inert source-changing keys, file-less and new-file buffers, `/changes` and `superchat-changes-diff` opening it selected with typed-ahead keys dropped, and the option that turns it off. Set `DIFFS_EL_DIR` to a diffs.el checkout to include the diffs.el renderer case; without it that case is skipped. `scripts/editor-mode-regression.el` (`"^editor-mode/"`) covers editor mode: the chat View in a side window beside the file (opening it, a second conversation in the same window, `superchat-act` without taking focus, `superchat-send-region`, the plan panel keeping its own place, another window already showing the View left open), agent mode as before and in the conversation's own tab, a Run from the chat leaving the file's window alone while the review screen takes it, and closing a conversation shown only beside the file (by the View's own ownership marker, before the tab's, asking once while a Run works). Batch Emacs has working tabs and side windows in its one frame; the test that another window is left open runs the opening as an interactive Emacs would.

The system-contribution set in `scripts/m5-guidance-regression.el` uses the same setup with `--eval '(ert-run-tests-batch-and-exit "^m5-guidance/")'`. It checks that a tool rewrite refreshes dynamic system blocks in place across real attempts, that a base or static block with equal text is never edited, that an empty block keeps its position, and that a whole-system replacement by middleware ends automatic refresh. It needs neither scratch files nor a real model.

The prepared package directory must include the dependencies needed by the pinned llm, including compat, plz, plz-event-source and plz-media-type. A failed bare `locate-library` does not establish that a dependency is uninstalled. `-L` affects later loads; `-Q` still skips the user's init file.

Byte-compile touched modules, or all top-level modules for a cross-module change. Keep generated bytecode outside the source tree so a later load cannot accidentally test stale code:

```sh
SUPERCHAT_BYTECODE=$(mktemp -d)
export SUPERCHAT_BYTECODE
emacs -Q --batch \
  --eval "(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
  -L "$LLM" -L . \
  --eval '(setq byte-compile-dest-file-function
                (lambda (source)
                  (expand-file-name
                   (concat (file-name-base source) ".elc")
                   (getenv "SUPERCHAT_BYTECODE"))))' \
  -f batch-byte-compile superchat*.el ob-superchat.el
```

`scripts/smoke.el` owns the case list and its header explains mock versus live-model execution. Compare with `scripts/smoke-expected/`; investigate a mismatch before changing its baseline. Explain any intentional baseline change in the relevant change record; older explanations remain in the [historical migration log](../../.wayfinder/map.md). Source lookup, a mock-provider smoke, a real-provider request and interactive UI acceptance are different evidence: report which were actually exercised.

For documentation-only changes, check local links and runnable examples and confirm production sources and smoke scripts are unchanged. Compile or behavioral checks are appropriate when the document makes a new executable claim. No commit, publication or evidence-ledger ceremony is required to run these checks.

## Package and source conventions

`superchat.el` owns the authoritative Version/Package-Requires header; `superchat-pkg.el` is the installation descriptor and can lag during development. Do not duplicate package headers across ordinary modules.

Keep lexical binding enabled. Internal helpers use `superchat--name`, public functions use `superchat-name`; match existing formatting. Optional dependencies use `(require 'foo nil t)`, and files using external llm/mcp functions declare them. Optional product capabilities are queried through `superchat-service-get`, not guessed from a function's presence.

The transport is llm.el; the gptel migration is complete. Do not restore the removed gptel curl cleanup advice. `superchat-agent.el` is not an implemented module in this checkout; a proposed future file name in a design is not an existing load target.

The root `skills/` directory is product prompt material consumed by `>name`. It is unrelated to coding-agent skill tooling. Local discovery reads flat files in `superchat-skills-directory`; standard directory examples are not automatically discovered. See the [Skills guide](../SKILLS_QUICKSTART.md) for supported paths rather than assuming an import/export module exists.

## UI and editor conventions

For interactive widget.el controls read the `emacs-widget-controls` skill. Emacs 31 controls need explicit `:keymap widget-keymap`; avoid the non-existent `widget-button-pressed-hook` and follow the skill's notification deferral and keymap checks.

Plan review and progress use the `superchat-plan.el` TextUI side window, not inline widgets in chat. Frames wrap a column flex in a one-element list; a bare element list renders as one inline row there.

Source preview is not an edit. Inline rewrite accepts one atomic Change Set; editor buffers remain owned according to the [Session close protocol](../protocols/session-editor-workspace-close.md), never by name, project or whichever View is current.

## Where to write

Use [docs/README.md](../README.md) for document placement and [the local tracker](issue-tracker.md) for implementation issues. Keep historical records distinct from current contracts. README.md and README_cn.md are paired user guides; update both for shared behavior changes.
