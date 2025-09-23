# Repository Guidelines

## Project Structure & Module Organization
- Root Emacs Lisp sources `superchat.el` (chat UI, commands) and `superchat-memory.el` (memory store). Keep new features grouped by module; add new files alongside these with a lexical-binding header.
- `docs/` holds design notes such as `memory-design.org`; extend it with architecture briefs or protocol docs rather than inlining long explanations in code.
- Runtime data lives under `superchat-data-directory` (defaults to `~/.emacs.d/superchat/`). Do not commit generated files; share reproducible fixtures under `docs/` or a future `test/data/` directory.

## Build, Test, and Development Commands
- Byte-compile to surface warnings before review: `emacs -Q --batch -L . -f batch-byte-compile superchat.el superchat-memory.el`.
- Run Checkdoc on touched files to keep docstrings lint-free: `emacs -Q --batch -L . -l checkdoc -f checkdoc-file superchat.el`.
- For a manual smoke run, load the package and start a session: `emacs -Q -L . -l superchat.el -f superchat` (uses your existing gptel backend).

## Coding Style & Naming Conventions
- Stick to idiomatic Emacs Lisp with two-space indentation, lexical-binding, and docstrings in the imperative mood.
- Prefix public entry points with `superchat-` and memory utilities with `superchat-memory-`; internal helpers should use the `superchat--` double-dash pattern.
- Follow the existing comment separators (`;;;`, `;; --- section ---`) and keep `defcustom` descriptions concise and user-facing.

## Testing Guidelines
- Add automated coverage with ERT; place test files in `test/` (e.g., `test/superchat-memory-tests.el`).
- Execute suites via `emacs -Q --batch -L . -l ert -l test/superchat-memory-tests.el -f ert-run-tests-batch-and-exit`.
- Stub network calls so tests do not hit live LLM endpoints. Cover conversation state transitions and memory persistence edge cases.

## Commit & Pull Request Guidelines
- Follow the existing `type: summary` format (`fix:`, `feat:`, `remove:`). Keep subjects under 72 characters and write in the imperative.
- PRs should describe behaviour changes, reference any related issues, and note manual validation (e.g., "Byte-compiled cleanly", "Tested /clear command").
- Include screenshots or GIFs whenever UI tweaks alter the chat buffer, command palette, or memory prompts.

## Configuration & Security Notes
- Run `M-x superchat-ensure-directories` before first launch to create required folders; never commit local data or API credentials.
- Optional dependency `org-ql` accelerates memory search—keep `require` calls wrapped with `nil t` as in `superchat-memory.el` and provide fallbacks.
- Store API keys solely in your gptel configuration; this repository must remain key-free.
