# Decision: eval_elisp never asks the user

Status: accepted — the user, 2026-09-24: "A", choosing to stop prompts inside the tool and add one sentence of guidance, after saying "这个交互体验其实如果用于大模型寻求人类建议的时候，我觉得挺好，但如果大模型可以自己找的话，可能会比让人类来回应要舒服一些——这个「验证」的体验不太舒服。"

## Problem

In the attended chat acceptance run `m8-attended-chat-deepseek-02` (commit `3b5af2d6`), the model "verified" a key binding by evaluating `(command-execute 'describe-function)` in a temporary buffer. `describe-function` asks for a function name in the minibuffer, so the user most likely had to answer a "Describe function:" prompt in the middle of the task (36 s that nothing else in the record accounts for; `docs/reports/2026-09-24-chat-entry-acceptance-latest-commit.md`, `.scratch/issues/2026-09-24-a-verification-ran-an-interactive-command.md`). The call had been approved, so nothing unsafe happened; the experience is what is wrong. The user's principle: a prompt that waits for the human is right when the model seeks the human's advice or decision; when the model can find something out itself, it should not make the human respond.

What the model was sent did not ask for running the command (the `emacs-keybindings` page says to check that the command exists and to read the binding with `key-binding`), and nothing warned against running a command that reads input.

## Decision

1. **The tool stops a prompt.** `eval_elisp` evaluates its expression with `inhibit-interaction` bound to t. When the expression tries to read input from the user, it stops there and the tool returns an error that tells the model what to do instead: find the value itself (`commandp` for whether a command exists, `key-binding` in the target buffer for a key, `read_buffer` or `read_file` for text), and ask the user (with `ask_user`, or in its reply) only for what only the user can answer. As with any evaluation error, effects of the expression before that point are not rolled back.
2. **One sentence of guidance.** The `emacs-keybindings` reference page says to check a command with `(commandp 'COMMAND)` and not to run it to test it.
3. **Asking the user stays where it belongs.** `ask_user` is unchanged. The approval question for a call is asked before its expression runs, outside the binding, and is unchanged.

## Boundaries

Emacs's `inhibit-interaction` stops minibuffer reads and `read-char`, `read-event` and `read-char-exclusive` (in Emacs 30.1 `barf_if_interaction_inhibited` is called in `minibuf.c` and `lread.c`), and so everything built on them: `completing-read`, `read-string`, `y-or-n-p`, `yes-or-no-p`, `read-passwd`, `read-multiple-choice`, and a command whose interactive form asks through them, such as `describe-function`. It does not stop `read-key-sequence` and what is built on it (`read-key`, the key prompt of `describe-key`): such an expression still reaches the user, as before this decision. A timer the expression starts runs after the tool returns and is not covered. This is a narrowing of point 1, not an equivalent of it.

## Alternatives considered

**Guidance only.** One sentence in the reference page and nothing in the tool: the smallest change, but the model may not follow it, and the next run can prompt the user again. The user chose the tool.

**Also stop `read-key-sequence` by advising it.** It would cover `describe-key`, but it puts a permanent advice on a core input function in every Emacs that loads superchat, and natively compiled callers bypass it unless subr trampolines are on. Not done; the case is recorded as a boundary.

**Refuse expressions that mention `command-execute` or `call-interactively`.** It guesses from text: a command can prompt without either word, and those words can appear harmlessly. `eval_elisp` is deliberately not classified by its text.

## Consequences

- A user who asks the model to run an interactive command for them (install a package, answer a question the command asks) gets an explanation instead and runs it themselves; that interaction is the user's own.
- An expression that loads code which asks a question (a file's risky local variables, a lock on a file) fails with the same explanation instead of asking.
- Test: `scripts/eval-elisp-regression.el`.

## Recorded at implementation (2026-09-24)

`inhibit-interaction` alone was not enough. On Emacs 30.1 `read-multiple-choice` reads each key with `read-event` inside `(condition-case nil … (error nil))` and asks again when nothing came back, so it caught the stop and looped for ever, printing its prompt; inside a tool call that is an Emacs that no longer answers. The tool therefore also binds `signal-hook-function` for the duration of the expression: the first `inhibited-interaction` throws straight out of the whole expression, past the expression's own handlers, and every other signal goes to the previous hook as before. Found by running the new tests on the Emacs 30.1 build; `eval-elisp/a-prompt-that-swallows-errors-still-ends` pins it on every version.

## Amendment — the same guidance in the operating note (2026-09-24)

The user, 2026-09-24: "A", to also put the guidance into the Emacs operating note that every request with `eval_elisp` or `edit_buffer` carries (`superchat-emacs-operating-note-text`), not only into the reference page the model may not read. The note's first line now reads "Check a symbol exists (fboundp, commandp, boundp); never run a command to test it." Reading a key with `key-binding` in the target buffer was already in the note's second line.

The note is limited to 1024 characters (`docs/decisions/implemented/2026-09-20-emacs-operating-note.md`) and was at 1023, so three pieces of wording were shortened: "Check that a symbol exists (…, apropos) before using it" lost "that", "apropos" and "before using it"; "To make your change live" became "To make it live"; "Report only what you checked" became "Report what you checked". The note was then 1021 characters; the same day's correction of the `lookup-key` line (`docs/decisions/implemented/2026-09-20-emacs-operating-note.md`, "之后的修改") brought it to 985. The 2026-09-20 decision's verbatim quote of the note is older than this change. Test: `operating-note/the-built-in-text-keeps-to-its-contract`; smoke baselines `A.txt`, `A.full.txt` change only in the note text.
