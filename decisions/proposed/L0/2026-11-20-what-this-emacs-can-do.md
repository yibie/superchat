# Decision: What this Emacs can do

Status: proposed

## Problem

People comparing Superchat with Codex or Claude operating a computer
call it limited: "collect the chats in telega, run a clutch query". The
power is there: `eval_elisp` evaluates in the person's own Emacs, and a
model read a telega buffer with it in one call. What is missing is
finding things. The model does not know which packages are loaded or
what their functions are, so it guesses: in one five-hour session 15
`eval_elisp` calls failed, most on guessed internal names. And every
`eval_elisp` is class `exec`, so at `ask` each one is asked, read or
not. Some packages have hundreds of commands, so listing them all is no
answer either.

## Proposal

Three steps, each usable alone: find, record, declare.

1. **Find (L2): a deferred tool `describe`.** It reads Emacs's own
   documentation, in layers, never all at once:
   - a package or prefix: how many public functions it has, grouped by
     name prefix or file, with a count per group;
   - a group, or a QUERY over names and docstrings: names, first lines
     and key bindings, at most 30, saying how many more there are;
   - a function: its arglist and docstring;
   - a buffer: its major mode and the commands its keymaps bind.
   Names with `--` are left out unless asked for. Class `read`.
2. **Record (L1, L4, L5): recipes from code that ran.** `/learn PACKAGE`,
   and dreaming for packages used that day, gathers the `eval_elisp`
   calls of the Tape that touched PACKAGE and ended without error, with
   how many runs used each, and asks a run to write
   `skills/PACKAGE/SKILL.md` from them: what each does, the code as it
   ran, the package's version. A model's account is never the source; a
   recipe whose version no longer matches is marked to recheck. The
   draft is shown to the person, who keeps or drops it.
3. **Declare (L2, L3): a skill's interface.** A `SKILL.md` may list in
   its frontmatter the functions it opens, each with its arguments and a
   class:
   ```yaml
   interface:
     - telega-chat-get-messages: [chat, limit] read
     - telega-chat-send-text: [chat, text] write
   ```
   Loading the skill makes each a tool with typed arguments that calls
   only that function, under its class's permission rule. Step 2's draft
   proposes the interface from the functions its recipes called.

`eval_elisp` stays for exploring; what is done often goes through a
declared interface.

## Alternatives considered

- Register commands by hand, one tool per function ("a queryable command
  list", as suggested in the discussion). Not taken as the base: Emacs
  already documents every function and keymap, and hand registration
  would cover only what someone wrote; it stays possible as plugins.
- Generate plugin code from a package. Not taken: generated Lisp must be
  reviewed as code, and the arguments of most functions have no types to
  generate from; a declared interface is data, checked when loaded.
- Let the model summarise what it learned into notes. Not taken: tried
  on this Tape, it repeated a wrong fact in 7 runs (see the facts
  proposal); only code that ran is a source.
- A read-only class for `eval_elisp`. Not taken: whether a form only
  reads cannot be told reliably; declared `read` functions give that
  where it is known.

## Acceptance criteria

- On the person's Emacs, `describe` of `telega` answers in under 60
  lines, and a group or query narrows it to the message functions.
- A run asked to collect a telega chat's messages finds the function
  with `describe` and needs no failed `eval_elisp` guess.
- `/learn telega` after such runs drafts a `SKILL.md` whose recipes are
  code that ran, with the package's version.
- With that skill's interface, a `read` function runs at `ask` without a
  question, and a `write` one is asked.
- An undeclared function cannot be called through the interface tools.

## Risks

- Prefix groups fit some packages badly; the query path covers them.
- Recipes go stale as packages change; the version mark only flags them.
- An interface marks a function `read` that writes. The person reviews
  the draft, and the class is theirs to decide.
- Skills grow in number; the catalog already collapses above 20.
