# Decision: What this Emacs can do

Status: implemented

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

## Decision

Three steps, each usable alone: find, record, declare.

1. **Find (L2): a deferred tool `describe`.** It reads Emacs's own
   documentation in layers: a prefix gives its public functions grouped
   by name prefix with a count per group, and how many `PREFIX--` names
   it has; a prefix with a QUERY gives names, first lines and key
   bindings, at most 30; a function gives its arglist and docstring; a
   buffer gives its major mode and the commands its keymaps bind. Class
   `read`. A `PREFIX--` prefix lists the private names, because some
   packages (telega) keep their real API there.
2. **Record (L1, L5): `/learn PREFIX`**, decided in the L5 note of
   2026-11-21. Recipes are only `eval_elisp` code an answer cited.
3. **Declare (L2, L3): a skill's interface.**
   - An entry is a code template with typed holes, the way a recipe
     ran: `(telega--getChatHistory (telega-chat-get chat_id) 0 0 limit)`,
     because many API functions take objects a model cannot pass as JSON.
   - Entries live in `interface.eld` beside `SKILL.md`: a list of plists
     (`:name`, `:class`, `:doc`, `:args`, `:code`), read with `read` and
     never evaluated as a whole.
   - A call checks each argument against its type (string, number,
     boolean) and puts it into the form as a literal; a string never
     becomes code. The form is evaluated and printed as `eval_elisp` does.
   - Each entry is a deferred tool `SKILL__NAME` (tool names may not hold
     a dot) of the entry's class. The skills catalog says how many tools a
     skill has; `load_tools` given the skill's name loads all of them, kept
     for the conversation. A `read` entry runs at `ask` without a question.
   - The tools are rebuilt before each request from the skills on disk,
     so editing `interface.eld` takes effect at the next request.

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
- Bare functions as entries, called with JSON values. Not taken: most of
  telega's API takes objects, so few functions could be offered.
- The interface inside `SKILL.md`, in the frontmatter or a fenced block.
  Not taken: the fixed YAML subset has no nesting, and a block would need
  its own parser; Lisp data needs none.
- Every skill's tools in every request. Not taken: the tool list would
  grow with each skill, against the deferred-tools decision.

## Consequences

- On the person's Emacs, `describe` of `telega` answers in 34 lines for
  979 functions. A live DeepSeek run collecting a telega chat went from 35
  calls with 10 guessed `eval_elisp` to 14 calls with none.
- An interface entry's code is the skill author's: a template that
  writes but is marked `read` runs without a question. The person writes
  or reviews `interface.eld`; the class is theirs to decide.
- Not built: `/learn` drafting `interface.eld` from its recipes, dreaming
  drafting skills, and marking a recipe to recheck when the package's
  version changes. Interfaces are written by hand until then.
- Prefix groups fit some packages badly; the query path covers them.
- Skills grow in number; the catalog already collapses above 20.
