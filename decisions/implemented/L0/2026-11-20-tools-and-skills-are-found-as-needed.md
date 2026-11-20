# Decision: Tools and skills are found as needed

Status: implemented

## Problem

Every request carried the full specification of every tool offered: ten
built-in tools were about 7,300 characters, fourteen with the inbox and
timer tools, and every tool a plugin registers added its own. The model paid
for all of them on every request, and a long list makes the right tool
harder to pick. Skills were half progressive: each request listed every
skill's name, description and path, and the model read a body with `read`
when it fitted. But the list itself grew with every skill, and only skills
under `superchat-directory` were found, none of a project's.

The person wanted both found progressively: the model sees that something
exists, then loads its detail when it needs it.

## Decision

- **Tools.**
  - A tool is defined `:deferred t` or not. The core is not deferred:
    `read`, `grep`, `find`, `edit`, `write`, `shell`, `eval_elisp`, and
    `load_tools` itself, as the person agreed; `send` stays offered to the
    runs nobody watches. Deferred: the memory tools (`recall`, `note`,
    `tape_query`), the timer tools, and a plugin's tools unless it defines
    them `:deferred nil`.
  - The system prompt lists the deferred tools not loaded, each by name
    and first sentence. `load_tools` (`names`) appends a `load` event;
    loaded tools stay offered for every later run of the timeline, as the
    person decided, and no other timeline's.
  - `superchat-run :tools` nil is now the offered set, worked out for each
    request so a tool loaded in a run is offered at its next request;
    `(except NAMES...)` is that but NAMES, which the chat uses to leave out
    `send`; an exact list stays exact.
  - Each call is checked against the names its request offered: a known
    tool not offered is an error result, which says to load it when it is
    deferred; the run goes on. Before, a tool not offered (`send` in a
    chat) ran anyway.
  - The base instructions and the dreaming task say to load `recall` and
    `note` with `load_tools`.
- **Skills.**
  - Found under a project's `.superchat/skills/` and
    `superchat-directory/skills/`; a project's skill hides the user's of
    the same name.
  - Up to `superchat-context-skills-listed` (20) skills the catalog lists
    each, as before. Above it, one line says how many there are and where,
    and to find one with `find` and `grep` over their `description:`
    lines, then read it.

## Alternatives considered

- Keep every tool in every request. Not taken: the cost grows with every
  plugin, and a long list makes the choice worse.
- A search over tool descriptions, as Claude Code's tool search. Not taken
  now: the deferred list already names every tool; `load_tools` by name.
- A `skills` search tool, as the proposal had. Not taken: `find` and
  `grep` over the skill directories do it with the core tools, so the core
  stays the seven the person agreed.
- Keep the memory tools in the core. Not taken: the agreed core did not
  hold them; the prompts that name them say to load them, and a timeline
  keeps them loaded once loaded.
- Fix the offered list at the start of a run. Not taken: a tool loaded in
  a run would then be offered only from the next run.

## Consequences

- A first request of a timeline offers eight tools, not fourteen or more;
  the catalog of deferred tools moves some characters into the system
  prompt.
- A conversation that needs `recall` spends one round on `load_tools`, once
  per timeline.
- A plugin whose tools must be offered at once defines them `:deferred
  nil`.
- The dreaming task's text changed, and `superchat-timer-dream` knows a
  scope's dreaming timer by that text. A timer set before (on the person's
  Tape, #3749) is no longer found: `/dream` would set a second one and
  `/dream off` would not stop the old. The person cancels it with
  `/timers cancel` and turns dreaming on again; there is no migration.
  Knowing the timer by a data key instead of its text would end this, and
  is a rule change of its own.
