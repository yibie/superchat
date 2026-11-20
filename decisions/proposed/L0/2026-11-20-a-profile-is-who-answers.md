# Decision: A profile is who answers

Status: proposed

## Problem

What answers a run is spread over options with different reaches. The model
can change per run (`superchat-run :model`) and so can extra system text
(`:system`) and the tool list (`:tools`). The provider cannot:
`superchat-llm-provider` is global and `superchat-llm-request` takes only
`:model`. Every skill under `skills/` enters every request's catalog. A
plugin, once enabled, adds its tools, views and commands for everything.
The level is per timeline and the reasoning effort global. So "the reviewer
on a slow model with read-only tools and two skills" cannot be named, kept,
reused or seen on the Tape afterwards.

gptel solves the same with presets (`gptel-make-preset`, gptel.el:2522 in
the person's checkout): a name for a set of options (`:backend`, `:model`,
`:system`, `:tools`, any `gptel-*` option), `:parents` applied first,
`:append`/`:prepend` to build on a parent instead of replacing it, and an
`@name` cookie in a prompt that applies a preset to that message only.
oh-my-pi defines agents as Markdown files with YAML frontmatter (`tools`,
`spawns`, `model`, `thinkingLevel`), the body being the system prompt
(`task/discovery.ts`, `prompts/agents/reviewer.md`).

A profile is useful with one conversation as much as with many; it is also
what a delegated agent would be, if the multi-agent proposal is taken
(`2026-11-20-many-agents-at-once.md`).

## Proposal

- **A profile** is a name and a fixed set of fields:
  - `description`: one line, shown when choosing;
  - `parents`: profiles applied first, in order;
  - `provider`, `model`, `reasoning`, `level`;
  - `tools`, `skills`: names, replacing the parent's; `add_tools`,
    `add_skills`: names added to the parent's;
  - `system`: text added after the parent's.
  An unknown field is an error that names it.
- **Two ways to define one, the same fields:**
  - Elisp: `(superchat-profile-define NAME &key description parents provider
    model reasoning level tools add-tools skills add-skills system)`.
  - A file `profiles/NAME.md` under `superchat-directory` or a project's
    `.superchat/`: YAML frontmatter in a fixed form, the body the `system`
    text. The project's file wins over the user's, as omp's order does.
    ```markdown
    ---
    description: Reviews a diff for defects only
    parents: [base]
    model: deepseek-pro
    level: read-only
    tools: [read, grep, find]
    skills: [code-review]
    ---
    Report defects with file and line. No praise, no style notes.
    ```
- **The fixed form of the YAML** is a subset Superchat reads itself, since
  Emacs ships no YAML reader and SPEC allows no dependency: one `key: value`
  per line; a value is a plain or quoted scalar, or a flow list `[a, b]`,
  or a block list of `- a` lines; no nesting, no anchors, no multi-line
  scalars. Anything else is an error with the line. A file and a
  `superchat-profile-define` that say the same make the same profile.
- **Using one:**
  - `superchat-run :profile NAME` applies it for that run: model, provider,
    reasoning, the tools offered, the skills in the catalog, the system
    text; `:level` is the timeline's when the profile names none.
  - The chat: `/profile NAME` for the buffer's next runs, `/profile` to show
    it; `@NAME` at the start of a message for that message only.
  - The `user` entry records `:profile NAME`, so the Tape says who answered.
- **Plugins:** a profile chooses among the tools a plugin registered, as it
  chooses any tool. Enabling a plugin stays global: a plugin may add hooks
  and commands, which cannot be limited to one run without changing the
  plugin specification (L5).

## Alternatives considered

- Only Elisp, as gptel. Not taken: the person wants files that can be read,
  edited and shared, as omp's agents are.
- Only Markdown files. Not taken: the person wants both; Elisp also lets a
  package define a profile.
- Full YAML through a package. Not taken: SPEC allows only what Emacs ships;
  a fixed subset is enough for flat fields and gives exact errors.
- Profiles that enable and disable plugins. Not taken now: enabling is
  global; per-run plugins need the plugin rules rewritten.
- `:append`/`:prepend` on every field, as gptel. Not taken: `add_tools`,
  `add_skills` and `system` (always added) cover what a profile builds on;
  fewer forms, one meaning each.

## Acceptance criteria

- A file and an Elisp definition with the same fields give `equal`
  profiles; a file with an unknown key, a nested map or an unclosed list is
  refused with its line.
- `parents` apply in order; `add_tools` adds to the parent's tools; `tools`
  replaces them; `system` texts join parent first.
- A run with `:profile` sends that provider and model, offers only its
  tools, lists only its skills, and its `user` entry has `:profile`.
- `@reviewer` applies to one message; the next uses the buffer's profile.
- A project's `profiles/NAME.md` wins over the user's of the same name.

## Risks

- Changing the provider per request touches `superchat-llm.el`, the only
  file that knows the bridge.
- Six layers change (L0 to L5); the order is L0, L2 (provider and skills
  per request), L3 (profiles, `:profile`), L5 (commands).
- A profile's `level` lower than the timeline's is clear; a higher one
  (`auto` in an `ask` conversation) gives a run more than the person set,
  and may need to be refused or asked.
