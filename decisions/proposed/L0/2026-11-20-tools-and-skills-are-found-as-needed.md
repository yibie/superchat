# Decision: Tools and skills are found as needed

Status: proposed

## Problem

Every request carries the full specification of every tool offered: ten
built-in tools are about 7,300 characters, fourteen with the inbox and timer
tools, and every tool a plugin registers adds its own. The model pays for
all of them on every request, and a long list makes the right tool harder
to pick. Skills are already half progressive: each request lists every
skill's name, description and path, and the model reads a body with `read`
when it fits (`superchat-context.el`, "Skills you can use"). But the list
itself grows with every skill, and only skills under `superchat-directory`
are found, none of a project's.

The person wants both found progressively: the model sees that something
exists, then loads its detail when it needs it.

## Proposal

- **Tools.**
  - A tool is defined `:deferred t` or not. Not deferred, its specification
    goes with every request, as now: a small core (`read`, `grep`, `find`,
    `edit`, `write`, `shell`, `eval_elisp`, as the person agreed).
    Deferred, the system prompt
    lists only its name and first sentence.
  - A core tool `load_tools` (`names`) returns the specifications of the
    deferred tools named; from the next request of the run, those tools are
    offered as any other. A call of a deferred tool not loaded is answered
    with an error that says to load it first.
  - Plugin tools are deferred unless they say otherwise; the memory tools
    (`recall`, `note`, `tape_query`) and the timer tools are deferred.
  - What a run loaded is a `load` event on the Tape. Loaded tools stay
    offered for every later run of the timeline, as the person decided: a
    conversation that needed `recall` once will need it again, and
    reloading costs a round each time.
- **Skills.**
  - Three steps, as in Anthropic's skills: the catalog (name and
    description), the body (`SKILL.md`), then the files the body points to
    (scripts, references), each read only when needed.
  - Skills are found under `superchat-directory/skills/` and a project's
    `.superchat/skills/`; a project's skill wins over the user's of the
    same name.
  - Up to a limit (say 20 skills or 2,000 characters), the catalog lists
    each skill as now. Above it, the catalog says how many skills there are
    and a tool `skills` (`query`) searches names and descriptions and
    returns the matches with their paths.
  - A profile (`2026-11-20-a-profile-is-who-answers.md`) narrows the
    catalog to its skills, and could name the tools it loads at once.

## Alternatives considered

- Keep every tool in every request. Not taken: the cost grows with every
  plugin, and a long list makes the choice worse.
- Let the model search tools by description, as Claude Code's tool search.
  Taken in part: the deferred list names every tool, so a search is needed
  only when the list itself grows too long; `load_tools` by name first.
- Load a skill's body for the model when a word in the request matches.
  Not taken: the model decides better than a keyword whether a skill fits;
  a wrong match spends context on every request.
- A separate `skill` tool that returns a body. Not taken while `read` does
  it; the search tool is only for when the catalog is too long to list.

## Acceptance criteria

- A request offers only the core tools and lists the deferred ones by name
  and first sentence; after `load_tools` with `recall`, the next request
  offers `recall` and the one before did not; a later run of the same
  timeline offers it from its first request, another timeline does not.
- Calling a deferred tool not loaded is an error result naming
  `load_tools`, and the run goes on.
- With a project skill and a user skill of the same name, the catalog
  lists the project's.
- Above the limit, the catalog is one line and `skills` with a query
  returns the matching skills and their paths.
- The characters of tool specifications in a first request drop by at
  least a third against today's.

## Risks

- A model that does not load a tool it needs takes a round more, or tries
  `shell` instead; the first sentences in the list must say enough.
- What a run has loaded is state across requests; it must be on the Tape,
  not only in memory, or a paused run forgets it.
- Changes L2 (deferred tools, `load_tools`, `skills`), L3 (the system
  prompt's lists, the tools a request offers) and L5 for plugins; the L2
  file is at 803 of 900 lines.
