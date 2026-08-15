---
status: proposed
---

# Use progressive disclosure for Agent capabilities

Superchat should not spend model context on the implementation body of every
available Skill or on every command that Emacs can execute.  Following the
Agent Skills model and Pi's implementation, capability discovery should keep
small metadata resident and load instructions, command details, or bundled
resources only after the Agent chooses to activate them.

## Decision

- Keep capability metadata—name, description, and enough scope information to
  identify relevance—in the standing model context or an equivalent registry.
- Load a Skill's `SKILL.md` only on activation; load its `references/`,
  `scripts/`, and `assets/` independently and only when the activated task
  requires them.
- Discover Emacs commands progressively from the target Buffer Context rather
  than exporting the complete `M-x` namespace up front.
- Allow explicit activation (`>skill-name` or `/skill:name`) and model-directed
  activation from the resident descriptions.  An extra always-on Skill Router
  is not required by the design.
- Keep progressive disclosure separate from Authority: seeing a capability or
  loading its instructions never grants permission to execute its effects.

## Consequences

- Direct Chat and unrelated tasks do not pay the context cost of project Skills,
  mode-specific commands, or their supporting resources.
- Skill packages remain portable across Pi, Codex, Claude, and Superchat when
  they follow the Agent Skills directory and frontmatter conventions.
- Standard-format discovery must preserve lazy resource boundaries; converting
  every Skill into a fully expanded in-memory prompt is not the target design.
- Existing explicit Skill syntax remains a compatibility path while automatic
  adoption evolves toward the primary Agent selecting a Skill from metadata.
- This ADR records a future architecture direction only; it does not authorize
  changing the current importer, prompt assembly, or tests.

## Research basis

- [Agent Skills specification](https://agentskills.io/specification)
- [Pi Skills documentation](https://github.com/badlogic/pi-mono/blob/main/packages/coding-agent/docs/skills.md)
- [awesome-autoresearch curation Skill](https://github.com/yibie/awesome-autoresearch/blob/main/.agents/skills/autoresearch-curation/SKILL.md)
