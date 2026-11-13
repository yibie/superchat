# Skills and workflows

A skill is a reusable instruction kept as a markdown file; a workflow is a recipe of several prompts run one after another. The tutorial [SKILLS_QUICKSTART.md](../SKILLS_QUICKSTART.md) walks through writing a first skill; this page is the reference.

## Skills

Skills live in `superchat-skills-directory` (default `skills/` under `superchat-data-directory`); the bundled ones are copied there on first run. The bundled skills are `code-review`, `planning`, `refactor` and `render-ui` (the generative-UI syntax for the `render_ui` tool). Apply one with `>name` at the start of a message, or through a Preset:

```
>code-review #~/src/utils.el
```

A skill is markdown with YAML frontmatter. A `type: prompt` skill (the default) injects the body as the system prompt for that turn:

```markdown
---
name: explain-region
description: Explain a code region to a senior engineer new to this codebase
version: "1.0"
type: prompt
triggers: ["explain this", "这段代码什么意思"]
---

You are a senior engineer explaining code to a peer. Focus on:
1. What this code actually does (not what it's supposed to do)
2. Why it's structured this way
3. Any non-obvious edge cases
```

Field rules: `name` and `description` are required. `version` defaults to `"1.0"`. `type` defaults to `"prompt"`; the valid values are `"prompt"`, `"agent"` and `"plan"`. `triggers` defaults to `[]`.

A `type: agent` skill activates a multi-turn Agent Profile with tool access. Give it a stable `profile_id`, independent from the mutable `name`, and declare `capability` explicitly for its work purpose (for example `profile_id: profile.coder` with `capability: coding`). Changing `profile_id` creates a new historical identity; changing `name` does not. A Request may omit or repeat that Capability, but the Runtime rejects a conflicting selection before creating the Run. A Profile without `profile_id` warns, falls back to `name` as identity, and uses the Runtime-selected Capability (or `chat` when none was selected). `type: plan` activates read-only planning mode. Multi-step recipes are `.workflow` files, not a skill type.

### The model finds skills itself

Every request that offers `read_file` carries a skill catalog: the name, the one-line `description` and the file location of each skill. When a task matches a description, or when you name a skill in your message ("use the denote-journal skill"), the model reads that skill with `read_file`, and reads any file the skill points to by a relative path when it needs it. The catalog lists folders holding a `SKILL.md` (the Agent Skills layout of Pi, Claude Code and Codex) at any depth in the skills directory, and single `.md` / `.org` skills there that have a `description`. `superchat-skills-extra-directories` adds other folders, for example `~/.agents/skills`. Mode skills (`type: agent` or `plan`) and skills marked `disable-model-invocation: true` are applied with `>name` only. `>name` also finds a folder `name/SKILL.md`.

`/skill-install user/repo[@branch]` installs a skill from a repository. More complete examples are in `examples/standard-skills/`.

## Workflows

A workflow is a linear recipe in a `.workflow` file under `superchat-workflow-directory` (default `workflow/` under `superchat-data-directory`). Each non-empty, non-comment line is one step; steps run one after another through an asynchronous callback chain, so Emacs stays responsive for the whole run.

```
# research.workflow — lines starting with # are comments, blank lines are skipped.
Search for the latest information about "$input" on the web. Provide a comprehensive overview.

# Step 2 sees step 1's output through $result.
Based on this information: $result, summarise the three most important points concisely.

# Any earlier step is addressable as $stepN.
Step 1 found: $step1. Step 2 summarised: $step2. Write one concluding sentence about $input.
```

Run it with `>>research Emacs 31` or `/workflow research Emacs 31` (`>` is for skills, `>>` for workflows). The first `>>` invocation loads the workflow code and runs the recipe at once. For discoverable input, complete `/workflow`, type a space, then complete again to choose a saved recipe; Corfu or company can show the candidates, and `M-TAB` (`completion-at-point`) also works. Completion fills only the name and never runs it; type your task after the name and press `C-c C-c`.

Variables: `$input` (the trigger argument), `$result` (the previous step's output), `$stepN` (step N's output), `$lang` (`superchat-lang`, default `"English"`), `$date`.

Per-step annotations: `@model-name` at the start of a line sets the model for that step; `/command args` at the start of a line expands a user-defined command's prompt template with its args bound to `$input`; a leading `#~/path/to/file` or `#/absolute/path/to/file` attaches a context file. The remaining text is sent as the step prompt, and later `#...` body text (such as "issue #42") is left alone.

Names: a workflow name is the file's basename in that directory; discovery compares basenames in NFC form, so NFC and NFD spellings resolve to the same file. Keep the name short and put the instructions in the file body. A name containing spaces must be double-quoted when invoked, `/workflow "weather report" London` or `>>"weather report" London`; completion adds the quotes, and unquoted names end at whitespace. A leading `@model` is read as a model switch before `>>` routing, so an `@`-prefixed token is reserved there. An empty `>>` name reports usage.

`M-x superchat-workflow-edit` opens an existing recipe or prepares a new one; save it with `C-x C-s`. Opening the editor never saves or runs the workflow. `examples/research.workflow` is a runnable sample. `M-x superchat-workflow-import-legacy-dir` turns `type: workflow` SKILL.md files into `.workflow` files.

A step advances only from its unique recorded `run.completed` with status `completed`. A Run with no terminal waits; other terminal outcomes, conflicting terminals and unreadable records stop advancement. This reads the recorded outcome rather than judging the whole history; see the [request lifecycle](../protocols/request-lifecycle.md).

### The research workflow

`M-x superchat-research-workflow` runs Collector, Analyst and Reviewer as sequential child Actors that publish immutable Artifacts. After restarting Emacs, `M-x superchat-research-workflow-status` shows the recorded Run outcomes, Artifact availability and the next resumable stage. `M-x superchat-research-workflow-resume` starts a stage only when the recorded predecessor outcomes and Artifact checks permit it; unavailable Artifacts may still need manual review.
