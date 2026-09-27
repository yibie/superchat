# Decision: the model finds skills itself — a Pi-style catalog, loaded with read_file

Status: accepted — user, 2026-09-26: "superchat 的 skill 只能靠 >名字 手动触发，模型自身无法发现或读取——这个太糟糕了，我希望可以使用类似 Pi 那样的方式，来渐进式发现 skill，甚至可以通过自然语言提及 skill，让模型自动使用"; then "都按建议" (2026-09-27) to the five remaining questions (rely on the model when a skill is named; which skills are listed; which folders; bundled reference pages unchanged; the `denote-journal` skill written by the leader and reviewed by the user).

## 一句话

以后每次请求都带一份 skill 目录（名字、一句描述、文件位置）。模型遇到相关的事，或者你在话里提到某个 skill，它自己用 `read_file` 把那份 skill 读进来；skill 里提到的附带文件，用到时再读。你照样可以用 `>名字` 强制使用。

打个比方：以前 skill 像锁在柜子里的说明书，只有你亲手递给模型它才看得到；以后柜门上贴一张目录，模型自己看目录、按需去取。

## Problem

- `superchat-skills.el` loads a skill only through `>name` typed by the person (`superchat-skills-invoke`); a skill is a Preset whose body joins that turn's prompt (type `prompt`) or becomes the system persona (type `agent`/`plan`). The model never sees which skills exist and cannot read one.
- Skill files are single `name.md|org|txt` files in `superchat-skills-directory`. The Agent Skills standard (a directory holding `SKILL.md`) that Pi, Claude Code and DeepSeek Harness use is only half-supported: `superchat-preset.el` converts SKILL.md for export, and GitHub install flattens `name/SKILL.md` to `name.md`.
- The frontmatter reader `superchat-preset-parse-frontmatter` reads a YAML block scalar (`description: |`) as the single character `|`.

## What the references do

- **Pi** (`packages/coding-agent/src/core/skills.ts`, `docs/skills.md`): at startup it scans skill locations for directories containing `SKILL.md` (recursively) and, in its own folders, root `.md` files whose frontmatter has a non-empty `description`; the system prompt lists every skill not marked `disable-model-invocation: true` as `<available_skills>` with name, description (at most 1024 characters) and location, preceded by "Use the read tool to load a skill's file when the task matches its description" and "When a skill file references a relative path, resolve it against the skill directory". Its docs say models do not always load a skill; `/skill:name` forces it. Other agents' folders (`~/.claude/skills`) are added by the user in settings.
- **DeepSeek Harness** (`packages/skill`): a registry merged from providers (filesystem, bundled), a session catalog of names and descriptions, a model-facing `skill` loader tool, `/name` for the user.

## Decision

1. **A catalog in the system text.** Every request whose final tool surface has `read_file` carries, when at least one skill is listed, Pi's instructions and an `<available_skills>` list: name, description, absolute file location. The model loads a skill with `read_file`, including files the skill refers to by a relative path. Natural-language mention relies on the model seeing the name in the list (question 1, A); runs measure how often it misses.
2. **What is found.** In `superchat-skills-directory`: directories containing `SKILL.md`, at any depth, and root `.md`/`.org` files whose frontmatter has a non-empty `description`. In each directory of the new `superchat-skills-extra-directories` (default nil; question 3, B): directories containing `SKILL.md` at any depth. The first skill of a name wins, the skills directory first.
3. **What is listed** (question 2, A): every found skill with a description, except those whose frontmatter says `type: agent` or `type: plan` (they switch the session's mode and stay `>name` only) and those with `disable-model-invocation: true`. A description is cut at 1024 characters. The frontmatter reader for the catalog understands YAML block scalars (`|`, `>`).
4. **`>name` keeps working**, and also finds `name/SKILL.md` in the skills directory.
5. **Bundled reference pages are unchanged** (question 4, A): `read_reference` and its index stay as they are; merging them into the catalog is a later, separate decision.
6. The `denote-journal` skill used by the task simulation is written by the leader from the package's source and reviewed by the user before any run uses it (question 5, A). It is test data, not bundled.

## Amendment 2026-09-27: scripts in a skill

Asked whether a skill can bring its whole folder and scripts, the user answered: scripts come in every language ("Emacs 里的 Skill 脚本，语言是各色各样的，没有同意。尽管我们推荐用 elisp"); running one follows the permission level (Q2 A); no directory-listing tool — a skill names the files it uses, as in Pi and DeepSeek Harness, which give the skill's base directory and never list it (Q3 C).

- The catalog text tells the model how to run a script a skill names: an Emacs Lisp file with `eval_elisp` `(load "ABSOLUTE-PATH")`, any other script by starting its program from `eval_elisp` (for example `call-process`); either is full Emacs control and follows the session's permission level — asked about per call under the default `edit-buffers` level. It recommends nothing about the script's language to the model; the recommendation of Emacs Lisp is for skill authors.
- Reading a skill's other files needs no change: `read_file` with the path resolved against the skill's folder. Reading a folder is an error, and stays so.

## Alternatives considered

**A dedicated loader tool (DeepSeek Harness's `skill`).** Records "skill loaded" as its own event and can refuse unlisted names; lost to Pi's way, which the user named, needs no new tool, and reaches a skill's relative files with the same `read_file`.

**Merge skills into the reference index now.** One mechanism for bundled and user knowledge (DeepSeek Harness's shape); lost for now to changing one thing at a time.

**Load a skill automatically when its name appears in the person's message.** Guarantees use; lost for now because a common name (`review`) would fire by accident, and whether the model misses named skills is exactly what the runs will show.

**Read other agents' folders by default (`~/.agents/skills`, 66 skills here).** Pi does for `~/.agents/skills`; lost because most of those skills are written for command-line coding agents, and every request would carry their descriptions. One setting adds them.

## Acceptance

A new regression file (not the M3 set), red at the gap first: discovery rules (SKILL.md at depth, root files with and without a description, `type: agent`/`plan` and `disable-model-invocation` left out, extra directories only when set, the first name wins); a block-scalar description read whole and cut at 1024; the catalog text in Pi's shape with absolute locations; a chat request with `read_file` carries it, one without `read_file` does not, no skills means no block; a scripted model that reads a listed skill with `read_file` gets its text; `>name` finds `name/SKILL.md`. Then mutants, the full sweep, and the task simulation's `denote-journal` task with the skill in place.

## Consequences

- Every request that offers `read_file` costs the catalog's size (about 150–300 characters per listed skill); with no skills it costs nothing.
- A skill can now change what the model does without the person naming it; a skill's text is advice under the person's instructions and the session's permission level, as reference pages are.
