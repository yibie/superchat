---
name: handoff-readme-rewrite
audience: deepseek-v4-pro
status: ready
predecessor: docs/handoff-v1.0.1-patch.md (shipped, HEAD 2bd1aff)
do_not_commit_pkg_files: true
---

# Handoff: rewrite README for v1.0 product positioning

## Why you are reading this

`README.md` (569 lines) and `README_cn.md` (557 lines) are still
written for v0.5 — they describe superchat as "a friendly,
Claude-Code-style chat UI for Emacs." That positioning was true 7
months ago. By v1.0 superchat is something materially different:

> A **hook-based agent runtime** inside Emacs. Memory, skills,
> workflows, MCP tools, and file context all reach the LLM by being
> registered onto the same `superchat-core-run-turn` hook pipeline.
> Every feature is a plugin in the same shape; users can write their
> own.

The current README does not say this. It also has factually wrong
or outdated content (the `llm` minimum version, references to the
deleted `org-ql` dependency, no mention of SKILL.md format, no
mention of `type: workflow`). This handoff rewrites both READMEs.

**You are NOT changing any code. You are NOT submitting to MELPA.**
Your only deliverables are: edited README.md, edited README_cn.md,
two or three new example skills under `examples/standard-skills/`,
one new `docs/architecture.md` design page, and a commit.

## Trust the repo, not your priors

The previous session writing this handoff drifted into hallucinated
edits when working on code. **You are doing prose work here, which
is safer, but verify everything against git.** Re-confirm before
each step:

```bash
git status     # must be clean before each step
git log --oneline -3
```

HEAD this handoff was written against:
```
2bd1aff release: v1.0.1 — lint cleanup + correct llm minimum
```

If `git status` shows uncommitted changes, **stop** and ask the user.

---

## The product positioning you must convey

These are the points the new README must land. They are factually
true (verified against the repo on 2026-06-05). Do not editorialize
beyond what's here.

### What superchat actually is

A chat buffer in Emacs that resolves every turn through a single
hook pipeline (`superchat-core-run-turn`, in
`superchat-core.el:118`). The pipeline runs:

1. `superchat-core--parse-input` — extracts `@model`, `>skill`,
   `/command`, `#file` tokens
2. `superchat-system-prompt-functions` — builds the system prompt
3. `superchat-build-prompt-functions` — builds the user prompt
4. `superchat-post-turn-functions` — side effects (tape recording,
   memory capture)

`superchat-prompt-hooks.el` registers 5 prompt-building hooks today:
language instruction, file inlining, template substitution, memory
context, conversation history. Adding a new prompt builder = writing
a function `(lambda (turn) ...)` and `(add-hook ...)`. This is
**the** differentiator.

### What it shares with other Emacs LLM clients

- Streaming responses, model switching (`@model` syntax), MCP tools.
- Talks to any provider via [`llm.el`](https://github.com/ahyatt/llm).

### What separates it from gptel / ellama / chatgpt-shell

| Feature                              | gptel | ellama | chatgpt-shell | superchat |
|--------------------------------------|:-----:|:------:|:-------------:|:---------:|
| Multi-provider via `llm.el`          | yes   | yes    | (own)         | yes       |
| Streaming                            | yes   | yes    | yes           | yes       |
| MCP tools                            | (3rd-party) | no | no       | yes       |
| Per-turn `@model` override syntax    | no    | no     | no            | **yes**   |
| `#file` reference syntax             | (region) | (region) | no       | **yes**   |
| SKILL.md skill format (frontmatter + body) | no | no | no       | **yes**   |
| Multi-step workflows as a sub-type   | no    | no     | no            | **yes**   |
| SQLite-backed persistent memory with FTS5 | no | no | no         | **yes**   |
| Hook-based extension model (Bub-style runtime) | no | no | no | **yes** |

Frame this as "what you get by paying the cost of installing a
fourth chat client," not as a victory lap.

### What it does NOT do (be honest)

- No agent loops with tool use across many turns (single-turn tool
  calls only — multi-turn agentic loops are Phase 2 / out of scope).
- No retrieval-augmented generation against vector stores. Memory
  is FTS5 over your past captures, not embeddings.
- No multi-agent orchestration.
- No image / vision input (whatever `llm.el` supports passes
  through; superchat doesn't add anything).
- No telemetry, no cloud sync, no account system.

### Known limitations to disclose

- Memory and workflow features were rewritten in v0.7 / v0.8; the
  test suite includes one cosmetic known-tolerance
  (`roundtrip/code-review-byte-identical` was fixed but format-
  related tolerances exist in the prior history). Currently green
  103/103.
- Min Emacs: 28.1. Min llm: 0.24 (introduced `llm-models` API).
- Not yet on MELPA — install from source (see Install section).

---

## Source-of-truth facts you'll need

Read each before writing about it. Do not paraphrase from memory.

### Installation facts

```
;; Version: 1.0.1
;; Package-Requires: ((emacs "28.1") (llm "0.24"))
```

Source: `superchat.el:8-9`. Old README says `llm 0.7` and lists
`org-ql` as optional — both wrong now.

### Files-and-modules map

```
superchat.el              user-facing entry, customs, key bindings
superchat-core.el         turn struct, hook defvars, pipeline runner
superchat-dispatcher.el   superchat-send-input, dispatch result types
superchat-prompt-hooks.el 5 registered prompt builders
superchat-llm.el          llm.el call wrapper
superchat-render.el       buffer rendering, MD→Org, streaming insertion
superchat-memory.el       SQLite memory facade (capture, retrieve, prune)
superchat-db.el           SQLite layer (memory + tape tables)
superchat-skills.el       skill resolver, /command, >skill parsing
superchat-skills-standard.el  SKILL.md frontmatter loader + export
superchat-workflow.el     type: workflow executor (step-by-step)
superchat-mcp.el          MCP server tool integration
superchat-tools.el        built-in tool registry
superchat-models.el       model listing, @model parsing, caching
superchat-save.el         conversation export to org files
superchat-parser.el       small parsers (file refs, @model, /cmd)
superchat-executor.el     LLM-call helpers used by workflow
```

### Skill format facts

Canonical SKILL.md (verified at `skills/code-review.md`, see also
`docs/handoff-v0.7-skills-workflow.md` for the format contract):

```markdown
---
name: code-review
description: 专业的代码审查助手
version: "1.0"
type: prompt
triggers: ["审查", "code review"]
---

# Body — system prompt for type: prompt skills

You are a code review assistant. ...
```

For `type: workflow`, the body is N steps, one per non-empty
non-comment line:

```markdown
---
name: weekly-summary
description: Generate a weekly tech-news summary
version: "1.0"
type: workflow
---

# Each line below is one step. # comments and blank lines are skipped.
# Variables: $input $lang $date

/web-search "$input" recent news
@gpt-4o-mini 给上面结果做 3 个角度的中文摘要（商业/技术/社会）
将分析结果保存到 #~/Documents/weekly-summary.md
```

Field rules (from the v0.7 contract):
- `name`, `description` required
- `version` defaults to `"1.0"`
- `type` defaults to `"prompt"`; only `"prompt"` and `"workflow"` valid
- `triggers` defaults to `[]`

### Public commands a user actually types

From the dispatcher + alist + custom commands. Verify each by
grep before writing about it:

```
/recall <keywords>     — search memory, surface matches before next turn
/remember [text]       — capture text to memory (or capture last exchange)
/skill-install <url>   — install a skill from a URL or local path
/commands              — list all available commands
/reset                 — clear buffer + history + current command mode
/clear-context         — clear attached file context
/clear                 — clear the chat buffer
/define <name>         — define a new custom command on the fly
```

Plus prefixes:
- `@<model-name>` — one-turn model override
- `>skill-name [arg]` — invoke a skill
- `#path/to/file` — attach file as context

### Keybindings

From `superchat.el` around line 1420. Confirm by grep:

- `C-c C-c` — send input
- (whatever else `define-key` sets up — verify don't invent)

---

## Execution plan

### Step 1 — Audit & inventory (no writes)

Before editing anything, build the fact base. Produce a working
file `/tmp/readme-facts.md` (not committed) containing:

1. Output of `grep -n "^##\|^###" README.md` (current section list)
2. Every claim in current `README.md` that is FALSE as of HEAD
   2bd1aff. Grep to verify each. Examples to check:
   - "llm 0.7" → false, it's 0.24
   - "org-ql for cached memory queries" → false, no longer a dep
   - "completely independent of org-supertag" → still true (verify)
   - "gptel" anywhere → false, switched to llm.el in v0.5
3. Every public command, prefix, and keybinding. Grep to enumerate.
4. The 5 hook functions in `superchat-prompt-hooks.el` — list them
   with one-line summaries.
5. List of MCP-related variables and commands.

This audit feeds Steps 2-4. **Do not commit this file.**

### Step 2 — Rewrite `README.md`

Target length: 350-450 lines (current is 569). Cut the bloat, add
the positioning.

**Required sections, in this order**:

```
# superchat

[one-paragraph hero — what it is, who it's for, why now]

## Why superchat

[the comparison table from above, slightly trimmed; followed by 2-3
paragraphs of "when to pick superchat over X"]

## Install

[Requirements: Emacs 28.1, llm 0.24. No mention of org-ql.
manual install from source, since not on MELPA. Provider examples
trimmed to 3: OpenAI, Anthropic, Ollama.]

## 5-minute tour

[Five concrete things a user can do in their first session, each
shown as a short transcript. Pick from:
- Plain chat with streaming
- @model switching mid-conversation
- #file attachment
- /remember + /recall round-trip
- >code-review skill invocation (uses skills/code-review.md)
- Tool call (one of the built-in tools)
Pick five. Each is 3-8 lines max.]

## Core concepts

### Skills (SKILL.md)
[Format spec from above. Worked example. Link to skills/* in repo
and to docs/SKILLS_QUICKSTART.md.]

### Workflows (type: workflow)
[Format spec. Worked example. Note: each step is a fresh turn
through the same pipeline.]

### Memory
[SQLite + FTS5 facade. /remember writes, /recall reads, auto-recall
on every turn for queries above the threshold. Storage location.
How to inspect.]

### MCP
[How to wire a server, how tools surface. Reference existing config
docs.]

### The hook pipeline (advanced)
[Two paragraphs + one code block showing the function signature
of a hook function. Forward-reference to docs/architecture.md.]

## Customization

[The most-used 8-10 defcustoms, with one-liners. Not all of them.
Refer to M-x customize-group superchat for the rest.]

## Configuration cheatsheet

[A small block: data dir, language, default model, history limit.
Don't repeat installation.]

## Troubleshooting

[3-5 actual issues users hit: no llm backend configured, model not
found, tool calls failing, memory not persisting. One paragraph each.]

## Project status & roadmap

[v1.0.1 shipped. What's stable, what's experimental. Pointer to
ROADMAP.md.]

## License

GPL-3.
```

**Hero paragraph (write this first; everything else flows from it)**:

> superchat is a chat interface for large language models inside
> Emacs, built on a hook pipeline that turns every feature —
> memory, skills, workflows, tool calls — into a plugin in the
> same shape. If you want a Claude-Code-style chat in your editor
> but also want to write your own context-builders and post-turn
> side effects without forking the project, this is for you.

You may rephrase, but keep the three load-bearing claims: (a) chat
interface inside Emacs, (b) hook pipeline, (c) extensibility for
people who want to write their own hooks.

**Things to delete or shrink from current README**:

- "Key features include: Retaining the complete command system /
  Adding the ability to include files as context / ..." — bullet
  list that says nothing. Cut.
- "Open-sourced under the GPL-3 license" in the intro — move to
  License section.
- The 8-9 model-provider examples — keep 3.
- The long "Notes on Tool Calling with Local Models" subsection —
  trim to 4-5 sentences or move to a `docs/tool-calling.md`.
- Most of the per-command long descriptions — one line each.
- Anything starting with "Currently we support..." in marketing
  voice — rewrite as fact.

**Things to add that aren't there**:

- The "Why superchat" comparison table.
- The 5-minute tour with real transcripts.
- The hook pipeline concept.
- A note that the project is GitHub-only (not on MELPA yet) and
  why (so contributors don't file MELPA-prep bug reports).
- Link to `docs/architecture.md` (you'll write this in Step 4).

### Step 3 — Rewrite `README_cn.md`

Same structure as `README.md`, in Simplified Chinese. **Not a
machine translation.** The original `README_cn.md` reads like
careful native prose — match that register.

Use the existing `README_cn.md` for tone and terminology. Reuse
its translations of: 命令系统, 文件上下文, 模型切换, 工具调用, 记忆,
工作流, 钩子, etc. Don't invent new terms for established ones.

If a phrase in English has no good Chinese equivalent (e.g. "hook
pipeline"), keep the English term parenthetically: 钩子管线 (hook
pipeline).

### Step 4 — Write `docs/architecture.md`

300-500 lines. The technical companion to the README. Audience:
someone who wants to write their own hook or extend the pipeline.

Required sections:

```
# Architecture

## The pipeline

[ASCII diagram or numbered list of the parse → system-prompt →
build-prompt → post-turn flow. Reference superchat-core.el:118.]

## The turn struct

[Field-by-field. What each slot is for. When it's written.
Reference superchat-core.el:22-41.]

## Hook function signatures

(turn) → modified-turn or nil

[Worked example: take superchat-prompt-hook--memory-context from
superchat-prompt-hooks.el:132-139 and walk through it line by
line. Then show what a new hook to inject Git-blame context
would look like.]

## Registering a hook

[Example: a user dropping a (add-hook ...) into their init.el to
inject the current buffer's file path into every prompt.]

## The five built-in build-prompt hooks

[One paragraph each. Reference the actual function names.]

## The dispatcher

[Quick explanation of how superchat-send-input calls run-turn,
then picks a result type (:llm-query, :buffer, :echo, etc) and
hands off to superchat--dispatch-result. Reference
superchat-dispatcher.el:219.]

## Adding a new command

[The /command alist in superchat-dispatcher.el. How to add one.]

## Adding a new skill

[Pointer to docs/SKILLS_QUICKSTART.md, plus a paragraph on the
type: prompt vs type: workflow difference.]

## What's NOT pluggable yet

[Honest disclosure: tool registration is mostly hard-coded,
multi-turn agent loops aren't supported, etc.]
```

This is the page that says "here's how it actually works, here's
how to extend it." It's the document that justifies the "hook-based
runtime" positioning. Without this page, the README's claim is
unsupported.

### Step 5 — Two or three new example skills

Currently `examples/standard-skills/` only has `code-review/` (a
duplicate of the in-repo skill). The README will reference these
examples; add 2-3 more so the reference resolves to something
interesting.

**Required**: at least one `type: prompt` and one `type: workflow`.

Pick from this list (or invent better; just make them realistic):

- `examples/standard-skills/git-commit-message/SKILL.md`
  (`type: workflow`): step 1 reads `#git-diff` output via a
  /command, step 2 asks LLM to write a conventional-commit message,
  step 3 inserts it. Three steps.
- `examples/standard-skills/explain-region/SKILL.md`
  (`type: prompt`): system prompt that asks the LLM to explain
  the attached code region as if to a senior engineer who's new
  to the codebase.
- `examples/standard-skills/weekly-tech-digest/SKILL.md`
  (`type: workflow`): something like the weekly-summary example
  in this handoff, but actually useful.

Each example must:
1. Validate cleanly through `superchat-skills-standard--parse-frontmatter`
   (test by loading via the existing test machinery if practical;
   otherwise eyeball the format against `skills/code-review.md`).
2. Include a short `references/README.md` in the example
   directory explaining what the skill does and how to invoke it.

Do NOT add more than 3. The point is "here are some realistic
examples to copy from," not a complete library.

### Step 6 — Update `docs/SKILLS_QUICKSTART.md`

The existing file (139 lines) was written before the SKILL.md
format unification in v0.7. Update it to:

1. Use the canonical frontmatter format (not the old ad-hoc
   header).
2. Include the `type: workflow` section.
3. Reference the new examples from Step 5.
4. Link back to `README.md#core-concepts` and forward to
   `docs/architecture.md`.

Keep its current Chinese voice. Don't translate to English.

### Step 7 — Update `AGENTS.md` if needed

`AGENTS.md` (116 lines) is the "quirks for AI agents" doc. After
reading it, decide if any of its claims are obsoleted by v0.7-v1.0
work. Most likely candidates:

- Mentions of `gptel` (v0.5 switched away)
- Mentions of `superchat-agent.el` (deleted in v0.5)
- Mentions of `memory.org` or `soul.org` (replaced by SQLite in v0.8)

Update only the false claims. Do not restructure.

---

## Validation checklist (run after each step)

```bash
# Working tree
git status

# Lint your prose: check links, check for stale references
grep -nE "\\(llm \"0\\.[0-9]+\")" README.md README_cn.md
# Must show: 0.24 (or higher). Anything <0.24 is wrong.

grep -nE "org-ql|gptel|superchat-agent" README.md README_cn.md docs/architecture.md
# Must be empty (gptel may appear in a "we moved away from" historical
# note; org-ql/superchat-agent should be gone).

grep -nE "soul\\.org|memory\\.org" README.md README_cn.md
# Must be empty.

# Skills validate
emacs -batch -L . --eval "(progn
  (require 'superchat-skills-standard)
  (dolist (dir (directory-files \"examples/standard-skills\" t \"^[^.]\"))
    (when (file-directory-p dir)
      (let ((meta (superchat-skills-standard--load-metadata
                    (expand-file-name \"SKILL.md\" dir))))
        (princ (format \"%s: %S\\n\" (file-name-nondirectory dir) meta))))))"
# Each example must produce a non-nil plist.

# Test suite must still pass (you didn't touch code, so it should)
emacs -batch -L . -L test -l ert \
  -l test/test-skills.el \
  -l test/test-skills-roundtrip.el \
  -l test/test-workflow.el \
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran" | tail -1
# Must still show 0 unexpected.
```

---

## Out of scope (do NOT touch)

- Any `.el` file. This handoff is prose-only. If you find a real
  bug while reading code for facts, document it in the final
  report under "found but not fixed."
- Anything in `test/`.
- `ROADMAP.md`. Leave it as-is.
- The v1.0.0 / v1.0.1 git tags. They stay.
- MELPA submission. **Explicitly do NOT submit, and do not add
  any MELPA-related content to the README except a one-line "not
  yet on MELPA, install from source" note.** The user has decided
  to defer MELPA until docs are good.
- Screenshots or GIFs. Useful but out of scope for a prose
  rewrite. Leave a `<!-- TODO: screenshot of @model switching -->`
  HTML comment where the README would benefit.
- New defcustoms or new commands. README documents what exists.
- Translating the architecture doc into Chinese. English-only for
  Step 4.

---

## Style requirements

- **No marketing voice.** "Blazingly fast," "revolutionary,"
  "seamless," "powerful," "leverage" — kill on sight.
- **Concrete over abstract.** "Type `>code-review` after attaching
  a file" beats "easily invoke contextual code analysis."
- **Show transcripts when describing usage.** A 4-line example
  beats a paragraph of description.
- **Speak to the reader as a peer.** "If you want X, do Y." Not
  "Users can leverage..."
- **Cite source files.** When describing how a feature works,
  reference `superchat-foo.el:NN` so curious readers can verify.
- **Don't invent features.** If you're not sure something exists,
  grep before writing. The previous session producing this
  handoff hallucinated several functions; protect against that.

---

## Final report

Reply with:

1. Commit SHA (single commit fine, or split per-document — your call).
2. Line counts: README.md before/after, README_cn.md before/after,
   docs/architecture.md (new), docs/SKILLS_QUICKSTART.md before/after,
   AGENTS.md before/after.
3. New files created with line counts.
4. Output of the validation greps in the checklist (must show
   the absences claimed).
5. Test suite output (must remain green; you shouldn't have touched
   code).
6. List of stale claims you found in the current READMEs that you
   corrected — each as one line: "false: <claim>; true: <fact>".
7. List of "found but not fixed" code-level issues, if any.
8. Anything you skipped, with reason.

If a positioning decision feels uncertain (e.g. should "memory" or
"skills" come first in the Core Concepts section?), pick one,
ship it, and note your reasoning in the final report. Don't ask the
user mid-flight. The user will iterate on tone in a follow-up if
needed.
