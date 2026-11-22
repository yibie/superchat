# Superchat FAQ

What happens when you hand Superchat a task, what you control, and how it gets better. For keys, commands and options, see [reference.md](reference.md).

## What it can do

**What can Superchat actually do in Emacs?** Anything Emacs Lisp can do in your session, within the permission level you set. For example:

- Find all Org tasks due this week and summarize them.
- Find out how a package exposes a function, then use it.
- Debug an error in your Emacs configuration.
- Find today's Telega messages about a topic.
- Fix a failing test in the buffer you are working on.
- Turn a package task that worked into a skill with `/learn`.

**Can it work with any Emacs package?** Yes, with any package that is installed and loaded. Superchat has no per-package integration. It reaches a package through its functions, with `eval_elisp`. It finds those functions in Emacs's own documentation with `describe`.

**How does it find out what a package can do?** It looks the package up instead of guessing. `describe` lists a prefix's functions, narrows them by a keyword, shows a function's arguments and documentation, and lists the commands of a buffer's mode. A typical task goes like this:

1. It looks up the functions the task needs.
2. It calls them with `eval_elisp`.
3. It reads the result and corrects course if needed.
4. It answers and names the results it used.

`/learn` can then keep the code that worked, so next time is one call.

**Can it operate on my current buffer?** Yes. `read` and `edit` take a buffer as well as a path, and `read` sees unsaved changes. A conversation started from a buffer knows that buffer as its origin. In any buffer, `M-x superchat-ask` asks about the region and `M-x superchat-rewrite` replaces it.

**Does it run inside Emacs?** Yes. The agent loop runs in your Emacs process, and the model client ships with Superchat. Requests to the model provider go out through `curl`; the `shell` tool runs commands in a subprocess.

## How it works

**Does it do a task in one step?** No. It calls a tool, reads the result and decides the next step. It repeats this until it can answer. At 80 model responses (`superchat-max-rounds`) it looks back over its work and pauses; send a message to go on.

**Can I redirect it while it works?** Yes. Text you send while a run is going reaches it before its next step. `C-c C-k` cancels the run. `C-u C-c C-c` cancels it and sends your text.

**What happens when a tool call fails?** The failure becomes the tool's result, with the error's text, and the model sees it like any other result. A failed request to the model is retried from where it failed. A run that keeps repeating itself is told, once, to change its approach.

**Why does an answer name its sources?** An answer that relies on tool results ends with `Sources:`, naming them. You can see what the answer rests on and check it. `/learn` takes only code that ran and was cited, so the same record decides what can become a skill.

## Trust and control

**Can it change files or run commands without asking?** Not at the default level, `ask`, which asks before every `edit`, `write`, `eval_elisp` and `shell`. `read-only` refuses them all. `auto` allows them all without asking. In the prompt, `y` allows the call once, `n` refuses it and `a` allows that tool for the rest of the conversation.

**Is `auto` safe?** The levels are permission gates, not a sandbox. In `auto` the agent can edit files, run shell commands and Lisp, and set timers without asking. Use it only in projects you trust.

**What happens if I refuse a tool call?** The refusal becomes the call's result, and the run goes on: the model sees the refusal and chooses what to do next.

**Can I undo what it did?** Its edits are made in the file's buffer and then saved, so `undo` in that buffer reverts them. A shell command or a Lisp form cannot be undone this way. For those, use version control.

**Where is my data, and what is sent?** Everything stays in `superchat-directory`: the Tape (`tape.sqlite`), notes and skills. Each request to the model provider carries:

- the system prompt, with the project's `AGENTS.md`, the skills list and recent notes;
- the conversation;
- the tool results it contains.

Nothing goes anywhere else.

**Can I delete what it recorded?** The Tape is append-only: Superchat never deletes or changes an entry, and the database refuses it. To start over, close Emacs and remove `tape.sqlite`. There is no command to delete single entries.

## Memory

**What is the Tape, and how is it different from chat history?** It is one SQLite file holding every conversation, tool call, result, note and timer, in order. A chat buffer shows one view of it. Each request is built from the Tape, not from the last request. That is why a failed request can be retried in place. It also means a new conversation can find old work. The model follows the [Tape](https://tape.systems) data model.

**Does it remember earlier conversations?** Yes, when it looks. `recall` searches all the work on the Tape, and `tape_query` runs read-only SQL over it. The newest notes are in every request, so they need no search.

**What is the difference between `note` and `recall`?** `note` keeps a conclusion, such as "releases go out on Fridays". Later requests in the project carry the newest notes, up to 2,000 characters. `recall` searches the whole record when the model needs something it did not note.

**What does `/dream` do?** It sets a timer for 03:00 every night. At that time the model reviews the project's notes and replaces those that repeat or are wrong. It changes notes only. It also says when a skill needs a look.

## Skills

**What is a skill?** A `SKILL.md` file of recipes: short, titled Lisp forms that worked, with named holes for the values that change. When the values are numbers or strings, `/learn` also writes typed tools in `interface.eld`, so the model can call a recipe with arguments instead of writing the code again.

**Does it really learn, or just save prompts?** It saves code, not a summary of the conversation. Only forms that ran without error and that an answer cited go in, word for word. A value that came from your request becomes a hole, and runs of one procedure share one template. Next time the model calls the tool and skips the search. [learn.md](learn.md) shows three rounds of `/learn calendar-`, file by file.

**Does it learn automatically?** No. A skill changes only when you run `/learn PREFIX`, read the draft and save it with `C-c C-c`. A form you delete from a draft is not offered again.

**When should I use `/learn`?** After a package task worked and you expect to ask something like it again. Name the package's prefix, such as `/learn org-` or `/learn telega-`. Code that only probed is left out.

**Can I edit or delete a skill?** Yes. It is a plain file under `skills/NAME/` in `superchat-directory`. Edit it or remove the directory.

**What happens when a package changes?** The skill records the package's version. `/skills review` lists the skills whose package version has changed and the tools that repeat one another. A model with no tools then lists recipes that overlap. Nothing changes until you press a key.

**Is a skill global or per project?** `/learn` writes to `superchat-directory`, so every project sees the skill. A project can also keep skills in `.superchat/skills/`. A project skill hides a global one of the same name.

**Can skills call other skills?** Yes. Write a `SKILL.md` whose steps name other skills' tools:

```markdown
---
name: year-report
description: Use when asked for a year report.
---
## Year report
1. `load_tools leap`, then call `leap__leap_year_p` with the year.
2. `load_tools weekday`, then call `weekday__new_year_day` with the year.
Answer in one line: `YEAR: leap year yes|no; 1 January is DAY.`
```

Asked "Give me the year report for 2027.", the model reads this skill, loads both skills' tools and calls them in order. In our test with `deepseek-flash` it did so in 3 runs of 3. It is the model that follows these steps; Superchat does not enforce them. `/learn` does not write such a skill: you write it by hand.

**How is a skill different from an MCP tool?** A skill is Lisp that runs in your Emacs, drawn from work that succeeded. Superchat does not support MCP yet.

## Background work

**Can it work while I'm away?** Yes, while Emacs is running. Superchat is not a cloud agent. The model can set a `follow_up` or a `schedule`. When one is due, its work goes into the inbox and a background run takes it up. Run `M-x superchat-timer-start` once to turn timers on.

**What happens when Emacs is closed?** Nothing runs. The inbox is on the Tape, so pending work survives. A timer that came due while Emacs was closed fires once when it starts again.

**Will it ask me questions in the background?** A run nobody watches never blocks Emacs. A call that needs permission becomes a question, and the run waits. `M-x superchat-messages` lists what was sent to you and every question waiting.

**Does closing the chat buffer stop a run?** No. The run goes on, and reopening the conversation shows the rest of it. Text queued in the buffer but not yet sent is lost.

## Models and cost

**Which models can it use?** Models served through the OpenAI Chat Completions API or Anthropic's Messages API. That includes DeepSeek, Claude and the providers listed in [reference.md](reference.md#providers). OpenAI's Responses API and Gemini are not supported yet.

**What does it cost?** Superchat is free. Your model provider bills you for the requests.

**Can I use a local model?** Yes, through any OpenAI-compatible server such as Ollama or LM Studio. With a local model no request leaves your machine. The agent's own `shell` commands and Lisp can still reach the network.

**Can I switch models mid-conversation?** Yes. `/model NAME` sets the model of the current conversation from its next request.

## Alongside other tools

**Should I use it if I already have a coding agent?** If you mainly need a coding agent with its own runtime, Claude Code or Codex may be enough. Superchat is an agent inside Emacs: it works on your live session, keeps its work and turns what worked into tools. You can run one through agent-shell and use Superchat next to it.

**How does it differ from gptel?** gptel is a light, provider-rich client that you extend yourself, and gptel-agent adds tools to it. Superchat's loop runs in Emacs, keeps every step on the Tape and learns skills from cited work. They can live side by side.

**What can't it do yet?** It has no sub-agents, no MCP and no images. Its background work runs only while Emacs is open. Of the providers, only DeepSeek has been tested against the real service.
