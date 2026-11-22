# Coming from Superchat 1.x

Superchat 2 is a rewrite. It keeps the idea (an AI agent inside Emacs that remembers) and changes how it is built:

- **Much smaller.** About 6,500 lines of Emacs Lisp in 21 files, plus a 10,300-line model client in `bridge/`, against about 125,000 lines in 247 files. 11 options against 161.
- **One design, checked.** Six layers, each using only the public functions of those below, with gates that fail the build when a rule is broken. Every rule change has a decision note.
- **An agent that grows.** Answers name the calls they rest on, `/learn` turns that cited code into skills with typed tools, and long runs are kept on track by retries, a look-back at the round limit and a loop guard.

No measured comparison of speed between the two versions has been made.

## Moving over

1. **Replace the recipe.** Superchat 2 needs no `llm.el`; its model client ships in the repository.

   ```elisp
   (elpaca (superchat :repo "yibie/superchat"
                      :main "superchat.el"
                      :files (:defaults "bridge/lisp/*.el")))
   ```

2. **Replace the backend setting.** Name a provider and a model, and put the key in the environment (the error for a missing key names the variable):

   ```elisp
   ;; 1.x
   (setq superchat-llm-backend (make-llm-openai :key ... :chat-model "gpt-4o-mini"))
   ;; 2
   (setq superchat-llm-provider "deepseek"
         superchat-llm-model "deepseek-flash")
   ```

   A local server (Ollama, LM Studio) is one entry in `superchat-llm-providers`; see [reference](reference.md#providers).

3. **Rename or drop the other settings:**

   | 1.x | 2 |
   |---|---|
   | `superchat-data-directory` | `superchat-directory` |
   | `superchat-permission-default-level` | `superchat-default-level` |
   | `superchat-llm-max-tool-rounds` | `superchat-max-rounds` |
   | `superchat-context-max-chars` | `superchat-context-chars` |
   | `superchat-llm-model` (inside the backend) | `superchat-llm-model` (a model id) |
   | `superchat-plugins` | `superchat-plugins`, with a new plugin API ([api](api.md)) |
   | `superchat-llm-streaming`, retry and timeout options, `superchat-profiles`, `superchat-mcp-*`, `superchat-subagent-*`, the `superchat-fix-*`, `superchat-watch-*` and `superchat-annotate-*` options | remove: no counterpart |

4. **Old conversations stay where they were.** The new record, the Tape, starts empty in `superchat-directory`; nothing is imported from the 1.x database.

## What you type now

| 1.x | 2 |
|---|---|
| `@gpt-4o …` for one message | `/model NAME`, for the conversation |
| `#~/src/main.py …` | name the file; the model reads it |
| `>code-review …` | the model picks skills from their descriptions |
| `/permissions` | `/level read-only`, `ask` or `auto` |
| `M-x superchat-inline-ask` | `M-x superchat-ask` |
| `M-x superchat-rewrite-region` | `M-x superchat-rewrite` |
| `M-x superchat-butler`, `M-x superchat-pending` | `M-x superchat-messages`, `M-x superchat-timers` |

## Not in Superchat 2

These 1.x features have no counterpart yet: MCP servers, sub-agents, profiles, the review screen for edits before saving, `superchat-annotate` and `superchat-note`, compile watching with sandboxed fixes, the Telega cowork mode, generated UI, and Org Babel blocks. Edits in 2 are saved by the tool, under the permission level, with undo in the buffer.

## Similar and different

| | 1.x | 2 |
|---|---|---|
| Lives in Emacs, works on your buffers | yes | yes |
| Memory in SQLite | yes, with FTS5 | yes, one append-only record, the Tape |
| Skills as `SKILL.md` | yes | yes, plus skills learned from cited code, with typed tools |
| Work while you are away | yes (butler, watches) | yes (timers, inbox, background runs) |
| Permission levels | yes | yes |
| Model access | any `llm.el` backend | OpenAI-compatible and Anthropic, built in |
| Answers name their sources | no | yes |
| MCP, sub-agents | yes | no |
| Size | about 125,000 lines | about 17,000, with the model client |
