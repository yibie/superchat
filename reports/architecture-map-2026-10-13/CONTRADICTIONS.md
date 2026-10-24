# Superchat contradictions — architecture walk

Baseline: HEAD `b4f0548f4851a9cab4eec984427babc41d6ac719`. Product and existing documentation are read-only. Labels: **code fact**, **inference**, **guess**. A user-reported observation is attributed explicitly; it is not represented as a locally reproduced test or a database inspection.

## C1 — A synthesized tool survives its Run, but its callable retains that Run's authority

### The statements that disagree

- **Code fact — model-facing description:** “the tool becomes callable on / your next step, in this same run.” (`superchat-tools.el:1885–1887`). This is the exact HEAD wording: it promises next-step availability, **not explicitly “only this run.”** Reading it as an exclusive lifetime would be an inference.
- **Code fact — implementation docstring:** “When PERSIST is non-nil the tool also / joins the session registry and survives to later turns; sub-agents pass / nil, so their synthesis dies with their prompt.” (`superchat-synthesis.el:198–200`).
- **Code fact — registry docstring:** “Session-scoped `llm-tool' structs the agent wrote for itself.” (`superchat-synthesis.el:55–56`); the names list similarly says “Names of tools synthesized this session, newest first.” (`superchat-synthesis.el:63–64`).
- **Inference:** The three texts describe availability at different horizons rather than forming a strict three-way logical contradiction. The operational contradiction is stronger: the promised later-turn callable retains a creating-Run liveness capability and can refuse after that Run ends.

### Who is right in practice — source trace

1. **Code fact:** Runtime's tool context contains `:run-id` plus `:run-active-p` and interrupt closures over that ID (`superchat-runtime.el:3612–3637`). The Agent wrapper copies this context into a lexical factory (`superchat-agent-loop.el:553–572`). Invocation refreshes only `:context-envelope`, retaining the captured Run identity (`superchat-agent-loop.el:104–119`).
2. **Code fact:** `superchat--syn-define` constructs `wrapped`, then places **that wrapped descriptor** in the global `superchat-llm-synthesized-tools` variable when persistence is enabled (`superchat-synthesis.el:211–227`). Runtime defaults that option to true (`superchat-runtime.el:3990–3993`). The registry is not keyed by conversation Session ID (`superchat-synthesis.el:55`, `superchat-synthesis.el:222–227`).
3. **Code fact:** Later tool collection includes the synthesized registry (`superchat-llm.el:2002`), then request preparation wraps selected tools again (`superchat-llm.el:2781–2809`). Thus the newer request can have an outer wrapper for its current Run around the old wrapper retained as the original function (`superchat-agent-loop.el:517–533`).
4. **Code fact:** The old invocation fails the execution-active gate and returns a normal value, `[Tool call blocked: Run is no longer active]` (`superchat-tool-execution.el:389–434`, `superchat-tool-execution.el:1082–1087`). A synchronous implementation's ordinary return becomes `(cons 'ok value)` and enters finish-success (`superchat-tool-execution.el:895–927`), whose recorded completion uses `:status "succeeded"` (`superchat-tool-execution.el:541–567`).
5. **Inference:** This nested-wrapper path explains both halves of the reported result: an approved outer call returns the inner stale-Run refusal as text, while the outer call records success. It does not imply an authorization bypass or that the inner tool body ran.
6. **Code fact:** The disk store writes definition parts, not the Run closure, and rehydration constructs an unwrapped descriptor (`superchat-synthesis.el:417–437`, `superchat-synthesis.el:455–478`). The stale-wrapper defect is specifically in the in-process retained callable, not serialization of Run capabilities to that source file.

### User-visible cost / observation

- **User-reported evidence, not independently re-read:** On 2026-10-11, a tool defined by `define_tool` was approved and invoked in a later Run of conversation `20261011-153917-c52a`; it returned `[Tool call blocked: Run is no longer active]` while its Tape result was recorded as successful. The steering architect supplied this observation in Round 2. No row IDs were supplied and this walk did not open the live database.
- **Inference, supported by the trace above:** The user can approve a tool advertised as reusable and receive no useful execution; the success fact misleads subsequent model/history consumers about the semantic outcome (`superchat-tool-execution.el:895–927`, `superchat-tool-execution.el:541–567`). The mismatch belongs to lifetime/representation boundaries, not a missing approval prompt.

Status: source path verified; observed outcome supplied by the user; not reproduced in this walk.

## C2 — “Session-scoped” synthesis is actually a process-global registry plus a shared disk store

### The statements that disagree

- **Code fact — declared ownership:** “Session-scoped `llm-tool' structs the agent wrote for itself.” (`superchat-synthesis.el:55–56`); default tool selection promises “Every session-synthesized tool's name” (`superchat-select-tools.el:292`). The product's Session is the conversation container, not the Emacs process (`CONTEXT.md:39–47`).
- **Code fact — actual storage:** `(defvar superchat-llm-synthesized-tools nil)` and the assignment `(cons wrapped ... superchat-llm-synthesized-tools)` contain no Session key (`superchat-synthesis.el:55`, `superchat-synthesis.el:222–227`). Both source and ledger files are under the one configured data directory (`superchat-synthesis.el:339–353`).

### Who is right in practice

- **Code fact:** Selection adds names from this global list and materialization merges the same descriptors without filtering by Session (`superchat-select-tools.el:309`, `superchat-llm.el:1999–2009`). Reset clears the whole list, not one conversation (`superchat-synthesis.el:66–71`). Rehydration is guarded once per Emacs process (`superchat-synthesis.el:481–490`).
- **Inference:** The implementation, not the Session-scoped docstring, determines visibility: another conversation can be offered a name created elsewhere when its selection policy admits synthesis. This is distinct from C1's stale Run capability inside the retained descriptor.

### User-visible cost

**Inference:** A tool can appear outside its creating conversation, an identically named later definition can replace a shared entry, and reset affects every conversation sharing the process (`superchat-synthesis.el:211–227`, `superchat-synthesis.el:66–71`). Cross-conversation behaviour was not exercised here.

## C3 — “Every call asks” conflicts with the permission level that intentionally permits calls without asking

### The statements that disagree

- **Code fact — model/user-facing promises:** “every call asks the / user for confirmation” (`superchat-tools.el:1887–1888`); synthesis's returned success string says “Every call will ask the user for confirmation.” (`superchat-synthesis.el:242–243`); review UI repeats “every call still asks for confirmation” (`superchat-synthesis.el:551–552`). The rehydration docstring also says its guarded function “self-confirms” (`superchat-synthesis.el:455–459`).
- **Code fact — current permission contract:** Synthesized calls are “free only under / `danger-full-access'” (`superchat-synthesis.el:24–26`); the implemented permission decision explicitly requires no prompts for definitions or synthesized invocations under that mode (`docs/decisions/implemented/2026-09-17-permissions-the-dsh-way.md:151`, `docs/decisions/implemented/2026-09-17-permissions-the-dsh-way.md:210`).

### Who is right in practice

**Code fact:** The level stage permits full-control tools in `danger-full-access`, asks under `edit-buffers`, and denies under `read-only` (`superchat-sandbox.el:281–330`). `superchat--syn-guarded-function` only catches body errors; it does not ask (`superchat-synthesis.el:439–453`). The newer level contract therefore wins, not the unconditional confirmation prose.

### User-visible cost

**Inference:** A person/model can incorrectly expect an additional per-call question after selecting a permissive level; the resulting absence of a prompt is intended policy, not an approval bypass (`superchat-sandbox.el:281–330`, `superchat-synthesis.el:439–453`).

## C4 — The synthesis approval ledger claims a reviewed definition even when no definition-specific review occurred

### The statements that disagree

- **Code fact — ledger text:** “Each entry anchors a definition the user reviewed and / approved (or rejected) to its SHA-256.” (`superchat-synthesis.el:381–383`). The nearby design comment says approval occurs “at definition time, in front of the pretty-printed source” (`superchat-synthesis.el:322–325`).
- **Code fact — write condition:** After evaluating/registering a definition, `when persist` writes the source and unconditionally calls `superchat--syn-record-approval ... 'approved`; it does not inspect whether the level required an approval or whether a human saw this source (`superchat-synthesis.el:210–237`).

### Who is right in practice

- **Code fact:** `danger-full-access` permits `define_tool` without asking (`superchat-sandbox.el:260–264`, `superchat-sandbox.el:281–330`). The recorder emits `synthesis.approval.decided` with `:actor-id "user"` and updates the hash ledger (`superchat-synthesis.el:388–407`). Rehydration loads matching approved definitions silently (`superchat-synthesis.el:484–516`).
- **Inference:** The entry can mean “allowed by the current permission level and persisted,” not “this exact definition was reviewed by the human.” A broad authorization is not evidence of source-specific review.

### User-visible cost

**Inference:** Later inspection/restart can present a definition as human-reviewed and silently load it although the actual authorization was only the broader level. The stored hash still checks identity; it does not establish the claimed human-review provenance (`superchat-synthesis.el:355–358`, `superchat-synthesis.el:388–407`, `superchat-synthesis.el:484–516`). No live ledger was read or modified.

## C5 — Keeping the approval ledger out of SQLite does not make it unreachable by tools

### The statements that disagree

- **Code fact — claimed boundary:** “No registered tool writes arbitrary files” (`superchat-synthesis.el:332–334`) and “Kept separate from the store and from the tape database so no / agent-reachable tool can write it.” (`superchat-synthesis.el:346–349`).
- **Code fact — permission model:** Full-control tools include `eval_elisp`, shell commands and `define_tool`; the declaration explicitly says “Lisp can change any state and start any process, and a shell can write” (`superchat-sandbox.el:260–264`). The synthesis header likewise calls synthesized code “ALWAYS full Emacs control” (`superchat-synthesis.el:19`).

### Who is right in practice

**Code fact:** A permitted synthesized body is evaluated as a function definition and invoked with its supplied arguments (`superchat-synthesis.el:210`, `superchat-synthesis.el:439–453`). The separate path is just an ordinary file written with `write-region` (`superchat-synthesis.el:346–353`, `superchat-synthesis.el:378–386`); this is not filesystem isolation. Mode/policy decide whether such full-control execution is allowed (`superchat-sandbox.el:281–330`).

### User-visible cost

**Inference:** Separating files prevents ordinary database writes from editing the ledger, but does not justify trusting it against an authorized arbitrary-Lisp/shell tool. Ledger mutation is possible with sufficient granted authority; this is **not** evidence of escaping a denied permission or of tampering having occurred (`superchat-sandbox.el:260–330`, `superchat-synthesis.el:439–453`).

## C6 — “Allow all / no longer asks” actually selects a level that still asks before saving

### The statements that disagree

- **Code fact — answer API and transcript:** “Allow the open question HANDLE once and stop asking in its conversation.” (`superchat-approval.el:217–218`); the resulting panel says “Allowed; this conversation no longer asks” (`superchat-approval.el:297`). Round 3 also found the same unconditional promise in the Pending answer API and mode help (`superchat-pending.el:417–425`, `superchat-pending.el:478–483`).
- **Code fact — selected level:** The action selects `auto` (`superchat-approval.el:213`, `superchat-approval.el:228–230`), whose description explicitly says “Saving a buffer still asks, and the model can still put a question to you.” (`superchat-sandbox.el:58–61`).

### Who is right in practice

**Code fact:** `auto` combines `danger-full-access` mode with approval policy `ask` (`superchat-sandbox.el:58–61`). The persisting-tool branch still requests approval for `save_buffer` before the ordinary full-control allow branch (`superchat-sandbox.el:266–270`, `superchat-sandbox.el:281–330`). Thus the level description, not the unconditional panel/docstring, states the actual boundary.

### User-visible cost

**Inference:** When saving is explicitly exposed by a tool profile, someone who just selected “allow all” can still receive a save approval question despite the transcript's promise. The intended save exception remains intact; the defect here is the promise, not a proposed change to permissions (`superchat-sandbox.el:266–270`, `superchat-approval.el:288–301`).

## C7 — Subagent synthesis is prompt-local only at the descriptor-store boundary, not the function/name boundary

### The statements that disagree

- **Code fact — promised isolation:** “Sub-agents may synthesize, but only into their own in-flight prompt: / nothing reaches the session registry” (`superchat-synthesis.el:33–34`); “sub-agents pass / nil, so their synthesis dies with their prompt.” (`superchat-synthesis.el:199–200`).
- **Code fact — unconditional writes:** `eval defun-form t` and `push name superchat--syn-names` occur before `when persist` (`superchat-synthesis.el:210–222`). The function symbol is just `superchat-syn--` plus the tool name, with no child, Session or Run key (`superchat-synthesis.el:120–124`).

### Who is right in practice

- **Code fact:** Subagent Requests do pass `:persist-synthesized-tools nil` (`superchat-subagent.el:1170`). This suppresses the shared descriptor/disk branch, not the preceding global function/name writes (`superchat-synthesis.el:210–240`).
- **Code fact:** Redefining an already synthesized name is allowed (`superchat-synthesis.el:106–118`). Existing guarded callables retain a symbol and `apply` its current function definition, rather than retaining an isolated implementation (`superchat-synthesis.el:439–453`).
- **Inference:** An admitted child definition with an existing synthesis name can therefore replace the implementation used by an older parent/sibling descriptor even with persistence disabled. New child-only names also remain in the process-wide synthesis-name classification after the child prompt disappears. This does not assert that a child in a denying permission level can synthesize at all.

### User-visible cost

**Inference:** A later call can execute a changed body behind an unchanged descriptor, or a child-only name can affect later classification/selection. The advertised “dies with its prompt” isolation is not implemented at all state boundaries (`superchat-synthesis.el:73–75`, `superchat-synthesis.el:120–124`, `superchat-synthesis.el:210–227`, `superchat-synthesis.el:439–453`). Not exercised in this walk.

## C8 — The component reference still names the old minibuffer default

### The statements that disagree

- **Code fact — reference text:** “default: `superchat-approval-confirm`, `y`/`n`/`?`” (`docs/components/superchat-tool-execution.md:31`), also described as “the default minibuffer answerer” (`docs/components/superchat-tool-execution.md:102`).
- **Code fact — current defaults:** `superchat-approval-function` defaults to `superchat-approval-ask`, and `superchat-approval-prompt` defaults to `publish` (`superchat-approval.el:44`, `superchat-approval.el:56`).

### Who is right in practice

**Code fact:** `superchat-approval-ask` chooses the minibuffer only when the prompt setting is `minibuffer`; otherwise it publishes (`superchat-approval.el:346–355`). Publication maintains the open-question record and uses a conversation panel or fallback overlay (`superchat-approval.el:265–302`, `superchat-approval.el:329–344`). The implementation/current defcustoms win.

### User-visible cost

**Inference:** Someone following the component reference looks for an immediate minibuffer question rather than the published approval and its answering controls. Batch refusal remains implemented; that part of the old reference is not contradicted (`superchat-approval.el:459–470`).

## C9 — The default transport's capability probe names a nonexistent streaming resolver

### The statements that disagree

- **Code fact — Service contract:** `capabilities MODEL -> (:streaming BOOL :tools BOOL)` (`superchat-service.el:123–129`).
- **Code fact — provider implementation:** The `capabilities` method ignores `_model` and tests/calls `superchat--llm-resolve-streaming` (`superchat-transport-llm.el:30–36`). The implemented resolver is instead `superchat-llm--resolve-streaming (backend model tools-p)` (`superchat-llm.el:378`), which the actual request loop calls (`superchat-llm.el:2838`, `superchat-llm.el:3787`).

### Who is right in practice

**Code fact:** On the repository's stock definitions, the first spelling has only a declaration and references in `superchat-transport-llm.el`, not a definition/alias. Consequently its `fboundp` check is false and the advertised streaming value is nil; even a failing call is swallowed by `ignore-errors` (`superchat-transport-llm.el:14`, `superchat-transport-llm.el:30–36`). The real dispatch uses the correctly named resolver independently (`superchat-llm.el:2838`, `superchat-llm.el:3787`). A user-installed compatibility alias could change the probe's behaviour; none is provided on HEAD.

### User-visible cost

**Inference:** A plugin/consumer relying on the Service capability query can wrongly treat a streaming-capable model as non-streaming. This does not establish that the main chat currently loses streaming: its actual dispatch follows the other path. No downstream user symptom or runtime reproduction is claimed (`superchat-transport-llm.el:30–36`, `superchat-llm.el:2838`).

**Final-summary carry-forward:** C9 is a separate latent defect: the stock model-transport capability query calls the wrong resolver name and advertises streaming as nil, independently of the actual dispatch capability resolution.

## C10 — Changes calls itself record-free but now owns durable notice facts and a checkpoint

### The statements that disagree

- **Code fact — header:** “It records nothing of / its own. The candidates are derived each time they are asked for” (`superchat-changes.el:9–10`).
- **Code fact — current writers:** The module appends `run.ended-unwatched` and `run.seen` itself (`superchat-changes.el:955–965`, `superchat-changes.el:988–996`), and its notice fold maintains a DB projection checkpoint (`superchat-changes.el:904–907`).

### Who is right in practice

**Code fact:** Candidate buffer selection is derived, but the later notice subsystem is not record-free: it records user-Run unwatched endings, records acknowledgement before removing a notice, and rebuilds notices from stored facts (`superchat-changes.el:242`, `superchat-changes.el:790`, `superchat-changes.el:933–1004`). The header describes only the older candidate-list responsibility.

### User-visible cost

**Inference:** Treating this module as a pure disposable view hides that acknowledgement is a write which can fail and leave the notice present, and that its checkpoint is additional persisted read-model state (`superchat-changes.el:754–758`, `superchat-changes.el:978–1004`). The implementation deliberately reports such failure; the contradiction is the ownership description.

## C11 — The no-progress brake tells the user to use a command which refuses its newly waiting record

**Priority carry-forward:** The steering architect verified this defect on HEAD and attributed its introduction to `b3391425` (user confirmation, Round 4). This walk independently checked that commit's subject/file scope, not a fresh interactive reproduction. The HEAD branch/refusal trace below remains the behavioral evidence. Keep C11 near the top of the final ranking.

### The statements that disagree

- **Code fact — brake message:** “/work resume goes on” (`superchat-work.el:1453`).
- **Code fact — command gate:** `/work resume` requires status `continuable`; otherwise it signals “%s is %s; there is nothing to go on with” (`superchat-work.el:1519–1523`). The brake just paused the record and appended a `work.waiting` fact with `for="user"` (`superchat-work.el:1446–1450`, `superchat-work.el:321–336`), which folds to waiting rather than continuable (`superchat-work.el:163–173`).

### Who is right in practice

**Code fact:** The ordinary human-input hook can clear a user wait, but command input is explicitly excluded from that hook (`superchat-dispatcher.el:1485–1492`; `superchat-work.el:1065–1075`). Therefore typing the advertised slash command does not clear the brake's wait first.

### User-visible cost

**Inference:** Following the displayed instruction refuses to resume; sending an ordinary message can instead clear the user wait. No interactive reproduction was performed (`superchat-work.el:1446–1455`, `superchat-work.el:1519–1523`, `superchat-dispatcher.el:1485–1492`).

## C12 — Prepared subagent tools carry a Run ID but omit the executor's Run-liveness capability

### The statements that disagree

- **Code fact — lifecycle contract:** “Exact invocation identity, current liveness ... constrain execution; a tool list cached by the model does not grant execution rights.” This explicitly includes “internal prepared Subagent tools” (`docs/protocols/request-lifecycle.md:72`). “Runtime cancellation prohibits continuation before callbacks can resume model or tool work.” (`docs/protocols/request-lifecycle.md:92`).
- **Code fact — child tool context:** The subagent context constructor carries `:run-id` and `:turn-id` but no `:run-active-p` callback (`superchat-subagent.el:932–952`). It is passed to `superchat-tool-execute` by both the synchronous and asynchronous wrappers (`superchat-subagent.el:954–1045`).

### Who is right in practice

- **Code fact:** `superchat-tool-execution--run-active-p` returns true when the context has no `:run-active-p`, expressly retaining the old non-Runtime-caller behaviour (`superchat-tool-execution.el:389–402`). The pipeline's first-arrival gate calls that predicate (`superchat-tool-execution.el:404–407`, `superchat-tool-execution.el:1082–1087`).
- **Code fact:** Prepared child descriptors are submitted with their own wrapper, avoiding the ordinary Agent wrapper which captures Runtime's effect predicate (`superchat-subagent.el:1156–1170`; `superchat-llm.el:2799–2809`; `superchat-agent-loop.el:553–572`). The general Scope binder restores Scope but adds no liveness check (`superchat-llm.el:1808–1827`). The separate attempt-active wrapper is inside the optional evidence-contract path, not an unconditional protection for ordinary requests (`superchat-llm.el:3879–3881`, `superchat-llm.el:3977–3992`).
- **Inference — latent boundary defect:** A retained ordinary child tool callable invoked after its Run terminates can pass the Run-liveness portion of admission, provided its installation/permission/other gates still allow the call. This is a conditional source trace, not proof that a provider has delivered such a late invocation in practice. Cancellation of the provider request can prevent arrivals; it is not a check inside the retained tool callable.

### User-visible cost

**Inference:** A late child tool invocation could perform a permitted effect after cancellation and append tool facts under a terminal Run, contrary to the advertised effect-inert boundary (`superchat-tool-execution.el:389–407`, `superchat-tool-execution.el:979–1050`). No late provider callback or tool side effect was generated during this walk.

## C13 — Projection cleanup is name-based, not exact-installation-based

### The statements that disagree

- **Code fact — composition contract:** “cleanup remains owned by that exact installation” and duplicate identifiers in one layer are rejected (`docs/design/plugin-architecture.md:17–19`).
- **Code fact — projection registry:** Registering a projection overwrites its global name slot, then installs an undo thunk which calls `superchat-projection-unregister` with only that name; unregister removes whichever descriptor is current (`superchat-projection.el:40–54`).

### Who is right in practice

**Code fact:** Subagent, workflow, workspace and experience rows register projections using this port (`superchat-default-plugins.el:29`, `superchat-default-plugins.el:109–135`). The projection port is a global registry plus plugin undo, not a `superchat-scope-register` entry with an installation token (`superchat-projection.el:21–54`).

**Inference — latent lifetime hazard:** If two live installations register the same projection name, disposing the earlier one removes the later descriptor instead of leaving the still-live installation usable. This is conditional; no duplicate-projection composition was mounted here. It identifies an exception to the stated exact-cleanup contract, not a claim that every built-in startup triggers it.

### User-visible cost

**Inference:** A subsequent projection replay can fail with “Unknown Superchat projection” while a row which registered it remains mounted (`superchat-projection.el:50–59`).

## C14 — The component reference assigns all built-ins to tools-builtin; the row-owned tools survive independently

### The statements that disagree

- **Code fact — component reference:** “Without `tools-builtin` no built-in tool is a selectable descriptor.” (`docs/components/superchat-tool-execution.md:102`).
- **Code fact — current ownership:** `tools-builtin` calls an installer which explicitly excludes tools assigned to other rows (`superchat-tools.el:2485–2494`, `superchat-tools.el:2511–2517`); the row-to-tool inventory and installer give those rows their own scoped descriptors (`superchat-tools.el:2457–2483`). The row's own docstring repeats the obsolete no-built-ins claim (`superchat-tools.el:2513–2514`).

### Who is right in practice

**Code fact:** `work-records` installs its own descriptor (`superchat-work.el:1600–1607`), and memory/subagent/workspace do likewise (`superchat-memory.el:659–665`, `superchat-default-plugins.el:105–111`, `superchat-default-plugins.el:125–132`). Availability is resolved from the visible descriptor registration (`superchat-tools.el:2496–2507`). Memory/subagent/workspace declare a `tools` dependency; `work-records` installs directly with no `:inject` declaration. None depends on the `tools-builtin` row. In contrast, `timers` owns the scheduler, not a descriptor registration (`superchat-timer.el:833–849`); it is not an example of this descriptor split.

### User-visible cost

**Inference:** Someone removing only `tools-builtin` on the reference's assurance can still expose row-owned built-ins. Conversely, removing the owning row is the mechanism that removes a work-record/subagent/workspace/memory descriptor; selection still respects the request's own restrictions (`superchat-select-tools.el:83–94`, `superchat-tools.el:2496–2507`).

## C15 — The lifecycle contract still puts attachment recording in a removed sealing observer

### The statements that disagree

- **Code fact — lifecycle contract:** “The `chat-turn-recorder` plugin declares its `record-user-message` and `record-attachments` observers required at registration” (`docs/protocols/request-lifecycle.md:98`).
- **Code fact — current row:** “Attachment facts are not recorded here: Runtime writes them when it admits the request” (`superchat.el:5402–5404`); the row registers only `record-user-message` (`superchat.el:5406–5407`).

### Who is right in practice

**Code fact:** Runtime appends `attachment.added` during submission beside the admitted intent (`superchat-runtime.el:3547–3575`); the current row declaration and Runtime writer, not the old two-observer sentence, describe ownership.

### User-visible cost

**Inference:** Unmounting the chat recorder no longer disables attachment recording, and a failure is an admission/startup boundary failure rather than the missing sealing observer described by the stale text. This preserves evidence for attachments but changes where one must diagnose a failed request (`superchat-runtime.el:3547–3575`; `superchat.el:5400–5407`).

## C16 — Workspace project-finder cleanup still removes a shared hook on the first unmount

### The statements that disagree

- **Code fact — accepted lifetime rule:** “共享的资源由第一份安装建立，最后一份撤掉时才拆” (the first installation acquires a shared resource; only the last removal tears it down) (`docs/decisions/accepted/2026-10-11-a-shared-resource-outlives-one-installation.md:18`). That implementation explicitly repaired changes/timers/work-records; this is a further row, not a claim that those three repairs failed.
- **Code fact — another shared resource:** `workspace-resolution` adds the same global project finder for each installation and registers an unconditional per-Scope removal (`superchat-workspace.el:341–349`, `superchat-workspace.el:365–373`). It does not use the shared-effect counter.

### Who is right in practice

**Code fact:** The hook helpers call ordinary `add-hook` / `remove-hook` on `project-find-functions` with the same function (`superchat-workspace.el:341–349`). **Inference:** with two live installations, the hook is installed once but removing either installation removes it for both, contrary to the shared lifetime rule. No second installation was mounted in this walk.

### User-visible cost

**Inference:** Emacs' project lookup can stop recognizing a `.superchat/` workspace through this finder while another workspace-resolution row is still active (`superchat-workspace.el:326–349`, `superchat-workspace.el:365–373`).

## Round 3 checks on earlier review findings — not current contradictions

- **Code fact — settlement scan:** The ordinary Run-end observer calls historical `superchat-work-settle-missing` only when that Session has an entry in `superchat-work--owed` (`superchat-work.el:1577–1579`). The catch-up path reads the recorded outcomes once into a hash table and checks it before settling missing endings (`superchat-work.el:1162–1200`). **Inference:** the reported repeated all-ended-Runs scan is no longer the ordinary path; this is not a claim that every exceptional backlog operation is linear, nor a benchmark of all Work/round folds. Git confirms `981719f4` is an ancestor of this HEAD; behaviour claims here refer to HEAD, not that historical patch.
- **Code fact — wake retries:** `superchat-work--try-wake` reads the previous count/time before dispatch, refuses once the count exceeds the configured retry limit, and enforces the remaining delay (`superchat-work.el:881–898`). The historical “count never read before attempting” finding is false on HEAD. A new Emacs gets another budget by design (`superchat-work.el:863–865`).
- **Code fact — child Run attribution:** Pending tests `:parent-run-id` first and says `started by execution %s`; only the final fallback says `started by you` (`superchat-pending.el:125–147`). The reported child attribution defect is fixed on HEAD. Goal rounds also have their own label in this branch.

## C17 — Telega compatibility “approve” promises a preview even when the Session level never asks

### The statements that disagree

- **Code fact — promise:** `approve` means “compose and send through the audited tool after the user approves the full draft once” (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:81–87`); the action guidance repeats “The user approves the complete draft before it is sent” (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:460–466`). The command docstring repeats approval of the full draft (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:701–707`).
- **Code fact — actual gate:** The preview is an `approval/request` listener, not an unconditional part of sending (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:650–658`, `plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:734–740`). The Session level allows outside tools directly under danger-full-access; `auto` uses that sandbox mode with ask retained for exceptions (`superchat-sandbox.el:58–61`, `superchat-sandbox.el:304–330`).

### Who is right in practice

**Code fact:** Compatibility Requests select the preset/tool surface but do not pin an always-ask policy (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:540–601`). Send validates the text/chat then invokes the Telega send API; it has no second preview inside its body (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:110–131`). **Inference:** at a permissive Session/default level, an `approve` action can send without reaching the preview listener. This is authorized by the global permission decision, not a bypass of that decision; the contradiction is the stronger action-specific promise. No message was sent in this walk.

### User-visible cost

**Inference:** A user relying on the command's `approve` mode can lose the promised last look before an external message is sent. This finding is about the compatibility action, **not** the newer contextual Telega Session, whose tool ceiling excludes send/rule writes (`plugins/superchat-cowork-telega/superchat-cowork-telega-session.el:333–348`, `plugins/superchat-cowork-telega/superchat-cowork-telega-session.el:424–431`).

## C18 — “Killing a View does not cancel its Run” needs the Preset-owner exception

### The statements that disagree

- **Code fact — blanket comment:** “Killing a View does not cancel its Run” (`superchat.el:3960–3967`). The chat Sink deliberately declares no live-p and documents durable completion without declaring the Sink dead (`superchat-render.el:1617–1626`).
- **Code fact — owner contract/code:** A killed Preset-owning View retires Runtime execution before its Scope is withdrawn (`superchat.el:5108–5117`); retirement calls `superchat-runtime-retire-scope` (`superchat.el:2014–2045`).

### Who is right in practice

**Code fact:** Plain/global Runs and Preset-owned Runs differ: mode disable and exact View kill/major-mode change release the Preset owner, while no active Preset makes that release a no-op (`superchat.el:2014–2066`, `superchat.el:5134–5139`). **Inference:** the unconditional comment is too broad; this is not a defect in the explicit ownership teardown and does not mean merely hiding a window cancels anything.

### User-visible cost

**Inference:** Applying the plain-View orphaning rule to a Preset Run can unexpectedly stop its work. The Surface table must state this exception instead of promising every closed-View Run will finish (`superchat-runtime.el:3005–3043`, `superchat.el:2014–2066`). No lifecycle was exercised here.

## C19 — Pending is still advertised as writing nothing

### The statements that disagree

- **Code fact — current README:** “It only reads; nothing is queued or written” (`README.md:162`); Chinese repeats “它只读，不排队、不写任何东西” (`README_cn.md:330`).
- **Code fact — current commands/read path:** Pending's `d` action calls the durable notice acknowledgement (`superchat-pending.el:393–405`), and answering approvals calls the approval API (`superchat-pending.el:414–449`). Even refresh invokes Changes' notice reload (`superchat-pending.el:333–344`), whose fold maintains a persisted checkpoint (`superchat-changes.el:904–907`).

### Who is right in practice

**Code fact:** Acknowledgement appends `run.seen` before removing the notice (`superchat-changes.el:977–1004`); approval answers eventually append `approval.decided` (`superchat-approval.el:649–659`). The old read-only Pending decision predates these accepted additions; its historical restriction is not itself a new implementation bug. The README's present-tense blanket sentence is stale.

### User-visible cost

**Inference:** Opening/refreshing is not guaranteed storage-free, and `d` is a durable acknowledgement rather than merely hiding a row in this Emacs. Scripts/users cannot rely on the advertised no-write guarantee (`superchat-pending.el:333–344`, `superchat-changes.el:904–907`, `superchat-changes.el:977–1004`).

## C20 — Emacsclient cancellation reports cancelled instead of the actual terminal state

### The statements that disagree

- **Code fact — returned JSON:** After a truthy Runtime cancellation result, the adapter returns `"status" "cancelled"` without rereading status (`superchat-emacsclient.el:194–230`).
- **Code fact — Runtime meaning:** Cancel returns non-nil both after attempting cancellation and after stopping input control on an already terminal Run (`superchat-runtime.el:3090–3123`). Open unsettled work can change the requested cancelled terminal to `audit-incomplete` (`superchat-runtime.el:2670–2688`; `docs/protocols/request-lifecycle.md:96`).

### Who is right in practice

**Code fact:** The authoritative status accessor copies the Runtime status (`superchat-emacsclient.el:103–130`); the cancellation reply instead describes the requested action. **Inference:** for an audit-incomplete cancellation, or stopping input control on an already completed Run, those JSON APIs can disagree. This is a source-established conditional path, not an emacsclient reproduction.

### User-visible cost

**Inference:** A client treating the cancel reply as terminal truth can misclassify an incomplete audit or overwrite its knowledge of a previously completed outcome. It must query actual status rather than trust that hard-coded field (`superchat-emacsclient.el:103–130`, `superchat-emacsclient.el:194–230`).

## C21 — README simultaneously says fixed Stages are retired and still shipped

### The statements that disagree

- **Code fact — current path:** “The fixed Stage registry is retired” (`README.md:342`, `README_cn.md:347`; `docs/protocols/request-lifecycle.md:19–27`).
- **Code fact — opposite status:** “Fixed Stages are still present in the current implementation; their accepted replacement is not shipped functionality” (`README.md:500`; Chinese equivalent `README_cn.md:492`). The feature/current-status descriptions retain fixed Stages too (`README.md:21`, `README.md:505`).

### Who is right in practice

**Code fact:** Core directly prepares the Turn, selects tools, emits collect-context and assembles the prompt; default rows declare capabilities rather than install a Stage registry (`superchat-core.el:252–265`, `superchat-pipeline.el:508–587`, `superchat-select-tools.el:459–463`, `superchat-prompt.el:356–362`). The retired description is right.

### User-visible cost

**Inference:** Plugin authors can target a removed extension mechanism or incorrectly defer a migration already delivered. This is documentation/version-status drift, not evidence of a runtime failure.

## C22 — Tool inventory documentation and “nil means no built-ins” lag row-owned tools

### The statements that disagree

- **Code fact — documented inventory:** The tools component says “nine by default” and lists those names (`docs/components/superchat-tools.md:98`); README enumerates the same nine (`README.md:384`).
- **Code fact — inventory now:** The default constant contains thirteen names, including the three timer operations and work_record (`superchat-tools.el:1501–1515`). Its customization docstring still says “Set to nil to disable built-in tools” (`superchat-tools.el:1555–1558`), while the selection helper explicitly says row-offered tools “are not suppressed by superchat-llm-tool-names” (`superchat-select-tools.el:171–176`).

### Who is right in practice

**Code fact:** Candidate construction appends visible row-offered names, and work-records is such a row (`superchat-select-tools.el:171–184`, `superchat-tools.el:1574–1586`). Explicit request/Preset ceilings remain separate restrictions; this is not an escape from an explicit tools-none Request. README's Work section already describes the deliberate always-offered Work tool (`README.md:163`).

### User-visible cost

**Inference:** A user setting only the global name list to nil can still expose work_record while its row is active. Documentation of the default surface also understates it. Related but not identical to C14, which concerns removing tools-builtin rather than setting the name list.

## C23 — Sink-only visible-output wording excludes real transport-side presentation

### The statements that disagree

- **Code fact — exclusivity claim:** “Visible output goes only to the envelope's Sink” (`README.md:346–348`; same claim `README_cn.md:349–350`). The lifecycle also prohibits replacing the accepted Sink via compatibility routing (`docs/protocols/request-lifecycle.md:7–9`).
- **Code fact — extra presentation path:** The native adapter directly reenters its captured request buffer to report TTFT before calling the stream Sink (`superchat-llm.el:3093–3107`); the reporting helper directly inserts an assistant annotation (`superchat-llm.el:4471–4491`). It also closes response regions on continuations via a captured View (`superchat-llm.el:3494–3499`, `superchat-llm.el:3582–3586`).

### Who is right in practice

**Code fact:** Text/reasoning chunks and terminal delivery use the Sink, but all presentation is not exclusively Sink-mediated. The direct presentation is guarded by buffer liveness and relevant display state. **Inference:** the README needs either a narrower output definition or an explicit transport-presentation exception. This does not establish replacement of the accepted terminal Sink or any recording/permission bypass.

### User-visible cost

**Inference:** Supplying a custom Sink alone is not a proof that a producer has suppressed all View-side telemetry/presentation. A nonvisual integration also has to avoid/disable the retained View presentation path (`superchat-llm.el:3093–3107`, `superchat-llm.el:4471–4491`). No custom Sink was exercised here.

## C24 — “Whichever conversation is visible, wherever the file is” is not current in-place routing

### The statements that disagree

- **Code fact — public promise:** Inline ask/act/rewrite join the visible/main conversation “wherever the file is” (`README.md:166`). The inline Session helper's docstring repeats “the chat beside the code, else the main chat window” (`superchat-inline.el:96–104`).
- **Code fact — actual authority:** The delegated resolver explicitly says a project source uses “that Workspace's main conversation instead” and a non-chat source outside a project uses the temporary conversation (`superchat.el:3708–3717`); those branches precede the visible/main fallback (`superchat.el:3718–3736`).

### Who is right in practice

**Code fact:** `superchat-inline-session-id` calls that resolver in the source buffer. From project B, it chooses B's main conversation and can switch an existing side window away from another conversation without selecting it; it need not use the conversation the user was seeing (`superchat-inline.el:102–104`, `superchat.el:3718–3729`). The entry matrix's source-relative Session description is compatible with this implementation (`docs/architecture.md:117–119`).

### User-visible cost

**Inference:** A user expecting the visible conversation to receive the in-place Turn can instead get another Workspace conversation and a retargeted side window. The code preserves Workspace routing; the problem is the old universal visible-conversation promise, not a demonstrated cross-Workspace data leak. No windows were manipulated in this walk.

## C25 — “eval_elisp never asks” omits a deliberately retained key-input exception

### The statements that disagree

- **Code fact — public promise:** “Code run through eval_elisp never asks you anything” and “a question, a minibuffer, a key read” stops there (`README.md:405`). The function opens with the same “never asks” statement (`superchat-tools.el:976–981`).
- **Code fact — explicit boundary:** The function immediately excludes read-key-sequence (`superchat-tools.el:982–986`). The accepted ruling states that read-key-sequence, read-key and describe-key can still reach the user, and deferred timers are outside the binding; it calls this a narrowing, not an equivalent of the original promise (`docs/decisions/accepted/2026-09-24-eval-elisp-never-asks-the-user.md:19–25`). The component reference includes the narrower boundary (`docs/components/superchat-tools.md:123–125`).

### Who is right in practice

**Code fact:** The tool dynamically binds inhibit-interaction and uses signal-hook-function to escape inhibited-interaction, rather than replacing every Emacs key-input primitive (`superchat-tools.el:996–1014`). **Inference:** the narrower documented implementation boundary is the usable contract; the README's categorical formulation is not. This is an already accepted limitation, not a newly discovered security bypass. No interactive expression was evaluated.

### User-visible cost

**Inference:** An approved model expression can still ask for a key despite the broad promise. A consumer cannot treat eval_elisp as universally noninteractive, particularly when planning unattended work (`superchat-tools.el:982–986`; `docs/decisions/accepted/2026-09-24-eval-elisp-never-asks-the-user.md:19`).
