# Architecture

This is a map of the running implementation, not the target architecture. Read the [remaining plan](roadmap.md) for accepted changes and proposed designs, the [glossary](../CONTEXT.md) for domain language, and the [plugin guide](plugin-guide.md) to add an extension.

## Composition and ownership

Superchat is one Emacs package. Plugins contribute through Scope-accounted registrations; Service consumers call a named interface rather than selecting a provider implementation. A logical module or plugin is not necessarily a separate file.

| Responsibility | Source | What it does not own |
|---|---|---|
| Composition primitives | `superchat-plugin.el`, `superchat-service.el`, `superchat-event.el` (Event substrate, waterfalls and the fault ring), `superchat-invariant.el`, `superchat-sink.el` | Model execution, editor-resource ownership, durable Session state |
| Envelope validation and driver entry | `superchat-pipeline.el` | A second Run lifecycle; this file also defines the product `driver` plugin |
| Request preparation and chat routing | `superchat-core.el`, `superchat-parser.el`, `superchat-dispatcher.el`, `superchat-preset.el` | Runtime completion |
| Run/Turn lifecycle | `superchat-runtime.el` | Owning or closing borrowed editor buffers |
| Model transport and tool-loop instrumentation | `superchat-llm.el`, `superchat-models.el`, `superchat-agent-loop.el` | Public Run status or an alternative tool authorization decision |
| Tool selection, execution and permission | `superchat-select-tools.el`, `superchat-tools.el`, `superchat-tool-execution.el` (the execution pipeline), `superchat-sandbox.el` (permission levels), `superchat-approval.el` (approval) | Widening a permission level through child Scope shadowing |
| Prompt construction | `superchat-prompt-hooks.el`, `superchat-prompt.el` | Mutating Tape history |
| Durable facts and read models | `superchat-tape.el`, `superchat-db.el`, `superchat-projection.el`, `superchat-tape-view.el` | Treating a Projection or selected view as mutable source history |
| Workspaces, their rules, Work Records and timers | `superchat-workspace.el`, `superchat-work.el`, `superchat-timer.el`; binding and routing of conversations in `superchat.el` | Accepting or finishing a Run; confining what `eval_elisp` or a shell can reach |
| Chat View and live editor resources | `superchat.el`, `superchat-render.el`, `superchat-editor-workspace.el` | Inferring ownership from a buffer name, project or selected window |

Default plugin definitions live next to their implementations; `superchat-default-plugins.el` supplies lazy definitions. `superchat-plugins`, defined in `superchat-plugin.el`, is mounted at the end of `superchat.el`. Tools and entry points can be contributed by a shared plugin; feature names are not an inventory of independently removable plugins. Services have global providers, while contributions such as tools, commands and Event listeners have scoped visibility. The [composition contract](design/plugin-architecture.md) defines these distinctions.

The large UI, dispatcher, Runtime and LLM modules retain direct dependencies. The Kernel vocabulary and product code are also co-located in `superchat-pipeline.el`. A table calling all of these files a product-free Kernel would misdescribe the implementation.

## One request through the system

Public model producers submit an Envelope containing input, a declared Surface and an exact Sink through the `driver` Service. The driver adapts that Envelope to Runtime's internal Request; Runtime alone accepts and finishes execution.

1. **Prepare surface input.** Chat parses `@model`, `>skill`, `/command` and `#file` with `superchat-core-prepare-turn` over a preparation Turn. The dispatcher handles local commands and routing before a model Envelope exists. The preparation Turn and Runtime's execution Turn share a struct type, not a lifecycle owner.
2. **Run the driver preparation sequence.** `superchat-pipeline-submit` validates the Envelope, resolves the Session, admits the submission through Runtime, merges core state, selects tools, collects context through the `collect-context` Event and assembles the prompt. Admission may process, queue, steer, drop or handle the input without starting a new Run.
3. **Accept execution.** Runtime receives the prepared Request and borrows the caller's execution Scope. The `model-transport` provider runs model attempts and the tool loop. Tools cross the one execution pipeline, including asynchronous and delegated tool calls; the Session's permission level decides each call and the approval seam asks when it must.
4. **Persist and deliver.** Runtime uses its per-Run accounting of unsettled Effects and undecided Approvals when sealing, records Run/Turn facts and delivers through the originally accepted Sink. Required recording failures retain their audit-incomplete handling. Runtime quiesces execution; a late fact cannot reopen its terminal state. Consumers use the recorded Run terminal under the [request lifecycle contract](protocols/request-lifecycle.md).

The fixed Stage registry is retired: the eight names it declared (`parse`, `resolve-session`, `admit`, `load-state`, `select-tools`, `collect-context`, `assemble-prompt`, `save-state`) are no longer a registration or lookup surface. `parse` is the chat surface's own direct call; Session identity, state merging, admission, tool selection and prompt assembly are core driver calls; `collect-context` is an Event whose listeners return fragment lists. Extension now happens through Events and the two ordered-await waterfalls (`pre-step`, `before-provider-request`); calling the model, sealing a Run and delivering to a Sink were never Stages. The [request lifecycle contract](protocols/request-lifecycle.md) covers admission, queues, cancellation and failure handling.

## Prompt and history

`collect-context` listeners return labelled fragments. The pure `superchat-prompt-assemble` orders those fragments and returns `(:system :messages :receipt)`. Hard fragments must fit; soft fragments can be refit, truncated or omitted. A plugin adds or withdraws fragments through its own Scope registration, not by adding another driver.

The chat and inline-ask history collector uses a Tape-backed Context View with source exclusions and a remaining-budget refit. Other producers, including skills, workflows, subagents and compaction, can supply prepared text through their collector. The chat View also maintains process-local conversation state; that state is not permission to overwrite Tape. Anchor-backed compaction changes the selected View and preserves the original Entries.

The assembly receipt records selected fragment identities and budget decisions. Before each provider attempt `superchat--llm-record-request-evidence` appends a `request.header` (the system text and the tool schemas as the provider layer is about to send them) and a `request.message-list` (the messages as storage references, including material a Step inserted during the Run). The `prompt-reconstructable` Invariant runs after both rows exist and rebuilds the request from them; it fails when the receipt, the header reference or a named storage id does not resolve. It does not compare the rebuilt request with the bytes the provider received, and a request whose evidence the Tape refused is sent anyway: recording evidence never stops the request it describes.

## State and lifetimes

Session identity, execution identity, registration visibility and buffer ownership have different owners.

| State | Owner and lifetime |
|---|---|
| Tape Entries | Durable append-only facts; DB provides physical storage |
| Projection / Tape View | Rebuildable interpretations and selections of those facts |
| Session EditorWorkspace | Exact Session-owned buffer objects; its close protocol decides which buffers may close |
| Active Preset Scope | One exact Chat View activation; same-named Presets in separate Views are distinct |
| Run state | Runtime, from accepted Request through terminal handling and quiesce |
| Queued input | Runtime's in-process FIFO; a queued item retains its original Sink and borrowed Scope |
| Workspace binding of a Session | `workspace.bound` / `workspace.unbound` facts on that Session's Tape; read through `superchat-view-session-configuration` |
| Work Records and timers | Facts on the Tape (`work.*`, `timer.*`); the lists are rebuilt from them |
| Armed wake-ups and reminders | Emacs timers held in process memory (`superchat-work--armed`, `superchat-timer--armed`); rebuilt from the Tape when the `work-records` and `timers` rows are mounted |
| Which Run a woken record started | Process memory only (`superchat-work--woken-runs`); not on the Tape, so it does not survive a restart |

Runtime borrows execution Scopes; it creates no automatic Run Scope. There is no Session Scope level. The Preset owner calls Runtime retirement before disposing registrations on switch or View deactivation. Generic Scope disposal is not an independent Run cancellation or buffer-close authority. See the [Preset lifecycle](design/plugin-architecture.md#preset-composition-and-lifetime) and [Session close protocol](protocols/session-editor-workspace-close.md).

## Workspaces, Work Records and timed work

A Workspace is where a conversation belongs. `superchat-workspace-of-source` decides it from the buffer a conversation is started from: a buffer inside an Emacs project belongs to that project's Workspace; a buffer in one of `superchat-personal-workspace-modes` (agenda, calendar, feeds, chat, mail) belongs to the Personal Workspace when `superchat-personal-workspace-directory` is set; anything else is a temporary conversation with no directory. The binding is recorded on the Session's Tape as `workspace.bound`, and later readers take the root from that fact, not from the buffer that happens to be current.

What a Workspace scopes today, and what it does not:

| Path | Scope in the code |
|---|---|
| Rules (`AGENTS.md`, else `CLAUDE.md`, read from disk at collection time, at most 6000 characters) | The bound Workspace's root. One collector (`superchat-prompt-collector--workspace-rules`) serves the `chat`, `inline-ask` and `inline-act` surfaces, which also get the note about remembering, and the `inline-rewrite` and `workflow` surfaces, which get the rules alone; a delegated sub-agent gets the same rules appended to its system text. A rules file that exists and cannot be read is not treated as absent (`superchat-workspace-rules-problem`): the user gets a message, and on the three chat surfaces the request tells the model the rules are missing ([decision](decisions/accepted/2026-10-06-rules-on-rewrite-and-workflow.md)) |
| Automatic recall and the model's `memory_search` | The conversations of the same Workspace (`superchat-view-recall-session-ids`), narrowed further by `superchat-memory-auto-recall-cross-session`; a temporary conversation searches only itself |
| The `[Work records]` block | Records of the same Workspace; a temporary conversation has none |
| The model's `memory_search`, `tool_history`, `file_history`, `recent_errors` | The conversation of the Run that made the call (`superchat-tools--history-session`), not the buffer current when the tool runs; `memory_search` widens that to the Workspace as above, the other three stay in the one conversation. A call that belongs to no conversation searches nothing |
| The model's `sql` tool | **Not scoped, and not offered.** It reads every Session's rows, so nothing Superchat ships offers it: it is in no built-in Preset and is not part of `superchat-llm-tool-names` `all`. It exists only where the user's own list names it ([decision](decisions/accepted/2026-10-05-sql-leaves-the-ordinary-tool-surface.md)) |
| The user's `/recall` | Every conversation by default (`superchat-memory-recall-scope`); this is the user's own search, not the model's |
| `eval_elisp`, shells, agent-written tools | Not confined by a Workspace. They are the `full-control` class and are governed by the permission level alone |

A Work Record is the record of a piece of work that outlives one Run. The model registers it through the `work_record` tool (start, update, wait, done, read); `superchat-work.el` keeps it as `work.*` facts and decides the outcome of each execution from facts at `turn-completed`, without reading the answer. A record that is neither concluded nor waiting is continuable and waits for the user; nothing takes it up by itself.

Two things start a Run without the user typing:

- **A task timer** (`timer_create` with kind `task`). `superchat-timer-fire` appends `timer.fired` to the Workspace's most recently active conversation, tells the user, and then starts the task there: `timer.sending` is appended, the timer's text is sent, and `timer.started` names the Run that began. While an execution is running in that conversation nothing is sent and the task waits.
- **A record's wait for a time.** `superchat-work-wake` opens a new conversation bound to the record's Workspace, appends `work.woken` (naming the Workspace's main conversation), clears the wait, makes the record that conversation's current record and sends the wake message. When sending does not finish, the wait is put back. When the first Run of that conversation ends, one line is left in the main conversation.

Both go through the chat View's own send (`superchat-send-input` with `superchat-automated-input` bound), so Runtime remains the only thing that accepts the Run. Both write their "fired" fact **before** they send. That order means one occurrence never starts twice, including after a crash. What follows a send that does not happen differs between the two:

- **Work Record** (`scripts/work-wake-fault-regression.el`, 14 tests). A wake counts only when a Run started in its conversation: `superchat-work--due-p` reads `run.started` on that conversation's Tape. When the send signals, the wait is put back at once and the wake is tried again after `superchat-work-wake-retry-seconds` (300), at most `superchat-work-wake-retry-limit` (3) times in one Emacs. When Emacs exited between the facts and the send, the next scan (`superchat-work-wake-due`, run when the `work-records` row mounts) sees a wake whose conversation never ran and to whose record nothing was written since, and puts the wait back; the record is then taken up in a further new conversation. A record written to after the cut-off wake is left alone. One record's wake failing does not stop the scan.
- **Timer** (`scripts/timer-start-fault-regression.el`, 12 tests). A task counts as started only when `timer.started` is on the Tape. A fired task without it (`superchat-timer-unstarted-tasks`) is started later by `superchat-timer-retry-tasks`: after `superchat-timer-retry-seconds` (300) when the send signalled, at most `superchat-timer-retry-limit` (3) times in one Emacs; after `superchat-timer-busy-retry-seconds` (30), without limit, when the conversation was busy; and first thing in `superchat-timer-catch-up` after a restart. The occurrence keeps its single `timer.fired`. One case is not started again: `timer.sending` is on record, `timer.started` is not, and a Run started in that conversation afterwards. That Run may be the task, so `timer.unconfirmed` is appended and the conversation is told that Superchat cannot tell. A fired task that has not started can be cancelled. Only a `timer.fired` carrying `awaits-start` takes part, so occurrences fired before these facts existed are left alone. The retry counts are in memory.
- The pairing of a woken record with its Run is read from the Tape: the `work.woken` fact sits in the woken conversation and names the main conversation, and a `work.reported` fact marks that the line was written. It holds across a restart. A Run that an earlier Emacs started and never finished is found by the scan (`superchat-work--report-cut-off`): the record gets the outcome `interrupted`, the main conversation is told once that the execution was cut off and was not started again, and the model later sees that on the record. When that Run did end and only the line was missing, just the line is written. Nothing is started again in either case.
- A Run that was interrupted with an external Effect unresolved ends `audit-incomplete` under the [request lifecycle contract](protocols/request-lifecycle.md); neither a timer nor a Work Record retries it.

A woken record's new conversation has no recorded permission level, so it runs under `superchat-sandbox-default-mode`. Under the default `edit-buffers` level its edits stay in buffers and each `full-control` call asks for approval, with nobody necessarily present to answer. Under `danger-full-access` a `full-control` tool can write to disk without asking. "A timed Run never reaches the disk" is therefore not a guarantee of the code; what holds is that `edit_buffer` never saves and that `save_buffer` asks on every call.

## Entry matrix

Each row is a producer that submits an Envelope through the `driver` Service. "Rules" means the Workspace rules described above. The permission level of a Session is its last recorded `sandbox.mode`, else `superchat-sandbox-default-mode`; a producer with `:tools 'none` offers the model no tool, so no level is consulted.

| Producer (surface) | Session and Workspace | Rules | Permission level | History and Work Records |
|---|---|---|---|---|
| Chat window, `superchat-act` (`chat`) | The View's Session. A View opened from a buffer is its Workspace's main conversation (`superchat-conversation-session-id`), else a temporary one | The `workspace-rules` collector, from the Session's `workspace.bound` root, plus the note about remembering | The Session's | Tape Context View of the Session; recall inside the Workspace; the `[Work records]` block |
| In-place ask and act (`inline-ask`, `inline-act`) | `superchat-inline-session-id`: the same conversation the chat window would show for the source buffer | Same collector as chat | That Session's | No earlier turn of the conversation; a follow-up in the same overlay carries that overlay's own earlier Runs. No recall. The `[Work records]` block |
| Region rewrite (`inline-rewrite`) | Same Session as in-place ask | The Workspace's rules alone, from the same collector, placed before the request's own rewrite system text; no note about remembering | None consulted (`:tools 'none`) | None |
| Task timer | The Workspace's most recently active conversation; the conversation it was set in when that was temporary | As chat: it is sent through that View | That Session's | As chat |
| Woken Work Record | A new conversation bound to the record's Workspace | As chat | The default level (a new Session has recorded none) | A new conversation, so no earlier turns; the woken record is current and appears in the `[Work records]` block |
| Delegated sub-agent (`subagent`) | Its own Session (`subagent-…`); Workspace id and root copied from the parent's tool execution context at delegation | The rules text appended to its system text by `superchat--subagent-system-with-rules`; not the note about remembering | The parent's effective level, pinned when it starts; approval policy `never`, so it never asks | None of the parent's. A prepared prompt, plus a snapshot of the parent's current Work Record when there is one; the task is recorded under that record |
| Workflow step (`workflow`) | The Session of the View the workflow runs in; that View's Workspace id and root travel in `:runtime-extras` | The Workspace's rules alone, from the same collector, on every step; no note about remembering | That Session's | A prepared step prompt; no history, no `[Work records]` block; not recorded as conversation |
| Compaction (`compaction`) | An isolated Session `<session>/compaction-<hash>` | None | None consulted (`:tools 'none`) | Only the material it is given as a prepared prompt |
| Skill matching (`skills`) | An ephemeral Session `skills-match-…` | None | None consulted (`:tools 'none`) | Only the prepared match prompt; not recorded as conversation |
| Org Babel, Magit (`org-babel`, `magit`; blocking) | The Session that owns the calling buffer when there is one, else an ephemeral id. No Workspace is resolved | None | None consulted (`:tools 'none`) | None; not recorded as conversation |

The Telega and emacsclient integrations were not traced for this table.

## Extension interfaces and surfaces

Services expose declared methods; core driver calls perform request preparation. Events have declared `observe`, `collect`, `fold`, `first` or `waterfall` combination rules. Definitions and callback arguments live in `superchat-service.el` and `superchat-event.el`, rather than in a second handwritten inventory. Observe registrations may be required; Runtime uses the current `run-sealing` emission's failures to downgrade an otherwise complete Run when its recording is incomplete. Named Authority policies can only tighten the effective decision.

Chat, inline ask/rewrite, workflow, subagent, compaction, skills, Telega and emacsclient are producers or integrations over these interfaces. Org Babel and Magit use the declared blocking adapter because their host interfaces require a synchronous result. Overlay-based rewrite applies source changes only through an accepted Change Set. Plans, Memory, Workspace and Experience retain their domain-owned read models and integrations; they do not create another Runtime. Experience remains a default plugin with its recovery Service, projection, completion observer and read-only monitor. Its standalone context collector and review commands are absent; retained Claim/Feedback event types remain readable. The removal scope is defined by the [M3a decision](decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md).

Use [development verification](agents/development.md) for the canonical smoke and Kernel checks. The [roadmap](roadmap.md), not this map, owns migration order and remaining work.
