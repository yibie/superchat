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
| Tool selection and execution | `superchat-select-tools.el`, `superchat-tools.el`, `superchat-authority.el` | Granting more Authority through child Scope shadowing |
| Prompt construction | `superchat-prompt-hooks.el`, `superchat-prompt.el` | Mutating Tape history |
| Durable facts and read models | `superchat-tape.el`, `superchat-db.el`, `superchat-projection.el`, `superchat-tape-view.el` | Treating a Projection or selected view as mutable source history |
| Chat View and live editor resources | `superchat.el`, `superchat-render.el`, `superchat-editor-workspace.el` | Inferring ownership from a buffer name, project or selected window |

Default plugin definitions live next to their implementations; `superchat-default-plugins.el` supplies lazy definitions. `superchat-plugins`, defined in `superchat-plugin.el`, is mounted at the end of `superchat.el`. Tools and entry points can be contributed by a shared plugin; feature names are not an inventory of independently removable plugins. Services have global providers, while contributions such as tools, commands and Event listeners have scoped visibility. The [composition contract](design/plugin-architecture.md) defines these distinctions.

The large UI, dispatcher, Runtime and LLM modules retain direct dependencies. The Kernel vocabulary and product code are also co-located in `superchat-pipeline.el`. A table calling all of these files a product-free Kernel would misdescribe the implementation.

## One request through the system

Public model producers submit an Envelope containing input, a declared Surface and an exact Sink through the `driver` Service. The driver adapts that Envelope to Runtime's internal Request; Runtime alone accepts and finishes execution.

1. **Prepare surface input.** Chat parses `@model`, `>skill`, `/command` and `#file` with `superchat-core-prepare-turn` over a preparation Turn. The dispatcher handles local commands and routing before a model Envelope exists. The preparation Turn and Runtime's execution Turn share a struct type, not a lifecycle owner.
2. **Run the driver preparation sequence.** `superchat-pipeline-submit` validates the Envelope, resolves the Session, admits the submission through Runtime, merges core state, selects tools, collects context through the `collect-context` Event and assembles the prompt. Admission may process, queue, steer, drop or handle the input without starting a new Run.
3. **Accept execution.** Runtime receives the prepared Request and borrows the caller's execution Scope. The `model-transport` provider runs model attempts and the tool loop. Tools cross the shared Authority execution interface, including asynchronous and delegated tool calls.
4. **Persist and deliver.** Runtime uses its per-Run accounting of unsettled Effects and undecided Approvals when sealing, records Run/Turn facts and delivers through the originally accepted Sink. Required recording failures retain their audit-incomplete handling. Runtime quiesces execution; a late fact cannot reopen its terminal state. Consumers use the recorded Run terminal under the [request lifecycle contract](protocols/request-lifecycle.md).

The fixed Stage registry is retired: the eight names it declared (`parse`, `resolve-session`, `admit`, `load-state`, `select-tools`, `collect-context`, `assemble-prompt`, `save-state`) are no longer a registration or lookup surface. `parse` is the chat surface's own direct call; Session identity, state merging, admission, tool selection and prompt assembly are core driver calls; `collect-context` is an Event whose listeners return fragment lists. Extension now happens through Events and the two ordered-await waterfalls (`pre-step`, `before-provider-request`); calling the model, sealing a Run and delivering to a Sink were never Stages. The [request lifecycle contract](protocols/request-lifecycle.md) covers admission, queues, cancellation and failure handling.

## Prompt and history

`collect-context` listeners return labelled fragments. The pure `superchat-prompt-assemble` orders those fragments and returns `(:system :messages :receipt)`. Hard fragments must fit; soft fragments can be refit, truncated or omitted. A plugin adds or withdraws fragments through its own Scope registration, not by adding another driver.

The chat and inline-ask history collector uses a Tape-backed Context View with source exclusions and a remaining-budget refit. Other producers, including skills, workflows, subagents and compaction, can supply prepared text through their collector. The chat View also maintains process-local conversation state; that state is not permission to overwrite Tape. Anchor-backed compaction changes the selected View and preserves the original Entries.

The assembly receipt records selected fragment identities and budget decisions. Provider evidence is collected separately in `superchat-llm.el`. The `prompt-reconstructable` Invariant checks that an assembly receipt was appended; it does not independently reconstruct and compare every model-visible request. A recorded receipt is not, by itself, proof of full prompt reconstruction.

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

Runtime borrows execution Scopes; it creates no automatic Run Scope. There is no Session Scope level. The Preset owner calls Runtime retirement before disposing registrations on switch or View deactivation. Generic Scope disposal is not an independent Run cancellation or buffer-close authority. See the [Preset lifecycle](design/plugin-architecture.md#preset-composition-and-lifetime) and [Session close protocol](protocols/session-editor-workspace-close.md).

## Extension interfaces and surfaces

Services expose declared methods; core driver calls perform request preparation. Events have declared `observe`, `collect`, `fold`, `first` or `waterfall` combination rules. Definitions and callback arguments live in `superchat-service.el` and `superchat-event.el`, rather than in a second handwritten inventory. Observe registrations may be required; Runtime uses the current `run-sealing` emission's failures to downgrade an otherwise complete Run when its recording is incomplete. Named Authority policies can only tighten the effective decision.

Chat, inline ask/rewrite, workflow, subagent, compaction, skills, Telega and emacsclient are producers or integrations over these interfaces. Org Babel and Magit use the declared blocking adapter because their host interfaces require a synchronous result. Overlay-based rewrite applies source changes only through an accepted Change Set. Plans, Memory, Workspace and Experience retain their domain-owned read models and integrations; they do not create another Runtime. Experience remains a default plugin with its recovery Service, projection, completion observer and read-only monitor. Its standalone context collector and review commands are absent; retained Claim/Feedback event types remain readable. The removal scope is defined by the [M3a decision](decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md).

Use [development verification](agents/development.md) for the canonical smoke and Kernel checks. The [roadmap](roadmap.md), not this map, owns migration order and remaining work.
