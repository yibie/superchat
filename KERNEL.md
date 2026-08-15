# Project Kernel

status: enacted
version: K1
scope: superchat — an Emacs-native chat surface and Agent Runtime
source: START scan (`AGENTS.md`, `CONTEXT.md`, `superchat-*.el`, `test/`, `docs/adr/`)
verified_at: 2026-08-26
confidence: confirmed-only

## Concepts

### Session

since: K1
source: `CONTEXT.md:44`; `superchat-runtime.el:1372`; `superchat-db.el:133`; `superchat-tape.el:434`; `superchat.el:1231`

A Session is a durable, string-identified logical context that outlives any single request. Every Tape row carries a `session_id` NOT NULL column, `superchat-tape-append-event` refuses an event without a non-empty `:session-id`, and Runtime mints an ephemeral one (`superchat-runtime--new-id "session"`) when a submitted Request omits it. A Session also carries configuration — capability, topology, authority policy, workspace binding — which is not stored as mutable state but replayed from Tape by `superchat--restore-session-runtime-configuration`.

### Turn

since: K1
source: `superchat-core.el:21`; `superchat-core.el:44`; `superchat-core.el:135`; `CONTEXT.md:48`

A Turn is the concrete cl-struct `superchat-turn` holding one request's whole preparation state: read-only `id` and `session-id`, the raw `inbound` text, the parsed `clean-input`, and the derived `skill`, `preset`, `command`, `context-files`, `system-prompt`, `prompt`, `tools`, and `state`. `superchat-turn-new` creates one per request; `superchat-core-prepare-turn` runs it through a pure parse-then-hook pipeline that populates prompt and system-prompt without recording or sending anything.

### Run

since: K1
source: `superchat-runtime.el:1360`; `superchat-runtime.el:1410`; `superchat-runtime.el:108`; `superchat-runtime.el:91`; `CONTEXT.md:52`

A Run is one concrete execution inside a Turn, materialized as a plist entry in the process-local registry `superchat-runtime--runs` keyed by Run ID. The entry carries `:session-id`, `:turn-id`, `:run-id`, `:actor-id`, `:profile-id`, `:parent-run-id`, `:workspace-id`, `:correlation-id`, `:capability`, `:topology`, `:authority-policy`, `:surface`, `:status`, and a wall-clock `:deadline`. Its status moves toward one of six terminal values (`completed failed cancelled timed-out audit-incomplete abandoned`), after which further transport callbacks are ignored. A delegated Run names its direct parent through `:parent-run-id` rather than creating a second Runtime.

### Request Plan

since: K1
source: `superchat-dispatcher.el:366`; `superchat-dispatcher.el:389`; `superchat-dispatcher.el:428`; `superchat-dispatcher.el:651`; `superchat-runtime.el:1232`

A Request Plan is the inspectable plist that stands between a prepared Turn and a Run. `superchat--request-plan-from-turn` projects the mutable Turn into it (`:prompt`, `:input`, `:session-id`, `:context-envelope`, plus the Session's runtime configuration); `superchat-build-request-plan` produces one as a dry run with no backend call. Two paths hand a plan to `superchat-runtime-submit` — `superchat-execute-request-plan` and the dispatcher's `:llm-query` branch — and both wrap it in `superchat-extension-compose-request` first. Runtime then validates it structurally — non-empty `:prompt`, known topology, known Authority policy, existing `:workspace-root` — before accepting it.

### Preset and Agent Profile

since: K1
source: `superchat-preset.el:22`; `superchat-preset.el:51`; `superchat-preset.el:81`; `superchat-preset.el:381`; `superchat-skills.el:147`; `superchat-dispatcher.el:571`

A Preset is the cl-struct `superchat-preset`: a portable configuration bundle with read-only `name` and `profile-id` slots, a `type` of `prompt`, `agent`, `plan`, or `workflow`, an explicit `capability`, a `skill-body`, and optional tool/model/limit overrides. A Preset whose type is `agent` is an Agent Profile (`superchat-preset-agent-p`), and `profile-id` — not the user-facing lookup name — is its stable identity. A Skill is one source of Presets: `superchat-skills-load` parses a markdown file's YAML frontmatter into a `superchat-preset`, and the dispatcher reaches it through `superchat-skills-invoke` on `>skill` input.

### Capability and Topology

since: K1
source: `superchat-runtime.el:1337`; `superchat-runtime.el:1386`; `superchat-runtime.el:1327`; `superchat.el:1203`; `CONTEXT.md:28`; `CONTEXT.md:34`

Capability and Topology are two independent coordinates a Run is submitted with. Capability names the purpose of the work (defaulting to `"chat"`) and is fixed by an Agent Profile: `superchat-runtime--validate-profile-capability` signals when a Request's capability conflicts with the Profile's. Topology names the organization of participants and is a closed set — `single`, `team`, `subagent`, `workflow` — rejected at validation if unknown. They do not define each other: the team overlay in `superchat--apply-topology-overlay` adds team tools and a topology briefing without touching capability.

### Tape Entry

since: K1
source: `superchat-tape.el:417`; `superchat-db.el:133`; `superchat-db.el:378`; `superchat-tape.el:98`; `CONTEXT.md:71`

A Tape Entry is one immutable, ordered Fact. `superchat-tape-append-event` is the only Runtime-event door into the store; it assigns a logical `event_id` and `recorded_at`, redacts conventionally secret fields, size-limits payloads, and writes a single INSERT into the `tape` table, whose numeric AUTOINCREMENT `id` is the storage cursor. The persistence path is append-only: `superchat-db.el` issues INSERTs against `tape` and no UPDATE or DELETE of a tape row. Entries carry an envelope of optional identity keys — turn, run, actor, profile, workspace, correlation, causation, parent-run, team, audience, sensitivity.

### Anchor

since: K1
source: `superchat-tape.el:377`; `superchat-tape.el:36`; `superchat-tape-view.el:294`; `superchat-db.el:400`; `CONTEXT.md:75`

An Anchor is itself a Tape Entry (legacy kind `"anchor"`) that marks and distills earlier Entries while keeping enough provenance to return to them. `superchat-tape-append-compaction-anchor` writes a typed anchor carrying `source_ids` and refuses any source id that is not already a row in the same Session, and `superchat-view-expand-anchor` reads those ids back to recover the raw Entries. Creating an Anchor adds a row; it removes nothing.

### Context View

since: K1
source: `superchat-tape-view.el:1037`; `superchat-tape-view.el:1017`; `superchat-prompt-hooks.el:322`; `superchat-prompt-hooks.el:390`

A Context View is the rebuildable, purpose-scoped selection of Tape Entries handed to the model, materialized as a `"context-continuity-v0"` envelope plist. It carries `:status` (satisfied/partial/empty/unknown/blocked), a rendered `:prompt`, a `:scope` naming the session, purpose, lineage and selection policy, `:items`, a `:disclosure` split into covered / represented / omitted / recallable, and a `:receipt`. `superchat-view-assemble` builds it and `superchat-prompt-hook--conversation-history`, registered on `superchat-build-prompt-functions`, prepends it to the prompt and attaches the whole envelope to Turn state for audit. It never writes to Tape.

### Projection

since: K1
source: `superchat-projection.el:24`; `superchat-projection.el:53`; `superchat-projection.el:64`; `superchat-memory.el:248`; `superchat-tape-view.el:185`; `superchat-workspace.el:1040`

A Projection is a registered, versioned reducer over Tape events. `superchat-projection-register` takes a `:version`, an `:initial` state, an `:apply` of (STATE EVENT), and — for materialized projections — a `:select` and `:reset` that support checkpointed catch-up and full rebuild. Six producers register in tree: `memory`, `experience-applicability`, `session-configuration`, `workflow-instance`, `team`, and `superchat-workspace-artifacts`. Projection state is derived and resettable; the Tape rows it folds are not.

### Tool

since: K1
source: `superchat-tools.el:1714`; `superchat-tools.el:1731`; `superchat-tools.el:1765`; `superchat-tools.el:1449`; `superchat-authority.el:1132`; `superchat-agent-loop.el:321`; `superchat-llm.el:1278`

A Tool is a named, model-invocable function on the LLM surface, and it exists in exactly three provenances: the thirty-one reserved built-ins in `superchat-tools--builtin-tool-names`; plugin tools declared through `superchat-register-tool`, which must supply a mandatory `:effects` declaration and cannot reuse a built-in name; and tools the model synthesizes mid-run through the off-by-default `define_tool`. All three converge: every model-visible tool is wrapped by `superchat--agent-wrap-function` (bound as the request's tool wrapper in `superchat-llm.el`), and that wrapper calls `superchat-tool-execute`.

### Effect

since: K1
source: `superchat-authority.el:258`; `superchat-authority.el:377`; `superchat-authority.el:322`; `superchat-tools.el:1765`

An Effect is the pure descriptor `(:effect KIND :resource RESOURCE [:scope SCOPE])` that says what a tool call will do. The kind set is closed: `local-read`, `local-write`, `process-exec`, `runtime-exec`, `external-read`, `external-write`, and `unknown`. Effects are produced per-call by a resolver registered under the tool's name; a tool with no registered resolver resolves to a single `unknown`/`dynamic` effect rather than to nothing. This is how Authority stays name-blind — `superchat-register-tool` errors outright when `:effects` is absent, with the message that Authority never learns tool names.

### Authority Decision

since: K1
source: `superchat-authority.el:115`; `superchat-authority.el:373`; `superchat-authority.el:360`; `superchat-authority.el:51`; `superchat-agent-loop.el:55`

An Authority Decision is one of four ranked verdicts — `deny` (3), `ask` (2), `allow` (1), `defer` (0) — composed across a call's Effects with the highest rank winning. The default verdict for any recognized effect kind is `ask` when attendance is `attended` and `deny` when `unattended`. Named policies (`default observer developer-project operator cowork`, plus plugin-registered ones) are tighten-only, and preset guardrails follow the same rule: `superchat--agent-effective-guardrails` warns and clamps when a Preset tries to raise `max_tool_calls` above the global limit or to disable global destructive confirmation.

### Closure Judgment

since: K1
source: `superchat-closure.el:783`; `superchat-closure.el:13`; `superchat-runtime.el:795`; `superchat-runtime.el:81`

A Closure Judgment is a Tape-derived verdict about a Run or Turn, produced by the single pure interface `superchat-closure-judge`. It answers a typed query — `trace-well-formed`, `pending-effects`, `pending-approvals`, `residual-boundary`, `ready-terminal`, `durable-run-terminal`, `durable-turn-terminal` — and returns `:verdict`, the echoed `:judgment`, and an auditable `:witness` naming the contract and the event ids it relied on. It is load-bearing, not advisory: with `superchat-closure-terminal-guard-enabled` on by default, `superchat-runtime--guard-terminal` signals `superchat-closure-terminal-not-ready` unless the judgment proves the Run is ready for that exact terminal kind.

### Workspace Artifact

since: K1
source: `superchat-workspace.el:127`; `superchat-workspace.el:151`; `superchat-workspace.el:824`; `superchat-workspace.el:861`; `superchat-workspace.el:625`; `superchat-tools.el:1518`

A Workspace is a logical, id-addressed registry that Runs bind to (`superchat-workspace-bind`), and an Artifact is one revisioned, addressable unit of shared content inside it. Each Artifact has a generated `artifact-id`, an adapter (marker-backed buffer region, or a generated/durable file), a content hash, and a revision. Reads and writes reach it only through the `workspace_read` / `workspace_write` tools. A write originating from tool context against an existing Artifact must present the expected revision: `superchat-workspace-write` runs `superchat-workspace--check-revision` before and again at the commit point, and a stale or missing revision aborts the write. On the cross-Run read handoff the same check signals the typed condition `superchat-workspace-artifact-revision-error`, which Runtime and the Experience model match on by name.

### Extension

since: K1
source: `superchat-extension.el:201`; `superchat-extension.el:378`; `superchat-extension.el:292`; `superchat-dispatcher.el:436`

An Extension is a named, declaratively scoped bundle of contributions: an `:active-when` predicate over the Request, `:tools` it claims, a `:context` function producing deterministic prompt text, `:knowledge` for the system prompt, and unconditionally registered `:commands` and Authority `:policies`. `superchat-extension-compose-request`, called by the dispatcher for every plan it executes, folds active extensions' contributions into the Request. A claimed tool leaves the global model surface and returns only through activation or an explicit request-local tool list, so an inactive Extension costs nothing in the prompt or the tool surface.

### Claim and Feedback

since: K1
source: `superchat-claims.el:29`; `superchat-claims.el:120`; `superchat-claims.el:182`; `superchat-claims.el:278`; `superchat-claims.el:304`

A Claim is a contestable statement about Facts within an explicit scope, recorded as a `claim.recorded` Tape event with a statement, scope, one of five closed kinds (`generic attribution pattern context candidate`), and a basis of typed Tape references that must each resolve to exactly one existing event. Feedback is a separate `feedback.recorded` event that supports, challenges, or corrects a named Claim. Neither overwrites anything: a correction is a new Claim carrying `:supersedes` plus its Feedback, committed in one transaction, and the reading side (`superchat-claims-current-view`) is a replay, not stored truth.

## Identities

### Session identity is a minted, buffer-local string that partitions the Tape

since: K1
source: `superchat.el:2094`; `superchat.el:957`; `superchat.el:2100`; `superchat-db.el:135`; `superchat-db.el:378`; `superchat-runtime.el:1373`

A Session is identified by a string minted as `(format "%s%04x" (format-time-string "%Y%m%d-%H%M%S-") (random 65536))` by `superchat--new-session-id` and held in the buffer-local variable `superchat--session-id`; every Tape row is stored under that string in the NOT NULL `session_id` column, so the Session id is the partition key for all durable history. The View buffer name `*superchat:<session-id>*` is derived from the id and is a display label only, never the authority — `superchat-open-session` resolves a Session from the id and rebuilds the buffer. `superchat-runtime-submit` mints an ephemeral `session-<timestamp>-<rand>` when a caller supplies no `:session-id`, and duplicate helpers in `superchat-agent-loop.el:266` and `superchat-compact.el:93` recreate the same format when the variable is unset.

### Turn identity is a read-only struct slot, distinct from the Tape row id shown as "Turn" in the branch UI

since: K1
source: `superchat-core.el:24`; `superchat-core.el:47`; `superchat-runtime.el:762`; `superchat-runtime.el:1375`; `superchat.el:2231`; `superchat.el:2239`; `superchat-tape-view.el:35`

A Turn's authoritative identity is the `id` slot of the `superchat-turn` cl-struct, declared `:read-only t` and set once at construction to `(format "%d-%x" (time-convert nil 'integer) (random #xffff))`; its `session-id` slot is likewise read-only and defaults to the literal `"default"`. Runtime builds the Turn from the request and takes `turn-id` from that slot. Separately, the branch command's "Branch after Turn: #N" choices are SQLite Tape row ids returned by `superchat-view-entries` (`SELECT id, ...`), not `superchat-turn-id` values — that user-facing "Turn" number is a storage cursor and is not the runtime Turn identity.

### Run identity is minted once and is durable on Tape; the process-local registry is explicitly not authoritative

since: K1
source: `superchat-runtime.el:152`; `superchat-runtime.el:1376`; `superchat-runtime.el:1399`; `superchat-runtime.el:180`; `superchat-runtime.el:273`; `superchat-runtime.el:291`

`superchat-runtime--new-id` produces `run-<YYYYmmdd-HHMMSS-µµµµµµ>-<4 hex>` and `superchat-runtime-submit` allocates exactly one such `run-id` per accepted Run; that id is the key of the private hash table `superchat-runtime--runs` and is carried on every Runtime event. A Run's `:correlation-id` defaults to its own `run-id` when the caller supplies none, and a child Run names its parent through `:parent-run-id` rather than through any structure in its own id. Across a process restart the id on Tape is what survives: `superchat-runtime-reconcile-session` reconstructs open Runs by matching `run.started` events on `:run-id` and states in its own docstring that "Tape and Closure are the only sources of truth; the process-local Run registry is intentionally ignored."

### A Tape Entry has two identities: a logical event id assigned by Tape and a numeric storage cursor assigned by SQLite

since: K1
source: `superchat-tape.el:170`; `superchat-tape.el:417`; `superchat-tape.el:465`; `superchat-db.el:134`; `superchat-db.el:380`; `superchat-projection.el:79`

`superchat-tape--new-event-id` returns `evt_` plus the first 26 hex characters of a SHA-256 over current time, `emacs-pid`, and `random`, and `superchat-tape-append-event` assigns it (plus `recorded_at`) into the event envelope stored in the row's `meta` JSON; the docstring states the module assigns the logical `event_id` while "the database assigns the numeric storage cursor returned to legacy callers." That cursor is the `id INTEGER PRIMARY KEY AUTOINCREMENT` column, returned by `last_insert_rowid()`. `superchat-tape-append-event-record` returns both as `:event-id` and `:storage-id`. Ordering and resumption use the storage cursor — `superchat-projection-catch-up` checkpoints on it and errors unless each event's `storage-id` is an integer strictly greater than the last — while cross-event references (causation, closure accounting) use the logical event id.

### Workspace identity is a deterministic hash of the resolved root directory

since: K1
source: `superchat.el:1156`; `superchat.el:1164`; `superchat.el:3380`; `superchat-workspace.el:66`; `superchat-workspace.el:63`; `superchat-workspace.el:99`

`superchat--workspace-binding-for-directory` truenames and expands the directory, then derives the id as `"workspace-"` concatenated with the first 12 characters of `(secure-hash 'sha256 root)`, so the same directory always yields the same Workspace id — this path is reached at runtime through `/team <directory>`, which calls `superchat--bind-workspace`. The registry `superchat-workspace--registry` is keyed by that id string. `superchat-workspace-current-id` resolves the id by precedence — Tool Executor context, current buffer binding, the chat buffer's binding — and falls back to the literal `"legacy"` for direct Lisp calls, so an unbound caller is attributed to a named fallback Workspace rather than to none.

### Artifact identity is a process-local id; its revision is the SHA-256 of its content and gates writes

since: K1
source: `superchat-workspace.el:151`; `superchat-workspace.el:190`; `superchat-workspace.el:223`; `superchat-workspace.el:824`; `superchat-workspace.el:860`; `superchat-workspace.el:875`; `superchat-workspace.el:623`; `superchat-workspace.el:42`

`superchat-workspace--artifact-id` builds `artifact-<first 8 of sha256(workspace-id)>-<counter>` from the process-local counter `superchat-workspace--artifact-counter` and loops until the candidate is absent from that workspace's artifact alist, so uniqueness is guaranteed only within one workspace in one Emacs process. The Artifact's revision is `(secure-hash 'sha256 content)` recomputed from live content, and the metadata plist deliberately reports the same value under both `:revision` and `:content-hash`. Revision is load-bearing identity, not decoration: a write arriving from Tool Executor context must pass `expected-revision`, which `superchat-workspace--check-revision` compares for string equality, and file-backed writes re-check it at the commit point; a stale write fails with a plain error. The dedicated condition `superchat-workspace-artifact-revision-error` is reserved for the cross-Run handoff path, where `superchat-workspace--require-artifact-readable` checks the revision, consults the Closure delivery decision, and re-checks after that read because the live buffer may have moved.

### Agent Profile identity is `profile-id`, stable across renames; the preset name is a lookup label

since: K1
source: `superchat-preset.el:51`; `superchat-preset.el:368`; `superchat-preset.el:381`; `superchat.el:1086`; `superchat-runtime.el:1380`; `test/test-preset-contract.el:282`

The `superchat-preset` struct declares both `name` and `profile-id` as `:read-only t`, and only agent-type presets receive a `profile-id`; when an agent preset omits it, `superchat-preset-from-plist` falls back to the name and emits a `"Deprecated agent preset without profile-id"` warning, while prompt presets get none. Identity is what restoration follows: `superchat--resolve-profile-preset` first tries a by-name lookup, accepts it only if the loaded preset's `profile-id` matches, and otherwise scans every available skill for the matching `profile-id` — explicitly to restore "a renamed Profile." The test `runtime/agent-profile-id-is-stable` asserts that two presets with different names but `profile_id: profile.coder` carry the same identity. Runtime records `:profile-id` on the Run and validates the requested Capability against the Profile's.

### Agent Instance and Actor identity is minted per delegation, separate from the Profile it runs

since: K1
source: `superchat-subagent.el:824`; `superchat-subagent.el:860`; `superchat-subagent.el:866`; `superchat-subagent.el:1064`; `superchat-subagent.el:160`; `superchat-subagent.el:168`; `superchat-runtime.el:1377`; `superchat-subagent.el:407`

`superchat--subagent-make-context`, reached from the delegation engine, mints `subagent-<preset-name>-<YYYYmmdd-HHMMSS>-<4 hex>` and installs that one string as both `:session-id` and `:actor-id`, while `:profile-id` comes from the preset — so the instance identity is scoped to this participation and is not the Profile identity, and the same Profile delegated twice yields two distinct Actors. Team scope reuses Session identity: `superchat--team-current-id` returns the bound `superchat--team-id` or else the current `superchat--session-id`, and `superchat--team-current-actor` defaults to the literal `"lead"` — the same default Runtime applies to a Run's `:actor-id`. Team task ids are `T<tape row id>`, derived from the Tape storage cursor of the creating event.

### Tool execution identity is an execution id plus a canonical request hash, and both are enforced unique by Closure

since: K1
source: `superchat-authority.el:426`; `superchat-authority.el:406`; `superchat-authority.el:1220`; `superchat-authority.el:1132`; `superchat-closure.el:248`; `superchat-closure.el:279`

Each pass through the single tool choke point `superchat-tool-execute` takes `:execution-id` from context or mints one as `tool-<UTC timestamp with ms>-<6 hex>`, then computes `request-hash` via `superchat-authority-approval-hash`: a SHA-256 over a canonical payload of tool name, normalized args, sorted normalized effects, policy version, and that execution id. The two identities carry different accounting: `superchat-closure.el` keys effects by `execution-id` and rejects a second `tool.requested` with the same one as `duplicate-execution-id`, and keys approvals by `request-hash`, rejecting a repeat as `duplicate-approval` and refusing an `approval.requested` whose hash does not match the effect it claims. A tool name alone is therefore not an execution identity — it is only one field inside the hash.

### A synthesized tool is registered by name but approved by the SHA-256 over its complete definition

since: K1
source: `superchat-synthesis.el:392`; `superchat-synthesis.el:271`; `superchat-synthesis.el:541`; `superchat-synthesis.el:555`; `superchat-synthesis.el:596`

`superchat--syn-hash` returns `(secure-hash 'sha256 (prin1-to-string (list name description arg-plists body)))`, and the approval ledger is keyed by that hash, not by the tool name: `superchat-synthesis-rehydrate` recomputes the hash for each persisted entry and loads it silently only when `superchat--syn-approval-state` reports `approved` for that exact digest, routing anything "changed or never reviewed" to manual review. The name remains the registration and dispatch key — the live tool list is deduplicated by `llm-tool-name` and `superchat--syn-names` tracks names — so name and content identity are deliberately different keys: editing a body keeps the name but produces a new, unapproved identity.

### Skill/Preset lookup identity is the file basename in the skills directory, not the frontmatter name

since: K1
source: `superchat-skills.el:120`; `superchat-skills.el:132`; `superchat-skills.el:136`; `superchat-skills.el:147`; `superchat-skills.el:159`

`superchat-skills-get-available` enumerates skills as `file-name-base` of the files in `superchat-skills-directory`, `superchat-skills-exists-p` tests membership in that list, and `superchat-skills--find-file` resolves a skill by appending each supported extension to the given name — so the file basename is the addressable identity used by completion and by `/preset`-style activation. The frontmatter `name:` field is only the preset's display name and defaults to the basename when absent (`(or (cdr (assoc "name" alist)) skill-name)`); a file whose frontmatter name differs is still loadable only under its basename.

## Relations

### Session --contains--> Turn

since: K1
source: `superchat-core.el:25`; `superchat-core.el:44`; `superchat-runtime.el:762`; `superchat-runtime.el:1372`

A Turn names exactly one Session through the read-only `session-id` slot of the `superchat-turn` struct, set once by `superchat-turn-new` and never writable afterwards. Runtime resolves (or mints) the Session ID before it builds the Turn (`superchat-runtime--turn-from-request` is called with `session-id`), so a Session accumulates many Turns while a Turn can never migrate to another Session.

### Turn --owns--> Run

since: K1
source: `superchat-runtime.el:1374`; `superchat-runtime.el:1412`; `superchat-runtime.el:983`; `superchat-closure.el:519`; `superchat-closure.el:528`

Runtime creates the Turn first, reads its `turn-id`, then allocates a separate `run-id` and stores both on the Run registry entry, so every Run event envelope carries the turn_id of the Turn it executes. The edge is enforced, not merely recorded: Closure's interpreter rejects a trace as `invalid-run-identity` when a Run-scoped event lacks a turn-id or disagrees with the turn-id already bound to that Run, and the Turn is sealed by the `turn.completed` event emitted from the Run's terminal path (`superchat-runtime--complete-turn`).

### Run --parent-run-id--> Run

since: K1
source: `superchat-runtime.el:1250`; `superchat-runtime.el:239`; `superchat-tape.el:54`; `superchat-subagent.el:834`; `superchat-subagent.el:1045`

A delegated Run references its direct parent by a single `:parent-run-id` string: Runtime validates it as a non-empty string on submit, stores it on the Run entry, and copies it into every event envelope (persisted as `parent_run_id`). The value is not invented by the child — `superchat--subagent-make-context` takes it from the parent's live tool-execution context `:run-id`, and `superchat--subagent-runtime-request` passes it back into `superchat-runtime-submit`, so delegation chains are reconstructible from Tape alone.

### Entry --belongs-to--> Session

since: K1
source: `superchat-db.el:135`; `superchat-db.el:378`; `superchat-tape.el:434`; `superchat-db.el:386`

Session membership is a storage-level column, not a convention: the `tape` table declares `session_id TEXT NOT NULL`, every insert supplies it, and `superchat-tape-append-event` refuses any event whose `:session-id` is missing or empty. Reads are session-scoped by the same column (`superchat-db-tape-replay` selects `WHERE session_id = ?`), which is why replay, projection, and View assembly are all defined per Session.

### Entry --carries--> Runtime provenance envelope

since: K1
source: `superchat-tape.el:46`; `superchat-tape.el:453`; `superchat-runtime.el:225`; `superchat-workspace.el:336`

Provenance is a fixed, named set of envelope keys — turn_id, run_id, actor_id, profile_id, workspace_id, correlation_id, causation_id, parent_run_id, team_id, audience, sensitivity — copied from the in-memory event into the persisted `meta` JSON by `superchat-tape-append-event`. Producers fill it from their execution context rather than from payload text: `superchat-runtime--event` builds it from the Run registry entry, and `superchat-workspace--emit` copies the same keys off the tool-execution context, so an Entry points back at the Run, Actor, Profile, and Workspace that produced it.

### Entry --caused-by--> Entry

since: K1
source: `superchat-tape.el:46`; `superchat-runtime.el:1473`; `superchat-runtime.el:1489`; `superchat-closure.el:535`

Entries form a causal chain through `causation_id`, which holds the logical `event_id` of the Entry that caused this one. Runtime maintains it by writing the id of each freshly appended event back onto the Run entry as the next `:causation-id` (intent.received → run.started → …), and Closure enforces the chain when judging a trace: an event whose `causation-id` names an ancestor not already seen in order yields `missing-causal-ancestor`.

### Anchor --references--> source Entries by storage id

since: K1
source: `superchat-tape.el:377`; `superchat-tape.el:386`; `superchat-tape.el:353`; `superchat-compact.el:401`; `superchat-tape-view.el:968`

A compaction Anchor is an Entry whose meta carries `source_ids`, a vector of unique positive Tape storage ids; `superchat-tape-append-compaction-anchor` validates each id with `superchat-tape-storage-reference-p` and refuses the Anchor if any source is not already an Entry of the same Session, so synthetic provenance cannot be written. The referenced Entries are never deleted — the compaction path (`superchat-compact--apply-summary`) only appends the Anchor, and the View layer expresses substitution as a `:represented` range tagged `:via-storage-id` with the Anchor's own storage id.

### Projection --reduces--> Tape Entries

since: K1
source: `superchat-projection.el:24`; `superchat-projection.el:53`; `superchat-projection.el:64`; `superchat-tape-view.el:185`; `superchat-memory.el:248`; `superchat-workspace.el:1040`; `superchat-db.el:149`; `superchat-memory.el:206`

A Projection is a named registered reducer `(STATE EVENT) → STATE` folded over Tape rows, either purely (`superchat-projection-replay`) or materialized with a checkpointed `select`/`reset` (`superchat-projection-catch-up`, one transaction per event). In-tree producers register against real Tape queries — `session-configuration`, `team`, `workflow-instance`, `experience-applicability`, `superchat-workspace-artifacts`, and the materialized `memory` projection — and materialized rows keep the back-reference: the `memory` table stores `source_tape_ids` and `source_event_ids`, and re-application is made idempotent by looking the row up by its source event id.

### Agent Instance --participates-under--> one Agent Profile

since: K1
source: `superchat-runtime.el:1377`; `superchat-runtime.el:1380`; `superchat-runtime.el:1337`; `superchat-subagent.el:866`; `superchat-subagent.el:497`

Runtime keeps the participating identity (`:actor-id`, defaulting to "lead") separate from the role it participates under (`:profile-id`, taken from the submitted Preset's read-only `profile-id`), and both travel on the Run entry and its event envelopes. The edge is validated at submit time: `superchat-runtime--validate-profile-capability` signals when the requested Capability differs from the Agent Profile's own Capability. Sub-agents follow the same shape — `superchat--subagent-make-context` mints a fresh instance id as `:actor-id` while setting `:profile-id` from the Preset, and `superchat--team-member-event` records the pair on the shared team Tape.

### Preset --carries--> Capability

since: K1
source: `superchat-preset.el:52`; `superchat-preset.el:55`; `superchat-runtime.el:1387`; `superchat-runtime.el:1356`

The `superchat-preset` struct holds a read-only `profile-id` and a `capability` slot, and Runtime derives the Run's effective Capability from the request or, failing that, from the Preset (`(or (plist-get request :capability) (superchat-preset-capability preset))`, falling back to "chat"). A request may repeat the Preset's Capability but may not reinterpret it: a mismatch between the requested Capability and an Agent Profile's Capability is an error, not a silent override.

### Authority --composes-over--> Effects

since: K1
source: `superchat-tools.el:1765`; `superchat-authority.el:322`; `superchat-authority.el:373`; `superchat-authority.el:360`; `superchat-authority.el:383`; `superchat-authority.el:1132`

Authority never edges to a tool name; it edges to the effect descriptors a tool declares. Every plugin tool must supply `:effects` (registration errors otherwise, with the comment "Authority never learns tool names"), `superchat-authority-resolve-effects` maps a tool name plus normalized args to `(:effect KIND :resource … [:scope …])` descriptors — defaulting to `unknown`/`dynamic` when no resolver exists — and `superchat-authority-decide` maps each descriptor to a per-effect decision and folds them with `superchat-authority-compose-decisions` (deny > ask > allow > defer). `superchat-tool-execute` is the single seam where this composition happens for a model-invoked call.

### Approval --binds-to--> one Tool Execution

since: K1
source: `superchat-authority.el:406`; `superchat-authority.el:1220`; `superchat-authority.el:1284`; `superchat-authority.el:1307`

An approval is not a free-floating yes: `superchat-tool-execute` allocates an `execution-id` and computes a canonical `request-hash` (SHA-256 over tool name, normalized args, resolved effects, policy version, and execution id), and both `approval.requested` and `approval.decided` carry that same pair plus the policy version, chained by `causation-id`. The decision therefore names the exact argument-and-effect snapshot it approved, so a later execution with different arguments cannot inherit it.

### Workspace --binds-to--> Session

since: K1
source: `superchat-workspace.el:127`; `superchat-workspace.el:145`; `superchat.el:1178`; `superchat.el:1267`; `superchat-workspace.el:325`

`superchat-workspace-bind` takes an optional `session-id` and stores it on the workspace plist alongside the root; the chat surface supplies the live `superchat--session-id` both when binding a directory (`superchat--bind-workspace`) and when restoring a Session's configuration from projection. That stored Session ID is what makes Workspace facts auditable: `superchat-workspace--emit` appends nothing unless a Session ID can be resolved (tool context, current Session, or the workspace's own binding), and tags the event with the Workspace id.

### Artifact --belongs-to--> Workspace, --carries--> revision

since: K1
source: `superchat-workspace.el:111`; `superchat-workspace.el:254`; `superchat-workspace.el:222`; `superchat-workspace.el:824`; `superchat-workspace.el:861`; `superchat-tools.el:1023`

Workspaces live in a registry keyed by Workspace ID, and each workspace plist owns its Artifacts as an alist under `:artifacts` plus one `:active-artifact-id`; an Artifact is only ever reached through its workspace. Each Artifact carries a `revision` that is the SHA-256 content hash of its current content, and the revision is load-bearing for concurrency: a tool-context write to an existing Artifact must pass `expected-revision` and `superchat-workspace--check-revision` signals on mismatch, while cross-Run reads must additionally re-check the revision around the delivery decision.

## Lifecycles

### Run lifecycle and terminal seal

since: K1
source: `superchat-runtime.el:91`; `superchat-runtime.el:1427`; `superchat-runtime.el:1488`; `superchat-runtime.el:1077`; `superchat-runtime.el:1071`; `superchat-runtime.el:1155`; `superchat-runtime.el:1219`; `superchat-runtime.el:1007`; `superchat-runtime.el:1152`; `superchat-runtime.el:1621`; `superchat-runtime.el:1733`; `test/test-runtime.el:399`; `test/test-runtime.el:706`

A Run has states `accepted` -> `running-model` -> `delivering` -> one terminal state drawn from the closed set `{completed, failed, cancelled, timed-out, audit-incomplete, abandoned}` declared in `superchat-runtime--terminal-statuses` (`superchat-runtime.el:91`). Edges: `superchat-runtime-submit` creates the entry at `accepted` (`:1427`) and moves it to `running-model` after appending `run.started` (`:1488`); the observer's `attempt-started` branch re-asserts `running-model` for each provider attempt (`:1635`); `superchat-runtime--finish` sets `delivering` (`:1077`) and then the terminal status; `superchat-runtime--deadline-expired` enters `--finish` with `timed-out` (`:1155`); `superchat-runtime-cancel` enters it with `cancelled` (`:1226`); any error inside `--finish` diverts to `superchat-runtime--audit-incomplete` from the enclosing `condition-case` handler (`:1152`, defun at `:1007`), the only edge that reaches `audit-incomplete`. The seal is one-way and one-shot: `--finish` does nothing when the entry is already terminal (`:1071`), `superchat-runtime-cancel` returns nil on a terminal entry (`:1219`), every observer branch (`request`, `attempt-started`, `usage`, context branches) re-reads the registry and refuses to mutate a terminal Run (`:1621` onward), and the streaming callback only forwards a chunk while status is exactly `running-model` (`:1733`). `test/test-runtime.el:399` proves a deadline followed by a late provider `terminal` callback yields exactly one `run.failed` and one `turn.completed`.

### Restart reconciliation to `abandoned`

since: K1
source: `superchat-runtime.el:273`; `superchat-runtime.el:265`; `superchat.el:2185`; `superchat-closure.el:409`; `test/test-runtime.el:103`

A Run that was persisted as started but never sealed reaches its terminal state by a second, registry-independent path. `superchat-runtime-reconcile-session` (`superchat-runtime.el:273`) replays the Session Tape, and for each `run.started` with no `run.completed`/`run.failed`/`run.cancelled` whose Closure verdict is `open`, appends the pair `run.failed(:status abandoned :abandoned t)` then `turn.completed(:status abandoned :abandoned t)`. A partially written pair (abandoned Run seal present, Turn seal missing) is completed without duplicating the Run seal (`:322`). The function explicitly ignores the process-local registry and never recreates a prompt or provider request; its only caller is the Session View rebuild in `superchat.el:2185`, wrapped in `ignore-errors`. Closure recognizes `abandoned` as a legal terminal transition only when the Run is started and not already terminal (`superchat-closure.el:409`).

### Turn lifecycle

since: K1
source: `superchat-core.el:44`; `superchat-core.el:135`; `superchat-dispatcher.el:504`; `superchat-runtime.el:762`; `superchat-runtime.el:983`; `superchat-runtime.el:957`; `test/test-runtime.el:434`

A Turn moves `new -> prepared -> owned-by-Runtime -> completed`, and completes exactly once. `superchat-turn-new` (`superchat-core.el:44`) creates it with an id and session id; `superchat-core-prepare-turn` (`:135`) runs the pure parse plus system-prompt plus build-prompt chain and deliberately defers all post-turn hooks so a prepared Turn can be inspected without being recorded or sent. `superchat-runtime--turn-from-request` (`superchat-runtime.el:762`) is where Runtime takes ownership. Completion runs through the single seam `superchat-runtime--complete-turn` (`:983`): it first calls `superchat-runtime--run-turn-hooks`, which is guarded by the `:turn-hooks-run` flag so pre-topic/post-turn/post-topic hooks fire at most once (`:957`), then appends `turn.completed` only if `:turn-completed` is not already set. When Tape persistence has already failed the caller passes `unaudited`, which flips `:turn-completed` in memory without inventing a `turn.completed` event (`:990`). `test/test-runtime.el:434` asserts Runtime is the only Turn-completion owner.

### Approval and tool-execution lifecycle

since: K1
source: `superchat-authority.el:1132`; `superchat-authority.el:1209`; `superchat-authority.el:1217`; `superchat-authority.el:1243`; `superchat-authority.el:1271`; `superchat-authority.el:1276`; `superchat-authority.el:1282`; `superchat-authority.el:1308`; `superchat-authority.el:1324`; `superchat-authority.el:1064`; `superchat-authority.el:1080`; `superchat-authority.el:853`; `superchat-llm.el:1282`

Every model-invoked tool call passes through `superchat-tool-execute` (`superchat-authority.el:1132`, reached because `superchat-llm.el:1282-1283` wraps every provider tool with `superchat--agent-make-wrapper`) and follows `requested -> decided -> executing -> discharged|failed`. After effects are resolved a decision is computed (`:1209`-`:1215`) and `defer` is collapsed to `ask` when attendance is `attended` and to `deny` otherwise (`:1217`); `tool.requested` is appended before any branch is taken (`:1243`). On a block reason or `deny`, `superchat-authority--record-failure` (`:853`) writes `tool.failed` with status `blocked`/`denied` and the caller receives a model-readable string; `original-fn` is never reached (`:1271`, `:1276`). On `ask`, `approval.requested` is appended, the approval function is consulted, and `approval.decided` records `allow` or `deny` (`:1282`-`:1313`); a denial produces `tool.failed` with status `cancelled` and the string `[Tool call cancelled by user]` (`:1324`) — a denial is a recorded terminal outcome for that effect, not an omission. Only `superchat-authority--invoke` reaches the implementation, and it first checks Run liveness — a terminal Run yields `tool.failed` with status `run-inactive` without invoking the tool (`:1064`) — then appends `tool.execution.started` before running any hook or tool code (`:1080`).

### Closure obligation state machine

since: K1
source: `superchat-closure.el:233`; `superchat-closure.el:264`; `superchat-closure.el:351`; `superchat-closure.el:360`; `superchat-closure.el:377`; `superchat-closure.el:394`; `superchat-closure.el:88`; `superchat-closure.el:92`; `superchat-closure.el:107`; `superchat-closure.el:431`

Closure reconstructs the same lifecycle from the Tape as a fold over events, and rejects any out-of-order edge. An Effect node is created at `requested` by `tool.requested` (`superchat-closure.el:233`, `:264`), moves to `executing` only on `tool.execution.started` and only when its recorded decision was `allow` or its Approval node's disposition is `allow` (`:336`-`:351`), and reaches `discharged` on `tool.completed` from `executing` only (`:360`, `:367`). `tool.failed` maps to `failed` (from `executing`), `blocked`/`denied` (from `requested` with decision `deny`), or `cancelled` (from `requested` with decision `ask` and a denied Approval) per the transition table at `:377`-`:387`; anything else is `invalid-effect-failure` (`:394`). An Approval node is created at `requested` by `approval.requested` and closed by `approval.decided` to `allow` or `deny` (`:266`, `:299`). Accounted dispositions are enumerated at `:88` and `:92`; `superchat-closure--terminal-ready-p` (`:107`) permits a Run terminal only when the kind is one of `{completed, failed, timed-out, cancelled}`, the Run is started, not already terminal, every Effect and Approval is accounted, and (except for `cancelled`) the recorded `response.completed` status equals the terminal kind. `turn.completed` is legal only after a Run terminal exists, only once, and only with the same status (`:431`).

### Closure verdict lattice

since: K1
source: `superchat-closure.el:783`; `superchat-closure.el:144`; `superchat-closure.el:154`; `superchat-closure.el:810`; `superchat-closure.el:813`; `superchat-closure.el:758`; `superchat-subagent.el:648`; `superchat-runtime.el:795`; `superchat-runtime.el:835`; `superchat-runtime.el:839`

`superchat-closure-judge` (`superchat-closure.el:783`) answers a typed query over a trace with a verdict plus an auditable witness. The fold produces `invalid` (`:144`) or `unknown` (`:154`, for unsupported schema, missing causal ancestor, unknown event type, and unsupported Authority policy contract). At the public boundary the query kind decides what a caller sees: for every kind except `trace-well-formed`, an `invalid` fold result is downgraded to `unknown` (`:812`-`:815`); a `trace-well-formed` query is answered earlier (`:810`-`:811`) by `superchat-closure--trace-well-formed` (`:758`), which returns `valid` on a clean fold and otherwise passes the `invalid` verdict through unchanged. `invalid` is therefore a real public verdict on that one kind, and `superchat--subagent-delivery-decision` (`superchat-subagent.el:648`) branches on it to reject a handoff. `ready-terminal` returns `proved` or `open`; `durable-run-terminal` and `durable-turn-terminal` return `proved` only when the corresponding seal exists. Runtime consumes this in two distinct modes: `superchat-runtime--guard-terminal` (`superchat-runtime.el:795`, enabled by default) signals `superchat-closure-terminal-not-ready` (`:835`) unless the verdict is `proved` with a matching terminal kind — thereby diverting the Run to `audit-incomplete` — while `superchat-runtime--run-closure-shadow` (`:839`, invoked last and inside `ignore-errors` at `:1149`) only records an `agreed`/`disagreed`/`unknown` observation and can never retroactively fail a closed Run.

### Synthesized-tool lifecycle

since: K1
source: `superchat-synthesis.el:225`; `superchat-synthesis.el:159`; `superchat-synthesis.el:392`; `superchat-synthesis.el:425`; `superchat-synthesis.el:541`; `superchat-synthesis.el:512`; `superchat-llm.el:1335`; `superchat.el:2294`

A model-written tool moves `proposed -> confirmed -> live -> persisted+approved -> rehydrated`, with two absorbing side states. `superchat--syn-define` (`superchat-synthesis.el:225`) builds the `defun`, then `superchat--syn-confirm` (`:159`) pretty-prints it and asks; a refusal ends the lifecycle with `[Tool definition declined by user]`. On approval the form is evaluated, wrapped, and hot-injected into the in-flight prompt so the next round can call it; when `persist` is set it also joins the session registry, is written to the store, and its SHA-256 over name+description+args+body is recorded as `approved` in a separate ledger plus a `synthesis.approval.decided` Tape event (`:392`, `:425`). At startup `superchat-synthesis-rehydrate` (`:541`, called from `superchat.el:2294`) re-hashes each stored entry: `approved` rehydrates silently via `superchat--syn-register-raw` (`:512`), `rejected` is skipped in silence, and any other state — a changed body, a never-reviewed entry — is pushed to `pending` and left for `superchat-tools-review-synthesized`. Editing the store therefore invalidates the hash and demotes the tool out of the live set. The binding site that makes this reachable at runtime is `superchat-llm.el:1335`.

### Compaction and Anchor lifecycle

since: K1
source: `superchat-compact.el:485`; `superchat-compact.el:498`; `superchat-compact.el:501`; `superchat-compact.el:376`; `superchat-compact.el:460`; `superchat-compact.el:401`; `superchat-tape.el:377`; `superchat-compact.el:105`; `superchat.el:3196`

Compaction is a state machine on a buffer-local flag plus a background Run: `idle -> in-flight -> (anchored | failed) -> idle`. `superchat-compact-session` (`superchat-compact.el:485`) refuses to start when there are no messages (`:498`) or `superchat-compact--in-flight` is already set (`:501`), then submits an isolated Runtime Run with its own Session ID, actor `compactor`, capability `compaction`, and the `observer` Authority policy (`:376`, fields at `:385`-`:389`), setting the flag at `:519` and clearing it again if submission signals (`:524`). Its delivery function calls `superchat-compact--finish` (`:460`), which clears the in-flight flag first (`:466`-`:468`) and then branches: a non-`completed` status or an unparseable summary ends the cycle with no Anchor written; only a `completed` Run with a parsed summary reaches `superchat-compact--apply-summary` (`:401`). That function appends a typed compaction Anchor via `superchat-tape-append-compaction-anchor`, which validates that every source id already exists in the same Session and then appends an `anchor` row (`superchat-tape.el:377`, checks at `:386`-`:390`) — the sources are never deleted — and only afterwards collapses the in-memory history onto the Anchor, preserving by id any entries recorded while the Run was in flight (`:427`-`:448`). `superchat-compact--expand-anchor` (`:105`) is the inverse edge and touches in-memory history only: it re-reads the exact source rows from the Tape and replaces the Anchor message in place, and is a no-op when repeated (`:162`). Both directions are reachable as the `/compact` and `/expand` commands (`superchat.el:3196`).

## Action Contracts

### Submit a Request to the Runtime

since: K1
source: `superchat-runtime.el:1360`; `superchat-runtime.el:1232`; `superchat-runtime.el:1765`; `test/test-runtime.el:706`

precondition: the model transport `superchat--llm-generate-answer` must be loadable, and the Request must pass `superchat-runtime--validate-request` — non-empty `:prompt`, a known Topology of single/team/subagent/workflow, a known Authority policy, an existing `:workspace-root` when given, and a Capability that does not conflict with the Agent Profile's declared Capability.
input: one immutable prepared Request plist (`:prompt` plus optional `:session-id`, `:capability`, `:topology`, `:authority-policy`, `:preset`, `:deadline`, `:parent-run-id`, `:context-envelope`, `:evidence-contract`, delivery/stream functions).
permitted effect: Runtime mints Session/Turn/Run identities, registers a private entry in `superchat-runtime--runs`, appends `intent.received` then `run.started` to the Tape, arms the wall-clock deadline, binds the tool-execution context, and invokes the model transport; the caller gets back only `(:turn-id :run-id :session-id :status accepted)` (`superchat-runtime.el:1765`).
invariant: Runtime owns the mutable Turn, the transport request, the Tape lifecycle and result delivery — callers keep only IDs and must query status through `superchat-runtime-status` (`superchat-runtime.el:221`); validation runs before the Run entry is created, so a Request that fails validation produces no Run and no Tape entry.
verification: `test/test-runtime.el:706` `runtime/submit-owns-lifecycle-and-public-status`, plus `runtime/rejects-unknown-configuration` (`test/test-runtime.el:1962`) and `runtime/request-deadline-validates-duration` (`test/test-runtime.el:303`).

### Execute a model-invoked Tool

since: K1
source: `superchat-authority.el:1132`; `superchat-authority.el:1060`; `superchat-authority.el:1244`; `test/test-authority.el:1270`

precondition: `original-fn` is callable, `args` is a list, an async call supplies a callback, and CONTEXT carries a non-empty `:session-id` and an explicit `:attendance` of `attended` or `unattended`; anything else errors before any effect.
input: the tool name, the raw implementation function, the normalized argument list, and an execution context (session/turn/run identity, attendance, Authority policy, guardrails, optional validator, approval function, block reason).
permitted effect: resolve declared Effects, compute a decision, append `tool.requested` (and for `ask`, `approval.requested` + `approval.decided`), then append `tool.execution.started` and only then call the implementation; on any other path (`blocked`, `deny`, declined approval, inactive Run) a failure event is recorded and a readable string is returned to the model instead.
invariant: no code path reaches `original-fn` before its audit events are durably appended — a pre-execution Tape write failure is caught by the outer handler at `superchat-authority.el:1332` and turned into `[Tool call blocked because audit failed: …]`; a Run that has already gone terminal never reaches the implementation (`superchat-authority.el:1063`).
verification: `test/test-authority.el:1270` `authority/executor-prewrite-failure-prevents-side-effect`, `test/test-authority.el:1244` `authority/executor-does-not-run-after-runtime-termination`, `test/test-authority.el:719` `authority/executor-deny-never-invokes-tool`.

### Append a Runtime Entry to the Tape

since: K1
source: `superchat-tape.el:417`; `superchat-tape.el:98`; `superchat-db.el:367`; `test/test-tape-events.el:99`

precondition: the event plist must carry a non-empty string `:type`, a non-empty string `:session-id`, a non-empty `:event-id` (minted here when absent) and a positive integer `:schema`.
input: a Runtime event plist — type, session, optional envelope keys (turn-id, run-id, actor-id, profile-id, parent-run-id, correlation-id, causation-id), topic, summary, and payload.
permitted effect: exactly one row is INSERTed into the `tape` table with a module-assigned `event_id` and `recorded_at` and a redacted, size-bounded payload; the numeric storage cursor is returned (`superchat-tape-append-event-record` returns both logical `:event-id` and `:storage-id`).
invariant: payloads are redacted for sensitive keys and truncated past `superchat-tape-max-inline-bytes`; a caller-supplied `recorded_at` is ignored; when `:requires-payload` is set an omitted oversized payload is an error rather than a silent gap; persistence errors are deliberately propagated so the caller cannot proceed as if the write happened.
verification: `test/test-tape-events.el:99` `tape-event/persists-versioned-envelope`, `:120` `tape-event/ignores-caller-supplied-recorded-time`, `:130` `tape-event/redacts-sensitive-fields-recursively`, `:198` `tape-event/propagates-adapter-failure`.

### Seal a Run at a terminal status

since: K1
source: `superchat-runtime.el:1068`; `superchat-runtime.el:795`; `superchat-runtime.el:1210`; `test/test-runtime.el:1432`

precondition: the Run entry exists and its current status is not already one of `completed failed cancelled timed-out audit-incomplete abandoned`; the Closure guard (on by default) must judge the Run `ready-terminal` = proved for exactly this terminal kind.
input: the Run ID, the intended terminal status, and the response text (from the transport `terminal` observer event, from `superchat-runtime-cancel`, or from the expired deadline).
permitted effect: append `response.completed` (except for a clean cancellation), run Turn hooks, append the terminal `run.completed`/`run.cancelled`/`run.failed` event, mark the entry terminal with `:completed-at`, complete the Turn, then deliver to the caller and run completion hooks; the diagnostic Closure shadow runs last and double-isolated.
invariant: durable terminal facts precede delivery — a delivery failure is warned about but never reopens the Run (`superchat-runtime.el:1130`); if the Closure guard rejects or any append fails, the Run is instead sealed `audit-incomplete` with recovery events journaled (`superchat-runtime.el:1007`); a terminal Run ignores all later transport callbacks.
verification: `test/test-runtime.el:1432` `runtime/closure-terminal-guard-allows-proved-completion`, `:1522` `runtime/closure-terminal-guard-rejects-open-readiness`, `:1581` `runtime/closure-terminal-guard-failure-is-audit-incomplete`, `:1216` `runtime/delivery-failure-does-not-reopen-durable-run`.

### Compact a Session into an Anchor

since: K1
source: `superchat-compact.el:485`; `superchat-compact.el:401`; `superchat-tape.el:377`; `test/test-compact.el:60`

precondition: the Session has durable `user`/`assistant` rows after its latest compaction Anchor, and no compaction Run is already in flight; the model response must end with the completion marker or nothing is written.
input: the Session ID, the collected messages (each carrying its Tape row ID), and the previous Anchor whose source IDs are folded forward.
permitted effect: a background Runtime Run produces a summary, then exactly one typed `anchor` row is appended via `superchat-tape-append-compaction-anchor` recording `source_ids`, `message_count` and the compaction schema; the in-memory `superchat--conversation-history` collapses onto that Anchor while rows recorded during the Run are preserved in front of it.
invariant: every source ID must already exist in the same Session (`superchat-tape-storage-reference-p`) and must be a unique positive integer — no synthetic provenance; no Tape row is ever deleted or rewritten, so `>expand` can restore the sources; a failed or truncated Run leaves history untouched.
verification: `test/test-compact.el:60` `test-compact-session-writes-anchor-and-resets-history`, `:109` `test-compact-session-persists-typed-durable-provenance`, `:262` `test-compact-truncated-summary-leaves-history-alone`, `:338` `test-expand-anchor-restores-source-messages`.

### Write into a Workspace Artifact

since: K1
source: `superchat-workspace.el:839`; `superchat-workspace.el:824`; `superchat-workspace.el:851`; `superchat-workspace.el:899`; `test/test-workspace.el:248`

precondition: a Workspace resolves (or auto-create is on / the fallback buffer is live), the target Artifact is not a published `generated-file`, and a write arriving from Tool Executor context against an existing Artifact must carry an `expected-revision` equal to the Artifact's current revision.
input: the content string, an append flag, an optional artifact ID, the expected revision, and an optional workspace ID.
permitted effect: a buffer-backed Artifact's region is rewritten inside `atomic-change-group` (`superchat-workspace.el:899`); a file-backed Artifact is rewritten as a whole file through `superchat-workspace--write-file-artifact` (`superchat-workspace.el:747`, `with-temp-file`, not transactional). Either way the Artifact's revision and content hash are recomputed, the registry entry is replaced, and a bounded `workspace.artifact.changed` Tape event is emitted carrying only metadata plus old/new revision — never the content.
invariant: the revision is checked twice — once against the caller's optimistic snapshot (`superchat-workspace.el:861`) and again at the commit point (874 for files, 903 inside the buffer's atomic group), so an edit landing between check and mutation is rejected; published `generated-file` Artifacts are immutable and refuse writes outright (851); any error restores the previous Artifact record into the registry (937).
verification: `test/test-workspace.el:248` `test-workspace-tool-write-requires-current-revision`, `:272` `test-workspace-write-rejects-change-after-revision-check`, `:362` `test-workspace-tape-events-omit-content`.

### Define a synthesized Tool

since: K1
source: `superchat-synthesis.el:225`; `superchat-synthesis.el:103`; `superchat-synthesis.el:159`; `test/test-synthesis.el:57`

precondition: the request was built with `superchat--syn-bind` so an in-flight prompt exists (otherwise the placeholder at `superchat-tools.el:904` refuses); the name must match `[a-zA-Z][a-zA-Z0-9_-]{0,63}` and must not already be a built-in or MCP tool name; the user must approve the pretty-printed defun form.
input: name, description, a JSON argument array restricted to string/number/integer/boolean, and an Emacs Lisp body.
permitted effect: the defun is evaluated into the private `superchat-syn--` namespace, wrapped exactly like the run's other tools, injected into the in-flight prompt so the model can call it on the next round, and — only when `persist` is set — added to the session registry, written to the store and anchored to its content hash in the approval ledger.
invariant: a synthesized name may never shadow a built-in or MCP tool, though the agent may redefine its own earlier tool; every synthesized tool is treated as destructive and answers to `superchat-agent-confirm-synthesized` alone, so turning off global destructive confirmation does not extend to code the model just wrote; sub-agents pass `persist` nil so their synthesis dies with their prompt; every failure returns a readable string to the model instead of escaping the turn.
verification: `test/test-synthesis.el:57` `syn/tool-is-callable-in-the-same-run`, `:120` `syn/cannot-shadow-a-builtin-tool`, `:101` `syn/confirmation-does-not-follow-the-global-switch`, `:185` `syn/subagent-synthesis-never-reaches-the-session`.

### Render agent-authored UI

since: K1
source: `superchat-tools.el:989`; `superchat-tools.el:919`; `superchat-tools.el:1224`; `superchat-genui.el:374`; `test/test-genui-render.el:108`

precondition: `render_ui` is attached to the Request — it is not in the default `superchat-llm-tool-names` (`superchat-tools.el:1203`), so it reaches a Run only when a Capability preset, extension, or explicit tool list names it and `superchat--llm-tool-enabled-p` (`superchat-tools.el:1224`) admits it; `superchat-genui` and `textui` must load; and the component must be a string that reads under a restricted reader with `read-eval` and `read-circle` bound to nil (`superchat-tools.el:926`).
input: one s-expression string in the whitelisted grammar — `:text`, `:flex`, `:grid` containers and `button` widgets whose `:action` names a registered action (`cancel-agent`, `send-to-chat`); several top-level forms are auto-wrapped in a column flex rather than rejected.
permitted effect: `superchat-genui-render-form` validates, expands and renders the tree into a dedicated TextUI side-window buffer, returning the buffer name; every rejection or error is returned as a tool-result string.
invariant: validation is pure with respect to the action table and precedes expansion; only action positions receive closures, and those closures are built by the trusted registry from the agent's literal parameters — the agent's own data is never evaluated; depth (128), node budget (10000) and cycles are bounded, and the target buffer can never alias the chat surface.
verification: `test/test-genui.el:40` `genui/rejects-red-team-forms`, `:101` `genui/validation-does-not-call-action-builders`, `test/test-genui-render.el:108` `genui-render/renders-into-textui-buffer`, `:72` `genui-render/never-targets-the-chat-buffer`, `:27` `genui-render/is-default-in-tool-registry`.

## Invariants

### Audit before effect

since: K1
source: `superchat-authority.el:1244`; `superchat-authority.el:1080`; `superchat-authority.el:1332`; `test/test-authority.el:1270`

Every model-invoked tool records its intent durably before it can act: `tool.requested` (plus `approval.requested` and `approval.decided` when the decision is `ask`) is appended in `superchat-tool-execute`, and `tool.execution.started` is appended in `superchat-authority--invoke` before pre-tool hooks or the implementation run. Because a Tape write failure signals, the whole pre-execution sequence is wrapped in a `condition-case` whose handler returns `[Tool call blocked because audit failed: …]` without ever reaching the implementation — the audit gap is a refusal, not a silent execution.

### Authority decides on declared Effects, never on tool names

since: K1
source: `superchat-authority.el:383`; `superchat-authority.el:535`; `superchat-tools.el:1765`; `test/test-authority.el:576`

The base Authority verdict is computed from Effects alone: `superchat-authority-decide` (`superchat-authority.el:383`) composes `superchat-authority--decision-for-effect` over the resolved descriptors — `local-read`, `local-write`, `process-exec`, `runtime-exec`, `external-read`, `external-write`, `unknown` — under the declared attendance, and the tool name enters only as the `:tool` label of the returned plist. This is enforced at registration: `superchat-register-tool` refuses any plugin tool that does not declare `:effects` (`superchat-tools.el:1765`, "Authority never learns tool names"), and a tool with no resolver falls back to `unknown` (`superchat-authority.el:1195`), which is `ask` when attended and `deny` when unattended. Consequently, exposing a tool through an Agent Profile's toolset grants no Authority at all (`test/test-authority.el:576` `authority/profile-toolset-does-not-grant-authority`). The name-independence is a property of the base decision, not of the whole executor: `superchat-authority--effective-decision` (`superchat-authority.el:535`) still consults the tool name through the legacy `superchat--agent-check-permission` hook (`:586`), the session allowlist (`:598`), and the destructive-name list used by `superchat--agent-confirm-p` (`:551`) — so a name can still widen an Effects-derived `ask` to `allow`, and any Kernel claim of pure Effects-based authority must be scoped to `superchat-authority-decide`.

### Effective Authority and guardrails compose tighten-only

since: K1
source: `superchat-authority.el:488`; `superchat-authority.el:570`; `superchat-agent-loop.el:55`; `superchat-subagent.el:852`; `CONTEXT.md:39`

Composition may only narrow what is permitted, matching the project's own definition of Authority as "composed at execution time and can only be tightened by delegation". A plugin-registered policy returning `allow` is downgraded to `defer` (`superchat-authority--policy-decision`); built-in policy names cannot be replaced; an unattended Run's side effects are denied regardless of what a compatibility hook says; a Preset may not raise `max_tool_calls` above the global limit nor disable global destructive confirmation (it is warned and clamped by `min`/`or`); and a sub-agent context takes the `min` of parent and profile tool budgets, inherits the parent's `confirm-destructive` when set, and inherits the parent's Authority policy and attendance rather than choosing its own.

### The Tape is append-only and Anchors never delete

since: K1
source: `superchat-db.el:378`; `superchat-tape.el:377`; `superchat-db.el:421`; `superchat-tools.el:1061`; `test/test-compact.el:338`

The only statement that touches the `tape` table's contents is the single `INSERT INTO tape (session_id, topic, kind, content, meta) VALUES (?, ?, ?, ?, ?)` in `superchat-db-tape-append` (`superchat-db.el:378`); no UPDATE or DELETE against `tape` exists anywhere in the tree (only `tape_fts` index maintenance at `superchat-db.el:218,300-311`). The model-facing `sql` surface — `superchat-tool-sql` (`superchat-tools.el:1061`) over `superchat-db-tape-select` (`superchat-db.el:411`) — refuses anything not beginning `SELECT`/`WITH` (417-418), word-scans for mutating verbs so a `WITH … INSERT` cannot slip through (`superchat-db.el:421`), and rejects multi-statement queries (426). Compaction therefore adds a typed `anchor` row naming its unique, already-existing source rows (`superchat-tape.el:377`) rather than replacing them, which is exactly why `>expand` can restore the original Entries afterwards (`test/test-compact.el:338`).

### A Run cannot be sealed while an approval or effect is unaccounted

since: K1
source: `superchat-runtime.el:81`; `superchat-runtime.el:795`; `superchat-closure.el:100`; `test/test-runtime.el:1522`

The Closure terminal guard is enabled by default and runs inside `superchat-runtime--finish` before the terminal event is appended: it replays the Session's Tape, asks `superchat-closure-judge` for `ready-terminal`, and signals `superchat-closure-terminal-not-ready` unless the verdict is `proved` for exactly that terminal kind. Readiness requires that every recorded Approval carries an allow/deny disposition and every recorded Effect is `discharged`, `denied`, `blocked`, `failed` or `cancelled`; a rejection is caught and the Run is sealed `audit-incomplete` instead, so an open effect can never be papered over as a clean completion — not even by cancellation.

### Agent-authored UI is data and is never evaluated

since: K1
source: `superchat-tools.el:926`; `superchat-genui.el:295`; `superchat-genui.el:277`; `test/test-genui.el:40`

The component string is read with `read-eval` and `read-circle` bound to nil, then validated against a closed grammar (three container types, one widget type, two registered actions) before anything is expanded; validation never calls an action builder. Expansion copies literal nodes and substitutes trusted closures built by the action registry from validated literal parameters — no branch of `superchat-genui--expand-node` or `superchat-genui-render-form` evaluates any form the model supplied, and `superchat-genui-render-form` is the single trusted entry so no caller can reach a mid-chain function and skip validation.

### A synthesized tool may not shadow a built-in or MCP tool name

since: K1
source: `superchat-synthesis.el:103`; `superchat-synthesis.el:91`; `superchat-synthesis.el:117`; `test/test-synthesis.el:120`

`superchat--syn-check-name` refuses outright any name that already exists among the built-in tool list or the MCP tool set while not itself being an earlier synthesis — redefining the agent's own tool is allowed, capturing someone else's name is not. The check is also re-applied on rehydration, so a persisted tool whose name later became a built-in is skipped rather than loaded, and synthesized functions live in their own `superchat-syn--` symbol namespace so they cannot collide with anything Superchat or the user defined.

## Architecture boundaries

- Tool permission decision — authority: `superchat-authority.el` | owned
- Tool effect declaration — authority: `superchat-tools.el` | owned
- Run state machine and deadline — authority: `superchat-runtime.el` | owned
- Turn identity and prepare pipeline — authority: `superchat-core.el` | owned
- Durable history append discipline — authority: `superchat-tape.el` | owned
- SQLite schema and migrations — authority: `superchat-db.el` | owned
- Model transport — authority: `superchat-llm.el` | source-backed
- MCP server lifecycle — authority: `superchat-mcp.el` | source-backed
- Model-facing tool surface — authority: `superchat-tools.el` | owned
- Prompt assembly — authority: `superchat-prompt-hooks.el` | owned
- Preset parsing and application — authority: `superchat-preset.el` | owned
- Skill resolution and loading — authority: `superchat-skills.el` | owned
- Preset guardrail tightening and tool-call budget — authority: `superchat-agent-loop.el` | owned
- Agent-authored UI validation — authority: `superchat-genui.el` | owned
- Projection registry and rebuild — authority: `superchat-projection.el` | owned
- Markdown-to-Org rendering — authority: `superchat-render.el` | owned
- Model-visible context envelope — authority: `superchat-tape-view.el` | derived
- Closure judgment — authority: `superchat-closure.el` | derived
- Chat buffer rendered transcript — authority: `superchat-render.el` | derived
- Header-line run and context gauge — authority: `superchat.el` | derived

## Source evidence

### Scan basis

since: K1
source: `test/run-tests.el`; `docs/spec-agents/check-kernel.sh`

Every entry above was derived by reading the working tree on 2026-08-26 and was then
re-checked line by line by an independent pass that opened each cited path and confirmed
the code is reached at runtime.  The suite state at bootstrap was 895 tests, 879 results as
expected, 0 unexpected, 16 skipped (`emacs -Q --batch -L . -l test/run-tests.el`).  The
authority map below passes the form check in `docs/spec-agents/check-kernel.sh`.

### Excluded from K1

since: K1
source: `ROADMAP.md`; `docs/adr/0002-lazy-buffer-derived-workspaces.md`; `docs/adr/0003-progressive-disclosure-for-capabilities.md`; `docs/goals/homoiconic-agent.md`

`ROADMAP.md`, everything under `docs/goals/`, and ADRs 0002 and 0003 (both `status: proposed`)
describe intent, not enacted semantics, and contribute no entry.  Code that is defined but
never registered or called is likewise excluded: `superchat-tool-read-file`,
`superchat-tool-write-file`, `superchat-tool-list-files`, the implicit skill matcher in
`superchat-skills.el`, `superchat-skills-standard.el`, `superchat-preset-from-frontmatter`,
and `superchat-command-hooks` (declared, zero producers).
