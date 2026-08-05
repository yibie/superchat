# Superchat Phase 5 — Workspace 2.0

## TL;DR

- **What you'll get:** The existing marker region becomes a named
  `buffer-region` Artifact inside the Session-bound Workspace. Workspaces can
  hold multiple Artifact references, Team child runs resolve the same
  Workspace by ID, writes support optimistic revision checks, and Artifact
  lifecycle facts are replayable from Tape.
- **Approach:** Deepen `superchat-workspace.el` in place. Replace the one
  process-global marker pair with a registry keyed by the already-existing
  Workspace ID, keep the current public functions as compatibility wrappers,
  and add only the concrete `buffer-region` implementation.
- **What it will NOT do:** No Artifact table, generic adapter registry,
  background projector, copied file contents, file/Org/Git adapters, or
  envelope index columns.
- **Decisions:** Region content SHA-256 is the revision, so edits outside the
  region do not create false conflicts. Buffer edits and their Artifact event
  append share an `atomic-change-group`; a failed event append rolls back the
  edit. Persisted references whose buffers are gone remain visible but
  unresolved until the user binds a live region again.

GNU Emacs marker and buffer semantics used by this phase:

- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Overview-of-Markers.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Marker-Insertion-Types.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Killing-Buffers.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Narrowing.html>

## Scope

### In

- Workspace registry keyed by the existing Session `workspace-id`.
- Multiple `buffer-region` Artifact references per Workspace.
- Artifact ID, URI, type, adapter, role, revision, content hash, and live
  location metadata.
- Existing fallback buffer represented by the same `buffer-region`
  implementation.
- Targeted read/write/show/remove while preserving no-argument compatibility.
- Optimistic `expected_revision` checks for tool-driven writes.
- `workspace.artifact.added`, `.changed`, and `.removed` Tape events without
  copying Artifact content.
- A pure registered Workspace Artifact projection over those events.
- Session bind/restore registration and Team/subagent context reuse.
- Workspace tools in the built-in Capability/subagent toolsets that need them.

### Out

- File, Org heading, Git revision, URL, or generated-file adapters.
- A generic adapter registration interface with one implementation.
- A second Workspace/Artifact database or materialized projection.
- Reattaching dead buffer regions automatically after restart.
- OS sandbox claims or new Authority policy levels.
- Commits; the existing dirty worktree remains intact.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Artifact module and compatibility | None | 2, 3 | Own the state and marker semantics once |
| 2. Tape projection and conflict safety | 1 | 3, 4 | Makes references durable and writes auditable |
| 3. Session, Toolset, Authority, and Team integration | 1, 2 | 4 | Connects existing Runtime context to the module |
| 4. Verification and documentation | 1, 2, 3 | None | Locks compatibility and Phase 5 invariants |

## Execution Waves

```text
Wave 1:
└── Task 1: Artifact module and compatibility

Wave 2:
├── Task 2: Tape projection and conflict safety
└── Task 3: Integration (after Task 1 interface is fixed)

Wave 3:
└── Task 4: Canonical verification and focused fixes
```

## Tasks

### Task 1: Artifact module and compatibility

- **What:** Replace the singleton marker pair in `superchat-workspace.el` with
  Workspace records containing Artifact plists. Keep
  `superchat-workspace-set-region`, `clear`, `show`, `read`, `write`, and
  `info`; add targeted optional arguments plus an additive region command.
  Preserve marker insertion types, trimming, newline normalization, fallback
  behavior, read-only override, and narrowing safety.
- **Files:** `superchat-workspace.el`, `test/test-workspace.el`.
- **Agent:** `executor` — one owner for the deep module and its interface tests.
- **Acceptance:** Existing nine Workspace tests remain green; two Workspace
  IDs are isolated; one Workspace can hold and target multiple regions; killed
  buffers become unresolved rather than silently redirecting to another
  Workspace.
- **QA:** Focused ERT for `test/test-workspace.el`, including invalid Artifact
  IDs and killed-buffer failure paths.

### Task 2: Tape projection and conflict safety

- **What:** Emit bounded Artifact lifecycle events through
  `superchat-tape-append-event`, register a pure Workspace reducer, and rebuild
  reference metadata by Workspace ID from `topic = workspace`. Require a
  matching revision for writes issued inside Tool Executor context when the
  target already exists. Keep buffer mutation rollback-safe if auditing fails.
- **Files:** `superchat-workspace.el`, `test/test-workspace.el`.
- **Agent:** `executor` — same module owner avoids splitting one invariant.
- **Acceptance:** Tape payloads contain URI/revisions but not content; replay
  reconstructs add/change/remove state; stale revisions fail before mutation;
  audit failure leaves buffer content unchanged.
- **QA:** Focused ERT for event capture, replay, stale revision, and injected
  append failure.

### Task 3: Session, Toolset, Authority, and Team integration

- **What:** Ensure `/workspace` bind and Session restore register the logical
  Workspace. Resolve workspace tools from `superchat--tool-execution-context`.
  Add optional Artifact/revision arguments and Artifact-scoped effects. Make
  built-in Coding/Research/Planning and subagent presets expose only the
  appropriate read/write Workspace tools; keep Topology independent.
- **Files:** `superchat.el`, `superchat-tools.el`,
  `superchat-authority.el`, `superchat-subagent.el`,
  `test/test-skills.el`, `test/test-authority.el`,
  `test/test-subagent.el`.
- **Agent:** `executor` — integration is narrow but crosses the existing
  Runtime configuration seam.
- **Acceptance:** Session A and Session B no longer share scratchpad state;
  child runs with the same Workspace ID see the same Artifact set; Authority
  approval scopes can distinguish Artifact IDs; `/code` plus `/team` does not
  replace either configuration axis.
- **QA:** Focused Authority, Session-command, and Subagent ERT suites.

### Task 4: Verification and documentation

- **What:** Update module/tool descriptions and the bilingual README only
  where behavior changed. Run focused suites, canonical ERT, `check-parens`,
  byte compilation, architectural searches, and whitespace checks.
- **Files:** Evidence-driven documentation/fixes only.
- **Agent:** `verifier` — independent completion evidence.
- **Acceptance:** Zero unexpected ERT failures; no direct model/tool bypass;
  no singleton Workspace marker state remains; no generated `.elc` files.
- **QA:** Canonical `test/run-tests.el`, byte compilation of touched modules,
  `git diff --check`, and searches for obsolete marker ownership.

## Success Criteria

- [x] Workspace identity comes from Session/Run context, not one global marker.
- [x] Multiple Artifact references can coexist in one Workspace.
- [x] Existing region/fallback behavior remains compatible.
- [x] Artifact metadata contains ID, URI, type, adapter, role, revision/hash.
- [x] Stale expected revisions cannot overwrite newer content.
- [x] Artifact lifecycle is replayable from Tape without copied content.
- [x] Dead buffers remain explicit unresolved references.
- [x] Team members with the same Workspace ID resolve the same artifacts.
- [x] Capability controls Workspace tool access; `/team` remains only Topology.
- [x] Focused and canonical ERT suites have zero unexpected failures.

## Verification Result

- Focused Workspace ERT: 23/23 passed.
- Canonical ERT: 470 total, 456 passed, 14 expected skips, 0 unexpected.
- Byte compilation completed without new Phase 5 warnings.
- `check-parens` and `git diff --check` passed; no `.elc` files remain.
- Deliberate ceiling: Artifact replay scans `topic = workspace` events and
  filters by Workspace ID in Lisp. Add envelope index columns only after
  measured replay latency warrants the schema change.
