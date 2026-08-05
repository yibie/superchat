# Superchat Phase 3 — Runtime Configuration Orthogonalization

## TL;DR

- **What you'll get:** Capability, Topology, Authority Policy, Surface, and
  Workspace Binding become independently selectable Runtime inputs and
  recoverable Session state.
- **Approach:** Keep Preset as the Capability configuration, add only the
  missing buffer-local overlays, record configuration changes in the existing
  Tape, and resolve everything into the existing Runtime Request seam.
- **Not included:** A new capability module, a general Projection framework,
  Workspace 2.0 artifacts, resumable workflows, external Cowork integrations,
  or OS sandboxing.
- **Decisions:** `/team` becomes a Topology overlay and never replaces the
  active Capability. Named Authority policies are tighten-only in this phase.
  Surface is request-derived rather than a sticky user setting. Existing
  `superchat--active-preset` remains the Capability cache for compatibility.
  No commit is created unless explicitly requested.

GNU Emacs buffer-local variables remain disposable caches; Tape is used to
reconstruct logical Session configuration when a Session View is reopened.
This avoids relying on `permanent-local`, which is not intended for ordinary
editing state:

- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer_002dLocal-Variables.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffer_002dLocal.html>

## Scope

### In

- Buffer-local Capability, Topology, Authority Policy, Workspace Binding, and
  last Surface cache.
- Tape events for Capability, Topology, Authority, and Workspace changes.
- A replay view that derives the latest Session configuration from Tape.
- `/code`, `/research`, `/cowork`, `/team`, `/single`, `/authority`, and
  `/workspace` command semantics.
- Compatibility migration for `/agent`, `/plan`, `/skill`, explicit agent
  skills, `/team [directory]`, and `/reset`.
- Team tool/system-prompt overlay without replacing the Capability Preset.
- Runtime Request metadata and Tool Executor context propagation.
- Parent-to-Subagent tighten-only Authority/Workspace inheritance.

### Out

- Trigger and Workflow state machines.
- Per-step Workflow topology execution.
- New external tools or Integration Packages.
- Artifact IDs/adapters beyond a directory Workspace binding.
- General materialized projections/checkpoints.
- Allow policies that broaden existing Authority decisions.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Session configuration events and replay | None | 2, 4 | Commands need one durable state path |
| 2. Capability and Topology overlays | 1 | 5 | Delivers `/code` + `/team` composition |
| 3. Named tighten-only Authority policies | None | 4, 5 | Runtime needs a validated policy reference |
| 4. Runtime, Surface, Workspace, and Subagent propagation | 1, 3 | 5 | Makes configuration effective during execution |
| 5. Compatibility and architecture verification | 2, 4 | None | Proves independent switching and restoration |

## Parallel Execution Waves

```text
Wave 1:
├── Task 1: Session configuration events and replay
└── Task 3: Named tighten-only Authority policies

Wave 2:
├── Task 2: Capability and Topology overlays
└── Task 4: Runtime/Surface/Workspace/Subagent propagation

Wave 3:
└── Task 5: compatibility, cleanup, full verification
```

## Tasks

### Task 1: Session configuration events and replay

- **What:** Add buffer-local configuration caches in `superchat.el`; append
  `capability.activated`, `topology.changed`,
  `authority.policy.selected`, `workspace.bound`, and `workspace.unbound`
  through `superchat-tape-append-event`; derive the latest configuration from
  event envelopes in `superchat-tape-view.el`; restore it when rendering a
  persisted Session View.
- **Agent:** `executor` — implementation and focused ERT.
- **Files:** `superchat.el`, `superchat-tape-view.el`,
  `test/test-tape-view.el`.
- **Acceptance:** Reopening a Session restores all selected axes; a branched
  Session inherits parent configuration through its branch point; a missing
  or removed Capability falls back safely to chat.
- **QA:** Focused Tape View ERT plus malformed/missing event payload cases.
- **Commit:** None unless requested.

### Task 2: Capability and Topology overlays

- **What:** Keep Preset as Capability; add compact builtin fallbacks for code,
  research, and cowork; route all activation paths through one helper; make
  `/team` set only Topology; add `/single`; merge team coordination tools and
  instructions into each request without replacing Capability tools/persona.
- **Agent:** `executor` — command and request composition changes.
- **Files:** `superchat.el`, `superchat-dispatcher.el`,
  `test/test-skills.el`, `test/test-native-ui.el`.
- **Acceptance:** `/code` followed by `/team` retains the code Preset and
  produces `Capability=code`, `Topology=team`; `/single` removes only the
  topology overlay; old agent/skill activation still works.
- **QA:** Command ERT and Request Plan boundary ERT.
- **Commit:** None unless requested.

### Task 3: Named tighten-only Authority policies

- **What:** Validate `default`, `observer`, `developer-project`, `operator`,
  and `cowork` policy references. Compose their constraints into the existing
  Effect decision path. Include the policy identity in the approval policy
  version. Unknown policies fail closed.
- **Agent:** `executor` — security-sensitive policy implementation.
- **Files:** `superchat-authority.el`, `test/test-authority.el`.
- **Acceptance:** `observer` denies writes/exec; `developer-project` denies
  external effects and file effects outside its Workspace root; no named
  policy can turn an existing `ask` or `deny` into `allow`; unknown policies
  never execute tools.
- **QA:** Focused Authority ERT covering attended/unattended, inside/outside
  roots, unknown policies, and approval hashes.
- **Commit:** None unless requested.

### Task 4: Runtime, Surface, Workspace, and Subagent propagation

- **What:** Attach configuration to Request Plans; let Runtime normalize
  defaults, record a redacted configuration summary with `intent.received`,
  and pass Authority/Workspace metadata to the Tool Executor. Derive Surface
  from `origin.surface`, then `origin.major-mode`, then `superchat`. Bind a
  directory Workspace to `superchat-tools-default-directory`. Propagate the
  parent Authority and Workspace unchanged into child Runs.
- **Agent:** `executor` — Runtime and delegation integration.
- **Files:** `superchat-dispatcher.el`, `superchat-runtime.el`,
  `superchat-subagent.el`, `superchat-workflow.el`,
  `test/test-runtime.el`, `test/test-subagent.el`,
  `test/test-workflow.el`.
- **Acceptance:** Runtime status/event context carries every axis; Surface
  cannot elevate Authority; Subagents inherit or tighten but cannot replace
  the parent policy/root.
- **QA:** Runtime, Subagent, and Workflow focused ERT.
- **Commit:** None unless requested.

### Task 5: Compatibility and architecture verification

- **What:** Update old `/team` tests, remove only obsolete Team-Preset
  assumptions, check all request paths, and run canonical verification.
- **Agent:** `verifier` — independent evidence review after implementation.
- **Files:** Evidence-driven fixes only.
- **Acceptance:** Existing Preset/Tape data remains readable; old command
  aliases work; axes switch independently; no new direct model/tool bypass
  exists.
- **QA:** `emacs -Q --batch -L . -l test/run-tests.el`, focused ERT,
  check-parens, byte compilation, static searches, and `git diff --check`.
- **Commit:** None unless requested.

## Success Criteria

- [x] `/code` changes Capability only.
- [x] `/team` changes Topology only.
- [x] `/authority` changes Authority Policy only.
- [x] `/workspace` changes Workspace Binding only.
- [x] Surface is derived per Request and recorded without region contents.
- [x] Configuration is restored from Tape when reopening a Session.
- [x] Team overlay preserves Capability persona/model/tools.
- [x] Child Runs cannot broaden parent Authority or Workspace scope.
- [x] Existing `/agent`, `/plan`, `/skill`, `/team [directory]`, and `/reset`
      remain compatible.
- [x] Canonical ERT has zero unexpected failures.
- [x] Touched modules byte-compile without new warnings.
- [x] Static checks find no execution or Tool Executor bypass.
