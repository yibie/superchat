# cowork-telega-session-bundle — Work Plan

Status: Implemented and verified on 2026-07-28.

## TL;DR

- **What you'll get**: `plugins/superchat-cowork-telega/` becomes a bundle whose
  primary experience is one persistent, contextual Cowork Session; existing
  reply/digest/triage commands remain compatible while becoming secondary.
- **Approach**: preserve the proven tools, Authority effects, Tape events, and
  legacy entrypoints; move the existing monolith behind a compatibility module;
  add one deep Session module and one packaged `SKILL.md`.
- **What it will not do**: no core Runtime/Dispatcher change, no D-Bus, no
  background runs, no generic Surface abstraction, and no pretend “dynamic
  capability” implemented by exposing every tool at once.
- **Decisions**: the first Session is read-only at model-tool level
  (`telega_read_messages` + `memory_search`), supports multi-turn understanding,
  decisions, and drafts, and hands drafts back to Telega. Existing audited send
  and rule flows remain available during migration.

## Product Contract

The primary user result is:

> Stay in the current Telegram work context, understand what happened, decide
> what to do, and complete communication while retaining final control.

The initial vertical slice must support:

1. Start from the current Telega chat, with an optional selected message.
2. Ask a natural-language question without choosing reply/digest/triage.
3. Continue asking follow-up questions in the same Session.
4. Grant additional chat read scopes when needed.
5. Receive conclusions, next-step advice, or a draft in a sidecar Session view.
6. Put the latest answer into the originating Telega input area for editing.
7. Keep existing audited send, digest, and rule commands working.

## Cleanup Scope

### Smells being removed

- **Boundary violation**: one file owns Telega adapters, tools, rules, profiles,
  requests, commands, and product UX.
- **Role-first interface**: users must choose participant, analyst, or
  gatekeeper before expressing a goal.
- **Missing state**: every run uses an ephemeral buffer, so follow-up context is
  discarded.
- **Embedded resource**: workflow guidance is hard-coded inside Lisp strings
  instead of living as a bundle resource.

### Explicitly retained

- Existing tool functions and effect declarations.
- Existing Authority previews, policies, validators, and Tape events.
- Existing public command names and their tested behavior.
- The optional-plugin rule: core never eagerly requires this bundle.

## Deep Module

The new Session module exposes only:

```elisp
(superchat-cowork-telega-session-open chat origin-buffer &optional context)
(superchat-cowork-telega-session-submit session-buffer instruction)
(superchat-cowork-telega-session-grant-chats session-buffer chats)
```

Everything else—history bounds, prompt construction, scope approval,
request construction, rendering, and running-state control—is implementation.
Tests exercise behavior through this interface.

## Bundle Layout

```text
plugins/superchat-cowork-telega/
├── superchat-cowork-telega.el          bundle loader and packaged skill loader
├── superchat-cowork-telega-session.el  canonical Cowork Session
├── superchat-cowork-telega-legacy.el   existing proven tools and compatibility flows
├── skills/
│   └── cowork-telegram/
│       └── SKILL.md                    workflow guidance
├── test-cowork-telega.el
└── DESIGN.md
```

No script is added. There is no deterministic preprocessing task that needs
one; Telega APIs and existing Lisp helpers already cover the work.

## Task Dependency Graph

| Task | Depends On | Blocks | Reason |
|---|---|---|---|
| 1. Lock bundle/session behavior | None | 2, 3 | Refactor needs observable contracts |
| 2. Turn the monolith into a bundle | 1 | 3, 4 | Session needs a stable bundle loader |
| 3. Add the Cowork Session | 1, 2 | 4, 5 | Primary product behavior |
| 4. Reframe compatibility and docs | 2, 3 | 5 | Document actual behavior |
| 5. Verify and simplify | 3, 4 | None | Completion evidence |

## Execution Waves

```text
Wave 1:
└── Task 1: regression tests

Wave 2:
└── Task 2: bundle loader + legacy move + SKILL.md

Wave 3:
└── Task 3: Cowork Session implementation

Wave 4:
├── Task 4: compatibility/docs
└── Task 5: verification and diff simplification
```

The files overlap, so implementation remains single-owner and sequential.

## Tasks

### Task 1: Lock bundle and Session behavior

- **What**:
  - Make public/private-symbol and no-D-Bus tests scan every bundle `.el` file.
  - Add failing tests for stable Session reuse, multi-turn prompt history,
    read-only model tools, scope grants, out-of-scope rejection, running-state
    protection, and draft handoff.
- **Agent**: `test-engineer`
- **Skills**: `ai-slop-cleaner`, `karpathy-guidelines`
- **Depends**: None
- **Acceptance**:
  - Existing 24 plugin tests remain unchanged in meaning.
  - New tests fail only because Session/bundle behavior is not implemented.
- **QA**:
  - Happy: plugin ERT command reaches only the new expected failures.
  - Failure: hostile selected text cannot alter tools or read scopes.
- **Commit**: only if requested; use the repository Lore protocol.

### Task 2: Turn the monolith into a bundle

- **What**:
  - Move the current implementation unchanged to
    `superchat-cowork-telega-legacy.el`.
  - Replace the original file with a small loader.
  - Load packaged skill guidance from
    `skills/cowork-telegram/SKILL.md`.
  - Preserve feature and command loading compatibility.
- **Agent**: `executor`
- **Skills**: `codebase-design`, `ai-slop-cleaner`
- **Depends**: Task 1
- **Acceptance**:
  - `(require 'superchat-cowork-telega)` still loads every existing command.
  - Existing plugin ERT returns to green before Session behavior is added.
  - No production behavior is duplicated between loader and legacy module.
- **QA**:
  - Happy: existing 24 plugin tests pass.
  - Failure: loading the legacy module alone does not claim the bundle feature.
- **Commit**: only if requested; use the repository Lore protocol.

### Task 3: Add the Cowork Session

- **What**:
  - Add a persistent sidecar Session buffer keyed by originating chat.
  - Store bounded user/assistant history in the Session implementation.
  - Build each Runtime Request from origin, selected context, granted scopes,
    prior turns, and current instruction.
  - Expose only read + memory tools to the model in this first slice.
  - Use request-local scope approval and validator logic to keep reads inside
    the current or explicitly granted chat set.
  - Provide commands for follow-up, adding scopes, and inserting the latest
    answer into the originating Telega input area.
  - Make `M-x superchat-cowork-telega` the canonical command and retain the
    old one-shot behavior as `M-x superchat-cowork-telega-reply`.
- **Agent**: `executor`
- **Skills**: `codebase-design`, `karpathy-guidelines`
- **Depends**: Tasks 1 and 2
- **Acceptance**:
  - The same chat reuses one Session and follow-ups include prior turns.
  - Different chats do not share Session history or scopes.
  - No model turn can send or write rules from the Session request.
  - Scope refusal or invalid chat ID causes no Runtime submission/tool effect.
  - The latest answer can be handed to the original Telega input area.
- **QA**:
  - Happy: mocked two-turn Session includes turn one in turn two’s prompt.
  - Failure: concurrent submit, unknown scope, and dead origin buffer fail
    clearly without external effects.
- **Commit**: only if requested; use the repository Lore protocol.

### Task 4: Reframe compatibility and docs

- **What**:
  - Mark reply/digest/triage as compatibility/specialized commands.
  - Rewrite `DESIGN.md` around the Product Contract and Cowork Session.
  - Document the bundle layout and the deliberate read-only ceiling.
- **Agent**: `writer`
- **Skills**: `codebase-design`
- **Depends**: Tasks 2 and 3
- **Acceptance**:
  - Docs no longer describe three roles as the product interface.
  - Docs clearly distinguish the canonical Session from legacy actions.
  - D-Bus/background behavior remains explicitly out of scope.
- **QA**:
  - Command names, key bindings, feature names, and file names match code.
  - `rg` finds no primary-product claim based on “three roles.”
- **Commit**: only if requested; use the repository Lore protocol.

### Task 5: Verify and simplify

- **What**:
  - Run plugin ERT, main ERT, check-parens, checkdoc/static scans.
  - Inspect the diff and delete any pass-through helper or speculative option.
- **Agent**: `verifier`
- **Skills**: `ai-slop-cleaner`, `karpathy-guidelines`
- **Depends**: Tasks 3 and 4
- **Acceptance**:
  - All relevant tests pass with zero unexpected failures.
  - No new dependency or core modification.
  - No private `superchat--` reference, D-Bus dependency, blocking Runtime
    submit, or direct llm call appears in the bundle.
- **QA**:
  - Plugin ERT and main ERT commands below.
  - Static scans below.
- **Commit**: only if requested; use the repository Lore protocol.

## Verification

```sh
rtk test emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
  -l plugins/superchat-cowork-telega/test-cowork-telega.el \
  -f ert-run-tests-batch-and-exit

rtk test emacs -Q --batch -L . -l test/run-tests.el

rtk grep "superchat--" plugins/superchat-cowork-telega
rtk grep "dbus|notifications-notify|telega-notifications-msg-temex" \
  plugins/superchat-cowork-telega
rtk grep "superchat-runtime-submit-blocking|llm-chat|superchat--llm-generate-answer" \
  plugins/superchat-cowork-telega
```

## Success Criteria

- [x] `M-x superchat-cowork-telega` starts or reuses a contextual Session.
- [x] Users can ask at least two consecutive questions without restarting a
      workflow or losing prior context.
- [x] Read scope is explicit, persistent for that Session, and auditable.
- [x] The model-facing Session tool surface cannot send or modify rules.
- [x] The latest answer can return to the originating Telega input area.
- [x] Existing audited reply/digest/triage commands still pass their tests.
- [x] Plugin code is a bundle, not one monolithic `.el` file.
- [x] Full relevant verification is green.

## Deferred Trigger

Add true conversational capability escalation only after the public
request-validator seam can distinguish tool names, or after a second real
plugin needs the same mechanism. Until then, do not emulate it with an
all-tools request and a prompt.
