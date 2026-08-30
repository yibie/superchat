# mcp-authority-integration

status: confirmed
revision: 2
context_refs:
  - `KERNEL.md` — Concepts/Tool, Concepts/Effect, "Base Authority Decision
    --folds--> resolved Effects under Attendance",
    the tool-execution lifecycle entry, and the authority-map line
    `MCP server lifecycle — authority: superchat-mcp.el | source-backed`
  - `CONTEXT.md` — Authority, Capability, Actor, Run
  - `.scratch/start/REPORT.md` — START review, Conflicts item 2
  - `docs/spec-agents/single-authority.md`
  - upstream `mcp.el` working copy at `../mcp.el` (sibling checkout;
    also installed via straight at `~/.config/nova-emacs/straight/build/mcp/`)

## Problem and goal

Superchat advertises MCP as a stable capability (`README.md:352`, `README_cn.md:333`),
but the MCP tool surface is non-functional against every published `mcp.el`.
Four defects compound, each verified against the working tree and the sibling
`mcp.el` checkout:

1. **Wrong declared signature.** `superchat-mcp.el:23` declares
   `(declare-function mcp-hub-get-all-tool "mcp-hub" (asyncp categoryp errorHandle))`.
   No such signature has ever existed. The real one is
   `(cl-defun mcp-hub-get-all-tool (&key asyncp categoryp))` (`../mcp.el/mcp-hub.el:80`).
2. **Wrong call.** `superchat-mcp.el:235` calls `(mcp-hub-get-all-tool nil t nil)`
   positionally against that `&key` signature, so every server raises
   "Keyword argument nil not one of (:asyncp :categoryp)" and
   `superchat-mcp-get-tools` returns nil.
3. **Redundant per-server loop.** `mcp-hub-get-all-tool` already `maphash`es every
   connected server itself (`../mcp.el/mcp-hub.el:97-115`). Superchat calls it once
   per active server (`superchat-mcp.el:230-243`), so a repaired call would return
   the full tool set N times. The per-server filter it uses to compensate keys on
   `:server` (`superchat-mcp.el:239-240`), a key `mcp.el` never sets; the key it does
   set is `:category "mcp-<server>"`, and only when called with `:categoryp t`.
4. **Wrong value type.** `mcp-make-text-tool` returns a gptel-style plist
   — `(:function :name :description :args :async)`, docstring: "Create a `gptel' tool"
   (`../mcp.el/mcp.el:918-960`) — not an `llm-tool` cl-struct.
   `superchat-llm.el:1288-1290` maps the Authority wrapper over the combined tool
   list, and the wrapper calls `llm-tool-name`, which signals
   `wrong-type-argument llm-tool` on a plist.

Three `/mcp-*` commands in the same file are also broken:
`superchat.el:1280-1281` dispatch `superchat-mcp-start-server` and
`superchat-mcp-stop-server` with zero arguments, but both require `SERVER-NAME`
(`superchat-mcp.el:127`, `:154`) — guaranteed `wrong-number-of-arguments`.
`superchat.el:1282` dispatches `superchat-mcp-check-all-health`
(`superchat-mcp.el:196`), which returns an alist rather than a string, so the
dispatcher yields `:noop` and nothing is shown.

**Not a security bypass.** An earlier characterisation of this as "MCP bypasses
Authority and Tape" was wrong and is corrected here. `superchat-llm.el:1288-1290`
wraps the *combined* list on the ordinary request path, MCP tools included; a
prepared subagent uses its separate subagent wrapper. Both routes enter
`superchat-tool-execute`, so the design already places MCP inside the shared
executor invariant. Defect 4 would crash the request rather than silently admit
an unaudited call, and defect 2 means execution never reaches that point at all.
The `KERNEL.md` invariant "every model-invoked tool call passes through
`superchat-tool-execute`" therefore holds today — vacuously, for MCP.
This is a functionality defect, and the goal is to make the path work *while
staying inside* that invariant rather than to close an open hole.

**Collateral.** `superchat--syn-check-name` (`superchat-synthesis.el:96`) prevents a
model-synthesized tool from taking an MCP tool's name by consulting
`(ignore-errors (superchat-mcp-get-tools))`. Because that call returns nil today —
and `ignore-errors` swallows defect 2 — the guard is currently vacuous. Repairing
MCP restores it. `KERNEL.md` records this guard as an enacted invariant
("A synthesized tool may not shadow a built-in or MCP tool name"), so the repair
also makes that entry true in fact rather than only in code.

**Goal.** MCP tools join the same registered model surface as plugin tools and
follow the same topology-selected wrapper routes, carrying declared Effects,
Authority approval and Tape audit, with test coverage that fails if the call
form drifts from `mcp.el` again.

## Unchanged contracts

`do` must not disturb any of these:

1. **The base fold receives declared Effects.** Every MCP tool declares `:effects`.
   Given the resolved snapshot, the base Authority fold uses Effect kind and
   Attendance rather than branching directly on the tool name. Resolver selection
   and effective-policy compatibility paths remain name-sensitive; no bypass of
   effect resolution may be introduced for MCP.
2. **One executor, topology-selected wrappers.** MCP tools use the same routes as
   plugin tools: ordinary, workflow, and team-lead Runs apply
   `superchat--agent-wrap-function`; prepared subagent Runs apply
   `superchat--subagent-wrap-function`. Both call `superchat-tool-execute`. A third,
   MCP-specific wrapper is a contract violation even if it appears to work.
3. **Unregistration is complete.** Stopping a server goes through
   `superchat-unregister-tool` (`superchat-tools.el:1783`), which already removes the
   effect resolver and the approval preview. No dangling resolver may survive a stop.
4. **The default tool surface is unchanged.** `superchat-llm-tool-names` keeps its
   five-verb default; MCP tools appear only when tools are attached at all.
5. **`superchat-mcp-servers` keeps its type and meaning** — `(repeat string)`, a
   session-level server allowlist, nil meaning "all configured servers".
6. **Test baseline does not regress.** `emacs -Q --batch -L . -l test/run-tests.el`
   is currently 895 tests, 879 as expected, 0 unexpected, 16 skipped.

## Decision and boundaries

- **Registration replaces per-request collection.** MCP tools are registered with
  `superchat-register-tool` when a server connects and removed with
  `superchat-unregister-tool` when it stops. `superchat-mcp-get-tools` stops being
  the surface path.
- **`superchat-llm.el:782-784` is deleted.** That direct `append` of `mcp-tools`
  into the request tool list is the line that puts MCP tools on the surface outside
  the allowlist mechanism; once registration owns exposure it must not remain.
  Note `superchat-synthesis.el:96` also calls `superchat-mcp-get-tools`; the shadow
  guard must keep working, so the function is retained or replaced by an equivalent
  reader over the registry — `do` chooses, but the guard's behaviour is fixed by
  the invariant below.
- **One call, not N.** `mcp-hub-get-all-tool` is called **once** with
  `:categoryp t`; server attribution comes from each tool's `:category`
  (`"mcp-<server>"`), not from `:server`. The wrong `declare-function` at
  `superchat-mcp.el:23` is corrected to the real `&key` arglist.
- **Effects are declared per server.** A new defcustom
  `superchat-mcp-server-effects` maps a server name to the effect descriptors its
  tools carry. A server with no declaration falls back to a single `unknown` effect,
  which is exactly today's de-facto treatment (`superchat-authority.el:322-330`:
  no resolver → `unknown` → ask when attended, deny when unattended).
  The value structure reserves a place for per-tool overrides, but **per-tool
  override is not implemented in this SPEC** — adding it later must not require
  changing the shape of existing declarations.
- **The three broken `/mcp-*` commands are in scope**, since they live in the same
  file and the same test fixture serves them.
- **Tool naming stays `mcp:<server>:<tool>`.** It cannot collide with built-ins
  (`superchat-register-tool` errors on a built-in name) and re-registration of the
  same name is replacement, not duplication.

## Model delta

The original MCP work introduced no new Kernel concept. Revision 2 synchronizes
this live contract with the later Authority clarification: the base fold is
name-independent only after Effect resolution, the effective seam remains
name-sensitive, and topology selects one of two wrappers. This wording correction
does not change MCP registration or execution behavior. Two effects of the
original work on `KERNEL.md` remain:

- The authority-map line
  `MCP server lifecycle — authority: superchat-mcp.el | source-backed`
  remains correct and unchanged. Tool *exposure* is already covered by
  `Model-facing tool surface — authority: superchat-tools.el`, and this work moves
  MCP onto that owner rather than adding a boundary.
- Two entries that are true-but-vacuous for MCP today become true in fact:
  the tool-execution lifecycle invariant, and the synthesized-tool shadow guard.
  This is a fact for `learn` to record in `EVIDENCE.md`, not a Kernel revision.

Separately confirmed in `plan` and **not part of this SPEC's scope**: K2 will add
`Attendance` (Concepts) and `Turn routing` (Architecture boundaries). `learn` writes
those; they do not depend on this work.

## Action Contracts

### AC: Register one MCP server's tools

- **precondition** — the server is present in `mcp-hub-servers`, survives the
  `superchat-mcp--active-servers` filter (`superchat-mcp.el:50-59`), and its
  connection reports `connected`.
- **input** — the server name, plus the result of a **single**
  `(mcp-hub-get-all-tool :categoryp t)` call for the whole hub.
- **permitted effect** — writes only to `superchat-tools--external-registry` and to
  Authority's effect-resolver and approval-preview tables, by way of
  `superchat-register-tool`. It must not touch the built-in tool table, must not
  mutate an in-flight `llm-chat-prompt`, and must not start or stop any server.
- **invariant** — the registered name is `mcp:<server>:<tool>` and is disjoint from
  `superchat-tools--builtin-tool-names`; effects come from
  `superchat-mcp-server-effects` for that server, defaulting to a single `unknown`
  descriptor; registering a name already registered replaces it rather than
  appending a duplicate; tools are attributed by `:category`, never by `:server`.
- **verification** — `test/test-superchat-mcp.el`, wired into `test/run-tests.el`.

### AC: Unregister one MCP server's tools

- **precondition** — the server is being stopped, or has been dropped from
  `superchat-mcp-servers`.
- **input** — the server name.
- **permitted effect** — `superchat-unregister-tool` for each `mcp:<server>:*` name;
  no other registry is touched.
- **invariant** — after the call, no `mcp:<server>:*` name resolves to a tool, an
  effect resolver, or an approval preview; tools of *other* servers are untouched.
- **verification** — as above; the "resolver and preview are gone" assertion is
  explicit, because a dangling resolver is silent.

## Seams and verification

**Contract layer** (catches this class of defect recurring):
assert that `mcp-hub-get-all-tool` really accepts `:categoryp`, via `func-arity` /
`help-function-arglist` on the live symbol. Skip when `mcp.el` is absent. This is
the layer that would have caught defects 1 and 2 — the existing suite missed them
precisely because it stubs superchat's own `superchat-mcp-get-tools` to nil
(`test/test-llm-backend.el:203`, `:545`, `:2987`).

**Behaviour layer** (stub `mcp-hub-get-all-tool` to return known plists, so it runs
without `mcp.el`):

1. the hub function is called **once**, not once per active server;
2. tools are attributed by `:category`, and two servers exposing the same tool name
   produce two distinct prefixed names with **no duplicates**;
3. what reaches the model surface is an `llm-tool` struct, not a plist;
4. invoking a registered MCP tool reaches `superchat-tool-execute`;
5. a server absent from `superchat-mcp-server-effects` resolves to `unknown`;
   a declared server resolves to its declared effects;
6. after unregistration, tool, effect resolver and approval preview are all gone;
7. `superchat--syn-check-name` sees registered MCP names (today it sees nil).

**Command layer**: `/mcp-start-server NAME` and `/mcp-stop-server NAME` no longer
signal `wrong-number-of-arguments`; `/mcp-health` produces rendered output rather
than `:noop`.

**Housekeeping**: `test/test-superchat-mcp.el` exists but is not loaded by
`test/run-tests.el` and calls `superchat-get-gptel-tools`
(`test/test-superchat-mcp.el:87`), a function that no longer exists. It is rewritten,
not extended, and must be added to the runner — a test file outside the runner is
not coverage.

**Acceptance**: `emacs -Q --batch -L . -l test/run-tests.el` reports 0 unexpected
results, a total no lower than 895, and `test-superchat-mcp` tests actually running
(not skipped) in the stubbed behaviour layer.

## Compatibility and migration

`compatible`.

- Nobody currently has a working MCP tool surface — the collection function returns
  nil against every published `mcp.el` — so no working behaviour changes.
- `superchat-mcp-server-effects` is a new defcustom; its default (nil) reproduces
  today's treatment (`unknown` → ask when attended, deny when unattended).
- `superchat-mcp-servers` is untouched, so existing user configuration keeps working.
- No migration step is required of users. Users who *want* fewer approval prompts opt
  in by declaring effects per server; not declaring them is safe, only chattier.

## Out of scope

- Per-tool effect overrides (the value shape reserves room; the behaviour is not built).
- Parsing MCP tool annotations (`readOnlyHint`, `destructiveHint`) to infer effects.
  `mcp.el` does not parse annotations at all — zero hits across its sources — so this
  would be upstream work in the sibling `../mcp.el` checkout.
- MCP resources, prompts, resource templates, and sampling. Only tools are integrated.
- `superchat-mcp-health-timeout` (`superchat-mcp.el:40`), a defcustom nothing reads.
  Recorded as a finding; deleting or wiring it is separate work.
- Async server connect. `superchat-mcp-start-servers` passes SYNCP=t
  (`superchat-mcp.el:121`) and blocks Emacs; that is a known, separate defect.
- Kernel K2 additions (`Attendance`, `Turn routing`) — confirmed in `plan`, written by
  `learn`, independent of this work.
- ROADMAP.md retirement — confirmed in `plan`, single-context, disjoint scope; it is
  not a slice of this SPEC.

## Issue map

To be filled by `arrange`. Suggested seams, each independently verifiable:

1. Correct the `declare-function` and the call form; single call with `:categoryp t`;
   attribute by `:category`. (Contract-layer test lands here.)
2. `superchat-mcp-server-effects` defcustom plus effect resolution with the
   `unknown` fallback.
3. Register on connect / unregister on stop; delete `superchat-llm.el:782-784`;
   keep the `superchat-synthesis.el:96` shadow guard working.
4. The three `/mcp-*` command defects.
5. Rewrite `test/test-superchat-mcp.el` and wire it into `test/run-tests.el`.

Slice 3 depends on 1 and 2. Slice 5 covers all of them and should land with, not
after, the slices it verifies.

## Revision notes

- **r2 (2026-09-01)** — compatible contract correction after the K9
  counterexample review: replaces the whole-executor name-blind claim with the
  resolved-Effect base-fold boundary and records both the ordinary/team/workflow
  wrapper and the prepared-subagent wrapper. MCP still has one shared execution
  seam and no MCP-specific wrapper.

- **r1 (2026-08-26)** — initial capture from the confirmed `plan`. Records the
  correction that MCP does not bypass Authority (it crashes before reaching it, and
  in practice never gets that far), the decision to register rather than collect
  per request, per-server effect declaration with an `unknown` fallback, inclusion
  of the three broken commands, and the two-layer test strategy whose contract layer
  exists specifically because the stub-at-our-own-boundary pattern hid defects 1
  and 2.
