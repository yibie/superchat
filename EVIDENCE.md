# Evidence

status: active
scope: superchat — verified observations that change a later judgement
owner: `learn` (sole writer, per `SPEC-AGENTS.md:45`)
convention: `docs/spec-agents/evidence-links.md` — entries carry a stable
`E-YYYYMMDD-NNN` id so a Slice's `evidence_ref` can point at one
order: append-only, oldest first

Only observations that were verified and that would change a later decision
belong here. An entry records what was measured, what it means, and what to do
about it — not a session log.

## 2026-08-27 — The MCP half of the synthesized-tool shadow guard is unreachable

id: E-20260827-001
status: confirmed
scope: `superchat-synthesis.el` name guard; `KERNEL.md` invariant
"A synthesized tool may not shadow a built-in or MCP tool name"
source: slice `.specs/mcp-authority-integration/issues/03-register-unregister-lifecycle.md`

### Observation

After slice 03, `superchat--syn-existing-names` (`superchat-synthesis.el:92`)
really does report registered MCP names: with `mcp:filesystem:read_file`
registered through `superchat-register-tool`, the name is in the list it
returns. That half of the repair holds.

But `superchat--syn-check-name` (`superchat-synthesis.el:117`) tests the name
grammar *before* the membership test. `superchat--syn-valid-name-p`
(`superchat-synthesis.el:112`) requires
``\`[a-zA-Z][a-zA-Z0-9_-]\{0,63\}\'``, and every registered MCP name has the
form `mcp:<server>:<tool>` (`superchat-mcp.el:666`, `superchat-mcp.el:545`).
A colon is outside that grammar.

Measured in `emacs -Q --batch` against the current tree:

| Call | Result |
| --- | --- |
| `(member "mcp:filesystem:read_file" (superchat--syn-existing-names))` | non-nil — the guard sees it |
| `(superchat--syn-valid-name-p "mcp:filesystem:read_file")` | `nil` |
| `(superchat--syn-check-name "mcp:filesystem:read_file")` | `"Error: invalid tool name ... (letters, digits, _ and - only)."` |
| `(superchat--syn-valid-name-p "mcp:x")` | `nil` |

The refusal comes from the grammar branch, never from the shadow branch.

### Interpretation

The grammar closes both directions: an MCP name can never be *requested*
(it fails the grammar), and a name that can be requested can never *equal* an
MCP name (every MCP name contains colons). What prevents a synthesized tool
from taking an MCP tool's name today is therefore the naming scheme, not the
membership test.

The membership test's MCP half has exactly one observable job left: not
signalling. That job is not trivial — before slice 03 it *did* signal.
`superchat--syn-existing-names` mapped `llm-tool-name`, a cl-struct accessor,
over the gptel-style plists `mcp.el` returns, and the `ignore-errors` sat
around the collection call rather than around the `mapcar`, so one connected
MCP server broke `define_tool` outright.

Consequences for the static model:

- `KERNEL.md:502` describes the mechanism as "`superchat--syn-check-name`
  refuses outright any name that already exists among the built-in tool list
  or the MCP tool set". For the built-in set that is enforced and tested
  (`test/test-synthesis.el:120`). For the MCP set it over-describes what the
  code does — the outcome holds, the stated mechanism does not carry it.
- Same for the `define_tool` precondition line at `KERNEL.md:444`.
- `.specs/mcp-authority-integration/SPEC.md` claims repairing MCP makes this
  entry "true in fact rather than only in code". That is half right: the
  crash is gone and the guard now sees MCP names, but the MCP branch of the
  refusal was never reachable and still is not.

### Recommended next action

- Do **not** loosen the name grammar to make the membership test reachable.
  The grammar is the stronger guarantee; the membership test is the weaker
  one, and trading down would be a regression dressed as a fix.
- Take a Kernel wording revision through `plan`: for the MCP half, name the
  namespace (`mcp:` plus colons, outside the synthesized-name grammar) as the
  mechanism, and keep the membership test credited for the built-in half.
- Treat this as a standing dependency: if tool naming ever changes so that an
  MCP name becomes grammar-legal, the membership test becomes load-bearing on
  that day, and `mcp/synthesis-shadow-guard-sees-mcp-names-without-signalling`
  (`test/test-superchat-mcp.el:976`) is the test that must then grow a real
  refusal assertion.

### Verification

`emacs -Q --batch -L . -l test/run-tests.el` → 955 tests, 939 as expected,
0 unexpected, 16 skipped (2026-08-27). The table above was produced by a
throwaway batch script that registered one MCP-named tool through
`superchat-register-tool` and called the guard directly; no product file was
modified to obtain it.

### References

- `superchat-synthesis.el:92`, `:112`, `:117`
- `superchat-mcp.el:545`, `:666`, `:682`
- `KERNEL.md:444`, and the shadow-guard invariant at `:502` — **renumbered to `:532` and
  retitled by K2 on the same day** (`E-20260827-004`)
- `test/test-synthesis.el:120` `syn/cannot-shadow-a-builtin-tool` (built-in
  half, genuinely enforced)
- `test/test-superchat-mcp.el:976`, `:1016` (MCP half — pins "sees the name,
  does not signal", and asserts the pre-slice-03 expression still signals)
- `.specs/mcp-authority-integration/SPEC.md` — "Collateral" and "Model delta"

## 2026-08-27 — One wrapped tool call creates the chat view and stamps a session id into it

id: E-20260827-002
status: confirmed
scope: `superchat-agent-loop.el` tool wrapper; `superchat-llm.el` view callback;
consumed by `superchat-workflow.el`
source: incidental finding while adding registration tests for slice 03; the
behaviour predates that slice and is unchanged by it

### Observation

In `emacs -Q --batch` with `superchat` loaded and no chat ever opened,
`(get-buffer superchat-buffer-name)` is nil. Wrapping a single tool with
`superchat--agent-wrap-tool` (`superchat-agent-loop.el:423`) and calling it
once — with `superchat-tool-execute` stubbed, so no tool actually runs —
leaves behind:

- the `*Superchat*` buffer, created;
- buffer-local `superchat--session-id` in it, set to a freshly minted
  `"20260827-004700-a8b3"`;
- buffer-local `superchat--agent-tool-call-count` in it, set to 1.

The global default of `superchat--session-id` stays nil, so this is a
buffer-local write into a buffer the call itself brought into existence.

Mechanism, read out of the source rather than guessed:
`superchat--agent-wrap-tool` routes the wrapped function through
`superchat--callback-in-view` (`superchat-llm.el:1154`), whose body is
`(with-current-buffer (get-buffer-create buffer-name) ...)`. Inside that
buffer the wrapper calls `superchat--agent-ensure-session-id`
(`superchat-agent-loop.el:266`), which `setq`s the id; because
`superchat--session-id` is `defvar-local` (`superchat.el:962`), the `setq`
creates the binding in that new buffer.

### Interpretation

Wrapping and invoking a tool is not supposed to be an identity-creating act,
but it is: one tool call materialises a chat view and mints a Session identity
for it, with no Session having been opened.

This has a real consumer. `superchat-workflow--run-step`
(`superchat-workflow.el:424`) reads `superchat--session-id` out of exactly
that buffer with `buffer-local-value` and prefers it over the correlation id
its caller passed. So an id stamped by an unrelated tool call silently becomes
a workflow's Runtime session id.

That is not hypothetical — it is how the finding surfaced.
`workflow-async-chain-reuses-correlation-id` (`test/test-workflow.el:573`)
failed with `:session-id` equal to a minted agent id instead of the
`"workflow-test"` the test passed in, as soon as an earlier test in the same
batch invoked the real wrapper. Tests run alphabetically, so an `mcp/…` test
preceded a `workflow-…` test and the leak crossed between them.

**The containment is in the test, not in the product.** It was fixed by
binding `superchat-buffer-name` to a throwaway buffer and killing it in
`test-superchat-mcp--with-surface`. The runtime behaviour is untouched and
still reachable.

### Recommended next action

- Out of scope for `mcp-authority-integration`. It touches Session identity,
  so it needs `plan` before any code moves.
- Two questions for `plan`, which are the actual fork in the road:
  1. Should `superchat--callback-in-view` **create** the view
     (`get-buffer-create`), or only enter it when it is live and otherwise run
     the callback where it stands? Rendering already guards on
     `buffer-live-p` (`superchat-agent-loop.el:284`), so the create is not
     what makes rendering work.
  2. Should `superchat--agent-ensure-session-id` mint into whichever buffer
     happens to be current, or should a Session id only ever be minted by
     whoever opens a Session?
- Until that is settled: any new test that drives the real tool wrapper must
  bind `superchat-buffer-name` to a throwaway buffer and kill it afterwards,
  or it leaks an identity into `*Superchat*` for every test that runs after
  it. `test-superchat-mcp--with-surface` (`test/test-superchat-mcp.el`) is the
  worked example.

### Verification

Reproduced in `emacs -Q --batch -L . -l <script>` on the current tree: the
before/after buffer state and both buffer-local values above are printed by
the script, and no product file was modified to obtain them. The test-order
failure was observed directly during slice 03 (1 unexpected,
`workflow-async-chain-reuses-correlation-id`) and disappeared once the view
was isolated in the test; the suite is 955 / 939 / 0 unexpected / 16 skipped
as of 2026-08-27.

### References

- `superchat-agent-loop.el:266` `superchat--agent-ensure-session-id`
- `superchat-agent-loop.el:284` (rendering already guards on `buffer-live-p`)
- `superchat-agent-loop.el:321` `superchat--agent-wrap-function`,
  `:423` `superchat--agent-wrap-tool`
- `superchat-llm.el:1154` `superchat--callback-in-view`
- `superchat.el:962` `defvar-local superchat--session-id`
- `superchat-agent-loop.el:88` `defvar-local superchat--agent-tool-call-count`
- `superchat-workflow.el:424` (the consumer that reads the leaked id)
- `test/test-workflow.el:573` `workflow-async-chain-reuses-correlation-id`
  (the test that caught it)

## 2026-08-27 — MCP tool surface repaired and routed through Authority

id: E-20260827-003
status: confirmed
scope: `.specs/mcp-authority-integration/` — all four slices; `superchat-mcp.el`,
`superchat-llm.el`, `superchat-synthesis.el`, `superchat.el`, `test/test-superchat-mcp.el`
source: slices 01–04 of `.specs/mcp-authority-integration/SPEC.md` (confirmed, r1);
`check` report `.scratch/check/mcp-authority-integration.md`
verification: `emacs -Q --batch -L . -l test/run-tests.el`

### Observation

The MCP tool surface was non-functional against every published `mcp.el` and is now
functional and audited. Measured on the current tree:

| | at SPEC capture | at close |
| --- | --- | --- |
| suite | 895 / 879 as expected / 0 unexpected / 16 skipped | **955 / 939 / 0 / 16** |
| `mcp/*` tests | 0 loaded (file outside the runner) | 60, all running, none skipped |
| `superchat-mcp.el` | 326 lines (`git show HEAD:superchat-mcp.el`) | 1010 lines |

The delta is fully accounted for: `test/run-tests.el:97` newly loads
`test/test-superchat-mcp.el`, which went from 0 `ert-deftest` forms to 60.
895 + 60 = 955; 879 + 60 = 939; skipped unchanged.

Four original defects closed, each verified by a test that fails if the defect returns:
the `declare-function` arglist and the positional call against `mcp.el`'s `&key`
signature; the per-server loop that would have returned the whole hub N times;
attribution by `:category` rather than the `:server` key `mcp.el` never sets; and the
gptel-style plist reaching `llm-tool-name`. Four `required` findings raised by `check`
were closed in a repair round, and three `suggestion` items were taken.

### Interpretation

Two `KERNEL.md` entries that were true-but-vacuous for MCP became true in fact: every
model-invoked tool call passing `superchat-tool-execute`, and the synthesized-tool name
guard seeing registered MCP names. The single-choke-point invariant held throughout —
it was never breached, because execution never reached the wrapper at all.

The most transferable lesson is about test placement, not about MCP. The original defects
survived a green suite because every test stubbed `superchat-mcp-get-tools` — Superchat's
own boundary — to nil, so nothing ever exercised the call form against the real `mcp.el`
signature. The repair added a contract layer that asserts against the live symbol's
arglist (`func-arity` / `help-function-arglist`), skipped when `mcp.el` is absent. A test
that stubs the seam it is meant to protect verifies nothing.

### Recommended next action

- The same pattern is still present elsewhere: `test/test-synthesis.el:120`
  (`syn/cannot-shadow-a-builtin-tool`) stubs `superchat--syn-existing-names` and therefore
  never exercises the real scoping. See `E-20260827-005`.
- Six `check` suggestions remain open and were deliberately not taken: servers started
  outside Superchat never auto-register until `/mcp`; `/mcp` counts from the hub while
  `/backend` and `/tools` count from the registry; the `mcp:<server>:` naming rule is
  encoded in three places; `superchat-mcp--approval-preview` does not self-protect;
  `:args` is forwarded across two schema conventions with no test; and
  `superchat-mcp-health-timeout` is a defcustom nothing reads.

### Verification

`emacs -Q --batch -L . -l test/run-tests.el` → `Ran 955 tests, 939 results as expected,
0 unexpected, 16 skipped`. `superchat-mcp.el` byte-compiles with zero warnings.
`docs/spec-agents/check-kernel.sh .` → `ok: authority map form is valid.`

### References

- `.specs/mcp-authority-integration/SPEC.md` and its four issues
- `.scratch/check/mcp-authority-integration.md`
- JJ change `sxxslrvn` "mcp: repair the tool surface and route it through Authority"
- `superchat-mcp.el:606` `superchat-mcp--collect-tools`, `:768` register, `:892` unregister,
  `:830` reconcile
- `test/run-tests.el:97` (the added load line)

## 2026-08-27 — START re-scan: KERNEL.md K1 → K2

id: E-20260827-004
status: confirmed
scope: `KERNEL.md`; the `start` re-scan action
source: `.scratch/start/REPORT.md` (re-scan section, 2026-08-27); confirmed through `plan`
verification: `docs/spec-agents/check-kernel.sh`; a per-reference anchor check over all 409
`source:` references

### Observation

The first re-scan of this project's Kernel returned a verdict for all 87 checkable items
(67 entries plus 20 authority rows). 85 supported. Two did not hold, and **neither was
caused by drift — both clauses were already inaccurate when K1 was authored**:

1. **Contradicted.** The Run-lifecycle entry claimed `--finish`'s handler was "the only
   edge that reaches `audit-incomplete`". `superchat-runtime--audit-incomplete`
   (`superchat-runtime.el:1007`) has six call sites: `:1153` (inside `--finish`), `:692`,
   `:742`, `:1592`, `:1618`, `:1759`. `superchat-runtime.el` is unmodified in the working
   tree and its last commit `8017b48` predates K1's bootstrap commit `22978a5`.
2. **Unsupportable as worded.** The shadow-guard invariant said the guard refuses a name
   "already existing among the built-in tool list", while `KERNEL.md` itself defines
   "built-in" as the thirty-one reserved names in `superchat-tools--builtin-tool-names`.
   Measured in `emacs -Q --batch -L . -L ../llm`: under the shipped five-verb allowlist,
   `superchat--syn-check-name` **allows** `sql`, `shell-command`, `workspace_write`,
   `render_ui` and `define_tool` — all reserved — and refuses only `read_buffer` and
   `eval_elisp`, which the allowlist exposes. With the allowlist set to `all`, all of them
   are refused. The guard is scoped to the *exposed* surface, not the reserved set.

Separately, 36 `source:` references across 15 entries pointed at lines whose construct had
moved. All 36 were positional only: every cited construct was located again by content
(`superchat.el` +5/+12, `superchat-synthesis.el` +1/+14, `superchat-llm.el` +3), and all 31
distinct targets land on a `defun`/`defvar`/call site.

Zero entries were missing `since:` or `source:`.

### Interpretation

Two findings matter beyond their own repair.

**A re-scan is the only action that reads reality back into the Kernel.** `check` compares
code against the Kernel and never the reverse, so a clause that was wrong on the day it
was written can pass every subsequent `check` indefinitely. Both corrections here are of
exactly that kind. This is the first evidence in this project that the re-scan earns its
cost.

**Line-number provenance decays silently and asymmetrically.** One re-anchored reference —
`superchat-synthesis.el:117` in the shadow-guard entry — drifted onto a *different* but
plausible-looking definition, so a reader following it would have been reassured rather
than warned. Content-based relocation caught it; reading the citation would not have.

### Recommended next action

- Re-run the anchor check whenever `KERNEL.md`'s cited files change substantially; a
  reference that lands on a blank or comment line is the cheap signal.
- The adversarial pass refuted 7 of 49 proposed Kernel additions. Two of the refusals
  correct earlier records: `Attendance` is **not** a K2 addition (K1 already states its
  effect at the Authority Decision entry), and the evidence contract
  `epistemic-learning-v0` is **not enacted** — see the excluded-material entry in
  `KERNEL.md`. Both had been listed as K2 candidates in the 2026-08-26 START report.
- 37 confirmed candidate semantics remain unpromoted in `.scratch/start/REPORT.md`. They
  are available to a later `plan`; they are not Kernel content today.

### Verification

`docs/spec-agents/check-kernel.sh .` → `ok: authority map form is valid.` (22 authority
rows, 4 `derived`, none with a second site). All 409 `source:` references resolve to an
existing file with an in-range line number. Suite unchanged by this revision at
955 / 939 / 0 unexpected / 16 skipped — K2 changes no code.

**Anchor verification is incomplete, and deliberately so.** See `E-20260827-008`: a
concurrent session was writing product code throughout this `learn`, and the K2 anchors
were invalidated twice while being written. What is verified is the semantic content of
every entry. What is **not** verified as of this entry is that each `source:` line still
points at the construct it names, for the seven files that session is editing.

### References

- `.scratch/start/REPORT.md` — the re-scan report, including the full 42-item candidate list
- `KERNEL.md` — `### Re-scan basis`, `### Excluded from the enacted sections`
- `superchat-runtime.el:1007`, `:692`, `:742`, `:1153`, `:1592`, `:1618`, `:1759`
- `superchat-synthesis.el:92`, `:112`, `:117`, `:526`; `superchat-tools.el:1714`
- `test/test-runtime.el:1034` (a committed test pinning a non-`--finish` edge)

## 2026-08-27 — A test that stubs its own boundary verifies nothing

id: E-20260827-005
status: confirmed
scope: `test/test-synthesis.el`; any test that replaces the seam it is meant to protect
source: `E-20260827-003` (the MCP contract-layer repair); `E-20260827-004` (the re-scan)
verification: read the two tests; run the probe in `E-20260827-004`

### Observation

The same defect shape has now been confirmed twice in this repository.

The MCP defects survived a green suite because every test stubbed
`superchat-mcp-get-tools` — Superchat's own boundary — to nil, so the call form was never
exercised against the real `mcp.el` signature (`test/test-llm-backend.el:203`, `:545`,
`:2987`).

`syn/cannot-shadow-a-builtin-tool` (`test/test-synthesis.el:120`) has the identical shape:
it `cl-letf`s `superchat--syn-existing-names` to return `'("read-file" "shell-command")`
and then asserts the membership branch works. It therefore verifies that *given a list*,
the guard refuses a member of it — and never verifies **what is in the real list**. That
is exactly the fact that turned out to be wrong (`E-20260827-004`).

### Interpretation

Stubbing a dependency is normal. Stubbing the function under test's own data source turns
the test into a tautology: it and the implementation are two consequences of the same
assumption, so it cannot fail when the assumption is wrong. The tell is that the stub's
return value is the thing the invariant is about.

### Recommended next action

- Not promoted to `docs/lessons/` yet: two instances in one subsystem is a pattern worth
  recording, but the scope condition ("a test that stubs the function under test's own
  data source") has not been checked against the rest of the suite. Promote after a sweep.
- Any test written for the three defects confirmed in the 2026-08-27 `plan` must exercise
  the real table, not a stub. This is a hard requirement on the next SPEC.

### Verification

Read both tests directly: `test/test-synthesis.el:120` `cl-letf`s
`superchat--syn-existing-names` to `'("read-file" "shell-command")`, and
`test/test-llm-backend.el:203` stubs `superchat-mcp-get-tools` to nil. The probe in
`E-20260827-004` shows what the real table contains and why the stub hid it. No file was
modified to obtain either observation.

### References

- `test/test-synthesis.el:120` `syn/cannot-shadow-a-builtin-tool`
- `test/test-llm-backend.el:203`, `:545`, `:2987`
- `.specs/mcp-authority-integration/SPEC.md` — "Seams and verification", contract layer

## 2026-08-27 — ROADMAP.md retired to archive/, and what that drops

id: E-20260827-006
status: confirmed
scope: root `ROADMAP.md` (429 lines) → `archive/ROADMAP.md`
source: confirmed in `plan` 2026-08-27; doctrine `UPGRADE.md` §1 and the phase-shaped
conversion rule; `docs/spec-agents/WORKFLOW.md` Knowledge Classes ("仓库不记录未来意图")
verification: the file is readable at `archive/ROADMAP.md`; no root document links to it

### Observation

`ROADMAP.md` recorded intent ahead of work, which the repository no longer does. Before
moving it I inventoried what the move actually drops, because most of the file turned out
**not** to be future intent:

- **Milestones v0.5 – v1.2.0 are historical fact**, not intent: each is marked `SHIPPED`
  with commit ranges. That history is already covered by `CHANGELOG.md` and by git.
- **v0.9 and v1.0 are almost entirely `[x]` completed items** — the module split, autoload
  coverage, CI, the `(llm "0.24")` bump, `docs/architecture.md`, the CHANGELOG. Also history.
- **Genuinely dropped future intent is three lines**: v0.9's open questions (keep Emacs
  28.1 or bump to 28.2; add `Cask`/`Eldev`; untrack `superchat.elc`), and v1.0's "actual PR
  to melpa/melpa pending manual submission by maintainer".

One section is neither history nor intent and deserves a separate decision:
**"Cross-cutting concerns"** states five standing engineering conventions —
`lexical-binding: t` everywhere; the `superchat--` private / `superchat-` public prefix
split; every milestone extends ERT coverage in the same change; no silent breaking
changes (a `defcustom` default change needs `:safe` plus a one-version deprecation
cycle); and working-tree hygiene. `check` has been enforcing the first two in practice —
the `mcp-authority-integration` engineering axis checked exactly them.

### Interpretation

Those five are a **Protocol**, not a roadmap. `docs/spec-agents/knowledge-promotion.md`
routes "稳定的开发、评审、测试和协作约定" to `docs/protocols/`, and they meet the bar:
they are scoped, repeatedly applied, and already used as review criteria. Archiving the
roadmap without saying so would delete a live convention by filing it under retired
intent — the exact failure the "show the user what is being dropped" rule exists to
prevent.

They are **not** promoted here. Promotion changes an acceptance boundary and therefore
needs `plan`, which has not confirmed it; `learn` may not create the Protocol on its own.

### Recommended next action

- Take the five cross-cutting conventions to `plan` as a candidate
  `docs/protocols/engineering-conventions.md`. Until then `check` continues to apply them
  from practice rather than from a record, which is how they nearly got lost.
- The three dropped intent lines need no home. If MELPA submission is still wanted it
  becomes work through `plan`, not a line in a retired file.

### Verification

`archive/ROADMAP.md` exists and is byte-identical to the retired root file
(`cmp` clean; `diff` against `git show HEAD:ROADMAP.md` is empty, and `jj status` records
the move as a rename, so history follows).

**Correction (same day).** This entry originally claimed no root document still referenced
`ROADMAP.md`. That was wrong and was caught by an adversarial review. Four references were
left dangling by the move and have since been repaired: `README.md` and `README_cn.md` each
carried a live markdown link `[ROADMAP.md](ROADMAP.md)`, `AGENTS.md:210` named it, and
`KERNEL.md`'s own excluded-material entry cited it as a `source:` path. All four now point
at `archive/ROADMAP.md`. The lesson is narrow and worth keeping: **retiring a file is not
done until its inbound links are found**, and "no document references it" is a claim that
must be greped for, not assumed.

### References

- `archive/ROADMAP.md` — full retired content, including all SHIPPED milestone history
- `CHANGELOG.md` — the durable home for shipped history
- `docs/spec-agents/knowledge-promotion.md` — Protocol destination and required fields
- `.scratch/check/mcp-authority-integration.md` — the engineering axis that applied the
  conventions without a record to cite

## 2026-08-27 — Installed doctrine is a revision behind the installed CLI

id: E-20260827-007
status: confirmed
scope: `SPEC-AGENTS.md` and `.claude/skills/` versus the `spec-agents` CLI on PATH
source: discovered while closing `mcp-authority-integration` in `learn`
verification: `spec-agents check-state`; byte comparison against the upstream checkout

### Observation

`spec-agents` v4.1.0 is installed at `/usr/local/bin/spec-agents` and enforces rules the
doctrine text in this repository does not describe.

Byte comparison against the upstream working copy at
`/Users/chenyibin/Documents/prj/SPEC-AGENTS.md` (HEAD `e6ae0b8`): `START.md`, `UPGRADE.md`,
`docs/spec-agents/` (9 files) and all six skills are **identical**. Only `SPEC-AGENTS.md`
differs, and the local copy is the **older** one — upstream replaced the prose list of six
actions with a table of CLI commands (`status`, `ready`, `gate`, `transition`,
`check-state`).

Two concrete consequences hit this `learn`:

1. `bin/spec-agents:161` requires a SPEC whose slices are all `done` to have
   `status: verified`. The installed `capture` skill lists the valid values as
   `draft | confirmed | in-progress | revised | superseded` — **`verified` is not among
   them**. Following the installed skill produces a permanent violation.
2. `bin/spec-agents:148` cannot resolve this project's `spec_ref:` values. The slices wrote
   `` `path` — section name ``, which resolves from neither root nor slice.

   **Correction (same day, from a peer session working in the upstream repo).** This entry
   first blamed the backticks, saying the CLI "treats the whole value as a path". That is
   wrong: `bin/spec-agents:94` does `gsub(/`/, "", v)` and strips backticks, which is why
   upstream's own backtick-wrapped bare paths resolve. What actually kills the parse is the
   **trailing section name**. And the root cause is one level up: `skills/arrange/SKILL.md:51`
   gives the minimal slice template a bare `spec_ref:` line with **no format specification at
   all**, so writing `` `path` — section `` violates no written rule.

### Interpretation

The gate is only useful when it is green, and a red gate that is red for reasons nobody
can act on trains people to ignore it. Both causes here were formatting conventions, not
real state violations — which is the worst kind of noise, because it hides a genuine
violation in the same list. `check-state` now exits 0 for this project.

The deeper point: doctrine and its enforcement tool can drift apart, and the *tool* is
what actually decides. When they disagree, the tool wins and the text needs updating —
upstream, never here.

### Recommended next action

- Not fixable in this repository. `docs/spec-agents/WORKFLOW.md` states that no action in
  a managed project writes installed doctrine; changing it means a `plan` in the
  SPEC-AGENTS repository followed by a reinstall here.
- Until then, treat `bin/spec-agents` as authoritative over the installed skill text on
  slice and SPEC frontmatter.

### Verification

`spec-agents check-state` briefly reported `ok: no state violations.` (exit 0, from 9) once
the two format changes were made.

**Correction (same day).** Both changes were then **reverted**, because an adversarial
review pointed out that `learn` is not the writer of `SPEC.md`
(`docs/spec-agents/WORKFLOW.md`: "SPEC 由 `capture` 写入") and that reformatting `spec_ref`
exceeds the `evidence_ref` mandate in `docs/spec-agents/evidence-links.md`. The gate is red
again — 5 violations — and that is the honest state: **no action in this workflow is
authorised to close a SPEC, while the installed CLI requires one to.** That gap is the
finding; making the gate green by exceeding a mandate was not a fix. It needs `plan`, and
the repair belongs upstream.

### References

- `/usr/local/bin/spec-agents` v4.1.0; upstream `bin/spec-agents:148`, `:161`
- `.claude/skills/capture/SKILL.md:35` (the stale status list)
- `SPEC-AGENTS.md` versus upstream `AGENTS.md`

## 2026-08-27 — Kernel anchors cannot be stabilised while another session writes the tree

id: E-20260827-008
status: confirmed
scope: `KERNEL.md` line-number provenance; concurrent multi-session work in one working copy
source: observed directly during the K2 `learn` of 2026-08-27
verification: file mtimes and `git diff --stat` during the write, recorded below

### Observation

K2 re-anchored 36 references at 16:16. By 16:22 they were stale again, and the cause was
not this session. A second session was editing product code throughout:

| time | file | change |
| --- | --- | --- |
| 16:21:19 | `superchat.el` | — |
| 16:21:58 | `superchat-llm.el` | — |
| 16:22:17 | `superchat-runtime.el` | +26 / −18 (a new `:max-tokens` request field) |
| 16:22:33 | `superchat-prompt-hooks.el` | +15 / −4 |
| 16:23:14 | `superchat-tape-view.el` | +30 / −20 |
| 16:25:11 | `superchat-compact.el` | +208 / −64 |

The last three were **not modified when this `learn` began**. The `superchat-runtime.el`
change alone invalidated **all 70** of the Kernel's references into that file — measured,
not estimated: every one of the 57 distinct cited lines held different content afterwards.
Those 70 were re-anchored a second time, at which point the earlier pass over
`superchat.el`, `superchat-llm.el`, `superchat-prompt-hooks.el` and `superchat-compact.el`
had itself gone stale.

The semantic claims were unaffected throughout. `superchat-runtime--audit-incomplete`
still has exactly six call sites — the load-bearing fact behind K2's correction A — they
had simply moved from 692/742/1153/1592/1618/1759 to 694/744/1155/1598/1624/1767.

### Interpretation

**Line-number provenance is not a stable anchor under concurrent writers, and no amount of
care in the writing action fixes that.** The Kernel records *meaning*, which survived every
one of these edits; it records meaning's *location*, which did not survive ten minutes. The
two have different stability and the current format does not distinguish them.

This is also a workflow finding, not only a formatting one. `docs/spec-agents/WORKFLOW.md`
requires that two pieces of work needing the working copy at the same time be isolated
(`jj workspace add` or `git worktree add`). That did not happen here: a `learn` writing the
Kernel and an implementation session writing code shared one working copy, and the `learn`
lost. The isolation rule exists for exactly this and was not applied.

### Recommended next action

- **Do not re-anchor again until the tree is quiet.** A third pass would be invalidated the
  same way and would make K2's verification claim false rather than incomplete.
- When the other session has stopped, run one anchor pass over all 409 `source:` references
  plus the ~150 short-form inline refs in entry prose, which no pass in this `learn` covered
  at all. The check that catches it: a cited line that is blank or a comment.
- Take to `plan`: whether `source:` should carry line numbers at all. A construct name
  (`superchat-runtime--audit-incomplete`) is stable under every edit above; a line number
  survived ten minutes. The trade-off is precision against durability, and it is a Kernel
  format decision, not something `learn` may change.
- Apply the isolation rule: concurrent work in this repository needs `jj workspace add`.

### Verification

Reproduce with `stat -f "%Sm %N" -t "%H:%M:%S" superchat*.el` against the times above, and
`git diff --stat superchat-compact.el superchat-prompt-hooks.el superchat-tape-view.el`
(253 insertions across three files that were clean at the start of this `learn`).
`grep -n "superchat-runtime--audit-incomplete" superchat-runtime.el` → six call sites plus
the defun, confirming the semantic claim held while its coordinates did not.

### References

- `KERNEL.md` — `### Run lifecycle and terminal seal`, `### Re-scan basis`
- `E-20260827-004` — the re-scan whose verification this entry qualifies
- `docs/spec-agents/WORKFLOW.md` — the working-copy isolation invariant

## 2026-08-27 — Adversarial review found six factual defects in a Kernel revision that had already passed verification

id: E-20260827-009
status: confirmed
scope: the K2 `learn` of 2026-08-27; the review practice itself
source: an independent read-only reviewer run on a fresh context after K2 was written
verification: every finding below was re-verified by the commander before acceptance

### Observation

K2 was written, checked against `check-kernel.sh`, checked with a per-reference anchor
script, and its test baseline re-run. It still contained six false statements, all found by
a reviewer instructed to default to "the writer got something wrong":

1. **A wrong claim about when a Run can be sealed `audit-incomplete`.** K2 said the four
   evidence-path edges "arise only for a Run carrying an `:evidence-contract`". Three are
   gated; the fourth is not — submit installs `:audit-failure-function` into *every*
   tool-execution context (`superchat-runtime.el:1590`) and `superchat-authority.el:981`
   calls it without consulting the contract. An ordinary chat Run reaches
   `audit-incomplete` whenever a durable `tool.completed` append fails.
2. **"Tighten-only in effect"** for the session permission Mode. False: the `execute`
   branch (`superchat-authority.el:589`) sits above the guardrail confirm requirement
   (`:599`) and the base default (`:611`), so it turns an otherwise-`ask` call into `allow`.
   The code's docstring scopes the claim to *a registered Authority policy*; K2 dropped the
   qualifier.
3. **"Every literal is under `test/`"** — 33 are; **323 more are under the git-tracked
   `experiments/` tree**, whose harnesses call the real Runtime.
4. **The renamed invariant still over-claimed.** "Already on the live tool surface" — the
   guard calls `superchat-get-llm-tools` with no argument, so it reads the *global*
   allowlist and cannot see a request-local profile.
5. **The revision mis-stated its own size**: "36 references across 15 entries" for what was
   86 across 31.
6. **A dangling `ROADMAP.md`**, plus three more inbound links the archive move broke
   (`README.md`, `README_cn.md`, `AGENTS.md`) that `E-20260827-006` had claimed did not
   exist.

It also caught three doctrine violations: `learn` writing `SPEC.md`, `learn` reformatting
`spec_ref`, and a closed-SPEC section left in `STATUS.md`.

### Interpretation

**The gates all passed and the document was still wrong six times.** `check-kernel.sh`
verifies the *form* of the authority map. The anchor script verifies that a cited line is
not blank. Neither can verify that a sentence is true, and five of the six defects were
true-sounding sentences about control flow — precisely what a machine check cannot reach.

The defects also cluster: every one is an **over-general claim**. "only", "every",
"tighten-only", "already on the surface". In each case the code had a narrower truth and
the writer reached for the tidier sentence. That is a specific, checkable failure mode, not
generic carelessness — a Kernel entry containing a universal quantifier deserves a search
for the counterexample before it is written.

Two of the reviewer's own claims did not survive: it reported the suite unrunnable
(`invalid-read-syntax` in `test/test-compact.el`), which was a transient state caught
mid-write by the concurrent session — the suite now runs at 963 / 947 / 0 unexpected / 16
skipped. Adversarial output needs the same verification it demands.

### Recommended next action

- Keep the pattern: an independent reviewer on a fresh context, told to default to refuting,
  run **after** the writer's own verification passes. This is the second time in this
  repository it has caught defects a green check missed.
- When a Kernel entry states a universal, the writing action should record where it looked
  for the counterexample. All six defects here would have been caught by that one habit.

### Verification

Each finding re-verified independently before acceptance: `grep -n` for the six
`audit-incomplete` call sites and the three `(when evidence-contract …)` guards; the
`cond` order at `superchat-authority.el:569-611` read directly; literal counts by
`grep -rn … | wc -l` over `experiments/` and `test/`; the no-argument
`superchat-get-llm-tools` call at `superchat-synthesis.el:109`; a script diffing K1 against
K2 for the 86/31 count; `ls ROADMAP.md` and a repo-wide grep for inbound links. All six were
corrected the same day.

### References

- `E-20260827-004` — the revision this review audited
- `E-20260827-006`, `E-20260827-007` — the two entries it forced corrections to
- `KERNEL.md` — `### Run lifecycle and terminal seal`, `### Session permission Mode …`,
  `### A synthesized tool may not take a name the global tool allowlist exposes`,
  `### Re-scan basis`, `### Excluded from the enacted sections`

### Resolution (2026-08-27 20:14)

The concurrent session stopped at 16:50:44 and the tree has been quiet since. One full
recalibration pass then ran, grounded on the K1 backup: for every reference K1 owned, the
cited construct's content at commit `22d2785` was located in the current tree by content
and the line number rewritten.

Result over all **487** references (409 on `source:` lines, 78 inline in entry prose):

| | |
| --- | --- |
| still resolve to the construct K1 cited | 420 |
| K2 additions, verified individually by reading each line | 55 |
| blank or out of range | **0** |

39 references were rewritten across 19 entries by the automatic pass; 6 more were fixed by
hand after inspection (`superchat.el:1208→1225`, `1161→1178`, `1169→1186`,
`superchat-prompt-hooks.el:390→395`, `superchat-compact.el:376→581`,
`superchat-llm.el:1263→1324`). The remaining 8 entries — the three K2 rewrote and the five
K2 added — were checked reference by reference rather than automatically, because their
`source:` lists changed shape and positional alignment would have mis-mapped them.

The weak-anchor count is back to **16**, exactly the pre-existing set: multi-line SQL and
call continuations, plus comments that state an invariant. K2 introduced none.

**The method that worked is worth keeping**: relocate by *content* from a known-good
baseline commit, never by offset. Offsets were wrong within ten minutes; content survived
every edit in this episode, including one that re-indented a guard into a new `let` — the
construct was still found, only its indentation had changed.

**One Kernel gap this pass surfaced and did not fix.** The concurrent work added a
`:max-tokens` field to the Request, validated in `superchat-runtime--validate-request`.
`KERNEL.md`'s Action Contract *Submit a Request to the Runtime* enumerates the Request
plist keys and does not list it, so that contract is now incomplete. Adding it is a Kernel
change and needs `plan`; it is not repaired here.

## 2026-08-27 — Cross-agent adversarial exchange: four K2 entries are stale, and one of my findings was mis-attributed

id: E-20260827-010
status: confirmed
scope: `KERNEL.md` entries touched by the concurrent compact-budget work; the SPEC-closing
doctrine gap
source: an exchange with two peer sessions in the same Herdr workspace — the compact-budget
leader (`mcp-impl3`, pane `w3:p3D`) and a session working in the upstream SPEC-AGENTS repo
(`spec-agents-md-17`)
verification: every claim below re-opened in the code by this session before acceptance

### Observation

Two peers were asked to check for overlap and to review this session's work adversarially.
Both found real defects, in both directions.

**Against K2 — confirmed by re-reading the code here, all four:**

1. `### Compaction and Anchor lifecycle` omits a **new persistent side effect outside the
   Tape**: a successful compaction now writes a human-readable log under
   `superchat-compact--log-directory` (`compaction/` beneath `superchat-data-directory`,
   `superchat-compact.el:531-533`). The entry enumerates what `apply-summary` does and this
   is not in it. The peer's argument for why it matters is the sharp part: `### Context View`
   deliberately states "It never writes to Tape", so the Kernel already treats *where a thing
   writes* as load-bearing.
2. Same entry says both directions are reachable as `/compact` and `/expand`. There is now a
   third command, `/compact-log` (`superchat--cmd-compact-log`, `superchat-compact.el:775`).
3. Same entry's "only a completed Run with a parsed summary reaches apply-summary" is still
   literally true but now misleading: a well-formed, marker-complete summary is rejected purely
   for exceeding `floor(0.5 × budget)`.

   **Correction (2026-08-27, from the re-scan #2 peer).** The mechanism named here was wrong.
   `superchat-compact--parse-completed-summary` does take an `&optional max-chars`
   (`superchat-compact.el:382`), but **no caller passes it** — its single call site at `:672`
   passes `response` only. The length gate is the `summary` binding in
   `superchat-compact--finish` (`:673`-`:676`) and its refusal branch at `:685`-`:691`. The
   finding stands; the cited mechanism did not. The unused parameter is itself now recorded in
   `KERNEL.md`'s excluded list.
4. `### Context View` is not false but has lost its premise. The budget is now derived from
   the model window inside the very hook the entry names:
   `superchat-prompt-hook--conversation-history` calls `superchat--llm-context-budget-chars`
   (`superchat-prompt-hooks.el:331`) and attaches a second thing to Turn state,
   `:context-max-chars` (`:361`). The entry says one thing is attached, and its implicit
   fixed-4000-character premise no longer holds.

**In the other direction — three findings this session raised, all conceded by the peer:**
the assemble selection is no longer the "recent suffix" its docstring
(`superchat-tape-view.el:560`) and policy identifier (`:386`) still declare; the same policy
string is published at version 1 on the success receipt and version 0 on the failure
envelope; and the catastrophic context wipe is only half closed. The peer reproduced the
third one rather than conceding it in the abstract — 20 small messages plus one 45 000-char
user message against a 40 000 budget yields a 74-character prompt with zero body lines,
byte-identical to the original failure the whole brief was written to fix. Their own account:
the narrowing to `model-interpretation` was deliberate and recommended to the user, but the
surviving second path was not disclosed, so the user approved without knowing it.

**A finding of mine that was wrong.** `E-20260827-007` blamed the `spec_ref` parse failure on
backticks. `bin/spec-agents:94` strips them (`gsub(/`/, "", v)`); the trailing section name is
what breaks it, and the real gap is that `skills/arrange/SKILL.md:51` specifies no format for
the field at all. Corrected in place.

**The SPEC-closing gap, with much stronger evidence than this session had.** `verified` has
**zero** occurrences across all six `skills/*/SKILL.md`. Its only appearances in
`WORKFLOW.md` (`:139`, `:155`) belong to the **KnowledgeItem** lifecycle, a different entity —
the CLI borrowed a token from an adjacent state machine. Walking the authorization chain:
`capture` is the only action named to write `SPEC.md`, `do` explicitly declines it, `check` is
read-only, `arrange` writes slices, and `learn` has no write-boundary section at all while its
completion condition requires it to remove a *completed* SPEC from `STATUS.md`. The closing
action is split in half: `learn` may drop the SPEC from the status index but may not mark the
SPEC itself complete.

### Interpretation

**Adversarial review works better between agents with different jobs than between an author
and a reviewer of the same work.** The earlier single-purpose reviewer (`E-20260827-009`)
found six defects inside K2's own sentences. These two peers found a different class entirely:
entries that are internally consistent, cite correct lines, and pass every gate — and are
stale because *someone else's* change moved the world out from under them. No amount of
self-review reaches that, because the author has no reason to re-read an entry they did not
touch.

The second lesson is about who owns a disclosure. The peer narrowed a fix, recommended the
narrowing, got approval, and did not report that the same catastrophic failure remained
reachable by another route. Nothing was concealed and the narrowing was defensible; what was
missing was the sentence "and here is what this narrowing leaves open". A scope reduction is
not fully reported until its residue is.

### Recommended next action

- Items 1–4 are `plan` work for a **K3** revision. `learn` must not write them: they change
  what enacted entries mean, and the code they describe is still being changed.
- The `### Context View` revision should absorb the `:selection-policy` version inconsistency
  in the same pass rather than across two revisions, since both concern the same receipt.
- Do not re-anchor `KERNEL.md` again until the compact-budget work is finished; the user has
  already directed that peer to close the surviving wipe path, so the tree will move again.

### Verification

Re-opened here before acceptance: `superchat-compact.el:531-533`, `:775`, `:382-386`,
`:406-410`; `superchat-prompt-hooks.el:331`, `:361`; `superchat-tape-view.el:560`, `:386`,
`:579`, `:594`, `:1019`, `:1042`; `bin/spec-agents:90-96`, `:161`;
`grep -rn "verified" skills/*/SKILL.md` → 0 hits; `docs/spec-agents/WORKFLOW.md:139`, `:155`,
`:184-186`.

### References

- `E-20260827-009` — the single-reviewer pass this exchange complements
- `E-20260827-007` — the entry corrected here
- `E-20260827-008` — the concurrency finding these peers are the other half of

## 2026-08-27 — KERNEL.md K2 → K3: three contradictions retired, four entries caught up, 21 semantics admitted

id: E-20260827-011
status: confirmed
scope: `KERNEL.md` (K2 → K3); the compact-budget change landed the same day
source: `start` re-scan #2 (`.scratch/start/REPORT.md`, "# Start Review — Re-scan #2") and the
`plan` that confirmed it; work order `K3-LEARN-BRIEF.md` from pane `w3:p3D`
verification: `docs/spec-agents/check-kernel.sh`; a per-reference anchor check over all 463
`source:` references; `emacs -Q --batch -L . -l test/run-tests.el`

### Observation

A second full re-scan judged all 94 checkable items (72 entries plus 22 authority rows): 87
supported, 3 contradicted, 4 stale. K3 acts on all of it.

**The three contradictions were all K1-era wordings**, present unchanged in `git HEAD`, that had
passed every gate for two revisions:

1. `### Tool` claimed **every** model-visible tool is wrapped by `superchat--agent-wrap-function`.
   A subagent or team Run uses `superchat--subagent-wrap-function` instead: the delegation request
   supplies both a `:tool-wrapper` and an already-`:prepared-tools` list
   (`superchat-subagent.el:1036`), and `superchat-llm.el:1342` maps its own wrapper only when the
   tools are *not* prepared. Both wrappers call `superchat-tool-execute`; the "every … by one
   wrapper" clause was the false part.
2. `### Workspace Artifact` claimed reads and writes reach it **only** through `workspace_read` /
   `workspace_write`. There is a third tool (`workspace_publish`, `superchat-tools.el:1040`) and one
   direct product-code caller (`superchat-workflow.el:691`).
3. `### Define a synthesized Tool` still carried K1's "must not already be a built-in or MCP tool
   name" while the shadow invariant had been corrected in K2 to the narrower, true scope — **the
   same Kernel stated one rule two contradictory ways**, an inconsistency K2's own author
   introduced by fixing one site and not the other.

The four stale entries (`Context View`, the compaction lifecycle, `Compact a Session`, `Submit a
Request`) all came from the compact-budget work landed hours earlier.

### Interpretation

**Every contradiction found in three review rounds has been an over-general sentence, and none was
caught by a machine gate.** `check-kernel.sh` verifies the authority map's form; the anchor checker
verifies a cited line is not blank. Neither can read a quantifier. The nine defects found across
K1→K2→K3 were all of one shape: "only", "every", "never", "the single" — a tidier sentence than the
code supports, written by an author who had no reason to go looking for the second call site.

This is why K3's write recorded, for each new universal sentence, where the counterexample was
hunted (below). The habit is cheap and it is the only thing that has actually worked.

A second, structural lesson: **the Kernel can contradict itself, and nothing checks for that.**
Defect 3 existed because a correction was applied to an invariant and not to the Action Contract
stating the same rule. No gate compares two entries against each other.

### Where counterexamples were hunted for K3's universal sentences

Recorded because `KERNEL.md` states the conclusion and not the search:

- "one of two wrappers" — `grep -n "superchat-tool-execute" *.el` over every caller; both wrappers
  and no third; `:prepared-tools` producers enumerated (`superchat-subagent.el` only).
- "exactly three tools … one direct caller" — `grep -rn "superchat-workspace-read\|-write"` over
  all `*.el` excluding `test/` and `experiments/`; one non-tool caller.
- "consulting no capability at all" / "only when :source is override or lm-studio-runtime" —
  read `superchat--llm-context-budget-chars` (`superchat-llm.el:197`-`247`) branch by branch.
- "never reaches the Request Plan, the Run entry, the receipt or the Tape" —
  `grep -n ":context-max-chars"` across the tree; only Turn state.
- "every envelope builder publishes version 1" — all five construction sites read
  (`superchat-tape-view.el:1023`, `:1046`, `:1148`, `:1172`, `:1350`).
- "four outcomes" / "no caller passes max-chars" — `grep -n parse-completed-summary` → one
  definition, one call site, one argument.
- "fifteen validated keys, and a key not on the list is not checked" — `superchat-runtime--validate-request`
  read line by line and every `plist-member` / `plist-get` enumerated; `superchat-runtime-submit`
  likewise for the eleven read-but-unvalidated keys.
- "zero readers" for `superchat-compact-message-count`, `superchat-mcp-health-timeout`,
  `superchat-handoff-char-threshold` — `grep -rn` over the whole repository, test hits counted
  separately.

### Recommended next action

- A gate that compares Kernel entries against each other — the same rule stated in an Invariant and
  in an Action Contract must not disagree — would have caught defect 3 at zero cost. Worth a `plan`.
- `### One window capability, two different trust rules` is recorded as enacted behaviour, but the
  re-scan flagged it as possibly a defect rather than semantics. That judgement was deferred, not
  made.

### Verification

`docs/spec-agents/check-kernel.sh .` → `ok: authority map form is valid.` (24 authority rows).
All 463 `source:` references resolve: 0 missing files, 0 out of range; the 16 lines a
definition-heuristic flags are the pre-existing multi-line SQL/call continuations and
invariant-stating comments carried since K1. All 57 newly added references were printed and read
individually; one was re-anchored during the check (`superchat.el:1603` → `:1302`, help table →
command table) and one is a deliberate comment anchor (`superchat.el:9`, the `Package-Requires`
line). Suite: 967 tests, 951 as expected, 0 unexpected, 16 skipped. Entry counts: 75 `###` entries,
`since:` 54 K1 / 5 K2 / 16 K3.

### References

- `.scratch/start/REPORT.md` — re-scan #2, diff reports ①②③ and the four corrections to prior records
- `E-20260827-010` — corrected in place by this round (item 3's mechanism)
- `E-20260827-009` — the first review round, same defect shape

### Correction (2026-08-28, fresh-context review after the author's own adversarial round)

An independent reviewer on a fresh context, told to default to refuting, re-opened every changed K3 entry after the author's 12-agent round had already fixed 20 defects. It found two more and one omission, all corrected in place: the Run-lifecycle inline anchor for the streaming `running-model` guard pointed at `superchat-runtime.el:1733` (the `superchat--llm-request-max-tokens` `let`) instead of `:1740` — the author's own round had moved a correct `:1740` to the wrong line, so a review pass introduced a defect of the very class it was hunting; the Compact contract's `:max-tokens` formula omitted the `(when (> summary-char-limit 0) …)` guard at `superchat-compact.el:523`, under which a zero budget places no `:max-tokens`; and the new `always`-grant Relation listed three bounds and omitted the unattended side-effect floor above them in the same `cond` (`superchat-authority.el:572`). Twenty-two other changed items were confirmed clean with counterexample greps. Reviewed at KERNEL.md SHA-256 `d28fd986…`.

## 2026-08-27 — The silent context-overflow detector is dead against the declared dependency and live only on this machine

id: E-20260827-012
status: confirmed
scope: `superchat--llm-context-overflow-response-p` and the `overflow` recovery edge;
supersedes the loose note `docs/finding-context-overflow-signal.md`, now removed
source: found by the compact-budget worker during that SPEC; re-verified here
verification: greps over the declared dependency and over the fork on this machine's `load-path`

### Observation

`superchat--llm-context-overflow-response-p` (`superchat-llm.el:285`) decides whether a provider
response was silently truncated by a context overflow. All three of its branches require a non-nil
`:stop-reason`. It **does** have a production consumer — `superchat-llm.el:1676`, whose truthy result
drives `recover-context 'overflow` — so it is not dead code in the "never called" sense.

Measured:

| where | occurrences of `stop-reason` |
| --- | --- |
| declared dependency `llm 0.31.0` (`superchat.el:9` pins `0.31.1`) | **0 files** |
| declared dependency `llm 0.31.1` | **0 files** |
| the fork at `~/Documents/emacs/package/llm/`, at its committed HEAD | **0** |
| the same fork's **uncommitted working tree** | **15** occurrences, 4 files changed, +104/−36 |

`~/.config/nova-emacs/modules/nova-ai.el:99` puts that fork first on `load-path`.

### Interpretation

Against the dependency this project declares, the predicate is constantly nil and the `overflow`
recovery edge is unreachable — so it is not enacted semantics of superchat, and `KERNEL.md` excludes
it on exactly that ground (the same shape as `epistemic-learning-v0`: consumer complete, producer
absent from the shipped product).

**But on the machine this project is developed on, it fires** — for OpenAI-compatible providers —
because of an uncommitted patch in a sibling repository. That gap matters more than the dead branch
does: a behaviour that works for the maintainer and for nobody else is the hardest kind of defect to
notice, because every local test of it passes. The retired note recorded the first half of this and
was silent about the second.

### Recommended next action

- Do not implement against `:stop-reason` until it exists in a released `llm`. The compaction
  completion marker remains the only usable truncation signal for compacted output.
- If the fork's patch is wanted, it belongs upstream in `llm`, not as a local assumption here.
- Worth stating in `CONTEXT.md` or a Runbook that this machine's `load-path` differs from the
  declared dependency, since it silently changes which code paths are live. Not promoted here —
  that needs `plan`.

### Verification

`grep -rl stop-reason ~/.config/nova-emacs/elpa/llm-0.31.0 ~/.config/nova-emacs/elpa/llm-0.31.1`
→ 0 files each. `grep -rn stop-reason ~/Documents/emacs/package/llm/*.el` → 15; `git diff --stat`
in that repo → 4 files changed, +104/−36; `git grep stop-reason HEAD` there → 0.
`grep -n "package/llm" ~/.config/nova-emacs/modules/nova-ai.el` → `:99`.

### References

- `superchat-llm.el:285` (predicate), `:1676` (its consumer), `superchat.el:9` (declared dependency)
- `KERNEL.md` — `### Excluded from the enacted sections`, second paragraph
- retired: `docs/finding-context-overflow-signal.md` (untracked, folded into this entry and deleted)

### Addendum (2026-08-28) — K3 as first written contained twenty defects; all were found by adversarial review before hand-off

The `learn` above wrote K3, ran the five verifications, and passed all of them:
`check-kernel.sh` ok, 0 out-of-range references, 0 blank anchors, 967/951/0/16, doctrine untouched.
An independent adversarial pass over the freshly written text then found **20 defects that every
one of those gates had accepted**, and a second, refuting pass upheld all 20 while overturning 6
other accusations. They fall into three kinds:

**Over-general sentences I wrote (10).** The same shape as every previous round. `Tool` said a
"subagent **or team**" Run takes the subagent wrapper — a team lead's Run has no `:prepared-tools`
and takes the agent-loop one; the split is `subagent` against everything else. `Workspace Artifact`
said the model surface is "only three tools" — there is a fourth, `workspace_info`, and a
buffer-region Artifact can be rewritten by the ordinary buffer tools with no workspace tool at all.
The same entry said "every path goes through the same revision check", **contradicted by the very
next sentence of its own paragraph**, which stated the true narrower rule. `Compact a Session` said
four outcomes where there is an unguarded fifth. The `always` grant said the Session bounds it; a
`/clear` in the same buffer carries it across. `Workspace --binds-to--> Session` said workflow facts
go "never to a chat Session" when the workflow binding is the last resort of a three-step
resolution.

**Anchors that drifted inside entries I rewrote but did not re-verify (7).** This is a process
defect, not a wording one, and it is mine. The re-scan report listed nine drifted references and I
re-anchored exactly those nine — but `superchat-compact.el` had been rewritten (+337/−99) the same
day, and the K1-era anchors *inside the compaction entries I was rewriting* had drifted too:
`:385`-`:389` → `:591`-`:595`, `:519`/`:524` → `:746`/`:751`, `:466`-`:468` → `:677`-`:679`, `:162`
→ `:143`. Each still landed on real code, so the blank/comment checker passed them; they simply
pointed at the wrong real code. **A reference that moves onto other live code is invisible to every
mechanical check there is.**

**Claims that were true but attached to the wrong subject (3).** `:capability` was listed among the
keys `superchat-runtime-submit` reads without validating — submit does validate it, through a
separate function the precondition also mis-attributed to `validate-request`. The new
two-trust-rules invariant said "the same capability plist", when the two sites derive two
independent capabilities from different models.

### What this changes about the method

The five verifications are necessary and not close to sufficient: **they check form, not truth, and
they cannot see a reference that lands on the wrong live construct.** Three review rounds have now
each found defects that the gates passed, and the count is not falling — 6, then 4, then 20, the
last being the largest single write. The reviewer's own record is also worth keeping: 6 of its 26
accusations were overturned by the refuting pass, two of them cases where it asked for a correct
invariant to be weakened into a false one.

Two mechanical checks would have caught whole classes here and neither exists: comparing each
reference's cited construct against the *name the sentence uses* (would have caught all 7 anchors),
and checking that two entries stating the same rule agree (would have caught the revision-check
contradiction, which sat two sentences apart in one paragraph).

## 2026-08-28 — KERNEL.md K3 → K4: every code citation names its construct, and 40 of them had been pointing at the wrong file

id: E-20260828-001
status: confirmed
scope: `KERNEL.md` (K3 → K4, a re-anchor that changes no sentence and no `since:`); the checker
`test/kernel-check.el` and its 13 ERT tests; the Kernel review practice; one scratch-root incident
source: `plan` confirmed 2026-08-28; `do` by codex (work order `KERNEL-CHECK-BRIEF.md`), reviewed
in two fresh-context rounds (`rescan/kc-refute.md`, `rescan/kc-refute-2.md`, §C of the second being
the hand-fix list); `learn` work order `K4-LEARN-BRIEF.md`; this entry's own scans and probes
verification: `emacs -Q --batch -L . -l test/kernel-check.el -- check KERNEL.md`; the
`convert` round trip; `bash docs/spec-agents/check-kernel.sh .`; an annotation-stripped diff
against the K3 snapshot; two drift probes on real-copy scratch roots;
`emacs -Q --batch -L . -l test/run-tests.el`

### Observation

**The format.** Every `.el` citation in `KERNEL.md` now reads `file.el:NNN@construct`, the
construct being the top-level form that contains line NNN as the Lisp reader sees it
(`kernel-check--construct-at`). On the K3 text with its citations hand-corrected (below), the
converter reported `refs 761, inserted 751, errors 0, warnings 2`; `check` on its output reports
`errors 0, warnings 2`; converting the output again inserts 0 and is byte-identical. The two
warnings are the two citations that point at comments and therefore have no form: line 9 of
`superchat.el` (`Package-Requires`) and line 215 of `superchat-compact.el`. The commander's work
order records that the definition heuristic used through K3 had flagged 179 lines on a Kernel a
reader found clean; that number is reported there, not re-measured here.

**40 citations were pointing at the wrong file — 21 found by the independent checker review, 18
by the author's scan of every short citation, and one more (`:1590`) by the fresh-context review of
K4 itself — and 32 of them passed every mechanical check.**
A short citation `` `:NNN` `` inherits the file of the previous complete citation in its entry. In
eleven prose lines the author had switched files mid-sentence and gone on citing short, so the
short citations inherited a neighbouring file:

- 8 were caught by the range check on K3 itself, because the inherited file's line was blank or
  beyond its end (`superchat-authority.el:1627`, `:1740`; `superchat-tape.el:511`, `:531`, `:560`;
  `test/test-superchat-mcp.el:830` twice; `superchat-tools.el:557`).
- 13 more were found by the second reviewer reading the five entries those errors sat in
  (`kc-refute-2.md` §C rows 9–21).
- 18 more were found here by two scans over all 171 short `.el` citations: (a) the nearest
  backticked symbol before each citation, against the file that defines that symbol; (b) each
  citation's inherited `file:line`, against the entry's own `source:` line. Scan (b) alone flagged
  34 of the 39; the five it missed are range ends or lines the `source:` line does not list, all
  adjacent to a flagged one. The 18: `Approval and tool-execution lifecycle` (line 358) had twelve
  citations of `superchat-tool-execute`, `--record-failure` and `--invoke` inheriting
  `superchat-llm.el` from `superchat-llm.el:1282-1283`; `Synthesized-tool lifecycle` cited
  `superchat--syn-register-raw` as `superchat.el:526`; `Compaction and Anchor lifecycle` cited
  `superchat-compact--collect-messages` and `--keep-recent-messages` as `superchat-tape.el:427`
  and `:406`; the synthesized-name invariant cited `superchat--syn-existing-names` and
  `--syn-valid-name-p` as `superchat-tools.el:92` and `superchat-mcp.el:112`; the excluded-section
  note cited `superchat-llm.el`'s `evidence-contract` read as `superchat-runtime.el:1324`.
  Every one of the 31 landed on real, unrelated code (`superchat--provider-with-chat-model`,
  `superchat-show-ttft-breakdown`, `superchat-tape-append-event`, an MCP test …).

Each of the 39 was fixed by writing the file name out; the construct the converter then computed
matched the one the sentence names in all 39 cases (the full old → new table is in
`K4-CHANGES.md`). The 21 in §C were confirmed as listed; the review's list was short by 18 because
it examined only the entries in which errors had surfaced.

**Drift probes**, on two scratch roots made with `cp` of every `.el` and every cited `.md`/`.sh`
(`find -type l` → 0 before each write):

- One comment line inserted above `superchat-tool-execute` (above the autoload cookie at
  `superchat-authority.el:1131`):
  ```
  error: superchat-authority.el:1132@superchat-tool-execute: expected construct superchat-tool-execute, actual nil, line 1132: ;;;###autoload
  (× 6 — the six citations of that line)
  refs 772, errors 6, warnings 2   exit 1
  ```
- One line inserted inside that function's body (after line 1200, which is inside it):
  ```
  refs 772, errors 0, warnings 2   exit 0
  ```
  All 26 `superchat-authority.el` citations beyond the insertion stayed green with line numbers
  now one off. The construct check sees a form boundary being crossed and nothing else.

**Suite** after K4: `Ran 980 tests, 964 results as expected, 0 unexpected, 16 skipped`
(967 + 13 `kernel-check/`), `RUNNER_GATE_OK`. No `superchat-*.el` or `test/` file was written by
this `learn`; the newest mtimes under those paths (`test/test-kernel-check.el` 06:45,
`test/kernel-check.el` 06:43) precede the work order (07:00).

**Incident, as reported by the commander and verified here in its recovered state.** While
building drift probes, two separate agents each built a scratch root as a *symlink* to the checkout
and then wrote "into the copy" with `awk … > file`; the shell's `rm -i` alias had turned the
intended `rm` of the link into a prompt, the link survived, and the redirect wrote through it. The
real `superchat-compact.el` was truncated to 0 bytes once and `superchat-authority.el` once.
`superchat-compact.el` was recovered byte-exact from the JJ working-copy commit
(`jj --ignore-working-copy file show -r @ superchat-compact.el`, 36308 bytes / 787 lines);
`superchat-authority.el`, which had no uncommitted changes, from `git show HEAD:`. Verified now:
`superchat-compact.el` SHA-256 `04c696d1…` equals both the `jj file show` output and the scratch
copy `compact-recovered.el`; `superchat-authority.el` SHA-256 `839fce69…` equals
`git show HEAD:superchat-authority.el`, and `git status` shows no change to it. The suite ran
973 / 957 / 0 / 16 immediately after both recoveries; the current suite runs 980 / 964 / 0 / 16
after the checker gained 13 more tests.

### Interpretation

- The line number was never the anchor; the form is. A citation with a construct name survives
  any edit that does not move its line across a form boundary, and the checker names the form
  when it does. The price is that a green check proves the construct and not the line.
- **Wrong-file inheritance is now the largest known class of undetected Kernel defect** — 32
  silent cases, more than any one of the three earlier review rounds found (6, 4 and 20, per
  `E-20260827-012`) — and it is authorial,
  not mechanical: every case sits where a sentence cites one file and then another. No checker can
  see it, because the wrong line is real code. What does see it is the entry's own `source:` line
  (34 of 39), which is why that line is worth keeping complete.
- The reviewer's list was incomplete for a scoping reason, not a reading one: an error in an
  entry is a signal to scan *every* short citation, not the entries with errors. Two 40-line
  scripts did in minutes what five entries of hand reading had done in part.
- The incident is the same shape as the K3 anchor lesson: a step that looked read-only (writing
  into a scratch copy) was destructive because of one environmental fact (a symlink) plus one
  masked failure (an aliased `rm`). The recovery was cheap only because both files had a committed
  or working-copy-committed version to restore from.

### Where counterexamples were hunted, for the universal sentences in `### Anchor format since K4`

- "no sentence of any entry above changed … every entry keeps the `since:` it had at K3" — a
  range-aware annotation strip (`perl -pe 's/@[^`]*?(?=-\d+@|,\d+@|`)//g'`) of K4 diffed against
  the K3 snapshot: only the three header lines, the eleven corrected prose lines and the new entry
  differ; on each of the eleven, the line with every code span masked is identical to K3's;
  `since:` counts 54 / 5 / 16 / 1. (The brief's `sed` strip also reports line 535, an artefact:
  it eats the `-311` range end out of `superchat-db--ensure-schema-1-311`.)
- "all 39 … came from inheritance" — all 39 are short citations, by the fix table; the other 132
  short `.el` citations passed both scans and were not each read.
- "eight … blank or out of range, and the other 31 …" — the eight errors `check` printed on K3;
  39 − 8.
- "Exactly two citations … are left bare … a clean `check` prints exactly those two warnings and
  `--strict` cannot pass" — `check KERNEL.md` → `warnings 2`, both listed; `check --strict` →
  `errors 2, warnings 2`, exit 1.
- "a line inserted above a form fails the citation of that form's first line, while a citation
  deeper in the same form keeps its name" — probes A and B above.
- "`.md` and `.sh` citations … receive only the existence, range and blank-line checks" —
  `kernel-check--validate-reference` computes construct information only when
  `kernel-check--el-file-p`; `kernel-check--reference-location` has exactly the three statuses
  `missing`, `out-of-range`, `blank` plus the unreadable fallback.

### Recommended next action

- **Lesson candidate, not promoted here** (needs `plan`, then `docs/lessons/`): a scratch root is a
  real `cp` copy, never a symlink, and is checked with `test -L` before every write; a destructive
  step is verified before the next step runs. Scope: any agent writing into a copy of the tree.
- Checker candidate (needs `plan`): warn when a short citation's inherited `file:line` is absent
  from the entry's `source:` line while another file at that line is present — the scan that found
  34 of 39 here.
- Authoring rule already in `docs/protocols/kernel-review.md` by implication and worth making
  explicit at the next Protocol revision: after a file switch inside an entry's prose, the next
  citation is complete.
- `E-20260827-011` asked for a gate comparing entries that state one rule; `pairs` lists such
  entries but does not compare them. Still open.

### Verification

```
$ emacs -Q --batch -L . -l test/kernel-check.el -- check KERNEL.md
warning: [Excluded from the enacted sections] bare .el citation superchat-compact.el:215
warning: [Excluded from the enacted sections] bare .el citation superchat.el:9
refs 772, errors 0, warnings 2
$ emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md
… refs 772, errors 2, warnings 2      (exit 1)
$ bash docs/spec-agents/check-kernel.sh .
ok: authority map form is valid.
$ emacs -Q --batch -L . -l test/kernel-check.el -- pairs KERNEL.md OUT.md
wrote OUT.md: entries 76, shared groups 295, universal sentences 169
$ emacs -Q --batch -L . -l test/run-tests.el
Ran 980 tests, 964 results as expected, 0 unexpected, 16 skipped
```

Entry counts: 76 `###` entries, 24 authority rows, `since:` 54 K1 / 5 K2 / 16 K3 / 1 K4.

**Correction (2026-08-28).** The transcript above records `universal sentences 169`. The
reproducible figure is **168** — run three times by the K4 author and twice independently in
another session, deterministic each time, on the same `KERNEL.md` that yields the matching
`entries 76` and `shared groups 295`. Only the universal-sentence count was off by one; the two
`K4.md` copies under `.scratch/` give a different triple (75 / 306 / 163) and are not the file
measured here, so this was a transcription slip rather than a measurement of the wrong input. The
operational record that gates on this number, `docs/runbooks/kernel-check.md:61`, has been corrected
to 168; this transcript is left as written, per `docs/spec-agents/WORKFLOW.md:194`.

### References

- `E-20260827-009`, `E-20260827-011` — the two earlier rounds; same defect shape, now with a
  fourth kind (wrong-file citation) that is silent
- `rescan/kc-refute-2.md` §C — the 21-row hand-fix list; `K4-CHANGES.md` — all 39, old → new,
  with the verification outputs verbatim (commander's scratchpad)
- `docs/runbooks/kernel-check.md`, `docs/protocols/kernel-review.md` — promoted from this entry

### Verification (fresh-context review of K4, 2026-08-28)

An independent fresh-context reviewer (`scratchpad/rescan/k4-refute.md`) verified all 39
corrections recorded above as correct (0 wrong file, 0 wrong construct), found the 40th
wrong-file citation (`:1590`, corrected above), found the `define-error` naming bug in the
checker (fixed in the checker; one citation — `superchat-workspace.el:42` — re-annotated), and
flagged three over-general sentences in the `### Anchor format since K4` entry, which were
narrowed the same day. This is the review that protocol rule 3 of
`docs/protocols/kernel-review.md` requires. The checker's first live catch was against its own
citations: the `define-error` fix shifted `test/kernel-check.el` and `test/test-kernel-check.el`,
`check` reported all 7 affected citations in the new K4 entry's own `source:` line, and they were
re-anchored by content the same day.

## 2026-08-29 — KERNEL.md K4 → K5: declarable bare citations and documented pairs narrowing

id: E-20260829-001
status: confirmed
scope: `KERNEL.md` K4 → K5; `docs/runbooks/kernel-check.md`; `EVIDENCE.md`; `STATUS.md`; `test/kernel-check.el`; `test/test-kernel-check.el`; `docs/protocols/kernel-review.md` was inspected and remains unchanged; no product `.el` file was changed by this fix
source: first `do` work order `K5-DO-BRIEF.md`; its independent `REFUTE-REPORT.md` (D1–D9 and T1–T7); second `do` work order `K5-DO2-BRIEF.md` and `K5-DO2-REPORT.md`; the learn work order `K5-LEARN-BRIEF.md`; this K5-FIX3 work order `K5-FIX3-BRIEF.md`
verification: the strict checker, convert round trip, pairs report, authority-map check, standalone kernel-check ERT, full `run-tests.el`, `jj diff --stat`, warning count, and fresh-context review are recorded below; the jj diff-stat item is summarised rather than verbatim because its insertion/deletion counts include `EVIDENCE.md`

### Observation

K5 changes the Kernel header at lines 4 and 8–10, changes the `### Anchor format since K4` entry at lines 650–662, and marks the two intentional bare citations at the final excluded-section lines 676 and 704 as `@-`. The entry title remains `### Anchor format since K4`; its `since:` is now K5 and its source line names the final checker functions, tests, and this evidence id.

The revised Anchor entry states the actual K5 contract: `@-` declares an empty expected construct; a marked `.el` citation passes when the actual construct is empty and errors with `declared bare` plus the actual construct when code occupies the line; a marker on `.md` or `.sh` is an error; malformed annotated citations are errors; non-symbol-headed top-level objects yield their Lisp type names; `convert` does not invent `@-` and leaves a real `-`-headed form alone; and `pairs` falls back to `file:LINE` for `@-`.

The runbook now describes the `@-` row, non-`.el` marker errors, malformed citations, the complete narrowing vocabulary, `narrowed by:` output, the K5 summary, the strict gate, and the T1 text-blindness limit. `STATUS.md` records K5 as complete with evidence `E-20260829-001`, removes K5 from the next-action queue, and leaves the Blocker 2 and three-defect capture text in place.

The earlier K5-DO2 report stated the T1 conclusion but did not retain its concrete transcript.  This entry now records the reproduction: a scratch root was populated with `cp` copies of the cited `.el`, `.md`, and `.sh` files; before the swap, `test -L` returned 1 for `KERNEL.md`, `superchat.el`, and `superchat-compact.el`.  The source lines `superchat.el:9` and `superchat-compact.el:215` were then exchanged while the Kernel's two `@-` citations stayed unchanged.  The check command was `emacs -Q --batch -L /private/tmp/K5-FIX3-T1-kwtLOb/root -l /private/tmp/K5-FIX3-T1-kwtLOb/root/test/kernel-check.el -- check --strict --root /private/tmp/K5-FIX3-T1-kwtLOb/root /private/tmp/K5-FIX3-T1-kwtLOb/root/KERNEL.md`; the exact probe output was:

~~~~text
swapped source lines:
;; Token-aware recovery below owns automatic pre-send compaction.  Keep the
;; Package-Requires: ((emacs "29.1") (llm "0.31.1"))
strict output:
refs 806, errors 0, warnings 0
KERNEL test -L exit=1
superchat.el test -L exit=1
superchat-compact.el test -L exit=1
check exit=0
~~~~

This demonstrates the accepted T1 boundary: `@-` verifies structural absence from a top-level form, not the text of that line.

### Interpretation

The first `do` implemented the checker behavior. Its independent refute review found D1–D3 as the missing Kernel/runbook delivery and D4–D9 plus T1–T7 as defects or design findings. The second `do` fixed D4 (reserved `@-` pairing key), D5 (reserved `-` conversion), D6 (same-sentence self-exclusion), D7 (narrowed-first ordering test), D8 (actual-construct message test), D9 (duplicate sentence position handling), T3 (malformed citation error), T4 (non-`.el` marker error), T5 (verb-form narrowing terms), and T6 (explicit case-folding). This `learn` fixes D1–D3 in the allowed documents.

T1 is accepted as an explicit tradeoff: `@-` checks structural placement but does not anchor line text, so the two marked lines receive a human check on every Kernel revision. T7 is also accepted: sentence splitting remains punctuation-based outside code spans, so Markdown emphasis and abbreviations can leave fragments in the `pairs` view; this is a report-view limitation, not a Kernel semantic claim. T2 remains a measurement caveat about same-entry narrowing pairs and is not promoted into Kernel semantics. The `pairs` report showed that the other entries sharing Anchor constructs (`Session`, `Run`, `Capability and Topology`, and `Excluded from the enacted sections`) do not state the K5 marker rule, so no unrelated entry was changed.

The protocol file did not need a revision: its `scope` already covers every post-K4 Kernel revision, and its mechanical-gate reference remains correct after the runbook gate becomes `check --strict`.

### Where counterexamples were hunted

The following are the new or reworded universal clauses in `### Anchor format since K4`, with the searches and branch reads used before admitting them:

- The clause that a bare `.el` citation without `@-` warns while `--strict` treats it as an error, and that `@-` passes for an empty actual construct but errors for a non-empty one, was checked with `rg -n 'kernel-check--validate-reference|kernel-check--mismatch-format|declared bare|expected "-"' test/kernel-check.el test/test-kernel-check.el`. I read every branch of `kernel-check--validate-reference` at `test/kernel-check.el:486–518` and `kernel-check--mismatch-format` at `:638–644`, then re-ran the `intentional-bare` fixture at `test/test-kernel-check.el:488` and the reserved-construct conversion fixture at `:462`.
- The clause that `.md` and `.sh` citations carry no construct, receive the existence, range and blank-line checks, and reject markers, was checked with `rg -n 'kernel-check--el-file-p|kernel-check--mismatch-format|construct-markers-on-non-el' test/kernel-check.el test/test-kernel-check.el`. I followed the non-`.el` and `.el` branches in `kernel-check--validate-reference` (`:486–518`) and the exact error-format branch (`:638–644`), and ran `kernel-check/construct-markers-on-non-el-files-are-errors` at `test/test-kernel-check.el:668`.
- The clause that a top-level object with no symbol head yields its Lisp type name was checked with `rg -n 'kernel-check--form-construct-name|type-of|non-symbol-top-level-objects-get-type-names' test/kernel-check.el test/test-kernel-check.el`. I read the non-symbol branch at `test/kernel-check.el:326–343`; the new fixture at `test/test-kernel-check.el:763` covers a list-headed form, a literal string, and a vector, checks one `declared bare` error for each `@-` citation, and checks bare conversion to `@cons`, `@string`, and `@vector`. The commander's scan of all `superchat*.el` and `test/*.el` found zero non-symbol-headed top-level forms, so no existing citation required re-anchoring for this behavior.
- The malformed-citation clause was checked with `rg -n 'malformed-citation|citation-specs-or-report|citation-regexp' test/kernel-check.el test/test-kernel-check.el`. I read the malformed-span detection at `test/kernel-check.el:982–990`, its parser caller at `:200–246`, and ran `kernel-check/malformed-annotated-citations-are-errors` at `test/test-kernel-check.el:638`.
- The clause that `@-` only asserts absence from every top-level form and does not anchor line text was checked with `rg -n 'kernel-check--source-line|kernel-check--construct-info|kernel-check--validate-reference' test/kernel-check.el`. I read the source-line status path at `:418–434`, the reader/form path at `:372–402`, and the validation branches at `:486–518`; the T1 real-copy line-swap probe and its green strict result are recorded in this entry's Observation below.
- The `convert` clause was checked with `rg -n 'kernel-check-convert|reserved construct|equal construct "-"' test/kernel-check.el test/test-kernel-check.el`. I read every conversion branch at `test/kernel-check.el:573–636`, including the `-` special case, and ran `kernel-check/converter-leaves-minus-construct-bare-and-checkable` at `test/test-kernel-check.el:462`; the D5 mutation in `K5-DO2-REPORT.md` is the counterexample that forced the special case.
- The `pairs` clause was checked with `rg -n 'kernel-check--pair-key|kernel-check--render-pairs|reserved-bare' test/kernel-check.el test/test-kernel-check.el`. I read the key fallback at `test/kernel-check.el:646–662`, the render path at `:742` onward, and ran `kernel-check/pairs-does-not-share-reserved-bare-citations` at `test/test-kernel-check.el:376`.

These searches also cover the T1 compensating statement in the runbook. No new K5 universal sentence was admitted without a corresponding search record here.

### Verification

#### 1. `check --strict KERNEL.md`

~~~~text
refs 806, errors 0, warnings 0
~~~~

#### 2. `convert` round trip and `cmp`

~~~~text
refs 806, inserted 0, errors 0, warnings 0
convert_dir=/private/tmp/K5-FIX3-convert-wACmu9
source test -L exit=1
convert exit=0
cmp exit=0
~~~~

#### 3. `pairs KERNEL.md OUT.md`

~~~~text
wrote /private/tmp/K5-FIX3-pairs-odSBte/OUT.md: entries 76, shared groups 314, universal sentences 167, narrowed 95 in 30 entries
pairs_dir=/private/tmp/K5-FIX3-pairs-odSBte
pairs exit=0
~~~~

#### 4. `bash docs/spec-agents/check-kernel.sh .`

~~~~text
ok: authority map form is valid.
~~~~

#### 5. standalone kernel-check ERT

~~~~text
Ran 24 tests, 24 results as expected, 0 unexpected (2026-08-29 03:30:07-0700, 0.243132 sec)
~~~~

#### 6. full `run-tests.el`

~~~~text
$ emacs -Q --batch -L . -l test/run-tests.el
Ran 991 tests, 975 results as expected, 0 unexpected, 16 skipped (2026-08-29 03:30:18-0700, 10.396364 sec)
RUNNER_GATE_OK runner-sha=2c4d65a8080d2d5a014d9e23d9074a9cbff7bf9594cc469cfe8ef66c9b85c6da contract-sha=165ae98ba71a782d29d1e63ed09a58c1eb87445387bd38b6ac70c87b36f1b15a
~~~~

#### 7. `jj diff --stat`

~~~~text
`jj diff --stat` lists 31 files; the six K5 files (`KERNEL.md`, `docs/runbooks/kernel-check.md`, `EVIDENCE.md`, `STATUS.md`, `test/kernel-check.el`, `test/test-kernel-check.el`) are among them; the other 25 are pre-existing working-copy changes from earlier deliveries, untouched by K5.
~~~~

#### 8. `grep -c 'warning'` on strict-check output

~~~~text
refs 806, errors 0, warnings 0
0
check exit=0
~~~~

Entry counts: 76 `###` entries, 24 authority rows, `since:` 54 K1 / 5 K2 / 16 K3 / 0 K4 / 1 K5. The K5 `pairs` summary is `entries 76, shared groups 314, universal sentences 167, narrowed 95 in 30 entries`.

#### 9. `rg -n -F '@-' KERNEL.md`

~~~~text
655:K4 is a pure re-anchor: no sentence of any entry above changed, and every entry keeps the `since:` it had at K3.  What changed is the citation form.  From K4 a citation of an Emacs Lisp line carries, after `@`, the name of the top-level form that contains the line — `superchat-runtime.el:1627@superchat-runtime-submit` — computed by `test/kernel-check.el` (`kernel-check--construct-at`), which reads the cited file with the Lisp reader and takes the form whose extent covers the line: a named `defun`, `defmacro`, `defvar`, `defvar-local`, `defcustom`, `defconst`, `defsubst`, `cl-defun`, `cl-defmacro`, `cl-defstruct`, `cl-defgeneric`, `cl-defmethod`, `ert-deftest` or `define-*` form (its quoted first argument resolved to the symbol) yields its name, an unnamed top-level form yields `HEAD/FIRST-QUOTED-SYMBOL` or bare `HEAD`; a top-level object with no symbol head — a literal string, a vector, a list-headed form — yields its Lisp type name (`string`, `vector`, `cons`) (`test/kernel-check.el:326@kernel-check--form-construct-name`; `test/test-kernel-check.el:763@kernel-check/non-symbol-top-level-objects-get-type-names`), and a line inside no form — a comment, the file header, a blank line — yields nothing.  A short citation `:NNN` still names no file of its own; it inherits the file of the previous complete citation in the same entry, and that inheritance is where all 40 wrong-file references corrected in this revision came from — 21 found by the independent checker review, 18 by the author's scan of every short citation, and one more (line 1590) by the fresh-context review of K4 itself: eight were caught by the range check because the inherited file's line was blank or out of range, and the other 32 landed on real code in another file, invisible to `check`, and were found only by comparing the inherited file against the entry's own `source:` line and the construct the sentence names.  The checker's rule (`kernel-check--validate-reference`): a citation whose file is missing, whose line is out of range, or whose line is blank is an error; an annotated `.el` citation whose line is no longer inside the named form is an error naming both the expected and the actual form; a bare `.el` citation without `@-` is a warning, and `--strict` treats it as an error; an annotated `.el` citation with `@-` declares an empty expected construct, so an empty actual construct passes while a non-empty actual construct is an error whose message includes `declared bare` and the actual construct (`test/kernel-check.el:486@kernel-check--validate-reference`; `test/test-kernel-check.el:488@kernel-check/intentional-bare-citations-validate-and-convert`).  `.md` and `.sh` citations carry no construct; they receive the existence, range and blank-line checks, and a marker on them is an error (`test/kernel-check.el:486@kernel-check--validate-reference`; `test/test-kernel-check.el:668@kernel-check/construct-markers-on-non-el-files-are-errors`).  A citation shaped like the text “x.el:7@” is an error rather than a silently discarded citation (`test/kernel-check.el:982@kernel-check--malformed-citation-regexp`; `test/kernel-check.el:986@kernel-check--citation-specs-or-report`; `test/test-kernel-check.el:638@kernel-check/malformed-annotated-citations-are-errors`).  At K5, the two intentional bare citations in this document carry `@-`: `superchat.el:9@-` for the package header and `superchat-compact.el:215@-` for the excluded-section comment; `check --strict` is the gate and reports no warnings on this file (`test/kernel-check.el:525@kernel-check--check-buffer`; `E-20260829-001`).  The anchor is the form, not the line: a construct check sees drift when a cited line crosses a form boundary or the form is renamed; a shift that stays inside the same form is invisible to it, so a line inserted above a form fails the citation of that form's first line, while a citation deeper in the same form keeps its name and stays green with a line number that is now one off.  The line number therefore remains the reader's guide inside the form; the name is what the checker holds.
657:`@-` only asserts that the cited line is outside every top-level form; it does not anchor the line's text.  Swapping the two comment lines while keeping their `@-` citations leaves `check --strict` green (`test/kernel-check.el:486@kernel-check--validate-reference`; T1 probe recorded in `E-20260829-001`).
659:`convert` does not write `@-`; choosing this marker is a `learn` judgment, and a real top-level form whose head is `-` is left unconverted (`test/kernel-check.el:573@kernel-check-convert`; `test/test-kernel-check.el:462@kernel-check/converter-leaves-minus-construct-bare-and-checkable`).
661:In `pairs`, an `@-` citation is grouped by `file:LINE`, so it does not create a shared construct group (`test/kernel-check.el:646@kernel-check--pair-key`; `test/test-kernel-check.el:376@kernel-check/pairs-does-not-share-reserved-bare-citations`).
676:directly, and deliberately never registered on any hook, as the comment at `:215@-` says — together
704:dependency `llm 0.31.1` (`superchat.el:9@-`) never supplies one.  Against the dependency this
~~~~

### References

- `K5-DO-BRIEF.md`, `REFUTE-REPORT.md` — first code delivery and independent D1–D9/T1–T7 review
- `K5-DO2-BRIEF.md`, `K5-DO2-REPORT.md` — second code delivery, final checker behavior, and mutation results
- `K5-LEARN-BRIEF.md` — this document revision scope and acceptance contract
- `KERNEL.md`, `docs/runbooks/kernel-check.md`, `STATUS.md` — K5 documentation surfaces
- `docs/protocols/kernel-review.md` — unchanged protocol governing this revision

### Verification (fresh-context review of K5)

A fresh-context codex instance (`gpt-5.6-luna`, max reasoning) in pane `w3:p3J` was told to default to refuting; its report is `K5-REFUTE2-REPORT.md` (commander's scratchpad, 2026-08-29). It returned 5 accusations / 4 accepted trade-offs / 7 attempted-and-failed refutations. On a real `cp` copy it also applied reverse mutations to each of the second round's 10 fixes; all 10/10 made the tests red.

The commander (pane `w3:p1M`) reviewed the findings one by one. #1 was confirmed by an independent reproduction (three `@-` citations into a list-headed form, a top-level string and a vector passed `--strict`; a fourth into a `defun` errored) and is fixed in this revision through `kernel-check--form-construct-name`, the new test, and the Kernel clause above. #2 was confirmed but accepted as a scope boundary (`check` is specified for `KERNEL.md`; the runbook's own teaching spans are not citations). #3 and #4 were confirmed and corrected above. #5 was confirmed and is closed by this section. Trade-offs T1 (`@-` is structural, not textual), T2 (`pairs` narrowing is report pairing, not semantic proof) and T7 (punctuation-driven sentence splitting) are accepted and already recorded in the Kernel entry and runbook.

Re-verification after the fixes:

~~~~text
check --strict: refs 806, errors 0, warnings 0
convert: refs 806, inserted 0, errors 0, warnings 0
standalone ERT: Ran 24 tests, 24 results as expected, 0 unexpected
full run-tests: Ran 991 tests, 975 results as expected, 0 unexpected, 16 skipped
~~~~

## 2026-08-31 — fix-batch3 series: a compaction Anchor poisoned every later Run's audit, and the chat buffer had no transcript ownership (E-20260831-001)

### Observation

Two P0 regression campaigns (`.scratch/arch-test-20260830/p0-concurrency-report.md`,
`p0-compaction-report.md`) independently hit `audit-incomplete` terminals. Forensic
isolation (`audit-incomplete-investigation.md`) reproduced the cause at bare
`HEAD dd16062` with a real LM Studio model: `superchat-tape--row-event` classified any
row whose metadata carried `event_type`, `event_id` **or `schema`** as a Runtime
envelope; a typed compaction Anchor carries a string `schema`, so replay handed
Closure a malformed pseudo-event and the terminal guard failed every subsequent
ordinary Run in that Session (`invalid-envelope` → `audit-incomplete`, plus a
duplicated assistant row from the audit fallback's nil-tape-id re-delivery). The
minimal trigger is one Anchor in the Session; a tiny context budget only exposes it
earlier. Fixing it (batch3) surfaced, over five adversarial review rounds
(implementer: codex/gpt-5.6-luna; reviewer: codex/gpt-5.6-sol, default-to-refute),
five distinct latent defect classes that a green 1,03x-test canonical suite could not
see: (1) schema shape mistaken for replay identity; (2) response storage id reused
for divergent warning content (Entry identity mismatch); (3) `/expand`, compact-log
and async notice sources/idempotency/visibility gaps; (4) prompt/draft and streaming
finalize sharing ownerless markers (draft deletion, late-chunk fragments, stale
markers crossing `/clear`); (5) ownerless generic transcript insertion mis-ordering
or deleting `/commands`/`/backend`/async System output under an active Run.

### Interpretation

The durable layers (Tape append-only, Runtime terminal guard, Authority) held
throughout; every defect lived in classification and UI-region ownership. The final
design separates them explicitly: replay classifies a Runtime envelope only on
`event_type`/`event_id`; the audit warning is its own system material (deliberately
prompt-visible — user ruling 2026-08-31); and `superchat-render.el` is the single
transcript authority with explicit `:current-run` / `:independent-turn` owners,
bounded moving/unbounded fallback regions, persistent notices and one reset shared
by Session rebuild and `/clear`.

### Recommended next action

None for this defect family — closed. The open design agenda (status band, waiting
feedback, workflow-View input, post-cancel context seam, workflow name grammar, F3
mixed-input semantics, private compaction builder nil-guard) is tracked in
`STATUS.md` and routes through `plan`.

### Verification

Canonical `test/run-tests.el`: 1039 tests, 0 unexpected (independently re-run by
commander and reviewer); `kernel-check -- check --strict KERNEL.md`: refs 854, 0/0;
every reviewer counterexample probe flipped (batch3c probes 3/3, batch3d 2/2,
batch3e 5/5 positive, batch3f ports 3/3); reviewer release verdicts in
`.scratch/arch-test-20260830/batch3-review.md` (final section "batch3f 确认").

### References

`.scratch/arch-test-20260830/fix-batch3{,b,c,d,e,f}-report.md`;
`audit-incomplete-investigation.md`; `batch3-review.md`; STATUS.md 2026-08-31
entries; JJ working copy (shared, uncommitted by convention).

## 2026-08-31 — KERNEL.md K5 → K6: fact-sync of six fix batches and a new rendered-transcript Action Contract (E-20260831-002)

### Observation

K6 accumulates every Kernel edit that landed with the 2026-08-31 fix batches, each
plan-confirmed before code: Session identity narrowed to the single buffer-backed
minting site (helpers only return an already-bound id); Turn parse gives `>skill`
precedence and skips `/command`; request assembly dedups the tool surface in the
authority order registry → active-plan tool → synthesis; Seal Run records the audit
warning's independent system-material identity, its best-effort persistence boundary,
its deliberate prompt visibility, and the default-chat-UI scope of independent
display; Compact keeps only Anchor/history/log effects and references the persistent
success notice as a rendered-transcript sub-effect; and a new Action Contract
`Render the Chat Buffer Transcript` (entry 77) owns the three transcript owner
classes, bounded/unbounded assistant regions, persistent notices and the unified
reset. Entries now 77; strict refs 854, 0 errors, 0 warnings.

### Interpretation

Per `docs/protocols/kernel-review.md`: **pairs report** — written against
`/tmp/sc-batch3f-pairs.md` (`entries 77, shared groups 351, universal sentences 173,
narrowed 98 in 31 entries`). **Where counterexamples were hunted** — the reviewer's
rounds in `batch3-review.md` record the searches for every new universal sentence:
replay-classification history (git archaeology of every Tape producer and DB
migration since `a8eb94e` plus six real SQLite sites, "batch3 §1"); dedup order
across all four collector branches plus MCP namespaces ("batch2/batch3 §2");
transcript owner completeness by full-tree grep of every
`superchat--response-region-*` and `superchat--insert-system-message` caller with a
per-caller adjudication table ("batch3e §1", `fix-batch3e-report.md`); the
bounded/unbounded region sentences read branch-by-branch against
`superchat--response-region-stream/close` ("batch3f 确认 §2"); reset coverage by
enumerating `/clear`, Session rebuild, Runtime terminal delivery and tool-round
close callers ("batch3f 确认 §2"). **Fresh-context review** — the default-to-refute
sol reviews (batch3 through batch3f sections of `batch3-review.md`), whose findings
were re-verified each round before acceptance; the final release is "batch3f 确认".

### Recommended next action

K7 candidates remain open in `STATUS.md`: narrowing Kernel #8 (Authority
effects-only vs compatibility paths) and the residual scope notes from the
2026-08-30 audit; they were not part of this revision.

### Verification

`emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md` →
`refs 854, errors 0, warnings 0`; canonical 1039 / 0 unexpected; authority map ok;
pairs report stored at `/tmp/sc-batch3f-pairs.md` (copy referenced above).

### References

`KERNEL.md` header K6 (this revision); `batch3-review.md` all sections;
`fix-batch1{,b}-report.md`, `fix-batch2{,b}-report.md`, `fix-batch3{,b,c,d,e,f}-report.md`.

## 2026-08-31 — fix-batch4 series: design-round semantics landed, with a real symlink-escape caught and closed (E-20260831-003)

### Observation

The design round's six approved semantic changes are live and released by
cross-model review (implementer luna, reviewer sol, four rounds, final verdict
"放行" in `.scratch/arch-test-20260830/batch4-review.md` §"batch4d 终审"):
cancellation now excludes the cancelled exchange from the default Context View
(Tape intact, explicit recall works) and invalidates any compaction Anchor whose
provenance includes cancelled material, whole-Anchor, with a "可 /compact 重新压缩"
hint (user rulings 2026-08-31); workflow names accept non-ASCII via a single
resolver that enumerates real `.workflow` basenames, compares in NFC, rejects
symlinks and out-of-tree truenames, rejects ambiguous NFC-equivalent candidate
sets, and hands the one checked canonical path through both `>>` and `/workflow`
to the loader (admission and consumption share one resolution); mixed
`>skill /command` input echoes a reminder only when the whole cleaned string's
first token parses as a command; `superchat-skills-invoke` receives clean-input;
`superchat-compact--request` rejects a missing Session id; and the layer gates
scan call-time requires with an exact reviewed five-edge exemption list split
per gate (Kernel 1, integration 4), with AGENTS.md and both gate docstrings
worded to match. Review rounds caught, before release: a macOS NFC/NFD name
mismatch invisible to pure-Han tests, a real symlink escape that executed an
out-of-tree file through the full `>>` product path, a "first token" reminder
misimplemented as "any line start", a missed second entry point (`/workflow`
still resolving twice), and an E-005 violation (a multi-candidate test stubbing
`directory-files`/`file-name-base` to fabricate the ambiguity it asserted).

### Interpretation

Same lesson as the batch3 series, now on the input/naming surface: green gates
plus Han-only fixtures cannot see normalization, symlink, or second-entry-point
defects; the cross-model reviewer's real-filesystem probes could. The honest-skip
pattern (`skip-unless` on volumes that cannot hold both spellings) is the
accepted resolution when a real fixture is physically unavailable.

### Recommended next action

`fix-batch5` (UI batch: status band enrichment, collapsible reasoning region,
in-place workflow-View input, welcome recent-session list) is the remaining
approved work from the design round. TOCTOU under hostile concurrent local
writers remains an explicitly uncontracted boundary.

### Verification

Canonical 1053 tests / 0 unexpected / 17 skipped (the 17th is the declared
normalization-volume skip); kernel strict refs 858, 0/0; reviewer probes 19/19
and 4/4, independently re-run by commander and reviewer; per-round red-before-green
recorded in `fix-batch4{,b,c,d}-report.md`.

### References

`.scratch/arch-test-20260830/fix-batch4{,b,c,d}-report.md`; `batch4-review.md`;
STATUS.md 2026-08-31 design-round entry; user rulings recorded therein.

## 2026-08-31 — KERNEL.md K6 → K7: cancellation View semantics, workflow resolver contract, and per-gate exemption wording (E-20260831-004)

### Observation

K7 records the batch4-series Kernel edits, each plan-confirmed before code: the
Context View entry now states the cancellation exclusion rule (cancelled Runs'
material is omitted and disclosed `cancelled`; an Anchor whose provenance includes
cancelled material is invalidated whole with the recompact hint; the Tape stays
complete and explicit recall recovers the rows); the workflow grammar/resolver
sentences state the non-whitespace name token, the `@` reserved-prefix boundary,
NFC comparison, symlink/truename containment, the 0/1/ambiguous candidate rule
and single-resolution hand-off; and the layer-contract sentences state the exact
per-gate exemption lists instead of absolute quantifiers. Entries stay 77; strict
refs 858, 0 errors, 0 warnings.

### Interpretation

Per `docs/protocols/kernel-review.md`: **pairs report** — `/tmp/sc-batch4d-pairs.md`
(`entries 77, shared groups 355, universal sentences 173, narrowed 98 in 31
entries`). **Where counterexamples were hunted** — recorded across
`batch4-review.md`: cancellation selection against real SQLite traces including
mixed/old-Anchor layouts (§1 of both rounds); resolver universals against real
macOS filesystem probes (NFC/NFD both directions, symlink chains, broken links,
directory impostors, cross-volume truenames, case-folding, `.`/`..`; §"batch4b/4c
复验"); the first-token reminder against multi-line/code-fence/URL negatives and
all three shared-parser callers (§3, §2); the exemption lists against a full-tree
call-time require scan with per-edge file/feature/line comparison (§4). **Fresh-
context review** — the default-to-refute sol rounds (batch4 through batch4d
sections), findings re-verified each round; release verdict "batch4d 终审（系列
收官）". Version stamped by `learn` only after that release (an earlier premature
K7 header by the implementer was reverted pending this chain).

### Recommended next action

K8 candidates: Kernel #8 (Authority effects-only vs compatibility paths) narrowing
still outstanding; the whitelisted Kernel→distribution back-edge remains recorded
architecture debt pending the seam-extraction batch.

### Verification

`check --strict KERNEL.md` → refs 858, 0/0 after the K7 header stamp; canonical
1053 / 0 unexpected; authority map ok; pairs stored at `/tmp/sc-batch4d-pairs.md`.

### References

`KERNEL.md` header K7 (this revision); `batch4-review.md` all sections;
`fix-batch4{,b,c,d}-report.md`; E-20260831-003.

## 2026-09-01 — fix-batch5 series: the design-round UI batch, hardened by live-host review (E-20260901-001)

### Observation

The four approved UI capabilities are live and released
(`.scratch/arch-test-20260830/batch5-review.md`, final verdict "batch5c 终审
（战役收官）"): an aggregated status band on the existing header-line/mode-line
(model, phase/attempt, compact human-readable elapsed via the single
`superchat--format-duration`, context %, background Run count, MCP summary,
control hint); a collapsible per-Run Thinking region fed by the llm transport's
linearized cumulative reasoning stream (prefix-diff forwarding, replacement on
non-prefix, per-round baseline resets, 20000-char tail guard); in-place input in
the workflow View during a run (region ownership held through success, cancel and
failure sequences); and a welcome recent-session list with real-widget RET reopen
and folded Runtime-audit blocks. Live-host review across three rounds caught,
before release: qwen's cumulative reasoning partials re-appended as deltas (one
short answer ballooned to 127,519 chars, the same opening repeated 259 times); a
widget `:notify` arity crash that made the welcome list unopenable; a per-second
status timer leaking permanently when a running View was killed; the bare-`%`
mode-line defect recurring in the new band (third occurrence of the class — now
funneled through one escaping outlet whose comment records the history); and raw
total-seconds elapsed display (`1788250489s`). Declared semantics recorded with
user rulings: ordinary Runs in a Session share the Context View with in-flight
workflow material (region separation is renderer ownership, not prompt
isolation).

### Interpretation

Every blocker in this series lived at a host boundary — real producer shapes,
real widget keymaps, real timer lifecycles, real `format-mode-line` — none
reachable by the canonical suite alone. The implementer model switch (luna →
sol, user instruction 2026-09-01) took effect at batch5c; its first delivery
separated evidence classes correctly and respected the version/Evidence
discipline the previous implementer twice violated (premature K7/K8 stamps and a
worker-authored Evidence entry, both reverted by the commander).

### Recommended next action

Residuals tracked in STATUS: status-band richness beyond current data sources
(cost/token/jobs), the large-skill timeout root cause and progress display, the
Kernel→distribution seam extraction, low-severity UX leftovers (in-place retry
actions, separators/spinner, Tape browser polish, `/remember` echo placement,
dead-endpoint address, obsolete-macro warnings), and the display-matrix test
face (GUI/terminal themes, narrow windows, multi-frame).

### Verification

Canonical 1067 tests / 0 unexpected / 17 skipped; kernel strict refs 895, 0/0;
duration formatter boundary values verified independently by commander and
reviewer; real-daemon `format-mode-line` evidence at
`/tmp/superchat-format-probe.out`.

### References

`.scratch/arch-test-20260830/fix-batch5{,b,c}-report.md`; `batch5-review.md`;
user rulings 2026-08-31 (design round) and 2026-09-01 (worker model switch).

## 2026-09-01 — KERNEL.md K7 → K8: status projection, Thinking region, welcome list, duration and shared-context declarations (E-20260901-002)

### Observation

K8 records the batch5-series Kernel edits, plan-confirmed in the design round:
the Render the Chat Buffer Transcript entry gains the Runtime status projection
consumption, the Run-owned collapsible Thinking region (cumulative-producer
linearization stated with the correct prefix direction — the old accumulated
value is the prefix of the new partial — and the no-retro-deletion boundary),
the welcome recent-session list with its real keymap owner anchored at
`superchat--insert-welcome-recent-sessions`, the single duration formatter and
single `%`-escaping outlet (batch ERT asserts escaped input; real formatter
output is daemon-probe evidence — stated as two evidence classes, not one), and
the declared same-Session shared Context View for concurrent workflow/ordinary
Runs. Entries stay 77; strict refs 895, 0/0.

### Interpretation

Per `docs/protocols/kernel-review.md`: **pairs report** —
`/tmp/sc-batch5c-kernel-pairs.md` (77 entries / 382 shared groups / 176
universal sentences / 101 narrowed in 31 entries; commander re-run
`/tmp/sc-k8-pairs.md` agrees). **Where counterexamples were hunted** — recorded
in `batch5-review.md` §6: full-tree enumeration of duration/`format-seconds`/
legacy seconds strings (single formatter remains); branch-by-branch reading of
the `string-prefix-p` diff with the protocol/tool/recovery reset entries; welcome
callback/widget owner enumeration; all dynamic header/lighter outlets re-swept
for `%` bypasses with batch and daemon evidence kept distinct. **Fresh-context
review** — the default-to-refute sol rounds (batch5 through batch5c sections),
each finding re-verified; release verdict "batch5c 终审（战役收官）". Version
stamped by `learn` after that release; two premature version stamps and one
worker-authored Evidence entry during the series were reverted before this
record.

### Recommended next action

K9 candidates: Kernel #8 (Authority effects-only vs compatibility paths)
narrowing — still outstanding; the documented Kernel→distribution back-edge
pending seam extraction.

### Verification

`check --strict KERNEL.md` → refs 895, 0/0 after the K8 stamp; canonical 1067 /
0 unexpected; authority map ok; pairs stored at `/tmp/sc-batch5c-kernel-pairs.md`
and `/tmp/sc-k8-pairs.md`.

### References

`KERNEL.md` header K8 (this revision); `batch5-review.md` §5-§7;
`fix-batch5{,b,c}-report.md`; E-20260901-001.

## 2026-09-01 — fix-batch6a series: the Kernel back-edge is gone, obsolete macros are zero, and a Custom-lifecycle regression was caught before release (E-20260901-003)

### Observation

The structural batch and its fix-up are released (`batch6a-review.md`, verdict
"放行 batch6a2；允许进入 K9 learn"): the `superchat-llm.el → superchat` call-time
back-edge is removed — the Kernel now owns its transport configuration as single
`defcustom` declarations (default value, Custom type, group and setter in one
place, headless-safe), and the distribution injects six optional UI/timing
adapters through the validated `superchat-llm-configure` port (unknown, odd and
duplicate keys are readable errors). The Kernel require gate's exemption list is
empty again — "a Kernel feature requires only Kernel features" is once more an
exception-free true statement, with a headless request-path regression proving
`(featurep 'superchat)` stays nil through a real send. All shipping files
byte-compile with zero obsolete-macro warnings. Review caught before release:
the interim `defvar DEFAULT + later defcustom` split silently discarded
saved-but-unbound Custom values for 18 variables (LOST=18/18, flipped to 0/18
with a three-state child-Emacs regression: default / saved-then-lazy-load /
prebound), and the K9 sweep had missed literal overturned universals in product
docstrings, the MCP module commentary and the confirmed SPEC (all now reworded
or annotated; the one remaining "name-blind" hit is a compliant negation).

### Interpretation

Two reusable lessons: a Kernel that takes over configuration must take over the
whole Custom lifecycle, not just the default value — `custom-set-variables`
before lazy load is a real user pattern; and narrowing a Kernel universal is
incomplete until the same sentence is hunted verbatim across docstrings,
module commentaries and SPEC prose, not only KERNEL.md.

### Recommended next action

`fix-batch7` (correctness: skill file-context preservation, provider context
preflight, head-of-input syntax parsing, size-adaptive timeout with hard cap,
prefill/reasoning/answer phase display, non-partial-provider latency
visibility) dispatches next; `fix-batch6b` (polish) after it.

### Verification

Canonical 1070 / 0 unexpected / 17 skipped; strict refs 909, 0/0; Custom probe
LOST=0/18 re-run by reviewer; 34 migrated-declaration Kernel anchors verified
semantically by fresh-context review.

### References

`fix-batch6a-report.md`, `fix-batch6a2-report.md`, `batch6a-review.md`;
skill-timeout-diagnosis.md (dispatch basis for batch7).

## 2026-09-01 — KERNEL.md K8 → K9: Authority #8 narrowed to the real seam; transport configuration ownership recorded (E-20260901-004)

### Observation

K9 lands the plan-confirmed narrowing of the Authority entry family: given a
resolved Effect snapshot, the base fold decides on Effect kind plus explicit
Attendance — the tool name selects the resolver when no snapshot is supplied
and still keys the effective executor's compatibility paths (legacy hooks,
session allowlist, destructive-name confirmation), so "effects-only" is scoped
to the base decision and registration contract, not the whole seam; the paired
`Approval and tool-execution lifecycle` entry drops the single-wrapper
universal for the two-topology-wrapper reality. It also records the new
transport-configuration ownership: Kernel-owned `defcustom` declarations with
distribution adapter injection, and the exception-free Kernel require gate.
Entries stay 77; strict refs 909, 0/0.

### Interpretation

Per `docs/protocols/kernel-review.md`: **pairs report** — `/tmp/sc-k9-pairs.md`
(77 entries / 388 shared groups / 169 universal sentences / 101 narrowed in 31
entries; universal count dropped from 176 as overturned universals were
narrowed). **Where counterexamples were hunted** — recorded in
`fix-batch6a-report.md` §3 and `fix-batch6a2-report.md` §2 (branch-by-branch
reading of `superchat-authority-decide` and the effective-decision cond; full
attendance-floor / allowlist / destructive-confirm path enumeration) plus the
reviewer's verbatim sweeps for "never learns tool names" / name-blind /
"every tool" across `.el` docstrings, MCP commentary and `.specs/` (final
residue: one compliant negation at `superchat-tools.el:1750`). **Fresh-context
review** — `batch6a-review.md` both rounds, default-to-refute, findings
re-verified; release verdict "放行 batch6a2；允许进入 K9 learn". Version
stamped by `learn` after that release.

### Recommended next action

K10 candidates: none queued; future Kernel touches ride with batch7/6b edits.

### Verification

`check --strict KERNEL.md` → refs 909, 0/0 after the K9 stamp; canonical 1070 /
0 unexpected; pairs at `/tmp/sc-k9-pairs.md`.

### References

`KERNEL.md` header K9 (this revision); `batch6a-review.md`;
`fix-batch6a{,2}-report.md`; E-20260901-003.

## 2026-09-03 — Runtime HostLease and Cleanup lifecycle replace ambient kill attribution (E-20260903-001)

### Observation

The Runtime now acquires an ephemeral execution host as a fresh buffer object,
records that object's opaque host id and owner Run id in a process-local
HostLease, and releases it through one private adapter.  A same-name existing
buffer is not adopted.  Workflow no longer creates a protocol buffer or owns
its rejection cleanup; for a dead View it supplies only a requested name, the
ephemeral flag, and an optional last-known Authority mode.  Request validation
now treats `:ephemeral-buffer` as a boolean and accepts
`:host-authority-mode` only with explicit `:ephemeral-buffer t`.

Cleanup is represented by a process-local record instead of ambient dynamic
kill provenance.  Automatic cleanup starts only from `pending`; explicit retry
starts only from `retained`; both publish an opaque attempt id before callbacks.
Host-bearing attempts verify Run owner and host identity before the physical
effect and verify the same current attempt again before committing its result.
Hostless ordinary Runs converge to `released`.  Owner mismatch becomes
`unresolved` without invoking the releaser.  A retained debt can be retried or
abandoned; abandon drops Runtime's resource reference without claiming the
physical host was released.

### Interpretation

This is a stable project concept and lifecycle, not a test-only repair.  It
removes the structural contradiction in which Runtime, Workflow and test
instrumentation all tried to infer ownership from buffer names, backtraces,
dynamic provenance and replayed query watchers.  The remaining reentrancy
boundary is stated narrowly: an attempt already stale before the adapter call
has no host effect; invalidation from a synchronous hook inside the adapter
cannot undo an effect already in progress, but prevents the old attempt from
committing a result.

Per `docs/protocols/kernel-review.md`: **pairs report** —
`/tmp/superchat-kernel-pairs.md` (77 entries, 449 shared groups, 188 universal
sentences, 122 narrowed in 33 entries) was read before the K10 edits.  **Where
counterexamples were hunted** — full-tree `:ephemeral-buffer` and
`:host-authority-mode` producer scan; full-tree Cleanup accessor, release,
retry and abandon caller scan; Runtime/Workflow/Subagent buffer creation and
kill scan; branch-by-branch reading of validation, acquisition,
current-attempt, release, retry, abandon and unwind paths.  The search found
and closed two constructive counterexamples: adoption of a same-name caller
buffer, and hostless ordinary cleanup stuck in `releasing`.  It also narrowed
the stale-attempt claim around effects initiated before synchronous
invalidation.  **Fresh-context review** — independent default-to-refute agent
`kernel_host_review`, three rounds.  It first rejected the proposed contract
for same-name adoption, decorative owner ids and overbroad stale-effect claims;
then found the hostless regression; its final verdict after the fixes was
“放行：hostless 修复已关闭最后 blocker，未发现新的阻断.”

### Recommended next action

Keep HostLease and Cleanup process-local: they are execution-resource
coordination, not durable Tape facts.  Add a new durable protocol only if a
future product requirement needs cleanup debt to survive an Emacs restart.

### Verification

Canonical suite: 1143 tests, 1126 expected, 0 unexpected, 17 skipped;
`RUNNER_GATE_OK`.  Focused owner/acquisition/hostless/stale tests: 7/7 after
the final correction.  `check-parens` passed for all five edited Lisp/test
files.  Scratch byte compilation of Runtime and Workflow succeeded; its only
warnings are the two pre-existing unused lexical `err` bindings in evidence
recorders.  K10 `check --strict` reports 984 refs, 0 errors and 0 warnings;
`check-kernel.sh` reports `ok`; the post-promotion pairs report at
`/tmp/superchat-k10-pairs.md` contains 79 entries, 460 shared groups and no new
universal sentence.

### References

`superchat-runtime.el` HostLease/Cleanup structs, request validation,
acquisition and cleanup functions; `superchat-workflow.el` dead-View request
path; `test/test-runtime.el` private-host acquisition and hostless terminal
tests; `test/test-llm-backend.el` release/retry/reentrancy/owner/stale/abandon
tests; independent review `kernel_host_review`; canonical runner output from
2026-09-03.

## 2026-09-03 — Atomic Runtime host acquisition and Run publication (E-20260903-002)

### Observation

This evidence strengthens and partly corrects `E-20260903-001`.  The first
HostLease/Cleanup implementation passed its then-current suite, but a later
default-to-refute architecture review found five unclosed failure windows:
equal lease labels could substitute a different lease or buffer object; an
exception during ephemeral host initialization could leak the fresh buffer;
an exception after acquisition but before registry publication could do the
same; a registry helper could insert the Run and then exit nonlocally; and
rollback could kill a buffer whose exact object had meanwhile transferred to
a different lease.

Runtime now treats ephemeral acquisition and Run publication as one local
transaction.  A publisher must return normally for publication to succeed.
On nonlocal exit, Runtime removes only the exact partially published Run entry
and releases only the buffer still owned by the original lease.  If a different
lease has taken the same exact buffer object, rollback relinquishes it instead
of killing the new owner's resource.  Ordinary Runs use the same atomic entry
publication helper, but rollback never releases their caller-owned View.

HostLease slots are read-only.  Cleanup authority is proved by the current
registry entry, cleanup state and attempt id, the exact HostLease object, the
exact buffer object, and the lease's owner Run id.  Equal textual host ids are
diagnostic labels, not transferable authority.

### Interpretation

The stable semantic boundary is larger than cleanup alone: acceptance of an
ephemeral Request is the acquisition-and-publication transaction that creates
the private host, initializes it, creates its immutable lease, constructs the
Run entry and publishes that exact entry.  No failed submit path may leave an
ownerless private host or a partially accepted Run.  Resource transfer is
narrowly recognized only by a different current lease holding the same buffer
object; responsibility after that transfer belongs to the new lease.

This contradicts the sufficiency of the “owner Run id and opaque host id”
wording in `E-20260903-001`; it does not contradict the earlier conclusions
that leases remain process-local, buffer names are hints, Workflow is not the
host owner, and cleanup effects are guarded before and after the releaser.

Per `docs/protocols/kernel-review.md`, the pre-edit pairs report at
`/tmp/superchat-kernel-pairs-prelearn.md` contained 79 entries, 460 shared
groups, 188 universal sentences and 122 narrowed sentences in 33 entries, and
the paired HostLease, Cleanup, Run, Request and authority-map entries were read.
Counterexamples were hunted at every nonlocal-exit boundary from buffer
creation through registry insertion, with both same-label/different-object and
same-object/different-owner substitutions.  The final independent adversarial
review reported no remaining High finding and returned `ACCEPT WITH RISKS`.

### Recommended next action

Promote the atomic acquisition/publication boundary and exact-object lease
authority into the Runtime entries of `KERNEL.md`.  Keep the remaining Medium
risks explicit rather than silently widening this contract: Cleanup state is
not yet a public discovery interface, `unresolved` has no recovery transition,
and cleanup failure occurring after terminal notification has no separate
notification contract.

### Verification

Canonical suite on the fixed working copy: 1151 tests, 1134 expected,
0 unexpected, 17 skipped, with `RUNNER_GATE_OK`.  The added regression set
covers initialization failure, pre-publication failure, partial registry
insertion, ordinary-View preservation, exact-buffer lease transfer, exact
lease substitution before and during release, and hostless convergence.
`check-parens` passed.  Scratch byte compilation passed with only the two
pre-existing unused lexical `err` warnings in Runtime evidence recorders.
The authority-map checker reports `ok`.  Before re-anchoring, strict Kernel
checking correctly reports 984 references, 122 errors and 0 warnings due to
line drift in Runtime and the two edited test files.  After K11 promotion and
construct-level re-anchoring, `convert` reports 995 references, 0 inserted,
0 errors and 0 warnings; strict checking reports 995 references, 0 errors and
0 warnings.  The final pairs report at
`/tmp/superchat-kernel-k11-pairs-final.md` contains 79 entries, 465 shared
groups, 191 universal sentences and 129 narrowed sentences in 35 entries.

### References

JJ working-copy Change ID `pvxzplyq` at commit `a96f232d`, compared locally to
parent `wuuqouku` / commit `dd160620` with read-only `jj status`; no JJ or Git
write operation was performed.  `superchat-runtime.el` immutable HostLease,
acquisition transaction, atomic publication helper and exact cleanup predicate;
`test/test-runtime.el` acquisition/publication rollback and transfer tests;
`test/test-llm-backend.el` exact-lease substitution tests; independent reviews
`adversarial_arch_review` and `kernel_host_review`; canonical runner output from
2026-09-03.

## 2026-09-03 — Session EditorWorkspace authority and native close contract (E-20260903-003)

id: E-20260903-003
status: confirmed
scope: `.specs/session-editor-workspace/` slices 01–02; `superchat-editor-workspace.el`; Session View/tab integration in `superchat.el`; focused tests
source: `.specs/session-editor-workspace/issues/01-editor-workspace-authority.md`; `.specs/session-editor-workspace/issues/02-native-close-ux.md`
verification: EditorWorkspace 18/18, native UI 51/51, combined 69/69; canonical ERT 1184 total, 1167 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; check-parens, isolated byte compilation and focused diff check passed; independent adversarial review found no remaining High or Medium issue

### Observation

Slices 01–02 establish a process-local Session EditorWorkspace keyed by the
stable Session ID.  Its registry stores only lifecycle state.  Each ownership
query scans live buffers for the buffer-local Session ID and owned marker, so
unmarked and borrowed buffers stay outside the close set and no second buffer
inventory exists.  Allocation is lazy, reuses a live `(Session, role)` buffer,
and freezes while state is `closing` or `retained`.

The manual command, programmatic seam, and native tagged-tab close route to one
close action.  Active Runs come from one public Runtime status snapshot filtered
by Session ID.  Rejection has zero Run, state, buffer, and tab effects and
reports the manual retry command.  Acceptance publishes `closing` before Run
cancellation, then uses ordinary buffer killing.  Cancellation error or quit
keeps `closing` and performs no buffer or tab effect; a kill survivor yields
`retained`, remains visible, and can be retried from a fresh marker scan.

The user explicitly resolved the native-hook atomicity tradeoff: prevent hooks
ordered before Superchat can veto first; after the event reaches Superchat for
a tagged Session tab, EditorWorkspace consumes the transaction and performs
native deletion itself.  Later prevent hooks do not run.  Native pre-close
hooks still run, and a sole native tab is neutralized by removing its Session
parameter rather than deleting the tab object.

### Interpretation

The stable authority belongs at the Session live-editor boundary, not in the
Run state machine and not in tab/project/ibuffer adapters.  This is an enacted
authority for the marker-owned paths delivered by slices 01–02.  It coexists
with Runtime's K10/K11 HostLease/Cleanup compatibility because the remaining
producer migration and retirement slices are not delivered.  Project and
ibuffer behavior is likewise not part of this evidence.

The exclusive native hook policy is an explicit tradeoff: it sacrifices later
prevent-hook participation for a tagged Session tab after Superchat begins
irreversible effects, while preserving earlier veto and ordinary pre-close
behavior.  It therefore belongs in an ADR and a focused action protocol rather
than remaining implicit in implementation order.

### Recommended next action

Implement Slice 03 as read-only transient-project and ibuffer projections over
the existing marker authority.  Do not store project/ibuffer membership, call
project persistence, or treat files beneath a root as owned.  Later slices must
migrate Runtime producers before K10/K11 HostLease/Cleanup can be retired.

### Verification

`emacs -Q --batch -L . -l test/test-editor-workspace.el -f ert-run-tests-batch-and-exit`
reported 18/18.  Native UI focused ERT reported 51/51; the combined selection
reported 69/69.  `emacs -Q --batch -L . -l test/run-tests.el` reported 1184
tests, 1167 expected, 0 unexpected, 17 skipped, with `RUNNER_GATE_OK`.

Before K12, `pairs` reported 79 entries, 465 shared groups, 191 universal
sentences, and 129 narrowed in 35 entries.  Counterexamples were hunted by
enumerating every writer of the three buffer-local ownership variables, every
caller of `superchat-session-workspace-close`, every occurrence of the tab
Session parameter and prevent-close hook, and all Runtime HostLease/Cleanup
callers.  This confirmed coexistence rather than retirement.  A fresh-context,
default-to-refute review by `kernel_host_review` required the same narrowing
and found no basis to claim project/ibuffer or producer migration.

After promotion, the strict Kernel check reported 1011 references, 0 errors,
and 0 warnings; the authority-map checker reported `ok`.  The final pairs
report contains 81 entries, 479 shared groups, 193 universal sentences, and
130 narrowed in 36 entries.  The same learn pass mechanically re-anchored 60
pre-existing `superchat.el` and `test/test-native-ui.el` citations displaced
by Slice 02; their constructs and prose were unchanged.

### References

- `superchat-editor-workspace.el` state, marker scan, allocation, close, public command, and native hook
- `superchat.el` Session View allocation and tab selection
- `test/test-editor-workspace.el`; relevant Session-tab cases in `test/test-native-ui.el`
- `docs/adr/0004-session-editor-workspace-authority.md`
- `docs/protocols/session-editor-workspace-close.md`
- `/tmp/superchat-k12-pairs-before.md`

## 2026-09-04 — Transient project and ibuffer remain read projections (E-20260904-001)

id: E-20260904-001
status: confirmed
scope: `.specs/session-editor-workspace/issues/03-project-ibuffer-projections.md`; `superchat-editor-workspace.el`; `test/test-editor-workspace.el`
source: Slice 03 verification and independent adversarial review
verification: focused EditorWorkspace ERT 22/22; canonical ERT 1188 total, 1171 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; check-parens and focused diff check passed

### Observation

Slice 03 exposes an explicit existing root as the built-in project.el value
`(transient . ROOT/)`.  Nil root returns nil; empty or nonexistent roots are
rejected.  Construction and buffer projection call neither interactive
project discovery nor `project-remember-project`, do not create a directory,
and leave a sentinel `project-list-file` unchanged.

The explicit `project-buffers` projection can show one shared file buffer to
two Sessions, along with each Session's owned buffer under the root.  Each
EditorWorkspace close set still contains only its own exactly marked buffer,
and closing both Sessions leaves the shared file buffer live.  The ibuffer
adapter returns a standard predicate filter group evaluated by Emacs's real
filter machinery.  It distinguishes the Session markers, excludes a borrowed
buffer carrying only a Session hint, and continues to show an owned buffer in
`retained` until retry closes it.

### Interpretation

Project and ibuffer are views over existing editor facts, not ownership
authorities.  No new resource state, membership ledger, root registry, or
project persistence was needed.  The explicit root remains an input from the
durable Workspace authority; Slice 03 does not infer or store it.

This extends the enacted Session EditorWorkspace concept but adds no lifecycle
or irreversible decision, so it is promoted into the existing KERNEL entry and
does not require another ADR, close protocol, runbook, or lesson.

### Recommended next action

Execute Slice 04 in a different agent context: extract private idempotent Run
quiescence from editor release while retaining the legacy HostLease release
behavior until later migration slices.

### Verification

Focused ERT reported 22/22.  The canonical runner reported 1188 tests, 1171
expected, 0 unexpected, 17 skipped, and `RUNNER_GATE_OK`.  The independent
default-to-refute test review found no High or Medium issue; its checks used
the real `project-root`, `project-buffers`, and ibuffer filter evaluator.

Before K13, `pairs` reported 81 entries, 479 shared groups, 193 universal
sentences, and 130 narrowed in 36 entries.  Counterexample checks covered all
writes of the EditorWorkspace ownership markers and all three new adapter
callers; none of the adapters writes those markers or feeds close planning.
The same learn pass mechanically re-anchored the 15 K12 citations displaced by
Slice 03 without changing their semantics.

After promotion, strict Kernel checking reported 1016 references, 0 errors and
0 warnings; the authority-map checker reported `ok`.  The final pairs report
contains 81 entries, 484 shared groups, 194 universal sentences, and 131
narrowed in 37 entries.

### References

- `superchat-editor-workspace.el` project and ibuffer adapters
- `test/test-editor-workspace.el` Slice 03 regression cases
- `.specs/session-editor-workspace/issues/03-project-ibuffer-projections.md`
- `/tmp/superchat-k13-pairs-before.md`

## 2026-09-04 — Run quiescence no longer depends on editor Cleanup (E-20260904-002)

id: E-20260904-002
status: confirmed
scope: `.specs/session-editor-workspace/issues/04-run-quiescence.md`; `superchat-runtime.el`; focused Runtime and LLM lifecycle tests
source: Slice 04 verification and independent adversarial review
verification: Runtime ERT 99/99; LLM backend ERT 192 total, 187 expected, 0 unexpected, 5 skipped; canonical ERT 1193 total, 1176 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; check-parens and scratch Runtime byte compile passed

### Observation

All five live terminal paths — completed, failed, cancelled, timed-out, and
audit-incomplete — now enter one private Runtime quiescence seam.  Before any
legacy editor Cleanup ownership/state decision, it publishes a one-shot guard
and removes the deadline handle, exact transport timer owner, provider request,
delivery/stream closures, and pause reachability from the current Run entry.
Foreign, unresolved, retained, substituted, and releaser-error Cleanup cases
therefore cannot skip this work.  A later editor-release retry performs only
the optional host effect.

The first adversarial review found one real counterexample after the original
suite was green: an exception from deadline `cancel-timer` stopped the combined
timeout helper before it attempted exact transport-owner cancellation, while
the one-shot guard had already made retry impossible.  A focused failure probe
reproduced `transport-cancels=0`.  The repair isolates deadline cancellation
from transport-owner cancellation; the regression test also injects a
transport cancellation exception and proves that both timer effects are
attempted once, registry reachability is severed before either effect, and
exact-Run pause clearing still runs once.  The independent reviewer reran both
directions and closed the Medium/required finding.

### Interpretation

Execution quiescence is part of the existing Run lifecycle owned by Runtime;
editor-buffer release is a separate compatibility Cleanup effect.  This is a
stable lifecycle/authority clarification already confirmed by the Session
EditorWorkspace SPEC, so it is promoted into the existing Run and Cleanup
entries at K14.  It adds no public status, recovery transition, editor owner,
ADR, protocol, runbook, or lesson.

The transport timer owner's internal wait-then-hard cancellation order remains
a lower-level existing adapter risk if the first raw `cancel-timer` call itself
signals.  That is not evidence that the Runtime quiescence seam is incomplete:
Runtime invokes and isolates the exact owner adapter once.  Any hardening of
the owner's internal algorithm should be handled as a separate scoped change.

### Recommended next action

Execute Slice 05 in a fresh Herdr worker: accept an exact live borrowed buffer
at the Runtime boundary while preserving the mutually exclusive legacy
ephemeral path and atomic publication/rollback rules.

### Verification

The post-repair focused suites reported 99/99 Runtime tests and 187 expected
LLM backend tests with 5 environment skips.  The canonical runner reported
1176 expected, 0 unexpected, 17 skipped, and `RUNNER_GATE_OK`.  The independent
reviewer ran two failure-injection probes, found no remaining High or Medium
issue, and judged Slice 04 ready for learn.

Before K14, `pairs` reported 81 entries, 484 shared groups, 194 universal
sentences, and 131 narrowed in 37 entries.  The same learn pass mechanically
re-anchored citations displaced by Slice 04 without changing their semantics.
After promotion, strict Kernel checking reported 1026 references, 0 errors and
0 warnings; the authority-map checker reported `ok`.  The final pairs report
contains 81 entries, 489 shared groups, 197 universal sentences, and 134
narrowed in 37 entries.

### References

- `superchat-runtime.el:1404@superchat-runtime--cancel-timeout`
- `superchat-runtime.el:2177@superchat-runtime--quiesce`
- `superchat-runtime.el:2209@superchat-runtime--release`
- `test/test-runtime.el:423@runtime/every-live-terminal-path-quiesces-exactly-once`
- `test/test-llm-backend.el:7908@test-runtime-quiescence-precedes-foreign-host-rejection-once`
- `test/test-llm-backend.el:7965@test-runtime-quiescence-isolates-timer-errors-before-pause-clear`
- `test/test-llm-backend.el:8040@test-runtime-editor-release-retry-does-not-repeat-quiescence`
- `.specs/session-editor-workspace/issues/04-run-quiescence.md`
- `/tmp/superchat-k14-pairs-before.md`

## 2026-09-04 — Runtime admits exact borrowed buffers without acquiring ownership (E-20260904-003)

id: E-20260904-003
status: confirmed
scope: `.specs/session-editor-workspace/issues/05-runtime-exact-buffer.md`; `superchat-runtime.el`; `superchat-editor-workspace.el`; focused Runtime, EditorWorkspace, and LLM backend tests
source: Slice 05 implementation verification, independent three-axis review, and the post-check `plan` classification
verification: Runtime ERT 107/107; EditorWorkspace ERT 27/27; LLM backend ERT 192 total, 187 expected, 0 unexpected, 5 skipped; canonical ERT 1206 total, 1189 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; independent review found no functional required or blocker

### Observation

Runtime Request validation now admits `:buffer` as an eighteenth validated
top-level key.  The object must be an exact live buffer, and its presence is
mutually exclusive with `:ephemeral-buffer t`; both failures occur before host
acquisition, Run publication, Tape writes, or provider effects.  Submission
stores that same object on the Run entry, derives the actual name only for
diagnostics, creates no HostLease, and makes no buffer-release call on success,
cancellation, provider error, or publication rollback.  Killing the object and
creating a same-named replacement does not retarget the Run.  The existing
legacy ephemeral path still creates a fresh HostLease-owned buffer, while
ordinary name-based callers retain their previous compatibility behavior.

The middle-tier `superchat-editor-workspace-validate-buffer` is the only new
owner-marker validation point.  It returns the same live object, accepts an
ordinary unowned borrowed buffer, and rejects malformed owned metadata or an
owned Session mismatch.  Structural tests prove that Kernel Runtime neither
requires/names the EditorWorkspace module nor reads its marker symbols.  No
global hook, validation registry, attestation token, Request callback, or
second ownership ledger was introduced.

The independent check found no functional required or blocker.  It did find
one semantic documentation error: Slice 05's Planning resolution called the
work “not a SPEC/KERNEL change”, although `:buffer`, exact Request/Run object
identity, the live-object invariant, and mutual exclusion with the legacy
ephemeral shape are observable additions to the Submit Request Action
Contract.  K14 still claimed that validation covered exactly seventeen keys.

### Interpretation

The post-check `plan` decision classifies the implementation as the compatible
Runtime Request Action Contract delta already approved by SPEC AC7, not as an
unplanned ownership-authority transfer.  K15 therefore revises the existing
Submit Request contract: `since: K3` remains unchanged, while the K14 input and
invariant wording is explicitly superseded.  Runtime owns only generic shape,
liveness, exact borrowed identity, and publication semantics; it does not
understand EditorWorkspace markers.  EditorWorkspace remains the sole
ownership authority, and migration of shipped producers remains in Slices
06–08.

This promotion changes no Protocol, ADR, Context vocabulary, recovery
transition, public Cleanup discovery contract, or producer-migration claim.

### Recommended next action

Execute Slice 06 in a fresh dedicated worker: migrate the interactive chat
producer through EditorWorkspace validation into the exact-buffer Runtime
Request path, without claiming the remaining workflow/subagent migrations.

### Verification

The implementation worker reported 107/107 focused Runtime tests, 27/27
focused EditorWorkspace tests, 187 expected LLM backend tests with 5 skips,
and a canonical run of 1189 expected, 0 unexpected, 17 skipped with
`RUNNER_GATE_OK`.  The independent checker reproduced all four totals and
classified only the semantic documentation mismatch above; the post-check
plan then approved the Action Contract classification used by K15.

The learn pass independently reran all four suites with the same totals:
Runtime 107/107, EditorWorkspace 27/27, LLM backend 192 total with 187
expected and 5 skipped, and canonical 1206 total with 1189 expected, 0
unexpected, 17 skipped and `RUNNER_GATE_OK`.  It mechanically re-anchored the
citations displaced by Slice 05 without changing their semantics.  K15 strict
checking reports 1031 references, 0 errors and 0 warnings; the authority-map
checker reports `ok`.  The final pairs report contains 81 entries, 494 shared
groups, 198 universal sentences, and 136 narrowed in 37 entries.

### References

- `superchat-runtime.el:1465@superchat-runtime--validate-request`
- `superchat-runtime.el:1611@superchat-runtime-submit`
- `superchat-editor-workspace.el:82@superchat-editor-workspace-validate-buffer`
- `test/test-runtime.el:2557@runtime/exact-buffer-must-be-live-before-any-effect`
- `test/test-runtime.el:2572@runtime/exact-buffer-and-ephemeral-are-mutually-exclusive`
- `test/test-runtime.el:2593@runtime/exact-buffer-is-borrowed-and-name-is-diagnostic`
- `test/test-runtime.el:2628@runtime/exact-buffer-publication-rollback-preserves-caller-object`
- `test/test-editor-workspace.el:264@editor-workspace/validate-buffer-returns-exact-owned-object`
- `.specs/session-editor-workspace/issues/05-runtime-exact-buffer.md`

## 2026-09-04 — Chat, Workflow, and Compact use exact Session-owned hosts (E-20260904-004)

id: E-20260904-004
status: confirmed
scope: `.specs/session-editor-workspace/issues/06-session-workflow-compact.md`; Chat and Extension composition; Workflow and Compact producers; synchronous Runtime terminal handling; focused EditorWorkspace, Dispatcher, Extension, Workflow, Compact, Runtime, and LLM-backend tests
source: Slice 06 Revision 4 implementation and verification summaries; `slice06_final_check` PASS
verification: canonical ERT 1227 total, 1210 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; final focused Runtime 109/109 and Compact 39/39; marker, producer-shape, paren, scratch-byte-compile, and diff checks passed

### Observation

Chat now captures its producer-selected Session ID and exact View before
Extension composition.  The same Dispatcher submission seam serves ordinary
dispatch and inspectable plans.  It validates that pair before composition,
passes an acceptance guard into Extension, rejects an in-place mutation,
copied replacement, simultaneous retarget, or removal of either identity
before `extension.activated` can be appended, validates the unchanged final
pair again, and only then submits to Runtime.  A rejected composition has zero
Extension Tape, Runtime, or provider effect; an accepted composition records
one activation and submits once.

EditorWorkspace now classifies ownership through one private function.  Both
the owned marker and Session marker must be exact buffer-local, valid metadata.
Inherited defaults and dynamic bindings are unowned; one-sided or malformed
local metadata is rejected and cannot enter owner lookup, enumeration, role
reuse, ibuffer grouping, validation, or the close set.  Other modules use the
public owner/validation interfaces and do not reinterpret the marker shape.

Workflow captures its stable Session ID at its public boundary.  A live exact
View is validated and borrowed; after that View dies, later steps reuse the
same Session's `workflow-protocol` owned host rather than correlation identity
or a per-Run ephemeral buffer.  Compact derives a distinct child Session,
allocates and validates that exact child's `compaction-protocol` host, and
treats its live scope as operation-scoped.  Terminal delivery closes the child
through the public EditorWorkspace authority exactly once, including when the
consumer callback exits by `error` or `quit`; synchronous submission failure
does the same.  A refused close remains `retained` and a later public close
re-enumerates and retries it.  Runtime terminal completion itself still does
not close any EditorWorkspace.

Runtime's synchronous submission handler now treats `error` and `quit` alike
after Run publication: it seals the Run through the existing terminal path,
delivers once, quiesces execution handles, completes legacy Cleanup, and only
then re-signals the original condition.  Compact's outer acceptance guard
clears the parent View's in-flight flag on every failed submission exit.
Turn-hook and completion-hook loops isolate both `error` and `quit` around each
individual untrusted hook, so one hook cannot replace the provider condition,
skip later hooks, or interrupt terminal publication, delivery, quiescence, or
cleanup.

### Interpretation

These are enacted refinements of the already confirmed Session
EditorWorkspace and Runtime contracts, not new lifecycle owners.  The stable
boundary is exact producer identity plus exact Session-owned host selection:
Extension contributes content but never routes Chat; Workflow and Compact
select hosts but never own Run termination; Compact explicitly ends only the
operation-scoped child editor scope it created.  Runtime remains the sole
owner of atomic Run publication, terminal delivery, and execution quiescence,
while EditorWorkspace remains the sole interpreter of ownership markers and
the sole close authority.

The synchronous correction narrows its claim to Runs already published before
the provider's nonlocal exit.  It does not promise that arbitrary pre-
publication failure creates a Run, nor does it turn untrusted hooks into
transaction owners.  K10/K11 legacy HostLease/Cleanup remains enacted for
unmigrated callers and is not retired by this Slice.

### Recommended next action

Run `arrange` for Slice 07, the next issue in the current migration order, then
migrate Subagent, Skills, Org Babel, Magit, and Rewrite to explicit exact
Session hosts.  Do not retire HostLease/Cleanup until the remaining producer
and compatibility slices satisfy their own gates.

### Verification

The final Slice verification reported Runtime 109/109 and Compact 39/39.  The
canonical command `emacs -Q --batch -L . -l test/run-tests.el` reported 1227
total, 1210 expected, 0 unexpected, 17 skipped, and `RUNNER_GATE_OK`.
`slice06_final_check` returned PASS after the terminal-hook correction.  The
focused regression proves the original provider `quit` survives a quitting
Turn hook and completion hook, later hooks still run, delivery occurs once,
the Turn is completed, the Run is `failed` and quiesced, Cleanup is released,
and no registry entry remains `running-model` or `delivering`.  The real
Runtime/Compact regression additionally proves the parent in-flight flag is
clear and the operation child is closed.

Before learn, strict Kernel checking reported 1031 references, 268 errors and
0 warnings from concurrent line movement; the authority-map checker remained
`ok`.  This learn pass re-anchors those citations and promotes the confirmed
semantics as K16.  Final strict checking reports 1035 references, 0 errors and
0 warnings; the authority-map checker reports `ok`.  The final pairs report
contains 81 entries, 514 shared groups, 201 universal sentences, and 137
narrowed in 36 entries.  The learn pass independently reran Runtime 109/109
and Compact 39/39 with 0 unexpected, and reused the unchanged final canonical
1227/1210/0/17 result and `RUNNER_GATE_OK` because no production or test file
was modified.

### References

- `superchat-editor-workspace.el:69@superchat-editor-workspace--classify-buffer`; `:123@superchat-editor-workspace-owner-session`; `:138@superchat-editor-workspace-validate-buffer`
- `superchat-dispatcher.el:430@superchat--submit-chat-request`; `superchat-extension.el:378@superchat-extension-compose-request`
- `superchat-workflow.el:554@superchat-workflow--run-step`
- `superchat-compact.el:592@superchat-compact--request`; `:652@superchat-compact--submit-request`; `:717@superchat-compact--finish`; `:758@superchat-compact-session`
- `superchat-runtime.el:983@superchat-runtime--run-completion-hooks`; `:1141@superchat-runtime--run-turn-hooks`; `:1288@superchat-runtime--finish`; `:1619@superchat-runtime-submit`
- `test/test-cmd-dispatch.el:295@chat/extension-retarget-is-rejected-before-activation-publication`; `:355@chat/legal-extension-composition-publishes-and-submits-once`
- `test/test-workflow.el:630@workflow/dead-view-keeps-session-and-reuses-owned-role-host`
- `test/test-compact.el:1327@compact/terminal-delivery-closes-operation-child-exactly-once`; `:1356@compact/synchronous-submit-quit-clears-parent-and-closes-child`; `:1389@compact/real-runtime-provider-quit-seals-both-scopes`; `:1444@compact/on-finish-error-and-quit-still-close-child`; `:1465@compact/retained-child-remains-discoverable-and-retryable`
- `test/test-runtime.el:2720@runtime/synchronous-provider-quit-seals-published-run`; `:2752@runtime/provider-quit-survives-turn-and-completion-hook-quits`
- `.specs/session-editor-workspace/issues/06-session-workflow-compact.md`

## 2026-09-04 — Remaining headless producers use explicit EditorWorkspace lifetimes (E-20260904-005)

id: E-20260904-005
status: confirmed
scope: `.specs/session-editor-workspace/issues/07-subagent-headless-adapters.md`; EditorWorkspace acquisition and rollback; asynchronous Skills matching; Subagent host lifetime; Org Babel, Magit, and Rewrite blocking adapters; directly corresponding focused tests
source: Slice 07 Revision 5 implementation and verification summary; independent `slice07_final_recheck` PASS
verification: canonical ERT 1240 total, 1223 expected, 0 unexpected, 17 skipped; `RUNNER_GATE_OK`; independently rerun focused EditorWorkspace 34/34, Skills 23/23, Skills integration 11/11, Subagent 47 total/45 expected/0 unexpected/2 skipped, Ecosystem 16/16, Rewrite 23/23

### Observation

EditorWorkspace now exposes an additive acquisition operation returning the
plain plist `(:buffer BUFFER :newly-acquired BOOLEAN)` while the prior
buffer-only lookup remains unchanged.  Lookup/create and freshness are one
authority-owned synchronous transaction.  Exact rollback is fresh-only and
idempotent: false freshness is an unconditional no-op, a Session/role/buffer
identity mismatch has no effect, and ordinary kill query or hook
`error`/`quit` cannot authorize force killing.  A live survivor publishes
`retained`, remains visible through marker-derived enumeration, and can be
retried through public Session close.  The operation result is not durable,
does not enter a registry, and is not a lease, attestation, token, or kill
capability; rollback never closes the whole Session.

Asynchronous Skills semantic matching now treats its child Session as an
operation-scoped editor lifetime.  One guard claims callback delivery and
public close before either untrusted callback or close hook can run.  Real
Runtime tests cover success, cancellation, provider `error`/`quit`, callback
`error`/`quit`, and post-accept propagated conditions; the genuine
pre-accept fixture replaces only the public submit seam while keeping real
EditorWorkspace acquisition and close.  Callback and close each occur at most
once, the primary provider/callback/submission condition survives secondary
close or diagnostic failure, five consecutive rejection operations have zero
live-host growth, and refusal produces an actionable retained child that a
later public close releases.

Subagent now distinguishes Runtime acceptance from genuine pre-accept
rejection without changing Runtime.  Either a valid non-empty Run/Turn handle
or delivery before submit returns proves acceptance, and the delivery proof
is published before callback-facing work.  All accepted terminal and callback
outcomes retain the exact child Session host.  Only a synchronous exit with no
acceptance proof invokes rollback, and only when the same acquisition reports
fresh; role reuse remains live.  Rollback failure is secondary and a survivor
is retained for public retry.  Existing Actor, Team, parent Run, Artifact
Workspace, running/team projection, and callback-at-most-once behavior stayed
green.

Org Babel, Magit, and Rewrite submit the exact invocation/source buffer as
borrowed input.  An ordinary buffer receives an adapter-call Session without
marker mutation; an exactly owned buffer reuses its public owner; malformed
exact-local metadata is rejected; inherited/default metadata follows the
ordinary borrowed path; and marker locality and values remain unchanged.
The real adapter entry derives the Session from the queried owner or mints it
for an unowned buffer, making owner mismatch structurally unreachable there;
the public validation authority still rejects an exact mismatch.  Rewrite's
source and later preview buffers remain distinct identities.

### Interpretation

Revision 5 completes the Slice07 correction as a compatible refinement inside
the approved breaking migration.  It introduces no new durable concept or
second lifecycle authority: EditorWorkspace remains the only ownership,
freshness, rollback, and close authority; Skills owns only the trigger ending
its operation child; Subagent owns only the acceptance/pre-accept choice for
its trackable child; Runtime continues to own Run publication and terminal
delivery and merely borrows exact buffers.  The earlier Revision 4 green
numbers remain historical because their tests did not prove discoverability
or the full terminal matrix; the final recheck closed those findings with
real Runtime and EditorWorkspace paths.

### Recommended next action

Run `arrange` for Slice 08, `.specs/session-editor-workspace/issues/08-plugin-compatibility.md`.
Its `blocked_by: 05` dependency is already satisfied by `E-20260904-003`;
Slice07 is now done and verified, while Slice09 remains blocked on Slice08 in
addition to its other completed dependencies.

### Verification

Independent `slice07_final_recheck` reported PASS with no findings.  It closed
the prior Skills guard-ordering, real Skills/Subagent lifecycle matrix, and
real adapter-classifier coverage requirements.  Its canonical run reported
1240 total, 1223 expected, 0 unexpected, 17 skipped, and `RUNNER_GATE_OK`.
Because this learn pass changed only architecture/evidence/status documents on
the same product/test tree, it reused that canonical result and independently
reran all six focused groups: 34/34, 23/23, 11/11, 47 total with 45 expected
and 2 skipped, 16/16, and 23/23; all had zero unexpected results.

Before learn, strict Kernel checking reported 1035 references, 44 errors, and
0 warnings, all mechanical construct drift from the Slice07 code/test
insertions.  K17 re-anchors those references and promotes the confirmed
Revision 5 semantics.  Final strict checking reports 1062 references, 0
errors, and 0 warnings; the authority-map checker reports `ok`.  The generated
pairs report contains 84 entries, 533 shared groups, 213 universal sentences,
and 138 narrowed sentences in 36 entries.

### References

- `superchat-editor-workspace.el:331@superchat-editor-workspace-buffer`; `:393@superchat-editor-workspace-acquire-buffer`; `:409@superchat-editor-workspace-rollback-acquisition`; `:436@superchat-session-workspace-close`
- `superchat-skills.el:362@superchat-skills-match`
- `superchat-subagent.el:1010@superchat--subagent-runtime-request`; `:1056@superchat--subagent-run-async`
- `ob-superchat.el:81@ob-superchat--call-llm`; `superchat-magit.el:80@superchat-magit--call-llm`; `superchat-rewrite.el:167@superchat-rewrite--call-llm-raw`
- `test/test-editor-workspace.el:200@editor-workspace/acquisition-reports-freshness-atomically`; `:216@editor-workspace/rollback-is-fresh-exact-and-idempotent`; `:245@editor-workspace/rollback-retains-query-and-hook-failures`
- `test/test-skills.el:123@skills/real-runtime-terminal-matrix-closes-once`; `:170@skills/real-runtime-preaccept-rejections-do-not-accumulate`; `:188@skills/rejection-refusal-is-retained-retryable-and-secondary`; `:225@skills/reentrant-delivery-callback-and-close-are-once`
- `test/test-subagent.el:577@subagent/preaccept-rejection-rolls-back-fresh-only`; `:621@subagent/delivery-before-propagated-signal-proves-acceptance`; `:689@subagent/rollback-refusal-retains-public-retry-host`
- `test/test-ecosystem.el:140@ecosystem/blocking-adapters-use-public-identity-matrix`; `test/test-rewrite.el:236@rewrite/blocking-source-identity-matrix-is-public-and-exact`
- `.specs/session-editor-workspace/issues/07-subagent-headless-adapters.md`

## 2026-09-04 — Bundled Telega and published plugin samples use exact Session hosts (E-20260904-006)

id: E-20260904-006
status: confirmed
scope: `.specs/session-editor-workspace/issues/08-plugin-compatibility.md`; bundled Telega Runtime producers and their focused tests; `docs/plugin-guide.md`; the two active Experience experiment samples
source: Slice 08 implementation verification and independent `slice08_final_check` PASS
verification: final-check target ERT 1/1, 0 unexpected; independently rerun plugin standalone ERT 46 total, 45 expected, 0 unexpected, 1 skipped; reused unchanged canonical ERT 1240 total/1223 expected/0 unexpected/17 skipped with `RUNNER_GATE_OK`, Kernel strict 1062 references/0 errors/0 warnings, authority map `ok`, and two execution-env-unset experiment smokes at exit 0

### Observation

The bundled Telega reply, digest, triage, and contextual Session producers now
derive their existing stable Session IDs before composition, obtain their
role-specific hosts through the public EditorWorkspace interface, validate the
same exact live buffer, and submit that object through `:buffer`.  Extension
composition receives an identity acceptance function: changing or removing
the producer Session or exact buffer is rejected before activation Tape,
Runtime, or provider effects.  All four producers remain asynchronous and
retain their existing tool, approval, scope, capability, policy, history, and
callback behavior.

Real Runtime terminal and delivery tests cover success, cancellation,
provider-error-shaped failure, callback `error`, and callback `quit`.  The
ordinary Session-owned Telega hosts remain live after every terminal path and
are released only by public Session close or explicit user closure.  Negative
authority fixtures cover fresh acquisition, role reuse, wrong-Session owned
buffers, malformed local metadata, inherited/default metadata, and same-name
foreign buffers without adding a producer-side ownership classifier.  The
final correction separately proves an actually missing `:buffer` key and an
explicit `:buffer nil` value: `cl-remf` removes the full key/value pair, an
absence assertion validates the fixture outside the broad rejection form, and
the real compose/acceptance path rejects both with zero activation, Runtime,
or provider effects while the original host survives.

The public plugin guide now uses public EditorWorkspace allocation and
validation with an exact `:buffer`, states the ordinary borrowed versus owned
host lifetime boundary, and identifies the legacy host fields as compatibility
inputs pending Slice09 removal.  It deliberately leaves the numeric breaking
release unspecified and says repository searches cannot establish migration
of external consumers.  `paired-use.el` and `recovery-self-learning.el` remain
active opt-in samples and now use stable experiment Sessions, public
role-specific hosts, validation, and exact buffers; their environment-unset
loads make no provider call.

### Interpretation

This is a verified application of the enacted K17 contracts, not a new
concept, identity, lifecycle, invariant, authority boundary, or Action
Contract.  EditorWorkspace remains the sole ownership/validation/close
authority, Extension acceptance remains before activation effects, Runtime
borrows exact buffers, and ordinary Session hosts outlive terminal Runs.
Accordingly no Kernel, Protocol, ADR, Runbook, Lesson, or CONTEXT promotion is
warranted and K17 remains current.  Repository production producers are free
of the legacy host keys, but repository-external consumers remain unknown.
The removal release has no confirmed numeric version; this Evidence promises
neither `2.0.0` nor compatibility across all of `1.x`.  Publishing and provider
execution were outside scope.

### Recommended next action

Run `arrange` for Slice 09,
`.specs/session-editor-workspace/issues/09-retire-hostlease-cleanup.md`.
Slice08 is done and its dependency evidence is this record.  Slice09 has not
been executed or marked ready, and Runtime's legacy compatibility path remains
until that slice is implemented and independently verified.  The overall
`session-editor-workspace` SPEC is therefore still active.

### Verification

Independent `slice08_final_check` reported PASS with every Required finding
closed.  In this learn turn the final checker target 1/1 and plugin standalone
46 total/45 expected/0 unexpected/1 skipped are recorded as newly run by that
checker.  The canonical 1240/1223/0/17 `RUNNER_GATE_OK`, strict Kernel
1062/0/0, authority-map `ok`, and two environment-unset smoke exit-0 results
are explicitly reused from the preceding independent recheck; this learn turn
did not rerun ERT or the experiment smokes.  Search evidence is retained by
classification rather than combining incompatible key/line counts: ordinary
shipped producers, bundled-plugin production, active experiments, and current
guide examples have zero legacy producer use; Runtime compatibility code,
legacy test fixtures, compatibility prose, and historical records remain
separate; external consumers are unknown.

After this documentation-only learn update, the lightweight strict Kernel
check reports 1062 references, 0 errors, and 0 warnings, and the authority-map
checker reports `ok`.  No production/test code, provider, publication, or
Git/JJ write is part of this Evidence.

### References

- `plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:35@superchat-cowork-telega--request-buffer`; `:40@superchat-cowork-telega--compose-request`; `:642@superchat-cowork-telega--request`; `:790@superchat-cowork-telega--digest-request`; `:948@superchat-cowork-telega--triage-request`
- `plugins/superchat-cowork-telega/superchat-cowork-telega-session.el:377@superchat-cowork-telega--session-request`
- `plugins/superchat-cowork-telega/test-cowork-telega.el:817@cowork-telega/four-request-shapes-use-exact-owned-hosts`; `:869@cowork-telega/four-request-shapes-enforce-public-host-authority`; `:1008@cowork-telega/ordinary-hosts-survive-runtime-terminals-until-close`; `:1077@cowork-telega/callback-nonlocal-exits-cross-real-runtime`; `:1156@cowork-telega/extension-cannot-retarget-before-activation-tape`
- `docs/plugin-guide.md:101`; `:133`; `:162`; `:182`; `:189`
- `experiments/experience-learning-minimal/paired-use.el:474`; `:615`; `experiments/experience-learning-minimal/recovery-self-learning.el:163`; `:175`; `:399`
- `.specs/session-editor-workspace/issues/08-plugin-compatibility.md`

## 2026-09-05 — Runtime editor HostLease and Cleanup contracts are retired (E-20260905-001)

id: E-20260905-001
status: confirmed
scope: `.specs/session-editor-workspace/issues/09-retire-hostlease-cleanup.md`; Runtime exact-host admission, terminal quiescence and publication; current Session EditorWorkspace authority and public compatibility boundary
source: Slice09 implementation verification and independent `slice09_check` final PASS
verification: checker reran admission 2/2 and `check-parens`; reused independently verified Runtime 103/103, LLM backend 178 total/173 expected/0 unexpected/5 skipped, native UI 51/51, Compact 39/39, plugin 46 total/45 expected/0 unexpected/1 skipped, canonical 1220 total/1203 expected/0 unexpected/17 skipped with `RUNNER_GATE_OK`, two env-unset experiment smokes exit 0, scratch compile with two pre-existing unused-`err` warnings, and Authority map `ok`

### Observation

Runtime no longer defines or reconstructs HostLease/editor Cleanup, acquires or
releases an ephemeral editor host, exposes Cleanup failure/retry/abandon, or
accepts `:ephemeral-buffer` and `:host-authority-mode`. Host admission requires
one live exact `:buffer` and rejects missing, nil, dead, or retired-field input
before Run, Tape, provider, or host effects. Diagnostic buffer names derive
from that object and cannot retarget it.

The representation-only lease/substitution/unresolved/retry tests were removed
only after their surviving guarantees moved to exact-host behavior tests. Every
terminal branch still uses the private idempotent quiescence seam; atomic Run
publication and at-most-once delivery remain, and success, cancellation,
provider failure, audit failure, or publication rollback never makes Runtime
kill the borrowed exact buffer. EditorWorkspace remains the sole owner and
close authority.

The current guide names this as a breaking removal and preserves the public
Session EditorWorkspace plus exact-buffer example. Repository shipped/core,
bundled-plugin production, and both active experiments have zero legacy
producers. Historical references remain historical; repository-external
consumers are unknown, the numeric release is unspecified, and no publication
was authorized.

### Interpretation

This verifies the breaking retirement already confirmed by Revision 5 and
supersedes K10/K11's temporary Runtime editor-ownership and Cleanup contracts.
It does not retire Runtime quiescence, atomic publication, terminal delivery,
or exact borrowed-buffer identity from K15-K17. K18 retains Runtime as the sole
Run lifecycle owner, EditorWorkspace as the sole editor ownership/close owner,
and the prohibition on Kernel dependencies on middle-tier modules.

No new ADR, Lesson, Runbook, schema, durable identity, registry, classifier, or
Protocol action is introduced. The existing close Protocol and CONTEXT receive
only the minimum removal of wording that still claimed legacy compatibility
was enacted.

### Recommended next action

All nine `session-editor-workspace` slices are implemented, independently
checked, and evidence-linked. Keep the SPEC active administratively: the
upstream SPEC terminal-authority gap still prevents a doctrine-conformant
formal closure, so the next permitted work is a separate `plan` for that
upstream gap rather than more EditorWorkspace implementation. External release
version selection and publication remain outside this local acceptance.

### Verification

The independent final checker newly ran the two admission tests (2/2), loaded
the corrected public submit documentation, and ran `check-parens`. This learn
turn did not rerun ERT; it reuses the independent focused, canonical, plugin,
experiment-smoke, and scratch-compile results listed in the record header.

Before learn, citation checking reported 1062 references, 159 errors and 0
warnings: 94 Runtime, 38 Runtime-test, 26 LLM-test, and 1 native-UI citation.
K18 removes citations belonging to the approved K10/K11 retirement and
re-anchors retained contracts to verified same-name constructs. Shared
reconcile/status, Evidence/Closure, LLM pause/context, and tool-test movements
were mechanically re-anchored but are not attributed as Slice09 semantics.
After the documentation updates, the freshly run strict gate reports 979
references, 0 errors and 0 warnings; the Authority-map gate reports `ok`.

The pre-edit pairs report had 84 entries, 533 shared groups, 213 universal
sentences, and 138 narrowed sentences in 36 entries; the final K18 report has
82 entries, 480 shared groups, 204 universal sentences, and 128 narrowed
sentences in 35 entries. Counterexamples for the
new universal wording were hunted by scanning Runtime and the full tree for
legacy fields/functions and editor close/kill paths, reading host admission,
publication, finish/audit/cancel/quiesce branch by branch, and checking the
Runtime/EditorWorkspace authority map. The fresh-context default-to-refute
review was `slice09_check`; its sole docstring Required was corrected and its
final independent report closed every Required finding before this learn.

### References

- `superchat-runtime.el:1434@superchat-runtime--validate-host-admission`; `:1487@superchat-runtime-submit`; `:1998@superchat-runtime--quiesce`; `:213@superchat-runtime--publish-entry`
- `test/test-runtime.el:443@runtime/every-live-terminal-path-quiesces-exactly-once`; `:2461@runtime/exact-buffer-must-be-live-before-any-effect`; `:2476@runtime/exact-buffer-is-borrowed-and-name-is-diagnostic`; `:2510@runtime/exact-buffer-publication-rollback-preserves-caller-object`; `:2530@runtime/exact-buffer-success-keeps-borrowed-host-live`; `:2558@runtime/exact-buffer-cancel-keeps-borrowed-host-live`; `:2579@runtime/exact-buffer-provider-error-keeps-borrowed-host-live`
- `test/test-llm-backend.el:7713@test-runtime-quiescence-is-idempotent-and-preserves-borrowed-host`; `:7762@test-runtime-quiescence-isolates-adapter-errors-before-pause-clear`
- `docs/plugin-guide.md`; `.specs/session-editor-workspace/issues/09-retire-hostlease-cleanup.md`; independent `slice09_check` final report

## 2026-09-05 — Exact synchronous View routing and parameterized Tape kinds are restored (E-20260905-002)

- **ID:** `E-20260905-002`
- **Status:** confirmed
- **Scope:** the approved A+B architecture-fix batch and its mechanical Kernel citation re-anchor
- **Knowledge category:** verified Evidence plus mechanical citation maintenance; no additional product-semantic promotion

### Observation

A fixed two reproducible synchronous UI collision paths: `superchat--ensure-chat-view` and `superchat--render-session-view` now hand Render the selected EditorWorkspace buffer's actual post-rename/post-mode-init name. Two regressions exercise real allocation and Render/reopen paths, preserve a foreign buffer's contents, mode state, modified state, and exact pre-existing Render marker object/buffer/position/insertion type, and retain the selected buffer's prompt marker, history, and public validation. B parameterized Tape View `kinds` as bound query values; one matrix regression covers cursor behavior, two kinds, and a distinguishable real limit truncation. Independent final checks closed A's missing foreign-marker assertion and B's initially non-discriminating limit fixture; neither finding required a production change.

### Interpretation

The selected exact object remains the synchronous Render target instead of being re-routed by a stale requested name, and query values do not participate in SQL structure. These are restorations of the existing authority and query contracts, not an asynchronous Render migration, a new identity/registry/lifecycle rule, or a new query language. K18 `since` values and semantic text therefore remain unchanged; K19 records only verified evidence and citation movement.

### Recommended next action

Treat A+B as closed after this learn gate. Any deferred architecture candidate requires its own plan and validation; this batch does not alter the existing upstream terminal-authority blocker.

### Verification

- A final check: focused 2/2, Native UI 53/53, and `check-parens` passed.
- B final check: focused 1/1, Tape View 70/70, and `check-parens` passed.
- Canonical evidence is reused from B's independent check: 1223 total, 1206 expected, 0 unexpected, 17 skipped, `RUNNER_GATE_OK`. It ran before the final limit-fixture correction; that final test-only increment is covered by the focused and Tape View runs above, not represented as a post-correction canonical rerun.
- Before this learn, strict citation check reported 979 refs, 18 errors, 0 warnings. After exact re-anchoring it reports 979 refs, 0 errors, 0 warnings; the authority checker reports `ok`.

### References

- `superchat.el:2365@superchat--ensure-chat-view`; `superchat.el:2416@superchat--render-session-view`
- `test/test-native-ui.el:167@native-ui/ensure-chat-view-keeps-render-on-exact-owned-buffer`; `test/test-native-ui.el:239@native-ui/render-session-view-keeps-rebuild-on-exact-owned-buffer`
- `superchat-tape-view.el:25@superchat-view-entries`; `test/test-tape-view.el:1995@view/entries-binds-kinds-without-changing-query-contract`
- `docs/architecture-review-20260905/04-target-architecture.md` D3 sections 4, 8, and 9; Herdr final reports `arch_ui_final_check` and `arch_query_final_check`

## 2026-09-05 — Workflow recipes have a safe public edit entry (E-20260905-003)

id: E-20260905-003
status: confirmed
scope: `.specs/workflow-edit/issues/01-edit-entry.md`; public Workflow recipe discovery and ordinary Emacs editing
source: Slice01 implementation verification and independent `arch_fix_checker` PASS
verification: reused checker runs: focused edit 5 total/4 expected/0 unexpected/1 skipped; Workflow 55 total/53 expected/0 unexpected/2 skipped; fresh autoload invocation passed; layering 1/1; canonical 1228 total/1210 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; `check-parens` passed

### Observation

`M-x superchat-workflow-edit` is a lazy public entry that offers existing recipe names while accepting a new basename.  It composes the single configured Workflow directory and existing list, NFC resolver, prospective-path, and loader helpers: a unique existing recipe opens at its actual path without renaming an NFD file or changing its content; a new name uses an NFC basename and opens a real file-visiting buffer whose minimal parseable template is inserted only when the target is genuinely new and has no existing visiting buffer.  Re-entering the same unsaved draft preserves its exact contents.  Ordinary `save-buffer` makes it discoverable and loadable; the command does not automatically write or execute a recipe, submit Runtime work, or call a provider.

Invalid and nested names, unsafe paths, symlinks, and ambiguous NFC candidates are rejected before opening a target.  Independent temporary-filesystem probes additionally confirmed that an existing empty recipe stays empty and unmodified, a user-cleared unsaved draft stays empty on re-entry, and a dangling symlink opens neither itself nor its foreign target.  The private resolver, prospective-path interface, Authority-mode parameter chain, directory model, and Runtime/EditorWorkspace/transport boundaries did not change.

### Interpretation

This is the compatible user Action Contract approved in `.specs/workflow-edit/SPEC.md` Revision 1.  K20 promotes that verified contract and records `superchat-workflow.el` as the sole Workflow path-resolution/edit authority; `superchat.el` owns only its lazy autoload declaration.  It adds no registry, classifier, save layer, durable identity, lifecycle, or CONTEXT vocabulary, so no Protocol, ADR, Runbook, Lesson, or CONTEXT change is warranted.

### Recommended next action

Treat Workflow Edit Slice01 as functionally delivered and evidence-linked.  Do not reopen its implementation.  The existing upstream SPEC terminal-authority gap remains a separate administrative blocker; it prevents claiming formal SPEC closure but does not negate this verified Action Contract.

### Verification

This learn turn did not rerun ERT.  It reuses the independent checker results listed above, including real filesystem, visiting-buffer, ordinary-save, resolver/loader, fresh-autoload, and layering paths.  The two Workflow ambiguity tests were capability-guarded skips because this filesystem could not preserve two physically distinct NFC-equivalent names; they are not reported as passing.  Existing pure ambiguity classification plus real resolver, Unicode, and symlink tests passed.

Before learn, strict Kernel checking reported 979 references, 54 errors, and 0 warnings.  The checker classified every error as insertion-driven mechanical line movement with no symbol loss.  K20 re-anchors those same-name/same-semantics references and adds the approved contract; the fresh strict result is 992 references, 0 errors, and 0 warnings, and the authority-map form check reports `ok`.

### References

- `superchat-workflow.el:107@superchat-workflow-directory`; `:131@superchat-workflow--list`; `:172@superchat-workflow--resolve`; `:212@superchat-workflow--file-path`; `:226@superchat-workflow--load`; `:245@superchat-workflow--completion-list`; `:250@superchat-workflow-edit`
- `superchat.el:69@autoload/superchat-workflow-edit`
- `test/test-workflow.el:177@workflow/edit-new-draft-reuses-saves-and-loads`; `:233@workflow/edit-existing-nfd-opens-actual-file-unchanged`; `:260@workflow/edit-rejects-invalid-and-unsafe-before-opening`; `:278@workflow/edit-rejects-real-nfc-ambiguity-before-opening`; `:305@workflow/edit-autoloads-in-fresh-emacs`
- `.specs/workflow-edit/SPEC.md`; `.specs/workflow-edit/issues/01-edit-entry.md`; independent `arch_fix_checker` final report

## 2026-09-05 — Workflow drops an unused Mode relay while preserving call compatibility (E-20260905-004)

id: E-20260905-004
status: confirmed
scope: Workflow request construction and its private step-call compatibility
knowledge_class: verified observation; no new durable semantic rule

### Observation

Workflow no longer captures, recomputes, or recursively relays `last-known-authority-mode`.
The private step function retains its seventh optional slot as an explicitly ignored
argument; Session remains in the eighth slot. Initial and recursive callers pass nil
in the compatibility slot. Corresponding tests preserve View/Session behavior instead
of requiring an unused Mode value to be forwarded.

### Interpretation

The relay was not an authority source. Independent two-step probes with an arbitrary
seventh-slot sentinel obtained the configured `deny` policy and the `default` fallback
through real Session configuration. Authority still reads Mode in the execution buffer.
No concept, identity, relation, lifecycle, invariant, or Action Contract changed; K20,
its authority map, and CONTEXT remain unchanged. `--file-path`, the resolver, and the
new Workflow edit command are deliberately retained, not simplification leftovers.

### Recommended next action

Close this short-path cleanup. Do not recreate the Mode relay or infer authorization
to remove other private interfaces. External private consumers remain unknown; this
change preserves positional calling compatibility rather than claiming to audit them.

### Verification

This learn reuses the independent `workflow_authority_check` results: focused 2/2;
Workflow 55 total / 53 expected / 0 unexpected / 2 filesystem-capability skips;
three real LLM/Runtime Authority selectors 3/3; canonical 1228 total / 1210 expected /
0 unexpected / 18 skipped, `RUNNER_GATE_OK`; syntax and residue checks passed.
The configured-policy/default probe passed. Kernel strict was 992 refs / 0 errors /
0 warnings, and authority-map form was valid. No ERT rerun is claimed by this learn.

### References

- `superchat-workflow.el:574@superchat-workflow--run-step` and its initial/recursive callers.
- `test/test-workflow.el:692@workflow-execute-targets-current-session-view`.
- `test/test-workflow.el:806@workflow/dead-view-keeps-session-and-reuses-owned-role-host`.
- Herdr `arch_query_worker` implementation report and independent `arch_fix_checker`
  `workflow_authority_check` final report; this cleanup has no SPEC or Slice.

## 2026-09-05 — Architecture guide aligned with current implementation (E-20260905-005)

id: E-20260905-005
status: confirmed
scope: architecture guide and four 2026-09-05 architecture-review documents
knowledge_class: verified observation; no new durable semantic rule

### Observation

Independent `architecture_docs_final_check` closed all three Required groups:
preparation, command, and Runtime notification hook contracts are distinguished;
the Turn table includes every field and its actual writers; Skills discovery,
initialization seeding, standard interchange, and async Workflow execution now
describe the real entry paths. Current source anchors are distinguished from
explicitly historical pre-fix P1 reproductions.

### Interpretation

P8 documentation drift is resolved within this reviewed scope. This changes no
product behavior or Action Contract and creates no second authority: K20 and
CONTEXT remain unchanged. It does not close P3–P6, the deferred full Render
object-identity migration, or the upstream SPEC terminal-authority blocker.

### Recommended next action

Treat this docs-only correction as checked. Investigate a concrete remaining
dependency before proposing another refactor; do not infer implementation
approval or system-wide correctness from document consistency.

### Verification

Reuse the independent final checker: all six Elisp fences were readable by
Emacs, local Markdown links across all five documents resolved, and no trailing
whitespace was found. Current `superchat-submit` and `superchat--cmd-remember`
anchors were 2799 and 3556; P1's 2365/2416 references explicitly describe the
pre-fix historical reproduction. No ERT, canonical suite, or provider was run
for this docs-only acceptance; static readability is not execution evidence.

### References

- `docs/architecture.md`, hook contracts, Turn table, and Skills/Workflow examples.
- `docs/architecture-review-20260905/01-evolution.md`, `02-problems.md`,
  `03-refactoring.md`, and `04-target-architecture.md`.
- Herdr `arch_query_worker` `architecture_docs_required_fix` final report and
  independent `arch_fix_checker` `architecture_docs_final_check` final report.

## 2026-09-05 — Tool user-quit accounting is durable without becoming recovery-learning evidence (E-20260905-006)

id: E-20260905-006
status: confirmed
scope: `.specs/tool-quit-accounting/issues/01-tool-quit-accounting.md`; Authority tool outcome accounting, Runtime exact-Run interruption and recovery admission, Experience recovery-learning eligibility, and Closure replay
source: Slice01 implementation, required-fix verification, and independent `tool_quit_recheck` final PASS
verification: reused checker runs: Authority 87/87; Runtime 107/107; Experience 30/30; Closure 27/27; Agent Loop 27 total/26 expected/0 unexpected/1 skipped; canonical 1247 total/1229 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; eight relevant Elisp files passed `check-parens`; Authority map form passed

### Observation

Authority now records a user-interrupted pipeline with one phase-labelled
`tool.failed` when no outcome is already durable.  `pre` proves the
implementation was not called, `implementation` records that it began without
claiming its effects were absent, and `result-processing` preserves an already
returned implementation receipt.  Receipt presence is independent of value:
a legitimate `nil` result has receipt size/hash but no final-result fingerprint.

Ordinary and exact-evidence success both filter and freeze before persisting
`tool.completed`, notify post hooks with a detached copy, and deliver the same
frozen result.  Failure notification likewise observes a durable failure through
detached data.  Notification mutation or return cannot change the outcome, and
a notification `quit` cannot append a contradictory second outcome or replace a
real implementation failure.  Minimum accounting and exact-Run cancellation are
attempted before the original `quit` symbol/data is re-signalled; a secondary
cleanup condition does not replace it.

Runtime's request-local callback captures one Run and reuses its existing cancel
owner across synchronous re-entry, delayed async, late/repeated callbacks, and
two simultaneous Runs.  When interruption audit append fails, the existing
audit-failure seam seals `audit-incomplete` without fabricating cancellation or
success.  The interruption failure remains on Tape and Closure accounts its
Effect as failed, while Runtime recovery admission and every Experience
recovery-learning consumer exclude it through their respective single owner
classifiers.  Ordinary typed failures remain eligible.  Review Needs may retain
a `missing-outcome` blocked completeness diagnostic; it is not an open learning
sample and was not removed.

### Interpretation

This verifies the user-approved Revision 1 semantic change and promotes it in
K21.  It refines `tool.failed`, successful outcome ordering, notification
semantics, exact-Run interruption, and recovery-learning admission without a new
Tape event/schema, Run state, registry, dependency-injection seam, recovery
category, or second Authority.  Permission, approval, rewrite, ordinary hook
`error`, headless operation, append-only Tape, Closure transition, and all K12–K20
ownership contracts remain in force.  CONTEXT and the EditorWorkspace close
Protocol need no new vocabulary or rule.

### Recommended next action

Treat the Slice implementation as verified and evidence-linked.  Keep its SPEC
`confirmed`: the existing upstream SPEC terminal-authority gap still prevents a
doctrine-authorized formal closure.  A real interactive keyboard `C-g` path is
not proved by these Lisp condition tests and remains an explicit evidence limit,
not a blocker to the approved control-flow contract.

### Verification

This learn turn did not rerun ERT.  It reuses the independent checker totals in
the record header, including the real temporary SQLite/Tape Authority→Runtime,
Experience-consumer, and Closure-replay paths; tools and transports were local
fakes.  The tests signal Lisp `quit` and do not claim a physical keyboard `C-g`.

Before learn, strict Kernel checking reported 992 references, 142 errors and 0
warnings.  The errors were fixed-line citations displaced by the verified
Authority/Runtime/test insertions.  K21 re-anchors each to the same named
construct and adds the approved contract.  The freshly run strict gate reports
1014 references, 0 errors and 0 warnings; the Authority-map form gate reports
`ok`.

### References

- `superchat-authority.el:807@superchat-authority--implementation-receipt`; `:943@superchat-authority--handle-quit`; `:988@superchat-authority--finish-success`; `:1074@superchat-authority--finish-error`
- `superchat-runtime.el:1076@superchat-runtime--audit-incomplete`; `:1326@superchat-runtime-cancel`; `:1490@superchat-runtime-submit`
- `superchat-experience.el:1299@superchat-experience--learning-tool-outcome-p`; `:1402@superchat-experience--run-prefix`; `:1726@superchat-experience-review-needs`; `:1980@superchat-experience--run-skeletons`
- `test/test-authority.el:1408@authority/filter-quit-retains-a-nil-implementation-receipt`; `:1529@authority/exact-quit-matrix-preserves-one-durable-outcome`; `test/test-runtime.el:2671@runtime/authority-quit-cancels-exact-run-through-real-bridge`; `:2707@runtime/interruption-audit-gap-seals-incomplete-not-cancelled`; `test/test-experience.el:247@experience/user-interruption-is-absent-from-all-learning-views`; `test/test-closure.el:495@closure/user-interrupted-tool-is-still-an-accounted-failure`
- `.specs/tool-quit-accounting/SPEC.md`; `.specs/tool-quit-accounting/issues/01-tool-quit-accounting.md`; independent `arch_fix_checker` `tool_quit_recheck` final report

### Leader completion verification

After worker learn, the leader clarified K21's phase entry as the durable
`tool.execution.started` event, not implementation invocation (the `pre` phase
precedes invocation). No production or test changed. Fresh final canonical:
1247 total / 1229 expected / 0 unexpected / 18 skipped, `RUNNER_GATE_OK`.
Fresh strict: 1014 refs / 0 errors / 0 warnings; authority-map form valid.
Evidence/Slice linkage and focused whitespace checks passed. The independent
ten-AC acceptance remains the behavioral evidence; the keyboard-C-g and formal
SPEC-closure limitations above remain unchanged.

## 2026-09-05 — Async transcript callbacks retain their exact View object (E-20260905-007)

id: E-20260905-007
status: confirmed
scope: P1 asynchronous LLM, Runtime, Authority-tool, and Render callback targeting
source: tests-only regression implementation and independent `architecture_p1_regressions_check` PASS
verification: reused independent runs: target 2/2; LLM 180 total/175 expected/0 unexpected/5 skipped; Native UI 53/53; Runtime 107/107; canonical 1249 total/1231 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; `check-parens` and Authority-map form passed

### Observation

The production entry paths already capture the exact View buffer object before
asynchronous work begins. A real local LLM callback followed a renamed owner
through the actual Render stream, Thinking, terminal, and prompt paths while a
foreign buffer occupied the old name. A second real Runtime/Authority/Render
path killed the owner, created a same-name replacement, and delivered late
stream, terminal, and asynchronous tool callbacks without writing the
replacement, re-invoking the tool implementation, or publishing another Run or
Turn terminal event. Buffer content, prompt-marker identity and placement,
mode, modified state, owner regions, and tool transcript count were asserted.

### Interpretation

P1's remaining asynchronous production concern is resolved as an
evidence-backed no-change: repository Runtime/LLM/tool-wrapper entry points do
not reproduce stale-name retargeting. This does not claim that every private
Render helper has an explicit buffer parameter, nor that unknown external
private callers or advice share the protected entry conditions. It adds no
owner, registry, lifecycle, or public compatibility rule, so existing Kernel
semantics and their `since` values remain unchanged.

### Recommended next action

Treat P1 as complete for repository production entry points. Continue with
P3's open Runtime→Render/Experience and LLM→wrapper/synthesis seams; do not use
this evidence to authorize a broad Render API migration.

### Verification

This learn turn reuses the independent ERT results listed above and does not
claim to rerun them. Tests use local mock transport and temporary buffers, not
a provider or user database. A process-local negative control replaced the
exact callback wrapper with stale-name lookup and the owner-behavior test failed
0/1 as intended. Actual GUI scheduling, provider callbacks, unknown external
private calls, and arbitrary advice remain outside the evidence.

Before learn, strict checking reported 1014 references, 11 errors, and 0
warnings; all eleven were fixed-line citations displaced by the two accepted
tests. K22 mechanically re-anchors them to the same named constructs and adds
this evidence to the existing Render contract without changing its semantics.

### References

- `superchat-runtime.el:988@superchat-runtime--deliver`; `:1490@superchat-runtime-submit`
- `superchat-llm.el:1603@superchat--callback-in-view`; `:1666@superchat--llm-generate-answer`
- `superchat-agent-loop.el:280@superchat--agent-render-tool-call`; `:302@superchat--agent-render-tool-result`
- `superchat-render.el:500@superchat--response-region-prepare`; `:755@superchat--response-region-close`; `:887@superchat--response-region-stream`
- `test/test-llm-backend.el:3124@test-llm-render-callbacks-follow-renamed-owner-not-old-name-foreign`; `:3210@test-runtime-dead-owner-late-stream-terminal-tool-never-retargets`
- Independent `architecture_p1_regressions_check` final report

## 2026-09-05 — Bare Runtime treats absent Render presentation as optional (E-20260905-008)

id: E-20260905-008
status: confirmed
scope: P3 Runtime-to-Render headless compatibility and bounded no-change classifications
source: Runtime short-path repair and independent Runtime-to-Render check PASS
verification: reused independent runs: target 4/4; Runtime 111/111; Native UI 53/53; LLM 180 total/175 expected/0 unexpected/5 skipped; canonical 1253 total/1235 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; owned `check-parens` and Authority-map form passed

### Observation

Bare `superchat-runtime` previously called the absent Render status and stream
functions directly.  Runtime now makes only those two default presentation
handoffs conditional on the function being present; its already-guarded final
delivery remains unchanged.  A fresh child with no distribution or Render
loaded submitted both custom and default Requests, invoked the local fake
provider twice with the expected values, and left both Runs completed and
quiesced.  With Render functions present, status, stream, and final delivery
were all invoked; a Render `error` and original `quit` were not swallowed.

The bounded P3 investigation separately found the Authority-to-Agent Loop
compatibility seam useful as-is, and found no Runtime-to-Experience or
LLM-to-Synthesis defect.  Those are no-change classifications supported by
read-only caller and load-order probes, not new ERT coverage.  The raw LLM tool
path without a wrapper remains an open compatibility decision.

### Interpretation

This is a compatible headless repair to the existing Runtime/Render contract,
not a new presentation interface.  Runtime still owns Request lifecycle,
terminal publication, delivery and quiescence; Render still owns transcript
presentation.  Exact buffer and Session identity, Tape ordering, default final
delivery, and errors from present Render functions are unchanged.  The result
does not establish real GUI/provider behavior beyond the adjacent Render
regressions in the complete suite, and it does not close P3.

### Recommended next action

Keep P3 open for the raw-tool wrapper compatibility decision.  Continue the
architecture goal with a bounded P4 investigation of Request projections and
request-local Runtime-to-LLM controls.

### Verification

This learn pass reuses the independent checker results above and does not claim
to rerun ERT.  Before learn, the verified K22 baseline was 1016 references,
0 errors and 0 warnings; the 33 reported citation failures were all mechanical
movement caused by this batch's two production guard lines and four inserted
tests.  K23 re-anchors those references to the same named constructs.  No real
provider, GUI keyboard input, network, or user database was used.

### References

- `superchat-runtime.el:1490@superchat-runtime-submit`; `:988@superchat-runtime--deliver`; `:2017@superchat-runtime--quiesce`
- `test/test-runtime.el:718@runtime/runtime-submit-stays-headless-without-render`; `:795@runtime/runtime-submit-uses-render-functions-when-present`; `:831@runtime/runtime-submit-does-not-swallow-render-errors`; `:850@runtime/runtime-submit-does-not-swallow-render-stream-quit`
- `KERNEL.md` K23; independent Runtime-to-Render checker final report

## 2026-09-05 — Request-local LLM controls remain isolated across overlapping Runs (E-20260905-009)

id: E-20260905-009
status: confirmed
scope: P4 Runtime-to-LLM Request controls, delayed tool rounds, protocol retry, and cross-Run isolation
source: tests-only short-path verification and independent P4 check PASS
verification: reused independent runs: target 2/2; LLM 182 total/177 expected/0 unexpected/5 skipped; Runtime 111/111; canonical 1255 total/1237 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; `check-parens`, strict Kernel 1020 references/0 errors/0 warnings, and Authority-map form passed

### Observation

Two overlapping ordinary Runs traversed the real Runtime, LLM, registered local
tool, and Authority wrapper paths after both submission dynamic scopes had
ended. Distinct tools, Session/Run identities, copied context facts, wrapper
functions, and round-limit callables stayed with their originating Run. Both
tools executed once. Cancelling A made its late provider and tool callbacks
inert without ending or contaminating B; B completed through its own final
round and each Run published one matching terminal. A separate interleaving
proved retry budgets 0 and 1 remained local: A failed after one attempt while B
retried exactly once and completed.

### Interpretation

P4 is an evidence-backed, bounded production no-change. The current single
Runtime controls producer and LLM lexical callback model preserve the existing
Request projection contracts; no new control object, registry, schema, owner,
or Authority context is justified. This evidence does not cover unknown
external producers, every Cartesian combination of Request fields, or future
special variables. Reopen the investigation if a second producer appears, a
new async control is read without capture, absent/nil or callable identity
changes, mutable values cross the handoff without the required copy, a real
cross-Run leak appears, or defaults gain a second owner.

### Recommended next action

Mark P4 complete for current repository producers and proceed to the bounded P5
business-reader/physical-row investigation. Keep P3's raw-tool wrapper
compatibility choice open and do not infer that arbitrary future Request fields
are safe automatically.

### Verification

This learn turn reuses the independent ERT and canonical results in the record
header and does not claim to rerun them. The tests use local fake provider
edges, temporary buffers, real Runtime/LLM control flow, and real local
Authority-wrapped tools; no provider, network, or user database was used.
Source branches plus separate focused tests continue to support prepared-tools
absent versus present nil, persistence default versus explicit nil, callable
identity, deadline and token defaults, and defensive Request projections; the
two P4 tests do not themselves form one full field matrix. K23 was already strict at
1020/0/0 after the accepted tests, and no production semantic or citation
target changed, so this evidence does not manufacture K24.

### References

- `superchat-runtime.el:1490@superchat-runtime-submit`; `:1855` request-local LLM bindings
- `superchat-llm.el:1666@superchat--llm-generate-answer`; `superchat-agent-loop.el:486@superchat--agent-make-wrapper`
- `test/test-runtime.el:3048@runtime/forwards-request-local-transport-controls`
- `test/test-llm-backend.el:8019@test-runtime-overlapping-tool-round-controls-remain-run-local`; `:8174@test-runtime-overlapping-protocol-retries-remain-run-local`
- Independent P4 checker final report

## 2026-09-05 — Domain readers preserve distinct persisted-row contracts (E-20260905-010)

id: E-20260905-010
status: confirmed
scope: P5 Memory, Workspace, Team, and Claims persisted-reader compatibility
source: tests-only compatibility matrix and independent P5 check PASS
verification: reused independent runs: target 4/4; Memory 17/17; Workspace 30 total/29 expected/1 unexpected; Subagent 48 total/46 expected/0 unexpected/2 skipped; Claims 8/8; canonical 1259 total/1241 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; four owned files passed `check-parens`; Authority-map form passed

### Observation

Real temporary SQLite tests preserve four intentionally different reader
contracts. Memory globally selects `topic = memory` in storage order and stops
strictly at malformed metadata: its transaction checkpoint remains on the last
valid prefix, later rows do not materialize, and catch-up/rebuild do not create
duplicates. Workspace scans the global workspace topic, selects the requested
Workspace in its reducer, accepts current underscore and historical hyphen
envelope keys, skips malformed metadata, and still reads a later valid
Artifact. Team restricts one Team Session and its four topics, reads current
Runtime envelopes plus legacy topic/event rows, skips malformed metadata, and
continues in order. Claims retains global cross-Session `tape:N` compatibility
while typed event and storage references require the exact Session.

### Interpretation

P5 is an evidence-backed bounded no-change. The visible positional row knowledge
does not encode one interchangeable contract: scope, selected columns, legacy
input, malformed-data behavior, and checkpoint ownership differ by reader.
Replacing them with `superchat-tape-replay-events` would lose global Memory or
Workspace selection, Team legacy rows, or the current strict/tolerant error
distinction. A configurable facade would expose those differences as a wider
shallow Interface rather than remove them. Tape remains append-only, DB remains
the physical SQLite owner, Tape owns logical Runtime events, and each Projection
owns its reducer and compatible read selection.

### Recommended next action

Treat P5 as complete for current repository readers and continue to P6's
`/remember` source-selection decision. Reopen P5 if another reader duplicates
one of these exact contracts, a stored migration retires legacy rows, malformed
data policy changes, or a measured replay/query problem justifies a new seam.
Do not infer compatibility for unknown external raw-SQL consumers.

### Verification

This learn turn does not rerun ERT; it reuses the independent totals above.
Valid facts used public Tape/Memory/Workspace/Team/Claims paths and isolated
temporary databases. Tests inserted malformed JSON through the physical SQLite
adapter because the public append interface correctly refuses to manufacture
invalid metadata. No user database, provider, network, schema migration, or
production code changed.

The Workspace standalone unexpected result is accurately retained. It is an
unrelated pre-existing test-load issue: under lexical binding the standalone
file does not declare `superchat-buffer-name` special, so its local `let` is
invisible to the product's `boundp` lookup and the test observes the legacy
fallback; canonical load order declares the variable and passes. It is not a
same-name-buffer failure and was not repaired outside this P5 scope.

Before learn, strict checking reported 1020 references, 7 errors and 0 warnings.
Three Subagent citations moved because of this batch; four Workspace citations
were shared pre-existing one-line drift before the new test at line 451. K24
mechanically re-anchors all seven to the same named constructs. No contract or
`since` value changes.

### References

- `superchat-memory.el:173@superchat-memory--event-meta`; `:188@superchat-memory--projection-select`
- `superchat-workspace.el:983@superchat-workspace--projection-event`; `:1046@superchat-workspace--projected-artifacts`
- `superchat-subagent.el:171@superchat--team-meta`; `:180@superchat--team-event`; `:316@superchat--team-state`
- `superchat-claims.el:33@superchat-claims--typed-event`; `:50@superchat-claims--normalize-basis`
- `test/test-memory-sqlite-facade.el:56@test-memory-projection-stops-checkpoint-at-malformed-meta`; `test/test-workspace.el:451@test-workspace-projection-real-db-keeps-scope-legacy-and-tolerance`; `test/test-subagent.el:122@test-team-projection-real-db-keeps-session-legacy-and-tolerance`; `test/test-claims.el:90@claims/legacy-basis-is-global-but-typed-basis-is-session-exact`
- Independent P5 checker final report

## 2026-09-05 — Ordinary tool-bearing LLM Requests fail closed without a trusted wrapper (E-20260905-011)

id: E-20260905-011
status: confirmed
scope: P3 ordinary non-prepared LLM tool admission and external direct-caller migration
source: `.specs/llm-tool-wrapper-admission/SPEC.md` revision 1; Slice 01 implementation; independent P3 check and required-fix recheck PASS
verification: reused independent runs: focused 5/5; LLM 187 total/182 expected/0 unexpected/5 skipped; Runtime 111/111; canonical 1264 total/1246 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; final leader narrow check 6/6 and `check-parens` PASS

### Observation

An ordinary Request with non-empty, non-prepared tools now signals
`superchat-llm-tool-wrapper-required` before prompt construction, provider
request construction, provider invocation, or a tool effect when neither its
request-local wrapper nor the optional already-loaded Agent Loop producer
yields a callable wrapper.  A fresh child `emacs -Q` loaded the LLM Kernel
path without Agent Loop and observed all three provider-side counters at zero.
A real Runtime/Tape path with the default Closure terminal guard enabled
preserved the original condition while producing one failed delivery, one
durable `run.failed`, quiescence, and no provider or tool effect.

Trusted request-local wrapping still crosses the real Authority seam.  A
prepared internal tool reached the provider with `eq` object and function
identity and no wrapper invocation.  Exact-evidence wrapper validation still
runs first; standard Agent Loop wrapping and tool-free sync/async Requests
remain covered by the accepted full suite.

### Interpretation

P3 is complete for repository-owned paths.  LLM owns this pre-provider
admission check; Authority remains the sole model-invoked tool execution seam
and Runtime retains lifecycle, terminal, delivery, and quiescence ownership.
Prepared tools remain a trusted internal contract, and callable Lisp is not an
authentication mechanism for arbitrary or malicious producers.  The earlier
Authority-to-Agent Loop, Runtime-to-Experience, and LLM-to-Synthesis seams are
bounded no-change findings; `E-20260905-006` and `E-20260905-008` separately
cover tool-quit accounting and headless Runtime/Render compatibility.

### Recommended next action

Proceed with the ready `/remember`–`/recall` provenance Slice.  External direct
callers that attach ordinary tools must migrate to a standard Superchat entry
or the existing trusted callable-wrapper contract; setting `prepared` is not a
supported migration.  Unknown external callers remain unknown, and release or
publication remains unapproved and outside this evidence.

### Verification

This learn turn reuses the independent ERT and canonical results in the record
header; it does not claim to rerun them.  After the unrelated child-fixture
header was removed, the leader's narrow check ran the old prompt-repair case
plus all five P3 targets (`6/6`) and strict checking returned 1020 references,
29 errors, and 0 warnings.  The 29 failures are mechanical K24 citations moved
by the accepted LLM production insertion; the transient eight test shifts are
absent.  K25 re-anchors those citations to the same constructs, adds the
approved admission contract, and passes strict checking at 1025 references,
0 errors, and 0 warnings; the Authority-map form is valid.  No real provider,
network, user database, GUI keyboard interaction,
or external consumer certification was exercised.

### References

- `superchat-llm.el:459@superchat-llm-tool-wrapper-required`; `:1668@superchat--llm-generate-answer`
- `test/test-llm-backend.el:8278@test-fresh-headless-llm-rejects-tools-before-provider-effects`; `:8403@test-prepared-tools-preserve-object-and-function-identity`; `:8439@test-runtime-ordinary-wrapper-rejection-finishes-once-and-quiesces`
- `.specs/llm-tool-wrapper-admission/SPEC.md`; `.specs/llm-tool-wrapper-admission/issues/01-admit-tool-wrapper.md`
- `docs/adr/0005-fail-closed-ordinary-llm-tool-admission.md`; `KERNEL.md` K25
- Independent `arch_fix_checker` P3 recheck and leader narrow scope check

## 2026-09-05 — Explicit remember/recall suppresses only a verified bridge duplicate (E-20260905-012)

id: E-20260905-012
status: confirmed
scope: future explicit `/remember TEXT` bridge provenance and explicit `/recall` Tape-plus-Memory selection
source: `.specs/remember-recall-provenance/SPEC.md` revision 1; Slice 01 implementation; independent final check PASS
verification: final checker target 1/1 and Tape View 75/75 with `check-parens`; reused earlier checker runs Memory 23/23 and canonical 1275 total/1257 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`

### Observation

Future explicit remembers keep the source user Entry, `remember` Anchor, bridge
`memory.captured` event, and materialized Memory row as separate append-only
facts.  When exact identities are available, the bridge carries a typed
Session/Anchor/source relation.  Memory is the sole decoder and verifier: it
checks the Memory/capture correspondence, non-empty and equal event/relation/
recall/Anchor/source Sessions, exact Anchor storage identity and kind/topic,
the exact earlier source in `source_ids`, and source-before-Anchor-before-
capture ordering.

At explicit `/recall` only, the existing Tape and Memory queries and their
limits run first.  The merge suppresses a verified original bridge Memory only
when that exact Anchor occurs in the selected Tape rows; it performs no refill.
The selected Anchor and raw source remain, as do ordinary or historical
Memory, equal text, overlapping numeric IDs, corrected facts, cross-Session or
otherwise unverified candidates.  The final no-refill fixture observes this
filter through the real query and verifier rather than a constant stub.

### Interpretation

This is the approved compatible P6 selection revision, not deletion or
deduplication of history.  Tape remains append-only, Memory owns its Projection
and provenance validation, and `superchat.el` owns only explicit command
orchestration and the existing merge seam.  Continuity still emits `raw-fact`
with `:represented []`; no representation, recursion, schema, registry,
cross-write transaction, or historical backfill was added.

Nil Anchor return permits the old unlinked bridge without fabricating a
relation; Anchor `error`/`quit` follows the original stop/propagation path;
Memory failure does not roll back earlier Tape appends.  Corrections remain new
unlinked facts.  These facts do not establish private external payload
compatibility, real service/GUI/C-g behavior, or historical relationship data.

### Recommended next action

Run the final P1–P8 architecture-document synchronization, independent check,
and whole-goal completion audit.  Do not mark the architecture goal complete
until that audit closes all current-state entries and retained limitations.

### Verification

This learn turn does not rerun ERT.  It uses the final independent check's
target 1/1 and Tape View 75/75 after the no-refill fixture correction.  Memory
23/23 and canonical 1275/1257/0/18 were run by the checker before that final
test-only fixture addition and are reused with that timing boundary stated;
the final fixture changed no production behavior.  The checker also verified
the isolated `error`/`quit` probe and Authority-map form.  Before learn, strict
checking reported 1025 references, 43 errors, and 0 warnings: 42 citations are
mechanical movement in `superchat.el`, while one is the moved Tape View test.
K26 re-anchors each citation to its same named construct, adds only the approved
P6 contract, and passes strict checking at 1031 references, 0 errors, and 0
warnings; the Authority-map form is valid.

### References

- `superchat-memory.el:309@superchat-memory-capture-explicit`; `:490@superchat-memory-verified-remember-anchor-id`
- `superchat.el:3537@superchat--cmd-recall`; `:3573@superchat--cmd-remember`
- `test/test-tape-view.el:1659@remember-recall/provenance-removes-only-selected-bridge-memory`; `:1719@remember-recall/query-limit-filters-without-refill`
- `.specs/remember-recall-provenance/SPEC.md`; `.specs/remember-recall-provenance/issues/01-remember-recall-provenance.md`
- Independent P6 final checker report

## 2026-09-05 — Architecture review P1–P8 is closed for repository scope (E-20260905-013)

id: E-20260905-013
status: confirmed
scope: final documentation calibration and evidence accounting for the 2026-09-05 P1–P8 architecture-review goal
source: three-round architecture review; accepted implementation and bounded no-change evidence; independent `docs_final_check` PASS
verification: leader canonical 1275 total/1257 expected/0 unexpected/18 skipped with `RUNNER_GATE_OK`; K26 strict 1031/0/0; Authority-map form valid

### Observation

The five architecture documents preserve the three review rounds and now use
one current P1–P8 map: P1 `E-20260905-002`/`E-20260905-007`; P2
`E-20260905-002`; P3 `E-20260905-006`/`E-20260905-008`/
`E-20260905-011` plus ADR 0005; P4 `E-20260905-009`; P5
`E-20260905-010`; P6 `E-20260905-012`; P7 `E-20260905-004`; and P8
`E-20260905-005` plus the final documentation calibration.  The two last
boundary corrections in `02-problems.md` agree with the Workflow-edit limits
in `E-20260905-003` and the P5 reader limits in `E-20260905-010`.  This final
documentation batch changed no production code or tests.

### Interpretation

All repository work required by this architecture goal is either implemented
and independently accepted or closed as an evidence-backed bounded no-change.
The latter is a result under the inspected owners, callers, and triggers, not
deferred implementation presented as completion.  This does not certify
unknown external/private/advice callers or make any unrelated confirmed SPEC
formally terminal; the upstream SPEC-terminal authority gap remains separate.
K26 already contains the enacted product contracts, so this docs-only closure
adds no Kernel rule and does not justify a version bump.

### Recommended next action

The leader may complete the architecture-review goal after final accounting;
there is no remaining do/check action for P1–P8.  Reopen a bounded no-change
only when its recorded trigger occurs or a new product/compatibility decision
is approved.  Continue tracking unrelated administrative and historical work
independently.

### Verification

This learn turn does not rerun ERT.  The leader's fresh canonical run at
2026-09-05 12:45:52-0700, after the final P6 no-refill fixture, reports 1275
total, 1257 expected, 0 unexpected, and 18 skipped with `RUNNER_GATE_OK`;
`test/run-tests.el` had SHA-256
`2c4d65a8080d2d5a014d9e23d9074a9cbff7bf9594cc469cfe8ef66c9b85c6da`.
The 18 skips are not passes: they include two real-filesystem NFC capability
skips.  P5 also retains the separately documented historical Workspace
standalone result of 30 total/29 expected/1 unexpected even though canonical
passes.  Real GUI interaction, keyboard C-g, providers, and unknown
external/private/advice consumers were not tested.

The independent checker passed links, current symbols, history anchors,
whitespace, all P1–P8 evidence mappings, and 10 Emacs Lisp fences (six in the
guide, three in `02-problems.md`, one in `03-refactoring.md`).  After the two
last boundary edits, the leader reran links and whitespace; the fence result
is reused across that text-only correction.  The leader also ran K26 strict at
1031 references/0 errors/0 warnings and the Authority-map form successfully.
This learn turn reruns the strict, Authority, backlink, and whitespace gates.

### References

- `docs/architecture.md`
- `docs/architecture-review-20260905/01-evolution.md`
- `docs/architecture-review-20260905/02-problems.md`
- `docs/architecture-review-20260905/03-refactoring.md`
- `docs/architecture-review-20260905/04-target-architecture.md`
- `docs/adr/0005-fail-closed-ordinary-llm-tool-admission.md`; `KERNEL.md` K26
- Independent `docs_final_check` report and leader final verification

## 2026-09-06 — Framework aggregate01 exact View rollover and output contracts (E-20260906-001)

id: E-20260906-001
status: confirmed
scope: framework-regression-repairs Slice01, approved revisions 1–6; clear, explicit transcript addressing, final rendering, View prompt and tool-context presentation
applies_when: the shipped Session View and Runtime/LLM/Workflow/Agent Loop presentation paths execute these approved actions
source: SPEC rev6 and Slice01; independent Claude `reasoning-check-and-01-audit.md`, Part 2 COMPLETE; preceding P1/P2/prompt/O3/Context and F7 independent checks
verification: independent final canonical 1339/1305/0/34 exit0; real reasoning/P1/P2/Context21/21 exit0; focused327/325/0/2 exit0; EW/compact/subagent121/121 exit0; full realLLM199/196/3/0 exit1, known02 failures retained

### Observation

The independent aggregate audit maps all original AC1 and rev1–6 requirements to
persistent tests on the final tree. Exact clear preserves object/name/configuration
and creates a fresh Session through EditorWorkspace, with consent before effects,
real cancellation, post-callback revalidation and protected UI/ownership publication.
Refusal changes no View/cache/Tape/state; error/quit tests preserve publication and
View-reset consistency without pretending that cancellation can be undone. Old
protocol hosts remain discoverable and publicly closable, with their tab entry.

P1 supplies a final segment without a preceding partial or status. P2 supplies
explicit opaque owner operations and deletes call-with-owner after caller migration;
custom callbacks retain isolated dynamic compatibility. Prompt O1/O2 preserves
live drafts and one shared prompt, while consumed nil-marker pending state remains
adoptable. A non-face header property survives real Org fontification and does not
become owner authority. O3 computes meaningful final inputs before Render mutation;
formatter error/quit preserves that operation's prior View/state/projection.
Runtime retains its separate durable terminal, warning and once-delivery behavior;
healthy explicit rebuild is possible, not automatic repair. O4 is verified
no-change for borrowed/unbounded regions and independent-turn boundary placement.

Context distinguishes nil legacy from a claimed Runtime identity, captures public
Session/Run/opaque owner eligibility once, and withholds both presentation sites
for an invalid claim while warning once outside the transcript. Diagnostic
error/quit/reentry containment leaves Authority/effects/audit/count/return/callback
semantics unchanged. Public Runtime status publishes the default Run ID when no
custom owner is supplied; consumers pass that value opaquely, not as a substitute.
The real approval binder and native wrapper chain passed. F7's preflight test now
uses real published identity; its scoped negative control fails specifically at
the rendered assertion after setup, expected preflight error and zero warning pass.

The fresh cross-phase probe combines live draft, custom-owner Run A, reasoning,
eligible Authority tool, final, toggle, exact clear, late old tool and fresh Run B
on the same View. Both draft and old-Tape exclusion checks pass; final/tool/prompt
are visible. Clear preserves old rows. The late refusal appends four old-Run audit
rows but changes no new View or prompt; this remains Slice05's pending migration,
not proof of zero old audit or a failed append-only invariant.

### Interpretation

Slice01 is complete and eligible to be marked done with this Evidence. The bounded
batch5 reasoning correction is separate, recorded as E-20260906-002. Required
prompt fontification, Context preflight masking, P1 mechanism wording and unresolved
claimed-context findings have verified resolutions. O4 retains its existing
bounded/unbounded contract, not a deferred repair. Timing values remain View-local;
nil legacy does not assert Run isolation; custom/private callback compatibility is
not silently broadened. No lifecycle, Authority, Tape schema or replay migration is
promoted by01. Report F5 bad-Tape fail-closed and F6 raw-plus-Anchor retention remain
approved no-change. Full goal and formal SPEC closure are not asserted.

K27 promotion supersedes K26's implicit default-callback owner addressing,
selection-dependent rendering and stale prompt-adoption/final-opening mechanisms;
it adds the approved dedicated exact Session View rollover, preserving existing
identity and lifecycle invariants. CONTEXT and the Session close Protocol were read
and remain compatible: clear rollover is distinct from whole-workspace close, so
neither requires edits or a new document.

### Recommended next action

Arrange existing02 after01 check+learn; then preserve02→03→04→05 serial checks and
learns. Keep the three realLLM fixture failures in02, Org Babel in03, exact seven
Subagent/Team observations in04, and terminal-first zero-audit admission in05.
Do not prematurely replace the old inactive-tool four-event Kernel contract.

### Verification

This knowledge-only learn reuses the independent final-tree ERT runs, not new ERT.
The final check ran Emacs31.0.91 with existing isolated llm0.31.1, real SQLite/Tape/
Authority/Closure and transport mocks. Its `rvcheck/gates.sh`, `exits.txt` and logs
record the commands and process exits. Canonical's34 skips are not passes; real
dependency acceptance ran non-skipped. Two focused skips are NFC filesystem cases.
`RUNNER_GATE_OK` is separate from ERT. The full realLLM selector excludes the socket
test and fails exactly test-runtime-overlapping-tool-round-controls-remain-run-local,
test-tools-list-can-expose-all-builtins and test-tools-list-populates-default-small-tool-surface.
No provider, liveGUI or unknown external-callback certification follows.

Historical red provenance is retained in Slice01 and the worker reports:
framework-clear-do rev1–3 real cancel/rollover/late-owner traces; P1 final-without-
partial; prompt fontification correction; O3 formatter error/quit; Context invalid-
claim and preflight negative control. Early malformed fixtures are not product-red
evidence. Their later green gates and independent phase checks precede this final
aggregate check; this entry does not recast investigation stubs as real acceptance.

Where counterexamples were hunted for K27: pairs-before.md was generated before
edits; paired Session/Run/Tool/Context View, lifecycle, submit/seal, close/acquisition
and Render entries were read. EditorWorkspace validate-rollover/rollover and UI
clear-view-transaction were read through consent, cancel, reentry, failure and old-
host/tab branches; ordinary unowned borrowed input remains outside rollover.
Render state-for-owner/begin/publish/prepare/close/ensure-view-prompt were read for
absent-state, live/stale/nil prompt, non-current owner, active nil-close, unbounded
and formatter-failure counterexamples. Runtime default and custom delivery/stream,
published owner and LLM lexical round-end capture were compared. Agent wrapper's
public-status capture and six call/result sites were checked against nil legacy,
invalid claims, diagnostic failures and consumer quit. The independent fresh
Claude aggregate audit and its cross-phase probe are the pre-learn review;
its suggested O3 wording is narrowed here to Render operation preservation, with
Runtime delivery failure handling separate. Mechanical citations are re-anchored
to the same actual constructs, not by global numeric offsets.

### References

- `.specs/framework-regression-repairs/SPEC.md` revision6; `issues/01-clear.md`
- `/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/rvcheck/reasoning-check-and-01-audit.md`
- Same scratchpad: `rvcheck/{canonical,real-focused,regressions-real,readonly-real,llm-real-full,kernel-strict}.log`, `rvcheck/gates.sh`, `rvcheck/exits.txt`; `agg/probe-aggregate.el` and `.log`
- `/private/tmp/framework-output-context/WORKER_REPORT.md`; `/private/tmp/framework-context-preflight/WORKER_REPORT.md`; historical phase reports and independent checks cited by Slice01
- `/private/tmp/framework01-learn/pairs-before.md`; K27 construct audit and final gate logs in the same directory

## 2026-09-06 — Batch5 reasoning folds hide the body, not subsequent output (E-20260906-002)

id: E-20260906-002
status: confirmed
scope: existing batch5 Thinking overlay correction, independent of framework Slice01
applies_when: reasoning is inserted, streamed, finalized or toggled in an Org Session View
source: approved reasoning-visibility-plan.md; independent Claude reasoning-check-and-01-audit.md Part1 PASS
verification: final real reasoning/P1/P2/Context21/21; canonical1339/1305/0/34; true invisible-p checks after verified Org fontification
supersedes: no body-only semantic invariant; corrects its implementation and incomplete historical visibility evidence

### Observation

The product delta is one make-overlay rear-advance argument, t→nil, in
superchat--response-region--insert-reasoning-block. The body-end marker is unchanged;
no secondary positioning authority was introduced. Actual visibility red tests
failed at Assistant content (direct4/1/3/0, real Runtime1/0/1/0). After repair,
reasoning body folds independently across nil-close/tool rounds and multiple
owners; Thinking/Assistant headings, answers, tool records, B and prompt remain
visible. Empty reasoning creates no block. Native normalization/custom-owner tests
use real Runtime/SQLite/Tape/Closure guard with only model transport mocked.

The truncation notice is prepended INSIDE the bounded reasoning body and is hidden
while folded, visible when expanded. This is established by bound-reasoning and
insert-reasoning-block, the bounded-tail test's `(… "Thinking truncated" t)`
invisible-p assertion, and the independent Runtime probe's `truncnotice=HIDDEN`.
The check report's prose saying the notice is visible is inconsistent with its own
log and current test; that wording is not promoted. The direct old probe log starts
with a user-configured skills path and reader warning, so it is not presented as
proof of isolated environment use; persistent isolated ERT and the Runtime probe
carry the acceptance. This learn ran no such probe or user-state operation.

### Interpretation

K27 clarifies the existing body-only invariant, including bounded-tail notice
placement and non-advancing overlay end. It does not create a new identity, owner,
folding policy or general reasoning/provider guarantee. Batch5's prior overlay-get
and buffer-string checks could not prove answer visibility; actual invisible-p
with real Org fontification now supplies that evidence. No attribution to a VCS
release is inferred from the dirty feature's history.

### Recommended next action

Retain these direct and native regressions. No additional reasoning implementation
is required by the independently checked bounded task. Continue the distinct
framework feature through arrange02, not a sixth Slice or reasoning-in02 repair.

### Verification

Reuses the same independently fresh final-tree gates listed in E-20260906-001.
The real new reasoning fixture passes non-skipped; canonical skips it without llm.
Known fullLLM failures, socket exclusion, NFC skips and no provider/liveGUI limits
remain. Worker red.log parse error is not product red; red-final.log and real-red.log
are valid red. The independent review is the fresh-context pre-learn review; the
counterexample search checked streaming replace-reasoning-body, truncation prefix,
insert overlay boundaries, nil-close/retained blocks and per-overlay toggle, plus
the paired Render and reset invariants in pairs-before.md.

### References

- `/private/tmp/framework-reasoning-visibility/WORKER_REPORT.md` and its red/green logs
- Independent scratchpad `rvplan/reasoning-visibility-plan.md`; `rvcheck/reasoning-check-and-01-audit.md`, `probe-rv-runtime.log`, `real-focused.log`, `canonical.log`
- `superchat-render.el`, insert-reasoning-block/bound-reasoning/replace-reasoning-body/toggle-reasoning; `test/test-llm-backend.el`, folded-final/bounded-tail/multiround/native reasoning visibility tests
- `/private/tmp/framework01-learn/pairs-before.md`; K27 final gate logs

K27 learn verification (applies to E-20260906-001 and E-20260906-002): draft
convert inserted0 with1072 refs/errors0/warnings0; final repository strict also
reports1072/0/0 exit0, and Authority-map form is valid exit0. Pairs-after reports
87 entries/559 shared groups/214 universal sentences. The construct audit records
413 relocation/short-reference-expansion operations (246 unique old/new/construct
tuples), including newly added acceptance citations; existing named constructs
were retained, special struct/registration/autoload forms and intentional bare
header/comment references were inspected and independently checked by the Lisp
reader. The original234 errors were mechanical drift/blank citations, not a reason
to rewrite their unrelated contracts. All four changed Markdown files have no
trailing whitespace. Evidence IDs are unique and Slice01 links E-20260906-001
after verified append. Commands and full records are under
`/private/tmp/framework01-learn/{convert-final,strict-final,authority-final,pairs-after}.log`
and `citation-audit.json`; no ERT was rerun in learn.

## 2026-09-06 — Slice02 failure cleanup and faithful mock prompt accepted (E-20260906-003)

id: E-20260906-003
status: confirmed
scope: framework-regression-repairs Slice02, ten named test forms and existing LLM mock fixture
applies_when: evaluating those fixtures under assertion failure or the installed llm0.31.1 append-response contract
source: SPEC rev6 AC2; independent Claude framework02-check.md PASS, no required finding or blocker; user-authorized learn
verification: independent 09:59 batch gates and physical-resource probes below; learn-only citation/backlink gates recorded after execution
supersedes: no enacted product semantics; resolves the historical overlapping-controls and two tools-list failures attributed to Slice02

### Observation

The independent checker accepted all four AC2 criteria. Nine cleanup tails moved
from protected progn to unwind cleanup; core-pipeline's delete-file moved from
outside the entire unwind into cleanup. Reader checks preserve all original
assertion trees and other existing top-level forms, except the authorized mock
fixture. Test-local additions release accepted p4 Runs before the existing Tape
mock unwinds, release acquired deadline timers, retain borrowed shadow-test hosts,
and avoid summary work during native timer-test cleanup. No product, runner or
shared test-helper changes were made.

The mock retains original text/tools and prior interactions and appends exact
content with default/nil user or the supplied role, preserving order and returning
the updated list. Independent differential testing compares both mock plist and
nested native prompts with the real llm0.31.1 append-response implementation;
role/content sequences match for default/nil roles, assistant/user, Unicode and
newlines. The actual dispatcher finalize/followup test verifies appended
instructions remain provider-visible after earlier narration. The tool surface
assertions remain unchanged and pass after forced p4 failure in the same process,
before rescue; contamination was not authority to change tool counts.

The checker reran the worker's32 acquisition checkpoints in each ordinary/real
environment with PROBE FAILURES=0, preserving original conditions, function
bindings and borrowed-host text/properties. Actual buffers, timers, paths,
connections, registry entries and accepted Run resources are observed before any
harness rescue, rather than inferred from restored dynamic variables.

The separate advice-based checker probe has19 attempted checkpoints per
environment, not19 unconditional passes. Its raw logs contain15 clean cases,
2 S1 cases,1 O1 case, and1 untriggered injection;18 injections fired, preserving
their original conditions. C02 PROBE FAILURES=4 counts the three flagged cases
plus the untriggered case. This qualifies the report's shorthand that19 fired;
it does not replace the accepted AC2 verdict or the32-point passing matrix.

S1, a bounded suggestion: in runtime/default-terminal-delivery-resets-response-region,
cancelled/timed-out rows interrupted after the handle but before terminal dispatch
leave one nonterminal Run in their isolated, discarded table. No physical resource
leak was observed: deadline0 arms no timer, the exact buffer is dead, captured DB
connection closed and temporary directory absent. The arranged oracle for those
rows is met. Uniform Run cancellation is a possible later test cleanup, not an
implemented repair or a reason to silently extend Slice03 scope.

O1 remains an unresolved production observation requiring separate plan.
superchat-runtime--arm-deadline allocates through run-at-time before publishing the
timer in the entry via superchat-runtime--update. The checker's injected signal
at that allocation/publication seam leaves a live deadline timer unreferenced by
the now-failed Run. This is stronger than a restored-variable observation but
does not establish a naturally reachable, universal production defect: the
signal was injected inside submission, outside the tested protected-body
checkpoints. No ordinary trigger, reachability proof or repair is claimed here.
The untriggered failed-delivery injection is not passing evidence for that point.

### Interpretation

This is a verified test-fixture correction, not a new concept, identity,
lifecycle, Authority rule or Runtime contract. Product Authority placement remains
unchanged. No fixture lesson is promoted to a universal cleanup guarantee; S1/O1
stay scoped observations rather than enacted Kernel rules. Kernel revision K28
is authorized only to mechanically reanchor K27 citations and retain all enacted
semantics and every since field, following the existing file-version convention.
The source-snapshot hash in leader-owned docs/architecture-map remains a historical
snapshot; it is not rewritten by learn.

### Recommended next action

After learn gates pass, mark02 done with this verified Evidence backlink and hand
off to arrange existing03. Preserve the five serial Slices and03→04→05 contracts;
no03 arrangement or implementation is performed by this learn. Investigate O1
through a separate plan with concrete reachability and a minimal failure/release
oracle before any production repair. Keep S1 as a bounded optional cleanup
suggestion, without automatically assigning it to03 or weakening AC2. Do not
declare the active feature or the overall user goal complete.

### Verification

All ERT/probe results in this table are REUSED from the independent Claude check
on2026-09-06 around09:59–10:03, not rerun by this knowledge-only learn:

| Independent gate | total / passed / unexpected / skipped | exit |
| --- | --- | --- |
| canonical |1341/1307/0/34, plus separate RUNNER_GATE_OK|0|
| full-real LLM file, SOCKET EXCLUDED |201/201/0/0|0|
| seven direct files ordinary |515/494/0/21|0|
| seven direct files real |515/513/0/2|0|
| named ten ordinary and real |10/10/0/0 each|0|
| direct content ordinary and real |2/2/0/0 each|0|
| worker32-point physical probe, independently rerun in each environment |PROBE FAILURES=0; same-process tools-list consumers pass|0|
| checker19-attempt expanded probe in each environment |15 clean;2 S1;1 O1;1 injection not fired; raw counter4|not represented as an all-green ERT gate|

Environment is Emacs31.0.91, SQLite and the already installed isolated
llm0.31.1 .elc. The named test-ollama-utf8-split-chunk-integrity is explicitly
excluded from real/direct selectors, not passed or counted as a real skip;
canonical skips it without optional llm. The two real direct skips are the NFC
ambiguity filesystem cases; ordinary adds19 optional-llm skips. No provider,
socket, network or live-GUI acceptance is inferred. Historical worker red logs
(31 resource checkpoints plus4 tool-list failures, and old-fixture content red)
are attributed to do; the checker did not rerun those pre-repair sources.

Before learn, independent strict found1072 references,86 expected-construct line
drifts (llm38/runtime37/native11),0 warnings; authority-map form passed. These
mechanical diagnostics are not new semantic failures. Fresh learn-only strict,
authority, semantic-preservation, append/backlink and scope checks follow below
once executed; no ERT, production probe or socket rerun belongs to this learn.

### References

- `.specs/framework-regression-repairs/SPEC.md`, AC2; `issues/02-test-hygiene.md`
- `/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/c02/framework02-check.md` (actual single-c02 path)
- Same c02 directory: `gates.sh`, `progress.txt`, `{canonical,full-real,direct-ordinary,direct-real,named-ordinary,named-real,content-ordinary,content-real,inject-ordinary,inject-real,static,strict,authority}.log`
- Same c02 directory: `probe-c02.el`, `probe-c02-ordinary.log`, `probe-c02-real.log` (expanded cases and native/mock differential); `probe-timer.el` read only, not rerun by learn
- `/private/tmp/framework02-do/WORKER_REPORT.md`; `inject-red-final.log`, `content-red-final.log`, `static.log`, original form copies and diff artifacts
- `/private/tmp/framework02-learn/`: mechanical citation audit and fresh learn verification logs

E-20260906-003 learn verification: Evidence was appended and verified unique before
backlinking02. Final repository K28 strict reports1072 references/errors0/warnings0,
exit0; authority-map form valid, exit0. The86 relocations were selected by the
checker and resolved with the Lisp reader (llm38/runtime37/native11); no construct
identity changed. Normalizing only numeric citation positions makes the entire
enacted body byte-equal to K27, and every since field is identical. The file version
and source provenance follow the existing K28 convention. The draft helper's first
attempt lacked the Emacs Lisp syntax table and failed without a repository write;
the corrected reader pass and draft strict both passed before applying the patch.

Backlink/scope/whitespace verification passes: exactly EVIDENCE.md, KERNEL.md,
STATUS.md and issues/02-test-hygiene.md changed from the learn baseline; all other
captured files, including leader-owned docs/architecture-map, remain unchanged.
Existing Evidence is an unchanged prefix, IDs unique,02 done with E-20260906-003,
next action arrange03. All four changed Markdown files have no trailing whitespace.
No tests, production probes, sockets or ERT were rerun in learn. Commands, reused
check attribution, numeric relocation audit and fresh gate logs are in
`/private/tmp/framework02-learn/WORKER_REPORT.md`, `citation-audit.log`,
`strict-final.log`, `authority-final.log`, `semantic-preservation.log`,
`evidence-append.log` and `backlinks-scope-whitespace.log`.

## 2026-09-06 — Slice03 real Skill context in Org Babel accepted (E-20260906-004)

id: E-20260906-004
status: confirmed
scope: framework-regression-repairs Slice03, Babel query composition and direct adapter tests
applies_when: a Babel skill parameter names an available Skill file by basename, or is absent/missing
source: SPEC rev6 AC3; independent Claude framework03-check.md PASS; leader accepts mechanical-only learn
verification: independently fresh c03 gates and31-check probe, attributed below; fresh learn reference/backlink gates recorded after execution
supersedes: no enacted semantic rule; corrects the adapter's parameter/representation mismatch

### Observation

The independent check accepts AC3 with no required finding or blocker. The product
delta is build-query's symbol/string normalization and reading the real
superchat-preset-skill-body, plus two declarations. Executor/call-llm and all
previous ecosystem test forms remain reader-equal. Real temporary files, loader
and Preset struct establish basename lookup, distinct display-name metadata,
exact multiline/Unicode/whitespace content and the existing two-newline separator.
Absent/missing Skill paths remain silent. Actual Org expansion, execution and
RESULTS insertion preserve block options and result handling.

Only the blocking Runtime boundary is captured in those adapter tests. This proves
request construction and result integration, not actual Run/Tape lifecycle or
provider execution. Public EditorWorkspace identity remains real: ordinary and
inherited inputs stay borrowed/unowned, owned input uses its exact owner Session,
malformed markers reject without being swallowed into a model-error string.
Owner mismatch is structurally unreachable through the real adapter query; the
public EW mismatch negative test supplies the actual seam regression. A deliberate
assertion verifies the local fixture releases its actual files/directory/buffer.

The checker reports31 own checks with zero failures in ordinary and real llm
environments. Its preserved probe-c03.log contains31 OK checks and
PROBE_FAILURES=0; the both-environment attribution comes from the independent
report. Cases include .org plain files, quoted string header arguments, empty
body, loader error/unavailable fallback, non-string body compatibility, actual
Org RESULTS and exact owned/malformed buffer cases. No probe is rerun by learn.

### Interpretation

No concept, identity, lifecycle, Authority, Preset clause or new missing/empty-body
policy is promoted. K29 is only a file-version/provenance revision and the single
numeric citation relocation ob-superchat.el:81→84 for the unchanged call-llm form;
all enacted prose and every since field remain unchanged from K28.

S2 is a bounded no-change observation for03: an existing Skill with empty string
body contributes the literal two-newline separator before the query. This follows
the approved existing concatenation contract. Leader chose no change; no
string-empty-p guard or followup obligation is introduced. The optional Preset
clause suggested by the checker is not adopted. S1/O1 remain the separate
observations in E-20260906-003; neither is erased, repaired, or assigned to04.

### Recommended next action

After successful learn gates,03 is done with this verified Evidence backlink and
the next authorized handoff is arrange existing04. This learn performs neither04
arrange nor do. Preserve the five serial Slices, pending04/05 contracts and the
separate S1 suggestion/O1 reachability-plan frontier. No overall feature/goal or
formal SPEC terminal transition is claimed.

### Verification

REUSED from the independent c03 check on2026-09-06 at10:26–10:27, not fresh ERT
in this knowledge-only learn:

| Gate | total/passed/unexpected/skipped | exit |
| --- | --- | --- |
| focused ordinary and real llm |11/11/0/0 each|0|
| full ecosystem ordinary and real llm |22/22/0/0 each|0|
| public EW validation |5/5/0/0|0|
| Runtime layering/blocking selector |3/3/0/0|0|
| canonical |1347/1313/0/34 plus separate RUNNER_GATE_OK|0|
| own checker probe |31 checks/0 failures each environment, per independent report|not a replacement for ERT counts|

Environment: isolated Emacs31.0.91, existing llm0.31.1/SQLite. Canonical retains34
optional-dependency/NFC skips, including the UTF-8 socket test; socket is not
exercised. No provider, GUI or complete realLLM-suite acceptance is inferred.
Worker red-final.log11/7/4/0 is historical pre-repair evidence, not rerun by the
checker or learn; the earlier test-source parse error is not product red.
Independent K28 strict's sole expected-construct drift is the unchanged call-llm
form at84, and authority-map form passes. Fresh learn-only gates follow below.

### References

- `.specs/framework-regression-repairs/SPEC.md`, AC3; `issues/03-org-babel.md`
- `/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/c03/framework03-check.md`
- Same c03 directory: `gates.sh`, `progress.txt`, `{focused-ordinary,focused-real,ecosystem-ordinary,ecosystem-real,ew,runtime,canonical,static,strict,authority}.log`, `probe-c03.el`, `probe-c03.log`
- `/private/tmp/framework03-do/WORKER_REPORT.md`, `red-final.log`, `static.log` and scoped baseline/diff artifacts
- `/private/tmp/framework03-learn/`: append verification, strict/authority and scope/semantic checks

### Executed learn verification

Fresh learn strict:1072 references/errors0/warnings0, exit0; authority-map form
valid, exit0. Append-only/unique-ID verification preceded the03 done backlink.
Exact Kernel delta is version/provenance and call-llm81→84 only; all enacted text
and since fields are preserved. Baseline scope verification found exactly Evidence,
Kernel,03 and STATUS changed, with whitespace clean and architecture-map unchanged.
Logs: `strict-final.log`, `authority-final.log`, `evidence-append.log`,
`semantic-preservation.log`, `backlinks-scope-whitespace.log` in the learn directory.
ERT was not rerun for knowledge-only edits; counts above retain independent-check
attribution. Next authorized workflow step is arrange04, not performed here.

## 2026-09-06 — Slice04 finite Team/Subagent observations accepted (E-20260906-005)

id: E-20260906-005
status: confirmed
scope: framework-regression-repairs Slice04, SPEC rev6 AC4
applies_when: Closure interprets the seven approved Team/Subagent observation types
source: user-approved AC4; independent c04 PASS accepted by leader
supersedes: no identity, obligation, terminal or fallback contract

### Observation

The sole production change adds seven literal known-irrelevant types in Closure:
team.task.created, team.task.updated, team.message.sent, team.decision.published,
subagent.started, subagent.completed, subagent.failed. Witness ids are recorded
without creating/discharging obligations or proving a terminal. Unknown siblings,
schema/envelope/causal validation and illegal lifecycle rejection remain intact.
All prior direct test forms remain reader-equal; Subagent production is unchanged.

Independent tests drive actual native llm async tool scheduling, Authority, Team
producers, child Runtime/SQLite/Tape and enabled Closure guard, then report callback
and parent followup/final. Only model transport callbacks are replaced. Child
success is completed/deliver and failure is failed/reject; exact parent/child
lineage, explicit Team placement, no foreign Tape pollution, once delivery and
accepted host retention/public close are verified.

F1 bounds the Session claim: a parent View carries its buffer-local Session, and
callback-in-view restores that exact request buffer for tool invocation. The
checker also submitted a borrowed non-View buffer without a local Session: Team
id fell back to child id, while parent-run-id stayed exact and foreign Tape stayed
empty. Neither case establishes universal parent Session retention. No wrong
fallback was demonstrated; leaving Subagent unchanged is the accepted conditional
AC4 outcome. Do not promote a parent tool-context Session precedence step.

### Interpretation

K30 promotes only AC4's finite observation extension into the existing Closure
entry, with actual source/test anchors, and mechanically reanchors18 existing
citations. Existing since fields, identity entry, obligations, lifecycle and other
semantics remain unchanged. F1 is an evidence boundary, not a new fallback policy.
S1/O1 remain separate unresolved E-20260906-003 observations; S2 remains approved
literal empty-Skill separator concatenation/no-change under E-20260906-004.

### Recommended next action

After successful learn/backlink, arrange existing05. No05 arrangement or do occurs
here; its late-first admission/audit migration remains pending. No full feature or
goal closure is claimed, and S1/O1 are not automatically added to05.

### Verification

Reused independent fresh c04 results, not new ERT runs by learn:

| Gate | Actual result |
| --- | --- |
| Closure/Subagent real llm0.31.1 | 81/81/0/0, exit0 |
| Closure/Subagent ordinary | 81/76/0/5, exit0 |
| Canonical | 1353/1316/0/37, exit0 plus separate RUNNER_GATE_OK |
| Checker own real probe | 20 checks/0 failures |
| Saved pre-change Closure, seven matrix | 3/0/3, exit1 |
| Saved pre-change Closure, real no-parent child | 2/0/2, exit1; success/failure audit-incomplete/manual-review |
| Prior-form reader | Closure39 unchanged, Subagent64 unchanged, exit0 |

Ordinary skips are three real-llm and two depth-guard tests; the real run supplies
their non-skipped results. Canonical retains optional dependency/NFC/socket skips.
Batch Emacs31.0.91, actual temporary SQLite and isolated llm0.31.1; no provider,
socket, GUI or user-data acceptance claim. Saved-baseline replay loads an old
Closure only in isolated processes, never restores a repository file.

Worker cleanup.log is historical and was code-reviewed, NOT rerun by c04: three
post-delivery assertion injections retained the original failure and released
actual1/1/2 buffers, DB handles, paths/directories, with1/1/2 terminal Run tables.
Timeouts were disabled and timer allocation was zero; no positive timer-cleanup
proof is claimed. Worker's parent-red.log is historically green, not fallback red.
Independent strict K29 had18 mechanical drifts and authority form passed. Fresh
learn-only strict/authority/backlink/scope results are recorded after execution.

### References

- `.specs/framework-regression-repairs/SPEC.md`, AC4; `issues/04-subagent.md`
- `/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/c04/framework04-check.md`
- Same c04 directory: `gates.sh`, `direct-real.log`, `direct-ordinary.log`, `canonical.log`, `probe-c04.el`, `probe-c04.log`, `baseline-red-seven.log`, `baseline-red-child.log`, `reader.log`, `strict.log`, `authority.log`
- `/private/tmp/framework04-do/WORKER_REPORT.md`, `cleanup-probe.el`, `cleanup.log`, `parent-red.log`, `drift-audit.log`
- `/private/tmp/framework04-learn/`: append verification and fresh knowledge gates

### Executed learn verification

Fresh strict1077/errors0/warnings0, exit0; authority form valid, exit0. Five new
source/test anchors support the finite contract;18 old anchors were mechanically
relocated after unique-construct audit. The exact Kernel delta is only that scoped
AC4 addition, those18 anchors and file version/provenance; all existing since and
other semantic text, including identity, are preserved. Unique append-only005 was
verified before04 done backlink. Scope hashes confirm only Evidence/Kernel/04/STATUS
changed and all other baseline files, including architecture-map, remain unchanged;
whitespace passes. No ERT rerun for knowledge-only changes. Fresh logs are
`strict-final.log`, `authority-final.log`, `append-verified.log`,
`semantic-scope.log`, `backlink-whitespace.log` in the learn directory.

## 2026-09-06 — Slice05 non-admission and scoped consumer delivery accepted (E-20260906-006)

id: E-20260906-006
status: confirmed
scope: framework-regression-repairs Slice05, SPEC rev6 AC5 and approved callback correction
applies_when: Runtime-bound calls reach either pre-audit liveness admission refusal
source: approved AC5 migration and callback plan; independent c05 and c05r
supersedes: terminal-first four-event oracle and new gate2 consumer-error redelivery; earlier c05 PASS withdrawn only on once clause

### Observation

Authority now refuses valid terminal-first arrivals at the earliest post-structural
gate and rechecks before first audit. No new Run execution audit or implementation
effect is produced by those refusals. Native sync/async tool-bearing Runtime tests
use actual SQLite/Tape, enabled Closure guard, real tool registration/Authority and
model-supplied wrappers, with transport schedule mocked. Cancellation/completion/
failure/timed-out terminal traces stay unchanged and valid/proved. Real registered
preaudit callbacks cancel before audit; snapshots are taken after cancellation
rows. Already admitted tool.requested/approval.requested cancellation remains
audit-incomplete, retains rows and is blocked from effect by the preexecution gate.

Independent c05 initially passed but then withdrew only its once clause: gate2
consumer error was caught as audit failure and delivered again. The approved local
refusal-delivery-started correction was checked independently in c05r: the two
admission refusals deliver once and propagate original consumer error/quit data.
The new fault probe uses captured real Runtime context at Authority directly; it
is not a native scheduler consumer-fault claim. Original native matrix remains.

Existing deny/block/admitted refusal consumer-error redelivery persists as a
separate unresolved observation. c05r directly observes deny and admitted error
deliveries=2; block shares the unchanged outer-handler path by source inspection.
Successful invoke-async consumer-error swallowing and true audit-prewrite failure
routing remain unchanged. No claim that every refusal delivers once is supported.

### Interpretation

The reviewed K31 revision pairs lifecycle, Execute Tool, audit-before-effect and clear's
pending05 note under the approved non-admission distinction; 13 test citations are
mechanically relocated to unchanged constructs. A fifth paired sentence in Tool
execution identity says each pass allocates identity, contradicted by earliest
refusal; K31 narrows allocation to execution preparation past that gate.
Identity shape/hash/uniqueness do not change. This paired qualification is explicit
and independently accepted as written, not a new identity policy.

Existing since fields, admitted approval obligations/preexecution run-inactive
accounting, audit-gap refusal, invalid Closure transitions, finite seven K30
observations, ADR0005 ordinary wrapper admission and F1 Session fallback remain.
S1/O1 remain unresolved separately; O1 is an injected allocation/publication gap
without established general production reachability. S2 is approved no-change.

### Recommended next action

Next authorized handoff after learn is independent final five-slice audit plus
residual plan. Old deny/block/admitted refusal redelivery and S1/O1 remain open;
S2 Babel behavior stays no-change. No goal completion or SPEC terminal transition
is implied.

### Verification

Reused independent c05r fresh tests, not rerun by this knowledge-only learn:

- Real llm0.31.1 direct Authority/Runtime/Closure:236/236/0/0 exit0.
- Ordinary direct:236/232/0/4 exit0; four skips supplied non-skipped in real env.
- Focused ordinary5/5 and real4/4 exit0; canonical1361/1320/0/41 exit0, separate
  RUNNER_GATE_OK. Optional/NFC/dependency skips remain; no socket/provider/GUI pass.
- Original callback red ordinary and real: each1/0/1 exit1 at count1vs2.
- Guard-removal control: both tests fail count1vs2; harness exit0 means expected
  failures observed and scoped function restoration completed.
- c05r corrected callback probe: gate1/gate2 error/quit each one delivery, original
  condition propagated, Tape delta nil/effects0; old deny/admitted error still2.
- c05r old probe is55 PASS +1 FAIL across56 assertions, NOT all green. Its failed
  assertion expects the now-fixed TWO callbacks. PROCESS_EXIT=0 does not override
  that failing assertion. Earlier c05/probe-c05.log is56/56, including elapsed
  deadline proof, and is attributed to that earlier run, not claimed fresh here.
- Historical worker physical cleanup probes (3) and exploratory manual rescue (5
  private dirs) remain attributed to framework05-do; c05/c05r did not rerun those
  physical injected-failure probes. Do not equate no residual paths with all-object
  physical cleanup evidence.

Fresh pre-publication strict1087/0/0 and conversion inserted0 pass; authority map
is byte-equal to K30's valid map. Post-write gates and append/backlink integrity
are recorded below after execution, separately from reused ERT.

### Where counterexamples were hunted

- `pairs-before.md` from current K30 enumerates shared constructs; `paired-audit.md`
  records the entry-by-entry decision. The allocation sentence's earliest-gate
  counterexample is included rather than silently left contradictory.
- Read superchat-tool-execute branch-by-branch: structural validation, omitted vs
  invalid liveness predicate, callback-bearing preparation, evidence-origin check,
  first audit, decision/approval branches, preexecution call and outer handler.
  Compared actual --run-active-p, --invoke, --invoke-async and existing callback/
  prewrite tests. This bounds no-audit/no-effect to non-admission, not admitted
  cancellation or all callbacks.
- Search `tool.requested|run-inactive|pending Slice05|Every model-invoked|Each pass`
  in KERNEL and `superchat-tool-execute|refusal-delivery-started|run-active-p` across
  Authority, AgentLoop, Subagent, Runtime and direct tests. Read both wrapper
  callers; no wrapper/Session identity changes inferred.
- Real and negative logs above refute a universal once-refusal claim. Closure
  illegal late event tests and admitted approval matrix refute treating all late
  tool evidence as benign. Preservation clauses do not upgrade old traces.

### References

- `.specs/framework-regression-repairs/SPEC.md` AC5; `issues/05-late-tool-admission.md`.
- Session scratchpad `c05/framework05-check.md`, `framework05-callback-scope.md`,
  `framework05-callback-recheck.md`; original withdrawn PASS history retained.
- Session scratchpad `c05r/gates.sh`, direct/focused/canonical/red/negative logs,
  `my-probe.log`, `my-callback-probe.log`; earlier `c05/probe-c05.log`.
- `/private/tmp/framework05-do/WORKER_REPORT.md`, native red/green, negative controls,
  cleanup.log and exploratory-rescue.log; `/private/tmp/framework05-callback-fix/WORKER_REPORT.md`.
- `/private/tmp/framework05-learn/`: exact candidates, pairs reports, raw-log-audit,
  semantic and mechanical audits and candidate gates.
- Fresh-context, default-to-refute reviewer: independent Claude test worker,
  author of c05/c05r checks but not the Kernel candidate. Its separate text review
  reopened every changed sentence and all13 reanchors/10 new anchors against
  current source: session scratchpad `c05k/framework05-kernel-review.md`, accepted
  by leader. Verdict: acceptable AS WRITTEN, no blocker/required change.
- Publication re-verification: reviewed `c05k/KERNEL.converted.md` and the exact
  candidate are byte-identical, SHA256
  `893358441c57585b763c6aa26bf2006aea9d2a2ca2d8f453384aba8b643d9423`;
  every repository file still matches the staging source snapshot. Existing since
  fields and authority map remain identical. Reviewer optional identity-minter
  and predicate-quit additions are not adopted; no unreviewed Kernel prose or new
  obligation is introduced. Approved N1 STATUS present tense is applied after
  successful publication. No diff-hunk count is relied upon.

### Executed learn publication verification

E-20260906-006 was absent before append; the complete prior Evidence byte prefix
was verified unchanged and the new id occurred exactly once before05 backlink.
K31 was then published byte-identical to the independently reviewed candidate
(SHA256 above), with existing since fields unchanged. Written strict1087/errors0/
warnings0 and authority form pass, exits0. Thirteen mechanical relocations and
the five reviewed paired entries are the exact Kernel delta; no optional Kernel
clause or test was added.05 is done with this evidence_ref; STATUS now hands off
to independent final five-slice audit plus residual plan in present tense.
Scope hashes and whitespace verify exactly Evidence/Kernel/05/STATUS changed;
production/tests/SPEC/doctrine/architecture-map and other files are unchanged.
Fresh publication logs: `/private/tmp/framework05-learn/append-verified.log`,
`strict-written.log`, `authority-written.log`, `publication-scope-backlink.log`.
No ERT rerun, provider/socket/GUI or goal-complete claim is made.

## 2026-09-06 — Public Authority refusal delivery correction (E-20260906-007)

id: E-20260906-007
status: confirmed — published after independent Kernel/ADR text acceptance
scope: tool-refusal-delivery single Slice; public Authority refusal outcome/consumer boundary
applies_when: the listed public Authority refusals deliver a result
supersedes: E-20260906-006 unresolved old-refusal observation, only for the now-verified public paths

### Observation

User-approved option A is independently implementation-PASS in crc/framework-refusal-check.md.
Public blocked/deny/declined/admitted run-inactive refusal delivery now invokes its
consumer once and propagates the original error/quit data rather than retrying as
false audit failure. Gate1 stays outside the outer handler; gate2 and remaining
refusals use a local guard-before-delivery closure, passed to private invoke where
needed. Real audit failure, successful async swallowing, implementation failure,
legitimate audit rows and admitted cancellation/audit-incomplete remain unchanged.

Actual AgentLoop/Subagent native llm aggregate-receiver faults before forwarding
leave immediate running-model/no followup (inactive case audit-incomplete); after
forwarding, request2 already exists and final can complete. Timers are disabled,
not elapsed-time evidence. The two-tool fixture faults after both results have
been aggregated. Per-tool end-func fault before aggregation is untested: coverage
boundary, not an established defect or new obligation. Subagent omits run-active-p;
its existing legacy boundary is preserved, inactive native coverage is AgentLoop.

### Interpretation

Published K32 revises three paired entries and31 mechanical references1238→1241,
preserving existing since fields, identity/Authority/Runtime invariants and exact
private omitted-argument limits. Accepted ADR0006 records the breaking migration:
custom async consumers now observe formerly swallowed conditions and must handle
them at their own boundary when isolation is needed. This is not a uniform callback
policy or Runtime recovery contract. S1/O1 remain unresolved; S2 Babel no-change.
No goal completion or original framework01–05 rewrite is implied.

### Recommended next action

Independent fresh-context default-to-refute Kernel/ADR text review was accepted;
implementation PASS was not substituted for text approval. Publication follows
source/hash and ID re-verification, with Evidence prefix verification before the
issue backlink. Next handoff remains final integration audit and residual plan;
S1/O1 disposition and architecture-map refresh are not authorized by this learn.

### Verification

Reused independent crc fresh runs, NOT ERT reruns in this publication task:
focused ordinary13/7/0/6 and real13/13/0/0; direct ordinary323/310/0/13 and real323/323/0/0;
canonical1364/1321/0/43 with RUNNER_GATE_OK separately; fullreal201/201/0/0 with
UTF8 socket EXCLUDED. Emacs31.0.91, existing isolated llm0.31.1, actual temp SQLite/
Tape/Closure guard; model transport mocked. No provider/socket/GUI pass.

Historical worker unit red1/0/1 and native red2/0/2 are genuine assertion failures.
crc reruns guard-removal mutant and separately moves guard after delivery; each
mutant yields two expected failures (count and original condition). Those are
mutation discrimination, not product failures. crc independent probe23 checks:
22 pass, one checker expectation error; baseline reproduces the same audit-handler
consumer propagation. NOT23 green. Worker setup/parse/zero-selector logs remain
excluded from acceptance. Cleanup attribution stays bounded to prior fixtures,
not a new physical failure-injection audit.

Candidate mechanical results remain in candidate-gates.json and WORKER_REPORT.md
under /private/tmp/framework-refusal-learn-candidate/. Actual written publication
gates are recorded below after execution under /private/tmp/framework-refusal-learn/.

### Where counterexamples were hunted

pairs-before.md and paired-entries.md enumerate15 reopened entries; paired-audit.md
records each keep/change and source searches. Read tool-execute, invoke, invoke-async,
finish-error branches, both wrapper callers, Runtime predicate and native fixture.
Success swallowing/private omission/genuine prewrite faults refute any universal
callback claim. Before/after aggregate receiver and disabled timers refute general
stall/automatic terminal conclusions. No new consumer recovery policy is promoted.

### References

- .specs/tool-refusal-delivery/SPEC.md and issues/01-refusal-delivery.md.
- /private/tmp/framework-refusal-do/WORKER_REPORT.md and saved red/green/negative logs.
- Session scratchpad crc/framework-refusal-check.md, gates.sh, focused/direct/
  canonical/llm-real-nosocket logs, probe-ce.log and probe-ce-before.log, neg1.log,
  neg2.log. Independent implementation reviewer: Claude crc checker.
- /private/tmp/framework-refusal-learn-candidate/: KERNEL.candidate.md, diff,
  pairs-before.md, pairs-after.md, paired-audit.md, reanchor-audit.md, raw-log-audit.log.
- Independent Kernel/ADR text reviewer: Claude ckr worker, distinct from the
  candidate author; prior crc implementation PASS was reopened for text review.
  Report: /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/ckr/framework-refusal-kernel-review.md.
  Verdict ACCEPTABLE, accepted by leader; publication follows its section6.
- Re-verification: every candidate hash matches section1 and the saved manifest;
  current source matches the reviewed snapshot. Authority SHA256 is
  0a1051291c05e9d465287a2ca6ab00ea0b91692df3aa827b2c457ff20a37fa93;
  exact accepted K32 SHA256 is
  491623215f1612cd5d912ba9bc4e96841d68b0928d082063fd3aa6f3534da638.
  Reopened local guard/call sites, optional private fallback, gate1 placement,
  genuine audit handler, retained source/test anchors and paired contracts.
  No required text finding remains. Optional section7 O1–O4 are not adopted.
  E007 and ADR0006 were unused immediately before publication. Only authorized
  candidate/publication metadata transitions differ from reviewed non-Kernel
  candidates; exact before/after strings are in metadata-delta.json and diffs
  under /private/tmp/framework-refusal-learn/. Kernel text has no such delta.

### Executed publication verification

E007 was absent before append. The complete prior Evidence byte prefix and exact
new append were verified unchanged/unique before the issue backlink was written.
The issue is now done with E-20260906-007; its reviewed candidate bytes, including
historical K31 context, are unchanged. ADR0006 is accepted. Written K32 is exactly
the reviewed SHA256 above; Authority still matches its reviewed source SHA256.

Fresh written convert reports1093 refs/inserted0/errors0/warnings0 and produces
byte-identical text; written strict reports1093/errors0/warnings0; authority-map
form passes, all exits0. Written pairs is byte-identical to the reviewed candidate
report:87 entries/574 shared groups/211 universal sentences, narrowed136 in37.
Every original since field is identical. Scope/backlink/hash/added-whitespace
checks pass for exactly Evidence, Kernel, this issue, ADR0006 and STATUS.
No source/test/SPEC/old framework Slice/doctrine/architecture-map file changed.

Authorized metadata deltas are enumerated verbatim in metadata-delta.json:
Evidence candidate/proposed/review-pending wrappers become actual accepted review
and publication provenance; ADR status becomes accepted, proposed-ID qualifier is
removed and verification names ckr acceptance; STATUS's temporary-state wrapper
becomes actual publication state. This executed-gates paragraph records completed
checks. No section7 optional wording or new product/recovery claim was added.
ERT remains reused from crc with the exact limits above; no provider/socket/GUI
or goal-complete claim. S1/O1 disposition and architecture-map refresh remain
subsequent work, not authorized or performed by this publication.
Logs: /private/tmp/framework-refusal-learn/append-verified.log, gates.json,
convert-written.log, strict-written.log, authority-written.log, pairs-written.md,
scope-first.log and final-integrity.log. Next handoff is final integration audit
and residual plan; this learn stops without starting another repair.

## 2026-09-06 — Exact deadline acquisition and bounded residual disposition (E-20260906-008)

id: E-20260906-008
status: confirmed — published after independent Claude crk Kernel text acceptance
scope: runtime-deadline-acquisition single Slice; S1 bounded no-change; architecture-map refresh
applies_when: Runtime acquires its wall-clock deadline timer; this checked residual batch is reconciled
supersedes: E-20260906-003 unresolved O1/S1 handoff only within this approved disposition

### Observation

Independent Claude crm check PASS verifies the O1 change solely in
superchat-runtime--arm-deadline. Runtime holds an inactive exact native timer,
configures/publishes/activates it, and unwinds unsuccessful acquisition by
cancelling that object and compare-clearing its matching slot. Injected error/quit
conditions preserve their original data; foreign timers, other Runs and borrowed
hosts survive. Natural production triggering remains unproved. No new lifecycle,
registry, global sweep or callback policy was introduced.

S1 is user-accepted bounded no-change: the discarded isolated nonterminal fixture
entry has no observed physical leak. All 130 prior Runtime test forms remain;
this does not assert that all test state is terminal or explicitly cleaned.
S2 retains approved literal Babel concatenation; bad Tape fail-closed and
raw-plus-Anchor retention remain no-change.

Architecture README and source snapshot were refreshed; output sequence's stale
Slice05 text was corrected and delivered with fresh browser evidence. The runtime
responsibility diagram and its visual receipts remain unchanged historical
evidence. Logical calls are not require edges, opaque owner is not forced Run ID,
and the diagrams do not enumerate every execution path. crm checked relative
links and artifact hashes; its topology comparison is bounded by the worker's
saved patch because an independent before-sequence copy was unavailable.

### Interpretation

Published K33 adds the bounded acquisition mechanism in the existing Run
lifecycle entry and reanchors 54 Runtime citations across six verified line
groups by +12. Existing since fields and E007/K32 refusal/success compatibility
remain unchanged. No new ADR is needed for this compatible resource correction.
Framework01–05 and separate reasoning/refusal evidence remain E001–E007, not
rewritten or reopened. Per-tool end-func faults remain an untested coverage
boundary, not a proved defect or new obligation; aggregate-receiver and disabled
deadline limits in E007 remain intact.

### Recommended next action

Independent fresh-context default-to-refute Kernel text review by Claude crk was
ACCEPTABLE and accepted by the leader. Reverified the exact reviewed Kernel bytes,
all seven candidate hashes, unchanged current source and R1/R2 corrections before
publication. Evidence prefix/unique append verification precedes issue done/backlink;
README/snapshot use their separate do metadata writer after knowledge publication.
No SPEC terminal status is invented. Next handoff is leader final accounting,
not a self-declared goal completion or additional product repair.

### Verification

Reused independent crm fresh guarded tests, no ERT rerun in this candidate task:
focused ordinary3/2/0/1 and real3/3/0/0; direct ordinary242/235/0/7 and real242/242;
callers ordinary326/312/0/14 and real326/326; canonical1367/1323/0/44, with
RUNNER_GATE_OK separately; full-real-LLM201/201, UTF8 socket test EXCLUDED.
Emacs31.0.91, isolated llm0.31.1, real temp SQLite/Tape/Closure guard, mocked model
transport. Optional/NFC skips, external GUI/provider/socket verification limits
remain; no whole-environment support claim.

34 fault cells include 14 real submit cases with zero transport, one run.failed
and valid trace. Positive real elapsed deadline/cancel/completed/failed/disabled
and concurrent B timer isolation pass. Historical old-arm red observes a live
timer with nil slot, followed by test rescue after observation. crm repeats it
under a transport guard. Old-arm negative control fails the live-timer assertion;
M1 no-cancel and M3 no-unwind each fail both selected tests, M2 no-compare-clear
fails the fault matrix while the foreign-slot test passes as expected. These
expected mutant failures discriminate the repair, not product failures.

Safety event remains partly UNKNOWN: early worker fixture omitted the streaming
mock. crm source trace identifies possible http://localhost:11434/api/chat,
model fixture, generated system prompt and hold/wait inputs, temp DB, no
credentials. Whether curl spawned is UNKNOWN; listener existence was not probed.
The failure reason was not retained. Later mocked or guarded success cannot
retroactively prove early isolation. crm's prose said 12 guarded runs; the crm reviewer confirmed on re-count that
11 runs were guarded (strict/authority/static ran unguarded), each with hits=0;
the earlier fixture question remains UNKNOWN. The original count listed full-real-LLM
twice; there is no missing twelfth log. Publication re-count confirms the 11 saved
zero-hit summaries and the three unguarded mechanical logs. This does not establish
no provider/socket throughout the earlier worker execution.

Worker output delivery showcase9/9, fresh four-size headless containment pass;
worker inspected 1440x900 light and 2048x1320 dark, crm inspected 1440x900 light.
Machine visualReview remains pending; these are bounded screenshot observations,
not all-interaction or live GUI approval. Archify pin provenance is inherited
from the original local download, not re-resolved from network.

### Where counterexamples were hunted

pairs-before.md/paired-entries.md reopen the citation partners; paired-audit.md
records keep/change per entry. Read arm, update/entry, submit handler, quiesce,
native timer configuration/activation and test fault/replacement branches.
Search all Runtime deadline-timer writers and arm callers. The new eq-only
sentence is bounded by local acquisition cleanup: foreign replacement, disabled
deadline, terminal quiescence and B isolation refute a global cleanup reading.
No new Run completion, callback recovery or immunity to arbitrary secondary
native/advice faults is claimed. Existing E007 success swallowing and public
refusal original-condition clauses are preserved, not generalized.

### References

- .specs/runtime-deadline-acquisition/SPEC.md and issues/01-deadline-acquisition.md.
- E-20260906-001 through E-20260906-007: original five slices, separate reasoning
  and refusal implementation/learn histories, with original limits preserved.
- /private/tmp/framework-remaining-do/WORKER_REPORT.md, saved old/new red, negative
  control, commands and architecture delivery/visual artifacts.
- /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/crm/framework-remaining-check.md;
  guard.el, focused/direct/callers/canonical/full-real-llm logs, mutants.log,
  negative.log, old-red-oldarm.log. Implementation reviewer: independent Claude crm.
- /private/tmp/framework-remaining-learn-candidate/: candidate manifest, strict,
  convert, authority, pairs, per-reference reanchor and integrity audits.
- Kernel text reviewer: independent Claude crk, distinct from the candidate author;
  its earlier crm implementation check was not substituted for text approval.
  Report: /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/crk/framework-remaining-kernel-review.md.
  Exact accepted/published K33 SHA256: 4c0fbb17d2fa09937418ad5de89d562653a29a503290019964667771e2f41360.
  Sentence-by-sentence findings and paired/counterexample audit were reverified;
  required R1 corrects the reviewer count and R2 uses actual publication capture
  time after source-hash recheck. Optional O1–O4 are not adopted. Source Runtime
  SHA256 remains 8849ebdc47e8a6f69425ad3d5ad82f22f4de8c08f7d8425c2e350158cf5efc34;
  Authority remains 0a1051291c05e9d465287a2ca6ab00ea0b91692df3aa827b2c457ff20a37fa93.
  Metadata deltas and written gates: /private/tmp/framework-remaining-learn/.

### Executed publication verification

Independent Claude crk text review was ACCEPTABLE for exact K33 bytes; required
R1/R2 metadata corrections were reverified and applied, optional O1–O4 omitted.
E008 was absent before append. The complete prior Evidence prefix and unique
append were verified before the issue became done/evidence-linked. Written K33
matches the accepted hash above; Runtime and Authority retain their reviewed
hashes. All 87 since fields remain unchanged.

Fresh written strict:1097 refs/errors0/warnings0; convert:1097 refs/inserted0/
errors0/warnings0, byte-identical output; authority form passes, all exits0.
Written pairs is byte-identical to the accepted candidate:87 entries/578 shared
groups/212 universal sentences, narrowed137 in37. Backlink/unique ID, authored
whitespace and exact six-file scope checks pass; all other baseline files remain
unchanged. README has22 resolving relative links. At do-stage capture time
2026-09-07T01:54:05.538488+00:00, all nine source hashes were rechecked unchanged;
snapshot has the exact published K33 hash and ten matching hashes overall.
Its pending field describes state only. Diagram/HTML/visual receipts are frozen.

ERT remains reused from crm, with11 guarded logs each hits0; no ERT or transport
probe was run for this publication. Early curl spawn remains UNKNOWN. S1 remains
bounded no-change and S2 literal concatenation remains; this does not assert all
test state is terminal, natural leak prevalence, or a new callback recovery policy.
Logs and exact metadata deltas: /private/tmp/framework-remaining-learn/gates.json,
append-verified.json, R1-reverification.json, snapshot-reverification.json,
final-integrity.json and metadata-delta.json. Next handoff is leader final
accounting; no self-declared goal completion or further product task is started.

## 2026-09-06 — Ordinary chat visible input lifecycle

id: E-20260906-009
status: published
scope: chat-input-lifecycle Slice01; ordinary matching-active input retention, visible follow-up editing, singleton prompt and clear-before-consume
source: approved SPEC rev2 and independent Claude hpc implementation check

### Observation

Independent hpc PASS (zero Required) verifies actual call-interactively superchat
and keyboard-macro C-c C-c/C-q/C-k over real Runtime, temporary SQLite/Tape,
Closure guard and llm 0.31.1 with both native transport entries mocked. New/reopened
Views reuse one editable prompt. Matching ordinary active sends retain the same
visible draft with feedback, without consumption, another Run or automatic send;
after terminal the user explicitly sends. C-q edits that draft after exact
buffer/Session/marker revalidation. Parsed clear reaches existing confirmation
before consumption; refusal leaves View/Run/draft unchanged and acceptance uses
the existing exact Session rollover. Same-Session unrelated-buffer and Workflow
activity do not become blanket blockers. Render and lifecycle production stayed
byte-identical; actual product changes are UI and Dispatcher only.

### Interpretation

Promote the approved bounded input behavior into the existing Render and clear
Action Contracts; preserve opaque owner identity, View prompt authority,
Runtime lifecycle, Authority and callback contracts. Reanchor 73 occurrences
(53 distinct citations; UI41, cmd tests30, Dispatcher2) by actual reader constructs.
Preserve all existing since fields. The approved compatibility migration replaces
C-q hidden append representation with editing visible input and ordinary matching
active concurrent send with explicit retention. Consumers relying on the retired
UI behavior must edit/send the visible draft explicitly; no durable queue is promised.
No new ADR is proposed: this bounded, reversible UI decision and its migration are
captured in the approved SPEC and this Evidence, without a new recovery contract.

Residuals, not repairs or new obligations: hpc S1 other commands/Skills/Workflow
retain existing dispatch, including possible same-View concurrent work; no general
concurrency policy was approved. hpc S2 nonempty cancel redirect appends directly
to an existing draft (DRAFTREDIRECT), an unresolved UX choice outside ordinary
retention. hpc S3 queued-follow-up still has Render/mode-line/agents-panel readers;
the new C-q path does not produce a hidden value, but private/external producers
were not exhaustively established and the compatibility readers are not declared
universally dead. These hpc S1/S2 labels are separate from historical framework
S1 bounded no-change and Babel S2. No automatic issue proliferation or goal closure.

### Recommended next action

Independent Claude test worker (sole reviewer): original fresh-context default-to-refute human_prompt_lifecycle_kernel_review of SHA256 f9f9e41c77051da683ace5c9f38b8b64342f69f70e7a373d5f6c5f5191e61fec was NOT_PUBLISHABLE_AS_IS, Required R1 (Render precondition explicit-owner conflict), report /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/hpk/human-prompt-lifecycle-kernel-review.md; narrow human_prompt_lifecycle_kernel_r1_recheck of SHA256 64150942a53ac02327da23c128396f93d8254e8e6907973bee23f42175489c43 closed R1 and found the exact text PUBLISHABLE, report /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/hpk/human-prompt-lifecycle-kernel-r1-recheck.md. Publication reverification confirms the sole R1 sentence delta, pending-prompt callers without Run/transcript-owner arguments, unchanged remainder, approved hashes and source baseline; no optional N1–N4 changes. Implementation PASS is not text
approval. After approved text and source/hash reverification, append this record,
verify the full prior prefix and unique ID, then backlink Slice01 and publish the
reviewed Kernel/status. E009 is proposed and currently available, not reserved.
The SPEC remains confirmed; no administrative terminal transition is invented.

### Verification

Reused here, independently fresh in hpc: focused real17/17/0/0 and ordinary
17/8/0/9; related real601/599/0/2 and ordinary601/573/0/28; canonical
1376/1323/0/53 with ERT exit0 and a separate RUNNER_GATE_OK; full real LLM
201/201/0/0 with the socket test EXCLUDED. All six saved suite logs have guard
hits0 and exit0. Four saved mutants admission/identity/initial/prompt each install
and fail the named target test, exit1/guard0: discriminating negative controls,
not product failures. Historical do reds remain attributed to the do report.
hpc reports nine probe outcomes and a separate intentional make-network-process
guard self-check (hits1); the retained probes.log itself contains six ERT tests,
6/6 with hits0. The remaining reported outcomes and guard self-check are attributed
to the signed-off hpc report, not invented standalone raw logs or new candidate runs.
Authority form and reader-parens were reused by hpc from the do report; hpc also
independently confirmed the one-form control migration and 22 unchanged forms.
Its fresh strict1097/73/0 exit1 is mechanical drift, not semantic approval.
Fresh candidate-only gates: strict1112/0/0, convert1112/inserted0/errors0/warnings0
with byte-identical output, authority form exit0 with an unchanged authority map;
pairs87 entries/590 shared groups/213 universals, narrowed138 in37. Source hashes,
87 unchanged since fields, prefix/ID/backlink and repository-scope checks are
recorded in the temporary candidate package. These do not approve Kernel prose;
no ERT, daemon, GUI, provider or transport probe is run by this candidate preparation.
Batch-captured message feedback is not human visual/echo-area acceptance.

### Where counterexamples were hunted

pairs-before.md and paired-entries.md reopen 71 entries around the 22 affected
entries; reanchor-audit.md records each of the 73 occurrences, and paired-audit.md
records keep/reanchor/semantic decisions. Read Dispatcher send branches and public
status matching, UI open/reopen and C-q reader reentry, Render shared-prompt reuse,
and the unchanged cancel redirect and queued-value consumers. The new 'only if'
C-q sentence is checked against live/dead/replaced View, clear during minibuffer,
same Session and marker identity. No text/name-priority identity is inferred.
Counterexamples to a broad concurrency claim are unrelated-buffer and Workflow
Runs plus non-clear command dispatch. Counterexamples to universal draft immutability
are intentional C-q edits and S2 redirect; the Kernel scopes retention accordingly.
Exact tests, caller search and per-clause audit are in paired-audit.md. Existing
success/refusal callback, deadline, liveness and Tape invariants remain untouched.

### References

- .specs/chat-input-lifecycle/SPEC.md and issues/01-input-lifecycle.md.
- /private/tmp/human-prompt-lifecycle-do/WORKER_REPORT.md and handoff-scope.json.
- /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/hpc/human-prompt-lifecycle-check.md;
  hpc suite/exit/command logs, strict.log, mutant logs and probes.log.
- /private/tmp/human-prompt-lifecycle-learn-candidate/: exact candidate hash,
  source-index, reanchor and paired audits, strict/convert/authority/since/scope gates.
- Independent Claude test worker (sole reviewer): original fresh-context default-to-refute human_prompt_lifecycle_kernel_review of SHA256 f9f9e41c77051da683ace5c9f38b8b64342f69f70e7a373d5f6c5f5191e61fec was NOT_PUBLISHABLE_AS_IS, Required R1 (Render precondition explicit-owner conflict), report /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/hpk/human-prompt-lifecycle-kernel-review.md; narrow human_prompt_lifecycle_kernel_r1_recheck of SHA256 64150942a53ac02327da23c128396f93d8254e8e6907973bee23f42175489c43 closed R1 and found the exact text PUBLISHABLE, report /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/hpk/human-prompt-lifecycle-kernel-r1-recheck.md. Publication reverification confirms the sole R1 sentence delta, pending-prompt callers without Run/transcript-owner arguments, unchanged remainder, approved hashes and source baseline; no optional N1–N4 changes.

## 2026-09-07 — Selected active-run cancel edits the original visible draft

id: E-20260907-001
status: published
scope: superchat-cancel selected active-run UI branch and its Render pending prompt
kernel_text_reviewer: independent Claude test worker, fresh-context default-to-refute; PUBLISHABLE AS-IS (0 Required, 0 Semantic); /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/crk/cancel-redirect-visible-draft-kernel-review.md; K35 SHA256 868ccbd80a851f4f8ef1d778fb5e8776e6aaf9237e0bcb047fcdf20b742975a4

### Observation

The user-approved compatible UI revision cancels first, prefills the original
visible draft, preserves an unchanged string without mutation, and replaces it
for accepted edits (empty clears; whitespace is retained). Exact buffer/Session/
Render prompt checks bracket the editor; no auto-send or steer is introduced.
Minibuffer error/quit propagates after cancellation. crd independently PASS with
0 Required on the three do files; production changed only superchat-cancel.

### Interpretation

This short-path Action Contract belongs beside K34 ordinary-input retention,
whose cancellation distinction remains true and unchanged. Pending draft editing
does not require a Run transcript owner. Paused-owner/no-active branches, other
commands/Skills/Workflow dispatch and legacy queued-value readers are unchanged.
The rule is not global Runtime cancellation or an arbitrary change-hook rollback
promise. No new SPEC, Slice, backlink or ADR is needed for this bounded revision.

crd's four entry probes compare old/new code: send-input/send-region cancel with
an existing prompt; submit(origin)/agent-run remain active and open no editor.
Its act/request-plan observation is source/report context, not a fifth probe or
a universally settled cancellation policy. This pre-existing coverage remains
unresolved. The cosmetic missing success message on editor error/quit is not
repaired; whitespace-only differences are accepted edits, not unchanged RET.

### Recommended next action

Independent exact-text review accepted K35 SHA256 868ccbd80a851f4f8ef1d778fb5e8776e6aaf9237e0bcb047fcdf20b742975a4; reviewer/report recorded above.
Publication reverification 2026-09-07T08:07:09.973314+00:00: reviewed source/candidate hashes and Evidence prefix unchanged; ID available; review findings match current source.
The accepted text is published without optional N1/N2 changes; this record precedes exact
Kernel/STATUS bytes. No issue backlink exists on this short path. No goal or
administrative SPEC closure is asserted.

### Verification

Reused independent crd logs (not rerun in candidate preparation): focused real
5/5; ordinary 5/0/0/5; direct real 204/204 and ordinary 204/180/0/24; related
606/604/0/2 (NFC Workflow skips); canonical 1381/1323/0/58 with ERT exit0 and
separate RUNNER_GATE_OK; full real llm201/201, UTF8 socket fixture EXCLUDED by
selector. Each of these seven raw logs has guard hits0 and exit0. Environment:
isolated llm0.31.1, actual Runtime/SQLite/Tape/Closure, native async/stream mocks.
No GUI minibuffer or live provider acceptance. Guard selfcheck is historical do
evidence, not a fresh crd selfcheck; crd Authority form likewise reused do evidence.

Historical do red proves absent prefill, concatenation and stale callback writes;
crd's own post-read and trim mutants each fail one test, append mutant fails three.
These expected mutant failures discriminate the guards; they are not product
failures. Raw commands/results/hashes are indexed in reused-crd-gates.json.

Fresh candidate-only checks: convert1119/inserted0/0errors/0warnings and byte-equal;
strict1119/0/0; pairs87entries/596groups/215universals, narrowed140in37;
Authority form exit0 on current repository with candidate authority map byte-equal;
since87 unchanged; 72 individual reanchors across57 citations; all1065 baseline
repository files and historical candidate packs unchanged. The approximate
session-id citation claim was rechecked: original UI defvar-local1130 is now1142,
the same supporting construct, not claims18. No new source authority is inferred.

Where counterexamples were hunted: pairs-before.md, paired-entries.md and
pairs-audit.md reopen67 paired entries and enumerate both new quantified sentences
plus negative claims against complete cancel branches, Render prompt lookup,
tracking callers, real24-cell matrix and crd old/new entry probes. Only the Render
entry changes semantics after mechanical reanchors; all since/other semantics stay.

### References

- Do: /private/tmp/cancel-redirect-visible-draft-do/WORKER_REPORT.md; red.log; red-concat.log; scope-final.json.
- Independent check: /private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/crd/cancel-redirect-visible-draft-check.md; *.command.json;
  focused/direct/related/canonical/full-real-llm logs; probe-entry{,-old}.log;
  mutant-postread.log, mutant-trim.log, mutant-append.log.
- Candidate package: /private/tmp/cancel-redirect-visible-draft-learn-candidate; KERNEL.diff; reanchor-audit.md; pairs-audit.md;
  strict.log; convert.log; authority.log; freeze-check.json.
- Prior enacted ordinary input: E-20260906-009 / K34, retained unchanged.
