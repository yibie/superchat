# 01 Select explicit recall by verified remember provenance

status: done
blocked_by:
writer: do
authority: `superchat-memory.el` | owned — Memory is the sole owner of bridge-provenance recovery and validation; `superchat.el` only supplies identities and consumes the verified verdict at its existing explicit `/recall` merge seam.
spec_ref: `.specs/remember-recall-provenance/SPEC.md` — Action Contracts “Bridge an explicit remember with exact provenance” and “Select explicit recall candidates by verified provenance”
context_ref: `KERNEL.md` K24 — append-only Tape, Anchor provenance, Projection, Session, and Tape View contracts
evidence_ref: E-20260905-012

## Goal

Future explicit `/remember TEXT` bridge Memory facts carry exact typed
provenance.  Explicit `/recall` suppresses only the verified bridge Memory when
its exact remember Anchor was selected, while preserving raw Tape, unverified
Memory, existing queries, and all compatibility paths.

## Scope

- `superchat-memory.el` — store the future-only relation in the existing
  `memory.captured` payload; recover and strictly validate it through existing
  Memory/Tape identities without changing public search row shape.
- `superchat.el` — pass exact Session/Anchor/source identity from explicit
  `/remember TEXT`; consume the Memory-owned validation result at the sole
  explicit `/recall` Tape+Memory merge, after existing limited queries and
  without refill.
- `test/test-memory-sqlite-facade.el` — real SQLite projection, provenance
  recovery/validation, correction, review, malformed and partial-write cases.
- `test/test-tape-view.el` — real command → Tape/Memory → explicit recall →
  pending Turn/prompt selection behavior and compatibility matrix.

Read-only inspection confirms no `superchat-db.el` production edit is needed:
the existing exact Memory-row lookup and parameterized Tape SELECT seam can
support Memory-owned validation.  `superchat-db.el`, Tape View/Continuity,
dispatcher, prompt hooks, Runtime, Authority, runner, schema, and knowledge
files are outside write scope.

This Slice is functionally dependency-free.  The coordinator must schedule it
only after the separate P3 Slice completes independent `check` and `learn`, to
protect the shared dirty working copy.  That serial scheduling rule is not a
product or code dependency and is therefore not recorded in `blocked_by`.

## Acceptance

1. A real temporary SQLite explicit `/remember TEXT` appends, in order, its
   source user material and kind=`anchor`, topic=`remember` Anchor; when bridge
   writing succeeds, `memory.captured` uses the same non-empty Session and
   carries a complete typed relation to the exact Anchor and source storage
   IDs.
2. Memory alone decodes and validates the relation.  Validation proves the
   projected Memory row corresponds to that `memory.captured` event; event,
   relation, recall, Anchor, and source Sessions match; Anchor and source exist;
   source precedes Anchor, Anchor precedes capture; and Anchor `source_ids`
   contains that exact source.
3. After existing Tape and Memory limits are applied, a valid bridge Memory is
   suppressed only when its exact Anchor is present in the selected
   `tape-rows`.  There is no refill query; retained candidate ordering is
   unchanged.
4. The selected Anchor and its raw user source both remain and appear in the
   pending Turn/prompt in their existing form.  The test has positive content
   assertions, not merely a reduced count.
5. A valid relation whose Anchor is outside/not in the selected Tape result
   retains Memory.  Same text, overlapping Tape/Memory numeric IDs, historical
   and ordinary Memory, and another Session's fact all remain.
6. Missing/malformed relation, wrong Session, wrong Anchor kind/topic, missing
   or late source, source absent from `source_ids`, capture/row mismatch, and
   any validation error fail closed by retaining Memory without corrupting the
   remaining candidates.
7. Correction creates a new unlinked Memory fact and is not suppressed through
   its predecessor's provenance.  Existing review accept/reject, replacement,
   projection rebuild, FTS/LIKE fallback, row shape, ordering, and limit tests
   remain green.
8. Bridge disabled and no-argument `/remember` remain unchanged.  Fewer than
   twenty Tape hits still cause the existing global Memory query; twenty do
   not.  Filtering itself is the approved new behavior and occurs only at
   explicit `/recall`, not automatic recall or Continuity.
9. If Anchor append returns nil, the bridge may write the old unlinked Memory
   and no fake relation.  Anchor error follows the old stopping path.  Memory
   failure does not roll back already appended Tape rows.
10. No historical backfill, schema migration, text deduplication, cross-store
    bare-ID comparison, Anchor recursion, represented-range change, registry,
    cross-write transaction, or public search contract change is introduced.

## Verification

Red-before-green order:

1. Add the real SQLite explicit remember/recall duplicate case and prove the
   old path admits the bridge Memory alongside its selected Anchor.
2. Add focused Memory validation failures and partial-write cases.
3. Implement the typed relation and Memory-owned verifier minimally.
4. Apply the verifier only at explicit recall merge, then add compatibility,
   limit/no-refill, correction, Session, pending Turn, and prompt assertions.

Run focused new tests, both complete direct files, and the canonical runner:

```sh
emacs -Q --batch -L . -l test/test-memory-sqlite-facade.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-tape-view.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch -L test -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Also run check-parens on the four changed Elisp files, inspect the focused diff,
confirm the two test files are canonical-runner members, and search for schema,
Continuity/`:represented`, public Memory row-shape, Runtime, and Authority
changes.  All databases must be temporary and local; use no provider.

Hard-stop and return to `plan` if implementation needs a DB/schema migration,
a public Memory search shape change, a new durable identity or event kind,
Continuity/represented semantics, historical inference/backfill, a
cross-owner transaction, or Runtime/Authority changes.  If an exact identity
query truly cannot be expressed through existing read seams, report the
specific missing query rather than introducing a general DB facade.

## Evidence

For `check`, preserve the real duplicate red case, exact stored relation and
validation matrix, filtered/no-refill ordered results, raw-source/Anchor/prompt
positive assertions, partial-write observations, full/canonical counts, and
syntax/strict/authority results.  Leave `evidence_ref` empty until `learn`
records independently accepted evidence.

## Do verification summary — 2026-09-05

Tests-first preserved the real duplicate failure: the initial explicit-recall
case returned three pending items instead of the required raw user + Anchor
pair (1/1 unexpected), and the initial Memory provenance tests rejected the
new keywords before implementation (2/2 unexpected).  The minimal green path
adds a typed future bridge to `memory.captured`, validates its exact durable
Memory/capture/Session/Anchor/source chain solely in Memory, and filters only
verified bridge Memory after the existing explicit-recall limits.  AC1–AC10
are covered by the positive command-to-prompt case; selected/not-selected and
no-refill cases; malformed, mismatched, missing, and late-source matrices;
correction/rebuild/review coverage; and bridge-disabled, no-argument, and
partial-write compatibility cases.

Final verification: Memory SQLite 23/23; Tape View 75/75; canonical 1275 total,
1257 expected, 0 unexpected, 18 skipped, `RUNNER_GATE_OK`; all four changed
Elisp files pass `check-parens`; both focused tests are canonical members; the
authority map form is valid.  Kernel strict reports 1025 refs, 43 errors, 0
warnings: 42 are shared cumulative `superchat.el` source-anchor movement and
one is `test/test-tape-view.el` at
`view/assemble-cancelled-run-is-omitted-but-explicit-recallable`; semantic
Kernel review and mechanical re-anchoring remain for independent check/learn.
No DB/schema, Continuity/represented, Runtime, Authority, backfill, registry,
or public Memory search-row change was introduced.  `evidence_ref` remains
empty pending independent acceptance and learn.

Required correction: the real limit regression now observes the provenance
case itself.  With fewer than twenty selected Tape rows, one real Memory query
returns the ordered bridge and ordinary candidates; the real verifier removes
the selected Anchor's bridge, retains the exact Tape order plus ordinary
Memory, and performs no refill query.  Target and full Tape View are 1/1 and
75/75; production is unchanged from the independently checked implementation.
