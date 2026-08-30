# Remember/recall provenance

status: confirmed
revision: 1
context_refs: `CONTEXT.md` Tape, Entry, Anchor, Tape View, Projection, and Session vocabulary; `KERNEL.md` K24 append-only Tape and Anchor provenance contracts; `docs/architecture-review-20260905/02-problems.md` P6

## Problem and goal

Explicit `/remember TEXT` currently appends a source user Entry and a remember
Anchor, then the default compatibility bridge independently appends a
`memory.captured` event.  Explicit `/recall` concatenates Session-scoped Tape
results with global Memory results, so one remembered fact can enter the next
prompt more than once.

For future explicit remembers, record and validate an exact typed relationship
between the bridge Memory fact and its Session, remember Anchor, and source
Entry.  At the explicit `/recall` Tape+Memory merge seam only, suppress the
verified bridge Memory result when its exact remember Anchor was actually
selected.  Preserve all raw Tape facts and every unverified or unrelated
Memory result.

## Unchanged contracts

- Tape is append-only.  The source user Entry, Anchor, `memory.captured` event,
  and materialized Memory row remain distinct facts; none is deleted or
  rewritten.
- The change does not use or extend Continuity selection, Anchor recursion, or
  `:represented`; current Continuity `raw-fact` / `:represented []` behavior is
  unchanged.
- `superchat-memory-legacy-writes-enabled` defaults to non-nil.  Disabling it,
  the no-argument `/remember` Memory-only branch, and the `/recall` rule that
  supplements fewer than twenty Tape hits with global Memory all remain.
- Explicit recall keeps its current Tape query, Memory query, result ordering,
  and limits.  The new overlap filter runs after both queries and adds no
  refill query.
- A selected remember Anchor does not suppress its raw source user Entry.
  Equal text, equal bare numeric IDs in different stores, historical Memories,
  ordinary Memories, and corrected Memories are not treated as the same fact.
- Memory review accept/reject and correction, cross-Session global retrieval,
  Projection rebuild, schema, and existing public search result contracts
  remain.  A correction is a new fact and does not inherit this relationship.
- Partial writes retain existing non-transactional append-only behavior across
  Tape and Memory; this work adds no cross-owner transaction.

## Decision and boundaries

Only the explicit `/remember TEXT` compatibility-bridge write may attach the
new relationship.  It names one non-empty Session ID, the exact storage ID of
the successfully appended remember Anchor, and that Anchor's exact source
storage ID.

Memory owns decoding, restoring, and validating this relation.  The command
module may provide typed source values and ask Memory whether a retrieved row
is a verified bridge fact; it must not decode the `memory.captured` envelope or
duplicate the classifier.

A relationship is usable only when all of these are proven from stored data:

- the relation has the expected type and complete identities;
- the `memory.captured` event and materialized Memory row correspond;
- the relation Session equals the Memory capture event Session, recall
  Session, Anchor Session, and source Entry Session;
- the Anchor exists at the exact storage ID, is kind `anchor`, has topic
  `remember`, and precedes the bridge Memory event;
- the named source exists in that Session, precedes the Anchor, and occurs in
  the Anchor's `source_ids`.

At merge time, suppression occurs only if that exact Anchor is present in the
current `tape-rows`.  A missing, malformed, ambiguous, stale, or unverifiable
relation preserves the Memory candidate.

If the remember Anchor append returns nil, the existing bridge may still write
Memory but receives no fabricated relationship.  If Anchor append signals,
the original path stops.  If Memory append or projection fails, already
appended Tape rows are not rolled back.

## Model delta

**Remember bridge provenance:** a future explicit remember bridge Memory fact
may carry a typed, Session-scoped relationship to one exact remember Anchor
and its exact source Entry.

**Explicit recall selection:** the `/recall` Tape+Memory merge may omit only a
Memory candidate whose stored relationship validates and whose corresponding
remember Anchor is selected in that same Tape result set.

This is a user-approved compatible revision: it narrows duplicate Context
presentation without deleting history or changing general Memory/Tape search.
Kernel promotion belongs only to `learn` after independent verification.

## Action Contracts

### Bridge an explicit remember with exact provenance

precondition: `/remember TEXT` has a stable Session and has attempted its
existing source Entry and remember Anchor appends; the compatibility bridge is
enabled.

input: normalized remembered content and title, Session ID, exact Anchor
storage ID when available, and exact source storage ID.

permitted effect: append the existing `memory.captured` fact and materialize it,
optionally carrying a typed relation when complete exact identities exist.

invariant: no fake relation is emitted for a nil Anchor; source and Anchor
identity are not inferred from text or cross-store numeric equality; Tape
facts are never rolled back or mutated; no schema or general Memory behavior
changes.

### Select explicit recall candidates by verified provenance

precondition: explicit `/recall` has completed its existing Session Tape query
and, when applicable, its existing global Memory query.

input: the ordered, limited Tape rows; ordered Memory candidates; recall
Session; and Memory-owned provenance recovery/validation.

permitted effect: after the existing queries, remove a bridge Memory candidate
only when its verified exact remember Anchor occurs in the selected Tape rows;
then queue the remaining candidates without a refill query.

invariant: the selected Anchor and raw source remain; unrelated, historical,
ordinary, corrected, equal-text, and unverifiable Memory candidates remain;
ordering and query limits do not change; selection creates no represented-range
claim.

## Seams and verification

- `superchat.el` owns explicit `/remember` orchestration and the single
  explicit `/recall` Tape+Memory merge selection seam.
- `superchat-memory.el` owns relationship storage shape, envelope recovery,
  validation, and the predicate/data needed by that merge.
- Direct tests belong with existing command/Tape View and Memory SQLite tests.
- If relationship recovery truly requires a DB query extension, arrange may
  add the smallest `superchat-db.el` read-only owner and its direct test, but
  every existing public Memory search row shape, ordering, limit, FTS fallback,
  review, and rebuild contract must remain.  This is not authorization for a
  schema migration or general query facade.

Tests-first verification must use a real temporary SQLite/Tape store and cover:

- future explicit remember creates user → remember Anchor → bridge Memory with
  exact typed Session/Anchor/source relation;
- valid relation plus selected Anchor suppresses only its bridge Memory after
  query limiting, performs no refill, and retains Anchor plus raw user source;
- valid relation whose Anchor is not selected retains the Memory result;
- same text and overlapping numeric IDs across Tape/Memory do not suppress;
- historical and ordinary Memory without the relation remain;
- wrong Session, wrong kind/topic, missing/late source, source absent from
  `source_ids`, missing/malformed relation, or mismatched Memory event all fail
  closed by retaining Memory;
- correction creates a new unlinked fact; accept/reject behavior is unchanged;
- bridge disabled, no-argument remember, Tape result counts below/at twenty,
  ordering, pending Context consumption, and prompt formatting remain;
- nil Anchor allows the old unlinked bridge write, Anchor error stops on the
  old path, and Memory failure leaves prior Tape rows intact.

Focused command/Memory tests precede the canonical runner, check-parens,
Kernel strict read-only validation, and authority/layering checks.  No user
database, provider, or Continuity implementation is involved.

## Compatibility and migration

There is no historical backfill and no database schema migration.  Existing
rows without a verifiable relation remain visible, so old duplicates may
remain rather than being guessed away.  New records become suppressible only
through verified provenance.

The bridge default, no-argument remember behavior, Memory review/correction,
global Memory supplementation, and public searches retain their existing
contracts.  Unknown external consumers of private Memory payload details
remain unknown; release and publication are outside scope.

## Out of scope

- Text-based deduplication, bare-ID matching across stores, historical
  backfill, deleting Tape/Memory rows, or disabling the compatibility bridge.
- Continuity assembly, recursive Anchor expansion, non-empty `:represented`,
  compaction, Runtime, Authority, Experience, or provider changes.
- A database schema migration, general recall registry/framework, second
  Projection, or cross-write transaction.
- Making no-argument `/remember` create Tape rows or changing the Tape `<20`
  global Memory supplementation rule.
- Treating a correction as the same represented fact as its predecessor.

## Issue map

- `issues/01-remember-recall-provenance.md` — one ready end-to-end Slice over
  explicit remember provenance and explicit recall selection; existing DB read
  seams are sufficient, so no DB production owner is included.

## Revision notes

- Revision 1 (2026-09-05): captured the user-approved future-only provenance
  and explicit-recall selection contract from the P6 investigation.
