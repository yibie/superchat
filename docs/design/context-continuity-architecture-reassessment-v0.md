# Context Continuity Architecture Reassessment v0

> Status: design decision draft; production code is frozen.
>
> Date: 2026-08-08.
>
> Scope: Anchor, View, Handoff, Compact, and their relationship to the
> Runtime/Tape/Closure foundation. This document does not authorize an
> Observer, a Learning Layer, or a Compact repair.

## 1. Decision in one sentence

The original Tape/View/Handoff direction is not disproved, but it was never
finished or implemented as designed; the current Compact path is
**infeasible as promised** because it treats a bounded, free-text LLM summary
as the only forward context while promising that no important context is
lost.

The correction is architectural, not another test loop:

- Tape remains the lossless fact source.
- View is the bounded and explicitly lossy context selector.
- Anchor is a marker plus provenance, not a truth container.
- Handoff is a task-scoped state contract, not a synonym for compaction.
- Compact, if retained, is an optional lossy index or rendering cache.
- Raw Tape recall is explicit and auditable; it is not forbidden.

## 2. Scope and evidence

This reassessment is based on:

- the current source and repository navigation contract;
- `docs/design/tape-mechanisms.md`;
- `docs/design/runtime-architecture-v0.md`;
- the retired compaction audit in
  `experiments/archive/superchat-active-experiment-materials-through-2026-08-09.zip`;
- current `superchat-compact.el`, `superchat-tape-view.el`, and
  `superchat-prompt-hooks.el`;
- read-only `jj` history;
- independent Herdr Researcher and Reviewer audits followed by two rounds of
  cross-examination.

No production file was changed for this reassessment.

## 3. The stable architecture is not the failed experiment

The current verified ownership model is:

```text
Surface / Dispatcher / Core
              |
              v
           Runtime --------------------> llm transport / tool loop
              |                                      |
              |                                      v
              |                                  Authority
              |                                      |
              v                                      v
             Tape ---------------------------------> DB
              |
              +------------> Closure (pure judgment)
              |
              +------------> Projection (replayable views)
                                |
                                +--> Memory
                                +--> Workspace
                                +--> Workflow / Team views

Runtime clients: Workflow, Subagent, Compact
Prompt transformation: Core -> prompt hooks
```

Runtime, Tape, Authority, Closure, and Projection do not depend on Compact for
their ownership claims. Deleting Compact would remove a context-management
strategy, not the durable Runtime foundation.

This matters because the Compact failures do not prove that the Runtime,
Tape, Closure, Experience Audit, or the broader auditable-agent goal is
impossible.

## 4. What the original design actually said

The original mechanism design separated four responsibilities:

1. Tape preserves full history. Anchors are reconstruction markers, not
   deletion points (`tape-mechanisms.md:66-68`).
2. `source_ids` are load-bearing pointers that allow the exact source entries
   to be read (`:108-116`).
3. View assembles bounded, task-oriented context from Tape (`:249-319`).
4. Handoff is a phase transition whose summary is sufficient for a known
   downstream phase and whose source set is exact (`:379-390`).

The document did **not** prohibit reading raw Tape. It explicitly designed a
Tape-backed View and optional pre-handoff recall. Therefore the impossible
combination "bounded context + universal self-containment + never read raw
history" was not a necessary property of the original architecture.

However, the design was not ready to implement. It ended with five open
questions that explicitly "block writing the v1.1 handoff"
(`tape-mechanisms.md:549-574`).

The public seams proposed by that document are absent from all current `.el`
files:

- `superchat-db-tape-anchor-append`;
- `superchat-tape-view-assemble`;
- `superchat-context-source`;
- `superchat-db-tape-handoff-append`;
- `superchat-anchor-auto-write-enabled`.

Consequently, the original design is best classified as
**design-incomplete and unimplemented**, not failed.

## 5. What was implemented instead

The current path combines responsibilities that the design kept separate:

```text
mutable in-memory history
        |
        +--> select at most N messages
        |          |
        |          v
        |     free-text LLM summary
        |          |
        |          +--> write Tape anchor with source_ids
        |          |
        +----------+--> collapse the captured in-memory history
                              |
                              v
                     next prompt sees synthetic anchor
```

`superchat-compact.el` currently owns all of these concerns:

- context selection;
- semantic compression;
- anchor persistence;
- mutable-history replacement;
- automatic topic "handoff";
- prompt injection and `/expand` support.

That is not a deep module. Its hidden contract spans Compact, DB JSON
encoding, Tape View queries, prompt hooks, and buffer-local history.

More importantly, its prompt claims that the summary ensures no important
details or context are lost (`superchat-compact.el:38-40`), while the same
module limits input to 100 messages (`:72`) and may replace the captured
history with one generated string (`:251-279`). The overclaim existed in the
first Compact implementation.

## 6. Repository history: the plan was bypassed

Read-only `jj` evidence gives this order:

| Time | Change | What happened |
|---|---|---|
| 2026-06-14 | `rnkzutzs` / `4d1ca813` | The mechanism design existed in repository history. |
| 2026-06-21 09:13 | `tmzxtoyo` / `65feee07` | Compact, free-text summary, history collapse, anchor prompt injection, the missing reader call, and its mocked test were introduced together. |
| 2026-06-21 09:31 | `rnopulwp` / `1a8e8d49` | Tape schema v3, FTS5, and the current query-oriented View layer followed. |
| 2026-06-21 09:37 | `xyqlxwmz` / `181c0aca` | History IDs and `/expand` were added after Compact. |
| 2026-06-21 09:45 | `vwyzqqny` / `14c31a02` | Topic hooks called automatic Compact an automatic handoff. |
| 2026-06-22 | `oslkyswu` / `402b49dd` | Later integration fixes did not restore the designed View/Handoff contract. |

This is not evidence that the original architecture was tried and failed. It
is evidence that implementation began before its blocking questions were
resolved, then evolved along a different route while retaining the original
names.

## 7. Failure classification

The three deterministic findings in the fidelity audit have different
meanings. They must not be collapsed into one story.

| Finding | Verified fact | Classification | What it proves |
|---|---|---|---|
| M1-F1 | Compact selects the newest `N` messages but can collapse the entire captured history, so an unselected message disappears from forward in-memory context and is absent from the anchor's `source_ids`. | Structural contract defect caused by merging selection scope and replacement scope. | The current context transition is not coverage-closed. It does not prove Tape data loss or generic LLM forgetting. |
| M2-F1 | `superchat-db-tape-get-by-session` is declared and called but not defined; the `fboundp` guard silently makes persisted-anchor prompt injection return `nil`. | Unimplemented read seam, present since the first Compact change and hidden by a mock. | The persisted anchor read path was never connected. It does not prove the architecture impossible. |
| M4-F1 | A second synthetic anchor has no `:id`; `(:unknown 3)` is JSON-encoded as an object, read as an alist, and passed as an invalid SQLite ID by `/expand`. | Ordinary representation/serialization bug plus missing first-class anchor identity. | Multi-generation lineage is broken in the current implementation. It does not prove exact provenance is impossible. |

Fixing M1, M2, and M4 is necessary for a mechanically coherent Compact, but
it is insufficient to make the "no important information is lost" promise
true.

## 8. The actual impossibility boundary

For arbitrary unbounded histories and a fixed prompt budget, the following
three requirements cannot all hold:

1. the forward representation is bounded;
2. it is self-sufficient for every possible future question or lossless
   reconstruction;
3. raw history is never read again.

There are finitely many bounded representations and unboundedly many possible
histories. At least two histories must map to the same representation. A
future question that distinguishes those histories cannot be answered from
that representation alone.

`source_ids` do not invalidate this result. They are pointers, not content.
They restore navigability and reversibility only when the system is allowed to
read the referenced Tape entries. Once that read is allowed, the summary is no
longer universally self-contained—and that is acceptable.

A second, independent boundary is verification:

- a free-text LLM summary;
- a semantic completeness guarantee;
- no structured task contract and no human review;

cannot provide a mechanical proof that the summary is complete. Testing can
measure proxies and counterexamples; it cannot turn that open-world claim into
a guarantee.

## 9. Two separate verdicts

### 9.1 Original Tape/View/Handoff architecture

**Verdict: design-incomplete; feasible in principle under explicit scope;
neither proved nor disproved.**

It avoids the information-theoretic contradiction because:

- raw Tape remains lossless;
- View may select and reread raw entries;
- Handoff completeness is scoped to a known downstream phase, not arbitrary
  future questions;
- `source_ids` provide provenance and navigation, not compressed content.

The phrase "everything you need is in summary and source_ids" still needs an
explicit phase-scope qualification. Without it, the text invites the stronger
and impossible interpretation.

### 9.2 Current Compact promise

**Verdict: infeasible as promised; feasible only after reframing as explicit
lossy context management.**

The current implementation makes a generated summary the practical forward
carrier, collapses history, and promises that important information is not
lost. Better prompts, larger models, and more sentinel tests cannot make that
universal promise true.

## 10. Architectural decisions

The following decisions are frozen for the next planning stage:

1. **Do not patch toward the old promise.** The three mechanical defects may
   later be fixed, but no fix may be described as making compression lossless
   or safe in general.
2. **Do not build an Observer.** The code already has transport observation,
   Closure Shadow, terminal hooks, Tape consumers, and Projections. A semantic
   Observer would not solve the context information bound; it would add a
   second model-generated interpretation.
3. **Do not call automatic compaction a handoff.** A Handoff requires an
   explicit phase boundary and a task-scoped state contract. Current topic
   threshold compaction has neither.
4. **Do not treat a summary as authoritative state.** A summary is a lossy
   derived artifact with provenance.
5. **Do not forbid raw recall.** `/expand` or a future View may read exact Tape
   entries, but the recall must be explicit, bounded, and auditable.
6. **Do not let collapse exceed provenance coverage.** Any context replacement
   must identify every omitted or represented source range.
7. **Do not compose provenance through summary text.** Multi-generation
   material must retain a path to immutable raw Tape entries; a summary of a
   summary is not evidence of the original content.

## 11. The next work is a contract, not code

Before any Compact repair or semantic experiment, write and review a small
Context Continuity Contract that resolves the original blocking questions and
defines:

- what a bounded View may omit and how omissions are disclosed;
- the exact distinction between raw fact, deterministic projection, and
  model-generated interpretation;
- the phase-scoped fields required by a Handoff;
- the provenance type and JSON representation of source entry IDs;
- the conditions under which raw Tape may be recalled into a prompt;
- the multi-generation lineage rule;
- the failure behavior when the contract cannot be satisfied.

Only after that contract is frozen should the project choose one of two
implementation directions:

- remove or disable Compact and accept deterministic truncation temporarily;
  or
- retain Compact as an optional, explicitly lossy adapter behind the new
  contract.

That choice must be made before reopening Gate S semantic experiments.

## 12. Effect on the larger program

The broader Superchat direction remains coherent:

- Runtime/Tape/Closure can provide durable, auditable execution evidence;
- Projection can build deterministic read models;
- Experience Audit can search for repeated structure and may legitimately
  return zero observations;
- a later intervention can test whether a selected observation improves an
  outcome.

What cannot be promised is that a generic observer or free-text compressor can
turn arbitrary long-running experience into complete, bounded, truthful, and
self-sufficient semantic state.

The boundary between Agent Runtime and Learning Runtime therefore remains an
experimental boundary, not a missing module waiting to be named.
