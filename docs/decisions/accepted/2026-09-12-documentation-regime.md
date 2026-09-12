# Decision: Documentation regime after DeepSeek Harness

Status: accepted

## Problem

Design documents accumulated as exploration notes: several contracts for one subsystem, superseded versions kept beside current ones, plans that lived in a local scratch directory and were cited by committed documents, and instruction files giving three different counts of the same thing. A reader could not tell what described the tree.

## Decision

The user decided on 2026-09-12: "同意", adding "harness 我发现，它是一个组件一份 spec，而改动和决策的地方，会引用不同组件的 spec，具体每一份 spec，还有固定的结构". The regime, at the smallest tier of what dsh does (no verification scripts; no new bilingual requirement, while the existing README.md and README_cn.md pairing stays):

- One spec per delivered component with a fixed structure ([components/README.md](../../components/README.md)); no `current` spec for an unbuilt module.
- Decision records with a fixed skeleton and mandatory alternatives ([decisions/README.md](../README.md)); decisions cite component specs by relative link.
- Present tense only; one fact has one home; the tier table in [docs/README.md](../../README.md) says where each kind of fact lives.
- Word budgets for the two standing documents (root `AGENTS.md`, `architecture.md`), enforced by review.
- Postmortems only for subtle, systemic, costly-to-rediscover failures ([postmortems/README.md](../../postmortems/README.md)).

The historical design and research documents were deleted the same day with the user's approval ("同意，先将过时的记录删掉", "删"); they remain in version history.

## Alternatives considered

**dsh's full machinery** (word-budget gates, note-format gates, bilingual triplets, generated catalogs). Rejected for now: one reader language and one to three agents in parallel; gates are added when a failure shows the need.

**Keep the previous contract-per-experiment style.** Rejected: it produced the drift this decision removes.

## Consequences

Writing becomes part of every change. A behaviour or contract change without its spec and decision update is incomplete; a mechanical documentation edit (a link repair, a status header, a typo) needs no decision record. Older ADRs under `docs/adr/` stay frozen and are linked from newer records when superseded.
