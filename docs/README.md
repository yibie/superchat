# Documentation

Each fact has one home: the tier whose job it is. Elsewhere, link there.

| Tier | Job | Does not belong there |
|---|---|---|
| Root `AGENTS.md` | Standing orders an agent needs in every session, one to three lines each, each linking its home. Word budget 1,000. | Stories, worked examples, anything restated from a linked home |
| [architecture.md](architecture.md) | The map of `current` behaviour: composition, spine components, the turn loop, seams, extension points. Read before changing code. Word budget 2,000. | Type definitions, per-component detail, decision rationale, plans |
| [roadmap.md](roadmap.md) | The one remaining plan, with every item labelled `current`, `accepted`, `proposed` or `historical`, and each milestone's removal list and acceptance | The current map, decision rationale |
| [components/](components/README.md) | One spec per component: the contract a consumer relies on, in a fixed structure | Behaviour narration across components, rationale |
| [decisions/](decisions/README.md) | Decision records: the why and what was given up, with mandatory alternatives; `accepted/` holds user decisions (their implementation state is stated per item in the roadmap), `implemented/` describes shipped reality in the present tense | The order of work (→ roadmap), acceptance checklists once shipped |
| [postmortems/](postmortems/README.md) | Incident records, the only tier where narrative belongs | Design rationale |
| [protocols/](protocols/session-editor-workspace-close.md) | Cross-component behavioural contracts that no single component owns | Component-local detail |
| [adr/](adr/) | Accepted design decisions in force, written before the decisions/ format existed; one becomes historical only when a later record names it as superseded | New records |
| [SKILLS_QUICKSTART.md](SKILLS_QUICKSTART.md), [plugin-guide.md](plugin-guide.md) | Tutorials: an ordered path to an outcome | Reference tables, rationale |
| [CONTEXT.md](../CONTEXT.md) | Domain vocabulary, one canonical term per concept | Implementation detail |

Placement: bugs → postmortems; rationale → decisions; procedures → tutorials; contracts → components; standing orders → root `AGENTS.md` with a link to the rationale.

## Writing rules

- Document current state, not change history: no "previously", "now", "no longer". History lives in version control, decisions and postmortems.
- Every behaviour or contract change adds or updates a decision record in the same commit; a mechanical documentation edit does not.
- One physical line per paragraph.
- Name the exact function, event, Tape type or rule instead of a metaphor.
- A budgeted document that goes over its budget is fixed by relocating content to its home, then condensing, and only then by raising the budget with a stated reason.

## Slop checklist

Hunt these in any document: the same rule stated in two homes; narrated history; implementation-status annotations; hand-copied inventories that source or a script owns; reasoning transcripts; emphasis everywhere; spec-speak ("should", plans, checklists) in an implemented decision record.
