# Component specs

One spec per component, at `docs/components/<name>.md`. A component is a delivered logical module that provides one thing: a spine package, a capability seam, a surface, or a plugin. A module may span or share files; the spec, not the file, is the unit. The spec is that component's contract and states `current` behaviour only: no spec is written for a module that is not in the tree, and a planned module lives in [the roadmap](../roadmap.md) and `docs/decisions/` until it lands. Rationale lives in `docs/decisions/`, incidents in `docs/postmortems/`, and the map of how components fit together in `docs/architecture.md`.

Sections that do not apply may be omitted rather than filled with `None.`; the order of the sections that are present is fixed.

## Kinds

Every spec declares one `kind` in its front matter; the kind selects the skeleton below.

| Kind | Use it for | Example |
|---|---|---|
| `reference` | a component that mounts as a plugin or provides a Service | `superchat-session`, `superchat-tools`, `chat-commands` |
| `library` | a file with no plugin surface, only functions other files call | `superchat-kernel` (Scope, Service, Event primitives) |
| `profile` | a composition row list that assembles an application | `chat`, `inline`, `headless` |
| `group` | an index page for a family of components | `docs/components/README.md` itself, `seams/README.md` |

## Skeleton for `reference` and `library`

Sections in this order, with these names. Every H2 opens with one orienting paragraph before any table, code or H3.

```markdown
---
description: "One or two sentences on what a reader can do with this component, with searchable domain terms."
kind: "reference"
---

# superchat-<name>

## Summary

Three to five sentences: what a user or another component can DO with it, when to choose it, its main cost, its most important boundary. Not its role name or internal identity.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

One orienting sentence: the common path.

### When to choose it
### Minimal configuration
The smallest mount that works (a profile row or `superchat-plugin-mount` call) and the config table: Field, Default, Meaning.
### <one H3 per public operation>
Services provided (name, methods) and required; events emitted and listened to, each with its combination rule; tools or commands registered; Tape event types appended and what each means; failure behaviour when a dependency is absent.

## Understand the implementation

<details><summary>Implementation internals</summary>

Design concept and dataflow, enough to understand how it works. A source-map table: section of the file, line range, role. No function inventories and no docstring restatement.

</details>

## Further exploration

Three to seven adjacent pages, closest prerequisite first, one phrase each.

## Model experience

One H3 per distinct model-visible contribution, each with exactly these H4s in this order:

#### What the model sees
#### Token effect
#### KV cache effect

A component with no model-visible effect writes one sentence beginning "None, as …" or "Indirectly, through …", followed by a single `KV cache effect` H4.

## Known limitations

One orienting sentence, then top-level bullets: current constraints a consumer must know, with their consequence. Not a task list.

### Dev note

<details><summary>Working context for maintainers</summary>

Active hypotheses and unresolved questions only. `None.` when empty. Never authoritative.

</details>
```

## Skeleton for `profile` and `group`

`Summary`, `Table of contents`, then for a profile the ordered row list with each row's owner and what removing it costs; for a group a table of direct children with one-line roles. `Related documentation`, then `Dev note`. No Model experience or Known limitations sections.

## Rules

- Present tense only. No "previously", "now", "no longer". A spec has no change history.
- One fact has one home. If `docs/architecture.md` or another spec states it, link there.
- The spec updates in the same commit as the behaviour it describes.
- Name the exact function, event, or Tape type. Before writing "boundary", "surface", "shape", ask whether a more exact term exists.
- Every operation claim is reproduced against the current checkout before it is written; a claim that could not be reproduced is deleted, not softened.
- A spec cites the decision records that shaped it with relative links, never by prose name.
