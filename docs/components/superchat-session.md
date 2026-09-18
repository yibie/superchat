---
description: "Read a Session's durable history off the Tape: ordered entries, full-text search, branch lineage, supersession exclusions, Session configuration, compaction anchors, and the bounded context envelope a prompt is assembled from."
kind: "reference"
---

# superchat-session

## Summary

Use this component to read what a conversation durably is. It turns Tape rows into the Session-shaped answers a consumer needs: the Session's entries in Tape order, the ids of every Session on this machine, a context envelope bounded by entry and character budgets, the selection that produced it, the exclusions a supersession event imposes, the Session's replayed runtime configuration, and its compaction Anchors with their exact source rows. Choose it whenever you need history that outlives the process, and reach past it to the Tape or the chat View directly only when you are writing facts rather than reading them. Its main cost is that it reads SQLite on every call and caches nothing, so a caller that renders the same view repeatedly pays for the same rows each time. Its most important boundary is that it never writes: no method here appends, edits or deletes an Entry.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

Call the `session` Service method you need with a Session id; the chat history collector and compaction are the two consumers that call it on every turn.

### When to choose it

This component offers four different answers, and picking the right one is the whole decision. They are not interchangeable, and none substitutes for another:

| You want | Read | What it is |
|---|---|---|
| Raw rows, for an audit or a browser | `entries`, `search` | The Tape's own rows, undecoded. Runtime events sit among them under `kind` `system` |
| The Session's runtime configuration | `configuration` | A Projection replayed over the Session's history |
| The bounded context a prompt is assembled from | `assemble-view` | A Context View: a selection with its receipt, disclosure and omissions |
| The model-visible message list | `superchat-view-derive-messages` | The one derivation of model history |

`superchat-view-derive-messages` is the single derivation for model history, and the chat View's regenerable cache reads it too, which is why the two histories agree. It is not a method of this Service: it belongs to the model-history contract, and a consumer that wants model messages reads it rather than composing a list of its own. **Filtering `entries` by `kind` is not an alternative to it** -- `kind` cannot separate model material from runtime events, which is why the derivation exists.

### Minimal configuration

The component mounts as the `session-view` plugin:

```elisp
(superchat-plugin-mount 'session-view)
```

It declares `:inject '(tape)`: mounting it requires a Tape read capability, supplied here by the default `tape` provider, which owns the SQLite connection. What it depends on is that Service, not the particular plugin name -- a composition that supplies the same Service elsewhere satisfies it. Composition, activation and loss follow the [composition contract](../design/plugin-architecture.md) rather than a rule of this component's own. There is no configuration surface: the plugin takes no config keys.

| Field | Default | Meaning |
|---|---|---|
| (none) | — | The plugin reads no configuration. Budgets, purposes and exclusions are per-call arguments. |

### Reading entries

`session entries SESSION-ID &optional SINCE-ID KINDS LIMIT` returns rows for one Session, ordered by ascending `id`.

| Position | Column | Notes |
|---|---|---|
| `nth 0` | `id` | Integer storage id; globally addressable, so a row from a parent Session can be named without ambiguity |
| `nth 1` | `topic` | The row's topic |
| `nth 2` | `kind` | `user`, `assistant` or `anchor` for material rows; `system` for runtime events |
| `nth 3` | `content` | The row's text |
| `nth 4` | `meta` | JSON text, or nil |
| `nth 5` | `created_at` | Storage timestamp |

`SINCE-ID` restricts to rows with a strictly greater id. `KINDS` is a list of kind strings and restricts to those kinds. `LIMIT` caps the count. An unknown Session id returns no rows rather than signalling.

User intent and model responses keep their original text in the material row, before display rendering. Structural events retain bounded human-readable summaries. The [conversation-material decision](../decisions/implemented/2026-09-17-preserve-conversation-material.md) explains the storage boundary; Context View budgets still include an item intact or omit it with a receipt. Older rows that already contain a clipped prefix cannot be restored from their full-text fingerprints.

The `kind` column is not a sufficient discriminator on its own: runtime events are stored as `kind` `system` and carry an event envelope in their metadata. Read the metadata's event type when the distinction matters; `superchat-tape-replay-events` does this decoding.

### Listing Sessions

`session session-ids` returns every Tape Session id, most recently active first. It groups by Session id and orders by each Session's greatest row id, so the order is by last write, not by Session creation.

### Searching

`session search QUERY &optional SESSION-ID LIMIT` returns matching rows, capped at `LIMIT` (default 20). Passing a `SESSION-ID` restricts the search to it; a Session id with no match returns no rows. Search reads the storage-level full-text index and returns raw rows, not continuity items.

### Reading history with lineage

`session history SESSION-ID &optional MAX-ID` returns the Session's rows with its parent Sessions' rows prepended, following the branch lineage that a branch anchor records. `MAX-ID` truncates that lineage at a storage id. Rows the Session's own `turn.superseded` events replaced are removed, and because storage ids are globally addressable the exclusion set is unioned across every lineage segment. A cyclic lineage signals rather than returning a partial history.

### Reading Session configuration

`session configuration SESSION-ID` replays the Session's rows into the latest runtime configuration plist. It is a Projection over the Session history, not a stored row, so it answers with the configuration in force after the last relevant event.

| Key | Set by event |
|---|---|
| `:capability` | `capability.activated` |
| `:capability-source` | `capability.activated` |
| `:profile-id` | `capability.activated`, and removed when that payload has no profile id |
| `:topology` | `topology.changed` |
| `:permission-level` | `permission.level` |
| `:sandbox-mode` | `sandbox.mode` |
| `:approval-policy` | `approval.policy` |
| `:workspace-id`, `:workspace-root` | `workspace.bound`; both are set to nil by `workspace.unbound` |
| `:surface` | `intent.received` |

A Session with no such events answers the initial configuration: capability `chat`, topology `single`, and nil for capability source, workspace, surface and the three permission keys (an absent permission key means the configured default: `superchat-sandbox-default-mode`, `superchat-approval-default-policy`). An `authority.policy.selected` event written before M6 is ignored.

### Reading compaction Anchors

`session anchor-latest SESSION-ID` returns the Session's latest typed compaction Anchor when that Anchor's provenance is complete, and nil otherwise. An Anchor qualifies only when it carries the typed compact metadata at the supported version, when its source ids are present, unique and positive, when they name rows of the same Session that exist and precede it, and when each of those rows is a material row of the Session's lineage. A Session that never compacted answers nil.

`session expand-anchor SESSION-ID ANCHOR-ROW` returns the exact raw rows that Anchor distills, and nil for an Anchor that is malformed, foreign, missing, or not a typed compaction Anchor.

`session anchor-source-ids ANCHOR-ROW` returns the Anchor's validated source ids, or nil when the Anchor is not a typed compaction Anchor. Both Anchor methods read the metadata that the Context Continuity contract fixes; see [the contract](../design/context-continuity-contract-v0.md).

### Assembling a bounded context envelope

`session assemble-view SESSION-ID &key PURPOSE MAX-ENTRIES MAX-CHARS AFTER-STORAGE-ID THROUGH-STORAGE-ID PINNED-STORAGE-IDS EXPANDED-ANCHOR-IDS EXCLUDED-STORAGE-IDS` returns the deterministic context envelope a prompt is assembled from. Most arguments are validation-checked before any read:

| Argument | Accepted | Otherwise |
|---|---|---|
| `SESSION-ID` | Non-empty string | Signals |
| `PURPOSE` | Symbol | Signals |
| `MAX-ENTRIES`, `MAX-CHARS` | nil or a non-negative integer | Signals |
| `AFTER-STORAGE-ID`, `THROUGH-STORAGE-ID` | **Reserved in v0**: nil only | A non-nil value signals |
| `PINNED-STORAGE-IDS` | **Reserved in v0**: nil, or an empty vector | A non-empty vector signals |
| `EXPANDED-ANCHOR-IDS` | nil, or a vector of positive integers | Signals |
| `EXCLUDED-STORAGE-IDS` | nil or a list of positive integers | Signals |

The two budgets count different things: `MAX-ENTRIES` counts selected items and `MAX-CHARS` counts characters of the rendered prompt. Neither is a token budget -- token cost is a property of the consuming model, not of this envelope -- and a nil budget resolves to the contract's finite default rather than to no bound.

**Windowing and pins are reserved, not enabled.** The Context Continuity contract freezes `AFTER-STORAGE-ID`, `THROUGH-STORAGE-ID` and a non-empty `PINNED-STORAGE-IDS` as advanced selectors that direction 1 does not implement: a non-nil window or a non-empty pin vector signals rather than silently claiming unfiltered coverage. A later selector slice must bring its own public-seam tests before enabling them, and enabling them is a change to that contract, not a detail of this component. Budgets and omissions are unaffected: nil or omitted budgets resolve to the finite v0 defaults, and nil never means unlimited.

**Anchor expansion is not in that reserved set.** `EXPANDED-ANCHOR-IDS` is a live selector: naming an Anchor's storage id asks the assembly to read that Anchor's exact source rows in place of the distillation, and it is how Anchor expansion reaches a prompt. `expand-anchor` above is the same reading for one Anchor. Never treat the window/pin reservation as covering it.

When `EXCLUDED-STORAGE-IDS` is omitted, the View's supersession projection supplies it. If that read fails, the result is the unknown read-failure envelope rather than context assembled without its exclusions. The envelope carries a `:status`; `satisfied` is an ordinary assembly, and `empty`, `partial` and `unknown` state their own conditions. The envelope's selection, budget decisions and receipt are the Context Continuity contract's subject; see [the contract](../design/context-continuity-contract-v0.md).

### Rendering a continuity item

`session item-line ITEM` renders one normalized continuity item as its canonical body line: `User: CONTENT`, `Assistant: CONTENT` or `System: CONTENT` for the three material kinds, and `Summary: CONTENT` for an item whose class is `model-interpretation`. An item of any other kind signals. This is the renderer that both the context envelope and the compaction command use, so a consumer that renders continuity items itself would produce a second, divergent rendering.

### Absent dependencies

The plugin requires the `tape` plugin. Without it there is no Session provider, so every `session` Service call fails as an unknown Service method. The reads themselves need an open database connection; opening it is the caller's responsibility.

## Understand the implementation

<details><summary>Implementation internals</summary>

The component is one file, `superchat-tape-view.el`, plus the `tape` plugin it injects. Everything it returns is derived on read: Sessions, lineage, exclusions, configuration and Anchors are all recomputed from rows, and nothing is cached between calls.

A read is a SQL statement against the `tape` table. `superchat-view-entries` builds its SQL from optional clauses and passes the Session id, the `since-id` and the kinds as bound parameters. `superchat-view-search` delegates to the storage-level search. Everything else composes those two: `superchat-view-session-history` walks lineage and removes exclusions, `superchat-view-superseded-storage-ids` unions the `turn.superseded` ids of every lineage segment, `superchat-view-session-configuration` replays history through the `session-configuration` Projection, and `superchat-view-compaction-anchor-latest` validates the newest typed Anchor against the rows it names.

`superchat-view-assemble` is the largest operation: it validates its arguments, assembles a bounded envelope, and contains read failures instead of propagating them.

| Section | Lines | Role |
|---|---|---|
| Entries and Session listing | 27–63 | `entries`, `anchor-latest`, `session-ids` |
| Lineage | 65–83 | The branch anchor that names a parent Session and turn |
| Supersession | 85–122 | Per-Session `turn.superseded` ids, unioned across lineage |
| History | 124–151 | Lineage-ordered, exclusion-filtered rows |
| Session configuration | 174–242 | The `session-configuration` Projection and its initial value |
| Compaction Anchors | 254–356 | Typed Anchor recognition, provenance validation, expansion |
| Search | 358–361 | The `session` Service's search entry |
| Continuity items and rendering | 419–589 | Record normalization and `item-line` |
| Context assembly | 1181–1233 | The bounded envelope and its read-failure form |
| Plugin | 1308–1331 | The `session-view` plugin and its Service table |

</details>

## Further exploration

- [architecture.md](../architecture.md) — where the Tape, the read models and this component sit among the other owners; prerequisite.
- [The Context View contract](../design/context-continuity-contract-v0.md) — the envelope, receipt and Anchor semantics that `assemble-view` produces.
- [request-lifecycle.md](../protocols/request-lifecycle.md) — where the chat history collector calls this component in a turn.
- [plugin-architecture.md](../design/plugin-architecture.md) — how a plugin injects another plugin and provides a Service.

## Model experience

Indirectly: this component chooses what the model sees, and renders none of it itself.

#### What the model sees

The `assemble-view` envelope becomes the prompt's history fragment, so its selection and budget decisions decide which past material reaches the model, including the `System:`, `User:`, `Assistant:` and `Summary:` lines that `item-line` renders. The prompt also carries a compact disclosure header naming the storage identifiers of what was included, and every disclosure range in the receipt carries its Session id, so a Session identifier is model-visible to that extent; the Context Continuity contract's disclosure section is the authority on what is disclosed. The model never sees a Tool call's arguments or a raw Tape row from this component.

#### Token effect

This component is where the history fragment's token cost is decided. `MAX-ENTRIES` and `MAX-CHARS` bound it, and an item larger than the character budget is skipped rather than truncated, so the cost stays at or under the budget instead of exactly at it.

#### KV cache effect

The envelope is deterministic for a given Session, purpose and bounds, so an unchanged Session re-assembles byte-identically and the history fragment stays a cache prefix. A new Entry, a supersession exclusion or a changed budget moves the fragment, which invalidates the prefix from the first changed item onward.

## Known limitations

- Every method reads SQLite and nothing is cached, so a caller that re-renders the same view repeatedly pays full read cost each time. Render from a returned envelope rather than calling `assemble-view` again.
- `entries` returns raw rows, including runtime events under `kind` `system`, so a consumer that treats every row as conversation material will render event envelopes as messages. The remedy is to read model history through `superchat-view-derive-messages` rather than to filter rows here, because `kind` alone cannot make that separation.
- A failed read and an empty Session reach a caller as different statuses, but a caller that unwraps only the prompt sees both as no material. Read `:status` and the receipt before treating an envelope as evidence about a Session.
- `item-line` recognises exactly the three material kinds plus the `model-interpretation` class and signals on anything else, so a new item class reaches a consumer as an error rather than as a fallback rendering.
- `anchor-latest` answers nil both for a Session that never compacted and for one whose newest Anchor fails provenance validation, so a caller cannot distinguish "no compaction" from "an Anchor this component will not trust".
- The component reads no Session-scoped policy: any caller with database access reads every Session on the machine.

### Dev note

<details><summary>Working context for maintainers</summary>

`anchor-latest`'s two nil cases are indistinguishable to a caller. Whether that is worth a distinct answer is unresolved; `expand-anchor`'s docstring already separates malformed from missing, so the distinction exists one level down.

</details>
