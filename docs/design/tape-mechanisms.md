# Tape Mechanisms — Design Doc

> **Status**: design, not yet implemented (as of 2026-06-05, HEAD `c0fe5f2`).
> Targets superchat v1.1 (anchors) and v1.2 (views + handoffs).
> Supersedes nothing — this is new territory for superchat.

## What this doc is

A translation layer. [tape.systems](https://tape.systems/#mechanisms)
defines three mechanisms — **anchors, views, handoffs** — that turn
an append-only event log from a passive transcript into an active
runtime substrate. superchat already has the append-only tape
(`superchat-db-tape-append` against the `tape` SQLite table); this
doc decides what anchors/views/handoffs *mean* in superchat's
concrete codebase, with named functions, field names, and call
sites. Future handoffs to implementer models cite this doc; they
don't re-derive it.

## Why this matters now

Today every "recall" in superchat is full-text search:

- `superchat-send-input` auto-recall →
  `superchat-db-memory-search-simple input 5` (FTS5/trigram or LIKE
  fallback) → top-N matches injected as a prompt block.
- `/recall <keywords>` is the same call with a larger limit, surfaced
  to the user.
- Conversation history is a separate, dumb mechanism:
  `superchat-prompt-hook--conversation-history` reads the in-memory
  `superchat--conversation-history` list and concats it.

These mechanisms work *across topics, across phases, across
sessions*, without distinguishing them. tape.systems' insight:
**context is constructed, not searched**. Anchors mark phase
boundaries; views assemble context from segments around relevant
anchors; handoffs encode causal transitions. Bringing this into
superchat is the difference between "Emacs LLM client with memory"
and "Bub-style agent runtime where every long-running interaction
has structure."

The schema already anticipates this. From
`superchat-db.el:117-124`:

```sql
CREATE TABLE tape (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT    NOT NULL,
  kind       TEXT    NOT NULL CHECK(kind IN
               ('user','assistant','tool_call','tool_result',
                'anchor','system')),
  content    TEXT    NOT NULL,
  meta       TEXT    DEFAULT '{}',
  created_at TEXT    NOT NULL DEFAULT (datetime('now'))
)
```

`'anchor'` is already a valid `kind`. `superchat-db-tape-last-anchor`
(`superchat-db.el:249`) is a reader that returns the most recent
one. **Nothing in the codebase writes anchors yet.** The reader is
orphan. This doc connects the wires.

## tape.systems source claims, restated

From the page, edited for our purposes:

> **Anchors are reconstruction markers, not deletion points.** Full
> history is preserved before the anchor. You "Rebuild from anchor,
> skip full scans." Anchors can carry structured state payloads.

> **Views are task-oriented assembled context windows.** Context is
> constructed, not inherited wholesale. Views select entry subsets
> by session type (single, multi-turn, multi-session, topic-threaded)
> rather than ranking by relevance.

> **Handoffs are anchors plus state transitions.** A handoff writes
> a new anchor, attaches minimum inherited state, and shifts the
> execution origin past the new anchor. Encodes causality and phase
> intent, not just text.

Three discovery mechanisms that an FTS index structurally cannot
provide. We will implement all three, in order: anchors first
(v1.1), views and handoffs second (v1.2).

---

## Mechanism 1 — Anchors

### Conceptual definition (in superchat terms)

An anchor is a row in the `tape` table with `kind='anchor'`. Its
`content` is a one-line human-readable summary of what the anchor
marks. Its `meta` is a JSON object carrying structured state:

```json
{
  "phase": "topic-switch",
  "summary": "User moved from refactoring discussion to release planning.",
  "source_ids": [127, 129, 131, 132],
  "owner": "user",
  "created_by": "explicit"
}
```

| field         | required | meaning                                                                          |
|---------------|----------|----------------------------------------------------------------------------------|
| `phase`       | yes      | Short tag from a controlled vocabulary (see "Phase taxonomy" below).             |
| `summary`     | yes      | One sentence. Same content as the row's `content` column. Duplicated for queryability via JSON. |
| `source_ids`  | no       | Array of tape `id`s that informed this anchor. Empty array if none.              |
| `owner`       | no       | `"user"` if user-initiated, `"system"` if auto-written by a hook.                |
| `created_by`  | no       | `"explicit"` (e.g. `/anchor` command), `"workflow-step"`, `"skill-boundary"`, `"auto-rotation"`. |

`source_ids` is the load-bearing field. Without it, an anchor is
just a glorified system message. With it, you can reconstruct what
the anchor *summarizes* — by reading exactly those tape entries —
rather than reading every entry between this anchor and the
previous one.

### Public API to add

In `superchat-db.el`:

```elisp
(cl-defun superchat-db-tape-anchor-append
    (session-id summary &key phase source-ids owner created-by extra)
  "Append an anchor row to the tape for SESSION-ID.
SUMMARY is the one-line description (also goes into `content').
PHASE defaults to \"general\".  OWNER defaults to \"system\".
CREATED-BY defaults to \"explicit\".  SOURCE-IDS is a list of
tape ids.  EXTRA is an additional plist merged into meta.
Returns the new row id.")
```

This wraps the existing `superchat-db-tape-append` with `kind`
hard-coded to `'anchor'` and `meta` JSON-encoded from the keyword
arguments. **No schema change.** The `tape` table's `kind` CHECK
constraint already permits `'anchor'`.

Reader stays as-is: `superchat-db-tape-last-anchor` continues to
return the most recent anchor row for a session.

New companion readers we'll need in v1.2 but worth defining now:

```elisp
(defun superchat-db-tape-anchors-since (session-id since-id)
  "Return anchor rows for SESSION-ID with id > SINCE-ID, ordered ascending.")

(defun superchat-db-tape-entries-between (session-id from-id to-id)
  "Return non-anchor tape rows in (FROM-ID, TO-ID].
Use TO-ID = nil to mean \"up to the latest entry\".")

(defun superchat-db-tape-entries-by-ids (ids)
  "Return tape rows matching IDS, preserving the input order.")
```

### Phase taxonomy

Anchors without a phase tag are noise. We define a small controlled
vocabulary now and refuse to grow it without explicit user need.

| `phase` value      | When written                                                                 |
|--------------------|------------------------------------------------------------------------------|
| `"general"`        | Default if user runs `/anchor` without specifying phase.                     |
| `"topic-switch"`   | User explicitly says "let's switch topics" (manual) OR `/anchor topic`.      |
| `"skill-enter"`    | A `>skill-name` invocation. `source_ids` = the user turn that triggered it.  |
| `"skill-exit"`     | The skill's last assistant turn. `source_ids` = skill input + each step.     |
| `"workflow-step"`  | Between steps of a `type: workflow` skill. `source_ids` = prior step's turn. |
| `"handoff"`        | Phase transition with explicit summary; see Mechanism 3 below.               |
| `"session-start"`  | Optional. First entry of a new `session_id`. Used for clean reconstruction.  |
| `"session-resume"` | Optional. When a session is reopened after some idle threshold.              |

Phases beyond this list are **rejected** by the anchor-append API.
Userland extension comes later via a registered taxonomy variable;
v1.1 hardcodes the list.

### Write call sites (v1.1)

```
explicit:
  /anchor [phase] [summary]            in superchat-dispatcher
                                        → superchat-db-tape-anchor-append
  /handoff phase summary                in superchat-dispatcher (Mechanism 3,
                                        deferred to v1.2)

auto (defcustom-gated, default OFF):
  superchat-skills--invoke              writes skill-enter
                                        before running the skill body
  superchat-skills--invoke (after)     writes skill-exit on completion
  superchat-workflow-execute            writes workflow-step between steps
  superchat-send-input long-session     writes auto-rotation every N turns
                                        (defcustom N; default 50)
```

Each write goes through `superchat-db-tape-anchor-append` with
appropriate `phase`, `source-ids`, `owner`, `created-by`. The auto
sites are gated by:

```elisp
(defcustom superchat-anchor-auto-write-enabled nil
  "When non-nil, system anchors are written automatically at
skill / workflow / long-session boundaries.")
```

Default off because we want anchors to **mean** something. An
anchor every 50 turns with no narrative content is just noise.
Users opt in.

### Read call sites (v1.1)

None new. `superchat-db-tape-last-anchor` exists and is used by no
caller; we leave it alone. The v1.2 view assembler will be the
first real consumer.

### Why this is sufficient for v1.1

By the end of v1.1:

- Users can write anchors with `/anchor`.
- Skill / workflow code paths *can* write anchors automatically (if
  the defcustom is on).
- The tape table contains anchors in a queryable, structured form.
- Nothing consumes them yet — but they exist, with `source_ids` and
  `phase`, ready for v1.2.

Compare to ROADMAP v0.9's pattern: "first ship the scaffolding
(commits `0c1bb37`..`19ccfed`), then flip the dispatcher to use it
(commit `9bcef78`)." Same shape here: v1.1 ships the writer, v1.2
flips the reader.

### Test plan (v1.1)

Create `test/test-tape-anchors.el`:

- `anchor/append-creates-row-with-kind-anchor`
- `anchor/append-encodes-meta-as-json`
- `anchor/append-rejects-unknown-phase`
- `anchor/append-defaults-phase-to-general`
- `anchor/source-ids-empty-by-default`
- `anchor/last-anchor-returns-most-recent-for-session`
- `anchor/last-anchor-is-session-scoped`
- `anchor/auto-write-disabled-by-default-on-skill-enter`
- `anchor/auto-write-enabled-writes-skill-enter-anchor`
- `anchor/auto-write-records-source-ids-of-trigger-turn`
- `anchors-since/returns-anchors-after-cutoff-ascending`
- `entries-between/excludes-from-id-inclusive-to-id`
- `entries-by-ids/preserves-input-order`

---

## Mechanism 2 — Views

### Conceptual definition (in superchat terms)

A **view** is a Lisp data structure describing how to assemble a
context window from tape data. Not stored; built per turn.

```elisp
;; Plist shape of a view spec.
(:session-id     "20260605-180000-a4f2"   ; required, scope
 :since-anchor   123                       ; optional, id of last anchor to start from
 :include-kinds  (user assistant)          ; default: omit tool_call/tool_result
 :include-anchor-summaries t               ; default t — surfaces anchor.content
 :extra-entries  (97 98)                   ; optional, ids of entries to pin in
 :max-entries    20                        ; cap to avoid prompt bloat
 :order          :chronological            ; :chronological | :reverse-chronological
 :format         :role-tagged              ; :role-tagged | :raw)
```

The **assembler function** turns a view spec into a string suitable
for `setf (superchat-turn-prompt new-turn) ...`:

```elisp
(defun superchat-tape-view-assemble (view-spec)
  "Materialize VIEW-SPEC into a prompt-ready string.
Returns the empty string if the view selects no entries.")
```

### Replacing the conversation-history hook

Current code at `superchat-prompt-hooks.el:141-148`:

```elisp
(defun superchat-prompt-hook--conversation-history (turn)
  "Prepend conversation history context to `turn.prompt'."
  (let ((context (superchat--conversation-context-string
                  superchat-context-message-count)))
    (when (and context (not (string-empty-p context)))
      (setf (superchat-turn-prompt turn)
            (concat context "\n\n" (superchat-turn-prompt turn)))))
  turn)
```

This reads `superchat--conversation-history` (in-memory list). It
ignores anchors. It doesn't know about session boundaries.

The v1.2 replacement:

```elisp
(defun superchat-prompt-hook--tape-view (turn)
  "Prepend an assembled tape view to `turn.prompt'.
The view is built from the current session, starting from the most
recent anchor (or session start), bounded by
`superchat-context-message-count' entries."
  (let* ((session-id (superchat-turn-session-id turn))
         (last-anchor-row
          (superchat-db-tape-last-anchor session-id))
         (view-spec (list
                     :session-id session-id
                     :since-anchor (car last-anchor-row)
                     :include-kinds '(user assistant)
                     :include-anchor-summaries t
                     :max-entries superchat-context-message-count
                     :order :chronological
                     :format :role-tagged))
         (rendered (superchat-tape-view-assemble view-spec)))
    (when (and rendered (not (string-empty-p rendered)))
      (setf (superchat-turn-prompt turn)
            (concat rendered "\n\n" (superchat-turn-prompt turn)))))
  turn)
```

**Migration policy**: ship `--tape-view` alongside the existing
`--conversation-history` hook, behind a defcustom:

```elisp
(defcustom superchat-context-source 'in-memory
  "Where the conversation-history hook reads from.
- 'in-memory: use the volatile superchat--conversation-history list.
              Fastest; what every v1.0 user has today.
- 'tape-view: assemble a view from the tape table. Survives Emacs
              restarts, respects anchors, can express selective
              context.")
```

When `superchat-context-source` is `'tape-view`,
`--conversation-history` becomes a no-op and `--tape-view` runs in
its place. We do NOT remove `--conversation-history` for a release
cycle. Users who set the new value opt into the tape-based
reconstruction; users who don't see no behavior change.

### Why views matter beyond replacing history

Views are *parametrizable*. The auto-recall path that today calls
`superchat-db-memory-search-simple` could instead build a view that
combines:

1. The chronological tail of the current session since last anchor.
2. The `content` of every anchor written in the last N hours
   (cross-phase summary).
3. FTS5 hits on `memory` table as **annotation**, not main course.
4. (v1.3) Topic-thread: detect that the user's `clean-input` reuses
   terminology from a prior anchor's `summary`, and surface the
   tape entries cited by that anchor's `source_ids`.

Each item is a separate view spec; they get concat'd. A future
`superchat-prompt-hook--memory-context` rewrite reads this as
"build a recall view," not "FTS top 5."

### Test plan (v1.2 — Views portion)

Create `test/test-tape-views.el`:

- `view/empty-session-returns-empty-string`
- `view/single-session-renders-role-tagged-chronological`
- `view/since-anchor-skips-prior-entries`
- `view/since-anchor-includes-the-anchor-itself-as-summary-line`
- `view/include-kinds-filters-out-tool-rows`
- `view/extra-entries-pins-rows-outside-window`
- `view/max-entries-truncates-from-the-oldest`
- `view/format-raw-strips-role-labels`
- `prompt-hook/tape-view-replaces-conversation-history-when-opted-in`
- `prompt-hook/in-memory-stays-default-on-fresh-config`

---

## Mechanism 3 — Handoffs

### Conceptual definition (in superchat terms)

A **handoff** is an anchor with `phase: "handoff"` plus the
following invariants:

1. `summary` is a *self-contained* statement of what's done. A
   downstream phase can read just `summary` and proceed; reading
   the source entries is optional.
2. `source_ids` lists exactly the tape entries that produced the
   summary. Not a recent window; a curated causal subset.
3. The next read after this handoff treats this anchor's `id + 1`
   as the new origin. View assemblers that see a recent
   `phase: "handoff"` anchor will not look further back unless
   explicitly told to.

A handoff is to an anchor what an anchor is to a system message:
same data shape, stricter contract.

### Where handoffs appear

| Site                                  | What's in the handoff                                               |
|---------------------------------------|---------------------------------------------------------------------|
| End of a `>skill workflow` execution  | `summary` = workflow's final assistant output, condensed to ≤200 chars; `source_ids` = each step's primary tape entry. |
| User runs `/handoff phase summary`    | Explicit. User supplies both fields.                                |
| Future: agent-loop boundary           | Out of scope until v1.3+.                                           |

### Public API

In `superchat-db.el`:

```elisp
(cl-defun superchat-db-tape-handoff-append
    (session-id summary &key source-ids extra)
  "Append a handoff anchor. Equivalent to
`superchat-db-tape-anchor-append' with phase=\"handoff\",
owner=\"system\" (or \"user\" if called from /handoff),
and an enforced non-empty summary. Returns the new row id.")
```

In `superchat-dispatcher.el`:

```elisp
;; /handoff phase summary
;; Calls superchat-db-tape-handoff-append with owner="user"
;; and created_by="explicit". phase is currently ignored by
;; the DB but recorded in meta for future use.
```

In `superchat-workflow.el`:

```elisp
;; superchat-workflow-execute, after the last step succeeds:
(superchat-db-tape-handoff-append
 (superchat-turn-session-id last-step-turn)
 (superchat--summarize-workflow-result final-result)
 :source-ids step-tape-ids
 :extra `(:skill-name ,(plist-get skill-plist :name)))
```

`superchat--summarize-workflow-result` is the only new helper —
takes the workflow's final result string, truncates to 200 chars,
single-line. Not LLM-summarized in v1.2; that comes later.

### How views consume handoffs

A view assembler asked to build context for a turn in
`session_id=S`:

1. Find the most recent anchor for `S`. If it's a handoff:
   2a. Render the handoff's `summary` as the leading line:
       `[Phase: handoff (skill-name=git-commit-message)] <summary>`
   2b. Include only entries with `id > handoff.id`.
   2c. Do NOT walk back further unless `:include-pre-handoff t`.
2. If the most recent anchor is non-handoff: behave as in
   Mechanism 2 (use it as the `since` cutoff, include its
   `content` as a summary line).
3. If no anchor: full session tail up to `max-entries`.

### Why handoffs are not just anchors

If the rule were "treat anchors as cutoffs," handoffs would be
redundant. We don't:

- A `topic-switch` anchor does NOT prevent the view from including
  pre-anchor entries — it's a phase marker, not a privacy boundary.
- A `handoff` anchor DOES prevent that — the contract says
  "everything you need is in `summary` and `source_ids`."

This matters for workflows. After a workflow's handoff anchor, the
next chat turn shouldn't drag the workflow's 8 intermediate step
exchanges into the prompt. The handoff's one-line summary is
enough.

### Test plan (v1.2 — Handoffs portion)

Create `test/test-tape-handoffs.el`:

- `handoff/append-stores-phase-as-handoff`
- `handoff/append-rejects-empty-summary`
- `handoff/workflow-execute-writes-handoff-on-success`
- `handoff/workflow-execute-skips-handoff-on-error`
- `handoff/cmd-handoff-records-explicit-summary`
- `view/handoff-anchor-blocks-pre-handoff-entries`
- `view/handoff-anchor-renders-summary-as-first-line`
- `view/include-pre-handoff-flag-overrides-block`
- `view/non-handoff-anchor-does-not-block`

---

## Phasing and rollout

### v1.1 — anchors only

Ships:
- `superchat-db-tape-anchor-append` + 3 new reader helpers.
- `/anchor` command in `superchat-dispatcher.el`.
- `superchat-anchor-auto-write-enabled` defcustom, default `nil`.
- Auto-write call sites in `superchat-skills.el` and
  `superchat-workflow.el`, gated by the defcustom.
- `test/test-tape-anchors.el` (~13 tests).
- README + architecture doc updates explaining the new command.

Risk: minimal. No reader change. No behavior change for users who
don't run `/anchor` or flip the defcustom.

### v1.2 — views + handoffs

Ships:
- `superchat-tape-view-assemble` + view-spec contract.
- `superchat-prompt-hook--tape-view` registered alongside
  `--conversation-history`.
- `superchat-context-source` defcustom: `'in-memory` (default) or
  `'tape-view`.
- `superchat-db-tape-handoff-append`.
- `/handoff` command.
- Handoff write at end of successful workflow execution.
- View assembler treats `phase: "handoff"` anchors as cutoffs.
- `test/test-tape-views.el` + `test/test-tape-handoffs.el`
  (~19 tests combined).

Risk: real, because the reader path can regress conversation
context. Mitigation: defcustom default stays `'in-memory`. v1.2
users opt in to `'tape-view`; we collect feedback before deciding
v1.3 default flip.

### v1.3+ (not designed here)

- Topic-thread detection: user's `clean-input` shares vocabulary
  with a prior anchor's `summary` → surface that anchor's
  `source_ids`.
- Cross-session views: a single view spec spanning multiple
  `session_id`s.
- LLM-summarized workflow handoffs (today: truncate to 200 chars;
  future: ask an LLM to write a 1-sentence summary).
- Anchor decay / archival.
- User-extensible phase taxonomy.

---

## Out of scope (do not design here)

- Replacing memory `recall` (FTS5 over `memory` table). That layer
  is content-addressable lookup; views are session-scoped
  reconstruction. They coexist.
- Schema migration. Mechanism 1 needs none; Mechanisms 2 and 3 are
  pure-elisp on top of the existing schema.
- Multi-agent ownership beyond the `owner` field. tape.systems'
  multi-agent shared-tape model is interesting but assumes external
  coordination superchat doesn't have.
- UI for browsing anchors / handoffs (`M-x superchat-anchor-list`
  or similar). v1.3.

## Open questions for the user before writing v1.1 handoff

1. **Phase taxonomy lockdown**: keep the 8 phase values listed
   above? Add `"checkpoint"` for explicit save-points (separate
   from topic-switch)?

2. **`/anchor` syntax**: `/anchor [phase] [summary]` or
   `/anchor "summary" --phase=topic-switch`? Pick one before writing
   the parser.

3. **Auto-write default**: confirm `superchat-anchor-auto-write-enabled`
   defaults to `nil`. Some users will expect skill boundaries to
   auto-anchor without configuration.

4. **`/handoff` and `/anchor` overlap**: should `/anchor handoff foo`
   be equivalent to `/handoff foo`, or should `/anchor` reject the
   `"handoff"` phase to keep the contract clean?

5. **`source_ids` for `/anchor`**: when a user types `/anchor` mid-
   conversation, what does `source_ids` contain? Options:
   (a) empty — user-initiated, no causal trace;
   (b) the prior user turn's tape id only;
   (c) all tape entries since the last anchor.
   Pick one.

These five questions block writing the v1.1 handoff. The other
implementation choices are decided in this doc.
