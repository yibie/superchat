# Decision: markdown rendering is a function, not a Service

Status: accepted — the user chose the full chain on 2026-09-16 ("顺着链子走到底") after issue 03 was measured; implementation state is in [the roadmap](../../roadmap.md#milestones)

## Problem

Registered as issue 03 while fixing the two M4 follow-ups. `superchat-inline.el`
rendered an inline answer like this:

```elisp
(if (fboundp 'superchat-render-markdown-to-org)
    (superchat-service-call nil 'view 'markdown-to-org (or text ""))
  …)
```

It tested whether the *function* exists and then called through the *Service*.
The function is always there — `superchat-render.el` is eager-required — but the
Service is provided by the `chat-view` row, so a profile that mounts the inline
rows without `chat-view` made inline answers fail inside, after their own row
guard had passed. Measured:

```
with chat-view:    "*hi*"
without chat-view: superchat-service: no provider for view
```

The shipped profiles do not reach it (`default` mounts both, `headless` disables
both), but composing a profile is exactly what M4 invites, and the roadmap's own
M4 acceptance says removing a row leaves the rest working.

Inline was also the only caller going through the Service. `superchat-render.el`
itself, `superchat.el` and the Telega plugin — the only complete surface plugin
before M4, and the precedent that settled S2's `:inject` question — all call
`superchat-render-markdown-to-org` directly.

## Decision

Inline calls the function directly, like every other caller. With that, the
`view` Service has no consumers and is deleted, and the `chat-view` row, whose
`:apply` provided nothing else, is deleted with it.

Markdown rendering is a plain function. This is the same conclusion the
[attachment decision](2026-09-16-chat-attachments-recorded-at-admission.md)
reached for the chat Sink, which the dispatcher builds directly rather than
fetching from a row at delivery: a thing every surface needs, that cannot
meaningfully be swapped per composition, does not earn a Service.

## Alternatives considered

**Fix inline alone and keep the Service.** Rejected by the user: it leaves a
Service with no callers and a row that exists only to provide it — the same kind
of stub that was removed from `chat-turn-recorder` the same day.

**Make inline degrade when the Service is absent**, falling back to raw text.
Rejected: it preserves an intent — rendering is swappable per composition — that
nothing in the product actually uses, at the cost of splitting inline from every
other caller.

**Declare the dependency** by having the inline rows `:inject 'view`. Rejected:
it couples the inline surface to the chat window's row for a text helper, which
is the coupling S2 removed from these very rows.

## Consequences

The default profile is 29 rows. `headless` disables one fewer. Rendering is no
longer something a composition can remove, which is a narrowing of what a
profile can express: if a future surface wants its own renderer, it needs a new
Service and a new decision, not a revived `view`.

`m4/in-flight/a-live-run-keeps-rendering-after-its-view-row-leaves` loses its
subject. The property it pinned — that a Run keeps rendering when the
composition changes under it — is repointed at a `headless` switch, which
unmounts every chat row at once and is the case a user can actually reach.

The historical records in `.wayfinder/` still describe `chat-view` and the `view`
Service as they were; they are records of the migration, not descriptions of the
tree, and are left alone.
