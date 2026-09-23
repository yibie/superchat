# Decision: request evidence includes material a Step inserted

Status: accepted (user, 2026-09-23: "3. 按照建议", choosing to fix `.scratch/issues/2026-09-23-request-evidence-omits-step-claimed-material.md`).

## Problem

M1 promises that every provider request can be rebuilt from the Tape ([the request-header decision](../implemented/2026-09-13-reconstructable-provider-request.md)). A request that carried material a Step claimed mid-Run — a user's steer, an injection, the finish-check note — is not: `request.message-list` names only the history the Turn was prepared with, and `superchat-tape-request-evidence` rebuilds an `attachment.added` row from its content ("Injected model material") instead of its payload text. The check still reports `reproduced`.

## Decision

1. Each request's `request.message-list` names, in the order the request carried them, the admitted material its Steps inserted so far in the Run: `(:kind "step-material" :storage-id ATTACHMENT-ROW-ID)`.
2. `superchat-tape-request-evidence` rebuilds such an item from the `attachment.added` payload's text, exactly as the request carried it (including the `[Injected material]` framing if that is what the provider received), and reports `unknown` when the row is missing, its payload was omitted, or it is not an admitted material row. It never falls back to the row's summary content.
3. Nothing else changes: history items, tool-observation items, the header and the invariant keep their meaning.

## Acceptance

Scripted transport: a steered Run's continuation request rebuilds with the steer's text; the finish-check note appears in the rebuilt continuation (the expected-failure test `finish-check/request-evidence-omits-step-material` passes and loses its marker); a missing or omitted material row makes the rebuild `unknown`; requests without Step material rebuild exactly as before; M3's evidence tests and the canonical smoke stay green.

## Scope recorded at implementation (2026-09-23)

The rebuild rule of point 2 also covers a later turn's history item that names an admitted material row. The leader's probe showed such an item rebuilding as `(:kind "system" :content "Injected model material")` while the provider had received `User: [Injected material] TEXT` in the history fragment; that is the second fact of the Problem above, not a new decision. The item shapes, the two provider-visible forms (`prompt-block` for the first Step, `message` for later Steps) and the `unknown` conditions are written into [the continuity contract](../../design/context-continuity-contract-v0.md) ("Amendment 2026-09-23: material a Step inserted"). Found while preparing this slice and registered separately, not fixed here: all attempts of a Run share one request id, so the public rebuild reaches only the last attempt (`.scratch/issues/2026-09-23-request-evidence-rebuilds-only-the-last-attempt.md`).
