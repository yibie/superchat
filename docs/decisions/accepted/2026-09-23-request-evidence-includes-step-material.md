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
