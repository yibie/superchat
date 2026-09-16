# Decision: a chat Turn's attachments are recorded when it is admitted

Status: accepted — the user chose this over carrying the obligation with the Run on 2026-09-16 ("新方案"), after the first description of the defect was corrected by measurement; implementation state is in [the roadmap](../../roadmap.md#milestones)

## Problem

`chat-turn-recorder` writes a chat Turn's `attachment.added` facts from a
`run-sealing` listener (`superchat.el:4630`). Rows mount into the global Scope,
so unmounting the row removes the listener, and `:required t` means "a failure of
this observer is recorded", not "its absence is an error"
(`superchat-event.el:93-98`): a listener that is gone is simply not called, with
no error and no `audit-incomplete`. M4 made composition changes ordinary, so a
profile switch while a Run is in flight now reaches this.

Measured on a scripted fixture, not inferred:

- A text-only chat Turn is unaffected. The Tape carries the same events and the
  Session history the same rows whether the row is mounted at the seal or not,
  because Runtime appends `intent.received` at admission and the seal-time hook
  only reuses that row's id.
- A Turn carrying an image loses its `attachment.added` fact. The image is sent
  to the model and nothing records that it was.

That last sentence is the failure class M3 treats as an audit break everywhere
else — material sent without a record, pinned by mutant M9 and by
`m3/inject/material-is-recorded-before-dispatch`, which requires the injected
path to write `attachment.added` between `step.started` and
`request.message-list`: before the material is sent, not after the answer.

The chat Turn is inconsistent with itself. Its text is recorded at admission and
cannot be lost; its images are recorded at the seal and can be.

## Decision

A chat Turn's attachment facts are written where its intent is written: when
Runtime admits the request, next to `intent.received`, and no longer at the
seal. Recording that material was sent is part of sending it, so it does not
depend on a user-interface row being mounted when the Run ends.

The chat layer, which already computes each image's descriptor to build the
request, carries those descriptors on the envelope; Runtime appends one
`attachment.added` per descriptor, under the Turn identity it has just created.
The descriptors are data, not a deferred obligation: nothing has to survive a
composition change, because nothing is deferred.

`chat-turn-recorder` keeps `record-user-message`, which writes the View's
in-memory history row and is harmless to lose.

The Turn id is Runtime's, not the chat layer's: `superchat-runtime--turn-from-request`
mints a fresh Turn for every admitted request, so the chat layer cannot write a
correct `:turn-id` before submitting. This is why the descriptors travel and the
event is written on the Runtime side.

## Alternatives considered

**The obligation travels with the Run**, as the Sink does: the envelope carries
the recording function and Runtime honours what it captured at submit. The user
chose this first, on the strength of a defect description that measurement then
narrowed. Rejected once the defect was measured: everything at risk is already
known at submit, so carrying data is enough and carrying behaviour is not
needed.

**Keep `chat-turn-recorder` mounted in `headless`.** Rejected on inspection: its
hooks read the buffer-local `superchat--session-id` and write the View's
in-memory history as well as the Tape, so with no chat View current the
`(null superchat--session-id)` branch would start recording headless-native Runs
that are not recorded today.

**Make the loss loud**: record a fact when a seal-time recorder that existed at
submit is gone. Rejected: the material would still have been sent without a
record of what it was.

## Consequences

For a chat Turn with images, `attachment.added` now appears before `run.started`
instead of after `response.completed`. Nothing reads the ordering of that event
today; the ordering assertion in `m3/inject/material-is-recorded-before-dispatch`
belongs to the injected-material path, which is a different writer
(`superchat-runtime--attachment-event`, actor `runtime`, topic `inject`) and is
unchanged.

A Turn that is queued, steered or dropped before Runtime admits it records
neither its intent nor its attachments, exactly as today. A Turn that is admitted
and then cancelled records both, which is what `intent.received` already does.

This decision does not change what is recorded for a Turn whose row is mounted
throughout: exactly one `attachment.added` per image, with the same payload.
