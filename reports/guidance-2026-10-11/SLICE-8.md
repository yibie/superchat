# Slice 8: fixes from the architecture map (user approved 2026-10-13)

Source: reports/architecture-map-2026-10-13/CONTRADICTIONS.md and SUMMARY.md
(read-only walk by a second agent; the steering architect verified C11, C9,
C21, C20, C4, C6 on HEAD b4f0548f). Same discipline as slices 5-7: red test
first naming the assertion, mutation check, full check, stop before commit.
Four commits, in this order.

## 8a. C11 — the brake and /work resume (defect from b3391425)
- Today: the brake pauses the record and appends work.waiting :for user, then
  says "/work resume goes on"; /work resume refuses anything not continuable
  (superchat-work.el:1439-1455, 1519-1523). Slash commands skip the
  user-input hook that clears a user wait (superchat-dispatcher.el:1485-1492).
- Fix: /work resume on a record waiting for the user clears the wait (same
  fact the ordinary-message path writes) and then continues. Keep the brake's
  wait fact: it is the right durable state. Message text unchanged.
- Red tests: brake -> /work resume continues (today: user-error); the wait
  is cleared with the same fact the message path writes; a record that is
  done or waiting for a time is still refused.
- Note in the 2026-10-12 rounds-end-when-they-get-nowhere decision.

## 8b. C9 — capability probe calls a misspelled resolver
- superchat-transport-llm.el:30-36 calls superchat--llm-resolve-streaming;
  the function is superchat-llm--resolve-streaming (superchat-llm.el:378).
  ignore-errors hides it, so the Service always reports no streaming.
- Fix the name. Red test: the capability Service reports streaming for a
  provider the resolver says streams (today: nil). Add a byte-compile check
  or a test that the probed symbol is fboundp so a rename cannot hide again.

## 8c. Synthesized tools — one decision, five contradictions (C1 C2 C4 C5 C7, and C3 wording)
Write docs/decisions/accepted/2026-10-13-a-synthesized-tool-is-a-definition.md
with these rulings (user agreed to the architect's recommendations):
1. The registry stores definitions, never wrapped callables. Every Run wraps
   at admission, exactly as the disk-restore path already does
   (superchat-synthesis.el:216-224 vs the rehydrate path). This closes C1:
   a tool defined in one Run is usable in later Runs and later Sessions.
2. Visibility: the registry is per Workspace, not process-global; the
   docstring "per Session" is replaced by the true scope (C2). Subagent
   prompt-local synthesis must not replace a name another Session holds (C7):
   namespace child definitions by Run or refuse a clash.
3. The ledger records what actually happened: "approved by user at hash" only
   when a person saw the source; under danger-full-access or auto record
   "admitted without review" and do NOT auto-restore those silently on
   restart (C4). Restore asks once, or lists them in superchat-pending.
4. Wording: the tool description says "each call is admitted under this
   conversation's permission level" instead of "every call asks" (C3);
   "the ledger is outside SQLite" claim removed, it is not a protection
   against eval_elisp (C5).
5. A refusal from the inner wrapper is a tool failure, never a success
   result (the 11 Oct incident); assert in a test that a blocked call ends
   with tool.failed and the model sees the reason.
Red tests per ruling. Update superchat-tools.el:1885-1888 description,
synthesized tool docstrings, README line on define_tool.

## 8d. Documentation batch (C8 C10 C14 C15 C19 C21 C22 C23 C24 C25 C18 C20-wording)
One commit, no behaviour change except C20: emacsclient cancel returns the
actual terminal status from the Runtime instead of a fixed "cancelled"
(superchat-emacsclient.el:194-230). Each doc fix cites the contradiction id.
C21: README 342 and 500 (and README_cn) must say one thing; roadmap is the
home. C19: README 162 and superchat-pending-items docstring: reading writes
nothing; refresh checkpoints and y/n/a actions do. C6: the Allow-all panel
text says "this conversation no longer asks for tools; saving still asks".

## Not in this slice (user to decide later)
C12, C13, C16, C17: conditional hazards, no reproduction. List them in the
roadmap's open observations with the map as source.
