# Decision: Ordered system contributions survive request rewrites

Status: implemented — the user chose the issue's identity-preserving design and its takeover boundary on 2026-09-17 ("按照建议处理"); no identity-preserving middleware edit operation is introduced

## Problem

`superchat-pipeline-submit` reduced every tool-dependent system fragment to an old-text/renderer pair, and `superchat--request-render-tool-guidance` refreshed a block after a tool rewrite by `string-replace` over the whole system string. Two confirmed defects follow from losing the fragment's identity and position (`.scratch/m5-request-context/issues/02-guidance-equal-text-rewrite.md`): text equal to a block inside the explicit base or another contribution was rewritten too, and a block that middleware had removed by editing the string came back on a later tool change, because the empty-old-text branch appended it. The default chat and act surfaces do not register a colliding block; custom collectors and non-default surfaces with renderers did.

The request waterfall's public boundary is an opaque `:system` string. Given base G and block G assembled as `G\n\nG`, a middleware that returns `G` may mean "keep the base, drop the block" or "drop the base, keep the block". No algorithm over the string can tell; text deduplication, first or last occurrence, or reconstructing deleted blocks from the previous assembly would each pick one silently.

## Decision

`superchat-prompt-assemble` returns `:system-contributions`, the ordered `(:id :owner :text :render)` entries of every selected system and instruction fragment, and `superchat-prompt-materialize-system` is the one join that produces `:system` from them, so the assembler and every later rewrite yield byte-identical text for the same entries. `superchat-tool-guidance-initial` turns the entries and that text into the Request's `:tool-guidance` state only when some entry has a renderer; Runtime keeps the latest state on the Run.

On every `before-provider-request` attempt, after the waterfall: a refusal or absent state passes through; a result whose `:system` differs from the last materialized text is a takeover, so Runtime clears the Run's state and returns the middleware's string as it is; an unchanged tool surface keeps the state; a changed surface rerenders each dynamic entry in place from the accepted final names, keeps static entries, materializes once in the original order and stores the new state. An entry that renders empty keeps its position and separators, exactly as an empty selected fragment does at assembly, and renders again there when a later surface restores it. Static contributions, including the raw base, are never edited, whatever their text.

The takeover is the compatibility boundary: a whole-string replacement transfers the system text to its caller and ends automatic refresh for that Run. Runtime does not search the replacement for familiar blocks, so a removed block cannot return. Middleware has no identity-preserving edit operation; editing or deleting one contribution means replacing the string and giving up automatic refresh. The [request lifecycle contract](../../protocols/request-lifecycle.md#request-rewriting-and-tool-selection) states this.

## Alternatives considered

**Keep string replacement but match only the first or last occurrence, or dedupe equal text.** Each guesses which occurrence is the block. The G/G example shows the guess is unrecoverable from the string, and a wrong guess edits the user's base silently, which is the defect.

**Reconstruct deleted contributions from the previous assembly.** The old assembly says what was there, not what the middleware meant to keep. Reviving a block a middleware deliberately removed is the second reported defect.

**A structured `:system` candidate field for middleware.** It would let middleware edit contributions by identity, but the waterfall rejects unknown fields and every existing middleware returns strings. Adding the field is a public API decision with its own validation and migration rules; this change keeps the string boundary and states its cost instead.

**Serialize contributions or renderers into the Tape.** Renderers are closures and the contributions are process-local preparation data. Provider request evidence already records the final system text of every attempt, which is the fact a reader needs.

## Consequences

Middleware that prepends text by returning a new whole-system string, and expected surviving guidance to keep refreshing, no longer gets automatic refresh after that attempt; the string it returned is sent as it is on that attempt and offered again as the candidate on the next. No production middleware in this repository rewrites `:system`. `scripts/m5-guidance-regression.el` verifies the per-attempt provider text through the real driver, Runtime and waterfall, including the base collision, equal-text blocks, the empty-then-nonempty position, the takeover, the removed block and the accepted final names a renderer receives. The existing M3 tool-guidance test and the request-context inspected-send test keep passing, and the leader-owned M3 and M5 sets are unchanged.
