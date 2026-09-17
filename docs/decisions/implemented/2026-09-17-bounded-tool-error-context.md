# Bounded evaluation diagnostics and one error body

Status: implemented

## Decision

Native eval_elisp errors print objects with a sequence length of 12, depth of 4 and circular printing enabled. Diagnostic details are limited to 1024 characters including an explicit truncation marker when character clipping is needed; the fixed Error: evaluating expression: prefix is additional. This bounds model-visible diagnostics, not execution or effects, and does not change successful evaluation values.

The shared corrective message names failed tools and points to their preceding tool results. It does not copy the error bodies into another user-role message. Error and refusal classification still read the original tool results, so a genuine refusal keeps its no-retry instruction. The tools component describes [the result contract](../../components/superchat-tools.md#built-in-edit-and-evaluation-results).

A Lisp type error can include an entire keymap even when the requested operation and useful diagnosis are small. Repeating that object in both a tool result and a correction message spends context on the same failure twice. The retained error category and bounded details let the model repair the call; callers can query specific state if they need more information. This change does not claim to make the model choose a correct operation or stop it repeating a wrong one.

## Alternatives considered

**Increase the context window or tool budget.** That gives duplicated errors more room without improving the diagnostic information. Existing execution budgets retain their own meanings.

**Remove the error details altogether.** The model needs the actual condition to correct the call. One bounded error body and a short corrective instruction retain that distinction.

**Truncate every successful tool result.** Successful file, buffer and document reads have separate content and completeness requirements. This decision bounds native eval errors and removes repeated feedback text; it does not add a general tool-output truncation protocol.

**Keep the complete error in the correction message only.** The tool result is the native record of the failed call. Moving its evidence into a user message would confuse those roles and make prompt reconstruction depend on a duplicate representation.
