# Decision: Missing provider results fail the Run

Status: implemented — execution reliability work under the user's delegation to complete the remaining development

## Problem

The native llm.el success callback may deliver nil when the provider result contains neither text nor a tool result. Superchat previously generated `[Empty response from llm]` and used the default completed outcome. That incorrectly lets an absent answer satisfy consumers that rely on the recorded Run terminal.

The final configuration-task chat undo on frozen 5291b745 demonstrates this callback value and product fallback. Its raw HTTP response body is unavailable, so the upstream reason for the missing result is unknown. The fix does not infer a model, context-window, network or permission cause.

## Decision

At the existing final-response boundary, after context recovery, tool continuation and pending steering have been handled, require either a string response, a string `:text` value, or visible text accumulated in the current provider attempt. If all are absent, finish through the existing failed path with `[Missing final response from llm]`.

An explicit empty string remains a supplied response and retains its existing empty-display fallback and completion policy. Visible current-attempt streaming text may still supply the final body when the final callback itself is nil. Narration from an earlier tool round cannot supply that evidence: the existing continuation clears those parts. Reasoning or tool progress alone is not a final answer.

Tool-only results still take the tool continuation branch. A withdrawn or cancelled Run ignores late callbacks. The change introduces no new terminal status, automatic retry, tool call or synthetic model content, and does not alter the Runtime rule for unsettled Effects or Approvals.

## Consequences and verification

The [focused response regression](../../../scripts/m5-response-regression.el) distinguishes missing initial and follow-up responses from explicit empty strings, current-attempt text, legal tool rounds and cancellation. It exercises the real native adapter and Runtime with scripted provider output.

The previous real task remains a failed acceptance on its frozen version; its original recorded completed terminal is not rewritten. Detecting missing output accurately does not repair incorrect Lisp, unintended file changes or an inaccurate model answer. Those remain separate task postconditions.
