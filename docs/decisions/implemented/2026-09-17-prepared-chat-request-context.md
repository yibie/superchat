# Prepare inspected chat context once

Status: implemented

Superseded in part (2026-09-25): history is no longer part of the editable body. The earlier turns are their own messages ([earlier turns reach the model as real messages](2026-09-25-earlier-turns-as-real-tool-messages.md)), shown read-only in the inspector, so an edited body is sent with the same history selection as an unedited one; attachments and source selections deleted from the body still stay deleted.

## Decision

The act entry and its optional inspector collect context once. The request plan keeps the original system configuration separate from the assembled display text, and carries the collected system fragments and exact assembled user body to submission. The driver preserves that body, including explicit inspector edits, and applies the normal hard-content budget. The [request lifecycle](../../protocols/request-lifecycle.md#submission-and-preparation) owns the current behavior and its evidence boundary.

The defect had two effects. An assembled system preview was reused as the original system configuration, so submission appended the same system collectors again. The assembled body was also treated as a bare chat-input override, so history and attachments were reinserted even after the user deleted them in the inspector. Origin alone already had a special guard, which left the other contributions unprotected.

Preview and submission share base-fragment composition. Preview uses the same required-listener error boundary and Scope checks as submission. Collected system fragments retain their process-local tool renderers; this adds no serialized closures or new tool permission. An initial over-budget preview fails instead of becoming a sendable placeholder.

## Alternatives considered

**Remove duplicate text after assembly.** Equal text can be independent input from the user and a collector. Text equality does not establish that a contribution has already been assembled, and cannot express an intentional deletion.

**Add a guard to every built-in collector.** That leaves plugin contributions and collector side effects outside the contract. A prepared request carries the result of its one collection, while ordinary chat and prompt-skill input continue collecting normally.

**Collect again and filter the resulting body fragments.** Re-reading files and rebuilding history already changes the request's source bookkeeping before filtering. Reusing the prepared context avoids those effects.

**Reuse the complete displayed system string as an opaque base.** Pure tool-dependent fragments must remain separately available to the existing tool-rewrite path. The raw base, display string and collected fragments have distinct roles.

**Keep the original history receipt after arbitrary inspector edits.** The old selection no longer proves what the edited body contains. The original preview remains on the plan, but an edited body does not pass that receipt to Runtime or use it as a history replacement target.
