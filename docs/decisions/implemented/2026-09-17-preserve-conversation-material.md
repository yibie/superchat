# Preserve original conversation material on Tape

Status: implemented

## Decision

The content of intent.received and response.completed is conversation material. Tape persists the original text supplied by Runtime for those two event types, including an empty string. Runtime supplies user intent at admission and the final or partial response before Sink delivery and display rendering. Their existing event identity and content size/hash metadata remain unchanged.

The 500-display-column summary remains a presentation of structural events. It does not replace user or assistant text in the material row. Tool-event redaction and bounded summaries are unchanged. This uses the existing row and storage path; it introduces no second conversation history, duplicate material event or database schema.

Model context continues to come from the single history derivation and the [Context View contract](../../design/context-continuity-contract-v0.md). Its finite budget either includes a material item intact or omits the item with a receipt. Preserving full material does not force all history into every request. Inspection can still remove history under the [prepared-request contract](2026-09-17-prepared-chat-request-context.md).

## Evidence

The full configuration acceptance exposed the difference between capturing a complete answer and making it available to the next Run. One undo request received a 393-character prefix ending in an ellipsis, even though the complete previous answer and its correct hash were available to the UI and the independent answer observer. Tape's generic writer had applied the display-summary function to the assistant material row. The same writer also clipped long user intent.

This is a storage loss before Context View selection, not budget-driven compaction. Explicit raw recall and Session reopen cannot recover text that was never persisted. The [focused regression](../../../scripts/m5-history-material-regression.el) exercises the actual storage and subsequent provider path; the existing leader-owned regression sets remain unchanged.

This finding does not prove that a particular wrong undo was caused solely by missing history. The original task, retained prefix and observed old binding must still be considered. It does establish that the product was not providing the original conversation material required to assess that behavior fairly.

## Existing Sessions

The change preserves new material. Old rows containing only a clipped summary and a full-text fingerprint cannot be reconstructed from the fingerprint. They are not silently expanded, rewritten or replaced from the current UI cache. Existing compaction Anchors remain explicitly lossy interpretations with their own provenance; this fix does not turn them into original material.
