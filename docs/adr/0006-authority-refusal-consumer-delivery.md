---
status: accepted
scope: public superchat-tool-execute refusal delivery
applies_when: blocked, policy-denied, declined-approval or admitted run-inactive refusal reaches its consumer
source Evidence ID: E-20260906-007
verification: independent crc implementation PASS; independent ckr Kernel/ADR text ACCEPTABLE, leader accepted
supersedes: K31 unresolved public-refusal consumer-error redelivery exception
---

# Preserve the consumer condition after one Authority refusal delivery

User approved option A: public Authority refusal delivery attempts the async
consumer once and propagates its original error/quit symbol and data. The local
guard is published before delivery; the public caller supplies its refusal closure
to the private invoke liveness branch. Gate1 retains its existing external path.

This is breaking for custom async consumers whose errors previously disappeared
or were redelivered as audit failures. Such callers must catch/handle their own
conditions at their boundary if isolation is needed. There is no new context key,
registry, or alternative Runtime authority. Release selection is not part of this
record. The omitted optional private argument keeps its existing path; this is
not a guarantee for arbitrary private invocations.

Successful async consumer-error swallowing, implementation-failure delivery,
true audit-failure routing, legitimate refusal audit, approval obligations and
Runtime lifecycle remain unchanged. Choosing to swallow every callback error or
to terminate a Run automatically would be a different decision, not this migration.
Actual aggregate-receiver observations do not establish a general recovery rule.
