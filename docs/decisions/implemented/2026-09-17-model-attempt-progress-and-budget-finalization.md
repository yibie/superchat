# Decision: Model progress, explicit reasoning and tool budget finalization

Status: implemented — execution reliability work under the user's delegation to continue the remaining development to completion

## Problem

A multi-request Emacs task needs separate answers to three questions: whether the current provider request is making progress, which reasoning setting the provider actually receives, and why tool execution ends. A Run's final answer alone cannot establish that a configuration change is saved, loaded or effective.

The [M5 comparison](../../roadmap.md#m5--operating-context-and-reference-reading-accepted-in-progress) exposes these distinctions. The reviewed traces locate the six T3 wait expirations about 180 seconds after the first request, while their last requests have run for only 2–15 seconds. The evidence does not establish that those last requests need more than 180 seconds for a first token. The compatibility serializer also omits Qwen's automatic reasoning field, and exhausting the tool budget does not establish completion of the requested work.

## Decision

The [request lifecycle contract](../../protocols/request-lifecycle.md#model-progress-and-tool-budgets) separates per-attempt silence from the aggregate transport hard limit and Runtime's Run deadline. Native provider dispatch owns the silence timer. New text, reasoning or tool progress refreshes it; duplicates do not. Identity checks cover both queued timer callbacks and reentry while cancelling or replacing a timer, so a retired attempt cannot change the next attempt's timer, response or model phase. Human input retains its explicit clock suspension contract.

Reasoning defaults to the provider's setting. An explicitly requested effort on the exact `llm-openai-compatible` base provider requires a declared endpoint/model mapping or an explicit raw reasoning parameter. An unavailable mapping produces an actionable error before dispatch. This makes configuration a serialization declaration instead of a claim based on a model name. Native providers and specialized compatible subtypes retain llm.el's mapping. Explicit raw settings retain precedence; automatic mapping does not mutate the shared provider.

Tool budget finalization states the actual boundary and asks for completed, incomplete and unverified work. The existing interactive choice grants another full budget or requests a final answer without tools; batch mode finalizes. Runtime records the optional `:tool-budget` metadata and gives it to the original Sink. The View displays a separate notice, including after Session reopen. Neither the model's response bytes nor existing terminal statuses are repurposed. Literal tool-call text remains text; a structured call after tools are withdrawn cannot execute.

## Alternatives considered

**Increase the wait timeout.** This hides a clock ownership error and makes stalled attempts take longer to detect. It does not explain why a preamble changes the lifetime of all later requests. The existing configurable values remain available for a genuinely slow provider.

**Increase or remove the default tool round limit.** A larger number is not evidence that the model reaches the right buffer, writes valid Lisp or verifies persistence. The default interactive extension already permits more work; the aggregate hard limit and explicit stop/finalize policies remain meaningful. This change makes their operation and recorded outcome accurate rather than selecting a new universal number.

**Send one reasoning field to every provider.** Chat Completions, Responses and native Ollama use different schemas. An exact declaration and existing raw configuration preserve that distinction. Server metadata for a native API does not establish the spelling accepted by another endpoint.

**Keep automatic `none` as the global default and reject every unmapped compatible provider.** That makes ordinary unconfigured chat fail before the user chooses an effort. The default is `nil`, which delegates honestly to the provider. A user or Preset explicitly choosing `none` retains that request and receives a clear unsupported result if it cannot be mapped; saved values are not rewritten.

**Append a product footer to the model response or introduce a new completed status.** A footer changes response identity and contaminates inline, workflow and other consumers. A new terminal status changes unrelated delivery contracts. Optional metadata and a View notice preserve those contracts while keeping the budget decision observable.

**Infer the budget from the last attempt's phase.** A phase is process-local execution state, and ordinary request evidence does not provide a durable budget decision for a reopened View. Runtime retains the explicit decision, including if the final request subsequently fails or is cancelled.

## Consequences

The [configuration guide](../../../README.md#reasoning-settings) explains provider defaults, explicit Preset inheritance and compatible endpoint mappings. Existing users who explicitly set an unmapped effort need to declare its protocol or choose provider defaults. An explicit raw parameter overrides automatic effort selection, so it is unsuitable for users who expect Presets to vary that setting. Successful request serialization is evidence of the requested value, not proof that an arbitrary server honors it.

The focused [execution regression](../../../scripts/m5-execution-regression.el) covers real request construction, scripted tool execution through Authority and Runtime, fake-clock counterexamples and actual Emacs timer scheduling. The existing leader-owned M3 and M5 sets remain unchanged. Scripted correctness and a particular model's success on an Emacs task remain separate evidence; the reference reader and operating guide remain proposals in the roadmap.

## Amendment 2026-09-29: no round limit by default

The default `superchat-llm-max-tool-rounds` is now nil ([no tool round limit by default](../accepted/2026-09-29-no-tool-round-limit-by-default.md)); this decision's refusal to change the default is superseded. Its budget finalization, notice and record stand for anyone who sets a limit.
