# Agent Trajectory and Trace Design for Self-Learning Systems

> Research report — August 2026  
> Context: superchat tape system (150–300KB Elisp traces per run)

---

## 1. Design Principles

### 1.1 The Trace as the Primary Artifact

The central thesis from current research (2025–2026): **the execution trace is no longer a debug log — it is the working surface** for evaluation, failure attribution, compliance, and self-improvement loops [agent-engineering.ch, 2026-05-02].

Three converging forces make this true:

1. **Agents got long.** A 40-step trajectory with five tool calls, a planner-observer split, and memory writes cannot be judged by its final output. The same answer can be produced by a competent run or a lucky cascade of compensating errors. MASEval and TraceElephant score *trajectories* rather than outputs [TraceElephant, ACL 2026].

2. **Multi-agent systems break local observability.** The Context-Fragmented Violations paper shows violations only appear in the *joined trace across agents*. Each agent's local trace looks fine in isolation.

3. **Self-improvement needs durable trace data.** Socratic-SWE, TRACE, and Hermes Agent all distill traces into structured skill registries. The trace becomes training data, eval data, and audit data simultaneously.

### 1.2 The C.A.S.E. Evidence Model

The minimum useful trace must capture four layers [AgentEngineering.org, 2026-03-23]:

| Layer | Content | Why |
|-------|---------|-----|
| **C**ontext | Run/session id, agent identity, model, config version, memory state marker | Correlate across components |
| **A**ctions | Messages, plans, tool calls, tool results, handoffs, approval events | Reconstruct what happened |
| **S**ignals | Latency per step, token usage, cost, retries, errors, timeouts, status changes | Make trace *operational*, not just descriptive |
| **E**nforcement | Guardrail checks, permission checks, execution-boundary decisions, approval gates, policy assertions | See what the system allowed, denied, or escalated |

### 1.3 Key Design Axioms

From the surveyed literature, eight axioms emerge:

1. **Trace schema is a versioned API.** Evals, dashboards, fine-tuning pipelines, and potentially auditors all read it. Schema drift silently breaks everything downstream [agent-engineering.ch].

2. **Separate trace storage from harness storage.** If traces live inside your agent framework's database, you cannot replay them on a different harness or answer compliance questions after migration. Treat traces like event logs: durable, append-only, queryable, harness-agnostic.

3. **Every span needs a causal parent.** Logs capture individual events but lose relationships. A trace must preserve causation: "the LLM generated response X *because* tool call Y returned Z" [Fiddler, 2026-06-05].

4. **Keep the complete tree.** A half-sampled trajectory (five chat spans out of fifteen) is worse than no trace at all — it looks complete and lies to you. The unit of sampling must match the unit of debugging: the full trajectory [dev.to, 2026-07-04].

5. **Redact at write time, not read time.** Sensitive fields must never reach persistent storage. Apply redaction before the bytes hit the append-only log.

6. **Emit attributes usable for sampling at span creation time.** The OpenTelemetry collector needs `gen_ai.agent.name`, `gen_ai.operation.name`, `gen_ai.request.model` at span start to make tail-sampling decisions [OTel gen_ai conventions].

7. **Traces without evaluation scores are expensive storage, not observability.** Every span should carry a quality score at capture time (faithfulness, groundedness, relevance) [Fiddler, 2026-06-05].

8. **Build the eval pipeline backwards from traces, not forwards from synthetic data.** Your eval set should be sampled, labelled, and curated from real production traces [agent-engineering.ch].

---

## 2. Current Format Analysis

### 2.1 Superchat Tape Architecture (current state)

The superchat tape system already implements several advanced concepts:

**Strengths:**

| Feature | Implementation | Alignment with Best Practices |
|---------|---------------|-------------------------------|
| Append-only immutability | `superchat-tape--row-event` / SQLite `tape` table | ✓ Matches "durable, append-only, queryable" axiom |
| Event identity | `superchat-tape--new-event-id()` — `evt_` + SHA-256 hash | ✓ Stable, globally unique event IDs |
| Causal linking | `:causation-id` on every event, validated by Closure interpreter | ✓ Explicit causal chain (stronger than most systems) |
| Schema versioning | `:schema` field (currently `1`) on every Runtime event | ✓ Versioned API (though not yet migrated) |
| Redaction at write time | `superchat-tape--redact` before `json-encode` | ✓ Secret fields never touch disk |
| Content integrity | SHA-256 hashes for oversized payloads and exact evidence | ✓ Content-addressable evidence |
| Recovery queue | `superchat-tape-queue-recovery-events` for audit-incomplete runs | ✓ Durability even under persistence failure |
| Closure (formal verification) | `superchat-closure-judge` replays Tape to prove Run/Turn terminal states | ✓ Unique capability — formal lifecycle verification |
| Projection (materialized views) | `superchat-projection.el` — replayable checkpoints | ✓ Read models derived from immutable events |
| Continuity envelopes | `superchat-view--continuity-assemble` — bounded context windows | ✓ Deterministic, audit-disclosable context |

**Current event type catalog** (from `superchat-closure.el`):

```
Run lifecycle events:
  intent.received → run.started → [tool.* / approval.*] → response.completed
  → run.completed / run.failed / run.cancelled → turn.completed

Tool events:
  tool.requested → approval.requested → approval.decided
  → tool.execution.started → tool.completed / tool.failed

Evidence events:
  decision.snapshot.recorded  (full prepared provider envelope)
  output.first.recorded       (first observable output of an attempt)

Meta events:
  experience.intervention.recorded
  capability.activated, topology.changed, authority.policy.selected
  workspace.bound, workspace.unbound
  memory.captured, memory.corrected, memory.reviewed
  claim.recorded, feedback.recorded
  anchor.created
```

### 2.2 Gaps Identified

Comparing against the C.A.S.E. model and the best-practice literature:

| Gap | Current State | Impact |
|-----|--------------|--------|
| **No span hierarchy** | Events are flat; parentage is only through `:causation-id` | Cannot see tree structure at a glance |
| **No latency/duration on events** | `:recorded-at` exists but no `:duration-ms` field | Cannot compute per-step performance |
| **No token/cost tracking** | Token counts live inside oversized payloads (hash-only in Tape) | Cannot compute cost per run or detect drift |
| **No step-type taxonomy** | Event types are domain-specific strings (e.g. `tool.requested`), no standardized taxonomy | Hard to map to OTel conventions or cross-system tooling |
| **No retry/attempt count on tool events** | Retries exist but are implicit in event sequences | Cannot derive tool success rate metrics |
| **No agent identity on sub-agent spans** | `:actor-id` exists but sub-agent boundaries are implicit | Multi-agent traces are hard to segment |
| **No structured error codes** | Errors are free-text strings | Cannot alert on error categories |
| **Causation is Run-scoped only** | `:causation-id` links within one Run, but cross-Run/Workflow causation is implicit | Cannot trace Workflow-level causality |
| **No evaluation score events** | Closure judges terminal state but does not score intermediate quality | No quality signal in trace |
| **Payload omitted for large content** | >16KB payloads are replaced with hash+size | Useful content (e.g. long tool results) is invisible to trace readers |

### 2.3 Current Format Example (annotated)

```json
{
  "type": "tool.requested",
  "session-id": "consumer-N-absent",
  "turn-id": "1786427032-d409",
  "run-id": "run-20260811-134352-757125-1bbc",
  "actor-id": "lead",
  "correlation-id": "run-20260811-134352-757125-1bbc",
  "causation-id": "evt_c847cd61cc4a038cf4f43b96bd",
  "profile-id": "code",
  "workspace-id": "workspace-N-absent",
  "event-id": "evt_3935569a0e7fb29b755660303a",
  "schema": 1,
  "summary": "tool.requested: get_file_contents"
}
```

Note: Tool arguments and results are in `payload`, which is redacted and may be hash-only. No `duration-ms`, no `step-index`, no `attempt` number, no `status` field.

---

## 3. Recommended Additions

### 3.1 Immediate (low-effort, high-value)

These can be added to the current event plist without schema migration:

#### 3.1.1 Add `:duration-ms` to every event

```elisp
;; In superchat-tape--event or superchat-runtime--event:
:duration-ms ,(when start-time
                (* 1000.0 (- (float-time) start-time)))
```

This single field enables: latency histograms, per-step performance regression detection, tool slowness alerts, and identification of "slow tail" runs for sampling.

**Priority: HIGH.** Every surveyed source lists latency per step as a minimum required signal.

#### 3.1.2 Add `:attempt` ordinal to tool and LLM events

```elisp
:attempt 1    ;; First LLM call in this Run
:attempt 2    ;; Retry after tool failure feedback
```

This enables: retry rate metrics, "how many attempts did this tool take" queries, and identification of oscillating agents. The Run already tracks `:decision-snapshot-events` with attempt ordinals — surface this to individual tool events.

**Priority: HIGH.** Without attempt tracking, tool success rate metrics are impossible to compute from traces alone.

#### 3.1.3 Add `:error-code` and `:error-type` to failure events

```elisp
;; Instead of free-text error strings in payload:
:error-code "TOOL_TIMEOUT"
:error-type "transient"
```

Standardize on: `transient` (retryable), `permanent` (do not retry), `policy_denied`, `timeout`, `invalid_input`, `system_error`.

**Priority: HIGH.** Enables error categorization, alerting, and tail-sampling policies (keep traces with `permanent` errors at 100%).

#### 3.1.4 Add token/cost metadata to LLM events

```elisp
;; On response.completed:
:input-tokens 4521
:output-tokens 387
:model "claude-sonnet-4-20250514"
:provider "anthropic"
```

Even if the full prompt/response content is hash-only, the token counts and model identity are small enough to inline. These enable cost tracking per run and cost drift detection.

**Priority: HIGH.** Every surveyed source lists token usage as a required signal.

### 3.2 Medium-term (requires schema coordination)

#### 3.2.1 Span hierarchy: `:parent-event-id` and `:span-kind`

Adopt a tree structure aligned with OpenTelemetry:

```
Run span (kind: agent.run)
├── LLM call span (kind: gen_ai.client.operation) [attempt 1]
│   └── Tool call span (kind: agent.tool_call) [tool.requested]
│       └── Approval span (kind: agent.approval)
│           └── Tool execution span (kind: agent.tool_execution)
├── LLM call span (kind: gen_ai.client.operation) [attempt 2 — feedback from tool]
│   ...
└── Response span (kind: agent.response)
```

Implementation: Add `:parent-event-id` (references the containing span) and `:span-kind` (one of `agent.run`, `gen_ai.client.operation`, `agent.tool_call`, `agent.tool_execution`, `agent.approval`, `agent.retrieval`, `agent.handoff`).

**Why this matters:** A flat event list cannot answer "show me the tree of what happened." Every surveyed tracing system (OTel, LangSmith, Langfuse, AgentDebugX) uses hierarchical spans. The Closure interpreter already reconstructs this hierarchy implicitly — making it explicit in the trace format makes it machine-readable for any consumer.

#### 3.2.2 Workflow-level `:trace-id`

Currently `:correlation-id` equals `:run-id` by default. For Workflow topologies, runs should share a `:trace-id` (the Workflow's identity) while maintaining per-run `:run-id`.

```elisp
;; Top-level Workflow event:
:trace-id "wf-20260811-001"    // shared across all runs in this workflow
:run-id "run-20260811-001-aaaa"  // unique to this sub-run
```

This enables: cross-run causal tracing, Workflow-level cost aggregation, and answering "what happened across this entire Workflow execution."

#### 3.2.3 Structured output definition: selectable verbosity levels

The current `superchat-tape-max-inline-bytes` (16KB) is a blunt instrument. Replace with verbosity tiers:

| Level | What's Inlined | What's Hashed |
|-------|---------------|---------------|
| `minimal` | Identities, latencies, token counts, error codes | All content (prompts, responses, tool args/results) |
| `standard` (default) | Above + tool arguments, truncated responses (first 2KB) | Full responses beyond 2KB |
| `diagnostic` | Everything | Nothing (but PII still redacted) |
| `evidence` | Everything + exact JSON receipts | Nothing |

The `evidence` level already exists implicitly through `decision.snapshot.recorded` and `output.first.recorded` — formalize it.

### 3.3 Long-term (architectural)

#### 3.3.1 Evaluation score events

Add a new event type: `evaluation.scored` that attaches quality scores to spans at capture time.

```elisp
;; Appended after response.completed:
{:type "evaluation.scored"
 :target-event-id "evt_..."     ;; which event is being scored
 :metric "faithfulness"
 :score 0.92
 :scorer "centor-v2"}
```

This transforms traces from "what happened" to "how well did it happen" — the critical step for self-improvement loops [Fiddler, 2026-06-05] [Socratic-SWE].

#### 3.3.2 OpenTelemetry export adapter

Map the superchat event schema to OTel spans for export to Langfuse/Datadog/Honeycomb/self-hosted collectors:

| Superchat Event | OTel Span Kind | Key Attributes |
|----------------|----------------|----------------|
| `intent.received` | `agent.run` (root) | `agent.name`, `agent.session_id` |
| `run.started` | child of `agent.run` | `agent.run_id`, `agent.topology` |
| `response.completed` | `gen_ai.client.operation` | `gen_ai.request.model`, `gen_ai.usage.*` |
| `tool.requested` | `agent.tool_call` | `tool.name` |
| `tool.completed` | child of `agent.tool_call` | `tool.outcome`, `tool.duration_ms` |
| `approval.decided` | `agent.approval` | `approval.decision`, `approval.policy` |

Use the emerging OTel `gen_ai.*` semantic conventions [open-telemetry/semantic-conventions-genai, 2026] and the OWASP Agent Observability Standard extensions [aos.owasp.org].

#### 3.3.3 Trace-driven skill distillation pipeline

Following Socratic-SWE and TRACE patterns:

1. **Collect** traces from production runs, tagged with success/failure
2. **Contrast** successful vs. failed trajectories to identify capability gaps
3. **Distill** recurring failure modes into a structured Skill Registry
4. **Generate** targeted practice tasks that exercise missing capabilities
5. **Train** on those tasks, producing new traces → loop

The current `experience.intervention.recorded` event and `superchat-experience.el` are early steps toward this pattern. The missing piece is the contrastive analysis pipeline that identifies *which* capabilities are missing from *which* failure patterns.

---

## 4. Causal Analysis Enablement

### 4.1 What the Current System Already Enables

The Closure interpreter (`superchat-closure.el`) is a **uniquely advanced** capability: formal verification that a Run reached a valid terminal state by replaying the Tape. It can answer:

- "Did this Run have a matching intent → start → response → terminal sequence?"
- "Are there any unaccounted Effects or Approvals?"
- "Did the Run complete with all tools discharged?"

This is *causal analysis at the protocol level* — it verifies the lifecycle contract was satisfied.

### 4.2 What's Missing for Full Causal Analysis

Current limitations:

1. **No counterfactual replay.** Closure answers "did it follow the protocol?" but not "what if tool X had returned Y instead?" The evidence-contract system captures `decision.snapshot.recorded` (what was sent to the model) and `output.first.recorded` (what came back), but there is no mechanism to replay a trace with modified intermediate results.

2. **No cross-run causation visibility.** The `:causation-id` links events within a Run, but Workflow step N → step N+1 causation is only implicit in the Workflow engine's state. If step 3 of a Workflow produced bad output that made step 7 fail, that causal chain is not visible in the trace.

3. **No failure attribution.** TraceElephant shows that full traces improve attribution accuracy by up to 76.5% over output-only logs [ACL 2026]. The current system has full traces but no automated attribution — identifying *which agent* and *which step* is the root cause of a failure.

### 4.3 Recommended Causal Analysis Additions

#### 4.3.1 Attribution events

After a Run fails, an attribution analyzer could append:

```elisp
{:type "attribution.recorded"
 :target-run-id "run-..."
 :root-cause-event-id "evt_..."
 :root-cause-agent "lead"
 :decisive-step-index 3
 :failure-mode "tool_returned_empty"
 :confidence 0.87
 :method "contrastive-successful-traces"}
```

#### 4.3.2 Counterfactual markers

When a tool returns an error and the agent retries with a different approach:

```elisp
{:type "tool.failed"
 ;; ... existing fields ...
 :counterfactual true          ;; This failure changed the agent's path
 :alternative-path "agent chose fallback tool instead"
 :would-have-succeeded-with "correct_api_key"}
```

This enables the self-improvement loop: "if the agent had done X instead of Y at step 3, the run would have succeeded."

#### 4.3.3 Causal graph edges

Add explicit edge types to `:causation-id`:

```elisp
:causation {:event-id "evt_..."
            :relation :because-of}    ;; default
:causation {:event-id "evt_..."
            :relation :in-response-to} ;; tool result → next LLM call
:causation {:event-id "evt_..."
            :relation :delegated-from} ;; sub-agent spawn
```

This makes the causal graph machine-readable without replaying the full Closure interpreter.

---

## 5. References

| # | Source | Key Insight |
|---|--------|-------------|
| 1 | [The Trace Becomes the Primary Artifact of Agent Engineering](https://www.agent-engineering.ch/articles/traces-as-primary-artifact/), Daniel Huber, 2026-05-02 | Traces are the new working surface for evals, attribution, compliance, and self-improvement. Schema must be a versioned API. |
| 2 | [Agent Trace Instrumentation Specification](https://geodocs.dev/ai-agents/agent-trace-instrumentation-spec), 2026-05-03 | OTel span hierarchy for agents: `agent.run` → `gen_ai.client.operation` → `agent.tool_call`. Tail sampling by error/latency/cost. |
| 3 | [Tracing and Observability for Agent Systems](https://agentengineering.org/articles/tracing-and-observability-for-agent-systems/), 2026-03-23 | C.A.S.E. model: Context, Actions, Signals, Enforcement. Minimum trace fields. |
| 4 | [Agent Tracing: How to Track Agent Decisions](https://www.agentpatterns.tech/en/observability-monitoring/agent-tracing), 2026-03-21 | trace_id, span_id, parent_span_id, step_type, latency, status, stop_reason as minimum. Structured JSON logs. |
| 5 | [Trace Sampling for Agents](https://dev.to/gabrielanhaia/trace-sampling-for-agents-keeping-the-trajectories-that-matter-46ak), 2026-07-04 | Tail sampling at trajectory level. Keep failures/slow/expensive/eval at 100%, baseline at 15%. Two-tier collector for trace-ID routing. |
| 6 | [OpenTelemetry Agent: Which Telemetry Signals Matter Most](https://www.fiddler.ai/blog/opentelemetry-agent-telemetry-signals), 2026-06-05 | Traces are the primary signal. Metrics derived from traces: tool success rate, token/step, eval scores, path depth. Evaluation must be per-span. |
| 7 | [OTel GenAI Semantic Conventions](https://github.com/open-telemetry/semantic-conventions-genai), 2026 | Standard attributes: `gen_ai.operation.name`, `gen_ai.agent.name`, `gen_ai.request.model`, `gen_ai.usage.*`, `gen_ai.tool.*`. Spans for agent, LLM call, tool, retrieval, embeddings. |
| 8 | [OWASP Agent Observability Standard](https://aos.owasp.org/spec/trace/), 2026 | Security-focused trace extensions: guardian decisions (allow/deny/modify), protocol events (A2A, MCP), system health events. |
| 9 | [TraceElephant: Failure Attribution Benchmark](https://github.com/TraceElephant/TraceElephant), ACL 2026 | Full execution traces improve attribution accuracy by up to 76.5% over output-only. Annotated failure traces with responsible agent + decisive step. |
| 10 | [MASEval: Multi-Agent System Evaluation](https://maseval.readthedocs.io/), ACL 2026 | Trace-first evaluation. Per-agent message histories. System-level comparison. Canonical + raw trace layers. |
| 11 | [AgentDebugX Canonical Trace Schema](https://github.com/AgentDebugX/AgentDebugX/blob/main/docs/TRACE_SCHEMA.md), 2026 | AgentTrajectory → AgentEvent → Artifact model. DiagnosticReport with root_cause_event_id, findings, attribution. |
| 12 | [Socratic-SWE: Self-Evolving Coding Agents via Trace-Derived Agent Skills](https://arxiv.org/pdf/2606.07412v1), 2026 | Closed-loop: traces → Skill Registry → targeted task generation → training → new traces. +7.80 SWE-bench Verified. |
| 13 | [TRACE: Capability-Targeted Agentic Training](https://arxiv.org/html/2604.05336), 2026 | Contrasts successful vs failed trajectories to identify lacking capabilities. Synthesizes isolated training environments per capability. +15.3 on τ2-Bench. |
| 14 | [RL for LLM-based Multi-Agent Systems](https://arxiv.org/html/2605.02801v1), 2026-05-04 | Orchestration traces as temporal interaction graphs. Eight reward families, eight credit-bearing units, five orchestration sub-decisions. |
| 15 | [TRACE: Turn-level Reward Assignment via Credit Estimation](https://arxiv.org/html/2607.13988), 2026-07-15 | Dense credit assignment for long-horizon agents using log-ratio state values at tool-call boundaries. |

---

## Appendix: Quick Comparison Matrix

| Capability | superchat (current) | OTel GenAI Conventions | AgentDebugX | Best Practice Target |
|------------|---------------------|------------------------|-------------|---------------------|
| Event identity | ✓ `event-id` | ✓ `span_id` | ✓ `event_id` | Required |
| Causal parent | ✓ `causation-id` (intra-run only) | ✓ `parent_span_id` | ✓ `parent_event_id` | Required (cross-run) |
| Span hierarchy | ✗ (flat events) | ✓ (tree) | ✗ (flat, planned) | Required |
| Latency per step | ✗ | ✓ `duration_ms` | ✓ `duration_ms` | Required |
| Token/cost tracking | ✗ (hash-only) | ✓ `gen_ai.usage.*` | ✗ | Required |
| Tool arguments inline | ✗ (redacted >16KB) | Opt-in | ✓ `input` | Conditional |
| Error categorization | ✗ (free text) | ✓ `error.type` | ✓ `FailureFinding` | Required |
| Attempt/retry count | Implicit | ✗ | ✗ | Required |
| Eval scores in trace | ✗ | ✓ `eval.score` | ✗ | Required |
| Formal lifecycle verification | ✓ (Closure) | ✗ | ✗ | Unique advantage |
| Content-addressable evidence | ✓ (SHA-256) | ✗ | ✗ | Unique advantage |
| Schema versioning | ✓ (`:schema 1`) | ✓ (semconv version) | ✗ | Required |
| PII redaction at write | ✓ | Recommended | ✗ | Required |
| Recovery/durability queue | ✓ | ✗ | ✗ | Unique advantage |
| Cross-run trace ID | ✗ | ✓ `trace_id` | ✓ `trace_id` | Required |
| OTLP export | ✗ | ✓ (native) | Planned | Recommended |
| Security policy events | Partial (approval) | ✗ | ✗ | OWASP AOS alignment |
