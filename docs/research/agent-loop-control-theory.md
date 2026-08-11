# Agent Loop Architecture and Control Theory for Self-Learning LLM Agents

## Context

An Emacs Lisp agent runtime (Superchat) runs epistemic learning experiments:

1. **Training run** demonstrates recovery from an error (e.g., stale workspace artifact read → `workspace_info` → corrected `workspace_read`)
2. **Reflection** extracts a recovery rule (`claim.recorded` of kind `candidate`)
3. **Human promotion** (`candidate.disposition.recorded` with `decision: "promote"`)
4. **Treatment run** receives the promoted candidate via an Experience View injected as a user-role message before the model's next attempt
5. **With `temperature=0`**: injection works 100% — the model deterministically follows the recovery instruction
6. **With default temperature**: injection is unreliable — the model sometimes ignores the instruction, repeats the same error, or hallucinates a different tool

## Key Concepts

### 1. Open-Loop vs Closed-Loop Control

| | Open-Loop | Closed-Loop (Feedback) |
|---|---|---|
| **Definition** | Controller output is computed once, without observing its effect | Controller continuously measures output, compares to setpoint, adjusts |
| **LLM analog** | Single prompt → response. No post-generation correction | Agent loop: tool call → observe result → decide next action |
| **Superchat mapping** | A single `llm-chat` invocation without tools | The multi-turn `send-request` loop: `initial` → `tool-followup` → `final-answer` |
| **Vulnerability** | No recovery from wrong output | Can correct mid-trajectory, but each correction is still a stochastic sampling event |

The existing Superchat agent loop (`superchat-llm.el`, `send-request` λ) is already **closed-loop at the turn level**: tool errors produce corrective user-role feedback (`superchat--llm-tool-error-feedback`), and the model gets another chance. But the **experience injection** operates **open-loop at the Run level**: the Recovery View is injected once before a `tool-followup` attempt, and there is no further mechanism to enforce compliance during that attempt.

### 2. Feedback Control: Sensor → Controller → Actuator → Plant

Classical control theory's four-element loop maps surprisingly well onto LLM agents:

| Control Theory | Superchat Agent Analog |
|---|---|
| **Plant** (system being controlled) | The LLM provider + prompt context → the probability distribution over next tokens |
| **Sensor** (measures output) | The `observer` callback: reads `tool.failed` events from Authority, compares to expected outcome |
| **Controller** (computes correction) | `superchat-experience-assemble-recovery-view`: queries applicability graph, selects promoted candidate, produces corrective prompt |
| **Actuator** (applies correction) | `llm-chat-prompt-append-response` injects the View's `:prompt` text as a user-role message before `send-request` |
| **Setpoint** (desired state) | "Tool call succeeds" — the `tool.completed` event rather than `tool.failed` |

### 3. Intervention as Control Signal vs Suggestion as Context

This is the **critical distinction** for the temperature problem:

| | Control Signal | Suggestion / Context |
|---|---|---|
| **Mechanism** | Changes the probability distribution so the desired output is **structurally inevitable** (or nearly so) | Adds text to the prompt that the model **may** attend to |
| **Temperature sensitivity** | Low — works regardless of sampling strategy | High — stochastic sampling may select tokens from competing patterns |
| **Examples** | Constrained decoding (grammar/JSON schema), tool-choice forcing (`tool_choice: "required"`), logit bias, system-level stop sequences | System prompt instructions, user messages with "please do X", few-shot examples |
| **In Superchat** | `tool_choice` in the provider options, the `:context-envelope` / `:experience-action-policy` that **blocks** dependent tools via Authority | The injected Recovery View user message — this is the only mechanism currently used |

**Why temperature=0 works**: With greedy decoding (temperature=0), the model always selects the highest-probability token. If the injected recovery instruction is clear and well-placed, the highest-probability continuation is to follow it.

**Why temperature>0 fails**: With non-zero temperature, the model samples from the distribution. Even when the instruction is the highest-probability path, the model can sample a lower-probability token that begins a different trajectory. Over multiple tool calls, this compounds: at each step there is a non-zero probability of ignoring the instruction, and a multi-step recovery sequence has `(1 - p_compliance)^n` probability of full compliance.

### 4. Control Signal Strength / Gain

In control theory, **gain** is the ratio of output correction to error magnitude. A high-gain controller responds aggressively to small errors; a low-gain controller may never overcome disturbances.

In the LLM context, "gain" maps to **how strongly the injected instruction dominates the probability distribution**:

| Low Gain (Current) | High Gain (Needed) |
|---|---|
| Appended user message amidst conversation history | System-level injection at the **beginning** of the prompt, before all competing context |
| Natural language instruction: "Use this reviewed experience..." | Structural constraint: the tool choice is **forced** or the instruction is placed at positions the model cannot skip |
| No repetition or reinforcement | **Repeated** at multiple positions (system message + user message + tool description) |
| No penalty for ignoring | **Logit bias** or **tool_choice** to make the desired tool the only available option |
| Provider-agnostic (pure prompt) | Provider-specific (logit_bias, tool_choice, response_format) |

### 5. Feedforward Control

Feedforward control anticipates disturbances and pre-compensates, rather than waiting for error to be detected and then correcting (feedback). Feedforward is faster but requires an accurate model of the disturbance.

In the Superchat architecture, **experience injection is feedforward**: the Recovery View is injected _before_ the model makes its next tool call, based on the _prediction_ that the same error class will recur. There is no runtime feedback loop to verify compliance. This is appropriate when the disturbance (error pattern) is well-modeled and predictable, but fragile when stochastic sampling introduces unpredictable variation.

### 6. Deadband and Hysteresis

- **Deadband**: A range of error values where the controller takes no action (avoids over-correction)
- **Hysteresis**: The controller's action depends on the direction of change (prevents oscillation)

In the Superchat context:
- The `experience-action-policy` with `:dependent-tools` is a deadband: the blocked View only gates specific tools; unrelated tools pass through unchanged
- There is no hysteresis: the same Recovery View may be injected on every `tool-followup` within the same Run if the View stays `blocked`

### 7. Disturbance Rejection

Temperature is a **disturbance** in the control-theoretic sense: an external stochastic force that pushes the system away from the setpoint. The question becomes: **how do we design a controller (injection mechanism) with sufficient disturbance rejection to overcome temperature-induced stochasticity?**

Three classical strategies:
1. **Increase gain** (make the control signal stronger — structural injection)
2. **Add integral action** (accumulate error over time — repeated injection, escalating severity)
3. **Reduce disturbance at source** (lower temperature for critical tool calls — targeted `temperature=0` only for the recovery step)

## Architecture Mapping

### Current Superchat Epistemic Learning Pipeline

```
Training Run
  │
  ├─ tool.failed (e.g., workspace_read stale revision)
  ├─ Agent self-corrects (workspace_info → corrected workspace_read)
  ├─ run.completed
  │
  ▼
Reflection (external/human evaluator)
  │
  ├─ superchat-experience-record-evaluation  → exercise.evaluated
  ├─ superchat-experience-record-relation    → applicability.relation.recorded
  ├─ superchat-experience-record-disposition → candidate.disposition.recorded (promote)
  │
  ▼
Treatment Run
  │
  ├─ superchat-runtime-submit(request with :evidence-contract "epistemic-learning-v0"
  │                            and :context-envelope with :experience-session-ids)
  │
  ├─ runtime observer `:before-attempt` → superchat-runtime--recovery-intervention-view
  │   ├─ Queries superchat-experience-assemble-recovery-view(failure-event, session-ids)
  │   ├─ Assembles applicability graph
  │   ├─ Finds promoted candidate → statement text
  │   └─ Returns View with :prompt text
  │
  ├─ send-request('tool-followup')
  │   ├─ observer('before-attempt) → gets View
  │   ├─ llm-chat-prompt-append-response(real-prompt, view.prompt, 'user)
  │   │   ⚠️ THIS IS THE INJECTION POINT — a user-role message in the middle of conversation
  │   └─ llm-chat-streaming / llm-chat-async
  │
  └─ If injection ignored → tool fails again → observer fires → loop repeats
      (but with same injection mechanism, so same reliability problem)
```

### Injection Point Analysis

The current injection occurs at:

```elisp
;; In superchat-llm.el, within send-request lambda, attempt-reason='tool-followup':
(when-let ((view (funcall runtime-observer 'before-attempt
                           (list :attempt-reason attempt-reason))))
  (when-let ((text (plist-get view :prompt)))
    (llm-chat-prompt-append-response real-prompt text 'user)))
```

This means the Recovery View text is:
1. **Positioned**: as a user-role message appended after the assistant's failed tool call and the tool result, before the next assistant generation
2. **Formatted**: as a natural language instruction: `[Experience v0 | scope X]\nUse this reviewed experience only when it fits the current context:\n{statement}`
3. **Scope**: visible in one attempt only — if the model ignores it and fails again, a new View may be assembled and injected

### Control Flow Diagram

```
                    ┌──────────────────────────┐
                    │   Experience Projection   │
                    │  (applicability graph)    │
                    └────────────┬─────────────┘
                                 │ reads
                    ┌────────────▼─────────────┐
                    │   Recovery View Assembly  │  ← feedforward controller
                    │   (fail-closed, at most   │
                    │    one promoted candidate) │
                    └────────────┬─────────────┘
                                 │ View with :prompt text
                    ┌────────────▼─────────────┐
                    │  Prompt Injection Point   │  ← actuator
                    │  (user-role message)      │
                    └────────────┬─────────────┘
                                 │
    ┌────────────────────────────┼────────────────────────────┐
    │                            ▼                            │
    │  ┌──────────────────────────────────────────────┐      │
    │  │           LLM Provider (Plant)               │      │
    │  │  prompt → token distribution → sampled output │      │
    │  └──────────────────────┬───────────────────────┘      │
    │                         │                              │
    │                         ▼                              │
    │  ┌──────────────────────────────────────────────┐      │
    │  │         Authority Seam (Sensor)              │      │
    │  │  validate → resolve effects → execute → Tape │      │
    │  └──────────────────────┬───────────────────────┘      │
    │                         │                              │
    │              ┌──────────▼──────────┐                   │
    │              │  tool.completed?    │                   │
    │              └──────┬──────┬───────┘                   │
    │                  yes│      │no                         │
    │                     │      ▼                           │
    │                     │  tool.failed → observer fires    │
    │                     │  recovery-intervention-view      │
    │                     │  (feedback loop closes)          │
    │                     │      │                           │
    │                     │      └────→ back to injection ───┘
    │                     ▼
    │              run.completed ✓
    └────────────────────────────────────────────────────────┘
```

## Weaknesses

### 1. Injection as Context, Not Control

The Recovery View is injected as a **user-role natural language message**. This is fundamentally a **suggestion**, not a **control signal**. The model's probability distribution is nudged but not constrained. With temperature>0, the model can sample tokens that ignore the instruction entirely.

**Evidence**: The code at `superchat-llm.el` line ~1250 appends the View text via `llm-chat-prompt-append-response` with role `'user`. There is no `tool_choice` forcing, no logit bias, no constrained decoding schema.

### 2. No Compliance Verification During Attempt

The system checks whether a tool call succeeded or failed _after_ execution (via Authority's `tool.completed` / `tool.failed` events), but there is **no mid-generation verification** that the model is _attempting_ the instructed recovery. If the model calls `bash` instead of `workspace_info`, the system only discovers this post-hoc.

### 3. Single Injection, No Escalation

If the first injection is ignored and the tool fails again, the same mechanism fires again — another user message, same format, same "gain." There is no **escalation** (e.g., first: gentle reminder → second: explicit instruction → third: tool_choice forced).

### 4. Provider-Agnostic, No Structural Levers

The current architecture is deliberately provider-agnostic. This is valuable for portability, but it prevents using provider-specific control mechanisms:
- **OpenAI/Anthropic**: `tool_choice: {"type": "function", "function": {"name": "workspace_info"}}` would force the model to call the specific recovery tool
- **Logit bias**: Could penalize tokens associated with known-error tool names
- **JSON mode / structured output**: Could force a specific output format that structurally prevents certain errors

### 5. Experience View is Textual, Not Structural

The promoted candidate's `:statement` is free-text natural language. There is no structured representation that could be used for:
- Verifying whether the model's output _matches_ the candidate's prescribed action
- Computing a formal "compliance score"
- Automatically generating a tool_choice constraint from the candidate

### 6. Temperature is a Global, Not Per-Attempt Setting

Temperature is set once per request (via `llm-chat-prompt-temperature` in the prepared envelope). There is no mechanism to say: "for this specific recovery tool call, use temperature=0; for the rest of the conversation, use the default temperature."

## Recommendations

### Tier 1: Strengthen the Existing Injection (Low Implementation Cost)

**1a. Multi-Point Injection (Increase "Gain")**

Instead of injecting the Recovery View only as a user message, inject it at **three positions simultaneously**:
1. **System message**: Prepend the View text to the system prompt for this attempt
2. **User message**: Keep the existing user-role injection
3. **Tool description**: Dynamically prepend the recovery instruction to the description of the target tool

This triples the "surface area" of the instruction in the model's context, increasing the probability it dominates attention.

**1b. Escalating Injection**

Implement a three-tier escalation within a single Run:
```
Attempt 1 (tool-followup after first failure):
  → Inject as user message (current behavior)

Attempt 2 (tool-followup after second failure on same tool):
  → Inject with stronger language + system-message placement
  → Add explicit "DO NOT call {failed_tool_name}. Instead, call {recovery_tool_name}."

Attempt 3+ (or non-retryable failure):
  → Force tool_choice to the specific recovery tool (provider-dependent)
  → Or: set temperature=0 for this specific attempt
```

**1c. Repetition Within the Injected Message**

Structure the injected prompt to repeat the key instruction at the beginning and end:
```
[CRITICAL INSTRUCTION — FOLLOW EXACTLY]
Before reading {artifact}, you MUST call workspace_info to verify its revision.
...
[REMINDER: call workspace_info FIRST]
```

### Tier 2: Structural Control Signals (Medium Implementation Cost)

**2a. Per-Attempt Temperature Override**

Add a `:temperature` field to the experience View receipt. When a Recovery View is injected, the `send-request` lambda temporarily overrides the prompt's temperature to the View's specified value (e.g., 0 or 0.1). After the recovery attempt, temperature is restored.

This requires modifying `llm-chat-prompt-temperature` on the mutable `attempt-prompt` before `llm-chat-streaming`/`llm-chat-async`.

**2b. Tool Choice Forcing for Recovery**

When a Recovery View is assembled with status `satisfied`, include a `:tool-choice` field specifying the target tool. In the `send-request` lambda, when injecting a View with `:tool-choice`, set the provider's `tool_choice` option to force that specific tool.

This requires:
- Extending the View receipt to include `:target-tool` (extracted from the candidate's scope or statement)
- A provider-specific `tool_choice` setter in the `send-request` path
- Fallback to current behavior for providers that don't support `tool_choice`

**2c. Logit Bias for Tool Names**

For providers that support logit bias (OpenAI), penalize tokens that correspond to the _failed_ tool's name, and boost tokens for the recovery tool's name. This is a soft constraint — it doesn't guarantee compliance but significantly shifts probability mass.

### Tier 3: Architectural Changes (Higher Implementation Cost)

**3a. Recovery as a Separate Sub-Run with Guaranteed Parameters**

Instead of injecting a suggestion into the main agent loop, spawn a **deterministic sub-run** with `temperature=0` and a focused prompt: "You must call tool X with these constraints. Do nothing else." The sub-run's response is then fed back into the main Run as a tool result.

This isolates the recovery action from the stochastic main loop entirely.

**3b. Candidate → Tool Schema Translation**

Parse promoted candidate statements into structured representations:
```elisp
;; Example candidate statement:
;; "When workspace_read fails with a revision error, call workspace_info
;;  first to get the latest revision, then retry workspace_read."
;;
;; Translated to:
(:recovery-pattern
  :trigger-error "superchat-workspace-artifact-revision-error"
  :sequence [(:tool "workspace_info" :args (:path "$artifact_path"))
             (:tool "workspace_read"  :args (:path "$artifact_path" :revision "$new_revision"))]
  :parameter-bindings ((:artifact_path . :from-failed-tool-args-path)))
```

This structural representation enables:
- Automatic `tool_choice` forcing for each step
- Automatic argument extraction from the failed tool call
- Verification that the model's output matches the expected sequence

**3c. Compliance Monitor as a Separate Feedback Loop**

Add a lightweight "compliance classifier" that runs after each tool call in an epistemic learning Run. Before the tool actually executes (in the Authority pre-execution hook), check whether:
- The called tool matches the expected recovery tool
- The arguments match the expected pattern

If not, block with a targeted error message rather than letting it fail at the tool level.

**3d. Closed-Loop Experience with Integral Action**

Track "injection failure count" per Run. Each time the model ignores a Recovery View and the same error recurs, increase a counter. Use this counter to:
- Escalate injection severity (Tier 1b)
- After N failures, terminate the Run with a clear diagnostic: "Agent failed to follow recovery instruction after N attempts"
- Feed this counter back into the Experience evaluation as a metric (did the candidate actually help?)

## References

### Papers

1. **Shinn, N., Cassano, F., Gopinath, A., Narasimhan, K., & Yao, S. (2023).** "Reflexion: Language Agents with Verbal Reinforcement Learning." *NeurIPS 2023*. arXiv:2303.11366.
   - The foundational paper on verbal (linguistic) feedback loops for LLM agents. Agents maintain an "episodic memory" of reflections; each reflection is appended to the prompt as additional context. Key insight: linguistic feedback works as a form of RL without weight updates, but is inherently a _suggestion_ mechanism.

2. **Anonymous. (2025).** "Meta-Policy Reflexion: Reusable Reflective Memory and Rule Admissibility for Resource-Efficient LLM Agent." arXiv:2509.03990.
   - Addresses the core limitation of Reflexion: ephemeral, task-specific traces. Proposes "rule admissibility" — a formal criterion for when a reflective rule should be applied — directly relevant to the Superchat applicability graph (relations with `applies-under`/`fails-under`).

3. **Anonymous. (2025).** "Experience Memory Graph: One-Shot Error Correction for Agents."
   - Proposes a graph-based persistent memory for LLM agents, mirroring Superchat's Experience applicability graph architecture. Demonstrates that structured memory significantly outperforms prompt-based reflection.

4. **Anonymous. (2025).** "Failure Makes the Agent Stronger: Enhancing Accuracy through Structured Reflection for Reliable Tool Interactions."
   - Shows that heuristic "think more" prompts are fragile; structured error diagnosis and repair is needed. Directly relevant to the observation that Superchat's current injection is a "think more" style nudge.

5. **Anonymous. (2025).** "ReFlect: An Effective Harness System for Complex Long-Horizon LLM Reasoning."
   - A harness (wrapper) system that detects agent failures and intervenes, rather than modifying the agent itself. Architecture closely matches Superchat's Runtime observer pattern.

6. **Nogueira, I. B. R., & Skogestad, S. (2026).** "A Systematic Approach to Multi-Agent AI from Advanced Regulatory Control Theory: Safe and Auditable LLM Operator Agents for Process Control." arXiv:2606.30877.
   - Maps industrial process control (ARC) theory to multi-agent LLM systems. Each feedback loop maps to one specialized LLM agent; constraint conflicts are resolved deterministically by an orchestrator. The "deterministic constraint resolution regardless of LLM output" principle is directly applicable.

7. **Zahedifar, R., et al. (2025).** "LLM-Agent-Controller: A Universal Multi-Agent Large Language Model System as a Control Engineer." arXiv:2505.19567.
   - A multi-agent LLM system for control engineering tasks. Demonstrates the controller-agent decomposition pattern: a central controller agent coordinates specialized auxiliary agents.

### Books

8. **Åström, K. J., & Murray, R. M. (2021).** *Feedback Systems: An Introduction for Scientists and Engineers.* Princeton University Press.
   - Standard textbook on feedback control. Chapters on PID control, loop shaping, and disturbance rejection are directly applicable to LLM agent loop design.

9. **Skogestad, S., & Postlethwaite, I. (2005).** *Multivariable Feedback Control: Analysis and Design.* Wiley.
   - The reference work on advanced regulatory control. The MIN/MAX selector and split-range logic referenced in Nogueira & Skogestad (2026) comes from this tradition.

### Key Control Theory Concepts (Background)

10. **Feedback vs Feedforward**: Feedback corrects based on measured error; feedforward anticipates and pre-compensates. The Superchat experience injection is feedforward; adding a compliance monitor would add feedback.

11. **Gain Scheduling**: Varying controller gain based on operating conditions. Analogous to varying injection strength based on attempt count or error severity.

12. **Cascade Control**: An outer loop sets the setpoint for an inner loop. Analogous to the Runtime (outer) setting constraints for the Authority (inner) tool execution seam.

13. **Selector Control (Override Control)**: Multiple controllers compete; the most conservative one wins via MIN/MAX selection. Directly maps to Authority's "tighten-only" policy composition.

14. **Integral Windup**: Accumulated error drives the controller to saturation. Analogous to repeated failed recovery injections without escalation — the system "winds up" but has no mechanism to increase corrective force.

15. **Disturbance Observer (DOB)**: Estimates and cancels disturbances. Temperature is a measurable disturbance; a DOB approach would estimate its effect and pre-compensate (e.g., by boosting instruction salience proportional to temperature).

---

*Generated from analysis of the Superchat codebase (superchat-runtime.el, superchat-experience.el, superchat-authority.el, superchat-llm.el, superchat-agent-loop.el) and literature review.*
