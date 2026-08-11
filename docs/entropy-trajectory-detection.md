# Entropy-based Structure Detection for Agent Trajectories

> **Context:** A system produces tool-call trajectories across 7–8 experimental arms. The goal is to use information-theoretic measures to distinguish when experience changes behavior (i.e., whether an "experience" intervention is taking effect).

---

## 1. Entropy Measures

### 1.1 Shannon Entropy over Action Distributions

For a discrete action space $\mathcal{A} = \{a_1, \ldots, a_K\}$, the **Shannon entropy** of the action distribution $P(A)$ is:

$$H(A) = -\sum_{a \in \mathcal{A}} P(a) \log_2 P(a) \quad \text{(bits)}$$

- **Low entropy** → concentrated distribution (few actions dominate). Indicates focused, rigid, or convergent behavior.
- **High entropy** → flat distribution (many actions equally likely). Indicates exploration, diversity, or instability.
- **Maximum entropy** = $\log_2 K$ (uniform distribution); **minimum entropy** = 0 (deterministic single action).

In the context of agent trajectories, this can be measured per-step (single-turn action entropy) or aggregated across a run (run-level action entropy). Arigbabu (2026) proposes action entropy as a core observability signal for AI agents, noting that "low action entropy can indicate focus, but it can also indicate rigid behavior; high action entropy can indicate exploration, but it can also indicate instability" [1].

### 1.2 Conditional Entropy

**Conditional entropy** $H(A \mid C)$ captures the remaining uncertainty about actions when we already know the condition $C$ (e.g., which experimental arm, the preceding context, or prior actions):

$$H(A \mid C) = -\sum_{c \in C} P(c) \sum_{a \in \mathcal{A}} P(a \mid c) \log_2 P(a \mid c)$$

Key relationships:
- $H(A \mid C) \leq H(A)$, with equality iff $A$ and $C$ are independent.
- The **information gain** (mutual information) is $I(A; C) = H(A) - H(A \mid C)$.
- If conditioning on the experimental arm substantially reduces entropy, the arm is shaping behavior.

For trajectories, we can also compute **conditional entropy of next action given history**:

$$H(A_t \mid A_{1:t-1})$$

which measures whether the agent's history constrains its future actions. You et al. (2025) extend this to **trajectory conditional entropy** $H(a_{1:T-1} \mid z_{1:T})$, where $z_t$ are state representations—this "qualifies the number of bits required to describe action sequences after observing the environment" and lower values indicate more predictable, compressible behavior [2].

### 1.3 Trajectory Entropy (Joint Entropy over Sequences)

Rather than per-step entropy, **trajectory entropy** measures diversity across complete action sequences:

$$H(\tau) = H(A_1, A_2, \ldots, A_T)$$

For practical computation with discrete actions, a trajectory can be treated as a "string" over the action alphabet. The entropy rate (per-step entropy) can be estimated via:

$$H_{\text{rate}} \approx \frac{1}{T} H(A_1, \ldots, A_T)$$

Arigbabu (2026) defines trajectory entropy as measuring "diversity across complete strategies" and notes it is "useful when the same task is run multiple times or when several agents are compared on the same benchmark" [1]. When combined with outcome entropy (the entropy of final results), one can distinguish:
- **Moderate trajectory entropy + low outcome entropy** = robust behavior (agent adapts its path while reaching stable results).
- **High trajectory entropy + high outcome entropy** = chaotic/unreliable behavior.
- **Low trajectory entropy + low outcome entropy** = rigid but possibly fragile behavior.

### 1.4 Mutual Information and Information Gain

**Mutual information** quantifies how much one variable tells us about another:

$$I(A; C) = H(A) - H(A \mid C) = \sum_{a,c} P(a, c) \log_2 \frac{P(a, c)}{P(a)P(c)}$$

Applications relevant to the multi-arm experiment:
- **$I(\text{Arm}; \text{Action})$** — how much the experimental arm reduces uncertainty about what actions the agent will take. High MI → arm strongly shapes behavior.
- **$I(\text{Experience}; \text{Action})$** — whether accumulated experience (e.g., conversation turns, number of prior tasks) constrains subsequent actions.
- **Information gain across a run** — $H_{\text{before}} - H_{\text{after}}$, where entropy decreases as the agent gathers evidence. Arigbabu (2026): "Positive information gain indicates uncertainty reduction. Negative information gain indicates the run became more uncertain" [1].

Ahmed et al. (2019) further demonstrate that entropy dynamics are linked to the geometry of policy optimization: higher-entropy policies produce smoother optimization landscapes, enabling larger learning rates [3].

### 1.5 KL Divergence and Jensen-Shannon Divergence

To **compare action distributions across arms**, divergences provide interpretable distance measures:

**Kullback-Leibler divergence:**

$$D_{KL}(P \parallel Q) = \sum_{a} P(a) \log_2 \frac{P(a)}{Q(a)}$$

- Asymmetric: $D_{KL}(P \parallel Q) \neq D_{KL}(Q \parallel P)$.
- Measures the "extra bits" needed to encode $P$ using a code optimized for $Q$.

**Jensen-Shannon divergence (recommended):**

$$JSD(P \parallel Q) = \frac{1}{2} D_{KL}(P \parallel M) + \frac{1}{2} D_{KL}(Q \parallel M)$$

where $M = (P + Q)/2$. JSD is:
- **Symmetric** and bounded in $[0, \log_2(N_{\text{groups}})]$ (1 bit for 2 groups).
- Sensitive to shape differences, not just mean shifts—it captures changes in variance, multimodality, and higher-order moments [4].
- Decomposable—one can identify which specific actions contribute most to the divergence between arms.

**For the multi-arm experiment:** JSD between the action distribution of a treatment arm and the control arm quantifies how much "experience" reshapes behavior. When JSD → 0, experience has not differentiated behavior from control.

### 1.6 Normalized and Comparative Entropy Metrics

For cross-arm comparison, raw entropy values are hard to interpret. Normalized forms help:

| Metric | Formula | Interpretation |
|--------|---------|---------------|
| Normalized entropy | $H / \log_2 K$ | 0 = deterministic, 1 = uniform |
| Entropy reduction ratio | $(H_{\text{control}} - H_{\text{treatment}}) / H_{\text{control}}$ | Fraction of uncertainty removed by treatment |
| Batch entropy | $H_b(\pi; \mathcal{S}_0) = H(\frac{1}{|\mathcal{S}_0|}\sum_{s \in \mathcal{S}_0} \pi(\cdot \mid s))$ | More reliable than per-state entropy [5] |
| Tool entropy | $H(\text{Tool}) = -\sum_t P(t) \log P(t)$ | Concentration of tool usage |
| Delta segment entropy | $\Delta H_k = H_{k,\text{after}} - H_{k,\text{before}}$ (per tool call) | Signals whether a tool call reduced or increased uncertainty [6] |

---

## 2. Computation Sketch

### 2.1 Setup

Assume:
- $\mathcal{A}$ = discrete set of $K$ possible tool-call actions (e.g., `search`, `read_file`, `bash`, `edit`, `reason`, `ask_user`, ...).
- $G$ = set of experimental arms (7–8 groups), each with $N_g$ runs.
- Each run produces a trajectory $a_1, a_2, \ldots, a_T$ (variable length $T$).
- The data is organized as:

```
run_id | arm | turn | action | context_hash | success | ...
```

### 2.2 Per-Arm Action Entropy

For each arm $g$, estimate $\hat{P}_g(a)$ as the empirical frequency of action $a$ across all turns in all runs of arm $g$:

$$\hat{P}_g(a) = \frac{\text{count}_g(a)}{\sum_{a'} \text{count}_g(a')}$$

Then:

$$H_g = -\sum_{a} \hat{P}_g(a) \log_2 \hat{P}_g(a)$$

**Bias correction:** For small samples, apply the Miller-Madow correction: $\hat{H}_{\text{MM}} = \hat{H} + (m - 1) / (2N)$, where $m$ is the number of observed action types with non-zero counts.

### 2.3 Conditional Entropy: Arm → Action

$$H(\text{Action} \mid \text{Arm}) = \sum_{g} \hat{P}(g) \cdot H_g$$

where $\hat{P}(g)$ is the proportion of total turns belonging to arm $g$ (or uniform if arms are balanced).

Then mutual information:

$$I(\text{Arm}; \text{Action}) = H(\text{Action}) - H(\text{Action} \mid \text{Arm})$$

**Interpretation:** $I(\text{Arm}; \text{Action})$ tells us how many bits of action uncertainty are "explained" by knowing which arm the agent is in. If this is close to 0, arms are not producing distinguishable action patterns.

### 2.4 Inter-Arm Divergence (JSD Matrix)

For every pair of arms $(g, h)$, compute:

$$JSD(\hat{P}_g \parallel \hat{P}_h)$$

This yields a $|G| \times |G|$ symmetric distance matrix. Visualize as a heatmap. The control arm serves as the baseline; treatment arms with larger JSD from control show stronger behavioral differentiation.

### 2.5 Trajectory Entropy (Sequence-Level)

Two approaches for discrete action sequences:

**Approach A – n-gram entropy:** Treat each trajectory as a string over alphabet $\mathcal{A}$. Estimate entropy rate using Lempel-Ziv compression:

$$H_{\text{rate}} \approx \left( \frac{1}{L} \sum_{i=1}^{L} \frac{\Lambda_i}{\log L} \right)^{-1}$$

where $\Lambda_i$ is the length of the shortest substring starting at position $i$ that has not previously appeared [7].

**Approach B – bigram/trigram entropy:** Compute $P(a_t, a_{t-1})$ or $P(a_t \mid a_{t-1})$ and compute $H(A_t \mid A_{t-1})$:

$$H(A_t \mid A_{t-1}) = -\sum_{a_{t-1}} P(a_{t-1}) \sum_{a_t} P(a_t \mid a_{t-1}) \log_2 P(a_t \mid a_{t-1})$$

This captures first-order temporal structure: does knowing the last action constrain the next action?

### 2.6 Information Gain Across a Run

For each run, partition turns into early/mid/late windows and compute entropy per window. The slope of $H(t)$ over the run duration indicates whether the agent converges (descending entropy) or becomes more uncertain (ascending entropy).

### 2.7 Bootstrap Confidence Intervals

Since entropy is an estimator applied to finite samples, report 95% bootstrap CIs:

1. Resample trajectories (with replacement) within each arm $B = 1000$ times.
2. Compute the target metric on each bootstrap sample.
3. Report the 2.5th and 97.5th percentiles.

### 2.8 Statistical Tests for Arm Differences

Following Colas et al. (2019), use **Welch's t-test** with low confidence level ($\alpha < 0.05$) on bootstrap-derived entropy distributions to test whether arm pairs have significantly different entropy values [8]. For small sample sizes, bootstrap-based permutation tests (shuffling arm labels) are preferred.

---

## 3. What Entropy Can and Cannot Tell Us

### 3.1 What Entropy CAN Tell Us

| Signal | Operational Question Answered |
|--------|-------------------------------|
| **Action entropy** | Is the agent exploring broadly or behaving rigidly? |
| **JSD between arms** | Has the intervention produced a measurably different action distribution? (Detection) |
| **$I(\text{Arm}; \text{Action})$** | How much does the arm identity reduce uncertainty about agent behavior? (Effect size) |
| **Conditional entropy $H(A_t \mid A_{t-1})$** | Is the action sequence structured/compressible, or random? |
| **Entropy trajectory over turns** | Does the agent converge/compress behavior as the run progresses? (Learning curve shape) |
| **Delta segment entropy** | Are individual tool calls reducing or increasing uncertainty? (Tool quality signal) [6] |
| **Trajectory entropy vs. outcome entropy** | Is behavioral variability translating into outcome instability? (Robustness) |
| **Entropy change over calendar time** | Is the "experience" intervention taking effect cumulatively? (Drift detection) |

### 3.2 Entropy as Early Warning for "Experience Not Taking Effect"

Drawing from entropy-triggered retraining literature [9] and non-stationary RL [10], entropy can serve as a **behavioral drift detector**:

1. **Baseline stabilization:** Compute action entropy $H_g$ in a rolling window for each arm as more data accumulates.
2. **Expected pattern:** If experience is taking effect, we expect:
   - Entropy in treatment arms to **diverge** from control (JSD increasing over time).
   - Entropy within each arm to potentially **decrease** (behavior converging to a learned pattern).
3. **Warning signal:** If after sufficient data, JSD(treatment, control) remains near zero and is not trending upward, the experience may not be producing a behavioral effect.
4. **Confidence:** Use bootstrap CIs to distinguish noise from genuine separation.

Wang et al. (2026) formalize this as a principle: "exploration strength should increase when non-stationarity intensifies and decrease when the environment stabilizes" [10]. We can invert this: if entropy in a treatment arm does NOT decrease relative to control over time, the treatment is not "stabilizing" behavior—experience is not taking effect.

### 3.3 Controlled vs. Uncontrolled Behavior

Entropy alone does NOT distinguish between these scenarios:

| Scenario | Entropy profile | Actual nature |
|----------|----------------|---------------|
| A | Low entropy, all runs use the same 3 actions | **Controlled** — focused, effective |
| B | Low entropy, all runs stuck in a loop | **Uncontrolled** — degenerate behavior |
| C | High entropy, runs explore broadly then converge | **Controlled** — strategic exploration |
| D | High entropy, random tool calls, poor outcomes | **Uncontrolled** — chaotic |

Therefore, **entropy must always be interpreted jointly with outcome metrics** (success rate, cost, latency). Arigbabu (2026) emphasizes: "Entropy is not intrinsically desirable. An agent can be highly variable and still fail, just as it can be deterministic and effective on a narrow task" [1].

A paper on language agent control further demonstrates this empirically: "higher randomness increased action entropy but did not recover reason-, memory-, self-state- or veto-coupled behavior" [11]. This means **structured control and high entropy are separable dimensions**—you need both entropy AND outcome/control metrics to characterize behavior.

### 3.4 Key Limitations

1. **Entropy is not a quality score.** It describes distributional shape, not optimality.
2. **Entropy is sensitive to action space granularity.** Merging/splitting action categories changes entropy values arbitrarily.
3. **Entropy ignores semantics.** Two different actions with similar intent are counted as different; identical strings with different intent are counted as the same.
4. **Small-sample bias.** Entropy estimators are biased downward for small $N$; always report with bias correction and CIs.
5. **Causal ambiguity.** Low entropy in a treatment arm could mean the experience shaped behavior, OR that the treatment arm's task mix was inherently narrower. Control for task distribution.
6. **Temporal resolution matters.** Entropy computed over all turns vs. per-window gives different signals. Choose resolution based on the hypothesis being tested.
7. **Entropy monitoring can miss conditional drift.** If behavioral change occurs only in $P(\text{action} \mid \text{specific context})$ while the marginal $P(\text{action})$ remains stable, marginal entropy will not detect it [9].

---

## 4. Implementation Outline

### Phase 1: Data Preparation

```
trajectory_loader
  ├── Parse raw traces → normalized action labels
  ├── Group by arm, run_id, turn
  ├── Build arm × action count matrix
  └── Build run × turn × action tensor (for trajectory-level metrics)
```

**Action normalization:** Map raw tool-call strings to canonical action labels. Handle edge cases (e.g., same tool with different arguments → should these be separate actions?). Document the mapping.

### Phase 2: Core Metric Functions

```python
import numpy as np
from scipy.stats import entropy as scipy_entropy
from collections import Counter

def action_entropy(counts: np.ndarray, base: float = 2.0, bias_correct: bool = True) -> float:
    """Shannon entropy from action counts. Applies Miller-Madow correction."""
    probs = counts / counts.sum()
    # Filter zero-probability actions
    probs = probs[probs > 0]
    H = scipy_entropy(probs, base=base)
    if bias_correct:
        m = len(probs)
        N = counts.sum()
        H += (m - 1) / (2 * N * np.log(base))
    return H

def conditional_entropy(joint_counts: np.ndarray, base: float = 2.0) -> float:
    """H(Action | Arm) from joint counts [n_arms, n_actions]."""
    arm_counts = joint_counts.sum(axis=1)
    arm_probs = arm_counts / arm_counts.sum()
    H_cond = 0.0
    for g in range(joint_counts.shape[0]):
        if arm_counts[g] > 0:
            H_cond += arm_probs[g] * action_entropy(joint_counts[g])
    return H_cond

def mutual_information(joint_counts: np.ndarray, base: float = 2.0) -> float:
    """I(Arm; Action) from joint counts."""
    total = joint_counts.sum()
    action_marginal = joint_counts.sum(axis=0) / total
    H_action = scipy_entropy(action_marginal[action_marginal > 0], base=base)
    H_action_given_arm = conditional_entropy(joint_counts, base)
    return H_action - H_action_given_arm

def js_divergence(p: np.ndarray, q: np.ndarray, base: float = 2.0) -> float:
    """Jensen-Shannon divergence between two action distributions."""
    p = p / p.sum()
    q = q / q.sum()
    m = (p + q) / 2
    return 0.5 * scipy_entropy(p, m, base=base) + 0.5 * scipy_entropy(q, m, base=base)

def trajectory_entropy_rate(sequences: list[list[int]], alphabet_size: int) -> float:
    """Estimate entropy rate using Lempel-Ziv compression (Kontoyiannis estimator)."""
    # Flatten or process per-sequence
    raise NotImplementedError("See full implementation notes")

def bootstrap_ci(data, statistic_fn, B=1000, alpha=0.05):
    """Bootstrap percentile confidence interval."""
    n = len(data)
    stats = []
    rng = np.random.default_rng(42)
    for _ in range(B):
        sample = rng.choice(data, size=n, replace=True)
        stats.append(statistic_fn(sample))
    lower = np.percentile(stats, 100 * alpha / 2)
    upper = np.percentile(stats, 100 * (1 - alpha / 2))
    return np.mean(stats), lower, upper
```

### Phase 3: Analysis Pipeline

```
for each experimental arm g:
    1. Compute H_g (action entropy) with bootstrap CI
    2. Compute H(A_t | A_{t-1}) (bigram conditional entropy)
    3. Compute entropy-over-time curve (per-window H with sliding window)
    4. Compute trajectory-level entropy (per-run, then average)

globally:
    5. Compute H(Action | Arm), I(Arm; Action)
    6. Build pairwise JSD matrix → heatmap
    7. For each treatment arm vs control:
       - JSD(treatment, control) with bootstrap CI
       - Permutation test for significance
    8. Time-series analysis:
       - Rolling JSD(treatment, control) over calendar time / data accumulation
       - Trend test (Mann-Kendall) for JSD increase over time
    9. Correlation: entropy metrics × outcome metrics
       - Report as a dashboard, not as a single score
```

### Phase 4: Interpretation Dashboard

Report the following as a combined behavioral fingerprint per arm:

| Column | Meaning |
|--------|---------|
| $H$ | Raw action entropy |
| $H_{\text{norm}}$ | Normalized entropy ($H / \log_2 K$) |
| $H(A_t \mid A_{t-1})$ | First-order temporal structure |
| JSD(arm, control) | Divergence from baseline |
| $\Delta H$ over run | Entropy trend (negative = convergence) |
| Success rate | Outcome metric |
| Tool concentration | $1 - H_{\text{tool}} / \log_2(K_{\text{tool}})$ (1 = single-tool dominance) |

---

## 5. References

1. Arigbabu, O. A. (2026). "Entropy-Based Observability for AI Agent Behavior." arXiv:2606.05872. [Link](https://arxiv.org/abs/2606.05872)

2. You, B., Wang, C., & Liu, H. (2025). "Trajectory Entropy Reinforcement Learning for Predictable and Robust Control." arXiv:2505.04193. [Link](https://arxiv.org/abs/2505.04193)

3. Ahmed, Z., Le Roux, N., Norouzi, M., & Schuurmans, D. (2019). "Understanding the Impact of Entropy on Policy Optimization." *Proceedings of ICML 2019*. [Link](https://proceedings.mlr.press/v97/ahmed19a.html)

4. (2020). "An Information-Theoretic Measure for Balance Assessment in Comparative Clinical Studies." *Entropy*, 22(2), 218. Demonstrates JSD for multi-group distribution comparison. [Link](https://www.mdpi.com/1099-4300/22/2/218)

5. (2024). "Examining Policy Entropy of Reinforcement Learning Agents." arXiv:2211.11869. Introduces batch-entropy as a more reliable policy distribution measure. [Link](https://arxiv.org/abs/2211.11869)

6. (2026). "Rethinking the Role of Entropy in Optimizing Tool-Use." arXiv:2602.02050. Shows delta segment entropy correlates with tool-call quality. [Link](https://arxiv.org/abs/2602.02050)

7. Tuhin, P., Stanley, K. G., & Osgood, N. D. (2018). "Multiscale entropy rate analysis of complex mobile agents." *Royal Society Open Science*, 5, 180488. Applies Lempel-Ziv entropy rate estimation to agent trajectories. [Link](https://royalsocietypublishing.org/doi/10.1098/rsos.180488)

8. Colas, C., Sigaud, O., & Oudeyer, P.-Y. (2019). "A Hitchhiker's Guide to Statistical Comparisons of Reinforcement Learning Algorithms." *ICLR 2019 Workshop*. Recommends Welch's t-test for RL experiment comparisons. [Link](https://openreview.net/forum?id=ryx0N3IaIV)

9. (2026). "Entropy-Triggered Retraining as Nonequilibrium Entropy Production in Deployed Machine Learning Systems." arXiv:2601.00554. Framework for using relative entropy as a drift detection signal. [Link](https://arxiv.org/abs/2601.00554)

10. Wang, T., Xia, Z., Chen, X., & Liu, S. (2026). "Tracking Drift: Variation-Aware Entropy Scheduling for Non-Stationary Reinforcement Learning." arXiv:2601.19624. Shows how entropy should scale with environmental change. [Link](https://arxiv.org/abs/2601.19624)

11. (2026). "Unpredictability dissociates from structured action control in language agents." arXiv:2605.09692. Empirically demonstrates that high entropy does not imply structured control. [Link](https://arxiv.org/abs/2605.09692)

---

## Appendix: Quick Reference — Key Formulas

| Quantity | Formula | Range | What it answers |
|----------|---------|-------|-----------------|
| Action entropy | $H(A) = -\sum p(a) \log p(a)$ | $[0, \log K]$ | How diverse are the actions? |
| Conditional entropy | $H(A \mid C) = \sum p(c) H(A \mid c)$ | $[0, H(A)]$ | How much uncertainty remains given condition? |
| Mutual information | $I(A; C) = H(A) - H(A \mid C)$ | $[0, \min(H(A), H(C))]$ | How much does C tell us about A? |
| JSD | $\frac{1}{2}D_{KL}(P\|M) + \frac{1}{2}D_{KL}(Q\|M)$ | $[0, 1]$ (for 2 groups) | How different are two action distributions? |
| Normalized entropy | $H / \log K$ | $[0, 1]$ | How close to uniform vs. deterministic? |
| Entropy rate | LZ estimator | $[0, \log K]$ | How compressible/predictable is the action sequence? |
| Delta segment entropy | $H_{\text{after}} - H_{\text{before}}$ | $[-\log K, \log K]$ | Did this tool call reduce uncertainty? |
