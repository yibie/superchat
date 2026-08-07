# Artifact Semantic Use v0.3 — Offline Scorer Remediation

Status: **offline scorer accepted; no provider authorization**

This revision closes the known v0.2 scorer false positives before any provider
cohort is considered. It is a pure offline fixture and scoring program. It does
not load Superchat Runtime, call a provider, modify model weights, or demonstrate
Semantic Use, Task Effect, improvement, or Learning.

The rejected [`v0.2`](../v0.2/README.md) revision remains frozen. v0.3 reuses
its frozen synthetic data and pure parsing helpers, but its final content and
control verdicts consume only the explicit sealed v0.3 manifest. It does not
change the v0.2 files.

## Measurement boundary

Actor records contain an opaque `:assignment-id`, the exact
`:visible-inputs`, first-action evidence, and observed output evidence. They do
not contain evaluator-only row, condition, expected-target, or truth-join
fields. The evaluator receives those facts in a separate sealed manifest.

For content and sham records, prompt, Artifact, messages, and tool schema must
all be present and non-empty. For withheld records, `:visible-inputs` must not
contain an `:artifact` key at all; `:artifact nil` is rejected rather than
treated as absence.

There is one public scoring entry point:
`semuse-v03-cohort-result`. It invokes the ordered closure

```text
shape → structure → record-set → provider-envelope → artifact
      → first-action → content → control
```

The shape gate runs before any plist lookup or join. Later gates validate the
sealed manifest, actor/evaluator join, closed and recursively scanned visible
envelope, exact Artifact bytes and family grammar, Decision Snapshot/first
Action origin, content observations, and sham control. Invalid input returns
`:ok nil`, `:status not-identifiable`, and `:closure not-identifiable`; the
public scorer converts internal signals to the same fail-closed result shape.

The current audit registry contains **37 fixtures**: one supported baseline,
one valid-but-not-supported output regression, and 35 fail-closed attacks.
They cover top-level, nested, and cross-row answer truth; unknown, missing,
nil, blank, or cross-string visible inputs; duplicate, malformed, or cyclic
structures; whole, split, or cross-cell Artifact disclosure; actual-record
grammar failure; Artifact byte/length/hash/revision mismatches; sealed
schedule/structure drift; mutable v0.2-global drift; and coupled Decision
Snapshot/Action/terminal/tool-surface forgery.

## Run it

From the repository root, audit all 37 registered fixtures:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.3/remediation-prototype-v0.3.el audit
```

An audit passes only when every fixture matches its registered verdict. It
exits `0` on a complete match and nonzero otherwise. Audit one fixture with:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.3/remediation-prototype-v0.3.el audit top-answer-key
```

Score a fixture directly through the public cohort scorer:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.3/remediation-prototype-v0.3.el score valid-baseline
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.3/remediation-prototype-v0.3.el score actual-bad-grammar
```

`score` prints `COMPLETED` and exits `0` when validation and scoring completed,
including a `not-supported` verdict. It prints `FAIL-CLOSED` and exits `1` for
an invalid fixture. Unknown commands and fixture names exit `2`. `:ok` and the
process code describe scorer completion, not support for the semantic claim.

The pure scorer is in
[`remediation-logic-v0.3.el`](remediation-logic-v0.3.el); the command shell is
[`remediation-prototype-v0.3.el`](remediation-prototype-v0.3.el). ERT checks
are in [`test-remediation-v0.3.el`](test-remediation-v0.3.el).

## Evidence boundary

Passing this offline registry means only that every constructed fixture
matches its registered verdict. The Decision Snapshot, Action, terminal, and
their IDs are constructed offline records; their coupling is checked, but
they are not Runtime/Tape provenance. No request is sent to a model. There is
no Runtime/provider capture, real task Outcome, reusable Pattern, generated
practice, cross-task transfer, or self-learning claim in this revision.
