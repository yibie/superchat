# Artifact Semantic Use v0.2 — Strict Offline Remediation Prototype

Status: **independent-review VETO; frozen rejected revision**

The built-in audit still passes its 18 named fixtures, but fresh source-facts
and code-adversary reviewers reproduced false positives: an exact target in an
unknown model-visible envelope field, missing prompt/Artifact inputs,
ambiguous duplicate plist/schedule keys, unjoined Artifact bytes, and grammar
or structure failures outside the cohort closure.  The authoritative verdict
and next gate are in [`../../DECISION-MAP.md`](../../DECISION-MAP.md).

This throwaway revision repairs the reproduced v0.1 fail-open paths before a
Runtime harness or provider request is considered.  It is pure Emacs Lisp:
it does not require Superchat, call a model/provider, use Runtime, Workspace,
Tape, Authority, Closure, a database, or the network.

The question is narrow: can a scorer reject every supplied duplicate,
replacement, malformed first-output witness, false proof sentinel, schedule
failure, grammar failure, sham-control failure, and model-visible truth-table
oracle?
The synthetic records are fixtures for the scorer only, not experiment data.

## Run it

From the repository root:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el audit
```

Expected process result: `audit=PASS`, exit code `0`.  The command prints one
`PASS`/`FAIL` line for every replayed counterexample.

The deliberately damaged schedule must fail closed:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el broken
```

Expected process result: `audit=FAIL`, exit code `1`.

The small inspection TUI is started without an argument.  Its `q` command
exits `0`:

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el
```

The pure scorer is in
[`remediation-logic-v0.2.el`](remediation-logic-v0.2.el); the thin terminal
shell is in
[`remediation-prototype-v0.2.el`](remediation-prototype-v0.2.el).

## Claim boundary for this revision

The only result label retained here is `descriptive-consistency`.  The
strongest future sentence remains bounded to the 18 fixed content cells and
proved first outputs:

> Across the 18 preregistered content cells in these six fixed constructed
> rows, with request inclusion proved for each cell, the observed first
> outputs were consistent with task-local use of both the Artifact parameter
> and the task input.

For **this v0.2 revision only**, that descriptive boundary supersedes the
`caused` wording and the separate `Task Effect` claim clauses in the retired
v0 contract.  The original contract is preserved in
`experiments/archive/superchat-active-experiment-materials-through-2026-08-09.zip`
and is no longer active authority.  v0.2 does not authorize or report a
causal effect, a Task Effect estimand, reasoning, understanding, selection,
reuse, improvement, generalization, promotion, or Learning.

## Fail-closed scoring order

Every scoring entry point receives the schedule explicitly.  There is no
`(or schedule frozen-schedule)` fallback: `nil` is an empty schedule and fails
both count and hash checks.

Scoring has three mandatory gates, in order:

1. **Record-set gate.**  The input has exactly 30 records and exactly one
   record for every frozen assignment code.  Each code, position, row, and
   condition must join the same schedule entry.  Missing, extra, duplicate,
   replacement, malformed, or reordered identities fail.  Session,
   Workspace, producer Run, and consumer Run IDs must be non-empty opaque
   strings and unique within their field.  Non-withheld records must also have
   complete Artifact identity/byte fields; withheld records must carry an
   exact `proved` no-artifact witness.
2. **First-attempt gate.**  Each record must prove, with the exact enum
   `proved`, first request ordinal `1`, attempt ordinal `1`, one earliest
   terminal user-facing output, no answer before required delivery, no retry,
   and no replacement.  The raw first output, terminal output, UTF-8 bytes,
   and SHA-256 fields must agree.  This gate deliberately does not parse
   answer syntax.  Missing or malformed first-output evidence is
   `not-identifiable`; it is never a wrong answer and never a pass.  After the
   gates pass, the exact case-sensitive extractor rejects suffixes, extra
   lines, empty values, and malformed output.  Such a witnessed protocol
   result is scored at the content stage as `answer=protocol-failure` and
   `cell=not-supported`.
3. **Provider-envelope/oracle gate.**  Assignment code, row, condition,
   schedule position, expected target, and truth join are scorer-only.  They
   must be absent as fields and values from the model-visible prompt,
   Artifact, messages, tool schema, and envelope.  Runtime IDs are required
   to be opaque and have their own exact `proved` witness.  An exposed cell or
   truth-table oracle closes the result as `not-identifiable`.

Only after all three gates pass are answer states interpreted.  A witnessed,
well-formed wrong answer is `not-supported`; a witnessed malformed answer is
also `not-supported` with answer state `protocol-failure`.  Neither is
silently replaced by a later answer.

Every proof field uses exact equality to the symbol `proved`.  `:false`,
`false`, `0`, arbitrary non-nil values, and missing values do not prove
anything.

## Content, sham, and withheld are separate

The six crossed rows are the two-input matrix from v0.1.  Each family has a
parameter grammar, and the third content value is a real member of that
grammar:

| family | World A / B / third syntax | task-only syntax | operation |
|---|---|---|---|
| scale | `MUL-[0-9]{3}-ZQ` | `PAY-[0-9]{3}-RV` | parameter × task number |
| offset | `OFF-[PN][0-9]{3}-ZQ` | `RAW-[0-9]{3}-RV` | signed parameter + task number |
| permute | `ORD-DDDD-ZQ`, exactly a permutation of `1234` | `SEQ-[A-Z0-9]{4}-RV` | reorder four task characters |

The sham forms are separately frozen as `MUL-XXX-ZQ`, `OFF-X000-ZQ`, and
`ORD-XXXX-ZQ`.  A value such as `BAD-019-ZQ` fails the family grammar; the
gate does not infer validity from the absence of `X`.

World A, World B, and the third content value produce 18 independently
reported content-cell observations.  Sham has its own witness, target-hit,
abstention, and closure fields.  A sham target or other control failure that
blocks the maximum claim is
`control-driven-not-identifiable`, never a content-answer `not-supported`.
That same control-driven closure applies to a malformed or non-abstaining sham
output.  A malformed World-A or withheld output in the independent pair is
`protocol-failure` with `difference=nil`; it never produces a numeric pair.
The six withheld records never enter the content count.  World-A/withheld is
only an independent descriptive pair; missing withheld evidence gives
`difference=nil`, not a zero or an apparent effect.

## Frozen schedule

The literal expanded schedule below is authoritative.  The code stores the
same position/code/row/condition fields as plists and hashes the canonical
lines shown here:

```text
01|ASG-BF514E87CEEE|scale-x1|world-a
02|ASG-D91AAC285E58|offset-x1|decoy
03|ASG-FD94BAAFC5DB|permute-x1|withheld
04|ASG-005C6D92523C|scale-x2|world-b
05|ASG-F387700C2CCD|offset-x2|sham
06|ASG-B2D847926362|permute-x2|world-a
07|ASG-2F376B58C7B1|offset-x2|withheld
08|ASG-C563A052A268|scale-x2|decoy
09|ASG-F458E94F8D92|permute-x2|world-b
10|ASG-F79C9EADE560|offset-x1|sham
11|ASG-414B95EEB950|scale-x1|world-b
12|ASG-788EF91325FF|permute-x1|world-a
13|ASG-78C122A09673|permute-x1|world-b
14|ASG-4F912E84C684|scale-x1|decoy
15|ASG-43E1FBCADAFE|offset-x2|world-a
16|ASG-FCAE9F1FB481|permute-x2|decoy
17|ASG-109C815EB99A|scale-x2|sham
18|ASG-3C0193C57BC5|offset-x1|withheld
19|ASG-2379824BD54B|scale-x2|withheld
20|ASG-6094BC02B26C|permute-x2|sham
21|ASG-47A6DC1F9E3C|offset-x1|world-a
22|ASG-B126AE8E642B|scale-x1|sham
23|ASG-63FA23C90A47|permute-x1|decoy
24|ASG-FF78766337A0|offset-x2|world-b
25|ASG-3667D8F00897|permute-x2|withheld
26|ASG-FB0B7919EB79|offset-x2|decoy
27|ASG-8A21BD88E4B8|scale-x1|withheld
28|ASG-891E0ACC431C|permute-x1|sham
29|ASG-35D97662066D|offset-x1|world-b
30|ASG-4853FE793693|scale-x2|world-a
```

Expected schedule SHA-256:

```text
ca0a19a6f273033ac0858de65e9ead70a57d0c664837eac622be38faa9db21bf
```

## Audit coverage

`audit` replays all of the following and fails the process if any check is
not `PASS`:

```text
passing, v-only, x-only, missing content witness, missing withheld,
parser suffix, duplicate, extra, missing record,
wrong first + later correct replacement, truthy :false, empty schedule,
schedule drift, lowercase answer, BAD decoy, sham target, oracle exposed,
failed audit nonzero
```

Observed offline output from the current files is:

```text
audit=PASS
schedule_sha256=ca0a19a6f273033ac0858de65e9ead70a57d0c664837eac622be38faa9db21bf
passing_status=consistent-with-two-input-use
v_only_status=not-supported
x_only_status=not-supported
missing_withheld_status=missing-evidence difference=nil
```

This is a code/prototype result only.  It is not an independent review and
does not prove any Runtime, transport, provenance, provider, or model fact.

The parser-suffix and lowercase-answer audit cases specifically require
`stage=scored`, `content-status=not-supported`, and an affected cell with
`answer=protocol-failure`; they are not first-attempt gate failures.  The
truthy-`:false` case additionally requires content `not-identifiable` and an
independent A/withheld pair of `missing-evidence` with `difference=nil`.

## Remaining boundary

The prototype still does not prove producer provenance, immutable filesystem
bytes, exact Authority/Closure linkage, a real outbound serialization seam,
secret-safe transport capture, provider behavior, usage, latency, safety,
first-attempt behavior in a live Runtime, Task Effect, or Semantic Use in a
model.  Independent review rejected v0.2; Runtime/provider work remains
unauthorized pending a fresh offline revision and review.
