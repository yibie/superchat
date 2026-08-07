# Superchat 当前 Handoff — 2026-08-09

## Reactivation prompt

```text
We are continuing Superchat from this handoff. Read this file, root AGENTS.md,
docs/research/superchat-experiment-history-condensed-2026-08-09.md, and
docs/research/superchat-evidence-boundaries-2026-08-09.md first. Inspect the
current repository state and verify what still applies. Do not load archived
experiment material unless a specific audit question requires it. Continue
from v03-remediation without assuming old chat context is available.
```

## Repository

- Path: `/Users/chenyibin/Documents/emacs/package/superchat`
- Jujutsu working-copy change: `oyyuxnvp`; parent bookmark:
  `wip/superchat-20260810`
- Worktree is intentionally dirty. Preserve unrelated edits.
- Root `AGENTS.md` is the navigation/instruction source. `docs/AGENTS.md` is a
  historical journal, not instructions.

## Current goal

Determine whether a bounded Artifact Semantic Use observation can be made identifiable
without confusing Access, selected/replaced answers, control failure, Task Effect or
Learning.

The current implementation is a pure offline scorer prototype only. No Runtime harness
or provider/model request is authorized.

## Current active files

```text
experiments/artifact-semantic-use-v0/DECISION-MAP.md
experiments/artifact-semantic-use-v0/revisions/v0.2/README.md
experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-logic-v0.2.el
experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el
```

Everything else under the historical experiment roots was summarized and retired into
`experiments/archive/` on 2026-08-10.

## Completed immediately before handoff

- v0.1 independent review returned VETO: duplicate/replacement records, truthy false
  sentinels, explicit-empty schedule fallback, ambient case-folding, weak decoy grammar,
  sham closure conflation and model-visible oracle risk.
- User selected Codex GPT-5.6-Luna with `max` reasoning as v0.2 Coder.
- Luna created a strict 30-record offline scorer and TUI in `revisions/v0.2/`.
- Commander returned one classification regression: a witnessed malformed answer had
  been called missing first-attempt evidence. Luna corrected it so bytes/chronology stay
  proved while parser failure is `protocol-failure/not-supported`.
- Commander independently replayed the original 31-record replacement attack, truthy
  `:false` content/withheld proofs and malformed effect output. All now fail closed.
- Fresh source-facts and code-adversary reviewers independently returned VETO.  The
  experimental-validity reviewer accepted only the bounded descriptive wording.
- Reviewers and Commander reproduced model-visible target oracle acceptance, missing
  prompt/Artifact acceptance, duplicate plist/schedule-key ambiguity, unjoined Artifact
  bytes and structure failures outside the actual cohort/audit closure.

## Commands already run

```bash
emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el audit
# exit 0; all 18 named checks PASS

emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el broken
# exit 1

printf 'q\n' | emacs -Q --script experiments/artifact-semantic-use-v0/revisions/v0.2/remediation-prototype-v0.2.el
# exit 0
```

Both Elisp files passed `check-parens`. README schedule and code schedule matched exactly.
Frozen schedule SHA-256:

```text
ca0a19a6f273033ac0858de65e9ead70a57d0c664837eac622be38faa9db21bf
```

## Current claim boundary

v0.2 is VETO/`not-identifiable` and frozen as a rejected offline revision.  Its built-in
audit still proves that the 18 implemented fixtures pass, but independent attacks show
that invalid records can still receive `consistent-with-two-input-use`.  It proves no
live Runtime provenance, transport capture, provider behavior, Semantic Use, Task Effect
or Learning.

## Next steps

1. Leave v0.2 code unchanged as the rejected review specimen.
2. If offline remediation continues, create a fresh revision that rejects ambiguous
   plist shapes, checks the complete model-visible envelope, joins Artifact bytes to the
   scheduled condition, and includes structure validation in closure and process exit.
3. Add every reproduced attack as a runnable check and repeat all three independent
   review axes without the repair author reviewing their own code.
4. Keep Runtime/provider work closed until a later revision passes that review.  Even
   then, a separate ticket must decide whether a sacrificial preflight is worth a new
   contract; it cannot directly authorize an experiment.

## Do not touch

- Runtime, Tape, Authority, Closure, Workspace, Workflow, Memory or product schemas.
- Existing user changes elsewhere in the dirty worktree.
- Archived experiment files unless a named audit question requires extraction.
- Codex credentials, skills, plugins or unrelated sessions.
