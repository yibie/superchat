# Kernel Review

status: active
scope: every revision of `KERNEL.md` after K4 — adding, rewording, re-anchoring or retiring an entry or an authority row
applies_when: `learn` is about to write `KERNEL.md`, and `check` is verifying a Kernel revision
owner: project maintainer
source: `E-20260827-009` (six defects in a K2 that had passed every gate), `E-20260827-011` (twenty more in K3, all over-general sentences, and one self-contradiction between two entries), `E-20260828-001` (40 citations that pointed at the wrong file; 8 were caught by the K3-era `check`, 32 passed every mechanical check)
verification: the Evidence entry that records a Kernel revision names the `pairs` report it was written against, carries a "Where counterexamples were hunted" list covering every new universal sentence, and names the fresh-context review that preceded `learn`; `check` reports a revision lacking any of the three as not verified

## Why

Four rounds of review found 6, 4, 20 and 40 defects in Kernel revisions that had
already passed `check-kernel.sh`, the anchor check and the test suite (per
`E-20260827-012` / `E-20260828-001`). Every
factual defect was one of two shapes: a sentence with a universal quantifier
that the code did not support, or a citation that landed on real code the
sentence was not about. Neither shape is visible to a machine gate. One
defect was the same rule stated two contradictory ways in two entries, because
a correction reached one and not the other.

## Rules

1. **Paired entries change together.** Before editing an entry, run
   `pairs` (`docs/runbooks/kernel-check.md`) on the current `KERNEL.md`. Every
   other entry that cites a construct the edited entry cites is re-read in the
   same pass, and any of them that states the same rule is changed in the same
   revision. A revision that edits one member of a pair and not its partner is
   incomplete, not merely risky: that is exactly how K2 came to contradict
   itself.

2. **Every universal sentence records its counterexample search.** For each
   new or reworded sentence containing `only`, `every`, `never`, `exactly`,
   `always`, `single`, `sole`, `nothing`, `nowhere`, `all` or `no other` — the
   terms `pairs` lists — the Evidence entry for the revision says where the
   counterexample was looked for: which grep over which files, which function
   was read branch by branch, which callers were enumerated. A universal
   sentence with no recorded search is a candidate, not a fact, and does not
   enter `KERNEL.md`.

3. **A fresh-context, default-to-refute review precedes `learn`.** Before
   `learn` closes a Kernel revision, an independent reviewer on a fresh
   context, told to assume the author got something wrong, re-opens every
   changed entry against the current tree. Its findings are re-verified before
   acceptance (reviewers are wrong too: 6 of 26 accusations in the K3 round
   were overturned). The review and the re-verification are named in the
   Evidence entry. The author's own adversarial pass does not satisfy this
   rule; the two K3 defects the fresh reviewer found had survived that pass.

## What the rules do not cover

- A re-anchor that changes no sentence (a K4-style revision) needs rule 1
  only for the entries whose citations move, and rule 3 in full; it has no new
  universal sentences unless it adds an entry.
- The rules do not replace the mechanical gates in
  `docs/runbooks/kernel-check.md`; they run after those pass.
- Whether a confirmed behaviour is semantics or a defect is a `plan`
  judgement, not a review finding; the review reports it, `plan` decides.
