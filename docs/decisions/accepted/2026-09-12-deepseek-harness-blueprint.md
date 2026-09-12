# Decision: DeepSeek Harness is the blueprint; superchat is Emacs's AI layer

Status: accepted

## Problem

superchat's v2 Kernel already uses the plugin, Scope, Service, dependency-activation and Invariant ideas of Cordis, the framework under DeepSeek Harness (dsh). The product built on it has grown by addition for months and lacks a single reference for what shape it should converge to and what "done" means for the product as a whole.

## Decision

The user decided on 2026-09-12: "以 deepseek harness 作为蓝本，来重构 superchat 的架构", with the goal "希望 superchat 成为 emacs 的 ai layer，就好像 ormachy 一样，ai 通过 superchat 可以完全控制 emacs", where "ormachy" is DHH's Omarchy. Two consequences are therefore settled:

- The reference architecture is dsh at the local checkout `~/Documents/reference-code/deepseek-harness` (commit `c389f96`).
- The product target is a complete, opinionated default environment, not a toolkit: a fresh install works without assembly, and the AI reaches Emacs objects through superchat.

dsh's specific mechanisms (a small spine with everything else a plugin, capability seams as Definition + Provider + Consumer, one durable log as the only source of model-visible history, an inbox and waterfalls) are what the blueprint contains, not what this decision approves; each is adopted through its own proposed record and the roadmap.

The two target journeys from 2026-09-10 remain the acceptance lens: the chat window, and invocation on the buffer being edited.

What this decision does not settle: the concrete module split, the extension-point API, the tool surface, or any removal from the default set. Those are separate records, see [the roadmap](../../roadmap.md).

## Alternatives considered

**Bub (bub.build) as the reference** was named by the user on 2026-09-10 as a small-core reference. It remains a useful minimal example, but dsh is the chosen blueprint because it also prescribes the documentation regime and the seam discipline the user asked for.

**No external blueprint; continue simplifying in place.** Rejected by the user's statement; the day's audit showed the in-place work kept re-partitioning rather than reducing.

## Consequences

Every later architecture proposal is judged by whether it moves toward the dsh shape and toward the Omarchy-style complete default. Where dsh's seams are about a host operating system (filesystem, shell, sandbox), superchat's are about Emacs objects; that translation is design work, not a given.
