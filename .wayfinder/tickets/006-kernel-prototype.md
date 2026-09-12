---
title: Kernel primitives prototype
type: prototype
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: []
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
Can plugin/scope/service/event with reversible registration be done in ~300 lines of Elisp such that mount → register → unmount leaves no trace, without `unload-feature`? Build a throwaway prototype in the scratchpad, drive it with a fake transport, and react to its shape before the spec commits to it.

## Resolution (2026-09-11, sc-leader)

**Answer: yes.** Plugin / Scope / Service / Event / Stage with reversible
registration fits in **250 lines of Elisp** (`.wayfinder/research/kernel-prototype.el`),
and mount → register → unmount leaves no trace — no `unload-feature`, no
global symbols touched. The drive script
(`.wayfinder/research/kernel-prototype-drive.el`, run with
`emacs -Q --batch -l …`) passes 13 checks with a fake transport:

1. default mount list runs one Request end to end (admit → collect-context →
   assemble-prompt → transport → render → run-completed event);
2. Scope tree global → Preset → Run: a Preset-mounted plugin shadows the
   global `history` collector and folds the prompt; the global scope is
   unaffected; disposing the Preset reclaims its layer and its plugin;
3. losing the `model-transport` provider suspends the `driver` (pending, its
   `driver` service withdrawn); mounting a *different* transport re-applies
   the driver, which now uses the new one (Cordis epoch semantics);
4. config: defaults filled; invalid config → plugin `failed`, nothing
   registered, the good instance keeps answering; reconfigure = validate,
   unload, re-apply, and dependents re-apply too;
5. disposing the global Scope: zero fibers, zero layers, zero providers;
   plugin *definitions* survive (they are data, not state).

**Shape findings the spec must carry (each cost one bug in the prototype):**

- **Registrations land in the mount Scope, effects in a private child.**
  My first version gave `apply` a fresh child Scope for both; then nothing a
  plugin registered was visible to its siblings or the driver (lookup only
  walks ancestors). Cordis avoids this because a fiber's ctx carries the
  mount scope's *key*. Spec: a Scope has `parent / effects / layers /
  layer-owner`; `apply` receives an effect-scope whose `layer-owner` is the
  mount scope.
- **Registry keys are compared with `equal`.** Stage/Event registries are
  keyed by lists like `(stage collect-context)`; `assq` silently created a
  duplicate layer per registration.
- **Notify is a synchronous fix-point loop with a re-entrancy guard.** A
  provider that appears *during* another plugin's `apply` (inside notify) is
  picked up on the next pass; Elisp needs no microtask deferral.
- **"No trace" is a convention the Kernel cannot enforce.** Elisp cannot stop
  a plugin's `apply` from `defun`/`setq`-ing globals. The rule "a plugin
  contributes only through its Scope" must be an Invariant checked by review
  (or a load-time diff of `obarray` in a verification step), not by code.
- **`sk-define-plugin` shape works as `name / :inject / :config / :apply`**
  with config spec = alist `KEY -> (PREDICATE . DEFAULT)`; a plist validator
  is enough, no schema library.
- Ordering among several dependents of one service is unspecified (as in
  Cordis); the prototype re-applies in mount order.

**Not prototyped (left to 001/002/005):** a Run Scope created per submit and
disposed at the terminal; the Invariant registry; async Sinks; what
"observe" stages return. Nothing in the prototype depends on them.
