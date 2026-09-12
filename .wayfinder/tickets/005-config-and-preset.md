---
title: Plugin config and Preset composition
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: []
---
## Question
How is a plugin's `:config SPEC` declared and validated in Elisp, how does a Preset name plugins and their config, and how is the Scope tree (global → Preset → Run) created and disposed per Session/Run? Include what `superchat-plugins` (the default mount list) looks like.

## Resolution (2026-09-11, sc-leader)

**Plain language.** A plugin's config is a small plist checked against a
declared spec every time the plugin is (re)applied; bad config means the
plugin is `failed` and registered nothing. A Preset is a named bundle that
says *which plugins to mount on top of the defaults, with what config*; using
a Preset creates a child Scope, and every Run gets a grandchild Scope that
dies with the Run.

**Config spec (Elisp).** In `superchat-define-plugin`:
`:config ((KEY PREDICATE DEFAULT [DOC]) …)` — prototyped in ticket 006 as
`(KEY PREDICATE . DEFAULT)`; the spec form adds an optional doc string.
Validation = fill defaults, then `PREDICATE` per key, unknown keys rejected.
No schema library. `superchat-plugin-reconfigure NAME CONFIG` = validate,
unload, re-apply (dependents re-apply too, per 006).

**`superchat-plugins`** (the default mount list, a `defcustom`): a list whose
elements are `NAME` or `(NAME . CONFIG-PLIST)`. Mount order is irrelevant for
dependencies (PENDING until Services exist), but it is the tie-break order
for same-Stage contributors of equal Scope, so the list is written in the
order the built-ins should run. `(setq superchat-plugins (remq 'memory
superchat-plugins))` before `(require 'superchat)` disables a built-in;
`superchat-plugin-unmount` / `-mount` do the same at runtime.

**Preset.** `superchat-preset` gains one slot, `:plugins`, same element
shape as `superchat-plugins`. Its existing slots stay and are *consumed by
plugins* via `:context (:preset P)` in the Envelope (`tools` → `select-tools`
baseline reads it; `model`/`temperature`/`max-tokens`/`reasoning` →
`transport-llm`; `skill-body` → a `preset-persona` collector auto-mounted in
the Preset Scope when the slot is non-empty). YAML/Markdown preset files
declare `plugins:` as a list of names or `{name: …, config: {…}}` maps.

**Scope tree and lifetimes.**
- `global`: created at `(require 'superchat)`; `superchat-plugins` mounted here.
- `preset/<name>`: child of global, created by `superchat--activate-capability`
  (the current preset switch) and disposed when the Preset is deactivated or
  replaced; the Preset's `:plugins` mount here, so a Preset can shadow
  Stage/Event contributors, tools, commands and policies — never Services (001).
- `run/<run-id>`: child of the active Preset Scope (or global), created by
  `driver.submit` before `parse`, passed to every Stage and Event as the
  `scope` argument, disposed after the terminal Sink emission. Plugins that
  need per-Run state register it here (e.g. inline reference cycling);
  nothing survives the Run by accident.
Session is *not* a Scope level: Sessions are Tape identities, not
registration contexts; a plugin that needs per-Session state keys it by
session-id in its own effect-managed table.
