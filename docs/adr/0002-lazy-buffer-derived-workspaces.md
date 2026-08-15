---
status: proposed
---

# Derive Emacs workspaces lazily from live buffer context

Superchat runs inside Emacs, where the authoritative working state may be a
live buffer, a virtual buffer, or a dynamically related group of buffers rather
than a filesystem tree.  We therefore propose that Workspace be an optional,
derived scope for editor resources—not the mandatory parent of every Session or
Run—and that direct Chat remain valid with no Workspace attached.

## Decision

- A Buffer is the smallest editor resource; a Buffer Group or related project,
  tab, or session scope may provide a larger Workspace scope.
- Workspace derivation is lazy.  A new Chat starts with no Workspace and only
  resolves one when an operation needs an editor scope or the user explicitly
  attaches context.
- Asynchronous Requests capture their originating editor context at submission
  time; later resolution must not infer scope from an unrelated `current-buffer`.
- A Session or Run may remain workspace-less and may touch more than one scope;
  Workspace is not a one-to-one parent of conversation history.
- The Superchat chat buffer is a presentation surface, not an implicit
  Workspace resource.  Files remain an optional persistence adapter for live
  editor resources, not the identity of the Workspace itself.
- The same live Buffer Context also provides a lazy Command Capability View:
  major mode, minor modes, active keymaps, and buffer-local state suggest
  relevant commands, but do not grant Authority.  Commands remain subject to
  the shared execution policy and audit boundary.
- Command discovery is progressive rather than an eager export of every
  `M-x` command.  Direct Elisp evaluation remains an escape hatch; it is not
  the primary semantic representation of an Agent action.

## Consequences

- Global or conversational operations can run without manufacturing a fake
  global Workspace.
- Buffer- or Workspace-mutating operations must resolve an explicit or captured
  target before applying effects; missing scope is a request/approval boundary,
  not permission to guess.
- Tape may record a derived scope or attachment when it is actually used, but a
  simple Chat does not materialize Workspace state merely by being created.
- A future command-oriented Agent surface should derive candidate commands
  from the target Buffer Context and record recognized command execution as a
  structured action, without making a static command registry a prerequisite.
- The current filesystem-oriented Workspace binding remains an implementation
  boundary to revisit; this ADR does not authorize a migration or code change.

## Considered options

- **Eager directory Workspace per Session:** rejected because direct Chat,
  virtual buffers, and unsaved editor state do not require or even possess a
  meaningful directory.
- **One Workspace as the parent of every Session:** rejected because
  conversation scope and editor-resource scope are orthogonal, and one Run may
  legitimately cross multiple buffer groups.
- **Implicit global Workspace fallback:** rejected because it obscures the
  target of mutating operations and contaminates authority and audit scope.

## Research basis

- [重新思考 Agent 和编辑器](https://ecnelises.com/2026/08/rethinking-agents-and-editors/)
