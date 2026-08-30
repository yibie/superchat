# Workflow edit entry

status: confirmed
revision: 1
context_refs: `CONTEXT.md` Workflow vocabulary; `KERNEL.md` K19 Workflow identity, integration layering, and Architecture boundaries; `docs/architecture-review-20260905/02-problems.md` P7; `docs/architecture-review-20260905/03-refactoring.md` R3

## Problem and goal

Superchat discovers and executes standalone `.workflow` recipes, and
`superchat-workflow--file-path` can name a safe path for a recipe that does not
yet exist.  There is no Workflow-specific user entry for opening an existing
recipe or preparing a new one for ordinary Emacs editing and saving.

Add `M-x superchat-workflow-edit`: completion includes existing recipes while
accepting a new name; an existing safe recipe opens unchanged, and a new safe
name opens a file-visiting buffer with a minimal valid recipe template.  The
user remains the only party that saves or executes it.

## Unchanged contracts

- `superchat-workflow-directory` remains the single configurable recipe
  directory.  There is no project lookup, layered registry, or second store.
- Existing discovery continues through `superchat-workflow--list`, safe
  identity resolution through `superchat-workflow--resolve`, new-target
  calculation through `superchat-workflow--file-path`, and loading through
  `superchat-workflow--load`.
- Existing files retain their actual names and contents.  In particular, an
  existing NFD filename is not renamed merely because its lookup name is NFC
  equivalent.
- `.workflow` remains a linear recipe: blank and comment lines are ignored;
  every other line is an executable step with the existing annotations and
  substitution rules.
- Opening an editor performs no Runtime submission, provider call, recipe
  execution, automatic recipe write, or durable product-state write.  Existing
  lazy directory creation may still create `superchat-workflow-directory`.
- Runtime, Authority, EditorWorkspace, LLM/transport, Tape, and the integration
  layering rules do not change.

## Decision and boundaries

1. Add public autoloaded interactive command
   `superchat-workflow-edit`.  Its minibuffer completion candidates come from
   the existing Workflow list and completion does not require a match.
2. Resolve the supplied name before opening any file.  An existing unique safe
   NFC-equivalent candidate opens its canonical actual path.  If no candidate
   exists, normalize the new basename to NFC and ask
   `superchat-workflow--file-path` for the target.
3. Reject empty or non-basename names, `../` and nested paths, unsafe existing
   paths or symlinks, and ambiguous NFC-equivalent candidates before opening a
   target file.  Do not copy these classifiers into the command.
4. A genuinely new target receives a small valid template consisting of a
   format comment and one editable non-comment placeholder step.  Insertion
   changes only the visiting buffer; the path must not exist until the user
   saves normally.
5. Re-entering the command for the same unsaved new target reuses its existing
   file-visiting buffer and preserves the draft exactly; it does not insert the
   template again.  Opening an existing file likewise never replaces content.
6. Ordinary Emacs save behavior is the save layer.  This work adds no custom
   persistence abstraction, automatic execution, or provider-backed authoring.

## Model delta

This is a compatible addition of one user Action Contract: a user may safely
locate and open a Workflow recipe for editing, including preparing a new NFC
basename in the one configured directory.  Recipe identity, discovery,
execution, storage location, and safety rules remain unchanged.

K19's Architecture boundaries identify `superchat-workflow.el` as an
integration but do not yet name Workflow path editing as a separate entry.
The implementation authority is still `superchat-workflow.el`; after
independent verification, `learn` may promote the new Action Contract and
authority-map entry.  Capture and arrange do not edit Kernel knowledge.

## Action Contracts

### Edit a Workflow recipe

precondition: the user invokes `superchat-workflow-edit` with a string that can
be resolved as one existing safe recipe or accepted as one safe direct-directory
basename for a new recipe.

input: one recipe name selected from non-required completion or supplied
directly; existing names are compared under the current NFC resolver rules.

permitted effect: lazily create the configured Workflow directory when absent;
open the unique existing recipe unchanged, or visit the NFC target path for a
new recipe and insert one minimal valid in-memory template exactly once.

invariant: reject invalid, unsafe, symlinked, or ambiguous targets before
opening them; never overwrite an existing file or an unsaved draft; never save,
execute, submit Runtime work, or call a provider automatically; do not rename
an existing NFD file; user `C-x C-s` is the only recipe persistence step.

verification: isolated Emacs tests use temporary directories and real files,
buffers, resolver, loader, and normal save behavior.  They distinguish a new
unsaved buffer from a persisted recipe and prove reopen preservation and all
rejection boundaries.

## Compatibility and migration

Compatible addition.  Existing recipe files, paths, names, contents, commands,
and invocation syntax need no migration.  A new unsaved buffer creates no data
to migrate.  Repository-external callers of private helpers remain unknown,
so their signatures and existing observable behavior are preserved.

## Out of scope

- Changing or removing the `last-known-authority-mode` chain.
- Changing `--resolve` or `--file-path` contracts, deleting `--file-path`, or
  adding a query/path registry.
- Project-local or layered Workflow directories.
- Visual builder, custom save/export, auto-save/run, provider-authored template,
  or storage migration.
- Runtime, Authority, EditorWorkspace, LLM/transport, Tape, runner, or package
  layering changes.
- Publication or release-number decisions.

## Issue map

- `issues/01-edit-entry.md`: one end-to-end tracer Slice from completion and
  safe resolution through opening, ordinary save, rediscovery, and loading.

## Revision notes

- Revision 1 (2026-09-05): captured the user-confirmed Workflow editing Action
  Contract and one-Slice implementation boundary.
