# Plugin composition contract

This document describes the implemented composition interfaces. It is not a completion claim for the earlier v2 specification. The [roadmap](../roadmap.md) owns remaining work, the [architecture map](../architecture.md) locates implementations, and the [historical migration log](../../.wayfinder/map.md) records earlier specifications and accepted repair batches.

## Plugin definition and activation

`superchat-define-plugin` defines data; mounting applies it. Its `:inject` value is a list of required Service names, `:config` is a list of `(KEY PREDICATE DEFAULT [DOC])` entries, and `:apply` receives an effect Scope and validated configuration. Values in this macro are evaluated: literal lists must be quoted.

`superchat-plugin-mount`, `superchat-plugin-unmount` and `superchat-plugin-reconfigure` manage one installation. Unknown configuration keys and invalid values fail validation. Reconfiguration validates before withdrawing the old installation; changing configuration is not an in-place mutation of a live plugin.

A missing injected Service leaves the plugin `pending`. When the required provider set is available it becomes `active`; losing or replacing a provider unwinds its effects and reapplies it when dependencies are available. A failed `apply` is not a successful partial mount. The plugin's exact, self-removing cleanup owns its registrations; an old cleanup cannot remove a later same-name installation.

`superchat-plugins` is the current global composition list. Each element is a plugin name or `(NAME . CONFIG-PLIST)`. Defining a feature, an autoload or a tool does not prove that feature has its own plugin definition. Use `superchat-plugin-report` and the source definitions to inspect installations.

## Scope and registration

A Scope is a process-local registration context. Contributions are visible to it and its descendants, not to siblings or ancestors. Nearest-layer definitions shadow inherited names; duplicate identifiers in one layer are rejected. Registrations made through a plugin's effect Scope land in its mount layer, while cleanup remains owned by that exact installation.

Scope disposal withdraws registrations, disposes children before parent effects and unlinks the Scope. Explicit unregister and disposal consume the same one-shot cleanup. Empty layers and consumed cleanup effects do not accumulate through repeated mount/unmount cycles.

The registering interface determines the default target:

- Tools, commands and Authority registrations use explicit `:scope`, then the applying plugin's effect Scope, then the execution Scope, then global.
- Stage add/remove use explicit `:scope`, then the applying plugin's Scope, then global; a Stage callback does not implicitly turn later Stage registrations into Run-local ones.
- Event listeners take an explicit Scope through `superchat-on`.
- Surface names are package-wide Envelope vocabulary. A plugin-added Surface has Scope-accounted cleanup, but the Surface list is not a Preset-shadowed registry.

Arbitrary Elisp global mutation, timers or external effects are not automatically reversed. Register cleanup through the supplied Scope; the contribute-only-through-Scope Invariant is advisory, not an Elisp sandbox.

## Services and execution lookup

A Service declares required methods and has one active global provider. `superchat-service-provide` validates the method table. Preset Scopes cannot provide a shadow Service: provider replacement affects dependents globally.

`superchat-service-call SCOPE NAME METHOD ...` selects the caller's execution context, not a private provider. A nil Scope inherits the current execution Scope and otherwise uses global. Explicit `superchat-scope-global` chooses global. `superchat-service-get` is lookup-only: pass the Scope explicitly when inspecting one; nil there retains its global lookup meaning.

Pipeline Stage lookup, Runtime notifications, provider attempts, asynchronous tool callbacks, terminal Sink delivery and FIFO dispatch restore the accepted execution Scope. Runtime holds this reference privately while needed and releases it at quiesce; it is not serialized into Envelope, Tape or public status. Services remain global even when their methods execute for a child Scope.

## Scoped tools, commands and Authority

Tools and commands are read through the caller's visible Scope layers. A tool's materialized callable retains the exact installation identity paired with its Effect resolver and approval preview. Revocation invalidates an old descriptor rather than combining its implementation with a replacement's authorization metadata.

Authority folds contributed policy constraints through the caller's ancestry. `deny` is stricter than `ask`, which is stricter than `defer`; contributed `allow` is treated as `defer`. Child shadowing cannot relax a parent's constraint. The effective decision is made at execution, not inferred from a cached model-facing tool list.

Commands preserve user-file template fallback when a handler declines or the command provider is absent. Intentional same-layer tool or command replacement requires explicit unregister followed by registration. MCP refresh validates replacement data before changing its own installation; unchanged discovery preserves the existing installation.

## Preset composition and lifetime

Preset data carries a native `:plugins` composition with unique names and proper keyword configuration plists. Markdown frontmatter supports an inline list such as `plugins: [foo, "bar"]`, including `[]`. Map, nested and multiline forms are rejected; they are not silently accepted as full YAML configuration support.

Each Chat View activation creates an exact `preset/<name>` Scope directly under global. Two Views choosing the same name receive different Scope objects. Preset data contains no Scope. Runtime creates no automatic `run/<id>` Scope, and Session identity is not a Scope level.

The View owner validates definitions and configuration, mounts a candidate and checks its state before publishing the replacement. `pending` dependencies are legal. Candidate/configuration failure preserves the old activation while its owner is live; candidate work is retired before cleanup. If a callback kills, disables or aborts the exact owner View, both the old and candidate Scopes are released and no replacement is published. A configuration fact already appended to Tape is not rolled back.

Switching, explicit deactivation, disabling `superchat-mode`, an approved View kill or changing major mode calls Runtime retirement on the old Scope before disposing its registrations. A vetoed View kill does not perform that teardown. Neither step closes another user or Session-owned buffer. Restore of the same live Preset reuses its Scope and plugins without another activation fact; View text may be rendered again.

Normal chat captures one execution Scope through Preset preparation and queue/submit. If a `:pre` callback switches Preset and retires that Scope, the prepared ordinary request is rejected rather than rerouted. Explicit `>skill` and command mode-switch results may deliberately select the freshly active View Scope. Explicit caller Scope wins over ambient View selection. A plain View uses global and does not retire it or sweep unrelated Runs by Session identity.

## Retirement, withdrawal and evidence

`superchat-runtime-retire-scope` prohibits admission and continuation through one non-global Scope and its descendants, then uses Runtime's existing cancellation path. It does not itself unregister plugins or close buffers. The Preset owner composes retirement with disposal; generic Scope disposal does not become another lifecycle owner.

Withdrawing a `model-transport` provider, including through provider Scope disposal or returned undo, triggers Runtime's provider-epoch retirement. All matching old Runs become unable to continue before reentrant callbacks can act. Provider replacement does not make an old Run eligible for the new provider.

Cancellation does not prove an entered external Effect stopped. Such a Run can terminate as `audit-incomplete` while the Effect remains open; a genuine one-time late result can settle that Effect without changing the Run terminal or restarting execution. Queue handling, terminal recovery and exact delivery follow the [request lifecycle contract](../protocols/request-lifecycle.md).

## Verification and change direction

`scripts/kernel-check.el` exercises composition primitives; the canonical smoke script and R1–R9 exercise product interfaces, withdrawal, terminal evidence and scoped execution. See [development verification](../agents/development.md) for dependencies and invocation. Existing baselines are not evidence for a proposed replacement that has not been run.

The accepted retirement of fixed Stages and other future main-line changes are recorded in the [remaining plan](../roadmap.md). They do not change the current methods or authorize removing compatibility and safety behavior ahead of a verified replacement.
