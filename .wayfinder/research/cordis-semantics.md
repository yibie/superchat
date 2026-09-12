# Cordis semantics: inject re-activation, scoped registries, config validation / patch reload

Sources: the vendored Cordis framework and dsh packages under
`/Users/chenyibin/Documents/reference-code/deepseek-harness` (abbreviated `H/` below).
All line numbers were read from the files on 2026-09-11.

## 1. What happens to `inject` dependents when a service provider unmounts

**Plain language.** A plugin that says `inject: ['foo']` is not "started once
foo exists"; it is *kept alive only while foo exists*. When the provider goes
away, every dependent is unloaded (all its effects unwound) but stays
registered as a PENDING fiber. When any provider of `foo` becomes active again
(the same one or a different one), the dependent's `apply` runs again from
scratch. Even swapping one active provider for another triggers unload + reload.

**Evidence.**
- Docs state the contract: dependents are "unloaded too, and load again when
  the service returns" (`H/docs/cordis-tutorial/03-services.md:76-78`);
  `ctx.inject(deps, cb)` is "unloaded and re-run whenever a required service
  changes" (`H/docs/cordis-api/registry.md:26`).
- Provide/unprovide: `ReflectService.provide` registers the impl as an effect
  of the provider's fiber; its disposer deletes the store entry, calls
  `notify([name])`, then *awaits every dependent's `fiber.await()`* before
  removing the provider's own store entry so dependents' disposers can still
  reach the service ("ensure self access before dependencies cleanup")
  (`H/vendor/cordis/src/reflect.ts:277-305`).
- `notify` walks every runtime/fiber in the registry, re-checks each fiber
  that injects the name (`_checkImpl`), then `_refresh()` (`reflect.ts:314-336`).
- `_checkImpl` only accepts an impl whose fiber is ACTIVE (`_getImpl(name, true)`)
  and whose optional `check()` predicate passes (`H/vendor/cordis/src/fiber.ts:597-609`,
  `reflect.ts:237-243`). `_refresh` builds an *epoch string* from the uid of
  each injected provider's fiber; any missing impl sets epoch `INACTIVE`
  (`fiber.ts:611-623`).
- `_setEpoch`: INACTIVE -> something = LOADING/`_reload()`; anything else that
  changed (including provider A's uid -> provider B's uid) = UNLOADING/`_unload()`;
  `_unload` ends by reloading if the epoch is no longer INACTIVE, so a provider
  swap is exactly "unwind, then re-run apply" (`fiber.ts:625-639, 675-696`).
- Reload is deferred one microtask and re-validates config each time
  (`fiber.ts:646-657`). The dependent is *not disposed*: `uid` stays set, so it
  is PENDING, not DISPOSED (`fiber.ts:574-579`). Only `fiber.dispose()` /
  `registry.delete` set `uid = null` (`fiber.ts:265-297`, `registry.ts:258-267`).
- Provider ACTIVE/non-ACTIVE transitions also notify: `_updateState` calls
  `reflect.notify` for every impl owned by the transitioning fiber
  (`fiber.ts:581-595`), so a provider that FAILED or is UNLOADING counts as absent.

**What `inject` guarantees about ordering.**
- Load order in config is irrelevant; a dependent stays PENDING silently until
  every injected name has an ACTIVE, check-passing provider (`03-services.md:59,72`;
  `06-composition-and-hmr.md:63`).
- Inside `apply`, `ctx.foo` resolves through the fiber's frozen `store`
  snapshot taken at load (`fiber.ts:647`, `reflect.ts:152-167`); reading a
  non-injected service via `ctx.foo` throws "cannot get property without inject",
  `ctx.get('foo')` is the optional probe (`reflect.ts:144, 233-235`; `03-services.md:82-90`).
- Fiber disposers are collected in reverse registration order but *started
  concurrently* (`Promise.all` over `DisposableList.clear()`, `fiber.ts:676`,
  `utils.ts` `clear()` reverses). Inside one `ctx.effect`, disposers run
  strictly in reverse (`fiber.ts:427-442`).
- No ordering among multiple dependents is promised: `notify` iterates the
  registry map in insertion order and each unload/reload is its own async task.

**Superchat should copy:** the epoch idea (dependent = function of the set of
active providers; any change = unwind + re-run), PENDING as a legitimate
silent state, and "provider disposer waits for dependents before dropping its
own value". **Can skip:** the microtask deferral, the `check` predicate,
concurrent async disposers (Elisp is single-threaded: run disposers in reverse
order synchronously), and the `ctx` proxy that forbids un-injected reads.

## 2. Scoped registries (dsh-scope): shadowing, lookup, reclamation

**Plain language.** A registry keeps one always-present "global" table plus one
lazily-created table per scope key. Scope keys form a parent chain. Looking up
from a scope means: start with the global table, then overlay each ancestor's
table from farthest to nearest, then the scope's own table last, so the nearest
definition of a name wins. A scope's table is thrown away the moment its last
entry is removed. Registrations are tied to the fiber of the context they were
made through, so disposing the scope unwinds them.

**Evidence.**
- Contract: "a child scope sees its ancestors' layers (nearest shadows
  farthest)"; "reads never create layers, `merge()` materializes
  insertion-ordered named shadows along the scope chain"; "a scoped layer is
  reclaimed only when its whole aggregate is empty"
  (`H/packages/core/scope/README.md:12, 46, 73`).
- Chain: `scopeChainOf(key)` returns `[key, parent, grandparent, ...]`;
  parents live in a `WeakMap`, bound once with cycle check
  (`H/packages/core/scope/src/index.ts:39, 54-59, 72-102`).
- `chainLayers(scope)` returns existing layers farthest-first, exact scope
  last, skipping absent ones (`store.ts:192-199`). `merge(scope, pick)` =
  `new Map(global)` then `set` each chain layer's entries in that order
  (`store.ts:208-217`). `peek(scope)` is deliberately chain-blind for "own"
  contributions (`store.ts:174-183`).
- Concrete consumer: the tool registry builds `inherited = global tools, then
  each chain layer except own`, and resolves presentation mode by scanning
  `chainLayers` nearest-first (`H/packages/core/tools/src/index.ts:892-903, 1140-1156`).
- Registration: `ScopedLayers.effect(ctx, action)` reads `scopeOf(ctx)`,
  creates the layer on first write, runs the mutation, and yields an undo that
  removes the entry and deletes the layer `if layer.isEmpty()`; a throwing
  action deletes a freshly created empty layer (`store.ts:230-266`).
  `NamedEntries.insert` throws on duplicate name within one table and returns
  an idempotent undo (`store.ts:43-54`).
- Ownership: `createScope(ctx, key)` mounts a no-op plugin and tags
  `fiber.ctx.extend({[kScope]: key})`; `dispose()` disposes that fiber, so
  every `ctx.effect` made through the scoped context unwinds (`index.ts:120-147`).
  Nested contexts still carry a single nearest tag; hierarchy is key-level
  only (`README.md:102`).
- Agent presets mount a subtree into the agent's scope and refuse an unscoped
  context ("registrations would apply to every agent")
  (`H/packages/preset/agent-presets/src/mount.ts:379-385`).

**Superchat should copy:** global + per-scope alists keyed by an opaque scope
object, a parent chain, lookup = global then farthest->nearest overlay, lazy
layer creation and delete-when-empty, and tying each registration's undo to
the scope's lifetime. **Can skip:** `AnonymousEntries` symbol keys (a cons
cell or gensym is enough), the `Scoped<T>` type brand, the invariant companion,
and the event-carrier filter unless superchat adds scoped events.

## 3. Config validation at mount and on patch reload

**Plain language.** A plugin exports a `Config` schema. Every time the plugin
is about to run (first load *and* every reload), Cordis runs the schema over
the raw config; bad config throws a `ValidationError`, the fiber is marked
FAILED, and `await`ing it rethrows. Changing a plugin's config later does not
patch it in place: it validates the new config first, then does a full
unload + reload with the new config. The one exception is container plugins
(Group / Include), which intercept the update and diff their child rows by id,
restarting only the rows whose config actually changed.

**Evidence.**
- Validation: `resolveConfig(runtime, config)` calls `Config['~standard'].validate`;
  async validators are rejected; issues become `ValidationError("invalid config: ...")`
  (`H/vendor/cordis/src/fiber.ts:19-62`). It runs inside `_reload` after the
  `internal/config` waterfall, on every activation (`fiber.ts:641-657`);
  failure sets `_error`, logs, state FAILED, and `fiber.await()` rethrows
  (`fiber.ts:659-664, 704-710`). Schemastery exposes `~standard` and fills
  defaults (`H/vendor/schemastery/src/index.ts:275-292`; `05-config.md:51, 64-68`).
  The loader's `internal/config` hook interpolates `!!js` expressions except
  for Group/Include rows (`H/vendor/loader/src/index.ts:95-101`).
- Live update: `Fiber.update(config)` validates first, then runs the
  `internal/update` waterfall whose default tail is `restart()` =
  `_setEpoch(INACTIVE)` + `_refresh()` (full unload, then reload with
  `this.config` = new config) (`fiber.ts:718-753`). If the fiber is not ACTIVE
  it just stores the raw config and lets the next activation validate it
  (`fiber.ts:739-746`).
- Loader entry diff: `Entry.update` computes a key diff; a `config`-only diff
  calls `fiber.update(config, true)` (restart in place, same fiber); a diff in
  `name`/`inject`/`group` disposes the fiber and starts a new one, rolling back
  to the previous plugin on failure; `disabled` disposes it
  (`H/vendor/loader/src/config/entry.ts:114-122, 142-246`). The loader logs
  "reload" on `internal/update` and writes config back to the file unless
  `noSave` (`loader/src/index.ts:103-115`).
- Containers: `Group` handles `internal/update` itself with a transactional
  by-id diff (create/update changed rows, remove missing ones, roll back on
  failure) (`H/vendor/loader/src/config/group.ts:59-106, 122`). `Include` does
  the same after applying patches (`H/vendor/include/src/index.ts:206-212`).
- Patch reload: `watchUserPatches` registers the profile's `cordis.patch.yml`
  with HMR; on change it calls `entry.update({config:{...includeConfig, patches}})`
  on the root Include (`H/packages/boot/app-boot/src/index.ts:250-283`).
  A patch replaces a row's whole `config` (shallow key assignment, no deep
  merge) or inserts rows (`include/src/index.ts:58-125`;
  `app-boot/README.md:143`). Only `web`/custom profiles reload live; a rejected
  edit leaves the last good app running (`H/docs/architecture.md:27-29`;
  `app-boot/README.md:57`).

**Superchat should copy:** validate-before-run on every activation with a
loud aggregated error and a FAILED state; "config change = validate new config,
then unload + re-apply" as the default; id-keyed row diffing so only changed
rows restart; whole-config replacement semantics for overrides. **Can skip:**
Standard Schema / schemastery (a plist validator or `cl-check-type` per key is
enough), the `internal/update` waterfall extensibility, `!!js` interpolation,
file watching / HMR, and write-back of config to disk.
