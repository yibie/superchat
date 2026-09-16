# Decision: M4 — surfaces are plugin rows, composition is a profile

Status: accepted — the user approved the roadmap's M4 text on 2026-09-16 ("按照路线图的设计") and the three mechanism points below on the same day ("没问题"); implementation state is in [the roadmap](../../roadmap.md#milestones)

## Problem

The roadmap's M4 says the chat window, inline ask and rewrite, Org Babel, Magit and emacsclient are plugins that a profile row list assembles, that nothing leaves the default set, and that the acceptance is "removing any row of the default profile leaves the rest loading; a `headless` profile answers one prompt with no UI plugin mounted". The mechanism was never designed, and the current tree does not support either half of that acceptance.

Measured on commit `1f3a2324`:

- **The default list does not control the surfaces.** `superchat-plugins` (`superchat-plugin.el:236`) has 27 rows. Org Babel (`ob-superchat.el:43`), Magit (`superchat-magit.el:40`), inline rewrite (`superchat.el:60,62`) and emacsclient (`superchat.el:65-68`) have no row at all and reach the Runtime through autoloads. The `inline` row (`superchat-default-plugins.el:43`) registers only the `inline-reference` context collector, so removing it leaves `superchat-inline-ask` working. The chat window is loaded by eager `require` (`superchat.el:44,46`) with its entry command autoloaded (`superchat.el:3707`); only its Sink, commands, recorders, collectors and notices are rows. `docs/architecture.md:21` already warns that "feature names are not an inventory of independently removable plugins".
- **Only one surface is a real plugin.** `cowork-telega` (`plugins/superchat-cowork-telega/superchat-cowork-telega-legacy.el:1041`) registers its three surfaces, tools, policies, command and collector in its `:apply`, so unmounting it withdraws all of them.
- **The surface name list is hard-coded.** `superchat-pipeline--surfaces` (`superchat-pipeline.el:40`) is seeded with ten built-in names; `superchat-register-surface` exists and records a Scope undo (`superchat-pipeline.el:46-59`), which is how Telega adds its own.
- **Four rows register nothing.** `prompt-assembler` (`superchat-prompt.el:312`) and `tools-baseline` (`superchat-select-tools.el:415`) say in their own docstrings that they are capability declarations; `chat-parse` and `chat-session` (`superchat-dispatcher.el:1533,1537`) are empty seats left by the Stage retirement.
- **There is no composition other than the one flat list.** Preset `:plugins` (`superchat-preset.el:118`) can only add rows to a Chat View; tests hand-compose (`scripts/kernel-check.el:15`, `scripts/smoke.el:491`); nothing expresses "the default minus the user interface".
- **A missing provider is silent.** A row whose `:inject` is unmet stays pending (`superchat-plugin.el:357`); M3a found `driver` injecting `closure` unused, which would have deactivated `driver`, `inline`, `subagent` and `workflow` together.

Read in dsh (the accepted blueprint), verified at the cited places:

- Composition is one unit — a row: `id`, module `name`, `config`, `inject`, `disabled` (`vendor/loader/src/config/entry.ts:9-22`).
- A product is a **profile**: a named, ordered list of **bundles**, each bundle a patch layer (`packages/boot/app-boot/src/profile.ts:110-131`: `web` = base + web-app, `headless` = base + headless, `sdk` = base + sdk-app, `sdk-minimal` = one standalone tree). One selector chooses it (`--profile`).
- A surface layer **disables** base rows rather than deleting them, with the reason written down (`packages/bundle/web-app/cordis.patch.yml:356-359`): "the base is shared, and a row absent from a surface overlay would silently reappear the day someone reorders the composition". The web layer disables 22 base rows this way.
- `headless` is a shipped product surface, not a fixture (`packages/bundle/headless/cordis.patch.yml:1-5`): "one-shot task mode directly over dsh-base. It mounts no Host, HTTP server, Web runtime, or browser plugin", and it keeps the base's tools, skills and subagents.
- A row whose `inject` never resolves fails the boot loudly, naming the missing services (`packages/boot/app-boot/src/index.ts:722-754`); an optional dependency inside a live plugin is silently absent (`ctx.inject`, `ctx.get`).

## Decision

The user approved the roadmap's M4 text on 2026-09-16. Under it:

1. **The five surfaces become plugin rows**: the chat window, inline ask, inline rewrite, Org Babel, Magit and emacsclient. Each row owns its surface name, its entry points and the loading of its code.
2. **Composition is a profile**: a named list of layers over one base row list, and the profile decides what is mounted.
3. **Nothing leaves the default set.** The default profile reproduces today's mounted composition row for row, and the default user experience does not change: `M-x superchat` opens the same chat window, with the same commands and keys.
4. **`headless` is a product capability**: a named profile that answers one prompt with no user-interface row mounted.

## Mechanism (leader calls inside that approval)

These three points were not in the roadmap text. They are the leader's calls under that approval, and the user accepted them on 2026-09-16.

### A profile is a base list plus layers, and a layer may disable a row

`superchat-profiles` maps a profile name to an ordered list of layers; each layer is a list of entries, and an entry is a row (`NAME` or `(NAME . CONFIG)`) or a disable instruction (`(NAME . :disabled)`). `superchat-profile` names the profile that `(require 'superchat)` mounts, and its default value is `default`. The `default` profile is one layer: today's `superchat-plugins` value. `headless` is `default` plus a layer that disables the user-interface rows.

Rationale, which is dsh's: with several profiles over one base, a row that is merely absent from a layer comes back the day the base or the order changes. Disabling states the intent where a reader can see it. Two alternatives were rejected: separate complete row lists per profile (two lists drift), and a profile that is only "the default minus a name list" (it cannot add a row).

### `superchat-plugins` stays as the default profile's row list

The variable keeps its name, its meaning ("the rows the default profile mounts") and its customisation type, so an existing user configuration keeps working. What changes is that the mounting path reads it through the profile rather than directly.

`chat-parse` and `chat-session` are removed: they register nothing and name work that M3 moved into the dispatcher and Runtime, so they are empty seats, not declarations. `prompt-assembler` and `tools-baseline` stay, and the contract states what a legitimate declaration row is: a row that registers nothing may exist only when the capability it names is core and cannot be replaced by a plugin, and its docstring must say so. This is the only case where a row's absence changes nothing.

### The mounted profile is checked, and an unmet dependency is reported

After the profile is mounted, every row it names is either active or reported. A row left pending because a Service it injects is missing is named, with the missing Services, in a single message rather than staying silently inert. This is dsh's `assertEntriesActivated` without its exit: superchat cannot abort Emacs, so it warns and the profile report lists the pending rows.

### An entry point whose row is not mounted refuses

Emacs cannot withdraw an autoloaded command: once `org-babel-execute:superchat`
or `superchat-inline-ask` is on the load path, the symbol stays callable whatever
the profile says. A surface row therefore owns its *registration* — the surface
name, the Services and listeners it contributes, and the loading of its code —
and each entry point asks whether its row is mounted before it does anything.
Invoked without its row, an entry point refuses with a message naming its row
and the active profile, and submits nothing. Measured today, the opposite holds:
unmounting the `inline` row leaves `superchat-inline-ask` fully working, and Org
Babel and Magit have no row to unmount at all.

### What a user-interface row is

A row is a user-interface row when it registers an entry point a human uses: a command, a keymap, a chat View, a Sink that renders to a window, or a surface name whose envelopes originate from a human action. Under that definition `headless` disables the chat window, inline ask, inline rewrite, the chat commands, the chat Sink, the chat recorders and notices, and Telega when present; it keeps Tape, Runtime, Authority, tools, memory, skills, compaction, subagent, workflow, workspace and the model transport, as dsh's headless keeps its base.

## Alternatives considered

**Leave the chat window out of M4.** It is the product's front door and the riskiest row. Rejected because the roadmap's acceptance requires a profile with no user-interface row mounted, which is impossible while the chat window loads unconditionally. The risk is answered by order (the chat row is the last slice) and by the front-door test below, not by dropping the goal.

**Keep surfaces as autoloads and add only the profile mechanism.** Rejected: a profile that cannot mount or withdraw a surface is the same list that does not control the surfaces today.

**Copy dsh's patch documents literally (YAML layers, id-targeted patches).** Rejected: superchat's rows are Lisp data in a defcustom, and an Emacs user edits them in Lisp. The layer-with-disable shape is the part of dsh's model that carries the meaning; its file format does not.

### The names the acceptance needs

The tests are written before the implementation, so the acceptance fixes these
names and shapes; everything else is the implementation's to design.

- `superchat-profiles` — alist of profile name to an ordered list of layers.
  A layer is a list of entries; an entry is `NAME`, `(NAME . CONFIG)` or
  `(NAME . :disabled)`.
- `superchat-profile` — the profile `(require \='superchat)` mounts; default
  `default`.
- `superchat-plugins` — the `default` profile's single layer, unchanged in name
  and meaning.
- `superchat-profile-rows (&optional NAME)` — the resolved, ordered rows of
  NAME after its layers are applied and disabled rows removed.
- `superchat-profile-mount (&optional NAME)` — mount that profile into the
  global Scope; the load-time entry point that replaces the direct
  `superchat-plugin-mount-defaults` call.
- `superchat-profile-report (&optional NAME)` — one entry per row of the
  resolved profile: its name, its state, and for a pending row the Services it
  is still waiting for.

## Acceptance criteria

Test-first, leader-written, before implementation:

1. With the default profile, the mounted composition is today's, row for row, every row active, and the canonical smoke set and the M3 regression set pass unchanged.
2. `M-x superchat` from a cold Emacs opens the chat window and completes a Turn (the front door does not change).
3. Each of the five surfaces is a row: mounting the row registers its surface name; unmounting it withdraws that name, an envelope carrying it is refused, and the surface's own entry point refuses with a message instead of submitting.
4. Removing any single row from the default profile leaves the rest mounted, and a chat Turn still completes when the removed row is not the chat window's.
5. The `headless` profile mounts no user-interface row and answers one prompt end to end, with the answer on the Tape.
6. A profile that names a row whose injected Service is absent reports that row and the missing Service instead of leaving it silently pending.

## Risks

- **The front door.** Making the chat window a row changes what `(require 'superchat)` loads. The chat slice is last, the front-door test is written first, and if the chat window cannot become a row without changing the user's experience, that slice stops and is reported rather than delivered.
- **Autoload cookies.** Org Babel and Magit are reached through package-generated autoloads; a row must own its entry point without breaking a user who already has `org-babel-do-load-languages` configured.
- **Surface names are global.** `superchat-pipeline--surfaces` is not Scope-layered by design; two profiles in one Emacs process are not supported and the contract says so.
- **Silent behaviour change.** Any difference in the default profile is a regression, not a design choice; the default composition is compared row for row, not by feature name.
