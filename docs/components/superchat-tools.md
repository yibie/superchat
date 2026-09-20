---
description: "Register a tool that the model can call, decide which tools the model is offered, materialize them as llm.el descriptors, and execute one through the tool pipeline. The registry and the one execution seam every tool crosses."
kind: "reference"
---

# superchat-tools

## Summary

Use this component to put a capability in front of the model and to run it when the model asks. A plugin registers a tool with a name, a function, a description, and its argument schema; the Session's permission level decides every call, the Tape audits it, and the model receives an llm.el tool descriptor. Choose it for any new model-callable capability rather than calling a function directly from the agent loop. Its main cost is that registration is scope-accounted and unregistration destroys the installed closures, so a tool cannot be re-registered into a layer that already holds its name. Its most important boundary is that candidate enumeration, request selection, materialization and authorization are four separate answers: `descriptors` says what could be chosen, `materialize` says what the model is offered, and only the permission level (with the approval it may ask for) says what may run. The descriptor it returns carries the plugin's own function, so the guarantee that the tool pipeline runs first comes from the execution path, not from wrapping the object.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

Register a tool, then call it: every model-invoked call enters through the `tools` Service's `execute` method, which is the tool pipeline's execution interface (`superchat-tool-execute`).

### When to choose it

Choose `superchat-register-tool` when the capability is something the model decides to use. Choose a `tools/pre-execute` listener when a tool that already exists should be treated more strictly. Choose a Command when a human invokes it rather than the model.

### Minimal configuration

Two plugins, mounted with one injection each:

```elisp
(superchat-plugin-mount 'tools)          ; the registry, materialization and execute
(superchat-plugin-mount 'tools-builtin)  ; installs the built-in tools as selectable descriptors
```

`tools` declares `:inject '(tape)` and `tools-builtin` declares `:inject '(tools)` -- dependencies supplied by the default providers of those names. Neither takes configuration keys, and `superchat-define-plugin` supplies lazy definitions for both, so they are normally mounted by the default set rather than by hand. Composition and lifetime follow the [composition contract](../design/plugin-architecture.md).

| Plugin | Provides | Without it |
|---|---|---|
| `tools` | The `tools` Service | No registration, materialization or execution entry point |
| `tools-builtin` | The built-in tools' descriptor installations | No built-in tool is a selectable descriptor, because a built-in name is selectable only while this plugin's installation of it is visible |

### The `tools` Service

| Method | Arguments | Returns |
|---|---|---|
| `register` | The `superchat-register-tool` argument list | The tool name |
| `descriptors` | — | Names of the mounted built-in tools and of live plugin tool installations, de-duplicated |
| `materialize` | `&optional TOOL-NAMES INPUT-OPERATION` | llm.el tool objects |
| `execute` | The `superchat-tool-execute` argument list | The tool's result, or the refusal |

### Registering a tool

`superchat-register-tool NAME &key function description args async surfaces scope` returns `NAME`. Every argument is validated before anything is written:

| Argument | Required | Meaning |
|---|---|---|
| `NAME` | yes | Non-empty string, distinct from every built-in tool name and from `ask_user` |
| `:function` | yes | Callable. With `:async` non-nil it receives a callback as its first argument |
| `:description` | yes | Non-empty string; the text the model reads when choosing |
| `:args` | no | Argument schema in `llm-make-tool` form, a list of `(:name .. :type .. :description .. [:optional t])` plists |
| `:surfaces` | no | The Surfaces whose default baseline may offer `NAME` |
| `:scope` | no | The installation layer; otherwise plugin effect ownership wins over the current execution Scope, then global |

A registration that fails validation writes nothing: the checks run before the Scope write, and a duplicate `NAME` in the same layer is rejected by the Scope before its layer or cleanup changes. A registered tool declares nothing about what it does: under the Session's permission level it is an *outside* tool (asked about per call under `edit-buffers` unless it is an MCP tool of a trusted server, refused under `read-only`, free under `danger-full-access`).

`:surfaces` narrows where the default baseline offers the tool. A registered tool is otherwise offered by default on the chat-like surfaces regardless of `superchat-llm-tool-names`, which narrows only the built-in set; an explicit request-local profile includes a name only when it names it.

### Withdrawing a tool

`superchat-unregister-tool NAME &key scope` returns non-nil when it withdrew an installation and nil when the layer held no such name. It removes only that layer's own entry, so an inherited definition becomes visible again where one exists. Visibility is the composition contract's: contributions are visible to a Scope and its descendants, never to siblings, so withdrawal re-exposes an ancestor and never exposes a sibling ([plugin-architecture.md](../design/plugin-architecture.md)).

**A materialized descriptor binds one exact installation.** Its function is the one selected when that descriptor was materialized, resolved from that installation rather than from the name and the current Scope, so a same-name replacement can never run under a withdrawn descriptor. Withdrawal unbinds the installation's closures, so a descriptor already handed to a model is no longer executable and fails closed; a later selection resolves the name again through the current Scope chain and reaches whatever that chain now offers.

This identity is not an authorization: it fixes *which* implementation a descriptor refers to, and the permission level still decides whether that call may run.

### Listing descriptors

`tools descriptors` answers the first of four separate questions: **what declarations exist that a request could select from.** It returns the names of the mounted built-in tools and of the live plugin tool installations, de-duplicated. A name here is *selectable*, which is not the same as *permitted*.

The four questions are separate, and none of them answers the next one:

| Stage | Question it answers | What it does not prove |
|---|---|---|
| Candidate enumeration | Which declarations are available to choose from? | That the candidate has a callable implementation |
| Request selection | Which tools does this Surface and this explicit profile select? | That a selected tool may run |
| Materialization | What descriptor objects do the selected tools become? | That the request is authorized |
| Permission level + approval | May this specific call execute? | Nothing further; it is the last word |

**Materialization does not prove permission. An explicit profile is never widened** -- by the default surface or by a plugin registration.

### Materializing the model's tool surface

`tools materialize &optional TOOL-NAMES INPUT-OPERATION` returns llm.el tool objects. It implements a selection; it does not make one.

With `TOOL-NAMES` nil it returns the default surface for the current Surface: the built-in names selected by `superchat-llm-tool-names` -- eight by default, `list_buffers`, `apropos`, `describe-function`, `read_buffer`, `read_file`, `open_file`, `edit_buffer` and `eval_elisp`; none of them persists anything, `write_file` no longer exists, and `save_buffer` is offered only when a Preset or Request names it -- **plus the plugin tools that are visible by default on that Surface**. These names are the built-in default, not the whole default surface.

With `TOOL-NAMES` a list, it builds exactly that request-local profile and returns exactly those tools, without changing the variable or the global cache. It signals when a requested name is not resolvable rather than silently returning fewer tools, and neither the default list nor any plugin registration widens it.

`INPUT-OPERATION` adds the `ask_user` descriptor built for that operation. `ask_user` is never part of the default surface, and a prepared-tools request that advertises it is an error at the transport layer -- prepared tools are a trusted internal path, not a way around admission.

### Executing a tool

`tools execute` is `superchat-tool-execute`, whose contract is [superchat-authority](superchat-authority.md)'s. From this component's side two things matter:

- The body runs only after the pipeline's admission, decision and (when asked) approval steps, so a registered function is never reached by a refused call.
- Execution carries the Tape audit: a call needs the execution identity the audit checks, which Runtime's accepted execution context supplies. A caller outside an accepted Run gets a refusal instead of running a body.

The pipeline's event types are defined once, in [superchat-authority](superchat-authority.md); this component appends none of them itself.

### Built-in buffer names

`list_buffers` prints each buffer name as an Emacs Lisp string, followed by its size and major mode. Quoting makes leading spaces explicit and keeps embedded newlines escaped on one output line. Consumers pass the decoded name to `read_buffer`, including every significant space. The readable output format changes from a bare name; it is not a promise of byte compatibility for external parsers. Tool visibility, buffer scope and permission checks are unchanged.

### Built-in edit and evaluation results

`edit_buffer` replaces exactly one case-sensitive, literal occurrence in the accessible text of a live buffer, counting overlapping matches as separate occurrences. Empty old_string, zero matches and multiple matches return Error: without editing. Identical old_string and new_string return No change: without changing the buffer's modification tick. A replacement changes buffer text and reports one occurrence; the tool does not save or evaluate it. In a Lisp buffer (a mode derived from `lisp-data-mode`) the three structured edit tools -- `edit_buffer`, `append_to_buffer`, `replace_buffer` -- first rehearse the change on a private copy: when the buffer reads as Lisp before (the whole text, whatever is narrowed, scans the way `check-parens` scans it) and would not after, the call returns `Error: edit not applied; buffer … was not changed …` naming the kind of mismatch, its line and column and the line's text, and the real buffer is never touched -- no text change, modified mark, undo entry, change hook or lock file, and no mutation target on the call's `tool.completed` fact, so `/changes` does not list it. A buffer that did not read before is edited as usual and the result adds one `Note:` line saying whether it reads now. Nothing is evaluated, no mode hook runs, the tool repairs nothing, and `eval_elisp` is outside this check ([decision](../decisions/implemented/2026-09-19-a-broken-lisp-edit-is-sent-back.md)). Read the intended buffer before choosing the exact old text, and inspect the requested state after the change. Buffer selection remains name-based; this interface is not a versioned patch or a guarantee against later user edits.

`eval_elisp` accepts one complete expression and evaluates it in the calling Emacs context. Use progn or let for several operations, and with-current-buffer for an explicitly selected live buffer. Trailing whitespace and line comments are allowed; additional forms or malformed trailing input fail before eval is called. Result: reports only the expression's value. Error: enters the shared tool-error feedback path. The native evaluation-error result is distinct from a pipeline refusal even when its condition text contains words such as cancelled. Error objects print at depth 4 and sequence length 12; details contain at most 1024 characters, including an explicit marker when character clipping is needed, plus the fixed native error prefix. The corrective message lists failed tool names and refers to their tool results without copying the error bodies. For retryable failures it asks to repair the failed call, then continue the original user request after success; genuine user or policy refusals retain the no-retry instruction. Successful values retain their existing representation; see [bounded tool-error context](../decisions/implemented/2026-09-17-bounded-tool-error-context.md). Reading can construct objects and invoke custom hash callbacks after authorization; neither parse nor evaluation errors promise rollback. Output sent to standard-output or message is not captured in this result. Verify actual values and effective bindings. The tool's description tells the model that it is not for saving files: a change that must last is made in the buffer of the configuration file Emacs actually loads, and saving that buffer is the user's decision ([Buffer-first mutation](../decisions/implemented/2026-09-18-buffer-first-mutation.md)). Until 2026-09-19 the description said the opposite -- "for persistence, save the actual loaded configuration and verify in a fresh Emacs" -- an M5-era sentence M7's second slice failed to remove; three attended runs saved through `eval_elisp` while it stood.

The model receives these constraints in the selected tools' descriptions and argument schemas. Nothing reads or classifies the expression before the call is decided: every `eval_elisp` call is full Emacs control under the permission level, so no reader object is constructed and no custom hash test runs for a declined call. Normal tool selection and authorization still apply. The rationale is in [the tool-feedback decision](../decisions/implemented/2026-09-17-emacs-tool-feedback.md).

### Absent dependencies

The `tools` Service needs the `tape` plugin. Without `tape` a call cannot record its audit facts and refuses. Without `llm.el` loaded, `materialize` returns nil and logs a warning, so registration succeeds with no model-visible surface.

## Understand the implementation

<details><summary>Implementation internals</summary>

Two registries meet in this component. The built-in tools are a fixed list materialized by `superchat-llm-tools-reload`, cached in `superchat-llm-tools-list` and invalidated when the selected-names value, the subagent registry, or a withdrawal changes. Plugin tools live in the Scope's `tool` layer as installation plists, and `superchat-register-tool` installs one together with its effect resolver and approval preview.

`superchat-get-llm-tools` is the materializer. With an explicit profile it binds a temporary selected-names value and an empty cache, takes the requested names in order, and signals on any name it could not resolve. Without one it appends the external (declaration-gated) tools to the cached built-ins, removes `ask_user` unless `INPUT-OPERATION` asks for it, and returns the result.

`tools descriptors` answers from Scope installations -- the `builtin-tool` layer the `tools-builtin` plugin writes and the `tool` layer `superchat-register-tool` writes -- so a name is selectable exactly while something installed it.

</details>

## Further exploration

- [superchat-authority](superchat-authority.md) — the pipeline every execution crosses; prerequisite.
- [architecture.md](../architecture.md) — where the registry and the execution seam sit among the other owners.
- [plugin-architecture.md](../design/plugin-architecture.md) — Scopes, installation layers and scope-accounted cleanup.
- [plugin-guide.md](../plugin-guide.md) — registering a tool from a plugin.

## Model experience

Directly: this component is what the model chooses from, and its descriptions are prompt material.

#### What the model sees

One tool entry per materialized descriptor, carrying the name, the description and the argument schema exactly as registered. A plugin's `:description` is therefore model-visible text, and `:surfaces` decides whether the entry appears in the default surface or only under an explicit profile.

#### Token effect

The tool schemas are sent with every request, so each registered tool that the baseline offers costs its description and schema on every turn. The default built-in set has six tools, with default-visible plugin tools added according to their Surface rules. Descriptions cost request space whenever their tools are selected, which is why concise schemas and `:surfaces` matter.

#### KV cache effect

The tool schemas are part of the request's leading, stable region. Registration or withdrawal changes the schema block and invalidates the cache from it onward, so a tool registered mid-conversation costs a prefix rebuild. Materializing the same surface twice produces the same schemas and leaves the prefix intact.

## Known limitations

- `materialize` returns the plugin's own function in the descriptor, so a consumer that reads the descriptor and calls the function itself bypasses the permission level and the Tape audit. Only the agent loop's path is gated; the object is not.
- A tool's selectability and its function are one installation, so withdrawing a tool withdraws both. There is no way to withdraw a tool's selectability while leaving it executable for another caller.
- `superchat-llm-tool-names` narrows only the built-in names. A plugin tool that is visible by default on the Surface stays on it, so a user who wants one removed needs `:surfaces` or an explicit request-local profile.
- Materialization caches the built-in surface in a global, so two callers interleaving profile and non-profile materialization share one cache; the profile mode avoids it by binding an empty cache for its own duration.
- Descriptor identity is not a permission. A descriptor that still binds a live installation tells you which implementation a call would reach, never that the call is allowed; only the permission level's decision does that.

### Dev note

<details><summary>Working context for maintainers</summary>

Since M6 candidate enumeration and the function are one installation, so the old resolver-without-a-tool disagreement cannot occur; the four-stage table above still matters because selectable, offered and permitted remain three different answers.

</details>
