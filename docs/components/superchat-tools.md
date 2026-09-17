---
description: "Register a tool that the model can call, decide which tools the model is offered, materialize them as llm.el descriptors, and execute one through Authority. The registry and the one execution seam every tool crosses."
kind: "reference"
---

# superchat-tools

## Summary

Use this component to put a capability in front of the model and to run it when the model asks. A plugin registers a tool with a name, a function, a description, its argument schema and the effects it has; Authority gates every call, the Tape audits it, and the model receives an llm.el tool descriptor. Choose it for any new model-callable capability rather than calling a function directly from the agent loop. Its main cost is that registration is scope-accounted and unregistration destroys the installed closures, so a tool cannot be re-registered into a layer that already holds its name. Its most important boundary is that candidate enumeration, request selection, materialization and authorization are four separate answers: `descriptors` says what could be chosen, `materialize` says what the model is offered, and only Authority says what may run. The descriptor it returns carries the plugin's own function, so the guarantee that Authority runs first comes from the execution path, not from wrapping the object.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

Register a tool, then call it: every model-invoked call enters through the `tools` Service's `execute` method, which is Authority's execution interface.

### When to choose it

Choose `superchat-register-tool` when the capability is something the model decides to use. Choose an Authority effect resolver alone when the name already exists and only its effect classification changes. Choose a Command when a human invokes it rather than the model.

### Minimal configuration

Two plugins, mounted with one injection each:

```elisp
(superchat-plugin-mount 'tools)          ; the registry, materialization and execute
(superchat-plugin-mount 'tools-builtin)  ; the built-in tools' Authority declarations
```

`tools` declares `:inject '(authority tape)` and `tools-builtin` declares `:inject '(authority)` -- dependencies on the Authority and Tape Services, supplied here by the default providers of those names. Neither takes configuration keys, and `superchat-define-plugin` supplies lazy definitions for both, so they are normally mounted by the default set rather than by hand. Composition and lifetime follow the [composition contract](../design/plugin-architecture.md).

| Plugin | Provides | Without it |
|---|---|---|
| `tools` | The `tools` Service | No registration, materialization or execution entry point |
| `tools-builtin` | The built-in tools' effect resolvers and approval previews | No built-in tool is a selectable descriptor, because a name is selectable only when Authority has a visible resolver for it |

### The `tools` Service

| Method | Arguments | Returns |
|---|---|---|
| `register` | The `superchat-register-tool` argument list | The tool name |
| `descriptors` | — | Names with a visible Authority effect resolver, de-duplicated |
| `materialize` | `&optional TOOL-NAMES INPUT-OPERATION` | llm.el tool objects |
| `execute` | The `superchat-tool-execute` argument list | The tool's result, or the refusal |

### Registering a tool

`superchat-register-tool NAME &key function description args async effects approval-preview surfaces scope` returns `NAME`. Every argument is validated before anything is written:

| Argument | Required | Meaning |
|---|---|---|
| `NAME` | yes | Non-empty string, distinct from every built-in tool name and from `ask_user` |
| `:function` | yes | Callable. With `:async` non-nil it receives a callback as its first argument |
| `:description` | yes | Non-empty string; the text the model reads when choosing |
| `:args` | no | Argument schema in `llm-make-tool` form, a list of `(:name .. :type .. :description .. [:optional t])` plists |
| `:effects` | yes | A resolver function of the normalized argument list returning effect descriptors, or a static list of them |
| `:approval-preview` | no | Registered with `superchat-authority-register-approval-preview` |
| `:surfaces` | no | The Surfaces whose default baseline may offer `NAME` |
| `:scope` | no | The installation layer; otherwise plugin effect ownership wins over the current execution Scope, then global |

A registration that fails validation writes nothing: the checks run before the Scope write, and a duplicate `NAME` in the same layer is rejected by the Scope before its layer or cleanup changes. Function, effect resolver and approval preview stay together in one Scope installation.

`:surfaces` narrows where the default baseline offers the tool. A registered tool is otherwise offered by default on the chat-like surfaces regardless of `superchat-llm-tool-names`, which narrows only the built-in set; an explicit request-local profile includes a name only when it names it.

### Withdrawing a tool

`superchat-unregister-tool NAME &key scope` returns non-nil when it withdrew an installation and nil when the layer held no such name. It removes only that layer's own entry, so an inherited definition becomes visible again where one exists. Visibility is the composition contract's: contributions are visible to a Scope and its descendants, never to siblings, so withdrawal re-exposes an ancestor and never exposes a sibling ([plugin-architecture.md](../design/plugin-architecture.md)).

**A materialized descriptor binds one exact installation.** Its function, effect resolver and approval preview are the ones selected when that descriptor was materialized, resolved from that installation rather than from the name and the current Scope, so a same-name replacement can never be paired with the withdrawn one's resolver or preview. Withdrawal unbinds the installation's closures, so a descriptor already handed to a model is no longer executable and fails closed; a later selection resolves the name again through the current Scope chain and reaches whatever that chain now offers.

This identity is not an authorization: it fixes *which* implementation a descriptor refers to, and Authority still decides whether that call may run.

### Listing descriptors

`tools descriptors` answers the first of four separate questions: **what declarations exist that a request could select from.** It returns the names with a visible Authority effect resolver, including paired plugin tools and de-duplicated. A name here is *selectable*, which is not the same as *callable*: an effect resolver registered without a matching tool puts a name in this list with no function behind it, and Authority refuses such a call.

The four questions are separate, and none of them answers the next one:

| Stage | Question it answers | What it does not prove |
|---|---|---|
| Candidate enumeration | Which declarations are available to choose from? | That the candidate has a callable implementation |
| Request selection | Which tools does this Surface and this explicit profile select? | That a selected tool may run |
| Materialization | What descriptor objects do the selected tools become? | That the request is authorized |
| Authority | May this specific call execute? | Nothing further; it is the last word |

**A resolver's existence does not prove a function is available. Materialization does not prove permission. An explicit profile is never widened** -- by the default surface or by a plugin registration.

### Materializing the model's tool surface

`tools materialize &optional TOOL-NAMES INPUT-OPERATION` returns llm.el tool objects. It implements a selection; it does not make one.

With `TOOL-NAMES` nil it returns the default surface for the current Surface: the built-in names selected by `superchat-llm-tool-names` -- six by default, `list_buffers`, `apropos`, `describe-function`, `read_buffer`, `edit_buffer` and `eval_elisp` -- **plus the plugin tools that are visible by default on that Surface**. These names are the built-in default, not the whole default surface.

With `TOOL-NAMES` a list, it builds exactly that request-local profile and returns exactly those tools, without changing the variable or the global cache. It signals when a requested name is not resolvable rather than silently returning fewer tools, and neither the default list nor any plugin registration widens it.

`INPUT-OPERATION` adds the `ask_user` descriptor built for that operation. `ask_user` is never part of the default surface, and a prepared-tools request that advertises it is an error at the transport layer -- prepared tools are a trusted internal path, not a way around admission.

### Executing a tool

`tools execute` is `superchat-tool-execute`, whose contract is [superchat-authority](superchat-authority.md)'s. From this component's side two things matter:

- The body runs only after Authority's pre-policy, approval and monotonic-guard steps, so a registered function is never reached by a refused call.
- Execution carries the Tape audit: a call needs the execution identity the audit checks, which Runtime's accepted execution context supplies. A caller outside an accepted Run gets a refusal instead of running a body.

The pipeline's event types are defined once, in [superchat-authority](superchat-authority.md); this component appends none of them itself.

### Built-in buffer names

`list_buffers` prints each buffer name as an Emacs Lisp string, followed by its size and major mode. Quoting makes leading spaces explicit and keeps embedded newlines escaped on one output line. Consumers pass the decoded name to `read_buffer`, including every significant space. The readable output format changes from a bare name; it is not a promise of byte compatibility for external parsers. Tool visibility, buffer scope and Authority checks are unchanged.

### Built-in edit and evaluation results

`edit_buffer` replaces exactly one case-sensitive, literal occurrence in the accessible text of a live buffer, counting overlapping matches as separate occurrences. Empty old_string, zero matches and multiple matches return Error: without editing. Identical old_string and new_string return No change: without changing the buffer's modification tick. A replacement changes buffer text and reports one occurrence; the tool does not save or evaluate it. Read the intended buffer before choosing the exact old text, and inspect the requested state after the change. Buffer selection remains name-based; this interface is not a versioned patch or a guarantee against later user edits.

`eval_elisp` accepts one complete expression and evaluates it in the calling Emacs context. Use progn or let for several operations, and with-current-buffer for an explicitly selected live buffer. Trailing whitespace and line comments are allowed; additional forms or malformed trailing input fail before eval is called. Result: reports only the expression's value. Error: enters the shared tool-error feedback path. The native evaluation-error result is distinct from an Authority refusal even when its condition text contains words such as cancelled. Error objects print at depth 4 and sequence length 12; details contain at most 1024 characters, including an explicit marker when character clipping is needed, plus the fixed native error prefix. The corrective message lists failed tool names and refers to their tool results without copying the error bodies. For retryable failures it asks to repair the failed call, then continue the original user request after success; genuine user or policy refusals retain the no-retry instruction. Successful values retain their existing representation; see [bounded tool-error context](../decisions/implemented/2026-09-17-bounded-tool-error-context.md). Reading can construct objects and invoke custom hash callbacks after authorization; neither parse nor evaluation errors promise rollback. Output sent to standard-output or message is not captured in this result. Verify actual values and effective bindings; persistent changes also need the correct configuration file and an independent loading check.

The model receives these constraints in the selected tools' descriptions and argument schemas. Before reading input for effect classification, the conservative classifier rejects any raw #[sS] occurrence and keeps process-exec, including matching text inside strings or comments. It therefore does not construct reader objects or invoke custom hash tests during Authority's classification. Normal tool selection and authorization still apply. The rationale is in [the tool-feedback decision](../decisions/implemented/2026-09-17-emacs-tool-feedback.md).

### Absent dependencies

The `tools` Service needs the `authority` and `tape` plugins. Without `authority` there is no effect resolver to make a name selectable and no execution interface; without `tape` a call cannot record its audit facts and refuses. Without `llm.el` loaded, `materialize` returns nil and logs a warning, so registration succeeds with no model-visible surface.

## Understand the implementation

<details><summary>Implementation internals</summary>

Two registries meet in this component. The built-in tools are a fixed list materialized by `superchat-llm-tools-reload`, cached in `superchat-llm-tools-list` and invalidated when the selected-names value, the subagent registry, or a withdrawal changes. Plugin tools live in the Scope's `tool` layer as installation plists, and `superchat-register-tool` installs one together with its effect resolver and approval preview.

`superchat-get-llm-tools` is the materializer. With an explicit profile it binds a temporary selected-names value and an empty cache, takes the requested names in order, and signals on any name it could not resolve. Without one it appends the external (declaration-gated) tools to the cached built-ins, removes `ask_user` unless `INPUT-OPERATION` asks for it, and returns the result.

`tools descriptors` answers from Authority rather than from the tool registry, so the selectable set and the executable set cannot drift: a name with no visible resolver is not offered.

</details>

## Further exploration

- [superchat-authority](superchat-authority.md) — the decision every execution crosses; prerequisite.
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

- `materialize` returns the plugin's own function in the descriptor, so a consumer that reads the descriptor and calls the function itself bypasses Authority and the Tape audit. Only the agent loop's path is gated; the object is not.
- Candidate enumeration is derived from effect resolvers, and the resolver and the tool are separate writes. A name can therefore be a selectable candidate with no callable implementation; Authority refuses the call, which is a boundary rather than a substitute for a coherent registration.
- A tool's effect resolver and its function are one installation, so withdrawing a tool withdraws both. There is no way to withdraw a tool's selectability while leaving it executable for another caller.
- `superchat-llm-tool-names` narrows only the built-in names. A plugin tool that is visible by default on the Surface stays on it, so a user who wants one removed needs `:surfaces` or an explicit request-local profile.
- Materialization caches the built-in surface in a global, so two callers interleaving profile and non-profile materialization share one cache; the profile mode avoids it by binding an empty cache for its own duration.
- Descriptor identity is not a permission. A descriptor that still binds a live installation tells you which implementation a call would reach, never that the call is allowed; only Authority's decision does that.

### Dev note

<details><summary>Working context for maintainers</summary>

Candidate enumeration is derived from effect resolvers while the function lives in a separate write, so the two can disagree. Whether a resolver should be registerable without a matching tool is unresolved; until it is, the four-stage table above is what keeps the disagreement from being read as a permission.

</details>
