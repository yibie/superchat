# Advanced use

## Inspecting Runs from outside Emacs (`emacsclient`)

When an Emacs server is already running, the `superchat-emacsclient` adapter (the `emacsclient` row, mounted by default) answers JSON commands for listing active Runs, reading one Run or one conversation's history, and cancelling a Run:

```sh
emacsclient --eval '(superchat-emacsclient-list-runs)'
emacsclient --eval '(superchat-emacsclient-run-status "RUN-ID")'
emacsclient --eval '(superchat-emacsclient-session-history "SESSION-ID" 50)'
emacsclient --eval '(superchat-emacsclient-run-cancel "RUN-ID")'
```

The answer to `run-cancel` carries the status the Runtime holds for the Run once the cancel was asked for (`cancelled`, or another ending such as `audit-incomplete`) and `terminal`, whether that status is final; it is not a fixed word.

The adapter never starts the server. `emacsclient --eval` already evaluates arbitrary Emacs Lisp: any local process that can connect to the server can do the same, so these commands are a convenience surface, not a sandbox, authentication layer or security boundary.

## Shared workspace

`M-x superchat-artifact-board-set-region` makes the active region an Artifact in the current conversation's Workspace. A Workspace can hold several region Artifacts; coding agents and delegated executors can inspect them with `workspace_info`, then target `workspace_read` or `workspace_write` by Artifact ID. `workspace_publish` stores a new immutable generated Artifact under the Superchat data directory, so its content survives Emacs restarts.

Cross-Run reads and tool-driven writes include the reported revision. A read consumes another Actor's Artifact only when its producer has a unique recorded successful Run terminal and the Artifact passes its provenance, producer, revision and Tape-order checks; stale writes are rejected. Artifact metadata and lifecycle events are persisted; region content stays in its original buffer, while published generated content is durable.

## The turn pipeline

Every request (chat, inline ask, rewrite, sub-agent, workflow step, compaction, a plugin surface) is a Request envelope submitted through `superchat-pipeline-submit` and driven through the same main line:

```
submit → parse (in the surface) → admission → select tools → collect context
  → assemble prompt → (model-transport Service)
```

Session identity, state assembly, tool selection, prompt assembly and admission are core calls; the pieces a plugin extends are Events. A plugin registers a listener on the `collect-context` Event from its `apply` (a collector that returns labelled prompt fragments with a budget class) or on a lifecycle Event. The model's text, its reasoning and the final delivery go to the envelope's Sink (a small set of callbacks), so an inline overlay, the chat View and a background job differ in their Sink. Not everything shown goes through it: the stock llm.el transport also writes the time-to-first-token note and closes response regions directly in the request's View when that View is alive, so a Sink of your own does not by itself silence the View.

Prompt text is contributed by a `collect-context` listener registered from a plugin's `apply`:

```elisp
(superchat-define-plugin my-git-blame
  :apply
  (lambda (_scope _config)
    (superchat-context-register
     'my:git-blame
     (lambda (env _state _tools)
       (when-let* ((file (and (eq (plist-get env :surface) 'chat)
                              (plist-get (plist-get env :context) :chat-file-ref))))
         (list (superchat-prompt-fragment
                :owner 'my-git-blame :id "my:git-blame" :slot 'context
                :text (shell-command-to-string (format "git blame -L 1,20 -- %s" file))
                :priority 0 :budget-class 'soft))))
     :owner 'my-git-blame)))
```

The hooks `superchat-build-prompt-functions` and `superchat-system-prompt-functions` do not exist; use a `collect-context` listener. See the [architecture map](../architecture.md) for the current request path and the [request lifecycle contract](../protocols/request-lifecycle.md) for its guarantees.

## Writing plugins

Superchat mounts its default composition from `superchat-plugins`. A third-party plugin is an ordinary Emacs package built around `superchat-define-plugin`: it declares Service dependencies and configuration, then registers contributions (a prompt collector, a tool, a command, an Event listener) through its Scope. Unmounting withdraws those registrations; cancellation of active work follows the Runtime lifecycle contract, not arbitrary plugin cleanup. Start with the [plugin guide](../plugin-guide.md) and the [composition contract](../design/plugin-architecture.md); `plugins/superchat-cowork-telega` and `plugins/superchat-elfeed` are product examples.
