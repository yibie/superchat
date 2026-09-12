---
title: Telega reference plugin design
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: [007-default-plugin-inventory]
---
## Question
How is the Telega plugin written under the four-piece shape: which Services it injects, which Scope it mounts in (its own Preset?), how its three tools, surface, two policies, command and context collector are registered, and what replaces `:active-when`?

## Resolution (2026-09-11, sc-leader)

**Plain language.** Telega becomes one plugin file with a four-piece
definition. It does not touch `require` of any superchat file except the
Kernel; everything it needs comes through `:inject`, and everything it adds
goes through the Scope it is given. The activation predicate disappears:
Telega is "active" for a Request exactly when that Request's Surface is one
of the three it registered, which the driver already knows.

```elisp
(superchat-define-plugin cowork-telega
  :inject (driver tools authority commands session tape)
  :config ((:digest-chats listp nil "Chat ids granted read-only to digests")
           (:autonomy (lambda (v) (memq v '(draft approve))) 'draft))
  :apply
  (lambda (scope config)
    ;; 1. surfaces (Envelope vocabulary; replaces `superchat-register-surface')
    (superchat-surface-declare scope 'telega) (… 'telega-digest) (… 'telega-triage)
    ;; 2. three tools — same descriptors as today (…-legacy.el:165,237,422)
    (superchat-service-call scope 'tools 'register "telega_reply" …)
    (superchat-service-call scope 'tools 'register "telega_read_history" …)
    (superchat-service-call scope 'tools 'register "telega_propose_rules" …)
    ;; 3. two policies (replace `:policies` of define-extension)
    (superchat-service-call scope 'authority 'register-policy 'telega-digest #'…)
    (superchat-service-call scope 'authority 'register-policy 'telega-triage #'…)
    ;; 4. one command
    (superchat-service-call scope 'commands 'register "telega-rules" #'…)
    ;; 5. its context collector (replaces `:context` + `:knowledge`): runs only
    ;;    for its own surfaces, which is what `:active-when' used to express
    (superchat-stage-add scope 'collect-context 'telega-schedule
      (lambda (env state tools)
        (when (memq (plist-get env :surface) '(telega telega-digest telega-triage))
          (append (superchat-cowork-telega--schedule-fragments env)
                  (superchat-cowork-telega--knowledge-fragments env)))))
    ;; 6. tool baseline for its surfaces (replaces `:tools` of define-extension)
    (superchat-stage-add scope 'select-tools 'telega-surface-tools
      (lambda (env _state)
        (pcase (plist-get env :surface)
          ('telega '("telega_reply" "memory_search"))
          ('telega-digest '("telega_read_history" "memory_search"))
          ('telega-triage '("telega_propose_rules")))))))
```

**Where it mounts.** In the *global* Scope, from the user's `superchat-plugins`
(or `(superchat-plugin-mount 'cowork-telega)` at runtime). Not a Preset: its
three Surfaces are entered from Telega buffers, not from a chat Preset switch,
and its tools must be selectable whenever such a Request arrives. The
per-autonomy Agent Profiles it builds today (`…-legacy.el:590-610`) stay as
plain data passed in the Envelope's `:context (:preset …)`; they no longer
need `superchat-preset-from-plist` to be a Preset *Scope*.

**Submission.** The three flows (`superchat-cowork-telega`, digest, triage)
call `(superchat-service-call scope 'driver 'submit ENVELOPE)` with an
Envelope they build directly (content, surface, media, Sink with the Telega
callbacks, `:context (:telega-chat-id … :telega-autonomy …)`), which is what
`…-session.el:424-448` already does; `…-legacy.el`'s `superchat-pipeline-envelope-from-request`
translation and `superchat-cowork-telega--compose-request` are deleted.

**What replaces `:active-when`:** nothing — its two jobs are covered
structurally. (a) "Only contribute tools/context for my own Requests" =
the surface check inside the collector and `select-tools` contributor above
(the driver passes `:surface`). (b) "Only show my description to the model
while active" = the knowledge fragment is only emitted for those surfaces.
The old `superchat-extension-claimed-tool-p` cross-plugin guard is unnecessary
once tool names are Scope-owned (duplicate name = mount error).

**Unmount test (replaces the ecosystem tests):** `(superchat-plugin-unmount
'cowork-telega)` → `superchat-invariant-report`/registry dumps show no
`telega*` tool, policy, command, surface or collector; a Telega Envelope then
fails validation with `unknown surface telega` and no Run is created.
