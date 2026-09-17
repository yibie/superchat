;;; superchat-pipeline.el --- Envelope validation and the fixed driver -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; `superchat-pipeline-submit' (contract §4, `docs/design/turn-pipeline-contract.md')
;; validates an envelope, prepares input with core defaults, and hands the
;; prepared prompt to the existing `superchat-runtime-submit' as a legacy
;; request carrying the envelope's exact Sink, passed through as the
;; Request's `:sink' key (S01-fix-3 item 5: Runtime uses an explicit `:sink'
;; exactly as given for every slot -- stream, status, final, error,
;; orphaned -- and honours its own `:live-p').  Every built-in producer --
;; chat, inline ask/rewrite, subagent, skills, workflow, compaction -- and
;; the Telega reference plugin submit through it.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-sink)
(require 'superchat-event)
(require 'superchat-invariant)
(declare-function superchat-tape-append-event "superchat-tape" (event))

(declare-function superchat-runtime-submit "superchat-runtime" (request))
(declare-function superchat-select-tools-decision "superchat-select-tools" (request state))
(declare-function superchat-editor-workspace-validate-buffer
                  "superchat-editor-workspace" (buffer session-id))
(declare-function superchat--llm-context-budget-chars
                  "superchat-llm"
                  (&optional system-prompt turn-prompt target-model))

(define-error 'superchat-envelope-invalid
  "Superchat pipeline: envelope does not satisfy the Request envelope contract")

;;; Surfaces (contract §2.1)

(defvar superchat-pipeline--surfaces
  '(workflow subagent compaction skills external)
  "Registered Producer surface symbols.

The built-in surface rows `chat', `inline-ask', `inline-rewrite', `org-babel'
and `magit' register themselves through `superchat-register-surface', so they
are absent from this core seed and are withdrawn by the row's Scope.  The
remaining names are core-held for surfaces without a row in this slice; a
plugin (Telega, for example) adds its own with `superchat-register-surface'.")

(defun superchat-register-surface (symbol)
  "Register SYMBOL as a Producer surface.  Signal an error on a duplicate
or when SYMBOL is not a non-nil symbol (contract §2.1: a Surface is a
symbol; a registered string would otherwise pass the `memq' validation in
`superchat-pipeline-validate-envelope')."
  (unless (and (symbolp symbol) symbol)
    (error "superchat-register-surface requires a non-nil symbol, got %S" symbol))
  (when (memq symbol superchat-pipeline--surfaces)
    (error "Surface %S is already registered" symbol))
  (setq superchat-pipeline--surfaces
        (append superchat-pipeline--surfaces (list symbol)))
  (when (fboundp 'superchat-plugin-record-undo)
    (superchat-plugin-record-undo (lambda () (superchat-unregister-surface symbol))))
  symbol)

(defun superchat-unregister-surface (symbol)
  "Remove SYMBOL from the registered Producer surfaces; return non-nil if it was one."
  (when (memq symbol superchat-pipeline--surfaces)
    (setq superchat-pipeline--surfaces (delq symbol superchat-pipeline--surfaces))
    t))

;;; Small plist helpers

(defun superchat-pipeline--plist-p (value)
  "Return non-nil when VALUE is a proper plist with keyword keys."
  (and (listp value)
       (zerop (mod (length value) 2))
       (cl-loop for (key _value) on value by #'cddr always (keywordp key))))

(defun superchat-pipeline--plist-merge (base delta)
  "Return BASE with every key of DELTA overlaid, later keys winning."
  (let ((result (copy-sequence base)))
    (cl-loop for (key value) on delta by #'cddr
             do (setq result (plist-put result key value)))
    result))

(defun superchat-pipeline--owner-prefixed-key-p (key)
  "Return non-nil when keyword KEY looks like \":owner-rest\"."
  (and (keywordp key)
       (string-match-p "\\`:[A-Za-z0-9]+-" (symbol-name key))))

;;; Envelope validation (contract §2.2)

(defconst superchat-pipeline--forbidden-envelope-keys
  '(:buffer :buffer-name :stream-function :delivery-function
    :approval-function :prompt :input)
  "Legacy Request keys an envelope must never carry (contract §2.2).")

(defconst superchat-pipeline--forbidden-extras-keys
  '(:buffer :buffer-name :stream-function :delivery-function :prompt :input)
  "Legacy Request keys `:runtime-extras' must never carry (S10-fix-2).
An extras plist is merged onto the legacy Request after the envelope itself
validated, so these routing and prompt keys would otherwise bypass the
envelope boundary.  `:approval-function' is deliberately absent: contract
§2.3 documents it as the one legacy key an envelope may carry through
`:runtime-extras'.")

(defun superchat-pipeline--invalid (reason)
  "Signal `superchat-envelope-invalid' with REASON, a human-readable string."
  (signal 'superchat-envelope-invalid (list reason)))

(defun superchat-pipeline-validate-envelope (envelope)
  "Validate ENVELOPE (contract §2.2) and return a filled-in copy.
Signals `superchat-envelope-invalid' with a reason string for: missing or
empty `:content'; an unregistered `:surface'; a `:sink' that is not a
`superchat-sink' or is not live; a `:context' key without an owner prefix;
or the presence of any legacy Request key, at the envelope's top level or
inside `:runtime-extras' (S10-fix-2 finding 3).  `:approval-function' is the
one documented legacy bridge `:runtime-extras' may carry (contract §2.3);
the routing and prompt keys `:buffer' `:buffer-name' `:stream-function'
`:delivery-function' `:prompt' `:input' stay rejected there.  The returned
copy always has `:request-id' and, when absent, `:attendance' defaulted to
`attended'."
  (let ((content (plist-get envelope :content))
        (surface (plist-get envelope :surface))
        (sink (plist-get envelope :sink))
        (context (plist-get envelope :context)))
    (unless (and (stringp content) (not (string-empty-p content)))
      (superchat-pipeline--invalid "envelope :content must be a non-empty string"))
    (unless (memq surface superchat-pipeline--surfaces)
      (superchat-pipeline--invalid
       (format "envelope :surface %S is not a registered surface" surface)))
    (cond
     ((not (superchat-sink-p sink))
      (superchat-pipeline--invalid "envelope :sink must be a superchat-sink"))
     ((not (superchat-sink-live-p sink))
      (superchat-pipeline--invalid "envelope :sink is not live")))
    (when (and context (not (superchat-pipeline--plist-p context)))
      (superchat-pipeline--invalid "envelope :context must be a plist"))
    (when (superchat-pipeline--plist-p context)
      (cl-loop for (key _value) on context by #'cddr
               unless (superchat-pipeline--owner-prefixed-key-p key)
               do (superchat-pipeline--invalid
                   (format "envelope :context key %S lacks an owner prefix" key))))
    (dolist (key superchat-pipeline--forbidden-envelope-keys)
      (when (plist-member envelope key)
        (superchat-pipeline--invalid
         (format "envelope must not carry legacy Request key %S" key))))
    (let ((extras (plist-get envelope :runtime-extras)))
      (when extras
        (unless (superchat-pipeline--plist-p extras)
          (superchat-pipeline--invalid
           "envelope :runtime-extras must be a plist"))
        (cl-loop for (key _value) on extras by #'cddr
                 when (memq key superchat-pipeline--forbidden-extras-keys)
                 do (superchat-pipeline--invalid
                     (format
                      "envelope :runtime-extras must not carry legacy Request key %S"
                      key)))))
    (let ((copy (copy-sequence envelope)))
      (setq copy (plist-put copy :request-id (format "req-%08x" (random #x100000000))))
      (unless (plist-member copy :attendance)
        (setq copy (plist-put copy :attendance 'attended)))
      copy)))

;;; Execution-Scope gate and preparation-error reporting (contract §4)

(defun superchat-pipeline--assert-execution-scope-active ()
  "Reject work when the current borrowed execution Scope was retired.

The Scope itself remains caller-owned: this gate neither disposes it nor
changes plugin registrations."
  (when (superchat-execution-scope-retired-p
         (superchat-execution-scope-resolve nil))
    (error "Superchat execution Scope is retired")))

(defun superchat-pipeline--collect-context (envelope state tools)
  "Collect ENVELOPE's context fragments for STATE and candidate TOOLS.
This is the driver's direct call to the `collect-context' Event: one core
call, with the contributors still reached as Event listeners, so a plugin
adds or withdraws fragments through its Scope instead of a fixed preparation
registry.  An act/inspector plan has already crossed this boundary.  Its
prepared context supplies the captured system fragments and exact final
body; collecting again would duplicate or restore user-deleted material.

The borrowed execution Scope is checked before and after the call, because
listeners are public extension code and one may retire the Scope it runs
under; the rest of preparation must not continue in that case.

A preparation failure is reported as the `context-error' Event (listener
errors are recorded, never raised) and then re-signalled unchanged, so an
observer keeps seeing the established collection failure arguments while
the failure itself still terminates preparation.  The name is historical:
the event describes a failed context contribution, not a pipeline registry."
  (superchat-pipeline--assert-execution-scope-active)
  (let ((result
         (condition-case err
             (let* ((context (plist-get envelope :context))
                    (prepared (plist-get context :chat-prepared-context)))
               (if prepared
                   (let ((body (plist-get context :chat-prompt-override)))
                     (unless (and (stringp body) (not (string-empty-p body)))
                       (error "Prepared chat request has no final body"))
                     (append
                      (plist-get prepared :system-fragments)
                      (list (list :owner 'chat :id "content" :slot 'template
                                  :text body :priority 0
                                  :budget-class 'hard :required t))))
                 (apply #'append
                        (superchat-context-collect envelope state tools))))
           (error
            (ignore-errors
              (superchat-emit (superchat-execution-scope-resolve nil)
                              'context-error 'collect-context err envelope))
            (signal (car err) (cdr err))))))
    (superchat-pipeline--assert-execution-scope-active)
    result))

;;; Core defaults implemented directly by this slice (contract §4 "Absent")

(defun superchat-pipeline--merge-state (&optional envelope)
  "Return ENVELOPE's core `state'.
The old state-loading extension used to fold contributor deltas over these
defaults, and nothing was ever registered for it, so these defaults are the
whole state and the fold has no extension point to serve.

The core defaults are `:runtime-authority-mode' `ask' and
`:runtime-authority-policy' \"default\" (contract §4 row 3 \"absent\"), unless
ENVELOPE's `:context' carries `:chat-authority-mode'/`:chat-authority-policy'
(contract §9: the chat surface snapshots the View's
`superchat-authority-mode'/`superchat--active-authority-policy' there), in
which case those values seed the state instead.  This is a separate,
pipeline-`state' channel from the envelope's own top-level `:authority-policy'
field, which still flows unchanged to Runtime's own Request `:authority-policy'
key via `superchat-pipeline--dispatch-to-runtime'."
  (let ((context (plist-get envelope :context)))
    (list :runtime-authority-mode
          (or (plist-get context :chat-authority-mode) 'ask)
          :runtime-authority-policy
          (or (plist-get context :chat-authority-policy) "default"))))

(defconst superchat-pipeline--admit-result-keys '(:action :reason :target-run-id)
  "Keys `superchat-pipeline--valid-admit-result-p' recognizes on an admit
result; any other key makes the result malformed (S03a-fix-3 item 4).")

(defun superchat-pipeline--valid-admit-result-p (result)
  "Return non-nil when RESULT satisfies the admission result shape
`(:action process|queue|steer|drop :reason STRING :target-run-id
STRING-or-nil)' (contract §4 row 2).  `:action' is REQUIRED and must be one
of the four recognized symbols (S03a-fix-3 item 4 -- a missing `:action'
no longer validates); `:reason', when present, must be a string;
`:target-run-id', when present, must be nil or a string; any key outside
`superchat-pipeline--admit-result-keys' makes RESULT malformed."
  (and (superchat-pipeline--plist-p result)
       (memq (plist-get result :action) '(process queue steer drop))
       (or (not (plist-member result :reason))
           (stringp (plist-get result :reason)))
       (or (not (plist-member result :target-run-id))
           (let ((target (plist-get result :target-run-id)))
             (or (null target) (stringp target))))
       (cl-loop for (key _value) on result by #'cddr
                always (memq key superchat-pipeline--admit-result-keys))))

(defun superchat-pipeline--budget (&optional target-model)
  "Return the character budget for `assemble-prompt'.
When the LLM module is loaded, use the real model-derived
`superchat--llm-context-budget-chars' for TARGET-MODEL, with
`superchat-context-max-chars' bound to nil exactly like every other
whole-prompt assembly (R3-L9: that knob caps the `history' collector's own
budget, not the whole prompt).  Falls back to
`superchat--default-context-budget-chars' or 40000 for a headless Kernel
load without the LLM module."
  (if (fboundp 'superchat--llm-context-budget-chars)
      (let ((superchat-context-max-chars nil))
        (or (superchat--llm-context-budget-chars nil nil target-model)
            (bound-and-true-p superchat--default-context-budget-chars)
            40000))
    (or (bound-and-true-p superchat--default-context-budget-chars) 40000)))

(defun superchat-pipeline--context-fragments (envelope collected)
  "Join ENVELOPE's base content and system prompt with COLLECTED fragments.
A collector-provided content fragment replaces the base user content.  The
explicit system prompt is independent of collected system instructions;
equal text does not make either contribution redundant."
  (let* ((system (plist-get envelope :system-prompt))
         (system-fragment
          (and (stringp system) (not (string-empty-p system))
               (list :owner 'core :id "envelope-system-prompt" :slot 'system
                     :text system :priority 0 :budget-class 'hard :required t))))
    (append
     (unless (cl-find "content" collected
                      :key (lambda (fragment) (plist-get fragment :id))
                      :test #'equal)
       (list (list :owner 'core :id "content" :slot 'template
                   :text (plist-get envelope :content)
                   :priority 0 :budget-class 'hard :required t)))
     (and system-fragment (list system-fragment))
     collected)))

(defvar superchat-pipeline--host-buffer-name " *superchat-pipeline-host*"
  "Name of the dedicated hidden host buffer for envelope submissions that
carry no surface buffer in `:context'.  Never displayed; created once and
reused (contract-adjacent note in this slice's \"Current code\" section).")

(defun superchat-pipeline--host (envelope session-id)
  "Return ENVELOPE's host buffer for Runtime's `:buffer' admission gate.
Prefers a surface buffer from `:context' (`:inline-buffer', `:rewrite-buffer',
`:chat-buffer', then the generic `:host-buffer' used by producers without a
user-facing surface buffer -- subagent, skills, workflow, compaction; S13,
contract §2.2); falls back to a dedicated hidden buffer.
Validated against SESSION-ID through
`superchat-editor-workspace-validate-buffer' before returning, exactly like
every other producer call site -- the hidden fallback buffer is always
Session-unowned, so validation is a no-op for it."
  (let* ((context (plist-get envelope :context))
         (buffer (or (plist-get context :inline-buffer)
                     (plist-get context :rewrite-buffer)
                     (plist-get context :chat-buffer)
                     (plist-get context :host-buffer))))
    (superchat-editor-workspace-validate-buffer
     (if (buffer-live-p buffer)
         buffer
       (let ((host (get-buffer superchat-pipeline--host-buffer-name)))
         (if (buffer-live-p host)
             host
           (get-buffer-create superchat-pipeline--host-buffer-name))))
     session-id)))

;;; The driver

(defun superchat-pipeline--plist-extend (base &rest pairs)
  "Return BASE with each KEY VALUE in PAIRS appended, skipping a nil VALUE.
Several of Runtime's optional Request keys (`:parent-run-id' notably) are
validated only when the key is *present at all*, nil or not; a key whose
envelope value is nil must therefore be left out of the Request entirely,
not passed through as an explicit nil, so Runtime falls back to its own
default (`:deadline' is the same story: present-with-nil means \"no
deadline\", absent means Runtime's own default deadline -- omitting an
unset envelope `:deadline' preserves that default instead of cancelling it)."
  (let ((result (copy-sequence base)))
    (cl-loop for (key value) on pairs by #'cddr
             when value
             do (setq result (plist-put result key value)))
    result))

(defun superchat-pipeline--context-envelope (envelope)
  "Return the continuity ENVELOPE's prompt assembled from, or nil.
The chat surface stores it on its Turn's state during prompt assembly; other
surfaces have none.  Reading the stored envelope keeps the request evidence
pointing at the selection that actually happened."
  (let* ((context (plist-get envelope :context))
         (prepared (plist-get context :chat-prepared-context))
         (turn (and (listp context) (plist-get context :chat-turn)))
         (state (and turn (superchat-turn-state turn))))
    (if prepared
        ;; An edited body is opaque user text.  Keep its original preview on
        ;; the plan, but do not assert the old history selection still occurs
        ;; in the request or use it as an overflow-replacement target.
        (and (equal (plist-get context :chat-prompt-override)
                    (plist-get prepared :prompt))
             (plist-get prepared :context-envelope))
      (and (listp state) (plist-get state :context-envelope)))))

(defun superchat-pipeline--dispatch-to-runtime
    (envelope request-id session-id surface sink tools tool-decision prompt)
  "Build the legacy Request for the prepared PROMPT and submit it.
Returns the Runtime handle plus `:request-id' (contract §4, submission).
TOOL-DECISION is the full `select-tools' decision (`:names'/`:reason')
this call already made; it travels on the Request's `:tool-decision' so
the LLM layer honors it as-is instead of re-deciding TOOLS as `explicit'
\(S09-fix-2 item 2).

ENVELOPE's `:runtime-extras' plist -- keys the envelope does not model yet
\(e.g. `:input-core', `:workspace-id', `:workspace-root', `:context-envelope',
`:lang', `:accepted-function', `:owner-stop-function', `:transcript-owner',
`:actor-id', `:correlation-id', `:preset', `:max-tokens') -- is copied onto
the legacy Request verbatim, with every field this function itself computes
winning on any collision.  Not validated: the envelope's forbidden-key list
(contract §2.2) guards the envelope, not the legacy Request these extras
join.  S13/S25 retire this passthrough as producers move fully onto modeled
envelope fields."
  (superchat-pipeline--assert-execution-scope-active)
  (let* ((host (superchat-pipeline--host envelope session-id))
         (user-message (plist-get (car (plist-get prompt :messages)) :content))
         ;; Contract §2.2: the envelope's own `:origin' passes through to
         ;; the Run, merged with -- never replaced by -- the mandatory
         ;; surface attribution (finding 5 of S03a-fix-2's review).
         (origin (superchat-pipeline--plist-merge
                  (plist-get envelope :origin) (list :surface surface)))
         (request
          (superchat-pipeline--plist-merge
           (plist-get envelope :runtime-extras)
           (superchat-pipeline--plist-extend
            (list :prompt user-message
                  :input (plist-get envelope :content)
                  :system-prompt (or (plist-get prompt :system) "")
                  :session-id session-id
                  :buffer host
                  :sink sink
                  :tools tools
                  :tool-decision tool-decision
                  :tool-guidance (plist-get prompt :tool-guidance)
                  :attendance (plist-get envelope :attendance)
                  :surface surface
                  :origin origin)
            :parent-run-id (plist-get envelope :parent-run-id)
            :target-model (plist-get envelope :target-model)
            :capability (plist-get envelope :capability)
            :topology (plist-get envelope :topology)
            :authority-policy (plist-get envelope :authority-policy)
            :deadline (plist-get envelope :deadline)
            :context-files (plist-get envelope :media)
            :attachments (plist-get envelope :attachments)
            ;; Invariant 1: the rewrite host's bytes at submit (an inline-ask
            ;; host is the user's own buffer, which they may keep editing).
            :host-hash (and (eq surface 'inline-rewrite)
                            (buffer-live-p host)
                            (buffer-hash host))
            ;; Request evidence (M1): the continuity envelope the prompt's
            ;; history fragment was assembled from.  It is read here, after
            ;; assembly, because the dispatcher builds its plan before the
            ;; prompt exists and would therefore carry nothing.
            :context-envelope
            (superchat-pipeline--context-envelope envelope)
            :request-id request-id)))
         ;; Invariant 4: the assembly receipt is on the Tape before the model
         ;; sees the prompt.
         (receipt-record
          (ignore-errors
            (superchat-tape-append-event
             (list :type "prompt.assembled" :session-id session-id :actor-id "kernel"
                   :summary (format "prompt.assembled: %s" request-id)
                   :payload (list :request-id request-id :surface (symbol-name surface)
                                  :receipt (plist-get prompt :receipt)
                                  :system-chars (length (or (plist-get prompt :system) ""))
                                  :message-chars (length (or user-message "")))))))
         ;; `prompt-reconstructable' is checked where the request evidence is
         ;; complete -- after the `request.header' and `request.message-list'
         ;; rows exist (see `superchat--llm-record-request-evidence').  Running
         ;; it here could only test that the assembly receipt was appended,
         ;; which is exactly the weaker property the accepted decision replaced.
         (_ (unless receipt-record
              (superchat-invariant-check
               'prompt-reconstructable session-id nil nil)))
         (handle (superchat-runtime-submit request)))
    (append handle (list :request-id request-id))))

;;; Legacy Request → envelope (S14; the shape S10/S13 producers use by hand)

(defconst superchat-pipeline--request-envelope-keys
  '(:session-id :target-model :capability :topology :authority-policy
    :origin :attendance :parent-run-id :deadline :system-prompt :tools)
  "Legacy Request keys that map onto the envelope's own fields unchanged.")

(defconst superchat-pipeline--request-consumed-keys
  '(:type :prompt :input :clean-input :buffer :buffer-name
    :stream-function :delivery-function :context-files :surface)
  "Legacy Request keys the translation consumes rather than passing through.")

(defun superchat-pipeline-envelope-from-request (request &optional context)
  "Translate a legacy Runtime REQUEST plist into a Request envelope.
For plugins moving off `superchat-runtime-submit': `:prompt' becomes
`:context :prepared-prompt' (sent unchanged), `:input' the `:content',
`:buffer' the `:context :host-buffer', `:context-files' the `:media',
`:stream-function' / `:delivery-function' a callback Sink with the legacy
delivery shape (TEXT STATUS TAPE-ID); every other key --
`:approval-function', `:tool-validator', `:preset', `:actor-id',
`:record-conversation' … -- travels in `:runtime-extras'.  CONTEXT is an
optional owner-prefixed plist merged into the envelope's `:context'
\(`:telega-chat-id', say).  Contract §2.2, §2.3."
  (let* ((stream-function (plist-get request :stream-function))
         (delivery (plist-get request :delivery-function))
         (prompt (plist-get request :prompt))
         (surface (or (plist-get request :surface)
                      (plist-get (plist-get request :origin) :surface)
                      'external))
         (envelope
          (list :surface surface
                :content (or (plist-get request :input)
                             (plist-get request :clean-input)
                             prompt)
                :media (plist-get request :context-files)
                :sink (superchat-sink-callback
                       :id (format "%s-%08x" surface (random #x100000000))
                       :stream (lambda (_run chunk)
                                 (when (and (functionp stream-function)
                                            (not (eq stream-function #'ignore)))
                                   (let* ((payload (if (and (listp chunk)
                                                            (plist-member chunk :kind))
                                                       chunk
                                                     (superchat-sink-normalize-chunk chunk)))
                                          (text (plist-get payload :text)))
                                     (when (stringp text)
                                       (funcall stream-function
                                                (if (eq (plist-get payload :kind) 'reasoning)
                                                    (list :reasoning text)
                                                  text))))))
                       :final (lambda (_run result)
                                (when (functionp delivery)
                                  (funcall delivery (or (plist-get result :text) "")
                                           'completed (plist-get result :tape-id))))
                       :error (lambda (_run failure)
                                (when (functionp delivery)
                                  (funcall delivery (or (plist-get failure :text) "")
                                           (plist-get failure :status)
                                           (plist-get failure :tape-id)))))
                :context (append
                          (list :host-buffer (plist-get request :buffer)
                                :prepared-prompt prompt
                                :prepared-system (plist-get request :system-prompt))
                          context)))
         extras)
    (when (plist-member request :attachments)
      (setq envelope (plist-put envelope :attachments
                                (plist-get request :attachments))))
    (dolist (key superchat-pipeline--request-envelope-keys)
      (when (plist-member request key)
        (setq envelope (plist-put envelope key (plist-get request key)))))
    (when (plist-get request :input-core)
      (setq envelope (plist-put envelope :input-core (plist-get request :input-core))))
    (cl-loop for (key value) on request by #'cddr
             unless (or (memq key superchat-pipeline--request-envelope-keys)
                        (memq key superchat-pipeline--request-consumed-keys))
             do (setq extras (plist-put extras key value)))
    (when (plist-member request :clean-input)
      (setq extras (plist-put extras :clean-input (plist-get request :clean-input))))
    (plist-put envelope :runtime-extras extras)))

(defun superchat-pipeline-submit (envelope)
  "Validate ENVELOPE, prepare input with core defaults, and submit to
Runtime as a legacy request carrying ENVELOPE's exact Sink (contract §4).
Returns the Runtime handle plus `:request-id' on the `process' path, or
`(:status ACTION :reason R :request-id ID …)' without calling Runtime when
`admit' resolves to `queue', `steer' or `drop'."
  ;; A caller Scope is borrowed for this execution only.  It is not added to
  ;; the Envelope (which is durable/public-facing request data), and it is
  ;; captured privately by Runtime when a Run is accepted.
  (let ((superchat-execution-scope (superchat-execution-scope-resolve nil)))
    (superchat-pipeline--assert-execution-scope-active)
    (cl-block superchat-pipeline-submit
      (let* ((submitted-envelope envelope)
           (envelope (superchat-pipeline-validate-envelope envelope))
           (request-id (plist-get envelope :request-id))
           (explicit-session-id (plist-get envelope :session-id))
           (surface (plist-get envelope :surface))
           (sink (plist-get envelope :sink))
           ;; Contract §2.2: an explicit `:session-id' is never retargeted.
           ;; Session identity is Runtime-owned, so the pipeline resolves it
           ;; directly; the old session-resolution extension had no
           ;; contributor.
           (session-id
            (or explicit-session-id
                (format "ephemeral-%s" request-id)))
           (admission
            (let ((superchat-runtime--dispatching-envelope
                   (and (eq submitted-envelope superchat-runtime--dispatching-envelope)
                        envelope)))
              (superchat-runtime-admit-envelope session-id envelope))))
      (unless (eq (plist-get admission :status) 'process)
        (cl-return-from superchat-pipeline-submit
          (append admission (list :request-id request-id))))
      (let* (;; State assembly is driver work: the old state-loading extension
             ;; had no contributor, so the core defaults are merged directly
             ;; rather than through an extension point with one branch.
             (state (superchat-pipeline--merge-state envelope))
             ;; The one production decision point (contract §4.2; S09-fix-1
             ;; deletes this file's own `superchat-pipeline--select-tools'
             ;; -- a second, differently-worded decider with stale fixed
             ;; baselines that could turn an envelope result into an
             ;; explicit `:tools' bypassing S09's decision).
             ;; The *whole* decision (names, reason, and any dropped names
             ;; the call already recorded as faults) is kept, not just its
             ;; names: threading only names to Runtime's `:tools' would make
             ;; the later LLM call re-decide from them and misreport a
             ;; `stage'/`baseline' pipeline decision as `explicit'
             ;; (S09-fix-2 item 2).
             (tool-decision (superchat-select-tools-decision envelope state))
             (tools (plist-get tool-decision :names))
             ;; The inspector uses the same base-fragment composition.
             ;; Collected content replaces the base body; collected system
             ;; instructions join the original, unassembled system prompt.
             (collected
              (superchat-pipeline--collect-context envelope state tools))
             (fragments
              (superchat-pipeline--context-fragments envelope collected))
             ;; Prompt assembly is core: the old prompt-assembly extension
             ;; had exactly one contributor, the `prompt-assembler' default
             ;; plugin's core assembler, so it is called directly.
             (prompt
              (superchat-prompt-assemble
               fragments
               ;; A producer that prepared its whole prompt (subagent,
               ;; workflow, skills-match, compaction -- whose input is by
               ;; design a large slice of the window) already sized it;
               ;; re-budgeting would reject exactly the prompts compaction
               ;; exists for (review finding F1).
               (if (plist-get (plist-get envelope :context) :prepared-prompt)
                   most-positive-fixnum
                 (superchat-pipeline--budget (plist-get envelope :target-model))))))
          ;; Keep only pure, tool-dependent system renderers for later request
          ;; rewrites.  Collectors themselves are never rerun by the waterfall.
          (setq prompt
                (plist-put prompt :tool-guidance
                           (delq nil
                                 (mapcar
                                  (lambda (fragment)
                                    (when (and (memq (plist-get fragment :slot)
                                                     '(system instruction))
                                               (functionp (plist-get fragment :render-tools)))
                                      (cons (plist-get fragment :text)
                                            (plist-get fragment :render-tools))))
                                  fragments))))
          (superchat-pipeline--dispatch-to-runtime
           envelope request-id session-id surface sink tools tool-decision prompt))))))

(define-error 'superchat-pipeline-not-submitted
  "Superchat pipeline: blocking submission did not create a Run")

(defun superchat-pipeline-submit-blocking (envelope &optional timeout)
  "Submit ENVELOPE through the pipeline, then wait for its terminal response.
Only synchronous boundary adapters (Org Babel and Magit) use this entry.
An omitted Sink is replaced by a capture Sink, never a default buffer Sink.
An explicit Sink's terminal condition is re-signalled unchanged after sealing.
TIMEOUT defaults to ENVELOPE's :timeout, then the response timeout.
Admission without a new Run signals `superchat-pipeline-not-submitted'.
The capture Sink is live only for this call: queued work cannot outlive a
rejected blocking submission.  A lost inner Sink settles as cancelled."
  (let* ((envelope (copy-sequence envelope))
         (inner (plist-get envelope :sink))
         (limit (or timeout
                    (plist-get envelope :timeout)
                    (and (boundp 'superchat-response-timeout)
                         superchat-response-timeout)
                    120))
         (live t)
         response status done delivery-condition handle timed-out)
    (cl-labels
        ((deliver (slot run payload)
           (when inner
             (let ((emit (superchat-sink-emit inner slot run payload :capture-quit t)))
               (when (plist-get emit :condition)
                 ;; Runtime records the delivery fault; this boundary re-raises
                 ;; the original condition after the Run is sealed.
                 (unless delivery-condition
                   (setq delivery-condition (plist-get emit :condition)))
                 (signal (car delivery-condition) (cdr delivery-condition)))))))
      (unwind-protect
          (progn
            (setq envelope
                  (plist-put
                   envelope :sink
                   (superchat-sink-create
                    :id (format "blocking-%08x" (random #x100000000))
                    :live-p (lambda () (and live (or (null inner) (superchat-sink-live-p inner))))
                    :stream (and inner (lambda (run chunk) (superchat-sink-emit inner 'stream run chunk)))
                    :status (and inner (lambda (run st) (superchat-sink-emit inner 'status run st)))
                    :final (lambda (run payload)
                             (setq response (plist-get payload :text) status 'completed done t)
                             (deliver 'final run payload))
                    :error (lambda (run failure)
                             (setq response (plist-get failure :text)
                                   status (plist-get failure :status) done t)
                             (deliver 'error run failure))
                    :orphaned (lambda (run reason)
                                (setq response "[Runtime Sink lost]" status 'cancelled done t)
                                (deliver 'orphaned run reason)))))
            (setq handle (superchat-pipeline-submit envelope))
            (unless (and (eq (plist-get handle :status) 'accepted)
                         (plist-get handle :run-id))
              (signal 'superchat-pipeline-not-submitted (list handle)))
            (let ((deadline (+ (float-time) limit)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.05))
              (unless done
                (setq timed-out t)
                (superchat-runtime-cancel
                 (plist-get handle :run-id) (format "[Timed out after %ss]" limit))))
            (when delivery-condition
              (signal (car delivery-condition) (cdr delivery-condition)))
            (when timed-out
              (error "Superchat Runtime timed out after %ss" limit))
            (if (eq status 'completed)
                response
              (error "Superchat Runtime %s: %s" status response)))
        (setq live nil)
        ;; A caller C-g during the wait is not a terminal Sink condition.
        ;; Clean up only this call's accepted, still-active Run; never close
        ;; its host, and never replace the condition already unwinding.
        (when (and (eq (plist-get handle :status) 'accepted)
                   (plist-get handle :run-id))
          (let* ((run-id (plist-get handle :run-id))
                 (run (superchat-runtime-status run-id)))
            (when (and run (not (superchat-runtime--terminal-p (plist-get run :status))))
              (condition-case condition
                  (superchat-runtime-cancel run-id "[Blocking caller exited]")
                ((error quit)
                 (superchat-event-record-fault 'blocking run-id 'driver condition))))))))))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(declare-function superchat-runtime-resume-follow-ups "superchat-runtime" ())
(declare-function superchat-runtime-retire-transport "superchat-runtime" (provider-id))
(declare-function superchat-runtime-retire-scope "superchat-runtime" (scope &optional reason))
(declare-function superchat-runtime-cancel "superchat-runtime")
(declare-function superchat-runtime-steer "superchat-runtime")
(declare-function superchat-runtime-status "superchat-runtime")
(declare-function superchat-runtime-active-runs "superchat-runtime")
(declare-function superchat-runtime-pending-input "superchat-runtime")
(declare-function superchat-runtime-answer-input "superchat-runtime")
(declare-function superchat-runtime-enqueue-follow-up "superchat-runtime")
(declare-function superchat-runtime-pending-follow-ups "superchat-runtime")
(declare-function superchat-runtime-clear-follow-ups "superchat-runtime")
(declare-function superchat-runtime--terminal-p "superchat-runtime")
;;; The opaque agent handle ---------------------------------------------------
;;
;; A handle is process-local and addresses one Session's Runtime-owned
;; scheduling state.  It is deliberately a closure over the Session id rather
;; than a mutable record: there is nothing in it a holder can write, no queue
;; to drain, no Run to continue and no decision to make.  Anything a caller
;; wants to DO still goes through the driver Service's own methods, so the
;; handle cannot become a second authority over the same state.

(defun superchat-pipeline--agent-handle (session-id)
  "Return an opaque handle addressing SESSION-ID's scheduling state."
  (let ((id (copy-sequence session-id)))
    (lambda (operation)
      (pcase operation
        ('session-id id)
        ('state (superchat-runtime-session-scheduling-state id))
        (_ (error "Unknown agent-handle operation: %S" operation))))))

(defun superchat-pipeline--agent-handle-p (value)
  "Return non-nil when VALUE is an agent handle this driver produced."
  (and (functionp value)
       (condition-case nil
           (let ((id (funcall value 'session-id)))
             (and (stringp id) (not (string-empty-p id))))
         (error nil))))

(defun superchat-pipeline--agent-handle-state (handle)
  "Return HANDLE's read-only scheduling state, or signal for a non-handle."
  (unless (superchat-pipeline--agent-handle-p handle)
    (error "Not an agent handle: %S" handle))
  (funcall handle 'state))


(superchat-define-plugin driver
  :inject '(tape model-transport)
  :doc "Envelope validation, the execution main line and the Run lifecycle."
  :apply
  (lambda (scope _)
    (let ((provider-id (plist-get (superchat-service-get nil 'model-transport) :id)))
      ;; Registered before the driver Service, so its public entry points are
      ;; withdrawn first.  Exact provider liveness already blocks old effects
      ;; as soon as model-transport disappears, before any notify can reenter.
      (superchat-scope-effect
       scope (lambda ()
               (unless (eq provider-id (plist-get (superchat-service-get nil 'model-transport) :id))
                 (superchat-runtime-retire-transport provider-id))))
    (superchat-service-provide
     scope 'driver
     (superchat-service-table
      submit (lambda (envelope)
               (unless (eq provider-id (plist-get (superchat-service-get nil 'model-transport) :id))
                 (error "Driver transport epoch is no longer active"))
               (superchat-pipeline-submit envelope))
      submit-blocking (lambda (envelope &optional timeout)
                        (unless (eq provider-id (plist-get (superchat-service-get nil 'model-transport) :id))
                          (error "Driver transport epoch is no longer active"))
                        (superchat-pipeline-submit-blocking envelope timeout))
      cancel #'superchat-runtime-cancel
      retire-scope #'superchat-runtime-retire-scope
      steer #'superchat-runtime-steer
      status #'superchat-runtime-status
      active-runs #'superchat-runtime-active-runs
      pending-input #'superchat-runtime-pending-input
      answer-input #'superchat-runtime-answer-input
      enqueue-follow-up #'superchat-runtime-enqueue-follow-up
      pending-follow-ups #'superchat-runtime-pending-follow-ups
      clear-follow-ups #'superchat-runtime-clear-follow-ups
      terminal-p #'superchat-runtime--terminal-p
      ;; ── The opaque agent handle.  M3 asks for an addressable handle
      ;; resolved for a Session that exposes neither a queue nor a writable
      ;; loop and grants no Authority; this is that and only that.  Producers
      ;; keep `submit' as the submission path, and `steer' and `inject'
      ;; resolve through this same driver owner rather than a second registry.
      handle (lambda (session-id)
               (when (and (stringp session-id) (not (string-empty-p session-id)))
                 (superchat-pipeline--agent-handle session-id)))
      handle-p #'superchat-pipeline--agent-handle-p
      handle-state #'superchat-pipeline--agent-handle-state
      inject (lambda (session-id material &optional inject-scope)
               (superchat-runtime-inject session-id material inject-scope))
      ;; ── The transport-facing attempt seam.  A model transport that is
      ;; not the native llm.el loop needs the same Step brackets, the same
      ;; pre-step gate and the same inbox claim.  These two operations are that
      ;; seam, so the behaviour belongs to the driver rather than to one
      ;; adapter's callback protocol.
      advance-step #'superchat-runtime-step-advance
      turn-stopping #'superchat-runtime-turn-stopping))
    (superchat-runtime-resume-follow-ups))))

(provide 'superchat-pipeline)

;;; superchat-pipeline.el ends here
