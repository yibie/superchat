;;; superchat-pipeline.el --- Envelope validation and the fixed driver -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; `superchat-pipeline-submit' (contract §4, `docs/design/turn-pipeline-contract.md')
;; validates an envelope, runs stages 1-6 with core defaults, and hands the
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
(require 'superchat-stage)
(require 'superchat-invariant)
(declare-function superchat-tape-append-event "superchat-tape" (event))

(declare-function superchat-runtime-submit "superchat-runtime" (request))
(declare-function superchat-select-tools-decision "superchat-llm" (request state))
(declare-function superchat-editor-workspace-validate-buffer
                  "superchat-editor-workspace" (buffer session-id))
(declare-function superchat--llm-context-budget-chars
                  "superchat-llm"
                  (&optional system-prompt turn-prompt target-model))

(define-error 'superchat-envelope-invalid
  "Superchat pipeline: envelope does not satisfy the Request envelope contract")

;;; Surfaces (contract §2.1)

(defvar superchat-pipeline--surfaces
  '(chat inline-ask inline-rewrite org-babel magit workflow subagent
    compaction skills external)
  "Registered Producer surface symbols.  Seeded with the built-in ten; a
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

;;; Stage-error wrapping (contract §4, "Wrap stage errors")

(defun superchat-pipeline--run-stage (name envelope thunk)
  "Call THUNK for stage NAME.  On any `error', emit the `stage-error' Event
\(listener errors are recorded, never raised) and re-signal the condition."
  (condition-case err
      (funcall thunk)
    (error
     (ignore-errors (superchat-emit nil 'stage-error name err envelope))
     (signal (car err) (cdr err)))))

;;; Stage defaults implemented directly by this slice (contract §4 "Absent")

(defun superchat-pipeline--merge-state (deltas &optional envelope)
  "Merge `load-state' DELTAS oldest-first over the core defaults.
DELTAS is `collect' output, newest registration first, so it is reversed
before folding.  A delta whose `:delta' plist has a key not prefixed by its
own `:owner' is dropped and recorded as a `load-state' fault instead of
being merged.

The core defaults are `:runtime-authority-mode' `ask' and
`:runtime-authority-policy' \"default\" (contract §4 row 3 \"absent\"), unless
ENVELOPE's `:context' carries `:chat-authority-mode'/`:chat-authority-policy'
(contract §9: the chat surface snapshots the View's
`superchat-authority-mode'/`superchat--active-authority-policy' there), in
which case those values seed the state instead.  This is a separate,
pipeline-`state' channel from the envelope's own top-level `:authority-policy'
field, which still flows unchanged to Runtime's own Request `:authority-policy'
key via `superchat-pipeline--dispatch-to-runtime'."
  (let* ((context (plist-get envelope :context))
         (state
          (list :runtime-authority-mode
                (or (plist-get context :chat-authority-mode) 'ask)
                :runtime-authority-policy
                (or (plist-get context :chat-authority-policy) "default"))))
    (dolist (entry (reverse deltas))
      (let* ((owner (plist-get entry :owner))
             (delta (plist-get entry :delta))
             (prefix (and owner (concat ":" (symbol-name owner) "-"))))
        (if (and owner prefix (superchat-pipeline--plist-p delta)
                 (cl-loop for (key _value) on delta by #'cddr
                          always (string-prefix-p prefix (symbol-name key))))
            (setq state (superchat-pipeline--plist-merge state delta))
          (superchat-stage-record-fault
           'load-state (format "%s-delta" (or owner "unknown")) owner
           (list :dropped-delta delta)))))
    state))

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

(defun superchat-pipeline--dispatch-to-runtime
    (envelope request-id session-id surface sink tools tool-decision prompt)
  "Build the legacy Request for the prepared PROMPT and submit it.
Returns the Runtime handle plus `:request-id' (contract §4, stage 7).
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
            ;; Invariant 1: the inline host's bytes at submit.
            :host-hash (and (memq surface '(inline-ask inline-rewrite))
                            (buffer-live-p host)
                            (buffer-hash host)))))
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
         (_ (superchat-invariant-check 'prompt-reconstructable session-id nil receipt-record))
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
  "Validate ENVELOPE, run stages 1-6 with core defaults, and submit to
Runtime as a legacy request carrying ENVELOPE's exact Sink (contract §4).
Returns the Runtime handle plus `:request-id' on the `process' path, or
`(:status ACTION :reason R :request-id ID …)' without calling Runtime when
`admit' resolves to `queue', `steer' or `drop'."
  (cl-block superchat-pipeline-submit
    (let* ((envelope (superchat-pipeline-validate-envelope envelope))
           (request-id (plist-get envelope :request-id))
           (explicit-session-id (plist-get envelope :session-id))
           (surface (plist-get envelope :surface))
           (sink (plist-get envelope :sink))
           (session-id
            (superchat-pipeline--run-stage
             'resolve-session envelope
             (lambda ()
               ;; Contract §2.2: an explicit `:session-id' is never
               ;; retargeted -- the resolver stage is consulted only when
               ;; there is none to protect.
               (or explicit-session-id
                   (superchat-stage-call 'resolve-session envelope)
                   (format "ephemeral-%s" request-id)))))
           (admit-outcome
            (superchat-pipeline--run-stage
             'admit envelope
             (lambda ()
               ;; Invocation-local: a fault from a stage call made *inside*
               ;; an `admit' implementation (a nested `load-state', say)
               ;; must never be attributed to `admit' itself -- exactly the
               ;; false-drop the global-fault-ring diff in the now-deleted
               ;; `superchat-pipeline--stage-call-with-fault-p' risked
               ;; (S03a-fix-3 item 3; `select-tools' below now delegates to
               ;; `superchat-select-tools', which uses the same
               ;; invocation-local `superchat-stage-call-outcome' -- S09-fix-1).
               (superchat-stage-call-outcome
                'admit session-id envelope
                (list :active-run-id nil :pending-count 0)))))
           (admit-result (plist-get admit-outcome :result))
           (admit-faulted-p (plist-get admit-outcome :faulted))
           (admit-malformed-p (and admit-result
                                    (not (superchat-pipeline--valid-admit-result-p admit-result))))
           (admit-fault-reason
            (cond
             (admit-malformed-p
              (superchat-stage-record-fault
               'admit 'malformed-result nil (list :malformed-result admit-result))
              "admit stage returned a malformed result")
             ((and (null admit-result) admit-faulted-p)
              "admit stage implementation failed")))
           (action
            (cond
             (admit-fault-reason 'drop)
             ((null admit-result) 'process)
             (t (or (plist-get admit-result :action) 'process)))))
      (unless (eq action 'process)
        (cl-return-from superchat-pipeline-submit
          (list :status action
                :reason (or admit-fault-reason (plist-get admit-result :reason))
                :target-run-id (plist-get admit-result :target-run-id)
                :request-id request-id)))
      (let* ((deltas
              (superchat-pipeline--run-stage
               'load-state envelope
               (lambda () (superchat-stage-call 'load-state envelope session-id))))
             (state (superchat-pipeline--merge-state deltas envelope))
             ;; The one production decision point (contract §4.2; S09-fix-1
             ;; deletes this file's own `superchat-pipeline--select-tools'
             ;; -- a second, differently-worded decider with stale fixed
             ;; baselines that could turn an envelope result into an
             ;; explicit `:tools' bypassing S09's stage/baseline choice).
             ;; The *whole* decision (names, reason, and any dropped names
             ;; the call already recorded as stage faults) is kept, not
             ;; just its names: threading only names to Runtime's `:tools'
             ;; would make the later LLM call re-decide from them and
             ;; misreport a `stage'/`baseline' pipeline decision as
             ;; `explicit' (S09-fix-2 item 2).
             (tool-decision
              (superchat-pipeline--run-stage
               'select-tools envelope
               (lambda () (superchat-select-tools-decision envelope state))))
             (tools (plist-get tool-decision :names))
             (content-fragment
              (list :owner 'core :id "content" :slot 'template
                    :text (plist-get envelope :content)
                    :priority 0 :budget-class 'hard :required t))
             (envelope-system-prompt (plist-get envelope :system-prompt))
             (system-prompt-fragment
              (and (stringp envelope-system-prompt)
                   (not (string-empty-p envelope-system-prompt))
                   (list :owner 'core :id "envelope-system-prompt" :slot 'system
                         :text envelope-system-prompt
                         :priority 0 :budget-class 'hard :required t)))
             ;; Contract §4 row 5: the core content fragment (and, when
             ;; supplied, the envelope's own `:system-prompt' as a system
             ;; fragment) joins the collected fragments -- unless a collector
             ;; itself produced the \"content\" fragment (the chat surface's
             ;; template-substituted input, or a producer's prepared text;
             ;; spec §1.4).  One prompt path for every surface (step 3).
             (collected
              (superchat-pipeline--run-stage
               'collect-context envelope
               (lambda ()
                 (apply #'append
                        (superchat-stage-call 'collect-context envelope state tools)))))
             (fragments
              (append (unless (cl-find "content" collected
                                       :key (lambda (f) (plist-get f :id)) :test #'equal)
                        (list content-fragment))
                      (and system-prompt-fragment (list system-prompt-fragment))
                      collected))
             (prompt
              (superchat-pipeline--run-stage
               'assemble-prompt envelope
               (lambda ()
                 (superchat-stage-call
                  'assemble-prompt fragments
                  ;; A producer that prepared its whole prompt (subagent,
                  ;; workflow, skills-match, compaction -- whose input is by
                  ;; design a large slice of the window) already sized it;
                  ;; re-budgeting would reject exactly the prompts
                  ;; compaction exists for (review finding F1).
                  (if (plist-get (plist-get envelope :context) :prepared-prompt)
                      most-positive-fixnum
                    (superchat-pipeline--budget (plist-get envelope :target-model))))))))
        (superchat-pipeline--dispatch-to-runtime
         envelope request-id session-id surface sink tools tool-decision prompt)))))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(declare-function superchat-runtime-submit-blocking "superchat-runtime")
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
(superchat-define-plugin driver
  :inject '(tape closure)
  :doc "Envelope validation, the fixed Stage main line and the Run lifecycle."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'driver
     (superchat-service-table
      submit #'superchat-pipeline-submit
      submit-blocking #'superchat-runtime-submit-blocking
      cancel #'superchat-runtime-cancel
      steer #'superchat-runtime-steer
      status #'superchat-runtime-status
      active-runs #'superchat-runtime-active-runs
      pending-input #'superchat-runtime-pending-input
      answer-input #'superchat-runtime-answer-input
      enqueue-follow-up #'superchat-runtime-enqueue-follow-up
      pending-follow-ups #'superchat-runtime-pending-follow-ups
      clear-follow-ups #'superchat-runtime-clear-follow-ups
      terminal-p #'superchat-runtime--terminal-p))))

(provide 'superchat-pipeline)

;;; superchat-pipeline.el ends here
