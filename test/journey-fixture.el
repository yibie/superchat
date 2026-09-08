;;; journey-fixture.el --- Real-chain fixture for journey acceptance tests -*- lexical-binding: t; -*-

;;; Commentary:

;; This fixture deliberately replaces only the llm.el transport boundary.  It
;; keeps the real chat entry point, dispatcher, Runtime, llm provider utility
;; tool loop, Authority, Tape, and tool implementations in the path.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))

;; Journey tests must fail loudly rather than skip when the native transport
;; dependencies are unavailable.  The production modules make these optional;
;; this acceptance fixture intentionally does not.
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)
(require 'superchat)

(defvar superchat-authority-effect-resolvers)
(defvar superchat-db--connection)
(defvar superchat-db--path)
(defvar superchat-editor-workspace--states)
(defvar superchat-llm-tools-list)
(defvar superchat-llm-tool-names)
(defvar superchat-llm-model)
(defvar superchat-runtime--runs)
(defvar superchat-runtime-default-deadline)
(defvar superchat-closure-terminal-guard-enabled)
(defvar superchat-show-ttft)
(defvar superchat--llm-input-batches)
(defvar superchat--llm-raw-tool-uses)
(defvar superchat--llm-tool-names-signature)
(defvar superchat--model-list-cache)
(defvar superchat--subagent-registry-signature)
(defvar superchat--subagent-registry-tool-cache)
(defvar superchat-tools--external-registry)
(defvar superchat-skills-agents-md-file)
(defvar superchat-skills-directory)

(cl-defstruct (journey-fixture-state
               (:constructor journey-fixture--state-create))
  "Per-test state retained by the fake network endpoint."
  script
  requests
  transport-modes
  cancellations
  timers
  transport-buffers
  provider
  view
  baseline-buffers
  data-directory
  home-directory)

(defvar journey-fixture--state nil
  "The active `journey-fixture-state', dynamically bound by the fixture.")

(defun journey-fixture-set-script (script)
  "Install scripted provider rounds SCRIPT for the active fixture.

Each round accepts `:chunks', `:text', `:tool-uses', `:hang', `:error', and
`:after-chunks'.  Tool uses have the probe form
`((\"tool_name\" . ((argument . value) ...)) ...)'."
  (setf (journey-fixture-state-script journey-fixture--state)
        (copy-sequence script)))

(defun journey-fixture-requests ()
  "Return captured provider requests in chronological order."
  (nreverse
   (copy-sequence (journey-fixture-state-requests journey-fixture--state))))

(defun journey-fixture-last-request ()
  "Return the latest captured provider request."
  (car (last (journey-fixture-requests))))

(defun journey-fixture-transport-modes ()
  "Return dispatched transport modes in chronological order."
  (nreverse
   (copy-sequence
    (journey-fixture-state-transport-modes journey-fixture--state))))

(defun journey-fixture-cancellations ()
  "Return real `llm-cancel-request' observations in chronological order."
  (nreverse
   (copy-sequence (journey-fixture-state-cancellations journey-fixture--state))))

(defun journey-fixture-provider ()
  "Return the real llm-openai-compatible provider used by the fixture."
  (journey-fixture-state-provider journey-fixture--state))

(defun journey-fixture-set-provider-model (model)
  "Change the chat MODEL of the fixture's real provider struct."
  (setf (llm-openai-chat-model (journey-fixture-provider)) model))

(defun journey-fixture-open-chat ()
  "Open a real Superchat Session View and return its buffer."
  (superchat)
  (let ((view (or (get-buffer superchat-buffer-name) (current-buffer))))
    (setf (journey-fixture-state-view journey-fixture--state) view)
    view))

(defun journey-fixture-view ()
  "Return the active fixture's real chat view buffer."
  (or (journey-fixture-state-view journey-fixture--state)
      (error "Journey fixture has not opened a chat view")))

(defun journey-fixture-session-id (&optional view)
  "Return VIEW's durable Session identifier.

When VIEW is nil, use `journey-fixture-view'."
  (with-current-buffer (or view (journey-fixture-view))
    superchat--session-id))

(defun journey-fixture-send (text &optional view)
  "Enter TEXT through the real chat UI and invoke `superchat-send-input'."
  (with-current-buffer (or view (journey-fixture-view))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))
    (superchat-send-input)
    superchat--active-run-id))

(defun journey-fixture-active-run-id (&optional view)
  "Return the active Run ID associated with VIEW, if one is displayed."
  (with-current-buffer (or view (journey-fixture-view))
    superchat--active-run-id))

(defun journey-fixture-run-active-p (&optional view)
  "Return non-nil while VIEW's displayed Run is active."
  (let ((run-id (journey-fixture-active-run-id view)))
    (and run-id (member run-id (superchat-runtime-active-runs)))))

(defun journey-fixture-wait-until (predicate &optional seconds)
  "Pump Emacs until PREDICATE is non-nil or SECONDS elapse.

This only drives normal timer delivery for the fake network endpoint; it does
not replace Runtime, Authority, dispatch, or rendering behavior."
  (let ((deadline (+ (float-time) (or seconds 5.0))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)
      (sit-for 0.005))
    (funcall predicate)))

(defun journey-fixture-wait-for-run (view &optional seconds)
  "Wait until VIEW has no active Run and return non-nil on success."
  (journey-fixture-wait-until
   (lambda () (not (journey-fixture-run-active-p view))) seconds))

(defun journey-fixture-events (session-id)
  "Replay real Tape events for SESSION-ID."
  (superchat-tape-replay-events session-id))

(defun journey-fixture-event-types (session-id)
  "Return real Tape event type strings for SESSION-ID in append order."
  (mapcar (lambda (event) (plist-get event :type))
          (journey-fixture-events session-id)))

(defun journey-fixture-buffer-snapshot (buffer)
  "Capture source-buffer invariants for BUFFER without changing it."
  (with-current-buffer buffer
    (list :text (buffer-substring-no-properties (point-min) (point-max))
          :modified (buffer-modified-p)
          :undo-length (if (listp buffer-undo-list) (length buffer-undo-list) 0)
          :point (point))))

(defun journey-fixture-buffers-containing (needle)
  "Return live buffers whose visible text contains NEEDLE."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (string-match-p
             (regexp-quote needle)
             (buffer-substring-no-properties (point-min) (point-max))))))
   (buffer-list)))

(defun journey-fixture-buffer-text-index ()
  "Return an alist of (BUFFER . TEXT) for every live buffer's visible text."
  (mapcar (lambda (buffer)
            (cons buffer
                  (with-current-buffer buffer
                    (buffer-substring-no-properties (point-min) (point-max)))))
          (buffer-list)))

(defun journey-fixture-new-matches (needle before)
  "Return live buffers whose text contains NEEDLE now but did not in BEFORE.

BEFORE is a `journey-fixture-buffer-text-index' snapshot taken earlier.  A
buffer absent from BEFORE (created since the snapshot) counts as new."
  (seq-filter
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (string-match-p (regexp-quote needle)
                            (buffer-substring-no-properties (point-min) (point-max))))
          (not (let ((prior (cdr (assq buffer before))))
                 (and prior (string-match-p (regexp-quote needle) prior))))))
   (buffer-list)))

(defun journey-fixture--record-request (state mode provider prompt)
  "Capture real llm.el PROVIDER/PROMPT input before scripted delivery."
  (let ((record
         (list :mode mode
               :model (ignore-errors (llm-openai-chat-model provider))
               :prompt-text (condition-case nil
                                (llm-chat-prompt-to-text prompt)
                              (error (prin1-to-string prompt)))
               :context (ignore-errors (llm-chat-prompt-context prompt))
               :tools (mapcar #'llm-tool-name
                              (or (ignore-errors (llm-chat-prompt-tools prompt))
                                  '()))
               :prompt prompt)))
    (push record (journey-fixture-state-requests state))
    (push mode (journey-fixture-state-transport-modes state))))

(defun journey-fixture--make-tool-use (use)
  "Build an actual llm provider utility tool-use object from USE."
  (make-llm-provider-utils-tool-use
   :id (format "journey-call-%06x" (random #x1000000))
   :name (car use)
   :args (cdr use)))

(defun journey-fixture--deliver-round
    (state provider prompt partial-callback response-callback error-callback
           multi-output step)
  "Deliver one scripted provider STEP through llm.el's real result processor."
  (let ((chunks (plist-get step :chunks)))
    (when partial-callback
      (dolist (chunk chunks)
        (funcall partial-callback
                 (if multi-output (list :text chunk) chunk))))
    (when-let* ((after-chunks (plist-get step :after-chunks)))
      (funcall after-chunks state))
    (cond
     ((plist-get step :hang) nil)
     ((plist-get step :error)
      (let ((error-spec (plist-get step :error)))
        (funcall error-callback (car error-spec) (cdr error-spec))))
     (t
      (let* ((uses (mapcar #'journey-fixture--make-tool-use
                           (or (plist-get step :tool-uses) '())))
             (result (append (when (plist-member step :text)
                               (list :text (plist-get step :text)))
                             (when uses (list :tool-uses uses)))))
        ;; This is the crucial real chain: llm.el translates tool uses into
        ;; wrapped tool calls, then the actual Superchat Authority runs them.
        (llm-provider-utils-process-result
         provider prompt result multi-output response-callback error-callback))))))

(defun journey-fixture--fake-network
    (state mode provider prompt partial-callback response-callback error-callback
           multi-output)
  "Replace one llm.el network invocation with STATE's scripted response."
  (journey-fixture--record-request state mode provider prompt)
  (let* ((script (journey-fixture-state-script state))
         (step (or (car script) (list :text "[journey script exhausted]")))
         (delay (or (plist-get step :delay) 0.01))
         (transport (generate-new-buffer " *superchat-journey-transport*")))
    (setf (journey-fixture-state-script state) (cdr script))
    (push transport (journey-fixture-state-transport-buffers state))
    (let ((timer
           (run-at-time
            delay nil #'journey-fixture--deliver-round
            state provider prompt partial-callback response-callback error-callback
            multi-output step)))
      (push timer (journey-fixture-state-timers state)))
    ;; A buffer is an actual llm.el cancellation handle, so the original
    ;; `llm-cancel-request' generic (observed below) performs the abort.
    transport))

(defun journey-fixture--cleanup (state)
  "Tear down all state owned by a journey fixture STATE."
  (dolist (timer (journey-fixture-state-timers state))
    (when (timerp timer) (ignore-errors (cancel-timer timer))))
  (let (run-ids)
    (maphash (lambda (run-id _entry) (push run-id run-ids))
             superchat-runtime--runs)
    (dolist (run-id run-ids)
      (ignore-errors (superchat-runtime-cancel run-id "[journey fixture cleanup]"))))
  (dolist (buffer (journey-fixture-state-transport-buffers state))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (ignore-errors (kill-buffer buffer)))))
  ;; Product code can make a Session View, a browser, an ediff control panel,
  ;; and temporary adapter buffers.  They all belong to this test if they did
  ;; not predate its fixture.
  (dolist (buffer (buffer-list))
    (unless (memq buffer (journey-fixture-state-baseline-buffers state))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (ignore-errors (kill-buffer buffer))))))
  (ignore-errors (superchat-db-close))
  (ignore-errors (delete-directory (journey-fixture-state-data-directory state) t))
  (ignore-errors (delete-directory (journey-fixture-state-home-directory state) t)))

(defmacro journey-fixture-with (options &rest body)
  "Run BODY against a temporary real Superchat chat-and-provider fixture.

OPTIONS is a plist.  `:model' supplies a real OpenAI-compatible provider's
model name, `:script' supplies initial scripted network rounds, and
`:tools-policy' controls the public tool attachment policy."
  (declare (indent 1) (debug t))
  (let ((model (or (plist-get options :model) "journey-known-model"))
        (script (or (plist-get options :script) nil))
        (tools-policy (if (plist-member options :tools-policy)
                          (plist-get options :tools-policy)
                        nil)))
    `(let ((journey-fixture--context-files-baseline
            (default-value 'superchat--current-context-files)))
       (let* ((home (make-temp-file "superchat-journey-home-" t))
            (data (file-name-as-directory
                   (make-temp-file "superchat-journey-data-" t)))
            (process-environment (copy-sequence process-environment))
            (user-emacs-directory (file-name-as-directory home))
            (default-directory data)
            (superchat-data-directory data)
            (superchat-skills-directory (expand-file-name "skills/" data))
            (superchat-skills-agents-md-file (expand-file-name "AGENTS.md" data))
            (superchat-db--connection nil)
            (superchat-db--path nil)
            (superchat-editor-workspace--states (make-hash-table :test #'equal))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-runtime-default-deadline nil)
            ;; Dynamically shadow the producer accumulator Dired mutates
            ;; outside any Session View, so a leaked push cannot survive past
            ;; this fixture into a later test's default value.
            (superchat--current-context-files nil)
            ;; Match the production default.  D2 additionally proves that a
            ;; plan.progress event remains compatible with this terminal seal.
            (superchat-closure-terminal-guard-enabled t)
            (superchat--llm-input-batches (make-hash-table :test #'eq :weakness 'key))
            (superchat--llm-raw-tool-uses (make-hash-table :test #'eq :weakness 'key))
            (superchat-authority-effect-resolvers
             (copy-hash-table superchat-authority-effect-resolvers))
            (superchat-tools--external-registry
             (copy-tree superchat-tools--external-registry))
            (superchat-llm-tools-list nil)
            (superchat--llm-tool-names-signature nil)
            (superchat--subagent-registry-signature nil)
            (superchat--subagent-registry-tool-cache nil)
            (superchat--model-list-cache nil)
            (superchat-show-ttft nil)
            (superchat-response-timeout nil)
            (superchat-llm-model nil)
            (superchat-llm-tools-enabled ,tools-policy)
            ;; A safe default avoids a batch minibuffer.  Tests that exercise
            ;; approval override this through the public customization point.
            (superchat-authority-approval-function (lambda (_request) t))
            (superchat-llm-backend
             (make-llm-openai-compatible
              :url "http://127.0.0.1:9/v1" :chat-model ,model))
            (superchat-buffer-name
             (format " *superchat-journey-%06x*" (random #x1000000)))
            (state
             (journey-fixture--state-create
              :script (copy-sequence ,script)
              :provider superchat-llm-backend
              :baseline-buffers (copy-sequence (buffer-list))
              :data-directory data
              :home-directory home))
            (journey-fixture--state state)
            (cancel-observer
             (lambda (original request)
               (push request (journey-fixture-state-cancellations state))
               (funcall original request))))
       (setenv "HOME" home)
       (superchat-db-open)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (provider prompt partial-callback response-callback
                           error-callback &optional multi-output)
                    (journey-fixture--fake-network
                     state 'streaming provider prompt partial-callback
                     response-callback error-callback multi-output)))
                 ((symbol-function 'llm-chat-async)
                  (lambda (provider prompt response-callback error-callback
                           &optional multi-output)
                    (journey-fixture--fake-network
                     state 'async provider prompt nil response-callback
                     error-callback multi-output))))
         ;; This is a passthrough observer of the real cancellation generic,
         ;; not a cancellation replacement.
         (advice-add 'llm-cancel-request :around cancel-observer)
         (let (journey-fixture--body-returned journey-fixture--body-result)
           (unwind-protect
               (prog1
                   (setq journey-fixture--body-result (progn ,@body))
                 (setq journey-fixture--body-returned t))
             (advice-remove 'llm-cancel-request cancel-observer)
             (journey-fixture--cleanup state)
             ;; The shadowing binding above guarantees this, but assert it so a
             ;; future fixture edit that drops the binding fails loudly instead
             ;; of leaking Dired's mutation into later tests' default value.
             ;; Living in this `unwind-protect' cleanup form makes the check
             ;; run after every BODY exit -- normal return, a signaled
             ;; `error'/`quit', or a `throw' to an outer `catch' -- since
             ;; `unwind-protect' always runs its cleanup before a nonlocal
             ;; exit continues past it.  When BODY did not return normally,
             ;; the check runs under `ignore-errors' so its own failure
             ;; cannot replace BODY's pending condition or transfer of
             ;; control.
             (if journey-fixture--body-returned
                 (should (equal journey-fixture--context-files-baseline
                                (default-value
                                 'superchat--current-context-files)))
               (ignore-errors
                 (should (equal journey-fixture--context-files-baseline
                                (default-value
                                 'superchat--current-context-files))))))
           journey-fixture--body-result))))))

(defun journey-fixture-load-does-not-write-home-p ()
  "Return non-nil iff a fresh `require' of Superchat leaves a temp HOME clean.

The child Emacs is batch-only and exits before this function returns.  It is
used for C1 because the canonical runner has necessarily already loaded the
package before it can load acceptance tests.  Rather than checking one known
path, the child snapshots every regular file and directory under HOME (path
and size) both before and after `require'; any appearance, disappearance, or
size change anywhere in that tree counts as a write.  No Emacs bootstrap
artifact is currently exempted: `native-comp-jit-compilation' is disabled in
the child so async native-compilation cannot introduce one of its own."
  (let* ((home (make-temp-file "superchat-journey-load-home-" t))
         (root (file-name-directory (locate-library "superchat")))
         (program (expand-file-name invocation-name invocation-directory))
         (process-environment (copy-sequence process-environment))
         status before after)
    (unwind-protect
        (progn
          (setenv "HOME" home)
          (with-temp-buffer
            (setq status
                  (call-process
                   program nil (list t nil) nil
                   "-Q" "--batch"
                   "--eval"
                   "(setq native-comp-enable-subr-trampolines nil native-comp-jit-compilation nil load-prefer-newer t)"
                   "-L" root
                   "--eval"
                   (format
                    "(progn
                       (defun sc-journey--snap ()
                         (sort
                          (mapcar
                           (lambda (f)
                             (cons (file-relative-name f %S)
                                   (file-attribute-size (file-attributes f))))
                           (directory-files-recursively %S \".\" t))
                          (lambda (a b) (string< (car a) (car b)))))
                       (prin1 (sc-journey--snap)) (princ \"\\n\")
                       (require 'superchat)
                       (prin1 (sc-journey--snap)) (princ \"\\n\")
                       (kill-emacs 0))"
                    home home)))
            (goto-char (point-min))
            (setq before (condition-case nil (read (current-buffer))
                           (error :journey-snapshot-missing)))
            (setq after (condition-case nil (read (current-buffer))
                          (error :journey-snapshot-missing))))
          (and (zerop status)
               (not (eq before :journey-snapshot-missing))
               (not (eq after :journey-snapshot-missing))
               (equal before after)))
      (ignore-errors (delete-directory home t)))))

(provide 'journey-fixture)
;;; journey-fixture.el ends here
