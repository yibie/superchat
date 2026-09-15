;;; m3-regression.el --- M3 execution main line regression set -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; OWNERSHIP -- read before editing.  The user ruled on 2026-09-14 that this
;; regression set belongs to the leader agent alone.  Only the leader agent
;; the user designated for it may change this file, the breakage scripts in
;; `.scratch/m3-verification/regression-mutants/', or the set's record
;; `.scratch/m3-verification/REGRESSION-SET.md'.  Every other agent or session,
;; including implementers fixing a defect a test found, may run and read the
;; set but must not edit it: report the test you believe is wrong, or the
;; known-failure marker your fix should flip, to the leader with evidence.
;; The leader checks this file's hash at each review; an edit it did not make
;; is reported to the user as a violation.
;;
;; One command, one exit status, for the contracts M3 adds or moves.  The
;; authority is `docs/decisions/accepted/2026-09-14-m3-execution-main-line-resolution.md'
;; and the M3 entry of `docs/roadmap.md'.  The canonical smoke set
;; (`scripts/smoke.el') stays the byte-for-byte event-sequence gate; this file
;; asserts behaviour that a fixed sequence cannot express: who may claim what,
;; what a refusal leaves behind, which Step a fact belongs to, and what a
;; failure is recorded as.
;;
;; It replaced the ad-hoc probes under `.scratch/m3-verification/'.  Tests are
;; grouped by contract area rather than by the round that discovered them:
;;
;;   m3/admission/     submission before Run acceptance (FIFO, busy, retired)
;;   m3/parse/         chat input parsed by its surface before the inbox
;;   m3/inject/        identified material: validation, Scope, drops, delivery
;;   m3/steer/         steering: refusal, ordering with inject, no migration
;;   m3/pre-step/      the pre-step waterfall and source relationships
;;   m3/request/       the request waterfall and what it may not rewrite
;;   m3/step/          step.started / step.ended identity, placement, disposition
;;   m3/turn-stopping/ the natural-closing notification and its recheck
;;   m3/run-sealing/   the legacy terminal observer boundary
;;   m3/driver/        the opaque handle and the transport seam
;;   m3/context/       collect-context as an Event
;;   m3/real/          live provider evidence (skipped unless SMOKE_REAL=1)
;;
;; Run from the repository root (the load path is the smoke one; see
;; docs/agents/development.md):
;;
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$ELPA\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L "$ELPA/llm-0.32.1" -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/m3-regression.el -f ert-run-tests-batch-and-exit
;;
;; Select an area with a regexp instead of `-f':
;;
;;   --eval '(ert-run-tests-batch-and-exit "^m3/pre-step/")'
;;
;; Every test runs in its own scripted fixture: fresh HOME, database, Run
;; registry, inbox, follow-up FIFO and fault ring.  Listeners and Scopes a test
;; creates are removed in `unwind-protect'.  No test depends on another.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Scope objects are cyclic (parent <-> children).  An ERT failure report that
;; prints one without `print-circle' dies with "Apparently circular structure".
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

;;;; Fixture ------------------------------------------------------------------

(defconst m3r-session "m3r"
  "Session id used by tests that submit through the driver directly.")

(defmacro m3r-fixture (script &rest body)
  "Run BODY in a fresh scripted smoke fixture that answers with SCRIPT.
Adds the process-local state `smoke--with-fixture' does not reset: the
follow-up FIFO, its dispatch timers and the fault ring."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil))
     (smoke--with-fixture ,script ,@body)))

(defmacro m3r-with-scope (var name &rest body)
  "Bind VAR to a fresh child Scope of global named NAME around BODY."
  (declare (indent 2) (debug t))
  `(let ((,var (superchat-scope-create ,name superchat-scope-global)))
     (unwind-protect (progn ,@body)
       (ignore-errors (superchat-scope-dispose ,var)))))

(defmacro m3r-listening (registrations &rest body)
  "Register REGISTRATIONS, run BODY, then remove them.
Each registration is (SCOPE EVENT ID FUNCTION), the arguments of `superchat-on'."
  (declare (indent 1) (debug t))
  (let ((undo (make-symbol "undo")))
    `(let ((,undo nil))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (registration)
                         `(push (superchat-on ,@registration) ,undo))
                       registrations)
             ,@body)
         (mapc #'funcall ,undo)))))

;;;; Driving ------------------------------------------------------------------

(cl-defun m3r-submit (text &key (session m3r-session) scope (tools 'none))
  "Submit TEXT through the driver Service; return the admission plist.
SCOPE, when non-nil, is the borrowed execution Scope of the submission."
  (let ((superchat-execution-scope (or scope superchat-execution-scope)))
    (superchat-service-call
     nil 'driver 'submit
     (list :surface 'external :session-id session :tools tools
           :attendance 'attended :content text
           :context (list :host-buffer (get-buffer-create " *m3r-host*"))
           :runtime-extras '(:record-conversation nil)
           :sink (superchat-sink-discard)))))

(defun m3r-view-submit (view text &optional tools)
  "Submit TEXT through the driver hosted by chat VIEW; return the settled run id.
Tool tests need a real chat host buffer, and its Session."
  (m3r-settle
   (plist-get (superchat-service-call
               nil 'driver 'submit
               (append (list :surface 'external
                             :session-id (buffer-local-value 'superchat--session-id view)
                             :content text
                             :context (list :host-buffer view)
                             :runtime-extras '(:record-conversation nil)
                             :sink (superchat-sink-discard))
                       (and tools (list :tools tools))))
              :run-id)))

(defun m3r-write-skill (name body)
  "Write a prompt-type skill NAME whose body is BODY into the fixture."
  (make-directory superchat-skills-directory t)
  (with-temp-file (expand-file-name (concat name ".md") superchat-skills-directory)
    (insert (format "---\nname: %s\ndescription: m3r\ntype: prompt\n---\n%s\n" name body))))

(defun m3r-prompt-area (view)
  "The editable draft text of chat VIEW."
  (with-current-buffer view
    (buffer-substring-no-properties superchat--prompt-start (point-max))))

(defmacro m3r-refusing-queue (&rest body)
  "Run BODY while Runtime refuses every follow-up enqueue as `tape-failed'."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'superchat-runtime-enqueue-follow-up)
              (lambda (&rest _) (list :accepted nil :reason 'tape-failed))))
     ,@body))

(defun m3r-context-error-event ()
  "The Event a required context contributor's failure is reported on.
The M3 closing rulings name it `context-error'; until that rename lands it is
`stage-error'.  Behaviour tests use this so the rename needs no test edit; the
vocabulary test pins the final name."
  (if (assq 'context-error superchat-event--definitions) 'context-error 'stage-error))

(defmacro m3r-losing-receipt-once (type &rest body)
  "Run BODY while the first Tape append of TYPE commits but reports failure.
The row is written; the caller sees an error, as when SQLite commits an INSERT
and the receipt is then lost."
  (declare (indent 1) (debug t))
  (let ((original (make-symbol "original")) (armed (make-symbol "armed")))
    `(let ((,original (symbol-function 'superchat-tape-append-event-record))
           (,armed t))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event &rest rest)
                    (let ((result (apply ,original event rest)))
                      (when (and ,armed (equal ,type (plist-get event :type)))
                        (setq ,armed nil)
                        (error "m3r: append committed, receipt lost"))
                      result))))
         ,@body))))

(defun m3r-drops-for (session queue-id)
  (cl-count queue-id (m3r-drops session)
            :key (lambda (payload) (plist-get payload :queue-id)) :test #'equal))

(defun m3r-status (run-id)
  (plist-get (superchat-runtime-status run-id) :status))

(defun m3r-terminal-p (run-id)
  (and (smoke--run-done-p run-id)
       (superchat-runtime--terminal-p (m3r-status run-id))))

(defun m3r-settle (run-id &optional seconds)
  "Wait until RUN-ID is terminal and its Turn seal is on the Tape."
  (should (smoke--wait (lambda () (m3r-terminal-p run-id)) (or seconds 30)))
  ;; Status turns terminal before the seal finishes appending.
  (smoke--wait (lambda ()
                 (let ((session (plist-get (superchat-runtime-status run-id)
                                           :session-id)))
                   (m3r-of-type "turn.completed" (m3r-events session run-id))))
               3)
  run-id)

(cl-defun m3r-run (text &rest args)
  "Submit TEXT (with ARGS as for `m3r-submit'), wait, return the run id."
  (let ((id (plist-get (apply #'m3r-submit text args) :run-id)))
    (should (stringp id))
    (m3r-settle id)))

(defun m3r-run-id-of (session)
  "Return the newest run id SESSION started on the Tape."
  (plist-get (car (last (m3r-of-type "run.started" (m3r-events session))))
             :run-id))

(cl-defun m3r-run-tolerating-errors (text &rest args)
  "Submit TEXT like `m3r-run', but tolerate a submission that signals.
Returns (RUN-ID . CONDITION); RUN-ID is read from the Tape when submit signals
after accepting the Run."
  (let* ((condition nil)
         (admission (condition-case err (apply #'m3r-submit text args)
                      (error (setq condition err) nil)))
         (session (or (plist-get args :session) m3r-session))
         (id (or (plist-get admission :run-id) (m3r-run-id-of session))))
    (when id (m3r-settle id))
    (cons id condition)))

(cl-defun m3r-live-run (text &rest args)
  "Submit TEXT against a hanging script; return the run id once Step 1 dispatched."
  (let* ((session (or (plist-get args :session) m3r-session))
         (id (plist-get (apply #'m3r-submit text args) :run-id)))
    (should (stringp id))
    (should (smoke--wait
             (lambda ()
               (and (member id (superchat-runtime-active-runs))
                    smoke--requests
                    (m3r-of-type "step.started" (m3r-events session id))))
             30))
    id))

(defun m3r-wait-turns (session count &optional seconds)
  "Wait until SESSION has COUNT `turn.completed' rows."
  (should (smoke--wait
           (lambda () (>= (m3r-count "turn.completed" (m3r-events session)) count))
           (or seconds 30))))

;;;; Reading the Tape -----------------------------------------------------------

(defun m3r-events (session &optional run-id)
  "SESSION's Tape rows, restricted to RUN-ID when non-nil."
  (let ((events (and session (superchat-tape-replay-events session))))
    (if run-id
        (cl-remove-if-not (lambda (e) (equal run-id (plist-get e :run-id))) events)
      events)))

(defun m3r-type (event) (format "%s" (plist-get event :type)))
(defun m3r-types (events) (mapcar #'m3r-type events))
(defun m3r-count (type events) (cl-count type (m3r-types events) :test #'equal))
(defun m3r-index (type events &optional from-end)
  (cl-position type (m3r-types events) :test #'equal :from-end from-end))
(defun m3r-of-type (type events)
  (cl-remove-if-not (lambda (e) (equal type (m3r-type e))) events))
(defun m3r-payload (event key) (plist-get (plist-get event :payload) key))

(defun m3r-drops (session)
  "Payloads of SESSION's `queue.dropped' rows, oldest first."
  (mapcar (lambda (e) (plist-get e :payload))
          (m3r-of-type "queue.dropped" (m3r-events session))))

(defun m3r-drop-reason (session queue-id)
  (plist-get (cl-find queue-id (m3r-drops session)
                      :key (lambda (p) (plist-get p :queue-id)) :test #'equal)
             :reason))

(defun m3r-attachments (session &optional run-id)
  (m3r-of-type "attachment.added" (m3r-events session run-id)))

(defun m3r-message-lists (session run-id)
  "RUN-ID's `request.message-list' rows.
They are Session series records: the Run is named inside `:execution', not on
the row, so `m3r-events' with a run id does not return them."
  (cl-remove-if-not
   (lambda (e) (equal run-id (plist-get (m3r-payload e :execution) :run-id)))
   (m3r-of-type "request.message-list" (m3r-events session))))

(defun m3r-prompts ()
  "Provider prompt texts of this fixture, oldest first."
  (mapcar (lambda (r) (or (plist-get r :prompt-text) "")) (reverse smoke--requests)))

(defun m3r-prompt-has (marker &optional prompt)
  (if prompt
      (string-match-p (regexp-quote marker) prompt)
    (cl-some (lambda (p) (string-match-p (regexp-quote marker) p)) (m3r-prompts))))

(defun m3r-material (text &optional tag)
  (list :source (list :kind 'm3r :tag (or tag text)) :text text))

(defun m3r-pending (&optional session)
  (superchat-runtime-pending-injections (or session m3r-session)))

(defun m3r-step-dispositions (session run-id)
  (mapcar (lambda (e) (m3r-payload e :disposition))
          (m3r-of-type "step.ended" (m3r-events session run-id))))

;;;; Admission -----------------------------------------------------------------

(ert-deftest m3/admission/idle-submit-runs-without-queue-facts ()
  "An immediate idle submission gets no synthetic queue events."
  (m3r-fixture '((:text "answer"))
    (let* ((run (m3r-run "idle"))
           (events (m3r-events m3r-session)))
      (should (eq 'completed (m3r-status run)))
      (should (= 1 (m3r-count "run.started" events)))
      (should-not (m3r-of-type "queue.queued" events))
      (should-not (m3r-of-type "queue.dispatched" events)))))

(ert-deftest m3/admission/busy-submit-queues-then-dispatches-one-new-turn ()
  "Sending during a Run queues; the item becomes the next Turn, not a Step."
  (m3r-fixture '((:text "first" :delay 0.4) (:text "second"))
    (let* ((first (m3r-live-run "one"))
           (admission (m3r-submit "M3R_FOLLOWUP")))
      (should (eq 'queue (plist-get admission :status)))
      (should (equal "session-busy" (plist-get admission :reason)))
      (should (stringp (plist-get admission :queue-id)))
      (should-not (plist-get admission :run-id))
      (m3r-wait-turns m3r-session 2)
      (let ((events (m3r-events m3r-session)))
        (should (= 2 (m3r-count "run.started" events)))
        (should (= 1 (m3r-count "step.started" (m3r-events m3r-session first))))
        (should (< (m3r-index "queue.queued" events)
                   (m3r-index "turn.completed" events)
                   (m3r-index "queue.dispatched" events)))
        (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))
        (should (m3r-prompt-has "M3R_FOLLOWUP" (cadr (m3r-prompts))))))))

(ert-deftest m3/admission/view-close-drops-queued-followup-with-fact ()
  (m3r-fixture '((:hang t))
    (m3r-live-run "hold")
    (let ((queue-id (plist-get (m3r-submit "queued") :queue-id)))
      (should (stringp queue-id))
      (superchat-runtime-clear-follow-ups m3r-session 'view-closed)
      (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))
      (should (equal "view-closed" (m3r-drop-reason m3r-session queue-id))))))

(ert-deftest m3/admission/a-follow-up-drop-whose-receipt-is-lost-is-recorded-once ()
  "KNOWN DEFECT (found 2026-09-15): the follow-up FIFO has the same gap as
injection drops: a committed `queue.dropped' whose receipt is lost leaves the
item queued, and the next settlement records the drop again.  Remove
`:expected-result' when fixed."
  :expected-result :failed
  (m3r-fixture '((:hang t))
    (m3r-live-run "hold")
    (let ((queue-id (plist-get (m3r-submit "queued") :queue-id)))
      (should (stringp queue-id))
      (m3r-losing-receipt-once "queue.dropped"
        (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed)))
      (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed))
      (should (= 1 (m3r-drops-for m3r-session queue-id)))
      (should (zerop (superchat-runtime-pending-follow-ups m3r-session))))))

(ert-deftest m3/admission/queued-input-is-not-a-run-until-dispatched ()
  "Enqueue is not Run acceptance: a waiting item has no Turn or Run facts."
  (m3r-fixture '((:hang t) (:text "second"))
    (let* ((first (m3r-live-run "one"))
           (queue-id (plist-get (m3r-submit "M3R_WAITING") :queue-id)))
      (should (stringp queue-id))
      (let ((events (m3r-events m3r-session)))
        (should (equal queue-id (m3r-payload (car (m3r-of-type "queue.queued" events)) :queue-id)))
        (should (= 1 (m3r-count "intent.received" events)))
        (should (= 1 (m3r-count "run.started" events))))
      (should (equal (list first) (superchat-runtime-active-runs)))
      (superchat-runtime-cancel first "[m3r cancel]")
      (m3r-wait-turns m3r-session 2)
      (let* ((events (m3r-events m3r-session))
             (second (m3r-run-id-of m3r-session))
             (dispatched (car (m3r-of-type "queue.dispatched" events))))
        (should-not (equal first second))
        ;; The dispatch fact names the Run the item became, so it follows
        ;; that Run's start; the queue fact precedes it.
        (should (equal queue-id (m3r-payload dispatched :queue-id)))
        (should (equal second (m3r-payload dispatched :run-id)))
        (should (< (m3r-index "queue.queued" events)
                   (m3r-index "run.started" events t)))))))

(ert-deftest m3/admission/fifo-order-holds-across-an-idle-gap ()
  "Between one Turn's end and the next dispatch the Session is idle but not
empty.  A submission then joins the tail instead of overtaking the waiting
item, and every queued item is queued and dispatched exactly once."
  (m3r-fixture '((:hang t) (:text "b") (:text "c"))
    (let ((first (m3r-live-run "M3R_A")))
      (should (eq 'queue (plist-get (m3r-submit "M3R_B") :status)))
      (superchat-runtime-cancel first "[m3r cancel]")
      (should-not (superchat-runtime-active-runs))
      (should (= 1 (superchat-runtime-pending-follow-ups m3r-session)))
      (should (eq 'queue (plist-get (m3r-submit "M3R_C") :status)))
      (m3r-wait-turns m3r-session 3)
      (let ((events (m3r-events m3r-session))
            (prompts (m3r-prompts)))
        (should (= 3 (length prompts)))
        (should (m3r-prompt-has "M3R_B" (nth 1 prompts)))
        (should (m3r-prompt-has "M3R_C" (nth 2 prompts)))
        (should (= 2 (m3r-count "queue.queued" events)))
        (should (= 2 (m3r-count "queue.dispatched" events)))
        (should (= 3 (m3r-count "run.started" events)))
        (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))))))

(ert-deftest m3/admission/child-run-is-not-forced-into-the-parent-session-fifo ()
  (m3r-fixture '((:hang t) (:text "child answer"))
    (let* ((parent (m3r-live-run "parent"))
           (admission (superchat-service-call
                       nil 'driver 'submit
                       (list :surface 'external :session-id m3r-session :tools 'none
                             :attendance 'attended :content "M3R_CHILD" :parent-run-id parent
                             :context (list :host-buffer (get-buffer-create " *m3r-host*"))
                             :runtime-extras '(:record-conversation nil)
                             :sink (superchat-sink-discard))))
           (child (plist-get admission :run-id)))
      (should (eq 'accepted (plist-get admission :status)))
      (should (stringp child))
      (m3r-settle child)
      (should (eq 'completed (m3r-status child)))
      (should (equal parent (plist-get (superchat-runtime-status child) :parent-run-id)))
      (should (m3r-prompt-has "M3R_CHILD" (cadr (m3r-prompts))))
      (should-not (m3r-of-type "queue.queued" (m3r-events m3r-session)))
      (should (member parent (superchat-runtime-active-runs))))))

(ert-deftest m3/admission/explicit-session-is-kept-and-a-missing-one-is-ephemeral ()
  (m3r-fixture '((:text "a") (:text "b"))
    (let ((explicit (m3r-run "explicit" :session "m3r-explicit")))
      (should (equal "m3r-explicit" (plist-get (superchat-runtime-status explicit) :session-id))))
    (let* ((admission (superchat-service-call
                       nil 'driver 'submit
                       (list :surface 'external :tools 'none :attendance 'attended
                             :content "ephemeral"
                             :context (list :host-buffer (get-buffer-create " *m3r-host*"))
                             :runtime-extras '(:record-conversation nil)
                             :sink (superchat-sink-discard))))
           (run (plist-get admission :run-id)))
      (m3r-settle run)
      (should (stringp (plist-get admission :request-id)))
      (should (equal (concat "ephemeral-" (plist-get admission :request-id))
                     (plist-get (superchat-runtime-status run) :session-id))))))

(ert-deftest m3/admission/invalid-envelopes-are-refused-before-queueing ()
  "Validation precedes admission: a busy Session queues nothing invalid."
  (m3r-fixture '((:hang t))
    (m3r-live-run "hold")
    (let ((host (get-buffer-create " *m3r-host*")))
      (dolist (envelope
               (list (list :surface 'external :session-id m3r-session :content ""
                           :sink (superchat-sink-discard))
                     (list :surface 'm3r-unregistered :session-id m3r-session :content "x"
                           :sink (superchat-sink-discard))
                     (list :surface 'external :session-id m3r-session :content "x")
                     (list :surface 'external :session-id m3r-session :content "x"
                           :buffer-name "legacy" :sink (superchat-sink-discard))
                     (list :surface 'external :session-id m3r-session :content "x"
                           :context (list :host-buffer host)
                           :sink (superchat-sink-create :live-p #'ignore))))
        (ert-info ((format "envelope keys: %S"
                           (cl-loop for (key _) on envelope by #'cddr collect key)))
          (should (eq 'superchat-envelope-invalid
                      (car (should-error
                            (superchat-service-call nil 'driver 'submit envelope))))))))
    (should-not (m3r-of-type "queue.queued" (m3r-events m3r-session)))
    (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))))

(ert-deftest m3/admission/retired-scope-submission-is-refused-before-acceptance ()
  (m3r-fixture '((:text "must not be used"))
    (m3r-with-scope scope "m3r-retired"
      (superchat-execution-scope-retire scope)
      (should-error (m3r-submit "retired" :scope scope))
      (should-not smoke--requests)
      (should-not (m3r-of-type "run.started" (m3r-events m3r-session))))))

;;;; Parse ---------------------------------------------------------------------

(ert-deftest m3/parse/chat-input-sent-during-a-run-is-queued-parsed ()
  "The chat surface parses before the inbox, so the queued Turn keeps its text."
  (m3r-fixture '((:text "first" :delay 0.4) (:text "second"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (smoke--send view "M3R_CHAT_ONE")
      (should (smoke--wait (lambda () smoke--requests) 10))
      (smoke--send view "M3R_CHAT_TWO")
      (m3r-wait-turns session 2)
      (let ((events (m3r-events session))
            (prompts (m3r-prompts)))
        (should (= 2 (m3r-count "run.started" events)))
        (should (m3r-of-type "queue.queued" events))
        (should (m3r-of-type "queue.dispatched" events))
        (should (= 2 (length prompts)))
        (should-not (m3r-prompt-has "M3R_CHAT_TWO" (car prompts)))
        (should (m3r-prompt-has "M3R_CHAT_TWO" (cadr prompts)))))))

(ert-deftest m3/parse/chat-command-and-skill-during-a-run-are-parsed-before-queueing ()
  "Commands and skills bypass the chat surface's own plain-input queue, reach
Runtime admission, and wait there already parsed: a template changed while the
item waits does not change what is sent."
  (m3r-fixture '((:text "first" :delay 0.4) (:text "template") (:text "skill"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (puthash "m3rtpl" "M3R_TPL_OLD: $input" superchat--user-commands)
      (m3r-write-skill "m3rskill" "M3R_SKILL_BODY")
      (unwind-protect
          (progn
            (smoke--send view "first")
            (should (smoke--wait (lambda () smoke--requests) 10))
            (smoke--send view "/m3rtpl fill")
            (smoke--send view ">m3rskill hello")
            (puthash "m3rtpl" "M3R_TPL_NEW: $input" superchat--user-commands)
            (m3r-wait-turns session 3)
            (let ((prompts (m3r-prompts))
                  (events (m3r-events session)))
              (should (= 3 (length prompts)))
              (should (= 2 (m3r-count "queue.queued" events)))
              (should (= 2 (m3r-count "queue.dispatched" events)))
              (should (m3r-prompt-has "M3R_TPL_OLD: fill" (nth 1 prompts)))
              (should-not (m3r-prompt-has "M3R_TPL_NEW" (nth 1 prompts)))
              (should (m3r-prompt-has "M3R_SKILL_BODY" (nth 2 prompts)))))
        (remhash "m3rtpl" superchat--user-commands)))))

(ert-deftest m3/parse/non-chat-surfaces-are-not-chat-parsed ()
  (m3r-fixture '((:text "answer"))
    (puthash "m3rtpl" "M3R_TPL: $input" superchat--user-commands)
    (unwind-protect
        (let ((run (progn
                     ;; The template must exist, or "not expanded" is vacuous.
                     (should (gethash "m3rtpl" superchat--user-commands))
                     (m3r-run "/m3rtpl M3R_LITERAL"))))
          (should (eq 'completed (m3r-status run)))
          (should (m3r-prompt-has "/m3rtpl M3R_LITERAL"))
          (should-not (m3r-prompt-has "M3R_TPL:")))
      (remhash "m3rtpl" superchat--user-commands))))

(ert-deftest m3/parse/unknown-chat-command-starts-no-run ()
  "Characterization: an unknown slash command is not sent to the model.
Changing that is a product decision, not a side effect of moving parsing."
  (m3r-fixture '((:text "must not be used"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (smoke--send view "/m3r-no-such-command arg")
      (smoke--wait (lambda () smoke--requests) 0.3)
      (should-not smoke--requests)
      (should-not (m3r-of-type "run.started" (m3r-events session))))))

(ert-deftest m3/parse/turn-facts-are-recorded-once-per-turn-not-per-step ()
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*")))) (:text "second"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m3r-settle (smoke--send view "two steps")))
           (events (m3r-events session)))
      (should (eq 'completed (m3r-status run)))
      (should (= 2 (m3r-count "step.started" events)))
      (should (= 1 (m3r-count "intent.received" events)))
      (should (= 1 (m3r-count "run.started" events))))))

;; A refused enqueue must leave the chat View exactly as a draft the user has not
;; sent yet.  Plain input already does; the helper states the whole outcome once
;; so the plain test proves the criteria are reachable and the command/skill
;; test holds that path to the same criteria.
(defconst m3r-refused-queue-notice
  "Could not queue the message; it is still in the prompt."
  "The notice the chat View shows when Runtime refuses to queue a draft.")

(defconst m3r-busy-script
  '((:chunks ("M3R_BUSY_PART") :text "M3R_BUSY_DONE" :delay 1.5) (:text "resent"))
  "A busy first Run that streams and finishes by itself, then the resend answer.
The delay is the window in which the refused send happens; the send itself
takes milliseconds.")

(defun m3r-view-mode-state (view)
  "The parts of VIEW's state a draft that was never sent must not change."
  (with-current-buffer view
    (list :capability (bound-and-true-p superchat--active-capability)
          :preset superchat--active-preset
          :preset-scope superchat--active-preset-scope
          :command (bound-and-true-p superchat--current-command))))

(defun m3r-occurrences (needle haystack)
  (let ((count 0) (start 0))
    (while (string-match (regexp-quote needle) haystack start)
      (setq count (1+ count) start (match-end 0)))
    count))

(defun m3r-refused-send-outcome (draft marker &optional expected-in-resend setup)
  "Send DRAFT to a busy chat View while Runtime refuses to queue, then observe.
MARKER is a substring unique to DRAFT.  EXPECTED-IN-RESEND is what the provider
prompt must contain once the draft is sent again after the busy Run ends.
SETUP, when non-nil, runs after the View opens and before anything is sent:
opening a View reloads user commands from disk, so a template registered
earlier is gone by the time it is typed.  Run inside a fixture answering with
`m3r-busy-script'.  Signals an ERT failure naming the first criterion that does
not hold."
  (let* ((view (smoke--open-chat))
         (_ (when setup (funcall setup)))
         (first (smoke--send view "first"))
         (messages nil))
    (should (smoke--wait (lambda () smoke--requests) 10))
    (should (member first (superchat-runtime-active-runs)))
    (let ((transcript-before
           (with-current-buffer view
             (buffer-substring-no-properties (point-min) superchat--prompt-start)))
          (tracked-before (buffer-local-value 'superchat--active-run-id view))
          (mode-before (m3r-view-mode-state view)))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (push (apply #'format format-string args) messages)))))
        (m3r-refusing-queue (smoke--send view draft)))
      (with-current-buffer view
        (ert-info ((format "draft: %s, right after the refusal" draft))
          ;; 1. the draft is back in the prompt, editable
          (should (equal draft (m3r-prompt-area view)))
          (should-not (get-text-property superchat--prompt-start 'read-only))
          ;; 2. and it is not also locked into the transcript as sent
          (should (= 1 (m3r-occurrences marker (buffer-string))))
          ;; 3. the transcript above the prompt is unchanged
          (should (equal transcript-before
                         (buffer-substring-no-properties (point-min) superchat--prompt-start)))
          ;; 4. the user is told, with the same notice plain input shows
          (should (member m3r-refused-queue-notice messages))
          ;; 5. the View still tracks the Run that is still busy
          (should (equal tracked-before superchat--active-run-id))
          (should (member first (superchat-runtime-active-runs)))
          ;; 6. nothing was queued or sent
          (should (= 1 (length smoke--requests)))
          (should (zerop (superchat-runtime-pending-follow-ups superchat--session-id)))
          ;; 7. the View's mode is what it was: no command mode, Preset or
          ;;    capability left behind by a draft that was never sent
          (should (equal mode-before (m3r-view-mode-state view))))))
    ;; 8. the busy Run keeps streaming and finishes above the prompt, without
    ;;    disturbing the restored draft
    (m3r-settle first)
    (with-current-buffer view
      (ert-info ((format "draft: %s, after the busy Run finished" draft))
        (should (eq 'completed (m3r-status first)))
        (should (equal draft (m3r-prompt-area view)))
        (should (= 1 (m3r-occurrences marker (buffer-string))))
        (should (string-match-p "M3R_BUSY_DONE"
                                (buffer-substring-no-properties
                                 (point-min) superchat--prompt-start)))))
    ;; 9. the restored draft can simply be sent now
    (with-current-buffer view (superchat-send-input))
    (should (smoke--wait (lambda () (= 2 (length smoke--requests))) 10))
    (should (m3r-prompt-has (or expected-in-resend marker) (cadr (m3r-prompts))))))

(ert-deftest m3/parse/rejected-queueing-keeps-a-plain-draft-editable ()
  (m3r-fixture m3r-busy-script
    (m3r-refused-send-outcome "M3R_DRAFT" "M3R_DRAFT")))

(ert-deftest m3/parse/rejected-queueing-keeps-a-command-or-skill-draft ()
  "A command or skill refused by Runtime while a Run is busy gives its draft back.
The request lifecycle requires that \"rejected queueing leaves the producer's
draft available\".  Commands and skills take the direct send path, which
consumes the draft before admission answers; the send records what it
consumed and settles once the dispatch returns.  Fixed 2026-09-14 (defect found
by this set the same day); the criteria are those plain input meets, see
`m3r-refused-send-outcome'."
  (dolist (case '(("/m3rtpl M3R_TPL_DRAFT" "M3R_TPL_DRAFT" "M3R_TPL: M3R_TPL_DRAFT")
                  (">m3rskill M3R_SKILL_DRAFT" "M3R_SKILL_DRAFT" "M3R_SKILL_BODY")))
    (m3r-fixture m3r-busy-script
      (unwind-protect
          (m3r-refused-send-outcome
           (nth 0 case) (nth 1 case) (nth 2 case)
           (lambda ()
             (puthash "m3rtpl" "M3R_TPL: $input" superchat--user-commands)
             (m3r-write-skill "m3rskill" "M3R_SKILL_BODY")
             ;; Preconditions: the draft must be a real template and a real
             ;; skill, or a failure here would be an unknown command posing as
             ;; the defect.
             (should (gethash "m3rtpl" superchat--user-commands))
             (should (superchat-skills--find-file "m3rskill"))))
        (remhash "m3rtpl" superchat--user-commands)))))

;;;; Inject --------------------------------------------------------------------

(ert-deftest m3/inject/invalid-material-is-refused-and-not-stored ()
  (m3r-fixture '((:text "ignore"))
    (dolist (material (list '(:text "no source")
                            '(:source (:kind m3r))
                            '(:source (:kind m3r) :text "")
                            '(:source (:kind m3r) :text 42)
                            'not-a-plist))
      (should (equal '(:accepted nil :reason invalid-material)
                     (superchat-runtime-inject m3r-session material))))
    (should (equal '(:accepted nil :reason invalid-material)
                   (superchat-runtime-inject "" (m3r-material "x"))))
    (should (zerop (m3r-pending)))
    (should-not (m3r-drops m3r-session))))

(ert-deftest m3/inject/idle-injection-never-wakes-the-driver ()
  (m3r-fixture '((:text "must not be used"))
    (should (stringp (superchat-runtime-inject m3r-session (m3r-material "idle"))))
    (should-not (superchat-runtime-active-runs))
    (should-not smoke--requests)
    (should (= 1 (m3r-pending)))))

(ert-deftest m3/inject/retired-scope-is-refused ()
  (m3r-fixture '((:text "ignore"))
    (m3r-with-scope scope "m3r-retired"
      (superchat-execution-scope-retire scope)
      (should (equal 'scope-retired
                     (plist-get (superchat-runtime-inject
                                 m3r-session (m3r-material "x") scope)
                                :reason)))
      (should (zerop (m3r-pending))))))

(ert-deftest m3/inject/scope-is-exact-identity-not-ancestry ()
  "Material belongs to the exact Scope that submitted it.  Global is a Scope,
not a wildcard: a child Run cannot claim it, and a global Run cannot claim a
child's material."
  (m3r-fixture '((:text "q answer") (:text "global answer"))
    (m3r-with-scope preset "m3r-preset"
      (m3r-with-scope other "m3r-other"
        (superchat-runtime-inject m3r-session (m3r-material "M3R_PRESET") preset)
        (superchat-runtime-inject m3r-session (m3r-material "M3R_GLOBAL"))
        (m3r-run "sibling" :scope other)
        (should-not (m3r-prompt-has "M3R_PRESET"))
        (should-not (m3r-prompt-has "M3R_GLOBAL"))
        (should (= 2 (m3r-pending)))
        (m3r-run "global")
        (should (m3r-prompt-has "M3R_GLOBAL" (cadr (m3r-prompts))))
        (should-not (m3r-prompt-has "M3R_PRESET"))
        (should (= 1 (m3r-pending)))
        (should-not (m3r-drops m3r-session))))))

(ert-deftest m3/inject/idle-material-reaches-the-next-matching-turn-once ()
  (m3r-fixture '((:text "answer"))
    (m3r-with-scope preset "m3r-preset"
      (let* ((id (superchat-runtime-inject
                  m3r-session (m3r-material "M3R_VISIBLE" "vis") preset))
             gate-saw
             (run (m3r-listening
                      ((nil 'pre-step 'm3r-see
                            (lambda (proposal next)
                              (setq gate-saw (plist-get proposal :injected))
                              (funcall next proposal))))
                    (m3r-run "visible" :scope preset)))
             (rows (m3r-attachments m3r-session)))
        (should (equal '("M3R_VISIBLE")
                       (mapcar (lambda (m) (plist-get m :text)) gate-saw)))
        (should (eq 'completed (m3r-status run)))
        (should (m3r-prompt-has "M3R_VISIBLE"))
        (should (= 1 (length rows)))
        (let ((row (car rows)))
          (should (equal id (m3r-payload row :inject-id)))
          ;; The payload round-trips through JSON: symbols read back as names.
          (should (equal "m3r" (plist-get (m3r-payload row :source) :kind)))
          (should (equal "vis" (plist-get (m3r-payload row :source) :tag)))
          (should (equal (plist-get (superchat-runtime-status run) :turn-id)
                         (plist-get row :turn-id))))
        (should (zerop (m3r-pending)))))))

(ert-deftest m3/inject/material-is-recorded-before-dispatch ()
  "The durable source record exists at the moment the provider is called."
  (m3r-fixture '((:text "answer"))
    (superchat-runtime-inject m3r-session (m3r-material "M3R_ORDER"))
    (let* ((recorded-at-call nil)
           (streaming (symbol-function 'llm-chat-streaming))
           (async (symbol-function 'llm-chat-async))
           (observe (lambda ()
                      (push (mapcar (lambda (e) (m3r-payload e :text))
                                    (m3r-attachments m3r-session))
                            recorded-at-call)))
           (run (cl-letf (((symbol-function 'llm-chat-streaming)
                           (lambda (&rest args) (funcall observe) (apply streaming args)))
                          ((symbol-function 'llm-chat-async)
                           (lambda (&rest args) (funcall observe) (apply async args))))
                  (m3r-run "order")))
           (events (m3r-events m3r-session)))
      (should (eq 'completed (m3r-status run)))
      (should (equal '(("M3R_ORDER")) recorded-at-call))
      (should (< (m3r-index "step.started" events)
                 (m3r-index "attachment.added" events)
                 (m3r-index "request.message-list" events))))))

(ert-deftest m3/inject/unconsumed-live-run-material-is-dropped-with-fact ()
  "Material targeted at a live Run that ends without consuming it is discarded
through the audited drop path, not moved to a later Turn."
  (m3r-fixture '((:hang t) (:text "later"))
    (let* ((run (m3r-live-run "hold"))
           (id (superchat-runtime-inject m3r-session (m3r-material "M3R_UNUSED"))))
      (should (stringp id))
      (superchat-runtime-cancel run "[m3r cancel]")
      (m3r-settle run)
      (should (equal "run ended without consuming the injection"
                     (m3r-drop-reason m3r-session id)))
      (should (zerop (m3r-pending)))
      (m3r-run "later")
      (should-not (m3r-prompt-has "M3R_UNUSED")))))

(ert-deftest m3/inject/retirement-and-view-close-drop-only-their-own-material ()
  (m3r-fixture '((:text "ignore"))
    (m3r-with-scope kept "m3r-kept"
      (m3r-with-scope retired "m3r-retiring"
        (let ((kept-id (superchat-runtime-inject m3r-session (m3r-material "k") kept))
              (retired-id (superchat-runtime-inject m3r-session (m3r-material "r") retired)))
          (should (= 2 (m3r-pending)))
          (superchat-runtime-retire-scope retired "m3r retirement")
          (should (= 1 (m3r-pending)))
          (should (equal "scope-retired" (m3r-drop-reason m3r-session retired-id)))
          (should-not (m3r-drop-reason m3r-session kept-id))
          (superchat-runtime-clear-follow-ups m3r-session 'view-closed)
          (should (zerop (m3r-pending)))
          (should (equal "view-closed" (m3r-drop-reason m3r-session kept-id))))))))

(ert-deftest m3/inject/failed-drop-fact-keeps-the-material ()
  "A drop is committed only when its fact lands; a retry can still settle it."
  (m3r-fixture '((:text "ignore"))
    (let ((id (superchat-runtime-inject m3r-session (m3r-material "held"))))
      (cl-letf (((symbol-function 'superchat-runtime--queue-event)
                 (lambda (&rest _) (error "m3r append fails"))))
        (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed)))
      (should (= 1 (m3r-pending)))
      (should-not (m3r-drops m3r-session))
      (superchat-runtime-clear-follow-ups m3r-session 'view-closed)
      (should (zerop (m3r-pending)))
      (should (equal "view-closed" (m3r-drop-reason m3r-session id))))))

;; The accepted M3 record: "Audit errors distinguish an uncommitted append from
;; a committed append with a lost receipt; existing stable event identities
;; prevent duplicate dispatch or facts."
(ert-deftest m3/inject/a-drop-whose-receipt-is-lost-is-recorded-once ()
  "KNOWN DEFECT (found 2026-09-15): a `queue.dropped' append that commits but
loses its receipt leaves the injection pending, and the next settlement appends
a second `queue.dropped' for the same item.  Both the view-close drop and the
drop at the end of the targeted Run are affected.  Remove `:expected-result'
when fixed."
  :expected-result :failed
  (m3r-fixture '((:text "ignore"))
    (let ((id (superchat-runtime-inject m3r-session (m3r-material "held"))))
      (m3r-losing-receipt-once "queue.dropped"
        (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed)))
      (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed))
      (ert-info ("view-close drop")
        (should (= 1 (m3r-drops-for m3r-session id)))
        (should (zerop (m3r-pending))))))
  (m3r-fixture '((:hang t))
    (let* ((run (m3r-live-run "hold"))
           (id (superchat-runtime-inject m3r-session (m3r-material "unused"))))
      (m3r-losing-receipt-once "queue.dropped"
        (superchat-runtime-cancel run "[m3r cancel]")
        (m3r-settle run))
      (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed))
      (ert-info ("drop at the end of the targeted Run")
        (should (= 1 (m3r-drops-for m3r-session id)))
        (should (zerop (m3r-pending)))))))

(ert-deftest m3/inject/admitted-material-reaches-the-next-turn-once ()
  "KNOWN GAP (ruled 2026-09-15): the M3 record says admitted material \"enters
history once\".  Within a Run a later Step may carry it in the provider prompt;
a later Turn's history, which is assembled from the Tape, must contain it
exactly once.  Today the next Turn's prompt does not contain it at all.  Remove
`:expected-result' when implemented."
  :expected-result :failed
  (m3r-fixture '((:text "first") (:text "second") (:text "third"))
    (m3r-with-scope preset "m3r-preset"
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view)))
        (superchat-runtime-inject session (m3r-material "M3R_NEXT_TURN_MATERIAL") preset)
        (let ((superchat-execution-scope preset))
          (m3r-settle (smoke--send view "turn one"))
          (m3r-settle (smoke--send view "turn two"))
          (m3r-settle (smoke--send view "turn three")))
        (let ((prompts (m3r-prompts)))
          (should (= 3 (length prompts)))
          (should (= 1 (m3r-occurrences "M3R_NEXT_TURN_MATERIAL" (nth 0 prompts))))
          ;; history, not a second admission
          (should (= 1 (length (m3r-attachments session))))
          (should (zerop (m3r-pending session)))
          (ert-info ("the next Turn")
            (should (= 1 (m3r-occurrences "M3R_NEXT_TURN_MATERIAL" (nth 1 prompts)))))
          (ert-info ("the Turn after that")
            (should (= 1 (m3r-occurrences "M3R_NEXT_TURN_MATERIAL" (nth 2 prompts))))))))))

(ert-deftest m3/inject/later-step-derives-material-recorded-once ()
  "Material enters history once; a later Step of the same Run still sees it."
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                 (:text "second"))
    (m3r-with-scope preset "m3r-preset"
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view)))
        (superchat-runtime-inject session (m3r-material "M3R_LATER") preset)
        (let* ((superchat-execution-scope preset)
               (run (smoke--send view "two steps")))
          (m3r-settle run)
          (let ((prompts (m3r-prompts)))
            (should (= 2 (length prompts)))
            (should (m3r-prompt-has "M3R_LATER" (car prompts)))
            (should (m3r-prompt-has "M3R_LATER" (cadr prompts)))
            (should (= 1 (length (m3r-attachments session))))
            (should (zerop (m3r-pending session)))))))))

;;;; Steer ---------------------------------------------------------------------

(ert-deftest m3/steer/refused-without-a-live-run ()
  "Steering never starts a Turn, and a finished Run records it undelivered."
  (m3r-fixture '((:text "answer"))
    (should-error (superchat-runtime-steer "m3r-any" "   "))
    (should-not (superchat-runtime-steer "m3r-no-such-run" "M3R_NOWHERE"))
    (should-not (superchat-runtime-active-runs))
    (should-not smoke--requests)
    (let ((run (m3r-run "finish first")))
      (should-not (superchat-runtime-steer run "M3R_TOO_LATE"))
      (should (m3r-of-type "run.steer-undelivered" (m3r-events m3r-session run)))
      (should (= 1 (length smoke--requests)))
      (should-not (superchat-runtime-active-runs)))))

(ert-deftest m3/steer/steer-and-inject-share-one-ordered-claim ()
  "Within a next-step batch steer and inject keep insertion order, are claimed
once, and are recorded in that order."
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (superchat-runtime-steer run "M3R_S1")
      (superchat-runtime-inject m3r-session (m3r-material "M3R_I1"))
      (superchat-runtime-steer run "M3R_S2")
      (superchat-runtime-inject m3r-session (m3r-material "M3R_I2"))
      (let ((material (superchat-service-call
                       nil 'driver 'advance-step run '(:prompt "seam") nil)))
        (should (equal '("M3R_S1" "M3R_I1" "M3R_S2" "M3R_I2")
                       (mapcar (lambda (m) (plist-get m :text)) material)))
        (should (equal "steer" (format "%s" (plist-get (plist-get (car material) :source)
                                                        :kind)))))
      (should (equal '("M3R_S1" "M3R_I1" "M3R_S2" "M3R_I2")
                     (mapcar (lambda (e) (m3r-payload e :text))
                             (m3r-attachments m3r-session run))))
      (should (zerop (superchat-runtime-pending-steer-count run)))
      (should (zerop (m3r-pending)))
      (should-not (superchat-runtime-drain-steers run))
      (should-not (superchat-service-call
                   nil 'driver 'advance-step run '(:prompt "seam") nil)))))

(ert-deftest m3/steer/unconsumed-steer-does-not-migrate-to-a-later-run ()
  (m3r-fixture '((:hang t) (:text "later"))
    (let ((run (m3r-live-run "hold")))
      (should (superchat-runtime-steer run "M3R_STEER_LOST"))
      (superchat-runtime-cancel run "[m3r cancel]")
      (m3r-settle run)
      (should (m3r-of-type "run.steer-undelivered" (m3r-events m3r-session run)))
      (m3r-run "later")
      (should-not (m3r-prompt-has "M3R_STEER_LOST")))))

;;;; Pre-step ------------------------------------------------------------------

(ert-deftest m3/pre-step/gate-sees-the-real-execution-identity ()
  (m3r-fixture '((:text "answer"))
    (let* (seen
           (run (m3r-listening
                    ((nil 'pre-step 'm3r-see
                          (lambda (proposal next)
                            (setq seen proposal)
                            (funcall next proposal))))
                  (m3r-run "identity")))
           (started (car (m3r-of-type "step.started" (m3r-events m3r-session run)))))
      (should (eq 'completed (m3r-status run)))
      (should (equal run (plist-get seen :run-id)))
      (should (equal m3r-session (plist-get seen :session-id)))
      (should (equal (plist-get (superchat-runtime-status run) :turn-id)
                     (plist-get seen :turn-id)))
      (should (eql 1 (plist-get seen :step)))
      (should (stringp (plist-get (plist-get seen :batch) :prompt)))
      (should (eql 1 (m3r-payload started :step))))))

(ert-deftest m3/pre-step/reject-refuse-and-throw-open-no-step ()
  "Every non-entry outcome opens no Step, sends nothing and fails the Run."
  (dolist (gate (list (cons 'reject (lambda (_p _n) (list :reject "m3r says no")))
                      (cons 'refuse (lambda (_p _n) (superchat-waterfall-refuse "m3r refuses")))
                      (cons 'throw (lambda (_p _n) (error "m3r gate explodes")))))
    (m3r-fixture '((:text "must not be used"))
      (let* ((run (m3r-listening ((nil 'pre-step 'm3r-gate (cdr gate)))
                    (m3r-run (format "%s" (car gate)))))
             (events (m3r-events m3r-session run)))
        (ert-info ((format "gate outcome: %s" (car gate)))
          (should (eq 'failed (m3r-status run)))
          (should-not (m3r-of-type "step.started" events))
          (should-not (m3r-of-type "step.ended" events))
          (should-not (m3r-of-type "run.completed" events))
          (should-not smoke--requests)
          (when (eq (car gate) 'throw)
            ;; The waterfall helper records `(event waterfall)' as the stage for
            ;; every waterfall, so the listener id is what identifies it.
            (should (cl-find 'm3r-gate superchat-event-faults
                             :key (lambda (f) (plist-get f :id))))))))))

(ert-deftest m3/pre-step/non-entry-discards-claimed-material-with-outcome ()
  "A rejected or thrown proposal is consumed with an explicit non-entry fact and
never reinserted into the inbox."
  (dolist (gate (list (lambda (_p _n) (list :reject "m3r says no"))
                      (lambda (_p _n) (error "m3r gate explodes"))))
    (m3r-fixture '((:text "must not be used"))
      (let* ((id (superchat-runtime-inject m3r-session (m3r-material "M3R_REJECTED")))
             (run (m3r-listening ((nil 'pre-step 'm3r-gate gate))
                    (m3r-run "discard"))))
        (should (eq 'failed (m3r-status run)))
        (should (equal "pre-step-not-admitted" (m3r-drop-reason m3r-session id)))
        (should (zerop (m3r-pending)))
        (should-not (m3r-attachments m3r-session))))))

(defmacro m3r-with-gate-result (injections fn &rest body)
  "Inject INJECTIONS (a list of texts), run one Turn whose pre-step gate returns
\(:enter (:injected (FUNCALL FN CLAIMED))), and run BODY with RUN and IDS bound."
  (declare (indent 2) (debug t))
  `(let* ((ids (mapcar (lambda (text)
                         (superchat-runtime-inject m3r-session (m3r-material text)))
                       ,injections))
          (run (m3r-listening
                   ((nil 'pre-step 'm3r-transform
                         (lambda (proposal _next)
                           (list :enter
                                 (list :injected
                                       (funcall ,fn (plist-get proposal :injected)))))))
                 (car (m3r-run-tolerating-errors "transform")))))
     (ignore ids run)
     ,@body))

(ert-deftest m3/pre-step/one-to-one-rewrite-keeps-identity-and-source ()
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_ORIGINAL")
        (lambda (claimed)
          (mapcar (lambda (m) (plist-put (copy-tree m) :text "M3R_REWRITTEN")) claimed))
      (let ((rows (m3r-attachments m3r-session run)))
        (should (eq 'completed (m3r-status run)))
        (should (m3r-prompt-has "M3R_REWRITTEN"))
        (should-not (m3r-prompt-has "M3R_ORIGINAL"))
        (should (= 1 (length rows)))
        (should (equal "M3R_REWRITTEN" (m3r-payload (car rows) :text)))
        (should (equal (car ids) (m3r-payload (car rows) :inject-id)))
        (should (equal "M3R_ORIGINAL" (plist-get (m3r-payload (car rows) :source) :tag)))
        (should-not (m3r-drops m3r-session))))))

(ert-deftest m3/pre-step/merge-records-the-actual-sources-of-its-inputs ()
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_A" "M3R_B")
        (lambda (claimed)
          (list (list :source '(:kind merger) :text "M3R_MERGED"
                      :input-ids (mapcar (lambda (m) (plist-get m :inject-id)) claimed))))
      (let* ((rows (m3r-attachments m3r-session run))
             (source (m3r-payload (car rows) :source)))
        (should (eq 'completed (m3r-status run)))
        (should (= 1 (length rows)))
        (should (equal ids (m3r-payload (car rows) :input-ids)))
        (should (equal "derived" (format "%s" (plist-get source :kind))))
        (should (m3r-prompt-has "M3R_MERGED"))
        (should-not (m3r-prompt-has "[Injected material] M3R_A"))
        (should-not (m3r-drops m3r-session))))))

(ert-deftest m3/pre-step/derived-source-survives-the-tape-round-trip ()
  "A derived source's `:inputs' survives the Tape's JSON round trip.
It is a list of plists, which `json-encode' renders as ONE object whose
colliding keys drop every entry but the last; the Tape's codec tags that shape
so both inputs come back with their identity and original source intact, as
the record requires admitted material to be durably recorded with its source
relationship."
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_A" "M3R_B")
        (lambda (claimed)
          (list (list :source '(:kind merger) :text "M3R_MERGED"
                      :input-ids (mapcar (lambda (m) (plist-get m :inject-id)) claimed))))
      (let ((inputs (plist-get (m3r-payload (car (m3r-attachments m3r-session run)) :source)
                               :inputs)))
        (should (equal ids (mapcar (lambda (i) (plist-get i :id)) inputs)))
        (should (equal '("M3R_A" "M3R_B")
                       (mapcar (lambda (i) (plist-get (plist-get i :source) :tag))
                               inputs)))))))

(ert-deftest m3/pre-step/tape-tag-is-an-object-and-a-lookalike-list-is-inert ()
  "The Tape's plist-list tag is an OBJECT, and a list that looks like one is not one.
The tag has to be a single-key object so that the tagged form cannot be
confused with a plain value.  An earlier encoding used a two-element ARRAY
whose first element was the tag string; a plain list of strings that merely
started with that same text was then indistinguishable from a tagged value,
and reading it back returned its second element and dropped the rest.  This
asserts the shape that makes the distinction possible, not only that some
particular round trip happens to work."
  (let* ((marker "@superchat/plist-list")
         (plist-lists (list (list :id "a" :source '(:kind gate))))
         (lookalike (list marker "x" "y")))
    ;; The tagged encoding is an object whose key is the tag: its JSON text
    ;; opens a brace at that point.  An array there is the defect's shape.
    (let ((json (superchat-tape--encode-meta (list :inputs plist-lists))))
      (should (string-prefix-p "{\"inputs\":{\"" json))
      (should (string-match-p (format "\"%s\":" (regexp-quote marker)) json)))
    ;; A plain list whose first element is the tag text is not a tag.
    (should-not (superchat-tape--plist-list-p lookalike))
    (should (equal lookalike
                   (superchat-tape--decode-meta
                    (superchat-tape--encode-meta lookalike))))
    ;; Nor when it sits inside a payload beside other keys.
    (let ((payload (list :reasons lookalike :n 1)))
      (should (equal payload
                     (superchat-tape--decode-meta
                      (superchat-tape--encode-meta payload)))))
    ;; A one-element lookalike keeps its single element too.
    (should (equal (list marker)
                   (superchat-tape--decode-meta
                    (superchat-tape--encode-meta (list marker)))))
    ;; Only an object whose SOLE key is the tag is a tag.  An object that
    ;; carries the tag beside other keys is not one and must keep every key;
    ;; reading only "contains the tag key" would drop the rest.
    (let ((pretender (list superchat-tape--plist-list-tag
                           (list (list :a 1))
                           :other 2)))
      (should-not (superchat-tape--plist-list-p pretender))
      (should (equal pretender
                     (superchat-tape--decode-meta
                      (superchat-tape--encode-meta pretender)))))))

(ert-deftest m3/pre-step/split-links-every-part-to-its-input ()
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_WHOLE")
        (lambda (claimed)
          (let ((input (list (plist-get (car claimed) :inject-id))))
            (list (list :source '(:kind splitter) :text "M3R_PART_1" :input-ids input)
                  (list :source '(:kind splitter) :text "M3R_PART_2" :input-ids input))))
      (let ((rows (m3r-attachments m3r-session run)))
        (should (eq 'completed (m3r-status run)))
        (should (equal '("M3R_PART_1" "M3R_PART_2")
                       (mapcar (lambda (r) (m3r-payload r :text)) rows)))
        (dolist (row rows)
          (should (equal ids (m3r-payload row :input-ids)))
          (should (equal "derived" (format "%s" (plist-get (m3r-payload row :source) :kind)))))
        (should-not (m3r-drops m3r-session))))))

(ert-deftest m3/pre-step/partial-and-empty-admission-drop-the-rest ()
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_KEEP" "M3R_OMIT")
        (lambda (claimed) (list (car claimed)))
      (should (eq 'completed (m3r-status run)))
      (should (m3r-prompt-has "M3R_KEEP"))
      (should-not (m3r-prompt-has "M3R_OMIT"))
      (should-not (m3r-drop-reason m3r-session (car ids)))
      (should (equal "pre-step-not-admitted" (m3r-drop-reason m3r-session (cadr ids))))))
  (m3r-fixture '((:text "answer"))
    (m3r-with-gate-result '("M3R_ALL_OMITTED") (lambda (_claimed) nil)
      (should (eq 'completed (m3r-status run)))
      (should (m3r-of-type "step.started" (m3r-events m3r-session run)))
      (should-not (m3r-prompt-has "M3R_ALL_OMITTED"))
      (should-not (m3r-attachments m3r-session))
      (should (equal "pre-step-not-admitted" (m3r-drop-reason m3r-session (car ids))))
      (should (zerop (m3r-pending))))))

(ert-deftest m3/pre-step/invalid-replacement-fails-closed ()
  "A replacement that loses its source relationship is a pre-step failure: no
Step, no dispatch, the Run fails, and the claim is settled without reinsertion."
  (dolist (bad (list
                (cons 'source-changed-without-input-ids
                      (lambda (claimed)
                        (list (plist-put (copy-tree (car claimed)) :source '(:kind forged)))))
                (cons 'unknown-input-id
                      (lambda (_claimed)
                        (list '(:source (:kind forged) :text "t" :input-ids ("inject-ffffff")))))
                (cons 'not-model-visible
                      (lambda (claimed)
                        (list (list :source '(:kind forged)
                                    :input-ids (list (plist-get (car claimed) :inject-id))))))))
    (m3r-fixture '((:text "must not be used"))
      (m3r-with-gate-result '("M3R_CLAIMED") (cdr bad)
        (ert-info ((format "replacement: %s" (car bad)))
          (should (stringp run))
          (should (eq 'failed (m3r-status run)))
          (should-not smoke--requests)
          (should-not (m3r-of-type "step.started" (m3r-events m3r-session run)))
          (should-not (m3r-attachments m3r-session))
          (should (zerop (m3r-pending)))
          (should (equal "pre-step-not-admitted" (m3r-drop-reason m3r-session (car ids)))))))))

(ert-deftest m3/pre-step/gate-cannot-replace-execution-identity ()
  (m3r-fixture '((:text "answer"))
    (let* ((run (m3r-listening
                    ((nil 'pre-step 'm3r-forge
                          (lambda (proposal next)
                            (funcall next (plist-put (plist-put (copy-sequence proposal)
                                                                :run-id "m3r-forged")
                                                     :session-id "m3r-forged")))))
                  (m3r-run "forge")))
           (started (car (m3r-of-type "step.started" (m3r-events m3r-session run)))))
      (should (eq 'completed (m3r-status run)))
      (should (equal run (m3r-payload started :run-id)))
      (should-not (superchat-tape-replay-events "m3r-forged")))))

;;;; Request waterfall ---------------------------------------------------------

(ert-deftest m3/request/fold-and-middleware-rewrites-reach-the-provider ()
  (m3r-fixture '((:text "answer"))
    (let ((run (m3r-listening
                   ((nil 'before-provider-request 'm3r-fold-a
                         (lambda (value)
                           (plist-put value :system (concat (plist-get value :system) " M3R_FOLD_A"))))
                    (nil 'before-provider-request 'm3r-fold-b
                         (lambda (value)
                           (plist-put value :system (concat (plist-get value :system) " M3R_FOLD_B"))))
                    (nil 'before-provider-request 'm3r-middle
                         (lambda (value next)
                           (funcall next (plist-put value :system
                                                    (concat (plist-get value :system) " M3R_MIDDLE"))))))
                 (m3r-run "rewrite"))))
      (should (eq 'completed (m3r-status run)))
      (should (= 1 (length smoke--requests)))
      (dolist (marker '("M3R_FOLD_A" "M3R_FOLD_B" "M3R_MIDDLE"))
        (should (m3r-prompt-has marker))))))

(ert-deftest m3/request/refusal-closes-the-entered-step-as-failed ()
  (m3r-fixture '((:text "must not be used"))
    (let* ((run (m3r-listening
                    ((nil 'before-provider-request 'm3r-refuse
                          (lambda (_value _next) (superchat-waterfall-refuse "m3r"))))
                  (m3r-run "refuse")))
           (events (m3r-events m3r-session run)))
      (should (eq 'failed (m3r-status run)))
      (should-not smoke--requests)
      (should (= 1 (m3r-count "step.started" events)))
      (should (equal '("failed") (m3r-step-dispositions m3r-session run)))
      (should-not (m3r-of-type "run.completed" events))
      (should (< (m3r-index "step.ended" events) (m3r-index "run.failed" events))))))

(ert-deftest m3/request/non-delegating-middleware-ends-the-chain ()
  (m3r-fixture '((:text "answer"))
    (let* ((calls 0)
           (stop (lambda (value _next) (cl-incf calls) value))
           (run (m3r-listening
                    ((nil 'before-provider-request 'm3r-stop-1 stop)
                     (nil 'before-provider-request 'm3r-stop-2 stop))
                  (m3r-run "short-circuit"))))
      (should (eq 'completed (m3r-status run)))
      (should (= 1 calls)))))

(ert-deftest m3/request/late-continuation-is-inert ()
  (m3r-with-scope scope "m3r-late"
    (let ((downstream 0) stashed)
      (m3r-listening
          ;; Waterfall middleware runs in registration order.
          ((scope 'before-provider-request 'm3r-stash
                  (lambda (value next) (setq stashed next) value))
           (scope 'before-provider-request 'm3r-downstream
                  (lambda (value _next) (cl-incf downstream) value)))
        (superchat-emit scope 'before-provider-request '(:prompt "p" :system "s"))
        (should (functionp stashed))
        (should (zerop downstream))
        (ignore-errors (funcall stashed '(:prompt "late" :system "s")))
        (should (zerop downstream))))))

(ert-deftest m3/request/identity-and-tool-ceiling-are-not-rewritable ()
  (dolist (case (list (list 'run-id 'none (lambda (v) (plist-put v :run-id "m3r-replacement")))
                      (list 'unknown-tool 'none (lambda (v) (plist-put v :tools '("m3r-unknown"))))
                      (list 'widen-none 'none (lambda (v) (plist-put v :tools '("read_buffer"))))
                      (list 'widen-explicit '("read_buffer")
                            (lambda (v) (plist-put v :tools '("edit_buffer"))))
                      (list 'non-string-tool 'none (lambda (v) (plist-put v :tools '(ignore))))
                      (list 'claims-prepared 'none (lambda (v) (plist-put v :prepared-tools t)))
                      (list 'non-string-system 'none (lambda (v) (plist-put v :system 123)))))
    (m3r-fixture '((:text "must not be used"))
      (let ((run (m3r-listening
                     ((nil 'before-provider-request 'm3r-patch
                           (let ((patch (nth 2 case)))
                             (lambda (value next) (funcall next (funcall patch value))))))
                   (car (m3r-run-tolerating-errors "patch" :tools (nth 1 case))))))
        (ert-info ((format "patch: %s" (car case)))
          (should (stringp run))
          (should-not smoke--requests)
          (should (eq 'failed (m3r-status run))))))))

(ert-deftest m3/request/tool-rewrite-applies-per-attempt-through-authority ()
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*")))) (:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (calls 0)
           (run (m3r-listening
                    ((nil 'before-provider-request 'm3r-tools
                          (lambda (candidate next)
                            (cl-incf calls)
                            (funcall next (plist-put candidate :tools
                                                     (if (= calls 1) '("read_buffer") 'none))))))
                  (m3r-view-submit view "read")))
           (requests (reverse smoke--requests)))
      (should (eq 'completed (m3r-status run)))
      (should (= 2 calls))
      (should (equal '("read_buffer") (plist-get (car requests) :tools)))
      (should-not (plist-get (cadr requests) :tools))
      (should (m3r-of-type "tool.execution.started" (m3r-events session)))
      (should (eq 'reproduced (car (smoke--case-reconstruction session)))))))

(ert-deftest m3/request/tools-added-on-a-later-step-execute-through-authority ()
  "A tool-free first attempt continued by closing-time input may gain tools;
they still go through the trusted wrapper and Authority."
  (m3r-fixture '((:text "first")
                 (:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                 (:text "done"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (calls 0)
           (run (m3r-listening
                    ((nil 'before-provider-request 'm3r-add-tools
                          (lambda (candidate next)
                            (cl-incf calls)
                            (funcall next (plist-put candidate :tools
                                                     (if (= calls 2) '("read_buffer") 'none)))))
                     (nil 'turn-stopping 'm3r-continue
                          (m3r-closing-once
                           (lambda (run)
                             (superchat-runtime-inject (plist-get run :session-id)
                                                       (m3r-material "continue"))))))
                  (m3r-view-submit view "hello")))
           (tools (mapcar (lambda (r) (plist-get r :tools)) (reverse smoke--requests))))
      (should (eq 'completed (m3r-status run)))
      (should (= 3 calls))
      (should (equal '(nil ("read_buffer") nil) tools))
      (should (m3r-of-type "tool.execution.started" (m3r-events session))))))

(ert-deftest m3/request/tool-guidance-follows-final-tools-without-recollecting ()
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*")))) (:text "done"))
    (let* ((view (smoke--open-chat))
           (collected 0)
           (calls 0))
      (superchat-context-register
       'm3r-guidance
       (lambda (_envelope _state _tools)
         (cl-incf collected)
         (list (plist-put (superchat-prompt-fragment
                           :owner 'm3r :id "m3r-guidance" :slot 'system :text "M3R_GUIDE:initial")
                          :render-tools
                          (lambda (names)
                            (concat "M3R_GUIDE:" (if names (string-join names ",") "none"))))))
       :owner 'm3r)
      (unwind-protect
          (m3r-listening
              ((nil 'before-provider-request 'm3r-guided-tools
                    (lambda (candidate next)
                      (cl-incf calls)
                      (funcall next (plist-put candidate :tools
                                               (if (= calls 1) '("read_buffer") 'none))))))
            (let ((run (m3r-view-submit view "guided"))
                  (prompts (m3r-prompts)))
              (should (eq 'completed (m3r-status run)))
              (should (= 1 collected))
              (should (= 2 calls))
              (should (m3r-prompt-has "M3R_GUIDE:read_buffer" (car prompts)))
              (should (m3r-prompt-has "M3R_GUIDE:none" (cadr prompts)))))
        (superchat-context-unregister 'm3r-guidance)))))

(ert-deftest m3/request/ordinary-tools-without-a-trusted-wrapper-cannot-dispatch ()
  "ADR-0005: selected ordinary tools with no Authority wrapper fail closed.
The refusal surfaces synchronously from submit; the accepted Run still settles."
  (m3r-fixture '((:text "must not be used"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (condition
            (m3r-listening
                ((nil 'before-provider-request 'm3r-select
                      (lambda (candidate next)
                        (funcall next (plist-put candidate :tools '("read_buffer"))))))
              (cl-letf (((symbol-function 'superchat--agent-make-wrapper)
                         (lambda (&rest _) nil)))
                (condition-case err (progn (m3r-view-submit view "no wrapper") nil)
                  (error err)))))
           (run (m3r-run-id-of session)))
      (should (eq 'superchat-llm-tool-wrapper-required (car condition)))
      (should-not smoke--requests)
      (should (stringp run))
      (m3r-settle run)
      (should (eq 'failed (m3r-status run))))))

(ert-deftest m3/request/prepared-tool-surface-cannot-be-replaced ()
  (m3r-listening
      ((nil 'before-provider-request 'm3r-drop-prepared
            (lambda (candidate next) (funcall next (plist-put candidate :tools nil)))))
    (should-error (superchat--request-waterfall
                   nil (list :prompt "p" :system "s" :tools '("read_buffer")) nil t))))

;;;; Step facts ----------------------------------------------------------------

(ert-deftest m3/step/facts-carry-identity-and-close-before-the-run-terminal ()
  (m3r-fixture '((:text "answer"))
    (let* ((run (m3r-run "one step"))
           (status (superchat-runtime-status run))
           (events (m3r-events m3r-session run))
           (started (car (m3r-of-type "step.started" events)))
           (ended (car (m3r-of-type "step.ended" events)))
           (message-list (car (m3r-message-lists m3r-session run))))
      (should (= 1 (m3r-count "step.started" events)))
      (should (= 1 (m3r-count "step.ended" events)))
      (dolist (row (list started ended))
        (should (equal m3r-session (m3r-payload row :session-id)))
        (should (equal (plist-get status :turn-id) (m3r-payload row :turn-id)))
        (should (equal run (m3r-payload row :run-id)))
        (should (eql 1 (m3r-payload row :step)))
        (should (and (stringp (m3r-payload row :request-id))
                     (not (string-empty-p (m3r-payload row :request-id))))))
      (should (equal "completed" (m3r-payload ended :disposition)))
      (should (eql 1 (plist-get (m3r-payload message-list :execution) :step)))
      (should (equal (plist-get status :turn-id)
                     (plist-get (m3r-payload message-list :execution) :turn-id)))
      (should (< (m3r-index "run.started" events)
                 (m3r-index "step.started" events)
                 (m3r-index "step.ended" events)
                 (m3r-index "response.completed" events)
                 (m3r-index "run.completed" events))))))

(ert-deftest m3/step/cancel-closes-the-step-as-cancelled-before-run-cancelled ()
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (superchat-runtime-cancel run "[m3r cancel]")
      (m3r-settle run)
      (let ((events (m3r-events m3r-session run)))
        (should (eq 'cancelled (m3r-status run)))
        (should (equal '("cancelled") (m3r-step-dispositions m3r-session run)))
        (should (< (m3r-index "step.ended" events) (m3r-index "run.cancelled" events)))))))

(ert-deftest m3/step/provider-failure-closes-the-step-as-failed ()
  (m3r-fixture '((:error (error . "m3r provider down")))
    (let* ((run (m3r-run "fail"))
           (events (m3r-events m3r-session run)))
      (should (eq 'failed (m3r-status run)))
      (should (equal '("failed") (m3r-step-dispositions m3r-session run)))
      (should (< (m3r-index "step.ended" events) (m3r-index "run.failed" events))))))

(ert-deftest m3/step/each-model-attempt-is-its-own-step ()
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*")))) (:text "second"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m3r-settle (smoke--send view "two attempts")))
           (events (m3r-events session run))
           (brackets (cl-remove-if-not
                      (lambda (type) (member type '("step.started" "step.ended")))
                      (m3r-types events))))
      (should (eq 'completed (m3r-status run)))
      (should (= 1 (m3r-count "run.started" events)))
      (should (equal '("step.started" "step.ended" "step.started" "step.ended") brackets))
      (should (equal '(1 2) (mapcar (lambda (e) (m3r-payload e :step))
                                    (m3r-of-type "step.started" events))))
      (should (equal '(1 2) (mapcar (lambda (e) (plist-get (m3r-payload e :execution) :step))
                                    (m3r-message-lists session run))))
      (should (< (m3r-index "tool.execution.started" events)
                 (m3r-index "step.ended" events)
                 (m3r-index "step.started" events t))))))

(ert-deftest m3/step/advance-step-records-the-prior-disposition ()
  "The transport seam closes the prior Step with the disposition it is given."
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (should (listp (superchat-service-call nil 'driver 'advance-step run '(:prompt "a") nil)))
      (should (listp (superchat-service-call nil 'driver 'advance-step run '(:prompt "b") nil 'failed)))
      (should-error (superchat-service-call nil 'driver 'advance-step run '(:prompt "c") nil 'bogus))
      (let ((events (m3r-events m3r-session run)))
        (should (= 3 (m3r-count "step.started" events)))
        (should (equal '("completed" "failed") (m3r-step-dispositions m3r-session run)))
        (should-not (superchat-runtime--terminal-p (m3r-status run)))))))

(ert-deftest m3/step/refused-advance-opens-no-step-and-fails-the-run ()
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (m3r-listening
          ((nil 'pre-step 'm3r-refuse (lambda (&rest _) (superchat-waterfall-refuse "m3r"))))
        (should (eq superchat--step-refused
                    (superchat-service-call nil 'driver 'advance-step run '(:prompt "x") nil))))
      (m3r-settle run)
      (let ((events (m3r-events m3r-session run)))
        (should (eq 'failed (m3r-status run)))
        (should (= 1 (m3r-count "step.started" events)))
        (should (= 1 (m3r-count "step.ended" events)))))))

(ert-deftest m3/step/a-tool-protocol-retry-closes-the-prior-step-failed ()
  "A model call to an unknown tool returns to the driver for another attempt:
that attempt is a new Step, and the one that failed the tool protocol closes
`failed'."
  (m3r-fixture '((:tool-uses (("m3r_no_such_tool" (x . "1")))) (:text "recovered"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m3r-settle (smoke--send view "call a tool that does not exist")))
           (events (m3r-events session run)))
      (should (eq 'completed (m3r-status run)))
      (should (= 2 (length smoke--requests)))
      (should (= 1 (m3r-count "run.started" events)))
      (should (equal '("failed" "completed") (m3r-step-dispositions session run))))))

(ert-deftest m3/step/a-provider-failure-on-a-later-attempt-closes-that-step-failed ()
  (m3r-fixture '((:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                 (:error (error . "m3r provider down")))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m3r-settle (smoke--send view "tool, then the provider fails")))
           (events (m3r-events session run)))
      (should (eq 'failed (m3r-status run)))
      (should (equal '("completed" "failed") (m3r-step-dispositions session run)))
      (should (< (m3r-index "step.ended" events t) (m3r-index "run.failed" events))))))

(ert-deftest m3/step/full-baseline-rejects-missing-step-facts ()
  "The canonical smoke gate cannot be satisfied by deleting Step rows."
  (let* ((facts (smoke--facts-for-mode (smoke-case-A)))
         (key (smoke--event-fact-key facts))
         (stripped (smoke--with-reported-events
                    facts (smoke--without-step-events (cdr (assoc key facts)))))
         (standard-output #'ignore)
         (smoke--failures 0))
    (should (smoke--check "A" facts))
    (should-not (smoke--check "A" stripped))))

;;;; Turn-stopping -------------------------------------------------------------

(ert-deftest m3/turn-stopping/notifies-each-listener-once-before-the-seal ()
  (m3r-fixture '((:text "answer"))
    (let* (seen
           (observe (lambda (name)
                      (lambda (run _turn)
                        (let ((id (plist-get run :run-id)))
                          (push (list name
                                      (m3r-status id)
                                      (m3r-of-type "run.completed" (m3r-events m3r-session id)))
                                seen)))))
           (run (m3r-listening
                    ((nil 'turn-stopping 'm3r-a (funcall observe 'a))
                     (nil 'turn-stopping 'm3r-b (funcall observe 'b)))
                  (m3r-run "close"))))
      (should (eq 'completed (m3r-status run)))
      (should (equal '(a b) (sort (mapcar #'car seen) #'string<)))
      (dolist (row seen)
        (should-not (superchat-runtime--terminal-p (nth 1 row)))
        (should-not (nth 2 row))))))

(defun m3r-closing-once (action)
  "A turn-stopping listener that runs ACTION with the Run projection once."
  (let ((done nil))
    (lambda (run _turn)
      (unless done
        (setq done t)
        (funcall action run)))))

(ert-deftest m3/turn-stopping/injection-during-notification-continues-the-same-run ()
  (m3r-fixture '((:text "first") (:text "second"))
    (let* ((calls 0)
           (inject (m3r-closing-once
                    (lambda (run)
                      (superchat-runtime-inject (plist-get run :session-id)
                                                (m3r-material "M3R_CONTINUE")))))
           (run (m3r-listening
                    ((nil 'turn-stopping 'm3r-inject
                          (lambda (run turn) (cl-incf calls) (funcall inject run turn))))
                  (m3r-run "continue")))
           (events (m3r-events m3r-session run)))
      (should (eq 'completed (m3r-status run)))
      (should (= 2 (length smoke--requests)))
      (should (m3r-prompt-has "M3R_CONTINUE" (cadr (m3r-prompts))))
      (should (= 1 (m3r-count "run.started" (m3r-events m3r-session))))
      (should (= 2 calls))
      (should (= 1 (m3r-count "run.completed" events)))
      (should (equal '("completed" "completed") (m3r-step-dispositions m3r-session run)))
      (should (equal '(1 2)
                     (mapcar (lambda (e) (plist-get (m3r-payload e :execution) :step))
                             (m3r-message-lists m3r-session run)))))))

(ert-deftest m3/turn-stopping/steering-during-notification-continues-the-same-run ()
  (m3r-fixture '((:text "first") (:text "second"))
    (let ((run (m3r-listening
                   ((nil 'turn-stopping 'm3r-steer
                         (m3r-closing-once
                          (lambda (run)
                            (superchat-runtime-steer (plist-get run :run-id) "M3R_STEERED")))))
                 (m3r-run "steer"))))
      (should (eq 'completed (m3r-status run)))
      (should (= 2 (length smoke--requests)))
      (should (m3r-prompt-has "M3R_STEERED" (cadr (m3r-prompts))))
      (should (= 1 (m3r-count "run.started" (m3r-events m3r-session))))
      (should (= 2 (m3r-count "step.started" (m3r-events m3r-session run)))))))

(ert-deftest m3/turn-stopping/followup-during-notification-starts-a-new-turn ()
  "Future-Turn followups are not eligible next-step input: they queue."
  (m3r-fixture '((:text "first") (:text "second"))
    (let* (admission
           (first (m3r-listening
                      ((nil 'turn-stopping 'm3r-followup
                            (m3r-closing-once
                             (lambda (_run) (setq admission (m3r-submit "M3R_NEXT_TURN"))))))
                    (m3r-run "close"))))
      (should (eq 'queue (plist-get admission :status)))
      (m3r-wait-turns m3r-session 2)
      (should (= 1 (m3r-count "step.started" (m3r-events m3r-session first))))
      (should (= 2 (m3r-count "run.started" (m3r-events m3r-session))))
      (should-not (m3r-prompt-has "M3R_NEXT_TURN" (car (m3r-prompts))))
      (should (m3r-prompt-has "M3R_NEXT_TURN" (cadr (m3r-prompts)))))))

(ert-deftest m3/turn-stopping/listener-failure-fails-the-run ()
  (m3r-fixture '((:text "answer"))
    (let* ((run (m3r-listening
                    ((nil 'turn-stopping 'm3r-explode
                          (lambda (_run _turn) (error "m3r notification explodes"))))
                  (m3r-run "explode")))
           (events (m3r-events m3r-session run)))
      (should (eq 'failed (m3r-status run)))
      (should (= 1 (m3r-count "run.failed" events)))
      (should-not (m3r-of-type "run.completed" events))
      (should (equal '("failed") (m3r-step-dispositions m3r-session run))))))

(ert-deftest m3/turn-stopping/unrelated-recorded-fault-does-not-fail-the-run ()
  (m3r-fixture '((:text "answer"))
    (let ((run (m3r-listening
                   ((nil 'turn-stopping 'm3r-diagnose
                         (lambda (_run _turn)
                           (superchat-event-record-fault
                            'unrelated 'm3r-diagnostic 'probe '(error "not ours")))))
                 (m3r-run "diagnose"))))
      (should (eq 'completed (m3r-status run))))))

(ert-deftest m3/turn-stopping/unrecordable-material-is-audit-incomplete-without-dispatch ()
  (m3r-fixture '((:text "first") (:text "must not be sent"))
    (let* ((original (symbol-function 'superchat-tape-append-event))
           (run (m3r-listening
                    ((nil 'turn-stopping 'm3r-inject
                          (m3r-closing-once
                           (lambda (run)
                             (superchat-runtime-inject (plist-get run :session-id)
                                                       (m3r-material "M3R_UNRECORDABLE"))))))
                  (cl-letf (((symbol-function 'superchat-tape-append-event)
                             (lambda (event &rest rest)
                               (if (equal "attachment.added" (plist-get event :type))
                                   (error "m3r append fails")
                                 (apply original event rest)))))
                    (m3r-run "unrecordable")))))
      (should (eq 'audit-incomplete (m3r-status run)))
      (should (= 1 (length smoke--requests)))
      (should-not (m3r-prompt-has "M3R_UNRECORDABLE")))))

;;;; Run-sealing (legacy terminal observers) -----------------------------------

(ert-deftest m3/run-sealing/submission-during-seal-is-queued-not-duplicated ()
  (m3r-fixture '((:text "first") (:text "second"))
    (let* (admission
           (first (m3r-listening
                      ((nil 'run-sealing 'm3r-seal-submit
                            (let ((done nil))
                              (lambda (_run _turn)
                                (unless done
                                  (setq done t
                                        admission (m3r-submit "M3R_AFTER_SEAL")))))))
                    (m3r-run "seal"))))
      (should (eq 'completed (m3r-status first)))
      (should (eq 'queue (plist-get admission :status)))
      (should (equal "session-busy" (plist-get admission :reason)))
      (m3r-wait-turns m3r-session 2)
      (let ((events (m3r-events m3r-session)))
        (should (m3r-of-type "queue.queued" events))
        (should (m3r-of-type "queue.dispatched" events))
        (should (= 2 (m3r-count "turn.completed" events)))
        (should (= 1 (m3r-count "turn.completed" (m3r-events m3r-session first))))
        (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))))))

(ert-deftest m3/run-sealing/listener-fault-leaves-exactly-one-terminal-fact ()
  (m3r-fixture '((:text "answer"))
    (let* ((run (m3r-listening
                    ((nil 'run-sealing 'm3r-seal-explode
                          (lambda (_run _turn) (error "m3r seal explodes"))))
                  (m3r-run "seal fault")))
           (events (m3r-events m3r-session run)))
      (should (= 1 (+ (m3r-count "run.completed" events)
                      (m3r-count "run.failed" events)
                      (m3r-count "run.cancelled" events)))))))

;; M3 closing rulings, 2026-09-15: the product's own turn recorders are required
;; recording contributors of `run-sealing'.
(defconst m3r-turn-recorders
  '((superchat--hook-record-user-message . record-user-message)
    (superchat--hook-record-attachments . record-attachments))
  "The `chat-turn-recorder' listeners: their function and their listener id.")

(ert-deftest m3/run-sealing/a-failed-turn-recording-seals-audit-incomplete ()
  "KNOWN GAP (ruled 2026-09-15): when a turn recorder signals while the Run is
about to seal `completed', the Run seals `audit-incomplete' through the existing
`run.failed' fact.  The answer was delivered, so `response.completed' and the
Step's `completed' ending stay as recorded.  Remove `:expected-result' when
implemented."
  :expected-result :failed
  (dolist (recorder m3r-turn-recorders)
    (m3r-fixture '((:text "answer"))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (cl-letf (((symbol-function (car recorder))
                             (lambda (&rest _) (error "m3r recording fails"))))
                    (m3r-settle (smoke--send view "record me"))))
             (events (m3r-events session run)))
        (ert-info ((format "failing recorder: %s" (cdr recorder)))
          (should (cl-find (cdr recorder) superchat-event-faults
                           :key (lambda (fault) (plist-get fault :id))))
          (should (eq 'audit-incomplete (m3r-status run)))
          (should-not (m3r-of-type "run.completed" events))
          (should (= 1 (m3r-count "run.failed" events)))
          (should (equal "audit-incomplete"
                         (m3r-payload (car (m3r-of-type "run.failed" events)) :status)))
          (should (equal "audit-incomplete"
                         (m3r-payload (car (m3r-of-type "turn.completed" events)) :status)))
          (should (m3r-of-type "response.completed" events))
          (should (equal '("completed") (m3r-step-dispositions session run))))))))

(ert-deftest m3/run-sealing/other-failures-do-not-change-the-disposition ()
  "Only the declared recorders decide: a third-party `run-sealing' observer that
signals, or an unrelated fault that merely carries a recorder's listener id,
leaves a completed Run completed."
  (m3r-fixture '((:text "answer"))
    (let* ((view (smoke--open-chat))
           (run (m3r-listening
                    ((nil 'run-sealing 'm3r-third-party
                          (lambda (&rest _) (error "m3r third-party observer fails")))
                     (nil 'turn-stopping 'm3r-lookalike-fault
                          (lambda (&rest _)
                            (superchat-event-record-fault
                             'm3r-elsewhere 'record-user-message 'm3r '(error "not the recorder")))))
                  (m3r-settle (smoke--send view "observed")))))
      (should (cl-find 'm3r-third-party superchat-event-faults
                       :key (lambda (fault) (plist-get fault :id))))
      (should (eq 'completed (m3r-status run))))))

(ert-deftest m3/run-sealing/a-failed-turn-recording-leaves-a-cancelled-run-cancelled ()
  (dolist (recorder m3r-turn-recorders)
    (m3r-fixture '((:hang t))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view)))
        (cl-letf (((symbol-function (car recorder))
                   (lambda (&rest _) (error "m3r recording fails"))))
          (let ((run (smoke--send view "hold")))
            (should (smoke--wait (lambda () smoke--requests) 10))
            (superchat-runtime-cancel run "[m3r cancel]")
            (m3r-settle run)
            (ert-info ((format "failing recorder: %s" (cdr recorder)))
              (should (eq 'cancelled (m3r-status run)))
              (should (= 1 (m3r-count "run.cancelled" (m3r-events session run))))
              (should-not (m3r-of-type "run.failed" (m3r-events session run))))))))))

;;;; Driver handle and transport seam ------------------------------------------

(ert-deftest m3/driver/handle-is-opaque-plain-data-and-grants-nothing ()
  (m3r-fixture '((:text "answer"))
    (cl-flet ((call (&rest args) (apply #'superchat-service-call nil 'driver args)))
      (let* ((handle (call 'handle m3r-session))
             (state (call 'handle-state handle)))
        (should (call 'handle-p handle))
        (should-not (stringp handle))
        (should-not (call 'handle ""))
        (should-not (call 'handle 42))
        (should (equal m3r-session (funcall handle 'session-id)))
        (should (equal m3r-session (plist-get state :session-id)))
        (should-not (plist-get state :active-run-id))
        (should-not (plist-member state :queue))
        (cl-loop for (_key value) on state by #'cddr
                 do (should (or (null value) (stringp value) (integerp value)
                                (eq value t) (eq value :false))))
        (dolist (operation '(decide cancel submit))
          (should-error (funcall handle operation)))
        (should-error (call 'handle-state m3r-session))
        (should (stringp (call 'inject m3r-session (m3r-material "via-driver"))))
        (should (= 1 (plist-get (call 'handle-state handle) :pending-injections)))
        (should (= 1 (m3r-pending)))
        (m3r-run "handle run")
        (should-not (plist-get (call 'handle-state handle) :active-run-id))))))

(ert-deftest m3/driver/turn-stopping-seam-reports-waiting-input ()
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold"))
          (handle (superchat-service-call nil 'driver 'handle m3r-session))
          (seen 0))
      (should (equal run (plist-get (superchat-service-call nil 'driver 'handle-state handle)
                                    :active-run-id)))
      (m3r-listening ((nil 'turn-stopping 'm3r-seen (lambda (&rest _) (cl-incf seen))))
        (should (zerop (superchat-service-call nil 'driver 'turn-stopping run)))
        (should (= 1 seen))
        (superchat-runtime-steer run "M3R_WAITING")
        (should (= 1 (superchat-service-call nil 'driver 'turn-stopping run)))))))

;;;; Context -------------------------------------------------------------------

(ert-deftest m3/context/no-stage-vocabulary-survives ()
  "KNOWN GAP (ruled 2026-09-15): no Stage name survives M3.  The Event is
`context-error', fault records carry `:site', and the substrate is the feature
`superchat-event' in `superchat-event.el'.  Remove `:expected-result' when the
rename lands."
  :expected-result :failed
  (let ((superchat-event-faults nil))
    (superchat-event-record-fault 'm3r-site 'm3r-id 'm3r '(error "probe"))
    (should (plist-member (car superchat-event-faults) :site))
    (should-not (plist-member (car superchat-event-faults) :stage)))
  (should (eq 'observe (plist-get (cdr (assq 'context-error superchat-event--definitions))
                                  :rule)))
  (should-not (assq 'stage-error superchat-event--definitions))
  (should (featurep 'superchat-event))
  (should-not (featurep 'superchat-stage))
  (should-not (locate-library "superchat-stage")))

(ert-deftest m3/context/stage-registry-is-gone-and-collect-context-is-an-event ()
  (should-not (fboundp 'superchat-stage-add))
  (should-not (fboundp 'superchat-stage-call))
  (should-not (fboundp 'superchat-stage-call-outcome))
  (should-not (boundp 'superchat-stage-names))
  (should (eq 'collect (plist-get (cdr (assq 'collect-context superchat-event--definitions))
                                  :rule))))

(ert-deftest m3/context/assembly-order-follows-priority-not-collection ()
  (m3r-with-scope scope "m3r-order"
    (let ((superchat-plugin-current-scope nil))
      (superchat-context-register
       'm3r-low (lambda (&rest _) (list (list :owner 'm3r :id "low" :slot 'context
                                              :text "M3R_LOW" :priority 0 :budget-class 'hard)))
       :owner 'm3r :scope scope)
      (superchat-context-register
       'm3r-high (lambda (&rest _) (list (list :owner 'm3r :id "high" :slot 'context
                                               :text "M3R_HIGH" :priority 50 :budget-class 'hard)))
       :owner 'm3r :scope scope)
      (let* ((superchat-execution-scope scope)
             (fragments (apply #'append (superchat-context-collect nil nil nil)))
             (prompt (plist-get (car (plist-get (superchat-prompt-assemble fragments 1000000)
                                                :messages))
                                :content)))
        (should (equal '("high" "low") (mapcar (lambda (f) (plist-get f :id)) fragments)))
        (should (< (string-match-p "M3R_HIGH" prompt) (string-match-p "M3R_LOW" prompt)))))))

(ert-deftest m3/context/required-failure-aborts-before-dispatch-and-reports-once ()
  (m3r-fixture '((:text "must not be used"))
    (let ((errors 0))
      (superchat-context-register 'm3r-required
                                  (lambda (&rest _) (error "m3r required collector fails"))
                                  :owner 'm3r :required t)
      (unwind-protect
          (m3r-listening ((nil (m3r-context-error-event) 'm3r-observer (lambda (&rest _) (cl-incf errors))))
            (should (eq 'superchat-context-required-failed
                        (car (should-error (m3r-submit "required")))))
            (should-not smoke--requests)
            (should (= 1 errors))
            (should (= 1 (cl-count 'm3r-required superchat-event-faults
                                   :key (lambda (f) (plist-get f :id))))))
        (superchat-context-unregister 'm3r-required)))))

(ert-deftest m3/context/optional-failure-and-invalid-fragment-are-recorded-not-fatal ()
  (let ((superchat-event-faults nil)
        (superchat-execution-scope superchat-scope-global))
    (dolist (case (list (cons 'm3r-optional (lambda (&rest _) (error "m3r optional fails")))
                        (cons 'm3r-invalid (lambda (&rest _) '(:not-a-fragment-list t)))))
      (superchat-context-register (car case) (cdr case) :owner 'm3r)
      (unwind-protect
          (should-not (superchat-pipeline--collect-context nil nil nil))
        (superchat-context-unregister (car case)))
      (should (= 1 (cl-count (car case) superchat-event-faults
                             :key (lambda (f) (plist-get f :id))))))))

;;;; Real provider -------------------------------------------------------------

(ert-deftest m3/real/normal-turn-completes-with-a-closed-step ()
  :tags '(:real)
  (skip-unless (getenv "SMOKE_REAL"))
  (m3r-fixture nil
    (let* ((run (plist-get (m3r-submit "Reply with the single word: ready") :run-id))
           (_ (m3r-settle run 240))
           (events (m3r-events m3r-session run)))
      (should (eq 'completed (m3r-status run)))
      (should (= 1 (m3r-count "run.completed" events)))
      (should-not (m3r-of-type "run.failed" events))
      (should (m3r-of-type "request.header" (m3r-events m3r-session)))
      (should (member "completed" (m3r-step-dispositions m3r-session run))))))

(ert-deftest m3/real/cancelled-turn-seals-cancelled-with-a-closed-step ()
  :tags '(:real)
  (skip-unless (getenv "SMOKE_REAL"))
  (m3r-fixture nil
    (let ((run (plist-get (m3r-submit "Write a very long essay about the history of clocks.")
                          :run-id)))
      ;; Cancel as soon as the Step is open, so the attempt is in flight however
      ;; fast the model is.
      (should (smoke--wait (lambda () (m3r-of-type "step.started" (m3r-events m3r-session run)))
                           60))
      (superchat-runtime-cancel run "[m3r real cancel]")
      (m3r-settle run 60)
      (let ((events (m3r-events m3r-session run)))
        (should (eq 'cancelled (m3r-status run)))
        (should-not (m3r-of-type "run.completed" events))
        (should (= 1 (m3r-count "run.cancelled" events)))
        (should (equal (m3r-count "step.started" events) (m3r-count "step.ended" events)))))))

(provide 'm3-regression)
;;; m3-regression.el ends here
