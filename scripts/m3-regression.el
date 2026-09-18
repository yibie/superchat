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
;; authority is `docs/decisions/implemented/2026-09-14-m3-execution-main-line-resolution.md'
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
;; The M3a areas come from `docs/decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md'
;; and were written before its implementation:
;;
;;   m3a/seal/         Runtime seals `audit-incomplete' over open Effects and Approvals
;;   m3a/reopen/       repair of a cut-off Run when its Session is reopened
;;   m3a/consumer/     subagent, workflow and Artifact decisions from the recorded terminal
;;   m3a/experience/   which Runs an evaluation may cite
;;   m3a/removal/      Closure, Claims and Experience's entry points are gone
;;
;; The M4 areas come from `docs/decisions/proposed/2026-09-16-m4-surfaces-as-plugins.md'
;; and the M4 entry of `docs/roadmap.md', written before that implementation:
;;
;;   m4/default/       the default composition and the chat front door
;;   m4/profile/       a profile is layers over the default rows
;;   m4/surface/       a surface is a row that owns its name and entry point
;;   m4/headless/      a profile with no user-interface row still answers
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

(defmacro m3r-tape-unreadable-after-first-lookup (&rest body)
  "Run BODY while every Tape event-state lookup after the first is unreadable.
With `m3r-losing-receipt-once' this leaves a drop whose write committed but
cannot be confirmed: the item must stay pending and must not become input."
  (declare (indent 0) (debug t))
  (let ((original (make-symbol "original")) (lookups (make-symbol "lookups")))
    `(let ((,original (symbol-function 'superchat-tape--event-state))
           (,lookups 0))
       (cl-letf (((symbol-function 'superchat-tape--event-state)
                  (lambda (event)
                    (setq ,lookups (1+ ,lookups))
                    (if (= ,lookups 1)
                        (funcall ,original event)
                      (list :state 'unknown :condition '(error "m3r: Tape unreadable"))))))
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
  "The follow-up FIFO recognises a committed `queue.dropped' whose receipt was
lost and does not record the drop again.  Defect found 2026-09-15, fixed the
same day."
  (m3r-fixture '((:hang t))
    (m3r-live-run "hold")
    (let ((queue-id (plist-get (m3r-submit "queued") :queue-id)))
      (should (stringp queue-id))
      (m3r-losing-receipt-once "queue.dropped"
        (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed)))
      (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed))
      (should (= 1 (m3r-drops-for m3r-session queue-id)))
      (should (zerop (superchat-runtime-pending-follow-ups m3r-session))))))

(ert-deftest m3/admission/a-follow-up-awaiting-drop-confirmation-is-not-dispatched ()
  "A queued follow-up whose drop committed but cannot be confirmed stays queued
with its drop identity; when the Session becomes free it is reconciled, never
submitted as a Run."
  (m3r-fixture '((:hang t) (:text "must not be used"))
    (let* ((first (m3r-live-run "hold"))
           (queue-id (plist-get (m3r-submit "M3R_AWAITING_FOLLOWUP") :queue-id)))
      (should (stringp queue-id))
      (m3r-tape-unreadable-after-first-lookup
        (m3r-losing-receipt-once "queue.dropped"
          (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed)))
        (should (= 1 (superchat-runtime-pending-follow-ups m3r-session)))
        (superchat-runtime-cancel first "[m3r cancel]")
        (m3r-settle first)
        ;; Give the dispatcher its chance while the drop is still unconfirmed.
        (smoke--wait (lambda () (> (length smoke--requests) 1)) 1.0))
      (should (= 1 (length smoke--requests)))
      (should (= 1 (m3r-count "run.started" (m3r-events m3r-session))))
      (superchat-runtime-clear-follow-ups m3r-session 'view-closed)
      (should (zerop (superchat-runtime-pending-follow-ups m3r-session)))
      (should (= 1 (m3r-drops-for m3r-session queue-id))))))

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

;; The smoke V baseline line `chat-parser-direct' only checks that
;; `superchat-core-prepare-turn' exists.  The user accepted it on 2026-09-15 on
;; condition that parsing behaviour is proven by regression tests; together with
;; the tests around these two and smoke cases S, T and W, every chat input form
;; the parser reads (@model, >>workflow, >skill, /command, #file) is exercised.
(ert-deftest m3/parse/at-model-selects-the-model-for-that-turn ()
  (m3r-fixture '((:text "answer"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (models nil)
           (streaming (symbol-function 'llm-chat-streaming))
           (async (symbol-function 'llm-chat-async))
           (run (cl-letf (((symbol-function 'llm-chat-streaming)
                           (lambda (provider &rest args)
                             (push (llm-openai-chat-model provider) models)
                             (apply streaming provider args)))
                          ((symbol-function 'llm-chat-async)
                           (lambda (provider &rest args)
                             (push (llm-openai-chat-model provider) models)
                             (apply async provider args))))
                  (m3r-settle (smoke--send view "@m3r-other-model M3R_MODEL_QUESTION")))))
      (should (eq 'completed (m3r-status run)))
      (should (equal '("m3r-other-model") models))
      (should (equal "m3r-other-model"
                     (m3r-payload (car (m3r-of-type "run.started" (m3r-events session run)))
                                  :model)))
      (should (m3r-prompt-has "M3R_MODEL_QUESTION"))
      (should-not (m3r-prompt-has "@m3r-other-model")))))

(ert-deftest m3/parse/hash-file-reference-adds-the-file-and-strips-the-token ()
  (m3r-fixture '((:text "answer"))
    (let* ((view (smoke--open-chat))
           (file (expand-file-name "m3r-notes.txt" superchat-data-directory)))
      (with-temp-file file (insert "M3R_FILE_CONTENT line\n"))
      (let ((run (m3r-settle (smoke--send view (format "#%s M3R_FILE_QUESTION" file)))))
        (should (eq 'completed (m3r-status run)))
        (should (m3r-prompt-has "M3R_FILE_CONTENT"))
        (should (m3r-prompt-has "M3R_FILE_QUESTION"))
        (should-not (m3r-prompt-has (concat "#" file)))))))

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

(ert-deftest m3/parse/a-non-chat-submission-records-no-context-fault ()
  "A submission from a surface with no chat Turn assembles its prompt without a
recorded collector fault: a collector that reads Turn state skips an Envelope
that has none instead of signalling.
Fixes the registered issue `.scratch/context-collectors-non-chat/issues/01-...'."
  (m3r-fixture '((:text "answer"))
    (let ((run (m3r-run "hello")))
      (should (eq 'completed (m3r-status run)))
      (should-not superchat-event-faults)
      ;; The prompt was really assembled and sent, so the collectors ran.
      (should (= 1 (length smoke--requests))))))

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
  "A `queue.dropped' append that commits but loses its receipt is recognised by
its retained event identity and not written again, for the view-close drop and
the drop at the end of the targeted Run.  Defect found 2026-09-15, fixed the
same day."
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

(ert-deftest m3/inject/a-drop-awaiting-confirmation-is-not-admitted-as-input ()
  "An injection whose drop committed but cannot be confirmed stays pending with
its drop identity; a later Turn must not admit it, and it settles once the Tape
reads again, with one drop fact."
  (m3r-fixture '((:text "answer"))
    (let ((id (superchat-runtime-inject m3r-session (m3r-material "M3R_AWAITING_DROP"))))
      (m3r-tape-unreadable-after-first-lookup
        (m3r-losing-receipt-once "queue.dropped"
          (ignore-errors (superchat-runtime-clear-follow-ups m3r-session 'view-closed))))
      (should (= 1 (m3r-pending)))
      (m3r-run "a later turn")
      (should-not (m3r-prompt-has "M3R_AWAITING_DROP"))
      (should-not (m3r-attachments m3r-session))
      (superchat-runtime-clear-follow-ups m3r-session 'view-closed)
      (should (zerop (m3r-pending)))
      (should (= 1 (m3r-drops-for m3r-session id))))))

(ert-deftest m3/inject/admitted-material-reaches-the-next-turn-once ()
  "Admitted material enters history once (M3 closing ruling 4): within a Run a
later Step may carry it in the provider prompt, and a later Turn's history,
assembled from the Tape, contains it exactly once."
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

(ert-deftest m3/inject/material-over-the-inline-limit-is-refused-at-the-inbox ()
  "Material whose attachment payload cannot be stored inline is refused when it
is injected, with `material-too-large' and nothing left pending, so no later
Turn claims material it could never record.  The Session keeps working.
Fixes the registered issue `.scratch/oversized-injection/issues/01-...'."
  (m3r-fixture '((:text "answer"))
    (let* ((huge (make-string (* 2 superchat-tape-max-inline-bytes) ?x))
           (refusal (superchat-runtime-inject m3r-session (m3r-material huge "oversized"))))
      (should (equal '(:accepted nil :reason material-too-large) refusal))
      (should (= 0 (m3r-pending)))
      (let ((run (m3r-run "hello")))
        (should (eq 'completed (m3r-status run)))
        (should-not (m3r-attachments m3r-session run))
        (should-not (m3r-drops m3r-session))
        (should-not superchat-event-faults)))))

(ert-deftest m3/inject/material-that-grows-past-the-inline-limit-fails-the-step-with-a-drop-fact ()
  "A pre-step gate may rewrite admitted material into a payload the Tape cannot
store inline.  The whole batch is then refused before a Step opens: the claimed
input is dropped with `material-too-large', the Run fails, nothing is sent, and
no attachment row is written.  The material's fate is on the Tape.
Fixes the registered issue `.scratch/oversized-injection/issues/01-...'."
  (m3r-fixture '((:text "answer"))
    (let ((huge (make-string (* 2 superchat-tape-max-inline-bytes) ?y)))
      (m3r-with-gate-result '("M3R_GROWS")
          (lambda (claimed)
            (mapcar (lambda (material) (plist-put (copy-tree material) :text huge))
                    claimed))
        (let ((events (m3r-events m3r-session)))
          (should (eq 'failed (m3r-status run)))
          (should (equal '("material-too-large")
                         (mapcar (lambda (drop) (plist-get drop :reason))
                                 (m3r-drops m3r-session))))
          (should (equal (car ids)
                         (plist-get (car (m3r-drops m3r-session)) :queue-id)))
          (should-not (m3r-attachments m3r-session run))
          (should-not (m3r-of-type "step.started" events))
          (should (= 0 (length smoke--requests)))
          (should (= 0 (m3r-pending))))))))

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

;; M3 closing ruling 5: middleware receives no cancellation context; after
;; cancellation Runtime's execution boundaries stop the result.  These pin the
;; guarantee, not middleware stopping its own work.
(ert-deftest m3/pre-step/cancelling-inside-the-gate-opens-no-step-and-sends-nothing ()
  (m3r-fixture '((:text "must not be used"))
    (let* ((run (m3r-listening
                    ((nil 'pre-step 'm3r-cancel-inside
                          (lambda (proposal next)
                            (superchat-runtime-cancel (plist-get proposal :run-id)
                                                      "[m3r cancel inside pre-step]")
                            (funcall next proposal))))
                  (car (m3r-run-tolerating-errors "cancel inside the gate"))))
           (events (m3r-events m3r-session run)))
      (should (eq 'cancelled (m3r-status run)))
      (should-not smoke--requests)
      (should-not (m3r-of-type "step.started" events))
      (should (= 1 (m3r-count "run.cancelled" events)))
      (should-not (m3r-of-type "run.completed" events))
      (should-not (m3r-of-type "run.failed" events)))))

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

(defun m3r-cancel-inside-request-middleware ()
  "Run one Turn whose request middleware cancels it and then delegates.
Returns (RUN-ID . CONDITION-SIGNALLED-TO-THE-SENDER)."
  (m3r-listening
      ((nil 'before-provider-request 'm3r-cancel-inside
            (lambda (candidate next)
              (superchat-runtime-cancel (car (superchat-runtime-active-runs))
                                        "[m3r cancel inside request middleware]")
              (funcall next candidate))))
    (m3r-run-tolerating-errors "cancel inside request middleware")))

(ert-deftest m3/request/cancelling-inside-request-middleware-sends-nothing ()
  "Closing ruling 5: after cancellation the result enters no model transport,
the entered Step closes `cancelled', and the Run keeps its single `cancelled'
terminal."
  (m3r-fixture '((:text "must not be used"))
    (let* ((run (car (m3r-cancel-inside-request-middleware)))
           (events (m3r-events m3r-session run)))
      (should (stringp run))
      (should (eq 'cancelled (m3r-status run)))
      (should-not smoke--requests)
      (should (equal '("cancelled") (m3r-step-dispositions m3r-session run)))
      (should (= 1 (m3r-count "run.cancelled" events)))
      (should-not (m3r-of-type "run.completed" events))
      (should-not (m3r-of-type "run.failed" events)))))

(ert-deftest m3/request/cancelling-inside-request-middleware-is-not-reported-as-an-error ()
  "When request middleware cancels the current Run, the submission ends through
the existing cancellation path: the single `cancelled' terminal, no new failure
terminal, no resend, and no generic error.  The send boundary reports an
ordinary cancellation only for this Run's own cancellation with its execution
Scope still live and its recorded transport provider still current, so a
withdrawn transport, a missing provider or a retired Scope is still an error."
  (m3r-fixture '((:text "must not be used"))
    (let* ((outcome (m3r-cancel-inside-request-middleware))
           (run (car outcome)))
      (should (eq 'cancelled (m3r-status run)))
      (should-not smoke--requests)
      (should-not (cdr outcome)))))

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
  '((superchat--hook-record-user-message . record-user-message))
  "The `chat-turn-recorder' listeners: their function and their listener id.
`record-attachments' left this list on 2026-09-16: attachment facts are written
when Runtime admits the request, not when the Run seals, so a failure there is
pinned by `m3/admission/an-unrecordable-attachment-is-refused-before-it-is-sent'
instead.  A row kept in the product only to satisfy this list would be a stub
proving nothing.")

(ert-deftest m3/run-sealing/a-failed-turn-recording-seals-audit-incomplete ()
  "When a turn recorder signals while the Run is about to seal `completed', the
Run seals `audit-incomplete' through the existing `run.failed' fact (M3 closing
ruling 1).  The answer was delivered, so `response.completed' and the Step's
`completed' ending stay as recorded."
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

(ert-deftest m3/admission/an-unrecordable-attachment-is-refused-before-it-is-sent ()
  "When the attachment fact cannot be written, the Turn is refused while it is
being admitted: nothing reaches the provider, the Run seals `audit-incomplete'
through the existing `run.failed' fact, and no Run is left open.  Recording that
material was sent now happens before sending it, so the pre-2026-09-16 outcome --
the image reaches the model and the seal reports the audit gap afterwards -- can
no longer happen."
  (m3r-fixture '((:text "must not be sent"))
    (let* ((png (m3r-tiny-png))
           (view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (original (symbol-function 'superchat-tape-append-event))
           (signalled nil))
      (with-current-buffer view
        (setq-local superchat--current-context-files (list png)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event &rest rest)
                   (if (equal "attachment.added" (plist-get event :type))
                       (error "m3r attachment append fails")
                     (apply original event rest)))))
        (setq signalled (should-error (smoke--send view "look at this"))))
      (should (string-match-p "attachment append fails"
                              (error-message-string signalled)))
      ;; Refused before the model, not after it.
      (should (= 0 (length smoke--requests)))
      (should-not (superchat-runtime-active-runs))
      (let ((events (m3r-events session)))
        (should-not (m3r-of-type "response.completed" events))
        (should (= 1 (m3r-count "run.failed" events)))
        (should (equal "audit-incomplete"
                       (m3r-payload (car (m3r-of-type "run.failed" events))
                                    :status)))))))

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
  "No Stage name survives M3 (closing ruling 2): the Event is `context-error',
fault records carry `:site', and the substrate is the feature `superchat-event'
in `superchat-event.el'."
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
          (m3r-listening ((nil 'context-error 'm3r-observer (lambda (&rest _) (cl-incf errors))))
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

;;;; M3a: sealing, reopen, consumers, evaluation, removals ---------------------
;;
;; Written before the M3a implementation.  A test marked `:expected-result
;; :failed' pins a gap between the tree the tests were written on (b1f03670)
;; and the M3a record; its docstring names the gap, and it was checked to fail
;; at that assertion.  It loses the marker when the implementation lands.

(defun m3a-load (&rest features)
  "Load FEATURES inside a test, so the M3 tests above run with the usual load state."
  (dolist (feature features)
    (require feature)))

(defconst m3a-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "The repository root, for the source scan.")

(defconst m3a-effects '((:effect local-read :resource buffer))
  "An Effect class that asks for approval in an attended Run.")

(defconst m3a-endings '(completed failed timed-out cancelled)
  "The endings a Run can ask for.")

(defconst m3a-unsuccessful-endings '(audit-incomplete failed timed-out cancelled abandoned)
  "Every recorded ending other than `completed'.  Each test that loops over it
makes its fixture really end the Run that way.")

(defconst m3a-irregular-records '(foreign-cause result-without-start)
  "Rows a Run's record may hold that Closure's judge could not interpret.")

(defun m3a-run-terminals (events)
  (cl-remove-if-not
   (lambda (e) (member (m3r-type e) '("run.completed" "run.failed" "run.cancelled")))
   events))

(defmacro m3a-without-closure-guard (&rest body)
  "Run BODY with Closure's terminal guard off wherever that guard still exists.
Closure's guard used to be what sealed a Run with open work `audit-incomplete';
M3a made that Runtime's own accounting, and the guard variable is unbound, so
this binds nothing today.  It is kept so a re-introduced guard could not stand
in for Runtime's accounting again."
  (declare (indent 0) (debug t))
  `(cl-progv (and (boundp 'superchat-closure-terminal-guard-enabled)
                  '(superchat-closure-terminal-guard-enabled))
       '(nil)
     ,@body))

(defmacro m3a-with-tools (tools &rest body)
  "Register TOOLS, each (NAME . ARGS) for `superchat-register-tool', around BODY."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn
         ,@(mapcar (lambda (tool) `(superchat-register-tool ,@tool)) tools)
         ,@body)
     ,@(mapcar (lambda (tool) `(ignore-errors (superchat-unregister-tool ,(car tool))))
               tools)))

(defun m3a-deadline (ending)
  "A Runtime deadline that ends the Run as `timed-out' when ENDING asks for it."
  (and (eq ending 'timed-out) 1))

(defun m3a-end (run ending)
  "Ask RUN to end with ENDING.  `timed-out' comes from the deadline armed at submit."
  (pcase ending
    ('cancelled (superchat-runtime-cancel run "[m3a cancel]"))
    ('timed-out nil)
    (_ (superchat-runtime--finish run ending "[m3a ending]"))))

(defun m3a-forget-live-runs ()
  "Drop the process-local Run registry, as a restarted Emacs has none."
  (clrhash superchat-runtime--runs))

(defun m3a-reopen (session)
  "Open SESSION as a user does after a restart, then close its View again.
The View must be rebuilt, so a later reopen repeats the whole path.  It starts
from a scratch buffer: killing a View makes an unrelated buffer current, whose
directory may belong to an earlier fixture that no longer exists."
  (let ((name (superchat--session-buffer-name session)))
    (save-current-buffer
      (let ((default-directory temporary-file-directory))
        (with-temp-buffer
          (should-not (get-buffer name))
          (superchat-open-session session)
          (should (get-buffer name))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer name)))))))

(defun m3a-append (session run type payload &optional causation-id)
  "Append a TYPE row with RUN's identity, as a writer other than Runtime would."
  (let* ((started (car (m3r-of-type "run.started" (m3r-events session run))))
         (event (list :type type :session-id session
                      :turn-id (plist-get started :turn-id) :run-id run
                      :actor-id (plist-get started :actor-id)
                      :summary (format "m3a %s" type) :payload payload)))
    (when causation-id
      (setq event (plist-put event :causation-id causation-id)))
    (should (plist-get (superchat-tape-append-event-record event) :event-id))))

;;;; M3a sealing

(ert-deftest m3a/seal/an-open-effect-seals-audit-incomplete-whatever-the-requested-ending ()
  "A tool Effect with `tool.execution.started' and no settlement when the Run
ends makes the Run `audit-incomplete', whether it was completing, failing,
timing out or cancelled; `run.failed' and `turn.completed' say so, and no
settlement is invented.  Runtime's own accounting of open Effects decides this,
so Closure's guard is switched off where it still exists."
  (dolist (ending m3a-endings)
    (ert-info ((format "requested ending: %s" ending))
      (m3r-fixture '((:tool-uses (("m3a-pending"))) (:text "must not follow"))
        (m3a-without-closure-guard
          (let ((callbacks nil)
                (superchat-runtime-default-deadline (m3a-deadline ending)))
            (m3a-with-tools (("m3a-pending" :async t
                              :function (lambda (callback) (push callback callbacks) nil)
                              :description "m3a pending Effect" :effects m3a-effects))
              (let ((run (plist-get (m3r-submit "go" :tools '("m3a-pending")) :run-id)))
                (should (smoke--wait (lambda () callbacks) 10))
                (m3a-end run ending)
                (m3r-settle run)
                (let* ((events (m3r-events m3r-session run))
                       (terminals (m3a-run-terminals events)))
                  (should (= 1 (m3r-count "tool.execution.started" events)))
                  (should-not (m3r-of-type "tool.completed" events))
                  (should-not (m3r-of-type "tool.failed" events))
                  (should (= 1 (length terminals)))
                  (should (= 1 (m3r-count "turn.completed" events)))
                  (should (eq 'audit-incomplete (m3r-status run)))
                  (should (equal '("run.failed") (m3r-types terminals)))
                  (should (equal "audit-incomplete" (m3r-payload (car terminals) :status)))
                  (should (equal "audit-incomplete"
                                 (m3r-payload (car (m3r-of-type "turn.completed" events))
                                              :status))))))))))))

(ert-deftest m3a/seal/an-undecided-approval-seals-audit-incomplete-whatever-the-requested-ending ()
  "An Approval with `approval.requested' and no `approval.decided' when the Run
ends makes the Run `audit-incomplete'.  The Run ends while the approval prompt
is still open, as when the user leaves the prompt and the Run is stopped or
times out; the tool body never runs.  Runtime's own accounting of open Approvals
decides this, so Closure's guard is switched off where it still exists."
  (dolist (ending m3a-endings)
    (ert-info ((format "requested ending: %s" ending))
      (m3r-fixture '((:tool-uses (("m3a-asks"))) (:text "must not follow"))
        (m3a-without-closure-guard
          (let* ((ran 0) (run nil) (ended-while-asking nil)
                 (superchat-runtime-default-deadline (m3a-deadline ending))
                 (superchat-approval-function
                  (lambda (_request)
                    (if (eq ending 'timed-out)
                        (sleep-for 1.5)
                      (m3a-end run ending))
                    (setq ended-while-asking
                          (superchat-runtime--terminal-p (m3r-status run)))
                    t)))
            (m3a-with-tools (("m3a-asks" :function (lambda () (cl-incf ran) "ran")
                              :description "m3a approval" :effects m3a-effects))
              (setq run (plist-get (m3r-submit "go" :tools '("m3a-asks")) :run-id))
              (m3r-settle run)
              (let ((events (m3r-events m3r-session run)))
                (should ended-while-asking)
                (should (= 0 ran))
                (should (= 1 (m3r-count "approval.requested" events)))
                (should-not (m3r-of-type "tool.execution.started" events))
                (should (= 1 (length (m3a-run-terminals events))))
                (should (eq 'audit-incomplete (m3r-status run)))
                (should (equal "audit-incomplete"
                               (m3r-payload (car (m3a-run-terminals events)) :status)))))))))))

(ert-deftest m3a/seal/settled-effects-and-approvals-keep-the-requested-ending ()
  "When every entered Effect has settled and every Approval is decided, the Run
keeps the ending it asked for: `completed', `failed', `timed-out' or `cancelled'."
  (dolist (ending m3a-endings)
    (ert-info ((format "requested ending: %s" ending))
      (m3r-fixture '((:tool-uses (("m3a-pending"))) (:hang t))
        (let ((callbacks nil)
              (superchat-runtime-default-deadline (m3a-deadline ending)))
          (m3a-with-tools (("m3a-pending" :async t
                            :function (lambda (callback) (push callback callbacks) nil)
                            :description "m3a pending Effect" :effects m3a-effects))
            (let ((run (plist-get (m3r-submit "go" :tools '("m3a-pending")) :run-id)))
              (should (smoke--wait (lambda () callbacks) 10))
              (funcall (car callbacks) "settled")
              (should (smoke--wait
                       (lambda () (= 2 (m3r-count "step.started" (m3r-events m3r-session run))))
                       10))
              (m3a-end run ending)
              (m3r-settle run)
              (let ((events (m3r-events m3r-session run)))
                (should (= 1 (m3r-count "approval.decided" events)))
                (should (= 1 (m3r-count "tool.completed" events)))
                (should (= 1 (length (m3a-run-terminals events))))
                (should (eq ending (m3r-status run)))))))))))

(ert-deftest m3a/seal/one-open-effect-beside-a-settled-one-still-seals-audit-incomplete ()
  "Settling part of a Run's work is not enough: with two Effects entered and one
settled, a cancelled Run seals `audit-incomplete'.  Closure's guard is switched
off where it still exists."
  (m3r-fixture '((:tool-uses (("m3a-first") ("m3a-second"))) (:hang t))
    (m3a-without-closure-guard
      (let ((callbacks nil))
        (m3a-with-tools (("m3a-first" :async t
                          :function (lambda (callback) (push (cons 'first callback) callbacks) nil)
                          :description "m3a first Effect" :effects m3a-effects)
                         ("m3a-second" :async t
                          :function (lambda (callback) (push (cons 'second callback) callbacks) nil)
                          :description "m3a second Effect" :effects m3a-effects))
          (let ((run (plist-get (m3r-submit "go" :tools '("m3a-first" "m3a-second")) :run-id)))
            (should (smoke--wait (lambda () (= 2 (length callbacks))) 10))
            (funcall (cdr (assq 'first callbacks)) "settled")
            (superchat-runtime-cancel run "[m3a cancel]")
            (m3r-settle run)
            (let ((events (m3r-events m3r-session run)))
              (should (= 2 (m3r-count "tool.execution.started" events)))
              (should (= 1 (m3r-count "tool.completed" events)))
              (should (eq 'audit-incomplete (m3r-status run))))))))))

(ert-deftest m3a/seal/an-undecided-approval-beside-a-settled-effect-still-seals-audit-incomplete ()
  "With one Effect settled and an Approval still undecided, a Run cancelled from
the approval prompt seals `audit-incomplete'.  Closure's guard is switched off
where it still exists."
  (m3r-fixture '((:tool-uses (("m3a-first") ("m3a-asks"))) (:hang t))
    (m3a-without-closure-guard
      (let* ((callbacks nil) (run nil) (ran 0)
             (superchat-approval-function
              (lambda (request)
                (when (equal "m3a-asks" (plist-get request :tool-name))
                  (funcall (cdr (assq 'first callbacks)) "settled")
                  (superchat-runtime-cancel run "[m3a cancel]"))
                t)))
        (m3a-with-tools (("m3a-first" :async t
                          :function (lambda (callback) (push (cons 'first callback) callbacks) nil)
                          :description "m3a first Effect" :effects m3a-effects)
                         ("m3a-asks" :function (lambda () (cl-incf ran) "ran")
                          :description "m3a approval" :effects m3a-effects))
          (setq run (plist-get (m3r-submit "go" :tools '("m3a-first" "m3a-asks")) :run-id))
          (m3r-settle run)
          (let ((events (m3r-events m3r-session run)))
            (should (= 0 ran))
            (should (= 1 (m3r-count "tool.completed" events)))
            (should (= 2 (m3r-count "approval.requested" events)))
            (should (eq 'audit-incomplete (m3r-status run)))))))))

(ert-deftest m3a/seal/a-late-result-is-recorded-once-and-does-not-reopen-the-run ()
  "After a Run sealed `audit-incomplete' over an open Effect, the Effect's real
result is appended once after the seal, a repeated callback adds nothing, and
the Run stays sealed: one Run terminal, one Turn seal, no new provider request,
no Effect capability."
  (m3r-fixture '((:tool-uses (("m3a-pending"))) (:text "must not follow"))
    (let ((callbacks nil))
      (m3a-with-tools (("m3a-pending" :async t
                        :function (lambda (callback) (push callback callbacks) nil)
                        :description "m3a pending Effect" :effects m3a-effects))
        (let ((run (plist-get (m3r-submit "go" :tools '("m3a-pending")) :run-id)))
          (should (smoke--wait (lambda () callbacks) 10))
          (superchat-runtime-cancel run "[m3a cancel]")
          (m3r-settle run)
          (let ((requests (length smoke--requests)))
            (funcall (car callbacks) "real late result")
            (funcall (car callbacks) "repeated late result")
            (should (smoke--wait
                     (lambda () (m3r-of-type "tool.completed" (m3r-events m3r-session run)))
                     5))
            ;; Give a timer-driven duplicate the chance to land.
            (smoke--wait #'ignore 0.3)
            (let* ((events (m3r-events m3r-session run))
                   (terminals (m3a-run-terminals events)))
              (should (= 1 (m3r-count "tool.completed" events)))
              (should (= 1 (length terminals)))
              (should (< (cl-position (car terminals) events)
                         (m3r-index "tool.completed" events)))
              (should (= 1 (m3r-count "turn.completed" events)))
              (should (= requests (length smoke--requests)))
              (should-not (superchat-runtime-effect-capable-p run))
              (should (eq 'audit-incomplete (m3r-status run))))))))))

;;;; M3a reopen

(ert-deftest m3a/reopen/a-cut-off-run-gets-one-abandoned-pair-and-nothing-else ()
  "Reopening a Session whose Run was cut off by a restart seals that Run with the
`abandoned' pair, `run.failed' then `turn.completed'.  The Effect it waited on
gets no invented result and its open Step no invented `step.ended'.  Reopening
again adds nothing."
  (m3r-fixture '((:tool-uses (("m3a-pending"))) (:text "never"))
    (let ((callbacks nil))
      (m3a-with-tools (("m3a-pending" :async t
                        :function (lambda (callback) (push callback callbacks) nil)
                        :description "m3a pending Effect" :effects m3a-effects))
        (let ((run (plist-get (m3r-submit "go" :tools '("m3a-pending")) :run-id)))
          (should (smoke--wait (lambda () callbacks) 10))
          (m3a-forget-live-runs)
          (m3a-reopen m3r-session)
          (m3a-reopen m3r-session)
          (let* ((events (m3r-events m3r-session run))
                 (terminals (m3a-run-terminals events))
                 (turn-seals (m3r-of-type "turn.completed" events)))
            (should (equal '("run.failed") (m3r-types terminals)))
            (should (equal "abandoned" (m3r-payload (car terminals) :status)))
            (should (= 1 (length turn-seals)))
            (should (equal "abandoned" (m3r-payload (car turn-seals) :status)))
            (should (< (cl-position (car terminals) events)
                       (cl-position (car turn-seals) events)))
            (should (= 1 (m3r-count "tool.execution.started" events)))
            (should-not (m3r-of-type "tool.completed" events))
            (should-not (m3r-of-type "tool.failed" events))
            (should (= 1 (m3r-count "step.started" events)))
            (should-not (m3r-of-type "step.ended" events))))))))

(ert-deftest m3a/reopen/a-live-run-is-not-repaired ()
  "Opening a Session while its Run is still working in this Emacs writes no seal;
the Run later ends normally."
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (m3a-reopen m3r-session)
      (should (member run (superchat-runtime-active-runs)))
      (should-not (m3a-run-terminals (m3r-events m3r-session run)))
      (should-not (m3r-of-type "turn.completed" (m3r-events m3r-session run)))
      (superchat-runtime-cancel run "[m3a cleanup]")
      (m3r-settle run)
      (should (eq 'cancelled (m3r-status run)))
      (should (= 1 (length (m3a-run-terminals (m3r-events m3r-session run))))))))

(ert-deftest m3a/reopen/a-half-written-abandoned-pair-gets-only-its-turn-seal ()
  "A restart that fell between the repair's `run.failed' and its `turn.completed'
is finished on the next reopen: only the Turn seal is added, once."
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (m3a-forget-live-runs)
      (m3a-append m3r-session run "run.failed" '(:status abandoned :abandoned t))
      (m3a-reopen m3r-session)
      (m3a-reopen m3r-session)
      (let ((events (m3r-events m3r-session run)))
        (should (= 1 (length (m3a-run-terminals events))))
        (should (= 1 (m3r-count "turn.completed" events)))
        (should (equal "abandoned"
                       (m3r-payload (car (m3r-of-type "turn.completed" events)) :status)))))))

(ert-deftest m3a/reopen/repair-does-not-judge-the-record ()
  "Repair needs only `run.started' without a Run terminal.  A cut-off Run whose
record also holds an out-of-order fact, here a tool result with no recorded
start, is sealed `abandoned' like any other: repair reads the recorded Run and
Turn terminals, and judges nothing else."
  (m3r-fixture '((:hang t))
    (let ((run (m3r-live-run "hold")))
      (m3a-forget-live-runs)
      (m3a-append m3r-session run "tool.completed"
                  '(:tool-name "m3a" :execution-id "m3a-never-started"))
      (m3a-reopen m3r-session)
      (let ((events (m3r-events m3r-session run)))
        (should (equal '("run.failed") (m3r-types (m3a-run-terminals events))))
        (should (equal "abandoned" (m3r-payload (car (m3a-run-terminals events)) :status)))
        (should (= 1 (m3r-count "turn.completed" events)))))))

;;;; M3a consumers

(defmacro m3a-publishing (var &rest body)
  "Run BODY with tools that publish one Artifact into Workspace `m3a-ws'.
`m3a-publish' publishes and returns; `m3a-publish-and-wait' publishes and never
settles.  VAR holds the published Artifact's metadata."
  (declare (indent 1) (debug t))
  `(let ((,var nil))
     (m3a-load 'superchat-workspace)
     (superchat-workspace-bind "m3a-ws" nil m3r-session)
     (m3a-with-tools (("m3a-publish"
                       :function (lambda ()
                                   (setq ,var (superchat-workspace-publish
                                               "m3a artifact" 'report "m3a-ws"))
                                   "published")
                       :description "m3a Artifact producer" :effects m3a-effects)
                      ("m3a-publish-and-wait" :async t
                       :function (lambda (_callback)
                                   (setq ,var (superchat-workspace-publish
                                               "m3a artifact" 'report "m3a-ws"))
                                   nil)
                       :description "m3a Artifact producer that never settles"
                       :effects m3a-effects))
       ,@body)))

(defun m3a-reference (artifact)
  (list :workspace-id "m3a-ws" :artifact-id (plist-get artifact :artifact-id)
        :revision (plist-get artifact :revision)))

(defun m3a-artifact-decision (session run reference)
  "Artifact delivery's (ACTION REASON) for REFERENCE produced by RUN."
  (m3a-load 'superchat-workspace)
  (let ((decision (superchat-workspace-artifact-delivery-decision session run reference)))
    (list (plist-get decision :action) (plist-get decision :reason))))

(defun m3a-decisions (session run reference)
  "The three consumers' actions for RUN: (SUBAGENT WORKFLOW ARTIFACT)."
  (m3a-load 'superchat-subagent 'superchat-workflow)
  (list (plist-get (superchat-subagent-handoff-decision session run) :action)
        (plist-get (superchat-workflow--resume-decision session run) :action)
        (car (m3a-artifact-decision session run reference))))

(defun m3a-wait-second-step (run)
  (should (smoke--wait
           (lambda () (= 2 (m3r-count "step.started" (m3r-events m3r-session run))))
           10)))

(ert-deftest m3a/consumer/a-run-without-a-terminal-is-waited-for ()
  "Before a Run has a terminal on the Tape all three consumers answer `wait'."
  (m3r-fixture '((:tool-uses (("m3a-publish"))) (:hang t))
    (m3a-publishing artifact
      (let ((run (plist-get (m3r-submit "go" :tools '("m3a-publish")) :run-id)))
        (m3a-wait-second-step run)
        (should artifact)
        (should (equal '(wait wait wait)
                       (m3a-decisions m3r-session run (m3a-reference artifact))))
        (superchat-runtime-cancel run "[m3a cleanup]")
        (m3r-settle run)))))

(ert-deftest m3a/consumer/a-completed-run-is-delivered-after-the-artifact-checks ()
  "`run.completed' with status `completed' delivers a subagent report and advances
a workflow.  An Artifact is delivered only after its own checks, so a stale
revision is still rejected."
  (m3r-fixture '((:tool-uses (("m3a-publish"))) (:text "done"))
    (m3a-publishing artifact
      (let* ((run (m3r-run "go" :tools '("m3a-publish")))
             (reference (m3a-reference artifact)))
        (should (eq 'completed (m3r-status run)))
        (should (equal '(deliver advance deliver)
                       (m3a-decisions m3r-session run reference)))
        (should (equal '(reject artifact-revision-stale)
                       (m3a-artifact-decision
                        m3r-session run
                        (plist-put (copy-sequence reference) :revision "m3a-stale"))))))))

(ert-deftest m3a/consumer/every-other-ending-is-rejected ()
  "After `audit-incomplete', `failed', `timed-out', `cancelled' or `abandoned',
the subagent report is rejected, the workflow stops and the Artifact is rejected."
  (dolist (ending m3a-unsuccessful-endings)
    (ert-info ((format "ending: %s" ending))
      (m3r-fixture (pcase ending
                     ('failed '((:tool-uses (("m3a-publish")))
                                (:error (error . "m3a provider down"))))
                     ('audit-incomplete '((:tool-uses (("m3a-publish-and-wait")))
                                          (:text "must not follow")))
                     (_ '((:tool-uses (("m3a-publish"))) (:hang t))))
        (m3a-publishing artifact
          (let ((run (let ((superchat-runtime-default-deadline (m3a-deadline ending)))
                       (plist-get (m3r-submit "go" :tools '("m3a-publish" "m3a-publish-and-wait"))
                                  :run-id))))
            (pcase ending
              ('failed (m3r-settle run))
              ('timed-out
               ;; The Artifact is published before the deadline ends the Run.
               (should (smoke--wait (lambda () artifact) 10))
               (m3r-settle run))
              ('audit-incomplete
               (should (smoke--wait (lambda () artifact) 10))
               (superchat-runtime-cancel run "[m3a cancel]")
               (m3r-settle run))
              ('cancelled
               (m3a-wait-second-step run)
               (superchat-runtime-cancel run "[m3a cancel]")
               (m3r-settle run))
              ('abandoned
               (m3a-wait-second-step run)
               (m3a-forget-live-runs)
               (m3a-reopen m3r-session)))
            (should artifact)
            (should (equal (symbol-name ending)
                           (m3r-payload (car (m3a-run-terminals (m3r-events m3r-session run)))
                                        :status)))
            (should (equal '(reject stop reject)
                           (m3a-decisions m3r-session run (m3a-reference artifact))))))))))

(ert-deftest m3a/consumer/an-artifact-of-unknown-provenance-still-needs-manual-review ()
  "Artifact delivery keeps its own `manual-review': an Artifact published outside
any Run names no producing Run, even when the Run asked about completed."
  (m3r-fixture '((:text "done"))
    (m3a-load 'superchat-workspace)
    (superchat-workspace-bind "m3a-ws" nil m3r-session)
    (let* ((run (m3r-run "go"))
           (artifact (superchat-workspace-publish "published outside a Run" 'report "m3a-ws")))
      (should (eq 'completed (m3r-status run)))
      (should-not (plist-get artifact :run-id))
      (should (equal '(manual-review artifact-provenance-unknown)
                     (m3a-artifact-decision m3r-session run (m3a-reference artifact)))))))

(ert-deftest m3a/consumer/decisions-read-only-the-recorded-terminal ()
  "The consumers decide from the Run terminal on the Tape and nothing else.  A
completed Run stays deliverable when its record also holds a row citing a cause
outside this Session, or a tool result with no recorded start; subagent handoff
and workflow resume never answer `manual-review'."
  (dolist (irregular m3a-irregular-records)
    (ert-info ((format "the record also holds: %s" irregular))
      (m3r-fixture '((:tool-uses (("m3a-publish"))) (:text "done"))
        (m3a-publishing artifact
          (let ((run (m3r-run "go" :tools '("m3a-publish"))))
            (should (eq 'completed (m3r-status run)))
            (if (eq irregular 'foreign-cause)
                (m3a-append m3r-session run "delivery.failed" '(:reason "m3a")
                            "evt_recorded_in_another_session")
              (m3a-append m3r-session run "tool.completed"
                          '(:tool-name "m3a" :execution-id "m3a-never-started")))
            (let ((decisions (m3a-decisions m3r-session run (m3a-reference artifact))))
              (should-not (memq 'manual-review (butlast decisions)))
              (should (equal '(deliver advance deliver) decisions)))))))))

(ert-deftest m3a/consumer/an-unreadable-record-is-refused-not-waited-for ()
  "When the Tape cannot be read, a consumer decides against delivery instead of
signalling or waiting: subagent handoff answers `reject' and workflow resume
`stop', each with a reason, while Artifact delivery keeps its own
`manual-review'.  Nothing answers `wait', which would claim the Run is still
working."
  (m3r-fixture '((:tool-uses (("m3a-publish"))) (:text "done"))
    (m3a-publishing artifact
      (let ((run (m3r-run "go" :tools '("m3a-publish"))))
        (should (eq 'completed (m3r-status run)))
        (cl-letf (((symbol-function 'superchat-tape-replay-events)
                   (lambda (&rest _) (error "m3a: the Tape cannot be read"))))
          (let ((subagent (superchat-subagent-handoff-decision m3r-session run))
                (workflow (superchat-workflow--resume-decision m3r-session run))
                (delivery (superchat-workspace-artifact-delivery-decision
                           m3r-session run (m3a-reference artifact))))
            (should (eq 'reject (plist-get subagent :action)))
            (should (plist-get subagent :reason))
            (should (eq 'stop (plist-get workflow :action)))
            (should (plist-get workflow :reason))
            (should (eq 'manual-review (plist-get delivery :action)))
            (should (plist-get delivery :reason))))))))

(ert-deftest m3a/consumer/two-recorded-terminals-are-refused-not-resolved ()
  "A Run whose record holds two Run terminals is not resolved by picking one of
them: the subagent report is rejected, the workflow stops and the Artifact is
rejected, even though the first terminal is a `completed' one."
  (m3r-fixture '((:tool-uses (("m3a-publish"))) (:text "done"))
    (m3a-publishing artifact
      (let ((run (m3r-run "go" :tools '("m3a-publish"))))
        (should (eq 'completed (m3r-status run)))
        (m3a-append m3r-session run "run.cancelled" '(:status cancelled))
        (should (equal '("run.completed" "run.cancelled")
                       (m3r-types (m3a-run-terminals (m3r-events m3r-session run)))))
        (let ((decisions (m3a-decisions m3r-session run (m3a-reference artifact))))
          (should-not (memq 'wait decisions))
          (should-not (memq 'deliver decisions))
          (should-not (memq 'advance decisions))
          (should (equal '(reject stop reject) decisions)))))))

;;;; M3a evaluation

(defun m3a-submit-with-evidence (text)
  "Submit TEXT under the exact-evidence contract, which records Decision snapshots.
Return the run id."
  (let ((id (plist-get
             (superchat-service-call
              nil 'driver 'submit
              (list :surface 'external :session-id m3r-session :tools 'none
                    :attendance 'attended :content text
                    :context (list :host-buffer (get-buffer-create " *m3r-host*"))
                    :runtime-extras
                    (list :record-conversation nil
                          :evidence-contract "epistemic-learning-v0"
                          :context-envelope
                          (list :contract "context-continuity-v0" :status 'empty :prompt ""
                                :receipt (list :selected nil :represented nil)))
                    :sink (superchat-sink-discard)))
             :run-id)))
    (should (stringp id))
    id))

(defun m3a-evidence-script (ending)
  (pcase ending
    ((or 'completed 'audit-incomplete) '((:text "answer")))
    ('failed '((:error (error . "m3a provider down"))))
    (_ '((:hang t)))))

(defun m3a-evidence-run (ending)
  "Run one tool-free Turn with Decision snapshots and end it with ENDING.
`audit-incomplete' comes from a `run.completed' seal that cannot be written,
`timed-out' from the Runtime deadline while the model has not answered, and
`abandoned' from a restart and a reopen."
  (pcase ending
    ('audit-incomplete
     (let ((append (symbol-function 'superchat-tape-append-event-record)))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event &rest rest)
                    (if (equal "run.completed" (plist-get event :type))
                        (error "m3a: the completed seal cannot be written")
                      (apply append event rest)))))
         (m3r-settle (m3a-submit-with-evidence "go")))))
    ((or 'completed 'failed) (m3r-settle (m3a-submit-with-evidence "go")))
    (_ (let ((run (let ((superchat-runtime-default-deadline (m3a-deadline ending)))
                    (m3a-submit-with-evidence "go"))))
         (should (smoke--wait
                  (lambda () (m3r-of-type "step.started" (m3r-events m3r-session run)))
                  10))
         (pcase ending
           ('timed-out (m3r-settle run))
           ('cancelled (superchat-runtime-cancel run "[m3a cancel]")
                       (m3r-settle run))
           (_ (m3a-forget-live-runs)
              (m3a-reopen m3r-session)))
         run))))

(defun m3a-ref (event)
  (list :session-id (plist-get event :session-id) :event-id (plist-get event :event-id)))

(defun m3a-evaluation-spec (session run)
  "An evaluation of RUN citing its latest Decision snapshot and its Run terminal."
  (m3a-load 'superchat-experience)
  (let* ((events (m3r-events session run))
         (decision (car (last (m3r-of-type "decision.snapshot.recorded" events))))
         (terminal (car (m3a-run-terminals events))))
    (should decision)
    (should terminal)
    (list :session-id session :actor-id "m3a-evaluator" :exercise-id "m3a-exercise"
          :scope "m3a" :run-id run :decision (m3a-ref decision) :terminal (m3a-ref terminal)
          :evaluator '(:id "m3a" :version 1)
          :evaluator-manifest-sha256 (concat "sha256:" (make-string 64 ?a))
          :verdict "passed" :observations '(:note "m3a") :metrics '(:score 1)
          :evidence (list (m3a-ref terminal)))))

(defun m3a-write-evaluation-row (session run)
  "Write an evaluation row for RUN straight to the Tape, as an earlier version could."
  (let* ((spec (m3a-evaluation-spec session run))
         (terminal (car (m3a-run-terminals (m3r-events session run)))))
    (superchat-tape-append-event-record
     (list :type "exercise.evaluated" :schema 1 :session-id session
           :turn-id (plist-get terminal :turn-id) :run-id run :actor-id "m3a-evaluator"
           :causation-id (plist-get terminal :event-id)
           :topic "learning/evaluation" :summary "exercise.evaluated"
           :sensitivity "confidential" :requires-payload t
           :payload (list :exercise-id (plist-get spec :exercise-id)
                          :scope (plist-get spec :scope) :run-id run
                          :decision (plist-get spec :decision)
                          :terminal (plist-get spec :terminal)
                          :evaluator (plist-get spec :evaluator)
                          :evaluator-manifest-sha256 (plist-get spec :evaluator-manifest-sha256)
                          :verdict (plist-get spec :verdict)
                          :observations (plist-get spec :observations)
                          :metrics (plist-get spec :metrics)
                          :evidence (vconcat (plist-get spec :evidence)))))))

(defun m3a-invalid-evaluations (session)
  "Event ids of SESSION's evaluation rows the applicability graph refuses."
  (m3a-load 'superchat-experience)
  (delq nil
        (mapcar (lambda (issue)
                  (and (memq (plist-get issue :reason)
                             '(invalid-evaluation-contract malformed-evaluation))
                       (plist-get issue :event-id)))
                (plist-get (superchat-experience-applicability-graph (list session) "m3a")
                           :missing))))

(ert-deftest m3a/experience/a-completed-run-can-be-evaluated ()
  "An evaluation of a Run sealed `run.completed' with status `completed' and a
`turn.completed' for its Turn is recorded and reads back as valid."
  (m3a-load 'superchat-experience)
  (m3r-fixture (m3a-evidence-script 'completed)
    (let* ((run (m3a-evidence-run 'completed))
           (record (superchat-experience-record-evaluation
                    (m3a-evaluation-spec m3r-session run))))
      (should (eq 'completed (m3r-status run)))
      (should (plist-get record :event-id))
      (should-not (member (plist-get record :event-id) (m3a-invalid-evaluations m3r-session))))))

(ert-deftest m3a/experience/only-a-completed-run-can-be-evaluated ()
  "Recording an evaluation is refused for a Run that ended `audit-incomplete',
`failed', `timed-out', `cancelled' or `abandoned': only the recorded
`run.completed' with status `completed' can be cited."
  (m3a-load 'superchat-experience)
  (dolist (ending m3a-unsuccessful-endings)
    (ert-info ((format "ending: %s" ending))
      (m3r-fixture (m3a-evidence-script ending)
        (let ((run (m3a-evidence-run ending)))
          (should (equal (symbol-name ending)
                         (m3r-payload (car (m3a-run-terminals (m3r-events m3r-session run)))
                                      :status)))
          (should-error (superchat-experience-record-evaluation
                         (m3a-evaluation-spec m3r-session run)))
          (should-not (m3r-of-type "exercise.evaluated" (m3r-events m3r-session))))))))

(ert-deftest m3a/experience/an-evaluation-row-for-an-unsuccessful-run-is-not-evidence ()
  "An evaluation row already on the Tape reads back as invalid when its Run ended
`audit-incomplete', `failed', `timed-out', `cancelled' or `abandoned'; the same
row for a completed Run is valid."
  (m3a-load 'superchat-experience)
  (dolist (ending (cons 'completed m3a-unsuccessful-endings))
    (ert-info ((format "ending: %s" ending))
      (m3r-fixture (m3a-evidence-script ending)
        (let* ((run (m3a-evidence-run ending))
               (row (m3a-write-evaluation-row m3r-session run))
               (invalid (m3a-invalid-evaluations m3r-session)))
          (should (equal (symbol-name ending)
                         (m3r-payload (car (m3a-run-terminals (m3r-events m3r-session run)))
                                      :status)))
          (should (plist-get row :event-id))
          (if (eq ending 'completed)
              (should-not (member (plist-get row :event-id) invalid))
            (should (member (plist-get row :event-id) invalid))))))))

(ert-deftest m3a/experience/a-completed-run-without-its-turn-seal-cannot-be-evaluated ()
  "`run.completed' alone is not enough.  A Run whose `turn.completed' was never
written, as when a restart falls between the two seals, cannot be evaluated."
  (m3a-load 'superchat-experience)
  (m3r-fixture '((:hang t))
    (let ((run (m3a-submit-with-evidence "go")))
      (should (smoke--wait
               (lambda () (m3r-of-type "step.started" (m3r-events m3r-session run)))
               10))
      (m3a-forget-live-runs)
      (m3a-append m3r-session run "response.completed" '(:status completed))
      (m3a-append m3r-session run "run.completed" '(:status completed))
      (should-not (m3r-of-type "turn.completed" (m3r-events m3r-session run)))
      (should-error (superchat-experience-record-evaluation
                     (m3a-evaluation-spec m3r-session run))))))

;;;; M3a removals

(defun m3a-default-plugin-names ()
  (mapcar (lambda (row) (if (consp row) (car row) row))
          (eval (car (get 'superchat-plugins 'standard-value)) t)))

(ert-deftest m3a/removal/closure-is-gone ()
  "Closure's library, plugin, default-set row and Service are gone; the driver
injects only `tape' and `model-transport'; subagent no longer reads Closure's
alias of the Tape's subagent suffixes.  Every default plugin still mounts."
  (m3r-fixture nil
    (dolist (name (m3a-default-plugin-names))
      (ert-info ((format "default plugin: %s" name))
        (should (eq 'active (superchat-plugin-state name)))))
    (should (equal '(tape model-transport)
                   (superchat-plugin-inject (superchat-plugin-definition 'driver))))
    (should-not (memq 'closure (m3a-default-plugin-names)))
    (should-not (superchat-plugin-state 'closure)))
  (should-not (assq 'closure superchat-plugin--definitions))
  (should-not (assq 'closure superchat-service--definitions))
  (should-not (featurep 'superchat-closure))
  (should-not (locate-library "superchat-closure"))
  (should-not (fboundp 'superchat-closure-judge))
  (m3a-load 'superchat-subagent)
  (should-not (boundp 'superchat-closure--subagent-event-suffixes)))

(ert-deftest m3a/removal/runtime-has-no-closure-guard-shadow-or-observations ()
  "Runtime's Closure guard and shadow switches, the not-ready error, the
observation ring and the `closure-shadow' Event are gone."
  (should-not (boundp 'superchat-closure-terminal-guard-enabled))
  (should-not (boundp 'superchat-closure-shadow-enabled))
  (should-not (get 'superchat-closure-terminal-not-ready 'error-conditions))
  (should-not (fboundp 'superchat-runtime-closure-observations))
  (should-not (fboundp 'superchat-runtime-clear-closure-observations))
  (should-not (boundp 'superchat-runtime--closure-observations))
  (should-not (assq 'closure-shadow superchat-event--definitions)))

(ert-deftest m3a/removal/the-tape-keeps-no-closure-vocabulary ()
  "Tape event specs carry no `:closure' role and `superchat-tape-event-closure-role'
is gone.  A row of a type the inventory does not know is still imported and
replays with its type and payload, without the `:historical-opaque' mark only
Closure read."
  (m3r-fixture nil
    (superchat-tape-import-event
     (list :type "m3a.legacy.row" :session-id "m3a-legacy" :summary "legacy row"
           :payload '(:note "kept")))
    (let ((row (car (superchat-tape-replay-events "m3a-legacy"))))
      (should (equal "m3a.legacy.row" (plist-get row :type)))
      (should (equal '(:note "kept") (plist-get row :payload)))
      (should-not (plist-member row :historical-opaque))))
  (should-not (fboundp 'superchat-tape-event-closure-role))
  (should-not (cl-some (lambda (spec) (plist-member spec :closure))
                       superchat-tape-event-type-specs)))

(ert-deftest m3a/removal/claims-is-gone ()
  "The Claims library and its two commands are gone."
  (should-not (locate-library "superchat-claims"))
  (should-not (require 'superchat-claims nil t))
  (should-not (fboundp 'superchat-claim-capture))
  (should-not (fboundp 'superchat-claim-correct)))

(ert-deftest m3a/removal/claim-and-feedback-rows-stay-readable ()
  "Sessions recorded with Claims keep their rows: `claim.recorded' and
`feedback.recorded' stay known Tape types and replay with their payloads."
  (m3r-fixture nil
    (dolist (type '("claim.recorded" "feedback.recorded"))
      (should (superchat-tape-event-type-p type))
      (should (plist-get (superchat-tape-append-event-record
                          (list :type type :session-id "m3a-claims" :summary type
                                :payload (list :note type)))
                         :event-id)))
    (should (equal '(("claim.recorded" (:note "claim.recorded"))
                     ("feedback.recorded" (:note "feedback.recorded")))
                   (mapcar (lambda (e) (list (m3r-type e) (plist-get e :payload)))
                           (superchat-tape-replay-events "m3a-claims"))))))

(ert-deftest m3a/removal/experience-has-no-context-collector-or-prompt-slot ()
  "The default collectors no longer include Experience's, its function is gone,
and the prompt has no `experience' slot."
  (m3r-fixture nil
    (should (eq 'active (superchat-plugin-state 'chat-collectors)))
    (should (assq 'history (superchat-event-listeners nil 'collect-context)))
    (should-not (assq 'experience (superchat-event-listeners nil 'collect-context))))
  (should-not (fboundp 'superchat-prompt-collector--experience))
  (should-not (memq 'experience superchat-prompt-slots)))

(ert-deftest m3a/removal/history-does-not-copy-an-experience-view-into-the-context-envelope ()
  "The `history' collector's Context envelope no longer carries the Turn's
`:experience-view', so nothing set on a chat Turn reaches Authority that way."
  (m3r-fixture '((:text "answer"))
    (let ((view (smoke--open-chat))
          (turn nil))
      (m3r-listening
          ((nil 'collect-context 'm3a-experience-view
                (lambda (env _state _tools)
                  (when-let* ((chat-turn (plist-get (plist-get env :context) :chat-turn)))
                    (setq turn chat-turn)
                    (setf (superchat-turn-state chat-turn)
                          (plist-put (superchat-turn-state chat-turn)
                                     :experience-view '(:status satisfied :marker "m3a"))))
                  nil)))
        (m3r-settle (smoke--send view "hello")))
      (should turn)
      (let ((envelope (plist-get (superchat-turn-state turn) :context-envelope)))
        (should envelope)
        (should-not (plist-member envelope :experience-view))))))

(ert-deftest m3a/removal/review-is-gone-and-the-monitor-stays ()
  "The review of recovery needs has no entry left: no `/review' handler, command
or help line, and no `M-x superchat-review'.  The read-only
`M-x superchat-experience-monitor' stays."
  (m3a-load 'superchat-experience)
  (should (commandp 'superchat-experience-monitor))
  (m3r-fixture nil
    (with-current-buffer (smoke--open-chat)
      (should (member "compact" (superchat--commands-list)))
      (should (string-match-p "^/compact " (superchat--list-commands-as-string)))
      (should-not (member "review" (superchat--commands-list)))
      (should-not (string-match-p "^/review " (superchat--list-commands-as-string)))))
  (should-not (assoc "review" superchat--builtin-command-handlers))
  (should-not (fboundp 'superchat-review))
  (should-not (fboundp 'superchat-experience--cmd-review)))

(ert-deftest m3a/removal/recovery-keeps-its-experience-view-and-authority-gate ()
  "Experience stays mounted with its recovery Service, and the request-time
`:experience-view' still reaches Authority: after a tool failure, a `blocked'
recovery View denies a dependent tool on the next attempt without running it.
The View itself is stubbed; what is pinned is Runtime, transport and Authority."
  (m3a-load 'superchat-experience)
  (m3r-fixture '((:tool-uses (("m3a-breaks"))) (:tool-uses (("m3a-dependent"))) (:text "done"))
    (should (eq 'active (superchat-plugin-state 'experience)))
    (should (superchat-service-get nil 'recovery))
    (let ((ran 0) (views 0)
          (superchat-runtime-recovery-intervention-enabled t))
      (m3a-with-tools (("m3a-breaks" :function (lambda () (error "m3a tool broke"))
                        :description "m3a failing tool" :effects m3a-effects)
                       ("m3a-dependent" :function (lambda () (cl-incf ran) "ran")
                        :description "m3a dependent tool" :effects m3a-effects))
        (cl-letf (((symbol-function 'superchat-experience-assemble-recovery-view)
                   (lambda (_failure _sessions)
                     (cl-incf views)
                     (list :contract "experience-view-v0" :status 'blocked :scope "m3a"
                           :experience-action-policy
                           (list :kind "m3a-recovery" :dependent-tools (list "m3a-dependent"))))))
          (let* ((run (m3r-run "go" :tools '("m3a-breaks" "m3a-dependent")))
                 (failures (m3r-of-type "tool.failed" (m3r-events m3r-session run))))
            (should (= 1 views))
            (should (= 0 ran))
            (should (= 2 (length failures)))
            (should (equal "blocked" (m3r-payload (cadr failures) :status)))
            (should (string-match-p "Experience View is blocked"
                                    (m3r-payload (cadr failures) :block-reason)))))))))

(defconst m3a-removed-names
  '("superchat-closure" "superchat-claim" "'closure 'judge" "closure-shadow"
    "superchat-runtime-closure-observations" "superchat-review"
    "superchat-experience--cmd-review" "superchat-prompt-collector--experience")
  "Names that no product or canonical smoke file may still use after M3a.")

(ert-deftest m3a/removal/no-product-or-smoke-file-names-a-removed-part ()
  "No top-level `superchat*.el', plugin file or canonical smoke script still names
a removed part, so nothing calls, requires or binds it."
  (let ((files (append (directory-files m3a-root t "\\`superchat.*\\.el\\'")
                       (directory-files-recursively (expand-file-name "plugins" m3a-root)
                                                    "\\.el\\'")
                       (list (expand-file-name "scripts/smoke.el" m3a-root)
                             (expand-file-name "scripts/smoke-architecture.el" m3a-root))))
        (hits nil))
    (should (member (expand-file-name "superchat-runtime.el" m3a-root) files))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (name m3a-removed-names)
          (goto-char (point-min))
          (when (search-forward name nil t)
            (push (list (file-relative-name file m3a-root) name) hits)))))
    (should-not (nreverse hits))))


;;;; M4: surfaces as rows, composition by profile ------------------------------
;;
;; Written before the M4 implementation, from
;; `docs/decisions/proposed/2026-09-16-m4-surfaces-as-plugins.md'.  A test marked
;; `:expected-result :failed' pins a gap between the tree at 1f3a2324 and that
;; record; each names the promised function or row it is missing, so a red run
;; reports the gap rather than a bare void-function error.

(defconst m4-user-interface-rows
  '(chat chat-commands chat-turn-recorder chat-run-notices
    chat-collectors inline inline-ask inline-rewrite org-babel magit)
  "The rows that register a human entry point or render to a window.
`inline' is today\'s row, which slice S2 replaces with `inline-ask' and
`inline-rewrite'; both spellings are listed so the `headless' profile is
checked against whichever exists.  `chat' joined the list with slice S3.")

(defconst m4-surface-rows
  '((chat . chat) (inline-ask . inline-ask) (inline-rewrite . inline-rewrite)
    (org-babel . org-babel) (magit . magit))
  "The surface rows M4 adds, each with the surface name it must own.")

(defun m4-default-row-names ()
  "The row names of `superchat-plugins' standard value."
  (mapcar (lambda (row) (if (consp row) (car row) row))
          (eval (car (get 'superchat-plugins 'standard-value)) t)))

(defun m4-mounted-row-names ()
  (mapcar #'car (superchat-plugin-report)))

(defmacro m4-with-restored-composition (&rest body)
  "Run BODY, then put the mounted composition back as it was.
These tests mount and unmount rows in the global Scope, which no fixture
resets: without this, a row one test withdraws is missing for the next one.

Restoration goes through `superchat-profile-mount\', the product\'s own exact
composition operation, rather than replaying a list of names, so a row is always
remounted after the rows it injects.

The corrected history, after the user reproduced it: the kernel is fine.
`superchat-plugin-notify\' re-resolves pending rows when a dependency mounts, and
remounting `tape\' alone brings `tools-builtin\' back to active with its effect
resolvers.  The leak was a test: `a-pending-row-is-reported-with-its-missing-service\'
unmounted `tape\' and did not restore it, so every later wrapper captured a list
that no longer contained `tape\' and could not put it back.  With its resolvers
gone, tool selection dropped every explicitly named tool as an unknown
descriptor -- silently, in tests that had nothing to do with composition.  That
case now uses this wrapper too; this macro only makes the recovery robust."
  (declare (indent 0) (debug t))
  (let ((before (make-symbol "before"))
        (profile (make-symbol "profile")))
    `(let ((,before (m4-mounted-row-names))
           (,profile superchat-profile))
       (unwind-protect (progn ,@body)
         (ignore-errors (superchat-profile-mount ,profile))
         ;; A test that mounted rows outside the profile is put back too.
         (dolist (row ,before)
           (unless (superchat-plugin-state row)
             (ignore-errors (superchat-plugin-mount row))))))))

(ert-deftest m4/default/todays-rows-are-mounted-and-active ()
  "The mounted composition is exactly the default row list, every row active and
nothing else mounted.  This is the comparison M4 must keep true: the default
profile is compared row for row, not by feature name."
  (m3r-fixture nil
    (let ((rows (m4-default-row-names))
          (mounted (m4-mounted-row-names)))
      (should rows)
      (should-not (cl-set-difference rows mounted))
      (should-not (cl-set-difference mounted rows))
      (dolist (row rows)
        (ert-info ((format "row: %s" row))
          (should (eq 'active (superchat-plugin-state row))))))))

(ert-deftest m4/default/the-chat-entry-opens-and-answers ()
  "The front door does not change: `superchat' opens the chat View and a Turn
sent from it completes with the provider's answer."
  (m3r-fixture '((:text "front door answer"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (m3r-settle (smoke--send view "hello"))))
      (should (buffer-live-p view))
      (should (eq 'completed (m3r-status run)))
      ;; An answer reached the View's Session, whoever answered.
      (should (cl-find-if (lambda (row) (equal "assistant" (nth 2 row)))
                          (superchat-view-session-history session)))
      ;; `smoke--requests' counts the scripted fake network's calls, and
      ;; SMOKE_REAL=1 replaces that network with a real provider, so the
      ;; one-request assertion belongs to the scripted run only.  Without this
      ;; the front-door guard could not be verified against a real model.
      (unless (getenv "SMOKE_REAL")
        (should (= 1 (length smoke--requests)))))))

(ert-deftest m4/profile/the-default-profile-resolves-to-the-default-rows ()
  "`superchat-profile' names the mounted profile and defaults to `default', whose
resolved rows are `superchat-plugins'.
`superchat-plugins' is the layer the `default' profile resolves from, so an
existing configuration keeps working."
  (should (boundp 'superchat-profile))
  (should (boundp 'superchat-profiles))
  (should (fboundp 'superchat-profile-rows))
  (should (eq 'default (default-value 'superchat-profile)))
  (should (equal (m4-default-row-names)
                 (mapcar (lambda (row) (if (consp row) (car row) row))
                         (superchat-profile-rows 'default)))))

(ert-deftest m4/profile/a-layer-can-disable-a-row-and-keeps-the-order ()
  "A profile is layers over the default rows: a layer may add a row and may
disable one, and the rows it does not touch keep their order.  Disabling is
written down rather than achieved by absence, so a later reordering cannot
resurrect the row."
  (should (fboundp 'superchat-profile-rows))
  (should (boundp 'superchat-profiles))
  (let* ((superchat-profiles
          (cons '(m4r-test . ((:inherit default) ((memory . :disabled))))
                (and (boundp 'superchat-profiles) superchat-profiles)))
         (default-rows (m4-default-row-names))
         (rows (mapcar (lambda (row) (if (consp row) (car row) row))
                       (superchat-profile-rows 'm4r-test))))
    (should-not (memq 'memory rows))
    (should (equal (remq 'memory default-rows) rows))))

(ert-deftest m4/profile/mounting-a-profile-makes-the-composition-exactly-its-rows ()
  "Mounting a profile leaves the mounted composition equal to that profile's
rows: a row the profile does not name is unmounted, not left over from the
profile that was mounted before.  A profile that can only add cannot express
`headless' in a running Emacs."
  (should (fboundp 'superchat-profile-mount))
  (m3r-fixture nil
    (m4-with-restored-composition
     (let ((superchat-profiles
           (cons '(m4r-exact . ((:inherit default) ((memory . :disabled))))
                 (and (boundp 'superchat-profiles) superchat-profiles))))
      (should (memq 'memory (m4-mounted-row-names)))
      (superchat-profile-mount 'm4r-exact)
      (should-not (memq 'memory (m4-mounted-row-names)))
      (should (equal (sort (mapcar #'superchat-profile--entry-name
                                   (superchat-profile-rows 'm4r-exact))
                           #'string<)
                     (sort (m4-mounted-row-names) #'string<)))))))

(ert-deftest m4/profile/the-empty-seats-are-gone-and-the-declarations-stay ()
  "`chat-parse' and `chat-session' register nothing and name work M3 moved into
the dispatcher and Runtime, so they leave the default rows; `prompt-assembler'
and `tools-baseline' stay as declarations of a core capability."
  (let ((rows (m4-default-row-names)))
    (should (memq 'prompt-assembler rows))
    (should (memq 'tools-baseline rows))
    (should-not (memq 'chat-parse rows))
    (should-not (memq 'chat-session rows))))

(ert-deftest m4/surface/the-non-chat-surfaces-are-rows-that-own-their-names ()
  "Inline ask, inline rewrite, Org Babel, Magit and emacsclient are rows of the
default profile, and each row owns its surface name: with the row mounted the
name is registered, and unmounting the row withdraws it.  emacsclient registers
no surface name, so only its row is required."
  (m3r-fixture nil
    (m4-with-restored-composition
     (let ((rows (m4-default-row-names)))
      (should (memq 'emacsclient rows))
      (dolist (pair (cl-remove 'chat m4-surface-rows :key #'car))
        (ert-info ((format "surface row: %s" (car pair)))
          (should (memq (car pair) rows))
          (should (eq 'active (superchat-plugin-state (car pair))))
          (should (memq (cdr pair) superchat-pipeline--surfaces))
          (superchat-plugin-unmount (car pair))
          (should-not (memq (cdr pair) superchat-pipeline--surfaces))
          (superchat-plugin-mount (car pair))))))))

(ert-deftest m4/surface/the-chat-window-is-a-row-that-owns-its-name ()
  "The chat window is a row of the default profile that owns the `chat' surface
name, and the front door still works with it mounted.
The row owns registration, not loading: the chat code stays in superchat.el,
which eager `require' pulls in, so withdrawing the name is what unmounting can
do and the entry guards do the rest (see the refusal test below)."
  (m3r-fixture nil
    (m4-with-restored-composition
     (should (memq 'chat (m4-default-row-names)))
     (should (eq 'active (superchat-plugin-state 'chat)))
     (should (memq 'chat superchat-pipeline--surfaces))
     (superchat-plugin-unmount 'chat)
     (should-not (memq 'chat superchat-pipeline--surfaces))
     (superchat-plugin-mount 'chat))))

(ert-deftest m4/surface/the-chat-front-door-refuses-without-its-row ()
  "With the `chat' row unmounted the front door, the window's own send key and
the last submit seam each refuse, and nothing is built: no View, no Run, no
provider request.  The chat entry points are autoloads that cannot be withdrawn,
so this guard is the whole of what \"the surface is not in this profile\" means
for a user who still has the command bound."
  (m3r-fixture '((:text "must not be sent"))
    (m4-with-restored-composition
     (let ((runs-before (hash-table-count superchat-runtime--runs)))
       (should-not (get-buffer superchat-buffer-name))
       (superchat-plugin-unmount 'chat)
       (dolist (entry (list (lambda () (superchat))
                            (lambda () (with-temp-buffer (superchat-send-input)))
                            (lambda ()
                              (superchat--submit-composed-chat-request nil))))
         ;; As in the S2 refusal test: asserting only "it errors" would also
         ;; accept the Surface validation error the withdrawn name produces
         ;; anyway, so the refusal must name the row and the active profile.
         (let ((refusal (error-message-string (should-error (funcall entry)))))
           (should (string-match-p "chat" refusal))
           (should (string-match-p "profile" refusal))))
       ;; The refusal comes before the work, not after it.
       (should-not (get-buffer superchat-buffer-name))
       (should (= runs-before (hash-table-count superchat-runtime--runs)))
       (should (= 0 (length smoke--requests)))))))

(ert-deftest m4/in-flight/a-live-run-keeps-rendering-through-a-profile-switch ()
  "Switching the composition under a Run that is already answering does not stop
its rendering: the chat Sink is built into the envelope at submit, not fetched
from a row at delivery.  `headless' unmounts every chat row at once, which is the
switch a user can actually make.
This is also why rendering is a function rather than a Service (2026-09-16): a
renderer fetched at delivery time would be gone exactly when the rows are."
  ;; Scripted-only: this test acts inside the window a known answer arriving
  ;; after a known delay opens, and asserts that answer's text.  A real provider
  ;; supplies neither, so SMOKE_REAL skips it rather than failing on the fixture.
  (skip-when (getenv "SMOKE_REAL"))
  (m3r-fixture '((:text "still rendering" :delay 1.0))
    (m4-with-restored-composition
     (let* ((view (smoke--open-chat))
            (run (smoke--send view "hello")))
       (should (smoke--wait (lambda () (member run (superchat-runtime-active-runs))) 10))
       (superchat-profile-mount 'headless)
       (should-not (superchat-plugin-state 'chat))
       (m3r-settle run)
       (should (eq 'completed (m3r-status run)))
       (should (with-current-buffer view
                 (string-match-p "still rendering" (buffer-string))))))))

(defun m3r-tiny-png ()
  "Write a 1x1 PNG into the fixture's data directory and return its path."
  (let ((path (expand-file-name "m3r-attachment.png" default-directory)))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert (base64-decode-string
               (concat "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8"
                       "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="))))
    path))

(ert-deftest m4/in-flight/a-live-run-keeps-its-attachment-record ()
  "An image attached to a chat Turn is sent to the model, so a fact must record
it -- the rule M3 enforces everywhere else as \"material sent without a
record\".  The chat Turn's text is recorded at admission and cannot be lost, but
its attachments are recorded by `chat-turn-recorder' at the seal, so a
composition change while the Run is in flight sends the image and records
nothing, with no error and no audit failure.
Runtime writes the fact when it admits the request, so there is nothing for a
composition change to take away."
  ;; Scripted-only: this test acts inside the window a known answer arriving
  ;; after a known delay opens, and asserts that answer's text.  A real provider
  ;; supplies neither, so SMOKE_REAL skips it rather than failing on the fixture.
  (skip-when (getenv "SMOKE_REAL"))
  (m3r-fixture '((:text "late answer" :delay 1.0))
    (m4-with-restored-composition
     (let* ((png (m3r-tiny-png))
            (view (smoke--open-chat))
            (session (buffer-local-value 'superchat--session-id view))
            run)
       (with-current-buffer view
         (setq-local superchat--current-context-files (list png)))
       (setq run (smoke--send view "look at this"))
       (should (smoke--wait (lambda () (member run (superchat-runtime-active-runs))) 10))
       (superchat-profile-mount 'headless)
       (m3r-settle run)
       (should (eq 'completed (m3r-status run)))
       ;; Exactly one: the image was sent once, so it is recorded once.
       (should (= 1 (m3r-count "attachment.added" (m3r-events session))))))))

(ert-deftest m4/surface/an-entry-point-without-its-row-refuses ()
  "An Emacs autoload cannot be withdrawn, so each surface's entry point asks
whether its row is mounted: without it the entry point refuses and submits
nothing."
  (m3r-fixture '((:text "must not be sent"))
    (m4-with-restored-composition
     (require 'superchat-inline)
     (should (memq 'inline-ask (m4-default-row-names)))
     (superchat-plugin-unmount 'inline-ask)
     (with-temp-buffer
       (insert "some text")
       ;; The refusal must come from the row check, not from a side effect of
       ;; unmounting: it names the row and the active profile, as the record
       ;; requires.  Asserting only "it errors" would also accept the Surface
       ;; validation error that the withdrawn name produces anyway.
       (let ((refusal (error-message-string
                       (should-error (superchat-inline-ask "hello")))))
         (should (string-match-p "inline-ask" refusal))
         (should (string-match-p "profile" refusal))))
     (should (= 0 (length smoke--requests))))))

(ert-deftest m4/headless/no-user-interface-row-and-one-answer ()
  "The `headless' profile resolves to the default rows minus every
user-interface row, and with exactly those rows mounted one prompt is answered
end to end and the answer is on the Tape."
  (should (fboundp 'superchat-profile-rows))
  (m3r-fixture '((:text "headless answer"))
    (m4-with-restored-composition
     (let ((rows (mapcar (lambda (row) (if (consp row) (car row) row))
                        (superchat-profile-rows 'headless))))
      (should rows)
      (dolist (row m4-user-interface-rows)
        (ert-info ((format "user-interface row: %s" row))
          (should-not (memq row rows))))
      (dolist (row '(tape driver authority tools transport-llm))
        (ert-info ((format "kept row: %s" row))
          (should (memq row rows))))
      ;; Mount what the profile says and nothing else, then answer one prompt.
      (dolist (row (cl-set-difference (m4-mounted-row-names) rows))
        (superchat-plugin-unmount row))
      (let ((run (m3r-run "hello")))
        (should (eq 'completed (m3r-status run)))
        (should (m3r-of-type "response.completed" (m3r-events m3r-session run))))))))

(ert-deftest m4/profile/a-pending-row-is-reported-with-its-missing-service ()
  "A row the profile names but whose injected Service never arrives is reported,
with the Services it waits for, instead of staying silently inert."
  (should (fboundp 'superchat-profile-report))
  (m3r-fixture nil
    ;; This case unmounts a row every other row depends on, so it must put the
    ;; composition back; without the wrapper `tape' stayed unmounted for the
    ;; rest of the process.
    (m4-with-restored-composition
     (superchat-plugin-unmount 'tape)
     (let* ((report (superchat-profile-report))
           (pending (cl-remove-if-not (lambda (entry) (eq 'pending (nth 1 entry)))
                                      report)))
       (should pending)
       (should (cl-some (lambda (entry) (memq 'tape (nth 2 entry))) pending))))))

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
