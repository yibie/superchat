;;; test-runtime.el --- ERT tests for Superchat Runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat)
;; S05a-fix-1's Follow-up queue tests need a real Tape store and the
;; declared provider-network seam, both of which `journey-fixture-with'
;; supplies; evaluated at load time (matches `test-subagent.el').
(require 'journey-fixture
         (expand-file-name "journey-fixture" (file-name-directory load-file-name)))

(defmacro superchat-runtime-test--isolated (&rest body)
  "Run BODY with isolated Runtime state and completion hooks."
  (declare (indent 0))
  `(let ((superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-buffer-name " *superchat-runtime-test*")
         (superchat--active-timeout-timer nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-closure-shadow-functions nil)
         (superchat-runtime--warned-legacy-seams nil)
     (superchat-runtime--closure-observations nil))
     ,@body))

(defmacro superchat-runtime-test--with-temp-store (&rest body)
  "Run BODY against a temporary SQLite Tape store."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "superchat-runtime-reconcile-" t))
          (superchat-data-directory directory))
     (unwind-protect
         (progn
           (superchat-db-close)
           (superchat-db-open)
           ,@body)
       (superchat-db-close)
       (delete-directory directory t))))

(defun superchat-runtime-test--append-event
    (session-id turn-id run-id type event-id causation-id payload)
  "Append a schema-v1 Runtime event fixture."
  (superchat-tape-append-event-record
   (append
    (list :type type :event-id event-id :schema 1
          :session-id session-id :turn-id turn-id :run-id run-id
          :actor-id "lead" :payload payload)
    (when causation-id (list :causation-id causation-id)))))

(defun superchat-runtime-test--append-hanging-run
    (session-id turn-id run-id)
  "Append a Run with an in-flight tool Effect and no terminal events."
  (let ((intent-id (format "%s-intent" run-id))
        (start-id (format "%s-start" run-id))
        (request-id (format "%s-request" run-id))
        (execution-id (format "%s-exec" run-id)))
    (superchat-runtime-test--append-event
     session-id turn-id run-id "intent.received" intent-id nil nil)
    (superchat-runtime-test--append-event
     session-id turn-id run-id "run.started" start-id intent-id
     '(:status running-model :actor-id "lead" :capability "chat"))
    (superchat-runtime-test--append-event
     session-id turn-id run-id "tool.requested" request-id start-id
     `(:tool-name "read_buffer" :execution-id ,execution-id
       :request-hash "hash-open" :decision "allow"
       :policy-version "authority-v1"
       :effects ((:effect local-read :resource buffer))))
    (superchat-runtime-test--append-event
     session-id turn-id run-id "tool.execution.started" execution-id
     request-id
     `(:tool-name "read_buffer" :execution-id ,execution-id
       :request-hash "hash-open" :status "executing"))))

(defun superchat-runtime-test--request ()
  "Return a minimal prepared Runtime request."
  (let ((buffer (get-buffer-create " *superchat-runtime-test*")))
    (list :type :llm-query
          :input "hello"
          :clean-input "hello"
          :prompt "User question: hello"
          :session-id "session-runtime"
          :buffer buffer
          :buffer-name (buffer-name buffer))))

(defun superchat-runtime-test--view-request ()
  "Like `superchat-runtime-test--request', hosted by a live chat View.

Since S01, a legacy Request with no Sink callback only reaches the default
Render fallback when its buffer is a live `superchat-mode' View matching its
Session ID; tests that observe that default fallback need one.  This uses a
fresh buffer (never `superchat-runtime-test--request''s shared one) so nothing
else is affected by turning `superchat-mode' on."
  (let* ((request (superchat-runtime-test--request))
         (buffer (generate-new-buffer " *superchat-runtime-test-view*")))
    (with-current-buffer buffer
      (superchat-mode 1)
      (setq-local superchat--session-id (plist-get request :session-id)))
    (plist-put (plist-put request :buffer buffer)
               :buffer-name (buffer-name buffer))))

(ert-deftest runtime/run-in-buffer-p-exact-object ()
  "The read-only association is an exact live object, independent of liveness."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((host (generate-new-buffer " *exact runtime host*"))
            (other (generate-new-buffer " *other runtime host*"))
            (superchat-closure-terminal-guard-enabled t)
            (superchat-runtime-default-deadline nil)
            replacement run)
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--llm-generate-answer) #'ignore))
              ;; Actual acceptance publishes the host; this focused predicate
              ;; test does not claim to exercise a native model scheduler.
              (setq run (plist-get
                         (superchat-runtime-test-submit
                          (list :prompt "host identity" :session-id "exact-host"
                                :buffer host :deadline nil :delivery-function #'ignore))
                         :run-id))
              (let ((status (superchat-runtime-status run))
                    (events (superchat-tape-replay-events "exact-host"))
                    (name (buffer-name host)))
                (should (superchat-runtime-run-in-buffer-p run host))
                (should-not (superchat-runtime-run-in-buffer-p run other))
                (should-not (superchat-runtime-run-in-buffer-p run name))
                (should-not (superchat-runtime-run-in-buffer-p run nil))
                (should-not (superchat-runtime-run-in-buffer-p "unknown" host))
                (with-current-buffer host (rename-buffer (generate-new-buffer-name " *renamed exact host*")))
                (setq replacement (generate-new-buffer name))
                (should (superchat-runtime-run-in-buffer-p run host))
                (should-not (superchat-runtime-run-in-buffer-p run replacement))
                (should (equal status (superchat-runtime-status run)))
                (should (equal events (superchat-tape-replay-events "exact-host"))))
              (superchat-runtime-cancel run)
              (should (superchat-runtime-run-in-buffer-p run host))
              (kill-buffer host)
              (should-not (superchat-runtime-run-in-buffer-p run host))
              (should-not (superchat-runtime-run-in-buffer-p run replacement)))
          (when run (superchat-runtime-cancel run))
          (dolist (buffer (list host other replacement))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest runtime/request-plan-retains-context-envelope ()
  "The prepared Turn's View receipt must survive Request construction."
  (let* ((turn (superchat-turn-new "hello" "session-runtime"))
         (envelope '(:contract "context-continuity-v0"
                     :status satisfied
                     :receipt (:tape-head 12))))
    (setf (superchat-turn-state turn) (list :context-envelope envelope))
    (cl-letf (((symbol-function 'superchat--apply-topology-overlay) #'identity)
              ((symbol-function 'superchat--execute-llm-query)
               (lambda (&rest _args) '(:type :llm-query :prompt "hello")))
              ((symbol-function 'superchat--session-runtime-configuration)
               (lambda () nil)))
      (let ((plan (superchat--request-plan-from-turn turn)))
        (should (equal envelope (plist-get plan :context-envelope)))
        (should-not (eq envelope (plist-get plan :context-envelope)))))))

(ert-deftest runtime/closure-terminal-guard-is-enabled-by-default ()
  "A fresh Superchat load must enforce Closure terminal readiness."
  (should superchat-closure-terminal-guard-enabled))

(ert-deftest runtime/plan-progress-does-not-poison-the-next-run-terminal-guard ()
  "A Session-level plan event remains irrelevant to the next Runtime Run."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((session-id "runtime-plan-progress-guard")
            (superchat-closure-terminal-guard-enabled t)
            (superchat-runtime-default-deadline nil)
            (first-buffer (generate-new-buffer " *runtime-plan-first*"))
            (second-buffer (generate-new-buffer " *runtime-plan-second*"))
            observer first second)
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (setq observer superchat--llm-runtime-observer))))
              (setq first
                    (superchat-runtime-test-submit
                     (list :prompt "first" :session-id session-id
                           :buffer first-buffer :deadline nil
                           :delivery-function #'ignore)))
              (funcall observer 'terminal
                       '(:status completed :response "first complete"))
              (should (eq 'completed
                          (plist-get
                           (superchat-runtime-status (plist-get first :run-id))
                           :status)))
              ;; This is the real Session-level plan write: it deliberately
              ;; carries no Run or Turn identity.
              (superchat-tape-append-event-record
               (list :type "plan.progress" :schema 1 :session-id session-id
                     :actor-id "lead"
                     :payload
                     '(:proposed (:session-id "runtime-plan-progress-guard"
                                  :event-id "plan-proposal")
                       :step-id 1 :status "done")))
              (setq second
                    (superchat-runtime-test-submit
                     (list :prompt "second" :session-id session-id
                           :buffer second-buffer :deadline nil
                           :delivery-function #'ignore)))
              (funcall observer 'terminal
                       '(:status completed :response "second complete"))
              (should (eq 'completed
                          (plist-get
                           (superchat-runtime-status (plist-get second :run-id))
                           :status))))
          (dolist (buffer (list first-buffer second-buffer))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest runtime/reconcile-session-seals-open-runs-idempotently ()
  "Reopening a Session seals each open Run/Turn pair exactly once."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--with-temp-store
    (let ((session-id "session-reconcile")
          (turn-id "turn-open")
          (run-id "run-open"))
      ;; This Run has an in-flight Effect when the process disappears.
      (superchat-runtime-test--append-hanging-run
       session-id turn-id run-id)
      ;; A normal terminal Run in the same Session is not touched.
      (superchat-runtime-test--append-event
       session-id "turn-closed" "run-closed" "intent.received"
       "evt-closed-intent" nil nil)
      (superchat-runtime-test--append-event
       session-id "turn-closed" "run-closed" "run.started"
       "evt-closed-start" "evt-closed-intent"
       '(:status running-model :actor-id "lead" :capability "chat"))
      (superchat-runtime-test--append-event
       session-id "turn-closed" "run-closed" "response.completed"
       "evt-closed-response" "evt-closed-start"
       '(:status completed :content-size 2))
      (superchat-runtime-test--append-event
       session-id "turn-closed" "run-closed" "run.completed"
       "evt-closed-terminal" "evt-closed-response"
       '(:status completed))
      (superchat-runtime-test--append-event
       session-id "turn-closed" "run-closed" "turn.completed"
       "evt-closed-turn-terminal" "evt-closed-terminal"
       '(:status completed))
      (let* ((before (superchat-tape-replay-events session-id))
             (open-judgment
              (superchat-closure-judge
               (list :kind 'durable-run-terminal :run-id run-id)
               before)))
        (should (eq 'open (plist-get open-judgment :verdict))))
      (should (= 1 (superchat-runtime-reconcile-session session-id)))
      (let* ((trace (superchat-tape-replay-events session-id))
             (abandoned
              (cl-remove-if-not
               (lambda (event)
                 (and (equal run-id (plist-get event :run-id))
                      (equal "run.failed" (plist-get event :type))))
               trace))
             (turn-seals
              (cl-remove-if-not
               (lambda (event)
                 (and (equal run-id (plist-get event :run-id))
                      (equal "turn.completed" (plist-get event :type))))
               trace))
             (run-judgment
              (superchat-closure-judge
               (list :kind 'durable-run-terminal :run-id run-id) trace))
             (turn-judgment
              (superchat-closure-judge
               (list :kind 'durable-turn-terminal :run-id run-id
                     :turn-id turn-id)
               trace)))
        (should (= 1 (length abandoned)))
        (should (= 1 (length turn-seals)))
        (dolist (event (append abandoned turn-seals))
          (should (equal "abandoned"
                         (plist-get (plist-get event :payload) :status)))
          (should (eq t (plist-get (plist-get event :payload) :abandoned))))
        (should (eq 'proved (plist-get run-judgment :verdict)))
        (should (eq 'abandoned
                    (plist-get (plist-get run-judgment :judgment)
                               :terminal-kind)))
        (should (eq 'proved (plist-get turn-judgment :verdict)))
        (should-not
         (cl-some
          (lambda (event)
            (and (equal "run.failed" (plist-get event :type))
                 (equal "run-closed" (plist-get event :run-id))))
          trace)))
      ;; The second View rebuild sees the durable terminal and writes nothing.
      (should (= 0 (superchat-runtime-reconcile-session session-id)))
      (let ((trace (superchat-tape-replay-events session-id)))
        (should (= 1
                   (cl-count "run.failed"
                             (cl-remove-if-not
                              (lambda (event)
                                (equal run-id (plist-get event :run-id)))
                              trace)
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))
        (should (= 1
                   (cl-count "turn.completed"
                             (cl-remove-if-not
                              (lambda (event)
                                (equal run-id (plist-get event :run-id)))
                              trace)
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))))))

(ert-deftest runtime/reconcile-session-failure-does-not-block-view-open ()
  "Tape or Closure failures degrade reconciliation to no-op."
  (let (warnings)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (_session-id) (error "Tape unavailable")))
              ((symbol-function 'display-warning)
               (lambda (type message &rest args)
                 (push (list type message args) warnings))))
      (should (= 0 (superchat-runtime-reconcile-session "session-broken"))))
    (should (= 1 (length warnings)))
    (let ((warning (car warnings)))
      (should (eq 'superchat-runtime (nth 0 warning)))
      (should (eq :warning (car (nth 2 warning))))
      (should (string-match-p "session-broken" (nth 1 warning)))
      (should (string-match-p "Tape unavailable" (nth 1 warning)))
      (should (string-match-p "remain open in Tape" (nth 1 warning))))))

(ert-deftest runtime/reconcile-session-warning-failure-stays-contained ()
  "A warning failure must not break reconciliation's outer fallback."
  (cl-letf (((symbol-function 'superchat-tape-replay-events)
             (lambda (_session-id) (error "Tape unavailable")))
            ((symbol-function 'display-warning)
             (lambda (&rest _args) (error "Warnings unavailable"))))
    (should (= 0 (superchat-runtime-reconcile-session "session-warning")))))

(ert-deftest runtime/reconcile-session-zero-means-degraded-after-real-work ()
  "A failed retry is distinguishable from a healthy session with no work."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--with-temp-store
    (let ((session-id "session-reconcile-failure")
          (run-id "run-reconcile-failure")
          (turn-id "turn-reconcile-failure"))
      (superchat-runtime-test--append-hanging-run
       session-id turn-id run-id)
      (should (= 1 (superchat-runtime-reconcile-session session-id)))
      (let ((before (superchat-tape-replay-events session-id)))
        (should (= 1
                   (cl-count "run.failed" before
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))
        (should (= 1
                   (cl-count "turn.completed" before
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))
        (cl-letf (((symbol-function 'superchat-tape-replay-events)
                   (lambda (_session-id) (error "Tape unavailable"))))
          (should (= 0 (superchat-runtime-reconcile-session session-id))))
        (let ((after (superchat-tape-replay-events session-id)))
          (should (= (length before) (length after)))
          (should (equal (mapcar (lambda (event) (plist-get event :event-id))
                                 before)
                         (mapcar (lambda (event) (plist-get event :event-id))
                                 after))))))))

(ert-deftest runtime/reconcile-session-isolates-per-run-failures ()
  "A failed seal for one Run does not prevent the next Run from sealing."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--with-temp-store
    (let ((session-id "session-reconcile-isolated")
          (first-run "run-reconcile-first")
          (second-run "run-reconcile-second")
          (append-event-record
           (symbol-function 'superchat-tape-append-event-record))
          (fail-first t))
      (superchat-runtime-test--append-hanging-run
       session-id "turn-reconcile-first" first-run)
      (superchat-runtime-test--append-hanging-run
       session-id "turn-reconcile-second" second-run)
      (let (warnings)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event)
                     (if fail-first
                         (progn
                           (setq fail-first nil)
                           (error "first seal unavailable"))
                       (funcall append-event-record event))))
                  ((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (should (= 1 (superchat-runtime-reconcile-session session-id))))
        (should-not warnings))
      (let ((trace (superchat-tape-replay-events session-id)))
        (should-not
         (cl-some
          (lambda (event)
            (and (equal first-run (plist-get event :run-id))
                 (member (plist-get event :type)
                         '("run.failed" "turn.completed"))))
          trace))
        (should (= 1
                   (cl-count "run.failed"
                             (cl-remove-if-not
                              (lambda (event)
                                (equal second-run (plist-get event :run-id)))
                              trace)
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))
        (should (= 1
                   (cl-count "turn.completed"
                             (cl-remove-if-not
                              (lambda (event)
                                (equal second-run (plist-get event :run-id)))
                              trace)
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal)))))))

(ert-deftest runtime/request-deadline-validates-duration ()
  "Request deadlines accept nil/zero and reject invalid durations."
  (dolist (deadline '(nil 0 0.5 10))
    (should
     (equal deadline
            (plist-get
             (superchat-runtime--validate-request
              (list :prompt "hello" :deadline deadline))
             :deadline))))
  (dolist (deadline '(-1 "ten" (1 2)))
    (should-error
     (superchat-runtime--validate-request
      (list :prompt "hello" :deadline deadline)))))

(ert-deftest runtime/request-retires-legacy-editor-host-fields ()
  "Runtime admission rejects every explicit legacy editor-host field."
  (let ((buffer (generate-new-buffer " *superchat-runtime-legacy-reject*")))
    (unwind-protect
        (dolist (legacy '(:ephemeral-buffer :host-authority-mode))
          (let ((request (list :prompt "hello" :buffer buffer)))
            (setq request (plist-put request legacy nil))
            (should (plist-member request legacy))
            (should-error (superchat-runtime-test-submit request))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest runtime/request-requires-live-exact-buffer-before-effects ()
  "Missing, nil, and dead exact buffers fail before every Runtime effect."
  (superchat-runtime-test--isolated
    (let* ((name " *superchat-runtime-no-fallback*")
           (dead (generate-new-buffer " *superchat-runtime-dead-host*"))
           (missing (list :prompt "hello" :buffer-name name))
           (nil-buffer (list :prompt "hello" :buffer nil))
           (provider-calls 0)
           (tape-calls 0))
      (kill-buffer dead)
      (should-not (plist-member missing :buffer))
      (should (plist-member nil-buffer :buffer))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf tape-calls)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (cl-incf provider-calls))))
        (dolist (request (list missing nil-buffer
                               (list :prompt "hello" :buffer dead)))
          (should-error (superchat-runtime-test-submit request)))
        (should (= 0 tape-calls))
        (should (= 0 provider-calls))
        (should (= 0 (hash-table-count superchat-runtime--runs)))
        (should-not (get-buffer name))))))

(ert-deftest runtime/headless-deadline-times-out-and-seals-tape ()
  "A headless Run reaches the existing timed-out terminal on its deadline."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline 60)
          (superchat-response-timeout nil)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (copy-tree event) events)
                   (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) nil)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (list :prompt "wait" :input "wait"
                       :session-id "session-headless-deadline"
                       :surface 'headless
                       :buffer (get-buffer-create " *runtime-headless*"))))
               (run-id (plist-get handle :run-id))
               (timer (plist-get
                       (superchat-runtime--entry run-id)
                       :deadline-timer)))
          (should (timerp timer))
          ;; Invoke the real timer event handler without sleeping in the suite.
          (timer-event-handler timer)
          (should (eq 'timed-out
                      (plist-get (superchat-runtime-status run-id) :status)))
          (should-not
           (timerp
            (plist-get (superchat-runtime--entry run-id)
                       :deadline-timer)))
          (should (= 1
                     (cl-count "run.failed" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal)))
          (should (= 1
                     (cl-count "turn.completed" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal))))))))

(ert-deftest runtime/deadline-request-overrides-default-and-zero-disables ()
  "Request deadlines override the default; nil and zero leave Runs unbounded."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline 30)
          run-ids)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) nil)))
        (dolist (request
                 (mapcar (lambda (request)
                           (plist-put request :buffer
                                      (get-buffer-create " *runtime-deadlines*")))
                         '((:prompt "default")
                           (:prompt "nil" :deadline nil)
                           (:prompt "zero" :deadline 0))))
          (let* ((handle (superchat-runtime-test-submit request))
                 (run-id (plist-get handle :run-id))
                 (entry (superchat-runtime--entry run-id)))
            (push run-id run-ids)
            (if (equal "default" (plist-get request :prompt))
                (should (timerp (plist-get entry :deadline-timer)))
              (should-not (plist-get entry :deadline))
              (should-not (timerp (plist-get entry :deadline-timer))))))
        (dolist (run-id run-ids)
          (superchat-runtime-cancel run-id))))))

(ert-deftest runtime/completion-cancels-deadline-timer ()
  "A normally completed Run leaves no Runtime deadline timer behind."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline 30))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (list :prompt "done"
                       :buffer (get-buffer-create " *runtime-complete*"))))
               (entry (superchat-runtime--entry
                       (plist-get handle :run-id))))
          (should (eq 'completed (plist-get entry :status)))
          (should-not (timerp (plist-get entry :deadline-timer))))))))

(ert-deftest runtime/every-live-terminal-path-quiesces-exactly-once ()
  "Every live terminal kind crosses the one-shot quiescence seam."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline 30)
          (next-id 0)
          (quiescence-counts (make-hash-table :test #'equal))
          (transport-cancels (make-hash-table :test #'equal))
          (original-quiesce
           (symbol-function 'superchat-runtime--quiesce)))
      (cl-letf
           (((symbol-function 'superchat-tape-append-event-record)
            (lambda (_event)
              (cl-incf next-id)
              (list :event-id (format "evt-%d" next-id)
                    :storage-id next-id)))
           ((symbol-function 'superchat-tape-queue-recovery-events) #'ignore)
           ((symbol-function 'superchat--update-status) #'ignore)
           ((symbol-function 'llm-cancel-request) #'ignore)
           ((symbol-function 'superchat-llm--transport-timer-owner-cancel)
            (lambda (owner)
              (puthash owner (1+ (gethash owner transport-cancels 0))
                       transport-cancels)))
           ((symbol-function 'superchat-runtime--quiesce)
            (lambda (entry)
              (let ((run-id (plist-get entry :run-id)))
                (puthash run-id
                         (1+ (gethash run-id quiescence-counts 0))
                         quiescence-counts))
              (funcall original-quiesce entry))))
        (dolist (kind '(completed failed cancelled timed-out audit-incomplete))
          (let* ((name (format " *superchat-quiescence-%s*" kind))
                 (view (get-buffer-create name))
                 (timer-owner (list :owner kind))
                 observer handle run-id)
            (unwind-protect
                (progn
                  (cl-letf
                      (((symbol-function 'superchat-runtime--append-event)
                        (lambda (entry type payload summary
                                 &optional causation-id requires-payload)
                          (ignore summary causation-id requires-payload)
                          (when (and (eq kind 'audit-incomplete)
                                     (equal type "response.completed"))
                            (error "fixture audit failure"))
                          (cl-incf next-id)
                          (list :event-id (format "evt-%d" next-id)
                                :storage-id next-id
                                :entry entry :payload payload)))
                       ((symbol-function 'superchat--llm-generate-answer)
                        (lambda (&rest _args)
                          (setq observer superchat--llm-runtime-observer)
                          (funcall
                           superchat--llm-transport-timer-owner-function
                           timer-owner)
                          (funcall observer 'request 'provider-request))))
                    (setq handle
                          (superchat-runtime-test-submit
                           (list :prompt "terminal fixture"
                                 :session-id (format "session-%s" kind)
                                 :buffer view
                                 :buffer-name name
                                 :delivery-function #'ignore)))
                    (setq run-id (plist-get handle :run-id))
                    (pcase kind
                      ('completed
                       (funcall observer 'terminal
                                '(:status completed :response "done")))
                      ('failed
                       (funcall observer 'terminal
                                '(:status failed :response "failed")))
                      ('cancelled
                       (superchat-runtime-cancel run-id))
                      ('timed-out
                       (superchat-runtime--deadline-expired run-id))
                      ('audit-incomplete
                       (funcall observer 'terminal
                                '(:status completed :response "done"))))
                    (should (eq kind
                                (plist-get
                                 (superchat-runtime-status run-id) :status)))
                    (should (= 1 (gethash run-id quiescence-counts 0)))
                    (should (= 1 (gethash timer-owner transport-cancels 0)))
                    (should (buffer-live-p view))
                    (let ((entry (superchat-runtime--entry run-id)))
                      (should-not (plist-get entry :deadline-timer))
                      (should-not (plist-get entry :transport-timer-owner))
                      (should-not (plist-get entry :transport-request))
                      (should-not (plist-get entry :delivery-function))
                      (should-not (plist-get entry :stream-function)))))
              (when run-id
                (superchat-runtime-cancel run-id "test cleanup"))
              (when (buffer-live-p view) (kill-buffer view)))))))))

(ert-deftest runtime/deadline-and-provider-timeout-seal-once ()
  "A deadline and a late provider terminal callback produce one terminal pair."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline 30)
          (superchat-response-timeout nil)
          events observer)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (copy-tree event) events)
                   (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (list :prompt "race"
                       :buffer (get-buffer-create " *runtime-race*"))))
               (run-id (plist-get handle :run-id)))
          (superchat-runtime--deadline-expired run-id)
          (funcall observer 'terminal
                   '(:status timed-out :response "provider timeout"))
          (should (eq 'timed-out
                      (plist-get (superchat-runtime-status run-id) :status)))
          (should (= 1
                     (cl-count "run.failed" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal)))
          (should (= 1
                     (cl-count "turn.completed" events
                               :key (lambda (event) (plist-get event :type))
                               :test #'equal))))))))

(ert-deftest runtime/deadline-message-carries-last-model-phase ()
  "The total wall-clock deadline explains the last known model stage."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime-default-deadline nil)
          delivered)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) nil)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (list :prompt "wait"
                       :buffer (get-buffer-create " *runtime-phase*" )
                       :delivery-function
                       (lambda (value _status _tape-id)
                         (setq delivered value)))))
               (run-id (plist-get handle :run-id)))
          (superchat-runtime--update run-id :model-phase 'reasoning)
          (superchat-runtime--deadline-expired run-id)
          (should (string-match-p "last phase: reasoning" delivered)))))))

(ert-deftest runtime/legacy-executor-transport-shim-stays-deleted ()
  "The legacy blocking executor entry point must not come back."
  (require 'superchat-executor)
  (should-not (fboundp 'superchat-executor-call-llm)))

(ert-deftest runtime/is-the-only-turn-completion-owner ()
  "Only Runtime may expose or call a Turn completion seam."
  (should-not (fboundp 'superchat-core-finalize-turn))
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "superchat-runtime.el")
      (with-temp-buffer
          (insert-file-contents file)
        (should-not
           (re-search-forward
            "(\\s-*\\(?:superchat-core-finalize-turn\\|superchat-runtime--\\(?:finish\\|complete-turn\\)\\)\\_>"
            nil t)))))))

(ert-deftest runtime/product-entry-points-do-not-call-model-transport ()
  "Product entry points must submit through Runtime, never model transport.
The transport lives in `superchat-llm.el' (pinned to exactly one
streaming call); `superchat-runtime.el' is its only caller (pinned to
exactly one invocation).  Every other file — including the
`superchat.el' distribution — must stay transport-free."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (owners '("superchat-llm.el" "superchat-runtime.el")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (member (file-name-nondirectory file) owners)
        (with-temp-buffer
          (insert-file-contents file)
          (should-not
           (re-search-forward
            "(\\s-*\\(?:llm-chat\\|llm-chat-async\\|llm-chat-streaming\\|superchat--llm-generate-answer\\(?:-sync\\)?\\)\\_>"
            nil t)))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-llm.el" root))
      (should (= 1 (how-many "^[^;\n]*(\\s-*llm-chat-streaming\\_>"))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-runtime.el" root))
      (should (= 1 (how-many "^[^;\n]*(\\s-*superchat--llm-generate-answer\\_>"))))))

;; ═══════════════════════════════════════════════════════════
;; Static event-type gate -- a best-effort SYNTACTIC check, not a
;; value-flow analysis.  Per the 00c-fix-1d leader ruling: real
;; completeness is S03b's job (`superchat-tape-append-event' will
;; reject every unlisted type at append time, so a typo fails the
;; moment its code path actually runs); this gate exists to catch
;; syntactic drift early, in CI, without running anything.
;;
;; Covered: a literal string directly next to a `:type' keyword in a
;; form that reaches the Tape seam; any literal string anywhere inside
;; a known helper writer's type argument, whatever expression shape
;; that argument takes (a conditional, a backquoted plist, ...);
;; `:type' reached through the unquote form `read' produces for `` ,P ``
;; inside a backquoted list; `defun'/`cl-defun' writer discovery to a
;; fixpoint, counting only positional parameters; comparing discovered
;; `(name . index)' pairs against the static list; and failing closed
;; (signalling, naming the file) on a file that does not read cleanly,
;; truncated final forms included.
;;
;; NOT covered, by design (out of scope per the ruling above): a type
;; value reached through a `let'/`let*' binding, through a called
;; selector function's return value, or through `funcall'/`apply'.
;; Nothing here chases those -- S03b's append-time rejection is the
;; backstop for that class of drift.
;; ═══════════════════════════════════════════════════════════

(defun test-runtime--production-event-source-files ()
  "Return production Elisp files that can contain literal Tape event types."
  (let* ((root (file-name-directory (locate-library "superchat-runtime")))
         (plugins (expand-file-name "plugins" root))
         (top-level (directory-files root t "\\.el\\'"))
         (plugin-files (and (file-directory-p plugins)
                            (directory-files-recursively plugins "\\.el\\'"))))
    (cl-remove-if
     (lambda (file)
       (let ((base (file-name-nondirectory file)))
         (or (equal base "superchat-closure.el")
             (string-prefix-p "test-" base))))
     (append top-level plugin-files))))

(defconst test-runtime--dotted-literal-regexp
  "\\`[[:lower:]][[:lower:][:digit:]_-]*\\(?:\\.[[:lower:][:digit:]_-]+\\)+\\'"
  "Shape of a dotted Tape event-type string, e.g. \"workspace.unbound\".
Matched against parsed string VALUES (no surrounding quotes), never
against source text.")

(defconst test-runtime--tape-event-helper-writers
  '((superchat--configuration-event . 0)
    (superchat-workspace--emit . 1)
    (superchat-runtime--event . 1)
    (superchat-runtime--append-event . 1)
    (superchat-runtime--queue-event . 1)
    (superchat-authority--append-event . 0)
    (superchat-authority--event-base . 0))
  "Known Tape event helper writers, and the 0-based position of their
TYPE argument.  Each builds a Tape event plist as `(list :type type
...)' from one of its own parameters instead of a literal, so a
caller's literal type never appears next to a `:type' keyword in the
source -- only as a positional argument to one of these names.
Hand-maintained, like `superchat-closure--known-event-types' itself;
`runtime/helper-writer-list-covers-every-discovered-writer' below
checks it against every writer this codebase can be shown to define.")

(defconst test-runtime--tape-append-functions
  '(superchat-tape-append-event superchat-tape-append-event-record)
  "Functions a Tape event write ultimately reaches.")

(defun test-runtime--walk (form list-fn atom-fn)
  "Call LIST-FN on every cons cell and ATOM-FN on every other non-nil
leaf reachable in FORM, a value from `read'.  Comments and the
`lexical-binding' cookie never reach FORM -- `read' already skipped
them.  Backquote/unquote read as ordinary lists (like `(\\=` foo)' and
`(\\, foo)') and are walked the same way as any other list."
  (cond
   ((consp form)
    (funcall list-fn form)
    (test-runtime--walk (car form) list-fn atom-fn)
    (test-runtime--walk (cdr form) list-fn atom-fn))
   ((vectorp form) (mapc (lambda (x) (test-runtime--walk x list-fn atom-fn)) form))
   (form (funcall atom-fn form))))

(defun test-runtime--dotted-strings (form)
  "Return every string in FORM shaped like a dotted Tape event type."
  (let (found)
    (test-runtime--walk
     form #'ignore
     (lambda (atom)
       (when (and (stringp atom) (string-match-p test-runtime--dotted-literal-regexp atom))
         (push atom found))))
    found))

(defun test-runtime--reaches-tape-p (form)
  "Return non-nil if FORM calls a Tape append function anywhere."
  (catch 'found
    (test-runtime--walk
     form (lambda (list)
           (when (memq (car list) test-runtime--tape-append-functions)
             (throw 'found t)))
     #'ignore)
    nil))

(defun test-runtime--file-forms (file)
  "Return FILE's top-level forms via `read'.
Signals an error naming FILE if any form fails to read, including one
truncated by end-of-file -- `read' signals `end-of-file' both at a
clean end of file and when EOF cuts a form short, so the boundary
between the two is FILE's last non-whitespace, non-comment position: a
silently skipped or truncated file is exactly the kind of hole this
gate exists to close."
  (with-temp-buffer
    (insert-file-contents file)
    ;; `forward-comment' -- unlike `read' -- looks at the buffer's own
    ;; syntax table to recognize a comment, and a `with-temp-buffer' has
    ;; no Lisp comment syntax by default; without this, "`;;;' ends
    ;; here" trailers looked like a truncated form to `forward-comment'
    ;; alone, even though `read' had already finished cleanly.
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let (forms start)
      (condition-case err
          (while t
            (forward-comment (point-max))
            (setq start (point))
            (push (read (current-buffer)) forms))
        (end-of-file
         (when (< start (point-max))
           (error "%s: form starting at position %d truncated by end of file"
                  file start)))
        (error (error "%s: form failed to read: %s" file (error-message-string err))))
      (nreverse forms))))

(defun test-runtime--literal-tape-event-types (file)
  "Return dotted Tape event-type string literals FILE writes: those
sitting directly next to a `:type' keyword in a form that reaches the
Tape seam, and those anywhere inside a known helper writer's type
argument -- whatever form that argument takes -- see
`test-runtime--tape-event-helper-writers'."
  (let ((form (cons 'progn (test-runtime--file-forms file))))
    (append
     (when (test-runtime--reaches-tape-p form)
       (let (found)
         (test-runtime--walk
          form
          (lambda (list)
            (when (and (eq (car list) :type)
                      (stringp (cadr list))
                      (string-match-p test-runtime--dotted-literal-regexp (cadr list)))
              (push (cadr list) found)))
          #'ignore)
         found))
     (let (found)
       (test-runtime--walk
        form
        (lambda (list)
          (when-let* ((entry (assq (car list) test-runtime--tape-event-helper-writers)))
            (setq found (append (test-runtime--dotted-strings (nth (1+ (cdr entry)) list))
                                found))))
        #'ignore)
       found))))

(defun test-runtime--positional-params (params)
  "PARAMS truncated to the positional prefix before the first
`&'-prefixed marker (`&optional', `&rest', `&key', ...).  Enough of a
`cl-defun's argument list for this gate: it does not resolve keyword
or optional-default argument flow, only where in a plain positional
prefix PARAM sits."
  (cl-loop for p in params
           until (and (symbolp p) (string-prefix-p "&" (symbol-name p)))
           collect p))

(defun test-runtime--writer-index (params param)
  "PARAM's 0-based position among PARAMS's positional prefix."
  (cl-position param (test-runtime--positional-params params)))

(defun test-runtime--type-value-p (value param)
  "Return non-nil if VALUE -- a form bound to `:type' -- is PARAM
itself, or PARAM reached through the unquote form `read' produces for
`` ,PARAM `` inside a backquoted list: `` `(:type ,type ...)' `` reads
as `` (\\=` (:type (\\, type) ...)) ``, so the value sitting after `:type'
is `(\\, PARAM)', not PARAM directly."
  (or (eq value param)
      (and (consp value) (eq (car value) '\,) (eq (cadr value) param))))

(defun test-runtime--param-at-type-p (body param)
  "Return non-nil if BODY has `(list :type PARAM ...)' anywhere, PARAM
possibly reached through backquote/unquote -- see
`test-runtime--type-value-p'."
  (catch 'found
    (test-runtime--walk
     body
     (lambda (list)
       (when (and (eq (car list) :type) (test-runtime--type-value-p (cadr list) param))
         (throw 'found t)))
     #'ignore)
    nil))

(defun test-runtime--param-feeds-writer-p (body param pool)
  "Return non-nil if BODY passes PARAM into a POOL writer's type slot.
POOL is an alist of (NAME . INDEX) writers to check against."
  (catch 'found
    (test-runtime--walk
     body
     (lambda (list)
       (when-let* ((entry (assq (car list) pool)))
         (when (eq (nth (1+ (cdr entry)) list) param) (throw 'found t))))
     #'ignore)
    nil))

(defun test-runtime--discover-file-writers (file known)
  "Return every (NAME . INDEX) FILE's `defun'/`cl-defun' forms can be
shown to write, whether or not NAME is already in KNOWN -- see item 2
of `.scratch/core-simplification/issues/00c-fix-1c.md'.  A writer's
index I is its I-th positional parameter (see
`test-runtime--positional-params') flowing into the type position of a
`(list :type P ...)' that itself reaches the Tape, or into the type
position of a writer already in KNOWN or already found this call.
Iterates to a fixpoint, so a chain of any depth is followed."
  (let* ((defuns
          (cl-loop for form in (test-runtime--file-forms file)
                   when (and (consp form) (memq (car form) '(defun cl-defun)))
                   collect (list (nth 1 form) (nth 2 form) (cons 'progn (nthcdr 3 form)))))
         (found nil) (changed t))
    (while changed
      (setq changed nil)
      (dolist (fn defuns)
        (let* ((name (nth 0 fn)) (params (nth 1 fn)) (body (nth 2 fn)))
          (unless (assq name found)
            (let* ((pool (append known found))
                   (index
                    (cl-some
                     (lambda (p)
                       (and (or (and (test-runtime--reaches-tape-p body)
                                    (test-runtime--param-at-type-p body p))
                               (test-runtime--param-feeds-writer-p body p pool))
                            (test-runtime--writer-index params p)))
                     params)))
              (when index
                (push (cons name index) found)
                (setq changed t)))))))
    found))

(defun test-runtime--writer-discovery-mismatches (known)
  "Return discovered (name . index) pairs that disagree with KNOWN, or
are entirely missing from it."
  (let ((discovered
         (apply #'append
                (mapcar (lambda (file) (test-runtime--discover-file-writers file known))
                        (test-runtime--production-event-source-files)))))
    (cl-remove-if (lambda (pair) (equal (cdr pair) (cdr (assq (car pair) known))))
                 discovered)))

(ert-deftest runtime/helper-writer-list-covers-every-discovered-writer ()
  "Every Tape event helper writer this codebase can be shown to define
is listed in `test-runtime--tape-event-helper-writers' at the right
index -- discovered independently by shape, not by copying the list
back, so a new or wrongly-indexed helper is caught rather than
silently under-scanned by `test-runtime--literal-tape-event-types'."
  (should-not (test-runtime--writer-discovery-mismatches
              test-runtime--tape-event-helper-writers)))

(ert-deftest runtime/discovery-catches-a-writer-at-the-wrong-index ()
  "A real writer recorded at the wrong index fails the completeness
check above -- this is the mutation it exists to catch.  No file is
mutated; only the KNOWN alist passed to the same discovery machinery.
Asserts the mismatch is exactly this writer at its true index, not
merely that some mismatch (possibly an unrelated one) exists -- the
mutation also perturbs `superchat-workspace--fallback-artifact', whose
own call happens to name its argument the same as the parameter this
mutation misindexes."
  (let ((wrong (cons '(superchat-workspace--emit . 0)
                     (cl-remove-if (lambda (e) (eq (car e) 'superchat-workspace--emit))
                                  test-runtime--tape-event-helper-writers))))
    (should (member '(superchat-workspace--emit . 1)
                    (test-runtime--writer-discovery-mismatches wrong)))))

(ert-deftest runtime/closure-knows-literal-production-tape-event-types ()
  "Every literal production Tape event type is known by Closure.
This gate scans top-level production modules and plugin modules; dynamic
families are listed explicitly below because their type is assembled at runtime."
  (let ((types
         (delete-dups
          (apply #'append
                 (mapcar #'test-runtime--literal-tape-event-types
                         (test-runtime--production-event-source-files))))))
    ;; These representatives prove the scan covers the formerly missed plan,
    ;; experience, synthesis, and plugin sources rather than Closure itself.
    (dolist (type '("plan.progress"
                    "experience.candidate-ranking.recorded"
                    "synthesis.approval.decided"
                    "cowork.scope-granted"
                    "digest.scope-granted"
                    "triage.scope-granted"
                    "rule.approved"
                    "rule.removed"))
      (should (member type types)))
    ;; These representatives prove the scan now also covers a literal passed
    ;; to a known Tape event helper writer -- formerly missed because the
    ;; literal never appears next to a `:type' keyword in the source text,
    ;; only as a plain positional argument to the helper's name.  Each family
    ;; below is a real production call, not a mutation fixture.
    (dolist (type '("capability.activated"       ; superchat--configuration-event
                    "topology.changed"           ; superchat--configuration-event
                    "authority.policy.selected"  ; superchat--configuration-event
                    "workspace.bound"            ; superchat--configuration-event
                    "workspace.unbound"          ; superchat--configuration-event
                    "workspace.artifact.added"   ; superchat-workspace--emit
                    "workspace.artifact.changed" ; superchat-workspace--emit
                    "workspace.artifact.removed" ; superchat-workspace--emit
                    "tool.failed"                ; superchat-authority--append-event
                    "turn.completed"             ; superchat-runtime--event
                    "intent.received"))          ; superchat-runtime--append-event
      (should (member type types)))
    (dolist (type types)
      (should (member type superchat-closure--known-event-types)))
    ;; These producers construct the final type from a branch or format string
    ;; inline at the Tape call, so no literal scanner -- direct or through a
    ;; helper writer -- can observe the emitted value directly.
    (dolist (type '("memory.rejected"
                    "memory.reviewed"
                    "team.task.created"
                    "team.task.updated"))
      (should (member type superchat-closure--known-event-types)))
    ;; The dynamic `subagent.%s' family: Closure's knowledge of it and the
    ;; writer's own validation both come from the one shared constant, so
    ;; this checks that constant's expansion rather than a hand-copied list.
    (dolist (suffix superchat-closure--subagent-event-suffixes)
      (should (member (format "subagent.%s" suffix)
                      superchat-closure--known-event-types)))
    ;; S03b: Tape's static inventory (`superchat-tape-event-type-specs') is
    ;; the one source of truth; every type Closure knows about must also be
    ;; a type Tape's append-time validator accepts.
    (dolist (type superchat-closure--known-event-types)
      (should (superchat-tape-event-type-p type)))
    (dolist (type '("subagent.started" "subagent.completed" "subagent.failed"))
      (should (member type
                      (mapcar (lambda (spec) (plist-get spec :type))
                              superchat-tape-event-type-specs))))))

(ert-deftest runtime/gate-detects-unknown-direct-type-literal ()
  "A file with an unrecognized literal next to `:type' is caught.
Uses a scratch probe file, never the working copy: mutating a real
production file to prove this would leave the repository broken."
  (let ((probe (make-temp-file "sc-gate-direct-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--write ()\n"
                    "  (superchat-tape-append-event\n"
                    "   (list :type \"made.up.event\" :session-id \"x\")))\n"))
          (should (member "made.up.event"
                          (test-runtime--literal-tape-event-types probe)))
          (should-not (member "made.up.event"
                              superchat-closure--known-event-types)))
      (delete-file probe))))

(ert-deftest runtime/gate-detects-unknown-helper-writer-argument ()
  "A literal argument to a known Tape event helper writer is caught even
though the file's own text never puts it next to a `:type' keyword.
This is `superchat--configuration-event' with the exact literal from
the review evidence that motivated this fix (`workspace.unbound' typoed
to `unknown.configuration'), against a scratch probe file."
  (let ((probe (make-temp-file "sc-gate-helper-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--write ()\n"
                    "  (superchat--configuration-event \"unknown.configuration\" nil))\n"))
          (should (member "unknown.configuration"
                          (test-runtime--literal-tape-event-types probe)))
          (should-not (member "unknown.configuration"
                              superchat-closure--known-event-types)))
      (delete-file probe))))

(ert-deftest runtime/gate-detects-a-conditional-expression-in-the-type-position ()
  "A helper writer's type argument can be any form, not just a literal
or a bare parameter -- an `(if ...)' choosing between two literals (the
exact shape already in production at `superchat-authority.el:1223-1225')
must still surface every literal inside it, typos included, against a
scratch probe file."
  (let ((probe (make-temp-file "sc-gate-conditional-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--emit (claim)\n"
                    "  (superchat-authority--event-base\n"
                    "   (if claim \"tool.completde\" \"tool.failed\") nil nil))\n"))
          (should (member "tool.completde"
                          (test-runtime--literal-tape-event-types probe)))
          (should-not (member "tool.completde"
                              superchat-closure--known-event-types)))
      (delete-file probe))))

(ert-deftest runtime/gate-discovers-a-backquoted-writer ()
  "A writer whose plist is built with backquote/unquote --
`` `(:type ,type ...)' `` -- is discovered at the right index, the same
as a bare `:type type', against a scratch probe file."
  (let ((probe (make-temp-file "sc-gate-backquote-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--backquoted-writer (type)\n"
                    "  (superchat-tape-append-event\n"
                    "   `(:type ,type :session-id \"x\")))\n"
                    "(defun probe--backquoted-call ()\n"
                    "  (probe--backquoted-writer \"backquote.typoed\"))\n"))
          (should (equal '(probe--backquoted-writer . 0)
                        (assq 'probe--backquoted-writer
                              (test-runtime--discover-file-writers probe nil)))))
      (delete-file probe))))

(ert-deftest runtime/gate-discovers-a-cl-defun-writer ()
  "A `cl-defun' writer is discovered exactly like a `defun' writer,
counting only its positional parameters, against a scratch probe file."
  (let ((probe (make-temp-file "sc-gate-cl-defun-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(cl-defun probe--cl-writer (entry type &optional extra)\n"
                    "  (superchat-tape-append-event (list :type type :session-id entry)))\n"))
          (should (equal '(probe--cl-writer . 1)
                        (assq 'probe--cl-writer
                              (test-runtime--discover-file-writers probe nil)))))
      (delete-file probe))))

(ert-deftest runtime/gate-fails-closed-on-a-truncated-final-form ()
  "A file whose last form is cut off by end-of-file signals, rather
than reading as though nothing were left to see -- a silently accepted
truncated file is exactly the kind of hole this gate exists to close."
  (let ((probe (make-temp-file "sc-gate-truncated-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe (insert "(defun probe--broken (\n"))
          (should-error (test-runtime--file-forms probe)))
      (delete-file probe))))

(ert-deftest runtime/blocking-submit-is-limited-to-sync-adapters ()
  "Only declared synchronous boundary adapters may block on Runtime.
Everything else — Agent family, Surfaces, automation, maintenance hooks
like compaction — must submit asynchronously and return a Run handle."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (sync-adapters '("superchat-runtime.el" ; defines the adapter seam
                         "ob-superchat.el"      ; #+RESULTS: needs a string
                         "superchat-magit.el"   ; inserts a commit message
                         "superchat-rewrite.el"))) ; replaces the region
    (dolist (file (directory-files root t "\\.el\\'"))
      (unless (member (file-name-nondirectory file) sync-adapters)
        (with-temp-buffer
          (insert-file-contents file)
          (should-not
           (re-search-forward
            "(\\s-*superchat-runtime-submit-blocking\\_>" nil t)))))))

(ert-deftest runtime/blocking-pumps-stay-behind-declared-seams ()
  "Busy-wait pumps must not spread beyond their two declared owners.
`superchat-runtime.el' pumps inside the blocking adapter seam and
`superchat-subagent.el' inside the documented map primitive."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (owners '("superchat-runtime.el" "superchat-subagent.el")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((pumps (how-many "(\\s-*accept-process-output\\_>")))
          (if (member (file-name-nondirectory file) owners)
              (should (= 1 pumps))
            (should (= 0 pumps))))))))

(ert-deftest runtime/kernel-requires-stay-inside-kernel ()
  "Kernel require edges stay inside Kernel with no exceptions.
The kernel must stay loadable headless: no UI, chat distribution, or
integration package may be pulled in from below.  Scan both top-level and
call-time `require' forms."
  (let* ((root (file-name-directory (locate-library "superchat-runtime")))
         (kernel '("superchat-core.el" "superchat-preset.el"
                   "superchat-db.el" "superchat-tape.el"
                   "superchat-projection.el" "superchat-closure.el"
                   "superchat-authority.el"
                   "superchat-runtime.el" "superchat-llm.el"
                   "superchat-change-set.el" "superchat-sink.el"
                   "superchat-prompt.el" "superchat-stage.el"
                   "superchat-pipeline.el"))
         (kernel-features (mapcar #'file-name-sans-extension kernel)))
    (dolist (name kernel)
      (with-temp-buffer
        (insert-file-contents (expand-file-name name root))
        (goto-char (point-min))
        (while (re-search-forward "(\\s-*require\\s-+'\\(superchat[[:alnum:]_-]*\\)" nil t)
          (should (member (match-string 1) kernel-features)))))))

(ert-deftest runtime/llm-request-path-stays-headless ()
  "The Kernel LLM port can send a request without loading the distribution."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (locate-library "superchat-llm")))
        (script (make-temp-file "superchat-llm-headless-" nil ".el")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file script
            (insert
             "(progn\n"
             "  (require 'cl-lib)\n"
             "  (require 'superchat-llm)\n"
             "  (when (featurep 'superchat) (kill-emacs 2))\n"
             "  (let ((superchat-llm-backend 'headless-backend)\n"
             "        (superchat-response-timeout nil)\n"
             "        (superchat-buffer-name \" *superchat-llm-headless*\")\n"
             "        provider-called result)\n"
             "    (cl-letf (((symbol-function 'llm-make-chat-prompt)\n"
             "               (lambda (&rest _args) 'headless-prompt))\n"
             ;; S08-fix-3 item 1: `send-request' now reads an attempt's
             ;; tool surface via `llm-chat-prompt-tools'.  Real llm.el is
             ;; not on this headless subprocess's load-path at all (that is
             ;; the point of this probe), so the accessor itself would be
             ;; void without this stub; the request carries no tools.
             "              ((symbol-function 'llm-chat-prompt-tools)\n"
             "               (lambda (_prompt) nil))\n"
             "              ((symbol-function 'llm-chat-streaming)\n"
             "               (lambda (_provider _prompt _partial response _error\n"
             "                        &optional _multi-output)\n"
             "                 (setq provider-called t)\n"
             "                 (funcall response \"headless-ok\")\n"
             "                 'headless-request))\n"
             ;; S08: a tool-free attempt now requires proof of the
             ;; `:streaming' capability rather than streaming by default.
             ;; `llm-capabilities' is unbound in this minimal headless
             ;; load, and the bare symbol backend matches no adapter
             ;; rule, so resolution falls back to non-streaming and
             ;; dispatch takes the async transport instead.
             "              ((symbol-function 'llm-chat-async)\n"
             "               (lambda (_provider _prompt response _error\n"
             "                        &optional _multi-output)\n"
             "                 (setq provider-called t)\n"
             "                 (funcall response \"headless-ok\")\n"
             "                 'headless-request)))\n"
             "      (superchat--llm-generate-answer\n"
             "       \"headless request\" (lambda (value) (setq result value)) #'ignore))\n"
             "    (unless provider-called (kill-emacs 3))\n"
             "    (unless (equal result \"headless-ok\") (kill-emacs 4))\n"
             "    (when (featurep 'superchat) (kill-emacs 5))))\n"))
          (should (= 0 (call-process emacs nil nil nil
                                     "-Q" "--batch" "-L" root "-l" script))))
      (when (file-exists-p script)
        (delete-file script)))))

(ert-deftest runtime/runtime-submit-stays-headless-without-render ()
  "Bare Runtime treats absent Render presentation functions as no-ops."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (locate-library "superchat-runtime")))
        (script (make-temp-file "superchat-runtime-headless-" nil ".el")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file script
            (insert
             ";;; runtime-headless-probe.el --- isolated probe -*- lexical-binding: t; -*-\n"
             "(progn\n"
             "  (require 'cl-lib)\n"
             "  (require 'superchat-runtime)\n"
             "  (when (or (featurep 'superchat-render)\n"
             "            (fboundp 'superchat--update-status)\n"
             "            (fboundp 'superchat--stream-llm-result)\n"
             "            (fboundp 'superchat--process-llm-result))\n"
             "    (kill-emacs 2))\n"
             "  (let ((directory (make-temp-file \"runtime-bare-\" t))\n"
             "        (superchat-runtime--runs (make-hash-table :test #'equal))\n"
             "        (superchat-closure-shadow-enabled nil)\n"
             "        (provider-calls 0) custom-stream custom-delivery)\n"
             "    (unwind-protect\n"
             "        (let ((superchat-data-directory directory))\n"
             "          (superchat-db-close)\n"
             "          (cl-letf (((symbol-function 'superchat--llm-generate-answer)\n"
             "                     (lambda (_prompt _callback stream &rest _args)\n"
             "                       (cl-incf provider-calls)\n"
             "                       (funcall stream \"bare-chunk\")\n"
             "                       (funcall superchat--llm-runtime-observer\n"
             "                                'terminal\n"
             "                                '(:status completed :response \"bare-final\"))\n"
             "                       'bare-request)))\n"
             "            (let* ((custom-buffer (generate-new-buffer \" *runtime-bare-custom*\"))\n"
             "                   (custom (superchat-runtime-submit\n"
             "                            (list :prompt \"custom\" :buffer custom-buffer\n"
             "                                  :session-id \"bare-custom\" :deadline nil\n"
             "                                  :sink (superchat-sink-create\n"
             "                                         :stream (lambda (_run chunk)\n"
             "                                                   (setq custom-stream (plist-get chunk :text)))\n"
             "                                         :final (lambda (_run payload)\n"
             "                                                  (setq custom-delivery\n"
             "                                                        (list (plist-get payload :text) 'completed)))))))\n"
             "                   (default-buffer (generate-new-buffer \" *runtime-bare-default*\"))\n"
             "                   (default (superchat-runtime-submit\n"
             "                             (list :prompt \"default\" :buffer default-buffer\n"
             "                                   :session-id \"bare-default\" :deadline nil\n"
             "                                   :sink (superchat-sink-create)))))\n"
             "              (unless (and (= provider-calls 2)\n"
             "                           (equal custom-stream \"bare-chunk\")\n"
             "                           (equal custom-delivery '(\"bare-final\" completed))\n"
             "                           (eq 'completed (plist-get (superchat-runtime-status\n"
             "                                                        (plist-get custom :run-id))\n"
             "                                                       :status))\n"
             "                           (eq 'completed (plist-get (superchat-runtime-status\n"
             "                                                        (plist-get default :run-id))\n"
             "                                                       :status))\n"
             "                           (plist-get (superchat-runtime--entry\n"
             "                                       (plist-get custom :run-id)) :quiesced)\n"
             "                           (plist-get (superchat-runtime--entry\n"
             "                                       (plist-get default :run-id)) :quiesced))\n"
             "                (kill-emacs 3))\n"
             "              (kill-buffer custom-buffer)\n"
             "              (kill-buffer default-buffer))))\n"
             "      (superchat-db-close)\n"
             "      (delete-directory directory t))))\n"))
          (with-temp-buffer
            (let ((status (call-process emacs nil t nil
                                        "-Q" "--batch" "-L" root "-l" script))
                  (output (buffer-string)))
              (unless (= 0 status)
                (ert-fail (format "Bare Runtime child exited %s:\n%s"
                                  status output))))))
      (when (file-exists-p script)
        (delete-file script)))))

(ert-deftest runtime/runtime-submit-uses-render-functions-when-present ()
  "The standard UI path still receives status, stream, and final delivery.

Mocks `superchat--response-region-stream' (S02-fix-2), not
`superchat--stream-llm-result': the View Sink's `:stream' callback now
normalizes a raw chunk (contract §3.1) and dispatches text/reasoning
directly, so a plain string chunk like \"render-chunk\" reaches
`superchat--response-region-stream' without ever passing through the
legacy string-or-cons-only dispatcher."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-present*"))
            status-messages stream-chunks delivered)
        (with-current-buffer buffer
          (superchat-mode 1)
          (setq-local superchat--session-id "render-session"))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--update-status)
                       (lambda (message &optional _owner) (push message status-messages)))
                      ((symbol-function 'superchat--response-region-stream)
                       (lambda (chunk &optional _owner) (push chunk stream-chunks)))
                      ((symbol-function 'superchat--process-llm-result)
                       (lambda (response tape-id &optional _owner)
                         (setq delivered (list response tape-id))))
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (_prompt _callback stream &rest _args)
                         (funcall stream "render-chunk")
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "render-final"))
                         'render-request)))
              (let* ((handle (superchat-runtime-test-submit
                              (list :prompt "render" :buffer buffer
                                    :session-id "render-session"
                                    :deadline nil)))
                     (entry (superchat-runtime--entry
                             (plist-get handle :run-id))))
                (should (equal '("Prefill / waiting first token… 0s")
                               status-messages))
                (should (equal '("render-chunk") stream-chunks))
                (should (equal "render-final" (car delivered)))
                (should (integerp (cadr delivered)))
                (should (eq 'completed (plist-get entry :status)))
                (should (plist-get entry :quiesced))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/runtime-submit-does-not-swallow-render-errors ()
  "A present Render function's failure is not treated as absence.

S01: a plain buffer is no longer a View, so this registers a temporary
`superchat-runtime-view-sink-function' on a real `superchat-mode' View
instead of mocking the legacy Render entry points directly.  The property
under test is unchanged: an error from a present Sink `:status' callback
still propagates out of `superchat-runtime-submit', rather than being
treated the same as no callback at all."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-error*"))
            (provider-calls 0)
            (superchat-runtime-view-sink-function
             (lambda (_buffer _session _owner)
               (superchat-sink-create
                :status (lambda (_run _status) (error "render status exploded"))))))
        (with-current-buffer buffer
          (superchat-mode 1)
          (setq-local superchat--session-id "render-error-session"))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) (cl-incf provider-calls))))
              (should-error
               (superchat-runtime-test-submit
                (list :prompt "render error" :buffer buffer
                      :session-id "render-error-session" :deadline nil))
               :type 'error)
              (should (= 0 provider-calls)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/runtime-submit-does-not-swallow-render-stream-quit ()
  "A present streaming Render function's quit remains observable.

S01: as above, this registers a temporary `superchat-runtime-view-sink-function'
on a real `superchat-mode' View.  The property under test is unchanged: a
`quit' from a present Sink `:stream' callback still propagates."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-stream-quit*"))
            (provider-calls 0)
            (superchat-runtime-view-sink-function
             (lambda (_buffer _session _owner)
               (superchat-sink-create
                :stream (lambda (_run _chunk)
                          (signal 'quit '("render stream interrupted")))))))
        (with-current-buffer buffer
          (superchat-mode 1)
          (setq-local superchat--session-id "render-quit-session"))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                       (lambda (_prompt _callback stream &rest _args)
                         (cl-incf provider-calls)
                         (funcall stream "render-chunk"))))
              (let (caught)
                (condition-case err
                    (superchat-runtime-test-submit
                     (list :prompt "render quit" :buffer buffer
                           :session-id "render-quit-session" :deadline nil))
                  (quit (setq caught err)))
                (should (equal '(quit "render stream interrupted") caught)))
              (should (= 1 provider-calls)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;; ═══════════════════════════════════════════════════════════
;; S01: superchat-sink.el Runtime integration
;; ═══════════════════════════════════════════════════════════

(defmacro superchat-runtime-test--sink-fixture (&rest body)
  "Run BODY through the real LLM path with only the network transport faked.
Binds RESPOND (call with TEXT to complete the Run), FAIL (call with a
SYMBOL and MESSAGE to fail it), and STREAM-CHUNK (call with a chunk to
stream one, once the async or streaming transport has been invoked).

S09-fix-1 (review item 3): since S09, ordinary chat requests through this
fixture attach the default tool baseline (previously withheld from these
plain, keyword-free prompts under the deleted `on-demand' heuristic).  With
a tools attempt in play, capability resolution needs a real
`streaming-tool-use' answer, not just `streaming'; this fake model has no
real provider behind it to answer either honestly, so this fixture
declares both explicitly via the model-capability override (highest
precedence: override > provider > adapter > fallback) rather than stubbing
`llm-capabilities' itself, since capability resolution is real production
logic this fixture does not seam-replace.  Declared under both keys a
caller might be resolved against -- \"sink-fixture\" (the backend's own
`:chat-model') and \"unknown\" (`superchat--llm-generate-answer''s fallback
lookup key when neither the backend nor the request names a resolvable
model, which individual tests below were already declaring for themselves
before this fixture-level fix) -- and PREPENDED onto whatever a caller's
own dynamic `let' already bound, never resetting it: several tests here
wrap this fixture in their own `superchat-llm-model-capability-overrides'
`let' for a THIRD key, and this fixture's own binding must not shadow it
away for its dynamic extent."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)))
     (superchat-runtime-test--isolated
       (superchat-runtime-test--with-temp-store
         (let* ((superchat-llm-backend (make-llm-ollama :chat-model "sink-fixture"))
                (superchat-response-timeout nil)
                (superchat-llm-model-capability-overrides
                 (append (list (list "sink-fixture" :streaming t :streaming-tool-use t)
                               (list "unknown" :streaming t :streaming-tool-use t))
                         superchat-llm-model-capability-overrides))
                respond fail stream-chunk)
           (cl-letf (((symbol-function 'llm-chat-async)
                      (lambda (_p _q done err &optional _multi)
                        (setq respond done fail err) nil))
                     ((symbol-function 'llm-chat-streaming)
                      (lambda (_p _q partial done err &optional _multi)
                        (setq stream-chunk partial respond done fail err) nil)))
             ,@body))))))

(ert-deftest runtime/sink-final-called-exactly-once-and-after-turn-completed ()
  "The terminal payload callback fires exactly once, after `turn.completed'."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-final-once*"))
           (final-calls 0) turn-completed-seen-p)
      (unwind-protect
          (progn
            (superchat-runtime-test-submit
             (list :prompt "hello" :buffer buffer
                   :session-id "sink-final-once-session" :deadline nil
                   :delivery-function
                   (lambda (&rest _args)
                     (cl-incf final-calls)
                     ;; Read the real, already-durable Tape at the moment
                     ;; the payload callback fires: turn.completed must
                     ;; already be there.
                     (setq turn-completed-seen-p
                           (member "turn.completed"
                                   (mapcar (lambda (e) (plist-get e :type))
                                           (superchat-tape-replay-events
                                            "sink-final-once-session")))))))
            (funcall respond "final text")
            (should (= 1 final-calls))
            (should turn-completed-seen-p))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/sink-run-projection-is-deep-copied-and-has-exact-keys ()
  "`superchat-runtime-sink-run' exposes exactly its documented keys, deep-copied."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-projection*")))
      (unwind-protect
          (let* ((handle
                  (superchat-runtime-test-submit
                   (list :prompt "hello" :buffer buffer
                         :session-id "sink-projection-session" :deadline nil)))
                 (run-id (plist-get handle :run-id)))
            (should
             (equal
              '(:run-id :turn-id :session-id :surface :status :parent-run-id
                :started-at :completed-at :error :pending-steer-count :sink-id)
              (let (keys)
                (cl-loop for (key _value) on (superchat-runtime-sink-run run-id) by #'cddr
                         do (push key keys))
                (nreverse keys))))
            ;; Force a nested value into :error to prove copy-tree isolation:
            ;; mutating one call's result must not corrupt a later call's.
            (superchat-runtime--update run-id :error (list "boom" (list :detail "x")))
            (let* ((first (superchat-runtime-sink-run run-id))
                   (nested (plist-get first :error)))
              (setcar (cadr nested) :mutated)
              (should (equal (list "boom" (list :detail "x"))
                             (plist-get (superchat-runtime-sink-run run-id) :error)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

;; ═══════════════════════════════════════════════════════════
;; S01-fix-3: dead-Sink admission, once-per-seam warnings, blocking
;; delivery conditions
;; ═══════════════════════════════════════════════════════════

(ert-deftest runtime/dead-sink-admission-writes-no-tape-row ()
  "A dead Sink at admission signals before any Tape row, with no active Run."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((buffer (generate-new-buffer " *sink-dead-admission*"))
             (session-id "sink-dead-admission-session")
             (superchat-runtime-view-sink-function
              (lambda (_buffer _session _owner)
                (superchat-sink-create :live-p (lambda () nil))))
             caught)
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (superchat-mode 1)
                (setq-local superchat--session-id session-id))
              (condition-case err
                  (superchat-runtime-test-submit
                   (list :prompt "hello" :buffer buffer
                         :session-id session-id :deadline nil))
                (error (setq caught err)))
              (should caught)
              (should (string-match-p "not live at admission" (error-message-string caught)))
              (should-not (superchat-tape-replay-events session-id))
              (should-not (superchat-runtime-active-runs)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/blocking-submit-resignals-delivery-condition-after-sealing ()
  "A blocking call's own `:delivery-function' condition reaches the caller
after the Run seals, with exactly one `delivery.failed' receipt."
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((superchat-llm-backend (make-llm-ollama :chat-model "blocking-fault"))
             (superchat-response-timeout nil)
             (buffer (generate-new-buffer " *sink-blocking-condition*"))
             (session-id "sink-blocking-condition-session")
             caught)
        (unwind-protect
            (cl-letf (((symbol-function 'llm-chat-async)
                       (lambda (_p _q done _err &optional _multi)
                         (funcall done "final text") nil))
                      ((symbol-function 'llm-chat-streaming)
                       (lambda (_p _q _partial done _err &optional _multi)
                         (funcall done "final text") nil)))
              (condition-case err
                  (superchat-runtime-submit-blocking
                   (list :prompt "hello" :buffer buffer
                         :session-id session-id :deadline nil
                         :sink (superchat-sink-create
                                :final (lambda (&rest _args)
                                         (error "S01-fix-3 blocking delivery fault")))))
                (error (setq caught err)))
              (should caught)
              (should (string-match-p "S01-fix-3 blocking delivery fault"
                                      (error-message-string caught)))
              (let ((types (mapcar (lambda (e) (plist-get e :type))
                                   (superchat-tape-replay-events session-id))))
                (should (member "turn.completed" types))
                (should (= 1 (cl-count "delivery.failed" types :test #'equal)))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;; ═══════════════════════════════════════════════════════════
;; S01-fix-3 item 5: an explicit `:sink' request field (S03a's gap)
;; ═══════════════════════════════════════════════════════════

(ert-deftest runtime/explicit-sink-request-field-receives-every-slot ()
  "An explicit `:sink' request field is used for every Sink slot.

Contract §3.1: the `:stream' callback receives the normalized
`(:kind text|reasoning :text STRING)' payload, not the provider's raw
chunk — Runtime's single stream-emission lambda normalizes it
(S02-fix-3).

Forces the streaming transport (not async) for this fixture's unrecognized
fake model via `superchat-llm-model-capability-overrides', keyed
\"unknown\" (the fallback `superchat--llm-generate-answer' resolves to
when neither the backend nor the request names a model) — without this,
`superchat--llm-generate-answer' auto-collects the global tool list and
llm-ollama's fixture backend reports no `streaming-tool-use' capability,
so it silently falls back to `llm-chat-async' and `stream-chunk' never
fires."
  (let ((superchat-llm-model-capability-overrides
         (list (list "unknown" :streaming t :streaming-tool-use t))))
    (superchat-runtime-test--sink-fixture
      (let* ((buffer (generate-new-buffer " *sink-explicit-field*"))
             statuses stream-chunks final-payload
             (sink (superchat-sink-create
                    :status (lambda (_run status) (push status statuses))
                    :stream (lambda (_run chunk) (push chunk stream-chunks))
                    :final (lambda (_run payload) (setq final-payload payload)))))
        (unwind-protect
            (progn
              (superchat-runtime-test-submit
               (list :prompt "hello" :buffer buffer
                     :session-id "sink-explicit-field-session" :deadline nil
                     :sink sink))
              (when stream-chunk (funcall stream-chunk "a chunk"))
              (funcall respond "final text")
              (should (memq 'waiting statuses))
              (should (memq 'ready statuses))
              (should (equal '((:kind text :text "a chunk")) stream-chunks))
              (should (equal "final text" (plist-get final-payload :text))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/explicit-sink-stream-reasoning-chunk-normalizes ()
  "A provider reasoning chunk reaching an explicit Sink is normalized to
`(:kind reasoning :text STRING)' at Runtime's stream-emission seam
(contract §3.1, S02-fix-3), the same seam covered above for text.

See the capability-override comment on
`runtime/explicit-sink-request-field-receives-every-slot' just above:
the same fixture-only workaround is needed to force the streaming
transport."
  (let ((superchat-llm-model-capability-overrides
         (list (list "unknown" :streaming t :streaming-tool-use t))))
    (superchat-runtime-test--sink-fixture
      (let* ((buffer (generate-new-buffer " *sink-explicit-reasoning*"))
             stream-chunks
             (sink (superchat-sink-create
                    :stream (lambda (_run chunk) (push chunk stream-chunks)))))
        (unwind-protect
            (progn
              (superchat-runtime-test-submit
               (list :prompt "hello" :buffer buffer
                     :session-id "sink-explicit-reasoning-session" :deadline nil
                     :sink sink))
              (when stream-chunk (funcall stream-chunk '(:reasoning "thinking")))
              (funcall respond "final text")
              (should (equal '((:kind reasoning :text "thinking")) stream-chunks)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/explicit-sink-death-before-seal-orphans-run ()
  "An explicit `:sink' that dies before sealing orphans the Run, using its
own `:live-p' for admission and pre-seal liveness."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-explicit-death*"))
           (alive t) statuses orphaned-reasons
           (sink (superchat-sink-create
                  :live-p (lambda () alive)
                  :status (lambda (_run status) (push status statuses))
                  :orphaned (lambda (_run reason) (push reason orphaned-reasons)))))
      (unwind-protect
          (progn
            (superchat-runtime-test-submit
             (list :prompt "hello" :buffer buffer
                   :session-id "sink-explicit-death-session" :deadline nil
                   :sink sink))
            (when stream-chunk (funcall stream-chunk "chunk"))
            (setq alive nil)
            (funcall respond "final text")
            (should (equal '(sink-lost) orphaned-reasons))
            (should (memq 'orphaned statuses))
            (should-not (memq 'stopped statuses)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/explicit-sink-nil-value-signals-before-tape-row ()
  "A `:sink' key present with a nil value errors — presence, not truthiness,
selects the explicit-Sink path, and a nil value fails `superchat-sink-p'."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((buffer (generate-new-buffer " *sink-explicit-nil-value*"))
             (session-id "sink-explicit-nil-value-session")
             caught)
        (unwind-protect
            (progn
              (condition-case err
                  (superchat-runtime-test-submit
                   (list :prompt "hello" :buffer buffer
                         :session-id session-id :deadline nil
                         :sink nil))
                (error (setq caught err)))
              (should caught)
              (should (string-match-p ":sink must be a superchat-sink" (error-message-string caught)))
              (should-not (superchat-tape-replay-events session-id))
              (should-not (superchat-runtime-active-runs)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/sink-death-before-seal-orphans-run ()
  "A Sink that dies before sealing orphans the Run instead of finishing it."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-death-before-seal*"))
           (alive t)
           statuses orphaned-reasons
           (superchat-runtime-view-sink-function
            (lambda (_buffer _session _owner)
              (superchat-sink-create
               :live-p (lambda () alive)
               :status (lambda (_run status) (push status statuses))
               :orphaned (lambda (_run reason) (push reason orphaned-reasons))))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (superchat-mode 1)
              (setq-local superchat--session-id "sink-death-session"))
            (superchat-runtime-test-submit
             (list :prompt "hello" :buffer buffer
                   :session-id "sink-death-session" :deadline nil))
            (when stream-chunk (funcall stream-chunk "chunk"))
            (setq alive nil)
            (funcall respond "final text")
            (should (equal '(sink-lost) orphaned-reasons))
            (should (memq 'orphaned statuses))
            (should-not (memq 'stopped statuses))
            (let* ((events (superchat-tape-replay-events "sink-death-session"))
                   (cancel-event
                    (cl-find "run.cancelled" events
                             :key (lambda (e) (plist-get e :type)) :test #'equal)))
              (should cancel-event)
              (should (equal "sink-lost" (plist-get (plist-get cancel-event :payload) :reason)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/terminal-status-error-still-delivers-final-and-records-delivery-failed ()
  "A terminal `:status' callback error still lets the payload callback run."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-status-error*"))
           final-payload
           (superchat-runtime-view-sink-function
            (lambda (_buffer _session _owner)
              (superchat-sink-create
               :status (lambda (_run status)
                         (when (eq status 'ready) (error "status callback exploded")))
               :final (lambda (_run payload) (setq final-payload payload))))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (superchat-mode 1)
              (setq-local superchat--session-id "sink-status-error-session"))
            (superchat-runtime-test-submit
             (list :prompt "hello" :buffer buffer
                   :session-id "sink-status-error-session" :deadline nil))
            (funcall respond "final text")
            (should (equal "final text" (plist-get final-payload :text)))
            (should (member "delivery.failed"
                            (mapcar (lambda (e) (plist-get e :type))
                                    (superchat-tape-replay-events "sink-status-error-session")))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/terminal-status-quit-delivers-payload-then-resignals-quit ()
  "A terminal `:status' quit still lets the payload run, then re-signals."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-status-quit*"))
           final-called
           (superchat-runtime-view-sink-function
            (lambda (_buffer _session _owner)
              (superchat-sink-create
               :status (lambda (_run status)
                         (when (eq status 'ready) (signal 'quit '("status quit"))))
               :final (lambda (_run _payload) (setq final-called t))))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (superchat-mode 1)
              (setq-local superchat--session-id "sink-status-quit-session"))
            (superchat-runtime-test-submit
             (list :prompt "hello" :buffer buffer
                   :session-id "sink-status-quit-session" :deadline nil))
            (let (caught)
              (condition-case err
                  (funcall respond "final text")
                (quit (setq caught err)))
              (should (equal '(quit "status quit") caught)))
            (should final-called)
            (let ((types (mapcar (lambda (e) (plist-get e :type))
                                 (superchat-tape-replay-events "sink-status-quit-session"))))
              (should (member "turn.completed" types))
              (should (member "delivery.failed" types))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/terminal-payload-quit-records-receipt-then-resignals ()
  "A terminal payload callback quit records a receipt, then re-signals."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-payload-quit*"))
           (superchat-runtime-view-sink-function
            (lambda (_buffer _session _owner)
              (superchat-sink-create
               :final (lambda (_run _payload) (signal 'quit '("payload quit")))))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (superchat-mode 1)
              (setq-local superchat--session-id "sink-payload-quit-session"))
            (let* ((handle
                    (superchat-runtime-test-submit
                     (list :prompt "hello" :buffer buffer
                           :session-id "sink-payload-quit-session" :deadline nil)))
                   (run-id (plist-get handle :run-id))
                   caught)
              (condition-case err
                  (funcall respond "final text")
                (quit (setq caught err)))
              (should (equal '(quit "payload quit") caught))
              (let ((types (mapcar (lambda (e) (plist-get e :type))
                                   (superchat-tape-replay-events "sink-payload-quit-session"))))
                (should (member "turn.completed" types))
                (should (member "delivery.failed" types)))
              (should (plist-get (superchat-runtime--entry run-id) :quiesced))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/audit-incomplete-view-shows-response-then-notice ()
  "The default chat View shows the response, then the audit-incomplete notice.
Reuses the setup of `runtime/audit-incomplete-reuses-response-row-with-readable-warning',
which must itself keep passing unchanged.  Triggers audit-incomplete with a
deliberately injected Tape append failure (the seam rule's permitted
injection), on the terminal `run.completed' append specifically, so
`response.completed' has already durably landed (giving a non-nil tape-id)
before the failure — the actual real-world shape of this path, not a
replay substitution."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--sink-fixture
    (let* ((session-id "sink-audit-incomplete-session")
           (buffer-name (superchat--session-buffer-name session-id))
           (superchat-buffer-name buffer-name)
           (append-record (symbol-function 'superchat-tape-append-event-record)))
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event)
                       (if (equal "run.completed" (plist-get event :type))
                           (error "S01-fix-3 injected append failure")
                         (funcall append-record event)))))
            (with-current-buffer (get-buffer-create buffer-name)
              (superchat-mode 1)
              (setq-local superchat--session-id session-id)
              (setq-local superchat--conversation-history nil))
            (with-current-buffer buffer-name
              (superchat--insert-prompt)
              (superchat--prepare-for-response))
            (let* ((handle
                    (superchat-runtime-test-submit
                     (list :input "audit" :clean-input "audit" :prompt "audit"
                           :session-id session-id :buffer (get-buffer buffer-name)
                           :buffer-name buffer-name)))
                   (run-id (plist-get handle :run-id)))
              (funcall respond "the durable response")
              (should (eq 'audit-incomplete
                          (plist-get (superchat-runtime-status run-id) :status)))
              (let* ((text (with-current-buffer buffer-name (buffer-string)))
                     (response-at (string-match "the durable response" text))
                     (notice-at (string-match "\\[Audit incomplete:" text))
                     (count 0) (start 0))
                (should response-at)
                (should notice-at)
                (should (< response-at notice-at))
                (while (setq start (string-search "[Audit incomplete:" text start))
                  (setq count (1+ count) start (1+ start)))
                (should (= 1 count)))
              ;; The real replay carries the durable evidence this exact
              ;; injected failure produces: response.completed landed before
              ;; the injected run.completed failure, and the recovery journal
              ;; sealed run.failed/turn.completed as audit-incomplete.
              (let* ((events (superchat-tape-replay-events session-id))
                     (types (mapcar (lambda (e) (plist-get e :type)) events))
                     (run-failed (cl-find "run.failed" events
                                          :key (lambda (e) (plist-get e :type))
                                          :test #'equal)))
                (should (member "response.completed" types))
                (should run-failed)
                (should (equal "audit-incomplete"
                               (plist-get (plist-get run-failed :payload) :status)))
                (should (member "turn.completed" types)))))
        (when (get-buffer buffer-name) (kill-buffer buffer-name))))))

(ert-deftest runtime/view-sink-session-switch-completes-durably-without-rendering ()
  "R3-L7 end to end: a Run whose View's Session switches away while the
provider is still held (the terminal delivery arrives later) completes
durably.  The chat View Sink declares no `:live-p' (S02), so Runtime
never treats it as dead; the Sink's own `:final' callback checks the
View's live Session itself and renders nothing once it no longer
matches.  Asserts the durable event sequence — `response.completed',
`run.completed', `turn.completed' — on the ORIGINAL Session's Tape, no
`run.cancelled :reason sink-lost', Runtime status `completed', and the
View buffer left byte-identical to how it stood right before the switch."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer (generate-new-buffer " *sink-session-switch*"))
           (session-id "sink-session-switch-session")
           before-text handle run-id)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (superchat-mode 1)
              (setq-local superchat--session-id session-id)
              (let ((superchat-buffer-name (buffer-name buffer)))
                (superchat--insert-prompt)))
            (setq handle
                  (superchat-runtime-test-submit
                   (list :prompt "hello" :buffer buffer
                         :session-id session-id :deadline nil)))
            (setq run-id (plist-get handle :run-id))
            ;; "Hold the provider": `submit' has already returned (its own
            ;; synchronous `waiting'/mode-indicator status render is done);
            ;; neither `respond' nor `fail' has fired yet, so the terminal
            ;; result is still pending.  Capture whatever the View shows at
            ;; this point verbatim -- the assertion below is that nothing
            ;; changes after the switch, not any particular status text.
            (with-current-buffer buffer
              (setq before-text (buffer-string))
              ;; The View's Session switches away before the provider
              ;; finishes -- the Sink's own `current-p' now sees a mismatch.
              (setq-local superchat--session-id "sink-session-switch-other"))
            (funcall respond "SESSION_SWITCH_FINAL")
            (with-current-buffer buffer
              (should (equal before-text (buffer-string))))
            (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status)))
            (let* ((events (superchat-tape-replay-events session-id))
                   (types (mapcar (lambda (e) (plist-get e :type)) events)))
              (should (member "response.completed" types))
              (should (member "run.completed" types))
              (should (member "turn.completed" types))
              (should-not
               (cl-find-if
                (lambda (e)
                  (and (equal "run.cancelled" (plist-get e :type))
                       (equal "sink-lost"
                              (plist-get (plist-get e :payload) :reason))))
                events))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/view-sink-killed-buffer-completes-durably-without-rendering ()
  "R3-L7 end to end: a Run whose View buffer is killed while the provider
is still held completes durably and creates no new buffer for that
Session.  Same durable-completion assertions as the Session-switch case
above, plus: no buffer named for this Session/View exists afterward
(the Sink's `current-p' fails on `buffer-live-p' first, so nothing in
its callbacks ever calls `get-buffer-create')."
  (superchat-runtime-test--sink-fixture
    (let* ((buffer-name (generate-new-buffer-name " *sink-killed-view*"))
           (buffer (get-buffer-create buffer-name))
           (session-id "sink-killed-view-session")
           handle run-id)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (superchat-mode 1)
              (setq-local superchat--session-id session-id)
              (let ((superchat-buffer-name buffer-name))
                (superchat--insert-prompt)))
            (setq handle
                  (superchat-runtime-test-submit
                   (list :prompt "hello" :buffer buffer
                         :session-id session-id :deadline nil)))
            (setq run-id (plist-get handle :run-id))
            ;; "Hold the provider": same as the Session-switch case above --
            ;; `submit' has returned; neither `respond' nor `fail' has fired.
            (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))
            (funcall respond "KILLED_VIEW_FINAL")
            (should-not (get-buffer buffer-name))
            (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status)))
            (let* ((events (superchat-tape-replay-events session-id))
                   (types (mapcar (lambda (e) (plist-get e :type)) events)))
              (should (member "response.completed" types))
              (should (member "run.completed" types))
              (should (member "turn.completed" types))
              (should-not
               (cl-find-if
                (lambda (e)
                  (and (equal "run.cancelled" (plist-get e :type))
                       (equal "sink-lost"
                              (plist-get (plist-get e :payload) :reason))))
                events))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/eager-distribution-does-not-require-integrations ()
  "Integration packages load on demand, except four reviewed call-time edges.
The integration gate permits exactly the lazy `require' calls named in
`test-runtime--integration-require-exemptions', each keyed by the file,
the required feature, and the enclosing top-level `defun' rather than a
line number, so an edit that only shifts lines elsewhere in the file
cannot break or silently widen this gate.  Integration packages may
require another integration package within their declared boundary; all
other non-plugin files must use autoloads and registries.  Reads both
top-level and call-time `require' forms via `test-runtime--file-requires',
not a text scan."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (plugins '("superchat-rewrite" "superchat-compact" "superchat-workflow"
                   "superchat-subagent" "superchat-agents-panel"
                   "superchat-genui" "superchat-magit" "ob-superchat"
                   "superchat-emacsclient")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (let ((base (file-name-sans-extension
                   (file-name-nondirectory file))))
        (unless (member base plugins)
          (dolist (entry (test-runtime--file-requires file))
            (let ((feature (car entry)) (enclosing (cdr entry)))
              (should
               (or (not (member feature plugins))
                   (test-runtime--integration-require-exemption-p
                    (file-name-nondirectory file) feature enclosing))))))))))

(ert-deftest runtime/eager-gate-catches-a-require-moved-to-a-different-defun ()
  "A reviewed require is only exempt inside its named enclosing `defun'.
Uses a scratch probe file, never the working copy: mutating a real
production file to prove this would leave the repository broken."
  (let ((probe (make-temp-file "sc-gate-require-moved-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert "(defun probe--unreviewed-caller ()\n"
                    "  (require 'superchat-subagent nil t))\n"))
          (let ((entry (car (test-runtime--file-requires probe))))
            (should (equal "superchat-subagent" (car entry)))
            (should (eq 'probe--unreviewed-caller (cdr entry)))
            (should-not
             (test-runtime--integration-require-exemption-p
              "superchat-tools.el" (car entry) (cdr entry)))))
      (delete-file probe))))

(ert-deftest runtime/eager-gate-survives-the-exempted-call-shifting-lines ()
  "Padding a reviewed call with blank lines never breaks its exemption --
the gate is keyed by the enclosing `defun', not a line number.  Uses a
scratch probe file, never the working copy."
  (let ((probe (make-temp-file "sc-gate-require-padded-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file probe
            (insert (make-string 50 ?\n))
            (insert "(defun superchat-tool-render-ui (component)\n"
                    "  (require 'superchat-genui nil t))\n"))
          (let ((entry (car (test-runtime--file-requires probe))))
            (should (equal "superchat-genui" (car entry)))
            (should (eq 'superchat-tool-render-ui (cdr entry)))
            (should
             (test-runtime--integration-require-exemption-p
              "superchat-tools.el" (car entry) (cdr entry)))))
      (delete-file probe))))

(ert-deftest runtime/request-approval-function-reaches-tool-context ()
  "A Request-scoped :approval-function must reach the tool context."
  (superchat-runtime-test--isolated
    (should-error
     (superchat-runtime--validate-request
      '(:prompt "hi" :approval-function "not-a-function")))
    (let* ((approval (lambda (_request) t))
           captured)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _)
                   (setq captured
                         (plist-get superchat--tool-execution-context
                                    :approval-function))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            (list :status 'completed :response "ok")))))
        (superchat-runtime-test-submit
         (append (superchat-runtime-test--request)
                 (list :approval-function approval)))
        (should (eq approval captured))))))

(ert-deftest runtime/experience-action-policy-reaches-authority-context ()
  "A blocked View's explicit action policy reaches the Authority seam."
  (superchat-runtime-test--isolated
    (let* ((envelope
            '(:contract "context-continuity-v0"
              :status blocked
              :receipt (:tape-head 1)
              :experience-view (:status blocked)
              :experience-action-policy
              (:dependent-tools ("minimal_refresh"))))
           captured)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _)
                   (setq captured
                         (plist-get superchat--tool-execution-context
                                    :context-envelope))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            (list :status 'completed :response "ok")))))
        (superchat-runtime-test-submit
         (append (superchat-runtime-test--request)
                 (list :context-envelope envelope)))
        (should (equal envelope captured))))))

(ert-deftest runtime/rejects-malformed-experience-action-policy ()
  "An action policy must explicitly name non-empty tool strings."
  (should-error
   (superchat-runtime--validate-request
    '(:prompt "hello"
      :context-envelope
      (:contract "context-continuity-v0"
       :status blocked
       :receipt (:tape-head 1)
       :experience-action-policy (:dependent-tools (minimal_refresh)))))))

(ert-deftest runtime/revision-recovery-policy-advances-on-successful-tools-only ()
  "Typed revision recovery advances info -> read -> satisfied, once."
  (superchat-runtime-test--isolated
    (let* ((run-id "run-revision-recovery")
           (policy '(:kind "workspace-revision-recovery"
                     :phase "awaiting-info"
                     :required-tool "workspace_info"
                     :dependent-tools ("workspace_read")
                     :artifact-id "artifact-1"))
           (envelope (list :experience-action-policy policy)))
      (superchat-runtime--put
       (list :run-id run-id
             :status 'running-model
             :context-envelope envelope))
      ;; An info result for a different Artifact must not satisfy the policy.
      (superchat-runtime--recovery-tool-completed
       run-id '(:tool-name "workspace_info"
                :result "active Artifact artifact-other revision=rev-other"))
      (should (equal "awaiting-info"
                     (plist-get
                      (plist-get
                       (plist-get (superchat-runtime--entry run-id)
                                  :context-envelope)
                       :experience-action-policy)
                      :phase)))
      (superchat-runtime--recovery-tool-completed
       run-id '(:tool-name "workspace_info"
                :result "active Artifact artifact-1 revision=rev-1"))
      (should (equal "awaiting-read"
                     (plist-get
                      (plist-get
                       (plist-get (superchat-runtime--entry run-id)
                                  :context-envelope)
                       :experience-action-policy)
                      :phase)))
      (superchat-runtime--recovery-tool-completed
       run-id '(:tool-name "workspace_read"
                :args ("artifact-1" "rev-1")
                :result "content"))
      (let ((next-policy
             (plist-get
              (plist-get (superchat-runtime--entry run-id)
                         :context-envelope)
              :experience-action-policy)))
        (should (equal "satisfied" (plist-get next-policy :phase)))
        (should (equal "artifact-1"
                       (plist-get (plist-get next-policy :satisfied-by)
                                  :artifact-id))))
      ;; Terminal Runs do not accept late completion callbacks.
      (superchat-runtime--update run-id :status 'failed)
      (superchat-runtime--recovery-tool-completed
       run-id '(:tool-name "workspace_info"
                :result "active Artifact artifact-1 revision=rev-2"))
      (should (equal "satisfied"
                     (plist-get
                      (plist-get
                       (plist-get (superchat-runtime--entry run-id)
                                  :context-envelope)
                       :experience-action-policy)
                      :phase))))))

(ert-deftest runtime/session-capability-activation-rejects-conflict ()
  "Activating a Preset under a conflicting Capability name must fail."
  (with-temp-buffer
    (let ((preset (superchat-preset-from-plist
                   (list :name "coder"
                         :profile-id "coder"
                         :type 'agent
                         :capability "code"
                         :source 'builtin))))
      (should-error (superchat--activate-capability preset "research"))
      (should (eq preset (superchat--activate-capability preset "code"))))))

(ert-deftest runtime/production-sources-do-not-restore-agent-mode ()
  "Capability and Topology must not collapse back into `agent-mode'."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not
         (and (search-forward "agent-mode" nil t)
              (file-name-nondirectory file)))))))

(ert-deftest runtime/production-does-not-write-legacy-agent-identities ()
  "Production code may read legacy identity, but must not write it."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (search-forward "agent-id" nil t))
        (goto-char (point-min))
        (should-not (search-forward "subagent-id" nil t))
        (goto-char (point-min))
        (should-not (search-forward "subagent_id" nil t))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "superchat-subagent.el" root))
      (should (= 1 (how-many "\\_<agent_id\\_>")))
      (should (search-forward "(alist-get 'agent_id payload)" nil t)))))

(ert-deftest runtime/profile-storage-uses-domain-identity-name ()
  "Profile storage must not expose configuration-source terminology."
  (let ((root (file-name-directory (locate-library "superchat-runtime"))))
    (dolist (file (directory-files root t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (search-forward "declared-profile-id" nil t))))))

(ert-deftest runtime/submit-owns-lifecycle-and-public-status ()
  "A successful submit emits one ordered lifecycle and hides private state."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          timeline)
      (setq superchat-post-turn-functions
            (list (lambda (_turn) (push 'post-turn timeline))))
      (setq superchat-run-completed-functions
            (list (lambda (_status) (push 'run-hook timeline))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (push 'turn-hook timeline))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (intern (plist-get event :type)) timeline)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer tape-id &optional _owner)
                   (push (list 'delivered answer tape-id) timeline)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'request 'provider-request)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done"))
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--view-request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'accepted (plist-get handle :status)))
          (should (eq 'completed (plist-get status :status)))
          (should-not (plist-member status :turn))
          (should-not (plist-member status :transport-request))
          (should
           (equal
            (nreverse timeline)
            '(intent\.received run\.started response\.completed
              post-turn
              run\.completed turn\.completed
              (delivered "done" 3)
              run-hook turn-hook))))))))

(ert-deftest runtime/public-status-projects-attempt-context-usage ()
  "Provider-attempt observations update the public Run status."
  (superchat-runtime-test--isolated
    (let (observer handle)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0)) (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer))))
        (setq handle (superchat-runtime-test-submit
                      (superchat-runtime-test--request)))
        (funcall observer
                 'attempt-started
                 '(:attempt 2 :attempt-reason tool-followup
                   :model "model-x" :context-used 6800 :context-limit 8192
                   :context-limit-source lm-studio-runtime
                   :model-phase waiting-first-token))
        (let ((status (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "model-x" (plist-get status :model)))
          (should (= 2 (plist-get status :attempt)))
          (should (eq 'tool-followup (plist-get status :attempt-reason)))
          (should (= 6800 (plist-get status :context-used)))
          (should (eq 'lm-studio-runtime
                      (plist-get status :context-limit-source)))
          (should (eq 'waiting-first-token
                      (plist-get status :model-phase)))
          (should (plist-get status :context-estimated-p)))
        (funcall observer 'model-phase 'reasoning)
        (should (eq 'reasoning
                    (plist-get
                     (superchat-runtime-status (plist-get handle :run-id))
                     :model-phase)))
        (funcall observer 'model-phase 'answer-streaming)
        (should (eq 'answer-streaming
                    (plist-get
                     (superchat-runtime-status (plist-get handle :run-id))
                     :model-phase)))
        (funcall observer 'context-recovery-started '(:reason threshold))
        (should (eq 'compacting
                    (plist-get
                     (superchat-runtime-status (plist-get handle :run-id))
                     :context-recovery)))
        (funcall observer 'context-recovery-ended '(:reason threshold))
        (should-not
         (plist-get (superchat-runtime-status (plist-get handle :run-id))
                    :context-recovery))
        (funcall observer 'usage
                 '(:attempt 2 :input-tokens 7000 :output-tokens 500))
        (let ((status (superchat-runtime-status (plist-get handle :run-id))))
          (should (= 7500 (plist-get status :context-used)))
          (should (= 7000 (plist-get status :input-tokens)))
          (should (= 500 (plist-get status :output-tokens)))
          (should-not (plist-get status :context-estimated-p)))
        (funcall observer 'usage '(:attempt 2 :input-tokens 7100))
        (let ((status (superchat-runtime-status (plist-get handle :run-id))))
          (should (= 7100 (plist-get status :context-used)))
          (should (plist-get status :context-estimated-p)))
        (funcall observer 'context-recovery-started '(:reason overflow))
        (superchat-runtime-cancel (plist-get handle :run-id))
        (let ((status (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'cancelled (plist-get status :status)))
          (should-not (plist-get status :context-recovery)))))))

(ert-deftest runtime/evidence-is-durable-before-provider-and-first-output-once ()
  "Eligible Runs persist their prior and first observation synchronously."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events timeline evidence-context)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events (append events (list (copy-tree event))))
                   (setq timeline
                         (append timeline
                                 (list (plist-get event :type))))
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq evidence-context
                         (copy-tree superchat--llm-evidence-context))
                   (let ((snapshot-id
                          (funcall
                           superchat--llm-runtime-observer
                           'decision-snapshot
                           '(:attempt 1 :attempt-reason "initial"
                             :provider "mock"
                             :model "mock-model"
                             :prepared-envelope
                             (:format "superchat-prepared-envelope-v0"
                              :attempt-reason "initial"
                              :messages
                              [(:content "exact provider prompt")])))))
                     (should (and (stringp snapshot-id)
                                  (not (string-empty-p snapshot-id)))))
                   (setq timeline (append timeline '(provider-called)))
                   (funcall superchat--llm-runtime-observer
                            'first-output
                            '(:attempt 1 :kind "text"
                              :text "model-chosen"))
                   (funcall superchat--llm-runtime-observer
                            'first-output
                            '(:attempt 1 :kind "text" :text "ignored"))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((envelope '(:contract "context-continuity-v0"
                           :status satisfied
                           :receipt (:tape-head 7)))
               (request
                (append
                 (superchat-runtime-test--request)
                 (list :context-envelope envelope
                       :evidence-contract "epistemic-learning-v0")))
               (handle (superchat-runtime-test-submit request))
               (entry (superchat-runtime--entry
                       (plist-get handle :run-id)))
               (snapshot
                (cl-find "decision.snapshot.recorded" events
                         :key (lambda (event) (plist-get event :type))
                         :test #'equal))
               (first
                (cl-find "output.first.recorded" events
                         :key (lambda (event) (plist-get event :type))
                         :test #'equal)))
          (should
           (equal '("intent.received" "run.started"
                    "decision.snapshot.recorded" provider-called
                    "output.first.recorded" "response.completed"
                    "run.completed" "turn.completed")
                  timeline))
          (should (plist-get snapshot :requires-payload))
          (should
           (string-match-p
            "exact provider prompt"
            (plist-get (plist-get snapshot :payload)
                       :prepared-envelope-json)))
          (should (plist-get first :requires-payload))
          (should
           (string-match-p
            "model-chosen"
            (plist-get (plist-get first :payload) :observed-json)))
          (should
           (string-match-p
            "text"
            (plist-get (plist-get first :payload) :observed-json)))
          (should
           (equal (plist-get (plist-get first :payload) :observed-json)
                  (plist-get
                   (superchat-tape--redact (plist-get first :payload))
                   :observed-json)))
          (should (= 1 (cl-count "output.first.recorded" timeline
                                 :test #'equal)))
          (should (equal envelope
                         (plist-get evidence-context :context-envelope)))
          (should (equal envelope
                         (plist-get (superchat-turn-state
                                     (plist-get entry :turn))
                                    :context-envelope))))))))

(ert-deftest runtime/secret-decision-evidence-blocks-provider ()
  "Secret-valued opaque prior data fails before the provider effect."
  (superchat-runtime-test--isolated
    (let ((next-id 0) provider-called failed-entry condition)
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (&rest _args) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall
                   superchat--llm-runtime-observer 'decision-snapshot
                    (list
                     :attempt 1 :attempt-reason "initial"
                     :provider "mock" :model "mock"
                     :prepared-envelope
                     (list
                      :format "superchat-prepared-envelope-v0"
                      :attempt-reason "initial"
                      :messages
                      (vector
                       (list
                        :content
                        (lambda () "TOP-SECRET-DECISION-019"))))))
                   (setq provider-called t))))
        (setq condition
              (should-error
               (superchat-runtime-test-submit
                (append
                 (superchat-runtime-test--request)
                 '(:evidence-contract "epistemic-learning-v0"
                   :context-envelope
                   (:contract "context-continuity-v0" :status satisfied
                    :receipt (:tape-head 1)))))
               :type 'superchat-llm-evidence-recording-error))
        (maphash (lambda (_run-id entry) (setq failed-entry entry))
                 superchat-runtime--runs)
        (should-not provider-called)
        (should-not
         (string-match-p "TOP-SECRET-DECISION-019"
                         (error-message-string condition)))
        (should-not
         (string-match-p "TOP-SECRET-DECISION-019"
                         (or (plist-get failed-entry :error) "")))
        (should (eq 'audit-incomplete (plist-get failed-entry :status)))))))

(ert-deftest runtime/noneligible-run-ignores-evidence-observer-events ()
  "Ordinary Runs retain their existing lifecycle even if transport reports evidence."
  (superchat-runtime-test--isolated
    (let ((next-id 0) event-types)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) event-types)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'decision-snapshot '(:prompt "ignored"))
                   (funcall superchat--llm-runtime-observer
                            'first-output '(:kind "text" :text "ignored"))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit (superchat-runtime-test--request))
        (should
         (equal '("intent.received" "run.started" "response.completed"
                  "run.completed" "turn.completed")
                (nreverse event-types)))))))

(ert-deftest runtime/late-first-output-keeps-attempt-causation ()
  "A late output causes from its own snapshot without rewinding the Run head."
  (superchat-runtime-test--isolated
    (let ((next-id 0) events head-after-late)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events (append events (list (copy-tree event))))
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (dolist (attempt '((1 "initial")
                                      (2 "tool-protocol-retry")))
                     (funcall
                      superchat--llm-runtime-observer 'decision-snapshot
                      (list :attempt (car attempt)
                            :attempt-reason (cadr attempt)
                            :provider "mock" :model "mock"
                            :prepared-envelope
                            (list
                             :format "superchat-prepared-envelope-v0"
                             :attempt-reason (cadr attempt)))) )
                   (funcall superchat--llm-runtime-observer 'first-output
                            '(:attempt 1 :kind "text" :text "late one"))
                   (setq head-after-late
                         (plist-get
                          (superchat-runtime--entry
                           (plist-get superchat--tool-execution-context
                                      :run-id))
                          :causation-id))
                   (funcall superchat--llm-runtime-observer 'first-output
                            '(:attempt 2 :kind "text" :text "current two"))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit
         (append
          (superchat-runtime-test--request)
          '(:evidence-contract "epistemic-learning-v0"
            :context-envelope
            (:contract "context-continuity-v0" :status satisfied
             :receipt (:tape-head 1)))))
        (let* ((decisions
                (seq-filter
                 (lambda (event)
                   (equal "decision.snapshot.recorded"
                          (plist-get event :type)))
                 events))
               (outputs
                (seq-filter
                 (lambda (event)
                   (equal "output.first.recorded" (plist-get event :type)))
                 events)))
          (should (= 2 (length decisions)))
          (should (= 2 (length outputs)))
          (should (equal (plist-get (nth 0 decisions) :event-id)
                         (plist-get (nth 0 outputs) :causation-id)))
          (should (equal (plist-get (nth 1 decisions) :event-id)
                         (plist-get (nth 1 outputs) :causation-id)))
          (should (equal (plist-get (nth 1 decisions) :event-id)
                         head-after-late)))))))

(ert-deftest runtime/decision-prewrite-failure-closes-without-provider-effect ()
  "Failed mandatory evidence seals audit-incomplete before provider code runs."
  (superchat-runtime-test--isolated
    (let ((next-id 0) provider-called failed-entry)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "decision.snapshot.recorded"
                              (plist-get event :type))
                       (error "evidence store unavailable")
                     (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall
                    superchat--llm-runtime-observer
                    'decision-snapshot
                    '(:attempt 1 :attempt-reason "initial"
                      :provider "mock" :model "mock"
                      :prepared-envelope
                      (:format "superchat-prepared-envelope-v0"
                       :attempt-reason "initial" :messages [])))
                   (setq provider-called t))))
        (should-error
         (superchat-runtime-test-submit
          (append
           (superchat-runtime-test--request)
           '(:evidence-contract "epistemic-learning-v0"
             :context-envelope
             (:contract "context-continuity-v0" :status satisfied
              :receipt (:tape-head 1))))))
        (maphash (lambda (_run-id entry) (setq failed-entry entry))
                 superchat-runtime--runs)
        (should-not provider-called)
        (should (eq 'audit-incomplete
                    (plist-get failed-entry :status)))))))

(ert-deftest runtime/rejects-unknown-evidence-contract ()
  "Evidence capture is opt-in to one versioned contract."
  (should-error
   (superchat-runtime--validate-request
    '(:prompt "hello" :evidence-contract "future-contract"))))

(ert-deftest runtime/evidence-contract-rejects-malformed-view-before-provider ()
  "Malformed continuity receipts cannot reach model transport."
  (dolist (envelope
           '((:contract "other-view-v0" :status satisfied
              :receipt (:tape-head 1))
             (:contract "context-continuity-v0" :status invented
              :receipt (:tape-head 1))
             (:contract "context-continuity-v0" :status satisfied)
             (:contract "context-continuity-v0" :status satisfied
              :receipt nil)))
    (let (provider-called)
      (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (setq provider-called t))))
        (should-error
         (superchat-runtime-test-submit
          (append
           (superchat-runtime-test--request)
           (list :evidence-contract "epistemic-learning-v0"
                 :context-envelope envelope))))
        (should-not provider-called)))))

(ert-deftest runtime/evidence-accessor-failure-audits-before-provider ()
  "An unreadable prepared prompt seals audit-incomplete before transport."
  (superchat-runtime-test--isolated
    (let ((superchat-llm-backend '(mock-backend))
          (superchat-response-timeout nil)
          (provider-calls 0)
          failed-entry)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((next-id 0))
                   (lambda (_event) (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--collect-llm-tools)
                 (lambda (&rest _args) nil))
                ((symbol-function 'superchat--detect-response-mode)
                 (lambda (&rest _args) 'plain))
                ((symbol-function 'superchat--show-response-mode-indicator)
                 #'ignore)
                ((symbol-function 'superchat--get-adjusted-timeout)
                 (lambda (&rest _args) nil))
                ((symbol-function 'superchat--effective-llm-backend)
                 (lambda (&optional _model) superchat-llm-backend))
                ((symbol-function 'superchat--provider-name)
                 (lambda (_provider) "mock"))
                ((symbol-function 'superchat--provider-chat-model)
                 (lambda (_provider) "mock-model"))
                ((symbol-function 'superchat--build-llm-prompt)
                 (lambda (&rest _args) '(mock-prompt)))
                ((symbol-function 'llm-chat-prompt-interactions)
                 (lambda (_prompt) (error "prompt unreadable")))
                ((symbol-function 'llm-chat-prompt-interaction-role)
                 (lambda (_interaction) 'user))
                ((symbol-function 'llm-chat-prompt-interaction-content)
                 (lambda (_interaction) "unused"))
                ((symbol-function 'llm-chat-prompt-tools)
                 (lambda (_prompt) nil))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (&rest _args) (cl-incf provider-calls))))
        (should-error
         (superchat-runtime-test-submit
          (append
           (superchat-runtime-test--request)
           '(:evidence-contract "epistemic-learning-v0"
             :context-envelope
             (:contract "context-continuity-v0" :status satisfied
              :receipt (:tape-head 1)))))
         :type 'superchat-llm-evidence-recording-error)
        (maphash (lambda (_run-id entry) (setq failed-entry entry))
                 superchat-runtime--runs)
        (should (= 0 provider-calls))
        (should (eq 'audit-incomplete (plist-get failed-entry :status)))))))

(ert-deftest runtime/evidence-contract-rejects-stale-prepared-tools ()
  "Learning evidence cannot reuse tools that captured an older Runtime context."
  (should-error
   (superchat-runtime--validate-request
    '(:prompt "hello"
      :evidence-contract "epistemic-learning-v0"
      :context-envelope (:contract "context-continuity-v0"
                         :status satisfied :receipt (:tape-head 1))
      :prepared-tools nil))))

(ert-deftest runtime/evidence-contract-rejects-custom-tool-wrapper ()
  "An eligible Request cannot replace the Authority wrapper."
  (superchat-runtime-test--isolated
    (let ((provider-calls 0)
          (tool-effects 0))
      (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (cl-incf provider-calls))))
        (should-error
         (superchat-runtime-test-submit
          (append
           (superchat-runtime-test--request)
           (list :evidence-contract "epistemic-learning-v0"
                 :context-envelope
                 '(:contract "context-continuity-v0" :status satisfied
                   :receipt (:tape-head 1))
                 :tools (list (lambda () (cl-incf tool-effects)))
                 :tool-wrapper #'identity))))
        (should (= 0 provider-calls))
        (should (= 0 tool-effects))))))

(ert-deftest runtime/evidence-contract-is-text-only ()
  "The v0 receipt cannot misdescribe native multipart provider input."
  (should-error
   (superchat-runtime--validate-request
    '(:prompt "hello"
      :evidence-contract "epistemic-learning-v0"
      :context-envelope (:contract "context-continuity-v0"
                         :status satisfied :receipt (:tape-head 1))
      :context-files ("diagram.png")))))

(ert-deftest runtime/delivery-observes-durable-terminal-seal ()
  "Delivery adapters run only after the Run and Turn are durably closed."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events delivered-events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events
                         (append events (list (plist-get event :type))))
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit
         (append
          (superchat-runtime-test--request)
          (list :delivery-function
                (lambda (&rest _args)
                  (setq delivered-events (copy-sequence events))))))
        (should
         (equal '("intent.received" "run.started" "response.completed"
                  "run.completed" "turn.completed")
                delivered-events))))))

(ert-deftest runtime/delivery-failure-does-not-reopen-durable-run ()
  "A failed consumer handoff cannot rewrite an already sealed Run."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (&rest _args) (error "consumer failed"))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'completed (plist-get status :status)))
          ;; S01: the failed :delivery-function now also records one
          ;; `delivery.failed' Tape row after the Run/Turn are already sealed.
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.completed" "turn.completed" "delivery.failed")
                  (nreverse events))))))))

(ert-deftest runtime/closure-observations-are-bounded-copies ()
  "Runtime retains a bounded, caller-safe diagnostic history."
  (superchat-runtime-test--isolated
    (let ((superchat-runtime--closure-observation-limit 2))
      (superchat-runtime--record-closure-observation
       '(:source shadow :agreement agreed :run-id "run-1"))
      (superchat-runtime--record-closure-observation
       '(:source shadow :agreement disagreed :run-id "run-2"))
      (superchat-runtime--record-closure-observation
       '(:source guard :decision allowed :run-id "run-3"))
      (let ((observations (superchat-runtime-closure-observations)))
        (should
         (equal '("run-3" "run-2")
                (mapcar (lambda (item) (plist-get item :run-id))
                        observations)))
        (should (floatp (plist-get (car observations) :observed-at)))
        (setf (plist-get (car observations) :run-id) "mutated")
        (should
         (equal "run-3"
                (plist-get
                 (car (superchat-runtime-closure-observations))
                 :run-id))))
      (should
       (equal '("run-2")
              (mapcar
               (lambda (item) (plist-get item :run-id))
               (superchat-runtime-closure-observations t))))
      (superchat-runtime-clear-closure-observations)
      (should-not (superchat-runtime-closure-observations)))))

(ert-deftest runtime/closure-shadow-observes-without-owning-terminal-state ()
  "Shadow Closure agrees from Tape without changing Runtime delivery or status."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-shadow-" t))
           (superchat-data-directory directory)
           (superchat-closure-shadow-enabled t)
           report
           delivered
           (superchat-closure-shadow-functions
            (list (lambda (value) (setq report value)))))
      (unwind-protect
          (progn
            (superchat-db-close)
            (setq superchat-post-turn-functions
                  (list
                   (lambda (turn)
                     (superchat-tape-append-event
                      (list :type "attachment.added"
                            :session-id (superchat-turn-session-id turn)
                            :turn-id (superchat-turn-id turn)
                            :actor-id "user"
                            :payload '(:mime_type "image/png"))))))
            (superchat-tape-append-event
             '(:type "anchor.created"
               :session-id "session-runtime"
               :actor-id "user"
               :payload (:parent_session_id "parent")))
            (superchat-tape-append-event
             '(:type "extension.activated"
               :session-id "session-runtime"
               :actor-id "runtime"
               :payload (:extensions ("example"))))
            (superchat-tape-append-event
             '(:type "capability.activated"
               :session-id "session-runtime"
               :actor-id "user"
               :payload (:name "chat")))
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((request
                      (append
                       (superchat-runtime-test--request)
                       (list :delivery-function
                             (lambda (response status tape-id)
                               (setq delivered
                                     (list response status tape-id))))))
                     (handle (superchat-runtime-test-submit request))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 6) delivered))
                (should (eq 'agreed (plist-get report :agreement)))
                (should
                 (eq 'proved
                     (plist-get (plist-get report :run-judgment)
                                :verdict)))
                (should
                 (eq 'proved
                     (plist-get (plist-get report :turn-judgment)
                                :verdict)))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'shadow (plist-get observation :source)))
                  (should (eq 'agreed (plist-get observation :agreement)))
                  (should
                   (equal "session-runtime"
                          (plist-get observation :session-id)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-shadow-reports-disagreement-without-rejecting-run ()
  "A missing durable seal is reported but cannot change Runtime completion."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-shadow-gap-" t))
           (superchat-data-directory directory)
           (superchat-closure-shadow-enabled t)
           (replay (symbol-function 'superchat-tape-replay-events))
           (existing-host (get-buffer superchat-buffer-name))
           host
           report
           delivered
           (superchat-closure-shadow-functions
            (list (lambda (value) (setq report value)))))
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat-tape-replay-events)
                       (lambda (session-id &optional since-id limit)
                         (butlast
                          (funcall replay session-id since-id limit)
                          2)))
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((request (superchat-runtime-test--request))
                     (_ (setq host (plist-get request :buffer)))
                     (handle
                      (superchat-runtime-test-submit
                       (append
                        request
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 3) delivered))
                (should (eq 'disagreed (plist-get report :agreement)))
                (should
                 (eq 'open
                     (plist-get (plist-get report :run-judgment)
                                :verdict))))))
        (when (and (not existing-host) (buffer-live-p host))
          (kill-buffer host))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-shadow-failure-does-not-fail-run ()
  "An unavailable shadow Projection remains a diagnostic, not a terminal cause."
  (superchat-runtime-test--isolated
    (let ((superchat-closure-shadow-enabled t)
          (next-id 0)
          report
          delivered)
      (setq superchat-closure-shadow-functions
            (list (lambda (value) (setq report value))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat-tape-replay-events)
                 (lambda (&rest _args) (error "shadow unavailable")))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (response status tape-id)
                          (setq delivered (list response status tape-id)))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'completed (plist-get status :status)))
          (should (equal '("done" completed 3) delivered))
          (should (eq 'unknown (plist-get report :agreement)))
          (should (equal "shadow unavailable" (plist-get report :error))))))))

(ert-deftest runtime/closure-terminal-guard-allows-proved-completion ()
  "A proved readiness judgment permits the matching terminal seals."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-test-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id)))
                     (types
                      (mapcar
                       (lambda (event) (plist-get event :type))
                       (superchat-tape-replay-events "session-runtime"))))
                (should (eq 'completed (plist-get status :status)))
                (should (equal '("done" completed 3) delivered))
                (should
                 (equal '("intent.received" "run.started"
                          "response.completed" "run.completed"
                          "turn.completed")
                        types))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'guard (plist-get observation :source)))
                  (should (eq 'allowed (plist-get observation :decision)))
                  (should
                   (eq 'proved
                       (plist-get (plist-get observation :judgment)
                                  :verdict)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-allows-clean-cancellation ()
  "Cancellation is guarded against its response-free Tape prefix."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-cancel-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'llm-cancel-request) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) 'provider-request)))
              (let* ((handle
                      (superchat-runtime-test-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (run-id (plist-get handle :run-id)))
                (should (superchat-runtime-cancel run-id))
                (should
                 (eq 'cancelled
                     (plist-get (superchat-runtime-status run-id) :status)))
                (should
                 (equal '("[Cancelled by user]" cancelled 3) delivered))
                (should
                 (equal
                  '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (mapcar
                   (lambda (event) (plist-get event :type))
                   (superchat-tape-replay-events "session-runtime")))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-rejects-open-readiness ()
  "An open judgment prevents a terminal seal and becomes audit-incomplete."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-guard-open-" t))
           (superchat-data-directory directory)
           (superchat-closure-terminal-guard-enabled t)
           (replay (symbol-function 'superchat-tape-replay-events))
           delivered)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function 'superchat-tape-replay-events)
                  (lambda (session-id &optional since-id limit)
                    (butlast
                     (funcall replay session-id since-id limit))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-test-submit
                       (append
                        (superchat-runtime-test--request)
                        (list :delivery-function
                              (lambda (response status tape-id)
                                (setq delivered
                                      (list response status tape-id)))))))
                     (status
                      (superchat-runtime-status
                       (plist-get handle :run-id))))
                (should
                 (eq 'audit-incomplete (plist-get status :status)))
                (should (eq 'audit-incomplete (nth 1 delivered)))
                (should (= 3 (nth 2 delivered)))
                (should
                 (equal "done" (car delivered)))
                (should
                 (equal
                  '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (mapcar
                   (lambda (event) (plist-get event :type))
                   (funcall replay "session-runtime"))))
                (let ((observation
                       (car (superchat-runtime-closure-observations))))
                  (should (eq 'guard (plist-get observation :source)))
                  (should (eq 'rejected (plist-get observation :decision)))
                  (should
                   (eq 'open
                       (plist-get (plist-get observation :judgment)
                                  :verdict)))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/closure-terminal-guard-failure-is-audit-incomplete ()
  "A Guard interpreter failure cannot leak resources or claim completion."
  (superchat-runtime-test--isolated
    (let ((superchat-closure-terminal-guard-enabled t)
          (next-id 0)
          events delivered)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat-tape-replay-events)
                 (lambda (&rest _args) (error "guard unavailable")))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (response status tape-id)
                          (setq delivered
                                (list response status tape-id)))))))
               (run-id (plist-get handle :run-id))
               (entry (superchat-runtime--entry run-id)))
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should (eq 'audit-incomplete (nth 1 delivered)))
          (should
           (equal "done" (car delivered)))
          (should
           (equal '("intent.received" "run.started" "response.completed")
                  (nreverse events)))
          (let ((observation
                 (car (superchat-runtime-closure-observations))))
            (should (eq 'guard (plist-get observation :source)))
            (should (eq 'unknown (plist-get observation :decision)))
            (should
             (equal "guard unavailable" (plist-get observation :error))))
          (should-not (plist-get entry :delivery-function))
          (should-not (plist-get entry :transport-request)))))))

(ert-deftest runtime/real-compaction-anchor-allows-clean-completion ()
  "A real typed Anchor does not block an ordinary guarded Run."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((session-id "session-real-anchor")
             (buffer-name (generate-new-buffer-name " *superchat-real-anchor*"))
             (source-id
              (superchat-tape-append-material session-id "user" "prior fact"))
             (anchor-id
              (superchat-tape-append-compaction-anchor
               session-id "prior summary" (vector source-id))))
        (unwind-protect
            (progn
              (with-current-buffer (get-buffer-create buffer-name)
                (setq-local superchat--session-id session-id)
                (setq-local superchat--conversation-history nil))
              (let ((superchat-buffer-name buffer-name))
                (with-current-buffer buffer-name
                  (superchat--insert-prompt)
                  (superchat--prepare-for-response)))
              (let ((superchat-closure-terminal-guard-enabled t))
                (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                          ((symbol-function 'superchat--llm-generate-answer)
                           (lambda (&rest _args)
                             (funcall superchat--llm-runtime-observer
                                      'terminal
                                      '(:status completed :response "OK")))))
                  (let* ((handle
                          (superchat-runtime-test-submit
                           (list :input "next"
                                 :clean-input "next"
                                 :prompt "next"
                                 :session-id session-id
                                 :buffer (get-buffer buffer-name)
                                 :buffer-name buffer-name)))
                         (run-id (plist-get handle :run-id))
                         (status (superchat-runtime-status run-id))
                         (trace (superchat-tape-replay-events session-id))
                         (judgment
                          (superchat-closure-judge
                           (list :kind 'durable-run-terminal :run-id run-id)
                           trace))
                         (assistant-rows
                          (cl-remove-if-not
                           (lambda (row) (equal "assistant" (nth 2 row)))
                           (superchat-db-tape-replay session-id)))
                         (guard
                          (cl-find-if
                           (lambda (observation)
                             (eq 'guard (plist-get observation :source)))
                           (superchat-runtime-closure-observations))))
                    (should (integerp anchor-id))
                    (should (eq 'completed (plist-get status :status)))
                    (should (eq 'proved (plist-get judgment :verdict)))
                    (should (eq 'allowed (plist-get guard :decision)))
                    (should (eq 'proved
                                 (plist-get (plist-get guard :judgment)
                                            :verdict)))
                    (should (= 1 (length assistant-rows)))
                    (should (equal "OK" (nth 3 (car assistant-rows))))))))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name)))))))

(ert-deftest runtime/audit-incomplete-reuses-response-row-with-readable-warning ()
  "A real open Effect keeps response identity and persists a separate warning."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((session-id "session-audit-delivery")
             (buffer-name (superchat--session-buffer-name session-id))
             (superchat-buffer-name buffer-name)
             (superchat-closure-terminal-guard-enabled t))
        (unwind-protect
            (progn
              (with-current-buffer (get-buffer-create buffer-name)
                (superchat-mode 1)
                (setq-local superchat--session-id session-id)
                (setq-local superchat--conversation-history nil))
              (with-current-buffer buffer-name
                (superchat--insert-prompt)
                (superchat--prepare-for-response))
              (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                        ((symbol-function 'superchat--llm-generate-answer)
                         (lambda (&rest _) 'pending-request)))
                (let* ((handle
                        (superchat-runtime-test-submit
                         (list :input "audit"
                               :clean-input "audit"
                               :prompt "audit"
                               :session-id session-id
                               :buffer (get-buffer buffer-name)
                               :buffer-name buffer-name)))
                       (run-id (plist-get handle :run-id))
                       (entry (superchat-runtime--entry run-id))
                       (tool-record
                        (superchat-runtime--append-event
                         entry "tool.requested"
                         '(:tool-name "write_file"
                           :execution-id "exec-audit-open"
                           :request-hash "hash-audit-open"
                           :decision ask
                           :policy-version "authority-v1"
                           :effects ((:effect local-write :resource file)))
                         "tool.requested: write_file"
                         (plist-get entry :causation-id)))
                       (tool-event-id (plist-get tool-record :event-id)))
                  (setq entry
                        (superchat-runtime--update
                         run-id :causation-id tool-event-id))
                  (let* ((approval-record
                          (superchat-runtime--append-event
                           entry "approval.requested"
                           '(:tool-name "write_file"
                             :execution-id "exec-audit-open"
                             :request-hash "hash-audit-open"
                             :policy-version "authority-v1")
                           "approval.requested: write_file" tool-event-id))
                         (_entry
                          (superchat-runtime--update
                           run-id :causation-id
                           (plist-get approval-record :event-id)))
                         (_finish
                          (superchat-runtime--finish run-id 'completed "OK"))
                         (status (superchat-runtime-status run-id))
                         (trace (superchat-tape-replay-events session-id))
                         (guard
                          (cl-find-if
                           (lambda (observation)
                             (eq 'guard (plist-get observation :source)))
                           (superchat-runtime-closure-observations)))
                         (rows (superchat-db-tape-replay session-id))
                         (assistant-row
                          (cl-find-if
                           (lambda (row) (equal "assistant" (nth 2 row)))
                           rows))
                         (warning-row
                          (cl-find-if
                           (lambda (row)
                             (and (equal "system" (nth 2 row))
                                  (string-match-p "Audit incomplete" (nth 3 row))))
                           rows))
                         (history
                          (with-current-buffer buffer-name
                            (copy-tree superchat--conversation-history t)))
                         (response-id (nth 0 assistant-row)))
                    (should (eq 'audit-incomplete (plist-get status :status)))
                    (should (eq 'rejected (plist-get guard :decision)))
                    (should
                     (eq 'open
                         (plist-get (plist-get guard :judgment) :verdict)))
                    (should (member "response.completed"
                                    (mapcar (lambda (event) (plist-get event :type))
                                            trace)))
                    (should (equal "OK" (nth 3 assistant-row)))
                    (should (integerp response-id))
                    (should warning-row)
                    (should (not (= response-id (nth 0 warning-row))))
                    (should
                     (cl-every
                      (lambda (message)
                        (let* ((id (plist-get message :id))
                               (row (and id (cl-find id rows :key #'car))))
                          (or (null id)
                              (and row
                                   (equal (plist-get message :content)
                                          (nth 3 row))))))
                      history))
                    (should
                     (string-match-p "Audit incomplete"
                                     (with-current-buffer buffer-name
                                       (buffer-string))))
                    (let ((reopened (superchat--render-session-view session-id)))
                      (with-current-buffer reopened
                        (let* ((reopened-rows
                                (superchat-view-session-history session-id))
                               (reopened-response
                                (cl-find response-id reopened-rows :key #'car))
                               (reopened-warning
                                (cl-find (nth 0 warning-row)
                                         reopened-rows :key #'car)))
                          (should (equal "OK" (nth 3 reopened-response)))
                          (should (equal (nth 3 warning-row)
                                         (nth 3 reopened-warning)))
                          (should (string-match-p "Audit incomplete"
                                                  (buffer-string))))))))))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name)))))))

(ert-deftest runtime/events-use-logical-causation-and-storage-delivery ()
  "Runtime causation uses event identity while UI delivery keeps row identity."
  (superchat-runtime-test--isolated
    (let ((next-storage-id 0)
          events delivered)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (setq events (append events (list event)))
                   (cl-incf next-storage-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (_answer storage-id &optional _owner)
                   (setq delivered storage-id)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit (superchat-runtime-test--view-request))
        (should (= 3 delivered))
        (should
         (equal '("intent.received" "run.started"
                  "response.completed" "run.completed" "turn.completed")
                (mapcar (lambda (event) (plist-get event :type)) events)))
        (let ((started-payload (plist-get (nth 1 events) :payload)))
          (should (equal "lead" (plist-get started-payload :actor-id)))
          (should (equal "chat" (plist-get started-payload :capability)))
          (should-not (plist-member started-payload :profile-id)))
        (let ((previous nil))
          (dolist (event events)
            (let ((event-id (plist-get event :event-id)))
              (should (string-prefix-p "evt_" event-id))
              (should (equal previous (plist-get event :causation-id)))
              (setq previous event-id))))))))

(ert-deftest runtime/terminal-records-undelivered-steers ()
  "A terminal Run proves which queued steers never reached the model."
  (superchat-runtime-test--isolated
    (let (events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push event events)
                   (length events)))
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) 'provider-request)))
        (let* ((handle (superchat-runtime-test-submit
                        (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (superchat-runtime-steer run-id "take another path")
          (should (= 1 (plist-get (superchat-runtime-status run-id)
                                  :pending-steer-count)))
          (superchat-runtime-cancel run-id)
          (setq events (nreverse events))
          (let* ((status (superchat-runtime-status run-id))
                 (types (mapcar (lambda (event) (plist-get event :type))
                                events))
                 (undelivered
                  (cl-find "run.steer-undelivered" events
                           :key (lambda (event) (plist-get event :type))
                           :test #'equal)))
            (should (= 0 (plist-get status :pending-steer-count)))
            (should (= 1 (plist-get status :undelivered-steer-count)))
            (should-not (plist-member status :steer-queue))
            (should (equal '("intent.received" "run.started" "run.steered"
                             "run.steer-undelivered" "run.cancelled"
                             "turn.completed")
                           types))
            (should (= 1 (plist-get (plist-get undelivered :payload)
                                    :count)))))))))

(ert-deftest runtime/steer-during-post-turn-hook-is-rejected-with-undelivered-fact ()
  "A steer from a post-turn hook is rejected once terminalization has begun,
reached through a real scripted final-text completion (not an explicit
cancel).  Only the provider transport is replaced -- Runtime, the real LLM
coordinator, and Tape persistence are exercised for real."
  (let (superchat-post-turn-functions)
    (journey-fixture-with (:script (list (list :text "final answer")))
      (let* ((view (journey-fixture-open-chat))
             (session-id (journey-fixture-session-id view))
             (run-id (journey-fixture-send "go" view))
             before-result during-result)
        ;; Control: the same call succeeds while the Run is still running.
        (setq before-result (superchat-runtime-steer run-id "before finish"))
        (should before-result)
        (should (equal '("before finish") (superchat-runtime-drain-steers run-id)))
        (setq superchat-post-turn-functions
              (list (lambda (_turn)
                      (setq during-result
                            (superchat-runtime-steer run-id "during hook")))))
        (should (journey-fixture-wait-for-run view))
        (should-not during-result)
        (let* ((events (journey-fixture-events session-id))
               (types (mapcar (lambda (event) (plist-get event :type)) events))
               (undelivered
                (cl-find "run.steer-undelivered" events
                         :key (lambda (event) (plist-get event :type))
                         :test #'equal)))
          ;; Exactly the control call's "run.steered" -- the hook's rejected
          ;; call must not add a second one.
          (should (= 1 (cl-count "run.steered" types :test #'equal)))
          (should (= 1 (cl-count "run.steer-undelivered" types :test #'equal)))
          (should (= 1 (plist-get (plist-get undelivered :payload) :count))))))))

(ert-deftest runtime/steer-tape-failure-rolls-back-queue ()
  "A Tape append failure during steer leaves the queue exactly as it was.
Real Runtime and provider transport; the only replacement is the expressly
permitted injected `superchat-db-tape-append' failure (seam rule 4b)."
  (journey-fixture-with ()
    (let* ((view (journey-fixture-open-chat))
           (run-id (journey-fixture-send "go" view)))
      (should (= 0 (superchat-runtime-pending-steer-count run-id)))
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (&rest _args) (error "tape store unavailable"))))
        (should-not (superchat-runtime-steer run-id "lost instruction")))
      (should (= 0 (superchat-runtime-pending-steer-count run-id)))
      (should-not (superchat-runtime-drain-steers run-id))
      ;; The queue is clean, so a following steer is accepted normally.
      (should (superchat-runtime-steer run-id "kept instruction"))
      (should (= 1 (superchat-runtime-pending-steer-count run-id))))))

(ert-deftest runtime/steer-at-plain-completion-boundary-forces-another-attempt ()
  "A steer accepted through the real Runtime from round 1's scripted
`:after-chunks' point forces one more real attempt: the steer text lands in
the real next prompt, and exactly one real `run.steered' fact replays from
Tape with its full payload (`:instruction', content size, content hash) --
not a fabricated queue.  Real Runtime, real Tape, and the real
`llm-provider-utils' result processor; only the provider transport
(`llm-chat-streaming'/`llm-chat-async') is replaced, via
`journey-fixture-with'."
  (let* ((instruction "keep going") run-id steer-result)
    (journey-fixture-with
        (:script
         (list (list :chunks '("first chunk")
                     :text "first answer"
                     :after-chunks
                     (lambda (_state)
                       (setq steer-result
                             (superchat-runtime-steer run-id instruction))))
               (list :chunks '("second chunk") :text "second answer")))
      (let* ((view (journey-fixture-open-chat))
             (session-id (journey-fixture-session-id view)))
        (setq run-id (journey-fixture-send "continue" view))
        (should (journey-fixture-wait-for-run view))
        (should steer-result)
        (let ((requests (journey-fixture-requests)))
          (should (= 2 (length requests)))
          (should (string-match-p (regexp-quote instruction)
                                  (plist-get (nth 1 requests) :prompt-text))))
        (let* ((events (journey-fixture-events session-id))
               (types (mapcar (lambda (event) (plist-get event :type)) events))
               (steered
                (cl-find "run.steered" events
                         :key (lambda (event) (plist-get event :type))
                         :test #'equal))
               (payload (plist-get steered :payload))
               (expected-metadata
                (superchat-runtime--content-metadata instruction)))
          (should (= 1 (cl-count "run.steered" types :test #'equal)))
          (should-not (member "run.steer-undelivered" types))
          (should (equal instruction (plist-get payload :instruction)))
          (should (equal (plist-get expected-metadata :content-size)
                        (plist-get payload :content-size)))
          (should (equal (plist-get expected-metadata :content-hash)
                        (plist-get payload :content-hash))))))))

(ert-deftest runtime/repeated-steers-are-bounded-by-max-tool-rounds ()
  "Steers accepted through the real Runtime at successive scripted
`:after-chunks' plain-text boundaries stop at the round bound; the round
that hits the bound leaves its own steer queued, and Runtime records it as a
real, replayed `run.steer-undelivered' Tape fact at Run finish -- with the
exact accepted instructions, in order, and the exact undelivered instruction
hash."
  (let ((superchat-llm-max-tool-rounds 2)
        run-id steer-results)
    (journey-fixture-with
        (:script
         (list (list :chunks '("r1") :text "text-1"
                     :after-chunks
                     (lambda (_state)
                       (push (cons "steer-1"
                                   (superchat-runtime-steer run-id "steer-1"))
                             steer-results)))
               (list :chunks '("r2") :text "text-2"
                     :after-chunks
                     (lambda (_state)
                       (push (cons "steer-2"
                                   (superchat-runtime-steer run-id "steer-2"))
                             steer-results)))
               (list :chunks '("r3") :text "text-3"
                     :after-chunks
                     (lambda (_state)
                       (push (cons "steer-3"
                                   (superchat-runtime-steer run-id "steer-3"))
                             steer-results)))))
      (let* ((view (journey-fixture-open-chat))
             (session-id (journey-fixture-session-id view)))
        (setq run-id (journey-fixture-send "continue" view))
        (should (journey-fixture-wait-for-run view))
        (setq steer-results (nreverse steer-results))
        (should (equal '("steer-1" "steer-2" "steer-3")
                       (mapcar #'car steer-results)))
        (should (cl-every #'cdr steer-results))
        (should (= 3 (length (journey-fixture-requests))))
        (should (eq 'completed
                    (plist-get (superchat-runtime-status run-id) :status)))
        (let* ((events (journey-fixture-events session-id))
               (types (mapcar (lambda (event) (plist-get event :type)) events))
               (steered-events
                (seq-filter (lambda (event)
                              (equal "run.steered" (plist-get event :type)))
                            events))
               (undelivered
                (cl-find "run.steer-undelivered" events
                         :key (lambda (event) (plist-get event :type))
                         :test #'equal))
               (undelivered-payload (plist-get undelivered :payload)))
          (should (= 3 (cl-count "run.steered" types :test #'equal)))
          (should (equal '("steer-1" "steer-2" "steer-3")
                        (mapcar (lambda (event)
                                  (plist-get (plist-get event :payload)
                                             :instruction))
                                steered-events)))
          (should (= 1 (cl-count "run.steer-undelivered" types :test #'equal)))
          (should (= 1 (plist-get undelivered-payload :count)))
          (should (equal (list (plist-get
                                (superchat-runtime--content-metadata "steer-3")
                                :content-hash))
                        (plist-get undelivered-payload :instruction-hashes))))))))

(ert-deftest runtime/effect-capable-is-nil-while-terminalizing ()
  "Effect capability closes as soon as terminalization begins, reached
through a real scripted final-text completion, observed from a post-turn
hook.  Only the provider transport is replaced.

S04-fix-2: the same hook window also directly proves `:terminalizing'
itself is set at hook time -- not merely that `:status' already reads
`delivering', which by itself would also make effect capability false and
so would not by itself catch a deleted `:terminalizing t' update at the top
of `superchat-runtime--finish-body' (the S04 re-review's mutation-check
rejection).  It also proves the guard's real consequence: a child Run
cannot attach to this Run while it is `:terminalizing', refused with no
Tape row for the rejected child -- the same request shape as
`runtime/child-attach-refused-when-parent-terminal-or-terminalizing'."
  (let (superchat-post-turn-functions)
    (journey-fixture-with (:script (list (list :text "final answer")))
      (let* ((view (journey-fixture-open-chat))
             (session-id (journey-fixture-session-id view))
             (run-id (journey-fixture-send "go" view))
             during-result terminalizing-result
             events-before-attach events-after-attach child-error)
        (should (superchat-runtime-effect-capable-p run-id))
        (setq superchat-post-turn-functions
              (list (lambda (_turn)
                      (setq during-result
                            (superchat-runtime-effect-capable-p run-id))
                      (setq terminalizing-result
                            (plist-get (superchat-runtime--entry run-id)
                                       :terminalizing))
                      (setq events-before-attach
                            (length (journey-fixture-events session-id)))
                      (condition-case err
                          (superchat-runtime-test-submit
                           (append (superchat-runtime-test--request)
                                   (list :parent-run-id run-id)))
                        (error (setq child-error err)))
                      (setq events-after-attach
                            (length (journey-fixture-events session-id))))))
        (should (journey-fixture-wait-for-run view))
        (should-not during-result)
        (should terminalizing-result)
        (should child-error)
        (should (eq 'error (car child-error)))
        (should (string-match-p "Parent Run unavailable"
                                (error-message-string child-error)))
        (should (integerp events-before-attach))
        (should (= events-before-attach events-after-attach))))))

(ert-deftest runtime/every-llm-observer-event-has-a-branch ()
  "Every event the Runtime observer handles has a named pcase branch, and
every literal event `superchat-llm.el' emits through that observer has a
matching branch.  Two best-effort SYNTACTIC checks, one per source file: the
first reads `superchat-runtime-submit''s pcase clause keys, the second reads
every `(funcall runtime-observer (quote EVENT) ...)' call site in
`superchat-llm.el'.  A stray `drain-steers' branch, a silently unhandled
emitted event (this is what let the removed `drain-steers' observer event
stay green before this slice), or a dead branch nothing emits all fail this
test without running anything."
  (let* ((runtime-root (file-name-directory (locate-library "superchat-runtime")))
         (runtime-file (expand-file-name "superchat-runtime.el" runtime-root))
         (llm-root (file-name-directory (locate-library "superchat-llm")))
         (llm-file (expand-file-name "superchat-llm.el" llm-root))
         (expected
          '(terminal first-output usage model-phase before-attempt
            context-budget-exceeded attempt-started decision-snapshot
            audit-failure before-input-dispatch input-clock-exhausted
            request context-recovery-started context-recovery-ended))
         (sorted-expected (sort (copy-sequence expected) #'string-lessp))
         branch-events emitted-events)
    (with-temp-buffer
      (insert-file-contents runtime-file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (should (search-forward "(defun superchat-runtime-submit " nil t))
      (should (search-forward "(pcase event" nil t))
      (goto-char (match-beginning 0))
      (let* ((end (progn (forward-sexp) (point)))
             (form (read (buffer-substring-no-properties
                          (match-beginning 0) end))))
        (setq branch-events
              (mapcar (lambda (clause)
                        (let ((pattern (car clause)))
                          (if (and (consp pattern) (eq (car pattern) 'quote))
                              (cadr pattern)
                            pattern)))
                      (cddr form)))))
    (with-temp-buffer
      (insert-file-contents llm-file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (while (re-search-forward
              "(funcall[ \t\n]+\\(?:runtime-observer\\|superchat--llm-runtime-observer\\)\\_>"
              nil t)
        (goto-char (match-beginning 0))
        (let* ((end (progn (forward-sexp) (point)))
               (form (read (buffer-substring-no-properties
                            (match-beginning 0) end)))
               (event-arg (nth 2 form)))
          (push (if (and (consp event-arg) (eq (car event-arg) 'quote))
                    (cadr event-arg)
                  event-arg)
                emitted-events)
          (goto-char end))))
    (should-not (memq 'drain-steers branch-events))
    (should-not (memq 'drain-steers emitted-events))
    (should (equal sorted-expected (sort (delete-dups branch-events) #'string-lessp)))
    (should (equal sorted-expected (sort (delete-dups emitted-events) #'string-lessp)))))

(ert-deftest runtime/status-exposes-selected-tools-and-decision ()
  "S09-fix-1 item 2: the public Run status exposes which tool names the
most recent attempt actually carried and why (contract §4.2), through a
real scripted completion -- only the provider transport is replaced."
  (journey-fixture-with (:tools-policy 'always
                          :script (list (list :text "final answer")))
    (let* ((view (journey-fixture-open-chat))
           (run-id (journey-fixture-send "go" view)))
      (should (journey-fixture-wait-for-run view))
      (let ((status (superchat-runtime-status run-id)))
        (should (eq 'baseline (plist-get status :runtime-tool-decision)))
        (should (equal (plist-get (journey-fixture-last-request) :tools)
                       (plist-get status :runtime-selected-tools)))))))

(ert-deftest runtime/propagates-orthogonal-configuration ()
  "Runtime exposes config in status, audit events, and tool context."
  (superchat-runtime-test--isolated
    (let ((root (make-temp-file "superchat-runtime-root" t))
          (buffer (get-buffer-create " *runtime-config*"))
          (validator #'ignore)
          events intent-event tool-context)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (let ((id 0))
                       (lambda (event)
                         (push event events)
                         (when (equal "intent.received"
                                      (plist-get event :type))
                           (setq intent-event event))
                         (cl-incf id))))
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--process-llm-result) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (setq tool-context superchat--tool-execution-context)
                       (funcall superchat--llm-runtime-observer
                                'terminal
                                '(:status completed :response "done")))))
            (let* ((handle
                    (superchat-runtime-test-submit
                     `(:input "inspect"
                       :clean-input "inspect"
                       :prompt "inspect"
                       :buffer ,buffer
                       :capability "code"
                       :topology team
                       :authority-policy developer-project
                       :target-model "runtime-test-model"
                       :workspace-id "workspace-test"
                       :workspace-root ,root
                       :tool-validator ,validator
                       :origin (:major-mode text-mode
                                :uri "buffer://notes/region/10-20"
                                :region "do-not-persist"))))
                   (status
                    (superchat-runtime-status (plist-get handle :run-id)))
                   (payload (plist-get intent-event :payload)))
              (should (equal "code" (plist-get status :capability)))
              (should (equal "runtime-test-model" (plist-get status :model)))
              (should (eq 'team (plist-get status :topology)))
              (should (equal "developer-project"
                             (plist-get status :authority-policy)))
              (should (eq 'text-mode (plist-get status :surface)))
              (should
               (eq 'text-mode
                   (buffer-local-value
                    'superchat--last-surface
                    buffer)))
              (should (equal "workspace-test"
                             (plist-get status :workspace-id)))
              (should (eq 'team (plist-get tool-context :topology)))
              (should (equal "developer-project"
                             (plist-get tool-context :authority-policy)))
              (should (eq validator (plist-get tool-context :validate)))
              (should (equal "text-mode" (plist-get payload :surface)))
              (should (equal "runtime-test-model"
                             (plist-get
                              (plist-get (cl-find "run.started" events
                                                  :key (lambda (event)
                                                         (plist-get event :type))
                                                  :test #'equal)
                                         :payload)
                              :model)))
              (should
               (equal "buffer://notes/region/10-20"
                      (plist-get payload :origin-uri)))
              (should-not (string-match-p
                           "do-not-persist"
                           (prin1-to-string intent-event)))))
        (delete-directory root t)))))

(ert-deftest runtime/agent-preset-identifies-profile-and-instance ()
  "An Agent Run exposes stable Profile and direct parent identities."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "Coder Fast" :profile-id "profile.coder"
              :type agent :capability "coding"
              :body "Implement changes.")))
          (next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push event events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* (;; S11: :parent-run-id must name a real, live Run -- a
               ;; fabricated id is now refused at submission.  This parent
               ;; is left non-terminal (its own transport stub never fires
               ;; the terminal observer) so the child below can attach.
               (parent-run-id
                (plist-get
                 (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                            #'ignore))
                   (superchat-runtime-test-submit
                    (append (superchat-runtime-test--request)
                            (list :actor-id "parent-actor"))))
                 :run-id))
               (handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :actor-id "coder-1"
                        :parent-run-id parent-run-id
                        :capability "coding"
                        :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id)))
               (started
                (cl-find-if
                 (lambda (event)
                   (equal "run.started" (plist-get event :type)))
                 events)))
          (should (equal "coder-1" (plist-get status :actor-id)))
          (should (equal "profile.coder" (plist-get status :profile-id)))
          (should (equal parent-run-id
                         (plist-get status :parent-run-id)))
          (should (equal "coding" (plist-get status :capability)))
          (should (equal "profile.coder" (plist-get started :profile-id)))
          (should (equal parent-run-id
                         (plist-get started :parent-run-id)))
          (should
           (equal "profile.coder"
                  (plist-get (plist-get started :payload) :profile-id)))
          (should
           (equal "coder-1"
                  (plist-get (plist-get started :payload) :actor-id)))
          (should
           (equal "coding"
                  (plist-get (plist-get started :payload) :capability))))))))

(ert-deftest runtime/agent-profile-requires-profile-id ()
  "A legacy Agent Profile falls back to its name with a warning."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          profile
          warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _args)
                   (push message warnings)))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (setq profile
              (superchat-preset-from-plist
               '(:name "coder-safe" :type agent :capability "coding")))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "coder-safe" (plist-get status :profile-id)))
          (should
           (cl-some
            (lambda (message)
              (string-match-p
               "Deprecated agent preset without profile-id"
               message))
            warnings)))))))

(ert-deftest runtime/profile-name-is-not-an-implicit-capability ()
  "Runtime falls back to chat, never from Profile identity to Capability."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "coder-fast" :type agent :body "Implement changes.")))
          (next-id 0)
          warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _args)
                   (push message warnings)))
                ((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :actor-id "coder-1" :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "coder-fast" (plist-get status :profile-id)))
          (should (equal "chat" (plist-get status :capability)))
          (should
           (cl-some
            (lambda (message)
              (string-match-p "no explicit capability" message))
            warnings)))))))

(ert-deftest runtime/agent-profile-capability-conflict-rejected ()
  "A Request cannot reinterpret an Agent Profile as another Capability."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name "coder-safe" :profile-id "profile.coder"
              :type agent :capability "coding")))
          (transport-called nil)
          (event-count 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf event-count)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq transport-called t))))
        (let ((condition
               (should-error
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :capability "review" :preset profile)))
                :type 'error)))
          (should
           (string-match-p
            "Capability review conflicts with Agent Profile profile.coder"
            (error-message-string condition))))
        (should-not transport-called)
        (should (= 0 event-count))
        (should (= 0 (hash-table-count superchat-runtime--runs)))))))

(ert-deftest runtime/profile-does-not-grant-authority ()
  "Profile metadata cannot widen the Runtime-selected Authority."
  (superchat-runtime-test--isolated
    (let ((profile
           (superchat-preset-from-plist
            '(:name coder :profile-id profile.coder
              :type agent :capability coding
              :authority-policy developer-project)))
          (next-id 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :preset profile))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should
           (equal "default" (plist-get status :authority-policy))))))))

(ert-deftest runtime/non-agent-preset-does-not-create-profile ()
  "Prompt and Workflow Presets remain configuration, not Agent Profiles."
  (dolist (type '(prompt workflow))
    (let ((preset
           (superchat-preset-from-plist
            (list :name (symbol-name type) :type type))))
      (should-not (superchat-preset-profile-id preset)))))

(ert-deftest runtime/rejects-unknown-configuration ()
  "Runtime fails closed before transport for unknown policy or Topology."
  (should-error
   (superchat-runtime-test-submit
    '(:prompt "hello" :authority-policy "admin")))
  (should-error
   (superchat-runtime-test-submit
    '(:prompt "hello" :topology collective))))

(ert-deftest runtime/post-turn-waits-for-terminal-response ()
  "Post-turn hooks do not run while the provider request is active."
  (superchat-runtime-test--isolated
    (let (observer
          (post-calls 0)
          (next-id 0))
      (setq superchat-post-turn-functions
            (list (lambda (_turn) (cl-incf post-calls))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'provider-request)
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (= 0 post-calls))
          (should (eq 'running-model
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (funcall observer
                   'terminal '(:status completed :response "later"))
          (should (= 1 post-calls))
          (should (eq 'completed
                      (plist-get (superchat-runtime-status run-id)
                                 :status))))))))

(ert-deftest runtime/custom-delivery-and-stream-stay-private ()
  "Non-chat callers receive callbacks without exposing them through status."
  (superchat-runtime-test--isolated
    (let (delivered streamed)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest args)
                   (funcall (nth 2 args) "part")
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((request
                (append
                 (superchat-runtime-test--request)
                 (list :stream-function
                       (lambda (chunk) (setq streamed chunk))
                       :delivery-function
                       (lambda (response status tape-id)
                         (setq delivered
                               (list response status tape-id))))))
               (handle (superchat-runtime-test-submit request))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (equal "part" streamed))
          (should (equal '("done" completed 3) delivered))
          (should-not (plist-member status :delivery-function))
          (should-not (plist-member status :stream-function)))))))

(ert-deftest runtime/ordinary-publication-rolls-back-entry-not-caller-buffer ()
  "A failed ordinary publish removes its entry but preserves its View object."
  (superchat-runtime-test--isolated
    (let* ((name " *superchat-runtime-ordinary-partial-put*")
           (view (get-buffer-create name))
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (original-put (symbol-function 'superchat-runtime--put)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'superchat-runtime--put)
                       (lambda (entry)
                         (funcall original-put entry)
                         (error "ordinary after-put boom"))))
              (should-error
               (superchat-runtime-test-submit
                (list :prompt "hello" :buffer view :buffer-name name))
               :type 'error))
            (should (buffer-live-p view))
            (should (= 0 (hash-table-count superchat-runtime--runs))))
        (when (buffer-live-p view) (kill-buffer view))))))

(ert-deftest runtime/exact-buffer-must-be-live-before-any-effect ()
  "A modern Request rejects a dead exact buffer before publication or transport."
  (superchat-runtime-test--isolated
    (let ((buffer (generate-new-buffer " *superchat-runtime-dead-exact*"))
          (provider-calls 0)
          (superchat-runtime--runs (make-hash-table :test #'equal)))
      (kill-buffer buffer)
      (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (cl-incf provider-calls))))
        (should-error
         (superchat-runtime-test-submit (list :prompt "hello" :buffer buffer))
         :type 'error))
      (should (= 0 provider-calls))
      (should (= 0 (hash-table-count superchat-runtime--runs))))))

(ert-deftest runtime/exact-buffer-is-borrowed-and-name-is-diagnostic ()
  "A modern Run keeps exact identity and never retargets or kills by name."
  (superchat-runtime-test--isolated
    (let* ((name " *superchat-runtime-exact-borrowed*")
           (exact (generate-new-buffer name))
           (replacement nil)
           handle)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                     (let ((id 0)) (lambda (_event) (cl-incf id))))
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (kill-buffer exact)
                       (setq replacement (generate-new-buffer name))
                       (funcall superchat--llm-runtime-observer
                                'terminal
                                '(:status completed :response "done")))))
            (setq handle
                  (superchat-runtime-test-submit
                   (list :prompt "hello"
                         :session-id "exact-borrowed-session"
                         :buffer exact
                         :buffer-name "ignored-name")))
            (let ((entry (superchat-runtime--entry
                          (plist-get handle :run-id))))
              (should (eq exact (plist-get entry :buffer)))
              (should (equal name (plist-get entry :buffer-name)))
              (should-not (plist-member entry :cleanup))
              (should-not (superchat-runtime--entry-buffer entry))
              (should (buffer-live-p replacement))))
        (when (buffer-live-p exact) (kill-buffer exact))
        (when (buffer-live-p replacement) (kill-buffer replacement))))))

(ert-deftest runtime/exact-buffer-publication-rollback-preserves-caller-object ()
  "Exact-buffer publication rollback removes only the partial Run entry."
  (superchat-runtime-test--isolated
    (let ((buffer (generate-new-buffer " *superchat-runtime-exact-rollback*"))
          (superchat-runtime--runs (make-hash-table :test #'equal))
          (original-put (symbol-function 'superchat-runtime--put)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'superchat-runtime--put)
                       (lambda (entry)
                         (funcall original-put entry)
                         (error "exact after-put boom"))))
              (should-error
               (superchat-runtime-test-submit
                (list :prompt "hello" :buffer buffer))
               :type 'error))
            (should (buffer-live-p buffer))
            (should (= 0 (hash-table-count superchat-runtime--runs))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/exact-buffer-success-keeps-borrowed-host-live ()
  "Successful terminal quiescence keeps the exact borrowed buffer live."
  (superchat-runtime-test--isolated
    (let ((buffer (generate-new-buffer " *superchat-runtime-exact-success*"))
          handle)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (let ((id 0)) (lambda (_event) (cl-incf id))))
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed :response "done")))))
              (setq handle
                    (superchat-runtime-test-submit
                     (list :prompt "hello" :buffer buffer)))
              (should (eq buffer
                          (plist-get
                           (superchat-runtime--entry
                           (plist-get handle :run-id))
                           :buffer)))
              (should (buffer-live-p buffer))
              (should (plist-get
                       (superchat-runtime--entry (plist-get handle :run-id))
                       :quiesced)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/exact-buffer-cancel-keeps-borrowed-host-live ()
  "Cancellation severs the Run without releasing its borrowed buffer."
  (superchat-runtime-test--isolated
    (let ((buffer (generate-new-buffer " *superchat-runtime-exact-cancel*"))
          handle)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (let ((id 0)) (lambda (_event) (cl-incf id))))
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) 'exact-pending-request)))
              (setq handle
                    (superchat-runtime-test-submit
                     (list :prompt "hello" :buffer buffer)))
              (should (superchat-runtime-cancel (plist-get handle :run-id)))
              (should (buffer-live-p buffer))
              (should (plist-get
                       (superchat-runtime--entry (plist-get handle :run-id))
                       :quiesced)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/exact-buffer-provider-error-keeps-borrowed-host-live ()
  "A synchronous provider error leaves the borrowed exact buffer live."
  (superchat-runtime-test--isolated
    (let ((buffer (generate-new-buffer " *superchat-runtime-exact-error*"))
          )
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (let ((id 0)) (lambda (_event) (cl-incf id))))
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) (error "exact provider failed"))))
              (should-error
               (superchat-runtime-test-submit
               (list :prompt "hello" :buffer buffer))
               :type 'error)
              (should (buffer-live-p buffer)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest runtime/synchronous-provider-quit-seals-published-run ()
  "A provider quit is re-signalled only after terminal delivery and quiescence."
  (superchat-runtime-test--isolated
    (let ((deliveries 0)
          run-id
          caught)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0)) (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (signal 'quit nil))))
        (setq caught
              (condition-case _condition
                  (superchat-runtime-test-submit
                   (append
                    (superchat-runtime-test--request)
                    (list :delivery-function
                          (lambda (_response _status _tape-id)
                            (cl-incf deliveries)
                            (signal 'quit nil)))))
                (quit 'quit)))
        (should (eq 'quit caught))
        (maphash (lambda (id _entry) (setq run-id id))
                 superchat-runtime--runs)
        (should run-id)
        (should (= 1 deliveries))
        (let ((entry (superchat-runtime--entry run-id)))
          (should (eq 'failed (plist-get entry :status)))
          (should (plist-get entry :quiesced)))))))

(ert-deftest runtime/tool-interrupt-callback-cancels-only-its-captured-run ()
  "A delayed Authority interrupt remains bound to one exact Run."
  (superchat-runtime-test--isolated
    (let (contexts handles)
      (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (push (copy-sequence superchat--tool-execution-context)
                         contexts)
                   'provider-request))
                ((symbol-function 'llm-cancel-request) #'ignore))
        (dotimes (_ 2)
          (push (superchat-runtime-test-submit
                 (superchat-runtime-test--request))
                handles))
        (setq contexts (nreverse contexts)
              handles (nreverse handles))
        (let* ((run-a (plist-get (nth 0 handles) :run-id))
               (run-b (plist-get (nth 1 handles) :run-id))
               (interrupt-a (plist-get (nth 0 contexts) :interrupt-function)))
          (should (functionp interrupt-a))
          (should (funcall interrupt-a))
          (should (eq 'cancelled
                      (plist-get (superchat-runtime-status run-a) :status)))
          (should (eq 'running-model
                      (plist-get (superchat-runtime-status run-b) :status)))
          (should-not (funcall interrupt-a))
          (should (eq 'running-model
                      (plist-get (superchat-runtime-status run-b) :status))))))))

(ert-deftest runtime/user-interruption-does-not-open-recovery-intervention ()
  "Interruption-class failure is durable evidence, not a recovery trigger."
  (let ((superchat-runtime-recovery-intervention-enabled t)
        called)
    (cl-letf (((symbol-function 'superchat-experience-assemble-recovery-view)
               (lambda (&rest _) (setq called t) '(:status satisfied))))
      (should-not
       (superchat-runtime--recovery-intervention-view
        "run-a" 'tool-followup
        '(:type "tool.failed" :run-id "run-a" :session-id "session-a"
          :payload (:error-type "quit" :interruption "user-quit"
                    :phase "implementation"))))
      (should-not called))))

(ert-deftest runtime/authority-quit-cancels-exact-run-through-real-bridge ()
  "Authority audit precedes exact Runtime cancel under synchronous reentry."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let (caught observer (cancel-callbacks 0))
        (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'llm-cancel-request)
                   (lambda (_request)
                     (cl-incf cancel-callbacks)
                     (funcall observer 'terminal
                              '(:status completed :response "late"))))
                  ((symbol-function 'superchat--llm-generate-answer)
                   (lambda (&rest _args)
                     (setq observer superchat--llm-runtime-observer)
                     (funcall observer 'request 'provider-request)
                     (let ((superchat-agent-pre-tool-functions
                            (list (lambda (&rest _)
                                    (signal 'quit '(authority-stop))))))
                       (superchat-tool-execute
                        "read-file" (lambda (&rest _) "unreached")
                        '("README.md")
                        :context superchat--tool-execution-context)))))
          (condition-case condition
              (superchat-runtime-test-submit
               (append (superchat-runtime-test--request)
                       (list :approval-function (lambda (_request) t))))
            (quit (setq caught condition))))
        (should (equal '(quit authority-stop) caught))
        (should (= 1 cancel-callbacks))
        (let* ((events (superchat-tape-replay-events "session-runtime"))
               (types (mapcar (lambda (event) (plist-get event :type)) events)))
          (should (member "tool.failed" types))
          (should (member "run.cancelled" types))
          (should-not (member "tool.completed" types)))))))

(ert-deftest runtime/interruption-audit-gap-seals-incomplete-not-cancelled ()
  "A failed interruption outcome append uses Runtime audit-incomplete."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((real-append (symbol-function 'superchat-tape-append-event-record))
            caught)
        (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'llm-cancel-request)
                   (lambda (&rest _) (signal 'quit '(secondary-quit))))
                  ((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event)
                     (if (equal "tool.failed" (plist-get event :type))
                         (error "interruption audit unavailable")
                       (funcall real-append event))))
                  ((symbol-function 'superchat--llm-generate-answer)
                   (lambda (&rest _args)
                     (let ((superchat-agent-pre-tool-functions
                            (list (lambda (&rest _)
                                    (signal 'quit '(primary-audit-stop))))))
                       (superchat-tool-execute
                        "read-file" (lambda (&rest _) "unreached")
                        '("README.md")
                        :context superchat--tool-execution-context)))))
          (condition-case condition
              (superchat-runtime-test-submit
               (append (superchat-runtime-test--request)
                       (list :approval-function (lambda (_request) t))))
            (quit (setq caught condition))))
        (should (equal '(quit primary-audit-stop) caught))
        (let* ((events (superchat-tape-replay-events "session-runtime"))
               (types (mapcar (lambda (event) (plist-get event :type)) events))
               (run-id nil))
          (maphash (lambda (id _entry) (setq run-id id))
                   superchat-runtime--runs)
          (should (eq 'audit-incomplete
                      (plist-get (superchat-runtime-status run-id) :status)))
          (should-not (member "tool.failed" types))
          (should-not (member "tool.completed" types))
          (should-not (member "run.cancelled" types)))))))

(ert-deftest runtime/provider-quit-survives-turn-and-completion-hook-quits ()
  "Untrusted hook quits cannot interrupt provider-quit terminal sealing."
  (superchat-runtime-test--isolated
    (let ((deliveries 0)
          (later-turn-hooks 0)
          (later-completion-hooks 0)
          run-id
          caught)
      (setq superchat-pre-topic-finalized-functions
            (list (lambda (_turn) (signal 'quit '(turn-hook-quit)))
                  (lambda (_turn) (cl-incf later-turn-hooks))))
      (setq superchat-post-turn-functions
            (list (lambda (_turn) (cl-incf later-turn-hooks))))
      (setq superchat-post-topic-finalized-functions
            (list (lambda (_turn) (cl-incf later-turn-hooks))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status)
                    (signal 'quit '(completion-hook-quit)))
                  (lambda (_status)
                    (cl-incf later-completion-hooks))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0)) (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (signal 'quit '(provider-quit)))))
        (setq caught
              (condition-case condition
                  (superchat-runtime-test-submit
                   (append
                    (superchat-runtime-test--request)
                    (list :delivery-function
                          (lambda (_response _status _tape-id)
                            (cl-incf deliveries)))))
                (quit condition)))
        (maphash (lambda (id _entry) (setq run-id id))
                 superchat-runtime--runs)
        (should run-id)
        (should (equal '(quit provider-quit) caught))
        (should (= 1 deliveries))
        (should (= 3 later-turn-hooks))
        (should (= 1 later-completion-hooks))
        (let ((entry (superchat-runtime--entry run-id)))
          (should (eq 'failed (plist-get entry :status)))
          (should (plist-get entry :turn-completed))
          (should (plist-get entry :quiesced))
          (should (plist-get entry :quiesced)))
        (maphash
         (lambda (_id entry)
           (should-not (memq (plist-get entry :status)
                             '(running-model delivering))))
         superchat-runtime--runs)))))

(ert-deftest runtime/kernel-does-not-know-editor-workspace-ownership ()
  "Runtime has no middle-tier dependency or marker-level ownership copy."
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "superchat-runtime"))
           (buffer-string))))
    (should-not (string-match-p "superchat-editor-workspace" source))
    (should-not (string-match-p "superchat--editor-workspace-" source))))

(ert-deftest runtime/ordinary-run-converges-to-quiescence ()
  "A successful ordinary Run quiesces and preserves its borrowed host."
  (superchat-runtime-test--isolated
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (let ((id 0))
                 (lambda (_event) (cl-incf id))))
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _args)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          '(:status completed :response "done")))))
      (let* ((handle
              (superchat-runtime-test-submit
               (superchat-runtime-test--request)))
             (entry
              (superchat-runtime--entry (plist-get handle :run-id)))
             (buffer (plist-get entry :buffer)))
        (should (plist-get entry :quiesced))
        (should (buffer-live-p buffer))
        (should-not (plist-member entry :cleanup))))))

(ert-deftest runtime/blocking-waits-on-submitted-run ()
  "The blocking adapter returns delivery from the ordinary submit path."
  (superchat-runtime-test--isolated
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (let ((id 0))
                 (lambda (_event) (cl-incf id))))
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _args)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          '(:status completed :response "blocking done")))))
      (should
       (equal "blocking done"
              (superchat-runtime-submit-blocking
               (list :prompt "hello" :record-conversation nil
                     :buffer (get-buffer-create " *runtime-blocking*"))))))))

(ert-deftest runtime/blocking-timeout-cancels-run ()
  "A blocking wait cancels its accepted Run before signaling timeout."
  (superchat-runtime-test--isolated
    (let (cancelled)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) 'provider-request))
                ((symbol-function 'superchat-runtime-cancel)
                 (lambda (run-id &optional reason)
                   (setq cancelled (list run-id reason))
                   t)))
        (should-error
         (superchat-runtime-submit-blocking
          (list :prompt "hello"
                :buffer (get-buffer-create " *runtime-timeout*")) 0))
        (should (string-prefix-p "run-" (car cancelled)))
        (should (string-match-p "Timed out" (cadr cancelled)))))))

(ert-deftest runtime/request-without-session-gets-session-id ()
  "A Surface may omit Session identity while providing an exact host."
  (superchat-runtime-test--isolated
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (let ((id 0))
                 (lambda (_event) (cl-incf id))))
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat--llm-generate-answer)
               (lambda (&rest _args)
                 (funcall superchat--llm-runtime-observer
                          'terminal
                          '(:status completed :response "done")))))
      (let ((handle
             (superchat-runtime-test-submit
              (list :prompt "hello"
                    :buffer (get-buffer-create " *runtime-session-id*")))))
        (should (string-prefix-p "session-"
                                 (plist-get handle :session-id)))))))

(ert-deftest runtime/forwards-request-local-transport-controls ()
  "Prepared tools and loop controls are scoped to one Runtime request."
  (superchat-runtime-test--isolated
      (let ((wrapper (lambda (tool) tool))
          (round-limit (lambda () 'stop))
          captured active-p context-active-p)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq context-active-p
                         (plist-get superchat--tool-execution-context
                                    :run-active-p)
                         active-p superchat--llm-run-active-p)
                   (setq captured
                         (list superchat--llm-tools-prepared-p
                               superchat--llm-prepared-tools
                               superchat--llm-tool-wrapper
                               superchat--llm-round-limit-function
                               superchat--llm-unknown-tool-retry-limit
                               superchat--llm-persist-synthesized-tools
                               (and (functionp context-active-p)
                                    (funcall context-active-p))))
                   (should (funcall active-p))
                   (should (funcall context-active-p))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit
         (list :prompt "hello"
               :buffer (get-buffer-create " *runtime-controls*")
               :prepared-tools '(tool-a)
               :tool-wrapper wrapper
               :round-limit-function round-limit
               :unknown-tool-retries 7
               :persist-synthesized-tools nil))
        (should (equal (list t '(tool-a) wrapper round-limit 7 nil t)
                       captured))
        (should-not (funcall active-p))
        (should-not (funcall context-active-p))))))

(ert-deftest runtime/record-conversation-can-be-suppressed ()
  "Runtime events remain while legacy conversation hooks can be skipped."
  (superchat-runtime-test--isolated
    (let ((recorded 0))
      (setq superchat-post-turn-functions
            (list #'superchat--hook-record-user-message))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (let ((id 0))
                   (lambda (_event) (cl-incf id))))
                ((symbol-function 'superchat--record-message)
                 (lambda (&rest _args) (cl-incf recorded)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit
         (list :input "hello"
               :prompt "hello"
               :buffer (get-buffer-create " *runtime-no-record*")
               :record-conversation nil))
        (should (= 0 recorded))))))

(ert-deftest runtime/cancel-uses-latest-request-and-ignores-late-result ()
  "Cancellation targets the newest tool-round handle and is terminal."
  (superchat-runtime-test--isolated
    (let (observer cancelled delivered
          (next-id 0)
          events turn-hook)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--response-region-close)
                 (lambda (content &optional _final-p _owner) (setq delivered content)))
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'request-1)
                   (funcall observer 'request 'request-2)
                   'request-2)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--view-request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'request-2 cancelled))
          ;; No stream chunk arrived before the cancel, so the render Sink's
          ;; [Stopped] marker (S02) carries no partial answer -- unlike the
          ;; pre-S02 View, which rendered "[Cancelled by user]" as if it were
          ;; the model's own answer and recorded it as one (N6).
          (should (equal "[Stopped]" delivered))
          (should (eq 'cancelled
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (funcall observer
                   'terminal '(:status completed :response "too late"))
          (should (eq 'cancelled
                      (plist-get (superchat-runtime-status run-id)
                                 :status)))
          (should-not (member "response.completed" events))
          (should
           (equal '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (nreverse events)))
          (should turn-hook))))))

(ert-deftest runtime/local-cancel-wins-synchronous-provider-callback ()
  "A callback raised by provider cancellation cannot complete the Run first."
  (superchat-runtime-test--isolated
    (let (observer cancelled delivered
          (next-id 0)
          events)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--response-region-close)
                 (lambda (content &optional _final-p _owner) (push content delivered)))
                ((symbol-function 'llm-cancel-request)
                 (lambda (request)
                   (setq cancelled request)
                   (funcall observer
                            'terminal
                            '(:status completed :response "provider result"))))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'provider-request)
                   'provider-request)))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--view-request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'provider-request cancelled))
          (should
           (eq 'cancelled
               (plist-get (superchat-runtime-status run-id) :status)))
          ;; No stream chunk arrived before the cancel, so the [Stopped]
          ;; marker (S02) carries no partial answer.
          (should (equal '("[Stopped]") delivered))
          (should
           (equal '("intent.received" "run.started"
                    "run.cancelled" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/failed-run-still-completes-its-turn ()
  "A provider failure terminates both the Run and its owning Turn."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events
          completion-hook turn-hook)
      (setq superchat-run-completed-functions
            (list (lambda (_status) (setq completion-hook t))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status failed :response "provider failed")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'failed (plist-get status :status)))
          (should-not completion-hook)
          (should turn-hook)
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/synchronous-transport-error-closes-run ()
  "A synchronous transport error follows the ordinary failed terminal path."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events delivered turn-hook entry)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id &optional _owner) (setq delivered answer)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (error "network broken"))))
        (should-error
         (superchat-runtime-test-submit
          (superchat-runtime-test--view-request)))
        (maphash (lambda (_run-id value) (setq entry value))
                 superchat-runtime--runs)
        (should (eq 'failed (plist-get entry :status)))
        (should (plist-get entry :turn-completed))
        (should turn-hook)
        (should (string-match-p "network broken" delivered))
        (should (buffer-live-p (plist-get entry :buffer)))
        (should (plist-get entry :quiesced))
        (should
         (equal '("intent.received" "run.started" "response.completed"
                  "run.failed" "turn.completed")
                (nreverse events)))))))

(ert-deftest runtime/prewrite-failure-prevents-provider-call ()
  "A failed run.started append stops before transport execution."
  (superchat-runtime-test--isolated
    (let ((provider-called nil)
          turn-hook)
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "run.started" (plist-get event :type))
                       (error "db unavailable")
                     1)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args) (setq provider-called t))))
        (should-error
         (superchat-runtime-test-submit
          (superchat-runtime-test--request)))
        (should-not provider-called)
        (let (entry)
          (maphash
           (lambda (_run-id value) (setq entry value))
           superchat-runtime--runs)
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should (plist-get entry :turn-completed))
          (should turn-hook))))))

(ert-deftest runtime/prewrite-audit-failure-survives-restart ()
  "A pre-provider audit failure leaves a recoverable durable terminal seal."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-prewrite-recovery-" t))
           (superchat-data-directory directory)
           (append-record
            (symbol-function 'superchat-tape-append-event-record))
           provider-called run-id delivered-status)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal "run.started" (plist-get event :type))
                        (error "db unavailable")
                      (funcall append-record event))))
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args) (setq provider-called t))))
              (should-error
               (superchat-runtime-test-submit
                (append
                 (superchat-runtime-test--request)
                 (list :delivery-function
                       (lambda (_response status _tape-id)
                         (setq delivered-status status))))))
              (maphash (lambda (id _entry) (setq run-id id))
                       superchat-runtime--runs)
              (should-not provider-called)
              (should (eq 'audit-incomplete delivered-status))
              (should
               (eq 'audit-incomplete
                   (plist-get (superchat-runtime-status run-id) :status))))
            (superchat-db-close)
            (setq superchat-runtime--runs (make-hash-table :test #'equal))
            (let* ((trace
                    (superchat-tape-replay-events "session-runtime"))
                   (types (mapcar (lambda (event) (plist-get event :type))
                                  trace))
                   (turn-id (plist-get (car trace) :turn-id))
                   (run-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-run-terminal :run-id run-id)
                     trace))
                   (turn-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-turn-terminal
                           :run-id run-id :turn-id turn-id)
                     trace)))
              (should-not (member "run.started" types))
              (should (equal '("intent.received" "run.failed" "turn.completed")
                             types))
              (should (eq 'proved (plist-get run-judgment :verdict)))
              (should
               (eq 'audit-incomplete
                   (plist-get (plist-get run-judgment :judgment)
                              :terminal-kind)))
              (should (eq 'proved (plist-get turn-judgment :verdict)))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/timed-out-run-closes-despite-hook-errors ()
  "A timed-out Run persists closure and ignores extension hook failures."
  (superchat-runtime-test--isolated
    (let ((next-id 0)
          events
          (hook-calls 0))
      (setq superchat-turn-completed-functions
            (list (lambda (_status)
                    (cl-incf hook-calls)
                    (error "broken extension hook"))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status timed-out :response "provider timeout")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'timed-out (plist-get status :status)))
          (should (= 1 hook-calls))
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.failed" "turn.completed")
                  (nreverse events))))))))

(ert-deftest runtime/default-terminal-delivery-resets-response-region ()
  "Default failed, cancelled, and timed-out delivery leaves no stale region."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (dolist (terminal-status '(failed cancelled timed-out))
        (let* ((suffix (symbol-name terminal-status))
               (session-id (format "response-region-%s" suffix))
               (buffer-name (format " *superchat-response-region-%s*" suffix))
               (superchat-buffer-name buffer-name)
               (buffer (get-buffer-create buffer-name))
               observer)
          (unwind-protect
              (progn
                (with-current-buffer buffer
                  (setq-local superchat-mode t
                              superchat--session-id session-id
                              superchat--conversation-history nil
                              superchat--response-start-marker nil
                              superchat--assistant-response-start-marker nil
                              superchat--assistant-response-end-marker nil
                              superchat--response-region-persistent-marker nil
                              superchat--current-response-parts nil
                              superchat--prompt-start nil)
                  (insert "* User: terminal test\n")
                  (superchat--prepare-for-response))
                (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                           (lambda (_prompt _complete stream-callback &rest _args)
                             (setq observer superchat--llm-runtime-observer)
                             (funcall observer 'request 'response-region-request)
                             (funcall stream-callback "TERMINAL-EARLY")
                             (superchat--insert-persistent-system-message
                              "TERMINAL-NOTICE")
                             (funcall stream-callback "TERMINAL-LATE")
                             (when (eq terminal-status 'failed)
                               (funcall observer
                                        'terminal
                                        '(:status failed
                                          :response "FAILED-RESPONSE")))
                             'response-region-request))
                          ((symbol-function 'superchat--record-message) #'ignore))
                  (let* ((handle
                          (superchat-runtime-test-submit
                           (list :input "terminal test"
                                 :clean-input "terminal test"
                                 :prompt "terminal test"
                                 :session-id session-id
                                 :buffer buffer
                                 :buffer-name buffer-name
                                 :deadline 0)))
                         (run-id (plist-get handle :run-id))
                         (old-markers
                          (with-current-buffer buffer
                            (list superchat--response-start-marker
                                  superchat--assistant-response-start-marker
                                  superchat--assistant-response-end-marker
                                  superchat--response-region-persistent-marker))))
                    (pcase terminal-status
                      ('cancelled (superchat-runtime-cancel run-id))
                      ('timed-out (superchat-runtime--deadline-expired run-id)))
                    (with-current-buffer buffer
                      (dolist (marker old-markers)
                        (should-not (and (markerp marker)
                                         (marker-position marker))))
                      (should-not superchat--response-start-marker)
                      (should-not superchat--assistant-response-start-marker)
                      (should-not superchat--assistant-response-end-marker)
                      (should-not superchat--response-region-persistent-marker)
                      (should-not superchat--current-response-parts)
                      (should (string-match-p "TERMINAL-NOTICE"
                                              (buffer-string)))
                      ;; A fresh Run can use the same View without inheriting
                      ;; the terminal Run's markers or raw pending face.
                      (goto-char (point-max))
                      (insert "NEXT-INPUT")
                      (setq superchat--prompt-start nil)
                      (superchat--prepare-for-response)
                      (superchat--stream-llm-result "NEXT-RAW")
                      (superchat--process-llm-result "NEXT-FINAL")
                      (let ((text (buffer-string)))
                        (should (string-match-p "NEXT-FINAL" text))
                        (should-not (string-match-p "NEXT-RAW" text))
                        (should-not
                         (text-property-any (point-min) (point-max)
                                            'face
                                            'superchat-streaming-pending))
                        (should-not superchat--assistant-response-start-marker)
                        (should-not superchat--assistant-response-end-marker)
                        (should-not superchat--current-response-parts))))))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest runtime/completion-audit-failure-is-visible ()
  "A post-response Tape failure becomes audit-incomplete, never success."
  (superchat-runtime-test--isolated
    (let ((complete-turn
           (symbol-function 'superchat-runtime--complete-turn))
          (next-id 0)
          delivered
          completed-hook turn-hook completion-call)
      (setq superchat-run-completed-functions
            (list (lambda (_status) (setq completed-hook t))))
      (setq superchat-turn-completed-functions
            (list (lambda (_status) (setq turn-hook t))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "response.completed" (plist-get event :type))
                     (error "disk full")
                     (cl-incf next-id))))
                ((symbol-function 'superchat-runtime--complete-turn)
                 (lambda (entry status causation-id &optional unaudited)
                   (setq completion-call (list status unaudited))
                   (funcall complete-turn
                            entry status causation-id unaudited)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id &optional _owner) (setq delivered answer)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (superchat-runtime-test--view-request)))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'audit-incomplete (plist-get status :status)))
          (should (string-match-p "Audit incomplete" delivered))
          (should-not completed-hook)
          (should turn-hook)
          (should (equal '(audit-incomplete t) completion-call)))))))

(ert-deftest runtime/tool-completion-audit-failure-closes-run ()
  "A completed external effect without completion audit closes the Run."
  (superchat-runtime-test--isolated
    (let ((superchat-authority-approval-function (lambda (_request) t))
          (next-id 0)
          events side-effect delivered-status cancelled)
      (superchat-authority-register-effect-resolver
       "runtime_external_send"
       (lambda (_args)
         '((:effect external-write
            :resource message
            :scope "external://recipient/42"))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "tool.completed" (plist-get event :type))
                       (error "completion audit unavailable")
                     (push (plist-get event :type) events)
                     (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'request 'provider-request)
                   (superchat-tool-execute
                    "runtime_external_send"
                    (lambda ()
                      (setq side-effect t)
                      "sent")
                    nil
                    :context superchat--tool-execution-context)
                   ;; A late provider completion cannot recover the Run.
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (list
                  :input "send"
                  :prompt "send"
                  :buffer (get-buffer-create " *runtime-tool-audit*")
                  :attendance 'attended
                  :authority-policy 'cowork
                  :delivery-function
                  (lambda (_value status _tape-id)
                    (setq delivered-status status)))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should side-effect)
          (should (eq 'audit-incomplete (plist-get status :status)))
          (should (eq 'audit-incomplete delivered-status))
          (should (eq 'provider-request cancelled))
          (should-not (member "run.completed" events))
          (should-not (member "turn.completed" events))
          (should (plist-get status :completed-at)))))))

(ert-deftest runtime/secret-tool-result-is-withheld-on-audit-incomplete ()
  "Rejected exact tool output never leaks through Runtime delivery."
  (superchat-runtime-test--isolated
    (let ((superchat-authority-approval-function (lambda (_request) t))
          (next-id 0)
          events delivered delivered-status cancelled)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'request 'provider-request)
                   (let* ((snapshot-id
                           (funcall
                            superchat--llm-runtime-observer
                            'decision-snapshot
                            '(:attempt 1 :attempt-reason "initial"
                              :provider "mock" :model "mock"
                              :prepared-envelope
                              (:format "superchat-prepared-envelope-v0"
                               :attempt-reason "initial"
                               :messages [] :tools []))))
                          (context
                           (append
                            (copy-sequence superchat--tool-execution-context)
                            (list
                             :evidence-attempt 1
                             :decision-snapshot-event-id snapshot-id
                             :tool-call-id "call-1"
                             :tool-call-name "lookup"
                             :tool-call-ordinal 1
                             :tool-call-arguments '((key . "alpha"))))))
                    (superchat-tool-execute
                      "lookup"
                      (lambda (_key) (lambda () "TOP-SECRET-019"))
                      '("alpha") :context context))
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((handle
                (superchat-runtime-test-submit
                 (append
                  (superchat-runtime-test--request)
                  '(:evidence-contract "epistemic-learning-v0"
                    :context-envelope
                    (:contract "context-continuity-v0" :status satisfied
                     :receipt (:tape-head 1)))
                  (list
                   :delivery-function
                   (lambda (value status _tape-id)
                     (setq delivered value delivered-status status))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'audit-incomplete (plist-get status :status)))
          (should (eq 'audit-incomplete delivered-status))
          (should-not (string-match-p "TOP-SECRET-019" delivered))
          (should-not
           (string-match-p "TOP-SECRET-019"
                           (or (plist-get status :error) "")))
          (should (eq 'provider-request cancelled))
          (should-not (member "response.completed" events)))))))

(ert-deftest runtime/audit-incomplete-survives-restart-without-faking-effect ()
  "Tape recovery seals the Run but leaves its unaudited Effect unresolved."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-audit-recovery-" t))
           (superchat-data-directory directory)
           (superchat-authority-approval-function (lambda (_request) t))
           (append-record
            (symbol-function 'superchat-tape-append-event-record))
           run-id)
      (unwind-protect
          (progn
            (superchat-db-close)
            (superchat-authority-register-effect-resolver
             "runtime_recovery_external_send"
             (lambda (_args)
               '((:effect external-write
                  :resource message
                  :scope "external://recipient/42"))))
            (cl-letf
                (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal "tool.completed" (plist-get event :type))
                        (error "completion audit unavailable")
                      (funcall append-record event))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'llm-cancel-request) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (funcall superchat--llm-runtime-observer
                             'request 'provider-request)
                    (superchat-tool-execute
                     "runtime_recovery_external_send"
                     (lambda () "sent") nil
                     :context superchat--tool-execution-context))))
              (let ((handle
                     (superchat-runtime-test-submit
                      (list :input "send"
                            :prompt "send"
                            :session-id "session-audit-recovery"
                            :buffer (get-buffer-create " *runtime-audit-recovery*")
                            :attendance 'attended
                            :authority-policy 'cowork))))
                (setq run-id (plist-get handle :run-id))
                (should
                 (eq 'audit-incomplete
                     (plist-get (superchat-runtime-status run-id) :status)))))
            ;; Simulate a fresh process: only durable recovery + Tape remain.
            (superchat-db-close)
            (setq superchat-runtime--runs (make-hash-table :test #'equal))
            (let* ((trace
                    (superchat-tape-replay-events "session-audit-recovery"))
                   (types (mapcar (lambda (event) (plist-get event :type))
                                  trace))
                   (run-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-run-terminal :run-id run-id)
                     trace))
                   (turn-id (plist-get (car trace) :turn-id))
                   (turn-judgment
                    (superchat-closure-judge
                     (list :kind 'durable-turn-terminal
                           :run-id run-id :turn-id turn-id)
                     trace))
                   (pending
                    (superchat-closure-judge
                     (list :kind 'pending-effects :run-id run-id)
                     trace)))
              (should-not (member "tool.completed" types))
              (should (member "run.failed" types))
              (should (member "turn.completed" types))
              (should (eq 'proved (plist-get run-judgment :verdict)))
              (should
               (eq 'audit-incomplete
                   (plist-get (plist-get run-judgment :judgment)
                              :terminal-kind)))
              (should (eq 'proved (plist-get turn-judgment :verdict)))
              (should (eq 'open (plist-get pending :verdict)))
              (should
               (equal (mapcar (lambda (event) (plist-get event :event-id))
                              trace)
                      (mapcar
                       (lambda (event) (plist-get event :event-id))
                       (superchat-tape-replay-events
                        "session-audit-recovery"))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/audit-incomplete-releases-after-delivery-error ()
  "Audit failure still releases private state when its delivery adapter fails."
  (superchat-runtime-test--isolated
    (let ((next-id 0))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (if (equal "response.completed" (plist-get event :type))
                       (error "disk full")
                     (cl-incf next-id))))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (let* ((request
                (append
                 (superchat-runtime-test--request)
                 (list :delivery-function
                       (lambda (&rest _args)
                         (error "renderer broken")))))
               (handle (superchat-runtime-test-submit request))
               (run-id (plist-get handle :run-id))
               (entry (superchat-runtime--entry run-id)))
          (should (eq 'audit-incomplete (plist-get entry :status)))
          (should-not (plist-get entry :delivery-function))
          (should (buffer-live-p (plist-get entry :buffer)))
          (should (plist-get entry :quiesced)))))))

(ert-deftest runtime/records-one-reviewed-intervention-after-typed-failure ()
  "A typed Authority failure can open one durable same-Run intervention."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-recovery-" t))
           (superchat-data-directory directory)
           (superchat-runtime-recovery-intervention-enabled t)
           initial-view first-view second-view assembled-failure
           consumer-run-id
           assembled-sessions)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function
                   'superchat-experience-assemble-recovery-view)
                  (lambda (failure session-ids)
                    (setq assembled-failure failure
                          assembled-sessions session-ids)
                    (let ((payload (plist-get failure :payload)))
                      (when (and (equal (plist-get payload :tool-name)
                                        "workspace_read")
                                 (equal
                                  (plist-get payload :error-type)
                                  "superchat-workspace-artifact-revision-error"))
                      '(:status satisfied :scope "recovery-scope"
                        :context (:session-id "context-session"
                                  :event-id "evt-context")
                        :prompt "reviewed recovery"
                        :receipt (:candidate
                                  (:session-id "candidate-session"
                                   :event-id "evt-candidate")))))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'superchat--process-llm-result) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (setq initial-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason initial)))
                    (superchat-tool-execute
                     "workspace_read"
                     (lambda (&rest _args)
                       (signal
                        'superchat-workspace-artifact-revision-error
                        '("private stale revision")))
                     '("artifact-1" "stale")
                     :context superchat--tool-execution-context)
                    (setq first-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup))
                          second-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup)))
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status completed :response "done")))))
              (let ((handle
                     (superchat-runtime-test-submit
                      (append (superchat-runtime-test--request)
                              (list :approval-function
                                    (lambda (_request) t))))))
                (setq consumer-run-id (plist-get handle :run-id)))
            (let* ((trace
                    (superchat-tape-replay-events "session-runtime"))
                   (failed
                    (cl-find "tool.failed" trace
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal))
                   (interventions
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "experience.intervention.recorded"
                              (plist-get event :type)))
                     trace))
                   (intervention (car interventions)))
              (should-not initial-view)
              (should (eq 'satisfied (plist-get first-view :status)))
              (should (eq 'satisfied (plist-get second-view :status)))
              (should (equal "tool.failed"
                             (plist-get assembled-failure :type)))
              (should (equal "session-runtime"
                             (plist-get assembled-failure :session-id)))
              (should (equal '("session-runtime") assembled-sessions))
              (should (= 1 (length interventions)))
              (should (equal (plist-get failed :event-id)
                             (plist-get intervention :causation-id)))
              (should
               (equal "evt-candidate"
                      (plist-get
                       (plist-get
                        (plist-get
                         (plist-get intervention :payload) :receipt)
                        :candidate)
                       :event-id)))
              (let ((policy
                     (plist-get
                      (plist-get
                       (superchat-runtime--entry consumer-run-id)
                       :context-envelope)
                      :experience-action-policy)))
                (should (equal "workspace-revision-recovery"
                               (plist-get policy :kind)))
                (should (equal "awaiting-info"
                               (plist-get policy :phase)))
                (should (equal "artifact-1"
                               (plist-get policy :artifact-id))))
              (should-not
               (string-match-p "private stale revision"
                               (format "%S" failed))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/revision-recovery-policy-integrates-authority-and-runtime ()
  "The typed recovery policy blocks only the stale repeat and then clears."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-revision-policy-" t))
           (superchat-data-directory directory)
           (superchat-runtime-recovery-intervention-enabled t)
           (read-executions 0)
           (assembler-called nil)
           first-view
           info-view
           final-view)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function
                   'superchat-experience-assemble-recovery-view)
                  (lambda (_failure _sessions)
                    (setq assembler-called t)
                    '(:status satisfied
                      :scope "recovery-scope"
                      :context (:session-id "context" :event-id "ctx")
                      :prompt "Use workspace_info, then the current revision."
                      :receipt (:candidate (:session-id "candidate"
                                               :event-id "candidate")))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'superchat--process-llm-result) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    ;; The first request is intentionally stale.
                    (superchat-tool-execute
                     "workspace_read"
                     (lambda (&rest _tool-args)
                       (cl-incf read-executions)
                       (signal
                        'superchat-workspace-artifact-revision-error
                        '("stale")))
                     '("artifact-1" "stale")
                     :context superchat--tool-execution-context)
                    (setq first-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup)))
                    ;; A model repeat is refused by Authority; the original
                    ;; function must not run a second time.
                    (superchat-tool-execute
                     "workspace_read"
                     (lambda (&rest _tool-args)
                       (cl-incf read-executions)
                       "should-not-run")
                     '("artifact-1" "stale")
                     :context
                     (superchat--agent-current-runtime-context
                      superchat--tool-execution-context))
                    (superchat-tool-execute
                     "workspace_info"
                     (lambda ()
                       "active Artifact artifact-1 revision=rev-1")
                     nil
                     :context
                     (superchat--agent-current-runtime-context
                      superchat--tool-execution-context))
                    (setq info-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup)))
                    (superchat-tool-execute
                     "workspace_read"
                     (lambda (&rest _tool-args)
                       (cl-incf read-executions)
                       "content")
                     '("artifact-1" "rev-1")
                     :context
                     (superchat--agent-current-runtime-context
                      superchat--tool-execution-context))
                    (setq final-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup)))
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status completed :response "done")))))
              (superchat-runtime-test-submit
               (append (superchat-runtime-test--request)
                       (list :approval-function (lambda (_request) t)))))
            (let* ((entry
                    (cl-loop for value being the hash-values of superchat-runtime--runs
                             when (equal "session-runtime"
                                         (plist-get value :session-id))
                             return value))
                   (policy
                    (plist-get (plist-get entry :context-envelope)
                               :experience-action-policy))
                   (events (superchat-tape-replay-events "session-runtime"))
                   (blocked
                    (cl-find-if
                     (lambda (event)
                       (and (equal "tool.failed" (plist-get event :type))
                            (equal "blocked"
                                   (plist-get (plist-get event :payload)
                                              :status))))
                     events))
                   (execution-events
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "tool.execution.started"
                              (plist-get event :type)))
                     events)))
              (should assembler-called)
              (should (eq 'satisfied (plist-get first-view :status)))
              (should (equal "awaiting-read"
                             (plist-get
                              (plist-get info-view
                                         :experience-action-policy)
                              :phase)))
              (should (equal "satisfied"
                             (plist-get
                              (plist-get final-view
                                         :experience-action-policy)
                              :phase)))
              (should (= 2 read-executions))
              (should (eq 'completed (plist-get entry :status)))
              (should (equal "satisfied" (plist-get policy :phase)))
              (should blocked)
              ;; Initial read, info, corrected read; no execution event for
              ;; the blocked repeat.
              (should (= 3 (length execution-events)))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/blocked-recovery-view-updates-authority-envelope ()
  "A blocked recovery View returns its explicit dependent-action policy."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (superchat-runtime-test--isolated
    (let* ((directory (make-temp-file "superchat-runtime-recovery-gate-" t))
           (superchat-data-directory directory)
           (superchat-runtime-recovery-intervention-enabled t)
           blocked-view)
      (unwind-protect
          (progn
            (superchat-db-close)
            (cl-letf
                (((symbol-function
                   'superchat-experience-assemble-recovery-view)
                  (lambda (_failure _session-ids)
                    '(:status blocked
                      :experience-action-policy
                      (:dependent-tools ("workspace_read")))))
                 ((symbol-function 'superchat--update-status) #'ignore)
                 ((symbol-function 'superchat--process-llm-result) #'ignore)
                 ((symbol-function 'superchat--llm-generate-answer)
                  (lambda (&rest _args)
                    (funcall superchat--llm-runtime-observer
                             'before-attempt
                             '(:attempt-reason initial))
                    (superchat-tool-execute
                     "workspace_read"
                     (lambda ()
                       (signal
                        'superchat-workspace-artifact-revision-error
                        '("private stale revision")))
                     nil :context superchat--tool-execution-context)
                    (setq blocked-view
                          (funcall superchat--llm-runtime-observer
                                   'before-attempt
                                   '(:attempt-reason tool-followup)))
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status completed :response "done")))))
              (let* ((handle
                      (superchat-runtime-test-submit
                       (append (superchat-runtime-test--request)
                               (list :approval-function
                                     (lambda (_request) t)))))
                     (entry (superchat-runtime--entry
                             (plist-get handle :run-id))))
                (should (eq 'blocked (plist-get blocked-view :status)))
                (should
                 (equal '(:dependent-tools ("workspace_read"))
                        (plist-get (plist-get entry :context-envelope)
                                   :experience-action-policy)))
                (should
                 (eq 'blocked
                     (plist-get
                      (plist-get (plist-get entry :context-envelope)
                                 :experience-view)
                      :status))))))
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest runtime/record-message-reuses-runtime-tape-events ()
  "Conversation history reuses Runtime event IDs without legacy duplicates."
  (let ((superchat--conversation-history nil)
        (db-appends 0))
    (cl-letf (((symbol-function 'superchat-db-tape-append)
               (lambda (&rest _args) (cl-incf db-appends))))
      (superchat--record-message "user" "hello" 11)
      (superchat--record-message "assistant" "done" 12)
      (should (= 0 db-appends))
      (should
       (equal '(12 11)
              (mapcar (lambda (message) (plist-get message :id))
                      superchat--conversation-history))))))

(ert-deftest runtime/kernel-custom-configuration-preserves-three-init-states ()
  "Kernel Custom declarations preserve default, saved, and prebound values."
  (let* ((emacs (or (executable-find invocation-name)
                    (executable-find "emacs")))
         (root (file-name-directory (locate-library "superchat-llm")))
         (script (make-temp-file "superchat-kernel-custom-" nil ".el"))
         (data-directory (make-temp-file "superchat-kernel-custom-data-" t))
         ;; `superchat-buffer-name' moved to superchat.el (S25): the kernel
         ;; only `defvar's it, so it is no longer a kernel Custom here.
         (defaults
          '((superchat-context-max-chars)
            (superchat-context-chars-per-token . 2)
            (superchat-context-fixed-reserve . 4096)
            (superchat-response-timeout . 120)
            (superchat-response-timeout-hard-limit . 600)
            (superchat-tool-timeout-multiplier . 1.5)
            (superchat-llm-max-tool-rounds . 12)
            (superchat-llm-unknown-tool-retries . 2)
            (superchat-llm-round-limit-action . ask)
            (superchat-llm-backend)
            (superchat-llm-model)
            (superchat-llm-context-window-overrides)
            (superchat-llm-output-token-reserve . 2048)
            (superchat-llm-streaming . t)
            (superchat-show-ttft . t)
            (superchat-llm-reasoning . none)
            (superchat-show-ttft-breakdown)
            (superchat-llm-tools-enabled . on-demand)))
         (configured
          '((superchat-context-max-chars . 12345)
            (superchat-context-chars-per-token . 3.5)
            (superchat-context-fixed-reserve . 321)
            (superchat-response-timeout . 88)
            (superchat-response-timeout-hard-limit . 444)
            (superchat-tool-timeout-multiplier . 9)
            (superchat-llm-max-tool-rounds . 17)
            (superchat-llm-unknown-tool-retries . 4)
            (superchat-llm-round-limit-action . stop)
            (superchat-llm-backend . saved-backend)
            (superchat-llm-model . "saved-model")
            (superchat-llm-context-window-overrides
             ("saved-model" . 123456))
            (superchat-llm-output-token-reserve . 777)
            (superchat-llm-streaming)
            (superchat-show-ttft)
            (superchat-llm-reasoning . t)
            (superchat-show-ttft-breakdown . t)
            (superchat-llm-tools-enabled))))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (should (= 18 (length defaults)))
          (should (= (length defaults) (length configured)))
          (with-temp-file script
            (prin1
             `(progn
                (require 'cl-lib)
                (setq user-emacs-directory
                      ,(file-name-as-directory data-directory)
                      superchat-data-directory
                      ,(file-name-as-directory
                        (expand-file-name "data" data-directory)))
                (let* ((state (getenv "SUPERCHAT_CUSTOM_TEST_STATE"))
                       (defaults ',defaults)
                       (configured ',configured)
                       (expected (if (equal state "default")
                                     defaults
                                   configured)))
                  (pcase state
                    ("saved"
                     (apply #'custom-set-variables
                            (mapcar
                             (lambda (entry)
                               (list (car entry)
                                     (list 'quote (cdr entry))))
                             configured))
                     (dolist (entry configured)
                       (when (boundp (car entry))
                         (error "saved state bound %s too early" (car entry)))
                       (unless (get (car entry) 'saved-value)
                         (error "saved state missing saved-value for %s"
                                (car entry)))))
                    ("prebound"
                     (dolist (entry configured)
                       (set (car entry) (cdr entry)))
                     (dolist (entry configured)
                       (unless (boundp (car entry))
                         (error "prebound state left %s unbound" (car entry)))))
                    ("default"
                     (dolist (entry defaults)
                       (when (boundp (car entry))
                         (error "default state bound %s before load"
                                (car entry)))))
                    (_ (error "Unknown Custom test state: %S" state)))
                  (require 'superchat-llm)
                  (when (featurep 'superchat)
                    (error "Kernel Custom load pulled in the distribution"))
                  (dolist (entry expected)
                    (unless (equal (symbol-value (car entry)) (cdr entry))
                      (error "%s Kernel value for %s: expected %S, got %S"
                             state (car entry) (cdr entry)
                             (symbol-value (car entry)))))
                  (dolist (entry defaults)
                    (let* ((symbol (car entry))
                           (standard (get symbol 'standard-value)))
                      (unless (get symbol 'custom-type)
                        (error "%s missing custom-type for %s" state symbol))
                      (unless (and standard
                                   (equal (eval (car standard) t) (cdr entry)))
                        (error "%s wrong standard-value for %s: %S"
                               state symbol standard))
                      (unless (assq symbol (get 'superchat 'custom-group))
                        (error "%s missing superchat group membership for %s"
                               state symbol))))
                  (require 'superchat)
                  (dolist (entry expected)
                    (unless (equal (symbol-value (car entry)) (cdr entry))
                      (error "%s distribution value for %s: expected %S, got %S"
                             state (car entry) (cdr entry)
                             (symbol-value (car entry)))))))
             (current-buffer)))
          (dolist (state '("default" "saved" "prebound"))
            (with-temp-buffer
              (let* ((process-environment
                      (cons (concat "SUPERCHAT_CUSTOM_TEST_STATE=" state)
                            process-environment))
                     (status
                      (call-process emacs nil t nil
                                    "-Q" "--batch" "-L" root "-l" script)))
                (unless (= 0 status)
                  (ert-fail
                   (format "Custom child state %s exited %s:\n%s"
                           state status (buffer-string))))))))
      (when (file-exists-p script)
        (delete-file script))
      (when (file-directory-p data-directory)
        (delete-directory data-directory t)))))

;; These are deliberate, reviewed exceptions to the integration source-layer
;; gate.  Each exception is keyed by its enclosing top-level `defun', not a
;; line number: an unrelated edit that only shifts lines elsewhere in FILE
;; must never break or silently widen this gate.  Moving a require to a
;; different defun, or adding another one, must fail until reviewed.
(defconst test-runtime--integration-require-exemptions
  '(("superchat-tools.el" "superchat-subagent"
     superchat-tool-delegate-to-subagent-async)
    ("superchat-tools.el" "superchat-subagent"
     superchat-tool-delegate-to-subagent-parallel-async)
    ("superchat-tools.el" "superchat-genui" superchat-tool-render-ui)
    ("superchat-tools.el" "superchat-subagent"
     superchat--subagent-registry-summary))
  "Exact reviewed exceptions to the integration require gate.")

(defun test-runtime--file-requires (file)
  "Return (FEATURE . ENCLOSING-DEFUN) for every `require' form in FILE.
Walks FILE's forms via `test-runtime--file-forms'/`test-runtime--walk' --
reading the file's structure, not scanning its text for a line number --
so a line shift anywhere in FILE cannot affect the result.  FEATURE is the
required feature's name as a string.  ENCLOSING-DEFUN is the symbol naming
the innermost top-level `defun'/`cl-defun' containing the call, or nil for
a require outside any function."
  (let (found)
    (dolist (form (test-runtime--file-forms file))
      (let ((enclosing
             (and (consp form) (memq (car form) '(defun cl-defun))
                  (nth 1 form))))
        (test-runtime--walk
         form
         (lambda (list)
           (when (and (eq (car list) 'require)
                      (consp (cdr list))
                      (consp (cadr list))
                      (eq (car (cadr list)) 'quote))
             (push (cons (symbol-name (cadr (cadr list))) enclosing) found)))
         #'ignore)))
    (nreverse found)))

(defun test-runtime--integration-require-exemption-p (file feature enclosing-defun)
  "Return non-nil for a reviewed integration FILE/FEATURE/ENCLOSING-DEFUN edge."
  (member (list file feature enclosing-defun)
          test-runtime--integration-require-exemptions))

(defun runtime-test--late-admission (kind async &optional consumer-condition)
  "Exercise KIND admission timing through an actual native ASYNC wrapper.
CONSUMER-CONDITION selects a supplementary captured-context delivery probe."
  (let* ((directory (make-temp-file "superchat-late-admission-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-authority--policy-registry (copy-hash-table superchat-authority--policy-registry))
         (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
         (superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
         (superchat-llm-tools-list nil)
         (superchat--llm-tool-names-signature nil)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (superchat-llm-backend (make-llm-ollama :chat-model "fixture"))
         (superchat-response-timeout nil)
         (superchat-runtime-default-deadline nil)
         (superchat-buffer-name (generate-new-buffer-name " *f05-view*"))
         (session "f05-session") (name "f05_tool")
         (counts (make-hash-table :test #'eq))
         handle view provider prompt response error-cb multi context snapshot
         result (callbacks 0) armed)
    (cl-labels ((counted (key) (puthash key (1+ (gethash key counts 0)) counts))
                (cancel ()
                  (superchat-runtime-cancel (plist-get handle :run-id) "f05 cancel")
                  (setq snapshot (superchat-tape-replay-events session)))
                (transport (p q success failure multiple)
                  (setq provider p prompt q response success error-cb failure multi multiple
                        context (copy-tree superchat--tool-execution-context t))
                  nil))
      (let ((superchat-agent-tool-rewrite-functions
             (list (lambda (_tool args) (when armed (counted 'intercept)) (list :args args))))
            (superchat-agent-permission-functions
             (list (lambda (&rest _) (when armed (counted 'permission)) 'allow)))
            (superchat-agent-pre-tool-functions (list (lambda (&rest _) (counted 'pre))))
            (superchat-agent-post-tool-functions (list (lambda (&rest _) (counted 'post))))
            (superchat-agent-post-tool-failure-functions (list (lambda (&rest _) (counted 'failure)))))
        (unwind-protect
            (progn
              (superchat-db-open (expand-file-name "tape.db" directory))
              (setq view (superchat--ensure-chat-view superchat-buffer-name session))
              (superchat-authority-register-policy
               'f05-policy (lambda (&rest _) (when armed (counted 'policy))
                             (if (eq kind 'approval) 'ask 'defer)))
              (superchat-register-tool
               name :description "F05 fixture" :args nil :async async
               :effects (lambda (_args)
                          (when armed
                            (counted 'resolver)
                            (when (eq kind 'preaudit) (cancel)))
                          '((:effect local-read :resource "fixture")))
               :function (if async
                             (lambda (done) (counted 'effect) (funcall done "effect"))
                           (lambda () (counted 'effect) "effect")))
              (cl-letf (((symbol-function 'llm-chat-async)
                         (lambda (p q success failure &optional multiple)
                           (transport p q success failure multiple)))
                        ((symbol-function 'llm-chat-streaming)
                         (lambda (p q _partial success failure &optional multiple)
                           (transport p q success failure multiple))))
                (setq handle
                      (superchat-runtime-test-submit
                       (list :session-id session :buffer view :input "f05" :prompt "f05"
                             :tools (list name) :authority-policy 'f05-policy
                             :deadline (and (eq kind 'timed-out) 60)
                             :approval-function
                             (lambda (_request)
                               (counted 'approval)
                               (when (eq kind 'approval) (cancel)) t))))
                (should (functionp response))
                (should (functionp (plist-get context :run-active-p)))
                (should (funcall (plist-get context :run-active-p)))
                (should (equal (plist-get handle :run-id) (plist-get context :run-id)))
                (pcase kind
                  ('cancelled (cancel))
                  ('completed (funcall response "final"))
                  ('failed (funcall error-cb 'error "transport failure"))
                  ('timed-out
                   (let ((timer (plist-get (superchat-runtime--entry (plist-get handle :run-id))
                                          :deadline-timer)))
                     (should (timerp timer)) (timer-event-handler timer))))
                (unless (memq kind '(preaudit approval))
                  (should (eq kind (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)))
                  (setq snapshot (superchat-tape-replay-events session)))
                (clrhash counts)
                (setq armed t)
                (if consumer-condition
                    ;; Probe Authority delivery with the captured real context,
                    ;; without changing the upstream scheduler or AgentLoop;
                    ;; this is supplementary to the unchanged native matrix.
                    (let ((original (list consumer-condition "consumer boom" 'original-data))
                          observed)
                      (condition-case condition
                          (superchat-tool-execute
                           name (lambda (&rest _) (counted 'effect)) nil
                           :context context :async t
                           :callback (lambda (value)
                                       (cl-incf callbacks) (setq result value)
                                       (signal (car original) (cdr original))))
                        ((error quit) (setq observed condition)))
                      (should (= 1 callbacks))
                      (should (equal original observed))
                      (should (equal "[Tool call blocked: Run is no longer active]" result))
                      (message "REAL_REFUSAL_CONDITION_PASS kind=%s condition=%s" kind consumer-condition))
                  (llm-provider-utils-execute-tool-uses
                 provider prompt
                 (list (make-llm-provider-utils-tool-use :id "f05-call" :name name :args nil))
                 multi nil
                 (lambda (value) (cl-incf callbacks) (setq result value))
                 (lambda (&rest err) (ert-fail (format "native tool scheduler error %S" err)))))
                (message "F05 kind=%S async=%S counts=%S result=%S" kind async
                         counts result)
                (should (= 1 callbacks))
                (should (string-match-p "Run is no longer active" (format "%S" result)))
                (should (= 0 (gethash 'effect counts 0)))
                (should (= 0 (gethash 'pre counts 0)))
                (if (eq kind 'approval)
                    (let ((types (mapcar (lambda (e) (plist-get e :type)) snapshot)))
                      (should (member "tool.requested" types))
                      (should (member "approval.requested" types))
                      (should (equal snapshot (seq-take (superchat-tape-replay-events session)
                                                       (length snapshot))))
                      (should-not (seq-some
                                   (lambda (event) (equal "tool.execution.started" (plist-get event :type)))
                                   (superchat-tape-replay-events session)))
                      (should (eq 'audit-incomplete
                                  (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)))))
                (unless (eq kind 'approval)
                  (should (equal snapshot (superchat-tape-replay-events session)))
                  (let* ((query (list :kind 'durable-run-terminal :run-id (plist-get handle :run-id)))
                         (decision (superchat-closure-judge query snapshot)))
                    (should (eq 'proved (plist-get decision :verdict)))
                    (should (eq 'valid (plist-get (superchat-closure-judge
                                                   (list :kind 'trace-well-formed :run-id (plist-get handle :run-id))
                                                   snapshot) :verdict)))))
                (if (memq kind '(preaudit approval))
                    (should (> (gethash 'resolver counts 0) 0))
                  (should (= 0 (hash-table-count counts)))
                  ;; Supplementary context-only callback gate, using the actual
                  ;; captured Runtime identity; not a replacement native path.
                  (let ((ctx (copy-sequence context)) (normalizations 0))
                    (setq ctx (plist-put ctx :normalize (lambda (args) (cl-incf normalizations) args)))
                    (should (string-match-p "Run is no longer active"
                                            (superchat-tool-execute name (lambda () (counted 'effect)) nil :context ctx)))
                    (should (= 0 normalizations))
                    (should (equal snapshot (superchat-tape-replay-events session))))
                  ;; A formerly published but now unavailable Runtime entry is
                  ;; not permission to turn the captured predicate into legacy.
                  (remhash (plist-get handle :run-id) superchat-runtime--runs)
                  (should-not (superchat-runtime-status (plist-get handle :run-id)))
                  (should (string-match-p "Run is no longer active"
                                          (superchat-tool-execute name (lambda () (counted 'effect)) nil
                                                                  :context context)))
                  (should (equal snapshot (superchat-tape-replay-events session))))))
          (when handle (superchat-runtime-cancel (plist-get handle :run-id) "fixture cleanup"))
          (when view (superchat-session-workspace-close session))
          (superchat-db-close)
          (delete-directory directory t))))))

(ert-deftest runtime/late-admission-native-terminal-matrix ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-agent-loop nil t)))
  (dolist (kind '(cancelled completed failed timed-out))
    (dolist (async '(nil t)) (runtime-test--late-admission kind async))))

(ert-deftest runtime/late-admission-native-preaudit-cancel ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-agent-loop nil t)))
  (dolist (async '(nil t)) (runtime-test--late-admission 'preaudit async)))

(ert-deftest runtime/late-admission-native-admitted-approval-cancel ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-agent-loop nil t)))
  (dolist (async '(nil t)) (runtime-test--late-admission 'approval async)))

(ert-deftest runtime/late-admission-real-context-consumer-condition ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-agent-loop nil t)))
  (dolist (kind '(cancelled preaudit))
    (dolist (condition '(error quit))
      (runtime-test--late-admission kind t condition))))

(defun runtime-test--refusal-receiver (caller branch condition-kind after batch-size)
  "Observe a real CALLER/BRANCH with a faulty native scheduler receiver.
AFTER means forward to the real LLM continuation before signalling."
  (let* ((directory (make-temp-file "superchat-refusal-" t))
         (superchat-data-directory directory)
         (superchat-skills-directory (expand-file-name "skills" directory))
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat--subagent-running nil)
         (superchat--team-id nil) (superchat--session-id nil)
         (superchat--tool-execution-context nil)
         (superchat-authority--policy-registry (copy-hash-table superchat-authority--policy-registry))
         (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
         (superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
         (superchat-llm-tools-list nil) (superchat--llm-tool-names-signature nil)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (superchat-response-timeout nil) (superchat-runtime-default-deadline nil)
         (superchat-subagent-timeout nil)
         (superchat-agent-max-tool-calls (if (eq branch 'blocked) 0 50))
         (superchat-agent-confirm-destructive nil)
         (superchat-agent-permission-functions nil)
         (superchat--active-authority-policy 'refusal-fixture)
         (superchat-llm-backend (make-llm-ollama :chat-model "fixture"))
         (names (seq-take '("refusal_a" "refusal_b") batch-size))
         (effects 0) (receiver-calls 0) (requests 0) (reports 0)
         (original (list condition-kind "native refusal consumer" 'original-data))
         (superchat-buffer-name (generate-new-buffer-name " *refusal-view*"))
         view session run-id child-id provider prompt response error-cb multi followup
         observed received before-delivery after-delivery immediate-status)
    (let ((superchat-authority-approval-function
           (lambda (_request)
             (when (eq branch 'inactive)
               (superchat-runtime-cancel run-id "approval cancels") t))))
      (cl-labels ((transport (p q success failure multiple)
                   (cl-incf requests)
                   (if (= requests 1)
                       (setq provider p prompt q response success error-cb failure multi multiple)
                     (setq followup success))
                   nil))
        (unwind-protect
            (progn
              (superchat-db-open (expand-file-name "tape.db" directory))
              (superchat-authority-register-policy
               'refusal-fixture (lambda (&rest _) (if (eq branch 'deny) 'deny 'ask)))
              (dolist (name names)
                (superchat-register-tool
                 name :description "Refusal fixture" :args nil :async t
                 :effects '((:effect local-read :resource fixture))
                 :function (lambda (done) (cl-incf effects) (funcall done "effect"))))
              (cl-letf (((symbol-function 'llm-chat-async)
                         (lambda (p q success failure &optional multiple)
                           (transport p q success failure multiple)))
                        ((symbol-function 'llm-chat-streaming)
                         (lambda (p q _partial success failure &optional multiple)
                           (transport p q success failure multiple))))
                (if (eq caller 'subagent)
                    (progn
                      (make-directory superchat-skills-directory t)
                      (with-temp-file (expand-file-name "refusal-agent.md" superchat-skills-directory)
                        (insert "---\nname: refusal-agent\ntype: agent\ncapability: coding\ntools: ["
                                (mapconcat #'identity names ", ") "]\n---\nFixture agent.\n"))
                      (setq child-id (superchat--subagent-run-async
                                      "refusal-agent" "Refusal fixture" nil
                                      (lambda (_report) (cl-incf reports))))
                      (let ((entry (superchat--subagent-entry child-id)))
                        (should entry)
                        (setq session child-id run-id (plist-get entry :run-id)
                              view (get-buffer (plist-get entry :buffer-name)))))
                  (setq session "refusal-agentloop"
                        view (superchat--ensure-chat-view superchat-buffer-name session))
                  (setq run-id
                        (plist-get
                         (superchat-runtime-test-submit
                          (list :session-id session :buffer view :input "Refusal fixture"
                                :prompt "Refusal fixture" :tools names
                                :authority-policy 'refusal-fixture :deadline nil)) :run-id)))
                (should (functionp response))
                (should (eq 'running-model (plist-get (superchat-runtime-status run-id) :status)))
                (dolist (name names)
                  (should (llm-tool-async
                           (cl-find name (llm-chat-prompt-tools prompt) :key #'llm-tool-name :test #'equal))))
                (condition-case condition
                    (llm-provider-utils-execute-tool-uses
                     provider prompt
                     (mapcar (lambda (name)
                               (make-llm-provider-utils-tool-use :id name :name name :args nil)) names)
                     multi nil
                     (lambda (value)
                       (cl-incf receiver-calls)
                       (setq received value before-delivery (superchat-tape-replay-events session))
                       (when (or after (null condition-kind)) (funcall response value))
                       (when condition-kind (signal (car original) (cdr original))))
                     error-cb)
                  ((error quit) (setq observed condition)))
                (setq after-delivery (superchat-tape-replay-events session)
                      immediate-status (plist-get (superchat-runtime-status run-id) :status))
                (message "REFUSAL_NATIVE caller=%s branch=%s condition=%s after=%s batch=%s receiver=%s requests=%s status=%s observed=%S"
                         caller branch condition-kind after batch-size receiver-calls requests immediate-status observed)
                (should (= 1 receiver-calls))
                (should (equal (and condition-kind original) observed))
                (should (= 0 effects))
                (should-not (string-match-p "audit failed" (format "%S" received)))
                (should (equal (sort (mapcar #'car (if multi (plist-get received :tool-results) received))
                                     #'string-lessp)
                               names))
                (when (and condition-kind (not after))
                  (should (equal before-delivery after-delivery))
                  (should (= 1 requests)))
                (let* ((events (seq-filter (lambda (event) (equal run-id (plist-get event :run-id))) after-delivery))
                       (types (mapcar (lambda (event) (plist-get event :type)) events)))
                  (should (= batch-size (cl-count "tool.requested" types :test #'equal)))
                  (should (= batch-size (cl-count "tool.failed" types :test #'equal)))
                  (should-not (member "tool.execution.started" types))
                  (if (eq branch 'inactive)
                      (should (eq immediate-status 'audit-incomplete))
                    (should (eq immediate-status 'running-model))
                    (should (eq 'valid (plist-get (superchat-closure-judge
                                                  (list :kind 'trace-well-formed :run-id run-id) after-delivery)
                                                 :verdict)))))
                (unless (eq branch 'inactive)
                  (when (or after (null condition-kind))
                    (should (= 2 requests))
                    (should (functionp followup))
                    (funcall followup "Refusal final")
                    (should (eq 'completed (plist-get (superchat-runtime-status run-id) :status))))
                  (when (eq caller 'subagent)
                    (should (= reports (if (or after (null condition-kind)) 1 0)))))))
          (when run-id (superchat-runtime-cancel run-id "refusal fixture cleanup"))
          (when session (superchat-session-workspace-close session))
          (when (buffer-live-p view) (kill-buffer view))
          (superchat-db-close)
          (delete-directory directory t))))))

(ert-deftest runtime/refusal-native-receiver-matrix ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-subagent nil t)))
  (dolist (caller '(agent subagent))
    (dolist (branch (if (eq caller 'agent) '(blocked deny declined inactive) '(blocked deny declined)))
      (dolist (kind '(nil error quit))
        (runtime-test--refusal-receiver caller branch kind nil 1)))))

(ert-deftest runtime/refusal-native-multitool-receiver-progress ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t) (require 'superchat-subagent nil t)))
  (dolist (caller '(agent subagent))
    (dolist (after '(nil t))
      (runtime-test--refusal-receiver caller 'deny 'error after 2))))

(defun runtime-test--deadline-acquisition-fault (phase kind &optional submit replace)
  "Inject PHASE/KIND around actual deadline acquisition, optionally SUBMIT.
REPLACE installs a foreign timer before the fault; observe before rescue."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let* ((superchat-closure-terminal-guard-enabled t)
             (superchat-response-timeout nil)
             (view (generate-new-buffer " *deadline-acquisition*"))
             (foreign (run-at-time 60 nil #'ignore))
             (other (list :run-id "other" :status 'running-model :deadline-timer foreign))
             (entry (unless submit (list :run-id "fault" :status 'running-model
                                         :deadline (+ (float-time) 60) :deadline-timer nil)))
             (original (list kind "deadline acquisition fault" phase))
             (activate (symbol-function 'timer-activate))
             (configure (symbol-function 'timer-set-function))
             (update (symbol-function 'superchat-runtime--update))
             (lookup (symbol-function 'superchat-runtime--entry))
             timer run-id observed fired activated (transport 0))
        (cl-labels ((fault (at)
                      (when (and (eq phase at) (not fired))
                        (setq fired t)
                        (when replace
                          (let ((current (gethash run-id superchat-runtime--runs)))
                            (setf (plist-get current :deadline-timer) foreign)))
                        (signal (car original) (cdr original)))))
          (unwind-protect
              (progn
                (superchat-runtime--put other)
                (when entry (superchat-runtime--put entry))
                (cl-letf (((symbol-function 'timer-set-function)
                           (lambda (object fn &optional args)
                             (when (eq fn #'superchat-runtime--deadline-expired)
                               (setq timer object run-id (car args))
                               (fault 'configure-before))
                             (prog1 (funcall configure object fn args)
                               (when (eq object timer) (fault 'configure-after)))))
                          ((symbol-function 'superchat-runtime--update)
                           (lambda (id &rest props)
                             (when (and timer (eq (plist-get props :deadline-timer) timer))
                               (fault 'slot-before))
                             (prog1 (apply update id props)
                               (when (and timer (eq (plist-get props :deadline-timer) timer))
                                 (fault 'slot-after)))))
                          ((symbol-function 'timer-activate)
                           (lambda (object &rest args)
                             (when (eq object timer) (fault 'activate-before))
                             (prog1 (apply activate object args)
                               (when (eq object timer)
                                 (setq activated t)
                                 (fault 'activate-after)))))
                          ((symbol-function 'superchat-runtime--entry)
                           (lambda (id)
                             (when (and activated (equal id run-id)) (fault 'commit))
                             (funcall lookup id)))
                          ((symbol-function 'superchat--llm-generate-answer)
                           (lambda (&rest _) (cl-incf transport))))
                  (condition-case condition
                      (if submit
                          (superchat-runtime-test-submit
                           (list :session-id "deadline-fault" :buffer view
                                 :prompt "wait" :input "wait" :deadline 60))
                        (superchat-runtime--arm-deadline entry))
                    ((error quit) (setq observed condition))))
                (should fired)
                (should (equal original observed))
                (should (timerp timer))
                (should-not (memq timer timer-list))
                (should-not (eq timer (plist-get (gethash run-id superchat-runtime--runs) :deadline-timer)))
                (when replace
                  (should (eq foreign (plist-get (gethash run-id superchat-runtime--runs) :deadline-timer))))
                (should (memq foreign timer-list))
                (should (eq foreign (plist-get other :deadline-timer)))
                (should (buffer-live-p view))
                (should (= 0 transport))
                (when submit
                  (should (eq 'failed (plist-get (superchat-runtime-status run-id) :status)))
                  (let ((events (superchat-tape-replay-events "deadline-fault")))
                    (should (eq 'valid (plist-get (superchat-closure-judge
                                                  (list :kind 'trace-well-formed :run-id run-id) events) :verdict)))
                    (should (= 1 (cl-count "run.failed" events :test #'equal
                                          :key (lambda (e) (plist-get e :type)))))))
                (message "ACQUISITION_PASS phase=%s kind=%s submit=%s replace=%s" phase kind submit replace))
            (when (timerp timer) (cancel-timer timer))
            (cancel-timer foreign)
            (when (buffer-live-p view) (kill-buffer view))))))))

(ert-deftest runtime/deadline-acquisition-fault-matrix ()
  (dolist (phase '(configure-before configure-after slot-before slot-after
                  activate-before activate-after commit))
    (dolist (kind '(error quit))
      (runtime-test--deadline-acquisition-fault phase kind)
      (runtime-test--deadline-acquisition-fault phase kind t)))
  (dolist (phase '(slot-after activate-after commit))
    (dolist (kind '(error quit))
      (runtime-test--deadline-acquisition-fault phase kind nil t))))

(ert-deftest runtime/deadline-acquisition-real-terminal-matrix ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)))
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((superchat-closure-terminal-guard-enabled t)
            (superchat-response-timeout nil) (superchat-llm-streaming nil)
            (superchat-llm-backend (make-llm-ollama :chat-model "fixture"))
            views ids timers response failure other-id other-timer)
        (unwind-protect
            (cl-letf (((symbol-function 'llm-chat-async)
                       (lambda (_p _q done fail &optional _multi)
                         (setq response done failure fail) nil))
                      ((symbol-function 'llm-chat-streaming)
                       (lambda (_p _q _partial done fail &optional _multi)
                         (setq response done failure fail) nil)))
              (let ((other-view (generate-new-buffer " *deadline-other-run*")))
                (push other-view views)
                (setq other-id
                      (plist-get (superchat-runtime-test-submit
                                  (list :session-id "deadline-other" :buffer other-view
                                        :prompt "hold" :input "hold" :tools nil :deadline 60))
                                 :run-id)
                      other-timer (plist-get (superchat-runtime--entry other-id) :deadline-timer))
                (push other-id ids) (push other-timer timers)
                (should (memq other-timer timer-list)))
              (dolist (kind '(cancelled completed failed timed-out disabled))
                (let* ((view (generate-new-buffer " *deadline-positive*"))
                       (_ (push view views))
                       (handle (superchat-runtime-test-submit
                                (list :session-id (format "deadline-%s" kind) :buffer view
                                      :prompt "wait" :input "wait" :tools nil
                                      :deadline (pcase kind ('timed-out 0.02) ('disabled nil) (_ 60)))))
                       (id (plist-get handle :run-id))
                       (timer (plist-get (superchat-runtime--entry id) :deadline-timer)))
                  (push id ids) (when timer (push timer timers))
                  (message "DEADLINE_HANDLE kind=%s handle=%S status=%S timer=%S"
                           kind handle (superchat-runtime-status id) (timerp timer))
                  (if (eq kind 'disabled) (should-not timer)
                    (should (memq timer timer-list)))
                  (pcase kind
                    ('completed (funcall response "done"))
                    ('failed (funcall failure 'error "fixture failure"))
                    ('timed-out
                     (let ((limit (+ (float-time) 1)))
                       (while (and (superchat-runtime-effect-capable-p id) (< (float-time) limit))
                         (sleep-for 0.01))))
                    (_ (superchat-runtime-cancel id "cleanup")))
                  (should (eq (if (eq kind 'disabled) 'cancelled kind)
                              (plist-get (superchat-runtime-status id) :status)))
                  (should-not (plist-get (superchat-runtime--entry id) :deadline-timer))
                  (should-not (memq timer timer-list))
                  (should (buffer-live-p view))
                  (should (eq 'running-model (plist-get (superchat-runtime-status other-id) :status)))
                  (should (eq other-timer (plist-get (superchat-runtime--entry other-id) :deadline-timer)))
                  (should (memq other-timer timer-list))
                  (message "REAL_DEADLINE_PASS %s" kind))))
          (dolist (id ids) (superchat-runtime-cancel id "fixture cleanup"))
          (dolist (timer timers) (cancel-timer timer))
          (dolist (view views) (when (buffer-live-p view) (kill-buffer view))))))))

(ert-deftest runtime/deadline-acquisition-keeps-newer-slot ()
  "An activation reentry replacing A's slot cannot strand or clear its timer."
  (superchat-runtime-test--isolated
    (let* ((foreign (run-at-time 60 nil #'ignore))
           (entry (list :run-id "A" :status 'running-model
                        :deadline (+ (float-time) 60) :deadline-timer nil))
           (other (list :run-id "B" :status 'running-model :deadline-timer foreign))
           (activate (symbol-function 'timer-activate)) owned)
      (unwind-protect
          (progn
            (superchat-runtime--put entry) (superchat-runtime--put other)
            (cl-letf (((symbol-function 'timer-activate)
                       (lambda (timer &rest args)
                         (setq owned timer)
                         (prog1 (apply activate timer args)
                           (setf (plist-get entry :deadline-timer) foreign)))))
              (should (eq entry (superchat-runtime--arm-deadline entry))))
            (should-not (memq owned timer-list))
            (should (memq foreign timer-list))
            (should (eq foreign (plist-get entry :deadline-timer)))
            (should (eq foreign (plist-get other :deadline-timer))))
        (when owned (cancel-timer owned)) (cancel-timer foreign)))))


;;; Exact native transport release (endpoints mocked; scheduler/Authority/Tape real).
(defvar superchat-runtime-test--release-calls nil)
(defvar superchat-runtime-test--release-pending nil)
(defvar superchat-runtime-test--release-aborts nil)
(defvar superchat-runtime-test--release-mode 'async)
(defvar superchat-runtime-test--release-behavior nil)
(defvar superchat-runtime-test--release-cancel-behavior nil)
(defvar superchat-runtime-test--release-deliveries nil)
(defun superchat-runtime-test--release-status (id)
  (plist-get (superchat-runtime-status id) :status))
(defun superchat-runtime-test--release-wait (seconds)
  (let ((until (+ (float-time) seconds)))
    (while (< (float-time) until) (sleep-for 0.01))))
(defun superchat-runtime-test--release-until (predicate &optional seconds)
  (let ((until (+ (float-time) (or seconds 2))))
    (while (and (not (funcall predicate)) (< (float-time) until)) (sleep-for 0.01))
    (should (funcall predicate))))
(defun superchat-runtime-test--release-session-calls (session)
  (cl-remove-if-not (lambda (c) (equal session (plist-get c :session))) superchat-runtime-test--release-calls))
(defun superchat-runtime-test--release-run-ids (session)
  (delete-dups (mapcar (lambda (c) (plist-get c :run)) (superchat-runtime-test--release-session-calls session))))
(defun superchat-runtime-test--release-count-event (session type)
  (cl-count type (superchat-tape-replay-events session) :key (lambda (e) (plist-get e :type)) :test #'equal))
(defun superchat-runtime-test--release-judge (session run)
  (superchat-closure-judge (list :kind 'durable-run-terminal :run-id run)
                           (superchat-tape-replay-events session)))
(defun superchat-runtime-test--release-transport (kind provider prompt partial success failure multi)
  (let* ((context (copy-tree (or superchat--tool-execution-context
                                 (plist-get (cl-find prompt superchat-runtime-test--release-calls :key (lambda (c) (plist-get c :prompt)) :test #'eq) :context)) t))
         (run (plist-get context :run-id))
         (session (plist-get context :session-id))
         (handle (list :release-handle (length superchat-runtime-test--release-calls) :run run :session session))
         (call (list :kind kind :provider provider :prompt prompt :partial partial
                     :success success :failure failure :multi multi :context context
                     :run run :session session :handle handle)))
    (push call superchat-runtime-test--release-calls)
    (cond
     ((eq superchat-runtime-test--release-behavior 'sync-fail) (funcall failure 'error "fixture synchronous failure"))
     ((eq superchat-runtime-test--release-behavior 'sync-complete) (funcall success "fixture synchronous final"))
     ((eq superchat-runtime-test--release-behavior 'delayed-deadline) (superchat-runtime-test--release-wait 0.18))
     ((functionp superchat-runtime-test--release-behavior)
      (funcall superchat-runtime-test--release-behavior call)))
    handle))
(defun superchat-runtime-test--release-tool-response (call)
  "Feed CALL a tool batch through the real native scheduler."
  (llm-provider-utils-process-result
   (plist-get call :provider) (plist-get call :prompt)
   (list :tool-uses
         (list (make-llm-provider-utils-tool-use
                :id (concat "call-" (plist-get call :run))
                :name "runtime_release_ask" :args nil)))
   (plist-get call :multi) (plist-get call :success) (plist-get call :failure)))
(defun superchat-runtime-test--release-answer (pending &optional value)
  (condition-case err (progn (funcall (plist-get pending :done) (or value "Shenzhen")) nil)
    ((error quit) err)))
(defun superchat-runtime-test--release-submit (session &optional deadline buffer)
  (superchat-runtime-test-submit
   (list :session-id session
         :buffer (or buffer (superchat-editor-workspace-buffer session 'release-protocol t))
         :prompt "Call runtime_release_ask for required city" :input "runtime_release_ask"
         :tools '("runtime_release_ask") :deadline deadline :stream-function #'ignore
         :delivery-function (lambda (value status _id) (push (list session status value) superchat-runtime-test--release-deliveries)))))
(defun superchat-runtime-test--release-workflow (session)
  ;; Real public execute, real safe resolver, real file, real Runtime and Closure.
  (let ((superchat--session-id session) (superchat-buffer-name " *runtime-release-no-view*"))
    (superchat-workflow-execute-async "runtime-release")))
(defmacro superchat-runtime-test--release-fixture (&rest body)
  "Run BODY with real native tools and Tape, but no provider endpoints."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (and (require 'llm-ollama nil t)
                       (require 'llm-provider-utils nil t)
                       (fboundp 'llm-provider-utils-process-result)))
     (require 'superchat-workflow)
     (require 'superchat-agent-loop)
     (let* ((dir (make-temp-file "case-" t))
            (superchat-data-directory dir)
            (superchat-workflow-directory (expand-file-name "workflow/" dir))
            (superchat-db--connection nil) (superchat-db--path nil)
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-editor-workspace--states (make-hash-table :test #'equal))
            (superchat-tools--external-registry nil)
            (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
            (superchat-llm-tools-list nil) (superchat--llm-tool-names-signature nil)
            (superchat-llm-tool-names '("runtime_release_ask"))
            (superchat-llm-tools-enabled t)
            (superchat-llm-backend (make-llm-ollama :chat-model "release-fixture"))
            (superchat-response-timeout nil) (superchat-runtime-default-deadline nil)
            (superchat-response-timeout-hard-limit 30) (superchat-tool-timeout-multiplier 1)
            (superchat-closure-terminal-guard-enabled t) (superchat-closure-shadow-enabled nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-agent-permission-functions (list (lambda (&rest _) 'allow)))
            (superchat-runtime-test--release-calls nil)
          (superchat-runtime-test--release-pending nil)
          (superchat-runtime-test--release-aborts nil)
            (superchat-runtime-test--release-behavior nil) (superchat-runtime-test--release-cancel-behavior nil)
            (superchat-runtime-test--release-deliveries nil))
       (unwind-protect
           (progn
             (superchat-db-open (expand-file-name "tape.db" dir))
             (make-directory superchat-workflow-directory t)
             (with-temp-file (expand-file-name "runtime-release.workflow" superchat-workflow-directory)
               (insert "Call runtime_release_ask to get city\nSecond step use the city\nThird step summarize\n"))
             (superchat-register-tool
              "runtime_release_ask" :description "Isolated pending city callback" :args nil :async t
              :effects (lambda (_) '((:effect local-read :resource "release-city")))
              :function (lambda (done)
                          (push (list :done done :context (copy-tree superchat--tool-execution-context t)) superchat-runtime-test--release-pending)))
             (cl-letf (((symbol-function 'llm-capabilities)
			(lambda (_) (if (eq superchat-runtime-test--release-mode 'streaming)
					'(streaming tool-use streaming-tool-use) '(tool-use))))
                       ((symbol-function 'llm-chat-streaming)
			(lambda (p q partial success failure &optional multi)
                          (superchat-runtime-test--release-transport 'streaming p q partial success failure multi)))
                       ((symbol-function 'llm-chat-async)
			(lambda (p q success failure &optional multi)
                          (superchat-runtime-test--release-transport 'async p q nil success failure multi)))
                       ((symbol-function 'llm-cancel-request)
			(lambda (handle)
                          (push handle superchat-runtime-test--release-aborts)
                          (when superchat-runtime-test--release-cancel-behavior (funcall superchat-runtime-test--release-cancel-behavior handle)))))
               ,@body))
	 (maphash (lambda (_ e) (superchat-runtime--quiesce e)) superchat-runtime--runs)
	 (superchat-db-close)
	 (delete-directory dir t)))))


(ert-deftest runtime/release-abnormal-and-late-return ()
  "Real native adapter reports exact returned handles, including late returns."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (kind '(failed sync-fail sync-complete delayed-deadline))
      (superchat-runtime-test--release-fixture
       (setq superchat-runtime-test--release-behavior (unless (eq kind 'failed) kind))
       (let* ((h (superchat-runtime-test--release-submit "A" (and (eq kind 'delayed-deadline) 0.1)))
              (run (plist-get h :run-id)) (call (car superchat-runtime-test--release-calls)))
         (when (eq kind 'failed)
           (funcall (plist-get call :failure) 'error "async failure"))
         (should (eq (pcase kind ('sync-complete 'completed)
                            ('delayed-deadline 'timed-out) (_ 'failed))
                     (superchat-runtime-test--release-status run)))
         (should (equal superchat-runtime-test--release-aborts
                        (unless (eq kind 'sync-complete)
                          (list (plist-get call :handle)))))
         (should (plist-get (superchat-runtime--entry run) :quiesced))
         (should-not (plist-get (superchat-runtime--entry run) :transport-request))
         (superchat-runtime--quiesce (superchat-runtime--entry run))
         (superchat-runtime--finish run 'failed "duplicate terminal")
         (should (= (if (eq kind 'sync-complete) 0 1) (length superchat-runtime-test--release-aborts))))))))

(ert-deftest runtime/release-elapsed-clocks ()
  "Elapsed wait/hard/deadline release transport, with Closure enabled."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (clock '(wait hard deadline))
      (dolist (admitted '(nil t))
        (superchat-runtime-test--release-fixture
         (setq superchat-response-timeout (unless (eq clock 'deadline) 1)
               superchat-response-timeout-hard-limit (if (eq clock 'hard) 1 5)
               superchat-runtime-default-deadline (and (eq clock 'deadline) 0.18))
         (let ((started (float-time)))
           (superchat-runtime-test--release-workflow "A")
           (let* ((call (car superchat-runtime-test--release-calls)) (run (plist-get call :run))
                  (owner (superchat-runtime-transport-timer-owner run)))
             (when admitted (superchat-runtime-test--release-tool-response call))
             (when (eq clock 'hard)
               (if (plist-get call :partial)
                   (funcall (plist-get call :partial) "Output without completion")
                 ;; Async lacks partial output: isolate the real hard clock
                 ;; by cancelling only its sibling wait timer, not firing it.
                 (superchat-llm--transport-timer-owner-cancel-wait owner)))
             (superchat-runtime-test--release-until (lambda () (superchat-runtime--terminal-p (superchat-runtime-test--release-status run))) 2.5)
             (should (>= (- (float-time) started)
                         (if (eq clock 'deadline) 0.15 0.95)))
             (should (eq (if admitted 'audit-incomplete 'timed-out) (superchat-runtime-test--release-status run)))
             (should (equal superchat-runtime-test--release-aborts (list (plist-get call :handle))))
             (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "A" run) :verdict)))
             (when admitted
               (should-not (superchat-runtime-test--release-answer (car superchat-runtime-test--release-pending) "late answer"))
               ;; Preserve generic late outcome semantics; this is NOT an
               ;; explicit-input settlement or an exemption from Closure.
               (should (= 1 (superchat-runtime-test--release-count-event "A" "tool.completed")))
               (should (= 0 (superchat-runtime-test--release-count-event "A" "tool.failed"))))
             (should (= 1 (length superchat-runtime-test--release-calls)))
             (should (= 1 (length (superchat-runtime-test--release-run-ids "A")))))))))))

(ert-deftest runtime/release-cancel-same-buffer-isolation ()
  "Cancelling admitted A releases once, never B in the exact same buffer."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--release-fixture
     (setq superchat-response-timeout 2 superchat-runtime-test--release-behavior (lambda (_) nil))
     (superchat-runtime-test--release-workflow "S")
     (let* ((a (car superchat-runtime-test--release-calls)) (ra (plist-get a :run))
            (host (plist-get (superchat-runtime--entry ra) :buffer))
            (hb (superchat-runtime-test--release-submit "S" 5 host)) (rb (plist-get hb :run-id))
            (b (car superchat-runtime-test--release-calls)) (owner (superchat-runtime-transport-timer-owner rb))
            (wait (superchat-llm--transport-timer-owner-wait-timer owner))
            (hard (superchat-llm--transport-timer-owner-hard-timer owner))
            (deadline (plist-get (superchat-runtime--entry rb) :deadline-timer)))
       (superchat-runtime-test--release-tool-response a)
       (superchat-runtime-cancel ra)
       (should (eq 'audit-incomplete (superchat-runtime-test--release-status ra)))
       (should (eq 'running-model (superchat-runtime-test--release-status rb)))
       (should (eq owner (superchat-runtime-transport-timer-owner rb)))
       (should (memq wait timer-list)) (should (memq hard timer-list))
       (should (memq deadline timer-list))
       (should (eq (plist-get b :handle)
                   (plist-get (superchat-runtime--entry rb) :transport-request)))
       (should (= 1 (length superchat-runtime-test--release-aborts)))
       (should (eq (car superchat-runtime-test--release-aborts) (plist-get a :handle)))
       (should-not (superchat-runtime-cancel ra))
       (superchat-runtime--quiesce (superchat-runtime--entry ra))
       (should-not (superchat-runtime-test--release-answer (car superchat-runtime-test--release-pending) "late A"))
       (funcall (plist-get b :success) "B completed")
       (should (eq 'completed (superchat-runtime-test--release-status rb)))
       (should (= 1 (length superchat-runtime-test--release-aborts)))
       (should (= 2 (length superchat-runtime-test--release-calls)))
       (should (buffer-live-p host))))))

(ert-deftest runtime/release-adapter-conditions-and-reentry ()
  "Only cancel-adapter error/quit is isolated; published state wins reentry."
  (dolist (kind '(error quit reentry))
    (superchat-runtime-test--release-fixture
     (setq superchat-runtime-test--release-behavior (lambda (_) nil))
     (let* ((ha (superchat-runtime-test--release-submit "S")) (ra (plist-get ha :run-id))
            (a (car superchat-runtime-test--release-calls))
            (host (plist-get (superchat-runtime--entry ra) :buffer)) hb observed observed-handle)
       (setq superchat-runtime-test--release-cancel-behavior
             (lambda (handle)
               ;; Assert outside the adapter: its deliberate error isolation
               ;; would otherwise swallow an ERT assertion too.
               (setq observed (copy-sequence (superchat-runtime--entry ra))
                     observed-handle handle)
               (superchat-runtime--quiesce (superchat-runtime--entry ra))
               (if (eq kind 'reentry)
                   (progn
                     (setq hb (superchat-runtime-test--release-submit "S" 5 host))
                     (funcall (plist-get a :success) "late A from abort"))
                 (signal kind '("cancel adapter" original-data)))))
       (let ((condition (condition-case err (progn (superchat-runtime-cancel ra) nil)
                          ((error quit) err))))
         (should-not condition))
       (should (eq 'cancelled (plist-get observed :status)))
       (should (plist-get observed :quiesced))
       (should (eq observed-handle (plist-get a :handle)))
       (dolist (slot '(:transport-request :transport-timer-owner :deadline-timer
                       :delivery-function :stream-function))
         (should-not (plist-get observed slot)))
       (when (eq kind 'reentry) (should hb))
       (should (eq 'cancelled (superchat-runtime-test--release-status ra)))
       (should (= 1 (length superchat-runtime-test--release-aborts)))
       (when hb
         (let* ((rb (plist-get hb :run-id)) (b (car superchat-runtime-test--release-calls)))
           (should (eq 'running-model (superchat-runtime-test--release-status rb)))
           (should (eq (plist-get b :handle)
                       (plist-get (superchat-runtime--entry rb) :transport-request)))
           (funcall (plist-get b :success) "B normal")
           (should (eq 'completed (superchat-runtime-test--release-status rb)))
           (should (= 1 (length superchat-runtime-test--release-aborts)))))))))

(ert-deftest runtime/release-audit-failure-priority ()
  "Failed tool receipt stays audit-incomplete even when cancellation signals."
  (dolist (kind '(error quit))
    (superchat-runtime-test--release-fixture
     (superchat-runtime-test--release-workflow "A")
     (let* ((call (car superchat-runtime-test--release-calls)) (run (plist-get call :run))
            (original (symbol-function 'superchat-tape-append-event-record)))
       (superchat-runtime-test--release-tool-response call)
       (setq superchat-runtime-test--release-cancel-behavior (lambda (_) (signal kind '("audit abort"))))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal (plist-get event :type) "tool.completed")
                        (error "injected append failure")
                      (funcall original event)))))
         (should-not (superchat-runtime-test--release-answer (car superchat-runtime-test--release-pending)))
         (should (eq 'audit-incomplete (superchat-runtime-test--release-status run)))
         (should (plist-get (superchat-runtime--entry run) :quiesced))
         (should (= 1 (length superchat-runtime-test--release-aborts)))
         (should (eq (car superchat-runtime-test--release-aborts) (plist-get call :handle)))
         (should (= 1 (length superchat-runtime-test--release-calls)))
         (should (= 0 (superchat-runtime-test--release-count-event "A" "tool.completed")))
         (should (= 0 (superchat-runtime-test--release-count-event "A" "tool.failed"))))))))

(ert-deftest runtime/release-native-hidden-generation-gap ()
  "Characterize H1 loss: native synchronous H2 starts before H1 returns.
Runtime cannot release H1 it never observes.  This is a named adapter gap,
not a successful test of all-generation transport release."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (outcome '(cancel complete nested-fail nested-complete))
      (superchat-runtime-test--release-fixture
       (setq superchat-runtime-test--release-behavior
             (lambda (call)
               (if (= 1 (length superchat-runtime-test--release-calls))
                   (progn
                     (superchat-runtime-test--release-tool-response call)
                     (superchat-runtime-test--release-answer
                      (car superchat-runtime-test--release-pending)))
                 (pcase outcome
                   ('nested-fail
                    (funcall (plist-get call :failure) 'error "nested failure"))
                   ('nested-complete
                    (funcall (plist-get call :success) "nested success"))))))
       (let* ((h (superchat-runtime-test--release-submit "A"))
              (run (plist-get h :run-id))
              (h2 (plist-get (car superchat-runtime-test--release-calls) :handle))
              (h1 (plist-get (cadr superchat-runtime-test--release-calls) :handle)))
         (should (= 2 (length superchat-runtime-test--release-calls)))
         (pcase outcome
           ('nested-fail
            (should (eq 'failed (superchat-runtime-test--release-status run))))
           ('nested-complete
            (should (eq 'completed (superchat-runtime-test--release-status run))))
           (_
            (should (eq h2 (plist-get (superchat-runtime--entry run) :transport-request)))
            (if (eq outcome 'cancel)
                (superchat-runtime-cancel run)
              (funcall (plist-get (car superchat-runtime-test--release-calls) :success)
                       "normal success")
              (should (eq 'completed (superchat-runtime-test--release-status run))))))
         (should (equal (and (memq outcome '(cancel nested-fail)) (list h2))
                        superchat-runtime-test--release-aborts))
         (should-not (memq h1 superchat-runtime-test--release-aborts)))))))

(defmacro superchat-runtime-test--input-fixture (&rest body)
  "Use native capabilities; substitute only the model endpoints and presenter."
  (declare (indent 0) (debug t))
  `(let ((native-capabilities (symbol-function 'llm-capabilities)))
     (superchat-runtime-test--release-fixture
      (require 'llm-openai)
      (setq superchat-llm-backend
            (make-llm-openai-compatible
             :url "http://invalid.test"
             :chat-model (if (eq superchat-runtime-test--release-mode 'streaming)
                             "qwen/qwen3.8-27b" "input-core-async-fixture")))
      ;; S08: the adapter rule grants both capabilities to any model on
      ;; the exact `llm-openai-compatible' type, so the async mode needs
      ;; its non-streaming behavior declared explicitly now.
      (let ((superchat-llm-model-capability-overrides
             (unless (eq superchat-runtime-test--release-mode 'streaming)
               (list (list "input-core-async-fixture"
                           :streaming nil :streaming-tool-use nil)))))
        (cl-letf (((symbol-function 'llm-capabilities) native-capabilities))
          ,@body)))))

(ert-deftest runtime/trusted-ask-detaches-selection-before-adapter-reentry ()
  (let* ((superchat-llm-tools-enabled nil)
         (tools (list "ask_user"))
         (accepted (lambda (_) nil))
         (request (list :prompt "question" :tools tools :accepted-function accepted
                        :input-core (list :enabled t :operation 'ask-user :tool-name "ask_user"
                                          :presenter #'ignore)))
         (config (plist-get request :input-core)))
    (setq config
          (plist-put config :eligible-function
                     (lambda ()
                       (setcar tools "shell-command")
                       (plist-put request :accepted-function #'ignore)
                       (plist-put config :presenter #'error)
                       t)))
    (plist-put request :input-core config)
    (let ((prepared (superchat-runtime--prepare-input-request request)))
      (should (equal '("ask_user") (plist-get prepared :tools)))
      (should (eq accepted (plist-get prepared :accepted-function)))
      (should (eq #'ignore (plist-get (plist-get prepared :input-core) :presenter))))))

(ert-deftest runtime/trusted-ask-invalid-eligibility-before-publication ()
  (superchat-runtime-test--isolated
    (should-error
     (superchat-runtime-test-submit
      '(:prompt "question" :tools ("ask_user")
        :input-core (:enabled t :operation ask-user :tool-name "ask_user"
                     :presenter ignore :eligible-function 42))))
    (should (= 0 (hash-table-count superchat-runtime--runs)))))

(ert-deftest runtime/trusted-ask-unsupported-routes-before-publication ()
  (superchat-runtime-test--isolated
    (dolist (extra '((:parent-run-id "parent") (:topology subagent) (:topology team)
                     (:attendance unattended) (:prepared-tools nil) (:tool-wrapper ignore)))
      (let ((calls 0))
        (should-error
         (superchat-runtime--prepare-input-request
          (append extra
                  (list :tools '("ask_user")
                        :input-core (list :enabled t :operation 'ask-user :tool-name "ask_user"
                                          :presenter #'ignore
                                          :eligible-function (lambda () (cl-incf calls) t))))))
        (should (= 0 calls))))
    (should (= 0 (hash-table-count superchat-runtime--runs)))))

(ert-deftest runtime/input-core-stop-after-committed-terminal ()
  "Opted-in stop revokes delivery without rewriting a completed Tape fact."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (notification-condition '(nil error quit))
      (superchat-runtime-test--input-fixture
       (let ((append-event (symbol-function 'superchat-tape-append-event-record))
             (notifications 0) accepted stopped-before-notify active-in-hook run)
         (let* ((handle
                 (superchat-runtime-test-submit
                  (list :session-id "core-A"
                        :buffer (superchat-editor-workspace-buffer "core-A" 'release-protocol t)
                        :prompt "Get the city" :tools '("runtime_release_ask")
                        :deadline nil :stream-function #'ignore
                        :input-core
                        (list :enabled t :stop-consumer
                              (lambda (snapshot)
                                (cl-incf notifications)
                                (setq stopped-before-notify
                                      (and (plist-get snapshot :stop-requested)
                                           (not (superchat-runtime-effect-capable-p run))))
                                (should (superchat-runtime-cancel run))
                                (when notification-condition
                                  (signal notification-condition '("STOP CLEANUP")))))
                        :delivery-function
                        (lambda (&rest args)
                          (push args superchat-runtime-test--release-deliveries)))))
                (first (car superchat-runtime-test--release-calls)))
           (setq run (plist-get handle :run-id))
           (superchat-runtime-test--release-tool-response first)
           (should-not (superchat-runtime-test--release-answer
                        (car superchat-runtime-test--release-pending)))
           (should (= 2 (length superchat-runtime-test--release-calls)))
           (let ((superchat-run-completed-functions
                  (list (lambda (_)
                          (setq active-in-hook
                                (plist-get (superchat-runtime-status run) :control-active-p))))))
             (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                        (lambda (event)
                          (prog1 (funcall append-event event)
                            (when (and (equal run (plist-get event :run-id))
                                       (equal "run.completed" (plist-get event :type)))
                              (setq accepted (superchat-runtime-cancel run)))))))
               (funcall (plist-get (car superchat-runtime-test--release-calls) :success)
                        "Final city answer")))
           (message "INPUT_CORE_TERMINAL accepted=%S deliveries=%S completed=%s cancelled=%s notifications=%s"
                    accepted superchat-runtime-test--release-deliveries
                    (superchat-runtime-test--release-count-event "core-A" "run.completed")
                    (superchat-runtime-test--release-count-event "core-A" "run.cancelled") notifications)
           (should accepted)
           (should stopped-before-notify)
           (should active-in-hook)
           (should (= notifications 1))
           (should-not superchat-runtime-test--release-deliveries)
           (should (eq 'completed (superchat-runtime-test--release-status run)))
           (should (= 1 (superchat-runtime-test--release-count-event "core-A" "run.completed")))
           (should (= 0 (superchat-runtime-test--release-count-event "core-A" "run.cancelled")))
           (should (= 1 (superchat-runtime-test--release-count-event "core-A" "tool.completed")))
           (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "core-A" run) :verdict)))
           (should-not (plist-get (superchat-runtime-status run) :control-active-p))
           (should-not (superchat-runtime-cancel run))))))))

(ert-deftest runtime/input-core-question-once-and-cancel ()
  "A real native invocation exposes one answer/stop settlement, never both."
  (dolist (outcome '(answer cancel answer-cancel cancel-answer))
    (superchat-runtime-test--input-fixture
     (let (question capability run)
       (superchat-register-tool
        "runtime_release_ask" :description "Controlled input presenter" :args nil :async t
        :effects '((:effect local-read :resource "core-question"))
        :function (lambda (_done)
                    (setq capability (plist-get superchat--tool-execution-context :input-settlement))
                    (superchat-runtime--request-input capability '(:question "City?"))))
       (let* ((h (superchat-runtime-test-submit
                  (list :session-id "question" :buffer (superchat-editor-workspace-buffer "question" 'release-protocol t)
                        :prompt "Question" :tools '("runtime_release_ask") :deadline nil
                        :stream-function #'ignore :delivery-function #'ignore
                        :input-core (list :enabled t :tool-name "runtime_release_ask"
                                          :presenter (lambda (snapshot) (setq question snapshot))))))
              (call (car superchat-runtime-test--release-calls)))
         (setq run (plist-get h :run-id))
         (superchat-runtime-test--release-tool-response call)
         (should (functionp capability))
         (should question)
         (let ((handle (plist-get question :handle))
               (append-event (symbol-function 'superchat-tape-append-event-record)))
           (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                      (lambda (event)
                        (prog1 (funcall append-event event)
                          (cond
                           ((and (eq outcome 'answer-cancel) (equal (plist-get event :type) "tool.completed"))
                            (superchat-runtime-cancel run))
                           ((and (eq outcome 'cancel-answer) (equal (plist-get event :type) "tool.failed"))
                            (should-not (superchat-runtime-answer-input run handle "late"))))))))
             (if (memq outcome '(answer answer-cancel))
                 (superchat-runtime-answer-input run handle "Shenzhen")
               (should (superchat-runtime-cancel run))))
           (should-not (superchat-runtime-answer-input run handle "duplicate"))
           (should (= 1 (+ (superchat-runtime-test--release-count-event "question" "tool.completed")
                           (superchat-runtime-test--release-count-event "question" "tool.failed"))))
           (if (eq outcome 'answer)
               (progn
                 (should (= 2 (length superchat-runtime-test--release-calls)))
                 (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "final")
                 (should (eq 'completed (superchat-runtime-test--release-status run))))
             (should (= 1 (length superchat-runtime-test--release-calls)))
             (should (eq 'cancelled (superchat-runtime-test--release-status run))))
           (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "question" run) :verdict)))))))))

(ert-deftest runtime/input-core-active-stop-gates-and-ordinary-control ()
  "Try actual native tool/model progression while stopped A is still running-model."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (let ((effects 0) stopped-state)
       (superchat-register-tool
        "runtime_release_ask" :description "Immediate fixture effect" :args nil :async t
        :effects '((:effect local-read :resource "active-probe"))
        :function (lambda (done) (cl-incf effects) (funcall done "city")))
       (let* ((a (superchat-runtime-test-submit
                  (list :session-id "active-A" :buffer (superchat-editor-workspace-buffer "active-A" 'release-protocol t)
                        :prompt "A" :tools '("runtime_release_ask") :deadline nil
                        :input-core '(:enabled t) :stream-function #'ignore :delivery-function #'ignore)))
              (ca (car superchat-runtime-test--release-calls))
              (b (superchat-runtime-test--release-submit "ordinary-B"))
              (cb (car superchat-runtime-test--release-calls)))
         ;; Unstopped executable control: actual scheduler -> Authority -> effect -> model followup.
         (superchat-runtime-test--release-tool-response cb)
         (should (= 1 effects))
         (should (= 3 (length superchat-runtime-test--release-calls)))
         (setq superchat-runtime-test--release-cancel-behavior
               (lambda (_handle)
                 (unless stopped-state
                   (setq stopped-state (superchat-runtime-status (plist-get a :run-id))))
                 ;; Stop must be published before this actual cancellation adapter.
                 (superchat-runtime-test--release-tool-response ca)
                 ;; Independently try the real LLM response -> send-request path.
                 (funcall (plist-get ca :success) '(:tool-results (("fixture" . "late"))))))
         (should (superchat-runtime-cancel (plist-get a :run-id)))
         (should (equal superchat-runtime-test--release-aborts
                        (list (plist-get ca :handle))))
         ;; B has already advanced; its newest exact handle stays registered.
         (should (eq (plist-get (car superchat-runtime-test--release-calls) :handle)
                     (plist-get (superchat-runtime--entry (plist-get b :run-id)) :transport-request)))
         (should (eq (plist-get stopped-state :status) 'running-model))
         (should (plist-get stopped-state :stop-requested))
         (should (= 1 effects))
         (should (= 3 (length superchat-runtime-test--release-calls)))
         (should (superchat-runtime-effect-capable-p (plist-get b :run-id)))
         (setq superchat-runtime-test--release-cancel-behavior nil)
         (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "B final")
         (should (eq 'completed (superchat-runtime-test--release-status (plist-get b :run-id))))
         (should-not (plist-member (superchat-runtime-status (plist-get b :run-id)) :control-active-p)))))))

(ert-deftest runtime/input-core-stop-in-pre-tool-hook ()
  "An admitted ordinary tool cannot enter its effect after a reentrant stop."
  (superchat-runtime-test--input-fixture
   (let ((effects 0) run)
     (superchat-register-tool
      "runtime_release_ask" :description "Prehook effect" :args nil :async t
      :effects '((:effect local-read :resource "prehook"))
      :function (lambda (done) (cl-incf effects) (funcall done "result")))
     (setq run (plist-get
                (superchat-runtime-test-submit
                 (list :session-id "prehook" :buffer (superchat-editor-workspace-buffer "prehook" 'release-protocol t)
                       :prompt "Prehook" :tools '("runtime_release_ask") :deadline nil
                       :input-core '(:enabled t) :stream-function #'ignore :delivery-function #'ignore))
                :run-id))
     (let ((superchat-agent-pre-tool-functions
            (list (lambda (&rest _) (superchat-runtime-cancel run)))))
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls)))
     (should (= effects 0))
     (should (= 1 (length superchat-runtime-test--release-calls))))))

(defun superchat-runtime-test--input-submit (session &optional config delivery)
  "Submit the controlled input fixture through the public Runtime boundary."
  (superchat-runtime-test-submit
   (list :session-id session :buffer (superchat-editor-workspace-buffer session 'release-protocol t)
         :prompt "Core question" :tools '("runtime_release_ask") :deadline nil
         :input-core config :stream-function #'ignore :delivery-function (or delivery #'ignore))))

(defmacro superchat-runtime-test--with-question (&rest body)
  "Bind actual published input snapshots/capabilities around BODY."
  (declare (indent 0) (debug t))
  `(superchat-runtime-test--input-fixture
    (let (questions capabilities)
      (superchat-register-tool
       "runtime_release_ask" :description "Controlled question" :args nil :async t
       :effects '((:effect local-read :resource "input-core"))
       :function (lambda (_done)
                   (let ((capability (plist-get superchat--tool-execution-context :input-settlement)))
                     (push capability capabilities)
                     (superchat-runtime--request-input capability '(:question "City?")))))
      (let* ((config (list :enabled t :tool-name "runtime_release_ask"
                           :presenter (lambda (snapshot) (push snapshot questions))))
             (h (superchat-runtime-test--input-submit "input" config))
             (run (plist-get h :run-id))
             (call (car superchat-runtime-test--release-calls)))
        (superchat-runtime-test--release-tool-response call)
        (should (= 1 (length questions) (length capabilities)))
        ,@body))))

(ert-deftest runtime/input-core-config-before-publication ()
  "Config is per-Run, captured, default-off and validated before acceptance."
  (superchat-runtime-test--input-fixture
   (dolist (config '(t (:enabled nil) (:enabled t :unknown t)
                      (:enabled t :stop-consumer invalid)
                      (:enabled t :tool-name "x") (:enabled t :presenter invalid)))
     (let ((buffer (generate-new-buffer " *input-invalid*")))
       (unwind-protect
           (should-error
            (superchat-runtime-test-submit (list :prompt "invalid" :buffer buffer :input-core config)))
         (kill-buffer buffer)))
     (should (= 0 (hash-table-count superchat-runtime--runs)))
     (should-not superchat-runtime-test--release-calls))
   (let* ((config (list :enabled t))
          (a (superchat-runtime-test--input-submit "captured" config))
          (b (superchat-runtime-test--input-submit "default")))
     (setf (plist-get config :enabled) nil)
     (should (plist-get (superchat-runtime-status (plist-get a :run-id)) :control-active-p))
     (should-not (plist-member (superchat-runtime-status (plist-get b :run-id)) :control-active-p))
     (should (superchat-runtime-cancel (plist-get a :run-id)))
     (should (superchat-runtime-effect-capable-p (plist-get b :run-id)))
     (should-not (plist-get (superchat-runtime-status (plist-get a :run-id)) :control-active-p)))))

(ert-deftest runtime/input-core-receipt-exception-and-readback ()
  "Distinguish real absent, committed, conflicting and unreadable receipts."
  (dolist (kind '(error quit))
    (dolist (where '(before after observer conflict duplicate read-failed))
      (superchat-runtime-test--with-question
       (let ((append-event (symbol-function 'superchat-tape-append-event-record))
             (replay (symbol-function 'superchat-tape-replay-events))
             (observer (symbol-function 'superchat-runtime--recovery-tool-completed))
             read-failed caught)
         (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                    (lambda (event)
                      (if (not (equal "tool.completed" (plist-get event :type)))
                          (funcall append-event event)
                        (pcase where
                          ('before (signal kind '("BEFORE RECEIPT" original)))
                          ('after (funcall append-event event) (signal kind '("AFTER RECEIPT" original)))
                          ('conflict
                           (let ((wrong (copy-tree event t)))
                             (setf (plist-get (plist-get wrong :payload) :result-hash) "sha256:WRONG")
                             (funcall append-event wrong)))
                          ('duplicate
                           (prog1 (funcall append-event event)
                             (let ((extra (copy-tree event t)))
                               (setf (plist-get extra :event-id) (superchat-tape--new-event-id))
                               (funcall append-event extra))))
                          ('read-failed (prog1 (funcall append-event event) (setq read-failed t)))
                          (_ (funcall append-event event))))))
                   ((symbol-function 'superchat-tape-replay-events)
                    (lambda (&rest args)
                      (if read-failed (signal kind '("READ FAILURE")) (apply replay args))))
                   ((symbol-function 'superchat-runtime--recovery-tool-completed)
                    (lambda (&rest args)
                      (prog1 (apply observer args)
                        (when (eq where 'observer) (signal kind '("OBSERVER" original)))))))
           (setq caught (condition-case err
                            (superchat-runtime-answer-input run (plist-get (car questions) :handle) "深圳\n  city")
                          ((error quit) err))))
         (should (consp caught))
         (when (memq where '(before after observer))
           (should (equal caught (list kind (pcase where ('before "BEFORE RECEIPT")
                                                   ('after "AFTER RECEIPT") (_ "OBSERVER")) 'original))))
         (let ((facts (funcall (car capabilities) 'snapshot)))
           (should (eq 'answer (plist-get facts :claim)))
           (should (eq (pcase where ('before 'absent) ((or 'after 'observer) 'committed) (_ 'unknown))
                       (plist-get facts :receipt-state)))
           (should (eq 'not-started (plist-get facts :delivery))))
         (should (eq (if (eq where 'observer) 'failed 'audit-incomplete)
                     (superchat-runtime-test--release-status run)))
         (should (= 1 (length superchat-runtime-test--release-calls)))
         (let ((before (funcall replay "input")))
           (should-not (superchat-runtime-answer-input run (plist-get (car questions) :handle) "again"))
           (should (equal before (funcall replay "input")))))))))

(ert-deftest runtime/input-core-q2-retirement-and-synchronous-final ()
  "A nested Q2 is not cleared by Q1; synchronous final still finishes once."
  (dolist (outcome '(q2 final))
    (superchat-runtime-test--with-question
     (let ((q1 (car questions)) (c1 (car capabilities)))
       (setq superchat-runtime-test--release-behavior
             (lambda (next)
               (when (= 2 (length superchat-runtime-test--release-calls))
                 (if (eq outcome 'q2)
                     (superchat-runtime-test--release-tool-response next)
                   (funcall (plist-get next :success) "synchronous final")))))
       (should (superchat-runtime-answer-input run (plist-get q1 :handle) "first"))
       (should (eq 'returned (plist-get (funcall c1 'snapshot) :delivery)))
       (if (eq outcome 'q2)
           (progn
             (should (= 2 (length questions)))
             (should (eq (plist-get (car questions) :handle)
                         (plist-get (superchat-runtime-pending-input run) :handle)))
             (should-not (superchat-runtime-answer-input run (plist-get q1 :handle) "late Q1"))
             (should (superchat-runtime-cancel run))
             (should-not (superchat-runtime-answer-input run (plist-get (car questions) :handle) "late Q2"))
             (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.completed")))
             (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.failed"))))
         (should (eq 'completed (superchat-runtime-test--release-status run)))
         (should (= 1 (superchat-runtime-test--release-count-event "input" "run.completed"))))
       (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "input" run) :verdict)))))))

(ert-deftest runtime/input-core-expired-answer ()
  "An undispatched absolute deadline blocks an answer."
  (superchat-runtime-test--with-question
   (let ((handle (plist-get (car questions) :handle)))
     (superchat-runtime--update run :deadline (- (float-time) 1))
     (should-not (superchat-runtime-answer-input run handle "late"))
     (should (eq 'timed-out (superchat-runtime-test--release-status run)))
     (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.failed")))
     (should-not (superchat-runtime-answer-input run handle "duplicate")))))

(ert-deftest runtime/input-core-native-consumer-condition ()
  "Deferred real native scheduler receivers preserve original error and quit."
  (dolist (kind '(error quit))
    (superchat-runtime-test--input-fixture
     (let (question capability (received 0))
       (superchat-register-tool
        "runtime_release_ask" :description "Controlled question" :args nil :async t
        :effects '((:effect local-read :resource "input-core"))
        :function (lambda (_done)
                    (setq capability (plist-get superchat--tool-execution-context :input-settlement))
                    (superchat-runtime--request-input capability '(:question "City?"))))
       (let* ((config (list :enabled t :tool-name "runtime_release_ask"
                            :presenter (lambda (value) (setq question value))))
              (run (plist-get (superchat-runtime-test--input-submit "input" config) :run-id))
              (call (copy-sequence (car superchat-runtime-test--release-calls))))
         ;; Only replace the external receiver supplied to the real native
         ;; scheduler; its tool end-func, Authority and receipt remain real.
         (setf (plist-get call :success)
               (lambda (_value) (cl-incf received) (signal kind '("native consumer" original))))
         (superchat-runtime-test--release-tool-response call)
         (let ((condition (condition-case err
                              (progn (superchat-runtime-answer-input run (plist-get question :handle) "answer") nil)
                            ((error quit) err))))
           (should (equal condition (list kind "native consumer" 'original))))
         (should (= received 1))
         (should (eq 'raised (plist-get (funcall capability 'snapshot) :delivery)))
         (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.completed")))
         (should (eq 'running-model (superchat-runtime-test--release-status run)))
         (should-not (superchat-runtime-answer-input run (plist-get question :handle) "duplicate"))
         (should (superchat-runtime-cancel run))
         (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "input" run) :verdict))))))))

(ert-deftest runtime/input-core-consumer-stop-and-cleanup-condition ()
  "Stop from an entered native receiver cannot mask its primary condition."
  (dolist (kind '(error quit))
    (superchat-runtime-test--input-fixture
     (let (question capability run active-during-notify (notifications 0))
       (superchat-register-tool
        "runtime_release_ask" :description "Controlled question" :args nil :async t
        :effects '((:effect local-read :resource "input-core"))
        :function (lambda (_done)
                    (setq capability (plist-get superchat--tool-execution-context :input-settlement))
                    (superchat-runtime--request-input capability '(:question "City?"))))
       (setq run
             (plist-get
              (superchat-runtime-test--input-submit
               "input"
               (list :enabled t :tool-name "runtime_release_ask"
                     :presenter (lambda (value) (setq question value))
                     :stop-consumer
                     (lambda (_)
                       (cl-incf notifications)
                       (setq active-during-notify (plist-get (superchat-runtime-status run) :control-active-p))
                       (signal kind '("secondary cleanup"))))) :run-id))
       (let ((call (copy-sequence (car superchat-runtime-test--release-calls))))
         (setf (plist-get call :success)
               (lambda (_)
                 (should (superchat-runtime-cancel run))
                 (signal kind '("primary consumer" exact-data))))
         (superchat-runtime-test--release-tool-response call))
       (should (member run (superchat-runtime-active-runs)))
       (let ((condition (condition-case err
                            (superchat-runtime-answer-input run (plist-get question :handle) "answer")
                          ((error quit) err))))
         (should (equal condition (list kind "primary consumer" 'exact-data))))
       (should (= notifications 1))
       (should active-during-notify)
       (should-not (plist-get (superchat-runtime-status run) :control-active-p))
       (should (eq 'cancelled (superchat-runtime-test--release-status run)))
       (should (eq 'committed (plist-get (funcall capability 'snapshot) :receipt-state)))
       (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.completed")))
       (should (= 0 (superchat-runtime-test--release-count-event "input" "tool.failed")))
       (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "input" run) :verdict)))))))

(ert-deftest runtime/input-core-captures-tool-name-value ()
  "Mutating the caller's config string cannot change accepted eligibility."
  (superchat-runtime-test--input-fixture
   (let (question)
     (superchat-register-tool
      "runtime_release_ask" :description "Controlled question" :args nil :async t
      :effects '((:effect local-read :resource "input-core"))
      :function (lambda (_)
                  (superchat-runtime--request-input
                   (plist-get superchat--tool-execution-context :input-settlement)
                   '(:question "City?"))))
     (let* ((config (list :enabled t :tool-name (copy-sequence "runtime_release_ask")
                          :presenter (lambda (value) (setq question value))))
            (run (plist-get (superchat-runtime-test--input-submit "input" config) :run-id)))
       (aset (plist-get config :tool-name) 0 ?X)
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
       (should question)
       (should (superchat-runtime-cancel run))
       (should (= 1 (superchat-runtime-test--release-count-event "input" "tool.failed")))))))

(ert-deftest runtime/input-core-question-snapshot-detachment ()
  "Caller, presenter and query data cannot mutate the retained question."
  (dolist (source '(caller presenter query))
    (superchat-runtime-test--input-fixture
     (let* ((question (list :question (copy-sequence "City?")
                            :choices (vector (list (copy-sequence "Shenzhen")
                                                   (vector (copy-sequence "region"))))
                            :flags (bool-vector t nil)))
            (expected (prin1-to-string question)) presented capability)
       (superchat-register-tool
        "runtime_release_ask" :description "Controlled question" :args nil :async t
        :effects '((:effect local-read :resource "input-core"))
        :function (lambda (_)
                    (setq capability (plist-get superchat--tool-execution-context :input-settlement))
                    (superchat-runtime--request-input capability question)))
       (let* ((config (list :enabled t :tool-name "runtime_release_ask"
                            :presenter (lambda (value) (setq presented value))))
              (run (plist-get (superchat-runtime-test--input-submit "input" config) :run-id)))
         (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
         (let* ((untouched (superchat-runtime-pending-input run))
                (handle (plist-get untouched :handle))
                (target (pcase source ('caller question) ('presenter presented)
                                (_ (superchat-runtime-pending-input run)))))
           (should (eq handle (plist-get presented :handle)))
           (aset (plist-get target :question) 0 ?X)
           (let ((nested (aref (plist-get target :choices) 0)))
             (aset (car nested) 0 ?X)
             (aset (aref (cadr nested) 0) 0 ?X)
             (setcar nested "replacement")
             (aset (cadr nested) 0 "replacement"))
           (aset (plist-get target :choices) 0 '("replacement"))
           (aset (plist-get target :flags) 0 nil)
           (unless (eq source 'caller)
             (aset (plist-get target :run-id) 0 ?X)
             (aset (plist-get target :session-id) 0 ?X)
             (setf (plist-get target :handle) (make-symbol "stale")))
           (should (equal expected
                          (prin1-to-string
                           (plist-get (plist-get (superchat-runtime--entry run) :input-question) :question))))
           (dolist (snapshot (list untouched (superchat-runtime-pending-input run)))
             (should (equal expected
                            (prin1-to-string
                             (list :question (plist-get snapshot :question)
                                   :choices (plist-get snapshot :choices)
                                   :flags (plist-get snapshot :flags)))))
             (should (equal run (plist-get snapshot :run-id)))
             (should (equal "input" (plist-get snapshot :session-id)))
             (should (eq handle (plist-get snapshot :handle))))
           (should-not (superchat-runtime-answer-input run (make-symbol (symbol-name handle)) "stale"))
           (should (superchat-runtime-answer-input run handle "Shenzhen"))
           (should (eq 'committed (plist-get (funcall capability 'snapshot) :receipt-state)))
           (should-not (superchat-runtime-answer-input run handle "duplicate"))
           (should (= 2 (length superchat-runtime-test--release-calls)))
           (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "final")
           (should (eq 'completed (superchat-runtime-test--release-status run)))))))))

(defun superchat-runtime-test--clock-register ()
  (superchat-register-tool
   "runtime_release_ask" :description "Clock question" :args nil :async t
   :effects '((:effect local-read :resource "clock-question"))
   :function (lambda (_done)
               (superchat-runtime--request-input
                (plist-get superchat--tool-execution-context :input-settlement)
                '(:question "City?")))))
(defun superchat-runtime-test--clock-submit (presenter &optional deadline)
  (superchat-runtime-test-submit
   (list :session-id "clock" :buffer (superchat-editor-workspace-buffer "clock" 'probe t)
         :prompt "Question" :tools '("runtime_release_ask") :deadline deadline
         :stream-function #'ignore :delivery-function #'ignore
         :input-core (list :enabled t :tool-name "runtime_release_ask" :presenter presenter))))
(ert-deftest runtime/input-clock-pause-before-synchronous-presenter ()
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
     (let* (before-answer after-answer
                          (h (superchat-runtime-test--clock-submit
                              (lambda (q)
                                (let ((owner (superchat-runtime-transport-timer-owner (plist-get q :run-id))))
                                  (setq before-answer
                                        (list (superchat-llm--transport-timer-owner-wait-timer owner)
                                              (superchat-llm--transport-timer-owner-hard-timer owner)))
                                  (superchat-runtime-answer-input (plist-get q :run-id) (plist-get q :handle) "City")
                                  (setq after-answer
                                        (list (superchat-llm--transport-timer-owner-wait-timer owner)
                                              (superchat-llm--transport-timer-owner-hard-timer owner)))))))
                          (run (plist-get h :run-id)))
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
       (unwind-protect
           (progn
             (should (= 2 (length superchat-runtime-test--release-calls)))
             (should (equal '(nil nil) before-answer))
             (should (cl-every #'timerp after-answer)))
         (superchat-runtime-cancel run))))))
(ert-deftest runtime/input-continuation-delivery-enum-boundary ()
  "Pure record boundary, not an E2E fixture: returned/raised prove entry."
  (dolist (delivery '(not-started entered returned raised))
    (dolist (claim '(nil answer failed))
      (let ((entry (list :input-question
                         (list :capability
                               (lambda (operation)
                                 (should (eq operation 'snapshot))
                                 (list :delivery delivery :claim claim))))))
        (should (eq (superchat-runtime--input-blocked-reason entry)
                    (when (eq delivery 'not-started)
                      (if (eq claim 'answer) 'answer-not-delivered 'unanswered))))))))

(ert-deftest runtime/input-continuation-public-only-at-terminal ()
  "A private intent latch is not a running permission switch."
  (dolist (state '(running-model completed failed timed-out cancelled audit-incomplete))
    (let ((status (superchat-runtime--public-status
                   (list :status state :input-continuation-blocked 'unanswered))))
      (should (eq (plist-get status :input-continuation-blocked)
                  (unless (eq state 'running-model) 'unanswered))))))

(ert-deftest runtime/input-clock-postreceipt-expiry-no-native-delivery ()
  (superchat-runtime-test--input-fixture
   (superchat-runtime-test--clock-register)
   (let* (question (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q)) 60))
                   (run (plist-get h :run-id))
                   (append-event (symbol-function 'superchat-tape-append-event-record))
                   (aggregate (symbol-function 'llm-provider-utils-populate-tool-uses))
                   (aggregate-count 0))
     (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
     (cl-letf (((symbol-function 'llm-provider-utils-populate-tool-uses)
                (lambda (&rest args)
                  (cl-incf aggregate-count) (apply aggregate args)))
               ((symbol-function 'superchat-tape-append-event-record)
                (lambda (event)
                  (prog1 (funcall append-event event)
                    (when (equal "tool.completed" (plist-get event :type))
                      (superchat-runtime--update run :deadline (- (float-time) 1)))))))
       (superchat-runtime-answer-input run (plist-get question :handle) "City"))
     (unwind-protect
         (progn
           (should (= 0 aggregate-count))
           (should (= 1 (superchat-runtime-test--release-count-event "clock" "tool.completed")))
           (should (= 0 (superchat-runtime-test--release-count-event "clock" "tool.failed")))
           (should (= 1 (length superchat-runtime-test--release-calls)))
           (should (eq 'timed-out (superchat-runtime-test--release-status run))))
       (superchat-runtime-cancel run)))))
(ert-deftest runtime/input-clock-old-wait-callback-cannot-expire-h2 ()
  (superchat-runtime-test--input-fixture
   (superchat-runtime-test--clock-register)
   (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
   (let* (question (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
                   (run (plist-get h :run-id)) (owner (superchat-runtime-transport-timer-owner run))
                   (old (superchat-llm--transport-timer-owner-wait-timer owner))
                   (old-function (timer--function old)) (old-args (timer--args old)))
     (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
     (superchat-runtime-answer-input run (plist-get question :handle) "City")
     (let ((new (superchat-llm--transport-timer-owner-wait-timer owner)))
       ;; Adversarial stale closure, not a claim of naturally queued delivery.
       (apply old-function old-args)
       (unwind-protect
           (progn
             (should (eq 'running-model (superchat-runtime-test--release-status run)))
             (should (timerp new))
             (should-not (eq old new))
             (should (eq new (superchat-llm--transport-timer-owner-wait-timer owner))))
         (superchat-runtime-cancel run))))))

(ert-deftest runtime/input-clock-elapsed-human-time-and-residual ()
  "Human time exceeds the entire cap; only the old residual funds H2."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 1 superchat-response-timeout-hard-limit 1)
     (let* (question (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q)) 5))
                     (run (plist-get h :run-id)) (owner (superchat-runtime-transport-timer-owner run)))
       (superchat-runtime-test--release-wait .15)
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
       (let ((remaining (superchat-llm--transport-timer-owner-hard-remaining owner)))
         (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner)))
         (should (< remaining .95)) (should (> remaining .5))
         (superchat-runtime-test--release-wait 1.1)
         (should (eq 'running-model (superchat-runtime-test--release-status run)))
         (should (= remaining (superchat-llm--transport-timer-owner-hard-remaining owner)))
         (let ((started (float-time)))
           (superchat-runtime-answer-input run (plist-get question :handle) "City")
           (should (= 2 (length superchat-runtime-test--release-calls)))
           (should (<= (- (superchat-llm--transport-timer-owner-hard-deadline owner)
                          (float-time)) remaining))
           ;; Isolate the hard residual from its independent wait watchdog.
           (superchat-llm--transport-timer-owner-cancel-wait owner)
           (superchat-runtime-test--release-until
            (lambda () (eq 'timed-out (superchat-runtime-test--release-status run))) 1.5)
           (should (>= (- (float-time) started) (- remaining .05)))
           (should (< (- (float-time) started) (+ remaining .2)))))
       (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
       (should (= 1 (superchat-runtime-test--release-count-event "clock" "tool.completed")))
       (should (= 0 (superchat-runtime-test--release-count-event "clock" "tool.failed")))))))

(ert-deftest runtime/input-clock-same-stack-q1-q2 ()
  "Q1 presenter returns after H2 publishes Q2; it cannot repause Q1."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
     (let (q1 q2 owner q2-epoch q2-remaining observations)
       (setq superchat-runtime-test--release-behavior
             (lambda (call)
               (when (= 2 (length superchat-runtime-test--release-calls))
                 (superchat-runtime-test--release-tool-response call))))
       (let* ((h (superchat-runtime-test--clock-submit
                  (lambda (q)
                    (setq owner (superchat-runtime-transport-timer-owner (plist-get q :run-id)))
                    (push (list (superchat-llm--transport-timer-owner-hard-state owner)
                                (superchat-llm--transport-timer-owner-wait-timer owner)) observations)
                    (if q1
                        (setq q2 q
                              q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)
                              q2-remaining (superchat-llm--transport-timer-owner-hard-remaining owner))
                      (setq q1 q)
                      (superchat-runtime-answer-input (plist-get q :run-id) (plist-get q :handle) "Q1")))))
              (run (plist-get h :run-id)))
         (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
         (unwind-protect
             (progn
               (should q2) (should (equal observations '((paused nil) (paused nil))))
               (should (eq (plist-get q2 :handle) (superchat-llm--transport-timer-owner-input-handle owner)))
               (should (eq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
               (should (= q2-remaining (superchat-llm--transport-timer-owner-hard-remaining owner)))
               (should-not (superchat-runtime-answer-input run (plist-get q1 :handle) "stale"))
               (should (eq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
               (superchat-runtime-answer-input run (plist-get q2 :handle) "Q2")
               (should (= 3 (length superchat-runtime-test--release-calls)))
               (should (<= (- (superchat-llm--transport-timer-owner-hard-deadline owner)
                              (float-time)) q2-remaining)))
           (superchat-runtime-cancel run)))))))

(ert-deftest runtime/input-clock-already-entered-h1-output ()
  "H1 reenters H2 from its phase observer, then reaches old cancel-wait."
  (let ((superchat-runtime-test--release-mode 'streaming))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
     (let* ((h (superchat-runtime-test--clock-submit
                (lambda (q) (superchat-runtime-answer-input
                             (plist-get q :run-id) (plist-get q :handle) "City"))))
            (run (plist-get h :run-id)) (call (car superchat-runtime-test--release-calls))
            (owner (superchat-runtime-transport-timer-owner run))
            (update (symbol-function 'superchat-runtime--update)) new-wait triggered)
       (cl-letf (((symbol-function 'superchat-runtime--update)
                  (lambda (id &rest properties)
                    (prog1 (apply update id properties)
                      (when (and (equal id run) (not triggered)
                                 (eq 'answer-streaming (plist-get properties :model-phase)))
                        (setq triggered t)
                        (superchat-runtime-test--release-tool-response call)
                        (setq new-wait (superchat-llm--transport-timer-owner-wait-timer owner)))))))
         (funcall (plist-get call :partial) "old H1 output"))
       (unwind-protect
           (progn
             (should triggered) (should (= 2 (length superchat-runtime-test--release-calls)))
             (should (timerp new-wait)) (should (memq new-wait timer-list))
             (should (eq new-wait (superchat-llm--transport-timer-owner-wait-timer owner)))
             (funcall (plist-get (car superchat-runtime-test--release-calls) :partial) "new H2")
             (should-not (superchat-llm--transport-timer-owner-wait-timer owner)))
         (superchat-runtime-cancel run))))))

(ert-deftest runtime/input-clock-disabled-deadline-and-zero ()
  (dolist (kind '(disabled deadline zero acceptance))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout (unless (eq kind 'disabled) 2))
     (let* (question (h (superchat-runtime-test--clock-submit
                         (lambda (q) (setq question q)) (and (eq kind 'deadline) .12)))
                     (run (plist-get h :run-id)) (owner (superchat-runtime-transport-timer-owner run)))
       (when (eq kind 'acceptance)
         (superchat-runtime--update run :deadline (- (float-time) 1)))
       (when (eq kind 'zero)
         (setf (superchat-llm--transport-timer-owner-hard-deadline owner) (- (float-time) 1)))
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
       (if (eq kind 'disabled)
           (progn
             (should (eq 'disabled (superchat-llm--transport-timer-owner-hard-state owner)))
             (superchat-runtime-answer-input run (plist-get question :handle) "City")
             (should-not (superchat-llm--transport-timer-owner-hard-timer owner))
             (should-not (superchat-llm--transport-timer-owner-wait-timer owner))
             (superchat-runtime-cancel run))
         (when (eq kind 'deadline)
           (superchat-runtime-test--release-until
            (lambda () (eq 'timed-out (superchat-runtime-test--release-status run))) .5))
         (should (eq 'timed-out (superchat-runtime-test--release-status run)))
         (should (= 1 (length superchat-runtime-test--release-calls)))
         (should (= 1 (superchat-runtime-test--release-count-event "clock" "tool.failed"))))
       (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))))))

(ert-deftest runtime/input-clock-acquisition-rollback ()
  "Pre-native activation failure charges time; Q2 and closed beat old cleanup."
  (dolist (kind '(error quit cancel q2 deadline))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
     (let* (question q2-epoch injected observed
                     (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
                     (run (plist-get h :run-id)) (call (car superchat-runtime-test--release-calls))
                     (owner (superchat-runtime-transport-timer-owner run))
                     (activate (symbol-function 'timer-activate)))
       (superchat-runtime-test--release-tool-response call)
       (let ((remaining (superchat-llm--transport-timer-owner-hard-remaining owner))
             (handle (plist-get question :handle)))
         (cl-letf (((symbol-function 'timer-activate)
                    (lambda (timer &rest args)
                      (prog1 (apply activate timer args)
                        (when (and (not injected)
                                   (eq timer (superchat-llm--transport-timer-owner-hard-timer owner))
                                   (eq 'running (superchat-llm--transport-timer-owner-hard-state owner)))
                          (setq injected t)
                          (superchat-runtime-test--release-wait .03)
                          (pcase kind
                            ('deadline (superchat-runtime--update run :deadline (- (float-time) 1)))
                            ('cancel (superchat-runtime-cancel run))
                            ('q2
                             ;; Force real native tool execution during activation:
                             ;; adversarial reentry, not a natural provider event.
                             (superchat-runtime-test--release-tool-response call)
                             (setq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
                            (_ (signal kind '("activation original")))))))))
           (condition-case condition
               (superchat-runtime-answer-input run handle "City")
             ((error quit) (setq observed condition))))
         (unwind-protect
             (progn
               (should injected)
               (should (= 1 (length superchat-runtime-test--release-calls)))
               (should-not (superchat-llm--transport-timer-owner-hard-timer owner))
               (should-not (superchat-llm--transport-timer-owner-wait-timer owner))
               (pcase kind
                 ('deadline
                  (should (eq 'timed-out (superchat-runtime-test--release-status run)))
                  (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner))))
                 ('cancel (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner))))
                 ('q2
                  (should q2-epoch)
                  (should (eq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
                  (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner)))
                  (should (superchat-runtime-pending-input run)))
                 ('error
                  (should-not observed)
                  (should (eq 'failed (superchat-runtime-test--release-status run)))
                  (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
                  (should (< (superchat-llm--transport-timer-owner-hard-remaining owner)
                             (- remaining .02))))
                 (_
                  (should (equal observed (list kind "activation original")))
                  (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner)))
                  (should (< (superchat-llm--transport-timer-owner-hard-remaining owner)
                             (- remaining .02)))))
               (should (= 1 (superchat-runtime-test--release-count-event "clock" "tool.completed"))))
           (superchat-runtime-cancel run)))))))

(ert-deftest runtime/input-clock-audit-priority-after-expiry ()
  (dolist (committed '(nil t))
    (superchat-runtime-test--input-fixture
     (superchat-runtime-test--clock-register)
     (setq superchat-response-timeout 2)
     (let* (question (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
                     (run (plist-get h :run-id)) (owner (superchat-runtime-transport-timer-owner run))
                     (append-event (symbol-function 'superchat-tape-append-event-record)) observed)
       (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
       (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                  (lambda (event)
                    (if (equal "tool.completed" (plist-get event :type))
                        (progn
                          (when committed (funcall append-event event))
                          (superchat-runtime--update run :deadline (- (float-time) 1))
                          (error "receipt audit failure"))
                      (funcall append-event event)))))
         (condition-case condition
             (superchat-runtime-answer-input run (plist-get question :handle) "City")
           (error (setq observed condition))))
       (should (equal observed '(error "receipt audit failure")))
       (should (eq 'audit-incomplete (superchat-runtime-test--release-status run)))
       (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
       (should (= 1 (length superchat-runtime-test--release-calls)))
       (should (= (if committed 1 0)
                  (superchat-runtime-test--release-count-event "clock" "tool.completed")))
       (should (= 0 (superchat-runtime-test--release-count-event "clock" "tool.failed")))))))

(ert-deftest runtime/input-clock-same-buffer-a-b ()
  (superchat-runtime-test--input-fixture
   (superchat-runtime-test--clock-register)
   (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
   (let* (question (a (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
                   (ra (plist-get a :run-id)) (call-a (car superchat-runtime-test--release-calls))
                   (host (plist-get (superchat-runtime--entry ra) :buffer))
                   (b (superchat-runtime-test--release-submit "clock" 5 host))
                   (rb (plist-get b :run-id)) (ob (superchat-runtime-transport-timer-owner rb))
                   (wait-b (superchat-llm--transport-timer-owner-wait-timer ob))
                   (hard-b (superchat-llm--transport-timer-owner-hard-timer ob)))
     (superchat-runtime-test--release-tool-response call-a)
     (superchat-runtime-answer-input ra (plist-get question :handle) "City")
     (superchat-runtime-cancel ra)
     (unwind-protect
         (progn
           (should (eq wait-b (superchat-llm--transport-timer-owner-wait-timer ob)))
           (should (eq hard-b (superchat-llm--transport-timer-owner-hard-timer ob)))
           (should (memq wait-b timer-list)) (should (memq hard-b timer-list))
           (should (eq 'running-model (superchat-runtime-test--release-status rb))))
       (superchat-runtime-cancel rb)))))

(ert-deftest runtime/input-clock-reentry-before-activation ()
  "A timer enqueued after reentrant Q2/close must still be released exactly."
  (dolist (slot '(hard wait))
    (dolist (kind '(cancel q2))
      (superchat-runtime-test--input-fixture
       (superchat-runtime-test--clock-register)
       (setq superchat-response-timeout 2 superchat-response-timeout-hard-limit 5)
       (let* (question stale injected q2-epoch
                       (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
                       (run (plist-get h :run-id)) (call (car superchat-runtime-test--release-calls))
                       (owner (superchat-runtime-transport-timer-owner run))
                       (activate (symbol-function 'timer-activate)))
         (superchat-runtime-test--release-tool-response call)
         (let ((q1 (plist-get question :handle)))
           (cl-letf (((symbol-function 'timer-activate)
                      (lambda (timer &rest args)
                        (when (and (not injected)
                                   (eq timer
                                       (if (eq slot 'hard)
                                           (superchat-llm--transport-timer-owner-hard-timer owner)
                                         (superchat-llm--transport-timer-owner-wait-timer owner))))
                          (setq injected t stale timer)
                          (if (eq kind 'cancel)
                              (superchat-runtime-cancel run)
                            (superchat-runtime-test--release-tool-response call)
                            (setq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner))))
                        ;; The stale timer is only enqueued AFTER reentry.
                        (apply activate timer args))))
             (superchat-runtime-answer-input run q1 "City")))
         (unwind-protect
             (progn
               (should injected) (should-not (memq stale timer-list))
               (should (= 1 (length superchat-runtime-test--release-calls)))
               (if (eq kind 'cancel)
                   (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
                 (should q2-epoch)
                 (should (eq q2-epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
                 (should (superchat-runtime-pending-input run))))
           (superchat-runtime-cancel run)))))))

(ert-deftest runtime/input-clock-zero-resume-keeps-answer-receipt ()
  "An exhausted paused budget cannot dispatch or settle the answer twice."
  (superchat-runtime-test--input-fixture
    (superchat-runtime-test--clock-register)
    (setq superchat-response-timeout 2)
    (let* (question (h (superchat-runtime-test--clock-submit (lambda (q) (setq question q))))
           (run (plist-get h :run-id)) (owner (superchat-runtime-transport-timer-owner run)))
      (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
      (setf (superchat-llm--transport-timer-owner-hard-remaining owner) 0)
      (superchat-runtime-answer-input run (plist-get question :handle) "City")
      (should (eq 'timed-out (superchat-runtime-test--release-status run)))
      (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
      (should (= 1 (length superchat-runtime-test--release-calls)))
      (should (= 1 (superchat-runtime-test--release-count-event "clock" "tool.completed")))
      (should (= 0 (superchat-runtime-test--release-count-event "clock" "tool.failed"))))))

(defun superchat-runtime-test--owner-request (accepted stopped &optional delivery)
  "Prepare an owner-only Request; no input capability or presenter."
  (list :session-id "owner" :buffer (superchat-editor-workspace-buffer "owner" 'release-protocol t)
        :prompt "Owner request" :tools '("runtime_release_ask") :deadline nil
        :stream-function #'ignore :delivery-function (or delivery #'ignore)
        :accepted-function accepted :owner-stop-function stopped))

(defmacro superchat-runtime-test--owner-invalid (key)
  `(superchat-runtime-test--input-fixture
   (let ((request (superchat-runtime-test--owner-request #'ignore #'ignore))
         (admit (symbol-function 'superchat-runtime--validate-host-admission)) admitted)
     (plist-put request ,key 42)
     (cl-letf (((symbol-function 'superchat-runtime--validate-host-admission)
                (lambda (&rest args) (setq admitted t) (apply admit args))))
       (should-error (superchat-runtime-test-submit request)))
     (should-not admitted)
     (should (= 0 (hash-table-count superchat-runtime--runs)))
     (should-not (superchat-tape-replay-events "owner"))
     (should-not superchat-runtime-test--release-calls))))

(ert-deftest runtime/owner-invalid-accepted-function ()
  (superchat-runtime-test--owner-invalid :accepted-function))
(ert-deftest runtime/owner-invalid-stop-function ()
  (superchat-runtime-test--owner-invalid :owner-stop-function))

(ert-deftest runtime/owner-accepted-before-native-nil-return ()
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (let (accepted count-at-accept events-at-accept)
       (let* ((h (superchat-runtime-test-submit
                  (superchat-runtime-test--owner-request
                   (lambda (handle)
                     (setq accepted handle count-at-accept (length superchat-runtime-test--release-calls)
                           events-at-accept (superchat-tape-replay-events "owner"))
                     nil)
                   #'ignore)))
              (run (plist-get h :run-id))
              (entry (superchat-runtime--entry run)))
         (should (equal run (plist-get accepted :run-id)))
         (should (= 0 count-at-accept))
         (should (cl-find "run.started" events-at-accept :key (lambda (e) (plist-get e :type)) :test #'equal))
         (should (= 1 (length superchat-runtime-test--release-calls)))
         (should-not (plist-get entry :input-core))
         (should-not (plist-get entry :input-core-token))
         (should-not (superchat-llm--transport-timer-owner-input-clocks-p
                      (superchat-runtime-transport-timer-owner run)))
         (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "Done")
         (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict))))))))

(defmacro superchat-runtime-test--owner-startup-stop (mode)
  `(let ((superchat-runtime-test--release-mode ,mode))
    (superchat-runtime-test--input-fixture
     (let ((append-event (symbol-function 'superchat-runtime--append-event))
           injected stop-result (notifications 0) (acceptances 0))
       (cl-letf (((symbol-function 'superchat-runtime--append-event)
                  (lambda (entry type &rest args)
                    (prog1 (apply append-event entry type args)
                      (when (and (equal type "run.started") (not injected))
                        (setq injected t stop-result
                              (superchat-runtime-cancel (plist-get entry :run-id) "startup stop")))))))
         (let* ((h (superchat-runtime-test-submit
                    (superchat-runtime-test--owner-request
                     (lambda (_) (cl-incf acceptances)) (lambda (_) (cl-incf notifications)))))
                (run (plist-get h :run-id)))
           (should stop-result)
           (should-not superchat-runtime-test--release-calls)
           (should (= 1 notifications))
           (should (= 0 acceptances))
           (should (plist-get (superchat-runtime-status run) :stop-requested))
           (should (eq 'cancelled (superchat-runtime-test--release-status run)))
           (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict)))))))))

(ert-deftest runtime/owner-startup-stop-async () (superchat-runtime-test--owner-startup-stop 'async))
(ert-deftest runtime/owner-startup-stop-streaming () (superchat-runtime-test--owner-startup-stop 'streaming))

(ert-deftest runtime/owner-stop-inside-completed-delivery ()
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (let (run order accepted)
       (setq run
             (plist-get (superchat-runtime-test-submit
                         (superchat-runtime-test--owner-request
                          (lambda (h) (setq run (plist-get h :run-id)))
                          (lambda (_) (push 'stop order))
                          (lambda (&rest _)
                            (push 'enter order)
                            (setq accepted (superchat-runtime-cancel run))
                            (push 'after order)))) :run-id))
       (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "Done")
       (should accepted)
       (should (equal '(enter stop after) (reverse order)))
       (should (= 1 (superchat-runtime-test--release-count-event "owner" "run.completed")))
       (should (= 0 (superchat-runtime-test--release-count-event "owner" "run.cancelled")))
       (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict)))
       (should-not (superchat-runtime-cancel run))))))

(ert-deftest runtime/owner-callbacks-captured-before-host-admission ()
  (superchat-runtime-test--input-fixture
   (let (accepted stopped request
         (admit (symbol-function 'superchat-runtime--validate-host-admission)))
     (setq request (superchat-runtime-test--owner-request
                    (lambda (h) (setq accepted h)) (lambda (_) (setq stopped t))))
     (cl-letf (((symbol-function 'superchat-runtime--validate-host-admission)
                (lambda (value)
                  (plist-put request :accepted-function #'ignore)
                  (plist-put request :owner-stop-function #'ignore)
                  (funcall admit value))))
       (superchat-runtime-test-submit request))
     (should accepted)
     (should (superchat-runtime-cancel (plist-get accepted :run-id)))
     (should stopped))))

(defmacro superchat-runtime-test--owner-accepted-exit (exit)
  `(dolist (superchat-runtime-test--release-mode '(async streaming))
     (superchat-runtime-test--input-fixture
      (let (accepted caught (notifications 0))
        (condition-case condition
            (superchat-runtime-test-submit
             (superchat-runtime-test--owner-request
              (lambda (h)
                (setq accepted h)
                (if (eq ,exit 'stop)
                    (progn (superchat-runtime-cancel (plist-get h :run-id)) nil)
                  (signal ,exit '(owner-primary))))
              (lambda (_) (cl-incf notifications))))
          ((error quit) (setq caught condition)))
        (should accepted)
        (should-not superchat-runtime-test--release-calls)
        (if (eq ,exit 'stop)
            (progn (should-not caught) (should (= 1 notifications)))
          (should (equal caught (list ,exit 'owner-primary))))
        (should (eq (if (eq ,exit 'stop) 'cancelled 'failed)
                    (superchat-runtime-test--release-status (plist-get accepted :run-id))))
        (should-not (superchat-runtime-cancel (plist-get accepted :run-id)))))))

(ert-deftest runtime/owner-accepted-stop () (superchat-runtime-test--owner-accepted-exit 'stop))
(ert-deftest runtime/owner-accepted-error () (superchat-runtime-test--owner-accepted-exit 'error))
(ert-deftest runtime/owner-accepted-quit () (superchat-runtime-test--owner-accepted-exit 'quit))

(ert-deftest runtime/owner-startup-audit-commit-and-condition ()
  "Failed startup never invents acceptance or reuses its deferred cancel seal."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (type '("intent.received" "run.started"))
      (dolist (committed '(nil t))
        (dolist (condition '(error quit))
          (superchat-runtime-test--input-fixture
           (let ((append-event (symbol-function 'superchat-runtime--append-event))
                 injected caught run accepted stopped)
             (cl-letf (((symbol-function 'superchat-runtime--append-event)
                        (lambda (entry event &rest args)
                          (if (or injected (not (equal type event)))
                              (apply append-event entry event args)
                            (setq injected t run (plist-get entry :run-id))
                            (when committed (apply append-event entry event args))
                            (setq stopped (superchat-runtime-cancel run "startup audit stop"))
                            (signal condition '(startup-primary))))))
               (condition-case err
                   (superchat-runtime-test-submit
                    (superchat-runtime-test--owner-request
                     (lambda (_) (setq accepted t)) #'ignore))
                 ((error quit) (setq caught err))))
             (should stopped)
             (should (equal caught (list condition 'startup-primary)))
             (should-not accepted)
             (should-not superchat-runtime-test--release-calls)
             (should (eq 'audit-incomplete (superchat-runtime-test--release-status run)))
             (should (plist-get (superchat-runtime-status run) :stop-requested))
             (should-not (plist-get (superchat-runtime-status run) :control-active-p))
             (should (= (if committed 1 0)
                        (superchat-runtime-test--release-count-event "owner" type)))
             (when (equal type "intent.received")
               (should (= 0 (superchat-runtime-test--release-count-event "owner" "run.started"))))
             (should (= 0 (superchat-runtime-test--release-count-event "owner" "run.cancelled")))
             (message "OWNER STARTUP AUDIT mode=%s event=%s committed=%s condition=%s tape=%S closure=%S"
                      superchat-runtime-test--release-mode type committed condition
                      (mapcar (lambda (e) (plist-get e :type)) (superchat-tape-replay-events "owner"))
                      (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict)))))))))

(ert-deftest runtime/owner-deadline-at-acceptance-and-predispatch ()
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (dolist (cut '(accepted predispatch))
      (superchat-runtime-test--input-fixture
       (let* (run injected
                  (response-mode (symbol-function 'superchat-llm--response-mode))
                  (request (superchat-runtime-test--owner-request
                            (lambda (h)
                              (setq run (plist-get h :run-id))
                              (when (eq cut 'accepted) (sleep-for 0.12))) #'ignore)))
         (plist-put request :deadline 0.1)
         (cl-letf (((symbol-function 'superchat-llm--response-mode)
                    (lambda (&rest args)
                      (when (and (eq cut 'predispatch) (not injected))
                        (setq injected t)
                        ;; Isolate the synchronous check rather than letting
                        ;; the ordinary deadline timer prove this assertion.
                        (cancel-timer (plist-get (superchat-runtime--entry run) :deadline-timer))
                        (sleep-for 0.12))
                      (apply response-mode args))))
           (superchat-runtime-test-submit request))
         (should run)
         (should-not superchat-runtime-test--release-calls)
         (should (eq 'timed-out (superchat-runtime-test--release-status run)))
         (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict)))
         (should-not (superchat-runtime-cancel run)))))))

(ert-deftest runtime/owner-stop-once-before-abort-and-through-hooks ()
  (dolist (notification-condition '(nil error quit))
    (superchat-runtime-test--input-fixture
     (let* (run order accepted active-in-hook stopped-at-abort
               (superchat-turn-completed-functions
                (list (lambda (status)
                        (setq active-in-hook (plist-get (superchat-runtime-status (plist-get status :run-id)) :control-active-p))))))
       (setq run (plist-get
                  (superchat-runtime-test-submit
                   (superchat-runtime-test--owner-request
                    #'ignore
                    (lambda (snapshot)
                      (push 'notify order)
                      (setq accepted (superchat-runtime-cancel (plist-get snapshot :run-id)))
                      (when notification-condition (signal notification-condition '(owner-cleanup)))))) :run-id))
       (setq superchat-runtime-test--release-cancel-behavior
             (lambda (_)
               (push 'abort order)
               (setq stopped-at-abort (plist-get (superchat-runtime-status run) :stop-requested))))
       (should (superchat-runtime-cancel run))
       (should accepted)
       (should stopped-at-abort)
       (should active-in-hook)
       (should (equal '(notify abort) (reverse order)))
       (should (equal (plist-get (superchat-runtime-status run) :owner-stop-error)
                      (and notification-condition (list notification-condition 'owner-cleanup))))
       (should-not (superchat-runtime-cancel run))))))

(ert-deftest runtime/owner-does-not-settle-ordinary-async-tool ()
  (superchat-runtime-test--input-fixture
   (let* ((h (superchat-runtime-test-submit (superchat-runtime-test--owner-request nil #'ignore)))
          (run (plist-get h :run-id)))
     (superchat-runtime-test--release-tool-response (car superchat-runtime-test--release-calls))
     (should (= 1 (length superchat-runtime-test--release-pending)))
     (should-not (plist-get (plist-get (car superchat-runtime-test--release-pending) :context) :input-settlement))
     (should (superchat-runtime-cancel run))
     (should (= 0 (superchat-runtime-test--release-count-event "owner" "tool.failed")))
     (should (= 0 (superchat-runtime-test--release-count-event "owner" "tool.completed")))
     (should (eq 'audit-incomplete (superchat-runtime-test--release-status run)))
     (superchat-runtime-test--release-answer (car superchat-runtime-test--release-pending) "Late ordinary result")
     (should (= 1 (superchat-runtime-test--release-count-event "owner" "tool.completed")))
     (should (= 1 (length superchat-runtime-test--release-calls))))))

(ert-deftest runtime/owner-primary-survives-secondary-audit-condition ()
  (dolist (primary '(error quit))
    (dolist (secondary '(error quit))
      (superchat-runtime-test--input-fixture
       (let ((append-event (symbol-function 'superchat-runtime--append-event))
             run caught injected)
         (cl-letf (((symbol-function 'superchat-runtime--append-event)
                    (lambda (entry type &rest args)
                      (if (and (equal type "run.failed") (not injected))
                          (progn (setq injected t) (signal secondary '(secondary-audit)))
                        (apply append-event entry type args)))))
           (condition-case condition
               (superchat-runtime-test-submit
                (superchat-runtime-test--owner-request
                 (lambda (h) (setq run (plist-get h :run-id)) (signal primary '(primary-acceptance)))
                 #'ignore))
             ((error quit) (setq caught condition))))
         (should injected)
         (should (equal caught (list primary 'primary-acceptance)))
         (should-not superchat-runtime-test--release-calls)
         (should (eq 'audit-incomplete (superchat-runtime-test--release-status run)))
         (should-not (plist-get (superchat-runtime-status run) :control-active-p))
         (should (plist-get (superchat-runtime--entry run) :quiesced))
         (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict))))))))

(ert-deftest runtime/owner-stop-after-completed-seal-before-delivery ()
  (superchat-runtime-test--input-fixture
   (let ((append-event (symbol-function 'superchat-runtime--append-event))
         run accepted delivered (notifications 0))
     (setq run (plist-get
                (superchat-runtime-test-submit
                 (superchat-runtime-test--owner-request
                  #'ignore (lambda (_) (cl-incf notifications))
                  (lambda (&rest _) (setq delivered t)))) :run-id))
     (cl-letf (((symbol-function 'superchat-runtime--append-event)
                (lambda (entry type &rest args)
                  (prog1 (apply append-event entry type args)
                    (when (equal type "run.completed")
                      (setq accepted (superchat-runtime-cancel run "sealed stop")))))))
       (funcall (plist-get (car superchat-runtime-test--release-calls) :success) "Immutable answer"))
     (should accepted)
     (should (= 1 notifications))
     (should-not delivered)
     (should (= 1 (superchat-runtime-test--release-count-event "owner" "run.completed")))
     (should (= 0 (superchat-runtime-test--release-count-event "owner" "run.cancelled")))
     (should (eq 'proved (plist-get (superchat-runtime-test--release-judge "owner" run) :verdict)))
     (should-not (superchat-runtime-cancel run)))))

(ert-deftest runtime/owner-nested-current-run-not-stale-return ()
  "Runtime seam control, not a substitute for later public Workflow E2E."
  (dolist (superchat-runtime-test--release-mode '(async streaming))
    (superchat-runtime-test--input-fixture
     (let (current stopped a b c d stop-a)
       (cl-labels
           ((stop-owner (snapshot)
              (setq stopped t)
              (when (and current (not (equal current (plist-get snapshot :run-id))))
                (superchat-runtime-cancel current "execution stopped")))
            (submit (name delivery)
              (let ((request (superchat-runtime-test--owner-request
                              (lambda (h) (setq current (plist-get h :run-id)))
                              #'stop-owner delivery)))
                (plist-put request :prompt name)
                (plist-get (superchat-runtime-test-submit request) :run-id))))
         (setq a (submit "A" (lambda (&rest _)
                               (setq b (submit "B" (lambda (&rest _)
                                                     (setq c (submit "C" #'ignore)))))
                               (setq stop-a (superchat-runtime-cancel a "ancestor stop")))))
         (setq d (plist-get (superchat-runtime-test-submit
                             (superchat-runtime-test--owner-request nil nil)) :run-id))
         (setq superchat-runtime-test--release-behavior
               (lambda (call)
                 ;; A's completed delivery submits B; B alone completes on
                 ;; its native stack and submits C before B returns.
                 (when (= 3 (length superchat-runtime-test--release-calls))
                   (funcall (plist-get call :success) "B done"))))
         (funcall (plist-get (cl-find a superchat-runtime-test--release-calls
                                     :key (lambda (call) (plist-get call :run)) :test #'equal) :success) "A done")
         (should stop-a)
         (should stopped)
         (should (equal current c))
         (should (eq 'completed (superchat-runtime-test--release-status a)))
         (should (eq 'completed (superchat-runtime-test--release-status b)))
         (should (eq 'cancelled (superchat-runtime-test--release-status c)))
         (should (eq 'running-model (superchat-runtime-test--release-status d)))
         (should (= 4 (length superchat-runtime-test--release-calls)))
         (should-not (superchat-runtime-cancel a)))))))

(ert-deftest runtime/owner-initial-publication-reentry ()
  (dolist (exit '(stop error quit))
    (superchat-runtime-test--input-fixture
     (let ((publish (symbol-function 'superchat-runtime--publish-entry))
           run caught accepted stop-result)
       (cl-letf (((symbol-function 'superchat-runtime--publish-entry)
                  (lambda (entry)
                    (prog1 (funcall publish entry)
                      (setq run (plist-get entry :run-id)
                            stop-result (superchat-runtime-cancel run "publication stop"))
                      (unless (eq exit 'stop) (signal exit '(publication-primary)))))))
         (condition-case condition
             (superchat-runtime-test-submit
              (superchat-runtime-test--owner-request
               (lambda (_) (setq accepted t)) #'ignore))
           ((error quit) (setq caught condition))))
       (should stop-result)
       (should-not accepted)
       (should-not superchat-runtime-test--release-calls)
       (should (equal caught (unless (eq exit 'stop) (list exit 'publication-primary))))
       (should (eq (if (eq exit 'stop) 'cancelled 'audit-incomplete)
                   (superchat-runtime-test--release-status run)))
       (should-not (plist-get (superchat-runtime-status run) :control-active-p))))))

(ert-deftest runtime/owner-unpublished-condition-no-fake-run ()
  (dolist (condition '(error quit))
    (superchat-runtime-test--input-fixture
     (let (caught)
       (cl-letf (((symbol-function 'superchat-runtime--publish-entry)
                  (lambda (_) (signal condition '(before-publication)))))
         (condition-case err
             (superchat-runtime-test-submit (superchat-runtime-test--owner-request #'ignore #'ignore))
           ((error quit) (setq caught err))))
       (should (equal caught (list condition 'before-publication)))
       (should (= 0 (hash-table-count superchat-runtime--runs)))
       (should-not (superchat-tape-replay-events "owner"))
       (should-not superchat-runtime-test--release-calls)))))

(ert-deftest runtime/owner-accepted-evidence-condition-boundary ()
  (dolist (condition '(error quit))
    (superchat-runtime-test--input-fixture
     (let* (accepted caught
                   (request (superchat-runtime-test--owner-request
                             (lambda (handle)
                               (setq accepted handle)
                               (signal condition '(original-evidence-acceptance))) #'ignore)))
       (plist-put request :evidence-contract "epistemic-learning-v0")
       (plist-put request :context-envelope
                  '(:contract "context-continuity-v0" :status satisfied :receipt (:tape-head 1)))
       (condition-case err (superchat-runtime-test-submit request)
         ((error quit) (setq caught err)))
       (should accepted)
       (should (equal caught (cons 'superchat-llm-evidence-recording-error
                                  (cdr superchat-runtime--evidence-audit-error))))
       (should-not superchat-runtime-test--release-calls)
       (should (eq 'failed (superchat-runtime-test--release-status (plist-get accepted :run-id))))))))

;; ═══════════════════════════════════════════════════════════
;; S09: `superchat-select-tools' is the sole decision point (contract §4.2)
;; ═══════════════════════════════════════════════════════════

(defconst test-runtime--select-tools-decider-allowlist
  '(("superchat.el" . 2)
    ("superchat-synthesis.el" . 1))
  "Non-`superchat-llm.el' production files allowed to call
`superchat-get-llm-tools' directly today, and how many call sites each
has.  Both are listing/capability-detection uses, not per-request tool
decisions: `superchat.el' uses it once for a response-mode capability
probe (`superchat--llm-response-mode') and once for a tool-count status
report; `superchat-synthesis.el' uses it to read existing tool names for
`define_tool' collision validation.  Read the call site yourself before
raising a count here; every new production decision point belongs in
`superchat-select-tools', not a fresh `superchat-get-llm-tools' call.")

(ert-deftest runtime/only-select-tools-decides-tool-names ()
  "`superchat-select-tools' is the only production decision point for a
request's tool names (contract §4.2): no production file other than
`superchat-llm.el' (the materializer, `superchat--collect-llm-tools')
calls `superchat-get-llm-tools' outside the small allowlist above of
pre-existing listing/capability-probe uses."
  (let* ((root (file-name-directory (locate-library "superchat-runtime")))
         (files (directory-files root t "\\`superchat\\(-.*\\)?\\.el\\'")))
    (should files)
    (dolist (file files)
      (let* ((base (file-name-nondirectory file))
             (allowed (cdr (assoc base test-runtime--select-tools-decider-allowlist)))
             (text (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
             (count 0)
             (start 0))
        (while (string-match "(superchat-get-llm-tools\\_>" text start)
          (setq count (1+ count) start (match-end 0)))
        (cond
         ((equal base "superchat-llm.el")) ; the materializer; any count is fine
         (allowed (should (= allowed count)))
         (t (should (= 0 count))))))))

(ert-deftest runtime/select-tools-materializer-filters-by-decision-not-caller-restriction ()
  "S09-fix-1 item 5: the static gate above only counts call sites; it
cannot see a second decider baked directly into `superchat-llm.el' itself,
or the materializer silently re-deciding with the caller's own
restriction instead of `superchat-select-tools-decision''s actual
decision -- exactly the Critical defect (a stage or explicit selection
narrower than the caller's TOOL-NAMES widened back out whenever TOOL-NAMES
was nil).  This also proves the pipeline's former second, differently-
worded decider (`superchat-pipeline--select-tools' and its stale fixed
baselines) no longer exists."
  (should-not (fboundp 'superchat-pipeline--select-tools))
  (should-not (boundp 'superchat-pipeline--chat-tool-baseline))
  (should-not (boundp 'superchat-pipeline--inline-ask-tool-baseline))
  (should-not (fboundp 'superchat-pipeline--stage-call-with-fault-p))
  (let* ((root (file-name-directory (locate-library "superchat-llm")))
         (file (expand-file-name "superchat-llm.el" root))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (should (string-match-p "(member (llm-tool-name tool) names)" text))
    (should-not (string-match-p "(member (llm-tool-name tool) tool-names)" text))))

(ert-deftest runtime/select-tools-stage-decision-reaches-provider-request ()
  "S09-fix-2 item 3 (review item 5): a behavioural gate instead of a name
scan.  The static text-match gate above only proves two literal patterns
appear/do-not-appear in the source; a reviewer mutation (renaming the
duplicate decider) satisfied it while the real defect survived (S09-fix-1
re-review, item 5).  This test instead registers a real `select-tools'
stage contribution and drives a real chat send end to end through
`journey-fixture-with' (only the provider transport replaced): the
scripted response's own captured request must carry exactly the stage's
chosen names -- not the chat surface baseline -- and the real Run status
must report `:runtime-tool-decision' `stage', proving the registered
stage's decision is what actually reached the provider, not merely what
`superchat-select-tools' claims in isolation.

The complementary \"no stage -> exactly the documented chat baseline,
reason `baseline'\" case is already covered end to end by
`journey/d14-policy-visible-small-tool-set' (`test/test-journeys-chat.el'),
which also drives a real send through `journey-fixture-with' and asserts
the same two facts (exact provider `:tools' and `:runtime-tool-decision'
`baseline') for the no-stage path; not duplicated here."
  (require 'superchat-stage)
  (let ((stage-id
         (superchat-stage-add
          'select-tools 'runtime-test-stage-narrows-to-read-buffer
          (lambda (&rest _) (list "read_buffer"))
          :owner 'runtime-test)))
    (unwind-protect
        (journey-fixture-with (:tools-policy 'always
                                :script (list (list :text "stage answer")))
          (let* ((view (journey-fixture-open-chat))
                 (run-id (journey-fixture-send "go" view)))
            (should (journey-fixture-wait-for-run view))
            (should (equal '("read_buffer")
                           (plist-get (journey-fixture-last-request) :tools)))
            (let ((status (superchat-runtime-status run-id)))
              (should (eq 'stage (plist-get status :runtime-tool-decision)))
              (should (equal '("read_buffer")
                             (plist-get status :runtime-selected-tools))))))
      (superchat-stage-remove 'select-tools stage-id))))

;;; Follow-up queue (S05a, contract §7.5; hardened by S05a-fix-1) ----------
;;
;; Per S05a-fix-1 (review findings 4/5): every test here runs inside
;; `journey-fixture-with' for a real temporary Tape store and the declared
;; llm-chat-streaming/llm-chat-async seam; nothing stubs Runtime, Authority,
;; or Tape except an explicit, narrowly-targeted `superchat-db-tape-append'
;; failure injection (rule 4b).  `journey-fixture-with' also owns cleanup:
;; every buffer it did not see at entry is killed, and every Run still in
;; `superchat-runtime--runs' is force-cancelled (which quiesces it, severing
;; its deadline timer the same way a natural finish would) before the fresh
;; per-test `superchat-runtime--runs' table itself goes out of scope -- so
;; queue tests cannot leak buffers or Run timers past their own extent.

(defmacro superchat-runtime-test--queue-fixture (&rest body)
  "Run BODY inside `journey-fixture-with' with fresh, isolated queue state."
  (declare (indent 0) (debug t))
  `(journey-fixture-with ()
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-runtime--queue-sequence 0))
       (unwind-protect
           (progn ,@body)
         (maphash (lambda (_session-id timer) (cancel-timer timer))
                  superchat-runtime--queue-timers)))))

(defun superchat-runtime-test--queue-request (session-id &optional name prompt)
  "Return a minimal submit-ready follow-up request for SESSION-ID.
Its buffer is fresh and unnamed beyond NAME; `journey-fixture-with' kills it
along with everything else it did not see at fixture entry.  PROMPT defaults
to \"follow-up\"; pass a distinct string to make a request identifiable in
`journey-fixture-requests''s captured `:prompt-text'."
  (superchat-test--sink-request
   (list :prompt (or prompt "follow-up") :session-id session-id :deadline nil
         :buffer (generate-new-buffer (or name " *queue-request*")))))

(defun superchat-runtime-test--queue-events (session-id type)
  "Return SESSION-ID's real Tape events of TYPE, oldest first."
  (seq-filter (lambda (event) (equal type (plist-get event :type)))
              (superchat-tape-replay-events session-id)))

(defun superchat-runtime-test--queue-fire (session-id)
  "Wait for and fire SESSION-ID's pending dispatch timer synchronously.
Waits rather than asserting immediately: the timer is armed by a real,
already-scheduled Run-completion callback, and this machine routinely runs
several concurrent Emacs test processes, so its exact arrival can lag a
hair behind the triggering event becoming externally observable."
  (should
   (journey-fixture-wait-until
    (lambda () (timerp (gethash session-id superchat-runtime--queue-timers)))
    6))
  (timer-event-handler (gethash session-id superchat-runtime--queue-timers)))

(defun superchat-runtime-test--queue-wait-terminal (run-id)
  "Wait up to 6s for RUN-ID to reach a terminal status."
  (should
   (journey-fixture-wait-until
    (lambda ()
      (superchat-runtime--terminal-p
       (plist-get (superchat-runtime-status run-id) :status)))
    6)))

(ert-deftest runtime/follow-up-queue-is-fifo-per-session ()
  "Enqueued follow-ups dispatch in FIFO order, independently per session."
  (superchat-runtime-test--queue-fixture
    (let* ((id-a1 (superchat-runtime-enqueue-follow-up
                   "session-a" (superchat-runtime-test--queue-request
                                "session-a" " *queue-a1*" "follow-up a1")))
           (id-b1 (superchat-runtime-enqueue-follow-up
                   "session-b" (superchat-runtime-test--queue-request
                                "session-b" " *queue-b1*" "follow-up b1")))
           (id-a2 (superchat-runtime-enqueue-follow-up
                   "session-a" (superchat-runtime-test--queue-request
                                "session-a" " *queue-a2*" "follow-up a2"))))
      (should (= 2 (superchat-runtime-pending-follow-ups "session-a")))
      (should (= 1 (superchat-runtime-pending-follow-ups "session-b")))
      (superchat-runtime--schedule-dispatch-next "session-a")
      (superchat-runtime--schedule-dispatch-next "session-b")
      ;; Drive real dispatches until both sessions' queues drain.  Each
      ;; session's own Run must terminalize and quiesce before its next
      ;; follow-up is dispatched (§7.1's one-per-quiescence rule), so this
      ;; also proves session-a's two items are never both in flight at once.
      (should
       (journey-fixture-wait-until
        (lambda () (and (= 0 (superchat-runtime-pending-follow-ups "session-a"))
                        (= 0 (superchat-runtime-pending-follow-ups "session-b"))))
        6))
      (let ((dispatched-a (superchat-runtime-test--queue-events "session-a" "queue.dispatched"))
            (dispatched-b (superchat-runtime-test--queue-events "session-b" "queue.dispatched")))
        (should (equal (list id-a1 id-a2)
                       (mapcar (lambda (e) (plist-get (plist-get e :payload) :queue-id))
                               dispatched-a)))
        (should (equal (list id-b1)
                       (mapcar (lambda (e) (plist-get (plist-get e :payload) :queue-id))
                               dispatched-b))))
      ;; The real provider itself received session-a's two requests in
      ;; enqueue order -- not merely Tape's record of the fact, but the
      ;; actual captured llm.el call order.
      (let* ((prompts (mapcar (lambda (r) (plist-get r :prompt-text))
                               (journey-fixture-requests)))
             (a1-pos (cl-position-if
                      (lambda (p) (string-match-p (regexp-quote "follow-up a1") p))
                      prompts))
             (a2-pos (cl-position-if
                      (lambda (p) (string-match-p (regexp-quote "follow-up a2") p))
                      prompts)))
        (should a1-pos)
        (should a2-pos)
        (should (< a1-pos a2-pos))))))

(ert-deftest runtime/enqueue-follow-up-tape-failure-is-rolled-back ()
  "A failed `queue.queued' append leaves no in-memory trace (§7.1 rollback)."
  (superchat-runtime-test--queue-fixture
    (let ((result
           (cl-letf (((symbol-function 'superchat-db-tape-append)
                      (lambda (&rest _) (error "tape write failed"))))
             (superchat-runtime-enqueue-follow-up
              "session-enqueue-fail"
              (superchat-runtime-test--queue-request "session-enqueue-fail")))))
      (should (equal '(:accepted nil :reason tape-failed) result))
      (should (= 0 (superchat-runtime-pending-follow-ups "session-enqueue-fail")))
      (should-not (gethash "session-enqueue-fail" superchat-runtime--session-queues))
      (should (= 0 superchat-runtime--queue-sequence))
      (should-not
       (superchat-runtime-test--queue-events "session-enqueue-fail" "queue.queued")))))

(ert-deftest runtime/clear-follow-ups-records-dropped-with-reason ()
  "Clearing a session's queue drops every item and records why."
  (superchat-runtime-test--queue-fixture
    (let ((id1 (superchat-runtime-enqueue-follow-up
                "session-clear" (superchat-runtime-test--queue-request "session-clear")))
          (id2 (superchat-runtime-enqueue-follow-up
                "session-clear" (superchat-runtime-test--queue-request "session-clear"))))
      (superchat-runtime-clear-follow-ups "session-clear" "view-closed")
      (should (= 0 (superchat-runtime-pending-follow-ups "session-clear")))
      (should-not (gethash "session-clear" superchat-runtime--session-queues))
      (let ((dropped (superchat-runtime-test--queue-events "session-clear" "queue.dropped")))
        (should (equal (list id1 id2)
                       (mapcar (lambda (e) (plist-get (plist-get e :payload) :queue-id))
                               dropped)))
        (should (cl-every
                 (lambda (e) (equal "view-closed" (plist-get (plist-get e :payload) :reason)))
                 dropped))))))

(ert-deftest runtime/clear-follow-ups-tape-failure-does-not-retain-recorded-drop ()
  "A mid-clear append failure cannot resurrect an item Tape already dropped."
  (superchat-runtime-test--queue-fixture
    (let ((id1 (superchat-runtime-enqueue-follow-up
                "session-clear-fail"
                (superchat-runtime-test--queue-request "session-clear-fail")))
          (id2 (superchat-runtime-enqueue-follow-up
                "session-clear-fail"
                (superchat-runtime-test--queue-request "session-clear-fail")))
          (real (symbol-function 'superchat-db-tape-append))
          (calls 0))
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (&rest args)
                   (cl-incf calls)
                   (if (= calls 2) (error "tape write failed") (apply real args)))))
        (superchat-runtime-clear-follow-ups "session-clear-fail" "view-closed"))
      ;; id1's drop fact landed and was removed; id2's did not, so it stays
      ;; queued rather than being lost or silently treated as dropped.
      (should (= 1 (superchat-runtime-pending-follow-ups "session-clear-fail")))
      (should (equal id2
                     (plist-get
                      (car (gethash "session-clear-fail" superchat-runtime--session-queues))
                      :queue-id)))
      (let ((dropped
             (superchat-runtime-test--queue-events "session-clear-fail" "queue.dropped")))
        (should (equal (list id1)
                       (mapcar (lambda (e) (plist-get (plist-get e :payload) :queue-id))
                               dropped)))))))

(ert-deftest runtime/clear-follow-ups-cancels-and-removes-session-timer ()
  "Clearing a queue cancels and forgets its pending dispatch timer."
  (superchat-runtime-test--queue-fixture
    (superchat-runtime-enqueue-follow-up
     "session-clear-timer"
     (superchat-runtime-test--queue-request "session-clear-timer"))
    (superchat-runtime--schedule-dispatch-next "session-clear-timer")
    (let ((timer (gethash "session-clear-timer" superchat-runtime--queue-timers)))
      (should (timerp timer))
      (superchat-runtime-clear-follow-ups "session-clear-timer" "view-closed")
      (should-not (timer--triggered timer))
      (should-not (memq timer timer-list))
      (should-not (gethash "session-clear-timer" superchat-runtime--queue-timers)))))

(defun superchat-runtime-test--queue-dispatch-after (kind)
  "Enqueue one follow-up, finish the in-flight Run via KIND, assert dispatch.
KIND is `completed', `failed', or `cancelled'."
  (let* ((session (format "session-queue-%s" kind))
         (script (pcase kind
                   ('failed (list (list :error '(error . "boom"))
                                   (list :text "follow-up done")))
                   ('cancelled (list (list :hang t) (list :text "follow-up done")))
                   (_ (list (list :text "done") (list :text "follow-up done")))))
         run-id1)
    (journey-fixture-set-script script)
    (setq run-id1
          (plist-get (superchat-runtime-test-submit
                      (superchat-runtime-test--queue-request session))
                     :run-id))
    (superchat-runtime-enqueue-follow-up
     session (superchat-runtime-test--queue-request session))
    (if (eq kind 'cancelled)
        (should (superchat-runtime-cancel run-id1))
      (superchat-runtime-test--queue-wait-terminal run-id1))
    (should (eq kind (plist-get (superchat-runtime-status run-id1) :status)))
    ;; The dispatch timer is real and zero-delay: it fires on its own while
    ;; this polls, so it must never be fired explicitly (`timer-event-handler'
    ;; would race the same real event-loop processing this wait already
    ;; drives, and can double-fire or miss it entirely).
    (should
     (journey-fixture-wait-until
      (lambda () (= 0 (superchat-runtime-pending-follow-ups session))) 6))
    (let* ((dispatched (superchat-runtime-test--queue-events session "queue.dispatched"))
           (run-id2 (plist-get (plist-get (car dispatched) :payload) :run-id)))
      (should (= 1 (length dispatched)))
      (should (stringp run-id2))
      (should-not (equal run-id1 run-id2))
      ;; Proves the dispatched Run is real without asserting it is still
      ;; *active*: with a real async transport and no explicit timer
      ;; firing, it may have already completed by the time this checks.
      (should (= 1 (length
                    (cl-remove-if-not
                     (lambda (e) (equal run-id2 (plist-get e :run-id)))
                     (superchat-runtime-test--queue-events session "intent.received"))))))))

(ert-deftest runtime/follow-up-dispatched-after-completed ()
  "A queued follow-up is dispatched once its session's Run completes."
  (superchat-runtime-test--queue-fixture
    (superchat-runtime-test--queue-dispatch-after 'completed)))

(ert-deftest runtime/follow-up-dispatched-after-failed ()
  "A queued follow-up is dispatched once its session's Run fails."
  (superchat-runtime-test--queue-fixture
    (superchat-runtime-test--queue-dispatch-after 'failed)))

(ert-deftest runtime/follow-up-dispatched-after-cancelled ()
  "A queued follow-up is dispatched once its session's Run is cancelled."
  (superchat-runtime-test--queue-fixture
    (superchat-runtime-test--queue-dispatch-after 'cancelled)))

(ert-deftest runtime/follow-up-with-dead-target-is-dropped-and-next-continues ()
  "A dead-target follow-up is dropped; the live one behind it still dispatches."
  (superchat-runtime-test--queue-fixture
    (let (run-id1)
      (journey-fixture-set-script (list (list :text "done") (list :text "live done")))
      (setq run-id1
            (plist-get
             (superchat-runtime-test-submit
              (superchat-runtime-test--queue-request "session-queue-dead-target"))
             :run-id))
      (let* ((dead-buffer (generate-new-buffer " *queue-dead-target*"))
             (dead-request
              (list :prompt "dead" :session-id "session-queue-dead-target" :deadline nil
                    :buffer dead-buffer
                    :sink (superchat-sink-create
                           :live-p (lambda () (buffer-live-p dead-buffer)))))
             id-dead id-live)
        (kill-buffer dead-buffer)
        (setq id-dead
              (superchat-runtime-enqueue-follow-up "session-queue-dead-target" dead-request))
        (setq id-live
              (superchat-runtime-enqueue-follow-up
               "session-queue-dead-target"
               (superchat-runtime-test--queue-request "session-queue-dead-target")))
        (superchat-runtime-test--queue-wait-terminal run-id1)
        (should
         (journey-fixture-wait-until
          (lambda () (= 0 (superchat-runtime-pending-follow-ups
                           "session-queue-dead-target")))
          6))
        (let ((dropped
               (superchat-runtime-test--queue-events
                "session-queue-dead-target" "queue.dropped"))
              (dispatched
               (superchat-runtime-test--queue-events
                "session-queue-dead-target" "queue.dispatched")))
          (should (= 1 (length dropped)))
          (should (equal id-dead (plist-get (plist-get (car dropped) :payload) :queue-id)))
          (should (equal "sink-dead"
                         (plist-get (plist-get (car dropped) :payload) :reason)))
          (should (= 1 (length dispatched)))
          (should (equal id-live
                         (plist-get (plist-get (car dispatched) :payload) :queue-id)))
          ;; Proves the dispatched Run is real, without asserting it is
          ;; still *active* (racy: nothing here fires its timer explicitly).
          (should (stringp (plist-get (plist-get (car dispatched) :payload) :run-id))))))))

(ert-deftest runtime/dispatch-dead-target-tape-failure-retries-instead-of-losing-item ()
  "A dead-target whose own `queue.dropped' append fails is retried, not lost.

The injected failure wraps both the wait for the triggering Run and the
window where the retry runs: the dispatch timer is real and zero-delay, so
it can fire on its own mid-poll, and the failure must already be armed
whenever that happens -- firing it explicitly here would race the same
real event-loop processing these waits already drive."
  (superchat-runtime-test--queue-fixture
    (let (run-id1 id-dead real)
      (journey-fixture-set-script (list (list :text "done") (list :text "live done")))
      (setq run-id1
            (plist-get
             (superchat-runtime-test-submit
              (superchat-runtime-test--queue-request "session-dead-retry"))
             :run-id))
      (let ((dead-buffer (generate-new-buffer " *queue-dead-retry-target*")))
        (kill-buffer dead-buffer)
        (setq id-dead
              (superchat-runtime-enqueue-follow-up
               "session-dead-retry"
               (list :prompt "dead" :session-id "session-dead-retry" :deadline nil
                     :buffer dead-buffer))))
      (setq real (symbol-function 'superchat-db-tape-append))
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (session-id kind content &rest keys)
                   (if (string-match-p (regexp-quote (format "Follow-up %s dropped" id-dead))
                                        content)
                       (error "tape write failed")
                     (apply real session-id kind content keys)))))
        (superchat-runtime-test--queue-wait-terminal run-id1)
        ;; Let several real retry cycles run while every drop attempt fails:
        ;; the item must survive every one of them, never lost or dropped.
        (dotimes (_ 5) (accept-process-output nil 0.05) (sit-for 0.02))
        (should (= 1 (superchat-runtime-pending-follow-ups "session-dead-retry")))
        (should (equal id-dead
                       (plist-get (car (gethash "session-dead-retry"
                                                 superchat-runtime--session-queues))
                                  :queue-id)))
        (should-not
         (superchat-runtime-test--queue-events "session-dead-retry" "queue.dropped")))
      ;; With the injected failure gone, the next real retry succeeds.
      (should
       (journey-fixture-wait-until
        (lambda () (= 0 (superchat-runtime-pending-follow-ups "session-dead-retry"))) 6))
      (should (= 1 (length
                    (superchat-runtime-test--queue-events "session-dead-retry"
                                                            "queue.dropped")))))))

(ert-deftest runtime/dispatch-submit-error-tape-failure-retries-instead-of-losing-item ()
  "A submit-error whose own `queue.dropped' append fails is retried, not lost."
  (superchat-runtime-test--queue-fixture
    (let (run-id1 id-bad real)
      (journey-fixture-set-script (list (list :text "done") (list :text "retry done")))
      (setq run-id1
            (plist-get
             (superchat-runtime-test-submit
              (superchat-runtime-test--queue-request "session-submit-err-retry"))
             :run-id))
      ;; A request with no `:prompt' signals inside `superchat-runtime-submit'
      ;; itself, before any Run exists -- the declared `submit-error' path.
      (setq id-bad
            (superchat-runtime-enqueue-follow-up
             "session-submit-err-retry"
             (list :session-id "session-submit-err-retry" :deadline nil
                   :buffer (generate-new-buffer " *queue-submit-err-retry*"))))
      (setq real (symbol-function 'superchat-db-tape-append))
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (session-id kind content &rest keys)
                   (if (string-match-p (regexp-quote (format "Follow-up %s dropped" id-bad))
                                        content)
                       (error "tape write failed")
                     (apply real session-id kind content keys)))))
        (superchat-runtime-test--queue-wait-terminal run-id1)
        (dotimes (_ 5) (accept-process-output nil 0.05) (sit-for 0.02))
        (should (= 1 (superchat-runtime-pending-follow-ups "session-submit-err-retry")))
        (should (equal id-bad
                       (plist-get (car (gethash "session-submit-err-retry"
                                                 superchat-runtime--session-queues))
                                  :queue-id))))
      (should
       (journey-fixture-wait-until
        (lambda () (= 0 (superchat-runtime-pending-follow-ups "session-submit-err-retry")))
        6))
      (should (= 1 (length
                    (superchat-runtime-test--queue-events "session-submit-err-retry"
                                                            "queue.dropped")))))))

(ert-deftest runtime/dispatch-dispatched-fact-failure-still-consumes-item ()
  "A `queue.dispatched' append failure never resubmits the same follow-up."
  (superchat-runtime-test--queue-fixture
    (let (run-id1 id-live real)
      (journey-fixture-set-script (list (list :text "done") (list :text "live done")))
      (setq run-id1
            (plist-get
             (superchat-runtime-test-submit
              (superchat-runtime-test--queue-request "session-dispatched-fail"))
             :run-id))
      (setq id-live
            (superchat-runtime-enqueue-follow-up
             "session-dispatched-fail"
             (superchat-runtime-test--queue-request "session-dispatched-fail")))
      (setq real (symbol-function 'superchat-db-tape-append))
      (cl-letf (((symbol-function 'superchat-db-tape-append)
                 (lambda (session-id kind content &rest keys)
                   (if (string-match-p (regexp-quote (format "Follow-up %s dispatched"
                                                              id-live))
                                        content)
                       (error "tape write failed")
                     (apply real session-id kind content keys)))))
        (superchat-runtime-test--queue-wait-terminal run-id1)
        ;; The Run is actually submitted on the one and only attempt (there
        ;; is no retry path for an already-submitted item), so the item is
        ;; consumed even though its own fact could not be recorded.
        (should
         (journey-fixture-wait-until
          (lambda () (= 0 (superchat-runtime-pending-follow-ups "session-dispatched-fail")))
          6)))
      (should-not
       (superchat-runtime-test--queue-events "session-dispatched-fail" "queue.dispatched"))
      ;; Retrying it would duplicate the Run; confirm it did not, by real
      ;; Tape fact count rather than current `active-runs' membership
      ;; (racy: the dispatched Run's own completion is not controlled here).
      (should
       (= 2 (length
             (superchat-runtime-test--queue-events "session-dispatched-fail"
                                                     "intent.received")))))))

(ert-deftest runtime/dispatch-next-submits-at-most-one-follow-up-per-quiescence ()
  "A synchronously-completing provider still dispatches only one Run per call,
in FIFO order.

Uses its own inline `llm-chat-streaming'/`llm-chat-async' replacement (the
declared seam) that answers within the same call, rather than deferring
through a timer as `journey-fixture-with' does, so this can actually
reproduce the same-callback re-entrancy §7.1 forbids."
  (superchat-runtime-test--queue-fixture
    (let ((session "session-sync-dispatch") captured-prompts)
      (cl-letf* ((sync-transport
                  (lambda (provider prompt _partial response-callback error-callback
                                    multi-output)
                    (push (llm-chat-prompt-to-text prompt) captured-prompts)
                    (llm-provider-utils-process-result
                     provider prompt '(:text "done") multi-output
                     response-callback error-callback)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (provider prompt partial-callback response-callback
                                    error-callback &optional multi-output)
                    (funcall sync-transport provider prompt partial-callback
                             response-callback error-callback multi-output)))
                 ((symbol-function 'llm-chat-async)
                  (lambda (provider prompt response-callback error-callback
                                    &optional multi-output)
                    (funcall sync-transport provider prompt nil response-callback
                             error-callback multi-output))))
        (let (run-id0 id-1 id-2)
          (setq run-id0
                (plist-get
                 (superchat-runtime-test-submit
                  (superchat-runtime-test--queue-request session nil "run0"))
                 :run-id))
          ;; Submitting run0 above already completed and quiesced it
          ;; synchronously, scheduling a dispatch timer with no queue yet.
          (setq id-1
                (superchat-runtime-enqueue-follow-up
                 session (superchat-runtime-test--queue-request
                          session " *sync-fu1*" "follow-up 1")))
          (setq id-2
                (superchat-runtime-enqueue-follow-up
                 session (superchat-runtime-test--queue-request
                          session " *sync-fu2*" "follow-up 2")))
          (should (= 2 (superchat-runtime-pending-follow-ups session)))
          (superchat-runtime--schedule-dispatch-next session)
          (superchat-runtime-test--queue-fire session)
          ;; Exactly one dispatch per timer fire, even though the Run it
          ;; just submitted also completed synchronously inside this call.
          (should (= 1 (superchat-runtime-pending-follow-ups session)))
          (should (= 1 (length
                        (superchat-runtime-test--queue-events session "queue.dispatched"))))
          (let ((timer (gethash session superchat-runtime--queue-timers)))
            (should (timerp timer)))
          (superchat-runtime-test--queue-fire session)
          (should (= 0 (superchat-runtime-pending-follow-ups session)))
          (should (= 2 (length
                        (superchat-runtime-test--queue-events session "queue.dispatched"))))
          ;; FIFO, proven two ways: Tape's own `queue.dispatched' payload
          ;; order, and the real provider's own call order -- the
          ;; same-callback re-entrancy this test exists for could otherwise
          ;; reorder or double-submit either one.
          (should (equal (list id-1 id-2)
                         (mapcar (lambda (e) (plist-get (plist-get e :payload) :queue-id))
                                 (superchat-runtime-test--queue-events
                                  session "queue.dispatched"))))
          (let* ((prompts (nreverse captured-prompts))
                 (pos-1 (cl-position-if
                         (lambda (p) (string-match-p (regexp-quote "follow-up 1") p))
                         prompts))
                 (pos-2 (cl-position-if
                         (lambda (p) (string-match-p (regexp-quote "follow-up 2") p))
                         prompts)))
            (should pos-1)
            (should pos-2)
            (should (< pos-1 pos-2)))
          (ignore run-id0))))))

(ert-deftest runtime/dispatch-next-waits-while-session-has-active-run ()
  "Only one follow-up is submitted per completed Run; the rest keep waiting.

run1's own script round hangs rather than completing on its own, so it
stays reliably active for the \"stray reschedule\" check below instead of
racing that check against its own (real, async) completion; it is then
driven terminal deterministically by cancelling it, which still proves
dispatch resumes after any terminal transition (natural completion is
already covered by `runtime/follow-up-dispatched-after-completed')."
  (superchat-runtime-test--queue-fixture
    (let ((session "session-queue-wait") run-id0)
      (journey-fixture-set-script
       (list (list :text "done0") (list :hang t) (list :text "done2")))
      (setq run-id0
            (plist-get
             (superchat-runtime-test-submit (superchat-runtime-test--queue-request session))
             :run-id))
      (superchat-runtime-enqueue-follow-up
       session (superchat-runtime-test--queue-request session))
      (superchat-runtime-enqueue-follow-up
       session (superchat-runtime-test--queue-request session))
      (should (= 2 (superchat-runtime-pending-follow-ups session)))
      ;; Complete run0: exactly one follow-up is dispatched, the other waits.
      ;; The dispatch timer is real and zero-delay, so it is never fired
      ;; explicitly here -- it fires on its own while these wait, and an
      ;; explicit `timer-event-handler' call would race that same real
      ;; event-loop processing.
      (superchat-runtime-test--queue-wait-terminal run-id0)
      (should
       (journey-fixture-wait-until
        (lambda () (= 1 (superchat-runtime-pending-follow-ups session))) 6))
      (let* ((dispatched-1 (superchat-runtime-test--queue-events session "queue.dispatched"))
             (run-id1 (plist-get (plist-get (car dispatched-1) :payload) :run-id)))
        (should (= 1 (length dispatched-1)))
        (should (member run-id1 (superchat-runtime-active-runs)))
        ;; A stray reschedule while run1 is still active must not dispatch
        ;; early.  Nothing between here and the assertion yields to the real
        ;; event loop, so this span (unlike the waits above) cannot race the
        ;; timer it schedules.
        (superchat-runtime--schedule-dispatch-next session)
        (sit-for 0.1)
        (should (= 1 (superchat-runtime-pending-follow-ups session)))
        ;; Complete run1 (deterministically, since its round hangs): the
        ;; last follow-up is now dispatched.
        (should (superchat-runtime-cancel run-id1))
        (should
         (journey-fixture-wait-until
          (lambda () (= 0 (superchat-runtime-pending-follow-ups session))) 6))
        (should (= 2 (length
                      (superchat-runtime-test--queue-events session "queue.dispatched"))))))))

(ert-deftest runtime/drop-all-follow-ups-clears-every-session ()
  "The `kill-emacs-hook' handler drops every session's queue with `emacs-exit',
cancelling and forgetting each session's own pending dispatch timer too."
  (superchat-runtime-test--queue-fixture
    (superchat-runtime-enqueue-follow-up
     "session-exit-a" (superchat-runtime-test--queue-request "session-exit-a"))
    (superchat-runtime-enqueue-follow-up
     "session-exit-b" (superchat-runtime-test--queue-request "session-exit-b"))
    (superchat-runtime--schedule-dispatch-next "session-exit-a")
    (superchat-runtime--schedule-dispatch-next "session-exit-b")
    (let ((timer-a (gethash "session-exit-a" superchat-runtime--queue-timers))
          (timer-b (gethash "session-exit-b" superchat-runtime--queue-timers)))
      (should (timerp timer-a))
      (should (timerp timer-b))
      (superchat-runtime--drop-all-follow-ups)
      (should (= 0 (superchat-runtime-pending-follow-ups "session-exit-a")))
      (should (= 0 (superchat-runtime-pending-follow-ups "session-exit-b")))
      (should (= 0 (hash-table-count superchat-runtime--session-queues)))
      (should-not (timer--triggered timer-a))
      (should-not (timer--triggered timer-b))
      (should-not (memq timer-a timer-list))
      (should-not (memq timer-b timer-list))
      (should-not (gethash "session-exit-a" superchat-runtime--queue-timers))
      (should-not (gethash "session-exit-b" superchat-runtime--queue-timers))
      (let ((dropped-a (superchat-runtime-test--queue-events "session-exit-a" "queue.dropped"))
            (dropped-b (superchat-runtime-test--queue-events "session-exit-b" "queue.dropped")))
        (should (= 1 (length dropped-a)))
        (should (= 1 (length dropped-b)))
        (should (cl-every
                 (lambda (e) (equal "emacs-exit" (plist-get (plist-get e :payload) :reason)))
                 (append dropped-a dropped-b)))))))

(ert-deftest runtime/child-attach-refused-when-parent-terminal-or-terminalizing ()
  "S11/D5: a child Run cannot attach once its parent is :terminalizing.

The parent's `superchat-turn-completed-functions' hook fires from inside
`superchat-runtime--finish-body', after :terminalizing was set and after
:status already holds the parent's final terminal value -- exactly the
window the attach check must reject, with no Tape row for the rejected
child."
  (superchat-runtime-test--isolated
    (let (events child-error events-before-attach events-after-attach)
      (add-hook
       'superchat-turn-completed-functions
       (lambda (status)
         (setq events-before-attach (length events))
         (condition-case err
             (superchat-runtime-test-submit
              (append (superchat-runtime-test--request)
                      (list :parent-run-id (plist-get status :run-id))))
           (error (setq child-error err)))
         (setq events-after-attach (length events))))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event) (push event events) (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "done")))))
        (superchat-runtime-test-submit (superchat-runtime-test--request)))
      (should child-error)
      (should (eq 'error (car child-error)))
      (should (string-match-p "Parent Run unavailable"
                              (error-message-string child-error)))
      (should (integerp events-before-attach))
      (should (= events-before-attach events-after-attach)))))

(ert-deftest runtime/completed-parent-does-not-cancel-children-that-already-finished ()
  "S11/D5: a parent's cascade never re-terminates an already-finished child.

`superchat-runtime-cancel' already no-ops for a terminal Run, so cascading
into a child that finished on its own before its parent must append no
further events for that child."
  (superchat-runtime-test--isolated
    (let (events parent-run-id child-run-id child-events-at-finish)
      (setq parent-run-id
            (plist-get
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (event) (push event events) (length events)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'superchat--llm-generate-answer) #'ignore))
               (superchat-runtime-test-submit
                (append (superchat-runtime-test--request)
                        (list :actor-id "parent-actor"))))
             :run-id))
      (setq child-run-id
            (plist-get
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (event) (push event events) (length events)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'superchat--llm-generate-answer)
                        (lambda (&rest _args)
                          (funcall superchat--llm-runtime-observer
                                   'terminal
                                   '(:status completed :response "child done")))))
               (superchat-runtime-test-submit
                (append (superchat-runtime-test--request)
                        (list :actor-id "child-actor"
                              :parent-run-id parent-run-id))))
             :run-id))
      (should (eq 'completed
                  (plist-get (superchat-runtime-status child-run-id) :status)))
      (setq child-events-at-finish
            (length (cl-remove-if-not
                     (lambda (event) (equal child-run-id (plist-get event :run-id)))
                     events)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event) (push event events) (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore))
        (should (superchat-runtime-cancel parent-run-id))
        ;; Let the real timer-0 cascade actually run before asserting on it.
        (superchat-runtime-test--release-wait 0.1)
        (should (eq 'completed
                    (plist-get (superchat-runtime-status child-run-id) :status)))
        (should
         (= child-events-at-finish
            (length (cl-remove-if-not
                     (lambda (event) (equal child-run-id (plist-get event :run-id)))
                     events))))))))

(ert-deftest runtime/quiesce-prunes-child-from-parent-index ()
  "S11-fix-1: a quiesced child is removed from its parent's :child-run-ids
and is not revisited by a later cascade of that (still-live) parent."
  (superchat-runtime-test--isolated
    (let (events parent-run-id child-run-id)
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event) (push event events) (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer) #'ignore))
        (setq parent-run-id
              (plist-get (superchat-runtime-test-submit
                          (append (superchat-runtime-test--request)
                                  (list :actor-id "parent")))
                         :run-id)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event) (push event events) (length events)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result) #'ignore)
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (funcall superchat--llm-runtime-observer
                            'terminal
                            '(:status completed :response "child done")))))
        (setq child-run-id
              (plist-get
               (superchat-runtime-test-submit
                (append (superchat-runtime-test--request)
                        (list :actor-id "child" :parent-run-id parent-run-id)))
               :run-id)))
      (should (eq 'completed (plist-get (superchat-runtime-status child-run-id) :status)))
      ;; The child's own --finish-body already quiesced it synchronously;
      ;; the parent's live index no longer lists it.
      (should-not (plist-get (superchat-runtime-status parent-run-id) :child-run-ids))
      (let ((events-before (length events)))
        (cl-letf (((symbol-function 'superchat-tape-append-event)
                   (lambda (event) (push event events) (length events)))
                  ((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'superchat--process-llm-result) #'ignore))
          (should (superchat-runtime-cancel parent-run-id))
          (superchat-runtime-test--release-wait 0.1))
        ;; No cascade work for the pruned child: still `completed', and no
        ;; new event was ever appended for it.
        (should (eq 'completed (plist-get (superchat-runtime-status child-run-id) :status)))
        (should
         (= 0 (length (cl-remove-if-not
                       (lambda (event) (equal child-run-id (plist-get event :run-id)))
                       (seq-take events (- (length events) events-before))))))))))

(ert-deftest runtime/cancel-eventually-cancels-three-generations ()
  "S11/D5: cancelling a Run eventually cancels every live descendant, and no
`superchat-runtime-cancel' call for a descendant ever nests inside another
still-executing call's own decision (§7.1's \"no callbacks inside a
decision\", observed here via a passthrough :around advice rather than by
replacing Runtime/Tape internals -- the same technique journey-fixture.el
itself uses to observe `llm-cancel-request' without replacing it).

Three real Runs are chained by :parent-run-id (root -> child -> grandchild)
and submitted directly with `journey-fixture-with''s real Tape store and
provider-network seam; this is a non-goal-free probe of the cascade
mechanism itself (§7.4), not of subagent tool routing -- delegation
semantics are this slice's explicit non-goal."
  (journey-fixture-with ()
    (journey-fixture-set-script (list (list :hang t) (list :hang t) (list :hang t)))
    (let* ((root (plist-get
                  (superchat-runtime-test-submit
                   (superchat-runtime-test--queue-request "session-3gen" " *3gen-root*"))
                  :run-id))
           (child (plist-get
                   (superchat-runtime-test-submit
                    (append
                     (superchat-runtime-test--queue-request "session-3gen" " *3gen-child*")
                     (list :parent-run-id root)))
                   :run-id))
           (grandchild
            (plist-get
             (superchat-runtime-test-submit
              (append
               (superchat-runtime-test--queue-request "session-3gen" " *3gen-grandchild*")
               (list :parent-run-id child)))
             :run-id))
           (depth 0)
           nested-observed
           (observer
            (lambda (orig run-id &rest args)
              (when (> depth 0) (push run-id nested-observed))
              (cl-incf depth)
              (unwind-protect (apply orig run-id args)
                (cl-decf depth)))))
      (should (equal (list child)
                     (plist-get (superchat-runtime-status root) :child-run-ids)))
      (should (equal (list grandchild)
                     (plist-get (superchat-runtime-status child) :child-run-ids)))
      (advice-add 'superchat-runtime-cancel :around observer)
      (unwind-protect
          (progn
            (should (superchat-runtime-cancel root))
            ;; §7.1 timer-0 deferral: the cascade has not run synchronously
            ;; the instant `superchat-runtime-cancel' returns -- neither
            ;; descendant is terminal yet.
            (should-not
             (eq 'cancelled (plist-get (superchat-runtime-status child) :status)))
            (should-not
             (eq 'cancelled (plist-get (superchat-runtime-status grandchild) :status)))
            (should
             (journey-fixture-wait-until
              (lambda ()
                (and (eq 'cancelled (plist-get (superchat-runtime-status child) :status))
                     (eq 'cancelled
                         (plist-get (superchat-runtime-status grandchild) :status))))
              6)))
        (advice-remove 'superchat-runtime-cancel observer))
      ;; The invariant: every cascade-triggered `superchat-runtime-cancel'
      ;; call (root's own cascade calling child's, child's own cascade
      ;; calling grandchild's) happened with no other such call still on
      ;; the stack -- proving the deferral is real, not just delayed enough
      ;; to look asynchronous in this one run.
      (should-not nested-observed)
      (let* ((root-session (plist-get (superchat-runtime-status root) :session-id))
             (child-session (plist-get (superchat-runtime-status child) :session-id))
             (grandchild-session
              (plist-get (superchat-runtime-status grandchild) :session-id))
             (root-turn-completed-id
              (plist-get
               (cl-find-if
                (lambda (event)
                  (and (equal "turn.completed" (plist-get event :type))
                       (equal root (plist-get event :run-id))))
                (superchat-tape-replay-events root-session))
               :storage-id))
             (child-cancelled-id
              (plist-get
               (cl-find-if
                (lambda (event)
                  (and (equal "run.cancelled" (plist-get event :type))
                       (equal child (plist-get event :run-id))))
                (superchat-tape-replay-events child-session))
               :storage-id))
             (grandchild-cancelled-id
              (plist-get
               (cl-find-if
                (lambda (event)
                  (and (equal "run.cancelled" (plist-get event :type))
                       (equal grandchild (plist-get event :run-id))))
                (superchat-tape-replay-events grandchild-session))
               :storage-id)))
        (should root-turn-completed-id)
        (should child-cancelled-id)
        (should grandchild-cancelled-id)
        (should (< root-turn-completed-id child-cancelled-id))
        (should (< root-turn-completed-id grandchild-cancelled-id)))
      ;; S11-fix-1: the live index is pruned on quiesce, so a completed
      ;; cascade leaves no stale entries behind either.
      (should-not (plist-get (superchat-runtime-status root) :child-run-ids))
      (should-not (plist-get (superchat-runtime-status child) :child-run-ids)))))

(ert-deftest runtime/chat-prepared-prompt-is-not-re-budgeted ()
  "A prepared chat prompt longer than the old 40000-character pipeline
ceiling still reaches the provider unchanged (S10-fix-2 finding 1): the
prepared prompt was already assembled and refit against the model's real
budget by `superchat-core-prepare-turn', so the driver must not re-budget
it against a second ceiling."
  (journey-fixture-with ()
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (filler (make-string 45000 ?x))
           (prepared (concat "PREPARED-BIG-" filler)))
      (journey-fixture-set-script (list (list :text "big prompt accepted")))
      (let ((run (plist-get (superchat--submit-chat-request
                             (list :buffer view
                                   :session-id session
                                   :input "big question"
                                   :clean-input "big question"
                                   :prompt prepared
                                   :origin '(:surface chat)))
                            :run-id)))
        (should run)
        (should (journey-fixture-wait-until
                 (lambda ()
                   (superchat-runtime--terminal-p
                    (plist-get (superchat-runtime-status run) :status)))
                 8)))
      (let ((prompt-text (plist-get (journey-fixture-last-request) :prompt-text)))
        (should (string-match-p "PREPARED-BIG-" prompt-text))
        (should (string-match-p "x\\{45000\\}" prompt-text))
        (should (<= 45013 (length prompt-text)))))))

(ert-deftest runtime/chat-envelope-carries-request-local-approval-function ()
  "A composed chat request's `:approval-function' reaches Authority and wins
over the global presenter (contract §2.3, S10-fix-2 finding 2); without it
the global one is used."
  (journey-fixture-with (:tools-policy 'always)
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           (local-calls 0)
           (global-calls 0)
           (superchat-authority-approval-function
            (lambda (_request) (cl-incf global-calls) t))
           (local (lambda (_request) (cl-incf local-calls) t)))
      (journey-fixture-set-script
       (list (list :tool-uses '(("eval_elisp" . ((expression . "(+ 1 2)")))))
             (list :text "first done")))
      (let ((run (plist-get (superchat--submit-chat-request
                             (list :buffer view
                                   :session-id session
                                   :input "run it"
                                   :clean-input "run it"
                                   :prompt "run it"
                                   :tools '("eval_elisp")
                                   :origin '(:surface chat)
                                   :approval-function local))
                            :run-id)))
        (should run)
        (should (journey-fixture-wait-until
                 (lambda ()
                   (superchat-runtime--terminal-p
                    (plist-get (superchat-runtime-status run) :status)))
                 8))
        (should (member "tool.completed"
                        (journey-fixture-event-types session))))
      (should (= 1 local-calls))
      (should (= 0 global-calls))
      ;; Without a request-local function, the global presenter is used.
      (journey-fixture-set-script
       (list (list :tool-uses '(("eval_elisp" . ((expression . "(+ 2 3)")))))
             (list :text "second done")))
      (let ((run (plist-get (superchat--submit-chat-request
                             (list :buffer view
                                   :session-id session
                                   :input "run it again"
                                   :clean-input "run it again"
                                   :prompt "run it again"
                                   :tools '("eval_elisp")
                                   :origin '(:surface chat)))
                            :run-id)))
        (should run)
        (should (journey-fixture-wait-until
                 (lambda ()
                   (superchat-runtime--terminal-p
                    (plist-get (superchat-runtime-status run) :status)))
                 8)))
      (should (= 1 local-calls))
      (should (= 1 global-calls)))))

(ert-deftest runtime/chat-envelope-attendance-defaults-at-the-stage ()
  "The dispatcher fills `:attendance' with `attended', so pipeline stages see
the contract default rather than an explicit nil (S10-fix-2 finding 4)."
  (journey-fixture-with ()
    (let* ((view (journey-fixture-open-chat))
           (session (journey-fixture-session-id view))
           seen)
      (journey-fixture-set-script (list (list :text "attended ok")))
      (superchat-stage-add
       'admit 'runtime-test-attendance
       (lambda (_session-id envelope _snapshot)
         (when (and (equal session (plist-get envelope :session-id))
                    (eq 'chat (plist-get envelope :surface)))
           (setq seen (plist-get envelope :attendance)))
         nil)
       :owner 'runtime-test)
      (unwind-protect
          (progn
            (should (journey-fixture-send "attendance question" view))
            (should (journey-fixture-wait-for-run view)))
        (superchat-stage-remove 'admit 'runtime-test-attendance))
      (should (eq 'attended seen)))))

(provide 'test-runtime)

;;; test-runtime.el ends here
