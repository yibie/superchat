;;; test-runtime.el --- ERT tests for Superchat Runtime -*- lexical-binding: t; -*-

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat)

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
            (should-error (superchat-runtime-submit request))))
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
          (should-error (superchat-runtime-submit request)))
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
                (superchat-runtime-submit
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
          (let* ((handle (superchat-runtime-submit request))
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
                (superchat-runtime-submit
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
                          (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                   "superchat-runtime.el" "superchat-llm.el"))
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
             "              ((symbol-function 'llm-chat-streaming)\n"
             "               (lambda (_provider _prompt _partial response _error\n"
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
             "                                  :stream-function\n"
             "                                  (lambda (chunk) (setq custom-stream chunk))\n"
             "                                  :delivery-function\n"
             "                                  (lambda (value status _id)\n"
             "                                    (setq custom-delivery (list value status))))))\n"
             "                   (default-buffer (generate-new-buffer \" *runtime-bare-default*\"))\n"
             "                   (default (superchat-runtime-submit\n"
             "                             (list :prompt \"default\" :buffer default-buffer\n"
             "                                   :session-id \"bare-default\" :deadline nil))))\n"
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
  "The standard UI path still receives status, stream, and final delivery."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-present*"))
            status-messages stream-chunks delivered)
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--update-status)
                       (lambda (message &optional _owner) (push message status-messages)))
                      ((symbol-function 'superchat--stream-llm-result)
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
              (let* ((handle (superchat-runtime-submit
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
  "A present Render function's failure is not treated as absence."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-error*"))
            (provider-calls 0))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--update-status)
                       (lambda (_message &optional _owner) (error "render status exploded")))
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args) (cl-incf provider-calls))))
              (should-error
               (superchat-runtime-submit
                (list :prompt "render error" :buffer buffer
                      :session-id "render-error-session" :deadline nil))
               :type 'error)
              (should (= 0 provider-calls)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/runtime-submit-does-not-swallow-render-stream-quit ()
  "A present streaming Render function's quit remains observable."
  (superchat-runtime-test--isolated
    (superchat-runtime-test--with-temp-store
      (let ((buffer (generate-new-buffer " *runtime-render-stream-quit*"))
            (provider-calls 0))
        (unwind-protect
            (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--stream-llm-result)
                       (lambda (_chunk &optional _owner)
                         (signal 'quit '("render stream interrupted"))))
                      ((symbol-function 'superchat--process-llm-result) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (_prompt _callback stream &rest _args)
                         (cl-incf provider-calls)
                         (funcall stream "render-chunk"))))
              (let (caught)
                (condition-case err
                    (superchat-runtime-submit
                     (list :prompt "render quit" :buffer buffer
                           :session-id "render-quit-session" :deadline nil))
                  (quit (setq caught err)))
                (should (equal '(quit "render stream interrupted") caught)))
              (should (= 1 provider-calls)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest runtime/eager-distribution-does-not-require-integrations ()
  "Integration packages load on demand, except four reviewed call-time edges.
 The integration gate permits exactly these middle-tier lazy loads:
 `superchat-tools.el -> superchat-subagent' at lines 814, 833, and 1237,
 and `superchat-tools.el -> superchat-genui' at line 1001.  Integration
 packages may require another integration package within their declared
 boundary; all other non-plugin files must use autoloads and registries.
 Scan both top-level and call-time `require' forms."
  (let ((root (file-name-directory (locate-library "superchat-runtime")))
        (plugins '("superchat-rewrite" "superchat-compact" "superchat-workflow"
                   "superchat-subagent" "superchat-agents-panel"
                   "superchat-genui" "superchat-magit" "ob-superchat"
                   "superchat-emacsclient")))
    (dolist (file (directory-files root t "\\.el\\'"))
      (let ((base (file-name-sans-extension
                   (file-name-nondirectory file))))
        (unless (member base plugins)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "(\\s-*require\\s-+'\\([[:alnum:]_-]+\\)" nil t)
              (should (or (not (member (match-string 1) plugins)) (test-runtime--integration-require-exemption-p (file-name-nondirectory file) (match-string 1) (line-number-at-pos (match-beginning 0))))))))))))

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
        (superchat-runtime-submit
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
        (superchat-runtime-submit
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
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
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
        (setq handle (superchat-runtime-submit
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
               (handle (superchat-runtime-submit request))
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
               (superchat-runtime-submit
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
        (superchat-runtime-submit (superchat-runtime-test--request))
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
        (superchat-runtime-submit
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
         (superchat-runtime-submit
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
         (superchat-runtime-submit
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
         (superchat-runtime-submit
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
         (superchat-runtime-submit
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
        (superchat-runtime-submit
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
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :delivery-function
                        (lambda (&rest _args) (error "consumer failed"))))))
               (status
                (superchat-runtime-status (plist-get handle :run-id))))
          (should (eq 'completed (plist-get status :status)))
          (should
           (equal '("intent.received" "run.started" "response.completed"
                    "run.completed" "turn.completed")
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
                     (handle (superchat-runtime-submit request))
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
                      (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                      (superchat-runtime-submit
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
                      (superchat-runtime-submit
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
                      (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                          (superchat-runtime-submit
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
                (setq-local superchat--session-id session-id)
                (setq-local superchat--conversation-history nil))
              (with-current-buffer buffer-name
                (superchat--insert-prompt)
                (superchat--prepare-for-response))
              (cl-letf (((symbol-function 'superchat--update-status) #'ignore)
                        ((symbol-function 'superchat--llm-generate-answer)
                         (lambda (&rest _) 'pending-request)))
                (let* ((handle
                        (superchat-runtime-submit
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
        (superchat-runtime-submit (superchat-runtime-test--request))
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
        (let* ((handle (superchat-runtime-submit
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
                    (superchat-runtime-submit
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
        (let* ((handle
                (superchat-runtime-submit
                 (append
                  (superchat-runtime-test--request)
                  (list :actor-id "coder-1"
                        :parent-run-id "run-parent"
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
          (should (equal "run-parent"
                         (plist-get status :parent-run-id)))
          (should (equal "coding" (plist-get status :capability)))
          (should (equal "profile.coder" (plist-get started :profile-id)))
          (should (equal "run-parent"
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
                (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                (superchat-runtime-submit
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
   (superchat-runtime-submit
    '(:prompt "hello" :authority-policy "admin")))
  (should-error
   (superchat-runtime-submit
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
                (superchat-runtime-submit
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
               (handle (superchat-runtime-submit request))
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
               (superchat-runtime-submit
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
         (superchat-runtime-submit (list :prompt "hello" :buffer buffer))
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
                  (superchat-runtime-submit
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
               (superchat-runtime-submit
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
                    (superchat-runtime-submit
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
                    (superchat-runtime-submit
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
               (superchat-runtime-submit
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
                  (superchat-runtime-submit
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
          (push (superchat-runtime-submit
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
              (superchat-runtime-submit
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
              (superchat-runtime-submit
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
                  (superchat-runtime-submit
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
              (superchat-runtime-submit
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
             (superchat-runtime-submit
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
        (superchat-runtime-submit
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
        (superchat-runtime-submit
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
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id &optional _owner) (setq delivered answer)))
                ((symbol-function 'llm-cancel-request)
                 (lambda (request) (setq cancelled request)))
                ((symbol-function 'superchat--llm-generate-answer)
                 (lambda (&rest _args)
                   (setq observer superchat--llm-runtime-observer)
                   (funcall observer 'request 'request-1)
                   (funcall observer 'request 'request-2)
                   'request-2)))
        (let* ((handle
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'request-2 cancelled))
          (should (equal "[Cancelled by user]" delivered))
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
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (answer _tape-id &optional _owner) (push answer delivered)))
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
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
               (run-id (plist-get handle :run-id)))
          (should (superchat-runtime-cancel run-id))
          (should (eq 'provider-request cancelled))
          (should
           (eq 'cancelled
               (plist-get (superchat-runtime-status run-id) :status)))
          (should (equal '("[Cancelled by user]") delivered))
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
                (superchat-runtime-submit
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
         (superchat-runtime-submit
          (superchat-runtime-test--request)))
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
         (superchat-runtime-submit
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
               (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                          (superchat-runtime-submit
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
                (superchat-runtime-submit
                 (superchat-runtime-test--request)))
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
                (superchat-runtime-submit
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
                (superchat-runtime-submit
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
                     (superchat-runtime-submit
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
               (handle (superchat-runtime-submit request))
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
                     (superchat-runtime-submit
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
              (superchat-runtime-submit
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
                      (superchat-runtime-submit
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
         (defaults
          '((superchat-buffer-name . "*superchat*")
            (superchat-context-max-chars)
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
          '((superchat-buffer-name . "*saved-superchat*")
            (superchat-context-max-chars . 12345)
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
          (should (= 19 (length defaults)))
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
;; gate.  The location is part of each exception: moving a require or adding
;; another one must fail until the new edge is reviewed explicitly.
(defconst test-runtime--integration-require-exemptions
  '(("superchat-tools.el" "superchat-subagent" 814)
    ("superchat-tools.el" "superchat-subagent" 833)
    ("superchat-tools.el" "superchat-genui" 1001)
    ("superchat-tools.el" "superchat-subagent" 1237))
  "Exact reviewed exceptions to the integration require gate.")

(defun test-runtime--integration-require-exemption-p (file feature line)
  "Return non-nil for a reviewed integration FILE/FEATURE/LINE edge."
  (member (list file feature line)
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
                      (superchat-runtime-submit
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
                         (superchat-runtime-submit
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
                          (superchat-runtime-submit
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
                      (plist-get (superchat-runtime-submit
                                  (list :session-id "deadline-other" :buffer other-view
                                        :prompt "hold" :input "hold" :tools nil :deadline 60))
                                 :run-id)
                      other-timer (plist-get (superchat-runtime--entry other-id) :deadline-timer))
                (push other-id ids) (push other-timer timers)
                (should (memq other-timer timer-list)))
              (dolist (kind '(cancelled completed failed timed-out disabled))
                (let* ((view (generate-new-buffer " *deadline-positive*"))
                       (_ (push view views))
                       (handle (superchat-runtime-submit
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

(provide 'test-runtime)

;;; test-runtime.el ends here
