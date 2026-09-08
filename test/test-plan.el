;;; test-plan.el --- Structured plan tests: propose, review data, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Code:

(require 'ert)
(require 'superchat-plan)

;; Native transport-only integration fixture; never replaces Runtime or Closure.
(eval-and-compile
  (unless (fboundp 'test-input-batch--fixture)
    (load (expand-file-name "test-llm-backend.el"
                            (file-name-directory (or load-file-name buffer-file-name)))
          nil t)))

(defvar plan-owner-test--dispatch nil)

(defmacro plan-owner-test--fixture (mode &rest body)
  (declare (indent 1) (debug t))
  `(test-input-batch--fixture ,mode
     (require 'superchat)
     (let* ((superchat-editor-workspace--states (make-hash-table :test #'equal))
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions '(superchat-plan--nudge-on-completion))
            (superchat-post-turn-functions nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-llm-tool-names '("input_batch_effect"))
            (superchat-buffer-name (generate-new-buffer-name " *plan-owner*"))
            (superchat-plan-auto-execute t)
            (plan-owner-test--dispatch nil)
            (endpoint (symbol-function 'llm-request-plz-async))
            view session)
       (unwind-protect
           (save-window-excursion
             (call-interactively #'superchat)
             (setq view (current-buffer) session superchat--session-id)
             (setq-local superchat-authority-mode 'auto
                         superchat-authority--session-allowlist '("input_batch_effect"))
             (goto-char (point-max))
             (insert (propertize "PRIVATE draft\nsecond line" 'plan-draft t))
             (superchat-plan-propose '(("inspect" "inspected")) "owner plan")
             (cl-letf (((symbol-function 'llm-request-plz-async)
                        (lambda (&rest args)
                          (prog1 (apply endpoint args)
                            (should (eq (and (plist-get (car test-input-batch--requests)
                                                       :media-type) t)
                                        (eq ,mode 'streaming)))
                            (when plan-owner-test--dispatch
                              (funcall plan-owner-test--dispatch
                                       (car test-input-batch--requests)))))))
               ,@body))
         (let ((superchat-turn-completed-functions nil))
           (maphash (lambda (id _) (superchat-runtime-cancel id "fixture cleanup"))
                    superchat-runtime--runs))
         (when (buffer-live-p view)
           (with-current-buffer view (superchat--cancel-status-refresh-timer))
           (kill-buffer view))
         (dolist (buffer (superchat-editor-workspace-owned-buffers session))
           (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(defmacro plan-expiry-test--fixture (mode &rest body)
  "Use real Plan approval with explicit test-only trusted input configuration."
  (declare (indent 1) (debug t))
  `(plan-owner-test--fixture ,mode
     (let ((prepare (symbol-function 'superchat--prepare-generated-request)))
       (cl-letf (((symbol-function 'superchat--prepare-generated-request)
                  (lambda (&rest args)
                    (let ((request (apply prepare args)))
                      (setq request (plist-put request :tools '("ask_user")))
                      (setq request (plist-put request :input-core (test-trusted-ask--config)))
                      (plist-put request :deadline 1)))))
         ,@body))))

(defun plan-expiry-test--wait (run)
  "Wait for the actual terminal transition, failing after three seconds."
  (let ((bound (+ (float-time) 3)))
    (while (and (not (superchat-runtime--terminal-p
                     (plist-get (superchat-runtime-status run) :status)))
                (< (float-time) bound))
      (sleep-for 0.01)))
  (should (superchat-runtime--terminal-p
           (plist-get (superchat-runtime-status run) :status))))

(defun plan-expiry-test--events (run type)
  (cl-remove-if-not
   (lambda (event) (and (equal run (plist-get event :run-id))
                        (equal type (plist-get event :type))))
   (superchat-tape-replay-events superchat--session-id)))

(ert-deftest plan-expiry/pending-real-deadline-blocks-successor ()
  (dolist (mode '(async streaming))
    (plan-expiry-test--fixture mode
      (let ((draft (buffer-substring superchat--prompt-start (point-max)))
            (recall (list (list :content "QUEUED"))) observed)
        (setq-local superchat--pending-recalled-memories recall)
        (let ((superchat-turn-completed-functions
               (cons (lambda (status) (push status observed))
                     superchat-turn-completed-functions)))
          (superchat-plan-approve)
          (let* ((owner superchat-plan--execution) (run (plist-get owner :run)))
            (test-trusted-ask--reply (car test-input-batch--requests))
            (should (superchat-runtime-pending-input run))
            (should-not (plist-get (superchat-runtime-status run) :input-continuation-blocked))
            (plan-expiry-test--wait run)
            (should (= 1 (length test-input-batch--requests)))
            (should (eq 'unanswered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
            (should (eq 'unanswered (plist-get (car observed) :input-continuation-blocked)))
            (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
            (should-not (plist-get (superchat-runtime-status run) :stop-requested))
            (should-not (superchat-plan--execution-current-p owner))
            (should-not superchat-plan--nudge-pending)
            (should (= 0 superchat-plan--nudges))
            (should (= 1 (length (plan-expiry-test--events run "tool.failed"))))
            (should-not (plan-expiry-test--events run "tool.completed"))
            (should (eq 'proved (plist-get (superchat-closure-judge
                                           (list :kind 'durable-run-terminal :run-id run)
                                           (superchat-tape-replay-events session)) :verdict)))
          (should (eq recall superchat--pending-recalled-memories))
          (should (equal-including-properties draft
                    (buffer-substring superchat--prompt-start (point-max))))))))))

(ert-deftest plan-expiry/postreceipt-deadline-blocks-without-failed-duplicate ()
  (dolist (mode '(async streaming))
    (plan-expiry-test--fixture mode
      (superchat-plan-approve)
      (let* ((run (plist-get superchat-plan--execution :run))
             (append-event (symbol-function 'superchat-tape-append-event-record))
             (hits 0))
        (test-trusted-ask--reply (car test-input-batch--requests))
        (let ((question (superchat-runtime-pending-input run)))
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event &rest args)
                       (prog1 (apply append-event event args)
                         (when (and (equal run (plist-get event :run-id))
                                    (equal "tool.completed" (plist-get event :type)))
                           (cl-incf hits)
                           ;; Real elapsed deadline while the actual committed
                           ;; receipt append unwinds; no status/timer mutation.
                           (sleep-for (max 0 (+ 0.02 (- (plist-get (superchat-runtime--entry run) :deadline)
                                                       (float-time))))))))))
            (superchat-runtime-answer-input run (plist-get question :handle) "Paris")))
        (should (= hits 1))
        (should (= 1 (length test-input-batch--requests)))
        (should (eq 'answer-not-delivered
                    (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
        (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
        (should (= 1 (length (plan-expiry-test--events run "tool.completed"))))
        (should-not (plan-expiry-test--events run "tool.failed"))
        (should (eq 'proved (plist-get (superchat-closure-judge
                                       (list :kind 'durable-run-terminal :run-id run)
                                       (superchat-tape-replay-events session)) :verdict)))
        (should (= 0 superchat-plan--nudges))))))

(ert-deftest plan-expiry/receipt-audit-failure-blocks-before-hooks ()
  (dolist (mode '(async streaming))
    (dolist (commit '(nil t))
      (dolist (condition-type '(error quit))
        (plan-expiry-test--fixture mode
          (superchat-plan-approve)
          (let* ((run (plist-get superchat-plan--execution :run))
                 (append-event (symbol-function 'superchat-tape-append-event-record))
                 (hits 0) caught observed)
            (test-trusted-ask--reply (car test-input-batch--requests))
            (let ((question (superchat-runtime-pending-input run))
                  (superchat-turn-completed-functions
                   (cons (lambda (status) (push status observed)) superchat-turn-completed-functions)))
              (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                         (lambda (event &rest args)
                           (if (and (equal run (plist-get event :run-id))
                                    (equal "tool.completed" (plist-get event :type)))
                               (progn
                                 (cl-incf hits)
                                 (when commit (apply append-event event args))
                                 (signal condition-type '("EXACT INPUT AUDIT")))
                             (apply append-event event args)))))
                (condition-case err
                    (superchat-runtime-answer-input run (plist-get question :handle) "Paris")
                  ((error quit) (setq caught err)))))
            (should (= hits 1))
            (should (equal caught (list condition-type "EXACT INPUT AUDIT")))
            (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status)))
            (should (eq 'answer-not-delivered (plist-get (car observed) :input-continuation-blocked)))
            (should (= 1 (length test-input-batch--requests)))
            (should (= (if commit 1 0) (length (plan-expiry-test--events run "tool.completed"))))
            (should-not (plan-expiry-test--events run "tool.failed"))
            (should-not superchat-plan--nudge-pending)))))))

(ert-deftest plan-expiry/delivered-h2-failure-timeout-retains-legacy-nudges ()
  (dolist (mode '(async streaming))
    (dolist (end '(failed timed-out))
      (plan-expiry-test--fixture mode
        (superchat-plan-approve)
        (let ((run (plist-get superchat-plan--execution :run)))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (superchat-runtime-answer-input
           run (plist-get (superchat-runtime-pending-input run) :handle) "Paris")
          (should (= 2 (length test-input-batch--requests)))
          (if (eq end 'failed)
              (funcall (plist-get (car test-input-batch--requests) :on-error) 'http-error "H2 failure")
            (plan-expiry-test--wait run))
          (should (eq end (plist-get (superchat-runtime-status run) :status)))
          (should-not (plist-get (superchat-runtime-status run) :input-continuation-blocked))
          (should (= 3 (length test-input-batch--requests)))
          (should (= 1 superchat-plan--nudges))
          (dotimes (_ 2) (test-input-batch--reply (car test-input-batch--requests) nil))
          (should (= 4 (length test-input-batch--requests)))
          (should (= 2 superchat-plan--nudges))
          (should-not superchat-plan--nudge-pending))))))

(ert-deftest plan-expiry/same-stack-q1-q2-current-blocker ()
  (dolist (mode '(async streaming))
    (plan-expiry-test--fixture mode
      (let ((config (symbol-function 'test-trusted-ask--config)) q1)
        (cl-letf (((symbol-function 'test-trusted-ask--config)
                   (lambda (&rest args)
                     (let ((value (apply config args)))
                       (plist-put value :presenter
                                  (lambda (q)
                                    (unless q1
                                      (setq q1 q)
                                      (superchat-runtime-answer-input
                                       (plist-get q :run-id) (plist-get q :handle) "Paris"))))))))
          (setq plan-owner-test--dispatch
                (lambda (request)
                  (when (= 2 (length test-input-batch--requests))
                    (test-trusted-ask--reply request "Which date?"))))
          (superchat-plan-approve))
        (let ((run (plist-get superchat-plan--execution :run)))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let ((q2 (superchat-runtime-pending-input run)))
            (should (equal "Which date?" (plist-get q2 :question)))
            (should-not (eq (plist-get q1 :handle) (plist-get q2 :handle)))
            (should-not (superchat-runtime-answer-input run (plist-get q1 :handle) "stale"))
            (plan-expiry-test--wait run)
            (should (eq 'unanswered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
            (should-not (superchat-runtime-answer-input run (plist-get q2 :handle) "late")))
          (should (= 2 (length test-input-batch--requests)))
          (should (= 1 (length (plan-expiry-test--events run "tool.completed"))))
          (should (= 1 (length (plan-expiry-test--events run "tool.failed"))))
          (should (= 0 superchat-plan--nudges)))))))

(ert-deftest plan-expiry/pending-expiry-audit-receipt-priority ()
  (dolist (mode '(async streaming))
    (dolist (commit '(nil t))
      (plan-expiry-test--fixture mode
        (superchat-plan-approve)
        (let* ((run (plist-get superchat-plan--execution :run))
               (append-event (symbol-function 'superchat-tape-append-event-record))
               (hits 0))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event &rest args)
                       (if (and (equal run (plist-get event :run-id))
                                (equal "tool.failed" (plist-get event :type)))
                           (progn
                             (cl-incf hits)
                             (when commit (apply append-event event args))
                             (error "EXPIRY RECEIPT AUDIT"))
                         (apply append-event event args)))))
            (plan-expiry-test--wait run))
          (should (= hits 1))
          (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status)))
          (should (string-match-p "EXPIRY RECEIPT AUDIT" (plist-get (superchat-runtime-status run) :error)))
          (should (eq 'unanswered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
          (should (= 1 (length test-input-batch--requests)))
          (should (= (if commit 1 0) (length (plan-expiry-test--events run "tool.failed"))))
          (should-not (plan-expiry-test--events run "tool.completed"))
          (should-not superchat-plan--nudge-pending))))))

(ert-deftest plan-expiry/postreceipt-expiry-then-audit-priority ()
  (dolist (mode '(async streaming))
    (dolist (condition-type '(error quit))
      (plan-expiry-test--fixture mode
        (superchat-plan-approve)
        (let* ((run (plist-get superchat-plan--execution :run))
               (append-event (symbol-function 'superchat-tape-append-event-record))
               (hits 0) caught)
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let ((q (superchat-runtime-pending-input run)))
            (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                       (lambda (event &rest args)
                         (prog1 (apply append-event event args)
                           (when (and (equal run (plist-get event :run-id))
                                      (equal "tool.completed" (plist-get event :type)))
                             (cl-incf hits)
                             (sleep-for (max 0 (+ 0.02 (- (plist-get (superchat-runtime--entry run) :deadline)
                                                         (float-time)))))
                             (signal condition-type '("AFTER EXPIRY AUDIT")))))))
              (condition-case err
                  (superchat-runtime-answer-input run (plist-get q :handle) "Paris")
                ((error quit) (setq caught err)))))
          (should (= hits 1))
          (should (equal caught (list condition-type "AFTER EXPIRY AUDIT")))
          (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status)))
          (should (eq 'answer-not-delivered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
          (should (= 1 (length (plan-expiry-test--events run "tool.completed"))))
          (should-not (plan-expiry-test--events run "tool.failed"))
          (should (= 1 (length test-input-batch--requests)))
          (should-not superchat-plan--nudge-pending))))))

(ert-deftest plan-expiry/manual-input-timeout-does-not-retire-plan-owner ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((owner superchat-plan--execution)
             (a (plist-get owner :run))
             (a-request (car test-input-batch--requests))
             (manual (superchat--submit-chat-request
                      (list :prompt "MANUAL D" :buffer view :session-id session
                            :tools '("ask_user") :input-core (test-trusted-ask--config) :deadline 1)))
             (d (plist-get manual :run-id)))
        (test-trusted-ask--reply (car test-input-batch--requests))
        (plan-expiry-test--wait d)
        (should (eq 'unanswered (plist-get (superchat-runtime-status d) :input-continuation-blocked)))
        (should (superchat-plan--execution-current-p owner))
        (should (equal a (plist-get owner :run)))
        (should (= 0 superchat-plan--nudges))
        (should (= 2 (length test-input-batch--requests)))
        (test-input-batch--reply a-request nil)
        (should (= 3 (length test-input-batch--requests)))))))

(ert-deftest plan-expiry/pending-stop-preserves-cancelled-outcome ()
  (dolist (mode '(async streaming))
    (plan-expiry-test--fixture mode
      (superchat-plan-approve)
      (let ((run (plist-get superchat-plan--execution :run)))
        (test-trusted-ask--reply (car test-input-batch--requests))
        (should (superchat-runtime-cancel run "Explicit stop"))
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
        (should (equal "Explicit stop" (plist-get (superchat-runtime-status run) :stop-requested)))
        (should (eq 'unanswered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
        (should (= 1 (length test-input-batch--requests)))))))

(ert-deftest plan-expiry/same-stack-q2-audit-before-q1-return ()
  (dolist (mode '(async streaming))
    (plan-expiry-test--fixture mode
      (let ((config (symbol-function 'test-trusted-ask--config))
            (append-event (symbol-function 'superchat-tape-append-event-record))
            (receipts 0) q1 q2)
        (cl-letf (((symbol-function 'test-trusted-ask--config)
                   (lambda (&rest args)
                     (let ((value (apply config args)))
                       (plist-put value :presenter
                                  (lambda (q)
                                    (if q1 (setq q2 q) (setq q1 q))
                                    (superchat-runtime-answer-input
                                     (plist-get q :run-id) (plist-get q :handle) "answer")))))))
          (setq plan-owner-test--dispatch
                (lambda (request)
                  (when (= 2 (length test-input-batch--requests))
                    (test-trusted-ask--reply request "Which date?"))))
          (superchat-plan-approve))
        (let ((run (plist-get superchat-plan--execution :run)))
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event &rest args)
                       (when (and (equal run (plist-get event :run-id))
                                  (equal "tool.completed" (plist-get event :type)))
                         (cl-incf receipts)
                         (when (= receipts 2) (error "CURRENT Q2 AUDIT")))
                       (apply append-event event args))))
            (condition-case nil
                (test-trusted-ask--reply (car test-input-batch--requests))
              (error nil)))
          (should q1) (should q2)
          (should (= receipts 2))
          (should-not (eq (plist-get q1 :handle) (plist-get q2 :handle)))
          (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status)))
          (should (eq 'answer-not-delivered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))
          (should-not (superchat-runtime-pending-input run))
          (should (= 2 (length test-input-batch--requests)))
          (should (= 1 (length (plan-expiry-test--events run "tool.completed"))))
          (should-not (plan-expiry-test--events run "tool.failed"))
          (should (= 0 superchat-plan--nudges)))))))

(ert-deftest producer-ask/plan-real-default-factory ()
  (dolist (mode '(async streaming))
    (dolist (action '(answer stop expiry))
      (plan-owner-test--fixture mode
        (let ((superchat-runtime-default-deadline 1)
              (superchat-turn-completed-functions
               (cons #'superchat--notify-run-terminal superchat-turn-completed-functions))
              (draft (buffer-substring superchat--prompt-start (point-max)))
              (recall (list (list :content "MANUAL QUEUE"))))
          (setq-local superchat--pending-recalled-memories recall)
          (superchat-plan-approve)
          (let* ((run (plist-get superchat-plan--execution :run))
                 (request (car test-input-batch--requests)))
            (should (member "ask_user" (mapcar #'llm-tool-name
                                               (llm-chat-prompt-tools (plist-get request :prompt)))))
            (test-trusted-ask--reply request)
            (let* ((q (superchat-runtime-pending-input run))
                   (group (superchat--input-panel-find view session run (plist-get q :handle))))
              (should group)
              (should (eq 'plan (plist-get (widget-get group :input-snapshot) :presentation-scope)))
              (pcase action
                ('answer
                 (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Paris")))
                   (superchat--input-panel-action group 'answer))
                 (should (= 2 (length test-input-batch--requests)))
                 (test-input-batch--reply (car test-input-batch--requests) nil)
                 (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
                 (should (= 3 (length test-input-batch--requests))))
                ('stop
                 (superchat--input-panel-action group 'stop)
                 (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status))))
                ('expiry
                 (plan-expiry-test--wait run)
                 (should (eq 'timed-out (plist-get (superchat-runtime-status run) :status)))
                 (should (equal "Expired" (widget-value (nth 1 (widget-get group :children)))))
                 (should (eq 'unanswered (plist-get (superchat-runtime-status run) :input-continuation-blocked)))))
              (unless (eq action 'answer) (should (= 1 (length test-input-batch--requests)))))
            (should (eq recall superchat--pending-recalled-memories))
            (should (equal-including-properties draft
                      (buffer-substring superchat--prompt-start (point-max))))))))))

(ert-deftest plan-owner/native-approval-preserves-draft-and-recall ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (goto-char (+ superchat--prompt-start 4))
      (let* ((draft (buffer-substring superchat--prompt-start (point-max)))
             (marker superchat--prompt-start)
             (offset (- (point) marker))
             (recall (list (list :content "QUEUED PRIVATE RECALL")))
             tracked)
        (setq-local superchat--pending-recalled-memories recall)
        (setq plan-owner-test--dispatch (lambda (_) (setq tracked superchat--active-run-id)))
        (superchat-plan-approve)
        (should (= 1 (length test-input-batch--requests)))
        (should (stringp tracked))
        (should (eq marker superchat--prompt-start))
        (should (equal-including-properties draft (buffer-substring marker (point-max))))
        (should (= offset (- (point) marker)))
        (should (eq recall superchat--pending-recalled-memories))
        (let ((entry (superchat-runtime--entry tracked)))
          (should (eq 'ask-user (plist-get (plist-get entry :input-core) :operation)))
          (let ((prompt (superchat-turn-prompt (plist-get entry :turn))))
            (should (string-match-p "Execute the approved plan now" prompt))
            (should (string-match-p "Approved plan: owner plan" prompt))
            (should-not (string-match-p "PRIVATE draft\\|QUEUED PRIVATE RECALL" prompt))))
        (test-input-batch--reply (car test-input-batch--requests) nil)
        (should (= 2 (length test-input-batch--requests)))
        (should (equal-including-properties draft (buffer-substring marker (point-max))))
        (should (= offset (- (point) marker)))
        (should (eq recall superchat--pending-recalled-memories))))))

(ert-deftest plan-owner/native-nested-successors-stop-current ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let (runs)
        (setq plan-owner-test--dispatch
              (lambda (request)
                (push superchat--active-run-id runs)
                (when (< (length test-input-batch--requests) 3)
                  (test-input-batch--reply request nil))))
        (superchat-plan-approve)
        (should (= 3 (length test-input-batch--requests)))
        (should (cl-every #'stringp runs))
        (should (equal (car runs) superchat--active-run-id))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
          (call-interactively #'superchat-cancel))
        (should (eq 'cancelled (plist-get (superchat-runtime-status (car runs)) :status)))
        (should (= 3 (length test-input-batch--requests)))))))

(ert-deftest plan-owner/native-manual-terminal-does-not-nudge ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((owner superchat-plan--execution)
             (a (plist-get owner :run))
             (a-request (car test-input-batch--requests))
             (d (superchat--submit-chat-request
                 (list :prompt "MANUAL D" :buffer view :session-id session)))
             (d-request (car test-input-batch--requests)))
        (test-input-batch--reply d-request nil)
        (should (= 2 (length test-input-batch--requests)))
        (should (equal a (plist-get owner :run)))
        (should (= 0 superchat-plan--nudges))
        (should (eq 'completed (plist-get (superchat-runtime-status (plist-get d :run-id)) :status)))
        (test-input-batch--reply a-request nil)
        (should (= 3 (length test-input-batch--requests)))
        (should (= 1 superchat-plan--nudges))))))

(ert-deftest plan-owner/native-two-nudge-bound-and-closure ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (dotimes (_ 3) (test-input-batch--reply (car test-input-batch--requests) nil))
      (should (= 3 (length test-input-batch--requests)))
      (should (= 2 superchat-plan--nudges))
      (should-not superchat-plan--nudge-pending)
      (let ((events (superchat-tape-replay-events session)))
        (should (= 3 (cl-count "run.completed" events :key (lambda (e) (plist-get e :type)) :test #'equal)))
        (dolist (event events)
          (when (equal "run.completed" (plist-get event :type))
            (should (eq 'proved (plist-get (superchat-closure-judge
                                          (list :kind 'durable-run-terminal :run-id (plist-get event :run-id)
                                                :status 'completed) events) :verdict)))))))))

(ert-deftest plan-owner/native-failure-still-nudges-without-stop ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let ((a (plist-get superchat-plan--execution :run)))
        (funcall (plist-get (car test-input-batch--requests) :on-error) 'http-error "fixture failure")
        (should (eq 'failed (plist-get (superchat-runtime-status a) :status)))
        (should-not (plist-get (superchat-runtime-status a) :stop-requested))
        (should (= 2 (length test-input-batch--requests)))))))

(ert-deftest plan-owner/native-completed-stop-does-not-nudge ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((a (plist-get superchat-plan--execution :run))
             (append-event (symbol-function 'superchat-tape-append-event-record))
             stopped)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (and (not stopped) (equal "run.completed" (plist-get event :type)))
                         (setq stopped (superchat-runtime-cancel a "Stop after commit")))))))
          (test-input-batch--reply (car test-input-batch--requests) nil))
        (should stopped)
        (should (= 1 (length test-input-batch--requests)))
        (should (eq 'completed (plist-get (superchat-runtime-status a) :status)))
        (should (plist-get (superchat-runtime-status a) :stop-requested))))))

(defun plan-owner-test--approval-reentry (mode action)
  (plan-owner-test--fixture mode
    (let ((append-event (symbol-function 'superchat-tape-append-event-record))
          entered new-id)
      (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                 (lambda (event &rest args)
                   (prog1 (apply append-event event args)
                     (when (and (not entered) (equal "plan.approved" (plist-get event :type)))
                       (setq entered t)
                       (superchat-plan-clear)
                       (when (eq action 'reapprove)
                         (superchat-plan-propose '(("new" "new criterion")) "NEW PLAN")
                         (superchat-plan-approve)
                         (setq new-id (plist-get superchat--active-plan :event-id))))))))
        (superchat-plan-approve))
      (should entered)
      (should (= (if new-id 1 0) (length test-input-batch--requests)))
      (should (equal new-id (plist-get superchat--active-plan :event-id)))
      (when new-id (should (equal "NEW PLAN" (plist-get superchat--active-plan :summary))))
      (should (= (if new-id 2 1)
                 (cl-count "plan.approved" (superchat-tape-replay-events session)
                           :key (lambda (e) (plist-get e :type)) :test #'equal))))))

(ert-deftest plan-owner/native-approval-append-clear-wins ()
  (dolist (mode '(async streaming)) (plan-owner-test--approval-reentry mode 'clear)))

(ert-deftest plan-owner/native-approval-append-reapproval-wins ()
  (dolist (mode '(async streaming)) (plan-owner-test--approval-reentry mode 'reapprove)))

(defun plan-owner-test--extension-reentry (mode action)
  (plan-owner-test--fixture mode
    (let ((superchat-extension--registry nil)
          (append-event (symbol-function 'superchat-tape-append-event))
          entered current)
      (superchat-define-extension 'plan-fixture :description "Fixture" :context (lambda (_) "fixture context"))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (event &rest args)
                   (prog1 (apply append-event event args)
                     (when (and (not entered) (equal "extension.activated" (plist-get event :type)))
                       (setq entered t)
                       (if (eq action 'stop)
                           (superchat-plan--stop-execution superchat-plan--execution)
                         (superchat-plan-clear)
                         (superchat-plan-propose '(("new" "criterion")) "NEW")
                         (superchat-plan-approve)
                         (setq current (plist-get superchat-plan--execution :run))))))))
        (superchat-plan-approve))
      (should entered)
      (should (= (if current 1 0) (length test-input-batch--requests)))
      (should (= (if current 1 0)
                 (cl-count "run.started" (superchat-tape-replay-events session)
                           :key (lambda (e) (plist-get e :type)) :test #'equal)))
      (when current (should (equal current (plist-get superchat-plan--execution :run))))
      (should (cl-find "extension.activated" (superchat-tape-replay-events session)
                       :key (lambda (e) (plist-get e :type)) :test #'equal)))))

(ert-deftest plan-owner/native-extension-append-stop ()
  (dolist (mode '(async streaming)) (plan-owner-test--extension-reentry mode 'stop)))

(ert-deftest plan-owner/native-extension-append-reapproval ()
  (dolist (mode '(async streaming)) (plan-owner-test--extension-reentry mode 'reapprove)))

(ert-deftest plan-owner/native-elapsed-timeout-still-nudges-without-stop ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((superchat-runtime-default-deadline 0.2)) (superchat-plan-approve))
      (let ((a (plist-get superchat-plan--execution :run)))
        (should (= 1 (length test-input-batch--requests)))
        ;; Real timer dispatch may be delayed by queued native SSE work.  Wait
        ;; for the actual terminal, bounded independently; do not fire or mock
        ;; the Runtime timer and do not treat one sleep return as its delivery.
        (let ((limit (+ (float-time) 2)))
          (while (and (< (float-time) limit)
                      (eq 'running-model (plist-get (superchat-runtime-status a) :status)))
            (sleep-for 0.01)))
        (should (eq 'timed-out (plist-get (superchat-runtime-status a) :status)))
        (should-not (plist-get (superchat-runtime-status a) :stop-requested))
        (should (= 2 (length test-input-batch--requests)))
        (should (= 1 superchat-plan--nudges))))))

(ert-deftest plan-owner/native-entered-ancestor-stops-current-not-manual ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((a (plist-get superchat-plan--execution :run))
             (a-request (car test-input-batch--requests))
             (d (superchat--submit-chat-request
                 (list :prompt "MANUAL D" :buffer view :session-id session
                       :tools '("input_batch_effect"))))
             (d-request (car test-input-batch--requests))
             (nudge (symbol-function 'superchat-plan--nudge-on-completion))
             b c stopped)
        (setq plan-owner-test--dispatch
              (lambda (request)
                (if (= 3 (length test-input-batch--requests))
                    (progn (setq b superchat--active-run-id)
                           (test-input-batch--reply request nil))
                  (setq c superchat--active-run-id))))
        (cl-letf (((symbol-function 'superchat-plan--nudge-on-completion)
                   (lambda (status)
                     (funcall nudge status)
                     (when (equal a (plist-get status :run-id))
                       (setq stopped (superchat-runtime-cancel a "Stop entered ancestor"))))))
          (test-input-batch--reply a-request nil))
        (should stopped)
        (should b) (should c)
        (should (eq 'completed (plist-get (superchat-runtime-status a) :status)))
        (should (eq 'completed (plist-get (superchat-runtime-status b) :status)))
        (should (eq 'cancelled (plist-get (superchat-runtime-status c) :status)))
        (should (eq 'running-model (plist-get (superchat-runtime-status (plist-get d :run-id)) :status)))
        (setq plan-owner-test--dispatch nil)
        (test-input-batch--reply d-request '("input_batch_effect") "D effect")
        (should (equal '("D effect") test-input-batch--effects))
        (should-not (superchat-runtime-cancel a "Already ended"))))))

(defun plan-owner-test--view-reentry (mode after-track)
  (plan-owner-test--fixture mode
    (let ((append-event (symbol-function 'superchat-tape-append-event-record))
          (track (symbol-function 'superchat--track-active-run))
          changed run draft projection)
      (cl-labels ((replace-session ()
                    (unless changed
                      (setq changed t superchat--session-id "replacement-session")
                      (setq draft (buffer-substring (point-min) (point-max))
                            projection superchat--active-run-id))))
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (equal "run.started" (plist-get event :type))
                         (setq run (plist-get event :run-id))
                         (unless after-track (replace-session))))))
                  ((symbol-function 'superchat--track-active-run)
                   (lambda (handle)
                     (prog1 (funcall track handle)
                       (when after-track (replace-session))))))
          (superchat-plan-approve)))
      (should changed) (should run)
      (should-not test-input-batch--requests)
      (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
      (should (equal projection superchat--active-run-id))
      (should (equal-including-properties draft (buffer-substring (point-min) (point-max)))))))

(ert-deftest plan-owner/native-startup-view-reentry ()
  (dolist (mode '(async streaming)) (plan-owner-test--view-reentry mode nil)))
(ert-deftest plan-owner/native-tracking-view-reentry ()
  (dolist (mode '(async streaming)) (plan-owner-test--view-reentry mode t)))

(ert-deftest plan-owner/native-preparation-stop-prevents-submission ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((superchat-pre-topic-functions
             (list (lambda (_) (superchat-plan--stop-execution superchat-plan--execution)))))
        (superchat-plan-approve))
      (should-not test-input-batch--requests)
      (should (= 0 (hash-table-count superchat-runtime--runs))))))

(ert-deftest plan-owner/native-tool-result-then-successor ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let ((a (plist-get superchat-plan--execution :run)))
        (test-input-batch--reply (car test-input-batch--requests) '("input_batch_effect") "Plan effect")
        (should (equal '("Plan effect") test-input-batch--effects))
        (should (equal a (plist-get superchat-plan--execution :run)))
        (test-input-batch--reply (car test-input-batch--requests) nil)
        (should (= 3 (length test-input-batch--requests)))
        (should-not (equal a (plist-get superchat-plan--execution :run)))
        (let ((events (superchat-tape-replay-events session)))
          (should (cl-find "tool.completed" events :key (lambda (e) (plist-get e :type)) :test #'equal))
          (should (eq 'proved (plist-get (superchat-closure-judge
                                        (list :kind 'durable-run-terminal :run-id a) events) :verdict))))))))

(ert-deftest plan-owner/native-approval-append-primary-error-and-quit ()
  (dolist (mode '(async streaming))
    (dolist (kind '(error quit))
      (plan-owner-test--fixture mode
        (let ((append-event (symbol-function 'superchat-tape-append-event-record)) caught)
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event &rest args)
                       (prog1 (apply append-event event args)
                         (when (equal "plan.approved" (plist-get event :type))
                           (signal kind '("approval primary")))))))
            (condition-case condition (superchat-plan-approve)
              ((error quit) (setq caught condition))))
          (should (equal caught (list kind "approval primary")))
          (should-not test-input-batch--requests)
          (should-not superchat--active-plan)
          (should (= 1 (cl-count "plan.approved" (superchat-tape-replay-events session)
                                 :key (lambda (e) (plist-get e :type)) :test #'equal))))))))

(ert-deftest plan-owner/native-tracking-error-and-quit-fail-closed ()
  (dolist (mode '(async streaming))
    (dolist (kind '(error quit))
      (plan-owner-test--fixture mode
        (cl-letf (((symbol-function 'superchat--track-active-run)
                   (lambda (_) (signal kind '("tracking primary")))))
          ;; Runtime retains its existing condition/evidence boundary.
          (condition-case nil (superchat-plan-approve) ((error quit) nil)))
        (should-not test-input-batch--requests)
        (maphash (lambda (run _)
                   (should-not (eq 'running-model (plist-get (superchat-runtime-status run) :status))))
                 superchat-runtime--runs)))))

(ert-deftest plan-owner/generated-preparation-retains-agent-preset-and-hooks ()
  "Preparation unit control: real hooks/preset, no Runtime submission."
  (plan-owner-test--fixture 'async
    (let* ((recall (list (list :content "QUEUED MANUAL")))
           (superchat--active-preset
            (superchat-preset--create :name "producer-agent" :type 'agent
                                     :model "fixture-model" :tools '("input_batch_effect")
                                     :skill-body "EXACT AGENT PERSONA"))
           (superchat--current-context-files '("fixture-context"))
           (superchat--current-command "fixture-command")
           pre post template-called)
      (setq-local superchat--pending-recalled-memories recall)
      (let ((superchat-pre-topic-functions (list (lambda (_) (setq pre t))))
            (superchat-post-topic-functions (list (lambda (turn) (setq post t) turn))))
        (cl-letf (((symbol-function 'superchat--lookup-command-template)
                   (lambda (name) (setq template-called name) "GENERATED TEMPLATE: %s")))
          (let ((request (superchat--prepare-generated-request "EXPLICIT INSTRUCTION")))
            (should pre) (should post)
            (should (equal "fixture-command" template-called))
            (should (eq superchat--active-preset (plist-get request :preset)))
            (should (equal "fixture-model" (plist-get request :target-model)))
            (should (string-match-p "EXACT AGENT PERSONA" (plist-get request :system-prompt)))
            (should (member "input_batch_effect" (plist-get request :tools)))
            (should (member "fixture-context" (plist-get request :context-files)))
            (should-not (string-match-p "QUEUED MANUAL\\|PRIVATE draft" (plist-get request :prompt)))
            (should (eq recall superchat--pending-recalled-memories))
            (should-not (plist-get request :consume-pending-memories))
            (should-not test-input-batch--requests)))))))

(ert-deftest plan-owner/reentrant-manual-send-consumes-its-own-recall ()
  "Automatic exclusion must not mask a nested real ordinary producer."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let* ((recall (list (list :content "EXACT QUEUED RECALL")))
             (attach (symbol-function 'superchat--attach-turn-memories))
             entered generated-turn manual-memories manual-return-queue)
        (setq-local superchat--pending-recalled-memories recall)
        (let ((superchat-pre-topic-functions
               (list (lambda (turn)
                       (unless entered
                         (setq entered t generated-turn turn)
                         (let ((superchat-pre-topic-functions nil))
                           (call-interactively #'superchat-send-input))
                         (setq manual-return-queue superchat--pending-recalled-memories))))))
          (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                     (lambda (turn input sid &optional consume)
                       (prog1 (funcall attach turn input sid consume)
                         (when consume
                           (setq manual-memories (superchat-turn-retrieved-memories turn)))))))
            (superchat-plan-approve)))
        (should entered)
        (should (= 2 (length test-input-batch--requests)))
        (should (equal recall manual-memories))
        (should-not manual-return-queue)
        (should-not superchat--pending-recalled-memories)
        (should-not (superchat-turn-retrieved-memories generated-turn))))))

(ert-deftest plan-owner/later-manual-send-recall-control ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let* ((recall (list (list :content "EXACT QUEUED RECALL")))
             (attach (symbol-function 'superchat--attach-turn-memories)) manual-memories)
        (setq-local superchat--pending-recalled-memories recall)
        (superchat-plan-approve)
        (should (eq recall superchat--pending-recalled-memories))
        (should (superchat-runtime-cancel (plist-get superchat-plan--execution :run) "Manual control"))
        (cl-letf (((symbol-function 'superchat--attach-turn-memories)
                   (lambda (turn input sid &optional consume)
                     (prog1 (funcall attach turn input sid consume)
                       (when consume
                         (setq manual-memories (superchat-turn-retrieved-memories turn)))))))
          (call-interactively #'superchat-send-input))
        (should (= 2 (length test-input-batch--requests)))
        (should (equal recall manual-memories))
        (should-not superchat--pending-recalled-memories)))))

(ert-deftest plan-owner/clear-append-owned-rollover-preserves-current-state ()
  "Successful rollover permits NEW; the outer old Plan clear preserves it."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((append-event (symbol-function 'superchat-tape-append-event-record))
            (original-proposed superchat-plan--proposed)
            entered observed-proposal new-session proposal-result)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (and (not entered) (equal "plan.cleared" (plist-get event :type)))
                         (setq entered t)
                         (with-current-buffer view
                           (call-interactively #'superchat--clear-chat-and-context)
                           (setq proposal-result
                                 (superchat-plan-propose '(("New question" "New criterion")) "NEW PROPOSAL"))
                           (setq observed-proposal superchat-plan--proposed
                                 new-session superchat--session-id)))))))
          (superchat-plan-clear))
        (should entered)
        (should-not (equal session new-session))
        (should (string-match-p "Plan proposed:" proposal-result))
        (should-not (eq original-proposed observed-proposal))
        (should-not (equal (plist-get original-proposed :event-id)
                           (plist-get observed-proposal :event-id)))
        (should (equal "NEW PROPOSAL" (plist-get observed-proposal :summary)))
        (should (eq observed-proposal superchat-plan--proposed))
        (should-not test-input-batch--requests)
        (should (= 1 (cl-count "plan.cleared" (superchat-tape-replay-events session)
                               :key (lambda (e) (plist-get e :type)) :test #'equal)))))))

(ert-deftest plan-owner/clear-append-same-session-new-proposal-survives ()
  "Explicit read-model replacement then public propose, without a new generation."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((append-event (symbol-function 'superchat-tape-append-event-record))
            (original-proposed superchat-plan--proposed)
            entered new-proposed generation proposal-result)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (and (not entered) (equal "plan.cleared" (plist-get event :type)))
                         (setq entered t generation superchat-plan--approval-generation)
                         ;; Model the View having consumed the committed clear.
                         ;; Unlike the rollover test this is a direct read-model
                         ;; boundary, not a claimed public-only View reset.
                         (setq superchat-plan--proposed nil)
                         (setq proposal-result
                               (superchat-plan-propose '(("New" "Criterion")) "SAME SESSION NEW"))
                         (setq new-proposed superchat-plan--proposed))))))
          (superchat-plan-clear))
        (should entered) (should new-proposed)
        (should (string-match-p "Plan proposed:" proposal-result))
        (should-not (eq original-proposed new-proposed))
        (should-not (equal (plist-get original-proposed :event-id)
                           (plist-get new-proposed :event-id)))
        (should (equal "SAME SESSION NEW" (plist-get new-proposed :summary)))
        (should (equal session superchat--session-id))
        (should (eq generation superchat-plan--approval-generation))
        (should (eq new-proposed superchat-plan--proposed))
        (should-not test-input-batch--requests)))))

(ert-deftest plan-owner/clear-append-new-session-explicit-new-target ()
  "Public rollover then NEW proposal survives the older Plan clear frame."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((append-event (symbol-function 'superchat-tape-append-event-record))
            (original-proposed superchat-plan--proposed)
            entered new-proposed proposal-result)
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event &rest args)
                     (prog1 (apply append-event event args)
                       (when (and (not entered) (equal "plan.cleared" (plist-get event :type)))
                         (setq entered t)
                         (call-interactively #'superchat--clear-chat-and-context)
                         (setq proposal-result
                               (superchat-plan-propose '(("New" "Criterion")) "NEW SESSION NEW")
                               new-proposed superchat-plan--proposed))))))
          (superchat-plan-clear))
        (should entered)
        (should-not (equal session superchat--session-id))
        (should (string-match-p "Plan proposed:" proposal-result))
        (should-not (eq original-proposed new-proposed))
        (should-not (equal (plist-get original-proposed :event-id)
                           (plist-get new-proposed :event-id)))
        (should (equal "NEW SESSION NEW" (plist-get new-proposed :summary)))
        (should (eq new-proposed superchat-plan--proposed))
        (should (= 1 (cl-count "plan.cleared" (superchat-tape-replay-events session)
                               :key (lambda (e) (plist-get e :type)) :test #'equal)))
        (should-not test-input-batch--requests)))))

(ert-deftest plan-rollover/public-new-preserves-history ()
  (dolist (mode '(async streaming))
    (dolist (borrowed '(nil t))
      (plan-owner-test--fixture mode
        (when borrowed
          (kill-local-variable 'superchat--editor-workspace-session-id)
          (kill-local-variable 'superchat--editor-workspace-owned-p))
        (let ((old superchat-plan--proposed)
              (history (superchat-tape-replay-events session)))
          (should (eq 'cleared (call-interactively #'superchat--clear-chat-and-context)))
          (should-not (equal session superchat--session-id))
          (should-not superchat-plan--proposed)
          (should (string-match-p "Plan proposed:"
                                  (superchat-plan-propose '(("new" "done")) "NEW")))
          (should-not (eq old superchat-plan--proposed))
          (should-not (equal (plist-get old :event-id)
                             (plist-get superchat-plan--proposed :event-id)))
          (should (equal "NEW" (plist-get superchat-plan--proposed :summary)))
          (should (equal history (superchat-tape-replay-events session)))
          (should (cl-find (plist-get superchat-plan--proposed :event-id)
                           (superchat-tape-replay-events superchat--session-id)
                           :key (lambda (e) (plist-get e :event-id)) :test #'equal))
          (should-not test-input-batch--requests))))))

(ert-deftest plan-rollover/rejected-and-reset-failure ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let ((active superchat--active-plan)
            (owner superchat-plan--execution)
            (text (buffer-string)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
          (should (eq 'rejected (superchat--clear-chat-and-context))))
        (should (eq active superchat--active-plan))
        (should (eq owner superchat-plan--execution))
        (should-not (plist-get owner :stopped))
        (should (equal text (buffer-string)))
        (dolist (condition '(error quit))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'superchat--response-region-reset)
                     (lambda () (signal condition '("reset failure")))))
            (should (equal (condition-case err (superchat--clear-chat-and-context)
                             ((error quit) err))
                           (list condition "reset failure"))))
          (should (equal session superchat--session-id))
          (should (eq active superchat--active-plan))
          (should (eq owner superchat-plan--execution)))
        ;; Cancellation preceded the failed transaction and is not undone.
        (should (eq 'cancelled (plist-get (superchat-runtime-status
                                         (plist-get owner :run)) :status)))))))

(ert-deftest plan-rollover/borrowed-stop-reentry-new-survives ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((owner superchat-plan--execution)
             (run (plist-get owner :run))
             (stop (symbol-function 'superchat-runtime-cancel))
             new result)
        (kill-local-variable 'superchat--editor-workspace-session-id)
        (kill-local-variable 'superchat--editor-workspace-owned-p)
        (cl-letf (((symbol-function 'superchat-runtime-cancel)
                   (lambda (id &optional reason)
                     (prog1 (funcall stop id reason)
                       (when (equal id run)
                         (should-not (equal session superchat--session-id))
                         (should-not superchat--active-plan)
                         (setq result (superchat-plan-propose '(("new" "done")) "REENTRANT")
                               new superchat-plan--proposed))))))
          (superchat--clear-chat-and-context))
        (should (string-match-p "Plan proposed:" result))
        (should (eq new superchat-plan--proposed))
        (should (equal "REENTRANT" (plist-get new :summary)))
        (should (plist-get owner :stopped))
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
        (should (= 1 (length test-input-batch--requests)))))))

(ert-deftest plan-rollover/owned-active-success-late-isolated ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((owner superchat-plan--execution)
             (a (plist-get owner :run))
             (request (car test-input-batch--requests)))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (should (eq 'cleared (superchat--clear-chat-and-context))))
        (should-not superchat--active-plan)
        (should-not superchat-plan--execution)
        (should (plist-get owner :stopped))
        (should (eq 'cancelled (plist-get (superchat-runtime-status a) :status)))
        (should (string-match-p "Plan proposed:"
                                (superchat-plan-propose '(("new" "done")) "AFTER ACTIVE")))
        (let ((new superchat-plan--proposed) (text (buffer-string)))
          (test-input-batch--reply request nil)
          (should (eq new superchat-plan--proposed))
          (should (equal text (buffer-string))))
        (should (= 1 (length test-input-batch--requests)))
        (should-not (cl-find "plan.cleared" (superchat-tape-replay-events session)
                             :key (lambda (e) (plist-get e :type)) :test #'equal))))))

(ert-deftest plan-rollover/borrowed-unrelated-run-survives ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let* ((a (plist-get superchat-plan--execution :run))
             (d (superchat--submit-chat-request
                 (list :prompt "MANUAL D" :buffer view :session-id session
                       :tools '("input_batch_effect"))))
             (request (car test-input-batch--requests)))
        (kill-local-variable 'superchat--editor-workspace-session-id)
        (kill-local-variable 'superchat--editor-workspace-owned-p)
        (superchat--clear-chat-and-context)
        (should (eq 'cancelled (plist-get (superchat-runtime-status a) :status)))
        (should (eq 'running-model (plist-get (superchat-runtime-status
                                              (plist-get d :run-id)) :status)))
        (test-input-batch--reply request nil)
        (should (eq 'completed (plist-get (superchat-runtime-status
                                          (plist-get d :run-id)) :status)))
        (should (= 2 (length test-input-batch--requests)))))))

(ert-deftest plan-rollover/publication-error-quit-preserves-projection ()
  (dolist (mode '(async streaming))
    (dolist (condition '(error quit))
      (plan-owner-test--fixture mode
        (let ((old superchat-plan--proposed)
              (text (buffer-string))
              (watcher (lambda (_ value operation where)
                         (when (and (eq operation 'set) (eq where view)
                                    (not (equal value session)))
                           (signal condition '("publication failure"))))))
          (unwind-protect
              (progn
                (add-variable-watcher 'superchat--session-id watcher)
                (should (equal (condition-case err (superchat--clear-chat-and-context)
                                 ((error quit) err))
                               (list condition "publication failure"))))
            (remove-variable-watcher 'superchat--session-id watcher))
          (should (equal session superchat--session-id))
          (should (eq old superchat-plan--proposed))
          (should (equal text (buffer-string))))))))

(ert-deftest plan-rollover/replaced-projection-wins-at-handoff ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (let ((capture (symbol-function 'superchat-plan--capture-view-retirement))
            new result)
        (cl-letf (((symbol-function 'superchat-plan--capture-view-retirement)
                   (lambda ()
                     (let ((retire (funcall capture)))
                       (lambda (sid)
                         ;; Inject at the committed handoff, not publication.
                         (setq superchat-plan--proposed nil)
                         (setq result (superchat-plan-propose '(("new" "done")) "REPLACEMENT")
                               new superchat-plan--proposed)
                         (funcall retire sid))))))
          (superchat--clear-chat-and-context))
        (should (string-match-p "Plan proposed:" result))
        (should (eq new superchat-plan--proposed))
        (should (equal "REPLACEMENT" (plist-get new :summary)))))))

(ert-deftest plan-rollover/exact-target-and-rename ()
  (dolist (change '(rename session dead))
    (plan-owner-test--fixture 'async
      (let ((capture (symbol-function 'superchat-plan--capture-view-retirement))
            (old superchat-plan--proposed)
            replacement)
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'superchat-plan--capture-view-retirement)
                         (lambda ()
                           (let ((retire (funcall capture)))
                             (lambda (sid)
                               (pcase change
                                 ('rename (rename-buffer (generate-new-buffer-name " *renamed-plan*")))
                                 ('session (setq superchat--session-id "replacement-session"))
                                 ('dead
                                  (let ((name (buffer-name view)))
                                    (kill-buffer view)
                                    (setq replacement (get-buffer-create name))
                                    (with-current-buffer replacement
                                      (setq-local superchat--session-id sid
                                                  superchat-plan--proposed old)))))
                               (funcall retire sid))))))
                (superchat--clear-chat-and-context))
              (pcase change
                ('rename (with-current-buffer view (should-not superchat-plan--proposed)))
                ('session (with-current-buffer view (should (eq old superchat-plan--proposed))))
                ('dead (with-current-buffer replacement (should (eq old superchat-plan--proposed))))))
          (when (buffer-live-p replacement) (kill-buffer replacement)))))))

(ert-deftest plan-rollover/postcommit-stop-error-quit-keeps-new-session ()
  (dolist (condition '(error quit))
    (plan-owner-test--fixture 'async
      (superchat-plan-approve)
      (let ((stop (symbol-function 'superchat-plan--stop-execution))
            (owner superchat-plan--execution))
        (kill-local-variable 'superchat--editor-workspace-session-id)
        (kill-local-variable 'superchat--editor-workspace-owned-p)
        (cl-letf (((symbol-function 'superchat-plan--stop-execution)
                   (lambda (&rest args)
                     (prog1 (apply stop args)
                       (when (eq (car args) owner)
                         (signal condition '("postcommit stop failure")))))))
          (should (equal (condition-case err (superchat--clear-chat-and-context)
                           ((error quit) err))
                         (list condition "postcommit stop failure"))))
        (should-not (equal session superchat--session-id))
        (should-not superchat--active-plan)
        (should-not superchat-plan--execution)
        (should (plist-get owner :stopped))))))

(ert-deftest plan-rollover/shared-panel-refresh-never-close ()
  (plan-owner-test--fixture 'async
    (let ((panel (generate-new-buffer " *plan-panel-probe*"))
          (foreign (generate-new-buffer " *foreign-plan-view*"))
          new refreshed)
      (unwind-protect
          (let ((superchat-plan--view-buffer (buffer-name panel)))
            (with-current-buffer panel (setq major-mode 'textui-mode))
            (cl-letf (((symbol-function 'superchat-plan--view-close)
                       (lambda () (ert-fail "Rollover must not close shared panel")))
                      ((symbol-function 'textui-request-refresh)
                       (lambda (buffer)
                         (should (eq buffer panel))
                         (setq refreshed t)
                         (with-current-buffer view
                           (should (string-match-p "Plan proposed:"
                                                   (superchat-plan-propose '(("new" "done")) "PANEL NEW")))
                           (setq new superchat-plan--proposed))))
                      ((symbol-function 'superchat-plan--view-available-p) (lambda () nil)))
              (superchat--clear-chat-and-context)
              (should refreshed)
              (should (eq new superchat-plan--proposed))
              (setq refreshed nil)
              (cl-letf (((symbol-function 'superchat-plan--chat-buffer) (lambda () foreign)))
                (superchat--clear-chat-and-context))
              (should-not refreshed)
              (should (buffer-live-p panel))))
        (kill-buffer panel) (kill-buffer foreign)))))

(ert-deftest plan-owner/clear-normal-target-control ()
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-clear)
      (should-not superchat-plan--proposed)
      (should-not superchat--active-plan)
      (should-not test-input-batch--requests)
      (should (= 1 (cl-count "plan.cleared" (superchat-tape-replay-events session)
                             :key (lambda (e) (plist-get e :type)) :test #'equal))))))

(ert-deftest plan-owner/clear-stop-reentry-keeps-new-active-target ()
  "Real cancellation hooks may replace the active read-model before cleanup."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (let ((a (plist-get superchat-plan--execution :run))
            (new-active (list :event-id "explicit-new-active" :summary "New active target"))
            generation entered)
        (let ((superchat-turn-completed-functions
               (list (lambda (status)
                       (when (equal a (plist-get status :run-id))
                         (setq entered t generation superchat-plan--approval-generation
                               superchat--active-plan new-active))))))
          (superchat-plan-clear))
        (should entered)
        (should (eq generation superchat-plan--approval-generation))
        (should (eq new-active superchat--active-plan))
        (should (eq 'cancelled (plist-get (superchat-runtime-status a) :status)))
        (should (= 1 (length test-input-batch--requests)))))))

(ert-deftest plan/emit-tool-normalizes-provider-vectors ()
  "The tool adapter turns JSON arrays into the lists the plan domain expects."
  (let (received)
    (cl-letf (((symbol-function 'superchat-plan-propose)
               (lambda (steps summary)
                 (setq received (list steps summary))
                 "ok")))
      (should
       (equal "ok"
              (superchat-tool-emit-plan
               [["read" "file inspected"] ["plan" "steps verified"]]
               "demo")))
      (should
       (equal '((("read" "file inspected")
                 ("plan" "steps verified"))
                "demo")
              received)))))

(ert-deftest plan/propose-records-event-and-state ()
  "Proposing records plan.proposed and stores steps checked by default."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (let ((result
                   (superchat-plan-propose
                    '(("step one" "criterion one")
                      ("step two" "criterion two"))
                    "demo plan")))
              (should (string-match-p "Plan proposed: 2 steps" result)))
            ;; the review state lives in the session buffer as data, with
            ;; every step checked by default (user unchecks to drop)
            (with-current-buffer superchat-buffer-name
              (should superchat-plan--proposed)
              (let ((steps (plist-get superchat-plan--proposed :steps)))
                (should (= 2 (length steps)))
                (should (eq 1 (plist-get (nth 0 steps) :id)))
                (should (eq t (plist-get (nth 0 steps) :checked)))
                (should (equal "criterion one"
                               (plist-get (nth 0 steps) :accept)))))
            ;; the tape has the proposed event
            (let* ((sessions (superchat-view-session-ids))
                   (events (superchat-experience--view-events sessions)))
              (should
               (cl-some
                (lambda (e)
                  (equal "plan.proposed" (plist-get e :type)))
                events))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-records-included-steps ()
  "Approval marks checked steps included and appends plan.approved."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-approve-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")
               ("step two" "criterion two"))
             "demo")
            ;; uncheck the first step in the review data (the widget
            ;; notify path writes the same field)
            (with-current-buffer superchat-buffer-name
              (setf (plist-get (nth 0 (plist-get
                                       superchat-plan--proposed :steps))
                               :checked)
                    nil))
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (with-current-buffer superchat-buffer-name
              (should superchat--active-plan)
              (should (= 1 (cl-count t
                                     (plist-get superchat--active-plan :steps)
                                     :key (lambda (s) (plist-get s :included))
                                     :test #'eq))))
            (let* ((sessions (superchat-view-session-ids))
                   (events (superchat-experience--view-events sessions)))
              (should
               (cl-some
                (lambda (e)
                  (equal "plan.approved" (plist-get e :type)))
                events))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/prompt-hook-injects-active-plan ()
  "The approved plan reaches the execution context, and only then."
  (let ((superchat--active-plan nil)
        (turn (superchat-turn-new "do the work")))
    (setf (superchat-turn-prompt turn) "base task")
    (superchat-prompt-hook--active-plan turn)
    (should (equal "base task" (superchat-turn-prompt turn)))
    (setq superchat--active-plan
          (list :summary "demo"
                :steps '((:id 1 :text "step one" :accept "criterion one"
                          :included t))))
    (superchat-prompt-hook--active-plan turn)
    (should (string-match-p "Approved plan: demo"
                            (superchat-turn-prompt turn)))
    (should (string-match-p "step one"
                            (superchat-turn-prompt turn)))
    (should (string-match-p "Do not propose a new plan"
                            (superchat-turn-prompt turn)))))

(ert-deftest plan/file-ref-range-parses-and-slices ()
  "The # prefix accepts :START and :START-END line ranges."
  (let ((file (make-temp-file "range-test-" nil ".txt"
                              "l1\nl2\nl3\nl4\nl5\n"))
        (superchat-inline-max-bytes 100000))
    (unwind-protect
        (progn
          (should (equal (cons 2 4)
                         (superchat-parser-extract-file-range
                          (format "#%s:2-4" file))))
          (should (equal (cons 7 7)
                         (superchat-parser-extract-file-range
                          (format "#%s:7" file))))
          (should-not (superchat-parser-extract-file-range
                       (format "#%s" file)))
          (superchat--extract-file-path (format "#%s:2-4" file))
          (should (equal "l2\nl3\nl4"
                         (superchat--read-inline-file-content file))))
      (delete-file file))))

(ert-deftest plan/propose-rejects-when-plan-active ()
  "A second proposal is declined while a plan exists: one plan, one review."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-uniq-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (setq superchat--active-plan
                    '(:summary "old" :event-id "e" :steps nil)))
            (let ((result
                   (superchat-plan-propose
                    '(("step one" "criterion one")) "demo")))
              (should (string-match-p "declined" result)))
            (with-current-buffer superchat-buffer-name
              (should-not superchat-plan--proposed)))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-auto-executes-when-steps-included ()
  "Approving with checked steps sends the execution follow-up."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-auto-test*"))
           (superchat--session-id "plan-session")
           sent)
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (insert "\n* User: ")
              (setq superchat--prompt-start (point-marker)))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore)
                      ((symbol-function 'superchat-plan--submit-instruction)
                       (lambda (_owner instruction) (setq sent instruction))))
              (superchat-plan-approve))
            (should (equal "Execute the approved plan now." sent))
            (with-current-buffer superchat-buffer-name
              (should (equal "" (buffer-substring superchat--prompt-start (point-max))))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-exits-planning-mode ()
  "Approval clears a plan preset so execution runs without the persona."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-exit-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1)
              (insert "\n* User: ")
              (setq superchat--prompt-start (point-marker))
              (setq superchat--active-preset
                    (superchat-preset-from-frontmatter
                     "---\nname: ptest\ntype: plan\nversion: 1\n---\nbody")))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore)
                      ((symbol-function 'superchat-plan--submit-instruction)
                       (lambda (&rest _) nil)))
              (superchat-plan-approve))
            (with-current-buffer superchat-buffer-name
              (should-not superchat--active-preset)))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/approve-with-nothing-checked-rejects ()
  "Approving with zero checked steps keeps the review open."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-zero-approve*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            ;; proposal pre-checks every step; simulate the user
            ;; unchecking all of them before approving
            (with-current-buffer superchat-buffer-name
              (setf (plist-get (nth 0 (plist-get
                                      superchat-plan--proposed :steps))
                               :checked)
                    nil))
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (with-current-buffer superchat-buffer-name
              (should superchat-plan--proposed)
              (should-not (and (boundp 'superchat--active-plan)
                               superchat--active-plan))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(defun test-plan--frame-elements (frame)
  "Return every element nested in FRAME, flattened."
  (let ((els nil))
    (cl-labels ((walk (el)
                  (push el els)
                  (dolist (child (plist-get el :children))
                    (walk child))))
      (dolist (el frame) (walk el)))
    (nreverse els)))

(defun test-plan--frame-values (frame)
  "Return every string :value nested in FRAME."
  (cl-remove-if-not
   #'stringp
   (mapcar (lambda (el) (plist-get el :value))
           (test-plan--frame-elements frame))))

(ert-deftest plan/view-frame-carries-stateful-controls ()
  "The review frame projects the plan data into stateful checkbox controls,
and the widget notify path syncs the value back into the data."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-view-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (let* ((frame (superchat-plan--view-frame 24))
                   (els (test-plan--frame-elements frame))
                   (values (test-plan--frame-values frame))
                   (step-el
                    (cl-find 'superchat-plan-step-checkbox els
                             :key (lambda (el) (plist-get el :type))))
                   (approve-el
                    (cl-find 'push-button els
                             :key (lambda (el) (plist-get el :type)))))
              (should step-el)
              (should (eq t (plist-get step-el :value)))
              (should (eq 1 (plist-get step-el :step-id)))
              (should (eq #'superchat-plan--view-action
                          (plist-get approve-el :notify)))
              (should (eq :approve (plist-get approve-el :plan-action)))
              (should (eq widget-keymap (plist-get approve-el :keymap)))
              ;; the hybrid card layering: header box present, no
              ;; markdown artifacts, acceptance line shortened
              (should (cl-find "Plan: demo" values :test #'string=))
              (should (cl-some (lambda (v)
                                 (string-match-p "» criterion one" v))
                               values))
              (should-not (cl-some (lambda (v)
                                     (string-match-p "\\*\\*\|done when:" v))
                                   values))
              ;; render-level smoke: the frame must pass TextUI's
              ;; measure/validate pass (catches :type symbol-vs-keyword
              ;; mistakes that batch widget tests would miss)
              (when (fboundp 'textui--prepare-frame)
                (should (textui--prepare-frame frame)))
              ;; the notify path (as fired by toggling the control)
              ;; unchecks the step in the plan data — every control
              ;; routes through the one interaction seam
              (superchat-plan--view-action
               (widget-convert 'superchat-plan-step-checkbox
                               :value nil :step-id 1))
              (with-current-buffer superchat-buffer-name
                (should-not
                 (plist-get (nth 0 (plist-get
                                    superchat-plan--proposed :steps))
                            :checked)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/reopen-restores-unresolved-review ()
  "Rebuilding a session View restores an unapproved plan's review block."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-reopen-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-reopen-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            ;; a fresh View buffer simulates the reopened session: no
            ;; local plan state, same session id
            (let ((superchat-buffer-name fresh-buffer-name))
              (with-current-buffer (get-buffer-create fresh-buffer-name)
                (erase-buffer)
                (superchat-mode 1)
                (setq-local superchat--session-id "plan-session")
                (superchat-plan--restore-on-reopen "plan-session")
                (should superchat-plan--proposed)
                (should
                 (eq t (plist-get
                        (nth 0 (plist-get
                                superchat-plan--proposed :steps))
                        :checked)))
                ;; the restored review is the review frame again
                (should
                 (cl-find 'superchat-plan-step-checkbox
                          (plist-get (car (superchat-plan--view-frame 24))
                                     :children)
                          :key (lambda (el) (plist-get el :type)))))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/reopen-skips-resolved-review ()
  "An approved plan does not resurrect its review on a rebuilt View."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-reopen-skip-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-reopen-skip-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((superchat-buffer-name fresh-buffer-name))
              (with-current-buffer (get-buffer-create fresh-buffer-name)
                (erase-buffer)
                (superchat-mode 1)
                (setq-local superchat--session-id "plan-session")
                (superchat-plan--restore-on-reopen "plan-session")
                (should-not superchat-plan--proposed))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/clear-appends-resolved-event ()
  "Clearing a plan under review appends plan.cleared, so a rebuilt View
stays clear."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-clear-event-test*"))
           (fresh-buffer-name
            (generate-new-buffer-name "*plan-clear-event-fresh*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (let ((proposed-id
                   (with-current-buffer superchat-buffer-name
                     (plist-get superchat-plan--proposed :event-id))))
              (with-current-buffer superchat-buffer-name
                (superchat-plan-clear))
              (let* ((sessions (superchat-view-session-ids))
                     (events (superchat-experience--view-events sessions)))
                (should
                 (cl-some
                  (lambda (e)
                    (and (equal "plan.cleared" (plist-get e :type))
                         (equal proposed-id
                                (plist-get
                                 (plist-get (plist-get e :payload)
                                            :proposed)
                                 :event-id))))
                  events)))
              (let ((superchat-buffer-name fresh-buffer-name))
                (with-current-buffer (get-buffer-create fresh-buffer-name)
                  (erase-buffer)
                  (superchat-mode 1)
                  (setq-local superchat--session-id "plan-session")
                  (superchat-plan--restore-on-reopen "plan-session")
                  (should-not superchat-plan--proposed)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer fresh-buffer-name)
          (kill-buffer fresh-buffer-name))))))

(ert-deftest plan/report-progress-updates-state-and-tape ()
  "report_plan_progress marks step status and records plan.progress."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")
               ("step two" "criterion two"))
             "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((result
                   (superchat-plan-report-progress
                    '((1 "done") (2 "in-progress")))))
              (should (string-prefix-p "Progress recorded" result))
              (with-current-buffer superchat-buffer-name
                (should
                 (equal "done"
                        (plist-get
                         (nth 0 (plist-get superchat--active-plan :steps))
                         :status)))
                (should
                 (equal "in-progress"
                        (plist-get
                         (nth 1 (plist-get superchat--active-plan :steps))
                         :status))))
              (let* ((sessions (superchat-view-session-ids))
                     (events (superchat-experience--view-events sessions)))
                (should
                 (cl-some
                  (lambda (e)
                    (and (equal "plan.progress" (plist-get e :type))
                         (equal 1 (plist-get (plist-get e :payload)
                                             :step-id))
                         (equal "done"
                                (plist-get (plist-get e :payload)
                                           :status))))
                  events)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/report-progress-rejects-invalid ()
  "Progress updates validate shape, statuses, and step membership."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-reject-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            ;; no active plan
            (should
             (string-prefix-p
              "Error: no active plan"
              (superchat-plan-report-progress '((1 "done")))))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (should
             (string-prefix-p
              "Error: update"
              (superchat-plan-report-progress '((1 "almost")))))
            (should
             (string-prefix-p
              "Error: step 7 is not part"
              (superchat-plan-report-progress '((7 "done")))))
            (should
             (string-prefix-p
              "Error: updates must be"
              (superchat-plan-report-progress "done"))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/progress-block-renders-and-updates ()
  "Approval converts the review block into a progress view that updates."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-progress-block-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")
               ("step two" "criterion two"))
             "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            ;; the review frame is consumed: the view now projects the
            ;; progress frame with status markers
            (let* ((els (test-plan--frame-elements
                         (superchat-plan--view-frame 24)))
                   (values (test-plan--frame-values
                            (superchat-plan--view-frame 24))))
              (should-not
               (cl-find 'push-button els
                        :key (lambda (el) (plist-get el :type))))
              (should (cl-some (lambda (v)
                                 (string-match-p "— in progress" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "0/2 done" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "\\[ \\] 1\\. step one" v))
                               values))
              ;; the character progress bar reflects zero done
              (should (cl-find "0/2 done" values :test #'string=))
              (should (cl-find "[░░░░░░░░]" values :test #'string=)))
            (superchat-plan-report-progress '((1 "done")))
            (let* ((values (test-plan--frame-values
                            (superchat-plan--view-frame 24))))
              (should (cl-some (lambda (v)
                                 (string-match-p "1/2 done" v))
                               values))
              (should (cl-some (lambda (v)
                                 (string-match-p "\\[✓\\] 1\\. step one" v))
                               values))
              (should (cl-find "1/2 done" values :test #'string=))
              (should (cl-find "[████░░░░]" values :test #'string=)))
            (with-current-buffer superchat-buffer-name
              (should (string-match-p "report_plan_progress"
                                      (superchat-plan--context-string
                                       superchat--active-plan)))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))))))

(ert-deftest plan/nudge-pushes-pending-execution ()
  "Real current completion sends pending instructions, bounded at two nudges."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-clear)
      (superchat-plan-propose '(("step one" "criterion") ("step two" "criterion")) "demo")
      (superchat-plan-approve)
      (should superchat-plan--nudge-pending)
      (test-input-batch--reply (car test-input-batch--requests) nil)
      (should (= 2 (length test-input-batch--requests)))
      (should (string-match-p "2 plan steps still pending" (buffer-string)))
      (superchat-plan-report-progress '((1 "done")))
      (test-input-batch--reply (car test-input-batch--requests) nil)
      (should (= 3 (length test-input-batch--requests)))
      (should (= 2 superchat-plan--nudges))
      (should (string-match-p "1 plan step still pending" (buffer-string)))
      (test-input-batch--reply (car test-input-batch--requests) nil)
      (should (= 3 (length test-input-batch--requests)))
      (should-not superchat-plan--nudge-pending))))

(ert-deftest plan/nudge-stops-when-all-done ()
  "A real completed current Run with all included steps done sends no nudge."
  (dolist (mode '(async streaming))
    (plan-owner-test--fixture mode
      (superchat-plan-approve)
      (superchat-plan-report-progress '((1 "done")))
      (test-input-batch--reply (car test-input-batch--requests) nil)
      (should (= 1 (length test-input-batch--requests)))
      (should-not superchat-plan--nudge-pending))))

(ert-deftest plan/progress-updates-view-buffer ()
  "Reporting progress re-renders the live TextUI view buffer content."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (or (featurep 'textui) (require 'textui nil t))))
  (test-experience--with-temp-store
    (let* ((superchat-buffer-name
            (generate-new-buffer-name "*plan-view-live-test*"))
           (superchat--session-id "plan-session"))
      (unwind-protect
          (progn
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (erase-buffer)
              (superchat-mode 1))
            (superchat-plan-propose
             '(("step one" "criterion one")) "demo")
            (cl-letf (((symbol-function 'message) #'ignore))
              (let ((superchat-plan-auto-execute nil))
                (superchat-plan-approve)))
            (let ((view (get-buffer superchat-plan--view-buffer)))
              (should view)
              ;; textui-update defers the re-render; force the
              ;; synchronous rebuild for the batch check
              (textui-refresh view)
              (should
               (with-current-buffer view
                 (string-match-p "0/1 done" (buffer-string))))
              (superchat-plan-report-progress '((1 "done")))
              (textui-refresh view)
              (should
               (with-current-buffer view
                 (string-match-p "1/1 done" (buffer-string))))
              (should
               (with-current-buffer view
                 (string-match-p "✓" (buffer-string))))))
        (when (get-buffer superchat-buffer-name)
          (kill-buffer superchat-buffer-name))
        (when (get-buffer superchat-plan--view-buffer)
          (kill-buffer superchat-plan--view-buffer))))))

;;; test-plan.el ends here
