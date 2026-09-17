;;; m5-response-regression.el --- Final response presence regression -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline public-entry tests for missing final responses.  Provider success
;; callbacks can carry no answer; Runtime must not turn that into completion.
;; Explicit empty strings keep their existing policy.  Tools, visible stream
;; fallback, pending steer and cancellation keep their existing paths.
;;
;; Run from the repository root with the prepared dependencies:
;; emacs -Q --batch --eval '(progn (require (quote package))
;;   (setq package-user-dir "/path/to/elpa" native-comp-enable-subr-trampolines nil)
;;   (package-initialize))' -L . -L plugins/superchat-cowork-telega
;;   -l scripts/m5-response-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^m5-response/")'

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Response regression tests are offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "scripts/smoke.el"
                        (file-name-directory (locate-library "superchat")))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro m5resp--with-fixture (script &rest body)
  "Use the canonical fixture, with explicit provider-result boundary cases."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names))
     (smoke--with-fixture ,script
       (let ((deliver (symbol-function 'smoke--deliver)))
         (cl-letf
             (((symbol-function 'smoke--deliver)
               (lambda (provider prompt partial response error multi step)
                 ;; These are provider partial callbacks, not visible text
                 ;; secretly inserted into the transcript or Runtime.
                 (dolist (value (plist-get step :provider-partials))
                   (when partial (funcall partial value)))
                 (cond
                  ((plist-member step :adapter-result)
                   ;; Exercise the actual llm.el result sanitizer/adapter.
                   (llm-provider-utils-process-result
                    provider prompt (plist-get step :adapter-result)
                    multi response error))
                  ((plist-member step :callback-response)
                   ;; Explicit compatibility test for a provider returning a
                   ;; bare string rather than the usual multi-output plist.
                   (funcall response (plist-get step :callback-response)))
                  (t (funcall deliver provider prompt partial response error multi step))))))
           ,@body)))))

(defun m5resp--run (script &optional before-first-response)
  "Submit a real chat Run using SCRIPT and return its observable outcome.
BEFORE-FIRST-RESPONSE may enqueue real user input before the first completion."
  (m5resp--with-fixture script
    (let ((network (symbol-function 'smoke--fake-network))
          (values nil)
          (hook before-first-response)
          view run session outcome)
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (lambda (mode provider prompt partial response error multi)
                   (funcall network mode provider prompt partial
                            (lambda (value)
                              (push (copy-tree value t) values)
                              (when hook
                                (let ((once hook))
                                  (setq hook nil)
                                  (funcall once run)))
                              (funcall response value))
                            error multi))))
        (setq view (smoke--open-chat)
              run (smoke--send view "Read if needed, then give the final answer.")
              session (buffer-local-value 'superchat--session-id view))
        (should
         (smoke--wait
          (lambda ()
            (and (smoke--run-done-p run)
                 (cl-some
                  (lambda (event)
                    (and (equal run (plist-get event :run-id))
                         (equal "turn.completed" (plist-get event :type))))
                  (superchat-tape-replay-events session))))))
        (let* ((events
                (cl-remove-if-not
                 (lambda (event) (equal run (plist-get event :run-id)))
                 (superchat-tape-replay-events session)))
               (responses
                (cl-remove-if-not
                 (lambda (event) (equal "response.completed" (plist-get event :type)))
                 events))
               (terminals
                (cl-remove-if-not
                 (lambda (event)
                   (member (plist-get event :type)
                           '("run.completed" "run.failed" "run.cancelled")))
                 events))
               (response (car responses))
               (storage (plist-get response :storage-id))
               ;; These asserted bodies are short; select the durable row by
               ;; this exact Run's response fact, never by latest chat text.
               (body (nth 3 (cl-find storage (superchat-db-tape-replay session)
                                     :key #'car :test #'equal))))
          (should (= 1 (length responses)))
          (should (= 1 (length terminals)))
          (setq outcome
                (list :status (plist-get (superchat-runtime-status run) :status)
                      :body body
                      :response-status
                      (format "%s" (plist-get (plist-get response :payload) :status))
                      :terminal-type (plist-get (car terminals) :type)
                      :budget (plist-get (superchat-runtime-status run) :tool-budget)
                      :response-budget (plist-get (plist-get response :payload) :tool-budget)
                      :terminal-budget (plist-get (plist-get (car terminals) :payload) :tool-budget)
                      :requests (reverse (copy-tree smoke--requests))
                      :callbacks (nreverse values)
                      :tool-completions
                      (cl-count "tool.completed" events
                                :key (lambda (event) (plist-get event :type))
                                :test #'equal)))))
      outcome)))

(defun m5resp--assert-missing (outcome)
  "Check the visible failure and its native Runtime/Tape disposition."
  (should (eq 'failed (plist-get outcome :status)))
  (should (equal "failed" (plist-get outcome :response-status)))
  (should (equal "run.failed" (plist-get outcome :terminal-type)))
  (should (string-match-p "Missing final response" (plist-get outcome :body))))

(ert-deftest m5-response/missing-final-callback-fails ()
  (let ((outcome (m5resp--run '((:adapter-result nil)))))
    (should (equal '(nil) (plist-get outcome :callbacks)))
    (should (= 1 (length (plist-get outcome :requests))))
    (m5resp--assert-missing outcome)))

(ert-deftest m5-response/metadata-only-final-callback-fails ()
  (let ((outcome (m5resp--run '((:adapter-result (:input-tokens 12))))))
    (should (equal '((:input-tokens 12)) (plist-get outcome :callbacks)))
    (m5resp--assert-missing outcome)))

(ert-deftest m5-response/reasoning-progress-without-visible-answer-fails ()
  (let ((outcome
         (m5resp--run
          '((:provider-partials ((:reasoning "Thinking only."))
             :adapter-result (:reasoning "Thinking only."))))))
    (should (equal '((:reasoning "Thinking only.")) (plist-get outcome :callbacks)))
    (m5resp--assert-missing outcome)))

(ert-deftest m5-response/explicit-empty-plist-keeps-existing-outcome ()
  (let ((outcome (m5resp--run '((:text "")))))
    (should (equal '((:text "")) (plist-get outcome :callbacks)))
    (should (eq 'completed (plist-get outcome :status)))
    (should (equal "[Empty response from llm]" (plist-get outcome :body)))))

(ert-deftest m5-response/explicit-empty-string-keeps-existing-outcome ()
  (let ((outcome (m5resp--run '((:callback-response "")))))
    (should (equal '("") (plist-get outcome :callbacks)))
    (should (eq 'completed (plist-get outcome :status)))
    (should (equal "[Empty response from llm]" (plist-get outcome :body)))))

(ert-deftest m5-response/current-visible-stream-survives-nil-final ()
  (let ((outcome
         (m5resp--run '((:provider-partials ((:text "VISIBLE_STREAM"))
                        :adapter-result nil)))))
    (should (equal '(nil) (plist-get outcome :callbacks)))
    (should (eq 'completed (plist-get outcome :status)))
    (should (equal "VISIBLE_STREAM" (plist-get outcome :body)))))

(ert-deftest m5-response/previous-tool-preamble-does-not-hide-missing-followup ()
  (let ((outcome
         (m5resp--run
          '((:chunks ("Reading now.") :text "Reading now."
             :tool-uses (("read_buffer" (buffer . "*scratch*"))))
            (:adapter-result nil)))))
    (should (= 2 (length (plist-get outcome :requests))))
    (should (= 1 (plist-get outcome :tool-completions)))
    (should-not (cadr (plist-get outcome :callbacks)))
    (m5resp--assert-missing outcome)))

(ert-deftest m5-response/tool-only-round-still-continues ()
  (let ((outcome
         (m5resp--run '((:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                       (:text "Read finished.")))))
    (should (= 2 (length (plist-get outcome :requests))))
    (should (= 1 (plist-get outcome :tool-completions)))
    (should (plist-get (car (plist-get outcome :callbacks)) :tool-results))
    (should (eq 'completed (plist-get outcome :status)))
    (should (equal "Read finished." (plist-get outcome :body)))))

(ert-deftest m5-response/missing-budget-final-response-keeps-native-budget ()
  (let* ((superchat-llm-max-tool-rounds 1)
         (superchat-llm-round-limit-action 'finalize)
         (outcome
          (m5resp--run '((:tool-uses (("read_buffer" (buffer . "*scratch*"))))
                        (:adapter-result nil)))))
    (should (= 2 (length (plist-get outcome :requests))))
    (should-not (plist-get (cadr (plist-get outcome :requests)) :tools))
    (dolist (key '(:budget :response-budget :terminal-budget))
      (let ((budget (plist-get outcome key)))
        (should (= 1 (plist-get budget :rounds)))
        (should (= 1 (plist-get budget :limit)))
        (should (equal "finalize" (format "%s" (plist-get budget :action))))))
    (m5resp--assert-missing outcome)))

(ert-deftest m5-response/pending-steer-precedes-missing-response-finalization ()
  (let* ((marker "M5RESP_PENDING_STEER")
         (outcome
          (m5resp--run
           '((:adapter-result nil) (:text "Steer received."))
           (lambda (run)
             (should (superchat-runtime-steer run marker))
             (should (= 1 (superchat-runtime-pending-steer-count run)))))))
    (should (= 2 (length (plist-get outcome :requests))))
    (should-not (string-match-p marker
                                (plist-get (car (plist-get outcome :requests)) :prompt-text)))
    (should (string-match-p marker
                            (plist-get (cadr (plist-get outcome :requests)) :prompt-text)))
    (should (eq 'completed (plist-get outcome :status)))
    (should (equal "Steer received." (plist-get outcome :body)))))

(ert-deftest m5-response/cancelled-run-ignores-late-nil-callback ()
  (m5resp--with-fixture '((:hang t))
    (let ((network (symbol-function 'smoke--fake-network))
          callback view run session before requests)
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (lambda (mode provider prompt partial response error multi)
                   (setq callback response)
                   (funcall network mode provider prompt partial response error multi))))
        (setq view (smoke--open-chat)
              run (smoke--send view "Cancelled response boundary review.")
              session (buffer-local-value 'superchat--session-id view))
        (should (functionp callback))
        (superchat-runtime-cancel run "[response test cancellation]")
        (setq before (superchat-tape-replay-events session)
              requests (length smoke--requests))
        (funcall callback nil)
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
        (should (= requests (length smoke--requests)))
        (should (equal before (superchat-tape-replay-events session)))))))

;;; m5-response-regression.el ends here
