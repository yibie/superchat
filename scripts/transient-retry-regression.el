;;; transient-retry-regression.el --- offline transport retry tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Scripted provider tests for the narrowed transient retry decision.
;; Run with the prepared ELPA and pinned llm source, then
;; -l scripts/transient-retry-regression.el
;; --eval '(ert-run-tests-batch-and-exit "^transient-retry/")'.

;;; Code:
(require 'ert)
(require 'cl-lib)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defmacro transient-retry--fixture (script &rest body)
  "Run BODY with scripted network SCRIPT in an isolated chat."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names)
         (superchat-llm-tools-list nil)
         (superchat--llm-transient-retry-base-delay 0.01))
     (smoke--with-fixture ,script ,@body)))

(defun transient-retry--settle (run)
  "Wait for RUN to end and return its status."
  (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
  (superchat-runtime-status run))

(defun transient-retry--case (error-message &optional expected-requests)
  "Run one ERROR-MESSAGE followed by an answer; assert EXPECTED-REQUESTS."
  (transient-retry--fixture
      (list (list :error (cons 'error error-message)) '(:text "ok"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (status (transient-retry--settle run)))
      (should (eq (if (= (or expected-requests 2) 2) 'completed 'failed)
                  (plist-get status :status)))
      (should (= (or expected-requests 2) (length smoke--requests)))
      (if (= (or expected-requests 2) 2)
          (progn
            (should (eq 'transient-retry (plist-get status :attempt-reason)))
            (should (string-match-p "ok" (with-current-buffer view (buffer-string)))))
        (should (string-match-p (regexp-quote error-message)
                                (with-current-buffer view (buffer-string))))))))

(ert-deftest transient-retry/timeout-then-answer-completes ()
  (transient-retry--case "Operation timeout."))

(ert-deftest transient-retry/tls-failure-then-answer-completes ()
  (transient-retry--case "curl error code 35: SSL connect error"))

(ert-deftest transient-retry/http-errors-do-not-retry ()
  (transient-retry--case "Open AI returned error: server_error message: overloaded" 1)
  (transient-retry--case "Open AI returned error: invalid_request_error message: bad request" 1))

(ert-deftest transient-retry/budget-is-bounded ()
  (transient-retry--fixture
      (cl-loop for n from 1 to 6
               collect (list :error (cons 'error (format "curl error code 7: connection %d" n))))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (status (transient-retry--settle run)))
      (should (eq 'failed (plist-get status :status)))
      (should (= 6 (length smoke--requests)))
      (should (string-match-p "connection 6" (with-current-buffer view (buffer-string)))))))

(ert-deftest transient-retry/count-resets-after-success ()
  (let ((target (generate-new-buffer "transient-retry-target")))
    (unwind-protect
        (progn
          (with-current-buffer target (insert "token"))
          (transient-retry--fixture
              (list '(:error (error . "Operation timeout."))
                    `(:tool-uses (("read_buffer" (buffer . ,(buffer-name target)))))
                    '(:error (error . "Operation timeout."))
                    '(:text "ok"))
            (let* ((superchat-llm-transient-retries 1)
                   (view (smoke--open-chat))
                   (run (smoke--send view "read and answer")))
              (should (eq 'completed (plist-get (transient-retry--settle run) :status)))
              (should (= 4 (length smoke--requests))))))
      (kill-buffer target))))

(ert-deftest transient-retry/cancel-during-backoff-sends-nothing ()
  (transient-retry--fixture
      (list '(:error (error . "Operation timeout.")) '(:text "wrong"))
    (let* ((superchat--llm-transient-retry-base-delay 0.3)
           (view (smoke--open-chat))
           (run (smoke--send view "answer")))
      (should (smoke--wait (lambda () (and (= 1 (length smoke--requests))
                                           (= 1 (length smoke--script)))) 2))
      ;; Let the scripted error callback schedule its backoff first.
      (sleep-for 0.05)
      (superchat-runtime-cancel run "[cancelled]")
      (sleep-for 0.5)
      (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 (length smoke--requests))))))

(ert-deftest transient-retry/partial-output-is-not-retried ()
  (transient-retry--fixture
      (list '(:chunks ("partial") :error (error . "Operation timeout.")) '(:text "wrong"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer")))
      (should (eq 'failed (plist-get (transient-retry--settle run) :status)))
      (should (= 1 (length smoke--requests)))
      (should (string-match-p "partial" (with-current-buffer view (buffer-string)))))))

(ert-deftest transient-retry/classifier ()
  (should (superchat--llm-transient-error-p 'error "Operation timeout."))
  (dolist (code '(6 7 35 52 55 56))
    (should (superchat--llm-transient-error-p
             'error (format "curl error code %d: sample" code))))
  (dolist (message '("curl error code 3: malformed URL"
                     "HTTP error: Operation timeout."
                     "Open AI returned error: server_error message: timeout"
                     "Open AI returned error: invalid_request_error message: bad request"))
    (should-not (superchat--llm-transient-error-p 'error message)))
  (should-not (superchat--llm-transient-error-p 'llm-context-length-exceeded
                                                "Operation timeout.")))

(provide 'transient-retry-regression)
;;; transient-retry-regression.el ends here
