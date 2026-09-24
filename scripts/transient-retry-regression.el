;;; transient-retry-regression.el --- offline transport retry tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Scripted provider tests for transient transport and HTTP retry decisions.
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
         (superchat-llm-transient-retry-interval 0.01))
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

(ert-deftest transient-retry/http-body-without-status-does-not-retry ()
  (transient-retry--case "Open AI returned error: server_error message: overloaded" 1)
  (transient-retry--case "Open AI returned error: invalid_request_error message: bad request" 1))

(ert-deftest transient-retry/budget-is-bounded ()
  (transient-retry--fixture
      (cl-loop for n from 1 to 11
               collect (list :error (cons 'error (format "curl error code 7: connection %d" n))))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (status (transient-retry--settle run)))
      (should (eq 'failed (plist-get status :status)))
      (should (= 11 (length smoke--requests)))
      (should (string-match-p "connection 11" (with-current-buffer view (buffer-string)))))))

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
    (let* ((superchat-llm-transient-retry-interval 0.3)
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
                                                "Operation timeout."))
  (dolist (status '(429 500 503 599))
    (should (superchat--llm-transient-error-p 'error "provider body" status)))
  (dolist (status '(400 401 403 404 422 600))
    (should-not (superchat--llm-transient-error-p 'error "provider body" status)))
  (should-not (superchat--llm-transient-error-p
               'error "rate limit: retry later" nil)))

(ert-deftest transient-retry/defaults-are-ten-retries-thirty-seconds-apart ()
  (should (= 10 (default-value 'superchat-llm-transient-retries)))
  (should (= 30 (default-value 'superchat-llm-transient-retry-interval))))

(ert-deftest transient-retry/retry-waits-the-interval ()
  (transient-retry--fixture
      (list '(:error (error . "Operation timeout."))
            '(:error (error . "Operation timeout.")) '(:text "ok"))
    (let ((superchat-llm-transient-retry-interval 0.3)
          (request-times nil)
          (retry-delays nil)
          (network (symbol-function 'smoke--fake-network))
          (schedule (symbol-function 'run-at-time)))
      (cl-letf (((symbol-function 'smoke--fake-network)
                 (lambda (&rest args)
                   (push (float-time) request-times)
                   (apply network args)))
                ((symbol-function 'run-at-time)
                 (lambda (seconds repeat function &rest args)
                   (when (equal seconds 0.3) (push seconds retry-delays))
                   (apply schedule seconds repeat function args))))
        (let* ((view (smoke--open-chat))
               (run (smoke--send view "answer")))
          (should (eq 'completed (plist-get (transient-retry--settle run) :status)))
          (setq request-times (nreverse request-times))
          (should (= 3 (length request-times)))
          (should (>= (- (nth 1 request-times) (nth 0 request-times)) 0.27))
          (should (>= (- (nth 2 request-times) (nth 1 request-times)) 0.27))
          (should (equal '(0.3 0.3) (nreverse retry-delays))))))))

(defun transient-retry--http-step (status)
  "Return a real-llm transport error step for HTTP STATUS."
  (list :http-status status))

(defmacro transient-retry--http-fixture (script &rest body)
  "Run SCRIPT through real llm chat handling with a stubbed plz transport.
Non-HTTP steps still use smoke's scripted network."
  (declare (indent 1) (debug t))
  `(let ((real-streaming (symbol-function 'llm-chat-streaming)))
     (transient-retry--fixture ,script
       (let ((dispatcher nil))
         (setq dispatcher
               (lambda (provider prompt partial response error &optional multi)
                 (if-let* ((step (car smoke--script))
                           (status (plist-get step :http-status)))
                     (progn
                       (pop smoke--script)
                       (push (list :mode 'streaming :http-status status) smoke--requests)
                       ;; Calling the real generic may restore its function
                       ;; cell; keep dispatch in place for the next attempt.
                       (unwind-protect
                           (funcall real-streaming provider prompt partial response error multi)
                         (fset 'llm-chat-streaming dispatcher)))
                   (smoke--fake-network 'streaming provider prompt
                                        partial response error multi))))
         (cl-letf (((symbol-function 'llm-chat-streaming) dispatcher)
                   ((symbol-function 'plz-media-type-request)
                    (lambda (&rest args)
                      (let* ((else (plist-get (cddr args) :else))
                             (status (plist-get (car smoke--requests) :http-status))
                             (body (format
                                    "{\"error\":{\"type\":\"provider_error\",\"message\":\"busy%s\"}}"
                                    (make-string 120 ?x)))
                             (buffer (generate-new-buffer " *retry-http-transport*")))
                        (push (run-at-time
                               0.01 nil
                               (lambda ()
                                 (funcall else
                                          (make-plz-error
                                           :response (make-plz-response
                                                      :version 1.1 :status status
                                                      :headers nil :body body)))))
                              smoke--timers)
                        buffer))))
           ,@body)))))

(defun transient-retry--http-case (status retry-p)
  "Exercise real llm HTTP STATUS handling; expect a retry when RETRY-P."
  (transient-retry--http-fixture
      (list (transient-retry--http-step status) '(:text "ok"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (outcome (transient-retry--settle run)))
      (should (eq (if retry-p 'completed 'failed) (plist-get outcome :status)))
      (should (= (if retry-p 2 1) (length smoke--requests)))
      (if retry-p
          (should (eq 'transient-retry (plist-get outcome :attempt-reason)))
        (should (string-match-p "provider_error"
                                (with-current-buffer view (buffer-string))))))))

(ert-deftest transient-retry/http-429-then-answer-completes ()
  (transient-retry--http-case 429 t))

(ert-deftest transient-retry/http-503-then-answer-completes ()
  (dolist (status '(500 502 503))
    (transient-retry--http-case status t)))

(ert-deftest transient-retry/http-rejections-do-not-retry ()
  (dolist (status '(400 401 404 422))
    (transient-retry--http-case status nil)))

(ert-deftest transient-retry/http-status-does-not-leak ()
  (transient-retry--http-fixture
      (list (transient-retry--http-step 429)
            '(:error (error . "rate limit: retry later")) '(:text "wrong"))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer")))
      (should (eq 'failed (plist-get (transient-retry--settle run) :status)))
      (should (= 2 (length smoke--requests)))
      (should-not superchat--llm-http-status)
      (should (string-match-p "rate limit: retry later"
                              (with-current-buffer view (buffer-string)))))))

(defun transient-retry--notice-wait (run)
  "Wait until RUN has scheduled its first retry, then return its status."
  (should (smoke--wait
           (lambda () (eq 'retry-wait
                          (plist-get (superchat-runtime-status run) :model-phase))) 3))
  (superchat-runtime-status run))

(ert-deftest transient-retry/notice-sink-sequence ()
  (transient-retry--fixture
      (list '(:error (error . "Operation timeout.")) '(:text "ok"))
    (let ((superchat-llm-transient-retry-interval 0.5)
          (events nil)
          (emit (symbol-function 'superchat-sink-emit)))
      (cl-letf (((symbol-function 'superchat-sink-emit)
                 (lambda (sink slot run payload &rest options)
                   (when (eq slot 'status)
                     (push (list payload (copy-tree run)) events))
                   (apply emit sink slot run payload options))))
        (let* ((view (smoke--open-chat)) (run (smoke--send view "answer"))
               (during (transient-retry--notice-wait run)))
          (should (equal 1 (plist-get (plist-get during :retry) :attempt)))
          (should (eq 'completed (plist-get (transient-retry--settle run) :status)))
          (setq events (nreverse events))
          (should (equal '(waiting retrying waiting ready) (mapcar #'car events)))
          (let ((retry (plist-get (cadr (nth 1 events)) :retry)))
            (should (= 1 (plist-get retry :attempt)))
            (should (= 10 (plist-get retry :limit)))
            (should (equal "Operation timeout." (plist-get retry :error)))
            (should (numberp (plist-get retry :at))))
          (should-not (plist-get (cadr (nth 2 events)) :retry))
          (should-not (plist-get (cadr (nth 3 events)) :retry))
          (should-not (plist-get (superchat-runtime-status run) :retry)))))))

(ert-deftest transient-retry/notice-chat-window ()
  (transient-retry--fixture
      (list '(:error (error . "Operation timeout.")) '(:text "ok"))
    (let* ((superchat-llm-transient-retry-interval 0.8)
           (view (smoke--open-chat)) (run (smoke--send view "answer")))
      (transient-retry--notice-wait run)
      (with-current-buffer view
        (superchat--status-refresh-tick view)
        (should (string-match-p
                 "Provider error; retrying in [0-9]+s (1/10)… · Operation timeout\."
                 (buffer-string)))
        (should (string-match-p "retrying 1/10" (superchat--header-line-run-state))))
      (should (eq 'completed (plist-get (transient-retry--settle run) :status)))
      (with-current-buffer view
        (should-not (string-match-p "Provider error; retrying" (buffer-string)))
        (should (string-match-p "ok" (buffer-string)))))))

(defun transient-retry--notice-inline-case (surface)
  "Check the live SURFACE overlay during retry and after completion."
  (with-temp-buffer
    (insert "alpha\n")
    (goto-char (point-min))
    (transient-retry--fixture
        (list '(:error (error . "Operation timeout."))
              (if (eq surface 'rewrite) '(:text "```text\nbeta\n```") '(:text "ok")))
      (let* ((superchat-llm-transient-retry-interval 0.7)
             (overlay (pcase surface
                        ('ask (superchat-inline-ask "answer"))
                        ('act (superchat-inline-act "answer"))
                        ('rewrite (superchat-rewrite--start-overlay
                                   (point-min) (point-max) "change" "text"))))
             (property (if (eq surface 'rewrite) 'superchat-rewrite 'superchat-inline))
             (state (lambda () (overlay-get overlay property))))
        (should (smoke--wait (lambda () (plist-get (funcall state) :run-id)) 3))
        (let ((run (plist-get (funcall state) :run-id)))
          (transient-retry--notice-wait run)
          (should (string-match-p
                   "Provider error; retrying in [0-9]+s (1/10)…"
                   (or (overlay-get overlay
                                    (if (eq surface 'rewrite) 'before-string 'after-string)) "")))
          (should (eq 'completed (plist-get (transient-retry--settle run) :status)))
          (should (smoke--wait (lambda ()
                                 (eq 'ready (plist-get (funcall state) :status))) 3))
          (should-not (string-match-p
                       "Provider error; retrying"
                       (or (overlay-get overlay
                                        (if (eq surface 'rewrite) 'before-string 'after-string)) ""))))))))

(ert-deftest transient-retry/notice-inline-ask ()
  (transient-retry--notice-inline-case 'ask))
(ert-deftest transient-retry/notice-inline-act ()
  (transient-retry--notice-inline-case 'act))
(ert-deftest transient-retry/notice-rewrite ()
  (transient-retry--notice-inline-case 'rewrite))

(ert-deftest transient-retry/notice-http-summary ()
  (transient-retry--http-fixture
      (list (transient-retry--http-step 429) '(:text "ok"))
    (let* ((superchat-llm-transient-retry-interval 0.5)
           (view (smoke--open-chat)) (run (smoke--send view "answer"))
           (retry (plist-get (transient-retry--notice-wait run) :retry)))
      (should (equal "HTTP 429" (plist-get retry :error)))
      (should-not (string-match-p "provider_error\\|busy" (format "%s" retry)))
      (should (eq 'completed (plist-get (transient-retry--settle run) :status))))))

(ert-deftest transient-retry/notice-cleared-on-cancel ()
  (transient-retry--fixture
      (list '(:error (error . "Operation timeout.")) '(:text "wrong"))
    (let ((superchat-llm-transient-retry-interval 0.5)
          (events nil) (emit (symbol-function 'superchat-sink-emit)))
      (cl-letf (((symbol-function 'superchat-sink-emit)
                 (lambda (sink slot run payload &rest options)
                   (when (eq slot 'status) (push (list payload (copy-tree run)) events))
                   (apply emit sink slot run payload options))))
        (let* ((view (smoke--open-chat)) (run (smoke--send view "answer")))
          (transient-retry--notice-wait run)
          (superchat-runtime-cancel run "[cancelled]")
          (sleep-for 0.7)
          (setq events (nreverse events))
          (should (equal '(waiting retrying stopped) (mapcar #'car events)))
          (should-not (plist-get (cadr (car (last events))) :retry))
          (should-not (plist-get (superchat-runtime-status run) :retry))
          (should-not (plist-get (superchat-runtime-sink-run run) :retry))
          (should (= 1 (length smoke--requests))))))))

(ert-deftest transient-retry/notice-emacsclient-stays-waiting ()
  (should (eq 'waiting
              (superchat-emacsclient--public-status
               '(:status running-model :model-phase retry-wait)))))

(provide 'transient-retry-regression)
;;; transient-retry-regression.el ends here
