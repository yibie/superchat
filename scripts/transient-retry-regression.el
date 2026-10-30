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
(require 'plz)
(require 'llm-deepseek)

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
      (let ((text (with-current-buffer view (buffer-string))))
        (should (string-match-p "partial" text))
        (should (string-match-p "\\[API error: Operation timeout\\.\\]" text))))))

(ert-deftest transient-retry/partial-then-unretried-error-says-why ()
  "Text delivered, then an error that is not transient: the text and the reason."
  (transient-retry--fixture
      (list '(:chunks ("half an ans") :error (error . "curl error code 3: URL malformed")))
    (let* ((view (smoke--open-chat))
           (run (smoke--send view "answer")))
      (should (eq 'failed (plist-get (transient-retry--settle run) :status)))
      (let ((text (with-current-buffer view (buffer-string))))
        (should (string-match-p "half an ans" text))
        (should (string-match-p "\\[API error: curl error code 3" text))))))

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

(defun transient-retry--plz-unexpected-status (code)
  "Return what plz 0.9.1 reports for a curl that exited with CODE above 90."
  (format "Unexpected curl process status:%S code:%S.  Please report this bug to the `plz' maintainer."
          (format "exited abnormally with code %d\n" code) code))

(ert-deftest transient-retry/classifier-dropped-connection ()
  "Over HTTP/2 a dropped connection makes curl exit 16, 18, 56 or 92.
See docs/research/deepseek-drop-curl-codes.md."
  (dolist (code '(16 18 92))
    (should (superchat--llm-transient-error-p
             'error (format "curl error code %d: sample" code))))
  (should (superchat--llm-transient-error-p 'error "curl error code 16: nil"))
  (should (superchat--llm-transient-error-p
           'error (transient-retry--plz-unexpected-status 92)))
  (dolist (message (list "curl process killed"
                         "curl process interrupted"
                         (transient-retry--plz-unexpected-status 93)
                         "curl error code 93: sample"))
    (should-not (superchat--llm-transient-error-p 'error message))))

(ert-deftest transient-retry/dropped-connection-then-answer-completes ()
  (dolist (message (list "curl error code 16: nil"
                         "curl error code 18: Partial file. Only a part of the file was transferred."
                         (transient-retry--plz-unexpected-status 92)))
    (transient-retry--case message)))

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

(defun transient-retry--fake-curl (code &optional output)
  "Return a program that reads plz's curl config and exits with CODE.
It first writes OUTPUT, a string, as curl would: headers, then body."
  (let* ((data (and output (make-temp-file "superchat-fake-curl-out-" nil nil output)))
         (program (make-temp-file "superchat-fake-curl-" nil ".sh"
                                  (format "#!/bin/sh\ncat >/dev/null\n%sexit %d\n"
                                          (if data (format "cat '%s'\nrm -f '%s'\n" data data) "")
                                          code))))
    (set-file-modes program #o700)
    program))

(defmacro transient-retry--curl-fixture (script &rest body)
  "Run SCRIPT; a step (:curl-exit N) goes through the real llm.el and plz.
That step's curl is a program that exits with N, so plz and llm.el word
the failure themselves.  Other steps use smoke's scripted network."
  (declare (indent 1) (debug t))
  `(let ((real-streaming (symbol-function 'llm-chat-streaming))
         (real-async (symbol-function 'llm-chat-async))
         (programs nil))
     (unwind-protect
         (transient-retry--fixture ,script
           (let ((streaming nil) (async nil) (through-plz nil))
             (setq through-plz
                   (lambda (mode real args)
                     (let* ((step (pop smoke--script))
                            (plz-curl-program
                             (transient-retry--fake-curl (plist-get step :curl-exit)
                                                         (plist-get step :curl-output))))
                       (push plz-curl-program programs)
                       (push (list :mode mode :curl-exit (plist-get step :curl-exit))
                             smoke--requests)
                       ;; Calling the real generic may restore its function
                       ;; cell; keep dispatch in place for the next attempt.
                       (unwind-protect (apply real args)
                         (fset 'llm-chat-streaming streaming)
                         (fset 'llm-chat-async async))))
                   streaming
                   (lambda (provider prompt partial response error &optional multi)
                     (if (plist-get (car smoke--script) :curl-exit)
                         (funcall through-plz 'streaming real-streaming
                                  (list provider prompt partial response error multi))
                       (smoke--fake-network 'streaming provider prompt
                                            partial response error multi)))
                   async
                   (lambda (provider prompt response error &optional multi)
                     (if (plist-get (car smoke--script) :curl-exit)
                         (funcall through-plz 'async real-async
                                  (list provider prompt response error multi))
                       (smoke--fake-network 'async provider prompt
                                            nil response error multi))))
             (cl-letf (((symbol-function 'llm-chat-streaming) streaming)
                       ((symbol-function 'llm-chat-async) async))
               ,@body)))
       (mapc #'delete-file programs))))

(defun transient-retry--curl-case (code retry-p)
  "Let curl exit with CODE under the real llm.el and plz; expect a retry when RETRY-P.
Both the streaming and the non-streaming request are tried."
  (dolist (streaming '(t nil))
    (transient-retry--curl-fixture
        (list (list :curl-exit code) '(:text "ok"))
      (let* ((superchat-llm-streaming streaming)
             (view (smoke--open-chat))
             (run (smoke--send view "answer"))
             (outcome (transient-retry--settle run)))
        (should (eq (if streaming 'streaming 'async)
                    (plist-get (car (last smoke--requests)) :mode)))
        (should (eq (if retry-p 'completed 'failed) (plist-get outcome :status)))
        (should (= (if retry-p 2 1) (length smoke--requests)))
        (if retry-p
            (should (eq 'transient-retry (plist-get outcome :attempt-reason)))
          (should (string-match-p (format "code[: ]%d\\b" code)
                                  (with-current-buffer view (buffer-string)))))))))

(ert-deftest transient-retry/dropped-connection-through-plz ()
  "curl's exit code reaches the classifier in llm.el's and plz's own words."
  (dolist (code '(16 18 56 92))
    (transient-retry--curl-case code t)))

(ert-deftest transient-retry/other-curl-exits-through-plz-do-not-retry ()
  (dolist (code '(3 93))
    (transient-retry--curl-case code nil)))

(defconst transient-retry--json-head
  "HTTP/2 200 \r\ncontent-type: application/json\r\n\r\n"
  "Response headers of a non-streaming reply, as curl writes them.")

(defun transient-retry--body-case (body retry-p)
  "Let curl exit 0 after a 200 whose JSON BODY is all there is, without streaming.
Expect a retry when RETRY-P, else a failed execution naming the body."
  (transient-retry--curl-fixture
      (list (list :curl-exit 0 :curl-output (concat transient-retry--json-head body))
            '(:text "ok"))
    (let* ((superchat-llm-streaming nil)
           (view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (outcome (transient-retry--settle run)))
      (should (eq 'async (plist-get (car (last smoke--requests)) :mode)))
      (should (eq (if retry-p 'completed 'failed) (plist-get outcome :status)))
      (should (= (if retry-p 2 1) (length smoke--requests)))
      (when retry-p
        (should (eq 'transient-retry (plist-get outcome :attempt-reason)))))))

(ert-deftest transient-retry/cut-off-body-through-plz ()
  "An HTTP/2 stream reset with NO_ERROR after the headers: curl exits 0 and
the JSON stops short.  plz raised the parse error in its own timer and no
callback ran (docs/research/deepseek-drop-curl-codes.md)."
  (transient-retry--body-case "{\"id\":\"x\",\"choices\":[{" t))

(defconst transient-retry--sse-head
  "HTTP/2 200 \r\ncontent-type: text/event-stream\r\n\r\n"
  "Response headers of a streamed reply, as curl writes them.")

(defun transient-retry--sse-event (text)
  "Return one whole streamed event carrying TEXT."
  (format "data: {\"id\":\"x\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"%s\"}}]}\n\n"
          text))

(defun transient-retry--stream-case (backend body expected-status expected-requests)
  "Stream BODY from BACKEND under the real llm.el and plz; curl exits 0.
A second request, if any, answers \"SECOND\".  Expect EXPECTED-STATUS after
EXPECTED-REQUESTS requests and return the chat's text."
  (transient-retry--curl-fixture
      (list (list :curl-exit 0 :curl-output (concat transient-retry--sse-head body))
            '(:text "SECOND"))
    (let* ((superchat-llm-backend backend)
           (superchat-llm-streaming t)
           ;; DeepSeek streams only a request without tools.
           (superchat-llm-tools-enabled nil)
           (view (smoke--open-chat))
           (run (smoke--send view "answer"))
           (outcome (transient-retry--settle run)))
      (should (eq 'streaming (plist-get (car (last smoke--requests)) :mode)))
      (should (eq expected-status (plist-get outcome :status)))
      (should (= expected-requests (length smoke--requests)))
      (with-current-buffer view (buffer-string)))))

(defun transient-retry--deepseek ()
  "Return a DeepSeek provider for a stand-in curl."
  (make-llm-deepseek :key "unused" :chat-model "deepseek-v4-flash"))

(ert-deftest transient-retry/stream-whole-is-not-a-cut ()
  "Many events and [DONE] in one chunk, curl gone at once: the worst ordering."
  (let* ((n 300)
         (text (transient-retry--stream-case
                (transient-retry--deepseek)
                (concat (mapconcat (lambda (i) (transient-retry--sse-event (format "w%d " i)))
                                   (number-sequence 1 n) "")
                        "data: [DONE]\n\n")
                'completed 1)))
    (should (string-match-p (format "w1 .*w%d " n) text))))

(ert-deftest transient-retry/stream-cut-before-any-text-is-sent-again ()
  (let ((text (transient-retry--stream-case
               (transient-retry--deepseek)
               "data: {\"id\":\"x\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"PA"
               'completed 2)))
    (should (string-match-p "SECOND" text))
    (should-not (string-match-p "Missing final response" text))))

(ert-deftest transient-retry/stream-cut-after-text-says-so ()
  "The text that arrived stays, the execution fails, and nothing is sent again."
  (let ((text (transient-retry--stream-case
               (transient-retry--deepseek)
               (transient-retry--sse-event "PARTIAL")
               'failed 1)))
    (should (string-match-p "PARTIAL" text))
    (should (string-match-p "Reply stream ended before \\[DONE\\]" text))))

(ert-deftest transient-retry/stream-end-is-asked-of-deepseek-only ()
  "Another OpenAI-compatible server need not send [DONE]."
  (let ((text (transient-retry--stream-case
               (make-llm-openai-compatible :url "http://127.0.0.1:9/v1" :chat-model "smoke-model")
               (transient-retry--sse-event "WHOLE")
               'completed 1)))
    (should (string-match-p "WHOLE" text))))

(ert-deftest transient-retry/stream-of-headers-only-is-sent-again ()
  "curl exits 0 before any body: plz-event-source has no source to close, and
that error stayed in plz's timer with no callback.  Seen against real DeepSeek."
  (dolist (backend (list (transient-retry--deepseek)
                         (make-llm-openai-compatible :url "http://127.0.0.1:9/v1"
                                                     :chat-model "smoke-model")))
    (should (string-match-p "SECOND"
                            (transient-retry--stream-case backend "" 'completed 2)))))

(ert-deftest transient-retry/classifier-stream-cut ()
  (should (superchat--llm-transient-error-p 'error "Reply stream ended before [DONE]"))
  (should-not (superchat--llm-transient-error-p 'error "Reply stream ended"))
  (should-not (superchat--llm-transient-error-p 'error "Reply stream ended before [DONE]" 400)))

(ert-deftest transient-retry/classifier-stream-of-headers-only ()
  (should (superchat--llm-transient-error-p
           'error "Unparsed response body: (cl-no-applicable-method plz-event-source-close nil)"))
  (should-not (superchat--llm-transient-error-p
               'error "Unparsed response body: (cl-no-applicable-method something-else nil)")))

(defun transient-retry--unparsed-call (delivered raise)
  "Call the unparsed-body advice with DELIVERED bound; its THEN signals RAISE.
Return what ELSE received, or the condition that escaped."
  (let ((received nil))
    (condition-case err
        (let ((superchat--llm-plz-delivered delivered))
          (superchat--llm-plz-report-unparsed-body
           (lambda (_method _url &rest rest)
             (funcall (plist-get rest :then) 'response))
           'post "http://127.0.0.1:9/v1"
           :then (lambda (_) (signal (car raise) (cdr raise)))
           :else (lambda (error) (setq received (plz-error-message error))))
          received)
      (error (list 'escaped (car err))))))

(ert-deftest transient-retry/unparsed-body-goes-to-else-only-before-delivery ()
  (should (equal "Unparsed response body: (json-end-of-file 1 nil 22)"
                 (transient-retry--unparsed-call (list nil) '(json-end-of-file 1 nil 22))))
  (should (equal "Unparsed response body: (json-parse-error 8 nil 8)"
                 (transient-retry--unparsed-call (list nil) '(json-parse-error 8 nil 8))))
  ;; After llm.el had the reply, a parse error is its own and stays raised.
  (should (equal '(escaped json-end-of-file)
                 (transient-retry--unparsed-call (list t) '(json-end-of-file 1 nil 22))))
  ;; A request llm.el did not start for Superchat is left alone.
  (should (equal '(escaped json-end-of-file)
                 (transient-retry--unparsed-call nil '(json-end-of-file 1 nil 22))))
  ;; Only JSON errors are taken.
  (should (equal '(escaped wrong-type-argument)
                 (transient-retry--unparsed-call (list nil) '(wrong-type-argument nil)))))

(ert-deftest transient-retry/classifier-unparsed-body ()
  (should (superchat--llm-transient-error-p
           'error "Unparsed response body: (json-end-of-file 1 nil 22)"))
  (should (superchat--llm-transient-error-p
           'error "Unparsed response body: (json-parse-error 8 nil 8)"))
  (should-not (superchat--llm-transient-error-p
               'error "Failed to parse response, 12 bytes unprocessed")))

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
