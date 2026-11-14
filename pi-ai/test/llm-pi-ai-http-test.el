;;; llm-pi-ai-http-test.el --- Tests for llm-pi-ai-http  -*- lexical-binding: t; -*-

;; Tests for the HTTP seam.  No upstream test exists.  A fake start
;; function feeds bytes in pieces; one test runs real curl against an
;; Emacs server on 127.0.0.1 (no outside network).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'llm-pi-ai-http)

;;; Helpers

(cl-defstruct llm-pi-ai-http-test--rig
  "A fake curl: `output' and `exit' are the two callbacks the seam gave."
  output exit args config stdin (killed 0)
  chunks dones headers-calls handle)

(defun llm-pi-ai-http-test--run (rig &rest keys)
  "Start a request on RIG (fake start function) with request KEYS."
  (let ((llm-pi-ai-http-start-function
         (lambda (args config stdin on-output on-exit)
           (setf (llm-pi-ai-http-test--rig-args rig) args
                 (llm-pi-ai-http-test--rig-config rig) config
                 (llm-pi-ai-http-test--rig-stdin rig) stdin
                 (llm-pi-ai-http-test--rig-output rig) on-output
                 (llm-pi-ai-http-test--rig-exit rig) on-exit)
           (lambda () (cl-incf (llm-pi-ai-http-test--rig-killed rig))))))
    (setf (llm-pi-ai-http-test--rig-handle rig)
          (apply #'llm-pi-ai-http-request
                 :on-headers (lambda (s h)
                               (push (list s h)
                                     (llm-pi-ai-http-test--rig-headers-calls rig)))
                 :on-chunk (lambda (c) (push c (llm-pi-ai-http-test--rig-chunks rig)))
                 :on-done (lambda (r) (push r (llm-pi-ai-http-test--rig-dones rig)))
                 keys))))

(defun llm-pi-ai-http-test--feed (rig &rest pieces)
  "Feed PIECES (strings, made unibyte) to RIG as curl stdout reads."
  (dolist (p pieces)
    (funcall (llm-pi-ai-http-test--rig-output rig) (encode-coding-string p 'utf-8))))

(defun llm-pi-ai-http-test--body (rig)
  "Return all chunks of RIG joined."
  (apply #'concat (reverse (llm-pi-ai-http-test--rig-chunks rig))))

(defun llm-pi-ai-http-test--wait (pred &optional seconds)
  "Wait until PRED is non-nil, at most SECONDS (default 10)."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.02))))

(defconst llm-pi-ai-http-test--head "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n")

;;; Body decoding

(ert-deftest llm-pi-ai-http-test-split-multibyte-character ()
  "A character cut across two reads is delivered whole."
  (dolist (text '("héllo" "你好" "a\U0001F600b"))
    (let* ((bytes (encode-coding-string text 'utf-8))
           (rig (make-llm-pi-ai-http-test--rig)))
      ;; Cut at every byte position of the body.
      (dotimes (cut (1+ (length bytes)))
        (setf (llm-pi-ai-http-test--rig-chunks rig) nil)
        (llm-pi-ai-http-test--run rig :url "http://x")
        (funcall (llm-pi-ai-http-test--rig-output rig)
                 (concat (encode-coding-string llm-pi-ai-http-test--head 'utf-8)
                         (substring bytes 0 cut)))
        (funcall (llm-pi-ai-http-test--rig-output rig) (substring bytes cut))
        (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
        (should (equal (llm-pi-ai-http-test--body rig) text))
        (dolist (c (llm-pi-ai-http-test--rig-chunks rig))
          (should (cl-every (lambda (ch) (< ch #x3fff80)) c)))))))

(ert-deftest llm-pi-ai-http-test-header-split-and-body-in-same-read ()
  "Headers split over reads parse; body bytes after them in the same read arrive."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (llm-pi-ai-http-test--feed rig "HTTP/1.1 200 O" "K\r\nX-Request-Id: ab\r\nContent-Ty"
                               "pe: text/plain\r\n\r\nhel" "lo")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (should (equal (llm-pi-ai-http-test--body rig) "hello"))
    (should (equal (llm-pi-ai-http-test--rig-headers-calls rig)
                   '((200 (("x-request-id" . "ab") ("content-type" . "text/plain"))))))
    (should (equal (llm-pi-ai-http-test--rig-dones rig)
                   '((:status 200 :headers (("x-request-id" . "ab")
                                            ("content-type" . "text/plain"))))))))

(ert-deftest llm-pi-ai-http-test-skips-1xx-block ()
  "A `100 Continue' block before the real one is not the status."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (llm-pi-ai-http-test--feed rig "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 201 Created\r\n\r\nok")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (should (equal (llm-pi-ai-http-test--body rig) "ok"))
    (should (= (plist-get (car (llm-pi-ai-http-test--rig-dones rig)) :status) 201))))

(ert-deftest llm-pi-ai-http-test-error-status-with-body ()
  "A 429 with a JSON body delivers the body and ends with `:status 429'."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (llm-pi-ai-http-test--feed rig "HTTP/1.1 429 Too Many Requests\r\nRetry-After: 3\r\n\r\n"
                               "{\"error\":{\"message\":\"slow 下\"}}")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (should (equal (llm-pi-ai-http-test--body rig) "{\"error\":{\"message\":\"slow 下\"}}"))
    (let ((done (car (llm-pi-ai-http-test--rig-dones rig))))
      (should (= (plist-get done :status) 429))
      (should-not (plist-get done :error))
      (should (equal (cdr (assoc "retry-after" (plist-get done :headers))) "3")))
    (should (= (length (llm-pi-ai-http-test--rig-dones rig)) 1))))

;;; Done, errors, abort

(ert-deftest llm-pi-ai-http-test-exit-28-is-timeout ()
  "Exit code 28 maps to `timeout'; the message is curl's stderr."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x" :timeout 5)
    (should (equal (cl-subseq (llm-pi-ai-http-test--rig-args rig) 4 6)
                   '("--max-time" "5")))
    (llm-pi-ai-http-test--feed rig llm-pi-ai-http-test--head "partial")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 28
             "curl: (28) Operation timed out\n")
    (should (equal (llm-pi-ai-http-test--rig-dones rig)
                   '((:error timeout :message "curl: (28) Operation timed out"))))))

(ert-deftest llm-pi-ai-http-test-other-exit-is-network ()
  "Other non-zero exit codes map to `network'; empty stderr gets a message."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 6 "curl: (6) Could not resolve host: x\n")
    (should (equal (llm-pi-ai-http-test--rig-dones rig)
                   '((:error network :message "curl: (6) Could not resolve host: x")))))
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 7 "")
    (should (equal (plist-get (car (llm-pi-ai-http-test--rig-dones rig)) :message)
                   "curl exited with code 7"))))

(ert-deftest llm-pi-ai-http-test-exit-without-response-is-network ()
  "Exit 0 before any header block is a `network' error."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (should (eq (plist-get (car (llm-pi-ai-http-test--rig-dones rig)) :error) 'network))))

(ert-deftest llm-pi-ai-http-test-done-exactly-once ()
  "A second exit, or output after done, does nothing."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (llm-pi-ai-http-test--feed rig llm-pi-ai-http-test--head "a")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 28 "late")
    (llm-pi-ai-http-test--feed rig "b")
    (should (= (length (llm-pi-ai-http-test--rig-dones rig)) 1))
    (should (equal (llm-pi-ai-http-test--body rig) "a"))))

(ert-deftest llm-pi-ai-http-test-abort ()
  "After abort no chunk arrives and ON-DONE runs once with `aborted'."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (llm-pi-ai-http-test--feed rig llm-pi-ai-http-test--head "one")
    (llm-pi-ai-http-abort (llm-pi-ai-http-test--rig-handle rig))
    (llm-pi-ai-http-test--feed rig "two")
    (funcall (llm-pi-ai-http-test--rig-exit rig) 0 "")
    (llm-pi-ai-http-abort (llm-pi-ai-http-test--rig-handle rig))
    (should (equal (llm-pi-ai-http-test--body rig) "one"))
    (should (= (llm-pi-ai-http-test--rig-killed rig) 1))
    (should (= (length (llm-pi-ai-http-test--rig-dones rig)) 1))
    (should (eq (plist-get (car (llm-pi-ai-http-test--rig-dones rig)) :error)
                'aborted))))

(ert-deftest llm-pi-ai-http-test-curl-missing ()
  "A missing curl ends with `curl-missing', after the call returned."
  (let (dones handle
        (llm-pi-ai-http-curl-program "llm-pi-ai-no-such-curl-program"))
    (setq handle (llm-pi-ai-http-request :url "http://x"
                                         :on-done (lambda (r) (push r dones))))
    (should-not dones)
    (llm-pi-ai-http-test--wait (lambda () dones))
    (should (= (length dones) 1))
    (should (eq (plist-get (car dones) :error) 'curl-missing))
    (llm-pi-ai-http-abort handle)
    (should (= (length dones) 1))))

(ert-deftest llm-pi-ai-http-test-start-failure-is-network ()
  "Any other error starting the process is a `network' error."
  (let (dones
        (llm-pi-ai-http-start-function (lambda (&rest _) (error "Boom"))))
    (llm-pi-ai-http-request :url "http://x" :on-done (lambda (r) (push r dones)))
    (llm-pi-ai-http-test--wait (lambda () dones))
    (should (equal dones '((:error network :message "Boom"))))))

;;; Arguments

(ert-deftest llm-pi-ai-http-test-secrets-and-body-not-in-argv ()
  "Body goes to stdin as given, headers and URL to the config, none to argv."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run
     rig :url "https://h/v1?key=SECRETKEY" :method "POST"
     :headers '(("Authorization" . "Bearer SECRET") ("X-A" . "q\"uote"))
     :body "{\"k\":\"BODYTEXT 你\"}")
    (let ((argv (mapconcat #'identity (llm-pi-ai-http-test--rig-args rig) " ")))
      (should-not (string-match-p "SECRET\\|BODYTEXT\\|https://h" argv))
      (should (string-match-p "--data-binary @-" argv)))
    (should (equal (llm-pi-ai-http-test--rig-stdin rig) "{\"k\":\"BODYTEXT 你\"}"))
    (should (equal (llm-pi-ai-http-test--rig-config rig)
                   (concat "url = \"https://h/v1?key=SECRETKEY\"\n"
                           "request = \"POST\"\n"
                           "header = \"Authorization: Bearer SECRET\"\n"
                           "header = \"X-A: q\\\"uote\"\n")))))

(ert-deftest llm-pi-ai-http-test-no-body-no-data-option ()
  "Without BODY curl gets no `--data-binary'."
  (let ((rig (make-llm-pi-ai-http-test--rig)))
    (llm-pi-ai-http-test--run rig :url "http://x")
    (should-not (member "--data-binary" (llm-pi-ai-http-test--rig-args rig)))))

;;; SSE parser

(defun llm-pi-ai-http-test--sse (&rest chunks)
  "Feed CHUNKS to a fresh SSE parser; return the list of (EVENT DATA)."
  (let* (events
         (parse (llm-pi-ai-sse-parser (lambda (e d) (push (list e d) events)))))
    (dolist (c chunks) (funcall parse c))
    (nreverse events)))

(ert-deftest llm-pi-ai-http-test-sse-basic-and-split ()
  "An event cut at every position arrives once, whole."
  (let ((text "event: ping\ndata: {\"a\":1}\n\ndata: second\n\n"))
    (dotimes (cut (1+ (length text)))
      (should (equal (llm-pi-ai-http-test--sse (substring text 0 cut) (substring text cut))
                     '(("ping" "{\"a\":1}") (nil "second")))))))

(ert-deftest llm-pi-ai-http-test-sse-crlf-and-cr ()
  "CRLF and CR line ends work, also when CRLF is cut between reads."
  (let ((text "data: a\r\n\r\ndata: b\r\rdata: c\r\n\r\n"))
    (dotimes (cut (1+ (length text)))
      (should (equal (llm-pi-ai-http-test--sse (substring text 0 cut) (substring text cut))
                     '((nil "a") (nil "b") (nil "c")))))))

(ert-deftest llm-pi-ai-http-test-sse-comments-multiline-fields ()
  "Comments are ignored, data lines join with newlines, one leading space goes."
  (should (equal (llm-pi-ai-http-test--sse
                  ": keep-alive\n\ndata: l1\n: note\ndata:l2\ndata:  l3\nid: 7\nretry: 5\n\n")
                 '((nil "l1\nl2\n l3"))))
  (should (equal (llm-pi-ai-http-test--sse "data\n\n") '((nil ""))))
  (should-not (llm-pi-ai-http-test--sse "event: only\n\n"))
  (should-not (llm-pi-ai-http-test--sse "data: unfinished\n")))

;;; Real curl against a local Emacs server

(ert-deftest llm-pi-ai-http-test-real-curl-local-server ()
  "Real curl: status 418 with a body, secret in a header, body on stdin."
  (skip-unless (executable-find "curl"))
  (let* ((seen nil)
         (server
          (make-network-process
           :name "llm-pi-ai-http-test-server" :server t :host "127.0.0.1"
           :service t :family 'ipv4 :coding 'binary :noquery t
           :filter
           (lambda (conn string)
             (setq seen (concat seen string))
             (let* ((head-end (string-search "\r\n\r\n" seen))
                    (length (and head-end
                                 (string-match "Content-Length: \\([0-9]+\\)" seen)
                                 (string-to-number (match-string 1 seen))))
                    (body (and length (substring seen (+ head-end 4)))))
               (when (and body (>= (length body) length))
                 (process-send-string
                  conn
                  (concat "HTTP/1.1 418 Teapot\r\nConnection: close\r\n"
                          "X-T: yes\r\n\r\ncaf\303\251 "))
                 (sit-for 0.05)
                 (process-send-string conn body)
                 (delete-process conn))))))
         (port (process-contact server :service))
         chunks done)
    (unwind-protect
        (progn
          (llm-pi-ai-http-request
           :url (format "http://127.0.0.1:%d/x" port) :method "POST"
           :headers '(("Authorization" . "Bearer REALSECRET")
                      ("Content-Type" . "application/json"))
           :body "{\"msg\":\"你好\"}" :timeout 10
           :on-chunk (lambda (c) (push c chunks))
           :on-done (lambda (r) (setq done r)))
          (llm-pi-ai-http-test--wait (lambda () done))
          (should (eq (plist-get done :status) 418))
          (should (equal (cdr (assoc "x-t" (plist-get done :headers))) "yes"))
          (should (equal (apply #'concat (reverse chunks))
                         "café {\"msg\":\"你好\"}"))
          (should (string-match-p "Authorization: Bearer REALSECRET" seen))
          (should (string-match-p "\\`POST /x" seen)))
      (delete-process server))))

(provide 'llm-pi-ai-http-test)
;;; llm-pi-ai-http-test.el ends here
