;;; superchat-bridge-http-test.el --- Tests for superchat-bridge-http  -*- lexical-binding: t; -*-

;; Tests for the HTTP seam.  No upstream test exists.  A fake start
;; function feeds bytes in pieces; one test runs real curl against an
;; Emacs server on 127.0.0.1 (no outside network).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-bridge-http)

;;; Helpers

(cl-defstruct superchat-bridge-http-test--rig
  "A fake curl: `output' and `exit' are the two callbacks the seam gave."
  output exit args config stdin (killed 0)
  chunks dones headers-calls handle)

(defun superchat-bridge-http-test--run (rig &rest keys)
  "Start a request on RIG (fake start function) with request KEYS."
  (let ((superchat-bridge-http-start-function
         (lambda (args config stdin on-output on-exit)
           (setf (superchat-bridge-http-test--rig-args rig) args
                 (superchat-bridge-http-test--rig-config rig) config
                 (superchat-bridge-http-test--rig-stdin rig) stdin
                 (superchat-bridge-http-test--rig-output rig) on-output
                 (superchat-bridge-http-test--rig-exit rig) on-exit)
           (lambda () (cl-incf (superchat-bridge-http-test--rig-killed rig))))))
    (setf (superchat-bridge-http-test--rig-handle rig)
          (apply #'superchat-bridge-http-request
                 :on-headers (lambda (s h)
                               (push (list s h)
                                     (superchat-bridge-http-test--rig-headers-calls rig)))
                 :on-chunk (lambda (c) (push c (superchat-bridge-http-test--rig-chunks rig)))
                 :on-done (lambda (r) (push r (superchat-bridge-http-test--rig-dones rig)))
                 keys))))

(defun superchat-bridge-http-test--feed (rig &rest pieces)
  "Feed PIECES (strings, made unibyte) to RIG as curl stdout reads."
  (dolist (p pieces)
    (funcall (superchat-bridge-http-test--rig-output rig) (encode-coding-string p 'utf-8))))

(defun superchat-bridge-http-test--body (rig)
  "Return all chunks of RIG joined."
  (apply #'concat (reverse (superchat-bridge-http-test--rig-chunks rig))))

(defun superchat-bridge-http-test--wait (pred &optional seconds)
  "Wait until PRED is non-nil, at most SECONDS (default 10)."
  (let ((end (+ (float-time) (or seconds 10))))
    (while (and (not (funcall pred)) (< (float-time) end))
      (accept-process-output nil 0.02))))

(defconst superchat-bridge-http-test--head "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n")

;;; Body decoding

(ert-deftest superchat-bridge-http-test-split-multibyte-character ()
  "A character cut across two reads is delivered whole."
  (dolist (text '("héllo" "你好" "a\U0001F600b"))
    (let* ((bytes (encode-coding-string text 'utf-8))
           (rig (make-superchat-bridge-http-test--rig)))
      ;; Cut at every byte position of the body.
      (dotimes (cut (1+ (length bytes)))
        (setf (superchat-bridge-http-test--rig-chunks rig) nil)
        (superchat-bridge-http-test--run rig :url "http://x")
        (funcall (superchat-bridge-http-test--rig-output rig)
                 (concat (encode-coding-string superchat-bridge-http-test--head 'utf-8)
                         (substring bytes 0 cut)))
        (funcall (superchat-bridge-http-test--rig-output rig) (substring bytes cut))
        (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
        (should (equal (superchat-bridge-http-test--body rig) text))
        (dolist (c (superchat-bridge-http-test--rig-chunks rig))
          (should (cl-every (lambda (ch) (< ch #x3fff80)) c)))))))

(ert-deftest superchat-bridge-http-test-header-split-and-body-in-same-read ()
  "Headers split over reads parse; body bytes after them in the same read arrive."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (superchat-bridge-http-test--feed rig "HTTP/1.1 200 O" "K\r\nX-Request-Id: ab\r\nContent-Ty"
                               "pe: text/plain\r\n\r\nhel" "lo")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (should (equal (superchat-bridge-http-test--body rig) "hello"))
    (should (equal (superchat-bridge-http-test--rig-headers-calls rig)
                   '((200 (("x-request-id" . "ab") ("content-type" . "text/plain"))))))
    (should (equal (superchat-bridge-http-test--rig-dones rig)
                   '((:status 200 :headers (("x-request-id" . "ab")
                                            ("content-type" . "text/plain"))))))))

(ert-deftest superchat-bridge-http-test-skips-1xx-block ()
  "A `100 Continue' block before the real one is not the status."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (superchat-bridge-http-test--feed rig "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 201 Created\r\n\r\nok")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (should (equal (superchat-bridge-http-test--body rig) "ok"))
    (should (= (plist-get (car (superchat-bridge-http-test--rig-dones rig)) :status) 201))))

(ert-deftest superchat-bridge-http-test-error-status-with-body ()
  "A 429 with a JSON body delivers the body and ends with `:status 429'."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (superchat-bridge-http-test--feed rig "HTTP/1.1 429 Too Many Requests\r\nRetry-After: 3\r\n\r\n"
                               "{\"error\":{\"message\":\"slow 下\"}}")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (should (equal (superchat-bridge-http-test--body rig) "{\"error\":{\"message\":\"slow 下\"}}"))
    (let ((done (car (superchat-bridge-http-test--rig-dones rig))))
      (should (= (plist-get done :status) 429))
      (should-not (plist-get done :error))
      (should (equal (cdr (assoc "retry-after" (plist-get done :headers))) "3")))
    (should (= (length (superchat-bridge-http-test--rig-dones rig)) 1))))

;;; Done, errors, abort

(ert-deftest superchat-bridge-http-test-exit-28-is-timeout ()
  "Exit code 28 maps to `timeout'; the message is curl's stderr."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x" :timeout 5)
    (should (equal (cl-subseq (superchat-bridge-http-test--rig-args rig) 4 6)
                   '("--max-time" "5")))
    (superchat-bridge-http-test--feed rig superchat-bridge-http-test--head "partial")
    (funcall (superchat-bridge-http-test--rig-exit rig) 28
             "curl: (28) Operation timed out\n")
    (should (equal (superchat-bridge-http-test--rig-dones rig)
                   '((:error timeout :message "curl: (28) Operation timed out"))))))

(ert-deftest superchat-bridge-http-test-other-exit-is-network ()
  "Other non-zero exit codes map to `network'; empty stderr gets a message."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (funcall (superchat-bridge-http-test--rig-exit rig) 6 "curl: (6) Could not resolve host: x\n")
    (should (equal (superchat-bridge-http-test--rig-dones rig)
                   '((:error network :message "curl: (6) Could not resolve host: x")))))
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (funcall (superchat-bridge-http-test--rig-exit rig) 7 "")
    (should (equal (plist-get (car (superchat-bridge-http-test--rig-dones rig)) :message)
                   "curl exited with code 7"))))

(ert-deftest superchat-bridge-http-test-exit-without-response-is-network ()
  "Exit 0 before any header block is a `network' error."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (should (eq (plist-get (car (superchat-bridge-http-test--rig-dones rig)) :error) 'network))))

(ert-deftest superchat-bridge-http-test-done-exactly-once ()
  "A second exit, or output after done, does nothing."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (superchat-bridge-http-test--feed rig superchat-bridge-http-test--head "a")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (funcall (superchat-bridge-http-test--rig-exit rig) 28 "late")
    (superchat-bridge-http-test--feed rig "b")
    (should (= (length (superchat-bridge-http-test--rig-dones rig)) 1))
    (should (equal (superchat-bridge-http-test--body rig) "a"))))

(ert-deftest superchat-bridge-http-test-abort ()
  "After abort no chunk arrives and ON-DONE runs once with `aborted'."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (superchat-bridge-http-test--feed rig superchat-bridge-http-test--head "one")
    (superchat-bridge-http-abort (superchat-bridge-http-test--rig-handle rig))
    (superchat-bridge-http-test--feed rig "two")
    (funcall (superchat-bridge-http-test--rig-exit rig) 0 "")
    (superchat-bridge-http-abort (superchat-bridge-http-test--rig-handle rig))
    (should (equal (superchat-bridge-http-test--body rig) "one"))
    (should (= (superchat-bridge-http-test--rig-killed rig) 1))
    (should (= (length (superchat-bridge-http-test--rig-dones rig)) 1))
    (should (eq (plist-get (car (superchat-bridge-http-test--rig-dones rig)) :error)
                'aborted))))

(ert-deftest superchat-bridge-http-test-curl-missing ()
  "A missing curl ends with `curl-missing', after the call returned."
  (let (dones handle
        (superchat-bridge-http-curl-program "superchat-bridge-no-such-curl-program"))
    (setq handle (superchat-bridge-http-request :url "http://x"
                                         :on-done (lambda (r) (push r dones))))
    (should-not dones)
    (superchat-bridge-http-test--wait (lambda () dones))
    (should (= (length dones) 1))
    (should (eq (plist-get (car dones) :error) 'curl-missing))
    (superchat-bridge-http-abort handle)
    (should (= (length dones) 1))))

(ert-deftest superchat-bridge-http-test-start-failure-is-network ()
  "Any other error starting the process is a `network' error."
  (let (dones
        (superchat-bridge-http-start-function (lambda (&rest _) (error "Boom"))))
    (superchat-bridge-http-request :url "http://x" :on-done (lambda (r) (push r dones)))
    (superchat-bridge-http-test--wait (lambda () dones))
    (should (equal dones '((:error network :message "Boom"))))))

;;; Arguments

(ert-deftest superchat-bridge-http-test-secrets-and-body-not-in-argv ()
  "Body goes to stdin as given, headers and URL to the config, none to argv."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run
     rig :url "https://h/v1?key=SECRETKEY" :method "POST"
     :headers '(("Authorization" . "Bearer SECRET") ("X-A" . "q\"uote"))
     :body "{\"k\":\"BODYTEXT 你\"}")
    (let ((argv (mapconcat #'identity (superchat-bridge-http-test--rig-args rig) " ")))
      (should-not (string-match-p "SECRET\\|BODYTEXT\\|https://h" argv))
      (should (string-match-p "--data-binary @-" argv)))
    (should (equal (superchat-bridge-http-test--rig-stdin rig) "{\"k\":\"BODYTEXT 你\"}"))
    (should (equal (superchat-bridge-http-test--rig-config rig)
                   (concat "url = \"https://h/v1?key=SECRETKEY\"\n"
                           "request = \"POST\"\n"
                           "header = \"Authorization: Bearer SECRET\"\n"
                           "header = \"X-A: q\\\"uote\"\n")))))

(ert-deftest superchat-bridge-http-test-no-body-no-data-option ()
  "Without BODY curl gets no `--data-binary'."
  (let ((rig (make-superchat-bridge-http-test--rig)))
    (superchat-bridge-http-test--run rig :url "http://x")
    (should-not (member "--data-binary" (superchat-bridge-http-test--rig-args rig)))))

;;; SSE parser

(defun superchat-bridge-http-test--sse (&rest chunks)
  "Feed CHUNKS to a fresh SSE parser; return the list of (EVENT DATA)."
  (let* (events
         (parse (superchat-bridge-sse-parser (lambda (e d) (push (list e d) events)))))
    (dolist (c chunks) (funcall parse c))
    (nreverse events)))

(ert-deftest superchat-bridge-http-test-sse-basic-and-split ()
  "An event cut at every position arrives once, whole."
  (let ((text "event: ping\ndata: {\"a\":1}\n\ndata: second\n\n"))
    (dotimes (cut (1+ (length text)))
      (should (equal (superchat-bridge-http-test--sse (substring text 0 cut) (substring text cut))
                     '(("ping" "{\"a\":1}") (nil "second")))))))

(ert-deftest superchat-bridge-http-test-sse-crlf-and-cr ()
  "CRLF and CR line ends work, also when CRLF is cut between reads."
  (let ((text "data: a\r\n\r\ndata: b\r\rdata: c\r\n\r\n"))
    (dotimes (cut (1+ (length text)))
      (should (equal (superchat-bridge-http-test--sse (substring text 0 cut) (substring text cut))
                     '((nil "a") (nil "b") (nil "c")))))))

(ert-deftest superchat-bridge-http-test-sse-comments-multiline-fields ()
  "Comments are ignored, data lines join with newlines, one leading space goes."
  (should (equal (superchat-bridge-http-test--sse
                  ": keep-alive\n\ndata: l1\n: note\ndata:l2\ndata:  l3\nid: 7\nretry: 5\n\n")
                 '((nil "l1\nl2\n l3"))))
  (should (equal (superchat-bridge-http-test--sse "data\n\n") '((nil ""))))
  (should-not (superchat-bridge-http-test--sse "event: only\n\n"))
  (should-not (superchat-bridge-http-test--sse "data: unfinished\n")))

;;; Real curl against a local Emacs server

(ert-deftest superchat-bridge-http-test-real-curl-local-server ()
  "Real curl: status 418 with a body, secret in a header, body on stdin."
  (skip-unless (executable-find "curl"))
  (let* ((seen nil)
         (server
          (make-network-process
           :name "superchat-bridge-http-test-server" :server t :host "127.0.0.1"
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
          (superchat-bridge-http-request
           :url (format "http://127.0.0.1:%d/x" port) :method "POST"
           :headers '(("Authorization" . "Bearer REALSECRET")
                      ("Content-Type" . "application/json"))
           :body "{\"msg\":\"你好\"}" :timeout 10
           :on-chunk (lambda (c) (push c chunks))
           :on-done (lambda (r) (setq done r)))
          (superchat-bridge-http-test--wait (lambda () done))
          (should (eq (plist-get done :status) 418))
          (should (equal (cdr (assoc "x-t" (plist-get done :headers))) "yes"))
          (should (equal (apply #'concat (reverse chunks))
                         "café {\"msg\":\"你好\"}"))
          (should (string-match-p "Authorization: Bearer REALSECRET" seen))
          (should (string-match-p "\\`POST /x" seen)))
      (delete-process server))))

(provide 'superchat-bridge-http-test)
;;; superchat-bridge-http-test.el ends here
