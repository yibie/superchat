;;; superchat-bridge-http.el --- HTTP seam and SSE parser for pi-ai  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; No upstream file: @earendil-works/pi-ai@1.4.0 (commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8) reaches the network through
;; the `openai' npm client.  This file is the Emacs replacement, chosen
;; in `docs/HTTP.md': `make-process' running `curl'.
;;
;;   `superchat-bridge-http-request'  start a request, return a handle
;;   `superchat-bridge-http-abort'    cancel it
;;   `superchat-bridge-sse-parser'    server-sent events from body chunks
;;
;; How one request is run:
;;
;; - The URL and every header go into a curl config file (`-K FILE')
;;   made with mode 0600 and deleted when curl exits, so a secret such
;;   as an Authorization header is not in the process argument list.
;; - The body goes to curl's stdin as UTF-8 bytes (`--data-binary @-').
;; - curl runs with `-i'.  Its stdout is the status line and headers,
;;   an empty line, then the body.  This file parses the header block
;;   itself (skipping 1xx blocks), so a 4xx or 5xx response delivers
;;   its body through ON-CHUNK and ends with `(:status N ...)'.
;; - The body is decoded as UTF-8 incrementally: bytes of a character
;;   cut by a read boundary wait for the next read.
;;
;; `superchat-bridge-http-start-function' starts the process and is a variable
;; so that a test can feed bytes in pieces without a network.  It is
;; called with (ARGS CONFIG STDIN ON-OUTPUT ON-EXIT) and returns a
;; function of no arguments that kills the process.  ON-OUTPUT gets each
;; piece of curl's stdout as a unibyte string; ON-EXIT gets curl's exit
;; code and its stderr text, once.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar superchat-bridge-http-curl-program "curl"
  "Name or path of the curl program.")

(defvar superchat-bridge-http-start-function #'superchat-bridge-http--start-curl
  "Function that starts curl; see the Commentary of `superchat-bridge-http'.
It is called with (ARGS CONFIG STDIN ON-OUTPUT ON-EXIT) and returns a
function of no arguments that kills the process.  It signals
`superchat-bridge-http-curl-missing' when curl cannot be run.")

(define-error 'superchat-bridge-http-curl-missing "curl program not found")

;;; Process

(defvar superchat-bridge-http--config-files nil
  "The curl config files of requests still running; they hold credentials.")

(defun superchat-bridge-http--delete-config-files ()
  "Delete every curl config file left, as Emacs is killed mid-request."
  (dolist (file superchat-bridge-http--config-files)
    (ignore-errors (delete-file file))))

(add-hook 'kill-emacs-hook #'superchat-bridge-http--delete-config-files)

(defun superchat-bridge-http--start-curl (args config stdin on-output on-exit)
  "Run curl with ARGS and the config text CONFIG, writing STDIN to it.
Default value of `superchat-bridge-http-start-function'.  Call ON-OUTPUT with
each piece of stdout and ON-EXIT once with (CODE STDERR-TEXT).  Return
a function that kills the process and removes the config file."
  (let ((program (executable-find superchat-bridge-http-curl-program)))
    (unless program
      (signal 'superchat-bridge-http-curl-missing (list superchat-bridge-http-curl-program)))
    (let* ((config-file (make-temp-file "superchat-bridge-curl-"))
           (stderr-buffer (generate-new-buffer " *superchat-bridge-curl-stderr*"))
           (exited nil)
           (cleanup
            (lambda ()
              (setq superchat-bridge-http--config-files
                    (delete config-file superchat-bridge-http--config-files))
              (ignore-errors (delete-file config-file))))
           process)
      (condition-case err
          (progn
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region config nil config-file nil 'silent))
            (set-file-modes config-file #o600)
            (push config-file superchat-bridge-http--config-files)
            (setq process
                  (make-process
                   :name "superchat-bridge-curl"
                   :command (append (list program "-K" config-file) args)
                   :buffer nil
                   ;; Its own pipe, so that no "Process ... finished"
                   ;; line is written after curl's message.
                   :stderr (make-pipe-process
                            :name "superchat-bridge-curl-stderr" :buffer stderr-buffer
                            :noquery t :sentinel #'ignore)
                   :coding 'binary
                   :connection-type 'pipe
                   :noquery t
                   :filter (lambda (_proc string)
                             ;; curl has read its config by now.
                             (funcall cleanup)
                             (funcall on-output string))
                   :sentinel
                   (lambda (proc _event)
                     (when (and (not exited)
                                (memq (process-status proc) '(exit signal)))
                       (setq exited t)
                       (let ((errproc (get-buffer-process stderr-buffer)))
                         (when errproc (accept-process-output errproc 0)))
                       (let ((text (with-current-buffer stderr-buffer
                                     (buffer-string))))
                         (kill-buffer stderr-buffer)
                         (funcall cleanup)
                         (funcall on-exit (process-exit-status proc) text))))))
            (process-send-string process (encode-coding-string stdin 'utf-8-unix))
            (process-send-eof process))
        (error
         (funcall cleanup)
         (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
         (signal (car err) (cdr err))))
      (lambda ()
        (setq exited t)
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
        (funcall cleanup)))))

;;; Handle

(cl-defstruct (superchat-bridge-http-handle
               (:constructor superchat-bridge-http-handle--create)
               (:copier nil))
  "State of one request.
`kill' stops the process; `done' says ON-DONE was called or is due;
`raw' holds bytes not yet parsed (header block, or the cut end of a
UTF-8 character); `status' and `headers' are set once the final header
block is parsed."
  kill (done nil) (raw "") (status nil) (headers nil)
  on-headers on-chunk on-done)

(defun superchat-bridge-http--finish (handle result)
  "Call the ON-DONE of HANDLE with RESULT unless that already happened."
  (unless (superchat-bridge-http-handle-done handle)
    (setf (superchat-bridge-http-handle-done handle) t)
    (funcall (superchat-bridge-http-handle-on-done handle) result)))

(defun superchat-bridge-http--parse-header-block (block)
  "Parse the unibyte header BLOCK into (STATUS . HEADERS).
HEADERS is an alist of lower-case names; STATUS is nil when the first
line is not an HTTP status line."
  (let* ((lines (split-string (decode-coding-string block 'utf-8 t) "\r?\n"))
         (status (and (string-match "\\`HTTP/[0-9.]+ +\\([0-9]+\\)" (car lines))
                      (string-to-number (match-string 1 (car lines)))))
         headers)
    (dolist (line (cdr lines))
      (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line))
              headers)))
    (cons status (nreverse headers))))

(defun superchat-bridge-http--utf8-complete-length (bytes)
  "Return how many leading bytes of unibyte BYTES end on a character edge."
  (let* ((len (length bytes))
         (cut len)
         (i (1- len))
         (stop (max 0 (- len 4))))
    (while (and (>= i stop) (= cut len))
      (let ((b (aref bytes i)))
        (cond
         ((< b #x80) (setq i -1))
         ((>= b #xC0)
          (let ((need (cond ((< b #xE0) 2) ((< b #xF0) 3) (t 4))))
            (when (< (- len i) need) (setq cut i))
            (setq i -1)))
         (t (setq i (1- i))))))
    cut))

(defun superchat-bridge-http--deliver-body (handle bytes &optional flush)
  "Decode BYTES (unibyte) as UTF-8 and pass the text to ON-CHUNK.
Bytes of a cut character are kept in HANDLE until the next call; with
FLUSH, whatever is kept is delivered too."
  (let* ((all (concat (superchat-bridge-http-handle-raw handle) bytes))
         (cut (if flush (length all) (superchat-bridge-http--utf8-complete-length all))))
    (setf (superchat-bridge-http-handle-raw handle) (substring all cut))
    (when (> cut 0)
      (funcall (superchat-bridge-http-handle-on-chunk handle)
               (decode-coding-string (substring all 0 cut) 'utf-8 t)))))

(defun superchat-bridge-http--on-output (handle bytes)
  "Take BYTES of curl stdout for HANDLE."
  (unless (superchat-bridge-http-handle-done handle)
    (if (superchat-bridge-http-handle-status handle)
        (superchat-bridge-http--deliver-body handle bytes)
      (let ((all (concat (superchat-bridge-http-handle-raw handle) bytes))
            (continue t))
        (setf (superchat-bridge-http-handle-raw handle) all)
        (while (and continue (not (superchat-bridge-http-handle-done handle)))
          (let* ((raw (superchat-bridge-http-handle-raw handle))
                 (end (string-match "\r?\n\r?\n" raw)))
            (if (not end)
                (setq continue nil)
              (let* ((rest (substring raw (match-end 0)))
                     (parsed (superchat-bridge-http--parse-header-block
                              (substring raw 0 end))))
                (setf (superchat-bridge-http-handle-raw handle) rest)
                (when (and (car parsed) (>= (car parsed) 200))
                  (setf (superchat-bridge-http-handle-status handle) (car parsed)
                        (superchat-bridge-http-handle-headers handle) (cdr parsed))
                  (when (superchat-bridge-http-handle-on-headers handle)
                    (funcall (superchat-bridge-http-handle-on-headers handle)
                             (car parsed) (cdr parsed)))
                  (setq continue nil)
                  (let ((body (superchat-bridge-http-handle-raw handle)))
                    (setf (superchat-bridge-http-handle-raw handle) "")
                    (when (and (> (length body) 0)
                               (not (superchat-bridge-http-handle-done handle)))
                      (superchat-bridge-http--deliver-body handle body))))))))))))

(defun superchat-bridge-http--on-exit (handle code stderr)
  "End HANDLE after curl exited with CODE and wrote STDERR."
  (unless (superchat-bridge-http-handle-done handle)
    (cond
     ((/= code 0)
      (let ((message (string-trim stderr)))
        (superchat-bridge-http--finish
         handle
         (list :error (if (= code 28) 'timeout 'network)
               :message (if (string-empty-p message)
                            (format "curl exited with code %d" code)
                          message)))))
     ((not (superchat-bridge-http-handle-status handle))
      (superchat-bridge-http--finish
       handle (list :error 'network :message "No HTTP response received")))
     (t
      (superchat-bridge-http--deliver-body handle "" t)
      (superchat-bridge-http--finish
       handle (list :status (superchat-bridge-http-handle-status handle)
                    :headers (superchat-bridge-http-handle-headers handle)))))))

(defun superchat-bridge-http--quote (string)
  "Return STRING as a double-quoted curl config value."
  (concat "\""
          (replace-regexp-in-string
           "[\\\"\n\r\t]"
           (lambda (m) (pcase m
                         ("\n" "\\n") ("\r" "\\r") ("\t" "\\t")
                         (_ (concat "\\" m))))
           string t t)
          "\""))

(defun superchat-bridge-http--config (url method headers)
  "Return curl config text for URL, METHOD (or nil) and HEADERS."
  (concat "url = " (superchat-bridge-http--quote url) "\n"
          (and method
               (concat "request = " (superchat-bridge-http--quote method) "\n"))
          (mapconcat (lambda (h)
                       (concat "header = "
                               (superchat-bridge-http--quote
                                (format "%s: %s" (car h) (cdr h)))
                               "\n"))
                     headers "")))

;;; Entry points

(cl-defun superchat-bridge-http-request (&key url method headers body timeout
                                       on-headers on-chunk on-done)
  "Start an HTTP request to URL and return a handle for it.
METHOD is a string or nil (curl picks GET, or POST with BODY).  HEADERS
is an alist of strings.  BODY is a string sent as UTF-8 on curl's
stdin.  TIMEOUT is seconds for the whole request, or nil.

ON-HEADERS, if given, is called once with (STATUS HEADERS), HEADERS an
alist with lower-case names.  ON-CHUNK is called with each decoded piece
of the response body.  ON-DONE is called exactly once: with
\(:status N :headers ALIST) when a response ended, whatever N is, or
\(:error KIND :message S) with KIND `curl-missing', `network', `timeout'
or `aborted'.  ON-DONE is never called before this function returns."
  (let* ((handle (superchat-bridge-http-handle--create
                  :on-headers on-headers
                  :on-chunk (or on-chunk #'ignore)
                  :on-done (or on-done #'ignore)))
         (args (append (list "-sS" "-N" "-i" "--suppress-connect-headers")
                       (and timeout
                            (list "--max-time" (format "%s" timeout)))
                       (and body (list "--data-binary" "@-"))))
         (fail (lambda (kind message)
                 (run-at-time 0 nil #'superchat-bridge-http--finish handle
                              (list :error kind :message message)))))
    (condition-case err
        (setf (superchat-bridge-http-handle-kill handle)
              (funcall superchat-bridge-http-start-function
                       args
                       (superchat-bridge-http--config url method headers)
                       (or body "")
                       (lambda (bytes) (superchat-bridge-http--on-output handle bytes))
                       (lambda (code stderr)
                         (superchat-bridge-http--on-exit handle code stderr))))
      (superchat-bridge-http-curl-missing
       (funcall fail 'curl-missing
                (format "curl not found: %s" (cadr err))))
      (error
       (funcall fail 'network (error-message-string err))))
    handle))

(defun superchat-bridge-http-abort (handle)
  "Cancel the request HANDLE.
No ON-CHUNK call follows; ON-DONE is called once with `aborted', unless
it was already called."
  (unless (superchat-bridge-http-handle-done handle)
    (let ((kill (superchat-bridge-http-handle-kill handle)))
      (superchat-bridge-http--finish handle (list :error 'aborted
                                           :message "Request aborted"))
      (when kill (ignore-errors (funcall kill))))))

;;; Server-sent events

(defun superchat-bridge-sse-parser (on-event)
  "Return a function that takes body chunks and calls ON-EVENT.
ON-EVENT is called with (EVENT DATA) for each complete server-sent
event: EVENT the `event:' field or nil, DATA the `data:' lines joined
with newlines.  Handles LF, CRLF and CR line ends, comment lines,
multi-line data, and events split across chunks.  An event without
data is dropped; a trailing event not ended by an empty line is not
delivered."
  (let ((pending "") event (data nil))
    (lambda (chunk)
      (setq pending (concat pending chunk))
      (let ((start 0) (more t))
        (while (and more (string-match "\r\n\\|\n\\|\r" pending start))
          (let ((line-end (match-beginning 0))
                (next (match-end 0)))
            (if (and (= next (length pending))
                     (= (- next line-end) 1)
                     (eq (aref pending line-end) ?\r))
                ;; A lone CR at the end may be the first half of CRLF.
                (setq more nil)
              (let ((line (substring pending start line-end)))
                (setq start next)
                (cond
                 ((string-empty-p line)
                  (when data
                    (let ((e event)
                          (d (mapconcat #'identity (nreverse data) "\n")))
                      (setq event nil data nil)
                      (funcall on-event e d)))
                  (setq event nil data nil))
                 ((eq (aref line 0) ?:))
                 (t
                  (let* ((colon (string-search ":" line))
                         (field (if colon (substring line 0 colon) line))
                         (value (if colon (substring line (1+ colon)) "")))
                    (when (string-prefix-p " " value)
                      (setq value (substring value 1)))
                    (cond ((string= field "event") (setq event value))
                          ((string= field "data") (push value data))))))))))
        (setq pending (substring pending start))))))

(provide 'superchat-bridge-http)
;;; superchat-bridge-http.el ends here
