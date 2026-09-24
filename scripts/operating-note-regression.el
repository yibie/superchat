;;; operating-note-regression.el --- The Emacs operating note -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract: docs/decisions/accepted/2026-09-20-emacs-operating-note.md
;; What is pinned here is that the note reaches the provider as agreed.  That a
;; real model does better for it is NOT pinned anywhere: by the user's choice
;; it has not been verified.
;;
;;   emacs -Q --batch ... -l scripts/operating-note-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^operating-note/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst on--marker "Working in the user's Emacs:"
  "The built-in note's first line.")

(defvar on--systems nil
  "The system text of every provider request of this fixture, newest first.")

(defmacro on--fixture (script &rest body)
  "Run BODY with SCRIPT, capturing the system text each request really carries.
The fixture's `:prompt-text' holds the interactions only; the system text is the
prompt's context."
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((on--systems nil)
           (superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-approval-function (lambda (_request) t))
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil))
       (unwind-protect
           (smoke--with-fixture ,script
             (let ((network (symbol-function 'smoke--fake-network)))
               (cl-letf (((symbol-function 'smoke--fake-network)
                          (lambda (mode provider prompt partial response error multi)
                            (push (copy-sequence (or (llm-chat-prompt-context prompt) ""))
                                  on--systems)
                            (funcall network mode provider prompt
                                     partial response error multi))))
                 ,@body)))
         (setq superchat-llm-tools-list nil)))))

(defun on--settle (run)
  (should (stringp run))
  (should (smoke--wait
           (lambda () (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime-status run) :status)))
           20))
  run)

(defun on--count (needle text)
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq start (match-end 0) count (1+ count)))
    count))

(defun on--request-texts ()
  "The system text of every request, oldest first."
  (reverse on--systems))

(ert-deftest operating-note/a-default-chat-request-carries-it-once ()
  "With the default tools the note is in the request, once, and once again in
the request that follows a tool round."
  (on--fixture (list '(:tool-uses (("list_buffers"))) '(:text "done"))
    (let ((view (smoke--open-chat)))
      (on--settle (smoke--send view "bind a key for me"))
      (let ((texts (on--request-texts)))
        (should (= 2 (length texts)))
        (dolist (text texts)
          (should (= 1 (on--count on--marker text)))
          (should (string-match-p (regexp-quote "(kbd \"C-c t h\")") text)))))))

(ert-deftest operating-note/it-follows-the-final-tool-surface ()
  "Guard: no Emacs-changing tool on the final surface, no note."
  (on--fixture (list '(:text "a") '(:text "b"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view)))
      (dolist (tools (list '("list_buffers" "read_buffer") 'none))
        (on--settle
         (plist-get (superchat-service-call
                     nil 'driver 'submit
                     (list :surface 'chat :session-id session :tools tools
                           :content "just look"
                           :context (list :host-buffer view)
                           :runtime-extras '(:record-conversation nil)
                           :sink (superchat-sink-discard)))
                    :run-id)))
      (dolist (text (on--request-texts))
        (should (= 0 (on--count on--marker text)))))))

(ert-deftest operating-note/a-rewrite-that-takes-the-tools-away-takes-the-note-away ()
  "A `before-provider-request' middleware narrows the tools to a read: the note
is rendered against the FINAL surface, so that request does not carry it."
  (on--fixture (list '(:text "done"))
    (let* ((view (smoke--open-chat))
           (off (superchat-on nil 'before-provider-request 'on-narrow
                              (lambda (candidate next)
                                ;; `ask_user' is the trusted input tool: a rewrite may not drop it.
                                (funcall next (plist-put candidate :tools
                                                         '("read_buffer" "ask_user")))))))
      (unwind-protect
          (on--settle (smoke--send view "just read"))
        (funcall off))
      (should (equal '("read_buffer" "ask_user") (plist-get (car smoke--requests) :tools)))
      (should (= 0 (on--count on--marker (car (on--request-texts))))))))

(ert-deftest operating-note/the-user-can-turn-it-off-or-replace-it ()
  (on--fixture (list '(:text "a") '(:text "b"))
    (let ((view (smoke--open-chat)))
      (let ((superchat-emacs-operating-note nil))
        (on--settle (smoke--send view "first")))
      (let ((superchat-emacs-operating-note "ON_MY_OWN_NOTE: be careful."))
        (on--settle (smoke--send view "second")))
      (let ((texts (on--request-texts)))
        (should (= 0 (on--count on--marker (nth 0 texts))))
        (should (= 0 (on--count "ON_MY_OWN_NOTE" (nth 0 texts))))
        (should (= 1 (on--count "ON_MY_OWN_NOTE: be careful." (nth 1 texts))))
        (should (= 0 (on--count on--marker (nth 1 texts))))))))

(ert-deftest operating-note/the-current-buffer-entry-carries-it-too ()
  (on--fixture (list '(:text "done"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun on-sample () 1)\n")
      (let* ((plan (superchat-act "explain this"))
             (run (plist-get (plist-get plan :run-handle) :run-id)))
        (on--settle run)
        (should (= 1 (on--count on--marker (car (on--request-texts)))))))))

(ert-deftest operating-note/the-current-buffer-entry-follows-the-users-tool-list ()
  "The current-buffer entry collects before its tools are chosen.  \"Not decided
yet\" means the user's tool list, not the built-in default: a list that holds
no Emacs-changing tool gets no note.  Found while building the reference index
\(2026-09-20), which has the same rule."
  (on--fixture (list '(:text "a"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun on-sample () 1)\n")
      (let* ((superchat-llm-tool-names '("list_buffers" "read_buffer"))
             (superchat-llm-tools-list nil)
             (plan (superchat-act "explain this"))
             (run (plist-get (plist-get plan :run-handle) :run-id)))
        (on--settle run)
        (should-not (cl-intersection '("eval_elisp" "edit_buffer")
                                     (plist-get (car smoke--requests) :tools)
                                     :test #'equal))
        (should (= 0 (on--count on--marker (car (on--request-texts)))))))))

(ert-deftest operating-note/the-built-in-text-keeps-to-its-contract ()
  "Short, about the observed mistakes, and never an instruction to persist."
  (should (boundp 'superchat-emacs-operating-note-text))
  (let ((text superchat-emacs-operating-note-text)
        (case-fold-search t))
    (should (string-prefix-p on--marker text))
    (should (<= (length text) 1024))
    (dolist (needle '("(kbd " "eval-buffer" "user-init-file" "lookup-key" "progn"
                      "fundamental-mode" "unsaved" "saving is the user's"))
      (should (string-match-p (regexp-quote needle) text)))
    ;; 2026-09-20 correction: a user's init buffer is not something to
    ;; evaluate whole, and whether a buffer is unsaved is observed, not assumed
    ;; (the user may have saved it between two Runs).
    (dolist (needle '("only the forms you changed" "not the whole buffer"
                      "buffer-modified-p"))
      (should (string-match-p (regexp-quote needle) text)))
    ;; 2026-09-24 (user: "A"): a check reads state; running a command to see
    ;; whether it works made the user answer its prompt.
    (dolist (needle '("commandp" "never run a command to test it"))
      (should (string-match-p (regexp-quote needle) text)))
    (should-not (string-match-p "(with-current-buffer NAME (eval-buffer))\\|say it is unsaved"
                                text))
    (should-not (string-match-p
                 "for persistence\\|fresh emacs\\|\\bsave \\(the\\|it\\|your\\|that\\|this\\) \\|save-buffer\\|write-region\\|write-file"
                 text))))

(provide 'operating-note-regression)

;;; operating-note-regression.el ends here
