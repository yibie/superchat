;;; edit-check-regression.el --- A broken Lisp edit is sent back -*- lexical-binding: t; -*-

;;; Commentary:

;; A structured edit that would turn a Lisp buffer that reads into one that
;; does not is failed by the tool itself: the buffer is left exactly as it
;; was and the result tells the model what is wrong.  Contract:
;; docs/decisions/accepted/2026-09-19-a-broken-lisp-edit-is-sent-back.md
;;
;;   emacs -Q --batch ... -l scripts/edit-check-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^edit-check/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst ec--init
  ";;; init.el --- isolated ABC experiment configuration -*- lexical-binding: t; -*-
(setq-default fill-column 80)
(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd \"C-c t h\") #'ignore))
;; The loader sets custom-file to this home's custom.el before loading init.
(when (file-readable-p custom-file)
  (load custom-file nil t))
"
  "The init file of the attended acceptance fixture, byte for byte.")

;; The model's actual arguments in m7-attended-chat-02.
(defconst ec--old "  (define-key emacs-lisp-mode-map (kbd \"C-c t h\") #'ignore)")
(defconst ec--bad "  (define-key emacs-lisp-mode-map (kbd \"C-c t h\") #'describe-function))")
(defconst ec--good "  (define-key emacs-lisp-mode-map (kbd \"C-c t h\") #'describe-function)")

(defmacro ec--with-dir (dir &rest body)
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "ec-" t)))))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,dir (file-truename file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory ,dir t))))

(defun ec--visit (dir name text)
  "Visit a new file NAME in DIR holding TEXT; return the buffer."
  (let ((file (expand-file-name name dir))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)
    (find-file-noselect file)))

(defmacro ec--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode 'edit-buffers)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun ec--text (buffer)
  (with-current-buffer buffer
    (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest edit-check/the-edit-of-the-attended-run-is-sent-back-and-leaves-no-trace ()
  "The exact call of m7-attended-chat-02: one closing parenthesis too many."
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      (let* ((buffer (ec--visit dir "init.el" ec--init))
             (changes 0)
             (superchat-tool-mutation-target nil)
             undo-before result)
        (with-current-buffer buffer
          (should (derived-mode-p 'emacs-lisp-mode))
          (add-hook 'after-change-functions (lambda (&rest _) (cl-incf changes)) nil t)
          (setq undo-before buffer-undo-list))
        (setq result (superchat-tool-edit-buffer "init.el" ec--old ec--bad))
        (should (string-prefix-p "Error:" result))
        (should (string-match-p "line 4" result))
        (should (string-match-p "not changed\\|unchanged" result))
        (should (string-match-p (regexp-quote "#'describe-function)))") result))
        (should (equal ec--init (ec--text buffer)))
        (should-not (buffer-modified-p buffer))
        (should (eq undo-before (buffer-local-value 'buffer-undo-list buffer)))
        (should (= 0 changes))
        (should-not (file-exists-p (expand-file-name ".#init.el" dir)))
        (should-not (file-symlink-p (expand-file-name ".#init.el" dir)))
        (should-not superchat-tool-mutation-target)))))

(ert-deftest edit-check/the-correct-edit-answers-exactly-as-before ()
  "Guard: reads before, reads after -- not one word is added."
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      (let* ((buffer (ec--visit dir "init.el" ec--init))
             (result (superchat-tool-edit-buffer "init.el" ec--old ec--good)))
        (should (string-prefix-p
                 "Replaced 1 occurrence in buffer init.el. This tool does not save or evaluate the buffer.\nTarget: buffer init.el"
                 result))
        (should (string-suffix-p "Layer: in memory only; the buffer was not saved or evaluated" result))
        (should-not (string-match-p "read\\|balanc\\|parenthes" (downcase result)))
        (should (buffer-modified-p buffer))
        (should (string-match-p (regexp-quote "#'describe-function))\n") (ec--text buffer)))))))

(ert-deftest edit-check/sent-back-is-a-tool-failure-the-model-is-told-to-retry ()
  "The existing feedback reads the result as a failed call, and not as one of
the user-or-policy refusals it tells the model never to retry."
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      (ec--visit dir "init.el" ec--init)
      (let* ((result (superchat-tool-edit-buffer "init.el" ec--old ec--bad))
             (entries (list (cons "edit_buffer" result))))
        (should (superchat--llm-tool-error-entries entries))
        (should-not (superchat--llm-tool-error-non-retryable-p entries))))))

(ert-deftest edit-check/a-buffer-that-was-already-broken-is-edited-and-told-the-state ()
  "Broken before: the edit cannot be blamed and may be the repair, so it
applies; one line of fact is added."
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      (let ((buffer (ec--visit dir "broken.el" "(defun a () 1))\n(defun b () 2)\n")))
        ;; An edit that leaves it broken still applies, and says where.
        (let ((result (superchat-tool-edit-buffer "broken.el" "(defun b () 2)" "(defun b () 3)")))
          (should (string-prefix-p "Replaced 1 occurrence" result))
          (should (string-match-p "did not read as Lisp before" result))
          (should (string-match-p "still does not" result))
          (should (string-match-p "line 1" result))
          (should (string-match-p "(defun b () 3)" (ec--text buffer))))
        ;; The repair applies, and says it reads now.
        (let ((result (superchat-tool-edit-buffer "broken.el" "(defun a () 1))" "(defun a () 1)")))
          (should (string-prefix-p "Replaced 1 occurrence" result))
          (should (string-match-p "now reads as Lisp" result))
          (should (equal "(defun a () 1)\n(defun b () 3)\n" (ec--text buffer))))))))

(ert-deftest edit-check/only-lisp-buffers-and-only-real-parentheses-count ()
  "Guard: another mode is not checked; a parenthesis in a comment or a string
is not a parenthesis; a narrowed edit is judged on the whole buffer."
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      ;; Balanced prose made unbalanced: in a Lisp buffer this would be sent
      ;; back; here nothing is checked and nothing is said.
      (let* ((text (ec--visit dir "notes.txt" "one two\n"))
             (result (superchat-tool-edit-buffer "notes.txt" "one" "one ((")))
        (should (string-prefix-p "Replaced 1 occurrence" result))
        (should-not (string-match-p "Note:\\|Error" result))
        (should (equal "one (( two\n" (ec--text text))))
      (let ((lisp (ec--visit dir "ok.el" "(defun a ()\n  1)\n")))
        (should (string-prefix-p "Replaced 1 occurrence"
                                 (superchat-tool-edit-buffer "ok.el" "  1)" "  ;; a stray ) in a comment\n  \"and ( in a string\"\n  1)")))
        (should (string-match-p "stray" (ec--text lisp)))
        (with-current-buffer lisp
          (goto-char (point-min))
          (search-forward "1)")
          (narrow-to-region (match-beginning 0) (match-end 0))
          (let ((before (ec--text lisp))
                (result (superchat-tool-edit-buffer "ok.el" "1)" "1))")))
            (should (string-prefix-p "Error:" result))
            (should (equal before (ec--text lisp)))
            (should (equal "1)" (buffer-string)))))))))

(ert-deftest edit-check/append-and-replace-are-sent-back-too ()
  (ec--with-dir dir
    (ec--fixture '((:text "unused"))
      (let ((buffer (ec--visit dir "more.el" "(defun a () 1)\n")))
        (let ((result (superchat-tool-append-to-buffer "more.el" "(defun b () 2\n")))
          (should (string-prefix-p "Error:" result))
          (should (string-match-p "line 2" result)))
        (let ((result (superchat-tool-replace-buffer "more.el" "(defun c () \"unterminated)\n")))
          (should (string-prefix-p "Error:" result)))
        (should (equal "(defun a () 1)\n" (ec--text buffer)))
        (should-not (buffer-modified-p buffer))
        (should (string-prefix-p "Appended text"
                                 (superchat-tool-append-to-buffer "more.el" "(defun b () 2)\n")))
        (should (string-prefix-p "Buffer replaced"
                                 (superchat-tool-replace-buffer "more.el" "(defun c () 3)\n")))
        (should (equal "(defun c () 3)\n" (ec--text buffer)))))))

(ert-deftest edit-check/through-the-pipeline-the-model-is-told-and-its-retry-lands ()
  "A scripted model submits the broken edit, reads the error in its next
request, submits the right one.  Only the second call is a change."
  (ec--with-dir dir
    (let ((file (expand-file-name "init.el" dir)))
      (let ((coding-system-for-write 'utf-8-unix)) (write-region ec--init nil file nil 'silent))
      (ec--fixture
          (list `(:tool-uses (("open_file" (path . ,file))))
                `(:tool-uses (("edit_buffer" (buffer_name . "init.el")
                               (old_string . ,ec--old) (new_string . ,ec--bad))))
                `(:tool-uses (("edit_buffer" (buffer_name . "init.el")
                               (old_string . ,ec--old) (new_string . ,ec--good))))
                '(:text "EC_DONE"))
        (let* ((asked 0)
               (superchat-approval-function (lambda (_request) (cl-incf asked) t))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (smoke--send view "bind the key")))
          (should (smoke--wait
                   (lambda () (superchat-runtime--terminal-p
                               (plist-get (superchat-runtime-status run) :status)))
                   20))
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (should (= 0 asked))
          ;; Requests are newest first: the third one carries the error.
          (let ((third (plist-get (nth 1 smoke--requests) :prompt-text)))
            (should (string-match-p "Error: .*not changed" third))
            (should (string-match-p "line 4" third)))
          (should (string-match-p (regexp-quote "#'describe-function))\n")
                                  (ec--text (get-file-buffer file))))
          (should-not (string-match-p (regexp-quote ")))") (ec--text (get-file-buffer file))))
          ;; The Tape attributes one change, the list shows one buffer.
          (let ((targets (cl-loop for event in (superchat-tape-replay-events session)
                                  when (and (equal "tool.completed" (plist-get event :type))
                                            (plist-get (plist-get event :payload) :buffer))
                                  collect (plist-get (plist-get event :payload) :buffer))))
            (should (equal '("init.el") targets)))
          (should (= 1 (length (superchat-changes-candidates session run))))
          (should (equal ec--init (with-temp-buffer (insert-file-contents file) (buffer-string)))))))))

(provide 'edit-check-regression)

;;; edit-check-regression.el ends here
