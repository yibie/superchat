;;; eval-write-regression.el --- eval_elisp does not save files -*- lexical-binding: t; -*-

;;; Commentary:
;; G28 of the llm-pi-ai field test (2026-10-23 01:07): the model wrote a
;; project file with `with-temp-file' from `eval_elisp', and the file on
;; disk changed without the review screen and without the user saving it.
;; The tool description already said "saving is the user's decision"; now
;; the call keeps it.  A write outside `superchat-tool-eval-writable-directories'
;; ends the whole expression as a failed call that names the way to go:
;; change the buffer and tell the user which buffer to save.  Writes to a
;; temporary directory, where the model copies buffers to run tests on
;; them, still go through.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/eval-write-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^eval-write/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar ew--project nil "A directory standing for the user's project.")
(defvar ew--scratch nil "A directory standing for the writable temporary one.")

(defmacro ew--dirs (&rest body)
  "Run BODY with a fresh project directory and a fresh writable one."
  (declare (indent 0) (debug t))
  `(let* ((ew--project (file-name-as-directory (make-temp-file "ew-project" t)))
          (ew--scratch (file-name-as-directory (make-temp-file "ew-scratch" t)))
          (superchat-tool-eval-writable-directories (list ew--scratch)))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (string-prefix-p (file-truename ew--project)
                                     (file-truename (buffer-file-name buffer))))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory ew--project t)
       (delete-directory ew--scratch t))))

(defmacro ew--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (superchat-sandbox-default-mode 'danger-full-access)
           (superchat-changes-review-after-run nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun ew--file (name &optional content)
  "NAME in the project directory, holding CONTENT when it is given."
  (let ((file (expand-file-name name ew--project)))
    (when content
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region content nil file nil 'silent)))
    file))

(defun ew--disk (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun ew--eval-script (expression)
  (list `(:tool-uses (("eval_elisp" (expression . ,expression))))
        '(:text "EW-DONE")))

(defun ew--run (expression)
  "Have the scripted model evaluate EXPRESSION; return the eval outcomes."
  (let* ((view (smoke--open-chat))
         (run (smoke--send view "write")))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    (cl-remove-if-not
     (lambda (event)
       (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
            (equal "eval_elisp"
                   (plist-get (plist-get event :payload) :tool-name))))
     (superchat-tape-replay-events
      (buffer-local-value 'superchat--session-id view)))))

(defun ew--types (events)
  (mapcar (lambda (event) (plist-get event :type)) events))

(defun ew--second-prompt ()
  (plist-get (car smoke--requests) :prompt-text))

(ert-deftest eval-write/with-temp-file-in-the-project-is-refused ()
  "The 01:07 case: the file on disk keeps what the user last saved."
  (ew--dirs
    (let* ((file (ew--file "lib.el" "old\n"))
           (expression (format "(with-temp-file %S (insert \"new\\n\"))" file)))
      (ew--fixture (ew--eval-script expression)
        (should (equal '("tool.failed") (ew--types (ew--run expression))))
        (should (equal "old\n" (ew--disk file)))
        (should (string-match-p (regexp-quote "does not write files")
                                (ew--second-prompt)))
        (should (string-match-p (regexp-quote (file-name-nondirectory file))
                                (ew--second-prompt)))))))

(ert-deftest eval-write/saving-a-buffer-is-refused ()
  "The buffer keeps the change, still unsaved, for the user to review."
  (ew--dirs
    (let* ((file (ew--file "lib.el" "old\n"))
           (buffer (find-file-noselect file))
           (expression (format "(with-current-buffer %S (save-buffer))"
                               (buffer-name buffer))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "edit\n"))
      (ew--fixture (ew--eval-script expression)
        (should (equal '("tool.failed") (ew--types (ew--run expression))))
        (should (equal "old\n" (ew--disk file)))
        (should (buffer-modified-p buffer))))))

(ert-deftest eval-write/the-models-condition-case-does-not-hide-it ()
  "The refusal leaves the whole expression, as the time limit does."
  (ew--dirs
    (let* ((file (ew--file "lib.el" "old\n"))
           (expression
            (format "(condition-case nil (with-temp-file %S (insert \"new\")) (error 'caught))"
                    file)))
      (ew--fixture (ew--eval-script expression)
        (should (equal '("tool.failed") (ew--types (ew--run expression))))
        (should (equal "old\n" (ew--disk file)))))))

(ert-deftest eval-write/a-new-file-in-the-project-is-refused ()
  (ew--dirs
    (let* ((file (ew--file "new-test.el"))
           (expression (format "(with-temp-file %S (insert \"x\"))" file)))
      (ew--fixture (ew--eval-script expression)
        (should (equal '("tool.failed") (ew--types (ew--run expression))))
        (should-not (file-exists-p file))))))

(ert-deftest eval-write/a-copy-in-the-temporary-directory-is-written ()
  "Guard: copying a buffer to run tests on the copy still works."
  (ew--dirs
    (let* ((copy (expand-file-name "sub/copy.el" ew--scratch))
           (expression (format "(progn (make-directory %S t) (with-temp-file %S (insert \"copy\")) 'written)"
                               (file-name-directory copy) copy)))
      (ew--fixture (ew--eval-script expression)
        (should (equal '("tool.completed") (ew--types (ew--run expression))))
        (should (equal "copy" (ew--disk copy)))))))

(ert-deftest eval-write/the-default-writable-directories-are-temporary ()
  "Guard: by default the temporary directory and /tmp take writes."
  (let ((dirs (default-value 'superchat-tool-eval-writable-directories)))
    (should (member temporary-file-directory dirs))
    (should (member "/tmp/" dirs))))

(ert-deftest eval-write/called-outside-a-tool-call-it-returns-the-text ()
  (ew--dirs
    (let ((file (ew--file "lib.el" "old\n")))
      (should (string-prefix-p
               "Error: "
               (superchat-tool-eval-elisp
                (format "(with-temp-file %S (insert \"new\"))" file))))
      (should (equal "old\n" (ew--disk file))))))

(provide 'eval-write-regression)

;;; eval-write-regression.el ends here
