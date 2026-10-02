;;; workspace-remember-regression.el --- what a request says about remembering -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 2.3 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-2-rules-and-recall.md,
;; contract R4, W5 and W10): after the Workspace's rules, the system text
;; says what to do when the user says to remember something -- edit the
;; rules file's buffer and say what was written -- and a temporary
;; conversation is told that nothing said in it is kept.  What is pinned is
;; the text that reaches the provider, not that a model follows it.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-remember-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-remember/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-inline)
(defvar superchat-display-style)

(defvar wremember--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wremember--finder (dir)
  (cl-loop for root in wremember--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wremember--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-remember-" t))))

(defmacro wremember--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wremember--roots nil)
          ,@(mapcar (lambda (v) `(,v (wremember--temp-dir))) vars)
          (project-find-functions (list #'wremember--finder))
          (superchat-display-style 'editor))
     (setq wremember--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wremember--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wremember--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wremember--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wremember--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

;;;; The note itself -----------------------------------------------------------

(ert-deftest workspace-remember/the-note-names-the-file-to-edit ()
  (let ((root (wremember--temp-dir)))
    (unwind-protect
        (let ((agents (expand-file-name "AGENTS.md" root)))
          ;; No rules file yet: the model is told where to start one.
          (let ((note (superchat-workspace-remember-note root)))
            (should (string-match-p "no rules file yet" note))
            (should (string-match-p (regexp-quote agents) note))
            (should (string-match-p "tell the user what you wrote" note))
            (should (string-match-p "once the user saves" note))
            (should (string-match-p "only inferred" note)))
          ;; CLAUDE.md is the rules file when there is no AGENTS.md.
          (wremember--write root "CLAUDE.md" "x\n")
          (let ((note (superchat-workspace-remember-note root)))
            (should-not (string-match-p "no rules file yet" note))
            (should (string-match-p (regexp-quote (expand-file-name "CLAUDE.md" root))
                                    note))
            (should-not (string-match-p (regexp-quote agents) note)))
          (wremember--write root "AGENTS.md" "x\n")
          (let ((note (superchat-workspace-remember-note root)))
            (should (string-match-p (regexp-quote agents) note))
            (should (string-match-p "explicitly asks you to remember" note))
            (should (string-match-p "tell the user what you wrote" note))
            (should (string-match-p "once the user saves" note))
            (should (string-match-p "only inferred" note))))
      (delete-directory root t))))

(ert-deftest workspace-remember/the-temporary-note-says-nothing-is-kept ()
  (dolist (note (list (superchat-workspace-remember-note nil)
                      (superchat-workspace-remember-note "/no/such/dir/")))
    (should (string-match-p "temporary conversation" note))
    (should (string-match-p "will not be kept" note))
    (should (string-match-p "/workspace DIRECTORY" note))
    (should-not (string-match-p "open that" note))))

(ert-deftest workspace-remember/no-note-tells-the-model-to-save ()
  (dolist (note (list superchat-workspace-remember-note-with-file
                      superchat-workspace-remember-note-without-file
                      superchat-workspace-temporary-note))
    (should-not (string-match-p "save-buffer\\|write-file" note))
    (should (<= (length note) 480))))

;;;; The note in requests --------------------------------------------------------

(ert-deftest workspace-remember/a-workspace-request-says-where-to-write ()
  (wremember--with '((:text "ok") (:text "ok")) (p q)
    (wremember--write p "AGENTS.md" "RULE-MARKER-P\n")
    (hm--turn (with-current-buffer (wremember--file p) (superchat)) "hello")
    (hm--turn (with-current-buffer (wremember--file q) (superchat)) "hello")
    (let ((with-file (wremember--system 1))
          (without (wremember--system 2)))
      ;; The note follows the rules it is about.
      (should (string-match-p
               (concat "RULE-MARKER-P\n\nWhen the user explicitly asks you to remember"
                       ".*" (regexp-quote (expand-file-name "AGENTS.md" p)))
               with-file))
      (should-not (string-match-p "temporary conversation" with-file))
      (should (string-match-p
               (concat "no rules file yet.*"
                       (regexp-quote (expand-file-name "AGENTS.md" q)))
               without))
      (should-not (string-match-p (regexp-quote p) without))
      (should-not (string-match-p "temporary conversation" without)))))

(ert-deftest workspace-remember/a-temporary-request-says-nothing-is-kept ()
  (wremember--with '((:text "ok")) ()
    (hm--turn (smoke--open-chat) "hello")
    (let ((system (wremember--system 1)))
      (should (string-match-p "This is a temporary conversation" system))
      (should (string-match-p "/workspace DIRECTORY" system))
      (should-not (string-match-p "explicitly asks you to remember" system)))))

(ert-deftest workspace-remember/in-place-requests-carry-the-note ()
  (wremember--with '((:text "ok") (:text "ok")) (p)
    (let ((src (wremember--file p)))
      (with-current-buffer src
        (goto-char (point-min))
        (superchat-inline-ask "what is this"
                              (cons (line-beginning-position) (line-end-position))))
      (wremember--settled)
      (with-current-buffer src (superchat-inline-act "change it"))
      (wremember--settled)
      (should (string-match-p "no rules file yet" (wremember--system 1)))
      (should (string-match-p "no rules file yet" (wremember--system 2))))))

(provide 'workspace-remember-regression)
;;; workspace-remember-regression.el ends here
