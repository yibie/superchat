;;; workspace-rules-regression.el --- a workspace's AGENTS.md reaches its requests -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 2.2 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-2-rules-and-recall.md,
;; contract R2 and R3): a conversation bound to a Workspace carries that
;; Workspace's rules file -- `AGENTS.md', else `CLAUDE.md', read from disk,
;; at most 6000 characters -- in the system text of every request, and no
;; other Workspace's.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-rules-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-rules/")'

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

(defvar wrules--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wrules--finder (dir)
  (cl-loop for root in wrules--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wrules--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-rules-" t))))

(defmacro wrules--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wrules--roots nil)
          ,@(mapcar (lambda (v) `(,v (wrules--temp-dir))) vars)
          (project-find-functions (list #'wrules--finder))
          (superchat-display-style 'editor))
     (setq wrules--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wrules--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wrules--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wrules--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wrules--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

;;;; The text of the rules ------------------------------------------------------

(ert-deftest workspace-rules/agents-md-is-the-rules-file ()
  (let ((root (wrules--temp-dir)))
    (unwind-protect
        (progn
          (should-not (superchat-workspace-rules-file root))
          (should-not (superchat-workspace-rules-text root))
          (wrules--write root "CLAUDE.md" "CLAUDE-RULE\n")
          (should (equal (expand-file-name "CLAUDE.md" root)
                         (superchat-workspace-rules-file root)))
          (should (string-match-p "CLAUDE-RULE"
                                  (superchat-workspace-rules-text root)))
          (wrules--write root "AGENTS.md" "\n  AGENTS-RULE  \n\n")
          (should (equal (expand-file-name "AGENTS.md" root)
                         (superchat-workspace-rules-file root)))
          (let ((text (superchat-workspace-rules-text root)))
            (should (string-match-p "AGENTS-RULE\\'" text))
            (should-not (string-match-p "CLAUDE-RULE" text))
            (should (string-match-p (regexp-quote (expand-file-name "AGENTS.md" root))
                                    text))
            (should (string-prefix-p "Workspace rules, from " text))))
      (delete-directory root t))))

(ert-deftest workspace-rules/a-blank-or-missing-file-gives-nothing ()
  (let ((root (wrules--temp-dir)))
    (unwind-protect
        (progn
          (wrules--write root "AGENTS.md" " \n\n\t\n")
          (should-not (superchat-workspace-rules-text root))
          (delete-file (expand-file-name "AGENTS.md" root))
          ;; A directory called AGENTS.md is not a rules file and must not raise.
          (make-directory (expand-file-name "AGENTS.md" root))
          (should-not (superchat-workspace-rules-file root))
          (should-not (superchat-workspace-rules-text root))
          (should-not (superchat-workspace-rules-text nil))
          (should-not (superchat-workspace-rules-text
                       (expand-file-name "missing/" root))))
      (delete-directory root t))))

(ert-deftest workspace-rules/a-long-file-is-cut-and-says-so ()
  (let ((root (wrules--temp-dir)))
    (unwind-protect
        (progn
          (wrules--write root "AGENTS.md" (make-string 6000 ?a))
          (should-not (string-match-p "Cut at"
                                      (superchat-workspace-rules-text root)))
          (wrules--write root "AGENTS.md"
                         (concat (make-string 6000 ?a) "TAIL-MARKER"))
          (let ((text (superchat-workspace-rules-text root)))
            (should-not (string-match-p "TAIL-MARKER" text))
            (should (string-match-p (make-string 6000 ?a) text))
            (should-not (string-match-p (make-string 6001 ?a) text))
            (should (string-match-p "\\[Cut at 6000 characters; read .*AGENTS\\.md for the rest\\.\\]\\'"
                                    text))))
      (delete-directory root t))))

;;;; The rules in requests ------------------------------------------------------

(ert-deftest workspace-rules/a-workspace-conversation-carries-its-rules ()
  (wrules--with '((:text "ok") (:text "ok")) (p q)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P: diary titles are in English.\n")
    (let ((vp (with-current-buffer (wrules--file p) (superchat)))
          (vq (with-current-buffer (wrules--file q) (superchat))))
      (hm--turn vp "hello from p")
      (hm--turn vq "hello from q")
      (should (string-match-p "RULE-MARKER-P" (wrules--system 1)))
      (should (string-match-p "Workspace rules, from " (wrules--system 1)))
      ;; Q has no rules file, and P's rules never reach it.
      (should-not (string-match-p "RULE-MARKER-P" (wrules--system 2)))
      (should-not (string-match-p "Workspace rules, from " (wrules--system 2))))))

(ert-deftest workspace-rules/each-workspace-gets-only-its-own-rules ()
  (wrules--with '((:text "ok") (:text "ok")) (p q)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (wrules--write q "CLAUDE.md" "RULE-MARKER-Q\n")
    (let ((vp (with-current-buffer (wrules--file p) (superchat)))
          (vq (with-current-buffer (wrules--file q) (superchat))))
      (hm--turn vp "hello from p")
      (hm--turn vq "hello from q")
      (should (string-match-p "RULE-MARKER-P" (wrules--system 1)))
      (should-not (string-match-p "RULE-MARKER-Q" (wrules--system 1)))
      (should (string-match-p "RULE-MARKER-Q" (wrules--system 2)))
      (should-not (string-match-p "RULE-MARKER-P" (wrules--system 2))))))

(ert-deftest workspace-rules/the-file-on-disk-is-read-at-every-request ()
  "A saved change is in the next request; an unsaved buffer edit is not."
  (wrules--with '((:text "ok") (:text "ok") (:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-ONE\n")
    (let ((vp (with-current-buffer (wrules--file p) (superchat))))
      (hm--turn vp "first")
      (wrules--write p "AGENTS.md" "RULE-TWO\n")
      (hm--turn vp "second")
      (let ((buffer (find-file-noselect (expand-file-name "AGENTS.md" p))))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "RULE-UNSAVED\n"))
        (hm--turn vp "third"))
      (should (string-match-p "RULE-ONE" (wrules--system 1)))
      (should (string-match-p "RULE-TWO" (wrules--system 2)))
      (should-not (string-match-p "RULE-ONE" (wrules--system 2)))
      (should (string-match-p "RULE-TWO" (wrules--system 3)))
      (should-not (string-match-p "RULE-UNSAVED" (wrules--system 3))))))

(ert-deftest workspace-rules/a-request-made-from-a-file-carries-the-rules ()
  (wrules--with '((:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (with-current-buffer (wrules--file p) (superchat-act "do it"))
    (wrules--settled)
    (should (string-match-p "RULE-MARKER-P" (wrules--system 1)))))

(ert-deftest workspace-rules/a-conversation-outside-any-project-has-no-rules ()
  (wrules--with '((:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (hm--turn (smoke--open-chat) "hello")
    (should-not (string-match-p "Workspace rules, from " (wrules--system 1)))
    (should-not (string-match-p "RULE-MARKER-P" (wrules--system 1)))))

;; Leader additions after the mutant run: the two tests below each catch a
;; change the tests above let through.

(ert-deftest workspace-rules/a-rules-file-beside-a-temporary-conversation-is-not-read ()
  "The root comes from the conversation's binding, not from the directory
the View happens to sit in."
  (wrules--with '((:text "ok")) ()
    (let ((dir (wrules--temp-dir))
          (origin (generate-new-buffer "wrules-origin")))
      (unwind-protect
          (progn
            (wrules--write dir "AGENTS.md" "RULE-MARKER-LOOSE\n")
            (let ((view (with-current-buffer origin
                          (setq default-directory dir)
                          (superchat))))
              (with-current-buffer view (setq default-directory dir))
              (should-not (buffer-local-value 'superchat--workspace-binding view))
              (hm--turn view "hello"))
            (should-not (string-match-p "RULE-MARKER-LOOSE" (wrules--system 1))))
        (kill-buffer origin)
        (delete-directory dir t)))))

(ert-deftest workspace-rules/in-place-ask-and-act-carry-the-rules ()
  (wrules--with '((:text "ok") (:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let ((src (wrules--file p)))
      (with-current-buffer src
        (goto-char (point-min))
        (superchat-inline-ask "what is this"
                              (cons (line-beginning-position) (line-end-position))))
      (wrules--settled)
      (with-current-buffer src (superchat-inline-act "change it"))
      (wrules--settled)
      (should (string-match-p "RULE-MARKER-P" (wrules--system 1)))
      (should (string-match-p "RULE-MARKER-P" (wrules--system 2))))))

(provide 'workspace-rules-regression)
;;; workspace-rules-regression.el ends here
