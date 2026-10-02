;;; workspace-subagent-rules-regression.el --- a sub-agent carries its workspace's rules -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 2.5 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-2-rules-and-recall.md,
;; contract R3): a sub-agent delegated from a conversation bound to a
;; Workspace has that Workspace's rules in its system text, and no note
;; about remembering.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-subagent-rules-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-subagent-rules/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-subagent)
(defvar superchat-display-style)

(defvar wsub--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wsub--finder (dir)
  (cl-loop for root in wsub--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wsub--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-sub-" t))))

(defmacro wsub--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wsub--roots nil)
          ,@(mapcar (lambda (v) `(,v (wsub--temp-dir))) vars)
          (project-find-functions (list #'wsub--finder))
          (superchat-display-style 'editor))
     (setq wsub--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wsub--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wsub--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wsub--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wsub--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

;;;; The helper -------------------------------------------------------------------

(ert-deftest workspace-subagent-rules/the-rules-follow-the-preset-text ()
  (let ((root (wsub--temp-dir)))
    (unwind-protect
        (progn
          (should (equal "PRESET" (superchat--subagent-system-with-rules "PRESET" root)))
          (should (equal "PRESET" (superchat--subagent-system-with-rules "PRESET" nil)))
          (should-not (superchat--subagent-system-with-rules nil nil))
          (wsub--write root "AGENTS.md" "RULE-MARKER-SUB\n")
          (let ((text (superchat--subagent-system-with-rules "PRESET" root)))
            (should (string-prefix-p "PRESET\n\nWorkspace rules, from " text))
            (should (string-suffix-p "RULE-MARKER-SUB" text)))
          (dolist (empty '(nil ""))
            (should (string-prefix-p
                     "Workspace rules, from "
                     (superchat--subagent-system-with-rules empty root)))))
      (delete-directory root t))))

;;;; A delegated sub-agent -------------------------------------------------------

(defun wsub--delegate-round ()
  "A model round that delegates a task to the researcher sub-agent."
  '(:tool-uses (("delegate_to_subagent"
                 (preset . "researcher")
                 (task . "SUB-TASK-MARKER look into it")))))

(defun wsub--subagent-wire ()
  "The provider request whose user text holds the delegated task."
  (let ((wire (cl-find-if
               (lambda (wire)
                 (cl-some (lambda (message)
                            (and (equal "user" (alist-get 'role message))
                                 (string-match-p "SUB-TASK-MARKER" (hm--text message))))
                          wire))
               (reverse hm--wires))))
    (should wire)
    wire))

(defun wsub--system-of (wire)
  (let ((first (car wire)))
    (if (equal "system" (alist-get 'role first)) (hm--text first) "")))

(ert-deftest workspace-subagent-rules/a-delegated-sub-agent-carries-the-rules ()
  (wsub--with (list (wsub--delegate-round) '(:text "SUB-REPORT") '(:text "done")) (p)
    (wsub--write p "AGENTS.md" "RULE-MARKER-P\n")
    ;; Delegation is not on the default tool surface.
    (setq superchat-llm-tool-names
          (append superchat-llm-tool-names '("delegate_to_subagent")))
    (hm--turn (with-current-buffer (wsub--file p) (superchat)) "please delegate")
    (wsub--settled)
    (let ((system (wsub--system-of (wsub--subagent-wire))))
      (should (string-match-p "Workspace rules, from " system))
      (should (string-match-p "RULE-MARKER-P" system))
      ;; Writing rules is the main agent's.
      (should-not (string-match-p "explicitly asks you to remember" system)))))

(ert-deftest workspace-subagent-rules/a-sub-agent-of-a-temporary-conversation-has-none ()
  (wsub--with (list (wsub--delegate-round) '(:text "SUB-REPORT") '(:text "done")) (p)
    (wsub--write p "AGENTS.md" "RULE-MARKER-P\n")
    ;; Delegation is not on the default tool surface.
    (setq superchat-llm-tool-names
          (append superchat-llm-tool-names '("delegate_to_subagent")))
    (hm--turn (smoke--open-chat) "please delegate")
    (wsub--settled)
    (let ((system (wsub--system-of (wsub--subagent-wire))))
      (should-not (string-match-p "Workspace rules, from " system))
      (should-not (string-match-p "RULE-MARKER-P" system))
      (should-not (string-match-p "temporary conversation" system)))))

;; Leader addition after the mutant run: the root is the parent conversation's
;; binding, not the directory its View sits in.
(ert-deftest workspace-subagent-rules/a-rules-file-beside-a-temporary-conversation-is-not-read ()
  (wsub--with (list (wsub--delegate-round) '(:text "SUB-REPORT") '(:text "done")) ()
    (let ((dir (wsub--temp-dir)))
      (unwind-protect
          (progn
            (wsub--write dir "AGENTS.md" "RULE-MARKER-LOOSE\n")
            (setq superchat-llm-tool-names
                  (append superchat-llm-tool-names '("delegate_to_subagent")))
            (let ((view (smoke--open-chat)))
              (with-current-buffer view (setq default-directory dir))
              (should-not (buffer-local-value 'superchat--workspace-binding view))
              (hm--turn view "please delegate"))
            (wsub--settled)
            (should-not (string-match-p "RULE-MARKER-LOOSE"
                                        (wsub--system-of (wsub--subagent-wire)))))
        (delete-directory dir t)))))

(provide 'workspace-subagent-rules-regression)
;;; workspace-subagent-rules-regression.el ends here
