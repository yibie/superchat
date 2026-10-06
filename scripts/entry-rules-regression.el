;;; entry-rules-regression.el --- every entry carries the rules it should -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-06-rules-on-rewrite-and-workflow.md.
;; A Workspace's rules (AGENTS.md, else CLAUDE.md) used to reach the model
;; only from the chat window, in-place ask/act and a delegated sub-agent.
;; A region rewrite and a workflow step went without them.  These tests read
;; the request as it is sent to the provider: the rules are there, once,
;; from the right Workspace, without the note about remembering; and a rules
;; file that cannot be read is said, not hidden.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/entry-rules-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^entry-rules/")'

;;; Code:

(load (expand-file-name "workspace-rules-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-rewrite)
(require 'superchat-workflow)
(defvar superchat-rewrite-confirmation)

(defun er--system-or-empty (n)
  "The system text of the Nth provider request, \"\" when it has none."
  (let ((first (car (hm--wire n))))
    (if (equal "system" (alist-get 'role first)) (hm--text first) "")))

(defun er--count (needle text)
  "How many times NEEDLE occurs in TEXT."
  (let ((start 0) (n 0))
    (while (string-match (regexp-quote needle) text start)
      (setq n (1+ n) start (match-end 0)))
    n))

(defun er--rewrite (buffer)
  "Start a rewrite of the first five characters of BUFFER and wait for it."
  (let ((superchat-rewrite-confirmation 'overlay))
    (with-current-buffer buffer
      (superchat-rewrite--start-overlay (point-min) (+ (point-min) 5)
                                        "upcase it" nil)))
  (wrules--settled))

(defun er--workflow (view name body argument)
  "Write the workflow NAME with BODY and run it from VIEW with ARGUMENT."
  (make-directory superchat-workflow-directory t)
  (with-temp-file (expand-file-name (concat name ".workflow")
                                    superchat-workflow-directory)
    (insert body))
  (with-current-buffer view
    (superchat-workflow-execute-async name argument)))

(defun er--wait-requests (n)
  (should (smoke--wait (lambda () (and (>= (length hm--wires) n)
                                       (null (superchat-runtime-active-runs))))
                       30)))

(defconst er--remember "explicitly asks you to remember"
  "Words of the note about remembering that chat requests carry.")

;;;; Region rewrite ---------------------------------------------------------------

(ert-deftest entry-rules/a-rewrite-carries-the-rules-once-before-its-own-text ()
  (wrules--with '((:text "HELLO")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P: titles are in English.\n")
    (er--rewrite (wrules--file p))
    (let ((system (wrules--system 1)))
      (should (= 1 (er--count "RULE-MARKER-P" system)))
      (should (= 1 (er--count "Workspace rules, from " system)))
      ;; The rewrite's own instruction is still there and comes last, so
      ;; "reply with only the replacement" is the final word.
      (should (string-match-p "Rewrite only the target" system))
      (should (< (string-match "RULE-MARKER-P" system)
                 (string-match "Rewrite only the target" system)))
      ;; A rewrite is not where the user says "remember this".
      (should-not (string-match-p er--remember system)))
    ;; The user message is the rewrite's own: no history, no rules.
    (let ((body (hm--body (hm--wire 1))))
      (should (= 1 (length body)))
      (should-not (string-match-p "RULE-MARKER-P" (hm--text (car body)))))))

(ert-deftest entry-rules/a-rewrite-without-rules-is-as-it-was ()
  (wrules--with '((:text "HELLO")) (p)
    (er--rewrite (wrules--file p))
    (let ((system (wrules--system 1)))
      (should (string-prefix-p "You are an editor of" system))
      (should-not (string-match-p "Workspace rules" system))
      (should-not (string-match-p er--remember system))
      (should-not (string-match-p "AGENTS.md" system)))))

(ert-deftest entry-rules/a-rewrite-outside-any-workspace-is-as-it-was ()
  (wrules--with '((:text "HELLO")) ()
    (let* ((dir (wrules--temp-dir)))
      (unwind-protect
          (progn
            (wrules--write dir "AGENTS.md" "RULE-MARKER-LOOSE\n")
            (er--rewrite (wrules--file dir))
            (let ((system (wrules--system 1)))
              (should (string-prefix-p "You are an editor of" system))
              (should-not (string-match-p "RULE-MARKER-LOOSE" system))
              (should-not (string-match-p "temporary conversation" system))))
        (dolist (b (buffer-list))
          (when-let* ((f (buffer-file-name b)))
            (when (string-prefix-p dir f)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
        (delete-directory dir t)))))

(ert-deftest entry-rules/a-rewrite-gets-the-rules-of-its-own-workspace ()
  (wrules--with '((:text "HELLO") (:text "HELLO")) (p q)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (wrules--write q "AGENTS.md" "RULE-MARKER-Q\n")
    (let ((fp (wrules--file p))
          (fq (wrules--file q)))
      ;; The rewrite of P's file is started while Q's file is what is shown.
      (switch-to-buffer fq)
      (er--rewrite fp)
      (er--rewrite fq)
      (should (string-match-p "RULE-MARKER-P" (wrules--system 1)))
      (should-not (string-match-p "RULE-MARKER-Q" (wrules--system 1)))
      (should (string-match-p "RULE-MARKER-Q" (wrules--system 2)))
      (should-not (string-match-p "RULE-MARKER-P" (wrules--system 2))))))

;;;; Workflow ---------------------------------------------------------------------

(ert-deftest entry-rules/every-workflow-step-carries-the-rules-once ()
  (wrules--with '((:text "ONE") (:text "TWO")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let ((view (with-current-buffer (wrules--file p) (superchat))))
      (er--workflow view "erwf" "step one about $input\nstep two follows\n" "x")
      (er--wait-requests 2)
      (dolist (n '(1 2))
        (let ((system (wrules--system n)))
          (should (= 1 (er--count "RULE-MARKER-P" system)))
          (should (= 1 (er--count "Workspace rules, from " system)))
          (should-not (string-match-p er--remember system))))
      ;; The step prompt itself is unchanged.
      (should (string-match-p "step one about x"
                              (mapconcat #'hm--text (hm--body (hm--wire 1)) "\n"))))))

(ert-deftest entry-rules/a-workflow-keeps-its-rules-when-another-window-is-shown ()
  (wrules--with '((:text "ONE") (:text "TWO")) (p q)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (wrules--write q "AGENTS.md" "RULE-MARKER-Q\n")
    (let ((vp (with-current-buffer (wrules--file p) (superchat)))
          (vq (with-current-buffer (wrules--file q) (superchat))))
      (er--workflow vp "erwf2" "step one\nstep two\n" "")
      ;; The user turns to Q's conversation while the steps run.
      (switch-to-buffer vq)
      (set-buffer vq)
      (er--wait-requests 2)
      (dolist (n '(1 2))
        (should (string-match-p "RULE-MARKER-P" (wrules--system n)))
        (should-not (string-match-p "RULE-MARKER-Q" (wrules--system n)))))))

(ert-deftest entry-rules/a-workflow-in-a-temporary-conversation-has-no-rules ()
  (wrules--with '((:text "ONE")) ()
    (let ((view (superchat)))
      (er--workflow view "erwf3" "just one step\n" "")
      (er--wait-requests 1)
      (let ((system (er--system-or-empty 1)))
        (should-not (string-match-p "Workspace rules" system))
        (should-not (string-match-p "temporary conversation" system))))))

;;;; Entries that carry no rules, on purpose ---------------------------------

(ert-deftest entry-rules/compaction-and-skill-matching-carry-none ()
  (wrules--with '((:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let* ((view (with-current-buffer (wrules--file p) (superchat)))
           (session (hm--session view)))
      ;; The same Session on a surface that takes rules, to show the
      ;; collector does see them ...
      (should (superchat-prompt-collector--workspace-rules
               (list :session-id session :surface 'workflow) nil nil))
      ;; ... and gives nothing to the two that work on given material only.
      (dolist (surface '(compaction skills))
        (should-not (superchat-prompt-collector--workspace-rules
                     (list :session-id session :surface surface) nil nil))))))

;;;; A rules file that cannot be read --------------------------------------------

(defmacro er--unreadable (file &rest body)
  "Run BODY while FILE cannot be read."
  (declare (indent 1) (debug t))
  `(unwind-protect
       (progn (set-file-modes ,file #o000)
              (skip-unless (not (file-readable-p ,file)))
              ,@body)
     (set-file-modes ,file #o644)))

(defun er--messages (thunk)
  "Call THUNK; return the text it sent through `message'."
  (let (said)
    (cl-letf* ((real (symbol-function 'message))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (push (apply #'format format-string args) said))
                  (apply real format-string args))))
      (funcall thunk))
    (mapconcat #'identity (nreverse said) "\n")))

(ert-deftest entry-rules/the-problem-of-a-rules-file-is-told-apart ()
  (let ((root (wrules--temp-dir)))
    (unwind-protect
        (progn
          (should-not (superchat-workspace-rules-problem root))
          (should-not (superchat-workspace-rules-problem nil))
          (wrules--write root "AGENTS.md" "RULE\n")
          (should-not (superchat-workspace-rules-problem root))
          (er--unreadable (expand-file-name "AGENTS.md" root)
            (should (string-match-p "AGENTS.md is not readable"
                                    (superchat-workspace-rules-problem root)))
            ;; A second rules file that can be read is used; no problem.
            (wrules--write root "CLAUDE.md" "RULE-2\n")
            (should-not (superchat-workspace-rules-problem root))))
      (delete-directory root t))))

(ert-deftest entry-rules/a-chat-request-says-the-rules-could-not-be-read ()
  (wrules--with '((:text "ok")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let ((view (with-current-buffer (wrules--file p) (superchat))))
      (er--unreadable (expand-file-name "AGENTS.md" p)
        (let ((said (er--messages (lambda () (hm--turn view "hello")))))
          (should (string-match-p "workspace rules are not in this request" said)))
        (let ((system (wrules--system 1)))
          (should-not (string-match-p "RULE-MARKER-P" system))
          (should (string-match-p "could not read" system))
          (should (string-match-p "NOT in this request" system)))))))

(ert-deftest entry-rules/a-rewrite-tells-the-user-not-the-model ()
  (wrules--with '((:text "HELLO")) (p)
    (wrules--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let ((file (wrules--file p)))
      (er--unreadable (expand-file-name "AGENTS.md" p)
        (let ((said (er--messages (lambda () (er--rewrite file)))))
          (should (string-match-p "workspace rules are not in this request" said)))
        ;; The model must answer with the replacement only, so it is not
        ;; asked to talk about the missing rules.
        (let ((system (wrules--system 1)))
          (should (string-prefix-p "You are an editor of" system))
          (should-not (string-match-p "could not read" system)))))))

(ert-deftest entry-rules/a-sub-agent-without-readable-rules-is-said-to-the-user ()
  (require 'superchat-subagent)
  (let ((root (wrules--temp-dir)))
    (unwind-protect
        (progn
          (wrules--write root "AGENTS.md" "RULE\n")
          (er--unreadable (expand-file-name "AGENTS.md" root)
            (let (system)
              (let ((said (er--messages
                           (lambda ()
                             (setq system (superchat--subagent-system-with-rules
                                           "PRESET" root))))))
                (should (equal "PRESET" system))
                (should (string-match-p
                         "workspace rules are not in this sub-agent's request"
                         said))))))
      (delete-directory root t))))

(provide 'entry-rules-regression)
;;; entry-rules-regression.el ends here
