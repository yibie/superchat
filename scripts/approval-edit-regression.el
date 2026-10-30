;;; approval-edit-regression.el --- A published question waits in an edit Emacs can hold -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract: docs/decisions/accepted/2026-10-29-a-question-emacs-cannot-hold-is-given-up.md
;;
;; `superchat-approval-publish' waits for its answer in a recursive edit, so
;; the user goes on working.  That branch runs only in an interactive Emacs:
;; batch polls.  Each test here starts an INTERACTIVE child Emacs under a
;; pseudo-terminal (see approval-edit-child.el), publishes the question from
;; a timer as the product does, and either types real keys into it or stands
;; `recursive-edit' in to come back at once, as it does while input is
;; blocked.  Before the guard, that case spun forever: Emacs froze with the
;; question in the Tape and nothing after it (2026-10-29).
;;
;;   emacs -Q --batch -L . -l scripts/approval-edit-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^approval-edit/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst ae--scripts (file-name-directory (or load-file-name buffer-file-name)))
(defconst ae--repo (file-name-directory (directory-file-name ae--scripts)))

(defun ae--run (scenario plan)
  "Run SCENARIO in an interactive child; return its result plist.
PLAN is a list of (MARKER . KEYS): once the child has opened the question
MARKER, wait long enough for a person to have seen it, then type KEYS.  A
child that does not finish within the deadline is killed, and the test
fails: that is the spin this suite is about."
  (when (eq system-type 'windows-nt) (ert-skip "needs a pseudo-terminal"))
  (let* ((dir (file-name-as-directory (make-temp-file "approval-edit-" t)))
         (process-environment (append (list (concat "AEC_DIR=" dir)
                                            (concat "AEC_SCENARIO=" scenario)
                                            "TERM=xterm-256color")
                                      process-environment))
         (process (make-process
                   :name "approval-edit-child" :buffer nil :filter #'ignore
                   :connection-type 'pty :noquery t
                   :command (list (expand-file-name invocation-name invocation-directory)
                                  "-Q" "-nw" "-L" ae--repo
                                  "-l" (expand-file-name "approval-edit-child.el" ae--scripts))))
         (deadline (+ (float-time) 30))
         (result (expand-file-name "result.el" dir)))
    (set-process-window-size process 30 110)
    (unwind-protect
        (progn
          (while (and (not (file-exists-p result)) (< (float-time) deadline))
            (when-let* ((step (car plan))
                        ((file-exists-p (expand-file-name (concat (car step) ".open") dir))))
              (pop plan)
              ;; The question is open now; a person answers after seeing it.
              (sleep-for 0.6)
              (when (process-live-p process)
                (ignore-errors (process-send-string process (cdr step)))))
            (accept-process-output process 0.05))
          (should (file-exists-p result))
          (with-temp-buffer
            (insert-file-contents result)
            (read (current-buffer))))
      (when (process-live-p process) (delete-process process))
      (delete-directory dir t))))

(defconst ae--allow "\exsuperchat-approval-allow\r"
  "M-x superchat-approval-allow RET, as typed at a terminal.")
(defconst ae--deny "\exsuperchat-approval-deny\r")
(defconst ae--leave "\C-\M-c" "C-M-c: `exit-recursive-edit'.")

(defun ae--said-cannot-take-keys-p (result)
  (cl-some (lambda (text) (string-match-p "cannot take keys" text))
           (plist-get result :announced)))

(ert-deftest approval-edit/the-question-waits-in-a-recursive-edit-and-a-command-allows-it ()
  "Published from a timer, the question holds a recursive edit (depth 1)
while open; `superchat-approval-allow' typed at the keyboard decides it and
the edit is left: depth 0 again, the wait returns allowed."
  (let ((result (ae--run "typed" `(("q1" . ,ae--allow)))))
    (should (eq 'allowed (plist-get result :answer)))
    (should (= 1 (plist-get result :max-depth)))
    (should (= 0 (plist-get result :depth-after)))
    ;; A person took their time; timers ran meanwhile.
    (should (> (plist-get result :seconds) 0.4))
    (should (> (plist-get result :ticks) 3))
    (should-not (ae--said-cannot-take-keys-p result))))

(ert-deftest approval-edit/a-typed-deny-refuses ()
  (let ((result (ae--run "typed" `(("q1" . ,ae--deny)))))
    (should (eq 'rejected (plist-get result :answer)))
    (should (= 0 (plist-get result :depth-after)))))

(ert-deftest approval-edit/leaving-the-edit-by-hand-does-not-end-the-question ()
  "C-M-c at the question's own level: the edit was held, so it is entered
again and the question stays open until it is answered.  The guard does not
mistake this for an edit Emacs cannot hold."
  (let ((result (ae--run "typed" `(("q1" . ,(concat ae--leave ae--allow))))))
    (should (eq 'allowed (plist-get result :answer)))
    (should (= 1 (plist-get result :max-depth)))
    (should (= 0 (plist-get result :depth-after)))
    (should-not (ae--said-cannot-take-keys-p result))))

(ert-deftest approval-edit/an-edit-emacs-cannot-hold-is-not-spun-on ()
  "`recursive-edit' comes back at once, as it does while input is blocked.
The wait does not go round again: it polls, timers run, and an answer by
handle (emacsclient's way) decides it.  Without the guard the child spins
and never writes its result."
  (let ((result (ae--run "no-edit" nil)))
    (should (eq 'allowed (plist-get result :answer)))
    (should (< 0.8 (plist-get result :seconds) 5))
    (should (> (plist-get result :ticks) 5))
    (should (ae--said-cannot-take-keys-p result))
    (should-not (plist-get result :interruption))))

(ert-deftest approval-edit/an-unanswered-question-without-an-edit-is-given-up ()
  "Nobody answers and Emacs cannot hold the edit: after
`superchat-approval-unheld-wait' the question is refused, the user is told,
and the decision carries why."
  (let ((result (ae--run "no-edit-unanswered" nil)))
    (should (eq 'rejected (plist-get result :answer)))
    (should (< 1.4 (plist-get result :seconds) 6))
    (should (= 0 (plist-get result :depth-after)))
    (should (eq 'no-edit (plist-get result :interruption)))
    (should (cl-some (lambda (text) (string-match-p "no answer in 1.5 s" text))
                     (plist-get result :announced)))))

(ert-deftest approval-edit/the-run-ending-still-ends-the-question-without-an-edit ()
  "While the wait polls, the Run's watch still runs: a Run that dies one
second in abandons the question with `:run-ended', long before the bound."
  (let ((result (ae--run "no-edit-run-ends" nil)))
    (should (eq :run-ended (plist-get result :answer)))
    (should (< 0.8 (plist-get result :seconds) 5))
    (should (= 0 (plist-get result :depth-after)))))

(provide 'approval-edit-regression)

;;; approval-edit-regression.el ends here
