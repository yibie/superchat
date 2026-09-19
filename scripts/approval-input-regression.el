;;; approval-input-regression.el --- An approval is answered by input that came after it -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract: docs/decisions/accepted/2026-09-19-an-approval-is-answered-by-input-that-came-after-it.md
;;
;; Each test starts an INTERACTIVE child Emacs under a pseudo-terminal (see
;; approval-input-child.el) and types real keys into it: batch Emacs has no
;; input queue -- its question reads stdin -- so the defect cannot exist here.
;;
;;   emacs -Q --batch -L . -l scripts/approval-input-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^approval-input/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst ai--scripts (file-name-directory (or load-file-name buffer-file-name)))
(defconst ai--repo (file-name-directory (directory-file-name ai--scripts)))

(defun ai--run (scenario plan)
  "Run SCENARIO in an interactive child; return its results.
PLAN is a list of (MARKER . KEYS): once the child has opened the question
MARKER, wait long enough for a person to have seen it, then type KEYS."
  (when (eq system-type 'windows-nt) (ert-skip "needs a pseudo-terminal"))
  (let* ((dir (file-name-as-directory (make-temp-file "approval-input-" t)))
         (process-environment (append (list (concat "AIC_DIR=" dir)
                                            (concat "AIC_SCENARIO=" scenario)
                                            "TERM=xterm-256color")
                                      process-environment))
         (process (make-process
                   :name "approval-input-child" :buffer nil :filter #'ignore
                   :connection-type 'pty :noquery t
                   :command (list (expand-file-name invocation-name invocation-directory)
                                  "-Q" "-nw" "-L" ai--repo
                                  "-l" (expand-file-name "approval-input-child.el" ai--scripts))))
         (deadline (+ (float-time) 30))
         (result (expand-file-name "result.el" dir)))
    (set-process-window-size process 30 110)
    (unwind-protect
        (progn
          (while (and (not (file-exists-p result)) (< (float-time) deadline))
            (when-let* ((step (car plan))
                        ((file-exists-p (expand-file-name (concat (car step) ".open") dir))))
              (pop plan)
              ;; The question is on screen now; a person answers after seeing it.
              (sleep-for 0.6)
              ;; A child whose question was already answered has exited; the
              ;; result file then says by what.
              (when (process-live-p process)
                (ignore-errors (process-send-string process (cdr step)))))
            (accept-process-output process 0.05))
          (should (file-exists-p result))
          (with-temp-buffer
            (insert-file-contents result)
            (read (current-buffer))))
      (when (process-live-p process) (delete-process process))
      (delete-directory dir t))))

(ert-deftest approval-input/a-keystroke-from-before-the-question-does-not-answer-it ()
  "A `y' already in the queue: the question stays open until a new key comes."
  (pcase-let ((`((,_ ,answer ,seconds)) (ai--run "stale-y" '(("q1" . "n")))))
    (should (eq 'rejected answer))
    ;; It waited for the person: the queued key was not what answered.
    (should (> seconds 0.4))))

(ert-deftest approval-input/a-second-y-does-not-answer-the-next-question ()
  "`yy' typed at the first question: one answers it, the other is not an
answer to the question that follows."
  (pcase-let ((`((,_ ,first ,_) (,_ ,second ,seconds))
               (ai--run "double-y" '(("q1" . "yy") ("q2" . "n")))))
    (should (eq 'allowed first))
    (should (eq 'rejected second))
    (should (> seconds 0.4))))

(ert-deftest approval-input/new-input-answers-as-before ()
  "Guard: a key typed after the question is shown decides it, either way."
  (pcase-let ((`((,_ ,answer ,_)) (ai--run "fresh" '(("q1" . "y")))))
    (should (eq 'allowed answer)))
  (pcase-let ((`((,_ ,answer ,_)) (ai--run "fresh" '(("q1" . "n")))))
    (should (eq 'rejected answer))))

(ert-deftest approval-input/a-quit-is-a-quit-with-or-without-old-input ()
  "C-g while the question is open is a quit, never an approval -- also when an
old `y' was waiting in the queue."
  (pcase-let ((`((,_ ,answer ,_)) (ai--run "fresh" '(("q1" . "\C-g")))))
    (should (eq 'quit answer)))
  (pcase-let ((`((,_ ,answer ,_)) (ai--run "stale-y" '(("q1" . "\C-g")))))
    (should (eq 'quit answer))))

(ert-deftest approval-input/a-keyboard-macro-is-not-a-person ()
  "Inside a running keyboard macro the chain has no human answerer, a direct
call answers no without reading, and the macro's `y' is not consumed by the
question: it lands in the buffer like any typed key."
  (pcase-let ((`(,seen) (ai--run "macro" nil)))
    (should-not (plist-get seen :chain-has-a-human))
    (should (eq 'rejected (plist-get seen :direct-answer)))
    (should (equal "y" (plist-get seen :typed-into-buffer)))))

(provide 'approval-input-regression)

;;; approval-input-regression.el ends here
