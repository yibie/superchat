;;; work-defer-regression.el --- work the user put off waits for the user -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 12a (reports/guidance-2026-10-11/SLICE-12.md).  The `work_record'
;; description now ends: "When the user says to do the rest later, register
;; wait_for user with what is left in detail; Superchat will not go on by
;; itself."  The second half of that sentence is a claim about the product.
;; This file pins it: an execution the user began registers a wait for the
;; user; the user's next message is unrelated; no round is queued or run; the
;; record is continuable but its rounds are not going on; `/work resume' then
;; starts the next round.  It was expected green on the code before the
;; sentence and was.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/work-defer-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^work-defer/")'

;;; Code:

(load (expand-file-name "work-resume-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest work-defer/a-wait-for-the-user-is-not-taken-up-by-itself ()
  (pend--fresh
    (wrec--with (list (gr--start)
                      (wrec--call '(action . "wait") '(wait_for . "user")
                                  '(detail . "three books are left for next time"))
                      '(:text "The first three are done; the rest next time.")
                      '(:text "You're welcome.")
                      (wrec--call '(action . "done") '(conclusion . "WD-ALL-SIX"))
                      '(:text "All six are done."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (gr--first-turn view)
        (let ((id (gr--record)))
          (should (eq 'waiting-user (superchat-work-status (superchat-work-get id))))
          ;; An unrelated message from the user.
          (hm--turn view "thanks")
          (gr--quiet)
          (should (= 0 (gr--count session "work.round")))
          (should (= 0 (gr--count session "queue.queued")))
          (should (= 0 (gr--queued-anywhere)))
          (should (= 4 (length hm--wires)))
          (should-not (member "goal-rounds" (gr--actors session)))
          (should (eq 'continuable (superchat-work-status (superchat-work-get id))))
          (should-not (superchat-work-continuing-p id))
          ;; Only the user's word starts the rounds again.
          (should (string-match-p "\\`Superchat goes on with " (wres--resume view)))
          (gr--quiet)
          (should (= 1 (gr--count session "work.round")))
          (should (member "goal-rounds" (gr--actors session)))
          (should (eq 'completed (superchat-work-status (superchat-work-get id)))))))))

(provide 'work-defer-regression)
;;; work-defer-regression.el ends here
