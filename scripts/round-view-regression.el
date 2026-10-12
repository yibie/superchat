;;; round-view-regression.el --- a round's status goes to its conversation -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 7b, item 3.  In the user's own Emacs on 2026-10-12 a buffer named
;; "*superchat*" appeared beside the conversation "*superchat:ID*" and held
;; lines "Prefill / waiting first token… 10s"; the model read it in round 2.
;;
;; The waiting line of an execution belongs in its conversation's buffer and
;; in no other, whoever started the execution.  A batch Emacs does not show
;; the line in the conversation reliably (the control did not see it either),
;; so what is asserted is the half that can be seen: no buffer of the default
;; name is made for it.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L scripts -L plugins/superchat-cowork-telega \
;;     -l scripts/round-view-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^round-view/")'

;;; Code:

(load (expand-file-name "goal-rounds-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-buffer-name)

(defun rv--stray ()
  "The text of the buffer the default name makes, or nil when there is none."
  (let ((buffer (get-buffer (default-value 'superchat-buffer-name))))
    (and buffer (with-current-buffer buffer (buffer-string)))))

(ert-deftest round-view/the-users-own-execution-writes-its-status-in-its-conversation ()
  "The control: an execution the user started, slow enough to be refreshed."
  (pend--fresh
    (wrec--with '((:text "RV-ANSWER" :delay 3)) (p)
      (let ((view (wrec--view p)))
        (should-not (equal (buffer-name view) (default-value 'superchat-buffer-name)))
        (hm--turn view "hello")
        (should-not (rv--stray))))))

(ert-deftest round-view/a-round-writes-its-status-in-its-conversation ()
  (pend--fresh
    (wrec--with (list (gr--start) '(:text "The record is started.")
                      '(:text "RV-ROUND-ONE: nothing was left." :delay 3)
                      (wrec--call '(action . "done") '(conclusion . "RV-DONE"))
                      '(:text "All done."))
        (p)
      (let* ((view (wrec--view p)) (session (hm--session view)))
        (should-not (equal (buffer-name view) (default-value 'superchat-buffer-name)))
        (hm--turn view "tick them all off")
        (gr--quiet)
        (should (<= 1 (gr--count session "work.round")))
        ;; No buffer is made for a status line.
        (should-not (rv--stray))))))

(provide 'round-view-regression)
;;; round-view-regression.el ends here
