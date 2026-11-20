;;; test-limit.el --- tests for superchat-limit -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-limit)

(ert-deftest superchat-limit-a-verdict-is-read-from-the-first-word ()
  (should (eq (superchat-limit-verdict "DONE\nall read") 'done))
  (should (eq (superchat-limit-verdict "**Blocked**: a key is missing") 'blocked))
  (should (eq (superchat-limit-verdict "# CONTINUE") 'continue))
  ;; What names none goes on; the count of going on still bounds it.
  (should (eq (superchat-limit-verdict "I think it is done.") 'continue))
  (should (eq (superchat-limit-verdict "DONEish") 'continue))
  (should (eq (superchat-limit-verdict nil) 'continue)))

(ert-deftest superchat-limit-a-failed-judge-says-continue ()
  (let (got)
    (superchat-test-script (list '(:error "boom"))
      (superchat-limit-judge '((:kind user :text "go")) "lb" nil
                             (lambda (verdict why) (setq got (cons verdict why)))))
    (should (eq (car got) 'continue))
    (should (string-search "failed" (cdr got)))))

(ert-deftest superchat-limit-input-waiting-is-steered-text-or-what-the-hook-says ()
  (should (superchat-limit-pending-p "t" '("x")))
  (should-not (superchat-limit-pending-p "t" nil))
  (let ((superchat-limit-pending-functions (list (lambda (tape) (equal tape "t")))))
    (should (superchat-limit-pending-p "t" nil))
    (should-not (superchat-limit-pending-p "u" nil))))
