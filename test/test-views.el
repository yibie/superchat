;;; test-views.el --- tests for superchat-views -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-recall)
(require 'superchat-views)

(ert-deftest superchat-views-entries-move-open-and-refresh ()
  (superchat-test-with-tape
    (superchat-tape-append "t1" 'user "hi" :scope "/p/")
    (superchat-recall-note "t1" "one" "first note")
    (superchat-recall-note "t2" "two" "second note" nil "/p/")
    (let* (opened (superchat-views-open-function (lambda (tape) (push tape opened))))
      (superchat-entries "notes" (lambda () (superchat-view 'notes :scope "/p/")))
      (with-current-buffer "*superchat-view: notes*"
        (should (eq major-mode 'superchat-entries-mode))
        (should (string-search "second note" (buffer-string)))
        ;; Newest first: t2's note, then t1's.
        (superchat-entries-open)
        (superchat-entries-next)
        (superchat-entries-open)
        (should (equal opened '("t1" "t2")))
        (should-error (superchat-entries-next) :type 'user-error)
        (superchat-entries-previous)
        (superchat-entries-open)
        (should (equal (car opened) "t2"))
        ;; g fetches anew: a note written since shows.
        (superchat-recall-note "t1" "three" "third note")
        (superchat-entries-refresh)
        (should (string-search "third note" (buffer-string)))))))

(ert-deftest superchat-views-timers-open-through-the-function ()
  (superchat-test-with-tape
    (superchat-tape-append "t1" 'timer "brief" :data '(:daily "09:00") :scope "/p/")
    (let* (opened (superchat-views-open-function (lambda (tape) (push tape opened))))
      (superchat-timers)
      (with-current-buffer "*superchat: timers*"
        (goto-char (point-min))
        (superchat-timers-open)
        (should (equal opened '("t1")))))))

(provide 'test-views)
;;; test-views.el ends here
