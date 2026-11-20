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

(ert-deftest superchat-views-timers-say-when-and-how-in-words ()
  (let ((now (float-time)))
    (should (equal (superchat-timers--repeats '(:daily "03:00" :dream t)) "dream 03:00"))
    (should (equal (superchat-timers--repeats '(:every 7200)) "every 2h"))
    (should (equal (superchat-timers--repeats '(:at 1)) "once"))
    ;; The next time is never in the past, and at most a period away.
    (let ((next (superchat-timers--next '(:every 60 :from 0) now)))
      (should (and (>= next now) (<= next (+ now 60)))))
    (let ((next (superchat-timers--next '(:daily "03:00") now)))
      (should (and (> next now) (<= next (+ now 86400))))
      (should (equal (format-time-string "%H:%M" next) "03:00")))))

(provide 'test-views)
;;; test-views.el ends here
