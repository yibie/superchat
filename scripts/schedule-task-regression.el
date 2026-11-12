;;; schedule-task-regression.el --- the list of what you set to run -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for `M-x superchat-schedule-task' (superchat-schedule.el;
;; docs/decisions/accepted/2026-11-06-a-list-of-what-you-set-to-run.md).
;; Real timers fired at their times, real commands, a scripted model.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-jit-compilation nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/schedule-task-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^schedule-task/")'

;;; Code:

(load (expand-file-name "compile-watch-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-schedule)

(defun sched--row (id)
  (cl-find id (superchat-schedule-items) :key (lambda (item) (plist-get item :id))
           :test #'equal))

(defun sched--hourly-task (root text)
  "A task under ROOT due in a minute, then every hour: (VIEW TIMER)."
  (let* ((view (with-current-buffer (wtimer--file root) (superchat)))
         (timer (superchat-timer-create (wtimer--session view) 'task text
                                        (+ wtimer--now 60) 3600)))
    (list view timer)))

(ert-deftest schedule-task/each-one-set-is-a-row-of-its-kind ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (session (hm--session view))
           (reminder (superchat-timer-create session 'reminder "CALL-MUM" (+ wtimer--now 600)))
           (command (tcmd--set session "STARS" "echo 1" 86400))
           (rule (cwatch--rule view)))
      (should (= 3 (length (superchat-schedule-items))))
      (let ((row (sched--row (plist-get reminder :id))))
        (should (equal "reminder" (plist-get row :kind)))
        (should (equal "once" (plist-get row :when)))
        (should (equal (superchat-timer-format-time (+ wtimer--now 600)) (plist-get row :next)))
        (should (= 0 (plist-get row :ran))))
      (let ((row (sched--row (plist-get command :id))))
        (should (equal "command" (plist-get row :kind)))
        (should (equal "daily" (plist-get row :when))))
      (let ((row (sched--row (plist-get rule :id))))
        (should (equal "rule (compile)" (plist-get row :kind)))
        (should (string-match-p "compilation" (plist-get row :when))))
      (should (string-match-p "3 set: 2 timers, 1 rule" (superchat-schedule--header))))))

(ert-deftest schedule-task/a-task-that-ran-again-and-again-says-how-many-times ()
  ;; 2026-11-06, the user: an hourly task piled up rows like a log.
  (wtimer--with '((:text "FIRST") (:text "SECOND") (:text "THIRD")) (p)
    (cl-destructuring-bind (_view timer) (sched--hourly-task p "EVERY-HOUR")
      (own-place--fire-times timer 3)
      (should (= 1 (length (superchat-schedule-items))))
      (let ((row (sched--row (plist-get timer :id))))
        (should (= 3 (plist-get row :ran)))
        (should (equal "task" (plist-get row :kind)))
        (should (equal "hourly" (plist-get row :when)))
        (should (string-match-p "THIRD" (plist-get row :last)))
        (should (equal "3 not looked at" (plist-get row :state)))))))

(ert-deftest schedule-task/a-command-says-how-it-went ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (tcmd--set (hm--session view) "STARS" (tcmd--reading p "71\n") 86400)))
      (tcmd--fire timer)
      (let ((row (sched--row (plist-get timer :id))))
        (should (>= (plist-get row :ran) 1))
        (should (member (plist-get row :last) '("first run" "no change" "changed")))))))

(ert-deftest schedule-task/k-cancels-only-when-you-say-yes ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           (timer (superchat-timer-create (hm--session view) 'reminder "CALL-MUM"
                                          (+ wtimer--now 600))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (superchat-schedule-cancel (sched--row (plist-get timer :id))))
      (should (sched--row (plist-get timer :id)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (superchat-schedule-cancel (sched--row (plist-get timer :id))))
      (should-not (superchat-schedule-items)))))

(ert-deftest schedule-task/what-was-cancelled-or-is-done-is-not-listed ()
  (wtimer--with '((:text "DONE")) (p)
    (cl-destructuring-bind (_view _session timer) (presence--task p "ONCE-ONLY")
      (own-place--fire-and-settle timer)
      (should-not (sched--row (plist-get timer :id))))))

(ert-deftest schedule-task/ret-shows-the-last-result-and-it-counts-as-looked-at ()
  (wtimer--with '((:text "FIRST") (:text "SECOND")) (p)
    (cl-destructuring-bind (_view timer) (sched--hourly-task p "EVERY-HOUR")
      (own-place--fire-times timer 2)
      (superchat-schedule-open (sched--row (plist-get timer :id)))
      (should (equal "set" (plist-get (sched--row (plist-get timer :id)) :state)))
      (should-not (cl-remove-if-not (lambda (item) (eq 'unseen (plist-get item :kind)))
                                    (superchat-pending-items))))))

(ert-deftest schedule-task/t-brings-the-last-result-in ()
  (wtimer--with '((:text "FIRST") (:text "SECOND")) (p)
    (cl-destructuring-bind (view timer) (sched--hourly-task p "EVERY-HOUR")
      (own-place--fire-times timer 2)
      (superchat-schedule-bring (sched--row (plist-get timer :id)))
      (should (string-match-p "^> SECOND$" (superchat-butler-message view))))))

(ert-deftest schedule-task/a-task-waiting-to-start-says-so ()
  ;; Its conversation has a half-written message: the task waits behind it.
  (wtimer--with '((:text "FIRST")) (p)
    (cl-destructuring-bind (_view timer) (sched--hourly-task p "EVERY-HOUR")
      (setq wtimer--now (+ wtimer--now 60))
      (let* ((due (plist-get timer :due))
             (home (plist-get (superchat-timer-fire timer due) :session-id)))
        (wtimer--settled)
        (presence--type (superchat-timer--live-view home) "HALF-WRITTEN")
        (setq wtimer--now (+ wtimer--now 3600))
        (superchat-timer-fire timer (+ due 3600))
        (should (equal "due, not started" (plist-get (sched--row (plist-get timer :id)) :state)))))))

(ert-deftest schedule-task/a-command-running-says-so ()
  (wtimer--with nil (p)
    (let* ((view (tcmd--view p))
           ;; Set, it runs once at once; not waited for here.
           (timer (superchat-timer-create (hm--session view) 'command "SLOW"
                                          (+ wtimer--now 3600) 86400 "sleep 3; echo done")))
      (should (equal "running" (plist-get (sched--row (plist-get timer :id)) :state)))
      (tcmd--settled))))

(ert-deftest schedule-task/nothing-set-says-so ()
  (wtimer--with nil (_p)
    (should-not (superchat-schedule-items))
    (should (string-match-p "0 set" (superchat-schedule--header)))))

(ert-deftest schedule-task/the-command-opens-the-list ()
  (wtimer--with nil (p)
    (superchat-timer-create (hm--session (tcmd--view p)) 'reminder "CALL-MUM" (+ wtimer--now 600))
    (let ((buffer (superchat-schedule-task)))
      (with-current-buffer buffer
        (should (derived-mode-p 'superchat-schedule-mode))
        (should (string-match-p "CALL-MUM" (buffer-string)))))))

(provide 'schedule-task-regression)
;;; schedule-task-regression.el ends here
