;;; run-actor-regression.el --- an execution records who started it -*- lexical-binding: t; -*-

;; Guidance 2026-10-11, slice 2.  A scheduled task's Run and a woken Work
;; Record's Run say on `run.started' that Superchat started them, and which
;; part of it; a Run the user started keeps the Actor it had.

(load (expand-file-name "scripts/pending-regression.el" default-directory) nil t)
(let ((skip-loaded
       (lambda (load file &rest args)
         (unless (featurep (intern (file-name-base file)))
           (apply load file args)))))
  (advice-add 'load :around skip-loaded)
  (unwind-protect
      (load (expand-file-name "scripts/work-wake-fault-regression.el" default-directory) nil t)
    (advice-remove 'load skip-loaded)))

(defun ract--started (session)
  "The :actor-id of every `run.started' of SESSION, oldest first."
  (mapcar (lambda (e) (plist-get e :actor-id))
          (cl-remove-if-not (lambda (e) (equal "run.started" (plist-get e :type)))
                            (superchat-tape-replay-events session))))

(ert-deftest ract/a-scheduled-tasks-run-is-started-by-timers ()
  (pend--fresh
    (un--with-task '((:text "TASK")) (session fired)
      (should (equal '("timers") (ract--started session))))))

(ert-deftest ract/a-woken-records-run-is-started-by-work-records ()
  (pend--fresh
    (wrec--with '((:text "WOKEN")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((pair (wwf--due-record p "Actor"))
                 (views (superchat-work-wake-due)))
            (ignore pair)
            (should (= 1 (length views)))
            (wrec--settled)
            (should (equal '("work-records")
                           (ract--started (hm--session (car views)))))))))))

(ert-deftest ract/the-users-own-message-keeps-its-actor ()
  "Guard: a message the user sends records the Actor it always did."
  (pend--fresh
    (wrec--with '((:text "ANSWER")) (p)
      (let ((view (wrec--view p)))
        (hm--turn view "hello")
        (should (equal '("lead") (ract--started (hm--session view))))))))

(ert-deftest ract/the-users-message-after-a-woken-run-is-the-users ()
  "Who started a Run is said per Run, not per conversation."
  (pend--fresh
    (wrec--with '((:text "WOKEN") (:text "ANSWER")) (p)
      (let ((wrec--now wrec--now))
        (wrec--with-clock
          (let* ((_pair (wwf--due-record p "Actor after"))
                 (views (superchat-work-wake-due)))
            (wrec--settled)
            (hm--turn (car views) "and now me")
            (should (equal '("work-records" "lead")
                           (ract--started (hm--session (car views)))))))))))

(ert-deftest ract/a-scheduled-command-is-started-by-timers-too ()
  "A task whose text is a command with a template goes another way to the
model; its Run says who started it all the same."
  (pend--fresh
    (wrec--with '((:text "TEMPLATE ANSWER")) (p)
      (let* ((now 1800000000.0)
             (superchat-timer-now-function (lambda () now))
             (superchat-timer-notify-function #'ignore)
             (session (hm--session (wrec--view p))))
        (puthash "ractcmd" "RACT-TEMPLATE: $input" superchat--user-commands)
        (unwind-protect
            (progn
              (superchat-timer-create session 'task "/ractcmd fill me" (+ now 60))
              (setq now (+ now 60))
              (superchat-timer-catch-up)
              (wrec--settled)
              (should (equal '("timers") (ract--started session))))
          (remhash "ractcmd" superchat--user-commands)
          (superchat-timer-disarm-all))))))

(ert-deftest ract/a-message-built-for-later-says-who-started-it ()
  "The request composed for the queue carries the same name."
  (pend--fresh
    (wrec--with '((:text "UNUSED")) (p)
      (with-current-buffer (wrec--view p)
        (should-not (plist-get (superchat--build-chat-request "by the user")
                               :actor-id))
        (let ((superchat-automated-input 'timers))
          (should (equal "timers"
                         (plist-get (superchat--build-chat-request "by a timer")
                                    :actor-id))))))))

(ert-deftest ract/a-run-says-itself-that-superchat-started-it ()
  "No timer fact names this Run: what it recorded when it started is enough,
read from the Tape, so it holds after this Emacs has forgotten the Run."
  (pend--fresh
    (wrec--with '((:text "ANSWER") (:text "MINE")) (p)
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (auto (let ((superchat-automated-input 'timers))
                     (hm--turn view "sent by a timer")))
             (mine (hm--turn view "sent by me")))
        (should-not (hm--events session "timer.started"))
        (should (superchat-changes--unattended-run-p session auto))
        (should-not (superchat-changes--unattended-run-p session mine))
        (let ((superchat-runtime--runs (make-hash-table :test #'equal)))
          (should-not (superchat-runtime-status auto))
          (should (superchat-changes--unattended-run-p session auto))
          (should-not (superchat-changes--unattended-run-p session mine)))))))
