;;; timer-start-fault-regression.el --- a fired task that did not start -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for the scheduled task whose start goes wrong
;; (docs/decisions/accepted/2026-10-06-a-fired-task-that-did-not-start.md).
;; A task is fired by recording `timer.fired' first, so one occurrence never
;; fires twice.  These tests break what follows that record: the send fails,
;; Emacs exits before or during the send, or the conversation is busy.  The
;; task must then be started later, once, or be reported as unknown; it must
;; never be dropped silently and never be started twice.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/timer-start-fault-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^timer-fault/")'

;;; Code:

(load (expand-file-name "timer-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun tsf--task (root text)
  "Set a task with TEXT due in 60 seconds under ROOT; return (VIEW . SESSION).
The clock is then moved to the due time."
  (let* ((view (with-current-buffer (wtimer--file root) (superchat)))
         (session (wtimer--session view)))
    (superchat-timer-create session 'task text (+ wtimer--now 60))
    (setq wtimer--now (+ wtimer--now 60))
    (cons view session)))

(defun tsf--count (session type)
  (length (hm--events session type)))

(defmacro tsf--send-failing (&rest body)
  "Run BODY while every send signals an error."
  `(cl-letf (((symbol-function 'superchat-send-input)
              (lambda (&rest _) (error "TSF injected: send fails"))))
     ,@body))

(ert-deftest timer-fault/a-started-task-records-its-run ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let* ((session (cdr (tsf--task p "TSF-A do the thing")))
           (fired (car (superchat-timer-catch-up))))
      (wtimer--settled)
      (should (stringp (plist-get fired :run-id)))
      (let ((started (hm--events session "timer.started")))
        (should (= 1 (length started)))
        (should (equal (plist-get fired :run-id)
                       (plist-get (plist-get (car started) :payload) :run-id))))
      ;; The send is on record too: it is what a restart reads to know
      ;; that a send may have been cut off.
      (should (= 1 (tsf--count session "timer.sending")))
      (should-not (superchat-timer-unstarted-tasks)))))

(ert-deftest timer-fault/a-task-whose-send-failed-is-started-later-once ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let* ((pair (tsf--task p "TSF-B do the thing"))
           (view (car pair)) (session (cdr pair)))
      (tsf--send-failing
       (let ((fired (car (superchat-timer-catch-up))))
         (should-not (plist-get fired :run-id))))
      (should (= 0 (length hm--wires)))
      (should (= 1 (length (superchat-timer-unstarted-tasks))))
      ;; The failed send left nothing in the input area.
      ;; Only the line that says it came due names the task.
      (with-current-buffer view
        (should (= 1 (how-many "TSF-B" (point-min) (point-max)))))
      (should (timerp superchat-timer--retry-timer))
      ;; A retry before the task's own wait is over does not try it, and
      ;; leaves a retry scheduled for when it is.
      (cancel-timer superchat-timer--retry-timer)
      (setq superchat-timer--retry-timer nil)
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 0 (length hm--wires)))
      (should (timerp superchat-timer--retry-timer))
      (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds))
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 1 (length hm--wires)))
      (should (string-match-p "TSF-B" (mapconcat #'hm--text (hm--wire 1) "\n")))
      (should-not (superchat-timer-unstarted-tasks))
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 1 (length hm--wires)))
      (should (= 1 (tsf--count session "timer.fired")))
      (should (= 1 (tsf--count session "intent.received"))))))

(ert-deftest timer-fault/a-task-fired-before-emacs-exited-starts-on-restart ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let ((session (cdr (tsf--task p "TSF-C do the thing"))))
      ;; Emacs exits right after `timer.fired' is written.
      (cl-letf (((symbol-function 'superchat-timer--try-task) #'ignore))
        (superchat-timer-catch-up))
      (should (= 0 (length hm--wires)))
      (should (= 0 (tsf--count session "timer.sending")))
      ;; The restart.
      (superchat-timer-disarm-all)
      (should-not (superchat-timer-catch-up))
      (wtimer--settled)
      (should (= 1 (length hm--wires)))
      (should (= 1 (tsf--count session "timer.fired")))
      (should (= 1 (tsf--count session "timer.started")))
      (should-not (superchat-timer-catch-up))
      (wtimer--settled)
      (should (= 1 (length hm--wires))))))

(ert-deftest timer-fault/a-task-waits-while-its-conversation-is-busy ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let ((session (cdr (tsf--task p "TSF-D do the thing"))))
      (cl-letf (((symbol-function 'superchat-timer--session-busy-p)
                 (lambda (_session) t)))
        (let ((fired (car (superchat-timer-catch-up))))
          (should-not (plist-get fired :run-id))))
      (should (= 0 (length hm--wires)))
      (should (= 0 (tsf--count session "timer.sending")))
      (should (= 0 (tsf--count session "intent.received")))
      (should (timerp superchat-timer--retry-timer))
      ;; Waiting for a busy conversation is not a failed start.
      (should (= 0 (hash-table-count superchat-timer--retries)))
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 1 (length hm--wires)))
      (should-not (superchat-timer-unstarted-tasks)))))

(ert-deftest timer-fault/the-busy-check-sees-a-real-running-execution ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (wtimer--session view)))
      (should-not (superchat-timer--session-busy-p session))
      (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                 (lambda () '("run-tsf")))
                ((symbol-function 'superchat-runtime-status)
                 (lambda (_run) (list :session-id session))))
        (should (superchat-timer--session-busy-p session))
        (should-not (superchat-timer--session-busy-p "another-session"))))))

(ert-deftest timer-fault/a-failing-start-is-retried-a-limited-number-of-times ()
  (wtimer--with nil (p)
    (let ((session (cdr (tsf--task p "TSF-E do the thing")))
          (superchat-timer-retry-limit 2))
      (tsf--send-failing
       (superchat-timer-catch-up)
       (should (timerp superchat-timer--retry-timer))
       (dotimes (_ 2)
         (cancel-timer superchat-timer--retry-timer)
         (setq superchat-timer--retry-timer nil)
         (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds))
         (superchat-timer-retry-tasks))
       ;; The third failure is past the limit: nothing more is scheduled.
       (should-not superchat-timer--retry-timer)
       (should (= 3 (tsf--count session "timer.sending")))
       ;; And a later retry, whatever caused it, does not try it again.
       (setq wtimer--now (+ wtimer--now (* 10 superchat-timer-retry-seconds)))
       (superchat-timer-retry-tasks)
       (should (= 3 (tsf--count session "timer.sending")))
       (should-not superchat-timer--retry-timer))
      ;; It is still on record as not started, for the next start of Emacs.
      (should (= 1 (length (superchat-timer-unstarted-tasks))))
      (should (= 1 (tsf--count session "timer.fired"))))))

(ert-deftest timer-fault/a-send-cut-off-by-an-exit-is-reported-not-repeated ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let* ((pair (tsf--task p "TSF-F do the thing"))
           (view (car pair)) (session (cdr pair)))
      (cl-letf (((symbol-function 'superchat-timer--try-task) #'ignore))
        (superchat-timer-catch-up))
      ;; Emacs exited inside the send: `timer.sending' is on record, a Run
      ;; started in the conversation, and no `timer.started' says whose.
      (let* ((timer-id (plist-get (plist-get (car (hm--events session "timer.fired"))
                                             :payload)
                                  :timer-id))
             (occurrence (plist-get (car (superchat-timer-unstarted-tasks))
                                    :occurrence)))
        (superchat-timer--fact session "timer.sending" timer-id occurrence)
        (superchat-tape-append-event
         (list :type "run.started" :session-id session
               :run-id "run-tsf-cut-off" :turn-id "turn-tsf-cut-off"
               :actor-id "runtime" :summary "a run of unknown origin")))
      (superchat-timer-retry-tasks)
      (should (= 0 (length hm--wires)))
      (should (= 1 (tsf--count session "timer.unconfirmed")))
      (should-not (superchat-timer-unstarted-tasks))
      (with-current-buffer view
        (should (string-match-p "cannot tell whether it started" (buffer-string))))
      (superchat-timer-retry-tasks)
      (should (= 1 (tsf--count session "timer.unconfirmed")))
      (should (= 0 (length hm--wires))))))

(ert-deftest timer-fault/a-send-that-never-reached-a-run-is-started-again ()
  (wtimer--with '((:text "ANSWER")) (p)
    (let ((session (cdr (tsf--task p "TSF-G do the thing"))))
      (cl-letf (((symbol-function 'superchat-timer--try-task) #'ignore))
        (superchat-timer-catch-up))
      (let ((task (car (superchat-timer-unstarted-tasks))))
        (superchat-timer--fact session "timer.sending"
                               (plist-get task :id) (plist-get task :occurrence)))
      ;; No Run started after the send: it is safe to send again.
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 1 (length hm--wires)))
      (should (= 0 (tsf--count session "timer.unconfirmed")))
      (should-not (superchat-timer-unstarted-tasks)))))

(ert-deftest timer-fault/a-reminder-is-never-an-unstarted-task ()
  (wtimer--with nil (p)
    (let ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat)))))
      (superchat-timer-create session 'reminder "call mom" (+ wtimer--now 60))
      (setq wtimer--now (+ wtimer--now 60))
      (superchat-timer-catch-up)
      (should-not (superchat-timer-unstarted-tasks))
      (superchat-timer-retry-tasks)
      (should (= 0 (length hm--wires))))))

(ert-deftest timer-fault/a-task-fired-before-this-was-recorded-is-left-alone ()
  (wtimer--with nil (p)
    (let* ((session (cdr (tsf--task p "TSF-H old task")))
           (timer (car (superchat-timer-list))))
      ;; The shape of a `timer.fired' written before starts were recorded.
      (superchat-tape-append-event
       (list :type "timer.fired" :session-id session :actor-id "timers"
             :summary "Scheduled task: TSF-H old task"
             :payload (list :timer-id (plist-get timer :id)
                            :occurrence wtimer--now
                            :kind "task" :text "TSF-H old task")))
      (should-not (superchat-timer-unstarted-tasks))
      (superchat-timer-retry-tasks)
      (should (= 0 (length hm--wires))))))

(ert-deftest timer-fault/a-cancelled-task-is-not-started-later ()
  (wtimer--with nil (p)
    (tsf--task p "TSF-I do the thing")
    (let ((id (car (wtimer--ids))))
      (should (stringp id))
      (cl-letf (((symbol-function 'superchat-timer--try-task) #'ignore))
        (superchat-timer-catch-up))
      (should (= 1 (length (superchat-timer-unstarted-tasks))))
      ;; It has fired, so it is no longer a pending timer; it can still
      ;; be cancelled.
      (should-not (wtimer--ids))
      (should (superchat-timer-cancel id))
      (should-not (superchat-timer-unstarted-tasks))
      (should-not (superchat-timer-cancel id))
      (superchat-timer-retry-tasks)
      (should (= 0 (length hm--wires))))))

(ert-deftest timer-fault/the-new-facts-are-known-tape-events ()
  (dolist (type '("timer.sending" "timer.started" "timer.unconfirmed"))
    (should (eq 'session-annotation (superchat-tape-event-relevance type)))))

;;;; Two tasks on the one retry timer ------------------------------------------

(defun tsf--fire-retry ()
  "Run the retry the product scheduled, as its Emacs timer would."
  (should (timerp superchat-timer--retry-timer))
  (let* ((timer superchat-timer--retry-timer)
         (function (timer--function timer))
         (arguments (timer--args timer)))
    (cancel-timer timer)
    (apply function arguments)))

(ert-deftest timer-fault/a-busy-task-does-not-carry-a-failing-one-past-its-limit ()
  "The review's N1: one task keeps failing, another's conversation stays busy."
  (wtimer--with nil (p q)
    (let* ((failing (cdr (tsf--task p "TSF-N1-FAILING")))
           (busy (cdr (tsf--task q "TSF-N1-BUSY")))
           (superchat-timer-retry-limit 2)
           (tries 0))
      (cl-letf (((symbol-function 'superchat-timer--session-busy-p)
                 (lambda (session) (equal session busy)))
                ((symbol-function 'superchat-send-input)
                 (lambda () (cl-incf tries) (error "start unavailable"))))
        (superchat-timer-catch-up)
        (should (= 1 tries))
        ;; The busy task keeps the retry timer going every 30 seconds.  The
        ;; failing one is not tried again until its own wait is over.
        (dotimes (_ 5)
          (setq wtimer--now (+ wtimer--now superchat-timer-busy-retry-seconds))
          (tsf--fire-retry))
        (should (= 1 tries))
        ;; Its wait over, it is tried; and again; and then it is out of tries.
        (dotimes (_ 6)
          (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds))
          (tsf--fire-retry))
        (should (= 3 tries))
        (should (= 3 (tsf--count failing "timer.sending")))
        ;; The busy task was never sent, and still waits for its conversation.
        (should (= 0 (tsf--count busy "timer.sending")))
        (should (timerp superchat-timer--retry-timer))
        (should (= 2 (length (superchat-timer-unstarted-tasks))))))))

(ert-deftest timer-fault/the-retry-timer-is-due-at-the-soonest-time-asked ()
  "A task that failed waits five minutes; a busy one must not wait with it."
  (wtimer--with nil (p)
    (unwind-protect
        (progn
          (superchat-timer--retry-later superchat-timer-retry-seconds)
          (should (> (- superchat-timer--retry-at (float-time))
                     (- superchat-timer-retry-seconds 5)))
          (superchat-timer--retry-later superchat-timer-busy-retry-seconds)
          (should (<= (- superchat-timer--retry-at (float-time))
                      superchat-timer-busy-retry-seconds))
          ;; A later time asked for afterwards does not push it back.
          (let ((due superchat-timer--retry-at))
            (superchat-timer--retry-later superchat-timer-retry-seconds)
            (should (= due superchat-timer--retry-at))))
      (superchat-timer-disarm-all))))

(ert-deftest timer-fault/a-task-that-started-has-no-wait-left ()
  "Guard: what a failure set is cleared once the task starts."
  (wtimer--with '((:text "ANSWER")) (p)
    (let ((session (cdr (tsf--task p "TSF-N1-RECOVERS"))))
      (tsf--send-failing (superchat-timer-catch-up))
      (should (= 1 (hash-table-count superchat-timer--not-before)))
      (setq wtimer--now (+ wtimer--now superchat-timer-retry-seconds))
      (superchat-timer-retry-tasks)
      (wtimer--settled)
      (should (= 1 (tsf--count session "timer.started")))
      (should (= 0 (hash-table-count superchat-timer--not-before)))
      (should (= 0 (hash-table-count superchat-timer--retries))))))

(provide 'timer-start-fault-regression)
;;; timer-start-fault-regression.el ends here
