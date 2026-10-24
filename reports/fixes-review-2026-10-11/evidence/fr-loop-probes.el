;;; fr-loop-probes.el --- endings that leave no durable account, after today -*- lexical-binding: t; -*-
;; For the user's question "is the runtime closed-loop?".  Each test ends
;; something and then asks: is there a durable fact saying how it ended,
;; and was anybody told?  Observations, defects included.
(load (expand-file-name "scripts/pending-regression.el" default-directory) nil t)
(require 'superchat-pending)

(defun fr-loop--restart (session)
  "What a restart leaves: this Emacs knows no Run; the Tape is sealed on reopen."
  (clrhash superchat-runtime--runs)
  (when-let* ((view (superchat-timer--live-view session))) (kill-buffer view))
  (let ((default-directory superchat-data-directory))
    (superchat--render-session-view session)))

(ert-deftest fr-loop/a-scheduled-task-cut-off-mid-run-is-told-to-nobody-after-a-restart ()
  "The task started (timer.started on record), Emacs quit while it ran.
After the restart the Run is sealed abandoned on the Tape; no notice is
left, no list row says it, and the timer module does not start it again."
  (pend--fresh
    (wrec--with '((:text "NEVER-DELIVERED" :delay 30)) (p)
      (let* ((now 1800000000.0)
             (superchat-timer-now-function (lambda () now))
             (superchat-timer-notify-function #'ignore)
             (session (hm--session (wrec--view p))))
        (unwind-protect
            (progn
              (superchat-timer-create session 'task "FR-LOOP cut off task" (+ now 60))
              (setq now (+ now 60))
              (let ((fired (car (superchat-timer-catch-up))))
                (should (stringp (plist-get fired :run-id)))
                (should (= 1 (length (hm--events session "timer.started"))))
                (fr-loop--restart session)
                (let ((sealed (cl-remove-if-not
                               (lambda (e) (and (equal "run.failed" (plist-get e :type))
                                                (eq t (plist-get (plist-get e :payload) :abandoned))))
                               (superchat-tape-replay-events session))))
                  (superchat-timer-arm-all)
                  (princ (format "FR-LOOP TASK CUT OFF: sealed abandoned on Tape=%d; notices=%S; pending rows=%S; unstarted tasks=%S; unconfirmed=%S\n"
                                 (length sealed) superchat-changes--notices
                                 (mapcar (lambda (i) (list (plist-get i :kind) (plist-get i :what))) (superchat-pending-items))
                                 (superchat-timer-unstarted-tasks) (superchat-timer-unconfirmed-tasks)))
                  (should (= 1 (length sealed)))
                  (should-not superchat-changes--notices)
                  (should-not (superchat-pending-items)))))
          (superchat-timer-disarm-all))))))

(ert-deftest fr-loop/a-users-execution-cut-off-mid-run-is-sealed-and-listed-nowhere ()
  (pend--fresh
    (wrec--with '((:text "NEVER-DELIVERED" :delay 30)) (p)
      (let* ((view (wrec--view p)) (session (hm--session view))
             (run (smoke--send view "a question Emacs quits during")))
        (should (stringp run))
        (fr-loop--restart session)
        (let ((sealed (cl-remove-if-not
                       (lambda (e) (and (equal run (plist-get e :run-id))
                                        (equal "run.failed" (plist-get e :type))))
                       (superchat-tape-replay-events session))))
          (princ (format "FR-LOOP USER RUN CUT OFF: sealed=%d pending rows=%S notices=%S\n"
                         (length sealed) (superchat-pending-items) superchat-changes--notices))
          (should (= 1 (length sealed)))
          (should-not (superchat-pending-items)))))))

(ert-deftest fr-loop/a-run-end-observer-that-fails-is-a-fault-nobody-is-told-of ()
  "superchat-emit records an observer's error in the in-memory fault ring and
says nothing (superchat-event.el:131-150).  The changes row's own observer
wraps its summary in ignore-errors (superchat-changes.el:289-302), so a
failure there is not even a fault."
  (pend--fresh
    (wrec--with '((:text "DONE")) (p)
      (let* ((view (wrec--view p)) said)
        (cl-letf (((symbol-function 'superchat-changes--note-unattended)
                   (lambda (&rest _) (error "FR injected: notice observer fails")))
                  ((symbol-function 'superchat-changes-run-summary)
                   (lambda (&rest _) (error "FR injected: summary fails")))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (when format-string (push (apply #'format format-string args) said)) nil)))
          (hm--turn view "Do something"))
        (let ((faults (mapcar (lambda (f) (list (plist-get f :site) (plist-get f :id))) superchat-event-faults))
              (told (cl-remove-if-not (lambda (s) (string-match-p "FR injected\\|fault\\|observer" s)) said)))
          (princ (format "FR-LOOP OBSERVER FAULT: faults=%S; anything said about them=%S\n" faults told))
          (should (cl-find 'changes-unattended-notice superchat-event-faults
                           :key (lambda (f) (plist-get f :id))))
          (should-not (cl-find 'changes-notice superchat-event-faults
                               :key (lambda (f) (plist-get f :id))))
          (should-not told))))))

(ert-deftest fr-loop/a-task-out-of-tries-leaves-no-durable-fact-and-is-tried-again-after-a-restart ()
  (pend--fresh
    (wtimer--with '((:text "STARTED-AFTER-RESTART")) (p)
      (let ((session (cdr (tsf--task p "FR-LOOP gave up")))
            (superchat-timer-retry-limit 0))
        (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)
        (tsf--send-failing (superchat-timer-catch-up))
        (let ((types (delete-dups (mapcar (lambda (e) (plist-get e :type))
                                          (cl-remove-if-not (lambda (e) (string-prefix-p "timer." (plist-get e :type)))
                                                            (superchat-tape-replay-events session))))))
          (princ (format "FR-LOOP OUT OF TRIES: timer facts on the Tape=%S (no fact says it was given up); retry timer=%S; list row=%S\n"
                         types superchat-timer--retry-timer
                         (mapcar (lambda (i) (list (plist-get i :kind) (plist-get i :detail))) (pend--of 'due))))
          (should-not (member "timer.gave-up" types))
          ;; A new Emacs: the in-memory count is gone and the task is tried again.
          (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)
          (superchat-timer-retry-tasks)
          (wtimer--settled)
          (should (= 1 (tsf--count session "timer.started"))))
        (clrhash superchat-timer--retries) (clrhash superchat-timer--not-before)))))

(ert-run-tests-batch-and-exit "^fr-loop/")
;;; fr-loop-probes.el ends here
