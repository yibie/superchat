;;; p4-view-dependency.el --- automated sends need a chat View buffer -*- lexical-binding: t; -*-

;; Research probe, read-only, against the snapshot of 75e83370.  Observes
;; (does not judge) that a scheduled task and a woken Work Record both reach
;; Runtime by creating a chat View buffer and typing into it, so an automated
;; execution leaves a chat buffer behind even when nobody opened one.

(load (expand-file-name "scripts/timer-start-fault-regression.el" default-directory) nil t)
(mapc (lambda (test) (ert-delete-test (ert-test-name test))) (ert-select-tests t t))
;; The work-record fixture loads the history fixture again, which redefines
;; its tests; ERT refuses a redefinition unless the first set is gone.
(load (expand-file-name "scripts/work-record-regression.el" default-directory) nil t)
(mapc (lambda (test) (ert-delete-test (ert-test-name test))) (ert-select-tests t t))

(defun p4--chat-views ()
  (cl-remove-if-not (lambda (b) (buffer-local-value 'superchat-mode b)) (buffer-list)))

(ert-deftest p4/timer-fire-creates-a-chat-view-when-none-exists ()
  (wtimer--with '((:text "TASK DONE")) (p)
    (let* ((pair (tsf--task p "P4 scheduled task"))
           (view (car pair))
           (session (cdr pair)))
      ;; The user closed the conversation's buffer before the task came due.
      (let ((kill-buffer-query-functions nil)) (kill-buffer view))
      (let ((before (length (p4--chat-views)))
            (gone (file-name-as-directory (make-temp-file "p4-gone-" t))))
        ;; First as it happened by accident on the first run of this probe: an
        ;; Emacs timer runs in whatever buffer is current, and the View the
        ;; fire creates takes that buffer's `default-directory'.
        (delete-directory gone)
        (with-temp-buffer
          (setq default-directory gone)
          (superchat-timer-catch-up))
        (smoke--wait (lambda () (= 1 (tsf--count session "timer.started"))) 2)
        (princ (format "P4 OBSERVED (timer, ambient buffer's directory missing): timer.fired=%d timer.sending=%d timer.started=%d; chat views=%d\n"
                       (tsf--count session "timer.fired")
                       (tsf--count session "timer.sending")
                       (tsf--count session "timer.started")
                       (length (p4--chat-views))))
        ;; Then from a buffer whose directory exists: the retry starts it.  A
        ;; failed start may not be retried before `superchat-timer-retry-seconds'
        ;; (300) have passed on the timer's clock, so the clock is moved first.
        (setq wtimer--now (+ wtimer--now 301))
        (with-temp-buffer
          (setq default-directory p)
          (superchat-timer-retry-tasks))
        (should (smoke--wait (lambda () (= 1 (tsf--count session "timer.started"))) 10))
        (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20))
        (let ((after (p4--chat-views)))
          (princ (format "P4 OBSERVED (timer): chat View buffers before fire=%d after=%d; timer.sending=%d timer.started=%d; new view=%S; displayed in a window=%S\n"
                         before (length after)
                         (tsf--count session "timer.sending")
                         (tsf--count session "timer.started")
                         (mapcar #'buffer-name after)
                         (cl-some (lambda (b) (get-buffer-window b t)) after)))
          (should (> (length after) before)))))))

(ert-deftest p4/woken-record-creates-a-new-chat-view ()
  (wrec--with '((:text "WOKEN DONE")) (p)
    (wrec--with-clock
      (let* ((view (wrec--view p))
             (session (hm--session view))
             (id (superchat-work-start session "P4 record")))
        (superchat-work-wait session id "time" "later" (+ wrec--now 60))
        (setq wrec--now (+ wrec--now 61))
        (let ((before (length (p4--chat-views))))
          (superchat-work-wake-due)
          (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20))
          (let ((after (p4--chat-views)))
            (princ (format "P4 OBSERVED (wake): chat View buffers before=%d after=%d; views=%S; work.woken rows=%d; displayed in a window=%S\n"
                           before (length after) (mapcar #'buffer-name after)
                           (cl-count "work.woken" (superchat-work--rows)
                                     :key #'cadr :test #'equal)
                           (cl-some (lambda (b) (get-buffer-window b t)) after)))
            (should (> (length after) before))))))))

(ert-run-tests-batch-and-exit "^p4/")
