;;; notice-probe.el --- main review synchronous completion probe -*- lexical-binding: t; -*-
(load (expand-file-name "scripts/unattended-notice-regression.el" default-directory) nil t)
(ert-deftest delta-probe/synchronous-provider-timer-notice ()
  (un--fresh
    (wrec--with '((:text "FAST-ANSWER")) (p)
      (let* ((now 1800000000.0)
             (superchat-timer-now-function (lambda () now))
             (superchat-timer-notify-function #'ignore)
             (session (hm--session (wrec--view p)))
             (captured nil))
        (superchat-timer-create session 'task "FAST-TASK" (+ now 60))
        (setq now (+ now 60))
        ;; A synchronous callback is legal in the llm adapter path. Keep
        ;; actual timer -> dispatcher -> runtime -> tool/provider path.
        (cl-letf (((symbol-function 'smoke--fake-network)
                   (lambda (_mode provider prompt partial response error multi)
                     (push (hm--wire-of provider prompt) hm--wires)
                     (smoke--deliver provider prompt partial response error multi
                                     (or (pop smoke--script) '(:text "DONE")))
                     nil)))
          (setq captured (car (superchat-timer-catch-up)))
          (wrec--settled))
        (princ (format "FAST RESULT: fired=%S started=%d terminal=%d notices=%S\n"
                       captured (length (hm--events session "timer.started"))
                       (length (hm--events session "run.completed")) superchat-changes--notices))
        (should (= 1 (length (hm--events session "run.completed"))))
        (should (= 1 (length (hm--events session "timer.started"))))
        (should-not superchat-changes--notices)
        (princ "OBSERVED: synchronous completed scheduled Run has timer.started but no unattended notice.\n")))))

(ert-run-tests-batch-and-exit "^delta-probe/")
