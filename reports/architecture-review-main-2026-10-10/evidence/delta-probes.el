;;; delta-probes.el --- Main directory incremental review -*- lexical-binding: t; -*-
;; These probes assert observed behavior, including gaps, NOT acceptance.
;; Temporary fixtures only; no product or protected regression edits.
(load (expand-file-name "scripts/timer-start-fault-regression.el" default-directory) nil t)

(ert-deftest delta-probe/exhausted-task-retried-by-another-busy-task ()
  (wtimer--with nil (p q)
    (let* ((a (cdr (tsf--task p "FAIL-A")))
           (b (cdr (tsf--task q "BUSY-B")))
           (superchat-timer-retry-limit 2)
           (tries 0))
      (cl-letf (((symbol-function 'superchat-timer--session-busy-p)
                 (lambda (session) (equal session b)))
                ((symbol-function 'superchat-send-input)
                 (lambda () (cl-incf tries) (error "AUDIT start unavailable"))))
        (superchat-timer-catch-up)
        ;; Drive only callbacks actually scheduled by the product, not
        ;; unconditional manual retry scans. B perpetuates the shared timer.
        (dotimes (_ 5)
          (should (timerp superchat-timer--retry-timer))
          (let* ((timer superchat-timer--retry-timer)
                 (fn (timer--function timer))
                 (args (timer--args timer)))
            (cancel-timer timer)
            (apply fn args)))
        (should (= tries 6))
        (should (= 6 (tsf--count a "timer.sending")))
        (should (= 0 (tsf--count b "timer.sending")))
        (should (timerp superchat-timer--retry-timer))
        (princ (format "OBSERVED: retry-limit=2, initial+5 scheduled callbacks -> %d failed send attempts for A; B remains busy and keeps the retry timer alive.\n" tries))))))

(ert-run-tests-batch-and-exit "^delta-probe/")
