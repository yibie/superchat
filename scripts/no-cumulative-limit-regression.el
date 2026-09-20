;;; no-cumulative-limit-regression.el --- No cumulative transport limit by default -*- lexical-binding: t; -*-

;;; Commentary:

;; Contract: docs/decisions/accepted/2026-09-19-no-cumulative-transport-limit-by-default.md
;; Reuses the virtual clock of the M5 execution regression (its tests are
;; defined by the load and are not selected here).
;;
;;   emacs -Q --batch ... -l scripts/no-cumulative-limit-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^no-cumulative/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cus-edit)
(require 'wid-edit)
(load (expand-file-name "m5-execution-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; The fixture's variables are declared special per file; this file binds them
;; through `m5w-with-run' too.
(defvar m5w-now) (defvar m5w-timers) (defvar m5w-owner) (defvar m5w-requests)
(defvar m5w-events) (defvar m5w-final) (defvar m5w-live) (defvar m5w-output)
(defvar m5w-activity-hook)

(defun ncl--standard-value ()
  (eval (car (get 'superchat-response-timeout-hard-limit 'standard-value)) t))

(defun ncl--type-accepts-p (value)
  (let ((widget (widget-convert (custom-variable-type 'superchat-response-timeout-hard-limit))))
    (and (widget-apply widget :match value) t)))

(ert-deftest no-cumulative/the-default-is-no-limit ()
  (should-not (ncl--standard-value))
  (should (ncl--type-accepts-p nil))
  (should (ncl--type-accepts-p 900))
  (should-not (ncl--type-accepts-p 0))
  (should-not (ncl--type-accepts-p -5)))

(ert-deftest no-cumulative/a-conversation-that-keeps-progressing-is-not-cut-off ()
  "Well past the old 600 s, with progress inside every inactivity window."
  (m5w-with-run
    (let ((superchat-response-timeout-hard-limit nil))
      (m5w--start)
      (should-not (superchat-llm--transport-timer-owner-hard-timer m5w-owner))
      (should (eq 'disabled (superchat-llm--transport-timer-owner-hard-state m5w-owner)))
      (dotimes (index 100)
        (m5w--advance 8)
        (m5w--partial (1+ index) '(:reasoning "still working"))
        (m5w--tools (1+ index)))
      (should (> (- m5w-now 1000.0) 600))
      (should-not m5w-final)
      (should-not (m5w--status))
      (should-not (superchat-llm--transport-timer-owner-hard-timer m5w-owner)))))

(ert-deftest no-cumulative/silence-still-times-out ()
  "Guard: the inactivity watchdog does not depend on the cumulative limit."
  (m5w-with-run
    (let ((superchat-response-timeout-hard-limit nil))
      (m5w--start)
      (m5w--advance 11)
      (should (eq 'timed-out (m5w--status)))
      (should (string-match-p "no progress" m5w-final)))))

(ert-deftest no-cumulative/ask-user-pauses-and-resumes-without-a-budget ()
  "The trusted input wait stops the inactivity clock and nothing is exhausted;
the next dispatch starts a fresh inactivity window."
  (m5w-with-run
    (let ((superchat-response-timeout-hard-limit nil)
          (superchat--tool-execution-context
           '(:input-core-token test-input :input-core-tool-name "ask_user")))
      (m5w--start)
      (should (superchat-llm--transport-timer-owner-input-clocks-p m5w-owner))
      (m5w--advance 8)
      (let ((state (superchat-llm--input-pause-clock
                    m5w-owner (make-symbol "accepted-question"))))
        (should (eq 'disabled state)))
      (should-not (superchat-llm--transport-timer-owner-wait-timer m5w-owner))
      ;; The person takes twenty minutes.
      (m5w--advance 1200)
      (should-not m5w-final)
      (m5w--tools 1)
      (should (= 2 (length m5w-requests)))
      (should (superchat-llm--transport-timer-owner-wait-timer m5w-owner))
      (should-not (superchat-llm--transport-timer-owner-hard-timer m5w-owner))
      (should-not (cl-find 'input-clock-exhausted m5w-events :key #'car))
      ;; And the new window is a real one.
      (m5w--advance 11)
      (should (eq 'timed-out (m5w--status)))
      (should (string-match-p "no progress" m5w-final)))))

(ert-deftest no-cumulative/a-large-prompts-window-is-capped-only-by-a-set-limit ()
  (let ((superchat-response-timeout-hard-limit nil))
    (should (= 120 (superchat-llm--adaptive-response-timeout 120 1000)))
    (should (= (+ 120 (ceiling (/ (- 400000 4096) 256.0)))
               (superchat-llm--adaptive-response-timeout 120 400000))))
  (let ((superchat-response-timeout-hard-limit 600))
    (should (= 600 (superchat-llm--adaptive-response-timeout 120 400000)))))

(provide 'no-cumulative-limit-regression)

;;; no-cumulative-limit-regression.el ends here
