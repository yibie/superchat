;;; test-subagent.el --- Tests for superchat-subagent -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for sub-agent presets, runner, and delegation tool.

;;; Code:

(require 'ert)
(require 'superchat-subagent)

(ert-deftest test-subagent-preset-researcher ()
  "The researcher preset should be read-only."
  (let ((preset (superchat--subagent-preset 'researcher)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (member "read-file" (superchat-preset-tools preset)))
    (should (not (member "write-file" (superchat-preset-tools preset))))))

(ert-deftest test-subagent-preset-executor ()
  "The executor preset should include write/execute tools."
  (let ((preset (superchat--subagent-preset 'executor)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (member "write-file" (superchat-preset-tools preset)))
    (should (member "shell-command" (superchat-preset-tools preset)))))

(ert-deftest test-subagent-preset-introspector ()
  "The introspector preset should include Emacs introspection tools."
  (let ((preset (superchat--subagent-preset 'introspector)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (member "describe-function" (superchat-preset-tools preset)))
    (should (member "eval-elisp" (superchat-preset-tools preset)))))

(ert-deftest test-subagent-run-isolated ()
  "`superchat--subagent-run' should use an isolated session and history."
  (let ((main-session "main-session")
        (main-history '((:role user :content "main")))
        (superchat-buffer-name "*subagent-test-main*")
        (report nil))
    (unwind-protect
        (progn
          (get-buffer-create superchat-buffer-name)
          (setq superchat--session-id main-session
                superchat--conversation-history main-history)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer-sync)
                     (lambda (_prompt &optional _target-model _tools _agent-mode)
                       "sub-agent report")))
            (setq report (superchat--subagent-run 'researcher " investigate"))
            (should (string= report "sub-agent report"))
            ;; Main session/history should be unchanged.
            (should (string= superchat--session-id main-session))
            (should (equal superchat--conversation-history main-history))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-subagent-render-report ()
  "`superchat--subagent-render-report' should insert a report block."
  (let ((superchat-buffer-name "*subagent-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (superchat--subagent-render-report "researcher" "Found the answer.")
          (let ((content (buffer-string)))
            (should (string-match-p "Sub-agent report: researcher" content))
            (should (string-match-p "Found the answer." content))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-delegate-tool-calls-subagent ()
  "`superchat-tool-delegate-to-subagent' should run and render a sub-agent."
  (let ((superchat-buffer-name "*delegate-test*")
        (report nil))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (cl-letf (((symbol-function 'superchat--subagent-run)
                     (lambda (preset task &optional context)
                       (format "[%s] %s (ctx: %s)" preset task context)))
                    ((symbol-function 'superchat--subagent-render-report)
                     (lambda (_preset _report) nil)))
            (setq report (superchat-tool-delegate-to-subagent
                          "researcher" "find docs" "about hooks"))
            (should (string= report "[researcher] find docs (ctx: about hooks)"))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-cmd-subagent-parses-args ()
  "`superchat--cmd-subagent' should parse preset and task from args."
  (let ((result nil))
    (cl-letf (((symbol-function 'superchat-tool-delegate-to-subagent)
               (lambda (preset task _context)
                 (setq result (list preset task))
                 "report")))
      (superchat--cmd-subagent "subagent" "researcher find docs" nil nil nil)
      (should (equal result '("researcher" "find docs"))))))

(ert-deftest test-subagent-run-parallel-aggregates-reports ()
  "`superchat--subagent-run-parallel' should run specs and aggregate reports."
  (let* ((calls nil)
         (superchat-subagent-parallel-max 2)
         (specs '((:preset "researcher" :task "a")
                  (:preset "executor" :task "b")))
         (report nil))
    (cl-letf (((symbol-function 'superchat--subagent-run)
               (lambda (preset task _context)
                 (push (list preset task) calls)
                 (format "report-%s-%s" preset task))))
      (setq report (superchat--subagent-run-parallel specs))
      (should (= (length calls) 2))
      (should (string-match-p "report-researcher-a" report))
      (should (string-match-p "report-executor-b" report)))))

(ert-deftest test-subagent-run-parallel-handles-errors ()
  "`superchat--subagent-run-parallel' should not fail when one sub-agent errors."
  (let* ((superchat-subagent-parallel-max 2)
         (specs '((:preset "researcher" :task "a")
                  (:preset "executor" :task "b")))
         (report nil))
    (cl-letf (((symbol-function 'superchat--subagent-run)
               (lambda (preset task _context)
                 (if (string= preset "executor")
                     (error "executor failed")
                   (format "report-%s-%s" preset task)))))
      (setq report (superchat--subagent-run-parallel specs))
      (should (string-match-p "report-researcher-a" report))
      (should (string-match-p "ERROR: executor failed" report)))))

(ert-deftest test-subagent-run-parallel-respects-max ()
  "`superchat--subagent-run-parallel' should chunk work by superchat-subagent-parallel-max."
  (let* ((max-concurrent 0)
         (current 0)
         (superchat-subagent-parallel-max 2)
         (specs '((:preset "a" :task "1")
                  (:preset "b" :task "2")
                  (:preset "c" :task "3")
                  (:preset "d" :task "4")))
         (report nil))
    (cl-letf (((symbol-function 'superchat--subagent-run)
               (lambda (_preset _task _context)
                 (cl-incf current)
                 (setq max-concurrent (max max-concurrent current))
                 (sleep-for 0.05)
                 (cl-decf current)
                 "report")))
      (setq report (superchat--subagent-run-parallel specs))
      (should (<= max-concurrent 2))
      (should (string-match-p "Sub-agent report: d" report)))))

(provide 'test-subagent)

;;; test-subagent.el ends here
