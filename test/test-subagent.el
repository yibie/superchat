;;; test-subagent.el --- Tests for superchat-subagent -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for sub-agent presets, runner, and delegation tool.

;;; Code:

(require 'ert)
(require 'superchat-agent-loop)
(require 'superchat-dispatcher)
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
                     (lambda (_prompt &optional _target-model _tools _agent-mode _system _preset)
                       "sub-agent report")))
            (setq report (superchat--subagent-run 'researcher " investigate"))
            (should (string= report "sub-agent report"))
            ;; Main session/history should be unchanged.
            (should (string= superchat--session-id main-session))
            (should (equal superchat--conversation-history main-history))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-subagent-sync-run-uses-fresh-tool-counter-buffer ()
  "A legacy sync sub-agent does not inherit the caller's tool counter."
  (let ((superchat-buffer-name "*subagent-counter-test*")
        captured-buffer-name
        captured-count)
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq-local superchat--agent-tool-call-count 49)
          (cl-letf (((symbol-function 'superchat--llm-generate-answer-sync)
                     (lambda (_prompt &optional _model _tools _agent _system _preset)
                       (setq captured-buffer-name (buffer-name)
                             captured-count superchat--agent-tool-call-count)
                       "ok")))
            (superchat--subagent-run 'researcher "task"))
          (should (= 49 superchat--agent-tool-call-count)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))
    (should (string-prefix-p " *superchat-subagent:" captured-buffer-name))
    (should (= 0 captured-count))))

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
  (let ((result nil)
        (superchat-buffer-name "*cmd-subagent-test*"))
    (unwind-protect
        (cl-letf (((symbol-function 'superchat--subagent-run-async)
                   (lambda (preset task _context callback &optional _depth)
                     (setq result (list preset task))
                     (funcall callback "report"))))
          (superchat--cmd-subagent "subagent" "researcher find docs" nil nil nil)
          (should (equal result '("researcher" "find docs"))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

;; =======================================================
;; Async engine (v1.3)
;; =======================================================

(ert-deftest test-subagent-control-plane-lists-and-cancels ()
  "A running request is visible and cancelled through llm.el."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        (request (list :request))
        (cancelled nil)
        (report nil))
    (cl-letf (((symbol-function 'superchat--subagent-llm-async)
               (lambda (_ctx _prompt _tools _model _system _callback)
                 request))
              ((symbol-function 'llm-cancel-request)
               (lambda (value) (setq cancelled value))))
      (let ((id (superchat--subagent-run-async
                 'researcher "task" nil (lambda (r) (setq report r)))))
        (should (string-match-p id (superchat-subagent-list-running)))
        (should (superchat-subagent-cancel id))
        (should (eq request cancelled))
        (should (string= "[Cancelled by user]" report))
        (should-not superchat--subagent-running)))))

(ert-deftest test-subagent-control-plane-finishes-once ()
  "A late provider callback after cancellation is ignored."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        (calls 0)
        provider-callback)
    (cl-letf (((symbol-function 'superchat--subagent-llm-async)
               (lambda (_ctx _prompt _tools _model _system callback)
                 (setq provider-callback callback)
                 'request))
              ((symbol-function 'llm-cancel-request) #'ignore))
      (let ((id (superchat--subagent-run-async
                 'researcher "task" nil (lambda (_r) (cl-incf calls)))))
        (superchat-subagent-cancel id)
        (funcall provider-callback "late")
        (should (= 1 calls))))))

(ert-deftest test-subagent-timeout-cancels-request ()
  "Preset timeout schedules cancellation through the shared path."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout 30)
        scheduled-delay scheduled-function report)
    (cl-letf (((symbol-function 'superchat--subagent-preset)
               (lambda (_name)
                 (superchat-preset-from-plist
                  '(:name "short" :type agent :body "Body" :tools none
                    :timeout 5))))
              ((symbol-function 'superchat--subagent-llm-async)
               (lambda (&rest _args) 'request))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest _args)
                 (setq scheduled-delay delay scheduled-function function)
                 'timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'llm-cancel-request) #'ignore))
      (let ((id (superchat--subagent-run-async
                 "short" "task" nil (lambda (value) (setq report value)))))
        (should-not report)
        (should (= 5 scheduled-delay))
        (funcall scheduled-function)
        (should-not (superchat--subagent-entry id))))))

(ert-deftest test-subagent-run-async-unknown-preset ()
  "Unknown preset is delivered as an error string, never signaled."
  (let ((report nil))
    (superchat--subagent-run-async
     "no-such-preset" "task" nil (lambda (r) (setq report r)))
    (should (stringp report))
    (should (string-match-p "\\[Error" report))
    (should (string-match-p "no-such-preset" report))))

(ert-deftest test-subagent-run-async-returns-report ()
  "The async runner builds the prompt and delivers the LLM answer."
  (let ((report nil)
        (captured-prompt nil)
        (captured-system nil)
        (captured-ctx nil))
    (cl-letf (((symbol-function 'superchat--subagent-llm-async)
               (lambda (ctx prompt _tools _model system callback)
                 (setq captured-ctx ctx
                       captured-prompt prompt
                       captured-system system)
                 (funcall callback "async report"))))
      (superchat--subagent-run-async
       'researcher "investigate X" "background info"
       (lambda (r) (setq report r))))
    (should (string= report "async report"))
    (should (string-match-p "investigate X" captured-prompt))
    (should (string-match-p "background info" captured-prompt))
    ;; Preset persona (body) travels as the system prompt.
    (should (stringp captured-system))
    (should (string-match-p "research sub-agent" captured-system))
    ;; Explicit context: isolated session id and default depth 1.
    (should (string-prefix-p "subagent-researcher-"
                             (plist-get captured-ctx :session-id)))
    (should (= 1 (plist-get captured-ctx :depth)))))

(ert-deftest test-subagent-run-async-does-not-touch-main-session ()
  "Async delegation must not mutate the main session's dynamic state."
  (let ((superchat--session-id "main-session")
        (superchat--conversation-history '((:role user :content "main")))
        (report nil))
    (cl-letf (((symbol-function 'superchat--subagent-llm-async)
               (lambda (_ctx _prompt _tools _model _system callback)
                 (funcall callback "ok"))))
      (superchat--subagent-run-async
       'researcher "task" nil (lambda (r) (setq report r))))
    (should (string= report "ok"))
    (should (string= superchat--session-id "main-session"))
    (should (equal superchat--conversation-history
                   '((:role user :content "main"))))))

(ert-deftest test-subagent-parallel-async-aggregates-in-spec-order ()
  "Results aggregate in spec order even when completion is out of order."
  (let ((held nil)   ; (callback . report) conses; car of list = launched last
        (final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset task _context callback &optional _depth)
                 (push (cons callback (format "report-%s-%s" preset task))
                       held))))
      (superchat--subagent-run-parallel-async
       '((:preset "researcher" :task "a")
         (:preset "executor" :task "b"))
       (lambda (r) (setq final r)))
      (should (= 2 (length held)))
      ;; Complete in REVERSE launch order: b first, then a.
      (dolist (entry held)
        (funcall (car entry) (cdr entry))))
    (should final)
    (should (< (string-match "report-researcher-a" final)
               (string-match "report-executor-b" final)))))

(ert-deftest test-subagent-parallel-async-launch-window ()
  "At most `superchat-subagent-parallel-max' sub-agents are in flight;
completions launch the queued remainder."
  (let ((superchat-subagent-parallel-max 2)
        (launched nil)  ; alist preset -> callback
        (final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset _task _context callback &optional _depth)
                 (push (cons preset callback) launched))))
      (superchat--subagent-run-parallel-async
       '((:preset "a" :task "1") (:preset "b" :task "2")
         (:preset "c" :task "3") (:preset "d" :task "4"))
       (lambda (r) (setq final r)))
      ;; Only the window is launched initially.
      (should (= 2 (length launched)))
      ;; Completing one launches exactly one more.
      (funcall (cdr (assoc "a" launched)) "ra")
      (should (= 3 (length launched)))
      (funcall (cdr (assoc "b" launched)) "rb")
      (should (= 4 (length launched)))
      (funcall (cdr (assoc "c" launched)) "rc")
      (funcall (cdr (assoc "d" launched)) "rd"))
    (should final)
    (should (string-match-p "Sub-agent report: d" final))))

(ert-deftest test-subagent-parallel-async-error-isolation ()
  "One failing sub-agent must not prevent the others from aggregating."
  (let ((final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset task _context callback &optional _depth)
                 (funcall callback
                          (if (string= preset "executor")
                              "[Error: executor failed]"
                            (format "report-%s-%s" preset task))))))
      (superchat--subagent-run-parallel-async
       '((:preset "researcher" :task "a")
         (:preset "executor" :task "b"))
       (lambda (r) (setq final r))))
    (should (string-match-p "report-researcher-a" final))
    (should (string-match-p "executor failed" final))))

(ert-deftest test-subagent-parse-specs ()
  "JSON task arrays parse into spec plists; bad JSON returns an error string."
  (let ((specs (superchat--subagent-parse-specs
                "[{\"preset\": \"researcher\", \"task\": \"find docs\"}]")))
    (should (listp specs))
    (should (equal "researcher" (plist-get (car specs) :preset)))
    (should (equal "find docs" (plist-get (car specs) :task))))
  (should (stringp (superchat--subagent-parse-specs "not json"))))

(ert-deftest test-subagent-depth-guard-blocks-nested-delegation ()
  "At the depth limit, delegate tools degrade to a denial stub."
  (skip-unless (fboundp 'llm-make-tool))
  (let* ((superchat-subagent-max-depth 1)
         (preset (superchat--subagent-preset 'researcher))
         (ctx (superchat--subagent-make-context preset 1))
         (tool (llm-make-tool :name "delegate_to_subagent"
                              :description "delegate"
                              :args nil
                              :async t
                              :function #'ignore))
         (wrapped (superchat--subagent-wrap-tool ctx tool)))
    (should-not (llm-tool-async wrapped))
    (should (string-match-p "depth limit"
                            (funcall (llm-tool-function wrapped)
                                     "researcher" "nested task")))))

(ert-deftest test-subagent-depth-guard-allows-below-limit ()
  "Below the depth limit, nested delegation re-enters at depth + 1."
  (skip-unless (fboundp 'llm-make-tool))
  (let* ((superchat-subagent-max-depth 2)
         (preset (superchat--subagent-preset 'researcher))
         (ctx (superchat--subagent-make-context preset 1))
         (tool (llm-make-tool :name "delegate_to_subagent"
                              :description "delegate"
                              :args nil
                              :async t
                              :function #'ignore))
         (wrapped (superchat--subagent-wrap-tool ctx tool))
         (captured-depth nil)
         (result nil))
    (should (llm-tool-async wrapped))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (_preset _task _context callback &optional depth)
                 (setq captured-depth depth)
                 (funcall callback "nested report"))))
      (funcall (llm-tool-function wrapped)
               (lambda (r) (setq result r))
               "researcher" "nested task"))
    (should (= 2 captured-depth))
    (should (string= result "nested report"))))

(ert-deftest test-subagent-wrapped-tool-counts-and-errors-as-strings ()
  "Wrapped sub-agent tools enforce max calls and stringify errors."
  (let* ((superchat-agent-max-tool-calls 50)
         (preset (superchat-preset-from-plist
                  (list :name "bounded" :type 'agent :max-tool-calls 2)))
         (ctx (superchat--subagent-make-context preset 1))
         (boom (lambda (&rest _) (error "boom")))
         (wrapped (superchat--subagent-wrap-function ctx "t" boom nil)))
    ;; Errors come back as strings, not signals.
    (should (string-match-p "\\[Tool error: boom\\]" (funcall wrapped "x")))
    (should (string-match-p "\\[Tool error: boom\\]" (funcall wrapped "x")))
    ;; Third call exceeds the limit.
    (should (string-match-p "exceeded maximum tool calls" (funcall wrapped "x")))))

(ert-deftest test-subagent-profile-can-require-destructive-confirmation ()
  "A profile can require confirmation when the global policy does not."
  (let* ((superchat-agent-confirm-destructive nil)
         (superchat-agent-destructive-tools '("write-file"))
         (preset (superchat-preset-from-plist
                  (list :name "careful" :type 'agent
                        :confirm-destructive t)))
         (ctx (superchat--subagent-make-context preset 1))
         (called nil)
         (wrapped (superchat--subagent-wrap-function
                   ctx "write-file" (lambda (&rest _) (setq called t)) nil)))
    (cl-letf (((symbol-function 'superchat--agent-ask-confirm)
               (lambda (_name _args) nil)))
      (should (string-match-p "cancelled" (funcall wrapped "file" "body"))))
    (should-not called)))

(ert-deftest test-subagent-placeholder-lifecycle ()
  "Placeholders render, then get replaced in place by the report."
  (let ((superchat-buffer-name "*subagent-ph-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (let ((ph1 (superchat--subagent-begin-placeholder "researcher"))
                (ph2 (superchat--subagent-begin-placeholder "executor")))
            (should (string-match-p "researcher: running" (buffer-string)))
            (should (string-match-p "executor: running" (buffer-string)))
            ;; Replace out of order: second placeholder first.
            (superchat--subagent-end-placeholder ph2 "executor" "exec done")
            (superchat--subagent-end-placeholder ph1 "researcher" "res done")
            (let ((content (buffer-string)))
              (should-not (string-match-p "running" content))
              (should (string-match-p "Sub-agent report: researcher" content))
              (should (string-match-p "res done" content))
              (should (string-match-p "Sub-agent report: executor" content))
              (should (string-match-p "exec done" content))
              ;; In-place: researcher block stays before executor block.
              (should (< (string-match "res done" content)
                         (string-match "exec done" content))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-subagent-end-placeholder-fallback ()
  "A nil placeholder falls back to appending the report."
  (let ((superchat-buffer-name "*subagent-ph-fallback*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (superchat--subagent-end-placeholder nil "researcher" "fallback report")
          (should (string-match-p "Sub-agent report: researcher" (buffer-string)))
          (should (string-match-p "fallback report" (buffer-string))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(provide 'test-subagent)

;;; test-subagent.el ends here
