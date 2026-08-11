;;; audit.el --- Full structural audit for one recovery trace -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)

(defun superchat-recovery-trace-audit--read (path)
  "Read one Lisp value from PATH." 
  (with-temp-buffer
    (insert-file-contents-literally path)
    (read (current-buffer))))

(defun superchat-recovery-trace-audit--events (events type)
  "Select EVENTS whose type is TYPE." 
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    events))

(defun superchat-recovery-trace-audit--json (text)
  "Parse JSON TEXT into plists and lists." 
  (json-parse-string text :object-type 'plist :array-type 'list
                     :null-object nil :false-object :json-false))

(defun superchat-recovery-trace-audit--lisp (text)
  "Read one printed Lisp value TEXT." 
  (with-temp-buffer
    (insert text)
    (read (current-buffer))))

(defun superchat-recovery-trace-audit--assert (name condition)
  "Print NAME and signal if CONDITION is false." 
  (princ (format "ASSERT %-30s %s\n" name (if condition "ok" "FAIL")))
  (unless condition
    (error "Trace assertion failed: %s" name)))

(defun superchat-recovery-trace-audit (path)
  "Audit every recorded boundary in trace PATH and return its summary." 
  (let* ((trace (superchat-recovery-trace-audit--read path))
         (events (plist-get trace :tape-events))
         (requests (plist-get trace :transport-requests))
         (logs (plist-get trace :llm-log))
         (snapshots
          (superchat-recovery-trace-audit--events
           events "decision.snapshot.recorded"))
         (tool-requests
          (superchat-recovery-trace-audit--events events "tool.requested"))
         (tool-failures
          (superchat-recovery-trace-audit--events events "tool.failed"))
         (tool-completions
          (superchat-recovery-trace-audit--events events "tool.completed"))
         (interventions
          (superchat-recovery-trace-audit--events
           events "experience.intervention.recorded"))
         (terminal-events
          (cl-remove-if-not
           (lambda (event)
             (member (plist-get event :type)
                     '("run.completed" "run.failed" "run.cancelled")))
           events))
         (terminal (car (last terminal-events)))
         (cell-verdict-passed
          (or (plist-get (plist-get trace :recovery-verdict)
                         :cell-verdict-passed)
              ;; Backward-compatible read of pre-review traces.
              (plist-get (plist-get trace :recovery-verdict)
                         :first-recovery-action-passed)
              (plist-get (plist-get (plist-get trace :result) :verdict)
                         :passed)))
         (runtime-status (plist-get trace :runtime-status))
         (decoded-bodies
          (mapcar (lambda (request)
                    (superchat-recovery-trace-audit--json
                     (plist-get request :body)))
                  requests))
         (receive-logs
          (superchat-recovery-trace-audit--events logs 'api-receive))
         (received-tool-names
          (mapcar
           (lambda (entry)
             (let ((text (plist-get entry :message)))
               (cond
                ((string-match-p "workspace_read" text) '("workspace_read"))
                ((string-match-p "workspace_info" text) '("workspace_info"))
                (t nil))))
           receive-logs))
         (requested-tool-names
          (mapcar (lambda (event)
                    (plist-get (plist-get event :payload) :tool-name))
                  tool-requests))
         (info-completion
          (car
           (last
            (cl-remove-if-not
             (lambda (event)
               (equal "workspace_info"
                      (plist-get (plist-get event :payload) :tool-name)))
             tool-completions))))
         (info-result
          (and info-completion
               (plist-get (plist-get info-completion :payload)
                          :result-json)))
         (second-body (cadr decoded-bodies))
         (second-messages (and second-body (plist-get second-body :messages)))
         (second-tool-calls
          (and second-messages
               (apply #'append
                      (mapcar (lambda (message)
                                (or (plist-get message :tool_calls) nil))
                              second-messages)))))
    (princ (format "TRACE %s\n" path))
    (princ (format "format=%S source=%S\n"
                   (plist-get trace :format) (plist-get trace :source)))
    (princ (format "counts transport=%d llm=%d tape=%d snapshots=%d tools=%d failures=%d interventions=%d\n"
                   (length requests) (length logs) (length events)
                   (length snapshots) (length tool-requests)
                   (length tool-failures) (length interventions)))
    (princ (format "transport-roles=%S\n"
                   (mapcar (lambda (body)
                             (mapcar (lambda (message) (plist-get message :role))
                                     (plist-get body :messages)))
                           decoded-bodies)))
    (princ (format "llm-types=%S received-tools=%S\n"
                   (mapcar (lambda (entry) (plist-get entry :type)) logs)
                   received-tool-names))
    (princ (format "tape-tools=%S terminal=%S runtime-status=%S cell-verdict=%S\n"
                   requested-tool-names
                   (and terminal
                        (list (plist-get terminal :type)
                              (plist-get (plist-get terminal :payload)
                                         :status)))
                   runtime-status cell-verdict-passed))
    (superchat-recovery-trace-audit--assert
     "two provider requests" (= 2 (length requests)))
    (superchat-recovery-trace-audit--assert
     "two prepared snapshots" (= 2 (length snapshots)))
    (superchat-recovery-trace-audit--assert
     "llm callback count" (= 6 (length logs)))
    (superchat-recovery-trace-audit--assert
     "tape event count" (= 18 (length events)))
    (superchat-recovery-trace-audit--assert
     "tool order" (equal requested-tool-names
                          '("workspace_read" "workspace_info")))
    (superchat-recovery-trace-audit--assert
     "llm agrees with Tape"
     (equal received-tool-names
            '(("workspace_read") ("workspace_info"))))
    (superchat-recovery-trace-audit--assert
     "second HTTP body carries history"
     (equal (plist-get (plist-get (car second-tool-calls) :function) :name)
            "workspace_read"))
    (superchat-recovery-trace-audit--assert
     "typed revision failure"
     (and (= 1 (length tool-failures))
          (equal "superchat-workspace-artifact-revision-error"
                 (plist-get (plist-get (car tool-failures) :payload)
                            :error-type))))
    (superchat-recovery-trace-audit--assert
     "intervention causation"
     (and (= 1 (length interventions))
          (equal (plist-get (car tool-failures) :event-id)
                 (plist-get
                  (plist-get (plist-get (car interventions) :payload)
                             :trigger)
                  :event-id))))
    (superchat-recovery-trace-audit--assert
     "workspace info returns unresolved artifact"
     (and (stringp info-result) (string-match-p "live=nil" info-result)))
    (superchat-recovery-trace-audit--assert
     "terminal is failed"
     (and terminal
          (equal "run.failed" (plist-get terminal :type))
          (equal "failed" (plist-get (plist-get terminal :payload) :status))))
    (superchat-recovery-trace-audit--assert
     "cell verdict is separate from terminal"
     (and cell-verdict-passed
          (equal runtime-status "failed")))
    (superchat-recovery-trace-audit--assert
     "Runtime terminal is authoritative"
     (equal runtime-status
            (or (plist-get (plist-get trace :recovery-verdict)
                           :runtime-status)
                runtime-status)))
    (list :requests (length requests) :llm-events (length logs)
          :tape-events (length events) :runtime-status runtime-status
          :cell-verdict-passed cell-verdict-passed
          :run-completed (equal runtime-status "completed"))))

(when noninteractive
  (superchat-recovery-trace-audit
   (or (getenv "SUPERCHAT_RECOVERY_TRACE_INPUT")
       (expand-file-name "trace-p-missing.el"
                         (file-name-directory (or load-file-name
                                                   buffer-file-name))))))

(provide 'superchat-experience-view-recovery-trace-audit)

;;; audit.el ends here
