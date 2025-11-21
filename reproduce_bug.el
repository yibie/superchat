(require 'json)
(require 'cl-lib)

;; Mock gptel-request
(defun gptel-request (prompt &rest args)
  (message "gptel-request called"))

(defvar superchat-memory-session-summarizer-llm-prompt "Prompt")

(defun superchat-memory-summarize-session-history (history-content)
  "Use an LLM to summarize an entire session HISTORY-CONTENT and capture it."
  (when (and t ;; (featurep 'gptel)
             t ;; (fboundp 'gptel-request)
             (stringp history-content) (> (length history-content) 10)) ; Add a minimum length check
    (let* ((prompt (replace-regexp-in-string "\\$content" history-content superchat-memory-session-summarizer-llm-prompt nil t))
           (handler (lambda (response &rest _ignore)
                      (message "Handler called"))))
      (gptel-request prompt :callback handler)))))
