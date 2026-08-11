;;; trace.el --- One-cell recovery trace collector -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a diagnostic runner, not an acceptance contract.  It reuses the
;; existing v2.6 setup and Runtime path, but records every useful boundary in
;; one local Lisp-readable file: provider-visible request bodies, llm.el log
;; callbacks, durable Tape events, and the final Run status.  It deliberately
;; runs one P-missing cell only and never changes the product default.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'subr-x)

(defconst superchat-recovery-trace--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst superchat-recovery-trace--v26-runner
  (expand-file-name "../experience-view-internal-acceptance-v2.6/run.el"
                    superchat-recovery-trace--directory))

;; Reuse setup and execution seams from the already-frozen runner.  Loading
;; the file does not execute its once-only assignment unless its execute env
;; var is set, which this diagnostic runner never sets.
(load superchat-recovery-trace--v26-runner nil nil)

(defvar superchat-recovery-trace--llm-log nil)
(defvar superchat-recovery-trace--transport nil)

(defun superchat-recovery-trace--prompt-text (prompt)
  "Return readable provider PROMPT text without retaining the struct." 
  (cond
   ((and (fboundp 'llm-chat-prompt-to-text)
         (ignore-errors (llm-chat-prompt-p prompt)))
    (ignore-errors (llm-chat-prompt-to-text prompt)))
   ((stringp prompt) prompt)
   (t nil)))

(defun superchat-recovery-trace--headers (headers)
  "Return HEADERS with credentials redacted." 
  (mapcar
   (lambda (header)
     (cons (car header)
           (if (and (stringp (car header))
                    (string-match-p "\\`authorization\\'"
                                    (downcase (car header))))
               "<redacted>"
             (cdr header))))
   (cl-remove-if-not #'consp headers)))

(defun superchat-recovery-trace--llm-log-advice (original type &rest args)
  "Capture llm.el TYPE/ARGS, then call ORIGINAL." 
  (push (list :at (float-time)
              :type type
              :provider (ignore-errors
                          (format "%S" (plist-get args :provider)))
              :prompt (superchat-recovery-trace--prompt-text
                       (plist-get args :prompt))
              :message (format "%S" (plist-get args :msg)))
        superchat-recovery-trace--llm-log)
  (apply original type args))

(defun superchat-recovery-trace--transport-advice
    (original method url &rest args)
  "Capture one provider request before calling ORIGINAL." 
  (when (and (eq method 'post)
             (string-suffix-p "/api/chat" url))
    (push (list :at (float-time)
                :method method
                :url url
                :body (plist-get args :body)
                :headers
                (superchat-recovery-trace--headers
                 (plist-get args :headers))
                :connect-timeout (plist-get args :connect-timeout)
                :timeout (plist-get args :timeout))
          superchat-recovery-trace--transport))
  (apply original method url args))

(defun superchat-recovery-trace--dependency-records ()
  "Return loaded package versions and paths without private scratch roots." 
  (mapcar
   (lambda (name)
     (let* ((description (cadr (assq name package-alist)))
            (path (and (package-desc-p description)
                       (package-desc-dir description))))
       (list :feature name
             :version (and (package-desc-p description)
                           (package-version-join
                            (package-desc-version description)))
             :role (and path
                        (file-name-nondirectory (directory-file-name path))))))
   '(compat plz plz-event-source plz-media-type)))

(defun superchat-recovery-trace--write (path value)
  "Write VALUE to PATH as readable Lisp." 
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (let ((print-length nil)
          (print-level nil))
      (prin1 value (current-buffer))
      (insert "\n"))))

(defun superchat-recovery-trace--read (path)
  "Read one Lisp value from PATH, or nil when PATH is absent." 
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (read (current-buffer)))))

(defun superchat-recovery-trace--run ()
  "Run one P-missing recovery diagnostic and write its complete trace." 
  (let* ((output (getenv "SUPERCHAT_RECOVERY_TRACE_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (cell-directory (and root (expand-file-name "P-missing" root)))
         (cell '(:name "P-missing" :positive t :kind missing :stale nil))
         setup result failure)
    (unless (and output root)
      (error "SUPERCHAT_RECOVERY_TRACE_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite existing trace: %s" output))
    (make-directory cell-directory t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'llm)
    (require 'llm-ollama)
    (let ((superchat-data-directory cell-directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (setq setup (superchat-acceptance-v26--setup-experience cell))
        (superchat-db-close)))
    (setenv "SUPERCHAT_ACCEPTANCE_V26_EVIDENCE_ROOT" root)
    (advice-add 'llm--log :around #'superchat-recovery-trace--llm-log-advice)
    (advice-add 'plz-media-type-request :around
                #'superchat-recovery-trace--transport-advice)
    (unwind-protect
        (condition-case err
            (setq result (superchat-acceptance-v26--run-cell setup))
          (error (setq failure (list :error (error-message-string err)))))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (let* ((result-record
            (superchat-recovery-trace--read
             (expand-file-name "result.el" cell-directory)))
           (events (plist-get result-record :events))
           (terminal (car (last
                           (cl-remove-if-not
                            (lambda (event)
                              (member (plist-get event :type)
                                      '("run.completed" "run.failed"
                                        "run.cancelled")))
                            events))))
           (status (and terminal
                         (plist-get (plist-get terminal :payload) :status)))
           (trace
            (list
             :format "superchat-experience-view-recovery-trace-v0"
             :cell cell
             :source
             (list :repo-root-role "<repo-root>"
                   :jj (superchat-acceptance-v26--jj-identity
                        (superchat-acceptance-v26--repo-root))
                   :emacs emacs-version
                   :llm-commit superchat-acceptance-v26--llm-commit
                   :model superchat-acceptance-v26--model
                   :dependencies (superchat-recovery-trace--dependency-records))
             :result result-record
             :summary result
             :failure failure
             :runtime-status status
             :transport-requests (nreverse superchat-recovery-trace--transport)
             :llm-log (nreverse superchat-recovery-trace--llm-log)
             :tape-events events
             :notes
             '(:transport-body "Exact JSON request body captured in :transport-requests."
               :llm-response "Decoded provider partial/complete/error callbacks captured in :llm-log."
               :tape "Durable Runtime, Authority, Experience, and terminal events captured in :tape-events."
               :redaction "Authorization headers are redacted; no remote endpoint is used."))))
      (superchat-recovery-trace--write output trace)
      (kill-emacs (if (and result
                           (plist-get (plist-get result :verdict) :passed))
                      0
                    1)))))

(when (getenv "SUPERCHAT_RECOVERY_TRACE_EXECUTE")
  (superchat-recovery-trace--run))

(provide 'superchat-experience-view-recovery-trace)

;;; trace.el ends here
