;;; verify-runner.el --- No-chat gate for the frozen acceptance runner -*- lexical-binding: t; -*-

;;; Commentary:
;; This gate runs before provisioning or any provider request.  It checks the
;; executable runner as the user will load it: source invariants, byte
;; compilation, pure loading, and the runner/contract identity.  It is a
;; process guard, not another acceptance verifier.

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'subr-x)

(defconst superchat-acceptance-v26-gate--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst superchat-acceptance-v26-gate--runner
  (expand-file-name "run.el" superchat-acceptance-v26-gate--directory))
(defconst superchat-acceptance-v26-gate--trace
  (expand-file-name "../experience-view-recovery-trace/trace.el"
                    superchat-acceptance-v26-gate--directory))
(defconst superchat-acceptance-v26-gate--durable-trace
  (expand-file-name "../experience-view-recovery-trace/durable-trace.el"
                    superchat-acceptance-v26-gate--directory))
(defconst superchat-acceptance-v26-gate--contract
  (expand-file-name "../../docs/design/experience-view-internal-acceptance-contract-v2.6.md"
                    superchat-acceptance-v26-gate--directory))
(defconst superchat-acceptance-v26-gate--preflight
  (expand-file-name "preflight.el" superchat-acceptance-v26-gate--directory))
(defconst superchat-acceptance-v26-gate--self
  (expand-file-name "verify-runner.el" superchat-acceptance-v26-gate--directory))

(defun superchat-acceptance-v26-gate--read (path)
  "Read PATH as UTF-8 text without changing its bytes on disk."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (decode-coding-string (buffer-string) 'utf-8)))

(defun superchat-acceptance-v26-gate--sha256-file (path)
  "Return the exact byte SHA-256 of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun superchat-acceptance-v26-gate--assert (condition message)
  "Signal with MESSAGE unless CONDITION is non-nil."
  (unless condition
    (error "Runner gate: %s" message)))

(defun superchat-acceptance-v26-gate--compile (path)
  "Byte-compile PATH and reject critical lexical/data warnings."
  (let ((source (make-temp-file "superchat-runner-source-" nil ".el"))
        (warnings nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert-file-contents-literally path))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (type message &rest _args)
                       (push (format "%s: %s" type message) warnings))))
            (superchat-acceptance-v26-gate--assert
             (byte-compile-file source)
             (format "byte compilation failed for %s"
                     (file-name-nondirectory path))))
          (dolist (warning warnings)
            (when (string-match-p
                   "Unused lexical variable\\|reference to free variable\\|called as a function"
                   warning)
              (error "Runner gate: %s: %s"
                     (file-name-nondirectory path) warning))))
      (when (file-exists-p source)
        (delete-file source))
      (let ((destination (concat (file-name-sans-extension source) ".elc")))
        (when (file-exists-p destination)
          (delete-file destination))))))

(defun superchat-acceptance-v26-verify-runner ()
  "Run the no-chat gate for the v2.6 runner and return non-nil on success."
  (let ((process-environment (copy-sequence process-environment)))
    ;; Clear execute switches before loading any runner code.  The canonical
    ;; hash implementation is then reused from the runner itself; there is no
    ;; second copy here that can drift.
    (dolist (name '("SUPERCHAT_ACCEPTANCE_V26_EXECUTE"
                    "SUPERCHAT_DURABLE_TRACE_EXECUTE"
                    "SUPERCHAT_RECOVERY_TRACE_EXECUTE"))
      (setenv name nil))
    (load superchat-acceptance-v26-gate--runner nil nil)
    (let* ((runner-text (superchat-acceptance-v26-gate--read
                         superchat-acceptance-v26-gate--runner))
         (runner-sha (superchat-acceptance-v26-gate--sha256-file
                      superchat-acceptance-v26-gate--runner))
         (declared-runner-sha
          (superchat-acceptance-v26--contract-field "Final runner SHA-256"))
         (declared-contract-sha
          (superchat-acceptance-v26--contract-self-digest))
         (actual-contract-sha
          (superchat-acceptance-v26--contract-sha256))
         (preflight-sha
          (superchat-acceptance-v26-gate--sha256-file
           superchat-acceptance-v26-gate--preflight))
         (declared-preflight-sha
          (superchat-acceptance-v26--contract-field
           "Final preflight SHA-256"))
         (gate-sha
          (superchat-acceptance-v26-gate--sha256-file
           superchat-acceptance-v26-gate--self))
         (declared-gate-sha
          (superchat-acceptance-v26--contract-field
           "Process gate SHA-256")))
    (dolist (path (list superchat-acceptance-v26-gate--runner
                        superchat-acceptance-v26-gate--trace
                        superchat-acceptance-v26-gate--durable-trace
                        superchat-acceptance-v26-gate--contract))
      (superchat-acceptance-v26-gate--assert
       (file-readable-p path)
       (format "required file is unreadable: %s"
               (file-name-nondirectory path))))
    (superchat-acceptance-v26-gate--assert
     (equal runner-sha declared-runner-sha)
     (format "runner SHA mismatch: declared %s, actual %s"
             declared-runner-sha runner-sha))
    (superchat-acceptance-v26-gate--assert
     (equal (file-truename superchat-acceptance-v26--contract-path)
            (file-truename superchat-acceptance-v26-gate--contract))
     "runner and gate point at different contracts")
    (superchat-acceptance-v26-gate--assert
     (equal declared-contract-sha actual-contract-sha)
     "contract self-digest mismatch")
    (superchat-acceptance-v26-gate--assert
     (equal preflight-sha declared-preflight-sha)
     (format "preflight SHA mismatch: declared %s, actual %s"
             declared-preflight-sha preflight-sha))
    (superchat-acceptance-v26-gate--assert
     (equal gate-sha declared-gate-sha)
     (format "process gate SHA mismatch: declared %s, actual %s"
             declared-gate-sha gate-sha))
    (dolist (bad '(":status empty" ":receipt (:tape-head 0)"
                   ":disclosure (:represented [])"))
      (superchat-acceptance-v26-gate--assert
       (not (string-match-p (regexp-quote bad) runner-text))
       (format "unquoted data literal remains: %s" bad)))
    (dolist (name '("superchat-data-directory"
                    "superchat-runtime-recovery-intervention-enabled"
                    "superchat-llm-max-tool-rounds"
                    "superchat-llm-round-limit-action"
                    "superchat-llm-backend"))
      (superchat-acceptance-v26-gate--assert
       (string-match-p (format "(defvar %s)" name) runner-text)
       (format "dynamic binding declaration is missing: %s" name)))
    (dolist (path (list superchat-acceptance-v26-gate--runner
                        superchat-acceptance-v26-gate--trace
                        superchat-acceptance-v26-gate--durable-trace))
      (superchat-acceptance-v26-gate--compile path))
    ;; Pure loading is deliberately done with every execute switch cleared.
    ;; The gate must never provision a DB or reach the provider.
    (load superchat-acceptance-v26-gate--durable-trace nil nil)
      (princ (format "RUNNER_GATE_OK runner-sha=%s contract-sha=%s\n"
                     runner-sha declared-contract-sha))
      t)))

(provide 'superchat-experience-acceptance-v26-runner-gate)

;;; verify-runner.el ends here
