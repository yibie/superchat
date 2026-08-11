;;; generalization.el --- Thin no-leakage learning transfer check -*- lexical-binding: t; -*-

;; This reuses the closed loop once with a new opaque code and new local tool
;; names.  It is intentionally an experiment adapter, not product behavior.

;;; Code:

(require 'cl-lib)

(load (expand-file-name "closed-loop.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defconst superchat-learning-minimal-generalization--scenario
  '(:name "generalization-v1"
    :code "Q-K42"
    :scope "opaque-recovery/generalization-v1"
    :read-tool "inspect_token"
    :target-tool "refresh_marker"
    :wrong-tool "defer_unknown"
    :temperature 0
    ;; Ollama's seed is not part of the evidence contract's supported
    ;; request options; keep it explicit as unavailable rather than hiding it.
    :seed nil
    :max-tool-rounds 4))

(when (equal (getenv "SUPERCHAT_EXPERIENCE_GENERALIZATION_EXECUTE") "1")
  (condition-case err
      (let ((superchat-learning-minimal--scenario
             superchat-learning-minimal-generalization--scenario))
        (setenv "SUPERCHAT_EXPERIENCE_CLOSED_LOOP_OUTPUT"
                (getenv "SUPERCHAT_EXPERIENCE_GENERALIZATION_OUTPUT"))
        (superchat-learning-minimal-closed--run)
        (kill-emacs 0))
    (error
     (princ (format "generalization infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-learning-generalization)

;;; generalization.el ends here
