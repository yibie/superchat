;;; superchat-bridge-utils-provider-env.el --- Provider environment values  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/provider-env.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; A `ProviderEnv' is an alist of (NAME . VALUE) strings.  The source
;; falls back to the Bun sandbox's `/proc/self/environ' when
;; `process.env' is empty; Emacs has no such case, so that fallback is
;; left out.

;;; Code:

(defun superchat-bridge-get-provider-env-value (name &optional env)
  "Return the value of NAME, or nil: ENV first, then the process environment.
ENV is a `ProviderEnv' alist.  An empty value counts as unset, as the
source's `||' makes it."
  (let ((scoped (cdr (assoc name env)))
        (process (getenv name)))
    (cond ((and scoped (not (string-empty-p scoped))) scoped)
          ((and process (not (string-empty-p process))) process)
          (t nil))))

(provide 'superchat-bridge-utils-provider-env)

;;; superchat-bridge-utils-provider-env.el ends here
