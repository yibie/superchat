(add-to-list 'load-path "/Users/chenyibin/Documents/emacs/package/superchat/")
(require 'superchat-memory)

(setq superchat-memory-file "/Users/chenyibin/Documents/emacs/package/superchat/test/test-memory-2.org")
(setq superchat-memory-merge-similarity-threshold 0.1)

;; Temporarily disable gptel to force local Jaccard fallback
(fset 'gptel-request nil)

(message "Running evaluation with test-memory-2.org...")

(let ((groups (superchat-memory--find-merge-candidates)))
  (if (not groups)
      (princ "Result: No groups found.\n")
    (princ (format "Result: Found %d groups.\n" (length groups)))))