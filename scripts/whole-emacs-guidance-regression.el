;;; whole-emacs-guidance-regression.el --- the note: the whole Emacs, and another way -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-10-01-guidance-from-the-goals-batch.md:
;; the operating note says the origin buffer is not the task's limit and that
;; the user's own data is asked of the running Emacs before the model says it
;; cannot reach it.  (A second sentence, "try another way before stopping", was
;; tried the same day and withdrawn: no gain on the rename task, and window
;; tasks got worse.)  What is pinned is the text that reaches the provider,
;; not that a model does better for it.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/whole-emacs-guidance-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^whole-emacs-guidance/")'

;;; Code:

(require 'ert)
(require 'superchat-prompt-hooks)

(ert-deftest whole-emacs-guidance/the-origin-buffer-is-not-the-limit-of-the-task ()
  (let ((text superchat-emacs-operating-note-text))
    (should (string-match-p "not the task's limit" text))
    (should (string-match-p "agenda" text))
    (should (string-match-p "before saying you cannot reach" text))
    ;; The 2026-09-27 sentence stands as it was.
    (should (string-match-p "to get past an obstacle; say what blocks you" text))))

(ert-deftest whole-emacs-guidance/the-note-keeps-to-its-raised-cap ()
  (let ((text superchat-emacs-operating-note-text))
    (should (> (length text) 1024))
    (should (<= (length text) 1280))
    ;; Still never an instruction to persist.
    (should-not (string-match-p "save-buffer\\|write-file" text))))

(provide 'whole-emacs-guidance-regression)

;;; whole-emacs-guidance-regression.el ends here
