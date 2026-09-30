;;; task-sim-guidance-regression.el --- guidance from the task simulation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for
;; docs/decisions/accepted/2026-09-27-guidance-from-the-task-simulation.md:
;; the key-binding page says local-set-key edits the mode's map; the
;; operating note says not to change what is outside the task.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/task-sim-guidance-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^task-sim-guidance/")'

;;; Code:

(require 'ert)
(require 'superchat-prompt-hooks)
(require 'superchat-reference)

(ert-deftest task-sim-guidance/the-key-page-says-local-set-key-edits-the-mode-map ()
  (let ((page (superchat-reference-body "emacs-keybindings")))
    (should (stringp page))
    (should (<= (length page) 4096))
    (should (string-match-p "local-set-key" page))
    (should (string-match-p "that mode's map itself" page))
    (should (string-match-p "every buffer of the mode" page))
    (should (string-match-p "remove-hook" page))))

(ert-deftest task-sim-guidance/the-note-says-stay-inside-the-task ()
  (let ((text superchat-emacs-operating-note-text))
    ;; The cap was 1024 until 2026-10-01 (guidance from the goals batch).
    (should (<= (length text) 1280))
    (should (string-match-p "global settings" text))
    (should (string-match-p "other buffers" text))
    (should (string-match-p "Superchat's windows" text))
    (should (string-match-p "get past an obstacle" text))
    (should (string-match-p "say what blocks you" text))))

(provide 'task-sim-guidance-regression)

;;; task-sim-guidance-regression.el ends here
