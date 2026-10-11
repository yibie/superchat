;;; command-load-regression.el --- Commands exist after (require 'superchat) -*- lexical-binding: t; -*-

;;; Commentary:

;; A command that lives in a file nothing requires must still be there for
;; someone who runs Superchat from a checkout: `-L' the directory and
;; `(require 'superchat)', with no generated autoloads file.  This file loads
;; nothing but `superchat', so run it in an Emacs of its own.
;;
;;   emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/command-load-regression.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'superchat)

(ert-deftest command-load/the-commands-of-unrequired-files-are-there ()
  (dolist (command '(superchat-pending superchat-sessions superchat-session-history))
    (should (commandp command))))

(ert-deftest command-load/calling-one-loads-its-file ()
  (should (commandp 'superchat-pending))
  (autoload-do-load (symbol-function 'superchat-pending) 'superchat-pending)
  (should (featurep 'superchat-pending))
  (should-not (autoloadp (symbol-function 'superchat-pending))))

(provide 'command-load-regression)
;;; command-load-regression.el ends here
