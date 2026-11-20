;;; Directory Local Variables -*- no-byte-compile: t -*-
;; After Superchat's `edit' or `write' saves a file here, the gates run; any
;; line but `ok' and the total is a problem, and joins the tool's result
;; (superchat-lint.el).
((nil . ((superchat-lint-project-command
          . "emacs -Q --batch -l scripts/gates.el . 2>&1 | grep -vE '^(ok|total) '; test $? -ne 0"))))
