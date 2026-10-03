;;; ask-once-regression.el --- ask Emacs for several facts in one call -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 6.6 of M11
;; (docs/research/round-trips-first-principles.md): most round trips in real
;; runs were the model asking Emacs one question at a time, so a request that
;; offers eval_elisp says to ask for several facts in one call that returns a
;; list.  A request without eval_elisp (an in-place question) does not say it.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/ask-once-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^ask-once/")'

;;; Code:

(require 'ert)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-inline)

(ert-deftest ask-once/only-where-eval-elisp-is-offered ()
  (hm--fixture '((:text "ok") (:text "ok"))
    (hm--turn (smoke--open-chat) "hello")
    (should (string-match-p (regexp-quote superchat-prompt-ask-once-text)
                            (hm--text (car (hm--wire 1)))))
    (let ((source (get-buffer-create "ask-once-source")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (insert "abc\n")
              (superchat-inline-ask "what is this" (cons (point-min) (point-max))))
            (should (smoke--wait (lambda () (= 2 (length hm--wires))) 10))
            (should-not (string-match-p "Each tool call costs a round trip"
                                        (hm--text (car (hm--wire 2))))))
        (kill-buffer source)))))

(provide 'ask-once-regression)
;;; ask-once-regression.el ends here
