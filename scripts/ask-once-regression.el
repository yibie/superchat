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

(ert-deftest ask-once/case-only-renames-are-explained-where-eval-elisp-is-offered ()
  "M11 slice 6.7: in A2 the model took a case-only rename for an overwrite."
  (should (string-match-p "differ only in case are the same file"
                          superchat-prompt-case-only-names-text))
  (should (string-match-p "(file-name-case-insensitive-p" superchat-prompt-case-only-names-text))
  (should (equal '("eval_elisp")
                 (superchat-prompt--block-tools #'superchat-prompt-hook--case-only-names)))
  (hm--fixture '((:text "ok") (:text "ok"))
    (hm--turn (smoke--open-chat) "hello")
    (should (string-match-p (regexp-quote superchat-prompt-case-only-names-text)
                            (hm--text (car (hm--wire 1)))))
    (let ((source (get-buffer-create "ask-once-source")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (insert "abc\n")
              (superchat-inline-ask "what is this" (cons (point-min) (point-max))))
            (should (smoke--wait (lambda () (= 2 (length hm--wires))) 10))
            (should-not (string-match-p "differ only in case"
                                        (hm--text (car (hm--wire 2))))))
        (kill-buffer source)))))

(ert-deftest ask-once/a-change-on-disk-the-user-asked-for-is-theirs ()
  "M11 slice 6.8: in A2 the model stopped before a rename the user asked for."
  (should (string-match-p "^Saving a buffer you edited is the user's decision\\."
                          superchat-prompt-asked-disk-changes-text))
  (should (string-match-p "is already the user's decision: do it" superchat-prompt-asked-disk-changes-text))
  (should (string-match-p "permission level decides" superchat-prompt-asked-disk-changes-text))
  (should (equal '("eval_elisp")
                 (superchat-prompt--block-tools #'superchat-prompt-hook--asked-disk-changes)))
  (hm--fixture '((:text "ok") (:text "ok"))
    (hm--turn (smoke--open-chat) "hello")
    (should (string-match-p (regexp-quote superchat-prompt-asked-disk-changes-text)
                            (hm--text (car (hm--wire 1)))))
    (let ((source (get-buffer-create "ask-once-source")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (insert "abc\n")
              (superchat-inline-ask "what is this" (cons (point-min) (point-max))))
            (should (smoke--wait (lambda () (= 2 (length hm--wires))) 10))
            (should-not (string-match-p "already the user's decision"
                                        (hm--text (car (hm--wire 2))))))
        (kill-buffer source)))))

(ert-deftest ask-once/rename-file-does-a-case-only-rename-here ()
  "The fact the line states, checked on the file system the tests run on."
  (let ((dir (file-name-as-directory (make-temp-file "ask-once-case-" t))))
    (unwind-protect
        (if (not (file-name-case-insensitive-p dir))
            (ert-skip "This file system tells case apart")
          (with-temp-file (expand-file-name "A.JPG" dir) (insert "x"))
          (should (file-exists-p (expand-file-name "A.jpg" dir)))
          (rename-file (expand-file-name "A.JPG" dir) (expand-file-name "A.jpg" dir))
          (should (equal '("A.jpg") (directory-files dir nil "\\`[^.]"))))
      (delete-directory dir t))))

(provide 'ask-once-regression)
;;; ask-once-regression.el ends here
