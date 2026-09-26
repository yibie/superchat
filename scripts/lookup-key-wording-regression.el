;;; lookup-key-wording-regression.el --- one reading of lookup-key's number -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the 2026-09-25 correction of the `emacs-keybindings'
;; reference page (docs/decisions/accepted/2026-09-20-reference-reading-on-demand.md,
;; "之后的修改（2026-09-25）").  A number from `lookup-key' means the map has
;; no binding for the whole key: its first N keys are not a prefix there,
;; unbound or bound to a command (Emacs's own `lookup-key' docstring and the
;; Elisp manual, "Low-Level Key Binding").  The page said the first N keys
;; were bound to a command; the operating note, corrected on 2026-09-24, says
;; the map has no binding.  A model reading both got two answers.
;;
;; Only the two texts are checked; no request is made.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/lookup-key-wording-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^lookup-key-wording/")'

;;; Code:

(require 'ert)
(require 'superchat-reference)
(require 'superchat-prompt-hooks)

(defconst lookup-key-wording--shared
  '("that map has no binding for it"
    "key-binding in the target buffer is the effective binding")
  "What the page and the operating note both say about a key's binding.")

(defun lookup-key-wording--page ()
  (superchat-reference-body "emacs-keybindings"))

(ert-deftest lookup-key-wording/the-page-says-what-the-note-says ()
  "The reference page and the operating note read lookup-key's number the
same way, in the same words."
  (let ((case-fold-search nil))
    (dolist (phrase lookup-key-wording--shared)
      (should (string-match-p (regexp-quote phrase) superchat-emacs-operating-note-text))
      (should (string-match-p (regexp-quote phrase) (lookup-key-wording--page))))))

(ert-deftest lookup-key-wording/the-page-drops-the-bound-prefix-reading ()
  "The page no longer says the first N keys are bound to a command, nor that
the longer key cannot be bound in that map."
  (let ((page (lookup-key-wording--page))
        (case-fold-search t))
    (should-not (string-match-p "first N keys are already bound" page))
    (should-not (string-match-p "cannot be bound in that map" page))))

(ert-deftest lookup-key-wording/the-page-keeps-both-cases ()
  "The first N keys may be unbound or bound to a command; only the second
makes `define-key' fail, with the error the page is the pointer target for."
  (let ((page (lookup-key-wording--page))
        (case-fold-search nil))
    (should (string-match-p (regexp-quote "either unbound or bound to a command") page))
    (should (string-match-p (regexp-quote "starts with non-prefix key") page))
    (should (equal "emacs-keybindings"
                   (superchat-reference-pointer-for
                    "eval_elisp"
                    "Key sequence C-c t h starts with non-prefix key C-c t")))))

(provide 'lookup-key-wording-regression)

;;; lookup-key-wording-regression.el ends here
