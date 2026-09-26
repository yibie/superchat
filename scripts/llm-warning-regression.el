;;; llm-warning-regression.el --- superchat's switch for llm's nonfree warning -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Offline coverage for the decision "llm's not-free-software warning"
;; (docs/decisions/accepted/2026-09-25-llm-nonfree-warning.md): with
;; `superchat-llm-warning-message' nil, superchat's own chat requests are made
;; with llm's `llm-warn-on-nonfree' off -- llm reads it when the request is
;; made -- and llm's own setting is left as it was.  The scripted fixture
;; stands in for llm's request functions, so the check is the value llm would
;; read at that moment.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L /path/to/llm -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/llm-warning-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^llm-warning/")'

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defvar superchat-llm-warning-message)

(defun llm-warning--seen (option)
  "Send one chat turn with OPTION; return `llm-warn-on-nonfree' as llm saw it."
  (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
        (superchat-runtime--queue-timers (make-hash-table :test #'equal))
        (llm-warn-on-nonfree t)
        (superchat-llm-warning-message option)
        (seen :unset))
    (smoke--with-fixture (list '(:text "answer"))
      (let ((network (symbol-function 'smoke--fake-network)))
        (cl-letf (((symbol-function 'smoke--fake-network)
                   (lambda (&rest args)
                     (setq seen llm-warn-on-nonfree)
                     (apply network args))))
          (let* ((view (smoke--open-chat))
                 (run (smoke--send view "hello")))
            (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
            (should (eq t llm-warn-on-nonfree))))))
    seen))

(ert-deftest llm-warning/off-for-superchat-when-the-option-is-nil ()
  "With the option nil, llm sees its warning switched off for superchat's
request, and llm's own setting is unchanged afterwards."
  (should (null (llm-warning--seen nil))))

(ert-deftest llm-warning/llm-decides-by-default ()
  "Guard: by default llm's own setting is what llm sees."
  (should (eq t (default-value 'superchat-llm-warning-message)))
  (should (eq t (llm-warning--seen t))))

(provide 'llm-warning-regression)

;;; llm-warning-regression.el ends here
