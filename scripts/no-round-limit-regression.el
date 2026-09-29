;;; no-round-limit-regression.el --- No tool round limit by default -*- lexical-binding: t; -*-

;;; Commentary:
;; Contract: docs/decisions/accepted/2026-09-29-no-tool-round-limit-by-default.md
;; Reuses the tool-budget fixture of the M5 execution regression (its tests
;; are defined by the load and are not selected here).
;;
;;   emacs -Q --batch ... -l scripts/no-round-limit-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^no-round-limit/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'cus-edit)
(require 'wid-edit)
(load (expand-file-name "m5-execution-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun nrl--standard-value ()
  (eval (car (get 'superchat-llm-max-tool-rounds 'standard-value)) t))

(defun nrl--type-accepts-p (value)
  (let ((widget (widget-convert (custom-variable-type 'superchat-llm-max-tool-rounds))))
    (and (widget-apply widget :match value) t)))

(ert-deftest no-round-limit/the-default-is-no-limit ()
  (should-not (nrl--standard-value))
  (should (nrl--type-accepts-p nil))
  (should (nrl--type-accepts-p 12)))

(ert-deftest no-round-limit/thirty-rounds-reach-the-answer-unasked ()
  "With the default, a long tool conversation is neither asked about nor cut."
  (m5b-with-fixture
      (append (cl-loop repeat 30 collect (m5b-read-step))
              '((:text "NRL-DONE after thirty rounds.")))
    (let ((superchat-llm-max-tool-rounds (nrl--standard-value))
          (questions nil))
      (let ((view (smoke--open-chat))
            (noninteractive nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (question) (push question questions) nil)))
          (let ((run (m5b--settle (m5b--submit view))))
            (should-not questions)
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
            (should (= 31 (length smoke--requests)))
            (should (= 30 (length (m5b--events run "tool.completed"))))
            ;; Every request offered the tool: no forced tool-free final one.
            (dolist (request (m5b--requests))
              (should (equal '("read_buffer") (plist-get request :tools))))
            (should-not (plist-get (superchat-runtime-status run) :tool-budget))))))))

(provide 'no-round-limit-regression)

;;; no-round-limit-regression.el ends here
