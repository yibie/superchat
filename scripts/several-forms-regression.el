;;; several-forms-regression.el --- several list forms in one eval_elisp call run in order -*- lexical-binding: t; -*-

;;; Commentary:
;; Decision of 2026-11-09
;; (docs/decisions/accepted/2026-11-09-several-forms-in-one-eval-run-in-order.md):
;; the text of an `eval_elisp' call is read completely before anything is
;; evaluated.  One form runs as before.  Several top-level forms that are
;; all lists run in order as one `progn', and the result is the last one's
;; value.  Several forms of which one is not a list -- prose after a form,
;; as in "(foo) done" -- evaluate nothing and fail, naming the first such
;; form.  A text that does not read, anywhere, evaluates nothing.
;;
;; On 2026-11-09 six calls of one conversation failed with "Expected exactly
;; one expression; wrap multiple forms in progn", each a round trip and a
;; step towards the brake that stops an execution after three failed calls.
;;
;; Every test runs the real tool loop with a scripted model.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/several-forms-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^several-forms/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar sf--order nil "What the forms of one expression did, newest first.")

(defmacro sf--fixture (mode script &rest body)
  "Run BODY in the smoke fixture with SCRIPT, under the permission MODE."
  (declare (indent 2) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-approval-default-policy 'ask)
           (superchat-sandbox-default-mode ,mode)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (sf--order nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun sf--eval (expression)
  "A model round that calls `eval_elisp' with EXPRESSION."
  `(:tool-uses (("eval_elisp" (expression . ,expression)))))

(defun sf--run (view text)
  "Send TEXT from VIEW and wait for its Run to end; return the Run."
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun sf--outcome (view)
  "The one `tool.completed' or `tool.failed' event of `eval_elisp' in VIEW's Session."
  (let ((events (cl-remove-if-not
                 (lambda (event)
                   (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
                        (equal "eval_elisp" (plist-get (plist-get event :payload) :tool-name))))
                 (superchat-tape-replay-events
                  (buffer-local-value 'superchat--session-id view)))))
    (should (= 1 (length events)))
    (car events)))

(defun sf--result (view)
  "The type of VIEW's one `eval_elisp' outcome and the text the model received."
  (let ((outcome (sf--outcome view)))
    (list (plist-get outcome :type)
          (plist-get (plist-get outcome :payload) :result-text))))

(ert-deftest several-forms/one-form-is-as-before ()
  "Guard: one form, one value."
  (sf--fixture 'danger-full-access
      (list (sf--eval "(progn (push 'one sf--order) (+ 1 2))") '(:text "SF-DONE"))
    (let ((view (smoke--open-chat)))
      (sf--run view "compute")
      (should (equal '("tool.completed" "Result: 3") (sf--result view)))
      (should (equal '(one) sf--order)))))

(ert-deftest several-forms/several-lists-run-in-order-and-the-last-value-is-the-result ()
  "The six failures of 2026-11-09 were texts like these: lists one after
the other, with comments between them."
  (sf--fixture 'danger-full-access
      (list (sf--eval "(push 'first sf--order) ; the first\n(push 'second sf--order)\n;; then\n(reverse sf--order)\n")
            '(:text "SF-DONE"))
    (let ((view (smoke--open-chat)))
      (sf--run view "compute")
      (should (equal '("tool.completed" "Result: (first second)") (sf--result view)))
      (should (equal '(second first) sf--order)))))

(ert-deftest several-forms/the-question-and-the-tape-show-the-text-as-sent ()
  "The approval question carries the expression as the model sent it, and so
does the Tape's `tool.requested' fact; the progn is never shown."
  (let ((text "(push 'a sf--order)\n(push 'b sf--order) ; two forms"))
    (sf--fixture 'edit-buffers (list (sf--eval text) '(:text "SF-DONE"))
      (let* ((asked nil)
             (superchat-approval-function (lambda (request) (push request asked) t))
             (view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view)))
        (sf--run view "compute")
        (should (= 1 (length asked)))
        (should (equal "eval_elisp" (plist-get (car asked) :tool-name)))
        (should (equal (list text) (plist-get (car asked) :args)))
        ;; What `?' on the question shows: the arguments, printed.
        (should (string-search (pp-to-string (list text))
                               (superchat-approval--details (car asked))))
        (should-not (string-search "progn" (superchat-approval--details (car asked))))
        (let* ((requested (cl-find-if
                           (lambda (event)
                             (and (equal "tool.requested" (plist-get event :type))
                                  (equal "eval_elisp" (plist-get (plist-get event :payload) :tool-name))))
                           (superchat-tape-replay-events session)))
               (arguments (json-parse-string
                           (plist-get (plist-get requested :payload) :arguments-text)
                           :object-type 'alist)))
          (should (equal text (alist-get 'expression arguments))))
        (should (equal '(b a) sf--order))
        (should (equal "tool.completed" (car (sf--result view))))))))

(ert-deftest several-forms/a-form-that-is-not-a-list-stops-them-all-and-is-named ()
  "Prose after a form is not evaluated after the form ran: nothing runs."
  (sf--fixture 'danger-full-access
      (list (sf--eval "(push 'ran sf--order) done") '(:text "SF-DONE"))
    (let ((view (smoke--open-chat)))
      (sf--run view "compute")
      (should-not sf--order)
      (pcase-let ((`(,type ,text) (sf--result view)))
        (should (equal "tool.failed" type))
        ;; The words the model is told to aim for stay.
        (should (string-match-p (regexp-quote "Expected exactly one expression; wrap multiple forms in progn") text))
        (should (string-match-p (regexp-quote "form 2 of 2 is not: done.") text))
        (should (string-match-p (regexp-quote "Nothing was evaluated") text))))))

(ert-deftest several-forms/a-string-or-number-among-lists-is-named-too ()
  (sf--fixture 'danger-full-access
      (list (sf--eval "(push 'ran sf--order) \"note\" 42 (push 'later sf--order)") '(:text "SF-DONE"))
    (let ((view (smoke--open-chat)))
      (sf--run view "compute")
      (should-not sf--order)
      (should (string-match-p (regexp-quote "form 2 of 4 is not: \"note\".")
                              (cadr (sf--result view)))))))

(ert-deftest several-forms/a-text-that-does-not-read-anywhere-evaluates-nothing ()
  "The whole text is read first: a second form left open stops the first."
  (sf--fixture 'danger-full-access
      (list (sf--eval "(push 'ran sf--order)\n(push 'never sf--order") '(:text "SF-DONE"))
    (let ((view (smoke--open-chat)))
      (sf--run view "compute")
      (should-not sf--order)
      (pcase-let ((`(,type ,text) (sf--result view)))
        (should (equal "tool.failed" type))
        (should (string-match-p (regexp-quote "Error: the expression does not read as Lisp, so nothing was evaluated: 1 parenthesis is never closed (the outermost at line 2") text))))))

(provide 'several-forms-regression)

;;; several-forms-regression.el ends here
