;;; m5-tool-feedback-regression.el --- Tool feedback regression -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Pins the narrow buffer-edit and Lisp-evaluation contracts exposed by M5:
;; no empty-search loop, truthful no-op feedback, one fully parsed expression,
;; and an error result the existing model correction path recognizes.
;;
;; This standalone suite reuses canonical smoke helpers as a library. It
;; neither loads nor modifies the leader-owned M3/M5 sets, and needs no scratch
;; files. All model calls are scripted; no user configuration is evaluated.
;;
;; Run from the repository root with an existing prepared dependency directory:
;;
;;   emacs -Q --batch \
;;     --eval '(progn (require (quote package)) (setq package-user-dir "/path/to/prepared/elpa" native-comp-enable-subr-trampolines nil) (package-initialize))' \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/m5-tool-feedback-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^m5-tool-feedback/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(when (getenv "SMOKE_REAL")
  (error "Tool feedback regression tests are offline; unset SMOKE_REAL"))

(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar m5tf--side-effect 0
  "Dynamically bound counter for proving rejected input never executes.")

(defmacro m5tf--with-fixture (script &rest body)
  "Run BODY with SCRIPT through the canonical isolated product fixture."
  (declare (indent 1) (debug t))
  `(let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
         (superchat-runtime--queue-timers (make-hash-table :test #'equal))
         (superchat-event-faults nil)
         (superchat-llm-reasoning nil)
         (superchat-llm-tool-names superchat-llm-default-tool-names))
     (smoke--with-fixture ,script ,@body)))

(defvar m5tf--reader-calls 0)
(defvar m5tf--reader-early-calls 0)
(defvar m5tf--reader-approved nil)

(defmacro m5tf--with-reader-fixture (&rest body)
  "Provide a real hash-table reader callback and clean up its symbol."
  (declare (indent 0) (debug t))
  `(let* ((m5tf--reader-calls 0)
          (m5tf--reader-early-calls 0)
          (m5tf--reader-approved nil)
          (m5tf-test-symbol
           (intern (symbol-name (cl-gensym "m5tf-reader-test-"))))
          (m5tf-hash-expression
           (format "#s(hash-table test %s data (a 1 b 2))" m5tf-test-symbol)))
     (unwind-protect
         (progn
           (define-hash-table-test
             m5tf-test-symbol #'eq
             (lambda (_key)
               (setq m5tf--reader-calls (1+ m5tf--reader-calls))
               (unless m5tf--reader-approved
                 (setq m5tf--reader-early-calls
                       (1+ m5tf--reader-early-calls)))
               0))
           ,@body)
       (unintern (symbol-name m5tf-test-symbol) obarray))))

(defun m5tf--feedback (tool result)
  "Use the real model correction classifier for TOOL and RESULT."
  (superchat--llm-tool-error-feedback
   (list :tool-results (list (cons tool result)))))

(defun m5tf--assert-error (result)
  "Require RESULT to use the existing tool-error convention."
  (should (stringp result))
  (should (string-prefix-p "Error:" result)))

(defun m5tf--settle (run)
  "Wait until exact RUN is terminal."
  (should (stringp run))
  (should
   (smoke--wait
    (lambda ()
      (superchat-runtime--terminal-p
       (plist-get (superchat-runtime-status run) :status)))
    20))
  run)

;;;; Edit observations

(ert-deftest m5-tool-feedback/edit/empty-old-is-rejected-before-search ()
  "The historical empty search loops synchronously; bound the counterexample."
  (with-temp-buffer
    (insert "unchanged")
    (set-buffer-modified-p nil)
    (let ((calls 0)
          (tick (buffer-chars-modified-tick))
          (original (symbol-function 'search-forward))
          result)
      (cl-letf (((symbol-function 'search-forward)
                 (lambda (&rest arguments)
                   (setq calls (1+ calls))
                   (when (> calls 4)
                     (error "Bounded empty-search counterexample"))
                   (apply original arguments))))
        (setq result (superchat-tool-edit-buffer (buffer-name) "" "new")))
      (m5tf--assert-error result)
      (should (= 0 calls))
      (should (equal "unchanged" (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should-not (buffer-modified-p)))))

(ert-deftest m5-tool-feedback/edit/identical-text-reports-no-change ()
  (with-temp-buffer
    (insert "old")
    (set-buffer-modified-p nil)
    (let ((tick (buffer-chars-modified-tick))
          (result (superchat-tool-edit-buffer (buffer-name) "old" "old")))
      (should (string-prefix-p "No change:" result))
      (should-not (m5tf--feedback "edit_buffer" result))
      (should (equal "old" (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should-not (buffer-modified-p)))))

(ert-deftest m5-tool-feedback/edit/one-match-is-case-sensitive-and-literal ()
  (with-temp-buffer
    (insert "old OLD")
    (let* ((replacement "\\1 &\nnew")
           (result (superchat-tool-edit-buffer
                    (buffer-name) "old" replacement)))
      (should-not (string-prefix-p "Error:" result))
      (should (equal (concat replacement " OLD") (buffer-string))))))

(ert-deftest m5-tool-feedback/edit/overlapping-matches-are-ambiguous ()
  "Exact uniqueness includes occurrences that share source characters."
  (with-temp-buffer
    (insert "banana")
    (set-buffer-modified-p nil)
    (let ((tick (buffer-chars-modified-tick))
          (result (superchat-tool-edit-buffer (buffer-name) "ana" "X")))
      (m5tf--assert-error result)
      (should (equal "banana" (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should-not (buffer-modified-p)))))

(ert-deftest m5-tool-feedback/edit/missing-text-preserves-content ()
  (with-temp-buffer
    (insert "old")
    (set-buffer-modified-p nil)
    (let ((tick (buffer-chars-modified-tick))
          (result (superchat-tool-edit-buffer (buffer-name) "OLD" "new")))
      (m5tf--assert-error result)
      (should (m5tf--feedback "edit_buffer" result))
      (should (equal "old" (buffer-string)))
      (should (= tick (buffer-chars-modified-tick)))
      (should-not (buffer-modified-p)))))

(ert-deftest m5-tool-feedback/edit/ambiguous-text-preserves-content ()
  (with-temp-buffer
    (insert "old old")
    (let ((tick (buffer-chars-modified-tick))
          (result (superchat-tool-edit-buffer (buffer-name) "old" "new")))
      (m5tf--assert-error result)
      (should (m5tf--feedback "edit_buffer" result))
      (should (equal "old old" (buffer-string)))
      (should (= tick (buffer-chars-modified-tick))))))

(ert-deftest m5-tool-feedback/edit/identical-text-still-requires-one-match ()
  "A no-op request cannot pretend it found missing or ambiguous old text."
  (dolist (text '("absent" "old old"))
    (with-temp-buffer
      (insert text)
      (let ((tick (buffer-chars-modified-tick))
            (result (superchat-tool-edit-buffer (buffer-name) "old" "old")))
        (m5tf--assert-error result)
        (should (equal text (buffer-string)))
        (should (= tick (buffer-chars-modified-tick)))))))

(ert-deftest m5-tool-feedback/edit/buffer-success-neither-saves-nor-evaluates ()
  "Changing a live config buffer does not establish persistence or activation."
  (m5tf--with-fixture nil
    (let* ((file (expand-file-name "m5tf-config.el" user-emacs-directory))
           (old "(setq m5tf--side-effect 10)\n")
           (new "(setq m5tf--side-effect 20)\n")
           (m5tf--side-effect 0))
      (with-temp-file file (insert old))
      (with-current-buffer (find-file-noselect file)
        (let ((result (superchat-tool-edit-buffer (buffer-name) old new)))
          (should-not (string-prefix-p "Error:" result))
          (should (string-match-p "not save\\(?:d\\)?\\|does not save" result))
          (should (string-match-p "not saved or evaluated\\|not evaluated\\|does not save or evaluate" result))
          (should (equal new (buffer-string)))
          (should (buffer-modified-p))
          (should (= 0 m5tf--side-effect))))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (should (equal old (buffer-string)))))))

;;;; Parse before evaluation

(ert-deftest m5-tool-feedback/eval/one-expression-keeps-value-and-context ()
  (with-temp-buffer
    (insert "source text")
    (let ((name (buffer-name))
          (result (superchat-tool-eval-elisp "(buffer-name)")))
      (should (equal result (format "Result: %S" name))))
    (should (equal "Result: nil" (superchat-tool-eval-elisp "nil")))))

(ert-deftest m5-tool-feedback/eval/whitespace-and-comments-are-accepted ()
  (dolist (expression '(" \n\t(+ 1 2)\n \r\t"
                        "; heading\n(+ 1 2) ; trailing without newline"
                        "(+ 1 2)\n; one\n; two\n  "
                        "(progn \"; not a comment\" (+ 1 2)) ; tail"))
    (ert-info ((format "valid input: %S" expression))
      (should (equal "Result: 3" (superchat-tool-eval-elisp expression))))))

(ert-deftest m5-tool-feedback/eval/one-progn-can-perform-several-operations ()
  (let ((m5tf--side-effect 0))
    (should
     (equal "Result: 2"
            (superchat-tool-eval-elisp
             "(progn (setq m5tf--side-effect 1) (setq m5tf--side-effect 2))")))
    (should (= 2 m5tf--side-effect))))

(ert-deftest m5-tool-feedback/eval/extra-form-is-rejected-before-first-effect ()
  "Rejecting only after evaluation would still execute the first mutation."
  (let* ((m5tf--side-effect 0)
         (result
          (superchat-tool-eval-elisp
           "(progn (setq m5tf--side-effect 1) \"OK\") (setq m5tf--side-effect 2)")))
    (m5tf--assert-error result)
    (should (= 0 m5tf--side-effect))
    (should (m5tf--feedback "eval_elisp" result))))

(ert-deftest m5-tool-feedback/eval/extra-form-after-comment-is-rejected ()
  (let* ((m5tf--side-effect 0)
         (result
          (superchat-tool-eval-elisp
           "(setq m5tf--side-effect 1) ; comment\n(setq m5tf--side-effect 2)")))
    (m5tf--assert-error result)
    (should (= 0 m5tf--side-effect))))

(ert-deftest m5-tool-feedback/eval/malformed-tail-is-rejected-before-first-effect ()
  "An incomplete second form is not equivalent to comment-only EOF."
  (dolist (tail '("(" ")" "\"unterminated" "# invalid-reader-tail"))
    (let* ((m5tf--side-effect 0)
           (result
            (superchat-tool-eval-elisp
             (concat "(setq m5tf--side-effect 1) " tail))))
      (ert-info ((format "invalid tail: %S" tail))
        (m5tf--assert-error result)
        (should (= 0 m5tf--side-effect))
        (should (m5tf--feedback "eval_elisp" result))))))

(ert-deftest m5-tool-feedback/eval/empty-or-malformed-input-is-a-tool-error ()
  (dolist (expression '("" " \n " "; comment only" "(list 1" ")"))
    (let ((result (superchat-tool-eval-elisp expression)))
      (ert-info ((format "invalid input: %S" expression))
        (m5tf--assert-error result)
        (should (m5tf--feedback "eval_elisp" result))))))

(ert-deftest m5-tool-feedback/eval/runtime-error-reaches-existing-correction-path ()
  (let* ((result (superchat-tool-eval-elisp "(error \"M5TF_EVALUATION_FAILED\")"))
         (feedback (m5tf--feedback "eval_elisp" result)))
    (m5tf--assert-error result)
    (should (string-match-p "M5TF_EVALUATION_FAILED" result))
    (should (stringp feedback))
    (should (string-match-p "Do not claim success" feedback))
    (should (string-match-p "Correct the arguments" feedback))))

(ert-deftest m5-tool-feedback/eval/runtime-error-does-not-promise-rollback ()
  "A valid expression may fail after a real effect; report error without hiding it."
  (let* ((m5tf--side-effect 0)
         (result
          (superchat-tool-eval-elisp
           "(progn (setq m5tf--side-effect 1) (error \"M5TF_PARTIAL_EFFECT\"))")))
    (m5tf--assert-error result)
    (should (string-match-p "M5TF_PARTIAL_EFFECT" result))
    (should (= 1 m5tf--side-effect))))

;;;; Actual provider/tool/Authority continuation

(ert-deftest m5-tool-feedback/request/eval-error-is-in-the-followup-provider-prompt ()
  "The real wrapped call, rather than a fabricated result, prompts correction."
  (m5tf--with-fixture
      '((:tool-uses (("eval_elisp" (expression . "(error \"M5TF_PROVIDER_FAILURE\")"))))
        (:text "No change was made."))
    (let* ((asked 0)
           (superchat-authority-approval-function
            (lambda (_request) (setq asked (1+ asked)) t))
           (view (smoke--open-chat))
           (run (m5tf--settle (smoke--send view "Evaluate the requested diagnostic.")))
           (requests (reverse smoke--requests))
           (followup (plist-get (cadr requests) :prompt-text)))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 1 asked))
      (should (= 2 (length requests)))
      (should (member "eval_elisp" (plist-get (car requests) :tools)))
      (should (string-match-p "M5TF_PROVIDER_FAILURE" followup))
      (should (string-match-p "Tool execution failed" followup))
      (should (string-match-p "Do not claim success" followup)))))

(ert-deftest m5-tool-feedback/request/declined-eval-never-executes ()
  "Parsing improvements do not bypass the existing Authority approval gate."
  (m5tf--with-fixture
      '((:tool-uses (("eval_elisp" (expression . "(setq m5tf--side-effect 99)"))))
        (:text "The requested evaluation was declined."))
    (let* ((asked 0)
           (m5tf--side-effect 0)
           (superchat-authority-approval-function
            (lambda (_request) (setq asked (1+ asked)) nil))
           (view (smoke--open-chat)))
      (m5tf--settle (smoke--send view "Evaluate the requested diagnostic."))
      (should (= 1 asked))
      (should (= 0 m5tf--side-effect))
      (let ((followup (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "user or a policy prevented" followup))
        (should-not (string-match-p "Correct the arguments" followup))))))

(ert-deftest m5-tool-feedback/eval/error-message-is-not-an-authority-refusal ()
  "Words in an evaluation error do not become an authorization decision."
  (dolist (expression '("(symbol-value (make-symbol \"cancelled\"))"
                        "(error \"Search failed: blocked\")"))
    (let* ((result (superchat-tool-eval-elisp expression))
           (feedback (m5tf--feedback "eval_elisp" result)))
      (m5tf--assert-error result)
      (should (stringp feedback))
      (should (string-match-p "Correct the arguments" feedback))
      (should-not (string-match-p "user or a policy prevented" feedback)))))

(ert-deftest m5-tool-feedback/eval/error-exception-is-limited-to-native-eval ()
  "A result from another tool retains the existing refusal classification."
  (let* ((result (superchat-tool-eval-elisp "(error \"Search failed: blocked\")"))
         (feedback (m5tf--feedback "read_buffer" result)))
    (m5tf--assert-error result)
    (should (string-match-p "user or a policy prevented" feedback))
    (should-not (string-match-p "Correct the arguments" feedback))))

(ert-deftest m5-tool-feedback/request/real-refusal-wins-in-a-mixed-error-round ()
  "Exempt the evaluation error entry, not the whole round from refusal checks."
  (let* ((result (superchat-tool-eval-elisp "(error \"Search failed: blocked\")"))
         (feedback
          (superchat--llm-tool-error-feedback
           (list :tool-results
                 (list (cons "eval_elisp" result)
                       (cons "read_buffer" "[Tool call cancelled by user]"))))))
    (should (stringp feedback))
    (should (string-match-p "user or a policy prevented" feedback))
    (should-not (string-match-p "Correct the arguments" feedback))))

;;;; Reader admission

(ert-deftest m5-tool-feedback/eval/classifier-does-not-run-a-reader-callback ()
  "Reading a hash-table literal can invoke registered Lisp hash functions."
  (m5tf--with-reader-fixture
    (dolist (expression
             (list m5tf-hash-expression
                   (concat "'" m5tf-hash-expression)
                   (concat "[" m5tf-hash-expression "]")
                   (concat "(progn " m5tf-hash-expression ")")
                   (string-replace "hash-table" "hash\\-table" m5tf-hash-expression)
                   (concat m5tf-hash-expression " extra-form")))
      (should-not (superchat-tools--eval-elisp-read-only-p expression))
      (should (= 0 m5tf--reader-calls))
      (should (= 0 m5tf--reader-early-calls)))
    (should (superchat-tools--eval-elisp-read-only-p "(buffer-name)"))))

(ert-deftest m5-tool-feedback/request/declined-reader-literal-has-no-callback-effect ()
  "The real Authority preflight must not run Lisp through its classifier."
  (m5tf--with-reader-fixture
    (m5tf--with-fixture
        (list (list :tool-uses
                    (list (list "eval_elisp"
                                (cons 'expression m5tf-hash-expression))))
              '(:text "The evaluation was declined."))
      (let* ((asked 0)
             (superchat-authority-approval-function
              (lambda (_request) (setq asked (1+ asked)) nil))
             (view (smoke--open-chat)))
        (m5tf--settle (smoke--send view "Evaluate the requested diagnostic."))
        (should (= 1 asked))
        (should (= 0 m5tf--reader-calls))
        (should (= 0 m5tf--reader-early-calls))))))

(ert-deftest m5-tool-feedback/request/approved-reader-callback-runs-only-after-approval ()
  "A conservative classification does not remove approved reader capabilities."
  (m5tf--with-reader-fixture
    (m5tf--with-fixture
        (list (list :tool-uses
                    (list (list "eval_elisp"
                                (cons 'expression m5tf-hash-expression))))
              '(:text "The approved literal was evaluated."))
      (let* ((asked 0)
             (superchat-authority-approval-function
              (lambda (_request)
                (setq asked (1+ asked)
                      m5tf--reader-approved t)
                t))
             (view (smoke--open-chat)))
        (m5tf--settle (smoke--send view "Evaluate the requested diagnostic."))
        (should (= 1 asked))
        (should (> m5tf--reader-calls 0))
        (should (= 0 m5tf--reader-early-calls))
        (should (string-match-p (regexp-quote "Result: #s(hash-table")
                                (plist-get (car smoke--requests) :prompt-text)))))))

(provide 'm5-tool-feedback-regression)
;;; m5-tool-feedback-regression.el ends here
