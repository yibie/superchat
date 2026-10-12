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

;;;; Buffer names are values, including their whitespace

(ert-deftest m5-tool-feedback/buffers/listed-names-read-the-exact-live-buffers ()
  "Decode actual listed names and read the matching live buffer, not a trim twin."
  (let* ((stem (symbol-name (cl-gensym "m5tf-listed-")))
         (cases (list (cons (concat " " stem) "M5TF_LEADING_CONTENT")
                      (cons stem "M5TF_UNSPACED_DECOY")
                      (cons (concat stem " \"quoted\"\\part\nnext")
                            "M5TF_ESCAPED_CONTENT")))
         (buffers nil)
         (expected nil))
    (unwind-protect
        (progn
          (dolist (entry cases)
            (let ((buffer (generate-new-buffer (car entry))))
              (push buffer buffers)
              (with-current-buffer buffer
                (insert (cdr entry))
                (push (cons (buffer-name) (cdr entry)) expected))))
          ;; The tool must establish its own readable, single-line encoding.
          (let* ((print-escape-newlines nil)
                 (rows (cl-remove-if-not
                        (lambda (line)
                          (string-match-p (regexp-quote stem) line))
                        (split-string (superchat-tool-list-buffers) "\n" t)))
                 (decoded nil))
            (should (= 3 (length rows)))
            (dolist (row rows)
              (let* ((name-read (read-from-string row))
                     (name (car name-read)))
                (should (stringp name))
                (should (assoc name expected))
                ;; Feed the decoded output directly into the public read
                ;; tool; using the fixture's original name would be a shortcut.
                (should (equal (cdr (assoc name expected))
                               (let ((result (superchat-tool-read-buffer name)))
                                 ;; The text follows the header's `Content:' line.
                                 (and (string-match "\nContent:\n" result)
                                      (substring result (match-end 0))))))
                (let* ((size-read (read-from-string row (cdr name-read)))
                       (mode-read (read-from-string row (cdr size-read))))
                  (should (= (length (cdr (assoc name expected))) (car size-read)))
                  (should (eq 'fundamental-mode (car mode-read))))
                (push name decoded)))
            ;; A formatter that trims the leading name reads the decoy twice
            ;; and loses the intended buffer, even though both reads succeed.
            (should (equal (sort decoded #'string<)
                           (sort (mapcar #'car expected) #'string<)))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

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
           (superchat-approval-function
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
           (superchat-approval-function
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

(ert-deftest m5-tool-feedback/request/declined-reader-literal-has-no-callback-effect ()
  "Nothing before the approval reads the expression: no reader callback runs for a declined call."
  (m5tf--with-reader-fixture
    (m5tf--with-fixture
        (list (list :tool-uses
                    (list (list "eval_elisp"
                                (cons 'expression m5tf-hash-expression))))
              '(:text "The evaluation was declined."))
      (let* ((asked 0)
             (superchat-approval-function
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
             (superchat-approval-function
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

;;;; Bounded error rendering and one copy in model context

(defun m5tf--assert-bounded-error (result)
  "Check the public evaluation-error prefix and its details budget."
  (m5tf--assert-error result)
  (should (string-prefix-p "Error: evaluating expression: " result))
  (should (<= (length result)
              (+ (length "Error: evaluating expression: ") 1024))))

(defun m5tf--count-text (needle text)
  "Count exact nonoverlapping NEEDLE occurrences in TEXT."
  (let ((start 0) (count 0))
    (while (string-match (regexp-quote needle) text start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(ert-deftest m5-tool-feedback/error/sparse-keymap-retains-type-with-short-details ()
  "A wrong array operation should not dump hundreds of unrelated key bindings."
  (with-temp-buffer
    (let ((map (make-sparse-keymap))
          (print-length nil) (print-level nil) (print-circle nil))
      (dotimes (index 500)
        (define-key map (vector (+ index 32)) 'ignore))
      (use-local-map map)
      (let ((result (superchat-tool-eval-elisp "(aref (current-local-map) 0)")))
        (m5tf--assert-bounded-error result)
        (should (string-match-p "Wrong type argument: arrayp" result))
        (should (string-match-p "keymap" result))
        ;; Structural printing shrinks this fixture well below the final
        ;; string cap; merely printing the whole object then cutting at 1024
        ;; characters does not meet this observation.
        (should (< (length result) 512))))))

(ert-deftest m5-tool-feedback/error/long-string-has-an-explicit-truncation-marker ()
  (let ((result
         (superchat-tool-eval-elisp
          "(error \"%s\" (concat \"M5TF_LONG_BEGIN:\" (make-string 6000 ?汉) \"M5TF_LONG_END\"))")))
    (m5tf--assert-bounded-error result)
    (should (string-match-p "M5TF_LONG_BEGIN:" result))
    (should-not (string-match-p "M5TF_LONG_END" result))
    (let ((case-fold-search t))
      (should (string-match-p "truncated" result)))))

(ert-deftest m5-tool-feedback/error/circular-data-retains-a-bounded-real-diagnostic ()
  "A compatibility guard: a circular value still yields a useful real error."
  ;; Keep the old implementation safe when replaying the counterexample.
  ;; Assert the delivered diagnostic, not the implementation's printer settings.
  (let* ((print-length nil) (print-level nil) (print-circle t)
         (result
          (superchat-tool-eval-elisp
           "(let ((data (list 'M5TF_CYCLE_MARKER))) (setcdr data data) (signal 'wrong-type-argument (list 'arrayp data)))")))
    (m5tf--assert-bounded-error result)
    (should (string-match-p "Wrong type argument: arrayp" result))
    (should (string-match-p "M5TF_CYCLE_MARKER" result))))

(ert-deftest m5-tool-feedback/feedback/error-body-is-not-copied ()
  (let* ((error-result
          (superchat-tool-eval-elisp "(error \"M5TF_FEEDBACK_COPY_SENTINEL\")"))
         (feedback (m5tf--feedback "eval_elisp" error-result)))
    (m5tf--assert-error error-result)
    (should (string-match-p "eval_elisp" feedback))
    (should (string-match-p "preceding tool results" feedback))
    (should (string-match-p "Correct the arguments" feedback))
    (should-not (string-match-p "M5TF_FEEDBACK_COPY_SENTINEL" feedback))
    (should-not (string-match-p (regexp-quote error-result) feedback))))

(ert-deftest m5-tool-feedback/request/error-body-is-once-in-the-real-followup ()
  "Count the complete returned error, not its marker inside call arguments."
  (let ((expected "Error: evaluating expression: M5TF_PROVIDER_DEDUP_SENTINEL"))
    (m5tf--with-fixture
        '((:tool-uses
           (("eval_elisp" (expression . "(error \"M5TF_PROVIDER_DEDUP_SENTINEL\")"))))
          (:text "No change was made."))
      (let* ((asked 0)
             (superchat-approval-function
              (lambda (_request) (setq asked (1+ asked)) t))
             (view (smoke--open-chat)))
        (m5tf--settle (smoke--send view "Evaluate the requested diagnostic."))
        (should (= 1 asked))
        (should (= 2 (length smoke--requests)))
        (let ((followup (plist-get (car smoke--requests) :prompt-text)))
          ;; The call argument contains only the short sentinel. This exact
          ;; full string therefore proves the actual result still arrived.
          (should (= 1 (m5tf--count-text expected followup)))
          (should (string-match-p "Tool execution failed" followup))
          (should (string-match-p "preceding tool results" followup)))))))

(ert-deftest m5-tool-feedback/feedback/refusal-is-still-no-retry-without-copying ()
  (let* ((refusal "[Tool call cancelled by user]")
         (feedback (m5tf--feedback "eval_elisp" refusal)))
    (should (string-match-p "eval_elisp" feedback))
    (should (string-match-p "user or a policy prevented" feedback))
    (should (string-match-p "do not retry" feedback))
    (should-not (string-match-p "Correct the arguments" feedback))
    (should-not (string-match-p (regexp-quote refusal) feedback))))

;;;; Correctable call scope and actual refusal continuation

(defvar m5tf--provider-snapshots nil)
(defvar m5tf--denied-side-effect 0)

(defmacro m5tf--capture-provider-messages (&rest body)
  "Snapshot provider roles before mutation while running BODY.
These observations test delivery of control text, not model comprehension."
  (declare (indent 0) (debug t))
  `(let ((m5tf--provider-snapshots nil)
         (network (symbol-function 'smoke--fake-network)))
     (cl-letf (((symbol-function 'smoke--fake-network)
                (lambda (mode provider prompt partial response error multi)
                  (push
                   (list
                    :users
                    (cl-loop for item in (llm-chat-prompt-interactions prompt)
                             when (eq 'user (llm-chat-prompt-interaction-role item))
                             collect (copy-sequence
                                      (llm-chat-prompt-interaction-content item)))
                    :results
                    (cl-loop for item in (llm-chat-prompt-interactions prompt)
                             when (eq 'tool-results
                                      (llm-chat-prompt-interaction-role item))
                             collect (prin1-to-string
                                      (llm-chat-prompt-interaction-tool-results item))))
                   m5tf--provider-snapshots)
                  (funcall network mode provider prompt
                           partial response error multi))))
       ,@body)))

(ert-deftest m5-tool-feedback/request/corrected-call-keeps-original-task-in-scope ()
  "A real failure and successful retry retain a conditional return-to-task cue.
The scripted provider determines the actions; this test does not establish
whether any model follows the instruction or completes the original task."
  (m5tf--with-fixture
      '((:tool-uses
         (("eval_elisp"
           (expression . "(progn (setq m5tf--side-effect (1+ m5tf--side-effect)) (car))"))))
        (:tool-uses
         (("eval_elisp"
           (expression . "(progn (setq m5tf--side-effect (1+ m5tf--side-effect)) (car '(M5TF_RECOVERED)))"))))
        (:text "The call recovered; the original task is still the objective."))
    (let* ((m5tf--side-effect 0)
           (asked 0)
           (superchat-approval-function
            (lambda (_request) (cl-incf asked) t))
           (view (smoke--open-chat)))
      (m5tf--capture-provider-messages
        (let* ((run (m5tf--settle
                     (smoke--send view "M5TF_ORIGINAL_TASK: recover the call, then report the observed result.")))
               (requests (reverse m5tf--provider-snapshots))
               (initial-users (plist-get (nth 0 requests) :users))
               (failure-users (plist-get (nth 1 requests) :users))
               (success-users (plist-get (nth 2 requests) :users))
               (feedback (cadr failure-users)))
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (should (= 3 (length requests)))
          (should (= 2 asked))
          (should (= 2 m5tf--side-effect))
          (should (= 1 (length initial-users)))
          (should (string-match-p "M5TF_ORIGINAL_TASK" (car initial-users)))
          (should (= 2 (length failure-users)))
          (should (equal (car initial-users) (car failure-users)))
          (should (equal failure-users success-users))
          ;; Only the generated user-role instruction can satisfy this:
          ;; neither tool documentation nor a tool's result is searched.
          (should (string-match-p "failed call" feedback))
          (should (string-match-p
                   (regexp-quote "Once it succeeds, continue the user's original request.")
                   feedback))
          (should (string-match-p "Do not claim success" feedback))
          (should (= 1 (length (plist-get (nth 1 requests) :results))))
          (let ((results (plist-get (nth 2 requests) :results)))
            (should (= 2 (length results)))
            (should (string-match-p "Wrong number of arguments" (car results)))
            (should (string-match-p "Result: M5TF_RECOVERED" (cadr results)))))))))

(ert-deftest m5-tool-feedback/request/mixed-refusal-survives-an-unrelated-success ()
  "A real declined call dominates a mixed round and survives an allowed read."
  (let ((failing "(progn (setq m5tf--side-effect (1+ m5tf--side-effect)) (error \"M5TF_MIXED_RUNTIME_FAILURE\"))")
        (denied "(progn (setq m5tf--denied-side-effect 1) 'M5TF_DENIED_ACTION)"))
    (m5tf--with-fixture
        (list
         (list :tool-uses
               (list (list "eval_elisp" (cons 'expression failing))
                     (list "eval_elisp" (cons 'expression denied))))
         (list :tool-uses
               (list (list "read_buffer" (cons 'buffer smoke--origin-buffer-name))))
         '(:text "The denied action was not retried."))
      (let* ((m5tf--side-effect 0)
             (m5tf--denied-side-effect 0)
             (asked-tools nil)
             (superchat-approval-function
              (lambda (request)
                (push (plist-get request :tool-name) asked-tools)
                (not (equal denied (car (plist-get request :args))))))
             (view (smoke--open-chat)))
        (with-current-buffer smoke--origin-buffer-name
          (insert "M5TF_ALLOWED_READ"))
        (m5tf--capture-provider-messages
          (let* ((run (m5tf--settle
                       (smoke--send view "M5TF_REFUSAL_TASK: keep the declined action declined.")))
                 (requests (reverse m5tf--provider-snapshots))
                 (failure-users (plist-get (nth 1 requests) :users))
                 (success-users (plist-get (nth 2 requests) :users))
                 (feedback (cadr failure-users))
                 (session (buffer-local-value 'superchat--session-id view)))
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
            (should (= 3 (length requests)))
            (should (= 2 (cl-count "eval_elisp" asked-tools :test #'equal)))
            (should (= 1 m5tf--side-effect))
            (should (= 0 m5tf--denied-side-effect))
            ;; The allowed read also crosses Authority, whether or not that
            ;; read is exempt from presenting an approval prompt.
            (should (= 3 (cl-count "tool.requested" (smoke--types session)
                                    :test #'equal)))
            (should (= 2 (length failure-users)))
            (should (equal failure-users success-users))
            (should (string-match-p "user or a policy prevented" feedback))
            (should (string-match-p "do not retry it" feedback))
            (should-not (string-match-p "Correct the arguments" feedback))
            (should-not (string-match-p "Once it succeeds" feedback))
            (let ((results (plist-get (nth 2 requests) :results)))
              (should (= 2 (length results)))
              (should (string-match-p "M5TF_MIXED_RUNTIME_FAILURE" (car results)))
              (should (string-match-p "Tool call denied by user" (car results)))
              (should (string-match-p "M5TF_ALLOWED_READ" (cadr results))))))))))

(provide 'm5-tool-feedback-regression)
;;; m5-tool-feedback-regression.el ends here
