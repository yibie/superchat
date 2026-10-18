;;; edit-all-regression.el --- edit_buffer counts, matches a regexp, replaces all -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 9e (reports/guidance-2026-10-11/SLICE-9.md).  `edit_buffer' replaced
;; one literal match only.  In the user's first Run of 2026-10-18 it was on
;; the tool surface the whole time, used 0 times, while `eval_elisp' ran 40
;; times to do regexp replacements.  It now takes three more arguments:
;; `regexp' (old_string is a regexp, new_string may use \\N), `count_only'
;; (say how many matches and show the first three, change nothing) and `all'
;; (replace every match -- refused unless the same execution counted the same
;; pattern in the same buffer first).  A replacement of all is one
;; `edit.applied' fact with the number replaced.  Without the new arguments
;; the tool does exactly what it did.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/edit-all-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^edit-all/")'

;;; Code:

(load (expand-file-name "eval-failure-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst ea--buffer "ea-target")

(defmacro ea--with (lines script &rest body)
  "Run BODY with buffer `ea-target' holding LINES and SCRIPT as the model."
  (declare (indent 2) (debug t))
  `(let ((target (get-buffer-create ea--buffer)))
     (with-current-buffer target
       (erase-buffer)
       (insert ,lines)
       (undo-boundary)
       (set-buffer-modified-p nil))
     (unwind-protect
         (ef--fixture ,script ,@body)
       (with-current-buffer target (set-buffer-modified-p nil))
       (kill-buffer target))))

(defun ea--items (n)
  (mapconcat (lambda (i) (format "item-%d foo\n" i)) (number-sequence 1 n) ""))

(defun ea--call (&rest args)
  "One edit_buffer call on `ea-target' with ARGS, an alist of extra arguments."
  (list :tool-uses
        (list (cons "edit_buffer"
                    (append (list (cons 'buffer_name ea--buffer)) args)))))

(defun ea--text ()
  (with-current-buffer ea--buffer (buffer-string)))

(defun ea--results (view)
  "The prompt texts after each request, newest last."
  (ignore view)
  (mapcar (lambda (request) (plist-get request :prompt-text))
          (reverse smoke--requests)))

(defun ea--last-prompt ()
  (plist-get (car smoke--requests) :prompt-text))

(defun ea--facts (view type)
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type)))
                    (superchat-tape-replay-events
                     (buffer-local-value 'superchat--session-id view))))

(ert-deftest edit-all/count-only-counts-and-changes-nothing ()
  (ea--with (ea--items 5)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "count")
      (should (equal (ea--items 5) (ea--text)))
      (should-not (with-current-buffer ea--buffer (buffer-modified-p)))
      (let ((prompt (ea--last-prompt)))
        (should (string-match-p "5 matches in buffer ea-target" prompt))
        (should (string-match-p "line 1: item-1 foo" prompt))
        (should (string-match-p "line 3: item-3 foo" prompt))
        (should-not (string-match-p "line 4: item-4 foo" prompt))))))

(ert-deftest edit-all/all-without-a-count-is-refused ()
  (ea--with (ea--items 5)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal (ea--items 5) (ea--text)))
      (should (string-match-p "count_only" (ea--last-prompt)))
      (should-not (ea--facts view "edit.applied")))))

(ert-deftest edit-all/all-after-a-count-replaces-every-match-in-one-fact ()
  (ea--with (ea--items 199)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal (replace-regexp-in-string "foo" "bar" (ea--items 199)) (ea--text)))
      (should (with-current-buffer ea--buffer (buffer-modified-p)))
      (should (string-match-p "Replaced 199 occurrences in buffer ea-target" (ea--last-prompt)))
      (let ((facts (ea--facts view "edit.applied")))
        (should (= 1 (length facts)))
        (should (= 199 (plist-get (plist-get (car facts) :payload) :edits)))
        (should (equal "edit_buffer" (plist-get (plist-get (car facts) :payload) :source))))
      ;; The change review and the Unsaved row see the buffer as before.
      (should (cl-some (lambda (event)
                         (equal ea--buffer (plist-get (plist-get event :payload) :buffer)))
                       (ea--facts view "tool.completed")))
      ;; One undo puts it all back.
      (with-current-buffer ea--buffer
        ;; The command loop puts a boundary after each command; batch does not.
        (undo-boundary)
        (let ((last-command nil)) (undo-only))
        (should (equal (ea--items 199) (buffer-string)))))))

(ert-deftest edit-all/all-is-one-undo-step-even-across-boundaries ()
  "Something that puts an undo boundary while the replacements run -- a timer,
a process filter -- does not split them."
  (with-temp-buffer
    (buffer-enable-undo)
    (insert (ea--items 5))
    (undo-boundary)
    (let ((replace (symbol-function 'replace-match)))
      (cl-letf (((symbol-function 'replace-match)
                 (lambda (&rest args)
                   (prog1 (apply replace args) (undo-boundary)))))
        (should (= 5 (superchat-tools--edit-buffer-apply-more
                      (buffer-name) "foo" "bar" nil)))))
    (undo-boundary)
    (let ((last-command nil)) (undo-only))
    (should (equal (ea--items 5) (buffer-string)))))

(ert-deftest edit-all/a-count-of-another-pattern-does-not-allow-all ()
  (ea--with (ea--items 5)
      (list (ea--call '(old_string . "item") '(new_string . "x") '(count_only . t))
            (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal (ea--items 5) (ea--text))))))

(ert-deftest edit-all/a-count-in-an-earlier-execution-does-not-allow-all ()
  (ea--with (ea--items 5)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            '(:text "EA-COUNTED")
            (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "count")
      (ef--run view "now replace")
      (should (equal (ea--items 5) (ea--text))))))

(ert-deftest edit-all/a-regexp-replaces-with-groups ()
  (ea--with (ea--items 3)
      (list (ea--call '(old_string . "item-\\([0-9]+\\) foo") '(new_string . "n\\1")
                      '(regexp . t) '(count_only . t))
            (ea--call '(old_string . "item-\\([0-9]+\\) foo") '(new_string . "n\\1")
                      '(regexp . t) '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal "n1\nn2\nn3\n" (ea--text))))))

(ert-deftest edit-all/one-regexp-match-is-replaced-without-all ()
  (ea--with "alpha 12\nbeta\n"
      (list (ea--call '(old_string . "[0-9]+") '(new_string . "N") '(regexp . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal "alpha N\nbeta\n" (ea--text))))))

(ert-deftest edit-all/without-the-new-arguments-nothing-changed ()
  "Guard: the literal, one-match path answers as before."
  (ea--with "one two\nfoo foo\n"
      (list (ea--call '(old_string . "one") '(new_string . "ONE"))
            (ea--call '(old_string . "foo") '(new_string . "bar"))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "replace")
      (should (equal "ONE two\nfoo foo\n" (ea--text)))
      (let ((prompts (ea--results view)))
        (should (string-match-p "Replaced 1 occurrence in buffer ea-target" (nth 1 prompts)))
        (should (string-match-p "Found 2 matches for the text to replace" (nth 2 prompts))))
      ;; A literal old_string is never read as a regexp.
      (should-not (ea--facts view "edit.applied")))))

(ert-deftest edit-all/count-only-needs-no-edit-permission ()
  "Under read-only the count is answered; the replacement is refused."
  (ea--with (ea--items 2)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((superchat-sandbox-default-mode 'read-only)
          (view (smoke--open-chat)))
      (ef--run view "replace")
      (should (string-match-p "2 matches in buffer ea-target" (nth 1 (ea--results view))))
      (should (equal (ea--items 2) (ea--text))))))

(ert-deftest edit-all/the-description-grows-by-at-most-300-characters ()
  (let ((superchat-llm-tool-names superchat-llm-default-tool-names))
    (superchat-llm-tools-reload)
    (let ((tool (cl-find "edit_buffer" superchat-llm-tools-list
                         :key #'llm-tool-name :test #'equal)))
      (should tool)
      (should (<= (length (llm-tool-description tool)) (+ 430 300)))
      (should (equal '("buffer_name" "old_string" "new_string" "regexp" "all" "count_only")
                     (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool)))))))

(defun ea--failed (view)
  "The status and error type of each `tool.failed' edit_buffer fact in VIEW's Session."
  (mapcar (lambda (event)
            (let ((payload (plist-get event :payload)))
              (list (plist-get payload :status) (plist-get payload :error-type))))
          (cl-remove-if-not
           (lambda (event)
             (equal "edit_buffer" (plist-get (plist-get event :payload) :tool-name)))
           (ea--facts view "tool.failed"))))

(ert-deftest edit-all/an-edit-error-is-a-failed-call ()
  "An `Error:' answer of edit_buffer is a failure, as an eval error is (9a);
the text the model receives is unchanged."
  (ea--with "one two\nfoo foo\n"
      (list (ea--call '(old_string . "absent") '(new_string . "x"))
            (ea--call '(old_string . "foo") '(new_string . "bar"))
            (ea--call '(old_string . "foo") '(new_string . "bar") '(all . t))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "edit")
      (should (equal '(("failed" "not-found") ("failed" "ambiguous") ("failed" "needs-count"))
                     (ea--failed view)))
      (should-not (cl-some (lambda (event)
                             (equal "edit_buffer"
                                    (plist-get (plist-get event :payload) :tool-name)))
                           (ea--facts view "tool.completed")))
      (let ((prompts (ea--results view)))
        (should (string-match-p "Error: Could not find text to replace in buffer ea-target"
                                (nth 1 prompts)))
        (should-not (string-match-p (regexp-quote "[Tool call failed") (nth 1 prompts)))
        (should (string-match-p "Error: Found 2 matches" (nth 2 prompts)))))))

(ert-deftest edit-all/edit-errors-feed-the-failure-brake ()
  (ea--with "one\n"
      (list (ea--call '(old_string . "a1") '(new_string . "x"))
            (ea--call '(old_string . "a2") '(new_string . "x"))
            (ea--call '(old_string . "a3") '(new_string . "x"))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "edit")
      (should (string-match-p "The last 3 tool calls failed" (ea--last-prompt))))))

(ert-deftest edit-all/a-denied-edit-and-a-no-change-are-not-failures ()
  "Guard: a permission refusal stays denied; an identical replacement is not an error."
  (ea--with "one\n"
      (list (ea--call '(old_string . "one") '(new_string . "one"))
            '(:text "EA-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "edit")
      (should-not (ea--failed view))))
  (ea--with "one\n"
      (list (ea--call '(old_string . "one") '(new_string . "ONE"))
            '(:text "EA-DONE"))
    (let ((superchat-sandbox-default-mode 'read-only)
          (view (smoke--open-chat)))
      (ef--run view "edit")
      ;; The body itself answers the refusal; it is not a failure.
      (should (string-match-p (regexp-quote "[Tool call denied: this Session is read-only")
                              (ea--last-prompt)))
      (should-not (ea--failed view)))))

(ert-deftest edit-all/a-count-is-forgotten-when-its-execution-ends ()
  (ea--with (ea--items 3)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            '(:text "EA-DONE"))
    (let* ((view (smoke--open-chat))
           (run (ef--run view "count")))
      (should-not (cl-loop for key being the hash-keys of superchat-tools--edit-counted
                           thereis (equal run (car key))))
      ;; Asked again under that execution, all is refused.
      (let ((superchat--tool-execution-context (list :run-id run)))
        (should (string-match-p
                 "needs a count first"
                 (plist-get (superchat-tools--edit-buffer-plan-more
                             ea--buffer "foo" "bar" nil t nil)
                            :final)))))))

(ert-deftest edit-all/a-count-is-forgotten-when-its-execution-fails ()
  "Every ending forgets, not only a successful one."
  (ea--with (ea--items 3)
      (list (ea--call '(old_string . "foo") '(new_string . "bar") '(count_only . t))
            '(:error (error . "EA provider down")))
    (let* ((view (smoke--open-chat))
           (run (ef--run view "count")))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (should-not (cl-loop for key being the hash-keys of superchat-tools--edit-counted
                           thereis (equal run (car key)))))))

(ert-deftest edit-all/an-ending-forgets-only-its-own-counts ()
  (let ((superchat-tools--edit-counted (make-hash-table :test #'equal)))
    (puthash '("run-a" "b" "foo" nil) t superchat-tools--edit-counted)
    (puthash '("run-b" "b" "foo" nil) t superchat-tools--edit-counted)
    (superchat-tools--forget-edit-counts nil '(:run-id "run-a" :status completed))
    (should (equal '(("run-b" "b" "foo" nil))
                   (hash-table-keys superchat-tools--edit-counted)))))

(provide 'edit-all-regression)
;;; edit-all-regression.el ends here
