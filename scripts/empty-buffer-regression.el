;;; empty-buffer-regression.el --- edit_buffer writes the text of an empty buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Decision of 2026-11-09
;; (docs/decisions/accepted/2026-11-09-edit-buffer-writes-an-empty-buffer.md):
;; when the target buffer's accessible text is empty and old_string is the
;; empty string, `edit_buffer' makes new_string the buffer's text, through
;; the same permission path, unsaved-change mark, undo step and answer as
;; any other edit.  The Lisp check of 2026-09-19 applies: unbalanced text is
;; not written into an empty Lisp buffer.  An empty old_string on a buffer
;; with text is still an error, and its words say why and what to do.
;;
;; On 2026-11-09 a model made 206 `eval_elisp' calls and no `edit_buffer'
;; call, and wrote a 20 KB Emacs Lisp file as a Lisp string inside a JSON
;; string, because `edit_buffer' could not put text into an empty buffer.
;;
;; Every test runs the real tool loop with a scripted model.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/empty-buffer-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^empty-buffer/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-changes)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defmacro eb--fixture (mode script &rest body)
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
           (superchat-changes-review-after-run nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defmacro eb--with-dir (dir &rest body)
  "Bind DIR to a fresh directory; kill its buffers and delete it after BODY."
  (declare (indent 1) (debug t))
  `(let ((,dir (file-name-as-directory (file-truename (make-temp-file "eb-" t)))))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p ,dir (file-truename file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory ,dir t))))

(defun eb--edit (buffer old new &rest more)
  "A model round that calls `edit_buffer' on BUFFER with OLD, NEW and MORE."
  `(:tool-uses (("edit_buffer" (buffer_name . ,buffer) (old_string . ,old)
                 (new_string . ,new) ,@more))))

(defun eb--run (view text)
  "Send TEXT from VIEW and wait for its Run to end; return the Run."
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun eb--outcomes (view tool)
  "The `tool.completed' and `tool.failed' events of TOOL in VIEW's Session."
  (cl-remove-if-not
   (lambda (event)
     (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
          (equal tool (plist-get (plist-get event :payload) :tool-name))))
   (superchat-tape-replay-events
    (buffer-local-value 'superchat--session-id view))))

(defun eb--prompt (n)
  "The prompt text of the Nth request the scripted model received, from 1."
  (plist-get (nth (- (length smoke--requests) n) smoke--requests) :prompt-text))

(defun eb--text (buffer)
  (with-current-buffer buffer
    (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max)))))

(defun eb--lisp (lines)
  "LINES lines of balanced Emacs Lisp, ending in a newline."
  (let (out)
    (dotimes (i (/ lines 3))
      (push (format "(defun eb-generated-%d (x)\n  \"Return X plus %d; a ) in a string.\"\n  (+ x %d))\n"
                    i i i)
            out))
    (apply #'concat (nreverse out))))

(defconst eb--notes "first line\nsecond line\nthird line\n")

(ert-deftest empty-buffer/an-empty-old-string-writes-the-whole-text-of-an-empty-buffer ()
  "Same permission path (no question at edit-buffers), unsaved-change mark,
undo step and record as any other edit; the answer says what was written
and shows the lines."
  (eb--with-dir dir
    (let ((file (expand-file-name "notes.txt" dir)))
      (eb--fixture 'edit-buffers
          (list (eb--edit "notes.txt" "" eb--notes) '(:text "EB-DONE"))
        (let* ((buffer (find-file-noselect file))
               (asked 0)
               (superchat-approval-function (lambda (_request) (cl-incf asked) t))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (eb--run view "write the notes")))
          (should (= 0 asked))
          (should (equal eb--notes (eb--text buffer)))
          (should (buffer-modified-p buffer))
          (should-not (file-exists-p file))
          (let ((outcomes (eb--outcomes view "edit_buffer")))
            (should (equal '("tool.completed") (mapcar (lambda (e) (plist-get e :type)) outcomes)))
            (should (equal "notes.txt" (plist-get (plist-get (car outcomes) :payload) :buffer))))
          ;; Like one replacement, not like `all': no `edit.applied' fact.
          (should-not (member "edit.applied" (smoke--types session)))
          (should (member "notes.txt" (mapcar (lambda (row) (plist-get row :name))
                                              (superchat-changes-candidates session run))))
          (let ((answer (eb--prompt 2)))
            (should (string-match-p (regexp-quote "Wrote 3 lines, 34 characters, into the empty buffer notes.txt. This tool does not save or evaluate the buffer.") answer))
            (should (string-match-p (regexp-quote "Layer: in memory only; the buffer was not saved or evaluated") answer))
            (should (string-match-p (regexp-quote "Now, lines 1-3:\n1: first line\n2: second line\n3: third line") answer)))
          ;; One undo step takes the buffer back to empty and unmodified.
          (with-current-buffer buffer
            (let ((last-command nil))
              (undo-start)
              (undo-more 1))
            (should (equal "" (buffer-string)))
            (should-not (buffer-modified-p))))))))

(ert-deftest empty-buffer/the-permission-level-judges-it-as-an-edit ()
  "Under read-only an edit is refused; writing an empty buffer is an edit."
  (eb--fixture 'read-only
      (list (eb--edit "eb-scratch" "" "text\n") '(:text "EB-DONE"))
    (let ((buffer (get-buffer-create "eb-scratch")))
      (eb--run (smoke--open-chat) "write it")
      (should (string-match-p (regexp-quote "[Tool call denied") (eb--prompt 2)))
      (should (equal "" (eb--text buffer)))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest empty-buffer/unbalanced-lisp-is-not-written-into-an-empty-lisp-buffer ()
  "The check of 2026-09-19: an empty buffer reads as Lisp, the text would
not, so the call fails with that check's words and nothing changes."
  (eb--with-dir dir
    (let ((file (expand-file-name "new.el" dir)))
      (eb--fixture 'edit-buffers
          (list (eb--edit "new.el" "" "(defun a ()\n  (list 1 2)\n") '(:text "EB-DONE"))
        (let* ((buffer (find-file-noselect file))
               (undo-before (buffer-local-value 'buffer-undo-list buffer))
               (view (smoke--open-chat)))
          (should (with-current-buffer buffer (derived-mode-p 'emacs-lisp-mode)))
          (eb--run view "write it")
          (should (equal "" (eb--text buffer)))
          (should-not (buffer-modified-p buffer))
          (should (eq undo-before (buffer-local-value 'buffer-undo-list buffer)))
          (let ((outcome (car (eb--outcomes view "edit_buffer"))))
            (should (equal "tool.failed" (plist-get outcome :type)))
            (should (equal "lisp-unbalanced" (plist-get (plist-get outcome :payload) :error-type)))
            (should-not (plist-get (plist-get outcome :payload) :buffer)))
          (let ((answer (eb--prompt 2)))
            (should (string-match-p (regexp-quote "Error: edit not applied; buffer new.el was not changed. It reads as Lisp now, and after this edit it would not") answer))
            (should (string-match-p "1 parenthesis it opens and never closes" answer))))))))

(ert-deftest empty-buffer/nothing-into-an-empty-buffer-is-no-change ()
  (eb--fixture 'edit-buffers
      (list (eb--edit "eb-scratch" "" "") '(:text "EB-DONE"))
    (let ((buffer (get-buffer-create "eb-scratch"))
          (view (smoke--open-chat)))
      (eb--run view "nothing")
      (should (string-match-p (regexp-quote "No change: buffer eb-scratch is empty and new_string is empty.")
                              (eb--prompt 2)))
      (should (equal '("tool.completed")
                     (mapcar (lambda (e) (plist-get e :type)) (eb--outcomes view "edit_buffer"))))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest empty-buffer/a-buffer-with-text-says-why-and-what-to-do ()
  "An empty old_string on a buffer that has text is still an error; it says
when old_string may be empty, how much text there is, and how to add at
the end."
  (eb--fixture 'edit-buffers
      (list (eb--edit "eb-scratch" "" "more\n") '(:text "EB-DONE"))
    (let ((buffer (get-buffer-create "eb-scratch"))
          (view (smoke--open-chat)))
      (with-current-buffer buffer (insert eb--notes) (set-buffer-modified-p nil))
      (eb--run view "add")
      (let ((answer (eb--prompt 2)))
        (should (string-match-p (regexp-quote "Error: old_string must be nonempty here: it may be empty only when the buffer is empty, and buffer eb-scratch has 34 characters.") answer))
        (should (string-match-p (regexp-quote "Read it and give text that occurs once.") answer))
        (should (string-match-p (regexp-quote "To add text at the end, replace the last line with that line followed by the new text.") answer)))
      (let ((outcome (car (eb--outcomes view "edit_buffer"))))
        (should (equal "tool.failed" (plist-get outcome :type)))
        (should (equal "empty-old-string" (plist-get (plist-get outcome :payload) :error-type))))
      (should (equal eb--notes (eb--text buffer)))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest empty-buffer/regexp-all-and-count-only-are-as-before ()
  "With regexp, all or count_only an empty old_string fails as it did, in an
empty buffer too.  One Run each: three failed calls in a row end a Run."
  (eb--fixture 'edit-buffers
      (list (eb--edit "eb-scratch" "" "x" '(regexp . t)) '(:text "EB-1")
            (eb--edit "eb-scratch" "" "x" '(all . t)) '(:text "EB-2")
            (eb--edit "eb-scratch" "" "x" '(count_only . t)) '(:text "EB-3"))
    (let ((buffer (get-buffer-create "eb-scratch"))
          (view (smoke--open-chat)))
      (dotimes (_ 3) (eb--run view "try"))
      (let ((outcomes (eb--outcomes view "edit_buffer")))
        (should (= 3 (length outcomes)))
        (dolist (outcome outcomes)
          (should (equal "tool.failed" (plist-get outcome :type)))
          (should (equal "Error: old_string must be nonempty; read the buffer and supply unique text."
                         (plist-get (plist-get outcome :payload) :result-text)))))
      (should (equal "" (eb--text buffer)))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest empty-buffer/the-descriptions-say-so ()
  "The tool's description and old_string's description each say it."
  (let ((superchat-llm-tool-names superchat-llm-default-tool-names))
    (superchat-llm-tools-reload)
    (let* ((tool (cl-find "edit_buffer" superchat-llm-tools-list
                          :key #'llm-tool-name :test #'equal))
           (old (cl-find "old_string" (llm-tool-args tool)
                         :key (lambda (arg) (plist-get arg :name)) :test #'equal)))
      (should (string-match-p (regexp-quote "an empty old_string only writes new_string into an empty buffer")
                              (llm-tool-description tool)))
      (should (string-match-p (regexp-quote "Empty only for an empty buffer, to write new_string as its whole text.")
                              (plist-get old :description))))))

(ert-deftest empty-buffer/a-nested-call-from-eval-does-the-same ()
  (eb--fixture 'danger-full-access
      (list '(:tool-uses (("eval_elisp" (expression . "(superchat-tool \"edit_buffer\" :buffer_name \"eb-scratch\" :old_string \"\" :new_string \"one\\ntwo\\n\")"))))
            '(:text "EB-DONE"))
    (let* ((buffer (get-buffer-create "eb-scratch"))
           (view (smoke--open-chat)))
      (eb--run view "write it")
      (should (equal "one\ntwo\n" (eb--text buffer)))
      (should (buffer-modified-p buffer))
      (should (string-match-p (regexp-quote "Wrote 2 lines, 8 characters, into the empty buffer eb-scratch.")
                              (eb--prompt 2)))
      (let ((nested (car (eb--outcomes view "edit_buffer"))))
        (should (equal "tool.completed" (plist-get nested :type)))
        (should (equal "eb-scratch" (plist-get (plist-get nested :payload) :buffer)))
        (should (string-search "/" (plist-get (plist-get nested :payload) :execution-id)))))))

(ert-deftest empty-buffer/the-incident-a-new-lisp-file-in-one-call ()
  "The shape of 2026-11-09: an empty buffer visiting a new .el file.  The
text with one parenthesis missing is refused and leaves it empty and
unmodified; the balanced 300 lines go in with one call."
  (eb--with-dir dir
    (let* ((file (expand-file-name "export.el" dir))
           (good (eb--lisp 300))
           ;; The last closing parenthesis of line 150's form taken out.
           (bad (let ((lines (split-string good "\n")))
                  (setf (nth 149 lines) (string-remove-suffix ")" (nth 149 lines)))
                  (string-join lines "\n"))))
      (should (= 300 (length (split-string good "\n" t))))
      (eb--fixture 'edit-buffers
          (list (eb--edit "export.el" "" bad)
                (eb--edit "export.el" "" good)
                '(:text "EB-DONE"))
        (let* ((buffer (find-file-noselect file))
               (view (smoke--open-chat))
               (session (buffer-local-value 'superchat--session-id view))
               (run (eb--run view "write the exporter")))
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (let ((refused (eb--prompt 2)))
            (should (string-match-p (regexp-quote "Error: edit not applied; buffer export.el was not changed.") refused))
            (should (string-match-p "1 parenthesis it opens and never closes" refused)))
          (should (equal good (eb--text buffer)))
          (should (buffer-modified-p buffer))
          (should-not (file-exists-p file))
          (with-current-buffer buffer
            (should (derived-mode-p 'emacs-lisp-mode))
            (should-not (superchat-tools--lisp-read-problem)))
          (let ((answer (eb--prompt 3)))
            (should (string-match-p (regexp-quote (format "Wrote 300 lines, %d characters, into the empty buffer export.el." (length good))) answer))
            (should (string-match-p (regexp-quote "Now, lines 1-300:\n1: (defun eb-generated-0 (x)") answer))
            (should (string-match-p (regexp-quote "300:   (+ x 99))") answer)))
          (should (equal '("tool.failed" "tool.completed")
                         (mapcar (lambda (e) (plist-get e :type)) (eb--outcomes view "edit_buffer"))))
          (should (= 1 (length (superchat-changes-candidates session run)))))))))

(ert-deftest empty-buffer/a-new-file-opened-says-how-to-fill-it ()
  "The model's whole route: `open_file' on a path that does not exist tells
it the buffer is empty and how `edit_buffer' fills it; the fill lands in
the buffer, and nothing is written to disk.  `open_file' is not on the
default surface since 2026-10-27, so the test offers it."
  (eb--with-dir dir
    (let ((file (expand-file-name "fresh.el" dir))
          (text "(defun fresh-one ()\n  \"One.\"\n  1)\n"))
      (eb--fixture 'edit-buffers
          (list `(:tool-uses (("open_file" (path . ,file))))
                (eb--edit "fresh.el" "" text)
                '(:text "EB-DONE"))
        (let* ((superchat-llm-tool-names
                (append superchat-llm-default-tool-names '("open_file")))
               (view (smoke--open-chat)))
          (eb--run view "make the file")
          (should (string-match-p
                   (regexp-quote "The buffer is empty: edit_buffer with an empty old_string writes its whole text.")
                   (eb--prompt 2)))
          (let ((buffer (get-file-buffer file)))
            (should buffer)
            (should (equal text (eb--text buffer)))
            (should (buffer-modified-p buffer)))
          (should-not (file-exists-p file))
          (should (equal '("tool.completed")
                         (mapcar (lambda (e) (plist-get e :type)) (eb--outcomes view "edit_buffer")))))))))

(provide 'empty-buffer-regression)

;;; empty-buffer-regression.el ends here
