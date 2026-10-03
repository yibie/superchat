;;; elisp-script-regression.el --- a restricted Emacs Lisp script calls tools -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 6.1 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-6-elisp-script.md): the
;; `elisp_script' tool runs a script that calls tools with (tool ...) and
;; reports with (out ...); only the output reaches the model; nested calls
;; go through the one tool pipeline; the language cannot loop forever or
;; change state except through tools.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/elisp-script-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^elisp-script/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-changes)
(require 'superchat-script)
(defvar superchat-display-style)

(defvar escr--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun escr--finder (dir)
  (cl-loop for root in escr--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun escr--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-script-" t))))

(defmacro escr--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((escr--roots nil)
          ,@(mapcar (lambda (v) `(,v (escr--temp-dir))) vars)
          (project-find-functions (list #'escr--finder))
          (superchat-display-style 'editor))
     (setq escr--roots (list ,@vars))
     (unwind-protect
         (let ((superchat-llm-script-only-tool-names
                '("list_buffers" "timer_list" "timer_cancel")))
           (hm--fixture ,script
             ;; `elisp_script' is not offered by default (slice 6.5): these
             ;; tests offer it, with three tools reached only from scripts.
             (setq superchat-llm-tool-names
                   (append (cl-set-difference superchat-llm-default-tool-names
                                              superchat-llm-script-only-tool-names
                                              :test #'equal)
                           '("elisp_script"))
                   superchat-llm-tools-list nil)
             ,@body))
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun escr--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun escr--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun escr--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun escr--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(defun escr--script-round (code)
  "A model round that runs CODE with `elisp_script'."
  `(:tool-uses (("elisp_script" (code . ,code)))))

(defun escr--result (n)
  "The text of the tool result the model received in the Nth request."
  (let ((tool-message (cl-find-if (lambda (m) (equal "tool" (alist-get 'role m)))
                                  (reverse (hm--wire n)))))
    (should tool-message)
    ;; llm.el puts this before every tool result it sends.
    (string-remove-prefix "Result of tool call is " (hm--text tool-message))))

(defun escr--tool-events (session)
  "SESSION's tool facts as (TYPE TOOL-NAME EXECUTION-ID)."
  (delq nil
        (mapcar (lambda (event)
                  (when (member (plist-get event :type)
                                '("tool.requested" "tool.completed" "tool.failed"))
                    (let ((payload (plist-get event :payload)))
                      (list (plist-get event :type)
                            (plist-get payload :tool-name)
                            (plist-get payload :execution-id)))))
                (superchat-tape-replay-events session))))

(defmacro escr--with-buffer (name text &rest body)
  "Run BODY with buffer NAME holding TEXT; kill it afterwards."
  (declare (indent 2))
  `(let ((buffer (get-buffer-create ,name)))
     (with-current-buffer buffer (erase-buffer) (insert ,text))
     (unwind-protect (progn ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer))))

;;;; The language ---------------------------------------------------------------------

(ert-deftest elisp-script/what-a-script-may-not-do-is-refused-before-it-runs ()
  (dolist (case '(("(while t 1)" . "while")
                  ("(dolist (x (number-sequence 1 10)) (out x))" . "number-sequence")
                  ("(delete-file \"/tmp/x\")" . "delete-file")
                  ("(setq foo 1)" . "foo")
                  ("(funcall (lambda () 1))" . "funcall")
                  ("(defun f () 1)" . "defun")
                  ("(cl-loop for i below 3 collect i)" . "cl-loop")))
    (let ((result (superchat-script-run (car case))))
      (should (string-prefix-p "Script refused: " result))
      (should (string-match-p (regexp-quote (format "`%s'" (cdr case))) result))
      (should (string-match-p "Nothing was run\\.\\'" result))))
  (should (string-match-p "\\`Script refused: the script cannot be read: "
                          (superchat-script-run "(out \"unclosed"))))

(ert-deftest elisp-script/plain-lisp-runs-and-reports ()
  (should (equal "Script completed. 0 tool calls.\n3\n(2 4)"
                 (superchat-script-run "(out (+ 1 2)) (mapcar (lambda (x) (* 2 x)) (list 1 2))")))
  (should (equal "Script completed. 0 tool calls.\na\nb"
                 (superchat-script-run "(dolist (s (list \"a\" \"b\")) (out s))")))
  (should (equal "Script completed. 0 tool calls.\n"
                 (superchat-script-run "(out \"\") nil"))))

(ert-deftest elisp-script/long-output-keeps-both-ends ()
  (let ((result (superchat-script-run
                 "(out (concat \"HEAD\" (mapconcat #'identity (mapcar (lambda (_) \"xxxxxxxxxx\") (append (make-sparse-keymap) nil)) \"\")))")))
    (should (stringp result)))
  (let* ((superchat-script-max-output 100)
         (result (superchat-script-run
                  "(out (concat \"HEAD-MARK\" (format \"%0200d\" 0) \"TAIL-MARK\"))")))
    (should (<= (length result) 160))
    (should (string-match-p "HEAD-MARK" result))
    (should (string-match-p "TAIL-MARK\\'" result))
    (should (string-match-p "characters cut from the middle" result))))

;;;; Nested calls ---------------------------------------------------------------------

(ert-deftest elisp-script/reading-and-editing-take-one-round-trip ()
  (escr--with-buffer "escr-notes" "alpha beta\n"
    (escr--with nil ()
      (setq smoke--script
            (list (escr--script-round
                   "(out (tool \"read_buffer\" :buffer \"escr-notes\"))
                    (tool \"edit_buffer\" :buffer_name \"escr-notes\" :old_string \"alpha\" :new_string \"gamma\")
                    (out \"EDITED\")")
                  '(:text "done")))
      (let* ((view (smoke--open-chat))
             (run (hm--turn view "change it"))
             (session (hm--session view)))
        ;; Two requests: the script, then the answer.
        (should (= 2 (length hm--wires)))
        (should (equal "gamma beta\n" (with-current-buffer "escr-notes" (buffer-string))))
        (let ((result (escr--result 2)))
          (should (string-prefix-p "Script completed. 2 tool calls.\n" result))
          (should (string-match-p "alpha beta" result))
          (should (string-match-p "EDITED\\'" result)))
        ;; Each nested call is a tool fact of its own, numbered under the script.
        (let* ((events (escr--tool-events session))
               (script-id (nth 2 (cl-find "elisp_script" events :key #'cadr :test #'equal)))
               (nested (cl-remove-if-not (lambda (e) (member (cadr e) '("read_buffer" "edit_buffer")))
                                         events)))
          (should (equal '(("tool.requested" "read_buffer") ("tool.completed" "read_buffer")
                           ("tool.requested" "edit_buffer") ("tool.completed" "edit_buffer"))
                         (mapcar (lambda (e) (list (car e) (cadr e))) nested)))
          (should (stringp script-id))
          (should (equal (list (concat script-id "/1") (concat script-id "/2"))
                         (delete-dups (mapcar #'caddr nested)))))
        ;; The edit is on the record of unsaved changes, unlike one made with eval_elisp.
        (should (member "escr-notes"
                        (mapcar (lambda (row) (plist-get row :name))
                                (superchat-changes-candidates session run))))))))

(ert-deftest elisp-script/only-the-output-reaches-the-model ()
  (escr--with-buffer "escr-big" (concat "SECRET-MIDDLE " (make-string 3000 ?z) "\nLAST-LINE\n")
    (escr--with nil ()
      (setq smoke--script
            (list (escr--script-round
                   "(let ((text (tool \"read_buffer\" :buffer \"escr-big\")))
                      (out (if (string-match-p \"LAST-LINE\" text) \"FOUND\" \"MISSING\")))")
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "look")
      (let ((all (mapconcat #'hm--text (hm--wire 2) "\n")))
        (should (string-match-p "FOUND" all))
        (should-not (string-match-p "SECRET-MIDDLE" all))))))

(ert-deftest elisp-script/a-failure-keeps-what-was-done-and-said ()
  (escr--with-buffer "escr-notes" "alpha\n"
    (escr--with nil ()
      (setq smoke--script
            (list (escr--script-round
                   "(tool \"edit_buffer\" :buffer_name \"escr-notes\" :old_string \"alpha\" :new_string \"gamma\")
                    (out \"BEFORE\")
                    (tool \"no_such_tool\")
                    (out \"AFTER\")")
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "go")
      (let ((result (escr--result 2)))
        (should (string-prefix-p "Script failed. 1 tool call.\nBEFORE\nScript error: No tool named no_such_tool" result))
        (should-not (string-match-p "AFTER" result)))
      (should (equal "gamma\n" (with-current-buffer "escr-notes" (buffer-string)))))))

(ert-deftest elisp-script/what-a-script-cannot-call ()
  (escr--with nil ()
    (setq smoke--script
          (list (escr--script-round "(tool \"elisp_script\" :code \"1\")")
                (escr--script-round "(tool \"read_buffer\" :buffer \"x\" :bogus 1)")
                (escr--script-round "(tool \"read_buffer\")")
                '(:text "done")))
    (hm--turn (smoke--open-chat) "go")
    (should (string-match-p "Script error: elisp_script cannot be called from a script" (escr--result 2)))
    (should (string-match-p "Script error: read_buffer has no argument :bogus; its arguments are :buffer" (escr--result 3)))
    (should (string-match-p "Script error: read_buffer needs :buffer" (escr--result 4)))))

(ert-deftest elisp-script/a-script-makes-at-most-a-hundred-calls ()
  (escr--with-buffer "escr-notes" "x\n"
    (escr--with nil ()
      (setq smoke--script
            (list (escr--script-round
                   (format "(dolist (i (list %s)) (tool \"read_buffer\" :buffer \"escr-notes\"))"
                           (mapconcat #'number-to-string (number-sequence 1 101) " ")))
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "go")
      (let ((result (escr--result 2)))
        (should (string-prefix-p "Script failed. 100 tool calls.\n" result))
        (should (string-match-p "Script error: More than 100 tool calls in one script" result))))))

(ert-deftest elisp-script/a-nested-call-is-checked-like-a-direct-one ()
  "eval_elisp has full control: under the default level it is asked about,
and a refused call does not run, from a script as from the model."
  (escr--with nil ()
    (let ((asked 0)
          (superchat-approval-function (lambda (&rest _) (setq asked (1+ asked)) nil)))
      (setq smoke--script
            (list (escr--script-round
                   "(out (tool \"eval_elisp\" :expression \"(setq escr--ran t)\"))")
                  '(:text "done")))
      (defvar escr--ran nil)
      (setq escr--ran nil)
      (hm--turn (smoke--open-chat) "go")
      (should-not escr--ran)
      (should (string-match-p "Script completed. 1 tool call.\n" (escr--result 2))))))

;;;; The tool itself ------------------------------------------------------------------

(ert-deftest elisp-script/it-is-a-built-in-not-offered-by-default ()
  "Measured on real tasks, scripts made the model slower and less reliable,
so `elisp_script' is offered only when the user adds it (slice 6.5)."
  (should-not (member "elisp_script" superchat-llm-default-tool-names))
  (should-not (default-value 'superchat-llm-script-only-tool-names))
  (should (eq 'builtin (superchat-sandbox-tool-class "elisp_script")))
  (should (eq 'full-control (superchat-sandbox-tool-class "eval_elisp"))))

;;;; Tools reached only from a script (slice 6.2) -------------------------------------

(ert-deftest elisp-script/script-only-tools-are-listed-in-its-description ()
  (escr--with nil ()
    (let* ((offered (mapcar #'llm-tool-name (superchat-get-llm-tools)))
           (script (cl-find "elisp_script" (superchat-get-llm-tools)
                            :key #'llm-tool-name :test #'equal))
           (description (llm-tool-description script)))
      (should (member "elisp_script" offered))
      (should-not (member "list_buffers" offered))
      (should (string-match-p "^Tools reached only from a script (they are not offered directly):$"
                              description))
      (should (string-match-p "^- list_buffers: List live Emacs buffers" description))
      (should (string-match-p "^- timer_cancel :id: Cancel a pending timer" description))
      (should-not (string-match-p "^- read_buffer" description))))
  ;; With no script-only tools, the description lists none.
  (let ((superchat-llm-script-only-tool-names nil)
        (superchat-llm-tool-names (append superchat-llm-default-tool-names '("elisp_script")))
        (superchat-llm-tools-list nil))
    (should-not (string-match-p "Tools reached only from a script"
                                (llm-tool-description
                                 (cl-find "elisp_script" (superchat-get-llm-tools)
                                          :key #'llm-tool-name :test #'equal))))))

(ert-deftest elisp-script/a-script-calls-a-tool-that-is-not-offered ()
  (escr--with-buffer "escr-listed" "x\n"
    (escr--with nil ()
      (setq smoke--script
            (list (escr--script-round
                   "(let ((rows (tool \"list_buffers\")))
                      (out (if (string-match-p \"escr-listed\" rows) \"LISTED\" \"ABSENT\")))")
                  '(:text "done")))
      (hm--turn (smoke--open-chat) "list")
      (should (equal "Script completed. 1 tool call.\nLISTED" (escr--result 2)))
      ;; The requests offered the script tool, not the listing tool.
      (let ((first-tools (plist-get (car (last smoke--requests)) :tools)))
        (should (member "elisp_script" first-tools))
        (should-not (member "list_buffers" first-tools))))))

;;;; Guidance (slice 6.3) --------------------------------------------------------------

(ert-deftest elisp-script/the-model-is-told-when-to-write-a-script ()
  (escr--with '((:text "ok") (:text "ok")) ()
    (hm--turn (smoke--open-chat) "hello")
    (let ((system (hm--text (car (hm--wire 1)))))
      (should (string-match-p (regexp-quote superchat-prompt-script-guidance-text) system))
      (should (string-match-p "even when one needs another's result" system)))
    ;; Without the script tool on the request, nothing is said about it.
    (let ((superchat-llm-tool-names
           (remove "elisp_script" superchat-llm-default-tool-names))
          (superchat-llm-tools-list nil))
      (hm--turn (smoke--open-chat) "hello again")
      (should-not (string-match-p "elisp_script" (hm--text (car (hm--wire 2))))))))

;;;; A wider language (slice 6.4) ------------------------------------------------------

(ert-deftest elisp-script/reads-the-real-runs-asked-for-are-allowed ()
  "The functions and macros scripts were refused for in the runs of
2026-10-03 run now: they only read."
  (let ((dir (file-name-as-directory (make-temp-file "escr-dir-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.JPG" dir) (insert "x"))
          (with-temp-file (expand-file-name "b.txt" dir) (insert "y"))
          (should (equal "Script completed. 0 tool calls.\n(\"a.JPG\")"
                         (superchat-script-run
                          (format "(let ((d (expand-file-name %S)))
                                     (out (seq-filter (lambda (f) (string-suffix-p \".JPG\" f))
                                                      (directory-files d nil \"\\\\`[^.]\"))))"
                                  dir))))
          (should (string-match-p "\\`Script completed\\. 0 tool calls\\.\n.*a\\.JPG"
                                  (superchat-script-run
                                   (format "(out (file-expand-wildcards %S))"
                                           (concat dir "*.JPG"))))))
      (delete-directory dir t)))
  (should (string-prefix-p "Script completed. 0 tool calls.\n"
                           (superchat-script-run
                            "(out (mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list)))")))
  (should (equal "Script completed. 0 tool calls.\n3"
                 (superchat-script-run "(when-let ((x 1) (y 2)) (out (+ x y)))")))
  (should (equal "Script completed. 0 tool calls.\nnone"
                 (superchat-script-run "(if-let* ((x nil)) (out x) (out \"none\"))"))))

(ert-deftest elisp-script/with-current-buffer-reads-and-the-buffer-is-put-back ()
  (escr--with-buffer "escr-other" "OTHER-TEXT\n"
    (with-temp-buffer
      (let ((here (current-buffer)))
        (should (equal "Script completed. 0 tool calls.\nOTHER-TEXT\n"
                       (superchat-script-run
                        "(with-current-buffer \"escr-other\" (out (buffer-substring-no-properties (point-min) (point-max))))")))
        ;; A bare set-buffer is undone when the script ends.
        (superchat-script-run "(set-buffer \"escr-other\")")
        (should (eq here (current-buffer)))))))

(ert-deftest elisp-script/what-stays-refused-says-how-to-change-things ()
  (let ((result (superchat-script-run "(rename-file \"a\" \"b\")")))
    (should (string-match-p "`rename-file' is not allowed in a script; a script may only read and compute -- make changes through a tool, or use eval_elisp" result)))
  (should (string-match-p "`require' is not allowed" (superchat-script-run "(require 'org)")))
  ;; A macro outside the list stays refused: it could expand into a loop.
  (should (string-match-p "`cl-loop' is not allowed" (superchat-script-run "(cl-loop for i below 3 collect i)")))
  (should (equal "Script refused: the script cannot be read: a parenthesis or a string is not closed.  Nothing was run."
                 (superchat-script-run "(out \"unclosed"))))

(ert-deftest elisp-script/a-tool-without-arguments-says-so ()
  (escr--with nil ()
    (setq smoke--script
          (list (escr--script-round "(tool \"list_buffers\" :filter \"x\")") '(:text "done")))
    (hm--turn (smoke--open-chat) "go")
    (should (string-match-p "Script error: list_buffers takes no arguments" (escr--result 2)))))

(provide 'elisp-script-regression)
;;; elisp-script-regression.el ends here
