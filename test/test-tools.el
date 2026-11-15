;;; test-tools.el --- tests for superchat-tools -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-permission)
(require 'superchat-tools)

(defmacro superchat-test-tools--in-scope (&rest body)
  "Run BODY with timeline \"t\" at level auto, scoped to a temporary directory DIR."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((superchat-tools--always (make-hash-table :test #'equal))
           (dir (file-name-as-directory (file-truename (make-temp-file "sc-scope-" t)))))
       (unwind-protect
           (progn (superchat-tape-append "t" 'user "go" :scope dir)
                  (superchat-permission-set "t" 'auto)
                  ,@body)
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b) (string-prefix-p dir (buffer-file-name b)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (delete-directory dir t)))))

(defun superchat-test-tools--await (tape call)
  "Run CALL on TAPE and wait for its entry."
  (let (entry)
    (superchat-tool-call tape call (lambda (e) (setq entry e)))
    (superchat-test-wait (lambda () entry))
    entry))

(defun superchat-test-tools--call (name &rest args)
  "Call tool NAME with ARGS on timeline \"t\"; the tool entry."
  (superchat-test-tools--await "t" (list :id "c1" :name name :args args)))

(defun superchat-test-tools--ok (name &rest args)
  "The text of a successful call."
  (let ((entry (apply #'superchat-test-tools--call name args)))
    (should-not (plist-get (plist-get entry :data) :error))
    (plist-get entry :text)))

(defun superchat-test-tools--err (name &rest args)
  "The text of a failed call."
  (let ((entry (apply #'superchat-test-tools--call name args)))
    (should (eq (plist-get (plist-get entry :data) :error) t))
    (plist-get entry :text)))

(defun superchat-test-tools--write (dir name text)
  (let ((file (expand-file-name name dir)))
    (with-temp-file file (insert text))
    file))

(defun superchat-test-tools--live-processes ()
  (cl-remove-if-not (lambda (p) (string-prefix-p "superchat-process" (process-name p)))
                    (process-list)))

(ert-deftest superchat-tools-specs-shape ()
  (let ((all (superchat-tool-specs)))
    (dolist (n '("read_file" "read_buffer" "list_buffers" "search_files" "edit_buffer"
                 "write_file" "save_buffer" "eval_elisp" "shell"))
      (should (assoc n (mapcar (lambda (s) (cons (plist-get s :name) s)) all)))))
  (let ((spec (car (superchat-tool-specs '("edit_buffer")))))
    (should (equal (plist-get spec :name) "edit_buffer"))
    (should (stringp (plist-get spec :description)))
    (should (equal (mapcar (lambda (a) (plist-get a :name)) (plist-get spec :args))
                   '("buffer" "old" "new")))
    (should (plist-get (car (plist-get spec :args)) :required)))
  (should (equal (mapcar (lambda (s) (plist-get s :name))
                         (superchat-tool-specs '("shell" "read_file")))
                 '("shell" "read_file")))
  (should-error (superchat-tool-specs '("nope"))))

(ert-deftest superchat-tools-define-and-remove ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "echo" :class 'read :description "Echo"
                           :args '((:name "x" :type string :description "d" :required t))
                           :function (lambda (a) (plist-get a :x)))
    (should (equal (superchat-test-tools--ok "echo" :x "hi") "hi"))
    (should (member "echo" (mapcar (lambda (s) (plist-get s :name)) (superchat-tool-specs))))
    (superchat-tool-remove "echo")
    (should-not (member "echo" (mapcar (lambda (s) (plist-get s :name)) (superchat-tool-specs))))
    (should (string-match-p "Unknown tool: echo" (superchat-test-tools--err "echo" :x "hi")))
    (should-error (superchat-tool-define "bad" :class 'nonsense :function #'ignore))))

(ert-deftest superchat-tools-description-may-be-a-function ()
  (let ((n 0))
    (superchat-tool-define "dyn" :class 'read :function #'ignore
                           :description (lambda () (format "call %d" (cl-incf n))))
    (unwind-protect
        (progn
          (should (equal (plist-get (car (superchat-tool-specs '("dyn"))) :description) "call 1"))
          (should (equal (plist-get (car (superchat-tool-specs '("dyn"))) :description) "call 2")))
      (superchat-tool-remove "dyn"))))

(ert-deftest superchat-tools-tool-tape-is-bound-while-running ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "which" :class 'read :function (lambda (_) superchat-tool-tape))
    (unwind-protect
        (progn (should (equal (superchat-test-tools--ok "which") "t"))
               (should-not superchat-tool-tape))
      (superchat-tool-remove "which"))))

(ert-deftest superchat-tools-quit-becomes-entry ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "stop" :class 'read :function (lambda (_) (signal 'quit nil)))
    (unwind-protect
        (should (equal (superchat-test-tools--err "stop") "Interrupted by the user."))
      (superchat-tool-remove "stop"))
    (superchat-permission-set "t" 'ask)
    (let ((superchat-permission-ask-function (lambda (&rest _) (signal 'quit nil))))
      (should (equal (superchat-test-tools--err "eval_elisp" :code "1") "Interrupted by the user.")))))

;; Overlapping counts are tested in edit-buffer; these cover the call itself.
(ert-deftest superchat-tools-non-string-result-is-an-error ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "num" :class 'read :function (lambda (_) 42))
    (superchat-tool-define "nothing" :class 'read :function (lambda (_) nil))
    (unwind-protect
        (progn (should (string-match-p "42" (superchat-test-tools--err "num")))
               (should (string-match-p "nil" (superchat-test-tools--err "nothing"))))
      (superchat-tool-remove "num") (superchat-tool-remove "nothing"))))

(ert-deftest superchat-tools-required-argument-present-whatever-its-value ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "flag" :class 'read
                           :args '((:name "on" :type boolean :description "d" :required t))
                           :function (lambda (a) (format "%S" (plist-get a :on))))
    (unwind-protect
        (progn (should (equal (superchat-test-tools--ok "flag" :on nil) "nil"))
               (should (string-match-p "Missing" (superchat-test-tools--err "flag"))))
      (superchat-tool-remove "flag"))))

(ert-deftest superchat-tools-quit-is-allowed-while-the-tool-runs ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "seen" :class 'read
                           :function (lambda (_) (format "%S" inhibit-quit)))
    (superchat-tool-define "stop" :class 'read :function (lambda (_) (signal 'quit nil)))
    (unwind-protect
        (let ((inhibit-quit t))
          (should (equal (superchat-test-tools--ok "seen") "nil"))
          (should (equal (superchat-test-tools--err "stop") "Interrupted by the user."))
          (should inhibit-quit))
      (superchat-tool-remove "seen") (superchat-tool-remove "stop"))))

(ert-deftest superchat-tools-call-entry-shape ()
  (superchat-test-tools--in-scope
    (let ((entry (superchat-test-tools--await "t" '(:id "id9" :name "eval_elisp" :args (:code "(+ 1 2)")))))
      (should (eq (plist-get entry :kind) 'tool))
      (should (equal (plist-get entry :tape) "t"))
      (should (equal (plist-get entry :data) '(:call-id "id9" :name "eval_elisp" :error nil)))
      (should (equal (plist-get entry :text) "3")))))

(ert-deftest superchat-tools-unknown-and-missing-argument ()
  (superchat-test-tools--in-scope
    (should (string-match-p "Unknown tool: frob" (superchat-test-tools--err "frob")))
    (let ((text (superchat-test-tools--err "edit_buffer" :buffer "x" :old "a")))
      (should (string-match-p "Missing required argument: new" text)))
    (should (string-match-p "path" (superchat-test-tools--err "read_file")))))

(ert-deftest superchat-tools-function-error-becomes-entry ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "boom" :class 'read :function (lambda (_) (error "It broke")))
    (should (string-match-p "It broke" (superchat-test-tools--err "boom")))
    (superchat-tool-remove "boom")))

(ert-deftest superchat-tools-permission-levels ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'read-only)
    (let ((asked nil)
          (superchat-permission-ask-function (lambda (&rest _) (setq asked t) 'allow)))
      (should (string-match-p "Permission denied" (superchat-test-tools--err "eval_elisp" :code "1")))
      (let* ((events (superchat-tape-entries :tape "t" :kinds '(event)))
             (last-event (car (last events)))
             (tools (superchat-tape-entries :tape "t" :kinds '(tool))))
        (should (equal (plist-get (plist-get last-event :data) :type) "permission"))
        (should (string-match-p "eval_elisp.*exec.*deny" (plist-get last-event :text)))
        (should (< (plist-get last-event :id) (plist-get (car tools) :id))))
      (should-not asked)
      (let ((before (length (superchat-tape-entries :tape "t" :kinds '(event)))))
        (superchat-test-tools--ok "list_buffers")
        (should (= before (length (superchat-tape-entries :tape "t" :kinds '(event)))))))))

(ert-deftest superchat-tools-ask-allow-deny-always ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'ask)
    (let ((answers '(deny allow always)) (asks 0))
      (let ((superchat-permission-ask-function
             (lambda (tool summary)
               (should (member tool '("eval_elisp" "shell")))
               (should (string-match-p (regexp-quote tool) summary))
               (cl-incf asks) (pop answers))))
        (should (string-match-p "Permission denied" (superchat-test-tools--err "eval_elisp" :code "1")))
        (should (equal (superchat-test-tools--ok "eval_elisp" :code "1") "1"))
        (should (equal (superchat-test-tools--ok "eval_elisp" :code "2") "2"))
        (should (equal (superchat-test-tools--ok "eval_elisp" :code "3") "3"))
        (should (= asks 3))
        (should (equal (superchat-test-tools--ok "eval_elisp" :code "4") "4"))
        (should (= asks 3))
        ;; always is per tool: another exec tool still asks
        (setq answers '(deny))
        (superchat-test-tools--err "shell" :command "true")
        (should (= asks 4))))
    (let ((permission-events
           (cl-remove-if-not (lambda (e) (equal (plist-get (plist-get e :data) :type) "permission"))
                             (superchat-tape-entries :tape "t" :kinds '(event)))))
      (should (>= (length permission-events) 4))
      (should (string-match-p "allowed" (plist-get (nth 1 permission-events) :text))))))

(ert-deftest superchat-tools-always-is-per-timeline ()
  (superchat-test-tools--in-scope
    (superchat-tape-append "u" 'user "go" :scope dir)
    (superchat-permission-set "t" 'ask)
    (superchat-permission-set "u" 'ask)
    (let* ((asks 0)
           (superchat-permission-ask-function (lambda (&rest _) (cl-incf asks) 'always)))
      (superchat-test-tools--await "t" '(:id "a" :name "eval_elisp" :args (:code "1")))
      (superchat-test-tools--await "t" '(:id "b" :name "eval_elisp" :args (:code "1")))
      (should (= asks 1))
      (superchat-test-tools--await "u" '(:id "c" :name "eval_elisp" :args (:code "1")))
      (should (= asks 2)))))

(ert-deftest superchat-tools-scope-is-default-directory ()
  (superchat-test-tools--in-scope
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "default-directory")
                   (prin1-to-string dir)))
    (superchat-tape-append "h" 'user "go" :scope "home")
    (superchat-permission-set "h" 'auto)
    (should (equal (plist-get (superchat-test-tools--await
                                "h" '(:id "x" :name "eval_elisp" :args (:code "default-directory")))
                              :text)
                   (prin1-to-string (expand-file-name "~/"))))))

(ert-deftest superchat-tools-read-file ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "one\ntwo\nthree\n")
    (let ((text (superchat-test-tools--ok "read_file" :path "a.txt")))
      (should (string-match-p "Lines 1-3 of 3" text))
      (should (string-match-p "2\ttwo" text)))
    (let ((text (superchat-test-tools--ok "read_file" :path "a.txt" :start 2 :count 1)))
      (should (string-match-p "Lines 2-2 of 3" text))
      (should-not (string-match-p "three" text)))
    (should (string-match-p "Nothing at line 9"
                            (superchat-test-tools--ok "read_file" :path "a.txt" :start 9)))
    (should (superchat-test-tools--err "read_file" :path "missing.txt"))
    (superchat-test-tools--write
     dir "big.txt" (mapconcat #'number-to-string (number-sequence 1 1000) "\n"))
    (let ((text (superchat-test-tools--ok "read_file" :path "big.txt")))
      (should (string-match-p "Lines 1-400 of 1000" text))
      (should-not (string-match-p "\n401\t" text)))
    (should (string-match-p "Lines 401-800 of 1000"
                            (superchat-test-tools--ok "read_file" :path "big.txt"
                                                      :start 401 :count 9999)))))

(ert-deftest superchat-tools-read-buffer-and-list-buffers ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-visible")) (hidden (generate-new-buffer " sc-hidden")))
      (unwind-protect
          (progn
            (with-current-buffer b (insert "alpha\nbeta\n"))
            (let ((text (superchat-test-tools--ok "read_buffer" :buffer "sc-visible")))
              (should (string-match-p "Lines 1-2 of 2" text))
              (should (string-match-p "1\talpha" text)))
            (should (string-match-p "No buffer named nope"
                                    (superchat-test-tools--err "read_buffer" :buffer "nope")))
            (let ((text (superchat-test-tools--ok "list_buffers")))
              (should (string-match-p "sc-visible\tfundamental-mode\t-\t11" text))
              (should-not (string-match-p "sc-hidden" text))))
        (kill-buffer b) (kill-buffer hidden)))))

(ert-deftest superchat-tools-search-files ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "needle here\nnothing\n")
    (make-directory (expand-file-name "sub" dir))
    (superchat-test-tools--write dir "sub/b.txt" "another needle\n")
    (let ((text (superchat-test-tools--ok "search_files" :pattern "needle")))
      (should (string-match-p "a.txt:1:needle here" text))
      (should (string-match-p "b.txt:1:another needle" text)))
    (let ((text (superchat-test-tools--ok "search_files" :pattern "needle" :path "sub")))
      (should (string-match-p "b.txt" text))
      (should-not (string-match-p "a.txt" text)))
    (should (string-match-p "No matches" (superchat-test-tools--ok "search_files" :pattern "zzz")))
    (superchat-test-tools--write dir "many.txt" (mapconcat (lambda (_) "hit") (number-sequence 1 150) "\n"))
    (let ((text (superchat-test-tools--ok "search_files" :pattern "hit" :path "many.txt")))
      (should (string-match-p "more matching lines not shown" text))
      (should (= 100 (length (seq-filter (lambda (l) (string-match-p ":hit$" l))
                                         (split-string text "\n"))))))))

(ert-deftest superchat-tools-read-lines-given-as-floats ()
  ;; JSON numbers may arrive as floats.
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "one\ntwo\nthree\n")
    (let ((text (superchat-test-tools--ok "read_file" :path "a.txt" :start 2.0 :count 1.0)))
      (should (string-match-p "Lines 2-2 of 3" text))
      (should-not (string-match-p "three" text)))
    (let ((b (generate-new-buffer "sc-floats")))
      (unwind-protect
          (progn (with-current-buffer b (insert "alpha\nbeta\n"))
                 (should (string-match-p "Lines 2-2 of 2"
                                         (superchat-test-tools--ok "read_buffer" :buffer "sc-floats"
                                                                   :start 2.0 :count 1.5))))
        (kill-buffer b)))))

(ert-deftest superchat-tools-search-files-skips-git-and-binary-files ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "needle text\n")
    (make-directory (expand-file-name ".git" dir))
    (superchat-test-tools--write dir ".git/config" "needle in git\n")
    (let ((coding-system-for-write 'no-conversion))
      (superchat-test-tools--write dir "bin.dat" (concat "needle\0\1\2binary" (make-string 10 0))))
    (let ((text (superchat-test-tools--ok "search_files" :pattern "needle")))
      (should (string-match-p "a.txt:1:needle text" text))
      (should-not (string-match-p "\\.git" text))
      (should-not (string-match-p "bin.dat" text)))))

(ert-deftest superchat-tools-result-is-appended-with-quitting-off ()
  (superchat-test-tools--in-scope
    (let (seen)
      (cl-letf* ((append (symbol-function 'superchat-tape-append))
                 ((symbol-function 'superchat-tape-append)
                  (lambda (tape kind &rest rest)
                    (when (eq kind 'tool) (push inhibit-quit seen))
                    (apply append tape kind rest))))
        (let ((inhibit-quit nil))
          (superchat-test-tools--ok "list_buffers")))
      (should (equal seen '(t))))))

(ert-deftest superchat-tools-edit-buffer ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-edit")))
      (unwind-protect
          (progn
            (with-current-buffer b (buffer-enable-undo) (insert "foo bar baz\nbar\n") (undo-boundary))
            (let ((text (superchat-test-tools--err "edit_buffer" :buffer "sc-edit" :old "bar" :new "x")))
              (should (string-match-p "2 times" text)))
            (should (string-match-p "0 times" (superchat-test-tools--err
                                               "edit_buffer" :buffer "sc-edit" :old "qux" :new "x")))
            (should (equal (with-current-buffer b (buffer-string)) "foo bar baz\nbar\n"))
            (should (string-match-p "Replaced" (superchat-test-tools--ok
                                                "edit_buffer" :buffer "sc-edit" :old "baz" :new "qux")))
            (should (equal (with-current-buffer b (buffer-string)) "foo bar qux\nbar\n"))
            (with-current-buffer b
              (should-not buffer-file-name)
              (setq last-command nil)
              (undo)
              (should (equal (buffer-string) "foo bar baz\nbar\n")))
            (should (superchat-test-tools--err "edit_buffer" :buffer "nope" :old "a" :new "b")))
        (kill-buffer b)))))

(ert-deftest superchat-tools-edit-buffer-counts-exactly ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-case")))
      (unwind-protect
          (progn
            (with-current-buffer b (insert "Foo and foo aaa") (setq-local case-fold-search t))
            (should (string-match-p "Replaced" (superchat-test-tools--ok
                                                "edit_buffer" :buffer "sc-case" :old "Foo" :new "Bar")))
            (should (string-match-p "2 times" (superchat-test-tools--err
                                               "edit_buffer" :buffer "sc-case" :old "aa" :new "b")))
            (should (equal (with-current-buffer b (buffer-string)) "Bar and foo aaa")))
        (kill-buffer b)))))

(ert-deftest superchat-tools-edit-buffer-does-not-save ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "f.txt" "hello\n")))
      (with-current-buffer (find-file-noselect file)
        (superchat-test-tools--ok "edit_buffer" :buffer (buffer-name) :old "hello" :new "bye")
        (should (buffer-modified-p)))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "hello\n")))))

(ert-deftest superchat-tools-write-file-saves-through-buffer-and-undoes ()
  (superchat-test-tools--in-scope
    (let ((file (expand-file-name "new/dir/w.txt" dir)))
      (should (string-match-p "Wrote" (superchat-test-tools--ok
                                       "write_file" :path "new/dir/w.txt" :content "first")))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "first"))
      (superchat-test-tools--ok "write_file" :path "new/dir/w.txt" :content "second")
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "second"))
      (with-current-buffer (find-buffer-visiting file)
        (should-not (buffer-modified-p))
        (setq last-command nil)
        (undo)
        (should (equal (buffer-string) "first"))))))

(ert-deftest superchat-tools-save-buffer ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "s.txt" "a\n")))
      (with-current-buffer (find-file-noselect file)
        (insert "b")
        (should (string-match-p "Saved" (superchat-test-tools--ok "save_buffer" :buffer (buffer-name))))
        (should-not (buffer-modified-p)))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "ba\n")))
    (let ((b (generate-new-buffer "sc-nofile")))
      (unwind-protect
          (should (string-match-p "visits no file"
                                  (superchat-test-tools--err "save_buffer" :buffer "sc-nofile")))
        (kill-buffer b)))))

(ert-deftest superchat-tools-eval-elisp ()
  (superchat-test-tools--in-scope
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "(setq x 2) ; note\n(let ((y 3)) (* x y)) ; end")
                   "6"))
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "\"s\"") "\"s\""))
    (should (equal (superchat-test-tools--ok "eval_elisp"
                                             :code "(funcall (let ((n 5)) (lambda () n)))")
                   "5"))
    (let ((text (superchat-test-tools--ok "eval_elisp" :code "(message \"hello %d\" 1)\n42")))
      (should (string-match-p "\\`hello 1\n42\\'" text)))
    (should (string-match-p "boom" (superchat-test-tools--err "eval_elisp" :code "(error \"boom\")")))
    (let ((text (superchat-test-tools--err "eval_elisp" :code "(+ 1 2)\n(unclosed")))
      (should (string-match-p "Cannot read the code" text))
      (should-not (string-match-p "killed buffer" text)))))

(ert-deftest superchat-tools-shell ()
  (superchat-test-tools--in-scope
    (let ((text (superchat-test-tools--ok "shell" :command "echo hi; pwd")))
      (should (string-match-p "^hi$" text))
      (should (string-match-p (regexp-quote (directory-file-name dir)) text))
      (should (string-match-p "exit status 0" text)))
    (should (equal (superchat-test-tools--ok "shell" :command "exit 3") "\n[exit status 3]"))
    (let ((start (float-time)))
      (should (string-match-p "exit status 0" (superchat-test-tools--ok "shell" :command "cat")))
      (should (< (- (float-time) start) 5)))
    (should (string-match-p "oops" (superchat-test-tools--ok "shell" :command "echo oops >&2")))))

(ert-deftest superchat-tools-shell-time-limit ()
  (superchat-test-tools--in-scope
    (let ((superchat-shell-seconds 1) (start (float-time)))
      (should (string-match-p "timed out after 1 seconds"
                              (superchat-test-tools--ok "shell" :command "echo early; sleep 5")))
      (should (< (- (float-time) start) 4))
      (should-not (superchat-test-tools--live-processes)))))

(defmacro superchat-test-tools--with-async (&rest body)
  "BODY with async tool `later': DONE from a 0.1 s timer; its stop function
sets `stopped' and cancels the timer."
  (declare (indent 0))
  `(let ((stopped nil))
     (superchat-tool-define
      "later" :class 'read :async t
      :function (lambda (_ done)
                  (let ((timer (run-at-time 0.1 nil (lambda () (funcall done "late")))))
                    (lambda () (setq stopped t) (cancel-timer timer)))))
     (unwind-protect (progn ,@body)
       (superchat-tool-remove "later"))))

(ert-deftest superchat-tools-async-result-comes-later ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--with-async
      (let (got)
        (let ((stop (superchat-tool-call "t" '(:id "a" :name "later" :args nil)
                                         (lambda (e) (push e got)))))
          (should (functionp stop))
          (should-not got)
          (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))
          (superchat-test-wait (lambda () got))
          (should (equal (plist-get (car got) :text) "late"))
          (should (equal (plist-get (car got) :data) '(:call-id "a" :name "later" :error nil)))
          (should (equal (superchat-tape-entries :tape "t" :kinds '(tool)) got))
          (should-not stopped)
          ;; A stop after the result does nothing.
          (funcall stop)
          (should-not stopped))))))

(ert-deftest superchat-tools-async-error-and-bad-results ()
  (superchat-test-tools--in-scope
    (let (done-fn)
      (superchat-tool-define "err" :class 'read :async t
                             :function (lambda (_ done) (setq done-fn done) nil))
      (superchat-tool-define "boom" :class 'read :async t
                             :function (lambda (_ _done) (error "No start")))
      (superchat-tool-define "num" :class 'read :async t
                             :function (lambda (_ done) (funcall done 42) nil))
      (superchat-tool-define "sync-done" :class 'read :async t
                             :function (lambda (_ done) (funcall done nil "right away") nil))
      (superchat-tool-define "twice" :class 'read :async t
                             :function (lambda (_ done) (funcall done "one") (funcall done "two") nil))
      (unwind-protect
          (progn
            (let (got)
              (should (functionp (superchat-tool-call "t" '(:id "e" :name "err" :args nil)
                                                      (lambda (e) (push e got)))))
              (funcall done-fn nil "it failed")
              (funcall done-fn "again")
              (should (= (length got) 1))
              (should (equal (plist-get (car got) :text) "Error: it failed"))
              (should (eq (plist-get (plist-get (car got) :data) :error) t)))
            (should (string-match-p "No start" (superchat-test-tools--err "boom")))
            (should (string-match-p "42" (superchat-test-tools--err "num")))
            (should (equal (superchat-test-tools--err "sync-done") "Error: right away"))
            (should (equal (superchat-test-tools--ok "twice") "one"))
            (should-not (cl-find "two" (superchat-tape-entries :tape "t" :kinds '(tool))
                                 :key (lambda (e) (plist-get e :text)) :test #'equal)))
        (dolist (n '("err" "boom" "num" "sync-done" "twice")) (superchat-tool-remove n))))))

(ert-deftest superchat-tools-async-result-callback-signal-is-not-the-tools-error ()
  (superchat-test-tools--in-scope
    (superchat-tool-define "now" :class 'read :async t
                           :function (lambda (_ done) (funcall done "fine") nil))
    (unwind-protect
        (progn
          (should-error (superchat-tool-call "t" '(:id "a" :name "now" :args nil)
                                             (lambda (_) (error "mine"))))
          (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(tool))))))
      (superchat-tool-remove "now"))))

(ert-deftest superchat-tools-async-stop-before-the-result ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--with-async
      (let ((got nil))
        (let ((stop (superchat-tool-call "t" '(:id "a" :name "later" :args nil)
                                         (lambda (e) (push e got)))))
          (funcall stop)
          (should stopped))
        (let ((end (+ (float-time) 0.3)))
          (while (< (float-time) end) (accept-process-output nil 0.05)))
        (should-not got)
        (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))))
    ;; A tool that ignores the stop: its late DONE is dropped all the same.
    (let (done-fn got)
      (superchat-tool-define "deaf" :class 'read :async t
                             :function (lambda (_ done) (setq done-fn done) nil))
      (unwind-protect
          (let ((stop (superchat-tool-call "t" '(:id "d" :name "deaf" :args nil)
                                           (lambda (e) (push e got)))))
            (funcall stop)
            (funcall done-fn "late")
            (should-not got)
            (should-not (superchat-tape-entries :tape "t" :kinds '(tool))))
        (superchat-tool-remove "deaf")))))

(ert-deftest superchat-tools-async-binds-the-tape-and-directory-while-starting ()
  (superchat-test-tools--in-scope
    (let (seen)
      (superchat-tool-define "peek" :class 'read :async t
                             :function (lambda (_ done)
                                         (setq seen (list superchat-tool-tape default-directory inhibit-quit))
                                         (funcall done "x") nil))
      (unwind-protect
          (let ((inhibit-quit t))
            (superchat-test-tools--ok "peek")
            (should (equal seen (list "t" dir nil))))
        (superchat-tool-remove "peek")))))

(ert-deftest superchat-tools-async-denied-and-missing-do-not-start ()
  (superchat-test-tools--in-scope
    (let ((started nil))
      (superchat-tool-define "w" :class 'write :async t
                             :args '((:name "x" :type string :required t))
                             :function (lambda (_ done) (setq started t) (funcall done "no") nil))
      (unwind-protect
          (progn
            (should (string-match-p "Missing" (superchat-test-tools--err "w")))
            (superchat-permission-set "t" 'read-only)
            (should (string-match-p "denied" (superchat-test-tools--err "w" :x "1")))
            (should-not started))
        (superchat-tool-remove "w")))))

(ert-deftest superchat-tools-shell-does-not-block-emacs ()
  (superchat-test-tools--in-scope
    (let (got fired)
      (let ((stop (superchat-tool-call "t" '(:id "s" :name "shell" :args (:command "sleep 0.3; echo hi"))
                                       (lambda (e) (setq got e)))))
        (should (functionp stop))
        (should-not got)
        (run-at-time 0.05 nil (lambda () (setq fired (not got))))
        (superchat-test-wait (lambda () got))
        (should fired))
      (should (equal (plist-get got :text) "hi\n\n[exit status 0]"))
      (should-not (superchat-test-tools--live-processes)))))

(ert-deftest superchat-tools-shell-stop-kills-the-process ()
  (superchat-test-tools--in-scope
    (let (got)
      (let ((stop (superchat-tool-call "t" '(:id "s" :name "shell" :args (:command "sleep 3"))
                                       (lambda (e) (setq got e)))))
        (should (= 1 (length (superchat-test-tools--live-processes))))
        (funcall stop)
        (should-not (superchat-test-tools--live-processes))
        (let ((end (+ (float-time) 0.2)))
          (while (< (float-time) end) (accept-process-output nil 0.05)))
        (should-not got)
        (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))))))

(ert-deftest superchat-tools-shell-runs-in-the-scope ()
  (superchat-test-tools--in-scope
    (let ((default-directory temporary-file-directory))
      (should (string-match-p (regexp-quote (directory-file-name dir))
                              (superchat-test-tools--ok "shell" :command "pwd"))))))

(ert-deftest superchat-tools-search-does-not-block-emacs ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "needle\n")
    (let (got)
      (superchat-tool-call "t" '(:id "s" :name "search_files" :args (:pattern "needle"))
                           (lambda (e) (setq got e)))
      (should-not got)
      (superchat-test-wait (lambda () got))
      (should (string-match-p "a.txt:1:needle" (plist-get got :text))))
    (should (string-match-p "grep exited" (superchat-test-tools--err "search_files" :pattern "x"
                                                                     :path "missing")))))

(ert-deftest superchat-tools-search-stop-kills-grep ()
  (superchat-test-tools--in-scope
    (let (got)
      (let ((stop (superchat-tool-call "t" '(:id "s" :name "search_files" :args (:pattern "x"))
                                       (lambda (e) (setq got e)))))
        (should (= 1 (length (superchat-test-tools--live-processes))))
        (funcall stop)
        (should-not (superchat-test-tools--live-processes))
        (should-not got)))))

(ert-deftest superchat-tools-long-results-are-cut ()
  (superchat-test-tools--in-scope
    (let* ((superchat-tool-max-chars 100)
           (text (superchat-test-tools--ok "eval_elisp" :code "(make-string 250 ?a)")))
      (should (string-match-p "\\[\\(152\\|153\\) characters left out\\]" text))
      (should (< (length text) 150)))
    (let ((text (superchat-test-tools--ok "eval_elisp" :code "(make-string 50 ?a)")))
      (should-not (string-match-p "left out" text)))))

(ert-deftest superchat-tools-output-is-never-held-without-limit ()
  (superchat-test-tools--in-scope
    (let ((superchat-tool-max-chars 100) (superchat-shell-seconds 5) (start (float-time)))
      (let ((text (superchat-test-tools--ok "shell" :command "yes 0123456789")))
        (should (< (- (float-time) start) 3))
        (should (string-match-p "cut" text))
        (should (< (length text) 400)))
      (should-not (superchat-test-tools--live-processes)))))

(ert-deftest superchat-tools-search-output-is-cut-too ()
  (superchat-test-tools--in-scope
    (let ((superchat-tool-max-chars 100))
      (with-temp-file (expand-file-name "big.txt" dir) (insert (make-string 5000 ?x) "\n"))
      (let ((text (superchat-test-tools--ok "search_files" :pattern "x")))
        (should (string-match-p "cut" text))
        (should (< (length text) 600))))))

(ert-deftest superchat-tools-a-process-that-cannot-start-leaves-no-buffer ()
  (superchat-test-tools--in-scope
    (let ((shell-file-name "/nonexistent/sh") (before (buffer-list)))
      (should (plist-get (plist-get (superchat-test-tools--call "shell" :command "echo hi") :data)
                         :error))
      (should-not (seq-find (lambda (b) (and (not (memq b before))
                                             (string-match-p "superchat-process" (buffer-name b))))
                            (buffer-list))))
    (delete-directory dir)
    (let ((before (buffer-list)))
      (should (plist-get (plist-get (superchat-test-tools--call "shell" :command "echo hi") :data)
                         :error))
      (should-not (seq-find (lambda (b) (and (not (memq b before))
                                             (string-match-p "superchat-process" (buffer-name b))))
                            (buffer-list))))
    (make-directory dir)))

(ert-deftest superchat-tools-search-files-cut-keeps-the-line-cap ()
  (superchat-test-tools--in-scope
    ;; A small limit makes the output cut at 8000 characters, first.
    (let ((superchat-tool-max-chars 2000))
      (superchat-test-tools--write dir "big.txt"
                                   (mapconcat (lambda (_) "hit") (number-sequence 1 3000) "\n"))
      (let ((text (superchat-test-tools--ok "search_files" :pattern "hit" :path "big.txt")))
        (should (string-match-p "output cut" text))
        (should (= 100 (length (seq-filter (lambda (l) (string-match-p ":hit$" l))
                                           (split-string text "\n")))))))))

(ert-deftest superchat-tools-shell-notes-survive-the-cut ()
  (superchat-test-tools--in-scope
    (let ((text (superchat-test-tools--ok
                 "shell" :command "head -c 30000 /dev/zero | tr '\\0' z; exit 3")))
      (should (string-match-p "exit status 3" text)))
    (let ((superchat-shell-seconds 1))
      (should (string-match-p
               "timed out after 1 seconds"
               (superchat-test-tools--ok
                "shell" :command "head -c 30000 /dev/zero | tr '\\0' z; sleep 5"))))))

(provide 'test-tools)
;;; test-tools.el ends here
