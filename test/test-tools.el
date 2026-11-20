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
    (should-not (and (plist-get (plist-get entry :data) :error) (plist-get entry :text)))
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
    (let ((names (mapcar (lambda (s) (plist-get s :name)) all)))
      (dolist (n '("read" "grep" "find" "edit" "write" "eval_elisp" "shell"))
        (should (member n names)))
      ;; Coarser tools: these four are gone into read, edit and write.
      (dolist (n '("read_buffer" "list_buffers" "ls" "save_buffer"))
        (should-not (member n names)))))
  (let ((spec (car (superchat-tool-specs '("edit")))))
    (should (equal (plist-get spec :name) "edit"))
    (should (stringp (plist-get spec :description)))
    (should (equal (mapcar (lambda (a) (plist-get a :name)) (plist-get spec :args))
                   '("path" "buffer" "old" "new")))
    (should-not (plist-get (car (plist-get spec :args)) :required))
    (should (plist-get (nth 2 (plist-get spec :args)) :required)))
  ;; shell sends the model to the file tools for reading and searching.
  (let ((shell (plist-get (car (superchat-tool-specs '("shell"))) :description)))
    (dolist (word '("Never use it to read, list or search files" "read" "find" "grep"))
      (should (string-search word shell))))
  ;; grep, find and read steer the model away from extra calls.
  (let ((d (lambda (n) (plist-get (car (superchat-tool-specs (list n))) :description))))
    (should (string-search "(?i)" (funcall d "grep")))
    (should (string-search "context" (funcall d "grep")))
    (should (string-search "*diary*" (funcall d "find")))
    (should (string-search "limit 1" (funcall d "read"))))
  ;; The descriptions state the division of the tools.
  (dolist (n '("read" "grep" "find" "edit" "write"))
    (should (string-match-p "edit and write save the file.*shell.*eval_elisp"
                            (plist-get (car (superchat-tool-specs (list n))) :description))))
  (should (equal (mapcar (lambda (s) (plist-get s :name))
                         (superchat-tool-specs '("shell" "read")))
                 '("shell" "read")))
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

;; Overlapping counts are tested in edit; these cover the call itself.
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
    (let ((text (superchat-test-tools--err "edit" :buffer "x" :old "a")))
      (should (string-match-p "Missing required argument: new" text)))
    (should (string-match-p "path" (superchat-test-tools--err "read")))))

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
        (superchat-test-tools--ok "find" :pattern "*")
        (should (= before (length (superchat-tape-entries :tape "t" :kinds '(event)))))))))

(ert-deftest superchat-tools-ask-allow-deny-always ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'ask)
    (let ((answers '(deny allow always)) (asks 0))
      (let ((superchat-permission-ask-function
             (lambda (tool summary)
               (should (member tool '("eval_elisp" "shell")))
               ;; The question names the tool; the summary is the call's arguments.
               (should-not (string-match-p (regexp-quote tool) summary))
               (should (string-prefix-p "(:" summary))
               (should (equal (plist-get superchat-permission-asking :tape) "t"))
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
      ;; One event a question; a call that an earlier "always" allows asks
      ;; nobody and leaves none.
      (should (= (length permission-events) 4))
      (should (string-match-p "allowed" (plist-get (nth 1 permission-events) :text))))))

(ert-deftest superchat-tools-a-question-can-wait-for-its-answer ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'ask)
    (let* ((ran 0) (asks 0)
           (call (list :id "q1" :name "eval_elisp" :args '(:code "(setq ran (1+ ran)) \"ran\"")))
           (superchat-permission-ask-function (lambda (&rest _) (cl-incf asks) 'later))
           (entry (superchat-test-tools--await "t" call)))
      (ignore ran)
      (should (eq (plist-get entry :kind) 'event))
      (should (equal (plist-get entry :data)
                     (list :type "question" :call-id "q1" :tool "eval_elisp"
                           :summary (format "%S" (plist-get call :args)))))
      (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))
      (should-not (cl-find "permission" (superchat-tape-entries :tape "t" :kinds '(event))
                           :key (lambda (e) (plist-get (plist-get e :data) :type)) :test #'equal))
      ;; The answer on the Tape decides the same call; nobody is asked again.
      (superchat-permission-answer "t" "q1" 'allow)
      (let ((result (superchat-test-tools--await "t" (plist-put (copy-sequence call) :args '(:code "\"ran\"")))))
        (should (eq (plist-get result :kind) 'tool))
        (should (equal (plist-get result :text) "\"ran\"")))
      (should (= asks 1))
      (superchat-permission-answer "t" "q2" 'deny)
      (should (string-match-p "Permission denied"
                              (plist-get (superchat-test-tools--await
                                          "t" '(:id "q2" :name "eval_elisp" :args (:code "1")))
                                         :text))))))

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

(ert-deftest superchat-tools-result-is-appended-with-quitting-off ()
  (superchat-test-tools--in-scope
    (let (seen)
      (cl-letf* ((append (symbol-function 'superchat-tape-append))
                 ((symbol-function 'superchat-tape-append)
                  (lambda (tape kind &rest rest)
                    (when (eq kind 'tool) (push inhibit-quit seen))
                    (apply append tape kind rest))))
        (let ((inhibit-quit nil))
          (superchat-test-tools--ok "find" :pattern "*")))
      (should (equal seen '(t))))))

(ert-deftest superchat-tools-eval-elisp ()
  (superchat-test-tools--in-scope
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "(setq x 2) ; note\n(let ((y 3)) (* x y)) ; end")
                   "2\n6"))
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "\"a\nb\"") "\"a\\nb\""))
    (should (equal (superchat-test-tools--ok "eval_elisp" :code "\"s\"") "\"s\""))
    (should (equal (superchat-test-tools--ok "eval_elisp"
                                             :code "(funcall (let ((n 5)) (lambda () n)))")
                   "5"))
    (let ((text (superchat-test-tools--ok "eval_elisp" :code "(message \"hello %d\" 1)\n42")))
      (should (equal "hello 1\n\"hello 1\"\n42" text)))
    (should (string-match-p "boom" (superchat-test-tools--err "eval_elisp" :code "(error \"boom\")")))
    (let ((text (superchat-test-tools--err "eval_elisp" :code "(+ 1 2)\n(unclosed")))
      (should (string-match-p "Cannot read the code" text))
      (should-not (string-match-p "killed buffer" text)))))

(ert-deftest superchat-tools-eval-elisp-never-enters-the-debugger ()
  (superchat-test-tools--in-scope
    (let* ((entered nil)
           (debug-on-error t)
           ;; With it the debugger comes even for an error that is caught.
           (debug-on-signal t)
           (debugger (lambda (&rest _) (setq entered t))))
      (should (string-match-p "boom" (superchat-test-tools--err "eval_elisp" :code "(error \"boom\")")))
      ;; An error in a hook the code runs, as a user's mode hook would.
      (should (string-match-p
               "hooked"
               (superchat-test-tools--err
                "eval_elisp"
                :code "(with-temp-buffer (let ((text-mode-hook (list (lambda () (error \"hooked\"))))) (text-mode)))")))
      (should-not entered)
      ;; The code runs with the debugger off.
      (should (equal (superchat-test-tools--ok "eval_elisp"
                                               :code "(list debug-on-error debug-on-signal debug-on-quit)")
                     "(nil nil nil)")))))

(ert-deftest superchat-tools-shell ()
  (superchat-test-tools--in-scope
    (let ((text (superchat-test-tools--ok "shell" :command "echo hi; pwd")))
      (should (string-match-p "^hi$" text))
      (should (string-match-p (regexp-quote (directory-file-name dir)) text))
      (should (string-match-p "exit status 0" text)))
    (should (equal (superchat-test-tools--ok "shell" :command "exit 3") "[exit status 3]"))
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

(ert-deftest superchat-tools-long-results-are-cut ()
  (superchat-test-tools--in-scope
    (let* ((superchat-tool-max-chars 100)
           (text (superchat-test-tools--ok "eval_elisp" :code "(make-string 250 ?a)")))
      (should (string-match-p "\\[\\(152\\|153\\) characters left out\\]" text))
      (should (< (length text) 150)))
    (let ((text (superchat-test-tools--ok "eval_elisp" :code "(make-string 50 ?a)")))
      (should-not (string-match-p "left out" text)))))

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

(defun superchat-test-tools--kinds ()
  "Each entry of timeline \"t\" after the first: an event as its type."
  (mapcar (lambda (e) (if (eq (plist-get e :kind) 'event)
                          (plist-get (plist-get e :data) :type)
                        (symbol-name (plist-get e :kind))))
          (cdr (superchat-tape-entries :tape "t"))))

(ert-deftest superchat-tools-a-malformed-call-is-never-asked-about ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'ask)
    (let* ((asked 0)
           (superchat-permission-ask-function (lambda (&rest _) (cl-incf asked) 'allow)))
      (superchat-test-tools--err "frob")
      (superchat-test-tools--err "edit" :buffer "x")
      (should (= asked 0))
      (should (equal (superchat-test-tools--kinds) '("level" "level" "tool" "tool"))))))

(ert-deftest superchat-tools-a-quit-while-asked-leaves-no-permission-event ()
  (superchat-test-tools--in-scope
    (superchat-permission-set "t" 'ask)
    (let ((superchat-permission-ask-function (lambda (&rest _) (signal 'quit nil))))
      (superchat-test-tools--err "eval_elisp" :code "1")
      (should (equal (superchat-test-tools--kinds) '("level" "level" "tool"))))
    ;; An answer is recorded before the result, with what it allowed.
    (let ((superchat-permission-ask-function (lambda (&rest _) 'deny)))
      (superchat-test-tools--err "eval_elisp" :code "1")
      (should (equal (last (superchat-test-tools--kinds) 2) '("permission" "tool")))
      (should (eq (plist-get (plist-get (car (superchat-tape-entries :tape "t" :kinds '(event)
                                                                      :newest t :limit 1))
                                         :data)
                             :allowed)
                  nil)))))

(ert-deftest superchat-tools-cancelled-is-asked-also-with-no-question ()
  (superchat-test-tools--in-scope
    (let ((ran nil) entry)
      (superchat-tool-define "mark" :class 'read :function (lambda (_) (setq ran t) "ran"))
      (unwind-protect
          (superchat-tool-call "t" '(:id "c" :name "mark" :args nil)
                               (lambda (e) (setq entry e)) (lambda () t))
        (superchat-tool-remove "mark"))
      (should-not ran)
      (should (equal (plist-get entry :text) "Cancelled.")))))

(ert-deftest superchat-tools-async-signal-after-done-is-the-result ()
  (superchat-test-tools--in-scope
    (let (entries)
      (superchat-tool-define "both" :class 'read :async t
                             :function (lambda (_ done) (funcall done "fine") (error "then broke")))
      (unwind-protect
          (superchat-tool-call "t" '(:id "c" :name "both" :args nil)
                               (lambda (e) (push e entries)))
        (superchat-tool-remove "both"))
      (should (= (length entries) 1))
      (should (string-match-p "then broke" (plist-get (car entries) :text)))
      (should (eq (plist-get (plist-get (car entries) :data) :error) t)))))

(ert-deftest superchat-tools-a-read-outside-the-project-is-asked-about ()
  (superchat-test-tools--in-scope
    (let* ((outside (file-name-as-directory (make-temp-file "sc-outside-" t)))
           (secret (expand-file-name "secret" outside))
           asked)
      (unwind-protect
          (let ((superchat-permission-ask-function
                 (lambda (&rest _) (push t asked) 'deny)))
            (superchat-test-tools--write dir "in.txt" "inside")
            (with-temp-file secret (insert "key"))
            (make-symbolic-link secret (expand-file-name "link" dir))
            (superchat-permission-set "t" 'ask)
            (should (string-match-p "inside" (superchat-test-tools--ok "read" :path "in.txt")))
            (should-not asked)
            ;; An absolute path, `..', and a link inside that leads outside.
            (dolist (path (list secret (concat "../" (file-name-nondirectory
                                                       (directory-file-name outside))
                                                "/secret")
                                "link"))
              (setq asked nil)
              (should (string-match-p "outside the project"
                                      (superchat-test-tools--err "read" :path path)))
              (should asked))
            (should (string-match-p "outside the project"
                                    (plist-get (car (superchat-tape-entries
                                                     :tape "t" :kinds '(event) :newest t :limit 1))
                                               :text)))
            (setq asked nil)
            (superchat-permission-set "t" 'read-only)
            (superchat-test-tools--err "read" :path secret)
            (should-not asked)
            (superchat-permission-set "t" 'auto)
            (should (equal (superchat-test-tools--ok "read" :path secret) "key")))
        (delete-directory outside t)))))

(ert-deftest superchat-tools-at-home-every-path-is-outside ()
  (superchat-test-with-tape
    (let ((superchat-tools--always (make-hash-table :test #'equal)) asked)
      (superchat-tape-append "h" 'user "go" :scope "home")
      (let ((superchat-permission-ask-function (lambda (&rest _) (setq asked t) 'deny))
            (skill (expand-file-name "skills/s/SKILL.md" superchat-directory)))
        (superchat-tool-call "h" '(:id "c" :name "read" :args (:path "x")) #'ignore)
        (should asked)
        ;; A skill the system prompt offers is read with no question.
        (make-directory (file-name-directory skill) t)
        (with-temp-file skill (insert "a skill"))
        (setq asked nil)
        (superchat-tool-call "h" (list :id "d" :name "read" :args (list :path skill))
                             #'ignore)
        (should-not asked)))))

(ert-deftest superchat-tools-no-read-leaves-the-project-unasked ()
  (superchat-test-tools--in-scope
    (let* ((outside (file-name-as-directory (make-temp-file "sc-out-" t)))
           (asked 0)
           (superchat-permission-ask-function (lambda (&rest _) (cl-incf asked) 'deny)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "sub" outside))
            (with-temp-file (expand-file-name "secret" outside) (insert "KEY=hunter2"))
            (make-symbolic-link (expand-file-name "sub" outside) (expand-file-name "link" dir))
            (superchat-permission-set "t" 'ask)
            ;; Through a link and `..': the check and grep must see one path.
            (should-not (string-match-p "hunter2" (plist-get (superchat-test-tools--call
                                                              "grep" :pattern "KEY"
                                                              :path "link/../secret")
                                                             :text)))
            ;; A path that grep would take for an option.
            (should-not (string-match-p "hunter2" (plist-get (superchat-test-tools--call
                                                              "grep" :pattern "x"
                                                              :path (concat "-f" outside "secret"))
                                                             :text)))
            ;; The buffer of an outside file is outside too.
            (let ((b (find-file-noselect (expand-file-name "secret" outside))))
              (unwind-protect
                  (progn (setq asked 0)
                         (superchat-test-tools--err "read" :buffer (buffer-name b))
                         (should (= asked 1)))
                (kill-buffer b))))
        (delete-directory outside t)))))

(ert-deftest superchat-tools-at-home-a-search-of-the-default-path-is-asked ()
  (superchat-test-with-tape
    (let ((asked nil))
      (superchat-tape-append "h" 'user "go" :scope "home")
      (let ((superchat-permission-ask-function (lambda (&rest _) (setq asked t) 'deny)))
        (superchat-tool-call "h" '(:id "c" :name "grep" :args (:pattern "KEY"))
                             #'ignore)
        (should asked)))))

;;; read

(defun superchat-test-tools--numbers (from to)
  "The lines FROM to TO, each its own number, joined by newlines."
  (mapconcat #'number-to-string (number-sequence from to) "\n"))

(ert-deftest superchat-tools-read-returns-lines-without-numbers ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "one\ntwo\nthree\n")
    (should (equal (superchat-test-tools--ok "read" :path "a.txt") "one\ntwo\nthree"))
    (should (equal (superchat-test-tools--ok "read" :path "a.txt" :offset 2)
                   "two\nthree"))
    (should (equal (superchat-test-tools--ok "read" :path "a.txt" :offset 2 :limit 1)
                   "two\n\n[1 more lines in file. Use offset=3 to continue.]"))
    (should (equal (superchat-test-tools--err "read" :path "a.txt" :offset 9)
                   "Error: Offset 9 is beyond end of file (3 lines total)"))
    (superchat-test-tools--write dir "empty.txt" "")
    (should (equal (superchat-test-tools--ok "read" :path "empty.txt") ""))
    (should (superchat-test-tools--err "read" :path "missing.txt"))))

(ert-deftest superchat-tools-read-stops-at-2000-lines-and-names-the-next-offset ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "big.txt" (superchat-test-tools--numbers 1 2500))
    (let ((text (superchat-test-tools--ok "read" :path "big.txt")))
      (should (string-prefix-p (concat (superchat-test-tools--numbers 1 2000) "\n\n") text))
      (should (string-suffix-p "[Showing lines 1-2000 of 2500. Use offset=2001 to continue.]" text)))
    (should (equal (superchat-test-tools--ok "read" :path "big.txt" :offset 2001)
                   (superchat-test-tools--numbers 2001 2500)))
    ;; A limit over 2000 is stopped at 2000 all the same.
    (should (string-match-p "Showing lines 1-2000 of 2500"
                            (superchat-test-tools--ok "read" :path "big.txt" :limit 3000)))))

(ert-deftest superchat-tools-read-stops-at-the-character-limit-in-whole-lines ()
  (superchat-test-tools--in-scope
    (let ((superchat-tool-max-chars 1000))
      ;; Lines of 99 characters and a newline: 7 fit in the room of 750.
      (superchat-test-tools--write dir "w.txt" (mapconcat (lambda (_) (make-string 99 ?w))
                                                          (number-sequence 1 20) "\n"))
      (let ((text (superchat-test-tools--ok "read" :path "w.txt")))
        (should (equal text (concat (mapconcat (lambda (_) (make-string 99 ?w))
                                               (number-sequence 1 7) "\n")
                                    "\n\n[Showing lines 1-7 of 20 (750 characters limit). Use offset=8 to continue.]")))
        (should-not (string-match-p "left out" text)))
      ;; A single line longer than the room is cut, with a note.
      (superchat-test-tools--write dir "long.txt" (concat (make-string 2000 ?x) "\nnext\n"))
      (should (equal (superchat-test-tools--ok "read" :path "long.txt")
                     (concat (make-string 750 ?x)
                             "\n\n[Line 1 is 2000 characters; showing the first 750. Use offset=2 to continue.]")))
      (should (equal (superchat-test-tools--ok "read" :path "long.txt" :offset 2) "next")))))

(ert-deftest superchat-tools-read-sees-unsaved-edits-and-leaves-no-buffer ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "f.txt" "on disk\n")))
      (should (equal (superchat-test-tools--ok "read" :path "f.txt") "on disk"))
      (should-not (find-buffer-visiting file))
      (with-current-buffer (find-file-noselect file)
        (erase-buffer) (insert "in the buffer\n"))
      (should (equal (superchat-test-tools--ok "read" :path "f.txt") "in the buffer")))))

(ert-deftest superchat-tools-read-a-buffer ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-visible")))
      (unwind-protect
          (progn
            (with-current-buffer b (insert "alpha\nbeta\ngamma\n") (narrow-to-region 1 3))
            (should (equal (superchat-test-tools--ok "read" :buffer "sc-visible")
                           "alpha\nbeta\ngamma"))
            (should (equal (superchat-test-tools--ok "read" :buffer "sc-visible" :limit 1)
                           "alpha\n\n[2 more lines in buffer. Use offset=2 to continue.]"))
            (should (string-match-p "beyond end of buffer (3 lines total)"
                                    (superchat-test-tools--err "read" :buffer "sc-visible"
                                                               :offset 4)))
            (should (string-match-p "No buffer named nope"
                                    (superchat-test-tools--err "read" :buffer "nope")))
            (should (string-match-p "exactly one of path and buffer"
                                    (superchat-test-tools--err "read")))
            (should (string-match-p "exactly one of path and buffer"
                                    (superchat-test-tools--err "read" :buffer "sc-visible" :path "x"))))
        (kill-buffer b)))))

(ert-deftest superchat-tools-read-numbers-may-come-as-strings-or-floats ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "f.txt" "a\nb\nc\n")
    (should (equal (superchat-test-tools--ok "read" :path "f.txt" :offset "2" :limit "1")
                   "b\n\n[1 more lines in file. Use offset=3 to continue.]"))
    (should (equal (superchat-test-tools--ok "read" :path "f.txt" :offset 2.0 :limit 1.5)
                   "b\n\n[1 more lines in file. Use offset=3 to continue.]"))))

;;; grep, find and ls

(defun superchat-test-tools--tree (dir)
  "Files in DIR to search: a.txt, sub/b.txt, sub/deep/c.el, sub/.git/config.
DIR is no project: the .git is in a subdirectory."
  (superchat-test-tools--write dir "a.txt" "needle here\nnothing\nNEEDLE loud\n")
  (make-directory (expand-file-name "sub/deep" dir) t)
  (superchat-test-tools--write dir "sub/b.txt" "another needle\n")
  (superchat-test-tools--write dir "sub/deep/c.el" "(needle)\n")
  (make-directory (expand-file-name "sub/.git" dir))
  (superchat-test-tools--write dir "sub/.git/config" "needle in git\n"))

(ert-deftest superchat-tools-grep ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--tree dir)
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle")
                   "a.txt:1: needle here\nsub/b.txt:1: another needle\nsub/deep/c.el:1: (needle)"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :path "sub")
                   "b.txt:1: another needle\ndeep/c.el:1: (needle)"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :path "sub/b.txt")
                   "b.txt:1: another needle"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :ignore_case t :path "a.txt")
                   "a.txt:1: needle here\na.txt:3: NEEDLE loud"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :ignore_case nil :path "a.txt")
                   "a.txt:1: needle here"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle|nothing" :path "a.txt")
                   "a.txt:1: needle here\na.txt:2: nothing"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "(needle)" :literal t)
                   "sub/deep/c.el:1: (needle)"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "nothing" :context 1)
                   "a.txt-1- needle here\na.txt:2: nothing\na.txt-3- NEEDLE loud"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "zzz") "No matches found"))
    (should (string-match-p "No such file or directory: missing"
                            (superchat-test-tools--err "grep" :pattern "x" :path "missing")))))

(ert-deftest superchat-tools-grep-glob ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--tree dir)
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :glob "*.el")
                   "sub/deep/c.el:1: (needle)"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :glob "*.{el,txt}")
                   "a.txt:1: needle here\nsub/b.txt:1: another needle\nsub/deep/c.el:1: (needle)"))
    ;; With a slash the glob matches the path relative to PATH; * stays in a directory.
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :glob "sub/*")
                   "sub/b.txt:1: another needle"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle" :glob "sub/**/*.el")
                   "sub/deep/c.el:1: (needle)"))))

(ert-deftest superchat-tools-grep-and-find-respect-gitignore-in-the-project ()
  (superchat-test-tools--in-scope
    (let ((process-environment (cons "GIT_CONFIG_NOSYSTEM=1" process-environment)))
      (should (= 0 (call-process "git" nil nil nil "init" "-q" dir))))
    (superchat-test-tools--write dir ".gitignore" "ignored.txt\n")
    (superchat-test-tools--write dir "kept.txt" "needle\n")
    (superchat-test-tools--write dir "ignored.txt" "needle\n")
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle") "kept.txt:1: needle"))
    (should (equal (superchat-test-tools--ok "find" :pattern "*.txt") "kept.txt"))))

(ert-deftest superchat-tools-grep-skips-binary-files-and-links ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "needle text\n")
    (let ((coding-system-for-write 'no-conversion))
      (superchat-test-tools--write dir "bin.dat" (concat "needle\0\1\2binary" (make-string 10 0))))
    (make-symbolic-link (expand-file-name "a.txt" dir) (expand-file-name "link.txt" dir))
    (should (equal (superchat-test-tools--ok "grep" :pattern "needle") "a.txt:1: needle text"))))

(ert-deftest superchat-tools-grep-limit-and-long-lines ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "many.txt" (mapconcat (lambda (_) "hit") (number-sequence 1 150) "\n"))
    (let ((text (superchat-test-tools--ok "grep" :pattern "hit")))
      (should (= 100 (cl-count-if (lambda (l) (string-match-p "\\`many.txt:[0-9]+: hit\\'" l))
                                  (split-string text "\n"))))
      (should (string-suffix-p
               "\n\n[100 matches limit reached. Use limit=200 for more, or refine pattern]" text)))
    (should (string-suffix-p "many.txt:2: hit\n\n[2 matches limit reached. Use limit=4 for more, or refine pattern]"
                             (superchat-test-tools--ok "grep" :pattern "hit" :limit "2")))
    (superchat-test-tools--write dir "long.txt" (concat "wide " (make-string 600 ?w) "\n"))
    (should (equal (superchat-test-tools--ok "grep" :pattern "wide")
                   (concat "long.txt:1: wide " (make-string 495 ?w) "...\n\n"
                           "[Some lines truncated to 500 chars. Use read to see full lines]")))))

(ert-deftest superchat-tools-grep-character-limit ()
  (superchat-test-tools--in-scope
    (let ((superchat-tool-max-chars 1000))
      (superchat-test-tools--write dir "big.txt" (mapconcat (lambda (_) (make-string 80 ?x))
                                                            (number-sequence 1 50) "\n"))
      (let ((text (superchat-test-tools--ok "grep" :pattern "x")))
        (should (string-suffix-p "\n\n[750 characters limit reached]" text))
        (should (cl-every (lambda (l) (string-match-p "\\`big.txt:[0-9]+: x\\{80\\}\\'" l))
                          (split-string (car (split-string text "\n\n")) "\n")))
        (should-not (string-match-p "left out" text))))))

(ert-deftest superchat-tools-grep-does-not-block-emacs ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "a.txt" "needle\n")
    (let (got)
      (superchat-tool-call "t" '(:id "s" :name "grep" :args (:pattern "needle"))
                           (lambda (e) (setq got e)))
      (should-not got)
      (superchat-test-wait (lambda () got))
      (should (equal (plist-get got :text) "a.txt:1: needle")))))

(ert-deftest superchat-tools-grep-stop-kills-grep ()
  (superchat-test-tools--in-scope
    ;; grep blocks on reading a named pipe no one writes to.
    (call-process "mkfifo" nil nil nil (expand-file-name "pipe" dir))
    (let (got)
      (let ((stop (superchat-tool-call "t" '(:id "s" :name "grep" :args (:pattern "x"))
                                       (lambda (e) (setq got e)))))
        (should (= 1 (length (superchat-test-tools--live-processes))))
        (funcall stop)
        (should-not (superchat-test-tools--live-processes))
        (should-not got)))))

(ert-deftest superchat-tools-find ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--tree dir)
    (should (equal (superchat-test-tools--ok "find" :pattern "*.txt") "a.txt\nsub/b.txt"))
    (should (equal (superchat-test-tools--ok "find" :pattern "*.el" :path "sub") "deep/c.el"))
    (should (equal (superchat-test-tools--ok "find" :pattern "sub/*") "sub/b.txt\nsub/deep/"))
    (should (equal (superchat-test-tools--ok "find" :pattern "deep") "sub/deep/"))
    (should (equal (superchat-test-tools--ok "find" :pattern "config") "No files found matching pattern"))
    (should (equal (superchat-test-tools--ok "find" :pattern "*" :limit 2)
                   "a.txt\nsub/\n\n[2 results limit reached. Use limit=4 for more, or refine pattern]"))
    (should (string-match-p "Not a directory" (superchat-test-tools--err "find" :pattern "*"
                                                                         :path "a.txt")))))

(ert-deftest superchat-tools-read-a-directory-lists-it ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "b.txt" "")
    (superchat-test-tools--write dir "A.txt" "")
    (superchat-test-tools--write dir ".hidden" "")
    (make-directory (expand-file-name "c" dir))
    (should (equal (superchat-test-tools--ok "read" :path ".") ".hidden\nA.txt\nb.txt\nc/"))
    (should (equal (superchat-test-tools--ok "read" :path "c") "(empty directory)"))
    (should (equal (superchat-test-tools--ok "read" :path "." :limit 2)
                   ".hidden\nA.txt\n\n[2 entries limit reached. Use limit=4 for more]"))))

;;; edit and write

(ert-deftest superchat-tools-edit-a-buffer ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-edit")))
      (unwind-protect
          (progn
            (with-current-buffer b (buffer-enable-undo) (insert "foo bar baz\nbar\n") (undo-boundary))
            (should (string-match-p "2 times" (superchat-test-tools--err
                                               "edit" :buffer "sc-edit" :old "bar" :new "x")))
            (should (string-match-p "0 times" (superchat-test-tools--err
                                               "edit" :buffer "sc-edit" :old "qux" :new "x")))
            (should (equal (with-current-buffer b (buffer-string)) "foo bar baz\nbar\n"))
            (should (equal (superchat-test-tools--ok "edit" :buffer "sc-edit" :old "baz" :new "qux")
                           "Replaced 1 block in sc-edit."))
            (should (equal (with-current-buffer b (buffer-string)) "foo bar qux\nbar\n"))
            (with-current-buffer b
              (setq last-command nil)
              (undo)
              (should (equal (buffer-string) "foo bar baz\nbar\n")))
            (should (superchat-test-tools--err "edit" :buffer "nope" :old "a" :new "b"))
            (should (string-match-p "exactly one of path and buffer"
                                    (superchat-test-tools--err "edit" :old "a" :new "b")))
            (should (string-match-p "exactly one of path and buffer"
                                    (superchat-test-tools--err "edit" :buffer "sc-edit" :path "x"
                                                               :old "a" :new "b"))))
        (kill-buffer b)))))

(ert-deftest superchat-tools-edit-counts-exactly ()
  (superchat-test-tools--in-scope
    (let ((b (generate-new-buffer "sc-case")))
      (unwind-protect
          (progn
            (with-current-buffer b (insert "Foo and foo aaa") (setq-local case-fold-search t))
            (superchat-test-tools--ok "edit" :buffer "sc-case" :old "Foo" :new "Bar")
            (should (string-match-p "2 times" (superchat-test-tools--err
                                               "edit" :buffer "sc-case" :old "aa" :new "b")))
            (should (equal (with-current-buffer b (buffer-string)) "Bar and foo aaa")))
        (kill-buffer b)))))

(ert-deftest superchat-tools-edit-saves-the-file ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "f.txt" "hello\n")))
      (should (equal (superchat-test-tools--ok "edit" :path "f.txt" :old "hello" :new "bye")
                     "Replaced 1 block in f.txt."))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "bye\n"))
      ;; The buffer stays, saved, so the person can undo the edit there.
      (with-current-buffer (find-buffer-visiting file)
        (should-not (buffer-modified-p))
        (setq last-command nil)
        (undo)
        (should (equal (buffer-string) "hello\n"))
        (set-buffer-modified-p nil)
        (revert-buffer t t)))
    ;; A buffer visited for an edit that fails is not left open.
    (let ((file (superchat-test-tools--write dir "g.txt" "same same\n")))
      (superchat-test-tools--err "edit" :path "g.txt" :old "same" :new "x")
      (should-not (find-buffer-visiting file)))
    (should (string-match-p "No such file: none.txt"
                            (superchat-test-tools--err "edit" :path "none.txt" :old "a" :new "b")))))

(ert-deftest superchat-tools-edit-leaves-the-persons-unsaved-changes-unsaved ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "u.txt" "one two\n")))
      (with-current-buffer (find-file-noselect file) (goto-char (point-max)) (insert "mine\n"))
      (should (equal (superchat-test-tools--ok "edit" :path "u.txt" :old "two" :new "2")
                     (concat "Replaced 1 block in u.txt.\n\n[Not saved: the buffer held unsaved"
                             " changes of the person's; they save it]")))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "one two\n"))
      (with-current-buffer (find-buffer-visiting file)
        (should (equal (buffer-string) "one 2\nmine\n"))
        (set-buffer-modified-p nil)))))

(ert-deftest superchat-tools-a-save-that-changes-the-text-says-so ()
  (superchat-test-tools--in-scope
    (let ((file (superchat-test-tools--write dir "h.txt" "a  \n"))
          (before-save-hook (list (lambda () (when (equal (file-name-nondirectory buffer-file-name) "h.txt")
                                               (delete-trailing-whitespace))))))
      (should (string-match-p "Saving changed the text"
                              (superchat-test-tools--ok "edit" :path "h.txt" :old "a" :new "b")))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "b\n"))
      (should-not (string-match-p "Saving changed"
                                  (superchat-test-tools--ok "edit" :path "h.txt" :old "b" :new "c"))))))

(ert-deftest superchat-tools-a-save-shows-no-message ()
  (superchat-test-tools--in-scope
    (superchat-test-tools--write dir "q.txt" "a\n")
    ;; The "Wrote" message comes from C and is not seen in batch: the save
    ;; is checked to be silent where it writes.
    (let* ((write (symbol-function 'write-region)) (silent 'unset))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (&rest args) (setq silent save-silently) (apply write args))))
        (superchat-test-tools--ok "edit" :path "q.txt" :old "a" :new "b"))
      (should (eq silent t)))))

(ert-deftest superchat-tools-write-saves-the-file ()
  (superchat-test-tools--in-scope
    (let ((file (expand-file-name "new/dir/w.txt" dir)))
      (should (equal (superchat-test-tools--ok "write" :path "new/dir/w.txt" :content "first")
                     "Wrote 5 characters to new/dir/w.txt."))
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "first"))
      (superchat-test-tools--ok "write" :path "new/dir/w.txt" :content "second")
      (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "second"))
      (with-current-buffer (find-buffer-visiting file)
        (setq last-command nil)
        (undo)
        (should (equal (buffer-string) "first"))
        (set-buffer-modified-p nil)))
    ;; An empty new file is written too, and a save leaves no backup file,
    ;; whatever the person's Emacs says.
    (let ((make-backup-files t) (backup-enable-predicate (lambda (_) t)))
      (superchat-test-tools--ok "write" :path "e.txt" :content "")
      (superchat-test-tools--ok "write" :path "e.txt" :content "x"))
    (should (file-exists-p (expand-file-name "e.txt" dir)))
    (should-not (file-exists-p (expand-file-name "e.txt~" dir)))))

;;; shell

(ert-deftest superchat-tools-shell-keeps-the-end-and-saves-the-whole-output ()
  (superchat-test-tools--in-scope
    (let ((temporary-file-directory (file-name-as-directory (expand-file-name "tmp" dir))))
      (make-directory temporary-file-directory)
      (let* ((text (superchat-test-tools--ok "shell" :command "seq 1 2500"))
             (notes (split-string (cadr (split-string text "\n\n")) "\n")))
        (should (string-prefix-p (concat (superchat-test-tools--numbers 501 2500) "\n\n") text))
        (should (string-match "\\`\\[Showing lines 501-2500 of 2500. Full output: \\(.*\\)\\]\\'"
                              (car notes)))
        (should (equal (with-temp-buffer (insert-file-contents (match-string 1 (car notes)))
                                         (buffer-string))
                       (concat (superchat-test-tools--numbers 1 2500) "\n")))
        (should (equal (cadr notes) "[exit status 0]")))
      (should (equal (superchat-test-tools--ok "shell" :command "seq 1 3")
                     "1\n2\n3\n\n[exit status 0]")))))

(ert-deftest superchat-tools-shell-output-is-never-held-without-limit ()
  (superchat-test-tools--in-scope
    (let ((temporary-file-directory (file-name-as-directory (expand-file-name "tmp" dir)))
          (superchat-tool-max-chars 1000))
      (make-directory temporary-file-directory)
      (let* ((text (superchat-test-tools--ok "shell" :command "seq 1 30000; echo last; exit 4"))
             (file (and (string-match "Full output: \\(.*\\)\\]" text) (match-string 1 text))))
        (should (< (length text) 1000))
        (should (string-match-p "last\n\n\\[Showing lines [0-9]+-30001 of 30001\\. Full output: " text))
        (should (string-suffix-p "\n[exit status 4]" text))
        (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                       (concat (superchat-test-tools--numbers 1 30000) "\nlast\n"))))
      (should-not (superchat-test-tools--live-processes)))))

(ert-deftest superchat-tools-shell-timeout-argument ()
  (superchat-test-tools--in-scope
    (let ((start (float-time)))
      (should (equal (superchat-test-tools--ok "shell" :command "echo early; sleep 5" :timeout "1")
                     "early\n\n[timed out after 1 seconds and killed]"))
      (should (< (- (float-time) start) 4))
      (should-not (superchat-test-tools--live-processes)))))

(ert-deftest superchat-tools-shell-stops-a-command-whose-output-has-no-end ()
  (superchat-test-tools--in-scope
    (let* ((superchat-tool-max-chars 4000)
           (temporary-file-directory (file-name-as-directory (expand-file-name "tmp" dir)))
           (text (progn (make-directory temporary-file-directory)
                        (plist-get (superchat-test-tools--call "shell" :command "yes" :timeout 5)
                                   :text))))
      (should (string-match-p "output passed [0-9]+ characters; the command was stopped" text))
      (should (string-match "Full output: \\([^]]+\\)\\]" text))
      (should (< (file-attribute-size (file-attributes (match-string 1 text))) (* 250 4000))))))

(provide 'test-tools)
;;; test-tools.el ends here
