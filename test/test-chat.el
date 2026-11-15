;;; test-chat.el --- tests for superchat-chat -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-tools)
(require 'superchat-plugin)
(require 'superchat-chat)

(defvar superchat-test-chat--dir nil
  "The directory made for the running test; the only one the macro deletes.")

(defun superchat-test-chat--end-runs ()
  "Cancel every run a test left on the Tape, so the next test finds it free."
  (dolist (row (superchat-tape-select "SELECT DISTINCT tape FROM entry"))
    (when (superchat-run-active (car row)) (superchat-cancel (car row)))))

(defmacro superchat-test-chat--with (&rest body)
  "BODY with a Tape, in a directory made here, outside any project, with a read
tool `echo', and no chat buffers left behind."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((superchat-test-chat--dir (file-name-as-directory (make-temp-file "sc-chat-" t)))
           (superchat-chat--commands superchat-chat--commands)
           (superchat-plugin--definitions superchat-plugin--definitions)
           (superchat-plugin--enabled superchat-plugin--enabled))
       (superchat-tool-define "echo" :class 'read :description "Echo"
                              :args '((:name "x" :type string :required t))
                              :function (lambda (a) (concat "echo:" (plist-get a :x) "\nsecond line")))
       (unwind-protect
           (save-window-excursion
             (with-temp-buffer
               (setq default-directory superchat-test-chat--dir)
               ,@body))
         (superchat-tool-remove "echo")
         (superchat-test-chat--end-runs)
         (dolist (b (buffer-list))
           (when (buffer-local-value 'superchat-chat--tape b) (kill-buffer b)))
         (delete-directory superchat-test-chat--dir t)))))

(defun superchat-test-chat--text (buffer)
  "The text of BUFFER; of a chat buffer, the entries only, above the separator."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (or superchat-chat--block (point-max)))))

(defun superchat-test-chat--input (buffer)
  "The text after the entries of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties superchat-chat--input (point-max))))

(defun superchat-test-chat--type (buffer text)
  (with-current-buffer buffer (goto-char (point-max)) (insert text)))

(defun superchat-test-chat--tape-of (buffer)
  (buffer-local-value 'superchat-chat--tape buffer))

(defun superchat-test-chat--silent (&rest _)
  "A model that never answers."
  (list :cancelled nil :finished nil :request nil))

(defun superchat-test-chat--scope ()
  (superchat-tape-scope-of default-directory))

(ert-deftest superchat-chat-open-new ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (should (equal (buffer-name buffer) "*superchat: new*"))
      (should (eq (buffer-local-value 'major-mode buffer) 'superchat-chat-mode))
      (should (equal (superchat-test-chat--text buffer) ""))
      (should (string-match-p "\\`─+\n❯ \\'" (with-current-buffer buffer (buffer-string)))))))

(ert-deftest superchat-chat-open-reuses-buffer ()
  (superchat-test-chat--with
    (should (eq (superchat-chat-open "t1") (superchat-chat-open "t1")))
    (should-not (eq (superchat-chat-open "t1") (superchat-chat-open "t2")))))

(ert-deftest superchat-chat-plain-answer ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "Hello there"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (should (equal (superchat-test-chat--text buffer)
                     "* You\nhi\n\n* Assistant\nHello there\n\n"))
      (should (equal (buffer-name buffer) "*superchat: hi*"))
      (should (equal (mapcar (lambda (e) (plist-get e :kind)) (superchat-tape-entries :tape "t1"))
                     '(user assistant)))
      (should-not (buffer-local-value 'mode-line-process buffer)))))

(ert-deftest superchat-chat-title-is-40-chars ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send (make-string 60 ?x))))
      (should (equal (buffer-name buffer) (format "*superchat: %s*" (make-string 40 ?x)))))))

(ert-deftest superchat-chat-title-counts-characters-not-columns ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send (make-string 60 ?字))))
      (should (equal (buffer-name buffer) (format "*superchat: %s*" (make-string 40 ?字)))))))

(ert-deftest superchat-chat-a-call-with-lines-in-its-arguments-is-one-line ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "" :calls ((:id "c1" :name "echo" :args (:x "a\nb"))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (should (string-match-p "^\\*\\*\\* ✓ echo  :x \"a\\\\nb\"  ([0-9.]+s)$"
                              (superchat-test-chat--text buffer))))))

(ert-deftest superchat-chat-tool-round ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "Looking" :calls ((:id "c1" :name "echo" :args (:x "abc"))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (should (equal (replace-regexp-in-string "([0-9.]+s)" "(Ds)" (superchat-test-chat--text buffer))
                     (concat "* You\ngo\n\n* Assistant\nDone\n\n"
                             "** ▸ Looked, 1 call: echo\nLooking\n"
                             "*** ✓ echo  :x \"abc\"  (Ds)\n"
                             "#+begin_example\n:x \"abc\"\n#+end_example\n"
                             "#+begin_example\necho:abc\nsecond line\n#+end_example\n"))))))

(ert-deftest superchat-chat-tool-args-are-cut ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list `(:text "" :calls ((:id "c1" :name "echo" :args (:x ,(make-string 100 ?a)))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (should (string-match "\\*\\*\\* ✓ echo  \\(.*\\)  (" (superchat-test-chat--text buffer)))
      (should (= (length (match-string 1 (superchat-test-chat--text buffer))) 61))
      (should-not (string-match-p "\\* Assistant\n\n" (superchat-test-chat--text buffer))))))

(ert-deftest superchat-chat-render-equals-incremental ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "Looking" :calls ((:id "c1" :name "echo" :args (:x "abc"))
                                          (:id "c2" :name "nope" :args nil)))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (let ((incremental (with-current-buffer buffer (buffer-string))))
        (kill-buffer buffer)
        (let ((again (superchat-chat-open "t1")))
          (should (equal-including-properties incremental
                                              (with-current-buffer again (buffer-string)))))))))

(ert-deftest superchat-chat-streamed-text-shows-as-it-comes ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) snapshots)
      (let ((superchat-llm-request-function
             (lambda (_messages &rest keys)
               (funcall (plist-get keys :on-text) "Hel")
               (push (superchat-test-chat--text buffer) snapshots)
               (funcall (plist-get keys :on-text) "lo")
               (push (superchat-test-chat--text buffer) snapshots)
               (list :cancelled nil :finished nil :request nil))))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (should (equal (reverse snapshots)
                     '("* You\nhi\n\n* Assistant\nHel" "* You\nhi\n\n* Assistant\nHello")))
      (should (string-match-p "\\` \\[running [0-9]+s\\]\\'" (buffer-local-value 'mode-line-process buffer)))
      (with-current-buffer buffer (call-interactively (key-binding (kbd "C-c C-k"))))
      (should-not (buffer-local-value 'mode-line-process buffer))
      ;; Streamed text that never became an entry does not stay.
      (should (equal (superchat-test-chat--text buffer)
                     "* You\nhi\n\nCancelled by the user.\n")))))

(ert-deftest superchat-chat-unsent-input-survives ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-chat--type buffer "half a thought")
      (with-current-buffer buffer (goto-char (+ (marker-position superchat-chat--input) 4)))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer
          (superchat-chat-send "other")
          (should (equal (buffer-substring-no-properties superchat-chat--input (point-max))
                         "half a thought"))
          (should (= (point) (+ (marker-position superchat-chat--input) 4)))))
      (should (string-prefix-p "* You\nother" (superchat-test-chat--text buffer)))
      ;; The render after a handoff keeps it as well.
      (superchat-test-script (list '(:text "sum\nNEXT: n"))
        (with-current-buffer buffer (superchat-chat-send "/handoff")))
      (should (string-prefix-p "── summary: handoff ──" (superchat-test-chat--text buffer)))
      (should (equal (superchat-test-chat--input buffer) "half a thought")))))

(ert-deftest superchat-chat-above-input-is-read-only ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (with-current-buffer buffer
        (goto-char (1+ (point-min)))
        (should-error (insert "x") :type 'text-read-only)
        (goto-char (point-max))
        (insert "typed")
        (goto-char superchat-chat--input)
        (insert "front")
        (should (equal (superchat-test-chat--input buffer) "fronttyped"))))))

(ert-deftest superchat-chat-return-sends-on-last-line ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-chat--type buffer "line one\nline two")
      (with-current-buffer buffer
        (goto-char (marker-position superchat-chat--input))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (superchat-test-chat--input buffer) "\nline one\nline two"))
        (should-not (superchat-tape-entries :tape "t1"))
        (goto-char (point-max))
        (superchat-test-script (list '(:text "ok"))
          (call-interactively (key-binding (kbd "RET")))))
      (should (equal (plist-get (car (superchat-tape-entries :tape "t1")) :text) "line one\nline two"))
      (should (equal (superchat-test-chat--input buffer) "")))))

(ert-deftest superchat-chat-c-c-c-c-sends-anywhere ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-chat--type buffer "a\nb")
      (with-current-buffer buffer
        (goto-char superchat-chat--input)
        (superchat-test-script (list '(:text "ok"))
          (call-interactively (key-binding (kbd "C-c C-c")))))
      (should (equal (plist-get (car (superchat-tape-entries :tape "t1")) :text) "a\nb")))))

(ert-deftest superchat-chat-send-while-running-is-refused ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1"))
          (superchat-llm-request-function #'superchat-test-chat--silent))
      (with-current-buffer buffer (should (superchat-chat-send "one")))
      (superchat-test-chat--type buffer "two")
      (with-current-buffer buffer (call-interactively (key-binding (kbd "C-c C-c"))))
      (should (equal (superchat-test-chat--input buffer) "two"))
      (should (= (length (superchat-tape-entries :tape "t1" :kinds '(user))) 1))
      (with-current-buffer buffer
        (should-not (superchat-chat-send "three"))
        (superchat-cancel "t1")))))

(ert-deftest superchat-chat-c-c-c-k-cancels ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1"))
          (superchat-llm-request-function #'superchat-test-chat--silent))
      (with-current-buffer buffer
        (superchat-chat-send "one")
        (call-interactively (key-binding (kbd "C-c C-k"))))
      (should-not (superchat-run-active "t1"))
      (should (eq (plist-get (car (last (superchat-tape-entries :tape "t1"))) :kind) 'event))
      (should (string-suffix-p "Cancelled by the user.\n" (superchat-test-chat--text buffer))))))

(ert-deftest superchat-chat-unknown-command-is-not-sent ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-chat--type buffer "/bogus x")
      (superchat-test-script nil
        (with-current-buffer buffer (call-interactively (key-binding (kbd "C-c C-c"))))
        (should-not superchat-test-requests))
      (should-not (superchat-tape-entries :tape "t1"))
      (should (equal (superchat-test-chat--input buffer) "/bogus x")))))

(ert-deftest superchat-chat-command-registry ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) got)
      (superchat-command-define "ping" (lambda (rest) (push (list rest (current-buffer)) got))
                                "Ping.")
      (with-current-buffer buffer (should (superchat-chat-send "/ping  a b ")))
      (should (equal got (list (list "a b" buffer))))
      (should-not (superchat-tape-entries :tape "t1"))
      (superchat-command-define "ping" (lambda (_) (push 'again got)) "Ping 2.")
      (with-current-buffer buffer (superchat-chat-send "/ping"))
      (should (eq (car got) 'again))
      (superchat-command-remove "ping")
      (with-current-buffer buffer (should-not (superchat-chat-send "/ping"))))))

(ert-deftest superchat-chat-help-lists-commands ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-command-define "ping" #'ignore "Ping it.")
      (with-current-buffer buffer (superchat-chat-send "/help"))
      (let ((text (superchat-test-chat--text (get-buffer "*superchat-view: help*"))))
        (dolist (name '("new" "resume" "fork" "handoff" "topic" "recall" "notes" "level"
                        "model" "help" "ping"))
          (should (string-match-p (format "^/%s " name) text)))
        (should (string-match-p "Ping it\\." text))))))

(ert-deftest superchat-chat-new-command ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) (before (buffer-list)))
      (let ((new (with-current-buffer buffer
                   (superchat-chat-send "/new")
                   (current-buffer))))
        (should (memq new (cl-set-difference (buffer-list) before)))
        (should-not (equal (superchat-test-chat--tape-of new) "t1"))
        (should (equal default-directory (buffer-local-value 'default-directory buffer)))))))

(ert-deftest superchat-chat-resume-command ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "a1") '(:text "b1"))
      (with-current-buffer (superchat-chat-open "t1") (superchat-chat-send "first topic"))
      (with-current-buffer (superchat-chat-open "t2") (superchat-chat-send "second topic")))
    (let (offered shown (other (superchat-chat-open "t3")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq offered (mapcar #'car choices))
                   (car (seq-find (lambda (c) (string-prefix-p "first" (car c))) choices)))))
        (setq shown (with-current-buffer other
                      (superchat-chat-send "/resume")
                      (current-buffer))))
      (should (= (length offered) 2))
      (should (cl-every (lambda (s) (string-match-p "topic" s)) offered))
      (should (equal (superchat-test-chat--tape-of shown) "t1")))))

(ert-deftest superchat-chat-fork-command ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "a1") '(:text "a2"))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "one")
        (superchat-chat-send "two")))
    (let* ((shown (with-current-buffer (superchat-chat-open "t1")
                    (goto-char (point-min))
                    (search-forward "a1")
                    (superchat-chat-send "/fork")
                    (current-buffer)))
           (fork (superchat-test-chat--tape-of shown)))
      (should-not (equal fork "t1"))
      (should (equal (superchat-test-chat--text shown)
                     "* You\none\n\n* Assistant\na1\n\n── summary: fork ──\n\n"))
      (should (= (length (superchat-tape-entries :tape "t1")) 4)))))

(ert-deftest superchat-chat-fork-from-the-input-forks-at-the-latest-entry ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "a1") '(:text "a2"))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "one")
        (superchat-chat-send "two")))
    (let ((shown (with-current-buffer (superchat-chat-open "t1")
                   (goto-char (point-max))
                   (superchat-chat-send "/fork")
                   (current-buffer))))
      (should (string-match-p "a2" (superchat-test-chat--text shown))))))

(ert-deftest superchat-chat-return-above-the-input-is-refused ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (with-current-buffer buffer
        (goto-char (1+ (point-min)))
        (should-error (call-interactively (key-binding (kbd "RET"))) :type 'text-read-only)
        (should (= (length (superchat-tape-entries :tape "t1" :kinds '(user))) 1))))))

(ert-deftest superchat-chat-fork-without-entry-at-point ()
  (superchat-test-chat--with
    (with-current-buffer (superchat-chat-open "t1")
      (should-error (superchat-chat-send "/fork") :type 'user-error))))

(ert-deftest superchat-chat-handoff-command ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "a1") '(:text "All about a.\nNEXT: b"))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "one")
        (superchat-chat-send "/handoff Phase")))
    (let ((anchor (car (superchat-tape-entries :tape "t1" :kinds '(anchor)))))
      (should (equal (plist-get (plist-get anchor :data) :name) "Phase"))
      (should (equal (superchat-test-chat--text (superchat-chat-open "t1"))
                     "── summary: Phase ──\nAll about a.\n\nWhat comes next: b\n\n")))))

(ert-deftest superchat-chat-handoff-is-refused-while-running ()
  (superchat-test-chat--with
    (let ((superchat-llm-request-function #'superchat-test-chat--silent))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "one")
        (let ((said nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
            (superchat-chat-send "/handoff"))
          (should (string-match-p "still running" said)))
        (superchat-cancel "t1"))
      (should-not (superchat-tape-entries :tape "t1" :kinds '(anchor))))))

(ert-deftest superchat-chat-input-stays-when-the-timeline-is-busy ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "a1"))
        (with-current-buffer buffer (superchat-chat-send "one")))
      (let ((superchat-test-async t))
        (superchat-test-script (list '(:text "Summary.\nNEXT: n"))
          (with-current-buffer buffer (superchat-chat-send "/handoff"))
          (superchat-test-chat--type buffer "next question")
          (let (said)
            (cl-letf (((symbol-function 'message)
                       (lambda (f &rest a) (setq said (apply #'format f a)))))
              (with-current-buffer buffer (call-interactively (key-binding (kbd "C-c C-c")))))
            (should (equal said "A handoff is still running; C-c C-k cancels it")))
          (should (equal (superchat-test-chat--input buffer) "next question"))
          (superchat-test-drain)))
      (should (= (length (superchat-tape-entries :tape "t1" :kinds '(user))) 1)))))

(ert-deftest superchat-chat-entry-is-shown-once ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) (superchat-test-async t))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer
          (superchat-chat-send "hi")
          (superchat-chat-send "/level auto"))
        (superchat-test-drain))
      (let ((text (superchat-test-chat--text buffer)))
        (should (= (length (split-string text "Permission level: auto")) 2))
        (kill-buffer buffer)
        (should (equal text (superchat-test-chat--text (superchat-chat-open "t1"))))))))

(ert-deftest superchat-chat-new-buffer-takes-over-a-running-run ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) (superchat-test-async t))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi"))
        (kill-buffer buffer)
        (let ((again (superchat-chat-open "t1")))
          (should (string-match-p "\\[running [0-9]+s\\]" (buffer-local-value 'mode-line-process again)))
          (superchat-test-drain)
          (should (equal (superchat-test-chat--text again)
                         "* You\nhi\n\n* Assistant\nHello\n\n"))
          (should-not (buffer-local-value 'mode-line-process again)))))))

(ert-deftest superchat-chat-new-command-keeps-the-scope ()
  (superchat-test-chat--with
    (let* ((project (file-name-as-directory (expand-file-name "proj" superchat-test-chat--dir)))
           (project-scope (progn (make-directory (expand-file-name ".git" project) t)
                                 (superchat-tape-scope-of project))))
      (should (equal (superchat-tape-scope-of default-directory) "home"))
      (superchat-tape-append "p1" 'user "in the project" :scope project-scope)
      (superchat-tape-append "h1" 'user "at home" :scope "home")
      ;; Every chat is opened from inside the project, whatever its scope.
      (setq default-directory project)
      (dolist (case (list (cons "p1" project-scope) (cons "h1" "home")))
        (let ((new (with-current-buffer (superchat-chat-open (car case))
                     (superchat-chat-send "/new")
                     (current-buffer))))
          (superchat-test-script (list '(:text "ok"))
            (with-current-buffer new (superchat-chat-send "hello")))
          (should (equal (plist-get (car (superchat-tape-entries
                                          :tape (superchat-test-chat--tape-of new)))
                                    :scope)
                         (cdr case))))))))

(ert-deftest superchat-chat-scope-commands-use-the-timeline-scope ()
  (superchat-test-chat--with
    (superchat-tape-append "h1" 'user "at home topic" :scope "home")
    (superchat-tape-append "p1" 'user "in the project topic" :scope "/proj/")
    (setq default-directory (file-name-as-directory (expand-file-name "d" superchat-test-chat--dir)))
    (make-directory default-directory)
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (setq offered (mapcar #'car choices)) (caar choices))))
        (with-current-buffer (superchat-chat-open "h1") (superchat-chat-send "/resume")))
      (should (= (length offered) 1))
      (should (string-match-p "at home" (car offered))))))

(ert-deftest superchat-chat-cancel-and-mode-line-of-a-handoff ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "a1"))
        (with-current-buffer buffer (superchat-chat-send "one")))
      (let ((superchat-test-async t))
        (superchat-test-script (list '(:text "never") '(:text "Sum.\nNEXT: n"))
          (with-current-buffer buffer
            (superchat-chat-send "/handoff")
            (should (string-match-p "\\[running [0-9]+s\\]" mode-line-process))
            (call-interactively (key-binding (kbd "C-c C-k")))
            (should-not mode-line-process)
            ;; The event is shown at once, as a fresh render would show it.
            (should (string-match-p "Cancelled by the user" (buffer-string))))
          (should-not (superchat-run-active "t1"))
          (with-current-buffer buffer (superchat-chat-send "/handoff")
                               (should (string-match-p "\\[running [0-9]+s\\]" mode-line-process)))
          (superchat-test-drain)
          (should-not (buffer-local-value 'mode-line-process buffer))
          (should (= (length (superchat-tape-entries :tape "t1" :kinds '(anchor))) 1)))))))

(ert-deftest superchat-chat-cancel-with-nothing-running-says-so ()
  (superchat-test-chat--with
    (let (shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (f &rest a) (setq shown (apply #'format f a)))))
        (with-current-buffer (superchat-chat-open "t1")
          (call-interactively (key-binding (kbd "C-c C-k")))))
      (should (string-match-p "Nothing is running" shown)))))

(ert-deftest superchat-chat-recent-tape-past-many-inline-timelines ()
  (superchat-test-chat--with
    (superchat-tape-append "20240101-a" 'user "old" :scope "s/")
    (dotimes (i 150)
      (superchat-tape-append (format "e%d" i) 'user "i" :scope "s/" :data '(:ephemeral t)))
    (should (equal (superchat-chat-recent-tape "s/") "20240101-a"))))

(ert-deftest superchat-chat-topic-recall-notes-commands ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "the answer is plums"))
      (with-current-buffer (superchat-chat-open "t1") (superchat-chat-send "fruit?")))
    (superchat-tape-append "t1" 'anchor "S" :data '(:name "fruit" :summary "S" :next ""))
    (superchat-tape-append "t1" 'note "plums win" :data '(:title "T"))
    (let ((buffer (superchat-chat-open "t1")))
      (dolist (case '(("/topic fruit" "topic fruit" "#3 anchor")
                      ("/recall plums" "recall plums" "the answer is plums")
                      ("/notes" "notes" "plums win")))
        (with-current-buffer buffer (superchat-chat-send (car case)))
        (with-current-buffer (get-buffer (format "*superchat-view: %s*" (cadr case)))
          (should (derived-mode-p 'special-mode))
          (should buffer-read-only)
          (should (string-match-p (regexp-quote (nth 2 case)) (buffer-string))))))))

(ert-deftest superchat-chat-level-command ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) shown)
      (superchat-tape-append "t1" 'event "start" :data '(:type "cancelled")
                             :scope (superchat-test-chat--scope))
      (with-current-buffer buffer
        (superchat-chat-send "/level auto")
        (should (eq (superchat-permission-level "t1") 'auto))
        (should (string-match-p "Permission level: auto" (buffer-string)))
        (cl-letf (((symbol-function 'message)
                   (lambda (f &rest a) (setq shown (apply #'format f a)))))
          (superchat-chat-send "/level"))
        (should (equal shown "Permission level: auto"))
        (should-error (superchat-chat-send "/level bogus"))))))

(ert-deftest superchat-chat-model-command ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (f &rest a) (setq shown (apply #'format f a)))))
        (with-current-buffer buffer
          (let ((superchat-llm-model "base"))
            (superchat-chat-send "/model")
            (should (equal shown "Model: base")))
          (let ((superchat-llm-provider "deepseek"))
            (should-error (superchat-chat-send "/model big-one") :type 'user-error)
            (superchat-chat-send "/model deepseek-v4-pro"))
          (superchat-chat-send "/model")
          (should (equal shown "Model: deepseek-v4-pro"))))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send "hi"))
        (should (equal (plist-get (car superchat-test-requests) :model)
                       "deepseek-v4-pro")))
      (should-not (buffer-local-value 'superchat-chat--model (superchat-chat-open "t2"))))))

(ert-deftest superchat-chat-recent-tape-ignores-ephemeral ()
  (superchat-test-chat--with
    (let ((p (expand-file-name "p/" superchat-test-chat--dir))
          (q (expand-file-name "q/" superchat-test-chat--dir)))
      (superchat-tape-append "20240101-a" 'user "old" :scope p)
      (superchat-tape-append "e-p" 'user "ephemeral" :scope p :data '(:ephemeral t))
      (superchat-tape-append "other" 'user "x" :scope q)
      (should (equal (superchat-chat-recent-tape p) "20240101-a"))
      (superchat-tape-append "20240102-b" 'user "newer" :scope p)
      (superchat-tape-append "e-p" 'assistant "again")
      (should (equal (superchat-chat-recent-tape p) "20240102-b")))))

(ert-deftest superchat-chat-command-reuses-recent-timeline ()
  (superchat-test-chat--with
    (superchat-tape-append "20240101-a" 'user "earlier" :scope (superchat-test-chat--scope))
    (superchat)
    (let ((first (current-buffer)))
      (should (equal (superchat-test-chat--tape-of first) "20240101-a"))
      (should (equal (superchat-test-chat--text first) "* You\nearlier\n\n"))
      (superchat)
      (should (eq (current-buffer) first))
      (superchat '(4))
      (should-not (eq (current-buffer) first))
      (should-not (superchat-tape-entries
                   :tape (superchat-test-chat--tape-of (current-buffer)))))))

(ert-deftest superchat-chat-note-from-a-chat-buffer-uses-its-scope ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "ta" "/chat-scope/")))
      (with-current-buffer buffer
        (setq default-directory superchat-test-chat--dir)
        (superchat-note "kept in the chat scope"))
      (should (equal (plist-get (car (superchat-tape-entries :kinds '(note))) :scope)
                     "/chat-scope/")))))

(ert-deftest superchat-chat-a-note-from-elsewhere-shows-in-the-open-chat ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (superchat-note "kept from another buffer")
      (superchat-test-script (list '(:text "later"))
        (with-current-buffer buffer (superchat-chat-send "again")))
      (let ((incremental (superchat-test-chat--text buffer)))
        (should (string-match-p "kept from another buffer" incremental))
        (kill-buffer buffer)
        (should (equal incremental (superchat-test-chat--text (superchat-chat-open "t1"))))))))

(ert-deftest superchat-chat-note-command-appends-and-shows ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1" (superchat-test-chat--scope))))
      (with-current-buffer buffer (superchat-chat-send "/note keep this"))
      (let ((note (car (superchat-tape-entries :tape "t1" :kinds '(note)))))
        (should (equal (plist-get note :text) "keep this"))
        (should (equal (plist-get (plist-get note :data) :title) "keep this")))
      (should (string-match-p "keep this" (superchat-test-chat--text buffer))))))

(ert-deftest superchat-chat-an-entry-during-streaming-keeps-the-streamed-text ()
  (superchat-test-chat--with
    (let* ((buffer (superchat-chat-open "t1" (superchat-test-chat--scope))) answer
           (superchat-llm-request-function
            (lambda (_messages &rest keys)
              (funcall (plist-get keys :on-text) "Hel")
              (setq answer keys)
              (list :cancelled nil :finished nil :request nil))))
      (with-current-buffer buffer
        (superchat-chat-send "hi")
        (superchat-chat-send "/note aside")
        (funcall (plist-get answer :on-text) "lo"))
      (should (equal (superchat-test-chat--text buffer)
                     "* You\nhi\n\n* Assistant\nHello\n\nnote: aside\naside\n\n"))
      (funcall (plist-get answer :on-done) '(:text "Hello" :model "m"))
      (should (equal (superchat-test-chat--text buffer)
                     "* You\nhi\n\n* Assistant\nHello\n\nnote: aside\naside\n\n")))))

(ert-deftest superchat-chat-recall-command-uses-the-chat-scope ()
  (superchat-test-chat--with
    (superchat-tape-append "p1" 'user "a ferret here" :scope "/proj/")
    (superchat-tape-append "p1" 'note "kept" :data '(:title "kept"))
    (with-current-buffer (superchat-chat-open "p1")
      (superchat-recall "ferret")
      (should (string-match-p "a ferret here"
                              (superchat-test-chat--text "*superchat-view: recall ferret*")))
      (superchat-recall "")
      (should (string-match-p "kept" (superchat-test-chat--text "*superchat-view: notes*"))))))

(ert-deftest superchat-chat-command-records-origin ()
  (superchat-test-chat--with
    (let ((file (expand-file-name "a.el" superchat-test-chat--dir))
          (source (current-buffer)))
      (superchat-tape-append "20240101-a" 'user "earlier" :scope (superchat-test-chat--scope))
      (emacs-lisp-mode)
      (setq buffer-file-name file)
      (rename-buffer "a.el")
      (insert "one\ntwo\n")
      (superchat)
      (superchat-test-script (list '(:text "ok") '(:text "ok2"))
        (superchat-chat-send "first")
        (superchat-chat-send "second"))
      (let ((users (superchat-tape-entries :kinds '(user))))
        (should (equal (plist-get (plist-get (cadr users) :data) :origin)
                       (list :buffer "a.el" :file file :mode "emacs-lisp-mode" :line 3)))
        (should (stringp (plist-get (plist-get (plist-get (cadr users) :data) :origin) :mode)))
        (should-not (plist-get (plist-get (car (last users)) :data) :origin)))
      (with-current-buffer source
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))))

(ert-deftest superchat-chat-command-in-a-scope-with-no-timeline ()
  (superchat-test-chat--with
    (superchat)
    (should (equal (superchat-test-chat--text (current-buffer)) ""))
    (should (equal (buffer-name) "*superchat: new*"))))

(ert-deftest superchat-chat-send-origin-argument ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "ok"))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "hi" '(:buffer "b"))))
    (should (equal (plist-get (plist-get (car (superchat-tape-entries :tape "t1")) :data) :origin)
                   '(:buffer "b")))))

(ert-deftest superchat-chat-command-enables-plugins ()
  (superchat-test-chat--with
    (let (enabled (superchat-plugins '(chat-test-plugin)))
      (superchat-tape-append "20240101-a" 'user "earlier" :scope (superchat-test-chat--scope))
      (superchat-plugin-define 'chat-test-plugin :enable (lambda () (push 'on enabled)))
      (superchat)
      (superchat)
      (should (equal enabled '(on)))
      (should (superchat-plugin-enabled-p 'chat-test-plugin)))))

(ert-deftest superchat-chat-model-has-tape-tools ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "ok"))
      (with-current-buffer (superchat-chat-open "t1") (superchat-chat-send "hi"))
      (let ((names (mapcar (lambda (s) (plist-get s :name))
                           (plist-get (car superchat-test-requests) :tools))))
        (dolist (n '("recall" "tape_query" "note")) (should (member n names)))))))

;; What a test sees `message' say, the latest last.
(defmacro superchat-test-chat--said (var &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
              (lambda (f &rest a) (when f (setq ,var (apply #'format f a))))))
     ,@body))

(ert-deftest superchat-chat-a-command-that-opens-another-buffer-keeps-its-draft ()
  (superchat-test-chat--with
    (superchat-tape-append "t1" 'user "first topic" :scope (superchat-test-chat--scope))
    (superchat-tape-append "t2" 'user "second topic" :scope (superchat-test-chat--scope))
    (let ((one (superchat-chat-open "t1")) (two (superchat-chat-open "t2")))
      (superchat-test-chat--type two "a draft")
      (superchat-test-chat--type one "/resume")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (car (seq-find (lambda (c) (string-prefix-p "second" (car c))) choices)))))
        (with-current-buffer one (call-interactively (key-binding (kbd "C-c C-c")))))
      (should (equal (superchat-test-chat--input two) "a draft"))
      (should (equal (superchat-test-chat--input one) "")))))

(ert-deftest superchat-chat-typed-input-inherits-nothing ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert-and-inherit "typed")
        (should-not (get-text-property (1- (point-max)) 'face))
        (should-not (get-text-property (1- (point-max)) 'superchat-chat-id))))))

(ert-deftest superchat-chat-fork-from-typed-input-forks-at-the-latest-entry-now ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1" (superchat-test-chat--scope))))
      (superchat-test-script (list '(:text "a1"))
        (with-current-buffer buffer (superchat-chat-send "one")))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert-and-inherit "/fork"))
      (superchat-note "a later note")
      (with-current-buffer buffer
        (goto-char (1+ superchat-chat--input))
        (call-interactively (key-binding (kbd "C-c C-c"))))
      (let ((fork (seq-find (lambda (b) (not (member (superchat-test-chat--tape-of b) '(nil "t1"))))
                            (buffer-list))))
        (should (string-match-p "a later note" (superchat-test-chat--text fork)))))))

(ert-deftest superchat-chat-a-window-not-selected-follows-in-the-input ()
  (superchat-test-chat--with
    (let* ((buffer (superchat-chat-open "t1"))
           (window (split-window)))
      (set-window-buffer window buffer)
      (should-not (eq window (selected-window)))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (should (= (window-point window) (with-current-buffer buffer (point-max)))))))

(ert-deftest superchat-chat-an-error-is-shown-as-one ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) said)
      (superchat-test-chat--said said
        (superchat-test-script (list '(:error "boom"))
          (with-current-buffer buffer (superchat-chat-send "hi"))))
      (let* ((event (car (superchat-tape-entries :tape "t1" :kinds '(event))))
             (text (superchat-test-chat--text buffer)))
        (should (equal (plist-get (plist-get event :data) :type) "error"))
        (should (string-match (regexp-quote (plist-get event :text)) text))
        (should (eq (get-text-property (1+ (match-beginning 0)) 'font-lock-face buffer) 'error)))
      (should (string-match-p "boom" said)))))

(ert-deftest superchat-chat-a-note-shows-its-title ()
  (superchat-test-chat--with
    (superchat-tape-append "t1" 'user "hi" :scope (superchat-test-chat--scope))
    (superchat-tape-append "t1" 'note "the text" :data '(:title "The title"))
    (let ((text (superchat-test-chat--text (superchat-chat-open "t1"))))
      (should (equal text "* You\nhi\n\nnote: The title\nthe text\n\n")))))

(ert-deftest superchat-chat-recall-with-no-query-shows-the-notes ()
  (superchat-test-chat--with
    (superchat-tape-append "t1" 'user "hi" :scope (superchat-test-chat--scope))
    (superchat-tape-append "t1" 'note "kept here" :data '(:title "kept"))
    (with-current-buffer (superchat-chat-open "t1") (superchat-chat-send "/recall"))
    (should (string-match-p "kept here" (superchat-test-chat--text "*superchat-view: notes*")))
    (with-current-buffer (superchat-chat-open "t1")
      (should-error (superchat-chat-send "/topic") :type 'user-error))))

(ert-deftest superchat-chat-command-from-a-chat-buffer-records-no-origin ()
  (superchat-test-chat--with
    (with-current-buffer (superchat-chat-open "t1" (superchat-test-chat--scope))
      (superchat)
      (superchat-test-script (list '(:text "ok"))
        (superchat-chat-send "from the chat")))
    (should-not (plist-get (plist-get (car (last (superchat-tape-entries :kinds '(user)))) :data)
                           :origin))))

(ert-deftest superchat-chat-open-with-no-scope-keeps-the-directory-it-was-made-in ()
  (superchat-test-chat--with
    (let* ((project (file-name-as-directory (expand-file-name "proj" superchat-test-chat--dir)))
           (scope (progn (make-directory (expand-file-name ".git" project) t)
                         (superchat-tape-scope-of project)))
           (buffer (let ((default-directory project)) (superchat-chat-open "t1"))))
      (with-current-buffer buffer (setq default-directory superchat-test-chat--dir))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (should (equal (plist-get (car (superchat-tape-entries :tape "t1")) :scope) scope)))))

;;; The Org display

(defun superchat-test-chat--org (markdown)
  "What a chat buffer shows of an answer MARKDOWN, below its heading."
  (let ((tape (superchat-tape-new-name)))
    (superchat-tape-append tape 'user "q" :scope (superchat-test-chat--scope))
    (superchat-tape-append tape 'assistant markdown)
    (string-remove-suffix
     "\n\n" (string-remove-prefix "* You\nq\n\n* Assistant\n"
                                  (superchat-test-chat--text (superchat-chat-open tape))))))

(ert-deftest superchat-chat-markdown-becomes-org ()
  (superchat-test-chat--with
    (let (wrong)
     (pcase-dolist (`(,markdown ,org)
                   '(;; The info string's first word only, commas, no newline before the end.
                     ("```python title=x\n* a\n#+b\n```" "#+begin_src python\n,* a\n,#+b\n#+end_src")
                     ("~~~\n**x** # y\n~~~" "#+begin_src\n,**x** # y\n#+end_src")
                     ("````\n```\n````" "#+begin_src\n```\n#+end_src")
                     ("#+begin_src sh\necho *a*\n#+end_src" "#+begin_src sh\necho *a*\n#+end_src")
                     ("```\n# open to the end" "#+begin_src\n# open to the end\n#+end_src")
                     ;; Inline code first: nothing converts inside it.
                     ("use `**x**` and `a=b`" "use =**x**= and ~a=b~")
                     ;; Only # to ###### headings, the shallowest at level 2.
                     ("# A\n## B\ntext" "** A\n*** B\ntext")
                     ("## B\n### C" "** B\n*** C")
                     ("#tag\n####### seven" "#tag\n####### seven")
                     ("a\n\n---\n\n* * *\n___" "a\n\n-----\n\n-----\n-----")
                     ("* one\n+ two\n  - three" "- one\n- two\n  - three")
                     ;; One character is enough.
                     ("**a** *b* __c__ _d_ ~~e~~" "*a* /b/ *c* /d/ +e+")
                     ("**bold *inner* text**" "*bold /inner/ text*")
                     ("snake_case_name and 2 * 3 * 4" "snake_case_name and 2 * 3 * 4")
                     ("![pic](http://x/p.png)" "[[http://x/p.png]]")
                     ("[a [b] c](http://w/x_(y)) after" "[[http://w/x_(y)][a [b] c]] after")
                     ("[k](http://w/a[1])" "[[http://w/a\\[1\\]][k]]")
                     ("> one\n> two\nafter" "#+begin_quote\none\ntwo\n#+end_quote\nafter")
                     ("| a | b |\n|:--|--:|\n| 1 | 22 |" "| a |  b |\n|---+----|\n| 1 | 22 |")))
      (let ((got (superchat-test-chat--org markdown)))
        (unless (equal got org) (push (list markdown got) wrong))))
     (should-not wrong))
    (should-not (string-match-p "begin_src" (superchat-test-chat--org "say ```x``` here")))
    ;; The Tape keeps the Markdown.
    (should (equal (plist-get (car (superchat-tape-entries :kinds '(assistant) :newest t :limit 1)) :text)
                   "say ```x``` here"))))

(ert-deftest superchat-chat-streamed-text-is-raw-until-the-entry-comes ()
  (superchat-test-chat--with
    (let* ((buffer (superchat-chat-open "t1")) streamed face
           (superchat-llm-request-function
           (lambda (_messages &rest keys)
             (funcall (plist-get keys :on-text) "**bold**")
             (setq streamed (superchat-test-chat--text buffer)
                   face (with-current-buffer buffer
                          (get-text-property (- superchat-chat--block 2) 'font-lock-face)))
             (funcall (plist-get keys :on-done) '(:text "**bold**" :model "m"))
             (list :cancelled nil :finished nil :request nil))))
      (with-current-buffer buffer (superchat-chat-send "hi"))
      (should (equal streamed "* You\nhi\n\n* Assistant\n**bold**"))
      (should (eq face 'shadow))
      (should (equal (superchat-test-chat--text buffer) "* You\nhi\n\n* Assistant\n*bold*\n\n")))))

(defun superchat-test-chat--visible (buffer)
  "The entries of BUFFER as a person sees them, folded text left out."
  (with-current-buffer buffer
    (cl-loop for at from (point-min) below superchat-chat--block
             unless (invisible-p at) concat (string (char-after at)))))

(defun superchat-test-chat--d (text)
  "TEXT with every duration written D."
  (replace-regexp-in-string "([0-9.]+s)" "(Ds)" text))

(ert-deftest superchat-chat-tool-headings-and-the-process ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) during seen)
      (superchat-tool-define "peek" :class 'read :description "Peek" :args nil
                             :function (lambda (_)
                                         (setq during (superchat-test-chat--text buffer)
                                               seen (superchat-test-chat--visible buffer))
                                         "seen"))
      (unwind-protect
          (superchat-test-script
              (list '(:text "" :calls ((:id "c1" :name "echo" :args (:x "a"))))
                    '(:text "Looking" :calls ((:id "c2" :name "echo" :args (:x "b"))
                                              (:id "c3" :name "nope" :args nil)
                                              (:id "c4" :name "peek" :args nil)))
                    '(:text "Done"))
            (with-current-buffer buffer (superchat-chat-send "go")))
        (superchat-tool-remove "peek"))
      ;; While it runs, the process is open and a call with no result waits.
      (should (string-prefix-p "* You\ngo\n\n* Assistant\n** ⏳ Looked, 3 calls: echo ×2, peek\n"
                               (superchat-test-chat--d during)))
      (should (string-match-p "^\\*\\*\\* ✓ echo  :x \"b\"  (.*)\n\\*\\*\\* ⏳ peek$" seen))
      (should (string-match-p "^Looking$" seen))
      (let ((text (superchat-test-chat--d (superchat-test-chat--text buffer))))
        ;; The final answer first, then the process, then the call with an error.
        (should (equal text
                       (concat "* You\ngo\n\n* Assistant\nDone\n\n"
                               "** ▸ Looked, 3 calls: echo ×2, peek\n"
                               "*** ✓ echo  :x \"a\"  (Ds)\n#+begin_example\n:x \"a\"\n#+end_example\n"
                               "#+begin_example\necho:a\nsecond line\n#+end_example\n"
                               "Looking\n"
                               "*** ✓ echo  :x \"b\"  (Ds)\n#+begin_example\n:x \"b\"\n#+end_example\n"
                               "#+begin_example\necho:b\nsecond line\n#+end_example\n"
                               "*** ✓ peek  (Ds)\n#+begin_example\nseen\n#+end_example\n"
                               "** ✗ nope  (Ds)\n#+begin_example\nError: Unknown tool: nope\n#+end_example\n"))))
      ;; At the end the process is folded; the error's heading shows, its body does not.
      (should (equal (superchat-test-chat--d (superchat-test-chat--visible buffer))
                     (concat "* You\ngo\n\n* Assistant\nDone\n\n"
                             "** ▸ Looked, 3 calls: echo ×2, peek\n** ✗ nope  (Ds)\n")))
      (with-current-buffer buffer
        ;; TAB opens the read-only process to what it holds, each call still folded.
        (goto-char (point-min))
        (search-forward "▸ Looked")
        (call-interactively (key-binding (kbd "TAB")))
        (should (string-match-p "^\\*\\*\\* ✓ echo  :x \"a\"  (.*)\n\\*\\*\\* ✓ echo"
                                (superchat-test-chat--visible buffer)))
        ;; It stays open when its turn is drawn again.
        (superchat-note "a later note")
        (should (string-match-p "^\\*\\*\\* ✓ peek" (superchat-test-chat--visible buffer)))
        ;; The last level 1 heading folds only down to the separator.
        (goto-char (point-min))
        (search-forward "* Assistant")
        (call-interactively (key-binding (kbd "TAB")))
        (should (string-suffix-p "─\n❯ " (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (invisible-p superchat-chat--block))
        (should-not (invisible-p (1- (point-max))))))))

(ert-deftest superchat-chat-streamed-text-shows-above-the-process ()
  (superchat-test-chat--with
    (let* ((buffer (superchat-chat-open "t1")) (asked 0) streamed seen
           (superchat-llm-request-function
            (lambda (_messages &rest keys)
              (if (= (cl-incf asked) 1)
                  (funcall (plist-get keys :on-done)
                           '(:text "" :calls ((:id "c1" :name "echo" :args (:x "a"))) :model "m"))
                (funcall (plist-get keys :on-text) "Fin")
                (setq streamed (superchat-test-chat--text buffer)
                      seen (superchat-test-chat--visible buffer))
                (funcall (plist-get keys :on-done) '(:text "Fin" :model "m")))
              (list :cancelled nil :finished nil :request nil))))
      (with-current-buffer buffer (superchat-chat-send "go"))
      (should (string-prefix-p "* You\ngo\n\n* Assistant\nFin\n\n** ▸ Looked, 1 call: echo\n*** ✓ echo"
                               streamed))
      (should (string-match-p "^\\*\\*\\* ✓ echo" seen))
      (should (string-prefix-p "* You\ngo\n\n* Assistant\nFin\n\n** ▸ Looked, 1 call: echo\n*** ✓ echo"
                               (superchat-test-chat--text buffer)))
      (should-not (string-match-p "✓ echo" (superchat-test-chat--visible buffer))))))

(ert-deftest superchat-chat-a-turn-that-fails-folds-its-process ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (let (said)
        (superchat-test-chat--said said
          (superchat-test-script
              (list '(:text "Trying" :calls ((:id "c1" :name "echo" :args (:x "a"))))
                    '(:error "boom"))
            (with-current-buffer buffer (superchat-chat-send "go"))))
        (should (string-match-p "boom" said)))
      (let ((event (plist-get (car (superchat-tape-entries :tape "t1" :kinds '(event))) :text)))
        (should (equal (superchat-test-chat--visible buffer)
                       (concat "* You\ngo\n\n* Assistant\n** ▸ Looked, 1 call: echo\n"
                               event "\n")))))))

(ert-deftest superchat-chat-render-equals-incremental-with-groups ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "# Plan\nfirst" :calls ((:id "c1" :name "echo" :args (:x "a"))
                                              (:id "c2" :name "echo" :args (:x "b"))))
                '(:text "" :calls ((:id "c3" :name "nope" :args nil)))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (superchat-test-script (list '(:text "again"))
        (with-current-buffer buffer (superchat-chat-send "more")))
      (let ((incremental (with-current-buffer buffer (buffer-string)))
            (seen (superchat-test-chat--visible buffer)))
        ;; An intermediate text's headings sit below the process heading.
        (should (string-match-p "^\\*\\*\\* Plan$" (superchat-test-chat--text buffer)))
        (kill-buffer buffer)
        (let ((again (superchat-chat-open "t1")))
          (should (equal-including-properties incremental (with-current-buffer again (buffer-string))))
          (should (equal seen (superchat-test-chat--visible again))))))))

(ert-deftest superchat-chat-header-line ()
  (superchat-test-chat--with
    (let* ((superchat-llm-model "m1")
           (buffer (superchat-chat-open "t1"))
           (superchat-llm-request-function #'superchat-test-chat--silent))
      (with-current-buffer buffer
        (should (equal header-line-format "m1 · ask · home"))
        (superchat-chat-send "one")
        (should (equal header-line-format "m1 · ask · home · running"))
        (call-interactively (key-binding (kbd "C-c C-k")))
        (should (equal header-line-format "m1 · ask · home"))
        (superchat-chat-send "/level auto")
        (should (equal header-line-format "m1 · auto · home"))))))

(defun superchat-test-chat--waiting (buffer)
  "The waiting line of BUFFER, or nil."
  (with-current-buffer buffer
    (seq-some (lambda (o) (overlay-get o 'before-string)) (overlays-in (point-min) (point-max)))))

(ert-deftest superchat-chat-waiting-line ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")) during)
      (let ((superchat-llm-request-function #'superchat-test-chat--silent))
        (with-current-buffer buffer (superchat-chat-send "one"))
        (should (string-match-p "\\`. thinking… [0-9]+s\n\\'" (superchat-test-chat--waiting buffer)))
        (should (with-current-buffer buffer
                  (string-suffix-p "* You\none\n\n" (buffer-substring (point-min) superchat-chat--block))))
        (let ((timer (buffer-local-value 'superchat-chat--timer buffer)))
          (should (memq timer timer-list))
          (with-current-buffer buffer (call-interactively (key-binding (kbd "C-c C-k"))))
          (should-not (memq timer timer-list)))
        (should-not (superchat-test-chat--waiting buffer)))
      (superchat-tool-define "peek" :class 'read :description "Peek" :args nil
                             :function (lambda (_) (setq during (superchat-test-chat--waiting buffer)) "ok"))
      (unwind-protect
          (superchat-test-script (list '(:text "" :calls ((:id "c1" :name "peek" :args nil)))
                                       '(:text "Done"))
            (with-current-buffer buffer (superchat-chat-send "two")))
        (superchat-tool-remove "peek"))
      (should (string-match-p "\\`. peek running… [0-9]+s\n\\'" during))
      (should-not (superchat-test-chat--waiting buffer))
      ;; A buffer killed during a run takes its timer with it.
      (let ((superchat-llm-request-function #'superchat-test-chat--silent))
        (with-current-buffer buffer (superchat-chat-send "three"))
        (let ((timer (buffer-local-value 'superchat-chat--timer buffer)))
          (kill-buffer buffer)
          (should-not (memq timer timer-list)))))))

(ert-deftest superchat-chat-user-text-is-never-a-heading ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send "* not a heading\nsecond")))
      (should (string-match-p "^ \\* not a heading$" (superchat-test-chat--text buffer)))
      (should (equal (plist-get (car (superchat-tape-entries :tape "t1" :kinds '(user))) :text)
                     "* not a heading\nsecond")))))

(ert-deftest superchat-chat-a-conversation-is-a-tab ()
  (superchat-test-chat--with
    (superchat-test-script (list '(:text "a1"))
      (with-current-buffer (superchat-chat-open "t1" (superchat-test-chat--scope))
        (superchat-chat-send "one")))
    (unwind-protect
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (car (seq-find (lambda (c) (equal (cdr c) "t1")) choices)))))
          (switch-to-buffer (superchat-chat-open "t1"))
          (let ((tabs (length (tab-bar-tabs))))
            ;; /new and /fork: one more tab each, showing the new chat alone.
            (dolist (command '("/new" "/fork"))
              (with-current-buffer (superchat-chat-open "t1")
                (switch-to-buffer (current-buffer))
                (superchat-chat-send command))
              (cl-incf tabs)
              (should (= (length (tab-bar-tabs)) tabs))
              (should (= (length (window-list)) 1))
              (should-not (equal (superchat-test-chat--tape-of (window-buffer)) "t1")))
            ;; /resume of a conversation that has a tab selects it, no new tab.
            (switch-to-buffer (superchat-chat-open "t1"))
            (tab-bar-new-tab)
            (switch-to-buffer "*scratch*")
            (cl-incf tabs)
            (with-current-buffer (superchat-chat-open "t1") (superchat-chat-send "/resume"))
            (should (= (length (tab-bar-tabs)) tabs))
            (should (equal (superchat-test-chat--tape-of (window-buffer)) "t1"))))
      (tab-bar-close-other-tabs))))

(ert-deftest superchat-chat-a-turn-has-one-assistant-heading ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "" :calls ((:id "c1" :name "echo" :args (:x "a"))))
                '(:text "Looking" :calls ((:id "c2" :name "echo" :args (:x "b"))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (let ((text (superchat-test-chat--text buffer)))
        (should (= (cl-count-if (lambda (l) (equal l "* Assistant")) (split-string text "\n")) 1))
        ;; The final answer above the process, which holds the other rounds.
        (should (string-match-p "\\`\\* You\ngo\n\n\\* Assistant\nDone\n\n\\*\\* ▸ Looked, 2 calls: echo ×2\n" text))
        (should (string-match-p "^Looking$" text)))
      ;; A full render gives the same text.
      (let ((incremental (superchat-test-chat--text buffer)))
        (kill-buffer buffer)
        (should (equal incremental (superchat-test-chat--text (superchat-chat-open "t1"))))))))

(ert-deftest superchat-chat-a-new-tab-shows-the-new-chat-whatever-the-display-rules ()
  (superchat-test-chat--with
    (unwind-protect
        (let ((display-buffer-alist '(("\\*superchat" (display-buffer-in-side-window)
                                       (side . bottom))))
              (switch-to-buffer-obey-display-actions t))
          (switch-to-buffer (superchat-chat-open "t1" (superchat-test-chat--scope)))
          (with-current-buffer (window-buffer) (superchat-chat-send "/new"))
          (should (= (length (window-list)) 1))
          (should-not (equal (superchat-test-chat--tape-of (window-buffer)) "t1"))
          (should (superchat-test-chat--tape-of (window-buffer))))
      (tab-bar-close-other-tabs))))

(ert-deftest superchat-chat-typing-in-the-history-types-in-the-input ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script (list '(:text "Hello"))
        (with-current-buffer buffer (superchat-chat-send "hi")))
      (superchat-test-chat--type buffer "draft ")
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "Hello")
        (let ((last-command-event ?x))
          (call-interactively (key-binding "x")))
        (should (= (point) (point-max)))
        (should (equal (superchat-test-chat--input buffer) "draft x"))))))

(provide 'test-chat)
;;; test-chat.el ends here
