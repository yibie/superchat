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

(defmacro superchat-test-chat--with (&rest body)
  "BODY with a Tape, in a directory made here, outside any project, with a read
tool `echo', and no chat buffers left behind."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((superchat-test-chat--dir (file-name-as-directory (make-temp-file "sc-chat-" t)))
           (superchat-tools--always (make-hash-table :test #'equal))
           (superchat-agent--runs (make-hash-table :test #'equal))
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
         (dolist (b (buffer-list))
           (when (buffer-local-value 'superchat-chat--tape b) (kill-buffer b)))
         (delete-directory superchat-test-chat--dir t)))))

(defun superchat-test-chat--text (buffer)
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

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
      (should (equal (superchat-test-chat--text buffer) "")))))

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
                     "▸ You\nhi\n\n▸ Assistant\nHello there\n\n"))
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

(ert-deftest superchat-chat-tool-round ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list '(:text "Looking" :calls ((:id "c1" :name "echo" :args (:x "abc"))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (should (equal (superchat-test-chat--text buffer)
                     (concat "▸ You\ngo\n\n▸ Assistant\nLooking\n\n"
                             "⚙ echo(:x \"abc\") → echo:abc\n▸ Assistant\nDone\n\n"))))))

(ert-deftest superchat-chat-tool-args-are-cut ()
  (superchat-test-chat--with
    (let ((buffer (superchat-chat-open "t1")))
      (superchat-test-script
          (list `(:text "" :calls ((:id "c1" :name "echo" :args (:x ,(make-string 100 ?a)))))
                '(:text "Done"))
        (with-current-buffer buffer (superchat-chat-send "go")))
      (should (string-match "⚙ echo(\\(.*\\)) → " (superchat-test-chat--text buffer)))
      (should (= (length (match-string 1 (superchat-test-chat--text buffer))) 61))
      (should-not (string-match-p "▸ Assistant\n\n" (superchat-test-chat--text buffer))))))

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
                     '("▸ You\nhi\n\n▸ Assistant\nHel" "▸ You\nhi\n\n▸ Assistant\nHello")))
      (should (equal (buffer-local-value 'mode-line-process buffer) " [running]"))
      (with-current-buffer buffer (superchat-chat--cancel))
      (should-not (buffer-local-value 'mode-line-process buffer))
      ;; Streamed text that never became an entry does not stay.
      (should (equal (superchat-test-chat--text buffer)
                     "▸ You\nhi\n\nCancelled by the user.\n")))))

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
      (should (string-prefix-p "▸ You\nother" (superchat-test-chat--text buffer)))
      ;; A re-render keeps it as well.
      (superchat-tape-append "t1" 'anchor "sum" :data '(:name "n" :summary "sum" :next ""))
      (with-current-buffer buffer (superchat-chat--render))
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
      (with-current-buffer buffer (superchat-chat--send-input))
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
        (with-current-buffer buffer (superchat-chat--send-input))
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
                     "▸ You\none\n\n▸ Assistant\na1\n\n── summary: fork ──\n\n\n"))
      (should (= (length (superchat-tape-entries :tape "t1")) 4)))))

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
                     "── summary: Phase ──\nAll about a.\n\n")))))

(ert-deftest superchat-chat-handoff-is-refused-while-running ()
  (superchat-test-chat--with
    (let ((superchat-llm-request-function #'superchat-test-chat--silent))
      (with-current-buffer (superchat-chat-open "t1")
        (superchat-chat-send "one")
        (should-error (superchat-chat-send "/handoff") :type 'error)
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
          (with-current-buffer buffer (superchat-chat--send-input))
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
          (should (equal (buffer-local-value 'mode-line-process again) " [running]"))
          (superchat-test-drain)
          (should (equal (superchat-test-chat--text again)
                         "▸ You\nhi\n\n▸ Assistant\nHello\n\n"))
          (should-not (buffer-local-value 'mode-line-process again)))))))

(ert-deftest superchat-chat-new-command-keeps-the-scope ()
  (superchat-test-chat--with
    (let* ((project (file-name-as-directory (expand-file-name "proj" superchat-test-chat--dir)))
           (project-scope (progn (make-directory (expand-file-name ".git" project) t)
                                 (superchat-tape-scope-of project))))
      (should (equal (superchat-tape-scope-of default-directory) "home"))
      (superchat-tape-append "p1" 'user "in the project" :scope project-scope)
      (dolist (case (list (cons "p1" project-scope) (cons "h1" "home")))
        (unless (equal (cdr case) project-scope)
          (superchat-tape-append "h1" 'user "at home" :scope "home"))
        (let ((new (with-current-buffer (superchat-chat-open (car case))
                     (superchat-chat-send "/new")
                     (current-buffer))))
          (superchat-test-script (list '(:text "ok"))
            (with-current-buffer new (superchat-chat-send "hello")))
          (should (equal (plist-get (car (superchat-tape-entries
                                          :tape (superchat-test-chat--tape-of new)))
                                    :scope)
                         (cdr case))))))))

(ert-deftest superchat-chat-recent-tape-past-many-inline-timelines ()
  (superchat-test-chat--with
    (superchat-tape-append "20240101-a" 'user "old" :scope "s/")
    (dotimes (i 150) (superchat-tape-append (format "inline:%d" i) 'user "i" :scope "s/"))
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
                      ("/notes" "notes of this scope" "plums win")))
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
          (superchat-chat-send "/model big-one")
          (superchat-chat-send "/model")
          (should (equal shown "Model: big-one"))))
      (superchat-test-script (list '(:text "ok"))
        (with-current-buffer buffer (superchat-chat-send "hi"))
        (should (equal (plist-get (car superchat-test-requests) :model) "big-one")))
      (should-not (buffer-local-value 'superchat-chat--model (superchat-chat-open "t2"))))))

(ert-deftest superchat-chat-recent-tape-ignores-inline ()
  (superchat-test-chat--with
    (let ((p (expand-file-name "p/" superchat-test-chat--dir))
          (q (expand-file-name "q/" superchat-test-chat--dir)))
      (superchat-tape-append "20240101-a" 'user "old" :scope p)
      (superchat-tape-append "inline:p" 'user "inline" :scope p)
      (superchat-tape-append "other" 'user "x" :scope q)
      (should (equal (superchat-chat-recent-tape p) "20240101-a"))
      (superchat-tape-append "20240102-b" 'user "newer" :scope p)
      (superchat-tape-append "inline:p" 'user "inline again")
      (should (equal (superchat-chat-recent-tape p) "20240102-b")))))

(ert-deftest superchat-chat-command-reuses-recent-timeline ()
  (superchat-test-chat--with
    (superchat-tape-append "20240101-a" 'user "earlier" :scope (superchat-test-chat--scope))
    (superchat)
    (let ((first (current-buffer)))
      (should (equal (superchat-test-chat--tape-of first) "20240101-a"))
      (should (equal (superchat-test-chat--text first) "▸ You\nearlier\n\n"))
      (superchat)
      (should (eq (current-buffer) first))
      (superchat '(4))
      (should-not (eq (current-buffer) first))
      (should-not (superchat-tape-entries
                   :tape (superchat-test-chat--tape-of (current-buffer)))))))

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

(provide 'test-chat)
;;; test-chat.el ends here
