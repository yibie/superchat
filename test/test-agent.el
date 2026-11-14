;;; test-agent.el --- tests for superchat-agent -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-permission)
(require 'superchat-tools)
(require 'superchat-agent)

(defmacro superchat-test-agent--with (&rest body)
  "BODY with a Tape, a tool `echo' (read) and `poke' (write), outside any project."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((default-directory (file-name-as-directory (make-temp-file "sc-agent-" t)))
           (superchat-agent--runs (make-hash-table :test #'equal))
           (superchat-tools--always (make-hash-table :test #'equal)))
       (superchat-tool-define "echo" :class 'read :description "Echo"
                              :args '((:name "x" :type string :required t))
                              :function (lambda (a) (concat "echo:" (plist-get a :x))))
       (superchat-tool-define "poke" :class 'write :description "Poke"
                              :function (lambda (_) "poked"))
       (unwind-protect (progn ,@body)
         (superchat-tool-remove "echo")
         (superchat-tool-remove "poke")
         (delete-directory default-directory t)))))

(defun superchat-test-agent--entries (tape)
  (mapcar (lambda (e) (cons (plist-get e :kind) (plist-get e :text)))
          (superchat-tape-entries :tape tape)))

(defun superchat-test-agent--call (id name &rest args)
  (list :id id :name name :args args))

(defvar superchat-test-agent--seen nil)

(defun superchat-test-agent--silent ()
  "A request function that never answers; it pushes its keys on `superchat-test-agent--seen'."
  (lambda (messages &rest keys)
    (push (cons messages keys) superchat-test-agent--seen)
    (list :cancelled nil :finished nil :request nil)))

(ert-deftest superchat-agent-plain-answer ()
  (superchat-test-agent--with
    (let (texts entries done)
      (superchat-test-script (list '(:text "Hello"))
        (let ((run (superchat-run "t" "hi" :origin '(:buffer "a.el")
                                  :on-text (lambda (s) (push s texts))
                                  :on-entry (lambda (e) (push (plist-get e :kind) entries))
                                  :on-done (lambda (r) (push (plist-get r :status) done)))))
          (should (eq (plist-get run :status) 'done))
          (should-not (superchat-run-active "t")))
        (should (equal (plist-get (car superchat-test-requests) :messages)
                       '((:role user :text "hi")))))
      (should (equal texts '("Hello")))
      (should (equal (nreverse entries) '(user assistant)))
      (should (equal done '(done)))
      (should (equal (superchat-test-agent--entries "t") '((user . "hi") (assistant . "Hello"))))
      (should (equal (plist-get (plist-get (car (superchat-tape-entries :tape "t")) :data) :origin)
                     '(:buffer "a.el"))))))

(ert-deftest superchat-agent-tool-round ()
  (superchat-test-agent--with
    (superchat-test-script
        (list `(:text "looking" :calls (,(superchat-test-agent--call "c1" "echo" :x "a")))
              '(:text "done"))
      (superchat-run "t" "go")
      (should (equal (mapcar #'car (superchat-test-agent--entries "t"))
                     '(user assistant tool assistant)))
      (let* ((second (cadr (reverse superchat-test-requests)))
             (messages (plist-get second :messages)))
        (should (equal (mapcar (lambda (m) (plist-get m :role)) messages) '(user assistant tool)))
        (should (equal (plist-get (nth 1 messages) :calls)
                       (list (superchat-test-agent--call "c1" "echo" :x "a"))))
        (should (equal (nth 2 messages)
                       '(:role tool :text "echo:a" :call-id "c1" :name "echo")))))))

(ert-deftest superchat-agent-two-calls-run-in-order ()
  (superchat-test-agent--with
    (superchat-test-script
        (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "one")
                        ,(superchat-test-agent--call "c2" "echo" :x "two")))
              '(:text "ok"))
      (superchat-run "t" "go")
      (should (equal (mapcar #'cdr (superchat-test-agent--entries "t"))
                     '("go" "" "echo:one" "echo:two" "ok"))))))

(ert-deftest superchat-agent-denied-tool-goes-on ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (superchat-permission-set "t" 'read-only)
    (superchat-test-script
        (list `(:calls (,(superchat-test-agent--call "c1" "poke"))) '(:text "sorry"))
      (let ((run (superchat-run "t" "go")))
        (should (eq (plist-get run :status) 'done)))
      (let ((tool (cl-find 'tool (superchat-tape-entries :tape "t") :key (lambda (e) (plist-get e :kind)))))
        (should (plist-get (plist-get tool :data) :error))
        (should (string-match-p "denied" (plist-get tool :text))))
      (should (equal (plist-get (car (last (superchat-tape-entries :tape "t"))) :text) "sorry")))))

(ert-deftest superchat-agent-model-error ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-script (list '(:error "boom"))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status)))))
      (should (eq status 'error))
      (let ((last (car (last (superchat-tape-entries :tape "t")))))
        (should (eq (plist-get last :kind) 'event))
        (should (equal (plist-get (plist-get last :data) :type) "error"))
        (should (string-match-p "boom" (plist-get last :text))))
      (should-not (superchat-run-active "t")))))

(ert-deftest superchat-agent-max-rounds ()
  (superchat-test-agent--with
    (let ((superchat-max-rounds 2) status
          (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call call call)
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 2)))
      (should (eq status 'error))
      (let ((last (car (last (superchat-tape-entries :tape "t")))))
        (should (equal (plist-get (plist-get last :data) :type) "error"))))))

(ert-deftest superchat-agent-one-run-per-timeline ()
  (superchat-test-agent--with
    (let ((superchat-test-agent--seen nil)
          (superchat-llm-request-function (superchat-test-agent--silent)))
      (let ((run (superchat-run "t" "one")))
        (should (eq (superchat-run-active "t") run))
        (should (eq (plist-get run :status) 'running))
        (should-error (superchat-run "t" "two"))
        (should-not (superchat-run-active "other"))
        (should (equal (mapcar #'cdr (superchat-test-agent--entries "t")) '("one")))))))

(ert-deftest superchat-agent-cancel-without-run ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "x" :scope "home")
    (superchat-cancel "t")
    (should (equal (superchat-test-agent--entries "t") '((user . "x"))))))

(ert-deftest superchat-agent-cancel-while-waiting ()
  (superchat-test-agent--with
    (let ((superchat-test-agent--seen nil) status
          (superchat-llm-request-function (superchat-test-agent--silent)))
      (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
      (superchat-cancel "t")
      (should (eq status 'cancelled))
      (should-not (superchat-run-active "t"))
      (let ((last (car (last (superchat-tape-entries :tape "t")))))
        (should (eq (plist-get last :kind) 'event))
        (should (equal (plist-get (plist-get last :data) :type) "cancelled")))
      (superchat-cancel "t")
      (should (= (length (superchat-tape-entries :tape "t")) 2)))))

(ert-deftest superchat-agent-cancel-inside-the-request ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-script
          (list (lambda (_) (superchat-cancel "t") '(:text "late")))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status)))))
      (should (eq status 'cancelled))
      (should (equal (mapcar #'car (superchat-test-agent--entries "t")) '(user event))))))

(ert-deftest superchat-agent-cancel-marks-calls-without-results ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "a")
                          ,(superchat-test-agent--call "c2" "echo" :x "b")))
                '(:text "never"))
        (superchat-run "t" "go"
                       :on-entry (lambda (e) (when (eq (plist-get e :kind) 'assistant)
                                               (superchat-cancel "t")))
                       :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 1)))
      (should (eq status 'cancelled))
      (let ((tools (cl-remove-if-not (lambda (e) (eq (plist-get e :kind) 'tool))
                                     (superchat-tape-entries :tape "t"))))
        (should (equal (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id)) tools)
                       '("c1" "c2")))
        (should (cl-every (lambda (e) (and (equal (plist-get e :text) "Cancelled.")
                                           (plist-get (plist-get e :data) :error)))
                          tools)))
      (should (eq (plist-get (car (last (superchat-tape-entries :tape "t"))) :kind) 'event)))))

(ert-deftest superchat-agent-cancel-after-first-call ()
  (superchat-test-agent--with
    (superchat-test-script
        (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "a")
                        ,(superchat-test-agent--call "c2" "echo" :x "b"))))
      (superchat-run "t" "go"
                     :on-entry (lambda (e) (when (equal (plist-get e :text) "echo:a")
                                             (superchat-cancel "t"))))
      (should (equal (mapcar #'cdr (superchat-test-agent--entries "t"))
                     '("go" "" "echo:a" "Cancelled." "Cancelled by the user."))))))

(ert-deftest superchat-agent-tools-argument ()
  (superchat-test-agent--with
    (dolist (case `((nil ,(length (superchat-tool-specs))) (none nil) (("echo") 1)))
      (let ((tape (superchat-tape-new-name)))
        (superchat-test-script (list '(:text "a"))
          (superchat-run tape "go" :tools (car case))
          (let ((tools (plist-get (car superchat-test-requests) :tools)))
            (if (cadr case)
                (should (= (length tools) (cadr case)))
              (should-not tools))))))
    (let ((tape (superchat-tape-new-name)))
      (superchat-test-script (list '(:text "a"))
        (superchat-run tape "go" :tools '("echo") :system "EXTRA" :model "m1")
        (should (equal (plist-get (car (plist-get (car superchat-test-requests) :tools)) :name) "echo"))
        (should (equal (plist-get (car superchat-test-requests) :model) "m1"))
        (should (string-match-p "EXTRA" (plist-get (car superchat-test-requests) :system)))))))

(ert-deftest superchat-agent-handoff ()
  (superchat-test-agent--with
    (superchat-test-script
        (list '(:text "A1") '(:text "The summary.\nNEXT: finish it") '(:text "A2"))
      (superchat-run "t" "first")
      (let ((ids (mapcar (lambda (e) (plist-get e :id)) (superchat-tape-entries :tape "t")))
            anchor)
        (superchat-handoff "t" "plan" (lambda (a) (setq anchor a)))
        (should (equal (plist-get anchor :refs) ids))
        (should (eq (plist-get anchor :kind) 'anchor))
        (should (equal (plist-get anchor :text) "The summary."))
        (should (equal (plist-get anchor :data) '(:name "plan" :summary "The summary." :next "finish it")))
        (let ((request (car superchat-test-requests)))
          (should-not (plist-get request :tools))
          (should (equal (mapcar (lambda (m) (plist-get m :role)) (plist-get request :messages))
                         '(user)))
          (should (string-match-p "first" (plist-get (car (plist-get request :messages)) :text))))
        (superchat-run "t" "second")
        (let ((messages (plist-get (car superchat-test-requests) :messages)))
          (should (string-prefix-p "Summary of the conversation so far:\nThe summary."
                                   (plist-get (car messages) :text)))
          (should (equal (mapcar (lambda (m) (plist-get m :role)) messages) '(user user)))
          (should (equal (plist-get (cadr messages) :text) "second")))))))

(ert-deftest superchat-agent-handoff-default-name-and-error ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "x" :scope "home")
    (let (got)
      (superchat-test-script (list '(:text "S"))
        (superchat-handoff "t" nil (lambda (a) (setq got a))))
      (should (equal (plist-get (plist-get got :data) :name) "handoff"))
      (should (equal (plist-get (plist-get got :data) :next) ""))
      (setq got 'unset)
      (superchat-test-script (list '(:error "down"))
        (superchat-handoff "t" nil (lambda (a) (setq got a))))
      (should-not got)
      (should (eq (plist-get (car (last (superchat-tape-entries :tape "t"))) :kind) 'event)))))

(ert-deftest superchat-agent-handoff-before-the-user-entry ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 50))
      (superchat-test-script (list '(:text "old answer") '(:text "The summary.\nNEXT: n") '(:text "new answer"))
        (superchat-run "t" (make-string 60 ?u))
        (should (= (length superchat-test-requests) 1))
        (superchat-run "t" "second")
        (should (= (length superchat-test-requests) 3))
        (let ((handoff (cadr superchat-test-requests))
              (last (car superchat-test-requests)))
          (should-not (plist-get handoff :tools))
          (should (equal (mapcar (lambda (m) (plist-get m :text)) (plist-get last :messages))
                         '("Summary of the conversation so far:\nThe summary.\n\nWhat comes next:\nn"
                           "second"))))
        (should (equal (mapcar #'car (superchat-test-agent--entries "t"))
                       '(user assistant anchor user assistant)))))))

(ert-deftest superchat-agent-no-mid-run-handoff ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 50))
      (superchat-test-script
          (list `(:text ,(make-string 80 ?a)
                  :calls (,(superchat-test-agent--call "c1" "echo" :x "a")))
                '(:text "end"))
        (superchat-run "t" "go")
        (should (= (length superchat-test-requests) 2))
        (should-not (superchat-tape-entries :tape "t" :kinds '(anchor)))))))

(ert-deftest superchat-agent-no-handoff-below-the-limit ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 500))
      (superchat-test-script (list '(:text "a"))
        (superchat-run "t" (make-string 60 ?u))
        (should (= (length superchat-test-requests) 1))))))

(ert-deftest superchat-agent-fork ()
  (superchat-test-agent--with
    (superchat-test-script (list '(:text "A1") '(:text "A2") '(:text "B"))
      (superchat-run "t" "q1")
      (let* ((entries (superchat-tape-entries :tape "t"))
             (a1 (plist-get (nth 1 entries) :id)))
        (superchat-run "t" "q2")
        (let ((fork (superchat-fork "t" a1)))
          (should-not (equal fork "t"))
          (let ((first (car (superchat-tape-entries :tape fork))))
            (should (eq (plist-get first :kind) 'anchor))
            (should (equal (plist-get first :text) ""))
            (should (equal (plist-get first :refs)
                           (list (plist-get (car entries) :id) a1)))
            (should (equal (plist-get first :scope) (plist-get (car entries) :scope))))
          (superchat-run fork "q3")
          (should (equal (mapcar (lambda (m) (plist-get m :text))
                                 (plist-get (car superchat-test-requests) :messages))
                         '("q1" "A1" "q3"))))))))

;; The tool entry of a call being cancelled is its only result.
(ert-deftest superchat-agent-cancel-inside-a-tool ()
  (superchat-test-agent--with
    (superchat-tool-define "stop" :class 'read :description "Stops the run"
                           :function (lambda (_) (superchat-cancel superchat-tool-tape) "stopped"))
    (unwind-protect
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "stop"))) '(:text "never"))
          (let ((run (superchat-run "t" "go")))
            (should (eq (plist-get run :status) 'cancelled)))
          (should (= (length superchat-test-requests) 1))
          (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(tool)))))
          (should-not (superchat-run-active "t")))
      (superchat-tool-remove "stop"))))

(ert-deftest superchat-agent-fork-at-a-call-takes-its-results ()
  (superchat-test-agent--with
    (superchat-test-script
        (list `(:text "using" :calls (,(superchat-test-agent--call "c1" "echo" :x "a")))
              '(:text "done") '(:text "fresh"))
      (superchat-run "t" "q1")
      (let* ((assistant (nth 1 (superchat-tape-entries :tape "t")))
             (fork (superchat-fork "t" (plist-get assistant :id))))
        (should (equal (length (plist-get (car (superchat-tape-entries :tape fork)) :refs)) 3))
        (superchat-run fork "q2")
        (let ((messages (plist-get (car superchat-test-requests) :messages)))
          (should (equal (mapcar (lambda (m) (plist-get m :role)) messages)
                         '(user assistant tool user)))
          (should (equal (plist-get (nth 2 messages) :call-id)
                         (plist-get (car (plist-get (nth 1 messages) :calls)) :id))))))))

;; A run always ends: whatever is signalled becomes an error event.
(ert-deftest superchat-agent-signals-end-the-run ()
  (superchat-test-agent--with
    (dolist (start (list (lambda () (superchat-test-script (list '(:text "x"))
                                      (superchat-run "t" "go" :tools '("nope"))))
                         (lambda () (let ((superchat-llm-request-function
                                           (lambda (&rest _) (error "no network")))) 
                                      (superchat-run "t" "go")))))
      (let ((run (funcall start)))
        (should (eq (plist-get run :status) 'error))
        (should-not (superchat-run-active "t"))
        (let ((last (car (last (superchat-tape-entries :tape "t")))))
          (should (equal (plist-get (plist-get last :data) :type) "error")))))
    (let ((entries (superchat-tape-entries :tape "t" :kinds '(event))))
      (should (string-match-p "nope" (plist-get (car entries) :text)))
      (should (string-match-p "no network" (plist-get (cadr entries) :text))))
    (superchat-test-script (list '(:text "fine"))
      (should (eq (plist-get (superchat-run "t" "again") :status) 'done)))))

(ert-deftest superchat-agent-tool-runs-with-quit-enabled ()
  (superchat-test-agent--with
    (let ((superchat-test-async t) seen)
      (superchat-tool-define "probe" :class 'read :description "Probe"
                             :function (lambda (_) (setq seen (list inhibit-quit)) "ok"))
      (unwind-protect
          (superchat-test-script
              (list `(:calls (,(superchat-test-agent--call "c1" "probe"))) '(:text "done"))
            (superchat-run "t" "go")
            (superchat-test-drain)
            (should (equal seen '(nil)))
            (should-not (superchat-run-active "t")))
        (superchat-tool-remove "probe")))))

(ert-deftest superchat-agent-handoff-occupies-its-timeline ()
  (superchat-test-agent--with
    (let ((superchat-test-async t))
      (superchat-tape-append "t" 'user "x" :scope "home")
      (superchat-test-script (list '(:text "S") '(:text "answer") '(:text "S2"))
        (superchat-handoff "t" "one")
        (should (superchat-run-active "t"))
        (should-error (superchat-run "t" "y"))
        (should-error (superchat-handoff "t" "two"))
        (superchat-test-drain)
        (should-not (superchat-run-active "t"))
        (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(anchor)))))
        (superchat-run "t" "y")
        (should-error (superchat-handoff "t" "two"))
        (superchat-test-drain)
        (should-not (superchat-run-active "t"))
        (superchat-handoff "t" "two")
        (superchat-test-drain)
        (should (= 2 (length (superchat-tape-entries :tape "t" :kinds '(anchor)))))))))

(ert-deftest superchat-agent-failed-handoff-keeps-the-user-text ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 50) status)
      (superchat-test-script (list '(:text "old") '(:error "down"))
        (superchat-run "t" (make-string 60 ?u))
        (superchat-run "t" "second" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 2)))
      (should (eq status 'error))
      (should (equal (mapcar #'car (superchat-test-agent--entries "t"))
                     '(user assistant event user)))
      (should-not (superchat-run-active "t")))))

(defun superchat-test-agent--fork-messages (fork)
  "Roles of the first request of a run on FORK."
  (superchat-test-script (list '(:text "ok"))
    (superchat-run fork "next")
    (mapcar (lambda (m) (plist-get m :role))
            (plist-get (car superchat-test-requests) :messages))))

(ert-deftest superchat-agent-fork-between-a-call-and-its-results ()
  (superchat-test-agent--with
    (let* ((calls (list (superchat-test-agent--call "c1" "echo" :x "a")
                        (superchat-test-agent--call "c2" "echo" :x "b")))
           (ids (list (plist-get (superchat-tape-append "t" 'user "q" :scope "home") :id)
                      (plist-get (superchat-tape-append "t" 'assistant "" :data (list :calls calls)) :id)
                      (plist-get (superchat-tape-append "t" 'event "asked" :data '(:type "permission")) :id)
                      (plist-get (superchat-tape-append "t" 'tool "r1" :data '(:call-id "c1" :name "echo")) :id)
                      (plist-get (superchat-tape-append "t" 'tool "r2" :data '(:call-id "c2" :name "echo")) :id))))
      (dolist (at (list (nth 2 ids) (nth 3 ids)))
        (should (equal (superchat-test-agent--fork-messages (superchat-fork "t" at))
                       '(user assistant tool tool user)))))))

(provide 'test-agent)
;;; test-agent.el ends here
