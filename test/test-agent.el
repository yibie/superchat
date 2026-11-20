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
           (superchat-agent--runs (make-hash-table :test #'equal)))
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

(defmacro superchat-test-agent--retrying (responses &rest body)
  "BODY with RESPONSES scripted and waits of 0.01 s between retries."
  (declare (indent 1))
  `(let ((superchat-agent-retry-waits '(0.01 0.01 0.01 0.01 0.01)))
     (superchat-test-script ,responses ,@body)))

(defun superchat-test-agent--events (tape)
  (cl-loop for e in (superchat-tape-entries :tape tape)
           when (eq (plist-get e :kind) 'event) collect (plist-get (plist-get e :data) :type)))

(ert-deftest superchat-agent-a-passing-failure-is-retried-where-it-failed ()
  (superchat-test-agent--with
    (let (status seen)
      (superchat-test-agent--retrying
          (list `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))
                '(:error "503 overloaded" :kind server :status 503)
                '(:error "timed out" :kind timeout)
                '(:text "done"))
        (superchat-run "t" "go" :on-entry (lambda (e) (push e seen))
                       :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (superchat-run-active "t"))
        (superchat-test-wait (lambda () status))
        (should (= (length superchat-test-requests) 4))
        ;; the retries send what the failed request sent
        (should (equal (plist-get (nth 0 superchat-test-requests) :messages)
                       (plist-get (nth 2 superchat-test-requests) :messages))))
      (should (eq status 'done))
      (should (equal (superchat-test-agent--events "t") '("retry" "retry")))
      (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(tool)))))
      (let ((retry (cl-find-if (lambda (e) (eq (plist-get e :kind) 'event)) (reverse seen))))
        (should (equal (plist-get retry :data)
                       '(:type "retry" :kind "server" :status 503 :wait 0.01 :attempt 1 :of 5)))
        (should (string-match-p "503 overloaded.*0.01 s (1/5)" (plist-get retry :text)))))))

(ert-deftest superchat-agent-the-count-of-retries-starts-again-after-an-answer ()
  (superchat-test-agent--with
    (let ((superchat-agent-retry-waits '(0.01)) status)
      (superchat-test-script
          (list '(:error "x" :kind network)
                `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))
                '(:error "x" :kind network)
                '(:text "done"))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (superchat-test-wait (lambda () status)))
      (should (eq status 'done))
      (should (equal (superchat-test-agent--events "t") '("retry" "retry"))))))

(ert-deftest superchat-agent-a-failure-that-will-not-pass-ends-the-run ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-agent--retrying (list '(:error "401 bad key" :kind other :status 401))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 1)))
      (should (eq status 'error))
      (should (equal (superchat-test-agent--events "t") '("error"))))))

(ert-deftest superchat-agent-retries-run-out ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-agent--retrying (make-list 6 '(:error "429 slow" :kind rate-limit :status 429))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (superchat-test-wait (lambda () status))
        (should (= (length superchat-test-requests) 6)))
      (should (eq status 'error))
      (should (equal (superchat-test-agent--events "t")
                     '("retry" "retry" "retry" "retry" "retry" "error"))))))

(ert-deftest superchat-agent-cancel-stops-the-wait ()
  (superchat-test-agent--with
    (let ((superchat-agent-retry-waits '(0.05)) status)
      (superchat-test-script (list '(:error "x" :kind server))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (plist-get (superchat-run-active "t") :timer))
        (superchat-cancel "t")
        (should (eq status 'cancelled))
        (let ((end (+ (float-time) 0.1)))
          (while (< (float-time) end) (accept-process-output nil 0.01)))
        ;; no request after the cancel: the script has no response left for one
        (should (= (length superchat-test-requests) 1)))
      (should-not (superchat-run-active "t"))
      (should (equal (superchat-test-agent--events "t") '("retry" "cancelled"))))))

(defun superchat-test-agent--cut-off (&rest entries)
  "Append ENTRIES, each (KIND TEXT DATA), to timeline t after a user entry."
  (superchat-tape-append "t" 'user "first" :scope "home")
  (pcase-dolist (`(,kind ,text ,data) entries)
    (superchat-tape-append "t" kind text :data data)))

(ert-deftest superchat-agent-a-run-cut-off-by-emacs-is-marked-before-the-next ()
  (superchat-test-agent--with
    (superchat-test-agent--cut-off
     `(assistant "" (:calls (,(superchat-test-agent--call "c1" "echo" :x "a")
                             ,(superchat-test-agent--call "c2" "poke"))))
     '(tool "echo:a" (:call-id "c1" :name "echo" :error nil)))
    (superchat-test-script (list '(:text "ok"))
      (superchat-run "t" "again")
      (let ((sent (plist-get (car superchat-test-requests) :messages)))
        (should (string-match-p "\\`Interrupted: Emacs stopped"
                                (plist-get (cl-find "c2" sent :key (lambda (m) (plist-get m :call-id))
                                                    :test #'equal)
                                           :text)))))
    (let ((entries (nthcdr 3 (superchat-tape-entries :tape "t"))))
      (should (equal (mapcar (lambda (e) (plist-get e :kind)) entries) '(tool event user assistant)))
      (should (equal (plist-get (car entries) :data) '(:call-id "c2" :name "poke" :error t)))
      (should (equal (plist-get (plist-get (nth 1 entries) :data) :type) "interrupted")))))

(ert-deftest superchat-agent-only-a-run-that-never-ended-is-marked ()
  (dolist (case `((((assistant "done" nil)) nil)
                  (((assistant "" (:calls (,(superchat-test-agent--call "c1" "poke"))))
                    (tool "Cancelled." (:call-id "c1" :name "poke" :error t))
                    (event "Cancelled by the user." (:type "cancelled")))
                   nil)
                  (((tool "x" (:call-id "c9" :name "echo" :error nil))
                    (event "boom" (:type "error")))
                   nil)
                  (() ("interrupted"))
                  (((assistant "" (:calls (,(superchat-test-agent--call "c1" "note"))))
                    (note "kept" (:title "k")))
                   ("interrupted"))
                  (((event "x" (:type "retry"))) ("interrupted"))))
    (superchat-test-agent--with
      (apply #'superchat-test-agent--cut-off (car case))
      (let ((before (length (superchat-tape-entries :tape "t"))))
        (superchat-test-script (list '(:text "ok")) (superchat-run "t" "again"))
        (should (equal (cl-loop for e in (nthcdr before (superchat-tape-entries :tape "t"))
                                when (eq (plist-get e :kind) 'event)
                                collect (plist-get (plist-get e :data) :type))
                       (cadr case))))))
  ;; A failed handoff ends with its error event and then the user entry.
  (superchat-test-agent--with
    (superchat-tape-append "t" 'event "Handoff failed: x" :data '(:type "error") :scope "home")
    (superchat-tape-append "t" 'user "hello")
    (superchat-test-script (list '(:text "ok")) (superchat-run "t" "again"))
    (should-not (cl-find "interrupted" (superchat-tape-entries :tape "t" :kinds '(event))
                         :key (lambda (e) (plist-get (plist-get e :data) :type)) :test #'equal))))

(defmacro superchat-test-agent--asking-nobody (&rest body)
  "BODY at level ask, where asking the person in the minibuffer is an error."
  (declare (indent 0))
  `(let ((superchat-permission-ask-function (lambda (&rest _) (error "Asked in the minibuffer"))))
     (superchat-permission-set "t" 'ask)
     ,@body))

(ert-deftest superchat-agent-an-unattended-run-waits-for-its-answer ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'event "" :data '(:type "level" :level "ask") :scope "home")
    (superchat-test-agent--asking-nobody
      (let (status)
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "a")
                            ,(superchat-test-agent--call "c2" "poke")
                            ,(superchat-test-agent--call "c3" "echo" :x "b")))
                  '(:text "done"))
          (superchat-run "t" "go" :unattended t
                         :on-done (lambda (r) (setq status (plist-get r :status))))
          (should (eq status 'waiting))
          (should-not (superchat-run-active "t"))
          (should (equal (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id))
                                 (superchat-tape-entries :tape "t" :kinds '(tool)))
                         '("c1")))
          (should (equal (plist-get (plist-get (superchat-question "t") :data) :call-id) "c2"))
          (should-error (superchat-run "t" "again"))
          (setq status nil)
          (superchat-answer "t" 'allow :on-done (lambda (r) (setq status (plist-get r :status))))
          (should (eq status 'done))
          (should-not (superchat-question "t"))
          (should (equal (mapcar (lambda (e) (cons (plist-get (plist-get e :data) :call-id)
                                                   (plist-get e :text)))
                                 (superchat-tape-entries :tape "t" :kinds '(tool)))
                         '(("c1" . "echo:a") ("c2" . "poked") ("c3" . "echo:b"))))
          (should (= (length superchat-test-requests) 2)))))))

(ert-deftest superchat-agent-a-denied-answer-goes-on ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'event "" :data '(:type "level" :level "ask") :scope "home")
    (superchat-test-agent--asking-nobody
      (let (status)
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c2" "poke"))) '(:text "fine"))
          (superchat-run "t" "go" :unattended t)
          ;; Emacs starts again: no run in memory, the question on the Tape.
          (clrhash superchat-agent--runs)
          (superchat-answer "t" 'deny :on-done (lambda (r) (setq status (plist-get r :status)))))
        (should (eq status 'done))
        (should (string-match-p "Permission denied"
                                (plist-get (car (superchat-tape-entries :tape "t" :kinds '(tool)))
                                           :text)))
        (should-not (member "interrupted" (superchat-test-agent--events "t")))
        (should-error (superchat-answer "t" 'allow))))))

(ert-deftest superchat-agent-no-round-limit-when-nil ()
  (superchat-test-agent--with
    (let ((superchat-max-rounds nil) status (paused 'unset)
          (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call call call '(:text "Done."))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status)
                                                          paused (plist-get r :paused))))
        (should-not paused)
        (should (= (length superchat-test-requests) 4)))
      (should (eq status 'done)))))

(ert-deftest superchat-agent-at-auto-the-judge-lets-a-run-go-on ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (superchat-permission-set "t" 'auto)
    (let ((superchat-max-rounds 2) (superchat-agent-continues 1) status
          (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call call '(:text "Status: continue\nLook-back one.")
                                   '(:text "CONTINUE\nTwo more files to read.")
                                   call call '(:text "Look-back two."))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 7))
        (let ((sent (reverse superchat-test-requests)))
          (should (string-search superchat-limit-note (plist-get (nth 2 sent) :system)))
          ;; The judge: its own prompt, no tool, the task, the facts and the look-back.
          (should (equal (plist-get (nth 3 sent) :system) superchat-limit-judge-prompt))
          (should-not (plist-get (nth 3 sent) :tools))
          (let ((asked (plist-get (car (plist-get (nth 3 sent) :messages)) :text)))
            (should (string-search "Task:\ngo" asked))
            (should (string-search "Calls: echo 2." asked))
            (should (string-search "Look-back one." asked)))
          ;; The first request after going on sees the look-back, once, in its
          ;; system prompt; no request carries it as a message.
          (should (string-search "Look-back one." (plist-get (nth 4 sent) :system)))
          (should-not (string-search "Look-back one." (plist-get (nth 5 sent) :system)))
          (dolist (r (list (nth 4 sent) (nth 5 sent)))
            (should-not (seq-find (lambda (m) (string-search "Look-back one." (or (plist-get m :text) "")))
                                  (plist-get r :messages))))
          (should (string-search superchat-limit-note (plist-get (nth 6 sent) :system)))))
      ;; It went on once, then paused as at any level.
      (should (eq status 'done))
      (should (equal (seq-filter (lambda (ty) (memq ty '(verdict look-back continued paused)))
                                 (superchat-test-agent--types "t"))
                     '(verdict look-back continued paused))))))

(ert-deftest superchat-agent-the-judge-decides-a-look-back-that-says-finished ()
  (dolist (case '(("DONE: all read." nil) ("**BLOCKED** needs a key." t)))
    (superchat-test-agent--with
      (superchat-tape-append "t" 'user "earlier" :scope "home")
      (superchat-permission-set "t" 'auto)
      (let ((superchat-max-rounds 2) run
            (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
        (superchat-test-script (list call call '(:text "**Status: finished**\nThe sum is 21.")
                                     `(:text ,(car case)))
          (superchat-run "t" "go" :on-done (lambda (r) (setq run r)))
          (should (= (length superchat-test-requests) 4))
          ;; The look-back request carries the facts counted from the Tape.
          (let ((system (plist-get (cadr superchat-test-requests) :system)))
            (should (string-search "Facts of this run, counted from the Tape:" system))
            (should (string-search "Calls: echo 2." system))))
        (should (eq (plist-get run :status) 'done))
        (should (eq (and (plist-get run :paused) t) (cadr case)))
        (should-not (memq 'continued (superchat-test-agent--types "t")))
        ;; The answer is the look-back without its status line.
        (should (equal (plist-get (car (last (superchat-tape-entries :tape "t" :kinds '(assistant)))) :text)
                       "The sum is 21."))))))

(ert-deftest superchat-agent-a-look-back-that-says-blocked-stops-at-auto ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (superchat-permission-set "t" 'auto)
    (let ((superchat-max-rounds 1) status
          (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call '(:text "Status: blocked\nI need the key."))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status))))
        (should (= (length superchat-test-requests) 2)))
      (should (eq status 'done))
      (should-not (memq 'continued (superchat-test-agent--types "t"))))))

(ert-deftest superchat-agent-a-run-going-round-is-redirected-once ()
  (superchat-test-agent--with
    (let ((superchat-guard-min-calls 3) (superchat-guard-churn 2)
          (poke `(:calls (,(superchat-test-agent--call "c" "echo" :x "a"))))
          (edit `(:calls ((:id "e" :name "edit" :args (:path "x.el" :old "a" :new "b"))))))
      (superchat-tool-define "edit" :class 'read :description "Edit" :function (lambda (_) "Replaced."))
      (unwind-protect
          (superchat-test-script (list poke edit edit poke poke '(:text "Done."))
            (superchat-run "t" "go")
            (let ((systems (mapcar (lambda (r) (plist-get r :system)) (reverse superchat-test-requests))))
              ;; After three calls, two of them to x.el, the next request is redirected, once.
              (should (equal (mapcar (lambda (s) (and (string-search "Process check" s) t)) systems)
                             '(nil nil nil t nil nil))))
            (should (equal (cl-count 'redirect (superchat-test-agent--types "t")) 1)))
        (load "superchat-tools")))))

(ert-deftest superchat-agent-max-rounds ()
  (superchat-test-agent--with
    (let ((superchat-max-rounds 2) status paused
          (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call call '(:text "Here is what I have."))
        (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status)
                                                          paused (plist-get r :paused))))
        ;; A caller tells a pause at the limit from an answer.
        (should paused)
        ;; Two responses, then the one the limit asks for, with no tools.
        (should (= (length superchat-test-requests) 3))
        (let ((ended (car superchat-test-requests)))
          ;; The tools stay offered, so a provider cannot leak its call syntax
          ;; as text; the system line says to call none.
          (should (plist-get ended :tools))
          (should (string-search superchat-limit-note (plist-get ended :system)))))
      (should (eq status 'done))
      ;; The calls of the last response ran: every call has its result.
      (should (equal (superchat-test-agent--types "t")
                     '(user assistant tool assistant tool assistant paused)))
      ;; The answer the limit asked for is on the Tape, and claims no call.
      (let ((answer (car (last (superchat-tape-entries :tape "t" :kinds '(assistant))))))
        (should (equal (plist-get answer :text) "Here is what I have."))
        (should-not (plist-get (plist-get answer :data) :calls)))
      (should (string-match-p "Paused at the round limit: 2 model responses"
                              (plist-get (car (last (superchat-tape-entries :tape "t"))) :text)))
      ;; The event says what the run did, from the Tape: the last response
      ;; claims a call it never ran, so two calls were made, not three.
      (should (string-match-p ": 2 model responses, 2 tool calls\\."
                              (plist-get (car (last (superchat-tape-entries :tape "t"))) :text))))))

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

;; A cancel from inside ON-ENTRY emits the entries after the one in hand;
;; the outer emit must not give them a second time.
(ert-deftest superchat-agent-on-entry-that-cancels-sees-each-entry-once ()
  (superchat-test-agent--with
    (let ((superchat-default-level 'ask)
          (superchat-permission-ask-function (lambda (&rest _) 'allow))
          ids)
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "c1" "poke"))))
        (superchat-run "t" "go"
                       :on-entry
                       (lambda (e)
                         (push (plist-get e :id) ids)
                         (when (equal (plist-get (plist-get e :data) :type)
                                      "permission")
                           (superchat-cancel "t")))))
      (should (equal (nreverse ids)
                     (mapcar (lambda (e) (plist-get e :id))
                             (superchat-tape-entries :tape "t")))))))

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

;; A callback that always signals still cannot leave the run stuck.
(ert-deftest superchat-agent-signalling-on-entry-ends-the-run ()
  (superchat-test-agent--with
    (let (status)
      (superchat-test-script (list '(:text "x"))
        (should-error (superchat-run "t" "go" :on-entry (lambda (_) (error "callback"))
                                     :on-done (lambda (r) (setq status (plist-get r :status))))))
      (should (eq status 'error))
      (should-not (superchat-run-active "t"))
      (superchat-test-script (list '(:text "fine"))
        (should (eq (plist-get (superchat-run "t" "again") :status) 'done))))))

;; The same holds for a cancel: its own entries reach the callback too.
(ert-deftest superchat-agent-signalling-on-entry-cannot-block-a-cancel ()
  (superchat-test-agent--with
    (let ((superchat-test-async t) (break nil) status)
      (superchat-test-script (list '(:text "never"))
        (superchat-run "t" "go"
                       :on-entry (lambda (_) (when break (error "callback")))
                       :on-done (lambda (r) (setq status (plist-get r :status))))
        (setq break t)
        (should-error (superchat-cancel "t"))
        (should (eq status 'cancelled))
        (should-not (superchat-run-active "t"))
        (superchat-test-drain)))))

(ert-deftest superchat-agent-cancel-a-pending-handoff ()
  (superchat-test-agent--with
    (let ((superchat-test-async t))
      (superchat-tape-append "t" 'user "x" :scope "home")
      (superchat-test-script (list '(:text "S") '(:text "S"))
        (superchat-handoff "t")
        (should (superchat-run-active "t"))
        (superchat-cancel "t")
        (should-not (superchat-run-active "t"))
        (superchat-test-drain)
        (should (equal (mapcar #'car (superchat-test-agent--entries "t")) '(user event)))
        (superchat-handoff "t")
        (superchat-test-drain)
        (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(anchor)))))))))

(ert-deftest superchat-agent-cancel-during-the-handoff-keeps-the-user-text ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 50) status)
      (superchat-test-script (list '(:text "old"))
        (superchat-run "t" (make-string 60 ?u)))
      (let ((superchat-test-async t))
        (superchat-test-script (list '(:text "S"))
          (superchat-run "t" "second" :origin '(:buffer "b.el")
                         :on-done (lambda (r) (setq status (plist-get r :status))))
          (superchat-cancel "t")
          (superchat-test-drain)))
      (should (eq status 'cancelled))
      (should (equal (mapcar #'car (superchat-test-agent--entries "t"))
                     '(user assistant user event)))
      (should (equal (plist-get (nth 2 (superchat-tape-entries :tape "t")) :text) "second"))
      (should-not (superchat-run-active "t")))))

(ert-deftest superchat-agent-scope-of-a-new-timeline ()
  (superchat-test-agent--with
    (superchat-test-script (list '(:text "a") '(:text "b") '(:text "c"))
      (superchat-run "a" "go" :scope "/given/")
      (superchat-run "b" "go")
      (superchat-run "a" "again" :scope "/ignored/")
      (should (equal (mapcar (lambda (e) (plist-get e :scope)) (superchat-tape-entries :tape "a"))
                     '("/given/" "/given/" "/given/" "/given/")))
      (should (equal (plist-get (car (superchat-tape-entries :tape "b")) :scope) "home")))))

(ert-deftest superchat-agent-handoff-text-is-cut ()
  (superchat-test-agent--with
    (dolist (word '("AAAA" "BBBB" "CCCC"))
      (superchat-tape-append "t" 'user (concat word (make-string 80 ?x)) :scope "home"))
    (let ((superchat-context-chars 150))
      (superchat-test-script (list '(:text "S"))
        (superchat-handoff "t")
        (let ((text (plist-get (car (plist-get (car superchat-test-requests) :messages)) :text)))
          (should (string-match-p "CCCC" text))
          (should-not (string-match-p "AAAA" text))
          (should (string-match-p "left out" text)))))))

;; An asynchronous tool: DONE from a 0.2 s timer, or later by hand.
(defvar superchat-test-agent--stopped nil "How many times the slow tool was stopped.")
(defvar superchat-test-agent--done nil "DONE of the slow tool's latest call.")

(defmacro superchat-test-agent--with-slow (seconds &rest body)
  "BODY with tool `slow', which answers \"slow\" after SECONDS (nil: never by itself)."
  (declare (indent 1))
  `(let ((superchat-test-agent--stopped 0) (superchat-test-agent--done nil))
     (superchat-tool-define
      "slow" :class 'read :async t :description "Slow"
      :function (lambda (_ done)
                  (setq superchat-test-agent--done done)
                  (let ((timer (when ,seconds
                                 (run-at-time ,seconds nil (lambda () (funcall done "slow"))))))
                    (lambda ()
                      (cl-incf superchat-test-agent--stopped)
                      (when timer (cancel-timer timer))))))
     (unwind-protect (progn ,@body)
       (superchat-tool-remove "slow"))))

(ert-deftest superchat-agent-async-tool-then-sync-tool-in-order ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow 0.1
      (let (status entries)
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "slow")
                            ,(superchat-test-agent--call "c2" "echo" :x "two")))
                  '(:text "all done"))
          (let ((run (superchat-run "t" "go"
                                    :on-entry (lambda (e) (push (plist-get e :text) entries))
                                    :on-done (lambda (r) (setq status (plist-get r :status))))))
            ;; Returned before the first result: Emacs was not held.
            (should (eq (plist-get run :status) 'running))
            (should (eq (superchat-run-active "t") run))
            (should (= (length superchat-test-requests) 1))
            (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))
            (superchat-test-wait (lambda () status)))
          (should (eq status 'done))
          (should (equal (mapcar #'cdr (superchat-test-agent--entries "t"))
                         '("go" "" "slow" "echo:two" "all done")))
          (should (equal (nreverse entries) '("go" "" "slow" "echo:two" "all done")))
          (should (equal (mapcar (lambda (m) (plist-get m :role))
                                 (plist-get (car superchat-test-requests) :messages))
                         '(user assistant tool tool)))
          (should (= (length superchat-test-requests) 2))
          (should-not (superchat-run-active "t")))))))

(ert-deftest superchat-agent-second-run-is-refused-while-a-tool-runs ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow nil
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "c1" "slow"))) '(:text "x"))
        (superchat-run "t" "go")
        (should (superchat-run-active "t"))
        (should-error (superchat-run "t" "again"))
        (should-error (superchat-handoff "t"))
        (superchat-cancel "t")
        (should-not (superchat-run-active "t"))))))

(ert-deftest superchat-agent-cancel-while-an-async-tool-runs ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow nil
      (let ((statuses nil) (superchat-test-async t))
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "slow")
                            ,(superchat-test-agent--call "c2" "echo" :x "b")))
                  '(:text "never"))
          (superchat-run "t" "go" :on-done (lambda (r) (push (plist-get r :status) statuses)))
          (superchat-test-wait (lambda () superchat-test-agent--done))
          (superchat-cancel "t")
          (should (= superchat-test-agent--stopped 1))
          (should (equal statuses '(cancelled)))
          (should-not (superchat-run-active "t"))
          (should (equal (mapcar #'cdr (superchat-test-agent--entries "t"))
                         '("go" "" "Cancelled." "Cancelled." "Cancelled by the user.")))
          (let ((tools (superchat-tape-entries :tape "t" :kinds '(tool))))
            (should (equal (mapcar (lambda (e) (plist-get (plist-get e :data) :call-id)) tools)
                           '("c1" "c2")))
            (should (cl-every (lambda (e) (plist-get (plist-get e :data) :error)) tools)))
          (should (equal (plist-get (plist-get (car (last (superchat-tape-entries :tape "t")))
                                               :data)
                                    :type)
                         "cancelled"))
          ;; A late answer changes nothing and starts nothing.
          (let ((before (length (superchat-tape-entries :tape "t"))))
            (funcall superchat-test-agent--done "late")
            (superchat-test-drain)
            (should (= before (length (superchat-tape-entries :tape "t"))))
            (should (= (length superchat-test-requests) 1))
            (should (equal statuses '(cancelled))))
          ;; The timeline is free.
          (superchat-test-script (list '(:text "again"))
            (superchat-run "t" "next")
            (superchat-test-drain)
            (should-not (superchat-run-active "t"))))))))

(ert-deftest superchat-agent-cancelled-run-does-not-start-the-next-call ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow 0.1
      (let ((ran nil))
        (superchat-tool-define "after" :class 'read :function (lambda (_) (setq ran t) "x"))
        (unwind-protect
            (superchat-test-script
                (list `(:calls (,(superchat-test-agent--call "c1" "slow")
                                ,(superchat-test-agent--call "c2" "after"))))
              (superchat-run "t" "go")
              (superchat-cancel "t")
              (let ((end (+ (float-time) 0.3)))
                (while (< (float-time) end) (accept-process-output nil 0.05)))
              (should-not ran)
              (should (= superchat-test-agent--stopped 1)))
          (superchat-tool-remove "after"))))))

(ert-deftest superchat-agent-signal-in-the-continuation-of-an-async-tool-ends-the-run ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow 0.1
      (let (status)
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "slow"))) '(:text "never"))
          (superchat-run "t" "go"
                         :on-entry (lambda (e) (when (eq (plist-get e :kind) 'tool)
                                                 (error "callback")))
                         :on-done (lambda (r) (setq status (plist-get r :status))))
          (should (superchat-run-active "t"))
          ;; The signal leaves the timer; Emacs reports it there.
          (superchat-test-wait (lambda () status))
          (should (eq status 'error))
          (should-not (superchat-run-active "t"))
          (should (= (length superchat-test-requests) 1))
          (should (equal (plist-get (plist-get (car (last (superchat-tape-entries :tape "t"))) :data)
                                    :type)
                         "error"))
          (superchat-test-script (list '(:text "fine"))
            (should (eq (plist-get (superchat-run "t" "again") :status) 'done))))))))

(ert-deftest superchat-agent-async-tool-that-signals-is-an-error-result ()
  (superchat-test-agent--with
    (superchat-tool-define "bad" :class 'read :async t :function (lambda (_ _d) (error "no start")))
    (unwind-protect
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "bad"))) '(:text "ok"))
          (should (eq (plist-get (superchat-run "t" "go") :status) 'done))
          (should (string-match-p "no start"
                                  (plist-get (car (superchat-tape-entries :tape "t" :kinds '(tool)))
                                             :text))))
      (superchat-tool-remove "bad"))))

(ert-deftest superchat-agent-ephemeral-run-is-not-a-conversation ()
  (superchat-test-agent--with
    (superchat-test-script (list '(:text "plums") '(:text "kept"))
      (superchat-run "e" "ephemeral plums question" :ephemeral t)
      (superchat-run "n" "normal question"))
    (should (eq (plist-get (plist-get (car (superchat-tape-entries :tape "e")) :data) :ephemeral) t))
    (should-not (plist-get (plist-get (car (superchat-tape-entries :tape "n")) :data) :ephemeral))
    (should (equal (mapcar (lambda (e) (plist-get e :tape)) (superchat-view 'timelines))
                   '("n")))
    (should (member "e" (mapcar (lambda (e) (plist-get e :tape))
                                (superchat-view 'search :query "plums"))))))

(ert-deftest superchat-agent-result-text-and-error ()
  (superchat-test-agent--with
    (let (runs)
      (superchat-test-script
          (list '(:text "Hello")
                (list :text "first" :calls (list (superchat-test-agent--call "c1" "echo" :x "a")))
                '(:text "last answer")
                '(:error "model broke"))
        (dolist (tape '("a" "b" "c"))
          (superchat-run tape "go" :on-done (lambda (r) (push r runs)))))
      (setq runs (nreverse runs))
      (should (equal (mapcar (lambda (r) (plist-get r :status)) runs) '(done done error)))
      (should (equal (plist-get (nth 0 runs) :text) "Hello"))
      (should (equal (plist-get (nth 1 runs) :text) "last answer"))
      (should-not (plist-get (nth 1 runs) :error))
      (should-not (plist-get (nth 2 runs) :text))
      (should (equal (plist-get (nth 2 runs) :error) "model broke")))))

(ert-deftest superchat-agent-result-text-nil-when-cancelled-early ()
  (superchat-test-agent--with
    (let ((superchat-test-agent--seen nil) result
          (superchat-llm-request-function (superchat-test-agent--silent)))
      (superchat-run "t" "go" :on-done (lambda (r) (setq result r)))
      (superchat-cancel "t")
      (should (eq (plist-get result :status) 'cancelled))
      (should-not (plist-get result :text)))))

(defun superchat-test-agent--well-formed (messages)
  "Every tool message answers an earlier call, every call has its result."
  (cl-loop for (m . later) on messages
           when (eq (plist-get m :role) 'tool)
           do (should (cl-some (lambda (a) (cl-some (lambda (c) (equal (plist-get c :id)
                                                                      (plist-get m :call-id)))
                                                    (plist-get a :calls)))
                               (cl-subseq messages 0 (- (length messages) (length later) 1))))
           when (eq (plist-get m :role) 'assistant)
           do (dolist (c (plist-get m :calls))
                (should (cl-some (lambda (r) (and (eq (plist-get r :role) 'tool)
                                                  (equal (plist-get r :call-id) (plist-get c :id))))
                                 later)))))

(defun superchat-test-agent--rounds (n)
  "N scripted responses that call echo with a long argument, then an answer."
  (append (cl-loop for i below n
                   collect (list :text (format "round %d of the work, some words" i)
                                 :calls (list (superchat-test-agent--call
                                               (format "c%d" i) "echo" :x (make-string 100 ?x)))))
          (list '(:text "finished"))))

(ert-deftest superchat-agent-budget-leaves-out-the-oldest ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 700))
      (superchat-tape-append "t" 'anchor "SUM" :scope "home"
                             :data '(:name "n" :summary "SUM" :next ""))
      (let (requests (left-out nil))
        (superchat-test-script (superchat-test-agent--rounds 9)
          (superchat-run "t" "go")
          (setq requests (reverse superchat-test-requests)))
        (should (= (length requests) 10))
        (cl-loop
         for request in requests for k from 0
         for m = (plist-get request :messages)
         for standin = (seq-find (lambda (x) (string-match-p "earlier entries" (plist-get x :text))) m)
         do
         ;; the entries on the Tape when request K was made: anchor, user, K rounds of two
         (should (<= (superchat-test-agent--size m) 700))
         (should (string-prefix-p "Summary of the conversation so far:" (plist-get (nth 0 m) :text)))
         (should (equal (plist-get (nth 1 m) :text) "go"))
         (superchat-test-agent--well-formed m)
         (if (null standin)
             (should (= (length m) (+ 2 (* 2 k))))
           (setq left-out t)
           (should (eq (nth 2 m) standin))
           (should (string-match-p
                    (format "\\`\\[%d earlier entries of this conversation are left out of this request; recall and tape_query can read them\\.\\]\\'"
                            (- (+ 2 (* 2 k)) (1- (length m))))
                    (plist-get standin :text)))
           ;; the latest assistant entry and its result stay
           (should (equal (plist-get (car (last m)) :call-id) (format "c%d" (1- k))))))
        (should left-out)
        (should (equal (mapcar (lambda (e) (plist-get e :kind)) (superchat-tape-entries :tape "t"))
                       (append '(anchor user) (apply #'append (make-list 9 '(assistant tool)))
                               '(assistant))))))))

(ert-deftest superchat-agent-budget-over-the-bound-only-by-what-is-kept ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 200))
      (superchat-test-script (superchat-test-agent--rounds 2)
        (superchat-run "t" "go")
        (let ((m (plist-get (car superchat-test-requests) :messages)))
          (should (equal (mapcar (lambda (x) (plist-get x :role)) m)
                         '(user user assistant tool)))
          (superchat-test-agent--well-formed m))))))

;; Two results of one response, with a permission event between the call
;; and them, leave together.
(ert-deftest superchat-agent-budget-keeps-all-results-of-a-response-together ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 400)
          (superchat-default-level 'ask)
          (superchat-permission-ask-function (lambda (&rest _) 'allow)))
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "a" "poke")
                          ,(superchat-test-agent--call "b" "echo" :x (make-string 300 ?x))
                          ,(superchat-test-agent--call "c" "echo" :x "s")))
                `(:calls (,(superchat-test-agent--call "d" "echo" :x "t")))
                '(:text "done"))
        (superchat-run "t" "go")
        (should (= (length superchat-test-requests) 3))
        (dolist (request superchat-test-requests)
          (superchat-test-agent--well-formed (plist-get request :messages)))
        (should (seq-find (lambda (x) (string-match-p "left out" (plist-get x :text)))
                          (plist-get (car superchat-test-requests) :messages)))))))

(ert-deftest superchat-agent-request-under-the-bound-is-unchanged ()
  (superchat-test-agent--with
    (superchat-test-script (superchat-test-agent--rounds 3)
      (superchat-run "t" "go")
      (let ((m (plist-get (car superchat-test-requests) :messages)))
        (should (= (length m) 7))
        (should-not (seq-find (lambda (x) (string-match-p "left out" (plist-get x :text))) m))
        (should (equal (mapcar (lambda (x) (plist-get x :role)) m)
                       '(user assistant tool assistant tool assistant tool)))))
    (should (= (length (superchat-tape-entries :tape "t")) 8))))

;;; The fourth review

(defun superchat-test-agent--size (messages)
  "Text plus printed arguments of calls of MESSAGES, counted here on purpose."
  (cl-loop for m in messages
           sum (+ (length (plist-get m :text))
                  (cl-loop for c in (plist-get m :calls)
                           sum (length (format "%S" (plist-get c :args)))))))

(defun superchat-test-agent--types (tape)
  "The kind of each entry of TAPE, an event as its type."
  (mapcar (lambda (e) (if (eq (plist-get e :kind) 'event)
                          (intern (plist-get (plist-get e :data) :type))
                        (plist-get e :kind)))
          (superchat-tape-entries :tape tape)))

(ert-deftest superchat-agent-cancel-ends-the-run-when-a-stop-signals ()
  (superchat-test-agent--with
    (superchat-tool-define "bad" :class 'read :async t
                           :function (lambda (_ _d) (lambda () (error "Process foo is not running"))))
    (unwind-protect
        (let (dones)
          (superchat-test-script (list `(:calls (,(superchat-test-agent--call "c1" "bad"))))
            (superchat-run "t" "go" :on-done (lambda (r) (push (plist-get r :status) dones)))
            (superchat-cancel "t"))
          (should (equal dones '(cancelled)))
          (should-not (superchat-run-active "t"))
          (should (seq-find (lambda (e) (string-match-p "foo is not running" (plist-get e :text)))
                            (superchat-tape-entries :tape "t" :kinds '(event))))
          (should (equal (mapcar (lambda (e) (plist-get e :text))
                                 (superchat-tape-entries :tape "t" :kinds '(tool)))
                         '("Cancelled."))))
      (superchat-tool-remove "bad"))
    (let (dones (superchat-llm-request-function (lambda (&rest _) (list :abort 'junk))))
      (superchat-run "u" "go" :on-done (lambda (r) (push (plist-get r :status) dones)))
      (superchat-cancel "u")
      (should (equal dones '(cancelled)))
      (should-not (superchat-run-active "u"))
      (should (memq 'error (superchat-test-agent--types "u"))))))

(defvar superchat-test-agent--ran 0 "How many times the tool `mark' ran.")

(defmacro superchat-test-agent--with-mark (&rest body)
  "BODY with a write tool `mark' that counts its runs and an async one, `amark'."
  `(let ((superchat-test-agent--ran 0))
     (superchat-tool-define "mark" :class 'write
                            :function (lambda (_) (cl-incf superchat-test-agent--ran) "marked"))
     (superchat-tool-define "amark" :class 'write :async t
                            :function (lambda (_ done) (cl-incf superchat-test-agent--ran)
                                        (funcall done "amarked")))
     (unwind-protect (progn ,@body)
       (superchat-tool-remove "mark") (superchat-tool-remove "amark"))))

(ert-deftest superchat-agent-cancel-during-the-question-keeps-the-timeline ()
  (dolist (name '("mark" "amark"))
    (superchat-test-agent--with
      (superchat-test-agent--with-mark
        (let (dones refused)
          (superchat-test-script
              (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "a")
                              ,(superchat-test-agent--call "c2" name)
                              ,(superchat-test-agent--call "c3" "echo" :x "b"))))
            (let ((superchat-permission-ask-function
                   (lambda (&rest _)
                     (superchat-cancel "t")
                     ;; Still busy: nothing of the cancelled run is written yet.
                     (setq refused (and (superchat-run-active "t")
                                        (condition-case nil (superchat-run "t" "next") (error t))))
                     'allow)))
              (superchat-run "t" "go" :on-done (lambda (r) (push (plist-get r :status) dones)))))
          (should refused)
          (should (= superchat-test-agent--ran 0))
          (should (equal dones '(cancelled)))
          (should-not (superchat-run-active "t"))
          (should (equal (mapcar (lambda (e) (plist-get e :text))
                                 (superchat-tape-entries :tape "t" :kinds '(tool)))
                         '("echo:a" "Cancelled." "Cancelled.")))
          (should (eq (car (last (superchat-test-agent--types "t"))) 'cancelled)))))))

(ert-deftest superchat-agent-new-run-after-a-cancel-during-the-question-is-clean ()
  (superchat-test-agent--with
    (superchat-test-agent--with-mark
      (let ((superchat-permission-ask-function (lambda (&rest _) (superchat-cancel "t") 'allow)))
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c2" "mark"))) '(:text "ans2"))
          (superchat-run "t" "go")
          (superchat-run "t" "next")
          (let ((m (plist-get (car superchat-test-requests) :messages)))
            (superchat-test-agent--well-formed m)
            (should (equal (mapcar (lambda (x) (plist-get x :role)) m)
                           '(user assistant tool user))))
          (should (= superchat-test-agent--ran 0)))))))

(ert-deftest superchat-agent-hundreds-of-sync-calls-do-not-deepen-the-stack ()
  (superchat-test-agent--with
    (let ((calls (cl-loop for i below 400
                          collect (superchat-test-agent--call (format "c%d" i) "echo" :x "a"))))
      (superchat-test-script (list `(:calls ,calls) '(:text "fin"))
        (let ((run (superchat-run "t" "go")))
          (should (eq (plist-get run :status) 'done))
          (should (equal (plist-get run :text) "fin"))
          (should (= 400 (length (superchat-tape-entries :tape "t" :kinds '(tool))))))))))

(ert-deftest superchat-agent-signal-in-on-entry-after-hundreds-of-calls-ends-the-run ()
  (superchat-test-agent--with
    (let ((calls (cl-loop for i below 300
                          collect (superchat-test-agent--call (format "c%d" i) "echo" :x "a")))
          dones)
      (superchat-test-script (list `(:calls ,calls) '(:text "fin"))
        (superchat-run "t" "go" :on-done (lambda (r) (push (plist-get r :status) dones))))
      (should (equal dones '(done))))))

(ert-deftest superchat-agent-run-start-handoff-that-cannot-start-keeps-the-user-text ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
    (superchat-tape-append "t" 'assistant "ok")
    (let ((superchat-context-chars 100) res
          (superchat-llm-request-function (lambda (&rest _) (user-error "No model set"))))
      (superchat-run "t" "MY TEXT" :on-done (lambda (r) (setq res r)))
      (should (eq (plist-get res :status) 'error))
      (should (string-match-p "No model set" (plist-get res :error)))
      (should-not (superchat-run-active "t"))
      (should (equal (superchat-test-agent--types "t") '(user assistant error user)))
      (should (equal (plist-get (car (last (superchat-tape-entries :tape "t"))) :text) "MY TEXT"))
      (should (equal (plist-get (nth 2 (superchat-tape-entries :tape "t")) :text)
                     (plist-get res :error))))))

(ert-deftest superchat-agent-run-start-handoff-uses-the-runs-model ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
    (let ((superchat-context-chars 100))
      (superchat-test-script (list '(:text "S") '(:text "answer"))
        (superchat-run "t" "go" :model "m1")
        (should (equal (mapcar (lambda (r) (plist-get r :model)) superchat-test-requests)
                       '("m1" "m1")))))))

(ert-deftest superchat-agent-failed-handoff-error-is-the-events-text ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
    (superchat-tape-append "t" 'assistant "ok")
    (let ((superchat-context-chars 100) res)
      (superchat-test-script (list '(:error "boom"))
        (superchat-run "t" "hello" :on-done (lambda (r) (setq res r))))
      (should (equal (plist-get res :error) "Handoff failed: boom"))
      (should (equal (plist-get (nth 2 (superchat-tape-entries :tape "t")) :text)
                     "Handoff failed: boom")))))

(ert-deftest superchat-agent-on-entry-cancelling-the-user-entry-ends-once ()
  (superchat-test-agent--with
    (let (dones)
      (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
      (let ((superchat-context-chars 100))
        (superchat-test-script (list '(:error "boom"))
          (superchat-run "t" "hello"
                         :on-entry (lambda (e) (when (equal (plist-get e :text) "hello")
                                                 (superchat-cancel "t")))
                         :on-done (lambda (r) (push (plist-get r :status) dones)))))
      ;; The user entry comes while the run is ending with its error: too late to cancel.
      (should (equal dones '(error)))
      (should-not (superchat-run-active "t")))
    (setq dones nil)
    (superchat-test-script (list '(:text "answer"))
      (superchat-run "u" "hello"
                     :on-entry (lambda (e) (when (eq (plist-get e :kind) 'user)
                                             (superchat-cancel "u")))
                     :on-done (lambda (r) (push (plist-get r :status) dones)))
      (should (equal dones '(cancelled)))
      (should-not superchat-test-requests))))

(ert-deftest superchat-agent-size-counts-the-arguments-of-calls ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "write files" :scope "home")
    (dotimes (i 5)
      (superchat-tape-append
       "t" 'assistant "" :data (list :model "m"
                                     :calls (list (list :id (format "w%d" i) :name "write"
                                                        :args (list :content (make-string 100000 ?q))))))
      (superchat-tape-append "t" 'tool "Wrote."
                             :data (list :call-id (format "w%d" i) :name "write")))
    (superchat-tape-append "t" 'assistant "done" :data '(:model "m"))
    (superchat-test-script (list '(:text "SUM") '(:text "ok"))
      (superchat-run "t" "next")
      (should (seq-find (lambda (e) (eq (plist-get e :kind) 'anchor))
                        (superchat-tape-entries :tape "t"))))))

(ert-deftest superchat-agent-budget-counts-the-arguments-of-calls ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 1500))
      (superchat-test-script
          (append (cl-loop for i below 4
                           collect (list :calls (list (superchat-test-agent--call
                                                       (format "c%d" i) "echo" :x (make-string 400 ?x)))))
                  (list '(:text "done")))
        (superchat-run "t" "go")
        (let ((m (plist-get (car superchat-test-requests) :messages)))
          (should (<= (superchat-test-agent--size m) 1500))
          (should (seq-find (lambda (x) (string-match-p "left out" (plist-get x :text))) m))
          (superchat-test-agent--well-formed m))))))

(ert-deftest superchat-agent-leaving-out-never-makes-a-request-larger ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 500))
      (superchat-tape-append "t" 'user (make-string 20 ?a) :scope "home")
      (superchat-tape-append "t" 'assistant (make-string 20 ?b) :data '(:model "m"))
      (superchat-test-script (list '(:text "ok"))
        (superchat-run "t" (make-string 460 ?u))
        ;; 20 + 20 + 460 = 500: fits, nothing is left out.
        (should (= (length (plist-get (car superchat-test-requests) :messages)) 3)))
      (superchat-tape-append "u" 'user (make-string 20 ?a) :scope "home")
      (superchat-test-script (list '(:text "ok"))
        (superchat-run "u" (make-string 485 ?u))
        ;; 505 would not fit, but the stand-in is longer than what it would save.
        (let ((m (plist-get (car superchat-test-requests) :messages)))
          (should (<= (superchat-test-agent--size m) 505))
          (should-not (seq-find (lambda (x) (string-match-p "left out" (plist-get x :text))) m)))))))

(ert-deftest superchat-agent-permission-event-reaches-on-entry-while-a-tool-runs ()
  (superchat-test-agent--with
    (let (done seen (superchat-permission-ask-function (lambda (&rest _) 'allow)))
      (superchat-tool-define "wait" :class 'write :async t
                             :function (lambda (_ d) (setq done d) nil))
      (unwind-protect
          (superchat-test-script
              (list `(:calls (,(superchat-test-agent--call "c1" "wait"))) '(:text "x"))
            (superchat-run "t" "go" :on-entry (lambda (e) (push (plist-get e :kind) seen)))
            (should (equal (reverse seen) '(user assistant event)))
            (funcall done "ok")
            (should (equal (reverse seen) '(user assistant event tool assistant))))
        (superchat-tool-remove "wait")))))

(ert-deftest superchat-agent-text-is-set-before-on-entry-sees-the-answer ()
  (superchat-test-agent--with
    (let (res)
      (superchat-test-script (list '(:text "THE ANSWER"))
        (superchat-run "t" "q" :on-done (lambda (r) (setq res r))
                       :on-entry (lambda (e) (when (eq (plist-get e :kind) 'assistant)
                                               (error "display broke"))))
        (should (equal (plist-get res :text) "THE ANSWER"))
        (should (eq (plist-get res :status) 'error))))
    (let (res)
      (superchat-test-script (list '(:text "THE ANSWER"))
        (superchat-run "u" "q" :on-done (lambda (r) (setq res r))
                       :on-entry (lambda (e) (when (eq (plist-get e :kind) 'assistant)
                                               (superchat-cancel "u")))))
      (should (eq (plist-get res :status) 'cancelled))
      (should (equal (plist-get res :text) "THE ANSWER")))))

(defun superchat-test-agent--no-orphans (messages)
  "Non-nil when every tool message of MESSAGES answers a call of the assistant
message it follows, the other tool messages between them aside."
  (let ((head nil) (ok t))
    (dolist (m messages)
      (if (eq (plist-get m :role) 'tool)
          (unless (cl-some (lambda (c) (equal (plist-get c :id) (plist-get m :call-id)))
                           (plist-get head :calls))
            (setq ok nil))
        (setq head m)))
    ok))

(defun superchat-test-agent--history (i)
  "Random entries for case I, each the arguments of `superchat-tape-append'
after the timeline name: results that answer nothing or an older response,
anchors, notes, permission events, and calls with their results."
  (let (out (n 0))
    (cl-flet ((text () (make-string (pcase (random 6) (0 0) (1 (random 600)) (_ (random 120))) ?a))
              (add (&rest args) (push args out)))
      (dotimes (_ (random 7))
        (pcase (random 11)
          ((or 0 1) (add 'user (text)))
          (2 (add 'anchor (text) :data '(:name "h" :summary "s" :next "")))
          (3 (add 'note (text) :data '(:title "t")))
          (8 (add 'tool (text) :data (list :call-id "stray" :name "echo")))
          (9 (add 'tool (text) :data (list :call-id (format "c%d-1" i) :name "echo")))
          (_ (let ((calls (cl-loop for k below (random 4)
                                   collect (list :id (format "c%d-%d" i (cl-incf n))
                                                 :name "echo" :args (list :x (text))))))
               (add 'assistant (text) :data (list :model "m" :calls calls))
               (dolist (c calls)
                 (when (zerop (random 3)) (add 'event "p" :data '(:type "permission")))
                 (add 'tool (text) :data (list :call-id (plist-get c :id) :name "echo")))
               (when (zerop (random 4))
                 (add 'tool (text) :data (list :call-id "stray" :name "echo")))))))
      (add 'user (text)))
    (nreverse out)))

(defun superchat-test-agent--request-at (tape history budget)
  "The messages of the first request of a run on the new timeline TAPE at
BUDGET, its ON-ENTRY writing HISTORY after the run's user entry \"go\"; nil
when the run made no request."
  (let ((superchat-context-chars budget) (written nil))
    (superchat-test-script (list '(:text "x"))
      (superchat-run tape "go" :scope "home" :tools 'none
                     :on-entry (lambda (_)
                                 (unless written
                                   (setq written t)
                                   (dolist (h history) (apply #'superchat-tape-append tape h)))))
      (plist-get (car superchat-test-requests) :messages))))

(defun superchat-test-agent--budget-problems (out whole budget)
  "What is wrong with OUT, the request made at BUDGET, given WHOLE, the
request made from the same entries with room for all of them."
  (let* ((standin-p (lambda (m) (string-match-p "\\`\\[[0-9]+ earlier entries" (plist-get m :text))))
         (stands (cl-remove-if-not standin-p out))
         (rest (cl-remove-if standin-p out))
         (latest (cl-position 'assistant whole :key (lambda (m) (plist-get m :role)) :from-end t))
         ;; Never left out: anchors, the run's user entry, the latest response and its results.
         (protected
          (append (cl-remove-if-not
                   (lambda (m) (or (equal (plist-get m :text) "go")
                                   (string-prefix-p "Summary of the conversation so far:"
                                                    (plist-get m :text))))
                   whole)
                  (and latest (cons (nth latest whole)
                                    (cl-loop for m in (nthcdr (1+ latest) whole)
                                             while (eq (plist-get m :role) 'tool) collect m)))))
         problems)
    (when (cl-set-difference protected rest :test #'equal)
      (push "protected item missing" problems))
    (dolist (m (list out whole))
      (unless (superchat-test-agent--no-orphans m)
        (push "a tool message that answers no call before it" problems)))
    (let ((tail whole))
      (dolist (m rest)
        (setq tail (cdr (member m tail)))
        (unless (or tail (equal m (car (last whole)))) (push "not in the order of the whole" problems))))
    (when (and (not (equal rest whole)) (null stands))
      (push "left out with no stand-in" problems))
    (when (cdr stands) (push "two stand-ins" problems))
    (let ((where (mapcar (lambda (m) (plist-get m :call-id))
                         (cl-remove-if-not (lambda (m) (eq (plist-get m :role) 'tool)) out))))
      (dolist (m out)
        (dolist (c (plist-get m :calls))
          (unless (member (plist-get c :id) where) (push "call without result" problems)))))
    ;; Leaving anything out costs a stand-in; below that cost nothing can be done.
    (when (and (> (superchat-test-agent--size out) budget)
               (<= (+ (superchat-test-agent--size protected)
                      (length (format "[%d earlier entries of this conversation are left out of this request; recall and tape_query can read them.]" 10)))
                   budget))
      (push (format "size %d over budget %d" (superchat-test-agent--size out) budget) problems))
    problems))

(ert-deftest superchat-agent-budget-property ()
  (superchat-test-agent--with
    (let ((bad nil) (made 0))
      (random "budget-property")
      (dotimes (i 200)
        (let* ((history (superchat-test-agent--history i))
               (whole (superchat-test-agent--request-at (format "w%d" i) history 1000000)))
          (dolist (budget '(50 300 900))
            (when-let* ((out (superchat-test-agent--request-at (format "p%d-%d" i budget)
                                                               history budget)))
              (cl-incf made)
              (when-let* ((problems (superchat-test-agent--budget-problems out whole budget)))
                (push (list i budget problems) bad))))))
      (should (> made 300))
      (should-not bad))))

;;; The fifth review: one way a run ends

(defvar superchat-test-agent--timer nil "The timer of the tool `slowx'.")

(defmacro superchat-test-agent--with-slowx (&rest body)
  "BODY with an asynchronous tool `slowx' that answers from a 0.05 s timer."
  `(progn
     (setq superchat-test-agent--timer nil)
     (superchat-tool-define
      "slowx" :class 'read :async t :description "Slow"
      :function (lambda (_ done)
                  (let ((timer (run-at-time 0.05 nil (lambda () (funcall done "slow-done")))))
                    (setq superchat-test-agent--timer timer)
                    (lambda () (cancel-timer timer)))))
     (superchat-tool-define
      "slowask" :class 'write :async t :description "Slow, asks"
      :function (lambda (_ done)
                  (let ((timer (run-at-time 0.05 nil (lambda () (funcall done "slow-done")))))
                    (setq superchat-test-agent--timer timer)
                    (lambda () (cancel-timer timer)))))
     (unwind-protect (progn ,@body)
       (superchat-tool-remove "slowx") (superchat-tool-remove "slowask"))))

(defun superchat-test-agent--idle ()
  "Wait until the model, the tool timers and every run are quiet."
  (superchat-test-drain)
  ;; A tool timer still alive could still answer; wait for it, not for a fixed time.
  (with-timeout (2 (error "The tool timer did not finish"))
    (while (memq superchat-test-agent--timer timer-list)
      (accept-process-output nil 0.01)))
  (accept-process-output nil 0.01)
  (superchat-test-drain))

(defun superchat-test-agent--next-request-ok (tape)
  "Start another run on TAPE; its request must be well formed."
  (superchat-test-script (list '(:text "ans3"))
    (let ((superchat-test-async nil))
      (superchat-run tape "next")
      (superchat-test-agent--well-formed (plist-get (car superchat-test-requests) :messages)))))

(ert-deftest superchat-agent-on-entry-signal-on-a-permission-event-stops-the-tool ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slowx
      (let ((superchat-test-async t) dones
            (superchat-permission-ask-function (lambda (&rest _) 'allow)))
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "slowask"))) '(:text "x"))
          (superchat-run "t" "go"
                         :on-entry (lambda (e) (when (equal (plist-get (plist-get e :data) :type)
                                                            "permission")
                                                 (error "display broke")))
                         :on-done (lambda (r) (push (plist-get r :status) dones)))
          (superchat-test-agent--idle))
        (should (equal dones '(error)))
        (should-not (memq superchat-test-agent--timer timer-list))
        (should-not (eq (plist-get (car (last (superchat-tape-entries :tape "t"))) :kind) 'tool))
        (should (equal (mapcar #'cdr (cl-remove-if-not (lambda (x) (eq (car x) 'tool))
                                                       (superchat-test-agent--entries "t")))
                       '("Cancelled.")))))))

(ert-deftest superchat-agent-a-run-adds-results-only-for-its-own-calls ()
  (superchat-test-agent--with
    (let ((superchat-test-async t) dones)
      (superchat-test-script
          (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                  '("c1" "c2" "c3")))
                '(:text "never") '(:text "ans3"))
        (superchat-run "t" "go"
                       :on-entry (lambda (e) (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                               (error "display broke")))
                       :on-done (lambda (r) (push (plist-get r :status) dones)))
        (superchat-test-drain)
        (superchat-run "t" "second")
        (superchat-cancel "t")
        (superchat-test-drain)
        (should (equal dones '(error)))
        (let ((types (superchat-test-agent--types "t")))
          ;; Run 1 owes c2 and c3 before its error; run 2 owes nothing.
          (should (equal types '(user assistant tool tool tool error user cancelled))))
        (superchat-run "t" "third")
        (superchat-test-drain)
        (superchat-test-agent--well-formed (plist-get (car superchat-test-requests) :messages))))))

(ert-deftest superchat-agent-failed-handoff-with-a-signalling-on-entry-keeps-the-user-text ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
    (let ((superchat-context-chars 100) dones)
      (superchat-test-script (list '(:error "boom"))
        (condition-case nil
            (superchat-run "t" "MY TEXT"
                           :on-entry (lambda (e) (when (eq (plist-get e :kind) 'event)
                                                   (error "display broke")))
                           :on-done (lambda (r) (push (plist-get r :status) dones)))
          (error nil)))
      (should (equal dones '(error)))
      (should-not (superchat-run-active "t"))
      (should (equal (plist-get (car (last (superchat-tape-entries :tape "t" :kinds '(user))))
                                :text)
                     "MY TEXT")))))

(ert-deftest superchat-agent-ended-run-gives-its-on-entry-nothing-more ()
  (superchat-test-agent--with
    (let ((superchat-permission-ask-function
           (lambda (&rest _) (superchat-cancel "t") 'allow))
          seen dones)
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "c1" "poke"))) '(:text "ans2"))
        (superchat-run "t" "go"
                       :on-entry (lambda (e) (push (plist-get e :id) seen))
                       :on-done (lambda (r) (push (plist-get r :status) dones)
                                  (superchat-run "t" "second")))
        (should (equal dones '(cancelled)))
        (should (= (apply #'max seen)
                   (plist-get (car (superchat-tape-entries :tape "t" :kinds '(event) :limit 1
                                                           :newest t))
                              :id)))))))

(ert-deftest superchat-agent-signal-on-an-entry-mid-calls-leaves-no-call-unanswered ()
  (superchat-test-agent--with
    (let (dones)
      (superchat-test-script
          (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                  '("c1" "c2" "c3"))))
        (superchat-run "t" "go"
                       :on-entry (lambda (e) (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                               (error "display broke")))
                       :on-done (lambda (r) (push (plist-get r :status) dones))))
      (should (equal dones '(error)))
      (should (equal (superchat-test-agent--types "t")
                     '(user assistant tool tool tool error))))))

(ert-deftest superchat-agent-signal-on-the-first-cancelled-result-ends-the-cancel ()
  (superchat-test-agent--with
    (let (dones)
      (superchat-test-script
          (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                  '("c1" "c2" "c3"))))
        (condition-case nil
            (superchat-run "t" "go"
                           :on-entry (lambda (e)
                                       (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                         (superchat-cancel "t"))
                                       (when (equal (plist-get e :text) "Cancelled.")
                                         (error "display broke")))
                           :on-done (lambda (r) (push (plist-get r :status) dones)))
          (error nil)))
      (should (equal dones '(cancelled)))
      (should (equal (superchat-test-agent--types "t")
                     '(user assistant tool tool tool cancelled))))))

(ert-deftest superchat-agent-a-cancel-that-cannot-stop-the-request-drops-its-answer ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 500 ?x) :scope "home")
    (superchat-tape-append "t" 'assistant "ok")
    (let ((superchat-context-chars 100) (superchat-test-async t) dones)
      (superchat-test-script (list '(:text "SUM\nNEXT: n") '(:text "ans"))
        (cl-letf (((symbol-function 'superchat-llm-cancel) (lambda (_) (error "cancel broke"))))
          (superchat-run "t" "MY TEXT" :on-done (lambda (r) (push (plist-get r :status) dones)))
          (superchat-cancel "t"))
        (superchat-test-agent--idle))
      (should (equal dones '(cancelled)))
      (should (equal (superchat-test-agent--types "t") '(user assistant error user cancelled)))
      (should-not (superchat-tape-entries :tape "t" :kinds '(anchor))))))

(ert-deftest superchat-agent-a-cancelled-handoff-drops-its-late-answer ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "hello" :scope "home")
    (let ((superchat-test-async t) got)
      (superchat-test-script (list '(:text "SUM\nNEXT: n"))
        (cl-letf (((symbol-function 'superchat-llm-cancel) (lambda (_) (error "cancel broke"))))
          (superchat-handoff "t" nil (lambda (a) (push a got)))
          (superchat-cancel "t"))
        (superchat-test-agent--idle))
      (should-not got)
      (should-not (superchat-tape-entries :tape "t" :kinds '(anchor))))))

(ert-deftest superchat-agent-a-throw-out-of-a-tool-call-ends-the-run ()
  (superchat-test-agent--with
    (let ((superchat-permission-ask-function (lambda (&rest _) (throw 'out 'thrown)))
          dones)
      (superchat-test-script
          (list `(:calls (,(superchat-test-agent--call "c1" "poke")
                          ,(superchat-test-agent--call "c2" "echo" :x "a"))))
        (should (eq (catch 'out
                      (superchat-run "t" "go"
                                     :on-done (lambda (r) (push (list (plist-get r :status)
                                                                      (plist-get r :error))
                                                                dones))))
                    'thrown)))
      (should (equal (mapcar #'car dones) '(error)))
      (should (string-match-p "interrupted" (cadr (car dones))))
      (should-not (superchat-run-active "t"))
      (should (equal (superchat-test-agent--types "t")
                     '(user assistant tool tool error))))))

(defconst superchat-test-agent--matrix-kinds
  `((user . ,(lambda (e) (eq (plist-get e :kind) 'user)))
    (assistant-with-calls . ,(lambda (e) (and (eq (plist-get e :kind) 'assistant)
                                              (plist-get (plist-get e :data) :calls))))
    (permission . ,(lambda (e) (equal (plist-get (plist-get e :data) :type) "permission")))
    (first-result . ,(lambda (e) (equal (plist-get (plist-get e :data) :call-id) "c1")))
    (async-result . ,(lambda (e) (equal (plist-get (plist-get e :data) :call-id) "c2")))
    (last-result . ,(lambda (e) (equal (plist-get (plist-get e :data) :call-id) "c3")))
    (error-event . ,(lambda (e) (equal (plist-get (plist-get e :data) :type) "error")))
    (cancelled-event . ,(lambda (e) (equal (plist-get (plist-get e :data) :type) "cancelled")))
    (final-assistant . ,(lambda (e) (and (eq (plist-get e :kind) 'assistant)
                                         (not (plist-get (plist-get e :data) :calls))))))
  "Entry kind and what recognises it; the callback acts on the first match.")

(defun superchat-test-agent--matrix-case (kind pred action)
  "Run one run whose ON-ENTRY does ACTION (`signal' or `cancel') on the first
entry PRED recognises; check what every way of ending owes."
  (let ((superchat-test-async t) dones seen hit
        (superchat-permission-ask-function (lambda (&rest _) 'allow)))
    (superchat-test-script
        (list `(:calls (,(superchat-test-agent--call "c1" "poke")
                        ,(superchat-test-agent--call "c2" "slowx")
                        ,(superchat-test-agent--call "c3" "echo" :x "b")))
              (if (eq kind 'error-event) '(:error "model broke") '(:text "ans2")))
      (condition-case nil
          (progn
            (superchat-run
             "t" "go"
             :on-entry (lambda (e)
                         (push (plist-get e :id) seen)
                         (when (and (not hit) (funcall pred e))
                           (setq hit t)
                           (if (eq action 'signal) (error "display broke") (superchat-cancel "t"))))
             :on-done (lambda (r)
                        (push (list (plist-get r :status)
                                    (plist-get (car (superchat-tape-entries :tape "t" :newest t :limit 1))
                                               :id)
                                    (and (memq superchat-test-agent--timer timer-list) t))
                              dones)))
            (when (eq kind 'cancelled-event)
              (superchat-test-wait (lambda () superchat-test-agent--timer))
              (superchat-cancel "t")))
        (error nil))
      (superchat-test-agent--idle)
      (should hit)
      ;; ON-DONE once; the timeline is free; no tool is still running.
      (should (= (length dones) 1))
      (should-not (superchat-run-active "t"))
      (should-not (nth 2 (car dones)))
      (let* ((end (nth 1 (car dones)))
             (mine (cl-remove-if (lambda (e) (> (plist-get e :id) end))
                                 (superchat-tape-entries :tape "t")))
             (ids (cl-loop for e in mine
                           when (eq (plist-get e :kind) 'assistant)
                           append (mapcar (lambda (c) (plist-get c :id))
                                          (plist-get (plist-get e :data) :calls))))
             (last-event (apply #'max (cons 0 (mapcar (lambda (e) (plist-get e :id))
                                                      (cl-remove-if-not
                                                       (lambda (e) (member (plist-get (plist-get e :data) :type)
                                                                           '("error" "cancelled")))
                                                       mine))))))
        (should (eq (plist-get (car (last mine)) :kind) 'event))
        (dolist (id ids)
          (let ((results (cl-remove-if-not (lambda (e) (equal (plist-get (plist-get e :data) :call-id) id))
                                           mine)))
            (should (= (length results) 1))
            (should (< (plist-get (car results) :id) last-event))))
        ;; A following run: well formed, and nothing of run 1 after its end.
        (superchat-test-agent--next-request-ok "t")
        (superchat-test-agent--idle)
        (should (equal (plist-get (nth (length mine) (superchat-tape-entries :tape "t")) :text)
                       "next"))
        (should (<= (apply #'max seen) end))))))

(ert-deftest superchat-agent-every-way-of-ending-owes-the-same ()
  (dolist (kind superchat-test-agent--matrix-kinds)
    (dolist (action '(signal cancel))
      (ert-info ((format "%s x %s" (car kind) action))
        (superchat-test-agent--with
          (superchat-test-agent--with-slowx
            (superchat-test-agent--matrix-case (car kind) (cdr kind) action)))))))

;;; The budget follows the model.

(defmacro superchat-test-agent--with-windows (&rest body)
  "BODY with provider \"cw\" (models m8: 8000, m2: 2000) set as the default
model m8 and `superchat-context-chars' nil."
  (declare (indent 0))
  ;; A changed `superchat-llm-providers' is seen at once: no cache to reset.
  `(let ((superchat-llm-providers
          (list (cons "cw" (lambda ()
                             (superchat-bridge-openai-compatible-provider
                              :id "cw" :base-url "http://localhost:1/v1"
                              :models '((:id "m8" :context-window 8000)
                                        (:id "m2" :context-window 2000)))))))
         (superchat-llm-provider "cw") (superchat-llm-model "m8")
         (superchat-context-chars nil))
     ,@body))

(defun superchat-test-agent--budget-said (model)
  "The budget the refusal of a run of MODEL on a huge first message names."
  (let ((run (superchat-run (superchat-tape-new-name) (make-string 300000 ?u)
                            :scope "home" :model model)))
    (and (string-match "is \\([0-9]+\\)\\." (plist-get run :error))
         (string-to-number (match-string 1 (plist-get run :error))))))

(ert-deftest superchat-agent-budget-function ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (should (= (superchat-test-agent--budget-said nil) 4000))
      (should (= (superchat-test-agent--budget-said "m2") 1000))
      (should (= (superchat-test-agent--budget-said "unknown") 120000))
      (let ((superchat-context-chars 500))
        (should (= (superchat-test-agent--budget-said nil) 500))
        (should (= (superchat-test-agent--budget-said "m2") 500))))
    ;; The scripted model has no window: no provider is set.
    (let ((superchat-context-chars nil) (superchat-llm-provider nil))
      (should (= (superchat-test-agent--budget-said nil) 120000)))))

(ert-deftest superchat-agent-budget-of-the-model-of-the-run ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (dolist (tape '("a" "b"))
        (superchat-tape-append tape 'user (make-string 1500 ?a) :scope "home")
        (superchat-tape-append tape 'assistant "ok" :data '(:model "x")))
      ;; 1500 characters: under the 4000 of m8, over the 1000 of m2.
      (superchat-test-script (list '(:text "fine"))
        (superchat-run "a" "next")
        (should (= (length superchat-test-requests) 1)))
      (superchat-test-script (list '(:text "S\nNEXT: n") '(:text "fine"))
        (superchat-run "b" "next" :model "m2")
        (should (= (length superchat-test-requests) 2))
        (should (equal (mapcar (lambda (r) (plist-get r :model)) superchat-test-requests)
                       '("m2" "m2")))
        (should (superchat-tape-entries :tape "b" :kinds '(anchor)))))))

(ert-deftest superchat-agent-derived-budget-leaves-out-and-stays-well-formed ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let (left-out)
        (superchat-test-script (superchat-test-agent--rounds 24)
          (superchat-run "t" "go")
          (should (= (length superchat-test-requests) 25))
          (dolist (request (reverse superchat-test-requests))
            (let ((m (plist-get request :messages)))
              (should (<= (superchat-test-agent--size m) 4000))
              (superchat-test-agent--well-formed m)
              (when (seq-find (lambda (x) (string-match-p "earlier entries" (plist-get x :text))) m)
                (setq left-out t))))
          (should-not (superchat-run-active "t")))
        (should left-out)
        ;; Nothing was left out while the whole fitted.
        (should-not (seq-find (lambda (x) (string-match-p "earlier entries" (plist-get x :text)))
                              (plist-get (car (last superchat-test-requests)) :messages)))))))

(defun superchat-test-agent--refusal (budget size)
  "Run a first message of SIZE characters at BUDGET; return (RUN . REQUESTS)."
  (let ((superchat-context-chars budget) run)
    (superchat-test-script (list '(:text "answer"))
      (setq run (superchat-run "t" (make-string size ?u)))
      (cons run (length superchat-test-requests)))))

(ert-deftest superchat-agent-refuses-over-twice-the-budget ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let* ((res (superchat-test-agent--refusal 100 201))
             (run (car res)))
        (should (= (cdr res) 0))
        (should (eq (plist-get run :status) 'error))
        (should (equal (plist-get run :error)
                       "This request needs 201 characters; the budget for m8 is 100. Raise `superchat-context-chars' or start a new conversation."))
        (should-not (superchat-run-active "t"))
        (should (equal (mapcar #'car (superchat-test-agent--entries "t")) '(user event)))
        (should (equal (cdr (cadr (superchat-test-agent--entries "t")))
                       (plist-get run :error))))))
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let ((res (superchat-test-agent--refusal 100 200)))
        (should (= (cdr res) 1))
        (should (eq (plist-get (car res) :status) 'done))))))

(ert-deftest superchat-agent-refuses-a-later-round-too ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let ((superchat-context-chars 100) run)
        ;; Round 1's result (300 characters) is the latest, so it is kept.
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x (make-string 300 ?x)))))
          (setq run (superchat-run "t" "go"))
          (should (= (length superchat-test-requests) 1)))
        (should (eq (plist-get run :status) 'error))
        (should (string-match-p "budget for m8 is 100" (plist-get run :error)))
        (should-not (superchat-run-active "t"))
        (should (equal (mapcar #'car (superchat-test-agent--entries "t"))
                       '(user assistant tool event)))))))

(ert-deftest superchat-agent-the-run-after-a-refusal-recovers ()
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let ((superchat-context-chars 100) (huge (make-string 500 ?H)) runs)
        (superchat-test-script (list '(:text "S\nNEXT: n") '(:text "fine"))
          ;; 1: refused.  2: its handoff cuts the huge entry out of the
          ;; summary (the oldest are dropped first), then the run is answered.
          (push (superchat-run "t" huge) runs)
          (should (= (length superchat-test-requests) 0))
          (push (superchat-run "t" "next") runs)
          (let ((handoff (plist-get (car (last superchat-test-requests)) :messages)))
            (should (= (length superchat-test-requests) 2))
            (should-not (string-match-p "HHH" (plist-get (car handoff) :text)))
            (should (<= (length (plist-get (car handoff) :text)) 600)))
          (should (eq (plist-get (car runs) :status) 'done))
          (let ((m (plist-get (car superchat-test-requests) :messages)))
            (should (equal (mapcar (lambda (x) (plist-get x :role)) m) '(user user)))
            (should (string-prefix-p "Summary of the conversation so far:" (plist-get (car m) :text)))
            (should (equal (plist-get (cadr m) :text) "next"))))
        (should (eq (plist-get (cadr runs) :status) 'error))
        ;; A huge message again: refused again, and the one after recovers again.
        (superchat-test-script (list '(:text "S2\nNEXT: n") '(:text "fine again"))
          (should (eq (plist-get (superchat-run "t" huge) :status) 'error))
          (should (= (length superchat-test-requests) 0))
          (should (eq (plist-get (superchat-run "t" "small") :status) 'done))
          (should (= (length superchat-test-requests) 2)))))))

(defun superchat-test-agent--first-text (requests)
  "The text of the first message of the oldest of REQUESTS."
  (plist-get (car (plist-get (car (last requests)) :messages)) :text))

(ert-deftest superchat-agent-handoff-text-begins-with-the-current-anchor ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "old" :scope "home")
    (superchat-tape-append "t" 'anchor "SECRET-EARLIER-SUMMARY"
                           :data '(:name "h" :summary "SECRET-EARLIER-SUMMARY" :next ""))
    (dotimes (i 8) (superchat-tape-append "t" 'user (format "e%d %s" i (make-string 200 ?.))))
    (let ((superchat-context-chars 1000))
      (superchat-test-script (list '(:text "S\nNEXT: n") '(:text "fine"))
        (superchat-run "t" "go")
        (let ((text (superchat-test-agent--first-text superchat-test-requests)))
          (should (string-prefix-p "Summary of the conversation so far:\nSECRET-EARLIER-SUMMARY" text))
          (should (string-match-p "earlier entries left out" text))
          (should (string-match-p "e7 " text))
          (should-not (string-match-p "e0 " text)))))))

(ert-deftest superchat-agent-handoff-text-is-cut-to-the-budget-when-one-entry-is-larger ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "small" :scope "home")
    (superchat-tape-append "t" 'user (make-string 50000 ?B))
    (let ((superchat-context-chars 1000))
      (superchat-test-script (list '(:text "S\nNEXT: n"))
        (superchat-handoff "t")
        (let ((text (superchat-test-agent--first-text superchat-test-requests)))
          (should (< (length text) 1500))
          (should (string-match-p "characters cut" text)))))))

(ert-deftest superchat-agent-the-summary-a-handoff-appends-is-a-quarter-of-the-budget ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 1000 ?o) :scope "home")
    (let ((superchat-context-chars 400))
      (superchat-test-script
          (list (list :text (concat (make-string 700 ?S) "\nNEXT: n")) '(:text "a1") '(:text "a2"))
        (superchat-run "t" "q1")
        (let ((anchor (car (superchat-tape-entries :tape "t" :kinds '(anchor)))))
          (should (<= (length (plist-get anchor :text)) 100))
          (should (<= (length (plist-get (plist-get anchor :data) :summary)) 100)))
        ;; The question tells the model what to aim for.
        (should (string-match-p "100 characters"
                                (plist-get (car (plist-get (car (last superchat-test-requests))
                                                           :messages))
                                           :text)))
        ;; No handoff before every run: the anchor is small enough.
        (superchat-run "t" "q2")
        (should (= (length superchat-test-requests) 3))
        (should (= (length (superchat-tape-entries :tape "t" :kinds '(anchor))) 1))))))

(ert-deftest superchat-agent-on-entry-cannot-start-a-run-while-the-ending-emits ()
  (superchat-test-agent--with
    (let (busy order second)
      (superchat-test-script
          (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                  '("c1" "c2")))
                '(:text "second"))
        (superchat-run "t" "go"
                       :on-entry (lambda (e)
                                   (when (equal (plist-get e :text) "Cancelled.")
                                     (push 'entry order)
                                     (condition-case err (superchat-run "t" "early")
                                       (error (push (cadr err) busy))))
                                   (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                     (superchat-cancel "t")))
                       :on-done (lambda (_)
                                  (push 'done order)
                                  (setq second (superchat-run "t" "after")))))
      (should (= (length busy) 1))
      (should (string-match-p "busy" (car busy)))
      (should (eq (car (last order)) 'entry))
      (should (eq (car order) 'done))
      (should (eq (plist-get second :status) 'done))
      (should (equal (mapcar #'cdr (cl-remove-if-not
                                     (lambda (x) (eq (car x) 'user))
                                     (superchat-test-agent--entries "t")))
                     '("go" "after"))))))

(ert-deftest superchat-agent-the-user-text-is-written-when-the-size-check-signals ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (let ((superchat-context-chars nil) res)
      (cl-letf (((symbol-function 'superchat-llm-context-window)
                 (lambda (&rest _) (error "window data broke"))))
        (superchat-run "t" "MY TEXT" :on-done (lambda (r) (setq res r))))
      (should (eq (plist-get res :status) 'error))
      (should-not (superchat-run-active "t"))
      (should (equal (plist-get (car (last (superchat-tape-entries :tape "t"))) :text)
                     "MY TEXT")))))

(ert-deftest superchat-agent-the-refusal-does-not-count-the-stand-in ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user (make-string 450 ?a) :scope "home")
    (superchat-tape-append "t" 'user (make-string 450 ?b))
    (let ((superchat-context-chars 1000) run)
      (superchat-test-script (list '(:text "ok"))
        ;; What is never left out is 1920 characters: under twice the budget.
        (setq run (superchat-run "t" (make-string 1920 ?u)))
        (should (= (length superchat-test-requests) 1))
        (should (eq (plist-get run :status) 'done))))))

(ert-deftest superchat-agent-the-refusal-says-which-model-and-what-to-raise ()
  (superchat-test-agent--with
    (let ((superchat-context-chars nil) (superchat-llm-provider nil) (superchat-llm-model nil)
          run)
      (superchat-test-script nil
        (setq run (superchat-run "t" (make-string 250000 ?u))))
      (should (eq (plist-get run :status) 'error))
      (should (string-match-p "no model set" (plist-get run :error)))
      (should-not (string-match-p "nil" (plist-get run :error)))
      (should-not (string-match-p "superchat-context-chars" (plist-get run :error)))))
  (superchat-test-agent--with
    (superchat-test-agent--with-windows
      (let (run)
        (superchat-test-script nil
          (setq run (superchat-run "t" (make-string 8100 ?u))))
        (should (string-match-p "budget for m8 is 4000" (plist-get run :error)))
        (should-not (string-match-p "superchat-context-chars" (plist-get run :error))))))
  (superchat-test-agent--with
    (let ((superchat-context-chars 100) run)
      (superchat-test-script nil
        (setq run (superchat-run "t" (make-string 300 ?u))))
      (should (string-match-p "Raise `superchat-context-chars'" (plist-get run :error))))))

(ert-deftest superchat-agent-a-result-that-answers-no-call-never-reaches-the-model ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'tool "stray one" :scope "home"
                           :data '(:call-id "x1" :name "echo"))
    (superchat-tape-append "t" 'user "hi")
    (superchat-tape-append "t" 'tool "stray two" :data '(:call-id "x2" :name "echo"))
    (superchat-tape-append "t" 'assistant "" :data (list :model "m" :calls
                                                         (list (superchat-test-agent--call
                                                                "c1" "echo" :x "a"))))
    (superchat-tape-append "t" 'tool "answer" :data '(:call-id "c1" :name "echo"))
    (superchat-tape-append "t" 'tool "wrong" :data '(:call-id "zz" :name "echo"))
    (superchat-tape-append "t" 'assistant "mid" :data '(:model "m"))
    (superchat-tape-append "t" 'tool "late" :data '(:call-id "c1" :name "echo"))
    (superchat-test-script (list '(:text "ok"))
      (superchat-run "t" "go")
      (let ((m (plist-get (car superchat-test-requests) :messages)))
        (should (equal (mapcar (lambda (x) (plist-get x :call-id))
                               (cl-remove-if-not (lambda (x) (eq (plist-get x :role) 'tool)) m))
                       '("c1")))
        (should (superchat-test-agent--no-orphans m))
        (superchat-test-agent--well-formed m)))))

(ert-deftest superchat-agent-on-entry-that-signals-in-the-ending-still-gets-every-entry ()
  (superchat-test-agent--with
    (let (seen order caught)
      (superchat-test-script
          (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                  '("c1" "c2" "c3"))))
        (condition-case err
            (superchat-run "t" "go"
                           :on-entry (lambda (e)
                                       (push (plist-get e :id) seen)
                                       (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                         (superchat-cancel "t"))
                                       (when (equal (plist-get e :text) "Cancelled.")
                                         (error "display broke")))
                           :on-done (lambda (_) (push 'done order)))
          (error (push 'signal order) (setq caught (cadr err)))))
      (should (equal order '(signal done)))
      (should (equal caught "display broke"))
      ;; Both results and the event reached ON-ENTRY, though it signalled on the first.
      (should (equal (reverse seen)
                     (mapcar (lambda (e) (plist-get e :id)) (superchat-tape-entries :tape "t")))))))

(ert-deftest superchat-agent-a-cancel-in-the-ending-changes-nothing ()
  (superchat-test-agent--with
    (let (dones)
      (superchat-test-script (list '(:error "boom"))
        (superchat-run "t" "go"
                       :on-entry (lambda (e) (when (eq (plist-get e :kind) 'event)
                                               (superchat-cancel "t")))
                       :on-done (lambda (r) (push (plist-get r :status) dones))))
      (should (equal dones '(error)))
      (should (equal (superchat-test-agent--types "t") '(user error))))))

(ert-deftest superchat-agent-a-handoff-with-no-summary-fails ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "hello" :scope "home")
    (let (got)
      (superchat-test-script (list '(:text "  \n"))
        (superchat-handoff "t" nil (lambda (a) (push a got))))
      (should (equal got '(nil)))
      (should-not (superchat-tape-entries :tape "t" :kinds '(anchor)))
      (should (string-match-p "no summary"
                              (cdr (car (last (superchat-test-agent--entries "t")))))))))

(ert-deftest superchat-agent-a-later-signal-in-the-ending-is-shown ()
  (superchat-test-agent--with
    (let (shown)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
        (superchat-test-script
            (list `(:calls ,(mapcar (lambda (i) (superchat-test-agent--call i "echo" :x "a"))
                                    '("c1" "c2" "c3"))))
          (condition-case nil
              (superchat-run "t" "go"
                             :on-entry (lambda (e)
                                         (when (equal (plist-get (plist-get e :data) :call-id) "c1")
                                           (superchat-cancel "t"))
                                         (when (equal (plist-get e :text) "Cancelled.")
                                           (error "display broke")))
                             :on-done #'ignore)
            (error nil))))
      ;; Two Cancelled. results: the first signal is raised, the second shown.
      (should (= (cl-count-if (lambda (m) (string-match-p "display broke" m)) shown) 1)))))

;;; The sixth review

(ert-deftest superchat-agent-on-done-error-wins-and-the-pending-on-entry-error-is-shown ()
  (superchat-test-agent--with
    (let (shown caught)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) shown))))
        (superchat-test-script (list '(:error "boom"))
          (condition-case err
              (superchat-run "t" "go"
                             :on-entry (lambda (e) (when (eq (plist-get e :kind) 'event)
                                                     (error "display broke")))
                             :on-done (lambda (_) (error "done broke")))
            (error (setq caught (cadr err))))))
      (should (equal caught "done broke"))
      (should (cl-some (lambda (m) (string-match-p "display broke" m)) shown))
      (should-not (superchat-run-active "t")))))

(ert-deftest superchat-agent-an-error-raised-by-on-done-reaches-the-caller ()
  (superchat-test-agent--with
    (superchat-test-script (list '(:text "fine"))
      (should (equal (cadr (should-error (superchat-run "t" "go"
                                                        :on-done (lambda (_) (error "done broke")))))
                     "done broke")))
    (should-not (superchat-run-active "t"))))

(ert-deftest superchat-agent-a-signal-inside-a-tool-call-is-only-the-error-event ()
  (superchat-test-agent--with
    (let (dones run)
      (cl-letf (((symbol-function 'superchat-tool-call) (lambda (&rest _) (error "tape broke"))))
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "echo" :x "a"))))
          (setq run (superchat-run "t" "go"
                                   :on-done (lambda (r) (push (plist-get r :status) dones))))))
      (should (eq (plist-get run :status) 'error))
      (should (string-match-p "tape broke" (plist-get run :error)))
      (should (equal dones '(error)))
      (should (equal (superchat-test-agent--types "t") '(user assistant tool error))))))

(defun superchat-test-agent--answer-once (call)
  "A request function: the first request answers at once with CALL, later
ones never answer.  Each handle is a fresh list."
  (let ((n 0))
    (lambda (_messages &rest keys)
      (when (= (cl-incf n) 1)
        (funcall (plist-get keys :on-done) (list :text "" :calls (list call) :model "s")))
      (list :cancelled nil :finished nil :request n))))

(ert-deftest superchat-agent-a-tool-that-answered-is-never-stopped ()
  (superchat-test-agent--with
    (let ((stopped nil) (superchat-llm-request-function
                         (superchat-test-agent--answer-once
                          (superchat-test-agent--call "c1" "echo" :x "a"))))
      (cl-letf (((symbol-function 'superchat-tool-call)
                 (lambda (tape call on-result &rest _)
                   (funcall on-result (superchat-tape-append
                                       tape 'tool "done" :data (list :call-id (plist-get call :id)
                                                                     :name "echo")))
                   (lambda () (setq stopped t))))
                ((symbol-function 'superchat-llm-cancel) #'ignore))
        (superchat-run "t" "go")
        (superchat-cancel "t"))
      (should-not stopped)
      (should-not (superchat-run-active "t")))))

(ert-deftest superchat-agent-a-cancel-never-cancels-a-request-that-answered ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slowx
      (let ((cancelled nil) (superchat-llm-request-function
                             (superchat-test-agent--answer-once
                              (superchat-test-agent--call "c1" "slowx"))))
        (cl-letf (((symbol-function 'superchat-llm-cancel) (lambda (h) (push h cancelled))))
          (superchat-run "t" "go")
          (should (superchat-run-active "t"))
          (superchat-cancel "t"))
        (superchat-test-agent--idle)
        (should-not cancelled)
        (should (equal (superchat-test-agent--types "t") '(user assistant tool cancelled)))))))

(ert-deftest superchat-agent-a-change-of-the-budget-is-seen-at-the-next-request ()
  (superchat-test-agent--with
    (let ((superchat-context-chars 100000))
      (superchat-tool-define "shrink" :class 'read :description "Shrinks the budget"
                             :function (lambda (_) (setq superchat-context-chars 100) "ok"))
      (unwind-protect
          (superchat-test-script
              (list `(:calls (,(superchat-test-agent--call "c1" "shrink"))) '(:text "never"))
            (let ((run (superchat-run "t" (make-string 300 ?u))))
              (should (= (length superchat-test-requests) 1))
              (should (eq (plist-get run :status) 'error))
              (should (string-match-p "is 100\\." (plist-get run :error)))))
        (superchat-tool-remove "shrink")))))

(ert-deftest superchat-agent-fork-at-an-entry-not-in-the-conversation-is-an-error ()
  (superchat-test-agent--with
    (let ((other (superchat-tape-append "o" 'user "elsewhere" :scope "home")))
      (superchat-tape-append "t" 'user "q" :scope "home")
      (should-error (superchat-fork "t" (plist-get other :id)))
      (should-error (superchat-fork "t" 0)))))

(ert-deftest superchat-test-agent-alone-has-memory-tools ()
  "A package that requires only `superchat-agent' can recall and keep notes.
Run in a fresh Emacs: in this one another test file may have loaded them."
  (let ((dirs (mapcar (lambda (f) (file-name-directory (locate-library f)))
                      '("superchat-agent" "superchat-bridge"))))
    (with-temp-buffer
      (should (= 0 (apply #'call-process
                          (expand-file-name invocation-name invocation-directory)
                          nil t nil "-Q" "--batch"
                          (append
                           (mapcan (lambda (d) (list "-L" d)) dirs)
                           (list "--eval"
                                 (concat "(progn (require 'superchat-agent) (prin1"
                                         " (mapcar (lambda (s) (and (plist-get s :description) t))"
                                         " (superchat-tool-specs '(\"recall\" \"tape_query\" \"note\")))))"))))))
      (should (equal "(t t t)" (string-trim (buffer-string)))))))

(provide 'test-agent)
;;; test-agent.el ends here

(ert-deftest superchat-agent-steered-text-goes-in-after-the-calls ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow nil
      (let (status steers)
        (superchat-test-script
            (list `(:calls (,(superchat-test-agent--call "c1" "slow")
                            ,(superchat-test-agent--call "c2" "echo" :x "b")))
                  '(:text "ok, not that file"))
          (superchat-run "t" "go" :on-done (lambda (r) (setq status (plist-get r :status)
                                                             steers (plist-get r :steers))))
          ;; Steered while a call is out: nothing goes in between a call and its result.
          (superchat-steer "t" "not that file")
          (should-not (superchat-tape-entries :tape "t" :kinds '(tool)))
          (funcall superchat-test-agent--done "slow")
          (superchat-test-wait (lambda () status))
          ;; The request after it carries both results, then the steered text.
          (should (equal (mapcar (lambda (m) (plist-get m :role))
                                 (plist-get (car superchat-test-requests) :messages))
                         '(user assistant tool tool user))))
        (should (eq status 'done))
        (should-not steers)
        (should (equal (superchat-test-agent--entries "t")
                       '((user . "go") (assistant . "") (tool . "slow") (tool . "echo:b")
                         (user . "not that file") (assistant . "ok, not that file"))))
        (should (plist-get (plist-get (nth 4 (superchat-tape-entries :tape "t")) :data) :steer))))))

(ert-deftest superchat-agent-steered-text-a-run-never-took-comes-back ()
  (superchat-test-agent--with
    (superchat-test-agent--with-slow nil
      (let (done)
        (superchat-test-script (list `(:calls (,(superchat-test-agent--call "c1" "slow"))))
          (superchat-run "t" "go" :on-done (lambda (r) (setq done r)))
          (superchat-steer "t" "one")
          (superchat-steer "t" "two")
          (superchat-cancel "t"))
        (should (eq (plist-get done :status) 'cancelled))
        (should (equal (plist-get done :steers) '("one" "two")))
        (should-not (member "one" (mapcar #'cdr (superchat-test-agent--entries "t"))))
        (should-error (superchat-steer "t" "late"))))))

(ert-deftest superchat-agent-at-the-limit-waiting-input-pauses-the-run ()
  (superchat-test-agent--with
    (superchat-tape-append "t" 'user "earlier" :scope "home")
    (superchat-permission-set "t" 'auto)
    (let* ((superchat-max-rounds 1) done
           (superchat-limit-pending-functions (list (lambda (tape) (equal tape "t"))))
           (call `(:calls (,(superchat-test-agent--call "c" "echo" :x "a")))))
      (superchat-test-script (list call '(:text "never asked"))
        (superchat-run "t" "go" :on-done (lambda (r) (setq done r)))
        ;; No look-back, no going on: the waiting input drives the next run.
        (should (= (length superchat-test-requests) 1)))
      (should (eq (plist-get done :status) 'done))
      (should (plist-get done :paused))
      (should-not (member 'continued (superchat-test-agent--types "t"))))))
