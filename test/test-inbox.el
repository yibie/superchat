;;; test-inbox.el --- tests for superchat-inbox -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-inbox)

(defmacro superchat-test-inbox--with (&rest body)
  "BODY with a Tape, in a temporary directory, no run and no worker waiting."
  (declare (indent 0))
  `(superchat-test-with-tape
     (let ((default-directory (file-name-as-directory (make-temp-file "sc-inbox-" t)))
           (superchat-agent--runs (make-hash-table :test #'equal))
           (superchat-inbox--timer nil))
       (unwind-protect (progn ,@body)
         (when superchat-inbox--timer (cancel-timer superchat-inbox--timer))
         (delete-directory default-directory t)))))

(defun superchat-test-inbox--idle ()
  "Wait until the worker has nothing more to start and no run goes."
  (superchat-test-wait (lambda () (and (null superchat-inbox--timer)
                                       (= 0 (hash-table-count superchat-agent--runs))))))

(ert-deftest superchat-inbox-items-put-together-are-one-run ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "All three, answered."))
      (let ((items (list (superchat-inbox-put "t" "one")
                         (superchat-inbox-put "t" "two")
                         (superchat-inbox-put "t" "three"))))
        (should (= 3 (length (superchat-inbox-pending "t"))))
        (superchat-test-inbox--idle)
        (should (= 1 (length superchat-test-requests)))
        (let ((user (car (superchat-tape-entries :tape "t" :kinds '(user)))))
          (should (equal (plist-get user :text) "one\n\ntwo\n\nthree"))
          (should (equal (plist-get user :refs) (mapcar (lambda (e) (plist-get e :id)) items))))
        (should-not (superchat-inbox-pending))))))

(ert-deftest superchat-inbox-an-item-waits-for-the-run-that-goes ()
  (superchat-test-inbox--with
    (let ((superchat-test-async t))
      (superchat-test-script (list '(:text "first") '(:text "second"))
        (superchat-run "t" "go")
        (superchat-inbox-put "t" "later")
        (superchat-test-drain)
        (superchat-test-inbox--idle)
        (superchat-test-drain)
        (superchat-test-inbox--idle)
        (should (equal (mapcar (lambda (e) (plist-get e :text))
                               (superchat-tape-entries :tape "t" :kinds '(user)))
                       '("go" "later")))
        (should (= 2 (length superchat-test-requests)))))))

(ert-deftest superchat-inbox-an-item-from-a-source-is-put-once ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "ok"))
      (should (superchat-inbox-put "t" "build failed" :source "ci" :id "run-7"))
      (should-not (superchat-inbox-put "t" "build failed" :source "ci" :id "run-7"))
      (should (= 1 (length (superchat-tape-entries :tape "t" :kinds '(inbox)))))
      (superchat-test-inbox--idle))))

(ert-deftest superchat-inbox-what-was-pending-when-emacs-stopped-is-answered ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'inbox "left over" :scope "home"
                           :data '(:source nil :id nil :unattended t))
    (superchat-test-script (list '(:text "done"))
      (superchat-inbox-wake)
      (superchat-test-inbox--idle)
      (should (equal (plist-get (car (superchat-tape-entries :tape "t" :kinds '(user))) :text)
                     "left over"))
      (should-not (superchat-inbox-pending "t")))))

(ert-deftest superchat-inbox-an-unattended-item-waits-at-a-question ()
  (superchat-test-inbox--with
    (superchat-tool-define "poke" :class 'write :description "Poke" :function (lambda (_) "poked"))
    (unwind-protect
        (let ((superchat-default-level 'ask)
              (superchat-permission-ask-function (lambda (&rest _) (error "Asked in the minibuffer"))))
          (superchat-test-script (list '(:text "" :calls ((:id "c1" :name "poke" :args nil))))
            (superchat-inbox-put "t" "poke it" :unattended t)
            (superchat-test-inbox--idle)
            (should (superchat-question "t"))
            (superchat-inbox-put "t" "and then")
            (superchat-test-inbox--idle)
            ;; The new item waits behind the question.
            (should (= 1 (length (superchat-inbox-pending "t"))))))
      (superchat-tool-remove "poke"))))

(defun superchat-test-inbox--tool (name &rest args)
  "Call tool NAME with ARGS for timeline t; its result entry."
  (let (entry)
    (superchat-tool-call "t" (list :id "c" :name name :args args) (lambda (e) (setq entry e)))
    entry))

(defun superchat-test-inbox--fired ()
  (mapcar (lambda (e) (plist-get e :text)) (superchat-tape-entries :tape "t" :kinds '(inbox))))

(ert-deftest superchat-inbox-a-follow-up-fires-once ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (let ((now (float-time)))
      (should (string-match-p "\\`Timer [0-9]+ set"
                              (plist-get (superchat-test-inbox--tool "follow_up" :in "60" :text "check")
                                         :text)))
      (superchat-inbox--fire now)
      (should-not (superchat-test-inbox--fired))
      (superchat-inbox--fire (+ now 61))
      (superchat-inbox--fire (+ now 120))
      (should (equal (superchat-test-inbox--fired) '("check")))
      (should (plist-get (plist-get (car (superchat-tape-entries :tape "t" :kinds '(inbox))) :data)
                         :unattended))
      (should-not (superchat-inbox-timers)))))

(ert-deftest superchat-inbox-a-schedule-missed-many-times-fires-once ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (let ((now (float-time)))
      (superchat-tape-append "t" 'timer "daily brief" :data '(:daily "09:00") :at (- now (* 3 86400)))
      (superchat-tape-append "t" 'timer "tick" :data (list :every 300 :from (- now 1000))
                             :at (- now 1000))
      (superchat-inbox--fire now)
      (superchat-inbox--fire (+ now 10))
      (should (equal (sort (superchat-test-inbox--fired) #'string<) '("daily brief" "tick")))
      (superchat-inbox--fire (+ now 300))
      (should (equal (sort (superchat-test-inbox--fired) #'string<) '("daily brief" "tick" "tick")))
      (should (= 2 (length (superchat-inbox-timers)))))))

(ert-deftest superchat-inbox-a-schedule-does-not-fire-for-a-time-before-it-was-set ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (should (string-match-p "set" (plist-get (superchat-test-inbox--tool "schedule" :daily "0:00"
                                                                         :text "x")
                                             :text)))
    (should (string-match-p "set" (plist-get (superchat-test-inbox--tool "schedule" :every 60
                                                                         :text "y")
                                             :text)))
    (superchat-inbox--fire (+ (float-time) 1))
    (should-not (superchat-test-inbox--fired))))

(ert-deftest superchat-inbox-a-cancelled-timer-never-fires ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-test-inbox--tool "follow_up" :in 0 :text "never")
    (let ((id (plist-get (car (superchat-inbox-timers)) :id)))
      (should (string-match-p "cancelled"
                              (plist-get (superchat-test-inbox--tool "cancel_timer" :id id) :text)))
      (superchat-inbox--fire (+ (float-time) 10))
      (should-not (superchat-test-inbox--fired))
      (should (plist-get (plist-get (superchat-test-inbox--tool "cancel_timer" :id id) :data)
                         :error)))))

(ert-deftest superchat-inbox-a-timer-tool-refuses-what-it-cannot-keep ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (dolist (args '((:every 10 :text "x") (:daily "25:00" :text "x") (:text "x")
                    (:every 60 :daily "09:00" :text "x")))
      (should (plist-get (plist-get (apply #'superchat-test-inbox--tool "schedule" args) :data)
                         :error)))
    (should-not (superchat-inbox-timers))))

(ert-deftest superchat-inbox-start-fires-due-timers-and-runs-them ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-tape-append "t" 'assistant "hello")
    (superchat-tape-append "t" 'timer "look again" :data (list :at (float-time)))
    (let ((superchat-inbox-poll-seconds 0.05) (superchat-inbox--poll nil))
      (unwind-protect
          (superchat-test-script (list '(:text "looked"))
            (superchat-inbox-start)
            (superchat-test-wait (lambda () (cdr (superchat-tape-entries :tape "t" :kinds '(user)))))
            (superchat-test-inbox--idle)
            (should (equal (plist-get (car (last (superchat-tape-entries :tape "t"))) :text) "looked")))
        (cancel-timer superchat-inbox--poll)))))

(provide 'test-inbox)
;;; test-inbox.el ends here
