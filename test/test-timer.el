;;; test-timer.el --- tests for superchat-timer -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-timer)
(require 'test-inbox)

(ert-deftest superchat-timer-a-run-nobody-watches-cannot-set-a-timer-unasked ()
  (superchat-test-inbox--with
    (let ((superchat-default-level 'ask)
          (superchat-permission-ask-function (lambda (&rest _) (error "Asked in the minibuffer"))))
      (superchat-test-script
          (list '(:text "" :calls ((:id "l1" :name "load_tools" :args (:names "follow_up"))))
                '(:text "" :calls ((:id "f1" :name "follow_up" :args (:in 60 :text "again")))))
        (let ((inhibit-message t))
          (superchat-inbox-put "t" "Your timer 1 is due: again" :unattended t)
          (superchat-test-inbox--idle))
        ;; Loaded, a timer still makes more work nobody watches: the run waits
        ;; for the person.
        (should (superchat-question "t"))
        (should-not (superchat-timer-list "t"))))))

(defun superchat-test-inbox--tool (name &rest args)
  "Call tool NAME with ARGS for timeline t; its result entry."
  (let (entry)
    (superchat-tool-call "t" (list :id "c" :name name :args args) (lambda (e) (setq entry e)))
    entry))

(defun superchat-test-inbox--fired ()
  "The texts the timers put, each without its `Your timer ID is due: '."
  (mapcar (lambda (e)
            (let ((text (plist-get e :text)))
              (should (string-match "\\`Your timer [0-9]+ is due: " text))
              (substring text (match-end 0))))
          (superchat-tape-entries :tape "t" :kinds '(inbox))))

(ert-deftest superchat-timer-a-follow-up-fires-once ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (let ((now (float-time)))
      (should (string-match-p "\\`Timer [0-9]+ set"
                              (plist-get (superchat-test-inbox--tool "follow_up" :in "60" :text "check")
                                         :text)))
      (superchat-timer--fire now)
      (should-not (superchat-test-inbox--fired))
      (superchat-timer--fire (+ now 61))
      (superchat-timer--fire (+ now 120))
      (should (equal (superchat-test-inbox--fired) '("check")))
      (should (plist-get (plist-get (car (superchat-tape-entries :tape "t" :kinds '(inbox))) :data)
                         :unattended))
      (should-not (superchat-timer-list)))))

(ert-deftest superchat-timer-a-schedule-missed-many-times-fires-once ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (let ((now (float-time)))
      (superchat-tape-append "t" 'timer "daily brief" :data '(:daily "09:00") :at (- now (* 3 86400)))
      (superchat-tape-append "t" 'timer "tick" :data (list :every 300 :from (- now 1000))
                             :at (- now 1000))
      (superchat-timer--fire now)
      (superchat-timer--fire (+ now 10))
      (should (equal (sort (superchat-test-inbox--fired) #'string<) '("daily brief" "tick")))
      (superchat-timer--fire (+ now 300))
      (should (equal (sort (superchat-test-inbox--fired) #'string<) '("daily brief" "tick" "tick")))
      (should (= 2 (length (superchat-timer-list)))))))

(ert-deftest superchat-timer-a-schedule-does-not-fire-for-a-time-before-it-was-set ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (should (string-match-p "set" (plist-get (superchat-test-inbox--tool "schedule" :daily "0:00"
                                                                         :text "x")
                                             :text)))
    (should (string-match-p "set" (plist-get (superchat-test-inbox--tool "schedule" :every 60
                                                                         :text "y")
                                             :text)))
    (superchat-timer--fire (+ (float-time) 1))
    (should-not (superchat-test-inbox--fired))))

(ert-deftest superchat-timer-a-cancelled-timer-never-fires ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-test-inbox--tool "follow_up" :in 60 :text "never")
    (let ((id (plist-get (car (superchat-timer-list)) :id)))
      (should (string-match-p "cancelled"
                              (plist-get (superchat-test-inbox--tool "cancel_timer" :id id) :text)))
      (superchat-timer--fire (+ (float-time) 10))
      (should-not (superchat-test-inbox--fired))
      (should (plist-get (plist-get (superchat-test-inbox--tool "cancel_timer" :id id) :data)
                         :error)))))

(ert-deftest superchat-timer-a-timer-tool-refuses-what-it-cannot-keep ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    ;; Timers are looked at once a minute: a follow-up sooner is refused.
    (should (string-match-p "60 seconds or more"
                            (plist-get (superchat-test-inbox--tool "follow_up" :in 5 :text "x") :text)))
    (dolist (args '((:every 10 :text "x") (:daily "25:00" :text "x") (:text "x")
                    (:every 60 :daily "09:00" :text "x")))
      (should (plist-get (plist-get (apply #'superchat-test-inbox--tool "schedule" args) :data)
                         :error)))
    (should-not (superchat-timer-list))))

(ert-deftest superchat-timer-start-fires-due-timers-and-runs-them ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-tape-append "t" 'assistant "hello")
    (superchat-tape-append "t" 'timer "look again" :data (list :at (float-time)))
    (let ((superchat-timer-poll-seconds 0.05) (superchat-timer--poll nil))
      (unwind-protect
          (superchat-test-script (list '(:text "looked"))
            (superchat-timer-start)
            (superchat-test-wait (lambda () (cdr (superchat-tape-entries :tape "t" :kinds '(user)))))
            (superchat-test-inbox--idle)
            (should (equal (plist-get (car (last (superchat-tape-entries :tape "t"))) :text) "looked")))
        (cancel-timer superchat-timer--poll)))))

(ert-deftest superchat-timer-the-model-sees-what-a-timer-tool-answered ()
  (superchat-test-inbox--with
    (superchat-test-script (list '(:text "" :calls ((:id "f1" :name "follow_up" :args (:in 600 :text "x"))))
                                 '(:text "Set."))
      (superchat-run "t" "remind me")
      (superchat-inbox-put "t" "meanwhile")
      (let ((sent (plist-get (car superchat-test-requests) :messages)))
        (should (equal (mapcar (lambda (m) (plist-get m :role)) sent) '(user assistant tool)))
        (should (equal (plist-get (nth 1 sent) :calls)
                       '((:id "f1" :name "follow_up" :args (:in 600 :text "x")))))))
    (superchat-test-inbox--idle)))

(ert-deftest superchat-timer-dreaming-is-one-daily-timer-per-scope ()
  (superchat-test-inbox--with
    (let ((timer (superchat-timer-dream "/p/")))
      (should (equal (plist-get timer :data) '(:daily "03:00")))
      (should (equal (plist-get timer :scope) "/p/"))
      (should (equal (plist-get (superchat-timer-dream "/p/") :id) (plist-get timer :id)))
      (should-not (equal (plist-get (superchat-timer-dream "/q/") :id) (plist-get timer :id)))
      (should (= 2 (length (superchat-timer-list))))
      (should-not (superchat-timer-dream "/q/" t))
      ;; Due a day after it was set, it is a quiet run on its own timeline.
      (superchat-test-script (list '(:text "nothing new"))
        (superchat-timer--fire (+ (float-time) 86400))
        (superchat-test-inbox--idle)
        (should (string-search superchat-inbox--quiet
                               (plist-get (car superchat-test-requests) :system)))
        (should (equal (plist-get (car (superchat-tape-entries :tape (plist-get timer :tape)
                                                               :kinds '(user)))
                                  :text)
                       (format "Your timer %d is due: %s" (plist-get timer :id)
                               superchat-timer--dream))))
      (should-not (superchat-timer-dream "/p/" t))
      (should-not (superchat-timer-list)))))

(provide 'test-inbox)
;;; test-inbox.el ends here

(ert-deftest superchat-timer-a-conversation-has-a-cap-on-its-timers ()
  (superchat-test-inbox--with
    (let ((superchat-timer-max 2))
      (superchat-tape-append "t" 'user "go" :scope "home")
      (superchat-test-inbox--tool "follow_up" :in 600 :text "a")
      (superchat-test-inbox--tool "follow_up" :in 600 :text "b")
      (let ((refused (superchat-test-inbox--tool "follow_up" :in 600 :text "c")))
        (should (plist-get (plist-get refused :data) :error))
        (should (string-search "cancel one first" (plist-get refused :text))))
      (should (= 2 (length (superchat-timer-list "t"))))
      ;; Cancelling one makes room.
      (superchat-timer-cancel (plist-get (car (superchat-timer-list "t")) :id))
      (superchat-test-inbox--tool "follow_up" :in 600 :text "c")
      (should (= 2 (length (superchat-timer-list "t")))))))

(ert-deftest superchat-timer-a-schedule-the-model-set-lapses-and-dreaming-does-not ()
  (superchat-test-inbox--with
    (superchat-tape-append "t" 'user "go" :scope "home")
    (superchat-test-inbox--tool "schedule" :every 60 :text "tick")
    (superchat-timer-dream "/p/")
    (should (= 2 (length (superchat-timer-list))))
    (let ((later (+ (float-time) (* 8 86400))))
      (cl-letf (((symbol-function 'float-time)
                 (let ((real (symbol-function 'float-time)))
                   (lambda (&optional time) (if time (funcall real time) later)))))
        (should (equal (mapcar (lambda (e) (plist-get e :text)) (superchat-timer-list))
                       (list superchat-timer--dream)))
        ;; A lapsed schedule puts nothing, whenever it was due.
        (superchat-timer--fire later)
        (should-not (seq-find (lambda (e) (equal (plist-get (plist-get e :data) :source) "timer"))
                              (superchat-tape-entries :tape "t" :kinds '(inbox))))))))
