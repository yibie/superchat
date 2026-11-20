;;; superchat-timer.el --- Timers that put work in the inbox, and dreaming -*- lexical-binding: t; -*-

;;; Commentary:
;; A timer is a `timer' entry on the Tape.  When it is due it puts an item in
;; the inbox, whose source and id say which timer and which time: the inbox's
;; own dedup is what makes a time fire once.  Polled once a minute while
;; Emacs runs; what came due while it was closed fires once at the next look.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-tools)
(require 'superchat-inbox)

(defvar superchat-timer-poll-seconds 60
  "Seconds between two looks at the timers once `superchat-timer-start' ran.")

(defvar superchat-timer--poll nil "The repeating timer of `superchat-timer-start', or nil.")

(defun superchat-timer--fired-p (id time)
  "Non-nil when timer ID has put its item for the time TIME."
  (superchat-tape-select
   (concat "SELECT 1 FROM entry WHERE kind = 'inbox'"
           " AND json_extract(data, '$.source') = 'timer'"
           " AND json_extract(data, '$.id') = ? LIMIT 1")
   (list (format "%d@%d" id (floor time)))))

(defvar superchat-timer-max 8
  "Timers that may still fire one conversation may have: a model cannot
fill the Tape with timers that run nobody watches.")

(defvar superchat-timer-days 7
  "Days a `schedule' set by the model fires before it lapses; dreaming,
which code sets, has no end.")

(defun superchat-timer-list (&optional tape)
  "The timers, of TAPE or of every timeline, that may still fire, oldest
first: not cancelled, not lapsed and, for a follow-up, not fired."
  (let* ((all (superchat-tape-entries :tape tape :kinds '(timer)))
         (cancelled (cl-loop for e in all when (plist-get (plist-get e :data) :cancel)
                             append (plist-get e :refs))))
    (seq-remove (lambda (e) (let ((data (plist-get e :data)))
                              (or (plist-get data :cancel) (memq (plist-get e :id) cancelled)
                                  (and (plist-get data :until) (> (float-time) (plist-get data :until)))
                                  (and (plist-get data :at)
                                       (superchat-timer--fired-p (plist-get e :id)
                                                                 (plist-get data :at))))))
                all)))

(defun superchat-timer--due (timer now)
  "The latest time, at or before NOW and not before TIMER was set, at which
it is due; nil when there is none."
  (let* ((data (plist-get timer :data)) (set (plist-get timer :at))
         (time (cond ((plist-get data :at))
                     ((plist-get data :every)
                      (let ((from (or (plist-get data :from) set)) (every (plist-get data :every)))
                        (and (>= now from) (+ from (* every (floor (- now from) every))))))
                     ((plist-get data :daily)
                      (let* ((d (decode-time now))
                             (hm (mapcar #'string-to-number (split-string (plist-get data :daily) ":")))
                             (today (float-time (encode-time
                                                 (list 0 (cadr hm) (car hm) (decoded-time-day d)
                                                       (decoded-time-month d) (decoded-time-year d)
                                                       nil -1 nil)))))
                        (if (<= today now) today (- today 86400)))))))
    (and time (<= time now) (or (plist-get data :at) (>= time set)) time)))

(defun superchat-timer--fire (now)
  "Put the item of each timer due at NOW that has not put it yet: one item for
the latest time it is due, however many were missed while Emacs was closed."
  (dolist (timer (superchat-timer-list))
    (when-let* ((time (superchat-timer--due timer now)))
      (superchat-inbox-put (plist-get timer :tape)
                           (format "Your timer %d is due: %s" (plist-get timer :id)
                                   (plist-get timer :text))
                           :source "timer" :id (format "%d@%d" (plist-get timer :id) (floor time))
                           :unattended t))))

;;;###autoload
(defun superchat-timer-start ()
  "Look at the timers and the inbox now and every `superchat-timer-poll-seconds'."
  (interactive)
  (when superchat-timer--poll (cancel-timer superchat-timer--poll))
  (setq superchat-timer--poll
        (run-at-time 0 superchat-timer-poll-seconds
                     (lambda () (superchat-timer--fire (float-time)) (superchat-inbox-wake)))))

(defun superchat-timer--number (value)
  "VALUE as a number: a model may send a number as a string."
  (if (stringp value) (string-to-number value) value))

(defun superchat-timer--set (data text)
  "Append a timer with DATA and TEXT to the timeline the tool runs for; one
that repeats lapses after `superchat-timer-days'."
  (when (>= (length (superchat-timer-list superchat-tool-tape)) superchat-timer-max)
    (error "This conversation has %d timers that may still fire; cancel one first"
           superchat-timer-max))
  (let ((timer (superchat-tape-append
                superchat-tool-tape 'timer text
                :data (if (plist-get data :at) data
                        (append data (list :until (+ (float-time) (* superchat-timer-days 86400))))))))
    (format "Timer %d set; cancel_timer with that id stops it." (plist-get timer :id))))

(superchat-tool-define
 "follow_up" :class 'write :deferred t
 :description "Come back to this conversation later, with nobody watching: after IN seconds (60 or more) you get TEXT as a new message. Only for what waits on time or on someone else, or a reminder the person asked for; never to split work you can do now: go on with it in this response. Do not poll."
 :args '((:name "in" :type number :description "Seconds from now" :required t)
         (:name "text" :type string :description "What to do then" :required t))
 :function (lambda (args)
             (let ((in (superchat-timer--number (plist-get args :in))))
               ;; The timers are looked at once a minute: a sooner time is a promise
               ;; they cannot keep, and an invitation to pace work by timer.
               (unless (and (numberp in) (>= in 60))
                 (error "IN is 60 seconds or more: timers are looked at once a minute"))
               (superchat-timer--set (list :at (+ (float-time) in)) (plist-get args :text)))))

(superchat-tool-define
 "schedule" :class 'write :deferred t
 :description "Do TEXT again and again in this conversation, with nobody watching: every EVERY seconds (at least 60), or daily at DAILY (\"HH:MM\", local time). Only when the person asked for it."
 :args '((:name "every" :type number :description "Seconds between two times")
         (:name "daily" :type string :description "\"HH:MM\" each day")
         (:name "text" :type string :description "What to do each time" :required t))
 :function (lambda (args)
             (let ((every (superchat-timer--number (plist-get args :every)))
                   (daily (plist-get args :daily)))
               (superchat-timer--set
                (cond ((and every daily) (error "Give EVERY or DAILY, not both"))
                      ((and (numberp every) (>= every 60)) (list :every every :from (+ (float-time) every)))
                      ((and (stringp daily)
                            (string-match-p "\\`\\([01]?[0-9]\\|2[0-3]\\):[0-5][0-9]\\'" daily))
                       (list :daily daily))
                      (t (error "EVERY is 60 seconds or more, DAILY is \"HH:MM\"")))
                (plist-get args :text)))))

(defun superchat-timer-cancel (id)
  "Cancel timer ID, of any timeline."
  (let ((timer (car (seq-filter (lambda (e) (eql (plist-get e :id) id))
                                (superchat-timer-list)))))
    (unless timer (error "No timer %s that may still fire" id))
    (superchat-tape-append (plist-get timer :tape) 'timer "" :data '(:cancel t)
                           :refs (list id))
    (format "Timer %d cancelled." id)))

(superchat-tool-define
 "cancel_timer" :class 'read :deferred t
 :description "Stop a timer that follow_up or schedule set."
 :args '((:name "id" :type number :description "The timer's id" :required t))
 :function (lambda (args)
             (superchat-timer-cancel (superchat-timer--number (plist-get args :id)))))

;;; Dreaming

(defconst superchat-timer--dream
  "Review the notes of this project while nobody watches. Read the notes with recall (view notes), and what happened since your last review with recall (views timelines and search). When two notes say the same, or one is wrong or out of date, write one note with the note tool that replaces them: its sources are the ids of the notes it replaces. Delete nothing. Send nothing unless a note contradicts what the person said. When nothing is new since your last review, write nothing."
  "The task of a dreaming run; its timer is known by `:dream', not this text.")

(defun superchat-timer-dream (scope &optional off)
  "Turn dreaming on for SCOPE, a daily review of its notes at 03:00, and
return its timer; with OFF, turn it off and return nil."
  (let ((timer (seq-find (lambda (e) (and (plist-get (plist-get e :data) :dream)
                                          (equal (plist-get e :scope) scope)))
                         (superchat-timer-list))))
    (cond (off (when timer (superchat-timer-cancel (plist-get timer :id))) nil)
          (timer)
          (t (superchat-tape-append (superchat-tape-new-name) 'timer superchat-timer--dream
                                    :data '(:daily "03:00" :dream t) :scope scope)))))

(provide 'superchat-timer)
;;; superchat-timer.el ends here
