;;; superchat-inbox.el --- The inbox and its worker -*- lexical-binding: t; -*-

;;; Commentary:
;; Work for a timeline goes into the inbox, an `inbox' entry on the Tape, and
;; one worker starts the runs.  An item is pending until a `user' entry has it
;; in its refs: the Tape stays append-only, and what was sent is the only
;; thing that says an item was answered.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-tools)
(require 'superchat-agent)

(defvar superchat-inbox--timer nil "The timer of the worker's next look, or nil.")

(defconst superchat-inbox--quiet
  "Nobody is watching this run. The person sees only what you send with the send tool, and the question when a call needs their permission. Send only what they need to act on or asked for; send nothing when there is nothing new."
  "The system prompt's extra text for a run nobody watches: start quiet.")

(defun superchat-inbox-pending (&optional tape)
  "The pending items, oldest first; with TAPE, those of that timeline."
  (let* ((rows (superchat-tape-select
                (concat "SELECT i.id, i.tape FROM entry i WHERE i.kind = 'inbox'"
                        (if tape " AND i.tape = ?" "")
                        " AND NOT EXISTS (SELECT 1 FROM entry u, json_each(u.refs) r"
                        " WHERE u.kind = 'user' AND u.tape = i.tape AND r.value = i.id)"
                        " ORDER BY i.id")
                (and tape (list tape))))
         (ids (mapcar #'car rows)))
    (sort (cl-loop for name in (delete-dups (mapcar #'cadr rows))
                   append (seq-filter (lambda (e) (memq (plist-get e :id) ids))
                                      (superchat-tape-entries :tape name :kinds '(inbox))))
          (lambda (a b) (< (plist-get a :id) (plist-get b :id))))))

(cl-defun superchat-inbox-put (tape text &key source id unattended scope)
  "Put TEXT in the inbox of timeline TAPE and wake the worker; return the item.
When an item of SOURCE with ID is already on the Tape, put nothing and return
nil.  SCOPE is that of a new timeline, by default that of `default-directory'.
UNATTENDED makes the run that answers it ask nobody."
  (unless (and source id
               (superchat-tape-select
                (concat "SELECT 1 FROM entry WHERE kind = 'inbox'"
                        " AND json_extract(data, '$.source') = ?"
                        " AND json_extract(data, '$.id') = ? LIMIT 1")
                (list source id)))
    (prog1 (superchat-tape-append
            tape 'inbox text
            :data (list :source source :id id :unattended (and unattended t))
            :scope (unless (superchat-tape-entries :tape tape :limit 1)
                     (or scope (superchat-tape-scope-of default-directory))))
      (superchat-inbox-wake))))

(defun superchat-inbox-wake ()
  "Let the worker look at the inbox once Emacs is free: after an item is put,
after a run it started ends, and when Emacs starts, for what was left."
  (unless superchat-inbox--timer
    (setq superchat-inbox--timer (run-at-time 0 nil #'superchat-inbox--work))))

(defun superchat-inbox--work ()
  "Start one run for each timeline with pending items that is free: no run,
no handoff, no question waiting.  Its text is the items' texts, oldest first."
  (setq superchat-inbox--timer nil)
  (pcase-dolist (`(,tape . ,items) (seq-group-by (lambda (e) (plist-get e :tape))
                                                (superchat-inbox-pending)))
    (unless (or (superchat-run-active tape) (superchat-question tape))
      ;; One timeline that cannot start must not keep the others waiting;
      ;; its items stay pending and the person is told.
      (condition-case err
          (let ((unattended (seq-some (lambda (e) (plist-get (plist-get e :data) :unattended))
                                      items)))
            (superchat-run tape (mapconcat (lambda (e) (plist-get e :text)) items "\n\n")
                           :refs (mapcar (lambda (e) (plist-get e :id)) items)
                           :unattended unattended
                           :system (and unattended superchat-inbox--quiet)
                           :on-done (lambda (run)
                                      (when (eq (plist-get run :status) 'waiting)
                                        (message "Superchat: %s waits for your answer: %s" tape
                                                 (plist-get (superchat-question tape) :text)))
                                      (superchat-inbox-wake))))
        (error (message "Superchat inbox: %s: %s" tape (error-message-string err)))))))

;;; Timers

(defvar superchat-inbox-poll-seconds 60
  "Seconds between two looks at the timers once `superchat-inbox-start' ran.")

(defvar superchat-inbox--poll nil "The repeating timer of `superchat-inbox-start', or nil.")

(defun superchat-inbox--fired-p (id time)
  "Non-nil when timer ID has put its item for the time TIME."
  (superchat-tape-select
   (concat "SELECT 1 FROM entry WHERE kind = 'inbox'"
           " AND json_extract(data, '$.source') = 'timer'"
           " AND json_extract(data, '$.id') = ? LIMIT 1")
   (list (format "%d@%d" id (floor time)))))

(defun superchat-inbox-timers (&optional tape)
  "The timers, of TAPE or of every timeline, that may still fire, oldest
first: not cancelled and, for a follow-up, not fired."
  (let* ((all (superchat-tape-entries :tape tape :kinds '(timer)))
         (cancelled (cl-loop for e in all when (plist-get (plist-get e :data) :cancel)
                             append (plist-get e :refs))))
    (seq-remove (lambda (e) (let ((data (plist-get e :data)))
                              (or (plist-get data :cancel) (memq (plist-get e :id) cancelled)
                                  (and (plist-get data :at)
                                       (superchat-inbox--fired-p (plist-get e :id)
                                                                 (plist-get data :at))))))
                all)))

(defun superchat-inbox--due (timer now)
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

(defun superchat-inbox--fire (now)
  "Put the item of each timer due at NOW that has not put it yet: one item for
the latest time it is due, however many were missed while Emacs was closed."
  (dolist (timer (superchat-inbox-timers))
    (when-let* ((time (superchat-inbox--due timer now)))
      (superchat-inbox-put (plist-get timer :tape)
                           (format "Your timer %d is due: %s" (plist-get timer :id)
                                   (plist-get timer :text))
                           :source "timer" :id (format "%d@%d" (plist-get timer :id) (floor time))
                           :unattended t))))

(defun superchat-inbox-start ()
  "Look at the timers and the inbox now and every `superchat-inbox-poll-seconds'."
  (interactive)
  (when superchat-inbox--poll (cancel-timer superchat-inbox--poll))
  (setq superchat-inbox--poll
        (run-at-time 0 superchat-inbox-poll-seconds
                     (lambda () (superchat-inbox--fire (float-time)) (superchat-inbox-wake)))))

(defun superchat-inbox--number (value)
  "VALUE as a number: a model may send a number as a string."
  (if (stringp value) (string-to-number value) value))

(defun superchat-inbox--set (data text)
  "Append a timer with DATA and TEXT to the timeline the tool runs for."
  (let ((timer (superchat-tape-append superchat-tool-tape 'timer text :data data)))
    (format "Timer %d set; cancel_timer with that id stops it." (plist-get timer :id))))

(superchat-tool-define
 "follow_up" :class 'read
 :description "Come back to this conversation later: after IN seconds you get TEXT as a new message, with nobody watching. Use it to check on something that takes time; do not poll."
 :args '((:name "in" :type number :description "Seconds from now" :required t)
         (:name "text" :type string :description "What to do then" :required t))
 :function (lambda (args)
             (let ((in (superchat-inbox--number (plist-get args :in))))
               (unless (and (numberp in) (>= in 0)) (error "IN is a number of seconds, 0 or more"))
               (superchat-inbox--set (list :at (+ (float-time) in)) (plist-get args :text)))))

(superchat-tool-define
 "schedule" :class 'read
 :description "Do TEXT again and again in this conversation, with nobody watching: every EVERY seconds (at least 60), or daily at DAILY (\"HH:MM\", local time). Only when the person asked for it."
 :args '((:name "every" :type number :description "Seconds between two times")
         (:name "daily" :type string :description "\"HH:MM\" each day")
         (:name "text" :type string :description "What to do each time" :required t))
 :function (lambda (args)
             (let ((every (superchat-inbox--number (plist-get args :every)))
                   (daily (plist-get args :daily)))
               (superchat-inbox--set
                (cond ((and every daily) (error "Give EVERY or DAILY, not both"))
                      ((and (numberp every) (>= every 60)) (list :every every :from (+ (float-time) every)))
                      ((and (stringp daily)
                            (string-match-p "\\`\\([01]?[0-9]\\|2[0-3]\\):[0-5][0-9]\\'" daily))
                       (list :daily daily))
                      (t (error "EVERY is 60 seconds or more, DAILY is \"HH:MM\"")))
                (plist-get args :text)))))

(defun superchat-inbox-cancel-timer (id)
  "Cancel timer ID, of any timeline."
  (let ((timer (car (seq-filter (lambda (e) (eql (plist-get e :id) id))
                                (superchat-inbox-timers)))))
    (unless timer (error "No timer %s that may still fire" id))
    (superchat-tape-append (plist-get timer :tape) 'timer "" :data '(:cancel t)
                           :refs (list id))
    (format "Timer %d cancelled." id)))

(superchat-tool-define
 "cancel_timer" :class 'read
 :description "Stop a timer that follow_up or schedule set."
 :args '((:name "id" :type number :description "The timer's id" :required t))
 :function (lambda (args)
             (superchat-inbox-cancel-timer (superchat-inbox--number (plist-get args :id)))))

;;; Dreaming

(defconst superchat-inbox--dream
  "Review the notes of this project while nobody watches. Read them with recall (view notes), and what happened since your last review with recall (views timelines and search). When two notes say the same, or one is wrong or out of date, write one note with the note tool that replaces them: its sources are the ids of the notes it replaces. Delete nothing. Send nothing unless a note contradicts what the person said. When nothing is new since your last review, write nothing."
  "The task of a dreaming run, and how its timer is known.")

(defun superchat-inbox-dream (scope &optional off)
  "Turn dreaming on for SCOPE, a daily review of its notes at 03:00, and
return its timer; with OFF, turn it off and return nil."
  (let ((timer (seq-find (lambda (e) (and (equal (plist-get e :text) superchat-inbox--dream)
                                          (equal (plist-get e :scope) scope)))
                         (superchat-inbox-timers))))
    (cond (off (when timer (superchat-inbox-cancel-timer (plist-get timer :id))) nil)
          (timer)
          (t (superchat-tape-append (superchat-tape-new-name) 'timer superchat-inbox--dream
                                    :data '(:daily "03:00") :scope scope)))))

;;; Sending

(superchat-tool-define
 "send" :class 'read
 :description "Tell the person something now: a line they see even when they are not reading this conversation. Only what they need to act on or asked for."
 :args '((:name "text" :type string :description "The message" :required t))
 :function (lambda (args)
             (let ((text (plist-get args :text)))
               (superchat-tape-append superchat-tool-tape 'event text :data '(:type "sent"))
               (message "Superchat: %s" (car (split-string text "\n")))
               "Sent.")))

(provide 'superchat-inbox)
;;; superchat-inbox.el ends here
