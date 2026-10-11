;;; superchat-pending.el --- One list of what is under way and what waits -*- lexical-binding: t; -*-

;;; Commentary:
;; `superchat-pending' shows, in one list, what Superchat is doing and what
;; it is waiting for: executions under way and who started them, Work
;; Records waiting for the user or for a time, records and scheduled tasks
;; that came due and did not start, tasks Superchat cannot tell were
;; started, timers still to come, work that ended unseen, and buffers left
;; unsaved.  Before, these were four lists and a conversation to search.
;;
;; Nothing here is a new fact.  Every row is read from what the other
;; modules already keep -- the Runtime's Runs, the Work Record and timer
;; facts on the Tape, the notices and the marks on buffers -- and nothing is
;; written or queued (docs/decisions/accepted/2026-10-11-one-list-of-what-is-pending.md).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'superchat-work)
(require 'superchat-timer)
(require 'superchat-changes)

(declare-function superchat-runtime-active-runs "superchat-runtime" ())
(declare-function superchat-runtime-status "superchat-runtime" (run-id))
(declare-function superchat-runtime-cancel "superchat-runtime" (run-id &optional response))
(declare-function superchat-runtime--terminal-p "superchat-runtime" (status))
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role &optional create))
(declare-function superchat--render-session-view "superchat" (session-id))

(defconst superchat-pending-buffer-name "*superchat pending*")

(defconst superchat-pending-unconfirmed-days 7
  "For how many days a task that may or may not have started stays listed.")

(defconst superchat-pending--kinds
  '((running     . "Running")
    (ask-user    . "Waiting for you")
    (due         . "Due, not started")
    (unconfirmed . "May have started")
    (unseen      . "Ended, not looked at")
    (unsaved     . "Unsaved")
    (continuable . "Can be continued")
    (wait-time   . "Waiting for a time")
    (wait-other  . "Waiting")
    (timer       . "Scheduled"))
  "The kinds of row, in the order they are listed, with what each is called.")

;;;; Reading -------------------------------------------------------------------

(defun superchat-pending--item (kind key what detail &optional session buffer)
  (list :kind kind :key key :what what :detail (or detail "")
        :session session :buffer buffer))

(defun superchat-pending--one-line (text)
  "TEXT on one line, or the empty string."
  (if (stringp text)
      (string-trim (replace-regexp-in-string "[\n\r\t ]+" " " text))
    ""))

(defun superchat-pending--running ()
  "The executions under way, each with who started it."
  (when (fboundp 'superchat-runtime-active-runs)
    (delq nil
          (mapcar
           (lambda (run)
             (when-let* ((status (superchat-runtime-status run))
                         ((not (superchat-runtime--terminal-p
                                (plist-get status :status))))
                         (session (plist-get status :session-id)))
               (superchat-pending--item
                'running run
                (if (superchat-changes--unattended-run-p session run)
                    (format "started by Superchat: %s"
                            (superchat-changes--notice-what session run))
                  "started by you")
                (format "execution %s" run)
                session)))
           (superchat-runtime-active-runs)))))

(defun superchat-pending--records ()
  "The open Work Records, each by how it stands."
  (let ((now (superchat-work--now)) items)
    (dolist (record (superchat-work-records))
      (let* ((id (plist-get record :id))
             (goal (superchat-pending--one-line (plist-get record :goal)))
             (waiting (plist-get record :waiting))
             (until (plist-get waiting :until))
             (session (or (plist-get (car (last (plist-get record :wakes))) :session)
                          (plist-get record :session-id))))
        (pcase (superchat-work-status record)
          ('completed nil)
          ('waiting-user
           (push (superchat-pending--item
                  'ask-user id goal
                  (superchat-pending--one-line (plist-get waiting :detail))
                  session)
                 items))
          ('waiting-other
           (cond ((and (numberp until) (superchat-work--due-p record) (<= until now))
                  (push (superchat-pending--item
                         'due id goal
                         (format "work record, due %s, not taken up"
                                 (superchat-timer-format-time until))
                         session)
                        items))
                 ((numberp until)
                  (push (superchat-pending--item
                         'wait-time id goal
                         (format "until %s" (superchat-timer-format-time until))
                         session)
                        items))
                 (t
                  (push (superchat-pending--item
                         'wait-other id goal
                         (superchat-pending--one-line
                          (format "%s %s" (or (plist-get waiting :for) "")
                                  (or (plist-get waiting :detail) "")))
                         session)
                        items))))
          (_
           (push (superchat-pending--item
                  'continuable id goal
                  (if (equal "interrupted" (car (last (plist-get record :outcomes))))
                      "its last execution was interrupted"
                    (superchat-pending--one-line (plist-get record :resume)))
                  session)
                 items)))))
    (nreverse items)))

(defun superchat-pending--tasks ()
  "Scheduled tasks that came due and have no execution on record."
  (append
   (mapcar (lambda (task)
             ;; "Being sent" is written before the send: a send that failed
             ;; leaves it behind.  Whether the task may have started is the
             ;; timer module's own question, asked the way it asks it.
             (let ((unsure (and (plist-get task :sending)
                                (superchat-timer--run-after-sending-p task))))
               (superchat-pending--item
                (if unsure 'unconfirmed 'due)
                (cons (plist-get task :id) (plist-get task :occurrence))
                (superchat-pending--one-line (plist-get task :text))
                (format (if unsure
                            "scheduled task, due %s: an execution began in its conversation while it was being sent"
                          "scheduled task, due %s, not started")
                        (superchat-timer-format-time (plist-get task :occurrence)))
                (plist-get task :session-id))))
           (superchat-timer-unstarted-tasks))
   (mapcar (lambda (task)
             (superchat-pending--item
              'unconfirmed
              (cons (plist-get task :id) (plist-get task :occurrence))
              (superchat-pending--one-line (plist-get task :text))
              (format "scheduled task, due %s: Emacs exited while it was being sent; not started again"
                      (superchat-timer-format-time (plist-get task :occurrence)))
              (plist-get task :session-id)))
           (superchat-timer-unconfirmed-tasks
            (- (superchat-timer--now)
               (* superchat-pending-unconfirmed-days 86400))))))

(defun superchat-pending--timers ()
  "The timers still to come."
  (mapcar (lambda (timer)
            (superchat-pending--item
             'timer (plist-get timer :id)
             (superchat-pending--one-line (plist-get timer :text))
             (format "%s at %s%s"
                     (plist-get timer :kind)
                     (superchat-timer-format-time (plist-get timer :next-due))
                     (if (plist-get timer :repeat) ", repeating" ""))
             (plist-get timer :session-id)))
          (superchat-timer-list)))

(defun superchat-pending--unseen ()
  "Executions Superchat started by itself that ended and were not looked at."
  (mapcar (lambda (notice)
            (superchat-pending--item
             'unseen (plist-get notice :run)
             (plist-get notice :what)
             (format "%s%s" (plist-get notice :status)
                     (let ((n (plist-get notice :unsaved)))
                       (if (and (numberp n) (> n 0))
                           (format ", %d unsaved when it ended" n)
                         "")))
             (plist-get notice :session)))
          (reverse superchat-changes--notices)))

(defun superchat-pending--unsaved ()
  "Buffers a tool edited that are still unsaved, each with whose edit it is."
  (let (items)
    (dolist (buffer (buffer-list))
      (when-let* (((buffer-modified-p buffer))
                  (marks (superchat-changes--marks buffer)))
        (push (superchat-pending--item
               'unsaved (buffer-name buffer)
               (buffer-name buffer)
               (or (and (buffer-file-name buffer)
                        (abbreviate-file-name (buffer-file-name buffer)))
                   "not visiting a file")
               (car (car (last marks)))
               buffer)
              items)))
    (nreverse items)))

(defun superchat-pending-items ()
  "Everything under way and everything waiting, as a list of plists.
Each is (:kind :key :what :detail :session :buffer).  `:kind' is one of the
symbols of `superchat-pending--kinds', and the list is in that order.  Read
from what the other modules keep; nothing is written."
  (let ((items (append (superchat-pending--running)
                       (superchat-pending--records)
                       (superchat-pending--tasks)
                       (superchat-pending--timers)
                       (superchat-pending--unseen)
                       (superchat-pending--unsaved))))
    (cl-loop for (kind . _) in superchat-pending--kinds
             append (cl-remove-if-not
                     (lambda (item) (eq kind (plist-get item :kind)))
                     items))))

;;;; The list ------------------------------------------------------------------

(defun superchat-pending--entries ()
  (mapcar (lambda (item)
            (list item
                  (vector (alist-get (plist-get item :kind) superchat-pending--kinds)
                          (plist-get item :what)
                          (plist-get item :detail)
                          (or (plist-get item :session) ""))))
          (superchat-pending-items)))

(defun superchat-pending--view (session)
  "The live View of SESSION, made from its Tape when there is none."
  (or (and (fboundp 'superchat-editor-workspace-buffer)
           (let ((buffer (superchat-editor-workspace-buffer session 'session-view)))
             (and (buffer-live-p buffer) buffer)))
      (superchat--render-session-view session)))

(defun superchat-pending-open (item)
  "Go to what the row at point is about.
An unsaved buffer is shown; anything else leads to its conversation.  An
ended execution is thereby looked at, and leaves the list."
  (interactive (list (tabulated-list-get-id)))
  (when item
    (pcase (plist-get item :kind)
      ('unsaved
       (if (buffer-live-p (plist-get item :buffer))
           (pop-to-buffer (plist-get item :buffer))
         (user-error "That buffer is gone")))
      (kind
       (let ((session (plist-get item :session)))
         (unless (stringp session)
           (user-error "This row has no conversation to go to"))
         (when (eq kind 'unseen)
           (superchat-changes-notice-dismiss (plist-get item :key)))
         (pop-to-buffer (superchat-pending--view session)))))))

(defun superchat-pending-show-record (item)
  "Show the full Work Record of the row at point."
  (interactive (list (tabulated-list-get-id)))
  (if (and item (memq (plist-get item :kind)
                      '(ask-user due wait-time wait-other continuable))
           (stringp (plist-get item :key)))
      (superchat-work-list-show (plist-get item :key))
    (user-error "The row at point is not a work record")))

(defun superchat-pending-stop (item)
  "Stop the execution of the row at point."
  (interactive (list (tabulated-list-get-id)))
  (unless (and item (eq 'running (plist-get item :kind)))
    (user-error "The row at point is not a running execution"))
  (when (yes-or-no-p (format "Stop execution %s? " (plist-get item :key)))
    (superchat-runtime-cancel (plist-get item :key))
    (superchat-pending-refresh)))

(defun superchat-pending-dismiss (item)
  "Forget the ended execution of the row at point without going to it."
  (interactive (list (tabulated-list-get-id)))
  (unless (and item (eq 'unseen (plist-get item :kind)))
    (user-error "Only an ended execution can be dismissed"))
  (superchat-changes-notice-dismiss (plist-get item :key))
  (superchat-pending-refresh))

(defun superchat-pending-refresh ()
  "Read everything again."
  (interactive)
  (when-let* ((buffer (get-buffer superchat-pending-buffer-name)))
    (with-current-buffer buffer
      (when (derived-mode-p 'superchat-pending-mode)
        (tabulated-list-print t)))))

(defvar superchat-pending-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'superchat-pending-open)
    (define-key map (kbd "v") #'superchat-pending-show-record)
    (define-key map (kbd "k") #'superchat-pending-stop)
    (define-key map (kbd "d") #'superchat-pending-dismiss)
    (define-key map (kbd "g") #'superchat-pending-refresh)
    map)
  "Keys of the pending list.")

(define-derived-mode superchat-pending-mode tabulated-list-mode "Superchat-Pending"
  "What Superchat is doing and what it waits for, in one list.
RET goes to the conversation (or the unsaved buffer), v shows a work
record, k stops a running execution, d forgets an ended one, g reads
everything again."
  (setq tabulated-list-format
        [("State" 22 nil) ("What" 44 nil) ("Detail" 52 nil) ("Conversation" 0 nil)])
  (setq tabulated-list-entries #'superchat-pending--entries)
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;###autoload
(defun superchat-pending ()
  "Show what Superchat is doing and what it is waiting for, in one list.
Executions under way and who started them; work records waiting for you or
for a time; what came due and did not start; scheduled tasks Superchat
cannot tell were started; timers still to come; work that ended unseen; and
buffers left unsaved."
  (interactive)
  (let ((buffer (get-buffer-create superchat-pending-buffer-name)))
    (with-current-buffer buffer
      (superchat-pending-mode)
      (tabulated-list-print))
    (pop-to-buffer buffer)
    (when (zerop (buffer-size buffer))
      (message "superchat: nothing is under way and nothing is waiting"))
    buffer))

(provide 'superchat-pending)
;;; superchat-pending.el ends here
