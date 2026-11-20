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

(defvar superchat-inbox-notify-functions nil
  "Functions called with TAPE, TEXT and KIND, `sent' after the `send' tool or
`question' when a run of the worker waits for an answer: a system notification.")

(defun superchat-inbox--notify (tape text kind)
  "Show TEXT of KIND from timeline TAPE and run `superchat-inbox-notify-functions'.
A function that fails is shown and does not stop the others or the run."
  (let ((line (car (split-string text "\n"))))
    (message "Superchat: %s" (if (eq kind 'question)
                                 (format "%s waits for your answer: %s" tape line)
                               line)))
  (dolist (f superchat-inbox-notify-functions)
    (condition-case err (funcall f tape text kind)
      (error (message "Superchat: notify %s failed: %s" f (error-message-string err))))))

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

;; A run at the round limit pauses while items wait for its timeline: they
;; drive the next run, not its look-back.
(add-hook 'superchat-agent-pending-functions #'superchat-inbox-pending)

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

(defun superchat-inbox--text (items)
  "The text of a run for ITEMS, oldest first: one item's own text, or each
marked with its place, so the model can tell how many messages came."
  (if (cdr items)
      (let ((n (length items)) (i 0))
        (mapconcat (lambda (e) (format "[%d/%d] %s" (cl-incf i) n (plist-get e :text)))
                   items "\n\n"))
    (plist-get (car items) :text)))

(defun superchat-inbox--work ()
  "Start one run for each timeline with pending items that is free: no run,
no handoff, no question waiting.  Its text is the items', oldest first, marked
with each one's place when there is more than one."
  (setq superchat-inbox--timer nil)
  (pcase-dolist (`(,tape . ,items) (seq-group-by (lambda (e) (plist-get e :tape))
                                                 (superchat-inbox-pending)))
    (unless (or (superchat-run-active tape) (superchat-question tape))
      ;; One timeline that cannot start must not keep the others waiting;
      ;; its items stay pending and the person is told.
      (condition-case err
          (let ((unattended (seq-some (lambda (e) (plist-get (plist-get e :data) :unattended))
                                      items)))
            (superchat-run tape (superchat-inbox--text items)
                           :refs (mapcar (lambda (e) (plist-get e :id)) items)
                           :unattended unattended
                           :system (and unattended superchat-inbox--quiet)
                           :on-done (lambda (run)
                                      (when (eq (plist-get run :status) 'waiting)
                                        (superchat-inbox--notify
                                         tape (plist-get (superchat-question tape) :text)
                                         'question))
                                      (superchat-inbox-wake))))
        (error (message "Superchat inbox: %s: %s" tape (error-message-string err)))))))

;;; Sending

(superchat-tool-define
 "send" :class 'read
 :description "Tell the person something now: a line they see even when they are not reading this conversation. Only in a run the system prompt says nobody watches, and only what they need to act on or asked for; when the person reads the conversation, answer there."
 :args '((:name "text" :type string :description "The message" :required t))
 :function (lambda (args)
             (let ((text (plist-get args :text)))
               (superchat-tape-append superchat-tool-tape 'event text :data '(:type "sent"))
               (superchat-inbox--notify superchat-tool-tape text 'sent)
               "Sent.")))

(provide 'superchat-inbox)
;;; superchat-inbox.el ends here
