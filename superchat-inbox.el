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
(require 'superchat-agent)

(defvar superchat-inbox--timer nil "The timer of the worker's next look, or nil.")

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
          (superchat-run tape (mapconcat (lambda (e) (plist-get e :text)) items "\n\n")
                         :refs (mapcar (lambda (e) (plist-get e :id)) items)
                         :unattended (seq-some (lambda (e) (plist-get (plist-get e :data)
                                                                      :unattended))
                                               items)
                         :on-done (lambda (_) (superchat-inbox-wake)))
        (error (message "Superchat inbox: %s: %s" tape (error-message-string err)))))))

(provide 'superchat-inbox)
;;; superchat-inbox.el ends here
