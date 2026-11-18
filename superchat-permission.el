;;; superchat-permission.el --- Levels and the decision for one tool call -*- lexical-binding: t; -*-

;;; Commentary:
;; A timeline's level is its latest `level' event on the Tape.  Remembering
;; "always" per tool is `superchat-tools.el's business: asking has no timeline.

;;; Code:

(require 'superchat)
(require 'superchat-tape)

(defconst superchat-permission--table
  '((read-only (read . allow) (write . deny) (exec . deny))
    (ask (read . allow) (write . ask) (exec . ask))
    (auto (read . allow) (write . allow) (exec . allow)))
  "Level, then class, to decision.")

(defun superchat-permission--read-key (tool-name summary)
  "Ask in the minibuffer whether TOOL-NAME may do SUMMARY."
  (pcase (read-char-choice
          (format "Superchat: %s %s  [y]es [n]o [a]lways in this conversation: "
                  tool-name summary)
          '(?y ?n ?a))
    (?y 'allow) (?n 'deny) (?a 'always)))

(defvar superchat-permission-ask-function #'superchat-permission--read-key
  "Called with a tool name and a summary; returns `allow', `deny' or `always',
or `later' when nobody is there to answer now.")

(defun superchat-permission-level (tape)
  "The level of timeline TAPE: its latest level event, else the default.
One query for the newest, because every tool call asks."
  (if-let* ((level (caar (superchat-tape-select
                          (concat "SELECT json_extract(data, '$.level') FROM entry"
                                  " WHERE tape = ? AND kind = 'event'"
                                  " AND json_extract(data, '$.type') = 'level'"
                                  " ORDER BY id DESC LIMIT 1")
                          (list tape)))))
      (intern level)
    superchat-default-level))

(defun superchat-permission-set (tape level)
  "Append the event that makes LEVEL the level of timeline TAPE."
  (unless (assq level superchat-permission--table)
    (error "Unknown permission level: %S" level))
  (superchat-tape-append tape 'event (format "Permission level: %s" level)
                         :data (list :type "level" :level (symbol-name level))))

(defun superchat-permission-decide (tape class)
  "`allow', `deny' or `ask' for a tool of CLASS in timeline TAPE."
  (alist-get class (alist-get (superchat-permission-level tape)
                              superchat-permission--table)))

(defvar superchat-permission-asking nil
  "While the user is asked: (:tape TAPE :tool NAME :summary SUMMARY).
A run waiting for an answer looks like a slow tool unless the display can
tell the two apart.")

(defun superchat-permission-ask (tool-name summary &optional tape)
  "Ask the user about TOOL-NAME doing SUMMARY: `allow', `deny', `always' or
`later'.  TAPE is the timeline the call belongs to."
  (let ((superchat-permission-asking (list :tape tape :tool tool-name :summary summary)))
    (funcall superchat-permission-ask-function tool-name summary)))

(defun superchat-permission-answer (tape call-id answer)
  "Append the person's ANSWER (`allow', `deny' or `always') to the question
about call CALL-ID of timeline TAPE."
  (unless (memq answer '(allow deny always))
    (error "An answer is allow, deny or always, not %S" answer))
  (superchat-tape-append tape 'event (format "Answered: %s" answer)
                         :data (list :type "answer" :call-id call-id
                                     :answer (symbol-name answer))))

(defun superchat-permission-answered (tape call-id)
  "The answer the person gave to the question about call CALL-ID of
timeline TAPE, or nil."
  (when-let* ((answer (caar (superchat-tape-select
                             (concat "SELECT json_extract(data, '$.answer') FROM entry"
                                     " WHERE tape = ? AND kind = 'event'"
                                     " AND json_extract(data, '$.type') = 'answer'"
                                     " AND json_extract(data, '$.call-id') = ?"
                                     " ORDER BY id DESC LIMIT 1")
                             (list tape call-id)))))
    (intern answer)))

(provide 'superchat-permission)
;;; superchat-permission.el ends here
