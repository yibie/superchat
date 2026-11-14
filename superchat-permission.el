;;; superchat-permission.el --- Levels and the decision for one tool call -*- lexical-binding: t; -*-

;;; Commentary:
;; A timeline's level is its latest `level' event on the Tape.  Remembering
;; "always" per tool is `superchat-tools.el's business: asking has no timeline.

;;; Code:

(require 'seq)
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
  "Called with a tool name and a summary; returns `allow', `deny' or `always'.")

(defun superchat-permission-level (tape)
  "The level of timeline TAPE: its latest level event, else the default."
  (let ((event (seq-find (lambda (e) (equal (plist-get (plist-get e :data) :type) "level"))
                         (reverse (superchat-tape-entries :tape tape :kinds '(event))))))
    (if event
        (intern (plist-get (plist-get event :data) :level))
      superchat-default-level)))

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

(defun superchat-permission-ask (tool-name summary)
  "Ask the user about TOOL-NAME doing SUMMARY: `allow', `deny' or `always'."
  (funcall superchat-permission-ask-function tool-name summary))

(provide 'superchat-permission)
;;; superchat-permission.el ends here
