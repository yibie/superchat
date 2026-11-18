;;; superchat-messages.el --- The buffer of what runs sent -*- lexical-binding: t; -*-

;;; Commentary:
;; A run nobody watches tells the person only what it sends with the `send'
;; tool.  This buffer gathers those messages from every timeline, so the
;; person reads them in one place and goes to the conversation from there.

;;; Code:

(require 'superchat)
(require 'superchat-tape)
(require 'superchat-chat)

(defvar-keymap superchat-messages-mode-map
  :parent special-mode-map
  "RET" #'superchat-messages-open
  "g" #'superchat-messages)

(define-derived-mode superchat-messages-mode special-mode "Superchat-Messages"
  "What runs sent to the person, newest last.  RET opens its conversation.")

(defun superchat-messages ()
  "Show what runs sent with the `send' tool, newest last."
  (interactive)
  (let ((sent (superchat-tape-select
               (concat "SELECT tape, text, at FROM entry WHERE kind = 'event'"
                       " AND json_extract(data, '$.type') = 'sent' ORDER BY id"))))
    (with-current-buffer (get-buffer-create "*superchat: messages*")
      (let ((inhibit-read-only t))
        (superchat-messages-mode)
        (erase-buffer)
        (pcase-dolist (`(,tape ,text ,at) sent)
          (insert (propertize (format "%s  %s  %s\n" (format-time-string "%F %R" at) tape
                                      (car (split-string text "\n")))
                              'superchat-messages-tape tape)))
        (unless sent (insert "Nothing was sent.\n")))
      (goto-char (point-max))
      (forward-line -1)
      (pop-to-buffer (current-buffer)))))

(defun superchat-messages-open ()
  "Open the conversation of the message at point."
  (interactive)
  (pop-to-buffer (superchat-chat-open
                  (or (get-text-property (point) 'superchat-messages-tape)
                      (user-error "No message on this line")))))

(provide 'superchat-messages)
;;; superchat-messages.el ends here
