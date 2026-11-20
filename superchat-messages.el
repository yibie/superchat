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
  "y" #'superchat-messages-answer
  "n" #'superchat-messages-answer
  "a" #'superchat-messages-answer
  "g" #'superchat-messages)

(define-derived-mode superchat-messages-mode special-mode "Superchat-Messages"
  "What runs sent to the person, newest last.  RET opens its conversation."
  (setq header-line-format
        (substitute-command-keys
         " \\<superchat-messages-mode-map>\\[superchat-messages-open] open conversation   y n a answer a question   \\[superchat-messages] refresh   \\[quit-window] quit")))

;;;###autoload
(defun superchat-messages ()
  "Show what runs sent with the `send' tool and the questions that wait for
the person, newest last."
  (interactive)
  (let ((sent (seq-filter
               (pcase-lambda (`(,id ,tape ,_ ,_ ,type))
                 (or (equal type "sent")
                     (equal (plist-get (superchat-question tape) :id) id)))
               (superchat-tape-select
                (concat "SELECT id, tape, text, at, json_extract(data, '$.type') FROM entry"
                        " WHERE kind = 'event' AND json_extract(data, '$.type')"
                        " IN ('sent', 'question') ORDER BY id")))))
    (with-current-buffer (get-buffer-create "*superchat: messages*")
      (let ((inhibit-read-only t))
        (superchat-messages-mode)
        (erase-buffer)
        (pcase-dolist (`(,_ ,tape ,text ,at ,type) sent)
          (insert (propertize (format "%s  %s  %s%s\n" (format-time-string "%F %R" at) tape
                                      (car (split-string text "\n"))
                                      (if (equal type "question")
                                          (concat "  ? waits for " (superchat-chat-answer-keys))
                                        ""))
                              'superchat-messages-tape tape
                              'superchat-messages-question (equal type "question"))))
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

(defun superchat-messages-answer ()
  "Answer the question at point with the key typed, y, n or a, as its chat
buffer would, and show that buffer."
  (interactive)
  (unless (get-text-property (point) 'superchat-messages-question)
    (user-error "No question on this line"))
  (let ((chat (superchat-chat-open (get-text-property (point) 'superchat-messages-tape))))
    (with-current-buffer chat
      (superchat-chat-send (string last-command-event)))
    (superchat-messages)
    (pop-to-buffer chat)))

(provide 'superchat-messages)
;;; superchat-messages.el ends here
