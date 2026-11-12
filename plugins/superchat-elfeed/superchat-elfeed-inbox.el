;;; superchat-elfeed-inbox.el --- what comes into elfeed goes into the inbox -*- lexical-binding: t; -*-

;;; Commentary:
;; docs/decisions/accepted/2026-11-03-an-inbox-that-keeps-what-came-in.md
;;
;; Loaded by `superchat-inbox' once elfeed is, with or without Superchat.  It
;; keeps only which entry came in; what reads it reads the entry from elfeed.

;;; Code:

(require 'superchat-inbox)

(defvar elfeed-new-entry-hook)
(declare-function elfeed-entry-id "elfeed-db" (entry))

(defun superchat-elfeed-inbox--on-new-entry (entry)
  "ENTRY is new to elfeed: `elfeed-new-entry-hook'.  Nothing here may break elfeed.
elfeed calls this inside its parsing, for every new entry of every feed."
  (condition-case err
      (let ((id (elfeed-entry-id entry)))
        (superchat-inbox-push "feed_new" "elfeed"
                              (if (consp id) (list (car id) (cdr id)) (list id))))
    (error (message "superchat: the inbox missed an elfeed entry: %s"
                    (error-message-string err)))))

(add-hook 'elfeed-new-entry-hook #'superchat-elfeed-inbox--on-new-entry)

(provide 'superchat-elfeed-inbox)
;;; superchat-elfeed-inbox.el ends here
