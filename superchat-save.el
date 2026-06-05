;;; superchat-save.el --- Conversation save for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Save conversations to Org files.  Extracted from superchat.el monolith
;; (v0.9 split step 2).

;;; Code:

(require 'cl-lib)

;; ── Forward declarations (owned by superchat.el) ──
(declare-function superchat--save-directory "superchat" ())
(declare-function superchat--ensure-directories "superchat" ())

(defcustom superchat-default-save-method 'ask
  "Default save method for conversations."
  :type '(choice (const :tag "Ask each time" ask)
                 (const :tag "New file" new-file)
                 (const :tag "Append to node" append-node)
                 (const :tag "New subnode" new-node)
                 (const :tag "New heading" new-heading))
  :group 'superchat)

(defun superchat--format-conversation (conversation)
  "Format conversation for org-mode display."
  (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun superchat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (when (and conversation title (stringp conversation) (stringp title))
    (superchat--ensure-directories)
    (let ((save-dir (superchat--save-directory))
          (filename (expand-file-name (concat title ".org") (superchat--save-directory))))
      (make-directory save-dir t)
      (with-current-buffer (find-file-noselect filename)
        (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n" 
                        title (format-time-string "%Y-%m-%d")))
        (insert "* AI Conversation\n\n")
        (insert (superchat--format-conversation conversation))
        (save-buffer)
        ;;(message "Conversation saved to: %s" filename)
        ))))

(defun superchat--save-append-to-node (conversation)
  "Append conversation to current org headline."
  (when (and conversation (stringp conversation) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n\n* AI Assistant Conversation\n")
      (insert (format "#+CAPTION: Generated %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (superchat--format-conversation conversation))
      ;;(message "Conversation appended to current node")
      )))

(defun superchat--save-as-subnode (conversation title)
  "Create new subnode under current headline."
  (when (and conversation title (stringp conversation) (stringp title) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n")
      (org-insert-heading)
      (insert title)
      (org-set-property "DATE" (format-time-string "%Y-%m-%d"))
      (org-set-property "TAGS" "ai-conversation")
      (insert "\n")
      (insert (superchat--format-conversation conversation))
      ;;(message "Conversation saved as subnode: %s" title)
      )))

;;;###autoload
(defun superchat--save-conversation ()
  "Save Chat View conversation with user choice of method."
  (interactive)
  (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
    (when (and conversation (stringp conversation))
      (let* ((title (read-string "Conversation title: " (format "Chat-%s" (format-time-string "%Y%m%d"))))
             (choice (completing-read "Save conversation as: "
                                      '("new file" "append to current node" "create new subnode")
                                      nil t)))
        (pcase choice
          ("new file" (superchat--save-as-new-file conversation title))
          ("append to current node" (superchat--save-append-to-node conversation))
          ("create new subnode" (superchat--save-as-subnode conversation title)))))))


(provide 'superchat-save)
;;; superchat-save.el ends here
