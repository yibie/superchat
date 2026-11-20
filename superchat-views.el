;;; superchat-views.el --- Buffers that browse the Tape -*- lexical-binding: t; -*-

;;; Commentary:
;; The timers that may still fire, and lists of entries (notes, a search, a
;; topic), each in a read-only buffer whose keys act on the line at point.
;; A view opens a conversation through `superchat-views-open-function',
;; which the chat sets, so this file does not need the chat.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)
(require 'superchat)
(require 'superchat-view)
(require 'superchat-timer)

(defvar superchat-views-open-function #'ignore
  "Called with a timeline name to show its conversation; the chat sets it.")

;;; Entries

(defvar-keymap superchat-entries-mode-map
  :parent special-mode-map
  "RET" #'superchat-entries-open
  "n" #'superchat-entries-next
  "p" #'superchat-entries-previous
  "g" #'superchat-entries-refresh)

(define-derived-mode superchat-entries-mode special-mode "Superchat-Entries"
  "Entries of the Tape.  RET opens the conversation of the entry at point,
n and p go to the next and previous entry, g shows them anew.")

(defvar-local superchat-entries--view nil "The function that gives the entries.")

(defun superchat-entries (title view)
  "Show the entries VIEW returns, called with no argument, in a buffer named
after TITLE, an entry a block that knows its entry."
  (with-current-buffer (get-buffer-create (format "*superchat-view: %s*" title))
    (let ((inhibit-read-only t) (entries (funcall view)))
      (superchat-entries-mode)
      (setq superchat-entries--view view)
      (erase-buffer)
      (dolist (e entries)
        (unless (bobp) (insert "\n\n"))
        (let ((start (point)))
          (insert (propertize (superchat-view-render (list e)) 'superchat-entry e))
          (put-text-property start (1+ start) 'superchat-entry-start t)))
      (unless entries (insert "No entries."))
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(defun superchat-entries-open ()
  "Open the conversation of the entry at point."
  (interactive)
  (funcall superchat-views-open-function
           (plist-get (or (get-text-property (point) 'superchat-entry)
                          (user-error "No entry here"))
                      :tape)))

(defun superchat-entries-next ()
  "Go to the next entry."
  (interactive)
  (goto-char (prop-match-beginning
              (or (save-excursion (end-of-line)
                                  (text-property-search-forward 'superchat-entry-start t t))
                  (user-error "No later entry")))))

(defun superchat-entries-previous ()
  "Go to the previous entry."
  (interactive)
  (goto-char (prop-match-beginning
              (or (save-excursion (beginning-of-line)
                                  (text-property-search-backward 'superchat-entry-start t t))
                  (user-error "No earlier entry")))))

(defun superchat-entries-refresh ()
  "Show the entries anew."
  (interactive)
  (superchat-entries (string-remove-suffix "*" (string-remove-prefix "*superchat-view: " (buffer-name)))
                                superchat-entries--view))

;;; Timers

(defvar-keymap superchat-timers-mode-map
  :parent special-mode-map
  "c" #'superchat-timers-cancel
  "RET" #'superchat-timers-open
  "g" #'superchat-timers)

(define-derived-mode superchat-timers-mode special-mode "Superchat-Timers"
  "The timers that may still fire.  c cancels one, RET opens its
conversation, g shows them anew.")

(defun superchat-timers ()
  "Show the timers that may still fire, one a line."
  (interactive)
  (with-current-buffer (get-buffer-create "*superchat: timers*")
    (let ((inhibit-read-only t) (line (line-number-at-pos)))
      (superchat-timers-mode)
      (erase-buffer)
      (dolist (e (superchat-timer-list))
        (let ((data (plist-get e :data)))
          (insert (propertize
                   (format "%d  %s  %s%s  %s\n" (plist-get e :id) (plist-get e :tape)
                           (string-trim (format "%S" (cl-loop for (k v) on data by #'cddr
                                                              unless (eq k :until)
                                                              append (list k v)))
                                        "(" ")")
                           (if-let* ((until (plist-get data :until)))
                               (format-time-string "  until %F %R" until)
                             "")
                           (car (split-string (plist-get e :text) "\n")))
                   'superchat-timers-timer e))))
      (if (= (buffer-size) 0) (insert "No timers.\n") (goto-char (point-min)) (forward-line (1- line))))
    (pop-to-buffer (current-buffer))))

(defun superchat-timers--at-point ()
  "The timer on this line."
  (or (get-text-property (line-beginning-position) 'superchat-timers-timer)
      (user-error "No timer on this line")))

(defun superchat-timers-cancel ()
  "Cancel the timer on this line."
  (interactive)
  (message "%s" (superchat-timer-cancel (plist-get (superchat-timers--at-point) :id)))
  (superchat-timers))

(defun superchat-timers-open ()
  "Open the conversation of the timer on this line."
  (interactive)
  (funcall superchat-views-open-function (plist-get (superchat-timers--at-point) :tape)))

(provide 'superchat-views)
;;; superchat-views.el ends here
