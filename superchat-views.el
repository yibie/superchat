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
n and p go to the next and previous entry, g shows them anew."
  (setq header-line-format
        (substitute-command-keys
         " \\<superchat-entries-mode-map>\\[superchat-entries-open] open conversation   \\[superchat-entries-next] next   \\[superchat-entries-previous] previous   \\[superchat-entries-refresh] refresh   \\[quit-window] quit")))

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
  :parent tabulated-list-mode-map
  "c" #'superchat-timers-cancel
  "RET" #'superchat-timers-open)

(define-derived-mode superchat-timers-mode tabulated-list-mode "Superchat-Timers"
  "The timers that may still fire.  c cancels one, RET opens its
conversation, g shows them anew, S sorts by the column at point."
  (setq tabulated-list-format [("Next" 16 t) ("Repeats" 20 t) ("Project" 16 t)
                               ("Task" 0 nil)]
        tabulated-list-sort-key '("Next")
        tabulated-list-padding 2
        tabulated-list-use-header-line nil
        header-line-format
        (substitute-command-keys
         " \\<superchat-timers-mode-map>\\[superchat-timers-cancel] cancel   \\[superchat-timers-open] open conversation   \\[revert-buffer] refresh   \\[tabulated-list-sort] sort by column   \\[quit-window] quit"))
  (add-hook 'tabulated-list-revert-hook #'superchat-timers--entries nil t)
  (tabulated-list-init-header))

(defun superchat-timers--next (data set)
  "When a timer of DATA, set at SET, fires next, as a time."
  (let ((now (float-time)))
    (cond ((plist-get data :at))
          ((plist-get data :every)
           (let ((from (or (plist-get data :from) set)) (every (plist-get data :every)))
             (if (> from now) from (+ from (* every (ceiling (- now from) every))))))
          ((plist-get data :daily)
           (pcase-let* ((`(,h ,m) (mapcar #'string-to-number
                                          (split-string (plist-get data :daily) ":")))
                        (today (decode-time now)))
             (let ((at (float-time (encode-time (list 0 m h (nth 3 today) (nth 4 today)
                                                      (nth 5 today) nil -1 nil)))))
               (if (> at now) at (+ at 86400))))))))

(defun superchat-timers--repeats (data)
  "How a timer of DATA repeats, in words."
  (cond ((plist-get data :dream) (format "dream %s" (plist-get data :daily)))
        ((plist-get data :daily) (format "daily %s" (plist-get data :daily)))
        ((plist-get data :every)
         (let ((s (plist-get data :every)))
           (cond ((zerop (% s 86400)) (format "every %dd" (/ s 86400)))
                 ((zerop (% s 3600)) (format "every %dh" (/ s 3600)))
                 (t (format "every %dm" (/ s 60))))))
        (t "once")))

(defun superchat-timers--entries ()
  "Fill `tabulated-list-entries' with the timers that may still fire."
  (setq tabulated-list-entries
        (mapcar (lambda (e)
                  (let* ((data (plist-get e :data))
                         (next (superchat-timers--next data (plist-get e :at)))
                         (scope (plist-get e :scope)))
                    (list e (vector
                             (propertize (format-time-string "%m-%d %a %H:%M" next)
                                         'face 'bold)
                             (concat (superchat-timers--repeats data)
                                     (if (plist-get data :until)
                                         (format-time-string " ~%m-%d" (plist-get data :until))
                                       ""))
                             (propertize (if (string-prefix-p "/" scope)
                                             (file-name-nondirectory (directory-file-name scope))
                                           scope)
                                         'face 'font-lock-constant-face)
                             (car (split-string (plist-get e :text) "\n"))))))
                (superchat-timer-list))))

;; Loaded through the chat, which sets `superchat-views-open-function'.
;;;###autoload (autoload 'superchat-timers "superchat-chat" nil t)
(defun superchat-timers ()
  "Show the timers that may still fire as a table, the soonest first."
  (interactive)
  (with-current-buffer (get-buffer-create "*superchat: timers*")
    (unless (derived-mode-p 'superchat-timers-mode) (superchat-timers-mode))
    (superchat-timers--entries)
    (tabulated-list-print t)
    (pop-to-buffer (current-buffer))))

(defun superchat-timers--at-point ()
  "The timer on this line."
  (or (tabulated-list-get-id) (user-error "No timer on this line")))

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
