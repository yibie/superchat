;;; superchat-schedule.el --- the list of what you set to run by itself -*- lexical-binding: t; -*-

;;; Commentary:
;; `M-x superchat-schedule-task': one row for each timer and rule still set --
;; what kind it is, when it runs, when next, how many times it ran, how it
;; went last time and how it stands now.  A task that runs every hour is one
;; row however many times it ran: the list says how many, so its ends do not
;; pile up like a log (the user, 2026-11-06).  Everything is read from what the
;; timer, watch and changes modules keep; the only thing it writes is a
;; cancellation, after the user said yes.
;; docs/decisions/accepted/2026-11-06-a-list-of-what-you-set-to-run.md

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'superchat-timer)

(declare-function superchat-watch-rule-words "superchat-watch" (rule))
(declare-function superchat-watch--kind "superchat-watch" (on))
(declare-function superchat-butler-text-of "superchat-butler" (run))
(declare-function superchat-butler-line-of "superchat-butler" (run))
(declare-function superchat-butler-open-line "superchat-butler" (line))
(declare-function superchat-butler-bring-from-here "superchat-butler" (line))
(declare-function superchat-changes-notice-dismiss "superchat-changes" (run))
(defvar superchat-changes--notices)
(defvar superchat-butler-session-id)

(defconst superchat-schedule-buffer-name "*superchat schedule*")

(defun superchat-schedule--one-line (text)
  (if (stringp text)
      (string-trim (replace-regexp-in-string "[\n\r\t ]+" " " text))
    ""))

(defun superchat-schedule--fired-counts ()
  "Timer id -> how many times it fired, by the Tape."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row (superchat-timer--rows))
      (cl-destructuring-bind (_session type payload) row
        (when (equal type "timer.fired")
          (let ((id (superchat-timer--payload-get payload 'timer_id 'timer-id)))
            (puthash id (1+ (gethash id table 0)) table)))))
    table))

(defun superchat-schedule--unseen (runs)
  "How many of RUNS ended and were not looked at."
  (cl-count-if (lambda (run)
                 (cl-find run (bound-and-true-p superchat-changes--notices)
                          :key (lambda (notice) (plist-get notice :run))
                          :test #'equal))
               runs))

(defun superchat-schedule--status-words (status)
  (pcase status
    ("first" "first run")
    ("unchanged" "no change")
    ("nothing" "nothing to tell")
    ((pred stringp) status)
    (_ "")))

(defun superchat-schedule--kind (timer)
  "What kind TIMER is, in a word or two."
  (cond ((plist-get timer :on)
         (if (and (fboundp 'superchat-watch--kind)
                  (superchat-watch--kind (plist-get timer :on)))
             "rule (feeds)"
           "rule (compile)"))
        ((eq 'command (plist-get timer :kind))
         (if (plist-get timer :brief) "command + brief" "command"))
        (t (symbol-name (plist-get timer :kind)))))

(defun superchat-schedule--when (timer)
  "When TIMER runs, in words."
  (cond ((plist-get timer :on)
         (if (fboundp 'superchat-watch-rule-words)
             (superchat-watch-rule-words timer)
           (plist-get timer :on)))
        ((plist-get timer :repeat)
         (superchat-timer--repeat-name (plist-get timer :repeat)))
        (t "once")))

(defun superchat-schedule-items ()
  "Each timer and rule still set, as a plist, in the order they were set.
\(:id :kind :when :next :ran :last :state :text :session :runs :timer)."
  (let ((fired (superchat-schedule--fired-counts))
        (stats (superchat-timer-command-stats))
        (waiting (mapcar (lambda (task) (plist-get task :id))
                         (superchat-timer-unstarted-tasks))))
    (mapcar
     (lambda (timer)
       (let* ((id (plist-get timer :id))
              (looks (or (plist-get timer :on) (plist-get timer :command)))
              (stat (gethash id stats))
              (runs (unless looks (superchat-timer--runs-of id)))
              (newest (car (last runs)))
              (ran (if looks (if stat (plist-get stat :runs) 0) (gethash id fired 0)))
              (unseen (superchat-schedule--unseen runs)))
         (list :id id :timer timer :session (plist-get timer :session-id) :runs runs
               :kind (superchat-schedule--kind timer)
               :when (superchat-schedule--when timer)
               :next (if (plist-get timer :next-due)
                         (superchat-timer-format-time (plist-get timer :next-due))
                       "")
               :ran ran
               :last (superchat-schedule--one-line
                      (cond (looks (and stat (> (plist-get stat :runs) 0)
                                        (superchat-schedule--status-words
                                         (plist-get stat :last-status))))
                            ((and newest (fboundp 'superchat-butler-text-of))
                             (superchat-butler-text-of newest))))
               :state (cond ((member id waiting) "due, not started")
                            ((and (plist-get timer :command)
                                  (gethash id superchat-timer--running))
                             "running")
                            ((> unseen 0) (format "%d not looked at" unseen))
                            ((superchat-timer--active-p) "set")
                            (t "set; timers are off"))
               :text (superchat-schedule--one-line (plist-get timer :text)))))
     (append (superchat-timer-list) (superchat-timer-rules)))))

(defun superchat-schedule--entries ()
  (mapcar (lambda (item)
            (list item
                  (vector (plist-get item :kind)
                          (plist-get item :when)
                          (plist-get item :next)
                          (number-to-string (plist-get item :ran))
                          (or (plist-get item :last) "")
                          (plist-get item :state)
                          (plist-get item :text))))
          (superchat-schedule-items)))

(defun superchat-schedule--header ()
  "How many are set, of which kinds."
  (let* ((items (superchat-schedule-items))
         (rules (cl-count-if (lambda (item) (plist-get (plist-get item :timer) :on)) items)))
    (format " %d set: %d timer%s, %d rule%s.  RET last result  t bring it in  k cancel  g again"
            (length items)
            (- (length items) rules) (if (= 1 (- (length items) rules)) "" "s")
            rules (if (= 1 rules) "" "s"))))

(defun superchat-schedule--item ()
  (or (tabulated-list-get-id) (user-error "There is no row here")))

(defun superchat-schedule-cancel (item)
  "Cancel the timer or rule of the row at point, once you said yes."
  (interactive (list (superchat-schedule--item)))
  (when (yes-or-no-p (format "Cancel \"%s\"? " (plist-get item :text)))
    (superchat-timer-cancel (plist-get item :id))
    (superchat-schedule-refresh)))

(defun superchat-schedule--last-line (item)
  "The butler's line of ITEM's last result, or nil."
  (when (fboundp 'superchat-butler-line-of)
    (let ((run (if (or (plist-get (plist-get item :timer) :on)
                       (plist-get (plist-get item :timer) :command))
                   (superchat-timer-last-result-run (plist-get item :id))
                 (car (last (plist-get item :runs))))))
      (and run (superchat-butler-line-of run)))))

(defun superchat-schedule-open (item)
  "Show the last result of the row at point."
  (interactive (list (superchat-schedule--item)))
  (let ((line (superchat-schedule--last-line item)))
    (unless line (user-error "It has not run yet"))
    (superchat-butler-open-line line)
    ;; Looked at: its ends leave the list of what is left to look at.
    (when (fboundp 'superchat-changes-notice-dismiss)
      (dolist (run (plist-get item :runs))
        (superchat-changes-notice-dismiss run)))))

(defun superchat-schedule-bring (item)
  "Put the last result of the row at point in the message you are writing."
  (interactive (list (superchat-schedule--item)))
  (let ((line (superchat-schedule--last-line item)))
    (unless line (user-error "It has not run yet"))
    (superchat-butler-bring-from-here line)))

(defun superchat-schedule-refresh ()
  "Read everything again."
  (interactive)
  (when-let* ((buffer (get-buffer superchat-schedule-buffer-name)))
    (with-current-buffer buffer
      (setq header-line-format (superchat-schedule--header))
      (tabulated-list-print t))))

(defvar superchat-schedule-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'superchat-schedule-open)
    (define-key map (kbd "t") #'superchat-schedule-bring)
    (define-key map (kbd "k") #'superchat-schedule-cancel)
    (define-key map (kbd "g") #'superchat-schedule-refresh)
    map))

(define-derived-mode superchat-schedule-mode tabulated-list-mode "Superchat-Schedule"
  "What you set to run by itself, one row each."
  (setq tabulated-list-format
        [("Kind" 15 t) ("When" 30 t) ("Next" 17 t) ("Ran" 5 t :right-align t)
         ("Last" 30 nil) ("State" 18 t) ("What" 40 nil)])
  (setq tabulated-list-entries #'superchat-schedule--entries)
  (tabulated-list-init-header)
  (setq header-line-format (superchat-schedule--header)))

;;;###autoload
(defun superchat-schedule-task ()
  "List what you set to run by itself: timers, commands and rules.
One row each, with how many times it ran and how it went last time."
  (interactive)
  (let ((buffer (get-buffer-create superchat-schedule-buffer-name)))
    (with-current-buffer buffer
      (superchat-schedule-mode)
      (tabulated-list-print))
    (pop-to-buffer buffer)
    (when (zerop (length (superchat-schedule-items)))
      (message "superchat: nothing is set to run by itself"))
    buffer))

(provide 'superchat-schedule)
;;; superchat-schedule.el ends here
