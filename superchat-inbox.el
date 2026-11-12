;;; superchat-inbox.el --- what came in, kept until it is read -*- lexical-binding: t; -*-

;;; Commentary:
;; docs/decisions/accepted/2026-11-03-an-inbox-that-keeps-what-came-in.md
;;
;; A package says something came in: `superchat-inbox-push'.  The inbox writes it
;; on the Tape first, so it is kept when nothing reads it yet, when Superchat is
;; not loaded, or when Emacs exits before it is read.  Whatever reads a kind
;; registers with `superchat-inbox-handle' and takes what is pending with
;; `superchat-inbox-pending'; once it has done with it, `superchat-inbox-done'
;; moves the kind's cursor past it.
;;
;; This file is small on purpose: the person's Emacs loads it at startup, and it
;; needs only the Tape.  When something comes in that a rule is set for and
;; nothing reads it, it loads Superchat when Emacs is idle.
;;
;; An event holds what names the thing, never the thing: an elfeed entry is its
;; feed and its id, and whoever reads it reads it from elfeed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'superchat-tape)

(defgroup superchat-inbox nil
  "What came in, kept until it is read."
  :group 'superchat)

(defcustom superchat-inbox-wake t
  "Non-nil: load Superchat when something came in that a rule is set for.
It is loaded when Emacs has been idle for `superchat-inbox-wake-seconds'."
  :type 'boolean)

(defcustom superchat-inbox-wake-seconds 2
  "How long Emacs is idle before the inbox loads Superchat."
  :type 'number)

(defcustom superchat-inbox-sources
  '((elfeed . "plugins/superchat-elfeed/superchat-elfeed-inbox"))
  "Packages that say what comes in, and the file that listens to each.
Each file is loaded once its package is."
  :type '(alist :key-type symbol :value-type string))

(defvar superchat-inbox--handlers nil
  "What reads each kind: an alist of (KIND . FUNCTION).
FUNCTION is called with no argument when KIND has something pending.")

(defvar superchat-inbox--cursors (make-hash-table :test #'equal)
  "For each database and kind, the storage id of the last event done with.")

(defvar superchat-inbox--wake-timer nil
  "The idle timer that loads Superchat.")

(defun superchat-inbox--meta (text)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (ignore-errors (json-read-from-string (or text "")))))

(defun superchat-inbox--cursor (kind)
  "The storage id of the last event of KIND done with, or 0."
  (let ((key (list (superchat-db--path) kind)))
    (or (gethash key superchat-inbox--cursors)
      (puthash key
               (let ((through 0))
                 (dolist (row (superchat-db-tape-select
                               "SELECT meta FROM tape
                                WHERE kind = 'system'
                                  AND meta LIKE '%\"event_type\":\"inbox.done\"%'"))
                   (let ((payload (alist-get 'payload (superchat-inbox--meta (car row)))))
                     (when (equal kind (alist-get 'kind payload))
                       (setq through (max through (or (alist-get 'through payload) 0))))))
                 through)
               superchat-inbox--cursors))))

(defun superchat-inbox-pending (kind)
  "What came in of KIND and is not done with, oldest first.
Each is a plist (:storage-id ID :source SOURCE :key KEY :at RECORDED-AT)."
  (let (out)
    (dolist (row (superchat-db-tape-select
                  "SELECT id, meta FROM tape
                   WHERE id > ? AND kind = 'system'
                     AND meta LIKE '%\"event_type\":\"inbox.event\"%'
                   ORDER BY id ASC"
                  (list (superchat-inbox--cursor kind))))
      (let* ((meta (superchat-inbox--meta (nth 1 row)))
             (payload (alist-get 'payload meta)))
        (when (equal kind (alist-get 'kind payload))
          (push (list :storage-id (nth 0 row)
                      :source (alist-get 'source payload)
                      :key (alist-get 'key payload)
                      :at (alist-get 'recorded_at meta))
                out))))
    (nreverse out)))

(defun superchat-inbox-done (kind through &optional why)
  "Done with what came in of KIND up to the storage id THROUGH.
WHY, a string, says why when it was not read."
  (when (> through (superchat-inbox--cursor kind))
    (superchat-tape-append-event
     (list :type "inbox.done" :session-id "butler"
           :payload (append (list :kind kind :through through)
                            (when why (list :why why)))))
    (puthash (list (superchat-db--path) kind) through superchat-inbox--cursors)))

(defvar superchat-inbox--rule-seen nil
  "What `superchat-inbox--rule-on-p' found lately: (TIME KIND . FOUND).")

(defun superchat-inbox--rule-on-p (kind)
  "Non-nil when a rule that starts on KIND is set and not cancelled.
Read from the Tape, so it holds when Superchat is not loaded; what it found is
kept for a minute, for an event comes in for each entry of a fetch."
  (if (and superchat-inbox--rule-seen
           (equal kind (nth 1 superchat-inbox--rule-seen))
           (< (- (float-time) (nth 0 superchat-inbox--rule-seen)) 60))
      (nthcdr 2 superchat-inbox--rule-seen)
    (let ((found (superchat-inbox--rule-on-tape-p kind)))
      (setq superchat-inbox--rule-seen (cons (float-time) (cons kind found)))
      found)))

(defun superchat-inbox--rule-on-tape-p (kind)
  "Non-nil when the Tape holds a rule on KIND that was not cancelled."
  (let ((made nil) (cancelled nil))
    (dolist (row (superchat-db-tape-select
                  "SELECT meta FROM tape
                   WHERE kind = 'system'
                     AND (meta LIKE '%\"event_type\":\"timer.created\"%'
                          OR meta LIKE '%\"event_type\":\"timer.cancelled\"%')"))
      (let* ((meta (superchat-inbox--meta (car row)))
             (payload (alist-get 'payload meta))
             (id (or (alist-get 'timer_id payload) (alist-get 'timer-id payload))))
        (if (equal "timer.cancelled" (alist-get 'event_type meta))
            (push id cancelled)
          (when (equal kind (alist-get 'on payload))
            (push id made)))))
    (and (cl-some (lambda (id) (not (member id cancelled))) made) t)))

(defun superchat-inbox--loaded-p ()
  "Non-nil when Superchat is loaded."
  (featurep 'superchat))

(defun superchat-inbox--wake ()
  "Load Superchat, without a word: what reads the inbox comes with it."
  (setq superchat-inbox--wake-timer nil)
  (unless (superchat-inbox--loaded-p)
    (let ((inhibit-message t))
      (condition-case err
          (require 'superchat)
        (error (message "superchat: the inbox could not load Superchat: %s"
                        (error-message-string err)))))))

(defun superchat-inbox--arrived (kind)
  "Something of KIND came in: tell what reads it, or wake Superchat."
  (if-let* ((handler (alist-get kind superchat-inbox--handlers nil nil #'equal)))
      (funcall handler)
    (when (and superchat-inbox-wake
               (not (timerp superchat-inbox--wake-timer))
               (superchat-inbox--rule-on-p kind))
      (setq superchat-inbox--wake-timer
            (run-with-idle-timer superchat-inbox-wake-seconds nil #'superchat-inbox--wake)))))

(defun superchat-inbox-push (kind source key)
  "Keep that something of KIND came in from SOURCE, named by KEY.
KIND and SOURCE are strings; KEY is a string or a list of strings that lets
SOURCE find it again.  Nothing here may break the caller."
  (condition-case err
      (progn
        (superchat-tape-append-event
         (list :type "inbox.event" :session-id "butler"
               :payload (list :kind kind :source source :key key)))
        (superchat-inbox--arrived kind))
    (error (message "superchat: the inbox could not keep what came in: %s"
                    (error-message-string err)))))

(defun superchat-inbox-handle (kind function)
  "Have FUNCTION read what comes in of KIND.
When something is pending already, FUNCTION is called soon."
  (setf (alist-get kind superchat-inbox--handlers nil nil #'equal) function)
  (run-at-time 0 nil (lambda ()
                       (when (and (eq function (alist-get kind superchat-inbox--handlers
                                                          nil nil #'equal))
                                  (superchat-inbox-pending kind))
                         (funcall function)))))

(defun superchat-inbox-unhandle (kind)
  "Nothing reads what comes in of KIND any more; the inbox keeps it."
  (setq superchat-inbox--handlers
        (cl-remove kind superchat-inbox--handlers :key #'car :test #'equal)))

(defun superchat-inbox--forget ()
  "Read the cursors and the rules from the Tape again: another Tape, or a test."
  (clrhash superchat-inbox--cursors)
  (setq superchat-inbox--rule-seen nil))

(provide 'superchat-inbox)

;; After `provide': a file that listens requires this one.
(dolist (source superchat-inbox-sources)
  (with-eval-after-load (car source)
    (require (intern (file-name-nondirectory (cdr source))) (cdr source) t)))

;;; superchat-inbox.el ends here
