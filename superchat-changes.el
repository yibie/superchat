;;; superchat-changes.el --- Unsaved buffers a Session's tools edited -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Buffer-first mutation: the model's ordinary edits end in a buffer, and
;; saving is the user's.  This module is the outlet that makes those edits
;; visible when the user is not looking at the buffer.  It records nothing of
;; its own.  The candidates are derived each time they are asked for:
;;
;;   targets of the Session's (or one Run's) structured buffer-editing tool
;;   calls, read from their `tool.completed' facts on the Tape
;;       ∩
;;   buffers that are live and `buffer-modified-p' right now
;;
;; A stated limit: what code run through `eval_elisp' (or any other full
;; Emacs control tool) changed is not attributed and may not be listed.  Emacs
;; is not monitored to close that gap.  The list may also include the user's
;; own edits to a buffer a tool touched; the user looks at a diff, so that is
;; acceptable.
;;
;; Diff is observation.  Save and revert here are explicit user operations,
;; so they do not contradict "the model's default path performs no
;; persistence".  Nothing in this file is a model tool.
;; See docs/decisions/accepted/2026-09-18-buffer-first-mutation.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-plugin)
(require 'superchat-event)
(require 'superchat-tape)

(declare-function superchat-db-tape-select "superchat-db" (sql &optional params))
(defvar superchat--session-id)

(defun superchat-changes--child-sessions (run-ids)
  "Return the ids of Sessions whose Runs were started by one of RUN-IDS.
A delegated child's structured edits belong to the Session that started it."
  (when (fboundp 'superchat-db-tape-select)
    (delete-dups
     (cl-loop for run in run-ids
              append (mapcar
                      (lambda (row) (if (consp row) (car row) row))
                      (ignore-errors
                        (superchat-db-tape-select
                         "SELECT DISTINCT session_id FROM tape WHERE meta LIKE ?"
                         (list (format "%%\"parent_run_id\":\"%s\"%%" run)))))))))

(defun superchat-changes--targets (session-id &optional run-id)
  "Return (NAME . FILE) of every structured edit SESSION-ID's Tape records.
With RUN-ID only that Run's, together with the Runs it delegated to."
  (let* ((events (superchat-tape-replay-events session-id))
         (own (if run-id
                  (cl-remove-if-not (lambda (event) (equal run-id (plist-get event :run-id)))
                                    events)
                events))
         (runs (delete-dups (delq nil (mapcar (lambda (event) (plist-get event :run-id)) own))))
         (children (cl-remove session-id (superchat-changes--child-sessions runs)
                              :test #'equal))
         (all (append own
                      (cl-loop for child in children
                               append (ignore-errors (superchat-tape-replay-events child))))))
    (delete-dups
     (delq nil
           (mapcar (lambda (event)
                     (when (equal "tool.completed" (plist-get event :type))
                       (when-let* ((name (plist-get (plist-get event :payload) :buffer)))
                         (cons name (plist-get (plist-get event :payload) :buffer-file)))))
                   all)))))

(defun superchat-changes-candidates (session-id &optional run-id)
  "Return the unsaved buffers SESSION-ID's structured tool calls edited.
Each element is (:buffer BUFFER :name NAME :file FILE :new-file BOOL).  With
RUN-ID, only what that Run (and the Runs it delegated to) edited.  Derived
from the Tape and the buffers' state now; see this file's commentary for what
is not attributed."
  (let (seen rows)
    (dolist (target (superchat-changes--targets session-id run-id))
      (let* ((file (cdr target))
             (buffer (or (and file (find-buffer-visiting file))
                         (get-buffer (car target)))))
        (when (and (buffer-live-p buffer)
                   (not (memq buffer seen))
                   (buffer-modified-p buffer))
          (push buffer seen)
          (let ((visited (buffer-file-name buffer)))
            (push (list :buffer buffer :name (buffer-name buffer) :file visited
                        :new-file (and visited (not (file-exists-p visited)) t))
                  rows)))))
    (nreverse rows)))

(defun superchat-changes--lines (rows)
  (mapconcat (lambda (row)
               (format "- %s%s"
                       (plist-get row :name)
                       (cond ((plist-get row :new-file)
                              (format " -> %s (new file, nothing on disk yet)" (plist-get row :file)))
                             ((plist-get row :file) (format " -> %s" (plist-get row :file)))
                             (t " (not visiting a file)"))))
             rows "\n"))

(defun superchat-changes-run-summary (session-id run-id)
  "Return the Run-end text about RUN-ID's unsaved edits, or nil when none."
  (when-let* ((rows (superchat-changes-candidates session-id run-id)))
    (format "%d buffer%s edited by this Run %s unsaved -- saving is yours; /changes to review:\n%s"
            (length rows) (if (= 1 (length rows)) "" "s")
            (if (= 1 (length rows)) "is" "are")
            (superchat-changes--lines rows))))

(defun superchat-changes--announce (text)
  "Tell the user TEXT.  The chat View renders the Tape and is not written to."
  (message "superchat: %s" text))

(defun superchat-changes--on-turn-completed (_run status)
  "At a terminal Run, announce what it left unsaved."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status)))
    (when-let* ((session (plist-get status :session-id))
                (run (plist-get status :run-id))
                (text (ignore-errors (superchat-changes-run-summary session run))))
      (superchat-changes--announce text))))

;;;; The user's operations -----------------------------------------------------

(defun superchat-changes--read-buffer (session-id)
  (let* ((rows (superchat-changes-candidates session-id))
         (names (mapcar (lambda (row) (plist-get row :name)) rows)))
    (unless names (user-error "No unsaved buffers edited by this Session's tools"))
    (get-buffer (completing-read "Buffer: " names nil t))))

(defun superchat-changes-diff (buffer)
  "Show how BUFFER differs from its file.  Observation only."
  (interactive (list (superchat-changes--read-buffer superchat--session-id)))
  (if (buffer-file-name buffer)
      (diff-buffer-with-file buffer)
    (display-buffer buffer)))

(defun superchat-changes-save (buffer)
  "Save BUFFER.  The user's operation: this is never offered to a model."
  (interactive (list (superchat-changes--read-buffer superchat--session-id)))
  (with-current-buffer buffer (save-buffer)))

(defun superchat-changes-revert (buffer)
  "Discard BUFFER's unsaved changes.  The user's operation."
  (interactive (list (superchat-changes--read-buffer superchat--session-id)))
  (with-current-buffer buffer
    (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t)
      ;; A new file that was never saved has nothing to go back to.
      (set-buffer-modified-p nil)
      (kill-buffer buffer))))

;;;###autoload
(defun superchat-changes ()
  "List the unsaved buffers this Session's tools edited, with what to do next."
  (interactive)
  (let ((rows (superchat-changes-candidates superchat--session-id)))
    (message "%s"
             (if rows
                 (format "%s\nM-x superchat-changes-diff / -save / -revert"
                         (superchat-changes--lines rows))
               "No unsaved buffers edited by this Session's tools"))))

(defun superchat--cmd-changes (_cmd _args _input _lang _target-model)
  "The `/changes' command: this Session's unsaved, tool-edited buffers."
  (let ((rows (superchat-changes-candidates superchat--session-id)))
    (list :type :echo
          :content
          (if rows
              (concat "Unsaved buffers edited by this Session's tools "
                      "(edits made through eval_elisp are not tracked):\n"
                      (superchat-changes--lines rows)
                      "\nReview with M-x superchat-changes-diff; keep with M-x superchat-changes-save; "
                      "discard with M-x superchat-changes-revert.  Saving is yours, not the model's.")
            "No unsaved buffers edited by this Session's tools."))))

(superchat-define-plugin changes
  :doc "The unsaved-changes outlet's Run-end notice.  The /changes command is
registered with the other built-in commands by `chat-commands'."
  :apply
  (lambda (scope _)
    (superchat-on scope 'turn-completed 'changes-notice
                  #'superchat-changes--on-turn-completed)))

(provide 'superchat-changes)

;;; superchat-changes.el ends here
