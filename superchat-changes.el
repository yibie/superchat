;;; superchat-changes.el --- Unsaved buffers a Session's tools edited -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Buffer-first mutation: the model's ordinary edits end in a buffer, and
;; saving is the user's.  This module is the outlet that makes those edits
;; visible when the user is not looking at the buffer.  It records nothing of
;; its own.  The candidates are derived each time they are asked for:
;;
;;   the Session's (or one Run's) Runs that made a structured buffer edit, read
;;   from their `tool.completed' facts on the Tape, delegated Runs included
;;       ∩
;;   buffers that are live, `buffer-modified-p' right now, and carry the mark
;;   of one of those Runs
;;
;; The mark is how the edited buffer is recognized.  A name and a file describe
;; a buffer at one moment and either can come to mean another buffer (a rename
;; followed by a reuse of the name, `set-visited-file-name', kill and revisit);
;; the buffer object is the only identity Emacs has.  The mark is a buffer-local
;; variable set by the editing tool: it is not on the Tape, not on disk, records
;; nothing about what changed, and dies with its buffer.  The Tape's `:buffer'
;; and `:buffer-file' stay as evidence of that moment and locate nothing.
;; See docs/decisions/implemented/2026-09-19-changes-recognizes-a-buffer-by-its-mark.md.
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
;; See docs/decisions/implemented/2026-09-18-buffer-first-mutation.md.
;;
;; The review screen shows the candidates one buffer at a time: the unified
;; diff of the file against the buffer, `s' to save it (the user's operation),
;; `n' to leave it unsaved and see the next, `q' to stop.  It is the screen the
;; attended acceptance runs used, brought into the product and colored with
;; `diff-mode', and with diffs.el's `diffs-minor-mode' when diffs.el is
;; installed.  When a Run leaves unsaved buffers it appears by itself in another
;; window and the cursor goes to it -- after the minibuffer, when that is in use
;; -- and comes back to where it was when the review ends; `/changes' and
;; `superchat-changes-diff' open it selected too.  Keys typed before it is
;; selected are dropped, so they never save anything.  Buffers that visit no file have nothing to save
;; to and are not on the screen.
;; See docs/decisions/accepted/2026-09-24-editor-mode-agent-mode-and-review-screen.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff)
(require 'diff-mode)
(require 'superchat-plugin)
(require 'superchat-event)
(require 'superchat-tape)

(declare-function superchat-db-tape-select "superchat-db" (sql &optional params))
(declare-function diffs-minor-mode "diffs" (&optional arg))
(defvar diffs-minor-mode)
(defvar superchat--session-id)

(defcustom superchat-changes-review-after-run t
  "Whether the review screen appears by itself when a Run leaves unsaved buffers.
It is shown in a window other than the selected one and the cursor goes to it;
keys typed before that are dropped.  While the minibuffer is in use it waits
for the minibuffer to finish.  `/changes' and `superchat-changes-diff' open
it and select it whatever this says."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-changes-review-use-diffs t
  "Whether the review screen is drawn by diffs.el when diffs.el is installed.
Without diffs.el, or with this nil, the colors are `diff-mode''s own."
  :type 'boolean
  :group 'superchat)

(defvar-local superchat-changes--edited-by nil
  "The structured edits made to this buffer: (BUFFER . PAIRS).
PAIRS is a list of (SESSION-ID . RUN-ID).  BUFFER is the buffer the mark was
given to: `clone-buffer' copies local variables, and a copy naming another
buffer is not a mark.")
;; A major-mode change is not a new buffer.
(put 'superchat-changes--edited-by 'permanent-local t)

(defun superchat-changes-mark-buffer (buffer session-id run-id)
  "Note on BUFFER itself that RUN-ID of SESSION-ID changed it with a tool."
  (when (and (buffer-live-p buffer) (stringp session-id) (stringp run-id))
    (with-current-buffer buffer
      (let ((pairs (superchat-changes--marks buffer))
            (pair (cons session-id run-id)))
        (setq superchat-changes--edited-by
              (cons buffer (if (member pair pairs) pairs (cons pair pairs))))))))

(defun superchat-changes--marks (buffer)
  "Return the (SESSION-ID . RUN-ID) pairs BUFFER is marked with."
  (let ((mark (buffer-local-value 'superchat-changes--edited-by buffer)))
    (and (consp mark) (eq buffer (car mark)) (cdr mark))))

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

(defun superchat-changes--editing-runs (session-id &optional run-id)
  "Return (SESSION . RUN) of every Run the Tape shows making a structured edit.
SESSION-ID's Runs, or with RUN-ID only that Run, together with the Runs they
delegated to."
  (let* ((events (superchat-tape-replay-events session-id))
         (own (if run-id
                  (cl-remove-if-not (lambda (event) (equal run-id (plist-get event :run-id)))
                                    events)
                events))
         (runs (delete-dups (delq nil (mapcar (lambda (event) (plist-get event :run-id)) own))))
         (children (cl-remove session-id (superchat-changes--child-sessions runs)
                              :test #'equal))
         (pairs nil))
    (cl-flet ((collect (session events)
                (dolist (event events)
                  (when (and (equal "tool.completed" (plist-get event :type))
                             (plist-get (plist-get event :payload) :buffer)
                             (plist-get event :run-id))
                    (push (cons session (plist-get event :run-id)) pairs)))))
      (collect session-id own)
      (dolist (child children)
        (collect child (ignore-errors (superchat-tape-replay-events child)))))
    (delete-dups (nreverse pairs))))

(defun superchat-changes-candidates (session-id &optional run-id)
  "Return the unsaved buffers SESSION-ID's structured tool calls edited.
Each element is (:buffer BUFFER :name NAME :file FILE :new-file BOOL).  With
RUN-ID, only what that Run (and the Runs it delegated to) edited.  Derived
from the Tape and the buffers' state now; see this file's commentary for what
is not attributed."
  (let ((runs (superchat-changes--editing-runs session-id run-id))
        rows)
    (when runs
      ;; The buffer is recognized by its own mark, never looked up by the name
      ;; or the file the Tape recorded.  Everything reported is read from the
      ;; live buffer now, so a rename or `set-visited-file-name' never makes an
      ;; old path pass for the current one.
      (dolist (buffer (buffer-list))
        (when (and (buffer-modified-p buffer)
                   (cl-intersection runs (superchat-changes--marks buffer) :test #'equal))
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
  "At a terminal Run, announce what it left unsaved and show the review screen.
This runs inside the terminal emission, so the screen is shown from a timer."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status)))
    (when-let* ((session (plist-get status :session-id))
                (run (plist-get status :run-id))
                (text (ignore-errors (superchat-changes-run-summary session run))))
      (superchat-changes--announce text)
      (when superchat-changes-review-after-run
        (run-at-time 0 nil #'superchat-changes--review-after-run session run)))))

;;;; The review screen ----------------------------------------------------------

(defconst superchat-changes-review-buffer-name "*superchat review*"
  "Name of the review screen's buffer.")

(defvar-local superchat-changes-review--state nil
  "Where the review screen is: (ROWS . INDEX).
ROWS are candidate plists, as `superchat-changes-candidates' returns them.")
;; Every redraw runs `diff-mode', which would otherwise forget where it was.
(put 'superchat-changes-review--state 'permanent-local t)

(defvar-local superchat-changes-review--return-window nil
  "The window that was selected when the review screen took the cursor.")
;; Rendering sets `diff-mode' again for each buffer; keep this across it.
(put 'superchat-changes-review--return-window 'permanent-local t)

(defvar-local superchat-changes-review--active nil
  "Non-nil in the review buffer; enables its keys in
`minor-mode-overriding-map-alist'.")

(defvar superchat-changes-review-keys
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'superchat-changes-review-save)
    (define-key map (kbd "n") #'superchat-changes-review-next)
    (define-key map (kbd "q") #'superchat-changes-review-quit)
    (define-key map (kbd "g") #'superchat-changes-review-refresh)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    ;; Keys that would change the source buffer, edit this view, or start a
    ;; per-block flow the review does not offer: `diff-mode''s hunk
    ;; application, ediff patching and hunk kills; diffs.el's accept, reject,
    ;; reset, apply and annotation keys.
    (dolist (key '("a" "A" "R" "U" "v" "x" "c" "E" "k" "K" "M-k"
                   "C-c C-a" "C-c C-c" "C-c C-e" "C-c C-s" "C-c C-r"
                   "C-c C-d" "C-c C-u" "C-c C-w" "C-c C-n"))
      (define-key map (kbd key) #'superchat-changes-review-unavailable))
    map)
  "Keys of the review screen.  They take precedence over `diff-mode''s and
diffs.el's through `minor-mode-overriding-map-alist'.")

(defun superchat-changes--review-diff (buffer)
  "Return the unified diff of BUFFER's file against BUFFER's text.
The labels are a/NAME and b/NAME relative to the file's directory.  A file not
on disk yet is compared with the null device, so the whole text is added."
  (let* ((file (buffer-file-name buffer))
         (name (file-name-nondirectory file))
         (exists (file-exists-p file))
         (temp (make-temp-file "superchat-unsaved-diff-")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (write-region (point-min) (point-max) temp nil 'silent)))
          (with-temp-buffer
            (let ((status (call-process diff-command nil t nil "-u"
                                        "-L" (if exists (concat "a/" name) null-device)
                                        "-L" (concat "b/" name)
                                        (if exists file null-device) temp)))
              (cond
               ((and (integerp status) (> status 1))
                (format "(diff could not compare %s with the buffer: %s)\n"
                        file (string-trim (buffer-string))))
               ((zerop (buffer-size))
                "(the buffer's text equals the file; saving only clears the modified mark)\n")
               (t (buffer-string))))))
      (delete-file temp))))

(defun superchat-changes--review-row ()
  "The candidate the review screen is showing."
  (let ((state superchat-changes-review--state))
    (nth (cdr state) (car state))))

(defun superchat-changes--review-render (review)
  "Draw the review buffer REVIEW for the candidate it is at."
  (with-current-buffer review
    (let* ((state superchat-changes-review--state)
           (row (superchat-changes--review-row))
           (buffer (plist-get row :buffer))
           (live (buffer-live-p buffer))
           (file (and live (buffer-file-name buffer)))
           (inhibit-read-only t))
      (when (bound-and-true-p diffs-minor-mode)
        (diffs-minor-mode -1))
      (erase-buffer)
      (insert (format "Review %d/%d · %s%s\n"
                      (1+ (cdr state)) (length (car state))
                      (if live (buffer-name buffer) (plist-get row :name))
                      (if file (format " (%s)" (abbreviate-file-name file)) "")))
      (insert (cond ((not file) "That buffer is gone or no longer visits a file.\n")
                    ((file-exists-p file)
                     "Unsaved.  The diff below is what saving would write to disk.\n")
                    (t "Not on disk yet.  Saving creates the file with the whole buffer.\n")))
      (insert "s: save   n: leave unsaved, next   q: stop (the rest stay unsaved)   "
              "g: refresh   SPC/DEL: scroll   M-n/M-p: next/previous change\n\n")
      (when file
        (insert (superchat-changes--review-diff buffer))
        (setq default-directory (file-name-directory file)))
      ;; Read-only before the mode starts, so `diff-mode' gives its read-only
      ;; single-key navigation; the review's own keys still come first.
      (setq buffer-read-only t)
      (diff-mode)
      (font-lock-mode 1)
      (when (and file superchat-changes-review-use-diffs (require 'diffs nil t))
        (condition-case err
            (diffs-minor-mode 1)
          (error (message "superchat: diffs.el could not draw the review: %s"
                          (error-message-string err)))))
      (setq-local superchat-changes-review--active t)
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'superchat-changes-review--active
                              superchat-changes-review-keys)
                        minor-mode-overriding-map-alist))
      (add-hook 'window-selection-change-functions
                #'superchat-changes--review-selected nil t)
      (goto-char (point-min)))))

(defun superchat-changes--review-selected (window)
  "Drop keys typed ahead when WINDOW, which shows the review screen, is selected.
A key the user typed before the screen became theirs must not save anything."
  (when (eq window (selected-window))
    (discard-input)))

(defun superchat-changes-review (rows &optional select)
  "Show the review screen for ROWS, candidate plists, starting at the first.
Rows whose buffer is gone or visits no file are not shown; the check is made
now, since ROWS may have been computed before a timer ran.  Returns the review
buffer, or nil when nothing is left to show.  The screen is displayed in a
window other than the selected one; with SELECT it is also selected and keys
typed ahead are dropped, otherwise the selected window stays as it was.  A
selected screen gives the cursor back to the window it came from when the
review ends."
  (let ((rows (cl-remove-if-not
               (lambda (row)
                 (let ((buffer (plist-get row :buffer)))
                   (and (buffer-live-p buffer) (buffer-file-name buffer))))
               rows)))
    (when rows
      (let ((review (get-buffer-create superchat-changes-review-buffer-name)))
        (with-current-buffer review
          (setq superchat-changes-review--state (cons rows 0)))
        (superchat-changes--review-render review)
        (let ((window (display-buffer
                       review
                       '((display-buffer-reuse-window
                          display-buffer-use-some-window
                          display-buffer-pop-up-window
                          display-buffer-below-selected)
                         (inhibit-same-window . t)))))
          (when select
            (superchat-changes--review-select window)))
        review))))

(defun superchat-changes--review-select (window)
  "Move the cursor to WINDOW, the review screen, dropping keys typed ahead.
The window selected before is where the cursor returns when the review ends."
  (when (window-live-p window)
    (let ((from (selected-window)))
      (with-current-buffer (window-buffer window)
        (unless (eq from window)
          (setq superchat-changes-review--return-window from))))
    (select-window window)
    (discard-input)))

(defun superchat-changes--review-select-after-minibuffer ()
  "Once the minibuffer in use is done, move the cursor to the review screen."
  (remove-hook 'minibuffer-exit-hook
               #'superchat-changes--review-select-after-minibuffer)
  ;; The exit hook runs while the minibuffer is still up; select just after.
  (run-at-time 0 nil
               (lambda ()
                 (when-let* ((review (get-buffer superchat-changes-review-buffer-name))
                             (window (get-buffer-window review t)))
                   (if (active-minibuffer-window)
                       (add-hook 'minibuffer-exit-hook
                                 #'superchat-changes--review-select-after-minibuffer)
                     (superchat-changes--review-select window))))))

(defun superchat-changes--review-after-run (session run)
  "Show the review screen for what RUN of SESSION left unsaved and select it.
While the minibuffer is in use it is shown unselected and selected after."
  (when-let* ((rows (ignore-errors (superchat-changes-candidates session run))))
    (let ((busy (active-minibuffer-window)))
      (when (and (superchat-changes-review rows (not busy)) busy)
        (add-hook 'minibuffer-exit-hook
                  #'superchat-changes--review-select-after-minibuffer)))))

(defun superchat-changes--review-buffer ()
  (or (and (local-variable-p 'superchat-changes-review--state)
           (current-buffer))
      (get-buffer superchat-changes-review-buffer-name)
      (user-error "No review screen is open")))

(defun superchat-changes--review-advance (review)
  "Show REVIEW's next candidate, or close it after the last."
  (with-current-buffer review
    (let ((state superchat-changes-review--state))
      (if (< (1+ (cdr state)) (length (car state)))
          (progn
            (setq superchat-changes-review--state (cons (car state) (1+ (cdr state))))
            (superchat-changes--review-render review))
        (superchat-changes--review-close review)))))

(defun superchat-changes--review-close (review)
  "Close the review buffer REVIEW, give its windows back and the cursor too."
  (let ((return (and (buffer-live-p review)
                     (buffer-local-value 'superchat-changes-review--return-window
                                         review))))
    (dolist (window (get-buffer-window-list review nil t))
      (quit-restore-window window 'bury))
    (when (buffer-live-p review)
      (kill-buffer review))
    (when (window-live-p return)
      (select-window return))))

(defun superchat-changes-review-save ()
  "Save the buffer on the review screen, then show the next one.
Saving is the user's operation, the same `superchat-changes-save' offers."
  (interactive)
  (let* ((review (superchat-changes--review-buffer))
         (buffer (plist-get (with-current-buffer review (superchat-changes--review-row))
                            :buffer)))
    (cond
     ((not (buffer-live-p buffer)) (message "That buffer is gone; nothing to save"))
     ((not (buffer-modified-p buffer)) (message "%s has no unsaved changes" (buffer-name buffer)))
     (t (superchat-changes-save buffer)))
    (superchat-changes--review-advance review)))

(defun superchat-changes-review-next ()
  "Leave the buffer on the review screen unsaved and show the next one."
  (interactive)
  (superchat-changes--review-advance (superchat-changes--review-buffer)))

(defun superchat-changes-review-quit ()
  "Stop reviewing.  Every buffer not saved on the screen stays unsaved."
  (interactive)
  (superchat-changes--review-close (superchat-changes--review-buffer)))

(defun superchat-changes-review-refresh ()
  "Redraw the review screen from the buffer's text as it is now."
  (interactive)
  (superchat-changes--review-render (superchat-changes--review-buffer)))

(defun superchat-changes-review-unavailable ()
  "Say that this key does nothing on the review screen."
  (interactive)
  (message "Not on the review screen: s saves, n leaves unsaved, q stops"))

;;;; The user's operations -----------------------------------------------------

(defun superchat-changes--read-buffer (session-id)
  (let* ((rows (superchat-changes-candidates session-id))
         (names (mapcar (lambda (row) (plist-get row :name)) rows)))
    (unless names (user-error "No unsaved buffers edited by this Session's tools"))
    (get-buffer (completing-read "Buffer: " names nil t))))

(defun superchat-changes-diff (buffer)
  "Open the review screen for BUFFER alone: how it differs from its file.
`s' there saves it (the user's operation) and `q' leaves it unsaved.  A buffer
that visits no file has nothing to compare with and is simply displayed."
  (interactive (list (superchat-changes--read-buffer superchat--session-id)))
  (if (buffer-file-name buffer)
      (superchat-changes-review
       (list (list :buffer buffer :name (buffer-name buffer)
                   :file (buffer-file-name buffer)
                   :new-file (not (file-exists-p (buffer-file-name buffer)))))
       t)
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
  "List the unsaved buffers this Session's tools edited and open the review screen."
  (interactive)
  (let ((rows (superchat-changes-candidates superchat--session-id)))
    (message "%s"
             (if rows
                 (format "%s\nReview: s saves, n leaves unsaved, q stops"
                         (superchat-changes--lines rows))
               "No unsaved buffers edited by this Session's tools"))
    (when rows
      (superchat-changes-review rows t))))

(defun superchat--cmd-changes (_cmd _args _input _lang _target-model)
  "The `/changes' command: this Session's unsaved, tool-edited buffers.
Besides the list, it opens the review screen once the command has returned."
  (let ((rows (superchat-changes-candidates superchat--session-id)))
    (when rows
      (run-at-time 0 nil #'superchat-changes-review rows t))
    (list :type :echo
          :content
          (if rows
              (concat "Unsaved buffers edited by this Session's tools "
                      "(edits made through eval_elisp are not tracked):\n"
                      (superchat-changes--lines rows)
                      "\nThe review screen shows each file's diff: s saves it, n leaves it unsaved, q stops.  "
                      "Buffers that visit no file are listed here only.  Saving is yours, not the model's.")
            "No unsaved buffers edited by this Session's tools."))))

(superchat-define-plugin changes
  :doc "The unsaved-changes outlet's Run-end notice and review screen.  The
/changes command is registered with the other built-in commands by
`chat-commands'."
  :apply
  (lambda (scope _)
    (superchat-on scope 'turn-completed 'changes-notice
                  #'superchat-changes--on-turn-completed)))

(provide 'superchat-changes)

;;; superchat-changes.el ends here
