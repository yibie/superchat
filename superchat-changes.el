;;; superchat-changes.el --- Unsaved buffers a Session's tools edited -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Buffer-first mutation: the model's ordinary edits end in a buffer, and
;; saving is the user's.  This module is the outlet that makes those edits
;; visible when the user is not looking at the buffer.  The list of
;; candidates is recorded nowhere; it is derived each time it is asked for
;; (the notices further down are recorded: this module appends
;; `run.ended-unwatched' and `run.seen' and keeps a checkpoint of its fold):
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
;; See docs/decisions/implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff)
(require 'diff-mode)
(require 'superchat-plugin)
(require 'superchat-event)
(require 'superchat-tape)
(declare-function superchat-view-session-configuration "superchat-tape-view"
                  (session-id))

(declare-function superchat-db-tape-select "superchat-db" (sql &optional params))
(declare-function superchat-db-projection-checkpoint "superchat-db"
                  (name &optional scope))
(declare-function superchat-db-projection-set-checkpoint "superchat-db"
                  (name version last-storage-id &optional scope))
(declare-function superchat-runtime-status "superchat-runtime" (run-id))
(declare-function superchat-runtime--terminal-p "superchat-runtime" (status))
(declare-function superchat-runtime-admit-between-turns "superchat-runtime"
                  (session-id source text))
(declare-function superchat-plugin-row-active-p "superchat-plugin" (name))
(declare-function superchat-conversation-session-id "superchat" ())
(defvar superchat--tool-execution-context)
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

(defvar-local superchat-changes--settling nil
  "Sessions whose unsaved tool edits the save or revert under way settles.")
(put 'superchat-changes--settling 'permanent-local t)

(defun superchat-changes--pending-sessions (buffer)
  "Return the Sessions whose tools' unsaved edits BUFFER holds, if it is the
user who acts on it now: no tool is running and the `changes' row is on."
  (when (and (buffer-modified-p buffer)
             (buffer-file-name buffer)
             (null superchat--tool-execution-context)
             (fboundp 'superchat-plugin-row-active-p)
             (superchat-plugin-row-active-p 'changes))
    (delete-dups (mapcar #'car (superchat-changes--marks buffer)))))

(defun superchat-changes--tell-sessions (buffer sessions text)
  "Tell SESSIONS' next Turn TEXT about BUFFER; the edits are then settled.
The note is admitted material outside any Run (decision \"the next turn
learns what the user saved\"); BUFFER keeps no mark of those Sessions, so a
later save is not reported until their tools edit it again."
  (dolist (session sessions)
    (ignore-errors
      (superchat-runtime-admit-between-turns
       session (list :kind 'user-save :buffer (buffer-name buffer)) text)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((pairs (cl-remove-if (lambda (pair) (member (car pair) sessions))
                                 (superchat-changes--marks buffer))))
        (setq superchat-changes--edited-by (and pairs (cons buffer pairs)))))))

(defun superchat-changes--before-settle ()
  "Before a save or revert, note whose unsaved tool edits it settles."
  (setq superchat-changes--settling
        (superchat-changes--pending-sessions (current-buffer))))

(defun superchat-changes--after-save ()
  "After the user saved a buffer Sessions' tools edited, tell their next Turn."
  (when-let* ((sessions (prog1 superchat-changes--settling
                          (setq superchat-changes--settling nil))))
    (superchat-changes--tell-sessions
     (current-buffer) sessions
     (format "The user saved %s (%s); the file on disk now holds the buffer's text."
             (buffer-name) (buffer-file-name)))))

(defun superchat-changes--after-revert ()
  "After the user threw away Sessions' unsaved tool edits, tell their next Turn."
  (when-let* ((sessions (prog1 superchat-changes--settling
                          (setq superchat-changes--settling nil))))
    (superchat-changes--tell-sessions
     (current-buffer) sessions
     (format "The user discarded your unsaved edits to %s (%s); the buffer holds the file's text again."
             (buffer-name) (buffer-file-name)))))

(add-hook 'before-save-hook #'superchat-changes--before-settle)
(add-hook 'after-save-hook #'superchat-changes--after-save)
(add-hook 'before-revert-hook #'superchat-changes--before-settle)
(add-hook 'after-revert-hook #'superchat-changes--after-revert)

(defun superchat-changes--child-sessions (run-ids)
  "Return the ids of Sessions whose Runs were started by one of RUN-IDS.
A delegated child's structured edits belong to the Session that started it.
The Tape is read once however many RUN-IDS there are: one Run is asked for
by name, several are picked out of every delegation on record.  Asking
once per Run made the unsaved list of a long conversation take as long as
its Runs times the whole Tape."
  (when (and run-ids (fboundp 'superchat-db-tape-select))
    (if (null (cdr run-ids))
        (delete-dups
         (mapcar (lambda (row) (if (consp row) (car row) row))
                 (ignore-errors
                   (superchat-db-tape-select
                    "SELECT DISTINCT session_id FROM tape WHERE meta LIKE ?"
                    (list (format "%%\"parent_run_id\":\"%s\"%%"
                                  (car run-ids)))))))
      (let ((wanted (make-hash-table :test #'equal))
            found)
        (dolist (run run-ids) (puthash run t wanted))
        (dolist (row (ignore-errors
                       (superchat-db-tape-select
                        "SELECT DISTINCT session_id, meta FROM tape WHERE meta LIKE ?"
                        (list "%\"parent_run_id\":\"%"))))
          (let ((session (nth 0 row))
                (meta (nth 1 row)))
            (when (and (stringp meta)
                       (string-match "\"parent_run_id\":\"\\([^\"]+\\)\"" meta)
                       (gethash (match-string 1 meta) wanted))
              (push session found))))
        (delete-dups (nreverse found))))))

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

(defun superchat-changes--workspace-root (session-id)
  "The root of the Workspace SESSION-ID is bound to, or nil."
  (and (fboundp 'superchat-view-session-configuration)
       (stringp session-id)
       (let ((root (plist-get (superchat-view-session-configuration session-id)
                              :workspace-root)))
         (and (stringp root) (not (string-empty-p root)) root))))

(defun superchat-changes--outside-p (file root)
  "Non-nil when FILE lies outside the directory ROOT.
Nil when either is nil: a conversation with no Workspace has no outside."
  (and (stringp file) (stringp root)
       (not (file-in-directory-p (file-truename file) (file-truename root)))))

(defun superchat-changes-candidates (session-id &optional run-id)
  "Return the unsaved buffers SESSION-ID's structured tool calls edited.
Each element is (:buffer BUFFER :name NAME :file FILE :new-file BOOL
:outside ROOT).  `:outside' is the root of SESSION-ID's Workspace when FILE
lies outside it, else nil: such an edit is not stopped, it is pointed out
before saving (contract W9).  With RUN-ID, only what that Run (and the Runs
it delegated to) edited.  Derived from the Tape and the buffers' state now;
see this file's commentary for what is not attributed."
  (let ((runs (superchat-changes--editing-runs session-id run-id))
        (root (superchat-changes--workspace-root session-id))
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
                        :new-file (and visited (not (file-exists-p visited)) t)
                        :outside (and (superchat-changes--outside-p visited root)
                                      root))
                  rows)))))
    (nreverse rows)))

(defun superchat-changes--lines (rows)
  (mapconcat (lambda (row)
               (format "- %s%s%s"
                       (plist-get row :name)
                       (cond ((plist-get row :new-file)
                              (format " -> %s (new file, nothing on disk yet)" (plist-get row :file)))
                             ((plist-get row :file) (format " -> %s" (plist-get row :file)))
                             (t " (not visiting a file)"))
                       (if (plist-get row :outside)
                           (format " [outside this workspace, %s]"
                                   (abbreviate-file-name (plist-get row :outside)))
                         "")))
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
      (when (and file (plist-get row :outside))
        (insert (format "Outside this conversation's workspace (%s).\n"
                        (abbreviate-file-name (plist-get row :outside)))))
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

(defconst superchat-changes--unattended-actors '("timers" "work-records")
  "The Actors a Run records when Superchat started it at a time.")

(defun superchat-changes--unattended-run-p (session run)
  "Non-nil when Superchat started RUN of SESSION at a time, not the user.
Read from SESSION's Tape.  A Run says so itself: `run.started' records the
Actor that started it.  A Run from before that was recorded is told by the
facts around it: a scheduled task records `timer.started' with the Run it
began, and a woken Work Record's Run is the first one of a conversation that
holds `work.woken'."
  (let ((events (ignore-errors (superchat-tape-replay-events session)))
        first woken started)
    (dolist (event events)
      (pcase (plist-get event :type)
        ("run.started"
         (when (and (equal run (plist-get event :run-id))
                    (member (plist-get event :actor-id)
                            superchat-changes--unattended-actors))
           (setq started t))
         (unless first (setq first (plist-get event :run-id))))
        ("work.woken" (setq woken t))
        ("timer.started"
         (when (equal run (plist-get (plist-get event :payload) :run-id))
           (setq started t)))))
    (or started (and woken (equal run first)))))

(defun superchat-changes--review-after-run (session run)
  "Show the review screen for what RUN of SESSION left unsaved and select it.
While the minibuffer is in use it is shown unselected and selected after.
An execution Superchat started at a time shows no screen: the user is doing
something else, and the echo area has already said what is unsaved."
  (when-let* (((not (superchat-changes--unattended-run-p session run)))
              (rows (ignore-errors (superchat-changes-candidates session run))))
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

;; A code buffer has no conversation of its own.  See the review screen's
;; part of docs/decisions/implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md.
(defun superchat-changes--session ()
  "Return the Session a review command acts on here.
The current buffer's own, when it is a chat View; else the conversation the
chat window shows, as an in-place command from this buffer would join it
(M10 slice 2): the chat beside this buffer, else the main one."
  (or (and (local-variable-p 'superchat--session-id) superchat--session-id)
      (and (fboundp 'superchat-conversation-session-id)
           (superchat-conversation-session-id))
      (user-error "No conversation here")))

(defun superchat-changes--read-buffer (session-id)
  (let* ((rows (superchat-changes-candidates session-id))
         (names (mapcar (lambda (row) (plist-get row :name)) rows)))
    (unless names (user-error "No unsaved buffers edited by this Session's tools"))
    (get-buffer (completing-read "Buffer: " names nil t))))

(defun superchat-changes-diff (buffer)
  "Open the review screen for BUFFER alone: how it differs from its file.
`s' there saves it (the user's operation) and `q' leaves it unsaved.  A buffer
that visits no file has nothing to compare with and is simply displayed.
Interactively, BUFFER is one the conversation here left unsaved: this chat's,
or the one shown beside this buffer."
  (interactive
   (list (superchat-changes--read-buffer (superchat-changes--session))))
  (if (buffer-file-name buffer)
      (superchat-changes-review
       (list (list :buffer buffer :name (buffer-name buffer)
                   :file (buffer-file-name buffer)
                   :new-file (not (file-exists-p (buffer-file-name buffer)))))
       t)
    (display-buffer buffer)))

(defun superchat-changes-save (buffer)
  "Save BUFFER.  The user's operation: this is never offered to a model."
  (interactive (list (superchat-changes--read-buffer (superchat-changes--session))))
  (with-current-buffer buffer (save-buffer)))

(defun superchat-changes-revert (buffer)
  "Discard BUFFER's unsaved changes.  The user's operation."
  (interactive (list (superchat-changes--read-buffer (superchat-changes--session))))
  (with-current-buffer buffer
    (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t)
      ;; A new file that was never saved has nothing to go back to.
      (when-let* ((sessions (superchat-changes--pending-sessions buffer)))
        (superchat-changes--tell-sessions
         buffer sessions
         (format "The user discarded your unsaved edits to %s (%s); the file was never written."
                 (buffer-name) (buffer-file-name))))
      (set-buffer-modified-p nil)
      (kill-buffer buffer))))

;;;###autoload
(defun superchat-changes ()
  "List the unsaved buffers this Session's tools edited and open the review screen.
The Session is this chat's, or that of the chat shown beside this buffer."
  (interactive)
  (let ((rows (superchat-changes-candidates (superchat-changes--session))))
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

;;;; What work Superchat started by itself has left for the user ----------------

;; An execution the user did not start shows no review screen and its line in
;; the echo area is gone with the next key.  Each one that ends leaves a
;; notice here and a mark on the mode line that stays until the user has
;; looked.  The list lives in this Emacs only: the buffers it points at do
;; not outlive it either, and a Work Record keeps its executions on its own.

(declare-function superchat--render-session-view "superchat" (session-id))
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role))

(defvar superchat-changes--notices nil
  "Notices of ended executions Superchat started by itself, newest first.
Each is a plist: `:run' and `:session' (strings), `:what' (what started it),
`:status' (how it ended, a symbol; `cut-off' when Emacs quit while it ran),
`:unsaved' (how many buffers it left unsaved, counted when it ended in this
Emacs), `:time', and `:origin': `unattended' for an execution Superchat
started, `user' for one the user started that ended with its conversation
closed.  Only the first kind is counted on the mode line and shown in the
notices list; `superchat-pending' lists both.

What the Tape says, kept here so that nothing redrawing reads the Tape:
`superchat-changes-notices-reload' fills it, when an execution ends, when
one is looked at, and when the plugin is mounted.")

(defvar superchat-changes--unsaved-at-end (make-hash-table :test #'equal)
  "Run id to the number of buffers it left unsaved, counted when it ended.
Of this Emacs only: the marks on buffers do not outlive it.")

(defvar superchat-changes--started-by (make-hash-table :test #'equal)
  "Run id to the words for what started it, once they were worked out.")

(defconst superchat-changes--unseen-checkpoint 'changes-unseen
  "Names the storage id below which no execution is left to look at.")

(defvar superchat-changes-notice-mode-line nil
  "The mode line's mark for `superchat-changes--notices'; nil when none.")
;; A string with a keymap in the mode line is honoured only from a risky
;; variable.
(put 'superchat-changes-notice-mode-line 'risky-local-variable t)

(defconst superchat-changes-notices-buffer-name "*superchat notices*")

(defvar superchat-changes-notice-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'superchat-changes-notices)
    map)
  "Keys of the mode line's mark.")

(defun superchat-changes--own-notices ()
  "The notices of executions Superchat started by itself, newest first.
These are what the mode line counts and the notices list shows."
  (cl-remove-if-not (lambda (notice) (eq 'unattended (plist-get notice :origin)))
                    superchat-changes--notices))

(defun superchat-changes--notices-refresh ()
  "Make the mode line's mark and the open notices list say what is pending."
  (let ((count (length (superchat-changes--own-notices))))
    (setq superchat-changes-notice-mode-line
          (and (> count 0)
               (propertize
                (format " [Superchat: %d]" count)
                'face 'warning
                'mouse-face 'mode-line-highlight
                'help-echo
                "Work Superchat did by itself has ended: mouse-1 or \
M-x superchat-changes-notices"
                'local-map superchat-changes-notice-mode-line-map)))
    (unless (memq 'superchat-changes-notice-mode-line global-mode-string)
      (setq global-mode-string
            (append (or global-mode-string '(""))
                    '(superchat-changes-notice-mode-line))))
    (when-let* ((buffer (get-buffer superchat-changes-notices-buffer-name)))
      (with-current-buffer buffer
        (when (derived-mode-p 'superchat-changes-notices-mode)
          (tabulated-list-print t))))
    (force-mode-line-update t)))

(defun superchat-changes--unseen-bound ()
  "The storage id below which no execution is left to look at."
  (or (cadr (superchat-db-projection-checkpoint
             superchat-changes--unseen-checkpoint))
      0))

(defun superchat-changes--row-time (created)
  "The time the Tape row stamped CREATED (UTC text) was written."
  (or (ignore-errors (date-to-time (concat created " +0000")))
      (current-time)))

(defun superchat-changes--started-by (session run actor)
  "Say what started RUN of SESSION, whose `run.started' names ACTOR.
Asked of SESSION's Tape once for a Run."
  (or (gethash run superchat-changes--started-by)
      (puthash
       run
       (let* ((json-object-type 'alist) (json-array-type 'list)
              (json-key-type 'symbol)
              (payload
               (lambda (type &optional of-run)
                 (alist-get
                  'payload
                  (ignore-errors
                    (json-read-from-string
                     (or (caar
                          (superchat-db-tape-select
                           (concat
                            "SELECT meta FROM tape WHERE session_id = ?"
                            " AND json_extract(meta, '$.event_type') = ?"
                            (if of-run
                                " AND json_extract(meta, '$.payload.\"run-id\"') = ?"
                              "")
                            " ORDER BY id DESC LIMIT 1")
                           (if of-run (list session type run)
                             (list session type))))
                         "{}"))))))
              (id (lambda (payload &rest keys)
                    (cl-some (lambda (key) (alist-get key payload)) keys)))
              (timer (or (funcall id (funcall payload "timer.started" t)
                                  'timer-id 'timer_id)
                         (and (equal actor "timers")
                              (funcall id (funcall payload "timer.sending")
                                       'timer-id 'timer_id))))
              (work (and (not timer)
                         (funcall id (funcall payload "work.woken")
                                  'work-id 'work_id))))
         (cond (timer (format "scheduled task %s" timer))
               (work (format "work record %s" work))
               ((equal actor "timers") "scheduled task")
               ((equal actor "work-records") "work record")
               (t "scheduled work")))
       superchat-changes--started-by)))

(defun superchat-changes--unseen-from-tape ()
  "Read from the Tape the ended executions that were not looked at.
Return them newest first, and move the bound past what is settled.

One read of the Tape rows above the bound, of every conversation: the Runs
that began there, how each ended, and which were looked at.  A Run is
Superchat's own when its `run.started' names one of
`superchat-changes--unattended-actors'; a Run from before the Actor was
recorded names the user.  A Run the user started is listed only when the
fact `run.ended-unwatched' says it ended with its conversation closed.
A Run with no ending on the Tape that this Emacs is not running was cut
off."
  (let* ((bound (superchat-changes--unseen-bound))
         (rows (superchat-db-tape-select
                "SELECT id, session_id,
                        json_extract(meta, '$.event_type'),
                        json_extract(meta, '$.run_id'),
                        json_extract(meta, '$.actor_id'),
                        json_extract(meta, '$.payload.abandoned'),
                        json_extract(meta, '$.payload.status'),
                        created_at,
                        json_extract(meta, '$.payload.\"tool-budget\".action'),
                        json_extract(meta, '$.payload.\"tool-budget\".limit')
                 FROM tape
                 WHERE id > ?
                   AND json_extract(meta, '$.event_type') IN
                       ('run.started', 'run.completed', 'run.failed',
                        'run.cancelled', 'run.seen', 'run.ended-unwatched')
                 ORDER BY id ASC"
                (list bound)))
         (runs (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         (unwatched (make-hash-table :test #'equal))
         order last unsettled notices)
    (dolist (row rows)
      (cl-destructuring-bind (id session type run actor abandoned ended created
                                 budget-action budget-limit)
          row
        (setq last id)
        (pcase type
          ("run.started"
           (unless (gethash run runs)
             (push run order)
             (puthash run (list :run run :session session :actor actor
                                :began id :time created)
                      runs)))
          ((or "run.completed" "run.failed" "run.cancelled")
           (when-let* ((entry (gethash run runs)))
             (puthash run
                      (plist-put
                       (plist-put entry :status
                                  (cond ((equal type "run.completed") 'completed)
                                        ((equal type "run.cancelled") 'cancelled)
                                        ((memq abandoned '(1 t)) 'cut-off)
                                        (t 'failed)))
                       :time created)
                      runs)
             (superchat-changes--note-calls-cut-off
              run runs budget-action budget-limit)))
          ("run.seen" (puthash run t seen))
          ;; It says how the Run ended itself: the Run's own rows may lie
          ;; below the bound, which a Run the user started does not hold.
          ("run.ended-unwatched"
           (puthash run t unwatched)
           (unless (gethash run runs)
             (push run order)
             (puthash run (list :run run :session session :began id
                                :status (and (stringp ended) (intern ended))
                                :time created)
                      runs))
           (superchat-changes--note-calls-cut-off
            run runs budget-action budget-limit)))))
    (dolist (run (nreverse order))
      (let* ((entry (gethash run runs))
             (session (plist-get entry :session))
             (actor (plist-get entry :actor)))
        (when (and (not (gethash run seen))
                   (or (member actor superchat-changes--unattended-actors)
                       (gethash run unwatched)))
          (let* ((live (and (fboundp 'superchat-runtime-status)
                            (superchat-runtime-status run)))
                 (status (or (plist-get entry :status)
                             (if live
                                 (and (superchat-runtime--terminal-p
                                       (plist-get live :status))
                                      (plist-get live :status))
                               'cut-off))))
            (setq unsettled (if unsettled
                                (min unsettled (plist-get entry :began))
                              (plist-get entry :began)))
            (when status
              (push (list :run run :session session
                          :origin (if (member actor
                                              superchat-changes--unattended-actors)
                                      'unattended
                                    'user)
                          :what (if (member actor
                                            superchat-changes--unattended-actors)
                                    (superchat-changes--started-by session run actor)
                                  "started by you")
                          :status status
                          :calls-cut-off (plist-get entry :calls-cut-off)
                          :unsaved (gethash run superchat-changes--unsaved-at-end 0)
                          :time (superchat-changes--row-time
                                 (plist-get entry :time)))
                    notices))))))
    (let ((settled (cond (unsettled (1- unsettled)) (last last) (t bound))))
      (when (> settled bound)
        (superchat-db-projection-set-checkpoint
         superchat-changes--unseen-checkpoint 1 settled)))
    notices))

(defun superchat-changes--note-calls-cut-off (run runs action limit)
  "Mark RUN in RUNS as cut off by the tool-call LIMIT when ACTION says so.
ACTION and LIMIT are the `:tool-budget' of the fact that ended RUN, read
from the Tape (slice 9b)."
  (when-let* ((entry (gethash run runs))
              (limit (superchat-tool-cut-off-limit
                      (list :action action :limit limit))))
    (puthash run (plist-put entry :calls-cut-off limit) runs)))

(defun superchat-changes--notice-ending (notice)
  "How NOTICE's execution ended, in the words the lists show."
  (if-let* ((limit (plist-get notice :calls-cut-off)))
      (superchat-tool-cut-off-text limit)
    (format "%s" (plist-get notice :status))))

(defun superchat-changes-notices-reload ()
  "Make the notices what the Tape says now, and show that."
  (setq superchat-changes--notices (superchat-changes--unseen-from-tape))
  (superchat-changes--notices-refresh))

(defun superchat-changes--conversation-open-p (session)
  "Non-nil while SESSION's conversation has a live View."
  (let ((view (and (fboundp 'superchat-editor-workspace-buffer)
                   (ignore-errors
                     (superchat-editor-workspace-buffer session 'session-view)))))
    (buffer-live-p view)))

(defun superchat-changes--ended-unwatched-p (status)
  "Non-nil when STATUS is of an execution the user started and could not see end.
That is one started from a conversation -- not by Superchat at a time, not
delegated by another execution, not asked from a buffer in place -- whose
conversation has no live View now."
  (and (not (member (plist-get status :actor-id)
                    superchat-changes--unattended-actors))
       (null (plist-get status :parent-run-id))
       (eq 'chat (plist-get status :surface))
       (not (superchat-changes--conversation-open-p
             (plist-get status :session-id)))))

(defun superchat-changes--note-unattended (_run status)
  "At the end of an execution nobody watched, see that it is left to look at.
One Superchat started by itself is listed by what its start recorded; every
way of ending counts, and the one that failed is the one most easily
missed.  One the user started is listed when its conversation was closed
by the time it ended, which the fact `run.ended-unwatched' records here.
The notices are read from the Tape; what is kept here is only how many
buffers the execution left unsaved, which the Tape does not say."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status)))
    (when-let* ((session (plist-get status :session-id))
                (run (plist-get status :run-id)))
      (let ((own (superchat-changes--unattended-run-p session run)))
        (when (and (not own)
                   (superchat-changes--ended-unwatched-p status)
                   (not (cl-find run superchat-changes--notices
                                 :key (lambda (n) (plist-get n :run))
                                 :test #'equal)))
          ;; Written before the read below, which is what lists it.
          (condition-case err
              (progn
                (superchat-tape-append-event
                 (list :type "run.ended-unwatched"
                       :session-id session
                       :run-id run
                       :actor-id "user"
                       :summary (format "Ended with its conversation closed: %s"
                                        (plist-get status :status))
                       :payload (append
                                 (list :run-id run
                                       :status (symbol-name
                                                (plist-get status :status)))
                                 (when-let* ((budget (plist-get status :tool-budget)))
                                   (list :tool-budget budget)))))
                (setq own t))
            (error
             (message "superchat: that %s ended unwatched was not recorded: %s"
                      run (error-message-string err)))))
        (when own
          (unless (gethash run superchat-changes--unsaved-at-end)
            (puthash run (length (ignore-errors
                                   (superchat-changes-candidates session run)))
                     superchat-changes--unsaved-at-end))
          (superchat-changes-notices-reload))))))

(defun superchat-changes-notice-dismiss (run)
  "Record that the user looked at the ended execution RUN; forget its notice.
That is the fact `run.seen' in the execution's conversation.  When it
cannot be written the notice stays, and the echo area says so.  Return
non-nil when the notice is gone."
  (let ((notice (cl-find run superchat-changes--notices
                         :key (lambda (n) (plist-get n :run))
                         :test #'equal)))
    (when notice
      (condition-case err
          (progn
            (superchat-tape-append-event
             (list :type "run.seen"
                   :session-id (plist-get notice :session)
                   :run-id run
                   :actor-id "user"
                   :summary (format "Looked at: %s, %s"
                                    (plist-get notice :what)
                                    (plist-get notice :status))
                   :payload (list :run-id run)))
            (remhash run superchat-changes--unsaved-at-end)
            (superchat-changes-notices-reload)
            t)
        (error
         (message "superchat: that %s was looked at could not be recorded: %s"
                  run (error-message-string err))
         nil)))))

(defun superchat-changes--notices-entries ()
  "Rows of the notices list, newest first."
  (mapcar (lambda (notice)
            (list (plist-get notice :run)
                  (vector (format-time-string "%m-%d %H:%M" (plist-get notice :time))
                          (plist-get notice :what)
                          (superchat-changes--notice-ending notice)
                          (let ((n (plist-get notice :unsaved)))
                            (if (> n 0) (format "%d unsaved" n) ""))
                          (plist-get notice :session))))
          (superchat-changes--own-notices)))

(defun superchat-changes-notices-open (run)
  "Go to the conversation of the execution RUN and forget its notice."
  (interactive (list (tabulated-list-get-id)))
  (when-let* ((notice (cl-find run superchat-changes--notices
                               :key (lambda (n) (plist-get n :run))
                               :test #'equal))
              (session (plist-get notice :session)))
    (let ((view (or (and (fboundp 'superchat-editor-workspace-buffer)
                         (let ((buffer (superchat-editor-workspace-buffer
                                        session 'session-view)))
                           (and (buffer-live-p buffer) buffer)))
                    (superchat--render-session-view session))))
      (superchat-changes-notice-dismiss run)
      (pop-to-buffer view))))

(defun superchat-changes-notices-dismiss (run)
  "Forget the notice at point without going anywhere."
  (interactive (list (tabulated-list-get-id)))
  (when run (superchat-changes-notice-dismiss run)))

(defun superchat-changes-notices-dismiss-all ()
  "Record every listed execution as looked at."
  (interactive)
  (dolist (notice (superchat-changes--own-notices))
    (superchat-changes-notice-dismiss (plist-get notice :run))))

(defvar superchat-changes-notices-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'superchat-changes-notices-open)
    (define-key map (kbd "d") #'superchat-changes-notices-dismiss)
    (define-key map (kbd "D") #'superchat-changes-notices-dismiss-all)
    map)
  "Keys of the notices list.")

(define-derived-mode superchat-changes-notices-mode tabulated-list-mode
  "Superchat-Notices"
  "Executions Superchat started by itself that have ended.
RET goes to the conversation and forgets the notice, d forgets it,
D forgets all."
  (setq tabulated-list-format [("Ended" 12 t) ("Started by" 40 t) ("Outcome" 12 t)
                               ("Left" 10 t) ("Conversation" 0 t)])
  (setq tabulated-list-entries #'superchat-changes--notices-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun superchat-changes-notices ()
  "List the executions Superchat started by itself that have ended.
A scheduled task or a Work Record that woke leaves one notice when its
execution ends, however it ended; the mode line shows how many are pending."
  (interactive)
  (let ((buffer (get-buffer-create superchat-changes-notices-buffer-name)))
    (with-current-buffer buffer
      (superchat-changes-notices-mode)
      (tabulated-list-print))
    (pop-to-buffer buffer)
    buffer))

(defvar superchat-timer-started-functions)
(declare-function superchat-runtime-status "superchat-runtime" (run-id))

(defun superchat-changes--note-started-late (session run)
  "Leave RUN's notice when it ended before it was on record as a task's.
A scheduled task's `timer.started' is written after the send returns; an
execution that ended inside that send was not yet known to be Superchat's
own when it ended.  Called with SESSION and RUN once the fact is written."
  (when-let* ((status (and (fboundp 'superchat-runtime-status)
                           (superchat-runtime-status run))))
    (superchat-changes--note-unattended
     nil (append (list :session-id session :run-id run) status))))

(superchat-define-plugin changes
  :doc "The unsaved-changes outlet's Run-end notice and review screen, and the
notices of work Superchat started by itself.  The /changes command is
registered with the other built-in commands by `chat-commands'."
  :apply
  (lambda (scope _)
    (superchat-on scope 'turn-completed 'changes-notice
                  #'superchat-changes--on-turn-completed)
    (superchat-on scope 'turn-completed 'changes-unattended-notice
                  #'superchat-changes--note-unattended)
    ;; What ended, or was cut off, in an earlier Emacs is still to be seen.
    (condition-case err
        (superchat-changes-notices-reload)
      (error (message "superchat: the notices could not be read: %s"
                      (error-message-string err))))
    ;; One hook however many installations: the last one out removes it.
    (superchat-scope-shared-effect
     scope 'changes-task-started
     (lambda ()
       (add-hook 'superchat-timer-started-functions
                 #'superchat-changes--note-started-late))
     (lambda ()
       (remove-hook 'superchat-timer-started-functions
                    #'superchat-changes--note-started-late)))))

(provide 'superchat-changes)

;;; superchat-changes.el ends here
