;;; superchat-butler.el --- What work nobody asked for concluded -*- lexical-binding: t; -*-

;;; Commentary:
;; Work nobody asked for -- a scheduled task, a Work Record taken up at its time --
;; runs in a conversation of its own.  When it ends, the program writes one line:
;; what it was, how it ended, the first line of its answer.  That line is a fact
;; on the butler's Session, shown in `*superchat butler*', and the same words are
;; inserted in the conversation the user is using, on no Tape (the doorbell).
;;
;; The first phase has no model: nothing here is read by one, and nothing outside
;; can start one
;; (docs/decisions/accepted/2026-10-31-work-nobody-asked-for-has-its-own-place.md).
;;
;; A timer's fixed command has no conversation and no model: its line is said
;; here too, by `superchat-butler-say', and `RET' shows what it printed
;; (docs/decisions/accepted/2026-10-31-a-command-that-runs-by-itself.md).
;;
;; The one way from such work to the work at hand is `superchat-butler-bring': what
;; it concluded goes at the start of the message being written in the conversation
;; in use.  Nothing is sent
;; (docs/decisions/accepted/2026-10-31-bringing-a-conclusion-into-your-message.md).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-plugin)
(require 'superchat-db)
(require 'superchat-tape)
(require 'superchat-tape-view)
(require 'superchat-changes)
(require 'ansi-color)

(declare-function superchat-work-get "superchat-work" (id))
(declare-function superchat-timer-command-stats "superchat-timer" ())
(declare-function superchat-timer-command-stats-words "superchat-timer" (stats &optional rule))
(declare-function superchat-fix-apply-texts "superchat-fix" (root paths patch))
(declare-function superchat-runtime--terminal-p "superchat-runtime" (status))
(declare-function superchat--insert-persistent-system-message "superchat-render"
                  (content &optional owned-response-marker))
(declare-function superchat--render-session-view "superchat" (session-id))
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role &optional create))
(declare-function superchat--response-region-ensure-view-prompt "superchat-render"
                  (&optional independent))
(declare-function superchat--display-session-view "superchat" (buffer session-id))
(declare-function superchat--workspace-main-view "superchat" (workspace &optional source))
(declare-function superchat--home-view "superchat" (workspace source))
(declare-function superchat--chat-view-p "superchat" (&optional buffer))
(declare-function superchat-workspace-home-config-p "superchat-workspace" (config))
(declare-function superchat-workspace-home "superchat-workspace" ())
(declare-function superchat--temporary-view "superchat" ())
(declare-function superchat--break-line "superchat" (line width))
(declare-function superchat--fit-window "superchat" (text buffer))
(defvar superchat-buffer-name)
(defvar superchat--session-id)
(defvar superchat--prompt-start)

(defconst superchat-butler-buffer-name "*superchat butler*")

(defconst superchat-butler-output-buffer-name "*superchat command output*")

(defconst superchat-butler-lines-shown 500
  "How many of the newest lines the butler shows.")

(defconst superchat-butler-text-width 200
  "How wide the first line of an answer may be in a line of the butler.")

(defconst superchat-butler-brought-chars 3000
  "How much of what a piece of work said goes in a message at most.")

(defconst superchat-butler-brought-heading "Butler's report (not my words):"
  "The first line of a message with a conclusion in it.
It is ordinary text so that the message does not open with a quote, and it tells
the model these are not the words of whoever sends the message.  It is short: it
stands on the heading line of the input area, in a window that may cut lines.")

(defvar superchat-butler--conclusions (make-hash-table :test #'equal)
  "Run id -> what its line says it concluded, or nil, for the Runs with a line.
The lists show the conclusion, and nothing drawing reads the Tape.")

;;;; The lines ---------------------------------------------------------------------

(defun superchat-butler--row-line (row)
  "The line a Tape ROW (META CREATED-AT) holds."
  (let* ((meta (ignore-errors
                 (superchat-tape-read-json-alist (or (nth 0 row) "{}") 'list)))
         (payload (alist-get 'payload meta)))
    (list :run (alist-get 'run-id payload)
          :session (alist-get 'session payload)
          :what (alist-get 'what payload)
          :status (let ((status (alist-get 'status payload)))
                    (and (stringp status) (intern status)))
          :ending (alist-get 'ending payload)
          :text (alist-get 'text payload)
          :time (superchat-changes--row-time (nth 1 row)))))

(defun superchat-butler-lines ()
  "The butler's newest lines, oldest first.
Each is a plist (:run :session :what :status :ending :text :time)."
  (let (lines)
    (dolist (row (superchat-db-tape-select
                  "SELECT meta, created_at FROM tape
                   WHERE session_id = ?
                     AND json_extract(meta, '$.event_type') = 'butler.line'
                   ORDER BY id DESC LIMIT ?"
                  (list superchat-butler-session-id superchat-butler-lines-shown)))
      (push (superchat-butler--row-line row) lines))
    lines))

(defun superchat-butler-line-of (run)
  "The butler's line of RUN, or nil."
  (when-let* ((row (car (superchat-db-tape-select
                         "SELECT meta, created_at FROM tape
                          WHERE session_id = ?
                            AND json_extract(meta, '$.event_type') = 'butler.line'
                            AND json_extract(meta, '$.\"payload\".\"run-id\"') = ?
                          ORDER BY id DESC LIMIT 1"
                         (list superchat-butler-session-id run)))))
    (superchat-butler--row-line row)))

(defun superchat-butler-line-text (line)
  "LINE in words: what it was, how it ended, what it said."
  (let ((text (plist-get line :text)))
    (format "%s %s%s"
            (plist-get line :what)
            (plist-get line :ending)
            (if (and (stringp text) (not (string-empty-p text)))
                (concat ": " text)
              ""))))

(defun superchat-butler-text-of (run)
  "What the line of RUN says the work concluded, or nil.  Reads no Tape."
  (gethash run superchat-butler--conclusions))

;;;; What a line says ---------------------------------------------------------------

(defun superchat-butler--short (text width)
  "The first line of TEXT that says anything, at most WIDTH wide; nil when none."
  (when (stringp text)
    (when-let* ((line (cl-find-if (lambda (line) (not (string-blank-p line)))
                                  (split-string text "\n"))))
      (truncate-string-to-width (string-trim line) width nil nil "…"))))

(defun superchat-butler--facts (session)
  "What started SESSION, from its Tape: (TASK-TEXT RECORD-ID MAIN-SESSION)."
  (let (task record main)
    (dolist (event (ignore-errors (superchat-tape-replay-events session)))
      (let ((payload (plist-get event :payload)))
        (pcase (plist-get event :type)
          ("timer.fired" (setq task (plist-get payload :text)))
          ("work.woken" (setq record (plist-get payload :work-id)
                              main (plist-get payload :main))))))
    (list task record main)))

(defun superchat-butler--what (session)
  "What the work of SESSION was, in words."
  (cl-destructuring-bind (task record _main) (superchat-butler--facts session)
    (cond
     (task (format "Scheduled task \"%s\"" (superchat-butler--short task 60)))
     (record (format "Work record %s \"%s\"" record
                     (superchat-butler--short
                      (or (plist-get (ignore-errors (superchat-work-get record)) :goal) "")
                      60)))
     (t "Scheduled work"))))

(defun superchat-butler--last-answer (session run)
  "The last thing the model said in RUN of SESSION, whole, or nil."
  (caar (superchat-db-tape-select
         "SELECT content FROM tape
          WHERE session_id = ? AND kind = 'assistant'
            AND json_extract(meta, '$.run_id') = ?
            AND content <> ''
          ORDER BY id DESC LIMIT 1"
         (list session run))))

(defun superchat-butler--answer (session run)
  "The first line of the last thing the model said in RUN of SESSION, or nil."
  (superchat-butler--short (superchat-butler--last-answer session run)
                           superchat-butler-text-width))

(defun superchat-butler--ending (run status)
  "How RUN, ended with STATUS, ended, in the words of the lists."
  (let ((notice (cl-find run superchat-changes--notices
                         :key (lambda (notice) (plist-get notice :run))
                         :test #'equal)))
    (cond
     ((and notice (or (plist-get notice :calls-cut-off)
                      (plist-get notice :calls-stopped)))
      (superchat-changes--notice-ending notice))
     ((eq status 'cut-off) "cut off when Emacs quit")
     ((eq status 'cancelled) "stopped")
     (t (format "%s" status)))))

(defun superchat-butler--append (line)
  "Record LINE, a plist (:run :session :what :status :ending :text), and show it."
  (superchat-tape-append-event
   (list :type "butler.line"
         :session-id superchat-butler-session-id
         :actor-id "butler"
         :summary (superchat-butler-line-text line)
         :payload (list :run-id (plist-get line :run)
                        :session (plist-get line :session)
                        :what (plist-get line :what)
                        :status (and (plist-get line :status)
                                     (symbol-name (plist-get line :status)))
                        :ending (plist-get line :ending)
                        :text (plist-get line :text))))
  (puthash (plist-get line :run) (plist-get line :text) superchat-butler--conclusions)
  (superchat-butler--show (plist-put line :time (current-time)))
  line)

(defun superchat-butler--write (session run status)
  "Write the line of RUN of SESSION, ended with STATUS; return it as a plist."
  (superchat-butler--append
   (list :run run :session session
         :what (superchat-butler--what session)
         :status status
         :ending (superchat-butler--ending run status)
         :text (superchat-butler--answer session run))))

(defun superchat-butler-say (id what ending text &optional width)
  "Say a line of work that has no conversation, and ring the doorbell.
ID names it for the lists and for `superchat-butler-show'; WHAT, ENDING and
TEXT are the words of the line, TEXT at most WIDTH wide (by default
`superchat-butler-text-width').  Return the line."
  (let ((line (superchat-butler--append
               (list :run id :session superchat-butler-session-id
                     :what what :ending ending
                     :text (superchat-butler--short
                            text (or width superchat-butler-text-width))))))
    (when (superchat-plugin-row-active-p 'butler)
      (superchat-butler--ring line nil))
    line))

(defun superchat-butler-cut-off (session run)
  "Say that RUN of SESSION was cut off when an earlier Emacs exited.
It has no answer and it is not started again; the user would hear of it nowhere
else.  Return the line."
  (let ((line (superchat-butler--append
               (list :run run :session session
                     :what (superchat-butler--what session)
                     :ending "cut off when Emacs exited, not started again"))))
    (when (superchat-plugin-row-active-p 'butler)
      (superchat-butler--ring line (list session)))
    line))

;;;; The buffer ---------------------------------------------------------------------

(defun superchat-butler--insert (line)
  (insert (propertize (format "%s  %s\n"
                              (format-time-string "%m-%d %H:%M" (plist-get line :time))
                              (superchat-butler-line-text line))
                      'superchat-butler-line line)))

(defun superchat-butler--show (line)
  "Add LINE to the butler's buffer, leaving a reader's place alone."
  (when-let* ((buffer (get-buffer superchat-butler-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (superchat-butler--insert line))))))

(defun superchat-butler-refresh ()
  "Read the butler's lines again."
  (interactive)
  (with-current-buffer (get-buffer-create superchat-butler-buffer-name)
    (unless (derived-mode-p 'superchat-butler-mode)
      (superchat-butler-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mapc #'superchat-butler--insert (superchat-butler-lines))
      (current-buffer))))

(defun superchat-butler--result (id)
  "What the fact `command.result' ID records, as an alist, or nil."
  (when-let* ((row (caar (superchat-db-tape-select
                          "SELECT meta FROM tape
                           WHERE session_id = ?
                             AND json_extract(meta, '$.event_type') = 'command.result'
                             AND json_extract(meta, '$.run_id') = ?
                           ORDER BY id DESC LIMIT 1"
                          (list superchat-butler-session-id id)))))
    (alist-get 'payload (superchat-tape-read-json-alist row 'list))))

(defvar-local superchat-butler--output-of nil
  "The id of the line whose command's output this buffer shows.")

(defun superchat-butler--compile-ending (payload)
  "What became of the compilation that failed that PAYLOAD records, in words."
  (let ((exit (alist-get 'exit payload))
        (why (alist-get 'why payload)))
    (concat
     (if exit (format "It failed with exit code %s." exit) "It failed.")
     (pcase (alist-get 'status payload)
       ("told" "")
       ("nothing" (format "  The model looked and had nothing to tell: %s"
                          (alist-get 'nothing payload)))
       ("failed" (format "  The model could not look: %s" why))
       ("stopped" "  You stopped the model.")
       ("replaced" (format "  The model stopped looking: %s" why))
       ("skipped" (format "  Not looked into: %s" why))
       (status (format "  %s" status))))))

(defvar superchat-butler-show-functions nil
  "How a rule's result of another kind is shown: (KIND . FUNCTION).
FUNCTION inserts the body, given the result's payload as an alist; what the
line is and how it went comes before it.")

(defun superchat-butler--show-other (payload function)
  "Insert the result PAYLOAD with FUNCTION, of `superchat-butler-show-functions'."
  (insert (format "%s\n\n" (alist-get 'what payload)))
  (when-let* ((timer (alist-get 'timer-id payload))
              (stats (gethash timer (superchat-timer-command-stats))))
    (insert (superchat-timer-command-stats-words stats t) "\n\n"))
  (funcall function payload))

(defun superchat-butler-show (id)
  "Show what the command of the line ID did; it counts as looked at."
  (let* ((payload (superchat-butler--result id))
         (other (cdr (assoc (alist-get 'kind payload) superchat-butler-show-functions)))
         (compile (equal "compile" (alist-get 'kind payload)))
         (output (alist-get 'output payload))
         (stderr (alist-get 'stderr payload))
         (bytes (alist-get 'bytes payload))
         (note (alist-get 'note payload))
         (no-note (alist-get 'note-failed payload))
         (buffer (get-buffer-create superchat-butler-output-buffer-name)))
    (unless payload
      (user-error "What this command printed is not on record"))
    (with-current-buffer buffer
      (superchat-butler-output-mode)
      (setq superchat-butler--output-of id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if other
            (superchat-butler--show-other payload other)
        (insert (format "%s\n$ %s\nin %s\n%s\n\n"
                        (alist-get 'what payload)
                        (alist-get 'command payload)
                        (alist-get 'directory payload)
                        (if compile
                            (superchat-butler--compile-ending payload)
                          (or (alist-get 'why payload)
                              (format "%s, exit %s" (alist-get 'status payload)
                                      (alist-get 'exit payload))))))
        (when-let* ((timer (alist-get 'timer-id payload))
                    (stats (gethash timer (superchat-timer-command-stats))))
          (insert (superchat-timer-command-stats-words stats compile) "\n\n"))
        (when (stringp note)
          (insert (cond
                   ((alist-get 'fix-tried payload)
                    "The model's words (it worked in a copy of this project):\n")
                   (compile
                      "The model's words (it can read this project and change nothing):\n")
                   (t
                    "The model's words (it read the output below and has no tools):\n"))
                  note "\n\n"
                  (if compile "" "What the command printed:\n")))
        (when-let* ((read (alist-get 'read payload)))
          (insert (format "It read: %s\n\n" (mapconcat #'identity read ", "))))
        (when-let* ((patch (alist-get 'fix-patch payload)))
          (insert "A fix that passes the build in a copy of the project.  Nothing of yours\n"
                  "has changed: `a' puts it in your buffers, unsaved, for you to review.\n\n"
                  patch "\n"))
        (when-let* ((dropped (alist-get 'fix-dropped payload)))
          (insert (format "Its change did not pass, or could not be offered, and was dropped: %s\n\n"
                          dropped)))
        (when-let* ((no-fix (alist-get 'no-fix payload)))
          (insert (format "It did not try a fix: %s\n\n" no-fix)))
        (when compile
          (insert "What the compilation printed:\n"))
        (when (stringp no-note)
          (insert (format "No note from the model: %s\n\n" no-note)))
        (insert (if (and (stringp output) (not (string-empty-p output)))
                    (ansi-color-filter-apply output)
                  "(no output)"))
        (when (and (numberp bytes) (stringp output) (> bytes (string-bytes output)))
          (insert (format "\n\n(the %s %d of %d bytes)"
                          (if compile "last" "first") (string-bytes output) bytes)))
        (when (and (stringp stderr) (not (string-empty-p stderr)))
          (insert "\n\nOn standard error:\n" (ansi-color-filter-apply stderr))))
        (insert "\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer)
    (superchat-changes-notice-dismiss id)
    buffer))

(defun superchat-butler-open ()
  "Go to what the line at point is about; its work counts as looked at.
That is its conversation, or what its command printed."
  (interactive)
  (let ((line (get-text-property (line-beginning-position) 'superchat-butler-line)))
    (unless line
      (user-error "There is no line here"))
    (superchat-butler-open-line line)))

(defun superchat-butler-open-line (line)
  "Go to what LINE is about; its work counts as looked at."
  (let ((session (plist-get line :session)))
      (if (equal session superchat-butler-session-id)
          (superchat-butler-show (plist-get line :run))
        (let ((view (or (let ((buffer (superchat-editor-workspace-buffer session 'session-view)))
                          (and (buffer-live-p buffer) buffer))
                        (superchat--render-session-view session))))
          (pop-to-buffer view)
          (superchat-changes-notice-dismiss (plist-get line :run))))))

;;;; Bringing it in -----------------------------------------------------------------

(defun superchat-butler--quotable (text)
  "TEXT such that what the user reads of it is what would be sent.
Escape sequences and control and invisible characters go, a tab and a newline
stay.  `[[' is split, or Org would hide the target of a link."
  (let ((clean (concat (cl-remove-if
                        (lambda (char)
                          (and (not (memq char '(?\n ?\t)))
                               (memq (get-char-code-property char 'general-category)
                                     '(Cc Cf Zl Zp))))
                        (ansi-color-filter-apply text)))))
    (while (string-match "\\[\\[" clean)
      (setq clean (replace-match "[ [" t t clean)))
    clean))

(defun superchat-butler--quoted (text width)
  "TEXT with every line marked as quoted (`superchat-core--quoted-at-p').
With WIDTH each is broken to be at most that wide, the mark included."
  (mapconcat (lambda (line)
               (if (string-empty-p line)
                   ">"
                 (mapconcat (lambda (piece) (concat "> " piece))
                            (if width
                                (superchat--break-line line (- width 2))
                              (list line))
                            "\n")))
             (split-string text "\n")
             "\n"))

(defun superchat-butler--cut (text)
  "TEXT, cut to `superchat-butler-brought-chars' with a note saying so."
  (if (<= (length text) superchat-butler-brought-chars)
      text
    (format "%s\n(the first %d of %d characters)"
            (substring text 0 superchat-butler-brought-chars)
            superchat-butler-brought-chars
            (length text))))

(defun superchat-butler--command-said (line)
  "All the command of LINE said, for a message.
Its line, the command itself, what it printed, and how much of that was kept:
the Tape holds the start of a long output."
  (let* ((payload (superchat-butler--result (plist-get line :run)))
         (output (alist-get 'output payload))
         (output (if (stringp output) (string-trim output) ""))
         (stderr (alist-get 'stderr payload))
         (bytes (alist-get 'bytes payload))
         (note (alist-get 'note payload))
         (patch (alist-get 'fix-patch payload))
         ;; The line holds its first line, cut to fit: when that is all of it,
         ;; the rest is what is left to say.
         (rest (when (stringp note)
                 (let ((trimmed (string-trim note))
                       (first (plist-get line :text)))
                   (if (and (stringp first)
                            (string-prefix-p (concat first "\n") (concat trimmed "\n")))
                       (string-trim (substring trimmed (length first)))
                     trimmed)))))
    (concat (superchat-butler-line-text line) "\n"
            (if (and rest (not (string-empty-p rest)))
                (concat (superchat-butler--cut rest) "\n")
              "")
            (if (stringp patch)
                (concat "A fix that passes the build in a copy; it is not applied:\n"
                        (superchat-butler--cut patch) "\n")
              "")
            ;; A rule on feeds runs no command: nothing to show for one.
            (let ((command (alist-get 'command payload)))
              (if (and (stringp command) (not (string-empty-p command)))
                  (concat "$ " command "\n"
                          (superchat-butler--cut output)
                          (if (and (numberp bytes) (> bytes (+ (string-bytes output) 1)))
                              (format "\n(%d bytes in all)" bytes)
                            ""))
                ""))
            (if (and (stringp stderr) (not (string-empty-p (string-trim stderr))))
                (concat "\nOn standard error:\n"
                        (superchat-butler--cut (string-trim stderr)))
              ""))))

(defun superchat-butler--said (line)
  "All the work of LINE said, for a message: what it is and how it ended first."
  (if (equal (plist-get line :session) superchat-butler-session-id)
      (superchat-butler--command-said line)
    (let ((answer (or (superchat-butler--last-answer (plist-get line :session)
                                                     (plist-get line :run))
                      (plist-get line :text))))
      (concat (format "%s %s" (plist-get line :what) (plist-get line :ending))
              (if (stringp answer)
                  (concat "\n" (superchat-butler--cut (string-trim answer)))
                "")))))

(defun superchat-butler--words (line view)
  "What LINE puts in a message in VIEW.
All of it can be read there: when VIEW's window cuts long lines, they are
broken.  That can split a long word, so it is not done where the window wraps
them."
  (let ((window (get-buffer-window view)))
    (concat superchat-butler-brought-heading "\n"
            (superchat-butler--quoted
             (superchat-butler--quotable (string-trim (superchat-butler--said line)))
             (and window
                  (buffer-local-value 'truncate-lines view)
                  (max 20 (1- (window-body-width window))))))))

(defun superchat-butler--work-origin (line)
  "The conversation the work of LINE belongs to: what it ran in or was set in."
  (let ((session (plist-get line :session)))
    (if (equal session superchat-butler-session-id)
        (alist-get 'set-in (superchat-butler--result (plist-get line :run)))
      session)))

(defun superchat-butler--home-view (line)
  "With no conversation in sight, the one to bring LINE into.
The main one of the project its work belongs to.  For work of Home, which has
no main conversation, the conversation of the source the work came from:
the newest one bound under the origin conversation's source key; when the
origin has no source (a conversation nobody opened), the conversation of
the buffer the user is in, else the origin itself.  The temporary one when
the work belongs to no Workspace."
  (let* ((origin (superchat-butler--work-origin line))
         (config (and (stringp origin) (superchat-view-session-configuration origin)))
         (workspace (plist-get config :workspace-id)))
    (cond
     ((and workspace (superchat-workspace-home-config-p config))
      (let* ((key (plist-get config :source-key))
             (session (and (stringp key)
                           (car (superchat-view-source-session-ids key workspace)))))
        (cond
         (session
          (or (superchat-editor-workspace-buffer session 'session-view)
              (superchat--render-session-view session)))
         ((and (buffer-live-p (window-buffer (selected-window)))
               (not (superchat--chat-view-p (window-buffer (selected-window))))
               (not (minibufferp (window-buffer (selected-window))))
               (not (string-prefix-p " " (buffer-name (window-buffer (selected-window))))))
          (superchat--home-view (superchat-workspace-home)
                                (window-buffer (selected-window))))
         (t
          (or (superchat-editor-workspace-buffer origin 'session-view)
              (superchat--render-session-view origin))))))
     (workspace
      (superchat--workspace-main-view
       (list :id workspace :root (plist-get config :workspace-root))))
     (t (superchat--temporary-view)))))

(defun superchat-butler--attached-what (line)
  "The one line that stands for what LINE brings in."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string
                 "[\n\t]+" " "
                 (format "%s %s: %s" (or (plist-get line :what) "")
                         (or (plist-get line :ending) "")
                         (or (plist-get line :text) ""))))
   160 nil nil "…"))

(defvar-local superchat-butler--attached nil
  "What is attached to the message being written in this conversation.
Plists (:run RUN :what WHAT :words WORDS [:sent FUNCTION]), oldest first.  WORDS
go at the start of the message when it is sent; until then the input area is
the user's (the user, 2026-11-06: in the input area it hid what they wrote).
WHAT and WORDS may be functions of no arguments, asked each time: what is
attached then follows what it stands for, as the user's notes do.  Empty WORDS
are not sent.  FUNCTION is called once WORDS went into the message.")

(defun superchat-butler--attached-part (one key)
  "The KEY of the attachment ONE, asked now when it is a function."
  (let ((value (plist-get one key)))
    (or (if (functionp value) (funcall value) value) "")))

(defun superchat-butler--attached-live (view)
  "What is attached to VIEW's message and has words now."
  (with-current-buffer view
    (cl-remove-if (lambda (one)
                    (string-empty-p (superchat-butler--attached-part one :words)))
                  superchat-butler--attached)))

(defvar-local superchat-butler--attached-overlay nil
  "The overlay that shows `superchat-butler--attached' above the input.")

(defvar-local superchat-butler--attached-whole nil
  "Non-nil when the attachments are shown whole, as they will be sent.")

(defun superchat-butler--prompt (view)
  "Where VIEW's input area starts, or nil."
  (with-current-buffer view
    (and (markerp superchat--prompt-start)
         (eq (marker-buffer superchat--prompt-start) view)
         (marker-position superchat--prompt-start))))

(defun superchat-butler--show-attached (view)
  "Show what is attached to VIEW's message, above the heading of its input area.
Not on the heading line: there it stood where the user writes (2026-11-06)."
  (with-current-buffer view
    (when (overlayp superchat-butler--attached-overlay)
      (delete-overlay superchat-butler--attached-overlay))
    (setq superchat-butler--attached-overlay nil)
    (let ((prompt (superchat-butler--prompt view)))
      (when (and (superchat-butler--attached-live view) prompt)
        ;; On the first character of the heading, advancing at its front:
        ;; what goes in before the heading (a doorbell, an answer) goes in
        ;; above it, and it stays right above the heading.
        (let* ((heading (save-excursion (goto-char prompt) (line-beginning-position)))
               (overlay (make-overlay heading (1+ heading) nil t nil)))
          (overlay-put overlay 'superchat-butler-attached t)
          (overlay-put
           overlay 'before-string
           (propertize
            (concat
             (mapconcat
              (lambda (one)
                (if superchat-butler--attached-whole
                    (concat (superchat-butler--attached-part one :words) "\n")
                  (format "Attached: %s\n" (superchat-butler--attached-part one :what))))
              (superchat-butler--attached-live view) "")
             (format "(sent at the start of your message; C-c C-b %s, C-u C-c C-b drops %s)\n"
                     (if superchat-butler--attached-whole "folds it" "shows all of it")
                     (if (cdr (superchat-butler--attached-live view)) "them" "it")))
            'face 'shadow))
          (setq superchat-butler--attached-overlay overlay))))))

(defun superchat-butler--attach (view run what words &optional sent)
  "Attach WORDS, from the work of RUN, to the message being written in VIEW.
WHAT is the one line that stands for them.  The same work twice is once.
SENT, a function, is called once WORDS went into the message.
Return non-nil when it was attached now."
  (prog1
      (with-current-buffer view
        (if (cl-find run superchat-butler--attached
                     :key (lambda (one) (plist-get one :run)) :test #'equal)
            (progn (message "superchat: that is attached already") nil)
          (setq superchat-butler--attached
                (append superchat-butler--attached
                        (list (append (list :run run :what what :words words)
                                      (and sent (list :sent sent))))))
          t))
    (superchat-butler--show-attached view)))

(defun superchat-butler-attached-p (view run)
  "Non-nil when the attachment RUN is on the message being written in VIEW."
  (and (buffer-live-p view)
       (cl-find run (buffer-local-value 'superchat-butler--attached view)
                :key (lambda (one) (plist-get one :run)) :test #'equal)
       t))

(defun superchat-butler-attached-refresh (run)
  "Show again every message RUN is attached to: what it stands for changed."
  (dolist (view (buffer-list))
    (when (superchat-butler-attached-p view run)
      (superchat-butler--show-attached view))))

(defun superchat-butler-attached-toggle (&optional drop)
  "Show what is attached to this message whole, or fold it again.
With a prefix argument DROP, take it off: nothing of it is sent."
  (interactive "P")
  (unless (superchat-butler--attached-live (current-buffer))
    (user-error "Nothing is attached to this message"))
  (if drop
      (progn (setq superchat-butler--attached nil
                   superchat-butler--attached-whole nil)
             (message "superchat: taken off; it is not sent"))
    (setq superchat-butler--attached-whole (not superchat-butler--attached-whole)))
  (superchat-butler--show-attached (current-buffer)))

(defun superchat-butler-message (&optional view)
  "What sending VIEW's message now would send.
What is attached, then what is written."
  (with-current-buffer (or view (current-buffer))
    (let ((prompt (superchat-butler--prompt (current-buffer))))
      (concat (mapconcat (lambda (one)
                           (concat (superchat-butler--attached-part one :words) "\n\n"))
                         (superchat-butler--attached-live (current-buffer)) "")
              (if prompt (buffer-substring-no-properties prompt (point-max)) "")))))

(defun superchat-butler-attach-into-input (view)
  "Put what is attached to VIEW's message at its start, as it is sent.
Called by `superchat-send-input' first: from here on the message is one text,
the one the conversation shows and the model gets."
  (with-current-buffer view
    (let ((prompt (superchat-butler--prompt view)))
      (when (and superchat-butler--attached prompt)
        (let* ((inhibit-read-only t)
               (live (superchat-butler--attached-live view))
               (words (mapconcat
                       (lambda (one)
                         (concat (superchat-butler--attached-part one :words) "\n\n"))
                       live "")))
          (save-excursion
            (goto-char prompt)
            (insert words))
          (setq superchat-butler--attached nil
                superchat-butler--attached-whole nil)
          (superchat-butler--show-attached view)
          (dolist (one live)
            (when-let* ((sent (plist-get one :sent)))
              (with-demoted-errors "superchat: after sending: %S"
                (funcall sent)))))))))

(defun superchat-butler--show-view (view)
  "Show VIEW and select its window."
  (if-let* ((window (get-buffer-window view)))
      (select-window window)
    (superchat--display-session-view
     view (buffer-local-value 'superchat--session-id view))))

(defun superchat-butler-bring (line &optional view)
  "Put what the work of LINE concluded in the message being written.
Nothing is sent.  That is in VIEW; without one, in the conversation in use, else
the main one of the Workspace the work belongs to.  It counts as looked at.  In
the work's own conversation nothing is quoted: the model there knows it already."
  (let* ((target (or view
                     (superchat-butler--conversation-in-use nil t)
                     (superchat-butler--home-view line)))
         (session (buffer-local-value 'superchat--session-id target))
         (own (equal session (plist-get line :session))))
    ;; Shown first: how wide the words may be is the window's.
    (superchat-butler--show-view target)
    (unless own
      (superchat-butler--attach target (plist-get line :run)
                                (superchat-butler--attached-what line)
                                (superchat-butler--words line target)))
    (with-current-buffer target
      (goto-char (point-max)))
    (unless own
      (superchat-tape-append-event
       (list :type "butler.brought"
             :session-id superchat-butler-session-id
             :run-id (plist-get line :run)
             :actor-id "user"
             :summary (format "Brought into %s: %s" session (plist-get line :what))
             :payload (list :run-id (plist-get line :run) :into session))))
    ;; What it points at was reached: only now is it looked at.
    (superchat-changes-notice-dismiss (plist-get line :run))
    (unless own
      (message "superchat: attached to your message, sent with it; C-c C-b shows it, nothing is sent yet"))
    target))

(defun superchat-butler-bring-from-here (line)
  "`superchat-butler-bring' LINE, from a list in the selected window.
You go on to write: the window of the list goes away.  First: going away
selects the window that was selected when it came."
  (when (with-current-buffer (window-buffer (selected-window))
          (derived-mode-p 'superchat-pending-mode 'superchat-butler-mode
                          'superchat-butler-output-mode))
    (quit-window))
  (superchat-butler-bring line))

(defun superchat-butler-bring-at-point ()
  "Put what the line at point concluded in the message you are writing."
  (interactive)
  (let ((line (get-text-property (line-beginning-position) 'superchat-butler-line)))
    (unless line
      (user-error "There is no line here"))
    (superchat-butler-bring-from-here line)))

(defun superchat-butler-bring-output ()
  "Put what the command shown here concluded in the message you are writing."
  (interactive)
  (let ((line (superchat-butler-line-of superchat-butler--output-of)))
    (unless line
      (user-error "Nothing here to bring in"))
    (superchat-butler-bring-from-here line)))

(defvar superchat-butler-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'superchat-butler-open)
    (define-key map (kbd "t") #'superchat-butler-bring-at-point)
    (define-key map (kbd "l") #'superchat-pending)
    (define-key map (kbd "g") #'superchat-butler-refresh)
    map)
  "Keys of the butler's buffer.")

(define-derived-mode superchat-butler-mode special-mode "Superchat-Butler"
  "What work nobody asked for concluded, oldest first.
RET goes to its conversation or shows what its command printed, t puts what it
concluded in the message you are writing, l shows what is under way and what
waits, g reads the lines again."
  (setq truncate-lines t)
  (setq header-line-format
        " RET look   t put it in your message   l what is pending   g read again"))

(defun superchat-butler--set-buffer-text (buffer bytes)
  "Make BUFFER hold BYTES, in its own coding, as one step of undo.
It is left unsaved; point and marks stay where they can."
  (with-current-buffer buffer
    (let ((text (decode-coding-string bytes (or buffer-file-coding-system 'utf-8-unix)))
          (source (generate-new-buffer " *superchat-fix-text*" t)))
      (unwind-protect
          (progn
            (with-current-buffer source (insert text))
            (save-restriction
              (widen)
              ;; `replace-region-contents' takes a buffer only from Emacs 31.
              (with-suppressed-warnings ((obsolete replace-buffer-contents))
                (replace-buffer-contents source))))
        (kill-buffer source)))))

;;;###autoload
(defun superchat-butler-apply-fix (id)
  "Put the fix of the result ID in the user's buffers, unsaved, and review it.
All of it or none: what no longer matches what is there now is refused."
  (interactive (list superchat-butler--output-of))
  (require 'superchat-fix)
  (let* ((payload (or (superchat-butler--result id)
                      (user-error "What this is about is not on record")))
         (patch (alist-get 'fix-patch payload))
         (paths (alist-get 'fix-paths payload))
         (root (alist-get 'fix-root payload)))
    (unless (stringp patch)
      (user-error "This has no fix to apply"))
    (let* ((texts (superchat-fix-apply-texts root paths patch))
           (buffers
            (mapcar (lambda (entry)
                      (let ((buffer (find-file-noselect
                                     (expand-file-name (car entry) root))))
                        (with-current-buffer buffer
                          (barf-if-buffer-read-only))
                        (cons buffer (cdr entry))))
                    texts))
           rows)
      (dolist (entry buffers)
        (let ((buffer (car entry)))
          (superchat-butler--set-buffer-text buffer (cdr entry))
          (superchat-changes-mark-buffer buffer superchat-butler-session-id id)
          (let ((file (buffer-file-name buffer)))
            (push (list :buffer buffer :name (buffer-name buffer) :file file
                        :new-file (not (file-exists-p file)) :outside nil)
                  rows))))
      (superchat-tape-append-event
       (list :type "butler.applied"
             :session-id superchat-butler-session-id
             :actor-id "butler"
             :summary (format "The fix of %s was put in %s" id
                              (mapconcat #'identity paths ", "))
             :requires-payload t
             :payload (list :run id :paths (vconcat paths))))
      (superchat-changes-notice-dismiss id)
      (superchat-changes-review (nreverse rows) t))))

(defvar superchat-butler-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'superchat-butler-bring-output)
    (define-key map (kbd "a") #'superchat-butler-apply-fix)
    map)
  "Keys of the buffer showing what a command printed.")

(define-derived-mode superchat-butler-output-mode special-mode "Superchat-Output"
  "What a timer's command printed.
t puts it in the message you are writing, a applies a fix when there is one.")

;;;###autoload
(defun superchat-butler ()
  "Show what work nobody asked for concluded, and return the buffer."
  (interactive)
  (let ((buffer (superchat-butler-refresh)))
    (pop-to-buffer buffer)
    ;; On the newest line, so that its keys work without moving first.
    (with-current-buffer buffer
      (goto-char (point-max))
      (forward-line -1))
    buffer))

;;;; The doorbell -------------------------------------------------------------------

(defun superchat-butler--conversation-in-use (except &optional foreground)
  "The chat View the user is using, unless its Session is in EXCEPT; or nil.
That is the one in the selected window, else one shown in this frame.  With
FOREGROUND a conversation nobody opened does not count, though it may be shown."
  (cl-some (lambda (window)
             (let* ((buffer (window-buffer window))
                    (session (buffer-local-value 'superchat--session-id buffer)))
               (and (buffer-local-value 'superchat-mode buffer)
                    (not (member session except))
                    (not (and foreground
                              (plist-get (superchat-view-session-configuration session)
                                         :background)))
                    buffer)))
           (cons (selected-window) (window-list nil 'no-minibuf))))

(defun superchat-butler--bring-button (line view)
  "The button that brings LINE into VIEW, as text.
A text button keeps its keys through Org: they come with its category, and Org
takes only a `keymap' on the text itself away."
  (with-temp-buffer
    (insert-text-button "[bring in]"
                        'follow-link t
                        'help-echo "Put this in your message; nothing is sent"
                        'action (lambda (_button) (superchat-butler-bring line view)))
    (buffer-string)))

(defun superchat-butler--see-button (line)
  "A button showing the fix of LINE."
  (with-temp-buffer
    (insert-text-button "[see fix]"
                        'follow-link t
                        'help-echo "See what it would change; nothing changes yet"
                        'action (lambda (_button) (superchat-butler-show (plist-get line :run))))
    (buffer-string)))

(defun superchat-butler--has-fix-p (line)
  "Non-nil when there is a fix to apply for LINE."
  (and (equal (plist-get line :session) superchat-butler-session-id)
       (stringp (alist-get 'fix-patch (superchat-butler--result (plist-get line :run))))))

(defun superchat-butler--doorbell (line view)
  "What the doorbell says of LINE in VIEW.
The buttons come first, where no window cuts them off, and the rest is broken to
fit: a chat View may cut long lines, and this is all there is to read of it
there."
  (let* ((fix (superchat-butler--has-fix-p line))
         (before (if fix
                     "* System: Butler: [bring in] [see fix] "
                   "* System: Butler: [bring in] "))
         (text (superchat-butler-line-text line))
         (fitted (superchat--fit-window (concat before text) view)))
    (concat "Butler: " (superchat-butler--bring-button line view) " "
            (if fix (concat (superchat-butler--see-button line) " ") "")
            ;; A window narrower than what comes before it breaks that too.
            (if (string-prefix-p before fitted)
                (substring fitted (length before))
              text))))

(defvar superchat-butler--unrung nil
  "Lines said while no conversation was in use, newest first: (LINE . EXCEPT).
The doorbell rings for them in the first conversation that is.")

(defun superchat-butler--ring-in (line view)
  "Put LINE in VIEW, on no Tape."
  (condition-case err
      (let ((superchat-buffer-name (buffer-name view)))
        (with-current-buffer view
          (superchat--insert-persistent-system-message
           (superchat-butler--doorbell line view))))
    (error (message "superchat: the butler could not say it in %s: %s"
                    (buffer-name view) (error-message-string err)))))

(defun superchat-butler--ring (line except)
  "Put LINE in the conversation in use, on no Tape; EXCEPT as above.
With none in use, it rings when one is: the inbox wakes Superchat when nobody
is in a conversation."
  (if-let* ((view (superchat-butler--conversation-in-use except)))
      (superchat-butler--ring-in line view)
    (push (cons line except) superchat-butler--unrung)
    (add-hook 'window-buffer-change-functions #'superchat-butler--ring-unrung)))

(defun superchat-butler--ring-unrung (&optional _frame)
  "Ring for the lines said while no conversation was in use, once one is.
On `window-buffer-change-functions' only while there are some."
  (let (left)
    (dolist (entry (reverse superchat-butler--unrung))
      (if-let* ((view (superchat-butler--conversation-in-use (cdr entry))))
          (superchat-butler--ring-in (car entry) view)
        (push entry left)))
    (setq superchat-butler--unrung left))
  (unless superchat-butler--unrung
    (remove-hook 'window-buffer-change-functions #'superchat-butler--ring-unrung)))

;;;; When work ends -----------------------------------------------------------------

(defun superchat-butler--on-end (_run status)
  "An execution ended with STATUS: when nobody asked for it, say what it concluded.
The doorbell does not ring in the conversation that ended."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status)))
    (when-let* ((session (plist-get status :session-id))
                (run (plist-get status :run-id))
                ((superchat-changes--unattended-run-p session run)))
      (superchat-butler--ring (superchat-butler--write session run (plist-get status :status))
                              (list session)))))

(defun superchat-butler-catch-up ()
  "Write the lines of what an earlier Emacs left: work it never saw end.
Those are the notices of work nobody asked for that have no line; the doorbell
does not ring for them."
  (dolist (line (superchat-butler-lines))
    (puthash (plist-get line :run) (plist-get line :text)
             superchat-butler--conclusions))
  (dolist (notice (reverse (superchat-changes--own-notices)))
    (let ((run (plist-get notice :run)))
      (when (eq 'none (gethash run superchat-butler--conclusions 'none))
        (superchat-butler--write (plist-get notice :session) run
                                 (plist-get notice :status))))))

(superchat-define-plugin butler
  :doc "What work nobody asked for concluded: the butler's lines and the doorbell."
  :apply
  (lambda (scope _)
    (superchat-on scope 'turn-completed 'butler-line #'superchat-butler--on-end)
    (condition-case err
        (superchat-butler-catch-up)
      (error (message "superchat: the butler could not read what ended earlier: %s"
                      (error-message-string err))))
    ;; The buffer is always there; it comes up only when asked for.
    (condition-case err
        (superchat-butler-refresh)
      (error (message "superchat: the butler's buffer could not be drawn: %s"
                      (error-message-string err))))))

(provide 'superchat-butler)

;;; superchat-butler.el ends here
