;;; superchat-chat.el --- The chat buffer and slash commands -*- lexical-binding: t; -*-

;;; Commentary:
;; The buffer is a rendering of the `conversation' view of one timeline, plus
;; an input area.  Entries are appended to it one at a time as the run emits
;; them; rendering the whole view again gives the same text.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-permission)
(require 'superchat-agent)
(require 'superchat-recall)
(require 'superchat-plugin)

(defvar-local superchat-chat--tape nil "Name of this buffer's timeline.")
(defvar-local superchat-chat--model nil "Model for this buffer, or nil for the default.")
(defvar-local superchat-chat--pending-scope nil
  "Scope given to `superchat-chat-open', for a timeline with no entry yet.")
(defvar-local superchat-chat--origin nil "Origin for the next request sent, or nil.")
(defvar-local superchat-chat--input nil "Marker: where the input area starts.")
(defvar-local superchat-chat--stream nil "Marker: where streamed text starts, or nil.")
(defvar-local superchat-chat--shown 0 "Id of the latest entry shown; nothing at or below it is shown again.")

(defvar superchat-chat--commands nil
  "Alist of (NAME FUNCTION . DOC), NAME without the slash.")

(defun superchat-command-define (name function doc)
  "Define slash command NAME (a string without the slash).
FUNCTION is called with the rest of the line, in the chat buffer."
  (setq superchat-chat--commands
        (append (assoc-delete-all name superchat-chat--commands)
                (list (cons name (cons function doc))))))

(defun superchat-command-remove (name)
  "Remove slash command NAME."
  (setq superchat-chat--commands (assoc-delete-all name superchat-chat--commands)))

(defun superchat-chat--scope ()
  "The scope of this buffer: that of its timeline, else the one it was opened with."
  (or (plist-get (car (superchat-tape-entries :tape superchat-chat--tape :limit 1)) :scope)
      superchat-chat--pending-scope))

;;; Rendering

(defun superchat-chat--tool-line (entry)
  "The one line for tool entry ENTRY: its call and the first line of its result."
  (let* ((data (plist-get entry :data))
         (assistant (car (superchat-tape-entries
                          :tape (plist-get entry :tape) :kinds '(assistant)
                          :before (plist-get entry :id) :newest t :limit 1)))
         (call (seq-find (lambda (c) (equal (plist-get c :id) (plist-get data :call-id)))
                         (plist-get (plist-get assistant :data) :calls)))
         (args (if-let* ((plist (plist-get call :args)))
                   (string-trim (format "%S" plist) "(" ")")
                 ""))
         (args (if (> (length args) 60) (concat (substring args 0 60) "…") args)))
    (propertize (format "⚙ %s(%s) → %s\n" (plist-get data :name) args
                        (car (split-string (plist-get entry :text) "\n")))
                'face 'shadow)))

(defun superchat-chat--seal (string &optional id)
  "Make STRING read-only, carrying ID.  Text typed right after it is free:
only its last character is rear-nonsticky, so nothing can be inserted inside."
  (unless (string-empty-p string)
    (add-text-properties 0 (length string)
                         (list 'read-only t 'front-sticky '(read-only) 'superchat-chat-id id)
                         string)
    (add-text-properties (1- (length string)) (length string)
                         '(rear-nonsticky (read-only)) string))
  string)

(defun superchat-chat--format (entry)
  "The text of ENTRY as the buffer shows it, read-only and carrying its id."
  (let* ((text (string-trim-right (plist-get entry :text)))
         (string
          (pcase (plist-get entry :kind)
            ('user (concat (propertize "▸ You" 'face 'bold) "\n" text "\n\n"))
            ('assistant (if (string-empty-p text) ""
                          (concat (propertize "▸ Assistant" 'face 'bold) "\n" text "\n\n")))
            ('tool (superchat-chat--tool-line entry))
            ('anchor (concat (propertize (format "── summary: %s ──"
                                                 (plist-get (plist-get entry :data) :name))
                                         'face 'shadow)
                             "\n" text "\n\n"))
            (_ (propertize (concat text "\n") 'face 'shadow)))))
    (superchat-chat--seal string (plist-get entry :id))))

(defun superchat-chat--insert (string)
  "Insert the sealed STRING above the input area; the user's point stays put."
  (let ((inhibit-read-only t)
        (tail (and (>= (point) superchat-chat--input) (- (point-max) (point)))))
    (save-excursion
      (goto-char superchat-chat--input)
      (insert string)
      (set-marker superchat-chat--input (point)))
    (when tail (goto-char (- (point-max) tail)))))

(defun superchat-chat--unstream ()
  "Remove the streamed text, which the entry about to be shown replaces."
  (when superchat-chat--stream
    (let ((inhibit-read-only t))
      (delete-region superchat-chat--stream superchat-chat--input))
    (set-marker superchat-chat--stream nil)
    (setq superchat-chat--stream nil)))

(defun superchat-chat--render ()
  "Show the whole `conversation' view again, keeping the unsent input."
  (let* ((inhibit-read-only t)
         (placed (marker-position superchat-chat--input))
         (input (and placed (buffer-substring placed (point-max))))
         (tail (if (and placed (>= (point) placed)) (- (point-max) (point)) 0)))
    (setq superchat-chat--stream nil superchat-chat--shown 0)
    (erase-buffer)
    (dolist (entry (superchat-view 'conversation :tape superchat-chat--tape))
      (setq superchat-chat--shown (max superchat-chat--shown (plist-get entry :id)))
      (insert (superchat-chat--format entry)))
    (set-marker superchat-chat--input (point))
    (when input (insert input))
    (goto-char (- (point-max) tail))))

(defun superchat-chat--line (text width)
  "TEXT on one line, cut to WIDTH characters."
  (truncate-string-to-width (replace-regexp-in-string "[\n\t]+" " " text) width))

(defun superchat-chat--name (tape)
  "The buffer name for timeline TAPE."
  (let ((first (car (superchat-tape-entries :tape tape :kinds '(user) :limit 1))))
    (format "*superchat: %s*" (if first (superchat-chat--line (plist-get first :text) 40) "new"))))

(defun superchat-chat--mode-line ()
  "Show [running] while the timeline has a run."
  (setq mode-line-process (and (superchat-run-active superchat-chat--tape) " [running]"))
  (force-mode-line-update))

(defun superchat-chat--add (entry)
  "Show ENTRY, which was just appended to the timeline."
  (when (> (plist-get entry :id) superchat-chat--shown)
    (superchat-chat--unstream)
    (if (eq (plist-get entry :kind) 'anchor)
        (superchat-chat--render)
      (setq superchat-chat--shown (plist-get entry :id))
      (superchat-chat--insert (superchat-chat--format entry)))
    (when (eq (plist-get entry :kind) 'user)
      (rename-buffer (superchat-chat--name superchat-chat--tape) t)))
  (superchat-chat--mode-line))

(defun superchat-chat--on-text (text)
  "Show streamed TEXT as it comes."
  (unless superchat-chat--stream
    (setq superchat-chat--stream (copy-marker superchat-chat--input))
    (superchat-chat--insert (superchat-chat--seal (propertize "▸ Assistant\n" 'face 'bold))))
  (superchat-chat--insert (superchat-chat--seal (copy-sequence text))))

(defun superchat-chat--buffer (tape)
  "The chat buffer of timeline TAPE, or nil."
  (seq-find (lambda (b) (equal (buffer-local-value 'superchat-chat--tape b) tape))
            (buffer-list)))

(defun superchat-chat--in (tape function)
  "A function that calls FUNCTION in the chat buffer of TAPE as it is when
the function is called: a buffer killed and opened again gets the rest of a run."
  (lambda (&rest args)
    (when-let* ((buffer (superchat-chat--buffer tape)))
      (with-current-buffer buffer (apply function args)))))

;;; Sending

(defun superchat-chat-send (text &optional origin)
  "In a chat buffer, send TEXT as the user's request; ORIGIN goes on its entry.
A line starting with / is a command.  Return nil when nothing was sent."
  (unless superchat-chat--tape (user-error "Not a Superchat chat buffer"))
  (cond
   ((string-prefix-p "/" text)
    (if-let* ((command (and (string-match "\\`/\\([^ \t\n]*\\)[ \t\n]*" text)
                            (assoc (match-string 1 text) superchat-chat--commands))))
        (progn (funcall (cadr command) (string-trim (substring text (match-end 0)))) t)
      (message "Unknown command %s; /help lists them" (car (split-string text)))
      nil))
   ((superchat-run-active superchat-chat--tape)
    (message "A request is still running; C-c C-k cancels it")
    nil)
   (t
    (let ((tape superchat-chat--tape)
          (origin (or origin superchat-chat--origin)))
      (setq superchat-chat--origin nil)
      (unwind-protect
          (apply #'superchat-run superchat-chat--tape text
                         :origin origin :model superchat-chat--model
                         :on-text (superchat-chat--in tape #'superchat-chat--on-text)
                         :on-entry (superchat-chat--in tape #'superchat-chat--add)
                         :on-done (superchat-chat--in
                                   tape (lambda (_run)
                                            (superchat-chat--unstream)
                                            (superchat-chat--mode-line)))
                         ;; The first request of a timeline fixes its scope.
                         (and superchat-chat--pending-scope
                              (not (superchat-tape-entries :tape tape :limit 1))
                              (list :scope superchat-chat--pending-scope)))
        (superchat-chat--mode-line))
      t))))

(defun superchat-chat--send-input ()
  "Send the input area; keep it when nothing was sent."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties superchat-chat--input
                                                           (point-max)))))
    ;; The input goes only once the request is on its way: an error or a
    ;; refusal leaves it where it was.  Entries are inserted above the marker.
    (when (and (not (string-empty-p text)) (superchat-chat-send text))
      (delete-region superchat-chat--input (point-max)))))

(defun superchat-chat--return ()
  "Send on the last line of the input, else insert a newline."
  (interactive)
  (if (and (>= (point) superchat-chat--input) (= (line-end-position) (point-max)))
      (superchat-chat--send-input)
    (newline)))

(defun superchat-chat--cancel ()
  "Cancel the run of this timeline."
  (interactive)
  (if (superchat-run-active superchat-chat--tape)
      ;; A cancelled handoff has no callback to show its event: render it.
      (progn (superchat-cancel superchat-chat--tape)
             (superchat-chat--render)
             (superchat-chat--mode-line))
    (message "Nothing is running in this conversation")))

(defvar-keymap superchat-chat-mode-map
  "RET" #'superchat-chat--return
  "C-c C-c" #'superchat-chat--send-input
  "C-c C-k" #'superchat-chat--cancel)

(define-derived-mode superchat-chat-mode text-mode "Superchat"
  "Major mode of a Superchat conversation: entries above, input below."
  (setq superchat-chat--input (make-marker)))

;;; Opening

(defun superchat-chat-open (tape &optional scope)
  "The chat buffer of timeline TAPE, made and rendered when there is none.
SCOPE is the scope of a timeline that has no entry yet."
  (or (superchat-chat--buffer tape)
      (with-current-buffer (generate-new-buffer "*superchat: new*")
        (superchat-chat-mode)
        (setq superchat-chat--tape tape superchat-chat--pending-scope scope)
        (superchat-chat--render)
        (superchat-chat--mode-line)
        (rename-buffer (superchat-chat--name tape) t)
        (current-buffer))))

(defun superchat-chat-recent-tape (scope)
  "The most recently active timeline of SCOPE that is not ephemeral, or nil."
  ;; Not the `timelines' view: it answers with the first entry of each timeline
  ;; and has a limit, and this needs only the newest name.
  (caar (superchat-tape-select
         (concat "SELECT tape FROM entry WHERE scope = ? AND tape NOT IN"
                 " (SELECT tape FROM entry WHERE kind = 'user'"
                 " AND json_extract(data, '$.ephemeral') = 1)"
                 " GROUP BY tape ORDER BY MAX(id) DESC LIMIT 1")
         (list scope))))

;;;###autoload
(defun superchat (&optional new)
  "Open the chat of the most recent timeline of this buffer's project.
With a prefix argument NEW, start a new timeline."
  (interactive "P")
  (superchat-plugin-enable-configured)
  (let* ((origin (append (list :buffer (buffer-name))
                         (and buffer-file-name (list :file buffer-file-name))
                         (list :mode (symbol-name major-mode) :line (line-number-at-pos))))
         (tape (or (and (not new)
                        (superchat-chat-recent-tape (superchat-tape-scope-of default-directory)))
                   (superchat-tape-new-name)))
         (buffer (superchat-chat-open tape (superchat-tape-scope-of default-directory))))
    (with-current-buffer buffer
      (setq superchat-chat--origin origin))
    (pop-to-buffer buffer)))

;;; Commands

(defun superchat-chat--show (title text)
  "Show TEXT in a read-only buffer named after TITLE."
  (with-current-buffer (get-buffer-create (format "*superchat-view: %s*" title))
    (let ((inhibit-read-only t))
      (special-mode)
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

(defun superchat-chat--show-entries (title entries)
  "Show ENTRIES, rendered, in a read-only buffer named after TITLE."
  (superchat-chat--show title (if entries (superchat-view-render entries) "No entries.")))

(defun superchat-chat--show-view (title argument &rest args)
  "Show the view that ARGS name under TITLE; ARGUMENT must not be empty."
  (when (string-empty-p argument) (user-error "/%s needs an argument" title))
  (superchat-chat--show-entries (format "%s %s" title argument) (apply #'superchat-view args)))

(defun superchat-chat--title (text)
  "The title of a note TEXT: its first 60 characters."
  (substring text 0 (min 60 (length text))))

;;;###autoload
(defun superchat-note (text)
  "Keep TEXT as a note of this buffer's scope, as the model's note tool does."
  (interactive (list (read-string "Note: ")))
  (when (string-empty-p (string-trim text)) (user-error "A note needs text"))
  (let ((scope (superchat-tape-scope-of default-directory)))
    (superchat-recall-note (or (superchat-chat-recent-tape scope) (superchat-tape-new-name))
                           (superchat-chat--title text) text nil scope)))

;;;###autoload
(defun superchat-recall (query)
  "Show what the Tape has on QUERY in this buffer's scope.
With no QUERY, show the notes."
  (interactive (list (read-string "Recall (empty for notes): ")))
  (let ((scope (superchat-tape-scope-of default-directory)))
    (if (string-empty-p query)
        (superchat-chat--show-entries "notes" (superchat-view 'notes :scope scope))
      (superchat-chat--show-entries
       (format "recall %s" query) (superchat-view 'search :query query :scope scope)))))

(defun superchat-chat--note (text)
  "Append TEXT as a note to this timeline and show it."
  (when (string-empty-p text) (user-error "/note needs text"))
  (superchat-chat--add (superchat-recall-note superchat-chat--tape (superchat-chat--title text)
                                              text nil (superchat-chat--scope))))

(defun superchat-chat--resume (_)
  "Choose a timeline of this scope and open it."
  (let ((choices (mapcar (lambda (e)
                           (cons (format "%s  [%s]"
                                         (superchat-chat--line (plist-get e :text) 60)
                                         (plist-get e :tape))
                                 (plist-get e :tape)))
                         (superchat-view 'timelines :scope (superchat-chat--scope)))))
    (unless choices (user-error "No timelines in this scope"))
    (pop-to-buffer
     (superchat-chat-open
      (cdr (assoc (completing-read "Resume: " choices nil t) choices))))))

(defun superchat-chat--fork (_)
  "Fork at the entry at point and open the new timeline."
  (let ((id (or (get-text-property (point) 'superchat-chat-id)
                (and (> superchat-chat--input (point-min))
                     (get-text-property (1- superchat-chat--input) 'superchat-chat-id)))))
    (unless id (user-error "No entry at point"))
    (pop-to-buffer (superchat-chat-open (superchat-fork superchat-chat--tape id)))))

(defun superchat-chat--handoff (name)
  "Summarise the conversation into an anchor named NAME; an error when busy."
  (superchat-handoff superchat-chat--tape (and (not (string-empty-p name)) name)
                     (superchat-chat--in superchat-chat--tape
                                         (lambda (anchor)
                                           (superchat-chat--render)
                                           (superchat-chat--mode-line)
                                           (message (if anchor "Handoff made" "Handoff failed")))))
  (superchat-chat--mode-line))

(defun superchat-chat--level (name)
  "Show the permission level, or set it to NAME."
  (if (string-empty-p name)
      (message "Permission level: %s" (superchat-permission-level superchat-chat--tape))
    (superchat-chat--add (superchat-permission-set superchat-chat--tape (intern name)))))

(defun superchat-chat--model-command (name)
  "Show the model of this buffer, or set it to NAME."
  (if (string-empty-p name)
      (message "Model: %s" (or superchat-chat--model superchat-llm-model "not set"))
    (let ((known (superchat-llm-model-ids)))
      (unless (member name known)
        (user-error "Unknown model %S of %s; one of: %s"
                    name superchat-llm-provider (string-join known ", "))))
    (setq superchat-chat--model name)
    (message "Model of this buffer: %s" name)))

(defun superchat-chat--help (_)
  "List the commands."
  (superchat-chat--show "help"
                        (mapconcat (lambda (c) (format "/%s  %s" (car c) (cddr c)))
                                   superchat-chat--commands "\n")))

(superchat-command-define
 "new" (lambda (_)
         (pop-to-buffer (superchat-chat-open (superchat-tape-new-name) (superchat-chat--scope))))
 "Start a new timeline in this scope, in a new buffer.")
(superchat-command-define "resume" #'superchat-chat--resume "Open another timeline of this scope.")
(superchat-command-define "fork" #'superchat-chat--fork
                          "Fork at the entry at point and open the new timeline.")
(superchat-command-define "handoff" #'superchat-chat--handoff
                          "[name] Summarise this conversation into an anchor.")
(superchat-command-define
 "topic" (lambda (name) (superchat-chat--show-view "topic" name 'topic :name name
                                                   :scope (superchat-chat--scope)))
 "name. Show the entries of the topic.")
(superchat-command-define
 "recall" (lambda (query) (superchat-chat--show-view "recall" query 'search :query query
                                                     :scope (superchat-chat--scope)))
 "query. Search the Tape of this scope.")
(superchat-command-define
 "notes" (lambda (_) (superchat-chat--show-view "notes" "of this scope" 'notes
                                                :scope (superchat-chat--scope)))
 "Show the notes of this scope.")
(superchat-command-define "note" #'superchat-chat--note
                          "text. Keep TEXT as a note, as the note tool does.")
(superchat-command-define "level" #'superchat-chat--level
                          "[read-only|ask|auto] Show or set the permission level.")
(superchat-command-define "model" #'superchat-chat--model-command
                          "[name] Show or set the model of this buffer.")
(superchat-command-define "help" #'superchat-chat--help "List the commands.")

(provide 'superchat-chat)
;;; superchat-chat.el ends here
