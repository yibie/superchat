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
(defvar-local superchat-chat--origin nil "Origin for the next request sent, or nil.")
(defvar-local superchat-chat--input nil "Marker: where the input area starts.")
(defvar-local superchat-chat--stream nil "Marker: where streamed text starts, or nil.")

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
    (add-text-properties 0 (length string)
                         (list 'read-only t 'rear-nonsticky t
                               'superchat-chat-id (plist-get entry :id))
                         string)
    string))

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
    (setq superchat-chat--stream nil)
    (erase-buffer)
    (dolist (entry (superchat-view 'conversation :tape superchat-chat--tape))
      (insert (superchat-chat--format entry)))
    (set-marker superchat-chat--input (point))
    (when input (insert input))
    (goto-char (- (point-max) tail))))

(defun superchat-chat--name (tape)
  "The buffer name for timeline TAPE."
  (let ((first (car (superchat-tape-entries :tape tape :kinds '(user) :limit 1))))
    (format "*superchat: %s*"
            (if first
                (truncate-string-to-width
                 (replace-regexp-in-string "[\n\t]+" " " (plist-get first :text)) 40)
              "new"))))

(defun superchat-chat--mode-line ()
  "Show [running] while the timeline has a run."
  (setq mode-line-process (and (superchat-run-active superchat-chat--tape) " [running]"))
  (force-mode-line-update))

(defun superchat-chat--add (entry)
  "Show ENTRY, which was just appended to the timeline."
  (superchat-chat--unstream)
  (if (eq (plist-get entry :kind) 'anchor)
      (superchat-chat--render)
    (superchat-chat--insert (superchat-chat--format entry)))
  (when (eq (plist-get entry :kind) 'user)
    (rename-buffer (superchat-chat--name superchat-chat--tape) t))
  (superchat-chat--mode-line))

(defun superchat-chat--on-text (text)
  "Show streamed TEXT as it comes."
  (unless superchat-chat--stream
    (setq superchat-chat--stream (copy-marker superchat-chat--input))
    (superchat-chat--insert (propertize "▸ Assistant\n" 'face 'bold 'read-only t
                                        'rear-nonsticky t)))
  (superchat-chat--insert (propertize text 'read-only t 'rear-nonsticky t)))

(defun superchat-chat--in (buffer function)
  "A function that calls FUNCTION in BUFFER, if BUFFER is still alive."
  (lambda (&rest args)
    (when (buffer-live-p buffer)
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
    (let ((buffer (current-buffer))
          (origin (or origin superchat-chat--origin)))
      (setq superchat-chat--origin nil)
      (unwind-protect
          (superchat-run superchat-chat--tape text
                         :origin origin :model superchat-chat--model
                         :on-text (superchat-chat--in buffer #'superchat-chat--on-text)
                         :on-entry (superchat-chat--in buffer #'superchat-chat--add)
                         :on-done (superchat-chat--in
                                   buffer (lambda (_run)
                                            (superchat-chat--unstream)
                                            (superchat-chat--mode-line))))
        (superchat-chat--mode-line))
      t))))

(defun superchat-chat--send-input ()
  "Send the input area; keep it when nothing was sent."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties superchat-chat--input
                                                           (point-max)))))
    (unless (string-empty-p text)
      (delete-region superchat-chat--input (point-max))
      (unless (superchat-chat-send text)
        (goto-char (point-max))
        (insert text)))))

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
      (superchat-cancel superchat-chat--tape)
    (message "No request is running")))

(defvar-keymap superchat-chat-mode-map
  "RET" #'superchat-chat--return
  "C-c C-c" #'superchat-chat--send-input
  "C-c C-k" #'superchat-chat--cancel)

(define-derived-mode superchat-chat-mode text-mode "Superchat"
  "Major mode of a Superchat conversation: entries above, input below."
  (setq superchat-chat--input (make-marker)))

;;; Opening

(defun superchat-chat-open (tape)
  "The chat buffer of timeline TAPE, made and rendered when there is none."
  (or (seq-find (lambda (b) (equal (buffer-local-value 'superchat-chat--tape b) tape))
                (buffer-list))
      (with-current-buffer (generate-new-buffer "*superchat: new*")
        (superchat-chat-mode)
        (setq superchat-chat--tape tape)
        (superchat-chat--render)
        (rename-buffer (superchat-chat--name tape) t)
        (current-buffer))))

(defun superchat-chat-recent-tape (scope)
  "The most recently active timeline of SCOPE that is not an inline one, or nil."
  (when-let* ((entry (seq-find (lambda (e) (not (string-prefix-p "inline:" (plist-get e :tape))))
                               (superchat-view 'timelines :scope scope :limit 100))))
    (plist-get entry :tape)))

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
         (buffer (superchat-chat-open tape)))
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

(defun superchat-chat--show-view (title argument &rest args)
  "Show the view that ARGS name under TITLE; ARGUMENT must not be empty."
  (when (string-empty-p argument) (user-error "/%s needs an argument" title))
  (let ((entries (apply #'superchat-view args)))
    (superchat-chat--show (format "%s %s" title argument)
                          (if entries (superchat-view-render entries) "No entries."))))

(defun superchat-chat--scope ()
  "The scope of this buffer."
  (superchat-tape-scope-of default-directory))

(defun superchat-chat--resume (_)
  "Choose a timeline of this scope and open it."
  (let ((choices (mapcar (lambda (e)
                           (cons (format "%s  [%s]"
                                         (truncate-string-to-width
                                          (replace-regexp-in-string "[\n\t]+" " "
                                                                    (plist-get e :text))
                                          60 nil nil "…")
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
  "Summarise the conversation into an anchor named NAME."
  (if (superchat-run-active superchat-chat--tape)
      (message "A request is still running; C-c C-k cancels it")
    (superchat-handoff superchat-chat--tape (and (not (string-empty-p name)) name)
                       (superchat-chat--in (current-buffer)
                                           (lambda (anchor)
                                             (superchat-chat--render)
                                             (message (if anchor "Handoff made" "Handoff failed")))))))

(defun superchat-chat--level (name)
  "Show the permission level, or set it to NAME."
  (if (string-empty-p name)
      (message "Permission level: %s" (superchat-permission-level superchat-chat--tape))
    (superchat-chat--add (superchat-permission-set superchat-chat--tape (intern name)))))

(defun superchat-chat--model-command (name)
  "Show the model of this buffer, or set it to NAME."
  (if (string-empty-p name)
      (message "Model: %s" (or superchat-chat--model superchat-llm-model "the provider's own"))
    (setq superchat-chat--model name)
    (message "Model of this buffer: %s" name)))

(defun superchat-chat--help (_)
  "List the commands."
  (superchat-chat--show "help"
                        (mapconcat (lambda (c) (format "/%s  %s" (car c) (cddr c)))
                                   superchat-chat--commands "\n")))

(superchat-command-define
 "new" (lambda (_) (pop-to-buffer (superchat-chat-open (superchat-tape-new-name))))
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
(superchat-command-define "level" #'superchat-chat--level
                          "[read-only|ask|auto] Show or set the permission level.")
(superchat-command-define "model" #'superchat-chat--model-command
                          "[name] Show or set the model of this buffer.")
(superchat-command-define "help" #'superchat-chat--help "List the commands.")

(provide 'superchat-chat)
;;; superchat-chat.el ends here
