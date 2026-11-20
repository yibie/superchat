;;; superchat-chat.el --- The chat buffer and slash commands -*- lexical-binding: t; -*-

;;; Commentary:
;; The buffer is an Org rendering of the `conversation' view of one timeline,
;; then a prompt and an input area.  The view is shown turn by turn, and the
;; current turn is drawn again from its entries as each comes, so rendering
;; the whole view again gives the same text.  What a turn says is made in
;; `superchat-turn', from its entries.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-permission)
(require 'superchat-agent)
(require 'superchat-recall)
(require 'superchat-timer)
(require 'superchat-plugin)
(require 'superchat-turn)

(defvar-local superchat-chat--tape nil "Name of this buffer's timeline.")
(defvar-local superchat-chat--model nil "Model for this buffer, or nil for the default.")
(defvar-local superchat-chat--pending-scope nil
  "Scope given to `superchat-chat-open', for a timeline with no entry yet.")
(defvar-local superchat-chat--origin nil "Origin for the next request sent, or nil.")
(defvar-local superchat-chat--turn nil "Marker: where the current turn's text starts.")
(defvar-local superchat-chat--entries nil "The entries of the current turn, in order.")
(defvar-local superchat-chat--stream nil "Marker: where streamed text starts, under the Assistant heading.")
(defvar-local superchat-chat--stream-end nil "Marker: where streamed text ends and the rest of the turn starts.")
(defvar-local superchat-chat--block nil "Marker: where the separator and the prompt start.")
(defvar-local superchat-chat--input nil "Marker: where the input area starts.")
(defvar-local superchat-chat--waiting nil "Overlay of the waiting line, above the separator.")
(defvar-local superchat-chat--timer nil "Timer that keeps the waiting line and the mode line current.")
(defvar-local superchat-chat--began nil "When the timeline became busy.")
(defvar-local superchat-chat--queue nil "Texts sent while the timeline was busy, the newest first.")
(defvar-local superchat-chat--steered nil "Texts given to the run that it has not taken yet, oldest first.")
(defvar-local superchat-chat--force nil "Non-nil: the queue goes when the run ends, however it ends.")
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
  "The scope of this buffer.  A chat buffer has its timeline's, else the one it
was opened with; any other buffer that of `default-directory'."
  (if superchat-chat--tape
      (or (plist-get (car (superchat-tape-entries :tape superchat-chat--tape :limit 1)) :scope)
          superchat-chat--pending-scope)
    (superchat-tape-scope-of default-directory)))

(defun superchat-chat--origin-here ()
  "The origin of a request made from the current buffer, at point."
  (append (list :buffer (buffer-name))
          (and buffer-file-name (list :file buffer-file-name))
          (list :mode (symbol-name major-mode) :line (line-number-at-pos))))

(defun superchat-chat--folds (from to)
  "The (POSITION KEY FOLDED) of each heading between FROM and TO, FOLDED being
how it is shown by default."
  (let ((at from) folds)
    (while (setq at (text-property-not-all at to 'superchat-turn-fold nil))
      (push (cons at (get-text-property at 'superchat-turn-fold)) folds)
      (setq at (1+ at)))
    folds))

(defun superchat-chat--body (at)
  "The (START . END) of the body of the heading at AT as Org reckons it in the
text now, ending before the first line that belongs to no heading as deep, or
nil when it has none."
  (save-excursion
    (goto-char at)
    (let* ((level (org-current-level))
           (start (line-end-position))
           (stop start))
      ;; The separator and the input are no heading's body either.
      (while (and (< stop (min (point-max) superchat-chat--block))
                  (not (let ((loose (get-text-property stop 'superchat-turn-loose)))
                         (and loose (<= loose level)))))
        (setq stop (next-single-property-change stop 'superchat-turn-loose nil
                                                (min (point-max) superchat-chat--block))))
      (save-restriction
        (narrow-to-region (point-min) stop)
        (org-end-of-subtree t t)
        (let ((end (if (and (bolp) (> (point) start)) (1- (point)) (point))))
          (and (> end start) (cons start end)))))))

(defun superchat-chat--choices (from to)
  "The (KEY . FOLDED) of each heading in sight between FROM and TO that the
person folded or opened, against the way it is shown by default."
  (cl-loop for (at key folded) in (superchat-chat--folds from to)
           for body = (superchat-chat--body at)
           for now = (and body (org-fold-folded-p (car body)) t)
           when (and body (not (invisible-p at)) (not (eq now folded)))
           collect (cons key now)))

(defun superchat-chat--fold (from to choices)
  "Fold, by its key, each heading between FROM and TO that folds by default or
by CHOICES: its body as Org reckons it now."
  (pcase-dolist (`(,at ,key ,folded) (superchat-chat--folds from to))
    (when-let* (((if-let* ((choice (assoc key choices))) (cdr choice) folded))
                (body (superchat-chat--body at)))
      (org-fold-region (car body) (cdr body) t 'outline))))

(defun superchat-chat--edit (function)
  "Call FUNCTION with the buffer writable.  The user's point and that of every
window showing the buffer keep their distance from the end in the input area,
their position above it."
  (let* ((inhibit-read-only t)
         ;; In the entries a position, for a turn drawn again; below them a
         ;; marker, which moves with what is inserted above it.
         (keep (lambda (at) (cond ((>= at superchat-chat--input) (- at (point-max)))
                                  ((< at superchat-chat--block) at)
                                  (t (copy-marker at)))))
         (here (funcall keep (point)))
         ;; A window not selected has a point of its own.  Its start is a
         ;; marker that an insertion before markers would carry down to the
         ;; prompt: it is kept as a position.
         (windows (mapcar (lambda (w) (list w (funcall keep (window-point w)) (window-start w)))
                          (get-buffer-window-list nil nil t))))
    (funcall function)
    (let ((back (lambda (at) (cond ((markerp at) (prog1 (marker-position at) (set-marker at nil)))
                                   ((<= at 0) (+ (point-max) at))
                                   (t (min at superchat-chat--input))))))
      (goto-char (funcall back here))
      (pcase-dolist (`(,w ,at ,start) windows)
        (set-window-point w (funcall back at))
        (set-window-start
         w (if (and (integerp at) (<= at 0))
               ;; Following the input: the end on the window's last line.
               (save-excursion (goto-char (point-max))
                               (vertical-motion (- 1 (window-body-height w)) w)
                               (point))
             (min start (point-max)))
         t)))))

(defun superchat-chat--put (at string &rest stay)
  "Insert STRING at marker AT, before every marker there but those of STAY."
  (let ((from (marker-position at)))
    (goto-char from)
    (insert-before-markers string)
    (dolist (marker stay) (when (= marker (point)) (set-marker marker from)))))

(defun superchat-chat--draw (&optional choices)
  "Draw the current turn again from its entries, keeping what streamed; a
heading the person folded or opened stays so, as do those of CHOICES."
  (let* ((choices (append choices (superchat-chat--choices superchat-chat--turn superchat-chat--block)))
         (streamed (buffer-substring superchat-chat--stream superchat-chat--stream-end))
         (text (superchat-turn-text superchat-chat--entries (not (string-empty-p streamed))
                                    (not (superchat-run-active superchat-chat--tape)))))
    (delete-region superchat-chat--turn superchat-chat--block)
    (superchat-chat--put superchat-chat--turn (car text) superchat-chat--turn)
    (superchat-chat--put superchat-chat--stream-end streamed
                         superchat-chat--turn superchat-chat--stream)
    (superchat-chat--put superchat-chat--block (cdr text)
                         superchat-chat--turn superchat-chat--stream superchat-chat--stream-end)
    (superchat-chat--fold superchat-chat--turn superchat-chat--block choices)))

(defconst superchat-chat--prompt
  (superchat-turn-seal (concat (superchat-turn-dim (make-string 40 ?─)) "\n"
                               (superchat-turn-dim "❯ " 'bold)))
  "The separator line and the prompt, between the entries and the input.")

(defun superchat-chat--render ()
  "Show the whole `conversation' view again, keeping the unsent input."
  (superchat-chat--edit
   (lambda ()
     (let ((input (buffer-substring superchat-chat--input (point-max)))
           (choices (superchat-chat--choices (point-min) superchat-chat--block))
           (turns nil))
       (setq superchat-chat--shown 0)
       (dolist (entry (superchat-view 'conversation :tape superchat-chat--tape))
         (setq superchat-chat--shown (max superchat-chat--shown (plist-get entry :id)))
         (if (or (not turns) (eq (plist-get entry :kind) 'user))
             (push (list entry) turns)
           (push entry (car turns))))
       (setq turns (nreverse (mapcar #'reverse turns))
             superchat-chat--entries (car (last turns)))
       (erase-buffer)
       (dolist (turn (butlast turns))
         (let ((text (superchat-turn-text turn nil t))) (insert (car text) (cdr text))))
       (dolist (marker (list superchat-chat--turn superchat-chat--stream
                             superchat-chat--stream-end superchat-chat--block))
         (set-marker marker (point)))
       (superchat-chat--fold (point-min) (point) choices)
       (insert superchat-chat--prompt)
       (set-marker superchat-chat--input (point))
       (insert input)
       (superchat-chat--draw choices)))))

(defun superchat-chat--line (text width)
  "TEXT on one line, cut to WIDTH characters."
  (let ((line (replace-regexp-in-string "[\n\t]+" " " text)))
    (substring line 0 (min width (length line)))))

(defun superchat-chat--name (tape)
  "The buffer name for timeline TAPE."
  (let ((first (car (superchat-tape-entries :tape tape :kinds '(user) :limit 1))))
    (format "*superchat: %s*" (if first (superchat-chat--line (plist-get first :text) 40) "new"))))

(defun superchat-chat--waiting-line (now)
  "The waiting line of the current turn at time NOW, its strip above it.
A question to the person looks like a slow tool unless it says so."
  (let* ((asking (and (equal (plist-get superchat-permission-asking :tape) superchat-chat--tape)
                      superchat-permission-asking))
         (strip (superchat-turn-strip superchat-chat--entries asking))
         (answered (delq nil (mapcar (lambda (e) (and (eq (plist-get e :kind) 'tool)
                                                      (plist-get (plist-get e :data) :call-id)))
                                     superchat-chat--entries)))
         (call (seq-find (lambda (c) (not (member (plist-get c :id) answered)))
                         (mapcan (lambda (e) (copy-sequence (plist-get (plist-get e :data) :calls)))
                                 superchat-chat--entries)))
         (last (car (last superchat-chat--entries)))
         (retry (and (equal (plist-get (plist-get last :data) :type) "retry")
                     (plist-get last :data))))
    (concat
     (unless (string-empty-p strip) (concat strip "\n"))
     (if asking
         (propertize (format "⏸ waiting for your answer (minibuffer): %s %s — y/n/a\n"
                             (plist-get asking :tool)
                             (superchat-turn-cut (plist-get asking :summary) 60))
                     'face 'warning)
       (if retry
           (propertize (format "⟳ trying again in %ds (%d/%d)\n"
                               (max 0 (ceiling (- (+ (plist-get last :at) (plist-get retry :wait)) now)))
                               (plist-get retry :attempt) (plist-get retry :of))
                       'face 'shadow)
       (propertize (format "%c %s %ds\n" (aref "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏" (mod (floor (* now 10)) 10))
                           (if call (format "%s running…" (plist-get call :name)) "thinking…")
                           (- now (or (plist-get last :at) now)))
                   'face 'shadow))))))

(defun superchat-chat--queued-line ()
  "The line that shows the text the run has not taken yet, else the queue, or
nil when both are empty."
  (when-let* ((texts (or superchat-chat--steered (reverse superchat-chat--queue)))
              (first (car texts)))
    (let ((line (car (split-string first "\n"))))
      (propertize (format "%s %s: %s\n"
                          (if superchat-chat--steered "↪" "⏸")
                          (concat (if (cdr texts) (format "%d " (length texts)) "")
                                  (if superchat-chat--steered "for its next step" "queued"))
                          (if (string-search "\n" first)
                              (concat (substring line 0 (min 60 (length line))) "…")
                            (superchat-turn-cut line 60)))
                  'face 'shadow))))

(defun superchat-chat--stop ()
  "Cancel the timer of the buffer."
  (when superchat-chat--timer
    (cancel-timer superchat-chat--timer)
    (setq superchat-chat--timer nil)))

(defun superchat-chat--tick ()
  "Keep the buffer current while its timeline is busy: the entries as they
come, whichever run made them, and the state of the run in the lines."
  (superchat-chat--add nil))

(defvar-local superchat-chat--looked nil
  "When this buffer last looked for entries made while it sat still.")

(defun superchat-chat--watch ()
  "Look for entries of the timeline that came while this buffer sat still, at
most once a second; that starts the timer when the timeline is busy, so a run
this buffer did not start is followed too.  Nil, for the mode line."
  (when (and superchat-chat--tape (not superchat-chat--timer)
             (< (or superchat-chat--looked 0) (- (float-time) 1)))
    (setq superchat-chat--looked (float-time))
    (superchat-chat--tick))
  nil)

(defun superchat-chat--status ()
  "Show the state of the timeline in the header line, the mode line and the
waiting line; a timer keeps them current while the timeline is busy."
  (let ((run (superchat-run-active superchat-chat--tape))
        (now (float-time)))
    (if (not run)
        (superchat-chat--stop)
      (unless superchat-chat--timer
        (let ((buffer (current-buffer)))
          (setq superchat-chat--began now
                superchat-chat--timer
                (run-at-time 0.2 0.2 (lambda () (when (buffer-live-p buffer)
                                                  (with-current-buffer buffer
                                                    (superchat-chat--tick)))))))))
    (setq header-line-format
          (string-replace "%" "%%"
                          (concat (or superchat-chat--model superchat-llm-model "not set")
                                  " · " (symbol-name (superchat-permission-level superchat-chat--tape))
                                  " · " (abbreviate-file-name (superchat-chat--scope))
                                  (and run " · running")))
          mode-line-process (and run (format " [running %ds]" (- now superchat-chat--began))))
    (move-overlay superchat-chat--waiting superchat-chat--block superchat-chat--block)
    (overlay-put superchat-chat--waiting 'before-string
                 (when-let* ((lines (delq nil (list (and run (not (eq (plist-get run :status) 'handoff))
                                                         (superchat-chat--waiting-line now))
                                                    (when-let* ((question (and (not run)
                                                                               (superchat-question
                                                                                superchat-chat--tape))))
                                                      (format (propertize "⏸ waiting for your answer: %s — send %s\n"
                                                                          'face 'warning)
                                                              (plist-get question :text)
                                                              (superchat-chat-answer-keys 'warning)))
                                                    (superchat-chat--queued-line)))))
                   (apply #'concat lines)))
    (force-mode-line-update)))

(defun superchat-chat-answer-keys (&optional face)
  "The string \"y, n or a\", in FACE, with the three keys also in bold."
  (apply #'format (propertize "%s, %s or %s" 'face face)
         (mapcar (lambda (k) (propertize k 'face (delq nil (list 'bold face)))) '("y" "n" "a"))))

(defun superchat-chat--add (entry)
  "Show the entries of the timeline after the latest one shown, the entry just
appended among them: one that reached the Tape another way is shown too."
  (when (and (eq (plist-get entry :kind) 'user) (plist-get (plist-get entry :data) :steer))
    (pop superchat-chat--steered))
  (let ((new (superchat-tape-entries :tape superchat-chat--tape :after superchat-chat--shown)))
    (cond
     ;; A render for an anchor shows the rest of the list itself.
     ((seq-find (lambda (e) (eq (plist-get e :kind) 'anchor)) new) (superchat-chat--render))
     (new
      (superchat-chat--edit
       (lambda ()
         (dolist (entry new)
           (pcase (plist-get entry :kind)
             ;; Entries of the turn before that came with it are drawn first.
             ('user (when superchat-chat--entries (superchat-chat--draw))
                    (set-marker superchat-chat--turn superchat-chat--block)
                    (setq superchat-chat--entries nil))
             ;; The entry replaces what streamed; a retry discards it.
             ('assistant (delete-region superchat-chat--stream superchat-chat--stream-end))
             ('event (when (equal (plist-get (plist-get entry :data) :type) "retry")
                       (delete-region superchat-chat--stream superchat-chat--stream-end))))
           (setq superchat-chat--entries (append superchat-chat--entries (list entry))
                 superchat-chat--shown (plist-get entry :id)))
         (superchat-chat--draw)))))
    (when (seq-find (lambda (e) (eq (plist-get e :kind) 'user)) new)
      (rename-buffer (superchat-chat--name superchat-chat--tape) t)))
  (superchat-chat--status))

(defun superchat-chat--on-text (text)
  "Show streamed TEXT as it comes, raw, where the final answer goes."
  (superchat-chat--edit
   (lambda ()
     (let ((first (= superchat-chat--stream superchat-chat--stream-end)))
       (superchat-chat--put superchat-chat--stream-end
                            (superchat-turn-seal (superchat-turn-dim (copy-sequence text)))
                            superchat-chat--turn superchat-chat--stream)
       ;; The first text may bring the turn its Assistant heading.
       (when first (superchat-chat--draw))))))

(defun superchat-chat--unstream ()
  "Remove the streamed text that never became an entry."
  (superchat-chat--edit
   (lambda ()
     (delete-region superchat-chat--stream superchat-chat--stream-end)
     (superchat-chat--draw))))

(defun superchat-chat--busy ()
  "When the timeline has a run or a handoff, say so and return non-nil."
  (when-let* ((run (superchat-run-active superchat-chat--tape)))
    (message "A %s is still running; C-c C-k cancels it"
             (if (eq (plist-get run :status) 'handoff) "handoff" "request"))))

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
   ((superchat-chat--busy) nil)
   ((superchat-question superchat-chat--tape)
    (if-let* ((answer (cdr (assoc text '(("y" . allow) ("n" . deny) ("a" . always))))))
        (unwind-protect
            (progn (apply #'superchat-answer superchat-chat--tape answer
                          (superchat-chat--callbacks superchat-chat--tape))
                   t)
          (superchat-chat--status))
      (message "A call waits for your answer: send y, n or a")
      nil))
   (t
    (let ((tape superchat-chat--tape)
          (origin (or origin superchat-chat--origin)))
      (setq superchat-chat--origin nil)
      (unwind-protect
          (apply #'superchat-run superchat-chat--tape text
                 :origin origin :model superchat-chat--model
                 ;; The person reads the answer here: `send' is for runs
                 ;; nobody watches.
                 :tools (delete "send" (mapcar (lambda (s) (plist-get s :name))
                                               (superchat-tool-specs)))
                 (append (superchat-chat--callbacks tape)
                         ;; The first request of a timeline fixes its scope.
                         (and superchat-chat--pending-scope
                              (not (superchat-tape-entries :tape tape :limit 1))
                              (list :scope superchat-chat--pending-scope))))
        (superchat-chat--status))
      t))))

(defun superchat-chat--callbacks (tape)
  "The callbacks of a run of TAPE that this buffer shows."
  (list :on-text (superchat-chat--in tape #'superchat-chat--on-text)
        :on-entry (superchat-chat--in tape #'superchat-chat--add)
        :on-done (superchat-chat--in
                  tape (lambda (run)
                         (superchat-chat--unstream)
                         (superchat-chat--status)
                         (when (eq (plist-get run :status) 'error)
                           (message "Request failed: %s" (plist-get run :error)))
                         ;; Text the run never took goes as the queue would.
                         ;; It came before anything queued since.
                         (setq superchat-chat--steered nil
                               superchat-chat--queue (append superchat-chat--queue
                                                             (reverse (plist-get run :steers))))
                         (superchat-chat--release (eq (plist-get run :status) 'done))))))

(defun superchat-chat--enqueue (text)
  "Queue TEXT, sent when the timeline is free."
  (push text superchat-chat--queue)
  (superchat-chat--status))

(defun superchat-chat--release (sent)
  "The timeline is free: send the queue when SENT or forced, else put its
text back at the start of the input and say so."
  (when superchat-chat--queue
    (let ((text (string-join (reverse superchat-chat--queue) "\n\n"))
          (force superchat-chat--force))
      (setq superchat-chat--queue nil superchat-chat--force nil)
      (if (or sent force)
          (superchat-chat-send text)
        (superchat-chat--edit
         (lambda ()
           (goto-char superchat-chat--input)
           (insert text (if (= (point) (point-max)) "" "\n\n"))))
        (message "The queued request was not sent; it is back in the input"))
      (superchat-chat--status)))
  (setq superchat-chat--force nil))

(defun superchat-chat--send-input (&optional force)
  "Send the input area; keep it when nothing was sent.  While the timeline is
busy, give it to the run going, queue it behind a handoff, or with FORCE cancel
the run and send the queue and the input."
  (interactive "P")
  (let* ((buffer (current-buffer))
         (text (string-trim (buffer-substring-no-properties superchat-chat--input
                                                            (point-max))))
         (busy (superchat-run-active superchat-chat--tape))
         (clear (lambda () (with-current-buffer buffer
                             (delete-region superchat-chat--input (point-max))))))
    (cond
     ((and force busy)
      (unless (string-empty-p text) (superchat-chat--enqueue text))
      (funcall clear)
      (setq superchat-chat--force t)
      (superchat-chat--cancel))
     ((string-empty-p text) nil)
     ;; A run going takes it before its next request; a handoff queues it.
     ((and busy (eq (plist-get busy :status) 'running) (not (string-prefix-p "/" text)))
      (superchat-steer superchat-chat--tape text)
      (setq superchat-chat--steered (append superchat-chat--steered (list text)))
      (funcall clear)
      (superchat-chat--status))
     ((and busy (not (string-prefix-p "/" text)))
      (superchat-chat--enqueue text)
      (funcall clear))
     ;; The input goes only once the request is on its way: an error or a
     ;; refusal leaves it where it was.  Entries are inserted above the marker.
     ;; A command may have gone to another chat buffer: this one's input goes.
     ((superchat-chat-send text) (funcall clear)))))

(defun superchat-chat--return ()
  "Send on the last line of the input, else insert a newline."
  (interactive)
  (if (and (>= (point) superchat-chat--input) (= (line-end-position) (point-max)))
      (superchat-chat--send-input)
    (newline)))

(defun superchat-chat--cancel ()
  "Cancel the run of this timeline."
  (interactive)
  (if-let* ((run (superchat-run-active superchat-chat--tape)))
      ;; A cancelled handoff has no callback to show its event, nor to free
      ;; the queue: render it and free it here.
      (progn (superchat-cancel superchat-chat--tape)
             (superchat-chat--render)
             (superchat-chat--status)
             (when (eq (plist-get run :status) 'handoff) (superchat-chat--release nil)))
    (message "Nothing is running in this conversation")))

(defun superchat-chat--cycle ()
  "Org's cycling at point; the subtree of the last heading ends at the
separator, so the prompt and the input never fold."
  (interactive)
  (save-restriction
    (when (< (point) superchat-chat--block)
      (narrow-to-region (point-min) superchat-chat--block))
    (call-interactively #'org-cycle)))

(defun superchat-chat--self-insert ()
  "Insert the character typed; above the input area, at the end of the input."
  (interactive)
  (when (< (point) superchat-chat--input) (goto-char (point-max)))
  (call-interactively #'org-self-insert-command))

(defvar-keymap superchat-chat-mode-map
  "RET" #'superchat-chat--return
  "TAB" #'superchat-chat--cycle
  "C-c C-c" #'superchat-chat--send-input
  "C-c C-k" #'superchat-chat--cancel
  "<remap> <self-insert-command>" #'superchat-chat--self-insert)

(define-derived-mode superchat-chat-mode org-mode "Superchat"
  "Major mode of a Superchat conversation: entries above, input below."
  (setq superchat-chat--turn (point-min-marker)
        superchat-chat--stream (point-min-marker)
        superchat-chat--stream-end (point-min-marker)
        superchat-chat--block (point-min-marker)
        superchat-chat--input (point-min-marker)
        superchat-chat--waiting (make-overlay (point-min) (point-min)))
  ;; Each buffer keeps its own converted answers.
  (setq-local superchat-turn-answers (make-hash-table :test #'equal))
  ;; Input that grows past the window's bottom scrolls it a line, so the
  ;; input stays at the bottom instead of jumping to the middle.
  (setq-local scroll-conservatively 101 scroll-margin 0)
  (font-lock-add-keywords
   nil '((superchat-turn-cells (0 (get-text-property (match-beginning 0) 'superchat-turn-cell) t)))
   'append)
  (add-hook 'kill-buffer-hook #'superchat-chat--stop nil t)
  ;; A redisplay of the buffer is the one moment that costs nothing, and it is
  ;; when a timeline another run made busy is noticed.
  (setq mode-line-format (append mode-line-format '((:eval (superchat-chat--watch))))))

;;; Opening

(defun superchat-chat-open (tape &optional scope)
  "The chat buffer of timeline TAPE, made and rendered when there is none.
SCOPE is the scope of a timeline that has no entry yet."
  (or (superchat-chat--buffer tape)
      (with-current-buffer (generate-new-buffer "*superchat: new*")
        (superchat-chat-mode)
        (setq superchat-chat--tape tape
              superchat-chat--pending-scope (or scope (superchat-tape-scope-of default-directory)))
        (superchat-chat--render)
        (superchat-chat--status)
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

(defun superchat-chat--tape-here ()
  "The timeline of this buffer's own conversation, or nil when it has none.
A conversation belongs to the buffer it was started from: the chat buffer
itself, a chat buffer opened from here with nothing sent yet, else the most
recently active timeline of this scope that has a `user' entry whose origin is
this file, or this buffer when it visits none."
  (let ((origin (superchat-chat--origin-here)))
    (cl-flet ((mine (o) (if (plist-get origin :file)
                            (equal (plist-get o :file) (plist-get origin :file))
                          (and (null (plist-get o :file))
                               (equal (plist-get o :buffer) (plist-get origin :buffer))))))
      (or superchat-chat--tape
          (cl-some (lambda (b)
                     (with-current-buffer b
                       (and superchat-chat--tape superchat-chat--origin
                            (mine superchat-chat--origin)
                            (not (superchat-tape-entries :tape superchat-chat--tape :limit 1))
                            superchat-chat--tape)))
                   (buffer-list))
          (caar (superchat-tape-select
                 (concat "SELECT tape FROM entry WHERE scope = ? AND tape IN"
                         " (SELECT tape FROM entry WHERE kind = 'user' AND "
                         (if (plist-get origin :file)
                             "json_extract(data, '$.origin.file') = ?"
                           "json_extract(data, '$.origin.file') IS NULL AND json_extract(data, '$.origin.buffer') = ?")
                         ") AND tape NOT IN (SELECT tape FROM entry WHERE kind = 'user'"
                         " AND json_extract(data, '$.ephemeral') = 1)"
                         " GROUP BY tape ORDER BY MAX(id) DESC LIMIT 1")
                 (list (superchat-chat--scope)
                       (or (plist-get origin :file) (plist-get origin :buffer)))))))))

;;;###autoload
(defun superchat (&optional new)
  "Open the chat of this buffer's own conversation, or a new one when it has
none.  With a prefix argument NEW, always a new one."
  (interactive "P")
  (superchat-plugin-enable-configured)
  (let* ((origin (and (not superchat-chat--tape) (superchat-chat--origin-here)))
         (scope (superchat-chat--scope))
         (tape (or (and (not new) (superchat-chat--tape-here))
                   (superchat-tape-new-name)))
         (buffer (superchat-chat-open tape scope)))
    (when origin
      (with-current-buffer buffer (setq superchat-chat--origin origin)))
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

(defun superchat-chat--title (text)
  "The title of a note TEXT: its first 60 characters."
  (substring text 0 (min 60 (length text))))

;;;###autoload
(defun superchat-note (text)
  "Keep TEXT as a note of this buffer's scope, as the model's note tool does."
  (interactive (list (read-string "Note: ")))
  (when (string-empty-p (string-trim text)) (user-error "A note needs text"))
  (let* ((scope (superchat-chat--scope))
         (tape (or (superchat-chat-recent-tape scope) (superchat-tape-new-name))))
    (superchat-recall-note tape (superchat-chat--title text) text nil scope)
    (when-let* ((buffer (superchat-chat--buffer tape)))
      (with-current-buffer buffer (superchat-chat--add nil)))))

;;;###autoload
(defun superchat-recall (query)
  "Show what the Tape has on QUERY in this buffer's scope.
With no QUERY, show the notes."
  (interactive (list (read-string "Recall (empty for notes): ")))
  (let ((scope (superchat-chat--scope)))
    (if (string-empty-p query)
        (superchat-chat--show-entries "notes" (superchat-view 'notes :scope scope))
      (superchat-chat--show-entries
       (format "recall %s" query) (superchat-view 'search :query query :scope scope)))))

(defun superchat-chat--note (text)
  "Append TEXT as a note to this timeline and show it."
  (when (string-empty-p text) (user-error "/note needs text"))
  (superchat-recall-note superchat-chat--tape (superchat-chat--title text)
                         text nil (superchat-chat--scope))
  (superchat-chat--add nil))

(defun superchat-chat--switch (buffer)
  "Show chat BUFFER in the selected window, in place of this conversation.
`set-window-buffer', not `switch-to-buffer': a rule of the user's
`display-buffer-alist' for chat buffers must not send it to another window.
The window keeps its history, so a window tab line lists the conversations
it showed."
  (set-window-buffer nil buffer)
  (set-buffer buffer))

(defun superchat-chat--resume (name)
  "Open another timeline: the one called NAME, any scope, or one chosen from
this scope's `timelines' view with `completing-read'."
  (let ((name (and name (string-trim name))))
    (superchat-chat--switch
     (superchat-chat-open
      (if (and name (not (string-empty-p name)))
          (progn
            (unless (superchat-tape-entries :tape name :limit 1)
              (user-error "No timeline named %s" name))
            name)
        (let ((choices (mapcar (lambda (e)
                                 (cons (format "%s  [%s]"
                                               (superchat-chat--line (plist-get e :text) 60)
                                               (plist-get e :tape))
                                       (plist-get e :tape)))
                               (superchat-view 'timelines :scope (superchat-chat--scope)))))
          (unless choices (user-error "No timelines in this scope"))
          (cdr (assoc (completing-read "Resume: " choices nil t) choices))))))))

(defun superchat-chat--fork (_)
  "Fork at the entry at point and open the new timeline."
  (let ((id (if (>= (point) superchat-chat--input)
                (and (> superchat-chat--shown 0) superchat-chat--shown)
              (get-text-property (point) 'superchat-turn-id))))
    (unless id (user-error "No entry at point"))
    (superchat-chat--switch (superchat-chat-open (superchat-fork superchat-chat--tape id)))))

(defun superchat-chat--handoff (name)
  "Summarise the conversation into an anchor named NAME; refused when busy."
  (unless (superchat-chat--busy)
    (superchat-handoff superchat-chat--tape (and (not (string-empty-p name)) name)
                     (superchat-chat--in superchat-chat--tape
                                         (lambda (anchor)
                                           (superchat-chat--render)
                                           (superchat-chat--status)
                                           (message (if anchor "Handoff made" "Handoff failed"))
                                           (superchat-chat--release anchor)))))
  (superchat-chat--status))

(defun superchat-chat--level (name)
  "Show the permission level, or set it to NAME."
  (if (string-empty-p name)
      (message "Permission level: %s" (superchat-permission-level superchat-chat--tape))
    (superchat-permission-set superchat-chat--tape (intern name) (superchat-chat--scope))
    (superchat-chat--add nil)))

(defun superchat-chat--model-command (name)
  "Show the model of this buffer, or set it to NAME."
  (if (string-empty-p name)
      (message "Model: %s" (or superchat-chat--model superchat-llm-model "not set"))
    (let ((known (superchat-llm-model-ids)))
      (unless (member name known)
        (user-error "Unknown model %S of %s; one of: %s"
                    name superchat-llm-provider (string-join known ", "))))
    (setq superchat-chat--model name)
    (superchat-chat--status)
    (message "Model of this buffer: %s" name)))

(defun superchat-chat--help (_)
  "List the commands."
  (superchat-chat--show "help"
                        (mapconcat (lambda (c) (format "/%s  %s" (car c) (cddr c)))
                                   superchat-chat--commands "\n")))

(superchat-command-define
 "new" (lambda (_)
         (superchat-chat--switch (superchat-chat-open (superchat-tape-new-name) (superchat-chat--scope))))
 "Start a new timeline in this scope, in this window.")
(superchat-command-define "resume" #'superchat-chat--resume
                          "[name] Open another timeline of this scope, or the one named NAME.")
(superchat-command-define "fork" #'superchat-chat--fork
                          "Fork at the entry at point and open the new timeline.")
(superchat-command-define "handoff" #'superchat-chat--handoff
                          "[name] Summarise this conversation into an anchor.")
(superchat-command-define
 "topic" (lambda (name)
           (when (string-empty-p name) (user-error "/topic needs a name"))
           (superchat-chat--show-entries
            (concat "topic " name)
            (superchat-view 'topic :name name :scope (superchat-chat--scope))))
 "name. Show the entries of the topic.")
(superchat-command-define "recall" #'superchat-recall
                          "[query] Search the Tape of this scope; with none, show the notes.")
(superchat-command-define
 "notes" (lambda (_) (superchat-chat--show-entries
                      "notes" (superchat-view 'notes :scope (superchat-chat--scope))))
 "Show the notes of this scope.")
(superchat-command-define
 "timers" (lambda (arg)
            (if (string-match "\\`cancel +\\([0-9]+\\)\\'" arg)
                (message "%s" (superchat-timer-cancel (string-to-number (match-string 1 arg))))
              (superchat-chat--show
               "timers"
               (if-let* ((timers (superchat-timer-list)))
                   (mapconcat (lambda (e) (format "%d  %s  %s  %s\n" (plist-get e :id) (plist-get e :tape)
                                                  (string-trim (format "%S" (plist-get e :data)) "(" ")")
                                                  (plist-get e :text)))
                              timers "")
                 "No timers."))))
 "[cancel ID] Show the timers that may still fire, or cancel one.")
(superchat-command-define
 "dream" (lambda (arg)
           (let ((scope (superchat-chat--scope)))
             (if (equal arg "off")
                 (progn (superchat-timer-dream scope t) (message "Dreaming is off for %s" scope))
               (message "Dreaming is on for %s: timer %d, each day at 03:00" scope
                        (plist-get (superchat-timer-dream scope) :id)))))
 "[off] Review this scope's notes each night while nobody watches, or stop.")
(superchat-command-define "note" #'superchat-chat--note
                          "text. Keep TEXT as a note, as the note tool does.")
(superchat-command-define "level" #'superchat-chat--level
                          "[read-only|ask|auto] Show or set the permission level.")
(superchat-command-define "model" #'superchat-chat--model-command
                          "[name] Show or set the model of this buffer.")
(superchat-command-define "help" #'superchat-chat--help "List the commands.")

(provide 'superchat-chat)
;;; superchat-chat.el ends here
