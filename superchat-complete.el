;;; superchat-complete.el --- Completion at point, shown as ghost text -*- lexical-binding: t; -*-

;;; Commentary:
;; One request of `superchat-llm-request' per pause in typing, with no run
;; and nothing on the Tape: a completion is asked for at every pause, and
;; most are never taken.  Only the code around point is sent.

;;; Code:

(require 'subr-x)
(require 'superchat)
(require 'superchat-llm)

(defconst superchat-complete--delay 0.3
  "Seconds of idleness after an edit before a completion is asked for.")

(defconst superchat-complete--marker "<CURSOR>")

(defconst superchat-complete--system
  "You complete code in an editor. The user message is a piece of a file \
with the marker <CURSOR> where the cursor is. Reply with only the text to \
insert at <CURSOR>: no explanation, no code fence, no text that is already \
before or after the marker. Finish the current line or statement; at most \
5 lines. Keep the file's indentation and style. If nothing fits, reply with \
nothing.")

(defvar superchat-complete-mode)

(defvar-local superchat-complete--text nil
  "The suggestion shown at point, or nil; while non-nil its keys are on.")
(defvar-local superchat-complete--overlay nil)
(defvar-local superchat-complete--handle nil)
(defvar-local superchat-complete--timer nil)
(defvar-local superchat-complete--tick nil
  "The buffer's modification tick after the last command.")

(defvar superchat-complete--map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'superchat-complete-accept)
    (define-key map (kbd "<tab>") #'superchat-complete-accept)
    (define-key map (kbd "C-g") #'superchat-complete-dismiss)
    map)
  "Keys while a suggestion is shown.")

;; An empty overlay's `keymap' never applies at point, so the keys are an
;; emulation map: above the minor modes, whose TAB (company, corfu,
;; yasnippet) would come first otherwise.
(defvar superchat-complete--emulation
  `((superchat-complete--text . ,superchat-complete--map)))
(add-to-list 'emulation-mode-map-alists 'superchat-complete--emulation)

(defun superchat-complete--region ()
  "Start and end of the text sent.  In a Lisp mode: the top-level form at
point and the two before it, to the line that starts the next one; the
form being typed is unbalanced, so its end is that line, not `forward-sexp'.
Elsewhere: 60 lines before point and 20 after."
  (if (derived-mode-p 'lisp-data-mode)
      (cons (save-excursion
              (end-of-line)
              (dotimes (_ 3) (re-search-backward "^\\s(" nil 'move))
              (point))
            (save-excursion
              (forward-line 1)
              (if (re-search-forward "^\\s(" nil t) (line-end-position) (point-max))))
    (cons (save-excursion (forward-line -60) (point))
          (save-excursion (forward-line 20) (line-end-position)))))

(defun superchat-complete--prompt ()
  "The user message for a completion at point."
  (let ((region (superchat-complete--region)))
    (format "File: %s (%s)\n\n%s%s%s"
            (if buffer-file-name (file-name-nondirectory buffer-file-name) (buffer-name))
            major-mode
            (buffer-substring-no-properties (car region) (point))
            superchat-complete--marker
            (buffer-substring-no-properties (point) (cdr region)))))

(defun superchat-complete--clean (text before)
  "TEXT made fit to insert after BEFORE, the line's text before point."
  (let ((s (string-trim-right text))
        (typed (string-trim-left before)))
    ;; By lines, not one regexp over the answer, which overflows on a long one.
    (let ((lines (split-string s "\n")))
      (when (and (cdr lines) (string-prefix-p "```" (string-trim-left (car lines)))
                 (equal (string-trim (car (last lines))) "```"))
        (setq s (string-join (butlast (cdr lines)) "\n"))))
    (setq s (string-replace superchat-complete--marker "" s))
    ;; A model that writes the line again from its start.
    (when (and (> (length typed) 0) (string-prefix-p typed (string-trim-left s)))
      (setq s (substring (string-trim-left s) (length typed))))
    (if (string-match-p "[ \t]\\'" before) (string-trim-left s "[ \t]+") s)))

(defun superchat-complete-dismiss ()
  "Hide the suggestion and cancel the request in flight."
  (interactive)
  (when superchat-complete--overlay (delete-overlay superchat-complete--overlay))
  (setq superchat-complete--overlay nil superchat-complete--text nil)
  (when superchat-complete--handle
    (superchat-llm-cancel superchat-complete--handle)
    (setq superchat-complete--handle nil)))

(defun superchat-complete-accept ()
  "Insert the suggestion shown."
  (interactive)
  (when-let* ((text superchat-complete--text))
    (superchat-complete-dismiss)
    (insert text)))

(defun superchat-complete--show (text)
  "Show TEXT at point, dimmed, after the cursor."
  (superchat-complete-dismiss)
  (unless (string-empty-p text)
    (setq superchat-complete--overlay (make-overlay (point) (point) nil t)
          superchat-complete--text text)
    ;; Typing moves the overlay with point, so where it was shown is kept.
    (overlay-put superchat-complete--overlay 'superchat-at (point))
    (overlay-put superchat-complete--overlay 'after-string
                 (propertize text 'face 'shadow 'cursor 0))))

;;;###autoload
(defun superchat-complete ()
  "Ask for a completion at point and show it as ghost text."
  (interactive)
  (superchat-complete-dismiss)
  (let ((buffer (current-buffer))
        (tick (buffer-chars-modified-tick))
        (point (point))
        (before (buffer-substring-no-properties (line-beginning-position) (point)))
        (over nil))
    (condition-case err
        (let ((handle
              (let ((superchat-llm-reasoning nil))
                (superchat-llm-request
                 (list (list :role 'user :text (superchat-complete--prompt)))
                 :system superchat-complete--system
                 :on-done
                 (lambda (response)
                   (setq over t)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (setq superchat-complete--handle nil)
                       ;; An answer for text that is no longer there is dropped.
                       (when (and (= tick (buffer-chars-modified-tick)) (= point (point)))
                         (superchat-complete--show
                          (superchat-complete--clean (plist-get response :text) before))))))
                 :on-error
                 (lambda (error)
                   (setq over t)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer (setq superchat-complete--handle nil)))
                   (message "Completion failed: %s" (plist-get error :message)))))))
          ;; A model that answers at once has no request left to cancel.
          (unless over (setq superchat-complete--handle handle)))
      (user-error
       (when superchat-complete-mode (superchat-complete-mode -1))
       (message "%s; superchat-complete-mode is off" (error-message-string err))))))

(defun superchat-complete--worth-asking-p ()
  "Non-nil where a completion is worth a request: not before a word or
symbol, and after a space or at the end of the line, closing delimiters
after point counting as its end."
  (and (not (minibufferp))
       (not (looking-at-p "\\sw\\|\\s_"))
       (or (memq (char-before) '(?\s ?\t))
           (looking-at-p "\\s)*[ \t]*$"))))

(defun superchat-complete--typed-through-p (changed)
  "Non-nil when the last command, which CHANGED the buffer, typed the next
character of the suggestion; the suggestion is then shown without it."
  (when-let* (((and changed superchat-complete--overlay))
              (text superchat-complete--text)
              ((= (point) (1+ (overlay-get superchat-complete--overlay 'superchat-at))))
              ((eq (char-before) (aref text 0))))
    (superchat-complete--show (substring text 1))
    t))

(defun superchat-complete--ask-later (buffer)
  "Ask in BUFFER when it is still the selected window's; timers run in any buffer."
  (when (and (buffer-live-p buffer) (eq buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (setq superchat-complete--timer nil)
      (superchat-complete))))

(defun superchat-complete--post-command ()
  "After a command: keep, drop or ask for a suggestion."
  ;; Any command that changed the buffer counts as typing, so modes that
  ;; remap `self-insert-command' (Org, electric pairs, paredit) work too.
  (let ((changed (not (eql superchat-complete--tick (buffer-chars-modified-tick)))))
    (setq superchat-complete--tick (buffer-chars-modified-tick))
    (unless (or (memq this-command '(nil superchat-complete-accept superchat-complete))
                (superchat-complete--typed-through-p changed))
      (superchat-complete-dismiss)
      (when superchat-complete--timer (cancel-timer superchat-complete--timer))
      (setq superchat-complete--timer
            (and changed (superchat-complete--worth-asking-p)
                 (run-with-idle-timer superchat-complete--delay nil
                                      #'superchat-complete--ask-later (current-buffer)))))))

;;;###autoload
(define-minor-mode superchat-complete-mode
  "Ask the model for a completion when typing pauses; show it as ghost text.
\\<superchat-complete--map>\\[superchat-complete-accept] inserts it, \
\\[superchat-complete-dismiss] hides it, and typing goes on past it."
  :lighter " SC"
  (if superchat-complete-mode
      (progn (setq superchat-complete--tick (buffer-chars-modified-tick))
             (add-hook 'post-command-hook #'superchat-complete--post-command nil t))
    (remove-hook 'post-command-hook #'superchat-complete--post-command t)
    (when superchat-complete--timer (cancel-timer superchat-complete--timer))
    (setq superchat-complete--timer nil)
    (superchat-complete-dismiss)))

(provide 'superchat-complete)
;;; superchat-complete.el ends here
