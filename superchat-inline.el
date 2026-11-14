;;; superchat-inline.el --- Rewrite a region in place; ask about a region -*- lexical-binding: t; -*-

;;; Commentary:
;; The replacement is shown by the `display' property of an overlay over the
;; region, so the buffer text stays what it was until the user accepts.  The
;; run is asynchronous, but a scripted model answers inside `superchat-run',
;; so the overlay is made before the run starts.

;;; Code:

(require 'subr-x)
(require 'diff-mode)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-chat)

(defconst superchat-inline--system
  "Answer with the replacement text only: no explanation, no remarks, and no
code fence unless the fence is part of the replacement itself.")

(defun superchat-inline--origin ()
  "Where the request comes from: the current buffer and point."
  (list :buffer (buffer-name) :file buffer-file-name :mode (symbol-name major-mode)
        :line (line-number-at-pos)))

(defun superchat-inline--region ()
  "The text of the active region, or a `user-error'."
  (unless (use-region-p) (user-error "Select a region first"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun superchat-inline--unfence (text)
  "TEXT without a fenced code block that wraps all of it."
  (if (string-match "\\`[ \t\n]*```[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t\n]*\\'" text)
      (match-string 1 text)
    text))

(defun superchat-inline--show (overlay text)
  "Make OVERLAY display TEXT in place of the region and remember it."
  (overlay-put overlay 'superchat-text text)
  (overlay-put overlay 'display (propertize text 'face 'diff-added)))

(defun superchat-inline--overlay ()
  "The rewrite overlay at point, or a `user-error'."
  (or (seq-find (lambda (o) (overlay-get o 'superchat-text)) (overlays-at (point)))
      (user-error "No rewrite here")))

(defun superchat-inline-accept ()
  "Replace the region under the overlay at point with the replacement."
  (interactive)
  (let ((overlay (superchat-inline--overlay)))
    (when (superchat-run-active (overlay-get overlay 'superchat-tape))
      (user-error "The model is still answering; C-c C-k discards it"))
    (let ((start (overlay-start overlay))
          (end (overlay-end overlay))
          (text (overlay-get overlay 'superchat-text)))
      (delete-overlay overlay)
      (goto-char start)
      (delete-region start end)
      (insert text))))

(defun superchat-inline-discard ()
  "Discard the replacement, cancelling the run if it is active.
The region stays as it is."
  (interactive)
  (let ((overlay (superchat-inline--overlay)))
    (superchat-cancel (overlay-get overlay 'superchat-tape))
    (delete-overlay overlay)))

(defvar-keymap superchat-inline-map
  "C-c C-c" #'superchat-inline-accept
  "C-c C-k" #'superchat-inline-discard)

(defun superchat-inline--end (overlay run)
  "RUN ended: keep OVERLAY when it answered, else remove it and say why."
  (let ((status (plist-get run :status)))
    (when (overlay-buffer overlay)
      (if (eq status 'done)
          (superchat-inline--show
           overlay (superchat-inline--unfence
                    (plist-get (car (superchat-tape-entries
                                     :tape (plist-get run :tape) :kinds '(assistant)
                                     :newest t :limit 1))
                               :text)))
        (delete-overlay overlay)
        (message "Rewrite %s%s" (if (eq status 'error) "failed: " "cancelled")
                 (if (eq status 'error)
                     (plist-get (car (superchat-tape-entries
                                      :tape (plist-get run :tape) :kinds '(event)
                                      :newest t :limit 1))
                                :text)
                   ""))))))

;;;###autoload
(defun superchat-rewrite (instruction)
  "Ask the model to rewrite the region by INSTRUCTION.
The replacement shows over the region; \\<superchat-inline-map>\\[superchat-inline-accept] accepts it, \\[superchat-inline-discard] discards it."
  (interactive (list (progn (superchat-inline--region)
                            (read-string "Rewrite how: "))))
  (let* ((tape (concat "inline:" (superchat-tape-new-name)))
         (text (superchat-inline--region))
         (overlay (make-overlay (region-beginning) (region-end)))
         (started nil))
    (deactivate-mark)
    (overlay-put overlay 'keymap superchat-inline-map)
    (overlay-put overlay 'superchat-tape tape)
    (overlay-put overlay 'help-echo "C-c C-c accepts, C-c C-k discards")
    (superchat-inline--show overlay "")
    ;; No overlay may outlive a run that never started.
    (unwind-protect
        (progn
          (superchat-run tape (format "%s\n\n%s" instruction text)
                         :tools 'none :system superchat-inline--system
                         :origin (superchat-inline--origin)
                         :on-text (lambda (piece)
                                    (when (overlay-buffer overlay)
                                      (superchat-inline--show
                                       overlay (concat (overlay-get overlay 'superchat-text)
                                                       piece))))
                         :on-done (lambda (run) (superchat-inline--end overlay run)))
          (setq started t))
      (unless started (delete-overlay overlay)))))

;;;###autoload
(defun superchat-ask (question)
  "Ask QUESTION about this buffer, or the region, in the chat buffer."
  (interactive (list (read-string "Ask: ")))
  (let* ((region (and (use-region-p) (superchat-inline--region)))
         (origin (superchat-inline--origin))
         (scope (superchat-tape-scope-of default-directory))
         (buffer (superchat-chat-open (or (superchat-chat-recent-tape scope)
                                          (superchat-tape-new-name)))))
    (display-buffer buffer '(display-buffer-in-direction (direction . right)))
    (with-current-buffer buffer
      (superchat-chat-send (if region
                               (format "%s\n\n> %s" question
                                       (replace-regexp-in-string "\n" "\n> " region))
                             question)
                           origin))))

(provide 'superchat-inline)
;;; superchat-inline.el ends here
