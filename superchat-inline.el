;;; superchat-inline.el --- Rewrite a region in place; ask about a region -*- lexical-binding: t; -*-

;;; Commentary:
;; The replacement is shown by the `display' property of an overlay over the
;; region, so the buffer text stays what it was until the user accepts.  The
;; run is asynchronous, but a scripted model answers inside `superchat-run',
;; so the overlay is made before the run starts.

;;; Code:

(require 'subr-x)
(require 'superchat)
(require 'superchat-tape)
(require 'superchat-agent)
(require 'superchat-chat)

(defconst superchat-inline--system
  "Answer with the replacement text only: no explanation, no remarks, and no
code fence unless the fence is part of the replacement itself.")

(defun superchat-inline--region ()
  "The text of the active region, or a `user-error'."
  (unless (use-region-p) (user-error "Select a region first"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun superchat-inline--unfence (text)
  "TEXT without a fenced code block that wraps all of it.  Lines, not a
regexp over the whole answer, which overflows the matcher on a long one."
  (let* ((lines (split-string (string-trim text) "\n"))
         (body (butlast (cdr lines))))
    (if (and (cdr lines) (string-prefix-p "```" (car lines))
             (equal (car (last lines)) "```")
             ;; Two blocks with text between them are not one block.
             (not (seq-some (lambda (l) (string-prefix-p "```" l)) body)))
        (string-join body "\n")
      text)))

(defun superchat-inline--show (overlay text &optional ready)
  "Show TEXT after the region of OVERLAY and remember it.  The region stays in
sight, struck through, so the person sees what would go and what would come;
the last line says which keys act, READY once the answer is complete."
  (let ((apart (if (or (string-search "\n" text)
                       (string-search "\n" (buffer-substring (overlay-start overlay)
                                                              (overlay-end overlay))))
                   "\n" " ")))
    (overlay-put overlay 'superchat-text text)
    (overlay-put overlay 'face '(:inherit shadow :strike-through t))
    (overlay-put overlay 'after-string
                 (concat apart (propertize text 'face 'highlight) apart
                         (propertize (if ready "[on the struck-through text: C-c C-c accept · C-c C-k discard]"
                                       "[rewriting… on the struck-through text, C-c C-k cancels]")
                                     'face 'shadow)))))

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
      ;; The overlay goes last: when the buffer refuses the edit, it stays.
      (goto-char start)
      (delete-region start end)
      (insert text)
      (delete-overlay overlay))))

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
          (progn
            (superchat-inline--show
             overlay (superchat-inline--unfence (plist-get run :text)) t)
            (message "Rewrite ready: on the struck-through text, C-c C-c accepts, C-c C-k discards"))
        (delete-overlay overlay)
        (message "Rewrite %s%s" (if (eq status 'error) "failed: " "cancelled")
                 (if (eq status 'error) (plist-get run :error) ""))))))

(defun superchat-rewrite (instruction)
  "Ask the model to rewrite the region by INSTRUCTION.
The replacement shows after the region, which is struck through;
\\<superchat-inline-map>\\[superchat-inline-accept] accepts it, \\[superchat-inline-discard] discards it."
  (interactive (list (progn (superchat-inline--region)
                            (read-string "Rewrite how: "))))
  (let* ((tape (superchat-tape-new-name))
         (text (superchat-inline--region))
         (overlay (make-overlay (region-beginning) (region-end)))
         (started nil))
    (deactivate-mark)
    ;; The keys act where the overlay is: point goes there.
    (goto-char (overlay-start overlay))
    (overlay-put overlay 'keymap superchat-inline-map)
    (overlay-put overlay 'superchat-tape tape)
    (superchat-inline--show overlay "")
    ;; No overlay may outlive a run that never started.
    (unwind-protect
        (progn
          (superchat-run tape (format "%s\n\n%s" instruction text)
                         :tools 'none :ephemeral t :system superchat-inline--system
                         :origin (superchat-chat--origin-here)
                         :on-text (lambda (piece)
                                    (when (overlay-buffer overlay)
                                      (superchat-inline--show
                                       overlay (concat (overlay-get overlay 'superchat-text)
                                                       piece))))
                         :on-done (lambda (run) (superchat-inline--end overlay run)))
          (setq started t))
      (unless started (delete-overlay overlay)))))

(defun superchat-ask (question)
  "Ask QUESTION about this buffer, or the region, in the chat buffer."
  (interactive (list (read-string "Ask: ")))
  (let* ((region (and (use-region-p) (superchat-inline--region)))
         (origin (superchat-chat--origin-here))
         (scope (superchat-chat--scope))
         (tape (or (superchat-chat-recent-tape scope) (superchat-tape-new-name)))
         (buffer (superchat-chat-open tape scope))
         (text (if region
                   (format "%s\n\n> %s" question (replace-regexp-in-string "\n" "\n> " region))
                 question)))
    (display-buffer buffer '(display-buffer-in-direction (direction . right)))
    (with-current-buffer buffer
      (if (not (superchat-run-active tape))
          (superchat-chat-send text origin)
        ;; Not lost: the person sends it when the timeline is free.
        (goto-char (point-max))
        (insert (if (bolp) "" "\n") text)
        (message "%s is busy: the question is in its input, not sent" (buffer-name))))))

(provide 'superchat-inline)
;;; superchat-inline.el ends here
