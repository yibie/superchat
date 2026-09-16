;;; superchat-rewrite.el --- Deterministic region rewrite with diff preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Region rewriting: select code → instruct the model → the rewritten text
;; streams into a preview overlay over the region (source untouched) →
;; accept as one atomic Change Set, reject, refine, diff, or merge as
;; conflict markers.  No tool-calling; the answer is parsed for fenced code
;; blocks.  A rectangle / non-contiguous region gets one overlay and one
;; Run per segment.  `superchat-rewrite-apply-code-block-at-point' applies
;; a chat reply's code block as a patch.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'diff)
(require 'superchat-sink)
(require 'superchat-inline)

;; ── Forward declarations ──
(defvar superchat-llm-backend)

(defgroup superchat-rewrite nil
  "Region rewriting with LLM + unified-diff preview."
  :group 'superchat)

(defcustom superchat-rewrite-confirmation 'overlay
  "How a single-region rewrite is confirmed before it is applied.
`overlay' — the default (S17a): the rewritten text is previewed in an
            overlay over the region while it streams; the source buffer
            is untouched until the user accepts with \\`C-c C-a'
            (`superchat-rewrite-accept'); \\`C-c C-k' rejects.  A saved
            `ediff' value from earlier versions behaves the same way.
`auto'    — apply immediately once the rewrite is ready, with no
            preview or question (still undo-able).
A rectangle / non-contiguous region gets one overlay per segment, each
confirmed the same way."
  :type '(choice (const :tag "Overlay preview + accept" overlay)
                 (const :tag "Auto-apply (undo-able)" auto))
  :group 'superchat-rewrite)

(defcustom superchat-rewrite-context-lines 20
  "Lines before and after the region sent to the model as context."
  :type 'integer
  :group 'superchat-rewrite)

(defvar superchat-rewrite-instruction-history nil
  "Minibuffer history of rewrite instructions.")

(defconst superchat-rewrite--error-display-seconds 5
  "Seconds a failed rewrite's overlay stays visible before it is removed.")

(defconst superchat-rewrite--overlay-priority 2000
  "Overlay priority of a rewrite preview, above every source overlay.")

(defcustom superchat-rewrite-system-prompt
  "You are a precise code editor. Rewrite the provided code according \
to the user's instruction. Return ONLY the rewritten code inside a \
fenced code block with the appropriate language tag. Do not include \
explanations, apologies, or markdown outside the code fence. \
Preserve the original indentation style."
  "System prompt used for region rewrites."
  :type 'string
  :group 'superchat-rewrite)

;; ── Source tracking (for apply-code-block-at-point) ──

(defvar superchat-rewrite--last-source-buffer nil
  "Buffer name last used by send-region/defun or rewrite-region.")
(defvar superchat-rewrite--last-source-region nil
  "Region (BEG . END) last used by send-region or rewrite-region.")

(defun superchat-rewrite--record-source (buffer-name beg end)
  "Record BUFFER-NAME and (BEG . END) as the last rewrite source."
  (setq superchat-rewrite--last-source-buffer buffer-name)
  (setq superchat-rewrite--last-source-region (cons beg end)))

;;;###autoload
(defun superchat-rewrite-region (beg end instruction)
  "Rewrite the region from BEG to END according to INSTRUCTION.
The command returns at once with the preview overlay; the model's answer
streams into the overlay's `display' while the source buffer stays
byte-for-byte unchanged until `superchat-rewrite-accept' applies it as one
Change Set.  A non-contiguous (rectangle / multiple) region, per
`region-noncontiguous-p', gets one independent overlay and Run per segment
and returns the list of overlays.  When `superchat-rewrite-confirmation'
is `auto', a ready rewrite is accepted automatically (still undo-able)."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Rewrite instruction: " nil
                          'superchat-rewrite-instruction-history))
     (user-error "No active region")))
  (superchat-plugin-require-active-row 'inline-rewrite)
  (unless superchat-llm-backend
    (user-error "superchat-llm-backend is not configured"))
  (let ((current-lang (symbol-name major-mode)))
    (if (and (fboundp 'region-noncontiguous-p)
             (region-noncontiguous-p))
        ;; S17b: one independent overlay and Run per segment; no block-count
        ;; matching between segments any more.
        (let ((segments (superchat-rewrite--collect-region-segments)))
          (unless segments
            (user-error "No region segments found"))
          (mapcar (lambda (segment)
                    (superchat-rewrite--start-overlay
                     (nth 1 segment) (nth 2 segment) instruction current-lang))
                  segments))
      (superchat-rewrite--start-overlay beg end instruction current-lang))))

(defun superchat-rewrite--collect-region-segments ()
  "Return a list of (TEXT BEG END) for each non-contiguous region segment.
Returns nil when `region-noncontiguous-p' is false or unsupported."
  (when (and (fboundp 'region-noncontiguous-p)
             (region-noncontiguous-p))
    (let ((segments nil))
      (dolist (reg (funcall region-extract-function 'bounds))
        (let ((beg (car reg))
              (end (cdr reg)))
          (push (list (buffer-substring-no-properties beg end) beg end)
                segments)))
      (nreverse segments))))

;; ── Overlay preview (S17a; S17b adds iterate / diff / merge / navigation) ──

(defun superchat-rewrite--context-text (beg end)
  "Return the model input for rewriting BEG..END with its surrounding lines.
`superchat-rewrite-context-lines' lines before and after the region wrap
the region itself, which is marked with <<<REGION>>> / <<<END>>>."
  (let* ((n (max 0 superchat-rewrite-context-lines))
         (before-start (save-excursion (goto-char beg)
                                       (forward-line (- n))
                                       (point)))
         (after-end (save-excursion (goto-char end)
                                    (forward-line n)
                                    (line-end-position))))
    (concat (buffer-substring-no-properties before-start beg)
            "<<<REGION>>>\n"
            (buffer-substring-no-properties beg end)
            "\n<<<END>>>"
            (buffer-substring-no-properties end after-end))))

(defvar superchat-rewrite--overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'superchat-rewrite-accept)
    (define-key map (kbd "C-c C-k") #'superchat-rewrite-reject)
    (define-key map (kbd "C-c C-c") #'superchat-rewrite-cancel)
    (define-key map (kbd "C-c C-r") #'superchat-rewrite-iterate)
    (define-key map (kbd "C-c C-d") #'superchat-rewrite-diff)
    (define-key map (kbd "C-c C-m") #'superchat-rewrite-merge)
    (define-key map (kbd "C-c C-n") #'superchat-rewrite-next)
    (define-key map (kbd "C-c C-p") #'superchat-rewrite-previous)
    (define-key map (kbd "RET") #'superchat-rewrite-menu)
    (define-key map [mouse-1] #'superchat-rewrite-menu)
    map)
  "Keymap active on a rewrite preview overlay.")

(defun superchat-rewrite--overlay-at-point ()
  "Return the rewrite preview overlay at point, or nil."
  (cl-find-if (lambda (overlay) (overlay-get overlay 'superchat-rewrite))
              (overlays-at (point))))

(defun superchat-rewrite--overlay ()
  "Return the rewrite overlay a command addresses, or signal."
  (or (superchat-rewrite--overlay-at-point)
      (user-error "No rewrite preview here")))

(defun superchat-rewrite--state (overlay)
  "Return OVERLAY's rewrite state plist."
  (overlay-get overlay 'superchat-rewrite))

(defun superchat-rewrite--status-line (state)
  "Return the status bar text for STATE."
  (pcase (plist-get state :status)
    ('waiting (let ((model (plist-get state :model)))
                (if model (format "REWRITE  Waiting… [%s]" model)
                  "REWRITE  Waiting…")))
    ('typing "REWRITE  Typing…")
    ('ready "REWRITE  Ready  C-c C-a accept  C-c C-k reject")
    ('error (format "REWRITE  Error: %s" (or (plist-get state :error) "failed")))
    ('cancelled "REWRITE  Cancelled")
    (_ "REWRITE")))

(defun superchat-rewrite--render (overlay)
  "Refresh OVERLAY's status bar and preview from its state.
The preview (`display') replaces the covered text visually only: while
waiting it is the original text in `shadow' face; while typing, the
streamed text so far; when ready, the extracted code.  On an error the
original text stays visible and only the status bar changes."
  (when (overlay-buffer overlay)
    (let* ((state (superchat-rewrite--state overlay))
           (status (plist-get state :status))
           (original (plist-get state :original))
           (text (plist-get state :text))
           (preview
            (pcase status
              ('waiting (propertize original 'face 'shadow))
              ('typing (if (and text (not (string-empty-p text))) text
                         (propertize original 'face 'shadow)))
              ('ready (or text original))
              (_ original))))
      (overlay-put overlay 'before-string
                   (concat (propertize (superchat-rewrite--status-line state)
                                       'face 'mode-line-emphasis)
                           "\n"))
      (overlay-put overlay 'display preview))))

(defun superchat-rewrite--set-state (overlay key value)
  "Set KEY to VALUE in OVERLAY's state and re-render."
  (when (overlayp overlay)
    (overlay-put overlay 'superchat-rewrite
                 (plist-put (superchat-rewrite--state overlay) key value))
    (superchat-rewrite--render overlay)))

(defun superchat-rewrite--make-overlay (beg end state)
  "Create the rewrite preview overlay over BEG..END carrying STATE."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'superchat-rewrite state)
    (overlay-put overlay 'keymap superchat-rewrite--overlay-keymap)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'priority superchat-rewrite--overlay-priority)
    (superchat-rewrite--render overlay)
    overlay))

(defun superchat-rewrite--delete-overlay (overlay)
  "Remove OVERLAY and its pending error timer."
  (when (overlayp overlay)
    (let ((timer (plist-get (superchat-rewrite--state overlay) :error-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (delete-overlay overlay)))

(defun superchat-rewrite--sink (overlay lang-tag)
  "Return the preview Sink for OVERLAY (contract §3).
Stream chunks accumulate into the preview; the final text is parsed for a
fenced LANG-TAG code block, which becomes the ready preview (no block →
error); a failure shows its text for `superchat-rewrite--error-display-seconds'
and then removes the overlay; a cancellation removes it at once."
  (superchat-sink-create
   :id (format "rewrite-%08x" (random #x100000000))
   :live-p (lambda ()
             (and (overlayp overlay)
                  (overlay-buffer overlay)
                  (buffer-live-p (overlay-buffer overlay))))
   :status (lambda (_run status)
             (pcase status
               ('waiting (superchat-rewrite--set-state overlay :status 'waiting))
               ((or 'typing 'calling-tool)
                (superchat-rewrite--set-state overlay :status 'typing))))
   :stream (lambda (_run chunk)
             (let* ((payload (if (and (listp chunk) (plist-member chunk :kind))
                                 chunk
                               (superchat-sink-normalize-chunk chunk)))
                    (text (and payload (eq (plist-get payload :kind) 'text)
                               (plist-get payload :text))))
               (when (and (stringp text) (not (string-empty-p text)))
                 (let ((state (superchat-rewrite--state overlay)))
                   (overlay-put overlay 'superchat-rewrite
                                (plist-put state :text
                                           (concat (or (plist-get state :text) "")
                                                   text))))
                 (superchat-rewrite--set-state overlay :status 'typing))))
   :final (lambda (_run result)
            (let ((code (superchat-rewrite--extract-code-block
                         (or (plist-get result :text) "") lang-tag)))
              (if (null code)
                  (superchat-rewrite--fail overlay "no code block in the answer")
                (overlay-put overlay 'superchat-rewrite
                             (plist-put (superchat-rewrite--state overlay) :text code))
                (superchat-rewrite--set-state overlay :status 'ready)
                (when (eq superchat-rewrite-confirmation 'auto)
                  (superchat-rewrite-accept overlay)))))
   :error (lambda (_run failure)
            (if (eq (plist-get failure :status) 'cancelled)
                (progn
                  (superchat-rewrite--set-state overlay :status 'cancelled)
                  (superchat-rewrite--delete-overlay overlay))
              (superchat-rewrite--fail overlay (or (plist-get failure :text) "failed"))))
   :orphaned (lambda (_run _reason)
               (superchat-rewrite--delete-overlay overlay))))

(defun superchat-rewrite--fail (overlay message)
  "Show MESSAGE in OVERLAY's status bar, then remove the overlay later."
  (when (overlayp overlay)
    (overlay-put overlay 'superchat-rewrite
                 (plist-put (superchat-rewrite--state overlay) :error message))
    (superchat-rewrite--set-state overlay :status 'error)
    (superchat-rewrite--set-state
     overlay :error-timer
     (run-at-time superchat-rewrite--error-display-seconds nil
                  (lambda () (superchat-rewrite--delete-overlay overlay))))))

(defun superchat-rewrite--model-name ()
  "Return the configured model name for the status bar, or nil."
  (and (fboundp 'superchat--provider-chat-model)
       (ignore-errors (superchat--provider-chat-model superchat-llm-backend))))

(defun superchat-rewrite--content (buffer beg end instruction lang-tag base)
  "Return the model input for rewriting BASE (the text at BEG..END of BUFFER
on a first ask, or the previous answer when iterating) under INSTRUCTION.
A first ask wraps the region with `superchat-rewrite-context-lines' lines of
surrounding context; an iteration sends the previous answer alone."
  (with-current-buffer buffer
    (let ((region-text (buffer-substring-no-properties beg end)))
      (format "Rewrite the following %s code according to this instruction: %s\n\n%s"
              lang-tag instruction
              (if (equal base region-text)
                  (superchat-rewrite--context-text beg end)
                (concat "<<<REGION>>>\n" base "\n<<<END>>>"))))))

(defun superchat-rewrite--submit (overlay instruction base)
  "Submit a rewrite Run for OVERLAY: rewrite BASE under INSTRUCTION.
The overlay goes back to `waiting' with an empty stream; the Run's answer
streams into it through `superchat-rewrite--sink'.  Returns OVERLAY."
  (superchat-plugin-require-active-row 'inline-rewrite)
  (let* ((buffer (overlay-buffer overlay))
         (beg (overlay-start overlay))
         (end (overlay-end overlay))
         (state (superchat-rewrite--state overlay))
         (lang-tag (plist-get state :lang))
         (session-id (plist-get state :session-id))
         (envelope
          (list :surface 'inline-rewrite
                :session-id session-id
                :content (superchat-rewrite--content buffer beg end instruction
                                                     lang-tag base)
                :tools 'none
                :attendance 'attended
                :system-prompt
                (format "%s\n\nUse language tag `%s' for the code fence."
                        superchat-rewrite-system-prompt lang-tag)
                :sink (superchat-rewrite--sink overlay lang-tag)
                :context (list :rewrite-buffer buffer
                               :rewrite-region (cons (copy-marker beg)
                                                     (copy-marker end t))
                               :rewrite-original base))))
    (add-to-history 'superchat-rewrite-instruction-history instruction)
    (overlay-put overlay 'superchat-rewrite
                 (plist-put (plist-put (plist-put state :instruction instruction)
                                       :base base)
                            :text nil))
    (superchat-rewrite--set-state overlay :status 'waiting)
    (condition-case err
        (let ((handle (superchat-service-call nil 'driver 'submit envelope)))
          (when (plist-get handle :run-id)
            (overlay-put overlay 'superchat-rewrite
                         (plist-put (superchat-rewrite--state overlay)
                                    :run-id (plist-get handle :run-id)))))
      ((error quit)
       (superchat-rewrite--delete-overlay overlay)
       (signal (car err) (cdr err))))
    overlay))

(defun superchat-rewrite--start-overlay (beg end instruction lang-tag)
  "Start an asynchronous rewrite of BEG..END and return its preview overlay."
  (when (= beg end)
    (user-error "Cannot rewrite an empty region"))
  (let* ((buffer (current-buffer))
         (original (buffer-substring-no-properties beg end))
         (state (list :run-id nil :session-id (superchat-inline-session-id buffer)
                      :original original :instruction instruction
                      :lang lang-tag :model (superchat-rewrite--model-name)
                      :status 'waiting :text nil :error nil))
         (overlay (superchat-rewrite--make-overlay beg end state)))
    (superchat-rewrite--record-source (buffer-name) beg end)
    (superchat-rewrite--submit overlay instruction original)))

;;;###autoload
(defun superchat-rewrite-accept (&optional overlay)
  "Apply the ready rewrite of OVERLAY (default the preview at point).
The region is replaced through `superchat-change-set-apply' -- one undo
step, one `edit.applied' Tape event -- and the overlay is removed."
  (interactive)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (state (superchat-rewrite--state overlay))
         (buffer (overlay-buffer overlay)))
    (unless (eq (plist-get state :status) 'ready)
      (user-error "Rewrite is not ready yet (%s)" (plist-get state :status)))
    (let ((beg (overlay-start overlay))
          (end (overlay-end overlay))
          (text (plist-get state :text)))
      (superchat-rewrite--delete-overlay overlay)
      (superchat-service-call nil 'change-set 'apply
       buffer (list (list beg end text))
       :session-id (plist-get state :session-id)
       :run-id (plist-get state :run-id)
       :source 'inline-rewrite)
      (message "Rewrite applied. Undo to revert."))))

;;;###autoload
(defun superchat-rewrite-reject (&optional overlay)
  "Discard the rewrite preview OVERLAY (default the one at point)."
  (interactive)
  (superchat-rewrite--delete-overlay (or overlay (superchat-rewrite--overlay)))
  (message "Rewrite rejected."))

;;;###autoload
(defun superchat-rewrite-cancel (&optional overlay)
  "Cancel the running rewrite of OVERLAY (default the one at point).
The Run is cancelled through Runtime; its cancellation removes the overlay."
  (interactive)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (run-id (plist-get (superchat-rewrite--state overlay) :run-id)))
    (when run-id
      (ignore-errors (superchat-service-call nil 'driver 'cancel run-id "rewrite cancelled")))
    ;; Safety net: the Sink's cancelled branch normally did this already.
    (superchat-rewrite--delete-overlay overlay)))

;;;###autoload
(defun superchat-rewrite-iterate (&optional overlay instruction)
  "Refine OVERLAY's ready rewrite with another INSTRUCTION (S17b).
The previous answer becomes the text to rewrite; the source stays untouched."
  (interactive)
  (superchat-plugin-require-active-row 'inline-rewrite)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (state (superchat-rewrite--state overlay)))
    (unless (eq (plist-get state :status) 'ready)
      (user-error "Rewrite is not ready yet (%s)" (plist-get state :status)))
    (let ((instruction (or instruction
                           (read-string "Refine rewrite: " nil
                                        'superchat-rewrite-instruction-history))))
      (superchat-rewrite--submit overlay instruction (plist-get state :text)))))

;;;###autoload
(defun superchat-rewrite-diff (&optional overlay)
  "Show OVERLAY's rewrite as a unified diff in *superchat-rewrite-diff* (S17b).
Returns the diff buffer; the source buffer is untouched."
  (interactive)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (state (superchat-rewrite--state overlay))
         (text (superchat-rewrite--diff-text (plist-get state :original)
                                             (or (plist-get state :text) "")))
         (buffer (get-buffer-create "*superchat-rewrite-diff*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (when (fboundp 'diff-mode) (diff-mode))
        (setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

(defun superchat-rewrite--conflict-text (original replacement)
  "Return ORIGINAL and REPLACEMENT as one conflict-marker block."
  (concat "<<<<<<< original\n" original
          (if (string-suffix-p "\n" original) "" "\n")
          "=======\n" replacement
          (if (string-suffix-p "\n" replacement) "" "\n")
          ">>>>>>> rewrite\n"))

;;;###autoload
(defun superchat-rewrite-merge (&optional overlay)
  "Accept OVERLAY's rewrite as conflict markers and enable `smerge-mode' (S17b).
One Change Set replaces the region with `<<<<<<< original' / `=======' /
`>>>>>>> rewrite' sections; the overlay is removed."
  (interactive)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (state (superchat-rewrite--state overlay))
         (buffer (overlay-buffer overlay)))
    (unless (eq (plist-get state :status) 'ready)
      (user-error "Rewrite is not ready yet (%s)" (plist-get state :status)))
    (let ((beg (overlay-start overlay))
          (end (overlay-end overlay))
          (text (superchat-rewrite--conflict-text (plist-get state :original)
                                                  (plist-get state :text))))
      (superchat-rewrite--delete-overlay overlay)
      (superchat-service-call nil 'change-set 'apply
       buffer (list (list beg end text))
       :session-id (plist-get state :session-id)
       :run-id (plist-get state :run-id)
       :source 'inline-rewrite-merge)
      (with-current-buffer buffer
        (require 'smerge-mode)
        (smerge-mode 1))
      (message "Rewrite merged as conflict markers; resolve with smerge."))))

(defun superchat-rewrite--sorted-overlays ()
  "Return the current buffer's rewrite overlays ordered by position."
  (sort (superchat-rewrite-overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

;;;###autoload
(defun superchat-rewrite-next ()
  "Move point to the next rewrite preview in the buffer, wrapping around."
  (interactive)
  (let* ((overlays (superchat-rewrite--sorted-overlays))
         (next (or (cl-find-if (lambda (o) (> (overlay-start o) (point))) overlays)
                   (car overlays))))
    (unless next (user-error "No rewrite previews in this buffer"))
    (goto-char (overlay-start next))
    next))

;;;###autoload
(defun superchat-rewrite-previous ()
  "Move point to the previous rewrite preview in the buffer, wrapping around."
  (interactive)
  (let* ((overlays (superchat-rewrite--sorted-overlays))
         (previous (or (cl-find-if (lambda (o) (< (overlay-start o) (point)))
                                   (reverse overlays))
                       (car (last overlays)))))
    (unless previous (user-error "No rewrite previews in this buffer"))
    (goto-char (overlay-start previous))
    previous))

;;;###autoload
(defun superchat-rewrite-accept-all ()
  "Accept every ready rewrite preview in the current buffer."
  (interactive)
  (let ((count 0))
    (dolist (overlay (reverse (superchat-rewrite--sorted-overlays)))
      (when (eq (plist-get (superchat-rewrite--state overlay) :status) 'ready)
        (superchat-rewrite-accept overlay)
        (cl-incf count)))
    (message "Accepted %d rewrite(s)." count)
    count))

;;;###autoload
(defun superchat-rewrite-reject-all ()
  "Discard every rewrite preview in the current buffer (cancelling running ones)."
  (interactive)
  (let ((overlays (superchat-rewrite-overlays)))
    (dolist (overlay overlays)
      (if (memq (plist-get (superchat-rewrite--state overlay) :status) '(waiting typing))
          (superchat-rewrite-cancel overlay)
        (superchat-rewrite--delete-overlay overlay)))
    (message "Rejected %d rewrite(s)." (length overlays))
    (length overlays)))

;;;###autoload
(defun superchat-rewrite-menu (&optional overlay)
  "Show the rewrite menu for OVERLAY (default the one at point)."
  (interactive)
  (let* ((overlay (or overlay (superchat-rewrite--overlay)))
         (choice (read-multiple-choice
                  "Superchat rewrite: "
                  '((?a "accept" "apply the rewrite")
                    (?k "reject" "discard the preview")
                    (?c "cancel" "cancel the running rewrite")
                    (?r "refine" "rewrite the answer with another instruction")
                    (?d "diff" "show the rewrite as a diff")
                    (?m "merge" "apply as conflict markers")
                    (?A "accept all" "apply every ready rewrite in the buffer")
                    (?K "reject all" "discard every preview in the buffer")
                    (?q "quit" "leave it as is")))))
    (pcase (car choice)
      (?a (superchat-rewrite-accept overlay))
      (?k (superchat-rewrite-reject overlay))
      (?c (superchat-rewrite-cancel overlay))
      (?r (superchat-rewrite-iterate overlay))
      (?d (superchat-rewrite-diff overlay))
      (?m (superchat-rewrite-merge overlay))
      (?A (superchat-rewrite-accept-all))
      (?K (superchat-rewrite-reject-all))
      (?q nil))))

(defun superchat-rewrite-overlays (&optional buffer)
  "Return live rewrite preview overlays in BUFFER (default current buffer)."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (cl-remove-if-not (lambda (overlay)
                               (overlay-get overlay 'superchat-rewrite))
                             (overlays-in (point-min) (point-max)))))))

;; ── Code block extraction ──

(defun superchat-rewrite--extract-code-block (text lang-tag)
  "Extract the first fenced code block from TEXT.
Prefer blocks tagged with LANG-TAG; fall back to any fenced block.
Returns the code content (without fences), or nil."
  (when (stringp text)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((pattern (format "```%s\\s-*\n\\(\\(?:.\\|\n\\)*?\\)\n```"
                             (regexp-quote lang-tag)))
            (fallback "```[a-z-]*\\s-*\n\\(\\(?:.\\|\n\\)*?\\)\n```"))
        (if (re-search-forward pattern nil t)
            (match-string 1)
          (goto-char (point-min))
          (when (re-search-forward fallback nil t)
            (match-string 1)))))))

(defun superchat-rewrite--extract-all-code-blocks (text lang-tag)
  "Extract ALL fenced code blocks from TEXT.
Prefer blocks tagged with LANG-TAG; fall back to any fenced blocks.
Returns a list of code strings (without fences), in order of appearance."
  (when (stringp text)
    (let ((blocks nil))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((pattern (format "```%s\\s-*\n\\(\\(?:.\\|\n\\)*?\\)\n```"
                               (regexp-quote lang-tag)))
              (fallback "```[a-z-]*\\s-*\n\\(\\(?:.\\|\n\\)*?\\)\n```"))
          (while (or (re-search-forward pattern nil t)
                     (re-search-forward fallback nil t))
            (push (match-string 1) blocks))))
      (nreverse blocks))))

(defun superchat-rewrite--diff-text (before after)
  "Return a full unified diff from BEFORE to AFTER, via `diff-no-select'."
  (if (equal before after)
      "No textual changes.\n"
    (let ((before-buffer (generate-new-buffer " *superchat-rewrite-preview-before*"))
          (after-buffer (generate-new-buffer " *superchat-rewrite-preview-after*"))
          (diff-buffer (generate-new-buffer " *superchat-rewrite-preview-diff*")))
      (unwind-protect
          (progn
            (with-current-buffer before-buffer (insert before))
            (with-current-buffer after-buffer (insert after))
            (diff-no-select before-buffer after-buffer "-u" t diff-buffer)
            (with-current-buffer diff-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
        (dolist (buf (list before-buffer after-buffer diff-buffer))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun superchat-rewrite--preview-and-confirm (preview-name diff-text prompt)
  "Show DIFF-TEXT in a help window named after PREVIEW-NAME, then ask PROMPT.
Returns non-nil when the user confirms.  Owns a freshly created preview
buffer object (`generate-new-buffer', which Emacs uniquifies against any
existing buffer of that name rather than reusing it) and the window
configuration: both are restored to their pre-call state on every exit —
accept, decline, or a nonlocal exit such as `quit' — so no preview buffer
or window ever survives this call, and a pre-existing buffer that happens
to share PREVIEW-NAME is never touched, let alone killed."
  (let ((window-config (current-window-configuration))
        (preview-buffer (generate-new-buffer preview-name)))
    (unwind-protect
        (progn
          (with-help-window preview-buffer
            (with-current-buffer standard-output
              (insert diff-text)))
          (y-or-n-p prompt))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer))
      (set-window-configuration window-config))))

(defun superchat-rewrite--confirm-change (original replacement target-buf)
  "Preview a diff from ORIGINAL to REPLACEMENT for TARGET-BUF and ask to apply.
Returns non-nil when the user confirms.  See
`superchat-rewrite--preview-and-confirm' for the exit guarantee."
  (superchat-rewrite--preview-and-confirm
   (format "*Superchat Rewrite Preview <%s>*" (buffer-name target-buf))
   (superchat-rewrite--diff-text original replacement)
   (format "Apply this rewrite to %s? " (buffer-name target-buf))))

;; ── Apply code block at point (Part A: reply → patch) ──

;;;###autoload
(defun superchat-rewrite-apply-code-block-at-point ()
  "Apply the code block at point as a patch to the last source buffer.
Finds the nearest `#+begin_src ... #+end_src' org block, extracts
its content, and applies it to the source buffer recorded by the
last send-region/send-defun/rewrite-region operation.  When no
source is recorded, prompts for a target buffer.

Uses `superchat-change-set-apply', so the replacement lands in one
undo step and records an `edit.applied' Tape event tagged with the
calling buffer's `superchat--session-id'.  When a target region is
known, the change is previewed as a diff and confirmed before it is
applied; otherwise the code block is appended to the end of the
target buffer without a prompt."
  (interactive)
  (superchat-plugin-require-active-row 'inline-rewrite)
  (let ((block-info (superchat-rewrite--code-block-at-point))
        (session-id (buffer-local-value 'superchat--session-id (current-buffer))))
    (unless block-info
      (user-error "No org source block found at point"))
    (let* ((code (car block-info))
           (target-buf (superchat-rewrite--resolve-target-buffer)))
      (unless target-buf
        (user-error "No target buffer — send a region/defun first, or select a buffer"))
      (let ((target-region (superchat-rewrite--resolve-target-region target-buf)))
        (if target-region
            (let* ((beg (car target-region))
                   (end (cdr target-region))
                   (original (with-current-buffer target-buf
                               (buffer-substring-no-properties beg end))))
              (if (superchat-rewrite--confirm-change original code target-buf)
                  (progn
                    (superchat-service-call nil 'change-set 'apply
                     target-buf (list (list beg end code))
                     :session-id session-id
                     :source 'chat-apply-code-block)
                    (message "Region rewritten in %s. Undo to revert."
                             (buffer-name target-buf)))
                (message "Rewrite cancelled.")))
          ;; No region: append to end of target buffer, no confirmation.
          (with-current-buffer target-buf
            (goto-char (point-max))
            (let* ((needs-newline (not (bolp)))
                   (insert-pos (point))
                   (text (concat (if needs-newline "\n" "") code "\n")))
              (superchat-service-call nil 'change-set 'apply
               target-buf (list (list insert-pos insert-pos text))
               :session-id session-id
               :source 'chat-apply-code-block)
              (message "Code block appended to %s (undo to revert)."
                       (buffer-name target-buf)))))))))

(defun superchat-rewrite--code-block-at-point ()
  "Return (CODE . LANG-TAG) for the org source block at point, or nil."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^[ \\t]*#\\+begin_src\\s-*\\([a-z-]*\\)" nil t)
      (let ((lang-tag (or (match-string 1) "")))
        (forward-line 1)
        (let ((code-beg (point))
              (code-end (save-excursion
                          (if (re-search-forward "^[ \\t]*#\\+end_src" nil t)
                              (progn (beginning-of-line) (point))
                            (point-max)))))
          (when (> code-end code-beg)
            (cons (string-trim-right
                   (buffer-substring-no-properties code-beg code-end))
                  lang-tag)))))))

(defun superchat-rewrite--resolve-target-buffer ()
  "Return the target buffer for applying a code block.
Uses the last recorded source, or prompts the user."
  (if (and superchat-rewrite--last-source-buffer
           (get-buffer superchat-rewrite--last-source-buffer)
           (buffer-live-p (get-buffer superchat-rewrite--last-source-buffer)))
      (get-buffer superchat-rewrite--last-source-buffer)
    (let ((buf-name (read-buffer "Apply code block to buffer: " nil t)))
      (get-buffer buf-name))))

(defun superchat-rewrite--resolve-target-region (buffer)
  "Return (BEG . END) of the last recorded region in BUFFER, or nil."
  (when (and superchat-rewrite--last-source-region
             (equal (buffer-name buffer)
                    superchat-rewrite--last-source-buffer))
    superchat-rewrite--last-source-region))

(provide 'superchat-rewrite)
;;; superchat-rewrite.el ends here
