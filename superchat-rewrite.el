;;; superchat-rewrite.el --- Deterministic region rewrite with diff preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Region rewriting: select code → instruct the model → the rewritten text
;; streams into a preview overlay over the region (source untouched) →
;; accept as one atomic Change Set, reject, refine, diff, or merge as
;; conflict markers.  No tool-calling.  Only the selected text (the Rewrite
;; target) is rewritten; the text around it goes along as a read-only
;; Rewrite reference, and the answer is the replacement alone
;; (docs/decisions/accepted/2026-09-26-region-rewrite-target-and-reference.md).  A rectangle / non-contiguous region gets one overlay and one
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
  "Lines of Rewrite reference on each side of the target.
In a `prog-mode' buffer they widen the definition the target is in; in
Org and Markdown the reference is the target's heading section instead."
  :type 'integer
  :group 'superchat-rewrite)

(defvar superchat-rewrite-instruction-history nil
  "Minibuffer history of rewrite instructions.")

(defconst superchat-rewrite--error-display-seconds 5
  "Seconds a failed rewrite's overlay stays visible before it is removed.")

(defconst superchat-rewrite--overlay-priority 2000
  "Overlay priority of a rewrite preview, above every source overlay.")

(defcustom superchat-rewrite-system-prompt nil
  "System text of a region rewrite, or nil for the built-in one.
The built-in text depends on the buffer: code in a `prog-mode' buffer, an
editor of that kind of text anywhere else (see
`superchat-rewrite--system-text').  A string replaces it for every buffer."
  :type '(choice (const :tag "Built-in, by the buffer's kind" nil)
                 string)
  :group 'superchat-rewrite)

(defconst superchat-rewrite--reference-max-chars 8000
  "Most characters of Rewrite reference sent on each side of the target.
The part nearest the target is kept.")

(defconst superchat-rewrite--marker-regexp "<<<[A-Z][A-Z ]*>>>"
  "Matches the labels a rewrite request puts around its parts.
An answer that contains one has returned more than the replacement.")

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
             (read-string (superchat-rewrite--instruction-prompt
                           (region-beginning) (region-end))
                          nil 'superchat-rewrite-instruction-history))
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

(defun superchat-rewrite--instruction-prompt (beg end)
  "Return the instruction prompt naming the target BEG..END.
It says how much is selected and how it begins, so a wrong selection is
seen before anything is sent."
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (length (split-string (string-trim-right text "\n+") "\n")))
         (size (if (> lines 1)
                   (format "%d lines" lines)
                 (format "%d character%s" (length text)
                         (if (= 1 (length text)) "" "s"))))
         (shown (truncate-string-to-width
                 (replace-regexp-in-string "\n" "⏎" (string-trim text))
                 30 nil nil "…")))
    (format "Rewrite %s «%s» — instruction: " size shown)))

(defun superchat-rewrite--heading-regexp ()
  "Return the heading regexp of the current buffer's markup, or nil."
  (cond ((derived-mode-p 'org-mode) "^\\*+[ \t]")
        ((derived-mode-p 'markdown-mode) "^#+[ \t]")))

(defun superchat-rewrite--enclosing-definition (pos)
  "Return (START . END) of the top-level definition containing POS, or nil."
  (save-excursion
    (ignore-errors
      (goto-char pos)
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (when (and (<= (point) pos) (< pos end))
          (cons (point) end))))))

(defun superchat-rewrite--reference-bounds (beg end)
  "Return (START . STOP) of the Rewrite reference around BEG..END.
In Org and Markdown it is the target's heading section, never crossing into
another one.  In a `prog-mode' buffer it is the definition the target is
in, widened by `superchat-rewrite-context-lines' lines on each side.
Elsewhere it is that many lines on each side."
  (save-excursion
    (let ((n (max 0 superchat-rewrite-context-lines))
          (heading (superchat-rewrite--heading-regexp)))
      (cond
       (heading
        (cons (progn (goto-char beg)
                     (beginning-of-line)
                     (cond ((looking-at heading) (point))
                           ((re-search-backward heading nil t) (point))
                           (t (point-min))))
              (progn (goto-char end)
                     (cond ((and (bolp) (looking-at heading)) (point))
                           ((re-search-forward heading nil t) (match-beginning 0))
                           (t (point-max))))))
       (t
        (let ((code (derived-mode-p 'prog-mode)))
          (cons (progn (goto-char (or (and code (car (superchat-rewrite--enclosing-definition beg)))
                                      beg))
                       (forward-line (- n))
                       (point))
                (progn (goto-char (or (and code (cdr (superchat-rewrite--enclosing-definition
                                                           (max beg (1- end)))))
                                      end))
                       (forward-line n)
                       (line-end-position)))))))))

(defun superchat-rewrite--reference (beg end)
  "Return (BEFORE . AFTER), the Rewrite reference around BEG..END, or nil.
Each side is at most `superchat-rewrite--reference-max-chars' characters,
cut at a line boundary, keeping the part nearest the target."
  (let* ((bounds (superchat-rewrite--reference-bounds beg end))
         (max superchat-rewrite--reference-max-chars)
         (before (buffer-substring-no-properties (min (car bounds) beg) beg))
         (after (buffer-substring-no-properties end (max (cdr bounds) end))))
    (when (> (length before) max)
      (setq before (substring before (- max)))
      (when (string-match "\n" before)
        (setq before (substring before (match-end 0)))))
    (when (> (length after) max)
      (setq after (substring after 0 max))
      (when (string-match "\n[^\n]*\\'" after)
        (setq after (substring after 0 (match-beginning 0)))))
    (setq before (string-trim before "\n+" "\n+")
          after (string-trim after "\n+" "\n+"))
    (unless (and (string-empty-p before) (string-empty-p after))
      (cons before after))))

(defun superchat-rewrite--kind-name ()
  "Return a readable name for the current buffer's kind of text."
  (let ((name (string-remove-suffix "-mode" (symbol-name major-mode))))
    (pcase name
      ("org" "Org")
      ((or "markdown" "gfm") "Markdown")
      ((or "emacs-lisp" "lisp-interaction" "lisp-data") "Emacs Lisp")
      ("fundamental" "plain")
      (_ (capitalize (replace-regexp-in-string "-" " " name))))))

(defun superchat-rewrite--system-text ()
  "Return the built-in system text for a rewrite in the current buffer."
  (let ((kind (superchat-rewrite--kind-name)))
    (if (derived-mode-p 'prog-mode)
        (format "You are a precise editor of %s code. The user selects a piece of \
code (the target) and gives an instruction. Rewrite only the target according \
to the instruction. Code marked as reference is there so you understand the \
setting: never change it and never include it in your answer. Reply with only \
the replacement for the target: no explanation, no markers, and no code fence \
around it. Keep the indentation style of the surrounding code." kind)
      (format "You are an editor of %s text. The user selects a passage (the \
target) and gives an instruction. Rewrite only the target according to the \
instruction. Text marked as reference is there so you understand the setting: \
never change it and never include it in your answer. Reply with only the \
replacement for the target: no explanation, no markers, and no code fence \
around it. Keep the markup of the text (links, lists, emphasis) as it is." kind))))

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
  "Return the rewrite preview overlay at point or ending right at point, or nil.
After a forward selection point sits at the preview's end, just outside it."
  (or (cl-find-if (lambda (overlay) (overlay-get overlay 'superchat-rewrite))
                  (overlays-at (point)))
      (cl-find-if (lambda (overlay)
                    (and (overlay-get overlay 'superchat-rewrite)
                         (= (overlay-end overlay) (point))))
                  (overlays-in (max (point-min) (1- (point))) (point)))))

(defun superchat-rewrite--when-at-preview (command)
  "Return COMMAND when point is on a rewrite preview or right after it."
  (and (superchat-rewrite--overlay-at-point) command))

(defvar superchat-rewrite-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding '(("C-c C-a" . superchat-rewrite-accept)
                       ("C-c C-k" . superchat-rewrite-reject)
                       ("C-c C-c" . superchat-rewrite-cancel)
                       ("C-c C-r" . superchat-rewrite-iterate)
                       ("C-c C-d" . superchat-rewrite-diff)
                       ("C-c C-m" . superchat-rewrite-merge)
                       ("C-c C-n" . superchat-rewrite-next)
                       ("C-c C-p" . superchat-rewrite-previous)))
      (define-key map (kbd (car binding))
                  `(menu-item "" ,(cdr binding)
                              :filter superchat-rewrite--when-at-preview)))
    map)
  "The preview's `C-c' keys for point right after a preview.
Inside a preview its overlay keymap already applies; elsewhere each key
filters to nothing, so the major mode's own binding is used.  RET is not
here: right after a preview it stays the mode's (a new line).")

(define-minor-mode superchat-rewrite-preview-mode
  "Reach a rewrite preview's keys from right after it.
On while the buffer has a rewrite preview; see
`superchat-rewrite-preview-mode-map'."
  :lighter nil
  :keymap superchat-rewrite-preview-mode-map)

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
    ('retrying (let ((model (plist-get state :model)))
                 (concat "REWRITE  "
                         (superchat-sink-retry-text (plist-get state :retry))
                         (if model (format " [%s]" model) ""))))
    ('typing "REWRITE  Typing…")
    ('ready (concat "REWRITE  Ready"
                    (let ((times (plist-get state :longer)))
                      (when times
                        (format " (%d× longer than the selection: check it before accepting)"
                                times)))
                    "  C-c C-a accept  C-c C-k reject  C-c C-d diff  RET menu"))
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
              ((or 'waiting 'retrying) (propertize original 'face 'shadow))
              ('typing (let ((shown (and text (superchat-rewrite--stream-text text))))
                         (if (and shown (not (string-empty-p shown))) shown
                           (propertize original 'face 'shadow))))
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
    (with-current-buffer (overlay-buffer overlay)
      (superchat-rewrite-preview-mode 1))
    overlay))

(defun superchat-rewrite--delete-overlay (overlay)
  "Remove OVERLAY and its pending error timer."
  (when (overlayp overlay)
    (let ((timer (plist-get (superchat-rewrite--state overlay) :error-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (let ((buffer (overlay-buffer overlay)))
      (delete-overlay overlay)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (cl-some (lambda (other) (overlay-get other 'superchat-rewrite))
                           (overlays-in (point-min) (point-max)))
            (superchat-rewrite-preview-mode -1)))))))

(defun superchat-rewrite--unfence (text)
  "Return TEXT without one code fence around all of it."
  (if (string-match "\\`[ \t\n]*```[^`\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t\n]*\\'" text)
      (match-string 1 text)
    text))

(defun superchat-rewrite--stream-text (text)
  "Return the streamed TEXT so far without an opening or closing fence."
  (if (string-match "\\`[ \t\n]*```[^`\n]*\\(?:\n\\|\\'\\)" text)
      (replace-regexp-in-string "\n?```[ \t\n]*\\'" "" (substring text (match-end 0)))
    text))

(defun superchat-rewrite--answer (answer original)
  "Check and clean the model's ANSWER for replacing ORIGINAL.
Return (:error MESSAGE) when ANSWER contains the request's markers, else
\(:text TEXT :longer TIMES): TEXT without a fence around it and ending in a
newline exactly when ORIGINAL does; TIMES, when TEXT is more than three
times as long as ORIGINAL and more than 200 characters longer, is how many
times longer it is."
  (if (string-match-p superchat-rewrite--marker-regexp answer)
      (list :error "the answer contains the request's markers (<<<…>>>), so it is not a replacement for the selection")
    (let ((text (superchat-rewrite--unfence answer)))
      (cond ((and (string-suffix-p "\n" original) (not (string-suffix-p "\n" text)))
             (setq text (concat text "\n")))
            ((and (not (string-suffix-p "\n" original)) (string-suffix-p "\n" text))
             (setq text (substring text 0 -1))))
      (list :text text
            :longer (let ((new (length text)) (old (max 1 (length original))))
                      (when (and (> new (* 3 old)) (> (- new old) 200))
                        (round (/ new (float old)))))))))

(defun superchat-rewrite--sink (overlay _lang-tag)
  "Return the preview Sink for OVERLAY (contract §3).
Stream chunks accumulate into the preview; the final text, checked and
cleaned by `superchat-rewrite--answer', becomes the ready preview; a failure
shows its text for `superchat-rewrite--error-display-seconds' and then
removes the overlay; a cancellation removes it at once."
  (superchat-sink-create
   :id (format "rewrite-%08x" (random #x100000000))
   :live-p (lambda ()
             (and (overlayp overlay)
                  (overlay-buffer overlay)
                  (buffer-live-p (overlay-buffer overlay))))
   :status (lambda (run status)
             (pcase status
               ('waiting
                (superchat-rewrite--set-state overlay :status 'waiting)
                (superchat-rewrite--set-state overlay :retry nil))
               ('retrying
                (superchat-rewrite--set-state overlay :retry (plist-get run :retry))
                (superchat-rewrite--set-state overlay :status 'retrying))
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
   :final (lambda (run result)
            (let* ((state (superchat-rewrite--state overlay))
                   (answer (superchat-rewrite--answer
                            (or (plist-get result :text) "")
                            (plist-get state :original))))
              (if (plist-get answer :error)
                  (superchat-rewrite--fail overlay (plist-get answer :error))
                (overlay-put overlay 'superchat-rewrite
                             (plist-put (plist-put state :text (plist-get answer :text))
                                        :longer (plist-get answer :longer)))
                (superchat-rewrite--set-state overlay :status 'ready)
                (superchat-rewrite--tell-conversation overlay run)
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

(defun superchat-rewrite--tell-conversation (overlay run)
  "Show OVERLAY's finished rewrite RUN in the conversation's chat window."
  (let* ((run-id (if (stringp run) run (plist-get run :run-id)))
         (buffer (overlay-buffer overlay)))
    (when (and (stringp run-id) (buffer-live-p buffer)
               (fboundp 'superchat-conversation-show-turn))
      (superchat-conversation-show-turn
       (plist-get (superchat-rewrite--state overlay) :session-id) run-id
       (list :kind "region rewrite" :buffer buffer :file (buffer-file-name buffer)
             :line (with-current-buffer buffer
                     (line-number-at-pos (overlay-start overlay))))))))

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

(defun superchat-rewrite--content (buffer beg end instruction base)
  "Return the model input for rewriting BASE under INSTRUCTION.
BASE is the text at BEG..END of BUFFER on a first ask, which then carries
the Rewrite reference around it, or the previous answer when iterating,
which goes alone.  The reference, the target and the instruction are
labelled parts, the instruction last."
  (with-current-buffer buffer
    (let* ((first-ask (equal base (buffer-substring-no-properties beg end)))
           (reference (and first-ask (superchat-rewrite--reference beg end))))
      (concat
       (when reference
         (concat
          "Reference: the text around the target, so you understand its setting. "
          "Read it; do not change it and do not include it in your answer.\n"
          (unless (string-empty-p (car reference))
            (concat "<<<REFERENCE BEFORE>>>\n" (car reference)
                    "\n<<<END REFERENCE BEFORE>>>\n"))
          (unless (string-empty-p (cdr reference))
            (concat "<<<REFERENCE AFTER>>>\n" (cdr reference)
                    "\n<<<END REFERENCE AFTER>>>\n"))
          "\n"))
       "The target, the only text to rewrite:\n<<<TARGET>>>\n"
       (string-remove-suffix "\n" base)
       "\n<<<END TARGET>>>\n\n"
       "Instruction: " instruction "\n\n"
       "Reply with the replacement for the target only."))))

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
                :content (superchat-rewrite--content buffer beg end instruction base)
                :tools 'none
                :system-prompt
                (or superchat-rewrite-system-prompt
                    (with-current-buffer buffer (superchat-rewrite--system-text)))
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
