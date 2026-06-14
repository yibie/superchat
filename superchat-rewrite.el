;;; superchat-rewrite.el --- Deterministic region rewrite with ediff preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Deterministic region rewriting: select code → instruct LLM →
;; preview diff with ediff → apply or reject.  No tool-calling;
;; the LLM response is parsed for fenced code blocks.
;;
;; v1 extensions:
;;   - multi-region / rectangle support (region-noncontiguous-p)
;;   - apply-code-block-at-point (reply code blocks as patches)

;;; Code:

(require 'cl-lib)
(require 'ediff)

;; ── Forward declarations ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function superchat--build-llm-prompt "superchat-llm" (text tools))
(declare-function llm-make-chat-prompt "llm" (text &rest args))
(declare-function llm-make-context "llm" (&rest args))
(declare-function llm-chat "llm" (provider prompt &optional multi-output))
(declare-function llm-chat-prompt-context "llm" (prompt))
(declare-function llm-chat-prompt-interactions "llm" (prompt))

(defgroup superchat-rewrite nil
  "Region rewriting with LLM + ediff preview."
  :group 'superchat)

(defcustom superchat-rewrite-confirmation 'ediff
  "How to confirm rewrites before applying them.
`ediff'  — always show an ediff session.
`auto'   — apply immediately (still undo-able)."
  :type '(choice (const :tag "Ediff preview" ediff)
                 (const :tag "Auto-apply (undo-able)" auto))
  :group 'superchat-rewrite)

(defcustom superchat-rewrite-system-prompt
  "You are a precise code editor. Rewrite the provided code according \
to the user's instruction. Return ONLY the rewritten code inside a \
fenced code block with the appropriate language tag. Do not include \
explanations, apologies, or markdown outside the code fence. \
Preserve the original indentation style."
  "System prompt used for region rewrites."
  :type 'string
  :group 'superchat-rewrite)

(defcustom superchat-rewrite-multi-region-separator "// === BLOCK %d ===\n"
  "Format string for separating regions in multi-region prompts.
%d is replaced with the 1-indexed block number."
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
Supports non-contiguous (rectangle / multiple) regions via
`region-noncontiguous-p'.  Multi-region mode collects all
segments, sends them as labelled blocks, and applies the LLM's
response block-by-block — one ediff per segment.

When `superchat-rewrite-confirmation' is `auto', rewrites are
applied directly without ediff (still undo-able)."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Rewrite instruction: "))
     (user-error "No active region")))
  (unless superchat-llm-backend
    (user-error "superchat-llm-backend is not configured"))
  (let ((current-lang (symbol-name major-mode)))
    (if (and (fboundp 'region-noncontiguous-p)
             (region-noncontiguous-p))
        (superchat-rewrite--multi-region current-lang instruction)
      (superchat-rewrite--single-region beg end instruction current-lang))))

;; ── Single region (v1 behaviour — preserved byte-for-byte) ──

(defun superchat-rewrite--single-region (beg end instruction lang-tag)
  "Rewrite a single contiguous region BEG..END."
  (let* ((original (buffer-substring-no-properties beg end))
         (prompt (format
                  "Rewrite the following %s code according to this instruction: %s\n\n```%s\n%s\n```"
                  lang-tag instruction lang-tag original))
         (rewritten (superchat-rewrite--call-llm prompt lang-tag)))
    (unless rewritten
      (user-error "LLM returned no usable code block"))
    (superchat-rewrite--record-source (buffer-name) beg end)
    (superchat-rewrite--apply beg end original rewritten lang-tag)))

;; ── Multi region (rectangle / non-contiguous) ──

(defun superchat-rewrite--multi-region (lang-tag instruction)
  "Rewrite multiple non-contiguous regions."
  (let* ((segments (superchat-rewrite--collect-region-segments))
         (n (length segments)))
    (unless segments
      (user-error "No region segments found"))
    ;; Build a prompt with each segment in a labelled code block.
    (let ((prompt-parts
           (list (format "Rewrite the following %d %s code blocks according to this instruction: %s\n\n"
                         n lang-tag instruction))))
      (cl-loop for seg in segments
               for i from 1
               do (push (format "%s```%s\n%s\n```\n"
                                (format superchat-rewrite-multi-region-separator i)
                                lang-tag
                                (nth 0 seg))
                        prompt-parts))
      (push (format "Return exactly %d fenced code blocks, in the same order, with no extra commentary."
                    n)
            prompt-parts)
      (let* ((prompt (string-join (nreverse prompt-parts)))
             (response-text (superchat-rewrite--call-llm-raw prompt lang-tag))
             (blocks (superchat-rewrite--extract-all-code-blocks response-text lang-tag)))
        (unless blocks
          (user-error "LLM returned no code blocks"))
        (unless (= (length blocks) n)
          (user-error "Block count mismatch: expected %d, got %d.  Aborting."
                      n (length blocks)))
        ;; Record source before applying
        (superchat-rewrite--record-source (buffer-name)
                                          (nth 1 (car segments))
                                          (nth 2 (car segments)))
        ;; Apply each block to its corresponding segment
        (cl-loop for seg in segments
                 for blk in blocks
                 for i from 1
                 do (let ((seg-beg (nth 1 seg))
                          (seg-end (nth 2 seg))
                          (original (nth 0 seg)))
                      (message "Applying block %d/%d to region %d..%d"
                               i n seg-beg seg-end)
                      (superchat-rewrite--apply seg-beg seg-end original blk lang-tag)))))))

(defun superchat-rewrite--collect-region-segments ()
  "Return a list of (TEXT BEG END) for each non-contiguous region segment.
Returns nil when `region-noncontiguous-p' is false or unsupported."
  (when (and (fboundp 'region-noncontiguous-p)
             (region-noncontiguous-p))
    (let ((segments nil))
      (dolist (reg (funcall #'region-extract-function
                            'bounds))
        (let ((beg (car reg))
              (end (cdr reg)))
          (push (list (buffer-substring-no-properties beg end)
                      beg end)
                segments)))
      (nreverse segments))))

;; ── LLM call helpers ──

(defun superchat-rewrite--call-llm (prompt lang-tag)
  "Send PROMPT to the LLM and extract the rewritten code.
Returns the FIRST code block (stripped of fences), or nil on failure."
  (let ((text (superchat-rewrite--call-llm-raw prompt lang-tag)))
    (superchat-rewrite--extract-code-block text lang-tag)))

(defun superchat-rewrite--call-llm-raw (prompt lang-tag)
  "Send PROMPT to the LLM and return the raw response text.
Uses a blocking `llm-chat' without tools for deterministic output."
  (unless (fboundp 'llm-make-chat-prompt)
    (error "llm.el is not available"))
  (let* ((effective-backend
          (if (fboundp 'superchat--effective-llm-backend)
              (funcall 'superchat--effective-llm-backend)
            superchat-llm-backend))
         (system-prompt
          (format "%s\n\nUse language tag `%s' for the code fence."
                  superchat-rewrite-system-prompt lang-tag))
         (full-prompt
          (if (fboundp 'superchat--build-llm-prompt)
              (funcall 'superchat--build-llm-prompt prompt nil)
            (llm-make-chat-prompt prompt)))
         (context (llm-make-context))
         (result-text nil))
    (setf (llm-chat-prompt-context full-prompt) context)
    (when system-prompt
      (setf (llm-chat-prompt-interactions full-prompt)
            `((:role system :content ,system-prompt))))
    (condition-case err
        (let ((result (llm-chat effective-backend full-prompt)))
          (setq result-text
                (cond
                 ((stringp result) result)
                 ((and (consp result) (stringp (plist-get result :text)))
                  (plist-get result :text))
                 (t (format "%S" result)))))
      (error
       (user-error "LLM call failed: %s" (error-message-string err))))
    result-text))

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
            (push (match-string 1) blocks))
          (when blocks
            (goto-char (point-min))
            (while (or (re-search-forward pattern nil t)
                       (re-search-forward fallback nil t))
              ;; Already collected; just advance
              nil))))
      (nreverse blocks))))

;; ── Apply helpers ──

(defun superchat-rewrite--apply (beg end original rewritten lang-tag)
  "Apply the REWRITTEN code to the region BEG..END.
ORIGINAL is the current buffer text; LANG-TAG is the language tag.
Shows an ediff session unless `superchat-rewrite-confirmation' is `auto'."
  (if (eq superchat-rewrite-confirmation 'auto)
      (superchat-rewrite--do-replace beg end rewritten)
    (superchat-rewrite--ediff-and-replace beg end original rewritten lang-tag)))

(defun superchat-rewrite--do-replace (beg end new-text)
  "Replace the region BEG..END with NEW-TEXT in one atomic undo step."
  (atomic-change-group
    (goto-char beg)
    (delete-region beg end)
    (insert new-text))
  (message "Region rewritten (%d chars → %d chars). Undo to revert."
           (- end beg) (length new-text)))

(defun superchat-rewrite--ediff-and-replace (beg end original rewritten lang-tag)
  "Show an ediff session comparing ORIGINAL vs REWRITTEN.
LANG-TAG is used for buffer names.  If the user accepts,
replace the region BEG..END with the rewritten text."
  (let ((buf-a (generate-new-buffer (format "*superchat-rewrite-original <%s>*"
                                            lang-tag)))
        (buf-b (generate-new-buffer (format "*superchat-rewrite-rewritten <%s>*"
                                            lang-tag))))
    (with-current-buffer buf-a
      (insert original)
      (condition-case nil
          (funcall (intern lang-tag) nil t)
        (error nil))
      (goto-char (point-min)))
    (with-current-buffer buf-b
      (insert rewritten)
      (condition-case nil
          (funcall (intern lang-tag) nil t)
        (error nil))
      (goto-char (point-min)))
    (let ((ediff-quit-hook
           (lambda ()
             (superchat-rewrite--on-ediff-quit
              beg end rewritten buf-a buf-b))))
      (ediff-buffers buf-a buf-b))))

(defun superchat-rewrite--on-ediff-quit (beg end rewritten buf-a buf-b)
  "Handle ediff session completion.
Prompts the user for confirmation, then applies the rewrite or cleans up."
  (unwind-protect
      (when (y-or-n-p "Apply this rewrite to the buffer? ")
        (with-current-buffer (marker-buffer (or (and (markerp beg) beg)
                                                beg))
          (goto-char beg)
          (atomic-change-group
            (delete-region beg end)
            (insert rewritten)))
        (message "Region rewritten. Undo to revert."))
    (ignore-errors (kill-buffer buf-a))
    (ignore-errors (kill-buffer buf-b))))

;; ── Apply code block at point (Part A: reply → patch) ──

;;;###autoload
(defun superchat-rewrite-apply-code-block-at-point ()
  "Apply the code block at point as a patch to the last source buffer.
Finds the nearest `#+begin_src ... #+end_src' org block, extracts
its content, and applies it to the source buffer recorded by the
last send-region/send-defun/rewrite-region operation.  When no
source is recorded, prompts for a target buffer.

The replacement is ediff-previewed and undo-able."
  (interactive)
  (let ((block-info (superchat-rewrite--code-block-at-point)))
    (unless block-info
      (user-error "No org source block found at point"))
    (let* ((code (car block-info))
           (lang-tag (cdr block-info))
           (target-buf (superchat-rewrite--resolve-target-buffer)))
      (unless target-buf
        (user-error "No target buffer — send a region/defun first, or select a buffer"))
      (let ((target-region (superchat-rewrite--resolve-target-region target-buf)))
        (if target-region
            ;; Have a known region: ediff preview + apply
            (let ((beg (car target-region))
                  (end (cdr target-region))
                  (original (with-current-buffer target-buf
                              (buffer-substring-no-properties
                               (car target-region) (cdr target-region)))))
              (with-current-buffer target-buf
                (superchat-rewrite--ediff-and-replace beg end original code lang-tag)))
          ;; No region: append to end of target buffer
          (with-current-buffer target-buf
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((start (point)))
              (atomic-change-group
                (insert code "\n"))
              (message "Code block appended to %s at position %d (undo to revert)."
                       (buffer-name) start))))))))

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
