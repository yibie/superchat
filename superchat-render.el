;;; superchat-render.el --- Buffer rendering for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Chat buffer rendering: MD→Org conversion, prompt insertion,
;; streaming display, TTFT annotation, and response finalization.
;; Extracted from superchat.el monolith (v0.9 split step 4).

;;; Code:

(require 'cl-lib)
(require 'superchat-preset)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-buffer-name)
(defvar superchat--response-start-marker)
(defvar superchat--assistant-response-start-marker)
(defvar superchat--assistant-response-end-marker)
(defvar superchat--response-region-persistent-marker)
(defvar-local superchat--response-region-states nil
  "Per-owner response-region state for this View.

Each state owns its markers and stream parts.  The registry is the only
additional state needed for interleaved Runs; callers still select placement
through the existing owner-bearing response-region API.")
(defvar-local superchat--response-region-current-owner nil)
(defvar superchat--runtime-transcript-owner)
(defvar superchat--prompt-start)
(defvar superchat--queued-follow-up)
(defvar superchat--active-preset)
(defvar superchat--current-turn)
(defvar superchat--current-response-parts)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat--ttft-start-time)
(defvar superchat--ttft-elapsed)
(defvar superchat--nonpartial-e2e-elapsed)
(defvar superchat-reasoning-max-chars 20000
  "Maximum number of characters retained in one Thinking block.
When the limit is reached, the renderer keeps the newest tail and inserts
an explicit truncation notice at the front of the block.")
(defvar superchat-file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-inline-context-template)
(defvar superchat-inline-max-bytes)
(defvar superchat--context-files nil)
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--read-inline-file-content "superchat" (file-path))
(declare-function superchat--render-inline-context "superchat" (file-path content))
(declare-function superchat--textual-file-p "superchat" (file-path))
(declare-function superchat--record-message
                  "superchat" (role content &optional existing-tape-id))
(declare-function superchat--refresh-thinking-status
                  "superchat" (&optional owner))

(defvar superchat-reasoning-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'superchat-toggle-reasoning)
    (define-key map [tab] #'superchat-toggle-reasoning)
    map)
  "Keymap used by a folded reasoning heading.")

(defun superchat-render-markdown-to-org (md-string)
  "Convert MD-STRING from Markdown to Org-mode format.
This function is adapted from ollama-buddy's implementation,
using a robust two-pass approach to handle code blocks correctly.
This is a pure function that takes a string and returns a converted string."
  (if (string-empty-p md-string)
      ""
    (with-temp-buffer
      (insert md-string)
      (save-excursion
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (save-match-data
            ;; First, handle code blocks by temporarily protecting their content
            (goto-char (point-min))
            (let ((code-blocks nil)
                  (counter 0)
                  block-start block-end lang content placeholder)
              (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
                (setq lang (match-string 1)
                      content (match-string 2)
                      block-start (match-beginning 0)
                      block-end (match-end 0)
                      placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
                (push (list placeholder lang content) code-blocks)
                (delete-region block-start block-end)
                (goto-char block-start)
                (insert placeholder)
                (setq counter (1+ counter)))

              ;; Apply regular Markdown to Org transformations
              (goto-char (point-min))
              (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
                (replace-match (concat (match-string 1) "- \\2")))

              (goto-char (point-min))
              (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
                (replace-match "*\\1*"))

              (goto-char (point-min))
              (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
                (replace-match "\\1/\\2/\\3"))

              (goto-char (point-min))
              (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
                (replace-match "[[\\2][\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "`\\(.*?\\)`" nil t)
                (replace-match "=\\1="))

              (goto-char (point-min))
              (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
                (replace-match "-----"))

              (goto-char (point-min))
              (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
                (replace-match "[[\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "^\\(#+\\) " nil t)
                (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))

              ;; (goto-char (point-min))
              ;; (while (re-search-forward "—" nil t)
              ;;   (replace-match ", "))

              ;; Restore code blocks
              (dolist (block (nreverse code-blocks))
                (let ((placeholder (nth 0 block))
                      (lang (nth 1 block))
                      (content (nth 2 block)))
                  (goto-char (point-min))
                  (when (search-forward placeholder nil t)
                    (replace-match (format "#+begin_src %s\n%s\n#+end_src" (or lang "") content) t t))))))))
      (buffer-string))))

(defun superchat--format-assistant-answer (content)
  "Format CONTENT with recognized headings beneath the level-two Assistant.
Preserve relative depth and code semantics using standard Org comma escaping
in the View; raw copying may include those commas.  This retains the existing
Markdown single-star list ambiguity, not arbitrary native Org detection.
Generic callers keep `superchat-render-markdown-to-org' without this nesting."
  (require 'org)
  (let ((caller (current-buffer)))
    (with-temp-buffer
      (insert content)
      (let ((case-fold-search t) blocks (counter 0))
        ;; Protect complete fenced/native source blocks before Markdown parsing.
        ;; Placeholders are local formatting data, never View/owner identities.
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*\\(```+\\|~~~+\\|#\\+begin_src\\b\\)\\(.*\\)$" nil t)
          (let* ((start (match-beginning 0))
                 (delimiter (match-string 1))
                 (native (string-prefix-p "#" delimiter))
                 (header (if native (match-string 0)
                           (concat "#+begin_src " (string-trim (match-string 2)))))
                 (end-pattern (if native "^[ \t]*#\\+end_src[^\n]*"
                                (concat "^[ \t]*" (regexp-quote delimiter) "[ \t]*$")))
                 (body-start (progn (forward-line 1) (point))))
            (when (re-search-forward end-pattern nil t)
              (let* ((body (buffer-substring body-start (match-beginning 0)))
                     (block (concat header "\n" (org-escape-code-in-string body)
                                    (if native (match-string 0) "#+end_src")))
                     token)
                (while (progn
                         (setq token (format "SUPERCHAT_ANSWER_SOURCE_%d" counter)
                               counter (1+ counter))
                         (string-match-p (regexp-quote token) content)))
                (push (cons token block) blocks)
                (delete-region start (point))
                (insert token)))))
        ;; Keep the generic formatter's caller context and O3 pre-mutation seam.
        (let* ((protected (buffer-string))
               (formatted (with-current-buffer caller
                            (superchat-render-markdown-to-org protected))))
          (erase-buffer)
          (insert formatted))
        (let ((org-inhibit-startup t))
          (delay-mode-hooks (org-mode)))
        (let ((positions (org-element-map (org-element-parse-buffer) 'headline
                           (lambda (headline) (org-element-property :begin headline)))))
          (dolist (position (reverse positions))
            (goto-char position)
            (insert "**")))
        (dolist (block blocks)
          (goto-char (point-min))
          (when (search-forward (car block) nil t)
            (replace-match (cdr block) t t)))
        (buffer-string)))))


(defun superchat--insert-prompt (&optional transcript-owner)
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (let* ((owner (or transcript-owner :pending))
             (state (superchat--response-region--state-for-owner owner transcript-owner)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n")  ; Always insert a blank line before the prompt
        (let* ((preset-label
                (when (and (boundp 'superchat--active-preset)
                           superchat--active-preset)
                  (format "%s:%s"
                          (superchat-preset-type superchat--active-preset)
                          (superchat-preset-name superchat--active-preset))))
               (headline
                (cond
                 ((and superchat--current-command preset-label)
                  (format "* User [%s mode | %s]: " superchat--current-command preset-label))
                 (superchat--current-command
                  (format "* User [%s mode]: " superchat--current-command))
                 (preset-label
                  (format "* User [%s]: " preset-label))
                 (t "* User: "))))
          (insert (propertize headline 'face 'superchat-prompt-face
                              'superchat-prompt-header t
                              'rear-nonsticky '(superchat-prompt-header)))
          (setq superchat--prompt-start (point-marker))
          (set-marker-insertion-type superchat--prompt-start nil)
          (plist-put state :prompt-marker superchat--prompt-start)
          ;; A prompt is a real boundary for this owner even before its first
          ;; stream chunk.  Keeping the marker here lets a workflow callback
          ;; find its transcript block after the prompt has been consumed by a
          ;; concurrent ordinary request.
          (unless (plist-get state :assistant-end)
            (plist-put state :assistant-end
                       (copy-marker
                        (save-excursion
                          (goto-char superchat--prompt-start)
                          (line-beginning-position))
                        nil)))
          (when (and (stringp superchat--queued-follow-up)
                     (not (string-empty-p superchat--queued-follow-up)))
            (insert superchat--queued-follow-up)
            (setq superchat--queued-follow-up nil)
            (force-mode-line-update t)
            (when (fboundp 'superchat-agents-panel-refresh)
              (superchat-agents-panel-refresh)))
          (superchat--response-region--publish-state state transcript-owner))))))

(defun superchat--response-region--view-prompt ()
  "Return the existing editable prompt marker, if its header remains intact.
Identity comes from the state association.  The dedicated header property
validates its boundary independently of font-lock, without matching text."
  (cl-loop for (_owner . state) in superchat--response-region-states
           for marker = (plist-get state :prompt-marker)
           when (and (superchat--response-region--marker-position marker)
                     (> marker (point-min))
                     (get-text-property (1- marker) 'superchat-prompt-header))
           return marker))

(defun superchat--response-region-ensure-view-prompt (&optional _independent)
  "Reuse the editable View prompt, associated with pending, without reselection."
  (let ((prompt (superchat--response-region--view-prompt)))
    (if prompt
        (let ((pending (superchat--response-region--state-for-owner :pending t)))
          (dolist (entry superchat--response-region-states)
            (unless (eq (cdr entry) pending)
              (when (eq prompt (plist-get (cdr entry) :prompt-marker))
                (plist-put (cdr entry) :prompt-marker nil))))
          (plist-put pending :prompt-marker prompt)
          (setq superchat--prompt-start prompt))
      (superchat--insert-prompt :pending))))

(defun superchat--prepare-for-response ()
  "Move to end of buffer, insert newline, and set marker for response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let* ((state
            (superchat--response-region--state-for-owner
             (if superchat--runtime-transcript-owner
                 superchat--runtime-transcript-owner
               :pending)))
           (inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (setq superchat--ttft-elapsed nil
            superchat--nonpartial-e2e-elapsed nil)
      (plist-put state :status-marker (point-marker))
      (superchat--response-region--select-state state))))

(defun superchat--update-status (message &optional transcript-owner)
  "Update the status MESSAGE in the chat buffer (e.g., 'Assistant is thinking...')."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((state (superchat--response-region--state-for-owner transcript-owner transcript-owner))
             (marker (superchat--response-region--state-marker-position
                      state :status-marker))
             (inhibit-read-only t))
        (unless marker
          (superchat--response-region--reserve-before-prompt state)
          (setq marker
                (or (superchat--response-region--state-marker-position
                     state :assistant-start)
                    (superchat--response-region--state-marker-position
                     state :assistant-end)
                    (superchat--response-region--prompt-line-position state)
                    (point-max)))
          (goto-char marker)
          (unless (bolp) (insert "\n"))
          (setq marker (point-marker))
          (plist-put state :status-marker marker))
        (goto-char marker)
        (delete-region (point) (line-end-position))
        (insert (propertize message 'face 'superchat-status-active-face))
        (superchat--response-region--publish-state state transcript-owner)))))

(defmacro superchat--ttft-log (stage)
  "Log STAGE with elapsed time from `superchat--ttft-start-time'.
Only active when `superchat-show-ttft-breakdown' is non-nil.
STAGE is a string label describing the pipeline step."
  `(when (and superchat-show-ttft-breakdown superchat--ttft-start-time)
     (message "⏱ TTFT [%s]: %.3fs" ,stage
              (- (float-time) superchat--ttft-start-time))))

(defun superchat--response-region--marker-position (marker)
  "Return MARKER's position when it belongs to the current buffer."
  (and (markerp marker)
       (marker-position marker)
       (eq (marker-buffer marker) (current-buffer))
       (marker-position marker)))

(defun superchat--response-region--detach-variable (variable)
  "Detach the marker stored in VARIABLE and set VARIABLE to nil."
  (when (boundp variable)
    (let ((marker (symbol-value variable)))
      (when (markerp marker)
        (set-marker marker nil))
      (set variable nil))))

(defun superchat--response-region--effective-owner (&optional owner)
  "Return the concrete response owner for OWNER's placement contract."
  (pcase owner
    (:current-run
     (or superchat--runtime-transcript-owner
         superchat--response-region-current-owner
         :pending))
    (:independent-turn
     (or superchat--response-region-current-owner :pending))
    ((pred identity) owner)
    (_ (or superchat--runtime-transcript-owner
           superchat--response-region-current-owner
           :pending))))

(defun superchat--response-region--new-state (owner)
  "Return an empty response-region state owned by OWNER."
  (list :owner owner
        :status-marker nil
        :assistant-start nil
        :assistant-end nil
        :persistent-marker nil
        :prompt-marker nil
        :parts nil
        :reasoning-parts nil
        :reasoning-text nil
        :reasoning-latest nil
        :reasoning-body-start nil
        :reasoning-body-end nil
        :reasoning-overlay nil
        :reasoning-overlays nil
        :reasoning-folded t))

(defun superchat--response-region--state-for-owner (&optional owner explicit)
  "Return the View-local response state for OWNER.

A pending state is adopted by `superchat--response-region-begin' or a legacy
implicit lookup.  This is the only owner-to-marker association in the renderer; no
caller needs to manufacture or inspect a marker of its own.
EXPLICIT lookups never adopt pending or import compatibility variables."
  (let* ((owner (superchat--response-region--effective-owner owner))
         (entry (assoc owner superchat--response-region-states))
         (state (cdr entry)))
    (unless state
      (when (and (not explicit) (not (eq owner :pending))
                 (setq entry (assoc :pending superchat--response-region-states))
                 (null (plist-get (cdr entry) :prompt-marker)))
        (setq state (cdr entry))
        (plist-put state :owner owner)
        (setcar entry owner))
      (unless state
        (setq state (superchat--response-region--new-state owner))
        ;; Compatibility for callers/tests that set the pre-registry marker
        ;; variables directly before entering the response API.
        (when (and (not explicit) (null superchat--response-region-states))
          (plist-put state :status-marker superchat--response-start-marker)
          (plist-put state :assistant-start
                     superchat--assistant-response-start-marker)
          (plist-put state :assistant-end
                     superchat--assistant-response-end-marker)
          (plist-put state :persistent-marker
                     superchat--response-region-persistent-marker)
          (plist-put state :prompt-marker superchat--prompt-start)
          (plist-put state :parts superchat--current-response-parts))
        (push (cons owner state) superchat--response-region-states)))
    state))

(defun superchat--response-region-begin (owner)
  "Initialize OWNER once, adopting only consumed/prepared pending state.
An existing owner's repeated handoff never reselects it."
  (when (and owner (not (assoc owner superchat--response-region-states)))
    (let ((state (superchat--response-region--state-for-owner owner)))
      (superchat--response-region--reserve-before-prompt state)
      (superchat--response-region--select-state state))))

(defun superchat--response-region--reserve-before-prompt (state)
  "Give STATE its own line before an unsubmitted View prompt when unbounded."
  (when (and (not (plist-get state :assistant-end))
             (not (plist-get state :assistant-start)))
    (when-let* ((prompt (superchat--response-region--view-prompt)))
      (save-excursion
        (goto-char prompt)
        (beginning-of-line)
        (let ((position (point)) (inhibit-read-only t))
          (insert-before-markers "\n")
          (plist-put state :assistant-end (copy-marker position nil)))))))

(defun superchat--response-region--publish-state (state explicit)
  "Refresh STATE's compatibility projection when allowed by EXPLICIT addressing."
  (when (or (not explicit)
            (equal (plist-get state :owner) superchat--response-region-current-owner))
    (superchat--response-region--select-state state)))

(defun superchat--response-region--select-state (state)
  "Make STATE the current compatibility projection and return it."
  (let* ((prompt-state
          (or (and (superchat--response-region--state-marker-position
                    state :prompt-marker)
                   state)
              (cl-find-if
               (lambda (entry)
                 (and (eq (plist-get (cdr entry) :owner) :pending)
                      (superchat--response-region--state-marker-position
                       (cdr entry) :prompt-marker)))
               superchat--response-region-states)
              (cl-find-if
               (lambda (entry)
                 (superchat--response-region--state-marker-position
                  (cdr entry) :prompt-marker))
               superchat--response-region-states)))
         (prompt-marker
          (or (and prompt-state
                   (plist-get prompt-state :prompt-marker))
              ;; A callback can select its own Run while the editable prompt
              ;; belongs to another state.  Preserve the live input marker as
              ;; a compatibility projection even if that state is temporarily
              ;; outside the current buffer selection.
              (and (markerp superchat--prompt-start)
                   (marker-position superchat--prompt-start)
                   superchat--prompt-start))))
    (setq superchat--response-region-current-owner (plist-get state :owner)
          superchat--response-start-marker (plist-get state :status-marker)
          superchat--assistant-response-start-marker
          (plist-get state :assistant-start)
          ;; A prompt-only boundary belongs to the owner-keyed registry but
          ;; is not an open assistant response.  Keep the legacy
          ;; compatibility projection nil until the Run opens its block.
          superchat--assistant-response-end-marker
          (and (plist-get state :assistant-start)
               (plist-get state :assistant-end))
          superchat--response-region-persistent-marker
          (plist-get state :persistent-marker)
          superchat--prompt-start prompt-marker
          superchat--current-response-parts (plist-get state :parts)))
  state)

(defun superchat--response-region--state-marker-position (state key)
  "Return STATE marker KEY's position in the current buffer."
  (superchat--response-region--marker-position (plist-get state key)))

(defun superchat--response-region--detach-state-marker (state key)
  "Detach marker KEY from STATE."
  (let ((marker (plist-get state key)))
    (when (markerp marker)
      (set-marker marker nil))
    (plist-put state key nil)))

(defun superchat--response-region--prompt-line-position (&optional state)
  "Return STATE's prompt line beginning, or nil."
  (let ((state (or state
                   (superchat--response-region--state-for-owner))))
    (let ((position (or (superchat--response-region--state-marker-position
                        state :prompt-marker)
                       (superchat--response-region--view-prompt))))
      (when position
        (save-excursion
          (goto-char position)
          (line-beginning-position))))))

(defun superchat--response-region-clear-status (&optional owner)
  "Remove the transient status line owned by OWNER's response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((state (superchat--response-region--state-for-owner owner owner))
             (marker (superchat--response-region--state-marker-position
                      state :status-marker)))
        (when marker
          (let ((inhibit-read-only t))
            (goto-char marker)
            (delete-region (point) (line-end-position))))
        (superchat--response-region--detach-state-marker state :status-marker)
        (superchat--response-region--publish-state state owner)))))

(defun superchat--response-region-consume-prompt ()
  "Detach the prompt boundary consumed by the current input.

The assistant end marker, when present, remains in place so an older Run can
continue streaming before the newly submitted user row."
  (let* ((prompt superchat--prompt-start)
         (state
          (or (cl-loop for entry in superchat--response-region-states
                       for candidate = (cdr entry)
                       when (eq prompt (plist-get candidate :prompt-marker))
                       return candidate)
              (superchat--response-region--state-for-owner))))
    (when (and prompt (eq prompt (plist-get state :prompt-marker)))
      ;; The prompt marker starts at the headline, while the user may have
      ;; appended arbitrary text after it.  Move the owner boundary to the
      ;; actual end of the submitted row before detaching the input marker
      ;; when this owner has not opened an assistant block yet.  An already
      ;; streaming owner keeps its boundary before the prompt so late output
      ;; remains before the newly submitted row.
      (when (and (not (superchat--response-region--state-active-p state))
                 (plist-get state :assistant-end))
        (set-marker (plist-get state :assistant-end) (point-max) nil))
      (superchat--response-region--detach-state-marker state :prompt-marker))
    (setq superchat--prompt-start nil)
    (superchat--response-region--select-state state)))

(defun superchat--response-region-reset ()
  "Detach all response-region state and discard stream parts."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (dolist (entry superchat--response-region-states)
      (let ((state (cdr entry)))
        (dolist (key '(:status-marker :assistant-start :assistant-end
                       :persistent-marker :prompt-marker
                       :reasoning-body-start :reasoning-body-end))
          (superchat--response-region--detach-state-marker state key))
        (dolist (overlay (plist-get state :reasoning-overlays))
          (when (overlayp overlay)
            (delete-overlay overlay)))))
    (dolist (variable '(superchat--response-start-marker
                        superchat--assistant-response-start-marker
                        superchat--assistant-response-end-marker
                        superchat--response-region-persistent-marker
                        superchat--prompt-start))
      (superchat--response-region--detach-variable variable))
    (setq superchat--response-region-states nil
          superchat--response-region-current-owner nil
          superchat--current-response-parts nil
          superchat--ttft-elapsed nil
          superchat--nonpartial-e2e-elapsed nil)))

(defun superchat--response-region--state-active-p (state)
  "Return non-nil when STATE has an open assistant response region."
  (superchat--response-region--state-marker-position state :assistant-start))

(defun superchat--response-region--active-p ()
  "Return non-nil when the current response state is active."
  (superchat--response-region--state-active-p
   (superchat--response-region--state-for-owner)))

(defun superchat--response-region--assistant-tail-position (&optional state)
  "Return STATE's current assistant tail position."
  (let ((state (or state (superchat--response-region--state-for-owner))))
    (or (superchat--response-region--state-marker-position
         state :assistant-end)
        (superchat--response-region--prompt-line-position state)
        (point-max))))

(defun superchat--response-region--current-run-transcript-position
    (&optional state)
  "Return the insertion position for STATE's current Run transcript."
  (let ((state (or state (superchat--response-region--state-for-owner))))
    (or (superchat--response-region--state-marker-position
         state :assistant-end)
        (superchat--response-region--state-marker-position
         state :persistent-marker)
        (superchat--response-region--prompt-line-position state)
        (point-max))))

(defun superchat--response-region--position-outside-current-run-p
    (position &optional state)
  "Return non-nil when POSITION is outside STATE's active subregion."
  (let* ((state (or state (superchat--response-region--state-for-owner)))
         (start (superchat--response-region--state-marker-position
                 state :assistant-start))
         (end (superchat--response-region--state-marker-position
               state :assistant-end)))
    (or (null start)
        (< position start)
        (and end (>= position end)))))

(defun superchat--response-region--discard-current-reasoning (state)
  "Discard the in-progress reasoning block of STATE."
  (let ((overlay (plist-get state :reasoning-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay)
      (setf (plist-get state :reasoning-overlays)
            (delq overlay (plist-get state :reasoning-overlays)))))
  (superchat--response-region--detach-state-marker state :reasoning-body-start)
  (superchat--response-region--detach-state-marker state :reasoning-body-end)
  (plist-put state :reasoning-overlay nil)
  (plist-put state :reasoning-parts nil)
  (plist-put state :reasoning-text nil)
  (plist-put state :reasoning-latest nil)
  (plist-put state :reasoning-folded t))

(defun superchat--response-region-prepare (&optional transcript-owner)
  "Create one assistant region for the next assistant/tool round.

TRANSCRIPT-OWNER explicitly addresses a state without selecting it.  Omitted
or nil retains the legacy dynamic owner, falling back to pending.
Existing owner states are never detached, which
lets a workflow and an ordinary request retain independent transcript
blocks in the same View.  Placement prefers an existing persistent marker, then
the owner's retained end, with a later prompt only as a fallback."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((owner (or transcript-owner (if superchat--runtime-transcript-owner
                                             superchat--runtime-transcript-owner
                                           :pending)))
             (state (superchat--response-region--state-for-owner owner transcript-owner))
             (inhibit-read-only t))
        (superchat--response-region--reserve-before-prompt state)
        (superchat--response-region-clear-status (plist-get state :owner))
        (let ((existing-boundary
               (superchat--response-region--state-marker-position
                state :assistant-end)))
          (superchat--response-region--detach-state-marker
           state :assistant-start)
          (superchat--response-region--detach-state-marker
           state :assistant-end)
          (superchat--response-region--discard-current-reasoning state)
          (plist-put state :parts nil)
          (let* ((persistent-marker (plist-get state :persistent-marker))
                 (prompt-position
                  (superchat--response-region--prompt-line-position state))
                 (position (or persistent-marker existing-boundary
                               prompt-position (point-max)))
                 (insert-text (if existing-boundary
                                  #'insert-before-markers #'insert)))
            (goto-char position)
            (when persistent-marker
              (set-marker-insertion-type persistent-marker t))
            (unless (bolp) (funcall insert-text "\n"))
            (let ((start (point)))
              (funcall insert-text "\n** Assistant\n")
              (plist-put state :assistant-start (copy-marker start nil)))
            (cond
             (persistent-marker
              (plist-put state :assistant-end (copy-marker (point) t)))
             (prompt-position
              (plist-put state :assistant-end
                         (copy-marker (point) nil)))
             (existing-boundary
              (plist-put state :assistant-end (copy-marker (point) nil))))
            (superchat--response-region--publish-state state transcript-owner)))))))

(defun superchat--response-region-insert-transcript (text owner &optional transcript-owner)
  "Insert transcript TEXT owned by explicit OWNER.

OWNER is `:current-run' for Run-owned records and `:independent-turn' for
material outside that Run.  TRANSCRIPT-OWNER supplies the opaque concrete owner
without selecting its compatibility projection.  Nil retains legacy addressing;
callers do not maintain a parallel marker system."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((owner-key (or transcript-owner (superchat--response-region--effective-owner owner)))
             (state (superchat--response-region--state-for-owner owner-key transcript-owner))
             (inhibit-read-only t))
        (when (eq owner :current-run)
          (superchat--response-region--reserve-before-prompt state))
        (superchat--response-region--publish-state state transcript-owner)
        (pcase owner
          (:current-run
           (let ((insert-text
                  (if (superchat--response-region--state-active-p state)
                      #'insert
                    ;; A following owner's start may share the closed end's
                    ;; position.  It must move past this historical append.
                    #'insert-before-markers))
                 (persistent-marker (plist-get state :persistent-marker))
                 (end-marker (plist-get state :assistant-end)))
             (unwind-protect
                 (progn
                   (when persistent-marker
                     (set-marker-insertion-type persistent-marker t))
                   (when end-marker
                     (set-marker-insertion-type end-marker t))
                   (goto-char
                    (superchat--response-region--current-run-transcript-position state))
                   (unless (bolp) (funcall insert-text "\n"))
                   (funcall insert-text (or text ""))
                   (unless (bolp) (funcall insert-text "\n")))
               (when persistent-marker
                 (set-marker-insertion-type persistent-marker nil))
               (when end-marker
                 (set-marker-insertion-type end-marker nil)))
             ;; A Run may publish headers before its first model chunk.  Keep a
             ;; concrete boundary for that owner so the eventual response can
             ;; open before an independently editable prompt.
             (unless (plist-get state :assistant-end)
               (plist-put state :assistant-end (copy-marker (point) nil)))))
          (:independent-turn
           (let ((turn-marker
                  (and (markerp (plist-get state :status-marker))
                       (marker-position (plist-get state :status-marker))
                       (copy-marker (plist-get state :status-marker) nil)))
                 insert-marker)
             (superchat--response-region-clear-status (plist-get state :owner))
             (setq insert-marker
                   (cond
                    ((and turn-marker
                          (superchat--response-region--position-outside-current-run-p
                           (marker-position turn-marker) state))
                     turn-marker)
                    ((let ((position
                            (superchat--response-region--prompt-line-position
                             state)))
                       (and position
                            (superchat--response-region--position-outside-current-run-p
                             position state)
                            (copy-marker position nil))))
                    ((superchat--response-region--state-active-p state)
                     (or (and (superchat--response-region--state-marker-position
                               state :assistant-end)
                              (plist-get state :assistant-end))
                         (progn
                           (plist-put state :assistant-end
                                      (copy-marker (point-max) nil))
                           (plist-get state :assistant-end))))
                    (t (copy-marker (point-max) nil))))
             (set-marker-insertion-type insert-marker nil)
             (goto-char insert-marker)
             (unless (bolp) (insert "\n"))
             (insert (or text ""))
             (unless (bolp) (insert "\n"))
             (unless (eq insert-marker (plist-get state :assistant-end))
               (set-marker insert-marker nil))
             (when (and (markerp turn-marker)
                        (not (eq turn-marker insert-marker)))
               (set-marker turn-marker nil))))
          (_
           (error "Unknown response transcript owner: %S" owner)))
        (superchat--response-region--publish-state state transcript-owner)))))

(defconst superchat--reasoning-truncation-notice
  "[Thinking truncated; showing newest text.]\n"
  "Notice prepended when a Thinking block exceeds its character limit.")

(defun superchat--response-region--bound-reasoning (text)
  "Return TEXT bounded by `superchat-reasoning-max-chars'.
The newest tail is retained so a long cumulative provider partial cannot
make the View grow without bound while the notice makes the loss visible."
  (let ((limit superchat-reasoning-max-chars))
    (if (or (not (integerp limit))
            (<= limit 0)
            (<= (length text) limit))
        text
      (let* ((notice superchat--reasoning-truncation-notice)
             (notice-length (min limit (length notice)))
             (tail-length (max 0 (- limit notice-length))))
        (concat
         (substring notice 0 notice-length)
         (if (> tail-length 0)
             (substring text (max 0 (- (length text) tail-length)))
           ""))))))

(defun superchat--response-region--replace-reasoning-body (state text)
  "Replace STATE's visible Thinking body with bounded TEXT."
  (let* ((start-marker (plist-get state :reasoning-body-start))
         (end-marker (plist-get state :reasoning-body-end))
         (overlay (plist-get state :reasoning-overlay))
         (start (and (markerp start-marker)
                     (marker-position start-marker)))
         (end (and (markerp end-marker)
                   (marker-position end-marker))))
    (when (and start end overlay)
      (goto-char start)
      (delete-region start end)
      (insert text)
      (put-text-property start (point) 'face 'superchat-reasoning-face)
      (set-marker start-marker start)
      (set-marker end-marker (point))
      (move-overlay overlay start (point)))))

(defun superchat--response-region--insert-reasoning-block (state text)
  "Insert a folded reasoning block for STATE containing TEXT."
  (let ((heading-start (point)))
    (insert "** Thinking\n")
    (let ((body-start (point)))
      (insert text)
      (unless (bolp) (insert "\n"))
      (let* ((body-end (point))
             (overlay (make-overlay body-start body-end nil nil nil)))
        (add-to-invisibility-spec '(superchat-reasoning))
        (overlay-put overlay 'invisible 'superchat-reasoning)
        (overlay-put overlay 'superchat-reasoning-owner
                     (plist-get state :owner))
        (overlay-put overlay 'superchat-reasoning-folded t)
        (put-text-property heading-start (point) 'face
                           'superchat-reasoning-face)
        (put-text-property heading-start (point) 'superchat-reasoning-heading t)
        (put-text-property heading-start (point) 'keymap
                           superchat-reasoning-map)
        (put-text-property heading-start (point) 'superchat-reasoning-overlay
                           overlay)
        (plist-put state :reasoning-body-start (copy-marker body-start nil))
        (plist-put state :reasoning-body-end (copy-marker body-end t))
        (plist-put state :reasoning-overlay overlay)
        (push overlay (plist-get state :reasoning-overlays))
        (plist-put state :reasoning-folded t)))))

(defun superchat--response-region-add-reasoning (text &optional transcript-owner)
  "Append reasoning TEXT to the current owner's folded thinking block."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((state (superchat--response-region--state-for-owner transcript-owner transcript-owner))
             (text (if (stringp text) text (format "%s" text)))
             (inhibit-read-only t))
        (unless (superchat--response-region--state-active-p state)
          (superchat--response-region-prepare transcript-owner)
          (setq state (superchat--response-region--state-for-owner transcript-owner transcript-owner)))
        (let* ((current (or (plist-get state :reasoning-text)
                            (string-join
                             (reverse (plist-get state :reasoning-parts)) "")))
               (bounded (superchat--response-region--bound-reasoning
                         (concat current text))))
          (plist-put state :reasoning-text bounded)
          ;; Keep one canonical bounded value.  In particular, do not retain
          ;; all cumulative partials as separate strings.
          (plist-put state :reasoning-parts (list bounded))
          (let ((lines (cl-remove-if #'string-empty-p
                                     (split-string text "\n"))))
            (when lines
              (plist-put state :reasoning-latest
                         (truncate-string-to-width
                          (car (last lines)) 160 nil nil "…"))))
          (if (plist-get state :reasoning-overlay)
              (superchat--response-region--replace-reasoning-body state bounded)
            (goto-char (plist-get state :assistant-start))
            (superchat--response-region--insert-reasoning-block state bounded)))
        (superchat--response-region--publish-state state transcript-owner)
        (if (fboundp 'superchat--refresh-thinking-status)
            (superchat--refresh-thinking-status (plist-get state :owner))
          (superchat--update-status
           (format "Thinking… · %s" (or (plist-get state :reasoning-latest) ""))
           transcript-owner))))))

(defun superchat--response-region-reasoning-latest (&optional owner)
  "Return the latest reasoning line for OWNER's current round."
  (plist-get
   (superchat--response-region--state-for-owner
    (superchat--response-region--effective-owner owner) owner)
   :reasoning-latest))

(defun superchat-toggle-reasoning (&optional event)
  "Toggle the folded reasoning block at point.
EVENT is accepted for mouse bindings and is ignored for keyboard use."
  (interactive (list last-input-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((overlay (get-text-property (point) 'superchat-reasoning-overlay)))
    (unless (overlayp overlay)
      (user-error "No reasoning block at point"))
    (let ((folded (not (overlay-get overlay 'superchat-reasoning-folded))))
      (overlay-put overlay 'superchat-reasoning-folded folded)
      (overlay-put overlay 'invisible (and folded 'superchat-reasoning))
      folded)))

(defun superchat--response-region--round-cleanup
    (state &optional keep-persistent)
  "Close STATE's active round, retaining its folded blocks."
  (dolist (key '(:status-marker :assistant-start :assistant-end))
    (superchat--response-region--detach-state-marker state key))
  (unless keep-persistent
    (superchat--response-region--detach-state-marker state :persistent-marker))
  (plist-put state :parts nil)
  (plist-put state :reasoning-parts nil)
  (plist-put state :reasoning-text nil)
  (plist-put state :reasoning-latest nil)
  (plist-put state :reasoning-overlay nil))

(defun superchat--response-region-close (content &optional final-p transcript-owner)
  "Rewrite the current owner's assistant region with CONTENT and close it.
Compute meaningful CONTENT before mutating its View or response state."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((meaningful (and (stringp content)
                              (not (string-empty-p (string-trim content)))))
             (owner (superchat--response-region--effective-owner transcript-owner))
             ;; Read the state that lookup would return, without creating or
             ;; adopting it.  Legacy lookup may adopt consumed pending state.
             (existing-state
              (or (cdr (assoc owner superchat--response-region-states))
                  (and (not transcript-owner)
                       (let ((pending (cdr (assoc :pending superchat--response-region-states))))
                         (and (null (plist-get pending :prompt-marker)) pending)))))
             (final-reasoning
              (when meaningful
                (superchat--response-region--bound-reasoning
                 (or (plist-get existing-state :reasoning-text)
                     (string-join (reverse (plist-get existing-state :reasoning-parts)) "")))))
             (formatted-final
              (when meaningful (superchat--format-assistant-answer content)))
             (state (superchat--response-region--state-for-owner transcript-owner transcript-owner))
             (inhibit-read-only t))
        ;; A tool round may have closed the last region without another partial
        ;; ever arriving.  Meaningful final content still owns a new segment;
        ;; nil-content round boundaries remain harmless when already closed.
        (when (and meaningful
                   (not (superchat--response-region--state-active-p state)))
          (superchat--response-region-prepare (plist-get state :owner)))
        (superchat--response-region--publish-state state transcript-owner)
        (superchat--response-region-clear-status (plist-get state :owner))
        (let* ((start-position
                (superchat--response-region--state-marker-position
                 state :assistant-start))
               (end-position
                (or (superchat--response-region--state-marker-position
                     state :assistant-end)
                    (and (superchat--response-region--state-active-p state)
                         (superchat--response-region--state-marker-position
                          state :persistent-marker))
                    (superchat--response-region--prompt-line-position state)
                    (point-max)))
               (response-content
                (if (stringp content)
                    content
                  (string-join (reverse (plist-get state :parts)) "")))
               (reasoning-content
                (if meaningful final-reasoning
                  (superchat--response-region--bound-reasoning
                   (or (plist-get state :reasoning-text)
                       (string-join (reverse (plist-get state :reasoning-parts)) "")))))
               (persistent-marker
                (plist-get state :persistent-marker))
               ;; A following owner can begin at this segment's exact end.
               ;; Its markers belong after the replacement, even if their
               ;; ordinary insertion type is nil.
               (following-markers
                (cl-loop for entry in superchat--response-region-states
                         for other = (cdr entry)
                         unless (eq other state)
                         append (cl-loop for value in other
                                         when (and (markerp value)
                                                   (eq (marker-buffer value) (current-buffer))
                                                   (equal (marker-position value) end-position))
                                         collect value))))
          (when (and start-position end-position)
            (setq end-position (max start-position end-position))
            (let ((following-types
                   (mapcar (lambda (marker)
                             (cons marker (marker-insertion-type marker)))
                           following-markers)))
              (unwind-protect
                  (progn
                    (dolist (marker following-markers)
                      (set-marker-insertion-type marker t))
                    (when persistent-marker
                      (set-marker persistent-marker start-position)
                      (set-marker-insertion-type persistent-marker t))
                    (superchat--response-region--discard-current-reasoning state)
                    (superchat--response-region--detach-state-marker state :assistant-start)
                    (superchat--response-region--detach-state-marker state :assistant-end)
                    (goto-char start-position)
                    (delete-region start-position end-position)
                    (plist-put state :assistant-start (point-marker))
                    (superchat--response-region--publish-state state transcript-owner)
                    (if (string-empty-p (string-trim reasoning-content))
                        (insert "\n** Assistant\n")
                      (insert "\n")
                      (superchat--response-region--insert-reasoning-block
                       state reasoning-content)
                      (insert "** Assistant\n"))
                    (when final-p
                      (cond
                       ((numberp superchat--ttft-elapsed)
                        (superchat--annotate-ttft superchat--ttft-elapsed
                                                  (plist-get state :assistant-start)))
                       ((numberp superchat--nonpartial-e2e-elapsed)
                        (superchat--annotate-nonpartial-latency
                         superchat--nonpartial-e2e-elapsed
                         (plist-get state :assistant-start)))))
                    (unless (string-empty-p (string-trim response-content))
                      (insert (if meaningful formatted-final
                                (superchat--format-assistant-answer response-content))))
                    (unless (bolp) (insert "\n"))
                    (put-text-property (plist-get state :assistant-start) (point)
                                       'superchat-role 'assistant)
                    (when persistent-marker
                      (set-marker persistent-marker (point)))
                    (dolist (marker following-markers)
                      (set-marker marker (point))))
                (when persistent-marker
                  (set-marker-insertion-type persistent-marker nil))
                (dolist (entry following-types)
                  (set-marker-insertion-type (car entry) (cdr entry))))))
          ;; An already closed owner can receive late tool records.  Its retained
          ;; end boundary is the append location, not another round to detach.
          (when start-position
            (let ((closed-end (point)))
              (superchat--response-region--round-cleanup state (not final-p))
              (plist-put state :assistant-end (copy-marker closed-end nil))
              (superchat--response-region--publish-state state transcript-owner))))))))

;; Compatibility entry point retained for callers from older integrations;
;; all product call sites use `superchat--response-region-prepare'.
(defun superchat--prepare-assistant-response-area ()
  "Open an assistant region through the response-region API."
  (superchat--response-region-prepare))

;; Compatibility entry point retained for older tests and integrations;
;; final and tool-round callers share `superchat--response-region-close'.
(defun superchat--render-close-tool-round ()
  "Close the current tool round through the response-region API."
  (superchat--response-region-close nil nil))

(defun superchat--annotate-ttft (elapsed &optional start-marker)
  "Append a TTFT annotation to the current Assistant header.
ELAPSED is seconds since the request was dispatched.  The annotation
is inserted on the `** Assistant' header line so it stays visible in
the chat buffer (echo-area `message' is unreliable during streaming
because redisplay clobbers it)."
  (setq superchat--ttft-elapsed elapsed
        superchat--nonpartial-e2e-elapsed nil)
  (let ((start-marker (or start-marker superchat--assistant-response-start-marker)))
    (when (and start-marker (marker-position start-marker))
      (with-current-buffer (get-buffer-create superchat-buffer-name)
        (let ((inhibit-read-only t)
              (annotation (propertize (format "  ⚡ TTFT: %.2fs" elapsed)
                                      'face 'shadow)))
          (save-excursion
            (goto-char start-marker)
            ;; Marker sits just before the inserted "\n** Assistant\n";
            ;; move to end of the "** Assistant" header line.
            (when (re-search-forward "^\\*\\* Assistant" nil t)
              (end-of-line)
              (insert annotation))))))))

(defun superchat--annotate-nonpartial-latency (elapsed &optional start-marker)
  "Append callback-only end-to-end ELAPSED time to the Assistant header.
The stored value survives until a non-streaming response opens its Assistant
region; unlike TTFT, it is explicitly labeled unavailable for this provider."
  (setq superchat--nonpartial-e2e-elapsed elapsed
        superchat--ttft-elapsed nil)
  (let ((start-marker (or start-marker superchat--assistant-response-start-marker)))
    (when (and start-marker (marker-position start-marker))
      (with-current-buffer (get-buffer-create superchat-buffer-name)
        (let ((inhibit-read-only t)
              (annotation
               (propertize
                (format
                 "  ⏱ E2E: %.2fs · TTFT unavailable (non-partial provider)"
                 elapsed)
                'face 'shadow)))
          (save-excursion
            (goto-char start-marker)
            (when (re-search-forward "^\\*\\* Assistant" nil t)
              (end-of-line)
              (insert annotation))))))))

(defun superchat--stream-llm-result (chunk &optional transcript-owner)
  "Process a CHUNK from the LLM streaming response, update UI and accumulate.
CHUNK is rendered with the `superchat-streaming-pending' face so raw
markdown mid-stream doesn't get re-interpreted by font-lock (which
was causing visible \"residue\" / flicker before the final rewrite)."
  (if (and (consp chunk)
           (memq (car chunk) '(reasoning :reasoning)))
      (superchat--response-region-add-reasoning
       (if (eq (car chunk) :reasoning)
           (plist-get chunk :reasoning)
         (cadr chunk)) transcript-owner)
    (superchat--response-region-stream chunk transcript-owner)))

(defun superchat--response-region-stream (chunk &optional transcript-owner)
  "Append CHUNK to the current response region and advance its tail."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((state (superchat--response-region--state-for-owner transcript-owner transcript-owner))
             (inhibit-read-only t))
        (unless (superchat--response-region--state-active-p state)
          (superchat--response-region-prepare transcript-owner)
          (setq state (superchat--response-region--state-for-owner transcript-owner transcript-owner)))
        (push (or chunk "") (plist-get state :parts))
        (let ((end-marker (plist-get state :assistant-end)))
          (when end-marker
            ;; Persistent material is immediately after this marker.  Insert at
            ;; the marker and let its insertion type carry the boundary forward
            ;; so every later chunk remains in the owned region.
            (set-marker-insertion-type end-marker t))
          (goto-char (or end-marker
                         (superchat--response-region--prompt-line-position state)
                         (point-max)))
          (let ((start (point)))
            (insert (or chunk ""))
            (put-text-property start (point)
                               'face 'superchat-streaming-pending)))
        (superchat--response-region--publish-state state transcript-owner)))))

(defun superchat--process-llm-result (answer &optional tape-id transcript-owner)
  "Finalize the LLM ANSWER processing.
This function is called ONCE after the entire response has been streamed.
When TAPE-ID is non-nil, reuse that Runtime event for history."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content (if (and answer (not (string-empty-p answer)))
                                answer
                              "Assistant did not provide a response.")))
      (superchat--response-region-close response-content t transcript-owner)

      ;; 1. Update conversation history with the full response
      (superchat--record-message "assistant" response-content tape-id)
      
      ;; 2. Insert the next prompt (it handles its own spacing)
      (superchat--response-region-ensure-view-prompt))))

(defun superchat--insert-system-message (content)
  "Insert a system message CONTENT into the chat buffer and refresh the prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (superchat--response-region-insert-transcript
     (propertize (format "* System: %s" content) 'face 'italic)
     :independent-turn)
    (superchat--refresh-prompt)))

(defun superchat--response-region-insert-persistent
    (content &optional owned-response-marker transcript-owner)
  "Insert persistent system CONTENT before the current prompt.
When an assistant region is active, keep a boundary before the notice block so
later chunks and tool records remain in that Run's transcript.  Only
OWNED-RESPONSE-MARKER may consume a transient status line, and only when it is
still the buffer's current response marker."
  (let* ((target-name (and (stringp superchat-buffer-name)
                           superchat-buffer-name))
         (buffer (if target-name
                     (get-buffer-create target-name)
                   (current-buffer))))
    (with-current-buffer buffer
      (let* ((state (superchat--response-region--state-for-owner transcript-owner transcript-owner))
             (inhibit-read-only t))
        (superchat--response-region--publish-state state transcript-owner)
        (when (and (markerp owned-response-marker)
                   (eq owned-response-marker (plist-get state :status-marker))
                   (marker-position owned-response-marker))
          (superchat--response-region-clear-status (plist-get state :owner)))
        (let* ((assistant-active
                (superchat--response-region--state-active-p state))
               (response-active
                (or assistant-active
                    (superchat--response-region--state-marker-position
                     state :status-marker)))
             (persistent-marker
              (plist-get state :persistent-marker))
               (prompt-line-position
                (superchat--response-region--prompt-line-position state))
               (insert-marker
                (and response-active
                     (null persistent-marker)
                     (copy-marker
                      (if assistant-active
                          (superchat--response-region--assistant-tail-position
                           state)
                        (or prompt-line-position (point-max)))
                      nil)))
               (system-text
                (propertize (format "* System: %s" content)
                            'face 'italic)))
          ;; The first notice during a Run establishes the boundary.  Later
          ;; notices are appended immediately before the prompt, after the
          ;; already-recorded notice block, while the boundary remains before
          ;; all of them.
          (when insert-marker
            (plist-put state :persistent-marker insert-marker)
            (setq persistent-marker insert-marker)
            (when assistant-active
              ;; The assistant region and notice boundary start at the same
              ;; position.  Keep a separate moving end marker for stream
              ;; writes; the notice boundary itself stays before the notice
              ;; text until the round is closed.
              (plist-put state :assistant-end
                         (copy-marker (marker-position insert-marker) nil))))
          (when persistent-marker
            (set-marker-insertion-type persistent-marker nil))
          (goto-char (if insert-marker
                         insert-marker
                         (or prompt-line-position (point-max))))
          (unless (bolp) (insert "\n"))
          (insert system-text "\n")
          (unless (superchat--response-region--state-marker-position
                   state :prompt-marker)
            (superchat--response-region-ensure-view-prompt))
          (superchat--response-region--publish-state state transcript-owner))))))

(defun superchat--insert-persistent-system-message
    (content &optional owned-response-marker)
  "Insert persistent system CONTENT through the response-region API."
  (superchat--response-region-insert-persistent
   content owned-response-marker))

(defun superchat--refresh-prompt ()
  "Clear any status message and reuse the pending View prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (superchat--response-region-clear-status)
      (superchat--response-region-ensure-view-prompt))))


(provide 'superchat-render)
;;; superchat-render.el ends here
