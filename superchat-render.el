;;; superchat-render.el --- Buffer rendering for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Chat buffer rendering: MD→Org conversion, prompt insertion,
;; streaming display, TTFT annotation, and response finalization.
;; Extracted from superchat.el monolith (v0.9 split step 4).

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-preset)
(require 'superchat-sink)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-buffer-name)
(defvar superchat-mode)
(defvar superchat--session-id)
;; Owned by superchat-runtime.el; render sets it directly rather than
;; requiring the Kernel file, so Runtime stays loadable without Render.
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
(declare-function superchat-runtime--sink-stop-requested-p
                  "superchat-runtime" (run))

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


(defun superchat--protected-chat-header (text)
  "Return structural header TEXT protected from normal editing.
Call only at product-owned header insertion sites, never on answer text.
The following draft or response body must not inherit these properties."
  (propertize text 'read-only t 'front-sticky '(read-only)
              'rear-nonsticky t))

(defun superchat--insert-prompt (&optional transcript-owner)
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (let* ((owner (or transcript-owner :pending))
             (state (superchat--response-region--state-for-owner owner transcript-owner)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
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
          (insert (superchat--protected-chat-header "\n")
                  (propertize (superchat--protected-chat-header headline)
                              'face 'superchat-prompt-face
                              'superchat-prompt-header t))
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
        :heading-marker nil
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
        :reasoning-folded t
        :pending-tool-calls nil))

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

(defun superchat--response-region-begin (owner &optional initial-assistant)
  "Initialize OWNER once, adopting only consumed/prepared pending state.
INITIAL-ASSISTANT opens its first response before any model output arrives.
Omitted preserves header-only producers such as Workflow.  An existing owner's
repeated handoff never prepares another segment or reselects it."
  (when (and owner (not (assoc owner superchat--response-region-states)))
    (let ((state (superchat--response-region--state-for-owner owner)))
      (superchat--response-region--reserve-before-prompt state)
      (when initial-assistant
        (superchat--response-region-prepare owner)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (superchat--response-region--assistant-tail-position state))
            ;; The waiting line belongs below this first heading, separately
            ;; from the body that will arrive later.
            (insert-before-markers "\n")
            (plist-put state :status-marker (copy-marker (1- (point)) nil)))))
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
        ;; S21-fix-1 (Spec 2): before this owner's markers are detached,
        ;; mark any of its still-pending tool calls stopped.
        (superchat--response-region--fail-pending-tool-calls state)
        (dolist (key '(:status-marker :assistant-start :assistant-end
                       :heading-marker :persistent-marker :prompt-marker
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
the owner's retained end, with a later prompt only as a fallback.

When STATE's :mid-turn-continuation is set (S21: `superchat--response-
region-close' sets it on a tool-round close, meaning this same logical
Run is still going, just past a tool round, not a new Run's first open),
this reopens the boundary at the SAME position a heading would have gone
-- so a later close still only ever rewrites its own round, never an
earlier round's tool records -- but skips writing the `** Assistant:'
line itself, since one is already visible above the tool records.  The
flag is one-shot: cleared here whether or not it was set, so a genuinely
new Run's first `prepare' (mid-turn-continuation always nil) still writes
its own heading."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (let* ((owner (or transcript-owner (if superchat--runtime-transcript-owner
                                             superchat--runtime-transcript-owner
                                           :pending)))
             (state (superchat--response-region--state-for-owner owner transcript-owner))
             (inhibit-read-only t)
             (skip-heading (plist-get state :mid-turn-continuation)))
        (plist-put state :mid-turn-continuation nil)
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
            (if skip-heading
                ;; No text to insert -- reopen the (empty) boundary exactly
                ;; where it already was, touching the buffer not at all, so
                ;; another owner's own region starting at this same
                ;; position (a concurrent workflow/subagent stream, say)
                ;; never has its own markers disturbed by an insertion
                ;; that has nothing to do with it.  :assistant-end's type
                ;; is `t' (advance on insertion at its own position) even
                ;; though it starts out equal to :assistant-start (type
                ;; nil, stays put): `superchat--response-region-add-
                ;; reasoning' inserts a folded reasoning block directly at
                ;; :assistant-start with no marker-type management of its
                ;; own (unlike `superchat--response-region-stream'/
                ;; `-insert-transcript', which toggle :assistant-end to
                ;; `t' for their own insertion and restore it after) --
                ;; without this, that first reasoning block would leave
                ;; :assistant-end stale (still at the shared start
                ;; position), so a later close's END-POSITION would miss
                ;; it and re-insert a duplicate.
                (progn
                  (plist-put state :assistant-start (copy-marker position nil))
                  (plist-put state :assistant-end (copy-marker position t)))
              (unless (bolp) (funcall insert-text "\n"))
              (superchat--response-region--detach-state-marker state :heading-marker)
              (let ((start (point)))
                (funcall insert-text (superchat--protected-chat-header "\n** Assistant:\n"))
                (plist-put state :assistant-start (copy-marker start nil))
                ;; S21: this marker alone survives a later mid-turn-
                ;; continuation reopen (:assistant-start does not -- it gets
                ;; relocated past each round's tool records), so a much
                ;; later final close can tell whether the heading it would
                ;; otherwise rewrite is this same, still-visible one.
                (plist-put state :heading-marker (copy-marker start nil)))
              (cond
               (persistent-marker
                (plist-put state :assistant-end (copy-marker (point) t)))
               (prompt-position
                (plist-put state :assistant-end
                           (copy-marker (point) nil)))
               (existing-boundary
                (plist-put state :assistant-end (copy-marker (point) nil)))))
            (superchat--response-region--publish-state state transcript-owner)))))))

(defun superchat--response-region-insert-transcript
    (text owner &optional transcript-owner insert-controls)
  "Insert transcript TEXT owned by explicit OWNER.

OWNER is `:current-run' for Run-owned records and `:independent-turn' for
material outside that Run.  TRANSCRIPT-OWNER supplies the opaque concrete owner
without selecting its compatibility projection.  Nil retains legacy addressing;
callers do not maintain a parallel marker system.
INSERT-CONTROLS, when non-nil, inserts native controls at point after TEXT.
It requires an existing closed explicit owner and may only insert presentation,
not invoke actions.  Failed insertion rolls back text, new overlays and touched
markers.  Omitted preserves the existing text-only behavior and return value."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (save-excursion
      (when insert-controls
        (let ((existing (cdr (assoc transcript-owner superchat--response-region-states))))
          (unless (and (eq owner :current-run) transcript-owner existing
                       (not (superchat--response-region--state-active-p existing))
                       (superchat--response-region--state-marker-position existing :assistant-end))
            (error "Controls require an existing closed transcript owner"))))
      (let* ((owner-key (or transcript-owner (superchat--response-region--effective-owner owner)))
             (state (superchat--response-region--state-for-owner owner-key transcript-owner))
             (inhibit-read-only t))
        (when (eq owner :current-run)
          (superchat--response-region--reserve-before-prompt state))
        (superchat--response-region--publish-state state transcript-owner)
        (pcase owner
          (:current-run
           (let* ((insert-text
                  (if (superchat--response-region--state-active-p state)
                      #'insert
                    ;; A following owner's start may share the closed end's
                    ;; position.  It must move past this historical append.
                    #'insert-before-markers))
                 (persistent-marker (plist-get state :persistent-marker))
                 (end-marker (plist-get state :assistant-end))
                 (position (superchat--response-region--current-run-transcript-position state))
                 (following
                  (when insert-controls
                    (cl-loop for entry in superchat--response-region-states
                             unless (eq (cdr entry) state)
                             append (cl-loop for value in (cdr entry)
                                             when (and (markerp value)
                                                       (eq (marker-buffer value) (current-buffer))
                                                       (equal (marker-position value) position))
                                             collect value))))
                 (saved-markers
                  (when insert-controls
                    (mapcar (lambda (marker)
                              (list marker (marker-position marker) (marker-insertion-type marker)))
                            (delete-dups (delq nil (append following (list persistent-marker end-marker)))))))
                 (old-overlays (when insert-controls (append (car (overlay-lists)) (cdr (overlay-lists)))))
                 (insert-end (when insert-controls (copy-marker position t)))
                 inserted)
             (unwind-protect
                 (cl-flet ((insert-record ()
                   (dolist (marker following) (set-marker-insertion-type marker t))
                   (when persistent-marker
                     (set-marker-insertion-type persistent-marker t))
                   (when end-marker
                     (set-marker-insertion-type end-marker t))
                   (goto-char
                    (superchat--response-region--current-run-transcript-position state))
                   (unless (bolp) (funcall insert-text "\n"))
                   (funcall insert-text (or text ""))
                   (when insert-controls (funcall insert-controls))
                   (unless (bolp) (funcall insert-text "\n"))
                   (setq inserted t)))
                   (insert-record))
               (when insert-controls
                 (unless inserted
                   ;; Actions are deferred by the presenter: these exact new
                   ;; overlays belong only to this failed insertion callback.
                   (let ((inhibit-quit t) (inhibit-modification-hooks t))
                     (dolist (overlay (overlays-in position insert-end))
                       (when (and (not (memq overlay old-overlays))
                                  (>= (overlay-start overlay) position)
                                  (<= (overlay-end overlay) insert-end))
                         (delete-overlay overlay)))
                     ;; widget.el insertion does not reliably participate in
                     ;; change-group undo.  This callback is insertion-only:
                     ;; remove its exact fresh span, never surrounding text.
                     (delete-region position insert-end)))
                 (dolist (entry saved-markers)
                   (unless inserted (set-marker (car entry) (cadr entry)))
                   (set-marker-insertion-type (car entry) (nth 2 entry)))
                 (set-marker insert-end nil))
               (when (and persistent-marker (not insert-controls))
                 (set-marker-insertion-type persistent-marker nil))
               (when (and end-marker (not insert-controls))
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
Compute meaningful CONTENT before mutating its View or response state.

A tool-round close (FINAL-P nil) rewrites its own round's text exactly as
before, but never (re-)inserts the `** Assistant:' heading line itself --
only the final close (FINAL-P t) does.  It marks STATE's
:mid-turn-continuation so the next `superchat--response-region-prepare'
\(reached through `superchat--response-region-stream' once this round's
`superchat--response-region--round-cleanup' below marks the region
inactive) also skips writing that heading while still repositioning the
region's boundary past this round's own tool records -- see the flag's
use there.  Before this (S21), the heading text was identical on every
close (a no-op replace), so the *visible* second/third/... `** Assistant:'
heading a multi-round tool journey showed always came from that next
`prepare', not from this function's own rewrite; now neither writes one
until the true final close."
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
             (inhibit-read-only t)
             ;; S21-fix-1 (Spec 2): a true final close ends this owner's Run;
             ;; any tool call whose result never reached the renderer is
             ;; marked stopped here, independent of the rewrite below (a
             ;; still-live marker updates in place even when this round's own
             ;; assistant-start/-end are inactive, e.g. cancellation landed
             ;; right after a tool call with no narration since).
             (_swept-pending
              (when final-p
                (superchat--response-region--fail-pending-tool-calls state))))
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
               ;; A tool-round close on the FIRST round of a segment finds
               ;; a real, already-inserted `** Assistant:' heading between
               ;; START-POSITION and END-POSITION (from `superchat--
               ;; response-region-prepare''s initial, non-skipped
               ;; insertion) and must spare it -- the heading has to stay
               ;; visible with the tool records nesting under it WHILE
               ;; they happen, not only reappear once the final close
               ;; eventually reinserts one.  A LATER round in the same
               ;; segment (after `superchat--response-region-prepare''s
               ;; own :mid-turn-continuation-driven reopen, which writes
               ;; no heading at all) finds none here, so DELETE-START
               ;; correctly falls back to START-POSITION itself.
               (heading-bounds
                (and (not final-p) start-position end-position
                     (save-excursion
                       (goto-char start-position)
                       (and (re-search-forward "\\*\\* Assistant:\n"
                                                (max start-position end-position) t)
                            (cons (match-beginning 0) (match-end 0))))))
               (body-start (cdr heading-bounds))
               ;; S21: a live reasoning block (or status preview) `superchat--
               ;; response-region-add-reasoning' inserted straight at START-
               ;; POSITION, before this round's heading ever existed, sits in
               ;; [START-POSITION . HEADING-START) -- strictly before
               ;; DELETE-START, so the main delete-region below (which spares
               ;; the heading onward) never reaches it.  Left alone, this
               ;; close's own `--discard-current-reasoning' still deletes
               ;; that block's OVERLAY (a tool round's own reasoning must
               ;; become independently toggleable, per this function's own
               ;; docstring), permanently un-hiding its now-orphaned raw
               ;; text while a fresh copy is inserted after the heading --
               ;; a visible duplicate.  It is swept up separately, below.
               (heading-start (car heading-bounds))
               (delete-start (or body-start start-position))
               ;; S21: a multi-round segment's real `** Assistant:' heading
               ;; sits at :heading-marker, which (unlike :assistant-start)
               ;; survives every mid-turn-continuation reopen unmoved.  When
               ;; it lies strictly before DELETE-START, this close's own
               ;; delete-region below cannot touch it -- it is still visible
               ;; further up, above this round's tool records -- so the
               ;; FINAL-P heading insertion two paragraphs down must not
               ;; write a second one.  A single-round final close has no
               ;; such earlier heading (:heading-marker coincides with
               ;; DELETE-START, since neither BODY-START nor a prior round
               ;; ever moved it away), so it still deletes and rewrites the
               ;; one heading it owns exactly as before.
               (heading-already-visible
                (let ((heading-marker-position
                       (superchat--response-region--state-marker-position
                        state :heading-marker)))
                  (and heading-marker-position delete-start
                       (< heading-marker-position delete-start))))
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
            (setq end-position (max delete-start end-position))
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
                    (goto-char delete-start)
                    (delete-region delete-start end-position)
                    (plist-put state :assistant-start (point-marker))
                    (superchat--response-region--publish-state state transcript-owner)
                    ;; S21: only the true final close writes the `**
                    ;; Assistant:' heading line itself -- see this
                    ;; function's own docstring and `superchat--response-
                    ;; region-prepare''s :mid-turn-continuation handling.
                    ;; A tool round's OWN reasoning still gets its folded
                    ;; block (and thus its own :reasoning-overlays entry)
                    ;; regardless of FINAL-P; only the following heading
                    ;; text is conditional.
                    (if (string-empty-p (string-trim reasoning-content))
                        (when (and final-p (not heading-already-visible))
                          (insert (superchat--protected-chat-header "\n** Assistant:\n")))
                      (insert "\n")
                      (superchat--response-region--insert-reasoning-block
                       state reasoning-content)
                      (when (and final-p (not heading-already-visible))
                        (insert (superchat--protected-chat-header "** Assistant:\n"))))
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
          ;; S21: sweep up the orphaned pre-heading residue (see HEADING-
          ;; START's own docstring above) now that the main rewrite above is
          ;; done.  Both endpoints are plain buffer positions captured
          ;; before that rewrite ran and are strictly less than DELETE-
          ;; START, so nothing the rewrite touched shifts them.
          (when (and heading-start (> heading-start start-position))
            (delete-region start-position heading-start))
          ;; An already closed owner can receive late tool records.  Its retained
          ;; end boundary is the append location, not another round to detach.
          (when start-position
            (let ((closed-end (point)))
              (unless final-p
                (plist-put state :mid-turn-continuation t))
              (superchat--response-region--round-cleanup state (not final-p))
              (plist-put state :assistant-end (copy-marker closed-end nil))
              (superchat--response-region--publish-state state transcript-owner))))))))

(defun superchat--response-region-update-transcript-line (marker replacement)
  "Replace the heading line at MARKER with REPLACEMENT in place.

MARKER must still point inside a live buffer, at the start of a heading
line already inserted by `superchat--response-region-insert-transcript'
\(S21: a tool-call heading's `*** ⏳ NAME' line, updated to `*** ✓/✗ NAME
\(D.Ds)' once its result arrives).  This never inserts or removes a line
-- it only rewrites the one line already there, so a caller must do this
before any later `superchat--response-region-close' rewrite moves past
it; once a later close's rewrite has run, MARKER's line may no longer be
inside the still-live part of the region.  Silently does nothing when
MARKER's buffer is no longer live."
  (when (and (markerp marker) (marker-buffer marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (let ((inhibit-read-only t)
              (position (marker-position marker)))
          (goto-char position)
          (delete-region position (line-end-position))
          (goto-char position)
          (insert replacement))))))

(defun superchat--response-region-current-tail-marker (transcript-owner)
  "Return a fresh, insertion-type-nil marker at TRANSCRIPT-OWNER's current
transcript tail (S21-fix-2): the same position
`superchat--response-region-insert-transcript' would insert at right now.
Calling this once before and once after one insertion brackets exactly what
that insertion added, with no text search."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((state (superchat--response-region--state-for-owner
                  transcript-owner transcript-owner)))
      (copy-marker
       (superchat--response-region--current-run-transcript-position state)
       nil))))

(defun superchat--response-region-track-pending-tool-call
    (transcript-owner execution-id heading-marker end-marker tool-name call-time)
  "Record TRANSCRIPT-OWNER's pending TOOL-NAME call for EXECUTION-ID.

HEADING-MARKER is the start of its `*** ⏳ NAME' heading line; END-MARKER is
the position right after its rendered call block, where the eventual result
is inserted (S21-fix-2: under this call's own heading, not the owner's
current tail, so out-of-order completion never lands a result under the
wrong heading).  CALL-TIME is the `float-time' the call started.  The record
lives inside TRANSCRIPT-OWNER's own response-region state (S21-fix-1: no
global table), so it is naturally isolated from every other owner and is
swept up with that owner's state by `superchat--response-region-reset' or a
final `superchat--response-region-close' -- see
`superchat--response-region--fail-pending-tool-calls'.  Removed on ordinary
completion by `superchat--response-region-untrack-pending-tool-call'."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((state (superchat--response-region--state-for-owner
                  transcript-owner transcript-owner)))
      (plist-put state :pending-tool-calls
                 (cons (list execution-id heading-marker end-marker tool-name call-time)
                       (plist-get state :pending-tool-calls))))))

(defun superchat--response-region-pin-pending-end-markers (transcript-owner position)
  "Return TRANSCRIPT-OWNER's pending end-of-call markers currently at POSITION.

Call this *before* inserting new content at POSITION (S21-fix-2), then
after the insertion `set-marker' each returned marker back to POSITION.
The insertion may go through `superchat--response-region-insert-transcript'
in its inactive-region form, which uses `insert-before-markers' and so
unconditionally pushes every marker sitting at POSITION past the new text,
regardless of that marker's own insertion type; a still-pending call's own
end marker logically belongs exactly at POSITION -- right after its own
already-rendered call, before whatever is inserted next -- so it must be
pinned back rather than swept along with the owner's advancing tail."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((state (superchat--response-region--state-for-owner
                  transcript-owner transcript-owner)))
      (cl-loop for entry in (plist-get state :pending-tool-calls)
               for end = (nth 2 entry)
               when (and (markerp end) (marker-buffer end) (= (marker-position end) position))
               collect end))))

(defun superchat--response-region-untrack-pending-tool-call (transcript-owner execution-id)
  "Remove and return TRANSCRIPT-OWNER's pending record for EXECUTION-ID.
Returns (HEADING-MARKER END-MARKER TOOL-NAME CALL-TIME), or nil when
EXECUTION-ID has no pending record under TRANSCRIPT-OWNER -- a different
owner's own call using the same id, if any, is untouched."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let* ((state (superchat--response-region--state-for-owner
                   transcript-owner transcript-owner))
           (entry (assoc execution-id (plist-get state :pending-tool-calls))))
      (when entry
        (plist-put state :pending-tool-calls
                   (delq entry (plist-get state :pending-tool-calls)))
        (cdr entry)))))

(defun superchat--response-region-insert-at-call-marker (transcript-owner marker text)
  "Insert TEXT at MARKER, a position inside TRANSCRIPT-OWNER's own transcript
region (S21-fix-2: a tool call's own end-of-call marker, so its result lands
directly under its own heading instead of at the owner's current tail).

A later call's own heading/end markers, still pending under the same owner,
strictly after MARKER's position shift forward by ordinary marker
relocation and need no help.  The owner's `:assistant-end'/`:persistent-
marker', and every OTHER still-pending call's own heading/end markers, are
temporarily switched to advance-on-insertion for this one call so a
marker that happens to sit at exactly MARKER's position (the common case:
no narration ever separated this call from whichever came right after it)
is correctly pushed past the newly inserted TEXT instead of being left
pointing into the middle of it.  Silently does nothing when MARKER's buffer
is no longer live."
  (when (and (markerp marker) (marker-buffer marker) (buffer-live-p (marker-buffer marker)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (let* ((inhibit-read-only t)
               (state (superchat--response-region--state-for-owner
                       transcript-owner transcript-owner))
               (guard-markers
                (delq nil
                      (append
                       (list (plist-get state :assistant-end)
                             (plist-get state :persistent-marker))
                       (cl-loop for entry in (plist-get state :pending-tool-calls)
                                append (list (nth 1 entry) (nth 2 entry))))))
               (saved-types (mapcar #'marker-insertion-type guard-markers)))
          (unwind-protect
              (progn
                (dolist (guard guard-markers) (set-marker-insertion-type guard t))
                (goto-char (marker-position marker))
                (insert text))
            (cl-mapc (lambda (guard type) (set-marker-insertion-type guard type))
                     guard-markers saved-types)))))))

(defun superchat--response-region--fail-pending-tool-calls (state)
  "Rewrite each of STATE's still-pending tool-call headings to `stopped'.

Backstops `superchat--agent-render-tool-result''s ordinary in-place update
for a call whose result never arrives (cancelled, timed out, or its render
withheld) -- see `superchat--response-region-reset' and a final
`superchat--response-region-close' (S21-fix-1, Spec 2).  A live marker's
heading line is rewritten to `*** ✗ NAME (stopped)', preserving its
`superchat-tool-call-id' property; a marker whose buffer already died is
simply dropped.  Always clears STATE's pending list."
  (dolist (entry (plist-get state :pending-tool-calls))
    (pcase-let ((`(,id ,heading-marker ,_end-marker ,tool-name ,_call-time) entry))
      (when (and (markerp heading-marker) (marker-buffer heading-marker)
                 (buffer-live-p (marker-buffer heading-marker)))
        (superchat--response-region-update-transcript-line
         heading-marker
         (propertize (format "*** ✗ %s (stopped)" tool-name)
                     'face 'font-lock-warning-face
                     'superchat-tool-call-id id)))))
  (plist-put state :pending-tool-calls nil))

;; Compatibility entry point retained for callers from older integrations;
;; all product call sites use `superchat--response-region-prepare'.
(defun superchat--prepare-assistant-response-area ()
  "Open an assistant region through the response-region API."
  (superchat--response-region-prepare))

;; Compatibility entry point retained for older tests and integrations;
;; final and tool-round callers share `superchat--response-region-close'.
(defun superchat--annotate-ttft (elapsed &optional start-marker)
  "Append a TTFT annotation to the current Assistant header.
ELAPSED is seconds since the request was dispatched.  The annotation
is inserted on the `** Assistant:' header line so it stays visible in
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
            ;; Marker sits just before the inserted "\n** Assistant:\n";
            ;; move to end of the "** Assistant:" header line.
            (when (re-search-forward "^\\*\\* Assistant" nil t)
              (end-of-line)
              (insert (superchat--protected-chat-header annotation)))))))))

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
              (insert (superchat--protected-chat-header annotation)))))))))

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

(defun superchat--tool-budget-notice (budget)
  "Return a display notice for a recorded tool BUDGET, or nil.
This text belongs to the View, never to the model's response body."
  (let ((rounds (plist-get budget :rounds))
        (limit (plist-get budget :limit))
        (action (plist-get budget :action)))
    (when (and (integerp rounds) (integerp limit)
               (> limit 0) (>= rounds limit)
               (member action '(finalize stop "finalize" "stop")))
      (format
       "Tool round limit reached (%d/%d). %s"
       rounds limit
       (if (member action '(stop "stop"))
           "Execution stopped without requesting a final answer."
         "A final answer was requested; unfinished or unverified work may remain.")))))

(declare-function superchat-tape-replay-events "superchat-tape" (session-id &optional since-id limit))

(defun superchat-render-run-record-line (session-id run-id)
  "The product's record line for RUN-ID's tool calls, or nil when it made none.
It says what the Run's tools changed, read from the Tape: every buffer a
structured tool changed (the `:buffer' of its `tool.completed' facts) with its
state now -- unsaved, saved, or closed -- or that none was changed, and how
many times `eval_elisp' ran, whose effects are not tracked.  It is shown to
the person under the answer and never sent to the model
\(docs/decisions/accepted/2026-09-27-claims-checked-against-the-record.md)."
  (let (called buffers (evals 0))
    (dolist (event (and (stringp session-id) (stringp run-id)
                        (ignore-errors (superchat-tape-replay-events session-id))))
      (when (equal (plist-get event :run-id) run-id)
        (let ((payload (plist-get event :payload)))
          (pcase (plist-get event :type)
            ("tool.requested" (setq called t))
            ("tool.completed"
             (when-let* ((name (plist-get payload :buffer)))
               (unless (member name buffers) (push name buffers)))
             (when (equal (plist-get payload :tool-name) "eval_elisp")
               (cl-incf evals)))))))
    (when called
      (concat
       "Record of this task: "
       (if buffers
           (concat "changed "
                   (mapconcat
                    (lambda (name)
                      (let ((buffer (get-buffer name)))
                        (format "%s (%s)" name
                                (cond ((null buffer) "closed")
                                      ((buffer-modified-p buffer) "unsaved")
                                      (t "saved")))))
                    (reverse buffers) ", "))
         "no buffer or file was changed by a tool")
       (when (> evals 0)
         (format "; ran Emacs Lisp %d time%s (what it changed is not tracked)"
                 evals (if (= evals 1) "" "s")))
       "."))))

(defun superchat-render-view-sink (buffer session-id transcript-owner)
  "Return the chat View's Sink for BUFFER/SESSION-ID/TRANSCRIPT-OWNER.

The chat dispatcher passes it as the chat envelope's `:sink', so Runtime
routes an ordinary chat Request's stream/status/final/error here instead of
holding any render probe of its own.  Declares no `:live-p' (per
`docs/design/turn-pipeline-contract.md' on legacy-bridge/View Sinks): each
callback checks BUFFER's liveness, `superchat-mode' and Session ID itself
and does nothing when they no longer match, so a Run whose View was cleared
or replaced still completes durably without ever declaring the Sink dead."
  (cl-labels
      ((current-p ()
         (and (buffer-live-p buffer)
              (with-current-buffer buffer
                (and superchat-mode
                     (or (null superchat--session-id)
                         (equal superchat--session-id session-id))))))
       (budget-notice (payload)
         (when-let* ((notice
                      (superchat--tool-budget-notice
                       (plist-get payload :tool-budget))))
           (superchat--response-region-insert-persistent
            notice nil transcript-owner)))
       (record-line (run)
         ;; What the Run's tools changed, under the answer (Part A of the
         ;; 2026-09-27 decision); product text, not the model's.
         (when-let* ((line (superchat-render-run-record-line
                            session-id
                            (if (stringp run) run (plist-get run :run-id)))))
           (superchat--response-region-insert-persistent
            (propertize line 'face 'shadow) nil transcript-owner))))
    (superchat-sink-create
     :id (format "view-%s" (buffer-name buffer))
     :status
     (let ((waiting-seen (make-hash-table :test #'equal)))
       (lambda (run status)
         (when (current-p)
           (with-current-buffer buffer
             (let ((superchat-buffer-name (buffer-name buffer)))
               (pcase status
                 ('waiting
                  (let ((run-id (plist-get run :run-id)))
                    (unless (gethash run-id waiting-seen)
                      (puthash run-id t waiting-seen)
                      (when (fboundp 'superchat--update-status)
                        (superchat--response-region-begin transcript-owner t)
                        (superchat--update-status
                         "Prefill / waiting first token… 0s" transcript-owner)))))
                 ((or 'ready 'error 'stopped 'orphaned)
                  ;; Establish/select OWNER first: clearing a status line it
                  ;; never had would otherwise leave a fresh, unselected empty
                  ;; state behind (via `--state-for-owner's explicit-lookup
                  ;; auto-create), which then blocks `--response-region-begin'
                  ;; from ever selecting this owner as current later on.
                  (superchat--response-region-begin transcript-owner)
                  (superchat--response-region-clear-status transcript-owner))))))))
     :stream
     (lambda (_run chunk)
       (when (current-p)
         (with-current-buffer buffer
           (let* ((superchat-buffer-name (buffer-name buffer))
                  ;; Contract §3.1's stream callback receives `(:kind
                  ;; text|reasoning :text STRING)'.  A conforming producer or
                  ;; direct Sink caller already hands us that shape; today's
                  ;; raw transport chunk (a plain string, or the legacy
                  ;; `(reasoning X)'/`(:reasoning X)' cons, still emitted by
                  ;; `superchat--llm-generate-answer' in `superchat-llm.el',
                  ;; outside this fix's write scope) is normalized onto the
                  ;; same shape by the already-tested
                  ;; `superchat-sink-normalize-chunk' — both are dispatched
                  ;; below instead of being handed to the legacy
                  ;; string-or-cons-only `superchat--stream-llm-result',
                  ;; which does not recognize the normalized plist and
                  ;; signals `wrong-type-argument' on it.
                  (payload (if (and (listp chunk) (plist-member chunk :kind))
                               chunk
                             (superchat-sink-normalize-chunk chunk))))
             ;; First chunk for a brand-new OWNER: establish and select its
             ;; state so it becomes the current owner (an explicit
             ;; `transcript-owner' only re-publishes an already-current
             ;; owner; `--response-region-begin' is what makes a new one
             ;; current for the first time, exactly as the pre-S02 bridge did).
             (superchat--response-region-begin transcript-owner)
             (pcase (plist-get payload :kind)
               ('text (superchat--response-region-stream
                       (plist-get payload :text) transcript-owner))
               ('reasoning (superchat--response-region-add-reasoning
                            (plist-get payload :text) transcript-owner)))))))
     :final
     (lambda (run payload)
       ;; A stop requested during/after this outcome's own terminal race
       ;; (the Run legitimately completed, but the interactive stop landed
       ;; before this callback ran) must not let it render -- unrelated to
       ;; the `:error' `cancelled' marker below, which is a real cancel and
       ;; always renders.
       (when (and (current-p) (not (superchat-runtime--sink-stop-requested-p run)))
         (with-current-buffer buffer
           (let ((superchat-buffer-name (buffer-name buffer)))
             (superchat--response-region-begin transcript-owner)
             (superchat--process-llm-result
              (plist-get payload :text) (plist-get payload :tape-id)
              transcript-owner)
             (budget-notice payload)
             (record-line run)))))
     :error
     (lambda (run failure)
       (when (current-p)
         (with-current-buffer buffer
           (let ((superchat-buffer-name (buffer-name buffer)))
             (superchat--response-region-begin transcript-owner)
             (pcase (plist-get failure :status)
               ('cancelled
                (let* ((state (superchat--response-region--state-for-owner
                               transcript-owner transcript-owner))
                       (partial (string-join (reverse (plist-get state :parts)) "")))
                  (superchat--response-region-close
                   (if (string-empty-p partial)
                       "[Stopped]"
                     (concat partial "\n\n[Stopped]"))
                   t transcript-owner)
                  (superchat--response-region-ensure-view-prompt)))
               ('audit-incomplete
                (unless (superchat-runtime--sink-stop-requested-p run)
                  (superchat--process-llm-result
                   (plist-get failure :text) (plist-get failure :tape-id)
                   transcript-owner)
                  (when (plist-get failure :tape-id)
                    (ignore-errors
                      (superchat--response-region-insert-persistent
                       "[Audit incomplete: Run could not be fully closed; see *Messages* for details.]"
                       nil transcript-owner)))))
               (_
                (unless (superchat-runtime--sink-stop-requested-p run)
                  (superchat--process-llm-result
                   (plist-get failure :text) (plist-get failure :tape-id)
                   transcript-owner)
                  (record-line run))))
             (budget-notice failure)))))
     :orphaned (lambda (_run _reason) nil))))


;; No plugin row here.  `superchat-render-markdown-to-org' and
;; `superchat-render-view-sink' are called directly -- by the dispatcher, by
;; superchat.el, by inline and by Telega -- so rendering is not something a
;; composition can take away (2026-09-16 decision: rendering is a function).

(provide 'superchat-render)
;;; superchat-render.el ends here
