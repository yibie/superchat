;;; superchat-inline.el --- Inline ask: question at point, answer in an overlay -*- lexical-binding: t; -*-

;;; Commentary:

;; Inline ask (Producer surface `inline-ask', contract §2.1) asks a question
;; about the code at point and shows the answer in an overlay, never editing
;; the source buffer.  The reference -- the `defun' at point in `prog-mode',
;; else the current line -- travels as an `attachment' prompt fragment; the
;; answer streams into the overlay's `after-string' through an overlay Sink
;; (contract §3), so the source buffer's text, modified flag and undo list
;; stay exactly as they were.
;;
;; One Session per project, named `inline:<project-root>'
;; (`inline:global' outside a project or under the `global' scope, approved
;; decision 1).  The Run's tools come from the surface baseline (contract
;; §4.2 rev. 4): the user's own `superchat-authority-unprompted-read-tools'
;; list, so an inline ask never prompts by default.
;;
;; S18a: the session id, reference builder, overlay and its Sink,
;; clear/copy/menu/overlays, the `inline-reference' collector, and
;; `superchat-inline-ask' submitting the envelope through
;; `superchat-pipeline-submit'.  S18b: reference cycling with a highlight
;; (`superchat-inline-cycle-reference', \`C-c SPC' in the ask minibuffer),
;; reply in place (`superchat-inline-reply'), and visiting the inline
;; conversation as a Session View (`superchat-inline-visit-session' /
;; `superchat-inline-return').

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'subr-x)
(require 'superchat-prompt)
(require 'superchat-sink)
(require 'superchat-event)

(defgroup superchat-inline nil
  "Inline ask: question at point, answer in an overlay."
  :group 'superchat)

(defcustom superchat-inline-viewport-lines 8
  "Maximum answer lines shown in an inline overlay."
  :type '(integer :tag "Lines")
  :group 'superchat-inline)

(defcustom superchat-inline-session-scope 'project
  "Session scope for inline ask.
`project' keeps one Session per project, named
\"inline:<project-root>\"; `global' uses one Session for every buffer."
  :type '(choice (const :tag "One per project" project)
                 (const :tag "One global Session" global))
  :group 'superchat-inline)

(defvar superchat-inline-ask-history nil
  "Minibuffer history of inline ask questions.")

(defcustom superchat-inline-reference-candidates
  '((prog-mode region none defun sexp line window buffer)
    (text-mode region none line sentence paragraph window buffer)
    (t region none line paragraph window buffer))
  "Reference candidates, per major mode, cycled by `superchat-inline-cycle-reference'.
Each entry is (MODE CANDIDATE...); the first MODE the buffer derives from
wins, `t' is the fallback.  `region' is offered only while the region is
active; `none' sends no reference at all."
  :type '(repeat (cons symbol (repeat symbol)))
  :group 'superchat-inline)

(defvar-local superchat-inline--reference nil
  "The reference candidate symbol currently selected in this buffer.")

(defvar-local superchat-inline--reference-highlight nil
  "The highlight overlay of the selected reference range, or nil.")

(defvar-local superchat-inline--saved-header-line nil
  "The buffer's `header-line-format' before the reference header was shown.")

(defvar superchat-inline--visit-return nil
  "Where `superchat-inline-return' goes back to: (WINDOW-CONFIG BUFFER POINT).")

(defconst superchat-inline--error-display-seconds 5
  "Seconds an inline error stays visible before its overlay is cleared.")

(defconst superchat-inline--overlay-priority 65
  "Overlay priority of an inline answer, above the source's own overlays.")

;;; Session

(defun superchat-inline-session-id (&optional buffer)
  "Return the inline ask Session id for BUFFER (default current buffer).
`superchat-inline-session-scope' `project' gives one Session per project,
named \"inline:<project-root>\"; outside a project, and for the `global'
scope, the id is \"inline:global\".  Never prompts."
  (let* ((buffer (if (and (bufferp buffer) (buffer-live-p buffer))
                     buffer
                   (current-buffer)))
         (root (and (eq superchat-inline-session-scope 'project)
                    (with-current-buffer buffer
                      (when-let* ((project (ignore-errors
                                             (project-current nil))))
                        (ignore-errors (project-root project)))))))
    (if (and (stringp root) (not (string-empty-p root)))
        (format "inline:%s" (file-name-as-directory (file-truename root)))
      "inline:global")))

;;; Reference

(defun superchat-inline--reference-bounds ()
  "Return the reference range at point as (BEG . END).
The `defun' at point wins in `prog-mode'; otherwise the current line."
  (or (and (derived-mode-p 'prog-mode)
           (bounds-of-thing-at-point 'defun))
      (cons (line-beginning-position) (line-end-position))))

(defun superchat-inline--reference-label (beg end)
  "Return the reference label for BEG..END in the current buffer."
  (format "In buffer \"%s\" (file \"%s\"), lines %d–%d:"
          (buffer-name)
          (or (buffer-file-name) "")
          (line-number-at-pos beg)
          (line-number-at-pos (max beg (1- end)))))

(defun superchat-inline--reference-block (beg end)
  "Return the reference block for BEG..END in the current buffer.
The block is the label line followed by a fenced code block carrying the
exact BEG..END text."
  (let ((text (buffer-substring-no-properties beg end))
        (lang (replace-regexp-in-string
               "-mode\\'" "" (symbol-name major-mode))))
    (format "%s\n```%s\n%s\n```"
            (superchat-inline--reference-label beg end)
            lang
            text)))

(defun superchat-inline--reference (&optional bounds)
  "Return a reference plist for BOUNDS (default the reference at point).
The plist carries `:beg', `:end', `:label' and `:block'."
  (let* ((bounds (or bounds (superchat-inline--reference-bounds)))
         (beg (car bounds))
         (end (cdr bounds)))
    (list :beg beg :end end
          :label (superchat-inline--reference-label beg end)
          :block (superchat-inline--reference-block beg end))))

(defun superchat-inline--normalize-reference (reference)
  "Return REFERENCE as a reference plist.
REFERENCE nil builds the default reference at point; a (BEG . END) pair is
used as the reference range."
  (cond
   ((eq reference 'none) nil)
   ((null reference)
    (if superchat-inline--reference
        (superchat-inline--normalize-reference superchat-inline--reference)
      (superchat-inline--reference)))
   ((and (symbolp reference) (superchat-inline-reference-bounds reference))
    (superchat-inline--reference (superchat-inline-reference-bounds reference)))
   ((and (consp reference)
         (integerp (car reference))
         (integerp (cdr reference)))
    (superchat-inline--reference reference))
   (t (user-error "Invalid inline reference: %S" reference))))

;;; Reference cycling (S18b)

(defun superchat-inline--candidates ()
  "Return the candidate list for the current buffer's major mode."
  (or (cdr (cl-find-if (lambda (entry)
                         (or (eq (car entry) t) (derived-mode-p (car entry))))
                       superchat-inline-reference-candidates))
      '(region none line window buffer)))

(defun superchat-inline-reference-bounds (candidate)
  "Return CANDIDATE's range at point as (BEG . END), or nil.
CANDIDATE is one of the symbols in `superchat-inline-reference-candidates'."
  (pcase candidate
    ('none nil)
    ('region (and (use-region-p) (cons (region-beginning) (region-end))))
    ('defun (bounds-of-thing-at-point 'defun))
    ('sexp (bounds-of-thing-at-point 'sexp))
    ('line (cons (line-beginning-position) (line-end-position)))
    ('sentence (bounds-of-thing-at-point 'sentence))
    ('paragraph (bounds-of-thing-at-point 'paragraph))
    ('window (let ((window (get-buffer-window (current-buffer))))
               (if window
                   (cons (window-start window) (window-end window t))
                 (cons (point-min) (point-max)))))
    ('buffer (cons (point-min) (point-max)))
    (_ nil)))

(defun superchat-inline--clear-reference-highlight ()
  "Remove the reference highlight and header line of the current buffer."
  (when (overlayp superchat-inline--reference-highlight)
    (delete-overlay superchat-inline--reference-highlight))
  (setq superchat-inline--reference-highlight nil)
  (when (local-variable-p 'superchat-inline--saved-header-line)
    (setq header-line-format superchat-inline--saved-header-line)
    (kill-local-variable 'superchat-inline--saved-header-line)))

(defun superchat-inline--highlight-reference (candidate bounds)
  "Highlight BOUNDS as the selected CANDIDATE and announce it in the header line."
  (superchat-inline--clear-reference-highlight)
  (unless (local-variable-p 'superchat-inline--saved-header-line)
    (setq superchat-inline--saved-header-line header-line-format))
  (when bounds
    (let ((overlay (make-overlay (car bounds) (cdr bounds))))
      (overlay-put overlay 'face 'secondary-selection)
      (overlay-put overlay 'superchat-inline-reference candidate)
      (overlay-put overlay 'priority 60)
      (setq superchat-inline--reference-highlight overlay)))
  (setq header-line-format (format "Including %s" candidate)))

;;;###autoload
(defun superchat-inline-cycle-reference ()
  "Select the next reference candidate for the inline ask and highlight it.
Cycles through `superchat-inline-reference-candidates' for the buffer's
mode, skipping `region' while the region is inactive and candidates with
no range at point.  Returns the selected candidate symbol."
  (interactive)
  (let* ((buffer (if (minibufferp) (window-buffer (minibuffer-selected-window))
                   (current-buffer))))
    (with-current-buffer buffer
      (let* ((candidates (superchat-inline--candidates))
             (available (cl-remove-if-not
                         (lambda (candidate)
                           (or (eq candidate 'none)
                               (superchat-inline-reference-bounds candidate)))
                         candidates))
             (position (cl-position superchat-inline--reference available))
             (next (or (and position (nth (1+ position) available))
                       (car available))))
        (setq superchat-inline--reference next)
        (superchat-inline--highlight-reference
         next (superchat-inline-reference-bounds next))
        (unless (minibufferp) (message "Including %s" next))
        next))))

(defun superchat-inline--minibuffer-exit ()
  "Clear the reference highlight in the buffer the ask was started from."
  (let ((buffer (window-buffer (minibuffer-selected-window))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (superchat-inline--clear-reference-highlight)))))

(defvar superchat-inline-ask-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c SPC") #'superchat-inline-cycle-reference)
    map)
  "Keymap of the inline ask minibuffer; \\`C-c SPC' cycles the reference.")

;;; Overlay

(defun superchat-inline--overlay-bounds ()
  "Return the overlay range at point as (BEG . END).
The current line; on an empty line borrow one character after point, else
one before, so the overlay always covers at least one character."
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when (= beg end)
      (cond ((< end (point-max)) (setq end (1+ end)))
            ((> beg (point-min)) (setq beg (1- beg)))))
    (cons beg end)))

(defun superchat-inline--overlay-at-point ()
  "Return the inline answer overlay at point, or nil."
  (cl-find-if (lambda (overlay)
                (overlay-get overlay 'superchat-inline))
              (overlays-at (point))))

(defun superchat-inline--overlay ()
  "Return the inline answer overlay this command addresses, or signal."
  (or (superchat-inline--overlay-at-point)
      (user-error "No inline answer here")))

(defvar superchat-inline--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-RET") #'superchat-inline-menu)
    (define-key map [mouse-1] #'superchat-inline-menu)
    (define-key map (kbd "q") #'superchat-inline-clear)
    (define-key map (kbd "r") #'superchat-inline-reply)
    (define-key map (kbd "v") #'superchat-inline-visit-session)
    map)
  "Transient keymap of an inline answer overlay.")

(defun superchat-inline--status-header (state)
  "Return the viewport status header line for STATE."
  (pcase (plist-get state :status)
    ('waiting "superchat inline · Waiting…")
    ('typing "superchat inline · Typing…")
    ('ready "superchat inline · Ready · M-RET menu")
    ('error (format "superchat inline · Error: %s"
                    (or (plist-get state :error-message) "failed")))
    (_ "superchat inline")))

(defun superchat-inline--viewport-body (text)
  "Return TEXT rendered and trimmed to the last viewport lines.
TEXT goes through `superchat-render-markdown-to-org' when that renderer is
loaded, else through verbatim."
  (let* ((rendered (if (fboundp 'superchat-render-markdown-to-org)
                       (superchat-service-call nil 'view 'markdown-to-org (or text ""))
                     (or text "")))
         (lines (split-string (string-trim-right rendered "\n+") "\n"))
         (limit (max 1 superchat-inline-viewport-lines)))
    (string-join (last lines limit) "\n")))

(defun superchat-inline--render (overlay)
  "Refresh OVERLAY's after-string viewport from its state.
The string always ends in a newline so following display lines never
shift."
  (let* ((state (overlay-get overlay 'superchat-inline))
         (body (superchat-inline--viewport-body (plist-get state :text))))
    (overlay-put overlay 'after-string
                 (concat (superchat-inline--status-header state) "\n"
                         (if (string-empty-p body) "" (concat body "\n"))))))

(defun superchat-inline--set-state (overlay key value)
  "Set KEY to VALUE in OVERLAY's state and re-render its viewport."
  (overlay-put overlay 'superchat-inline
               (plist-put (overlay-get overlay 'superchat-inline) key value))
  (superchat-inline--render overlay))

(defun superchat-inline--make-overlay (beg end state)
  "Create and return an inline overlay over BEG..END carrying STATE."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'superchat-inline state)
    (overlay-put overlay 'keymap superchat-inline--keymap)
    (overlay-put overlay 'evaporate nil)
    (overlay-put overlay 'priority superchat-inline--overlay-priority)
    (superchat-inline--render overlay)
    overlay))

;;; Sink

(defun superchat-inline--sink (overlay)
  "Return OVERLAY's Sink (contract §3).
`:live-p' is true only while OVERLAY and its buffer are live.  Stream
chunks append to the viewport, statuses update the header, the final text
becomes the Ready answer, and an error is shown for
`superchat-inline--error-display-seconds' before the overlay is cleared."
  (superchat-sink-create
   :id (format "inline-%08x" (random #x100000000))
   :live-p (lambda ()
             (and (overlayp overlay)
                  (overlay-buffer overlay)
                  (buffer-live-p (overlay-buffer overlay))))
   :status (lambda (_run status)
             (superchat-inline--set-state
              overlay :status
              (pcase status
                ('waiting 'waiting)
                ((or 'typing 'calling-tool) 'typing)
                ('ready 'ready)
                (_ 'error))))
   :stream (lambda (_run chunk)
             (let* ((payload (if (and (listp chunk) (plist-member chunk :kind))
                                 chunk
                               (superchat-sink-normalize-chunk chunk)))
                    (text (and payload (plist-get payload :text))))
               (when (and (stringp text) (not (string-empty-p text)))
                 (let ((state (overlay-get overlay 'superchat-inline)))
                   (superchat-inline--set-state
                    overlay :text (concat (or (plist-get state :text) "") text)))
                 (unless (eq 'ready
                             (plist-get (overlay-get overlay 'superchat-inline)
                                        :status))
                   (superchat-inline--set-state overlay :status 'typing)))))
   :final (lambda (_run result)
            (superchat-inline--set-state overlay :text
                                         (or (plist-get result :text) ""))
            (superchat-inline--set-state overlay :status 'ready))
   :error (lambda (_run failure)
            (superchat-inline--set-state
             overlay :error-message (or (plist-get failure :text) "failed"))
            (superchat-inline--set-state overlay :status 'error)
            (superchat-inline--set-state
             overlay :error-timer
             (run-at-time superchat-inline--error-display-seconds nil
                          (lambda () (superchat-inline-clear overlay)))))
   :orphaned (lambda (_run _reason)
               (superchat-inline--set-state overlay :error-message "sink lost")
               (superchat-inline--set-state overlay :status 'error))))

;;; Commands

;;;###autoload
(defun superchat-inline-ask (question &optional reference)
  "Ask QUESTION about the reference at point; show the answer in an overlay.
REFERENCE, when non-nil, is a (BEG . END) range to use instead of the
reference at point.  Returns the overlay.  The source buffer is never
edited: the answer lives only in the overlay's `after-string'."
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda () (add-hook 'minibuffer-exit-hook
                                  #'superchat-inline--minibuffer-exit nil t))
           (read-from-minibuffer "Ask (C-c SPC cycles the reference): " nil
                                 superchat-inline-ask-minibuffer-map nil
                                 'superchat-inline-ask-history))))
  (unless (and (stringp question) (not (string-empty-p (string-trim question))))
    (user-error "Inline ask needs a question"))
  (let* ((buffer (current-buffer))
         (session-id (superchat-inline-session-id buffer))
         (reference (superchat-inline--normalize-reference reference))
         (overlay-bounds (superchat-inline--overlay-bounds))
         (state (list :run-id nil
                      :session-id session-id
                      :question question
                      :reference (and reference
                                      (cons (plist-get reference :beg)
                                            (plist-get reference :end)))
                      :status 'waiting
                      :text ""))
         (overlay (superchat-inline--make-overlay
                   (car overlay-bounds) (cdr overlay-bounds) state)))
    (superchat-inline--clear-reference-highlight)
    (setq superchat-inline--reference nil)
    (superchat-inline--submit overlay question reference)))

(defun superchat-inline--submit (overlay question reference)
  "Submit QUESTION about REFERENCE (a reference plist or nil) for OVERLAY.
Used by `superchat-inline-ask' and `superchat-inline-reply'; returns OVERLAY."
  (let* ((buffer (overlay-buffer overlay))
         (state (overlay-get overlay 'superchat-inline))
         (session-id (plist-get state :session-id))
         (sink (superchat-inline--sink overlay))
         (envelope (list :surface 'inline-ask
                         :session-id session-id
                         :content question
                         :tools nil
                         :attendance 'attended
                         :sink sink
                         :context (append
                                   (list :inline-buffer buffer
                                         :inline-range (cons (overlay-start overlay)
                                                             (overlay-end overlay)))
                                   (when reference
                                     (list :inline-reference-text
                                           (plist-get reference :block)
                                           :inline-reference-label
                                           (plist-get reference :label)))))))
    (superchat-inline--set-state overlay :question question)
    (superchat-inline--set-state overlay :text "")
    (superchat-inline--set-state overlay :status 'waiting)
    (let ((handle (superchat-service-call nil 'driver 'submit envelope)))
      (when (plist-get handle :run-id)
        (superchat-inline--set-state overlay :run-id (plist-get handle :run-id))))
    overlay))

;;;###autoload
(defun superchat-inline-reply (&optional overlay question)
  "Ask a follow-up QUESTION in OVERLAY's inline conversation (S18b).
The same overlay and Session are reused, so the earlier turns reach the
model as history; the reference range of the first ask is sent again."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (state (overlay-get overlay 'superchat-inline))
         (question (or question
                       (read-string "Reply: " nil 'superchat-inline-ask-history))))
    (unless (and (stringp question) (not (string-empty-p (string-trim question))))
      (user-error "Inline reply needs a question"))
    (when (memq (plist-get state :status) '(waiting typing))
      (user-error "The previous answer is still streaming"))
    (let ((timer (plist-get state :error-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (with-current-buffer (overlay-buffer overlay)
      (superchat-inline--submit
       overlay question
       (let ((range (plist-get state :reference)))
         (and range (superchat-inline--reference range)))))))

;;;###autoload
(defun superchat-inline-visit-session (&optional overlay)
  "Open the inline conversation of OVERLAY as a Session View (S18b).
`superchat-inline-return' comes back to the exact buffer, point and window
configuration."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (session-id (plist-get (overlay-get overlay 'superchat-inline) :session-id)))
    (setq superchat-inline--visit-return
          (list (current-window-configuration) (current-buffer) (point)))
    (require 'superchat)
    (superchat-open-session session-id)))

;;;###autoload
(defun superchat-inline-return ()
  "Return from `superchat-inline-visit-session' to where the visit started."
  (interactive)
  (pcase superchat-inline--visit-return
    (`(,config ,buffer ,point)
     (setq superchat-inline--visit-return nil)
     (when (window-configuration-p config)
       (set-window-configuration config))
     (when (buffer-live-p buffer)
       (switch-to-buffer buffer)
       (goto-char point)))
    (_ (user-error "No inline visit to return from"))))

;;;###autoload
(defun superchat-inline-clear (&optional overlay)
  "Remove OVERLAY (default the inline answer at point) and cancel its Run."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (state (and (overlayp overlay) (overlay-get overlay 'superchat-inline)))
         (timer (plist-get state :error-timer))
         (run-id (plist-get state :run-id)))
    (when (timerp timer)
      (cancel-timer timer))
    (when (and run-id (fboundp 'superchat-runtime-cancel))
      (ignore-errors (superchat-service-call nil 'driver 'cancel run-id "inline answer cleared")))
    (when (overlayp overlay)
      (delete-overlay overlay))))

;;;###autoload
(defun superchat-inline-copy (&optional overlay)
  "Copy the answer of OVERLAY (default the inline answer at point)."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (text (or (plist-get (overlay-get overlay 'superchat-inline) :text) "")))
    (if (string-empty-p text)
        (user-error "No inline answer to copy")
      (kill-new text)
      (message "Copied inline answer"))))

;;;###autoload
(defun superchat-inline-menu (&optional overlay)
  "Show the inline answer menu for OVERLAY (default the one at point)."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (choice (read-multiple-choice
                  "Superchat inline: "
                  '((?r "reply" "ask a follow-up in this conversation")
                    (?v "visit" "open the conversation as a Session View")
                    (?c "clear" "remove this answer")
                    (?w "copy" "copy the answer to the kill ring")
                    (?q "quit" "leave it as is")))))
    (pcase (car choice)
      (?r (superchat-inline-reply overlay))
      (?v (superchat-inline-visit-session overlay))
      (?c (superchat-inline-clear overlay))
      (?w (superchat-inline-copy overlay))
      (?q nil))))

(defun superchat-inline-overlays (&optional buffer)
  "Return live inline answer overlays in BUFFER (default current buffer)."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (with-current-buffer buffer
           (cl-remove-if-not (lambda (overlay)
                               (overlay-get overlay 'superchat-inline))
                             (overlays-in (point-min) (point-max)))))))

;;; Collector

(defun superchat-inline--collect-reference (env _state _tools)
  "Return ENV's inline reference as one hard `attachment' fragment.
The envelope's `:context' carries `:inline-reference-text' only for an
inline ask; every other surface contributes nothing here."
  (let ((text (plist-get (plist-get env :context) :inline-reference-text)))
    (when (and (stringp text) (not (string-empty-p text)))
      (list (superchat-prompt-fragment
             :owner 'inline :id "inline-reference"
             :slot 'attachment :text text
             :priority 0 :budget-class 'hard :required t)))))

;; The `inline' plugin (superchat-default-plugins.el) registers
;; `superchat-inline--collect-reference' on collect-context.

(provide 'superchat-inline)
;;; superchat-inline.el ends here
