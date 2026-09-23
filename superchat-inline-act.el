;;; superchat-inline-act.el --- Inline act: a tool Turn at point, reported in an overlay -*- lexical-binding: t; -*-

;;; Commentary:

;; Inline act (Producer surface `inline-act') starts a full tool Turn from
;; the buffer the user is editing and reports it in the same inline overlay
;; as `superchat-inline-ask'.  The envelope carries `:tools nil', so the
;; default tool baseline, the ordinary permission levels and the approval
;; seam apply unchanged; the chat window is never opened.
;;
;; Reuse: `superchat-inline--make-overlay' and its accessors build the
;; overlay, and the `inline' context collector already carries the reference
;; because it reads `:inline-reference-text' from the envelope's context.
;;
;; The header distinguishes a tool call from the model typing: while the row
;; is mounted it advises `superchat-inline--status-header' so `calling-tool'
;; shows "Working: tool…" instead of inline-ask's collapsed "Typing…".

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-inline)
(require 'superchat-changes)

(defvar superchat-inline-act-history nil
  "Minibuffer history of inline act instructions.")

;;; Header

(defun superchat-inline-act--status-header (original state)
  "Around advice on `superchat-inline--status-header' for `inline-act'.
Show the Working line when STATE's status is `working'; otherwise call
ORIGINAL, inline-ask's own header."
  (if (eq 'working (plist-get state :status))
      "superchat inline · Working: tool…"
    (funcall original state)))

(defun superchat-inline-act--install-header-advice ()
  "Install the inline-act header advice; the row's Scope removes it.
Called from the `inline-act' row's `:apply'."
  (advice-add 'superchat-inline--status-header :around
              #'superchat-inline-act--status-header)
  (superchat-plugin-record-undo
   (lambda ()
     (advice-remove 'superchat-inline--status-header
                    #'superchat-inline-act--status-header))))

;;; Footer

(defun superchat-inline-act--footer (session-id run-id)
  "Return the footer for the Run RUN-ID in SESSION-ID, or an empty string.
The footer states what the Tape and the live buffers show -- how many tool
calls the Run made and which buffers it left unsaved -- never that the task
succeeded.  Empty when there is nothing to say."
  (let* ((events (and (stringp session-id) (stringp run-id)
                      (ignore-errors
                        (superchat-tape-replay-events session-id))))
         (calls (cl-count-if
                 (lambda (event)
                   (and (equal run-id (plist-get event :run-id))
                        (member (plist-get event :type)
                                '("tool.completed" "tool.failed"))))
                 events))
         (summary (and run-id
                       (ignore-errors
                         (superchat-changes-run-summary session-id run-id))))
         (lines (append (when (> calls 0)
                          (list (format "— %d tool call%s"
                                        calls (if (= calls 1) "" "s"))))
                        (when summary (list summary)))))
    (if lines (string-join lines "\n") "")))

(defun superchat-inline-act--finish-text (overlay text)
  "Return TEXT with OVERLAY's Run footer appended, or TEXT when there is none."
  (let* ((state (overlay-get overlay 'superchat-inline))
         (answer (or text ""))
         (footer (superchat-inline-act--footer (plist-get state :session-id)
                                               (plist-get state :run-id))))
    (cond ((string-empty-p footer) answer)
          ((string-empty-p answer) footer)
          (t (concat answer "\n\n" footer)))))

;;; Sink

(defun superchat-inline-act--sink (overlay)
  "Return OVERLAY's Sink (contract §3).
Like `superchat-inline--sink', except a `calling-tool' status is stored as
`working' rather than collapsed to `typing'; the header advice renders it."
  (superchat-sink-create
   :id (format "inline-act-%08x" (random #x100000000))
   :live-p (lambda ()
             (and (overlayp overlay)
                  (overlay-buffer overlay)
                  (buffer-live-p (overlay-buffer overlay))))
   :status (lambda (_run status)
             (superchat-inline--set-state
              overlay :status
              (pcase status
                ('waiting 'waiting)
                ('typing 'typing)
                ('calling-tool 'working)
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
            (superchat-inline--set-state
             overlay :text
             (superchat-inline-act--finish-text overlay (plist-get result :text)))
            (superchat-inline--set-state overlay :status 'ready))
   :error (lambda (_run failure)
            (superchat-inline--set-state
             overlay :text
             (superchat-inline-act--finish-text
              overlay
              (plist-get (overlay-get overlay 'superchat-inline) :text)))
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

;;; Keys

(defvar superchat-inline-act--keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map superchat-inline--keymap)
    (define-key map (kbd "d") #'superchat-inline-act-diff)
    (define-key map (kbd "r") #'superchat-inline-act-reply)
    (define-key map (kbd "M-RET") #'superchat-inline-act-menu)
    (define-key map [mouse-1] #'superchat-inline-act-menu)
    map)
  "Transient keymap of an inline act overlay.
It inherits inline ask's keys and replaces the ones whose inline ask meaning
would be wrong here: `r' and the menu reply on the `inline-act' surface, so
tools stay available, and the menu also offers the diff.")

(defun superchat-inline-act-menu (&optional overlay)
  "Show the inline act menu for OVERLAY (default the one at point)."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (choice (read-multiple-choice
                  "Superchat inline act: "
                  '((?r "reply" "ask a follow-up; tools stay available")
                    (?d "diff" "show how an edited buffer differs from its file")
                    (?v "visit" "open the conversation as a Session View")
                    (?c "clear" "cancel a Run still going and remove this answer")
                    (?w "copy" "copy the answer to the kill ring")
                    (?q "quit" "leave it as is")))))
    (pcase (car choice)
      (?r (superchat-inline-act-reply overlay))
      (?d (superchat-inline-act-diff overlay))
      (?v (superchat-inline-visit-session overlay))
      (?c (superchat-inline-clear overlay))
      (?w (superchat-inline-copy overlay))
      (?q nil))))

(defun superchat-inline-act-diff (&optional overlay)
  "Show how the buffer OVERLAY's Run edited differs from its file."
  (interactive)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (state (overlay-get overlay 'superchat-inline))
         (rows (superchat-changes-candidates (plist-get state :session-id)
                                             (plist-get state :run-id))))
    (unless rows
      (user-error "This Run left no unsaved edited buffer"))
    (let* ((names (mapcar (lambda (row) (plist-get row :name)) rows))
           (name (if (= 1 (length rows))
                     (car names)
                   (completing-read "Buffer: " names nil t))))
      (superchat-changes-diff (get-buffer name)))))

(defun superchat-inline-act-reply (&optional overlay question)
  "Ask a follow-up QUESTION in OVERLAY's inline act conversation.
The same overlay, Session and surface are reused, so the earlier turns
reach the model as history and the reply stays an `inline-act' Run."
  (interactive)
  (superchat-plugin-require-active-row 'inline-act)
  (let* ((overlay (or overlay (superchat-inline--overlay)))
         (state (overlay-get overlay 'superchat-inline))
         (question (or question
                       (read-string "Reply: " nil 'superchat-inline-act-history))))
    (unless (and (stringp question) (not (string-empty-p (string-trim question))))
      (user-error "Inline act reply needs a question"))
    (when (memq (plist-get state :status) '(waiting typing working))
      (user-error "The previous answer is still streaming"))
    (let ((timer (plist-get state :error-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (with-current-buffer (overlay-buffer overlay)
      (superchat-inline-act--submit
       overlay question
       (let ((range (plist-get state :reference)))
         (and range (superchat-inline--reference range)))))))

;;; Commands

;;;###autoload
(defun superchat-inline-act (instruction &optional reference)
  "Run INSTRUCTION as a full tool Turn at point; report it in an overlay.
REFERENCE, when non-nil, is a (BEG . END) range to use instead of the
reference at point.  Returns the overlay.  The Run uses the default tool
baseline, so tools may edit buffers and the approval seam asks in the
minibuffer exactly as it does for the chat window; no chat buffer is
opened."
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda () (add-hook 'minibuffer-exit-hook
                                  #'superchat-inline--minibuffer-exit nil t))
           (read-from-minibuffer "Act (C-c SPC cycles the reference): " nil
                                 superchat-inline-ask-minibuffer-map nil
                                 'superchat-inline-act-history))))
  (superchat-plugin-require-active-row 'inline-act)
  (unless (and (stringp instruction)
               (not (string-empty-p (string-trim instruction))))
    (user-error "Inline act needs an instruction"))
  (let* ((buffer (current-buffer))
         (session-id (superchat-inline-session-id buffer))
         (reference (superchat-inline--normalize-reference reference))
         (overlay-bounds (superchat-inline--overlay-bounds))
         (state (list :surface 'inline-act
                      :run-id nil
                      :session-id session-id
                      :question instruction
                      :reference (and reference
                                      (cons (plist-get reference :beg)
                                            (plist-get reference :end)))
                      :status 'waiting
                      :text ""))
         (overlay (superchat-inline--make-overlay
                   (car overlay-bounds) (cdr overlay-bounds) state)))
    (overlay-put overlay 'keymap superchat-inline-act--keymap)
    (superchat-inline--clear-reference-highlight)
    (setq superchat-inline--reference nil)
    (superchat-inline-act--submit overlay instruction reference)))

(defun superchat-inline-act--submit (overlay instruction reference)
  "Submit INSTRUCTION about REFERENCE (a reference plist or nil) for OVERLAY.
The envelope's surface is `inline-act', `:tools' is nil so the default
baseline applies, and the context gains `:inline-act t'; the Sink is built
by `superchat-inline-act--sink'.  Returns OVERLAY."
  (superchat-plugin-require-active-row 'inline-act)
  (let* ((buffer (overlay-buffer overlay))
         (state (overlay-get overlay 'superchat-inline))
         (session-id (plist-get state :session-id))
         (sink (superchat-inline-act--sink overlay))
         (envelope (list :surface 'inline-act
                         :session-id session-id
                         :content instruction
                         :tools nil
                         :sink sink
                         :context (append
                                   (list :inline-buffer buffer
                                         :inline-range (cons (overlay-start overlay)
                                                             (overlay-end overlay))
                                         :inline-act t)
                                   (when reference
                                     (list :inline-reference-text
                                           (plist-get reference :block)
                                           :inline-reference-label
                                           (plist-get reference :label)))))))
    (superchat-inline--set-state overlay :question instruction)
    (superchat-inline--set-state overlay :text "")
    (superchat-inline--set-state overlay :status 'waiting)
    (let ((handle (superchat-service-call nil 'driver 'submit envelope)))
      (when (plist-get handle :run-id)
        (superchat-inline--set-state overlay :run-id (plist-get handle :run-id))))
    overlay))

(provide 'superchat-inline-act)
;;; superchat-inline-act.el ends here
