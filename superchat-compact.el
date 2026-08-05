;;; superchat-compact.el --- Session compaction for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Implements session compaction following the tape.systems context
;; architecture: long conversations are summarized into an `anchor'
;; row on the tape, and subsequent prompts start from that anchor.

;;; Code:

(require 'cl-lib)
(require 'superchat-runtime)

;; External declarations
(declare-function superchat--recent-conversation-messages "superchat" (&optional message-limit char-limit))
(declare-function superchat--format-conversation-message "superchat" (message))
(declare-function superchat-runtime-submit "superchat-runtime" (request))
(declare-function superchat-db-tape-append "superchat-db" (session-id kind content &optional metadata))
(declare-function superchat-db-tape-get-by-session "superchat-db" (session-id))
(declare-function superchat-view-anchor-latest "superchat-tape-view" (session-id))
(declare-function superchat-view-expand-anchor "superchat-tape-view" (session-id anchor-row))
(declare-function superchat--insert-system-message "superchat-render" (content))

(defvar superchat--session-id)
(defvar superchat--conversation-history)
(defvar superchat-buffer-name)

;; ═══════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-compact nil
  "Session compaction settings for superchat."
  :group 'superchat)

(defcustom superchat-compact-prompt
  "Purpose: To create a comprehensive record that ensures no important details or context are lost between sessions.
This process prioritizes thoroughness over brevity to retain all critical information.

The contents of the summary depends on the type of the conversation:

## Coding or technical sessions working towards specific goals

- Overall purpose and goals of the interaction
- Important progress made in the current session
- Mistakes and dead-ends that should be avoided in subsequent steps
- Technical details that need to be preserved
- Key decisions and architectural changes
- Unfinished tasks and actionable next steps

Provide as much detail as necessary, and err on the side of providing too much information.  Be thorough.

## Exploratory conversations or non-technical sessions

Summarize the chat in a way that allows any LLM to continue the conversation based on the summary.

- Emphasize topics covered in the conversation
- In the order in which they were covered.  Retain the narrative flow of the conversation in your summary.
- Include points of disagreement with the user
- Be sure to include any explicit instructions provided by the user in their turns.
  You are expected to continue to follow these

Provide as much detail as necessary, and err on the side of providing too much information.  Be thorough."
  "System prompt used to compact a Superchat session into an anchor summary.
Can be a string or a function that returns a string."
  :type '(choice (string :tag "Custom prompt string")
                 (function :tag "Function that returns prompt string"))
  :group 'superchat-compact)

(defcustom superchat-compact-message-count 100
  "Maximum number of recent conversation messages to include in compaction.
Nil means include all messages."
  :type '(choice (const :tag "All messages" nil)
                 (integer :tag "Limit"))
  :group 'superchat-compact)

;; ═══════════════════════════════════════════════════════════
;; Anchor helpers
;; ═══════════════════════════════════════════════════════════

(defun superchat-compact--ensure-session-id ()
  "Return `superchat--session-id', creating one if necessary."
  (or (and (boundp 'superchat--session-id) superchat--session-id)
      (setq superchat--session-id
            (format "%s%04x"
                    (format-time-string "%Y%m%d-%H%M%S-")
                    (random 65536)))))

(defun superchat-compact--latest-anchor ()
  "Return the latest anchor plist for the current session, or nil.
The plist contains :id, :timestamp, :content, and :metadata."
  (when (and (boundp 'superchat--session-id)
             superchat--session-id
             (fboundp 'superchat-db-tape-get-by-session))
    (let ((rows (cl-remove-if-not
                 (lambda (row)
                   (string= (plist-get row :kind) "anchor"))
                 (superchat-db-tape-get-by-session superchat--session-id))))
      ;; Rows are typically returned oldest-first; take the last one.
      (car (last rows)))))

(defun superchat-compact--anchor-summary ()
  "Return the content of the latest anchor for the current session, or nil."
  (when-let* ((anchor (superchat-compact--latest-anchor)))
    (plist-get anchor :content)))

;; ═══════════════════════════════════════════════════════════
;; Anchor expansion
;; ═══════════════════════════════════════════════════════════

(defun superchat-compact--expand-anchor (&optional session-id)
  "Expand the latest anchor for SESSION-ID back into in-memory history.
Replaces the synthetic anchor system message with the original source
entries recovered from tape.  Returns the number of entries restored,
or nil if no anchor is found."
  (let* ((sid (or session-id
                  (and (boundp 'superchat--session-id) superchat--session-id)))
         (anchor (when sid (superchat-view-anchor-latest sid))))
    (when anchor
      (let* ((entries (superchat-view-expand-anchor sid anchor))
             (messages (mapcar
                        (lambda (row)
                          (let ((kind (nth 2 row))
                                (content (nth 3 row))
                                (id (nth 0 row)))
                            (pcase kind
                              ("user" (list :role 'user :content content :id id))
                              ("assistant" (list :role 'assistant :content content :id id))
                              (_ (list :role 'system :content content :id id)))))
                        entries)))
        (when messages
          (setq superchat--conversation-history
                (append (reverse messages)
                        (cl-remove-if (lambda (m) (plist-get m :anchor))
                                      superchat--conversation-history)))
          (message "Expanded anchor into %d entries." (length messages))
          (length messages))))))

;; ═══════════════════════════════════════════════════════════
;; Automatic handoff
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-handoff nil
  "Automatic session handoff settings for superchat."
  :group 'superchat)

(defcustom superchat-handoff-auto-enabled nil
  "When non-nil, automatically compact the session when the prompt
exceeds `superchat-handoff-char-threshold'.  Experimental."
  :type 'boolean
  :group 'superchat-handoff)

(defcustom superchat-handoff-char-threshold 12000
  "Approximate character count threshold for automatic handoff.
When `superchat-handoff-auto-enabled' is non-nil and the built
prompt exceeds this length, a background compaction Run is started;
the current Turn proceeds unchanged and later Turns start from the
anchor."
  :type 'integer
  :group 'superchat-handoff)

(defun superchat-handoff--prompt-char-count (turn)
  "Return the approximate character count of TURN's prompt."
  (+ (length (or (superchat-turn-prompt turn) ""))
     (length (or (superchat-turn-system-prompt turn) ""))))

(defun superchat-handoff--maybe-auto-compact (turn)
  "If the prompt is too long, start a background compaction.
TURN proceeds unchanged; the anchor benefits later Turns.  Blocking
here would freeze the main chat Surface on a nested Run.  Intended for
`superchat-post-topic-functions'."
  (when (and superchat-handoff-auto-enabled
             (boundp 'superchat--session-id)
             superchat--session-id
             (fboundp 'superchat-compact-session)
             (not (bound-and-true-p superchat-compact--in-flight))
             (> (superchat-handoff--prompt-char-count turn)
                superchat-handoff-char-threshold))
    (message "Prompt exceeds threshold; compacting session in the background...")
    (superchat-compact-session))
  turn)

;; Register automatic handoff on the post-topic hook.
(add-hook 'superchat-post-topic-functions #'superchat-handoff--maybe-auto-compact)

;; ═══════════════════════════════════════════════════════════
;; Compaction
;; ═══════════════════════════════════════════════════════════

(defun superchat-compact--collect-messages ()
  "Collect recent conversation messages eligible for compaction.
Returns a list of message plists from `superchat--conversation-history'."
  (when (and (boundp 'superchat--conversation-history)
             superchat--conversation-history)
    (let ((count superchat-compact-message-count)
          (history superchat--conversation-history))
      (if (null count)
          (reverse history)
        (reverse (cl-subseq history 0 (min count (length history))))))))

(defun superchat-compact--build-compaction-prompt (messages)
  "Build the full prompt sent to the LLM for compaction.
MESSAGES is a list of message plists."
  (let ((conversation-text
         (mapconcat
          (lambda (msg)
            (let ((role (plist-get msg :role))
                  (content (string-trim (or (plist-get msg :content) ""))))
              (when (and content (not (string-empty-p content)))
                (format "%s: %s" (capitalize (symbol-name role)) content))))
          messages
          "\n\n")))
    (format "%s\n\n%s"
            (if (functionp superchat-compact-prompt)
                (funcall superchat-compact-prompt)
              superchat-compact-prompt)
            conversation-text)))

(defvar-local superchat-compact--in-flight nil
  "Non-nil while an asynchronous compaction Run is pending for this buffer.")

(defun superchat-compact--anchor-message (summary)
  "Return the in-memory anchor system message for SUMMARY."
  `(:role system
    :content ,(concat "[Session anchor]\n\n" summary)
    :anchor t
    :timestamp ,(current-time)))

(defun superchat-compact--request (prompt session-id on-finish)
  "Build the compaction Runtime Request for PROMPT in SESSION-ID.
ON-FINISH receives (RESPONSE STATUS) at any terminal Run state."
  (list :input "Compact current session"
        :clean-input "Compact current session"
        :prompt prompt
        :session-id session-id
        :actor-id "compactor"
        :capability "compaction"
        :topology 'single
        :authority-policy "observer"
        :surface 'compaction
        :origin '(:surface compaction :kind session-compaction)
        :record-conversation nil
        :stream-function #'ignore
        :buffer-name superchat-buffer-name
        :delivery-function
        (lambda (response status _tape-id)
          (funcall on-finish response status))))

(defun superchat-compact--apply-summary
    (buffer session-id captured-length messages summary)
  "Persist SUMMARY as the anchor and collapse BUFFER's history onto it.
MESSAGES and CAPTURED-LENGTH describe the history snapshot the summary
covers; entries recorded after the snapshot are preserved in front of
the anchor.  When BUFFER died or moved to another session, the anchor
still reaches the tape and the in-memory history is left alone."
  (let ((metadata
         `(:source_ids ,(mapcar (lambda (m) (or (plist-get m :id) :unknown))
                                messages)
           :message_count ,(length messages)
           :timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    ;; ponytail: compaction anchors are prompt material, not bounded event
    ;; summaries; migrate when Tape has a material-content contract.
    (superchat-db-tape-append session-id "anchor" summary :meta metadata)
    (if (not (buffer-live-p buffer))
        (message "Session %s compacted; anchor kept on tape." session-id)
      (with-current-buffer buffer
        (if (not (and (equal session-id
                             (and (boundp 'superchat--session-id)
                                  superchat--session-id))
                      (>= (length superchat--conversation-history)
                          captured-length)))
            (message "Session %s compacted; history changed, anchor kept on tape only."
                     session-id)
          (let ((added (- (length superchat--conversation-history)
                          captured-length)))
            (setq superchat--conversation-history
                  (append (cl-subseq superchat--conversation-history 0 added)
                          (list (superchat-compact--anchor-message summary)))))
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (superchat--insert-system-message
               (format "Session compacted. Anchor: %s"
                       (substring (string-trim summary) 0
                                  (min 200 (length (string-trim summary))))))))
          (message "Session compacted into anchor."))))))

(defun superchat-compact--finish
    (buffer session-id captured-length messages response status)
  "Handle terminal RESPONSE with STATUS for the pending compaction Run."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq superchat-compact--in-flight nil)))
  (cond
   ((not (eq status 'completed))
    (message "Compaction failed (%s): %s" status response))
   ((not (and (stringp response)
              (not (string-empty-p (string-trim response)))))
    (message "Compaction produced empty summary."))
   (t
    (superchat-compact--apply-summary
     buffer session-id captured-length messages response))))

(defun superchat-compact-session ()
  "Compact the session's conversation history into an anchor asynchronously.
Submits a background Runtime Run; on completion an `anchor' row reaches
the SQLite tape and the in-memory history collapses onto the anchor,
keeping messages recorded while the Run was in flight.  Returns the Run
handle, or nil when there is nothing to compact or one is pending."
  (interactive)
  (let* ((messages (superchat-compact--collect-messages))
         (session-id (superchat-compact--ensure-session-id))
         (buffer (current-buffer)))
    (cond
     ((null messages)
      (message "No messages to compact.")
      nil)
     (superchat-compact--in-flight
      (message "Compaction already in progress.")
      nil)
     (t
      (let* ((captured-length (length superchat--conversation-history))
             (prompt (superchat-compact--build-compaction-prompt messages))
             (request
              (superchat-compact--request
               prompt session-id
               (lambda (response status)
                 (superchat-compact--finish
                  buffer session-id captured-length messages
                  response status)))))
        (setq superchat-compact--in-flight t)
        (message "Compacting session %s in the background..." session-id)
        (condition-case err
            (superchat-runtime-submit request)
          (error
           (setq superchat-compact--in-flight nil)
           (message "Compaction failed: %s" (error-message-string err))
           nil)))))))

;; ═══════════════════════════════════════════════════════════
;; Slash commands
;; ═══════════════════════════════════════════════════════════

;;;###autoload
(defun superchat--cmd-compact (_cmd _args _input _lang _target-model)
  "Start compacting the current session into an anchor summary."
  (if (superchat-compact-session)
      '(:type :echo :content "Session compaction started in the background.")
    '(:type :echo :content "No compaction started.")))

;;;###autoload
(defun superchat--cmd-expand (_cmd _args _input _lang _target-model)
  "Expand the latest anchor back into the in-memory conversation history."
  (let ((count (superchat-compact--expand-anchor)))
    (if count
        `(:type :echo :content ,(format "Expanded anchor into %d entries." count))
      '(:type :echo :content "No anchor found to expand."))))

(provide 'superchat-compact)

;;; superchat-compact.el ends here
