;;; superchat-compact.el --- Session compaction for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Implements session compaction following the tape.systems context
;; architecture: long conversations are summarized into an `anchor'
;; row on the tape, and subsequent prompts start from that anchor.

;;; Code:

(require 'cl-lib)
(require 'superchat-runtime)
(require 'superchat-tape)
(require 'superchat-tape-view)

;; External declarations
(declare-function superchat--recent-conversation-messages "superchat" (&optional message-limit char-limit))
(declare-function superchat--format-conversation-message "superchat" (message))
(declare-function superchat-runtime-submit "superchat-runtime" (request))
(declare-function superchat-view-entries "superchat-tape-view"
                  (session-id &optional since-id kinds limit))
(declare-function superchat-view-compaction-anchor-latest
                  "superchat-tape-view" (session-id))
(declare-function superchat-view-compaction-anchor-source-ids
                  "superchat-tape-view" (row))
(declare-function superchat-view-expand-anchor "superchat-tape-view" (session-id anchor-row))
(declare-function superchat-view-assemble "superchat-tape-view"
                  (session-id &rest keys))
(declare-function superchat--insert-system-message "superchat-render" (content))

(defvar superchat--session-id nil)
(defvar superchat--conversation-history nil)
(defvar superchat-buffer-name nil)
(defvar superchat-context-message-count nil)
(defvar superchat-context-max-chars nil)
(defvar-local superchat--expanded-compaction-anchor-ids nil)

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

;; ═══════════════════════════════════════════════════════════
;; Anchor expansion
;; ═══════════════════════════════════════════════════════════

(defun superchat-compact--expand-anchor (&optional session-id)
  "Expand the latest anchor for SESSION-ID back into in-memory history.
Replaces its synthetic system message with exact source entries recovered
from Tape.  Newer raw facts stay in place; repeating the operation is a
no-op."
  (let* ((sid (or session-id
                  (and (boundp 'superchat--session-id) superchat--session-id)))
         (anchor (when sid (superchat-view-compaction-anchor-latest sid))))
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
        (when entries
          (let* ((anchor-id (nth 0 anchor))
                 (anchor-position
                  (cl-position-if
                   (lambda (message)
                     (and (plist-get message :anchor)
                          (= (or (plist-get message :id) -1) anchor-id)))
                   superchat--conversation-history))
                 (anchor-present (integerp anchor-position))
                 (history
                  (if anchor-present
                      (cl-remove-if
                       (lambda (message)
                         (and (plist-get message :anchor)
                              (= (or (plist-get message :id) -1) anchor-id)))
                       superchat--conversation-history)
                    superchat--conversation-history))
                 (known-ids
                  (delete-dups
                   (delq nil (mapcar (lambda (message) (plist-get message :id))
                                     history))))
                 (new-messages
                  (cl-remove-if
                   (lambda (message)
                     (member (plist-get message :id) known-ids))
                   (reverse messages)))
                 (new-history
                  (if anchor-present
                      (mapcan
                       (lambda (message)
                         (if (and (plist-get message :anchor)
                                  (= (or (plist-get message :id) -1)
                                     anchor-id))
                             (copy-sequence new-messages)
                           (list message)))
                       superchat--conversation-history)
                    (append new-messages superchat--conversation-history))))
            (if (and (not anchor-present)
                     (= (length new-messages) 0))
                nil
              (setq superchat--conversation-history new-history)
              (setq superchat--expanded-compaction-anchor-ids
                    (cons anchor-id
                          (delete-dups
                           (copy-sequence
                            superchat--expanded-compaction-anchor-ids))))
              (when (> (length new-messages) 0)
                (message "Expanded anchor into %d entries." (length new-messages)))
              (and (> (length new-messages) 0) (length new-messages)))))))))

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

;; Token-aware recovery below owns automatic pre-send compaction.  Keep the
;; character heuristic callable for compatibility, but do not race it against
;; the same-Run recovery path by registering it on the pre-submit hook.

(defun superchat-compact--context-prompt (session-id)
  "Return SESSION-ID's current bounded Context View prompt."
  (plist-get
   (superchat-view-assemble
    session-id
    :max-entries (and (boundp 'superchat-context-message-count)
                      superchat-context-message-count)
    :max-chars (and (boundp 'superchat-context-max-chars)
                    superchat-context-max-chars)
    :expanded-anchor-ids
    (vconcat superchat--expanded-compaction-anchor-ids))
   :prompt))

(defun superchat-handoff--context-budget-exceeded (details)
  "Compact after a context rejection and resume the same Run once."
  (let ((resume (plist-get details :resume-function))
        (fail (plist-get details :fail-function)))
    (when (and superchat-handoff-auto-enabled
               (functionp resume)
               (functionp fail)
               (boundp 'superchat--session-id)
               superchat--session-id
               (not (bound-and-true-p superchat-compact--in-flight)))
      (let ((session-id superchat--session-id)
            (buffer (current-buffer)))
        (when (superchat-compact-session
               (lambda (success value)
                 (if (and success
                          (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (equal session-id superchat--session-id)))
                     (with-current-buffer buffer
                       (let ((context (superchat-compact--context-prompt
                                       session-id)))
                         (if (and (stringp context)
                                  (not (string-empty-p context)))
                             (funcall resume context)
                           (funcall fail
                                    "[Compaction produced no usable Context View]"))))
                   (funcall fail
                            (if success
                                "[Session changed during context recovery]"
                              value))))
               (plist-get details :model))
          (message "Context limit reached; compacting, then this Run will resume once.")
          t)))))

(add-hook 'superchat-llm-context-budget-exceeded-functions
          #'superchat-handoff--context-budget-exceeded)

;; ═══════════════════════════════════════════════════════════
;; Compaction
;; ═══════════════════════════════════════════════════════════

(defconst superchat-compact--completion-marker
  "[[SUPERCHAT-COMPACTION-COMPLETE]]")

(defun superchat-compact--parse-completed-summary (response)
  "Return RESPONSE without its completion marker, or nil when incomplete."
  (when (and (stringp response)
             (not (string-empty-p (string-trim response))))
    (let* ((lines (split-string (string-trim-right response) "\n"))
           (last-line (car (last lines)))
           (summary (string-trim-right
                     (mapconcat #'identity (butlast lines) "\n"))))
      (when (and (string= (string-trim last-line)
                         superchat-compact--completion-marker)
                 (not (string-empty-p (string-trim summary))))
        summary))))

(defun superchat-compact--collect-messages ()
  "Collect recent durable raw messages after the latest typed Anchor.
The returned plists carry the Tape row ID used as Anchor provenance."
  (let* ((session-id (and (boundp 'superchat--session-id)
                          superchat--session-id))
         (anchor (and session-id
                      (superchat-view-compaction-anchor-latest session-id)))
         (after-id (and anchor (nth 0 anchor)))
         (rows (and session-id (superchat-view-entries session-id after-id)))
         (rows (cl-remove-if-not
                (lambda (row)
                  (member (nth 2 row) '("user" "assistant")))
                rows))
         (count superchat-compact-message-count)
         (rows (if (and count (> count 0) (> (length rows) count))
                   (last rows count)
                 rows)))
    (mapcar
     (lambda (row)
       (list :role (intern (nth 2 row))
             :content (nth 3 row)
             :id (nth 0 row)))
     rows)))

(defun superchat-compact--build-compaction-prompt
    (messages &optional previous-summary)
  "Build the full prompt sent to the LLM for compaction.
MESSAGES is a list of message plists.  PREVIOUS-SUMMARY is folded into a
new checkpoint instead of being silently discarded on repeated compaction."
  (let* ((conversation-text
          (mapconcat
           (lambda (msg)
             (let ((role (plist-get msg :role))
                   (content (string-trim (or (plist-get msg :content) ""))))
               (when (and content (not (string-empty-p content)))
                 (format "%s: %s"
                         (capitalize (if (symbolp role)
                                         (symbol-name role)
                                       role))
                         content))))
           messages
           "\n\n"))
         (has-previous
          (and (stringp previous-summary)
               (not (string-empty-p (string-trim previous-summary))))))
    (format "%s\n\n%s%s%s\n\nEnd the summary with this exact final line and no text after it:\n%s"
            (if (functionp superchat-compact-prompt)
                (funcall superchat-compact-prompt)
              superchat-compact-prompt)
            (if has-previous
                (format (concat
                         "Update the existing checkpoint below with the new "
                         "conversation. Preserve still-relevant goals, "
                         "constraints, decisions, progress, and next steps.\n\n"
                         "<previous-summary>\n%s\n</previous-summary>\n\n"
                         "<new-conversation>\n")
                        previous-summary)
              "<conversation>\n")
            conversation-text
            (if has-previous
                "\n</new-conversation>"
              "\n</conversation>")
            superchat-compact--completion-marker)))

(defvar-local superchat-compact--in-flight nil
  "Non-nil while an asynchronous compaction Run is pending for this buffer.")

(defun superchat-compact--anchor-message (anchor-id summary source-ids)
  "Return the in-memory typed Anchor message for SUMMARY."
  `(:role system
    :content ,(concat "[Session anchor]\n\n" summary)
    :anchor t
    :compaction-anchor t
    :id ,anchor-id
    :source-ids ,source-ids
    :timestamp ,(current-time)))

(defun superchat-compact--runtime-session-id (session-id)
  "Return an isolated Runtime Session ID for a compaction of SESSION-ID."
  (format "%s/compaction-%s" session-id
          (substring (md5 (format "%s:%s:%s"
                                  session-id (float-time) (random))) 0 12)))

(defun superchat-compact--request
    (prompt session-id on-finish &optional target-model)
  "Build the compaction Runtime Request for PROMPT from SESSION-ID.
ON-FINISH receives (RESPONSE STATUS) at any terminal Run state.
TARGET-MODEL keeps automatic compaction on the model used by the blocked Run."
  (append
   (list :input "Compact current session"
         :clean-input "Compact current session"
         :prompt prompt
         :session-id (superchat-compact--runtime-session-id session-id)
         :actor-id "compactor"
         :capability "compaction"
         :topology 'single
         :authority-policy "observer"
         :surface 'compaction
         :origin `(:surface compaction :kind session-compaction
                   :source-session-id ,session-id)
         :record-conversation nil)
   (when target-model (list :target-model target-model))
   (list :stream-function #'ignore
         :buffer-name superchat-buffer-name
         :delivery-function
         (lambda (response status _tape-id)
           (funcall on-finish response status)))))

(defun superchat-compact--apply-summary
    (buffer session-id messages previous-anchor summary)
  "Persist SUMMARY as the anchor and collapse BUFFER's history onto it.
MESSAGES carry the exact durable source IDs.  Newer entries recorded while
the Run was in flight are preserved by ID in front of the new Anchor."
  (let* ((source-ids
          (vconcat
           (delete-dups
            (append (and previous-anchor
                         (append
                          (superchat-view-compaction-anchor-source-ids
                           previous-anchor)
                          nil))
                    (mapcar (lambda (message) (plist-get message :id))
                            messages)))))
         (anchor-id
          (superchat-tape-append-compaction-anchor
           session-id summary source-ids :message-count (length source-ids))))
    (if (not (buffer-live-p buffer))
        (message "Session %s compacted; anchor kept on tape." session-id)
      (with-current-buffer buffer
        (if (not (equal session-id
                        (and (boundp 'superchat--session-id)
                             superchat--session-id)))
            (message "Session %s compacted; history changed, anchor kept on tape only."
                     session-id)
          (let* ((source-id-list (append source-ids nil))
                 (max-source-id (apply #'max source-id-list))
                 (history (cl-remove-if
                           (lambda (message)
                             (or (member (plist-get message :id) source-id-list)
                                 (and previous-anchor
                                      (plist-get message :anchor))))
                           superchat--conversation-history))
                 (leading nil)
                 (tail history))
            ;; The newest history prefix can only contain rows recorded after
            ;; the snapshot.  Stable Tape IDs identify that prefix even when
            ;; the history length has been capped or changed in flight.
            (while (and tail
                        (let ((id (plist-get (car tail) :id)))
                          (or (null id) (> id max-source-id))))
              (push (pop tail) leading))
            (setq superchat--conversation-history
                  (append (nreverse leading)
                          (list (superchat-compact--anchor-message
                                 anchor-id summary source-ids))
                          tail)))
          (when (fboundp 'superchat--insert-system-message)
            (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (superchat--insert-system-message
               (format "Session compacted. Anchor: %s"
                       (substring (string-trim summary) 0
                                  (min 200 (length (string-trim summary)))))))))
          (message "Session compacted into anchor."))))))

(defun superchat-compact--finish
    (buffer session-id messages previous-anchor response status
            &optional on-finish)
  "Handle terminal RESPONSE with STATUS for the pending compaction Run."
  (let ((summary (and (eq status 'completed)
                      (superchat-compact--parse-completed-summary response))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq superchat-compact--in-flight nil)))
    (cond
     ((not (eq status 'completed))
      (message "Compaction failed (%s): %s" status response)
      (when (functionp on-finish)
        (funcall on-finish nil response)))
     ((null summary)
      (message "Compaction response incomplete; no Anchor written.")
      (when (functionp on-finish)
        (funcall on-finish nil
                 "[Compaction response was incomplete; no Anchor written]")))
     (t
      (superchat-compact--apply-summary
       buffer session-id messages previous-anchor summary)
      (when (functionp on-finish)
        (funcall on-finish t summary))))))

(defun superchat-compact-session (&optional on-finish target-model)
  "Compact the session's conversation history into an anchor asynchronously.
Submits a background Runtime Run; on completion an `anchor' row reaches
the SQLite tape and the in-memory history collapses onto the anchor,
keeping messages recorded while the Run was in flight.  Returns the Run
handle, or nil when there is nothing to compact or one is pending.
ON-FINISH, when non-nil, receives (SUCCESS VALUE).  TARGET-MODEL is an
optional one-shot model override for the compaction Run."
  (interactive)
  (let* ((session-id (superchat-compact--ensure-session-id))
         (messages (superchat-compact--collect-messages))
         (buffer (current-buffer)))
    (cond
     ((null messages)
      (message "No messages to compact.")
      nil)
     (superchat-compact--in-flight
      (message "Compaction already in progress.")
      nil)
     (t
      (let* ((previous-anchor
              (superchat-view-compaction-anchor-latest session-id))
             (previous-summary (and previous-anchor
                                    (nth 3 previous-anchor)))
             (prompt (superchat-compact--build-compaction-prompt
                      messages previous-summary))
             (request
              (superchat-compact--request
               prompt session-id
               (lambda (response status)
                 (superchat-compact--finish
                  buffer session-id messages previous-anchor
                  response status on-finish))
               target-model)))
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
