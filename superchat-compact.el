;;; superchat-compact.el --- Session compaction for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Implements session compaction following the tape.systems context
;; architecture: long conversations are summarized into an `anchor'
;; row on the tape, and subsequent prompts start from that anchor.

;;; Code:

(require 'cl-lib)

;; External declarations
(declare-function superchat--recent-conversation-messages "superchat" (&optional message-limit char-limit))
(declare-function superchat--format-conversation-message "superchat" (message))
(declare-function superchat--llm-generate-answer-sync "superchat-llm" (prompt &optional target-model tools agent-mode))
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

(defun superchat-compact-session ()
  "Compact the current session's conversation history into an anchor.
Writes an `anchor' row to the SQLite tape and updates the in-memory
history to contain only the anchor as a system message.  Returns the
anchor content, or nil if there is nothing to compact."
  (interactive)
  (let* ((messages (superchat-compact--collect-messages))
         (session-id (superchat-compact--ensure-session-id))
         prompt summary metadata)
    (if (null messages)
        (progn
          (message "No messages to compact.")
          nil)
      (message "Compacting session %s..." session-id)
      (setq prompt (superchat-compact--build-compaction-prompt messages))
      (setq summary (condition-case err
                        (superchat--llm-generate-answer-sync prompt)
                      (error
                       (message "Compaction failed: %s" (error-message-string err))
                       nil)))
      (if (not (and summary (not (string-empty-p (string-trim summary)))))
          (progn
            (message "Compaction produced empty summary.")
            nil)
        (setq metadata `(:source_ids ,(mapcar (lambda (m) (or (plist-get m :id) :unknown))
                                               messages)
                          :message_count ,(length messages)
                          :timestamp ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (superchat-db-tape-append session-id "anchor" summary :meta metadata)
        (setq superchat--conversation-history
              (list `(:role system :content ,(concat "[Session anchor]\n\n" summary)
                            :anchor t :timestamp ,(current-time))))
        (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
          (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (superchat--insert-system-message
               (format "Session compacted. Anchor: %s"
                       (substring (string-trim summary) 0
                                  (min 200 (length (string-trim summary)))))))))
        (message "Session compacted into anchor.")
        summary))))

(provide 'superchat-compact)

;;; superchat-compact.el ends here
