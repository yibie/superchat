;;; superchat-compact.el --- Session compaction for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Implements session compaction following the tape.systems context
;; architecture: long conversations are summarized into an `anchor'
;; row on the tape, and subsequent prompts start from that anchor.

;;; Code:

(require 'cl-lib)
(require 'superchat-runtime)
(require 'superchat-llm)
(require 'superchat-tape)
(require 'superchat-tape-view)
(require 'superchat-editor-workspace)
(require 'superchat-pipeline)
(require 'superchat-sink)

;; External declarations
(declare-function superchat--llm-context-budget-chars
                  "superchat-llm"
                  (&optional system-prompt turn-prompt target-model))
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
(declare-function superchat--response-region-insert-persistent
                  "superchat-render"
                  (content &optional owned-response-marker))
(declare-function superchat--session-buffer-name "superchat" (session-id))

(defvar superchat--session-id nil)
(defvar superchat--conversation-history nil)
(defvar superchat-buffer-name nil)
(defvar superchat-context-message-count nil)
(defvar superchat-context-max-chars nil)
(defvar superchat-data-directory nil)
(defvar superchat--current-turn nil)
(defvar superchat--response-start-marker nil)
(defvar-local superchat--expanded-compaction-anchor-ids nil)

;; ═══════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-compact nil
  "Session compaction settings for superchat."
  :group 'superchat)

(defcustom superchat-compact-prompt
  "You are a context summarization assistant. Your task is to read a conversation between a user and an AI assistant, then produce a structured summary following the exact format specified.

Do NOT continue the conversation. Do NOT respond to any questions in the conversation. ONLY output the structured summary."
  "System prompt used to compact a Superchat session into an anchor summary.
Can be a string or a function that returns a string."
  :type '(choice (string :tag "Custom prompt string")
                 (function :tag "Function that returns prompt string"))
  :group 'superchat-compact)

(defcustom superchat-compact-message-count nil
  "Deprecated compatibility option; compaction now uses the Context View budget.
The compaction range is determined only by the previous Anchor and its
keep-recent boundary."
  :type '(choice (const :tag "All messages" nil)
                 (integer :tag "Limit"))
  :group 'superchat-compact)

;; ═══════════════════════════════════════════════════════════
;; Anchor helpers
;; ═══════════════════════════════════════════════════════════

(defun superchat-compact--ensure-session-id ()
  "Return the already-bound `superchat--session-id', if any."
  (and (boundp 'superchat--session-id) superchat--session-id))

(defun superchat-compact--session-buffer (session-id)
  "Return a live buffer safely identified as SESSION-ID's View, or nil.
Prefers SESSION-ID's named View buffer so recovery started from a
callback running in another buffer (a network callback commonly does)
still reaches the real View, but only trusts it once its own
buffer-local `superchat--session-id' actually matches SESSION-ID -- a
same-named buffer that no longer belongs to SESSION-ID must never be
treated as its View.  Falls back to the current buffer only when its
own local Session ID already equals SESSION-ID (interactive use, and
every existing unit fixture, keep working unchanged); a tab/View
buffer reused for a different Session in the meantime no longer
qualifies, so this returns nil instead of silently handing back a
foreign Session's buffer for the caller to compact."
  (when (stringp session-id)
    (let ((named (get-buffer (superchat--session-buffer-name session-id))))
      (cond
       ((and named (buffer-live-p named)
             (with-current-buffer named
               (equal session-id (bound-and-true-p superchat--session-id))))
        named)
       ((equal session-id (bound-and-true-p superchat--session-id))
        (current-buffer))))))

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
                 (already-expanded-p
                  (member anchor-id superchat--expanded-compaction-anchor-ids))
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
            (cond
             (already-expanded-p nil)
             ((and (not anchor-present)
                   (= (length new-messages) 0))
              (setq superchat--expanded-compaction-anchor-ids
                    (cons anchor-id
                          (delete-dups
                           (copy-sequence
                            superchat--expanded-compaction-anchor-ids))))
              0)
             (t
              (setq superchat--conversation-history new-history)
              (setq superchat--expanded-compaction-anchor-ids
                    (cons anchor-id
                          (delete-dups
                           (copy-sequence
                            superchat--expanded-compaction-anchor-ids))))
              (when (> (length new-messages) 0)
                (message "Expanded anchor into %d entries." (length new-messages)))
              (length new-messages)))))))))

;; ═══════════════════════════════════════════════════════════
;; Automatic handoff
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-handoff nil
  "Automatic session handoff settings for superchat."
  :group 'superchat)

(defcustom superchat-handoff-auto-enabled t
  "When non-nil, automatically compact the session on context overflow
and when the prompt exceeds `superchat-handoff-char-threshold'."
  :type 'boolean
  :group 'superchat-handoff)

(defcustom superchat-handoff-char-threshold nil
  "Character threshold for automatic handoff, or nil to derive it.
When nil, automatic handoff starts at 85 percent of the effective Context View
budget.  When `superchat-handoff-auto-enabled' is non-nil and the built prompt
exceeds this threshold, a background compaction Run is started; the current
Turn proceeds unchanged and later Turns start from the Anchor."
  :type '(choice (const :tag "Derive from context budget" nil)
                 (natnum :tag "Characters"))
  :group 'superchat-handoff)

(defun superchat-handoff--prompt-char-count (turn)
  "Return the approximate character count of TURN's prompt."
  (+ (length (or (superchat-turn-prompt turn) ""))
     (length (or (superchat-turn-system-prompt turn) ""))))

(defun superchat-handoff--context-budget (turn)
  "Return TURN's effective Context View character budget."
  (or (plist-get (superchat-turn-state turn) :context-max-chars)
      (superchat--llm-context-budget-chars
       (superchat-turn-system-prompt turn)
       (superchat-turn-prompt turn)
       (superchat-turn-target-model turn))))

(defun superchat-handoff--char-threshold (turn)
  "Return the configured or derived automatic handoff threshold for TURN."
  (or superchat-handoff-char-threshold
      (floor (* 0.85 (superchat-handoff--context-budget turn)))))

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
                (superchat-handoff--char-threshold turn)))
    (message "Prompt exceeds threshold; compacting session in the background...")
    (let ((superchat--current-turn turn))
      (superchat-compact-session)))
  turn)

;; Token-aware recovery below owns automatic pre-send compaction.  Keep the
;; character heuristic callable for compatibility, but do not race it against
;; the same-Run recovery path by registering it on the pre-submit hook.

(defun superchat-compact--context-budget (&optional target-model)
  "Return the current effective context character budget.
Use the prepared turn's recorded budget when no one-shot TARGET-MODEL is
present; recovery compaction with a target model derives its own budget."
  (let* ((turn (and (boundp 'superchat--current-turn)
                    superchat--current-turn))
         (stored (and (not target-model)
                      turn
                      (plist-get (superchat-turn-state turn)
                                 :context-max-chars))))
    (or stored
        (superchat--llm-context-budget-chars
         (and turn (superchat-turn-system-prompt turn))
         (and turn (superchat-turn-prompt turn))
         target-model))))

(defun superchat-compact--context-envelope (session-id)
  "Return SESSION-ID's current bounded Context View envelope."
  (superchat-view-assemble
   session-id
   :max-entries (and (boundp 'superchat-context-message-count)
                     superchat-context-message-count)
   :max-chars (superchat-compact--context-budget)
   :expanded-anchor-ids
   (vconcat superchat--expanded-compaction-anchor-ids)))

(defun superchat-compact--context-prompt (session-id)
  "Return SESSION-ID's current bounded Context View prompt."
  (plist-get (superchat-compact--context-envelope session-id) :prompt))

(defun superchat-compact--context-usable-p (envelope)
  "Return non-nil when ENVELOPE contains substantive context material."
  (let ((items (plist-get envelope :items))
        (disclosure (plist-get envelope :disclosure)))
    (or (and items (> (length items) 0))
        (and disclosure
             (or (> (length (or (plist-get disclosure :covered) [])) 0)
                 (> (length (or (plist-get disclosure :represented) [])) 0))))))

(defun superchat-handoff--context-budget-exceeded (details)
  "Compact after a context rejection and resume the same Run once.
Reads the target Session from DETAILS' `:session-id' (falling back to
the current buffer's) so recovery still works when the current buffer
at call time is not that Session's View -- a network callback commonly
runs in another buffer, not the View that owns the Run."
  (let* ((resume (plist-get details :resume-function))
         (fail (plist-get details :fail-function))
         (session-id (or (plist-get details :session-id)
                          (and (boundp 'superchat--session-id)
                               superchat--session-id)))
         (buffer (and session-id (superchat-compact--session-buffer session-id)))
         (in-flight (and buffer (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (bound-and-true-p superchat-compact--in-flight)))))
    (when (and superchat-handoff-auto-enabled
               (functionp resume)
               (functionp fail)
               session-id
               buffer
               (not in-flight))
      ;; S19-fix-4: the boundary above (S19-fix-2/3) covers only the
      ;; asynchronous ON-FINISH callback once compaction has been
      ;; accepted.  Collecting messages, building the compaction
      ;; request, and submitting it (inside `superchat-compact-session'
      ;; itself) all run synchronously right here, before that callback
      ;; ever exists to catch anything -- a signal from any of that
      ;; previously escaped after `recover-context' had already set
      ;; `context-recovery-attempted'/`context-recovery-active', with
      ;; neither RESUME nor FAIL ever able to run, leaving the original
      ;; Run open (the re-review's exact finding).  Wrap the synchronous
      ;; start in the identical capture-and-resignal shape: `error' is
      ;; swallowed (FAIL is the report); a `quit' still resolves the
      ;; main Run the same way and is then re-signalled once that
      ;; durable work is done.
      (let (start-quit-condition)
        (unwind-protect
            (condition-case start-err
                (when (with-current-buffer buffer
                        (superchat-compact-session
                         (lambda (success value)
                           ;; A signal anywhere in this chain (the
                           ;; recovery-View rebuild included) must still
                           ;; resolve the main Run one way or the other:
                           ;; swallowing it here as an untraced
                           ;; `delivery.failed' on the *compaction* Run
                           ;; would leave the main Run stalled in
                           ;; `running-model' forever.  `error' is
                           ;; swallowed (FAIL is the report); a `quit'
                           ;; still resolves the main Run the same way
                           ;; and is then re-signalled once that durable
                           ;; work is done (contract §3.3's terminal-
                           ;; `quit' rule) -- including a `quit'
                           ;; re-signalled up from `resume''s own
                           ;; recovery handler, caught here like any
                           ;; other signal from this call.
                           (let (quit-condition)
                             (unwind-protect
                                 (condition-case rebuild-err
                                     (if (and success
                                              (buffer-live-p buffer)
                                              (with-current-buffer buffer
                                                (equal session-id superchat--session-id)))
                                         (with-current-buffer buffer
                                           (let* ((envelope (superchat-compact--context-envelope
                                                             session-id))
                                                  (context (plist-get envelope :prompt)))
                                             (if (and (stringp context)
                                                      (not (string-empty-p context))
                                                      (superchat-compact--context-usable-p
                                                       envelope))
                                                 (funcall resume context)
                                               (let ((failure
                                                      "[Compaction produced no usable Context View]"))
                                                 (when (fboundp 'superchat--insert-system-message)
                                                   (superchat--insert-system-message failure))
                                                 (funcall fail failure)))))
                                       (funcall fail
                                                (if success
                                                    "[Session changed during context recovery]"
                                                  value)))
                                   (error
                                    (funcall fail
                                             (format "[Context recovery rebuild failed: %s]"
                                                     (error-message-string rebuild-err))))
                                   (quit
                                    (setq quit-condition rebuild-err)
                                    (funcall fail
                                             (format "[Context recovery rebuild failed: %s]"
                                                     (error-message-string rebuild-err)))))
                               (when quit-condition
                                 (signal (car quit-condition) (cdr quit-condition))))))
                         (plist-get details :model)))
                  (message "Context limit reached; compacting, then this Run will resume once.")
                  t)
              (error
               (funcall fail
                        (format "[Compaction could not start: %s]"
                                (error-message-string start-err)))
               t)
              (quit
               (setq start-quit-condition start-err)
               (funcall fail
                        (format "[Compaction could not start: %s]"
                                (error-message-string start-err)))
               t))
          (when start-quit-condition
            (signal (car start-quit-condition) (cdr start-quit-condition))))))))

;; DEPTH `t' (Emacs's own "append" convention, equivalent to numeric depth
;; 90) is deliberate and must match the other registration site in
;; `superchat.el': `superchat-llm-context-budget-exceeded-functions' is
;; dispatched with `run-hook-with-args-until-success', so a more specific
;; recovery handler registered at the default depth (0) runs before this
;; catch-all default, instead of racing it on `add-hook''s undocumented
;; prepend-by-default order.
(add-hook 'superchat-llm-context-budget-exceeded-functions
          #'superchat-handoff--context-budget-exceeded t)

;; ═══════════════════════════════════════════════════════════
;; Compaction
;; ═══════════════════════════════════════════════════════════

(defconst superchat-compact--completion-marker
  "[[SUPERCHAT-COMPACTION-COMPLETE]]")

(defconst superchat-compact--initial-instructions
  "The messages above are a conversation to summarize. Create a structured context checkpoint summary that another LLM will use to continue the work.

Use this EXACT format:

## Goal
[What is the user trying to accomplish? Can be multiple items if the session covers different tasks.]

## Constraints & Preferences
- [Any constraints, preferences, or requirements mentioned by user]
- [Or \"(none)\" if none were mentioned]

## Progress
### Done
- [x] [Completed tasks/changes]

### In Progress
- [ ] [Current work]

### Blocked
- [Issues preventing progress, if any]

## Key Decisions
- **[Decision]**: [Brief rationale]

## Next Steps
1. [Ordered list of what should happen next]

## Critical Context
- [Any data, examples, or references needed to continue]
- [Or \"(none)\" if not applicable]

Keep each section concise. Preserve exact file paths, function names, and error messages.")

(defconst superchat-compact--incremental-instructions
  "The messages above are NEW conversation messages to incorporate into the existing summary provided in <previous-summary> tags.

Update the existing structured summary with new information. RULES:
- PRESERVE all existing information from the previous summary
- ADD new progress, decisions, and context from the new messages
- UPDATE the Progress section: move items from \"In Progress\" to \"Done\" when completed
- UPDATE \"Next Steps\" based on what was accomplished
- PRESERVE exact file paths, function names, and error messages
- If something is no longer relevant, you may remove it

Use this EXACT format:

## Goal
[Preserve existing goals, add new ones if the task expanded]

## Constraints & Preferences
- [Preserve existing, add new ones discovered]

## Progress
### Done
- [x] [Include previously done items AND newly completed items]

### In Progress
- [ ] [Current work - update based on progress]

### Blocked
- [Current blockers - remove if resolved]

## Key Decisions
- **[Decision]**: [Brief rationale] (preserve all previous, add new)

## Next Steps
1. [Update based on current state]

## Critical Context
- [Preserve important context, add new if needed]

Keep each section concise. Preserve exact file paths, function names, and error messages.")

(defun superchat-compact--parse-completed-summary (response &optional max-chars)
  "Return RESPONSE without its completion marker, or nil when incomplete.
When MAX-CHARS is non-nil, also reject summaries exceeding that character
limit."
  (when (and (stringp response)
             (not (string-empty-p (string-trim response))))
    (let* ((lines (split-string (string-trim-right response) "\n"))
           (last-line (car (last lines)))
           (summary (string-trim-right
                     (mapconcat #'identity (butlast lines) "\n"))))
      (when (and (string= (string-trim last-line)
                         superchat-compact--completion-marker)
                 (not (string-empty-p (string-trim summary)))
                 (or (null max-chars)
                     (<= (length summary) max-chars)))
        summary))))

(defun superchat-compact--message-line (message)
  "Render MESSAGE using the same character accounting as Context View."
  (superchat-view--continuity-item-line
   (list :class 'raw-fact
         :kind (symbol-name (plist-get message :role))
         :content (or (plist-get message :content) ""))))

(defun superchat-compact--keep-recent-messages (rows context-budget)
  "Return the newest raw ROWS fitting thirty percent of CONTEXT-BUDGET.
The newest row is retained even when its rendered line alone exceeds that
portion, so compaction never consumes the latest fact."
  (let ((keep-budget (floor (* context-budget 0.3)))
        (selected nil)
        (selected-chars 0))
    (catch 'keep-recent-stop
      (dolist (row (reverse (copy-sequence rows)))
        (let* ((message (list :role (intern (nth 2 row))
                              :content (nth 3 row)))
               (line-length
                (length (superchat-compact--message-line message)))
               (candidate-chars (+ selected-chars line-length
                                    (if selected 1 0))))
          (if (and selected (> candidate-chars keep-budget))
              (throw 'keep-recent-stop nil)
            (push row selected)
            (setq selected-chars candidate-chars)))))
    selected))

(defun superchat-compact--collect-messages (&optional context-budget)
  "Collect raw messages to summarize after the latest typed Anchor.
The newest thirty percent of CONTEXT-BUDGET remains raw in the Context View;
the returned plists carry the Tape row IDs used as Anchor provenance.
Rows the Session replaced with `turn.superseded' are skipped; a failed
supersession-index read signals, so compaction never summarizes history
that still contains replaced turns."
  (let* ((session-id (and (boundp 'superchat--session-id)
                          superchat--session-id))
         (superseded (and session-id
                          (superchat-view-superseded-storage-ids session-id)))
         (anchor (and session-id
                      (superchat-view-compaction-anchor-latest session-id)))
         (after-id (and anchor (nth 0 anchor)))
         (rows (and session-id (superchat-view-entries session-id after-id)))
         (rows (cl-remove-if-not
                (lambda (row)
                  (and (member (nth 2 row) '("user" "assistant"))
                       (not (memql (nth 0 row) superseded))))
                rows))
         (context-budget (or context-budget
                             (superchat-compact--context-budget)))
         (recent-rows (superchat-compact--keep-recent-messages
                       rows context-budget))
         (compaction-count (- (length rows) (length recent-rows)))
         (rows (if (> compaction-count 0)
                   (cl-subseq rows 0 compaction-count)
                 nil)))
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
           #'identity
           (delq nil
                 (mapcar
                  (lambda (msg)
                    (let ((role (plist-get msg :role))
                          (content (string-trim
                                    (or (plist-get msg :content) ""))))
                      (when (not (string-empty-p content))
                        (format "%s: %s"
                                (capitalize (if (symbolp role)
                                                (symbol-name role)
                                              role))
                                content))))
                  messages))
           "\n\n"))
         (has-previous
          (and (stringp previous-summary)
               (not (string-empty-p (string-trim previous-summary)))))
         (instructions (if has-previous
                           superchat-compact--incremental-instructions
                         superchat-compact--initial-instructions))
         (conversation
          (if has-previous
              (format "<previous-summary>\n%s\n</previous-summary>\n\n<new-conversation>\n%s\n</new-conversation>"
                      previous-summary conversation-text)
            (format "<conversation>\n%s\n</conversation>"
                    conversation-text))))
    (format "%s\n\n%s\n\n%s\n\nEnd the summary with this exact final line and no text after it:\n%s"
            (if (functionp superchat-compact-prompt)
                (funcall superchat-compact-prompt)
              superchat-compact-prompt)
            instructions
            conversation
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

(defun superchat-compact--summary-char-limit (&optional target-model)
  "Return the maximum summary characters for TARGET-MODEL's context budget."
  (floor (* 0.5 (superchat-compact--context-budget target-model))))

(defun superchat-compact--summary-max-tokens (summary-char-limit)
  "Return the output-token ceiling for SUMMARY-CHAR-LIMIT characters."
  (when (> summary-char-limit 0)
    (max 1
         (floor (/ summary-char-limit
                   (float (if (and (numberp superchat-context-chars-per-token)
                                  (> superchat-context-chars-per-token 0))
                             superchat-context-chars-per-token
                           2)))))))

(defun superchat-compact--log-directory ()
  "Return the human-readable compaction log directory."
  (expand-file-name "compaction/"
                    (or superchat-data-directory user-emacs-directory)))

(defun superchat-compact--log-files (session-id)
  "Return existing Markdown compaction logs for SESSION-ID."
  (let ((directory (superchat-compact--log-directory)))
    (when (and (stringp session-id) (file-directory-p directory))
      (directory-files
       directory t
       (concat "\\`" (regexp-quote session-id) "-[0-9]+\\.md\\'")
       t))))

(defun superchat-compact--log-number (file session-id)
  "Return FILE's numeric sequence for SESSION-ID."
  (when (string-match
         (concat "\\`" (regexp-quote session-id) "-\\([0-9]+\\)\\.md\\'")
         (file-name-nondirectory file))
    (string-to-number (match-string 1 (file-name-nondirectory file)))))

(defun superchat-compact--latest-log (session-id)
  "Return the latest Markdown compaction log for SESSION-ID, or nil."
  (car
   (sort (superchat-compact--log-files session-id)
         (lambda (left right)
           (> (or (superchat-compact--log-number left session-id) 0)
              (or (superchat-compact--log-number right session-id) 0))))))

(defun superchat-compact--write-log (session-id summary)
  "Write SUMMARY as the next human-readable Markdown log for SESSION-ID."
  (let* ((directory (superchat-compact--log-directory))
         (files (superchat-compact--log-files session-id))
         (last-number
          (apply #'
                 max
                 0
                 (delq nil
                       (mapcar (lambda (file)
                                 (superchat-compact--log-number file session-id))
                               files))))
         (file (expand-file-name
                (format "%s-%d.md" session-id (1+ (or last-number 0)))
                directory)))
    (make-directory directory t)
    (with-temp-file file
      (insert summary)
      (unless (bolp) (insert "\n")))
    file))

(defun superchat-compact--request
    (prompt session-id on-finish &optional target-model max-tokens)
  "Build the compaction Request envelope for PROMPT from SESSION-ID (S13b).
ON-FINISH receives (RESPONSE STATUS) at any terminal Run state, exactly once,
through the envelope's callback Sink.  TARGET-MODEL keeps automatic
compaction on the model used by the blocked Run.  MAX-TOKENS limits the
compaction response when non-nil.  The prepared PROMPT rides in `:context
:prepared-prompt'; the child protocol buffer is `:context :host-buffer'."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Compaction Request requires a non-empty session ID"))
  (let* ((runtime-session-id
          (superchat-compact--runtime-session-id session-id))
         (runtime-buffer
          (superchat-editor-workspace-buffer
           runtime-session-id 'compaction-protocol t))
         (runtime-buffer
          (superchat-editor-workspace-validate-buffer
           runtime-buffer runtime-session-id))
         (delivered nil)
         (deliver
          (lambda (response status)
            (unless delivered
              (setq delivered t)
              (unwind-protect
                  (funcall on-finish response status)
                (superchat-compact--close-child-workspace
                 runtime-session-id))))))
    (append
     (list :surface 'compaction
           :content "Compact current session"
           :session-id runtime-session-id
           :capability "compaction"
           :topology 'single
           :authority-policy "observer"
           :tools 'none
           :origin `(:surface compaction :kind session-compaction
                     :source-session-id ,session-id)
           :sink (superchat-sink-callback
                  :id (format "compaction-%08x" (random #x100000000))
                  :stream #'ignore
                  :final (lambda (_run result)
                           (funcall deliver (or (plist-get result :text) "") 'completed))
                  :error (lambda (_run failure)
                           (funcall deliver (or (plist-get failure :text) "")
                                    (plist-get failure :status))))
           :context (list :host-buffer runtime-buffer
                          :prepared-prompt prompt)
           :runtime-extras
           (append (list :clean-input "Compact current session"
                         :actor-id "compactor"
                         :record-conversation nil)
                   (when max-tokens (list :max-tokens max-tokens))))
     (when target-model (list :target-model target-model)))))

(defun superchat-compact--close-child-workspace (session-id)
  "Close operation-scoped compaction SESSION-ID without masking its result.

Retained state remains owned and discoverable by EditorWorkspace.  A close
error or quit is reported, but never replaces the compaction terminal result
or an error already leaving its terminal callback."
  (condition-case condition
      (superchat-session-workspace-close session-id)
    ((error quit)
     (display-warning
      'superchat-compact
      (format "Could not close compaction child workspace %s: %s"
              session-id (error-message-string condition))
      :warning)
     nil)))

(defun superchat-compact--submit-request (request)
  "Submit the compaction envelope REQUEST, closing its child workspace on failure."
  (condition-case condition
      (superchat-pipeline-submit request)
    ((error quit)
     (superchat-compact--close-child-workspace
      (plist-get request :session-id))
     (signal (car condition) (cdr condition)))))

(defun superchat-compact--apply-summary
    (buffer session-id messages previous-anchor summary
            &optional transient-marker)
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
          (when (fboundp 'superchat--response-region-insert-persistent)
            (superchat--response-region-insert-persistent
             (format "Session compacted. Anchor: %s (可 /expand)"
                     anchor-id)
             transient-marker))
          (message "Session compacted into anchor."))))))

(defun superchat-compact--finish
    (buffer session-id messages previous-anchor response status
            &optional on-finish summary-max-chars transient-marker)
  "Handle terminal RESPONSE with STATUS for the pending compaction Run."
  (let* ((raw-summary (and (eq status 'completed)
                           (superchat-compact--parse-completed-summary response)))
         (summary (and raw-summary
                       (if (or (null summary-max-chars)
                               (<= (length raw-summary) summary-max-chars))
                           raw-summary))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq superchat-compact--in-flight nil)))
    (cond
     ((not (eq status 'completed))
      (message "Compaction failed (%s): %s" status response)
      (when (functionp on-finish)
        (funcall on-finish nil response)))
     ((and raw-summary (null summary))
      (message "Compaction summary exceeds %d characters; no Anchor written."
               summary-max-chars)
      (when (functionp on-finish)
        (funcall on-finish nil
                 (format "[Compaction summary exceeded %d characters; no Anchor written]"
                         summary-max-chars))))
     ((null raw-summary)
      (message "Compaction response incomplete; no Anchor written.")
      (when (functionp on-finish)
        (funcall on-finish nil
                 "[Compaction response was incomplete; no Anchor written]")))
     (t
      ;; A signal out of Anchor application (e.g. the injected Tape-append
      ;; failure this guards against) must never escape uncaught: this runs
      ;; from the compaction Run's own delivery callback, so an escaping
      ;; error is merely recorded as that Run's `delivery.failed' while the
      ;; *main* Run's ON-FINISH -- the only thing that can ever resume or
      ;; fail it -- would simply never be called, stalling it forever.
      ;; `error' is swallowed here (ON-FINISH is the report); a `quit' still
      ;; finalizes the main Run the same way and is then re-signalled once
      ;; that durable work is done (contract §3.3's terminal-`quit' rule).
      (let (apply-failure quit-condition)
        (unwind-protect
            (progn
              (condition-case apply-err
                  (progn
                    (superchat-compact--apply-summary
                     buffer session-id messages previous-anchor summary
                     transient-marker)
                    (condition-case err
                        (superchat-compact--write-log session-id summary)
                      (error
                       (message "Could not write compaction log: %s"
                                (error-message-string err)))))
                (error
                 (setq apply-failure (error-message-string apply-err))
                 (message "Compaction anchor application failed: %s" apply-failure))
                (quit
                 (setq apply-failure (error-message-string apply-err)
                       quit-condition apply-err)
                 (message "Compaction anchor application failed: %s" apply-failure)))
              (when (functionp on-finish)
                (if apply-failure
                    (funcall on-finish
                             nil
                             (format "[Compaction anchor application failed: %s]"
                                     apply-failure))
                  (funcall on-finish t summary))))
          (when quit-condition
            (signal (car quit-condition) (cdr quit-condition)))))))))

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
         (context-budget (superchat-compact--context-budget target-model))
         (summary-max-chars (floor (* 0.5 context-budget)))
         (summary-max-tokens
          (superchat-compact--summary-max-tokens summary-max-chars))
         (messages (superchat-compact--collect-messages context-budget))
         (buffer (current-buffer))
         (transient-marker superchat--response-start-marker))
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
                  response status on-finish summary-max-chars transient-marker))
               target-model summary-max-tokens)))
        (setq superchat-compact--in-flight t)
        (message "Compacting session %s in the background..." session-id)
        (let (accepted)
          (unwind-protect
              (condition-case err
                  (prog1 (superchat-compact--submit-request request)
                    (setq accepted t))
                (error
                 (message "Compaction failed: %s" (error-message-string err))
                 nil))
            (unless accepted
              (setq superchat-compact--in-flight nil)))))))))

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
    (cond
     ((null count)
      (let* ((session-id (and (boundp 'superchat--session-id)
                              superchat--session-id))
             (anchor (and session-id
                          (superchat-view-compaction-anchor-latest
                           session-id))))
        (if (and anchor
                 (member (nth 0 anchor)
                         superchat--expanded-compaction-anchor-ids))
            '(:type :echo
              :content "Anchor already expanded; Context View uses raw entries.")
          '(:type :echo :content "No anchor found to expand."))))
     ((zerop count)
      '(:type :echo :content "Anchor already expanded; Context View uses raw entries."))
     (t
      `(:type :echo :content ,(format "Expanded anchor into %d entries." count))))))

;;;###autoload
(defun superchat--cmd-compact-log (_cmd args _input _lang _target-model)
  "Open a numbered compaction log for the current session.
With no ARGS, open the latest log.  A positive integer selects that sequence."
  (let* ((argument (string-trim (or args "")))
         (number (and (string-match-p "\\`[1-9][0-9]*\\'" argument)
                      (string-to-number argument)))
         (session-id (and (boundp 'superchat--session-id)
                          superchat--session-id))
         (file
          (cond
           ((string-empty-p argument)
            (and session-id (superchat-compact--latest-log session-id)))
           (number
            (cl-find-if
             (lambda (candidate)
               (= number (superchat-compact--log-number candidate session-id)))
             (and session-id (superchat-compact--log-files session-id)))))))
    (cond
     (file
      (find-file file)
      '(:type :noop))
     ((string-empty-p argument)
      '(:type :echo :content "No compaction log found for this session."))
     (number
      `(:type :echo :content
              ,(format "No compaction log %d found for this session." number)))
     (t
      '(:type :echo :content "Usage: /compact-log [positive log number]")))))

(provide 'superchat-compact)

;;; superchat-compact.el ends here
