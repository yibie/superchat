;;; superchat-core.el — Turn struct + pipeline runner for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines the turn lifecycle and hook protocols.  Everything is a hook:
;; add your function to the right defvar list and the pipeline runs it.
;;
;; Pipeline: parse → system-prompt → build-prompt → post-turn
;;
;; Hook signatures:
;;   system-prompt-functions:  (turn) → modified-turn or nil
;;   build-prompt-functions:   (turn) → modified-turn or nil
;;   post-turn-functions:      (turn) → nil (side-effect only)

;;; Code:

(require 'cl-lib)

;; ═══════════════════════════════════════════════════════════
;; Turn struct
;; ═══════════════════════════════════════════════════════════

(cl-defstruct (superchat-turn
               (:constructor superchat-turn--create)
               (:copier superchat-turn-copy))
  (id nil :read-only t)
  (session-id nil :read-only t)
  (inbound "")
  (clean-input "")
  (target-model nil)
  (skill nil)
  (preset nil)
  (command nil)
  (command-args "")
  (context-files nil)
  (system-prompt "")
  (prompt "")
  (tools nil)
  (retrieved-memories nil)
  (streaming-parts nil)
  (llm-result "")
  (error nil)
  (state nil))

(defun superchat-turn-new (inbound &optional session-id)
  "Create a fresh turn for INBOUND text, with optional SESSION-ID."
  (superchat-turn--create
   :id (format "%d-%x" (time-convert nil 'integer) (random #xffff))
   :session-id (or session-id "default")
   :inbound (string-trim (or inbound ""))
   :clean-input (string-trim (or inbound ""))
   :state nil))

;; ═══════════════════════════════════════════════════════════
;; Hooks — just add-hook on these defvar lists
;; ═══════════════════════════════════════════════════════════

(defvar superchat-system-prompt-functions nil
  "Hook: (turn) → modified-turn or nil.  Builds the system prompt.")

(defvar superchat-build-prompt-functions nil
  "Hook: (turn) → modified-turn or nil.  Builds the final prompt.")

(defvar superchat-post-turn-functions nil
  "Hook: (turn) → nil.  Side-effects: tape recording, etc.")

;; Topic lifecycle hooks (tape.systems semantics)
(defvar superchat-pre-topic-functions nil
  "Hook: (turn) → modified-turn or nil.  Runs before a new topic/turn.")
(defvar superchat-post-topic-functions nil
  "Hook: (turn) → modified-turn or nil.  Runs after prompt is built.")
(defvar superchat-pre-topic-finalized-functions nil
  "Hook: (turn) → modified-turn or nil.  Runs before finalizing a topic.")
(defvar superchat-post-topic-finalized-functions nil
  "Hook: (turn) → nil.  Runs after a topic is finalized and rendered.")

(defvar superchat-command-hooks nil
  "Hook: (command args input lang target-model) → result-plist or nil.
First non-nil return wins.  For slash-command dispatch.")

;; ═══════════════════════════════════════════════════════════
;; Pipeline runner
;; ═══════════════════════════════════════════════════════════

(defun superchat-core--run-hook-chain (turn hook-var)
  "Run each function in HOOK-VAR with TURN, accumulating changes."
  (let ((current turn))
    (dolist (fn (symbol-value hook-var))
      (when (functionp fn)
        (condition-case err
            (when-let ((modified (funcall fn (superchat-turn-copy current))))
              (when (superchat-turn-p modified)
                (setq current modified)))
          (error
           (message "superchat-core: hook %S failed: %s"
                    fn (error-message-string err))))))
    current))

(defun superchat-core--parse-input (turn)
  "Parse model switch, skill, command, and file refs from TURN's inbound."
  (let ((input (superchat-turn-inbound turn)))
    ;; @model
    (when-let ((parsed (and (fboundp 'superchat-parser-model-switch)
                            (superchat-parser-model-switch input))))
      (setf (superchat-turn-clean-input turn) (car parsed))
      (setf (superchat-turn-target-model turn) (cdr parsed)))
    ;; >>workflow — dedicated workflow prefix: `>' invokes skills,
    ;; `>>' invokes .workflow recipes.  Rewrites to the reserved
    ;; `workflow' skill token so the dispatcher's workflow branch
    ;; handles it: ">>research foo" ≡ ">workflow research foo".
    (let ((ci (superchat-turn-clean-input turn)))
      (when (and (stringp ci)
                 (string-match "\\`>>\\s-*\\([a-zA-Z0-9_-]+\\)?\\s-*" ci))
        (setf (superchat-turn-skill turn) "workflow")
        (setf (superchat-turn-clean-input turn)
              (string-trim
               (concat (or (match-string 1 ci) "") " "
                       (substring ci (match-end 0)))))))
    ;; >skill
    (unless (superchat-turn-skill turn)
      (when-let ((parsed (and (fboundp 'superchat-skills-parse-input)
                              (superchat-skills-parse-input
                               (superchat-turn-clean-input turn)))))
        (setf (superchat-turn-skill turn) (car parsed))
        (setf (superchat-turn-clean-input turn)
              (replace-regexp-in-string
               (concat ">" (regexp-quote (car parsed)) "\\s-*")
               "" (superchat-turn-clean-input turn)))))
    ;; /command
    (when-let ((parsed (and (fboundp 'superchat-parser-command)
                            (superchat-parser-command
                             (superchat-turn-clean-input turn)))))
      (setf (superchat-turn-command turn) (car parsed))
      (setf (superchat-turn-command-args turn) (or (cdr parsed) "")))
    ;; #file
    (when (fboundp 'superchat--extract-file-paths)
      (ignore-errors
        (setf (superchat-turn-context-files turn)
              (superchat--extract-file-paths
               (superchat-turn-inbound turn))))))
  turn)

(defun superchat-core-run-turn (turn)
  "Execute TURN through the parse and hook pipeline.
Parses @model, >skill, /command, #file from turn.inbound, then
runs system-prompt and build-prompt hooks to populate turn.prompt
and turn.system-prompt.  Request-time decisions such as tool
collection and backend invocation stay in the dispatcher so plain
chat does not pay agent/tool overhead before the first token."
  (let ((current (superchat-turn-copy turn)))
    (superchat-core--parse-input current)
    (setq current (superchat-core--run-hook-chain
                   current 'superchat-system-prompt-functions))
    (setq current (superchat-core--run-hook-chain
                   current 'superchat-build-prompt-functions))
    ;; Topic-finalized lifecycle hooks (run around post-turn hooks).
    (dolist (fn superchat-pre-topic-finalized-functions)
      (when (functionp fn)
        (ignore-errors (funcall fn current))))
    ;; Post-turn hooks
    (dolist (fn superchat-post-turn-functions)
      (when (functionp fn)
        (ignore-errors (funcall fn current))))
    (dolist (fn superchat-post-topic-finalized-functions)
      (when (functionp fn)
        (ignore-errors (funcall fn current))))
    current))

(provide 'superchat-core)

;;; superchat-core.el ends here
