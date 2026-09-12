;;; superchat-core.el — Turn struct + pipeline runner for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines the turn lifecycle and hook protocols.  Everything is a hook:
;; add your function to the right defvar list and the pipeline runs it.
;;
;; Preparation: parse → system-prompt → build-prompt
;;
;; Hook signatures:
;;   system-prompt-functions:  (turn) → modified-turn or nil
;;   build-prompt-functions:   (turn) → modified-turn or nil

;;; Code:

(require 'cl-lib)
(require 'superchat-prompt)

(declare-function superchat-prompt-collect "superchat-prompt-hooks" (turn))
(declare-function superchat--llm-context-budget-chars
                  "superchat-llm" (&optional system-prompt turn-prompt target-model))
(defvar superchat-context-max-chars)

;; ═══════════════════════════════════════════════════════════
;; Turn struct
;; ═══════════════════════════════════════════════════════════

(cl-defstruct (superchat-turn
               (:constructor superchat-turn--create)
               (:copier superchat-turn-copy))
  (id nil :read-only t)
  (session-id nil :read-only t)
  (origin nil)
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

;; Topic lifecycle hooks (tape.systems semantics)
(defvar superchat-pre-topic-functions nil
  "Hook: (turn) → modified-turn or nil.  Runs before a new topic/turn.")
(defvar superchat-post-topic-functions nil
  "Hook: (turn) → modified-turn or nil.  Runs after prompt is built.")

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
            (when-let* ((modified (funcall fn (superchat-turn-copy current))))
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
    (when-let* ((parsed (and (fboundp 'superchat-parser-model-switch)
                            (superchat-parser-model-switch input))))
      (setf (superchat-turn-clean-input turn) (car parsed))
      (setf (superchat-turn-target-model turn) (cdr parsed)))
    ;; >>workflow — dedicated workflow prefix: `>' invokes skills,
    ;; `>>' invokes .workflow recipes.  Rewrites to the reserved
    ;; `workflow' skill token so the dispatcher's workflow branch
    ;; handles it: ">>research foo" ≡ ">workflow research foo".
    (let ((ci (superchat-turn-clean-input turn)))
      (when (and (stringp ci)
                 (string-match "\\`>>\\s-*" ci))
        (setf (superchat-turn-skill turn) "workflow")
        (setf (superchat-turn-clean-input turn)
              (string-trim (substring ci (match-end 0))))))
    ;; >skill
    (unless (superchat-turn-skill turn)
      (when-let* ((parsed (and (fboundp 'superchat-skills-parse-input)
                              (superchat-skills-parse-input
                               (superchat-turn-clean-input turn)))))
        (setf (superchat-turn-skill turn) (car parsed))
        (setf (superchat-turn-clean-input turn)
              (replace-regexp-in-string
               (concat ">" (regexp-quote (car parsed)) "\\s-*")
               "" (superchat-turn-clean-input turn)))))
    ;; /command
    (unless (superchat-turn-skill turn)
      (when-let* ((parsed (and (fboundp 'superchat-parser-command)
                              (superchat-parser-command
                               (superchat-turn-clean-input turn)))))
        (setf (superchat-turn-command turn) (car parsed))
        (setf (superchat-turn-command-args turn) (or (cdr parsed) ""))))
    ;; #file
    (when (fboundp 'superchat--extract-file-path)
      (when-let* ((file (ignore-errors
                          (superchat--extract-file-path
                           (superchat-turn-clean-input turn)))))
        (setf (superchat-turn-context-files turn) (list file))))
  turn))

(defun superchat-core-prepare-turn (turn)
  "Prepare TURN through parsing and the collect/assemble prompt pipeline.
Parses @model, >skill, /command, #file from turn.inbound, then collects
prompt fragments through the `collect-context' stage (contract §4.3) and
assembles them once against the model's real character budget, setting
`prompt' and `system-prompt' on the turn.  When the hard fragments alone
exceed the budget, sets `error' to `prompt-over-budget' and leaves
`prompt' empty instead of signalling past this function; callers decide
how to surface that.  Post-turn hooks are deliberately deferred so
callers can inspect the prepared request without recording or sending it."
  (let ((current (superchat-turn-copy turn)))
    (superchat-core--parse-input current)
    (let* ((fragments (superchat-prompt-collect current))
           ;; The whole-prompt budget stays window-derived even when the
           ;; user has set `superchat-context-max-chars': that knob's
           ;; documented meaning (superchat-llm.el) is "recent
           ;; conversation" only, and the `history' collector alone
           ;; enforces it (its `:refit' caps against it directly).
           ;; Letting it also shrink the whole prompt here would turn a
           ;; deliberately small history budget into spurious
           ;; `prompt-over-budget' failures once system/tool blurbs and
           ;; attachments are added in.
           (budget (let ((superchat-context-max-chars nil))
                     (superchat--llm-context-budget-chars
                      nil nil (superchat-turn-target-model current)))))
      (condition-case nil
          (let ((result (superchat-stage-call 'assemble-prompt fragments budget)))
            (setf (superchat-turn-system-prompt current)
                  (or (plist-get result :system) ""))
            (setf (superchat-turn-prompt current)
                  (or (plist-get (car (plist-get result :messages)) :content) "")))
        (superchat-prompt-over-budget
         (setf (superchat-turn-error current) 'prompt-over-budget)
         (setf (superchat-turn-prompt current) ""))))
    current))

(provide 'superchat-core)

;;; superchat-core.el ends here
