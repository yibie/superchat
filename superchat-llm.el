;;; superchat-llm.el --- LLM backend abstraction for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM provider abstraction, tool collection, prompt building, and
;; streaming answer generation.  Extracted from superchat.el monolith
;; (v0.9 split step 5).

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
;; The model port stays UI-free: streaming sinks arrive through the
;; Runtime request, and MCP tools are picked up behind `fboundp' when the
;; optional bridge is loaded.
(require 'superchat-preset)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-streaming)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-tools-enabled)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-unknown-tool-retries)
(defvar superchat-llm-round-limit-action)
(defvar superchat-response-timeout)
(defvar superchat-tool-timeout-multiplier)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat--ttft-start-time)
(defvar superchat-buffer-name)
(defvar superchat--active-timeout-timer)
(defvar superchat--conversation-history)
;; Per-run transport bindings owned by superchat-runtime.el.
(defvar superchat--llm-runtime-observer)
(defvar superchat--llm-evidence-context nil)
(defvar superchat--llm-raw-tool-uses
  (make-hash-table :test #'eq :weakness 'key)
  "Eligible prompts awaiting provider tool-use capture before normalization.")
(defvar superchat--llm-callback-attempt nil
  "Provider-attempt ordinal bound around one transport callback.")
(defvar superchat--llm-tool-origin nil
  "Immutable evidence origin bound while llm.el invokes one tool call.")
(defvar superchat--llm-tools-prepared-p)
(defvar superchat--llm-prepared-tools)
(defvar superchat--llm-tool-wrapper)
(defvar superchat--llm-round-limit-function)
(defvar superchat--llm-unknown-tool-retry-limit)
(defvar superchat--llm-persist-synthesized-tools)
(defvar superchat--llm-run-active-p)
(defvar superchat--llm-ollama-response-p nil
  "Non-nil while starting a Superchat request through an Ollama provider.")
(declare-function superchat--ttft-log "superchat" (label))
(declare-function superchat--annotate-ttft "superchat-render" (elapsed))
(declare-function llm-ollama-p "llm-ollama" (object))
(declare-function superchat--detect-response-mode "superchat" (&optional tools tools-known-p))
(declare-function superchat--show-response-mode-indicator "superchat" (&optional mode))
(declare-function superchat--get-adjusted-timeout "superchat" (&optional mode))
(declare-function superchat-get-llm-tools "superchat-tools" (&optional tool-names))
(declare-function superchat-mcp-get-tools "superchat-mcp" ())
(declare-function llm-capabilities "llm" (provider))
(declare-function llm-chat-async "llm"
                  (provider prompt response-callback error-callback
                            &optional multi-output))
(declare-function llm-chat-streaming "llm"
                  (provider prompt partial-callback response-callback
                            error-callback &optional multi-output))
(declare-function llm-chat-prompt-tools "llm" (prompt))
(declare-function llm-chat-prompt-p "llm" (prompt))
(declare-function llm-chat-prompt-interactions "llm" (prompt))
(declare-function llm-chat-prompt-interaction-role "llm" (interaction))
(declare-function llm-chat-prompt-interaction-content "llm" (interaction))
(declare-function llm-chat-prompt-interaction-tool-results "llm" (interaction))
(declare-function llm-chat-prompt-interaction-multi-turn-plist "llm" (interaction))
(declare-function llm-chat-prompt-tool-result-call-id "llm" (result))
(declare-function llm-chat-prompt-tool-result-tool-name "llm" (result))
(declare-function llm-chat-prompt-tool-result-result "llm" (result))
(declare-function llm-chat-prompt-context "llm" (prompt))
(declare-function llm-chat-prompt-temperature "llm" (prompt))
(declare-function llm-chat-prompt-max-tokens "llm" (prompt))
(declare-function llm-chat-prompt-response-format "llm" (prompt))
(declare-function llm-chat-prompt-reasoning "llm" (prompt))
(declare-function llm-chat-prompt-non-standard-params "llm" (prompt))
(declare-function llm-chat-prompt-tool-options "llm" (prompt))
(declare-function llm-tool-options-tool-choice "llm" (options))
(declare-function llm-provider-chat-request "llm-provider-utils"
                  (provider prompt streaming))
(declare-function llm-provider-utils-tool-use-p "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-id "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-name "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-args "llm-provider-utils" (value))
(declare-function llm-provider-utils--normalize-args
                  "llm-provider-utils" (args &optional false-value))
(declare-function llm-chat-prompt-append-response "llm"
                  (prompt response &optional role))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function make-llm-tool "llm" (&rest args))
(declare-function make-llm-media "llm" (&rest args))
(declare-function llm-make-multipart "llm" (&rest parts))

(define-error 'superchat-llm-evidence-recording-error
  "Superchat could not record required decision evidence")

(defun superchat--llm-canonical-evidence-value (value)
  "Return VALUE with llm.el false sentinels canonicalized for exact JSON."
  (cond
   ((eq value :false) :json-false)
   ((consp value)
    (cons (superchat--llm-canonical-evidence-value (car value))
          (superchat--llm-canonical-evidence-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'superchat--llm-canonical-evidence-value
                     (append value nil))))
   (t value)))

(defun superchat--llm-set-tool-use-args (use value)
  "Set USE's arguments without expanding an optional llm accessor early.

`superchat-llm.el' is loaded before optional llm.el in the canonical test
runner.  Calling the generated accessor's `setf' form directly would then
leave an unresolved `(setf ...)' function in the interpreted definition.
The struct slot API resolves the concrete llm object at call time."
  (setf (cl-struct-slot-value 'llm-provider-utils-tool-use 'args use) value)
  value)

(defun superchat--llm-capture-provider-tool-uses
    (original provider prompt tool-uses)
  "Call ORIGINAL after preserving PROMPT's raw provider TOOL-USES."
  (when-let* ((capture (and (hash-table-p superchat--llm-raw-tool-uses)
                            (gethash prompt superchat--llm-raw-tool-uses))))
    (let ((raw (copy-tree tool-uses t)))
      (dolist (use raw)
        (when (llm-provider-utils-tool-use-p use)
          (superchat--llm-set-tool-use-args
           use
           (superchat--llm-canonical-evidence-value
            (llm-provider-utils-tool-use-args use)))))
      (setcar capture raw))
    (remhash prompt superchat--llm-raw-tool-uses))
  (funcall original provider prompt tool-uses))

(with-eval-after-load 'llm-provider-utils
  (unless (advice-member-p #'superchat--llm-capture-provider-tool-uses
                           'llm-provider-populate-tool-uses)
    (advice-add 'llm-provider-populate-tool-uses :around
                #'superchat--llm-capture-provider-tool-uses)))

(defun superchat--llm-require-accessors (names label)
  "Require every function in NAMES for exact evidence named LABEL."
  (dolist (name names)
    (unless (fboundp name)
      (error "llm.el %s accessor is unavailable: %s" label name))))

(defun superchat--llm-evidence-tool-use (use)
  "Return one llm.el tool USE as structured evidence."
  (superchat--llm-require-accessors
   '(llm-provider-utils-tool-use-p
     llm-provider-utils-tool-use-id
     llm-provider-utils-tool-use-name
     llm-provider-utils-tool-use-args)
   "tool-use")
  (unless (llm-provider-utils-tool-use-p use)
    (error "Unsupported llm.el interaction content: %S" (type-of use)))
  (list :call-id (llm-provider-utils-tool-use-id use)
        :tool-name (llm-provider-utils-tool-use-name use)
        :arguments
        (superchat--llm-canonical-evidence-value
         (copy-tree (llm-provider-utils-tool-use-args use) t))))

(defun superchat--llm-evidence-tool-result (result)
  "Return one llm.el tool RESULT as structured evidence."
  (list :call-id (llm-chat-prompt-tool-result-call-id result)
        :tool-name (llm-chat-prompt-tool-result-tool-name result)
        :result (copy-tree
                 (llm-chat-prompt-tool-result-result result) t)))

(defun superchat--llm-evidence-content (content)
  "Return llm.el interaction CONTENT and its structural format."
  (cond
   ((stringp content) (cons "text" content))
   ((null content) (cons "empty" nil))
   ((listp content)
    (cons "tool-uses"
          (vconcat (mapcar #'superchat--llm-evidence-tool-use content))))
   (t (error "Unsupported llm.el interaction content: %S"
             (type-of content)))))

(defun superchat--llm-evidence-messages (real-prompt)
  "Return every provider-visible interaction slot from REAL-PROMPT."
  (superchat--llm-require-accessors
   '(llm-chat-prompt-interactions
     llm-chat-prompt-interaction-role
     llm-chat-prompt-interaction-content
     llm-chat-prompt-interaction-tool-results
     llm-chat-prompt-interaction-multi-turn-plist
     llm-chat-prompt-tool-result-call-id
     llm-chat-prompt-tool-result-tool-name
     llm-chat-prompt-tool-result-result)
   "prepared-message")
  (vconcat
   (mapcar
    (lambda (interaction)
      (let* ((content
              (superchat--llm-evidence-content
               (llm-chat-prompt-interaction-content interaction)))
             (results
              (llm-chat-prompt-interaction-tool-results interaction)))
        (list
         :role (format "%s"
                       (llm-chat-prompt-interaction-role interaction))
         :content-format (car content)
         :content (cdr content)
         :tool-results
         (vconcat (mapcar #'superchat--llm-evidence-tool-result results))
         :multi-turn
         (copy-tree
          (llm-chat-prompt-interaction-multi-turn-plist interaction) t))))
    (llm-chat-prompt-interactions real-prompt))))

(defun superchat--llm-evidence-tool-records (tools)
  "Return TOOLS' provider-visible schemas as structured evidence."
  (superchat--llm-require-accessors
   '(llm-tool-name llm-tool-description llm-tool-args llm-tool-async)
   "prepared-tool")
  (vconcat
   (mapcar
    (lambda (tool)
      (list :name (llm-tool-name tool)
            :description (llm-tool-description tool)
            :args (vconcat (copy-tree (llm-tool-args tool) t))
            :async (and (llm-tool-async tool) t)))
    tools)))

(defun superchat--llm-validate-evidence-tools (tools)
  "Reject a tool surface that cannot carry exact attempt provenance."
  (condition-case nil
      (superchat--llm-require-accessors
       '(llm-tool-name llm-tool-description llm-tool-args llm-tool-function
         llm-tool-async make-llm-tool llm-provider-utils--normalize-args
         llm-provider-populate-tool-uses
         llm-provider-utils-tool-use-p llm-provider-utils-tool-use-id
         llm-provider-utils-tool-use-name llm-provider-utils-tool-use-args)
       "evidence-tool")
    (error
     (signal 'superchat-llm-evidence-recording-error
             '("Required llm.el evidence-tool accessor is unavailable"))))
  (unless (listp tools)
    (signal 'superchat-llm-evidence-recording-error
            '("epistemic-learning-v0 requires a tool list")))
  (let ((tool-names (make-hash-table :test #'equal)))
    (dolist (tool tools)
      (let ((name (llm-tool-name tool))
            (args (llm-tool-args tool)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (signal 'superchat-llm-evidence-recording-error
                  '("epistemic-learning-v0 requires a named tool")))
        (when (gethash name tool-names)
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Duplicate evidence tool name: %s" name))))
        (puthash name t tool-names)
        (unless (functionp (llm-tool-function tool))
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Evidence tool is not callable: %s" name))))
        (unless (listp args)
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Evidence tool schema is invalid: %s" name))))
        (let ((arg-names (make-hash-table :test #'equal)))
          (dolist (spec args)
            (let ((arg-name (and (listp spec) (plist-get spec :name))))
              (unless (and (stringp arg-name)
                           (not (string-empty-p arg-name))
                           (not (gethash arg-name arg-names))
                           (memq (plist-get spec :type)
                                 '(string integer number boolean array)))
                (signal 'superchat-llm-evidence-recording-error
                        (list (format "Evidence tool argument is invalid: %s"
                                      name))))
              (puthash arg-name t arg-names))))
        (when (or (equal name "define_tool")
                  (and (fboundp 'superchat--syn-tool-p)
                       (superchat--syn-tool-p name))
                  (llm-tool-async tool))
          (signal
           'superchat-llm-evidence-recording-error
           (list
            (format "epistemic-learning-v0 excludes dynamic or async tool: %s"
                    name))))))))

(defun superchat--llm-evidence-tools (prompt)
  "Return PROMPT's provider-visible tool schemas as structured evidence."
  (unless (fboundp 'llm-chat-prompt-tools)
    (error "llm.el prepared-tool accessor is unavailable"))
  (superchat--llm-evidence-tool-records
   (llm-chat-prompt-tools prompt)))

(defun superchat--llm-evidence-options (prompt streaming multi-output)
  "Return actual Superchat-prepared options from PROMPT."
  (superchat--llm-require-accessors
   '(llm-chat-prompt-temperature llm-chat-prompt-max-tokens
     llm-chat-prompt-response-format llm-chat-prompt-reasoning
     llm-chat-prompt-non-standard-params llm-chat-prompt-tool-options
     llm-tool-options-tool-choice)
   "prepared-option")
  (let* ((tool-options (llm-chat-prompt-tool-options prompt))
         (tool-choice
          (and tool-options (llm-tool-options-tool-choice tool-options))))
    (list :temperature (llm-chat-prompt-temperature prompt)
          :max-tokens (llm-chat-prompt-max-tokens prompt)
          :response-format
          (copy-tree (llm-chat-prompt-response-format prompt) t)
          :reasoning (llm-chat-prompt-reasoning prompt)
          :non-standard-params
          (copy-tree (llm-chat-prompt-non-standard-params prompt) t)
          :tool-choice
          (cond ((null tool-choice) nil)
                ((symbolp tool-choice) (symbol-name tool-choice))
                (t tool-choice))
          :streaming (and streaming t)
          :multi-output (and multi-output t))))

(defun superchat--llm-set-prompt-tools (prompt tools)
  "Set PROMPT's TOOLS after llm.el is loaded."
  (setf (cl-struct-slot-value 'llm-chat-prompt 'tools prompt) tools))

(defun superchat--llm-tool-use-call-args (use tool)
  "Return (t . POSITIONAL-ARGS) when USE is valid for TOOL."
  (condition-case nil
      (let* ((arguments
              (copy-tree (llm-provider-utils-tool-use-args use) t))
             (schema (llm-tool-args tool))
             (valid (listp arguments))
             positional)
        (dolist (spec schema)
          (let* ((name (plist-get spec :name))
                 (key (and (stringp name) (intern name)))
                 (pair (and key (assq key arguments)))
                 (value (and pair (cdr pair)))
                 (type (plist-get spec :type)))
            (cond
             ((and pair
                   (pcase type
                     ('string (stringp value))
                     ('integer (integerp value))
                     ('number (numberp value))
                     ('boolean (eq value t))))
              (push value positional))
             ((and pair (eq type 'boolean)
                   (memq value '(:false :json-false)))
              (push nil positional))
             (pair (setq valid nil))
             ((plist-get spec :optional) (push nil positional))
             (t (setq valid nil)))))
        (when valid
          (dolist (argument arguments)
            (unless
                (and
                 (consp argument)
                 (cl-some
                  (lambda (spec)
                    (let ((name (plist-get spec :name)))
                      (and (stringp name)
                           (eq (intern name) (car argument)))))
                  schema))
              (setq valid nil))))
        (when valid
          (cons t (nreverse positional))))
    (error nil)))

(defun superchat--llm-attempt-call-origin
    (prompt tool received-args consumed base-origin &optional raw-uses)
  "Return the unused PROMPT tool use matching TOOL and RECEIVED-ARGS."
  (let* ((interactions (llm-chat-prompt-interactions prompt))
         (interaction (car (last interactions)))
         (uses (or (car-safe raw-uses)
                   (and interaction
                        (llm-chat-prompt-interaction-content interaction))))
         (expected-name (llm-tool-name tool))
         (match
          (and
           (listp uses)
           (cl-loop
            for use in uses
            for ordinal from 0
            for mapped =
            (and (llm-provider-utils-tool-use-p use)
                 (equal expected-name
                        (llm-provider-utils-tool-use-name use))
                 (superchat--llm-tool-use-call-args use tool))
            when (and mapped
                      (not (memq ordinal (car consumed)))
                      (equal received-args (cdr mapped)))
            return (cons ordinal use)))))
    (unless (and (stringp expected-name)
                 (not (string-empty-p expected-name))
                 match)
      (error "Tool call does not match its provider attempt"))
    (let* ((ordinal (car match))
           (use (cdr match))
           (call-id (llm-provider-utils-tool-use-id use)))
      (unless (and (stringp call-id)
                   (not (string-empty-p call-id))
                   (= 1
                      (cl-count
                       call-id uses :test #'equal
                       :key
                       (lambda (candidate)
                         (and (llm-provider-utils-tool-use-p candidate)
                              (llm-provider-utils-tool-use-id candidate))))))
        (error "Tool call ID is missing or duplicated"))
      (push ordinal (car consumed))
      (append
       (copy-tree base-origin t)
       (list :tool-call-id call-id
             :tool-call-name (llm-provider-utils-tool-use-name use)
             :tool-call-ordinal (1+ ordinal)
             :tool-call-arguments
             (copy-tree (llm-provider-utils-tool-use-args use) t))))))

(defun superchat--llm-restore-provider-tool-use (prompt origin)
  "Restore ORIGIN's arguments in PROMPT after llm.el normalization."
  (let* ((interactions (llm-chat-prompt-interactions prompt))
         (interaction (car (last interactions)))
         (uses (and interaction
                    (llm-chat-prompt-interaction-content interaction)))
         (use (and (listp uses)
                   (nth (1- (plist-get origin :tool-call-ordinal)) uses))))
    (unless (and (llm-provider-utils-tool-use-p use)
                 (equal (plist-get origin :tool-call-id)
                        (llm-provider-utils-tool-use-id use))
                 (equal (plist-get origin :tool-call-name)
                        (llm-provider-utils-tool-use-name use)))
      (error "Provider tool-use history diverged before execution"))
    (superchat--llm-set-tool-use-args
     use
     (llm-provider-utils--normalize-args
      (copy-tree (plist-get origin :tool-call-arguments) t)
      :false))))

(defun superchat--llm-attempt-tools (prompt tools base-origin)
  "Clone TOOLS so every function invocation binds BASE-ORIGIN for PROMPT."
  (let ((consumed (cons nil nil))
        (raw-uses (cons nil nil)))
    (puthash prompt raw-uses superchat--llm-raw-tool-uses)
    (mapcar
     (lambda (tool)
       (let ((name (llm-tool-name tool))
             (function (llm-tool-function tool))
             (async (llm-tool-async tool)))
         (make-llm-tool
          :name name
          :description (llm-tool-description tool)
          :args (copy-tree (llm-tool-args tool) t)
          :async async
          :function
          (lambda (&rest args)
            (let* ((origin
                    (condition-case _err
                        (let ((origin
                               (superchat--llm-attempt-call-origin
                                prompt tool args consumed base-origin raw-uses)))
                          (superchat--llm-restore-provider-tool-use prompt origin)
                          origin)
                      (error
                       (when-let* ((observer
                                    (plist-get
                                     base-origin :audit-failure-observer)))
                         (ignore-errors
                           (funcall
                            observer
                            (list
                             :attempt
                             (plist-get base-origin :evidence-attempt)
                             :kind "tool-origin-invalid"
                             :tool-name name))))
                       nil)))
                   (active-p (plist-get base-origin :attempt-active-p))
                   (result
                    (if origin
                        "[Tool call blocked: stale provider attempt]"
                      "[Tool call blocked because exact audit failed]")))
              (if (and origin (functionp active-p) (funcall active-p))
                  (let ((superchat--llm-tool-origin origin))
                    (apply function args))
                (progn
                  (when-let* ((observer
                               (and origin
                                    (plist-get
                                     base-origin :stale-observer))))
                    (funcall observer origin result))
                  (if async
                      (progn
                        (when (functionp (car args))
                          (funcall (car args) result))
                        nil)
                    result))))))))
     tools)))

(cl-defgeneric superchat--provider-name (provider)
  "Return a human-readable lowercase name for PROVIDER.")

(cl-defgeneric superchat--provider-chat-model (provider)
  "Return the chat-model of PROVIDER, or nil if not extractable.")

(cl-defgeneric superchat--provider-with-chat-model (provider new-model)
  "Return a copy of PROVIDER with chat-model set to NEW-MODEL.
Default: return PROVIDER unchanged (no override available).")

(defun superchat--copy-provider-with-chat-model
    (provider copy-function struct-type new-model)
  "Copy PROVIDER with COPY-FUNCTION and set its NEW-MODEL slot.
STRUCT-TYPE names the provider struct that owns the `chat-model' slot."
  (let ((copy (funcall copy-function provider)))
    (setf (cl-struct-slot-value struct-type 'chat-model copy) new-model)
    copy))

(defun superchat--effective-llm-backend (&optional target-model)
  "Return the effective llm backend, applying model override.
TARGET-MODEL is a one-shot model override; falls back to
`superchat-llm-model'.  When neither resolves, the raw backend is
returned.  If model override is unavailable for the provider type
\(only OpenAI, Claude, Ollama have copy-* methods installed), the
backend is returned unchanged and the override is silently dropped."
  (let* ((model (or target-model superchat-llm-model))
         (backend superchat-llm-backend))
    (cond
     ((null backend) nil)
     ((null model) backend)
     (t
      (condition-case nil
          (superchat--provider-with-chat-model backend model)
        (error backend))))))

(defun superchat--should-attach-tools-p (input)
  "Return non-nil when tools should be attached for INPUT.
Honors `superchat-llm-tools-enabled':
  nil          -> never
  `always'/t   -> always
  `on-demand'  -> only when INPUT shows a tool intent:
                  contains a `#'-style file reference, explicitly names
                  `render_ui', `eshell-command', or a team tool, or a slash
                  command is currently active."
  (cond
   ((null superchat-llm-tools-enabled) nil)
   ((memq superchat-llm-tools-enabled '(always t)) t)
   (t  ;; on-demand and anything else conservatively maps here
    (or (and (boundp 'superchat--current-command)
             superchat--current-command)
        (and (stringp input)
             (or (string-match-p superchat--file-ref-regexp input)
                 (superchat--render-ui-intent-p input)
                 (superchat--eshell-command-intent-p input)
                 (superchat--team-tool-intent-p input)))))))

(defun superchat--team-tool-intent-p (input)
  "Return non-nil when INPUT explicitly names a team tool."
  (and (stringp input)
       (string-match-p
        "\\_<\\(?:team_task\\|team_message\\|delegate_to_subagent\\(?:_parallel\\)?\\)\\_>"
        input)))

(defun superchat--eshell-command-intent-p (input)
  "Return non-nil when INPUT explicitly asks for the Eshell bridge."
  (and (stringp input)
       (or (string-match-p "\\_<eshell-command\\_>" input)
           (string-match-p "\\_<eshell command\\_>" input))))

(defun superchat--render-ui-intent-p (input)
  "Return non-nil when INPUT explicitly names the built-in `render_ui'."
  (and (stringp input)
       (string-match-p "\\_<render_ui\\_>" input)))

(defun superchat--collect-llm-tools (&optional input tool-names)
  "Collect built-in + MCP tools for the current request.
Returns nil when `superchat-llm-tools-enabled' (combined with INPUT
for the `on-demand' policy) forbids attaching tools — that lets the
caller bypass llm.el's multi-output / tool-calling mode entirely,
which is the main contributor to time-to-first-token.

When TOOL-NAMES is non-nil, only tools whose names are members of
that list are returned.  This allows presets to restrict the active
tool set without changing global allowlists.  The symbol `none'
means an explicitly empty tool set (preset declared `tools: []'):
no tools are attached at all."
  (unless (eq tool-names 'none)
    (when (or tool-names (superchat--should-attach-tools-p input))
      (let* ((render-ui-intent (superchat--render-ui-intent-p input))
             (llm-tools (when (fboundp 'superchat-get-llm-tools)
                          (superchat-get-llm-tools tool-names)))
             (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                          (superchat-mcp-get-tools)))
             (all (append llm-tools mcp-tools))
             ;; A direct render request has a single, audited UI path.
             ;; Do not let an older persisted synthesized tool, or the
             ;; self-extension primitive itself, compete with `render_ui'.
             (all (if render-ui-intent
                      (cl-remove-if
                       (lambda (tool)
                         (equal (llm-tool-name tool) "define_tool"))
                       all)
                    all)))
          (append
           (if tool-names
               (cl-remove-if-not (lambda (tool)
                                   (member (llm-tool-name tool) tool-names))
                                 all)
             all)
           ;; An explicit profile is an exact request-local surface.
           ;; Synthesized tools remain available only to unprofiled chat.
           (unless (or render-ui-intent tool-names)
             (bound-and-true-p superchat-llm-synthesized-tools))
           ;; Plan-scoped: progress reporting exists only while the
           ;; session's approved plan governs this request (and no
           ;; preset restricts the surface — approval clears the
           ;; planning preset, so execution requests reach this branch).
           (when (and (not render-ui-intent)
                      (not tool-names)
                      (boundp 'superchat-buffer-name)
                      superchat-buffer-name
                      (boundp 'superchat--active-plan)
                      (buffer-live-p (get-buffer superchat-buffer-name))
                      (buffer-local-value
                       'superchat--active-plan
                       (get-buffer superchat-buffer-name))
                      (fboundp 'llm-make-tool)
                      (fboundp 'superchat--report-plan-progress-spec))
             (list
              (apply #'llm-make-tool
                     :name "report_plan_progress"
                     (superchat--report-plan-progress-spec)))))))))

(defun superchat--llm-extract-text (result)
  "Extract the :text field from an llm.el multi-output RESULT.
For plain string results, returns the string unchanged."
  (cond
   ((null result) "")
   ((stringp result) result)
   ((and (consp result) (stringp (plist-get result :text)))
    (plist-get result :text))
   (t (format "%S" result))))

(defun superchat--llm-tool-round-p (result)
  "Return non-nil when RESULT asks the caller to continue after tools.
llm.el executes requested tools and appends their results to the mutable
chat prompt, but leaves sending the next model request to its caller.

The only signal that matters is whether this round called tools:
llm.el attaches `:tool-results' precisely when it executed tool uses
\(see `llm-provider-utils-execute-tool-uses'), and a bare alist of
tool-result conses is the non-multi-output form of the same thing.

Accompanying `:text' must NOT end the loop.  Models routinely narrate
the call they are about to make (\"Let me read that file first.\") in
the same round as the tool use; treating that narration as the final
answer strands the tool result unseen and hands the user the narration
instead of an answer.  The final answer is the round that returns text
and no tool calls."
  (and (consp result)
       (or (not (keywordp (car result)))
           (plist-get result :tool-results))
       t))

(defun superchat--llm-tool-result-entries (result)
  "Return RESULT's tool-result alist, or nil for ordinary text.

llm.el returns a plist with a `:tool-results' alist in multi-output mode
and the same alist directly in non-multi-output mode.  Keeping this shape
normalization in one place makes error classification consistent across the
sync, async, and sub-agent loops."
  (let ((tool-results
         (cond
          ((and (consp result) (plist-get result :tool-results))
           (plist-get result :tool-results))
          ((and (consp result) (not (keywordp (car result)))) result))))
    (and (listp tool-results) tool-results)))

(defun superchat--llm-tool-error-entries (result)
  "Return failed tool-result entries found in RESULT.

Tool functions deliberately return failures as readable strings so llm.el
can append them as tool results.  Only known failure markers are classified;
ordinary tool output such as a file whose first line is arbitrary text is
left alone."
  (cl-remove-if-not
   (lambda (entry)
     (and (consp entry)
          (stringp (cdr entry))
          (let ((message (string-trim (cdr entry))))
            (or (string-prefix-p "Error:" message)
                (string-prefix-p "genui:" message)
                (string-prefix-p "[tool-error" message)
                (string-prefix-p "[Tool error" message)
                (string-prefix-p "[Error:" message)
                (string-prefix-p "[ERROR:" message)
                (string-prefix-p "[Tool call failed" message)
                (string-prefix-p "[Tool call blocked" message)
                (string-prefix-p "[Tool call denied" message)
                (string-prefix-p "[Tool call cancelled" message)
                (string-prefix-p "[Tool call canceled" message)
                (string-prefix-p "[Tool definition declined" message)
                (string-prefix-p "❌" message)
                (string-prefix-p "[Synthesized tool" message)
                (string-prefix-p "[Call to synthesized tool" message)
                (string-prefix-p "[Delegation denied" message)))))
   (superchat--llm-tool-result-entries result)))

(defun superchat--llm-tool-error-non-retryable-p (result)
  "Return non-nil when RESULT contains a user/policy-blocked tool call."
  (cl-some
   (lambda (entry)
     (string-match-p
      "\\b\\(?:refused\\|declined\\|cancelled\\|canceled\\|denied\\|blocked\\)\\b"
      (cdr entry)))
   (superchat--llm-tool-error-entries result)))

(defun superchat--llm-tool-error-feedback (result)
  "Return corrective feedback when RESULT contains failed tool calls.

Add a short user-role instruction after tool results: providers otherwise
tend to narrate a fallback instead of repairing and retrying the requested
tool.  Return nil for successful tool rounds or ordinary text responses.
Callers can use `superchat--llm-tool-error-non-retryable-p' to stop rather
than retry a user or policy refusal."
  (let ((errors (superchat--llm-tool-error-entries result)))
    (when errors
      (concat
       "Tool execution failed. Do not claim success or silently substitute a fallback. "
       (if (superchat--llm-tool-error-non-retryable-p result)
           "The user or a policy prevented this call; do not retry it. Explain the limitation or continue without that tool."
         "Correct the arguments and retry the requested tool.")
       "\n"
       (mapconcat
        (lambda (entry)
          (format "- %s: %s" (car entry) (cdr entry)))
        errors
        "\n")))))

(defun superchat--llm-tool-protocol-error-p (type)
  "Return non-nil when TYPE is an llm.el tool-call validation error.

These errors are reported through the request error callback rather than
as a normal `:tool-results' response.  They are still model-correctable:
the preceding assistant tool-use must be removed before the prompt can be
sent again."
  (and (symbolp type)
       (string-prefix-p "llm-tool-" (symbol-name type))))

(defun superchat--llm-tool-protocol-error-feedback (type message)
  "Build corrective user feedback for an llm.el tool error TYPE/MESSAGE."
  (let ((detail (cond
                 ((stringp message) message)
                 ((and (consp message) (stringp (car message)))
                  (car message))
                 (t (format "%s" message)))))
    (format
     "%s\nOriginal llm.el error (%s): %s\nDo not claim success unless the corrected tool call succeeds."
     (pcase type
       ('llm-tool-unknown-tool
        "The requested tool name is not available. Call only an exact name from the tool list, or answer directly without tools.")
       ('llm-tool-missing-argument
        "The tool call is missing a required argument. Supply the required arguments and retry the tool.")
       ('llm-tool-unknown-argument
        "The tool call contains an unsupported argument. Remove it, use the declared argument names, and retry the tool.")
       ('llm-tool-call-error
        "The tool arguments were not complete valid JSON. Retry with shorter arguments that fit in one response.")
       (_
        "The tool call was rejected by llm.el. Correct its name or arguments and retry the tool."))
     type
     detail)))

(defun superchat--llm-round-limit-action ()
  "Return the action to take after exhausting the current tool budget."
  (pcase superchat-llm-round-limit-action
    ('ask (if noninteractive
              'finalize
            (if (y-or-n-p
                 (format "Tool round limit reached. Allow another %d rounds? "
                         superchat-llm-max-tool-rounds))
                'continue
              'finalize)))
    ((or 'finalize 'stop) superchat-llm-round-limit-action)
    (_ 'stop)))

(defun superchat--llm-disable-prompt-tools (prompt)
  "Remove tools from mutable llm.el PROMPT for its final request."
  (when (fboundp 'llm-chat-prompt-tools)
    ;; As with `superchat--llm-drop-dangling-tool-use', llm.el may load
    ;; after this file.  Resolve the struct slot only when the accessor is
    ;; live instead of baking an unavailable setf expander into this defun.
    (let ((slot (cl-struct-slot-offset 'llm-chat-prompt 'tools)))
      (if (eq (cl-struct-sequence-type 'llm-chat-prompt) 'list)
          (setf (nth slot prompt) nil)
        (aset prompt slot nil)))))

(defun superchat--llm-drop-dangling-tool-use (prompt)
  "Remove PROMPT's trailing assistant tool-use record, if present.

llm.el writes the assistant's tool calls into PROMPT before executing
them (`llm-provider-populate-tool-uses').  When a call names an unknown
tool, no tool-result interaction ever follows, and re-sending the prompt
with that dangling record violates the provider conversation contract
\(an assistant tool call must be answered by a tool result).  Returns
non-nil when a record was dropped."
  (when (and (fboundp 'llm-chat-prompt-interactions)
             (fboundp 'llm-chat-prompt-interaction-role))
    (let* ((interactions (llm-chat-prompt-interactions prompt))
           (last-interaction (car (last interactions))))
      (when (and last-interaction
                 (or (not (fboundp 'llm-chat-prompt-p))
                     (llm-chat-prompt-p prompt))
                 (eq (llm-chat-prompt-interaction-role last-interaction)
                     'assistant)
                 ;; Plain prose is a string; tool-use records are not.
                 (not (stringp (llm-chat-prompt-interaction-content
                                last-interaction))))
        ;; `superchat-llm.el' may load before optional `llm.el'.  Using the
        ;; accessor directly in `setf' then expands to a late-bound
        ;; `(setf llm-chat-prompt-interactions)' call, which does not exist.
        ;; Resolve the slot offset at runtime instead, after llm.el has made
        ;; the prompt struct available.
        (let ((slot (cl-struct-slot-offset 'llm-chat-prompt 'interactions))
              (value (butlast interactions)))
          (if (eq (cl-struct-sequence-type 'llm-chat-prompt) 'list)
              (setf (nth slot prompt) value)
            (aset prompt slot value)))
        t))))

(defun superchat--image-attachment-info (file)
  "Return supported image metadata for FILE, or nil."
  (when-let* ((mime-type
               (pcase (downcase (or (file-name-extension file) ""))
                 ((or "jpg" "jpeg") "image/jpeg")
                 ("png" "image/png")
                 ("webp" "image/webp"))))
    (list :path file :mime-type mime-type
          :size (file-attribute-size (file-attributes file)))))

(defun superchat--image-media (info)
  "Read image attachment INFO as native `llm-media'."
  (let ((path (plist-get info :path)))
    (unless (file-readable-p path)
      (user-error "Image attachment is not readable: %s" path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (make-llm-media :mime-type (plist-get info :mime-type)
                      :data (buffer-string)))))

(defun superchat--build-llm-prompt
    (text tools &optional context preset context-files)
  "Build an `llm-chat-prompt' struct from TEXT and TOOLS.
llm.el ≥ 0.7 requires a struct for `llm-chat-streaming' / `llm-chat',
even when no tools are attached.  TOOLS may be nil.

CONTEXT, when a non-empty string, is passed as the prompt's
`:context' — llm.el turns it into the system message.  This is how
turn system-prompts (preset persona, language instruction, tool
guidance) reach the provider.

PRESET may override temperature, max tokens, and reasoning for this
request.  Otherwise reasoning comes from `superchat-llm-reasoning' so that
reasoning-capable providers (Ollama qwen3.x, deepseek-r1, etc.) skip
thinking by default — thinking blocks streaming and inflates TTFT.

Supported image CONTEXT-FILES become native `llm-multipart' media."
  (let* ((preset-reasoning (and preset (superchat-preset-reasoning preset)))
         (reasoning (if (and preset-reasoning
                             (not (eq preset-reasoning 'inherit)))
                        preset-reasoning
                      superchat-llm-reasoning))
         (images (delq nil (mapcar #'superchat--image-attachment-info
                                   context-files)))
         (content (if images
                      (apply #'llm-make-multipart text
                             (mapcar #'superchat--image-media images))
                    text))
         (args (append (when tools (list :tools tools))
                       (when (and (stringp context)
                                  (not (string-empty-p (string-trim context))))
                         (list :context context))
                       (when (and preset (superchat-preset-temperature preset))
                         (list :temperature
                               (superchat-preset-temperature preset)))
                       (when (and preset (superchat-preset-max-tokens preset))
                         (list :max-tokens
                               (superchat-preset-max-tokens preset)))
                       (when reasoning
                         (list :reasoning (if (eq reasoning t)
                                             'medium
                                           reasoning))))))
    (apply #'llm-make-chat-prompt content args)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-chat-model ((provider llm-openai))
     (llm-openai-chat-model provider)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-name ((_provider llm-openai-compatible))
     "open ai compatible"))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-openai) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-openai 'llm-openai new-model)))

(eval-after-load 'llm-claude
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-claude) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-claude 'llm-claude new-model)))

(eval-after-load 'llm-ollama
  '(cl-defmethod superchat--provider-name ((_provider llm-ollama))
     "ollama"))

(eval-after-load 'llm-ollama
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-ollama) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-ollama 'llm-ollama new-model)))


;; --- LLM Backend (Extracted from supertag-rag.el) ---

;; Provider accessors. The real llm.el v0.7+ uses cl-defstruct providers
;; (e.g. `llm-openai'), not plists. These cl-defgenerics let us extract
;; the chat-model generically and override it (e.g. for `@model') per
;; provider type, with safe no-op defaults for providers we don't have
;; specialized methods for.

(cl-defmethod superchat--provider-name (provider)
  "Return a human-readable name for PROVIDER.
Default fallback when no specialized method matches.
Tries `llm-name' when fbound (handles both string and symbol returns),
otherwise returns a placeholder.  Specialized methods (installed via
`eval-after-load' for known provider structs) take precedence over
this one."
  (if (fboundp 'llm-name)
      (let ((name (ignore-errors (llm-name provider))))
        (cond
         ((null name) "unknown")
         ((stringp name) (downcase name))
         ((symbolp name) (downcase (symbol-name name)))
         (t (downcase (format "%s" name)))))
    "unknown"))

(cl-defmethod superchat--provider-chat-model (provider)
  "Return chat-model of PROVIDER, or nil.
Default: cannot extract chat-model from this provider type.
Specialized methods (installed via `eval-after-load' for known
provider structs) take precedence over this one."
  nil)

(cl-defmethod superchat--provider-with-chat-model (provider new-model)
  "Return a copy of PROVIDER with model set to NEW-MODEL.
Default: cannot override model on this provider type."
  provider)

;; --- Dispatchers ---

(defun superchat--callback-in-view (buffer-name callback)
  "Return a CALLBACK that runs in the Superchat View BUFFER-NAME."
  (lambda (&rest args)
    (let ((superchat-buffer-name buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
        (apply callback args)))))

(defun superchat--llm-request-plz-propagate-callback-errors
    (original url &rest args)
  "Call ORIGINAL for URL, routing async success callback errors to `:on-error'.
For a Superchat Ollama request, decode curl output as UTF-8 so Emacs carries
incomplete characters across process-filter chunks."
  (let ((on-success (plist-get args :on-success))
        (on-error (plist-get args :on-error)))
    (when (functionp on-success)
      (setq args
            (plist-put
             args :on-success
             (lambda (response)
               (condition-case err
                   (funcall on-success response)
                 (llm-error
                  (if (functionp on-error)
                      (funcall on-error (car err) (error-message-string err))
                    (signal (car err) (cdr err)))))))))
    (let ((process (apply original url args)))
      (when (and superchat--llm-ollama-response-p
                 (processp process))
        (set-process-coding-system process 'utf-8-unix 'binary))
      process)))

(with-eval-after-load 'llm-request-plz
  (unless (advice-member-p
           #'superchat--llm-request-plz-propagate-callback-errors
           'llm-request-plz-async)
    (advice-add 'llm-request-plz-async :around
                #'superchat--llm-request-plz-propagate-callback-errors)))

(defun superchat--llm-watch-plz-request (request active-p)
  "Finalize REQUEST when plz misses its asynchronous process sentinel.

ACTIVE-P returns non-nil while the originating Runtime request is live."
  (when (and (processp request)
             (eq (process-sentinel request) 'plz--sentinel))
    (let (timer)
      (setq timer
            (run-with-timer
             1 1
             (lambda ()
               (cond
                ((not (funcall active-p))
                 (cancel-timer timer))
                ((process-live-p request) nil)
                (t
                 (cancel-timer timer)
                 (when (and (buffer-live-p (process-buffer request))
                            (eq :plz-result
                                (process-get request :plz-result))
                            (not (eq 'buffer
                                     (process-get request :plz-as))))
                   (funcall (process-sentinel request)
                            request "workaround"))))))))))

(defun superchat--llm-generate-answer
    (prompt callback stream-callback
            &optional target-model context-files tools system-prompt preset)
  "Generate an answer for PROMPT using llm.el, handling streaming and tool use.
Optionally use TARGET-MODEL for this request only.
CONTEXT-FILES is an optional list of file paths to include as context.
TOOLS is an optional list of tool names to expose; when nil, tools are
collected from PROMPT and global settings as before.
SYSTEM-PROMPT, when a non-empty string, is sent as the system
message via the llm prompt's `:context' (turn system-prompts:
preset persona, language instruction, tool guidance).
CALLBACK is called with the final response string (or with
\(tool-call . ...) /\(tool-result . ...) markers for back-compat).
STREAM-CALLBACK is called with each text chunk during streaming.

llm.el executes requested tools and mutates its prompt with their
results.  Superchat reuses that prompt to issue subsequent model
requests until it receives text, bounded by
`superchat-llm-max-tool-rounds'."
  ;; The transport still reads distribution config (backend, timeouts,
  ;; TTFT and response-mode UI) owned by superchat.el.  A kernel-only
  ;; caller reaching the port directly loads the distribution once here;
  ;; extracting those seams from the monolith is the next deepening step.
  (unless (boundp 'superchat-llm-backend)
    (require 'superchat nil t))
  (if (null superchat-llm-backend)
      (let ((response
             "[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]"))
        (when (functionp superchat--llm-runtime-observer)
          (funcall superchat--llm-runtime-observer
                   'terminal (list :status 'failed :response response)))
        (when callback
          (funcall callback response)))
    ;; Collect tools and detect mode ONCE per request — both
    ;; `--show-response-mode-indicator' and `--get-adjusted-timeout'
    ;; used to recompute this independently, which meant 3 sync
    ;; MCP polls and 3 tool-set walks before the first byte left
    ;; the keyboard.
    (let* ((request-buffer-name superchat-buffer-name)
           (runtime-observer superchat--llm-runtime-observer)
           ;; The Runtime binding is dynamic and provider callbacks are
           ;; asynchronous.  Capture the request evidence once so a later
           ;; tool-followup does not fall back to the global nil value after
           ;; the Runtime's dynamic scope has unwound.
           (evidence-context
            (copy-tree superchat--llm-evidence-context t))
           (evidence-contract
            (plist-get evidence-context :contract))
           (active-experience-view
            (plist-get
             (plist-get evidence-context :context-envelope)
             :experience-view))
           (tools-prepared-p superchat--llm-tools-prepared-p)
           (tools (if tools-prepared-p
                      superchat--llm-prepared-tools
                    (superchat--collect-llm-tools
                     (when (stringp prompt) prompt)
                     tools)))
           (_evidence-tools
            (when evidence-contract
              (superchat--llm-validate-evidence-tools tools)))
           ;; One wrapper closure per request: the run's tools share a
           ;; guardrail set and a call counter, and a tool the agent
           ;; synthesizes mid-run must join the same accounting.
           (wrap-fn
            (or superchat--llm-tool-wrapper
                (and tools
                     (not tools-prepared-p)
                     (fboundp 'superchat--agent-make-wrapper)
                     (superchat--agent-make-wrapper preset))))
           (_evidence-wrapper
            (when (and evidence-contract tools (not (functionp wrap-fn)))
              (signal
               'superchat-llm-evidence-recording-error
               '("epistemic-learning-v0 requires the Authority tool wrapper"))))
           (tools (if (and wrap-fn (not tools-prepared-p))
                      (mapcar wrap-fn tools)
                    tools))
           (response-mode (superchat--detect-response-mode tools t))
           (_ (superchat--show-response-mode-indicator response-mode))
           (adjusted-timeout (superchat--get-adjusted-timeout response-mode))
           (effective-backend (superchat--effective-llm-backend target-model))
           ;; Emacs must carry incomplete UTF-8 characters across the raw
           ;; process-filter chunks produced by plz for Ollama NDJSON.
           (superchat--llm-ollama-response-p
            (and (fboundp 'llm-ollama-p)
                 (llm-ollama-p effective-backend)))
           ;; Only stream tool calls when the provider explicitly promises
           ;; that its streaming decoder preserves them.  Plain chat remains
           ;; streaming; tool-capable providers without that capability use
           ;; llm.el's non-streaming async path instead.
           (streaming-request-p
            (or (null tools)
                (and (fboundp 'llm-capabilities)
                     (memq 'streaming-tool-use
                           (ignore-errors
                             (llm-capabilities effective-backend))))))
           (real-prompt (superchat--build-llm-prompt
                         prompt tools system-prompt preset context-files))
           ;; Keep the request-level functions unwrapped by provider attempt.
           ;; `real-prompt' adopts completed attempt history, including its
           ;; attempt wrappers, so using its tool objects here would nest old
           ;; provenance inside every later tool round.
           (evidence-base-tools
            (and evidence-contract (copy-tree tools t)))
           ;; Bind `define_tool' to THIS prompt, so what it writes can be
           ;; injected into the request that is already in flight and used
           ;; on the next round.  Persist to the session registry: unlike a
           ;; sub-agent, the main session has later turns.
           (_ (when (fboundp 'superchat--syn-bind)
                (superchat--syn-bind
                 real-prompt wrap-fn
                 superchat--llm-persist-synthesized-tools)))
           (multi-output (and tools t))
           (response-parts '())
           (tool-rounds 0)
           (tool-round-limit superchat-llm-max-tool-rounds)
           (final-answer-requested nil)
           (completed nil)
           (timeout-timer nil)
           ;; ── TTFT measurement (meausred flag is lexical, closure-safe) ──
           (ttft-measured nil)
           ;; Tracks the cumulative streamed text as reported by
           ;; llm.el so we can compute the actual delta to forward
           ;; downstream.  llm.el's partial-callback semantics are the
           ;; same in both modes ("the entire response that has been
           ;; received" — see `llm-chat-streaming' and
           ;; `llm-provider-utils-streaming-accumulate'):
           ;;   - multi-output=nil  → chunk is the FULL text so far
           ;;   - multi-output=t    → chunk is a plist; :text is the
           ;;                         FULL text so far as well
           ;; We normalise both paths to deltas so `response-parts'
           ;; and `stream-callback' always see incremental text.
           (cumulative "")
           (tool-error-retries 0)
           (unknown-tool-retry-limit
            (or superchat--llm-unknown-tool-retry-limit
                superchat-llm-unknown-tool-retries))
           (round-limit-function superchat--llm-round-limit-function)
           (run-active-p (or superchat--llm-run-active-p (lambda () t)))
           (request-generation 0)
           (record-first-output
            (lambda (value)
              (let ((attempt (or superchat--llm-callback-attempt
                                 request-generation)))
                (when (and evidence-contract
                           (> attempt 0)
                           (functionp runtime-observer))
                  (condition-case err
                      (funcall runtime-observer 'first-output
                               (append (list :attempt attempt) value))
                    (error
                     (signal 'superchat-llm-evidence-recording-error
                             (list (error-message-string err)))))))))
           (send-request nil)
           ;; ---- local helper: finalize exactly once ----
           (finalize
            (lambda (response &optional status)
              (unless completed
                (setq completed t)
                (when timeout-timer (cancel-timer timeout-timer))
                (with-current-buffer (get-buffer-create request-buffer-name)
                  (setq superchat--active-timeout-timer nil))
                (when (functionp runtime-observer)
                  (funcall runtime-observer
                           'terminal
                           (list :status (or status 'completed)
                                 :response response)))
                (when callback
                  (funcall callback response)))))
           ;; Holds elapsed seconds captured on the FIRST stream-cb call
           ;; (any chunk type) until we can annotate the Assistant header
           ;; on the first text chunk.  nil once consumed.
           (stream-cb
            (lambda (chunk)
              (let* ((text (cond
                            ((stringp chunk) chunk)
                            ((and (consp chunk)
                                  (stringp (plist-get chunk :text)))
                             (plist-get chunk :text)))))
                (cond
                 ;; text chunk — cumulative full text in BOTH payload shapes
                 ((stringp text)
                  (unless completed
                    (let ((delta
                           (cond
                            ((string-prefix-p cumulative text)
                             (substring text (length cumulative)))
                            ;; defensive: provider reset / out-of-order — emit full
                            (t text))))
                      (setq cumulative text)
                      (unless (string-empty-p delta)
                        (funcall record-first-output
                                 (list :kind "text" :text delta))
                        ;; ── TTFT: measure on first non-empty text delta ──
                        ;; This is the ONLY reliable signal that the LLM has
                        ;; actually started generating.  Empty flushes, nil
                        ;; callbacks, reasoning blocks, and tool-call plists
                        ;; are all filtered out before we reach this point.
                        (when (and superchat-show-ttft
                                   (not ttft-measured))
                          (setq ttft-measured t)
                          (with-current-buffer (get-buffer-create request-buffer-name)
                            (when superchat--ttft-start-time
                              (let ((elapsed (- (float-time) superchat--ttft-start-time)))
                                (message "⚡ TTFT: %.2fs" elapsed)
                                (superchat--annotate-ttft elapsed)))))
                        (push delta response-parts)
                        (when stream-callback
                          (funcall stream-callback delta)))
                      (when (fboundp 'superchat--extend-timeout)
                        (superchat--extend-timeout)))))
                 ;; tool-call/tool-result markers — forward to caller for back-compat
                 ((and (consp chunk)
                       (memq (car chunk) '(tool-call tool-result)))
                  (when callback
                    (funcall callback chunk))
                  (when (fboundp 'superchat--extend-timeout)
                    (superchat--extend-timeout)))
                 ;; reasoning block — ignore
                 ((and (consp chunk) (eq (car chunk) 'reasoning))
                  nil)
                 (t
                  (message "superchat: unhandled stream payload: %S" chunk))))))
           (response-cb nil)
           ;; llm.el calls the error callback with TWO arguments
           ;; (type message) — see `llm-provider-utils-callback-in-buffer'
           ;; call sites, every one `error-callback 'error <msg>'.  A
           ;; one-argument lambda here crashes inside plz's timer with
           ;; `wrong-number-of-arguments', which then masks the real API
           ;; error.  The sub-agent path already takes (_type message);
           ;; `&rest' keeps this robust if llm.el ever changes the arity.
           ;; Bound via setq below so its body can reference `error-cb'
           ;; for the unknown-tool re-send (a let* self-reference
           ;; would be a free variable).
           (error-cb nil))

      ;; Safety-net timeout. Fires only when nothing else has finalised.
      (when adjusted-timeout
        (setq timeout-timer
              (run-with-timer
               adjusted-timeout nil
               (lambda ()
                 (funcall finalize
                          (if response-parts
                              (string-join (nreverse response-parts) "")
                            "[Response timeout. Try: (setq superchat-response-timeout 300)]")
                          'timed-out))))
        (with-current-buffer (get-buffer-create request-buffer-name)
          (setq superchat--active-timeout-timer timeout-timer)))

      (when target-model
        (message "Switching to model: %s" target-model))

      ;; ── TTFT measurement: reset per-request flag ──
      (when superchat-show-ttft
        (setq ttft-measured nil))

      (superchat--ttft-log "pre-llm-call")

      ;; ── Prompt size diagnostic ──
      (when superchat-show-ttft-breakdown
        (let* ((chars (length (or (and (stringp prompt) prompt) "")))
               (tools-chars (if tools (length (format "%S" tools)) 0))
               (history-len (length superchat--conversation-history))
               (est-tokens (/ (+ chars tools-chars) 4)))
          (message "⏱ TTFT [prompt-size]: %d chars + %d tools + %d history-msgs (~%d tokens)"
                   chars tools-chars history-len est-tokens)))

      (setq error-cb
            (lambda (type &rest more)
              (let ((err (if more (car more) type)))
                (when (eq type 'superchat-llm-evidence-recording-error)
                  (signal type (if more more (list err))))
                (when (superchat--llm-tool-protocol-error-p type)
                  (funcall record-first-output
                           (list :kind "tool-protocol-error"
                                 :error-type (format "%s" type)
                                 :message (format "%s" err))))
                (cond
                 ;; Tool-call validation errors are model-correctable.  The
                 ;; provider has already appended an assistant tool-use
                 ;; record, so remove that dangling record, append the exact
                 ;; llm.el error as a user-role correction, and resend.
                 ((and (fboundp 'superchat--llm-tool-protocol-error-p)
                       (superchat--llm-tool-protocol-error-p type)
                       (not final-answer-requested)
                       (not evidence-contract)
                       (not completed)
                       (funcall run-active-p)
                       (< tool-error-retries
                          unknown-tool-retry-limit)
                       (fboundp 'superchat--llm-drop-dangling-tool-use)
                       (fboundp 'llm-chat-prompt-append-response))
                  (cl-incf tool-error-retries)
                  (superchat--llm-drop-dangling-tool-use real-prompt)
                  (llm-chat-prompt-append-response
                   real-prompt
                   (superchat--llm-tool-protocol-error-feedback type err)
                   'user)
                  (setq response-parts '() cumulative "")
                  (condition-case e
                      (funcall send-request 'tool-protocol-retry)
                    (superchat-llm-evidence-recording-error
                     (signal (car e) (cdr e)))
                    (error
                     (funcall finalize
                              (format "[llm-chat-streaming error: %s]"
                                      (error-message-string e))
                              'failed))))
                 (t
                  (funcall finalize
                           (cond
                            (response-parts
                             (string-join (nreverse response-parts) ""))
                            (t
                             (format "[API error: %s]"
                                     (or (and (stringp err) err)
                                         (and (consp err) (format "%S" err))
                                         "unknown"))))
                           'failed))))))
      (setq response-cb
            (lambda (response)
              (when (funcall run-active-p)
                (cond
                 ((and (stringp response)
                       (not (string-empty-p response)))
                  (funcall record-first-output
                           (list :kind "text" :text response)))
                 ((and (consp response)
                       (stringp (plist-get response :text))
                       (not (string-empty-p (plist-get response :text))))
                  (funcall record-first-output
                           (list :kind "text"
                                 :text (plist-get response :text)))))
                (cond
                 ((superchat--llm-tool-round-p response)
                  (cond
                   (final-answer-requested
                    (funcall finalize
                             "[Tool call returned after requesting a final answer]"
                             'failed))
                   (t
                    (let ((continue t))
                      (cl-incf tool-rounds)
                      (when (and (numberp tool-round-limit)
                                 (>= tool-rounds tool-round-limit))
                        (pcase (if (functionp round-limit-function)
                                   (funcall round-limit-function)
                                 (superchat--llm-round-limit-action))
                        ('continue
                         (if (and (numberp superchat-llm-max-tool-rounds)
                                  (> superchat-llm-max-tool-rounds 0))
                             (setq tool-round-limit
                                   (+ tool-round-limit
                                      superchat-llm-max-tool-rounds))
                           (setq final-answer-requested t
                                 multi-output nil)
                           (superchat--llm-disable-prompt-tools real-prompt)))
                        ('finalize
                         (setq final-answer-requested t
                               multi-output nil)
                         (superchat--llm-disable-prompt-tools real-prompt))
                        ('stop
                         (setq continue nil)
                         (funcall finalize
                                  (format "[Stopped after %d tool rounds]"
                                          tool-rounds)
                                  'failed))))
                      (when (and continue (not completed))
                        (when (not final-answer-requested)
                          (let ((feedback
                                 (superchat--llm-tool-error-feedback response))
                                (non-retryable
                                 (superchat--llm-tool-error-non-retryable-p
                                  response)))
                            (when feedback
                              (when (fboundp 'llm-chat-prompt-append-response)
                                (llm-chat-prompt-append-response
                                 real-prompt feedback 'user)))
                            ;; A user/policy refusal is terminal for this
                            ;; tool call; request one tool-free explanation
                            ;; rather than resending the blocked operation.
                            (when non-retryable
                              (setq final-answer-requested t
                                    multi-output nil)
                              (superchat--llm-disable-prompt-tools
                               real-prompt))))
                        ;; Close this round's rendered text before the next
                        ;; request: the transcript keeps the round's prose and
                        ;; the tool records, and the next round's text starts a
                        ;; fresh region after them.  Also drop this round's
                        ;; streamed parts so they cannot leak into the final
                        ;; answer's fallback join.
                        (when (fboundp 'superchat--render-close-tool-round)
                          (superchat--render-close-tool-round))
                        (setq response-parts '())
                        ;; Each round is a fresh llm request whose
                        ;; accumulated text restarts from empty.
                        (setq cumulative "")
                        (condition-case err
                            (funcall send-request
                                     (if final-answer-requested
                                         'final-answer
                                       'tool-followup))
                          (superchat-llm-evidence-recording-error
                           (signal (car err) (cdr err)))
                          (error
                           (funcall finalize
                                    (format "[llm-chat-streaming error: %s]"
                                            (error-message-string err))
                                    'failed))))))))
                  (t
                   (funcall finalize
                            (cond
                             ((and (consp response)
                                   (stringp (plist-get response :text))
                                   (not (string-empty-p
                                         (plist-get response :text))))
                              (plist-get response :text))
                             ((and (stringp response)
                                   (not (string-empty-p response)))
                              response)
                             (response-parts
                              (string-join (nreverse response-parts) ""))
                             (t "[Empty response from llm]"))))))))

      ;; llm.el may invoke these after the initiating dynamic binding has
      ;; unwound.  Route every round through the originating Session View.
      (setq finalize (superchat--callback-in-view request-buffer-name finalize)
            stream-cb (superchat--callback-in-view request-buffer-name stream-cb)
            response-cb (superchat--callback-in-view request-buffer-name response-cb)
            error-cb (superchat--callback-in-view request-buffer-name error-cb))

      (setq send-request
            (lambda (attempt-reason)
              (unless (memq attempt-reason
                            '(initial tool-protocol-retry
                              tool-followup final-answer))
                (error "Unknown provider attempt reason: %s" attempt-reason))
              ;; ── Stage 5 checkpoint: a pause request defers this attempt
              ;; at the tool boundary.  The prelude above is idempotent-safe
              ;; to re-run on resume; the model call itself never starts
              ;; while paused.
              (if (and (boundp 'superchat--pause-requested)
                       superchat--pause-requested)
                  (progn
                    (setq superchat--pending-round-reason attempt-reason)
                    (setq superchat--pending-send-request
                          (lambda (reason) (send-request reason)))
                    (when (fboundp 'superchat--pause-boundary)
                      (superchat--pause-boundary))))
                (progn
              (when (functionp runtime-observer)
                (dolist (message-text
                         (funcall runtime-observer 'drain-steers nil))
                  (unless (fboundp 'llm-chat-prompt-append-response)
                    (error "llm.el prompt mutation is unavailable"))
                  (llm-chat-prompt-append-response
                   real-prompt message-text 'user)))
              ;; Experience is a recovery instruction for the next tool
              ;; action, not a final-answer instruction.  Clear the local
              ;; view before the tool-free final request so a model cannot
              ;; repeat `workspace_info' after the corrected read succeeded.
              (when (eq attempt-reason 'final-answer)
                (setq active-experience-view nil)
                (when (fboundp 'llm-chat-prompt-append-response)
                  (llm-chat-prompt-append-response
                   real-prompt
                   "The requested tool work is complete. Answer directly in text now. Do not call any tool."
                   'user)))
              (when (and (eq attempt-reason 'tool-followup)
                         (functionp runtime-observer))
                (when-let ((view
                            (funcall runtime-observer 'before-attempt
                                     (list :attempt-reason attempt-reason))))
                  (when-let ((text (plist-get view :prompt)))
                    (unless (fboundp 'llm-chat-prompt-append-response)
                      (error "llm.el prompt mutation is unavailable"))
                    (llm-chat-prompt-append-response real-prompt text 'user))
                  ;; A blocked View has no prompt, but it still carries the
                  ;; action gate that Authority must enforce for this round.
                  (setq active-experience-view view)))
              (let* ((generation (cl-incf request-generation))
                     (attempt-prompt
                      (if evidence-contract
                          (copy-tree real-prompt t)
                        real-prompt))
                     snapshot-id)
                (when (and evidence-contract
                           (functionp runtime-observer))
                  (condition-case err
                      (let* ((_ (unless
                                    (fboundp 'llm-provider-chat-request)
                                  (error
                                   "llm.el request serializer is unavailable")))
                             ;; Force llm.el's provider :before methods to
                             ;; materialize actual default options on this
                             ;; isolated attempt before the snapshot.
                             (_ (llm-provider-chat-request
                                 effective-backend attempt-prompt
                                 streaming-request-p))
                             (provider
                              (superchat--provider-name effective-backend))
                             (model
                              (or (superchat--provider-chat-model
                                   effective-backend)
                                  target-model
                                  "unknown"))
                             (prompt-tools
                              (progn
                                (unless (fboundp 'llm-chat-prompt-tools)
                                  (error
                                   "llm.el prepared-tool accessor is unavailable"))
                                (llm-chat-prompt-tools attempt-prompt)))
                             (current-tools
                              (and prompt-tools
                                   (copy-tree evidence-base-tools t)))
                             (prompt-tool-records
                              (superchat--llm-evidence-tool-records
                               prompt-tools))
                             (_
                              (unless
                                  (equal
                                   prompt-tool-records
                                   (superchat--llm-evidence-tool-records
                                    current-tools))
                                (error
                                 "Prepared and provider tool schemas diverged")))
                             (_ (unless (fboundp 'llm-chat-prompt-context)
                                  (error
                                   "llm.el system-prompt accessor is unavailable")))
                             (prepared-envelope
                              (list
                               :format "superchat-prepared-envelope-v0"
                               :attempt-reason (symbol-name attempt-reason)
                               :system-prompt
                               (or (llm-chat-prompt-context attempt-prompt) "")
                               :messages
                               (superchat--llm-evidence-messages attempt-prompt)
                               :tools
                               prompt-tool-records
                               :context-files (vconcat context-files)
                               :view-envelope
                               (let ((envelope
                                      (plist-get evidence-context
                                                 :context-envelope)))
                                 (list
                                  :contract (plist-get envelope :contract)
                                  :status (plist-get envelope :status)
                                  :receipt
                                  (copy-tree (plist-get envelope :receipt))
                                  :disclosure
                                  (copy-tree
                                   (plist-get envelope :disclosure))
                                  :experience-action-policy
                                  (copy-tree
                                   (or
                                    (plist-get envelope
                                               :experience-action-policy)
                                    (plist-get active-experience-view
                                               :experience-action-policy))
                                   t)
                                  :experience-view
                                  (copy-tree active-experience-view t)))
                               :options
                               (superchat--llm-evidence-options
                                attempt-prompt streaming-request-p
                                multi-output))))
                        ;; This is the structured Superchat-prepared envelope,
                        ;; not a claim about provider-specific HTTP wire bytes.
                        ;; Tape owns exact encoding, redaction refusal, size,
                        ;; and hash.
                        (setq
                         snapshot-id
                         (funcall
                          runtime-observer 'decision-snapshot
                          (list
                           :attempt generation
                           :attempt-reason (symbol-name attempt-reason)
                           :provider provider
                           :model model
                           :prepared-envelope prepared-envelope)))
                        (unless (and (stringp snapshot-id)
                                     (not (string-empty-p snapshot-id)))
                          (error "Runtime did not return a snapshot event ID"))
                        (superchat--llm-set-prompt-tools
                         attempt-prompt
                         (superchat--llm-attempt-tools
                          attempt-prompt current-tools
                          (list
                           :evidence-contract evidence-contract
                           :evidence-attempt generation
                           :decision-snapshot-event-id snapshot-id
                           :attempt-active-p
                           (lambda ()
                             (and (= generation request-generation)
                                  (not completed)
                                  (funcall run-active-p)))
                           :stale-observer
                           (lambda (origin result)
                             (let ((superchat--llm-callback-attempt generation))
                               (funcall
                                record-first-output
                                (list
                                 :kind "stale-tool-call-blocked"
                                 :tool-name
                                 (plist-get origin :tool-call-name)
                                 :call-id
                                 (plist-get origin :tool-call-id)
                                 :arguments
                                 (copy-tree
                                  (plist-get origin :tool-call-arguments) t)
                                 :result result))))
                           :audit-failure-observer
                           (lambda (value)
                             (let ((superchat--llm-callback-attempt generation))
                               (ignore-errors
                                 (funcall
                                  record-first-output
                                  (list
                                   :kind "tool-origin-invalid"
                                   :tool-name
                                   (plist-get value :tool-name))))
                               (when (functionp runtime-observer)
                                 (funcall
                                 runtime-observer
                                  'audit-failure value))))))))
                    (superchat-llm-evidence-recording-error
                     (signal (car err) (cdr err)))
                    (error
                     (signal 'superchat-llm-evidence-recording-error
                             (list (error-message-string err))))))
                (let* ((attempt-stream-cb
                        (lambda (chunk)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (or (not evidence-contract)
                                      (and (= generation request-generation)
                                           (funcall run-active-p)))
                              (funcall stream-cb chunk)))))
                       (attempt-response-cb
                        (lambda (response)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (or (not evidence-contract)
                                      (and (= generation request-generation)
                                           (funcall run-active-p)))
                              (when evidence-contract
                                (setq real-prompt attempt-prompt))
                              (funcall response-cb response)))))
                       (attempt-error-cb
                        (lambda (type &rest more)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (or (not evidence-contract)
                                      (= generation request-generation))
                              (when evidence-contract
                                (setq real-prompt attempt-prompt))
                              (apply error-cb type more)))))
                     (request
                      (if streaming-request-p
                          (llm-chat-streaming effective-backend
                                              attempt-prompt
                                              attempt-stream-cb
                                              attempt-response-cb
                                              attempt-error-cb
                                              multi-output)
                        (llm-chat-async effective-backend
                                        attempt-prompt
                                        attempt-response-cb
                                        attempt-error-cb
                                        multi-output))))
                (when (and (= generation request-generation)
                           (functionp runtime-observer))
                  (funcall runtime-observer 'request request))
                (superchat--llm-watch-plz-request
                 request
                 (lambda ()
                   (and (not completed)
                        (funcall run-active-p))))
                  request)))))

      (condition-case err
          (funcall send-request 'initial)
        (superchat-llm-evidence-recording-error
         (signal (car err) (cdr err)))
        (error
         (if evidence-contract
             (signal 'superchat-llm-evidence-recording-error
                     (list (error-message-string err)))
           (funcall finalize
                    (format "[llm-chat-streaming error: %s]"
                            (error-message-string err))
                    'failed)))))))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
