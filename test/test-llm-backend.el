;;; test-llm-backend.el --- Tests for the v0.5 llm.el backend integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for superchat's llm.el backend, dispatchers, and parser.
;; No live API calls are made — `llm-chat', `llm-chat-async',
;; `llm-chat-streaming',
;; `llm-name', `llm-make-tool', and `llm-make-chat-prompt' are mocked
;; via `cl-letf' to the real v0.7+ signatures so the dispatcher's actual
;; call patterns are exercised:
;;
;;   (llm-chat          provider prompt &optional multi-output)
;;   (llm-chat-streaming provider prompt partial-cb response-cb error-cb
;;                       &optional multi-output)
;;   (llm-make-chat-prompt text :tools TOOLS)
;;   (llm-name          provider)
;;   (llm-make-tool     &rest args)
;;
;; Tests do not require llm.el to be installed in the host Emacs.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'llm nil t)

;; The canonical suite intentionally runs without optional llm.el.  Define
;; its boundary shapes before loading Superchat so cl-generic expansion sees
;; the same prompt/interaction slots as llm 0.31.1.
(unless (fboundp 'make-llm-chat-prompt)
  (eval
   '(cl-defstruct llm-chat-prompt
      context examples interactions tools temperature max-tokens
      response-format reasoning non-standard-params tool-options)))
(unless (fboundp 'make-llm-chat-prompt-interaction)
  (eval
   '(cl-defstruct llm-chat-prompt-interaction
      role content tool-results multi-turn-plist)))
(unless (fboundp 'make-llm-tool-options)
  (eval '(cl-defstruct llm-tool-options (tool-choice 'auto))))
(unless (fboundp 'make-llm-chat-prompt-tool-result)
  (eval
   '(cl-defstruct llm-chat-prompt-tool-result call-id tool-name result)))
(unless (fboundp 'make-llm-provider-utils-tool-use)
  (eval
   '(cl-defstruct llm-provider-utils-tool-use id name args)))
(unless (fboundp 'llm-provider-utils--normalize-args)
  (defun llm-provider-utils--normalize-args (args &optional false-value)
    "Test fallback for the llm 0.31.1 argument normalizer."
    (cl-subst false-value :json-false
              (cl-subst false-value :false (copy-tree args t)))))
(unless (fboundp 'llm-provider-populate-tool-uses)
  (defun llm-provider-populate-tool-uses (&rest _args)
    "Test fallback for the llm 0.31.1 pre-execution recording seam."))
(unless (get 'llm-tool-call-error 'error-conditions)
  (define-error 'llm-error "LLM error")
  (define-error 'llm-tool-call-error "LLM tool call error" 'llm-error))

(require 'superchat)
(require 'superchat-parser)
(require 'superchat-tools)

;;;---------------------------------------------
;;; Helpers
;;;---------------------------------------------

(defvar test-llm--native-make-tool
  (and (featurep 'llm) (symbol-function 'llm-make-tool)))
(defvar test-llm--native-raw-make-tool
  (and (featurep 'llm) (symbol-function 'make-llm-tool)))
(defvar test-llm--native-tool-name
  (and (featurep 'llm) (symbol-function 'llm-tool-name)))
(defvar test-llm--native-tool-description
  (and (featurep 'llm) (symbol-function 'llm-tool-description)))
(defvar test-llm--native-tool-args
  (and (featurep 'llm) (symbol-function 'llm-tool-args)))
(defvar test-llm--native-tool-function
  (and (featurep 'llm) (symbol-function 'llm-tool-function)))
(defvar test-llm--native-tool-async
  (and (featurep 'llm) (symbol-function 'llm-tool-async)))

(defun test-llm--make-tool (&rest args)
  "Return a native llm tool when available, otherwise a test double."
  (if test-llm--native-make-tool
      (apply test-llm--native-make-tool args)
    (cons 'mock-tool args)))

(defun test-llm--make-mock-backend (provider-sym)
  "Return a fake llm provider plist tagged with PROVIDER-SYM.
The result behaves like a `make-llm-openai'/`-claude'/etc. struct
well enough for `superchat-backend-show' and the cl-defgeneric
helpers to introspect it (via the mocked `llm-name')."
  (list 'mock-backend
        :chat-model "gpt-4o-mini"
        :provider provider-sym))

(ert-deftest test-provider-name-keeps-ollama-distinct-from-model ()
  "Ollama evidence names the provider, not its selected model."
  (unless (require 'llm-ollama nil t)
    (ert-skip "llm-ollama is not installed"))
  (should
   (equal "ollama"
          (superchat--provider-name
           (make-llm-ollama :chat-model "qwen3.5:9b")))))

(ert-deftest test-openai-compatible-preserves-response-tool-call-id ()
  "The compatible transport keeps provider identity and call identity apart."
  (unless (require 'llm-openai nil t)
    (ert-skip "llm-openai is not installed"))
  (let* ((provider
          (make-llm-openai-compatible
           :key "offline" :chat-model "qwen3.5:9b"
           :url "http://127.0.0.1:11434/v1/"))
         (response
          '((choices
             . [((message
                  . ((tool_calls
                      . [((id . "call_provider_E_R17")
                          (type . "function")
                          (function
                           . ((name . "refresh_handle")
                              (arguments . "{\"code\":\"E-R17\"}"))))]))))])))
         (use (car (llm-provider-extract-tool-uses provider response)))
         (stream-use
          (car
           (llm-provider-collect-streaming-tool-uses
            provider
            '[((index . 0) (id . "call_stream_E_R17")
               (function . ((name . "refresh_handle")
                            (arguments . "{\"code\":"))))
              ((index . 0)
               (function . ((arguments . "\"E-R17\"}"))))]))))
    (should (equal "open ai compatible"
                   (superchat--provider-name provider)))
    (should (equal "call_provider_E_R17"
                   (llm-provider-utils-tool-use-id use)))
    (should (equal "refresh_handle"
                   (llm-provider-utils-tool-use-name use)))
    (should (equal '((code . "E-R17"))
                   (llm-provider-utils-tool-use-args use)))
    (should (equal "call_stream_E_R17"
                   (llm-provider-utils-tool-use-id stream-use)))
    (should (equal '((code . "E-R17"))
                   (llm-provider-utils-tool-use-args stream-use)))))

(defun test-llm--with-mock-llm (body)
  "Run BODY with stub llm.el bindings for the v0.7+ real API.
Mocks `llm-make-tool', `llm-name', `llm-make-chat-prompt', `llm-chat',
`llm-chat-streaming', `superchat--provider-chat-model', and
`superchat-mcp-get-tools'.  The dispatchers in superchat.el then
exercise their actual call patterns against these mocks, so signature
mismatches are caught at test time."
  (cl-letf* (((symbol-function 'llm-make-tool)
              (or test-llm--native-make-tool
                  #'test-llm--make-tool))
             ((symbol-function 'make-llm-tool)
              (or test-llm--native-raw-make-tool
                  #'test-llm--make-tool))
             ((symbol-function 'llm-tool-name)
              (or test-llm--native-tool-name
                  (lambda (tool) (plist-get (cdr tool) :name))))
             ((symbol-function 'llm-tool-description)
              (or test-llm--native-tool-description
                  (lambda (tool) (plist-get (cdr tool) :description))))
             ((symbol-function 'llm-tool-args)
              (or test-llm--native-tool-args
                  (lambda (tool) (plist-get (cdr tool) :args))))
             ((symbol-function 'llm-tool-function)
              (or test-llm--native-tool-function
                  (lambda (tool) (plist-get (cdr tool) :function))))
             ((symbol-function 'llm-tool-async)
              (or test-llm--native-tool-async
                  (lambda (tool) (plist-get (cdr tool) :async))))
             ((symbol-function 'llm-name)
              (lambda (backend)
                (or (plist-get (cdr backend) :provider)
                    (car backend))))
             ;; In production this is dispatched via cl-defgeneric to
             ;; `(llm-openai-chat-model provider)' once `llm-openai' is
             ;; loaded.  Tests don't load llm-openai, so we install a
             ;; plist-aware fallback that the mock backends (which are
             ;; plain plists) can answer.
             ((symbol-function 'superchat--provider-chat-model)
              (lambda (provider)
                (and (consp provider)
                     (plist-get (cdr provider) :chat-model))))
             ((symbol-function 'llm-make-chat-prompt)
              (lambda (text &rest args)
                (list 'mock-prompt
                      :text text
                      :tools (plist-get args :tools))))
             ((symbol-function 'llm-chat)
              (lambda (_backend _prompt &optional _multi-output)
                "[mocked llm-chat response]"))
             ((symbol-function 'llm-capabilities)
              (lambda (_provider)
                '(streaming tool-use streaming-tool-use)))
             ((symbol-function 'llm-chat-streaming)
              (lambda (_provider _prompt _partial-cb _response-cb _error-cb
                       &optional _multi-output)
                nil))
             ((symbol-function 'superchat-mcp-get-tools)
              (lambda () nil)))
    (funcall body)))

;;;---------------------------------------------
;;; Defcustoms
;;;---------------------------------------------

(ert-deftest test-llm-backend-defaults ()
  "LLM defcustoms exist with their documented defaults."
  (should (boundp 'superchat-llm-backend))
  (should (boundp 'superchat-llm-model))
  (should (boundp 'superchat-llm-context-window-overrides))
  (should (boundp 'superchat-llm-output-token-reserve))
  (should (boundp 'superchat-llm-streaming))
  (should (boundp 'superchat-llm-tool-names))
  (should (null superchat-llm-backend))
  (should (null superchat-llm-model))
  (should (null superchat-llm-context-window-overrides))
  (should (= 2048 superchat-llm-output-token-reserve))
  (should (eq superchat-llm-streaming t))
  ;; The default surface is the five-verb core.  Everything else attaches
  ;; by scope — Capability presets, Topology, or extension activation.
  (should (equal superchat-llm-tool-names
                 '("list_buffers" "apropos" "read_buffer" "edit_buffer"
                   "eval_elisp")))
  (should-not (member "render_ui" superchat-llm-tool-names)))

(ert-deftest test-context-capability-reports-source-precedence ()
  "Overrides beat runtime discovery, which beats llm.el metadata."
  (let ((superchat-llm-context-window-overrides '(("model-x" . 8192))))
    (cl-letf (((symbol-function 'superchat--llm-lm-studio-context-window)
               (lambda (_provider _model)
                 (ert-fail "runtime discovery should not run for an override"))))
      (should (equal '(:limit 8192 :source override)
                     (superchat--llm-context-capability nil "model-x")))))
  (let ((superchat-llm-context-window-overrides nil))
    (cl-letf (((symbol-function 'superchat--llm-lm-studio-context-window)
               (lambda (_provider _model) 65536))
              ((symbol-function 'llm-chat-token-limit)
               (lambda (_provider) 32000)))
      (should (equal '(:limit 65536 :source lm-studio-runtime)
                     (superchat--llm-context-capability nil "model-x"))))
    (cl-letf (((symbol-function 'superchat--llm-lm-studio-context-window)
               (lambda (_provider _model) nil))
              ((symbol-function 'llm-chat-token-limit)
               (lambda (_provider) 32000)))
      (should (equal '(:limit 32000 :source llm)
                     (superchat--llm-context-capability nil "model-x"))))))

(ert-deftest test-lm-studio-context-window-uses-loaded-instance ()
  "LM Studio discovery reads the active instance's load-time window."
  (unless (require 'llm-openai nil t)
    (ert-skip "llm-openai is not installed"))
  (let ((provider
         (make-llm-openai-compatible
          :key "offline" :chat-model "qwen/qwen3.8-27b"
          :url "http://localhost:1234/v1/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "lms") "/fake/lms")))
              ((symbol-function 'call-process)
               (lambda (&rest _args)
                 (insert
                  "[{\"type\":\"llm\",\"identifier\":\"qwen/qwen3.8-27b\",\"modelKey\":\"qwen/qwen3.8-27b\",\"contextLength\":65536}]")
                 0)))
      (should (= 65536
                 (superchat--llm-lm-studio-context-window
                  provider "qwen/qwen3.8-27b"))))))

(ert-deftest test-context-overflow-response-detects-provider-fallbacks ()
  "Silent overflow is distinguished from ordinary output truncation."
  (should
   (superchat--llm-context-overflow-response-p
    '(:stop-reason stop :input-tokens 10001 :output-tokens 10) 10000))
  (should
   (superchat--llm-context-overflow-response-p
    '(:stop-reason length :input-tokens 9900 :output-tokens 0) 10000))
  (should-not
   (superchat--llm-context-overflow-response-p
    '(:stop-reason length :input-tokens 5000 :output-tokens 0) 10000))
  (should
   (superchat--llm-context-overflow-response-p
    '(:stop-reason length :text "") 10000)))

;;;---------------------------------------------
;;; superchat-backend-show
;;;---------------------------------------------

(ert-deftest test-backend-show-unconfigured ()
  "With `superchat-llm-backend' nil, /backend reports UNCONFIGURED."
  (let ((superchat-llm-backend nil))
    (let ((content (superchat-backend-show)))
      (should (stringp content))
      (should (string-match-p "UNCONFIGURED" content))
      (should (string-match-p "llm\\.el" content))
      (should (string-match-p "make-llm-openai" content)))))

(ert-deftest test-backend-show-with-mock-provider ()
  "With a mock backend, /backend reports the provider and model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai)))
       (let ((content (superchat-backend-show)))
         (should (string-match-p "openai" content))
         (should (string-match-p "gpt-4o-mini" content))
         (should (string-match-p "Streaming: yes" content)))))))

;;;---------------------------------------------
;;; superchat-model-list
;;;---------------------------------------------

(ert-deftest test-model-list-no-backend ()
  "Without a backend, /models still renders a useful help message."
  (let ((superchat-llm-backend nil)
        (superchat-manual-models nil))
    (let ((content (superchat-model-list)))
      (should (stringp content))
      (should (string-match-p "Available Models" content))
      (should (string-match-p "superchat-manual-models" content)))))

(ert-deftest test-model-list-with-manual-models ()
  "Manual model list is shown in /models output."
  (let ((superchat-llm-backend nil)
        (superchat-manual-models '("gpt-4o-mini" "claude-3-5-sonnet")))
    (let ((content (superchat-model-list)))
      (should (string-match-p "Manual configuration" content))
      (should (string-match-p "gpt-4o-mini" content))
      (should (string-match-p "claude-3-5-sonnet" content)))))

(ert-deftest test-model-list-with-backend ()
  "Configured backend's :chat-model is shown as the current model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-manual-models nil))
       (let ((content (superchat-model-list)))
         (should (string-match-p "Configured llm backend" content))
         (should (string-match-p "gpt-4o-mini" content)))))))

(ert-deftest test-model-list-override-model ()
  "`superchat-llm-model' overrides the backend's :chat-model."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-llm-model "gpt-4o")
           (superchat-manual-models nil))
       (let ((content (superchat-model-list)))
         (should (string-match-p "Current model: gpt-4o" content)))))))

(ert-deftest test-effective-backend-copies-openai-compatible-subclass ()
  "A model override preserves an OpenAI-compatible provider's subtype and key."
  (unless (require 'llm-deepseek nil t)
    (ert-skip "llm-deepseek is unavailable"))
  (let* ((key-function (lambda () "not-a-live-key"))
         (superchat-llm-backend
          (make-llm-deepseek
           :key key-function
           :chat-model "deepseek-v4-pro"))
         (superchat-llm-model nil)
         (effective
          (superchat--effective-llm-backend "deepseek-v4-flash")))
    (should (llm-deepseek-p effective))
    (should-not (eq effective superchat-llm-backend))
    (should (equal "deepseek-v4-flash"
                   (llm-deepseek-chat-model effective)))
    (should (equal "deepseek-v4-pro"
                   (llm-deepseek-chat-model superchat-llm-backend)))
    (should (eq key-function (llm-deepseek-key effective)))
    (should (equal "https://api.deepseek.com"
                   (llm-deepseek-url effective)))))

;;;---------------------------------------------
;;; superchat--is-ollama-backend-p
;;;---------------------------------------------

(ert-deftest test-ollama-detection-ollama-provider ()
  "An ollama provider is correctly identified as Ollama."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'ollama)))
       (should (superchat--is-ollama-backend-p))))))

(ert-deftest test-ollama-detection-non-ollama-provider ()
  "A non-ollama provider is correctly identified as NOT Ollama."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai)))
       (should-not (superchat--is-ollama-backend-p))))))

(ert-deftest test-ollama-detection-nil-backend ()
  "Nil backend is not an Ollama backend."
  (let ((superchat-llm-backend nil))
    (should-not (superchat--is-ollama-backend-p))))

;;;---------------------------------------------
;;; superchat-llm-tools-list
;;;---------------------------------------------

(ert-deftest test-tools-list-empty-when-llm-missing ()
  "When `llm-make-tool' is not fbound, the registry stays nil."
  (let ((superchat-llm-tools-list nil))
    (cl-letf (((symbol-function 'llm-make-tool) nil))
      (let ((result (superchat-get-llm-tools)))
        (should (null result))))))

(ert-deftest test-tools-list-populates-default-small-tool-surface ()
  "With a mock `llm-make-tool', reload registers the default small tool surface."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tool-names
            '("list_buffers" "apropos" "read_buffer" "edit_buffer"
              "eval_elisp")))
       (let ((tools (superchat-llm-tools-reload)))
         (should (listp tools))
         (should (= 5 (length tools)))
         ;; After reload, the cache var is populated (not nil).
         (should superchat-llm-tools-list)
         (should (= 5 (length superchat-llm-tools-list)))
         (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos"
                          "eval_elisp")
                        (mapcar #'llm-tool-name
                                superchat-llm-tools-list))))))))

(ert-deftest test-apropos-tool-has-discovery-schema ()
  "The apropos tool exposes a required pattern and optional type."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("apropos")))
       (let* ((tool (car (superchat-llm-tools-reload)))
              (args (llm-tool-args tool)))
         (should (equal '("pattern" "type")
                        (mapcar (lambda (arg) (plist-get arg :name)) args)))
         (should-not (plist-get (car args) :optional))
         (should (plist-get (cadr args) :optional))
         (should (eq #'superchat-tool-apropos
                     (llm-tool-function tool))))))))

(ert-deftest test-workspace-tool-exposes-artifact-revision-contract ()
  "Workspace writes expose and forward Artifact conflict arguments."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("workspace_write")))
       (let* ((tool (car (superchat-llm-tools-reload)))
              (args (llm-tool-args tool)))
         (should (equal '("content" "append" "artifact_id"
                          "expected_revision")
                        (mapcar (lambda (arg) (plist-get arg :name)) args)))))))
  (cl-letf (((symbol-function 'superchat-workspace-write)
             (lambda (&rest args) args)))
    (should (equal '("new" t "artifact-a" "revision-a")
                   (superchat-tool-workspace-write
                    "new" t "artifact-a" "revision-a")))))

(ert-deftest test-request-tool-profile-preserves-declared-order ()
  "An explicit Profile tool list is provider-visible in declared order."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tools-list nil)
            (names '("list_buffers" "read_buffer" "edit_buffer" "replace_buffer"
                     "eval_elisp" "eshell-command"
                     "workspace_read" "workspace_write" "workspace_info"))
            (tools (superchat-get-llm-tools names)))
       (should (equal names (mapcar #'llm-tool-name tools)))))))

(ert-deftest test-emit-plan-tool-uses-json-array-schema ()
  "The provider sees emit_plan steps as an array of string pairs."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool (car (superchat-get-llm-tools '("emit_plan"))))
            (steps (car (llm-tool-args tool)))
            (pair (plist-get steps :items)))
       (should (eq 'array (plist-get steps :type)))
       (should (eq 'array (plist-get pair :type)))
       (should (eq 'string
                   (plist-get (plist-get pair :items) :type)))))))

(ert-deftest test-request-tool-profile-rejects-unknown-name ()
  "An explicit Profile cannot silently lose a requested tool."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tool-name (symbol-function 'llm-tool-name)))
       (cl-letf (((symbol-function 'llm-tool-name)
                  (lambda (tool)
                    (unless tool
                      (signal 'wrong-type-argument '(llm-tool nil)))
                    (funcall tool-name tool))))
         (let ((err (should-error
                     (superchat-get-llm-tools '("missing-tool")))))
           (should (string-match-p
                    "Requested llm tool profile is not resolvable.*missing-tool"
                    (error-message-string err)))))))))

(ert-deftest test-on-demand-attaches-tools-for-render-ui-intent ()
  "A direct `render_ui' request gets tools without enabling them globally."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (should (superchat--should-attach-tools-p
             "Use render_ui to draw a comparison table."))
    (should-not (superchat--should-attach-tools-p
                 "Explain the difference between Redux and Zustand."))))

(ert-deftest test-on-demand-attaches-tools-for-eshell-intent ()
  "An explicit `eshell-command' request gets the command bridge on demand."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (should (superchat--should-attach-tools-p
             "Use eshell-command to run pwd."))
    (should (superchat--should-attach-tools-p
             "Use eshell command to list the current directory."))
    (should-not (superchat--should-attach-tools-p
                 "Explain what Eshell is."))))

(ert-deftest test-on-demand-attaches-tools-for-team-intent ()
  "Explicit team tool names attach the collaboration surface on demand."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil))
    (dolist (input '("Use team_task to create the work."
                     "Call team_message after the review."
                     "Use delegate_to_subagent for research."
                     "Use delegate_to_subagent_parallel for both tasks."))
      (should (superchat--should-attach-tools-p input)))
    (should-not (superchat--should-attach-tools-p
                 "Explain how agent teamwork could work."))))

(ert-deftest test-render-ui-intent-hides-synthesis-tools ()
  "An explicit render request keeps agent-written UI tools out of the turn."
  (let ((superchat-llm-tools-enabled 'on-demand)
        (superchat--current-command nil)
        (superchat-llm-synthesized-tools
         (list (test-llm--make-tool
                :name "render-state-comparison-vui")))
        (superchat--llm-tool-names-signature nil))
    (cl-letf (((symbol-function 'llm-tool-name)
               (or test-llm--native-tool-name
                   (lambda (tool) (plist-get (cdr tool) :name))))
             ((symbol-function 'superchat-get-llm-tools)
               (lambda (&optional _tool-names)
                 (list (test-llm--make-tool :name "render_ui")
                       (test-llm--make-tool :name "define_tool"))))
             ((symbol-function 'superchat-mcp-get-tools)
               (lambda () nil)))
      (should (equal '("render_ui")
                     (mapcar #'llm-tool-name
                             (superchat--collect-llm-tools
                              "Use render_ui with a button.")))))))

(ert-deftest test-tools-list-reloads-when-allowlist-changes ()
  "Changing `superchat-llm-tool-names' must invalidate the cached tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names '("read-file")))
       (superchat-get-llm-tools)
       (setq superchat-llm-tool-names
             '("read-file" "define_tool"))
       (should (member "define_tool"
                       (mapcar #'llm-tool-name
                               (superchat-get-llm-tools))))))))

(ert-deftest test-request-tool-profile-expands-global-allowlist ()
  "An explicit preset tool list should not require global configuration."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names '("read-file"))
           (superchat-llm-synthesized-tools
            (list (test-llm--make-tool :name "grep-subagent"))))
       (should
        (equal
         '("team_task" "delegate_to_subagent")
         (mapcar #'llm-tool-name
                 (superchat--collect-llm-tools
                  "team request"
                  '("team_task" "delegate_to_subagent")))))
       (should (equal superchat-llm-tool-names '("read-file")))))))

(ert-deftest test-tools-list-can-expose-all-builtins ()
  "The larger legacy tool surface remains available behind an explicit opt-in."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tools-list nil)
            (superchat-llm-tool-names 'all)
            (expected-count 31))
       (let ((tools (superchat-llm-tools-reload)))
         ;; `all' explicitly opts into every remaining built-in surface.
         (should (= expected-count (length tools)))
         (should (= expected-count (length superchat-llm-tools-list))))))))

(ert-deftest test-plan-progress-tool-attaches-only-when-plan-active ()
  "report_plan_progress joins the request surface only while the session's
approved plan exists."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-plan)
     (let ((superchat-llm-tools-list nil)
           (superchat-llm-tools-enabled 'always))
       (let ((names (mapcar #'llm-tool-name
                            (or (superchat--collect-llm-tools "hi" nil)
                                nil))))
         (should-not (member "report_plan_progress" names)))
       (let ((buf (generate-new-buffer "*plan-tool-attach*")))
         (unwind-protect
             (progn
               (with-current-buffer buf
                 (setq-local superchat--active-plan
                             (list :summary "s" :event-id "e"
                                   :steps nil)))
               (let ((superchat-buffer-name (buffer-name buf)))
                 (let ((names
                        (mapcar #'llm-tool-name
                                (or (superchat--collect-llm-tools "hi" nil)
                                    nil))))
                   (should (member "report_plan_progress" names)))))
           (kill-buffer buf)))))))

(ert-deftest test-eshell-command-executor-refusal-never-invokes-tool ()
  "The shared Executor never runs Eshell after user refusal."
  (cl-letf (((symbol-function 'superchat-tape-append-event)
             (lambda (_event) 1))
            ((symbol-function 'eshell-command-result)
             (lambda (&rest _args)
               (ert-fail "refused Eshell command was executed"))))
    (should
     (equal
      "[Tool call cancelled by user]"
      (superchat-tool-execute
       "eshell-command"
       #'superchat-tool-eshell-command
       '("pwd")
       :context
       '(:session-id "session-1"
         :attendance attended
         :approval-function ignore))))))

(ert-deftest test-eshell-command-uses-tool-directory-and-strips-properties ()
  "The Eshell bridge delegates in the effective tool directory."
  (let ((captured-directory nil)
        (superchat-tools-default-directory "/tmp"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'superchat--extend-timeout) #'ignore)
              ((symbol-function 'eshell-command-result)
               (lambda (_command status-var)
                 (set status-var 0)
                 (setq captured-directory default-directory)
                 (propertize "ok" 'face 'bold))))
      (should (equal "ok" (superchat-tool-eshell-command "pwd")))
      (should (equal "/tmp/" captured-directory)))))

(ert-deftest test-eshell-command-reports-nonzero-status ()
  "A failed Eshell command becomes model-correctable tool feedback."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
            ((symbol-function 'superchat--extend-timeout) #'ignore)
            ((symbol-function 'eshell-command-result)
             (lambda (_command status-var)
               (set status-var 127)
               "missing-command: command not found\n")))
    (let ((result (superchat-tool-eshell-command "missing-command")))
      (should (string-prefix-p "Error:" result))
      (should (string-match-p "status 127" result))
      (should (string-match-p "command not found" result)))))

(ert-deftest test-delegate-tool-discovers-custom-agent-presets ()
  "Delegate tool advertises custom agent skills, but not prompt skills."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (with-temp-file (expand-file-name "reviewer.md" tmp-dir)
               (insert "---\nname: reviewer\ndescription: Reviews patches\ntype: agent\n---\nReview carefully.\n"))
             (with-temp-file (expand-file-name "summarize.md" tmp-dir)
               (insert "---\nname: summarize\ndescription: Summarizes text\ntype: prompt\n---\nSummarize.\n"))
             (let* ((tools (superchat-llm-tools-reload))
                    (delegate (car tools))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "reviewer — Reviews patches" description))
               (should-not (string-match-p "summarize" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-delegate-tool-advertises-callable-skill-filename ()
  "Registry advertises the filename accepted by the sub-agent resolver."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (with-temp-file (expand-file-name "reviewer.md" tmp-dir)
               (insert "---\nname: fancy-name\ndescription: Reviews patches\ntype: agent\n---\nReview.\n"))
             (let* ((delegate (car (superchat-llm-tools-reload)))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "reviewer — Reviews patches" description))
               (should-not (string-match-p "fancy-name" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-delegate-tool-normalizes-mixed-case-and-builtin-collisions ()
  "Registry names remain callable on case-sensitive filesystems."
  (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t)))
    (unwind-protect
        (let ((superchat-skills-directory tmp-dir))
          (with-temp-file (expand-file-name "MyAgent.md" tmp-dir)
            (insert "---\nname: mixed\ndescription: Mixed case\ntype: agent\n---\nBody.\n"))
          (with-temp-file (expand-file-name "Researcher.md" tmp-dir)
            (insert "---\nname: collision\ndescription: Collision\ntype: agent\n---\nBody.\n"))
          (let ((registry (superchat--subagent-registry)))
            (should (assoc "myagent" registry))
            (should (= 1 (length (seq-filter
                                  (lambda (entry)
                                    (string= "researcher" (car entry)))
                                  registry))))
            (should (equal "mixed"
                           (superchat-preset-name
                            (superchat--subagent-preset "myagent"))))))
      (delete-directory tmp-dir t))))

(ert-deftest test-delegate-tool-cache-refreshes-after-agent-file-change ()
  "Cached delegate descriptions refresh when agent skills change."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((tmp-dir (make-temp-file "superchat-agent-registry-" t))
           (superchat-llm-tools-list nil)
           (superchat--subagent-registry-signature nil)
           (superchat-llm-tool-names '("delegate_to_subagent")))
       (unwind-protect
           (let ((superchat-skills-directory tmp-dir))
             (superchat-get-llm-tools)
             (with-temp-file (expand-file-name "new-agent.md" tmp-dir)
               (insert "---\nname: new-agent\ndescription: Newly added\ntype: agent\n---\nBody.\n"))
             (let* ((delegate (car (superchat-get-llm-tools)))
                    (description (llm-tool-description delegate)))
               (should (string-match-p "new-agent — Newly added" description))))
         (delete-directory tmp-dir t))))))

(ert-deftest test-tools-list-cached-after-first-call ()
  "`superchat-get-llm-tools' returns the cached list on subsequent calls."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil))
       (superchat-get-llm-tools)
       (let ((first-call superchat-llm-tools-list))
         (should first-call)
         (should (eq first-call (superchat-get-llm-tools))))))))

(ert-deftest test-build-prompt-uses-native-multipart-for-images ()
  "Image context is sent as unibyte `llm-media' in a multipart prompt."
  (let ((image (make-temp-file "superchat-image-" nil ".png"))
        captured-content captured-data)
    (unwind-protect
        (progn
          (with-temp-file image
            (set-buffer-multibyte nil)
            (insert (unibyte-string #x89 #x50 #x4e #x47)))
          (cl-letf (((symbol-function 'make-llm-media)
                     (lambda (&rest args)
                       (setq captured-data (plist-get args :data))
                       (cons 'media args)))
                    ((symbol-function 'llm-make-multipart)
                     (lambda (&rest parts) (cons 'multipart parts)))
                    ((symbol-function 'llm-make-chat-prompt)
                     (lambda (content &rest _args)
                       (setq captured-content content))))
            (superchat--build-llm-prompt
             "Describe this image" nil nil nil (list image)))
          (should (eq (car captured-content) 'multipart))
          (should (equal (cadr captured-content) "Describe this image"))
          (should (equal (plist-get (cdr (caddr captured-content)) :mime-type)
                         "image/png"))
          (should-not (multibyte-string-p captured-data)))
      (delete-file image))))

(ert-deftest test-tool-error-feedback-recognizes-tool-failure-markers ()
  "Common tool failure strings become a corrective model message."
  (let* ((result '(:tool-results
                   (("write-file" . "❌ Content too large for quick-write"))))
         (feedback (superchat--llm-tool-error-feedback result)))
    (should (stringp feedback))
    (should (string-match-p "write-file" feedback))
    (should (string-match-p "Content too large" feedback))
    (should (string-match-p "retry" feedback))))

;;;---------------------------------------------
;;; superchat--llm-generate-answer (async streaming)
;;;---------------------------------------------

(ert-deftest test-final-render-keeps-tool-transcript ()
  "Formatting the final answer must not erase tool records above it."
  (let ((superchat-buffer-name " *superchat-tool-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (insert "* User: inspect this\n")
          (setq superchat--response-start-marker (point-marker)
                superchat--assistant-response-start-marker nil)
          (insert "Assistant is thinking...\n")
          (insert "** Tool result: read-file\n#+begin_example\ncontents\n#+end_example\n")
          (superchat--prepare-assistant-response-area)
          (goto-char (point-max))
          (insert "raw final answer")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore)
                    ((symbol-function 'superchat--insert-prompt) #'ignore))
            (superchat--process-llm-result "formatted final answer"))
          (let ((rendered (buffer-string)))
            (should (string-match-p "Tool result: read-file" rendered))
            (should (string-match-p "contents" rendered))
            (should (string-match-p "formatted final answer" rendered))
            (should (< (string-match "Tool result: read-file" rendered)
                       (string-match "formatted final answer" rendered)))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-tool-round-p-continues-when-model-narrates ()
  "Prose alongside a tool call must NOT be mistaken for the final answer.
Models routinely narrate the call they are about to make (\"Let me read
that file first.\") in the same round as the tool use.  Treating that
narration as the final answer strands the tool result unseen and hands
the user the narration instead of an answer."
  ;; Tool call with narration -> still a tool round.
  (should (superchat--llm-tool-round-p
           '(:text "Let me read that file first."
             :tool-results (("read-file" . "contents")))))
  ;; Tool call with no prose -> tool round.
  (should (superchat--llm-tool-round-p
           '(:text "" :tool-results (("read-file" . "contents")))))
  ;; Bare tool-result alist (non-multi-output form) -> tool round.
  (should (superchat--llm-tool-round-p '((read-file . "contents"))))
  ;; Text and NO tool call -> this is the final answer; stop.
  (should-not (superchat--llm-tool-round-p '(:text "The answer is 42.")))
  (should-not (superchat--llm-tool-round-p "The answer is 42.")))

(ert-deftest test-async-dispatcher-continues-when-model-narrates ()
  "The loop keeps going when round 1 carries prose *and* a tool call."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if (= (length requests) 1)
                        (funcall response-cb
                                 '(:text "Let me read that file first."
                                   :tool-results (("fake-tool" . "tool result"))))
                      (funcall response-cb
                               '(:text "final answer after tool"))))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 (length requests)))
         (should (string= final-result "final answer after tool")))))))

(ert-deftest test-async-dispatcher-feeds-tool-error-back-to-llm ()
  "A failed tool result gets an explicit correction message on the next round."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")
                  (test-llm--make-tool :name "read_buffer")))
           (requests 0)
           (feedback nil)
           final-result)
       (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (if (= requests 1)
                        (funcall response-cb
                                 '(:text "I tried it"
                                   :tool-results
                                   (("render_ui" .
                                     "genui: :flex :direction 必须是 :row 或 :column"))))
                      (funcall response-cb "corrected answer")))))
         (superchat--llm-generate-answer
          "Use render_ui with vui-table"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 requests))
         (should (= 1 (length feedback)))
         (should (eq 'user (cadar feedback)))
         (should (string-match-p "render_ui" (caar feedback)))
         (should (string-match-p ":flex :direction" (caar feedback)))
         (should (string-match-p "retry\\|重试" (caar feedback)))
         (should (string= "corrected answer" final-result)))))))

(ert-deftest test-async-dispatcher-retries-after-tool-protocol-error ()
  "Malformed tool arguments are fed back through the same bounded retry path."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")))
           (requests 0)
           (feedback nil)
           final-result)
       (cl-letf (((symbol-function 'superchat--llm-drop-dangling-tool-use)
                  #'ignore)
                 ((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           error-cb &optional _multi-output)
                    (cl-incf requests)
                    (if (= requests 1)
                        (funcall error-cb
                                 'llm-tool-missing-argument
                                 "Missing required argument 'components' for tool 'render_ui'")
                      (funcall response-cb "corrected answer")))))
         (superchat--llm-generate-answer
          "Use render_ui"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 requests))
         (should (= 1 (length feedback)))
         (should (eq 'user (cadar feedback)))
         (should (string-match-p "components" (caar feedback)))
         (should (string-match-p "retry" (caar feedback)))
         (should (string= "corrected answer" final-result)))))))

(ert-deftest test-plz-success-callback-tool-errors-reach-error-callback ()
  "Tool parsing errors raised after HTTP success must not strand the Run."
  (let (observed)
    (superchat--llm-request-plz-propagate-callback-errors
     (lambda (_url &rest args)
       (funcall (plist-get args :on-success) 'response))
     "https://example.invalid"
     :on-success
     (lambda (_response)
       (signal 'llm-tool-call-error '("truncated JSON")))
     :on-error
     (lambda (type message)
       (setq observed (list type message))))
    (should (eq 'llm-tool-call-error (car observed)))
    (should (string-match-p "truncated JSON" (cadr observed)))))

(ert-deftest test-plz-coding-unchanged-without-ollama-request ()
  "The shared plz advice must not alter unrelated process coding."
  (let ((process (make-pipe-process :name "ert-plz-coding" :noquery t
                                    :coding 'binary)))
    (unwind-protect
        (let ((before (process-coding-system process))
              (superchat--llm-ollama-response-p nil))
          (should (eq process
                      (superchat--llm-request-plz-propagate-callback-errors
                       (lambda (_url &rest _args) process)
                       "https://example.invalid")))
          (should (equal before (process-coding-system process))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest test-async-dispatcher-continues-after-blocked-tool ()
  "A user/policy refusal does not disable unrelated later tool rounds."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "render_ui")))
           (requests 0)
           (multi-output-values nil)
           (feedback nil)
           tools-disabled
           final-result)
       (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt message &optional role)
                    (push (list message role) feedback)))
                 ((symbol-function 'superchat--llm-disable-prompt-tools)
                  (lambda (_prompt) (setq tools-disabled t)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional multi-output)
                    (cl-incf requests)
                    (push multi-output multi-output-values)
                    (pcase requests
                      (1
                       (funcall response-cb
                                '(:tool-results
                                  (("render_ui" .
                                    "[Tool call denied by permission hook]")))))
                      (2
                       (funcall response-cb
                                '(:tool-results
                                  (("read_buffer" . "safe contents")))))
                      (_ (funcall response-cb "acknowledged"))))))
         (superchat--llm-generate-answer
          "Use render_ui"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 3 requests))
         (should (equal '(t t t) multi-output-values))
         (should-not tools-disabled)
         (should (= 1 (length feedback)))
         (should
          (cl-some (lambda (entry)
                     (string-match-p "do not retry" (car entry)))
                   feedback))
         (should (string= "acknowledged" final-result)))))))

(ert-deftest test-render-keeps-tool-transcript-when-model-narrates ()
  "Narration before a tool call must not drag the tool records into the
final rewrite.  The assistant marker is anchored at the run's first text
chunk, so a tool record appended after that narration sits inside the
region `superchat--process-llm-result' deletes — unless the round's text
is closed out when the tool record lands."
  (let ((superchat-buffer-name " *superchat-narrate-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (insert "* User: inspect this\n")
          (setq superchat--response-start-marker (point-marker)
                superchat--assistant-response-start-marker nil
                superchat--current-response-parts nil)
          (insert "Assistant is thinking...\n")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore)
                    ((symbol-function 'superchat--insert-prompt) #'ignore))
            ;; Round 1: the model narrates, THEN calls a tool.
            (superchat--stream-llm-result "Let me read that file first.")
            (superchat--agent-render-tool-call "read-file" '("README.md"))
            (superchat--agent-render-tool-result "read-file" "contents")
            ;; Round 2: the model answers.
            (superchat--stream-llm-result "raw final answer")
            (superchat--process-llm-result "The answer is 42."))
          (let ((rendered (buffer-string)))
            (should (string-match-p "Let me read that file first." rendered))
            (should (string-match-p "Tool call: read-file" rendered))
            (should (string-match-p "contents" rendered))
            (should (string-match-p "The answer is 42." rendered))
            ;; Order: narration -> tool record -> final answer.
            (should (< (string-match "Let me read that file first." rendered)
                       (string-match "Tool call: read-file" rendered)))
            (should (< (string-match "Tool call: read-file" rendered)
                       (string-match "The answer is 42." rendered)))
            ;; The raw streamed text of the last round is rewritten, not kept.
            (should-not (string-match-p "raw final answer" rendered))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-async-dispatcher-streams-and-finalizes ()
  "Async dispatcher streams chunks via partial-cb and finalizes with
the authoritative text from response-cb (not the joined chunks)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil)
           (streamed-chunks '()))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda () nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial-cb response-cb
                           _error-cb &optional _multi-output)
                    ;; Partial: push chunks in real streaming order.
                    (funcall partial-cb "Hello, ")
                    (funcall partial-cb "world")
                    ;; Response: hand the dispatcher the authoritative
                    ;; final text.  This is what llm.el does for a
                    ;; non-tool call.
                    (funcall response-cb "streamed result"))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (chunk) (push chunk streamed-chunks))
          nil
          nil)
         ;; Authoritative final text wins over the chunk join.
         (should (string= final-result "streamed result"))
         ;; Streamed chunks are still delivered to the user for UX.
         (should (= 2 (length streamed-chunks)))
         (should (member "Hello, " streamed-chunks))
         (should (member "world" streamed-chunks)))))))

(ert-deftest test-async-dispatcher-falls-back-for-nonstreaming-tool-use ()
  "Tool requests use `llm-chat-async' unless streaming tool use is advertised."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'deepseek))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (async-calls 0)
           (streaming-calls 0)
           final-result)
       (cl-letf (((symbol-function 'llm-capabilities)
                  (lambda (_provider) '(streaming tool-use)))
                 ((symbol-function 'llm-chat-async)
                  (lambda (_provider _prompt response-cb _error-cb
                           &optional _multi-output)
                    (cl-incf async-calls)
                    (if (= async-calls 1)
                        (funcall response-cb
                                 '(:tool-results
                                   (("fake-tool" . "tool result"))))
                      (funcall response-cb "final answer"))))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (&rest _args)
                    (cl-incf streaming-calls))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          #'ignore)
         (should (= 2 async-calls))
         (should (= 0 streaming-calls))
         (should (equal "final answer" final-result)))))))

(ert-deftest test-async-dispatcher-restores-originating-view ()
  "Delayed callbacks should run in the View that started the request."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((target-name (generate-new-buffer-name " *superchat-origin*"))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            partial-cb response-cb seen)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-get-llm-tools)
                      (lambda () nil))
                     ((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt partial response _error
                               &optional _multi-output)
                        (setq partial-cb partial
                              response-cb response))))
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (superchat--llm-generate-answer
                  "hello"
                  (lambda (_answer)
                    (push (list :final (buffer-name)
                                superchat-buffer-name)
                          seen))
                  (lambda (_chunk)
                    (push (list :stream (buffer-name)
                                superchat-buffer-name)
                          seen)))))
             ;; Simulate llm.el invoking callbacks after the initiating
             ;; dynamic binding has unwound and another buffer is current.
             (with-temp-buffer
               (funcall partial-cb "chunk")
               (funcall response-cb "done"))
             (should (equal (nreverse seen)
                            `((:stream ,target-name ,target-name)
                              (:final ,target-name ,target-name)))))
         (when (get-buffer target-name)
           (kill-buffer target-name)))))))

(ert-deftest test-async-dispatcher-reports-latest-runtime-request ()
  "Runtime observes each live request handle and one terminal outcome."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
           (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (calls 0)
           responses errors events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (memq event '(request terminal))
                  (push (list event value) events)))))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial-cb response-cb
                             error-cb &optional _multi-output)
                      (cl-incf calls)
                      (push response-cb responses)
                      (push error-cb errors)
                      (intern (format "request-%d" calls)))))
           (superchat--llm-generate-answer
            "use the tool" #'ignore #'ignore)
           (funcall (car responses)
                    '(:tool-results (("fake-tool" . "result"))))
           (funcall (car responses) "final")
           ;; A provider callback arriving after completion is ignored.
           (funcall (car errors) 'error "late")
           (should
            (equal
             (nreverse events)
             '((request request-1)
               (request request-2)
               (terminal (:status completed :response "final")))))))))))

(ert-deftest test-async-dispatcher-reports-attempt-and-context-usage ()
  "Transport observations expose attempt estimates and provider usage."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-output-token-reserve 1024)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (memq event '(attempt-started usage terminal))
                  (push (list event value) events)))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model)
                      '(:limit 8192 :source llm)))
                   ((symbol-function 'superchat--llm-estimate-context-tokens)
                    (lambda (_provider _prompt) 6800))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt partial-cb response-cb
                             _error-cb &optional _multi-output)
                      (funcall partial-cb
                               '(:input-tokens 7000 :output-tokens 500))
                      (funcall response-cb '(:text "done"))
                      'request-1)))
           (superchat--llm-generate-answer
            "hello" #'ignore #'ignore)
           (should
            (equal
             (nreverse events)
             '((attempt-started
                (:attempt 1 :attempt-reason initial
                 :model "gpt-4o-mini" :context-used 6800
                 :output-reserve 1024
                 :context-limit 8192 :context-limit-source llm))
               (usage
                (:attempt 1 :input-tokens 7000 :output-tokens 500))
               (terminal (:status completed :response "done")))))))))))

(ert-deftest test-llm-context-budget-preflight-gates-provider ()
  "An over-budget attempt fails before invoking the provider transport."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'never)
           (superchat-llm-output-token-reserve 2048)
           (max-tokens nil)
           (provider-calls 0)
           events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (memq event '(attempt-started terminal))
                  (push (list event value) events)))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model) '(:limit 10000)))
                   ((symbol-function 'superchat--llm-estimate-context-tokens)
                    (lambda (_provider _prompt) 9000))
                   ((symbol-function 'llm-make-chat-prompt)
                    (lambda (_text &rest _args)
                      (make-llm-chat-prompt :max-tokens max-tokens)))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial response _error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (funcall response "within budget")
                      'provider-request)))
           (superchat--llm-generate-answer "too large" #'ignore #'ignore)
           (should (= 0 provider-calls))
           (should (equal
                    (nreverse events)
                    '((terminal
                       (:status failed
                        :response
                        "[llm context budget exceeded: used 9000 + reserve 2048 > limit 10000; request was not sent; compact and retry]")))))
           (setq events nil)
           (let ((superchat-llm-context-budget-exceeded-functions
                  (list (lambda (&rest _args)
                          (error "budget hook failed")))))
             (superchat--llm-generate-answer
              "hook failure" #'ignore #'ignore)
             (should (equal
                      (nreverse events)
                      '((terminal
                         (:status failed
                          :response
                          "[llm context budget exceeded: used 9000 + reserve 2048 > limit 10000; request was not sent; compact and retry]")))))
             (should (= 0 provider-calls)))
           (setq events nil)
           (let ((superchat--llm-evidence-context
                  '(:contract "epistemic-learning-v0"
                    :context-envelope
                    (:contract "context-continuity-v0"
                     :status satisfied :receipt (:tape-head 1))))
                 (superchat-llm-context-budget-exceeded-functions
                  (list (lambda (&rest _args)
                          (error "budget hook failed"))))
                 (raised nil))
             (condition-case err
                 (superchat--llm-generate-answer
                  "evidence hook failure" #'ignore #'ignore)
               (error (setq raised err)))
             (should-not raised)
             (should (equal
                      (nreverse events)
                      '((terminal
                         (:status failed
                          :response
                          "[llm context budget exceeded: used 9000 + reserve 2048 > limit 10000; request was not sent; compact and retry]")))))
             (should (= 0 provider-calls)))
           (setq max-tokens 1000)
           (superchat--llm-generate-answer
            "fits with override" #'ignore #'ignore)
           (should (= 1 provider-calls))))))))

(ert-deftest test-llm-context-budget-compacts-and-resumes-same-run ()
  "A claimed budget recovery rebuilds the prompt before its first request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'never)
           (superchat-llm-output-token-reserve 2048)
           (superchat--llm-context-envelope '(:prompt "OLD CONTEXT"))
           (estimates '(9000 1000))
           (provider-calls 0)
           final provider-text recovery-reason events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (memq event '(context-budget-exceeded
                                    context-recovery-started
                                    attempt-started terminal))
                  (push (list event value) events))))
             (superchat-llm-context-budget-exceeded-functions
              (list
               (lambda (details)
                 (setq recovery-reason (plist-get details :reason))
                 (funcall (plist-get details :resume-function)
                          "NEW CONTEXT")
                 t))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model) '(:limit 10000)))
                   ((symbol-function 'superchat--llm-estimate-context-tokens)
                    (lambda (_provider _prompt)
                      (prog1 (car estimates)
                        (setq estimates (cdr estimates)))))
                   ((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest _args)
                      (make-llm-chat-prompt
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text)))))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider prompt _partial response _error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (setq provider-text
                            (llm-chat-prompt-interaction-content
                             (car (llm-chat-prompt-interactions prompt))))
                      (funcall response "done")
                      'provider-request)))
           (superchat--llm-generate-answer
            "before\nOLD CONTEXT\nafter"
            (lambda (result) (setq final result)) #'ignore)
           (should (eq 'threshold recovery-reason))
           (should (= 1 provider-calls))
           (should (equal "done" final))
           (should (string-match-p "NEW CONTEXT" provider-text))
           (should-not (string-match-p "OLD CONTEXT" provider-text))
           (should
            (equal '(context-budget-exceeded context-recovery-started
                      attempt-started terminal)
                   (mapcar #'car (nreverse events))))))))))

(ert-deftest test-llm-pause-defers-provider-and-resumes-pending-request ()
  "A pause checkpoint must defer the provider call until its continuation runs."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'never)
           (superchat--pause-requested t)
           (superchat--pending-round-reason nil)
           (superchat--pending-send-request nil)
           (provider-calls 0)
           (boundary-calls 0)
           final)
       (cl-letf (((symbol-function 'superchat--pause-boundary)
                  (lambda () (cl-incf boundary-calls)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-callback response-callback
                           _error-callback &optional _multi-output)
                    (cl-incf provider-calls)
                    (funcall response-callback "resumed")
                    'provider-request)))
         (superchat--llm-generate-answer
          "pause before sending"
          (lambda (result) (setq final result))
          #'ignore)
         (should (= 0 provider-calls))
         (should (= 1 boundary-calls))
         (should (eq 'initial superchat--pending-round-reason))
         (should (functionp superchat--pending-send-request))
         (setq superchat--pause-requested nil)
         (funcall superchat--pending-send-request
                  superchat--pending-round-reason)
         (should (= 1 provider-calls))
         (should (equal "resumed" final)))))))

(ert-deftest test-llm-cancelled-pause-state-allows-next-request ()
  "A cancelled pause must not block the next request in the same View."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'never)
           (superchat--pause-requested t)
           (superchat--pending-round-reason nil)
           (superchat--pending-send-request nil)
           (provider-calls 0)
           final)
       (cl-letf (((symbol-function 'superchat--pause-boundary) #'ignore)
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-callback response-callback
                           _error-callback &optional _multi-output)
                    (cl-incf provider-calls)
                    (funcall response-callback "next request")
                    'provider-request)))
         (superchat--llm-generate-answer
          "pause before cancellation" #'ignore #'ignore)
         (should (= 0 provider-calls))
         (superchat--clear-pause-state)
         (superchat--llm-generate-answer
          "next request" (lambda (result) (setq final result)) #'ignore)
         (should (= 1 provider-calls))
         (should (equal "next request" final))
         (should-not superchat--pause-requested)
         (should-not superchat--pending-send-request))))))

(ert-deftest test-llm-resume-rejects-dead-run-even-with-empty-steer ()
  "Empty resume must not invoke a continuation for a dead Run."
  (let ((superchat-buffer-name (generate-new-buffer-name " *superchat-resume-dead*"))
        (superchat-response-timeout nil)
        (messages nil)
        (invoked nil))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq-local superchat--pause-requested t
                      superchat--active-run-id "dead-run"
                      superchat--pending-round-reason 'tool-followup
                      superchat--pending-send-request
                      (lambda (_reason) (setq invoked t)))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _args) ""))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (superchat-resume))
          (should-not invoked)
          (should-not superchat--pause-requested)
          (should-not superchat--pending-send-request)
          (should (cl-some (lambda (text)
                             (string-match-p "no longer active" text))
                           messages)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-llm-resume-cleans-state-after-continuation-error ()
  "A synchronous resume error must not strand the pending continuation."
  (let ((superchat-buffer-name (generate-new-buffer-name " *superchat-resume-error*"))
        (superchat-response-timeout nil)
        (messages nil)
        raised)
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (setq-local superchat--pause-requested t
                      superchat--active-run-id "live-run"
                      superchat--pending-round-reason 'tool-followup
                      superchat--pending-send-request
                      (lambda (_reason) (error "resume boom")))
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("live-run")))
                    ((symbol-function 'read-string)
                     (lambda (&rest _args) ""))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (condition-case err
                (superchat-resume)
              (error (setq raised err))))
          (should-not raised)
          (should-not superchat--pause-requested)
          (should-not superchat--pending-send-request)
          (should-not (and (boundp 'superchat--resume-timeout-timer)
                           (timerp superchat--resume-timeout-timer)))
          (should (cl-some (lambda (text)
                             (string-match-p "resume boom" text))
                           messages)))
      (when (timerp (and (boundp 'superchat--resume-timeout-timer)
                         superchat--resume-timeout-timer))
        (cancel-timer superchat--resume-timeout-timer))
      (when (timerp (and (boundp 'superchat--active-timeout-timer)
                         superchat--active-timeout-timer))
        (cancel-timer superchat--active-timeout-timer))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-llm-provider-overflow-compacts-and-retries-once ()
  "Provider errors and empty length stops retry the interrupted Run once."
  (test-llm--with-mock-llm
   (lambda ()
     (dolist (overflow-shape '(error empty-length))
       (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'never)
           (superchat--llm-context-envelope '(:prompt "OLD CONTEXT"))
           (provider-calls 0)
           provider-texts final recovery-reason events)
       (let ((superchat--llm-runtime-observer
              (lambda (event _value)
                (when (memq event '(attempt-started
                                    context-recovery-started terminal))
                  (push event events))))
             (superchat-llm-context-budget-exceeded-functions
              (list
               (lambda (details)
                 (setq recovery-reason (plist-get details :reason))
                 (funcall (plist-get details :resume-function)
                          "NEW CONTEXT")
                 t))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model) '(:limit 10000)))
                   ((symbol-function 'superchat--llm-estimate-context-tokens)
                    (lambda (_provider _prompt) 1000))
                   ((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest _args)
                      (make-llm-chat-prompt
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text)))))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider prompt _partial response error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (push (copy-sequence
                             (llm-chat-prompt-interaction-content
                              (car (llm-chat-prompt-interactions prompt))))
                            provider-texts)
                      (if (= provider-calls 1)
                          (if (eq overflow-shape 'error)
                              (funcall error 'llm-context-length-exceeded
                                       "provider rejected the request")
                            (funcall response
                                     '(:text "" :stop-reason length)))
                        (funcall response "recovered"))
                      (intern (format "request-%d" provider-calls)))))
           (superchat--llm-generate-answer
            "before\nOLD CONTEXT\nafter"
            (lambda (result) (setq final result)) #'ignore)
           (should (eq 'overflow recovery-reason))
           (should (= 2 provider-calls))
           (should (equal "recovered" final))
           (should (string-match-p "OLD CONTEXT" (cadr provider-texts)))
           (should (string-match-p "NEW CONTEXT" (car provider-texts)))
           (should (equal '(attempt-started context-recovery-started
                            attempt-started terminal)
                          (nreverse events))))))))))

(ert-deftest test-async-dispatcher-records-exact-prior-before-provider ()
  "Eligible transport reports actual text, model and first output."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat--llm-evidence-context
             '(:contract "epistemic-learning-v0"
               :context-envelope (:contract "context-continuity-v0"
                                  :status satisfied
                                  :receipt (:tape-head 9)
                                  :experience-view
                                  (:contract "experience-view-v0"
                                   :status satisfied
                                   :receipt
                                   (:candidate
                                    (:session-id "candidate"
                                     :event-id "evt-candidate")
                                    :promotion
                                    (:session-id "promotion"
                                     :event-id "evt-promotion"))
                                   :experience-action-policy
                                   (:dependent-tools ("workspace_read"))))))
            provider-prompt timeline evidence-events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (push (list event (copy-tree value)) evidence-events)
                (setq timeline (append timeline (list event)))
                (when (eq event 'decision-snapshot) "snapshot-1"))))
         (cl-letf (((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest args)
                      (make-llm-chat-prompt
                       :context (plist-get args :context)
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text))
                       :tools (plist-get args :tools))))
                   ((symbol-function 'llm-provider-chat-request)
                    (lambda (_provider prompt _streaming)
                      (setf (llm-chat-prompt-temperature prompt) 0.42)
                      'serialized))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider prompt partial-cb response-cb _error-cb
                             &optional _multi-output)
                      (setq provider-prompt prompt
                            timeline (append timeline '(provider-called)))
                      (funcall partial-cb "A")
                      (funcall partial-cb "AB")
                      (funcall response-cb "AB")
                      'provider-request)))
           (superchat--llm-generate-answer
            "exact prompt" #'ignore #'ignore nil nil nil "exact system")
           (let* ((events (nreverse evidence-events))
                  (snapshot (cadr (assq 'decision-snapshot events)))
                  (first (cadr (assq 'first-output events))))
             (should (< (cl-position 'decision-snapshot timeline)
                        (cl-position 'provider-called timeline)))
             (let ((prepared (plist-get snapshot :prepared-envelope)))
               (should
                (equal "exact prompt"
                       (plist-get (aref (plist-get prepared :messages) 0)
                                  :content)))
               (should
                (equal "exact system"
                       (plist-get prepared :system-prompt)))
               (should
                (equal
                 (plist-get
                  (plist-get superchat--llm-evidence-context
                             :context-envelope)
                  :experience-view)
                 (plist-get (plist-get prepared :view-envelope)
                            :experience-view)))
               (should
                (equal '(:dependent-tools ("workspace_read"))
                       (plist-get (plist-get prepared :view-envelope)
                                  :experience-action-policy)))
               (should (= 0 (length (plist-get prepared :tools)))))
             (should-not
              (plist-get
               (plist-get
                (plist-get snapshot :prepared-envelope) :options)
               :tool-choice))
             (should (equal "exact system"
                            (llm-chat-prompt-context provider-prompt)))
             (should (equal "gpt-4o-mini" (plist-get snapshot :model)))
             (should-not (llm-chat-prompt-tools provider-prompt))
             (should (= 0.42 (llm-chat-prompt-temperature provider-prompt)))
             (should (equal '(:attempt 1 :kind "text" :text "A") first)))))))))

(ert-deftest test-async-dispatcher-isolates-tool-bearing-attempts ()
  "Eligible attempts snapshot structured tool history without sharing prompts."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-effects 0)
            (origins nil)
            (superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-tools-prepared-p t)
           (superchat--llm-tool-wrapper #'identity)
           (superchat--llm-prepared-tools
            (list (test-llm--make-tool
                   :name "lookup" :description "Look up one key"
                   :args '((:name "key" :type string
                            :description "Lookup key"))
                   :function
                   (lambda (_key)
                     (cl-incf tool-effects)
                     (push (copy-tree superchat--llm-tool-origin t) origins)
                     "value"))))
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0" :status satisfied
               :receipt (:tape-head 1))))
           prompts snapshots evidence-events second-response
           (calls 0) final-result)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (push (cons event (copy-tree value t)) evidence-events)
                (when (eq event 'decision-snapshot)
                  (setq snapshots
                        (append snapshots (list (copy-tree value t))))
                  (format "snapshot-%d" (plist-get value :attempt))))))
       (cl-letf (((symbol-function 'llm-make-chat-prompt)
                  (lambda (text &rest args)
                    (make-llm-chat-prompt
                     :context (plist-get args :context)
                     :interactions
                     (list (make-llm-chat-prompt-interaction
                            :role 'user :content text))
                     :tools (plist-get args :tools))))
                 ((symbol-function 'llm-provider-chat-request)
                  (lambda (_provider prompt _streaming)
                    (setf (llm-chat-prompt-temperature prompt) 0.27)
                    (setf (llm-chat-prompt-tool-options prompt)
                          (make-llm-tool-options :tool-choice "lookup"))
                    'serialized))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial response _error
                           &optional _multi-output)
                    (cl-incf calls)
                    (setq prompts (append prompts (list prompt)))
                    (if (= calls 1)
                        (progn
                          (setf
                           (llm-chat-prompt-interactions prompt)
                           (append
                            (llm-chat-prompt-interactions prompt)
                            (list
                             (make-llm-chat-prompt-interaction
                              :role 'assistant
                              :content
                              (list
                               (make-llm-provider-utils-tool-use
                                :id "call-1" :name "lookup"
                                :args '((key . "alpha"))))
                              :multi-turn-plist
                              '(:openai-reasoning-id "reason-1"))
                             (make-llm-chat-prompt-interaction
                              :role 'tool-results :content nil
                              :tool-results
                              (list
                               (make-llm-chat-prompt-tool-result
                                :call-id "call-1" :tool-name "lookup"
                                :result "value"))))))
                          (funcall response
                                   '(:tool-results (("lookup" . "value")))))
                      (setq second-response response))
                    (intern (format "request-%d" calls)))))
         (superchat--llm-generate-answer
          "use tool" (lambda (value) (setq final-result value)) #'ignore)
         (should (= 2 calls))
         (should-not final-result)
         (should-not (eq (nth 0 prompts) (nth 1 prompts)))
         (should-not
          (eq (llm-chat-prompt-tools (nth 0 prompts))
              (llm-chat-prompt-tools (nth 1 prompts))))
         (should-not
          (eq (llm-chat-prompt-interactions (nth 0 prompts))
              (llm-chat-prompt-interactions (nth 1 prompts))))
         (should (= 0.27 (llm-chat-prompt-temperature (nth 0 prompts))))
         (should
          (= 0.27
             (plist-get
              (plist-get
               (plist-get (nth 0 snapshots) :prepared-envelope) :options)
              :temperature)))
         (should
          (equal
           "lookup"
           (plist-get
            (plist-get
             (plist-get (nth 0 snapshots) :prepared-envelope) :options)
            :tool-choice)))
         (let* ((second
                 (plist-get (nth 1 snapshots) :prepared-envelope))
                (messages (plist-get second :messages))
                (assistant (aref messages 1))
                (tool-results (aref messages 2)))
           (should (= 1 (length (plist-get second :tools))))
           (should (equal "call-1"
                          (plist-get
                           (aref (plist-get assistant :content) 0) :call-id)))
           (should
            (equal "reason-1"
                   (plist-get (plist-get assistant :multi-turn)
                              :openai-reasoning-id)))
           (should (equal "value"
                          (plist-get
                           (aref (plist-get tool-results :tool-results) 0)
                           :result))))
         ;; Simulate llm.el receiving a second-round tool call.  The wrapper
         ;; must bind attempt 2 directly, not call through attempt 1's wrapper.
         (setf
          (llm-chat-prompt-interactions (nth 1 prompts))
          (append
           (llm-chat-prompt-interactions (nth 1 prompts))
           (list
            (make-llm-chat-prompt-interaction
             :role 'assistant
             :content
             (list
              (make-llm-provider-utils-tool-use
               :id "call-2" :name "lookup" :args '((key . "beta"))))))))
         (should
          (equal "value"
                 (funcall
                  (llm-tool-function
                   (car (llm-chat-prompt-tools (nth 1 prompts))))
                  "beta")))
         (should (= 1 tool-effects))
         (should (= 2 (plist-get (car origins) :evidence-attempt)))
         (should
          (equal "snapshot-2"
                 (plist-get (car origins) :decision-snapshot-event-id)))
         ;; A late attempt-1 provider response must be rejected by its cloned
         ;; function before the request-level wrapper or Authority can run.
         (setf (llm-chat-prompt-interactions (nth 0 prompts))
               (butlast (llm-chat-prompt-interactions (nth 0 prompts))))
         (should
          (string-match-p
           "stale provider attempt"
           (funcall
            (llm-tool-function
             (car (llm-chat-prompt-tools (nth 0 prompts))))
            "alpha")))
         (should (= 1 tool-effects))
         (should
          (cl-find-if
           (lambda (entry)
             (and (eq (car entry) 'first-output)
                  (= 1 (plist-get (cdr entry) :attempt))
                  (equal "stale-tool-call-blocked"
                         (plist-get (cdr entry) :kind))))
           evidence-events))
         (funcall second-response "done")
         (should (equal "done" final-result))))))))

(ert-deftest test-async-dispatcher-origin-skips-malformed-same-name-use ()
  "A skipped same-name use cannot lend its identity to a later Action."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((effect-count 0)
            origin errors
            (tool
             (test-llm--make-tool
              :name "lookup"
              :args '((:name "x" :type string))
              :function
              (lambda (_x)
                (cl-incf effect-count)
                (setq origin (copy-tree superchat--llm-tool-origin t))
                "ok")))
            (uses
             (list
              (make-llm-provider-utils-tool-use
               :id "call-bad" :name "lookup"
               :args '((x . "same") (extra . "bad")))
              (make-llm-provider-utils-tool-use
               :id "call-good" :name "lookup"
               :args '((x . "same")))))
            (prompt
             (make-llm-chat-prompt
              :interactions
              (list (make-llm-chat-prompt-interaction
                     :role 'assistant :content uses))))
            (wrapped
             (car
              (superchat--llm-attempt-tools
               prompt (list tool)
               '(:evidence-contract "epistemic-learning-v0"
                 :evidence-attempt 1
                 :decision-snapshot-event-id "snapshot-1"
                 :attempt-active-p always)))))
       (setf (llm-chat-prompt-tools prompt) (list wrapped))
       (if (fboundp 'llm-provider-utils-execute-tool-uses)
           (cl-letf (((symbol-function 'llm-provider-populate-tool-uses)
                      #'ignore))
             (llm-provider-utils-execute-tool-uses
              nil prompt uses nil nil #'ignore
              (lambda (type &rest message)
                (push (cons type message) errors))))
         ;; Canonical -Q has no optional llm package.  This is the call that
         ;; llm 0.31.1 makes after skipping the malformed first use.
         (funcall (llm-tool-function wrapped) "same"))
       (should (= 1 effect-count))
       (should (equal "call-good" (plist-get origin :tool-call-id)))
       (should (= 2 (plist-get origin :tool-call-ordinal)))
       (should (equal '((x . "same"))
                      (plist-get origin :tool-call-arguments)))))))

(ert-deftest test-async-dispatcher-origin-failure-notifies-runtime ()
  "Late origin validation failure closes evidence instead of timing out."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((active t)
            (effect-count 0)
            audit-events
            (tool
             (test-llm--make-tool
              :name "lookup" :args '((:name "x" :type string))
              :function (lambda (_x) (cl-incf effect-count) "ok")))
            (use
             (make-llm-provider-utils-tool-use
              :id "" :name "lookup" :args '((x . "same"))))
            (prompt
             (make-llm-chat-prompt
              :interactions
              (list (make-llm-chat-prompt-interaction
                     :role 'assistant :content (list use)))))
            (wrapped
             (car
              (superchat--llm-attempt-tools
               prompt (list tool)
               `(:evidence-contract "epistemic-learning-v0"
                 :evidence-attempt 1
                 :decision-snapshot-event-id "snapshot-1"
                 :attempt-active-p ,(lambda () active)
                 :audit-failure-observer
                 ,(lambda (value)
                    (push value audit-events)
                    (setq active nil)))))))
       (should
        (equal "[Tool call blocked because exact audit failed]"
               (funcall (llm-tool-function wrapped) "same")))
       (should (= 0 effect-count))
       (should (= 1 (length audit-events)))
       (should-not active)))))

(ert-deftest test-async-dispatcher-origin-rejects-normalized-false-or-null ()
  "An ambiguous nil argument cannot become durable Action evidence."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((effect-count 0)
            audit-events
            (tool
             (test-llm--make-tool
              :name "lookup" :args '((:name "x" :type string))
              :function (lambda (_x) (cl-incf effect-count) "ok")))
            (use
             (make-llm-provider-utils-tool-use
              :id "call-1" :name "lookup" :args '((x))))
            (prompt
             (make-llm-chat-prompt
              :interactions
              (list (make-llm-chat-prompt-interaction
                     :role 'assistant :content (list use)))))
            (wrapped
             (car
              (superchat--llm-attempt-tools
               prompt (list tool)
               `(:evidence-contract "epistemic-learning-v0"
                 :evidence-attempt 1
                 :decision-snapshot-event-id "snapshot-1"
                 :attempt-active-p always
                 :audit-failure-observer
                 ,(lambda (value) (push value audit-events)))))))
       (should
        (equal "[Tool call blocked because exact audit failed]"
               (funcall (llm-tool-function wrapped) nil)))
       (should (= 0 effect-count))
       (should (= 1 (length audit-events)))))))

(ert-deftest test-async-dispatcher-evidence-rejects-dynamic-and-async-tools ()
  "Eligible transport fails before provider effects for deferred tool surfaces."
  (test-llm--with-mock-llm
   (lambda ()
     (dolist (case '(("" nil nil)
                     (lookup nil nil)
                     ("define_tool" nil nil)
                     ("learned" nil ("learned"))
                     ("async_lookup" t nil)))
       (let ((superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat--llm-tools-prepared-p t)
             (superchat--llm-tool-wrapper #'identity)
             (superchat--llm-prepared-tools
              (list (test-llm--make-tool
                     :name (nth 0 case) :async (nth 1 case)
                     :function #'ignore)))
             (superchat--syn-names (nth 2 case))
             (superchat--llm-evidence-context
              '(:contract "epistemic-learning-v0"
                :context-envelope
                (:contract "context-continuity-v0" :status satisfied
                 :receipt (:tape-head 1))))
             (provider-calls 0))
         (cl-letf (((symbol-function 'llm-provider-chat-request)
                    (lambda (&rest _args) (cl-incf provider-calls)))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
           :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))))))))

(ert-deftest test-async-dispatcher-evidence-validates-raw-tool-surface ()
  "Malformed raw tools fail before wrappers can hide them from evidence."
  (dolist
      (tools
       (list
        (list (test-llm--make-tool :name "lookup" :function nil))
        (list (test-llm--make-tool :name "lookup" :function #'ignore)
              (test-llm--make-tool :name "lookup" :function #'ignore))
        (list (test-llm--make-tool
               :name "lookup" :args [(:name "x" :type string)]
               :function #'ignore))
        (list (test-llm--make-tool
               :name "lookup" :args '((:name "" :type string))
               :function #'ignore))
        (list (test-llm--make-tool
               :name "lookup"
               :args '((:name "x" :type string)
                       (:name "x" :type string))
               :function #'ignore))))
    (test-llm--with-mock-llm
     (lambda ()
       (let ((superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat--llm-tools-prepared-p t)
             (superchat--llm-tool-wrapper #'identity)
             (superchat--llm-prepared-tools tools)
             (superchat--llm-evidence-context
              '(:contract "epistemic-learning-v0"))
             (provider-calls 0))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
            :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))))))))

(ert-deftest test-evidence-boolean-false-retains-provider-provenance ()
  "Explicit JSON false remains distinct from the nil passed to Elisp."
  (dolist (provider-false '(:json-false :false))
    (test-llm--with-mock-llm
     (lambda ()
       (let* ((superchat--llm-raw-tool-uses (make-hash-table :test #'eq))
              (prompt (make-llm-chat-prompt :interactions nil))
              called-value origin
              (tool
               (test-llm--make-tool
                :name "flag"
                :args '((:name "enabled" :type boolean :optional t))
                :function
                (lambda (enabled)
                  (setq called-value (list enabled)
                        origin (copy-tree superchat--llm-tool-origin t))
                  "done")))
              (wrapped
               (car
                (superchat--llm-attempt-tools
                 prompt (list tool)
                 '(:evidence-attempt 1
                   :decision-snapshot-event-id "evt_snapshot"
                   :attempt-active-p always))))
              (use
               (make-llm-provider-utils-tool-use
                :id "call-false" :name "flag"
                :args `((enabled . ,provider-false)))))
         (superchat--llm-validate-evidence-tools (list tool))
         (superchat--llm-capture-provider-tool-uses
          #'ignore nil prompt (list use))
         (setf (llm-chat-prompt-interactions prompt)
               (list (make-llm-chat-prompt-interaction
                      :role 'assistant :content (list use))))
         ;; llm.el 0.31.1 mutates the live provider use before invoking the tool.
         (setf (llm-provider-utils-tool-use-args use) '((enabled)))
         (should (equal "done" (funcall (llm-tool-function wrapped) nil)))
         (should (equal '(nil) called-value))
         (should
          (equal '((enabled . :json-false))
                 (plist-get origin :tool-call-arguments)))
         (should
          (equal "{\"enabled\":false}"
                 (plist-get
                  (superchat-tape-exact-json
                   (plist-get origin :tool-call-arguments))
                  :json)))
         (should (equal '((enabled . :false))
                        (llm-provider-utils-tool-use-args use)))
         (should
          (equal '((enabled . :json-false))
                 (plist-get (superchat--llm-evidence-tool-use use)
                            :arguments))))))))

(ert-deftest test-evidence-boolean-null-is-not-treated-as-false ()
  "Explicit JSON null cannot borrow false's Elisp nil execution value."
  (dolist (arguments '(((enabled . :null)) ((enabled))))
    (test-llm--with-mock-llm
     (lambda ()
       (let* ((superchat--llm-raw-tool-uses (make-hash-table :test #'eq))
              (prompt (make-llm-chat-prompt :interactions nil))
              called
              (tool
               (test-llm--make-tool
                :name "flag"
                :args '((:name "enabled" :type boolean :optional t))
                :function (lambda (_enabled) (setq called t))))
              (wrapped
               (car
                (superchat--llm-attempt-tools
                 prompt (list tool)
                 '(:evidence-attempt 1
                   :decision-snapshot-event-id "evt_snapshot"
                   :attempt-active-p always))))
              (use
               (make-llm-provider-utils-tool-use
                :id "call-null" :name "flag" :args arguments)))
         (superchat--llm-capture-provider-tool-uses
          #'ignore nil prompt (list use))
         (setf (llm-chat-prompt-interactions prompt)
               (list (make-llm-chat-prompt-interaction
                      :role 'assistant :content (list use))))
         (setf (llm-provider-utils-tool-use-args use) '((enabled)))
         (should
          (equal "[Tool call blocked because exact audit failed]"
                 (funcall (llm-tool-function wrapped) nil)))
         (should-not called))))))

(ert-deftest test-evidence-optional-boolean-omission-remains-valid ()
  "An omitted optional boolean is provably absent, not explicit null."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat--llm-raw-tool-uses (make-hash-table :test #'eq))
            (prompt (make-llm-chat-prompt :interactions nil))
            called origin
            (tool
             (test-llm--make-tool
              :name "flag"
              :args '((:name "enabled" :type boolean :optional t))
              :function
              (lambda (enabled)
                (setq called (list enabled)
                      origin (copy-tree superchat--llm-tool-origin t)))))
            (wrapped
             (car
              (superchat--llm-attempt-tools
               prompt (list tool)
               '(:evidence-attempt 1
                 :decision-snapshot-event-id "evt_snapshot"
                 :attempt-active-p always))))
            (use
             (make-llm-provider-utils-tool-use
              :id "call-omitted" :name "flag" :args nil)))
       (superchat--llm-capture-provider-tool-uses
        #'ignore nil prompt (list use))
       (setf (llm-chat-prompt-interactions prompt)
             (list (make-llm-chat-prompt-interaction
                    :role 'assistant :content (list use))))
       (funcall (llm-tool-function wrapped) nil)
       (should (equal '(nil) called))
       (should-not (plist-get origin :tool-call-arguments))))))

(ert-deftest test-async-dispatcher-evidence-preflights-origin-accessors ()
  "Missing origin helpers fail before an eligible provider request."
  (dolist (accessor '(llm-provider-utils--normalize-args
                      llm-provider-populate-tool-uses
                      llm-provider-utils-tool-use-id
                      llm-tool-function))
    (test-llm--with-mock-llm
     (lambda ()
       (let ((superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat--llm-tools-prepared-p t)
             (superchat--llm-tool-wrapper #'identity)
             (superchat--llm-prepared-tools
              (list (test-llm--make-tool
                     :name "lookup" :args '((:name "x" :type string))
                     :function #'ignore)))
             (superchat--llm-evidence-context
              '(:contract "epistemic-learning-v0"))
             (provider-calls 0))
         (cl-letf (((symbol-function accessor) nil)
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
            :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))))))))

(ert-deftest test-async-dispatcher-evidence-requires-authority-wrapper ()
  "Eligible tools fail before the provider when Authority is unavailable."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0" :status satisfied
               :receipt (:tape-head 1))))
           (tool-effects 0)
           (provider-calls 0))
       (cl-letf (((symbol-function 'superchat--collect-llm-tools)
                  (lambda (&rest _args)
                    (list (test-llm--make-tool
                           :name "lookup"
                           :function (lambda () (cl-incf tool-effects))))))
                 ((symbol-function 'superchat--agent-make-wrapper) nil)
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (&rest _args) (cl-incf provider-calls))))
         (should-error
          (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
          :type 'superchat-llm-evidence-recording-error)
         (should (= 0 provider-calls))
         (should (= 0 tool-effects)))))))

(ert-deftest test-async-dispatcher-evidence-rejects-tool-schema-divergence ()
  "The snapshot and actual provider tool surface cannot silently differ."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-tools-prepared-p t)
           (superchat--llm-tool-wrapper #'identity)
           (superchat--llm-prepared-tools
            (list (test-llm--make-tool
                   :name "lookup" :description "base" :function #'ignore)))
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0" :status satisfied
               :receipt (:tape-head 1))))
           (provider-calls 0) (snapshots 0))
       (let ((superchat--llm-runtime-observer
              (lambda (event _value)
                (when (eq event 'decision-snapshot) (cl-incf snapshots)))))
         (cl-letf (((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest args)
                      (make-llm-chat-prompt
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text))
                       :tools (plist-get args :tools))))
                   ((symbol-function 'llm-provider-chat-request)
                    (lambda (_provider prompt _streaming)
                      (let ((tool (car (llm-chat-prompt-tools prompt))))
                        (setcdr tool
                                (plist-put (cdr tool) :description "mutated")))
                      'serialized))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
            :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))
           (should (= 0 snapshots))))))))

(ert-deftest test-async-dispatcher-noneligible-tools-keep-existing-behavior ()
  "Tool-free evidence gating does not change ordinary tool requests."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-tools-prepared-p t)
           (superchat--llm-prepared-tools
            (list (test-llm--make-tool :name "lookup")))
           (provider-calls 0)
           result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial response _error
                           &optional _multi-output)
                    (cl-incf provider-calls)
                    (funcall response "done")
                    'provider-request)))
         (superchat--llm-generate-answer
          "use ordinary tool" (lambda (value) (setq result value)) #'ignore)
         (should (= 1 provider-calls))
         (should (equal "done" result)))))))

(ert-deftest test-async-dispatcher-evidence-write-failure-prevents-provider ()
  "A required snapshot failure is not converted into a provider request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0" :status satisfied
               :receipt (:tape-head 1))))
           (provider-calls 0)
           (superchat--llm-runtime-observer
            (lambda (event _value)
              (when (eq event 'decision-snapshot)
                (error "Tape unavailable")))))
       (cl-letf (((symbol-function 'llm-make-chat-prompt)
                  (lambda (text &rest _args)
                    (list 'mock-prompt
                          :interactions
                          (list (list :role 'user :content text))
                          :tools nil)))
                 ((symbol-function 'llm-chat-prompt-interactions)
                  (lambda (prompt)
                    (plist-get (cdr prompt) :interactions)))
                 ((symbol-function 'llm-chat-prompt-interaction-role)
                  (lambda (interaction) (plist-get interaction :role)))
                 ((symbol-function 'llm-chat-prompt-interaction-content)
                  (lambda (interaction) (plist-get interaction :content)))
                 ((symbol-function 'llm-chat-prompt-tools)
                  (lambda (_prompt) nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (&rest _args) (cl-incf provider-calls))))
         (should-error
          (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
          :type 'superchat-llm-evidence-recording-error)
         (should (= 0 provider-calls)))))))

(ert-deftest test-async-dispatcher-evidence-accessors-fail-closed ()
  "Eligible transport never substitutes stale prompt or tool input."
  (dolist (case '(messages-unavailable messages-error
                  tools-unavailable tools-error))
    (test-llm--with-mock-llm
     (lambda ()
       (let ((superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat--llm-evidence-context
              '(:contract "epistemic-learning-v0"
                :context-envelope
                (:contract "context-continuity-v0" :status satisfied
                 :receipt (:tape-head 1))))
             (superchat--llm-runtime-observer #'ignore)
             (provider-calls 0))
         (cl-letf (((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest args)
                      (list 'mock-prompt
                            :interactions
                            (list (list :role 'user :content text))
                            :tools (plist-get args :tools))))
                   ((symbol-function 'llm-chat-prompt-interactions)
                    (pcase case
                      ('messages-unavailable nil)
                      ('messages-error
                       (lambda (_prompt) (error "messages unreadable")))
                      (_ (lambda (prompt)
                           (plist-get (cdr prompt) :interactions)))))
                   ((symbol-function 'llm-chat-prompt-interaction-role)
                    (lambda (interaction) (plist-get interaction :role)))
                   ((symbol-function 'llm-chat-prompt-interaction-content)
                    (lambda (interaction) (plist-get interaction :content)))
                   ((symbol-function 'llm-chat-prompt-tools)
                    (pcase case
                      ('tools-unavailable nil)
                      ('tools-error
                       (lambda (_prompt) (error "tools unreadable")))
                      (_ (lambda (prompt)
                           (plist-get (cdr prompt) :tools)))))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
            :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))))))))

(ert-deftest test-async-dispatcher-tool-choice-accessor-fails-closed ()
  "Eligible tool choice must be readable before the provider request."
  (dolist (case '(unavailable error))
    (test-llm--with-mock-llm
     (lambda ()
       (let ((superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat--llm-tools-prepared-p t)
             (superchat--llm-tool-wrapper #'identity)
             (superchat--llm-prepared-tools
              (list (test-llm--make-tool
                     :name "lookup" :function #'ignore)))
             (superchat--llm-evidence-context
              '(:contract "epistemic-learning-v0"
                :context-envelope
                (:contract "context-continuity-v0" :status satisfied
                 :receipt (:tape-head 1))))
             (superchat--llm-runtime-observer #'ignore)
             (provider-calls 0))
         (cl-letf (((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest args)
                      (make-llm-chat-prompt
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text))
                       :tools (plist-get args :tools)
                       :tool-options
                       (make-llm-tool-options :tool-choice "lookup"))))
                   ((symbol-function 'llm-provider-chat-request)
                    (lambda (&rest _args) 'serialized))
                   ((symbol-function 'llm-chat-prompt-tool-options)
                    (pcase case
                      ('unavailable nil)
                      (_ (lambda (_prompt)
                           (error "tool choice unreadable")))))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args) (cl-incf provider-calls))))
           (should-error
            (superchat--llm-generate-answer "blocked" #'ignore #'ignore)
            :type 'superchat-llm-evidence-recording-error)
           (should (= 0 provider-calls))))))))

(ert-deftest test-async-dispatcher-evidence-protocol-error-does-not-retry ()
  "Tool-free evidence records an unexpected protocol error, then stops."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0" :status satisfied
               :receipt (:tape-head 1))))
           (calls 0) error-callback events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (setq events (append events (list (list event value))))
                (when (eq event 'decision-snapshot) "snapshot-1"))))
         (cl-letf (((symbol-function 'llm-make-chat-prompt)
                    (lambda (text &rest _args)
                      (make-llm-chat-prompt
                       :interactions
                       (list (make-llm-chat-prompt-interaction
                              :role 'user :content text))
                       :tools nil)))
                   ((symbol-function 'llm-provider-chat-request)
                    (lambda (&rest _args) 'serialized))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial _response error
                             &optional _multi-output)
                      (cl-incf calls)
                      (setq error-callback error)
                      'provider-request)))
           (superchat--llm-generate-answer "plain" #'ignore #'ignore)
           (funcall error-callback
                    'llm-tool-missing-argument "unexpected tool")
           (should (= 1 calls))
           (let ((first (cadr (assq 'first-output events)))
                 (terminal (cadr (assq 'terminal events))))
             (should (= 1 (plist-get first :attempt)))
             (should (equal "tool-protocol-error"
                            (plist-get first :kind)))
             (should (eq 'failed (plist-get terminal :status))))))))))

(ert-deftest test-agent-tool-restores-originating-view ()
  "Delayed sync and async agent tools should use their originating View."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((target-name
            (generate-new-buffer-name " *superchat-agent-origin*"))
           (superchat-agent-confirm-destructive nil)
           (superchat-authority-approval-function (lambda (_request) t))
           (original (lambda () "ok"))
           asyncp done-cb seen wrapped)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-tape-append-event)
                      (lambda (_event) 1))
                     ((symbol-function 'superchat--agent-render-tool-call)
                      (lambda (&rest _)
                        (push (list :call (buffer-name) superchat-buffer-name)
                              seen)))
                     ((symbol-function 'superchat--agent-render-tool-result)
                      (lambda (&rest _)
                        (push (list :result (buffer-name) superchat-buffer-name)
                              seen))))
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :name "mock"
                         :description "mock"
                         :args nil
                         :function original
                         :async asyncp)))))
             (with-temp-buffer
               (should (equal "ok"
                              (funcall (llm-tool-function wrapped)))))
             (should (equal (nreverse seen)
                            `((:call ,target-name ,target-name)
                              (:result ,target-name ,target-name))))
             (setq asyncp t
                   original (lambda (done) (setq done-cb done))
                   seen nil)
             (with-current-buffer (get-buffer-create target-name)
               (let ((superchat-buffer-name target-name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :name "mock"
                         :description "mock"
                         :args nil
                         :function original
                         :async asyncp)))))
             (with-temp-buffer
               (funcall (llm-tool-function wrapped)
                        (lambda (_result)
                          (push (list :callback (buffer-name)
                                      superchat-buffer-name)
                                seen))))
             (with-temp-buffer
               (funcall done-cb "async-ok"))
             (should (equal (nreverse seen)
                            `((:call ,target-name ,target-name)
                              (:result ,target-name ,target-name)
                              (:callback ,target-name ,target-name)))))
         (when (get-buffer target-name)
           (kill-buffer target-name)))))))

(ert-deftest test-async-dispatcher-diffs-cumulative-plist-chunks ()
  "Multi-output plist chunks carry the FULL text so far (see
`llm-provider-utils-streaming-accumulate'); the dispatcher must forward
deltas, not re-insert the whole accumulated text on every chunk."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil)
           (streamed-chunks '()))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda () nil))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial-cb response-cb
                           _error-cb &optional _multi-output)
                    ;; llm.el accumulates: each partial carries all text
                    ;; received so far.
                    (funcall partial-cb '(:text "Hello, "))
                    (funcall partial-cb '(:text "Hello, world"))
                    (funcall partial-cb '(:text "Hello, world!"))
                    (funcall response-cb '(:text "Hello, world!")))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (chunk) (push chunk streamed-chunks))
          nil
          nil)
         (should (string= final-result "Hello, world!"))
         (should (equal (nreverse streamed-chunks)
                        '("Hello, " "world" "!"))))))))

(ert-deftest test-async-dispatcher-retries-after-tool-round ()
  "A tool round reuses its prompt and requests the model's final answer."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if (= (length requests) 1)
                        ;; llm.el has already appended this result to PROMPT.
                        (funcall response-cb
                                 '((fake-tool . "tool result")))
                      (funcall response-cb "final answer after tool")))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 2 (length requests)))
         (should (eq (car requests) (cadr requests)))
         (should (string= final-result "final answer after tool")))))))

(ert-deftest test-runtime-steers-are-injected-before-provider-attempt ()
  "A Runtime steer reaches the prompt at the next model boundary."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           appended final-result)
       (cl-letf (((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt text role)
                    (push (list text role) appended)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (should (equal '(("take another path" user)) appended))
                    (funcall response-cb "done"))))
         (let ((superchat--llm-runtime-observer
                (lambda (event _value)
                  (when (eq event 'drain-steers)
                    '("take another path")))))
           (superchat--llm-generate-answer
            "continue" (lambda (result) (setq final-result result)) #'ignore))
         (should (equal "done" final-result)))))))

(ert-deftest test-tool-followup-injects-runtime-recovery-view-without-extra-request ()
  "A reviewed recovery prompt rides the existing post-tool request."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool
                   :name "fake-tool"
                   :function (lambda (&rest _args) "value"))))
           (requests 0)
           first-response
           appended
           before-attempts
           snapshots
           final-result
           (evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope
              (:contract "context-continuity-v0"
               :status empty
               :receipt (:tape-head 0))))
           (runtime-observer
            (lambda (event value)
              (pcase event
                ('before-attempt
                 (push (plist-get value :attempt-reason) before-attempts)
                 '(:status satisfied
                   :prompt "reviewed recovery"
                   :receipt
                   (:candidate (:session-id "candidate"
                                :event-id "evt-candidate"))))
                ('decision-snapshot
                 (push (copy-tree value t) snapshots)
                 (format "snapshot-%d"
                         (plist-get value :attempt)))))))
       (cl-letf (((symbol-function 'llm-make-chat-prompt)
                  (lambda (text &rest args)
                    (make-llm-chat-prompt
                     :context (plist-get args :context)
                     :interactions
                     (list (make-llm-chat-prompt-interaction
                            :role 'user :content text))
                     :tools (plist-get args :tools))))
                 ((symbol-function 'llm-provider-chat-request)
                  (lambda (_provider _prompt _streaming) 'serialized))
                 ((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt text role)
                    (push (list text role) appended)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (if (= requests 1)
                        (setq first-response response-cb)
                      (funcall response-cb "recovered")))))
         (let ((superchat--llm-evidence-context evidence-context)
               (superchat--llm-runtime-observer runtime-observer))
           (superchat--llm-generate-answer
            "use the tool" (lambda (result) (setq final-result result))
            #'ignore))
         ;; Invoke the first provider callback after the dynamic Runtime
         ;; binding has unwound, as a real asynchronous provider does.
         (should (= 1 requests))
         (let ((superchat--llm-evidence-context nil)
               (superchat--llm-runtime-observer nil))
           (funcall first-response '((fake-tool . "failed"))))
         (should (= 2 requests))
         (should (equal '(tool-followup) before-attempts))
         (should (equal '(("reviewed recovery" user)) appended))
         (should (equal "recovered" final-result))
         (should (= 2 (length snapshots)))
         (let* ((prepared
                 (plist-get (cadr (nreverse snapshots))
                            :prepared-envelope))
                (view-envelope (plist-get prepared :view-envelope)))
           (should (equal "context-continuity-v0"
                          (plist-get view-envelope :contract)))
           (should (eq 'empty (plist-get view-envelope :status)))
           (should (equal '(:tape-head 0)
                          (plist-get view-envelope :receipt)))
           (should (eq 'satisfied
                       (plist-get (plist-get view-envelope
                                             :experience-view)
                                  :status)))
           (should (equal '(:candidate (:session-id "candidate"
                                      :event-id "evt-candidate"))
                          (plist-get (plist-get view-envelope
                                                :experience-view)
                                     :receipt)))))))))

(ert-deftest test-async-dispatcher-does-not-finalize-before-followup-response ()
  "A tool round must not finalize while its asynchronous follow-up runs.

Synchronous mocks hide this ordering error because the follow-up callback
can run re-entrantly before the first callback returns.  Real providers
return from the first callback after scheduling their next request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (requests '())
           first-response second-response final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (push prompt requests)
                    (if first-response
                        (setq second-response response-cb)
                      (setq first-response response-cb)))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 1 (length requests)))
         (should-not final-result)
         (funcall first-response '((fake-tool . "tool result")))
         (should (= 2 (length requests)))
         (should-not final-result)
         (funcall second-response "final answer after asynchronous tool")
         (should (string= final-result
                          "final answer after asynchronous tool")))))))

(ert-deftest test-async-dispatcher-recovers-missed-plz-sentinel ()
  "A dead plz request with an unprocessed response is finalized once."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'ollama))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (transport-buffer (generate-new-buffer " *test-plz-response*"))
           response-cb scheduled timer-cancelled sentinel-called final-result)
       (unwind-protect
           (cl-letf (((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt _partial-cb callback
                               _error-cb &optional _multi-output)
                        (setq response-cb callback)
                        'fake-plz-request))
                     ((symbol-function 'run-with-timer)
                      (lambda (_seconds _repeat function &rest args)
                        (setq scheduled
                              (lambda () (apply function args)))
                        'fake-watchdog-timer))
                     ((symbol-function 'cancel-timer)
                      (lambda (timer)
                        (setq timer-cancelled timer)))
                     ((symbol-function 'processp)
                      (lambda (value) (eq value 'fake-plz-request)))
                     ((symbol-function 'process-live-p)
                      (lambda (_process) nil))
                     ((symbol-function 'process-buffer)
                      (lambda (_process) transport-buffer))
                     ((symbol-function 'process-get)
                      (lambda (_process property)
                        (pcase property
                          (:plz-result :plz-result)
                          (:plz-as nil))))
                     ((symbol-function 'process-sentinel)
                      (lambda (_process) 'plz--sentinel))
                     ((symbol-function 'plz--sentinel)
                      (lambda (_process status)
                        (setq sentinel-called status)
                        (funcall response-cb "final answer"))))
             (superchat--llm-generate-answer
              "finish the request"
              (lambda (result) (setq final-result result))
              #'ignore)
             (should scheduled)
             (should-not final-result)
             (funcall scheduled)
             (should (equal sentinel-called "workaround"))
             (should (eq timer-cancelled 'fake-watchdog-timer))
             (should (equal final-result "final answer")))
         (when (buffer-live-p transport-buffer)
           (kill-buffer transport-buffer)))))))

(ert-deftest test-async-dispatcher-stops-at-tool-round-limit ()
  "Tool-round cap applies before Superchat sends another model request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "fake-tool")))
           (superchat-llm-max-tool-rounds 1)
           (superchat-llm-round-limit-action 'stop)
           (requests 0)
           final-result)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (funcall response-cb '((fake-tool . "tool result"))))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= requests 1))
         (should (string= final-result "[Stopped after 1 tool rounds]")))))))

(ert-deftest test-async-dispatcher-finalizes-after-tool-round-limit ()
  "The finalization action keeps completion metadata enabled."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           (superchat-llm-tools-list
           (list (test-llm--make-tool
                   :name "fake-tool"
                   :function (lambda (&rest _args) "value"))))
           (superchat-llm-max-tool-rounds 2)
           (superchat-llm-round-limit-action 'finalize)
           (superchat--llm-evidence-context
            '(:contract "epistemic-learning-v0"
              :context-envelope (:contract "context-continuity-v0"
                                 :status empty
                                 :receipt (:tape-head 0))))
           snapshots
           (superchat--llm-runtime-observer
            (lambda (event value)
              (cond
               ((eq event 'before-attempt)
                '(:status satisfied :prompt "reviewed recovery"))
               ((eq event 'decision-snapshot)
                (push (copy-tree value t) snapshots)
                (format "snapshot-%d" (length snapshots))))))
           (requests '())
           appended
           tools-disabled
           final-result)
       (cl-letf (((symbol-function 'superchat--llm-disable-prompt-tools)
                  (lambda (_prompt) (setq tools-disabled t)))
                 ((symbol-function 'llm-make-chat-prompt)
                  (lambda (text &rest args)
                    (make-llm-chat-prompt
                     :context (plist-get args :context)
                     :interactions
                     (list (make-llm-chat-prompt-interaction
                            :role 'user :content text))
                     :tools (plist-get args :tools))))
                 ((symbol-function 'llm-provider-chat-request)
                  (lambda (_provider _prompt _streaming) 'serialized))
                 ((symbol-function 'llm-chat-prompt-append-response)
                  (lambda (_prompt text role)
                    (push (list text role) appended)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt _partial-cb response-cb
                           _error-cb &optional multi-output)
                    (push (cons prompt multi-output) requests)
                    (if (<= (length requests) 2)
                        (funcall response-cb '((fake-tool . "tool result")))
                      (funcall response-cb "final answer without tools")))))
         (superchat--llm-generate-answer
          "use the tool"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil))
         (should (= 3 (length requests)))
         (should (llm-chat-prompt-p (caar requests)))
         (should (llm-chat-prompt-p (caar (cdr requests))))
         (should (cdr (car requests)))
         (should tools-disabled)
         (should (string= final-result "final answer without tools"))
         (should
          (cl-some (lambda (entry)
                     (and (eq 'user (cadr entry))
                          (string-match-p "Answer directly in text"
                                          (car entry))))
                   appended))
         (should (= 3 (length snapshots)))
         (let* ((ordered (nreverse snapshots))
                (followup-envelope
                 (plist-get (cadr ordered) :prepared-envelope))
                (final-envelope
                 (plist-get (caddr ordered) :prepared-envelope)))
           (should (eq 'satisfied
                       (plist-get
                        (plist-get (plist-get followup-envelope
                                              :view-envelope)
                                   :experience-view)
                        :status)))
           (should-not (plist-get (plist-get final-envelope
                                             :view-envelope)
                                  :experience-view))))))))

(ert-deftest test-async-dispatcher-error-cb-finalizes ()
  "When the error-cb fires, the final callback receives an error string.

llm.el invokes the error callback with TWO arguments (type message) at
every call site (`error-callback 'error <msg>').  A one-argument error-cb
crashes inside plz's timer with `wrong-number-of-arguments', which masks
the real API error, so this test drives the real two-argument contract."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (final-result nil))
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb _response-cb
                           error-cb &optional _multi-output)
                    ;; The two-argument form llm.el actually uses.
                    (funcall error-cb 'error "rate limit exceeded"))))
         (superchat--llm-generate-answer
          "say hi"
          (lambda (result) (setq final-result result))
          (lambda (_chunk) nil)
          nil
          nil)
         (should (string-match-p "rate limit exceeded" final-result)))))))

(ert-deftest test-prompt-repair-survives-late-llm-load ()
  "Prompt repair must work if llm.el loads after Superchat.

`superchat-llm.el' is allowed to load before the optional llm dependency.
Define the minimal prompt structs after the module is loaded to reproduce
that order; a setter baked into the source at load time must not be needed.
Run the reproduction in a child Emacs so its fake llm structs cannot pollute
the other tests in this file."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (or (locate-library "superchat-llm")
                                       (expand-file-name "../superchat-llm.el"
                                                         (file-name-directory
                                                          (or load-file-name
                                                              buffer-file-name))))))
        (script (make-temp-file "superchat-llm-late-load-" nil ".el")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file script
            (insert
             "(progn\n"
             "  (require 'cl-lib)\n"
             "  (require 'superchat-llm)\n"
             "  (cl-defstruct llm-chat-prompt interactions tools)\n"
             "  (cl-defstruct llm-chat-prompt-interaction role content)\n"
             "  (let* ((user (make-llm-chat-prompt-interaction\n"
             "                 :role 'user :content \"question\"))\n"
             "         (tool-use (make-llm-chat-prompt-interaction\n"
             "                     :role 'assistant\n"
             "                     :content '((call:search . nil))))\n"
             "         (prompt (make-llm-chat-prompt\n"
             "                  :interactions (list user tool-use)\n"
             "                  :tools '(render-ui))))\n"
             "    (unless (superchat--llm-drop-dangling-tool-use prompt)\n"
             "      (kill-emacs 2))\n"
             "    (unless (equal (list user)\n"
             "                   (llm-chat-prompt-interactions prompt))\n"
             "      (kill-emacs 3))\n"
             "    (superchat--llm-disable-prompt-tools prompt)\n"
             "    (when (llm-chat-prompt-tools prompt)\n"
             "      (kill-emacs 4))))\n"))
          (should (= 0 (call-process emacs nil nil nil
                                     "-Q" "--batch" "-L" root "-l" script))))
      (when (file-exists-p script)
        (delete-file script)))))

(ert-deftest test-async-dispatcher-plain-chat-does-not-enumerate-tools ()
  "Plain on-demand chat should not synchronously walk built-in or MCP tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'on-demand)
           (superchat-show-response-mode t)
           (superchat--current-command nil)
           (tool-calls 0)
           (mcp-calls 0)
           captured-prompt
           captured-multi-output)
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda ()
                    (setq tool-calls (1+ tool-calls))
                    (list 'unexpected-tool)))
                 ((symbol-function 'superchat-mcp-get-tools)
                  (lambda ()
                    (setq mcp-calls (1+ mcp-calls))
                    (list 'unexpected-mcp-tool)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider prompt partial-cb response-cb
                           _error-cb &optional multi-output)
                    (setq captured-prompt prompt
                          captured-multi-output multi-output)
                    (funcall partial-cb "ok")
                    (funcall response-cb "ok"))))
         (superchat--llm-generate-answer
          "plain question"
          (lambda (_result) nil)
          (lambda (_chunk) nil)
          nil
          nil)
         (should (= 0 tool-calls))
         (should (= 0 mcp-calls))
         (should (eq (car captured-prompt) 'mock-prompt))
         (should (string= "plain question"
                          (plist-get (cdr captured-prompt) :text)))
         (should (null (plist-get (cdr captured-prompt) :tools)))
         (should captured-multi-output))))))

(ert-deftest test-core-run-turn-does-not-collect-tools ()
  "The core parse hook runner should not do request-time tool collection."
  (let ((superchat-system-prompt-functions nil)
        (superchat-build-prompt-functions nil)
        (superchat-post-turn-functions nil)
        (superchat-llm-backend (test-llm--make-mock-backend 'openai))
        (collect-calls 0))
    (cl-letf (((symbol-function 'superchat--collect-llm-tools)
               (lambda (&optional _input)
                 (setq collect-calls (1+ collect-calls))
                 (list 'unexpected-tool))))
      (let ((prepared (superchat-core-run-turn
                       (superchat-turn-new "plain question" "test-session"))))
        (should (= 0 collect-calls))
        (should (null (superchat-turn-tools prepared)))))))

(ert-deftest test-send-input-plain-chat-builds-prompt-from-user-input ()
  "The default send path must pass the user's plain text into the LLM prompt."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         captured-prompt)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (insert "Why is the first token slow?")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt _callback _stream-callback
                              &optional _target-model _context-files _tools _system _preset)
                       (setq captured-prompt prompt))))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-prompt)
    (should (string-match-p "Why is the first token slow\\?"
                            captured-prompt))))

(ert-deftest test-send-input-preserves-leading-ascii-subject ()
  "A leading ASCII subject must not be stripped from Chinese plain input."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         captured-prompt)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (insert "RaBitQ 是什么")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (prompt _callback _stream-callback
                              &optional _target-model _context-files _tools _system _preset)
                       (setq captured-prompt prompt))))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-prompt)
    (should (string-match-p "RaBitQ 是什么" captured-prompt))))

(ert-deftest test-send-input-inherits-active-preset-tools ()
  "When a buffer has an active preset, plain input should inherit its tools."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-send-test*"))
         (superchat-buffer-name buffer-name)
         (superchat-lang "English")
         (superchat-context-message-count 0)
         (superchat-context-max-chars nil)
         (superchat-memory-auto-recall-min-length 999999)
         (superchat-system-prompt-functions nil)
         (superchat-build-prompt-functions nil)
         (superchat-post-turn-functions nil)
         (active-preset (superchat-preset-from-plist
                         (list :name "coder"
                               :type 'agent
                               :tools '("read-file" "search-text"))))
         captured-tools
         captured-preset)
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (setq-local superchat--prompt-start (point-marker))
          (setq-local superchat--session-id "test-session")
          (setq-local superchat--current-command nil)
          (setq-local superchat--current-context-files nil)
          (setq-local superchat--conversation-history nil)
          (setq-local superchat--pending-recalled-memories nil)
          (setq-local superchat--active-preset active-preset)
          (insert "read the readme")
          (cl-letf (((symbol-function 'superchat--prepare-for-response)
                     (lambda ()
                       (setq superchat--response-start-marker
                             (point-marker))))
                    ((symbol-function 'superchat--update-status)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--record-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (_prompt _callback _stream-callback
                              &optional _target-model _context-files tools _system preset)
                       (setq captured-tools tools
                             captured-preset preset)
                       ;; Don't actually stream; finalize immediately.
                       nil)))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-tools)
    (should (eq active-preset captured-preset))
    (should (member "read-file" captured-tools))
    (should (member "search-text" captured-tools))))

(ert-deftest test-completion-at-point-prefixes-model-candidates ()
  "@ completion candidates should include the @ prefix because bounds include it."
  (let ((superchat--prompt-start nil))
    (with-temp-buffer
      (setq-local superchat--prompt-start (point-marker))
      (insert "@g")
      (cl-letf (((symbol-function 'superchat--get-available-models)
                 (lambda () '("gpt-4o" "claude-sonnet"))))
        (let ((capf (superchat--completion-at-point)))
          (should capf)
          (should (equal '("@gpt-4o" "@claude-sonnet")
                         (nth 2 capf))))))))

(ert-deftest test-completion-at-point-accepts-provider-qualified-models ()
  "@ completion should stay active after a provider namespace slash."
  (let ((superchat--prompt-start nil))
    (with-temp-buffer
      (setq-local superchat--prompt-start (point-marker))
      (insert "@qwen/")
      (cl-letf (((symbol-function 'superchat--get-available-models)
                 (lambda () '("qwen/qwen3.8-27b"))))
        (let ((capf (superchat--completion-at-point)))
          (should capf)
          (should (equal '("@qwen/qwen3.8-27b")
                         (nth 2 capf))))))))

(ert-deftest test-slash-key-triggers-completion-in-prompt ()
  "Typing / in the prompt should invoke completion immediately."
  (let ((completion-calls 0))
    (with-temp-buffer
      (setq-local superchat--prompt-start (point-marker))
      (cl-letf (((symbol-function 'completion-at-point)
                 (lambda ()
                   (setq completion-calls (1+ completion-calls))
                   nil)))
        (superchat--insert-slash-and-complete)
        (should (string= "/" (buffer-string)))
        (should (= 1 completion-calls))))))

;;;---------------------------------------------
;;; superchat-parser (pure, no llm deps)
;;;---------------------------------------------

(ert-deftest test-parser-model-switch-with-model ()
  "`@gpt-4o Hello' -> ('Hello' . 'gpt-4o')."
  (let ((parsed (superchat-parser-model-switch "@gpt-4o Hello, world!")))
    (should parsed)
    (should (string= "Hello, world!" (car parsed)))
    (should (string= "gpt-4o" (cdr parsed)))))

(ert-deftest test-parser-model-switch-with-provider-qualified-model ()
  "Provider-qualified model IDs keep their slash-qualified name."
  (let ((parsed
         (superchat-parser-model-switch
          "@qwen/qwen3.8-27b Explain opt-in and opt-out.")))
    (should parsed)
    (should (string= "Explain opt-in and opt-out." (car parsed)))
    (should (string= "qwen/qwen3.8-27b" (cdr parsed)))))

(ert-deftest test-parser-model-switch-no-model ()
  "Plain text without @ returns nil."
  (should-not (superchat-parser-model-switch "just a regular message")))

(ert-deftest test-parser-command-simple ()
  "`/help some args' -> ('help' . 'some args')."
  (let ((parsed (superchat-parser-command "/help some args")))
    (should parsed)
    (should (string= "help" (car parsed)))
    (should (string= "some args" (cdr parsed)))))

(ert-deftest test-parser-command-multiline ()
  "Multi-line args are preserved after the command name."
  (let ((parsed (superchat-parser-command "/explain\nline1\nline2")))
    (should parsed)
    (should (string= "explain" (car parsed)))
    (should (string-match-p "line1" (cdr parsed)))
    (should (string-match-p "line2" (cdr parsed)))))

(ert-deftest test-parser-command-no-match ()
  "A non-command input returns nil."
  (should-not (superchat-parser-command "no slash here")))

(ert-deftest test-parser-define-with-prompt ()
  "`/define name \"prompt\"' -> ('name' . 'prompt')."
  (let ((parsed (superchat-parser-define "/define explain-code \"Please explain $input\"")))
    (should parsed)
    (should (string= "explain-code" (car parsed)))
    (should (string= "Please explain $input" (cdr parsed)))))

(ert-deftest test-parser-define-name-only ()
  "`/define name' -> ('name' . '')."
  (let ((parsed (superchat-parser-define "/define greet")))
    (should parsed)
    (should (string= "greet" (car parsed)))
    (should (string= "" (cdr parsed)))))

(ert-deftest test-parser-extract-file-path-quoted ()
  "`#\"/path/with space/file.txt\"' normalizes and unescapes."
  (let ((path (superchat-parser-extract-file-path "#\"/tmp/foo bar.txt\"")))
    (should path)
    (should (string-match-p "foo bar.txt" path))))

(ert-deftest test-parser-extract-file-path-unquoted ()
  "`#/path/to/file' returns the normalized path."
  (let ((path (superchat-parser-extract-file-path "#/etc/hosts")))
    (should path)
    (should (string-match-p "hosts" path))))

(ert-deftest test-parser-extract-file-path-none ()
  "Input without a #file ref returns nil."
  (should-not (superchat-parser-extract-file-path "no file ref here")))

;;;---------------------------------------------
;;; Ollama UTF-8 split-chunk integrity (real loopback)
;;;---------------------------------------------

(defun test-ollama-utf8--run-one (ndjson-body split-offset)
  "Run one split-chunk case.  NDJSON-BODY is the raw multibyte NDJSON.
SPLIT-OFFSET splits the ENCODED body into two TCP writes.
Returns the final text from superchat--llm-generate-answer, or nil on timeout."
  (let* ((encoded-body (encode-coding-string ndjson-body 'utf-8))
         (body-len (length encoded-body))
         (hdr (format
               "HTTP/1.1 200 OK\r\nContent-Type: application/x-ndjson\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
               body-len))
         (view-buffer-name " *ert-ollama-utf8*")
         (processes-before (process-list))
         (buffers-before (buffer-list))
         result done server port request clients
         (deadline (+ (float-time) 15)))
    (setq server
          (make-network-process
           :name "ert-ollama-srv"
           :server t :host "127.0.0.1" :service 0
           :family 'ipv4 :noquery t :coding 'binary
           :filter
           (lambda (client string)
             (cl-pushnew client clients :test #'eq)
             (when (eq 'open (process-status client))
               (let ((acc (or (process-get client :acc) ""))
                     (clen (process-get client :clen)))
                 (setq acc (concat acc string))
                 (process-put client :acc acc)
                 (unless clen
                   (when (string-match "\r\n\r\n" acc)
                     (let* ((hdrs (substring acc 0 (match-beginning 0)))
                            (body-start (+ (match-beginning 0) 4)))
                       (setq clen
                             (if (string-match
                                  "Content-Length: *\\([0-9]+\\)" hdrs)
                                 (string-to-number (match-string 1 hdrs))
                               0))
                       (process-put client :clen clen)
                       (setq acc (substring acc body-start))
                       (process-put client :acc acc))))
                 (when (and clen (>= (length acc) clen)
                            (not (process-get client :replied)))
                   (process-put client :replied t)
                   (let* ((chunk1 (substring encoded-body 0 split-offset))
                          (chunk2 (substring encoded-body split-offset)))
                     (process-send-string client hdr)
                     (process-send-string client chunk1)
                     (sleep-for 0.05)
                     (when (eq 'open (process-status client))
                       (process-send-string client chunk2)
                       (process-send-eof client)))))))))
    (setq port (process-contact server :service))
    (unwind-protect
        (let ((superchat-llm-backend
               (make-llm-ollama :host "127.0.0.1" :port port
                                :chat-model "test"))
              (superchat-llm-model nil)
              (superchat-llm-reasoning nil)
              (superchat-llm-tools-enabled nil)
              (superchat-response-timeout 15)
              (superchat-show-ttft nil)
              (superchat--llm-tools-prepared-p nil)
              (superchat--llm-prepared-tools nil)
              (superchat--llm-tool-wrapper nil)
              (superchat--llm-round-limit-function nil)
              (superchat--llm-unknown-tool-retry-limit 0)
              (superchat--llm-persist-synthesized-tools nil)
              (superchat--llm-run-active-p (lambda () (not done)))
              (superchat--llm-runtime-observer nil)
              (superchat-buffer-name view-buffer-name))
          (setq request
                (superchat--llm-generate-answer
                 "hi"
                 (lambda (response) (setq result response done t))
                 (lambda (_chunk) nil)
                 nil nil 'none nil nil))
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
      (dolist (process
               (delete-dups
                (append clients (list request server)
                        (cl-remove-if-not
                         (lambda (candidate)
                           (let ((name (process-name candidate)))
                             (or (string-prefix-p "ert-ollama-" name)
                                 (string-prefix-p "plz-request-curl" name))))
                         (cl-set-difference (process-list) processes-before)))))
        (when (and (processp process) (process-live-p process))
          (delete-process process)))
      (dolist (buffer
               (cons (get-buffer view-buffer-name)
                     (cl-remove-if-not
                      (lambda (candidate)
                        (string-prefix-p " *plz-request-curl"
                                         (buffer-name candidate)))
                      (cl-set-difference (buffer-list) buffers-before))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    (when done result)))

(defvar test-ollama-utf8--filter-errors nil
  "Errors recorded by the plz-media-type-process-filter guard advice.")

(defvar test-ollama-utf8--filter-chunks nil
  "Raw plz media-type filter deliveries in the current test case.")

(defun test-ollama-utf8--filter-guard (original process media-types string)
  "Guard advice: call ORIGINAL, record any process-filter error, continue.
Only changes error propagation for observation; never re-splits or decodes."
  (push string test-ollama-utf8--filter-chunks)
  (condition-case err
      (funcall original process media-types string)
    (error
     (push (list (error-message-string err) process media-types string)
           test-ollama-utf8--filter-errors))))

(ert-deftest test-ollama-utf8-split-chunk-integrity ()
  "Preserve UTF-8 characters split across real curl/plz filter chunks.
Five cases cover ASCII, intact emoji, and all three internal emoji splits.
A guard on plz-media-type-process-filter catches and records internal
filter errors so the full matrix finishes and reports every bad split."
  (unless (and (require 'llm nil t)
               (require 'llm-ollama nil t)
               (require 'llm-request-plz nil t)
               (require 'plz nil t)
               (executable-find "curl"))
    (ert-skip "llm.el and curl dependencies unavailable"))
  (unless (and (featurep 'superchat-llm)
               (fboundp 'plz-media-type-process-filter))
    (ert-skip "superchat-llm.el or plz-media-type unavailable"))
  (let* ((test-ollama-utf8--filter-errors nil)
         (test-ollama-utf8--filter-chunks nil)
         (ascii-content "HelloASCIIWorld")
         (ascii-ndjson
          (concat "{\"message\":{\"content\":\"" ascii-content "\"}}\n"
                  "{\"done\":true}\n"))
         (emoji "\U0001F600")               ; 😀 = f0 9f 98 80 in UTF-8
         (emoji-content (concat "Hello" emoji "World"))
         (emoji-ndjson
          (concat "{\"message\":{\"content\":\"" emoji-content "\"}}\n"
                  "{\"done\":true}\n"))
         (emoji-bytes (encode-coding-string emoji 'utf-8))
         (emoji-encoded (encode-coding-string emoji-ndjson 'utf-8))
         (emoji-offset (string-match (regexp-quote emoji-bytes) emoji-encoded))
         cases failures)
    (unless emoji-offset (ert-skip "emoji not found in encoded NDJSON"))
    (setq cases
          (list (list "ASCII" ascii-ndjson 0 ascii-content nil)
                (list "emoji-intact" emoji-ndjson 0 emoji-content nil)
                (list "emoji-byte-1" emoji-ndjson (+ emoji-offset 1)
                      emoji-content t)
                (list "emoji-byte-2" emoji-ndjson (+ emoji-offset 2)
                      emoji-content t)
                (list "emoji-byte-3" emoji-ndjson (+ emoji-offset 3)
                      emoji-content t)))
    (advice-add 'plz-media-type-process-filter :around
                #'test-ollama-utf8--filter-guard)
    (unwind-protect
        (progn
          (dolist (case cases)
            (pcase-let ((`(,name ,body ,split ,expected ,internal-p) case))
              (setq test-ollama-utf8--filter-chunks nil)
              (let ((got (test-ollama-utf8--run-one body split)))
                (unless (equal expected got)
                  (push (format "%s expected %S got %S" name expected got)
                        failures))
                (when internal-p
                  (let* ((chunks (nreverse test-ollama-utf8--filter-chunks))
                         (prefix-at
                          (cl-position-if
                           (lambda (chunk) (string-match-p "Hello" chunk))
                           chunks))
                         (emoji-at
                          (cl-position-if
                           (lambda (chunk) (string-match-p emoji chunk))
                           chunks)))
                    (unless (and prefix-at emoji-at (< prefix-at emoji-at))
                      (push (format "%s did not cross a real filter boundary"
                                    name)
                            failures)))))))
          (when test-ollama-utf8--filter-errors
            (push (format "filter-errors=%d: %S"
                          (length test-ollama-utf8--filter-errors)
                          (mapcar #'car test-ollama-utf8--filter-errors))
                  failures)))
      (advice-remove 'plz-media-type-process-filter
                     #'test-ollama-utf8--filter-guard))
    (should-not (advice-member-p #'test-ollama-utf8--filter-guard
                                 'plz-media-type-process-filter))
    (when failures
      (ert-fail (mapconcat #'identity (nreverse failures) "\n")))))

;;; superchat-test-llm-backend.el ends here
