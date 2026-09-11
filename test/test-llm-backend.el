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
Mocks `llm-make-tool', `llm-name', `llm-chat', `llm-chat-streaming',
`superchat--provider-chat-model', and `superchat-mcp-get-tools'.
`llm-make-chat-prompt' and `llm-chat-prompt-append-response' are the real
llm.el functions (S08-fix-3 item 1, option D): they only build or extend an
in-memory `llm-chat-prompt' struct and never touch the network, so the seam
rule (only the provider transport may be replaced) forbids mocking them --
production code reads an attempt's real tool surface off the real struct
with `llm-chat-prompt-tools', which a plain-list stand-in cannot satisfy.
The dispatchers in superchat.el then exercise their actual call patterns
against the remaining mocks, so signature mismatches are caught at test
time."
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

;; S08-fix-3 item 1 (option D): `test-llm-mock-prompt-append-preserves-content-and-roles'
;; deleted here.  It tested only whether the removed `llm-make-chat-prompt' /
;; `llm-chat-prompt-append-response' fakes replicated real llm.el's own
;; append contract -- with those fakes gone (the seam rule forbids
;; replacing them; they touch no network), the real functions are exercised
;; directly by every other test in this file, and asserting llm.el's own
;; correctness against itself is not superchat's test to own.

(ert-deftest test-llm-mock-prompt-content-reaches-finalize-and-followup ()
  "Actual LLM dispatch sends appended instructions, retaining earlier content."
  (dolist (action '(continue finalize))
    (test-llm--with-mock-llm
     (lambda ()
       (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled 'always)
             (superchat-llm-tools-list
              (list (test-llm--make-tool :name "fixture-tool" :function #'ignore)))
             (superchat-llm-max-tool-rounds 1)
             (superchat--llm-round-limit-function (lambda () action))
             (superchat--llm-runtime-observer
              (lambda (event _value)
                (when (eq event 'before-attempt)
                  '(:prompt "recovery\n精确内容"))))
             (calls 0) first-response final-result snapshots)
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (_provider prompt _partial response _error
                             &optional _multi-output)
                      (push (mapcar
                             (lambda (interaction)
                               (list (llm-chat-prompt-interaction-role interaction)
                                     (llm-chat-prompt-interaction-content interaction)))
                             (llm-chat-prompt-interactions prompt)) snapshots)
                      (cl-incf calls)
                      (if (= calls 1)
                          (progn
                            ;; Model transport records its tool-round narration.
                            (llm-chat-prompt-append-response prompt "prior narration" 'assistant)
                            (setq first-response response))
                        (funcall response "done")))))
           (superchat--llm-generate-answer
            "original request" (lambda (value) (setq final-result value)) #'ignore)
           (funcall first-response '(("fixture-tool" . "tool result")))
           (should (= 2 calls))
           (should (equal "done" final-result))
           (should (equal '((user "original request")) (cadr snapshots)))
           (should
            (equal
             (list '(user "original request") '(assistant "prior narration")
                   (list 'user
                         (if (eq action 'continue) "recovery\n精确内容"
                           "The requested tool work is complete. Answer directly in text now. Do not call any tool.")))
             (car snapshots)))))))))

(defun test-render--match-count (regexp text)
  "Count non-overlapping REGEXP matches in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count)))
    count)))

(defun test-llm--make-native-chat-prompt (text &rest args)
  "Build a test prompt that preserves provider-visible TEXT and ARGS."
  (make-llm-chat-prompt
   :context (plist-get args :context)
   :interactions
   (list (make-llm-chat-prompt-interaction :role 'user :content text))
   :tools (plist-get args :tools)
   :temperature (plist-get args :temperature)
   :max-tokens (plist-get args :max-tokens)
   :response-format (plist-get args :response-format)
   :reasoning (plist-get args :reasoning)
   :non-standard-params (plist-get args :non-standard-params)
   :tool-options (plist-get args :tool-options)))

(defun test-llm--openai-like-chat-request (_provider prompt streaming)
  "Serialize PROMPT into the provider-visible OpenAI-compatible shape."
  (let* ((context (ignore-errors (llm-chat-prompt-context prompt)))
         (interactions (ignore-errors
                         (llm-chat-prompt-interactions prompt)))
         (messages
          (append
           (when (and (stringp context) (not (string-empty-p context)))
             (list (list :role "system" :content context)))
           (mapcar
            (lambda (interaction)
              (let ((content
                     (llm-chat-prompt-interaction-content interaction)))
                (list :role
                      (symbol-name
                       (llm-chat-prompt-interaction-role interaction))
                      :content
                      (if (stringp content) content (format "%s" content)))))
            interactions)))
         (tools (ignore-errors (llm-chat-prompt-tools prompt))))
    (append
     (list :model "gpt-4o-mini"
           :stream (if streaming t :false)
           :messages (vconcat messages))
     (when tools
       (list
        :tools
        (vconcat
         (mapcar
          (lambda (tool)
            (list :type "function"
                  :function
                  (list :name (llm-tool-name tool)
                        :description (or (llm-tool-description tool) "")
                        :parameters (list :type "object"))))
          tools)))))))

(defun test-llm--generic-word-token-count (_provider string)
  "Reproduce llm.el's generic word-count estimator for STRING."
  (with-temp-buffer
    (insert string)
    (/ (* (count-words (point-min) (point-max)) 4) 3)))

(defun test-llm--append-native-prompt-response (prompt response &optional role)
  "Append RESPONSE with ROLE to native test PROMPT."
  (setf (llm-chat-prompt-interactions prompt)
        (append
         (llm-chat-prompt-interactions prompt)
         (list (make-llm-chat-prompt-interaction
                :role (or role 'assistant) :content response))))
  prompt)

(defun test-llm--pump-timers-until (predicate timeout)
  "Pump real Emacs timers until PREDICATE succeeds or TIMEOUT expires."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

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

(ert-deftest test-context-budget-derives-only-from-trusted-window ()
  "Trusted windows use the measured reserve; static and missing windows fall back."
  (let ((superchat-context-max-chars nil)
        (superchat-context-chars-per-token 2)
        (superchat-context-fixed-reserve 700))
    (dolist (source '(override lm-studio-runtime))
      (cl-letf (((symbol-function 'superchat--llm-context-capability)
                 (lambda (_provider _model)
                   (list :limit 1000 :source source))))
        ;; 14 measured chars -> ceil(14 / 2) = 7 tokens; the remaining
        ;; 293 tokens are 586 chars, below the 50%-of-window cap.
        (should (= 586
                   (superchat--llm-context-budget-chars
                    "1234" "1234567890" "model-x")))))
    (dolist (source '(llm nil))
      (cl-letf (((symbol-function 'superchat--llm-context-capability)
                 (lambda (_provider _model)
                   (and source (list :limit 1000 :source source)))))
        (should (= 40000
                   (superchat--llm-context-budget-chars
                    "1234" "1234567890" "model-x")))))))

(ert-deftest test-context-budget-explicit-max-chars-wins ()
  "An explicit character budget bypasses model-window derivation."
  (let ((superchat-context-max-chars 1234))
    (cl-letf (((symbol-function 'superchat--llm-context-capability)
               (lambda (&rest _args)
                 (ert-fail "explicit budget must not inspect capabilities"))))
      (should (= 1234
                 (superchat--llm-context-budget-chars
                  "system" "turn" "model-x"))))))

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
  "Approved decision 3 (D14): `on-demand' behaves exactly like `always' now
-- the keyword heuristic this test used to probe
\(`superchat--should-attach-tools-p') was deleted, so a plain prompt gets
tools too, not only a `render_ui'-mentioning one."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-enabled 'on-demand))
       (should (superchat--collect-llm-tools
                "Use render_ui to draw a comparison table."))
       (should (superchat--collect-llm-tools
                "Explain the difference between Redux and Zustand."))))))

(ert-deftest test-on-demand-attaches-tools-for-eshell-intent ()
  "Approved decision 3 (D14): `on-demand' behaves exactly like `always' now
-- an `eshell-command' mention is no longer required to attach tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-enabled 'on-demand))
       (should (superchat--collect-llm-tools
                "Use eshell-command to run pwd."))
       (should (superchat--collect-llm-tools
                "Explain what Eshell is."))))))

(ert-deftest test-on-demand-attaches-tools-for-team-intent ()
  "Approved decision 3 (D14): `on-demand' behaves exactly like `always' now
-- a team-tool mention is no longer required to attach tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-enabled 'on-demand))
       (dolist (input '("Use team_task to create the work."
                        "Explain how agent teamwork could work."))
         (should (superchat--collect-llm-tools input)))))))

(ert-deftest test-render-ui-intent-hides-synthesis-tools ()
  "An explicit render request keeps agent-written UI tools out of the turn."
  (let ((superchat-llm-tools-enabled 'on-demand)
        ;; `all' so the baseline's own configured-names filter (S09-fix-3
        ;; item 1) does not drop this mock's arbitrarily named tools --
        ;; only `all' passes every registered/cached name through
        ;; unfiltered; the `render_ui' exclusion (also decision-time now,
        ;; S09-fix-3 item 2) is what this test actually exercises.
        (superchat-llm-tool-names 'all)
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
          (superchat--annotate-ttft 1.25)
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore)
                    ((symbol-function 'superchat--insert-prompt) #'ignore))
            (superchat--process-llm-result "formatted final answer"))
          (let ((rendered (buffer-string)))
            (should (string-match-p "Tool result: read-file" rendered))
            (should (string-match-p "contents" rendered))
            (should (string-match-p "formatted final answer" rendered))
            (should (string-match-p "TTFT: 1.25s" rendered))
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
            (superchat--agent-render-tool-call
             "read-file" '("README.md") (current-buffer))
            (superchat--agent-render-tool-result
             "read-file" "contents" (current-buffer))
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

(ert-deftest test-render-late-stream-chunk-stays-in-owned-region ()
  "A chunk after a persistent notice is finalized exactly once in the region."
  (let ((superchat-buffer-name " *superchat-late-chunk-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (setq-local superchat--response-start-marker nil
                      superchat--assistant-response-start-marker nil
                      superchat--assistant-response-end-marker nil
                      superchat--current-response-parts nil
                      superchat--prompt-start nil)
          (insert "* User: inspect this\n")
          (superchat--prepare-for-response)
          (superchat--update-status "thinking")
          (superchat--stream-llm-result "EARLY")
          (superchat--insert-persistent-system-message "COMPACT-NOTICE")
          (superchat--stream-llm-result "LATE")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
            (superchat--process-llm-result "FINAL LATE"))
          (let* ((text (buffer-string))
                 (final-position (string-match "FINAL LATE" text))
                 (notice-position (string-match "COMPACT-NOTICE" text))
                 (prompt-position
                  (string-match "\\* User: \\(\\?:\\n\\|\\'\\)" text)))
            (should (= 1 (test-render--match-count "\\_<LATE\\_>" text)))
            (should (= 1 (test-render--match-count "COMPACT-NOTICE" text)))
            (should-not
             (text-property-any (point-min) (point-max)
                                'face 'superchat-streaming-pending))
            (should (< final-position notice-position))
            (should (< notice-position prompt-position))
            (should-not superchat--assistant-response-start-marker)
            (should-not superchat--assistant-response-end-marker)
            (should-not superchat--current-response-parts)))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-render-tool-round-after-notice-is-exactly-once ()
  "Tool rounds keep late narration, records, and the final prompt ordered."
  (let ((superchat-buffer-name " *superchat-tool-notice-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (setq-local superchat--response-start-marker nil
                      superchat--assistant-response-start-marker nil
                      superchat--assistant-response-end-marker nil
                      superchat--current-response-parts nil
                      superchat--prompt-start nil)
          (insert "* User: inspect\n")
          (superchat--prepare-for-response)
          (superchat--update-status "thinking")
          (superchat--stream-llm-result "ROUND1-EARLY")
          (superchat--insert-persistent-system-message "COMPACT-NOTICE")
          (superchat--stream-llm-result "ROUND1-LATE")
          (superchat--agent-render-tool-call
           "read-file" '("README.md") (current-buffer))
          (superchat--agent-render-tool-result
           "read-file" "TOOL-CONTENTS" (current-buffer))
          (superchat--stream-llm-result "ROUND2-NARRATE")
          (superchat--agent-render-tool-call
           "search-text" '("marker") (current-buffer))
          (superchat--agent-render-tool-result
           "search-text" "SECOND-CONTENTS" (current-buffer))
          (superchat--stream-llm-result "ROUND3-RAW")
          (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
            (superchat--process-llm-result "ROUND3-FINAL"))
          (let ((text (buffer-string)))
            (dolist (regexp '("ROUND1-EARLY" "ROUND1-LATE"
                              "Tool call: read-file" "TOOL-CONTENTS"
                              "ROUND2-NARRATE" "Tool call: search-text"
                              "SECOND-CONTENTS" "ROUND3-FINAL"
                              "COMPACT-NOTICE"))
              (should (= 1 (test-render--match-count regexp text))))
            (should-not (string-match-p "ROUND3-RAW" text))
            (should-not
             (text-property-any (point-min) (point-max)
                                'face 'superchat-streaming-pending))
            (should
             (< (string-match "ROUND1-LATE" text)
                (string-match "Tool call: read-file" text)))
            (should
             (< (string-match "TOOL-CONTENTS" text)
                (string-match "ROUND2-NARRATE" text)))
            (should
             (< (string-match "SECOND-CONTENTS" text)
                (string-match "ROUND3-FINAL" text)))
            (should
             (< (string-match "ROUND3-FINAL" text)
                (string-match "COMPACT-NOTICE" text)))
            (should
             (< (string-match "COMPACT-NOTICE" text)
                (string-match "\\* User: \\(\\?:\\n\\|\\'\\)" text)))
            (should-not superchat--assistant-response-start-marker)
            (should-not superchat--assistant-response-end-marker)
            (should-not superchat--current-response-parts)))
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
                  ;; Approved decision 3 (D14): `on-demand' attaches
                  ;; tools like `always' now, so this mock is called
                  ;; (with TOOL-NAMES) even for plain, keyword-free
                  ;; input; it stays a no-tools stub for this test's own
                  ;; concern.
                  (lambda (&optional _tool-names) nil))
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

(ert-deftest test-streaming-reasoning-payloads-are-forwarded-normalized ()
  "Cumulative reasoning payloads reach the stream sink as deltas without a warning."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-list nil)
           (messages nil)
           (reasoning-chunks nil)
           final-result)
       (cl-letf (((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (push (apply #'format format-string args) messages)))
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt partial-cb response-cb
                           _error-cb &optional _multi-output)
                    ;; This is the shape delivered by llm.el's streaming
                    ;; accumulator: each partial contains all reasoning seen
                    ;; so far, not just the newly arrived suffix.
                    (funcall partial-cb '(:reasoning "private thought"))
                    (funcall partial-cb
                             '(reasoning "private thought\nlegacy thought"))
                    (funcall partial-cb
                             '(:reasoning "private thought\nlegacy thought\nfinal thought"))
                    ;; A provider reset is not a prefix: treat the new
                    ;; partial as the replacement value rather than trying
                    ;; to compute a negative suffix.
                    (funcall partial-cb '(reasoning "reset thought"))
                    (funcall partial-cb '(:reasoning "reset thought plus"))
                    (funcall response-cb "visible answer"))))
         (superchat--llm-generate-answer
          "answer"
          (lambda (result) (setq final-result result))
          (lambda (chunk) (push chunk reasoning-chunks)))
         (should (equal "visible answer" final-result))
         (should (equal '((:reasoning "private thought")
                         (:reasoning "\nlegacy thought")
                         (:reasoning "\nfinal thought")
                         (:reasoning "reset thought")
                         (:reasoning " plus"))
                        (nreverse reasoning-chunks)))
         (should-not
          (cl-some (lambda (message)
                     (string-match-p "unhandled stream payload" message))
                   messages)))))))

(defun test-reasoning--assert-visibility (text hidden)
  "Assert TEXT exists and every occurrence has actual HIDDEN visibility."
  (save-excursion
    (goto-char (point-min))
    (should (search-forward text nil t))
    (goto-char (point-min))
    (while (search-forward text nil t)
      (let ((end (point)))
        (dotimes (offset (length text))
          (should (eq (not (null (invisible-p (- end (1+ offset))))) hidden)))))))

(defun test-reasoning--fontify ()
  "Run real Org fontification and prove its heading face was installed."
  (font-lock-set-defaults)
  (font-lock-fontify-region (point-min) (point-max))
  (save-excursion
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* Thinking\\|^\\* User:" nil t))
    (should (memq (if (string-prefix-p "**" (match-string 0))
                      'org-level-2 'org-level-1)
                  (let ((face (get-text-property (match-beginning 0) 'face)))
                    (if (listp face) face (list face)))))))

(ert-deftest test-render-reasoning-block-remains-folded-after-finalize ()
  "Reasoning deltas form a retained, toggleable folded transcript block."
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (setq-local superchat-mode t
                  superchat--session-id "reasoning-render-session"
                  superchat--response-region-states nil
                  superchat--response-region-current-owner nil
                  superchat--prompt-start nil
                  superchat--current-response-parts nil)
      (insert "* User: explain\n")
      (superchat--response-region-prepare)
      (superchat--stream-llm-result '(:reasoning "first thought"))
      (superchat--stream-llm-result '(reasoning "second thought"))
      (superchat--stream-llm-result "visible stream")
      (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
        (superchat--process-llm-result "final answer"))
      (test-reasoning--fontify)
      (test-reasoning--assert-visibility "first thought" t)
      (test-reasoning--assert-visibility "** Thinking" nil)
      (test-reasoning--assert-visibility "** Assistant" nil)
      (test-reasoning--assert-visibility "final answer" nil)
      (goto-char (point-min))
      (should (search-forward "** Thinking" nil t))
      (let* ((heading (line-beginning-position))
             (overlay (get-text-property heading
                                         'superchat-reasoning-overlay)))
        (should (overlayp overlay))
        (should (overlay-get overlay 'invisible))
        (should (string-match-p "first thought.*second thought"
                                (buffer-string)))
        (should (string-match-p "\\*\\* Assistant:\nfinal answer"
                                (buffer-string)))
        (goto-char heading)
        (superchat-toggle-reasoning)
        (should-not (overlay-get overlay 'invisible))
        (test-reasoning--assert-visibility "first thought" nil)
        (superchat-toggle-reasoning)
        (test-reasoning--assert-visibility "first thought" t)
        (test-reasoning--assert-visibility "final answer" nil)))))

(ert-deftest test-render-reasoning-block-is-bounded-and-keeps-tail ()
  "A long Thinking block is capped while retaining its newest text."
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name))
          (superchat-reasoning-max-chars 96))
      (setq-local superchat-mode t
                  superchat--session-id "reasoning-cap-session"
                  superchat--response-region-states nil
                  superchat--response-region-current-owner nil
                  superchat--prompt-start nil
                  superchat--current-response-parts nil)
      (insert "* User: explain\n")
      (superchat--response-region-prepare)
      (superchat--stream-llm-result
       (list :reasoning (concat (make-string 180 ?A)
                                " opening thought that should be replaced.")))
      (superchat--stream-llm-result
       '(:reasoning " and the newest tail must survive the truncation."))
      (let* ((state (cdr (assq :pending superchat--response-region-states)))
             (start (marker-position (plist-get state :reasoning-body-start)))
             (end (marker-position (plist-get state :reasoning-body-end)))
             (body (buffer-substring-no-properties start end)))
        (should (<= (length (string-trim-right body))
                    superchat-reasoning-max-chars))
        (should (string-match-p "Thinking truncated" body))
        (should (string-suffix-p "newest tail must survive the truncation."
                                 (string-trim-right body))))
      (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
        (superchat--process-llm-result "final answer"))
      (test-reasoning--fontify)
      (test-reasoning--assert-visibility "Thinking truncated" t)
      (test-reasoning--assert-visibility "newest tail must survive" t)
      (test-reasoning--assert-visibility "** Thinking" nil)
      (test-reasoning--assert-visibility "** Assistant" nil)
      (test-reasoning--assert-visibility "final answer" nil)
      (let ((text (buffer-string)))
        (should (= 1 (test-render--match-count "Thinking truncated" text)))
        (should (= 1 (test-render--match-count
                      "newest tail must survive the truncation\."
                      text)))))))

(ert-deftest test-render-reasoning-rounds-hide-only-their-own-bodies ()
  "Nil round close retains independent folds without hiding tools or another owner."
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (superchat--insert-prompt)
      (superchat--response-region-begin "A")
      (superchat--response-region-add-reasoning "ROUND1-" "A")
      (superchat--response-region-add-reasoning "REASON" "A")
      (superchat--response-region-stream "NARRATION" "A")
      (test-reasoning--assert-visibility "ROUND1-REASON" t)
      (test-reasoning--assert-visibility "NARRATION" nil)
      (superchat--response-region-close nil nil "A")
      (superchat--response-region-close nil nil "A")
      (superchat--response-region-insert-transcript "TOOL-CALL\nTOOL-RESULT" :current-run)
      (superchat--response-region-begin "B")
      (superchat--response-region-stream "B-PART" "B")
      (superchat--response-region-add-reasoning "ROUND2-REASON" "A")
      (superchat--response-region-stream "PART2" "A")
      (superchat--response-region-close "FINAL" t "A")
      (test-reasoning--fontify)
      (dolist (text '("** Thinking" "** Assistant" "NARRATION" "FINAL"
                      "TOOL-CALL" "TOOL-RESULT" "B-PART" "* User:"))
        (test-reasoning--assert-visibility text nil))
      (let ((overlays (plist-get (cdr (assoc "A" superchat--response-region-states))
                                 :reasoning-overlays)))
        (should (= 2 (length overlays)))
        (dolist (overlay overlays)
          (test-reasoning--assert-visibility "ROUND1-REASON" t)
          (test-reasoning--assert-visibility "ROUND2-REASON" t)
          (goto-char (overlay-start overlay))
          (superchat-toggle-reasoning)
          (should-not (invisible-p (overlay-start overlay)))
          (dolist (other overlays)
            (unless (eq other overlay)
              (should (invisible-p (overlay-start other)))))
          (superchat-toggle-reasoning)
          (should (invisible-p (overlay-start overlay)))))
      (superchat--response-region-close "B-FINAL" t "B")
      (test-reasoning--assert-visibility "B-FINAL" nil)
      (test-reasoning--assert-visibility "FINAL" nil))))

(ert-deftest test-render-reasoning-real-runtime-custom-owner-visibility ()
  "Native normalization preserves body-only folds beside another owner's stream."
  (unless (and (featurep 'llm) (require 'llm-openai nil t)
               (sqlite-available-p))
    (ert-skip "Requires installed llm and SQLite"))
  (dolist (variant '(nonempty empty truncated))
    (let* ((directory (make-temp-file "superchat-reasoning-visibility-" t))
           (superchat-data-directory directory)
           (superchat-db--connection nil) (superchat-db--path nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (superchat-closure-terminal-guard-enabled t)
           (superchat-closure-shadow-enabled nil)
           (superchat-llm-backend (make-llm-openai :key "unused" :chat-model "visibility"))
           (superchat-llm-tools-enabled nil)
           (superchat-show-ttft nil) (superchat-response-timeout nil)
           (superchat-reasoning-max-chars (if (eq variant 'truncated) 40 20000))
           partial response handle)
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq-local superchat-mode t superchat-buffer-name (buffer-name)
                        superchat--session-id "reasoning-visibility")
            (superchat-db-open)
            (superchat--insert-prompt)
            (superchat--response-region-consume-prompt)
            (superchat--prepare-for-response)
            (cl-letf (((symbol-function 'llm-chat-streaming)
                       (lambda (_provider _prompt part done _error &optional _multi)
                         (setq partial part response done) nil)))
              (setq handle (superchat-runtime-test-submit
                            (list :prompt "Q" :buffer (current-buffer)
                                  :session-id "reasoning-visibility" :tools 'none
                                  :deadline nil :transcript-owner "R-CUSTOM")))
              (should (functionp partial))
              (unless (eq variant 'empty)
                (funcall partial '(reasoning "REASON-ONE"))
                (funcall partial '(reasoning "REASON-ONE REASON-TWO-LONGER-TAIL-XXXXXXXXXXXXXXXXXXXXXXXXXXXX")))
              (funcall partial "STREAMED")
              (test-reasoning--assert-visibility "STREAMED" nil)
              (superchat--response-region-begin "B")
              (superchat--response-region-stream "B-PART" "B")
              (funcall response "FINAL-ANSWER **md**"))
            (test-reasoning--fontify)
            (should (eq 'completed
                        (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)))
            (should (= 1 (cl-count-if
                          (lambda (event) (equal "run.completed" (plist-get event :type)))
                          (superchat-tape-replay-events "reasoning-visibility"))))
            (should (equal "B" superchat--response-region-current-owner))
            (should (equal '("B-PART")
                           (plist-get (cdr (assoc "B" superchat--response-region-states)) :parts)))
            (dolist (text '("** Assistant" "FINAL-ANSWER" "*md*" "B-PART" "* User:"))
              (test-reasoning--assert-visibility text nil))
            (let ((overlays (plist-get (cdr (assoc "R-CUSTOM" superchat--response-region-states))
                                       :reasoning-overlays)))
              (if (eq variant 'empty)
                  (progn
                    (should-not overlays)
                    (should-not (string-match-p "Thinking" (buffer-string))))
                (should (= 1 (length overlays)))
                (should (equal "R-CUSTOM" (overlay-get (car overlays) 'superchat-reasoning-owner)))
                (test-reasoning--assert-visibility "** Thinking" nil)
                (test-reasoning--assert-visibility
                 (if (eq variant 'truncated) "Thinking truncated" "REASON-ONE") t)))
            (superchat--response-region-close "B-FINAL" t "B")
            (test-reasoning--assert-visibility "B-FINAL" nil)
            (test-reasoning--assert-visibility "FINAL-ANSWER" nil))
        (maphash (lambda (run _) (superchat-runtime-cancel run "test cleanup"))
                 superchat-runtime--runs)
        (superchat-db-close)
        (delete-directory directory t)))))

(ert-deftest test-render-thinking-status-uses-runtime-clock-and-latest-reasoning ()
  "The streaming wait line projects Runtime elapsed time and latest thought."
  (let ((superchat-runtime--runs (make-hash-table :test #'equal))
        (now 120.0))
    (with-temp-buffer
      (let ((superchat-buffer-name (buffer-name)))
        (setq-local superchat-mode t
                    superchat--active-run-id "run-thinking"
                    superchat--response-region-states nil
                    superchat--response-region-current-owner nil
                    superchat--prompt-start nil
                    superchat--current-response-parts nil)
        (superchat-runtime--put
         (list :run-id "run-thinking"
               :session-id "thinking-session"
               :transcript-owner "owner-thinking"
               :status 'running-model
               :capability "chat"
               :model "model-thinking"
               :started-at 100.0))
        (unwind-protect
            (cl-letf (((symbol-function 'float-time)
                       (lambda (&rest _args) now)))
              (let ((superchat--runtime-transcript-owner "owner-thinking"))
                (superchat--response-region-prepare)
                (superchat--stream-llm-result
                 '(:reasoning "latest thought")))
              (should (string-match-p
                       "Thinking… 20s · latest thought"
                       (buffer-string))))
          (superchat--response-region-reset))))))

(ert-deftest test-runtime-phase-distinguishes-model-output-stages ()
  "The status band names prefill, reasoning, and answer streaming distinctly."
  (dolist (case '((waiting-first-token . "prefill/waiting first token")
                  (reasoning . "reasoning")
                  (answer-streaming . "answer streaming")))
    (should
     (equal (cdr case)
            (superchat--runtime-phase
             (list :status 'running-model :model-phase (car case)) t))))
  (should
   (string-match-p
    "last phase: reasoning"
    (superchat--runtime-phase
     '(:status timed-out :model-phase reasoning) nil))))

(ert-deftest test-async-dispatcher-falls-back-for-nonstreaming-tool-use ()
  "Tool requests use `llm-chat-async' unless streaming tool use is advertised."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'deepseek))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           ;; `all' so the baseline's own configured-names filter
           ;; (S09-fix-3 item 1) does not drop this fixture's arbitrarily
           ;; named tool -- only `all' passes every registered/cached name
           ;; through unfiltered.
           (superchat-llm-tool-names 'all)
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
                      ;; Approved decision 3 (D14): `on-demand' attaches
                      ;; tools like `always' now, so this mock is
                      ;; called (with TOOL-NAMES) for plain input too.
                      (lambda (&optional _tool-names) nil))
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
           ;; `all' so the baseline's own configured-names filter
           ;; (S09-fix-3 item 1) does not drop this fixture's arbitrarily
           ;; named tool -- only `all' passes every registered/cached name
           ;; through unfiltered.  The matching `--llm-tool-names-signature'
           ;; keeps `superchat-get-llm-tools' from treating that change as
           ;; a cache miss and rebuilding the real ~31-tool registry in
           ;; place of this fixture's single pre-populated tool -- this
           ;; test's own exact-equal `:runtime-selected-tools' assertion
           ;; below needs exactly `("fake-tool")', not the full registry.
           (superchat-llm-tool-names 'all)
           (superchat--llm-tool-names-signature 'all)
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
                 :model "gpt-4o-mini" :model-phase waiting-first-token
                 :context-used 6800
                 :output-reserve 1024
                 :context-limit 8192
                 :streaming t :streaming-tool-use t
                 :capability-source provider :streaming-enabled t
                 :runtime-selected-tools ("fake-tool")
                 :runtime-tool-decision baseline
                 :context-limit-source llm))
               (usage
                (:attempt 1 :input-tokens 7000 :output-tokens 500))
               (terminal (:status completed :response "done")))))))))))

(ert-deftest test-stream-events-publish-prefill-reasoning-and-answer-phases ()
  "Runtime phase events come from attempt, reasoning, and text delta seams."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-show-ttft nil)
           phases)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (pcase event
                  ('attempt-started
                   (push (plist-get value :model-phase) phases))
                  ('model-phase (push value phases))))))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt partial-cb response-cb
                             _error-cb &optional _multi-output)
                      (funcall partial-cb '(:reasoning "private thought"))
                      (funcall partial-cb "answer")
                      (funcall response-cb "answer")
                      'request-1)))
           (superchat--llm-generate-answer "hello" #'ignore #'ignore)
           (should
            (equal '(waiting-first-token reasoning answer-streaming)
                   (nreverse phases)))))))))

(ert-deftest test-llm-context-estimator-does-not-trust-generic-word-count ()
  "A one-word payload is measured from its serialized request, not words."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((provider (test-llm--make-mock-backend 'openai))
            (text (make-string 96000 ?x))
            (prompt (test-llm--make-native-chat-prompt text)))
       ;; The provider serializer is the boundary; the estimator itself is
       ;; intentionally real so this test covers the E-005 decision source.
       (cl-letf (((symbol-function 'llm-provider-chat-request)
                  #'test-llm--openai-like-chat-request)
                 ((symbol-function 'llm-count-tokens)
                  #'test-llm--generic-word-token-count))
         (should (> (superchat--llm-estimate-context-tokens provider prompt)
                    40000)))))))

(ert-deftest test-llm-context-estimator-falls-back-to-local-json ()
  "Provider-request JSON failure uses the local evidence JSON branch."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((provider (test-llm--make-mock-backend 'openai))
            (prompt (test-llm--make-native-chat-prompt "fallback two"))
            (opaque (make-marker))
            (expected
             (json-serialize
              (list :system (llm-chat-prompt-context prompt)
                    :messages (superchat--llm-evidence-messages prompt)
                    :tools
                    (superchat--llm-evidence-tool-records
                     (llm-chat-prompt-tools prompt)))))
            serialized estimate)
       (cl-letf (((symbol-function 'llm-provider-chat-request)
                  (lambda (&rest _args) (list :opaque opaque))))
         (setq serialized
               (superchat--llm-serialized-attempt provider prompt t))
         (let ((superchat--llm-preflight-streaming-p t))
           (setq estimate
                 (superchat--llm-estimate-context-tokens provider prompt))))
       (should (equal expected serialized))
       (should
        (= estimate
           (max 1
                (ceiling
                 (* (ceiling
                     (/ (length serialized)
                        superchat--llm-preflight-chars-per-token))
                    superchat--llm-preflight-safety-factor)))))))))

(ert-deftest test-llm-context-estimator-falls-back-to-lisp ()
  "Local evidence JSON failure uses the printed prompt branch."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (insert "opaque")
       (let* ((provider (test-llm--make-mock-backend 'openai))
              (opaque (point-marker))
              (interaction
               (make-llm-chat-prompt-interaction
                :role 'user
                :content "fallback three"
                :multi-turn-plist (list :opaque opaque)))
              (prompt
               (make-llm-chat-prompt
                :context "SYSTEM"
                :interactions (list interaction)))
              (local-evidence
               (list :system (llm-chat-prompt-context prompt)
                     :messages (superchat--llm-evidence-messages prompt)
                     :tools
                     (superchat--llm-evidence-tool-records
                      (llm-chat-prompt-tools prompt))))
              (expected (prin1-to-string prompt))
              serialized estimate)
         (should-error (json-serialize local-evidence))
         (cl-letf (((symbol-function 'llm-provider-chat-request)
                    (lambda (&rest _args) (list :opaque opaque))))
           (setq serialized
                 (superchat--llm-serialized-attempt provider prompt t))
           (let ((superchat--llm-preflight-streaming-p t))
             (setq estimate
                   (superchat--llm-estimate-context-tokens provider prompt))))
         (should (equal expected serialized))
         (should
          (= estimate
             (max 1
                  (ceiling
                   (* (ceiling
                       (/ (length serialized)
                          superchat--llm-preflight-chars-per-token))
                      superchat--llm-preflight-safety-factor))))))))))

(ert-deftest test-llm-context-preflight-allows-calibrated-wordy-request ()
  "The live-usage calibration fits 5000 short words in an 8192 window."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled nil)
           (superchat-llm-output-token-reserve 2048)
           (text (concat (mapconcat #'identity (make-list 5000 "a") " ")
                         " Reply with exactly OK."))
           (provider-calls 0)
           attempt final)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (eq event 'attempt-started)
                  (setq attempt value)))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model)
                      '(:limit 8192 :source override)))
                   ((symbol-function 'llm-provider-chat-request)
                    #'test-llm--openai-like-chat-request)
                   ((symbol-function 'llm-count-tokens)
                    #'test-llm--generic-word-token-count)
                   ((symbol-function 'llm-make-chat-prompt)
                    #'test-llm--make-native-chat-prompt)
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial response _error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (funcall response "provider-called")
                      'provider-request)))
           (superchat--llm-generate-answer
            text (lambda (value) (setq final value)) #'ignore)
           (should (= 1 provider-calls))
           (should (equal "provider-called" final))
           ;; The review's real provider reported 5015 input tokens.  Keep a
           ;; margin above that usage without crossing the 6144-token input
           ;; budget left by the requested output reserve.
           (should (>= (plist-get attempt :context-used) 5015))
           (should (<= (plist-get attempt :context-used) 6144))))))))

(ert-deftest test-llm-context-budget-preflight-gates-provider ()
  "An over-budget attempt fails before invoking the provider transport."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           ;; `nil' is the only value that disables the surface
           ;; baseline (contract §4.2 rule 4); `never' is not `nil' and
           ;; no longer rides on the deleted on-demand keyword
           ;; heuristic (approved decision 3, D14), so it would now
           ;; attach tools to LARGE-TEXT and change what this budget
           ;; test is measuring.
           (superchat-llm-tools-enabled nil)
           (superchat-llm-output-token-reserve 2048)
           (large-text (make-string 16000 ?x))
           (provider-calls 0)
           events)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (when (memq event '(attempt-started terminal))
                  (push (list event value) events)))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model) '(:limit 10000)))
                   ((symbol-function 'llm-provider-chat-request)
                    #'test-llm--openai-like-chat-request)
                   ((symbol-function 'llm-count-tokens)
                    #'test-llm--generic-word-token-count)
                   ((symbol-function 'llm-make-chat-prompt)
                    #'test-llm--make-native-chat-prompt)
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial response _error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (funcall response "within budget")
                      'provider-request)))
           (superchat--llm-generate-answer large-text #'ignore #'ignore)
           (should (= 0 provider-calls))
           (should (string-match-p
                    "文件过大.*输出预留 2048.*模型上下文为 10000"
                    (plist-get (cadar events) :response)))
           (setq events nil)
           (let ((superchat-llm-context-budget-exceeded-functions
                  (list (lambda (&rest _args)
                          (error "budget hook failed")))))
             (superchat--llm-generate-answer
              large-text #'ignore #'ignore)
             (should (string-match-p
                      "请求未发送"
                      (plist-get (cadar events) :response)))
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
                  large-text #'ignore #'ignore)
               (error (setq raised err)))
             (should-not raised)
             (should (string-match-p
                      "请求未发送"
                      (plist-get (cadar events) :response)))
             (should (= 0 provider-calls)))
           (let ((superchat--llm-request-max-tokens 1000))
             (superchat--llm-generate-answer
              large-text #'ignore #'ignore))
           (should (= 1 provider-calls))))))))

(ert-deftest test-llm-context-preflight-names-window-and-action-in-chat-response ()
  "An impossible request is rejected with the model window and next action."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'always)
           ;; `all' so the baseline's own configured-names filter
           ;; (S09-fix-3 item 1) does not drop this fixture's arbitrarily
           ;; named tool -- only `all' passes every registered/cached name
           ;; through unfiltered.
           (superchat-llm-tool-names 'all)
           (superchat-llm-tools-list
            (list (test-llm--make-tool :name "schema-probe")))
           (superchat-llm-output-token-reserve 2048)
           (large-text (make-string 16000 ?x))
           (provider-calls 0)
           estimated-tools final)
       (cl-letf (((symbol-function 'superchat--llm-context-capability)
                  (lambda (_provider _model)
                    '(:limit 8192 :source lm-studio-runtime)))
                 ((symbol-function 'llm-provider-chat-request)
                  (lambda (provider prompt streaming)
                    (let ((request
                           (test-llm--openai-like-chat-request
                            provider prompt streaming)))
                      (setq estimated-tools (plist-get request :tools))
                      request)))
                 ((symbol-function 'llm-count-tokens)
                  #'test-llm--generic-word-token-count)
                 ((symbol-function 'llm-make-chat-prompt)
                  #'test-llm--make-native-chat-prompt)
                 ((symbol-function 'llm-chat-streaming)
                  (lambda (&rest _args)
                    (cl-incf provider-calls))))
         (superchat--llm-generate-answer
          large-text
          (lambda (result) (setq final result))
          #'ignore)
         (should estimated-tools)
         (should (= 0 provider-calls))
         (should (string-match-p "文件过大" final))
         (should (string-match-p "模型上下文为 8192" final))
         (should (string-match-p "建议缩小或换模型" final)))))))

(ert-deftest test-adaptive-response-timeout-scales-with-size-and-has-hard-cap ()
  "Prompt/tool estimates lengthen first-token waits without crossing the cap."
  (let ((superchat-response-timeout-hard-limit 300))
    (should (= 120 (superchat-llm--adaptive-response-timeout 120 1024)))
    (let ((large (superchat-llm--adaptive-response-timeout 120 50000)))
      (should (> large 120))
      (should (<= large 300)))
    (should (= 300
               (superchat-llm--adaptive-response-timeout 120 1000000)))))

(ert-deftest test-first-answer-token-cancels-wait-timeout-but-not-hard-cap ()
  "Visible answer output removes the silence timer; wall-clock cap remains."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 0.05)
             (superchat-response-timeout-hard-limit 1)
             (superchat-show-ttft nil)
             (superchat-llm--distribution-adapters
              (list :activity-function #'superchat--extend-timeout))
             (timers-before (copy-sequence timer-list))
             partial-cb response-cb final)
         (unwind-protect
             (cl-letf (((symbol-function 'llm-chat-streaming)
                        (lambda (_provider _prompt partial response _error
                                 &optional _multi-output)
                          (setq partial-cb partial
                                response-cb response)
                          'provider-request)))
               (superchat--llm-generate-answer
                "hello" (lambda (value) (setq final value)) #'ignore)
               (let* ((created
                       (cl-remove-if
                        (lambda (timer) (memq timer timers-before))
                        timer-list))
                      (wait-timer superchat--active-timeout-timer)
                      (hard-timer
                       (cl-find-if (lambda (timer)
                                     (not (eq timer wait-timer)))
                                   created)))
                 (should (timerp wait-timer))
                 (should (timerp hard-timer))
                 (funcall partial-cb '(:reasoning "thinking"))
                 (funcall partial-cb "A")
                 (should-not (memq wait-timer timer-list))
                 (should-not superchat--active-timeout-timer)
                 (should (memq hard-timer timer-list))
                 (funcall response-cb "A")
                 (should (equal "A" final))
                 (should-not (memq hard-timer timer-list))))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(ert-deftest test-real-hard-timeout-survives-reasoning-and-tool-activity ()
  "Real timer ordering lets the hard wall terminate the Runtime Run."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 0.05)
             (superchat-response-timeout-hard-limit 1)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-llm--distribution-adapters
              (list :activity-function #'superchat--extend-timeout))
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-turn-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             partial-cb handle wait-timer hard-timer started-at)
         (unwind-protect
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (_event) (cl-incf next-event-id)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--stream-llm-result) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (_provider _prompt partial _response _error
                                 &optional _multi-output)
                          (setq partial-cb partial)
                          'provider-request)))
               (setq started-at (float-time)
                     handle
                     (superchat-runtime-test-submit
                      (list :type :llm-query
                            :input "timer"
                            :clean-input "timer"
                            :prompt "timer"
                            :session-id "session-real-hard-timeout"
                            :buffer (current-buffer)
                            :buffer-name (buffer-name)
                            :deadline nil)))
               (setq wait-timer superchat--active-timeout-timer
                     hard-timer
                     (cl-find-if
                      (lambda (timer)
                        (and (not (memq timer timers-before))
                             (not (eq timer wait-timer))))
                      timer-list))
               (should (timerp wait-timer))
               (should (timerp hard-timer))
               (let ((hard-time (copy-tree (timer--time hard-timer))))
                 (funcall partial-cb '(:reasoning "thinking"))
                 (funcall partial-cb '(tool-call "probe"))
                 (should (equal hard-time (timer--time hard-timer))))
               (should
                (test-llm--pump-timers-until
                 (lambda ()
                   (eq 'timed-out
                       (plist-get
                        (superchat-runtime-status
                         (plist-get handle :run-id))
                        :status)))
                 1.8))
               (should (< (- (float-time) started-at) 1.8))
               (should (eq 'timed-out
                           (plist-get
                            (superchat-runtime-status
                             (plist-get handle :run-id))
                            :status)))
               (should-not (memq wait-timer timer-list))
               (should-not (memq hard-timer timer-list)))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(ert-deftest test-overlapping-runtime-runs-own-their-transport-timers ()
  "Activity and terminal cleanup stay within one overlapping Runtime Run."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 0.10)
             (superchat-response-timeout-hard-limit 1)
             (superchat--timeout-extension-amount 0.50)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-llm--distribution-adapters
              (list :activity-function #'superchat--extend-timeout))
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-turn-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             (calls 0)
             partial-1 partial-2 response-2
             handle-1 handle-2 wait-1 wait-2 hard-1 hard-2)
         (unwind-protect
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (_event) (cl-incf next-event-id)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--stream-llm-result) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (_provider _prompt partial response _error
                                 &optional _multi-output)
                          (cl-incf calls)
                          (if (= calls 1)
                              (setq partial-1 partial)
                            (setq partial-2 partial
                                  response-2 response))
                          (intern (format "overlap-request-%d" calls)))))
               (let ((before (copy-sequence timer-list)))
                 (setq handle-1
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "one"
                              :clean-input "one"
                              :prompt "one"
                              :session-id "session-overlap"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       wait-1 superchat--active-timeout-timer
                       hard-1
                       (cl-find-if
                        (lambda (timer)
                          (and (not (memq timer before))
                               (not (eq timer wait-1))))
                        timer-list)))
               (let ((before (copy-sequence timer-list)))
                 (setq handle-2
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "two"
                              :clean-input "two"
                              :prompt "two"
                              :session-id "session-overlap"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       wait-2 superchat--active-timeout-timer
                       hard-2
                       (cl-find-if
                        (lambda (timer)
                          (and (not (memq timer before))
                               (not (eq timer wait-2))))
                        timer-list)))
               (should (cl-every #'timerp
                                 (list wait-1 wait-2 hard-1 hard-2)))
               (let ((wait-1-before (float-time (timer--time wait-1)))
                     (wait-2-before (float-time (timer--time wait-2)))
                     (hard-1-before (copy-tree (timer--time hard-1)))
                     (hard-2-before (copy-tree (timer--time hard-2))))
                 (funcall partial-1 '(:reasoning "run-one-active"))
                 (should (> (- (float-time (timer--time wait-1))
                               wait-1-before)
                            0.49))
                 (should (< (abs (- (float-time (timer--time wait-2))
                                    wait-2-before))
                            0.01))
                 (should (equal hard-1-before (timer--time hard-1)))
                 (should (equal hard-2-before (timer--time hard-2))))
               (let ((superchat--tool-execution-context
                      (list :run-id (plist-get handle-1 :run-id)))
                     (hard-1-before (copy-tree (timer--time hard-1)))
                     (hard-2-before (copy-tree (timer--time hard-2))))
                 (let ((wait-1-before (float-time (timer--time wait-1)))
                       (wait-2-before (float-time (timer--time wait-2))))
                   (cl-letf (((symbol-function 'shell-command-to-string)
                              (lambda (_command) "tool complete")))
                     (should
                      (equal "tool complete"
                             (superchat-tool-shell-command "ignored"))))
                   (should (> (- (float-time (timer--time wait-1))
                                 wait-1-before)
                              0.49))
                   (should (< (abs (- (float-time (timer--time wait-2))
                                      wait-2-before))
                              0.01)))
                 (let ((wait-1-before (float-time (timer--time wait-1)))
                       (wait-2-before (float-time (timer--time wait-2))))
                   (cl-letf (((symbol-function 'read-multiple-choice)
                              (lambda (_prompt _choices &optional _help)
                                (cons ?y "yes, allow once"))))
                     (should
                      (superchat-authority-confirm
                       '(:tool-name "read_file"
                         :args nil
                         :effects ((:effect fs-read :resource "fixture"))
                         :request-hash "fixture-hash"))))
                   (should (> (- (float-time (timer--time wait-1))
                                 wait-1-before)
                              0.49))
                   (should (< (abs (- (float-time (timer--time wait-2))
                                      wait-2-before))
                              0.01)))
                 (should (equal hard-1-before (timer--time hard-1)))
                 (should (equal hard-2-before (timer--time hard-2))))
               ;; Keep Run 2 open independently, then cross both original
               ;; silence deadlines before terminating the newer Run first.
               (funcall partial-2 '(:reasoning "run-two-active"))
               (let ((deadline (+ (float-time) 0.20)))
                 (while (< (float-time) deadline)
                   (accept-process-output nil 0.01)))
               (should
                (eq 'running-model
                    (plist-get
                     (superchat-runtime-status (plist-get handle-1 :run-id))
                     :status)))
               (should
                (eq 'running-model
                    (plist-get
                     (superchat-runtime-status (plist-get handle-2 :run-id))
                     :status)))
               (should (eq superchat--active-timeout-timer wait-2))
               (funcall response-2 "two complete")
               (should
                (eq 'completed
                    (plist-get
                     (superchat-runtime-status (plist-get handle-2 :run-id))
                     :status)))
               (should
                (eq 'running-model
                    (plist-get
                     (superchat-runtime-status (plist-get handle-1 :run-id))
                     :status)))
               (should (memq wait-1 timer-list))
               (should (memq hard-1 timer-list))
               (should-not (memq wait-2 timer-list))
               (should-not (memq hard-2 timer-list))
               (should-not superchat--active-timeout-timer)
               (should
                (superchat-runtime-cancel
                 (plist-get handle-1 :run-id) "one cancelled"))
               (should
                (eq 'cancelled
                    (plist-get
                     (superchat-runtime-status (plist-get handle-1 :run-id))
                     :status)))
               (should-not (memq wait-1 timer-list))
               (should-not (memq hard-1 timer-list)))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(ert-deftest test-overlapping-runtime-pause-target-keeps-newer-owner-timers ()
  "An older tool boundary cannot consume a newer Run's pause request."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 10)
             (superchat-response-timeout-hard-limit 20)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-turn-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             (calls 0)
             response-1 handle-1 handle-2 owner-2 wait-2 hard-2)
         (unwind-protect
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (_event) (cl-incf next-event-id)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--stream-llm-result) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'superchat--response-region-close)
                        #'ignore)
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (_provider _prompt _partial response _error
                                 &optional _multi-output)
                          (cl-incf calls)
                          (when (= calls 1)
                            (setq response-1 response))
                          (intern (format "pause-owner-request-%d" calls)))))
               (setq handle-1
                     (superchat-runtime-test-submit
                      (list :type :llm-query
                            :input "one" :clean-input "one" :prompt "one"
                            :session-id "session-pause-owner"
                            :buffer (current-buffer)
                            :buffer-name (buffer-name) :deadline nil)))
               (setq handle-2
                     (superchat-runtime-test-submit
                      (list :type :llm-query
                            :input "two" :clean-input "two" :prompt "two"
                            :session-id "session-pause-owner"
                            :buffer (current-buffer)
                            :buffer-name (buffer-name) :deadline nil)))
               (setq owner-2
                     (plist-get
                      (superchat-runtime--entry (plist-get handle-2 :run-id))
                      :transport-timer-owner)
                     wait-2
                     (superchat-llm--transport-timer-owner-wait-timer owner-2)
                     hard-2
                     (superchat-llm--transport-timer-owner-hard-timer owner-2))
               (setq-local superchat--active-run-id
                           (plist-get handle-2 :run-id))
               (superchat-pause)
               (should
                (equal
                 (plist-get handle-2 :run-id)
                 (superchat-llm--transport-timer-owner-run-id owner-2)))
               (let ((wait-2-before (copy-tree (timer--time wait-2)))
                     (hard-2-before (copy-tree (timer--time hard-2))))
                 ;; Run 1 reaches the tool-followup checkpoint before the
                 ;; paused target Run 2 reaches its own boundary.
                 (funcall response-1 '((fake-tool . "result")))
                 (should (= calls 3))
                 (should superchat--pause-requested)
                 (should (equal superchat--pause-run-id
                                (plist-get handle-2 :run-id)))
                 (should-not superchat--pending-send-request)
                 (should (memq wait-2 timer-list))
                 (should (memq hard-2 timer-list))
                 (should (equal wait-2-before (timer--time wait-2)))
                 (should (equal hard-2-before (timer--time hard-2)))
                 (should
                  (eq wait-2
                      (superchat-llm--transport-timer-owner-wait-timer
                       owner-2)))
                 (should
                  (eq hard-2
                      (superchat-llm--transport-timer-owner-hard-timer
                       owner-2)))
                 (should
                  (eq owner-2
                      (plist-get
                       (superchat-runtime--entry
                        (plist-get handle-2 :run-id))
                       :transport-timer-owner)))
                 (should
                  (equal
                   (plist-get handle-2 :run-id)
                   (superchat-llm--transport-timer-owner-run-id owner-2)))
                 (should (eq superchat--active-timeout-timer wait-2))
                 (should (eq superchat--active-timeout-timer-owner owner-2))))
           (dolist (handle (list handle-1 handle-2))
             (when handle
               (ignore-errors
                 (superchat-runtime-cancel
                  (plist-get handle :run-id) "cleanup"))))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(defun test-llm--with-pause-pending-runtime-harness (body)
  "Call BODY inside a real overlapping-Runtime pause harness.
BODY receives SUBMIT, RESPONSE, CALL-COUNT, and EVENTS functions.  The
transport and Tape persistence are test boundaries; Runtime lifecycle, pause
checkpointing, timer ownership, resume, steer, and terminal cleanup are real."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 10)
             (superchat-response-timeout-hard-limit 20)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-turn-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             (calls 0)
             responses events handles)
         (unwind-protect
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (event)
                          (push (copy-tree event t) events)
                          (cl-incf next-event-id)))
                       ((symbol-function 'superchat--update-status) #'ignore)
                       ((symbol-function 'superchat--stream-llm-result) #'ignore)
                       ((symbol-function 'superchat--process-llm-result) #'ignore)
                       ((symbol-function 'superchat--response-region-close)
                        #'ignore)
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (_provider _prompt _partial response _error
                                 &optional _multi-output)
                          (cl-incf calls)
                          (setq responses
                                (append responses (list response)))
                          (intern
                           (format "pause-pending-request-%d" calls)))))
               (cl-labels
                   ((submit
                     (label)
                     (let ((handle
                            (superchat-runtime-test-submit
                             (list :type :llm-query
                                   :input label
                                   :clean-input label
                                   :prompt label
                                   :session-id "session-pause-pending"
                                   :buffer (current-buffer)
                                   :buffer-name (buffer-name)
                                   :deadline nil))))
                       (push handle handles)
                       handle))
                    (response
                     (ordinal)
                     (or (nth (1- ordinal) responses)
                         (error "No provider response callback %d" ordinal)))
                    (call-count () calls)
                    (recorded-events ()
                                     (nreverse (copy-sequence events))))
                 (funcall body #'submit #'response #'call-count
                          #'recorded-events)))
           (dolist (handle handles)
             (ignore-errors
               (superchat-runtime-cancel
                (plist-get handle :run-id) "cleanup")))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(ert-deftest test-pause-pending-refuses-retarget-and-resumes-own-run ()
  "A pending Run 1 pause rejects Run 2 and resumes only Run 1."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending-1 superchat--pending-send-request)
              (owner-1
               (plist-get (superchat-runtime--entry run-1)
                          :transport-timer-owner))
              repeat-message)
         ;; Repeating pause for the pending owner remains a supported no-op.
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (setq repeat-message
                            (apply #'format format-string args)))))
           (superchat-pause))
         (should (string-match-p "Pause requested" repeat-message))
         (should (eq pending-1 superchat--pending-send-request))
         (let* ((handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id))
              rejection-message)
         (setq-local superchat--active-run-id run-2)
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (setq rejection-message
                            (apply #'format format-string args)))))
           (superchat-pause))
         (should (string-match-p "另一 Run 已暂停等待恢复"
                                 rejection-message))
         (should (equal run-1 superchat--pause-run-id))
         (should (eq pending-1 superchat--pending-send-request))
         (should (superchat-llm--pending-pause-p pending-1))
         (should (equal run-1
                        (superchat-llm--pending-pause-run-id pending-1)))
         (should (eq owner-1
                     (superchat-llm--pending-pause-transport-owner
                      pending-1)))
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _args) "steer run one"))
                   ((symbol-function 'message) #'ignore))
           (superchat-resume))
         (should (= 3 (funcall call-count)))
         (should
          (eq 'pause-pending-request-3
              (plist-get (superchat-runtime--entry run-1)
                         :transport-request)))
         (should
          (eq 'pause-pending-request-2
              (plist-get (superchat-runtime--entry run-2)
                         :transport-request)))
         (should (equal run-1 superchat--resume-timeout-run-id))
         (should
          (cl-find-if
           (lambda (event)
             (and (equal "run.steered" (plist-get event :type))
                  (equal run-1 (plist-get event :run-id))))
           (funcall recorded-events)))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request)))))))

(ert-deftest test-pause-pending-survives-other-run-boundary ()
  "Run 2 reaches its boundary normally without replacing Run 1 pending."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count _recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending-1 superchat--pending-send-request)
              (handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id)))
         (setq-local superchat--active-run-id run-2)
         (cl-letf (((symbol-function 'message) #'ignore))
           (superchat-pause))
         (funcall (funcall response 2) '((fake-tool . "result")))
         (should (= 3 (funcall call-count)))
         (should
          (eq 'pause-pending-request-3
              (plist-get (superchat-runtime--entry run-2)
                         :transport-request)))
         (should (equal run-1 superchat--pause-run-id))
         (should (eq pending-1 superchat--pending-send-request))
         (should (equal run-1
                        (superchat-llm--pending-pause-run-id pending-1)))
         (should (eq 'tool-followup
                     (superchat-llm--pending-pause-reason pending-1))))))))

(ert-deftest test-pause-pending-terminal-cleanup-is-owner-exact ()
  "Run 2 terminal cleanup preserves Run 1 pending; Run 1 clears it."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response _call-count _recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending-1 superchat--pending-send-request)
              (handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id)))
         (setq-local superchat--active-run-id run-2)
         (cl-letf (((symbol-function 'message) #'ignore))
           (superchat-pause))
         (should (superchat-runtime-cancel run-2 "cancel run two"))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-2) :status)))
         (should (equal run-1 superchat--pause-run-id))
         (should (eq pending-1 superchat--pending-send-request))
         (should (superchat-runtime-cancel run-1 "cancel run one"))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-1) :status)))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request))))))

(ert-deftest test-stale-active-cancel-cancels-live-pending-owner ()
  "A stale active Run cannot make UI cancel discard a live pending owner."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response _call-count _recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending-1 superchat--pending-send-request)
              (handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id))
              cancel-message)
         ;; Run 2 is the latest View Run, but Run 1 still owns the pending
         ;; continuation after Run 2's retarget request is refused.
         (setq-local superchat--active-run-id run-2)
         (cl-letf (((symbol-function 'message) #'ignore))
           (superchat-pause))
         (cl-letf (((symbol-function 'read-string) (lambda (&rest _args) ""))
                   ((symbol-function 'message) #'ignore))
           (superchat-cancel))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-2) :status)))
         (should (eq pending-1 superchat--pending-send-request))
         (should (member run-1 (superchat-runtime-active-runs)))

         ;; ACTIVE-RUN-ID still names terminal Run 2.  The next UI cancel must
         ;; cancel the live pending owner instead of clearing its continuation.
         (cl-letf (((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (setq cancel-message
                            (apply #'format format-string args)))))
           (superchat-cancel))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-1) :status)))
         (should-not (member run-1 (superchat-runtime-active-runs)))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request)
         (should (string-match-p "已取消暂停中的 Run" cancel-message))
         (should (string-match-p (regexp-quote run-1) cancel-message))

         ;; The slot is reusable only because its owner reached terminal and
         ;; performed owner-exact release.
         (let* ((handle-3 (funcall submit "three"))
                (run-3 (plist-get handle-3 :run-id)))
           (setq-local superchat--active-run-id run-3)
           (cl-letf (((symbol-function 'message) #'ignore))
             (superchat-pause))
           (should superchat--pause-requested)
           (should (equal run-3 superchat--pause-run-id))))))))

(defun test-llm--assert-renamed-pending-owner-terminal-cleanup
    (terminal expected-status)
  "Assert rename-safe pending cleanup for TERMINAL and EXPECTED-STATUS."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response _call-count _recorded-events)
     (let* ((handle-1 (funcall submit "rename-owner"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let ((old-name (buffer-name)))
         (should (superchat-llm--pending-pause-p
                  superchat--pending-send-request))
         (rename-buffer
          (generate-new-buffer-name " *superchat-renamed-pause-view*") t)
         (should-not (get-buffer old-name))
         (pcase terminal
           ('cancel
            (should (superchat-runtime-cancel run-1 "cancel renamed owner")))
           ('timeout
            (superchat-runtime--deadline-expired run-1))
           ('complete
            (should (superchat-runtime--finish run-1 'completed "complete")))
           (_ (error "Unknown terminal test path: %S" terminal)))
         (should (eq expected-status
                     (plist-get (superchat-runtime-status run-1) :status)))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request)

         (let* ((handle-2 (funcall submit "after-rename-terminal"))
                (run-2 (plist-get handle-2 :run-id)))
           (setq-local superchat--active-run-id run-2)
           (cl-letf (((symbol-function 'message) #'ignore))
             (superchat-pause))
           (should superchat--pause-requested)
           (should (equal run-2 superchat--pause-run-id))))))))

(ert-deftest test-renamed-view-cancel-clears-pending-owner ()
  "Cancel locates and clears its pending owner after the View is renamed."
  (test-llm--assert-renamed-pending-owner-terminal-cleanup
   'cancel 'cancelled))

(ert-deftest test-renamed-view-timeout-clears-pending-owner ()
  "Timeout locates and clears its pending owner after the View is renamed."
  (test-llm--assert-renamed-pending-owner-terminal-cleanup
   'timeout 'timed-out))

(ert-deftest test-renamed-view-complete-clears-pending-owner ()
  "Completion locates and clears its pending owner after the View is renamed."
  (test-llm--assert-renamed-pending-owner-terminal-cleanup
   'complete 'completed))

(ert-deftest test-three-run-pause-retarget-keeps-one-owner-through-reuse ()
  "Three Runs cannot split pending, steer, timer, reason, or owner identity."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending-1 superchat--pending-send-request)
              (handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id))
              (handle-3 (funcall submit "three"))
              (run-3 (plist-get handle-3 :run-id))
              rejection-messages continuation-1 owner-1)
         (dolist (run-id (list run-2 run-3))
           (setq-local superchat--active-run-id run-id)
           (cl-letf (((symbol-function 'message)
                      (lambda (format-string &rest args)
                        (push (apply #'format format-string args)
                              rejection-messages))))
             (superchat-pause))
           (should (equal run-1 superchat--pause-run-id))
           (should (eq pending-1 superchat--pending-send-request)))
         (should (= 2 (length rejection-messages)))
         (should
          (cl-every (lambda (text)
                      (string-match-p "另一 Run 已暂停等待恢复" text))
                    rejection-messages))
         (should (superchat-llm--pending-pause-p pending-1))
         (setq continuation-1
               (superchat-llm--pending-pause-continuation pending-1)
               owner-1
               (superchat-llm--pending-pause-transport-owner pending-1))
         (should (equal run-1
                        (superchat-llm--pending-pause-run-id pending-1)))
         (should (eq owner-1
                     (plist-get (superchat-runtime--entry run-1)
                                :transport-timer-owner)))
         (should (eq continuation-1
                     (superchat-llm--pending-pause-continuation pending-1)))
         (should (eq 'tool-followup
                     (superchat-llm--pending-pause-reason pending-1)))
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _args) "steer run one"))
                   ((symbol-function 'message) #'ignore))
           (superchat-resume))
         (should (= 4 (funcall call-count)))
         (should (equal run-1 superchat--resume-timeout-run-id))
         (should (superchat-runtime-cancel run-1 "finish run one"))
         (should-not superchat--resume-timeout-run-id)
         (setq-local superchat--active-run-id run-3)
         (cl-letf (((symbol-function 'message) #'ignore))
           (superchat-pause))
         (funcall (funcall response 3) '((fake-tool . "result")))
         (let* ((pending-3 superchat--pending-send-request)
                (owner-3
                 (plist-get (superchat-runtime--entry run-3)
                            :transport-timer-owner)))
           (should (superchat-llm--pending-pause-p pending-3))
           (should (equal run-3
                          (superchat-llm--pending-pause-run-id pending-3)))
           (should (eq owner-3
                       (superchat-llm--pending-pause-transport-owner
                        pending-3)))
           (should (eq 'tool-followup
                       (superchat-llm--pending-pause-reason pending-3)))
           (should-not
            (eq continuation-1
                (superchat-llm--pending-pause-continuation pending-3)))
           (cl-letf (((symbol-function 'read-string)
                      (lambda (&rest _args) "steer run three"))
                     ((symbol-function 'message) #'ignore))
             (superchat-resume))
           (should (= 5 (funcall call-count)))
           (should (equal run-3 superchat--resume-timeout-run-id))
           (let ((steer-events
                  (cl-remove-if-not
                   (lambda (event)
                     (equal "run.steered" (plist-get event :type)))
                   (funcall recorded-events))))
             (should (equal (list run-1 run-3)
                            (mapcar (lambda (event)
                                      (plist-get event :run-id))
                                    steer-events))))))))))

(ert-deftest test-pause-resume-fails-closed-on-identity-mismatch ()
  "Resume clears a split target/record identity before steer or continuation."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((handle-2 (funcall submit "two"))
              (run-2 (plist-get handle-2 :run-id))
              mismatch-message)
         ;; Construct the pre-fix split identity directly: target Run 2 while
         ;; the record and continuation still belong to Run 1.
         (setq-local superchat--active-run-id run-2
                     superchat--pause-run-id run-2)
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _args) "must not steer"))
                   ((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (setq mismatch-message
                            (apply #'format format-string args)))))
           (superchat-resume))
         (should (= 2 (funcall call-count)))
         (should-not
          (cl-find-if
           (lambda (event)
             (equal "run.steered" (plist-get event :type)))
           (funcall recorded-events)))
         (should (string-match-p "Run 身份不一致" mismatch-message))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request)
         (should-not superchat--resume-timeout-run-id))))))

(ert-deftest test-pause-resume-rechecks-live-run-after-steer ()
  "A Run sealed during steer cannot invoke its pending continuation."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count _recorded-events)
     (let* ((handle-1 (funcall submit "one"))
            (run-1 (plist-get handle-1 :run-id)))
       (setq-local superchat--active-run-id run-1)
       (superchat-pause)
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let* ((pending superchat--pending-send-request)
              (continuation
               (superchat-llm--pending-pause-continuation pending))
              (append-event
               (symbol-function 'superchat-tape-append-event))
              (continuation-invoked nil)
              resume-message)
         ;; The harness already treats Tape persistence as a boundary.  Seal
         ;; the real Runtime Run synchronously after its real steer append;
         ;; liveness lookup, steer, cancellation, and continuation choice stay
         ;; on product paths.
         (setf (superchat-llm--pending-pause-continuation pending)
               (lambda (reason)
                 (setq continuation-invoked t)
                 (funcall continuation reason)))
         (cl-letf (((symbol-function 'superchat-tape-append-event)
                    (lambda (event)
                      (let ((result (funcall append-event event)))
                        (when (equal "run.steered" (plist-get event :type))
                          (superchat-runtime-cancel
                           run-1 "terminal during steer"))
                        result)))
                   ((symbol-function 'read-string)
                    (lambda (&rest _args) "steer before terminal"))
                   ((symbol-function 'message)
                    (lambda (format-string &rest args)
                      (setq resume-message
                            (apply #'format format-string args)))))
           (superchat-resume))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-1) :status)))
         (should-not continuation-invoked)
         (should (= 1 (funcall call-count)))
         (should-not superchat--pause-requested)
         (should-not superchat--pause-run-id)
         (should-not superchat--pending-send-request)
         (should-not superchat--resume-timeout-run-id)
         (should (string-match-p "no longer active" resume-message)))))))

(ert-deftest test-pending-pause-validator-shares-llm-attempt-reasons ()
  "The View validator accepts every attempt reason owned by the LLM layer."
  (with-temp-buffer
    (let ((owner
           (superchat-llm--make-transport-timer-owner
            :buffer (current-buffer) :run-id "shared-reason-run")))
      (dolist (reason superchat-llm-provider-attempt-reasons)
        (should
         (superchat--pending-pause-valid-p
          (superchat-llm--make-pending-pause
           :run-id "shared-reason-run"
           :transport-owner owner
           :reason reason
           :continuation #'ignore)))))))

(ert-deftest test-provider-effect-rechecks-runtime-live-after-preparation ()
  "A Run sealed during attempt preparation sends no provider request."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled nil)
           (live t)
           (provider-calls 0)
           final)
       (let ((superchat--llm-run-active-p (lambda () live))
             (superchat--llm-runtime-observer
              (lambda (event _value)
                (when (eq event 'attempt-started)
                  (setq live nil)))))
         (cl-letf (((symbol-function 'llm-chat-streaming)
                    (lambda (&rest _args)
                      (cl-incf provider-calls)
                      'provider-request)))
           (superchat--llm-generate-answer
            "terminal before provider"
            (lambda (result) (setq final result))
            #'ignore)))
       (should (= 0 provider-calls))
       (should (string-match-p "no longer active" final))))))

(ert-deftest test-durable-terminal-hook-cannot-resume-steer-or-send-provider ()
  "A durable terminal seal makes the still-delivering Run effect-inert."
  (test-llm--with-pause-pending-runtime-harness
   (lambda (submit response call-count recorded-events)
     (let* ((handle (funcall submit "terminal-hook-resume"))
            (run-id (plist-get handle :run-id)))
       (setq-local superchat--active-run-id run-id)
       (superchat-pause)
       ;; The first response reaches a real tool boundary and stashes the
       ;; follow-up attempt without sending it.
       (funcall (funcall response 1) '((fake-tool . "result")))
       (let ((provider-before (funcall call-count))
             hook-fired
             hook-observation)
         (setq superchat-post-turn-functions
               (list
                (lambda (_turn)
                  (unless hook-fired
                    (setq hook-fired t)
                    (let* ((entry-before
                            (superchat-runtime--entry run-id))
                           (status-before
                            (plist-get entry-before :status))
                           (active-before
                            (member run-id
                                    (superchat-runtime-active-runs)))
                           (request-before
                            (plist-get entry-before :transport-request))
                           (types-before
                            (mapcar (lambda (event)
                                      (plist-get event :type))
                                    (funcall recorded-events))))
                      (cl-letf (((symbol-function 'read-string)
                                 (lambda (&rest _args)
                                   "steer after durable terminal"))
                                ((symbol-function 'message) #'ignore))
                        (superchat-resume))
                      (let* ((entry-after
                              (superchat-runtime--entry run-id))
                             (types-after
                              (mapcar (lambda (event)
                                        (plist-get event :type))
                                      (funcall recorded-events))))
                        (setq hook-observation
                              (list
                               :terminal-before-hook
                               (member "run.cancelled" types-before)
                               :status-before status-before
                               :active-before active-before
                               :provider-before provider-before
                               :provider-after (funcall call-count)
                               :request-unchanged
                               (eq request-before
                                   (plist-get entry-after
                                              :transport-request))
                               :steer-after-terminal
                               (and (member "run.cancelled" types-after)
                                    (member "run.steered" types-after))
                               :status-after
                               (plist-get entry-after :status)))))))))
         (should (superchat-runtime-cancel run-id "terminal hook seal"))
         (should hook-fired)
         (should (plist-get hook-observation :terminal-before-hook))
         (should (eq 'delivering
                     (plist-get hook-observation :status-before)))
         ;; Keep the UI-facing active semantics used by the three-press cancel
         ;; fallback; effect capability is the narrower boundary.
         (should (plist-get hook-observation :active-before))
         (should (= 1 (plist-get hook-observation :provider-before)))
         (should (= 1 (plist-get hook-observation :provider-after)))
         (should (plist-get hook-observation :request-unchanged))
         (should-not (plist-get hook-observation :steer-after-terminal))
         (should (eq 'delivering
                     (plist-get hook-observation :status-after)))
         (should (eq 'cancelled
                     (plist-get (superchat-runtime-status run-id)
                                :status))))))))

(ert-deftest test-dead-exact-view-never-retargets-by-name ()
  "A dead exact View never resolves to a same-named replacement."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name (generate-new-buffer-name
                   " *superchat-dead-ephemeral-owner*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-buffer-name name)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            captured replacement observed)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-tape-append-event)
                      (lambda (_event) (cl-incf next-event-id)))
                     ((symbol-function 'superchat--update-status) #'ignore)
                     ((symbol-function 'superchat--llm-generate-answer)
                      (lambda (&rest _args) 'pending-ephemeral-request))
                     ((symbol-function 'llm-cancel-request) #'ignore))
             ;; Put the captured object in a completed call frame before GC;
             ;; otherwise the evaluator's lexical stack can itself keep the
             ;; weak key alive and obscure what the Runtime registry retains.
             (setq observed
                   (funcall
                    (lambda ()
                      (setq captured (get-buffer-create name))
                      (let* ((handle
                              (superchat-runtime-test-submit
                               (list :prompt "exact owner"
                                     :buffer captured
                                     :buffer-name name
                                     :deadline nil
                                     :delivery-function #'ignore)))
                             (run-id (plist-get handle :run-id))
                             (entry-buffer
                              (plist-get (superchat-runtime--entry run-id)
                                         :buffer)))
                        (should (eq captured entry-buffer))
                        (kill-buffer captured)
                        (setq replacement (get-buffer-create name))
                        (let ((retargeted
                               (eq replacement
                                   (superchat-runtime--entry-buffer
                                    (superchat-runtime--entry run-id)))))
                          (superchat-runtime-cancel
                           run-id "dead owner release")
                          (let ((released
                                 (superchat-runtime--entry run-id)))
                            (list
                             :retargeted retargeted
                             :replacement-live
                             (buffer-live-p replacement)
                             :buffer-field-preserved
                             (plist-member released :buffer)
                             :exact-buffer (plist-get released :buffer)
                             :released-locator
                             (superchat-runtime--entry-buffer released))))))))
             )
         (when (buffer-live-p replacement)
           (kill-buffer replacement)))
       (should-not (plist-get observed :retargeted))
       (should (plist-get observed :replacement-live))
       (should (plist-get observed :buffer-field-preserved))
       (should (eq captured (plist-get observed :exact-buffer)))
       ;; A dead exact object must not restore legacy name fallback.
       (should-not (plist-get observed :released-locator))
       (should-not (buffer-live-p captured))))))

(ert-deftest test-callback-view-object-survives-rename-without-clone-handoff ()
  "Callbacks follow one captured object; clone/content copies never inherit it."
  (let* ((old-name (generate-new-buffer-name
                    " *superchat-callback-object-old*"))
         (owner (get-buffer-create old-name))
         clone moved wrapped renamed-name
         (calls 0)
         callback-buffer callback-label)
    (unwind-protect
        (progn
          (with-current-buffer owner
            (insert "same content")
            (setq wrapped
                  (superchat--callback-in-view
                   owner
                   (lambda ()
                     (cl-incf calls)
                     (setq callback-buffer (current-buffer)
                           callback-label superchat-buffer-name))))
            (setq clone
                  (clone-buffer
                   (generate-new-buffer-name
                    " *superchat-callback-object-clone*")
                   nil)))
          (setq moved
                (generate-new-buffer
                 (generate-new-buffer-name
                  " *superchat-callback-object-moved*")))
          (with-current-buffer moved (insert "same content"))
          (with-current-buffer owner
            (setq renamed-name
                  (rename-buffer
                   (generate-new-buffer-name
                    " *superchat-callback-object-renamed*")
                   t)))
          (with-current-buffer clone (funcall wrapped))
          (should (= 1 calls))
          (should (eq owner callback-buffer))
          (should (equal renamed-name callback-label))
          (should-not (get-buffer old-name))
          (kill-buffer owner)
          ;; Neither an Emacs clone nor a same-content buffer is an implicit
          ;; View handoff after the authoritative object dies.
          (with-current-buffer clone (funcall wrapped))
          (with-current-buffer moved (funcall wrapped))
          (should (= 1 calls))
          (should-not (get-buffer old-name)))
      (dolist (buffer (list owner clone moved (get-buffer old-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest test-llm-live-callbacks-follow-renamed-view-object ()
  "Stream, response, and error callbacks use the captured renamed View."
  (dolist (path '(response error))
    (test-llm--with-mock-llm
     (lambda ()
       (let* ((old-name
               (generate-new-buffer-name
                (format " *superchat-%s-callback-old*" path)))
              (owner (get-buffer-create old-name))
              partial-fn response-fn error-fn renamed-name
              final-value final-buffer final-label
              stream-buffer stream-label)
         (unwind-protect
             (with-current-buffer owner
               (let ((superchat-buffer-name old-name)
                     (superchat-llm-backend
                      (test-llm--make-mock-backend 'openai))
                     (superchat-response-timeout nil)
                     (superchat-show-ttft nil)
                     (superchat-llm-tools-enabled nil))
                 (cl-letf
                     (((symbol-function 'llm-chat-streaming)
                       (lambda (_provider _prompt partial response error
                                &optional _multi-output)
                         (setq partial-fn partial
                               response-fn response
                               error-fn error)
                         'captured-provider-request)))
                   (superchat--llm-generate-answer
                    "callback owner"
                    (lambda (value)
                      (setq final-value value
                            final-buffer (current-buffer)
                            final-label superchat-buffer-name))
                    (lambda (_chunk)
                      (setq stream-buffer (current-buffer)
                            stream-label superchat-buffer-name)))
                   (setq renamed-name
                         (rename-buffer
                          (generate-new-buffer-name
                           (format " *superchat-%s-callback-renamed*"
                                   path))
                          t))
                   ;; Provider callbacks do not inherit the initiating
                   ;; buffer's dynamic current-buffer binding.
                   (with-temp-buffer
                     (pcase path
                       ('response
                        (funcall partial-fn '(:text "part"))
                        (funcall response-fn '(:text "done")))
                       ('error
                        (funcall error-fn 'error "boom"))))
                   (should (eq owner final-buffer))
                   (should (equal renamed-name final-label))
                   (pcase path
                     ('response
                      (should (eq owner stream-buffer))
                      (should (equal renamed-name stream-label))
                      (should (equal "done" final-value)))
                     ('error
                      (should (string-match-p "API error: boom"
                                              final-value))))
                   (should-not (get-buffer old-name)))))
           (dolist (buffer (list owner (get-buffer old-name)))
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))))))))

(ert-deftest test-llm-render-callbacks-follow-renamed-owner-not-old-name-foreign ()
  "Real LLM callbacks and Render stay with a renamed exact View object."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((old-name
             (generate-new-buffer-name " *superchat-render-owner-old*"))
            (owner (get-buffer-create old-name))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            partial-fn response-fn foreign foreign-marker renamed-name)
       (unwind-protect
           (progn
             (with-current-buffer owner
               (org-mode)
               (setq-local superchat-mode t
                           superchat--session-id "session-render-owner"
                           superchat--response-region-states nil
                           superchat--response-region-current-owner nil
                           superchat--prompt-start nil
                           superchat--current-response-parts nil)
               (insert "* User: exact owner\n")
               (let ((superchat-buffer-name old-name))
                 (superchat--prepare-for-response)
                 (cl-letf (((symbol-function 'llm-chat-streaming)
                            (lambda (_provider _prompt partial response _error
                                     &optional _multi-output)
                              (setq partial-fn partial
                                    response-fn response)
                              'renamed-render-request)))
                   (superchat--llm-generate-answer
                    "render into exact owner"
                    #'superchat--process-llm-result
                    #'superchat--stream-llm-result)))
               (setq renamed-name
                     (rename-buffer
                      (generate-new-buffer-name
                       " *superchat-render-owner-renamed*")
                      t)))
             (setq foreign (get-buffer-create old-name))
             (with-current-buffer foreign
               (fundamental-mode)
               (insert "FOREIGN-SENTINEL")
               (setq-local superchat--prompt-start (copy-marker (point) t))
               (setq foreign-marker superchat--prompt-start)
               (set-buffer-modified-p nil))
             (with-temp-buffer
               (funcall partial-fn '(:reasoning "owner thought"))
               (funcall partial-fn '(:text "owner stream"))
               (cl-letf (((symbol-function 'superchat--record-message) #'ignore))
                 (funcall response-fn '(:text "owner final"))))
             (with-current-buffer owner
               (let* ((text (buffer-string))
                      (prompt-marker
                       (cl-loop for entry in superchat--response-region-states
                                for marker = (plist-get (cdr entry)
                                                        :prompt-marker)
                                when (and (markerp marker)
                                          (marker-position marker))
                                return marker)))
                 (should (string-match-p "\\*\\* Thinking" text))
                 (should (string-match-p "owner thought" text))
                 (should (string-match-p "\\*\\* Assistant:\nowner final" text))
                 (should-not (string-match-p "owner stream" text))
                 (should (= 1 (test-render--match-count
                               "\\* User: \\(?:\\n\\|\\'\\)" text)))
                 (should (markerp prompt-marker))
                 (should (eq owner (marker-buffer prompt-marker)))
                 (should (= (point-max)
                            (marker-position prompt-marker)))
                 (should-not (marker-insertion-type prompt-marker))
                 (should (derived-mode-p 'org-mode))))
             (with-current-buffer foreign
               (should (equal "FOREIGN-SENTINEL" (buffer-string)))
               (should (eq foreign (marker-buffer foreign-marker)))
               (should (= (point-max) (marker-position foreign-marker)))
               (should (marker-insertion-type foreign-marker))
               (should (eq major-mode 'fundamental-mode))
               (should-not (buffer-modified-p)))
             (should (eq owner (get-buffer renamed-name))))
         (dolist (buffer (list owner foreign (get-buffer old-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(ert-deftest test-runtime-dead-owner-late-stream-terminal-tool-never-retargets ()
  "Late callbacks close one Run without writing a same-name replacement."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name " *superchat-late-owner*"))
            (owner (get-buffer-create name))
            (tool-name (format "late_owner_%x" (random #xfffffff)))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-agent-confirm-destructive nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            (implementation-calls 0)
            (provider-calls 0)
            events wrapped-tool authority-done partial-fn response-fn handle
            replacement replacement-marker owner-before-kill)
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-name
              :function
              (lambda (done &rest _args)
                (cl-incf implementation-calls)
                (setq authority-done done)
                'pending)
              :description "late exact owner regression"
              :args nil :async t
              :effects (list (list :effect 'local-read :resource "probe")))
             (cl-letf
                 (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (push (copy-tree event t) events)
                     (cl-incf next-event-id)))
                  ((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (_provider prompt partial response _error
                            &optional _multi-output)
                     (setq wrapped-tool (car (llm-chat-prompt-tools prompt))
                           partial-fn partial
                           response-fn response)
                     'late-owner-request)))
               (with-current-buffer owner
                 (org-mode)
                 (setq-local superchat-mode t
                             superchat--session-id "session-late-owner"
                             superchat--response-region-states nil
                             superchat--response-region-current-owner nil
                             superchat--prompt-start nil
                             superchat--current-response-parts nil)
                 (insert "* User: late owner\n")
                 (let ((superchat-buffer-name name))
                   (superchat--prepare-for-response)
                   (setq handle
                         (superchat-runtime-test-submit
                          (list :input "late owner" :clean-input "late owner"
                                :prompt "late owner"
                                :session-id "session-late-owner"
                                :buffer owner :buffer-name name
                                :tools (list tool-name)
                                :approval-function (lambda (_request) t)
                                :deadline nil)))))
               (with-temp-buffer
                 (funcall partial-fn '(:reasoning "late owner thought"))
                 (funcall partial-fn '(:text "late owner stream"))
                 (funcall (llm-tool-function wrapped-tool)
                          (lambda (value)
                            (cl-incf provider-calls)
                            (funcall response-fn value))))
               (with-current-buffer owner
                 (setq owner-before-kill (buffer-string))
                 (should (string-match-p "late owner thought" owner-before-kill))
                 (should (string-match-p "late owner stream" owner-before-kill))
                 (should (string-match-p "\\* User: late owner"
                                         owner-before-kill))
                 (should
                  (= 1
                     (test-render--match-count
                      (regexp-quote (format "Tool call: %s" tool-name))
                      owner-before-kill))))
               (kill-buffer owner)
               (setq replacement (get-buffer-create name))
               (with-current-buffer replacement
                 (fundamental-mode)
                 (insert "REPLACEMENT-SENTINEL")
                 (setq-local superchat--prompt-start (copy-marker (point) t))
                 (setq replacement-marker superchat--prompt-start)
                 (set-buffer-modified-p nil))
               (with-temp-buffer
                 (funcall authority-done "tool result")
                 (funcall authority-done "duplicate tool result")
                 (funcall partial-fn '(:text "too late"))
                 (funcall response-fn "duplicate terminal"))
               (let* ((run-id (plist-get handle :run-id))
                      (types (mapcar (lambda (event) (plist-get event :type))
                                     events)))
                 (should (= 1 implementation-calls))
                 (should (= 1 provider-calls))
                 (should (= 1 (cl-count "tool.completed" types :test #'equal)))
                 (should (= 1 (cl-count "run.completed" types :test #'equal)))
                 (should (= 1 (cl-count "turn.completed" types :test #'equal)))
                 (should (eq 'completed
                             (plist-get (superchat-runtime-status run-id)
                                        :status))))
               (with-current-buffer replacement
                 (should (equal "REPLACEMENT-SENTINEL" (buffer-string)))
                 (should (eq replacement
                             (marker-buffer replacement-marker)))
                 (should (= (point-max)
                            (marker-position replacement-marker)))
                 (should (marker-insertion-type replacement-marker))
                 (should (eq major-mode 'fundamental-mode))
                 (should-not (buffer-modified-p)))))
         (when handle
           (ignore-errors
             (superchat-runtime-cancel
              (plist-get handle :run-id) "late owner test cleanup")))
         (superchat-unregister-tool tool-name)
         (dolist (buffer (list owner replacement (get-buffer name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(ert-deftest test-llm-context-recovery-follows-renamed-view-object ()
  "Context recovery resumes in the captured View after it is renamed."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((old-name
             (generate-new-buffer-name
              " *superchat-context-recovery-old*"))
            (owner (get-buffer-create old-name))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat--llm-context-envelope '(:prompt "OLD CONTEXT"))
            (provider-calls 0)
            resume provider-second-buffer renamed-name
            final final-buffer final-label)
       (unwind-protect
           (with-current-buffer owner
             (let ((superchat-buffer-name old-name)
                   (superchat-llm-context-budget-exceeded-functions
                    (list
                     (lambda (details)
                       (setq resume (plist-get details :resume-function))
                       t))))
               (cl-letf
                   (((symbol-function 'superchat--llm-context-capability)
                     (lambda (_provider _model) '(:limit 10000)))
                    ((symbol-function
                      'superchat--llm-estimate-context-tokens)
                     (lambda (_provider _prompt) 1000))
                    ((symbol-function 'llm-make-chat-prompt)
                     (lambda (text &rest _args)
                       (make-llm-chat-prompt
                        :interactions
                        (list (make-llm-chat-prompt-interaction
                               :role 'user :content text)))))
                    ((symbol-function 'llm-chat-streaming)
                     (lambda (_provider _prompt _partial response error
                              &optional _multi-output)
                       (cl-incf provider-calls)
                       (if (= provider-calls 1)
                           (funcall error 'llm-context-length-exceeded
                                    "provider rejected the request")
                         (setq provider-second-buffer (current-buffer))
                         (funcall response "recovered"))
                       (intern (format "request-%d" provider-calls)))))
                 (superchat--llm-generate-answer
                  "before\nOLD CONTEXT\nafter"
                  (lambda (result)
                    (setq final result
                          final-buffer (current-buffer)
                          final-label superchat-buffer-name))
                  #'ignore)
                 (should (= 1 provider-calls))
                 (should (functionp resume))
                 (setq renamed-name
                       (rename-buffer
                        (generate-new-buffer-name
                         " *superchat-context-recovery-renamed*")
                        t))
                 (with-temp-buffer
                   (funcall resume "NEW CONTEXT"))
                 (should (= 2 provider-calls))
                 (should (equal "recovered" final))
                 (should (eq owner provider-second-buffer))
                 (should (eq owner final-buffer))
                 (should (equal renamed-name final-label))
                 (should-not (get-buffer old-name)))))
         (dolist (buffer (list owner (get-buffer old-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(ert-deftest test-runtime-killed-view-response-still-closes-without-rendering ()
  "A provider response closes durably after its captured View is killed."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name
              " *superchat-runtime-killed-callback*"))
            (owner (get-buffer-create name))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            events response-fn error-fn handle render-calls)
       (unwind-protect
           (cl-letf
               (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (plist-get event :type) events)
                   (cl-incf next-event-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (&rest _args) (cl-incf render-calls)))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider _prompt _partial response error
                          &optional _multi-output)
                   (setq response-fn response
                         error-fn error)
                   'killed-view-provider-request)))
             (with-current-buffer owner
               (let ((superchat-buffer-name name))
                 (setq handle
                       (superchat-runtime-test-submit
                        (list :input "killed callback"
                              :clean-input "killed callback"
                              :prompt "killed callback"
                              :session-id "session-killed-callback"
                              :buffer owner
                              :buffer-name name
                              :deadline nil)))))
             (should (functionp response-fn))
             (should (functionp error-fn))
             (kill-buffer owner)
             (with-temp-buffer
               (funcall response-fn "completed after View death"))
             (let* ((run-id (plist-get handle :run-id))
                    (status (superchat-runtime-status run-id))
                    (released (superchat-runtime--entry run-id)))
               (should (eq 'completed (plist-get status :status)))
               (should (member "run.completed" events))
               (should (member "turn.completed" events))
               (should (= 0 (or render-calls 0)))
               (should-not (get-buffer name))
               (should (plist-member released :buffer))
               (should (eq owner (plist-get released :buffer)))
               (should-not (buffer-live-p (plist-get released :buffer)))
               (should (plist-get released :quiesced))
               (should-not (superchat-runtime--entry-buffer released))))
         (dolist (buffer (list owner (get-buffer name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))))))

(ert-deftest test-runtime-killed-view-stream-protocol-still-assembles-response ()
  "Dead Views drop rendering, not cumulative transport protocol state."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name
              " *superchat-dead-stream-protocol*"))
            (owner (get-buffer-create name))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            (activity-calls 0)
            (superchat-llm--distribution-adapters
             (list :activity-function
                   (lambda (_owner) (cl-incf activity-calls))))
            events partial-fn response-fn handle stream-render-calls
            final-render-calls)
       (unwind-protect
           (cl-letf
               (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (copy-tree event t) events)
                   (cl-incf next-event-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--stream-llm-result)
                 (lambda (&rest _args) (cl-incf stream-render-calls)))
                ((symbol-function 'superchat--process-llm-result)
                 (lambda (&rest _args) (cl-incf final-render-calls)))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider _prompt partial response _error
                          &optional _multi-output)
                   (setq partial-fn partial
                         response-fn response)
                   'dead-stream-protocol-request)))
             (with-current-buffer owner
               (let ((superchat-buffer-name name))
                 (setq handle
                       (superchat-runtime-test-submit
                        (list :input "dead stream protocol"
                              :clean-input "dead stream protocol"
                              :prompt "dead stream protocol"
                              :session-id "session-dead-stream-protocol"
                              :buffer owner
                              :buffer-name name
                              :deadline nil)))))
             (should (functionp partial-fn))
             (should (functionp response-fn))
             (kill-buffer owner)
             (with-temp-buffer
               (funcall partial-fn
                        '(:text "partial" :input-tokens 9 :output-tokens 1))
               (funcall partial-fn
                        '(:text "partial answer"
                          :input-tokens 9 :output-tokens 2))
               (funcall response-fn nil))
             (let* ((run-id (plist-get handle :run-id))
                    (status (superchat-runtime-status run-id))
                    (response-event
                     (cl-find-if
                      (lambda (event)
                        (equal "response.completed"
                               (plist-get event :type)))
                      events)))
               (should (eq 'completed (plist-get status :status)))
               (should (eq 'answer-streaming
                           (plist-get status :model-phase)))
               (should (= 9 (plist-get status :input-tokens)))
               (should (= 2 (plist-get status :output-tokens)))
               (should (= 2 activity-calls))
               (should response-event)
               (should (equal "partial answer"
                              (plist-get response-event :summary)))
               (should (= 0 (or stream-render-calls 0)))
               (should (= 0 (or final-render-calls 0)))
               (should-not (get-buffer name))))
         (when (buffer-live-p owner)
           (kill-buffer owner))
         (when (get-buffer name)
           (kill-buffer (get-buffer name))))))))

(ert-deftest test-runtime-dead-view-async-tool-done-reaches-provider-once ()
  "A dead View cannot interrupt async Authority completion or Run closure."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name
              " *superchat-dead-async-tool*"))
            (owner (get-buffer-create name))
            (tool-name
             (format "batch7h_dead_done_%x" (random #xfffffff)))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-agent-confirm-destructive nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            events wrapped-tool authority-done response-fn handle provider-value
            (provider-calls 0)
            done-error
            (transcript-render-calls 0)
            (final-render-calls 0))
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-name
              :function
              (lambda (done &rest _args)
                (setq authority-done done)
                'pending)
              :description "batch7h dead View async completion probe"
              :args nil
              :async t
              :effects
              (list (list :effect 'local-read :resource "batch7h-probe")))
             (cl-letf
                 (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (push (copy-tree event t) events)
                     (cl-incf next-event-id)))
                  ((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'superchat--response-region-close) #'ignore)
                  ((symbol-function 'superchat--response-region-insert-transcript)
                   (lambda (&rest _args)
                     (cl-incf transcript-render-calls)))
                  ((symbol-function 'superchat--stream-llm-result) #'ignore)
                  ((symbol-function 'superchat--process-llm-result)
                   (lambda (&rest _args) (cl-incf final-render-calls)))
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (_provider prompt _partial response _error
                            &optional _multi-output)
                     (setq wrapped-tool
                           (car (llm-chat-prompt-tools prompt))
                           response-fn response)
                     'dead-async-tool-request)))
               (with-current-buffer owner
                 (let ((superchat-buffer-name name))
                   (setq handle
                         (superchat-runtime-test-submit
                          (list :input "dead async tool"
                                :clean-input "dead async tool"
                                :prompt "dead async tool"
                                :session-id "session-dead-async-tool"
                                :buffer owner
                                :buffer-name name
                                :tools (list tool-name)
                                :approval-function (lambda (_request) t)
                                :deadline nil)))))
               (should wrapped-tool)
               (should (functionp response-fn))
               (with-temp-buffer
                 (funcall
                  (llm-tool-function wrapped-tool)
                  (lambda (value)
                    (cl-incf provider-calls)
                    (setq provider-value value)
                    (funcall response-fn "finished after async tool"))))
               (should (functionp authority-done))
               (kill-buffer owner)
               (with-temp-buffer
                 (condition-case err
                     (funcall authority-done "tool-result")
                   (error (setq done-error (error-message-string err)))))
               (let* ((run-id (plist-get handle :run-id))
                      (status (superchat-runtime-status run-id))
                      (types (mapcar (lambda (event)
                                       (plist-get event :type))
                                     events)))
                 (should-not done-error)
                 (should (= 1 (or provider-calls 0)))
                 (should (equal "tool-result" provider-value))
                 (should (= 1 (or transcript-render-calls 0)))
                 (should (= 0 (or final-render-calls 0)))
                 (should (member "tool.completed" types))
                 (should (member "response.completed" types))
                 (should (member "run.completed" types))
                 (should (member "turn.completed" types))
                 (should (eq 'completed (plist-get status :status)))
                 (should-not (get-buffer name)))
               ;; Both audited render helpers must also tolerate a missing
               ;; display label and an explicitly dead buffer object.
               (let ((renders-before (or transcript-render-calls 0)))
                 (let ((superchat-buffer-name nil))
                   (should-not
                    (superchat--agent-render-tool-call tool-name nil))
                   (should-not
                    (superchat--agent-render-tool-result tool-name "ignored")))
                 (let ((superchat-buffer-name owner))
                   (should-not
                    (superchat--agent-render-tool-call tool-name nil))
                   (should-not
                    (superchat--agent-render-tool-result tool-name "ignored")))
                 (should (= renders-before
                            (or transcript-render-calls 0)))
                 (should-not (get-buffer name)))))
         (when handle
           (ignore-errors
             (superchat-runtime-cancel
              (plist-get handle :run-id) "batch7h test cleanup")))
         (superchat-unregister-tool tool-name)
         (when (buffer-live-p owner)
           (kill-buffer owner))
         (when (get-buffer name)
           (kill-buffer (get-buffer name))))))))

(ert-deftest test-agent-async-render-error-still-delivers-provider-once ()
  "A presentation failure cannot suppress an async provider continuation."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((owner (generate-new-buffer " *superchat-render-error-owner*"))
            (name (buffer-name owner))
            (superchat-agent-confirm-destructive nil)
            (superchat-authority-approval-function (lambda (_request) t))
            (superchat-agent-pre-tool-functions nil)
            (superchat-agent-post-tool-functions nil)
            (superchat-agent-post-tool-failure-functions nil)
            (next-event-id 0)
            wrapped authority-done provider-value done-error
            (provider-calls 0))
       (unwind-protect
           (progn
             (with-current-buffer owner
               (setq-local superchat--session-id "session-render-error")
               (let ((superchat-buffer-name name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :function
                         (lambda (done &rest _args)
                           (setq authority-done done))
                         :name "render_error_async"
                         :description "async presentation failure regression"
                         :args nil
                         :async t)))))
             (cl-letf
                 (((symbol-function 'superchat-tape-append-event)
                   (lambda (_event) (cl-incf next-event-id)))
                  ((symbol-function 'superchat--agent-render-tool-call) #'ignore)
                  ((symbol-function 'superchat--agent-render-tool-result)
                   (lambda (&rest _args) (error "render exploded")))
                  ((symbol-function 'display-warning) #'ignore))
               (with-temp-buffer
                 (funcall
                  (llm-tool-function wrapped)
                  (lambda (value)
                    (cl-incf provider-calls)
                    (setq provider-value value))))
               (should (functionp authority-done))
               (condition-case err
                   (with-temp-buffer
                     (funcall authority-done "tool-result"))
                 (error (setq done-error (error-message-string err))))
               (should-not done-error)
               (should (= 1 provider-calls))
               (should (equal "tool-result" provider-value))))
         (when (buffer-live-p owner)
           (kill-buffer owner)))))))

(ert-deftest test-agent-sync-result-never-retargets-same-name-view ()
  "A sync result remains anchored to the View object captured by its wrapper."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name " *superchat-sync-result-owner*"))
            (owner (get-buffer-create name))
            (superchat-agent-confirm-destructive nil)
            (superchat-authority-approval-function (lambda (_request) t))
            (superchat-agent-pre-tool-functions nil)
            (superchat-agent-post-tool-functions nil)
            (superchat-agent-post-tool-failure-functions nil)
            (next-event-id 0)
            replacement wrapped render-buffers)
       (unwind-protect
           (progn
             (with-current-buffer owner
               (setq-local superchat--session-id "session-sync-result-owner")
               (let ((superchat-buffer-name name))
                 (setq wrapped
                       (superchat--agent-wrap-tool
                        (test-llm--make-tool
                         :function
                         (lambda ()
                           (kill-buffer owner)
                           (setq replacement (get-buffer-create name))
                           "tool-result")
                         :name "sync_result_owner"
                         :description "sync result owner regression"
                         :args nil
                         :async nil)))))
             (cl-letf
                 (((symbol-function 'superchat-tape-append-event)
                   (lambda (_event) (cl-incf next-event-id)))
                  ((symbol-function 'superchat--response-region-close) #'ignore)
                  ((symbol-function 'superchat--response-region-insert-transcript)
                   (lambda (&rest _args)
                     (push (current-buffer) render-buffers))))
               (with-temp-buffer
                 (should (equal "tool-result"
                                (funcall (llm-tool-function wrapped)))))
               (should (buffer-live-p replacement))
               (should-not (memq replacement render-buffers))))
         (when (buffer-live-p owner)
           (kill-buffer owner))
         (when (buffer-live-p replacement)
           (kill-buffer replacement))
         (when (get-buffer name)
           (kill-buffer (get-buffer name))))))))

(ert-deftest test-runtime-stream-sink-error-preserves-activity-and-terminal ()
  "A custom stream sink cannot interrupt transport protocol progression."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((name
             (generate-new-buffer-name " *superchat-stream-sink-error*"))
            (owner (get-buffer-create name))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            (activity-calls 0)
            (superchat-llm--distribution-adapters
             (list :activity-function
                   (lambda (_owner) (cl-incf activity-calls))))
            events partial-fn response-fn handle sink-error)
       (unwind-protect
           (cl-letf
               (((symbol-function 'superchat-tape-append-event)
                 (lambda (event)
                   (push (copy-tree event t) events)
                   (cl-incf next-event-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider _prompt partial response _error
                          &optional _multi-output)
                   (setq partial-fn partial
                         response-fn response)
                   'stream-sink-error-provider-request)))
             (with-current-buffer owner
               (let ((superchat-buffer-name name))
                 (setq handle
                       (superchat-runtime-test-submit
                        (list :input "stream sink error"
                              :clean-input "stream sink error"
                              :prompt "stream sink error"
                              :session-id "session-stream-sink-error"
                              :buffer owner
                              :buffer-name name
                              :stream-function
                              (lambda (_chunk) (error "stream sink exploded"))
                              :delivery-function #'ignore
                              :deadline nil)))))
             (condition-case err
                 (with-temp-buffer
                   (funcall partial-fn
                            '(:text "partial answer"
                              :input-tokens 13 :output-tokens 2)))
               (error (setq sink-error (error-message-string err))))
             (with-temp-buffer
               (funcall response-fn nil))
             (let* ((status
                     (superchat-runtime-status (plist-get handle :run-id)))
                    (response-event
                     (cl-find-if
                      (lambda (event)
                        (equal "response.completed" (plist-get event :type)))
                      events)))
               (should-not sink-error)
               (should (= 1 activity-calls))
               (should (eq 'completed (plist-get status :status)))
               (should (eq 'answer-streaming
                           (plist-get status :model-phase)))
               (should (= 13 (plist-get status :input-tokens)))
               (should (= 2 (plist-get status :output-tokens)))
               (should response-event)
               (should (equal "partial answer"
                              (plist-get response-event :summary)))))
         (when (buffer-live-p owner)
           (kill-buffer owner))
         (when (get-buffer name)
           (kill-buffer (get-buffer name))))))))

(ert-deftest test-workflow-stream-sink-captures-owner-view-object ()
  "Workflow streaming drops presentation after its captured View dies."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-workflow)
     (let* ((name
             (generate-new-buffer-name " *superchat-workflow-stream-owner*"))
            (owner (get-buffer-create name))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0)
            (activity-calls 0)
            (superchat-llm--distribution-adapters
             (list :activity-function
                   (lambda (_owner) (cl-incf activity-calls))))
            (real-sink (symbol-function 'superchat-workflow--stream-chunk))
            replacement partial-fn response-fn handle partial-error
            (sink-calls 0))
       (unwind-protect
           (cl-letf
               (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-event-id)))
                ((symbol-function 'superchat--update-status) #'ignore)
                ((symbol-function 'superchat--track-active-run) #'ignore)
                ((symbol-function 'superchat-workflow--render-step-header) #'ignore)
                ((symbol-function 'superchat-workflow--ensure-view-prompt) #'ignore)
                ((symbol-function 'superchat-workflow--fail-step) #'ignore)
                ((symbol-function 'superchat--record-message) #'ignore)
                ((symbol-function 'superchat--response-region-close) #'ignore)
                ((symbol-function 'display-warning) #'ignore)
                ((symbol-function 'superchat-workflow--stream-chunk)
                 (lambda (chunk)
                   (cl-incf sink-calls)
                   (funcall real-sink chunk)))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider _prompt partial response _error
                          &optional _multi-output)
                   (setq partial-fn partial
                         response-fn response)
                   'workflow-stream-owner-provider-request)))
             (with-current-buffer owner
               (setq-local superchat--session-id "session-workflow-stream-owner")
               (let ((superchat-buffer-name name))
                 (setq handle
                       (superchat-workflow--run-step
                        0
                        (list (superchat-workflow--parse-line "one step"))
                        '(:input "" :lang "en" :date "2026-09-02")
                        nil owner "workflow-stream-owner"))))
             (should (functionp partial-fn))
             (should (functionp response-fn))
             (kill-buffer owner)
             (setq replacement (get-buffer-create name))
             (condition-case err
                 (with-temp-buffer
                   (funcall partial-fn
                            '(:text "partial answer"
                              :input-tokens 17 :output-tokens 2)))
               (error (setq partial-error (error-message-string err))))
             (with-temp-buffer
               (funcall response-fn "done"))
             (let ((status
                    (superchat-runtime-status (plist-get handle :run-id))))
               (should-not partial-error)
               (should (= 0 sink-calls))
               (should (= 1 activity-calls))
               (should (eq 'completed (plist-get status :status)))
               (should (= (point-min)
                          (with-current-buffer replacement (point-max))))))
         (when handle
           (ignore-errors
             (superchat-runtime-cancel
              (plist-get handle :run-id) "workflow stream test cleanup")))
         (when (buffer-live-p owner)
           (kill-buffer owner))
         (when (buffer-live-p replacement)
           (kill-buffer replacement))
         (when (get-buffer name)
           (kill-buffer (get-buffer name))))))))

(ert-deftest test-workflow-public-entry-stream-sink-captures-target-view ()
  "A foreign caller/canceler cannot become any workflow presentation owner."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-workflow)
     (dolist (terminal '(completed cancelled))
       (let* ((target-name
               (generate-new-buffer-name
                " *superchat-workflow-public-target*"))
              (target (get-buffer-create target-name))
              (caller
               (generate-new-buffer " *superchat-workflow-public-caller*"))
              (canceler
               (generate-new-buffer " *superchat-workflow-public-canceler*"))
              (superchat-runtime--runs (make-hash-table :test #'equal))
              (superchat-llm-backend
               (test-llm--make-mock-backend 'openai))
              (superchat-response-timeout nil)
              (superchat-show-ttft nil)
              (superchat-llm-tools-enabled nil)
              (superchat-pre-topic-finalized-functions nil)
              (superchat-post-turn-functions nil)
              (superchat-post-topic-finalized-functions nil)
              (superchat-run-completed-functions nil)
              (superchat-turn-completed-functions nil)
              (superchat-closure-terminal-guard-enabled nil)
              (superchat-closure-shadow-enabled nil)
              (activity-calls 0)
              (superchat-llm--distribution-adapters
               (list :activity-function
                     (lambda (_owner) (cl-incf activity-calls))))
              partial-fn response-fn handle
              presentation-buffers close-buffers record-buffers
              workflow-header-buffer step-header-buffer prompt-buffer
              stream-buffer failure-buffer final-buffer stopped-buffer)
         (unwind-protect
             (cl-letf
                 (((symbol-function 'superchat-workflow--load)
                   (lambda (&rest _args) "one step"))
                  ((symbol-function 'superchat--update-status) #'ignore)
                  ((symbol-function 'superchat--track-active-run) #'ignore)
                  ((symbol-function
                    'superchat--response-region--prompt-line-position)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'superchat--insert-prompt)
                   (lambda (&optional _owner)
                     (setq prompt-buffer (current-buffer))
                     (push (current-buffer) presentation-buffers)))
                  ((symbol-function 'superchat--response-region-insert-transcript)
                   (lambda (text &rest _args)
                     (push (current-buffer) presentation-buffers)
                     (cond
                      ((string-prefix-p "** Workflow:" text)
                       (setq workflow-header-buffer (current-buffer)))
                      ((string-prefix-p "** Step " text)
                       (setq step-header-buffer (current-buffer)))
                      ((string-prefix-p "** Workflow FAILED" text)
                       (setq failure-buffer (current-buffer)))
                      ((string-prefix-p "** Workflow stopped" text)
                       (setq stopped-buffer (current-buffer)))
                      ((string-prefix-p "** Workflow complete" text)
                       (setq final-buffer (current-buffer))))))
                  ((symbol-function 'superchat--response-region-close)
                   (lambda (&rest _args)
                     (push (current-buffer) close-buffers)
                     (push (current-buffer) presentation-buffers)))
                  ((symbol-function 'superchat--record-message)
                   (lambda (&rest _args)
                     (push (current-buffer) record-buffers)
                     (push (current-buffer) presentation-buffers)))
                  ((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'superchat--stream-llm-result)
                   (lambda (_chunk &optional _owner)
                     (setq stream-buffer (current-buffer))
                     (push (current-buffer) presentation-buffers)))
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (_provider _prompt partial response _error
                            &optional _multi-output)
                     (setq partial-fn partial
                           response-fn response)
                     'workflow-public-owner-provider-request)))
               (with-current-buffer target
                 (setq-local superchat-mode t)
                 (setq-local superchat-buffer-name target-name)
                 (setq-local superchat--session-id
                             (format "session-workflow-public-owner-%s-%x"
                                     terminal (random most-positive-fixnum))))
               (let ((superchat-buffer-name target-name)
                     (superchat-lang "English"))
                 (with-current-buffer caller
                   (setq handle
                         (superchat-workflow-execute-async
                          "foreign-owner" nil "/tmp/unused.workflow"))))
               (should (functionp partial-fn))
               (with-current-buffer canceler
                 (funcall partial-fn
                          '(:text "partial answer"
                            :input-tokens 19 :output-tokens 2)))
               (should (= 1 activity-calls))
               (if (eq terminal 'completed)
                   (with-current-buffer canceler
                     (funcall response-fn "completed workflow answer"))
                 (with-current-buffer canceler
                   (superchat-runtime-cancel
                    (plist-get handle :run-id)
                    "cancel from unrelated workflow buffer")))
               (should (eq terminal
                           (plist-get
                            (superchat-runtime-status
                             (plist-get handle :run-id))
                            :status)))
               (should (eq target workflow-header-buffer))
               (should (eq target step-header-buffer))
               (should (eq target prompt-buffer))
               (should (eq target stream-buffer))
               (should (cl-every (lambda (buffer) (eq buffer target))
                                 presentation-buffers))
               (should (equal (list target)
                              close-buffers))
               (if (eq terminal 'completed)
                   (progn
                     (should (eq target final-buffer))
                     (should-not failure-buffer)
                     (should (equal (list target) record-buffers)))
                 (should (eq target stopped-buffer))
                 (should-not failure-buffer)
                 (should-not final-buffer)
                 (should-not record-buffers)))
           (when handle
             (ignore-errors
               (superchat-runtime-cancel
                (plist-get handle :run-id) "workflow owner test cleanup")))
           (when (buffer-live-p target)
             (kill-buffer target))
           (when (buffer-live-p caller)
             (kill-buffer caller))
           (when (buffer-live-p canceler)
             (kill-buffer canceler))
           (when (get-buffer target-name)
             (kill-buffer (get-buffer target-name)))))))))

(ert-deftest test-workflow-dead-target-keeps-session-protocol-headless ()
  "A dead workflow View loses presentation without losing Session progress."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-workflow)
     (let* ((target-name
             (generate-new-buffer-name " *superchat-workflow-dead-target*"))
            (target (get-buffer-create target-name))
            (caller (generate-new-buffer " *superchat-workflow-dead-caller*"))
            (callback-buffer
             (generate-new-buffer " *superchat-workflow-dead-callback*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            partial-fns response-fns handle replacement correlation-id
            target-session-id
            presentation-buffers observations step-two-host
            caller-before caller-after tracked-run-id tracked-timer-p)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-workflow--load)
                      (lambda (&rest _args) "first step\nsecond step"))
                     ((symbol-function
                       'superchat--response-region--prompt-line-position)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'superchat--insert-prompt)
                      (lambda (&optional _owner) (push (current-buffer) presentation-buffers)))
                     ((symbol-function
                       'superchat--response-region-insert-transcript)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'superchat--response-region-close)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'superchat--record-message)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'superchat--stream-llm-result)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt partial response _error
                               &optional _multi-output)
                        (let* ((effects
                                '((:effect local-write
                                           :resource buffer
                                           :scope current)))
                               (authority
                                (superchat-authority-decide
                                 "workflow_test_write" nil 'attended
                                 'default effects))
                               (decision
                                (superchat-authority--effective-decision
                                 "workflow_test_write" nil authority nil)))
                          (setq observations
                                (append
                                 observations
                                 (list
                                  (list :buffer (current-buffer)
                                        :mode superchat-authority-mode
                                        :decision decision)))))
                        (setq partial-fns
                              (append partial-fns (list partial))
                              response-fns
                              (append response-fns (list response)))
                        (intern (format "workflow-dead-request-%d"
                                        (length response-fns))))))
             (with-current-buffer target
               (setq-local superchat-mode t)
               (setq-local superchat-buffer-name target-name)
               (setq-local superchat--session-id
                           (setq target-session-id
                                 (format "session-workflow-dead-%x"
                                         (random most-positive-fixnum))))
               (setq-local superchat-authority-mode 'explore)
               (setq-local superchat-tools-default-directory
                           "/tmp/workflow-dead-target-directory"))
             (with-current-buffer caller
               (insert "FOREIGN-DOCUMENT-CONTENT")
               (setq-local superchat-authority-mode 'execute)
               (setq-local superchat--last-surface 'foreign-caller)
               (setq-local superchat-tools-default-directory
                           "/tmp/workflow-dead-caller-directory")
               (setq-local superchat--active-run-id "foreign-run")
               (setq-local superchat--session-id "foreign-session")
               (setq-local superchat--workspace-binding
                           '(:id "foreign-workspace"
                             :root "/tmp/workflow-dead-caller-workspace"))
               (setq caller-before
                     (list :text (buffer-string)
                           :authority superchat-authority-mode
                           :surface superchat--last-surface
                           :directory superchat-tools-default-directory
                           :active-run superchat--active-run-id
                           :session superchat--session-id
                           :workspace (copy-tree
                                       superchat--workspace-binding))))
             (let ((superchat-buffer-name target-name)
                   (superchat-lang "English"))
               (with-current-buffer caller
                 (setq handle
                       (superchat-workflow-execute-async
                        "dead-owner" nil "/tmp/unused.workflow"))))
             (should (= 1 (length response-fns)))
             (setq correlation-id
                   (plist-get
                    (superchat-runtime--entry (plist-get handle :run-id))
                    :correlation-id))
             (should (= 1 (length observations)))
             (should (eq target
                         (plist-get (nth 0 observations) :buffer)))
             (should (eq 'explore
                         (plist-get (nth 0 observations) :mode)))
             (should (eq 'deny
                         (plist-get (nth 0 observations) :decision)))
             (setq tracked-run-id
                   (buffer-local-value 'superchat--active-run-id target)
                   tracked-timer-p
                   (timerp
                    (buffer-local-value
                     'superchat--status-refresh-timer target)))
             (should (equal tracked-run-id (plist-get handle :run-id)))
             (should tracked-timer-p)
             (should (cl-every (lambda (buffer) (eq buffer target))
                               presentation-buffers))
             (setq presentation-buffers nil)
             (kill-buffer target)
             (setq replacement (get-buffer-create target-name))
             (with-current-buffer replacement
               (setq-local superchat-mode t)
               (setq-local superchat-buffer-name target-name)
               (setq-local superchat--session-id "replacement-session")
               (setq-local superchat--workspace-binding
                           (list :id "replacement-workspace"
                                 :root "/tmp/does-not-exist-batch7k")))
             (with-current-buffer callback-buffer
               (funcall (nth 0 response-fns) "first answer"))
             (should (= 2 (length response-fns)))
             (let (entries step-two)
               (maphash (lambda (_run-id entry) (push entry entries))
                        superchat-runtime--runs)
               (setq step-two
                     (cl-find "workflow-step-2" entries
                              :key (lambda (entry)
                                     (plist-get entry :actor-id))
                              :test #'equal))
               (should step-two)
               (should (eq 'running-model (plist-get step-two :status)))
               (should (equal target-session-id
                              (plist-get step-two :session-id)))
               (should-not (plist-get step-two :workspace-id))
               (should-not (plist-get step-two :workspace-root))
               (should-not (plist-get step-two :ephemeral-buffer))
               (setq step-two-host (plist-get step-two :buffer))
               (should (buffer-live-p step-two-host))
               (should (string-prefix-p
                        (format " *superchat-editor:%s:workflow-protocol*"
                                target-session-id)
                        (plist-get step-two :buffer-name)))
               (should-not (eq step-two-host caller))
               (should-not (eq step-two-host replacement))
               (should-not (eq step-two-host callback-buffer)))
             (should (= 2 (length observations)))
             (should (eq step-two-host
                         (plist-get (nth 1 observations) :buffer)))
             (should (eq 'ask
                         (plist-get (nth 1 observations) :mode)))
             (should (eq 'ask
                         (plist-get (nth 1 observations) :decision)))
             (with-current-buffer callback-buffer
               (funcall (nth 1 response-fns) "second answer"))
             (should (buffer-live-p step-two-host))
             (should-not presentation-buffers)
             (with-current-buffer caller
               (setq caller-after
                     (list :text (buffer-string)
                           :authority superchat-authority-mode
                           :surface superchat--last-surface
                           :directory superchat-tools-default-directory
                           :active-run superchat--active-run-id
                           :session superchat--session-id
                           :workspace (copy-tree
                                       superchat--workspace-binding))))
             (should (equal caller-before caller-after))
             (should (eq 'completed
                         (plist-get
                          (superchat-runtime-status
                           (plist-get handle :run-id))
                          :status)))
             (should (eq replacement (get-buffer target-name))))
         (maphash
          (lambda (run-id entry)
            (unless (memq (plist-get entry :status)
                          '(completed failed cancelled timed-out
                            audit-incomplete abandoned))
              (ignore-errors
                (superchat-runtime-cancel run-id "dead owner test cleanup"))))
          superchat-runtime--runs)
         (dolist (buffer (list target caller callback-buffer replacement
                               step-two-host (get-buffer target-name)))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest test-workflow-public-entry-sync-rejection-renders-in-target ()
  "A synchronous Runtime rejection renders only in the captured workflow View."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-workflow)
     (let* ((target-name
             (generate-new-buffer-name " *superchat-workflow-reject-target*"))
            (target (get-buffer-create target-name))
            (caller (generate-new-buffer " *superchat-workflow-reject-caller*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            fail-buffer fail-text prompt-buffer)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-workflow--load)
                      (lambda (&rest _args) "one step"))
                     ((symbol-function 'superchat--update-status) #'ignore)
                     ((symbol-function 'superchat--track-active-run) #'ignore)
                     ((symbol-function
                       'superchat--response-region--prompt-line-position)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'superchat--insert-prompt)
                      (lambda (&optional _owner) (setq prompt-buffer (current-buffer))))
                     ((symbol-function
                       'superchat--response-region-insert-transcript)
                      (lambda (text &rest _args)
                        (when (string-prefix-p "** Workflow FAILED" text)
                          (setq fail-buffer (current-buffer)
                                fail-text text)))))
             (with-current-buffer target
               (setq-local superchat-mode t)
               (setq-local superchat-buffer-name target-name)
               (setq-local superchat--session-id "workflow-rejection-session")
               (setq-local superchat--workspace-binding
                           (list :id "gone"
                                 :root "/tmp/does-not-exist-batch7k-reject")))
             (let ((superchat-buffer-name target-name)
                   (superchat-lang "English"))
               (with-current-buffer caller
                 (superchat-workflow-execute-async
                  "reject-owner" nil "/tmp/unused.workflow")))
             (should (eq target fail-buffer))
             (should (eq target prompt-buffer))
             (should (string-match-p "workspace-root" (downcase fail-text))))
         (dolist (buffer (list target caller (get-buffer target-name)))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest test-workflow-public-entry-headless-does-not-create-view ()
  "A headless workflow isolates its Session-owned host from the caller."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-workflow)
     (let* ((target-name
             (generate-new-buffer-name " *superchat-workflow-headless-target*"))
            (caller (generate-new-buffer " *superchat-workflow-headless-caller*"))
            (canceler
             (generate-new-buffer " *superchat-workflow-headless-canceler*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            partial-fn handle presentation-buffers provider-buffer
            runtime-host runtime-host-name caller-before caller-after)
       (unwind-protect
           (cl-letf (((symbol-function 'superchat-workflow--load)
                      (lambda (&rest _args) "one step"))
                     ((symbol-function
                       'superchat--response-region--prompt-line-position)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'superchat--insert-prompt)
                      (lambda (&optional _owner) (push (current-buffer) presentation-buffers)))
                     ((symbol-function
                       'superchat--response-region-insert-transcript)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'superchat--response-region-close)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'superchat--stream-llm-result)
                      (lambda (&rest _args)
                        (push (current-buffer) presentation-buffers)))
                     ((symbol-function 'llm-chat-streaming)
                      (lambda (_provider _prompt partial _response _error
                               &optional _multi-output)
                        (setq partial-fn partial
                              provider-buffer (current-buffer))
                        'workflow-headless-provider-request)))
             (with-current-buffer caller
               (insert "FOREIGN-DOCUMENT-CONTENT")
               (setq-local superchat-authority-mode 'execute)
               (setq-local superchat--last-surface 'foreign-caller)
               (setq-local superchat-tools-default-directory
                           "/tmp/workflow-headless-caller-directory")
               (setq-local superchat--active-run-id "foreign-run")
               (setq-local superchat--session-id "foreign-session")
               (setq-local superchat--workspace-binding
                           '(:id "foreign-workspace"
                             :root "/tmp/workflow-headless-workspace"))
               (setq caller-before
                     (list :text (buffer-string)
                           :authority superchat-authority-mode
                           :surface superchat--last-surface
                           :directory superchat-tools-default-directory
                           :active-run superchat--active-run-id
                           :session superchat--session-id
                           :workspace (copy-tree
                                       superchat--workspace-binding))))
             (let ((superchat-buffer-name target-name)
                   (superchat-lang "English"))
               (with-current-buffer caller
                 (setq handle
                       (superchat-workflow-execute-async
                        "headless-owner" nil "/tmp/unused.workflow"))))
             (should (functionp partial-fn))
             (should-not presentation-buffers)
             (should-not (get-buffer target-name))
             (let ((entry
                    (superchat-runtime--entry (plist-get handle :run-id))))
               (setq runtime-host (plist-get entry :buffer)
                     runtime-host-name (plist-get entry :buffer-name))
               (should-not (plist-get entry :ephemeral-buffer)))
             (should (buffer-live-p runtime-host))
             (should (eq provider-buffer runtime-host))
             (should-not (eq caller runtime-host))
             (should (string-prefix-p
                      " *superchat-editor:foreign-session:workflow-protocol*"
                                      runtime-host-name))
             ;; A headless run has no live View, so its per-step
             ;; `stream-function' is `#'ignore' (`superchat-workflow.el'
             ;; ~677-683) and every presentation function above is mocked to
             ;; a no-op recorder -- nothing in this run is meant to write
             ;; into `runtime-host' at all.  It used to read non-empty only
             ;; because two things wrote into it that were never supposed
             ;; to: the response-mode indicator (fixed by IND-1) and
             ;; Runtime's old default status rendering into a non-View
             ;; buffer (removed by S01, D11).  Neither writer exists any
             ;; more, so the isolation this test is actually about is that
             ;; the protocol/ownership buffer stays completely untouched.
             (should (zerop (buffer-size runtime-host)))
             (with-current-buffer canceler
               (funcall partial-fn
                        '(:text "headless partial"
                          :input-tokens 3 :output-tokens 1))
               (superchat-runtime-cancel
                (plist-get handle :run-id) "headless owner cleanup"))
             (should (eq 'cancelled
                         (plist-get
                          (superchat-runtime-status (plist-get handle :run-id))
                          :status)))
             (should (buffer-live-p runtime-host))
             (should-not presentation-buffers)
             (should-not (get-buffer target-name))
             (with-current-buffer caller
               (setq caller-after
                     (list :text (buffer-string)
                           :authority superchat-authority-mode
                           :surface superchat--last-surface
                           :directory superchat-tools-default-directory
                           :active-run superchat--active-run-id
                           :session superchat--session-id
                           :workspace (copy-tree
                                       superchat--workspace-binding))))
             (should (equal caller-before caller-after)))
         (dolist (buffer (list caller canceler runtime-host
                               (get-buffer target-name)))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest test-terminal-hook-started-runtime-survives-cancel-ui-cleanup ()
  "Cancel cleanup cannot clear a Run started synchronously by a terminal hook."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 10)
             (superchat-response-timeout-hard-limit 20)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             handle-1 handle-2 owner-2 wait-2 hard-2
             wait-2-before hard-2-before)
         (let ((superchat-turn-completed-functions
                (list
                 (lambda (_status)
                   (unless handle-2
                     (setq handle-2
                           (superchat-runtime-test-submit
                            (list :type :llm-query
                                  :input "two"
                                  :clean-input "two"
                                  :prompt "two"
                                  :session-id "session-terminal-owner"
                                  :buffer (current-buffer)
                                  :buffer-name (buffer-name)
                                  :deadline nil)))
                     (setq owner-2
                           (plist-get
                            (superchat-runtime--entry
                             (plist-get handle-2 :run-id))
                            :transport-timer-owner)
                           wait-2
                           (superchat-llm--transport-timer-owner-wait-timer
                            owner-2)
                           hard-2
                           (superchat-llm--transport-timer-owner-hard-timer
                            owner-2)
                           wait-2-before (copy-tree (timer--time wait-2))
                           hard-2-before (copy-tree (timer--time hard-2))))))))
           (unwind-protect
               (cl-letf (((symbol-function 'superchat-tape-append-event)
                          (lambda (_event) (cl-incf next-event-id)))
                         ((symbol-function 'superchat--update-status) #'ignore)
                         ((symbol-function 'superchat--stream-llm-result)
                          #'ignore)
                         ((symbol-function 'superchat--process-llm-result)
                          #'ignore)
                         ((symbol-function 'superchat--response-region-close)
                          #'ignore)
                         ((symbol-function 'read-string)
                          (lambda (&rest _args) ""))
                         ((symbol-function 'llm-chat-streaming)
                          (lambda (&rest _args) 'terminal-owner-request)))
                 (setq handle-1
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "one"
                              :clean-input "one"
                              :prompt "one"
                              :session-id "session-terminal-owner"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil)))
                 (setq-local superchat--active-run-id
                             (plist-get handle-1 :run-id))
                 (superchat-cancel)
                 (should handle-2)
                 (should
                  (eq 'running-model
                      (plist-get
                       (superchat-runtime-status (plist-get handle-2 :run-id))
                       :status)))
                 (should (memq wait-2 timer-list))
                 (should (memq hard-2 timer-list))
                 (should (equal wait-2-before (timer--time wait-2)))
                 (should (equal hard-2-before (timer--time hard-2)))
                 (should
                  (eq wait-2
                      (superchat-llm--transport-timer-owner-wait-timer
                       owner-2)))
                 (should
                  (eq hard-2
                      (superchat-llm--transport-timer-owner-hard-timer
                       owner-2)))
                 (should
                  (eq owner-2
                      (plist-get
                       (superchat-runtime--entry
                        (plist-get handle-2 :run-id))
                       :transport-timer-owner)))
                 (should (eq superchat--active-timeout-timer wait-2))
                 (should (eq superchat--active-timeout-timer-owner owner-2)))
             (when handle-2
               (ignore-errors
                 (superchat-runtime-cancel
                  (plist-get handle-2 :run-id) "cleanup")))
             (dolist (timer (cl-remove-if
                             (lambda (item) (memq item timers-before))
                             timer-list))
               (cancel-timer timer)))))))))

(ert-deftest test-overlapping-runtime-recovery-cannot-republish-an-older-wait ()
  "Recovery rearm respects newer publication and terminal release."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-runtime--runs (make-hash-table :test #'equal))
             (superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout 10)
             (superchat-response-timeout-hard-limit 20)
             (superchat-show-ttft nil)
             (superchat-llm-tools-enabled nil)
             (superchat-pre-topic-finalized-functions nil)
             (superchat-post-turn-functions nil)
             (superchat-post-topic-finalized-functions nil)
             (superchat-run-completed-functions nil)
             (superchat-turn-completed-functions nil)
             (superchat-closure-shadow-enabled nil)
             (superchat-closure-terminal-guard-enabled nil)
             (superchat-closure-shadow-functions nil)
             (superchat-runtime--closure-observations nil)
             (timers-before (copy-sequence timer-list))
             (next-event-id 0)
             (calls 0)
             recovery-resume error-1 response-2 error-3 response-4
             handle-1 handle-2 handle-3 handle-4
             owner-1 owner-2 owner-3 owner-4
             wait-1 wait-1-rearmed wait-2 wait-3 wait-4
             hard-1 hard-2 hard-3 hard-4)
         (unwind-protect
             (let ((superchat-llm-context-budget-exceeded-functions
                    (list
                     (lambda (details)
                       (setq recovery-resume
                             (plist-get details :resume-function))
                       t))))
               (cl-letf (((symbol-function 'superchat-tape-append-event)
                          (lambda (_event) (cl-incf next-event-id)))
                         ((symbol-function 'superchat--update-status) #'ignore)
                         ((symbol-function 'superchat--stream-llm-result)
                          #'ignore)
                         ((symbol-function 'superchat--process-llm-result)
                          #'ignore)
                         ((symbol-function 'llm-make-chat-prompt)
                          #'test-llm--make-native-chat-prompt)
                         ((symbol-function 'llm-chat-streaming)
                          (lambda (_provider _prompt _partial response error
                                   &optional _multi-output)
                            (cl-incf calls)
                            (pcase calls
                              (1 (setq error-1 error))
                              (2 (setq response-2 response))
                              (4 (setq error-3 error))
                              (5 (setq response-4 response)))
                            (intern
                             (format "recovery-overlap-request-%d" calls)))))
                 (setq handle-1
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "before OLD CONTEXT after"
                              :clean-input "before OLD CONTEXT after"
                              :prompt "before OLD CONTEXT after"
                              :context-envelope '(:prompt "OLD CONTEXT")
                              :session-id "session-recovery-overlap"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       owner-1
                       (plist-get
                        (superchat-runtime--entry
                         (plist-get handle-1 :run-id))
                        :transport-timer-owner)
                       wait-1
                       (superchat-llm--transport-timer-owner-wait-timer
                        owner-1)
                       hard-1
                       (superchat-llm--transport-timer-owner-hard-timer
                        owner-1))
                 (should (eq superchat--active-timeout-timer wait-1))
                 (funcall error-1 'llm-context-length-exceeded "too large")
                 (should (functionp recovery-resume))
                 (should-not (memq wait-1 timer-list))
                 (should-not superchat--active-timeout-timer)
                 (setq handle-2
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "two"
                              :clean-input "two"
                              :prompt "two"
                              :session-id "session-recovery-overlap"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       owner-2
                       (plist-get
                        (superchat-runtime--entry
                         (plist-get handle-2 :run-id))
                        :transport-timer-owner)
                       wait-2
                       (superchat-llm--transport-timer-owner-wait-timer
                        owner-2)
                       hard-2
                       (superchat-llm--transport-timer-owner-hard-timer
                        owner-2))
                 (should (eq superchat--active-timeout-timer wait-2))
                 (should (eq superchat--active-timeout-timer-owner owner-2))
                 ;; Even if an unrelated compatibility path has already
                 ;; erased the public slot, the View's publication order is
                 ;; not allowed to move backwards to the older recovery.
                 (let ((wait-2-before (copy-tree (timer--time wait-2)))
                       (hard-2-before (copy-tree (timer--time hard-2))))
                   (setq superchat--active-timeout-timer nil
                         superchat--active-timeout-timer-owner nil)
                   (funcall recovery-resume "NEW CONTEXT")
                   (should (= calls 3))
                   (setq wait-1-rearmed
                         (superchat-llm--transport-timer-owner-wait-timer
                          owner-1))
                   (should (timerp wait-1-rearmed))
                   (should (memq wait-1-rearmed timer-list))
                   (should (equal wait-2-before (timer--time wait-2)))
                   (should (equal hard-2-before (timer--time hard-2)))
                   (should-not superchat--active-timeout-timer)
                   (should-not superchat--active-timeout-timer-owner)
                   (should-not
                    (superchat-llm--transport-timer-owner-publish-wait
                     owner-1 wait-1-rearmed))
                   (should-not superchat--active-timeout-timer)
                   (should
                    (superchat-llm--transport-timer-owner-publish-wait
                     owner-2 wait-2))
                   (should (eq superchat--active-timeout-timer wait-2))
                   (should (eq superchat--active-timeout-timer-owner owner-2)))
                 (should
                  (superchat-runtime-cancel
                   (plist-get handle-1 :run-id) "older cancelled"))
                 (should-not (memq wait-1-rearmed timer-list))
                 (should-not (memq hard-1 timer-list))
                 (should (memq wait-2 timer-list))
                 (should (memq hard-2 timer-list))
                 (should (eq superchat--active-timeout-timer wait-2))
                 (should (eq superchat--active-timeout-timer-owner owner-2))
                 ;; A stale tool callback may arrive after Run 1 released its
                 ;; owner.  Its Runtime identity must fail closed instead of
                 ;; selecting the legacy View-local fallback and moving Run
                 ;; 2's published wait.
                 (let ((superchat--tool-execution-context
                        (list :run-id (plist-get handle-1 :run-id)))
                       (wait-2-before (float-time (timer--time wait-2)))
                       (hard-2-before (copy-tree (timer--time hard-2))))
                   (cl-letf (((symbol-function 'shell-command-to-string)
                              (lambda (_command) "late tool complete")))
                     (should
                      (equal "late tool complete"
                             (superchat-tool-shell-command "ignored"))))
                   (should (< (abs (- (float-time (timer--time wait-2))
                                      wait-2-before))
                              0.01))
                   (cl-letf (((symbol-function 'read-multiple-choice)
                              (lambda (_prompt _choices &optional _help)
                                (cons ?y "yes, allow once"))))
                     (should
                      (superchat-authority-confirm
                       '(:tool-name "read_file"
                         :args nil
                         :effects ((:effect fs-read :resource "fixture"))
                         :request-hash "late-fixture-hash"))))
                   (should (< (abs (- (float-time (timer--time wait-2))
                                      wait-2-before))
                              0.01))
                   (should (equal hard-2-before (timer--time hard-2)))
                   (should (eq superchat--active-timeout-timer wait-2))
                   (should (eq superchat--active-timeout-timer-owner owner-2)))
                 (funcall response-2 "newer complete")
                 (should-not (memq wait-2 timer-list))
                 (should-not (memq hard-2 timer-list))
                 (should-not superchat--active-timeout-timer)
                 (should-not superchat--active-timeout-timer-owner)
                 ;; A separate older Run enters recovery but is released
                 ;; before its asynchronous resume arrives.  The late resume
                 ;; must neither start a provider retry nor rearm a private
                 ;; wait timer after terminal cleanup.
                 (setq handle-3
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "before STALE CONTEXT after"
                              :clean-input "before STALE CONTEXT after"
                              :prompt "before STALE CONTEXT after"
                              :context-envelope '(:prompt "STALE CONTEXT")
                              :session-id "session-late-recovery"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       owner-3
                       (plist-get
                        (superchat-runtime--entry
                         (plist-get handle-3 :run-id))
                        :transport-timer-owner)
                       wait-3
                       (superchat-llm--transport-timer-owner-wait-timer
                        owner-3)
                       hard-3
                       (superchat-llm--transport-timer-owner-hard-timer
                        owner-3))
                 (should (eq superchat--active-timeout-timer wait-3))
                 (funcall error-3 'llm-context-length-exceeded "too large")
                 (should (functionp recovery-resume))
                 (should-not
                  (superchat-llm--transport-timer-owner-wait-timer owner-3))
                 (setq handle-4
                       (superchat-runtime-test-submit
                        (list :type :llm-query
                              :input "four"
                              :clean-input "four"
                              :prompt "four"
                              :session-id "session-late-recovery"
                              :buffer (current-buffer)
                              :buffer-name (buffer-name)
                              :deadline nil))
                       owner-4
                       (plist-get
                        (superchat-runtime--entry
                         (plist-get handle-4 :run-id))
                        :transport-timer-owner)
                       wait-4
                       (superchat-llm--transport-timer-owner-wait-timer
                        owner-4)
                       hard-4
                       (superchat-llm--transport-timer-owner-hard-timer
                        owner-4))
                 (should
                  (superchat-runtime-cancel
                   (plist-get handle-3 :run-id) "recovery owner released"))
                 (should-not (memq wait-3 timer-list))
                 (should-not (memq hard-3 timer-list))
                 (let ((calls-before calls)
                       (wait-4-before (copy-tree (timer--time wait-4)))
                       (hard-4-before (copy-tree (timer--time hard-4))))
                   (funcall recovery-resume "FRESH CONTEXT")
                   (should (= calls calls-before))
                   (should-not
                    (superchat-llm--transport-timer-owner-wait-timer owner-3))
                   (should (equal wait-4-before (timer--time wait-4)))
                   (should (equal hard-4-before (timer--time hard-4)))
                   (should (eq superchat--active-timeout-timer wait-4))
                   (should (eq superchat--active-timeout-timer-owner owner-4)))
                 (funcall response-4 "latest complete")
                 (should-not (memq wait-4 timer-list))
                 (should-not (memq hard-4 timer-list))
                 (should-not superchat--active-timeout-timer)
                 (should-not superchat--active-timeout-timer-owner)))
           (dolist (timer (cl-remove-if
                           (lambda (item) (memq item timers-before))
                           timer-list))
             (cancel-timer timer))))))))

(ert-deftest test-nonpartial-provider-leaves-e2e-timing-in-assistant-header ()
  "A callback-only provider leaves a durable non-partial timing annotation."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (let ((superchat-buffer-name (buffer-name))
             (superchat-llm-backend
              (test-llm--make-mock-backend 'openai))
             (superchat-response-timeout nil)
             (superchat-show-ttft t)
             (superchat-llm--distribution-adapters
              (list :nonpartial-latency-function
                    #'superchat--llm-report-nonpartial-latency)))
         (setq-local superchat--ttft-start-time 100.0
                     superchat--response-region-states nil
                     superchat--response-region-current-owner nil
                     superchat--prompt-start nil
                     superchat--current-response-parts nil)
         (insert "* User: hello\n")
         (superchat--prepare-for-response)
         (cl-letf (((symbol-function 'float-time)
                    (lambda (&rest _args) 103.0))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial-cb response-cb
                             _error-cb &optional _multi-output)
                      (funcall response-cb "whole response")
                      'provider-request))
                   ((symbol-function 'superchat--record-message) #'ignore))
           (superchat--llm-generate-answer
            "hello" #'superchat--process-llm-result
            #'superchat--stream-llm-result))
         (let ((text (buffer-string)))
           (should (string-match-p "E2E: 3\\.00s" text))
           (should
            (string-match-p
             "TTFT unavailable (non-partial provider)" text)))
         (superchat--response-region-reset))))))

(ert-deftest test-llm-context-budget-compacts-and-resumes-same-run ()
  "A claimed budget recovery rebuilds the prompt before its first request."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((old-context (make-string 16000 ?x))
            (superchat-llm-backend
             (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-llm-tools-enabled nil)
            (superchat-llm-output-token-reserve 2048)
            (superchat--llm-context-envelope (list :prompt old-context))
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
                   ((symbol-function 'llm-provider-chat-request)
                    #'test-llm--openai-like-chat-request)
                   ((symbol-function 'llm-count-tokens)
                    #'test-llm--generic-word-token-count)
                   ((symbol-function 'llm-make-chat-prompt)
                    #'test-llm--make-native-chat-prompt)
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
            (format "before\n%s\nafter" old-context)
            (lambda (result) (setq final result)) #'ignore)
           (should (eq 'threshold recovery-reason))
           (should (= 1 provider-calls))
           (should (equal "done" final))
           (should (string-match-p "NEW CONTEXT" provider-text))
           (should-not (string-match-p (regexp-quote old-context)
                                       provider-text))
           (let ((ordered (nreverse events)))
             (should
              (equal '(context-budget-exceeded context-recovery-started
                        attempt-started terminal)
                     (mapcar #'car ordered)))
             (should (> (plist-get (cadr (nth 0 ordered)) :context-used)
                        7952))
             (should (< (plist-get (cadr (nth 2 ordered)) :context-used)
                        7952)))))))))

(ert-deftest test-llm-pause-defers-provider-and-resumes-pending-request ()
  "A pause checkpoint must defer the provider call until its continuation runs."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled nil)
           (superchat--pause-requested t)
           (superchat--pause-run-id "run-pause")
           (superchat--pending-send-request nil)
           (superchat--llm-transport-timer-owner-function
            (lambda (_owner) "run-pause"))
           (provider-calls 0)
           (boundary-calls 0)
           final)
       (cl-letf (((symbol-function 'superchat--pause-boundary)
                  (lambda (_run-id _owner) (cl-incf boundary-calls)))
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
         (let ((pending superchat--pending-send-request))
           (should (superchat-llm--pending-pause-p pending))
           (should (eq 'initial
                       (superchat-llm--pending-pause-reason pending)))
           (should
            (functionp
             (superchat-llm--pending-pause-continuation pending)))
           (setq superchat--pause-requested nil)
           (funcall (superchat-llm--pending-pause-continuation pending)
                    (superchat-llm--pending-pause-reason pending)))
         (should (= 1 provider-calls))
         (should (equal "resumed" final)))))))

(ert-deftest test-llm-pause-steer-reestimates-before-provider ()
  "A large steer received while paused is budgeted before the attempt."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled nil)
           (superchat-llm-output-token-reserve 128)
           (superchat--pause-requested t)
           (superchat--pause-run-id "run-pause")
           (superchat--pending-send-request nil)
           (superchat--llm-transport-timer-owner-function
            (lambda (_owner) "run-pause"))
           (steer (mapconcat #'identity (make-list 5000 "STEER") " "))
           (drained nil)
           (provider-calls 0)
           budget-details attempt-details final)
       (let ((superchat--llm-runtime-observer
              (lambda (event value)
                (pcase event
                  ('context-budget-exceeded
                   (setq budget-details value))
                  ('attempt-started
                   (setq attempt-details value)))))
             (superchat--llm-drain-steers-function
              (lambda ()
                (unless drained
                  (setq drained t)
                  (list steer)))))
         (cl-letf (((symbol-function 'superchat--llm-context-capability)
                    (lambda (_provider _model)
                      '(:limit 1000 :source override)))
                   ((symbol-function 'llm-provider-chat-request)
                    #'test-llm--openai-like-chat-request)
                   ((symbol-function 'llm-count-tokens)
                    #'test-llm--generic-word-token-count)
                   ((symbol-function 'llm-make-chat-prompt)
                    #'test-llm--make-native-chat-prompt)
                   ((symbol-function 'llm-chat-prompt-append-response)
                    #'test-llm--append-native-prompt-response)
                   ((symbol-function 'superchat--pause-boundary) #'ignore)
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider _prompt _partial response _error
                             &optional _multi-output)
                      (cl-incf provider-calls)
                      (funcall response "provider-called")
                      'provider-request)))
           (superchat--llm-generate-answer
            "short" (lambda (value) (setq final value)) #'ignore)
           (should (= 0 provider-calls))
           (let ((pending superchat--pending-send-request))
             (should (superchat-llm--pending-pause-p pending))
             (setq superchat--pause-requested nil)
             (funcall (superchat-llm--pending-pause-continuation pending)
                      (superchat-llm--pending-pause-reason pending)))
           (should (= 0 provider-calls))
           (should-not attempt-details)
           (should (> (plist-get budget-details :context-used) 872))
           (should (string-match-p "请求未发送" final))))))))

(ert-deftest test-llm-cancelled-pause-state-allows-next-request ()
  "A cancelled pause must not block the next request in the same View."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend
            (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled nil)
           (superchat--pause-requested t)
           (superchat--pause-run-id "run-pause")
           (superchat--pending-send-request nil)
           (superchat--llm-transport-timer-owner-function
            (lambda (_owner) "run-pause"))
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
          (let ((owner
                 (superchat-llm--make-transport-timer-owner
                  :buffer (current-buffer) :run-id "dead-run")))
            (setq-local superchat--pause-requested t
                        superchat--pause-run-id "dead-run"
                        superchat--active-run-id "dead-run"
                        superchat--pending-send-request
                        (superchat-llm--make-pending-pause
                         :run-id "dead-run"
                         :transport-owner owner
                         :reason 'tool-followup
                         :continuation
                         (lambda (_reason) (setq invoked t)))))
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
          (let ((owner
                 (superchat-llm--make-transport-timer-owner
                  :buffer (current-buffer) :run-id "live-run")))
            (setq-local superchat--pause-requested t
                        superchat--pause-run-id "live-run"
                        superchat--active-run-id "live-run"
                        superchat--pending-send-request
                        (superchat-llm--make-pending-pause
                         :run-id "live-run"
                         :transport-owner owner
                         :reason 'tool-followup
                         :continuation
                         (lambda (_reason) (error "resume boom"))))
          (cl-letf (((symbol-function 'superchat-runtime-effect-capable-p)
                     (lambda (run-id) (equal run-id "live-run")))
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
                           messages))))
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
           (superchat-llm-tools-enabled nil)
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
            ;; This test is about the prompt/evidence shape, not tool
            ;; policy; disable tools explicitly rather than ride the
            ;; on-demand default (which now attaches tools to every
            ;; request, approved decision 3, D14).
            (superchat-llm-tools-enabled nil)
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
       ;; S08-fix-3 item 1 (option D): `llm-make-chat-prompt' is the real
       ;; function (built by `test-llm--with-mock-llm' onto a real struct);
       ;; only the provider transport is replaced here.
       (cl-letf (((symbol-function 'llm-chat-streaming)
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
         ;; S08-fix-3 item 1 (option D): `llm-make-chat-prompt' is the real
         ;; function, building a real struct; each CASE below deliberately
         ;; breaks only the one accessor it names, leaving the other real
         ;; (`llm-chat-prompt-interaction-role'/`-content' need no override
         ;; either -- real interactions answer them directly).
         (cl-letf (((symbol-function 'llm-chat-prompt-interactions)
                    (pcase case
                      ('messages-unavailable (lambda (_prompt) nil))
                      ('messages-error
                       (lambda (_prompt) (error "messages unreadable")))
                      (_ (symbol-function 'llm-chat-prompt-interactions))))
                   ((symbol-function 'llm-chat-prompt-tools)
                    (pcase case
                      ('tools-unavailable (lambda (_prompt) nil))
                      ('tools-error
                       (lambda (_prompt) (error "tools unreadable")))
                      (_ (symbol-function 'llm-chat-prompt-tools))))
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
               (setq-local superchat--session-id "agent-origin-session")
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
                  ;; Approved decision 3 (D14): `on-demand' attaches
                  ;; tools like `always' now, so this mock is called
                  ;; (with TOOL-NAMES) even for plain, keyword-free
                  ;; input; it stays a no-tools stub for this test's own
                  ;; concern.
                  (lambda (&optional _tool-names) nil))
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
         (let ((superchat--llm-drain-steers-function
                (lambda () '("take another path"))))
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

(ert-deftest test-async-dispatcher-plain-chat-attaches-tools-via-one-registry-walk ()
  "Approved decision 3 (D14): `on-demand' behaves exactly like `always' now
-- the keyword heuristic this test used to require (`superchat--should-
attach-tools-p') was deleted, so a plain, keyword-free chat request DOES
walk the tool registry once.  MCP tools still arrive only through that
same `superchat-get-llm-tools' call (its own docstring), never a second,
separate MCP round-trip -- MCP-CALLS stays 0.  (Renamed from
`test-async-dispatcher-plain-chat-does-not-enumerate-tools', S09-fix-5
item 4: the old name said the opposite of what this test asserts and has
always asserted since D14 -- it still uses `superchat-llm-tool-names' `all'
to keep its arbitrarily-named mock tool out of the configured-names
filter, so it does not itself guard the default five-name baseline;
`llm/select-tools-decision-includes-plugin-registered-tool-by-default' and
`llm/select-tools-decision-excludes-unconfigured-builtin-tool' below cover
that.)"
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (superchat-llm-tools-enabled 'on-demand)
           ;; `all' so the baseline's own configured-names filter
           ;; (S09-fix-3 item 1) does not drop this mock's arbitrarily
           ;; named tool -- only `all' passes every registered/cached name
           ;; through unfiltered.
           (superchat-llm-tool-names 'all)
           (superchat-show-response-mode t)
           (superchat--current-command nil)
           (tool-calls 0)
           (mcp-calls 0)
           captured-prompt
           captured-multi-output)
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda (&optional _tool-names)
                    (setq tool-calls (1+ tool-calls))
                    (list (test-llm--make-tool :name "baseline-tool"))))
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
         (should (= 1 tool-calls))
         (should (= 0 mcp-calls))
         (should (llm-chat-prompt-p captured-prompt))
         (should (string= "plain question"
                          (llm-chat-prompt-interaction-content
                           (car (llm-chat-prompt-interactions captured-prompt)))))
         (should (equal '("baseline-tool")
                        (mapcar #'llm-tool-name (llm-chat-prompt-tools captured-prompt))))
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
                              &optional _target-model _context-files _tools _system _preset
                              _precomputed-tool-decision)
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
                              &optional _target-model _context-files _tools _system _preset
                              _precomputed-tool-decision)
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
         ;; S10: `superchat-select-tools-decision' rule 1 filters an
         ;; explicit `:tools' list to registered descriptors -- previously
         ;; only enforced inside the real (here-stubbed)
         ;; `superchat--llm-generate-answer'; the pipeline now enforces it
         ;; once, earlier, for every chat submission (contract §10 gate 8:
         ;; "every selected name has an effect resolver", an existing S09
         ;; invariant this slice moved the enforcement point for, not a new
         ;; one).  Real, already-registered tool names preserve this test's
         ;; own intent (the preset's own tools reach the model call) under
         ;; that invariant; the old fake names ("read-file"/"search-text")
         ;; never had a resolver and were silently dropped.
         (active-preset (superchat-preset-from-plist
                         (list :name "coder"
                               :type 'agent
                               :tools '("list_buffers" "read_buffer"))))
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
                              &optional _target-model _context-files tools _system preset
                              _precomputed-tool-decision)
                       (setq captured-tools tools
                             captured-preset preset)
                       ;; Don't actually stream; finalize immediately.
                       nil)))
            (superchat-send-input)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))
    (should captured-tools)
    (should (eq active-preset captured-preset))
    (should (member "list_buffers" captured-tools))
    (should (member "read_buffer" captured-tools))))

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

(ert-deftest test-parser-model-switch-only-recognizes-command-head ()
  "An email-like token in pasted body text is not a model switch."
  (should-not
   (superchat-parser-model-switch
    "Review this source:\nconst owner = user@outlook.com;")))

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

(ert-deftest test-parser-file-reference-only-recognizes-command-head ()
  "A #path after ordinary prose is body text, not an attachment."
  (should (equal superchat--file-ref-regexp
                 superchat-parser--file-ref-regexp))
  (should-not
   (superchat-parser-extract-file-path
    "Review this pasted source:\nconst path = '#/etc/hosts';")))

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

(ert-deftest test-collect-llm-tools-deduplicates-registry-before-synthesis ()
  "The registry tool wins when widening meets a same-named synthesis."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names '("read_buffer"))
           (superchat-llm-tools-enabled 'always))
       (let* ((registry (car (superchat-get-llm-tools)))
              (synthesis (test-llm--make-tool
                          :name "read_buffer"
                          :description "synthesis duplicate"))
              (superchat-llm-synthesized-tools (list synthesis))
              (tools (superchat--collect-llm-tools "plain request")))
         (should (equal '("read_buffer")
                        (mapcar #'llm-tool-name tools)))
         (should (eq registry (car tools))))))))

(ert-deftest test-collect-llm-tools-plan-wins-before-synthesis ()
  "The active plan tool wins over a same-named synthesized tool."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-plan)
     (let ((superchat-llm-tools-list nil)
           (superchat--llm-tool-names-signature nil)
           (superchat-llm-tool-names
            '("list_buffers" "apropos" "read_buffer" "edit_buffer"
              "eval_elisp"))
           (superchat-llm-tools-enabled 'always))
       (with-temp-buffer
         (setq-local superchat--active-plan
                     (list :summary "approved" :event-id "plan-event"
                           :steps nil))
         (let* ((synthesis (test-llm--make-tool
                            :name "report_plan_progress"
                            :description "synthesis duplicate"))
                (superchat-llm-synthesized-tools (list synthesis))
                (superchat-buffer-name (buffer-name))
                (tools (superchat--collect-llm-tools "plain request"))
                (plan-tools
                 (cl-remove-if-not
                  (lambda (tool)
                    (equal "report_plan_progress" (llm-tool-name tool)))
                  tools)))
           (should (= 1 (length plan-tools)))
           (should (eq #'superchat-tool-report-plan-progress
                       (llm-tool-function (car plan-tools))))
           (should (string-match-p
                    "Report progress on the approved plan"
                    (llm-tool-description (car plan-tools))))
           (should-not (member synthesis tools))))))))

(ert-deftest test-render-reasoning-default-cap-is-20000-and-keeps-tail ()
  "The unbound product default retains one bounded newest-tail value."
  (with-temp-buffer
    (org-mode)
    (let ((superchat-buffer-name (buffer-name)))
      (should (= 20000 (default-value 'superchat-reasoning-max-chars)))
      (should (= 20000 superchat-reasoning-max-chars))
      (setq-local superchat-mode t
                  superchat--session-id "reasoning-default-cap-session"
                  superchat--response-region-states nil
                  superchat--response-region-current-owner nil
                  superchat--prompt-start nil
                  superchat--current-response-parts nil)
      (insert "* User: cap\n")
      (superchat--response-region-prepare)
      (dolist (delta (list (make-string 9000 ?A)
                           (make-string 9000 ?B)
                           (concat (make-string 5000 ?C) "REAL-TAIL-1")
                           (concat (make-string 1000 ?D) "REAL-TAIL-2")))
        (superchat--stream-llm-result (list :reasoning delta)))
      (let* ((state (cdr (assq :pending
                               superchat--response-region-states)))
             (text (plist-get state :reasoning-text)))
        (should (= 20000 (length text)))
        (should (= 1 (test-render--match-count "Thinking truncated" text)))
        (should (string-match-p "REAL-TAIL-1" text))
        (should (string-suffix-p "REAL-TAIL-2" text))))))

(ert-deftest test-reasoning-baseline-resets-across-new-provider-requests ()
  "Protocol retry, tool round, and recovery each emit a fresh first partial."
  (test-llm--with-mock-llm
   (lambda ()
     (dolist
         (case
          (list
           (list :name 'protocol-retry
                 :tools-enabled 'always
                 :tools (list (test-llm--make-tool :name "render_ui"))
                 :prompt "retry"
                 :first
                 (lambda (_response error)
                   (funcall error 'llm-tool-missing-argument
                            "missing components")))
           (list :name 'tool-round
                 :tools-enabled 'always
                 :tools (list (test-llm--make-tool :name "fake-tool"))
                 :prompt "tool round"
                 :first
                 (lambda (response _error)
                   (funcall response '((fake-tool . "tool result")))))
           (list :name 'context-recovery
                 :tools-enabled nil
                 :prompt "before\nOLD CONTEXT\nafter"
                 :context "OLD CONTEXT"
                 :first
                 (lambda (_response error)
                   (funcall error 'llm-context-length-exceeded
                            "provider rejected request")))))
       (let* ((name (plist-get case :name))
              (recovering (eq name 'context-recovery))
              (superchat-llm-backend
               (test-llm--make-mock-backend 'openai))
              (superchat-response-timeout nil)
              (superchat-llm-tools-enabled
               (plist-get case :tools-enabled))
              (superchat-llm-tools-list (plist-get case :tools))
              (superchat--llm-context-envelope
               (and recovering (list :prompt (plist-get case :context))))
              (superchat-llm-context-budget-exceeded-functions
               (and recovering
                    (list
                     (lambda (details)
                       (funcall (plist-get details :resume-function)
                                "NEW CONTEXT")
                       t))))
              (requests 0)
              chunks final)
         (ert-info ((format "reasoning reset seam: %s" name))
           (cl-letf
               (((symbol-function 'superchat--llm-drop-dangling-tool-use)
                 #'ignore)
                ((symbol-function 'llm-chat-prompt-append-response) #'ignore)
                ((symbol-function 'superchat--response-region-close) #'ignore)
                ((symbol-function 'superchat--llm-context-capability)
                 (lambda (_provider _model) '(:limit 10000)))
                ((symbol-function 'superchat--llm-estimate-context-tokens)
                 (lambda (_provider _prompt) 1000))
                ((symbol-function 'llm-make-chat-prompt)
                 (lambda (text &rest args)
                   (make-llm-chat-prompt
                    :interactions
                    (list (make-llm-chat-prompt-interaction
                           :role 'user :content text))
                    :tools (plist-get args :tools))))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider _prompt partial response error
                          &optional _multi-output)
                   (cl-incf requests)
                   (funcall partial '(:reasoning "same thought"))
                   (if (= requests 1)
                       (funcall (plist-get case :first) response error)
                     (funcall response "done")))))
             (superchat--llm-generate-answer
              (plist-get case :prompt)
              (lambda (value) (setq final value))
              (lambda (chunk) (push chunk chunks)))
             (should (= 2 requests))
             (should
              (equal '((:reasoning "same thought")
                       (:reasoning "same thought"))
                     (nreverse chunks)))
             (should (equal "done" final)))))))))

(ert-deftest test-llm-configure-rejects-malformed-adapter-plists ()
  "The transport adapter port rejects malformed configuration atomically."
  (let ((superchat-llm--distribution-adapters
         (list :activity-function #'ignore)))
    (dolist (case
             (list
              (list (list :unknown-function #'ignore)
                    "Unknown Superchat LLM adapter key")
              (list (list :activity-function)
                    "even key/value plist")
              (list (list :activity-function #'ignore
                          :activity-function 42)
                    "Duplicate Superchat LLM adapter key")
              (list (list :activity-function 42)
                    "Invalid Superchat LLM adapter")))
      (let* ((before (copy-tree superchat-llm--distribution-adapters))
             (error-data
              (should-error
               (apply #'superchat-llm-configure (car case))
               :type 'error)))
        (should (string-match-p (cadr case)
                                (error-message-string error-data)))
        (should (equal before superchat-llm--distribution-adapters))))
    (superchat-llm-configure :activity-function #'ignore)
    (should
     (equal (list :activity-function #'ignore)
            superchat-llm--distribution-adapters))))

(defvar superchat-workflow-directory)

(defconst test-llm--workflow-caller-snapshot-exclusions
  '(;; Emacs display code owns this timestamp; it is not a workflow write.
    buffer-display-time
    ;; Emacs display code owns this counter; it is not a workflow write.
    buffer-display-count)
  "Volatile Emacs display locals omitted from workflow caller snapshots.")

(defun test-llm--workflow-caller-snapshot ()
  "Return caller text, point, and a stable full buffer-local snapshot."
  (let ((print-circle t)
        (print-length nil)
        (print-level nil))
    (list
     :text (buffer-string)
     :point (point)
     :locals
     (sort
      (mapcar
       (lambda (binding)
         (let ((name (if (symbolp binding) binding (car binding))))
           (cons (symbol-name name) (prin1-to-string binding))))
       (cl-remove-if
        (lambda (binding)
          (memq (if (symbolp binding) binding (car binding))
                test-llm--workflow-caller-snapshot-exclusions))
        (buffer-local-variables)))
      (lambda (left right) (string< (car left) (car right)))))))

(defun test-llm--make-workflow-fixture (stem &optional live-target)
  "Return a narrow workflow fixture named from STEM.
When LIVE-TARGET is non-nil, create the View buffer as part of the fixture."
  (let* ((directory
          (make-temp-file (format "superchat-workflow-%s-" stem) t))
         (target-name
          (generate-new-buffer-name
           (format " *superchat-workflow-%s-target*" stem))))
    (list :directory directory
          :workflow-file
          (expand-file-name (format "%s.workflow" stem) directory)
          :target-name target-name
          :target (and live-target (get-buffer-create target-name))
          :caller
          (generate-new-buffer
           (format " *superchat-workflow-%s-caller*" stem)))))

(defun test-llm--workflow-protocol-buffers ()
  "Return the live private Runtime hosts created for workflow tests."
  (cl-remove-if-not
   (lambda (buffer)
     (string-prefix-p " *superchat-workflow-protocol:" (buffer-name buffer)))
   (buffer-list)))

(defun test-llm--cleanup-workflow-fixture (fixture &rest buffers)
  "Remove FIXTURE, its private Runtime hosts, and additional BUFFERS."
  (dolist (buffer
           (delete-dups
            (append (test-llm--workflow-protocol-buffers)
                    buffers
                    (list (plist-get fixture :target)
                          (plist-get fixture :caller)
                          (get-buffer (plist-get fixture :target-name))))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (kill-local-variable 'kill-buffer-query-functions))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (let ((directory (plist-get fixture :directory)))
    (when (file-directory-p directory)
      (delete-directory directory t))))

(ert-deftest test-workflow-headless-sync-rejection-retains-session-host ()
  "A pre-accept Runtime rejection leaves the Session-owned host reusable."
  (require 'superchat-workflow)
  (let* ((directory (make-temp-file "superchat-workflow-sync-reject-" t))
         (workflow-file (expand-file-name "sync-reject.workflow" directory))
         (target-name
          (generate-new-buffer-name " *superchat-workflow-sync-reject-target*"))
         (caller
          (generate-new-buffer " *superchat-workflow-sync-reject-caller*"))
         (session-id "workflow-sync-reject-session")
         (superchat-workflow-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         caller-before caller-after protocol-buffers result provider-called)
    (unwind-protect
        (progn
          (with-temp-file workflow-file
            (insert "@gpt-4o-mini\n"))
          (cl-letf (((symbol-function 'llm-chat-streaming)
                     (lambda (&rest _args) (setq provider-called t))))
            (with-current-buffer caller
              (insert "FOREIGN-DOCUMENT-CONTENT")
              (setq-local superchat-authority-mode 'execute)
              (setq-local superchat--last-surface 'foreign-caller)
              (setq-local superchat-tools-default-directory
                          "/tmp/workflow-sync-reject-caller-directory")
              (setq-local superchat--active-run-id "foreign-run")
              (setq-local superchat--session-id session-id)
              (setq-local superchat--workspace-binding
                          '(:id "foreign-workspace"
                            :root "/tmp/workflow-sync-reject-workspace"))
              (setq caller-before (test-llm--workflow-caller-snapshot))
              (let ((superchat-buffer-name target-name)
                    (superchat-lang "English"))
                (setq result
                      (superchat-workflow-execute-async
                       "sync-reject" nil workflow-file)))
              (setq caller-after (test-llm--workflow-caller-snapshot))))
          (setq protocol-buffers
                (superchat-editor-workspace-owned-buffers session-id))
          (should-not result)
          (should-not provider-called)
          (should (= 1 (length protocol-buffers)))
          (should (eq (car protocol-buffers)
                      (superchat-editor-workspace-buffer
                       session-id 'workflow-protocol)))
          (should (= 0 (hash-table-count superchat-runtime--runs)))
          (should-not (get-buffer target-name))
          (should (equal caller-before caller-after)))
      (dolist (buffer protocol-buffers)
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (when (buffer-live-p caller)
        (kill-buffer caller))
      (when (get-buffer target-name)
        (kill-buffer target-name))
      (when (file-directory-p directory)
        (delete-directory directory t))
      (ignore-errors (superchat-session-workspace-close session-id)))))

 (ert-deftest test-workflow-headless-accepted-host-is-session-owned ()
  "An accepted headless host lives until its Session Workspace closes."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-workflow)
  (let* ((fixture (test-llm--make-workflow-fixture "accepted-host"))
         (directory (plist-get fixture :directory))
         (workflow-file (plist-get fixture :workflow-file))
         (target-name (plist-get fixture :target-name))
         (caller (plist-get fixture :caller))
         (session-id "workflow-accepted-host-session")
         (superchat-workflow-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-runtime-default-deadline nil)
         (superchat-llm-backend (test-llm--make-mock-backend 'openai))
         ;; S08: a tool-free attempt now requires proof of the `:streaming'
         ;; capability rather than streaming unconditionally.  The mock
         ;; backend is a plain plist with no `llm-capabilities' method and
         ;; no adapter-rule type match, and `superchat--provider-chat-model'
         ;; has no method for it either, so `superchat-llm--resolve-streaming'
         ;; resolves it under the fallback model name "unknown" (verified by
         ;; instrumenting the resolver's call); declare that key explicitly
         ;; so dispatch still picks the `llm-chat-streaming' transport this
         ;; fixture mocks.
         (superchat-llm-model-capability-overrides
          (list (list "unknown" :streaming t :streaming-tool-use t)))
         (superchat-response-timeout nil)
         (superchat-show-ttft nil)
         (superchat-llm-tools-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         handle host response-function entry protocol-buffers)
    (unwind-protect
        (progn
          (with-temp-file workflow-file
            (insert "one valid step\n"))
          (cl-letf
              (((symbol-function 'llm-make-chat-prompt)
                (lambda (text &rest args)
                  (apply #'test-llm--make-native-chat-prompt text args)))
               ((symbol-function 'llm-chat-streaming)
               (lambda (_provider _prompt _partial response _error
                                   &optional _multi-output)
                  (setq host (current-buffer)
                        response-function response)
                  'workflow-accepted-host-provider-request)))
            (with-current-buffer caller
              (setq-local superchat--session-id session-id)
              (let ((superchat-buffer-name target-name)
                    (superchat-lang "English"))
                (setq handle
                      (superchat-workflow-execute-async
                       "accepted-host" nil workflow-file))))
            (should handle)
            (should (functionp response-function))
            (should (buffer-live-p host))
            (funcall response-function "accepted answer")
            (setq entry
                  (superchat-runtime--entry (plist-get handle :run-id)))
            (should (eq 'completed (plist-get entry :status)))
            (should (buffer-live-p host))
            (should (eq host (plist-get entry :buffer)))
            (should (plist-get entry :quiesced)))
          (setq protocol-buffers
                (superchat-editor-workspace-owned-buffers session-id))
          (should (equal (list host) protocol-buffers))
          (should-not (get-buffer target-name)))
      (ignore-errors (superchat-session-workspace-close session-id))
      (test-llm--cleanup-workflow-fixture fixture host))))

(ert-deftest test-workflow-dead-target-sync-rejection-retains-session-host ()
  "A rejected next step leaves its Session-owned protocol host reusable."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-workflow)
  (let* ((fixture (test-llm--make-workflow-fixture "dead-reject" t))
         (directory (plist-get fixture :directory))
         (workflow-file (plist-get fixture :workflow-file))
         (target-name (plist-get fixture :target-name))
         (target (plist-get fixture :target))
         (caller (plist-get fixture :caller))
         session-id
         (superchat-workflow-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-runtime-default-deadline nil)
         (superchat-llm-backend (test-llm--make-mock-backend 'openai))
         ;; S08: see the identical comment in
         ;; test-workflow-headless-accepted-host-is-session-owned above.
         (superchat-llm-model-capability-overrides
          (list (list "unknown" :streaming t :streaming-tool-use t)))
         (superchat-response-timeout nil)
         (superchat-show-ttft nil)
         (superchat-llm-tools-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (original-validate
          (symbol-function 'superchat-runtime--validate-request))
         caller-before caller-after handle response-function
         validate-requests protocol-buffers (provider-calls 0) failure-text)
    (unwind-protect
        (progn
          (with-temp-file workflow-file
            (insert "first step\n@gpt-4o-mini\n"))
          (with-current-buffer target
            (setq-local superchat-mode t)
            (setq-local superchat-buffer-name target-name)
            (setq-local superchat--session-id
                        (setq session-id
                              (format "workflow-dead-reject-%x"
                                      (random most-positive-fixnum)))))
          (cl-letf
              (((symbol-function
                 'superchat--response-region--prompt-line-position)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--insert-prompt) #'ignore)
               ((symbol-function
                 'superchat--response-region-insert-transcript)
                (lambda (text &rest _args)
                  (when (string-prefix-p "** Workflow FAILED" text)
                    (setq failure-text text))))
               ((symbol-function 'superchat--response-region-close) #'ignore)
               ((symbol-function 'superchat--update-status) #'ignore)
               ((symbol-function 'llm-make-chat-prompt)
                (lambda (text &rest args)
                  (apply #'test-llm--make-native-chat-prompt text args)))
               ((symbol-function 'llm-chat-streaming)
                (lambda (_provider _prompt _partial response _error
                                   &optional _multi-output)
                  (cl-incf provider-calls)
                  (setq response-function response)
                  'workflow-dead-reject-provider-request))
               ((symbol-function 'superchat-runtime--validate-request)
                (lambda (request)
                  (push (copy-tree request t) validate-requests)
                  (funcall original-validate request))))
            (with-current-buffer caller
              (insert "FOREIGN-DOCUMENT-CONTENT")
              (setq-local superchat-authority-mode 'execute)
              (setq-local superchat--last-surface 'foreign-caller)
              (setq-local superchat-tools-default-directory
                          "/tmp/workflow-dead-reject-caller-directory")
              (setq-local superchat--active-run-id "foreign-run")
              (setq-local superchat--session-id "foreign-session")
              (setq-local superchat--workspace-binding
                          '(:id "foreign-workspace"
                            :root "/tmp/workflow-dead-reject-workspace"))
              (setq caller-before (test-llm--workflow-caller-snapshot))
              (let ((superchat-buffer-name target-name)
                    (superchat-lang "English"))
                (setq handle
                      (superchat-workflow-execute-async
                       "dead-reject" nil workflow-file))))
            (should-not failure-text)
            (should (functionp response-function))
            (should (= 1 provider-calls))
            (kill-buffer target)
            (with-current-buffer caller
              (funcall response-function "first answer")
              (setq caller-after (test-llm--workflow-caller-snapshot))))
          (setq validate-requests (nreverse validate-requests)
                protocol-buffers
                (superchat-editor-workspace-owned-buffers session-id))
          ;; S13b: the second step's empty prompt is now rejected by the
          ;; envelope validation (`:content' must be non-empty) before any
          ;; Runtime Request exists, so only the accepted first step reaches
          ;; `superchat-runtime--validate-request'.  The rejection still
          ;; leaves the Session-owned host reusable, which is the point.
          (should (= 1 (length validate-requests)))
          (should (= 1 (length protocol-buffers)))
          (should (eq (car protocol-buffers)
                      (superchat-editor-workspace-buffer
                       session-id 'workflow-protocol)))
          (should (= 1 provider-calls))
          (should (= 1 (hash-table-count superchat-runtime--runs)))
          (should
           (eq 'completed
               (plist-get
                (superchat-runtime-status (plist-get handle :run-id))
                :status)))
          (should-not (get-buffer target-name))
          (should (equal caller-before caller-after)))
      (ignore-errors (superchat-session-workspace-close session-id))
      (test-llm--cleanup-workflow-fixture fixture))))

(ert-deftest test-workflow-live-view-unbound-authority-mode-is-conservative ()
  "An unbound live-View mode does not signal and retains ask semantics."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (require 'superchat-workflow)
  (let* ((fixture (test-llm--make-workflow-fixture "unbound-mode" t))
         (directory (plist-get fixture :directory))
         (workflow-file (plist-get fixture :workflow-file))
         (target-name (plist-get fixture :target-name))
         (target (plist-get fixture :target))
         (superchat-workflow-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-runtime-default-deadline nil)
         (superchat-llm-backend (test-llm--make-mock-backend 'openai))
         ;; S08: see the identical comment in
         ;; test-workflow-headless-accepted-host-is-session-owned above.
         (superchat-llm-model-capability-overrides
          (list (list "unknown" :streaming t :streaming-tool-use t)))
         (superchat-response-timeout nil)
         (superchat-show-ttft nil)
         (superchat-llm-tools-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         saved-mode saved-mode-local-p handle response-function
         provider-buffer authority-decision observed-error)
    (unwind-protect
        (progn
          (with-temp-file workflow-file
            (insert "one valid step\n"))
          (with-current-buffer target
            (setq-local superchat-mode t)
            (setq-local superchat-buffer-name target-name)
            (setq-local superchat--session-id
                        (format "workflow-unbound-mode-%x"
                                (random most-positive-fixnum)))
            (setq saved-mode superchat-authority-mode
                  saved-mode-local-p
                  (local-variable-p 'superchat-authority-mode))
            (makunbound 'superchat-authority-mode)
            (should-not (boundp 'superchat-authority-mode)))
          (cl-letf
              (((symbol-function
                 'superchat--response-region--prompt-line-position)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--insert-prompt) #'ignore)
               ((symbol-function
                 'superchat--response-region-insert-transcript)
                #'ignore)
               ((symbol-function 'superchat--response-region-close) #'ignore)
               ((symbol-function 'superchat--update-status) #'ignore)
               ((symbol-function 'superchat--track-active-run) #'ignore)
               ((symbol-function 'llm-make-chat-prompt)
                (lambda (text &rest args)
                  (apply #'test-llm--make-native-chat-prompt text args)))
               ((symbol-function 'llm-chat-streaming)
                (lambda (_provider _prompt _partial response _error
                                   &optional _multi-output)
                  (let* ((effects
                          '((:effect local-write
                                     :resource buffer
                                     :scope current)))
                         (authority
                          (superchat-authority-decide
                           "batch7n_unbound_write" nil 'attended
                           'default effects)))
                    (setq provider-buffer (current-buffer)
                          authority-decision
                          (superchat-authority--effective-decision
                           "batch7n_unbound_write" nil authority nil)
                          response-function response))
                  'workflow-unbound-mode-provider-request)))
            (with-current-buffer target
              (setq observed-error
                    (condition-case err
                        (progn
                          (let ((superchat-buffer-name target-name)
                                (superchat-lang "English"))
                            (setq handle
                                  (superchat-workflow-execute-async
                                   "unbound-mode" nil workflow-file)))
                          nil)
                      (error err)))))
          (should-not observed-error)
          (should handle)
          (should (eq target provider-buffer))
          (should (eq 'ask authority-decision))
          (should (functionp response-function))
          (with-current-buffer target
            (should-not (boundp 'superchat-authority-mode))
            (funcall response-function "unbound mode answer"))
          (should
           (eq 'completed
               (plist-get
                (superchat-runtime-status (plist-get handle :run-id))
                :status)))
          (should (buffer-live-p target)))
      (when (buffer-live-p target)
        (with-current-buffer target
          (setq-local superchat-authority-mode saved-mode)
          (unless saved-mode-local-p
            (kill-local-variable 'superchat-authority-mode)))
        (kill-buffer target))
      (test-llm--cleanup-workflow-fixture fixture))))

 (ert-deftest test-workflow-foreign-caller-respects-unbound-view-mode ()
  "The public workflow entry checks mode existence in the captured View."
  (require 'superchat-workflow)
  (let* ((fixture (test-llm--make-workflow-fixture "foreign-unbound" t))
         (directory (plist-get fixture :directory))
         (workflow-file (plist-get fixture :workflow-file))
         (target-name (plist-get fixture :target-name))
         (target (plist-get fixture :target))
         (caller (plist-get fixture :caller))
         (superchat-workflow-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-runtime-default-deadline nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-terminal-guard-enabled nil)
         (saved-default (default-value 'superchat-authority-mode))
         result observed-error provider-buffer authority-decision status-timer)
    (unwind-protect
        (progn
          (set-default 'superchat-authority-mode 'execute)
          (with-temp-file workflow-file
            (insert "one valid step\n"))
          (with-current-buffer target
            (setq-local superchat-mode t)
            (setq-local superchat-buffer-name target-name)
            (setq-local superchat--session-id "workflow-foreign-unbound")
            (setq-local superchat-authority-mode 'execute)
            (makunbound 'superchat-authority-mode)
            (should-not (boundp 'superchat-authority-mode)))
          (with-current-buffer caller
            (kill-local-variable 'superchat-authority-mode)
            (should (boundp 'superchat-authority-mode)))
          (cl-letf
              (((symbol-function
                 'superchat--response-region--prompt-line-position)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--insert-prompt) #'ignore)
               ((symbol-function
                 'superchat--response-region-insert-transcript)
                #'ignore)
               ((symbol-function 'superchat--response-region-close) #'ignore)
               ((symbol-function 'superchat--update-status) #'ignore)
               ((symbol-function 'superchat--llm-generate-answer)
                (lambda (&rest _args)
                  (let* ((effects
                          '((:effect local-write
                                     :resource buffer
                                     :scope current)))
                         (authority
                          (superchat-authority-decide
                           "foreign_unbound_write" nil 'attended
                           'default effects)))
                    (setq provider-buffer (current-buffer)
                          authority-decision
                          (superchat-authority--effective-decision
                           "foreign_unbound_write" nil authority nil))
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status failed :response "fixture stop"))))))
            (with-current-buffer caller
              (setq observed-error
                    (condition-case err
                        (let ((superchat-buffer-name target-name)
                              (superchat-lang "English"))
                          (setq result
                                (superchat-workflow-execute-async
                                 "foreign-unbound" nil workflow-file))
                          nil)
                      (error err)))))
          (should-not observed-error)
          (should result)
          (should (eq target provider-buffer))
          (should (eq 'ask authority-decision))
          (with-current-buffer target
            (should
             (equal (plist-get result :run-id) superchat--active-run-id))
            (setq status-timer superchat--status-refresh-timer)
            (should (timerp status-timer)))
          (should
           (eq 'failed
               (plist-get
                (superchat-runtime-status (plist-get result :run-id))
                :status))))
      (set-default 'superchat-authority-mode saved-default)
      (test-llm--cleanup-workflow-fixture fixture)
      (when (timerp status-timer)
        (cancel-timer status-timer)))))

(ert-deftest test-workflow-run-step-respects-unbound-view-mode ()
  "The step runner checks mode existence in its View, not its caller."
  (require 'superchat-workflow)
  (let* ((fixture (test-llm--make-workflow-fixture "run-step-unbound" t))
         (target (plist-get fixture :target))
         (caller (plist-get fixture :caller))
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-runtime-default-deadline nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-closure-terminal-guard-enabled nil)
         (saved-default (default-value 'superchat-authority-mode))
         (steps (list (list :prompt "one valid step")))
         result observed-error provider-buffer authority-decision status-timer)
    (unwind-protect
        (progn
          (set-default 'superchat-authority-mode 'execute)
          (with-current-buffer target
            (setq-local superchat-mode t)
            (setq-local superchat--session-id "workflow-step-unbound")
            (setq-local superchat-authority-mode 'execute)
            (makunbound 'superchat-authority-mode)
            (should-not (boundp 'superchat-authority-mode)))
          (with-current-buffer caller
            (kill-local-variable 'superchat-authority-mode)
            (should (boundp 'superchat-authority-mode)))
          (cl-letf
              (((symbol-function
                 'superchat--response-region--prompt-line-position)
                (lambda (&rest _args) nil))
               ((symbol-function 'superchat--insert-prompt) #'ignore)
               ((symbol-function
                 'superchat--response-region-insert-transcript)
                #'ignore)
               ((symbol-function 'superchat--response-region-close) #'ignore)
               ((symbol-function 'superchat--update-status) #'ignore)
               ((symbol-function 'superchat--llm-generate-answer)
                (lambda (&rest _args)
                  (let* ((effects
                          '((:effect local-write
                                     :resource buffer
                                     :scope current)))
                         (authority
                          (superchat-authority-decide
                           "step_unbound_write" nil 'attended
                           'default effects)))
                    (setq provider-buffer (current-buffer)
                          authority-decision
                          (superchat-authority--effective-decision
                           "step_unbound_write" nil authority nil))
                    (funcall superchat--llm-runtime-observer
                             'terminal
                             '(:status failed :response "fixture stop"))))))
            (with-current-buffer caller
              (setq observed-error
                    (condition-case err
                        (progn
                          (setq result
                                (superchat-workflow--run-step
                                 0 steps
                                 '(:input "" :lang "English"
                                   :date "2026-09-02")
                                 nil target "workflow-step-unbound" nil))
                          nil)
                      (error err)))))
          (should-not observed-error)
          (should result)
          (should (eq target provider-buffer))
          (should (eq 'ask authority-decision))
          (with-current-buffer target
            (should
             (equal (plist-get result :run-id) superchat--active-run-id))
            (setq status-timer superchat--status-refresh-timer)
            (should (timerp status-timer)))
          (should
           (eq 'failed
               (plist-get
                (superchat-runtime-status (plist-get result :run-id))
                :status))))
      (set-default 'superchat-authority-mode saved-default)
      (test-llm--cleanup-workflow-fixture fixture)
      (when (timerp status-timer)
        (cancel-timer status-timer)))))

(ert-deftest test-runtime-quiescence-is-idempotent-and-preserves-borrowed-host ()
  "Quiescence severs exact-Run resources once and never closes its host."
  (let* ((superchat-runtime--runs (make-hash-table :test #'equal))
         (run-id "quiescence-borrowed-host")
         (host (generate-new-buffer " *superchat-quiescence-borrowed*"))
         (deadline (run-at-time 3600 nil #'ignore))
         (timer-owner (list :owner run-id))
         (deadline-cancels 0)
         (transport-cancels 0)
         (pause-clears 0)
         (real-cancel-timer (symbol-function 'cancel-timer))
         (entry (list :run-id run-id :status 'failed :buffer host
                      :buffer-name (buffer-name host)
                      :deadline-timer deadline
                      :transport-timer-owner timer-owner
                      :transport-request 'provider-request
                      :delivery-function #'ignore
                      :stream-function #'ignore)))
    (unwind-protect
        (progn
          (superchat-runtime--put entry)
          (cl-letf (((symbol-function 'cancel-timer)
                     (lambda (timer)
                       (when (eq timer deadline) (cl-incf deadline-cancels))
                       (funcall real-cancel-timer timer)))
                    ((symbol-function 'superchat-llm--transport-timer-owner-cancel)
                     (lambda (owner)
                       (should (eq owner timer-owner))
                       (cl-incf transport-cancels)))
                    ((symbol-function 'superchat--clear-pause-state)
                     (lambda (owner)
                       (should (equal owner run-id))
                       (cl-incf pause-clears))))
            (superchat-runtime--quiesce entry)
            (superchat-runtime--quiesce (superchat-runtime--entry run-id)))
          (let ((current (superchat-runtime--entry run-id)))
            (should (= 1 deadline-cancels))
            (should (= 1 transport-cancels))
            (should (= 1 pause-clears))
            (should (plist-get current :quiesced))
            (dolist (key '(:deadline-timer :transport-timer-owner
                           :transport-request :delivery-function
                           :stream-function))
              (should-not (plist-get current key)))
            (should (eq host (plist-get current :buffer)))
            (should (buffer-live-p host))))
      (when (timerp deadline) (funcall real-cancel-timer deadline))
      (when (buffer-live-p host) (kill-buffer host)))))

(ert-deftest test-runtime-quiescence-isolates-adapter-errors-before-pause-clear ()
  "Timer adapter errors do not gate registry severing or pause clearing."
  (let* ((superchat-runtime--runs (make-hash-table :test #'equal))
         (run-id "quiescence-adapter-errors")
         (host (generate-new-buffer " *superchat-quiescence-errors*"))
         (deadline (run-at-time 3600 nil #'ignore))
         (timer-owner (list :owner run-id))
         (pause-clears 0)
         (registry-severed nil)
         (real-cancel-timer (symbol-function 'cancel-timer))
         (entry (list :run-id run-id :status 'failed :buffer host
                      :deadline-timer deadline
                      :transport-timer-owner timer-owner
                      :transport-request 'provider-request
                      :delivery-function #'ignore
                      :stream-function #'ignore)))
    (unwind-protect
        (progn
          (superchat-runtime--put entry)
          (cl-letf (((symbol-function 'cancel-timer)
                     (lambda (_timer)
                       (let ((current (superchat-runtime--entry run-id)))
                         (setq registry-severed
                               (and (plist-get current :quiesced)
                                    (null (plist-get current :deadline-timer))
                                    (null (plist-get current :transport-request)))))
                       (error "deadline cancel failed")))
                    ((symbol-function 'superchat-llm--transport-timer-owner-cancel)
                     (lambda (_owner) (error "transport cancel failed")))
                    ((symbol-function 'superchat--clear-pause-state)
                     (lambda (owner)
                       (should (equal owner run-id))
                       (cl-incf pause-clears))))
            (superchat-runtime--quiesce entry))
          (should registry-severed)
          (should (= 1 pause-clears))
          (should (buffer-live-p host)))
      (when (timerp deadline) (funcall real-cancel-timer deadline))
      (when (buffer-live-p host) (kill-buffer host)))))

(ert-deftest test-runtime-overlapping-tool-round-controls-remain-run-local ()
  "Delayed interleaved tool rounds retain each Request's controls and context."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-a (format "p4_run_a_%x" (random #xfffffff)))
            (tool-b (format "p4_run_b_%x" (random #xfffffff)))
            (buffer-a (generate-new-buffer " *p4-run-a*"))
            (buffer-b (generate-new-buffer " *p4-run-b*"))
            (context-a (list :source "context-a" :facts '("fact-a")))
            (context-b (list :source "context-b" :facts '("fact-b")))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil) (superchat-show-ttft nil)
            (superchat-llm-max-tool-rounds 1)
            (superchat-agent-confirm-destructive nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0) (calls-a 0) (calls-b 0)
            (round-a 0) (round-b 0) events
            wrapped-a wrapped-b response-a response-b followup-a followup-b
            handle-a handle-b seen-a seen-b)
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-a :description "P4 Run A tool" :args nil
              :effects (list (list :effect 'local-read :resource "p4-a"))
              :function (lambda (&rest _args)
                          (cl-incf calls-a)
                          (setq seen-a
                                (copy-tree superchat--tool-execution-context t))
                          "effect-a"))
             (superchat-register-tool
              tool-b :description "P4 Run B tool" :args nil
              :effects (list (list :effect 'local-read :resource "p4-b"))
              :function (lambda (&rest _args)
                          (cl-incf calls-b)
                          (setq seen-b
                                (copy-tree superchat--tool-execution-context t))
                          "effect-b"))
             (cl-letf
                 (((symbol-function 'superchat-tape-append-event)
                   (lambda (event)
                     (push (copy-tree event t) events)
                     (cl-incf next-event-id)))
                  ((symbol-function 'display-warning) #'ignore)
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (_provider prompt _partial response _error
                            &optional _multi-output)
                     (let ((text (llm-chat-prompt-interaction-content
                                  (car (llm-chat-prompt-interactions prompt))))
                           (tools (llm-chat-prompt-tools prompt)))
                       (cond
                        ((equal text "request-a")
                         (if response-a (setq followup-a response)
                           (setq response-a response wrapped-a (car tools))))
                        ((equal text "request-b")
                         (if response-b (setq followup-b response)
                           (setq response-b response wrapped-b (car tools))))
                        (t (ert-fail (format "Unexpected prompt: %S" text)))))
                     'p4-provider-request)))
               (unwind-protect
                   (progn
                     (setq handle-a
                           (superchat-runtime-test-submit
                            (list :prompt "request-a" :input "request-a"
                                  :session-id "p4-session-a" :buffer buffer-a
                                  :tools (list tool-a) :context-envelope context-a
                                  :approval-function (lambda (_request) t)
                                  :round-limit-function
                                  (lambda () (cl-incf round-a) 'continue)
                                  :unknown-tool-retries 0 :deadline nil)))
                     (setq handle-b
                           (superchat-runtime-test-submit
                            (list :prompt "request-b" :input "request-b"
                                  :session-id "p4-session-b" :buffer buffer-b
                                  :tools (list tool-b) :context-envelope context-b
                                  :approval-function (lambda (_request) t)
                                  :round-limit-function
                                  (lambda () (cl-incf round-b) 'finalize)
                                  :unknown-tool-retries 2 :deadline nil)))
                     (setf (plist-get context-a :source) "mutated-a"
                           (plist-get context-b :source) "mutated-b")
                     (should (equal tool-a (llm-tool-name wrapped-a)))
                     (should (equal tool-b (llm-tool-name wrapped-b)))
                     (should-not (eq (llm-tool-function wrapped-a)
                                     (llm-tool-function wrapped-b)))
                     ;; Both submissions have returned: invoke B then A through the
                     ;; real Authority wrappers, then interleave their tool rounds.
                     (let ((result-b (funcall (llm-tool-function wrapped-b)))
                           (result-a (funcall (llm-tool-function wrapped-a))))
                       (should (equal "effect-b" result-b))
                       (should (equal "effect-a" result-a))
                       (funcall response-b (list (cons tool-b result-b)))
                       (funcall response-a (list (cons tool-a result-a))))
                     (should (equal '(1 1) (list round-a round-b)))
                     (should (functionp followup-a))
                     (should (functionp followup-b))
                     (should (equal "context-a"
                                    (plist-get (plist-get seen-a :context-envelope)
                                               :source)))
                     (should (equal '("fact-b")
                                    (plist-get (plist-get seen-b :context-envelope)
                                               :facts)))
                     (should (equal (plist-get handle-a :run-id)
                                    (plist-get seen-a :run-id)))
                     (should (equal (plist-get handle-b :run-id)
                                    (plist-get seen-b :run-id)))
                     (should (superchat-runtime-cancel
                              (plist-get handle-a :run-id) "cancel A"))
                     (funcall followup-a "late A")
                     (ignore-errors (funcall (llm-tool-function wrapped-a)))
                     (should (= 1 calls-a))
                     (should (eq 'running-model
                                 (plist-get (superchat-runtime-status
                                             (plist-get handle-b :run-id))
                                            :status)))
                     (funcall followup-b "final B")
                     (should (= 1 calls-b))
                     (should (eq 'cancelled
                                 (plist-get (superchat-runtime-status
                                             (plist-get handle-a :run-id))
                                            :status)))
                     (should (eq 'completed
                                 (plist-get (superchat-runtime-status
                                             (plist-get handle-b :run-id))
                                            :status)))
                     (let ((run-a (plist-get handle-a :run-id))
                           (run-b (plist-get handle-b :run-id)))
                       (should (= 1 (cl-count-if
                                     (lambda (event)
                                       (and (equal run-a (plist-get event :run-id))
                                            (equal "run.cancelled"
                                                   (plist-get event :type))))
                                     events)))
                       (should (= 1 (cl-count-if
                                     (lambda (event)
                                       (and (equal run-b (plist-get event :run-id))
                                            (equal "run.completed"
                                                   (plist-get event :type))))
                                     events)))))
                 ;; Include an accepted Run even if submission unwinds before
                 ;; its returned handle is assigned.  Keep the local Tape seam
                 ;; installed for this fixture's cancellation as well.
                 (maphash (lambda (run-id _entry)
                            (superchat-runtime-cancel run-id "P4 cleanup"))
                          superchat-runtime--runs))))
         (superchat-unregister-tool tool-a)
         (superchat-unregister-tool tool-b)
         (dolist (buffer (list buffer-a buffer-b))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest test-runtime-overlapping-protocol-retries-remain-run-local ()
  "Delayed protocol errors consume only their originating Run retry budget."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((buffer-a (generate-new-buffer " *p4-retry-a*"))
            (buffer-b (generate-new-buffer " *p4-retry-b*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil) (superchat-show-ttft nil)
            (superchat-llm-tools-enabled nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-terminal-guard-enabled nil)
            (superchat-closure-shadow-enabled nil)
            (next-event-id 0) (calls-a 0) (calls-b 0)
            error-a error-b retry-response-b handle-a handle-b)
       (unwind-protect
           (cl-letf
               (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) (cl-incf next-event-id)))
                ((symbol-function 'superchat--llm-drop-dangling-tool-use)
                 #'ignore)
                ((symbol-function 'llm-chat-prompt-append-response) #'ignore)
                ((symbol-function 'llm-chat-streaming)
                 (lambda (_provider prompt _partial response error
                          &optional _multi-output)
                   (pcase (llm-chat-prompt-interaction-content
                           (car (llm-chat-prompt-interactions prompt)))
                     ("retry-a" (cl-incf calls-a) (setq error-a error))
                     ("retry-b" (cl-incf calls-b)
                      (if (= calls-b 1) (setq error-b error)
                        (setq retry-response-b response))))
                   'p4-retry-request)))
             (setq handle-a
                   (superchat-runtime-test-submit
                    (list :prompt "retry-a" :buffer buffer-a
                          :session-id "p4-retry-session-a"
                          :unknown-tool-retries 0 :deadline nil)))
             (setq handle-b
                   (superchat-runtime-test-submit
                    (list :prompt "retry-b" :buffer buffer-b
                          :session-id "p4-retry-session-b"
                          :unknown-tool-retries 1 :deadline nil)))
             (funcall error-a 'llm-tool-missing-argument "missing A")
             (funcall error-b 'llm-tool-missing-argument "missing B")
             (should (= 1 calls-a))
             (should (= 2 calls-b))
             (should (functionp retry-response-b))
             (should (eq 'failed
                         (plist-get (superchat-runtime-status
                                     (plist-get handle-a :run-id))
                                    :status)))
             (should (eq 'running-model
                         (plist-get (superchat-runtime-status
                                     (plist-get handle-b :run-id))
                                    :status)))
             (funcall retry-response-b "B recovered")
             (should (eq 'completed
                         (plist-get (superchat-runtime-status
                                     (plist-get handle-b :run-id))
                                    :status))))
         (dolist (handle (list handle-a handle-b))
           (when handle
             (ignore-errors
               (superchat-runtime-cancel (plist-get handle :run-id)
                                         "P4 retry cleanup"))))
         (dolist (buffer (list buffer-a buffer-b))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest test-ordinary-raw-tools-require-callable-wrapper-before-provider ()
  "Raw ordinary tools fail closed before any provider call or tool effect."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-name (format "p3_raw_%x" (random #xfffffff)))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled 'always)
            (provider-calls 0)
            (tool-effects 0))
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-name :description "P3 admission tool" :args nil
              :effects (list (list :effect 'local-read :resource "p3"))
              :function (lambda (&rest _args) (cl-incf tool-effects) "effect"))
             (dolist (producer '(absent non-callable))
               (cl-letf (((symbol-function 'superchat--agent-make-wrapper)
                          (if (eq producer 'absent)
                              nil
                            (lambda (_preset) 'not-callable)))
                         ((symbol-function 'llm-chat-streaming)
                          (lambda (&rest _args) (cl-incf provider-calls))))
                 (should-error
                  (superchat--llm-generate-answer
                   "raw ordinary" #'ignore #'ignore nil nil
                   (list tool-name))
                  :type 'superchat-llm-tool-wrapper-required)))
             (should (= 0 provider-calls))
             (should (= 0 tool-effects)))
         (superchat-unregister-tool tool-name))))))

(ert-deftest test-fresh-headless-llm-rejects-tools-before-provider-effects ()
  "A bare LLM load rejects ordinary tools without loading Agent Loop."
  (let ((emacs (or (executable-find invocation-name)
                   (executable-find "emacs")))
        (root (file-name-directory (or (locate-library "superchat-llm")
                                       (expand-file-name "../superchat-llm.el"
                                                         (file-name-directory
                                                          (or load-file-name
                                                              buffer-file-name))))))
        (script (make-temp-file "superchat-llm-wrapper-admission-" nil ".el")))
    (unwind-protect
        (progn
          (unless emacs
            (ert-skip "Cannot find the current Emacs executable"))
          (with-temp-file script
            (insert
             "(progn\n"
             "  (require 'cl-lib)\n"
             "  (require 'superchat-llm)\n"
             "  (when (or (featurep 'superchat-agent-loop)\n"
             "            (fboundp 'superchat--agent-make-wrapper))\n"
             "    (kill-emacs 2))\n"
             "  (let ((superchat-llm-backend 'offline-provider)\n"
             "        (superchat-response-timeout nil)\n"
             "        (superchat-show-ttft nil)\n"
             "        (superchat-llm-tools-enabled 'always)\n"
             "        (prompt-constructions 0)\n"
             "        (provider-constructions 0)\n"
             "        (provider-invocations 0)\n"
             "        (tool (list 'mock-tool :name \"fresh-tool\"\n"
             "                    :description \"fresh\" :args nil\n"
             "                    :function #'ignore :async nil))\n"
             ;; S09-fix-1: an explicit `:tools' name is now filtered to
             ;; registered descriptors (contract §4.2 rule 1) before it can
             ;; reach the wrapper-required gate below, so this bare/fresh
             ;; child -- which loads only `superchat-llm', never
             ;; `superchat-authority' -- must register "fresh-tool" as one
             ;; itself; a real Authority load is exactly what this test
             ;; deliberately avoids requiring.
             "        (superchat-authority-effect-resolvers\n"
             "         (let ((h (make-hash-table :test #'equal)))\n"
             "           (puthash \"fresh-tool\" #'ignore h) h))\n"
             "        caught)\n"
             "    (cl-letf (((symbol-function 'superchat-get-llm-tools)\n"
             "               (lambda (&optional _names) (list tool)))\n"
             "              ((symbol-function 'llm-tool-name)\n"
             "               (lambda (value) (plist-get (cdr value) :name)))\n"
             "              ((symbol-function 'llm-tool-description)\n"
             "               (lambda (value)\n"
             "                 (plist-get (cdr value) :description)))\n"
             "              ((symbol-function 'llm-tool-args)\n"
             "               (lambda (value) (plist-get (cdr value) :args)))\n"
             "              ((symbol-function 'llm-tool-function)\n"
             "               (lambda (value)\n"
             "                 (plist-get (cdr value) :function)))\n"
             "              ((symbol-function 'llm-tool-async)\n"
             "               (lambda (value) (plist-get (cdr value) :async)))\n"
             "              ((symbol-function 'llm-make-chat-prompt)\n"
             "               (lambda (&rest _args)\n"
             "                 (cl-incf prompt-constructions) 'prompt))\n"
             "              ((symbol-function 'llm-provider-chat-request)\n"
             "               (lambda (&rest _args)\n"
             "                 (cl-incf provider-constructions) 'request))\n"
             "              ((symbol-function 'llm-chat-streaming)\n"
             "               (lambda (&rest _args)\n"
             "                 (cl-incf provider-invocations))))\n"
             "      (condition-case err\n"
             "          (superchat--llm-generate-answer\n"
             "           \"fresh raw\" #'ignore #'ignore nil nil '(\"fresh-tool\"))\n"
             "        (superchat-llm-tool-wrapper-required\n"
             "         (setq caught err)))\n"
             "      (unless (and (eq (car-safe caught)\n"
             "                       'superchat-llm-tool-wrapper-required)\n"
             "                   (= prompt-constructions 0)\n"
             "                   (= provider-constructions 0)\n"
             "                   (= provider-invocations 0))\n"
             "        (princ (format \"caught=%S prompt=%S constructed=%S invoked=%S\\n\"\n"
             "                       caught prompt-constructions\n"
             "                       provider-constructions provider-invocations))\n"
             "        (kill-emacs 3))))\n"
             "  (princ \"FRESH_HEADLESS_REJECT_OK\\n\"))\n"))
          (with-temp-buffer
            (let ((status (call-process emacs nil t nil
                                        "-Q" "--batch" "-L" root "-l" script)))
              (unless (= 0 status)
                (ert-fail (format "Fresh child exited %s:\n%s"
                                  status (buffer-string))))
              (should (string-match-p "FRESH_HEADLESS_REJECT_OK"
                                      (buffer-string))))))
      (when (file-exists-p script)
        (delete-file script)))))

(ert-deftest test-ordinary-request-local-wrapper-remains-authority-backed ()
  "A trusted request-local wrapper is applied once and still reaches Authority."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-name (format "p3_wrapped_%x" (random #xfffffff)))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled 'always)
            (superchat-agent-confirm-destructive nil)
            (superchat-authority-approval-function (lambda (_request) t))
            (superchat--tool-execution-context
             '(:session-id "p3-wrapper-session" :actor-id "lead"
               :attendance attended :authority-policy default))
            (superchat--llm-tool-wrapper
             (superchat--agent-make-wrapper nil))
            (provider-calls 0)
            (tool-effects 0)
            provider-tool)
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-name :description "P3 wrapped tool" :args nil
              :effects (list (list :effect 'local-read :resource "p3"))
              :function (lambda (&rest _args) (cl-incf tool-effects) "effect"))
             (cl-letf (((symbol-function 'superchat-tape-append-event)
                        (lambda (_event) 1))
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (_provider prompt _partial _response _error
                                 &optional _multi-output)
                          (cl-incf provider-calls)
                          (setq provider-tool
                                (car (llm-chat-prompt-tools prompt)))
                          'p3-provider-request)))
               (superchat--llm-generate-answer
                "wrapped ordinary" #'ignore #'ignore nil nil (list tool-name))
               (should (= 1 provider-calls))
               (should (equal tool-name (llm-tool-name provider-tool)))
               (should (equal "effect"
                              (funcall (llm-tool-function provider-tool))))
               (should (= 1 tool-effects))))
         (superchat-unregister-tool tool-name))))))

(ert-deftest test-prepared-tools-preserve-object-and-function-identity ()
  "Trusted prepared tools reach the provider unchanged and are not rewrapped."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-function (lambda (&rest _args) "prepared effect"))
            (tool (test-llm--make-tool
                   :name "prepared_identity" :description "Prepared identity"
                   :args nil :function tool-function))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat--llm-tools-prepared-p t)
            (superchat--llm-prepared-tools (list tool))
            (request-wrapper-calls 0)
            (agent-wrapper-calls 0)
            provider-tool)
       (let ((superchat--llm-tool-wrapper
              (lambda (_value) (cl-incf request-wrapper-calls))))
         (cl-letf (((symbol-function 'superchat--agent-make-wrapper)
                    (lambda (_preset)
                      (cl-incf agent-wrapper-calls)
                      #'identity))
                   ((symbol-function 'llm-chat-streaming)
                    (lambda (_provider prompt _partial _response _error
                             &optional _multi-output)
                      (setq provider-tool
                            (car (llm-chat-prompt-tools prompt)))
                      'prepared-provider-request)))
           (should (eq 'prepared-provider-request
                       (superchat--llm-generate-answer
                        "prepared" #'ignore #'ignore)))
           (should (eq tool provider-tool))
           (should (eq tool-function (llm-tool-function provider-tool)))
           (should (= 0 request-wrapper-calls))
           (should (= 0 agent-wrapper-calls))))))))

(ert-deftest test-runtime-ordinary-wrapper-rejection-finishes-once-and-quiesces ()
  "Runtime records one failed terminal when LLM rejects raw ordinary tools."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((tool-name (format "p3_runtime_%x" (random #xfffffff)))
            (session-id (format "p3-runtime-%x" (random #xfffffff)))
            (host (generate-new-buffer " *p3-runtime-admission*"))
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-llm-backend (test-llm--make-mock-backend 'openai))
            (superchat-response-timeout nil)
            (superchat-show-ttft nil)
            (superchat-llm-tools-enabled 'always)
            (superchat-runtime-default-deadline nil)
            (superchat-pre-topic-finalized-functions nil)
            (superchat-post-turn-functions nil)
            (superchat-post-topic-finalized-functions nil)
            (superchat-run-completed-functions nil)
            (superchat-turn-completed-functions nil)
            (superchat-closure-shadow-enabled nil)
            (provider-calls 0)
            (tool-effects 0)
            (deliveries nil))
       (unwind-protect
           (progn
             (superchat-register-tool
              tool-name :description "P3 Runtime tool" :args nil
              :effects (list (list :effect 'local-read :resource "p3"))
              :function (lambda (&rest _args) (cl-incf tool-effects) "effect"))
             (cl-letf (((symbol-function 'superchat--agent-make-wrapper) nil)
                       ((symbol-function 'llm-chat-streaming)
                        (lambda (&rest _args) (cl-incf provider-calls))))
               (let (caught)
                 (condition-case err
                     (superchat-runtime-test-submit
                      (list :prompt "raw Runtime tool" :input "raw Runtime tool"
                            :session-id session-id :buffer host
                            :tools (list tool-name) :deadline nil
                            :delivery-function
                            (lambda (value status tape-id)
                              (push (list value status tape-id) deliveries))))
                   (superchat-llm-tool-wrapper-required
                    (setq caught err)))
                 (should (eq 'superchat-llm-tool-wrapper-required
                             (car-safe caught))))
             (should (= 1 (hash-table-count superchat-runtime--runs)))
             (let ((entry (car (hash-table-values superchat-runtime--runs))))
               (should (eq 'failed (plist-get entry :status)))
               (should (plist-get entry :quiesced))
               (should (= 1 (length deliveries)))
               (should (eq 'failed (nth 1 (car deliveries))))
               (should (= 1 (cl-count-if
                             (lambda (event)
                               (equal "run.failed" (plist-get event :type)))
                             (superchat-tape-replay-events session-id)))))
             (should (= 0 provider-calls))
             (should (= 0 tool-effects))))
         (superchat-unregister-tool tool-name)
         (when (buffer-live-p host) (kill-buffer host)))))))

(cl-defun test-output-p1--real-loop
    (&key async plist-final empty-final (narration t) (rounds 1) partial custom-owner target interleave
          context-status-fault warning-fault)
  "Exercise P1 with real Runtime, llm tools and durable Closure evidence.
Only the model streaming transport is replaced.  TARGET is rename or dead."
  (unless (and (featurep 'llm) (require 'llm-openai nil t)
               (require 'llm-provider-utils nil t) (sqlite-available-p))
    (ert-skip "P1 requires installed llm and SQLite"))
  (let* ((directory (make-temp-file "superchat-output-p1-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-editor-workspace--states (make-hash-table :test #'equal))
         (superchat-tools--external-registry
          (copy-tree superchat-tools--external-registry))
         (superchat-authority-effect-resolvers
          (copy-hash-table superchat-authority-effect-resolvers))
         (superchat-llm-tools-list superchat-llm-tools-list)
         (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (superchat-llm-backend (make-llm-openai :key "unused" :chat-model "p1"))
         (superchat-response-timeout nil)
         (superchat-show-ttft nil)
         (superchat-show-ttft-breakdown nil)
         (superchat-agent-confirm-destructive nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-buffer-name (generate-new-buffer-name " *output-p1*"))
         (original-name superchat-buffer-name)
         (session "output-p1-session")
         (tool-name "output_p1_read")
         (expected-final (if empty-final "[Empty response from llm]" "P1-FINAL"))
         (calls 0) (effects 0) (context-warnings 0)
         (wrap-function (symbol-function 'superchat--agent-wrap-function))
         (execute-function (symbol-function 'superchat-tool-execute))
         (warning-function (symbol-function 'display-warning))
         recorded-callback pending final-callback view replacement handle)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "tape.db" directory))
          (setq view (superchat--ensure-chat-view original-name session))
          (superchat-register-tool
           tool-name :description "P1 deterministic local read" :args nil
           :async async
           :effects '((:effect local-read :resource "p1-fixture"))
           :function (if async
                         (lambda (done)
                           (cl-incf effects)
                           (setq pending done))
                       (lambda () (cl-incf effects) "P1-TOOL-RESULT")))
          (cl-letf (((symbol-function 'superchat-tool-execute)
                     (lambda (&rest args)
                       ;; Observe the actual callback; admission/evidence still
                       ;; runs through the unchanged Authority implementation.
                       (setq recorded-callback
                             (plist-get (plist-get (nthcdr 3 args) :context)
                                        :evidence-action-recorded-function))
                       (apply execute-function args)))
                    ((symbol-function 'superchat--agent-wrap-function)
                     (lambda (&rest args)
                       (if context-status-fault
                           (cl-letf (((symbol-function 'superchat-runtime-status)
                                      (lambda (_) (signal context-status-fault nil))))
                             (apply wrap-function args))
                         (apply wrap-function args))))
                    ((symbol-function 'display-warning)
                     (lambda (type message &rest args)
                       (if (and context-status-fault
                                (string-match-p "unresolved Runtime context" message))
                           (progn
                             (cl-incf context-warnings)
                             (if (eq warning-fault 'reentry)
                                 (progn (should (functionp recorded-callback))
                                        (funcall recorded-callback)
                                        (signal 'quit nil))
                               (when warning-fault (signal warning-fault nil))))
                         (apply warning-function type message args))))
                    ((symbol-function 'llm-chat-streaming)
                     (lambda (provider prompt part response error-cb
                              &optional multi-output)
                       (cl-incf calls)
                       (if (<= calls rounds)
                           (progn
                             (when narration
                               (funcall part (list :text (format "P1-NARRATION-%s" calls))))
                             ;; Use llm's actual tool scheduler and the actual
                             ;; Agent Loop wrapper installed on this prompt.
                             (llm-provider-utils-execute-tool-uses
                              provider prompt
                              (list (make-llm-provider-utils-tool-use
                                     :id (format "p1-call-%s" calls)
                                     :name tool-name :args nil))
                              multi-output
                              (and narration
                                   (list :text (format "P1-NARRATION-%s" calls)))
                              response error-cb))
                         (setq final-callback response)
                         (when partial (funcall part '(:text "P1-PARTIAL"))))
                       nil)))
            (with-current-buffer view
              (setq-local superchat-authority-mode 'auto
                          superchat-authority--session-allowlist (list tool-name))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "P1-QUESTION"))
              (superchat--response-region-consume-prompt)
              (superchat--prepare-for-response)
              (setq handle
                    (superchat--submit-chat-request
                     (append
                      (list :prompt "P1-QUESTION" :input "P1-QUESTION"
                            :session-id session :buffer view :deadline nil
                            :tools (list tool-name))
                      (when custom-owner
                        (list :transcript-owner "p1-custom-owner"))))))
            (when async
              (when interleave
                (with-current-buffer view
                  (superchat--response-region-begin "P2-B")
                  (superchat--response-region-stream "P2-B-PARTIAL" "P2-B")))
              (dotimes (_ rounds)
                (should (functionp pending))
                (let ((done pending))
                  (setq pending nil)
                  (let ((superchat--runtime-transcript-owner
                         (and interleave "P2-B")))
                    (funcall done "P1-TOOL-RESULT"))))
              (when interleave
                (with-current-buffer view
                  (let ((state (cdr (assoc "P2-B" superchat--response-region-states))))
                    (should (equal '("P2-B-PARTIAL") (plist-get state :parts)))
                    (should (superchat--response-region--state-active-p state))
                    (should (equal "P2-B" superchat--response-region-current-owner)))
                  (superchat--response-region-close "P2-B-FINAL" t "P2-B"))))
            (should (= effects rounds))
            (when context-status-fault (should (= 1 context-warnings)))
            (should (= calls (1+ rounds)))
            (should (functionp final-callback))
            (when target
              (with-current-buffer view
                (superchat--cancel-status-refresh-timer)
                (if (eq target 'rename)
                    (rename-buffer (generate-new-buffer-name " *output-p1-renamed*"))
                  (kill-buffer view)))
              (setq replacement (get-buffer-create original-name))
              (with-current-buffer replacement (insert "P1-FOREIGN")))
            (with-temp-buffer
              (funcall final-callback
                       (let ((text (if empty-final "" "P1-FINAL")))
                         (if plist-final (list :text text) text)))
              ;; A duplicate transport completion must not duplicate delivery.
              (funcall final-callback "P1-DUPLICATE"))
            (let* ((run-id (plist-get handle :run-id))
                   (status (superchat-runtime-status run-id))
                   (events (superchat-tape-replay-events session)))
              (should (eq 'completed (plist-get status :status)))
              (should (eq 'proved
                          (plist-get
                           (superchat-closure-judge
                            (list :kind 'durable-run-terminal :run-id run-id)
                            events) :verdict)))
              (when custom-owner
                (should (equal "p1-custom-owner" (plist-get status :transcript-owner))))
              (should (= rounds (cl-count "tool.requested" events
                                          :key (lambda (e) (plist-get e :type)) :test #'equal)))
              (should (= 1 (cl-count "run.completed" events
                                     :key (lambda (e) (plist-get e :type)) :test #'equal)))
              (should (string-match-p (regexp-quote expected-final) (prin1-to-string
                                                  (superchat-db-tape-replay session))))
              (should-not (string-match-p "P1-DUPLICATE" (prin1-to-string events))))
            (when replacement
              (with-current-buffer replacement
                (should (equal "P1-FOREIGN" (buffer-string)))))
            (unless (eq target 'dead)
              (with-current-buffer view
                (let ((text (buffer-string)))
                  (should (string-match-p (regexp-quote expected-final)
                                          (prin1-to-string superchat--conversation-history)))
                  (should (= 1 (test-render--match-count (regexp-quote expected-final) text)))
                  ;; S21: a tool round never opens a second `** Assistant:'
                  ;; heading for the SAME owner's turn -- exactly one covers
                  ;; the whole turn regardless of how many rounds ran.  The
                  ;; interleaved "P2-B" owner below is a genuinely separate,
                  ;; concurrent transcript region with its own heading.
                  (should (= (+ 1 (if interleave 1 0))
                             (test-render--match-count "^\\*\\* Assistant" text)))
                  (should-not (string-match-p "Prefill / waiting first token" text))
                  (should-not
                   (cl-loop for entry in superchat--response-region-states
                            thereis (superchat--response-region--state-marker-position
                                     (cdr entry) :status-marker)))
                  (should-not
                   (cl-loop for position from (point-min) below (point-max)
                            for face = (get-text-property position 'face)
                            thereis (or (eq face 'superchat-streaming-pending)
                                        (and (listp face)
                                             (memq 'superchat-streaming-pending face)))))
                  (should-not (string-match-p "P1-PARTIAL\\|P1-DUPLICATE" text))
                  (if context-status-fault
                      (progn
                        (should-not (string-match-p "Tool call\\|Tool result\\|P1-TOOL-RESULT" text)))
                    (should (= rounds (test-render--match-count "P1-TOOL-RESULT" text))))
                  (when narration
                    (dotimes (round rounds)
                      (should (= 1 (test-render--match-count
                                    (format "P1-NARRATION-%s" (1+ round)) text)))))
                  (unless context-status-fault
                    (should (< (string-match "P1-TOOL-RESULT" text)
                               (string-match (regexp-quote expected-final) text)))))))))
      (maphash (lambda (run-id _entry)
                 (superchat-runtime-cancel run-id "P1 test cleanup"))
               superchat-runtime--runs)
      (superchat-unregister-tool tool-name)
      (dolist (buffer (list view replacement))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (superchat--cancel-status-refresh-timer))
          (kill-buffer buffer)))
      (superchat-db-close)
      (delete-directory directory t))))

(ert-deftest output-p1/real-sync-string-final ()
  (test-output-p1--real-loop))

(ert-deftest output-context/real-evidence-loop-with-diagnostic-faults ()
  "Actual evidence-action-recorded presentation gates do not stop tool completion."
  (dolist (async '(nil t))
    (dolist (fault '(error quit reentry))
      (test-output-p1--real-loop :async async :custom-owner t
                                :interleave async :context-status-fault 'error
                                :warning-fault fault))))

(ert-deftest output-p2/real-round-end-captures-owner ()
  (test-output-p1--real-loop :async t :custom-owner t :interleave t))
(ert-deftest output-p1/real-async-plist-final ()
  (test-output-p1--real-loop :async t :plist-final t))
(ert-deftest output-p1/real-sync-plist-no-narration ()
  (test-output-p1--real-loop :plist-final t :narration nil))
(ert-deftest output-p1/real-async-string-no-narration ()
  (test-output-p1--real-loop :async t :narration nil))
(ert-deftest output-p1/real-multiple-rounds-custom-owner ()
  (test-output-p1--real-loop :async t :rounds 3 :custom-owner t))
(ert-deftest output-p1/real-partial-control ()
  (test-output-p1--real-loop :partial t :custom-owner t))
(ert-deftest output-p1/real-renamed-exact-buffer ()
  (test-output-p1--real-loop :target 'rename :custom-owner t))
(ert-deftest output-p1/real-dead-exact-buffer ()
  (test-output-p1--real-loop :target 'dead :async t))

(ert-deftest output-p1/real-empty-final-after-tool ()
  "The real LLM loop substitutes empty final before default Render delivery."
  (test-output-p1--real-loop :empty-final t :async t :custom-owner t))

(defun test-lmstudio--event (request delta)
  "Feed synthetic DELTA through REQUEST's real SSE parser and event timers."
  (let ((media (cdr (plist-get request :media-type))))
    (unless (plist-get request :source)
      (let ((buffer (generate-new-buffer " *lmstudio-sse*")))
        (setf (plist-get request :source)
              (plz-event-source-open
               (plz-event-source-buffer :buffer (buffer-name buffer)
                                        :handlers (oref media events))))))
    (let ((bytes (concat "data: " (json-serialize `(:choices [(:delta ,delta)])) "\n\n")))
      ;; Split the SSE field across transport deliveries, while JSON argument
      ;; strings (including Unicode) are independently split across events.
      ;; This does not claim arbitrary split-UTF-8-byte support upstream.
      (dolist (chunk (list (substring bytes 0 3) (substring bytes 3)))
        (plz-event-source--insert
         (plist-get request :source)
         (plz-media-type-decode-coding-string media chunk))))
    ;; Native SSE handlers dispatch through real one-shot Emacs timers.
    (sleep-for 0.01)))

(defun test-lmstudio--run (case)
  "Replay native compatible SSE through actual Runtime and Authority."
  (unless (and (featurep 'llm) (require 'llm-openai nil t)
               (require 'llm-provider-utils nil t) (sqlite-available-p))
    (ert-skip "Native compatible SSE requires installed llm and SQLite"))
  (let* ((directory (make-temp-file "lmstudio-native-" t))
         (superchat-data-directory directory)
         (superchat-db--connection nil) (superchat-db--path nil)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-terminal-guard-enabled t)
         (superchat-closure-shadow-enabled nil)
         (superchat-llm-backend
          (make-llm-openai-compatible :url "http://invalid.test"
                                      :chat-model (if (eq case 'unsupported)
                                                      "unknown-synthetic-model"
                                                    "qwen/qwen3.8-27b")))
         (superchat-response-timeout nil)
         (superchat-llm-tools-enabled 'always)
         (superchat-agent-confirm-destructive nil)
         (superchat-show-ttft nil)
         (superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
         (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
         (superchat-llm-tools-list superchat-llm-tools-list)
         (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
         (host (generate-new-buffer " *lmstudio-native-host*"))
         (tool "lmstudio_probe_read")
         requests parts delivered effects handle caught pending)
    (unwind-protect
        (progn
          (superchat-db-open (expand-file-name "tape.db" directory))
          (superchat-register-tool
           tool :description "Synthetic read" :args '((:name "value" :type string :description "value"))
           :effects '((:effect local-read :resource "lmstudio-fixture"))
           :async (eq case 'async)
           :function (if (eq case 'async)
                         (lambda (done value) (push value effects) (setq pending done))
                       (lambda (value) (push value effects) "RESULT")))
          (with-current-buffer host
            (setq-local superchat-authority-mode 'auto
                        superchat-authority--session-allowlist (list tool)))
          (cl-letf (((symbol-function 'llm-request-plz-async)
                     (lambda (_url &rest args)
                       (push (append args (list :source nil)) requests) nil)))
		   (setq handle
			 (superchat-runtime-test-submit
			  (list :prompt "SYNTHETIC" :input "SYNTHETIC"
				:session-id "lmstudio-probe" :buffer host :deadline nil
				:tools (list tool) :transcript-owner "opaque-lmstudio-owner"
				:stream-function (lambda (part &rest _) (push part parts))
				:delivery-function (lambda (&rest args) (push args delivered)))))
		   (let ((request (car requests)))
		     (message "NATIVE selection=%S" (car (plist-get request :media-type)))
		     (when (eq case 'unsupported)
                       ;; S08: the adapter rule grants both capabilities to
                       ;; any model on the exact `llm-openai-compatible'
                       ;; type, so an unrecognized model name no longer
                       ;; falls back to non-streaming.
                       (should (plist-get request :media-type)))
		     (unless (eq case 'unsupported)
		       (should (plist-get request :media-type))
		       (should-not (memq 'streaming-tool-use (llm-capabilities superchat-llm-backend)))
		       (test-lmstudio--event request '(:content "BEFORE"))
		       (should (string-match-p "BEFORE" (prin1-to-string parts)))
		       (should (eq 'running-model (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)))
		       (should-not delivered)
		       (test-lmstudio--event request '(:reasoning_content "THOUGHT"))
		       (test-lmstudio--event
			request `(:tool_calls [(:index 0 :id "call-a" :type "function"
						       :function (:name ,(if (eq case 'missing-name) "" tool)
									:arguments ,(if (eq case 'missing-argument) "{}" "{\"value\":")))]))
		       (should-not effects)
		       (when (eq case 'multiple)
			 (test-lmstudio--event request `(:content " BETWEEN" :tool_calls
								  [(:index 1 :id "call-b" :type "function" :function (:name ,tool :arguments "{\"value\":\"二"))])))
		       (unless (memq case '(malformed missing-argument))
			 (test-lmstudio--event request '(:tool_calls [(:index 0 :function (:arguments "\"中🙂\"}"))])))
		       (when (eq case 'multiple)
			 (test-lmstudio--event request '(:tool_calls [(:index 1 :function (:arguments "号\"}"))])))
		       (should-not effects)
		       (when (eq case 'cancel)
			 (superchat-runtime-cancel (plist-get handle :run-id) "probe cancel"))
		       (condition-case err
			   (funcall (plist-get request :on-success) nil)
			 (error (setq caught err)))
		       (when (eq case 'async)
			 (should (functionp pending))
			 (should (= 1 (length requests)))
			 (funcall pending "RESULT"))
		       (message "NATIVE case=%S effects=%S requests=%d status=%S condition=%S"
				case effects (length requests)
				(plist-get (superchat-runtime-status (plist-get handle :run-id)) :status) caught)
		       (if (memq case '(malformed cancel missing-argument missing-name))
			   (progn
			     (should-not effects)
			     (pcase case
			       ('malformed (should (eq 'llm-tool-call-error (car caught))))
			       ((or 'missing-name 'missing-argument)
				(should (= 2 (length requests)))
				(should-not caught)))
			     (should-not (seq-find (lambda (event) (equal "tool.requested" (plist-get event :type)))
						   (superchat-tape-replay-events "lmstudio-probe")))
			     (when (eq case 'cancel)
			       (let ((before-parts (copy-tree parts))
				     (before-tape (superchat-tape-replay-events "lmstudio-probe")))
				 (test-lmstudio--event request '(:content "LATE CANCELLED"))
				 (funcall (plist-get request :on-success) nil)
				 (should (equal parts before-parts))
				 (should (equal before-tape (superchat-tape-replay-events "lmstudio-probe")))
				 (should (eq 'proved (plist-get
						      (superchat-closure-judge
						       (list :kind 'durable-run-terminal
							     :run-id (plist-get handle :run-id)) before-tape)
						      :verdict)))
				 (should (= 1 (length delivered)))
				 (should (= 1 (length requests))))))
			 (should (equal effects (if (eq case 'multiple) '("二号" "中🙂") '("中🙂"))))
			 (should (= 2 (length requests)))
			 (test-lmstudio--event (car requests) '(:content "AFTER"))
			 (should (string-match-p "AFTER" (prin1-to-string parts)))
			 (funcall (plist-get (car requests) :on-success) nil)
			 (should (= 1 (length delivered)))
			 (let* ((id (plist-get handle :run-id))
				(events (superchat-tape-replay-events "lmstudio-probe")))
			   (should (eq 'completed (plist-get (superchat-runtime-status id) :status)))
			   (should (equal "opaque-lmstudio-owner" (plist-get (superchat-runtime-status id) :transcript-owner)))
			   (should (eq 'proved (plist-get (superchat-closure-judge
							   (list :kind 'durable-run-terminal :run-id id) events) :verdict)))
			   (dolist (type '("tool.requested" "tool.completed"))
			     (should (= (length effects) (cl-count type events :key (lambda (event) (plist-get event :type)) :test #'equal)))))
			 ;; Late text/final must not create another delivery or Run.
			 (test-lmstudio--event (car requests) '(:content " LATE"))
			 (funcall (plist-get (car requests) :on-success) nil)
			 (should (= 1 (length delivered)))
			 (should (= 1 (hash-table-count superchat-runtime--runs))))))))
      (maphash (lambda (id _) (superchat-runtime-cancel id "probe cleanup")) superchat-runtime--runs)
      (superchat-unregister-tool tool)
      (dolist (request requests)
        (when-let* ((source (plist-get request :source)))
          (plz-event-source-close source)
          (kill-buffer (oref source buffer))))
      (when (buffer-live-p host) (kill-buffer host))
      (superchat-db-close)
      (delete-directory directory t))))
(ert-deftest lmstudio/native-normal () (test-lmstudio--run 'normal))
(ert-deftest lmstudio/native-async-tool () (test-lmstudio--run 'async))
(ert-deftest lmstudio/native-malformed () (test-lmstudio--run 'malformed))
(ert-deftest lmstudio/native-cancel () (test-lmstudio--run 'cancel))
(ert-deftest lmstudio/native-interleaved-tools () (test-lmstudio--run 'multiple))
(ert-deftest lmstudio/native-missing-argument () (test-lmstudio--run 'missing-argument))
(ert-deftest lmstudio/native-missing-name () (test-lmstudio--run 'missing-name))
(ert-deftest lmstudio/native-unsupported-model-fallback () (test-lmstudio--run 'unsupported))

;;; Actual-attempt input batch admission (native parsers, transport fixtures).

(defun test-trusted-ask--config (&optional eligibility)
  "Stage1 explicit adapter fixture; no public UI producer is enabled."
  (list :enabled t :operation 'ask-user :tool-name "ask_user"
        :eligible-function (or eligibility (lambda () t))
        :presenter (lambda (snapshot) (push snapshot test-input-batch--questions))))

(defun test-trusted-ask--submit (&optional tools eligibility policy deadline)
  (superchat-runtime-test-submit
   (list :prompt "Required city" :session-id "trusted-ask" :buffer test-input-batch--host
         :surface 'chat :tools (or tools '("ask_user")) :deadline deadline :authority-policy policy
         :input-core (test-trusted-ask--config eligibility)
         :stream-function #'ignore :delivery-function #'ignore)))

(defun test-trusted-ask--reply (request &optional question)
  "Use native JSON/SSE parsing, never call the tool function directly."
  (if (plist-get request :media-type)
      (let* ((delta `(:tool_calls [(:index 0 :id ,(format "trusted-call-%d" (length test-input-batch--requests))
                                  :type "function" :function
                                  (:name "ask_user" :arguments
                                         ,(json-serialize `(:question ,(or question "Which city?")))))]))
             (expected (decode-coding-string (json-serialize `(:choices [(:delta ,delta)])) 'utf-8))
             (media (cdr (plist-get request :media-type)))
             (source (plist-get request :source)) completed)
        ;; This fixture declares packet delivery before transport success.
        ;; Observe the exact native message handler RETURN for this connection
        ;; and packet; a fixed sleep is not evidence that its timer ran.
        (should-not source)
        (setq source
              (plz-event-source-open
               (plz-event-source-buffer
                :buffer (buffer-name (generate-new-buffer " *trusted-ask-sse*"))
                :handlers
                (mapcar
                 (lambda (pair)
                   (if (not (eq (car pair) 'message)) pair
                     (cons (car pair)
                           (lambda (event)
                             (prog1 (funcall (cdr pair) event)
                               (when (and (eq source (plist-get request :source))
                                          (equal expected (oref event data)))
                                 (setq completed source)))))))
                 (oref media events)))))
        (setf (plist-get request :source) source)
        (test-lmstudio--event request delta)
        (let ((bound (+ (float-time) 2)))
          (while (and (not completed) (< (float-time) bound)) (sleep-for 0.01)))
        (should (eq source completed))
        (funcall (plist-get request :on-success) nil))
    (funcall
     (plist-get request :on-success)
     `((choices . [((message . ((role . "assistant")
                               (tool_calls . [((id . ,(format "trusted-call-%d" (length test-input-batch--requests))) (type . "function")
                                               (function . ((name . "ask_user")
                                                            (arguments . ,(json-serialize
                                                                           `(:question ,(or question "Which city?")))))))])))
                    (finish_reason . "tool_calls"))])))))

(ert-deftest trusted-ask/native-real-factory-no-approval-and-closure ()
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let* ((approvals 0)
            (superchat-authority-approval-function (lambda (_) (cl-incf approvals) nil)))
        (with-current-buffer test-input-batch--host
          (setq-local superchat-authority-mode 'explore
                      superchat-authority--session-allowlist nil))
        (let* ((handle (test-trusted-ask--submit)) (run (plist-get handle :run-id)))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let ((pending (superchat-runtime-pending-input run)))
            (should (equal "Which city?" (plist-get pending :question)))
            (should (= approvals 0))
            (should (= 1 (length test-input-batch--requests)))
            (should-not (superchat-runtime-answer-input run (plist-get pending :handle) "  "))
            (should (superchat-runtime-answer-input run (plist-get pending :handle) "Paris")))
          (should (= 2 (length test-input-batch--requests)))
          (test-input-batch--reply (car test-input-batch--requests) nil)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (let* ((events (superchat-tape-replay-events "trusted-ask"))
                 (requested (cl-find "tool.requested" events :test #'equal
                                     :key (lambda (e) (plist-get e :type))))
                 (payload (plist-get requested :payload)))
            (should (equal "allow" (plist-get payload :decision)))
            (should (equal "human-input-v1:allow" (plist-get payload :disposition)))
            (should (equal (plist-get payload :request-hash)
                           (superchat-authority-approval-hash
                            "ask_user" '("Which city?")
                            (list (list :effect 'human-interaction :resource 'human-input
                                        :scope (list :session-id "trusted-ask" :run-id run)))
                            (plist-get payload :policy-version) (plist-get payload :execution-id)
                            "human-input-v1:allow")))
            (should (string-match-p "human-interaction" (prin1-to-string (plist-get payload :effects))))
            (should (= 1 (cl-count "tool.completed" events :test #'equal
                                  :key (lambda (e) (plist-get e :type)))))
            (should-not (cl-find "approval.requested" events :test #'equal
                                 :key (lambda (e) (plist-get e :type))))
            (should (eq 'proved (plist-get (superchat-closure-judge
                                           (list :kind 'durable-run-terminal :run-id run) events)
                                          :verdict)))))))))

(ert-deftest trusted-ask/unsupported-adapter-no-native ()
  (test-input-batch--fixture 'async
    (should-error (test-trusted-ask--submit nil (lambda () nil)))
    (should-not test-input-batch--requests)
    (should (= 0 (hash-table-count superchat-runtime--runs)))))

(defun test-trusted-ask--descriptor-substitution (field)
    (test-input-batch--fixture 'async
      (let ((factory (symbol-function 'superchat-tools--make-ask-user)))
        (cl-letf (((symbol-function 'superchat-tools--make-ask-user)
                   (lambda (operation)
                     (let ((tool (funcall factory operation)))
                       (pcase field
                         ('function (setf (llm-tool-function tool) #'ignore))
                         ('async (setf (llm-tool-async tool) nil))
                         ('args (setf (llm-tool-args tool) nil)))
                       tool))))
          (should-error (test-trusted-ask--submit))
          (should-not test-input-batch--requests)))))

(ert-deftest trusted-ask/descriptor-function-before-native ()
  (test-trusted-ask--descriptor-substitution 'function))
(ert-deftest trusted-ask/descriptor-async-before-native ()
  (test-trusted-ask--descriptor-substitution 'async))
(ert-deftest trusted-ask/descriptor-schema-before-native ()
  (test-trusted-ask--descriptor-substitution 'args))

(defun test-trusted-ask--schema-string-mutation (field)
  "Mutate actual factory FIELD without rebinding its canonical schema."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let* ((factory (symbol-function 'superchat-tools--make-ask-user))
             (canonical (plist-get (car superchat-tools--ask-user-args) field))
             (expected (copy-sequence canonical))
             (first-character (aref canonical 0))
             (hits 0))
        (unwind-protect
            (progn
              (cl-letf (((symbol-function 'superchat-tools--make-ask-user)
                         (lambda (operation)
                           (let ((tool (funcall factory operation)))
                             (cl-incf hits)
                             (aset (plist-get (car (llm-tool-args tool)) field) 0 ?X)
                             tool))))
                (condition-case nil (test-trusted-ask--submit) (error nil)))
              (should (= hits 1))
              (should-not test-input-batch--requests)
              (should (equal canonical expected)))
          ;; Restore only potential RED-run damage, after the assertions;
          ;; never replace the canonical object or mask the native0 oracle.
          (unless (equal canonical expected)
            (aset canonical 0 first-character)))))))

(ert-deftest trusted-ask/descriptor-name-string-mutation-before-native ()
  (test-trusted-ask--schema-string-mutation :name))

(ert-deftest trusted-ask/descriptor-description-string-mutation-before-native ()
  (test-trusted-ask--schema-string-mutation :description))

(ert-deftest trusted-ask/profile-selection-matrix ()
  (dolist (row '((nil nil nil) (nil always t) (nil on-demand t)
                 (none always nil) (("input_batch_effect") nil nil)
                 (("ask_user") nil t) (("ask_user") always t)))
    (test-input-batch--fixture 'async
      (let* ((superchat-llm-tools-enabled (nth 1 row))
             (handle (superchat-runtime-test-submit
                      (list :prompt "hello" :session-id "trusted-ask" :buffer test-input-batch--host
                            :tools (car row) :deadline nil :surface 'chat
                            :input-core (test-trusted-ask--config)
                            :stream-function #'ignore :delivery-function #'ignore)))
             (entry (superchat-runtime--entry (plist-get handle :run-id)))
             (prompt (plist-get (car test-input-batch--requests) :prompt))
             (names (mapcar #'llm-tool-name (llm-chat-prompt-tools prompt))))
        (should (eq (not (null (member "ask_user" names))) (nth 2 row)))
        (should (eq (not (null (plist-get entry :input-core))) (nth 2 row)))
        (should (eq (not (null (plist-get entry :input-core-token))) (nth 2 row)))))))

(ert-deftest trusted-ask/native-provenance-spoofs-denied-before-policy ()
  (dolist (mode '(async streaming))
    (dolist (fault '(function token run async effects))
      (test-input-batch--fixture mode
        (let* ((approvals 0) (policy-calls 0)
               (superchat-authority-approval-function (lambda (_) (cl-incf approvals) t))
               (execute (symbol-function 'superchat-tool-execute))
               (policy (symbol-function 'superchat-authority--human-input-decision))
               (handle (test-trusted-ask--submit)) (run (plist-get handle :run-id)))
          (cl-letf (((symbol-function 'superchat-authority--human-input-decision)
                     (lambda (&rest args) (cl-incf policy-calls) (apply policy args)))
                    ((symbol-function 'superchat-tool-execute)
                     (lambda (name function args &rest options)
                       (when (equal name "ask_user")
                         (let ((context (copy-sequence (plist-get options :context))))
                           (pcase fault
                             ('function (setq function (lambda (&rest _) (error "SPOOF EXECUTED"))))
                             ('token (setq context (plist-put context :input-core-token (make-symbol "forged"))))
                             ('run (setq context (plist-put context :run-id "not-the-accepted-run")))
                             ('async (setq options (plist-put options :async nil)))
                             ('effects (setq context (plist-put context :resolve-effects
                                                               (lambda (_) '((:effect local-write :resource file)))))))
                           (setq options (plist-put options :context context))))
                       (apply execute name function args options))))
            (test-trusted-ask--reply (car test-input-batch--requests)))
          (should-not test-input-batch--questions)
          (should (= 0 approvals))
          (should (= 0 policy-calls))
          (should-not (superchat-runtime-pending-input run))
          ;; Wrong Run is rejected by the existing inactive-run gate before
          ;; audit admission.  Other provenance faults use ordinary denial audit.
          (unless (eq fault 'run)
            (let ((events (superchat-tape-replay-events "trusted-ask")))
              (should (= 1 (cl-count "tool.failed" events :test #'equal
                                    :key (lambda (e) (plist-get e :type)))))
              (should-not (cl-find "tool.execution.started" events :test #'equal
                                   :key (lambda (e) (plist-get e :type)))))))))))

(ert-deftest trusted-ask/native-tightening-and-downstream-approval ()
  (dolist (mode '(async streaming))
    (dolist (restriction '(deny ask downstream))
      (test-input-batch--fixture mode
        (let* ((approvals 0)
              (superchat-authority-approval-function (lambda (_) (cl-incf approvals) nil))
              (superchat-authority--policy-registry (copy-hash-table superchat-authority--policy-registry)))
          (superchat-authority-register-policy
           'trusted-restrict
           (lambda (effects _context)
             (if (eq restriction 'downstream)
                 (if (eq 'human-interaction (plist-get (car effects) :effect)) 'defer 'ask)
               restriction)))
          (with-current-buffer test-input-batch--host
            (setq-local superchat-authority-mode 'auto superchat-authority--session-allowlist nil))
          (superchat-authority-register-effect-resolver
           "input_batch_effect" (lambda (_) '((:effect local-write :resource buffer :scope "fixture"))))
          (let* ((handle (test-trusted-ask--submit '("ask_user" "input_batch_effect") nil
                                                 'trusted-restrict))
                 (run (plist-get handle :run-id)))
            (test-trusted-ask--reply (car test-input-batch--requests))
            (should (= approvals 0))
            (if (eq restriction 'downstream)
                (let ((pending (superchat-runtime-pending-input run)))
                  (should pending)
                  (superchat-runtime-answer-input run (plist-get pending :handle) "Paris")
                  (test-input-batch--reply (car test-input-batch--requests) '("input_batch_effect") "write")
                  (should (= approvals 1))
                  (should-not test-input-batch--effects))
              (should-not test-input-batch--questions)
              (should-not (superchat-runtime-pending-input run)))
            (test-input-batch--reply (car test-input-batch--requests) nil)
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))))))

(ert-deftest trusted-ask/native-eligibility-reentry-before-invoke ()
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let* ((eligible t)
             (append-event (symbol-function 'superchat-tape-append-event-record))
             (handle (test-trusted-ask--submit nil (lambda () eligible)))
             (run (plist-get handle :run-id)))
        (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                   (lambda (event)
                     (prog1 (funcall append-event event)
                       (when (equal "tool.execution.started" (plist-get event :type))
                         (setq eligible nil))))))
          (test-trusted-ask--reply (car test-input-batch--requests)))
        (should-not test-input-batch--questions)
        (should-not (superchat-runtime-pending-input run))
        (should (= 1 (length test-input-batch--requests)))
        (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
        (let ((events (superchat-tape-replay-events "trusted-ask")))
          (should (= 1 (cl-count "tool.failed" events :test #'equal
                                :key (lambda (e) (plist-get e :type)))))
          (should (eq 'proved (plist-get (superchat-closure-judge
                                         (list :kind 'durable-run-terminal :run-id run) events)
                                        :verdict))))))))

(ert-deftest trusted-ask/native-stop-expiry-late-answer ()
  "Actual descriptor settlement, not a Plan progression or UI integration proof."
  (dolist (mode '(async streaming))
    (dolist (end '(stop expiry))
      (test-input-batch--fixture mode
        (let* ((handle (test-trusted-ask--submit nil nil nil (when (eq end 'expiry) 0.5)))
               (run (plist-get handle :run-id)))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let ((pending (superchat-runtime-pending-input run)))
            (should pending)
            (if (eq end 'stop) (should (superchat-runtime-cancel run "Stop response"))
              (let ((bound (+ (float-time) 3)))
                (while (and (superchat-runtime-effect-capable-p run) (< (float-time) bound))
                  (sleep-for 0.01))))
            (should (eq (if (eq end 'stop) 'cancelled 'timed-out)
                        (plist-get (superchat-runtime-status run) :status)))
            (should-not (superchat-runtime-answer-input run (plist-get pending :handle) "late")))
          (should (= 1 (length test-input-batch--requests)))
          (let ((events (superchat-tape-replay-events "trusted-ask")))
            (should (= 1 (cl-count "tool.failed" events :test #'equal
                                  :key (lambda (e) (plist-get e :type)))))
            (should-not (cl-find "tool.completed" events :test #'equal
                                 :key (lambda (e) (plist-get e :type))))
            (should (eq 'proved (plist-get (superchat-closure-judge
                                           (list :kind 'durable-run-terminal :run-id run) events)
                                          :verdict)))))))))

(ert-deftest trusted-ask/adapter-failure-audit-priority ()
  (dolist (mode '(async streaming))
    (dolist (fault '(error quit commit-error commit-quit))
      (test-input-batch--fixture mode
        (let* ((eligible t) (started-hits 0) (failed-hits 0)
               (append-event (symbol-function 'superchat-tape-append-event-record))
               (handle (test-trusted-ask--submit nil (lambda () eligible)))
               (run (plist-get handle :run-id)))
          (cl-letf (((symbol-function 'superchat-tape-append-event-record)
                     (lambda (event)
                       (if (and (equal run (plist-get event :run-id))
                                (equal "tool.failed" (plist-get event :type)))
                           (progn
                             (cl-incf failed-hits)
                             (when (memq fault '(commit-error commit-quit)) (funcall append-event event))
                             (signal (if (memq fault '(quit commit-quit)) 'quit 'error)
                                     '("INPUT RECEIPT AUDIT")))
                         (prog1 (funcall append-event event)
                           (when (and (equal run (plist-get event :run-id))
                                      (equal "tool.execution.started" (plist-get event :type)))
                             (cl-incf started-hits)
                             (setq eligible nil)))))))
            (condition-case condition
                (test-trusted-ask--reply (car test-input-batch--requests))
              ((error quit) (should (equal (cdr condition) '("INPUT RECEIPT AUDIT"))))))
          (should (= 1 started-hits))
          (should (= 1 failed-hits))
          (should (eq 'audit-incomplete (plist-get (superchat-runtime-status run) :status)))
          (should-not test-input-batch--questions)
          (should (= 1 (length test-input-batch--requests)))
          (let ((events (superchat-tape-replay-events "trusted-ask")))
            (should (= (if (memq fault '(commit-error commit-quit)) 1 0)
                       (cl-count "tool.failed" events :test #'equal
                                 :key (lambda (e) (plist-get e :type)))))
            (should-not (cl-find "tool.completed" events :test #'equal
                                 :key (lambda (e) (plist-get e :type))))))))))

(ert-deftest trusted-ask/native-invalid-question-and-mixed-batch ()
  (dolist (mode '(async streaming))
    (dolist (kind '(empty nonstring mixed))
      (test-input-batch--fixture mode
        (let* ((handle (test-trusted-ask--submit '("ask_user" "input_batch_effect")))
               (run (plist-get handle :run-id)))
          (pcase kind
            ('mixed (test-input-batch--reply (car test-input-batch--requests)
                                            '("input_batch_effect" "ask_user")))
            ('empty (test-trusted-ask--reply (car test-input-batch--requests) "  "))
            ('nonstring (test-trusted-ask--reply (car test-input-batch--requests) 7)))
          (should-not test-input-batch--questions)
          (should-not test-input-batch--effects)
          (should-not (superchat-runtime-pending-input run))
          (when (eq kind 'mixed)
            (should (= 0 test-input-batch--authority-count))
            (should (= 1 (length test-input-batch--requests)))
            (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))))))))

(ert-deftest trusted-ask/native-recovery-retains-authentic-operation ()
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let ((superchat-llm-context-budget-exceeded-functions
             (list (lambda (details)
                     (funcall (plist-get details :resume-function) "NEW CONTEXT") t))))
        (let* ((handle (superchat-runtime-test-submit
                        (list :prompt "City" :context-envelope '(:prompt "City")
                              :session-id "trusted-ask" :buffer test-input-batch--host :tools '("ask_user")
                              :deadline nil :input-core (test-trusted-ask--config)
                              :stream-function #'ignore :delivery-function #'ignore)))
               (run (plist-get handle :run-id)) (first (car test-input-batch--requests)))
          (funcall (plist-get first :on-error) 'http-error "maximum context length exceeded")
          (should (= 2 (length test-input-batch--requests)))
          (should (equal "ask_user" (plist-get (car test-input-batch--requests) :policy)))
          (should-not (eq (plist-get first :policy) (plist-get (car test-input-batch--requests) :policy)))
          (test-trusted-ask--reply (car test-input-batch--requests))
          (let ((question (superchat-runtime-pending-input run)))
            (should question)
            (should (superchat-runtime-answer-input run (plist-get question :handle) "Paris")))
          (should (= 3 (length test-input-batch--requests)))
          (test-input-batch--reply (car test-input-batch--requests) nil)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))))

(ert-deftest trusted-ask/native-synchronous-q1-q2-keeps-current-question ()
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let* ((endpoint (symbol-function 'llm-request-plz-async))
             (config (test-trusted-ask--config)) first-question)
        (plist-put config :presenter
                   (lambda (question)
                     (if first-question (push question test-input-batch--questions)
                       (setq first-question question)
                       (superchat-runtime-answer-input
                        (plist-get question :run-id) (plist-get question :handle) "Paris"))))
        (cl-letf (((symbol-function 'llm-request-plz-async)
                   (lambda (&rest args)
                     (prog1 (apply endpoint args)
                       (when (= 2 (length test-input-batch--requests))
                         (test-trusted-ask--reply (car test-input-batch--requests) "Which date?"))))))
          (let* ((handle (superchat-runtime-test-submit
                          (list :prompt "City" :session-id "trusted-ask" :buffer test-input-batch--host
                                :tools '("ask_user") :deadline nil :input-core config
                                :stream-function #'ignore :delivery-function #'ignore)))
                 (run (plist-get handle :run-id)))
            (test-trusted-ask--reply (car test-input-batch--requests))
            (let ((q2 (superchat-runtime-pending-input run)))
              (should (equal "Which date?" (plist-get q2 :question)))
              (should-not (eq (plist-get first-question :handle) (plist-get q2 :handle)))
              (should-not (superchat-runtime-answer-input run (plist-get first-question :handle) "late"))
              (should (superchat-runtime-answer-input run (plist-get q2 :handle) "Tomorrow")))
            (should (= 3 (length test-input-batch--requests)))
            (test-input-batch--reply (car test-input-batch--requests) nil)
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))))))))
(defvar superchat--llm-input-batches)
(defvar test-input-batch--requests nil)
(defvar test-input-batch--questions nil)
(defvar test-input-batch--ordinary-pending nil)
(defvar test-input-batch--effects nil)
(defvar test-input-batch--authority-count 0)
(defvar test-input-batch--dispatch-prompt nil)
(defvar test-input-batch--host nil)
(defvar test-input-batch--delivered nil)

(defmacro test-input-batch--fixture (mode &rest body)
  "Run BODY with actual native parsers/Authority and isolated Tape."
  (declare (indent 1) (debug t))
  `(progn
     (unless (and (featurep 'llm) (require 'llm-openai nil t)
                  (require 'llm-provider-utils nil t) (sqlite-available-p))
       (ert-skip "Input admission requires installed native llm and SQLite"))
     (let* ((directory (make-temp-file "input-batch-" t))
            (superchat-data-directory directory)
            (superchat-db--connection nil) (superchat-db--path nil)
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat--llm-input-batches (make-hash-table :test #'eq :weakness 'key))
            (superchat--llm-raw-tool-uses (make-hash-table :test #'eq :weakness 'key))
            (superchat-closure-terminal-guard-enabled t) (superchat-closure-shadow-enabled nil)
            (superchat-llm-backend
             (make-llm-openai-compatible :url "http://invalid.test"
                                       :chat-model (if (eq ,mode 'streaming)
                                                       "qwen/qwen3.8-27b" "input-batch-async")))
            ;; S08: the adapter rule now grants both capabilities to any
            ;; model on the exact `llm-openai-compatible' type, so an
            ;; unrecognized model no longer forces the async path on its
            ;; own.  This fixture's non-streaming modes still need the
            ;; async transport, so declare it explicitly rather than rely
            ;; on the model name being unrecognized.
            (superchat-llm-model-capability-overrides
             (unless (eq ,mode 'streaming)
               (list (list "input-batch-async"
                           :streaming nil :streaming-tool-use nil))))
            (superchat-response-timeout nil) (superchat-runtime-default-deadline nil)
            (superchat-show-ttft nil) (superchat-llm-tools-enabled 'always)
            (superchat-tools--external-registry (copy-tree superchat-tools--external-registry))
            (superchat-authority-effect-resolvers (copy-hash-table superchat-authority-effect-resolvers))
            (superchat-llm-tools-list superchat-llm-tools-list)
            (superchat--llm-tool-names-signature superchat--llm-tool-names-signature)
            (test-input-batch--host (generate-new-buffer " *input-batch*"))
            (test-input-batch--requests nil) (test-input-batch--questions nil)
            (test-input-batch--ordinary-pending nil) (test-input-batch--effects nil)
            (test-input-batch--authority-count 0) (test-input-batch--delivered nil)
            (observe-dispatch
             (lambda (original provider prompt &rest args)
               ;; Observe the real generic invocation; registration is product
               ;; code, never this observer or a prompt-build substitute.
               (let ((test-input-batch--dispatch-prompt prompt))
                 (apply original provider prompt args))))
            (observe-authority
             (lambda (original &rest args)
               (cl-incf test-input-batch--authority-count) (apply original args))))
       (unwind-protect
           (progn
             (superchat-db-open (expand-file-name "tape.db" directory))
             (superchat-register-tool
              "input_batch_effect" :description "Fixture effect"
              :args '((:name "value" :type string :description "value"))
              :effects '((:effect local-read :resource "input-batch"))
              :function (lambda (value) (push value test-input-batch--effects) "EFFECT RESULT"))
             (superchat-register-tool
              "input_batch_ask" :description "Controlled input fixture"
              :args '((:name "value" :type string :description "question")) :async t
              :effects '((:effect local-read :resource "input-batch-question"))
              :function
              (lambda (done value)
                (if-let* ((capability (plist-get superchat--tool-execution-context :input-settlement)))
                    (superchat-runtime--request-input capability (list :question value))
                  (push done test-input-batch--ordinary-pending))))
             (with-current-buffer test-input-batch--host
               (setq-local superchat-authority-mode 'auto
                           superchat-authority--session-allowlist '("input_batch_effect" "input_batch_ask")))
             (advice-add 'llm-chat-streaming :around observe-dispatch)
             (advice-add 'llm-chat-async :around observe-dispatch)
             (advice-add 'superchat-tool-execute :around observe-authority)
             (cl-letf (((symbol-function 'llm-request-plz-async)
                        (lambda (url &rest args)
                          (apply #'superchat--llm-request-plz-propagate-callback-errors
                                 (lambda (_url &rest actual)
                                   (push (append actual
                                                 (list :source nil :prompt test-input-batch--dispatch-prompt
                                                       :policy (gethash test-input-batch--dispatch-prompt
                                                                        superchat--llm-input-batches)))
                                         test-input-batch--requests)
                                   nil)
                                 url args))))
               ,@body))
         (advice-remove 'llm-chat-streaming observe-dispatch)
         (advice-remove 'llm-chat-async observe-dispatch)
         (advice-remove 'superchat-tool-execute observe-authority)
         (maphash (lambda (id _) (superchat-runtime-cancel id "fixture cleanup")) superchat-runtime--runs)
         (dolist (request test-input-batch--requests)
           (when-let* ((source (plist-get request :source)))
             (plz-event-source-close source)
             (when (buffer-live-p (oref source buffer)) (kill-buffer (oref source buffer)))))
         (when (buffer-live-p test-input-batch--host) (kill-buffer test-input-batch--host))
         (superchat-db-close) (delete-directory directory t)))))

(defun test-input-batch--submit (session &optional opted extra)
  (superchat-runtime-test-submit
   (append (list :prompt "INPUT BATCH" :session-id session :buffer test-input-batch--host :deadline nil
                 :tools '("input_batch_effect" "input_batch_ask") :stream-function #'ignore
                 :delivery-function (lambda (&rest value) (push value test-input-batch--delivered))
                 :input-core (when opted
                               (list :enabled t :tool-name "input_batch_ask"
                                     :presenter (lambda (value) (push (cons session value) test-input-batch--questions)))))
           extra)))

(defun test-input-batch--reply (request names &optional value)
  "Deliver synthetic native SSE or parsed async JSON through REQUEST."
  (if (plist-get request :media-type)
      (let ((before test-input-batch--authority-count))
        (cl-loop for name in names for index from 0 do
                 (test-lmstudio--event
                  request `(:tool_calls [(:index ,index :id ,(format "call-%s-%s" (length test-input-batch--requests) index)
                                          :type "function" :function (:name ,name :arguments "{\"value\":"))]))
                 (test-lmstudio--event
                  request `(:tool_calls [(:index ,index :function
                                          (:arguments ,(concat (decode-coding-string (json-serialize (or value "STALE")) 'utf-8) "}")))]))
                 (should (= before test-input-batch--authority-count)))
        (unless names (test-lmstudio--event request '(:content "FINAL")))
        (funcall (plist-get request :on-success) nil))
    (let* ((calls (cl-loop for name in names for index from 0 collect
                           `((id . ,(format "call-%s-%s" (length test-input-batch--requests) index))
                             (type . "function")
                             (function . ((name . ,name) (arguments . ,(json-serialize `(:value ,(or value "STALE")))))))))
           (message (append '((role . "assistant"))
                            (if names (list (cons 'tool_calls (vconcat calls))) '((content . "FINAL")))))
           (data (list (cons 'choices (vector (list (cons 'message message)
                                                   (cons 'finish_reason (if names "tool_calls" "stop"))))))))
      (funcall (plist-get request :on-success) data))))

(defun test-input-batch--reject (mode names)
  (test-input-batch--fixture mode
    (let* ((handle (test-input-batch--submit "reject" t)) (run (plist-get handle :run-id)))
      (test-input-batch--reply (car test-input-batch--requests) names)
      (should (= 0 test-input-batch--authority-count))
      (should-not test-input-batch--effects)
      (should-not test-input-batch--questions)
      (should (= 1 (length test-input-batch--requests)))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (let ((events (superchat-tape-replay-events "reject")))
        (should-not (cl-find "tool.requested" events :key (lambda (e) (plist-get e :type)) :test #'equal))
        (should (eq 'proved (plist-get (superchat-closure-judge
                                       (list :kind 'durable-run-terminal :run-id run) events) :verdict)))))))

(ert-deftest input-batch/reject-stream-effect-first ()
  (test-input-batch--reject 'streaming '("input_batch_effect" "input_batch_ask")))
(ert-deftest input-batch/reject-stream-ask-first ()
  (test-input-batch--reject 'streaming '("input_batch_ask" "input_batch_effect")))
(ert-deftest input-batch/reject-stream-two-asks ()
  (test-input-batch--reject 'streaming '("input_batch_ask" "input_batch_ask")))
(ert-deftest input-batch/reject-async-effect-first ()
  (test-input-batch--reject 'async '("input_batch_effect" "input_batch_ask")))
(ert-deftest input-batch/reject-async-ask-first ()
  (test-input-batch--reject 'async '("input_batch_ask" "input_batch_effect")))
(ert-deftest input-batch/reject-async-two-asks ()
  (test-input-batch--reject 'async '("input_batch_ask" "input_batch_ask")))

(ert-deftest input-batch/singleton-and-ordinary-controls ()
  "Answers enter the actual next model prompt; ordinary batches keep their behavior."
  (dolist (mode '(async streaming))
    (dolist (case '(ask effect unopt-effect-first unopt-ask-first unopt-two-asks))
      (test-input-batch--fixture mode
        (let* ((opted (memq case '(ask effect)))
               (run (plist-get (test-input-batch--submit "control" opted) :run-id))
               (first (car test-input-batch--requests))
               (names (pcase case ('ask '("input_batch_ask")) ('effect '("input_batch_effect"))
                             ('unopt-effect-first '("input_batch_effect" "input_batch_ask"))
                             ('unopt-ask-first '("input_batch_ask" "input_batch_effect"))
                             (_ '("input_batch_ask" "input_batch_ask")))))
          (test-input-batch--reply first names)
          (should (= (length names) test-input-batch--authority-count))
          (if (eq case 'ask)
              (progn
                (should-not test-input-batch--effects)
                (should (= 1 (length test-input-batch--requests)))
                (should (superchat-runtime-pending-input run))
                (should (superchat-runtime-answer-input
                         run (plist-get (cdar test-input-batch--questions) :handle) "深圳"))
                (should (= 2 (length test-input-batch--requests)))
                (should (string-match-p "深圳" (prin1-to-string (plist-get (car test-input-batch--requests) :data))))
                ;; Only a NEW model generation supplies the new effect args.
                (test-input-batch--reply (car test-input-batch--requests) '("input_batch_effect") "深圳")
                (should (equal '("深圳") test-input-batch--effects)))
            (dolist (done test-input-batch--ordinary-pending) (funcall done "ordinary answer")))
          (should (>= (length test-input-batch--requests) 2))
          (test-input-batch--reply (car test-input-batch--requests) nil)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
          (dolist (request test-input-batch--requests)
            (should (equal (and opted "input_batch_ask") (plist-get request :policy)))
            (should (eq (plist-get first :prompt) (plist-get request :prompt))))
          (when opted
            (should-not (eq (plist-get first :policy) (plist-get (car test-input-batch--requests) :policy)))))))))

(ert-deftest input-batch/overlap-foreign-and-raw-capture ()
  "Overlapping A/B use exact prompts; foreign same-model/tools are not gated."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let* ((a (plist-get (test-input-batch--submit "A" t) :run-id))
             (ar (car test-input-batch--requests)) (raw (list nil))
             (b (plist-get (test-input-batch--submit "B" t) :run-id))
             (br (car test-input-batch--requests)) foreign-effects foreign-result)
        (should-not (eq (plist-get ar :prompt) (plist-get br :prompt)))
        (puthash (plist-get ar :prompt) raw superchat--llm-raw-tool-uses)
        (test-input-batch--reply ar '("input_batch_ask"))
        (should (= 1 (length (car raw))))
        (should (equal "input_batch_ask" (llm-provider-utils-tool-use-name (caar raw))))
        (should-not (gethash (plist-get ar :prompt) superchat--llm-raw-tool-uses))
        (should (gethash (plist-get ar :prompt) superchat--llm-input-batches))
        (test-input-batch--reply br '("input_batch_effect" "input_batch_ask"))
        (should (= 1 test-input-batch--authority-count))
        (should (eq 'failed (plist-get (superchat-runtime-status b) :status)))
        (let ((foreign (llm-make-chat-prompt
                        "FOREIGN" :tools
                        (mapcar (lambda (name)
                                  (llm-make-tool :name name :description "foreign"
                                                 :args '((:name "value" :type string :description "value"))
                                                 :function (lambda (value) (push (cons name value) foreign-effects) "OK")))
                                '("input_batch_effect" "input_batch_ask")))))
          (if (eq mode 'streaming)
              (llm-chat-streaming superchat-llm-backend foreign #'ignore
                                  (lambda (value) (setq foreign-result value)) #'ignore)
            (llm-chat-async superchat-llm-backend foreign (lambda (value) (setq foreign-result value)) #'ignore))
          (should-not (gethash foreign superchat--llm-input-batches))
          (test-input-batch--reply (car test-input-batch--requests) '("input_batch_effect" "input_batch_ask"))
          (should (= 2 (length foreign-effects)))
          (should foreign-result))
        (should (superchat-runtime-answer-input a (plist-get (cdar test-input-batch--questions) :handle) "深圳"))
        (test-input-batch--reply (car test-input-batch--requests) nil)
        (should (eq 'completed (plist-get (superchat-runtime-status a) :status)))))))

(ert-deftest input-batch/registration-rollback-and-weak-lifetime ()
  "Only an unentered registration rolls back; policy values cannot retain prompts."
  (unless (featurep 'llm) (ert-skip "Requires real prompt structs"))
  (let ((superchat--llm-input-batches (make-hash-table :test #'eq :weakness 'key)))
    (dolist (condition '(error quit))
      (dolist (prior '(nil "previous"))
        (dolist (replace '(nil t))
          (let* ((prompt (llm-make-chat-prompt "rollback"))
                 (put (symbol-function 'puthash)) once entered caught)
            (when prior (puthash prompt prior superchat--llm-input-batches))
            (cl-letf (((symbol-function 'puthash)
                       (lambda (key value table)
                         (prog1 (funcall put key value table)
                           (when (and (eq table superchat--llm-input-batches) (not once))
                             (setq once t)
                             (when replace (funcall put key "replacement" table))
                             (signal condition '(registration-boundary)))))))
              (condition-case err
                  (superchat--llm-dispatch-input-batch prompt "ask" (lambda () (setq entered t)))
                ((error quit) (setq caught err))))
            (should (equal caught (list condition 'registration-boundary)))
            (should-not entered)
            (should (equal (if replace "replacement" prior)
                           (gethash prompt superchat--llm-input-batches)))))))
    (clrhash superchat--llm-input-batches)
    (let ((held (llm-make-chat-prompt "held")))
      (let ((name (propertize "ask" 'fixture-backreference held)))
        (superchat--llm-dispatch-input-batch held name #'ignore)
        (should-not (text-properties-at 0 (gethash held superchat--llm-input-batches))))
      ;; The tool closure really points back to its prompt, like native tools.
      ;; The table's detached string must not keep that cycle alive.
      (let ((cycle (llm-make-chat-prompt "cycle")))
        (setf (llm-chat-prompt-tools cycle)
              (list (llm-make-tool :name "ask" :description "cycle" :args nil
                                   :function (let ((target cycle)) (lambda () target)))))
        (superchat--llm-dispatch-input-batch cycle "ask" #'ignore)
        (should (= 2 (hash-table-count superchat--llm-input-batches))))
      (garbage-collect)
      (should (= 1 (hash-table-count superchat--llm-input-batches)))
      (should (equal "ask" (gethash held superchat--llm-input-batches))))))

(ert-deftest input-batch/native-throw-and-terminal-late ()
  "Native entry may retain callbacks even on throw; terminal callbacks keep admission."
  (dolist (mode '(async streaming))
    (dolist (exit '(error quit cancel completed))
      (test-input-batch--fixture mode
        (let (run caught)
          (if (memq exit '(error quit))
              (let* ((endpoint (symbol-function 'llm-request-plz-async)))
                (cl-letf (((symbol-function 'llm-request-plz-async)
                           (lambda (&rest args)
                             (apply endpoint args)
                             (signal exit '(native-entered)))))
                  (condition-case err (test-input-batch--submit "late" t)
                    ((error quit) (setq caught err))))
                (setq run (car (hash-table-keys superchat-runtime--runs)))
                ;; Ordinary error is normalized by the existing LLM submit
                ;; boundary; quit retains its original condition.
                (when (eq exit 'quit) (should (equal '(quit native-entered) caught))))
            (setq run (plist-get (test-input-batch--submit "late" t) :run-id))
            (if (eq exit 'cancel) (should (superchat-runtime-cancel run "stop"))
              (test-input-batch--reply (car test-input-batch--requests) nil)))
          (should run)
          (let* ((request (car test-input-batch--requests))
                 (before (superchat-tape-replay-events "late")))
            (should (equal "input_batch_ask" (gethash (plist-get request :prompt) superchat--llm-input-batches)))
            (test-input-batch--reply request '("input_batch_effect" "input_batch_ask"))
            (should (= 0 test-input-batch--authority-count))
            (should-not test-input-batch--effects)
            (should (= 1 (length test-input-batch--requests)))
            (should (equal before (superchat-tape-replay-events "late")))))))))

(ert-deftest input-batch/recovery-registers-actual-prompt ()
  "Context recovery registers its actual dispatch, including ordinary prompt reuse."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let ((superchat-llm-context-budget-exceeded-functions
             (list (lambda (details)
                     (funcall (plist-get details :resume-function) "NEW CONTEXT") t))))
        (let* ((run (plist-get
                     (test-input-batch--submit
                      "recovery" t '(:context-envelope (:prompt "INPUT BATCH"))) :run-id))
               (first (car test-input-batch--requests)))
          (funcall (plist-get first :on-error) 'http-error "maximum context length exceeded")
          (should (= 2 (length test-input-batch--requests)))
          (let ((second (car test-input-batch--requests)))
            (should (equal "input_batch_ask" (plist-get second :policy)))
            (should-not (eq (plist-get first :policy) (plist-get second :policy)))
            (should (string-match-p "NEW CONTEXT" (prin1-to-string (plist-get second :data))))
            (test-input-batch--reply second '("input_batch_ask" "input_batch_effect"))
            (should (= 0 test-input-batch--authority-count))
            (should-not test-input-batch--effects)
            (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))))))))

(ert-deftest input-batch/evidence-copy-and-async-exclusion ()
  "Supported sync evidence copies register every attempt; async input remains excluded."
  (dolist (mode '(async streaming))
    (dolist (case '(effect mixed async-excluded))
      (test-input-batch--fixture mode
        (let* ((request
                  (list :prompt "EVIDENCE" :session-id "evidence" :buffer test-input-batch--host :deadline nil
                        ;; A configured name need not be exposed in the tool
                        ;; list.  This proves policy on SUPPORTED sync evidence
                        ;; attempts, not async human-input evidence support.
                        :tools (if (eq case 'async-excluded) '("input_batch_ask") '("input_batch_effect"))
                        :stream-function #'ignore :delivery-function #'ignore
                        :input-core (list :enabled t :tool-name "input_batch_ask" :presenter #'ignore)
                        :evidence-contract "epistemic-learning-v0"
                        :context-envelope '(:contract "context-continuity-v0" :status satisfied
                                            :receipt (:tape-head 1))))
               (run (if (eq case 'async-excluded)
                        (progn
                          (should-error (superchat-runtime-test-submit request)
                                        :type 'superchat-llm-evidence-recording-error)
                          (car (hash-table-keys superchat-runtime--runs)))
                      (plist-get (superchat-runtime-test-submit request) :run-id)))
               (first (car test-input-batch--requests)))
          (if (eq case 'async-excluded)
              (progn
                (should-not test-input-batch--requests)
                (should-not test-input-batch--effects)
                (should-not (eq 'completed (plist-get (superchat-runtime-status run) :status))))
            (should first)
            (should (equal "input_batch_ask" (plist-get first :policy)))
            (test-input-batch--reply first
                                     (if (eq case 'mixed) '("input_batch_effect" "input_batch_ask")
                                       '("input_batch_effect")))
            (if (eq case 'mixed)
                (progn (should (= 0 test-input-batch--authority-count))
                       (should-not test-input-batch--effects)
                       (should (= 1 (length test-input-batch--requests)))
                       (should (eq 'failed (plist-get (superchat-runtime-status run) :status))))
              (should (equal '("STALE") test-input-batch--effects))
              (should (= 2 (length test-input-batch--requests)))
              (let ((second (car test-input-batch--requests)))
                (should-not (eq (plist-get first :prompt) (plist-get second :prompt)))
                (should (equal "input_batch_ask" (plist-get second :policy)))
                (should-not (eq (plist-get first :policy) (plist-get second :policy)))
                (test-input-batch--reply second nil)
                (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
                (let ((events (superchat-tape-replay-events "evidence")))
                  (should (= 2 (cl-count "decision.snapshot.recorded" events
                                         :key (lambda (event) (plist-get event :type)) :test #'equal)))
                  (should (= 1 (cl-count "tool.completed" events
                                         :key (lambda (event) (plist-get event :type)) :test #'equal)))
                  (should (eq 'proved (plist-get (superchat-closure-judge
                                                 (list :kind 'durable-run-terminal :run-id run) events)
                                                :verdict))))))))))))

(ert-deftest input-batch/synchronous-answer-followup-dispatch ()
  "Synchronous transport/presenter reentry registers each nested native generation."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (let ((endpoint (symbol-function 'llm-request-plz-async)) answered)
        (cl-letf (((symbol-function 'llm-request-plz-async)
                   (lambda (&rest args)
                     (apply endpoint args)
                     (pcase (length test-input-batch--requests)
                       (1 (test-input-batch--reply (car test-input-batch--requests) '("input_batch_ask")))
                       (2 (test-input-batch--reply (car test-input-batch--requests) '("input_batch_effect") "FRESH"))
                       (3 (test-input-batch--reply (car test-input-batch--requests) nil)))
                     nil)))
          (let ((run
                 (plist-get
                  (superchat-runtime-test-submit
                   (list :prompt "SYNC" :session-id "sync" :buffer test-input-batch--host :deadline nil
                         :tools '("input_batch_effect" "input_batch_ask")
                         :stream-function #'ignore
                         :delivery-function (lambda (&rest value) (push value test-input-batch--delivered))
                         :input-core
                         (list :enabled t :tool-name "input_batch_ask"
                               :presenter
                               (lambda (question)
                                 (setq answered
                                       (superchat-runtime-answer-input
                                        (plist-get question :run-id) (plist-get question :handle) "FRESH"))))))
                  :run-id)))
            (should answered)
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
            (should (= 3 (length test-input-batch--requests)))
            (should (= 2 test-input-batch--authority-count))
            (should (equal '("FRESH") test-input-batch--effects))
            (should (= 1 (length test-input-batch--delivered)))
            (let ((policies (mapcar (lambda (r) (plist-get r :policy)) test-input-batch--requests)))
              (should (cl-every (lambda (p) (equal p "input_batch_ask")) policies))
              (should-not (eq (nth 0 policies) (nth 1 policies)))
              (should-not (eq (nth 1 policies) (nth 2 policies))))))))))

(ert-deftest input-batch/clock-prepare-precedes-native-entry ()
  "Stopped/throwing preparation rolls back; native-entered throws retain."
  (dolist (kind '(stop error quit replacement native-error))
    (let* ((superchat--llm-input-batches (make-hash-table :test #'eq :weakness 'key))
           (prompt (list 'prompt)) (previous (copy-sequence "previous"))
           (replacement (copy-sequence "replacement")) entered prepared observed)
      (puthash prompt previous superchat--llm-input-batches)
      (condition-case condition
          (superchat--llm-dispatch-input-batch
           prompt "ask"
           (lambda () (setq entered t) (error "native entered"))
           (lambda ()
             (setq prepared t)
             (pcase kind
               ('stop nil)
               ('replacement (puthash prompt replacement superchat--llm-input-batches) nil)
               ('native-error t)
               (_ (signal kind '("prepare original"))))))
        ((error quit) (setq observed condition)))
      (should prepared)
      (if (eq kind 'native-error)
          (progn (should entered)
                 (should (equal "ask" (gethash prompt superchat--llm-input-batches)))
                 (should (equal observed '(error "native entered"))))
        (should-not entered)
        (should (eq (if (eq kind 'replacement) replacement previous)
                    (gethash prompt superchat--llm-input-batches)))
        (when (memq kind '(error quit))
          (should (equal observed (list kind "prepare original"))))))))

(ert-deftest input-batch/clock-recovery-preserves-human-pause ()
  "Retained native overflow cannot turn human time into model silence."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (setq superchat-response-timeout 1 superchat-response-timeout-hard-limit 5)
      (let ((superchat-llm-context-budget-exceeded-functions
             (list (lambda (details)
                     (funcall (plist-get details :resume-function) "NEW CONTEXT") t))))
        (let* ((run (plist-get (test-input-batch--submit
                               "clock-recovery" t '(:context-envelope (:prompt "INPUT BATCH"))) :run-id))
               (request (car test-input-batch--requests))
               (owner (superchat-runtime-transport-timer-owner run)))
          (test-input-batch--reply request '("input_batch_ask"))
          (let ((question (superchat-runtime-pending-input run))
                (epoch (superchat-llm--transport-timer-owner-clock-epoch owner))
                (remaining (superchat-llm--transport-timer-owner-hard-remaining owner)))
            (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner)))
            (funcall (plist-get request :on-error) 'http-error "maximum context length exceeded")
            (should (= 1 (length test-input-batch--requests)))
            (should-not (superchat-llm--transport-timer-owner-wait-timer owner))
            (sleep-for 1.2)
            (should (equal question (superchat-runtime-pending-input run)))
            (should (eq epoch (superchat-llm--transport-timer-owner-clock-epoch owner)))
            (should (= remaining (superchat-llm--transport-timer-owner-hard-remaining owner)))
            (should (eq 'running-model (plist-get (superchat-runtime-status run) :status)))
            (should (superchat-runtime-answer-input
                     run (plist-get (cdar test-input-batch--questions) :handle) "City"))
            (should (= 2 (length test-input-batch--requests)))
            (should (eq 'running (superchat-llm--transport-timer-owner-hard-state owner)))
            (should (superchat-llm--transport-timer-owner-wait-timer owner))
            (should (<= (- (superchat-llm--transport-timer-owner-hard-deadline owner)
                           (float-time)) remaining))
            (test-input-batch--reply (car test-input-batch--requests) nil)
            (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
            (should-not (superchat-llm--transport-timer-owner-wait-timer owner))))))))

(ert-deftest input-batch/clock-recovery-defers-until-dispatch ()
  "The existing pause checkpoint defers opted-in recovery wait acquisition."
  (dolist (mode '(async streaming))
    (test-input-batch--fixture mode
      (setq superchat-response-timeout 1 superchat-response-timeout-hard-limit 5)
      (let ((superchat-llm-context-budget-exceeded-functions
             (list (lambda (details)
                     (funcall (plist-get details :resume-function) "NEW CONTEXT") t))))
        (let* ((run (plist-get (test-input-batch--submit
                               "deferred-recovery" t '(:context-envelope (:prompt "INPUT BATCH"))) :run-id))
               (owner (superchat-runtime-transport-timer-owner run))
               (deadline (superchat-llm--transport-timer-owner-hard-deadline owner))
               pending)
          (with-current-buffer test-input-batch--host
            (setq-local superchat--pause-requested t superchat--pause-run-id run)
            (cl-letf (((symbol-function 'superchat--pause-boundary) #'ignore))
              (funcall (plist-get (car test-input-batch--requests) :on-error)
                       'http-error "maximum context length exceeded"))
            (setq pending superchat--pending-send-request)
            (should pending)
            (should (= 1 (length test-input-batch--requests)))
            (should-not (superchat-llm--transport-timer-owner-wait-timer owner))
            (should (= deadline (superchat-llm--transport-timer-owner-hard-deadline owner)))
            (setq superchat--pause-requested nil)
            (funcall (superchat-llm--pending-pause-continuation pending)
                     (superchat-llm--pending-pause-reason pending)))
          (should (= 2 (length test-input-batch--requests)))
          (should (superchat-llm--transport-timer-owner-wait-timer owner))
          (should (= deadline (superchat-llm--transport-timer-owner-hard-deadline owner)))
          (test-input-batch--reply (car test-input-batch--requests) nil)
          (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))))

(ert-deftest input-batch/clock-recovery-acquisition-unwind ()
  "Wait-only recovery rollback leaves hard elapsed charged and newer input intact."
  (dolist (mode '(async streaming))
    (dolist (exit '(error quit cancel question q2))
      (test-input-batch--fixture mode
        (setq superchat-response-timeout 1 superchat-response-timeout-hard-limit 5)
        (let* (resume caught acquired
               (superchat-llm-context-budget-exceeded-functions
                (list (lambda (details) (setq resume (plist-get details :resume-function)) t)))
               (run (plist-get (test-input-batch--submit
                                "recovery-unwind" t '(:context-envelope (:prompt "INPUT BATCH"))) :run-id))
               (request (car test-input-batch--requests))
               (owner (superchat-runtime-transport-timer-owner run))
               (deadline (superchat-llm--transport-timer-owner-hard-deadline owner))
               (activate (symbol-function 'timer-activate)))
          (funcall (plist-get request :on-error) 'http-error "maximum context length exceeded")
          (should resume)
          (cl-letf (((symbol-function 'timer-activate)
                     (lambda (timer &rest args)
                       (prog1 (apply activate timer args)
                         (unless acquired
                           (setq acquired timer)
                           (sleep-for 0.03)
                           (pcase exit
                             ('cancel (superchat-runtime-cancel run "stop"))
                             ((or 'question 'q2)
                              (test-input-batch--reply request '("input_batch_ask"))
                              (when (eq exit 'q2)
                                (superchat-runtime-answer-input
                                 run (plist-get (cdar test-input-batch--questions) :handle) "City")
                                (test-input-batch--reply (car test-input-batch--requests)
                                                        '("input_batch_ask"))))
                             (_ (signal exit '(recovery-acquisition)))))))))
            (condition-case condition (funcall resume "NEW CONTEXT")
              ((error quit) (setq caught condition))))
          (should acquired)
          (should-not (memq acquired timer-list))
          (should-not (superchat-llm--transport-timer-owner-wait-timer owner))
          (should (= (if (eq exit 'q2) 2 1) (length test-input-batch--requests)))
          (pcase exit
            ('quit
             ;; S19-fix-3 / contract §3.3's terminal-`quit' rule: a `quit'
             ;; during recovery finalizes this Run `failed' (like `error'
             ;; already does) and is then re-signalled to the caller, so it
             ;; is no longer left `running' with its deadline untouched.
             (should (equal caught '(quit recovery-acquisition)))
             (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner)))
             (should (eq 'failed (plist-get (superchat-runtime-status run) :status))))
            ((or 'question 'q2)
             (should (superchat-runtime-pending-input run))
             (should (eq 'paused (superchat-llm--transport-timer-owner-hard-state owner))))
            (_ (should (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner))))))))))

;;;---------------------------------------------
;;; S08 · streaming capability declaration and resolution
;;;---------------------------------------------

(ert-deftest llm/resolve-streaming-precedence-override-provider-adapter-fallback ()
  "Override beats provider beats adapter beats fallback, decided per boolean."
  (unless (and (require 'llm-openai nil t) (require 'llm-deepseek nil t))
    (ert-skip "llm-openai/llm-deepseek are not installed"))
  (let ((superchat-llm-model-capability-overrides nil))
    ;; `llm-openai-compatible' (exact type): unknown model has no
    ;; model-matched capabilities, so the provider only asserts
    ;; `streaming'; the adapter rule then supplies `streaming-tool-use'.
    (let* ((provider
            (make-llm-openai-compatible
             :key "x" :chat-model "unknown-compatible-model"
             :url "http://127.0.0.1:1234/v1/"))
           (with-tools (superchat-llm--resolve-streaming provider "unknown-compatible-model" t))
           (without-tools (superchat-llm--resolve-streaming provider "unknown-compatible-model" nil)))
      (should (equal (list :streaming t :streaming-tool-use t :source 'adapter) with-tools))
      (should (equal (list :streaming t :streaming-tool-use t :source 'provider) without-tools)))
    ;; `llm-deepseek' subtypes `llm-openai-compatible' but is not `eq' to
    ;; it, so an unknown model gets provider `streaming' and falls all
    ;; the way to `fallback' for `streaming-tool-use'.
    (let* ((provider
            (make-llm-deepseek :key "x" :chat-model "unknown-local"))
           (resolved (superchat-llm--resolve-streaming provider "unknown-local" t)))
      (should (equal (list :streaming t :streaming-tool-use nil :source 'fallback) resolved))
      ;; An override for the same model takes precedence over both the
      ;; provider and adapter levels and decides both booleans.
      (superchat-llm-set-model-capabilities
       "unknown-local" '(:streaming t :streaming-tool-use t))
      (should (equal (list :streaming t :streaming-tool-use t :source 'override)
                     (superchat-llm--resolve-streaming provider "unknown-local" t))))
    ;; `llm-openai' always advertises both capabilities via the provider
    ;; level, regardless of model.
    (let* ((provider (make-llm-openai :key "x" :chat-model "gpt-anything"))
           (resolved (superchat-llm--resolve-streaming provider "gpt-anything" t)))
      (should (equal (list :streaming t :streaming-tool-use t :source 'provider) resolved)))))

(ert-deftest llm/set-model-capabilities-validates-input ()
  "MODEL must be a non-empty string; both booleans must be present and each
exactly `nil' or `t' -- a missing key, `no', a string, or a number all
signal a clear `user-error' instead of being silently coerced."
  (let ((superchat-llm-model-capability-overrides nil))
    (should-error
     (superchat-llm-set-model-capabilities "m" '(:streaming t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities "m" '(:streaming-tool-use t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities
      "m" (list :streaming 'no :streaming-tool-use t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities
      "m" '(:streaming "yes" :streaming-tool-use t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities
      "m" '(:streaming 1 :streaming-tool-use t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities 42 '(:streaming t :streaming-tool-use t))
     :type 'user-error)
    (should-error
     (superchat-llm-set-model-capabilities "" '(:streaming t :streaming-tool-use t))
     :type 'user-error)
    ;; The positive control: a genuinely valid declaration still succeeds.
    (superchat-llm-set-model-capabilities "m" '(:streaming t :streaming-tool-use nil))
    (should (equal '(:streaming t :streaming-tool-use nil)
                   (alist-get "m" superchat-llm-model-capability-overrides
                              nil nil #'equal)))))

(ert-deftest llm/tools-attempt-requires-both-booleans ()
  "A tools attempt streams only when both booleans resolve true; async otherwise.

Real `llm-openai', real `llm-chat-prompt' struct, real (unreplaced)
`llm-capabilities' — only the two transport calls are replaced.  S08-fix-3
item 1/2: `test-llm--with-mock-llm' replaces `llm-make-chat-prompt' with a
plain plist, so the real `llm-chat-prompt-tools' struct accessor the
production code now calls cannot be exercised through it; using a real
provider and prompt here both fixes that and drops the last
`llm-capabilities' stub this test used to sit under (S08-fix-2 item 3 had
already removed the test's own explicit stub, but the shared helper still
installed one)."
  (unless (require 'llm-openai nil t)
    (ert-skip "llm-openai is not installed"))
  (let* ((model "gpt-4o-mini")
         (superchat-llm-backend (make-llm-openai :key "unused" :chat-model model))
         (superchat-llm-model-capability-overrides nil)
         (superchat-response-timeout nil)
         (superchat-llm-tools-enabled 'always)
         ;; `all' so the baseline's own configured-names filter
         ;; (S09-fix-3 item 1) does not drop this fixture's arbitrarily
         ;; named tool -- only `all' passes every registered/cached name
         ;; through unfiltered.
         (superchat-llm-tool-names 'all)
         (superchat-llm-tools-list
          (list (test-llm--make-tool :name "fake-tool" :description "d"
                                     :args nil :function (lambda () "used"))))
         (async-calls 0) (streaming-calls 0) final-result)
    ;; Declare `streaming' but withhold `streaming-tool-use' for MODEL.
    ;; The override alone decides both booleans (precedence: override >
    ;; provider > adapter > fallback), so this test needs no
    ;; `llm-capabilities' replacement at all: the real backend's own
    ;; capability shape is irrelevant here.
    (superchat-llm-set-model-capabilities
     model '(:streaming t :streaming-tool-use nil))
    (cl-letf (((symbol-function 'llm-chat-async)
               (lambda (_provider _prompt response-cb _error-cb
                        &optional _multi-output)
                 (cl-incf async-calls)
                 (if (= async-calls 1)
                     (funcall response-cb
                              '(:tool-results (("fake-tool" . "tool result"))))
                   (funcall response-cb "final answer"))))
              ((symbol-function 'llm-chat-streaming)
               (lambda (&rest _args) (cl-incf streaming-calls))))
      (superchat--llm-generate-answer
       "use the tool"
       (lambda (result) (setq final-result result))
       #'ignore)
      (should (= 2 async-calls))
      (should (= 0 streaming-calls))
      (should (equal "final answer" final-result)))))

(ert-deftest llm/streaming-capabilities-resolve-per-attempt-not-per-request ()
  "A round-limit final-answer attempt resolves capabilities against its own
(tool-free) surface, not the original tool-bearing request's.  Real
`llm-deepseek', unrecognized model: it streams but never advertises
`streaming-tool-use', and is not the exact `llm-openai-compatible' adapter
type, so with tools attached :streaming-tool-use falls to `fallback' and
the attempt must go async; the subsequent tool-free final attempt must
resolve :streaming from `provider' and actually stream.  Only the two
transport calls are replaced; llm.el itself is real."
  (unless (require 'llm-deepseek nil t)
    (ert-skip "llm-deepseek is not installed"))
  (let* ((superchat-llm-backend
          (make-llm-deepseek :key "x" :chat-model "unknown-local"))
         (superchat-llm-model-capability-overrides nil)
         (superchat-response-timeout nil)
         (superchat-llm-tools-enabled 'always)
         (superchat-llm-tools-list
          (list (test-llm--make-tool :name "fake-tool" :description "d"
                                     :args nil :function (lambda () "used"))))
         (superchat-llm-max-tool-rounds 1)
         (async-calls 0) (streaming-calls 0) attempts final-result
         (superchat--llm-runtime-observer
          (lambda (event value)
            (when (eq event 'attempt-started) (push value attempts)))))
    (cl-letf (((symbol-function 'llm-chat-async)
               (lambda (_provider _prompt response-cb _error-cb
                        &optional _multi-output)
                 (cl-incf async-calls)
                 (funcall response-cb
                          '(:tool-results (("fake-tool" . "tool result"))))))
              ((symbol-function 'llm-chat-streaming)
               (lambda (_provider _prompt _partial-cb response-cb _error-cb
                        &optional _multi-output)
                 (cl-incf streaming-calls)
                 (funcall response-cb "final answer"))))
      (superchat--llm-generate-answer
       "use the tool"
       (lambda (result) (setq final-result result))
       #'ignore))
    (setq attempts (nreverse attempts))
    (should (= 2 (length attempts)))
    (should (= 1 async-calls))
    (should (= 1 streaming-calls))
    (should (equal "final answer" final-result))
    (let ((tool-attempt (nth 0 attempts))
          (final-attempt (nth 1 attempts)))
      (should (plist-get tool-attempt :streaming))
      (should-not (plist-get tool-attempt :streaming-tool-use))
      (should (eq 'fallback (plist-get tool-attempt :capability-source)))
      (should (plist-get final-attempt :streaming))
      (should (eq 'provider (plist-get final-attempt :capability-source))))))

(ert-deftest llm/master-switch-off-disables-streaming-and-is-recorded-separately ()
  "`superchat-llm-streaming' nil forces async and is recorded apart from capability-source.

Real `llm-openai', real `llm-chat-prompt' struct, real (unreplaced)
`llm-capabilities' — only the two transport calls are replaced.  `llm-openai'
unconditionally reports `streaming' in its capabilities (a pure function, no
network call), so :capability-source resolves to `provider' without an
override or a mock.  See the S08-fix-3 item 1/2 note on the previous test
for why this test moved off `test-llm--with-mock-llm'."
  (unless (require 'llm-openai nil t)
    (ert-skip "llm-openai is not installed"))
  (let* ((superchat-llm-backend
          (make-llm-openai :key "unused" :chat-model "unrecognized-model"))
         (superchat-llm-model-capability-overrides nil)
         (superchat-response-timeout nil)
         (superchat-llm-tools-enabled nil)
         (superchat-llm-streaming nil)
         (async-calls 0) (streaming-calls 0) attempt final-result)
    (let ((superchat--llm-runtime-observer
           (lambda (event value)
             (when (eq event 'attempt-started) (setq attempt value)))))
      (cl-letf (((symbol-function 'llm-chat-async)
                 (lambda (_provider _prompt response-cb _error-cb
                          &optional _multi-output)
                   (cl-incf async-calls)
                   (funcall response-cb "final answer")))
                ((symbol-function 'llm-chat-streaming)
                 (lambda (&rest _args) (cl-incf streaming-calls))))
        (superchat--llm-generate-answer
         "no tools"
         (lambda (result) (setq final-result result))
         #'ignore)
        (should (= 1 async-calls))
        (should (= 0 streaming-calls))
        (should (equal "final answer" final-result))
        (should-not (plist-get attempt :streaming-enabled))
        ;; The master switch does not change which level decided the
        ;; capability itself.
        (should (eq 'provider (plist-get attempt :capability-source)))
        (should (plist-get attempt :streaming))))))

;; S08-fix-3 item 3: `llm/attempt-details-record-capability-source' moved to
;; `test/test-journeys-chat.el' (as `journey-chat/attempt-details-record-capability-source').
;; It uses the real journey fixture, which requires `llm'/`llm-openai'
;; unconditionally and fails loudly by design; that contradicts this file's
;; own documented "tests do not require llm.el" property (`ert-skip'-ing the
;; one test here instead just traded a loud file-load failure for a silent
;; per-test skip, which the S08 re-review rejected -- journey tests must
;; fail, not skip).  The journeys file already requires the fixture
;; unconditionally at its own top level, so this assertion belongs there.

(ert-deftest llm/drain-port-nil-means-no-steers ()
  "With no Runtime bound, the drain/pending ports are nil and steer-free."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-backend (test-llm--make-mock-backend 'openai))
           (superchat-response-timeout nil)
           (requests 0)
           final-result)
       (should-not superchat--llm-drain-steers-function)
       (should-not superchat--llm-pending-steer-count-function)
       (cl-letf (((symbol-function 'llm-chat-streaming)
                  (lambda (_provider _prompt _partial-cb response-cb
                           _error-cb &optional _multi-output)
                    (cl-incf requests)
                    (funcall response-cb "no steers here"))))
         (superchat--llm-generate-answer
          "say hi" (lambda (result) (setq final-result result)) #'ignore)
         (should (= 1 requests))
         (should (string= "no steers here" final-result)))))))

;;;---------------------------------------------
;;; S09: superchat-select-tools (contract §4.2)
;;;---------------------------------------------

(ert-deftest llm/select-tools-explicit-none-and-list-filtered-to-descriptors ()
  "Contract §4.2 rule 1: an explicit `:tools' on the request wins outright
-- `none' stays `none', and a list is filtered to registered Authority
descriptors, dropping any name with no registered effect resolver."
  (should (eq 'none (superchat-select-tools (list :tools 'none) nil)))
  (should (equal '("read_buffer" "apropos")
                  (superchat-select-tools
                   (list :tools '("read_buffer" "not-a-real-tool" "apropos"))
                   nil))))

(ert-deftest llm/select-tools-stage-implementation-wins-over-baseline ()
  "Contract §4.2 rule 2: a registered `select-tools' stage implementation
wins over the surface baseline when REQUEST carries no explicit `:tools'."
  (require 'superchat-stage)
  (let ((id (superchat-stage-add
             'select-tools 'llm-test-select-tools-narrow
             (lambda (_request _state) '("read_buffer"))
             :owner 'llm-test)))
    (unwind-protect
        (let ((superchat-llm-tools-enabled 'always))
          (should (equal '("read_buffer")
                          (superchat-select-tools (list :surface 'chat) nil))))
      (superchat-stage-remove 'select-tools id))))

(ert-deftest llm/select-tools-baseline-by-surface-and-input-operation ()
  "Contract §4.2 rule 3: the surface baseline differs by surface; only the
chat/workflow/subagent/external/plugin bucket adds `ask_user' when the
request carries an input operation; `inline-rewrite', `org-babel',
`magit', `skills' and `compaction' always get `none'.

The chat/workflow/external/no-surface bucket's expected order here is
`superchat-get-llm-tools''s own registry-build order (`list_buffers',
`read_buffer', `edit_buffer', `apropos', `eval_elisp' -- same as D14's
already-established expectation), not `superchat-llm-tool-names''s own
literal list order: S09-fix-2 item 1 sources this baseline from that
exact registry query instead of a hand-listed approximation of it, so a
decision's reported names now always match what actually materializes."
  (let ((superchat-llm-tools-enabled 'always)
        (superchat-llm-tool-names
         '("list_buffers" "read_buffer" "apropos" "edit_buffer" "eval_elisp"))
        (superchat-authority-unprompted-read-tools
         '("read_buffer" "list_buffers" "apropos")))
    (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos" "eval_elisp")
                    (superchat-select-tools (list :surface 'chat) nil)))
    (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos"
                      "eval_elisp" "ask_user")
                    (superchat-select-tools
                     (list :surface 'chat :input-core '(:operation ask-user)) nil)))
    (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos" "eval_elisp")
                    (superchat-select-tools (list :surface 'workflow) nil)))
    (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos" "eval_elisp")
                    (superchat-select-tools (list :surface 'external) nil)))
    (should (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos" "eval_elisp")
                    (superchat-select-tools (list) nil)))
    (should (equal '("read_buffer" "list_buffers" "apropos")
                    (superchat-select-tools (list :surface 'inline-ask) nil)))
    (dolist (surface '(inline-rewrite org-babel magit skills compaction))
      (should (eq 'none (superchat-select-tools (list :surface surface) nil))))))

(ert-deftest llm/select-tools-nil-policy-gives-none-for-chat ()
  "Contract §4.2 rule 4: `superchat-llm-tools-enabled' nil forces `none'
for every baseline (chat included, even with an input operation granted),
but never overrides rule 1's explicit `:tools'."
  (let ((superchat-llm-tools-enabled nil))
    (should (eq 'none (superchat-select-tools (list :surface 'chat) nil)))
    (should (eq 'none (superchat-select-tools
                        (list :surface 'chat :input-core '(:operation ask-user)) nil)))
    (should (equal '("read_buffer")
                    (superchat-select-tools (list :tools '("read_buffer")) nil)))))

;; S09-fix-1: the materializer now keeps exactly what `superchat-select-
;; tools-decision' decided instead of re-querying with the caller's own
;; restriction (or none at all).  These tests exercise the collector
;; (`superchat--collect-llm-tools'), not just the selector's own return
;; value, since that is exactly where the review's Critical defect lived.

(ert-deftest llm/collect-tools-materializes-exactly-the-stage-selection ()
  "A `select-tools' stage implementation's narrower decision is exactly
what the materializer sends -- it never re-queries with the caller's own
(wider or absent) restriction (contract §4.2).  The review's own probe: a
stage returning only (\"read_buffer\") still materialized (\"read_buffer\"
\"edit_buffer\") before this fix."
  (test-llm--with-mock-llm
   (lambda ()
     (superchat-stage-add
      'select-tools 'llm-fix-test-narrow
      (lambda (&rest _) (list "read_buffer")) :owner 'test)
     (unwind-protect
         (should (equal '("read_buffer")
                        (mapcar #'llm-tool-name
                                (superchat--collect-llm-tools "hello"))))
       (superchat-stage-remove 'select-tools 'llm-fix-test-narrow)))))

(ert-deftest llm/select-tools-stage-all-invalid-names-fails-closed ()
  "A stage result whose names are all invalid yields `none' (fail-closed),
distinct from no stage result at all, which falls to the baseline
(contract §4 row 4; S09-fix-1 item 4)."
  (superchat-stage-add
   'select-tools 'llm-fix-test-invalid
   (lambda (&rest _) (list "not_a_real_tool_at_all")) :owner 'test)
  (unwind-protect
      (should (eq 'none (superchat-select-tools (list :surface 'chat) nil)))
    (superchat-stage-remove 'select-tools 'llm-fix-test-invalid)))

(ert-deftest llm/select-tools-explicit-dropped-names-are-recorded ()
  "An explicit `:tools' name lacking a registered descriptor is dropped
and recorded as a stage fault, not silently discarded (contract §4.2
rule 1; S09-fix-1 item 4)."
  (let ((superchat-stage-faults nil))
    (superchat-select-tools
     (list :tools '("read_buffer" "not_a_real_tool_at_all")) nil)
    (should
     (cl-find "not_a_real_tool_at_all" superchat-stage-faults
              :key (lambda (fault)
                     (plist-get (plist-get fault :condition) :dropped-name))
              :test #'equal))))

(ert-deftest llm/collect-tools-respects-surface-none-list-end-to-end ()
  "`:surface' now reaches the materializer, through the buffer-local
`superchat--last-surface' Runtime sets before dispatch (S09-fix-1 item 1):
`compaction', `skills', `inline-rewrite', `org-babel', and `magit'
producers that pass no `:tools' genuinely get `none' end to end -- not the
generic baseline (contract §4.2 rule 3; the review's compaction/skills
tool leak finding).  `chat' still attaches its baseline."
  (test-llm--with-mock-llm
   (lambda ()
     (with-temp-buffer
       (dolist (surface '(compaction skills inline-rewrite org-babel magit))
         (let ((superchat--last-surface surface))
           (should-not (superchat--collect-llm-tools "hello"))))
       (let ((superchat--last-surface 'chat))
         (should (superchat--collect-llm-tools "hello")))))))

(ert-deftest llm/select-tools-baseline-all-expands-to-every-registered-descriptor ()
  "`superchat-llm-tool-names' set to `all' selects every registered
descriptor in stable order for the baseline, not the fixed five (contract
§4.2 rule 3; S09-fix-1 item 6) -- distinct from a list value, which stays
an exact allowlist."
  (test-llm--with-mock-llm
   (lambda ()
     ;; `superchat-get-llm-tools' caches its build keyed on
     ;; `superchat--llm-tool-names-signature'; isolating the cache
     ;; alongside `superchat-llm-tool-names' keeps a broad `all' build
     ;; from surviving into a later test as a stale global cache.
     (let* ((superchat-llm-tools-list nil)
            (superchat--llm-tool-names-signature nil)
            (superchat-llm-tool-names 'all)
            (names (superchat-select-tools (list :surface 'chat) nil)))
       (should (listp names))
       (should (member "list_buffers" names))
       ;; `all' is strictly wider than the fixed five whenever more than
       ;; five descriptors are registered, which the real Superchat tool
       ;; registry already is.
       (should (> (length names) 5))))))

(ert-deftest llm/select-tools-baseline-custom-list-filtered-to-descriptors ()
  "A custom `superchat-llm-tool-names' list is filtered to registered
descriptors for the baseline too, not used verbatim (contract §4.2;
S09-fix-1 item 6)."
  (let ((superchat-llm-tool-names '("read_buffer" "not_a_real_tool_at_all")))
    (should (equal '("read_buffer")
                   (superchat-select-tools (list :surface 'chat) nil)))))

;; S09-fix-3 item 1, narrowed by S09-fix-5 item 3's user decision: the
;; baseline still intersects `superchat-llm-tool-names' with one registry
;; query, but only for a BUILT-IN name -- a plugin-registered name always
;; survives regardless (see `llm/select-tools-decision-includes-plugin-
;; registered-tool-by-default' below).  This test now covers the narrower
;; surviving half of the original `s09_extra' reproduction: an extra
;; descriptor the registry query returns that is NOT plugin-registered
;; must still not widen the baseline.  Simulated via a direct stub of
;; `superchat-get-llm-tools' rather than a real `superchat-register-tool'
;; call, because every non-built-in name the real registry can hold is, by
;; construction, plugin-registered -- there is no longer a production path
;; that produces a genuine non-plugin extra.

(ert-deftest llm/select-tools-baseline-ignores-non-plugin-descriptor-outside-configuration ()
  "An extra descriptor the registry query returns, that is NOT
plugin-registered (no entry in `superchat-tools--external-registry'), must
not widen the baseline: the decision and the materialized list stay
exactly the configured five."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tool-names
             '("list_buffers" "read_buffer" "apropos" "edit_buffer" "eval_elisp"))
            (superchat-llm-tools-enabled 'always)
            (expected '("list_buffers" "read_buffer" "apropos"
                        "edit_buffer" "eval_elisp")))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda (&rest _)
                    (append
                     (mapcar (lambda (name) (test-llm--make-tool :name name))
                             superchat-llm-tool-names)
                     (list (test-llm--make-tool :name "s09_extra_non_plugin"))))))
         (let ((decision (superchat-select-tools-decision (list :surface 'chat) nil)))
           (should (equal expected (plist-get decision :names)))
           (should (eq 'baseline (plist-get decision :reason)))
           (should
            (equal expected
                   (mapcar #'llm-tool-name
                           (let ((superchat--last-surface 'chat))
                             (superchat--collect-llm-tools "hello")))))))))))

;; S09-fix-3 item 2: `report_plan_progress', a synthesized tool, and the
;; `render_ui' exclusion are decided inside `superchat-select-tools-
;; decision' itself now -- they must show up in the DECISION's own
;; `:names', not only in what the materializer separately assembles.

(ert-deftest llm/select-tools-decision-includes-plan-progress-when-plan-active ()
  "`report_plan_progress' is in the DECISION's own names while a plan is
active (S09-fix-3 item 2)."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-plan)
     (let ((buf (generate-new-buffer "*s09fix3-plan-decision*")))
       (unwind-protect
           (progn
             (with-current-buffer buf
               (setq-local superchat--active-plan
                           (list :summary "s" :event-id "e" :steps nil)))
             (let ((superchat-buffer-name (buffer-name buf))
                   (superchat-llm-tools-enabled 'always))
               (should
                (member "report_plan_progress"
                        (plist-get
                         (superchat-select-tools-decision (list :surface 'chat) nil)
                         :names)))))
         (kill-buffer buf))))))

(ert-deftest llm/select-tools-decision-includes-synthesized-tool-name ()
  "A session-synthesized tool's name is in the DECISION's own names
(S09-fix-3 item 2)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-synthesized-tools
            (list (llm-make-tool :name "s09fix3_synth" :description "d"
                                  :args nil :function #'ignore)))
           (superchat-llm-tools-enabled 'always))
       (should
        (member "s09fix3_synth"
                (plist-get
                 (superchat-select-tools-decision (list :surface 'chat) nil)
                 :names)))))))

(ert-deftest llm/select-tools-decision-drops-define-tool-for-render-ui-intent ()
  "A direct `render_ui' request already excludes `define_tool' from the
DECISION's own names (S09-fix-3 item 2)."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tools-list nil)
            (superchat--llm-tool-names-signature nil)
            (superchat-llm-tool-names 'all)
            (superchat-llm-tools-enabled 'always))
       (unwind-protect
           (progn
             (should-not
              (member "define_tool"
                      (plist-get
                       (superchat-select-tools-decision
                        (list :surface 'chat :input "please render_ui now") nil)
                       :names)))
             (should
              (member "define_tool"
                      (plist-get
                       (superchat-select-tools-decision (list :surface 'chat) nil)
                       :names))))
         (setq superchat-llm-tools-list nil
               superchat--llm-tool-names-signature nil))))))

(ert-deftest llm/collect-tools-precomputed-stage-decision-with-synthesized-tool-stays-exact ()
  "The review's own probe (S09-fix-3 item 2): a precomputed stage decision
naming only `read_buffer', with a synthesized tool also present, still
materializes exactly `(\"read_buffer\")' -- a synthesized tool never rides
along just because it exists; only a name the decision actually listed is
ever resolved into a struct."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-synthesized-tools
            (list (llm-make-tool :name "s09fix3_synth2" :description "d"
                                  :args nil :function #'ignore))))
       (should
        (equal '("read_buffer")
               (mapcar #'llm-tool-name
                       (superchat--collect-llm-tools
                        "hello" nil nil
                        (list :names '("read_buffer") :reason 'stage)))))))))

;; S09-fix-3 item 3: an unknown, non-list policy value falls back to the
;; fixed five names filtered to registered descriptors, and signals
;; nothing (the re-review's `unknown-policy' reproduction, which used to
;; signal `(wrong-type-argument listp unknown-policy)').

(ert-deftest llm/select-tools-unknown-non-list-policy-falls-back-without-signal ()
  "An unknown non-list `superchat-llm-tool-names' value (e.g. the symbol
`unknown-policy') falls back to the fixed five names filtered to
registered descriptors, with no signal (S09-fix-3 item 3)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tool-names 'unknown-policy)
           (superchat-llm-tools-enabled 'always))
       (should
        (equal '("list_buffers" "read_buffer" "edit_buffer" "apropos" "eval_elisp")
               (superchat-select-tools (list :surface 'chat) nil)))))))

;;;---------------------------------------------
;;; S09-fix-5: one query even when empty; plan-tool guards in the
;;; decision; plugin tools in the baseline by default
;;;---------------------------------------------

(ert-deftest llm/select-tools-decision-queries-registry-exactly-once-even-when-empty ()
  "The baseline decision's registry query is reused by the materializer
even when it legitimately returns an empty STRUCTS list -- an explicit
`:queried' presence marker decides reuse, not the query result's own
truthiness (S09-fix-5 item 1; the re-review's own counting probe against
the pre-fix code printed EMPTY-REGISTRY-CALLS=2)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((calls 0)
           (superchat-llm-tool-names nil)
           (superchat-llm-tools-enabled 'always))
       (cl-letf (((symbol-function 'superchat-get-llm-tools)
                  (lambda (&rest _) (cl-incf calls) nil)))
         (should-not (superchat--collect-llm-tools "hello")))
       (should (= 1 calls))))))

(ert-deftest llm/select-tools-decision-omits-plan-progress-when-materializer-guard-absent ()
  "`report_plan_progress' is NOT added to the DECISION's own names when
either materializer guard (`llm-make-tool'/
`superchat--report-plan-progress-spec') is unavailable, even while a plan
is active -- the decision must never name a tool the materializer cannot
build (S09-fix-5 item 2; the re-review's finding that the decision-time
predicate omitted both guards)."
  (test-llm--with-mock-llm
   (lambda ()
     (require 'superchat-plan)
     (let ((buf (generate-new-buffer "*s09fix5-plan-guard-absent*")))
       (unwind-protect
           (progn
             (with-current-buffer buf
               (setq-local superchat--active-plan
                           (list :summary "s" :event-id "e" :steps nil)))
             (let ((superchat-buffer-name (buffer-name buf))
                   (superchat-llm-tools-enabled 'always))
               (cl-letf (((symbol-function 'superchat--report-plan-progress-spec) nil))
                 (should-not
                  (member "report_plan_progress"
                          (plist-get
                           (superchat-select-tools-decision (list :surface 'chat) nil)
                           :names))))))
         (kill-buffer buf))))))

;; S09-fix-5 item 3 (user decision, contract §4.2 rule 3 amended): a tool a
;; plugin registered through `superchat-register-tool' is part of the
;; chat-like default baseline regardless of `superchat-llm-tool-names';
;; that variable still narrows only Superchat's own built-in tools.

(ert-deftest llm/select-tools-decision-includes-plugin-registered-tool-by-default ()
  "A tool registered via `superchat-register-tool' is in the default chat
decision without customizing `superchat-llm-tool-names' (S09-fix-5 item 3;
the `human_input_read' regression S09-fix-4 had papered over with an
explicit `all' test binding instead of a product decision)."
  (test-llm--with-mock-llm
   (lambda ()
     (let* ((superchat-llm-tools-list nil)
            (superchat--llm-tool-names-signature nil)
            (superchat-llm-tool-names
             '("list_buffers" "read_buffer" "apropos" "edit_buffer" "eval_elisp"))
            (superchat-llm-tools-enabled 'always))
       (superchat-register-tool
        "s09fix5_plugin" :description "test-only plugin tool" :args nil
        :function #'ignore
        :effects (list (list :effect 'external-read :resource 's09fix5-plugin)))
       (unwind-protect
           (should
            (member "s09fix5_plugin"
                    (plist-get
                     (superchat-select-tools-decision (list :surface 'chat) nil)
                     :names)))
         (superchat-unregister-tool "s09fix5_plugin")
         (setq superchat-llm-tools-list nil
               superchat--llm-tool-names-signature nil))))))

(ert-deftest llm/select-tools-decision-excludes-unconfigured-builtin-tool ()
  "A built-in tool outside the configured default five is still excluded
-- the amendment widens the baseline only for plugin-registered tools, not
for every built-in one (S09-fix-5 item 3)."
  (test-llm--with-mock-llm
   (lambda ()
     (let ((superchat-llm-tool-names
            '("list_buffers" "read_buffer" "apropos" "edit_buffer" "eval_elisp"))
           (superchat-llm-tools-enabled 'always))
       (should-not
        (member "define_tool"
                (plist-get
                 (superchat-select-tools-decision (list :surface 'chat) nil)
                 :names)))))))

;;; superchat-test-llm-backend.el ends here
