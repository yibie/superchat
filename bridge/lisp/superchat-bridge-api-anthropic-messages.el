;;; superchat-bridge-api-anthropic-messages.el --- Anthropic Messages API, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/anthropic-messages.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;;   `superchat-bridge-api-anthropic-messages-stream'          `stream'
;;   `superchat-bridge-api-anthropic-messages-stream-simple'   `streamSimple'
;;
;; Everything else is private and uses the prefix `superchat-bridge-am--'.
;;
;; The `@anthropic-ai/sdk' client (0.129.0) is replaced by
;; `superchat-bridge-http-request' (curl).  A request is a POST of the
;; JSON that `buildParams' makes to `<base-url>/v1/messages?beta=true',
;; as the SDK's `beta.messages.create' makes it: `betas' leaves the body
;; and becomes the `anthropic-beta' header (joined with commas), as do
;; `user_profile_id' and `workspace_id'.  The headers are the SDK's, in
;; its order, later over earlier without regard to case, a nil value
;; removing one: `Accept', `anthropic-dangerous-direct-browser-access'
;; and `anthropic-version: 2023-06-01'; `X-Api-Key' from the API key;
;; the client's default headers as `createClient' makes them (session
;; affinity, the model's, the options'); `Content-Type'; the request's
;; own (`anthropic-beta' and friends).  The request is wrapped in
;; `superchat-bridge-retry-provider-request' where the source wraps it in
;; `retryProviderRequest'.  The body of the response is read with the
;; file's own SSE decoder (the source's `iterateSseMessages'), not with
;; `superchat-bridge-sse-parser': the source's decoder keeps an event
;; that has no data, delivers a last event not ended by a blank line,
;; and keeps the raw lines for its error text.
;;
;; Representation.
;;   MODEL     an `superchat-bridge-model'; its `compat' as the catalog
;;             gives it (an alist with kebab-case symbol keys, nil for
;;             JSON false, `:null' for JSON null, which counts as absent).
;;             `allowed-fallback-models' is a list of alists with string
;;             keys (\"provider\", \"model\", \"cost\").
;;   CONTEXT   an `superchat-bridge-context' already normalized.
;;   OPTIONS   a plist.  Keys read here: `:api-key' `:headers' `:env'
;;             `:signal' `:session-id' `:cache-retention' `:max-tokens'
;;             `:temperature' `:metadata' `:tool-choice' `:timeout-ms'
;;             `:max-retries' `:max-retry-delay-ms' `:on-payload'
;;             `:on-response' `:on-provider-stream-event', and the
;;             source's `AnthropicOptions': `:thinking-enabled'
;;             (`:false' or nil is false, `:undefined' or absent is
;;             undefined; the difference matters for `thinking:
;;             disabled'), `:thinking-budget-tokens', `:effort',
;;             `:thinking-display', `:interleaved-thinking' (absent is
;;             true, `:false' false).  `stream-simple' also reads
;;             `:reasoning', `:thinking-budgets' and `:sampling-params'.
;;             `:metadata' is an alist or plist whose `user_id' is read;
;;             `:tool-choice' a string or a JSON object.
;;   TOOLS     a tool's `constrained-sampling' is a plist
;;             (:type \"json_schema\" :strict \"prefer\" or \"require\");
;;             any other value is not JSON-schema sampling and is sent
;;             as an ordinary tool, as the source does.
;;   HOOKS     `:on-payload' gets (PARAMS MODEL), PARAMS the body as an
;;             alist (with `betas'), and may return a replacement (an
;;             alist, plist or hash table) or nil; `:on-response' gets
;;             an `superchat-bridge-provider-response' and MODEL;
;;             `:on-provider-stream-event' each parsed event (a hash
;;             table) and MODEL.  An error a hook signals ends the stream
;;             with an `error' event.
;;   ABORT     `:signal' is an `superchat-bridge-abort-signal'; a listener
;;             on it aborts the request in flight.
;;   DIAGNOSTICS  an assistant message's `diagnostics' is a list of
;;             plists (:type STRING :timestamp MS [:error (:message S)]
;;             [:details PLIST]), the source's `AssistantMessageDiagnostic'.
;;
;; JSON is built and read as in `superchat-bridge-api-openai-completions'
;; (see its commentary): objects are alists with symbol keys in the
;; source's key order (nil is `{}'), arrays vectors, t and `:false',
;; `:null'; caller data goes through `superchat-bridge-am--json'.  Events
;; parse to hash tables with string keys.
;;
;; Errors.  The source's `catch' puts `error.message' in the message, not
;; `formatProviderError'.  An HTTP status error has the message the
;; Anthropic SDK's `APIError' has: the status, a space, and the body's
;; JSON (the SDK takes the whole body as `error', which has no top-level
;; `message'), e.g. `429 {\"type\":\"error\",\"error\":{...}}'; a body
;; that is no JSON is given as it is.  So a status leads the text as in
;; the openai-completions port.  The plist carries `:type' from
;; `body.error.type' for the strict-tool fallback.
;;
;; Left out, whole:
;;   - OAuth (Claude Code subscription tokens): an API key containing
;;     \"sk-ant-oat\" ends the stream with an error, since the source's
;;     OAuth branch (Bearer token, Claude Code identity headers and
;;     system prompt, the `claude-code' and `oauth' betas, Claude Code
;;     tool names) is not ported; `isOAuth' is therefore always false.
;;   - GitHub Copilot (`buildCopilotDynamicHeaders', its Bearer client):
;;     a model of provider \"github-copilot\" ends with an error.
;;   - Workload identity federation (`getAnthropicFederation', the SDK's
;;     token exchange): never used; a request needs a key or header.
;;   - The `client' option (an injected SDK client), the `fetch' option,
;;     client identity headers (`utils/identity.ts' is not ported), the
;;     SDK's User-Agent and X-Stainless-* headers, its default timeout
;;     of ten minutes (`:timeout-ms' is honoured when given).
;;   - The grammar half of `src/api/constrained-sampling.ts' (Anthropic
;;     has no grammar tools).
;;
;; Differences owed to the runtime:
;;   - `timeoutMs' is the time until the response headers, kept by a
;;     timer, as in the openai-completions port.
;;   - A connection failure reads \"Connection error: <curl's message>\",
;;     a timeout \"Request timed out.\", an abort before the response
;;     \"Request was aborted.\", one during the body \"Request was
;;     aborted\" (the source's `iterateSseMessages').
;;   - Private copies here, until those files have owners: of
;;     `src/utils/transcript.ts' (`getInitialSystemMessage',
;;     `getCurrentTools', `getCurrentSystemMessage', `resolveTranscript'),
;;     `src/utils/estimate.ts' (`estimateContextTokens'),
;;     `src/api/simple-options.ts' (`buildBaseOptions' and the thinking
;;     budget helpers), the JSON-schema half of
;;     `src/api/constrained-sampling.ts', and `appendAssistantMessage
;;     Diagnostic' of `src/utils/diagnostics.ts'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'superchat-bridge-types)
(require 'superchat-bridge-event-stream)
(require 'superchat-bridge-models)
(require 'superchat-bridge-http)
(require 'superchat-bridge-utils-abort)
(require 'superchat-bridge-utils-retry)
(require 'superchat-bridge-utils-provider-retry)
(require 'superchat-bridge-utils-error-body)
(require 'superchat-bridge-utils-headers)
(require 'superchat-bridge-utils-json-parse)
(require 'superchat-bridge-utils-provider-env)
(require 'superchat-bridge-utils-sanitize-unicode)
(require 'superchat-bridge-utils-text)
(require 'superchat-bridge-api-transform-messages)

;;; Small helpers

(defun superchat-bridge-am--now-ms ()
  "Return the current time in milliseconds, like `Date.now()'."
  (floor (* 1000 (float-time))))

(defun superchat-bridge-am--obj (&rest keys-and-values)
  "Return a new alist from KEYS-AND-VALUES, symbol keys in order."
  (let (result)
    (while keys-and-values
      (push (cons (car keys-and-values) (cadr keys-and-values)) result)
      (setq keys-and-values (cddr keys-and-values)))
    (nreverse result)))

(defun superchat-bridge-am--put (alist key value)
  "Set KEY to VALUE in ALIST and return it, as `obj[key] = value'.
An existing key keeps its place, a new one goes last.  ALIST is changed,
unless it is nil: the new alist is then returned."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell value) alist)
      (nconc alist (list (cons key value))))))

(defun superchat-bridge-am--spread (&rest alists)
  "Return `{...a, ...b}' of ALISTS: a key keeps its first place."
  (let (result)
    (dolist (alist alists)
      (dolist (entry alist)
        (setq result (superchat-bridge-am--put result (car entry) (cdr entry)))))
    result))

(defun superchat-bridge-am--truthy (value)
  "Return non-nil when VALUE is truthy in JavaScript, for a Lisp VALUE.
nil, `:null', `:false', `:undefined', the empty string and 0 are not."
  (not (or (null value) (memq value '(:null :false :undefined))
           (equal value "") (eql value 0) (eql value 0.0))))

(defun superchat-bridge-am--jget (object key)
  "Return the member KEY of OBJECT, a parsed JSON hash table, or nil.
Null and false read as nil, as `??' and `||' read them in the source."
  (and (hash-table-p object)
       (let ((value (gethash key object)))
         (and (not (memq value '(:null :false))) value))))

(defun superchat-bridge-am--json-key (key)
  "Return KEY, a symbol, keyword or string, as the symbol for an object key."
  (cond ((keywordp key) (intern (substring (symbol-name key) 1)))
        ((symbolp key) key)
        (t (intern (format "%s" key)))))

(defun superchat-bridge-am--alist-like-p (value)
  "Return non-nil when VALUE is a non-empty alist with symbol or string keys."
  (and (consp value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (and (symbolp (car entry)) (car entry)
                                 (not (memq (car entry) '(:null :false))))
                            (stringp (car entry)))))
                 value)))

(defun superchat-bridge-am--plist-like-p (value)
  "Return non-nil when VALUE is a non-empty plist with keyword keys."
  (and (consp value) (keywordp (car value)) (cl-evenp (length value))
       (not (memq (car value) '(:null :false)))
       (cl-loop for (key) on value by #'cddr always (keywordp key))))

(defun superchat-bridge-am--json (value)
  "Return VALUE, a caller's data, in the form `json-serialize' takes.
Hash tables, alists and plists become alists with symbol keys, vectors
stay arrays, a list that is no object becomes a vector, and nil stays
nil, which is `{}'.  The result is always a new structure."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (push (cons (superchat-bridge-am--json-key key)
                             (superchat-bridge-am--json item))
                       result))
               value)
      (nreverse result)))
   ((vectorp value) (vconcat (mapcar #'superchat-bridge-am--json value)))
   ((superchat-bridge-am--alist-like-p value)
    (mapcar (lambda (entry)
              (cons (superchat-bridge-am--json-key (car entry))
                    (superchat-bridge-am--json (cdr entry))))
            value))
   ((superchat-bridge-am--plist-like-p value)
    (cl-loop for (key item) on value by #'cddr
             collect (cons (superchat-bridge-am--json-key key)
                           (superchat-bridge-am--json item))))
   ((consp value) (vconcat (mapcar #'superchat-bridge-am--json value)))
   (t value)))

(defun superchat-bridge-am--serialize (value)
  "Return VALUE, Lisp data of the file commentary, as JSON text."
  (let ((text (json-serialize (superchat-bridge-am--json value)
                              :null-object :null :false-object :false)))
    (if (multibyte-string-p text) text (decode-coding-string text 'utf-8))))

(defun superchat-bridge-am--member (object name)
  "Return the member NAME, a string, of OBJECT: an alist, plist or hash table."
  (cond ((hash-table-p object) (gethash name object))
        ((superchat-bridge-am--plist-like-p object)
         (plist-get object (intern (concat ":" name))))
        ((consp object)
         (cdr (or (assq (intern name) object) (assoc name object))))))

(defun superchat-bridge-am--blank-p (text)
  "Return non-nil when TEXT, a string or nil, is empty after `trim'."
  (or (null text) (string-empty-p (superchat-bridge-string-trim-js text))))

(defun superchat-bridge-am--compat-entry (model key)
  "Return (KEY . VALUE) of MODEL's own `compat' for KEY, or nil if absent.
A `:null' value counts as absent, as `??' reads it."
  (let ((entry (assq key (superchat-bridge-model-compat model))))
    (and entry (not (eq (cdr entry) :null)) entry)))

(defun superchat-bridge-am--compat-or (model key default)
  "Return `model.compat?.KEY ?? DEFAULT'."
  (let ((entry (superchat-bridge-am--compat-entry model key)))
    (if entry (cdr entry) default)))

(defun superchat-bridge-am--compat-true-p (model key)
  "Return non-nil when `model.compat?.KEY === true'."
  (eq (cdr (superchat-bridge-am--compat-entry model key)) t))

(defun superchat-bridge-am--opt-false-p (value)
  "Return non-nil when an option VALUE is the source's `false'."
  (memq value '(nil :false)))

;;; Diagnostics (src/utils/diagnostics.ts)

(defun superchat-bridge-am--append-diagnostic (message diagnostic)
  "Append DIAGNOSTIC, a plist, to the diagnostics of MESSAGE.
The source's `appendAssistantMessageDiagnostic'."
  (setf (superchat-bridge-assistant-message-diagnostics message)
        (append (superchat-bridge-assistant-message-diagnostics message)
                (list diagnostic))))

;;; Transcript helpers (src/utils/transcript.ts)

(defun superchat-bridge-am--system-messages (messages)
  "Return the system messages among MESSAGES."
  (seq-filter #'superchat-bridge-system-message-p messages))

(defun superchat-bridge-am--tool-name (tool)
  "Return the name of TOOL, a tool or a tool reference."
  (if (superchat-bridge-tool-p tool)
      (superchat-bridge-tool-name tool)
    (superchat-bridge-tool-reference-name tool)))

(defun superchat-bridge-am--get-initial-system-message (messages)
  "Return the first of MESSAGES when it is a system message.
The source's `getInitialSystemMessage'."
  (and messages (superchat-bridge-system-message-p (car messages)) (car messages)))

(defun superchat-bridge-am--get-current-tools (messages)
  "Return the tools in force after every system message of MESSAGES.
The source's `getCurrentTools': removals, then additions, in order;
a re-added name keeps its place."
  (let (tools)
    (dolist (message (superchat-bridge-am--system-messages messages))
      (dolist (removed (superchat-bridge-system-message-tools-removed message))
        (setq tools (assoc-delete-all (superchat-bridge-am--tool-name removed) tools
                                      #'equal)))
      (dolist (tool (superchat-bridge-system-message-tools-added message))
        (let ((cell (assoc (superchat-bridge-tool-name tool) tools)))
          (if cell
              (setcdr cell tool)
            (setq tools (append tools
                                (list (cons (superchat-bridge-tool-name tool) tool))))))))
    (mapcar #'cdr tools)))

(defun superchat-bridge-am--get-current-system-message (messages)
  "Return one system message holding the current prompt and tools, or nil.
The source's `getCurrentSystemMessage'."
  (let (content sections timestamp seen)
    (dolist (message (superchat-bridge-am--system-messages messages))
      (unless seen
        (setq seen t timestamp (superchat-bridge-message-timestamp message)))
      (let ((text (superchat-bridge-content-text (superchat-bridge-message-content message))))
        (when (> (length text) 0) (push text content)))
      (dolist (section (superchat-bridge-system-message-sections message))
        (if (null (cdr section))
            (setq sections (assoc-delete-all (car section) sections #'equal))
          (let ((cell (assoc (car section) sections)))
            (if cell
                (setcdr cell (cdr section))
              (setq sections (append sections (list (cons (car section)
                                                          (cdr section))))))))))
    (let ((tools (superchat-bridge-am--get-current-tools messages)))
      (when (or seen tools)
        (superchat-bridge-make-system-message
         :content (string-join (nreverse content) "\n\n")
         :sections sections
         :tools-added tools
         :timestamp (or timestamp 0))))))

(defun superchat-bridge-am--resolve-transcript (context supports-mid-convo-system-messages)
  "Return CONTEXT, or one with its system messages collapsed into one.
The source's `resolveTranscript' (with `collapseSystemMessages')."
  (if supports-mid-convo-system-messages
      context
    (let* ((messages (superchat-bridge-context-messages context))
           (head (superchat-bridge-am--get-current-system-message messages))
           (rest (seq-remove #'superchat-bridge-system-message-p messages)))
      (superchat-bridge-make-context :messages (if head (cons head rest) rest)))))

;;; Estimating context tokens (src/utils/estimate.ts)

(defun superchat-bridge-am--estimate-text-tokens (text)
  "Return the estimated tokens of TEXT: `estimateTextTokens'."
  (ceiling (/ (float (superchat-bridge-utf16-length text)) 3.5)))

(defun superchat-bridge-am--content-chars (content)
  "Return the estimated characters of CONTENT, a string or text and images."
  (if (stringp content)
      (superchat-bridge-utf16-length content)
    (let ((chars 0))
      (dolist (block content)
        (setq chars (+ chars (if (superchat-bridge-text-content-p block)
                                 (superchat-bridge-utf16-length
                                  (or (superchat-bridge-text-content-text block) ""))
                               4800))))
      chars)))

(defun superchat-bridge-am--estimate-tools-tokens (tools)
  "Return the estimated tokens of TOOLS: `estimateToolsTokens'."
  (if (null tools)
      0
    (superchat-bridge-am--estimate-text-tokens
     (superchat-bridge-am--serialize
      (vconcat (mapcar (lambda (tool)
                         (if (superchat-bridge-tool-p tool)
                             (superchat-bridge-am--obj
                              'name (superchat-bridge-tool-name tool)
                              'description (superchat-bridge-tool-description tool)
                              'parameters (superchat-bridge-tool-parameters tool))
                           (superchat-bridge-am--obj
                            'name (superchat-bridge-am--tool-name tool))))
                       tools))))))

(defun superchat-bridge-am--estimate-message-tokens (message)
  "Return the estimated tokens of MESSAGE: `estimateMessageTokens'."
  (cond
   ((superchat-bridge-system-message-p message)
    (+ (superchat-bridge-am--estimate-text-tokens
        (superchat-bridge-system-message-text message))
       (superchat-bridge-am--estimate-tools-tokens
        (superchat-bridge-system-message-tools-added message))
       (superchat-bridge-am--estimate-tools-tokens
        (superchat-bridge-system-message-tools-removed message))))
   ((or (superchat-bridge-user-message-p message)
        (superchat-bridge-tool-result-message-p message))
    (ceiling (/ (float (superchat-bridge-am--content-chars
                        (superchat-bridge-message-content message)))
                3.5)))
   (t
    (let ((chars 0))
      (dolist (block (superchat-bridge-message-content message))
        (setq chars
              (+ chars
                 (cond ((superchat-bridge-text-content-p block)
                        (superchat-bridge-utf16-length
                         (or (superchat-bridge-text-content-text block) "")))
                       ((superchat-bridge-thinking-content-p block)
                        (superchat-bridge-utf16-length
                         (or (superchat-bridge-thinking-content-thinking block) "")))
                       (t (+ (superchat-bridge-utf16-length
                              (or (superchat-bridge-tool-call-name block) ""))
                             (superchat-bridge-utf16-length
                              (superchat-bridge-safe-json-stringify
                               (superchat-bridge-tool-call-arguments block)))))))))
      (ceiling (/ (float chars) 3.5))))))

(defun superchat-bridge-am--context-tokens (usage)
  "Return the tokens USAGE reports: `calculateContextTokens'."
  (let ((total (superchat-bridge-usage-total-tokens usage)))
    (if (and total (/= total 0))
        total
      (+ (superchat-bridge-usage-input usage) (superchat-bridge-usage-output usage)
         (superchat-bridge-usage-cache-read usage)
         (superchat-bridge-usage-cache-write usage)))))

(defun superchat-bridge-am--estimate-context-tokens (context)
  "Return the estimated tokens of CONTEXT: `estimateContextTokens' `.tokens'."
  (let* ((messages (superchat-bridge-context-messages context))
         (latest-prefix-timestamp nil)
         (info nil)
         (index 0))
    (dolist (message messages)
      (when (superchat-bridge-assistant-message-p message)
        (let ((usage (superchat-bridge-assistant-message-usage message)))
          (when (and (or (null latest-prefix-timestamp)
                         (>= (or (superchat-bridge-message-timestamp message) 0)
                             latest-prefix-timestamp))
                     (not (member (superchat-bridge-assistant-message-stop-reason message)
                                  '("aborted" "error")))
                     usage
                     (> (superchat-bridge-am--context-tokens usage) 0))
            (setq info (cons usage index)))))
      (setq latest-prefix-timestamp
            (max (or latest-prefix-timestamp (- (expt 2 60)))
                 (or (superchat-bridge-message-timestamp message) 0)))
      (setq index (1+ index)))
    (if info
        (+ (superchat-bridge-am--context-tokens (car info))
           (apply #'+ (mapcar #'superchat-bridge-am--estimate-message-tokens
                              (nthcdr (1+ (cdr info)) messages))))
      (apply #'+ (mapcar #'superchat-bridge-am--estimate-message-tokens messages)))))

;;; Simple options (src/api/simple-options.ts)

(defconst superchat-bridge-am--min-answer-tokens 1024
  "`MIN_ANSWER_TOKENS': tokens always left for the answer.")

(defconst superchat-bridge-am--default-thinking-budgets
  '(("minimal" . 1024) ("low" . 2048) ("medium" . 8192) ("high" . 16384))
  "`DEFAULT_THINKING_BUDGETS'.")

(defun superchat-bridge-am--clamp-max-tokens-to-context (model context max-tokens)
  "Return MAX-TOKENS held to what MODEL's window has left after CONTEXT.
The source's `clampMaxTokensToContext'."
  (let ((window (or (superchat-bridge-model-context-window model) 0)))
    (if (<= window 0)
        (max 1 max-tokens)
      (min max-tokens
           (max 1 (- window (superchat-bridge-am--estimate-context-tokens context)
                     4096))))))

(defun superchat-bridge-am--key-name (key)
  "Return KEY, a string or symbol, as a name string."
  (cond ((stringp key) key)
        ((keywordp key) (substring (symbol-name key) 1))
        (t (symbol-name key))))

(defun superchat-bridge-am--merge-params (&rest param-lists)
  "Merge PARAM-LISTS, alists of (NAME . VALUE), later over earlier.
`{...a, ...b}': a name keeps the place of its first appearance."
  (let (result)
    (dolist (params param-lists)
      (dolist (entry params)
        (let* ((name (superchat-bridge-am--key-name (car entry)))
               (cell (assoc name result)))
          (if cell
              (setcdr cell (cdr entry))
            (setq result (append result (list (cons name (cdr entry)))))))))
    result))

(defun superchat-bridge-am--resolve-sampling-params (model thinking-level request-params)
  "Return the sampling parameters of MODEL at THINKING-LEVEL, or nil.
REQUEST-PARAMS win per name.  The source's `resolveSamplingParams'."
  (let* ((effective (superchat-bridge-clamp-thinking-level model thinking-level))
         (by-level (cdr (assoc effective
                               (superchat-bridge-model-sampling-params-by-thinking-level
                                model))))
         (model-params (superchat-bridge-model-sampling-params model)))
    (when (or model-params by-level request-params)
      (superchat-bridge-am--merge-params model-params by-level request-params))))

(defun superchat-bridge-am--build-base-options (model context options api-key)
  "Return the stream options of the simple OPTIONS: `buildBaseOptions'.
MODEL and CONTEXT size `:max-tokens'; API-KEY wins over the options'."
  (list :temperature (plist-get options :temperature)
        :sampling-params (superchat-bridge-am--resolve-sampling-params
                          model (or (plist-get options :reasoning) "off")
                          (plist-get options :sampling-params))
        :max-tokens (superchat-bridge-am--clamp-max-tokens-to-context
                     model context
                     (or (plist-get options :max-tokens)
                         (superchat-bridge-model-max-tokens model)))
        :signal (plist-get options :signal)
        :api-key (if (superchat-bridge-am--truthy api-key)
                     api-key
                   (plist-get options :api-key))
        :cache-retention (plist-get options :cache-retention)
        :session-id (plist-get options :session-id)
        :headers (plist-get options :headers)
        :on-payload (plist-get options :on-payload)
        :on-response (plist-get options :on-response)
        :on-provider-stream-event (plist-get options :on-provider-stream-event)
        :timeout-ms (plist-get options :timeout-ms)
        :max-retries (plist-get options :max-retries)
        :max-retry-delay-ms (plist-get options :max-retry-delay-ms)
        :metadata (plist-get options :metadata)
        :env (plist-get options :env)))

(defun superchat-bridge-am--thinking-budget-for-level (level custom-budgets)
  "Return the thinking budget of LEVEL, CUSTOM-BUDGETS over the defaults.
The source's `thinkingBudgetForLevel' with `clampReasoning'."
  (let* ((level (if (member level '("xhigh" "max")) "high" level))
         (custom (and custom-budgets
                      (pcase level
                        ("minimal" (superchat-bridge-thinking-budgets-minimal custom-budgets))
                        ("low" (superchat-bridge-thinking-budgets-low custom-budgets))
                        ("medium" (superchat-bridge-thinking-budgets-medium custom-budgets))
                        ("high" (superchat-bridge-thinking-budgets-high custom-budgets))))))
    (or custom (cdr (assoc level superchat-bridge-am--default-thinking-budgets)))))

(defun superchat-bridge-am--adjust-max-tokens-for-thinking
    (base-max-tokens model-max-tokens level custom-budgets)
  "Return (MAX-TOKENS . THINKING-BUDGET): `adjustMaxTokensForThinking'.
BASE-MAX-TOKENS nil means no cap of the caller's."
  (let* ((budget (superchat-bridge-am--thinking-budget-for-level level custom-budgets))
         (max-tokens (if (null base-max-tokens)
                         model-max-tokens
                       (min (+ base-max-tokens budget) model-max-tokens))))
    (when (<= max-tokens budget)
      ;; `clampThinkingBudgetToAnswerRoom'
      (setq budget (min budget (max 0 (- max-tokens
                                         superchat-bridge-am--min-answer-tokens)))))
    (cons max-tokens budget)))

;;; Constrained sampling, JSON schema (src/api/constrained-sampling.ts)

(define-error 'superchat-bridge-am--unsupported-strict-schema
  "Unsupported strict JSON schema")

(defconst superchat-bridge-am--unsupported-strict-schema-keys
  '($ref $defs definitions allOf oneOf patternProperties dependentSchemas
         dependencies unevaluatedProperties propertyNames contains prefixItems
         not if then else)
  "`UNSUPPORTED_STRICT_SCHEMA_KEYS'.")

(defun superchat-bridge-am--unsupported (format-string &rest args)
  "Signal `UnsupportedStrictJsonSchemaError' with FORMAT-STRING and ARGS."
  (signal 'superchat-bridge-am--unsupported-strict-schema
          (list (apply #'format format-string args))))

(defun superchat-bridge-am--schema-object-p (value)
  "Return non-nil when VALUE, canonical JSON, is an object.
The source's `isJsonSchemaObject'."
  (and (listp value) (not (memq value '(:null :false)))))

(defun superchat-bridge-am--has (schema key)
  "Return non-nil when SCHEMA has the member KEY: `schema[key] !== undefined'."
  (and (assq key schema) t))

(defun superchat-bridge-am--get (schema key)
  "Return the member KEY of SCHEMA, an alist."
  (cdr (assq key schema)))

(defun superchat-bridge-am--structured-schema-p (schema)
  "Return non-nil when SCHEMA is an object or array schema.
The source's `isStructuredSchema'."
  (and (superchat-bridge-am--schema-object-p schema)
       (let* ((type (superchat-bridge-am--get schema 'type))
              (types (cond ((stringp type) (list type))
                           ((vectorp type) (append type nil)))))
         (or (member "object" types) (member "array" types)
             (superchat-bridge-am--has schema 'properties)
             (superchat-bridge-am--has schema 'items)))))

(defun superchat-bridge-am--schema-allows-null-p (schema)
  "Return non-nil when SCHEMA admits null: `schemaAllowsNull'."
  (and (superchat-bridge-am--schema-object-p schema)
       (let ((type (superchat-bridge-am--get schema 'type))
             (enum (superchat-bridge-am--get schema 'enum))
             (any-of (superchat-bridge-am--get schema 'anyOf)))
         (or (equal type "null")
             (and (vectorp type) (seq-contains-p type "null"))
             (and (superchat-bridge-am--has schema 'const)
                  (eq (superchat-bridge-am--get schema 'const) :null))
             (and (vectorp enum) (seq-contains-p enum :null))
             (and (vectorp any-of)
                  (seq-some #'superchat-bridge-am--schema-allows-null-p any-of))))))

(defun superchat-bridge-am--strict-node (schema check)
  "Return SCHEMA made strict: `makeJsonSchemaNodeStrict'.
SCHEMA is canonical JSON (see `superchat-bridge-am--json') and is
changed; CHECK is nil or a function (KEY VALUE) that is true for a
keyword the provider rejects."
  (unless (superchat-bridge-am--schema-object-p schema)
    (superchat-bridge-am--unsupported "boolean schemas are unsupported"))
  (dolist (key superchat-bridge-am--unsupported-strict-schema-keys)
    (when (superchat-bridge-am--has schema key)
      (superchat-bridge-am--unsupported "%s schemas are unsupported" key)))
  (when check
    (dolist (entry schema)
      (when (funcall check (symbol-name (car entry)) (cdr entry))
        (superchat-bridge-am--unsupported
         "%s: %s is unsupported" (car entry)
         (superchat-bridge-am--serialize-value (cdr entry))))))
  (when (superchat-bridge-am--has schema 'anyOf)
    (let ((any-of (superchat-bridge-am--get schema 'anyOf)))
      (unless (and (vectorp any-of) (> (length any-of) 0))
        (superchat-bridge-am--unsupported "anyOf must contain at least one schema"))
      (setf (alist-get 'anyOf schema)
            (vconcat (mapcar (lambda (variant)
                               (when (superchat-bridge-am--structured-schema-p variant)
                                 (superchat-bridge-am--unsupported
                                  "object and array unions are unsupported"))
                               (superchat-bridge-am--strict-node variant check))
                             any-of)))))
  (when (superchat-bridge-am--has schema 'items)
    (let ((items (superchat-bridge-am--get schema 'items)))
      (when (vectorp items)
        (superchat-bridge-am--unsupported "tuple schemas are unsupported"))
      (setf (alist-get 'items schema) (superchat-bridge-am--strict-node items check))))
  (let ((is-object (equal (superchat-bridge-am--get schema 'type) "object")))
    (when (and (superchat-bridge-am--has schema 'properties) (not is-object))
      (superchat-bridge-am--unsupported "properties require type object"))
    (if (not is-object)
        schema
      (when (and (superchat-bridge-am--has schema 'additionalProperties)
                 (not (eq (superchat-bridge-am--get schema 'additionalProperties) :false)))
        (superchat-bridge-am--unsupported
         "schema-valued or true additionalProperties is unsupported"))
      (when (and (superchat-bridge-am--has schema 'properties)
                 (not (superchat-bridge-am--schema-object-p
                       (superchat-bridge-am--get schema 'properties))))
        (superchat-bridge-am--unsupported "object properties must be a schema map"))
      (when (and (superchat-bridge-am--has schema 'required)
                 (let ((required (superchat-bridge-am--get schema 'required)))
                   (or (not (vectorp required))
                       (seq-some (lambda (key) (not (stringp key))) required))))
        (superchat-bridge-am--unsupported "object required must be a string array"))
      (let* ((properties (superchat-bridge-am--get schema 'properties))
             (names (mapcar (lambda (entry) (symbol-name (car entry))) properties))
             (required (append (superchat-bridge-am--get schema 'required) nil)))
        (when (seq-some (lambda (key) (not (member key names))) required)
          (superchat-bridge-am--unsupported "required contains an unknown property"))
        (dolist (entry properties)
          (let ((strict (superchat-bridge-am--strict-node (cdr entry) check)))
            (setcdr entry
                    (if (and (not (member (symbol-name (car entry)) required))
                             (not (superchat-bridge-am--schema-allows-null-p strict)))
                        (superchat-bridge-am--obj
                         'anyOf (vector strict (superchat-bridge-am--obj 'type "null")))
                      strict))))
        (setq schema (superchat-bridge-am--put schema 'required (vconcat names)))
        (superchat-bridge-am--put schema 'additionalProperties :false)))))

(defun superchat-bridge-am--serialize-value (value)
  "Return VALUE, canonical JSON, as `JSON.stringify' writes it."
  (if (and (listp value) (not (memq value '(:null :false))))
      (superchat-bridge-am--serialize value)
    (superchat-bridge-safe-json-stringify value)))

(defun superchat-bridge-am--make-strict-json-schema (schema &optional check)
  "Return a strict copy of SCHEMA, a tool's parameters: `makeStrictJsonSchema'.
CHECK is as in `superchat-bridge-am--strict-node'."
  (let ((cloned (superchat-bridge-am--json schema)))
    (unless (superchat-bridge-am--schema-object-p cloned)
      (superchat-bridge-am--unsupported "root schema must have type object"))
    (setq cloned (superchat-bridge-am--strict-node cloned check))
    (unless (equal (superchat-bridge-am--get cloned 'type) "object")
      (superchat-bridge-am--unsupported "root schema must have type object"))
    cloned))

(defun superchat-bridge-am--json-schema-sampling (tool)
  "Return the `constrained-sampling' of TOOL when it is JSON-schema sampling."
  (let ((config (superchat-bridge-tool-constrained-sampling tool)))
    (and (superchat-bridge-am--plist-like-p config)
         (equal (plist-get config :type) "json_schema")
         config)))

(defun superchat-bridge-am--resolve-strict-sampling (tool supports-strict-mode check)
  "Return t when TOOL is sent in strict mode, else nil.
The source's `resolveJsonSchemaStrictSampling'; CHECK as in
`superchat-bridge-am--strict-node'.  A tool that requires strict mode
and cannot have it signals an error."
  (let ((config (superchat-bridge-am--json-schema-sampling tool)))
    (cond
     ((null config) nil)
     (supports-strict-mode
      (condition-case err
          (progn (superchat-bridge-am--make-strict-json-schema
                  (superchat-bridge-tool-parameters tool) check)
                 t)
        (superchat-bridge-am--unsupported-strict-schema
         (if (not (equal (plist-get config :strict) "require"))
             nil
           (error "Tool \"%s\" requires JSON-schema constrained sampling, but %s."
                  (superchat-bridge-tool-name tool) (cadr err))))))
     ((equal (plist-get config :strict) "require")
      (error "Tool \"%s\" requires JSON-schema constrained sampling, but %s."
             (superchat-bridge-tool-name tool) "strict tools are unsupported"))
     (t nil))))

;;; Cache control and compat

(defun superchat-bridge-am--resolve-cache-retention (cache-retention env)
  "Return the cache retention: CACHE-RETENTION, else from ENV, else \"short\".
The source's `resolveCacheRetention'."
  (cond (cache-retention cache-retention)
        ((equal (superchat-bridge-get-provider-env-value "PI_CACHE_RETENTION" env)
                "long")
         "long")
        (t "short")))

(defun superchat-bridge-am--get-compat (model)
  "Return the compatibility settings of MODEL, a plist: `getAnthropicCompat'."
  (let ((is-open-router (or (equal (superchat-bridge-model-provider model) "openrouter")
                            (and (string-search "openrouter.ai"
                                                (or (superchat-bridge-model-base-url model) ""))
                                 t))))
    (list
     :supports-eager-tool-input-streaming
     (superchat-bridge-am--compat-or model 'supports-eager-tool-input-streaming t)
     :supports-long-cache-retention
     (superchat-bridge-am--compat-or model 'supports-long-cache-retention t)
     :send-session-affinity-headers
     (superchat-bridge-am--compat-or model 'send-session-affinity-headers is-open-router)
     :session-affinity-format
     (superchat-bridge-am--compat-or model 'session-affinity-format
                                     (and is-open-router "openrouter"))
     :supports-cache-control-on-tools
     (superchat-bridge-am--compat-or model 'supports-cache-control-on-tools t)
     :supports-temperature
     (superchat-bridge-am--compat-or model 'supports-temperature t)
     :allow-empty-signature
     (superchat-bridge-am--compat-or model 'allow-empty-signature nil)
     :supports-strict-tools
     (superchat-bridge-am--compat-or model 'supports-strict-tools nil)
     :supports-mid-convo-system-messages
     (superchat-bridge-am--compat-or model 'supports-mid-convo-system-messages nil)
     :supports-mid-convo-tool-changes
     (superchat-bridge-am--compat-or model 'supports-mid-convo-tool-changes nil))))

(defun superchat-bridge-am--get-cache-control (model cache-retention env)
  "Return the `cache_control' object for MODEL, or nil: `getCacheControl'.
CACHE-RETENTION and ENV are the options'."
  (let ((retention (superchat-bridge-am--resolve-cache-retention cache-retention env)))
    (unless (equal retention "none")
      (if (and (equal retention "long")
               (plist-get (superchat-bridge-am--get-compat model)
                          :supports-long-cache-retention))
          (superchat-bridge-am--obj 'type "ephemeral" 'ttl "1h")
        (superchat-bridge-am--obj 'type "ephemeral")))))

(defun superchat-bridge-am--fallback-models (model)
  "Return `model.compat?.allowedFallbackModels' as a list."
  (let ((value (superchat-bridge-am--compat-or model 'allowed-fallback-models nil)))
    (if (vectorp value) (append value nil) value)))

;;; Betas

(defconst superchat-bridge-am--fine-grained-tool-streaming-beta
  "fine-grained-tool-streaming-2025-05-14")
(defconst superchat-bridge-am--interleaved-thinking-beta
  "interleaved-thinking-2025-05-14")
(defconst superchat-bridge-am--server-side-fallback-beta
  "server-side-fallback-2026-07-01")
(defconst superchat-bridge-am--mid-conversation-output-config-beta
  "mid-conversation-output-config-2026-07-01")
(defconst superchat-bridge-am--thinking-binding-controls-beta
  "thinking-binding-controls-2026-08-01")
(defconst superchat-bridge-am--inline-tools-beta "inline-tools-2026-09-15")

(defun superchat-bridge-am--deferred-tool-placeholder ()
  "Return `DEFERRED_TOOL_PLACEHOLDER', a new alist each time."
  (superchat-bridge-am--obj
   'name "__pi_deferred_placeholder__"
   'description "Reserved placeholder. Never available. Never call this."
   'input_schema (superchat-bridge-am--obj 'type "object" 'properties nil 'required [])
   'defer_loading t))

(defun superchat-bridge-am--get-beta-features (model context native-tool-changes options)
  "Return the beta features for a request, a list: `getBetaFeatures'.
MODEL, CONTEXT, NATIVE-TOOL-CHANGES and OPTIONS as in the source; an
`anthropic-beta' header of the model or the options replaces them, a
nil one leaving none."
  (let ((configured :undefined))
    (dolist (headers (list (superchat-bridge-model-headers model)
                           (plist-get options :headers)))
      (dolist (entry headers)
        (when (string= (downcase (car entry)) "anthropic-beta")
          (setq configured (cdr entry)))))
    (cond
     ((memq configured '(nil :null)) nil)
     ((stringp configured)
      (seq-uniq (seq-remove #'string-empty-p
                            (mapcar #'superchat-bridge-string-trim-js
                                    (split-string configured ",")))))
     (t
      (let (features)
        (when (and (superchat-bridge-am--get-current-tools
                    (superchat-bridge-context-messages context))
                   (not (plist-get (superchat-bridge-am--get-compat model)
                                   :supports-eager-tool-input-streaming)))
          (push superchat-bridge-am--fine-grained-tool-streaming-beta features))
        (when (and (superchat-bridge-model-reasoning model)
                   (eq (plist-get options :thinking-enabled) t)
                   (not (eq (plist-get options :interleaved-thinking) :false))
                   (not (superchat-bridge-am--compat-true-p model 'force-adaptive-thinking)))
          (push superchat-bridge-am--interleaved-thinking-beta features))
        (when (superchat-bridge-am--fallback-models model)
          (push superchat-bridge-am--server-side-fallback-beta features))
        (when (superchat-bridge-am--compat-true-p model 'supports-mid-convo-effort)
          (push superchat-bridge-am--mid-conversation-output-config-beta features)
          (push superchat-bridge-am--thinking-binding-controls-beta features))
        (when native-tool-changes
          (push superchat-bridge-am--inline-tools-beta features))
        (seq-uniq (nreverse features)))))))

;;; Tools

(defconst superchat-bridge-am--strict-unsupported-keywords
  '("minimum" "maximum" "exclusiveMinimum" "exclusiveMaximum" "multipleOf"
    "maxItems" "uniqueItems" "minContains" "maxContains" "minProperties"
    "maxProperties")
  "`ANTHROPIC_STRICT_UNSUPPORTED_KEYWORDS'.")

(defconst superchat-bridge-am--strict-string-formats
  '("date-time" "time" "date" "duration" "email" "hostname" "uri" "ipv4"
    "ipv6" "uuid")
  "`ANTHROPIC_STRICT_STRING_FORMATS'.")

(defun superchat-bridge-am--strict-unsupported-keyword-p (key value)
  "Return non-nil when Anthropic's strict mode rejects KEY with VALUE.
The source's `isAnthropicStrictUnsupportedKeyword'."
  (cond ((member key superchat-bridge-am--strict-unsupported-keywords) t)
        ((equal key "minItems") (not (and (numberp value) (or (= value 0) (= value 1)))))
        ((equal key "format")
         (or (not (stringp value))
             (not (member value superchat-bridge-am--strict-string-formats))))))

(defun superchat-bridge-am--prefer-p (tool)
  "Return non-nil when TOOL's sampling is JSON schema with strict \"prefer\"."
  (equal (plist-get (superchat-bridge-am--json-schema-sampling tool) :strict) "prefer"))

(defun superchat-bridge-am--convert-tools (tools supports-eager supports-strict
                                                 cache-control disable-preferred)
  "Return TOOLS as a list of Anthropic tool alists: `convertTools'.
SUPPORTS-EAGER, SUPPORTS-STRICT, CACHE-CONTROL and DISABLE-PREFERRED are
the source's `supportsEagerToolInputStreaming', `supportsStrictTools',
`cacheControl' and `disablePreferredStrictTools'."
  (let ((last (1- (length tools))) (index 0))
    (mapcar
     (lambda (tool)
       (let* ((strict (if (and disable-preferred (superchat-bridge-am--prefer-p tool))
                          nil
                        (superchat-bridge-am--resolve-strict-sampling
                         tool supports-strict
                         #'superchat-bridge-am--strict-unsupported-keyword-p)))
              ;; `getJsonSchemaToolParameters'
              (parameters (if strict
                              (superchat-bridge-am--make-strict-json-schema
                               (superchat-bridge-tool-parameters tool))
                            (superchat-bridge-am--json
                             (superchat-bridge-tool-parameters tool))))
              (legacy (superchat-bridge-am--obj
                       'type "object"
                       'properties (superchat-bridge-am--get parameters 'properties)
                       'required (or (superchat-bridge-am--get parameters 'required) [])))
              (input-schema (if strict
                                (superchat-bridge-am--spread parameters legacy)
                              legacy))
              (result (superchat-bridge-am--obj 'name (superchat-bridge-tool-name tool))))
         (when (superchat-bridge-tool-description tool)
           (superchat-bridge-am--put result 'description
                                     (superchat-bridge-tool-description tool)))
         (when supports-eager (superchat-bridge-am--put result 'eager_input_streaming t))
         (when strict (superchat-bridge-am--put result 'strict t))
         (superchat-bridge-am--put result 'input_schema input-schema)
         (when (and cache-control (= index last))
           (superchat-bridge-am--put result 'cache_control cache-control))
         (setq index (1+ index))
         result))
     tools)))

;;; Messages

(defun superchat-bridge-am--normalize-tool-call-id (id)
  "Return ID as Anthropic's pattern and length take it: `normalizeToolCallId'."
  (let ((clean (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" id)))
    (substring clean 0 (min 64 (length clean)))))

(defun superchat-bridge-am--image-block (image)
  "Return IMAGE, an image content, as a base64 image block.
The source's `convertImageBlock'."
  (superchat-bridge-am--obj
   'type "image"
   'source (superchat-bridge-am--obj
            'type "base64"
            'media_type (superchat-bridge-image-content-mime-type image)
            'data (superchat-bridge-image-content-data image))))

(defun superchat-bridge-am--text-block (text)
  "Return the text block of TEXT, surrogates sanitized."
  (superchat-bridge-am--obj 'type "text" 'text (superchat-bridge-sanitize-surrogates text)))

(defun superchat-bridge-am--convert-content-blocks (content)
  "Return CONTENT, text and images, as Anthropic content.
The source's `convertContentBlocks'.  Text alone is one string, the
texts joined by newlines."
  (if (not (seq-some #'superchat-bridge-image-content-p content))
      (superchat-bridge-sanitize-surrogates
       (mapconcat (lambda (block) (or (superchat-bridge-text-content-text block) ""))
                  content "\n"))
    (let ((blocks (mapcar (lambda (block)
                            (if (superchat-bridge-text-content-p block)
                                (superchat-bridge-am--text-block
                                 (superchat-bridge-text-content-text block))
                              (superchat-bridge-am--image-block block)))
                          content)))
      (unless (seq-some #'superchat-bridge-text-content-p content)
        (push (superchat-bridge-am--obj 'type "text" 'text "(see attached image)") blocks))
      (vconcat blocks))))

(defun superchat-bridge-am--convert-tool-result (msg &optional content)
  "Return the `tool_result' block of MSG, with CONTENT or its own content."
  (superchat-bridge-am--obj
   'type "tool_result"
   'tool_use_id (superchat-bridge-tool-result-message-tool-call-id msg)
   'content (superchat-bridge-am--convert-content-blocks
             (or content (superchat-bridge-message-content msg)))
   'is_error (if (superchat-bridge-tool-result-message-is-error msg) t :false)))

(defun superchat-bridge-am--effort-p (value)
  "Return non-nil when VALUE is an Anthropic effort: `isAnthropicEffort'."
  (member value '("low" "medium" "high" "xhigh" "max")))

(defun superchat-bridge-am--convert-assistant (msg allow-empty-signature)
  "Return the content blocks of MSG, an assistant message, as a list.
The assistant branch of `convertMessages'; ALLOW-EMPTY-SIGNATURE as there."
  (let (blocks)
    (dolist (block (superchat-bridge-message-content msg))
      (cond
       ((superchat-bridge-text-content-p block)
        (unless (superchat-bridge-am--blank-p (superchat-bridge-text-content-text block))
          (push (superchat-bridge-am--text-block (superchat-bridge-text-content-text block))
                blocks)))
       ((superchat-bridge-thinking-content-p block)
        (let* ((signature (superchat-bridge-thinking-content-thinking-signature block))
               (has-signature (not (superchat-bridge-am--blank-p signature)))
               (thinking (or (superchat-bridge-thinking-content-thinking block) "")))
          (cond
           ;; Redacted thinking: pass the opaque payload back as redacted_thinking.
           ((superchat-bridge-thinking-content-redacted block)
            (push (superchat-bridge-am--obj 'type "redacted_thinking" 'data signature)
                  blocks))
           ((and (superchat-bridge-am--blank-p thinking) (not has-signature)))
           ;; A missing or empty signature (e.g. from an aborted stream) becomes
           ;; plain text, unless the model accepts empty signatures.
           ((not has-signature)
            (push (if allow-empty-signature
                      (superchat-bridge-am--obj
                       'type "thinking"
                       'thinking (superchat-bridge-sanitize-surrogates thinking)
                       'signature "")
                    (superchat-bridge-am--text-block thinking))
                  blocks))
           (t
            (push (superchat-bridge-am--obj
                   'type "thinking"
                   'thinking (superchat-bridge-sanitize-surrogates thinking)
                   'signature signature)
                  blocks)))))
       ((superchat-bridge-tool-call-p block)
        (push (superchat-bridge-am--obj
               'type "tool_use"
               'id (superchat-bridge-tool-call-id block)
               'name (superchat-bridge-tool-call-name block)
               'input (or (superchat-bridge-tool-call-arguments block)
                          (superchat-bridge-json-object)))
              blocks))))
    (nreverse blocks)))

(defun superchat-bridge-am--convert-system (msg convert-tool-definitions)
  "Return the blocks of MSG, a later system message, as a list.
The system branch of `convertMessages'.  CONVERT-TOOL-DEFINITIONS is
nil when tool changes are not native."
  (let ((text (superchat-bridge-render-system-message-update msg))
        blocks)
    (when (> (length text) 0)
      (push (superchat-bridge-am--text-block text) blocks))
    (when convert-tool-definitions
      (let* ((added (superchat-bridge-system-message-tools-added msg))
             (redefined (mapcar #'superchat-bridge-tool-name added)))
        (dolist (tool (superchat-bridge-system-message-tools-removed msg))
          ;; A new definition under the same name replaces the old one.
          (unless (member (superchat-bridge-am--tool-name tool) redefined)
            (push (superchat-bridge-am--obj
                   'type "tool_removal"
                   'tool (superchat-bridge-am--obj
                          'type "tool_reference"
                          'name (superchat-bridge-am--tool-name tool)))
                  blocks)))
        (dolist (definition (funcall convert-tool-definitions added))
          (push (superchat-bridge-am--obj
                 'type "tool_addition"
                 'tool (superchat-bridge-am--obj 'type "tool_definition"
                                                 'definition definition))
                blocks))))
    (nreverse blocks)))

(defun superchat-bridge-am--convert-messages (messages cache-control allow-empty-signature
                                                       managed-provider
                                                       convert-tool-definitions)
  "Return (PARAMS . ASSISTANT-LEVELS) for MESSAGES: `convertMessages'.
MESSAGES are transformed; PARAMS is a list of message alists whose
`content' is a string or a vector of blocks; ASSISTANT-LEVELS an alist
\(INDEX . EFFORT).  The other arguments are as in the source (OAuth is
not ported)."
  (let ((items (vconcat messages))
        (params nil) (levels nil) (pending nil) (i 0))
    (cl-flet ((add (message) (setq params (append params (list message))))
              (flush () (setq params (append params pending) pending nil)))
      (while (< i (length items))
        (let ((msg (aref items i)))
          (cond
           ((superchat-bridge-system-message-p msg)
            ;; Held back until the next assistant message: tool_result blocks
            ;; must follow their tool_use at once.
            (let ((blocks (superchat-bridge-am--convert-system
                           msg convert-tool-definitions)))
              (when blocks
                (setq pending (append pending
                                      (list (superchat-bridge-am--obj
                                             'role "system"
                                             'content (vconcat blocks))))))))
           ((superchat-bridge-user-message-p msg)
            (let ((content (superchat-bridge-message-content msg)))
              (if (stringp content)
                  (unless (superchat-bridge-am--blank-p content)
                    (add (superchat-bridge-am--obj
                          'role "user"
                          'content (superchat-bridge-sanitize-surrogates content))))
                (let ((blocks (delq nil
                                    (mapcar
                                     (lambda (item)
                                       (if (superchat-bridge-text-content-p item)
                                           (unless (superchat-bridge-am--blank-p
                                                    (superchat-bridge-text-content-text item))
                                             (superchat-bridge-am--text-block
                                              (superchat-bridge-text-content-text item)))
                                         (superchat-bridge-am--image-block item)))
                                     content))))
                  (when blocks
                    (add (superchat-bridge-am--obj 'role "user"
                                                   'content (vconcat blocks))))))))
           ((superchat-bridge-assistant-message-p msg)
            (flush)
            (let ((blocks (superchat-bridge-am--convert-assistant msg allow-empty-signature)))
              (when blocks
                (let ((index (length params)))
                  (add (superchat-bridge-am--obj 'role "assistant" 'content (vconcat blocks)))
                  (when (and managed-provider
                             (equal (superchat-bridge-assistant-message-api msg)
                                    "anthropic-messages")
                             (equal (superchat-bridge-assistant-message-provider msg)
                                    managed-provider)
                             (superchat-bridge-am--effort-p
                              (superchat-bridge-assistant-message-provider-thinking-level
                               msg)))
                    (push (cons index (superchat-bridge-assistant-message-provider-thinking-level
                                       msg))
                          levels))))))
           ((superchat-bridge-tool-result-message-p msg)
            ;; All consecutive tool results go in one user message; the images of
            ;; error results follow them all (#10794).
            (let ((results nil) (images nil) (j i))
              (while (and (< j (length items))
                          (superchat-bridge-tool-result-message-p (aref items j)))
                (let* ((tool-msg (aref items j))
                       (split (superchat-bridge-split-error-tool-result-images tool-msg)))
                  (push (superchat-bridge-am--convert-tool-result
                         tool-msg (and split (superchat-bridge-error-tool-result-split-content
                                              split)))
                        results)
                  (when split
                    (push (superchat-bridge-am--obj
                           'type "text"
                           'text (superchat-bridge-error-tool-result-split-caption split))
                          images)
                    (dolist (image (superchat-bridge-error-tool-result-split-images split))
                      (push (superchat-bridge-am--image-block image) images))))
                (setq j (1+ j)))
              (setq i (1- j))
              (add (superchat-bridge-am--obj
                    'role "user"
                    'content (vconcat (nreverse results) (nreverse images))))))))
        (setq i (1+ i)))
      (flush))
    ;; Cache the conversation history: cache_control on the last user or
    ;; system message.
    (when (and cache-control params)
      (let* ((last (car (last params)))
             (content (alist-get 'content last)))
        (when (member (alist-get 'role last) '("user" "system"))
          (cond
           ((and (vectorp content) (> (length content) 0))
            (let ((block (aref content (1- (length content)))))
              (when (member (alist-get 'type block)
                            '("text" "image" "tool_result" "tool_addition"
                              "tool_removal"))
                (superchat-bridge-am--put block 'cache_control cache-control))))
           ((stringp content)
            (setf (alist-get 'content last)
                  (vector (superchat-bridge-am--obj 'type "text" 'text content
                                                    'cache_control cache-control))))))))
    (cons params levels)))

(defun superchat-bridge-am--insert-thinking-level-messages (converted active-effort)
  "Return the messages of CONVERTED with effort markers.
The source's `insertThinkingLevelMessages'.  CONVERTED is
(PARAMS . LEVELS); ACTIVE-EFFORT is the current effort."
  (let ((index 0) (messages nil))
    (dolist (message (car converted))
      (let ((historical (cdr (assq index (cdr converted)))))
        (when historical
          (push (superchat-bridge-am--marker historical) messages)))
      (push message messages)
      (setq index (1+ index)))
    (push (superchat-bridge-am--marker active-effort) messages)
    (nreverse messages)))

(defun superchat-bridge-am--marker (effort)
  "Return the system message that sets EFFORT from this point on."
  (superchat-bridge-am--obj 'role "system" 'content []
                            'output_config (superchat-bridge-am--obj 'effort effort)))

;;; Request body

(cl-defstruct (superchat-bridge-am--built
               (:constructor superchat-bridge-am--make-built)
               (:copier nil))
  "What `buildParams' returns."
  params has-preferred-strict-tools provider-thinking-level)

(defun superchat-bridge-am--build-params (model context options disable-preferred)
  "Return the request of MODEL for CONTEXT: `buildParams'.
OPTIONS is the stream's plist; DISABLE-PREFERRED is
`disablePreferredStrictTools'.  Return an `superchat-bridge-am--built'."
  (let* ((cache-control (superchat-bridge-am--get-cache-control
                         model (plist-get options :cache-retention)
                         (plist-get options :env)))
         (compat (superchat-bridge-am--get-compat model))
         (messages (superchat-bridge-context-messages context))
         (initial (superchat-bridge-am--get-initial-system-message messages))
         (initial-text (if initial (superchat-bridge-system-message-text initial) ""))
         (transformed (superchat-bridge-transform-messages
                       messages model
                       (lambda (id _model _source)
                         (superchat-bridge-am--normalize-tool-call-id id))))
         (conversation (if initial (cdr transformed) transformed))
         (has-preferred nil)
         (convert-definitions
          (lambda (tools &optional tool-cache-control)
            (let ((definitions (superchat-bridge-am--convert-tools
                                tools
                                (plist-get compat :supports-eager-tool-input-streaming)
                                (plist-get compat :supports-strict-tools)
                                tool-cache-control disable-preferred)))
              (when (cl-some (lambda (definition tool)
                               (and (eq (alist-get 'strict definition) t)
                                    (superchat-bridge-am--prefer-p tool)))
                             definitions tools)
                (setq has-preferred t))
              definitions)))
         (initial-tools (and initial (superchat-bridge-system-message-tools-added initial)))
         (native-tool-changes (and (plist-get compat :supports-mid-convo-system-messages)
                                   (plist-get compat :supports-mid-convo-tool-changes)
                                   initial-tools t))
         (thinking-disabled
          (and (superchat-bridge-model-reasoning model)
               (memq (plist-get options :thinking-enabled) '(nil :false))
               (plist-member options :thinking-enabled)
               (not (eq (cdr (assoc "off" (superchat-bridge-model-thinking-level-map model)))
                        :null))))
         (effort-messages (and (superchat-bridge-am--compat-true-p model 'supports-mid-convo-effort)
                               (not thinking-disabled)))
         (converted (superchat-bridge-am--convert-messages
                     conversation cache-control
                     (plist-get compat :allow-empty-signature)
                     (and effort-messages (superchat-bridge-model-provider model))
                     (and native-tool-changes convert-definitions)))
         (active-effort (or (plist-get options :effort) "high"))
         (betas (superchat-bridge-am--get-beta-features
                 model context native-tool-changes options))
         (params (superchat-bridge-am--obj
                  'model (superchat-bridge-model-id model)
                  'messages (vconcat
                             (if effort-messages
                                 (superchat-bridge-am--insert-thinking-level-messages
                                  converted active-effort)
                               (car converted)))
                  'max_tokens (or (plist-get options :max-tokens)
                                  (superchat-bridge-model-max-tokens model))
                  'stream t)))
    (when betas
      (superchat-bridge-am--put params 'betas (vconcat betas)))
    (when (> (length initial-text) 0)
      (superchat-bridge-am--put
       params 'system
       (vector (let ((block (superchat-bridge-am--text-block initial-text)))
                 (if cache-control
                     (superchat-bridge-am--put block 'cache_control cache-control)
                   block)))))
    ;; Temperature is incompatible with extended thinking and unsupported on
    ;; Claude Opus 4.7+.
    (when (and (plist-get options :temperature)
               (not (eq (plist-get options :thinking-enabled) t))
               (not (superchat-bridge-am--compat-true-p model 'supports-mid-convo-effort))
               (plist-get compat :supports-temperature))
      (superchat-bridge-am--put params 'temperature (plist-get options :temperature)))
    (let ((tool-cache-control (and (plist-get compat :supports-cache-control-on-tools)
                                   cache-control)))
      (if native-tool-changes
          ;; Initial tools stay active with the cache breakpoint on the last
          ;; one, then the placeholder; later tools come by value.
          (superchat-bridge-am--put
           params 'tools
           (vconcat (funcall convert-definitions initial-tools tool-cache-control)
                    (list (superchat-bridge-am--deferred-tool-placeholder))))
        (let ((tools (superchat-bridge-am--get-current-tools messages)))
          (when tools
            (superchat-bridge-am--put
             params 'tools
             (vconcat (funcall convert-definitions tools tool-cache-control)))))))
    (cond
     (thinking-disabled
      ;; No effort is sent; Anthropic rejects `block_binding' with disabled thinking.
      (superchat-bridge-am--put params 'thinking (superchat-bridge-am--obj 'type "disabled")))
     (effort-messages
      (superchat-bridge-am--put
       params 'thinking
       (superchat-bridge-am--obj
        'type "adaptive"
        'display (or (plist-get options :thinking-display) "summarized")
        'block_binding (superchat-bridge-am--obj 'prefix_mismatch_behavior "drop_block")))
      (superchat-bridge-am--put params 'output_config
                                (superchat-bridge-am--obj 'effort "high")))
     ((and (superchat-bridge-model-reasoning model)
           (eq (plist-get options :thinking-enabled) t))
      (let ((display (or (plist-get options :thinking-display) "summarized")))
        (if (superchat-bridge-am--compat-true-p model 'force-adaptive-thinking)
            (progn
              (superchat-bridge-am--put params 'thinking
                                        (superchat-bridge-am--obj 'type "adaptive"
                                                                  'display display))
              (when (plist-get options :effort)
                (superchat-bridge-am--put params 'output_config
                                          (superchat-bridge-am--obj
                                           'effort (plist-get options :effort)))))
          (superchat-bridge-am--put
           params 'thinking
           (superchat-bridge-am--obj
            'type "enabled"
            'budget_tokens (let ((budget (plist-get options :thinking-budget-tokens)))
                             (if (superchat-bridge-am--truthy budget) budget 1024))
            'display display))))))
    (let ((user-id (superchat-bridge-am--member (plist-get options :metadata) "user_id")))
      (when (stringp user-id)
        (superchat-bridge-am--put params 'metadata (superchat-bridge-am--obj 'user_id user-id))))
    (let ((tool-choice (plist-get options :tool-choice)))
      (when (superchat-bridge-am--truthy tool-choice)
        (superchat-bridge-am--put params 'tool_choice
                                  (if (stringp tool-choice)
                                      (superchat-bridge-am--obj 'type tool-choice)
                                    (superchat-bridge-am--json tool-choice)))))
    (let ((fallbacks (superchat-bridge-am--fallback-models model)))
      (when fallbacks
        (superchat-bridge-am--put
         params 'fallbacks
         (vconcat (mapcar (lambda (fallback)
                            (superchat-bridge-am--obj
                             'model (superchat-bridge-am--member fallback "model")))
                          fallbacks)))))
    (superchat-bridge-am--make-built
     :params params
     :has-preferred-strict-tools has-preferred
     :provider-thinking-level (and effort-messages active-effort))))

;;; Stop reasons

(defun superchat-bridge-am--map-stop-reason (reason stop-details)
  "Return (STOP-REASON . ERROR-MESSAGE) for REASON: `mapStopReason'.
STOP-DETAILS is the parsed `stop_details' or nil.  An unknown reason
signals an error."
  (pcase reason
    ("end_turn" (cons "stop" nil))
    ("max_tokens" (cons "length" nil))
    ("tool_use" (cons "toolUse" nil))
    ("refusal"
     (cons "error" (let ((explanation (superchat-bridge-am--jget stop-details "explanation")))
                     (if (superchat-bridge-am--truthy explanation)
                         explanation
                       "The model refused to complete the request"))))
    ;; Stop is good enough: resubmit.
    ("pause_turn" (cons "stop" nil))
    ;; No stop sequences are supplied, so this should never happen.
    ("stop_sequence" (cons "stop" nil))
    ;; Content flagged by safety filters.
    ("sensitive" (cons "error" "Provider stopped with: sensitive"))
    (_ (error "Unhandled stop reason: %s" reason))))

;;; Errors of a request

(defun superchat-bridge-am--sdk-message (status error message)
  "Return the message of the Anthropic SDK's `APIError' for these parts.
`APIError.makeMessage': ERROR is the parsed body, MESSAGE the body text
when it was no JSON."
  (let ((text (cond
               ((and (hash-table-p error)
                     (superchat-bridge-am--truthy (gethash "message" error)))
                (let ((m (gethash "message" error)))
                  (if (stringp m) m (superchat-bridge-safe-json-stringify m))))
               ((superchat-bridge-am--truthy error)
                (superchat-bridge-safe-json-stringify error))
               (t message))))
    (cond ((and status (superchat-bridge-am--truthy text)) (format "%d %s" status text))
          (status (format "%d status code (no body)" status))
          ((superchat-bridge-am--truthy text) text)
          (t "(no status code or body)"))))

(defun superchat-bridge-am--parse-or-nil (text)
  "Return TEXT parsed as JSON, or nil when it is none: the SDK's `safeJSON'."
  (condition-case nil
      (json-parse-string text :object-type 'hash-table :array-type 'array
                         :null-object :null :false-object :false)
    (error nil)))

(defun superchat-bridge-am--status-error (status headers body-text)
  "Return the provider error plist for an HTTP STATUS, HEADERS and BODY-TEXT.
It has the keys `superchat-bridge-retry-provider-request' reads and the
`:type' of the SDK's `APIError'."
  (let* ((json (and (> (length body-text) 0) (superchat-bridge-am--parse-or-nil body-text)))
         (message (if json nil body-text)))
    (list :message (superchat-bridge-am--sdk-message status json message)
          :status status :headers headers :error json
          :type (superchat-bridge-am--jget (superchat-bridge-am--jget json "error") "type"))))

(defun superchat-bridge-am--connection-error (result timed-out)
  "Return the provider error plist for RESULT, the seam's `(:error ...)'.
TIMED-OUT says the request's own timer ended it."
  (let ((kind (plist-get result :error)))
    (list :message
          (cond ((or timed-out (eq kind 'timeout)) "Request timed out.")
                ((eq kind 'aborted) "Request was aborted.")
                (t (format "Connection error: %s" (plist-get result :message))))
          :status nil :headers nil)))

(defun superchat-bridge-am--error-text (err)
  "Return the `error.message' of ERR, an error plist or a Lisp error."
  (cond ((and (consp err) (keywordp (car err)))
         (let ((message (plist-get err :message)))
           (if (stringp message) message (format "%s" message))))
        ((and (consp err) (symbolp (car err)) (stringp (cadr err)) (null (cddr err)))
         (cadr err))
        ((and (consp err) (symbolp (car err)))
         (error-message-string err))
        (t (superchat-bridge-safe-json-stringify err))))

(defconst superchat-bridge-am--strict-limit-messages
  '("The compiled grammar is too large"
    "Too many strict tools ("
    "Schemas contains too many parameters with union types (")
  "Rejections after which preferred strict tools are sent non-strict.")

;;; SSE (the source's own decoder)

(cl-defstruct (superchat-bridge-am--sse
               (:constructor superchat-bridge-am--make-sse)
               (:copier nil))
  "The state of the source's `iterateSseMessages': `SseDecoderState' and
the text not yet split into lines."
  (event nil) (data nil) (raw nil) (buffer ""))

(defun superchat-bridge-am--sse-flush (sse)
  "Return the event SSE holds as (EVENT DATA RAW), or nil: `flushSseEvent'."
  (when (or (superchat-bridge-am--sse-event sse) (superchat-bridge-am--sse-data sse))
    (prog1 (list (superchat-bridge-am--sse-event sse)
                 (string-join (reverse (superchat-bridge-am--sse-data sse)) "\n")
                 (reverse (superchat-bridge-am--sse-raw sse)))
      (setf (superchat-bridge-am--sse-event sse) nil
            (superchat-bridge-am--sse-data sse) nil
            (superchat-bridge-am--sse-raw sse) nil))))

(defun superchat-bridge-am--sse-line (sse line)
  "Take LINE into SSE; return a finished event or nil: `decodeSseLine'."
  (if (string-empty-p line)
      (superchat-bridge-am--sse-flush sse)
    (push line (superchat-bridge-am--sse-raw sse))
    (unless (eq (aref line 0) ?:)
      (let* ((colon (string-search ":" line))
             (field (if colon (substring line 0 colon) line))
             (value (if colon (substring line (1+ colon)) "")))
        (when (string-prefix-p " " value)
          (setq value (substring value 1)))
        (cond ((string= field "event") (setf (superchat-bridge-am--sse-event sse) value))
              ((string= field "data") (push value (superchat-bridge-am--sse-data sse))))))
    nil))

(defun superchat-bridge-am--sse-lines (sse)
  "Return the events of the whole lines in SSE's buffer: `consumeLine' loop."
  (let ((events nil) (buffer (superchat-bridge-am--sse-buffer sse)) (start 0))
    (while (string-match "[\r\n]" buffer start)
      (let* ((break (match-beginning 0))
             (next (if (and (eq (aref buffer break) ?\r)
                            (< (1+ break) (length buffer))
                            (eq (aref buffer (1+ break)) ?\n))
                       (+ break 2)
                     (1+ break)))
             (event (superchat-bridge-am--sse-line sse (substring buffer start break))))
        (when event (push event events))
        (setq start next)))
    (setf (superchat-bridge-am--sse-buffer sse) (substring buffer start))
    (nreverse events)))

(defun superchat-bridge-am--sse-feed (sse text)
  "Add TEXT to SSE and return the events it completes."
  (setf (superchat-bridge-am--sse-buffer sse)
        (concat (superchat-bridge-am--sse-buffer sse) text))
  (superchat-bridge-am--sse-lines sse))

(defun superchat-bridge-am--sse-finish (sse)
  "Return the events left in SSE at the end of the body."
  (let ((events (superchat-bridge-am--sse-lines sse))
        (rest (superchat-bridge-am--sse-buffer sse)))
    (when (> (length rest) 0)
      (setf (superchat-bridge-am--sse-buffer sse) "")
      (let ((event (superchat-bridge-am--sse-line sse rest)))
        (when event (setq events (append events (list event))))))
    (let ((trailing (superchat-bridge-am--sse-flush sse)))
      (when trailing (setq events (append events (list trailing)))))
    events))

(defconst superchat-bridge-am--message-events
  '("message_start" "message_delta" "message_stop" "content_block_start"
    "content_block_delta" "content_block_stop")
  "`ANTHROPIC_MESSAGE_EVENTS'.")

;;; The stream

(cl-defstruct (superchat-bridge-am--state
               (:constructor superchat-bridge-am--make-state)
               (:copier nil))
  "The state of one call of `superchat-bridge-api-anthropic-messages-stream'."
  stream model options output context current-tools
  (finished nil) handle signal listener timer
  url headers (disable-strict nil) built usage-model input-transformations
  (sse (superchat-bridge-am--make-sse)) (saw-start nil) (saw-end nil)
  (indexes (make-hash-table :test #'eq))
  (partial (make-hash-table :test #'eq)))

(defun superchat-bridge-am--emit (st type &rest keys)
  "Push an event of TYPE with `partial' and KEYS onto the stream of ST."
  (superchat-bridge-event-stream-push
   (superchat-bridge-am--state-stream st)
   (apply #'superchat-bridge-make-event :type type
          :partial (superchat-bridge-am--state-output st) keys)))

(defun superchat-bridge-am--add-block (st block index)
  "Add BLOCK, at stream INDEX, to the message of ST; return its position."
  (let ((output (superchat-bridge-am--state-output st)))
    (setf (superchat-bridge-message-content output)
          (append (superchat-bridge-message-content output) (list block)))
    (puthash block index (superchat-bridge-am--state-indexes st))
    (1- (length (superchat-bridge-message-content output)))))

(defun superchat-bridge-am--find-block (st index)
  "Return (POSITION . BLOCK) of the first open block at stream INDEX, or nil.
The source's `blocks.findIndex((b) => b.index === event.index)'."
  (let ((position 0) found)
    (dolist (block (superchat-bridge-message-content (superchat-bridge-am--state-output st)))
      (when (and (not found)
                 (eql (gethash block (superchat-bridge-am--state-indexes st) :none) index))
        (setq found (cons position block)))
      (setq position (1+ position)))
    found))

(defun superchat-bridge-am--fallback-cost (model response-model)
  "Return the cost of RESPONSE-MODEL among MODEL's allowed fallbacks, or nil."
  (let* ((fallback (seq-find (lambda (fallback)
                               (and (equal (superchat-bridge-am--member fallback "provider")
                                           (superchat-bridge-model-provider model))
                                    (equal (superchat-bridge-am--member fallback "model")
                                           response-model)))
                             (superchat-bridge-am--fallback-models model)))
         (cost (and fallback (superchat-bridge-am--member fallback "cost"))))
    (when cost
      (let ((rate (lambda (name) (or (superchat-bridge-am--member cost name) 0))))
        (superchat-bridge-make-model-cost
         :input (funcall rate "input") :output (funcall rate "output")
         :cache-read (funcall rate "cacheRead") :cache-write (funcall rate "cacheWrite"))))))

(defun superchat-bridge-am--total (usage)
  "Set `totalTokens' of USAGE from its parts: Anthropic gives none."
  (setf (superchat-bridge-usage-total-tokens usage)
        (+ (superchat-bridge-usage-input usage) (superchat-bridge-usage-output usage)
           (superchat-bridge-usage-cache-read usage)
           (superchat-bridge-usage-cache-write usage))))

(defun superchat-bridge-am--on-message-start (st event)
  "Take EVENT, a `message_start', for ST."
  (let* ((model (superchat-bridge-am--state-model st))
         (output (superchat-bridge-am--state-output st))
         (message (superchat-bridge-am--jget event "message"))
         (usage (superchat-bridge-am--jget message "usage"))
         (transformations (and (hash-table-p message)
                              (gethash "input_transformations" message)))
         (response-model (superchat-bridge-am--jget message "model"))
         (n (lambda (object key) (or (superchat-bridge-am--jget object key) 0)))
         (u (superchat-bridge-assistant-message-usage output)))
    (setf (superchat-bridge-assistant-message-response-id output)
          (superchat-bridge-am--jget message "id"))
    (when (vectorp transformations)
      (setf (superchat-bridge-am--state-input-transformations st) transformations))
    (unless (equal response-model (superchat-bridge-model-id model))
      (setf (superchat-bridge-assistant-message-response-model output) response-model))
    (let ((cost (and (not (equal response-model (superchat-bridge-model-id model)))
                     (superchat-bridge-am--fallback-cost model response-model))))
      (setf (superchat-bridge-am--state-usage-model st)
            (if cost
                (let ((copy (copy-sequence model)))
                  (setf (superchat-bridge-model-id copy) response-model
                        (superchat-bridge-model-cost copy) cost)
                  copy)
              model)))
    ;; The initial usage keeps the input tokens even if the stream is aborted.
    (setf (superchat-bridge-usage-input u) (funcall n usage "input_tokens")
          (superchat-bridge-usage-output u) (funcall n usage "output_tokens")
          (superchat-bridge-usage-cache-read u) (funcall n usage "cache_read_input_tokens")
          (superchat-bridge-usage-cache-write u) (funcall n usage "cache_creation_input_tokens")
          (superchat-bridge-usage-cache-write1h u)
          (funcall n (superchat-bridge-am--jget usage "cache_creation")
                   "ephemeral_1h_input_tokens"))
    (superchat-bridge-am--total u)
    (superchat-bridge-calculate-cost (superchat-bridge-am--state-usage-model st) u)))

(defun superchat-bridge-am--on-block-start (st event)
  "Take EVENT, a `content_block_start', for ST."
  (let* ((block (superchat-bridge-am--jget event "content_block"))
         (type (superchat-bridge-am--jget block "type"))
         (index (gethash "index" event))
         (output (superchat-bridge-am--state-output st))
         (start (lambda (new event-type)
                  (superchat-bridge-am--emit
                   st event-type
                   :content-index (superchat-bridge-am--add-block st new index)))))
    (pcase type
      ("fallback"
       (when (superchat-bridge-message-content output)
         (error "Anthropic performed an unsupported mid-output model fallback")))
      ("text"
       (funcall start (superchat-bridge-make-text-content
                       :text (or (superchat-bridge-am--jget block "text") ""))
                "text_start"))
      ("thinking"
       (funcall start (superchat-bridge-make-thinking-content
                       :thinking (or (superchat-bridge-am--jget block "thinking") "")
                       :thinking-signature (or (superchat-bridge-am--jget block "signature")
                                               ""))
                "thinking_start"))
      ("redacted_thinking"
       (funcall start (superchat-bridge-make-thinking-content
                       :thinking "[Reasoning redacted]"
                       :thinking-signature (superchat-bridge-am--jget block "data")
                       :redacted t)
                "thinking_start"))
      ("tool_use"
       (let ((call (superchat-bridge-make-tool-call
                    :id (superchat-bridge-am--jget block "id")
                    :name (superchat-bridge-am--jget block "name")
                    :arguments (or (superchat-bridge-am--jget block "input")
                                   (superchat-bridge-json-object)))))
         (puthash call "" (superchat-bridge-am--state-partial st))
         (funcall start call "toolcall_start"))))))

(defun superchat-bridge-am--on-block-delta (st event)
  "Take EVENT, a `content_block_delta', for ST."
  (let* ((delta (superchat-bridge-am--jget event "delta"))
         (found (superchat-bridge-am--find-block st (gethash "index" event)))
         (position (car found))
         (block (cdr found)))
    (pcase (superchat-bridge-am--jget delta "type")
      ("text_delta"
       (when (superchat-bridge-text-content-p block)
         (let ((text (superchat-bridge-am--jget delta "text")))
           (setf (superchat-bridge-text-content-text block)
                 (concat (superchat-bridge-text-content-text block) text))
           (superchat-bridge-am--emit st "text_delta" :content-index position :delta text))))
      ("thinking_delta"
       (when (superchat-bridge-thinking-content-p block)
         (let ((text (superchat-bridge-am--jget delta "thinking")))
           (setf (superchat-bridge-thinking-content-thinking block)
                 (concat (superchat-bridge-thinking-content-thinking block) text))
           (superchat-bridge-am--emit st "thinking_delta" :content-index position
                                      :delta text))))
      ("input_json_delta"
       (when (superchat-bridge-tool-call-p block)
         (let* ((text (superchat-bridge-am--jget delta "partial_json"))
                (partial (concat (gethash block (superchat-bridge-am--state-partial st) "")
                                 text)))
           (puthash block partial (superchat-bridge-am--state-partial st))
           (setf (superchat-bridge-tool-call-arguments block)
                 (superchat-bridge-parse-streaming-json partial))
           (superchat-bridge-am--emit st "toolcall_delta" :content-index position
                                      :delta text))))
      ("signature_delta"
       (when (superchat-bridge-thinking-content-p block)
         (setf (superchat-bridge-thinking-content-thinking-signature block)
               (concat (or (superchat-bridge-thinking-content-thinking-signature block) "")
                       (superchat-bridge-am--jget delta "signature"))))))))

(defun superchat-bridge-am--on-block-stop (st event)
  "Take EVENT, a `content_block_stop', for ST."
  (let* ((found (superchat-bridge-am--find-block st (gethash "index" event)))
         (position (car found))
         (block (cdr found)))
    (when block
      (remhash block (superchat-bridge-am--state-indexes st))
      (cond
       ((superchat-bridge-text-content-p block)
        (superchat-bridge-am--emit st "text_end" :content-index position
                                   :content (superchat-bridge-text-content-text block)))
       ((superchat-bridge-thinking-content-p block)
        (superchat-bridge-am--emit st "thinking_end" :content-index position
                                   :content (superchat-bridge-thinking-content-thinking block)))
       ((superchat-bridge-tool-call-p block)
        (let ((parsed (superchat-bridge-parse-tool-call-arguments
                       (gethash block (superchat-bridge-am--state-partial st)))))
          (setf (superchat-bridge-tool-call-arguments block) (cdr (assq 'arguments parsed)))
          (when (assq 'arguments-error parsed)
            (setf (superchat-bridge-tool-call-arguments-error block)
                  (cdr (assq 'arguments-error parsed))))
          ;; Finalized in place; the scratch buffer goes, so a replay only
          ;; carries parsed arguments.
          (remhash block (superchat-bridge-am--state-partial st))
          (superchat-bridge-am--emit st "toolcall_end" :content-index position
                                     :tool-call block)))))))

(defun superchat-bridge-am--on-message-delta (st event)
  "Take EVENT, a `message_delta', for ST."
  (let* ((output (superchat-bridge-am--state-output st))
         (u (superchat-bridge-assistant-message-usage output))
         (delta (superchat-bridge-am--jget event "delta"))
         (usage (superchat-bridge-am--jget event "usage"))
         (transformations (gethash "input_transformations" event))
         (reason (superchat-bridge-am--jget delta "stop_reason")))
    (when (vectorp transformations)
      (setf (superchat-bridge-am--state-input-transformations st) transformations))
    (when (superchat-bridge-am--truthy reason)
      (setf (superchat-bridge-assistant-message-raw-stop-reason output) reason)
      (let ((mapped (superchat-bridge-am--map-stop-reason
                     reason (superchat-bridge-am--jget delta "stop_details"))))
        (setf (superchat-bridge-assistant-message-stop-reason output) (car mapped))
        (when (cdr mapped)
          (setf (superchat-bridge-assistant-message-error-message output) (cdr mapped)))))
    ;; Only fields that are present: proxies may leave input_tokens out here.
    (when usage
      (let ((field (lambda (key) (superchat-bridge-am--jget usage key))))
        (when (funcall field "input_tokens")
          (setf (superchat-bridge-usage-input u) (funcall field "input_tokens")))
        (when (funcall field "output_tokens")
          (setf (superchat-bridge-usage-output u) (funcall field "output_tokens")))
        (when (funcall field "cache_read_input_tokens")
          (setf (superchat-bridge-usage-cache-read u) (funcall field "cache_read_input_tokens")))
        (when (funcall field "cache_creation_input_tokens")
          (setf (superchat-bridge-usage-cache-write u)
                (funcall field "cache_creation_input_tokens")))
        ;; Vercel AI Gateway puts the TTL breakdown in deltas.
        (let ((long (superchat-bridge-am--jget (funcall field "cache_creation")
                                               "ephemeral_1h_input_tokens")))
          (when long (setf (superchat-bridge-usage-cache-write1h u) long)))
        ;; Reasoning tokens are a subset of output tokens.
        (let ((thinking (superchat-bridge-am--jget (funcall field "output_tokens_details")
                                                   "thinking_tokens")))
          (when thinking (setf (superchat-bridge-usage-reasoning u) thinking)))))
    (superchat-bridge-am--total u)
    (superchat-bridge-calculate-cost (superchat-bridge-am--state-usage-model st) u)))

(defun superchat-bridge-am--on-event (st event)
  "Take EVENT, one parsed Anthropic stream event, for ST.
The body of the source's `for await (const event ...)' loop."
  (let ((hook (plist-get (superchat-bridge-am--state-options st) :on-provider-stream-event)))
    (when hook (funcall hook event (superchat-bridge-am--state-model st))))
  (pcase (superchat-bridge-am--jget event "type")
    ("message_start" (superchat-bridge-am--on-message-start st event))
    ("content_block_start" (superchat-bridge-am--on-block-start st event))
    ("content_block_delta" (superchat-bridge-am--on-block-delta st event))
    ("content_block_stop" (superchat-bridge-am--on-block-stop st event))
    ("message_delta" (superchat-bridge-am--on-message-delta st event))))

(defun superchat-bridge-am--on-sse (st sse)
  "Take SSE, an (EVENT DATA RAW) of the decoder, for ST.
The body of the source's `iterateAnthropicEvents' loop."
  (pcase-let ((`(,name ,data ,raw) sse))
    (cond
     ((equal name "error") (error "%s" data))
     ((member name superchat-bridge-am--message-events)
      (let ((event (condition-case err
                       (superchat-bridge-parse-json-with-repair data)
                     (error
                      (error "Could not parse Anthropic SSE event %s: %s; data=%s; raw=%s"
                             name (superchat-bridge-am--error-text err) data
                             (string-join raw "\\n"))))))
        (pcase (superchat-bridge-am--jget event "type")
          ("message_start" (setf (superchat-bridge-am--state-saw-start st) t))
          ("message_stop" (setf (superchat-bridge-am--state-saw-end st) t)))
        (superchat-bridge-am--on-event st event))))))

;;; Ending the stream

(defun superchat-bridge-am--cleanup (st)
  "Release what ST holds: its abort listener, timer and HTTP request."
  (let ((signal (superchat-bridge-am--state-signal st))
        (listener (superchat-bridge-am--state-listener st))
        (timer (superchat-bridge-am--state-timer st))
        (handle (superchat-bridge-am--state-handle st)))
    (when (and signal listener)
      (superchat-bridge-abort-signal-remove-listener signal listener))
    (setf (superchat-bridge-am--state-listener st) nil)
    (when timer
      (funcall superchat-bridge-retry-cancel-timer-function timer)
      (setf (superchat-bridge-am--state-timer st) nil))
    (when handle
      (setf (superchat-bridge-am--state-handle st) nil)
      (superchat-bridge-http-abort handle))))

(defun superchat-bridge-am--aborted-p (st)
  "Return non-nil when the signal of ST is aborted."
  (let ((signal (superchat-bridge-am--state-signal st)))
    (and signal (superchat-bridge-abort-signal-aborted-p signal))))

(defun superchat-bridge-am--fail (st err)
  "End the stream of ST with an `error' event for ERR: the source's `catch'.
ERR is a Lisp error condition or an error plist.  Do nothing when the
stream has ended."
  (unless (superchat-bridge-am--state-finished st)
    (setf (superchat-bridge-am--state-finished st) t)
    (superchat-bridge-am--cleanup st)
    (let ((output (superchat-bridge-am--state-output st)))
      (setf (superchat-bridge-assistant-message-stop-reason output)
            (if (superchat-bridge-am--aborted-p st) "aborted" "error"))
      (setf (superchat-bridge-assistant-message-error-message output)
            (superchat-bridge-am--error-text err))
      (superchat-bridge-event-stream-push
       (superchat-bridge-am--state-stream st)
       (superchat-bridge-make-event
        :type "error"
        :reason (superchat-bridge-assistant-message-stop-reason output)
        :error output))
      (superchat-bridge-event-stream-end (superchat-bridge-am--state-stream st)))))

(defun superchat-bridge-am--complete (st)
  "End the stream of ST normally after the body.
The source's code after its loop."
  (dolist (sse (superchat-bridge-am--sse-finish (superchat-bridge-am--state-sse st)))
    (superchat-bridge-am--on-sse st sse))
  (when (and (superchat-bridge-am--state-saw-start st)
             (not (superchat-bridge-am--state-saw-end st)))
    (error "Anthropic stream ended before message_stop"))
  (let* ((output (superchat-bridge-am--state-output st))
         (reason (superchat-bridge-assistant-message-stop-reason output))
         (transformations (superchat-bridge-am--state-input-transformations st)))
    (when (superchat-bridge-am--aborted-p st)
      (error "Request was aborted"))
    (when (equal reason "pending")
      (error "Anthropic stream ended without a stop reason"))
    (when (member reason '("aborted" "error"))
      (let ((message (superchat-bridge-assistant-message-error-message output)))
        (error "%s" (if (superchat-bridge-am--truthy message)
                        message
                      "An unknown error occurred"))))
    (when (and transformations (> (length transformations) 0))
      (superchat-bridge-am--append-diagnostic
       output
       (list :type "anthropic_input_transformations"
             :timestamp (superchat-bridge-am--now-ms)
             :details
             (list :transformations
                   (mapcar (lambda (transformation)
                             (list :type (superchat-bridge-am--jget transformation "type")
                                   :path (superchat-bridge-am--jget transformation "path")
                                   :reason (superchat-bridge-am--jget transformation
                                                                      "reason")))
                           transformations)))))
    (setf (superchat-bridge-am--state-finished st) t)
    (superchat-bridge-am--cleanup st)
    (superchat-bridge-event-stream-push
     (superchat-bridge-am--state-stream st)
     (superchat-bridge-make-event :type "done" :reason reason :message output))
    (superchat-bridge-event-stream-end (superchat-bridge-am--state-stream st))))

;;; Reading the response

(defun superchat-bridge-am--on-body (st text)
  "Take TEXT, a piece of the response body, for ST."
  (unless (superchat-bridge-am--state-finished st)
    (condition-case err
        (dolist (sse (superchat-bridge-am--sse-feed (superchat-bridge-am--state-sse st) text))
          (unless (superchat-bridge-am--state-finished st)
            (superchat-bridge-am--on-sse st sse)))
      (error (superchat-bridge-am--fail st err)))))

(defun superchat-bridge-am--on-end (st)
  "Take the end of the response body for ST."
  (unless (superchat-bridge-am--state-finished st)
    (condition-case err
        (superchat-bridge-am--complete st)
      (error (superchat-bridge-am--fail st err)))))

(defun superchat-bridge-am--attempt (st headers body done)
  "Make one request for ST with HEADERS and BODY; call DONE with its outcome.
DONE is the continuation `superchat-bridge-retry-provider-request' gives:
it gets `(:ok (STATUS HEADERS))' once the headers say success, or
`(:error PLIST)'.  The body then goes to ST.  This is the source's
`client.beta.messages.create(params, requestOptions).asResponse()'."
  (let ((timeout-ms (plist-get (superchat-bridge-am--state-options st) :timeout-ms))
        (status nil) (response-headers nil) (error-chunks nil)
        (streaming nil) (timed-out nil) (timer nil))
    (if (superchat-bridge-am--aborted-p st)
        (funcall done (list :error (list :message "Request was aborted."
                                         :status nil :headers nil)))
      (let ((stop-timer (lambda ()
                          (when timer
                            (funcall superchat-bridge-retry-cancel-timer-function timer)
                            (setq timer nil)))))
        (setf (superchat-bridge-am--state-handle st)
              (superchat-bridge-http-request
               :url (superchat-bridge-am--state-url st) :method "POST"
               :headers headers :body body
               :on-headers
               (lambda (code response)
                 (funcall stop-timer)
                 (setq status code response-headers response)
                 (when (and (>= code 200) (< code 300))
                   (setq streaming t)
                   (funcall done (list :ok (list code response)))))
               :on-chunk
               (lambda (text)
                 (if streaming
                     (superchat-bridge-am--on-body st text)
                   (push text error-chunks)))
               :on-done
               (lambda (result)
                 (funcall stop-timer)
                 (cond
                  ((plist-get result :status)
                   (if streaming
                       (superchat-bridge-am--on-end st)
                     (funcall done
                              (list :error
                                    (superchat-bridge-am--status-error
                                     (plist-get result :status)
                                     (plist-get result :headers)
                                     (apply #'concat (reverse error-chunks)))))))
                  (streaming
                   (superchat-bridge-am--fail
                    st (if (eq (plist-get result :error) 'aborted)
                           (list :message "Request was aborted")
                         (superchat-bridge-am--connection-error result timed-out))))
                  (status
                   (funcall done
                            (list :error
                                  (superchat-bridge-am--status-error
                                   status response-headers
                                   (apply #'concat (reverse error-chunks))))))
                  (t
                   (funcall done
                            (list :error (superchat-bridge-am--connection-error
                                          result timed-out))))))))
        ;; The SDK's timeout covers the time until the headers arrive.
        (when timeout-ms
          (setq timer
                (superchat-bridge--start-timer
                 timeout-ms
                 (lambda ()
                   (setq timer nil timed-out t)
                   (let ((handle (superchat-bridge-am--state-handle st)))
                     (when handle (superchat-bridge-http-abort handle)))))))))))

(defun superchat-bridge-am--headers (st params)
  "Return the headers of the request of ST for PARAMS, the body alist.
The SDK's `buildHeaders' over the client `createClient' makes."
  (let* ((model (superchat-bridge-am--state-model st))
         (options (superchat-bridge-am--state-options st))
         (api-key (plist-get options :api-key))
         (betas (alist-get 'betas params))
         (profile (alist-get 'user_profile_id params))
         (workspace (alist-get 'workspace_id params))
         (headers (superchat-bridge-provider-headers-to-record
                   '(("Accept" . "application/json")
                     ("anthropic-dangerous-direct-browser-access" . "true")
                     ("anthropic-version" . "2023-06-01"))
                   (and (superchat-bridge-am--truthy api-key)
                        (list (cons "X-Api-Key" api-key)))
                   (superchat-bridge-am--state-headers st)
                   '(("Content-Type" . "application/json"))
                   (append
                    (and betas
                         (list (cons "anthropic-beta"
                                     (mapconcat (lambda (beta) (format "%s" beta))
                                                betas ","))))
                    (and profile
                         (list (cons "anthropic-user-profile-id" (format "%s" profile))))
                    (and workspace
                         (list (cons "anthropic-workspace-id" (format "%s" workspace))))))))
    ;; The SDK's `validateHeaders': some authentication or an explicit null.
    (unless (or (assoc-string "x-api-key" headers t)
                (assoc-string "authorization" headers t)
                (seq-some (lambda (source)
                            (seq-some (lambda (entry)
                                        (and (member (downcase (car entry))
                                                     '("x-api-key" "authorization"))
                                             (memq (cdr entry) '(nil :null))))
                                      source))
                          (list (superchat-bridge-model-headers model)
                                (plist-get options :headers))))
      (error "Could not resolve authentication method. Expected one of apiKey, authToken, credentials, config, or profile to be set. Or for one of the \"X-Api-Key\" or \"Authorization\" headers to be explicitly omitted"))
    headers))

(defun superchat-bridge-am--body (params)
  "Return the JSON body of PARAMS without the keys the SDK sends as headers."
  (superchat-bridge-am--serialize
   (seq-remove (lambda (entry) (memq (car entry) '(betas user_profile_id workspace_id)))
               params)))

(defun superchat-bridge-am--fallback-p (st err)
  "Return non-nil when ERR lets ST send its preferred strict tools non-strict."
  (let ((message (plist-get err :message)))
    (and (not (superchat-bridge-am--aborted-p st))
         (not (superchat-bridge-am--state-disable-strict st))
         (superchat-bridge-am--built-has-preferred-strict-tools
          (superchat-bridge-am--state-built st))
         (eql (plist-get err :status) 400)
         (equal (plist-get err :type) "invalid_request_error")
         (stringp message)
         (seq-some (lambda (part) (string-search part message))
                   superchat-bridge-am--strict-limit-messages))))

(defun superchat-bridge-am--on-response (st result)
  "Take RESULT of the request of ST: the source's code after `await retry...'."
  (if (eq (car result) :error)
      (let ((err (cadr result)))
        (if (and (consp err) (keywordp (car err)) (superchat-bridge-am--fallback-p st err))
            ;; Only a rejected request is downgraded, never a started stream.
            (progn
              (setf (superchat-bridge-am--state-disable-strict st) t)
              (superchat-bridge-am--append-diagnostic
               (superchat-bridge-am--state-output st)
               (list :type "anthropic_strict_tools_fallback"
                     :timestamp (superchat-bridge-am--now-ms)
                     :error (list :message (plist-get err :message))
                     :details (list :reason "strict_tool_limits_exceeded")))
              (superchat-bridge-am--request st))
          (superchat-bridge-am--fail st err)))
    (condition-case err
        (let ((hook (plist-get (superchat-bridge-am--state-options st) :on-response))
              (response (cadr result)))
          (unless (superchat-bridge-am--state-finished st)
            (when hook
              (funcall hook (superchat-bridge-make-provider-response
                             :status (car response)
                             :headers (superchat-bridge-headers-to-record (cadr response)))
                       (superchat-bridge-am--state-model st)))
            (unless (superchat-bridge-am--state-finished st)
              (superchat-bridge-am--emit st "start"))))
      (error (superchat-bridge-am--fail st err)))))

(defun superchat-bridge-am--request (st)
  "Build and send the request of ST: one pass of the source's `for (;;)'."
  (condition-case err
      (let* ((model (superchat-bridge-am--state-model st))
             (options (superchat-bridge-am--state-options st))
             (signal (superchat-bridge-am--state-signal st))
             (built (progn
                      (when signal (superchat-bridge-abort-signal-throw-if-aborted signal))
                      (superchat-bridge-am--build-params
                       model (superchat-bridge-am--state-context st) options
                       (superchat-bridge-am--state-disable-strict st))))
             (params (superchat-bridge-am--built-params built))
             (hook (plist-get options :on-payload))
             (max-retries (plist-get options :max-retries))
             (max-retry-delay-ms (plist-get options :max-retry-delay-ms)))
        (setf (superchat-bridge-am--state-built st) built)
        (when (superchat-bridge-am--built-provider-thinking-level built)
          (setf (superchat-bridge-assistant-message-provider-thinking-level
                 (superchat-bridge-am--state-output st))
                (superchat-bridge-am--built-provider-thinking-level built)))
        (let ((next (and hook (funcall hook params model))))
          (when next
            (setq params (superchat-bridge-am--put (superchat-bridge-am--json next)
                                                   'stream t))))
        (let ((headers (superchat-bridge-am--headers st params))
              (body (superchat-bridge-am--body params)))
          (apply #'superchat-bridge-retry-provider-request
                 (lambda (done) (superchat-bridge-am--attempt st headers body done))
                 (lambda (result) (superchat-bridge-am--on-response st result))
                 :signal signal
                 (append (and max-retries (list :max-retries max-retries))
                         (and max-retry-delay-ms
                              (list :max-retry-delay-ms max-retry-delay-ms))))))
    (error (superchat-bridge-am--fail st err))))

(defun superchat-bridge-am--has-header-p (headers name)
  "Return non-nil when HEADERS, an alist, has a non-blank value for NAME.
The source's `hasHeader'."
  (cl-some (lambda (entry)
             (and (string= (downcase (car entry)) name)
                  (stringp (cdr entry))
                  (not (superchat-bridge-am--blank-p (cdr entry)))))
           headers))

(defun superchat-bridge-am--assert-request-auth (provider api-key headers)
  "Signal unless API-KEY or HEADERS authenticate PROVIDER: `assertRequestAuth'."
  (unless (or (superchat-bridge-am--truthy api-key)
              (superchat-bridge-am--has-header-p headers "authorization")
              (superchat-bridge-am--has-header-p headers "x-api-key")
              (superchat-bridge-am--has-header-p headers "cf-aig-authorization"))
    (error "No API key for provider: %s" provider)))

(defun superchat-bridge-am--client-headers (model options)
  "Return the default headers `createClient' gives for MODEL and OPTIONS.
The API key branch; the Copilot and OAuth branches are not ported and
signal an error."
  (let* ((api-key (plist-get options :api-key))
         (compat (superchat-bridge-am--get-compat model))
         (retention (superchat-bridge-am--resolve-cache-retention
                     (plist-get options :cache-retention) (plist-get options :env)))
         (session-id (and (not (equal retention "none")) (plist-get options :session-id))))
    (when (equal (superchat-bridge-model-provider model) "github-copilot")
      (error "GitHub Copilot is not supported by the Anthropic Messages port"))
    (when (and (stringp api-key) (string-search "sk-ant-oat" api-key))
      (error "Anthropic OAuth tokens are not supported by the Anthropic Messages port"))
    (superchat-bridge-provider-headers-to-record
     '(("accept" . "application/json")
       ("anthropic-dangerous-direct-browser-access" . "true"))
     (and session-id (plist-get compat :send-session-affinity-headers)
          (list (cons (if (equal (plist-get compat :session-affinity-format) "openrouter")
                          "x-session-id" "x-session-affinity")
                      session-id)))
     (superchat-bridge-model-headers model)
     (plist-get options :headers))))

(defun superchat-bridge-am--request-url (base-url)
  "Return the URL of the Messages endpoint under BASE-URL."
  (concat (string-remove-suffix "/" base-url) "/v1/messages?beta=true"))

(defun superchat-bridge-am--run (st)
  "Run the request of ST: the start of the source's `try' block."
  (condition-case err
      (let ((model (superchat-bridge-am--state-model st))
            (options (superchat-bridge-am--state-options st))
            (signal (superchat-bridge-am--state-signal st)))
        (superchat-bridge-am--assert-request-auth
         (superchat-bridge-model-provider model)
         (plist-get options :api-key) (plist-get options :headers))
        (setf (superchat-bridge-am--state-headers st)
              (superchat-bridge-am--client-headers model options))
        (setf (superchat-bridge-am--state-url st)
              (superchat-bridge-am--request-url (superchat-bridge-model-base-url model)))
        (when (and signal (not (superchat-bridge-abort-signal-aborted-p signal)))
          (let ((listener (lambda ()
                            (let ((handle (superchat-bridge-am--state-handle st)))
                              (when handle (superchat-bridge-http-abort handle))))))
            (setf (superchat-bridge-am--state-listener st) listener)
            (superchat-bridge-abort-signal-add-listener signal listener)))
        (superchat-bridge-am--request st))
    (error (superchat-bridge-am--fail st err))))

;;; Entry points

(defun superchat-bridge-api-anthropic-messages-stream (model context options)
  "Stream a response of MODEL to CONTEXT, return the event stream at once.
The source's `stream'.  See the file commentary for MODEL, CONTEXT and
OPTIONS.  Every failure ends the stream with an `error' event."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (compat (superchat-bridge-am--get-compat model))
         (transcript (superchat-bridge-am--resolve-transcript
                      context (plist-get compat :supports-mid-convo-system-messages)))
         (output (superchat-bridge-make-assistant-message
                  :content nil
                  :api (superchat-bridge-model-api model)
                  :provider (superchat-bridge-model-provider model)
                  :model (superchat-bridge-model-id model)
                  :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
                  :stop-reason "pending"
                  :timestamp (superchat-bridge-am--now-ms)))
         (st (superchat-bridge-am--make-state
              :stream stream :model model :options options :output output
              :signal (plist-get options :signal) :context transcript
              :usage-model model
              :current-tools (superchat-bridge-am--get-current-tools
                              (superchat-bridge-context-messages transcript)))))
    (superchat-bridge-am--run st)
    stream))

(defun superchat-bridge-am--effort-for-level (model level)
  "Return the effort of LEVEL for MODEL: `mapThinkingLevelToEffort'."
  (let ((mapped (cdr (assoc level (superchat-bridge-model-thinking-level-map model)))))
    (if (stringp mapped)
        mapped
      (pcase level
        ((or "minimal" "low") "low")
        ("medium" "medium")
        (_ "high")))))

(defun superchat-bridge-api-anthropic-messages-stream-simple (model context options)
  "Stream for simple OPTIONS: the source's `streamSimple'.
OPTIONS may carry `:reasoning', a thinking level: adaptive models get an
effort, older ones a thinking budget.  A missing API key signals an
error here, as it throws in the source."
  (superchat-bridge-am--assert-request-auth (superchat-bridge-model-provider model)
                                            (plist-get options :api-key)
                                            (plist-get options :headers))
  (let ((base (append (superchat-bridge-am--build-base-options
                       model context options (plist-get options :api-key))
                      (list :tool-choice (plist-get options :tool-choice))))
        (reasoning (plist-get options :reasoning)))
    (cond
     ((not (superchat-bridge-am--truthy reasoning))
      (superchat-bridge-api-anthropic-messages-stream
       model context (append (list :thinking-enabled :false) base)))
     ((superchat-bridge-am--compat-true-p model 'force-adaptive-thinking)
      (superchat-bridge-api-anthropic-messages-stream
       model context
       (append (list :thinking-enabled t
                     :effort (superchat-bridge-am--effort-for-level model reasoning))
               base)))
     (t
      (let* ((adjusted (superchat-bridge-am--adjust-max-tokens-for-thinking
                        (plist-get base :max-tokens)
                        (superchat-bridge-model-max-tokens model)
                        reasoning (plist-get options :thinking-budgets)))
             (max-tokens (superchat-bridge-am--clamp-max-tokens-to-context
                          model context (car adjusted))))
        (superchat-bridge-api-anthropic-messages-stream
         model context
         (append (list :max-tokens max-tokens
                       :thinking-enabled t
                       :thinking-budget-tokens (min (cdr adjusted)
                                                    (max 0 (- max-tokens 1024))))
                 base)))))))

(provide 'superchat-bridge-api-anthropic-messages)

;;; superchat-bridge-api-anthropic-messages.el ends here
