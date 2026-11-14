;;; llm-pi-ai-api-openai-completions.el --- OpenAI Chat Completions API, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/openai-completions.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;;   `llm-pi-ai-api-openai-completions-stream'          `stream'
;;   `llm-pi-ai-api-openai-completions-stream-simple'   `streamSimple'
;;   `llm-pi-ai-api-openai-completions-convert-messages' `convertMessages'
;;   `llm-pi-ai-api-openai-completions-detect-compat'   `detectCompat'
;;   `llm-pi-ai-api-openai-completions-get-compat'      `getCompat'
;;
;; Everything else is private and uses the prefix `llm-pi-ai-oc--'.
;;
;; The `openai' npm client is replaced by `llm-pi-ai-http-request' (curl)
;; and `llm-pi-ai-sse-parser'.  A request is a POST of the JSON that
;; `buildParams' makes to `<base-url>/chat/completions', its headers as
;; `createClient' makes them (Authorization bearer from the API key, then
;; the model's headers, session affinity headers, and the options'
;; headers, merged without regard to case, a nil value removing one: see
;; `llm-pi-ai-provider-headers-to-record').  Each SSE `data:' line is one
;; `ChatCompletionChunk'; `[DONE]' ends the data (the stream then ends
;; when the connection does).  The request is wrapped in
;; `llm-pi-ai-retry-provider-request' where the source wraps it in
;; `retryProviderRequest': it covers the time until the response headers
;; arrive and, for a status other than 2xx, the whole error body.  Once a
;; response has begun nothing is retried.
;;
;; Representation.
;;   MODEL     an `llm-pi-ai-model'.  Its `compat' is an alist with
;;             kebab-case symbol keys (see `llm-pi-ai-model-catalog'): an
;;             entry that is present counts, a value of nil being JSON
;;             false; JSON null (`:null') counts as absent, as `??' does.
;;   CONTEXT   an `llm-pi-ai-context' already normalized (see
;;             `llm-pi-ai-normalize-context'): its `messages' hold the
;;             system prompt and tools as a leading system message.
;;   OPTIONS   a plist.  Keys read here: `:api-key' `:headers' `:env'
;;             `:signal' `:session-id' `:cache-retention' `:max-tokens'
;;             `:temperature' `:sampling-params' `:tool-choice'
;;             `:reasoning-effort' `:thinking-budgets' `:timeout-ms'
;;             `:max-retries' `:max-retry-delay-ms' `:on-payload'
;;             `:on-response' `:on-provider-stream-event'; `stream-simple'
;;             also reads `:reasoning'.  `:headers' is an alist of
;;             (NAME . VALUE), a nil VALUE removing a header.
;;             `:sampling-params' is an alist of (NAME . VALUE) with
;;             string (or symbol) names.  `:thinking-budgets' is an
;;             `llm-pi-ai-thinking-budgets'.  `:tool-choice' is a string
;;             or a JSON object in the form below.
;;   HOOKS     `:on-payload' is called with (PARAMS MODEL), PARAMS being
;;             the request body in the form below, and may return a
;;             replacement body or nil to keep it; `:on-response' with
;;             an `llm-pi-ai-provider-response' and MODEL; and
;;             `:on-provider-stream-event' with each parsed chunk (a
;;             hash table) and MODEL.  Their results, other than that of
;;             `:on-payload', are ignored; an error they signal ends the
;;             stream with an `error' event.
;;   ABORT     `:signal' is an `llm-pi-ai-abort-signal'.  A listener on it
;;             calls `llm-pi-ai-http-abort' for the request in flight.
;;
;; JSON.  The request body is built as Lisp data and serialized once by
;; `json-serialize' (UTF-8 decoded into a string for the HTTP seam).
;; The mapping, exact:
;;
;;   JSON object  an alist with SYMBOL keys, in the order the source
;;                builds the keys (the order is what the provider
;;                gets); nil is the empty object `{}'.  The body of a
;;                hook is accepted as such an alist, as a plist or as a
;;                hash table.
;;   JSON array   a vector; `[]' is `[]'.  A Lisp list is never an array.
;;   true, false  t and `:false'.  nil is never false: it is `{}'.
;;   null         `:null'.
;;   undefined    the key is left out, as `JSON.stringify' leaves it out.
;;
;; Data the caller supplies that goes into the body (tool `parameters',
;; a tool call's `arguments', `:tool-choice', sampling parameters) is
;; passed through `llm-pi-ai-oc--json', which accepts a hash table, an
;; alist with symbol or string keys, a plist with keyword keys, vectors
;; for arrays, and also a non-empty list that is no object as an array.
;; Chunks are parsed with `json-parse-string' as hash tables with string
;; keys (`:object-type hash-table'), vectors for arrays, `:null' for
;; null and `:false' for false; `llm-pi-ai-oc--jget' reads a member and
;; gives nil for null and false, as the source does by `??' and `||'.
;;
;; Left out, whole:
;;   - GitHub Copilot dynamic headers (`src/api/github-copilot-headers.ts');
;;   - Cloudflare specifics: none exist in this file beyond `detectCompat'
;;     recognising the providers, which is kept;
;;   - client identity headers (`getIdentityHeaders', `ClientIdentity',
;;     the `identity' option): `src/utils/identity.ts' is not ported;
;;   - the `openai' SDK's own default headers (User-Agent, X-Stainless-*);
;;   - the `fetch' option (no JavaScript fetch exists) and the options
;;     `telemetry', `transport', `websocketConnectTimeoutMs',
;;     `metadata' (the source's code ignores the last three here);
;;   - constrained sampling (`src/api/constrained-sampling.ts'): grammar
;;     tools (`custom' tool type and the streaming of custom tool
;;     input) and JSON-schema strict sampling.  A tool whose
;;     `constrained-sampling' is set signals an error that ends the
;;     stream; `supportsOpenAIGrammarTools' is read but changes nothing;
;;   - `convertMessages''s fourth parameter (it carries grammar tools).
;;
;; Differences owed to the runtime:
;;   - `timeoutMs' is the time until the response headers arrive, as the
;;     SDK's is, kept by a timer (`llm-pi-ai-retry-run-at-time-function')
;;     because the HTTP seam's timeout would cover the whole stream.
;;   - A connection failure reads "Connection error: <curl's message>",
;;     a timeout "Request timed out.", an abort "Request was aborted.".
;;   - From `src/utils/transcript.ts', `src/utils/estimate.ts' and
;;     `src/api/simple-options.ts' the parts this file needs are private
;;     copies here (`llm-pi-ai-oc--resolve-transcript' and friends,
;;     `llm-pi-ai-oc--estimate-context-tokens', and the sampling and
;;     thinking-budget helpers), until those files have owners.
;;   - `retryProviderRequest' is given the error plists described in
;;     `llm-pi-ai-utils-provider-retry'; an HTTP error body becomes the
;;     `message' the `openai' SDK's `APIError' would have, so that
;;     `llm-pi-ai-format-provider-error' gives the source's text.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'llm-pi-ai-types)
(require 'llm-pi-ai-event-stream)
(require 'llm-pi-ai-models)
(require 'llm-pi-ai-http)
(require 'llm-pi-ai-utils-abort)
(require 'llm-pi-ai-utils-retry)
(require 'llm-pi-ai-utils-provider-retry)
(require 'llm-pi-ai-utils-error-body)
(require 'llm-pi-ai-utils-hash)
(require 'llm-pi-ai-utils-headers)
(require 'llm-pi-ai-utils-json-parse)
(require 'llm-pi-ai-utils-provider-env)
(require 'llm-pi-ai-utils-sanitize-unicode)
(require 'llm-pi-ai-utils-text)
(require 'llm-pi-ai-api-transform-messages)
(require 'llm-pi-ai-api-openai-prompt-cache)

;;; Small helpers

(defun llm-pi-ai-oc--now-ms ()
  "Return the current time in milliseconds, like `Date.now()'."
  (floor (* 1000 (float-time))))

(defun llm-pi-ai-oc--obj (&rest keys-and-values)
  "Return a new alist from KEYS-AND-VALUES, symbol keys in order."
  (let (result)
    (while keys-and-values
      (push (cons (car keys-and-values) (cadr keys-and-values)) result)
      (setq keys-and-values (cddr keys-and-values)))
    (nreverse result)))

(defun llm-pi-ai-oc--put (alist key value)
  "Set KEY to VALUE in ALIST and return it, as `obj[key] = value'.
An existing key keeps its place, a new one goes last.  ALIST is changed."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell value) alist)
      (nconc alist (list (cons key value))))))

(defun llm-pi-ai-oc--truthy (value)
  "Return non-nil when VALUE is truthy in JavaScript, for a Lisp VALUE.
nil, `:null', `:false', the empty string and 0 are not."
  (not (or (null value) (memq value '(:null :false))
           (equal value "") (eql value 0) (eql value 0.0))))

(defun llm-pi-ai-oc--jget (object key)
  "Return the member KEY of OBJECT, a parsed JSON hash table, or nil.
Null and false read as nil, as `??' and `||' read them in the source."
  (and (hash-table-p object)
       (let ((value (gethash key object)))
         (and (not (memq value '(:null :false))) value))))

(defun llm-pi-ai-oc--includes (text part)
  "Return non-nil when the string TEXT contains PART."
  (and (string-search part text) t))

(defun llm-pi-ai-oc--json-key (key)
  "Return KEY, a symbol, keyword or string, as the symbol for an object key."
  (cond ((keywordp key) (intern (substring (symbol-name key) 1)))
        ((symbolp key) key)
        (t (intern (format "%s" key)))))

(defun llm-pi-ai-oc--alist-like-p (value)
  "Return non-nil when VALUE is a non-empty alist with symbol or string keys."
  (and (consp value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (and (symbolp (car entry)) (car entry)
                                 (not (memq (car entry) '(:null :false))))
                            (stringp (car entry)))))
                 value)))

(defun llm-pi-ai-oc--plist-like-p (value)
  "Return non-nil when VALUE is a non-empty plist with keyword keys."
  (and (consp value) (keywordp (car value)) (cl-evenp (length value))
       (not (memq (car value) '(:null :false)))
       (cl-loop for (key) on value by #'cddr always (keywordp key))))

(defun llm-pi-ai-oc--json (value)
  "Return VALUE, a caller's data, in the form `json-serialize' takes.
See the file commentary: hash tables, alists and plists become alists
with symbol keys, vectors stay arrays, a list that is no object becomes
a vector, and nil stays nil, which is `{}'."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (push (cons (llm-pi-ai-oc--json-key key) (llm-pi-ai-oc--json item))
                       result))
               value)
      (nreverse result)))
   ((vectorp value) (vconcat (mapcar #'llm-pi-ai-oc--json value)))
   ((llm-pi-ai-oc--alist-like-p value)
    (mapcar (lambda (entry)
              (cons (llm-pi-ai-oc--json-key (car entry))
                    (llm-pi-ai-oc--json (cdr entry))))
            value))
   ((llm-pi-ai-oc--plist-like-p value)
    (cl-loop for (key item) on value by #'cddr
             collect (cons (llm-pi-ai-oc--json-key key)
                           (llm-pi-ai-oc--json item))))
   ((consp value) (vconcat (mapcar #'llm-pi-ai-oc--json value)))
   (t value)))

(defun llm-pi-ai-oc--serialize (value)
  "Return VALUE, Lisp data of the file commentary, as JSON text.
This is `JSON.stringify' for the data this file builds."
  (let ((text (json-serialize (llm-pi-ai-oc--json value)
                              :null-object :null :false-object :false)))
    (if (multibyte-string-p text) text (decode-coding-string text 'utf-8))))

(defun llm-pi-ai-oc--bool (value)
  "Return `t' when VALUE is truthy and `:false' when not: `!!value' as JSON."
  (if (llm-pi-ai-oc--truthy value) t :false))

(defun llm-pi-ai-oc--tlm (model level)
  "Return `model.thinkingLevelMap?.[LEVEL]': a string, `:null' or `:undefined'."
  (let ((entry (assoc level (llm-pi-ai-model-thinking-level-map model))))
    (if entry (cdr entry) :undefined)))

(defun llm-pi-ai-oc--nullish-p (value)
  "Return non-nil when VALUE is the source's undefined or null."
  (memq value '(:undefined :null nil)))

;;; Transcript helpers (src/utils/transcript.ts)

;; Private copies of the parts of `src/utils/transcript.ts' this file
;; uses; `llm-pi-ai-utils-transcript.el' ports only two of its functions.

(defun llm-pi-ai-oc--system-messages (messages)
  "Return the system messages among MESSAGES."
  (seq-filter #'llm-pi-ai-system-message-p messages))

(defun llm-pi-ai-oc--tool-name (tool)
  "Return the name of TOOL, a tool or a tool reference."
  (if (llm-pi-ai-tool-p tool)
      (llm-pi-ai-tool-name tool)
    (llm-pi-ai-tool-reference-name tool)))

(defun llm-pi-ai-oc--get-current-tools (messages)
  "Return the tools in force after every system message of MESSAGES.
The source's `getCurrentTools': removals, then additions, in order;
a re-added name keeps its place."
  (let (tools)
    (dolist (message (llm-pi-ai-oc--system-messages messages))
      (dolist (removed (llm-pi-ai-system-message-tools-removed message))
        (setq tools (assoc-delete-all (llm-pi-ai-oc--tool-name removed) tools
                                      #'equal)))
      (dolist (tool (llm-pi-ai-system-message-tools-added message))
        (let ((cell (assoc (llm-pi-ai-tool-name tool) tools)))
          (if cell
              (setcdr cell tool)
            (setq tools (append tools
                                (list (cons (llm-pi-ai-tool-name tool) tool))))))))
    (mapcar #'cdr tools)))

(defun llm-pi-ai-oc--get-current-system-message (messages)
  "Return one system message holding the current prompt and tools, or nil.
The source's `getCurrentSystemMessage'."
  (let (content sections timestamp seen)
    (dolist (message (llm-pi-ai-oc--system-messages messages))
      (unless seen
        (setq seen t timestamp (llm-pi-ai-message-timestamp message)))
      (let ((text (llm-pi-ai-content-text (llm-pi-ai-message-content message))))
        (when (> (length text) 0) (push text content)))
      (dolist (section (llm-pi-ai-system-message-sections message))
        (if (null (cdr section))
            (setq sections (assoc-delete-all (car section) sections #'equal))
          (let ((cell (assoc (car section) sections)))
            (if cell
                (setcdr cell (cdr section))
              (setq sections (append sections
                                     (list (cons (car section)
                                                 (cdr section))))))))))
    (let ((tools (llm-pi-ai-oc--get-current-tools messages)))
      (when (or seen tools)
        (llm-pi-ai-make-system-message
         :content (string-join (nreverse content) "\n\n")
         :sections sections
         :tools-added tools
         :timestamp (or timestamp 0))))))

(defun llm-pi-ai-oc--resolve-transcript (context supports-mid-convo-system-messages)
  "Return CONTEXT, or one with its system messages collapsed into one.
The source's `resolveTranscript' (with `collapseSystemMessages'): a
model that takes no later system messages gets the replayed one in
front and the others dropped."
  (if supports-mid-convo-system-messages
      context
    (let* ((messages (llm-pi-ai-context-messages context))
           (head (llm-pi-ai-oc--get-current-system-message messages))
           (rest (seq-remove #'llm-pi-ai-system-message-p messages)))
      (llm-pi-ai-make-context :messages (if head (cons head rest) rest)))))

(defun llm-pi-ai-oc--last-non-additive-tool-change (system-messages)
  "Return the index in SYSTEM-MESSAGES of the last non-additive tool change.
The source's `findLastNonAdditiveToolChange'; -1 when there is none."
  (let ((declared nil) (last -1) (index 0))
    (dolist (message system-messages)
      (when (llm-pi-ai-system-message-tools-removed message)
        (setq last index))
      (dolist (tool (llm-pi-ai-system-message-tools-added message))
        (when (member (llm-pi-ai-tool-name tool) declared)
          (setq last index))
        (push (llm-pi-ai-tool-name tool) declared))
      (setq index (1+ index)))
    last))

(defun llm-pi-ai-oc--resolve-transcript-tools (messages support)
  "Split the tool declarations of MESSAGES for a transport.
The source's `resolveTranscriptTools'.  SUPPORT is `none', `additions' or
`changes'.  Return (REQUEST-TOOLS . FOLDED-SYSTEM-MESSAGES)."
  (let ((system-messages (llm-pi-ai-oc--system-messages messages)))
    (pcase support
      ('none (cons (llm-pi-ai-oc--get-current-tools system-messages)
                   (length system-messages)))
      ('changes (cons nil 0))
      (_ (let ((folded (max (if (and messages
                                     (llm-pi-ai-system-message-p (car messages)))
                                1 0)
                            (1+ (llm-pi-ai-oc--last-non-additive-tool-change
                                 system-messages)))))
           (cons (llm-pi-ai-oc--get-current-tools
                  (seq-take system-messages folded))
                 folded))))))

;;; Estimating context tokens (src/utils/estimate.ts)

(defun llm-pi-ai-oc--estimate-text-tokens (text)
  "Return the estimated tokens of TEXT: `estimateTextTokens'."
  (ceiling (/ (float (llm-pi-ai-utf16-length text)) 3.5)))

(defun llm-pi-ai-oc--content-chars (content)
  "Return the estimated characters of CONTENT, a string or text and images."
  (if (stringp content)
      (llm-pi-ai-utf16-length content)
    (let ((chars 0))
      (dolist (block content)
        (setq chars (+ chars (if (llm-pi-ai-text-content-p block)
                                 (llm-pi-ai-utf16-length
                                  (or (llm-pi-ai-text-content-text block) ""))
                               4800))))
      chars)))

(defun llm-pi-ai-oc--tools-json (tools)
  "Return TOOLS, a list of tools, as the JSON text `JSON.stringify' gives."
  (llm-pi-ai-oc--serialize
   (vconcat (mapcar (lambda (tool)
                      (llm-pi-ai-oc--obj
                       'name (llm-pi-ai-tool-name tool)
                       'description (llm-pi-ai-tool-description tool)
                       'parameters (llm-pi-ai-tool-parameters tool)))
                    tools))))

(defun llm-pi-ai-oc--estimate-tools-tokens (tools)
  "Return the estimated tokens of TOOLS: `estimateToolsTokens'."
  (if (null tools)
      0
    (llm-pi-ai-oc--estimate-text-tokens (llm-pi-ai-oc--tools-json tools))))

(defun llm-pi-ai-oc--estimate-message-tokens (message)
  "Return the estimated tokens of MESSAGE: `estimateMessageTokens'."
  (cond
   ((llm-pi-ai-system-message-p message)
    (+ (llm-pi-ai-oc--estimate-text-tokens
        (llm-pi-ai-system-message-text message))
       (llm-pi-ai-oc--estimate-tools-tokens
        (llm-pi-ai-system-message-tools-added message))
       (llm-pi-ai-oc--estimate-tools-tokens
        (mapcar (lambda (ref) (llm-pi-ai-make-tool :name (llm-pi-ai-oc--tool-name ref)))
                (llm-pi-ai-system-message-tools-removed message)))))
   ((or (llm-pi-ai-user-message-p message)
        (llm-pi-ai-tool-result-message-p message))
    (ceiling (/ (float (llm-pi-ai-oc--content-chars
                        (llm-pi-ai-message-content message)))
                3.5)))
   (t
    (let ((chars 0))
      (dolist (block (llm-pi-ai-message-content message))
        (setq chars
              (+ chars
                 (cond ((llm-pi-ai-text-content-p block)
                        (llm-pi-ai-utf16-length (or (llm-pi-ai-text-content-text block) "")))
                       ((llm-pi-ai-thinking-content-p block)
                        (llm-pi-ai-utf16-length
                         (or (llm-pi-ai-thinking-content-thinking block) "")))
                       (t (+ (llm-pi-ai-utf16-length
                              (or (llm-pi-ai-tool-call-name block) ""))
                             (llm-pi-ai-utf16-length
                              (llm-pi-ai-safe-json-stringify
                               (llm-pi-ai-tool-call-arguments block)))))))))
      (ceiling (/ (float chars) 3.5))))))

(defun llm-pi-ai-oc--context-tokens (usage)
  "Return the tokens USAGE reports: `calculateContextTokens'."
  (let ((total (llm-pi-ai-usage-total-tokens usage)))
    (if (and total (/= total 0))
        total
      (+ (llm-pi-ai-usage-input usage) (llm-pi-ai-usage-output usage)
         (llm-pi-ai-usage-cache-read usage) (llm-pi-ai-usage-cache-write usage)))))

(defun llm-pi-ai-oc--estimate-context-tokens (context)
  "Return the estimated tokens of CONTEXT: `estimateContextTokens' `.tokens'."
  (let* ((messages (llm-pi-ai-context-messages context))
         (latest-prefix-timestamp nil)
         (info nil)
         (index 0))
    (dolist (message messages)
      (when (llm-pi-ai-assistant-message-p message)
        (let ((usage (llm-pi-ai-assistant-message-usage message)))
          (when (and (or (null latest-prefix-timestamp)
                         (>= (or (llm-pi-ai-message-timestamp message) 0)
                             latest-prefix-timestamp))
                     (not (member (llm-pi-ai-assistant-message-stop-reason message)
                                  '("aborted" "error")))
                     usage
                     (> (llm-pi-ai-oc--context-tokens usage) 0))
            (setq info (cons usage index)))))
      (setq latest-prefix-timestamp
            (max (or latest-prefix-timestamp (- (expt 2 60)))
                 (or (llm-pi-ai-message-timestamp message) 0)))
      (setq index (1+ index)))
    (if info
        (+ (llm-pi-ai-oc--context-tokens (car info))
           (apply #'+ (mapcar #'llm-pi-ai-oc--estimate-message-tokens
                              (nthcdr (1+ (cdr info)) messages))))
      (apply #'+ (mapcar #'llm-pi-ai-oc--estimate-message-tokens messages)))))

;;; Simple options (src/api/simple-options.ts)

(defconst llm-pi-ai-oc--min-answer-tokens 1024
  "`MIN_ANSWER_TOKENS': tokens always left for the answer.")

(defun llm-pi-ai-oc--clamp-max-tokens-to-context (model context max-tokens)
  "Return MAX-TOKENS held to what MODEL's window has left after CONTEXT.
The source's `clampMaxTokensToContext'."
  (let ((window (or (llm-pi-ai-model-context-window model) 0)))
    (if (<= window 0)
        (max 1 max-tokens)
      (min max-tokens
           (max 1 (- window (llm-pi-ai-oc--estimate-context-tokens context)
                     4096))))))

(defun llm-pi-ai-oc--key-name (key)
  "Return KEY, a string or symbol, as a name string."
  (cond ((stringp key) key)
        ((keywordp key) (substring (symbol-name key) 1))
        (t (symbol-name key))))

(defun llm-pi-ai-oc--merge-params (&rest param-lists)
  "Merge PARAM-LISTS, alists of (NAME . VALUE), later over earlier.
`{...a, ...b}': a name keeps the place of its first appearance.  Names
come out as strings."
  (let (result)
    (dolist (params param-lists)
      (dolist (entry params)
        (let* ((name (llm-pi-ai-oc--key-name (car entry)))
               (cell (assoc name result)))
          (if cell
              (setcdr cell (cdr entry))
            (setq result (append result (list (cons name (cdr entry)))))))))
    result))

(defun llm-pi-ai-oc--resolve-sampling-params (model thinking-level request-params)
  "Return the sampling parameters of MODEL at THINKING-LEVEL, or nil.
REQUEST-PARAMS are the request's own, which win per name.  The source's
`resolveSamplingParams'."
  (let* ((effective (llm-pi-ai-clamp-thinking-level model thinking-level))
         (by-level (cdr (assoc effective
                               (llm-pi-ai-model-sampling-params-by-thinking-level
                                model))))
         (model-params (llm-pi-ai-model-sampling-params model)))
    (when (or model-params by-level request-params)
      (llm-pi-ai-oc--merge-params model-params by-level request-params))))

(defconst llm-pi-ai-oc--default-thinking-budgets
  '(("minimal" . 1024) ("low" . 2048) ("medium" . 8192) ("high" . 16384))
  "`DEFAULT_THINKING_BUDGETS'.")

(defun llm-pi-ai-oc--thinking-budget-for-level (level custom-budgets)
  "Return the thinking budget of LEVEL, CUSTOM-BUDGETS over the defaults.
The source's `thinkingBudgetForLevel'; CUSTOM-BUDGETS is an
`llm-pi-ai-thinking-budgets' or nil.  \"xhigh\" and \"max\" read as
\"high\", as `clampReasoning' does."
  (let* ((level (if (member level '("xhigh" "max")) "high" level))
         (custom (and custom-budgets
                      (pcase level
                        ("minimal" (llm-pi-ai-thinking-budgets-minimal custom-budgets))
                        ("low" (llm-pi-ai-thinking-budgets-low custom-budgets))
                        ("medium" (llm-pi-ai-thinking-budgets-medium custom-budgets))
                        ("high" (llm-pi-ai-thinking-budgets-high custom-budgets))))))
    (or custom (cdr (assoc level llm-pi-ai-oc--default-thinking-budgets)))))

(defun llm-pi-ai-oc--clamp-thinking-budget-to-answer-room (budget ceiling)
  "Return BUDGET held so that MIN_ANSWER_TOKENS remain under CEILING."
  (min budget (max 0 (- ceiling llm-pi-ai-oc--min-answer-tokens))))

;;; Compat (detectCompat, getCompat)

(defun llm-pi-ai-api-openai-completions-detect-compat (model)
  "Return the compatibility settings detected for MODEL, a plist.
The source's `detectCompat': from the provider name and the base URL;
it is the base that `llm-pi-ai-api-openai-completions-get-compat'
puts the model's own `compat' over.  Keys are the source's field names
in kebab case with a colon; false and undefined are nil."
  (let* ((provider (llm-pi-ai-model-provider model))
         (base-url (or (llm-pi-ai-model-base-url model) ""))
         (id (or (llm-pi-ai-model-id model) ""))
         (url (lambda (part) (llm-pi-ai-oc--includes base-url part)))
         (is-zai (or (member provider '("zai" "zai-coding-cn"))
                     (funcall url "api.z.ai") (funcall url "open.bigmodel.cn")))
         (is-together (or (equal provider "together")
                          (funcall url "api.together.ai")
                          (funcall url "api.together.xyz")))
         (is-moonshot (or (member provider '("moonshotai" "moonshotai-cn"))
                          (funcall url "api.moonshot.")))
         (is-open-router (or (equal provider "openrouter")
                             (funcall url "openrouter.ai")))
         (is-cloudflare-workers-ai (or (equal provider "cloudflare-workers-ai")
                                       (funcall url "api.cloudflare.com")))
         (is-cloudflare-ai-gateway (or (equal provider "cloudflare-ai-gateway")
                                       (funcall url "gateway.ai.cloudflare.com")))
         (is-nvidia (or (equal provider "nvidia")
                        (funcall url "integrate.api.nvidia.com")))
         (is-ant-ling (or (equal provider "ant-ling")
                          (funcall url "api.ant-ling.com")))
         (is-cerebras (or (equal provider "cerebras") (funcall url "cerebras.ai")))
         (is-deepseek (or (equal provider "deepseek")
                          (llm-pi-ai-oc--includes (downcase base-url) "deepseek.com")))
         (is-non-standard
          (or is-nvidia is-cerebras (equal provider "xai") (funcall url "api.x.ai")
              is-together (funcall url "chutes.ai") is-deepseek is-zai is-moonshot
              (equal provider "opencode") (funcall url "opencode.ai")
              is-cloudflare-workers-ai is-cloudflare-ai-gateway is-ant-ling))
         (use-max-tokens
          (or (funcall url "chutes.ai") is-deepseek is-moonshot
              is-cloudflare-ai-gateway is-together is-nvidia is-ant-ling is-zai))
         (is-grok (or (equal provider "xai") (funcall url "api.x.ai")))
         (is-open-router-developer-role-model
          (and is-open-router
               (or (string-prefix-p "anthropic/" id) (string-prefix-p "openai/" id))))
         (cache-control-format
          (and (equal provider "openrouter") (string-prefix-p "anthropic/" id)
               "anthropic")))
    (list
     :supports-store (not is-non-standard)
     :supports-developer-role (and (or is-open-router-developer-role-model
                                       (and (not is-non-standard)
                                            (not is-open-router)))
                                   t)
     :supports-reasoning-effort (not (or is-grok is-zai is-moonshot is-together
                                         is-cloudflare-ai-gateway is-nvidia
                                         is-ant-ling))
     :supports-usage-in-streaming t
     :supports-finish-reason t
     :max-tokens-field (if use-max-tokens "max_tokens" "max_completion_tokens")
     :requires-tool-result-name nil
     :requires-assistant-after-tool-result nil
     :requires-thinking-as-text nil
     :requires-reasoning-content-on-assistant-messages (and is-deepseek t)
     :thinking-format (cond (is-deepseek "deepseek") (is-zai "zai")
                            (is-together "together") (is-ant-ling "ant-ling")
                            (is-open-router "openrouter") (t "openai"))
     :open-router-routing nil
     :vercel-gateway-routing nil
     :chat-template-kwargs nil
     :chat-template-args nil
     :zai-tool-stream nil
     :supports-thinking-token-budget nil
     :thinking-token-budget-field nil
     ;; OpenAI compatibility alone does not imply strict JSON-schema tool support.
     :supports-strict-mode nil
     :supports-open-ai-grammar-tools nil
     :supports-mid-convo-system-messages nil
     :supports-mid-convo-tool-additions nil
     :cache-control-format cache-control-format
     :send-session-affinity-headers (and is-open-router t)
     :session-affinity-format (if is-open-router "openrouter" "openai")
     :supports-long-cache-retention (not (or is-together is-cloudflare-workers-ai
                                             is-cloudflare-ai-gateway is-nvidia
                                             is-ant-ling))
     :vllm-priority nil)))

(defconst llm-pi-ai-oc--compat-keys
  '((supports-store . :supports-store)
    (supports-developer-role . :supports-developer-role)
    (supports-reasoning-effort . :supports-reasoning-effort)
    (supports-usage-in-streaming . :supports-usage-in-streaming)
    (supports-finish-reason . :supports-finish-reason)
    (max-tokens-field . :max-tokens-field)
    (requires-tool-result-name . :requires-tool-result-name)
    (requires-assistant-after-tool-result . :requires-assistant-after-tool-result)
    (requires-thinking-as-text . :requires-thinking-as-text)
    (requires-reasoning-content-on-assistant-messages
     . :requires-reasoning-content-on-assistant-messages)
    (thinking-format . :thinking-format)
    (vercel-gateway-routing . :vercel-gateway-routing)
    (chat-template-kwargs . :chat-template-kwargs)
    (chat-template-args . :chat-template-args)
    (zai-tool-stream . :zai-tool-stream)
    (supports-thinking-token-budget . :supports-thinking-token-budget)
    (thinking-token-budget-field . :thinking-token-budget-field)
    (supports-strict-mode . :supports-strict-mode)
    (supports-open-ai-grammar-tools . :supports-open-ai-grammar-tools)
    (supports-mid-convo-system-messages . :supports-mid-convo-system-messages)
    (supports-mid-convo-tool-additions . :supports-mid-convo-tool-additions)
    (cache-control-format . :cache-control-format)
    (send-session-affinity-headers . :send-session-affinity-headers)
    (session-affinity-format . :session-affinity-format)
    (supports-long-cache-retention . :supports-long-cache-retention))
  "Model compat keys that override the detected value of the same field.
Each is `model.compat.X ?? detected.X' in `getCompat'.")

(defun llm-pi-ai-api-openai-completions-get-compat (model)
  "Return the resolved compatibility settings of MODEL, a plist.
The source's `getCompat': `detect-compat' with the entries of the model's
`compat' over it.  An entry that is present wins, its nil being false;
a `:null' entry is absent.  `open-router-routing' and `vllm-priority'
are the model's own, not detected."
  (let* ((detected (llm-pi-ai-api-openai-completions-detect-compat model))
         (model-compat (llm-pi-ai-model-compat model)))
    (if (null model-compat)
        detected
      (let ((result (copy-sequence detected)))
        (dolist (mapping llm-pi-ai-oc--compat-keys)
          (let ((entry (assq (car mapping) model-compat)))
            (when (and entry (not (eq (cdr entry) :null)))
              (setq result (plist-put result (cdr mapping) (cdr entry))))))
        (let ((routing (assq 'open-router-routing model-compat)))
          (setq result (plist-put result :open-router-routing
                                  (and routing (not (eq (cdr routing) :null))
                                       (cdr routing)))))
        (let ((priority (assq 'vllm-priority model-compat)))
          (setq result (plist-put result :vllm-priority
                                  (and priority (not (eq (cdr priority) :null))
                                       (cdr priority)))))
        result))))

;;; Headers and API key

(defun llm-pi-ai-oc--has-header-p (headers name)
  "Return non-nil when HEADERS, an alist, has a non-blank value for NAME.
The source's `hasHeader'; names compare without regard to case."
  (let ((expected (downcase name)))
    (cl-some (lambda (entry)
               (and (string= (downcase (car entry)) expected)
                    (stringp (cdr entry))
                    (> (length (llm-pi-ai-string-trim-js (cdr entry))) 0)))
             headers)))

(defun llm-pi-ai-oc--get-client-api-key (provider api-key headers)
  "Return the API key for PROVIDER: API-KEY, \"unused\" or an error.
The source's `getClientApiKey': a request that carries its own
authorization header needs none."
  (cond ((llm-pi-ai-oc--truthy api-key) api-key)
        ((or (llm-pi-ai-oc--has-header-p headers "authorization")
             (llm-pi-ai-oc--has-header-p headers "cf-aig-authorization"))
         "unused")
        (t (error "No API key for provider: %s" provider))))

(defun llm-pi-ai-oc--resolve-cache-retention (cache-retention env)
  "Return the cache retention: CACHE-RETENTION, else from ENV, else \"short\".
The source's `resolveCacheRetention'."
  (cond (cache-retention cache-retention)
        ((equal (llm-pi-ai-get-provider-env-value "PI_CACHE_RETENTION" env)
                "long")
         "long")
        (t "short")))

(defun llm-pi-ai-oc--request-headers (api-key model session-id compat
                                              options-headers)
  "Return the request headers as `createClient' makes them, an alist.
API-KEY is the bearer token; the rest is as in the source.  The
sources merge in this order, later over earlier, a nil value removing
a header: the SDK's defaults, the model's headers, the session
affinity headers, the options' headers."
  (let ((affinity
         (when (and session-id (plist-get compat :send-session-affinity-headers))
           (if (equal (plist-get compat :session-affinity-format) "openrouter")
               (list (cons "x-session-id" session-id))
             (append (and (equal (plist-get compat :session-affinity-format)
                                 "openai")
                         (list (cons "session_id" session-id)))
                     (list (cons "x-client-request-id" session-id)
                           (cons "x-session-affinity" session-id)))))))
    (llm-pi-ai-provider-headers-to-record
     (list (cons "Accept" "application/json")
           (cons "Content-Type" "application/json")
           (cons "Authorization" (concat "Bearer " api-key)))
     (llm-pi-ai-model-headers model)
     affinity
     options-headers)))

(defun llm-pi-ai-oc--request-url (base-url)
  "Return the URL of the chat completions endpoint under BASE-URL."
  (if (string-suffix-p "/" base-url)
      (concat (substring base-url 0 -1) "/chat/completions")
    (concat base-url "/chat/completions")))

;;; Reasoning details

(defun llm-pi-ai-oc--detail-p (detail)
  "Return non-nil when DETAIL is a valid `OpenAIReasoningDetail'.
The source's `isOpenAIReasoningDetail'; DETAIL is a parsed JSON value."
  (when (hash-table-p detail)
    (let ((id (gethash "id" detail :undefined))
          (format (gethash "format" detail :undefined))
          (index (gethash "index" detail :undefined))
          (type (gethash "type" detail))
          (signature (gethash "signature" detail :undefined)))
      (and (or (memq id '(:undefined :null)) (stringp id))
           (or (eq format :undefined) (stringp format))
           (or (eq index :undefined) (numberp index))
           (cond ((equal type "reasoning.summary")
                  (stringp (gethash "summary" detail)))
                 ((equal type "reasoning.encrypted")
                  (stringp (gethash "data" detail)))
                 ((equal type "reasoning.text")
                  (and (stringp (gethash "text" detail))
                       (or (memq signature '(:undefined :null))
                           (stringp signature)))))
           t))))

(defun llm-pi-ai-oc--parse-json-or-nil (text)
  "Parse TEXT as JSON (`JSON.parse') or return `:invalid' when it is not."
  (condition-case nil
      (json-parse-string text :object-type 'hash-table :array-type 'array
                         :null-object :null :false-object :false)
    (json-error :invalid)
    (wrong-type-argument :invalid)))

(defun llm-pi-ai-oc--parse-reasoning-details (signature)
  "Return the list of details SIGNATURE holds, or nil.
The source's `parseOpenAIReasoningDetails': SIGNATURE, a string or nil,
is JSON text of a non-empty array of valid details."
  (when (and (stringp signature) (> (length signature) 0))
    (let ((parsed (llm-pi-ai-oc--parse-json-or-nil signature)))
      (when (and (vectorp parsed) (> (length parsed) 0)
                 (cl-every #'llm-pi-ai-oc--detail-p parsed))
        (append parsed nil)))))

(defun llm-pi-ai-oc--parse-legacy-encrypted-detail (signature)
  "Return the encrypted detail SIGNATURE holds, or nil.
The source's `parseLegacyEncryptedReasoningDetail'."
  (when (and (stringp signature) (> (length signature) 0))
    (let ((parsed (llm-pi-ai-oc--parse-json-or-nil signature)))
      (when (and (llm-pi-ai-oc--detail-p parsed)
                 (equal (gethash "type" parsed) "reasoning.encrypted")
                 (stringp (gethash "id" parsed))
                 (> (length (gethash "id" parsed)) 0)
                 (> (length (gethash "data" parsed)) 0))
        parsed))))

(defun llm-pi-ai-oc--set-or-remove (table key value)
  "Set KEY of TABLE to VALUE, or remove it when VALUE is `:undefined'."
  (if (eq value :undefined)
      (remhash key table)
    (puthash key value table)))

(defun llm-pi-ai-oc--fill-missing-common-fields (target source)
  "Fill in the id, format and index of TARGET from SOURCE.
The source's `fillMissingCommonReasoningDetailFields': `??=' for id
and index, `||=' for format."
  (when (memq (gethash "id" target :undefined) '(:undefined :null))
    (llm-pi-ai-oc--set-or-remove target "id" (gethash "id" source :undefined)))
  (unless (llm-pi-ai-oc--truthy (gethash "format" target))
    (llm-pi-ai-oc--set-or-remove target "format"
                                 (gethash "format" source :undefined)))
  (when (memq (gethash "index" target :undefined) '(:undefined :null))
    (llm-pi-ai-oc--set-or-remove target "index"
                                 (gethash "index" source :undefined))))

(defun llm-pi-ai-oc--append-detail (details detail)
  "Return DETAILS, a list of detail tables, with DETAIL added.
The source's `appendOpenAIReasoningDetail': consecutive text, or
consecutive summary, details merge into the last one."
  (let* ((last (car (last details)))
         (type (gethash "type" detail))
         (last-type (and last (gethash "type" last))))
    (cond
     ((and (equal type "reasoning.text") (equal last-type "reasoning.text"))
      (puthash "text" (concat (gethash "text" last) (gethash "text" detail)) last)
      (unless (llm-pi-ai-oc--truthy (gethash "signature" last))
        (llm-pi-ai-oc--set-or-remove last "signature"
                                     (gethash "signature" detail :undefined)))
      (llm-pi-ai-oc--fill-missing-common-fields last detail)
      details)
     ((and (equal type "reasoning.summary") (equal last-type "reasoning.summary"))
      (puthash "summary" (concat (gethash "summary" last)
                                 (gethash "summary" detail))
               last)
      (llm-pi-ai-oc--fill-missing-common-fields last detail)
      details)
     (t (append details (list (copy-hash-table detail)))))))

;;; Cache control

(defun llm-pi-ai-oc--get-compat-cache-control (compat cache-retention)
  "Return the `cache_control' object for COMPAT and CACHE-RETENTION, or nil.
The source's `getCompatCacheControl'."
  (unless (or (not (equal (plist-get compat :cache-control-format) "anthropic"))
              (equal cache-retention "none"))
    (if (and (equal cache-retention "long")
             (plist-get compat :supports-long-cache-retention))
        (llm-pi-ai-oc--obj 'type "ephemeral" 'ttl "1h")
      (llm-pi-ai-oc--obj 'type "ephemeral"))))

(defun llm-pi-ai-oc--add-cache-control-to-text-content (message cache-control)
  "Mark the last text of MESSAGE, an alist, with CACHE-CONTROL.
Return non-nil when something was marked: `addCacheControlToTextContent'.
A string content becomes one marked text part."
  (let ((content (alist-get 'content message)))
    (cond
     ((stringp content)
      (unless (string-empty-p content)
        (setcdr (assq 'content message)
                (vector (llm-pi-ai-oc--obj 'type "text" 'text content
                                           'cache_control cache-control)))
        t))
     ((vectorp content)
      (let ((index (1- (length content))) (done nil))
        (while (and (not done) (>= index 0))
          (let ((part (aref content index)))
            (when (equal (alist-get 'type part) "text")
              (llm-pi-ai-oc--put part 'cache_control cache-control)
              (setq done t)))
          (setq index (1- index)))
        done)))))

(defun llm-pi-ai-oc--apply-anthropic-cache-control (messages tools cache-control)
  "Mark MESSAGES, a list of alists, and TOOLS, a vector or nil, for caching.
The source's `applyAnthropicCacheControl': the first instruction
message, the last tool and the last conversation message that has text."
  ;; addCacheControlToSystemPrompt: the first one is tried and that ends it.
  (let ((first (seq-find (lambda (message)
                           (member (alist-get 'role message) '("system" "developer")))
                         messages)))
    (when first
      (llm-pi-ai-oc--add-cache-control-to-text-content first cache-control)))
  ;; addCacheControlToLastTool
  (when (and tools (> (length tools) 0))
    (llm-pi-ai-oc--put (aref tools (1- (length tools))) 'cache_control
                       cache-control))
  ;; addCacheControlToLastConversationMessage
  (let ((done nil))
    (dolist (message (reverse messages))
      (when (and (not done)
                 (member (alist-get 'role message) '("user" "assistant" "tool"))
                 (llm-pi-ai-oc--add-cache-control-to-text-content
                  message cache-control))
        (setq done t)))))

;;; Tools

(defun llm-pi-ai-oc--convert-tools (tools compat)
  "Return TOOLS as a list of Chat Completions tool alists.
The source's `convertTools'.  A tool with constrained sampling is not
supported (see the file commentary) and signals an error."
  (mapcar
   (lambda (tool)
     (when (llm-pi-ai-tool-constrained-sampling tool)
       (error "Tool \"%s\" uses constrained sampling, which is not ported"
              (llm-pi-ai-tool-name tool)))
     (let ((function (llm-pi-ai-oc--obj 'name (llm-pi-ai-tool-name tool))))
       (when (llm-pi-ai-tool-description tool)
         (llm-pi-ai-oc--put function 'description
                            (llm-pi-ai-tool-description tool)))
       (llm-pi-ai-oc--put function 'parameters (llm-pi-ai-tool-parameters tool))
       ;; Only include strict if provider supports it.  Some reject unknown fields.
       (when (plist-get compat :supports-strict-mode)
         (llm-pi-ai-oc--put function 'strict :false))
       (llm-pi-ai-oc--obj 'type "function" 'function function)))
   tools))

;;; Messages

(defun llm-pi-ai-oc--normalize-tool-call-id (model id)
  "Return the tool call ID as MODEL's Chat Completions API takes it.
The `normalizeToolCallId' closure of `convertMessages'."
  (cond
   ((string-search "|" id)
    ;; Pipe-separated ids come from the OpenAI Responses API: {call_id}|{id}.
    (let* ((separator (string-search "|" id))
           (call-id (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_"
                                              (substring id 0 separator)))
           (item-id (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_"
                                              (substring id (1+ separator))))
           (combined (if (> (length item-id) 0)
                         (concat call-id "_" item-id)
                       call-id)))
      (if (<= (length combined) 40)
          combined
        (let* ((hash (substring (llm-pi-ai-short-hash id) 0
                                (min 8 (length (llm-pi-ai-short-hash id)))))
               (prefix (substring call-id 0
                                  (min (length call-id)
                                       (max 1 (- 40 (length hash) 1))))))
          (concat prefix "_" hash)))))
   ((equal (llm-pi-ai-model-provider model) "openai")
    (if (> (length id) 40) (substring id 0 40) id))
   (t id)))

(defun llm-pi-ai-oc--image-url-part (block)
  "Return the `image_url' content part for BLOCK, an image."
  (llm-pi-ai-oc--obj
   'type "image_url"
   'image_url (llm-pi-ai-oc--obj
               'url (format "data:%s;base64,%s"
                            (llm-pi-ai-image-content-mime-type block)
                            (llm-pi-ai-image-content-data block)))))

(defun llm-pi-ai-oc--text-part (text)
  "Return the `text' content part of TEXT, surrogates sanitized."
  (llm-pi-ai-oc--obj 'type "text" 'text (llm-pi-ai-sanitize-surrogates text)))

(defun llm-pi-ai-oc--blank-p (text)
  "Return non-nil when TEXT, a string or nil, is empty after `trim'."
  (or (null text) (string-empty-p (llm-pi-ai-string-trim-js text))))

(defun llm-pi-ai-oc--convert-assistant (model compat msg)
  "Return the Chat Completions message for MSG, an assistant message, or nil.
The assistant branch of `convertMessages'; nil is the source's `continue'."
  (let* ((blocks (llm-pi-ai-message-content msg))
         (message (llm-pi-ai-oc--obj
                   'role "assistant"
                   ;; Some providers don't accept null content, use "" instead
                   'content (if (plist-get compat :requires-assistant-after-tool-result)
                                "" :null)))
         (text-parts
          (mapcar (lambda (block) (llm-pi-ai-oc--text-part
                                   (llm-pi-ai-text-content-text block)))
                  (seq-remove (lambda (block)
                                (llm-pi-ai-oc--blank-p
                                 (llm-pi-ai-text-content-text block)))
                              (seq-filter #'llm-pi-ai-text-content-p blocks))))
         (assistant-text (mapconcat (lambda (part) (alist-get 'text part))
                                    text-parts ""))
         (thinking-blocks (seq-filter #'llm-pi-ai-thinking-content-p blocks))
         (tool-calls (seq-filter #'llm-pi-ai-tool-call-p blocks))
         (signed-details
          (cl-some (lambda (block)
                     (llm-pi-ai-oc--parse-reasoning-details
                      (llm-pi-ai-thinking-content-thinking-signature block)))
                   thinking-blocks))
         (legacy-details
          (delq nil (mapcar (lambda (tool-call)
                              (llm-pi-ai-oc--parse-legacy-encrypted-detail
                               (llm-pi-ai-tool-call-thought-signature tool-call)))
                            tool-calls)))
         (preserved (or signed-details legacy-details))
         (non-empty-thinking
          (seq-remove (lambda (block)
                        (llm-pi-ai-oc--blank-p
                         (llm-pi-ai-thinking-content-thinking block)))
                      thinking-blocks)))
    (cond
     (non-empty-thinking
      (if (plist-get compat :requires-thinking-as-text)
          ;; Thinking blocks become plain text (no tags, so the model does not
          ;; mimic them).
          (let ((thinking-text
                 (mapconcat (lambda (block)
                              (llm-pi-ai-sanitize-surrogates
                               (llm-pi-ai-thinking-content-thinking block)))
                            non-empty-thinking "\n\n")))
            (llm-pi-ai-oc--put message 'content
                               (vconcat (list (llm-pi-ai-oc--obj
                                               'type "text" 'text thinking-text))
                                        text-parts)))
        ;; Assistant content goes as a plain string, the standard format.
        (when (> (length assistant-text) 0)
          (llm-pi-ai-oc--put message 'content assistant-text))
        ;; reasoning_details is the structured alternative to a raw field.
        (unless preserved
          (let ((signature (llm-pi-ai-thinking-content-thinking-signature
                            (car non-empty-thinking))))
            (when (and (equal (llm-pi-ai-model-provider model) "opencode-go")
                       (equal signature "reasoning"))
              (setq signature "reasoning_content"))
            (when (member signature '("reasoning" "reasoning_content"
                                      "reasoning_text"))
              (llm-pi-ai-oc--put
               message (intern signature)
               (mapconcat #'llm-pi-ai-thinking-content-thinking
                          non-empty-thinking "\n")))))))
     ((> (length assistant-text) 0)
      (llm-pi-ai-oc--put message 'content assistant-text)))
    (when tool-calls
      (llm-pi-ai-oc--put
       message 'tool_calls
       (vconcat
        (mapcar
         (lambda (tool-call)
           (llm-pi-ai-oc--obj
            'id (llm-pi-ai-tool-call-id tool-call)
            'type "function"
            'function (llm-pi-ai-oc--obj
                       'name (llm-pi-ai-tool-call-name tool-call)
                       'arguments (llm-pi-ai-oc--serialize
                                   (or (llm-pi-ai-tool-call-arguments tool-call)
                                       (make-hash-table :test #'equal))))))
         tool-calls))))
    (when preserved
      (llm-pi-ai-oc--put message 'reasoning_details (vconcat preserved)))
    (when (and (plist-get compat :requires-reasoning-content-on-assistant-messages)
               (llm-pi-ai-model-reasoning model)
               (not (assq 'reasoning_content message)))
      (llm-pi-ai-oc--put message 'reasoning_content ""))
    ;; Skip assistant messages that have no content and no tool calls: some
    ;; providers require "either content or tool_calls, but not none".
    (let* ((content (alist-get 'content message))
           (has-content (and (not (eq content :null))
                             (> (length content) 0))))
      (when (or has-content (assq 'tool_calls message))
        message))))

(defun llm-pi-ai-api-openai-completions-convert-messages (model context compat)
  "Return the Chat Completions messages for CONTEXT as MODEL takes them.
The source's `convertMessages' (without its grammar tool options).
COMPAT is the plist `llm-pi-ai-api-openai-completions-get-compat' gives.
The result is a list of alists: see the file commentary for JSON."
  (let* ((normalized (llm-pi-ai-oc--resolve-transcript
                      context (plist-get compat :supports-mid-convo-system-messages)))
         (transformed
          (vconcat
           (llm-pi-ai-transform-messages
            (llm-pi-ai-context-messages normalized) model
            (lambda (id _model _source)
              (llm-pi-ai-oc--normalize-tool-call-id model id)))))
         (transcript-tools
          (llm-pi-ai-oc--resolve-transcript-tools
           (llm-pi-ai-context-messages normalized)
           (if (and (plist-get compat :supports-mid-convo-system-messages)
                    (plist-get compat :supports-mid-convo-tool-additions))
               'additions 'none)))
         (instruction-role (if (and (llm-pi-ai-model-reasoning model)
                                    (plist-get compat :supports-developer-role))
                               "developer" "system"))
         (count (length transformed))
         (params nil)
         (last-role nil)
         (system-index 0)
         (i 0))
    (while (< i count)
      (let ((msg (aref transformed i))
            (skip-last-role nil))
        ;; Some providers don't allow user messages directly after tool
        ;; results: a synthetic assistant message bridges the gap.
        (when (and (plist-get compat :requires-assistant-after-tool-result)
                   (equal last-role "toolResult")
                   (llm-pi-ai-user-message-p msg))
          (push (llm-pi-ai-oc--obj 'role "assistant"
                                   'content "I have processed the tool results.")
                params))
        (cond
         ((llm-pi-ai-system-message-p msg)
          ;; transformMessages keeps system messages in order, so their
          ;; position matches the transcript.
          (let ((added-tools (if (>= system-index (cdr transcript-tools))
                                 (llm-pi-ai-system-message-tools-added msg)
                               nil)))
            (setq system-index (1+ system-index))
            (when added-tools
              (push (llm-pi-ai-oc--obj
                     'role "system"
                     'tools (vconcat (llm-pi-ai-oc--convert-tools added-tools compat)))
                    params))
            (let ((text (if (= i 0)
                            (llm-pi-ai-system-message-text msg)
                          (llm-pi-ai-render-system-message-update msg))))
              (when (> (length text) 0)
                (push (llm-pi-ai-oc--obj 'role instruction-role
                                         'content (llm-pi-ai-sanitize-surrogates text))
                      params)))))
         ((llm-pi-ai-user-message-p msg)
          (let ((content (llm-pi-ai-message-content msg)))
            (if (stringp content)
                (push (llm-pi-ai-oc--obj 'role "user"
                                         'content (llm-pi-ai-sanitize-surrogates content))
                      params)
              (let ((parts
                     (mapcar
                      (lambda (item)
                        (if (llm-pi-ai-text-content-p item)
                            (llm-pi-ai-oc--text-part (llm-pi-ai-text-content-text item))
                          (llm-pi-ai-oc--image-url-part item)))
                      (seq-remove (lambda (item)
                                    (and (llm-pi-ai-text-content-p item)
                                         (string-empty-p
                                          (or (llm-pi-ai-text-content-text item) ""))))
                                  content))))
                (if (null parts)
                    (setq skip-last-role t)
                  (push (llm-pi-ai-oc--obj 'role "user" 'content (vconcat parts))
                        params))))))
         ((llm-pi-ai-assistant-message-p msg)
          (let ((converted (llm-pi-ai-oc--convert-assistant model compat msg)))
            (if converted
                (push converted params)
              (setq skip-last-role t))))
         ((llm-pi-ai-tool-result-message-p msg)
          (let ((image-blocks nil)
                (j i))
            (while (and (< j count)
                        (llm-pi-ai-tool-result-message-p (aref transformed j)))
              (let* ((tool-msg (aref transformed j))
                     (blocks (llm-pi-ai-message-content tool-msg))
                     (text-result (mapconcat
                                   (lambda (block)
                                     (or (llm-pi-ai-text-content-text block) ""))
                                   (seq-filter #'llm-pi-ai-text-content-p blocks)
                                   "\n"))
                     (has-images (seq-some #'llm-pi-ai-image-content-p blocks))
                     (has-text (not (llm-pi-ai-oc--blank-p text-result)))
                     (result-text (cond (has-text text-result)
                                        (has-images "(see attached image)")
                                        (t "(no tool output)")))
                     (result (llm-pi-ai-oc--obj
                              'role "tool"
                              'content (llm-pi-ai-sanitize-surrogates result-text)
                              'tool_call_id (llm-pi-ai-tool-result-message-tool-call-id
                                             tool-msg))))
                ;; Some providers require the 'name' field in tool results.
                (when (and (plist-get compat :requires-tool-result-name)
                           (llm-pi-ai-oc--truthy
                                (llm-pi-ai-tool-result-message-tool-name tool-msg)))
                  (llm-pi-ai-oc--put result 'name
                                     (llm-pi-ai-tool-result-message-tool-name
                                      tool-msg)))
                (push result params)
                (when (and has-images (member "image" (llm-pi-ai-model-input model)))
                  (dolist (block blocks)
                    (when (llm-pi-ai-image-content-p block)
                      (push (llm-pi-ai-oc--image-url-part block) image-blocks)))))
              (setq j (1+ j)))
            (setq i (1- j))
            (setq image-blocks (nreverse image-blocks))
            (if image-blocks
                (progn
                  (when (plist-get compat :requires-assistant-after-tool-result)
                    (push (llm-pi-ai-oc--obj
                           'role "assistant"
                           'content "I have processed the tool results.")
                          params))
                  (push (llm-pi-ai-oc--obj
                         'role "user"
                         'content (vconcat
                                   (list (llm-pi-ai-oc--obj
                                          'type "text"
                                          'text "Attached image(s) from tool result:"))
                                   image-blocks))
                        params)
                  (setq last-role "user"))
              (setq last-role "toolResult"))
            (setq skip-last-role t))))
        (unless skip-last-role
          (setq last-role (llm-pi-ai-message-role msg))))
      (setq i (1+ i)))
    (nreverse params)))

;;; Request body

(defun llm-pi-ai-oc--has-tool-history-p (messages)
  "Return non-nil when MESSAGES has a tool call or a tool result."
  (seq-some (lambda (message)
              (or (llm-pi-ai-tool-result-message-p message)
                  (and (llm-pi-ai-assistant-message-p message)
                       (seq-some #'llm-pi-ai-tool-call-p
                                 (llm-pi-ai-message-content message)))))
            messages))

(defun llm-pi-ai-oc--resolve-thinking-token-budget-field (compat)
  "Return the name of the budget field of COMPAT, a string, or nil."
  (cond ((plist-get compat :thinking-token-budget-field)
         (plist-get compat :thinking-token-budget-field))
        ((plist-get compat :supports-thinking-token-budget)
         "thinking_token_budget")))

(defun llm-pi-ai-oc--resolve-clamped-thinking-budget (model options params)
  "Return the thinking budget for OPTIONS under PARAMS' token ceiling, or nil.
The source's `resolveClampedThinkingBudget'."
  (let ((effort (plist-get options :reasoning-effort)))
    (when (and effort (llm-pi-ai-model-reasoning model))
      (let* ((ceiling (or (alist-get 'max_tokens params)
                          (alist-get 'max_completion_tokens params)
                          (llm-pi-ai-model-max-tokens model)))
             (budget (llm-pi-ai-oc--clamp-thinking-budget-to-answer-room
                      (llm-pi-ai-oc--thinking-budget-for-level
                       effort (plist-get options :thinking-budgets))
                      ceiling)))
        (and (> budget 0) budget)))))

(defun llm-pi-ai-oc--resolve-chat-template-value (model options value budget)
  "Return the request value of the chat template VALUE, or `:undefined'.
The source's `resolveChatTemplateKwargValue'.  VALUE is a compat entry:
a string, number, t, nil (false), `:null' or an alist with \"$var\"."
  (let ((effort (plist-get options :reasoning-effort)))
    (cond
     ((not (llm-pi-ai-oc--alist-like-p value))
      (cond ((null value) :false) (t value)))
     ((and (not effort)
           (llm-pi-ai-oc--truthy (cdr (assoc "omitWhenOff" value))))
      :undefined)
     ((equal (cdr (assoc "$var" value)) "thinking.enabled")
      (llm-pi-ai-oc--bool effort))
     ((equal (cdr (assoc "$var" value)) "thinking.budget")
      (or budget :undefined))
     (t
      (let ((mapped (if effort
                        (llm-pi-ai-oc--tlm model effort)
                      (llm-pi-ai-oc--tlm model "off"))))
        (cond ((eq mapped :undefined) (or effort :undefined))
              ((stringp mapped) mapped)
              (t :undefined)))))))

(defun llm-pi-ai-oc--build-chat-template-values (model options values budget)
  "Return the chat template values as an alist, or nil when none resolve.
The source's `buildChatTemplateValues'.  VALUES is an alist of compat
entries with string keys."
  (let (resolved)
    (dolist (entry values)
      (let ((value (llm-pi-ai-oc--resolve-chat-template-value
                    model options (cdr entry) budget)))
        (unless (eq value :undefined)
          (push (cons (intern (car entry)) value) resolved))))
    (nreverse resolved)))

(defun llm-pi-ai-oc--routing-json (routing)
  "Return ROUTING, a compat alist with string keys, as request JSON.
Lists are arrays, nil inside an object is false."
  (cond ((llm-pi-ai-oc--alist-like-p routing)
         (mapcar (lambda (entry)
                   (cons (intern (car entry))
                         (let ((value (cdr entry)))
                           (cond ((null value) :false)
                                 ((and (consp value) (not (llm-pi-ai-oc--alist-like-p value)))
                                  (vconcat value))
                                 (t (llm-pi-ai-oc--routing-json value))))))
                 routing))
        (t routing)))

(defun llm-pi-ai-oc--build-params (model context options compat cache-retention)
  "Return the request body for MODEL and CONTEXT, an alist.
The source's `buildParams'.  See the file commentary for the form of
OPTIONS, COMPAT and the result."
  (let* ((messages-in (llm-pi-ai-context-messages context))
         (transcript-tools
          (llm-pi-ai-oc--resolve-transcript-tools
           messages-in
           (if (and (eq (plist-get compat :supports-mid-convo-system-messages) t)
                    (eq (plist-get compat :supports-mid-convo-tool-additions) t))
               'additions 'none)))
         (messages (llm-pi-ai-api-openai-completions-convert-messages
                    model context compat))
         (cache-control (llm-pi-ai-oc--get-compat-cache-control
                         compat cache-retention))
         (effort (plist-get options :reasoning-effort))
         (long-cache (and (equal cache-retention "long")
                          (plist-get compat :supports-long-cache-retention)))
         (prompt-cache-key
          (and (or (and (llm-pi-ai-oc--includes (llm-pi-ai-model-base-url model)
                                                "api.openai.com")
                        (not (equal cache-retention "none")))
                   long-cache)
               (llm-pi-ai-clamp-openai-prompt-cache-key
                (plist-get options :session-id))))
         (params (llm-pi-ai-oc--obj 'model (llm-pi-ai-model-id model)
                                    'messages (vconcat messages)
                                    'stream t))
         (format (plist-get compat :thinking-format))
         (reasoning (llm-pi-ai-model-reasoning model))
         tools thinking-budget)
    (cl-macrolet ((put! (key value)
                    `(setq params (llm-pi-ai-oc--put params ,key ,value))))
      (when prompt-cache-key (put! 'prompt_cache_key prompt-cache-key))
      (when long-cache (put! 'prompt_cache_retention "24h"))
      (unless (null (plist-get compat :supports-usage-in-streaming))
        (put! 'stream_options (llm-pi-ai-oc--obj 'include_usage t)))
      (when (plist-get compat :supports-store)
        (put! 'store :false))
      (let ((max-tokens (plist-get options :max-tokens)))
        (when (llm-pi-ai-oc--truthy max-tokens)
          (if (equal (plist-get compat :max-tokens-field) "max_tokens")
              ;; Deprecated by OpenAI, but some compatible providers only
              ;; accept max_tokens.
              (put! 'max_tokens max-tokens)
            (put! 'max_completion_tokens max-tokens))))
      (when (plist-get options :temperature)
        (put! 'temperature (plist-get options :temperature)))
      (cond
       ((car transcript-tools)
        (setq tools (vconcat (llm-pi-ai-oc--convert-tools
                              (car transcript-tools) compat)))
        (put! 'tools tools)
        (when (plist-get compat :zai-tool-stream)
          (put! 'tool_stream t)))
       ((llm-pi-ai-oc--has-tool-history-p messages-in)
        ;; Anthropic (via LiteLLM/proxy) requires the tools parameter when
        ;; the conversation has tool calls or results.
        (setq tools (vector))
        (put! 'tools tools)))
      (when cache-control
        (llm-pi-ai-oc--apply-anthropic-cache-control messages tools cache-control))
      (when (llm-pi-ai-oc--truthy (plist-get options :tool-choice))
        (put! 'tool_choice (plist-get options :tool-choice)))
      (when (plist-get compat :vllm-priority)
        (put! 'priority (plist-get compat :vllm-priority)))
      (let ((budget-field (llm-pi-ai-oc--resolve-thinking-token-budget-field compat)))
        (setq thinking-budget
              (llm-pi-ai-oc--resolve-clamped-thinking-budget model options params))
        (cond
         ((and (equal format "zai") reasoning)
          (put! 'thinking (if effort
                              (llm-pi-ai-oc--obj 'type "enabled"
                                                 'clear_thinking :false)
                            (llm-pi-ai-oc--obj 'type "disabled")))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (let* ((mapped (llm-pi-ai-oc--tlm model effort))
                   (value (if (eq mapped :undefined) effort mapped)))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "qwen") reasoning)
          (put! 'enable_thinking (llm-pi-ai-oc--bool effort))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (let ((value (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                           (if (llm-pi-ai-oc--nullish-p mapped) effort mapped))))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "qwen-chat-template") reasoning)
          (put! 'chat_template_kwargs
                (llm-pi-ai-oc--obj 'enable_thinking (llm-pi-ai-oc--bool effort)
                                   'preserve_thinking t)))
         ((and (equal format "chat-template") reasoning)
          (let ((values (llm-pi-ai-oc--build-chat-template-values
                         model options (plist-get compat :chat-template-kwargs)
                         thinking-budget)))
            (when values (put! 'chat_template_kwargs values))))
         ((and (equal format "baseten") reasoning)
          (let ((values (llm-pi-ai-oc--build-chat-template-values
                         model options (plist-get compat :chat-template-args)
                         thinking-budget)))
            (when values (put! 'chat_template_args values)))
          (when (plist-get compat :supports-reasoning-effort)
            (let* ((mapped (if effort
                               (llm-pi-ai-oc--tlm model effort)
                             (llm-pi-ai-oc--tlm model "off")))
                   (value (if (eq mapped :undefined) effort mapped)))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "deepseek") reasoning)
          (cond (effort (put! 'thinking (llm-pi-ai-oc--obj 'type "enabled")))
                ((not (eq (llm-pi-ai-oc--tlm model "off") :null))
                 (put! 'thinking (llm-pi-ai-oc--obj 'type "disabled"))))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (put! 'reasoning_effort
                  (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                    (if (llm-pi-ai-oc--nullish-p mapped) effort mapped)))))
         ((and (equal format "openrouter") reasoning)
          ;; OpenRouter normalizes reasoning across providers via a nested
          ;; reasoning object.
          (cond (effort
                 (put! 'reasoning
                       (llm-pi-ai-oc--obj
                        'effort (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                                  (if (llm-pi-ai-oc--nullish-p mapped) effort mapped)))))
                ((not (eq (llm-pi-ai-oc--tlm model "off") :null))
                 (put! 'reasoning
                       (llm-pi-ai-oc--obj
                        'effort (let ((off (llm-pi-ai-oc--tlm model "off")))
                                  (if (llm-pi-ai-oc--nullish-p off) "none" off)))))))
         ((and (equal format "ant-ling") reasoning effort)
          (let ((value (llm-pi-ai-oc--tlm model effort)))
            (when (stringp value)
              (put! 'reasoning (llm-pi-ai-oc--obj 'effort value)))))
         ((and (equal format "together") reasoning)
          (put! 'reasoning (llm-pi-ai-oc--obj 'enabled (llm-pi-ai-oc--bool effort)))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (put! 'reasoning_effort
                  (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                    (if (llm-pi-ai-oc--nullish-p mapped) effort mapped)))))
         ((and (equal format "string-thinking") reasoning)
          (cond (effort
                 (put! 'thinking
                       (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                         (if (llm-pi-ai-oc--nullish-p mapped) effort mapped))))
                ((not (eq (llm-pi-ai-oc--tlm model "off") :null))
                 (put! 'thinking
                       (let ((off (llm-pi-ai-oc--tlm model "off")))
                         (if (llm-pi-ai-oc--nullish-p off) "none" off))))))
         ((and effort reasoning (plist-get compat :supports-reasoning-effort))
          ;; OpenAI-style reasoning_effort
          (put! 'reasoning_effort
                (let ((mapped (llm-pi-ai-oc--tlm model effort)))
                  (if (llm-pi-ai-oc--nullish-p mapped) effort mapped))))
         ((and (not effort) reasoning (plist-get compat :supports-reasoning-effort))
          (let ((off (llm-pi-ai-oc--tlm model "off")))
            (when (stringp off) (put! 'reasoning_effort off)))))
        ;; Cap reasoning with a top-level budget field.  Independent of
        ;; thinkingFormat: reasoning and the answer share max_tokens here, so an
        ;; uncapped reasoning phase could leave no answer and no tool call.
        (when (and budget-field thinking-budget)
          (put! (intern budget-field) thinking-budget)))
      ;; OpenRouter provider routing preferences
      (let ((routing (assq 'open-router-routing (llm-pi-ai-model-compat model))))
        (when (and routing (not (eq (cdr routing) :null)))
          (put! 'provider (llm-pi-ai-oc--routing-json (cdr routing)))))
      ;; Vercel AI Gateway provider routing preferences
      (let ((routing (assq 'vercel-gateway-routing (llm-pi-ai-model-compat model))))
        (when (and routing (not (eq (cdr routing) :null)))
          (let* ((only (cdr (assoc "only" (cdr routing))))
                 (order (cdr (assoc "order" (cdr routing)))))
            (when (or only order)
              (let (gateway)
                (when only (setq gateway (append gateway (list (cons 'only (vconcat only))))))
                (when order (setq gateway (append gateway (list (cons 'order (vconcat order))))))
                (put! 'providerOptions (llm-pi-ai-oc--obj 'gateway gateway)))))))
      ;; Last, so model and request sampling parameters override named
      ;; request fields.
      (dolist (entry (llm-pi-ai-oc--resolve-sampling-params
                      model (or effort "off") (plist-get options :sampling-params)))
        (put! (intern (car entry))
              (if (null (cdr entry)) :false (cdr entry)))))
    params))

;;; Usage and stop reasons

(defun llm-pi-ai-oc--parse-chunk-usage (raw-usage model)
  "Return the `llm-pi-ai-usage' of RAW-USAGE, a chunk's usage object.
The source's `parseChunkUsage'; the cost is calculated for MODEL."
  (let* ((details (llm-pi-ai-oc--jget raw-usage "prompt_tokens_details"))
         (prompt-tokens (or (llm-pi-ai-oc--jget raw-usage "prompt_tokens") 0))
         (cache-read (or (llm-pi-ai-oc--jget details "cached_tokens")
                         (llm-pi-ai-oc--jget raw-usage "prompt_cache_hit_tokens")
                         (llm-pi-ai-oc--jget raw-usage "cached_tokens")
                         0))
         (cache-write (or (llm-pi-ai-oc--jget details "cache_write_tokens") 0))
         ;; Cached tokens are cache-read tokens (hits); writes are separate and
         ;; not subtracted from them.
         (input (max 0 (- prompt-tokens cache-read cache-write)))
         ;; completion_tokens already includes reasoning_tokens.
         (output (or (llm-pi-ai-oc--jget raw-usage "completion_tokens") 0))
         (usage (llm-pi-ai-make-usage
                 :input input
                 :output output
                 :cache-read cache-read
                 :cache-write cache-write
                 :reasoning (or (llm-pi-ai-oc--jget
                                 (llm-pi-ai-oc--jget raw-usage
                                                     "completion_tokens_details")
                                 "reasoning_tokens")
                                0)
                 :total-tokens (+ input output cache-read cache-write)
                 :cost (llm-pi-ai-make-cost))))
    (llm-pi-ai-calculate-cost model usage)
    usage))

(defun llm-pi-ai-oc--map-stop-reason (reason)
  "Return (STOP-REASON . ERROR-MESSAGE) for the finish REASON.
The source's `mapStopReason'; the message is nil for no error."
  (cond
   ((member reason '("stop" "end")) (cons "stop" nil))
   ((equal reason "length") (cons "length" nil))
   ((member reason '("function_call" "tool_calls")) (cons "toolUse" nil))
   ((equal reason "content_filter")
    (cons "error" "Provider finish_reason: content_filter"))
   ((equal reason "network_error")
    (cons "error" "Provider finish_reason: network_error"))
   (t (cons "error" (format "Provider finish_reason: %s" reason)))))

;;; Errors of a request

(defun llm-pi-ai-oc--json-text (value)
  "Return VALUE, a parsed JSON value, as `JSON.stringify' writes it."
  (llm-pi-ai-safe-json-stringify value))

(defun llm-pi-ai-oc--sdk-message (status error message)
  "Return the message of the `openai' SDK's `APIError' for these parts.
The source of the text is `APIError.makeMessage': ERROR is the parsed
`error' member of the body, MESSAGE the body text when it was no JSON."
  (let ((text (cond
               ((and (hash-table-p error)
                     (llm-pi-ai-oc--truthy (gethash "message" error)))
                (let ((m (gethash "message" error)))
                  (if (stringp m) m (llm-pi-ai-oc--json-text m))))
               ((llm-pi-ai-oc--truthy error) (llm-pi-ai-oc--json-text error))
               (t message))))
    (cond ((and status text (llm-pi-ai-oc--truthy text))
           (format "%d %s" status text))
          (status (format "%d status code (no body)" status))
          ((llm-pi-ai-oc--truthy text) text)
          (t "(no status code or body)"))))

(defun llm-pi-ai-oc--status-error (status headers body-text)
  "Return the provider error plist for an HTTP STATUS, HEADERS and BODY-TEXT.
It has the keys `llm-pi-ai-retry-provider-request' and
`llm-pi-ai-normalize-provider-error' read.  What `message' and `error'
hold is what the `openai' SDK puts on its `APIError'."
  (let* ((json (and (> (length body-text) 0)
                    (let ((parsed (llm-pi-ai-oc--parse-json-or-nil body-text)))
                      (and (not (eq parsed :invalid)) parsed))))
         (json-p (llm-pi-ai-oc--truthy json))
         (error (and (hash-table-p json) (llm-pi-ai-oc--jget json "error")))
         (text (and (not json-p) body-text)))
    (list :message (llm-pi-ai-oc--sdk-message status error text)
          :status status :headers headers :error error)))

(defun llm-pi-ai-oc--connection-error (result timed-out)
  "Return the provider error plist for RESULT, the seam's `(:error ...)'.
TIMED-OUT says the request's own timer ended it."
  (let ((kind (plist-get result :error)))
    (list :message
          (cond ((or timed-out (eq kind 'timeout)) "Request timed out.")
                ((eq kind 'aborted) "Request was aborted.")
                (t (format "Connection error: %s" (plist-get result :message))))
          :status nil :headers nil)))

;;; The stream

(cl-defstruct (llm-pi-ai-oc--state
               (:constructor llm-pi-ai-oc--make-state)
               (:copier nil))
  "The state of one call of `llm-pi-ai-api-openai-completions-stream'."
  stream model options compat output
  (finished nil) handle signal listener timer
  (details nil) text-block thinking-block (has-finish-reason nil)
  (tool-state (make-hash-table :test #'eq))
  (by-index (make-hash-table :test #'eql))
  (by-id (make-hash-table :test #'equal))
  sse (done-seen nil) context)

(cl-defstruct (llm-pi-ai-oc--tool
               (:constructor llm-pi-ai-oc--make-tool)
               (:copier nil))
  "Scratch state of a streaming tool call block: what the source keeps
in the block's `partialArgs' and `streamIndex'."
  block (partial-args "") index)

(defun llm-pi-ai-oc--emit (st type &rest keys)
  "Push an event of TYPE with `partial' and KEYS onto the stream of ST."
  (llm-pi-ai-event-stream-push
   (llm-pi-ai-oc--state-stream st)
   (apply #'llm-pi-ai-make-event :type type
          :partial (llm-pi-ai-oc--state-output st) keys)))

(defun llm-pi-ai-oc--content-index (st block)
  "Return the index of BLOCK in the content of the message of ST."
  (cl-position block (llm-pi-ai-message-content (llm-pi-ai-oc--state-output st))
               :test #'eq))

(defun llm-pi-ai-oc--add-block (st block)
  "Add BLOCK at the end of the content of the message of ST."
  (let ((output (llm-pi-ai-oc--state-output st)))
    (setf (llm-pi-ai-message-content output)
          (append (llm-pi-ai-message-content output) (list block)))))

(defun llm-pi-ai-oc--apply-reasoning-details (st block)
  "Store the streamed reasoning details of ST as the signature of BLOCK.
The source's `applyStreamedReasoningDetails'."
  (when (llm-pi-ai-oc--state-details st)
    (setf (llm-pi-ai-thinking-content-thinking-signature block)
          (llm-pi-ai-oc--serialize
           (vconcat (llm-pi-ai-oc--state-details st))))))

(defun llm-pi-ai-oc--finish-block (st block)
  "Finish BLOCK, a content block of ST, with its `*_end' event.
The source's `finishBlock'."
  (let ((index (llm-pi-ai-oc--content-index st block)))
    (when index
      (cond
       ((llm-pi-ai-text-content-p block)
        (llm-pi-ai-oc--emit st "text_end" :content-index index
                            :content (llm-pi-ai-text-content-text block)))
       ((llm-pi-ai-thinking-content-p block)
        (llm-pi-ai-oc--apply-reasoning-details st block)
        (llm-pi-ai-oc--emit st "thinking_end" :content-index index
                            :content (llm-pi-ai-thinking-content-thinking block)))
       ((llm-pi-ai-tool-call-p block)
        (let* ((scratch (gethash block (llm-pi-ai-oc--state-tool-state st)))
               (parsed (llm-pi-ai-parse-tool-call-arguments
                        (and scratch (llm-pi-ai-oc--tool-partial-args scratch)))))
          (setf (llm-pi-ai-tool-call-arguments block) (cdr (assq 'arguments parsed)))
          (when (assq 'arguments-error parsed)
            (setf (llm-pi-ai-tool-call-arguments-error block)
                  (cdr (assq 'arguments-error parsed))))
          ;; Finalize in place and drop the scratch state, so a replay only
          ;; carries parsed arguments.
          (remhash block (llm-pi-ai-oc--state-tool-state st))
          (llm-pi-ai-oc--emit st "toolcall_end" :content-index index
                              :tool-call block)))))))

(defun llm-pi-ai-oc--ensure-text-block (st)
  "Return the text block of ST, starting it when it has none."
  (or (llm-pi-ai-oc--state-text-block st)
      (let ((block (llm-pi-ai-make-text-content :text "")))
        (setf (llm-pi-ai-oc--state-text-block st) block)
        (llm-pi-ai-oc--add-block st block)
        (llm-pi-ai-oc--emit st "text_start"
                            :content-index (llm-pi-ai-oc--content-index st block))
        block)))

(defun llm-pi-ai-oc--ensure-thinking-block (st signature)
  "Return the thinking block of ST, starting it with SIGNATURE if none."
  (or (llm-pi-ai-oc--state-thinking-block st)
      (let ((block (llm-pi-ai-make-thinking-content
                    :thinking "" :thinking-signature signature)))
        (setf (llm-pi-ai-oc--state-thinking-block st) block)
        (llm-pi-ai-oc--add-block st block)
        (llm-pi-ai-oc--emit st "thinking_start"
                            :content-index (llm-pi-ai-oc--content-index st block))
        block)))

(defun llm-pi-ai-oc--ensure-tool-call-block (st delta)
  "Return the scratch state of the tool call block that DELTA continues.
DELTA is the parsed tool call delta.  A block is started when none has
its stream index or its id.  The source's `ensureToolCallBlock'."
  (let* ((index (let ((i (gethash "index" delta))) (and (numberp i) i)))
         (id (llm-pi-ai-oc--jget delta "id"))
         (function (llm-pi-ai-oc--jget delta "function"))
         (name (or (llm-pi-ai-oc--jget function "name") ""))
         (scratch (or (and index (gethash index (llm-pi-ai-oc--state-by-index st)))
                      (and id (gethash id (llm-pi-ai-oc--state-by-id st))))))
    (unless scratch
      (let ((block (llm-pi-ai-make-tool-call
                    :id (or id "") :name name
                    :arguments (llm-pi-ai-json-object))))
        (setq scratch (llm-pi-ai-oc--make-tool :block block :index index))
        (puthash block scratch (llm-pi-ai-oc--state-tool-state st))
        (when index (puthash index scratch (llm-pi-ai-oc--state-by-index st)))
        (when id (puthash id scratch (llm-pi-ai-oc--state-by-id st)))
        (llm-pi-ai-oc--add-block st block)
        (llm-pi-ai-oc--emit st "toolcall_start"
                            :content-index (llm-pi-ai-oc--content-index st block))))
    (let ((block (llm-pi-ai-oc--tool-block scratch)))
      (when (and index (null (llm-pi-ai-oc--tool-index scratch)))
        (setf (llm-pi-ai-oc--tool-index scratch) index)
        (puthash index scratch (llm-pi-ai-oc--state-by-index st)))
      (when id (puthash id scratch (llm-pi-ai-oc--state-by-id st)))
      (when (and (string-empty-p (or (llm-pi-ai-tool-call-name block) ""))
                 (> (length name) 0))
        (setf (llm-pi-ai-tool-call-name block) name))
      scratch)))

(defun llm-pi-ai-oc--process-tool-call-delta (st delta)
  "Apply DELTA, one entry of a delta's `tool_calls', to the blocks of ST."
  (when (hash-table-p delta)
    (let* ((scratch (llm-pi-ai-oc--ensure-tool-call-block st delta))
           (block (llm-pi-ai-oc--tool-block scratch))
           (id (llm-pi-ai-oc--jget delta "id"))
           (function (llm-pi-ai-oc--jget delta "function"))
           (name (llm-pi-ai-oc--jget function "name"))
           (arguments (llm-pi-ai-oc--jget function "arguments"))
           (text ""))
      (when (and (string-empty-p (or (llm-pi-ai-tool-call-id block) "")) id)
        (setf (llm-pi-ai-tool-call-id block) id)
        (puthash id scratch (llm-pi-ai-oc--state-by-id st)))
      (when (and (string-empty-p (or (llm-pi-ai-tool-call-name block) "")) name)
        (setf (llm-pi-ai-tool-call-name block) name))
      (when (and (stringp arguments) (> (length arguments) 0))
        (setq text arguments)
        (setf (llm-pi-ai-oc--tool-partial-args scratch)
              (concat (llm-pi-ai-oc--tool-partial-args scratch) arguments))
        (setf (llm-pi-ai-tool-call-arguments block)
              (llm-pi-ai-parse-streaming-json
               (llm-pi-ai-oc--tool-partial-args scratch))))
      (llm-pi-ai-oc--emit st "toolcall_delta"
                          :content-index (llm-pi-ai-oc--content-index st block)
                          :delta text))))

(defun llm-pi-ai-oc--process-delta (st delta)
  "Apply DELTA, a choice's parsed `delta', to the message of ST."
  ;; Reasoning goes before content: some proxies aggregate reasoning and
  ;; answer into one delta, and the thinking block must precede the text.
  ;; The first non-empty reasoning field wins, to avoid duplicates.
  (let ((field (seq-find (lambda (name)
                           (let ((value (gethash name delta)))
                             (and (stringp value) (> (length value) 0))))
                         '("reasoning_content" "reasoning" "reasoning_text"))))
    (when field
      (let* ((text (gethash field delta))
             (signature (if (and (equal (llm-pi-ai-model-provider
                                         (llm-pi-ai-oc--state-model st))
                                        "opencode-go")
                                 (equal field "reasoning"))
                            "reasoning_content"
                          field))
             (block (llm-pi-ai-oc--ensure-thinking-block st signature)))
        (setf (llm-pi-ai-thinking-content-thinking block)
              (concat (llm-pi-ai-thinking-content-thinking block) text))
        (llm-pi-ai-oc--emit st "thinking_delta"
                            :content-index (llm-pi-ai-oc--content-index st block)
                            :delta text))))
  (let ((details (gethash "reasoning_details" delta)))
    (when (vectorp details)
      (seq-doseq (detail details)
        (when (llm-pi-ai-oc--detail-p detail)
          (llm-pi-ai-oc--ensure-thinking-block st "")
          ;; Replay data stays in the signature slot; text and summary deltas
          ;; merge into logical entries, encrypted ones stay discrete.
          (setf (llm-pi-ai-oc--state-details st)
                (llm-pi-ai-oc--append-detail
                 (llm-pi-ai-oc--state-details st) detail))))))
  (let ((content (gethash "content" delta)))
    (when (and (stringp content) (> (length content) 0))
      (let ((block (llm-pi-ai-oc--ensure-text-block st)))
        (setf (llm-pi-ai-text-content-text block)
              (concat (llm-pi-ai-text-content-text block) content))
        (llm-pi-ai-oc--emit st "text_delta"
                            :content-index (llm-pi-ai-oc--content-index st block)
                            :delta content))))
  (let ((tool-calls (gethash "tool_calls" delta)))
    (when (vectorp tool-calls)
      (seq-doseq (tool-call tool-calls)
        (llm-pi-ai-oc--process-tool-call-delta st tool-call)))))

(defun llm-pi-ai-oc--process-chunk (st chunk)
  "Apply CHUNK, a parsed `ChatCompletionChunk', to the message of ST.
This is the body of the source's `for await (const chunk ...)' loop."
  (let ((model (llm-pi-ai-oc--state-model st))
        (output (llm-pi-ai-oc--state-output st))
        (hook (plist-get (llm-pi-ai-oc--state-options st)
                         :on-provider-stream-event)))
    (when hook (funcall hook chunk model))
    (when (hash-table-p chunk)
      ;; Each chunk of a completion carries the same id.
      (unless (llm-pi-ai-oc--truthy (llm-pi-ai-assistant-message-response-id output))
        (setf (llm-pi-ai-assistant-message-response-id output)
              (llm-pi-ai-oc--jget chunk "id")))
      (let ((chunk-model (gethash "model" chunk)))
        (when (and (stringp chunk-model) (> (length chunk-model) 0)
                   (not (equal chunk-model (llm-pi-ai-model-id model)))
                   (not (llm-pi-ai-oc--truthy
                         (llm-pi-ai-assistant-message-response-model output))))
          (setf (llm-pi-ai-assistant-message-response-model output) chunk-model)))
      (when (llm-pi-ai-oc--jget chunk "usage")
        (setf (llm-pi-ai-assistant-message-usage output)
              (llm-pi-ai-oc--parse-chunk-usage (llm-pi-ai-oc--jget chunk "usage")
                                               model)))
      (let* ((choices (gethash "choices" chunk))
             (choice (and (vectorp choices) (> (length choices) 0)
                          (aref choices 0))))
        (when (hash-table-p choice)
          ;; Some providers (e.g. Moonshot) put usage in choice.usage.
          (when (and (not (llm-pi-ai-oc--jget chunk "usage"))
                     (llm-pi-ai-oc--jget choice "usage"))
            (setf (llm-pi-ai-assistant-message-usage output)
                  (llm-pi-ai-oc--parse-chunk-usage
                   (llm-pi-ai-oc--jget choice "usage") model)))
          (let ((finish-reason (llm-pi-ai-oc--jget choice "finish_reason")))
            (when (llm-pi-ai-oc--truthy finish-reason)
              (setf (llm-pi-ai-assistant-message-raw-stop-reason output)
                    finish-reason)
              (let ((mapped (llm-pi-ai-oc--map-stop-reason finish-reason)))
                (setf (llm-pi-ai-assistant-message-stop-reason output) (car mapped))
                (when (cdr mapped)
                  (setf (llm-pi-ai-assistant-message-error-message output)
                        (cdr mapped))))
              (setf (llm-pi-ai-oc--state-has-finish-reason st) t)))
          (let ((delta (llm-pi-ai-oc--jget choice "delta")))
            (when (hash-table-p delta)
              (llm-pi-ai-oc--process-delta st delta))))))))

;;; Ending the stream

(defun llm-pi-ai-oc--cleanup (st)
  "Release what ST holds: its abort listener, timer and HTTP request."
  (let ((signal (llm-pi-ai-oc--state-signal st))
        (listener (llm-pi-ai-oc--state-listener st))
        (timer (llm-pi-ai-oc--state-timer st))
        (handle (llm-pi-ai-oc--state-handle st)))
    (when (and signal listener)
      (llm-pi-ai-abort-signal-remove-listener signal listener))
    (setf (llm-pi-ai-oc--state-listener st) nil)
    (when timer
      (funcall llm-pi-ai-retry-cancel-timer-function timer)
      (setf (llm-pi-ai-oc--state-timer st) nil))
    (when handle
      (setf (llm-pi-ai-oc--state-handle st) nil)
      (llm-pi-ai-http-abort handle))))

(defun llm-pi-ai-oc--fail (st err)
  "End the stream of ST with an `error' event for ERR.
This is the `catch' of the source's `stream'.  ERR is a Lisp error
condition or an error plist.  Do nothing when the stream has ended."
  (unless (llm-pi-ai-oc--state-finished st)
    (setf (llm-pi-ai-oc--state-finished st) t)
    (llm-pi-ai-oc--cleanup st)
    (let ((output (llm-pi-ai-oc--state-output st))
          (signal (llm-pi-ai-oc--state-signal st)))
      (dolist (block (llm-pi-ai-message-content output))
        (when (llm-pi-ai-thinking-content-p block)
          (llm-pi-ai-oc--apply-reasoning-details st block)))
      (setf (llm-pi-ai-assistant-message-stop-reason output)
            (if (and signal (llm-pi-ai-abort-signal-aborted-p signal))
                "aborted" "error"))
      (setf (llm-pi-ai-assistant-message-error-message output)
            (llm-pi-ai-format-provider-error
             (llm-pi-ai-normalize-provider-error err)))
      ;; Some providers via OpenRouter give more information in this field.
      ;; The parsed body is already in the message, so the raw metadata is
      ;; appended only when it is not there yet.
      (let* ((error-object (and (consp err) (keywordp (car err))
                                (plist-get err :error)))
             (metadata (llm-pi-ai-oc--jget error-object "metadata"))
             (raw (llm-pi-ai-oc--jget metadata "raw")))
        (when (llm-pi-ai-oc--truthy raw)
          (let ((raw-text (if (stringp raw) raw
                            (if (hash-table-p raw) "[object Object]"
                              (format "%s" raw)))))
            (unless (llm-pi-ai-oc--includes
                     (llm-pi-ai-assistant-message-error-message output) raw-text)
              (setf (llm-pi-ai-assistant-message-error-message output)
                    (concat (llm-pi-ai-assistant-message-error-message output)
                            "\n" raw-text))))))
      (llm-pi-ai-event-stream-push
       (llm-pi-ai-oc--state-stream st)
       (llm-pi-ai-make-event
        :type "error"
        :reason (llm-pi-ai-assistant-message-stop-reason output)
        :error output))
      (llm-pi-ai-event-stream-end (llm-pi-ai-oc--state-stream st)))))

(defun llm-pi-ai-oc--complete (st)
  "End the stream of ST normally, after the last chunk.
The part of the source's `stream' after its loop; an error it throws
goes to `llm-pi-ai-oc--fail' in the caller."
  (let* ((output (llm-pi-ai-oc--state-output st))
         (compat (llm-pi-ai-oc--state-compat st))
         (signal (llm-pi-ai-oc--state-signal st)))
    (dolist (block (copy-sequence (llm-pi-ai-message-content output)))
      (llm-pi-ai-oc--finish-block st block))
    (when (and signal (llm-pi-ai-abort-signal-aborted-p signal))
      (error "Request was aborted"))
    (when (equal (llm-pi-ai-assistant-message-stop-reason output) "aborted")
      (error "Request was aborted"))
    (when (and (not (llm-pi-ai-oc--state-has-finish-reason st))
               (not (plist-get compat :supports-finish-reason)))
      (setf (llm-pi-ai-assistant-message-stop-reason output)
            (if (seq-some #'llm-pi-ai-tool-call-p
                          (llm-pi-ai-message-content output))
                "toolUse" "stop")))
    (when (equal (llm-pi-ai-assistant-message-stop-reason output) "error")
      (let ((message (llm-pi-ai-assistant-message-error-message output)))
        (error "%s" (if (llm-pi-ai-oc--truthy message)
                        message
                      "Provider returned an error stop reason"))))
    (when (or (and (plist-get compat :supports-finish-reason)
                   (not (llm-pi-ai-oc--state-has-finish-reason st)))
              (equal (llm-pi-ai-assistant-message-stop-reason output) "pending"))
      (error "Stream ended without finish_reason"))
    (setf (llm-pi-ai-oc--state-finished st) t)
    (llm-pi-ai-oc--cleanup st)
    (llm-pi-ai-event-stream-push
     (llm-pi-ai-oc--state-stream st)
     (llm-pi-ai-make-event
      :type "done" :reason (llm-pi-ai-assistant-message-stop-reason output)
      :message output))
    (llm-pi-ai-event-stream-end (llm-pi-ai-oc--state-stream st))))

;;; Reading the response

(defun llm-pi-ai-oc--on-sse (st _event data)
  "Take one server-sent event with DATA for ST: a chunk, `[DONE]' or an error."
  (unless (or (llm-pi-ai-oc--state-finished st)
              (llm-pi-ai-oc--state-done-seen st))
    (if (string-prefix-p "[DONE]" data)
        (setf (llm-pi-ai-oc--state-done-seen st) t)
      (let ((chunk (json-parse-string data :object-type 'hash-table
                                      :array-type 'array :null-object :null
                                      :false-object :false)))
        (let ((error-object (and (hash-table-p chunk)
                                 (llm-pi-ai-oc--jget chunk "error"))))
          (if (llm-pi-ai-oc--truthy error-object)
              ;; The SDK throws an APIError without a status for such a chunk.
              (llm-pi-ai-oc--fail
               st (list :message (llm-pi-ai-oc--sdk-message nil error-object nil)
                        :status nil :headers nil :error error-object))
            (llm-pi-ai-oc--process-chunk st chunk)))))))

(defun llm-pi-ai-oc--on-body (st text)
  "Take TEXT, a piece of the response body, for ST."
  (unless (llm-pi-ai-oc--state-finished st)
    (condition-case err
        (funcall (llm-pi-ai-oc--state-sse st) text)
      (error (llm-pi-ai-oc--fail st err)))))

(defun llm-pi-ai-oc--on-end (st)
  "Take the end of the response body for ST."
  (unless (llm-pi-ai-oc--state-finished st)
    (condition-case err
        (llm-pi-ai-oc--complete st)
      (error (llm-pi-ai-oc--fail st err)))))

(defun llm-pi-ai-oc--attempt (st url headers body done)
  "Make one request for ST and call DONE with its outcome.
DONE is the continuation `llm-pi-ai-retry-provider-request' gives its
request function: it gets `(:ok (STATUS HEADERS))' as soon as the
response headers say success, or `(:error PLIST)'.  The body then goes
to ST.  This is the `client.chat.completions.create(...).withResponse()'
of the source."
  (let ((signal (llm-pi-ai-oc--state-signal st))
        (timeout-ms (plist-get (llm-pi-ai-oc--state-options st) :timeout-ms))
        (status nil) (response-headers nil) (error-chunks nil)
        (streaming nil) (timed-out nil) (timer nil))
    (if (and signal (llm-pi-ai-abort-signal-aborted-p signal))
        (funcall done (list :error (list :message "Request was aborted."
                                         :status nil :headers nil)))
      (let ((stop-timer (lambda ()
                          (when timer
                            (funcall llm-pi-ai-retry-cancel-timer-function timer)
                            (setq timer nil)))))
        (setf (llm-pi-ai-oc--state-handle st)
              (llm-pi-ai-http-request
               :url url :method "POST" :headers headers :body body
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
                     (llm-pi-ai-oc--on-body st text)
                   (push text error-chunks)))
               :on-done
               (lambda (result)
                 (funcall stop-timer)
                 (cond
                  ((plist-get result :status)
                   (if streaming
                       (llm-pi-ai-oc--on-end st)
                     (funcall done
                              (list :error
                                    (llm-pi-ai-oc--status-error
                                     (plist-get result :status)
                                     (plist-get result :headers)
                                     (apply #'concat (reverse error-chunks)))))))
                  (streaming
                   (llm-pi-ai-oc--fail
                    st (llm-pi-ai-oc--connection-error result timed-out)))
                  (status
                   (funcall done
                            (list :error
                                  (llm-pi-ai-oc--status-error
                                   status response-headers
                                   (apply #'concat (reverse error-chunks))))))
                  (t
                   (funcall done
                            (list :error (llm-pi-ai-oc--connection-error
                                          result timed-out))))))))
        ;; The SDK's timeout covers the time until the headers arrive.
        (when timeout-ms
          (setq timer
                (llm-pi-ai--start-timer
                 timeout-ms
                 (lambda ()
                   (setq timer nil timed-out t)
                   (let ((handle (llm-pi-ai-oc--state-handle st)))
                     (when handle (llm-pi-ai-http-abort handle)))))))))))

(defun llm-pi-ai-oc--on-response (st result)
  "Take RESULT of the request of ST: the source's code after `await retry...'."
  (if (eq (car result) :error)
      (llm-pi-ai-oc--fail st (cadr result))
    (condition-case err
        (let* ((model (llm-pi-ai-oc--state-model st))
               (hook (plist-get (llm-pi-ai-oc--state-options st) :on-response))
               (response (cadr result)))
          (unless (llm-pi-ai-oc--state-finished st)
            (when hook
              (funcall hook (llm-pi-ai-make-provider-response
                             :status (car response)
                             :headers (llm-pi-ai-headers-to-record
                                       (cadr response)))
                       model))
            (unless (llm-pi-ai-oc--state-finished st)
              (llm-pi-ai-oc--emit st "start"))))
      (error (llm-pi-ai-oc--fail st err)))))

(defun llm-pi-ai-oc--run (st)
  "Run the request of ST: the body of the source's `try' block."
  (condition-case err
      (let* ((model (llm-pi-ai-oc--state-model st))
             (options (llm-pi-ai-oc--state-options st))
             (compat (llm-pi-ai-oc--state-compat st))
             (api-key (llm-pi-ai-oc--get-client-api-key
                       (llm-pi-ai-model-provider model)
                       (plist-get options :api-key) (plist-get options :headers)))
             (cache-retention (llm-pi-ai-oc--resolve-cache-retention
                               (plist-get options :cache-retention)
                               (plist-get options :env)))
             (cache-session-id (if (equal cache-retention "none")
                                   nil (plist-get options :session-id)))
             (headers (llm-pi-ai-oc--request-headers
                       api-key model cache-session-id compat
                       (plist-get options :headers)))
             (params (llm-pi-ai-oc--build-params
                      model (llm-pi-ai-oc--state-context st) options compat
                      cache-retention))
             (hook (plist-get options :on-payload))
             (next-params (and hook (funcall hook params model)))
             (body (llm-pi-ai-oc--serialize (or next-params params)))
             (url (llm-pi-ai-oc--request-url (llm-pi-ai-model-base-url model)))
             (signal (llm-pi-ai-oc--state-signal st))
             (max-retries (plist-get options :max-retries))
             (max-retry-delay-ms (plist-get options :max-retry-delay-ms)))
        (setf (llm-pi-ai-oc--state-sse st)
              (llm-pi-ai-sse-parser
               (lambda (event data) (llm-pi-ai-oc--on-sse st event data))))
        (when (and signal (not (llm-pi-ai-abort-signal-aborted-p signal)))
          (let ((listener (lambda ()
                            (let ((handle (llm-pi-ai-oc--state-handle st)))
                              (when handle (llm-pi-ai-http-abort handle))))))
            (setf (llm-pi-ai-oc--state-listener st) listener)
            (llm-pi-ai-abort-signal-add-listener signal listener)))
        (apply #'llm-pi-ai-retry-provider-request
               (lambda (done) (llm-pi-ai-oc--attempt st url headers body done))
               (lambda (result) (llm-pi-ai-oc--on-response st result))
               :signal signal
               (append (and max-retries (list :max-retries max-retries))
                       (and max-retry-delay-ms
                            (list :max-retry-delay-ms max-retry-delay-ms)))))
    (error (llm-pi-ai-oc--fail st err))))

;;; Entry points

(defun llm-pi-ai-api-openai-completions-stream (model context options)
  "Stream a response of MODEL to CONTEXT, return the event stream at once.
The source's `stream'.  See the file commentary for MODEL, CONTEXT and
OPTIONS.  Every failure of the request ends the stream with an `error'
event; nothing is signalled for it."
  (let* ((stream (llm-pi-ai-create-assistant-message-event-stream))
         (compat (llm-pi-ai-api-openai-completions-get-compat model))
         (transcript (llm-pi-ai-oc--resolve-transcript
                      context (plist-get compat :supports-mid-convo-system-messages)))
         (output (llm-pi-ai-make-assistant-message
                  :content nil
                  :api (llm-pi-ai-model-api model)
                  :provider (llm-pi-ai-model-provider model)
                  :model (llm-pi-ai-model-id model)
                  :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
                  :stop-reason "pending"
                  :timestamp (llm-pi-ai-oc--now-ms)))
         (st (llm-pi-ai-oc--make-state
              :stream stream :model model :options options :compat compat
              :output output :signal (plist-get options :signal)
              :context transcript)))
    (llm-pi-ai-oc--run st)
    stream))

(defun llm-pi-ai-api-openai-completions-stream-simple (model context options)
  "Stream like `llm-pi-ai-api-openai-completions-stream', for simple options.
The source's `streamSimple'.  OPTIONS may carry `:reasoning', a level
that is clamped to what MODEL supports.  A missing API key signals an
error here, as it throws in the source."
  (llm-pi-ai-oc--get-client-api-key (llm-pi-ai-model-provider model)
                                    (plist-get options :api-key)
                                    (plist-get options :headers))
  (let* ((reasoning (plist-get options :reasoning))
         (clamped (and reasoning
                       (llm-pi-ai-clamp-thinking-level model reasoning)))
         (effort (if (equal clamped "off") nil clamped)))
    (llm-pi-ai-api-openai-completions-stream
     model context
     (list :temperature (plist-get options :temperature)
           :sampling-params (llm-pi-ai-oc--resolve-sampling-params
                             model (or reasoning "off")
                             (plist-get options :sampling-params))
           :max-tokens (llm-pi-ai-oc--clamp-max-tokens-to-context
                        model context
                        (or (plist-get options :max-tokens)
                            (llm-pi-ai-model-max-tokens model)))
           :signal (plist-get options :signal)
           :api-key (plist-get options :api-key)
           :cache-retention (plist-get options :cache-retention)
           :session-id (plist-get options :session-id)
           :headers (plist-get options :headers)
           :on-payload (plist-get options :on-payload)
           :on-response (plist-get options :on-response)
           :on-provider-stream-event (plist-get options :on-provider-stream-event)
           :timeout-ms (plist-get options :timeout-ms)
           :max-retries (plist-get options :max-retries)
           :max-retry-delay-ms (plist-get options :max-retry-delay-ms)
           :env (plist-get options :env)
           :tool-choice (plist-get options :tool-choice)
           :reasoning-effort effort
           :thinking-budgets (plist-get options :thinking-budgets)))))

(provide 'llm-pi-ai-api-openai-completions)

;;; llm-pi-ai-api-openai-completions.el ends here
