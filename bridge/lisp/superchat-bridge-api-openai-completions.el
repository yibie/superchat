;;; superchat-bridge-api-openai-completions.el --- OpenAI Chat Completions API, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/openai-completions.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;;   `superchat-bridge-api-openai-completions-stream'          `stream'
;;   `superchat-bridge-api-openai-completions-stream-simple'   `streamSimple'
;;   `superchat-bridge-api-openai-completions-convert-messages' `convertMessages'
;;   `superchat-bridge-api-openai-completions-detect-compat'   `detectCompat'
;;   `superchat-bridge-api-openai-completions-get-compat'      `getCompat'
;;
;; Everything else is private and uses the prefix `superchat-bridge-oc--'.
;;
;; The `openai' npm client is replaced by `superchat-bridge-http-request' (curl)
;; and `superchat-bridge-sse-parser'.  A request is a POST of the JSON that
;; `buildParams' makes to `<base-url>/chat/completions', its headers as
;; `createClient' makes them (Authorization bearer from the API key, then
;; the model's headers, session affinity headers, and the options'
;; headers, merged without regard to case, a nil value removing one: see
;; `superchat-bridge-provider-headers-to-record').  Each SSE `data:' line is one
;; `ChatCompletionChunk'; `[DONE]' ends the data (the stream then ends
;; when the connection does).  The request is wrapped in
;; `superchat-bridge-retry-provider-request' where the source wraps it in
;; `retryProviderRequest': it covers the time until the response headers
;; arrive and, for a status other than 2xx, the whole error body.  Once a
;; response has begun nothing is retried.
;;
;; Representation.
;;   MODEL     an `superchat-bridge-model'.  Its `compat' is an alist with
;;             kebab-case symbol keys (see `superchat-bridge-model-catalog'): an
;;             entry that is present counts, a value of nil being JSON
;;             false; JSON null (`:null') counts as absent, as `??' does.
;;   CONTEXT   an `superchat-bridge-context' already normalized (see
;;             `superchat-bridge-normalize-context'): its `messages' hold the
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
;;             `superchat-bridge-thinking-budgets'.  `:tool-choice' is a string
;;             or a JSON object in the form below.
;;   HOOKS     `:on-payload' is called with (PARAMS MODEL), PARAMS being
;;             the request body in the form below, and may return a
;;             replacement body or nil to keep it; `:on-response' with
;;             an `superchat-bridge-provider-response' and MODEL; and
;;             `:on-provider-stream-event' with each parsed chunk (a
;;             hash table) and MODEL.  Their results, other than that of
;;             `:on-payload', are ignored; an error they signal ends the
;;             stream with an `error' event.
;;   ABORT     `:signal' is an `superchat-bridge-abort-signal'.  A listener on it
;;             calls `superchat-bridge-http-abort' for the request in flight.
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
;; passed through `superchat-bridge-oc--json', which accepts a hash table, an
;; alist with symbol or string keys, a plist with keyword keys, vectors
;; for arrays, and also a non-empty list that is no object as an array.
;; Chunks are parsed with `json-parse-string' as hash tables with string
;; keys (`:object-type hash-table'), vectors for arrays, `:null' for
;; null and `:false' for false; `superchat-bridge-oc--jget' reads a member and
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
;;     SDK's is, kept by a timer (`superchat-bridge-retry-run-at-time-function')
;;     because the HTTP seam's timeout would cover the whole stream.
;;   - A connection failure reads "Connection error: <curl's message>",
;;     a timeout "Request timed out.", an abort "Request was aborted.".
;;   - From `src/utils/transcript.ts', `src/utils/estimate.ts' and
;;     `src/api/simple-options.ts' the parts this file needs are private
;;     copies here (`superchat-bridge-oc--resolve-transcript' and friends,
;;     `superchat-bridge-oc--estimate-context-tokens', and the sampling and
;;     thinking-budget helpers), until those files have owners.
;;   - `retryProviderRequest' is given the error plists described in
;;     `superchat-bridge-utils-provider-retry'; an HTTP error body becomes the
;;     `message' the `openai' SDK's `APIError' would have, so that
;;     `superchat-bridge-format-provider-error' gives the source's text.

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
(require 'superchat-bridge-utils-hash)
(require 'superchat-bridge-utils-headers)
(require 'superchat-bridge-utils-json-parse)
(require 'superchat-bridge-utils-provider-env)
(require 'superchat-bridge-utils-sanitize-unicode)
(require 'superchat-bridge-utils-text)
(require 'superchat-bridge-api-transform-messages)
(require 'superchat-bridge-api-openai-prompt-cache)

;;; Small helpers

(defun superchat-bridge-oc--now-ms ()
  "Return the current time in milliseconds, like `Date.now()'."
  (floor (* 1000 (float-time))))

(defun superchat-bridge-oc--obj (&rest keys-and-values)
  "Return a new alist from KEYS-AND-VALUES, symbol keys in order."
  (let (result)
    (while keys-and-values
      (push (cons (car keys-and-values) (cadr keys-and-values)) result)
      (setq keys-and-values (cddr keys-and-values)))
    (nreverse result)))

(defun superchat-bridge-oc--put (alist key value)
  "Set KEY to VALUE in ALIST and return it, as `obj[key] = value'.
An existing key keeps its place, a new one goes last.  ALIST is changed."
  (let ((cell (assq key alist)))
    (if cell
        (progn (setcdr cell value) alist)
      (nconc alist (list (cons key value))))))

(defun superchat-bridge-oc--truthy (value)
  "Return non-nil when VALUE is truthy in JavaScript, for a Lisp VALUE.
nil, `:null', `:false', the empty string and 0 are not."
  (not (or (null value) (memq value '(:null :false))
           (equal value "") (eql value 0) (eql value 0.0))))

(defun superchat-bridge-oc--jget (object key)
  "Return the member KEY of OBJECT, a parsed JSON hash table, or nil.
Null and false read as nil, as `??' and `||' read them in the source."
  (and (hash-table-p object)
       (let ((value (gethash key object)))
         (and (not (memq value '(:null :false))) value))))

(defun superchat-bridge-oc--includes (text part)
  "Return non-nil when the string TEXT contains PART."
  (and (string-search part text) t))

(defun superchat-bridge-oc--json-key (key)
  "Return KEY, a symbol, keyword or string, as the symbol for an object key."
  (cond ((keywordp key) (intern (substring (symbol-name key) 1)))
        ((symbolp key) key)
        (t (intern (format "%s" key)))))

(defun superchat-bridge-oc--alist-like-p (value)
  "Return non-nil when VALUE is a non-empty alist with symbol or string keys."
  (and (consp value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (and (symbolp (car entry)) (car entry)
                                 (not (memq (car entry) '(:null :false))))
                            (stringp (car entry)))))
                 value)))

(defun superchat-bridge-oc--plist-like-p (value)
  "Return non-nil when VALUE is a non-empty plist with keyword keys."
  (and (consp value) (keywordp (car value)) (cl-evenp (length value))
       (not (memq (car value) '(:null :false)))
       (cl-loop for (key) on value by #'cddr always (keywordp key))))

(defun superchat-bridge-oc--json (value)
  "Return VALUE, a caller's data, in the form `json-serialize' takes.
See the file commentary: hash tables, alists and plists become alists
with symbol keys, vectors stay arrays, a list that is no object becomes
a vector, and nil stays nil, which is `{}'."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (push (cons (superchat-bridge-oc--json-key key) (superchat-bridge-oc--json item))
                       result))
               value)
      (nreverse result)))
   ((vectorp value) (vconcat (mapcar #'superchat-bridge-oc--json value)))
   ((superchat-bridge-oc--alist-like-p value)
    (mapcar (lambda (entry)
              (cons (superchat-bridge-oc--json-key (car entry))
                    (superchat-bridge-oc--json (cdr entry))))
            value))
   ((superchat-bridge-oc--plist-like-p value)
    (cl-loop for (key item) on value by #'cddr
             collect (cons (superchat-bridge-oc--json-key key)
                           (superchat-bridge-oc--json item))))
   ((consp value) (vconcat (mapcar #'superchat-bridge-oc--json value)))
   (t value)))

(defun superchat-bridge-oc--serialize (value)
  "Return VALUE, Lisp data of the file commentary, as JSON text.
This is `JSON.stringify' for the data this file builds."
  (let ((text (json-serialize (superchat-bridge-oc--json value)
                              :null-object :null :false-object :false)))
    (if (multibyte-string-p text) text (decode-coding-string text 'utf-8))))

(defun superchat-bridge-oc--bool (value)
  "Return `t' when VALUE is truthy and `:false' when not: `!!value' as JSON."
  (if (superchat-bridge-oc--truthy value) t :false))

(defun superchat-bridge-oc--tlm (model level)
  "Return `model.thinkingLevelMap?.[LEVEL]': a string, `:null' or `:undefined'."
  (let ((entry (assoc level (superchat-bridge-model-thinking-level-map model))))
    (if entry (cdr entry) :undefined)))

(defun superchat-bridge-oc--nullish-p (value)
  "Return non-nil when VALUE is the source's undefined or null."
  (memq value '(:undefined :null nil)))

;;; Transcript helpers (src/utils/transcript.ts)

;; Private copies of the parts of `src/utils/transcript.ts' this file
;; uses; `superchat-bridge-utils-transcript.el' ports only two of its functions.

(defun superchat-bridge-oc--system-messages (messages)
  "Return the system messages among MESSAGES."
  (seq-filter #'superchat-bridge-system-message-p messages))

(defun superchat-bridge-oc--tool-name (tool)
  "Return the name of TOOL, a tool or a tool reference."
  (if (superchat-bridge-tool-p tool)
      (superchat-bridge-tool-name tool)
    (superchat-bridge-tool-reference-name tool)))

(defun superchat-bridge-oc--get-current-tools (messages)
  "Return the tools in force after every system message of MESSAGES.
The source's `getCurrentTools': removals, then additions, in order;
a re-added name keeps its place."
  (let (tools)
    (dolist (message (superchat-bridge-oc--system-messages messages))
      (dolist (removed (superchat-bridge-system-message-tools-removed message))
        (setq tools (assoc-delete-all (superchat-bridge-oc--tool-name removed) tools
                                      #'equal)))
      (dolist (tool (superchat-bridge-system-message-tools-added message))
        (let ((cell (assoc (superchat-bridge-tool-name tool) tools)))
          (if cell
              (setcdr cell tool)
            (setq tools (append tools
                                (list (cons (superchat-bridge-tool-name tool) tool))))))))
    (mapcar #'cdr tools)))

(defun superchat-bridge-oc--get-current-system-message (messages)
  "Return one system message holding the current prompt and tools, or nil.
The source's `getCurrentSystemMessage'."
  (let (content sections timestamp seen)
    (dolist (message (superchat-bridge-oc--system-messages messages))
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
              (setq sections (append sections
                                     (list (cons (car section)
                                                 (cdr section))))))))))
    (let ((tools (superchat-bridge-oc--get-current-tools messages)))
      (when (or seen tools)
        (superchat-bridge-make-system-message
         :content (string-join (nreverse content) "\n\n")
         :sections sections
         :tools-added tools
         :timestamp (or timestamp 0))))))

(defun superchat-bridge-oc--resolve-transcript (context supports-mid-convo-system-messages)
  "Return CONTEXT, or one with its system messages collapsed into one.
The source's `resolveTranscript' (with `collapseSystemMessages'): a
model that takes no later system messages gets the replayed one in
front and the others dropped."
  (if supports-mid-convo-system-messages
      context
    (let* ((messages (superchat-bridge-context-messages context))
           (head (superchat-bridge-oc--get-current-system-message messages))
           (rest (seq-remove #'superchat-bridge-system-message-p messages)))
      (superchat-bridge-make-context :messages (if head (cons head rest) rest)))))

(defun superchat-bridge-oc--last-non-additive-tool-change (system-messages)
  "Return the index in SYSTEM-MESSAGES of the last non-additive tool change.
The source's `findLastNonAdditiveToolChange'; -1 when there is none."
  (let ((declared nil) (last -1) (index 0))
    (dolist (message system-messages)
      (when (superchat-bridge-system-message-tools-removed message)
        (setq last index))
      (dolist (tool (superchat-bridge-system-message-tools-added message))
        (when (member (superchat-bridge-tool-name tool) declared)
          (setq last index))
        (push (superchat-bridge-tool-name tool) declared))
      (setq index (1+ index)))
    last))

(defun superchat-bridge-oc--resolve-transcript-tools (messages support)
  "Split the tool declarations of MESSAGES for a transport.
The source's `resolveTranscriptTools'.  SUPPORT is `none', `additions' or
`changes'.  Return (REQUEST-TOOLS . FOLDED-SYSTEM-MESSAGES)."
  (let ((system-messages (superchat-bridge-oc--system-messages messages)))
    (pcase support
      ('none (cons (superchat-bridge-oc--get-current-tools system-messages)
                   (length system-messages)))
      ('changes (cons nil 0))
      (_ (let ((folded (max (if (and messages
                                     (superchat-bridge-system-message-p (car messages)))
                                1 0)
                            (1+ (superchat-bridge-oc--last-non-additive-tool-change
                                 system-messages)))))
           (cons (superchat-bridge-oc--get-current-tools
                  (seq-take system-messages folded))
                 folded))))))

;;; Estimating context tokens (src/utils/estimate.ts)

(defun superchat-bridge-oc--estimate-text-tokens (text)
  "Return the estimated tokens of TEXT: `estimateTextTokens'."
  (ceiling (/ (float (superchat-bridge-utf16-length text)) 3.5)))

(defun superchat-bridge-oc--content-chars (content)
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

(defun superchat-bridge-oc--tools-json (tools)
  "Return TOOLS, a list of tools, as the JSON text `JSON.stringify' gives."
  (superchat-bridge-oc--serialize
   (vconcat (mapcar (lambda (tool)
                      (superchat-bridge-oc--obj
                       'name (superchat-bridge-tool-name tool)
                       'description (superchat-bridge-tool-description tool)
                       'parameters (superchat-bridge-tool-parameters tool)))
                    tools))))

(defun superchat-bridge-oc--estimate-tools-tokens (tools)
  "Return the estimated tokens of TOOLS: `estimateToolsTokens'."
  (if (null tools)
      0
    (superchat-bridge-oc--estimate-text-tokens (superchat-bridge-oc--tools-json tools))))

(defun superchat-bridge-oc--estimate-message-tokens (message)
  "Return the estimated tokens of MESSAGE: `estimateMessageTokens'."
  (cond
   ((superchat-bridge-system-message-p message)
    (+ (superchat-bridge-oc--estimate-text-tokens
        (superchat-bridge-system-message-text message))
       (superchat-bridge-oc--estimate-tools-tokens
        (superchat-bridge-system-message-tools-added message))
       (superchat-bridge-oc--estimate-tools-tokens
        (mapcar (lambda (ref) (superchat-bridge-make-tool :name (superchat-bridge-oc--tool-name ref)))
                (superchat-bridge-system-message-tools-removed message)))))
   ((or (superchat-bridge-user-message-p message)
        (superchat-bridge-tool-result-message-p message))
    (ceiling (/ (float (superchat-bridge-oc--content-chars
                        (superchat-bridge-message-content message)))
                3.5)))
   (t
    (let ((chars 0))
      (dolist (block (superchat-bridge-message-content message))
        (setq chars
              (+ chars
                 (cond ((superchat-bridge-text-content-p block)
                        (superchat-bridge-utf16-length (or (superchat-bridge-text-content-text block) "")))
                       ((superchat-bridge-thinking-content-p block)
                        (superchat-bridge-utf16-length
                         (or (superchat-bridge-thinking-content-thinking block) "")))
                       (t (+ (superchat-bridge-utf16-length
                              (or (superchat-bridge-tool-call-name block) ""))
                             (superchat-bridge-utf16-length
                              (superchat-bridge-safe-json-stringify
                               (superchat-bridge-tool-call-arguments block)))))))))
      (ceiling (/ (float chars) 3.5))))))

(defun superchat-bridge-oc--context-tokens (usage)
  "Return the tokens USAGE reports: `calculateContextTokens'."
  (let ((total (superchat-bridge-usage-total-tokens usage)))
    (if (and total (/= total 0))
        total
      (+ (superchat-bridge-usage-input usage) (superchat-bridge-usage-output usage)
         (superchat-bridge-usage-cache-read usage) (superchat-bridge-usage-cache-write usage)))))

(defun superchat-bridge-oc--estimate-context-tokens (context)
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
                     (> (superchat-bridge-oc--context-tokens usage) 0))
            (setq info (cons usage index)))))
      (setq latest-prefix-timestamp
            (max (or latest-prefix-timestamp (- (expt 2 60)))
                 (or (superchat-bridge-message-timestamp message) 0)))
      (setq index (1+ index)))
    (if info
        (+ (superchat-bridge-oc--context-tokens (car info))
           (apply #'+ (mapcar #'superchat-bridge-oc--estimate-message-tokens
                              (nthcdr (1+ (cdr info)) messages))))
      (apply #'+ (mapcar #'superchat-bridge-oc--estimate-message-tokens messages)))))

;;; Simple options (src/api/simple-options.ts)

(defconst superchat-bridge-oc--min-answer-tokens 1024
  "`MIN_ANSWER_TOKENS': tokens always left for the answer.")

(defun superchat-bridge-oc--clamp-max-tokens-to-context (model context max-tokens)
  "Return MAX-TOKENS held to what MODEL's window has left after CONTEXT.
The source's `clampMaxTokensToContext'."
  (let ((window (or (superchat-bridge-model-context-window model) 0)))
    (if (<= window 0)
        (max 1 max-tokens)
      (min max-tokens
           (max 1 (- window (superchat-bridge-oc--estimate-context-tokens context)
                     4096))))))

(defun superchat-bridge-oc--key-name (key)
  "Return KEY, a string or symbol, as a name string."
  (cond ((stringp key) key)
        ((keywordp key) (substring (symbol-name key) 1))
        (t (symbol-name key))))

(defun superchat-bridge-oc--merge-params (&rest param-lists)
  "Merge PARAM-LISTS, alists of (NAME . VALUE), later over earlier.
`{...a, ...b}': a name keeps the place of its first appearance.  Names
come out as strings."
  (let (result)
    (dolist (params param-lists)
      (dolist (entry params)
        (let* ((name (superchat-bridge-oc--key-name (car entry)))
               (cell (assoc name result)))
          (if cell
              (setcdr cell (cdr entry))
            (setq result (append result (list (cons name (cdr entry)))))))))
    result))

(defun superchat-bridge-oc--resolve-sampling-params (model thinking-level request-params)
  "Return the sampling parameters of MODEL at THINKING-LEVEL, or nil.
REQUEST-PARAMS are the request's own, which win per name.  The source's
`resolveSamplingParams'."
  (let* ((effective (superchat-bridge-clamp-thinking-level model thinking-level))
         (by-level (cdr (assoc effective
                               (superchat-bridge-model-sampling-params-by-thinking-level
                                model))))
         (model-params (superchat-bridge-model-sampling-params model)))
    (when (or model-params by-level request-params)
      (superchat-bridge-oc--merge-params model-params by-level request-params))))

(defconst superchat-bridge-oc--default-thinking-budgets
  '(("minimal" . 1024) ("low" . 2048) ("medium" . 8192) ("high" . 16384))
  "`DEFAULT_THINKING_BUDGETS'.")

(defun superchat-bridge-oc--thinking-budget-for-level (level custom-budgets)
  "Return the thinking budget of LEVEL, CUSTOM-BUDGETS over the defaults.
The source's `thinkingBudgetForLevel'; CUSTOM-BUDGETS is an
`superchat-bridge-thinking-budgets' or nil.  \"xhigh\" and \"max\" read as
\"high\", as `clampReasoning' does."
  (let* ((level (if (member level '("xhigh" "max")) "high" level))
         (custom (and custom-budgets
                      (pcase level
                        ("minimal" (superchat-bridge-thinking-budgets-minimal custom-budgets))
                        ("low" (superchat-bridge-thinking-budgets-low custom-budgets))
                        ("medium" (superchat-bridge-thinking-budgets-medium custom-budgets))
                        ("high" (superchat-bridge-thinking-budgets-high custom-budgets))))))
    (or custom (cdr (assoc level superchat-bridge-oc--default-thinking-budgets)))))

(defun superchat-bridge-oc--clamp-thinking-budget-to-answer-room (budget ceiling)
  "Return BUDGET held so that MIN_ANSWER_TOKENS remain under CEILING."
  (min budget (max 0 (- ceiling superchat-bridge-oc--min-answer-tokens))))

;;; Compat (detectCompat, getCompat)

(defun superchat-bridge-api-openai-completions-detect-compat (model)
  "Return the compatibility settings detected for MODEL, a plist.
The source's `detectCompat': from the provider name and the base URL;
it is the base that `superchat-bridge-api-openai-completions-get-compat'
puts the model's own `compat' over.  Keys are the source's field names
in kebab case with a colon; false and undefined are nil."
  (let* ((provider (superchat-bridge-model-provider model))
         (base-url (or (superchat-bridge-model-base-url model) ""))
         (id (or (superchat-bridge-model-id model) ""))
         (url (lambda (part) (superchat-bridge-oc--includes base-url part)))
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
                          (superchat-bridge-oc--includes (downcase base-url) "deepseek.com")))
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

(defconst superchat-bridge-oc--compat-keys
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

(defun superchat-bridge-api-openai-completions-get-compat (model)
  "Return the resolved compatibility settings of MODEL, a plist.
The source's `getCompat': `detect-compat' with the entries of the model's
`compat' over it.  An entry that is present wins, its nil being false;
a `:null' entry is absent.  `open-router-routing' and `vllm-priority'
are the model's own, not detected."
  (let* ((detected (superchat-bridge-api-openai-completions-detect-compat model))
         (model-compat (superchat-bridge-model-compat model)))
    (if (null model-compat)
        detected
      (let ((result (copy-sequence detected)))
        (dolist (mapping superchat-bridge-oc--compat-keys)
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

(defun superchat-bridge-oc--has-header-p (headers name)
  "Return non-nil when HEADERS, an alist, has a non-blank value for NAME.
The source's `hasHeader'; names compare without regard to case."
  (let ((expected (downcase name)))
    (cl-some (lambda (entry)
               (and (string= (downcase (car entry)) expected)
                    (stringp (cdr entry))
                    (> (length (superchat-bridge-string-trim-js (cdr entry))) 0)))
             headers)))

(defun superchat-bridge-oc--get-client-api-key (provider api-key headers)
  "Return the API key for PROVIDER: API-KEY, \"unused\" or an error.
The source's `getClientApiKey': a request that carries its own
authorization header needs none."
  (cond ((superchat-bridge-oc--truthy api-key) api-key)
        ((or (superchat-bridge-oc--has-header-p headers "authorization")
             (superchat-bridge-oc--has-header-p headers "cf-aig-authorization"))
         "unused")
        (t (error "No API key for provider: %s" provider))))

(defun superchat-bridge-oc--resolve-cache-retention (cache-retention env)
  "Return the cache retention: CACHE-RETENTION, else from ENV, else \"short\".
The source's `resolveCacheRetention'."
  (cond (cache-retention cache-retention)
        ((equal (superchat-bridge-get-provider-env-value "PI_CACHE_RETENTION" env)
                "long")
         "long")
        (t "short")))

(defun superchat-bridge-oc--request-headers (api-key model session-id compat
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
    (superchat-bridge-provider-headers-to-record
     (list (cons "Accept" "application/json")
           (cons "Content-Type" "application/json")
           (cons "Authorization" (concat "Bearer " api-key)))
     (superchat-bridge-model-headers model)
     affinity
     options-headers)))

(defun superchat-bridge-oc--request-url (base-url)
  "Return the URL of the chat completions endpoint under BASE-URL."
  (if (string-suffix-p "/" base-url)
      (concat (substring base-url 0 -1) "/chat/completions")
    (concat base-url "/chat/completions")))

;;; Reasoning details

(defun superchat-bridge-oc--detail-p (detail)
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

(defun superchat-bridge-oc--parse-json-or-nil (text)
  "Parse TEXT as JSON (`JSON.parse') or return `:invalid' when it is not."
  (condition-case nil
      (json-parse-string text :object-type 'hash-table :array-type 'array
                         :null-object :null :false-object :false)
    (json-error :invalid)
    (wrong-type-argument :invalid)))

(defun superchat-bridge-oc--parse-reasoning-details (signature)
  "Return the list of details SIGNATURE holds, or nil.
The source's `parseOpenAIReasoningDetails': SIGNATURE, a string or nil,
is JSON text of a non-empty array of valid details."
  (when (and (stringp signature) (> (length signature) 0))
    (let ((parsed (superchat-bridge-oc--parse-json-or-nil signature)))
      (when (and (vectorp parsed) (> (length parsed) 0)
                 (cl-every #'superchat-bridge-oc--detail-p parsed))
        (append parsed nil)))))

(defun superchat-bridge-oc--parse-legacy-encrypted-detail (signature)
  "Return the encrypted detail SIGNATURE holds, or nil.
The source's `parseLegacyEncryptedReasoningDetail'."
  (when (and (stringp signature) (> (length signature) 0))
    (let ((parsed (superchat-bridge-oc--parse-json-or-nil signature)))
      (when (and (superchat-bridge-oc--detail-p parsed)
                 (equal (gethash "type" parsed) "reasoning.encrypted")
                 (stringp (gethash "id" parsed))
                 (> (length (gethash "id" parsed)) 0)
                 (> (length (gethash "data" parsed)) 0))
        parsed))))

(defun superchat-bridge-oc--set-or-remove (table key value)
  "Set KEY of TABLE to VALUE, or remove it when VALUE is `:undefined'."
  (if (eq value :undefined)
      (remhash key table)
    (puthash key value table)))

(defun superchat-bridge-oc--fill-missing-common-fields (target source)
  "Fill in the id, format and index of TARGET from SOURCE.
The source's `fillMissingCommonReasoningDetailFields': `??=' for id
and index, `||=' for format."
  (when (memq (gethash "id" target :undefined) '(:undefined :null))
    (superchat-bridge-oc--set-or-remove target "id" (gethash "id" source :undefined)))
  (unless (superchat-bridge-oc--truthy (gethash "format" target))
    (superchat-bridge-oc--set-or-remove target "format"
                                 (gethash "format" source :undefined)))
  (when (memq (gethash "index" target :undefined) '(:undefined :null))
    (superchat-bridge-oc--set-or-remove target "index"
                                 (gethash "index" source :undefined))))

(defun superchat-bridge-oc--append-detail (details detail)
  "Return DETAILS, a list of detail tables, with DETAIL added.
The source's `appendOpenAIReasoningDetail': consecutive text, or
consecutive summary, details merge into the last one."
  (let* ((last (car (last details)))
         (type (gethash "type" detail))
         (last-type (and last (gethash "type" last))))
    (cond
     ((and (equal type "reasoning.text") (equal last-type "reasoning.text"))
      (puthash "text" (concat (gethash "text" last) (gethash "text" detail)) last)
      (unless (superchat-bridge-oc--truthy (gethash "signature" last))
        (superchat-bridge-oc--set-or-remove last "signature"
                                     (gethash "signature" detail :undefined)))
      (superchat-bridge-oc--fill-missing-common-fields last detail)
      details)
     ((and (equal type "reasoning.summary") (equal last-type "reasoning.summary"))
      (puthash "summary" (concat (gethash "summary" last)
                                 (gethash "summary" detail))
               last)
      (superchat-bridge-oc--fill-missing-common-fields last detail)
      details)
     (t (append details (list (copy-hash-table detail)))))))

;;; Cache control

(defun superchat-bridge-oc--get-compat-cache-control (compat cache-retention)
  "Return the `cache_control' object for COMPAT and CACHE-RETENTION, or nil.
The source's `getCompatCacheControl'."
  (unless (or (not (equal (plist-get compat :cache-control-format) "anthropic"))
              (equal cache-retention "none"))
    (if (and (equal cache-retention "long")
             (plist-get compat :supports-long-cache-retention))
        (superchat-bridge-oc--obj 'type "ephemeral" 'ttl "1h")
      (superchat-bridge-oc--obj 'type "ephemeral"))))

(defun superchat-bridge-oc--add-cache-control-to-text-content (message cache-control)
  "Mark the last text of MESSAGE, an alist, with CACHE-CONTROL.
Return non-nil when something was marked: `addCacheControlToTextContent'.
A string content becomes one marked text part."
  (let ((content (alist-get 'content message)))
    (cond
     ((stringp content)
      (unless (string-empty-p content)
        (setcdr (assq 'content message)
                (vector (superchat-bridge-oc--obj 'type "text" 'text content
                                           'cache_control cache-control)))
        t))
     ((vectorp content)
      (let ((index (1- (length content))) (done nil))
        (while (and (not done) (>= index 0))
          (let ((part (aref content index)))
            (when (equal (alist-get 'type part) "text")
              (superchat-bridge-oc--put part 'cache_control cache-control)
              (setq done t)))
          (setq index (1- index)))
        done)))))

(defun superchat-bridge-oc--apply-anthropic-cache-control (messages tools cache-control)
  "Mark MESSAGES, a list of alists, and TOOLS, a vector or nil, for caching.
The source's `applyAnthropicCacheControl': the first instruction
message, the last tool and the last conversation message that has text."
  ;; addCacheControlToSystemPrompt: the first one is tried and that ends it.
  (let ((first (seq-find (lambda (message)
                           (member (alist-get 'role message) '("system" "developer")))
                         messages)))
    (when first
      (superchat-bridge-oc--add-cache-control-to-text-content first cache-control)))
  ;; addCacheControlToLastTool
  (when (and tools (> (length tools) 0))
    (superchat-bridge-oc--put (aref tools (1- (length tools))) 'cache_control
                       cache-control))
  ;; addCacheControlToLastConversationMessage
  (let ((done nil))
    (dolist (message (reverse messages))
      (when (and (not done)
                 (member (alist-get 'role message) '("user" "assistant" "tool"))
                 (superchat-bridge-oc--add-cache-control-to-text-content
                  message cache-control))
        (setq done t)))))

;;; Tools

(defun superchat-bridge-oc--convert-tools (tools compat)
  "Return TOOLS as a list of Chat Completions tool alists.
The source's `convertTools'.  A tool with constrained sampling is not
supported (see the file commentary) and signals an error."
  (mapcar
   (lambda (tool)
     (when (superchat-bridge-tool-constrained-sampling tool)
       (error "Tool \"%s\" uses constrained sampling, which is not ported"
              (superchat-bridge-tool-name tool)))
     (let ((function (superchat-bridge-oc--obj 'name (superchat-bridge-tool-name tool))))
       (when (superchat-bridge-tool-description tool)
         (superchat-bridge-oc--put function 'description
                            (superchat-bridge-tool-description tool)))
       (superchat-bridge-oc--put function 'parameters (superchat-bridge-tool-parameters tool))
       ;; Only include strict if provider supports it.  Some reject unknown fields.
       (when (plist-get compat :supports-strict-mode)
         (superchat-bridge-oc--put function 'strict :false))
       (superchat-bridge-oc--obj 'type "function" 'function function)))
   tools))

;;; Messages

(defun superchat-bridge-oc--normalize-tool-call-id (model id)
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
        (let* ((hash (substring (superchat-bridge-short-hash id) 0
                                (min 8 (length (superchat-bridge-short-hash id)))))
               (prefix (substring call-id 0
                                  (min (length call-id)
                                       (max 1 (- 40 (length hash) 1))))))
          (concat prefix "_" hash)))))
   ((equal (superchat-bridge-model-provider model) "openai")
    (if (> (length id) 40) (substring id 0 40) id))
   (t id)))

(defun superchat-bridge-oc--image-url-part (block)
  "Return the `image_url' content part for BLOCK, an image."
  (superchat-bridge-oc--obj
   'type "image_url"
   'image_url (superchat-bridge-oc--obj
               'url (format "data:%s;base64,%s"
                            (superchat-bridge-image-content-mime-type block)
                            (superchat-bridge-image-content-data block)))))

(defun superchat-bridge-oc--text-part (text)
  "Return the `text' content part of TEXT, surrogates sanitized."
  (superchat-bridge-oc--obj 'type "text" 'text (superchat-bridge-sanitize-surrogates text)))

(defun superchat-bridge-oc--blank-p (text)
  "Return non-nil when TEXT, a string or nil, is empty after `trim'."
  (or (null text) (string-empty-p (superchat-bridge-string-trim-js text))))

(defun superchat-bridge-oc--convert-assistant (model compat msg)
  "Return the Chat Completions message for MSG, an assistant message, or nil.
The assistant branch of `convertMessages'; nil is the source's `continue'."
  (let* ((blocks (superchat-bridge-message-content msg))
         (message (superchat-bridge-oc--obj
                   'role "assistant"
                   ;; Some providers don't accept null content, use "" instead
                   'content (if (plist-get compat :requires-assistant-after-tool-result)
                                "" :null)))
         (text-parts
          (mapcar (lambda (block) (superchat-bridge-oc--text-part
                                   (superchat-bridge-text-content-text block)))
                  (seq-remove (lambda (block)
                                (superchat-bridge-oc--blank-p
                                 (superchat-bridge-text-content-text block)))
                              (seq-filter #'superchat-bridge-text-content-p blocks))))
         (assistant-text (mapconcat (lambda (part) (alist-get 'text part))
                                    text-parts ""))
         (thinking-blocks (seq-filter #'superchat-bridge-thinking-content-p blocks))
         (tool-calls (seq-filter #'superchat-bridge-tool-call-p blocks))
         (signed-details
          (cl-some (lambda (block)
                     (superchat-bridge-oc--parse-reasoning-details
                      (superchat-bridge-thinking-content-thinking-signature block)))
                   thinking-blocks))
         (legacy-details
          (delq nil (mapcar (lambda (tool-call)
                              (superchat-bridge-oc--parse-legacy-encrypted-detail
                               (superchat-bridge-tool-call-thought-signature tool-call)))
                            tool-calls)))
         (preserved (or signed-details legacy-details))
         (non-empty-thinking
          (seq-remove (lambda (block)
                        (superchat-bridge-oc--blank-p
                         (superchat-bridge-thinking-content-thinking block)))
                      thinking-blocks)))
    (cond
     (non-empty-thinking
      (if (plist-get compat :requires-thinking-as-text)
          ;; Thinking blocks become plain text (no tags, so the model does not
          ;; mimic them).
          (let ((thinking-text
                 (mapconcat (lambda (block)
                              (superchat-bridge-sanitize-surrogates
                               (superchat-bridge-thinking-content-thinking block)))
                            non-empty-thinking "\n\n")))
            (superchat-bridge-oc--put message 'content
                               (vconcat (list (superchat-bridge-oc--obj
                                               'type "text" 'text thinking-text))
                                        text-parts)))
        ;; Assistant content goes as a plain string, the standard format.
        (when (> (length assistant-text) 0)
          (superchat-bridge-oc--put message 'content assistant-text))
        ;; reasoning_details is the structured alternative to a raw field.
        (unless preserved
          (let ((signature (superchat-bridge-thinking-content-thinking-signature
                            (car non-empty-thinking))))
            (when (and (equal (superchat-bridge-model-provider model) "opencode-go")
                       (equal signature "reasoning"))
              (setq signature "reasoning_content"))
            (when (member signature '("reasoning" "reasoning_content"
                                      "reasoning_text"))
              (superchat-bridge-oc--put
               message (intern signature)
               (mapconcat #'superchat-bridge-thinking-content-thinking
                          non-empty-thinking "\n")))))))
     ((> (length assistant-text) 0)
      (superchat-bridge-oc--put message 'content assistant-text)))
    (when tool-calls
      (superchat-bridge-oc--put
       message 'tool_calls
       (vconcat
        (mapcar
         (lambda (tool-call)
           (superchat-bridge-oc--obj
            'id (superchat-bridge-tool-call-id tool-call)
            'type "function"
            'function (superchat-bridge-oc--obj
                       'name (superchat-bridge-tool-call-name tool-call)
                       'arguments (superchat-bridge-oc--serialize
                                   (or (superchat-bridge-tool-call-arguments tool-call)
                                       (make-hash-table :test #'equal))))))
         tool-calls))))
    (when preserved
      (superchat-bridge-oc--put message 'reasoning_details (vconcat preserved)))
    (when (and (plist-get compat :requires-reasoning-content-on-assistant-messages)
               (superchat-bridge-model-reasoning model)
               (not (assq 'reasoning_content message)))
      (superchat-bridge-oc--put message 'reasoning_content ""))
    ;; Skip assistant messages that have no content and no tool calls: some
    ;; providers require "either content or tool_calls, but not none".
    (let* ((content (alist-get 'content message))
           (has-content (and (not (eq content :null))
                             (> (length content) 0))))
      (when (or has-content (assq 'tool_calls message))
        message))))

(defun superchat-bridge-api-openai-completions-convert-messages (model context compat)
  "Return the Chat Completions messages for CONTEXT as MODEL takes them.
The source's `convertMessages' (without its grammar tool options).
COMPAT is the plist `superchat-bridge-api-openai-completions-get-compat' gives.
The result is a list of alists: see the file commentary for JSON."
  (let* ((normalized (superchat-bridge-oc--resolve-transcript
                      context (plist-get compat :supports-mid-convo-system-messages)))
         (transformed
          (vconcat
           (superchat-bridge-transform-messages
            (superchat-bridge-context-messages normalized) model
            (lambda (id _model _source)
              (superchat-bridge-oc--normalize-tool-call-id model id)))))
         (transcript-tools
          (superchat-bridge-oc--resolve-transcript-tools
           (superchat-bridge-context-messages normalized)
           (if (and (plist-get compat :supports-mid-convo-system-messages)
                    (plist-get compat :supports-mid-convo-tool-additions))
               'additions 'none)))
         (instruction-role (if (and (superchat-bridge-model-reasoning model)
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
                   (superchat-bridge-user-message-p msg))
          (push (superchat-bridge-oc--obj 'role "assistant"
                                   'content "I have processed the tool results.")
                params))
        (cond
         ((superchat-bridge-system-message-p msg)
          ;; transformMessages keeps system messages in order, so their
          ;; position matches the transcript.
          (let ((added-tools (if (>= system-index (cdr transcript-tools))
                                 (superchat-bridge-system-message-tools-added msg)
                               nil)))
            (setq system-index (1+ system-index))
            (when added-tools
              (push (superchat-bridge-oc--obj
                     'role "system"
                     'tools (vconcat (superchat-bridge-oc--convert-tools added-tools compat)))
                    params))
            (let ((text (if (= i 0)
                            (superchat-bridge-system-message-text msg)
                          (superchat-bridge-render-system-message-update msg))))
              (when (> (length text) 0)
                (push (superchat-bridge-oc--obj 'role instruction-role
                                         'content (superchat-bridge-sanitize-surrogates text))
                      params)))))
         ((superchat-bridge-user-message-p msg)
          (let ((content (superchat-bridge-message-content msg)))
            (if (stringp content)
                (push (superchat-bridge-oc--obj 'role "user"
                                         'content (superchat-bridge-sanitize-surrogates content))
                      params)
              (let ((parts
                     (mapcar
                      (lambda (item)
                        (if (superchat-bridge-text-content-p item)
                            (superchat-bridge-oc--text-part (superchat-bridge-text-content-text item))
                          (superchat-bridge-oc--image-url-part item)))
                      (seq-remove (lambda (item)
                                    (and (superchat-bridge-text-content-p item)
                                         (string-empty-p
                                          (or (superchat-bridge-text-content-text item) ""))))
                                  content))))
                (if (null parts)
                    (setq skip-last-role t)
                  (push (superchat-bridge-oc--obj 'role "user" 'content (vconcat parts))
                        params))))))
         ((superchat-bridge-assistant-message-p msg)
          (let ((converted (superchat-bridge-oc--convert-assistant model compat msg)))
            (if converted
                (push converted params)
              (setq skip-last-role t))))
         ((superchat-bridge-tool-result-message-p msg)
          (let ((image-blocks nil)
                (j i))
            (while (and (< j count)
                        (superchat-bridge-tool-result-message-p (aref transformed j)))
              (let* ((tool-msg (aref transformed j))
                     (blocks (superchat-bridge-message-content tool-msg))
                     (text-result (mapconcat
                                   (lambda (block)
                                     (or (superchat-bridge-text-content-text block) ""))
                                   (seq-filter #'superchat-bridge-text-content-p blocks)
                                   "\n"))
                     (has-images (seq-some #'superchat-bridge-image-content-p blocks))
                     (has-text (not (superchat-bridge-oc--blank-p text-result)))
                     (result-text (cond (has-text text-result)
                                        (has-images "(see attached image)")
                                        (t "(no tool output)")))
                     (result (superchat-bridge-oc--obj
                              'role "tool"
                              'content (superchat-bridge-sanitize-surrogates result-text)
                              'tool_call_id (superchat-bridge-tool-result-message-tool-call-id
                                             tool-msg))))
                ;; Some providers require the 'name' field in tool results.
                (when (and (plist-get compat :requires-tool-result-name)
                           (superchat-bridge-oc--truthy
                                (superchat-bridge-tool-result-message-tool-name tool-msg)))
                  (superchat-bridge-oc--put result 'name
                                     (superchat-bridge-tool-result-message-tool-name
                                      tool-msg)))
                (push result params)
                (when (and has-images (member "image" (superchat-bridge-model-input model)))
                  (dolist (block blocks)
                    (when (superchat-bridge-image-content-p block)
                      (push (superchat-bridge-oc--image-url-part block) image-blocks)))))
              (setq j (1+ j)))
            (setq i (1- j))
            (setq image-blocks (nreverse image-blocks))
            (if image-blocks
                (progn
                  (when (plist-get compat :requires-assistant-after-tool-result)
                    (push (superchat-bridge-oc--obj
                           'role "assistant"
                           'content "I have processed the tool results.")
                          params))
                  (push (superchat-bridge-oc--obj
                         'role "user"
                         'content (vconcat
                                   (list (superchat-bridge-oc--obj
                                          'type "text"
                                          'text "Attached image(s) from tool result:"))
                                   image-blocks))
                        params)
                  (setq last-role "user"))
              (setq last-role "toolResult"))
            (setq skip-last-role t))))
        (unless skip-last-role
          (setq last-role (superchat-bridge-message-role msg))))
      (setq i (1+ i)))
    (nreverse params)))

;;; Request body

(defun superchat-bridge-oc--has-tool-history-p (messages)
  "Return non-nil when MESSAGES has a tool call or a tool result."
  (seq-some (lambda (message)
              (or (superchat-bridge-tool-result-message-p message)
                  (and (superchat-bridge-assistant-message-p message)
                       (seq-some #'superchat-bridge-tool-call-p
                                 (superchat-bridge-message-content message)))))
            messages))

(defun superchat-bridge-oc--resolve-thinking-token-budget-field (compat)
  "Return the name of the budget field of COMPAT, a string, or nil."
  (cond ((plist-get compat :thinking-token-budget-field)
         (plist-get compat :thinking-token-budget-field))
        ((plist-get compat :supports-thinking-token-budget)
         "thinking_token_budget")))

(defun superchat-bridge-oc--resolve-clamped-thinking-budget (model options params)
  "Return the thinking budget for OPTIONS under PARAMS' token ceiling, or nil.
The source's `resolveClampedThinkingBudget'."
  (let ((effort (plist-get options :reasoning-effort)))
    (when (and effort (superchat-bridge-model-reasoning model))
      (let* ((ceiling (or (alist-get 'max_tokens params)
                          (alist-get 'max_completion_tokens params)
                          (superchat-bridge-model-max-tokens model)))
             (budget (superchat-bridge-oc--clamp-thinking-budget-to-answer-room
                      (superchat-bridge-oc--thinking-budget-for-level
                       effort (plist-get options :thinking-budgets))
                      ceiling)))
        (and (> budget 0) budget)))))

(defun superchat-bridge-oc--resolve-chat-template-value (model options value budget)
  "Return the request value of the chat template VALUE, or `:undefined'.
The source's `resolveChatTemplateKwargValue'.  VALUE is a compat entry:
a string, number, t, nil (false), `:null' or an alist with \"$var\"."
  (let ((effort (plist-get options :reasoning-effort)))
    (cond
     ((not (superchat-bridge-oc--alist-like-p value))
      (cond ((null value) :false) (t value)))
     ((and (not effort)
           (superchat-bridge-oc--truthy (cdr (assoc "omitWhenOff" value))))
      :undefined)
     ((equal (cdr (assoc "$var" value)) "thinking.enabled")
      (superchat-bridge-oc--bool effort))
     ((equal (cdr (assoc "$var" value)) "thinking.budget")
      (or budget :undefined))
     (t
      (let ((mapped (if effort
                        (superchat-bridge-oc--tlm model effort)
                      (superchat-bridge-oc--tlm model "off"))))
        (cond ((eq mapped :undefined) (or effort :undefined))
              ((stringp mapped) mapped)
              (t :undefined)))))))

(defun superchat-bridge-oc--build-chat-template-values (model options values budget)
  "Return the chat template values as an alist, or nil when none resolve.
The source's `buildChatTemplateValues'.  VALUES is an alist of compat
entries with string keys."
  (let (resolved)
    (dolist (entry values)
      (let ((value (superchat-bridge-oc--resolve-chat-template-value
                    model options (cdr entry) budget)))
        (unless (eq value :undefined)
          (push (cons (intern (car entry)) value) resolved))))
    (nreverse resolved)))

(defun superchat-bridge-oc--routing-json (routing)
  "Return ROUTING, a compat alist with string keys, as request JSON.
Lists are arrays, nil inside an object is false."
  (cond ((superchat-bridge-oc--alist-like-p routing)
         (mapcar (lambda (entry)
                   (cons (intern (car entry))
                         (let ((value (cdr entry)))
                           (cond ((null value) :false)
                                 ((and (consp value) (not (superchat-bridge-oc--alist-like-p value)))
                                  (vconcat value))
                                 (t (superchat-bridge-oc--routing-json value))))))
                 routing))
        (t routing)))

(defun superchat-bridge-oc--build-params (model context options compat cache-retention)
  "Return the request body for MODEL and CONTEXT, an alist.
The source's `buildParams'.  See the file commentary for the form of
OPTIONS, COMPAT and the result."
  (let* ((messages-in (superchat-bridge-context-messages context))
         (transcript-tools
          (superchat-bridge-oc--resolve-transcript-tools
           messages-in
           (if (and (eq (plist-get compat :supports-mid-convo-system-messages) t)
                    (eq (plist-get compat :supports-mid-convo-tool-additions) t))
               'additions 'none)))
         (messages (superchat-bridge-api-openai-completions-convert-messages
                    model context compat))
         (cache-control (superchat-bridge-oc--get-compat-cache-control
                         compat cache-retention))
         (effort (plist-get options :reasoning-effort))
         (long-cache (and (equal cache-retention "long")
                          (plist-get compat :supports-long-cache-retention)))
         (prompt-cache-key
          (and (or (and (superchat-bridge-oc--includes (superchat-bridge-model-base-url model)
                                                "api.openai.com")
                        (not (equal cache-retention "none")))
                   long-cache)
               (superchat-bridge-clamp-openai-prompt-cache-key
                (plist-get options :session-id))))
         (params (superchat-bridge-oc--obj 'model (superchat-bridge-model-id model)
                                    'messages (vconcat messages)
                                    'stream t))
         (format (plist-get compat :thinking-format))
         (reasoning (superchat-bridge-model-reasoning model))
         tools thinking-budget)
    (cl-macrolet ((put! (key value)
                    `(setq params (superchat-bridge-oc--put params ,key ,value))))
      (when prompt-cache-key (put! 'prompt_cache_key prompt-cache-key))
      (when long-cache (put! 'prompt_cache_retention "24h"))
      (unless (null (plist-get compat :supports-usage-in-streaming))
        (put! 'stream_options (superchat-bridge-oc--obj 'include_usage t)))
      (when (plist-get compat :supports-store)
        (put! 'store :false))
      (let ((max-tokens (plist-get options :max-tokens)))
        (when (superchat-bridge-oc--truthy max-tokens)
          (if (equal (plist-get compat :max-tokens-field) "max_tokens")
              ;; Deprecated by OpenAI, but some compatible providers only
              ;; accept max_tokens.
              (put! 'max_tokens max-tokens)
            (put! 'max_completion_tokens max-tokens))))
      (when (plist-get options :temperature)
        (put! 'temperature (plist-get options :temperature)))
      (cond
       ((car transcript-tools)
        (setq tools (vconcat (superchat-bridge-oc--convert-tools
                              (car transcript-tools) compat)))
        (put! 'tools tools)
        (when (plist-get compat :zai-tool-stream)
          (put! 'tool_stream t)))
       ((superchat-bridge-oc--has-tool-history-p messages-in)
        ;; Anthropic (via LiteLLM/proxy) requires the tools parameter when
        ;; the conversation has tool calls or results.
        (setq tools (vector))
        (put! 'tools tools)))
      (when cache-control
        (superchat-bridge-oc--apply-anthropic-cache-control messages tools cache-control))
      (when (superchat-bridge-oc--truthy (plist-get options :tool-choice))
        (put! 'tool_choice (plist-get options :tool-choice)))
      (when (plist-get compat :vllm-priority)
        (put! 'priority (plist-get compat :vllm-priority)))
      (let ((budget-field (superchat-bridge-oc--resolve-thinking-token-budget-field compat)))
        (setq thinking-budget
              (superchat-bridge-oc--resolve-clamped-thinking-budget model options params))
        (cond
         ((and (equal format "zai") reasoning)
          (put! 'thinking (if effort
                              (superchat-bridge-oc--obj 'type "enabled"
                                                 'clear_thinking :false)
                            (superchat-bridge-oc--obj 'type "disabled")))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (let* ((mapped (superchat-bridge-oc--tlm model effort))
                   (value (if (eq mapped :undefined) effort mapped)))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "qwen") reasoning)
          (put! 'enable_thinking (superchat-bridge-oc--bool effort))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (let ((value (let ((mapped (superchat-bridge-oc--tlm model effort)))
                           (if (superchat-bridge-oc--nullish-p mapped) effort mapped))))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "qwen-chat-template") reasoning)
          (put! 'chat_template_kwargs
                (superchat-bridge-oc--obj 'enable_thinking (superchat-bridge-oc--bool effort)
                                   'preserve_thinking t)))
         ((and (equal format "chat-template") reasoning)
          (let ((values (superchat-bridge-oc--build-chat-template-values
                         model options (plist-get compat :chat-template-kwargs)
                         thinking-budget)))
            (when values (put! 'chat_template_kwargs values))))
         ((and (equal format "baseten") reasoning)
          (let ((values (superchat-bridge-oc--build-chat-template-values
                         model options (plist-get compat :chat-template-args)
                         thinking-budget)))
            (when values (put! 'chat_template_args values)))
          (when (plist-get compat :supports-reasoning-effort)
            (let* ((mapped (if effort
                               (superchat-bridge-oc--tlm model effort)
                             (superchat-bridge-oc--tlm model "off")))
                   (value (if (eq mapped :undefined) effort mapped)))
              (when (stringp value) (put! 'reasoning_effort value)))))
         ((and (equal format "deepseek") reasoning)
          (cond (effort (put! 'thinking (superchat-bridge-oc--obj 'type "enabled")))
                ((not (eq (superchat-bridge-oc--tlm model "off") :null))
                 (put! 'thinking (superchat-bridge-oc--obj 'type "disabled"))))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (put! 'reasoning_effort
                  (let ((mapped (superchat-bridge-oc--tlm model effort)))
                    (if (superchat-bridge-oc--nullish-p mapped) effort mapped)))))
         ((and (equal format "openrouter") reasoning)
          ;; OpenRouter normalizes reasoning across providers via a nested
          ;; reasoning object.
          (cond (effort
                 (put! 'reasoning
                       (superchat-bridge-oc--obj
                        'effort (let ((mapped (superchat-bridge-oc--tlm model effort)))
                                  (if (superchat-bridge-oc--nullish-p mapped) effort mapped)))))
                ((not (eq (superchat-bridge-oc--tlm model "off") :null))
                 (put! 'reasoning
                       (superchat-bridge-oc--obj
                        'effort (let ((off (superchat-bridge-oc--tlm model "off")))
                                  (if (superchat-bridge-oc--nullish-p off) "none" off)))))))
         ((and (equal format "ant-ling") reasoning effort)
          (let ((value (superchat-bridge-oc--tlm model effort)))
            (when (stringp value)
              (put! 'reasoning (superchat-bridge-oc--obj 'effort value)))))
         ((and (equal format "together") reasoning)
          (put! 'reasoning (superchat-bridge-oc--obj 'enabled (superchat-bridge-oc--bool effort)))
          (when (and effort (plist-get compat :supports-reasoning-effort))
            (put! 'reasoning_effort
                  (let ((mapped (superchat-bridge-oc--tlm model effort)))
                    (if (superchat-bridge-oc--nullish-p mapped) effort mapped)))))
         ((and (equal format "string-thinking") reasoning)
          (cond (effort
                 (put! 'thinking
                       (let ((mapped (superchat-bridge-oc--tlm model effort)))
                         (if (superchat-bridge-oc--nullish-p mapped) effort mapped))))
                ((not (eq (superchat-bridge-oc--tlm model "off") :null))
                 (put! 'thinking
                       (let ((off (superchat-bridge-oc--tlm model "off")))
                         (if (superchat-bridge-oc--nullish-p off) "none" off))))))
         ((and effort reasoning (plist-get compat :supports-reasoning-effort))
          ;; OpenAI-style reasoning_effort
          (put! 'reasoning_effort
                (let ((mapped (superchat-bridge-oc--tlm model effort)))
                  (if (superchat-bridge-oc--nullish-p mapped) effort mapped))))
         ((and (not effort) reasoning (plist-get compat :supports-reasoning-effort))
          (let ((off (superchat-bridge-oc--tlm model "off")))
            (when (stringp off) (put! 'reasoning_effort off)))))
        ;; Cap reasoning with a top-level budget field.  Independent of
        ;; thinkingFormat: reasoning and the answer share max_tokens here, so an
        ;; uncapped reasoning phase could leave no answer and no tool call.
        (when (and budget-field thinking-budget)
          (put! (intern budget-field) thinking-budget)))
      ;; OpenRouter provider routing preferences
      (let ((routing (assq 'open-router-routing (superchat-bridge-model-compat model))))
        (when (and routing (not (eq (cdr routing) :null)))
          (put! 'provider (superchat-bridge-oc--routing-json (cdr routing)))))
      ;; Vercel AI Gateway provider routing preferences
      (let ((routing (assq 'vercel-gateway-routing (superchat-bridge-model-compat model))))
        (when (and routing (not (eq (cdr routing) :null)))
          (let* ((only (cdr (assoc "only" (cdr routing))))
                 (order (cdr (assoc "order" (cdr routing)))))
            (when (or only order)
              (let (gateway)
                (when only (setq gateway (append gateway (list (cons 'only (vconcat only))))))
                (when order (setq gateway (append gateway (list (cons 'order (vconcat order))))))
                (put! 'providerOptions (superchat-bridge-oc--obj 'gateway gateway)))))))
      ;; Last, so model and request sampling parameters override named
      ;; request fields.
      (dolist (entry (superchat-bridge-oc--resolve-sampling-params
                      model (or effort "off") (plist-get options :sampling-params)))
        (put! (intern (car entry))
              (if (null (cdr entry)) :false (cdr entry)))))
    params))

;;; Usage and stop reasons

(defun superchat-bridge-oc--parse-chunk-usage (raw-usage model)
  "Return the `superchat-bridge-usage' of RAW-USAGE, a chunk's usage object.
The source's `parseChunkUsage'; the cost is calculated for MODEL."
  (let* ((details (superchat-bridge-oc--jget raw-usage "prompt_tokens_details"))
         (prompt-tokens (or (superchat-bridge-oc--jget raw-usage "prompt_tokens") 0))
         (cache-read (or (superchat-bridge-oc--jget details "cached_tokens")
                         (superchat-bridge-oc--jget raw-usage "prompt_cache_hit_tokens")
                         (superchat-bridge-oc--jget raw-usage "cached_tokens")
                         0))
         (cache-write (or (superchat-bridge-oc--jget details "cache_write_tokens") 0))
         ;; Cached tokens are cache-read tokens (hits); writes are separate and
         ;; not subtracted from them.
         (input (max 0 (- prompt-tokens cache-read cache-write)))
         ;; completion_tokens already includes reasoning_tokens.
         (output (or (superchat-bridge-oc--jget raw-usage "completion_tokens") 0))
         (usage (superchat-bridge-make-usage
                 :input input
                 :output output
                 :cache-read cache-read
                 :cache-write cache-write
                 :reasoning (or (superchat-bridge-oc--jget
                                 (superchat-bridge-oc--jget raw-usage
                                                     "completion_tokens_details")
                                 "reasoning_tokens")
                                0)
                 :total-tokens (+ input output cache-read cache-write)
                 :cost (superchat-bridge-make-cost))))
    (superchat-bridge-calculate-cost model usage)
    usage))

(defun superchat-bridge-oc--map-stop-reason (reason)
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

(defun superchat-bridge-oc--json-text (value)
  "Return VALUE, a parsed JSON value, as `JSON.stringify' writes it."
  (superchat-bridge-safe-json-stringify value))

(defun superchat-bridge-oc--sdk-message (status error message)
  "Return the message of the `openai' SDK's `APIError' for these parts.
The source of the text is `APIError.makeMessage': ERROR is the parsed
`error' member of the body, MESSAGE the body text when it was no JSON."
  (let ((text (cond
               ((and (hash-table-p error)
                     (superchat-bridge-oc--truthy (gethash "message" error)))
                (let ((m (gethash "message" error)))
                  (if (stringp m) m (superchat-bridge-oc--json-text m))))
               ((superchat-bridge-oc--truthy error) (superchat-bridge-oc--json-text error))
               (t message))))
    (cond ((and status text (superchat-bridge-oc--truthy text))
           (format "%d %s" status text))
          (status (format "%d status code (no body)" status))
          ((superchat-bridge-oc--truthy text) text)
          (t "(no status code or body)"))))

(defun superchat-bridge-oc--status-error (status headers body-text)
  "Return the provider error plist for an HTTP STATUS, HEADERS and BODY-TEXT.
It has the keys `superchat-bridge-retry-provider-request' and
`superchat-bridge-normalize-provider-error' read.  What `message' and `error'
hold is what the `openai' SDK puts on its `APIError'."
  (let* ((json (and (> (length body-text) 0)
                    (let ((parsed (superchat-bridge-oc--parse-json-or-nil body-text)))
                      (and (not (eq parsed :invalid)) parsed))))
         (json-p (superchat-bridge-oc--truthy json))
         (error (and (hash-table-p json) (superchat-bridge-oc--jget json "error")))
         (text (and (not json-p) body-text)))
    (list :message (superchat-bridge-oc--sdk-message status error text)
          :status status :headers headers :error error)))

(defun superchat-bridge-oc--connection-error (result timed-out)
  "Return the provider error plist for RESULT, the seam's `(:error ...)'.
TIMED-OUT says the request's own timer ended it."
  (let ((kind (plist-get result :error)))
    (list :message
          (cond ((or timed-out (eq kind 'timeout)) "Request timed out.")
                ((eq kind 'aborted) "Request was aborted.")
                (t (format "Connection error: %s" (plist-get result :message))))
          :status nil :headers nil)))

;;; The stream

(cl-defstruct (superchat-bridge-oc--state
               (:constructor superchat-bridge-oc--make-state)
               (:copier nil))
  "The state of one call of `superchat-bridge-api-openai-completions-stream'."
  stream model options compat output
  (finished nil) handle signal listener timer
  (details nil) text-block thinking-block (has-finish-reason nil)
  (tool-state (make-hash-table :test #'eq))
  (by-index (make-hash-table :test #'eql))
  (by-id (make-hash-table :test #'equal))
  sse (done-seen nil) context)

(cl-defstruct (superchat-bridge-oc--tool
               (:constructor superchat-bridge-oc--make-tool)
               (:copier nil))
  "Scratch state of a streaming tool call block: what the source keeps
in the block's `partialArgs' and `streamIndex'."
  block (partial-args "") index)

(defun superchat-bridge-oc--emit (st type &rest keys)
  "Push an event of TYPE with `partial' and KEYS onto the stream of ST."
  (superchat-bridge-event-stream-push
   (superchat-bridge-oc--state-stream st)
   (apply #'superchat-bridge-make-event :type type
          :partial (superchat-bridge-oc--state-output st) keys)))

(defun superchat-bridge-oc--content-index (st block)
  "Return the index of BLOCK in the content of the message of ST."
  (cl-position block (superchat-bridge-message-content (superchat-bridge-oc--state-output st))
               :test #'eq))

(defun superchat-bridge-oc--add-block (st block)
  "Add BLOCK at the end of the content of the message of ST."
  (let ((output (superchat-bridge-oc--state-output st)))
    (setf (superchat-bridge-message-content output)
          (append (superchat-bridge-message-content output) (list block)))))

(defun superchat-bridge-oc--apply-reasoning-details (st block)
  "Store the streamed reasoning details of ST as the signature of BLOCK.
The source's `applyStreamedReasoningDetails'."
  (when (superchat-bridge-oc--state-details st)
    (setf (superchat-bridge-thinking-content-thinking-signature block)
          (superchat-bridge-oc--serialize
           (vconcat (superchat-bridge-oc--state-details st))))))

(defun superchat-bridge-oc--finish-block (st block)
  "Finish BLOCK, a content block of ST, with its `*_end' event.
The source's `finishBlock'."
  (let ((index (superchat-bridge-oc--content-index st block)))
    (when index
      (cond
       ((superchat-bridge-text-content-p block)
        (superchat-bridge-oc--emit st "text_end" :content-index index
                            :content (superchat-bridge-text-content-text block)))
       ((superchat-bridge-thinking-content-p block)
        (superchat-bridge-oc--apply-reasoning-details st block)
        (superchat-bridge-oc--emit st "thinking_end" :content-index index
                            :content (superchat-bridge-thinking-content-thinking block)))
       ((superchat-bridge-tool-call-p block)
        (let* ((scratch (gethash block (superchat-bridge-oc--state-tool-state st)))
               (parsed (superchat-bridge-parse-tool-call-arguments
                        (and scratch (superchat-bridge-oc--tool-partial-args scratch)))))
          (setf (superchat-bridge-tool-call-arguments block) (cdr (assq 'arguments parsed)))
          (when (assq 'arguments-error parsed)
            (setf (superchat-bridge-tool-call-arguments-error block)
                  (cdr (assq 'arguments-error parsed))))
          ;; Finalize in place and drop the scratch state, so a replay only
          ;; carries parsed arguments.
          (remhash block (superchat-bridge-oc--state-tool-state st))
          (superchat-bridge-oc--emit st "toolcall_end" :content-index index
                              :tool-call block)))))))

(defun superchat-bridge-oc--ensure-text-block (st)
  "Return the text block of ST, starting it when it has none."
  (or (superchat-bridge-oc--state-text-block st)
      (let ((block (superchat-bridge-make-text-content :text "")))
        (setf (superchat-bridge-oc--state-text-block st) block)
        (superchat-bridge-oc--add-block st block)
        (superchat-bridge-oc--emit st "text_start"
                            :content-index (superchat-bridge-oc--content-index st block))
        block)))

(defun superchat-bridge-oc--ensure-thinking-block (st signature)
  "Return the thinking block of ST, starting it with SIGNATURE if none."
  (or (superchat-bridge-oc--state-thinking-block st)
      (let ((block (superchat-bridge-make-thinking-content
                    :thinking "" :thinking-signature signature)))
        (setf (superchat-bridge-oc--state-thinking-block st) block)
        (superchat-bridge-oc--add-block st block)
        (superchat-bridge-oc--emit st "thinking_start"
                            :content-index (superchat-bridge-oc--content-index st block))
        block)))

(defun superchat-bridge-oc--ensure-tool-call-block (st delta)
  "Return the scratch state of the tool call block that DELTA continues.
DELTA is the parsed tool call delta.  A block is started when none has
its stream index or its id.  The source's `ensureToolCallBlock'."
  (let* ((index (let ((i (gethash "index" delta))) (and (numberp i) i)))
         (id (superchat-bridge-oc--jget delta "id"))
         (function (superchat-bridge-oc--jget delta "function"))
         (name (or (superchat-bridge-oc--jget function "name") ""))
         (scratch (or (and index (gethash index (superchat-bridge-oc--state-by-index st)))
                      (and id (gethash id (superchat-bridge-oc--state-by-id st))))))
    (unless scratch
      (let ((block (superchat-bridge-make-tool-call
                    :id (or id "") :name name
                    :arguments (superchat-bridge-json-object))))
        (setq scratch (superchat-bridge-oc--make-tool :block block :index index))
        (puthash block scratch (superchat-bridge-oc--state-tool-state st))
        (when index (puthash index scratch (superchat-bridge-oc--state-by-index st)))
        (when id (puthash id scratch (superchat-bridge-oc--state-by-id st)))
        (superchat-bridge-oc--add-block st block)
        (superchat-bridge-oc--emit st "toolcall_start"
                            :content-index (superchat-bridge-oc--content-index st block))))
    (let ((block (superchat-bridge-oc--tool-block scratch)))
      (when (and index (null (superchat-bridge-oc--tool-index scratch)))
        (setf (superchat-bridge-oc--tool-index scratch) index)
        (puthash index scratch (superchat-bridge-oc--state-by-index st)))
      (when id (puthash id scratch (superchat-bridge-oc--state-by-id st)))
      (when (and (string-empty-p (or (superchat-bridge-tool-call-name block) ""))
                 (> (length name) 0))
        (setf (superchat-bridge-tool-call-name block) name))
      scratch)))

(defun superchat-bridge-oc--process-tool-call-delta (st delta)
  "Apply DELTA, one entry of a delta's `tool_calls', to the blocks of ST."
  (when (hash-table-p delta)
    (let* ((scratch (superchat-bridge-oc--ensure-tool-call-block st delta))
           (block (superchat-bridge-oc--tool-block scratch))
           (id (superchat-bridge-oc--jget delta "id"))
           (function (superchat-bridge-oc--jget delta "function"))
           (name (superchat-bridge-oc--jget function "name"))
           (arguments (superchat-bridge-oc--jget function "arguments"))
           (text ""))
      (when (and (string-empty-p (or (superchat-bridge-tool-call-id block) "")) id)
        (setf (superchat-bridge-tool-call-id block) id)
        (puthash id scratch (superchat-bridge-oc--state-by-id st)))
      (when (and (string-empty-p (or (superchat-bridge-tool-call-name block) "")) name)
        (setf (superchat-bridge-tool-call-name block) name))
      (when (and (stringp arguments) (> (length arguments) 0))
        (setq text arguments)
        (setf (superchat-bridge-oc--tool-partial-args scratch)
              (concat (superchat-bridge-oc--tool-partial-args scratch) arguments))
        (setf (superchat-bridge-tool-call-arguments block)
              (superchat-bridge-parse-streaming-json
               (superchat-bridge-oc--tool-partial-args scratch))))
      (superchat-bridge-oc--emit st "toolcall_delta"
                          :content-index (superchat-bridge-oc--content-index st block)
                          :delta text))))

(defun superchat-bridge-oc--process-delta (st delta)
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
             (signature (if (and (equal (superchat-bridge-model-provider
                                         (superchat-bridge-oc--state-model st))
                                        "opencode-go")
                                 (equal field "reasoning"))
                            "reasoning_content"
                          field))
             (block (superchat-bridge-oc--ensure-thinking-block st signature)))
        (setf (superchat-bridge-thinking-content-thinking block)
              (concat (superchat-bridge-thinking-content-thinking block) text))
        (superchat-bridge-oc--emit st "thinking_delta"
                            :content-index (superchat-bridge-oc--content-index st block)
                            :delta text))))
  (let ((details (gethash "reasoning_details" delta)))
    (when (vectorp details)
      (seq-doseq (detail details)
        (when (superchat-bridge-oc--detail-p detail)
          (superchat-bridge-oc--ensure-thinking-block st "")
          ;; Replay data stays in the signature slot; text and summary deltas
          ;; merge into logical entries, encrypted ones stay discrete.
          (setf (superchat-bridge-oc--state-details st)
                (superchat-bridge-oc--append-detail
                 (superchat-bridge-oc--state-details st) detail))))))
  (let ((content (gethash "content" delta)))
    (when (and (stringp content) (> (length content) 0))
      (let ((block (superchat-bridge-oc--ensure-text-block st)))
        (setf (superchat-bridge-text-content-text block)
              (concat (superchat-bridge-text-content-text block) content))
        (superchat-bridge-oc--emit st "text_delta"
                            :content-index (superchat-bridge-oc--content-index st block)
                            :delta content))))
  (let ((tool-calls (gethash "tool_calls" delta)))
    (when (vectorp tool-calls)
      (seq-doseq (tool-call tool-calls)
        (superchat-bridge-oc--process-tool-call-delta st tool-call)))))

(defun superchat-bridge-oc--process-chunk (st chunk)
  "Apply CHUNK, a parsed `ChatCompletionChunk', to the message of ST.
This is the body of the source's `for await (const chunk ...)' loop."
  (let ((model (superchat-bridge-oc--state-model st))
        (output (superchat-bridge-oc--state-output st))
        (hook (plist-get (superchat-bridge-oc--state-options st)
                         :on-provider-stream-event)))
    (when hook (funcall hook chunk model))
    (when (hash-table-p chunk)
      ;; Each chunk of a completion carries the same id.
      (unless (superchat-bridge-oc--truthy (superchat-bridge-assistant-message-response-id output))
        (setf (superchat-bridge-assistant-message-response-id output)
              (superchat-bridge-oc--jget chunk "id")))
      (let ((chunk-model (gethash "model" chunk)))
        (when (and (stringp chunk-model) (> (length chunk-model) 0)
                   (not (equal chunk-model (superchat-bridge-model-id model)))
                   (not (superchat-bridge-oc--truthy
                         (superchat-bridge-assistant-message-response-model output))))
          (setf (superchat-bridge-assistant-message-response-model output) chunk-model)))
      (when (superchat-bridge-oc--jget chunk "usage")
        (setf (superchat-bridge-assistant-message-usage output)
              (superchat-bridge-oc--parse-chunk-usage (superchat-bridge-oc--jget chunk "usage")
                                               model)))
      (let* ((choices (gethash "choices" chunk))
             (choice (and (vectorp choices) (> (length choices) 0)
                          (aref choices 0))))
        (when (hash-table-p choice)
          ;; Some providers (e.g. Moonshot) put usage in choice.usage.
          (when (and (not (superchat-bridge-oc--jget chunk "usage"))
                     (superchat-bridge-oc--jget choice "usage"))
            (setf (superchat-bridge-assistant-message-usage output)
                  (superchat-bridge-oc--parse-chunk-usage
                   (superchat-bridge-oc--jget choice "usage") model)))
          (let ((finish-reason (superchat-bridge-oc--jget choice "finish_reason")))
            (when (superchat-bridge-oc--truthy finish-reason)
              (setf (superchat-bridge-assistant-message-raw-stop-reason output)
                    finish-reason)
              (let ((mapped (superchat-bridge-oc--map-stop-reason finish-reason)))
                (setf (superchat-bridge-assistant-message-stop-reason output) (car mapped))
                (when (cdr mapped)
                  (setf (superchat-bridge-assistant-message-error-message output)
                        (cdr mapped))))
              (setf (superchat-bridge-oc--state-has-finish-reason st) t)))
          (let ((delta (superchat-bridge-oc--jget choice "delta")))
            (when (hash-table-p delta)
              (superchat-bridge-oc--process-delta st delta))))))))

;;; Ending the stream

(defun superchat-bridge-oc--cleanup (st)
  "Release what ST holds: its abort listener, timer and HTTP request."
  (let ((signal (superchat-bridge-oc--state-signal st))
        (listener (superchat-bridge-oc--state-listener st))
        (timer (superchat-bridge-oc--state-timer st))
        (handle (superchat-bridge-oc--state-handle st)))
    (when (and signal listener)
      (superchat-bridge-abort-signal-remove-listener signal listener))
    (setf (superchat-bridge-oc--state-listener st) nil)
    (when timer
      (funcall superchat-bridge-retry-cancel-timer-function timer)
      (setf (superchat-bridge-oc--state-timer st) nil))
    (when handle
      (setf (superchat-bridge-oc--state-handle st) nil)
      (superchat-bridge-http-abort handle))))

(defun superchat-bridge-oc--fail (st err)
  "End the stream of ST with an `error' event for ERR.
This is the `catch' of the source's `stream'.  ERR is a Lisp error
condition or an error plist.  Do nothing when the stream has ended."
  (unless (superchat-bridge-oc--state-finished st)
    (setf (superchat-bridge-oc--state-finished st) t)
    (superchat-bridge-oc--cleanup st)
    (let ((output (superchat-bridge-oc--state-output st))
          (signal (superchat-bridge-oc--state-signal st)))
      (dolist (block (superchat-bridge-message-content output))
        (when (superchat-bridge-thinking-content-p block)
          (superchat-bridge-oc--apply-reasoning-details st block)))
      (setf (superchat-bridge-assistant-message-stop-reason output)
            (if (and signal (superchat-bridge-abort-signal-aborted-p signal))
                "aborted" "error"))
      (setf (superchat-bridge-assistant-message-error-message output)
            (superchat-bridge-format-provider-error
             (superchat-bridge-normalize-provider-error err)))
      ;; Some providers via OpenRouter give more information in this field.
      ;; The parsed body is already in the message, so the raw metadata is
      ;; appended only when it is not there yet.
      (let* ((error-object (and (consp err) (keywordp (car err))
                                (plist-get err :error)))
             (metadata (superchat-bridge-oc--jget error-object "metadata"))
             (raw (superchat-bridge-oc--jget metadata "raw")))
        (when (superchat-bridge-oc--truthy raw)
          (let ((raw-text (if (stringp raw) raw
                            (if (hash-table-p raw) "[object Object]"
                              (format "%s" raw)))))
            (unless (superchat-bridge-oc--includes
                     (superchat-bridge-assistant-message-error-message output) raw-text)
              (setf (superchat-bridge-assistant-message-error-message output)
                    (concat (superchat-bridge-assistant-message-error-message output)
                            "\n" raw-text))))))
      (superchat-bridge-event-stream-push
       (superchat-bridge-oc--state-stream st)
       (superchat-bridge-make-event
        :type "error"
        :reason (superchat-bridge-assistant-message-stop-reason output)
        :error output))
      (superchat-bridge-event-stream-end (superchat-bridge-oc--state-stream st)))))

(defun superchat-bridge-oc--complete (st)
  "End the stream of ST normally, after the last chunk.
The part of the source's `stream' after its loop; an error it throws
goes to `superchat-bridge-oc--fail' in the caller."
  (let* ((output (superchat-bridge-oc--state-output st))
         (compat (superchat-bridge-oc--state-compat st))
         (signal (superchat-bridge-oc--state-signal st)))
    (dolist (block (copy-sequence (superchat-bridge-message-content output)))
      (superchat-bridge-oc--finish-block st block))
    (when (and signal (superchat-bridge-abort-signal-aborted-p signal))
      (error "Request was aborted"))
    (when (equal (superchat-bridge-assistant-message-stop-reason output) "aborted")
      (error "Request was aborted"))
    (when (and (not (superchat-bridge-oc--state-has-finish-reason st))
               (not (plist-get compat :supports-finish-reason)))
      (setf (superchat-bridge-assistant-message-stop-reason output)
            (if (seq-some #'superchat-bridge-tool-call-p
                          (superchat-bridge-message-content output))
                "toolUse" "stop")))
    (when (equal (superchat-bridge-assistant-message-stop-reason output) "error")
      (let ((message (superchat-bridge-assistant-message-error-message output)))
        (error "%s" (if (superchat-bridge-oc--truthy message)
                        message
                      "Provider returned an error stop reason"))))
    (when (or (and (plist-get compat :supports-finish-reason)
                   (not (superchat-bridge-oc--state-has-finish-reason st)))
              (equal (superchat-bridge-assistant-message-stop-reason output) "pending"))
      (error "Stream ended without finish_reason"))
    (setf (superchat-bridge-oc--state-finished st) t)
    (superchat-bridge-oc--cleanup st)
    (superchat-bridge-event-stream-push
     (superchat-bridge-oc--state-stream st)
     (superchat-bridge-make-event
      :type "done" :reason (superchat-bridge-assistant-message-stop-reason output)
      :message output))
    (superchat-bridge-event-stream-end (superchat-bridge-oc--state-stream st))))

;;; Reading the response

(defun superchat-bridge-oc--on-sse (st _event data)
  "Take one server-sent event with DATA for ST: a chunk, `[DONE]' or an error."
  (unless (or (superchat-bridge-oc--state-finished st)
              (superchat-bridge-oc--state-done-seen st))
    (if (string-prefix-p "[DONE]" data)
        (setf (superchat-bridge-oc--state-done-seen st) t)
      (let ((chunk (json-parse-string data :object-type 'hash-table
                                      :array-type 'array :null-object :null
                                      :false-object :false)))
        (let ((error-object (and (hash-table-p chunk)
                                 (superchat-bridge-oc--jget chunk "error"))))
          (if (superchat-bridge-oc--truthy error-object)
              ;; The SDK throws an APIError without a status for such a chunk.
              (superchat-bridge-oc--fail
               st (list :message (superchat-bridge-oc--sdk-message nil error-object nil)
                        :status nil :headers nil :error error-object))
            (superchat-bridge-oc--process-chunk st chunk)))))))

(defun superchat-bridge-oc--on-body (st text)
  "Take TEXT, a piece of the response body, for ST."
  (unless (superchat-bridge-oc--state-finished st)
    (condition-case err
        (funcall (superchat-bridge-oc--state-sse st) text)
      (error (superchat-bridge-oc--fail st err)))))

(defun superchat-bridge-oc--on-end (st)
  "Take the end of the response body for ST."
  (unless (superchat-bridge-oc--state-finished st)
    (condition-case err
        (superchat-bridge-oc--complete st)
      (error (superchat-bridge-oc--fail st err)))))

(defun superchat-bridge-oc--attempt (st url headers body done)
  "Make one request for ST and call DONE with its outcome.
DONE is the continuation `superchat-bridge-retry-provider-request' gives its
request function: it gets `(:ok (STATUS HEADERS))' as soon as the
response headers say success, or `(:error PLIST)'.  The body then goes
to ST.  This is the `client.chat.completions.create(...).withResponse()'
of the source."
  (let ((signal (superchat-bridge-oc--state-signal st))
        (timeout-ms (plist-get (superchat-bridge-oc--state-options st) :timeout-ms))
        (status nil) (response-headers nil) (error-chunks nil)
        (streaming nil) (timed-out nil) (timer nil))
    (if (and signal (superchat-bridge-abort-signal-aborted-p signal))
        (funcall done (list :error (list :message "Request was aborted."
                                         :status nil :headers nil)))
      (let ((stop-timer (lambda ()
                          (when timer
                            (funcall superchat-bridge-retry-cancel-timer-function timer)
                            (setq timer nil)))))
        (setf (superchat-bridge-oc--state-handle st)
              (superchat-bridge-http-request
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
                     (superchat-bridge-oc--on-body st text)
                   (push text error-chunks)))
               :on-done
               (lambda (result)
                 (funcall stop-timer)
                 (cond
                  ((plist-get result :status)
                   (if streaming
                       (superchat-bridge-oc--on-end st)
                     (funcall done
                              (list :error
                                    (superchat-bridge-oc--status-error
                                     (plist-get result :status)
                                     (plist-get result :headers)
                                     (apply #'concat (reverse error-chunks)))))))
                  (streaming
                   (superchat-bridge-oc--fail
                    st (superchat-bridge-oc--connection-error result timed-out)))
                  (status
                   (funcall done
                            (list :error
                                  (superchat-bridge-oc--status-error
                                   status response-headers
                                   (apply #'concat (reverse error-chunks))))))
                  (t
                   (funcall done
                            (list :error (superchat-bridge-oc--connection-error
                                          result timed-out))))))))
        ;; The SDK's timeout covers the time until the headers arrive.
        (when timeout-ms
          (setq timer
                (superchat-bridge--start-timer
                 timeout-ms
                 (lambda ()
                   (setq timer nil timed-out t)
                   (let ((handle (superchat-bridge-oc--state-handle st)))
                     (when handle (superchat-bridge-http-abort handle)))))))))))

(defun superchat-bridge-oc--on-response (st result)
  "Take RESULT of the request of ST: the source's code after `await retry...'."
  (if (eq (car result) :error)
      (superchat-bridge-oc--fail st (cadr result))
    (condition-case err
        (let* ((model (superchat-bridge-oc--state-model st))
               (hook (plist-get (superchat-bridge-oc--state-options st) :on-response))
               (response (cadr result)))
          (unless (superchat-bridge-oc--state-finished st)
            (when hook
              (funcall hook (superchat-bridge-make-provider-response
                             :status (car response)
                             :headers (superchat-bridge-headers-to-record
                                       (cadr response)))
                       model))
            (unless (superchat-bridge-oc--state-finished st)
              (superchat-bridge-oc--emit st "start"))))
      (error (superchat-bridge-oc--fail st err)))))

(defun superchat-bridge-oc--run (st)
  "Run the request of ST: the body of the source's `try' block."
  (condition-case err
      (let* ((model (superchat-bridge-oc--state-model st))
             (options (superchat-bridge-oc--state-options st))
             (compat (superchat-bridge-oc--state-compat st))
             (api-key (superchat-bridge-oc--get-client-api-key
                       (superchat-bridge-model-provider model)
                       (plist-get options :api-key) (plist-get options :headers)))
             (cache-retention (superchat-bridge-oc--resolve-cache-retention
                               (plist-get options :cache-retention)
                               (plist-get options :env)))
             (cache-session-id (if (equal cache-retention "none")
                                   nil (plist-get options :session-id)))
             (headers (superchat-bridge-oc--request-headers
                       api-key model cache-session-id compat
                       (plist-get options :headers)))
             (params (superchat-bridge-oc--build-params
                      model (superchat-bridge-oc--state-context st) options compat
                      cache-retention))
             (hook (plist-get options :on-payload))
             (next-params (and hook (funcall hook params model)))
             (body (superchat-bridge-oc--serialize (or next-params params)))
             (url (superchat-bridge-oc--request-url (superchat-bridge-model-base-url model)))
             (signal (superchat-bridge-oc--state-signal st))
             (max-retries (plist-get options :max-retries))
             (max-retry-delay-ms (plist-get options :max-retry-delay-ms)))
        (setf (superchat-bridge-oc--state-sse st)
              (superchat-bridge-sse-parser
               (lambda (event data) (superchat-bridge-oc--on-sse st event data))))
        (when (and signal (not (superchat-bridge-abort-signal-aborted-p signal)))
          (let ((listener (lambda ()
                            (let ((handle (superchat-bridge-oc--state-handle st)))
                              (when handle (superchat-bridge-http-abort handle))))))
            (setf (superchat-bridge-oc--state-listener st) listener)
            (superchat-bridge-abort-signal-add-listener signal listener)))
        (apply #'superchat-bridge-retry-provider-request
               (lambda (done) (superchat-bridge-oc--attempt st url headers body done))
               (lambda (result) (superchat-bridge-oc--on-response st result))
               :signal signal
               (append (and max-retries (list :max-retries max-retries))
                       (and max-retry-delay-ms
                            (list :max-retry-delay-ms max-retry-delay-ms)))))
    (error (superchat-bridge-oc--fail st err))))

;;; Entry points

(defun superchat-bridge-api-openai-completions-stream (model context options)
  "Stream a response of MODEL to CONTEXT, return the event stream at once.
The source's `stream'.  See the file commentary for MODEL, CONTEXT and
OPTIONS.  Every failure of the request ends the stream with an `error'
event; nothing is signalled for it."
  (let* ((stream (superchat-bridge-create-assistant-message-event-stream))
         (compat (superchat-bridge-api-openai-completions-get-compat model))
         (transcript (superchat-bridge-oc--resolve-transcript
                      context (plist-get compat :supports-mid-convo-system-messages)))
         (output (superchat-bridge-make-assistant-message
                  :content nil
                  :api (superchat-bridge-model-api model)
                  :provider (superchat-bridge-model-provider model)
                  :model (superchat-bridge-model-id model)
                  :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
                  :stop-reason "pending"
                  :timestamp (superchat-bridge-oc--now-ms)))
         (st (superchat-bridge-oc--make-state
              :stream stream :model model :options options :compat compat
              :output output :signal (plist-get options :signal)
              :context transcript)))
    (superchat-bridge-oc--run st)
    stream))

(defun superchat-bridge-api-openai-completions-stream-simple (model context options)
  "Stream for simple options.
Like `superchat-bridge-api-openai-completions-stream' otherwise.
The source's `streamSimple'.  OPTIONS may carry `:reasoning', a level
that is clamped to what MODEL supports.  A missing API key signals an
error here, as it throws in the source."
  (superchat-bridge-oc--get-client-api-key (superchat-bridge-model-provider model)
                                    (plist-get options :api-key)
                                    (plist-get options :headers))
  (let* ((reasoning (plist-get options :reasoning))
         (clamped (and reasoning
                       (superchat-bridge-clamp-thinking-level model reasoning)))
         (effort (if (equal clamped "off") nil clamped)))
    (superchat-bridge-api-openai-completions-stream
     model context
     (list :temperature (plist-get options :temperature)
           :sampling-params (superchat-bridge-oc--resolve-sampling-params
                             model (or reasoning "off")
                             (plist-get options :sampling-params))
           :max-tokens (superchat-bridge-oc--clamp-max-tokens-to-context
                        model context
                        (or (plist-get options :max-tokens)
                            (superchat-bridge-model-max-tokens model)))
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

(provide 'superchat-bridge-api-openai-completions)

;;; superchat-bridge-api-openai-completions.el ends here
