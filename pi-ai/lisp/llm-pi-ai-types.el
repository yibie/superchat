;;; llm-pi-ai-types.el --- Core types of pi-ai, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/types.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).

;; TypeScript interfaces become `cl-defstruct's.  Field names follow the
;; source; the accessors written by hand below are the authority:
;;
;;   `llm-pi-ai-message-<field>'          fields every message role has
;;   `llm-pi-ai-assistant-message-<field>'
;;   `llm-pi-ai-user-message-<field>'
;;   `llm-pi-ai-system-message-<field>'
;;   `llm-pi-ai-tool-result-message-<field>'  (`toolCallId' -> `tool-call-id')
;;   `llm-pi-ai-text-content-<field>'         (`mimeType'    -> `mime-type')
;;   `llm-pi-ai-tool-call-<field>'
;;   `llm-pi-ai-usage-<field>'                (`cacheRead'   -> `cache-read')
;;   `llm-pi-ai-cost-<field>'                 (`cacheRead'   -> `cache-read')
;;   `llm-pi-ai-<struct>-<field>', e.g. `llm-pi-ai-image-model-base-url'
;;
;; Which struct a message has is decided by its first field, `role':
;; `system', `user', `assistant' or `toolResult'.  `llm-pi-ai-message-p'
;; answers for all four.
;;
;; Every "-p" accessor and "-p" constructor is a plain reader and
;; writer, so an absent optional field reads as nil, like `undefined'.

;; Layout.  `cl-defstruct' puts an inherited slot at the position it
;; holds in the parent, and Emacs 32 refuses a child that repeats it
;; with another default.  So a message's struct has the slots of
;; `llm-pi-ai-message' first and then its own; its `role' and other
;; fields are therefore set by the constructors below rather than as
;; slot defaults.

;;; Code:

(require 'cl-lib)
(require 'seq)

;; This unit is checked with `scan-sexps' and `read'; its syntax table is
;; made Lisp's here, because the default one would take "-*-" as a comment.
(modify-syntax-entry ?\; "<"  (syntax-table))
(modify-syntax-entry ?\n ">"  (syntax-table))
(modify-syntax-entry ?\" "\"" (syntax-table))
(modify-syntax-entry ?\\ "\\" (syntax-table))

;;; Known api, provider and model type names

(defconst llm-pi-ai-known-apis
  '("openai-completions"
    "mistral-conversations"
    "openai-responses"
    "azure-openai-responses"
    "openai-codex-responses"
    "anthropic-messages"
    "bedrock-converse-stream"
    "google-generative-ai"
    "google-vertex"
    "pi-messages")
  "The `KnownApi' union of src/types.ts.
An api is any string; these are the ones pi ships adapters for.")

(defconst llm-pi-ai-known-image-apis '("openrouter-images")
  "The `KnownImageApi' union of src/types.ts.")

(defconst llm-pi-ai-known-classifier-apis
  '("typesafe-system-one"
    "cloudflare-workers-ai-system-one"
    "llama-cpp-classify"
    "openai-decisions")
  "The `KnownClassifierApi' union of src/types.ts.")

(defconst llm-pi-ai-known-providers
  '("amazon-bedrock" "ant-ling" "anthropic" "google" "google-vertex"
    "openai" "azure" "openai-codex" "radius" "typesafe" "nvidia"
    "deepseek" "github-copilot" "xai" "groq" "cerebras" "openrouter"
    "vercel-ai-gateway" "zai" "zai-coding-cn" "mistral" "minimax"
    "minimax-cn" "moonshotai" "moonshotai-cn" "huggingface" "fireworks"
    "together" "baseten" "opencode" "opencode-go" "kimi-coding" "meta"
    "cloudflare-workers-ai" "cloudflare-ai-gateway" "qwen-token-plan"
    "qwen-token-plan-cn" "qwen-token-plan-individual" "xiaomi"
    "xiaomi-token-plan-cn" "xiaomi-token-plan-ams" "xiaomi-token-plan-sgp")
  "The `KnownProvider' union of src/types.ts.")

(defconst llm-pi-ai-model-types '(chat image classifier)
  "The `ModelType' union of src/types.ts.")

;;; Content blocks

;; `cl-defstruct' writes a constructor as a function that accepts only
;; the keywords it saw when written, and re-evaluating this file would
;; otherwise leave that old constructor in place, rejecting the new
;; keywords.  Define structs through this macro, which takes the old
;; functions of the struct away first, and forgets the tag list the
;; old definition made, so a later `let'-binding of that variable can
;; hide it and `type-of' keep answering with the old name.
(defmacro llm-pi-ai-defstruct (name-and-options &rest slots-and-doc)
  "Define a struct with `cl-defstruct', replacing an earlier one.
NAME-AND-OPTIONS and SLOTS-AND-DOC are passed on as they are.  Any
constructor and copier that the definition names are unbound first,
so re-evaluating a definition replaces those functions too."
  (declare (indent 1) (debug edebug-forms))
  (let* ((name (if (consp name-and-options)
                   (car name-and-options)
                 name-and-options))
         (name-string (prin1-to-string name))
         (functions nil))
    (dolist (option (cdr-safe name-and-options))
      (let* ((key (car-safe option))
             (value (car-safe (cdr-safe option)))
             (function (cond ((and (eq key :constructor) (symbolp value)) value)
                             ((and (eq key :constructor) (consp value)) (car value))
                             ((and (eq key :copier) (symbolp value)) value))))
        (when function (push function functions))))
    `(progn
       (let ((tags (intern (format "cl-struct-%s-tags" ,name-string))))
         (when (boundp tags) (kill-local-variable tags)))
       (dolist (function ',functions)
         (when (fboundp function) (fmakunbound function)))
       (cl-defstruct ,name-and-options ,@slots-and-doc))))

(llm-pi-ai-defstruct (llm-pi-ai-text-content
                (:constructor llm-pi-ai-make-text-content)
                (:copier llm-pi-ai-copy-text-content))
  "A `TextContent': a piece of model output or user input text."
  (type "text")
  text
  text-signature)

(llm-pi-ai-defstruct (llm-pi-ai-thinking-content
                (:constructor llm-pi-ai-make-thinking-content)
                (:copier llm-pi-ai-copy-thinking-content))
  "A `ThinkingContent': a reasoning block."
  (type "thinking")
  thinking
  thinking-signature
  redacted)

(llm-pi-ai-defstruct (llm-pi-ai-image-content
                (:constructor llm-pi-ai-make-image-content)
                (:copier llm-pi-ai-copy-image-content))
  "An `ImageContent': base64 image data and its media type."
  (type "image")
  data
  mime-type)

(llm-pi-ai-defstruct (llm-pi-ai-tool-call
                (:constructor llm-pi-ai-make-tool-call)
                (:copier llm-pi-ai-copy-tool-call))
  "A `ToolCall' of an assistant message.
`arguments' is a JSON object; `arguments-error' is set when the
model's whole arguments text was not a valid JSON object, and
`arguments' is then an empty object."
  (type "toolCall")
  id
  name
  arguments
  thought-signature
  namespace
  arguments-error)

;;; Usage and cost

(llm-pi-ai-defstruct (llm-pi-ai-cost
                (:constructor llm-pi-ai-make-cost)
                (:copier llm-pi-ai-copy-cost))
  "The `cost' object of `Usage', in US dollars."
  (input 0)
  (output 0)
  (cache-read 0)
  (cache-write 0)
  (total 0))

(llm-pi-ai-defstruct (llm-pi-ai-usage
                (:constructor llm-pi-ai-make-usage))
  "A `Usage'.  `reasoning' is a subset of `output', not an addition."
  (input 0)
  (output 0)
  (cache-read 0)
  (cache-write 0)
  cache-write1h
  reasoning
  (total-tokens 0)
  (cost nil))

;;; Messages

(llm-pi-ai-defstruct (llm-pi-ai-message
                (:constructor llm-pi-ai-make-message)
                (:copier llm-pi-ai-copy-message))
  "The fields `SystemMessage', `UserMessage', `AssistantMessage' and
`ToolResultMessage' share.  Compare with `llm-pi-ai-user-message-p'
and friends, or with (llm-pi-ai-message-role MESSAGE), whose
value names the struct: `system', `user', `assistant' or
`toolResult'.  Do not use this constructor for anything but the
`Message' union in its place in a nodeapi list."
  role
  content
  timestamp)

(llm-pi-ai-defstruct (llm-pi-ai-system-message
                (:include llm-pi-ai-message (role "system"))
                (:constructor llm-pi-ai-make-system-message)
                (:copier llm-pi-ai-copy-system-message))
  "A `SystemMessage'.
The leading system message is the system prompt; later ones change
it: `content' adds instructions from that point on, `sections'
replace or remove named prompt sections by name (a nil value in the
alist removes one), and `tools-added'/`tools-removed' change the
tool set.  `role' is \"system\"."
  sections
  tools-added
  tools-removed)

(llm-pi-ai-defstruct (llm-pi-ai-user-message
                (:include llm-pi-ai-message (role "user"))
                (:constructor llm-pi-ai-make-user-message)
                (:copier llm-pi-ai-copy-user-message))
  "A `UserMessage'.  `role' is \"user\".")

(llm-pi-ai-defstruct (llm-pi-ai-assistant-message
                (:include llm-pi-ai-message (role "assistant"))
                (:constructor llm-pi-ai-make-assistant-message)
                (:copier llm-pi-ai-copy-assistant-message))
  "An `AssistantMessage': the result of one model response.
`role' is \"assistant\"."
  api
  provider
  model
  response-model
  response-id
  provider-thinking-level
  thinking-level
  diagnostics
  usage
  stop-reason
  deferred
  error-message
  raw-stop-reason
  end-turn
  duration-ms)

(llm-pi-ai-defstruct (llm-pi-ai-tool-result-message
                (:include llm-pi-ai-message (role "toolResult"))
                (:constructor llm-pi-ai-make-tool-result-message)
                (:copier llm-pi-ai-copy-tool-result-message))
  "A `ToolResultMessage'.  `nested-calls' is kept for the session
record and is not sent to the model.  `role' is \"toolResult\"."
  tool-call-id
  tool-name
  details
  usage
  nested-calls
  (is-error nil)
  duration-ms)

(defun llm-pi-ai-message-p (object)
  "Return non-nil when OBJECT is any of pi's four message types."
  (or (llm-pi-ai-system-message-p object)
      (llm-pi-ai-user-message-p object)
      (llm-pi-ai-assistant-message-p object)
      (llm-pi-ai-tool-result-message-p object)))

(defun llm-pi-ai-content-p (object)
  "Return non-nil when OBJECT is a content block or a tool call."
  (or (llm-pi-ai-text-content-p object)
      (llm-pi-ai-thinking-content-p object)
      (llm-pi-ai-image-content-p object)
      (llm-pi-ai-tool-call-p object)))

;;; Assistant message events

(llm-pi-ai-defstruct (llm-pi-ai-assistant-message-event
                (:constructor llm-pi-ai-make-event)
                (:copier llm-pi-ai-copy-event))
  "One `AssistantMessageEvent'.
`type' is the event type string; the other fields are the ones that
type carries, and the others are nil:

  \"start\" \"text_start\" \"text_delta\" \"text_end\"
  \"thinking_start\" \"thinking_delta\" \"thinking_end\"
  \"toolcall_start\" \"toolcall_delta\" \"toolcall_end\"
       content-index, partial, delta, content, tool-call
  \"done\"   reason, message
  \"error\"  reason, error

`partial' is the shared live response-so-far object, not an
event-time copy: its text and thinking blocks are empty when their
`*_start' event is emitted and grow through their `*_delta' events
until the authoritative `*_end'."
  type
  content-index
  partial
  delta
  content
  tool-call
  reason
  message
  error)

(defconst llm-pi-ai-stop-reasons
  '("pending" "stop" "length" "toolUse" "error" "aborted" "deferred")
  "The `StopReason' union of src/types.ts.")

(defconst llm-pi-ai-usage-stop-reasons '("stop" "length" "toolUse" "deferred")
  "Stop reasons of a `done' event, i.e. every reason but errors.")

(defconst llm-pi-ai-error-stop-reasons '("aborted" "error")
  "Stop reasons of an `error' event.")

(defconst llm-pi-ai-images-stop-reasons '("stop" "error" "aborted")
  "The `ImagesStopReason' union of src/types.ts.")

;;; Tools and request context

(llm-pi-ai-defstruct (llm-pi-ai-tool
                (:constructor llm-pi-ai-make-tool)
                (:copier llm-pi-ai-copy-tool))
  "A `Tool'.  `parameters' is a JSON Schema, held as an alist or plist
as typebox would describe it.  `constrained-sampling' is nil, t or
a description of the provider-side sampling constraint."
  name
  description
  parameters
  constrained-sampling)

(llm-pi-ai-defstruct (llm-pi-ai-tool-reference
                (:constructor llm-pi-ai-make-tool-reference)
                (:copier llm-pi-ai-copy-tool-reference))
  "A `ToolReference': names a tool without repeating its definition."
  name)

(llm-pi-ai-defstruct (llm-pi-ai-context
                (:constructor llm-pi-ai-make-context)
                (:copier llm-pi-ai-copy-context))
  "A request `Context'.  `system-prompt' and `tools' are shorthand for
a leading system message; `normalize-context' (not yet ported) folds
them into one before a provider sees the request."
  system-prompt
  messages
  tools)

(defun llm-pi-ai-transcript-context-p (object)
  "Return non-nil when OBJECT looks like a normalized request context.
TypeScript brands `TranscriptContext' so that only
`normalizeContext()' can produce one.  A Lisp value carries no
brand, so a context whose `system-prompt' is unset is taken as
normalized, which is what `normalizeContext()' guarantees."
  (and (llm-pi-ai-context-p object)
       (null (llm-pi-ai-context-system-prompt object))
       t))

;;; Deferred responses

(llm-pi-ai-defstruct (llm-pi-ai-deferred-handle
                (:constructor llm-pi-ai-make-deferred-handle)
                (:copier llm-pi-ai-copy-deferred-handle))
  "A `DeferredHandle': what to poll to obtain an asynchronous response."
  provider
  model-id
  api
  id
  expires-at
  poll-after-ms
  data)

;;; Images

(llm-pi-ai-defstruct (llm-pi-ai-images-context
                (:constructor llm-pi-ai-make-images-context)
                (:copier llm-pi-ai-copy-images-context))
  "An `ImagesContext'."
  input)

(llm-pi-ai-defstruct (llm-pi-ai-assistant-images
                (:constructor llm-pi-ai-make-assistant-images)
                (:copier llm-pi-ai-copy-assistant-images))
  "An `AssistantImages': one image-generation result, returned whole
instead of as an event stream."
  api
  provider
  model
  output
  response-id
  usage
  stop-reason
  error-message
  timestamp)

;;; Classifiers

(llm-pi-ai-defstruct (llm-pi-ai-classifier-context
                (:constructor llm-pi-ai-make-classifier-context)
                (:copier llm-pi-ai-copy-classifier-context))
  "A `ClassifierContext'.  `questions' maps a name to a question,
which is a plist with at least `type' (`choice', `score' or
`bool'), `instructions' and `criteria'."
  state
  images
  questions)

(llm-pi-ai-defstruct (llm-pi-ai-classifier-result
                (:constructor llm-pi-ai-make-classifier-result)
                (:copier llm-pi-ai-copy-classifier-result))
  "A `ClassifierResult'.  An answer in `answers' is a plist whose
`type' is `choice', `score' or `bool'; see `ClassifierAnswer'."
  api
  provider
  model
  answers
  usage
  stop-reason
  error-message
  timestamp)

;;; Models

(llm-pi-ai-defstruct (llm-pi-ai-model-cost-rates
                (:constructor llm-pi-ai-make-model-cost-rates)
                (:copier llm-pi-ai-copy-model-cost-rates))
  "`ModelCostRates': US dollars per million tokens."
  (input 0)
  (output 0)
  (cache-read 0)
  (cache-write 0))

(llm-pi-ai-defstruct (llm-pi-ai-model-cost-tier
                (:include llm-pi-ai-model-cost-rates)
                (:constructor llm-pi-ai-make-model-cost-tier)
                (:copier llm-pi-ai-copy-model-cost-tier))
  "A `ModelCostTier': rates that apply when the request's input
exceeds `input-tokens-above'."
  (input-tokens-above 0))

(llm-pi-ai-defstruct (llm-pi-ai-model-cost
                (:include llm-pi-ai-model-cost-rates)
                (:constructor llm-pi-ai-make-model-cost)
                (:copier llm-pi-ai-copy-model-cost))
  "A catalog `ModelCost'.  `tiers' holds `ModelCostTier's; the highest
matching input threshold applies to the whole request."
  tiers)

(llm-pi-ai-defstruct (llm-pi-ai-model-image-resize-options
                (:constructor llm-pi-ai-make-model-image-resize-options)
                (:copier llm-pi-ai-copy-model-image-resize-options))
  "A `ModelImageResizeOptions'.  `max-bytes' limits the base64 payload."
  max-width
  max-height
  max-bytes
  jpeg-quality)

(llm-pi-ai-defstruct (llm-pi-ai-model-image-input-limits
                (:constructor llm-pi-ai-make-model-image-input-limits)
                (:copier llm-pi-ai-copy-model-image-input-limits))
  "A `ModelImageInputLimits'."
  resize
  max-per-message
  max-per-request)

(llm-pi-ai-defstruct (llm-pi-ai-model-input-limits
                (:constructor llm-pi-ai-make-model-input-limits)
                (:copier llm-pi-ai-copy-model-input-limits))
  "`ModelInputLimits': request size and image limits."
  max-request-bytes
  images)

(llm-pi-ai-defstruct (llm-pi-ai-model
                (:constructor llm-pi-ai-make-model)
                (:copier llm-pi-ai-copy-model))
  "A catalog chat `Model'.  `type' is `chat' or nil; nil means chat,
as the source makes that the default.  `input' is a list of
`text'/`image' modality strings; `compat' is an alist of the
provider's compatibility overrides.  Use `llm-pi-ai-get-model-type'
rather than reading `type' directly.

`llm-pi-ai-image-model' and `llm-pi-ai-classifier-model' include
this struct, and a slot belongs to the parent that declares it, so
their `type' is this slot.  Build them with
`llm-pi-ai-make-image-model' and `llm-pi-ai-make-classifier-model',
which set it."
  (type nil)
  id
  name
  api
  provider
  base-url
  input
  input-limits
  cost
  headers
  reasoning
  thinking-level-map
  prompt-cache
  context-window
  max-tokens
  sampling-params
  sampling-params-by-thinking-level
  compat)

(llm-pi-ai-defstruct (llm-pi-ai-image-model
                (:include llm-pi-ai-model (type 'image))
                (:constructor llm-pi-ai-make-image-model)
                (:copier llm-pi-ai-copy-image-model))
  "An `ImageModel': usable with image generation only.  `output' always
includes `image'; `text' means the model can also return text.
`type' is `image'."
  output)

(llm-pi-ai-defstruct (llm-pi-ai-classifier-model
                (:include llm-pi-ai-model (type 'classifier))
                (:constructor llm-pi-ai-make-classifier-model)
                (:copier llm-pi-ai-copy-classifier-model))
  "A `ClassifierModel': usable with classification only.
`type' is `classifier'.")

(defun llm-pi-ai-get-model-type (model)
  "Return the `ModelType' of MODEL: `chat', `image' or `classifier'.
A chat model may leave `type' unset; it is then chat."
  (or (llm-pi-ai-model-type model) 'chat))

(defun llm-pi-ai-is-model-type (model type)
  "Return non-nil when MODEL is of TYPE, one of `chat', `image' or
`classifier'.  This is the `isModelType' predicate the source asks
callers to use instead of comparing `type' themselves."
  (eq (llm-pi-ai-get-model-type model) type))

;;; Other option shapes

(llm-pi-ai-defstruct (llm-pi-ai-provider-response
                (:constructor llm-pi-ai-make-provider-response)
                (:copier llm-pi-ai-copy-provider-response))
  "A `ProviderResponse' passed to `on-response' callbacks."
  status
  headers)

(llm-pi-ai-defstruct (llm-pi-ai-thinking-budgets
                (:constructor llm-pi-ai-make-thinking-budgets)
                (:copier llm-pi-ai-copy-thinking-budgets))
  "`ThinkingBudgets': token budgets per thinking level."
  minimal
  low
  medium
  high)

(defconst llm-pi-ai-thinking-levels
  '("minimal" "low" "medium" "high" "xhigh" "max")
  "The `THINKING_LEVELS' of src/providers/model-schema.ts.")

(defconst llm-pi-ai-model-thinking-levels
  (cons "off" llm-pi-ai-thinking-levels)
  "The `MODEL_THINKING_LEVELS' of src/providers/model-schema.ts.")

(defconst llm-pi-ai-model-input-modalities '("text" "image")
  "The `MODEL_INPUT_MODALITIES' of src/providers/model-schema.ts.")

(defconst llm-pi-ai-cache-retentions '("none" "short" "long")
  "The `CACHE_RETENTIONS' of src/providers/model-schema.ts.")

(defconst llm-pi-ai-model-prompt-cache-retentions '("short" "long")
  "The `MODEL_PROMPT_CACHE_RETENTIONS' of src/providers/model-schema.ts.")

(defconst llm-pi-ai-transports '("sse" "websocket" "websocket-cached" "auto")
  "The `Transport' union of src/types.ts.")

(defconst llm-pi-ai-tool-choices '("auto" "none")
  "The `ToolChoice' union of src/types.ts.")

(defconst llm-pi-ai-grammar-formats '("openai_lark" "openai_regex")
  "The `GrammarFormat' union of src/types.ts.")


(provide 'llm-pi-ai-types)

;;; llm-pi-ai-types.el ends here
