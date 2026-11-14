;;; superchat-bridge-types.el --- Core types of pi-ai, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/types.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).

;; TypeScript interfaces become `cl-defstruct's.  Field names follow the
;; source; the accessors written by hand below are the authority:
;;
;;   `superchat-bridge-message-<field>'          fields every message role has
;;   `superchat-bridge-assistant-message-<field>'
;;   `superchat-bridge-user-message-<field>'
;;   `superchat-bridge-system-message-<field>'
;;   `superchat-bridge-tool-result-message-<field>'  (`toolCallId' -> `tool-call-id')
;;   `superchat-bridge-text-content-<field>'         (`mimeType'    -> `mime-type')
;;   `superchat-bridge-tool-call-<field>'
;;   `superchat-bridge-usage-<field>'                (`cacheRead'   -> `cache-read')
;;   `superchat-bridge-cost-<field>'                 (`cacheRead'   -> `cache-read')
;;   `superchat-bridge-<struct>-<field>', e.g. `superchat-bridge-image-model-base-url'
;;
;; Which struct a message has is decided by its first field, `role':
;; `system', `user', `assistant' or `toolResult'.  `superchat-bridge-message-p'
;; answers for all four.
;;
;; Every "-p" accessor and "-p" constructor is a plain reader and
;; writer, so an absent optional field reads as nil, like `undefined'.

;; Layout.  `cl-defstruct' puts an inherited slot at the position it
;; holds in the parent, and Emacs 32 refuses a child that repeats it
;; with another default.  So a message's struct has the slots of
;; `superchat-bridge-message' first and then its own; its `role' and other
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

(defconst superchat-bridge-known-apis
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

(defconst superchat-bridge-known-image-apis '("openrouter-images")
  "The `KnownImageApi' union of src/types.ts.")

(defconst superchat-bridge-known-classifier-apis
  '("typesafe-system-one"
    "cloudflare-workers-ai-system-one"
    "llama-cpp-classify"
    "openai-decisions")
  "The `KnownClassifierApi' union of src/types.ts.")

(defconst superchat-bridge-known-providers
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

(defconst superchat-bridge-model-types '(chat image classifier)
  "The `ModelType' union of src/types.ts.")

;;; Content blocks

;; `cl-defstruct' writes a constructor as a function that accepts only
;; the keywords it saw when written, and re-evaluating this file would
;; otherwise leave that old constructor in place, rejecting the new
;; keywords.  Define structs through this macro, which takes the old
;; functions of the struct away first, and forgets the tag list the
;; old definition made, so a later `let'-binding of that variable can
;; hide it and `type-of' keep answering with the old name.
(defmacro superchat-bridge-defstruct (name-and-options &rest slots-and-doc)
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

(superchat-bridge-defstruct (superchat-bridge-text-content
                (:constructor superchat-bridge-make-text-content)
                (:copier superchat-bridge-copy-text-content))
  "A `TextContent': a piece of model output or user input text."
  (type "text")
  text
  text-signature)

(superchat-bridge-defstruct (superchat-bridge-thinking-content
                (:constructor superchat-bridge-make-thinking-content)
                (:copier superchat-bridge-copy-thinking-content))
  "A `ThinkingContent': a reasoning block."
  (type "thinking")
  thinking
  thinking-signature
  redacted)

(superchat-bridge-defstruct (superchat-bridge-image-content
                (:constructor superchat-bridge-make-image-content)
                (:copier superchat-bridge-copy-image-content))
  "An `ImageContent': base64 image data and its media type."
  (type "image")
  data
  mime-type)

(superchat-bridge-defstruct (superchat-bridge-tool-call
                (:constructor superchat-bridge-make-tool-call)
                (:copier superchat-bridge-copy-tool-call))
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

(superchat-bridge-defstruct (superchat-bridge-cost
                (:constructor superchat-bridge-make-cost)
                (:copier superchat-bridge-copy-cost))
  "The `cost' object of `Usage', in US dollars."
  (input 0)
  (output 0)
  (cache-read 0)
  (cache-write 0)
  (total 0))

(superchat-bridge-defstruct (superchat-bridge-usage
                (:constructor superchat-bridge-make-usage))
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

(superchat-bridge-defstruct (superchat-bridge-message
                (:predicate nil)        ; `superchat-bridge-message-p' is written below
                (:constructor superchat-bridge-make-message)
                (:copier superchat-bridge-copy-message))
  "The fields `SystemMessage', `UserMessage', `AssistantMessage' and
`ToolResultMessage' share.  Compare with `superchat-bridge-user-message-p'
and friends, or with (superchat-bridge-message-role MESSAGE), whose
value names the struct: `system', `user', `assistant' or
`toolResult'.  Do not use this constructor for anything but the
`Message' union in its place in a nodeapi list."
  role
  content
  timestamp)

(superchat-bridge-defstruct (superchat-bridge-system-message
                (:include superchat-bridge-message (role "system"))
                (:constructor superchat-bridge-make-system-message)
                (:copier superchat-bridge-copy-system-message))
  "A `SystemMessage'.
The leading system message is the system prompt; later ones change
it: `content' adds instructions from that point on, `sections'
replace or remove named prompt sections by name (a nil value in the
alist removes one), and `tools-added'/`tools-removed' change the
tool set.  `role' is \"system\"."
  sections
  tools-added
  tools-removed)

(superchat-bridge-defstruct (superchat-bridge-user-message
                (:include superchat-bridge-message (role "user"))
                (:constructor superchat-bridge-make-user-message)
                (:copier superchat-bridge-copy-user-message))
  "A `UserMessage'.  `role' is \"user\".")

(superchat-bridge-defstruct (superchat-bridge-assistant-message
                (:include superchat-bridge-message (role "assistant"))
                (:constructor superchat-bridge-make-assistant-message)
                (:copier superchat-bridge-copy-assistant-message))
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

(superchat-bridge-defstruct (superchat-bridge-tool-result-message
                (:include superchat-bridge-message (role "toolResult"))
                (:constructor superchat-bridge-make-tool-result-message)
                (:copier superchat-bridge-copy-tool-result-message))
  "A `ToolResultMessage'.  `nested-calls' is kept for the session
record and is not sent to the model.  `role' is \"toolResult\"."
  tool-call-id
  tool-name
  details
  usage
  nested-calls
  (is-error nil)
  duration-ms)

(defun superchat-bridge-message-p (object)
  "Return non-nil when OBJECT is any of pi's four message types."
  (or (superchat-bridge-system-message-p object)
      (superchat-bridge-user-message-p object)
      (superchat-bridge-assistant-message-p object)
      (superchat-bridge-tool-result-message-p object)))

(defun superchat-bridge-content-p (object)
  "Return non-nil when OBJECT is a content block or a tool call."
  (or (superchat-bridge-text-content-p object)
      (superchat-bridge-thinking-content-p object)
      (superchat-bridge-image-content-p object)
      (superchat-bridge-tool-call-p object)))

;;; Assistant message events

(superchat-bridge-defstruct (superchat-bridge-assistant-message-event
                (:constructor superchat-bridge-make-event)
                (:copier superchat-bridge-copy-event))
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

(defconst superchat-bridge-stop-reasons
  '("pending" "stop" "length" "toolUse" "error" "aborted" "deferred")
  "The `StopReason' union of src/types.ts.")

(defconst superchat-bridge-usage-stop-reasons '("stop" "length" "toolUse" "deferred")
  "Stop reasons of a `done' event, i.e. every reason but errors.")

(defconst superchat-bridge-error-stop-reasons '("aborted" "error")
  "Stop reasons of an `error' event.")

(defconst superchat-bridge-images-stop-reasons '("stop" "error" "aborted")
  "The `ImagesStopReason' union of src/types.ts.")

;;; Tools and request context

(superchat-bridge-defstruct (superchat-bridge-tool
                (:constructor superchat-bridge-make-tool)
                (:copier superchat-bridge-copy-tool))
  "A `Tool'.  `parameters' is a JSON Schema, held as an alist or plist
as typebox would describe it.  `constrained-sampling' is nil, t or
a description of the provider-side sampling constraint."
  name
  description
  parameters
  constrained-sampling)

(superchat-bridge-defstruct (superchat-bridge-tool-reference
                (:constructor superchat-bridge-make-tool-reference)
                (:copier superchat-bridge-copy-tool-reference))
  "A `ToolReference': names a tool without repeating its definition."
  name)

(superchat-bridge-defstruct (superchat-bridge-context
                (:constructor superchat-bridge-make-context)
                (:copier superchat-bridge-copy-context))
  "A request `Context'.  `system-prompt' and `tools' are shorthand for
a leading system message; `normalize-context' (not yet ported) folds
them into one before a provider sees the request."
  system-prompt
  messages
  tools)

(defun superchat-bridge-transcript-context-p (object)
  "Return non-nil when OBJECT looks like a normalized request context.
TypeScript brands `TranscriptContext' so that only
`normalizeContext()' can produce one.  A Lisp value carries no
brand, so a context whose `system-prompt' is unset is taken as
normalized, which is what `normalizeContext()' guarantees."
  (and (superchat-bridge-context-p object)
       (null (superchat-bridge-context-system-prompt object))
       t))

;;; Deferred responses

(superchat-bridge-defstruct (superchat-bridge-deferred-handle
                (:constructor superchat-bridge-make-deferred-handle)
                (:copier superchat-bridge-copy-deferred-handle))
  "A `DeferredHandle': what to poll to obtain an asynchronous response."
  provider
  model-id
  api
  id
  expires-at
  poll-after-ms
  data)

;;; Images

(superchat-bridge-defstruct (superchat-bridge-images-context
                (:constructor superchat-bridge-make-images-context)
                (:copier superchat-bridge-copy-images-context))
  "An `ImagesContext'."
  input)

(superchat-bridge-defstruct (superchat-bridge-assistant-images
                (:constructor superchat-bridge-make-assistant-images)
                (:copier superchat-bridge-copy-assistant-images))
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

(superchat-bridge-defstruct (superchat-bridge-classifier-context
                (:constructor superchat-bridge-make-classifier-context)
                (:copier superchat-bridge-copy-classifier-context))
  "A `ClassifierContext'.  `questions' maps a name to a question,
which is a plist with at least `type' (`choice', `score' or
`bool'), `instructions' and `criteria'."
  state
  images
  questions)

(superchat-bridge-defstruct (superchat-bridge-classifier-result
                (:constructor superchat-bridge-make-classifier-result)
                (:copier superchat-bridge-copy-classifier-result))
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

(superchat-bridge-defstruct (superchat-bridge-model-cost-rates
                (:constructor superchat-bridge-make-model-cost-rates)
                (:copier superchat-bridge-copy-model-cost-rates))
  "`ModelCostRates': US dollars per million tokens."
  (input 0)
  (output 0)
  (cache-read 0)
  (cache-write 0))

(superchat-bridge-defstruct (superchat-bridge-model-cost-tier
                (:include superchat-bridge-model-cost-rates)
                (:constructor superchat-bridge-make-model-cost-tier)
                (:copier superchat-bridge-copy-model-cost-tier))
  "A `ModelCostTier': rates that apply when the request's input
exceeds `input-tokens-above'."
  (input-tokens-above 0))

(superchat-bridge-defstruct (superchat-bridge-model-cost
                (:include superchat-bridge-model-cost-rates)
                (:constructor superchat-bridge-make-model-cost)
                (:copier superchat-bridge-copy-model-cost))
  "A catalog `ModelCost'.  `tiers' holds `ModelCostTier's; the highest
matching input threshold applies to the whole request."
  tiers)

(superchat-bridge-defstruct (superchat-bridge-model-image-resize-options
                (:constructor superchat-bridge-make-model-image-resize-options)
                (:copier superchat-bridge-copy-model-image-resize-options))
  "A `ModelImageResizeOptions'.  `max-bytes' limits the base64 payload."
  max-width
  max-height
  max-bytes
  jpeg-quality)

(superchat-bridge-defstruct (superchat-bridge-model-image-input-limits
                (:constructor superchat-bridge-make-model-image-input-limits)
                (:copier superchat-bridge-copy-model-image-input-limits))
  "A `ModelImageInputLimits'."
  resize
  max-per-message
  max-per-request)

(superchat-bridge-defstruct (superchat-bridge-model-input-limits
                (:constructor superchat-bridge-make-model-input-limits)
                (:copier superchat-bridge-copy-model-input-limits))
  "`ModelInputLimits': request size and image limits."
  max-request-bytes
  images)

(superchat-bridge-defstruct (superchat-bridge-model
                (:constructor superchat-bridge-make-model)
                (:copier superchat-bridge-copy-model))
  "A catalog chat `Model'.  `type' is `chat' or nil; nil means chat,
as the source makes that the default.  `input' is a list of
`text'/`image' modality strings; `compat' is an alist of the
provider's compatibility overrides.  Use `superchat-bridge-get-model-type'
rather than reading `type' directly.

`superchat-bridge-image-model' and `superchat-bridge-classifier-model' include
this struct, and a slot belongs to the parent that declares it, so
their `type' is this slot.  Build them with
`superchat-bridge-make-image-model' and `superchat-bridge-make-classifier-model',
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

(superchat-bridge-defstruct (superchat-bridge-image-model
                (:include superchat-bridge-model (type 'image))
                (:constructor superchat-bridge-make-image-model)
                (:copier superchat-bridge-copy-image-model))
  "An `ImageModel': usable with image generation only.  `output' always
includes `image'; `text' means the model can also return text.
`type' is `image'."
  output)

(superchat-bridge-defstruct (superchat-bridge-classifier-model
                (:include superchat-bridge-model (type 'classifier))
                (:constructor superchat-bridge-make-classifier-model)
                (:copier superchat-bridge-copy-classifier-model))
  "A `ClassifierModel': usable with classification only.
`type' is `classifier'.")

(defun superchat-bridge-get-model-type (model)
  "Return the `ModelType' of MODEL: `chat', `image' or `classifier'.
A chat model may leave `type' unset; it is then chat."
  (or (superchat-bridge-model-type model) 'chat))

(defun superchat-bridge-is-model-type (model type)
  "Return non-nil when MODEL is of TYPE, one of `chat', `image' or
`classifier'.  This is the `isModelType' predicate the source asks
callers to use instead of comparing `type' themselves."
  (eq (superchat-bridge-get-model-type model) type))

;;; Other option shapes

(superchat-bridge-defstruct (superchat-bridge-provider-response
                (:constructor superchat-bridge-make-provider-response)
                (:copier superchat-bridge-copy-provider-response))
  "A `ProviderResponse' passed to `on-response' callbacks."
  status
  headers)

(superchat-bridge-defstruct (superchat-bridge-thinking-budgets
                (:constructor superchat-bridge-make-thinking-budgets)
                (:copier superchat-bridge-copy-thinking-budgets))
  "`ThinkingBudgets': token budgets per thinking level."
  minimal
  low
  medium
  high)

(defconst superchat-bridge-thinking-levels
  '("minimal" "low" "medium" "high" "xhigh" "max")
  "The `THINKING_LEVELS' of src/providers/model-schema.ts.")

(defconst superchat-bridge-model-thinking-levels
  (cons "off" superchat-bridge-thinking-levels)
  "The `MODEL_THINKING_LEVELS' of src/providers/model-schema.ts.")

(defconst superchat-bridge-model-input-modalities '("text" "image")
  "The `MODEL_INPUT_MODALITIES' of src/providers/model-schema.ts.")

(defconst superchat-bridge-cache-retentions '("none" "short" "long")
  "The `CACHE_RETENTIONS' of src/providers/model-schema.ts.")

(defconst superchat-bridge-model-prompt-cache-retentions '("short" "long")
  "The `MODEL_PROMPT_CACHE_RETENTIONS' of src/providers/model-schema.ts.")

(defconst superchat-bridge-transports '("sse" "websocket" "websocket-cached" "auto")
  "The `Transport' union of src/types.ts.")

(defconst superchat-bridge-tool-choices '("auto" "none")
  "The `ToolChoice' union of src/types.ts.")

(defconst superchat-bridge-grammar-formats '("openai_lark" "openai_regex")
  "The `GrammarFormat' union of src/types.ts.")


(provide 'superchat-bridge-types)

;;; superchat-bridge-types.el ends here
