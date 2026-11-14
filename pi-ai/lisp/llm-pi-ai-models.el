;;; llm-pi-ai-models.el --- Providers and the Models collection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/models.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/', with
;; the part of `src/api/lazy.ts' that it needs (see "Lazy streams").
;;
;; Ported: `createProvider', `calculateCost', `getSupportedThinkingLevels',
;; `clampThinkingLevel', `hasApi', `modelsAreEqual', and of `createModels'
;; what a request needs: `setProvider', `deleteProvider', `clearProviders',
;; `getProviders', `getProvider', `getModels', `getModel', `getAuth',
;; `stream' and `streamSimple' (with `applyAuth' and `transformHeaders').
;;
;; Left out: `modelsStore' and the models store, `refresh' and every
;; `refreshModels' / `fetchModels' / publication, `checkAuth',
;; `getAvailable*', `login', `logout', OAuth, `getAllModels',
;; `getModelsOfType', `getModelOfType', `complete', `completeSimple',
;; `streamDeferred', `fetchDeferred', `cancelDeferred', `generateImages',
;; `classify', `filterModels', `filterAllModels', `images' and
;; `classifiers' of `createProvider', telemetry (`observeModelRequest'),
;; the `identity' option of `createModels', and the OpenCode session
;; header that `applyAuth' adds (`src/utils/identity.ts' is not ported;
;; a request's `:identity' option is passed to the API unchanged).
;; Passing a left-out `createProvider' keyword is an error.
;;
;; Representation.
;;   Provider            `llm-pi-ai-provider' struct.
;;   Models collection   `llm-pi-ai-models' struct, from
;;                       `llm-pi-ai-create-models'.
;;   Options             a plist with the source's option names as
;;                       keywords: `:api-key', `:headers', `:env',
;;                       `:signal', `:session-id', `:reasoning',
;;                       `:transform-headers', and any other key an API
;;                       reads.  Unknown keys are passed on unchanged.
;;   Headers             an alist of (NAME . VALUE) strings; a nil VALUE
;;                       is the source's `null', which removes a header.
;;   Env                 an alist of (NAME . VALUE) strings.
;;   Signal              an `llm-pi-ai-abort-signal'.
;;   Model               `llm-pi-ai-model'; see `llm-pi-ai-model-catalog'.
;;
;; Asynchrony.  Source's `stream' returns a stream at once and resolves
;; auth behind it.  Auth resolution reads the environment and memory, so
;; it is synchronous here, and runs inside `llm-pi-ai-lazy-stream''s setup:
;; every failure ends in the returned stream's `error' event, as in the
;; source, and the stream is already complete on return when the API's
;; stream is.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'llm-pi-ai-types)
(require 'llm-pi-ai-event-stream)
(require 'llm-pi-ai-auth)
(require 'llm-pi-ai-utils-abort)
(require 'llm-pi-ai-utils-models-error)
(require 'llm-pi-ai-utils-model-operations)
(require 'llm-pi-ai-utils-transcript)

;;; Lazy streams (src/api/lazy.ts)

;; `src/api/lazy.ts' has no Lisp file of its own yet; `lazyStream' and
;; `lazyApi' are here because `createProvider' and `Models.stream' need
;; them.  They keep the names a file `llm-pi-ai-api-lazy.el' would give
;; them.  `LazyStreamObserver' (telemetry) and the deferred members of
;; `ProviderStreams' / `lazyApi' are left out.

(cl-defstruct (llm-pi-ai-provider-streams
               (:constructor llm-pi-ai-make-provider-streams)
               (:copier nil))
  "Source's `ProviderStreams': the stream functions of an API implementation.
`stream' and `stream-simple' are functions
`(lambda (MODEL CONTEXT OPTIONS) ...)' that return an assistant message
event stream.  MODEL is an `llm-pi-ai-model', CONTEXT a normalized
`llm-pi-ai-context' (see `llm-pi-ai-normalize-context') and OPTIONS a
plist."
  stream
  stream-simple)

(defun llm-pi-ai--now-ms ()
  "Return the current time in milliseconds, like `Date.now()'."
  (floor (* 1000 (float-time))))

(defun llm-pi-ai--setup-error-message (model err timestamp)
  "Return the error message for a setup of MODEL that failed with ERR.
TIMESTAMP is when the setup began, in milliseconds.  This is the
source's `createSetupErrorMessage'."
  (llm-pi-ai-make-assistant-message
   :content nil
   :api (llm-pi-ai-model-api model)
   :provider (llm-pi-ai-model-provider model)
   :model (llm-pi-ai-model-id model)
   :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason "error"
   :error-message (llm-pi-ai-error-message-text err)
   :timestamp timestamp))

(defun llm-pi-ai-lazy-stream (model setup)
  "Return an assistant message event stream for MODEL at once.
SETUP is a function of no arguments that returns the stream to forward,
or signals an error; that error becomes the `error' event of the
returned stream, whose final message has stop reason \"error\" and the
error's text as its error message.  The events and result of the stream
SETUP returns are forwarded, as they are pushed when that stream is
still filling.  This is the source's `lazyStream'; its setup is
asynchronous there and runs to its end here."
  (let ((started-at (llm-pi-ai--now-ms))
        (outer (llm-pi-ai-create-assistant-message-event-stream))
        (inner nil))
    (condition-case err
        (setq inner (funcall setup))
      (error
       (let ((message (llm-pi-ai--setup-error-message model err started-at)))
         (llm-pi-ai-event-stream-push
          outer (llm-pi-ai-make-event :type "error" :reason "error"
                                      :error message))
         (llm-pi-ai-event-stream-end outer message))))
    (when inner
      (let ((result nil))
        (llm-pi-ai-event-stream-iterate
         inner
         (lambda (event)
           (llm-pi-ai-event-stream-push outer event)
           (let ((type (llm-pi-ai-assistant-message-event-type event)))
             (cond ((equal type "done")
                    (setq result (llm-pi-ai-assistant-message-event-message event)))
                   ((equal type "error")
                    (setq result (llm-pi-ai-assistant-message-event-error event))))))
         (lambda ()
           (llm-pi-ai-event-stream-end
            outer (or (llm-pi-ai-event-stream-result inner) result))))))
    outer))

(defun llm-pi-ai-lazy-api (load)
  "Return `ProviderStreams' that call the ones LOAD returns, on first use.
LOAD is a function of no arguments that returns an
`llm-pi-ai-provider-streams'; it runs inside the setup of each stream,
so a failure to load (e.g. `require' of a missing file) ends that
stream with an `error' event.  This is the source's `lazyApi'."
  (llm-pi-ai-make-provider-streams
   :stream
   (lambda (model context options)
     (llm-pi-ai-lazy-stream
      model
      (lambda ()
        (funcall (llm-pi-ai-provider-streams-stream (funcall load))
                 model context options))))
   :stream-simple
   (lambda (model context options)
     (llm-pi-ai-lazy-stream
      model
      (lambda ()
        (funcall (llm-pi-ai-provider-streams-stream-simple (funcall load))
                 model context options))))))

;;; Headers

(defun llm-pi-ai--merge-headers (base override)
  "Return BASE headers with OVERRIDE headers on top.
This is the source's `mergeHeaders'.  Both are header alists or nil.
An OVERRIDE name removes any BASE name
that differs from it only in case, and is then added at the end.
Return nil when both are nil."
  (when (or base override)
    (let ((merged (copy-sequence base)))
      (dolist (entry override)
        (let ((lower (downcase (car entry))))
          (setq merged (cl-remove-if (lambda (existing)
                                       (string= (downcase (car existing)) lower))
                                     merged)))
        (setq merged (append merged (list (cons (car entry) (cdr entry))))))
      merged)))

;;; Providers

(cl-defstruct (llm-pi-ai-provider
               (:constructor llm-pi-ai--make-provider)
               (:copier nil))
  "Source's `Provider': the runtime unit that owns id, auth, models and streams.
`auth' is an `llm-pi-ai-provider-auth'.  `get-models' and
`get-all-models' are functions of no arguments returning the chat
models, and the models of every type.  `stream' and `stream-simple' are
`(lambda (MODEL CONTEXT OPTIONS) ...)' returning an assistant message
event stream.  `refreshModels', `filterModels', `filterAllModels',
`fetchDeferred', `cancelDeferred', `generateImages' and `classify' of
the source are not ported."
  id
  name
  base-url
  headers
  auth
  get-models
  get-all-models
  stream
  stream-simple)

(cl-defun llm-pi-ai-create-provider (&key id name base-url headers auth models api)
  "Build a provider from parts, the source's `createProvider'.
ID is required, NAME defaults to ID, AUTH is an `llm-pi-ai-provider-auth'
and MODELS a list of models of any type (models without a type are chat
models).  API is the chat implementation: one `llm-pi-ai-provider-streams'
for all chat models, or an alist of (API-NAME . PROVIDER-STREAMS) that
dispatches on the model's `api'; a model whose api has no entry gets a
stream whose only event is an `error' (a `stream' ModelsError, `Provider
ID has no API implementation for \"API\"').  Signal an error when API is
absent or an empty alist.  The source's `fetchModels', `filterModels',
`filterAllModels', `images' and `classifiers' are not ported."
  (let* ((single (and (llm-pi-ai-provider-streams-p api) api))
         (by-api (and (not single) api))
         (streams (if single (list single) (mapcar #'cdr by-api))))
    (when (null streams)
      (error "Provider %s: \"api\" is required (images and classifiers are not ported)"
             id))
    (let* ((baseline (copy-sequence models))
           (api-for (lambda (model)
                      (or single (cdr (assoc (llm-pi-ai-model-api model) by-api)))))
           (dispatch
            (lambda (model run)
              (let ((impl (funcall api-for model)))
                (if impl
                    (funcall run impl)
                  (llm-pi-ai-lazy-stream
                   model
                   (lambda ()
                     (llm-pi-ai-signal-models-error
                      'stream
                      (format "Provider %s has no API implementation for \"%s\""
                              id (llm-pi-ai-model-api model))))))))))
      (llm-pi-ai--make-provider
       :id id
       :name (or name id)
       :base-url base-url
       :headers headers
       :auth auth
       :get-models (lambda ()
                     (seq-filter (lambda (model)
                                   (llm-pi-ai-is-model-type model 'chat))
                                 baseline))
       :get-all-models (lambda () baseline)
       :stream (lambda (model context options)
                 (funcall dispatch model
                          (lambda (impl)
                            (funcall (llm-pi-ai-provider-streams-stream impl)
                                     model context options))))
       :stream-simple (lambda (model context options)
                        (funcall dispatch model
                                 (lambda (impl)
                                   (funcall (llm-pi-ai-provider-streams-stream-simple impl)
                                            model context options))))))))

;;; Models collection

(cl-defstruct (llm-pi-ai-models
               (:constructor llm-pi-ai--make-models)
               (:copier nil))
  "Source's `ModelsImpl': providers plus auth application and requests.
`providers' is a list in registration order; `credentials' an
`llm-pi-ai-credential-store'; `auth-context' an `llm-pi-ai-auth-context'."
  (providers nil)
  credentials
  auth-context)

(cl-defun llm-pi-ai-create-models (&key credentials auth-context)
  "Return an empty Models collection, the source's `createModels'.
CREDENTIALS is an `llm-pi-ai-credential-store', by default an in-memory
one, and AUTH-CONTEXT an `llm-pi-ai-auth-context', by default the one
that reads the process environment.  The source's `modelsStore' and
`identity' options are not ported."
  (llm-pi-ai--make-models
   :credentials (or credentials (llm-pi-ai-make-in-memory-credential-store))
   :auth-context (or auth-context (llm-pi-ai-default-provider-auth-context))))

(defun llm-pi-ai-models-set-provider (models provider)
  "Add PROVIDER to MODELS, or replace the one with its id in place.
Provider ids are unique."
  (let ((cell (cl-position (llm-pi-ai-provider-id provider)
                           (llm-pi-ai-models-providers models)
                           :key #'llm-pi-ai-provider-id :test #'equal)))
    (if cell
        (setf (nth cell (llm-pi-ai-models-providers models)) provider)
      (setf (llm-pi-ai-models-providers models)
            (append (llm-pi-ai-models-providers models) (list provider)))))
  nil)

(defun llm-pi-ai-models-delete-provider (models id)
  "Remove the provider with ID from MODELS."
  (setf (llm-pi-ai-models-providers models)
        (cl-remove id (llm-pi-ai-models-providers models)
                   :key #'llm-pi-ai-provider-id :test #'equal))
  nil)

(defun llm-pi-ai-models-clear-providers (models)
  "Remove every provider from MODELS."
  (setf (llm-pi-ai-models-providers models) nil)
  nil)

(defun llm-pi-ai-models-get-providers (models)
  "Return the providers of MODELS in registration order."
  (copy-sequence (llm-pi-ai-models-providers models)))

(defun llm-pi-ai-models-get-provider (models id)
  "Return the provider of MODELS with ID, or nil."
  (cl-find id (llm-pi-ai-models-providers models)
           :key #'llm-pi-ai-provider-id :test #'equal))

(defun llm-pi-ai-models-get-models (models &optional provider)
  "Return the chat models of PROVIDER (an id), or of every provider.
Best-effort like the source: a provider whose `get-models' signals an
error yields no models."
  (if provider
      (let ((entry (llm-pi-ai-models-get-provider models provider)))
        (and entry
             (condition-case nil
                 (funcall (llm-pi-ai-provider-get-models entry))
               (error nil))))
    (let ((all nil))
      (dolist (entry (llm-pi-ai-models-providers models))
        (condition-case nil
            (setq all (append all (funcall (llm-pi-ai-provider-get-models entry))))
          (error nil)))
      all)))

(defun llm-pi-ai-models-get-model (models provider id)
  "Return the chat model ID of PROVIDER (an id) in MODELS, or nil."
  (cl-find id (llm-pi-ai-models-get-models models provider)
           :key #'llm-pi-ai-model-id :test #'equal))

(defun llm-pi-ai-models-get-auth (models provider-or-model &optional overrides)
  "Resolve the auth of a provider, given its id or one of its models.
Return an auth result plist (see `llm-pi-ai-resolve-provider-auth'), or
nil when the provider is unknown or not configured.  Given a model with
`headers', its headers are merged into the result's auth headers.
OVERRIDES is a plist with `:api-key', `:env', `:min-oauth-validity-ms'
and `:signal'.  Signals a `ModelsError' with code `auth' when API key
resolution or the credential store fails."
  (let* ((signal (llm-pi-ai-operation-signal (plist-get overrides :signal)))
         (provider-id (if (stringp provider-or-model)
                          provider-or-model
                        (llm-pi-ai-model-provider provider-or-model)))
         (provider (llm-pi-ai-models-get-provider models provider-id)))
    (when provider
      (let ((result (llm-pi-ai-resolve-provider-auth
                     provider-id (llm-pi-ai-provider-auth provider)
                     (llm-pi-ai-models-credentials models)
                     (llm-pi-ai-models-auth-context models)
                     (plist-put (copy-sequence overrides) :signal signal))))
        (if (or (null result) (stringp provider-or-model)
                (null (llm-pi-ai-model-headers provider-or-model)))
            result
          (let ((auth (copy-sequence (plist-get result :auth))))
            (setq auth (plist-put auth :headers
                                  (llm-pi-ai--merge-headers
                                   (plist-get auth :headers)
                                   (llm-pi-ai-model-headers provider-or-model))))
            (plist-put (copy-sequence result) :auth auth)))))))

(defun llm-pi-ai--require-provider (models model)
  "Return the provider of MODEL in MODELS, or signal `Unknown provider'."
  (or (llm-pi-ai-models-get-provider models (llm-pi-ai-model-provider model))
      (llm-pi-ai-signal-models-error
       'provider (format "Unknown provider: %s" (llm-pi-ai-model-provider model)))))

(defun llm-pi-ai--require-chat-provider (models model)
  "Return the provider of MODEL in MODELS after checking MODEL is a chat model."
  (llm-pi-ai-assert-chat-model model)
  (llm-pi-ai--require-provider models model))

(defun llm-pi-ai--plist-without (plist keys)
  "Return a copy of PLIST without the properties named in KEYS."
  (let ((out nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq out (append out (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    out))

(defun llm-pi-ai--merge-env (base override)
  "Return env alist BASE with OVERRIDE entries on top.
Return nil when both are nil."
  (when (or base override)
    (let ((merged (copy-sequence base)))
      (dolist (entry override)
        (setq merged (append (cl-remove (car entry) merged :key #'car :test #'equal)
                             (list (cons (car entry) (cdr entry))))))
      merged)))

(defun llm-pi-ai--apply-auth (models model options)
  "Resolve auth for MODEL and merge it into OPTIONS, the source's `applyAuth'.
Return (REQUEST-MODEL . REQUEST-OPTIONS).  Explicit request options win
per field; the `:transform-headers' function runs last, once, and is not
passed on.  Signal a `ModelsError' when the provider is unknown or not
configured."
  (llm-pi-ai--require-provider models model)
  (let ((resolution (llm-pi-ai-models-get-auth
                     models model
                     (list :api-key (plist-get options :api-key)
                           :env (plist-get options :env)
                           :signal (plist-get options :signal)))))
    (unless resolution
      (llm-pi-ai-signal-models-error
       'auth (format "Provider is not configured: %s" (llm-pi-ai-model-provider model))))
    (let* ((auth (plist-get resolution :auth))
           (api-key (or (plist-get options :api-key) (plist-get auth :api-key)))
           (request-model (if (plist-get auth :base-url)
                              (let ((copy (copy-sequence model)))
                                (setf (llm-pi-ai-model-base-url copy)
                                      (plist-get auth :base-url))
                                copy)
                            model))
           (headers (llm-pi-ai--merge-headers (plist-get auth :headers)
                                              (plist-get options :headers)))
           (transform (plist-get options :transform-headers)))
      (when transform
        (setq headers (funcall transform headers)))
      (let ((env (llm-pi-ai--merge-env (plist-get resolution :env)
                                       (plist-get options :env))))
        (cons request-model
              (append (list :api-key api-key :headers headers :env env)
                      (llm-pi-ai--plist-without
                       options '(:transform-headers :api-key :headers :env))))))))

(defun llm-pi-ai-models-stream (models model context &optional options)
  "Stream a response of MODEL to CONTEXT through its provider in MODELS.
CONTEXT is an `llm-pi-ai-context'; it is normalized first (see
`llm-pi-ai-normalize-context').  OPTIONS is a plist; see the Commentary
of this file.  Return an assistant message event stream at once.  Auth
is resolved with `llm-pi-ai-models-get-auth' (a stored credential, else
the environment through the auth context) and merged into OPTIONS, then
the provider's `stream' is called with the request model and options.
Every failure after the stream is returned (unknown provider, provider
not configured, auth failure, API signalling an error) ends it with an
`error' event.  This is `Models.stream'."
  (let ((transcript (llm-pi-ai-normalize-context context)))
    (llm-pi-ai-lazy-stream
     model
     (lambda ()
       (let* ((provider (llm-pi-ai--require-chat-provider models model))
              (applied (llm-pi-ai--apply-auth models model options)))
         (funcall (llm-pi-ai-provider-stream provider)
                  (car applied) transcript (cdr applied)))))))

(defun llm-pi-ai-models-stream-simple (models model context &optional options)
  "Like `llm-pi-ai-models-stream', calling the provider's `stream-simple'.
OPTIONS may carry `:reasoning'.  This is `Models.streamSimple'."
  (let ((transcript (llm-pi-ai-normalize-context context)))
    (llm-pi-ai-lazy-stream
     model
     (lambda ()
       (let* ((provider (llm-pi-ai--require-chat-provider models model))
              (applied (llm-pi-ai--apply-auth models model options)))
         (funcall (llm-pi-ai-provider-stream-simple provider)
                  (car applied) transcript (cdr applied)))))))

;;; Model functions

(defun llm-pi-ai-has-api (model api)
  "Return non-nil when MODEL is a chat model whose api is the string API.
Non-chat models never match.  This is the source's `hasApi'."
  (and (llm-pi-ai-is-model-type model 'chat)
       (equal (llm-pi-ai-model-api model) api)))

(defun llm-pi-ai-calculate-cost (model usage)
  "Set the cost of USAGE from the rates of MODEL and return it.
USAGE is an `llm-pi-ai-usage' with a `cost'.  The highest tier of
MODEL's cost whose `input-tokens-above' is below the request's input
tokens (input, cache read and cache write) prices the whole request.
Cache writes of one hour cost twice the input rate.  This is the
source's `calculateCost'."
  (let* ((cost-rates (llm-pi-ai-model-cost model))
         (input-tokens (+ (llm-pi-ai-usage-input usage)
                          (llm-pi-ai-usage-cache-read usage)
                          (llm-pi-ai-usage-cache-write usage)))
         (rates cost-rates)
         (matched-threshold -1))
    (dolist (tier (llm-pi-ai-model-cost-tiers cost-rates))
      (when (and (> input-tokens (llm-pi-ai-model-cost-tier-input-tokens-above tier))
                 (> (llm-pi-ai-model-cost-tier-input-tokens-above tier)
                    matched-threshold))
        (setq rates tier
              matched-threshold
              (llm-pi-ai-model-cost-tier-input-tokens-above tier))))
    (let* ((long-write (or (llm-pi-ai-usage-cache-write1h usage) 0))
           (short-write (- (llm-pi-ai-usage-cache-write usage) long-write))
           (r-input (float (llm-pi-ai-model-cost-rates-input rates)))
           (r-output (float (llm-pi-ai-model-cost-rates-output rates)))
           (r-cache-read (float (llm-pi-ai-model-cost-rates-cache-read rates)))
           (r-cache-write (float (llm-pi-ai-model-cost-rates-cache-write rates)))
           (cost (llm-pi-ai-usage-cost usage)))
      (setf (llm-pi-ai-cost-input cost)
            (* (/ r-input 1000000.0) (llm-pi-ai-usage-input usage)))
      (setf (llm-pi-ai-cost-output cost)
            (* (/ r-output 1000000.0) (llm-pi-ai-usage-output usage)))
      (setf (llm-pi-ai-cost-cache-read cost)
            (* (/ r-cache-read 1000000.0) (llm-pi-ai-usage-cache-read usage)))
      (setf (llm-pi-ai-cost-cache-write cost)
            (/ (+ (* r-cache-write short-write) (* r-input 2 long-write))
               1000000.0))
      (setf (llm-pi-ai-cost-total cost)
            (+ (llm-pi-ai-cost-input cost) (llm-pi-ai-cost-output cost)
               (llm-pi-ai-cost-cache-read cost) (llm-pi-ai-cost-cache-write cost)))
      cost)))

(defun llm-pi-ai-get-supported-thinking-levels (model)
  "Return the thinking levels MODEL supports, as strings in rank order.
A model that does not reason supports only \"off\".  Otherwise a level
is left out when the model's `thinking-level-map' maps it to `:null'
(the source's `null'); \"xhigh\" and \"max\" are also left out unless
the map has an entry for them.  This is the source's
`getSupportedThinkingLevels'."
  (if (not (llm-pi-ai-model-reasoning model))
      (list "off")
    (let ((map (llm-pi-ai-model-thinking-level-map model)))
      (seq-filter
       (lambda (level)
         (let ((entry (assoc level map)))
           (cond ((and entry (eq (cdr entry) :null)) nil)
                 ((member level '("xhigh" "max")) (and entry t))
                 (t t))))
       llm-pi-ai-model-thinking-levels))))

(defun llm-pi-ai-clamp-thinking-level (model level)
  "Return the supported thinking level of MODEL nearest to LEVEL.
Prefer the next higher level, then the next lower one; a LEVEL that is
not a known level gives the first supported one.  This is the source's
`clampThinkingLevel'."
  (let ((available (llm-pi-ai-get-supported-thinking-levels model)))
    (if (member level available)
        level
      (let ((requested (cl-position level llm-pi-ai-model-thinking-levels
                                    :test #'equal)))
        (if (null requested)
            (or (car available) "off")
          (or (cl-find-if (lambda (candidate) (member candidate available))
                          llm-pi-ai-model-thinking-levels :start requested)
              (cl-find-if (lambda (candidate) (member candidate available))
                          llm-pi-ai-model-thinking-levels :end requested
                          :from-end t)
              (car available)
              "off"))))))

(defun llm-pi-ai-models-are-equal (a b)
  "Return non-nil when models A and B have the same type, id and provider.
Nil when either is nil.  This is the source's `modelsAreEqual'."
  (and a b
       (eq (llm-pi-ai-get-model-type a) (llm-pi-ai-get-model-type b))
       (equal (llm-pi-ai-model-id a) (llm-pi-ai-model-id b))
       (equal (llm-pi-ai-model-provider a) (llm-pi-ai-model-provider b))))

(provide 'llm-pi-ai-models)

;;; llm-pi-ai-models.el ends here
