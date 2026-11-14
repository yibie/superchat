;;; superchat-bridge-models.el --- Providers and the Models collection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

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
;;   Provider            `superchat-bridge-provider' struct.
;;   Models collection   `superchat-bridge-models' struct, from
;;                       `superchat-bridge-create-models'.
;;   Options             a plist with the source's option names as
;;                       keywords: `:api-key', `:headers', `:env',
;;                       `:signal', `:session-id', `:reasoning',
;;                       `:transform-headers', and any other key an API
;;                       reads.  Unknown keys are passed on unchanged.
;;   Headers             an alist of (NAME . VALUE) strings; a nil VALUE
;;                       is the source's `null', which removes a header.
;;   Env                 an alist of (NAME . VALUE) strings.
;;   Signal              an `superchat-bridge-abort-signal'.
;;   Model               `superchat-bridge-model'; see `superchat-bridge-model-catalog'.
;;
;; Asynchrony.  Source's `stream' returns a stream at once and resolves
;; auth behind it.  Auth resolution reads the environment and memory, so
;; it is synchronous here, and runs inside `superchat-bridge-lazy-stream''s setup:
;; every failure ends in the returned stream's `error' event, as in the
;; source, and the stream is already complete on return when the API's
;; stream is.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'superchat-bridge-types)
(require 'superchat-bridge-event-stream)
(require 'superchat-bridge-auth)
(require 'superchat-bridge-utils-abort)
(require 'superchat-bridge-utils-models-error)
(require 'superchat-bridge-utils-model-operations)
(require 'superchat-bridge-utils-transcript)

;;; Lazy streams (src/api/lazy.ts)

;; `src/api/lazy.ts' has no Lisp file of its own yet; `lazyStream' and
;; `lazyApi' are here because `createProvider' and `Models.stream' need
;; them.  They keep the names a file `superchat-bridge-api-lazy.el' would give
;; them.  `LazyStreamObserver' (telemetry) and the deferred members of
;; `ProviderStreams' / `lazyApi' are left out.

(cl-defstruct (superchat-bridge-provider-streams
               (:constructor superchat-bridge-make-provider-streams)
               (:copier nil))
  "Source's `ProviderStreams': the stream functions of an API implementation.
`stream' and `stream-simple' are functions
`(lambda (MODEL CONTEXT OPTIONS) ...)' that return an assistant message
event stream.  MODEL is an `superchat-bridge-model', CONTEXT a normalized
`superchat-bridge-context' (see `superchat-bridge-normalize-context') and OPTIONS a
plist."
  stream
  stream-simple)

(defun superchat-bridge--now-ms ()
  "Return the current time in milliseconds, like `Date.now()'."
  (floor (* 1000 (float-time))))

(defun superchat-bridge--setup-error-message (model err timestamp)
  "Return the error message for a setup of MODEL that failed with ERR.
TIMESTAMP is when the setup began, in milliseconds.  This is the
source's `createSetupErrorMessage'."
  (superchat-bridge-make-assistant-message
   :content nil
   :api (superchat-bridge-model-api model)
   :provider (superchat-bridge-model-provider model)
   :model (superchat-bridge-model-id model)
   :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
   :stop-reason "error"
   :error-message (superchat-bridge-error-message-text err)
   :timestamp timestamp))

(defun superchat-bridge-lazy-stream (model setup)
  "Return an assistant message event stream for MODEL at once.
SETUP is a function of no arguments that returns the stream to forward,
or signals an error; that error becomes the `error' event of the
returned stream, whose final message has stop reason \"error\" and the
error's text as its error message.  The events and result of the stream
SETUP returns are forwarded, as they are pushed when that stream is
still filling.  This is the source's `lazyStream'; its setup is
asynchronous there and runs to its end here."
  (let ((started-at (superchat-bridge--now-ms))
        (outer (superchat-bridge-create-assistant-message-event-stream))
        (inner nil))
    (condition-case err
        (setq inner (funcall setup))
      (error
       (let ((message (superchat-bridge--setup-error-message model err started-at)))
         (superchat-bridge-event-stream-push
          outer (superchat-bridge-make-event :type "error" :reason "error"
                                      :error message))
         (superchat-bridge-event-stream-end outer message))))
    (when inner
      (let ((result nil))
        (superchat-bridge-event-stream-iterate
         inner
         (lambda (event)
           (superchat-bridge-event-stream-push outer event)
           (let ((type (superchat-bridge-assistant-message-event-type event)))
             (cond ((equal type "done")
                    (setq result (superchat-bridge-assistant-message-event-message event)))
                   ((equal type "error")
                    (setq result (superchat-bridge-assistant-message-event-error event))))))
         (lambda ()
           (superchat-bridge-event-stream-end
            outer (or (superchat-bridge-event-stream-result inner) result))))))
    outer))

(defun superchat-bridge-lazy-api (load)
  "Return `ProviderStreams' that call the ones LOAD returns, on first use.
LOAD is a function of no arguments that returns an
`superchat-bridge-provider-streams'; it runs inside the setup of each stream,
so a failure to load (e.g. `require' of a missing file) ends that
stream with an `error' event.  This is the source's `lazyApi'."
  (superchat-bridge-make-provider-streams
   :stream
   (lambda (model context options)
     (superchat-bridge-lazy-stream
      model
      (lambda ()
        (funcall (superchat-bridge-provider-streams-stream (funcall load))
                 model context options))))
   :stream-simple
   (lambda (model context options)
     (superchat-bridge-lazy-stream
      model
      (lambda ()
        (funcall (superchat-bridge-provider-streams-stream-simple (funcall load))
                 model context options))))))

;;; Headers

(defun superchat-bridge--merge-headers (base override)
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

(cl-defstruct (superchat-bridge-provider
               (:constructor superchat-bridge--make-provider)
               (:copier nil))
  "Source's `Provider': the runtime unit that owns id, auth, models and streams.
`auth' is an `superchat-bridge-provider-auth'.  `get-models' and
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

(cl-defun superchat-bridge-create-provider (&key id name base-url headers auth models api)
  "Build a provider from parts, the source's `createProvider'.
ID is required, NAME defaults to ID, AUTH is an `superchat-bridge-provider-auth'
and MODELS a list of models of any type (models without a type are chat
models).  API is the chat implementation: one
`superchat-bridge-provider-streams'
for all chat models, or an alist of (API-NAME . PROVIDER-STREAMS) that
dispatches on the model's `api'; a model whose api has no entry gets a
stream whose only event is an `error' (a `stream' ModelsError, `Provider
ID has no API implementation for \"API\"').  Signal an error when API is
absent or an empty alist.  The source's `fetchModels', `filterModels',
`filterAllModels', `images' and `classifiers' are not ported."
  (let* ((single (and (superchat-bridge-provider-streams-p api) api))
         (by-api (and (not single) api))
         (streams (if single (list single) (mapcar #'cdr by-api))))
    (when (null streams)
      (error "Provider %s: \"api\" is required (images and classifiers are not ported)"
             id))
    (let* ((baseline (copy-sequence models))
           (api-for (lambda (model)
                      (or single (cdr (assoc (superchat-bridge-model-api model) by-api)))))
           (dispatch
            (lambda (model run)
              (let ((impl (funcall api-for model)))
                (if impl
                    (funcall run impl)
                  (superchat-bridge-lazy-stream
                   model
                   (lambda ()
                     (superchat-bridge-signal-models-error
                      'stream
                      (format "Provider %s has no API implementation for \"%s\""
                              id (superchat-bridge-model-api model))))))))))
      (superchat-bridge--make-provider
       :id id
       :name (or name id)
       :base-url base-url
       :headers headers
       :auth auth
       :get-models (lambda ()
                     (seq-filter (lambda (model)
                                   (superchat-bridge-is-model-type model 'chat))
                                 baseline))
       :get-all-models (lambda () baseline)
       :stream (lambda (model context options)
                 (funcall dispatch model
                          (lambda (impl)
                            (funcall (superchat-bridge-provider-streams-stream impl)
                                     model context options))))
       :stream-simple (lambda (model context options)
                        (funcall dispatch model
                                 (lambda (impl)
                                   (funcall (superchat-bridge-provider-streams-stream-simple impl)
                                            model context options))))))))

;;; Models collection

(cl-defstruct (superchat-bridge-models
               (:constructor superchat-bridge--make-models)
               (:copier nil))
  "Source's `ModelsImpl': providers plus auth application and requests.
`providers' is a list in registration order; `credentials' an
`superchat-bridge-credential-store'; `auth-context' an `superchat-bridge-auth-context'."
  (providers nil)
  credentials
  auth-context)

(cl-defun superchat-bridge-create-models (&key credentials auth-context)
  "Return an empty Models collection, the source's `createModels'.
CREDENTIALS is an `superchat-bridge-credential-store', by default an in-memory
one, and AUTH-CONTEXT an `superchat-bridge-auth-context', by default the one
that reads the process environment.  The source's `modelsStore' and
`identity' options are not ported."
  (superchat-bridge--make-models
   :credentials (or credentials (superchat-bridge-make-in-memory-credential-store))
   :auth-context (or auth-context (superchat-bridge-default-provider-auth-context))))

(defun superchat-bridge-models-set-provider (models provider)
  "Add PROVIDER to MODELS, or replace the one with its id in place.
Provider ids are unique."
  (let ((cell (cl-position (superchat-bridge-provider-id provider)
                           (superchat-bridge-models-providers models)
                           :key #'superchat-bridge-provider-id :test #'equal)))
    (if cell
        (setf (nth cell (superchat-bridge-models-providers models)) provider)
      (setf (superchat-bridge-models-providers models)
            (append (superchat-bridge-models-providers models) (list provider)))))
  nil)

(defun superchat-bridge-models-delete-provider (models id)
  "Remove the provider with ID from MODELS."
  (setf (superchat-bridge-models-providers models)
        (cl-remove id (superchat-bridge-models-providers models)
                   :key #'superchat-bridge-provider-id :test #'equal))
  nil)

(defun superchat-bridge-models-clear-providers (models)
  "Remove every provider from MODELS."
  (setf (superchat-bridge-models-providers models) nil)
  nil)

(defun superchat-bridge-models-get-providers (models)
  "Return the providers of MODELS in registration order."
  (copy-sequence (superchat-bridge-models-providers models)))

(defun superchat-bridge-models-get-provider (models id)
  "Return the provider of MODELS with ID, or nil."
  (cl-find id (superchat-bridge-models-providers models)
           :key #'superchat-bridge-provider-id :test #'equal))

(defun superchat-bridge-models-get-models (models &optional provider)
  "Return the chat models of PROVIDER (an id), or of every provider.
Best-effort like the source: a provider whose `get-models' signals an
error yields no models."
  (if provider
      (let ((entry (superchat-bridge-models-get-provider models provider)))
        (and entry
             (condition-case nil
                 (funcall (superchat-bridge-provider-get-models entry))
               (error nil))))
    (let ((all nil))
      (dolist (entry (superchat-bridge-models-providers models))
        (condition-case nil
            (setq all (append all (funcall (superchat-bridge-provider-get-models entry))))
          (error nil)))
      all)))

(defun superchat-bridge-models-get-model (models provider id)
  "Return the chat model ID of PROVIDER (an id) in MODELS, or nil."
  (cl-find id (superchat-bridge-models-get-models models provider)
           :key #'superchat-bridge-model-id :test #'equal))

(defun superchat-bridge-models-get-auth (models provider-or-model &optional overrides)
  "Resolve the auth of a provider, given its id or one of its models.
Return an auth result plist (see `superchat-bridge-resolve-provider-auth'), or
nil when the provider is unknown or not configured.  Given a model with
`headers', its headers are merged into the result's auth headers.
OVERRIDES is a plist with `:api-key', `:env', `:min-oauth-validity-ms'
and `:signal'.  Signals a `ModelsError' with code `auth' when API key
resolution or the credential store fails."
  (let* ((signal (superchat-bridge-operation-signal (plist-get overrides :signal)))
         (provider-id (if (stringp provider-or-model)
                          provider-or-model
                        (superchat-bridge-model-provider provider-or-model)))
         (provider (superchat-bridge-models-get-provider models provider-id)))
    (when provider
      (let ((result (superchat-bridge-resolve-provider-auth
                     provider-id (superchat-bridge-provider-auth provider)
                     (superchat-bridge-models-credentials models)
                     (superchat-bridge-models-auth-context models)
                     (plist-put (copy-sequence overrides) :signal signal))))
        (if (or (null result) (stringp provider-or-model)
                (null (superchat-bridge-model-headers provider-or-model)))
            result
          (let ((auth (copy-sequence (plist-get result :auth))))
            (setq auth (plist-put auth :headers
                                  (superchat-bridge--merge-headers
                                   (plist-get auth :headers)
                                   (superchat-bridge-model-headers provider-or-model))))
            (plist-put (copy-sequence result) :auth auth)))))))

(defun superchat-bridge--require-provider (models model)
  "Return the provider of MODEL in MODELS, or signal `Unknown provider'."
  (or (superchat-bridge-models-get-provider models (superchat-bridge-model-provider model))
      (superchat-bridge-signal-models-error
       'provider (format "Unknown provider: %s" (superchat-bridge-model-provider model)))))

(defun superchat-bridge--require-chat-provider (models model)
  "Return the provider of MODEL in MODELS after checking MODEL is a chat model."
  (superchat-bridge-assert-chat-model model)
  (superchat-bridge--require-provider models model))

(defun superchat-bridge--plist-without (plist keys)
  "Return a copy of PLIST without the properties named in KEYS."
  (let ((out nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq out (append out (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    out))

(defun superchat-bridge--merge-env (base override)
  "Return env alist BASE with OVERRIDE entries on top.
Return nil when both are nil."
  (when (or base override)
    (let ((merged (copy-sequence base)))
      (dolist (entry override)
        (setq merged (append (cl-remove (car entry) merged :key #'car :test #'equal)
                             (list (cons (car entry) (cdr entry))))))
      merged)))

(defun superchat-bridge--apply-auth (models model options)
  "Resolve auth for MODEL and merge it into OPTIONS, the source's `applyAuth'.
Return (REQUEST-MODEL . REQUEST-OPTIONS).  Explicit request options win
per field; the `:transform-headers' function runs last, once, and is not
passed on.  Signal a `ModelsError' when the provider is unknown or not
configured."
  (superchat-bridge--require-provider models model)
  (let ((resolution (superchat-bridge-models-get-auth
                     models model
                     (list :api-key (plist-get options :api-key)
                           :env (plist-get options :env)
                           :signal (plist-get options :signal)))))
    (unless resolution
      (superchat-bridge-signal-models-error
       'auth (format "Provider is not configured: %s" (superchat-bridge-model-provider model))))
    (let* ((auth (plist-get resolution :auth))
           (api-key (or (plist-get options :api-key) (plist-get auth :api-key)))
           (request-model (if (plist-get auth :base-url)
                              (let ((copy (copy-sequence model)))
                                (setf (superchat-bridge-model-base-url copy)
                                      (plist-get auth :base-url))
                                copy)
                            model))
           (headers (superchat-bridge--merge-headers (plist-get auth :headers)
                                              (plist-get options :headers)))
           (transform (plist-get options :transform-headers)))
      (when transform
        (setq headers (funcall transform headers)))
      (let ((env (superchat-bridge--merge-env (plist-get resolution :env)
                                       (plist-get options :env))))
        (cons request-model
              (append (list :api-key api-key :headers headers :env env)
                      (superchat-bridge--plist-without
                       options '(:transform-headers :api-key :headers :env))))))))

(defun superchat-bridge-models-stream (models model context &optional options)
  "Stream a response of MODEL to CONTEXT through its provider in MODELS.
CONTEXT is an `superchat-bridge-context'; it is normalized first (see
`superchat-bridge-normalize-context').  OPTIONS is a plist; see the Commentary
of this file.  Return an assistant message event stream at once.  Auth
is resolved with `superchat-bridge-models-get-auth' (a stored credential, else
the environment through the auth context) and merged into OPTIONS, then
the provider's `stream' is called with the request model and options.
Every failure after the stream is returned (unknown provider, provider
not configured, auth failure, API signalling an error) ends it with an
`error' event.  This is `Models.stream'."
  (let ((transcript (superchat-bridge-normalize-context context)))
    (superchat-bridge-lazy-stream
     model
     (lambda ()
       (let* ((provider (superchat-bridge--require-chat-provider models model))
              (applied (superchat-bridge--apply-auth models model options)))
         (funcall (superchat-bridge-provider-stream provider)
                  (car applied) transcript (cdr applied)))))))

(defun superchat-bridge-models-stream-simple (models model context &optional options)
  "Like `superchat-bridge-models-stream', calling the provider's `stream-simple'.
OPTIONS may carry `:reasoning'.  This is `Models.streamSimple'."
  (let ((transcript (superchat-bridge-normalize-context context)))
    (superchat-bridge-lazy-stream
     model
     (lambda ()
       (let* ((provider (superchat-bridge--require-chat-provider models model))
              (applied (superchat-bridge--apply-auth models model options)))
         (funcall (superchat-bridge-provider-stream-simple provider)
                  (car applied) transcript (cdr applied)))))))

;;; Model functions

(defun superchat-bridge-has-api (model api)
  "Return non-nil when MODEL is a chat model whose api is the string API.
Non-chat models never match.  This is the source's `hasApi'."
  (and (superchat-bridge-is-model-type model 'chat)
       (equal (superchat-bridge-model-api model) api)))

(defun superchat-bridge-calculate-cost (model usage)
  "Set the cost of USAGE from the rates of MODEL and return it.
USAGE is an `superchat-bridge-usage' with a `cost'.  The highest tier of
MODEL's cost whose `input-tokens-above' is below the request's input
tokens (input, cache read and cache write) prices the whole request.
Cache writes of one hour cost twice the input rate.  This is the
source's `calculateCost'."
  (let* ((cost-rates (superchat-bridge-model-cost model))
         (input-tokens (+ (superchat-bridge-usage-input usage)
                          (superchat-bridge-usage-cache-read usage)
                          (superchat-bridge-usage-cache-write usage)))
         (rates cost-rates)
         (matched-threshold -1))
    (dolist (tier (superchat-bridge-model-cost-tiers cost-rates))
      (when (and (> input-tokens (superchat-bridge-model-cost-tier-input-tokens-above tier))
                 (> (superchat-bridge-model-cost-tier-input-tokens-above tier)
                    matched-threshold))
        (setq rates tier
              matched-threshold
              (superchat-bridge-model-cost-tier-input-tokens-above tier))))
    (let* ((long-write (or (superchat-bridge-usage-cache-write1h usage) 0))
           (short-write (- (superchat-bridge-usage-cache-write usage) long-write))
           (r-input (float (superchat-bridge-model-cost-rates-input rates)))
           (r-output (float (superchat-bridge-model-cost-rates-output rates)))
           (r-cache-read (float (superchat-bridge-model-cost-rates-cache-read rates)))
           (r-cache-write (float (superchat-bridge-model-cost-rates-cache-write rates)))
           (cost (superchat-bridge-usage-cost usage)))
      (setf (superchat-bridge-cost-input cost)
            (* (/ r-input 1000000.0) (superchat-bridge-usage-input usage)))
      (setf (superchat-bridge-cost-output cost)
            (* (/ r-output 1000000.0) (superchat-bridge-usage-output usage)))
      (setf (superchat-bridge-cost-cache-read cost)
            (* (/ r-cache-read 1000000.0) (superchat-bridge-usage-cache-read usage)))
      (setf (superchat-bridge-cost-cache-write cost)
            (/ (+ (* r-cache-write short-write) (* r-input 2 long-write))
               1000000.0))
      (setf (superchat-bridge-cost-total cost)
            (+ (superchat-bridge-cost-input cost) (superchat-bridge-cost-output cost)
               (superchat-bridge-cost-cache-read cost) (superchat-bridge-cost-cache-write cost)))
      cost)))

(defun superchat-bridge-get-supported-thinking-levels (model)
  "Return the thinking levels MODEL supports, as strings in rank order.
A model that does not reason supports only \"off\".  Otherwise a level
is left out when the model's `thinking-level-map' maps it to `:null'
(the source's `null'); \"xhigh\" and \"max\" are also left out unless
the map has an entry for them.  This is the source's
`getSupportedThinkingLevels'."
  (if (not (superchat-bridge-model-reasoning model))
      (list "off")
    (let ((map (superchat-bridge-model-thinking-level-map model)))
      (seq-filter
       (lambda (level)
         (let ((entry (assoc level map)))
           (cond ((and entry (eq (cdr entry) :null)) nil)
                 ((member level '("xhigh" "max")) (and entry t))
                 (t t))))
       superchat-bridge-model-thinking-levels))))

(defun superchat-bridge-clamp-thinking-level (model level)
  "Return the supported thinking level of MODEL nearest to LEVEL.
Prefer the next higher level, then the next lower one; a LEVEL that is
not a known level gives the first supported one.  This is the source's
`clampThinkingLevel'."
  (let ((available (superchat-bridge-get-supported-thinking-levels model)))
    (if (member level available)
        level
      (let ((requested (cl-position level superchat-bridge-model-thinking-levels
                                    :test #'equal)))
        (if (null requested)
            (or (car available) "off")
          (or (cl-find-if (lambda (candidate) (member candidate available))
                          superchat-bridge-model-thinking-levels :start requested)
              (cl-find-if (lambda (candidate) (member candidate available))
                          superchat-bridge-model-thinking-levels :end requested
                          :from-end t)
              (car available)
              "off"))))))

(defun superchat-bridge-models-are-equal (a b)
  "Return non-nil when models A and B have the same type, id and provider.
Nil when either is nil.  This is the source's `modelsAreEqual'."
  (and a b
       (eq (superchat-bridge-get-model-type a) (superchat-bridge-get-model-type b))
       (equal (superchat-bridge-model-id a) (superchat-bridge-model-id b))
       (equal (superchat-bridge-model-provider a) (superchat-bridge-model-provider b))))

(provide 'superchat-bridge-models)

;;; superchat-bridge-models.el ends here
