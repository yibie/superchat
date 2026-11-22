;;; superchat-bridge-provider-anthropic.el --- The Anthropic provider  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/providers/anthropic.ts' and `src/providers/anthropic.models.ts'
;; (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; `superchat-bridge-anthropic-provider' returns the provider; its models
;; are those of `data/anthropic.json'.  Its API key auth
;; (`anthropicApiKeyAuth', `resolve' only) resolves, in order: a stored
;; credential's key; ANTHROPIC_AUTH_TOKEN, as an `Authorization: Bearer'
;; header; ANTHROPIC_OAUTH_TOKEN, then ANTHROPIC_API_KEY, as the API key.
;;
;; The API is `superchat-bridge-lazy-api' over a loader that requires
;; `superchat-bridge-api-anthropic-messages' on the first request, the
;; source's `anthropicMessagesApi()' of `anthropic-messages.lazy.ts'.
;;
;; Left out, whole: the `oauth' member of the auth (`lazyOAuth',
;; Claude Pro/Max login; `src/auth/oauth/' is not ported), `login' of the
;; API key auth, and workload identity federation: the source's resolve
;; ends with it (ANTHROPIC_FEDERATION_RULE_ID, ANTHROPIC_ORGANIZATION_ID,
;; ANTHROPIC_IDENTITY_TOKEN_FILE and the optional ids), which needs the
;; SDK's token exchange; here it resolves to nil (not configured).  A
;; key from ANTHROPIC_OAUTH_TOKEN reaches the API, which ends the
;; request with an error, its OAuth branch not being ported.
;;
;; Of `anthropic.models.ts', `ANTHROPIC_MODELS' is
;; `superchat-bridge-anthropic-models'; the image and classifier catalogs
;; are not ported (the file has none).

;;; Code:

(require 'subr-x)
(require 'superchat-bridge-models)
(require 'superchat-bridge-auth)
(require 'superchat-bridge-model-catalog)
(require 'superchat-bridge-utils-abort)

(declare-function superchat-bridge-api-anthropic-messages-stream
                  "superchat-bridge-api-anthropic-messages" (model context options))
(declare-function superchat-bridge-api-anthropic-messages-stream-simple
                  "superchat-bridge-api-anthropic-messages" (model context options))

(defvar superchat-bridge-anthropic--models nil
  "The models of `data/anthropic.json' once read, an alist ((ID . MODEL) ...).")

(defun superchat-bridge-anthropic-models ()
  "Return the chat models of Anthropic as an alist ((ID . MODEL) ...).
The source's `ANTHROPIC_MODELS'.  The file is read on first use and kept."
  (or superchat-bridge-anthropic--models
      (setq superchat-bridge-anthropic--models
            (superchat-bridge-flatten-chat-model-catalog
             "anthropic" (superchat-bridge-model-catalog-read-provider "anthropic")))))

(defun superchat-bridge-anthropic--resolve (ctx credential signal)
  "Resolve Anthropic auth from CREDENTIAL or CTX's environment.
The `resolve' of the source's `anthropicApiKeyAuth', without federation;
SIGNAL is checked where the source checks it."
  (superchat-bridge-abort-signal-throw-if-aborted signal)
  (let ((key (plist-get credential :key)))
    (if (and key (not (string-empty-p key)))
        (list :auth (list :api-key key)
              :env (plist-get credential :env)
              :source "stored credential")
      (let ((token (superchat-bridge-auth-context-lookup-env
                    ctx superchat-bridge-anthropic-auth-token-env)))
        (superchat-bridge-abort-signal-throw-if-aborted signal)
        (if (and token (not (string-empty-p token)))
            (list :auth (list :headers (list (cons "Authorization"
                                                   (concat "Bearer " token))))
                  :source superchat-bridge-anthropic-auth-token-env)
          (catch 'found
            (dolist (env-var (list superchat-bridge-anthropic-oauth-token-env
                                   superchat-bridge-anthropic-api-key-env))
              (let ((value (superchat-bridge-auth-context-lookup-env ctx env-var)))
                (superchat-bridge-abort-signal-throw-if-aborted signal)
                (when (and value (not (string-empty-p value)))
                  (throw 'found (list :auth (list :api-key value) :source env-var)))))
            nil))))))

(defun superchat-bridge-anthropic--load-api ()
  "Return the `superchat-bridge-provider-streams' of the Anthropic Messages API.
Load `superchat-bridge-api-anthropic-messages' first."
  (require 'superchat-bridge-api-anthropic-messages)
  (superchat-bridge-make-provider-streams
   :stream #'superchat-bridge-api-anthropic-messages-stream
   :stream-simple #'superchat-bridge-api-anthropic-messages-stream-simple))

(defun superchat-bridge-anthropic-messages-api ()
  "Return the lazily loaded Anthropic Messages API, `anthropicMessagesApi()'
of `src/api/anthropic-messages.lazy.ts'."
  (superchat-bridge-lazy-api #'superchat-bridge-anthropic--load-api))

(defun superchat-bridge-anthropic-provider (&optional api)
  "Return the Anthropic provider, the source's `anthropicProvider()'.
API is an `superchat-bridge-provider-streams' to use in place of the
Anthropic Messages API, for tests."
  (superchat-bridge-create-provider
   :id "anthropic"
   :name "Anthropic"
   :base-url "https://api.anthropic.com"
   :auth (superchat-bridge-make-provider-auth
          :api-key (superchat-bridge-make-api-key-auth
                    :name "Anthropic API key"
                    :resolve #'superchat-bridge-anthropic--resolve
                    :env-vars (list superchat-bridge-anthropic-api-key-env
                                    superchat-bridge-anthropic-auth-token-env)))
   :models (mapcar #'cdr (superchat-bridge-anthropic-models))
   :api (or api (superchat-bridge-anthropic-messages-api))))

(provide 'superchat-bridge-provider-anthropic)

;;; superchat-bridge-provider-anthropic.el ends here
