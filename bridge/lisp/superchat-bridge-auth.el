;;; superchat-bridge-auth.el --- Auth resolution for pi-ai providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of, below `packages/ai/' of @earendil-works/pi-ai@1.4.0 (commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8):
;;
;;   `src/auth/types.ts'      the structs below, as far as API keys need
;;   `src/auth/context.ts'    `superchat-bridge-default-provider-auth-context'
;;   `src/auth/helpers.ts'    `envApiKeyAuth', its `resolve' only
;;   `src/auth/resolve.ts'    `resolveProviderAuth', API key paths only
;;   `src/env-api-keys.ts'    `findEnvKeys' and `getEnvApiKey'
;;   `src/auth/credential-store.ts'  an in-memory store, without the
;;                            per-provider promise chain (nothing here
;;                            runs concurrently)
;;
;; Left out: OAuth whole (`OAuthAuth', `OAuthCredential', `lazyOAuth',
;; `refreshStoredOAuthCredential', `resolveStoredOAuth', the `oauth'
;; member of `ProviderAuth', `minOAuthValidityMs' as anything but an
;; accepted key), `login' and `check' of `ApiKeyAuth', `AuthPrompt',
;; `AuthEvent', `AuthInteraction', `LoginOptions', `AuthCheck',
;; `CredentialInfo' as a type, the Bun `/proc/self/environ' fallback,
;; and the cache of the Google ADC file check.  A stored OAuth
;; credential makes `superchat-bridge-resolve-provider-auth' return nil, as the
;; source does for a provider with no OAuth handler.
;;
;; Representation.  Everything the source types as an object of plain
;; fields is a plist with the source's names as keywords:
;;
;;   ModelAuth     (:api-key STRING :headers HEADERS :base-url STRING)
;;   AuthResult    (:auth MODEL-AUTH :env ENV :source STRING)
;;   Credential    (:type api_key :key STRING :env ENV)
;;   Overrides     (:api-key STRING :env ENV :min-oauth-validity-ms N
;;                  :signal ABORT-SIGNAL)
;;
;; HEADERS is an alist (NAME . VALUE-or-nil), ENV an alist
;; (NAME . VALUE); see `superchat-bridge-models'.
;;
;; Asynchrony.  The source's `async' resolve functions run synchronously
;; here, since they read the environment and memory.  An error signalled
;; in place of a promise rejection; the abort signal is checked where the
;; source checks it, and an abort wins over any other outcome (see
;; `superchat-bridge-race-with-abort-signal').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'superchat-bridge-utils-abort)
(require 'superchat-bridge-utils-models-error)
(require 'superchat-bridge-utils-provider-env)

;;; Auth context (src/auth/types.ts `AuthContext', src/auth/context.ts)

(cl-defstruct (superchat-bridge-auth-context
               (:constructor superchat-bridge-make-auth-context)
               (:copier nil))
  "Source's `AuthContext': environment access for auth resolution.
`env' is a function `(lambda (NAME))' returning the value of the
variable NAME or nil.  `file-exists' is `(lambda (PATH))', true when the
file exists; it supports a leading `~'."
  env
  file-exists)

(defun superchat-bridge-default-provider-auth-context ()
  "Return the default auth context, the source's `defaultProviderAuthContext'.
Its `env' reads `process-environment' and returns nil for a variable that
is unset or only whitespace; its `file-exists' expands a leading `~'."
  (superchat-bridge-make-auth-context
   :env (lambda (name)
          (let ((value (getenv name)))
            (and (stringp value) (> (length (string-trim value)) 0) value)))
   :file-exists (lambda (path)
                  (let ((resolved (if (string-prefix-p "~" path)
                                      (concat (expand-file-name "~")
                                              (substring path 1))
                                    path)))
                    (and (file-exists-p resolved) t)))))

(defun superchat-bridge-auth-context-lookup-env (context name)
  "Return the value of NAME in auth CONTEXT, or nil."
  (funcall (superchat-bridge-auth-context-env context) name))

(defun superchat-bridge--overlay-env-auth-context (base env)
  "Return an auth context that reads ENV before the auth context BASE.
This is the source's `overlayEnvAuthContext'; an empty value in ENV
falls through to BASE."
  (superchat-bridge-make-auth-context
   :env (lambda (name)
          (let ((value (cdr (assoc name env))))
            (if (and value (not (string-empty-p value)))
                value
              (funcall (superchat-bridge-auth-context-env base) name))))
   :file-exists (lambda (path)
                  (funcall (superchat-bridge-auth-context-file-exists base) path))))

;;; Auth types (src/auth/types.ts)

(cl-defstruct (superchat-bridge-api-key-auth
               (:constructor superchat-bridge-make-api-key-auth)
               (:copier nil))
  "Source's `ApiKeyAuth' without `login' and `check'.
`name' is a display name.  `resolve' is a function
`(lambda (CTX CREDENTIAL SIGNAL))' with CTX an `superchat-bridge-auth-context',
CREDENTIAL the stored credential plist or nil and SIGNAL an abort
signal; it returns an auth result plist, or nil for \"not configured\",
or signals an error."
  name
  resolve)

(cl-defstruct (superchat-bridge-provider-auth
               (:constructor superchat-bridge-make-provider-auth)
               (:copier nil))
  "Source's `ProviderAuth' without `oauth': `api-key' is an
`superchat-bridge-api-key-auth'."
  api-key)

;;; Credential store (src/auth/types.ts `CredentialStore', credential-store.ts)

(cl-defstruct (superchat-bridge-credential-store
               (:constructor superchat-bridge-make-credential-store)
               (:copier nil))
  "Source's `CredentialStore': app-owned credentials, one per provider id.
The four slots are functions: `read' is `(lambda (PROVIDER-ID OPTIONS))'
returning a credential plist or nil; `list' is `(lambda (OPTIONS))'
returning plists `(:provider-id ID :type TYPE)'; `modify' is `(lambda
(PROVIDER-ID FN OPTIONS))', which calls FN with the current credential
and stores what it returns unless nil, returning the stored or current
credential; `delete' is `(lambda (PROVIDER-ID OPTIONS))'.  OPTIONS is a
plist with `:signal'.  They signal an error on storage failure."
  read
  list
  modify
  delete)

(defun superchat-bridge-make-in-memory-credential-store ()
  "Return an in-memory credential store, the source's `InMemoryCredentialStore'.
Without the source's per-provider promise chain: operations run to
their end one after another."
  (let ((entries nil))                  ; alist (PROVIDER-ID . CREDENTIAL)
    (superchat-bridge-make-credential-store
     :read (lambda (provider-id options)
             (when-let* ((signal (plist-get options :signal)))
               (superchat-bridge-abort-signal-throw-if-aborted signal))
             (cdr (assoc provider-id entries)))
     :list (lambda (options)
             (when-let* ((signal (plist-get options :signal)))
               (superchat-bridge-abort-signal-throw-if-aborted signal))
             (mapcar (lambda (entry)
                       (list :provider-id (car entry)
                             :type (plist-get (cdr entry) :type)))
                     entries))
     :modify (lambda (provider-id fn options)
               (let ((signal (plist-get options :signal)))
                 (when signal (superchat-bridge-abort-signal-throw-if-aborted signal))
                 (let* ((current (cdr (assoc provider-id entries)))
                        (next (funcall fn current)))
                   (when signal (superchat-bridge-abort-signal-throw-if-aborted signal))
                   (when next
                     (if (assoc provider-id entries)
                         (setcdr (assoc provider-id entries) next)
                       (setq entries (append entries (list (cons provider-id next))))))
                   (or next current))))
     :delete (lambda (provider-id options)
               (let ((signal (plist-get options :signal)))
                 (when signal (superchat-bridge-abort-signal-throw-if-aborted signal))
                 (setq entries (cl-remove provider-id entries
                                          :key #'car :test #'equal))
                 nil)))))

;;; envApiKeyAuth (src/auth/helpers.ts)

(defun superchat-bridge-env-api-key-auth (name env-vars)
  "Return standard API key auth called NAME reading ENV-VARS.
A stored credential's key wins; otherwise the first set variable of
ENV-VARS, a list of names, resolves.  This is the source's
`envApiKeyAuth' without its `login'."
  (superchat-bridge-make-api-key-auth
   :name name
   :resolve
   (lambda (ctx credential signal)
     (superchat-bridge-abort-signal-throw-if-aborted signal)
     (let ((key (plist-get credential :key)))
       (if (and key (not (string-empty-p key)))
           (list :auth (list :api-key key)
                 :env (plist-get credential :env)
                 :source "stored credential")
         (catch 'found
           (dolist (env-var env-vars)
             (let ((value (superchat-bridge-auth-context-lookup-env ctx env-var)))
               (superchat-bridge-abort-signal-throw-if-aborted signal)
               (when (and value (not (string-empty-p value)))
                 (throw 'found (list :auth (list :api-key value)
                                     :source env-var)))))
           nil))))))

;;; resolveProviderAuth (src/auth/resolve.ts)

(defun superchat-bridge--resolve-api-key (auth-context api-key provider-id credential signal)
  "Resolve with API-KEY auth, the source's `resolveApiKey'.
Any error AUTH-CONTEXT's resolution signals becomes a `ModelsError' with
code `auth'."
  (condition-case err
      (funcall (superchat-bridge-api-key-auth-resolve api-key)
               auth-context credential signal)
    (error
     (superchat-bridge-signal-models-error
      'auth (format "API key auth failed for provider %s" provider-id) err))))

(defun superchat-bridge--read-credential (credentials provider-id signal)
  "Read the credential of PROVIDER-ID from CREDENTIALS.
This is the source's `readCredential'.  Any error becomes a `ModelsError'
with code `auth'."
  (condition-case err
      (funcall (superchat-bridge-credential-store-read credentials)
               provider-id (list :signal signal))
    (error
     (superchat-bridge-signal-models-error
      'auth (format "Credential store read failed for %s" provider-id) err))))

(defun superchat-bridge-resolve-provider-auth (provider-id provider-auth credentials
                                                    auth-context &optional overrides)
  "Resolve the auth of the provider PROVIDER-ID, whose auth is PROVIDER-AUTH.
PROVIDER-AUTH is an `superchat-bridge-provider-auth', CREDENTIALS an
`superchat-bridge-credential-store', AUTH-CONTEXT an
`superchat-bridge-auth-context'
and OVERRIDES a plist `(:api-key :env :min-oauth-validity-ms :signal)'.
Return an auth result plist, or nil when the provider is not configured.
Signal a `ModelsError' with code `auth' when API key resolution or the
credential store fails; an aborted `:signal' signals its reason.

The order is the source's: an explicit `:api-key' override is used first;
else a stored credential owns the provider, and the environment is not
consulted, even when the stored credential is of a type this provider
cannot use (that gives nil); only when nothing is stored does the
auth context resolve.  An `:env' override is read before the auth
context, and is merged over a stored credential's env.  This is the
source's `resolveProviderAuth'."
  (let ((signal (superchat-bridge-operation-signal (plist-get overrides :signal)))
        (api-key-auth (superchat-bridge-provider-auth-api-key provider-auth)))
    (superchat-bridge-race-with-abort-signal
     (lambda ()
       (superchat-bridge-abort-signal-throw-if-aborted signal)
       (let* ((override-env (plist-get overrides :env))
              (override-key (plist-get overrides :api-key))
              (request-context (if override-env
                                   (superchat-bridge--overlay-env-auth-context
                                    auth-context override-env)
                                 auth-context)))
         (if (and override-key api-key-auth)
             (superchat-bridge--resolve-api-key
              request-context api-key-auth provider-id
              (list :type 'api_key :key override-key :env override-env)
              signal)
           (let ((stored (superchat-bridge--read-credential credentials provider-id signal)))
             (cond
              ((and stored (eq (plist-get stored :type) 'api_key) api-key-auth)
               (let ((credential
                      (if override-env
                          (plist-put (copy-sequence stored) :env
                                     (superchat-bridge--merge-env-alists
                                      (plist-get stored :env) override-env))
                        stored)))
                 (superchat-bridge--resolve-api-key request-context api-key-auth
                                             provider-id credential signal)))
              (stored nil)
              ;; Ambient (env vars).
              (api-key-auth
               (superchat-bridge--resolve-api-key request-context api-key-auth
                                           provider-id nil signal))
              (t nil))))))
     signal)))

(defun superchat-bridge--merge-env-alists (base override)
  "Return env alist BASE with OVERRIDE entries replacing equal names."
  (let ((merged (copy-sequence base)))
    (dolist (entry override)
      (setq merged (append (cl-remove (car entry) merged :key #'car :test #'equal)
                           (list (cons (car entry) (cdr entry))))))
    merged))

;;; Environment API keys (src/env-api-keys.ts)

(defconst superchat-bridge-anthropic-auth-token-env "ANTHROPIC_AUTH_TOKEN"
  "The source's `ANTHROPIC_AUTH_TOKEN_ENV'.")
(defconst superchat-bridge-anthropic-oauth-token-env "ANTHROPIC_OAUTH_TOKEN"
  "The source's `ANTHROPIC_OAUTH_TOKEN_ENV'.")
(defconst superchat-bridge-anthropic-api-key-env "ANTHROPIC_API_KEY"
  "The source's `ANTHROPIC_API_KEY_ENV'.")
(defconst superchat-bridge-anthropic-federation-rule-id-env "ANTHROPIC_FEDERATION_RULE_ID"
  "The source's `ANTHROPIC_FEDERATION_RULE_ID_ENV'.")
(defconst superchat-bridge-anthropic-organization-id-env "ANTHROPIC_ORGANIZATION_ID"
  "The source's `ANTHROPIC_ORGANIZATION_ID_ENV'.")
(defconst superchat-bridge-anthropic-service-account-id-env "ANTHROPIC_SERVICE_ACCOUNT_ID"
  "The source's `ANTHROPIC_SERVICE_ACCOUNT_ID_ENV'.")
(defconst superchat-bridge-anthropic-identity-token-file-env "ANTHROPIC_IDENTITY_TOKEN_FILE"
  "The source's `ANTHROPIC_IDENTITY_TOKEN_FILE_ENV'.")
(defconst superchat-bridge-anthropic-workspace-id-env "ANTHROPIC_WORKSPACE_ID"
  "The source's `ANTHROPIC_WORKSPACE_ID_ENV'.")

(defconst superchat-bridge--api-key-env-map
  '(("ant-ling" . "ANT_LING_API_KEY")
    ("qwen-token-plan" . "QWEN_TOKEN_PLAN_API_KEY")
    ("qwen-token-plan-cn" . "QWEN_TOKEN_PLAN_CN_API_KEY")
    ("qwen-token-plan-individual" . "QWEN_TOKEN_PLAN_API_KEY")
    ("openai" . "OPENAI_API_KEY")
    ("azure" . "AZURE_OPENAI_API_KEY")
    ("nvidia" . "NVIDIA_API_KEY")
    ("deepseek" . "DEEPSEEK_API_KEY")
    ("google" . "GEMINI_API_KEY")
    ("google-vertex" . "GOOGLE_CLOUD_API_KEY")
    ("groq" . "GROQ_API_KEY")
    ("cerebras" . "CEREBRAS_API_KEY")
    ("xai" . "XAI_API_KEY")
    ("typesafe" . "TYPESAFE_API_KEY")
    ("radius" . "RADIUS_API_KEY")
    ("openrouter" . "OPENROUTER_API_KEY")
    ("vercel-ai-gateway" . "AI_GATEWAY_API_KEY")
    ("zai" . "ZAI_API_KEY")
    ("zai-coding-cn" . "ZAI_CODING_CN_API_KEY")
    ("mistral" . "MISTRAL_API_KEY")
    ("minimax" . "MINIMAX_API_KEY")
    ("minimax-cn" . "MINIMAX_CN_API_KEY")
    ("moonshotai" . "MOONSHOT_API_KEY")
    ("moonshotai-cn" . "MOONSHOT_API_KEY")
    ("huggingface" . "HF_TOKEN")
    ("fireworks" . "FIREWORKS_API_KEY")
    ("together" . "TOGETHER_API_KEY")
    ("baseten" . "BASETEN_API_KEY")
    ("opencode" . "OPENCODE_API_KEY")
    ("opencode-go" . "OPENCODE_API_KEY")
    ("kimi-coding" . "KIMI_API_KEY")
    ("meta" . "META_API_KEY")
    ("cloudflare-workers-ai" . "CLOUDFLARE_API_KEY")
    ("cloudflare-ai-gateway" . "CLOUDFLARE_API_KEY")
    ("xiaomi" . "XIAOMI_API_KEY")
    ("xiaomi-token-plan-cn" . "XIAOMI_TOKEN_PLAN_CN_API_KEY")
    ("xiaomi-token-plan-ams" . "XIAOMI_TOKEN_PLAN_AMS_API_KEY")
    ("xiaomi-token-plan-sgp" . "XIAOMI_TOKEN_PLAN_SGP_API_KEY"))
  "The `envMap' of `getApiKeyEnvVars' in src/env-api-keys.ts.")

(defun superchat-bridge--api-key-env-vars (provider)
  "Return the list of API key variables of PROVIDER, a string, or nil.
This is the source's `getApiKeyEnvVars'."
  (cond ((equal provider "github-copilot") (list "COPILOT_GITHUB_TOKEN"))
        ;; ANTHROPIC_AUTH_TOKEN participates in env discovery/status, but
        ;; `superchat-bridge-get-env-api-key' skips it because requests must pass
        ;; it as Authorization: Bearer.
        ((equal provider "anthropic")
         (list superchat-bridge-anthropic-auth-token-env
               superchat-bridge-anthropic-oauth-token-env
               superchat-bridge-anthropic-api-key-env))
        (t (let ((entry (assoc provider superchat-bridge--api-key-env-map)))
             (and entry (list (cdr entry)))))))

(defun superchat-bridge--has-vertex-adc-credentials-p (env)
  "Return non-nil when Google Application Default Credentials exist.
ENV is a `ProviderEnv' alist.  This is the source's
`hasVertexAdcCredentials' without its result cache."
  (let ((explicit (cdr (assoc "GOOGLE_APPLICATION_CREDENTIALS" env))))
    (if (and explicit (not (string-empty-p explicit)))
        (and (file-exists-p explicit) t)
      (let ((path (or (superchat-bridge-get-provider-env-value
                       "GOOGLE_APPLICATION_CREDENTIALS" env)
                      (expand-file-name
                       "~/.config/gcloud/application_default_credentials.json"))))
        (and (file-exists-p path) t)))))

(defun superchat-bridge-find-env-keys (provider &optional env)
  "Return the API key variables of PROVIDER that are set, or nil.
ENV is a `ProviderEnv' alist read before the process environment.  This
only reports actual API key variables, not ambient credential sources.
This is the source's `findEnvKeys'."
  (let ((env-vars (superchat-bridge--api-key-env-vars provider)))
    (when env-vars
      (let ((found (seq-filter (lambda (env-var)
                                 (superchat-bridge-get-provider-env-value env-var env))
                               env-vars)))
        found))))

(defun superchat-bridge-get-env-api-key (provider &optional env)
  "Return the API key of PROVIDER from known environment variables, or nil.
ENV is a `ProviderEnv' alist read before the process environment.  The
ambient credentials of Google Vertex and Amazon Bedrock give the string
\"<authenticated>\".  Providers that require OAuth tokens give nil.
This is the source's `getEnvApiKey'."
  (let* ((env-keys (superchat-bridge-find-env-keys provider env))
         (api-key-env (and (car env-keys)
                           (if (equal provider "anthropic")
                               (seq-find (lambda (key)
                                           (not (equal key superchat-bridge-anthropic-auth-token-env)))
                                         env-keys)
                             (car env-keys)))))
    (cond
     (api-key-env (superchat-bridge-get-provider-env-value api-key-env env))
     ;; Vertex AI supports either an explicit API key or Application
     ;; Default Credentials (`gcloud auth application-default login').
     ((and (equal provider "google-vertex")
           (superchat-bridge--has-vertex-adc-credentials-p env)
           (or (superchat-bridge-get-provider-env-value "GOOGLE_CLOUD_PROJECT" env)
               (superchat-bridge-get-provider-env-value "GCLOUD_PROJECT" env))
           (superchat-bridge-get-provider-env-value "GOOGLE_CLOUD_LOCATION" env))
      "<authenticated>")
     ;; Amazon Bedrock: profile, IAM keys, bearer token, ECS task roles, IRSA.
     ((and (equal provider "amazon-bedrock")
           (or (superchat-bridge-get-provider-env-value "AWS_PROFILE" env)
               (and (superchat-bridge-get-provider-env-value "AWS_ACCESS_KEY_ID" env)
                    (superchat-bridge-get-provider-env-value "AWS_SECRET_ACCESS_KEY" env))
               (superchat-bridge-get-provider-env-value "AWS_BEARER_TOKEN_BEDROCK" env)
               (superchat-bridge-get-provider-env-value "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" env)
               (superchat-bridge-get-provider-env-value "AWS_CONTAINER_CREDENTIALS_FULL_URI" env)
               (superchat-bridge-get-provider-env-value "AWS_WEB_IDENTITY_TOKEN_FILE" env)))
      "<authenticated>")
     (t nil))))

(provide 'superchat-bridge-auth)

;;; superchat-bridge-auth.el ends here
