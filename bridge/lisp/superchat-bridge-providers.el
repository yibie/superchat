;;; superchat-bridge-providers.el --- Providers of one shape, and any OpenAI-compatible server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/providers/<id>.ts' and `src/providers/<id>.models.ts'
;; (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/', for
;; the 18 ids of `superchat-bridge--providers': ant-ling, baseten, cerebras,
;; groq, huggingface, moonshotai, moonshotai-cn, nvidia, qwen-token-plan,
;; qwen-token-plan-cn, qwen-token-plan-individual, together, xiaomi,
;; xiaomi-token-plan-ams, xiaomi-token-plan-cn, xiaomi-token-plan-sgp, zai
;; and zai-coding-cn.  Each of those files is the shape of `deepseek.ts':
;; `createProvider' with the openai-completions API, `envApiKeyAuth' with a
;; name and variables, a base URL and the models.  Here the shape is written
;; once and the table holds what differs, copied from the .ts files.
;; The models are those of `data/<id>.json', read on first use.
;;
;; `superchat-bridge-openai-compatible-provider' is NOT in the source.  It
;; makes a provider for a local server (LM Studio, Ollama) or any other
;; OpenAI-compatible endpoint, from a few arguments.

;;; Code:

(require 'cl-lib)
(require 'superchat-bridge-models)
(require 'superchat-bridge-auth)
(require 'superchat-bridge-model-catalog)
(require 'superchat-bridge-provider-deepseek)

(defconst superchat-bridge--providers
  '(("ant-ling" "Ant Ling" "https://api.ant-ling.com/v1"
     "Ant Ling API key" ("ANT_LING_API_KEY"))
    ("baseten" "Baseten" "https://inference.baseten.co/v1"
     "Baseten API key" ("BASETEN_API_KEY"))
    ("cerebras" "Cerebras" "https://api.cerebras.ai/v1"
     "Cerebras API key" ("CEREBRAS_API_KEY"))
    ("groq" "Groq" "https://api.groq.com/openai/v1"
     "Groq API key" ("GROQ_API_KEY"))
    ("huggingface" "Hugging Face" "https://router.huggingface.co/v1"
     "Hugging Face token" ("HF_TOKEN"))
    ("moonshotai" "Moonshot AI" "https://api.moonshot.ai/v1"
     "Moonshot AI API key" ("MOONSHOT_API_KEY"))
    ("moonshotai-cn" "Moonshot AI CN" "https://api.moonshot.cn/v1"
     "Moonshot AI API key" ("MOONSHOT_API_KEY"))
    ("nvidia" "NVIDIA" "https://integrate.api.nvidia.com/v1"
     "NVIDIA API key" ("NVIDIA_API_KEY"))
    ("qwen-token-plan" "Qwen Token Plan"
     "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
     "Qwen Token Plan API key" ("QWEN_TOKEN_PLAN_API_KEY"))
    ("qwen-token-plan-cn" "Qwen Token Plan CN"
     "https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1"
     "Qwen Token Plan CN API key" ("QWEN_TOKEN_PLAN_CN_API_KEY"))
    ("qwen-token-plan-individual" "Qwen Token Plan Individual"
     "https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1"
     "Qwen Token Plan Individual API key" ("QWEN_TOKEN_PLAN_API_KEY"))
    ("together" "Together" "https://api.together.ai/v1"
     "Together API key" ("TOGETHER_API_KEY"))
    ("xiaomi" "Xiaomi" "https://api.xiaomimimo.com/v1"
     "Xiaomi API key" ("XIAOMI_API_KEY"))
    ("xiaomi-token-plan-ams" "Xiaomi Token Plan AMS"
     "https://token-plan-ams.xiaomimimo.com/v1"
     "Xiaomi Token Plan AMS API key" ("XIAOMI_TOKEN_PLAN_AMS_API_KEY"))
    ("xiaomi-token-plan-cn" "Xiaomi Token Plan CN"
     "https://token-plan-cn.xiaomimimo.com/v1"
     "Xiaomi Token Plan CN API key" ("XIAOMI_TOKEN_PLAN_CN_API_KEY"))
    ("xiaomi-token-plan-sgp" "Xiaomi Token Plan SGP"
     "https://token-plan-sgp.xiaomimimo.com/v1"
     "Xiaomi Token Plan SGP API key" ("XIAOMI_TOKEN_PLAN_SGP_API_KEY"))
    ("zai" "Z.AI" "https://api.z.ai/api/coding/paas/v4"
     "Z.AI API key" ("ZAI_API_KEY"))
    ("zai-coding-cn" "Z.AI Coding CN"
     "https://open.bigmodel.cn/api/coding/paas/v4"
     "Z.AI Coding CN API key" ("ZAI_CODING_CN_API_KEY")))
  "Rows (ID NAME BASE-URL AUTH-LABEL ENV-VARS), as in the `<id>.ts' files.")

(defun superchat-bridge-provider-ids ()
  "Return the ids of the providers `superchat-bridge-provider' knows."
  (mapcar #'car superchat-bridge--providers))

(defun superchat-bridge--catalog-models (id)
  "Return the openai-completions chat models of `data/ID.json', a list."
  (cl-remove-if-not
   (lambda (model) (equal (superchat-bridge-model-api model) "openai-completions"))
   (mapcar #'cdr (superchat-bridge-flatten-chat-model-catalog
                  id (superchat-bridge-model-catalog-read-provider id)))))

(defun superchat-bridge-provider (id)
  "Return the provider of ID, one of `superchat-bridge-provider-ids'.
Its models are read from `data/ID.json' when it is built."
  (pcase-let ((`(,_ ,name ,base-url ,label ,env-vars)
               (or (assoc id superchat-bridge--providers)
                   (error "Unknown provider: %s" id))))
    (superchat-bridge-create-provider
     :id id :name name :base-url base-url
     :auth (superchat-bridge-make-provider-auth
            :api-key (superchat-bridge-env-api-key-auth label env-vars))
     :models (superchat-bridge--catalog-models id)
     :api (superchat-bridge-openai-completions-api))))

(defun superchat-bridge-provider-env-vars (provider)
  "Return the names of the variables PROVIDER reads its API key from.
Nil for a provider that needs no key."
  (superchat-bridge-api-key-auth-env-vars
   (superchat-bridge-provider-auth-api-key
    (superchat-bridge-provider-auth provider))))

;;; Not in the source

(defun superchat-bridge--compatible-model (id base-url spec)
  "Return the model of the provider ID at BASE-URL that SPEC describes.
SPEC is a model id, or a plist (:id :context-window :max-tokens :reasoning)."
  (let ((spec (if (stringp spec) (list :id spec) spec)))
    (superchat-bridge-make-model
     :type 'chat :id (plist-get spec :id) :name (plist-get spec :id)
     :api "openai-completions" :provider id :base-url base-url
     :input '("text") :cost (superchat-bridge-make-model-cost)
     :reasoning (plist-get spec :reasoning)
     :context-window (or (plist-get spec :context-window) 128000)
     :max-tokens (or (plist-get spec :max-tokens) 16384))))

(cl-defun superchat-bridge-openai-compatible-provider
    (&key id name base-url models api-key-env)
  "Return a provider for an OpenAI-compatible server.  Not in the source.
For a local server (LM Studio, Ollama) or any endpoint that speaks the
OpenAI Chat Completions API at BASE-URL.  ID is the provider id, NAME its
display name (default ID).  MODELS is a list whose items are a model id
string, or a plist (:id S :context-window N :max-tokens N :reasoning BOOL);
the defaults are 128000, 16384, no reasoning, text input and zero cost.
API-KEY-ENV is a list of variable names to read the key from, as for the
other providers.  Without it no key is needed: the provider's auth
resolves to the placeholder key \"none\", which goes in the
Authorization header, and nothing else is changed."
  (superchat-bridge-create-provider
   :id id :name name :base-url base-url
   :auth (superchat-bridge-make-provider-auth
          :api-key
          (if api-key-env
              (superchat-bridge-env-api-key-auth
               (format "%s API key" (or name id)) api-key-env)
            (superchat-bridge-make-api-key-auth
             :name "No API key"
             :resolve (lambda (_ctx _credential _signal)
                        (list :auth (list :api-key "none") :source "none")))))
   :models (mapcar (lambda (spec) (superchat-bridge--compatible-model id base-url spec))
                   models)
   :api (superchat-bridge-openai-completions-api)))

(provide 'superchat-bridge-providers)

;;; superchat-bridge-providers.el ends here
