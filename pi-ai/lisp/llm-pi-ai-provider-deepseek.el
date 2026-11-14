;;; llm-pi-ai-provider-deepseek.el --- The DeepSeek provider  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/providers/deepseek.ts' and `src/providers/deepseek.models.ts'
;; (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; `llm-pi-ai-deepseek-provider' returns the provider; its models are
;; those of `data/deepseek.json', and its API key is the stored
;; credential's, else the environment variable DEEPSEEK_API_KEY.
;;
;; The API implementation.  Source's `openAICompletionsApi()' is
;; `lazyApi(() => import("./openai-completions.ts"))'.  Here the
;; provider's `api' is `llm-pi-ai-lazy-api' over a loader that, when
;; the first request is made, runs
;;
;;   (require 'llm-pi-ai-api-openai-completions)
;;
;; and builds an `llm-pi-ai-provider-streams' from the two functions
;; that file must define, the module exports `stream' and `streamSimple'
;; of `src/api/openai-completions.ts':
;;
;;   (llm-pi-ai-api-openai-completions-stream MODEL CONTEXT OPTIONS)
;;   (llm-pi-ai-api-openai-completions-stream-simple MODEL CONTEXT OPTIONS)
;;
;; each returning an assistant message event stream.  Until that file
;; exists, a request ends in an `error' event whose message says the
;; feature cannot be found; no load-time failure.  A test or caller may
;; give `llm-pi-ai-deepseek-provider' an API of its own.
;;
;; Of `deepseek.models.ts', `DEEPSEEK_MODELS' is
;; `llm-pi-ai-deepseek-models'; `DEEPSEEK_IMAGE_MODELS' and
;; `DEEPSEEK_CLASSIFIER_MODELS' are not ported (the file has none).

;;; Code:

(require 'llm-pi-ai-models)
(require 'llm-pi-ai-auth)
(require 'llm-pi-ai-model-catalog)

(declare-function llm-pi-ai-api-openai-completions-stream
                  "llm-pi-ai-api-openai-completions" (model context options))
(declare-function llm-pi-ai-api-openai-completions-stream-simple
                  "llm-pi-ai-api-openai-completions" (model context options))

(defvar llm-pi-ai-deepseek--models nil
  "The models of `data/deepseek.json' once read, an alist ((ID . MODEL) ...).")

(defun llm-pi-ai-deepseek-models ()
  "Return the chat models of DeepSeek as an alist ((ID . MODEL) ...).
The source's `DEEPSEEK_MODELS'.  The file is read on first use and kept."
  (or llm-pi-ai-deepseek--models
      (setq llm-pi-ai-deepseek--models
            (llm-pi-ai-flatten-chat-model-catalog
             "deepseek" (llm-pi-ai-model-catalog-read-provider "deepseek")))))

(defun llm-pi-ai-deepseek--load-openai-completions-api ()
  "Return the `llm-pi-ai-provider-streams' of the openai-completions API.
Load `llm-pi-ai-api-openai-completions' first; signal if it is missing."
  (require 'llm-pi-ai-api-openai-completions)
  (llm-pi-ai-make-provider-streams
   :stream #'llm-pi-ai-api-openai-completions-stream
   :stream-simple #'llm-pi-ai-api-openai-completions-stream-simple))

(defun llm-pi-ai-openai-completions-api ()
  "Return the lazily loaded openai-completions API, `openAICompletionsApi()' of
`src/api/openai-completions.lazy.ts'."
  (llm-pi-ai-lazy-api #'llm-pi-ai-deepseek--load-openai-completions-api))

(defun llm-pi-ai-deepseek-provider (&optional api)
  "Return the DeepSeek provider, the source's `deepseekProvider()'.
API is an `llm-pi-ai-provider-streams' to use in place of the
openai-completions API, for tests."
  (llm-pi-ai-create-provider
   :id "deepseek"
   :name "DeepSeek"
   :base-url "https://api.deepseek.com"
   :auth (llm-pi-ai-make-provider-auth
          :api-key (llm-pi-ai-env-api-key-auth "DeepSeek API key"
                                               '("DEEPSEEK_API_KEY")))
   :models (mapcar #'cdr (llm-pi-ai-deepseek-models))
   :api (or api (llm-pi-ai-openai-completions-api))))

(provide 'llm-pi-ai-provider-deepseek)

;;; llm-pi-ai-provider-deepseek.el ends here
