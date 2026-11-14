;;; superchat-bridge-provider-deepseek.el --- The DeepSeek provider  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/providers/deepseek.ts' and `src/providers/deepseek.models.ts'
;; (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; `superchat-bridge-deepseek-provider' returns the provider; its models are
;; those of `data/deepseek.json', and its API key is the stored
;; credential's, else the environment variable DEEPSEEK_API_KEY.
;;
;; The API implementation.  Source's `openAICompletionsApi()' is
;; `lazyApi(() => import("./openai-completions.ts"))'.  Here the
;; provider's `api' is `superchat-bridge-lazy-api' over a loader that, when
;; the first request is made, runs
;;
;;   (require 'superchat-bridge-api-openai-completions)
;;
;; and builds an `superchat-bridge-provider-streams' from the two functions
;; that file must define, the module exports `stream' and `streamSimple'
;; of `src/api/openai-completions.ts':
;;
;;   (superchat-bridge-api-openai-completions-stream MODEL CONTEXT OPTIONS)
;;   (superchat-bridge-api-openai-completions-stream-simple MODEL CONTEXT OPTIONS)
;;
;; each returning an assistant message event stream.  Until that file
;; exists, a request ends in an `error' event whose message says the
;; feature cannot be found; no load-time failure.  A test or caller may
;; give `superchat-bridge-deepseek-provider' an API of its own.
;;
;; Of `deepseek.models.ts', `DEEPSEEK_MODELS' is
;; `superchat-bridge-deepseek-models'; `DEEPSEEK_IMAGE_MODELS' and
;; `DEEPSEEK_CLASSIFIER_MODELS' are not ported (the file has none).

;;; Code:

(require 'superchat-bridge-models)
(require 'superchat-bridge-auth)
(require 'superchat-bridge-model-catalog)

(declare-function superchat-bridge-api-openai-completions-stream
                  "superchat-bridge-api-openai-completions" (model context options))
(declare-function superchat-bridge-api-openai-completions-stream-simple
                  "superchat-bridge-api-openai-completions" (model context options))

(defvar superchat-bridge-deepseek--models nil
  "The models of `data/deepseek.json' once read, an alist ((ID . MODEL) ...).")

(defun superchat-bridge-deepseek-models ()
  "Return the chat models of DeepSeek as an alist ((ID . MODEL) ...).
The source's `DEEPSEEK_MODELS'.  The file is read on first use and kept."
  (or superchat-bridge-deepseek--models
      (setq superchat-bridge-deepseek--models
            (superchat-bridge-flatten-chat-model-catalog
             "deepseek" (superchat-bridge-model-catalog-read-provider "deepseek")))))

(defun superchat-bridge-deepseek--load-openai-completions-api ()
  "Return the `superchat-bridge-provider-streams' of the openai-completions API.
Load `superchat-bridge-api-openai-completions' first; signal if it is missing."
  (require 'superchat-bridge-api-openai-completions)
  (superchat-bridge-make-provider-streams
   :stream #'superchat-bridge-api-openai-completions-stream
   :stream-simple #'superchat-bridge-api-openai-completions-stream-simple))

(defun superchat-bridge-openai-completions-api ()
  "Return the lazily loaded openai-completions API, `openAICompletionsApi()' of
`src/api/openai-completions.lazy.ts'."
  (superchat-bridge-lazy-api #'superchat-bridge-deepseek--load-openai-completions-api))

(defun superchat-bridge-deepseek-provider (&optional api)
  "Return the DeepSeek provider, the source's `deepseekProvider()'.
API is an `superchat-bridge-provider-streams' to use in place of the
openai-completions API, for tests."
  (superchat-bridge-create-provider
   :id "deepseek"
   :name "DeepSeek"
   :base-url "https://api.deepseek.com"
   :auth (superchat-bridge-make-provider-auth
          :api-key (superchat-bridge-env-api-key-auth "DeepSeek API key"
                                               '("DEEPSEEK_API_KEY")))
   :models (mapcar #'cdr (superchat-bridge-deepseek-models))
   :api (or api (superchat-bridge-openai-completions-api))))

(provide 'superchat-bridge-provider-deepseek)

;;; superchat-bridge-provider-deepseek.el ends here
