;;; llm-pi-ai-utils-model-operations.el --- Model type checks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/model-operations.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Ported: `getModelType' and `isModelType', which `llm-pi-ai-types'
;; already has as `llm-pi-ai-get-model-type' and `llm-pi-ai-is-model-type',
;; and `assertChatModel'.  Left out with the image and classifier
;; operations they serve: `assertImageModel', `assertClassifierModel',
;; `assertClassifierInputSupported', `imageErrorResult' and
;; `classifierErrorResult'.

;;; Code:

(require 'llm-pi-ai-types)
(require 'llm-pi-ai-utils-models-error)

(defun llm-pi-ai-assert-chat-model (model)
  "Signal a `ModelsError' with code `provider' unless MODEL is a chat model.
This is the source's `assertChatModel'."
  (unless (llm-pi-ai-is-model-type model 'chat)
    (llm-pi-ai-signal-models-error
     'provider (format "Model %s/%s is not a chat model"
                       (llm-pi-ai-model-provider model)
                       (llm-pi-ai-model-id model))))
  model)

(provide 'llm-pi-ai-utils-model-operations)

;;; llm-pi-ai-utils-model-operations.el ends here
