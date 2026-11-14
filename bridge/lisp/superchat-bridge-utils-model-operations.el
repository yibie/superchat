;;; superchat-bridge-utils-model-operations.el --- Model type checks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/model-operations.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Ported: `getModelType' and `isModelType', which `superchat-bridge-types'
;; already has as `superchat-bridge-get-model-type' and `superchat-bridge-is-model-type',
;; and `assertChatModel'.  Left out with the image and classifier
;; operations they serve: `assertImageModel', `assertClassifierModel',
;; `assertClassifierInputSupported', `imageErrorResult' and
;; `classifierErrorResult'.

;;; Code:

(require 'superchat-bridge-types)
(require 'superchat-bridge-utils-models-error)

(defun superchat-bridge-assert-chat-model (model)
  "Signal a `ModelsError' with code `provider' unless MODEL is a chat model.
This is the source's `assertChatModel'."
  (unless (superchat-bridge-is-model-type model 'chat)
    (superchat-bridge-signal-models-error
     'provider (format "Model %s/%s is not a chat model"
                       (superchat-bridge-model-provider model)
                       (superchat-bridge-model-id model))))
  model)

(provide 'superchat-bridge-utils-model-operations)

;;; superchat-bridge-utils-model-operations.el ends here
