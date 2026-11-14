;;; llm-pi-ai-utils-transcript.el --- Transcript contexts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Part of the port of `src/utils/transcript.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Ported so far, because `llm-pi-ai-models' needs them:
;; `createInitialSystemMessage' and `normalizeContext'.  Left for whoever
;; ports the rest of the file: `getInitialSystemMessage',
;; `withoutInitialSystemMessage', `getCurrentTools',
;; `getCurrentSystemMessage' and the other replay helpers.

;;; Code:

(require 'llm-pi-ai-types)

(defun llm-pi-ai-create-initial-system-message (system-prompt tools)
  "Return the leading system message for SYSTEM-PROMPT and TOOLS, or nil.
Nil when both are empty, so an empty transcript stays empty.  This is
the source's `createInitialSystemMessage'."
  (let ((has-system-prompt (and system-prompt (> (length system-prompt) 0)))
        (has-tools (and tools (> (length tools) 0))))
    (when (or has-system-prompt has-tools)
      (llm-pi-ai-make-system-message
       :content (or system-prompt "")
       :tools-added (and has-tools tools)
       :timestamp 0))))

(defun llm-pi-ai-normalize-context (context)
  "Fold the system prompt and tools of CONTEXT into a leading system message.
Return a new `llm-pi-ai-context' holding the messages only, the source's
`TranscriptContext'.  This is the source's `normalizeContext'."
  (let* ((initial (llm-pi-ai-create-initial-system-message
                   (llm-pi-ai-context-system-prompt context)
                   (llm-pi-ai-context-tools context)))
         (messages (llm-pi-ai-context-messages context)))
    (llm-pi-ai-make-context
     :messages (if initial (cons initial messages) messages))))

(provide 'llm-pi-ai-utils-transcript)

;;; llm-pi-ai-utils-transcript.el ends here
