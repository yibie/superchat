;;; superchat-bridge-utils-transcript.el --- Transcript contexts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Part of the port of `src/utils/transcript.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Ported so far, because `superchat-bridge-models' needs them:
;; `createInitialSystemMessage' and `normalizeContext'.  Left for whoever
;; ports the rest of the file: `getInitialSystemMessage',
;; `withoutInitialSystemMessage', `getCurrentTools',
;; `getCurrentSystemMessage' and the other replay helpers.

;;; Code:

(require 'superchat-bridge-types)

(defun superchat-bridge-create-initial-system-message (system-prompt tools)
  "Return the leading system message for SYSTEM-PROMPT and TOOLS, or nil.
Nil when both are empty, so an empty transcript stays empty.  This is
the source's `createInitialSystemMessage'."
  (let ((has-system-prompt (and system-prompt (> (length system-prompt) 0)))
        (has-tools (and tools (> (length tools) 0))))
    (when (or has-system-prompt has-tools)
      (superchat-bridge-make-system-message
       :content (or system-prompt "")
       :tools-added (and has-tools tools)
       :timestamp 0))))

(defun superchat-bridge-normalize-context (context)
  "Fold the system prompt and tools of CONTEXT into a leading system message.
Return a new `superchat-bridge-context' holding the messages only, the source's
`TranscriptContext'.  This is the source's `normalizeContext'."
  (let* ((initial (superchat-bridge-create-initial-system-message
                   (superchat-bridge-context-system-prompt context)
                   (superchat-bridge-context-tools context)))
         (messages (superchat-bridge-context-messages context)))
    (superchat-bridge-make-context
     :messages (if initial (cons initial messages) messages))))

(provide 'superchat-bridge-utils-transcript)

;;; superchat-bridge-utils-transcript.el ends here
