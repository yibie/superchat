;;; llm-pi-ai-utils-text.el --- Text of messages, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/text.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;; Three functions that read a message's content as text:
;;
;;   `llm-pi-ai-content-text'               the source's `contentText'
;;   `llm-pi-ai-system-message-text'        `getSystemMessageText'
;;   `llm-pi-ai-render-system-message-update'  `renderSystemMessageUpdate'
;;
;; A message's `content' is a string, as on the wire, or a list of
;; content blocks: text, thinking and image blocks and tool calls
;; (see `llm-pi-ai-types').  Only text blocks have text to join;
;; thinking, images and tool calls are passed over.  `llm-pi-ai-content-text'
;; takes both forms, so a caller need not ask which it has.
;;
;; A system message's `sections' is an alist of section name to text
;; or nil, where nil means that the section is removed; the source's
;; `Record<string, string | null>' keeps the order the keys were
;; added in, which an alist does too.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'llm-pi-ai-types)

(defun llm-pi-ai--join (strings separator)
  "Return STRINGS, a list, joined with SEPARATOR between neighbours.
The source's `[...].join(separator)': a list with no string is the
empty string, not nil, so a caller that concatenates still gets a
string."
  (mapconcat #'identity strings separator))

(defun llm-pi-ai-content-text (content &optional separator)
  "Return the text of CONTENT, joined with SEPARATOR.
The source's `contentText'.  CONTENT is a string, which is returned
as it is, or a list of content blocks, of which the text blocks'
text is joined; the others have none.  SEPARATOR defaults to a
newline.  A list with no text gives the empty string, as `[].join'
does in the source, not nil."
  (if (stringp content)
      content
    (let ((separator (or separator "\n")))
      (llm-pi-ai--join
       (delq nil
             (mapcar (lambda (block)
                       (when (llm-pi-ai-text-content-p block)
                         (or (llm-pi-ai-text-content-text block) "")))
                     content))
       separator))))

(defun llm-pi-ai-system-message-text (message)
  "Render MESSAGE, a system message, as its complete prompt.
The source's `getSystemMessageText': the message's content, then the
text of each section that is not removed, joined with a blank line,
and empty parts left out.  A message with nothing in it gives the
empty string, as the source's `join' does, not nil."
  (let ((parts (list (llm-pi-ai-content-text (llm-pi-ai-message-content message)))))
    (dolist (section (llm-pi-ai-system-message-sections message))
      (let ((text (cdr section)))
        (when (not (null text))
          (push text parts))))
    (llm-pi-ai--join (seq-filter (lambda (part) (> (length part) 0))
                                 (nreverse parts))
                     "\n\n")))

(defun llm-pi-ai-render-system-message-update (message)
  "Render MESSAGE, a later system message, for an API that takes one.
That is an API that accepts a system message in mid-conversation.
The source's `renderSystemMessageUpdate'.  Section changes are framed
by name, so that the model can relate them to the leading prompt: a
removed section says so, and a changed one carries its new text.
This framing is for the request only and may change between versions."
  (let ((parts nil)
        (text (llm-pi-ai-content-text (llm-pi-ai-message-content message))))
    (when (> (length text) 0)
      (push text parts))
    (dolist (section (llm-pi-ai-system-message-sections message))
      (let ((name (car section))
            (value (cdr section)))
        (push (if (null value)
                  (format "Removed system prompt section \"%s\"." name)
                (format "Updated system prompt section \"%s\":\n\n%s" name value))
              parts)))
    (llm-pi-ai--join (nreverse parts) "\n\n")))

(provide 'llm-pi-ai-utils-text)

;;; llm-pi-ai-utils-text.el ends here
