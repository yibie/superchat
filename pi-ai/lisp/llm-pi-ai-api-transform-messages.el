;;; llm-pi-ai-api-transform-messages.el --- Messages for another model, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/transform-messages.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `llm-pi-ai-transform-messages'            the source's `transformMessages'
;;   `llm-pi-ai-split-error-tool-result-images'  `splitErrorToolResultImages'
;;   `llm-pi-ai-error-tool-result-split'       the `SplitErrorToolResult' interface
;;
;; A conversation recorded with one model is rewritten for the next
;; request: images become a note for a model that cannot read them,
;; thinking of other models is dropped, tool call ids are normalised
;; for the target API, a tool call without a result gets a synthetic
;; one, and failed or aborted assistant turns are left out.  The input
;; messages are never changed: a message that changes is a copy.
;;
;; The source's "null content becomes []" is a no-op here, since an
;; empty list is nil, and a missing content (nil, or `:null' as JSON
;; gives it) already is that; `:null' is turned into nil.  A message's
;; `content' is a string or a list of content blocks, as in
;; `llm-pi-ai-types'.

;;; Code:

(require 'cl-lib)
(require 'llm-pi-ai-types)
(require 'llm-pi-ai-utils-sanitize-unicode)

(defconst llm-pi-ai--non-vision-user-image-placeholder
  "(image omitted: model does not support images)")

(defconst llm-pi-ai--non-vision-tool-image-placeholder
  "(tool image omitted: model does not support images)")

(defconst llm-pi-ai--error-tool-image-placeholder
  "(image attached after the tool results)")

(cl-defstruct (llm-pi-ai-error-tool-result-split
               (:constructor llm-pi-ai-make-error-tool-result-split)
               (:copier nil))
  "An error tool result split into a text-only result and its images.
`content' is the result content with each run of images replaced by a
note, `caption' the text naming the tool call, to send before `images',
the list of image blocks."
  content
  caption
  images)

(defun llm-pi-ai--replace-images-with-placeholder (content placeholder)
  "Return CONTENT, a list of blocks, with each run of images as PLACEHOLDER.
The source's `replaceImagesWithPlaceholder': a run of image blocks
becomes one text block, none if a text block of that very text
comes right before it."
  (let ((result nil)
        (previous-was-placeholder nil))
    (dolist (block content)
      (if (llm-pi-ai-image-content-p block)
          (progn
            (unless previous-was-placeholder
              (push (llm-pi-ai-make-text-content :text placeholder) result))
            (setq previous-was-placeholder t))
        (push block result)
        (setq previous-was-placeholder
              (equal (llm-pi-ai-text-content-text block) placeholder))))
    (nreverse result)))

(defun llm-pi-ai-split-error-tool-result-images (message)
  "Split the images out of MESSAGE, an error tool result.
The source's `splitErrorToolResultImages', for APIs that reject images
in error results (Anthropic, and Claude on Bedrock).  Return nil when
MESSAGE is no error or has no image.  Otherwise return a
`llm-pi-ai-error-tool-result-split': send its `content' as the tool
result, and its `caption' and `images' after all tool results of the
turn, in the same user message (#10794)."
  (when (llm-pi-ai-tool-result-message-is-error message)
    (let* ((content (llm-pi-ai-message-content message))
           (images (cl-remove-if-not #'llm-pi-ai-image-content-p content)))
      (when images
        (llm-pi-ai-make-error-tool-result-split
         :content (llm-pi-ai--replace-images-with-placeholder
                   content llm-pi-ai--error-tool-image-placeholder)
         :caption (format "Images from the error result of tool call %s (%s):"
                          (llm-pi-ai-tool-result-message-tool-call-id message)
                          (llm-pi-ai-tool-result-message-tool-name message))
         :images images)))))

(defun llm-pi-ai--with-content (message content)
  "Return a copy of MESSAGE whose content is CONTENT."
  (let ((copy (copy-sequence message)))
    (setf (llm-pi-ai-message-content copy) content)
    copy))

(defun llm-pi-ai--downgrade-unsupported-images (messages model)
  "Return MESSAGES, with images made notes when MODEL takes none.
The source's `downgradeUnsupportedImages'.  A user message with a list
for content, and every tool result, is rewritten; a string is not."
  (if (member "image" (llm-pi-ai-model-input model))
      messages
    (mapcar
     (lambda (message)
       (cond
        ((and (llm-pi-ai-user-message-p message)
              (listp (llm-pi-ai-message-content message)))
         (llm-pi-ai--with-content
          message
          (llm-pi-ai--replace-images-with-placeholder
           (llm-pi-ai-message-content message)
           llm-pi-ai--non-vision-user-image-placeholder)))
        ((llm-pi-ai-tool-result-message-p message)
         (llm-pi-ai--with-content
          message
          (llm-pi-ai--replace-images-with-placeholder
           (llm-pi-ai-message-content message)
           llm-pi-ai--non-vision-tool-image-placeholder)))
        (t message)))
     messages)))

(defun llm-pi-ai--non-empty-string-p (value)
  "Return non-nil when VALUE is a string with characters, JavaScript truthy."
  (and (stringp value) (> (length value) 0)))

(defun llm-pi-ai--transform-assistant-block (block same-model model message
                                                   normalize-tool-call-id
                                                   tool-call-id-map)
  "Return the list of blocks that BLOCK of assistant MESSAGE becomes.
The `flatMap' callback of the source's first pass: a list of zero or one
block.  SAME-MODEL says that MESSAGE was made by MODEL; the other
arguments are those of `llm-pi-ai-transform-messages', of which
TOOL-CALL-ID-MAP is the hash table of original to normalised ids."
  (cond
   ((llm-pi-ai-thinking-content-p block)
    ;; Thinking from another model is dropped.  Signatures and encrypted
    ;; reasoning are only valid for the model that produced them, and
    ;; sending readable thinking as plain text makes the new model treat it
    ;; as part of the visible reply.
    (cond
     ((not same-model) nil)
     ;; Keep redacted and signed blocks even if the thinking text is empty
     ;; (OpenAI encrypted reasoning); they are needed for replay.
     ((or (llm-pi-ai-thinking-content-redacted block)
          (llm-pi-ai--non-empty-string-p
           (llm-pi-ai-thinking-content-thinking-signature block)))
      (list block))
     ((let ((thinking (llm-pi-ai-thinking-content-thinking block)))
        (or (not (llm-pi-ai--non-empty-string-p thinking))
            (equal (llm-pi-ai-string-trim-js thinking) "")))
      nil)
     (t (list block))))
   ((llm-pi-ai-text-content-p block)
    (if same-model
        (list block)
      (list (llm-pi-ai-make-text-content
             :text (llm-pi-ai-text-content-text block)))))
   ((llm-pi-ai-tool-call-p block)
    (let ((normalized block))
      (when (and (not same-model)
                 (llm-pi-ai--non-empty-string-p
                  (llm-pi-ai-tool-call-thought-signature block)))
        (setq normalized (copy-sequence block))
        (setf (llm-pi-ai-tool-call-thought-signature normalized) nil))
      (when (and (not same-model) normalize-tool-call-id)
        (let ((normalized-id (funcall normalize-tool-call-id
                                      (llm-pi-ai-tool-call-id block)
                                      model message)))
          (unless (equal normalized-id (llm-pi-ai-tool-call-id block))
            (puthash (llm-pi-ai-tool-call-id block) normalized-id
                     tool-call-id-map)
            (setq normalized (copy-sequence normalized))
            (setf (llm-pi-ai-tool-call-id normalized) normalized-id))))
      (list normalized)))
   (t (list block))))

(defun llm-pi-ai--now-ms ()
  "Return the current time in ms since the epoch, as `Date.now()'."
  (floor (* 1000 (float-time))))

(defun llm-pi-ai-transform-messages (messages model &optional normalize-tool-call-id)
  "Return MESSAGES, a list, rewritten to be sent to MODEL.
The source's `transformMessages'.  MODEL is an `llm-pi-ai-model'.
NORMALIZE-TOOL-CALL-ID, when given, is a function of three arguments,
(ID MODEL SOURCE), that returns the id to use for a tool call of the
assistant message SOURCE which another model made: OpenAI Responses
makes ids of 450 characters and more with characters like `|', and
Anthropic wants ^[a-zA-Z0-9_-]+$ and 64 characters at most.  The ids of
the tool results that answer such a call follow.

Done in two passes.  First, images the model cannot take become notes,
thinking blocks of other models go, text of other models loses its
signature, tool calls of other models lose their thought signature and
get a normalised id.  Second, a tool call without a result gets a
synthetic error result \"No result provided\" (a system message
between a call and its results waits until the results are in, so it
does not cause a duplicate), and an assistant message that ended
with `error' or `aborted' is dropped, being an incomplete turn that
must not be replayed."
  (let* ((tool-call-id-map (make-hash-table :test #'equal))
         ;; Null content from untyped callers is the empty list.
         (normalized-messages
          (mapcar (lambda (message)
                    (if (eq (llm-pi-ai-message-content message) :null)
                        (llm-pi-ai--with-content message nil)
                      message))
                  messages))
         (image-aware (llm-pi-ai--downgrade-unsupported-images
                       normalized-messages model))
         ;; First pass
         (transformed
          (mapcar
           (lambda (message)
             (cond
              ((llm-pi-ai-tool-result-message-p message)
               (let ((normalized-id
                      (gethash (llm-pi-ai-tool-result-message-tool-call-id message)
                               tool-call-id-map)))
                 (if (and normalized-id
                          (not (equal normalized-id
                                      (llm-pi-ai-tool-result-message-tool-call-id
                                       message))))
                     (let ((copy (copy-sequence message)))
                       (setf (llm-pi-ai-tool-result-message-tool-call-id copy)
                             normalized-id)
                       copy)
                   message)))
              ((llm-pi-ai-assistant-message-p message)
               (let ((same-model
                      (and (equal (llm-pi-ai-assistant-message-provider message)
                                  (llm-pi-ai-model-provider model))
                           (equal (llm-pi-ai-assistant-message-api message)
                                  (llm-pi-ai-model-api model))
                           (equal (llm-pi-ai-assistant-message-model message)
                                  (llm-pi-ai-model-id model)))))
                 (llm-pi-ai--with-content
                  message
                  (apply #'append
                         (mapcar
                          (lambda (block)
                            (llm-pi-ai--transform-assistant-block
                             block same-model model message
                             normalize-tool-call-id tool-call-id-map))
                          (llm-pi-ai-message-content message))))))
              ;; System and user messages pass through unchanged.
              (t message)))
           image-aware))
         ;; Second pass: synthetic empty tool results for orphaned tool calls.
         ;; This preserves thinking signatures and satisfies API requirements.
         (result nil)
         (pending-tool-calls nil)
         (existing-tool-result-ids (make-hash-table :test #'equal))
         ;; System messages are transparent to tool-call accounting: one that
         ;; lands between a tool call and its results is held back and emitted
         ;; after the results (synthetic ones included), so it never causes a
         ;; duplicate result for a call that is answered later.
         (held-system-messages nil))
    (cl-flet ((close-pending-tool-calls ()
                (when pending-tool-calls
                  (dolist (tool-call pending-tool-calls)
                    (unless (gethash (llm-pi-ai-tool-call-id tool-call)
                                     existing-tool-result-ids)
                      (push (llm-pi-ai-make-tool-result-message
                             :tool-call-id (llm-pi-ai-tool-call-id tool-call)
                             :tool-name (llm-pi-ai-tool-call-name tool-call)
                             :content (list (llm-pi-ai-make-text-content
                                             :text "No result provided"))
                             :is-error t
                             :timestamp (llm-pi-ai--now-ms))
                            result)))
                  (setq pending-tool-calls nil)
                  (setq existing-tool-result-ids
                        (make-hash-table :test #'equal)))
                (dolist (held (nreverse held-system-messages))
                  (push held result))
                (setq held-system-messages nil)))
      (dolist (message transformed)
        (cond
         ((llm-pi-ai-assistant-message-p message)
          ;; Orphaned tool calls of a previous assistant message get their
          ;; synthetic results now.
          (close-pending-tool-calls)
          ;; Skip errored/aborted assistant messages entirely.  These are
          ;; incomplete turns that shouldn't be replayed:
          ;; - May have partial content (reasoning without message,
          ;;   incomplete tool calls)
          ;; - Replaying them can cause API errors (e.g., OpenAI "reasoning
          ;;   without following item")
          ;; - The model should retry from the last valid state
          (unless (member (llm-pi-ai-assistant-message-stop-reason message)
                          '("error" "aborted"))
            (let ((tool-calls (cl-remove-if-not
                               #'llm-pi-ai-tool-call-p
                               (llm-pi-ai-message-content message))))
              (when tool-calls
                (setq pending-tool-calls tool-calls)
                (setq existing-tool-result-ids (make-hash-table :test #'equal))))
            (push message result)))
         ((llm-pi-ai-tool-result-message-p message)
          (puthash (llm-pi-ai-tool-result-message-tool-call-id message) t
                   existing-tool-result-ids)
          (push message result))
         ((llm-pi-ai-system-message-p message)
          (if pending-tool-calls
              (push message held-system-messages)
            (push message result)))
         ((llm-pi-ai-user-message-p message)
          ;; A new user turn interrupts tool flow: synthetic results for
          ;; orphaned calls.
          (close-pending-tool-calls)
          (push message result))
         (t (push message result))))
      ;; If the conversation ends with unresolved tool calls, synthesize
      ;; results now.
      (close-pending-tool-calls))
    (nreverse result)))

(provide 'llm-pi-ai-api-transform-messages)

;;; llm-pi-ai-api-transform-messages.el ends here
