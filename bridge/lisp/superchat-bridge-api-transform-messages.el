;;; superchat-bridge-api-transform-messages.el --- Messages for another model, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/transform-messages.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `superchat-bridge-transform-messages'            the source's `transformMessages'
;;   `superchat-bridge-split-error-tool-result-images'  `splitErrorToolResultImages'
;;   `superchat-bridge-error-tool-result-split'       the `SplitErrorToolResult' interface
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
;; `superchat-bridge-types'.

;;; Code:

(require 'cl-lib)
(require 'superchat-bridge-types)
(require 'superchat-bridge-utils-sanitize-unicode)

(defconst superchat-bridge--non-vision-user-image-placeholder
  "(image omitted: model does not support images)")

(defconst superchat-bridge--non-vision-tool-image-placeholder
  "(tool image omitted: model does not support images)")

(defconst superchat-bridge--error-tool-image-placeholder
  "(image attached after the tool results)")

(cl-defstruct (superchat-bridge-error-tool-result-split
               (:constructor superchat-bridge-make-error-tool-result-split)
               (:copier nil))
  "An error tool result split into a text-only result and its images.
`content' is the result content with each run of images replaced by a
note, `caption' the text naming the tool call, to send before `images',
the list of image blocks."
  content
  caption
  images)

(defun superchat-bridge--replace-images-with-placeholder (content placeholder)
  "Return CONTENT, a list of blocks, with each run of images as PLACEHOLDER.
The source's `replaceImagesWithPlaceholder': a run of image blocks
becomes one text block, none if a text block of that very text
comes right before it."
  (let ((result nil)
        (previous-was-placeholder nil))
    (dolist (block content)
      (if (superchat-bridge-image-content-p block)
          (progn
            (unless previous-was-placeholder
              (push (superchat-bridge-make-text-content :text placeholder) result))
            (setq previous-was-placeholder t))
        (push block result)
        (setq previous-was-placeholder
              (equal (superchat-bridge-text-content-text block) placeholder))))
    (nreverse result)))

(defun superchat-bridge-split-error-tool-result-images (message)
  "Split the images out of MESSAGE, an error tool result.
The source's `splitErrorToolResultImages', for APIs that reject images
in error results (Anthropic, and Claude on Bedrock).  Return nil when
MESSAGE is no error or has no image.  Otherwise return a
`superchat-bridge-error-tool-result-split': send its `content' as the tool
result, and its `caption' and `images' after all tool results of the
turn, in the same user message (#10794)."
  (when (superchat-bridge-tool-result-message-is-error message)
    (let* ((content (superchat-bridge-message-content message))
           (images (cl-remove-if-not #'superchat-bridge-image-content-p content)))
      (when images
        (superchat-bridge-make-error-tool-result-split
         :content (superchat-bridge--replace-images-with-placeholder
                   content superchat-bridge--error-tool-image-placeholder)
         :caption (format "Images from the error result of tool call %s (%s):"
                          (superchat-bridge-tool-result-message-tool-call-id message)
                          (superchat-bridge-tool-result-message-tool-name message))
         :images images)))))

(defun superchat-bridge--with-content (message content)
  "Return a copy of MESSAGE whose content is CONTENT."
  (let ((copy (copy-sequence message)))
    (setf (superchat-bridge-message-content copy) content)
    copy))

(defun superchat-bridge--downgrade-unsupported-images (messages model)
  "Return MESSAGES, with images made notes when MODEL takes none.
The source's `downgradeUnsupportedImages'.  A user message with a list
for content, and every tool result, is rewritten; a string is not."
  (if (member "image" (superchat-bridge-model-input model))
      messages
    (mapcar
     (lambda (message)
       (cond
        ((and (superchat-bridge-user-message-p message)
              (listp (superchat-bridge-message-content message)))
         (superchat-bridge--with-content
          message
          (superchat-bridge--replace-images-with-placeholder
           (superchat-bridge-message-content message)
           superchat-bridge--non-vision-user-image-placeholder)))
        ((superchat-bridge-tool-result-message-p message)
         (superchat-bridge--with-content
          message
          (superchat-bridge--replace-images-with-placeholder
           (superchat-bridge-message-content message)
           superchat-bridge--non-vision-tool-image-placeholder)))
        (t message)))
     messages)))

(defun superchat-bridge--non-empty-string-p (value)
  "Return non-nil when VALUE is a string with characters, JavaScript truthy."
  (and (stringp value) (> (length value) 0)))

(defun superchat-bridge--transform-assistant-block (block same-model model message
                                                   normalize-tool-call-id
                                                   tool-call-id-map)
  "Return the list of blocks that BLOCK of assistant MESSAGE becomes.
The `flatMap' callback of the source's first pass: a list of zero or one
block.  SAME-MODEL says that MESSAGE was made by MODEL; the other
arguments are those of `superchat-bridge-transform-messages', of which
TOOL-CALL-ID-MAP is the hash table of original to normalised ids."
  (cond
   ((superchat-bridge-thinking-content-p block)
    ;; Thinking from another model is dropped.  Signatures and encrypted
    ;; reasoning are only valid for the model that produced them, and
    ;; sending readable thinking as plain text makes the new model treat it
    ;; as part of the visible reply.
    (cond
     ((not same-model) nil)
     ;; Keep redacted and signed blocks even if the thinking text is empty
     ;; (OpenAI encrypted reasoning); they are needed for replay.
     ((or (superchat-bridge-thinking-content-redacted block)
          (superchat-bridge--non-empty-string-p
           (superchat-bridge-thinking-content-thinking-signature block)))
      (list block))
     ((let ((thinking (superchat-bridge-thinking-content-thinking block)))
        (or (not (superchat-bridge--non-empty-string-p thinking))
            (equal (superchat-bridge-string-trim-js thinking) "")))
      nil)
     (t (list block))))
   ((superchat-bridge-text-content-p block)
    (if same-model
        (list block)
      (list (superchat-bridge-make-text-content
             :text (superchat-bridge-text-content-text block)))))
   ((superchat-bridge-tool-call-p block)
    (let ((normalized block))
      (when (and (not same-model)
                 (superchat-bridge--non-empty-string-p
                  (superchat-bridge-tool-call-thought-signature block)))
        (setq normalized (copy-sequence block))
        (setf (superchat-bridge-tool-call-thought-signature normalized) nil))
      (when (and (not same-model) normalize-tool-call-id)
        (let ((normalized-id (funcall normalize-tool-call-id
                                      (superchat-bridge-tool-call-id block)
                                      model message)))
          (unless (equal normalized-id (superchat-bridge-tool-call-id block))
            (puthash (superchat-bridge-tool-call-id block) normalized-id
                     tool-call-id-map)
            (setq normalized (copy-sequence normalized))
            (setf (superchat-bridge-tool-call-id normalized) normalized-id))))
      (list normalized)))
   (t (list block))))

(defun superchat-bridge--now-ms ()
  "Return the current time in ms since the epoch, as `Date.now()'."
  (floor (* 1000 (float-time))))

(defun superchat-bridge-transform-messages (messages model &optional normalize-tool-call-id)
  "Return MESSAGES, a list, rewritten to be sent to MODEL.
The source's `transformMessages'.  MODEL is an `superchat-bridge-model'.
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
                    (if (eq (superchat-bridge-message-content message) :null)
                        (superchat-bridge--with-content message nil)
                      message))
                  messages))
         (image-aware (superchat-bridge--downgrade-unsupported-images
                       normalized-messages model))
         ;; First pass
         (transformed
          (mapcar
           (lambda (message)
             (cond
              ((superchat-bridge-tool-result-message-p message)
               (let ((normalized-id
                      (gethash (superchat-bridge-tool-result-message-tool-call-id message)
                               tool-call-id-map)))
                 (if (and normalized-id
                          (not (equal normalized-id
                                      (superchat-bridge-tool-result-message-tool-call-id
                                       message))))
                     (let ((copy (copy-sequence message)))
                       (setf (superchat-bridge-tool-result-message-tool-call-id copy)
                             normalized-id)
                       copy)
                   message)))
              ((superchat-bridge-assistant-message-p message)
               (let ((same-model
                      (and (equal (superchat-bridge-assistant-message-provider message)
                                  (superchat-bridge-model-provider model))
                           (equal (superchat-bridge-assistant-message-api message)
                                  (superchat-bridge-model-api model))
                           (equal (superchat-bridge-assistant-message-model message)
                                  (superchat-bridge-model-id model)))))
                 (superchat-bridge--with-content
                  message
                  (apply #'append
                         (mapcar
                          (lambda (block)
                            (superchat-bridge--transform-assistant-block
                             block same-model model message
                             normalize-tool-call-id tool-call-id-map))
                          (superchat-bridge-message-content message))))))
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
                    (unless (gethash (superchat-bridge-tool-call-id tool-call)
                                     existing-tool-result-ids)
                      (push (superchat-bridge-make-tool-result-message
                             :tool-call-id (superchat-bridge-tool-call-id tool-call)
                             :tool-name (superchat-bridge-tool-call-name tool-call)
                             :content (list (superchat-bridge-make-text-content
                                             :text "No result provided"))
                             :is-error t
                             :timestamp (superchat-bridge--now-ms))
                            result)))
                  (setq pending-tool-calls nil)
                  (setq existing-tool-result-ids
                        (make-hash-table :test #'equal)))
                (dolist (held (nreverse held-system-messages))
                  (push held result))
                (setq held-system-messages nil)))
      (dolist (message transformed)
        (cond
         ((superchat-bridge-assistant-message-p message)
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
          (unless (member (superchat-bridge-assistant-message-stop-reason message)
                          '("error" "aborted"))
            (let ((tool-calls (cl-remove-if-not
                               #'superchat-bridge-tool-call-p
                               (superchat-bridge-message-content message))))
              (when tool-calls
                (setq pending-tool-calls tool-calls)
                (setq existing-tool-result-ids (make-hash-table :test #'equal))))
            (push message result)))
         ((superchat-bridge-tool-result-message-p message)
          (puthash (superchat-bridge-tool-result-message-tool-call-id message) t
                   existing-tool-result-ids)
          (push message result))
         ((superchat-bridge-system-message-p message)
          (if pending-tool-calls
              (push message held-system-messages)
            (push message result)))
         ((superchat-bridge-user-message-p message)
          ;; A new user turn interrupts tool flow: synthetic results for
          ;; orphaned calls.
          (close-pending-tool-calls)
          (push message result))
         (t (push message result))))
      ;; If the conversation ends with unresolved tool calls, synthesize
      ;; results now.
      (close-pending-tool-calls))
    (nreverse result)))

(provide 'superchat-bridge-api-transform-messages)

;;; superchat-bridge-api-transform-messages.el ends here
