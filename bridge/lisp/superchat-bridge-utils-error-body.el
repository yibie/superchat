;;; superchat-bridge-utils-error-body.el --- Provider HTTP error bodies, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/error-body.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `superchat-bridge-normalize-provider-error'  the source's `normalizeProviderError'
;;   `superchat-bridge-format-provider-error'     `formatProviderError'
;;   `superchat-bridge-truncate-error-text'       `truncateErrorText'
;;   `superchat-bridge-safe-json-stringify'       `safeJsonStringify'
;;   `superchat-bridge-max-provider-error-body-chars'  `MAX_PROVIDER_ERROR_BODY_CHARS'
;;
;; What the source reads is a JavaScript `Error' that an SDK threw,
;; with the HTTP status and body under SDK-specific field names.
;; There are no SDKs here, so an error is one of these Lisp values:
;;
;;  - a plist with a `:message' key, the Error.  It may also carry the
;;    SDK fields, under these keys: `:status-code' (Mistral), `:status'
;;    (openai, @google/genai), `:body' (a string), `:error' (the parsed
;;    JSON body: a hash table), `:metadata' (a plist, with
;;    `:http-status-code') and `:response' (a plist with `:status-code'
;;    and `:body').  The probes run in the source's order.
;;  - a Lisp error condition, `(error "text")': the Error, with
;;    `error-message-string' as its message and no SDK fields.
;;  - anything else: a value that was thrown but is no Error.
;;
;; A "plain object" of the source is a hash table here, as
;; `json-parse-string' makes it.  A class instance (a stream wrapper,
;; an SDK's inner error) is any other non-string, non-nil value; it
;; never counts as a body.  The one stream-like value is a hash table
;; with a function under "pipe", as Node streams have a `pipe' method.
;;
;; Lengths, in `superchat-bridge-truncate-error-text' and the cap, are in
;; UTF-16 code units as in the source; see
;; `superchat-bridge-utf16-length'.  A cut that falls inside a character
;; above U+FFFF leaves a lone high surrogate character at the end, as
;; the source's `slice' leaves a lone surrogate; see
;; `superchat-bridge-sanitize-surrogates'.
;;
;; `superchat-bridge-safe-json-stringify' uses `json-serialize', which
;; writes a float with a fraction (1.0, where JavaScript writes 1),
;; reads an alist or a plist as an object, and refuses what it cannot
;; write (a function, a struct); what it refuses is shown as `prin1'
;; shows it, where the source's `String(value)' would give
;; "[object Object]".  nil stands for undefined (shown as
;; "undefined"), `:null' for null.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-bridge-utils-sanitize-unicode)

(defconst superchat-bridge-max-provider-error-body-chars 4000
  "Longest error body kept, in UTF-16 code units.
`MAX_PROVIDER_ERROR_BODY_CHARS' of the source.")

(cl-defstruct (superchat-bridge-normalized-provider-error
               (:constructor superchat-bridge-make-normalized-provider-error)
               (:copier nil))
  "A `NormalizedProviderError'.
`status' is the HTTP status when one was found, or nil.  `body' is the
raw HTTP body, trimmed and cut at the cap, or nil.  `message' is the
error's message, or its JSON text for a thrown non-error.
`message-carries-body' is non-nil when `message' already holds the
body, so that there is no separate one to add."
  status
  body
  message
  message-carries-body)

(defun superchat-bridge-safe-json-stringify (value)
  "Return VALUE as JSON text, or as plain text if it has none.
The source's `safeJsonStringify'; see the file commentary for how the
two differ."
  (cond
   ((null value) "undefined")
   ((eq value :null) "null")
   (t
    (condition-case nil
        (let ((text (decode-coding-string (json-serialize (vector value))
                                          'utf-8)))
          ;; `json-serialize' takes only objects and arrays at the
          ;; top, so VALUE rode in an array: take the brackets off.
          (substring text 1 (1- (length text))))
      (error (if (stringp value) value (prin1-to-string value)))))))

(defun superchat-bridge-truncate-error-text (text max-chars)
  "Return TEXT cut to MAX-CHARS UTF-16 code units, with a note if cut.
The source's `truncateErrorText'."
  (let ((length (superchat-bridge-utf16-length text)))
    (if (<= length max-chars)
        text
      (let ((kept nil)
            (count 0)
            (i 0))
        (while (< count max-chars)
          (let ((char (aref text i)))
            (setq i (1+ i))
            (if (and (> char #xFFFF) (<= char #x10FFFF))
                (let ((offset (- char #x10000)))
                  (push (+ #xD800 (ash offset -10)) kept)
                  (setq count (1+ count))
                  (when (< count max-chars)
                    (push (+ #xDC00 (logand offset #x3FF)) kept)
                    (setq count (1+ count))))
              (push char kept)
              (setq count (1+ count)))))
        ;; A high surrogate and the low one after it, as the cut left
        ;; them, are one character again.
        (let ((chars (nreverse kept))
              (joined nil))
          (while chars
            (let ((a (car chars)) (b (cadr chars)))
              (if (and b (superchat-bridge--high-surrogate-p a)
                       (superchat-bridge--low-surrogate-p b))
                  (progn (push (+ #x10000 (ash (- a #xD800) 10) (- b #xDC00))
                               joined)
                         (setq chars (cddr chars)))
                (push a joined)
                (setq chars (cdr chars)))))
          (format "%s... [truncated %d chars]"
                  (concat (nreverse joined))
                  (- length max-chars)))))))

(defun superchat-bridge--plain-non-empty-object-p (value)
  "Return non-nil when VALUE is a hash table with an entry.
The source's `isPlainNonEmptyObject'."
  (and (hash-table-p value) (> (hash-table-count value) 0)))

(defun superchat-bridge--readable-stream-like-p (value)
  "Return non-nil when VALUE looks like a stream, as the source asks.
The source's `isReadableStreamLike': it has a function `pipe'."
  (and (hash-table-p value) (functionp (gethash "pipe" value))))

(defun superchat-bridge--error-field (error key)
  "Return the field KEY of ERROR, a plist, or nil."
  (plist-get error key))

(defun superchat-bridge--error-p (value)
  "Return non-nil when VALUE is an Error of the file commentary."
  (or (and (consp value) (keywordp (car value))
           (plist-member value :message) t)
      (and (consp value) (symbolp (car value))
           (get (car value) 'error-conditions) t)))

(defun superchat-bridge--error-message (error)
  "Return the message of ERROR, see `superchat-bridge--error-p'."
  (if (keywordp (car error))
      (plist-get error :message)
    (error-message-string error)))

(defun superchat-bridge--extract-status (error)
  "Return the first numeric HTTP status of ERROR, or nil.
The source's `extractStatus': `:status-code', `:status',
`:metadata' `:http-status-code', `:response' `:status-code'."
  (when (keywordp (car error))
    (cl-find-if #'numberp
                (list (plist-get error :status-code)
                      (plist-get error :status)
                      (plist-get (plist-get error :metadata) :http-status-code)
                      (plist-get (plist-get error :response) :status-code)))))

(defun superchat-bridge--pick-body-text (error)
  "Return the raw body text of ERROR, or nil.
The source's `pickBodyText'."
  (when (keywordp (car error))
    (let ((body (plist-get error :body))
          (inner (plist-get error :error))
          (response-body (plist-get (plist-get error :response) :body)))
      (cond
       ((stringp body) body)
       ((superchat-bridge--plain-non-empty-object-p inner)
        (superchat-bridge-safe-json-stringify inner))
       ((stringp response-body) response-body)
       ((superchat-bridge--readable-stream-like-p response-body) nil)
       ((superchat-bridge--plain-non-empty-object-p response-body)
        (superchat-bridge-safe-json-stringify response-body))))))

(defun superchat-bridge--extract-body (error)
  "Return the trimmed, capped body of ERROR, or nil.
The source's `extractBody'."
  (let ((body-text (superchat-bridge--pick-body-text error)))
    (when body-text
      (let ((trimmed (superchat-bridge-string-trim-js body-text)))
        (when (> (length trimmed) 0)
          (superchat-bridge-truncate-error-text
           trimmed superchat-bridge-max-provider-error-body-chars))))))

(defun superchat-bridge-normalize-provider-error (error)
  "Return the `superchat-bridge-normalized-provider-error' of ERROR.
The source's `normalizeProviderError'; ERROR is one of the values the
file commentary lists."
  (if (not (superchat-bridge--error-p error))
      (superchat-bridge-make-normalized-provider-error
       :message (superchat-bridge-safe-json-stringify error)
       :message-carries-body nil)
    (let* ((status (superchat-bridge--extract-status error))
           (body (superchat-bridge--extract-body error))
           (message (superchat-bridge--error-message error)))
      (superchat-bridge-make-normalized-provider-error
       :status status
       :body body
       :message message
       :message-carries-body (or (null body)
                                 (and (string-search body message) t))))))

(defun superchat-bridge-format-provider-error (norm &optional prefix)
  "Return the display string of NORM, a normalized provider error.
The source's `formatProviderError'.  PREFIX is the provider prefix, or
nil for none.  Without PREFIX the form is \"STATUS: BODY\", with it
\"PREFIX (STATUS): BODY\"; the message stands in for the body when it
already carries it or when no status or body was found."
  (let ((status (superchat-bridge-normalized-provider-error-status norm))
        (body (superchat-bridge-normalized-provider-error-body norm))
        (message (superchat-bridge-normalized-provider-error-message norm)))
    (if (or (superchat-bridge-normalized-provider-error-message-carries-body norm)
            (null status)
            (null body))
        (if (and prefix status)
            (format "%s (%d): %s" prefix status message)
          message)
      (if prefix
          (format "%s (%d): %s" prefix status body)
        (format "%d: %s" status body)))))

(provide 'superchat-bridge-utils-error-body)

;;; superchat-bridge-utils-error-body.el ends here
