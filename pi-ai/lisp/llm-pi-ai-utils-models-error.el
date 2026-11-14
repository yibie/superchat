;;; llm-pi-ai-utils-models-error.el --- ModelsError of pi-ai  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/models-error.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Source's `ModelsError' is an `Error' with a `code' and an optional
;; `cause'.  Here it is the error symbol `llm-pi-ai-models-error' whose
;; data are (MESSAGE CODE CAUSE): signalled with
;; `llm-pi-ai-signal-models-error', read with
;; `llm-pi-ai-models-error-code' and friends.  CODE is one of the
;; symbols `model_source', `model_validation', `provider', `stream',
;; `auth' and `oauth', as the source's `ModelsErrorCode'.  CAUSE is the
;; error object that was caught (ERROR-SYMBOL . DATA), a string, or nil.
;;
;; `llm-pi-ai-error-message-text' is the source's `error.message' (or
;; `String(error)' for what is not an `Error'), which a stream's error
;; message and every caller that surfaces an error use.
;;
;; The source's `formatThrownValue' (src/utils/diagnostics.ts, not yet
;; ported) turns a thrown value into text; here a cause is rendered by
;; `llm-pi-ai-error-message-text'.

;;; Code:

(require 'cl-lib)

(define-error 'llm-pi-ai-models-error "ModelsError")

(defconst llm-pi-ai-models-error-codes
  '(model_source model_validation provider stream auth oauth)
  "The `ModelsErrorCode' union of src/utils/models-error.ts.")

(defun llm-pi-ai-models-error-p (object)
  "Return non-nil when OBJECT is an error object of a `ModelsError'."
  (and (consp object)
       (symbolp (car object))
       (memq 'llm-pi-ai-models-error (get (car object) 'error-conditions))
       t))

(defun llm-pi-ai-error-message-text (err)
  "Return the message of ERR, an error object, as the source's `error.message'.
A `ModelsError' or `llm-pi-ai-abort-error' gives the message it was made
with (the first of its data); any other error
object gives `error-message-string'; a string gives itself; any other
value gives its printed form, like `String(value)'."
  (cond ((and (consp err) (symbolp (car err))
              (or (memq 'llm-pi-ai-models-error (get (car err) 'error-conditions))
                  (memq 'llm-pi-ai-abort-error (get (car err) 'error-conditions)))
              (stringp (cadr err)))
         (cadr err))
        ((stringp err) err)
        ((and (consp err) (symbolp (car err))
              (get (car err) 'error-conditions))
         (error-message-string err))
        (t (format "%s" err))))

(defun llm-pi-ai-models-error--with-cause-detail (message cause)
  "Return MESSAGE with the text of CAUSE added, the source's `withCauseDetail'.
Unchanged when CAUSE is nil, empty, or already inside MESSAGE."
  (if (null cause)
      message
    (let ((detail (string-trim (llm-pi-ai-error-message-text cause))))
      (if (or (string-empty-p detail) (string-search detail message))
          message
        (format "%s: %s" message detail)))))

(defun llm-pi-ai-models-error (code message &optional cause)
  "Return the error object of a `ModelsError' with CODE, MESSAGE and CAUSE.
The message holds the text of CAUSE too, as the source's constructor does."
  (list 'llm-pi-ai-models-error
        (llm-pi-ai-models-error--with-cause-detail message cause)
        code cause))

(defun llm-pi-ai-signal-models-error (code message &optional cause)
  "Signal a `ModelsError' with CODE, MESSAGE and CAUSE.
This is the source's `throw new ModelsError'."
  (let ((err (llm-pi-ai-models-error code message cause)))
    (signal (car err) (cdr err))))

(defun llm-pi-ai-models-error-code (err)
  "Return the code of ERR, a `ModelsError' object."
  (nth 2 err))

(defun llm-pi-ai-models-error-cause (err)
  "Return the cause of ERR, a `ModelsError' object, or nil."
  (nth 3 err))

(provide 'llm-pi-ai-utils-models-error)

;;; llm-pi-ai-utils-models-error.el ends here
