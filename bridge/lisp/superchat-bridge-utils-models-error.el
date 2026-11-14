;;; superchat-bridge-utils-models-error.el --- ModelsError of pi-ai  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/models-error.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Source's `ModelsError' is an `Error' with a `code' and an optional
;; `cause'.  Here it is the error symbol `superchat-bridge-models-error' whose
;; data are (MESSAGE CODE CAUSE): signalled with
;; `superchat-bridge-signal-models-error', read with
;; `superchat-bridge-models-error-code' and friends.  CODE is one of the
;; symbols `model_source', `model_validation', `provider', `stream',
;; `auth' and `oauth', as the source's `ModelsErrorCode'.  CAUSE is the
;; error object that was caught (ERROR-SYMBOL . DATA), a string, or nil.
;;
;; `superchat-bridge-error-message-text' is the source's `error.message' (or
;; `String(error)' for what is not an `Error'), which a stream's error
;; message and every caller that surfaces an error use.
;;
;; The source's `formatThrownValue' (src/utils/diagnostics.ts, not yet
;; ported) turns a thrown value into text; here a cause is rendered by
;; `superchat-bridge-error-message-text'.

;;; Code:

(require 'cl-lib)

(define-error 'superchat-bridge-models-error "ModelsError")

(defconst superchat-bridge-models-error-codes
  '(model_source model_validation provider stream auth oauth)
  "The `ModelsErrorCode' union of src/utils/models-error.ts.")

(defun superchat-bridge-models-error-p (object)
  "Return non-nil when OBJECT is an error object of a `ModelsError'."
  (and (consp object)
       (symbolp (car object))
       (memq 'superchat-bridge-models-error (get (car object) 'error-conditions))
       t))

(defun superchat-bridge-error-message-text (err)
  "Return the message of ERR, an error object, as the source's `error.message'.
A `ModelsError' or `superchat-bridge-abort-error' gives the message it was made
with (the first of its data); any other error
object gives `error-message-string'; a string gives itself; any other
value gives its printed form, like `String(value)'."
  (cond ((and (consp err) (symbolp (car err))
              (or (memq 'superchat-bridge-models-error (get (car err) 'error-conditions))
                  (memq 'superchat-bridge-abort-error (get (car err) 'error-conditions)))
              (stringp (cadr err)))
         (cadr err))
        ((stringp err) err)
        ((and (consp err) (symbolp (car err))
              (get (car err) 'error-conditions))
         (error-message-string err))
        (t (format "%s" err))))

(defun superchat-bridge-models-error--with-cause-detail (message cause)
  "Return MESSAGE with the text of CAUSE added, the source's `withCauseDetail'.
Unchanged when CAUSE is nil, empty, or already inside MESSAGE."
  (if (null cause)
      message
    (let ((detail (string-trim (superchat-bridge-error-message-text cause))))
      (if (or (string-empty-p detail) (string-search detail message))
          message
        (format "%s: %s" message detail)))))

(defun superchat-bridge-models-error (code message &optional cause)
  "Return the error object of a `ModelsError' with CODE, MESSAGE and CAUSE.
The message holds the text of CAUSE too, as the source's constructor does."
  (list 'superchat-bridge-models-error
        (superchat-bridge-models-error--with-cause-detail message cause)
        code cause))

(defun superchat-bridge-signal-models-error (code message &optional cause)
  "Signal a `ModelsError' with CODE, MESSAGE and CAUSE.
This is the source's `throw new ModelsError'."
  (let ((err (superchat-bridge-models-error code message cause)))
    (signal (car err) (cdr err))))

(defun superchat-bridge-models-error-code (err)
  "Return the code of ERR, a `ModelsError' object."
  (nth 2 err))

(defun superchat-bridge-models-error-cause (err)
  "Return the cause of ERR, a `ModelsError' object, or nil."
  (nth 3 err))

(provide 'superchat-bridge-utils-models-error)

;;; superchat-bridge-utils-models-error.el ends here
