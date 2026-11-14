;;; superchat-bridge-utils-abort.el --- Abort signals for pi-ai operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/abort.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;; Source's `AbortController' and `AbortSignal' are one object here: an
;; `superchat-bridge-abort-signal' is created unaborted, and
;; `superchat-bridge-abort-signal-abort' aborts it.  A caller that holds the
;; object may abort it; a function that only receives it as an option
;; reads it.  Listeners are functions of no arguments.
;;
;; Auth resolution and every operation behind it run synchronously in
;; Emacs, so the source's `raceWithAbortSignal' (which stops waiting for
;; a promise) becomes `superchat-bridge-race-with-abort-signal', which runs a
;; function and then looks at the signal; see its docstring.

;;; Code:

(require 'cl-lib)

(define-error 'superchat-bridge-abort-error "AbortError")

(cl-defstruct (superchat-bridge-abort-signal
               (:constructor superchat-bridge-make-abort-signal)
               (:copier nil))
  "Source's `AbortSignal' together with its `AbortController'.
`aborted' is non-nil once aborted; `reason' is then the Lisp error
object (ERROR-SYMBOL . DATA) or other value it was aborted with;
`listeners' are the functions to call on abort."
  (aborted nil)
  (reason nil)
  (listeners nil))

(defun superchat-bridge-abort-signal-abort (signal &optional reason)
  "Abort SIGNAL with REASON, the source's `controller.abort(reason)'.
Without REASON the reason is an `superchat-bridge-abort-error' whose message is
\"The operation was aborted\".  Do nothing when SIGNAL is already
aborted.  Call the listeners once, in the order they were added."
  (unless (superchat-bridge-abort-signal-aborted signal)
    (setf (superchat-bridge-abort-signal-aborted signal) t
          (superchat-bridge-abort-signal-reason signal)
          (or reason
              (list 'superchat-bridge-abort-error "The operation was aborted")))
    (let ((listeners (reverse (superchat-bridge-abort-signal-listeners signal))))
      (setf (superchat-bridge-abort-signal-listeners signal) nil)
      (dolist (listener listeners)
        (funcall listener))))
  nil)

(defun superchat-bridge-abort-signal-aborted-p (signal)
  "Return non-nil when SIGNAL is aborted."
  (and (superchat-bridge-abort-signal-aborted signal) t))

(defun superchat-bridge-abort-signal-add-listener (signal listener)
  "Call LISTENER with no arguments when SIGNAL aborts.
Like `addEventListener', it is not called for a signal that is aborted
already; check `superchat-bridge-abort-signal-aborted-p' first."
  (push listener (superchat-bridge-abort-signal-listeners signal))
  listener)

(defun superchat-bridge-abort-signal-remove-listener (signal listener)
  "Remove LISTENER from SIGNAL."
  (setf (superchat-bridge-abort-signal-listeners signal)
        (delq listener (superchat-bridge-abort-signal-listeners signal)))
  nil)

(defun superchat-bridge-abort-reason (signal)
  "Return why SIGNAL was aborted, the source's `abortReason'.
When SIGNAL has no reason it is an `superchat-bridge-abort-error' object."
  (or (superchat-bridge-abort-signal-reason signal)
      (list 'superchat-bridge-abort-error "The operation was aborted")))

(defun superchat-bridge-abort--signal-reason (signal)
  "Signal the reason SIGNAL was aborted with.
An error object is signalled as it is; any other value is wrapped in an
`superchat-bridge-abort-error' whose data are a message and that value."
  (let ((reason (superchat-bridge-abort-reason signal)))
    (if (and (consp reason) (symbolp (car reason))
             (get (car reason) 'error-conditions))
        (signal (car reason) (cdr reason))
      (signal 'superchat-bridge-abort-error
              (list "The operation was aborted" reason)))))

(defun superchat-bridge-abort-signal-throw-if-aborted (signal)
  "Signal SIGNAL's reason when it is aborted, the source's `throwIfAborted'."
  (when (superchat-bridge-abort-signal-aborted signal)
    (superchat-bridge-abort--signal-reason signal)))

(defun superchat-bridge-operation-signal (&optional signal)
  "Return SIGNAL, or a new signal that nothing aborts.
This is the source's `operationSignal'."
  (or signal (superchat-bridge-make-abort-signal)))

(defun superchat-bridge-race-with-abort-signal (operation signal)
  "Run OPERATION, a function of no arguments, and return its value.
This is the source's `raceWithAbortSignal' for synchronous work.  When
SIGNAL is aborted before OPERATION starts, OPERATION does not run.
When SIGNAL is aborted by the time OPERATION returns or signals an
error, SIGNAL's reason is signalled in place of the value or error, as
the source's promise rejects with the reason when the abort comes first."
  (superchat-bridge-abort-signal-throw-if-aborted signal)
  (let ((outcome (condition-case err
                     (list 'value (funcall operation))
                   (error (list 'error err)))))
    (superchat-bridge-abort-signal-throw-if-aborted signal)
    (if (eq (car outcome) 'value)
        (cadr outcome)
      (signal (car (cadr outcome)) (cdr (cadr outcome))))))

(provide 'superchat-bridge-utils-abort)

;;; superchat-bridge-utils-abort.el ends here
