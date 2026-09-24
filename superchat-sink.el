;;; superchat-sink.el --- Delivery target of a Run -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A Sink is the exact delivery target of a Run: the callbacks Runtime uses
;; to hand a Run's stream chunks, status transitions and terminal result to
;; whichever caller submitted it.  It never carries a buffer routing key; the
;; caller decides what to do with what it is handed.

;;; Code:

(require 'cl-lib)

(cl-defstruct (superchat-sink
               (:constructor superchat-sink--raw-create)
               (:conc-name superchat-sink--)
               (:copier nil))
  id        ; string; `superchat-sink-create' fills "sink-<8 hex>" when nil
  live-p    ; nil (always live) or (lambda () BOOL)
  stream    ; nil or (lambda (RUN CHUNK))     CHUNK = (:kind text|reasoning :text STRING)
  status    ; nil or (lambda (RUN STATUS))    STATUS = waiting|typing|calling-tool|retrying|ready|error|stopped|orphaned
  final     ; nil or (lambda (RUN RESULT))    RESULT  = (:text STRING :tape-id ID-or-nil)
  error     ; nil or (lambda (RUN FAILURE))   FAILURE = (:status failed|timed-out|audit-incomplete|cancelled :text STRING :reason STRING-or-nil :tape-id ID-or-nil)
  orphaned) ; nil or (lambda (RUN REASON))    REASON = sink-lost

(defvar superchat-sink--diagnostics nil
  "Bounded ring of the last 100 Sink callback failures.
Each entry is a plist `(:sink-id ID :callback SLOT :condition CONDITION)'.
Internal to this file; read it only through `superchat-sink-diagnostics'.")

(defun superchat-sink-retry-text (retry &optional now)
  "Return the shared visible retry notice for RETRY at time NOW."
  (format "Provider error; retrying in %ds (%s/%s)…"
          (max 0 (ceiling (- (plist-get retry :at) (or now (float-time)))))
          (plist-get retry :attempt) (plist-get retry :limit)))

(defun superchat-sink--record-diagnostic (sink-id callback condition)
  "Push one diagnostic for SINK-ID/CALLBACK/CONDITION, capped at 100 entries."
  (push (list :sink-id sink-id :callback callback :condition condition)
        superchat-sink--diagnostics)
  (when (> (length superchat-sink--diagnostics) 100)
    (setcdr (nthcdr 99 superchat-sink--diagnostics) nil)))

(defun superchat-sink-diagnostics ()
  "Return the current bounded Sink diagnostics ring, most recent first."
  superchat-sink--diagnostics)

(defun superchat-sink-record-diagnostic (sink-id callback condition)
  "Record a diagnostic for SINK-ID/CALLBACK/CONDITION from outside this file.
Callers use this only when they themselves caught a condition that a Sink
callback caused further downstream, such as a Tape append that failed while
recording a Sink's delivery failure."
  (superchat-sink--record-diagnostic sink-id callback condition))

(cl-defun superchat-sink-create
    (&key id live-p stream status final error orphaned)
  "Build a Sink from the given callbacks.
ID is generated as \"sink-<8 hex>\" when nil.  Every other argument is
stored on the matching struct slot, unchanged.
Final and error payloads may also carry :tool-budget, a plist with
:rounds, :limit and :action (finalize or stop).  It describes why tool
execution ended; it neither changes :text nor asserts task success."
  (superchat-sink--raw-create
   :id (or id (format "sink-%08x" (random #x100000000)))
   :live-p live-p :stream stream :status status
   :final final :error error :orphaned orphaned))

(defalias 'superchat-sink-callback #'superchat-sink-create
  "Alias of `superchat-sink-create', named for call sites that build a Sink
purely from callbacks.")

(defun superchat-sink-id (sink)
  "Return SINK's id, or nil when SINK is nil."
  (and sink (superchat-sink--id sink)))

(defun superchat-sink-discard ()
  "Return a Sink with every callback nil.
It is always live and silently drops every emission; nothing it is given
reaches any buffer, function, or Tape row."
  (superchat-sink-create))

(defun superchat-sink-live-p (sink)
  "Return non-nil when SINK should still receive emissions.
A nil `live-p' predicate means always-live.  A predicate that signals
`error' counts as dead, and records one diagnostic."
  (let ((predicate (and sink (superchat-sink--live-p sink))))
    (cond
     ((null predicate) t)
     (t (condition-case condition
            (and (funcall predicate) t)
          (error
           (superchat-sink--record-diagnostic
            (superchat-sink-id sink) 'live-p condition)
           nil))))))

(defun superchat-sink--callback-for (sink slot)
  "Return SINK's callback for SLOT, one of the Sink's function slots."
  (and sink
       (pcase slot
         ('stream (superchat-sink--stream sink))
         ('status (superchat-sink--status sink))
         ('final (superchat-sink--final sink))
         ('error (superchat-sink--error sink))
         ('orphaned (superchat-sink--orphaned sink)))))

(cl-defun superchat-sink-emit (sink slot run payload &key capture-quit)
  "Invoke SINK's SLOT callback with RUN and PAYLOAD.

Return a plist `(:sink-id ID :callback SLOT :settled-p BOOL :condition
ERR-or-nil)'.  A missing callback is a settled discard.  `error' is always
caught and recorded as a diagnostic.  `quit' propagates immediately unless
CAPTURE-QUIT is non-nil, in which case it is caught, recorded, and returned
as `:condition' instead."
  (let ((sink-id (superchat-sink-id sink))
        (callback (superchat-sink--callback-for sink slot)))
    (if (not (functionp callback))
        (list :sink-id sink-id :callback slot :settled-p t :condition nil)
      (condition-case condition
          (progn
            (funcall callback run payload)
            (list :sink-id sink-id :callback slot :settled-p t :condition nil))
        (error
         (superchat-sink--record-diagnostic sink-id slot condition)
         (list :sink-id sink-id :callback slot :settled-p nil :condition condition))
        (quit
         (superchat-sink--record-diagnostic sink-id slot condition)
         (if capture-quit
             (list :sink-id sink-id :callback slot :settled-p nil :condition condition)
           (signal (car condition) (cdr condition))))))))

(defun superchat-sink-normalize-chunk (raw)
  "Normalize RAW provider chunk to `(:kind text|reasoning :text STRING)'.
A plain string is text.  `(reasoning X)' and `(:reasoning X)' are reasoning.
Anything else returns nil."
  (cond
   ((stringp raw) (list :kind 'text :text raw))
   ((and (consp raw) (memq (car raw) '(reasoning :reasoning)))
    (list :kind 'reasoning :text (cadr raw)))
   (t nil)))

(provide 'superchat-sink)
;;; superchat-sink.el ends here
