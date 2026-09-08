;;; superchat-emacsclient.el --- JSON Run surface for emacsclient -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; This optional adapter exposes a small, JSON-shaped surface for processes
;; using `emacsclient': list/status/history/cancel.  It never starts an Emacs
;; server; a server must already be running for `emacsclient' to reach these
;; functions.
;;
;; `emacsclient --eval' evaluates arbitrary Emacs Lisp.  Any local process
;; that can connect to the server can already evaluate arbitrary Emacs Lisp,
;; so this adapter is a convenience surface, not a sandbox, authentication
;; layer, or security boundary.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'superchat-db)
(require 'superchat-runtime)
(require 'superchat-tape-view)

(defcustom superchat-emacsclient-history-limit 50
  "Default maximum number of history rows returned by the adapter.

Callers may override this per request with the optional LIMIT argument to
`superchat-emacsclient-session-history'."
  :type 'natnum
  :group 'superchat)

(defun superchat-emacsclient--error-object (code message)
  "Build a JSON response object for CODE and MESSAGE."
  (list
   (cons "ok" json-false)
   (cons "error"
         (list (cons "code" code)
               (cons "message" (format "%s" message))))))

(defun superchat-emacsclient--encode (object)
  "Encode OBJECT as JSON without letting encoding failures escape."
  (condition-case err
      (json-encode object)
    (error
     (condition-case nil
         (json-encode
          (superchat-emacsclient--error-object
           "encoding-error" (error-message-string err)))
       (error "{\"ok\":false,\"error\":{\"code\":\"encoding-error\",\"message\":\"JSON encoding failed\"}}")))))

(defun superchat-emacsclient--safe (function)
  "Call FUNCTION and always return a JSON string.
FUNCTION is a zero-argument thunk; unexpected failures become JSON errors."
  (condition-case err
      (funcall function)
    (error
     (superchat-emacsclient--encode
      (superchat-emacsclient--error-object
       "error" (error-message-string err))))))

(defun superchat-emacsclient--public-status (status)
  "Return the contract §7.2 public status symbol for Runtime STATUS.
`completed' → `ready'; `failed', `timed-out', `audit-incomplete' →
`error'; `cancelled' → `stopped'; a non-terminal Run reports `waiting',
`typing' or `calling-tool' from its `:model-phase'."
  (pcase (plist-get status :status)
    ('completed 'ready)
    ((or 'failed 'timed-out 'audit-incomplete) 'error)
    ('cancelled 'stopped)
    ('orphaned 'orphaned)
    (_ (pcase (plist-get status :model-phase)
         ((or 'typing 'streaming 'first-token) 'typing)
         ((or 'calling-tool 'tool) 'calling-tool)
         (_ 'waiting)))))

(defun superchat-emacsclient--status-object (status)
  "Return the stable external fields from public STATUS.
`run_status' (S14) carries the contract §7.2 public status; the older
`status' field keeps Runtime's internal status unchanged."
  (let ((object (list (cons "run_status"
                            (symbol-name (superchat-emacsclient--public-status status))))))
    (dolist (field '((:run-id . "run_id")
                     (:status . "status")
                     (:session-id . "session_id")
                     (:model . "model")
                     (:capability . "capability")
                     (:started-at . "started_at")
                     (:completed-at . "completed_at")
                     (:error . "error")
                     (:actor-id . "actor_id")
                     (:profile-id . "profile_id")
                     (:parent-run-id . "parent_run_id")
                     (:workspace-id . "workspace_id")
                     (:correlation-id . "correlation_id")
                     (:topology . "topology")
                     (:authority-policy . "authority_policy")
                     (:surface . "surface")
                     (:pending-steer-count . "pending_steer_count")
                     (:undelivered-steer-count . "undelivered_steer_count")))
      (when (plist-member status (car field))
        (push (cons (cdr field) (plist-get status (car field))) object)))
    (nreverse object)))

(defun superchat-emacsclient--run-object (run-id)
  "Return a stable external object for RUN-ID, or nil if it disappeared."
  (when-let* ((status (superchat-runtime-status run-id)))
    (superchat-emacsclient--status-object status)))

(defun superchat-emacsclient--history-row-object (row)
  "Return a stable external object for Tape history ROW."
  (list (cons "id" (nth 0 row))
        (cons "topic" (or (nth 1 row) ""))
        (cons "kind" (or (nth 2 row) ""))
        (cons "content" (or (nth 3 row) ""))
        (cons "meta" (or (nth 4 row) ""))
        (cons "created_at" (or (nth 5 row) ""))))

(defun superchat-emacsclient--valid-name-p (value)
  "Return non-nil when VALUE is a non-empty external identifier."
  (and (stringp value) (not (string-empty-p value))))

;;;###autoload
(defun superchat-emacsclient-list-runs ()
  "Return active Runtime Runs as a JSON string.

The JSON object contains `ok' and a `runs' array.  This command does not
start a server and is useful when called through an already-running
`emacsclient' server."
  (superchat-emacsclient--safe
   (lambda ()
     (superchat-emacsclient--encode
      (list
       (cons "ok" t)
       (cons
        "runs"
        (vconcat
         (delq nil
               (mapcar #'superchat-emacsclient--run-object
                       (superchat-runtime-active-runs))))))))))

;;;###autoload
(defun superchat-emacsclient-run-status (run-id)
  "Return RUN-ID's public Runtime status as a JSON string.

An unknown RUN-ID returns a JSON `run-not-found' error object instead of
signalling an Emacs Lisp error."
  (superchat-emacsclient--safe
   (lambda ()
     (unless (superchat-emacsclient--valid-name-p run-id)
       (error "run_id must be a non-empty string"))
     (let ((object (superchat-emacsclient--run-object run-id)))
       (if object
           (superchat-emacsclient--encode
            (list (cons "ok" t) (cons "run" object)))
         (superchat-emacsclient--encode
          (superchat-emacsclient--error-object
           "run-not-found" (format "Run %s does not exist" run-id))))))))

;;;###autoload
(defun superchat-emacsclient-session-history (session-id &optional limit)
  "Return recent SESSION-ID Tape history as a bounded JSON string.

LIMIT defaults to `superchat-emacsclient-history-limit' and is the maximum
number of rows returned.  Rows remain in chronological order.  A missing
SESSION-ID or malformed LIMIT returns a JSON error object."
  (superchat-emacsclient--safe
   (lambda ()
     (unless (superchat-emacsclient--valid-name-p session-id)
       (error "session_id must be a non-empty string"))
     (let ((effective-limit
            (if (null limit) superchat-emacsclient-history-limit limit)))
       (unless (and (integerp effective-limit) (>= effective-limit 0))
         (error "limit must be a non-negative integer"))
       (if (not (member session-id (superchat-view-session-ids)))
           (superchat-emacsclient--encode
            (superchat-emacsclient--error-object
             "session-not-found"
             (format "Session %s does not exist" session-id)))
         (let* ((rows (superchat-view-session-history session-id))
                (length (length rows))
                (start (max 0 (- length effective-limit)))
                (recent (nthcdr start rows)))
           (superchat-emacsclient--encode
            (list
             (cons "ok" t)
             (cons "session_id" session-id)
             (cons "limit" effective-limit)
             (cons "total_rows" length)
             (cons "truncated" (if (> start 0) t json-false))
             (cons "entries"
                   (vconcat (mapcar
                             #'superchat-emacsclient--history-row-object
                             recent)))))))))))

;;;###autoload
(defun superchat-emacsclient-run-cancel (run-id)
  "Cancel active RUN-ID through the existing Runtime cancel seam.

Return a JSON string.  This is the only mutating adapter operation; it never
implements cancellation itself and does not start an Emacs server."
  (superchat-emacsclient--safe
   (lambda ()
     (unless (superchat-emacsclient--valid-name-p run-id)
       (error "run_id must be a non-empty string"))
     (let ((status (superchat-runtime-status run-id)))
       (cond
        ((null status)
         (superchat-emacsclient--encode
          (superchat-emacsclient--error-object
           "run-not-found" (format "Run %s does not exist" run-id))))
        ((superchat-runtime-cancel run-id "[Cancelled via emacsclient]")
         (superchat-emacsclient--encode
          (list (cons "ok" t)
                (cons "run_id" run-id)
                (cons "status" "cancelled"))))
        (t
         (superchat-emacsclient--encode
          (superchat-emacsclient--error-object
           "run-not-active"
           (format "Run %s is already %s"
                   run-id (plist-get status :status))))))))))

(provide 'superchat-emacsclient)

;;; superchat-emacsclient.el ends here
