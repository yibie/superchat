;;; superchat-tape.el --- Runtime event contract for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; New Runtime events enter Tape through this module.  The existing SQLite
;; schema remains unchanged: `kind' keeps its legacy values while the
;; versioned event envelope is stored in `meta'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'superchat-db)

(defgroup superchat-tape nil
  "Runtime event history for Superchat."
  :group 'superchat)

(defcustom superchat-tape-max-inline-bytes 16384
  "Maximum encoded payload size stored inline in a Tape event.
Larger payloads are replaced with byte-size and SHA-256 metadata."
  :type 'natnum
  :group 'superchat-tape)

(defconst superchat-tape--legacy-kind-map
  '(("intent.received" . "user")
    ("response.completed" . "assistant")
    ("tool.requested" . "tool_call")
    ("tool.execution.started" . "tool_call")
    ("tool.completed" . "tool_result")
    ("tool.failed" . "tool_result")
    ("anchor.created" . "anchor"))
  "Map Runtime event types to the existing Tape kind.")

(defconst superchat-tape--envelope-keys
  '((:turn-id . :turn_id)
    (:run-id . :run_id)
    (:actor-id . :actor_id)
    (:profile-id . :profile_id)
    (:workspace-id . :workspace_id)
    (:correlation-id . :correlation_id)
    (:causation-id . :causation_id)
    (:parent-run-id . :parent_run_id)
    (:team-id . :team_id)
    (:audience . :audience)
    (:sensitivity . :sensitivity))
  "Optional EVENT keys copied into the persisted envelope.")

(declare-function superchat-db-tape-append
                  "superchat-db"
                  (session-id kind content &rest options))
(declare-function superchat-db-tape-replay
                  "superchat-db"
                  (session-id &optional since-id limit))

(defun superchat-tape--plist-p (value)
  "Return non-nil when VALUE is a property list."
  (and (consp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun superchat-tape--alist-p (value)
  "Return non-nil when VALUE is an association list."
  (and (consp value)
       (cl-every (lambda (item)
                   (and (consp item)
                        (not (consp (car item)))))
                 value)))

(defun superchat-tape--sensitive-key-p (key)
  "Return non-nil when KEY conventionally contains a secret."
  (let ((name (downcase
               (replace-regexp-in-string
                "\\`:" "" (if (symbolp key) (symbol-name key)
                              (format "%s" key))))))
    (or (member name
                '("authorization" "cookie" "credentials" "credential"
                  "password" "passwd" "secret" "token" "access_token"
                  "refresh_token" "api_key" "apikey" "private_key"))
        (string-match-p
         "\\(?:\\`\\|[-_]\\)\\(?:password\\|secret\\|token\\|api[-_]?key\\)\\'"
         name))))

(defun superchat-tape--redact (value)
  "Return VALUE with conventionally secret fields recursively redacted."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash key
                  (if (superchat-tape--sensitive-key-p key)
                      "[REDACTED]"
                    (superchat-tape--redact item))
                  copy))
       value)
      copy))
   ((vectorp value)
    (apply #'vector (mapcar #'superchat-tape--redact (append value nil))))
   ((superchat-tape--plist-p value)
    (let (result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (setq result
                (nconc result
                       (list key
                             (if (superchat-tape--sensitive-key-p key)
                                 "[REDACTED]"
                               (superchat-tape--redact item)))))))
      result))
   ((superchat-tape--alist-p value)
    (mapcar
     (lambda (item)
       (cons (car item)
             (if (superchat-tape--sensitive-key-p (car item))
                 "[REDACTED]"
               (superchat-tape--redact (cdr item)))))
     value))
   ((consp value)
    (mapcar #'superchat-tape--redact value))
   (t value)))

(defun superchat-tape--encoded-payload (payload)
  "Return PAYLOAD redacted and bounded for inline persistence."
  (let* ((redacted (superchat-tape--redact payload))
         (encoded
          (condition-case nil
              (encode-coding-string (json-encode redacted) 'utf-8)
            (error
             (encode-coding-string (prin1-to-string redacted) 'utf-8))))
         (size (string-bytes encoded)))
    (if (<= size superchat-tape-max-inline-bytes)
        redacted
      (list :omitted t
            :size size
            :content_hash
            (concat "sha256:" (secure-hash 'sha256 encoded))))))

(defun superchat-tape--recorded-at ()
  "Return the current time as a UTC RFC 3339 timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))

(defun superchat-tape--new-event-id ()
  "Return a storage-independent event identity."
  (concat
   "evt_"
   (substring
    (secure-hash
     'sha256
     (format "%S:%s:%s" (current-time) (emacs-pid) (random)))
    0 26)))

(defun superchat-tape--legacy-kind (type)
  "Return the legacy Tape kind for event TYPE."
  (or (cdr (assoc type superchat-tape--legacy-kind-map))
      "system"))

(defun superchat-tape--summary (event)
  "Return a bounded human-readable summary for EVENT.
Summaries supplied by callers must already be redacted; tool event callers
therefore use only the normalized tool name and never arguments or results."
  (let* ((type (plist-get event :type))
         (payload (plist-get event :payload))
         (tool-name
          (cond
           ((superchat-tape--plist-p payload)
            (or (plist-get payload :tool-name)
                (plist-get payload :tool_name)))
           ((superchat-tape--alist-p payload)
            (or (alist-get 'tool-name payload)
                (alist-get 'tool_name payload)))
           (t nil)))
         (summary (plist-get event :summary)))
    (truncate-string-to-width
     (cond
      ((and (stringp summary) (not (string-empty-p summary))) summary)
      (tool-name (format "%s: %s" type tool-name))
      (t type))
     500 nil nil "…")))

(defun superchat-tape--row-event (row)
  "Decode one persisted Tape ROW into a normalized Runtime event."
  (let* ((storage-id (nth 0 row))
         (meta
          (condition-case err
              (json-parse-string
               (or (nth 4 row) "")
               :object-type 'plist
               :array-type 'list
               :null-object nil
               :false-object nil)
            (error
             (error "Invalid Tape event metadata at row %s: %s"
                    storage-id (error-message-string err)))))
         (runtime-envelope-p
          (or (plist-member meta :event_type)
              (plist-member meta :event_id)
              (plist-member meta :schema))))
    (when runtime-envelope-p
      (let ((event
             (list :type (plist-get meta :event_type)
                   :event-id (plist-get meta :event_id)
                   :schema (plist-get meta :schema)
                   :session-id (plist-get meta :session_id)
                   :recorded-at (plist-get meta :recorded_at)
                   :storage-id storage-id
                   :payload (plist-get meta :payload))))
        (dolist (entry superchat-tape--envelope-keys)
          (when (plist-member meta (cdr entry))
            (setq event
                  (plist-put event (car entry)
                             (plist-get meta (cdr entry))))))
        event))))

;;;###autoload
(defun superchat-tape-replay-events (session-id &optional since-id limit)
  "Return normalized Runtime events persisted for SESSION-ID.
SINCE-ID and LIMIT have the same meaning as `superchat-db-tape-replay'."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p session-id)))
  (delq nil
        (mapcar #'superchat-tape--row-event
                (superchat-db-tape-replay session-id since-id limit))))

;;;###autoload
(defun superchat-tape-append-event (event)
  "Validate and append Runtime EVENT to the existing Tape.

EVENT is a plist with required non-empty string fields `:type' and
`:session-id'.  This module assigns the logical `event_id' and `recorded_at';
the database assigns the numeric storage cursor returned to legacy callers.
Payloads are redacted and size-limited before reaching SQLite.  Persistence
errors are deliberately propagated to the caller."
  (unless (listp event)
    (signal 'wrong-type-argument (list 'listp event)))
  (let ((type (plist-get event :type))
        (session-id (plist-get event :session-id))
        (event-id (or (plist-get event :event-id)
                      (superchat-tape--new-event-id)))
        (schema (or (plist-get event :schema) 1)))
    (unless (and (stringp type) (not (string-empty-p type)))
      (error "Tape event :type must be a non-empty string"))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Tape event :session-id must be a non-empty string"))
    (unless (and (stringp event-id) (not (string-empty-p event-id)))
      (error "Tape event :event-id must be a non-empty string"))
    (unless (and (integerp schema) (> schema 0))
      (error "Tape event :schema must be a positive integer"))
    (let* ((payload
            (superchat-tape--encoded-payload
             (or (plist-get event :payload) '())))
           (meta (list :event_type type
                       :event_id event-id
                       :schema schema
                       :session_id session-id
                       :recorded_at (superchat-tape--recorded-at)
                       :payload payload)))
      (when (and (plist-get event :requires-payload)
                 (superchat-tape--plist-p payload)
                 (plist-get payload :omitted))
        (error "Tape payload is too large for event %s" type))
      (dolist (entry superchat-tape--envelope-keys)
        (let ((value (plist-get event (car entry))))
          (when value
            (setq meta (plist-put meta (cdr entry) value)))))
      (superchat-db-tape-append
       session-id
       (superchat-tape--legacy-kind type)
       (superchat-tape--summary event)
       :topic (or (plist-get event :topic) "")
       :meta meta))))

;;;###autoload
(defun superchat-tape-append-event-record (event)
  "Append EVENT and return its logical identity and storage cursor.
The returned plist contains `:event-id' and `:storage-id'."
  (let* ((event-id (or (plist-get event :event-id)
                       (superchat-tape--new-event-id)))
         (event (plist-put (copy-sequence event) :event-id event-id)))
    (list :event-id event-id
          :storage-id (superchat-tape-append-event event))))

(provide 'superchat-tape)

;;; superchat-tape.el ends here
