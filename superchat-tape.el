;;; superchat-tape.el --- Runtime event contract for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; New Runtime events enter Tape through this module.  The existing SQLite
;; schema remains unchanged: `kind' keeps its legacy values while the
;; versioned event envelope is stored in `meta'.  Audit-incomplete terminal
;; seals use a bounded sidecar until replay can reconcile them into SQLite.

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

(defconst superchat-tape-compaction-anchor-topic "compact"
  "Tape topic used by typed compaction Anchors.")

(defconst superchat-tape-compaction-anchor-schema
  "superchat.compaction-anchor.v1"
  "Schema identifier used by typed compaction Anchors.")

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

(define-error 'superchat-tape-unknown-event-type
  "Tape event type is not in the static inventory")
(define-error 'superchat-tape-invalid-event
  "Tape event failed its relevance-group validator")

(defconst superchat-tape-subagent-event-suffixes
  '("started" "completed" "failed")
  "Allowed suffixes of the dynamic `subagent.%s' Tape event family.
`superchat--team-member-event' in `superchat-subagent.el' validates its
EVENT argument against this same list before writing to the Tape, so the
writer's domain and this inventory's knowledge of it cannot drift apart.")

(defconst superchat-tape--run-annotation-event-types
  '("decision.snapshot.recorded" "output.first.recorded"
    "run.steered" "run.steer-undelivered" "delivery.failed")
  "Run-scoped types that annotate a Run without advancing it.
Tagged `:closure annotation' in `superchat-tape-event-type-specs' below;
kept here only as the literal source for that one tagging step.")

(defconst superchat-tape--artifact-event-types
  '("workspace.artifact.added" "workspace.artifact.changed"
    "workspace.artifact.removed")
  "Workspace event types whose artifact transition Closure validates.
Tagged `:closure artifact' in `superchat-tape-event-type-specs' below;
kept here only as the literal source for that one tagging step.")

(defconst superchat-tape-event-type-specs
  (append
   (mapcar (lambda (type)
             (append
              (list :type type :relevance 'run-scoped)
              (when (member type superchat-tape--run-annotation-event-types)
                (list :closure 'annotation))))
           '("intent.received" "run.started" "response.completed"
             "run.completed" "run.failed" "run.cancelled" "turn.completed"
             "tool.requested" "approval.requested" "approval.decided"
             "tool.execution.started" "tool.completed" "tool.failed"
             "decision.snapshot.recorded" "output.first.recorded"
             "run.steered" "run.steer-undelivered" "delivery.failed"))
   (mapcar (lambda (type) (list :type type :relevance 'configuration))
           '("capability.activated" "topology.changed"
             "authority.policy.selected" "workspace.bound"
             "workspace.unbound" "extension.activated"))
   (mapcar (lambda (type)
             (append
              (list :type type :relevance 'session-annotation)
              (when (member type superchat-tape--artifact-event-types)
                (list :closure 'artifact))))
           '("attachment.added" "anchor.created"
             "memory.captured" "memory.corrected" "memory.raw.added"
             "memory.rejected" "memory.reviewed"
             "plan.proposed" "plan.progress" "plan.approved" "plan.cleared"
             "claim.recorded" "feedback.recorded" "exercise.evaluated"
             "applicability.relation.recorded"
             "candidate.disposition.recorded"
             "experience.candidate-ranking.recorded"
             "experience.structure.recorded"
             "experience.intervention.recorded"
             "synthesis.approval.decided"
             "team.task.created" "team.task.updated"
             "team.message.sent" "team.decision.published"
             "cowork.scope-granted" "digest.scope-granted"
             "triage.scope-granted" "rule.approved" "rule.removed"
             "queue.queued" "queue.dispatched" "queue.dropped"
             "admission.dropped" "edit.applied" "turn.superseded"
             "workspace.artifact.added" "workspace.artifact.changed"
             "workspace.artifact.removed"))
   (mapcar (lambda (suffix)
             (list :type (format "subagent.%s" suffix)
                   :relevance 'session-annotation))
           superchat-tape-subagent-event-suffixes))
  "The one static inventory of Tape event types and their relevance.
Every `:type' Runtime appends through `superchat-tape-append-event' must be
listed here as `(:type STRING :relevance run-scoped|session-annotation
|configuration [:closure lifecycle|annotation|artifact])'.  `:closure' is
optional and, when absent, defaults per `superchat-tape-event-closure-role':
`run-scoped' rows default to `lifecycle', everything else to `annotation'.
It is set explicitly only on the five Run annotations (`annotation') and the
three `workspace.artifact.*' rows (`artifact'), which would otherwise take
the wrong default.  `superchat-closure.el' derives all of its own event-type
lists, including its Closure-role subgroups, from this constant; it keeps no
second inventory.")

(defun superchat-tape--event-type-spec (type)
  "Return the spec for TYPE in `superchat-tape-event-type-specs', or nil."
  (and (stringp type)
       (cl-find type superchat-tape-event-type-specs
                :key (lambda (spec) (plist-get spec :type))
                :test #'equal)))

(defun superchat-tape-event-type-p (type)
  "Return non-nil when TYPE is a known Tape event type."
  (and (superchat-tape--event-type-spec type) t))

(defun superchat-tape-event-relevance (type)
  "Return the relevance of TYPE, or nil when TYPE is unknown."
  (plist-get (superchat-tape--event-type-spec type) :relevance))

(defun superchat-tape-event-closure-role (type)
  "Return TYPE's Closure role, or nil when TYPE is unknown.
The role is one of `lifecycle', `annotation', or `artifact'.  An explicit
`:closure' value on TYPE's spec row wins; otherwise a `run-scoped' TYPE
defaults to `lifecycle' and every other relevance defaults to `annotation'."
  (let ((spec (superchat-tape--event-type-spec type)))
    (when spec
      (or (plist-get spec :closure)
          (if (eq (plist-get spec :relevance) 'run-scoped)
              'lifecycle
            'annotation)))))

(declare-function superchat-db-tape-append
                  "superchat-db"
                  (session-id kind content &rest options))
(declare-function superchat-db-tape-replay
                  "superchat-db"
                  (session-id &optional since-id limit))
(declare-function superchat-db-tape-select
                  "superchat-db" (sql &optional params))

(defun superchat-tape--plist-p (value)
  "Return non-nil when VALUE is a property list.
Total for any Lisp VALUE: an improper or circular list is rejected, not
signaled, by checking `proper-list-p' before measuring or walking it."
  (and (consp value)
       (let ((length (proper-list-p value)))
         (and length
              (cl-evenp length)
              (cl-loop for (key _value) on value by #'cddr
                       always (keywordp key))))))

(defun superchat-tape--alist-p (value)
  "Return non-nil when VALUE is an association list.
Total for any Lisp VALUE, per `superchat-tape--plist-p'.  An alist is a
proper list of cons cells; the key type is not restricted."
  (and (consp value)
       (proper-list-p value)
       (cl-every #'consp value)))

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

(defun superchat-tape-exact-json (value)
  "Return an exact JSON receipt for structured VALUE, or reject it.
Exact evidence must not need Tape's conventional keyed-field redaction: any
such change would make the persisted bytes differ from the observed input."
  (unless (equal value (superchat-tape--redact value))
    (error "Tape exact evidence contains a secret-valued field"))
  (let* ((json (json-encode value))
         (bytes (encode-coding-string json 'utf-8)))
    (list :json json
          :size (string-bytes bytes)
          :hash (concat "sha256:" (secure-hash 'sha256 bytes)))))

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

(defun superchat-tape--recovery-directory ()
  "Return the directory for Runtime events awaiting Tape recovery."
  (expand-file-name "tape-recovery/" superchat-data-directory))

(defun superchat-tape--recovery-path (run-id)
  "Return the recovery file path for RUN-ID."
  (expand-file-name
   (concat (secure-hash 'sha256 run-id) ".json")
   (superchat-tape--recovery-directory)))

(defun superchat-tape-queue-recovery-events (events)
  "Durably queue ordered Runtime EVENTS for later Tape replay.
Only an audit-incomplete Run seal and its Turn seal may use this fallback."
  (unless (and (consp events)
               (equal '("run.failed" "turn.completed")
                      (mapcar (lambda (event) (plist-get event :type))
                              events))
               (cl-every
                (lambda (event)
                  (let ((payload (plist-get event :payload)))
                    (and (memq (plist-get payload :status)
                               '(audit-incomplete "audit-incomplete"))
                         (eq t (plist-get payload :audit-incomplete)))))
                events))
    (error "Tape recovery requires run.failed then turn.completed"))
  (let* ((session-id (plist-get (car events) :session-id))
         (run-id (plist-get (car events) :run-id))
         previous-id
         normalized)
    (unless (and (stringp session-id) (not (string-empty-p session-id))
                 (stringp run-id) (not (string-empty-p run-id))
                 (cl-every
                  (lambda (event)
                    (and (equal session-id (plist-get event :session-id))
                         (equal run-id (plist-get event :run-id))))
                  events))
      (error "Tape recovery events must share Session and Run identity"))
    (dolist (source events)
      (let* ((event (copy-sequence source))
             (event-id (or (plist-get event :event-id)
                           (superchat-tape--new-event-id))))
        (setq event (plist-put event :event-id event-id))
        (setq event (plist-put event :schema 1))
        (setq event
              (plist-put event :payload
                         (superchat-tape--encoded-payload
                          (plist-get event :payload))))
        (when (and previous-id (not (plist-get event :causation-id)))
          (setq event (plist-put event :causation-id previous-id)))
        (setq previous-id event-id)
        (push event normalized)))
    (setq normalized (nreverse normalized))
    (let* ((directory (superchat-tape--recovery-directory))
           (path (superchat-tape--recovery-path run-id))
           temporary)
      (make-directory directory t)
      (set-file-modes directory #o700)
      (setq temporary (make-temp-file (expand-file-name ".pending-" directory)))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-encode (vconcat normalized)))
              (insert "\n"))
            (set-file-modes temporary #o600)
            (rename-file temporary path t)
            (setq temporary nil))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))
    normalized))

(defun superchat-tape--read-recovery-events (path)
  "Read queued recovery events from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)))

(defun superchat-tape--recovery-event-persisted-p (event)
  "Return non-nil when recovery EVENT already exists in SQLite Tape."
  (let ((event-id (plist-get event :event-id)))
    (cl-some
     (lambda (row)
       (equal event-id
              (plist-get (superchat-tape--row-event row) :event-id)))
     (superchat-db-tape-replay (plist-get event :session-id)))))

(defun superchat-tape--reconcile-recovery-events (session-id)
  "Copy queued recovery events for SESSION-ID into SQLite Tape."
  (let ((directory (superchat-tape--recovery-directory)))
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t "\\.json\\'"))
        (let ((events (superchat-tape--read-recovery-events path)))
          (when (equal session-id (plist-get (car events) :session-id))
            ;; ponytail: Audit gaps are rare; retain the tiny sidecar until a
            ;; measured volume justifies a commit-confirmed compactor.
            (dolist (event events)
              (unless (superchat-tape--recovery-event-persisted-p event)
                (superchat-tape-append-event event)))))))))

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
  "Decode one persisted Runtime-event ROW, or return nil for material rows."
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
              (plist-member meta :event_id))))
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
        (when (plist-get meta :historical_opaque)
          (setq event (plist-put event :historical-opaque t)))
        event))))

;;;###autoload
(defun superchat-tape-replay-events (session-id &optional since-id limit)
  "Return normalized Runtime events persisted for SESSION-ID.
SINCE-ID and LIMIT have the same meaning as `superchat-db-tape-replay'."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (signal 'wrong-type-argument (list 'non-empty-string-p session-id)))
  (superchat-tape--reconcile-recovery-events session-id)
  (delq nil
        (mapcar #'superchat-tape--row-event
                (superchat-db-tape-replay session-id since-id limit))))

;;;###autoload
(defun superchat-tape-storage-reference-p (session-id storage-id)
  "Return non-nil when STORAGE-ID belongs to SESSION-ID in Tape."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Tape reference session ID must be a non-empty string"))
  (unless (and (integerp storage-id) (> storage-id 0))
    (error "Tape reference storage ID must be a positive integer"))
  (and (superchat-db-tape-select
        "SELECT 1 FROM tape WHERE id = ? AND session_id = ? LIMIT 1"
        (list storage-id session-id))
       t))

(defun superchat-tape--compaction-source-ids (source-ids)
  "Validate SOURCE-IDS and return them as a unique positive-ID vector."
  (let ((ids (cond
              ((vectorp source-ids) (append source-ids nil))
              ((listp source-ids) source-ids)
              (t nil))))
    (unless (and ids
                 (cl-every (lambda (id) (and (integerp id) (> id 0))) ids)
                 (= (length ids) (length (delete-dups (copy-sequence ids)))))
      (error "Compaction Anchor source_ids must be unique positive integers"))
    (vconcat ids)))

;;;###autoload
(cl-defun superchat-tape-append-compaction-anchor
    (session-id summary source-ids &key message-count)
  "Append a typed compaction Anchor for SOURCE-IDS to SESSION-ID.
Every source must already exist in the same Session; no synthetic or
placeholder provenance is accepted."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Compaction Anchor session ID must be a non-empty string"))
  (unless (and (stringp summary) (not (string-empty-p (string-trim summary))))
    (error "Compaction Anchor summary must be non-empty"))
  (let ((ids (superchat-tape--compaction-source-ids source-ids)))
    (dolist (id (append ids nil))
      (unless (superchat-tape-storage-reference-p session-id id)
        (error "Compaction Anchor source %d is not in Session %s"
               id session-id)))
    (superchat-db-tape-append
     session-id "anchor" summary
     :topic superchat-tape-compaction-anchor-topic
     :meta `((schema . ,superchat-tape-compaction-anchor-schema)
             (anchor_type . "compact")
             (anchor_version . 1)
             (source_session_id . ,session-id)
             (source_ids . ,ids)
             (message_count . ,(or message-count (length ids)))))))

;;;###autoload
(cl-defun superchat-tape-append-material
    (session-id kind content &key meta topic)
  "Append full material CONTENT of legacy KIND to SESSION-ID.
Use this for trusted prompt material such as messages and anchors that must
remain replayable in full.  Runtime event payloads must use
`superchat-tape-append-event' so redaction and size bounds still apply."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Tape material session ID must be a non-empty string"))
  (unless (and (stringp kind) (not (string-empty-p kind)))
    (error "Tape material kind must be a non-empty string"))
  (unless (stringp content)
    (error "Tape material content must be a string"))
  (superchat-db-tape-append session-id kind content :meta meta :topic topic))

(defun superchat-tape--validate-envelope-fields (type session-id event-id schema)
  "Signal an error unless TYPE, SESSION-ID, EVENT-ID and SCHEMA are well-formed."
  (unless (and (stringp type) (not (string-empty-p type)))
    (error "Tape event :type must be a non-empty string"))
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "Tape event :session-id must be a non-empty string"))
  (unless (and (stringp event-id) (not (string-empty-p event-id)))
    (error "Tape event :event-id must be a non-empty string"))
  (unless (and (integerp schema) (> schema 0))
    (error "Tape event :schema must be a positive integer")))

(defun superchat-tape--build-and-persist (event &optional historical-opaque)
  "Encode EVENT's envelope and append it to storage.
When HISTORICAL-OPAQUE is non-nil, mark the row so replay surfaces it as an
opaque annotation instead of a type Closure interprets."
  (let ((type (plist-get event :type))
        (session-id (plist-get event :session-id))
        (event-id (or (plist-get event :event-id)
                      (superchat-tape--new-event-id)))
        (schema (or (plist-get event :schema) 1)))
    (superchat-tape--validate-envelope-fields type session-id event-id schema)
    (let* ((payload
            (superchat-tape--encoded-payload
             (or (plist-get event :payload) '())))
           (meta (list :event_type type
                       :event_id event-id
                       :schema schema
                       :session_id session-id
                       :recorded_at (superchat-tape--recorded-at)
                       :payload payload)))
      (when historical-opaque
        (setq meta (plist-put meta :historical_opaque t)))
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
(defun superchat-tape-append-event (event)
  "Validate and append Runtime EVENT to the existing Tape.

EVENT is a plist with required non-empty string fields `:type' and
`:session-id'.  `:type' must be listed in `superchat-tape-event-type-specs';
an unlisted type signals `superchat-tape-unknown-event-type'.  The payload
must be nil, a plist, or an alist, and a `run-scoped' type additionally
requires non-empty string `:run-id' and `:turn-id'; either failure signals
`superchat-tape-invalid-event'.  Use `superchat-tape-import-event' for rows
whose type is not known to this inventory.

This module assigns the logical `event_id' and `recorded_at'; the database
assigns the numeric storage cursor returned to legacy callers.  Payloads are
redacted and size-limited before reaching SQLite.  Persistence errors are
deliberately propagated to the caller."
  (unless (listp event)
    (signal 'wrong-type-argument (list 'listp event)))
  (let* ((type (plist-get event :type))
         (spec (superchat-tape--event-type-spec type)))
    (unless spec
      (signal 'superchat-tape-unknown-event-type (list type)))
    (let ((payload (plist-get event :payload)))
      (unless (or (null payload)
                  (superchat-tape--plist-p payload)
                  (superchat-tape--alist-p payload))
        (signal 'superchat-tape-invalid-event
                (list type 'invalid-payload-shape))))
    (when (eq (plist-get spec :relevance) 'run-scoped)
      (let ((run-id (plist-get event :run-id))
            (turn-id (plist-get event :turn-id)))
        (unless (and (stringp run-id) (not (string-empty-p run-id))
                     (stringp turn-id) (not (string-empty-p turn-id)))
          (signal 'superchat-tape-invalid-event
                  (list type 'missing-run-or-turn-identity)))))
    (superchat-tape--build-and-persist event)))

;;;###autoload
(defun superchat-tape-import-event (event)
  "Persist historical Runtime EVENT without inventory or validator checks.
Use this for rows whose `:type' is not known to
`superchat-tape-event-type-specs', such as a legacy Tape row replayed under
its original type.  The stored row is marked so `superchat-tape--row-event'
surfaces `:historical-opaque t'; `superchat-closure.el' then treats it as an
opaque annotation rather than interpreting its semantics."
  (unless (listp event)
    (signal 'wrong-type-argument (list 'listp event)))
  (superchat-tape--build-and-persist event t))

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
