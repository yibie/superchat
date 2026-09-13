;;; superchat-tape.el --- Runtime event contract for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; New Runtime events enter Tape through this module.  The existing SQLite
;; schema remains unchanged: `kind' keeps its legacy values while the
;; versioned event envelope is stored in `meta'.  Audit-incomplete terminal
;; seals use a bounded sidecar until replay can reconcile them into SQLite.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
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
    "run.steered" "run.steer-undelivered" "delivery.failed"
    ;; The M3 Step bracket.  A Step states where a Run's work began and how
    ;; that work ended; it opens no Effect, settles no Approval and seals no
    ;; Run, so Closure records it without advancing the Run's state.  Left out
    ;; of this list they would fall to Closure's unknown-event branch and make
    ;; every Run with a Step unprovable.
    "step.started" "step.ended")
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
             ;; M3 Step bracket: one driver-level model attempt and the tool
             ;; calls it produces.  Structural rows, not model-visible
             ;; messages; the accepted M3 record permits exactly these two
             ;; additions to the canonical sequences.
             "step.started" "step.ended"
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
             "prompt.assembled" "request.header" "request.message-list"
              "invariant.violated" "invariant.check-failed"
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

;;; META JSON shaping ---------------------------------------------------------
;; A tape row's META is JSON text, so every payload crosses exactly one
;; `json-encode' / `json-parse-string' round trip.  `json-encode' cannot
;; represent a LIST of property lists: it encodes the whole list as one JSON
;; object, so two plists sharing a key collide and only the last one survives.
;; That silently destroyed a derived source's `:inputs` -- a merge recorded the
;; identity and original source of both inputs, and the durable record came
;; back carrying one -- and it wrote the same kind of damage into every other
;; payload that carries such a list, such as the failure-reason lists in
;; delivery records and the effect lists in tool and approval records.
;;
;; Such a list is therefore encoded under an explicit SINGLE-KEY OBJECT,
;; `{"@superchat/plist-list": [ ... ]}'.  The object shape is what makes the
;; tag unambiguous: a tagged value is a plist whose key is the tag keyword, and
;; a plain list of strings can never be mistaken for it.  (A two-element ARRAY
;; whose first element was the tag string does not work: a plain list that
;; happens to start with that string is then indistinguishable from a tagged
;; value, and reading it back loses every element but the second.  Only the
;; object form is written AND read -- there is no array form to stay
;; compatible with.)  The tag's value IS the original list, so unwrapping is
;; identity and cannot invent a shape.
;;
;; The codec lives here, beside the payload rules it must satisfy, rather than
;; in `superchat-db.el': Tape requires that module, so the storage layer cannot
;; call back into these shape predicates.

(defconst superchat-tape--plist-list-tag :@superchat/plist-list
  "JSON object key marking a value that is a list of property lists.
A keyword, like every other payload key, so `json-encode' renders the tagged
form as an object rather than as an array.")

(defun superchat-tape--plist-list-p (value)
  "Return non-nil when VALUE is a proper non-empty list whose elements are plists.
The empty list is excluded: it carries no element to identify it, and
`json-encode' already round-trips it as an empty array."
  (and (consp value)
       (proper-list-p value)
       (cl-every #'superchat-tape--plist-p value)))

(defun superchat-tape--encode-meta-value (value)
  "Return VALUE as a JSON-encodable form that round-trips exactly.
Only the one shape `json-encode' cannot represent is rewritten.  Two shapes
still do not survive verbatim, exactly as before this codec existed: an alist
reads back as a plist, and a vector reads back as a list.  Neither loses an
entry, and event payloads are plists throughout."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key item)
                 (puthash key (superchat-tape--encode-meta-value item) copy))
               value)
      copy))
   ((vectorp value)
    (apply #'vector (mapcar #'superchat-tape--encode-meta-value (append value nil))))
   ((superchat-tape--plist-list-p value)
    ;; A plist whose single key is the tag keyword, so `json-encode' renders it
    ;; as an object.  The value is the original list as a vector of encoded
    ;; elements; the vector, not a list, is what keeps this pair itself from
    ;; looking like a list of plists to this same rule.
    (list superchat-tape--plist-list-tag
          (apply #'vector (mapcar #'superchat-tape--encode-meta-value value))))
   ((superchat-tape--plist-p value)
    (let (result)
      (while value
        (let ((key (pop value)) (item (pop value)))
          (setq result (nconc result
                              (list key (superchat-tape--encode-meta-value item))))))
      result))
   ((superchat-tape--alist-p value)
    (mapcar (lambda (item)
              (cons (car item) (superchat-tape--encode-meta-value (cdr item))))
            value))
   ((consp value) (mapcar #'superchat-tape--encode-meta-value value))
   (t value)))

(defun superchat-tape--decode-meta-value (value)
  "Return VALUE read back out of a META column, with tags resolved.
An untagged list of plists is the shape rows written before this codec used,
where JSON had already collided their keys.  It is returned as parsed rather
than guessed at: reinterpreting its half-empty plist cannot recover the entry
that was lost when it was written."
  (cond
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash (lambda (key item)
                 (puthash key (superchat-tape--decode-meta-value item) copy))
               value)
      copy))
   ((vectorp value)
    (apply #'vector (mapcar #'superchat-tape--decode-meta-value (append value nil))))
   ((and (superchat-tape--plist-p value)
         (null (cddr value))
         (eq (car value) superchat-tape--plist-list-tag))
    ;; The tagged form: an object whose ONLY key is the tag keyword, holding
    ;; the original list.  Three conditions, each load-bearing.  `plist-p'
    ;; keeps a plain list that merely starts with the tag's name from being
    ;; read as a tag -- only a real object can be one.  `(null (cddr value))'
    ;; requires exactly one key, so an object that happens to carry the tag
    ;; beside other keys keeps all of them instead of silently losing the
    ;; rest.  And the key must be the tag itself.
    (superchat-tape--decode-meta-value
     (plist-get value superchat-tape--plist-list-tag)))
   ((superchat-tape--plist-p value)
    (let (result)
      (while value
        (let ((key (pop value)) (item (pop value)))
          (setq result (nconc result
                              (list key (superchat-tape--decode-meta-value item))))))
      result))
   ((superchat-tape--alist-p value)
    (mapcar (lambda (item)
              (cons (car item) (superchat-tape--decode-meta-value (cdr item))))
            value))
   ((consp value) (mapcar #'superchat-tape--decode-meta-value value))
   (t value)))

(defun superchat-tape--encode-meta (meta)
  "JSON text for the tape META column, shaped by `superchat-tape--encode-meta-value'.
A value the encoder cannot represent at all keeps the previous fallback, an
empty object, because the row's own kind and content still carry the event."
  (if meta
      (condition-case nil
          (json-encode (superchat-tape--encode-meta-value meta))
        (error "{}"))
    "{}"))

(defun superchat-tape--decode-meta (json)
  "Read the tape META column text JSON back into Lisp values."
  (if (or (not (stringp json)) (string-empty-p json))
      nil
    (superchat-tape--decode-meta-value
     (json-parse-string json :object-type 'plist
                        :array-type 'list
                        :null-object nil :false-object nil))))

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

(defun superchat-tape--event-status (event)
  "Return EVENT's payload status as a symbol when it is textual."
  (let ((status (plist-get (plist-get event :payload) :status)))
    (if (stringp status) (intern status) status)))

(defun superchat-tape--terminal-event-p (event)
  "Return non-nil when EVENT is a Run terminal seal."
  (member (plist-get event :type)
          '("run.completed" "run.failed" "run.cancelled")))

(defun superchat-tape--session-events (session-id)
  "Return raw persisted Runtime events for SESSION-ID without reconciliation.

This deliberately bypasses `superchat-tape-replay-events': callers deciding
whether it is safe to append a recovery event must not first cause another
recovery journal to be replayed.  Storage or decoding errors propagate to the
caller, which can then keep absence distinct from unknown."
  (delq nil
        (mapcar #'superchat-tape--row-event
                (superchat-db-tape-replay session-id))))

(defun superchat-tape--find-persisted-event (event)
  "Return EVENT's exact persisted Runtime event, or nil when it is absent.

EVENT supplies its `:session-id' and stable `:event-id'.  A read failure is
not absence and deliberately propagates."
  (let ((event-id (plist-get event :event-id))
        (session-id (plist-get event :session-id)))
    (unless (and (stringp event-id) (not (string-empty-p event-id))
                 (stringp session-id) (not (string-empty-p session-id)))
      (error "Tape exact-event lookup requires Session and event identity"))
    (cl-find event-id (superchat-tape--session-events session-id)
             :key (lambda (candidate) (plist-get candidate :event-id))
             :test #'equal)))

(defun superchat-tape--event-state (event)
  "Return EVENT's persisted state as `present', `absent', or `unknown'.

The returned plist has `:event' for a present state and `:condition' for an
unknown state.  In particular, an unreadable Tape is never represented as an
absent event."
  (condition-case condition
      (if-let* ((persisted (superchat-tape--find-persisted-event event)))
          (list :state 'present :event persisted)
        (list :state 'absent))
    (error (list :state 'unknown :condition condition))))

(defun superchat-tape--run-terminal-state (session-id run-id)
  "Return the unique persisted Run terminal state for SESSION-ID/RUN-ID.

The result is `(:state present :event EVENT)', `(:state absent)', or
`(:state unknown :condition CONDITION)'.  Multiple seals are unknown for
recovery purposes: a later fallback must never add another terminal fact to
an already inconsistent trace."
  (condition-case condition
      (let ((terminals
             (cl-remove-if-not
              (lambda (event)
                (and (equal run-id (plist-get event :run-id))
                     (superchat-tape--terminal-event-p event)))
              (superchat-tape--session-events session-id))))
        (cond
         ((null terminals) (list :state 'absent))
         ((null (cdr terminals)) (list :state 'present :event (car terminals)))
         (t
          (list :state 'unknown
                :condition
                (list 'error
                      (format "Tape has multiple Run terminal seals for %s" run-id))))))
    (error (list :state 'unknown :condition condition))))

(defun superchat-tape--turn-terminal-state (session-id run-id turn-id)
  "Return the unique persisted Turn seal for SESSION-ID/RUN-ID/TURN-ID.

This has the same three-state contract as
`superchat-tape--run-terminal-state'."
  (condition-case condition
      (let ((terminals
             (cl-remove-if-not
              (lambda (event)
                (and (equal "turn.completed" (plist-get event :type))
                     (equal run-id (plist-get event :run-id))
                     (equal turn-id (plist-get event :turn-id))))
              (superchat-tape--session-events session-id))))
        (cond
         ((null terminals) (list :state 'absent))
         ((null (cdr terminals)) (list :state 'present :event (car terminals)))
         (t
          (list :state 'unknown
                :condition
                (list 'error
                      (format "Tape has multiple Turn seals for %s" turn-id))))))
    (error (list :state 'unknown :condition condition))))

(defun superchat-tape--recovery-directory ()
  "Return the directory for Runtime events awaiting Tape recovery."
  (expand-file-name "tape-recovery/" superchat-data-directory))

(defun superchat-tape--recovery-path (run-id)
  "Return the recovery file path for RUN-ID."
  (expand-file-name
   (concat (secure-hash 'sha256 run-id) ".json")
   (superchat-tape--recovery-directory)))

(defun superchat-tape--audit-recovery-events-p (events)
  "Return non-nil when EVENTS are the explicit audit-incomplete pair."
  (and (equal '("run.failed" "turn.completed")
              (mapcar (lambda (event) (plist-get event :type)) events))
       (cl-every
       (lambda (event)
         (let ((payload (plist-get event :payload)))
            (and (member (plist-get payload :status)
                         '(audit-incomplete "audit-incomplete"))
                 (eq t (plist-get payload :audit-incomplete)))))
        events)))

(defun superchat-tape--ordinary-turn-recovery-event-p (events)
  "Return non-nil when EVENTS is one ordinary missing-Turn recovery tail."
  (and (= (length events) 1)
       (let ((event (car events)))
         (and (equal "turn.completed" (plist-get event :type))
              (memq (superchat-tape--event-status event)
                    '(completed cancelled failed timed-out))
              (stringp (plist-get event :event-id))
              (not (string-empty-p (plist-get event :event-id)))
              (stringp (plist-get event :causation-id))
              (not (string-empty-p (plist-get event :causation-id)))))))

(defun superchat-tape-queue-recovery-events (events)
  "Durably queue ordered Runtime EVENTS for later Tape replay.

The protected per-Run sidecar accepts exactly either the explicit
`audit-incomplete' Run/Turn pair or one ordinary `turn.completed' tail.  The
latter can only be reconciled after SQLite confirms the one matching ordinary
Run seal named by its `:causation-id'; it never creates or replaces a Run
terminal."
  (unless (and (consp events)
               (or (superchat-tape--audit-recovery-events-p events)
                   (superchat-tape--ordinary-turn-recovery-event-p events)))
    (error "Tape recovery requires an audit pair or one ordinary Turn tail"))
  (let* ((session-id (plist-get (car events) :session-id))
         (run-id (plist-get (car events) :run-id))
         previous-id
         normalized)
    (unless (and (stringp session-id) (not (string-empty-p session-id))
                 (stringp run-id) (not (string-empty-p run-id))
                 (cl-every
                  (lambda (event)
                    (and (equal session-id (plist-get event :session-id))
                         (equal run-id (plist-get event :run-id))
                         (stringp (plist-get event :turn-id))
                         (not (string-empty-p (plist-get event :turn-id)))))
                  events))
      (error "Tape recovery events must share complete Session/Run/Turn identity"))
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

(defun superchat-tape--recovery-signal-unknown (state context)
  "Re-signal STATE's storage uncertainty while reconciling CONTEXT."
  (let ((condition (plist-get state :condition)))
    (if (and (consp condition) (symbolp (car condition)))
        (signal (car condition) (cdr condition))
      (error "Tape recovery cannot confirm %s" context))))

(defun superchat-tape--reconcile-ordinary-turn-recovery (event)
  "Append ordinary missing Turn EVENT only after its Run seal is confirmed."
  ;; Recheck both seals even when EVENT's stable ID is already present.  A
  ;; repeated replay must be idempotent, but it must not silently bless a
  ;; later conflicting Run/Turn history.
  (let ((run-state
         (superchat-tape--run-terminal-state
          (plist-get event :session-id) (plist-get event :run-id))))
    (pcase (plist-get run-state :state)
      ('unknown
       (superchat-tape--recovery-signal-unknown run-state "Run terminal"))
      ('present
       (let ((run-event (plist-get run-state :event))
             (status (superchat-tape--event-status event)))
         (when (and (memq status '(completed cancelled failed timed-out))
                    (equal (plist-get run-event :event-id)
                           (plist-get event :causation-id))
                    (equal (plist-get run-event :session-id)
                           (plist-get event :session-id))
                    (equal (plist-get run-event :run-id)
                           (plist-get event :run-id))
                    (equal (plist-get run-event :turn-id)
                           (plist-get event :turn-id))
                    (eq status (superchat-tape--event-status run-event))
                    (equal (plist-get run-event :type)
                           (pcase status
                             ('completed "run.completed")
                             ('cancelled "run.cancelled")
                             (_ "run.failed"))))
         (let ((turn-state
                (superchat-tape--turn-terminal-state
                 (plist-get event :session-id)
                 (plist-get event :run-id)
                 (plist-get event :turn-id))))
           (pcase (plist-get turn-state :state)
             ('unknown
              (superchat-tape--recovery-signal-unknown turn-state "Turn terminal"))
             ('absent (superchat-tape-append-event event))
             ;; A matching already-persisted tail is the normal idempotent
             ;; case.  A different existing Turn is equally not ours to add.
             ('present nil))))))
      ;; Absence is not permission to manufacture the ordinary Run seal.
      ('absent nil))))

(defun superchat-tape--reconcile-audit-recovery (events)
  "Reconcile one explicit audit recovery pair without competing seals."
  (let* ((run-event (car events))
         (turn-event (cadr events))
         ;; Do not let an exact old audit ID hide any other Run terminal.
         ;; The unique per-Run query is the proof that this sidecar still owns
         ;; the Run before either member of its pair is replayed.
         (run-state (superchat-tape--run-terminal-state
                     (plist-get run-event :session-id)
                     (plist-get run-event :run-id)))
         run-confirmed)
    (pcase (plist-get run-state :state)
      ('unknown
       (superchat-tape--recovery-signal-unknown run-state "audit Run seal"))
      ('present
       (let ((persisted (plist-get run-state :event)))
         (setq run-confirmed
               (and (equal (plist-get persisted :event-id)
                           (plist-get run-event :event-id))
                    (equal "run.failed" (plist-get persisted :type))
                    (eq 'audit-incomplete
                        (superchat-tape--event-status persisted))
                    (eq t (plist-get (plist-get persisted :payload)
                                     :audit-incomplete))))))
      ('absent
       ;; A receipt can fail after INSERT.  If this append signals, leave the
       ;; sidecar intact; its stable ID and the unique query decide next replay.
       (superchat-tape-append-event run-event)
       (setq run-confirmed t)))
    (when run-confirmed
      (let ((turn-state (superchat-tape--event-state turn-event)))
        (pcase (plist-get turn-state :state)
          ('unknown
           (superchat-tape--recovery-signal-unknown turn-state "audit Turn seal"))
          ('absent
           (let ((existing-turn
                  (superchat-tape--turn-terminal-state
                   (plist-get turn-event :session-id)
                   (plist-get turn-event :run-id)
                   (plist-get turn-event :turn-id))))
             (pcase (plist-get existing-turn :state)
               ('unknown
                (superchat-tape--recovery-signal-unknown
                 existing-turn "Turn terminal before audit recovery"))
               ('absent (superchat-tape-append-event turn-event))
               ('present nil))))
          ('present nil))))))

(defun superchat-tape--reconcile-recovery-events (session-id)
  "Copy queued recovery events for SESSION-ID into SQLite Tape."
  (let ((directory (superchat-tape--recovery-directory)))
    (when (file-directory-p directory)
      (dolist (path (directory-files directory t "\\.json\\'"))
        (let ((events (superchat-tape--read-recovery-events path)))
          (when (equal session-id (plist-get (car events) :session-id))
            ;; ponytail: Audit gaps are rare; retain the tiny sidecar until a
            ;; measured volume justifies a commit-confirmed compactor.
            (cond
             ((superchat-tape--audit-recovery-events-p events)
              (superchat-tape--reconcile-audit-recovery events))
             ((superchat-tape--ordinary-turn-recovery-event-p events)
              (superchat-tape--reconcile-ordinary-turn-recovery (car events)))
             (t
              (error "Invalid Tape recovery journal for Session %s" session-id)))))))))

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
         ;; The META column is JSON text shaped by `superchat-tape--decode-meta',
         ;; which restores the one shape `json-encode' cannot carry: a list of
         ;; property lists, such as a derived source's `:inputs'.
         (meta
          (condition-case err
              (superchat-tape--decode-meta (nth 4 row))
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
     :meta (superchat-tape--encode-meta
            `((schema . ,superchat-tape-compaction-anchor-schema)
              (anchor_type . "compact")
              (anchor_version . 1)
              (source_session_id . ,session-id)
              (source_ids . ,ids)
              (message_count . ,(or message-count (length ids))))))))

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
  (superchat-db-tape-append session-id kind content
                            :meta (superchat-tape--encode-meta meta)
                            :topic topic))

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
       :meta (superchat-tape--encode-meta meta)))))

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

;; ── Plugin (spec §3): the default provider defined next to its code ──
(declare-function superchat-projection-register "superchat-projection")
(declare-function superchat-projection-replay "superchat-projection")
(declare-function superchat-projection-catch-up "superchat-projection")
(superchat-define-plugin tape
  :doc "The append-only Tape and its projections."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'tape
     (superchat-service-table
      append-event #'superchat-tape-append-event
      append-event-record #'superchat-tape-append-event-record
      append-material (lambda (&rest args) (apply #'superchat-tape-append-material args))
      append-anchor (lambda (&rest args) (apply #'superchat-tape-append-compaction-anchor args))
      replay #'superchat-tape-replay-events
      register-projection (lambda (&rest args) (apply #'superchat-projection-register args))
      replay-projection #'superchat-projection-replay
      catch-up-projection #'superchat-projection-catch-up
      storage-reference-p #'superchat-tape-storage-reference-p
      exact-json #'superchat-tape-exact-json
      select #'superchat-db-tape-select))))

;;; ── Request evidence (M1): what the provider was actually sent ─────────────
;;
;; M1's acceptance requires an independent reconstruction of the
;; provider-visible system text, messages and tool schemas.  A `request.header'
;; row carries the system text, the tool schemas and the call parameters; a
;; `request.message-list' row carries the composition of one request's message
;; list as storage references plus the presentation receipt, never a second copy
;; of the message bodies.  A header is written only when the series it belongs
;; to changes, so a tool-call loop references one header instead of restating
;; the schemas on every step.

(defconst superchat-tape-request-header-schema 1
  "Schema version of the `request.header' payload.")
(defconst superchat-tape-request-message-list-schema 1
  "Schema version of the `request.message-list' payload.")
(defconst superchat-tape-request-template-version 1
  "Version of the continuity rendering the message list was composed under.
A reconstruction that does not know this version reports `unknown' rather than
comparing against the renderer it happens to have.")

(defun superchat-tape--request-digest (system tools model)
  "Return a stable digest of the header inputs SYSTEM, TOOLS and MODEL."
  (concat "sha256:"
          (secure-hash
           'sha256
           (encode-coding-string
            (json-encode (list :system (or system "")
                               :tools (or tools [])
                               :model (or model "")))
            'utf-8))))

(defun superchat-tape-request-header (request-id series-id reason system tools
                                                 &optional model)
  "Return the `request.header' event plist for one provider request series.
REQUEST-ID is the request that caused the write; SERIES-ID names the series;
REASON is `first', `resume', `change' or `new-series'.  SYSTEM is the complete
system text, TOOLS the tool schemas as the provider receives them, and MODEL
the target model with the call parameters that shape the request."
  (list :type "request.header"
        :payload (list :request-id request-id
                       :series-id series-id
                       :reason (symbol-name reason)
                       :schema superchat-tape-request-header-schema
                       :template-version superchat-tape-request-template-version
                       :system (or system "")
                       :tools (or tools [])
                       :model (or model "")
                       :header-digest
                       (superchat-tape--request-digest system tools model))))

(defun superchat-tape-request-message-list (request-id series-id header-storage-id
                                                       items receipt &optional execution)
  "Return the `request.message-list' event plist for one provider request.
ITEMS is the ordered provider-visible message list as composition, not
content: `(:kind KIND :storage-id ID [:source-ids IDS] [:lossy t])' per entry.
RECEIPT is the presentation receipt the provider received.  HEADER-STORAGE-ID
names the `request.header' row this request belongs to, so a reader that
cannot resolve it reports `unknown' instead of borrowing a neighbour's header.
EXECUTION optionally links this attempt to its Run, Turn and Step."
  (list :type "request.message-list"
        :payload (list :request-id request-id
                       :series-id series-id
                       :header-storage-id header-storage-id
                       :schema superchat-tape-request-message-list-schema
                       :template-version superchat-tape-request-template-version
                       :items (vconcat items)
                       :receipt receipt
                       :execution (copy-tree execution))))

(defun superchat-tape-request-series-reason (previous-digest series-id digest
                                                             previous-series
                                                             &optional resumed)
  "Return the reason a header must be written, or nil when none is needed.
PREVIOUS-DIGEST and PREVIOUS-SERIES are the recorded state; DIGEST is the
header digest about to be used and SERIES-ID the series it belongs to.
RESUMED is the caller's statement that the series continues across a boundary
a reader cannot be assumed to have followed, which is the one reason only the
caller can supply."
  (cond
   (resumed 'resume)
   ((null previous-digest) 'first)
   ((not (equal series-id previous-series)) 'new-series)
   ((not (equal digest previous-digest)) 'change)
   (t nil)))

(defun superchat-tape--request-headers (session-id)
  "Return this Session's `request.header' events in Tape order."
  (cl-remove-if-not
   (lambda (event) (equal "request.header" (plist-get event :type)))
   (superchat-tape-replay-events session-id)))

(defun superchat-tape-request-record (session-id request-id system tools model
                                                 message-items receipt &optional resumed execution)
  "Record one provider request's evidence on the Tape.
Appends a `request.header' row when the series it belongs to changed and a
`request.message-list' row for this request, returning the header's storage id.
SESSION-ID scopes the rows and REQUEST-ID names the request that caused them;
SYSTEM is the provider-visible system text, TOOLS the provider-visible tool
schemas, MODEL the target model with the call parameters, MESSAGE-ITEMS the
ordered message composition and RECEIPT the presentation receipt the provider
received.

Both rows remain Session annotations.  The message-list payload optionally
carries EXECUTION coordinates linking an attempt to its Run, Turn and Step;
this does not change the reusable header's series identity.  The series state
is read from the newest `request.header' already on the Tape, so a restart
resumes from recorded evidence instead of live memory.  RESUMED is the caller's
statement that the series crosses a boundary a reader cannot be assumed to have
followed.

Returns nil when SESSION-ID is unusable or the Tape refuses an append, so a
failed recording never breaks the request it describes."
  (when (and (stringp session-id) (not (string-empty-p session-id)))
    (condition-case nil
        (let* ((previous (car (last (superchat-tape--request-headers session-id))))
               (previous-payload (plist-get previous :payload))
               (previous-digest (plist-get previous-payload :header-digest))
               (previous-series (plist-get previous-payload :series-id))
               (previous-header-id (and previous (plist-get previous :storage-id)))
               (series-id (or previous-series (format "series-%s" session-id)))
               (digest (superchat-tape--request-digest system tools model))
               (reason (superchat-tape-request-series-reason
                        previous-digest series-id digest previous-series resumed))
               (header-id
                (if (null reason)
                    previous-header-id
                  (superchat-tape-append-event
                   (append (superchat-tape-request-header
                            (or request-id "request") series-id reason
                            system tools model)
                           (list :session-id session-id
                                 :actor-id "kernel"
                                 :summary (format "request.header: %s" series-id)))))))
          (when header-id
            (superchat-tape-append-event
             (append (superchat-tape-request-message-list
                      (or request-id "request") series-id header-id
                      message-items receipt execution)
                     (list :session-id session-id
                           :actor-id "kernel"
                           :summary
                           (format "request.message-list: %s" series-id)))))
          header-id)
      (error nil))))

(defun superchat-tape-request-evidence (session-id request-id)
  "Rebuild REQUEST-ID's provider request from the Tape, or report why not.
Returns `(:status reproduced :system S :messages M :tools T)' when the header
and every named storage id resolve, and `(:status unknown :reason R)' otherwise.
A reconstruction never guesses: an unresolvable header reference, a message row
that no longer exists, or a header whose payload was bounded away reports
`unknown' instead of comparing against a shorter request.

This is the check the accepted request-header decision requires; a recorded
`prompt.assembled' receipt alone is not evidence that a request can be rebuilt."
  (let* ((events (superchat-tape-replay-events session-id))
         (lists (cl-remove-if-not
                 (lambda (event)
                   (and (equal "request.message-list" (plist-get event :type))
                        (equal request-id
                               (plist-get (plist-get event :payload) :request-id))))
                 events))
         (row-at (lambda (storage-id)
                   (car (superchat-db-tape-select
                         "SELECT id, topic, kind, content FROM tape
                          WHERE id = ? AND session_id = ? LIMIT 1"
                         (list storage-id session-id))))))
    (cond
     ((null lists) (list :status 'unknown :reason "no message-list row for request"))
     (t
      (let* ((payload (plist-get (car (last lists)) :payload))
             (header-id (plist-get payload :header-storage-id))
             (header-events (superchat-tape--request-headers session-id))
             (header (cl-find header-id header-events
                              :key (lambda (event) (plist-get event :storage-id))))
             (header-payload (and header (plist-get header :payload)))
             (items (append (plist-get payload :items) nil)))
        (cond
         ((null header)
          (list :status 'unknown
                :reason (format "header %s does not resolve" header-id)))
         ;; A recorded header always carries its schema marker; a payload
         ;; bounded away by `superchat-tape--bound-payload' keeps only
         ;; `:omitted'/`:size'/`:content_hash', so the marker separates
         ;; "recorded with no tools" from "not recorded at all".
         ((or (plist-get header-payload :schema)
              (plist-get header-payload :template-version))
          (let* ((rows (mapcar (lambda (item)
                                 (funcall row-at (plist-get item :storage-id)))
                               items))
                 (missing (cl-find-if #'null rows)))
            (if missing
                (list :status 'unknown
                      :reason "a named storage id does not resolve")
              (list :status 'reproduced
                    :system (plist-get header-payload :system)
                    :tools (plist-get header-payload :tools)
                    :messages (mapcar (lambda (row)
                                        (list :kind (nth 2 row)
                                              :content (nth 3 row)))
                                      rows)
                    :receipt (plist-get payload :receipt)
                    :template-version (plist-get payload :template-version)))))
         (t
          (list :status 'unknown
                :reason "header payload was bounded away; schemas not recorded"))))))))

(provide 'superchat-tape)

;;; superchat-tape.el ends here
