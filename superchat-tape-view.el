;;; superchat-tape-view.el — Tape views for Superchat -*- lexical-binding: t; -*-

;;; Commentary:

;; View functions over the SQLite tape table.  Views are runtime
;; collections of entries; they do not introduce new storage structures.
;; This follows the tape.systems primitive model:
;;   Entry (immutable) → Anchor (marked distillation) → View (collection).

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'json)
(require 'superchat-projection)
(require 'superchat-tape)

(declare-function superchat-db-tape-select  "superchat-db" (sql &optional params))
(declare-function superchat-db-tape-search   "superchat-db" (query &optional session-id limit))
(declare-function superchat-db-tape-replay   "superchat-db" (session-id &optional since-id limit))
(declare-function superchat-db-tape-last-anchor "superchat-db" (session-id))

;; ═══════════════════════════════════════════════════════════
;; Low-level view constructors
;; ═══════════════════════════════════════════════════════════

(defun superchat-view-entries (session-id &optional since-id kinds limit max-id)
  "Return tape entries for SESSION-ID.
SINCE-ID restricts to entries with id > SINCE-ID.
KINDS is a list of kinds to include (e.g. '(\"user\" \"anchor\")).
LIMIT caps the result count.
MAX-ID restricts to entries with id <= MAX-ID; a non-integer non-nil
MAX-ID signals, as the Lisp-side id comparison it replaces did."
  (cl-check-type max-id (or null integer))
  (let* ((since-clause (when since-id "AND id > ?"))
         (max-clause (when max-id "AND id <= ?"))
         (kind-clause
          (when kinds
            (format "AND kind IN (%s)"
                    (mapconcat (lambda (_kind) "?") kinds ","))))
         (limit-clause (when limit (format "LIMIT %d" limit)))
         (sql (format "SELECT id, topic, kind, content, meta, created_at
                       FROM tape
                       WHERE session_id = ? %s %s %s
                       ORDER BY id ASC
                       %s"
                      (or since-clause "")
                      (or max-clause "")
                      (or kind-clause "")
                      (or limit-clause "")))
         (params (append (list session-id)
                         (when since-id (list since-id))
                         (when max-id (list max-id))
                         kinds)))
    (superchat-db-tape-select sql params)))

(defun superchat-view-anchor-latest (session-id)
  "Return the latest anchor entry for SESSION-ID, or nil."
  (superchat-db-tape-last-anchor session-id))

(defun superchat-view-session-ids ()
  "Return known Tape session ids, most recently active first."
  (mapcar
   #'car
   (superchat-db-tape-select
    "SELECT session_id, MAX(id) AS last_id
     FROM tape
     GROUP BY session_id
     ORDER BY last_id DESC")))

(defun superchat-view-workspace-session-ids (workspace-id)
  "Return the Sessions bound to WORKSPACE-ID, most recently active first.
A Session counts when the last `workspace.bound' or `workspace.unbound' fact
on its Tape binds it to exactly WORKSPACE-ID; the id is compared whole.
Activity is the Session's newest Tape row of any kind, so a Workspace's
main conversation is the first element and no pointer to it is stored.
Nil when WORKSPACE-ID is not a non-empty string."
  (when (and (stringp workspace-id) (not (string-empty-p workspace-id)))
    (let ((bound-to (make-hash-table :test #'equal))
          sessions)
      (dolist (row (superchat-db-tape-select
                    "SELECT session_id, meta
                     FROM tape
                     WHERE kind = 'system'
                       AND (meta LIKE '%workspace.bound%'
                            OR meta LIKE '%workspace.unbound%')
                     ORDER BY id ASC"))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol)
               (meta (ignore-errors
                       (json-read-from-string (or (nth 1 row) ""))))
               (type (superchat-view--object-get meta '(event_type event-type)))
               (payload (superchat-view--object-get meta '(payload))))
          (pcase type
            ("workspace.bound"
             (puthash (nth 0 row)
                      (superchat-view--object-get
                       payload '(workspace_id workspace-id id))
                      bound-to))
            ("workspace.unbound"
             (puthash (nth 0 row) nil bound-to)))))
      (maphash (lambda (session id)
                 (when (equal id workspace-id)
                   (push session sessions)))
               bound-to)
      (when sessions
        (mapcar
         #'car
         (superchat-db-tape-select
          (format "SELECT session_id, MAX(id) AS last_id
                   FROM tape
                   WHERE session_id IN (%s)
                   GROUP BY session_id
                   ORDER BY last_id DESC"
                  (mapconcat (lambda (_) "?") sessions ","))
          sessions))))))

;; `superchat-view-session-configuration' is defined further down this file.
(defun superchat-view-recall-session-ids (session-id)
  "Return the Sessions a search made from SESSION-ID may look in.
For a Session bound to a Workspace with a root, those are the Sessions bound
to that Workspace, SESSION-ID among them: recall is shared inside a Workspace
and never crosses to another (contract R5).  Any other Session -- a temporary
conversation, or one from before Workspaces -- gets only itself.  Nil when
SESSION-ID is not a non-empty string."
  (when (and (stringp session-id) (not (string-empty-p session-id)))
    (let* ((config (superchat-view-session-configuration session-id))
           (workspace-id (plist-get config :workspace-id))
           (root (plist-get config :workspace-root)))
      (if (and (stringp workspace-id) (stringp root) (not (string-empty-p root)))
          (cl-adjoin session-id
                     (superchat-view-workspace-session-ids workspace-id)
                     :test #'equal)
        (list session-id)))))

(defun superchat-view--branch-parent (rows)
  "Return (SESSION-ID . TURN-ID) from the branch anchor in ROWS."
  (when-let* ((anchor (cl-find-if
                       (lambda (row)
                         (and (string= (nth 1 row) "branch")
                              (string= (nth 2 row) "anchor")))
                       rows))
              (meta (let ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-key-type 'symbol))
                      (ignore-errors
                        (json-read-from-string (or (nth 4 anchor) "{}")))))
              (payload (or (superchat-view--object-get meta '(payload))
                           meta))
              (parent-session
               (superchat-view--object-get payload '(parent_session_id)))
              (parent-turn
               (superchat-view--object-get payload '(parent_turn_id))))
    (cons parent-session parent-turn)))

(defun superchat-view--superseded-ids-in-session (session-id)
  "Return SESSION-ID's own `turn.superseded' storage ids.
Reads only this Session's System rows.  A row that is not a valid
`turn.superseded' event -- unparseable meta, or a payload field that is
not a sequence of positive integer storage ids -- is skipped instead of
failing the scan; a failed Tape read still signals."
  (let (ids)
    (dolist (row (superchat-view-entries session-id nil '("system")))
      (let* ((meta (ignore-errors
                     (json-parse-string
                      (or (nth 4 row) "")
                      :object-type 'plist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)))
             (payload (and meta (plist-get meta :payload)))
             (named (and (equal "turn.superseded" (plist-get meta :event_type))
                         (listp payload)
                         (plist-get payload :superseded-storage-ids))))
        (when (and (or (vectorp named) (listp named))
                   (cl-every (lambda (id) (and (integerp id) (> id 0)))
                             (append named nil)))
          (setq ids (append named ids)))))
    (delete-dups ids)))

(defun superchat-view-superseded-storage-ids (session-id)
  "Return every replaced storage id of SESSION-ID and its parent Sessions.
The single supersession Projection: commands, View rebuilds, prompt
collection and assembly all read it.  Storage ids are globally
addressable, so every lineage segment's own `turn.superseded' ids are
unioned.  One malformed event row is skipped; a failed Tape read (or a
cyclic lineage) signals the caller instead of reporting no exclusions."
  (let (ids)
    (dolist (segment (superchat-view--continuity-lineage-segments session-id))
      (setq ids (append (superchat-view--superseded-ids-in-session
                         (plist-get segment :session-id))
                        ids)))
    (delete-dups ids)))

(cl-defun superchat-view-session-history
    (session-id &optional max-id seen &key excluded-storage-ids)
  "Return SESSION-ID history, following branch lineage through MAX-ID.
SEEN is an internal cycle guard.  EXCLUDED-STORAGE-IDS names rows this
View must hide; when omitted it defaults to
`superchat-view-superseded-storage-ids' for the whole lineage, so a
rebuilt View hides replaced turns."
  (when (member session-id seen)
    (error "Cyclic Superchat session lineage at %s" session-id))
  (let* ((excluded (or excluded-storage-ids
                       (superchat-view-superseded-storage-ids session-id)))
         (rows (superchat-view-entries session-id nil nil nil max-id))
         (rows (if excluded
                   (cl-remove-if
                    (lambda (row) (memql (nth 0 row) excluded))
                    rows)
                 rows))
         (parent (superchat-view--branch-parent rows)))
    (if parent
        (append
         (superchat-view-session-history
          (car parent) (cdr parent) (cons session-id seen)
          :excluded-storage-ids excluded)
         rows)
      rows)))

(defun superchat-view--object-get (object keys)
  "Return the first value in OBJECT matching one of KEYS."
  (catch 'found
    (dolist (key keys)
      (let ((value
             (cond
              ((hash-table-p object)
               (or (gethash key object)
                   (gethash (symbol-name key) object)))
              ((and (listp object) (keywordp (car object)))
               (or (plist-get object key)
                   (plist-get object
                              (intern (concat ":" (symbol-name key))))))
              ((listp object)
               (cdr (or (assq key object)
                        (assoc (symbol-name key) object))))
              (t nil))))
        (when value
          (throw 'found value))))
    nil))

(defun superchat-view--session-config-initial ()
  "Return the default Session configuration projection."
  (list :capability "chat"
        :capability-source nil
        :topology "single"
        :workspace-id nil
        :workspace-root nil
        :surface nil))

(defun superchat-view--session-config-apply (config row)
  "Apply Tape ROW to Session CONFIG and return CONFIG."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (meta (ignore-errors
                 (json-read-from-string (or (nth 4 row) ""))))
         (type (superchat-view--object-get meta '(event_type event-type)))
         (payload (superchat-view--object-get meta '(payload))))
    (pcase type
      ("capability.activated"
       (when-let* ((capability
                    (superchat-view--object-get payload '(capability name))))
         (setq config (plist-put config :capability capability))
         (let ((profile-id
                (superchat-view--object-get
                 payload '(profile_id profile-id))))
           (if profile-id
               (setq config (plist-put config :profile-id profile-id))
             (cl-remf config :profile-id)))
         (setq config
               (plist-put
                config :capability-source
                (superchat-view--object-get
                 payload '(capability_source capability-source source))))))
      ("topology.changed"
       (when-let* ((topology
                    (superchat-view--object-get payload '(topology))))
         (setq config (plist-put config :topology topology))))
      ("workspace.bound"
       (when-let* ((workspace-id
                    (superchat-view--object-get
                     payload '(workspace_id workspace-id id))))
         (setq config (plist-put config :workspace-id workspace-id))
         (setq config
               (plist-put
                config :workspace-root
                (superchat-view--object-get
                 payload '(workspace_root workspace-root root))))))
      ("workspace.unbound"
       (setq config (plist-put config :workspace-id nil))
       (setq config (plist-put config :workspace-root nil)))
      ;; Permission levels.  The keys are absent until a Session selects a
      ;; value, so the effective value falls back to the configured default.
      ("permission.level"
       (when-let* ((level (superchat-view--object-get payload '(level))))
         (setq config (plist-put config :permission-level level))))
      ("sandbox.mode"
       (when-let* ((mode (superchat-view--object-get payload '(mode))))
         (setq config (plist-put config :sandbox-mode mode))))
      ("approval.policy"
       (when-let* ((policy (superchat-view--object-get payload '(policy))))
         (setq config (plist-put config :approval-policy policy))))
      ("intent.received"
       (when-let* ((surface
                    (superchat-view--object-get payload '(surface))))
         (setq config (plist-put config :surface surface)))))
    config))


(defun superchat-view-session-configuration (session-id)
  "Replay SESSION-ID history into the latest runtime configuration plist."
  (superchat-projection-replay
   'session-configuration
   (superchat-view-session-history session-id)))

(defun superchat-view--row-meta (row)
  "Decode metadata from a six- or five-column Tape ROW."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (condition-case nil
        (json-read-from-string
         (or (nth (if (= (length row) 5) 3 4) row) "{}"))
      (error nil))))

(defconst superchat-view--compact-anchor-type "compact"
  "Metadata type for a Context Continuity compact Anchor.")

(defconst superchat-view--compact-anchor-version 1
  "Metadata version for a Context Continuity compact Anchor.")

(defun superchat-view--compact-anchor-row-p (row)
  "Return non-nil when ROW has the typed compact Anchor schema."
  (let* ((meta (superchat-view--row-meta row))
         (anchor-type (superchat-view--object-get meta '(anchor_type)))
         (version (superchat-view--object-get meta '(anchor_version))))
    (and meta
         (equal superchat-view--compact-anchor-type anchor-type)
         (equal superchat-view--compact-anchor-version version)
         (or (= (length row) 5)
             (string= (nth 2 row) "anchor")))))

(defun superchat-view--compact-anchor-row-source-ids (row)
  "Return valid typed compact source IDs from ROW, or nil."
  (when (superchat-view--compact-anchor-row-p row)
    (let* ((meta (superchat-view--row-meta row))
           (source-ids (superchat-view--object-get meta '(source_ids)))
           (ids (and (vectorp source-ids) (append source-ids nil))))
      (when (and ids
                 (cl-every (lambda (id) (and (integerp id) (> id 0))) ids)
                 (= (length ids) (length (delete-dups (copy-sequence ids)))))
        source-ids))))

(defun superchat-view-compaction-anchor-metadata (anchor-row)
  "Return decoded metadata for a typed compaction ANCHOR-ROW."
  (superchat-view--row-meta anchor-row))

(defun superchat-view-compaction-anchor-source-ids (anchor-row)
  "Return validated typed compaction source IDs from ANCHOR-ROW."
  (superchat-view--compact-anchor-row-source-ids anchor-row))

(defun superchat-view--continuity-pair-row-ids (records)
  "Return the storage ids of every row in a complete tool pair among RECORDS.
Computed once per Anchor check, so validating N sources pairs them once
rather than N times."
  (cl-loop for pair in (superchat-view--continuity-tool-observation-pairs records)
           append (list (superchat-view--continuity-record-id
                         (plist-get pair :requested))
                        (superchat-view--continuity-record-id
                         (plist-get pair :outcome)))))

(defun superchat-view--continuity-anchor-source-valid-p
    (record pair-row-ids anchor-id)
  "Return non-nil when RECORD may be a source of an Anchor.
A material record is valid when it is older than ANCHOR-ID; a tool row is
valid when it is older and belongs to a complete requested+outcome pair among
the Anchor's sources (PAIR-ROW-IDS, from
`superchat-view--continuity-pair-row-ids'), so an Anchor covers the
observation as one fact."
  (and (< (superchat-view--continuity-record-id record) anchor-id)
       (or (superchat-view--continuity-material-record-p record)
           (memql (superchat-view--continuity-record-id record)
                  pair-row-ids))))

(defun superchat-view--compaction-anchor-valid-p
  (session-id row &optional all-rows)
  "Return non-nil when ROW has complete same-Session provenance."
  (let* ((rows (or all-rows (superchat-view-entries session-id)))
         (anchor-id (nth 0 row))
         (meta (superchat-view--row-meta row))
         (source-session
          (superchat-view--object-get meta '(source_session_id)))
         (source-ids (superchat-view--compact-anchor-row-source-ids row))
         (ids (and source-ids (append source-ids nil)))
         (sources
          (and ids
               (mapcar (lambda (id) (cl-find id rows :key #'car)) ids)))
         (source-records
          (delq nil
                (mapcar (lambda (source)
                          (and source
                               (superchat-view--continuity-record
                                session-id source)))
                        sources)))
         (pair-row-ids
          (superchat-view--continuity-pair-row-ids source-records)))
    (and (superchat-view--compact-anchor-row-p row)
         (or (null source-session) (equal source-session session-id))
         (= (length sources) (length ids))
         (cl-every
          (lambda (source)
            (and source
                 (superchat-view--continuity-anchor-source-valid-p
                  (superchat-view--continuity-record session-id source)
                  pair-row-ids anchor-id)))
          sources))))

(defun superchat-view-compaction-anchor-latest (session-id)
  "Return the latest typed compaction Anchor for SESSION-ID when valid."
  (let* ((rows (superchat-view-entries session-id))
         (latest
          (car (last
                (cl-remove-if-not
                 #'superchat-view--compact-anchor-row-p
                 rows)))))
    (and latest
         (superchat-view--compaction-anchor-valid-p
          session-id latest rows)
         latest)))

(defun superchat-view-expand-anchor (session-id anchor-row)
  "Return the exact raw entries that typed compact ANCHOR-ROW distills.
Malformed, foreign, missing, and non-material provenance returns nil."
  (when anchor-row
    (let* ((anchor-id (nth 0 anchor-row))
           (all-rows (superchat-view-entries session-id))
           (stored-anchor
            (cl-find-if (lambda (row)
                          (and (= (nth 0 row) anchor-id)
                               (string= (nth 2 row) "anchor")))
                        all-rows))
           (ids
            (and stored-anchor
                 (superchat-view--compaction-anchor-valid-p
                  session-id stored-anchor all-rows)
                 (append
                  (superchat-view--compact-anchor-row-source-ids stored-anchor)
                  nil)))
           (source-rows
            (and ids
                 (mapcar (lambda (id)
                           (cl-find id all-rows :key #'car))
                         ids))))
      (when source-rows
        (sort source-rows
              (lambda (left right) (< (nth 0 left) (nth 0 right))))))))

;; ═══════════════════════════════════════════════════════════
;; Semantic and structured views
;; ═══════════════════════════════════════════════════════════

(defun superchat-view-search (query &optional session-id limit)
  "Full-text search QUERY across tape.
If SESSION-ID is non-nil, restrict to that session."
  (superchat-db-tape-search query session-id (or limit 20)))

(defun superchat-view-file-history (path &optional session-id limit)
  "Return tool_call/tool_result entries mentioning PATH.
If SESSION-ID is nil, search across all sessions."
  (let* ((like-pat (concat "%" path "%"))
         (base-sql
          "SELECT id, session_id, topic, kind, content, meta, created_at
           FROM tape
           WHERE kind IN ('tool_call', 'tool_result')
             AND (content LIKE ? OR meta LIKE ?)")
         (sql (if session-id
                  (concat base-sql " AND session_id = ? ORDER BY id DESC LIMIT ?")
                (concat base-sql " ORDER BY id DESC LIMIT ?")))
         (params (if session-id
                     (list like-pat like-pat session-id (or limit 20))
                   (list like-pat like-pat (or limit 20)))))
    (superchat-db-tape-select sql params)))

(defun superchat-view-tool-history (tool-name &optional session-id limit)
  "Return entries for TOOL-NAME.
If SESSION-ID is nil, search across all sessions."
  (let* ((meta-pat (concat "%\"name\":\"" tool-name "\"%"))
         (content-pat (concat "%" tool-name "%"))
         (base-sql
          "SELECT id, session_id, topic, kind, content, meta, created_at
           FROM tape
           WHERE kind IN ('tool_call', 'tool_result')
             AND (meta LIKE ? OR content LIKE ?)")
         (sql (if session-id
                  (concat base-sql " AND session_id = ? ORDER BY id DESC LIMIT ?")
                (concat base-sql " ORDER BY id DESC LIMIT ?")))
         (params (if session-id
                     (list meta-pat content-pat session-id (or limit 20))
                   (list meta-pat content-pat (or limit 20)))))
    (superchat-db-tape-select sql params)))

(defun superchat-view-recent-errors (session-id &optional count)
  "Return recent tool_result entries that look like errors for SESSION-ID.
If SESSION-ID is nil, search across all sessions."
  (let* ((base-sql
          "SELECT id, session_id, topic, kind, content, meta, created_at
           FROM tape
           WHERE kind = 'tool_result'
             AND (content LIKE '%Error:%' OR content LIKE '%error%' OR content LIKE '%FAILED%'
                  OR content LIKE '%exception%' OR content LIKE '%Traceback%')")
         (sql (if session-id
                  (concat base-sql " AND session_id = ? ORDER BY id DESC LIMIT ?")
                (concat base-sql " ORDER BY id DESC LIMIT ?")))
         (params (if session-id
                     (list session-id (or count 10))
                   (list (or count 10)))))
    (superchat-db-tape-select sql params)))

;; ═══════════════════════════════════════════════════════════
;; Context continuity v0
;; ═══════════════════════════════════════════════════════════

(defconst superchat-view--continuity-policy "continuity-v0/recent-first"
  "Selection policy identifier for context continuity v0.")

(defconst superchat-view--continuity-recall-policy
  "continuity-v0/explicit-selector"
  "Selection policy identifier for explicit context recall.")

(defun superchat-view--continuity-compact-anchor-p (record)
  "Return non-nil when RECORD is a typed compact Anchor."
  (superchat-view--compact-anchor-row-p (cdr record)))

(defun superchat-view--continuity-record (session-id row)
  "Attach SESSION-ID to a six-column Tape ROW."
  (cons session-id row))

(defun superchat-view--continuity-record-session-id (record)
  "Return the Session ID carried by continuity RECORD."
  (car record))

(defun superchat-view--continuity-record-id (record)
  "Return the storage ID carried by continuity RECORD."
  (nth 0 (cdr record)))

(defun superchat-view--continuity-record-kind (record)
  "Return the Tape kind carried by continuity RECORD."
  (nth 2 (cdr record)))

(defun superchat-view--continuity-record-topic (record)
  "Return the Tape topic carried by continuity RECORD."
  (nth 1 (cdr record)))

(defun superchat-view--continuity-record-content (record)
  "Return the content carried by continuity RECORD."
  (nth 3 (cdr record)))

(defun superchat-view--continuity-record-meta (record)
  "Decode RECORD's JSON metadata as an alist, or nil when malformed."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (condition-case nil
        (json-read-from-string (or (nth 4 (cdr record)) "{}"))
      (error nil))))

(defun superchat-view--continuity-runtime-event-p (record)
  "Return non-nil when RECORD carries a Runtime event envelope."
  (let ((event-type
         (superchat-view--object-get
          (superchat-view--continuity-record-meta record)
          '(event_type))))
    (and (stringp event-type) (> (length event-type) 0))))

(defun superchat-view--continuity-lineage-segments
    (session-id &optional max-id seen)
  "Return parent-first lineage segments for SESSION-ID through MAX-ID.
SEEN is an internal cycle guard.  Each segment retains its six-column rows."
  (when (member session-id seen)
    (error "Cyclic Superchat session lineage at %s" session-id))
  (let* ((rows (superchat-view-entries session-id nil nil nil max-id))
         (parent (superchat-view--branch-parent rows))
         (segment (list :session-id session-id
                        :through-storage-id
                        (and rows (nth 0 (car (last rows))))
                        :rows rows)))
    (if parent
        (append
         (superchat-view--continuity-lineage-segments
          (car parent) (cdr parent) (cons session-id seen))
         (list segment))
      (list segment))))

(defun superchat-view--continuity-segment-records (segments)
  "Flatten parent-first lineage SEGMENTS into continuity records."
  (apply
   #'append
   (mapcar
    (lambda (segment)
      (let ((session-id (plist-get segment :session-id)))
        (mapcar (lambda (row)
                  (superchat-view--continuity-record session-id row))
                (plist-get segment :rows))))
    segments)))

(defun superchat-view--continuity-public-lineage (segments)
  "Remove private row payloads from lineage SEGMENTS."
  (vconcat
   (mapcar
    (lambda (segment)
      (list :session-id (plist-get segment :session-id)
            :through-storage-id (plist-get segment :through-storage-id)))
    segments)))

(defun superchat-view--admitted-material (row)
  "Derive a user-material message from an admitted injection ROW.
Only Runtime's identified `attachment.added' payload is prompt material;
ordinary attachment metadata and other event summaries are not messages.
An unavailable payload signals rather than inventing historical text."
  (when (and (equal "system" (nth 2 row))
             (equal "inject" (nth 1 row))
             (equal "attachment.added"
                    (superchat-view--object-get
                     (superchat-view--row-meta row) '(event_type))))
    (let* ((event (superchat-tape--row-event row))
           (payload (plist-get event :payload)))
      (unless (and (equal 1 (plist-get event :schema))
                   (equal "runtime" (plist-get event :actor-id))
                   (stringp (plist-get payload :text))
                   (plist-get payload :source)
                   (not (plist-get payload :omitted)))
        (error "Admitted material at Tape row %s is unavailable" (car row)))
      (list :role (if (equal "finish-check"
                          (format "%s" (plist-get (plist-get payload :source) :origin)))
                      'system
                    'user)
            :content (superchat-tape-injected-material-line
                      (plist-get payload :text))
            :id (car row)
            :event-id (plist-get event :event-id)
            :source (plist-get payload :source)
            :input-ids (plist-get payload :input-ids)))))

(defun superchat-view--continuity-raw-item (record)
  "Normalize a material continuity RECORD as a raw fact."
  (let ((material (superchat-view--admitted-material (cdr record))))
    (append
     (list :class 'raw-fact
           :session-id (superchat-view--continuity-record-session-id record)
           :storage-id (superchat-view--continuity-record-id record)
           :kind (if material "user" (superchat-view--continuity-record-kind record))
           :content (if material (plist-get material :content)
                      (superchat-view--continuity-record-content record)))
     (when material
       (list :event-id (plist-get material :event-id)
             :source (plist-get material :source)
             :input-ids (plist-get material :input-ids))))))

(defun superchat-view--continuity-tool-pair-p (candidate)
  "Return non-nil when CANDIDATE is a tool-observation pair."
  (and (consp candidate)
       (eq (car candidate) :tool-observation)))

(defun superchat-view--continuity-row-event (record)
  "Return RECORD's decoded Runtime event plist, or nil."
  (condition-case nil
      (superchat-tape--row-event (cdr record))
    (error nil)))

(defun superchat-view--continuity-tool-observation-pairs (records &optional excluded-ids)
  "Return tool-observation pairs over lineage RECORDS.
Each pair is `(:tool-observation t :requested RECORD :outcome RECORD)'.
A requested row and the `tool.completed'/`tool.failed' row with the same
payload `:execution-id' pair within the same Session; an outcome without a
string `:result-text', a missing partner, or an EXCLUDED-IDS row yields no
pair.  Never paired by adjacency or tool name."
  (let (requested-by-key outcomes pairs)
    (dolist (record records)
      (let* ((session-id (superchat-view--continuity-record-session-id record))
             (id (superchat-view--continuity-record-id record))
             ;; Only tool rows can pair; decoding every row's meta on every
             ;; assembly would make a long Session's history cost grow with
             ;; its size.
             (event (and (member (superchat-view--continuity-record-kind record)
                                 '("tool_call" "tool_result"))
                         (superchat-view--continuity-row-event record)))
             (type (and event (plist-get event :type)))
             (payload (and event (plist-get event :payload)))
             (execution-id (and payload (plist-get payload :execution-id))))
        (when (and (stringp execution-id)
                   (not (memql id excluded-ids)))
          (cond
           ((equal type "tool.requested")
            (push (cons (cons session-id execution-id) record)
                  requested-by-key))
           ((and (member type '("tool.completed" "tool.failed"))
                 (stringp (plist-get payload :result-text)))
            (push (list :tool-observation t
                        :key (cons session-id execution-id)
                        :outcome record)
                  outcomes))))))
    (dolist (outcome (nreverse outcomes))
      (let ((requested (cdr (assoc (plist-get outcome :key) requested-by-key))))
        (when (and requested
                   (not (memql (superchat-view--continuity-record-id requested)
                               excluded-ids)))
          (push (list :tool-observation t
                      :requested requested
                      :outcome (plist-get outcome :outcome))
                pairs))))
    (nreverse pairs)))

(defun superchat-view--continuity-tool-observation-item (pair)
  "Normalize tool-observation PAIR as one item.
STATUS is `ok' for `tool.completed' and the outcome's own `:status' else."
  (let* ((requested (plist-get pair :requested))
         (outcome (plist-get pair :outcome))
         (requested-event (superchat-view--continuity-row-event requested))
         (outcome-event (superchat-view--continuity-row-event outcome))
         (requested-payload
          (and requested-event (plist-get requested-event :payload)))
         (outcome-payload (and outcome-event (plist-get outcome-event :payload)))
         (arguments-cut (plist-get requested-payload :arguments-truncated))
         (result-cut (plist-get outcome-payload :result-truncated)))
    (list :class 'tool-observation
          :kind "tool-observation"
          :storage-id (superchat-view--continuity-record-id outcome)
          :source-ids (vector (superchat-view--continuity-record-id requested)
                              (superchat-view--continuity-record-id outcome))
          :tool-name (plist-get requested-payload :tool-name)
          :execution-id (plist-get requested-payload :execution-id)
          :run-id (plist-get requested-event :run-id)
          :status (if (equal "tool.completed" (plist-get outcome-event :type))
                      "ok"
                    (plist-get outcome-payload :status))
          :arguments-text (plist-get requested-payload :arguments-text)
          :arguments-cut arguments-cut
          :arguments-chars (plist-get requested-payload :arguments-chars)
          :result-text (plist-get outcome-payload :result-text)
          :result-cut result-cut
          :result-chars (plist-get outcome-payload :result-chars)
          :lossy (or arguments-cut result-cut))))

(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function superchat-get-llm-tools "superchat-tools" (&optional tool-names input-operation))

(defun superchat-view--tool-parameter-names (tool-name)
  "Return TOOL-NAME's parameter names from the current tools, or `:unknown'."
  (let ((tool (and (fboundp 'superchat-get-llm-tools)
                   (cl-find tool-name (ignore-errors (superchat-get-llm-tools))
                            :key #'llm-tool-name :test #'equal))))
    (if tool
        (mapcar (lambda (arg) (plist-get arg :name)) (llm-tool-args tool))
      :unknown)))

(defun superchat-view-call-arguments (tool-name text cut-chars)
  "Return (:arguments JSON :note NOTE) for a stored call of TOOL-NAME.
TEXT is the call's stored `:arguments-text'; CUT-CHARS is its original
length when the text was cut.  A complete JSON object is sent as stored; a
complete positional array is named by TOOL-NAME's current parameter list; any
other text -- cut, unreadable, absent, or of a tool no longer defined -- is
sent as `{}' with NOTE `(Arguments not kept in full: TEXT)'."
  (let* ((parsed (if (and (stringp text) (not cut-chars))
                     (condition-case nil
                         (json-parse-string text :object-type 'hash-table
                                            :array-type 'array
                                            :null-object :null :false-object :false)
                       (error :unreadable))
                   :unreadable))
         (named
          (cond
           ((hash-table-p parsed) text)
           ((and (vectorp parsed) (zerop (length parsed))) "{}")
           ((vectorp parsed)
            (let ((names (superchat-view--tool-parameter-names tool-name)))
              (when (and (listp names) (<= (length parsed) (length names)))
                (let ((object (make-hash-table :test #'equal)))
                  (cl-loop for value across parsed
                           for name in names
                           unless (eq value :null)
                           do (puthash name value object))
                  (json-serialize object :null-object :null :false-object :false))))))))
    (if named
        (list :arguments named :note nil)
      (list :arguments "{}"
            :note (format "(Arguments not kept in full: %s)"
                          (if (stringp text)
                              (superchat-tape--history-cut-text text cut-chars cut-chars)
                            "none"))))))

(defun superchat-view-tool-observation-entries (session-id rows)
  "Return SESSION-ID's tool observation entries over Tape ROWS.
Each entry is `(:id OUTCOME-ID :source-ids (REQUESTED-ID OUTCOME-ID)
:content LINE)' in outcome order.  Pairing and rendering are the Context
View's own, so a consumer can never disagree with the prompt history; a row
without its partner or stored text yields no entry."
  (let* ((records (mapcar (lambda (row)
                            (superchat-view--continuity-record session-id row))
                          rows))
         (pairs (superchat-view--continuity-tool-observation-pairs records)))
    (mapcar
     (lambda (pair)
       (let* ((requested (plist-get pair :requested))
              (outcome (plist-get pair :outcome))
              (item (superchat-view--continuity-tool-observation-item pair)))
         (list :id (superchat-view--continuity-record-id outcome)
               :source-ids
               (list (superchat-view--continuity-record-id requested)
                     (superchat-view--continuity-record-id outcome))
               :content (superchat-view--continuity-item-line item))))
     pairs)))

(defun superchat-view--continuity-compact-anchor-source-records
    (anchor records)
  "Return valid raw source RECORDS for typed compact ANCHOR, or nil."
  (let* ((source-ids
          (superchat-view--compact-anchor-row-source-ids (cdr anchor)))
         (ids (and source-ids (append source-ids nil)))
         (anchor-id (superchat-view--continuity-record-id anchor))
         (session-id (superchat-view--continuity-record-session-id anchor)))
    (when ids
      (let ((sources
             (mapcar
              (lambda (id)
                (cl-find-if
                 (lambda (record)
                   (and (equal session-id
                               (superchat-view--continuity-record-session-id
                                record))
                        (= id (superchat-view--continuity-record-id record))))
                 records))
              ids)))
        (when (and (cl-every #'identity sources)
                   (let ((pair-row-ids
                          (superchat-view--continuity-pair-row-ids sources)))
                     (cl-every
                      (lambda (record)
                        (superchat-view--continuity-anchor-source-valid-p
                         record pair-row-ids anchor-id))
                      sources))
                   (= (length sources) (length ids)))
          (sort sources
                (lambda (left right)
                  (< (superchat-view--continuity-record-id left)
                     (superchat-view--continuity-record-id right)))))))))

(defun superchat-view--continuity-record-item (record &optional anchor source-ids)
  "Normalize RECORD, using derived compact ANCHOR when it matches."
  (if (and anchor
           (= (superchat-view--continuity-record-id record)
              (superchat-view--continuity-record-id anchor)))
      (list :class 'model-interpretation
            :storage-id (superchat-view--continuity-record-id record)
            :source-ids source-ids
            :lossy t
            :content (superchat-view--continuity-record-content record))
    (superchat-view--continuity-raw-item record)))

(defun superchat-view--continuity-role (kind)
  "Return the canonical prompt role label for material KIND."
  (cond
   ((string= kind "user") "User")
   ((string= kind "assistant") "Assistant")
   ((string= kind "system") "System")
   (t (error "Unsupported continuity prompt kind: %s" kind))))

(defun superchat-view--continuity-item-line (item)
  "Render one normalized continuity ITEM as its canonical body line."
  (pcase (plist-get item :class)
    ('tool-observation
     (superchat-tape-tool-observation-line
      (plist-get item :tool-name)
      (plist-get item :status)
      (plist-get item :arguments-text)
      (plist-get item :arguments-cut)
      (plist-get item :arguments-chars)
      (plist-get item :result-text)
      (plist-get item :result-cut)
      (plist-get item :result-chars)))
    ('model-interpretation
     (format "Summary: %s" (plist-get item :content)))
    (_
     (format "%s: %s"
             (superchat-view--continuity-role (plist-get item :kind))
             (plist-get item :content)))))

(defun superchat-view--continuity-select-records
    (records entry-budget char-budget &optional line-function item-function)
  "Select records from RECORDS within ENTRY-BUDGET and CHAR-BUDGET.
Records are considered newest first.  A record whose rendered line is larger
than the positive CHAR-BUDGET is skipped regardless of its class.  A
model-interpretation record that fits by itself but would exceed either
budget is also skipped.  Ordinary records that fit by themselves but exceed
the remaining budget terminate selection, preserving the recent-first
boundary.  The returned boundary describes only that termination; skipped
oversized records are returned separately.  LINE-FUNCTION renders a
normalized item for character accounting."
  (let ((render-line (or line-function
                         #'superchat-view--continuity-item-line))
        (selected nil)
        (selected-count 0)
        (selected-chars 0)
        boundary-record
        boundary-reason
        skipped-records)
    (catch 'budget-full
      (dolist (record (reverse (copy-sequence records)))
        (let* ((item (if item-function
                         (funcall item-function record)
                       (superchat-view--continuity-raw-item record)))
               (line-length (length (funcall render-line item)))
               (candidate-chars
                (+ selected-chars line-length
                   (if (> selected-count 0) 1 0))))
          (cond
           ((and char-budget
                 (> char-budget 0)
                 (> line-length char-budget))
            (push record skipped-records))
           ((and (eq (plist-get item :class) 'model-interpretation)
                 (or (and entry-budget (>= selected-count entry-budget))
                     (and char-budget (> candidate-chars char-budget))))
            nil)
           (t
            (when (and entry-budget (>= selected-count entry-budget))
              (setq boundary-record record)
              (setq boundary-reason 'budget)
              (throw 'budget-full nil))
            (when (and char-budget (> candidate-chars char-budget))
              (setq boundary-record record)
              (setq boundary-reason 'budget)
              (throw 'budget-full nil))
            (push record selected)
            (setq selected-count (1+ selected-count))
            (setq selected-chars candidate-chars))))))
    (list :records selected
          :boundary-record boundary-record
          :boundary-reason boundary-reason
          :skipped-records skipped-records)))

(defun superchat-view--continuity-material-record-p (record)
  "Return non-nil when continuity RECORD is prompt material."
  (let ((kind (superchat-view--continuity-record-kind record)))
    (or (member kind '("user" "assistant"))
        (superchat-view--admitted-material (cdr record))
        (and (string= kind "system")
             (not (superchat-view--continuity-runtime-event-p record))))))

;; ═══════════════════════════════════════════════════════════
;; Model-visible history (M1): the one derivation over the Tape
;; ═══════════════════════════════════════════════════════════

(defun superchat-view--derive-digest-entry (session-id anchored)
  "Return (DIGESTED . LEADING) for one Anchor in SESSION-ID.
DIGESTED is the set of rows the Anchor replaces: its named sources together
with every row inside their span, because the snapshot reaches from the oldest
source to the newest.  LEADING is the row count recorded after the newest
source, which the compaction keeps ahead of the digest instead of behind it."
  (let* ((row (cdr anchored))
         (ids (append (superchat-view--compact-anchor-row-source-ids row) nil)))
    (when ids
      (let ((low (apply #'min ids))
            (high (apply #'max ids)))
        (cons (append ids
                      (delq nil
                            (mapcar (lambda (other)
                                      (let ((id (nth 0 other)))
                                        (and (>= id low) (<= id high) id)))
                                    (car anchored))))
              high)))))

(defun superchat-view--derive-message (row)
  "Return the message plist for material ROW, or nil when ROW is no message.
Runtime events persist with the `system' kind and carry an event envelope in
their metadata.  Only identified admitted injection payloads contribute user
material; other event summaries are not messages.  Conversation material also
includes the `user' and `assistant' kinds and the typed compaction Anchor."
  (let ((kind (nth 2 row))
        (content (nth 3 row))
        (id (nth 0 row)))
    (cond
     ((string= kind "anchor")
      (list :role 'system
            :content (concat "[Session anchor]\n\n" (or content ""))
            :anchor t
            :compaction-anchor t
            :id id
            :source-ids (superchat-view--compact-anchor-row-source-ids row)))
     ((string= kind "user") (list :role 'user :content content :id id))
     ((string= kind "assistant") (list :role 'assistant :content content :id id))
     (t (superchat-view--admitted-material row)))))

(defun superchat-view-derive-messages (session-id &optional limit expanded-anchor-ids)
  "Derive the model-visible message list for SESSION-ID from the Tape.
This is M1's single derivation: the returned list is oldest-first, each entry
a plist with `:role', `:content' and `:id' (the Tape storage id), and the
source rows of that Session's valid compaction Anchors are collapsed to their
Anchor entry.  LIMIT keeps the newest LIMIT entries, matching the in-buffer
history cap; it counts entries rather than sources.

EXPANDED-ANCHOR-IDS names Anchors the caller has expanded; those Anchors
digest nothing, so their sources are returned as raw rows, which is the same
reading `superchat-view--continuity-assemble' already gives expanded Anchors.
An Anchor keeps its own storage position, and rows recorded after its newest
source follow it because they are newer than the summary that distils the
snapshot.  An Anchor whose provenance is unreadable contributes no digest set:
its summary is skipped and its sources remain visible, because dropping rows
on the strength of an unvalidated Anchor would lose history rather than
digest it."
  (let* ((all-rows (superchat-view-entries session-id))
         (expanded (append expanded-anchor-ids nil))
         ;; An Anchor at or below the highest expanded Anchor is read the way
         ;; `superchat-view--continuity-assemble' reads it: the summary is not
         ;; offered and its sources stand as raw rows, so an expansion still
         ;; takes effect when a later compaction adds another Anchor.
         (expanded-high-water (and expanded (apply #'max expanded)))
         (anchors (cl-remove-if
                   (lambda (row)
                     (and expanded-high-water
                          (<= (nth 0 row) expanded-high-water)))
                   (cl-remove-if-not #'superchat-view--compact-anchor-row-p
                                     all-rows)))
         (entries (delq nil (mapcar (lambda (row)
                                      (and (superchat-view--compaction-anchor-valid-p
                                            session-id row all-rows)
                                           (cons all-rows row)))
                                    anchors)))
         (digested (apply #'append
                          (mapcar (lambda (entry)
                                    (car (superchat-view--derive-digest-entry
                                          session-id entry)))
                                  entries)))
         (high (apply #'max 0 (mapcar (lambda (entry)
                                        (cdr (superchat-view--derive-digest-entry
                                              session-id entry)))
                                      entries))))
    (let* ((hidden-anchors (and expanded-high-water
                                (delq nil
                                      (mapcar (lambda (row)
                                                (and (string= (nth 2 row) "anchor")
                                                     (<= (nth 0 row) expanded-high-water)
                                                     (nth 0 row)))
                                              all-rows))))
           (kept (delq nil
                       (mapcar (lambda (row)
                                 (unless (or (and digested (member (nth 0 row) digested))
                                             (member (nth 0 row) hidden-anchors))
                                   (superchat-view--derive-message row)))
                               all-rows)))
           ;; A row recorded after the newest digested snapshot is newer than
           ;; the summary that distils the snapshot, so it follows the Anchor
           ;; in oldest-first order; the rows older than the snapshot and
           ;; outside it precede it.  Partitioning the kept rows this way
           ;; keeps an Anchor at the position its snapshot occupied.
           (leading (cl-remove-if-not
                     (lambda (message)
                       (let ((id (plist-get message :id)))
                         (and id (> id high) (not (plist-get message :anchor)))))
                     kept))
           (messages (append (cl-remove-if
                              (lambda (message)
                                (let ((id (plist-get message :id)))
                                  (and id (> id high)
                                       (not (plist-get message :anchor)))))
                              kept)
                             leading)))
      (if (and (integerp limit) (> limit 0) (> (length messages) limit))
          (last messages limit)
        messages))))

(defun superchat-view--continuity-branch-marker-p (record)
  "Return non-nil when RECORD is a validated structural branch marker."
  (and (string= (superchat-view--continuity-record-kind record) "anchor")
       (string= (or (superchat-view--continuity-record-topic record) "")
                "branch")
       (superchat-view--branch-parent (list (cdr record)))))

(defun superchat-view--continuity-nonmaterial-reason (record)
  "Return the honest omission reason for non-material RECORD."
  (if (and (string= (superchat-view--continuity-record-kind record) "anchor")
           (not (superchat-view--continuity-branch-marker-p record)))
      'unsupported-schema
    'not-prompt-material))

(defun superchat-view--continuity-record-ranges (records)
  "Compress chronological RECORDS into Session-scoped closed ranges."
  (let (current-session start previous ranges)
    (dolist (record records)
      (let ((session-id (superchat-view--continuity-record-session-id record))
            (id (superchat-view--continuity-record-id record)))
        (if (and previous
                 (equal session-id current-session)
                 (= id (1+ previous)))
            (setq previous id)
          (when start
            (push (list :session-id current-session
                        :range (vector start previous))
                  ranges))
          (setq current-session session-id)
          (setq start id)
          (setq previous id))))
    (when start
      (push (list :session-id current-session
                  :range (vector start previous))
            ranges))
    (vconcat (nreverse ranges))))

(defun superchat-view--continuity-omission-ranges (classified)
  "Compress CLASSIFIED `(RECORD . REASON)' omissions."
  (let (current-session start previous current-reason ranges)
    (dolist (entry classified)
      (let ((session-id
             (superchat-view--continuity-record-session-id (car entry)))
            (id (superchat-view--continuity-record-id (car entry)))
            (reason (cdr entry)))
        (if (and previous
                 (equal session-id current-session)
                 (= id (1+ previous))
                 (eq reason current-reason))
            (setq previous id)
          (when start
            (push (list :session-id current-session
                        :range (vector start previous)
                        :reason current-reason
                        :recoverable 'raw-recall)
                  ranges))
          (setq current-session session-id)
          (setq start id)
          (setq previous id)
          (setq current-reason reason))))
    (when start
      (push (list :session-id current-session
                  :range (vector start previous)
                  :reason current-reason
                  :recoverable 'raw-recall)
            ranges))
    (vconcat (nreverse ranges))))

(defun superchat-view--continuity-recallable-ranges (classified)
  "Return raw recall selectors for CLASSIFIED record omissions."
  (vconcat
   (mapcar
    (lambda (range)
      (list :session-id (plist-get range :session-id)
            :selector (list :range (plist-get range :range))))
    (append (superchat-view--continuity-record-ranges
             (mapcar #'car classified))
            nil))))

(defun superchat-view--continuity-range-label (range)
  "Render one disclosure RANGE as a canonical raw segment label."
  (let* ((session-id (plist-get range :session-id))
         (bounds (plist-get range :range))
         (first-id (aref bounds 0))
         (last-id (aref bounds 1)))
    (if (= first-id last-id)
        (format "%s#%d" session-id first-id)
      (format "%s#%d-%d" session-id first-id last-id))))

(defvar superchat-view-model-can-recall nil
  "Non-nil when the request offers the model a tool that performs raw recall
of omitted rows.  Contract v0 offers none, so the rendered header says
`recall none'.  `:disclosure :recallable' is computed regardless.")

(defun superchat-view--continuity-prompt
    (items covered omitted-count recall-available &optional line-function
            represented-count)
  "Render canonical prompt for ITEMS, COVERED, omissions, and recall state."
  (let* ((render-line (or line-function
                          #'superchat-view--continuity-item-line))
         (has-items (> (length items) 0))
         (segment (if has-items
                      (mapconcat #'superchat-view--continuity-range-label
                                 (append covered nil) ", ")
                    "none"))
         (header (format
                  "[Context v0 | raw %s | represented %s | omitted %s | recall %s]"
                  segment
                  (if (and represented-count (> represented-count 0))
                      (number-to-string represented-count)
                    "none")
                  (if (> omitted-count 0)
                      (number-to-string omitted-count)
                    "none")
                  (if recall-available "available" "none")))
         (body
          (mapconcat
           render-line
           (append items nil)
           "\n"))
         (body
          (if (cl-some (lambda (item)
                         (eq (plist-get item :class) 'tool-observation))
                       items)
              (concat superchat-tape-tool-observation-note "\n" body)
            body)))
    (if has-items
        (concat header "\n\n" body)
      header)))

(cl-defun superchat-view--continuity-assemble
    (session-id &key (purpose 'prompt) max-entries max-chars
                expanded-anchor-ids excluded-storage-ids)
  "Assemble a deterministic context continuity envelope for SESSION-ID.
Material belonging to a durably cancelled Run is excluded from this default
prompt View, while the underlying Tape rows remain available to explicit
recall.  EXCLUDED-STORAGE-IDS rows are skipped as if absent: a replaced
turn must leave neither context nor recallable disclosure behind."
  (let* ((segments
          (superchat-view--continuity-lineage-segments session-id))
         (all-records
          (superchat-view--continuity-segment-records segments))
         (all-records
          (if excluded-storage-ids
              (cl-remove-if
               (lambda (record)
                 (memql (superchat-view--continuity-record-id record)
                        excluded-storage-ids))
               all-records)
            all-records))
         (cancelled-run-ids
          (superchat-view--continuity-cancelled-run-ids all-records))
         (expanded-ids (append expanded-anchor-ids nil))
         (expanded-anchor-high-water
          (and expanded-ids (apply #'max expanded-ids)))
         (compact-anchor
          (car
           (last
            (cl-remove-if-not
             (lambda (record)
               (and (superchat-view--continuity-compact-anchor-p record)
                    (or (null expanded-anchor-high-water)
                        (> (superchat-view--continuity-record-id record)
                           expanded-anchor-high-water))))
             all-records))))
         (compact-source-records
          (when compact-anchor
            (superchat-view--continuity-compact-anchor-source-records
             compact-anchor all-records)))
         (compact-cancelled-source-p
          (cl-some
           (lambda (record)
             (superchat-view--continuity-cancelled-record-p
              record cancelled-run-ids))
           compact-source-records))
         (compact-source-ids
          (when compact-source-records
            (vconcat
             (mapcar #'superchat-view--continuity-record-id
                     compact-source-records))))
         (compact-valid-p
          (and compact-anchor compact-source-records compact-source-ids
               (not compact-cancelled-source-p)))
         (compact-anchor-id
          (and compact-anchor
               (superchat-view--continuity-record-id compact-anchor)))
         (material-records
          (cl-remove-if-not #'superchat-view--continuity-material-record-p
                            all-records))
         (source-id-list (and compact-valid-p (append compact-source-ids nil)))
         (cancelled-ids
          (cl-loop for record in all-records
                   when (superchat-view--continuity-cancelled-record-p
                         record cancelled-run-ids)
                   collect (superchat-view--continuity-record-id record)))
         (tool-pairs
          (superchat-view--continuity-tool-observation-pairs
           all-records (append source-id-list cancelled-ids)))
         (tool-pair-outcomes
          (mapcar (lambda (pair)
                    (cons (superchat-view--continuity-record-id
                           (plist-get pair :outcome))
                          pair))
                  tool-pairs))
         (candidate-records
          (if compact-valid-p
              (cl-remove-if-not
               (lambda (record)
                 (and (not
                       (superchat-view--continuity-cancelled-record-p
                        record cancelled-run-ids))
                      (or (and (= (superchat-view--continuity-record-id record)
                             compact-anchor-id)
                          (equal (superchat-view--continuity-record-session-id
                                  record)
                                 (superchat-view--continuity-record-session-id
                                  compact-anchor)))
                     (and (superchat-view--continuity-material-record-p record)
                          (not (and
                                (equal
                                 (superchat-view--continuity-record-session-id
                                  record)
                                 (superchat-view--continuity-record-session-id
                                  compact-anchor))
                                (member
                                 (superchat-view--continuity-record-id record)
                                 source-id-list)))))))
               all-records)
            (cl-remove-if
             (lambda (record)
               (superchat-view--continuity-cancelled-record-p
                record cancelled-run-ids))
             material-records)))
         (candidate-record-ids
          (mapcar #'superchat-view--continuity-record-id candidate-records))
         (candidates
          (cl-loop for record in all-records
                   for id = (superchat-view--continuity-record-id record)
                   for pair = (cdr (assq id tool-pair-outcomes))
                   when pair collect pair
                   else when (memql id candidate-record-ids)
                   collect record))
         (item-function
          (lambda (candidate)
            (cond
             ((superchat-view--continuity-tool-pair-p candidate)
              (superchat-view--continuity-tool-observation-item candidate))
             (compact-valid-p
              (superchat-view--continuity-record-item
               candidate compact-anchor compact-source-ids))
             (t (superchat-view--continuity-raw-item candidate)))))
         (entry-budget max-entries)
         (char-budget (or max-chars 4000))
         (selection
          (superchat-view--continuity-select-records
           candidates entry-budget char-budget nil item-function))
         (records (plist-get selection :records))
         (skipped-records (plist-get selection :skipped-records))
         (selected-records
          (cl-remove-if #'superchat-view--continuity-tool-pair-p records))
         (selected-pairs
          (cl-remove-if-not #'superchat-view--continuity-tool-pair-p records))
         (selected-pair-row-ids
          (cl-loop for pair in selected-pairs
                   append (list (superchat-view--continuity-record-id
                                 (plist-get pair :requested))
                                (superchat-view--continuity-record-id
                                 (plist-get pair :outcome)))))
         (skipped-pair-outcome-ids
          (cl-loop for candidate in skipped-records
                   when (superchat-view--continuity-tool-pair-p candidate)
                   collect (superchat-view--continuity-record-id
                            (plist-get candidate :outcome))))
         (pair-row-ids
          (cl-loop for pair in tool-pairs
                   append (list (superchat-view--continuity-record-id
                                 (plist-get pair :requested))
                                (superchat-view--continuity-record-id
                                 (plist-get pair :outcome)))))
         (omitted-pair-reasons
          (cl-loop for pair in tool-pairs
                   unless (memql (superchat-view--continuity-record-id
                                  (plist-get pair :outcome))
                                 selected-pair-row-ids)
                   append (let ((reason
                                 (if (memql
                                      (superchat-view--continuity-record-id
                                       (plist-get pair :outcome))
                                      skipped-pair-outcome-ids)
                                     'oversized
                                   'budget)))
                            (list (cons (superchat-view--continuity-record-id
                                         (plist-get pair :requested))
                                        reason)
                                  (cons (superchat-view--continuity-record-id
                                         (plist-get pair :outcome))
                                        reason)))))
         (selected-ids
          (append (mapcar #'superchat-view--continuity-record-id
                          selected-records)
                  selected-pair-row-ids))
         (anchor-selected-p
          (and compact-valid-p (member compact-anchor-id selected-ids)))
         (classified-omissions
          (delq
           nil
           (mapcar
            (lambda (record)
              (cond
               ((memql (superchat-view--continuity-record-id record)
                       pair-row-ids)
                (when-let* ((reason
                             (cdr (assq
                                   (superchat-view--continuity-record-id record)
                                   omitted-pair-reasons))))
                  (cons record reason)))
               ((and (superchat-view--continuity-compact-anchor-p record)
                     (member
                      (superchat-view--continuity-record-id record)
                      expanded-ids))
                (cons record 'not-prompt-material))
               ((superchat-view--continuity-cancelled-record-p
                 record cancelled-run-ids)
                (cons record 'cancelled))
               ((and compact-valid-p
                     (= (superchat-view--continuity-record-id record)
                        compact-anchor-id)
                     (equal
                      (superchat-view--continuity-record-session-id record)
                      (superchat-view--continuity-record-session-id
                       compact-anchor)))
                (unless (member
                         (superchat-view--continuity-record-id record)
                         selected-ids)
                  (cons record
                        (if (member record skipped-records)
                            'oversized
                          'budget))))
               ((and compact-valid-p
                     (equal
                      (superchat-view--continuity-record-session-id record)
                      (superchat-view--continuity-record-session-id
                       compact-anchor))
                     (member (superchat-view--continuity-record-id record)
                             source-id-list))
                (unless anchor-selected-p
                  (cons record 'budget)))
               ((and compact-anchor
                     (= (superchat-view--continuity-record-id record)
                        compact-anchor-id)
                     (not compact-valid-p))
                (cons record (if compact-cancelled-source-p
                                'cancelled
                              'malformed-provenance)))
               ((and (superchat-view--continuity-compact-anchor-p record)
                     (or (not compact-anchor)
                         (not
                          (= (superchat-view--continuity-record-id record)
                             compact-anchor-id))))
                (if (superchat-view--continuity-compact-anchor-source-records
                     record all-records)
                    (cons record 'not-prompt-material)
                  (cons record 'malformed-provenance)))
               ((not (superchat-view--continuity-material-record-p record))
                (cons record
                      (superchat-view--continuity-nonmaterial-reason record)))
               ((member (superchat-view--continuity-record-id record)
                        selected-ids)
                nil)
               ((member record skipped-records)
                (cons record 'oversized))
               (t (cons record 'budget))))
            all-records)))
         (omitted-count (length classified-omissions))
         (resource-omitted-p
          (cl-some (lambda (entry)
                     (memq (cdr entry)
                           '(budget oversized unsupported-schema
                             malformed-provenance cancelled)))
                   classified-omissions))
         (items (vconcat (mapcar item-function records)))
         (tape-head
          (and all-records
               (superchat-view--continuity-record-id
                (car (last all-records)))))
         (covered
          (superchat-view--continuity-record-ranges
           (let ((plain
                  (cl-remove-if
                   #'superchat-view--continuity-tool-pair-p records)))
             (if compact-valid-p
                 (cl-remove-if
                  (lambda (record)
                    (= (superchat-view--continuity-record-id record)
                       compact-anchor-id))
                  plain)
               plain))))
         (represented
          (if anchor-selected-p
              (mapcar
               (lambda (range)
                 (let* ((bounds (plist-get range :range))
                        (from (aref bounds 0))
                        (to (aref bounds 1))
                        (range-source-ids
                         (vconcat
                          (mapcar
                           #'superchat-view--continuity-record-id
                           (cl-remove-if-not
                            (lambda (record)
                              (let ((id
                                     (superchat-view--continuity-record-id
                                      record)))
                                (and (>= id from) (<= id to))))
                            compact-source-records)))))
                   (list :session-id (plist-get range :session-id)
                         :range bounds
                         :via-storage-id compact-anchor-id
                         :class 'model-interpretation
                         :lossy t
                         :source-ids range-source-ids)))
               (append
                (superchat-view--continuity-record-ranges
                 compact-source-records)
                nil))
            nil))
         (pair-represented
          (mapcar
           (lambda (pair)
             (let* ((requested (plist-get pair :requested))
                    (outcome (plist-get pair :outcome))
                    (item (superchat-view--continuity-tool-observation-item pair)))
               (append
                (list :session-id
                      (superchat-view--continuity-record-session-id outcome)
                      :via-storage-id
                      (superchat-view--continuity-record-id outcome)
                      :class 'tool-observation
                      :source-ids
                      (vector (superchat-view--continuity-record-id requested)
                              (superchat-view--continuity-record-id outcome)))
                (when (plist-get item :lossy) (list :lossy t)))))
           selected-pairs))
         (represented (vconcat (append represented pair-represented)))
         (represented-count
          (+ (if anchor-selected-p (length compact-source-ids) 0)
             (* 2 (length selected-pairs))))
         (omissions (superchat-view--continuity-omission-ranges
                     classified-omissions))
         (recallable (superchat-view--continuity-recallable-ranges
                      classified-omissions)))
    (list :contract "context-continuity-v0"
          ;; Amendment 2026-09-25: what the provider receives for this
          ;; selection -- messages, not the `:prompt' text below.
          :messages (and (> (length items) 0)
                         (superchat-tape-history-messages
                          items (superchat-tape-step-key-function session-id)))
          :status (cond
                   ((null all-records) 'empty)
                   (resource-omitted-p 'partial)
                   (t 'satisfied))
          :prompt (if (null all-records)
                      ""
                   (superchat-view--continuity-prompt
                    items covered omitted-count
                    (and superchat-view-model-can-recall
                         (> (length recallable) 0))
                    nil represented-count))
          :scope (list :session-id session-id
                       :purpose purpose
                       :through-storage-id tape-head
                       :lineage
                       (superchat-view--continuity-public-lineage segments)
                       :selection-policy superchat-view--continuity-policy)
          :items items
          :notice
          (and compact-cancelled-source-p
               "Context summary invalidated by a cancelled source Run; 可 /compact 重新压缩")
          :disclosure
          (list :covered covered
                :represented represented
                :omitted omissions
                :recallable recallable)
          :receipt (list :tape-head tape-head
                         :read-status 'ok
                         :selection-policy superchat-view--continuity-policy
                         :selection-policy-version 1))))

(defun superchat-view--continuity-unknown-envelope
    (session-id purpose &optional selection-policy recall)
  "Return an envelope-level read failure for SESSION-ID and PURPOSE.
SELECTION-POLICY defaults to the Assemble policy.  RECALL, when non-nil, is
echoed without claiming that the requested read succeeded."
  (let* ((policy (or selection-policy superchat-view--continuity-policy))
         (envelope
          (list :contract "context-continuity-v0"
                :status 'unknown
                :prompt ""
                :scope (list :session-id session-id
                             :purpose purpose
                             :through-storage-id nil
                             :lineage []
                             :selection-policy policy)
                :items []
                :disclosure (list :covered [] :represented []
                                  :omitted [] :recallable [])
                :receipt (list :tape-head nil
                               :read-status 'failed
                               :selection-policy policy
                               :selection-policy-version 1))))
    (if recall
        (append envelope (list :recall recall))
      envelope)))

(cl-defun superchat-view-assemble
    (session-id &key (purpose 'prompt) max-entries max-chars
                after-storage-id through-storage-id pinned-storage-ids
                expanded-anchor-ids excluded-storage-ids)
  "Assemble a deterministic context envelope, containing read failures.
EXCLUDED-STORAGE-IDS is a list of positive storage ids this View must
skip, e.g. the rows a `turn.superseded' event replaced.  When it is
omitted the View's supersession projection supplies them; if that read
fails the result is the unknown read-failure envelope, never context
assembled without its exclusions."
  (unless (and (stringp session-id) (> (length session-id) 0))
    (error "SESSION-ID must be a non-empty string"))
  (unless (symbolp purpose)
    (error "PURPOSE must be a symbol"))
  (dolist (budget (list max-entries max-chars))
    (unless (or (null budget) (natnump budget))
      (error "Continuity budgets must be non-negative integers")))
  (dolist (storage-id (list after-storage-id through-storage-id))
    (unless (or (null storage-id)
                (and (integerp storage-id) (> storage-id 0)))
      (error "Continuity storage IDs must be positive integers")))
  (unless (or (null pinned-storage-ids)
              (and (vectorp pinned-storage-ids)
                   (cl-every (lambda (id)
                               (and (integerp id) (> id 0)))
                             (append pinned-storage-ids nil))))
    (error "PINNED-STORAGE-IDS must be a vector of positive integers"))
  (unless (or (null expanded-anchor-ids)
              (and (vectorp expanded-anchor-ids)
                   (cl-every (lambda (id)
                               (and (integerp id) (> id 0)))
                             (append expanded-anchor-ids nil))))
    (error "EXPANDED-ANCHOR-IDS must be a vector of positive integers"))
  (unless (and (listp excluded-storage-ids)
               (cl-every (lambda (id) (and (integerp id) (> id 0)))
                         excluded-storage-ids))
    (error "EXCLUDED-STORAGE-IDS must be a list of positive integers"))
  (when (or after-storage-id
            through-storage-id
            (and pinned-storage-ids (> (length pinned-storage-ids) 0)))
    (error "Continuity window and pin selectors are reserved, not implemented"))
  (condition-case nil
      (let ((excluded (or excluded-storage-ids
                         (superchat-view-superseded-storage-ids session-id))))
        (superchat-view--continuity-assemble
         session-id
         :purpose purpose
         :max-entries max-entries
         :max-chars max-chars
         :expanded-anchor-ids expanded-anchor-ids
         :excluded-storage-ids excluded))
    (error (superchat-view--continuity-unknown-envelope session-id purpose))))

;; ═══════════════════════════════════════════════════════════
;; Formatting for LLM consumption
;; ═══════════════════════════════════════════════════════════

(defun superchat-view-row-to-memory-plist (row)
  "Convert a tape ROW into the memory plist shape used by the prompt hook.
ROW layout: (id session_id topic kind content meta created_at)."
  (when row
    (list :id        (nth 0 row)
          :title     (or (nth 3 row) "")
          :content   (or (nth 4 row) "")
          :keywords  (or (nth 2 row) "")
          :mood      ""
          :tags      ""
          :type      "tape"
          :timestamp (or (nth 6 row) ""))))

(defun superchat-view--continuity-record-run-id (record)
  "Return the Runtime Run ID carried by continuity RECORD, or nil."
  (superchat-view--object-get
   (superchat-view--continuity-record-meta record)
   '(run_id run-id)))

(defun superchat-view--continuity-cancelled-run-ids (records)
  "Return Run IDs with a durable `run.cancelled' event in RECORDS."
  (let (run-ids)
    (dolist (record records (delete-dups run-ids))
      (let ((meta (superchat-view--continuity-record-meta record)))
        (when (equal "run.cancelled"
                     (superchat-view--object-get meta '(event_type event-type)))
          (let ((run-id (superchat-view--object-get meta '(run_id run-id))))
            (when (and (stringp run-id) (> (length run-id) 0))
              (push run-id run-ids))))))))

(defun superchat-view--continuity-cancelled-record-p
    (record cancelled-run-ids)
  "Return non-nil when material RECORD belongs to a cancelled Run."
  (and (superchat-view--continuity-material-record-p record)
       (member (superchat-view--continuity-record-run-id record)
               cancelled-run-ids)))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(superchat-define-plugin session-view
  :inject '(tape)
  :doc "Bounded Context Views, search and Session history over the Tape."
  :apply
  (lambda (scope _)
    (superchat-projection-register
     'session-configuration
     :version 1
     :initial #'superchat-view--session-config-initial
     :apply #'superchat-view--session-config-apply)
    (superchat-service-provide
     scope 'session
     (superchat-service-table
      entries #'superchat-view-entries
      search #'superchat-view-search
      assemble-view (lambda (&rest args) (apply #'superchat-view-assemble args))
      superseded-ids #'superchat-view-superseded-storage-ids
      history (lambda (&rest args) (apply #'superchat-view-session-history args))
      configuration #'superchat-view-session-configuration
      session-ids #'superchat-view-session-ids
      anchor-latest #'superchat-view-compaction-anchor-latest
      anchor-source-ids #'superchat-view-compaction-anchor-source-ids
      expand-anchor #'superchat-view-expand-anchor
      item-line #'superchat-view--continuity-item-line))))

(provide 'superchat-tape-view)

;;; superchat-tape-view.el ends here
